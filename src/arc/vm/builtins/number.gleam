import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/math as builtins_math
import arc/vm/frame.{type State}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, Finite, Infinity, JsNumber, JsUndefined, NaN,
  NativeIsFinite, NativeIsNaN, NativeNumberConstructor, NativeNumberIsFinite,
  NativeNumberIsInteger, NativeNumberIsNaN, NativeNumberParseFloat,
  NativeNumberParseInt, NativeParseFloat, NativeParseInt, NegInfinity,
}
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string

/// Set up Number constructor + Number.prototype + global parseInt/parseFloat/isNaN/isFinite.
/// Returns #(Heap, BuiltinType, parse_int_ref, parse_float_ref, is_nan_ref, is_finite_ref).
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType, Ref, Ref, Ref, Ref) {
  // Static methods on Number constructor
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("isNaN", NativeNumberIsNaN, 1),
      #("isFinite", NativeNumberIsFinite, 1),
      #("isInteger", NativeNumberIsInteger, 1),
      #("parseInt", NativeNumberParseInt, 2),
      #("parseFloat", NativeNumberParseFloat, 1),
    ])

  // Static constants
  let constants = [
    #("NaN", value.data(JsNumber(NaN))),
    #("POSITIVE_INFINITY", value.data(JsNumber(Infinity))),
    #("NEGATIVE_INFINITY", value.data(JsNumber(NegInfinity))),
    #("MAX_SAFE_INTEGER", value.data(JsNumber(Finite(9_007_199_254_740_991.0)))),
    #(
      "MIN_SAFE_INTEGER",
      value.data(JsNumber(Finite(-9_007_199_254_740_991.0))),
    ),
    #("EPSILON", value.data(JsNumber(Finite(2.220446049250313e-16)))),
  ]

  // Global utility functions (separate refs — these are standalone globals)
  let #(h, parse_int_ref) =
    common.alloc_native_fn(h, function_proto, NativeParseInt, "parseInt", 2)
  let #(h, parse_float_ref) =
    common.alloc_native_fn(h, function_proto, NativeParseFloat, "parseFloat", 1)
  let #(h, is_nan_ref) =
    common.alloc_native_fn(h, function_proto, NativeIsNaN, "isNaN", 1)
  let #(h, is_finite_ref) =
    common.alloc_native_fn(h, function_proto, NativeIsFinite, "isFinite", 1)

  let ctor_props = list.append(constants, static_methods)
  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      [],
      fn(_) { NativeNumberConstructor },
      "Number",
      1,
      ctor_props,
    )

  #(h, bt, parse_int_ref, parse_float_ref, is_nan_ref, is_finite_ref)
}

/// Number() called as a function — type coercion to number.
pub fn call_as_function(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case args {
    [] -> JsNumber(Finite(0.0))
    [val, ..] -> JsNumber(builtins_math.to_number(val))
  }
  #(state, Ok(result))
}

/// Global parseInt(string, radix?)
pub fn parse_int(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let str_result = case args {
    [val, ..] -> {
      use #(s, state) <- result.map(frame.to_string(state, val))
      #(string.trim(s), state)
    }
    [] -> Ok(#("", state))
  }
  case str_result {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(str, state)) -> {
      let radix = case args {
        [_, r, ..] ->
          case builtins_math.to_number(r) {
            Finite(n) -> {
              let r = float.truncate(n)
              case r == 0 {
                True -> 10
                False -> r
              }
            }
            _ -> 10
          }
        _ -> 10
      }
      // Handle 0x prefix for hex
      let #(str, radix) = case radix {
        16 ->
          case string.starts_with(str, "0x") || string.starts_with(str, "0X") {
            True -> #(string.drop_start(str, 2), 16)
            False -> #(str, 16)
          }
        10 ->
          case string.starts_with(str, "0x") || string.starts_with(str, "0X") {
            True -> #(string.drop_start(str, 2), 16)
            False -> #(str, 10)
          }
        _ -> #(str, radix)
      }
      case radix >= 2 && radix <= 36 {
        False -> #(state, Ok(JsNumber(NaN)))
        True -> {
          let result = parse_int_digits(str, radix)
          #(state, Ok(JsNumber(result)))
        }
      }
    }
  }
}

/// Global parseFloat(string)
pub fn parse_float(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let str_result = case args {
    [val, ..] -> {
      use #(s, state) <- result.map(frame.to_string(state, val))
      #(string.trim(s), state)
    }
    [] -> Ok(#("", state))
  }
  case str_result {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(str, state)) ->
      case str {
        "Infinity" | "+Infinity" -> #(state, Ok(JsNumber(Infinity)))
        "-Infinity" -> #(state, Ok(JsNumber(NegInfinity)))
        _ ->
          case gleam_stdlib_parse_float(str) {
            Ok(n) -> #(state, Ok(JsNumber(Finite(n))))
            Error(_) ->
              case int.parse(str) {
                Ok(n) -> #(state, Ok(JsNumber(Finite(int.to_float(n)))))
                Error(_) -> #(state, Ok(JsNumber(NaN)))
              }
          }
      }
  }
}

/// Global isNaN(value) — coerces to number first, then checks NaN.
pub fn js_is_nan(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  let num = builtins_math.to_number(val)
  let result = case num {
    NaN -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// Global isFinite(value) — coerces to number first, then checks finiteness.
pub fn js_is_finite(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  let num = builtins_math.to_number(val)
  let result = case num {
    Finite(_) -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// Number.isNaN(value) — strict: returns true ONLY if value is a number AND is NaN.
/// Unlike global isNaN(), does NOT coerce the argument.
pub fn number_is_nan(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case args {
    [JsNumber(NaN), ..] -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// Number.isFinite(value) — strict: returns true ONLY if value is a finite number.
/// Unlike global isFinite(), does NOT coerce the argument.
pub fn number_is_finite(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case args {
    [JsNumber(Finite(_)), ..] -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// Number.isInteger(value) — returns true if value is a finite number with no fractional part.
pub fn number_is_integer(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case args {
    [JsNumber(Finite(n)), ..] -> {
      let truncated = int.to_float(float.truncate(n))
      value.JsBool(truncated == n)
    }
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Parse integer digits in given radix, returning NaN if no valid digits.
fn parse_int_digits(s: String, radix: Int) -> value.JsNum {
  // Handle leading sign
  let #(s, negative) = case string.first(s) {
    Ok("-") -> #(string.drop_start(s, 1), True)
    Ok("+") -> #(string.drop_start(s, 1), False)
    _ -> #(s, False)
  }
  let graphemes = string.to_graphemes(s)
  let result = parse_digits_loop(graphemes, radix, 0, False)
  case result {
    Error(_) -> NaN
    Ok(n) ->
      case negative {
        True -> Finite(int.to_float(-n))
        False -> Finite(int.to_float(n))
      }
  }
}

fn parse_digits_loop(
  graphemes: List(String),
  radix: Int,
  acc: Int,
  found_any: Bool,
) -> Result(Int, Nil) {
  case graphemes {
    [] ->
      case found_any {
        True -> Ok(acc)
        False -> Error(Nil)
      }
    [ch, ..rest] ->
      case digit_value(ch) {
        Ok(d) if d < radix ->
          parse_digits_loop(rest, radix, acc * radix + d, True)
        _ ->
          // Stop at first invalid digit (per spec)
          case found_any {
            True -> Ok(acc)
            False -> Error(Nil)
          }
      }
  }
}

fn digit_value(ch: String) -> Result(Int, Nil) {
  case ch {
    "0" -> Ok(0)
    "1" -> Ok(1)
    "2" -> Ok(2)
    "3" -> Ok(3)
    "4" -> Ok(4)
    "5" -> Ok(5)
    "6" -> Ok(6)
    "7" -> Ok(7)
    "8" -> Ok(8)
    "9" -> Ok(9)
    "a" | "A" -> Ok(10)
    "b" | "B" -> Ok(11)
    "c" | "C" -> Ok(12)
    "d" | "D" -> Ok(13)
    "e" | "E" -> Ok(14)
    "f" | "F" -> Ok(15)
    "g" | "G" -> Ok(16)
    "h" | "H" -> Ok(17)
    "i" | "I" -> Ok(18)
    "j" | "J" -> Ok(19)
    "k" | "K" -> Ok(20)
    "l" | "L" -> Ok(21)
    "m" | "M" -> Ok(22)
    "n" | "N" -> Ok(23)
    "o" | "O" -> Ok(24)
    "p" | "P" -> Ok(25)
    "q" | "Q" -> Ok(26)
    "r" | "R" -> Ok(27)
    "s" | "S" -> Ok(28)
    "t" | "T" -> Ok(29)
    "u" | "U" -> Ok(30)
    "v" | "V" -> Ok(31)
    "w" | "W" -> Ok(32)
    "x" | "X" -> Ok(33)
    "y" | "Y" -> Ok(34)
    "z" | "Z" -> Ok(35)
    _ -> Error(Nil)
  }
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)
