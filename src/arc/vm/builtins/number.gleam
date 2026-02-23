import arc/vm/builtins/common.{
  type BuiltinType, BuiltinType, alloc_proto, set_constructor,
}
import arc/vm/builtins/math as builtins_math
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, Finite, Infinity, JsNumber, JsObject, JsString,
  JsUndefined, NaN, NativeFunction, NativeIsFinite, NativeIsNaN,
  NativeNumberConstructor, NativeNumberIsFinite, NativeNumberIsInteger,
  NativeNumberIsNaN, NativeNumberParseFloat, NativeNumberParseInt,
  NativeParseFloat, NativeParseInt, NegInfinity, ObjectSlot,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/option.{Some}
import gleam/string

/// Set up Number constructor + Number.prototype + global parseInt/parseFloat/isNaN/isFinite.
/// Returns #(Heap, BuiltinType, parse_int_ref, parse_float_ref, is_nan_ref, is_finite_ref).
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType, Ref, Ref, Ref, Ref) {
  let #(h, number_proto) = alloc_proto(h, Some(object_proto), dict.new())

  // Number constructor
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeNumberConstructor),
        properties: dict.from_list([
          #("prototype", value.builtin_property(JsObject(number_proto))),
          #("name", value.builtin_property(JsString("Number"))),
          #("length", value.builtin_property(JsNumber(Finite(1.0)))),
          // Static constants
          #("NaN", constant_property(JsNumber(NaN))),
          #("POSITIVE_INFINITY", constant_property(JsNumber(Infinity))),
          #("NEGATIVE_INFINITY", constant_property(JsNumber(NegInfinity))),
          #(
            "MAX_SAFE_INTEGER",
            constant_property(JsNumber(Finite(9_007_199_254_740_991.0))),
          ),
          #(
            "MIN_SAFE_INTEGER",
            constant_property(JsNumber(Finite(-9_007_199_254_740_991.0))),
          ),
          #(
            "EPSILON",
            constant_property(JsNumber(Finite(2.220446049250313e-16))),
          ),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)
  let h = set_constructor(h, number_proto, ctor_ref)

  // Static methods on Number (strict — no coercion)
  let #(h, nisnan_ref) =
    alloc_native_fn(h, function_proto, NativeNumberIsNaN, "isNaN", 1)
  let h = add_method(h, ctor_ref, "isNaN", nisnan_ref)

  let #(h, nisfinite_ref) =
    alloc_native_fn(h, function_proto, NativeNumberIsFinite, "isFinite", 1)
  let h = add_method(h, ctor_ref, "isFinite", nisfinite_ref)

  let #(h, nisinteger_ref) =
    alloc_native_fn(h, function_proto, NativeNumberIsInteger, "isInteger", 1)
  let h = add_method(h, ctor_ref, "isInteger", nisinteger_ref)

  // Number.parseInt/parseFloat are the same as global ones per spec
  let #(h, nparseint_ref) =
    alloc_native_fn(h, function_proto, NativeNumberParseInt, "parseInt", 2)
  let h = add_method(h, ctor_ref, "parseInt", nparseint_ref)

  let #(h, nparsefloat_ref) =
    alloc_native_fn(h, function_proto, NativeNumberParseFloat, "parseFloat", 1)
  let h = add_method(h, ctor_ref, "parseFloat", nparsefloat_ref)

  // Global utility functions (separate refs — these are standalone globals)
  let #(h, parse_int_ref) =
    alloc_native_fn(h, function_proto, NativeParseInt, "parseInt", 2)
  let #(h, parse_float_ref) =
    alloc_native_fn(h, function_proto, NativeParseFloat, "parseFloat", 1)
  let #(h, is_nan_ref) =
    alloc_native_fn(h, function_proto, NativeIsNaN, "isNaN", 1)
  let #(h, is_finite_ref) =
    alloc_native_fn(h, function_proto, NativeIsFinite, "isFinite", 1)

  #(
    h,
    BuiltinType(prototype: number_proto, constructor: ctor_ref),
    parse_int_ref,
    parse_float_ref,
    is_nan_ref,
    is_finite_ref,
  )
}

/// Number() called as a function — type coercion to number.
pub fn call_as_function(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let result = case args {
    [] -> JsNumber(Finite(0.0))
    [val, ..] -> JsNumber(builtins_math.to_number(val))
  }
  #(heap, Ok(result))
}

/// Global parseInt(string, radix?)
pub fn parse_int(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let str = case args {
    [val, ..] -> string.trim(value.to_js_string(val))
    [] -> ""
  }
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
    False -> #(heap, Ok(JsNumber(NaN)))
    True -> {
      let result = parse_int_digits(str, radix)
      #(heap, Ok(JsNumber(result)))
    }
  }
}

/// Global parseFloat(string)
pub fn parse_float(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let str = case args {
    [val, ..] -> string.trim(value.to_js_string(val))
    [] -> ""
  }
  case str {
    "Infinity" | "+Infinity" -> #(heap, Ok(JsNumber(Infinity)))
    "-Infinity" -> #(heap, Ok(JsNumber(NegInfinity)))
    _ ->
      case gleam_stdlib_parse_float(str) {
        Ok(n) -> #(heap, Ok(JsNumber(Finite(n))))
        Error(_) ->
          case int.parse(str) {
            Ok(n) -> #(heap, Ok(JsNumber(Finite(int.to_float(n)))))
            Error(_) -> #(heap, Ok(JsNumber(NaN)))
          }
      }
  }
}

/// Global isNaN(value) — coerces to number first, then checks NaN.
pub fn js_is_nan(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  let num = builtins_math.to_number(val)
  let result = case num {
    NaN -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(heap, Ok(result))
}

/// Global isFinite(value) — coerces to number first, then checks finiteness.
pub fn js_is_finite(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  let num = builtins_math.to_number(val)
  let result = case num {
    Finite(_) -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(heap, Ok(result))
}

/// Number.isNaN(value) — strict: returns true ONLY if value is a number AND is NaN.
/// Unlike global isNaN(), does NOT coerce the argument.
pub fn number_is_nan(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let result = case args {
    [JsNumber(NaN), ..] -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(heap, Ok(result))
}

/// Number.isFinite(value) — strict: returns true ONLY if value is a finite number.
/// Unlike global isFinite(), does NOT coerce the argument.
pub fn number_is_finite(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let result = case args {
    [JsNumber(Finite(_)), ..] -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(heap, Ok(result))
}

/// Number.isInteger(value) — returns true if value is a finite number with no fractional part.
pub fn number_is_integer(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let result = case args {
    [JsNumber(Finite(n)), ..] -> {
      let truncated = int.to_float(float.truncate(n))
      value.JsBool(truncated == n)
    }
    _ -> value.JsBool(False)
  }
  #(heap, Ok(result))
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

fn constant_property(val: JsValue) -> value.Property {
  value.DataProperty(
    value: val,
    writable: False,
    enumerable: False,
    configurable: False,
  )
}

/// Allocate a native function object on the heap, root it, and return the ref.
fn alloc_native_fn(
  h: Heap,
  function_proto: Ref,
  native: value.NativeFn,
  name: String,
  length: Int,
) -> #(Heap, Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(native),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString(name))),
          #(
            "length",
            value.builtin_property(JsNumber(Finite(int.to_float(length)))),
          ),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Add a non-enumerable method property to an object on the heap.
fn add_method(h: Heap, obj_ref: Ref, name: String, fn_ref: Ref) -> Heap {
  case heap.read(h, obj_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let new_props =
        dict.insert(properties, name, value.builtin_property(JsObject(fn_ref)))
      heap.write(
        h,
        obj_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)
