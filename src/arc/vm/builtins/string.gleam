import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, Finite, JsNumber, JsObject, JsString, JsUndefined, NaN,
  NativeStringConstructor, NativeStringPrototypeAt, NativeStringPrototypeCharAt,
  NativeStringPrototypeCharCodeAt, NativeStringPrototypeConcat,
  NativeStringPrototypeEndsWith, NativeStringPrototypeIncludes,
  NativeStringPrototypeIndexOf, NativeStringPrototypeLastIndexOf,
  NativeStringPrototypePadEnd, NativeStringPrototypePadStart,
  NativeStringPrototypeRepeat, NativeStringPrototypeSlice,
  NativeStringPrototypeSplit, NativeStringPrototypeStartsWith,
  NativeStringPrototypeSubstring, NativeStringPrototypeToLowerCase,
  NativeStringPrototypeToString, NativeStringPrototypeToUpperCase,
  NativeStringPrototypeTrim, NativeStringPrototypeTrimEnd,
  NativeStringPrototypeTrimStart, NativeStringPrototypeValueOf,
}
import gleam/int
import gleam/list
import gleam/string

/// Set up String constructor + String.prototype.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("charAt", NativeStringPrototypeCharAt, 1),
      #("charCodeAt", NativeStringPrototypeCharCodeAt, 1),
      #("indexOf", NativeStringPrototypeIndexOf, 1),
      #("lastIndexOf", NativeStringPrototypeLastIndexOf, 1),
      #("includes", NativeStringPrototypeIncludes, 1),
      #("startsWith", NativeStringPrototypeStartsWith, 1),
      #("endsWith", NativeStringPrototypeEndsWith, 1),
      #("slice", NativeStringPrototypeSlice, 2),
      #("substring", NativeStringPrototypeSubstring, 2),
      #("toLowerCase", NativeStringPrototypeToLowerCase, 0),
      #("toUpperCase", NativeStringPrototypeToUpperCase, 0),
      #("trim", NativeStringPrototypeTrim, 0),
      #("trimStart", NativeStringPrototypeTrimStart, 0),
      #("trimEnd", NativeStringPrototypeTrimEnd, 0),
      #("split", NativeStringPrototypeSplit, 2),
      #("concat", NativeStringPrototypeConcat, 1),
      #("toString", NativeStringPrototypeToString, 0),
      #("valueOf", NativeStringPrototypeValueOf, 0),
      #("repeat", NativeStringPrototypeRepeat, 1),
      #("padStart", NativeStringPrototypePadStart, 1),
      #("padEnd", NativeStringPrototypePadEnd, 1),
      #("at", NativeStringPrototypeAt, 1),
    ])
  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(_) { NativeStringConstructor },
    "String",
    1,
    [],
  )
}

/// String() called as a function — type coercion to string.
pub fn call_as_function(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [] -> #(state, Ok(JsString("")))
    [val, ..] -> {
      use s, state <- frame.try_to_string(state, val)
      #(state, Ok(JsString(s)))
    }
  }
}

// ============================================================================
// String.prototype method implementations
// ============================================================================

/// String.prototype.charAt(index)
pub fn string_char_at(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let idx = helpers.get_int_arg(args, 0, 0)
      let len = string.length(s)
      case idx >= 0 && idx < len {
        True -> #(state, Ok(JsString(string.slice(s, idx, 1))))
        False -> #(state, Ok(JsString("")))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.charCodeAt(index)
pub fn string_char_code_at(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let idx = helpers.get_int_arg(args, 0, 0)
      let len = string.length(s)
      case idx >= 0 && idx < len {
        True -> {
          let ch = string.slice(s, idx, 1)
          case string.to_utf_codepoints(ch) {
            [cp, ..] -> {
              let code = string.utf_codepoint_to_int(cp)
              #(state, Ok(JsNumber(Finite(int.to_float(code)))))
            }
            [] -> #(state, Ok(JsNumber(NaN)))
          }
        }
        False -> #(state, Ok(JsNumber(NaN)))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.indexOf(searchString, position?)
pub fn string_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      use search, state <- frame.try_to_string(state, search_val)
      let from = helpers.get_int_arg(args, 1, 0)
      let result = index_of_from(s, search, from)
      #(state, Ok(JsNumber(Finite(int.to_float(result)))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.lastIndexOf(searchString, position?)
pub fn string_last_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      use search, state <- frame.try_to_string(state, search_val)
      let len = string.length(s)
      let from = case args {
        [_, pos_val, ..] ->
          case helpers.to_number_int(pos_val) {
            Ok(n) -> int.min(n, len)
            Error(_) -> len
          }
        _ -> len
      }
      let result = last_index_of_from(s, search, from)
      #(state, Ok(JsNumber(Finite(int.to_float(result)))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.includes(searchString, position?)
pub fn string_includes(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      use search, state <- frame.try_to_string(state, search_val)
      let from = helpers.get_int_arg(args, 1, 0)
      let sub = string.drop_start(s, from)
      #(state, Ok(value.JsBool(string.contains(sub, search))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.startsWith(searchString, position?)
pub fn string_starts_with(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      use search, state <- frame.try_to_string(state, search_val)
      let from = helpers.get_int_arg(args, 1, 0)
      let sub = string.drop_start(s, from)
      #(state, Ok(value.JsBool(string.starts_with(sub, search))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.endsWith(searchString, endPosition?)
pub fn string_ends_with(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      use search, state <- frame.try_to_string(state, search_val)
      let len = string.length(s)
      let end_pos = case args {
        [_, pos_val, ..] ->
          case pos_val {
            JsUndefined -> len
            _ ->
              case helpers.to_number_int(pos_val) {
                Ok(n) -> int.clamp(n, 0, len)
                Error(_) -> len
              }
          }
        _ -> len
      }
      let sub = string.slice(s, 0, end_pos)
      #(state, Ok(value.JsBool(string.ends_with(sub, search))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.slice(start, end?)
pub fn string_slice(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let len = string.length(s)
      let start = case args {
        [v, ..] ->
          case helpers.to_number_int(v) {
            Ok(n) ->
              case n < 0 {
                True -> int.max(len + n, 0)
                False -> int.min(n, len)
              }
            Error(_) -> 0
          }
        [] -> 0
      }
      let end = case args {
        [_, JsUndefined, ..] -> len
        [_, v, ..] ->
          case helpers.to_number_int(v) {
            Ok(n) ->
              case n < 0 {
                True -> int.max(len + n, 0)
                False -> int.min(n, len)
              }
            Error(_) -> 0
          }
        _ -> len
      }
      case end > start {
        True -> #(state, Ok(JsString(string.slice(s, start, end - start))))
        False -> #(state, Ok(JsString("")))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.substring(start, end?)
pub fn string_substring(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let len = string.length(s)
      let raw_start = case args {
        [v, ..] ->
          case helpers.to_number_int(v) {
            Ok(n) -> n
            Error(_) -> 0
          }
        [] -> 0
      }
      let raw_end = case args {
        [_, JsUndefined, ..] -> len
        [_, v, ..] ->
          case helpers.to_number_int(v) {
            Ok(n) -> n
            Error(_) -> 0
          }
        _ -> len
      }
      // Clamp to [0, len], then swap if start > end
      let start = int.clamp(raw_start, 0, len)
      let end = int.clamp(raw_end, 0, len)
      let #(start, end) = case start > end {
        True -> #(end, start)
        False -> #(start, end)
      }
      #(state, Ok(JsString(string.slice(s, start, end - start))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.toLowerCase()
pub fn string_to_lower_case(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.lowercase)
}

/// String.prototype.toUpperCase()
pub fn string_to_upper_case(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.uppercase)
}

/// String.prototype.trim()
pub fn string_trim(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.trim)
}

/// String.prototype.trimStart()
pub fn string_trim_start(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.trim_start)
}

/// String.prototype.trimEnd()
pub fn string_trim_end(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.trim_end)
}

/// String.prototype.split(separator, limit?)
pub fn string_split(
  this: JsValue,
  args: List(JsValue),
  state: State,
  array_proto: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      case args {
        [JsUndefined, ..] | [] -> {
          // No separator: return [this_string]
          let #(heap, ref) =
            common.alloc_array(state.heap, [JsString(s)], array_proto)
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
        [sep_val, ..] -> {
          case frame.to_string(state, sep_val) {
            Ok(#(sep, state)) -> {
              let heap = state.heap
              let parts = case sep {
                "" -> string.to_graphemes(s) |> list.map(JsString)
                _ -> string.split(s, sep) |> list.map(JsString)
              }
              // Apply limit if provided
              let parts = case args {
                [_, limit_val, ..] ->
                  case helpers.to_number_int(limit_val) {
                    Ok(limit) -> list.take(parts, limit)
                    Error(_) -> parts
                  }
                _ -> parts
              }
              let #(heap, ref) = common.alloc_array(heap, parts, array_proto)
              #(State(..state, heap:), Ok(JsObject(ref)))
            }
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        }
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.concat(...args)
pub fn string_concat(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> concat_loop(args, s, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

fn concat_loop(
  args: List(JsValue),
  acc: String,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [] -> #(state, Ok(JsString(acc)))
    [arg, ..rest] ->
      case frame.to_string(state, arg) {
        Ok(#(s, state)) -> concat_loop(rest, acc <> s, state)
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
  }
}

/// String.prototype.toString()
pub fn string_to_string(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, fn(s) { s })
}

/// String.prototype.valueOf()
pub fn string_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, fn(s) { s })
}

/// String.prototype.repeat(count)
pub fn string_repeat(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let count = helpers.get_int_arg(args, 0, 0)
      case count < 0 {
        True -> #(state, Ok(JsString("")))
        False -> #(state, Ok(JsString(string.repeat(s, count))))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.padStart(targetLength, padString?)
pub fn string_pad_start(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_pad(this, args, state, string.pad_start)
}

/// String.prototype.padEnd(targetLength, padString?)
pub fn string_pad_end(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_pad(this, args, state, string.pad_end)
}

fn string_pad(
  this: JsValue,
  args: List(JsValue),
  state: State,
  pad_fn: fn(String, Int, String) -> String,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let target_len = helpers.get_int_arg(args, 0, 0)
      case args {
        [_, v, ..] ->
          case v {
            JsUndefined -> #(state, Ok(JsString(pad_fn(s, target_len, " "))))
            _ -> {
              use pad, state <- frame.try_to_string(state, v)
              #(state, Ok(JsString(pad_fn(s, target_len, pad))))
            }
          }
        _ -> #(state, Ok(JsString(pad_fn(s, target_len, " "))))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// String.prototype.at(index) — relative indexing
pub fn string_at(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let idx = helpers.get_int_arg(args, 0, 0)
      let len = string.length(s)
      let actual_idx = case idx < 0 {
        True -> len + idx
        False -> idx
      }
      case actual_idx >= 0 && actual_idx < len {
        True -> #(state, Ok(JsString(string.slice(s, actual_idx, 1))))
        False -> #(state, Ok(JsUndefined))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Extract the string value from `this`. Primitive strings are the common case.
fn coerce_to_string(
  this: JsValue,
  state: State,
) -> Result(#(String, State), #(JsValue, State)) {
  case this {
    JsString(s) -> Ok(#(s, state))
    _ -> frame.to_string(state, this)
  }
}

/// Coerce `this` to string, apply a transformation, return the result.
fn string_transform(
  this: JsValue,
  state: State,
  transform: fn(String) -> String,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> #(state, Ok(JsString(transform(s))))
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Find first occurrence of `search` in `s` starting from index `from`.
fn index_of_from(s: String, search: String, from: Int) -> Int {
  let len = string.length(s)
  let search_len = string.length(search)
  // Empty search at valid position returns that position
  case search_len == 0 {
    True ->
      case from >= 0 && from <= len {
        True -> from
        False ->
          case from < 0 {
            True -> 0
            False -> len
          }
      }
    False -> index_of_loop(s, search, int.max(from, 0), len, search_len)
  }
}

fn index_of_loop(
  s: String,
  search: String,
  pos: Int,
  len: Int,
  search_len: Int,
) -> Int {
  case pos + search_len > len {
    True -> -1
    False ->
      case string.slice(s, pos, search_len) == search {
        True -> pos
        False -> index_of_loop(s, search, pos + 1, len, search_len)
      }
  }
}

/// Find last occurrence of `search` in `s` starting from index `from`.
fn last_index_of_from(s: String, search: String, from: Int) -> Int {
  let len = string.length(s)
  let search_len = string.length(search)
  case search_len == 0 {
    True -> int.min(from, len)
    False -> {
      let start = int.min(from, len - search_len)
      last_index_of_loop(s, search, start, search_len)
    }
  }
}

fn last_index_of_loop(
  s: String,
  search: String,
  pos: Int,
  search_len: Int,
) -> Int {
  case pos < 0 {
    True -> -1
    False ->
      case string.slice(s, pos, search_len) == search {
        True -> pos
        False -> last_index_of_loop(s, search, pos - 1, search_len)
      }
  }
}
