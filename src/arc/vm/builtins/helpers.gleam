/// Shared runtime helpers for builtins that can't import higher-level modules
/// like object.gleam (due to import cycles: object -> builtins -> builtins/*).
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, DataProperty, Finite, JsNumber, JsObject, JsString,
  JsUndefined, NaN, ObjectSlot,
}
import gleam/dict
import gleam/int
import gleam/option.{type Option, None, Some}

/// Walk the prototype chain to find a property by key.
/// Lightweight version of object.get_property that avoids the import cycle.
/// Checks own properties, then follows prototype links.
pub fn get_property_chain(
  h: Heap,
  ref: Ref,
  key: String,
) -> Result(JsValue, Nil) {
  case heap.read(h, ref) {
    Ok(ObjectSlot(properties:, prototype:, ..)) ->
      case dict.get(properties, key) {
        Ok(DataProperty(value: val, ..)) -> Ok(val)
        Error(_) ->
          case prototype {
            Some(proto_ref) -> get_property_chain(h, proto_ref, key)
            None -> Error(Nil)
          }
      }
    _ -> Error(Nil)
  }
}

/// Check if a JsValue is callable (a function object or native function).
pub fn is_callable(h: Heap, val: JsValue) -> Bool {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Ok(ObjectSlot(kind: value.FunctionObject(..), ..)) -> True
        Ok(ObjectSlot(kind: value.NativeFunction(_), ..)) -> True
        _ -> False
      }
    _ -> False
  }
}

/// Get element at index from a list (0-based). O(n).
pub fn list_at(lst: List(a), idx: Int) -> Option(a) {
  case idx, lst {
    0, [x, ..] -> Some(x)
    n, [_, ..rest] -> list_at(rest, n - 1)
    _, [] -> None
  }
}

/// Convert a JsValue to an integer (truncating floats).
/// Returns Error(Nil) for NaN, Infinity, undefined.
pub fn to_number_int(val: JsValue) -> Result(Int, Nil) {
  case val {
    JsNumber(Finite(n)) -> Ok(value.float_to_int(n))
    JsNumber(_) -> Error(Nil)
    JsUndefined -> Error(Nil)
    value.JsNull -> Ok(0)
    value.JsBool(True) -> Ok(1)
    value.JsBool(False) -> Ok(0)
    JsString(s) ->
      case int.parse(s) {
        Ok(n) -> Ok(n)
        Error(_) ->
          case gleam_stdlib_parse_float(s) {
            Ok(f) -> Ok(value.float_to_int(f))
            Error(_) -> Error(Nil)
          }
      }
    _ -> Error(Nil)
  }
}

/// Get an integer argument at position `idx`, with a default if missing or not numeric.
pub fn get_int_arg(args: List(JsValue), idx: Int, default: Int) -> Int {
  case list_at(args, idx) {
    Some(v) ->
      case to_number_int(v) {
        Ok(n) -> n
        Error(_) -> default
      }
    None -> default
  }
}

/// Get a numeric (JsNum) argument at position `idx`, defaulting to NaN.
pub fn get_num_arg(
  args: List(JsValue),
  idx: Int,
  to_number: fn(JsValue) -> value.JsNum,
) -> value.JsNum {
  case list_at(args, idx) {
    Some(v) -> to_number(v)
    None -> NaN
  }
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)
