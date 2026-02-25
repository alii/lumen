import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsElements, type JsValue, type Ref, ArrayObject, JsBool, JsNull, JsNumber,
  JsObject, JsString, JsUndefined, NativeArrayConstructor, NativeArrayIsArray,
  NativeArrayPrototypeJoin, NativeArrayPrototypePush, ObjectSlot,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string

/// Set up Array.prototype and Array constructor.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("join", NativeArrayPrototypeJoin, 1),
      #("push", NativeArrayPrototypePush, 1),
    ])
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("isArray", NativeArrayIsArray, 1),
    ])
  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(_) { NativeArrayConstructor },
    "Array",
    1,
    static_methods,
  )
}

/// Array() / new Array() — construct a new array.
pub fn construct(
  args: List(JsValue),
  state: State,
  array_proto: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  let #(heap, result) = native_array_constructor(args, heap, array_proto)
  #(State(..state, heap:), result)
}

/// Array.isArray(value) — check if a value is an array.
pub fn is_array(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  #(State(..state, heap:), Ok(native_is_array(args, heap)))
}

/// Array() / new Array() constructor.
/// - No args: empty array
/// - One numeric arg: array with that length (empty elements)
/// - Otherwise: array from the given elements
fn native_array_constructor(
  args: List(JsValue),
  heap: Heap,
  array_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  let #(length, elements) = case args {
    [] -> #(0, js_elements.new())
    [JsNumber(value.Finite(n))] -> {
      let len = value.float_to_int(n)
      case len >= 0 && int.to_float(len) == n {
        True -> #(len, js_elements.new())
        // Non-integer or negative: treat as single element
        False -> #(1, js_elements.from_list([JsNumber(value.Finite(n))]))
      }
    }
    _ -> {
      let count = list.length(args)
      #(count, js_elements.from_list(args))
    }
  }
  let #(heap, ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: ArrayObject(length),
        properties: dict.new(),
        elements:,
        prototype: Some(array_proto),
        symbol_properties: dict.new(),
      ),
    )
  #(heap, Ok(JsObject(ref)))
}

/// Array.isArray(value) — returns true if value is an ArrayObject on the heap.
fn native_is_array(args: List(JsValue), heap: Heap) -> JsValue {
  case args {
    [JsObject(ref), ..] ->
      case heap.read(heap, ref) {
        Ok(ObjectSlot(kind: ArrayObject(_), ..)) -> JsBool(True)
        _ -> JsBool(False)
      }
    _ -> JsBool(False)
  }
}

/// Array.prototype.join(separator)
/// Joins array elements into a string with the given separator (default ",").
pub fn array_join(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Ok(ObjectSlot(kind: ArrayObject(length:), elements:, ..)) -> {
          let sep_val = case args {
            [JsUndefined, ..] | [] -> JsString(",")
            [v, ..] -> v
          }
          case frame.to_string(state, sep_val) {
            Ok(#(separator, state)) ->
              case join_elements(elements, 0, length, separator, [], state) {
                #(state, Ok(result)) -> #(state, Ok(JsString(result)))
                #(state, Error(thrown)) -> #(state, Error(thrown))
              }
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        }
        _ -> #(state, Ok(JsString("")))
      }
    _ -> #(state, Ok(JsString("")))
  }
}

/// Iterate elements 0..length-1, converting each to string.
/// undefined/null → empty string per spec.
fn join_elements(
  elements: JsElements,
  idx: Int,
  length: Int,
  separator: String,
  acc: List(String),
  state: State,
) -> #(State, Result(String, JsValue)) {
  case idx >= length {
    True -> #(state, Ok(acc |> list.reverse |> string.join(separator)))
    False -> {
      let val = js_elements.get(elements, idx)
      case val {
        JsUndefined | JsNull ->
          join_elements(
            elements,
            idx + 1,
            length,
            separator,
            ["", ..acc],
            state,
          )
        _ ->
          case frame.to_string(state, val) {
            Ok(#(str, state)) ->
              join_elements(
                elements,
                idx + 1,
                length,
                separator,
                [str, ..acc],
                state,
              )
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
      }
    }
  }
}

/// Array.prototype.push(...items)
/// Appends items to the array and returns the new length.
pub fn array_push(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  case this {
    JsObject(ref) ->
      case heap.read(heap, ref) {
        Ok(ObjectSlot(
          kind: ArrayObject(length:),
          properties:,
          elements:,
          prototype:,
          symbol_properties:,
        )) -> {
          let #(new_elements, new_length) =
            push_elements(elements, args, length)
          let heap =
            heap.write(
              heap,
              ref,
              ObjectSlot(
                kind: ArrayObject(new_length),
                properties:,
                elements: new_elements,
                prototype:,
                symbol_properties:,
              ),
            )
          #(
            State(..state, heap:),
            Ok(JsNumber(value.Finite(int.to_float(new_length)))),
          )
        }
        _ -> #(State(..state, heap:), Ok(JsNumber(value.Finite(0.0))))
      }
    _ -> #(State(..state, heap:), Ok(JsNumber(value.Finite(0.0))))
  }
}

/// Insert args into elements starting at current length.
fn push_elements(
  elements: JsElements,
  args: List(JsValue),
  start: Int,
) -> #(JsElements, Int) {
  case args {
    [] -> #(elements, start)
    [val, ..rest] ->
      push_elements(js_elements.set(elements, start, val), rest, start + 1)
  }
}
