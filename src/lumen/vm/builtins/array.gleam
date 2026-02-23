import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{Some}
import lumen/vm/builtins/common.{
  type BuiltinType, BuiltinType, alloc_proto, set_constructor,
}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type JsValue, type Ref, ArrayObject, JsBool, JsNumber, JsObject, JsString,
  NativeArrayConstructor, NativeArrayIsArray, NativeFunction, ObjectSlot,
}

/// Set up Array.prototype and Array constructor.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  // Array.prototype — inherits from Object.prototype
  let #(h, array_proto) = alloc_proto(h, Some(object_proto), dict.new())

  // Array constructor — a NativeFunction
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeArrayConstructor),
        properties: dict.from_list([
          #("prototype", JsObject(array_proto)),
          #("name", JsString("Array")),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)

  // Array.isArray — static method on the constructor
  let #(h, is_array_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeArrayIsArray),
        properties: dict.from_list([#("name", JsString("isArray"))]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, is_array_ref)

  // Set Array.isArray on constructor
  let h = case heap.read(h, ctor_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let new_props = dict.insert(properties, "isArray", JsObject(is_array_ref))
      heap.write(
        h,
        ctor_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }

  let h = set_constructor(h, array_proto, ctor_ref)

  #(h, BuiltinType(prototype: array_proto, constructor: ctor_ref))
}

/// Array() / new Array() — construct a new array.
pub fn construct(
  args: List(JsValue),
  heap: Heap,
  array_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  native_array_constructor(args, heap, array_proto)
}

/// Array.isArray(value) — check if a value is an array.
pub fn is_array(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  #(heap, Ok(native_is_array(args, heap)))
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
    [] -> #(0, dict.new())
    [JsNumber(value.Finite(n))] -> {
      let len = float_to_int(n)
      case len >= 0 && int.to_float(len) == n {
        True -> #(len, dict.new())
        // Non-integer or negative: treat as single element
        False -> #(1, dict.from_list([#(0, JsNumber(value.Finite(n)))]))
      }
    }
    _ -> {
      let count = list.length(args)
      let elems =
        args
        |> list.index_map(fn(val, idx) { #(idx, val) })
        |> dict.from_list()
      #(count, elems)
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

fn float_to_int(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float.truncate(float.negate(f))
    False -> float.truncate(f)
  }
}
