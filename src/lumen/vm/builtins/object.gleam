import gleam/dict
import gleam/option.{Some}
import lumen/vm/builtins/common.{type BuiltinType, BuiltinType, set_constructor}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type JsValue, type Ref, JsObject, JsString, NativeFunction,
  NativeObjectConstructor, ObjectSlot, OrdinaryObject,
}

/// Set up Object constructor.
/// Object.prototype is already allocated (it's the root of all chains).
pub fn init(h: Heap, object_proto: Ref) -> #(Heap, BuiltinType) {
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeObjectConstructor),
        properties: dict.from_list([
          #("prototype", value.builtin_property(JsObject(object_proto))),
          #("name", value.builtin_property(JsString("Object"))),
        ]),
        elements: dict.new(),
        prototype: Some(object_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)
  let h = set_constructor(h, object_proto, ctor_ref)

  #(h, BuiltinType(prototype: object_proto, constructor: ctor_ref))
}

/// Object() / new Object() — creates a plain empty object,
/// or returns the argument if it's already an object.
pub fn call_native(
  args: List(JsValue),
  _this: JsValue,
  heap: Heap,
  object_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  case args {
    [JsObject(_) as obj, ..] -> #(heap, Ok(obj))
    _ -> {
      let #(heap, ref) =
        heap.alloc(
          heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: dict.new(),
            prototype: Some(object_proto),
          ),
        )
      #(heap, Ok(JsObject(ref)))
    }
  }
}
