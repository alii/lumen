import gleam/dict
import gleam/option.{Some}
import lumen/vm/builtins/common.{
  type BuiltinType, BuiltinType, alloc_proto, set_constructor,
}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type Ref, JsObject, JsString, NativeFunction, NativeFunctionConstructor,
  ObjectSlot,
}

/// Set up Function.prototype and Function constructor.
pub fn init(h: Heap, object_proto: Ref) -> #(Heap, BuiltinType) {
  let #(h, func_proto) = alloc_proto(h, Some(object_proto), dict.new())

  // Function constructor — stub (Function("return 1") needs eval)
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeFunctionConstructor),
        properties: dict.from_list([
          #("prototype", JsObject(func_proto)),
          #("name", JsString("Function")),
        ]),
        elements: dict.new(),
        prototype: Some(func_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)
  let h = set_constructor(h, func_proto, ctor_ref)

  #(h, BuiltinType(prototype: func_proto, constructor: ctor_ref))
}
