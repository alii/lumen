import gleam/dict
import gleam/option.{Some}
import lumen/vm/builtins/common.{
  type BuiltinType, BuiltinType, alloc_proto, set_constructor,
}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type Ref, JsObject, JsString, NativeFunction, NativeFunctionApply,
  NativeFunctionBind, NativeFunctionCall, NativeFunctionConstructor, ObjectSlot,
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
          #("prototype", value.builtin_property(JsObject(func_proto))),
          #("name", value.builtin_property(JsString("Function"))),
        ]),
        elements: dict.new(),
        prototype: Some(func_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)
  let h = set_constructor(h, func_proto, ctor_ref)

  // Function.prototype.call
  let #(h, call_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeFunctionCall),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString("call"))),
          #("length", value.builtin_property(value.JsNumber(value.Finite(1.0)))),
        ]),
        elements: dict.new(),
        prototype: Some(func_proto),
      ),
    )
  let h = heap.root(h, call_ref)

  // Function.prototype.apply
  let #(h, apply_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeFunctionApply),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString("apply"))),
          #("length", value.builtin_property(value.JsNumber(value.Finite(2.0)))),
        ]),
        elements: dict.new(),
        prototype: Some(func_proto),
      ),
    )
  let h = heap.root(h, apply_ref)

  // Function.prototype.bind
  let #(h, bind_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeFunctionBind),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString("bind"))),
          #("length", value.builtin_property(value.JsNumber(value.Finite(1.0)))),
        ]),
        elements: dict.new(),
        prototype: Some(func_proto),
      ),
    )
  let h = heap.root(h, bind_ref)

  // Add call, apply, bind as non-enumerable methods on Function.prototype
  let h = add_method(h, func_proto, "call", call_ref)
  let h = add_method(h, func_proto, "apply", apply_ref)
  let h = add_method(h, func_proto, "bind", bind_ref)

  #(h, BuiltinType(prototype: func_proto, constructor: ctor_ref))
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
