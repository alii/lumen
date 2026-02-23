import arc/vm/builtins/common.{
  type BuiltinType, BuiltinType, alloc_proto, set_constructor,
}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, Finite, JsNumber, JsObject, JsString,
  NativeBooleanConstructor, NativeFunction, ObjectSlot,
}
import gleam/dict
import gleam/option.{Some}

/// Set up Boolean constructor + Boolean.prototype.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, boolean_proto) = alloc_proto(h, Some(object_proto), dict.new())

  // Boolean constructor
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeBooleanConstructor),
        properties: dict.from_list([
          #("prototype", value.builtin_property(JsObject(boolean_proto))),
          #("name", value.builtin_property(JsString("Boolean"))),
          #("length", value.builtin_property(JsNumber(Finite(1.0)))),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)
  let h = set_constructor(h, boolean_proto, ctor_ref)

  #(h, BuiltinType(prototype: boolean_proto, constructor: ctor_ref))
}

/// Boolean() called as a function — type coercion to boolean.
pub fn call_as_function(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let result = case args {
    [] -> value.JsBool(False)
    [val, ..] -> value.JsBool(is_truthy(val))
  }
  #(heap, Ok(result))
}

/// JS truthiness check (duplicated from vm.gleam which keeps it private).
fn is_truthy(val: JsValue) -> Bool {
  case val {
    value.JsUndefined | value.JsNull | value.JsUninitialized -> False
    value.JsBool(b) -> b
    value.JsNumber(value.NaN) -> False
    value.JsNumber(Finite(n)) -> n != 0.0
    value.JsNumber(value.Infinity) | value.JsNumber(value.NegInfinity) -> True
    value.JsString(s) -> s != ""
    value.JsBigInt(value.BigInt(n)) -> n != 0
    value.JsObject(_) | value.JsSymbol(_) -> True
  }
}
