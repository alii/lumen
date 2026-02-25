import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State}
import arc/vm/heap.{type Heap}
import arc/vm/value.{type JsValue, type Ref, NativeBooleanConstructor}

/// Set up Boolean constructor + Boolean.prototype.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  common.init_type(
    h,
    object_proto,
    function_proto,
    [],
    fn(_) { NativeBooleanConstructor },
    "Boolean",
    1,
    [],
  )
}

/// Boolean() called as a function — type coercion to boolean.
pub fn call_as_function(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case args {
    [] -> value.JsBool(False)
    [val, ..] -> value.JsBool(value.is_truthy(val))
  }
  #(state, Ok(result))
}
