import arc/vm/builtins/common.{
  type BuiltinType, BuiltinType, alloc_proto, set_constructor,
}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, JsObject, JsString, JsUndefined,
  NativeErrorConstructor, NativeFunction, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/option.{Some}

/// All error-related builtin types.
pub type ErrorBuiltins {
  ErrorBuiltins(
    error: BuiltinType,
    type_error: BuiltinType,
    reference_error: BuiltinType,
    range_error: BuiltinType,
    syntax_error: BuiltinType,
  )
}

/// Set up all error prototypes and constructors as NativeFunctions.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, ErrorBuiltins) {
  // Error.prototype — has both "name" and "message" (NOT enumerable)
  let #(h, error_proto) =
    alloc_proto(
      h,
      Some(object_proto),
      dict.from_list([
        #("name", value.builtin_property(JsString("Error"))),
        #("message", value.builtin_property(JsString(""))),
      ]),
    )

  // Error subclass prototypes
  let #(h, type_error_proto) = alloc_named_proto(h, error_proto, "TypeError")
  let #(h, reference_error_proto) =
    alloc_named_proto(h, error_proto, "ReferenceError")
  let #(h, range_error_proto) = alloc_named_proto(h, error_proto, "RangeError")
  let #(h, syntax_error_proto) =
    alloc_named_proto(h, error_proto, "SyntaxError")

  // Error constructors — all NativeFunction with embedded proto ref
  let #(h, error_ctor) =
    alloc_error_ctor(h, function_proto, error_proto, "Error")
  let #(h, type_error_ctor) =
    alloc_error_ctor(h, function_proto, type_error_proto, "TypeError")
  let #(h, reference_error_ctor) =
    alloc_error_ctor(h, function_proto, reference_error_proto, "ReferenceError")
  let #(h, range_error_ctor) =
    alloc_error_ctor(h, function_proto, range_error_proto, "RangeError")
  let #(h, syntax_error_ctor) =
    alloc_error_ctor(h, function_proto, syntax_error_proto, "SyntaxError")

  #(
    h,
    ErrorBuiltins(
      error: BuiltinType(prototype: error_proto, constructor: error_ctor),
      type_error: BuiltinType(
        prototype: type_error_proto,
        constructor: type_error_ctor,
      ),
      reference_error: BuiltinType(
        prototype: reference_error_proto,
        constructor: reference_error_ctor,
      ),
      range_error: BuiltinType(
        prototype: range_error_proto,
        constructor: range_error_ctor,
      ),
      syntax_error: BuiltinType(
        prototype: syntax_error_proto,
        constructor: syntax_error_ctor,
      ),
    ),
  )
}

/// Native error constructor: if (message !== undefined) this.message = message
/// Creates a new error object with the proto embedded in the NativeFn.
/// Error instance "message" IS enumerable (data_property).
pub fn call_native(
  proto: Ref,
  args: List(JsValue),
  _this: JsValue,
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let props = case args {
    [JsUndefined, ..] | [] -> dict.new()
    [JsString(msg), ..] ->
      dict.from_list([#("message", value.data_property(JsString(msg)))])
    [other, ..] ->
      dict.from_list([
        #("message", value.data_property(JsString(to_string(other)))),
      ])
  }
  let #(heap, ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: props,
        elements: dict.new(),
        prototype: Some(proto),
      ),
    )
  #(heap, Ok(JsObject(ref)))
}

fn alloc_named_proto(h: Heap, parent: Ref, name: String) -> #(Heap, Ref) {
  alloc_proto(
    h,
    Some(parent),
    dict.from_list([#("name", value.builtin_property(JsString(name)))]),
  )
}

/// Allocate a NativeFunction error constructor on the heap.
fn alloc_error_ctor(
  h: Heap,
  function_proto: Ref,
  error_proto: Ref,
  name: String,
) -> #(Heap, Ref) {
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativeErrorConstructor(proto: error_proto)),
        properties: dict.from_list([
          #("prototype", value.builtin_property(JsObject(error_proto))),
          #("name", value.builtin_property(JsString(name))),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)
  let h = set_constructor(h, error_proto, ctor_ref)
  #(h, ctor_ref)
}

/// Simple toString for non-string error messages.
fn to_string(val: JsValue) -> String {
  value.to_js_string(val)
}
