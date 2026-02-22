import gleam/dict
import gleam/option.{Some}
import lumen/vm/builtins/common.{alloc_proto}
import lumen/vm/heap.{type Heap}
import lumen/vm/opcode
import lumen/vm/value.{
  type Ref, FunctionObject, JsObject, JsString, JsUndefined, ObjectSlot,
}

/// All error-related prototype and constructor refs.
pub type ErrorBuiltins {
  ErrorBuiltins(
    error_prototype: Ref,
    type_error_prototype: Ref,
    reference_error_prototype: Ref,
    range_error_prototype: Ref,
    syntax_error_prototype: Ref,
    error_constructor: Ref,
    type_error_constructor: Ref,
    reference_error_constructor: Ref,
    range_error_constructor: Ref,
    syntax_error_constructor: Ref,
    constructor_templates: dict.Dict(Int, opcode.FuncTemplate),
  )
}

/// Set up all error prototypes and constructors.
/// Uses negative func_index values to avoid collision with user closures.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, ErrorBuiltins) {
  // Error.prototype — has both "name" and "message"
  let #(h, error_proto) =
    alloc_proto(
      h,
      Some(object_proto),
      dict.from_list([
        #("name", JsString("Error")),
        #("message", JsString("")),
      ]),
    )

  // Error subclass prototypes — each just sets "name", inherits from Error.prototype
  let #(h, type_error_proto) = alloc_named_proto(h, error_proto, "TypeError")
  let #(h, reference_error_proto) =
    alloc_named_proto(h, error_proto, "ReferenceError")
  let #(h, range_error_proto) = alloc_named_proto(h, error_proto, "RangeError")
  let #(h, syntax_error_proto) =
    alloc_named_proto(h, error_proto, "SyntaxError")

  // Error constructor functions
  let #(h, error_ctor, error_tmpl) =
    alloc_constructor(h, function_proto, error_proto, "Error", -1)
  let #(h, type_error_ctor, type_error_tmpl) =
    alloc_constructor(h, function_proto, type_error_proto, "TypeError", -2)
  let #(h, reference_error_ctor, reference_error_tmpl) =
    alloc_constructor(
      h,
      function_proto,
      reference_error_proto,
      "ReferenceError",
      -3,
    )
  let #(h, range_error_ctor, range_error_tmpl) =
    alloc_constructor(h, function_proto, range_error_proto, "RangeError", -4)
  let #(h, syntax_error_ctor, syntax_error_tmpl) =
    alloc_constructor(h, function_proto, syntax_error_proto, "SyntaxError", -5)

  let constructor_templates =
    dict.from_list([
      #(-1, error_tmpl),
      #(-2, type_error_tmpl),
      #(-3, reference_error_tmpl),
      #(-4, range_error_tmpl),
      #(-5, syntax_error_tmpl),
    ])

  #(
    h,
    ErrorBuiltins(
      error_prototype: error_proto,
      type_error_prototype: type_error_proto,
      reference_error_prototype: reference_error_proto,
      range_error_prototype: range_error_proto,
      syntax_error_prototype: syntax_error_proto,
      error_constructor: error_ctor,
      type_error_constructor: type_error_ctor,
      reference_error_constructor: reference_error_ctor,
      range_error_constructor: range_error_ctor,
      syntax_error_constructor: syntax_error_ctor,
      constructor_templates:,
    ),
  )
}

/// Shorthand: alloc a prototype with just a "name" property.
fn alloc_named_proto(h: Heap, parent: Ref, name: String) -> #(Heap, Ref) {
  alloc_proto(h, Some(parent), dict.from_list([#("name", JsString(name))]))
}

/// Create a FuncTemplate for an Error constructor.
/// Bytecode: if (message !== undefined) this.message = message;
fn constructor_template(name: String) -> opcode.FuncTemplate {
  opcode.FuncTemplate(
    name: Some(name),
    arity: 1,
    local_count: 1,
    bytecode: [
      opcode.GetLocal(0),
      opcode.PushConst(0),
      opcode.BinOp(opcode.StrictNotEq),
      opcode.JumpIfFalse(7),
      opcode.GetThis,
      opcode.GetLocal(0),
      opcode.PutField("message"),
      opcode.Pop,
      // PC 8:
      opcode.PushConst(0),
      opcode.Return,
    ],
    constants: [JsUndefined],
    functions: [],
    env_descriptors: [],
    is_strict: False,
    is_arrow: False,
  )
}

/// Allocate an Error constructor function object on the heap.
/// Sets .prototype on the constructor and .constructor on the prototype.
fn alloc_constructor(
  h: Heap,
  function_proto: Ref,
  error_proto_ref: Ref,
  name: String,
  func_index: Int,
) -> #(Heap, Ref, opcode.FuncTemplate) {
  let template = constructor_template(name)

  // Empty env (no captures)
  let #(h, env_ref) = heap.alloc(h, value.EnvSlot([]))

  // Constructor FunctionObject with .prototype and .name
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: FunctionObject(func_index:, env: env_ref),
        properties: dict.from_list([
          #("prototype", JsObject(error_proto_ref)),
          #("name", JsString(name)),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ctor_ref)

  // Set .constructor on the prototype pointing back
  let h = case heap.read(h, error_proto_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let new_props = dict.insert(properties, "constructor", JsObject(ctor_ref))
      heap.write(
        h,
        error_proto_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }

  #(h, ctor_ref, template)
}
