import gleam/dict.{type Dict}
import gleam/option.{None}
import lumen/vm/builtins/array as builtins_array
import lumen/vm/builtins/common
import lumen/vm/builtins/error as builtins_error
import lumen/vm/builtins/function as builtins_function
import lumen/vm/heap.{type Heap}
import lumen/vm/opcode.{type FuncTemplate}
import lumen/vm/value.{type Ref}

/// Pre-allocated prototype objects and constructor functions for JS built-ins.
/// All refs are rooted so GC never collects them.
pub type Builtins {
  Builtins(
    object_prototype: Ref,
    function_prototype: Ref,
    array_prototype: Ref,
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
    constructor_templates: Dict(Int, FuncTemplate),
  )
}

/// Allocate and root all built-in prototype objects on the heap.
/// Must be called once before running any JS code.
///
/// Prototype chain:
///   Object.prototype         → None (end of chain)
///   Function.prototype       → Object.prototype
///   Array.prototype          → Object.prototype
///   Error.prototype          → Object.prototype
///   TypeError.prototype      → Error.prototype
///   ReferenceError.prototype → Error.prototype
///   RangeError.prototype     → Error.prototype
///   SyntaxError.prototype    → Error.prototype
pub fn init(h: Heap) -> #(Heap, Builtins) {
  // Object.prototype — the root of all prototype chains
  let #(h, object_proto) = common.alloc_proto(h, None, dict.new())

  // Core prototypes
  let #(h, function_proto) = builtins_function.init(h, object_proto)
  let #(h, array_proto) = builtins_array.init(h, object_proto)

  // Error prototypes + constructors
  let #(h, errors) = builtins_error.init(h, object_proto, function_proto)

  #(
    h,
    Builtins(
      object_prototype: object_proto,
      function_prototype: function_proto,
      array_prototype: array_proto,
      error_prototype: errors.error_prototype,
      type_error_prototype: errors.type_error_prototype,
      reference_error_prototype: errors.reference_error_prototype,
      range_error_prototype: errors.range_error_prototype,
      syntax_error_prototype: errors.syntax_error_prototype,
      error_constructor: errors.error_constructor,
      type_error_constructor: errors.type_error_constructor,
      reference_error_constructor: errors.reference_error_constructor,
      range_error_constructor: errors.range_error_constructor,
      syntax_error_constructor: errors.syntax_error_constructor,
      constructor_templates: errors.constructor_templates,
    ),
  )
}
