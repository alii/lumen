import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type NativeFn, type Property, type Ref, ArrayObject, JsObject,
  JsString, NativeFunction, ObjectSlot, OrdinaryObject,
}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, Some}

/// A prototype + constructor pair. Every JS builtin type has both.
pub type BuiltinType {
  BuiltinType(prototype: Ref, constructor: Ref)
}

/// Generator.prototype ref, used as the prototype for generator objects.
/// Generators don't have a user-visible constructor.
pub type GeneratorBuiltin {
  GeneratorBuiltin(prototype: Ref)
}

/// Pre-allocated prototype objects and constructor functions for JS built-ins.
/// All refs are rooted so GC never collects them.
pub type Builtins {
  Builtins(
    object: BuiltinType,
    function: BuiltinType,
    array: BuiltinType,
    error: BuiltinType,
    type_error: BuiltinType,
    reference_error: BuiltinType,
    range_error: BuiltinType,
    syntax_error: BuiltinType,
    math: Ref,
    string: BuiltinType,
    number: BuiltinType,
    boolean: BuiltinType,
    parse_int: Ref,
    parse_float: Ref,
    is_nan: Ref,
    is_finite: Ref,
    promise: BuiltinType,
    generator: GeneratorBuiltin,
    symbol: Ref,
  )
}

/// Allocate an ordinary prototype object on the heap, root it, and return
/// the updated heap + ref. Shared helper for all builtin modules.
pub fn alloc_proto(
  h: Heap,
  prototype: Option(Ref),
  properties: Dict(String, Property),
) -> #(Heap, Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        elements: js_elements.new(),
        prototype:,
        symbol_properties: dict.new(),
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Allocate a NativeFunction ObjectSlot with standard name/length properties.
pub fn alloc_native_fn(
  h: Heap,
  function_proto: Ref,
  native: NativeFn,
  name: String,
  arity: Int,
) -> #(Heap, Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(native),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString(name))),
          #(
            "length",
            value.builtin_property(
              value.JsNumber(value.Finite(int.to_float(arity))),
            ),
          ),
        ]),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Allocate N native function objects from specs, returning builtin_property
/// entries. Replaces the identical fold duplicated across builtin modules.
pub fn alloc_methods(
  h: Heap,
  function_proto: Ref,
  specs: List(#(String, NativeFn, Int)),
) -> #(Heap, List(#(String, Property))) {
  list.fold(specs, #(h, []), fn(acc, spec) {
    let #(h, props) = acc
    let #(name, native, arity) = spec
    let #(h, fn_ref) = alloc_native_fn(h, function_proto, native, name, arity)
    #(h, [#(name, value.builtin_property(JsObject(fn_ref))), ..props])
  })
}

/// Build the standard ctor properties list: prototype + name + length + extras.
fn ctor_properties(
  proto: Ref,
  name: String,
  arity: Int,
  extras: List(#(String, Property)),
) -> List(#(String, Property)) {
  [
    #("prototype", value.builtin_property(JsObject(proto))),
    #("name", value.builtin_property(JsString(name))),
    #(
      "length",
      value.builtin_property(value.JsNumber(value.Finite(int.to_float(arity)))),
    ),
    ..extras
  ]
}

/// Build the standard proto properties list: constructor + extras.
fn proto_properties(
  ctor_ref: Ref,
  extras: List(#(String, Property)),
) -> List(#(String, Property)) {
  [#("constructor", value.builtin_property(JsObject(ctor_ref))), ..extras]
}

/// Full proto-ctor cycle for a new builtin type using forward references.
///
/// Reserves the proto ref first, then allocates both objects in one pass —
/// no read-modify-write. Both proto and constructor are written exactly once.
/// This is the common case for most builtins.
pub fn init_type(
  h: Heap,
  parent_proto: Ref,
  function_proto: Ref,
  proto_props: List(#(String, Property)),
  ctor_fn: fn(Ref) -> NativeFn,
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
) -> #(Heap, BuiltinType) {
  // Reserve proto address — no data written yet
  let #(h, proto_ref) = heap.reserve(h)
  let h = heap.root(h, proto_ref)

  // Allocate constructor — proto_ref is already known via forward reference
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(ctor_fn(proto_ref)),
        properties: dict.from_list(ctor_properties(
          proto_ref,
          name,
          arity,
          ctor_props,
        )),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
      ),
    )
  let h = heap.root(h, ctor_ref)

  // Fill reserved proto — ctor_ref is now known, single write with all properties
  let h =
    heap.fill(
      h,
      proto_ref,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list(proto_properties(ctor_ref, proto_props)),
        elements: js_elements.new(),
        prototype: Some(parent_proto),
        symbol_properties: dict.new(),
      ),
    )

  #(h, BuiltinType(prototype: proto_ref, constructor: ctor_ref))
}

/// Proto-ctor cycle for a pre-allocated prototype (Object, Function bootstrap).
///
/// The proto already exists on the heap (allocated empty for bootstrap reasons).
/// Reads its current state, merges in proto_props + constructor, writes back.
/// This read-modify-write is unavoidable for pre-existing protos.
pub fn init_type_on(
  h: Heap,
  proto: Ref,
  function_proto: Ref,
  proto_props: List(#(String, Property)),
  ctor_fn: fn(Ref) -> NativeFn,
  name: String,
  arity: Int,
  ctor_props: List(#(String, Property)),
) -> #(Heap, BuiltinType) {
  // Allocate constructor — proto ref already known
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(ctor_fn(proto)),
        properties: dict.from_list(ctor_properties(
          proto,
          name,
          arity,
          ctor_props,
        )),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
      ),
    )
  let h = heap.root(h, ctor_ref)

  // Read-modify-write: merge proto_props + constructor onto existing proto
  let assert Ok(ObjectSlot(
    kind:,
    properties:,
    elements:,
    prototype:,
    symbol_properties:,
  )) = heap.read(h, proto)
  let new_props =
    list.fold(proto_properties(ctor_ref, proto_props), properties, fn(acc, p) {
      let #(key, val) = p
      dict.insert(acc, key, val)
    })
  let h =
    heap.write(
      h,
      proto,
      ObjectSlot(
        kind:,
        properties: new_props,
        elements:,
        prototype:,
        symbol_properties:,
      ),
    )

  #(h, BuiltinType(prototype: proto, constructor: ctor_ref))
}

/// Allocate a JS array from a list of values.
pub fn alloc_array(
  h: Heap,
  values: List(JsValue),
  array_proto: Ref,
) -> #(Heap, Ref) {
  let count = list.length(values)
  heap.alloc(
    h,
    ObjectSlot(
      kind: ArrayObject(count),
      properties: dict.new(),
      elements: js_elements.from_list(values),
      prototype: Some(array_proto),
      symbol_properties: dict.new(),
    ),
  )
}
