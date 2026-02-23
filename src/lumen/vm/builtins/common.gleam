import gleam/dict.{type Dict}
import gleam/option.{type Option}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type Property, type Ref, JsObject, ObjectSlot, OrdinaryObject,
}

/// A prototype + constructor pair. Every JS builtin type has both.
pub type BuiltinType {
  BuiltinType(prototype: Ref, constructor: Ref)
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
        elements: dict.new(),
        prototype:,
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Set .constructor on a prototype pointing back to its constructor.
/// "constructor" is NOT enumerable per spec (writable+configurable).
pub fn set_constructor(h: Heap, proto_ref: Ref, ctor_ref: Ref) -> Heap {
  case heap.read(h, proto_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let new_props =
        dict.insert(
          properties,
          "constructor",
          value.builtin_property(JsObject(ctor_ref)),
        )
      heap.write(
        h,
        proto_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }
}
