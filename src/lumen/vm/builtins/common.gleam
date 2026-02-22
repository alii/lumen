import gleam/dict.{type Dict}
import gleam/option.{type Option}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{type JsValue, type Ref, ObjectSlot, OrdinaryObject}

/// Allocate an ordinary prototype object on the heap, root it, and return
/// the updated heap + ref. Shared helper for all builtin modules.
pub fn alloc_proto(
  h: Heap,
  prototype: Option(Ref),
  properties: Dict(String, JsValue),
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
