import gleam/dict
import gleam/option.{Some}
import lumen/vm/builtins/common.{alloc_proto}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{type Ref}

/// Set up Array.prototype.
pub fn init(h: Heap, object_proto: Ref) -> #(Heap, Ref) {
  alloc_proto(h, Some(object_proto), dict.new())
}
