/// Generator.prototype builtins — .next(), .return(), .throw()
///
/// Generators don't have a user-visible constructor (can't `new Generator()`).
/// They're created internally by calling a `function*` generator function.
/// Generator.prototype provides the iteration methods.
import arc/vm/builtins/helpers
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type Ref, NativeGeneratorNext, NativeGeneratorReturn, NativeGeneratorThrow,
  ObjectSlot,
}
import gleam/dict
import gleam/option.{Some}

/// Generator.prototype ref, used as the prototype for generator objects.
pub type GeneratorBuiltin {
  GeneratorBuiltin(prototype: Ref)
}

/// Set up Generator.prototype with .next(), .return(), .throw() methods.
/// Returns the Generator.prototype ref (no constructor needed).
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, GeneratorBuiltin) {
  // Generator.prototype — inherits from Object.prototype
  // (Technically should inherit from %IteratorPrototype% but we skip that for now)
  let #(h, gen_proto) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: value.OrdinaryObject,
        properties: dict.new(),
        elements: dict.new(),
        prototype: Some(object_proto),
      ),
    )
  let h = heap.root(h, gen_proto)

  // Generator.prototype.next
  let #(h, next_ref) =
    helpers.alloc_native_fn(h, function_proto, NativeGeneratorNext, "next", 1)
  let h = helpers.add_method(h, gen_proto, "next", next_ref)

  // Generator.prototype.return
  let #(h, return_ref) =
    helpers.alloc_native_fn(
      h,
      function_proto,
      NativeGeneratorReturn,
      "return",
      1,
    )
  let h = helpers.add_method(h, gen_proto, "return", return_ref)

  // Generator.prototype.throw
  let #(h, throw_ref) =
    helpers.alloc_native_fn(h, function_proto, NativeGeneratorThrow, "throw", 1)
  let h = helpers.add_method(h, gen_proto, "throw", throw_ref)

  #(h, GeneratorBuiltin(prototype: gen_proto))
}
