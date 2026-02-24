import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

/// A reference to a heap slot. Public so heap.gleam can construct/destructure.
pub type Ref {
  Ref(id: Int)
}

/// Unique symbol identity. Not heap-allocated — symbols are value types on BEAM.
pub type SymbolId {
  SymbolId(id: Int)
}

/// Wrapper around BEAM's native arbitrary-precision integer.
pub type BigInt {
  BigInt(value: Int)
}

/// JS number representation. BEAM floats can't represent NaN or Infinity,
/// so we use an explicit tagged type.
pub type JsNum {
  Finite(Float)
  NaN
  Infinity
  NegInfinity
}

/// Stack values — the things that live on the VM stack or inside object properties.
/// BEAM manages their lifecycle automatically, no GC involvement needed.
///
/// Everything heap-allocated is JsObject(Ref). The heap slot's `kind` tag
/// distinguishes ordinary objects, arrays, and functions. `typeof` reads the
/// heap to tell "function" from "object".
pub type JsValue {
  JsUndefined
  JsNull
  JsBool(Bool)
  JsNumber(JsNum)
  JsString(String)
  JsObject(Ref)
  JsSymbol(SymbolId)
  JsBigInt(BigInt)
  /// Internal sentinel for Temporal Dead Zone. Never exposed to JS code.
  /// GetLocal/GetEnvVar throw ReferenceError when they encounter this.
  JsUninitialized
}

/// Identifies a built-in native function. Used by `NativeFunction` to dispatch
/// to the correct Gleam implementation when called from JS.
pub type NativeFn {
  NativeObjectConstructor
  NativeFunctionConstructor
  NativeArrayConstructor
  NativeArrayIsArray
  NativeErrorConstructor(proto: Ref)
  NativeFunctionCall
  NativeFunctionApply
  NativeFunctionBind
  /// A bound function created by Function.prototype.bind.
  /// When called, prepends bound_args to the call args and uses bound_this.
  NativeBoundFunction(
    target: Ref,
    bound_this: JsValue,
    bound_args: List(JsValue),
  )
  // Object static methods
  NativeObjectGetOwnPropertyDescriptor
  NativeObjectDefineProperty
  NativeObjectGetOwnPropertyNames
  NativeObjectKeys
  // Object.prototype instance methods
  NativeObjectPrototypeHasOwnProperty
  NativeObjectPrototypePropertyIsEnumerable
  // Array.prototype instance methods
  NativeArrayPrototypeJoin
  NativeArrayPrototypePush
  // Math methods
  NativeMathPow
  NativeMathAbs
  NativeMathFloor
  NativeMathCeil
  NativeMathRound
  NativeMathTrunc
  NativeMathSqrt
  NativeMathMax
  NativeMathMin
  NativeMathLog
  NativeMathSin
  NativeMathCos
  // String.prototype methods
  NativeStringPrototypeCharAt
  NativeStringPrototypeCharCodeAt
  NativeStringPrototypeIndexOf
  NativeStringPrototypeLastIndexOf
  NativeStringPrototypeIncludes
  NativeStringPrototypeStartsWith
  NativeStringPrototypeEndsWith
  NativeStringPrototypeSlice
  NativeStringPrototypeSubstring
  NativeStringPrototypeToLowerCase
  NativeStringPrototypeToUpperCase
  NativeStringPrototypeTrim
  NativeStringPrototypeTrimStart
  NativeStringPrototypeTrimEnd
  NativeStringPrototypeSplit
  NativeStringPrototypeConcat
  NativeStringPrototypeToString
  NativeStringPrototypeValueOf
  NativeStringPrototypeRepeat
  NativeStringPrototypePadStart
  NativeStringPrototypePadEnd
  NativeStringPrototypeAt
  // String/Number/Boolean constructors (type coercion functions)
  NativeStringConstructor
  NativeNumberConstructor
  NativeBooleanConstructor
  // Global utility functions (these coerce via ToNumber first)
  NativeParseInt
  NativeParseFloat
  NativeIsNaN
  NativeIsFinite
  // Number static methods (strict — NO coercion)
  NativeNumberIsNaN
  NativeNumberIsFinite
  NativeNumberIsInteger
  NativeNumberParseInt
  NativeNumberParseFloat
  // Promise
  NativePromiseConstructor
  NativePromiseThen
  NativePromiseCatch
  NativePromiseFinally
  NativePromiseResolveStatic
  NativePromiseRejectStatic
  /// Internal resolve function created by CreateResolvingFunctions.
  /// Carries refs to the promise object, promise data slot, and shared
  /// already-resolved BoxSlot.
  NativePromiseResolveFunction(
    promise_ref: Ref,
    data_ref: Ref,
    already_resolved_ref: Ref,
  )
  /// Internal reject function created by CreateResolvingFunctions.
  NativePromiseRejectFunction(
    promise_ref: Ref,
    data_ref: Ref,
    already_resolved_ref: Ref,
  )
  /// Promise.prototype.finally wrapper: called on fulfill, calls onFinally()
  /// then returns a promise that resolves to the original value.
  NativePromiseFinallyFulfill(on_finally: JsValue)
  /// Promise.prototype.finally wrapper: called on reject, calls onFinally()
  /// then returns a promise that rejects with the original reason.
  NativePromiseFinallyReject(on_finally: JsValue)
  /// Thunk that ignores its argument and returns the captured value.
  NativePromiseFinallyValueThunk(value: JsValue)
  /// Thunk that ignores its argument and throws the captured reason.
  NativePromiseFinallyThrower(reason: JsValue)
  // Generator
  NativeGeneratorNext
  NativeGeneratorReturn
  NativeGeneratorThrow
  /// Async function resume: called when awaited promise settles.
  /// async_data_ref points to AsyncFunctionSlot on heap.
  /// is_reject: False → fulfilled (push resolved value), True → rejected (throw value)
  NativeAsyncResume(async_data_ref: Ref, is_reject: Bool)
}

/// Distinguishes the kind of object stored in a unified ObjectSlot.
pub type ExoticKind {
  /// Plain JS object: `{}`, `new Object()`, error instances, prototypes, etc.
  OrdinaryObject
  /// JS array: `[]`, `new Array()`. `length` is tracked explicitly.
  ArrayObject(length: Int)
  /// JS function (closure). `func_index` identifies the bytecode template,
  /// `env` points to the EnvSlot holding captured variables.
  FunctionObject(func_index: Int, env: Ref)
  /// Built-in function implemented in Gleam, not bytecode.
  /// Callable like any function but dispatches to native code.
  NativeFunction(native: NativeFn)
  /// Promise object. The visible JS object has this kind, pointing to
  /// an internal PromiseSlot that holds state/reactions.
  PromiseObject(promise_data: Ref)
  /// Generator object. Points to a GeneratorSlot that holds suspended state.
  GeneratorObject(generator_data: Ref)
}

/// Property descriptor — writable/enumerable/configurable flags per property.
/// Following QuickJS: bit-flags on every property. No accessor properties yet.
pub type Property {
  DataProperty(
    value: JsValue,
    writable: Bool,
    enumerable: Bool,
    configurable: Bool,
  )
}

/// Normal assignment: all flags true (obj.x = val, object literals, etc.)
pub fn data_property(val: JsValue) -> Property {
  DataProperty(value: val, writable: True, enumerable: True, configurable: True)
}

/// Built-in methods/prototype props: writable+configurable, NOT enumerable.
/// This matches QuickJS and the spec for built-in function properties.
pub fn builtin_property(val: JsValue) -> Property {
  DataProperty(
    value: val,
    writable: True,
    enumerable: False,
    configurable: True,
  )
}

/// Extract refs reachable from a Property.
pub fn refs_in_property(prop: Property) -> List(Ref) {
  case prop {
    DataProperty(value:, ..) -> refs_in_value(value)
  }
}

/// A microtask job for the promise job queue.
pub type Job {
  /// Call handler(arg), then resolve/reject the child promise.
  PromiseReactionJob(
    handler: JsValue,
    arg: JsValue,
    resolve: JsValue,
    reject: JsValue,
  )
  /// Call thenable.then(resolve, reject) to assimilate a thenable.
  PromiseResolveThenableJob(
    thenable: JsValue,
    then_fn: JsValue,
    resolve: JsValue,
    reject: JsValue,
  )
}

/// Extract refs from a Job for GC root tracking.
pub fn refs_in_job(job: Job) -> List(Ref) {
  case job {
    PromiseReactionJob(handler:, arg:, resolve:, reject:) ->
      list.flatten([
        refs_in_value(handler),
        refs_in_value(arg),
        refs_in_value(resolve),
        refs_in_value(reject),
      ])
    PromiseResolveThenableJob(thenable:, then_fn:, resolve:, reject:) ->
      list.flatten([
        refs_in_value(thenable),
        refs_in_value(then_fn),
        refs_in_value(resolve),
        refs_in_value(reject),
      ])
  }
}

/// Internal promise state (pending/fulfilled/rejected).
pub type PromiseState {
  PromisePending
  PromiseFulfilled(value: JsValue)
  PromiseRejected(reason: JsValue)
}

/// A stored reaction waiting for promise settlement.
pub type PromiseReaction {
  PromiseReaction(
    child_resolve: JsValue,
    child_reject: JsValue,
    handler: JsValue,
  )
}

/// Saved try-frame for generator suspension (mirrors TryFrame from frame.gleam).
pub type SavedTryFrame {
  SavedTryFrame(catch_target: Int, stack_depth: Int)
}

/// Saved finally-completion for generator suspension (mirrors FinallyCompletion).
pub type SavedFinallyCompletion {
  SavedNormalCompletion
  SavedThrowCompletion(value: JsValue)
  SavedReturnCompletion(value: JsValue)
}

/// Generator internal lifecycle state.
pub type GeneratorState {
  /// Created but body not yet entered (before first .next())
  SuspendedStart
  /// Paused at a yield point
  SuspendedYield
  /// Currently executing (re-entrant .next() on a running generator)
  Executing
  /// Finished (returned or threw)
  Completed
}

/// What lives in a heap slot.
pub type HeapSlot {
  /// Unified object slot — covers ordinary objects, arrays, and functions.
  /// - `properties`: string-keyed own properties with descriptor flags
  /// - `elements`: integer-keyed elements (primarily for ArrayObject, bare values)
  /// - `prototype`: link for prototype chain traversal
  ObjectSlot(
    kind: ExoticKind,
    properties: Dict(String, Property),
    elements: Dict(Int, JsValue),
    prototype: Option(Ref),
  )
  /// Flat environment frame. Multiple closures in the same scope reference
  /// the same EnvSlot, so mutations to captured variables are visible across them.
  /// Compiler flattens the scope chain — no parent pointer, all captures are direct.
  /// Mutable captures stored as JsObject(box_ref) pointing to a BoxSlot.
  EnvSlot(slots: List(JsValue))
  /// Mutable variable cell for closure captures. When a variable is both captured
  /// by a closure AND mutated, both the local frame and EnvSlot hold a Ref to
  /// the same BoxSlot. Reads/writes go through this indirection.
  BoxSlot(value: JsValue)
  /// Iterator state for for-in loops. Eagerly snapshots enumerable keys
  /// upfront (per spec: prototype shadowing requires full collection).
  /// Stores pre-collected string keys as JsString values.
  ForInIteratorSlot(keys: List(JsValue))
  /// Iterator state for for-of loops over arrays. Lazy — holds a ref to
  /// the source array and reads elements one at a time (like QuickJS).
  /// Re-reads length each iteration to handle mutations during iteration.
  ArrayIteratorSlot(source: Ref, index: Int)
  /// Engine-internal promise state, separate from the JS-visible ObjectSlot.
  /// A promise needs both a normal object (for properties, prototype chain,
  /// .then/.catch lookup) AND internal state (pending/fulfilled/rejected,
  /// reaction queues) that must NOT be visible as JS properties. The ObjectSlot
  /// has `kind: PromiseObject(promise_data: Ref)` pointing here. Same approach
  /// as QuickJS's separate JSPromiseData.
  PromiseSlot(
    state: PromiseState,
    fulfill_reactions: List(PromiseReaction),
    reject_reactions: List(PromiseReaction),
    is_handled: Bool,
  )
  /// Engine-internal generator suspended state. The ObjectSlot has
  /// `kind: GeneratorObject(generator_data: Ref)` pointing here.
  /// Saves the full execution context so .next() can resume.
  GeneratorSlot(
    gen_state: GeneratorState,
    func_template_id: Int,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: List(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(SavedTryFrame),
    saved_finally_stack: List(SavedFinallyCompletion),
    saved_this: JsValue,
  )
  /// Engine-internal async function suspended state.
  /// Saves the full execution context so await can resume.
  AsyncFunctionSlot(
    promise_data_ref: Ref,
    resolve: JsValue,
    reject: JsValue,
    func_template_id: Int,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: List(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(SavedTryFrame),
    saved_finally_stack: List(SavedFinallyCompletion),
    saved_this: JsValue,
  )
}

/// Extract refs from a single JsValue. Only JsObject carries heap refs now.
pub fn refs_in_value(value: JsValue) -> List(Ref) {
  case value {
    JsObject(ref) -> [ref]
    JsUndefined
    | JsNull
    | JsBool(_)
    | JsNumber(_)
    | JsString(_)
    | JsSymbol(_)
    | JsBigInt(_)
    | JsUninitialized -> []
  }
}

/// Extract all refs reachable from a heap slot by walking its JsValues.
pub fn refs_in_slot(slot: HeapSlot) -> List(Ref) {
  case slot {
    ObjectSlot(kind:, properties:, elements:, prototype:) -> {
      let prop_refs =
        dict.values(properties)
        |> list.flat_map(refs_in_property)
      let elem_refs =
        dict.values(elements)
        |> list.flat_map(refs_in_value)
      let proto_refs = case prototype {
        Some(ref) -> [ref]
        None -> []
      }
      let kind_refs = case kind {
        FunctionObject(env: env_ref, func_index: _) -> [env_ref]
        NativeFunction(NativeErrorConstructor(proto: ref)) -> [ref]
        NativeFunction(NativeBoundFunction(target:, bound_this:, bound_args:)) -> [
          target,
          ..list.flatten([
            refs_in_value(bound_this),
            list.flat_map(bound_args, refs_in_value),
          ])
        ]
        NativeFunction(NativePromiseResolveFunction(
          promise_ref:,
          data_ref:,
          already_resolved_ref:,
        )) -> [promise_ref, data_ref, already_resolved_ref]
        NativeFunction(NativePromiseRejectFunction(
          promise_ref:,
          data_ref:,
          already_resolved_ref:,
        )) -> [promise_ref, data_ref, already_resolved_ref]
        NativeFunction(NativePromiseFinallyFulfill(on_finally:)) ->
          refs_in_value(on_finally)
        NativeFunction(NativePromiseFinallyReject(on_finally:)) ->
          refs_in_value(on_finally)
        NativeFunction(NativePromiseFinallyValueThunk(value:)) ->
          refs_in_value(value)
        NativeFunction(NativePromiseFinallyThrower(reason:)) ->
          refs_in_value(reason)
        NativeFunction(NativeAsyncResume(async_data_ref:, ..)) -> [
          async_data_ref,
        ]
        PromiseObject(promise_data:) -> [promise_data]
        GeneratorObject(generator_data:) -> [generator_data]
        OrdinaryObject | ArrayObject(_) | NativeFunction(_) -> []
      }
      list.flatten([prop_refs, elem_refs, proto_refs, kind_refs])
    }
    EnvSlot(slots:) -> list.flat_map(slots, refs_in_value)
    BoxSlot(value:) -> refs_in_value(value)
    ForInIteratorSlot(keys:) -> list.flat_map(keys, refs_in_value)
    ArrayIteratorSlot(source:, ..) -> [source]
    PromiseSlot(state:, fulfill_reactions:, reject_reactions:, ..) -> {
      let state_refs = case state {
        PromiseFulfilled(value:) -> refs_in_value(value)
        PromiseRejected(reason:) -> refs_in_value(reason)
        PromisePending -> []
      }
      let reaction_refs = fn(reactions: List(PromiseReaction)) {
        list.flat_map(reactions, fn(r) {
          list.flatten([
            refs_in_value(r.child_resolve),
            refs_in_value(r.child_reject),
            refs_in_value(r.handler),
          ])
        })
      }
      list.flatten([
        state_refs,
        reaction_refs(fulfill_reactions),
        reaction_refs(reject_reactions),
      ])
    }
    GeneratorSlot(
      env_ref:,
      saved_locals:,
      saved_stack:,
      saved_finally_stack:,
      saved_this:,
      ..,
    ) -> {
      let finally_refs =
        list.flat_map(saved_finally_stack, fn(fc) {
          case fc {
            SavedThrowCompletion(value:) -> refs_in_value(value)
            SavedReturnCompletion(value:) -> refs_in_value(value)
            SavedNormalCompletion -> []
          }
        })
      list.flatten([
        [env_ref],
        list.flat_map(saved_locals, refs_in_value),
        list.flat_map(saved_stack, refs_in_value),
        finally_refs,
        refs_in_value(saved_this),
      ])
    }
    AsyncFunctionSlot(
      promise_data_ref:,
      resolve:,
      reject:,
      env_ref:,
      saved_locals:,
      saved_stack:,
      saved_finally_stack:,
      saved_this:,
      ..,
    ) -> {
      let finally_refs =
        list.flat_map(saved_finally_stack, fn(fc) {
          case fc {
            SavedThrowCompletion(value:) -> refs_in_value(value)
            SavedReturnCompletion(value:) -> refs_in_value(value)
            SavedNormalCompletion -> []
          }
        })
      list.flatten([
        [promise_data_ref],
        refs_in_value(resolve),
        refs_in_value(reject),
        [env_ref],
        list.flat_map(saved_locals, refs_in_value),
        list.flat_map(saved_stack, refs_in_value),
        finally_refs,
        refs_in_value(saved_this),
      ])
    }
  }
}

/// Convert a JsValue to its JS string representation (ToString abstract op).
/// Note: for objects this returns "[object Object]" — proper toString/valueOf
/// dispatch requires heap access and should be done at the call site.
pub fn to_js_string(val: JsValue) -> String {
  case val {
    JsUndefined -> "undefined"
    JsNull -> "null"
    JsBool(True) -> "true"
    JsBool(False) -> "false"
    JsNumber(Finite(n)) -> js_format_number(n)
    JsNumber(NaN) -> "NaN"
    JsNumber(Infinity) -> "Infinity"
    JsNumber(NegInfinity) -> "-Infinity"
    JsString(s) -> s
    JsBigInt(BigInt(n)) -> int.to_string(n)
    JsSymbol(_) -> "Symbol()"
    JsObject(_) -> "[object Object]"
    JsUninitialized -> "undefined"
  }
}

/// Format a JS number as a string. Integer-valued floats omit the decimal.
pub fn js_format_number(n: Float) -> String {
  let truncated = float.truncate(n)
  case int.to_float(truncated) == n {
    True -> int.to_string(truncated)
    False -> float.to_string(n)
  }
}
