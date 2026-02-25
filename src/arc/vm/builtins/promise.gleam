import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type Job, type JsValue, type Ref, BoxSlot, JsBool, JsObject, JsString,
  NativeFunction, NativePromiseCatch, NativePromiseConstructor,
  NativePromiseFinally, NativePromiseRejectFunction, NativePromiseRejectStatic,
  NativePromiseResolveFunction, NativePromiseResolveStatic, NativePromiseThen,
  ObjectSlot, PromiseObject, PromiseReaction, PromiseSlot,
}
import gleam/dict
import gleam/list
import gleam/option.{Some}

/// Set up Promise.prototype and Promise constructor with static/instance methods.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("then", NativePromiseThen, 2),
      #("catch", NativePromiseCatch, 1),
      #("finally", NativePromiseFinally, 1),
    ])
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("resolve", NativePromiseResolveStatic, 1),
      #("reject", NativePromiseRejectStatic, 1),
    ])
  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(_) { NativePromiseConstructor },
    "Promise",
    1,
    static_methods,
  )
}

/// Allocate a new promise (PromiseSlot + ObjectSlot with PromiseObject kind).
/// Returns (heap, object_ref, data_ref).
pub fn create_promise(h: Heap, promise_proto: Ref) -> #(Heap, Ref, Ref) {
  // Allocate the internal data slot first
  let #(h, data_ref) =
    heap.alloc(
      h,
      PromiseSlot(
        state: value.PromisePending,
        fulfill_reactions: [],
        reject_reactions: [],
        is_handled: False,
      ),
    )
  // Allocate the visible object pointing to the data
  let #(h, obj_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: PromiseObject(promise_data: data_ref),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(promise_proto),
        symbol_properties: dict.new(),
      ),
    )
  #(h, obj_ref, data_ref)
}

/// Create resolve and reject functions sharing an already_resolved box.
/// Returns (heap, resolve_value, reject_value).
pub fn create_resolving_functions(
  h: Heap,
  function_proto: Ref,
  promise_ref: Ref,
  data_ref: Ref,
) -> #(Heap, JsValue, JsValue) {
  // Shared already-resolved flag (BoxSlot containing JsBool(False))
  let #(h, already_resolved_ref) = heap.alloc(h, BoxSlot(value: JsBool(False)))

  // Resolve function
  let #(h, resolve_fn_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativePromiseResolveFunction(
          promise_ref:,
          data_ref:,
          already_resolved_ref:,
        )),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString(""))),
          #("length", value.builtin_property(value.JsNumber(value.Finite(1.0)))),
        ]),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
      ),
    )

  // Reject function
  let #(h, reject_fn_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(NativePromiseRejectFunction(
          promise_ref:,
          data_ref:,
          already_resolved_ref:,
        )),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString(""))),
          #("length", value.builtin_property(value.JsNumber(value.Finite(1.0)))),
        ]),
        elements: js_elements.new(),
        prototype: Some(function_proto),
        symbol_properties: dict.new(),
      ),
    )

  #(h, JsObject(resolve_fn_ref), JsObject(reject_fn_ref))
}

/// Fulfill a promise — transition from pending to fulfilled, collect reaction jobs.
pub fn fulfill_promise(
  h: Heap,
  data_ref: Ref,
  result_value: JsValue,
) -> #(Heap, List(Job)) {
  case heap.read(h, data_ref) {
    Ok(PromiseSlot(
      state: value.PromisePending,
      fulfill_reactions: reactions,
      is_handled:,
      ..,
    )) -> {
      let jobs =
        list.map(reactions, fn(r) {
          value.PromiseReactionJob(
            handler: r.handler,
            arg: result_value,
            resolve: r.child_resolve,
            reject: r.child_reject,
          )
        })
      let h =
        heap.write(
          h,
          data_ref,
          PromiseSlot(
            state: value.PromiseFulfilled(result_value),
            fulfill_reactions: [],
            reject_reactions: [],
            is_handled:,
          ),
        )
      #(h, jobs)
    }
    _ -> #(h, [])
  }
}

/// Reject a promise — transition from pending to rejected, collect reaction jobs.
pub fn reject_promise(
  h: Heap,
  data_ref: Ref,
  reason: JsValue,
) -> #(Heap, List(Job)) {
  case heap.read(h, data_ref) {
    Ok(PromiseSlot(
      state: value.PromisePending,
      reject_reactions: reactions,
      is_handled:,
      ..,
    )) -> {
      let jobs =
        list.map(reactions, fn(r) {
          value.PromiseReactionJob(
            handler: r.handler,
            arg: reason,
            resolve: r.child_resolve,
            reject: r.child_reject,
          )
        })
      let h =
        heap.write(
          h,
          data_ref,
          PromiseSlot(
            state: value.PromiseRejected(reason),
            fulfill_reactions: [],
            reject_reactions: [],
            is_handled:,
          ),
        )
      #(h, jobs)
    }
    _ -> #(h, [])
  }
}

/// Core `.then()` logic (PerformPromiseThen).
/// If pending, stores reactions. If already settled, creates immediate job.
pub fn perform_promise_then(
  h: Heap,
  data_ref: Ref,
  on_fulfilled: JsValue,
  on_rejected: JsValue,
  child_resolve: JsValue,
  child_reject: JsValue,
) -> #(Heap, List(Job)) {
  // Per spec: non-callable handlers are replaced with pass-through sentinels.
  // JsUndefined = fulfill pass-through (identity), JsNull = reject pass-through (thrower).
  let fulfill_handler = case helpers.is_callable(h, on_fulfilled) {
    True -> on_fulfilled
    False -> value.JsUndefined
  }
  let reject_handler = case helpers.is_callable(h, on_rejected) {
    True -> on_rejected
    False -> value.JsNull
  }
  case heap.read(h, data_ref) {
    Ok(PromiseSlot(
      state: value.PromisePending,
      fulfill_reactions:,
      reject_reactions:,
      is_handled: _,
    )) -> {
      let fulfill_reaction =
        PromiseReaction(child_resolve:, child_reject:, handler: fulfill_handler)
      let reject_reaction =
        PromiseReaction(child_resolve:, child_reject:, handler: reject_handler)
      let h =
        heap.write(
          h,
          data_ref,
          PromiseSlot(
            state: value.PromisePending,
            fulfill_reactions: list.append(fulfill_reactions, [
              fulfill_reaction,
            ]),
            reject_reactions: list.append(reject_reactions, [reject_reaction]),
            is_handled: True,
          ),
        )
      #(h, [])
    }
    Ok(PromiseSlot(state: value.PromiseFulfilled(val), ..)) -> {
      let h = mark_handled(h, data_ref)
      #(h, [
        value.PromiseReactionJob(
          handler: fulfill_handler,
          arg: val,
          resolve: child_resolve,
          reject: child_reject,
        ),
      ])
    }
    Ok(PromiseSlot(state: value.PromiseRejected(reason), ..)) -> {
      let h = mark_handled(h, data_ref)
      #(h, [
        value.PromiseReactionJob(
          handler: reject_handler,
          arg: reason,
          resolve: child_resolve,
          reject: child_reject,
        ),
      ])
    }
    _ -> #(h, [])
  }
}

/// Check if a value is a promise (has PromiseObject kind).
pub fn is_promise(h: Heap, val: JsValue) -> Bool {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Ok(ObjectSlot(kind: PromiseObject(_), ..)) -> True
        _ -> False
      }
    _ -> False
  }
}

/// Get the promise data ref from a promise object.
pub fn get_data_ref(h: Heap, promise_ref: Ref) -> Result(Ref, Nil) {
  case heap.read(h, promise_ref) {
    Ok(ObjectSlot(kind: PromiseObject(promise_data:), ..)) -> Ok(promise_data)
    _ -> Error(Nil)
  }
}

/// Check if a value is a thenable (an object with a `.then` method).
/// Returns Ok(then_fn_value) if thenable, Error(Nil) otherwise.
/// Walks the prototype chain (per spec: Get(resolution, "then")).
pub fn get_thenable_then(h: Heap, val: JsValue) -> Result(JsValue, Nil) {
  case val {
    JsObject(ref) ->
      case helpers.get_property_chain(h, ref, "then") {
        Ok(then_val) ->
          case helpers.is_callable(h, then_val) {
            True -> Ok(then_val)
            False -> Error(Nil)
          }
        Error(_) -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// Mark a promise as handled (for unhandled rejection tracking).
fn mark_handled(h: Heap, data_ref: Ref) -> Heap {
  case heap.read(h, data_ref) {
    Ok(PromiseSlot(state:, fulfill_reactions:, reject_reactions:, ..)) ->
      heap.write(
        h,
        data_ref,
        PromiseSlot(
          state:,
          fulfill_reactions:,
          reject_reactions:,
          is_handled: True,
        ),
      )
    _ -> h
  }
}
