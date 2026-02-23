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
}

/// What lives in a heap slot.
pub type HeapSlot {
  /// Unified object slot — covers ordinary objects, arrays, and functions.
  /// - `properties`: string-keyed own properties (all object kinds)
  /// - `elements`: integer-keyed elements (primarily for ArrayObject)
  /// - `prototype`: link for prototype chain traversal
  ObjectSlot(
    kind: ExoticKind,
    properties: Dict(String, JsValue),
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
        |> list.flat_map(refs_in_value)
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
        OrdinaryObject | ArrayObject(_) | NativeFunction(_) -> []
      }
      list.flatten([prop_refs, elem_refs, proto_refs, kind_refs])
    }
    EnvSlot(slots:) -> list.flat_map(slots, refs_in_value)
    BoxSlot(value:) -> refs_in_value(value)
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
