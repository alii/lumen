import arc/vm/builtins/common.{type Builtins}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsElements, type JsValue, type Property, type Ref, type SymbolId,
  ArrayObject, DataProperty, Finite, FunctionObject, GeneratorObject, JsNumber,
  JsObject, JsString, NativeFunction, ObjectSlot, OrdinaryObject, PromiseObject,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import gleam/string

/// Walk the prototype chain to find a property by key.
/// Checks own properties first, then follows the prototype link.
/// For ArrayObject: handles numeric index lookup and "length".
/// Returns Error(Nil) if the property is not found anywhere in the chain.
pub fn get_property(heap: Heap, ref: Ref, key: String) -> Result(JsValue, Nil) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) ->
      case kind {
        ArrayObject(length:) ->
          // Array: check numeric index in elements, then "length", then properties, then prototype
          case key {
            "length" -> Ok(JsNumber(Finite(int.to_float(length))))
            _ ->
              case int.parse(key) {
                Ok(idx) -> Ok(js_elements.get(elements, idx))
                Error(_) ->
                  case dict.get(properties, key) {
                    Ok(DataProperty(value: val, ..)) -> Ok(val)
                    Error(_) -> walk_prototype(heap, prototype, key)
                  }
              }
          }
        OrdinaryObject
        | FunctionObject(..)
        | NativeFunction(_)
        | PromiseObject(_)
        | GeneratorObject(_) ->
          // Ordinary object / function / native / promise: check properties, then prototype chain
          case dict.get(properties, key) {
            Ok(DataProperty(value: val, ..)) -> Ok(val)
            Error(_) -> walk_prototype(heap, prototype, key)
          }
      }
    // Not an ObjectSlot or dangling ref
    _ -> Error(Nil)
  }
}

/// Walk the prototype chain for a property.
fn walk_prototype(
  heap: Heap,
  prototype: Option(Ref),
  key: String,
) -> Result(JsValue, Nil) {
  case prototype {
    Some(proto_ref) -> get_property(heap, proto_ref, key)
    None -> Error(Nil)
  }
}

/// Set an own property on an object (does NOT walk the prototype chain).
/// Respects writable flag — if existing property is non-writable, silently ignores (sloppy mode).
/// For ArrayObject: numeric keys go to elements (updating length), string keys go to properties.
/// Returns the updated heap. No-op if ref doesn't point to an ObjectSlot.
pub fn set_property(heap: Heap, ref: Ref, key: String, val: JsValue) -> Heap {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:, symbol_properties:)) ->
      case kind {
        ArrayObject(length:) ->
          case int.parse(key) {
            Ok(idx) -> {
              let new_elements = js_elements.set(elements, idx, val)
              let new_length = case idx >= length {
                True -> idx + 1
                False -> length
              }
              heap.write(
                heap,
                ref,
                ObjectSlot(
                  kind: ArrayObject(new_length),
                  properties:,
                  elements: new_elements,
                  prototype:,
                  symbol_properties:,
                ),
              )
            }
            Error(_) ->
              set_string_property(
                heap,
                ref,
                key,
                val,
                properties,
                kind,
                elements,
                prototype,
                symbol_properties,
              )
          }
        OrdinaryObject
        | FunctionObject(..)
        | NativeFunction(_)
        | PromiseObject(_)
        | GeneratorObject(_) ->
          set_string_property(
            heap,
            ref,
            key,
            val,
            properties,
            kind,
            elements,
            prototype,
            symbol_properties,
          )
      }
    _ -> heap
  }
}

/// Helper: set a string-keyed property, respecting writable flag.
fn set_string_property(
  heap: Heap,
  ref: Ref,
  key: String,
  val: JsValue,
  properties: dict.Dict(String, Property),
  kind: value.ExoticKind,
  elements: JsElements,
  prototype: Option(Ref),
  symbol_properties: dict.Dict(value.SymbolId, Property),
) -> Heap {
  let new_props = case dict.get(properties, key) {
    // Existing writable property: update value, preserve flags
    Ok(DataProperty(writable: True, enumerable:, configurable:, ..)) ->
      dict.insert(
        properties,
        key,
        DataProperty(value: val, writable: True, enumerable:, configurable:),
      )
    // Existing non-writable: silently fail (sloppy mode)
    Ok(DataProperty(writable: False, ..)) -> properties
    // New property: default all flags true
    Error(_) -> dict.insert(properties, key, value.data_property(val))
  }
  heap.write(
    heap,
    ref,
    ObjectSlot(
      kind:,
      properties: new_props,
      elements:,
      prototype:,
      symbol_properties:,
    ),
  )
}

/// Define an own property unconditionally — used for object literal fields
/// and internal setup. Always writes regardless of existing flags, with all flags true.
pub fn define_own_property(
  heap: Heap,
  ref: Ref,
  key: String,
  val: JsValue,
) -> Heap {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:, symbol_properties:)) -> {
      let new_props = dict.insert(properties, key, value.data_property(val))
      heap.write(
        heap,
        ref,
        ObjectSlot(
          kind:,
          properties: new_props,
          elements:,
          prototype:,
          symbol_properties:,
        ),
      )
    }
    _ -> heap
  }
}

/// Define a method property (writable, configurable, NOT enumerable).
/// Used for class methods and built-in methods.
pub fn define_method_property(
  heap: Heap,
  ref: Ref,
  key: String,
  val: JsValue,
) -> Heap {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:, symbol_properties:)) -> {
      let new_props = dict.insert(properties, key, value.builtin_property(val))
      heap.write(
        heap,
        ref,
        ObjectSlot(
          kind:,
          properties: new_props,
          elements:,
          prototype:,
          symbol_properties:,
        ),
      )
    }
    _ -> heap
  }
}

/// Check if a property exists anywhere in the prototype chain.
/// For `in` operator. Checks own properties, elements, then walks prototype.
pub fn has_property(heap: Heap, ref: Ref, key: String) -> Bool {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) -> {
      // Check elements first for arrays
      case kind {
        ArrayObject(length:) ->
          case key {
            "length" -> True
            _ ->
              case int.parse(key) {
                Ok(idx) ->
                  case idx >= 0 && idx < length {
                    True -> js_elements.has(elements, idx)
                    False ->
                      dict.has_key(properties, key)
                      || has_prototype_property(heap, prototype, key)
                  }
                Error(_) ->
                  dict.has_key(properties, key)
                  || has_prototype_property(heap, prototype, key)
              }
          }
        _ ->
          dict.has_key(properties, key)
          || has_prototype_property(heap, prototype, key)
      }
    }
    _ -> False
  }
}

fn has_prototype_property(
  heap: Heap,
  prototype: Option(Ref),
  key: String,
) -> Bool {
  case prototype {
    Some(proto_ref) -> has_property(heap, proto_ref, key)
    None -> False
  }
}

/// Delete an own property. Returns #(updated_heap, success).
/// Checks configurable flag — only deletes if configurable.
/// Non-existent properties return True (success).
/// For arrays, also handles element deletion.
pub fn delete_property(heap: Heap, ref: Ref, key: String) -> #(Heap, Bool) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:, symbol_properties:)) -> {
      // For arrays: check element deletion first
      case kind {
        ArrayObject(_) ->
          case int.parse(key) {
            Ok(idx) ->
              case js_elements.has(elements, idx) {
                True -> {
                  let new_elements = js_elements.delete(elements, idx)
                  let h =
                    heap.write(
                      heap,
                      ref,
                      ObjectSlot(
                        kind:,
                        properties:,
                        elements: new_elements,
                        prototype:,
                        symbol_properties:,
                      ),
                    )
                  #(h, True)
                }
                // Not present — success
                False -> #(heap, True)
              }
            Error(_) ->
              delete_string_property(
                heap,
                ref,
                key,
                properties,
                kind,
                elements,
                prototype,
                symbol_properties,
              )
          }
        _ ->
          delete_string_property(
            heap,
            ref,
            key,
            properties,
            kind,
            elements,
            prototype,
            symbol_properties,
          )
      }
    }
    _ -> #(heap, True)
  }
}

fn delete_string_property(
  heap: Heap,
  ref: Ref,
  key: String,
  properties: dict.Dict(String, Property),
  kind: value.ExoticKind,
  elements: JsElements,
  prototype: Option(Ref),
  symbol_properties: dict.Dict(SymbolId, Property),
) -> #(Heap, Bool) {
  case dict.get(properties, key) {
    Ok(DataProperty(configurable: True, ..)) -> {
      let new_props = dict.delete(properties, key)
      let h =
        heap.write(
          heap,
          ref,
          ObjectSlot(
            kind:,
            properties: new_props,
            elements:,
            prototype:,
            symbol_properties:,
          ),
        )
      #(h, True)
    }
    // Non-configurable — cannot delete
    Ok(DataProperty(configurable: False, ..)) -> #(heap, False)
    // Not found — success
    Error(_) -> #(heap, True)
  }
}

/// Collect enumerable property keys from an object, walking the prototype chain.
/// Now that we have property descriptors, we can safely walk prototypes and
/// filter by enumerable flag. Uses a seen set to skip shadowed keys.
pub fn enumerate_keys(heap: Heap, ref: Ref) -> List(String) {
  enumerate_keys_loop(heap, Some(ref), set.new(), [])
}

fn enumerate_keys_loop(
  heap: Heap,
  current: Option(Ref),
  seen: set.Set(String),
  acc: List(String),
) -> List(String) {
  case current {
    None -> list.reverse(acc)
    Some(ref) ->
      case heap.read(heap, ref) {
        Ok(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) -> {
          // Collect element keys (for arrays: numeric indices in order)
          let #(elem_acc, elem_seen) = case kind {
            ArrayObject(length:) ->
              collect_element_keys(elements, 0, length, seen, acc)
            _ -> #(acc, seen)
          }
          // Collect enumerable string property keys (skip already-seen)
          let #(final_acc, final_seen) =
            dict.fold(properties, #(elem_acc, elem_seen), fn(state, key, prop) {
              let #(a, s) = state
              case set.contains(s, key) {
                True -> #(a, s)
                False ->
                  case prop {
                    DataProperty(enumerable: True, ..) -> #(
                      [key, ..a],
                      set.insert(s, key),
                    )
                    _ -> #(a, set.insert(s, key))
                  }
              }
            })
          // Walk prototype chain
          enumerate_keys_loop(heap, prototype, final_seen, final_acc)
        }
        _ -> list.reverse(acc)
      }
  }
}

fn collect_element_keys(
  elements: JsElements,
  idx: Int,
  length: Int,
  seen: set.Set(String),
  acc: List(String),
) -> #(List(String), set.Set(String)) {
  case idx >= length {
    True -> #(acc, seen)
    False -> {
      let key = int.to_string(idx)
      case js_elements.has(elements, idx) && !set.contains(seen, key) {
        True ->
          collect_element_keys(
            elements,
            idx + 1,
            length,
            set.insert(seen, key),
            [key, ..acc],
          )
        False -> collect_element_keys(elements, idx + 1, length, seen, acc)
      }
    }
  }
}

/// Create a TypeError instance on the heap and return it as a JsObject value.
pub fn make_type_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  make_error(h, b.type_error.prototype, message)
}

/// Create a ReferenceError instance on the heap.
pub fn make_reference_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  make_error(h, b.reference_error.prototype, message)
}

/// Create a RangeError instance on the heap.
pub fn make_range_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  make_error(h, b.range_error.prototype, message)
}

/// Create a SyntaxError instance on the heap.
pub fn make_syntax_error(
  h: Heap,
  b: Builtins,
  message: String,
) -> #(Heap, JsValue) {
  make_error(h, b.syntax_error.prototype, message)
}

/// Internal helper — allocates an error object with the given prototype and message.
/// Error "message" property IS enumerable per spec.
fn make_error(h: Heap, proto: Ref, message: String) -> #(Heap, JsValue) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list([
          #("message", value.data_property(JsString(message))),
        ]),
        elements: js_elements.new(),
        prototype: Some(proto),
        symbol_properties: dict.new(),
      ),
    )
  #(h, JsObject(ref))
}

// ============================================================================
// Symbol-keyed property access
// ============================================================================

/// Get a symbol-keyed property, walking the prototype chain.
pub fn get_symbol_property(
  heap: Heap,
  ref: Ref,
  key: SymbolId,
) -> Result(JsValue, Nil) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      case dict.get(symbol_properties, key) {
        Ok(DataProperty(value: val, ..)) -> Ok(val)
        Error(_) ->
          case prototype {
            Some(proto_ref) -> get_symbol_property(heap, proto_ref, key)
            None -> Error(Nil)
          }
      }
    _ -> Error(Nil)
  }
}

/// Set a symbol-keyed own property on an object.
pub fn set_symbol_property(
  heap: Heap,
  ref: Ref,
  key: SymbolId,
  val: JsValue,
) -> Heap {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:, symbol_properties:)) -> {
      let new_sym_props = case dict.get(symbol_properties, key) {
        // Existing writable property: update value, preserve flags
        Ok(DataProperty(writable: True, enumerable:, configurable:, ..)) ->
          dict.insert(
            symbol_properties,
            key,
            DataProperty(value: val, writable: True, enumerable:, configurable:),
          )
        // Existing non-writable: silently fail (sloppy mode)
        Ok(DataProperty(writable: False, ..)) -> symbol_properties
        // New property: default all flags true
        Error(_) ->
          dict.insert(symbol_properties, key, value.data_property(val))
      }
      heap.write(
        heap,
        ref,
        ObjectSlot(
          kind:,
          properties:,
          elements:,
          prototype:,
          symbol_properties: new_sym_props,
        ),
      )
    }
    _ -> heap
  }
}

/// Define a symbol-keyed own property with specific descriptor flags.
pub fn define_symbol_property(
  heap: Heap,
  ref: Ref,
  key: SymbolId,
  prop: Property,
) -> Heap {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:, symbol_properties:)) -> {
      let new_sym_props = dict.insert(symbol_properties, key, prop)
      heap.write(
        heap,
        ref,
        ObjectSlot(
          kind:,
          properties:,
          elements:,
          prototype:,
          symbol_properties: new_sym_props,
        ),
      )
    }
    _ -> heap
  }
}

/// Check if a symbol-keyed property exists (own + prototype chain).
pub fn has_symbol_property(heap: Heap, ref: Ref, key: SymbolId) -> Bool {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      case dict.has_key(symbol_properties, key) {
        True -> True
        False ->
          case prototype {
            Some(proto_ref) -> has_symbol_property(heap, proto_ref, key)
            None -> False
          }
      }
    _ -> False
  }
}

/// Delete a symbol-keyed own property. Returns #(updated_heap, success).
pub fn delete_symbol_property(
  heap: Heap,
  ref: Ref,
  key: SymbolId,
) -> #(Heap, Bool) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:, symbol_properties:)) ->
      case dict.get(symbol_properties, key) {
        Ok(DataProperty(configurable: True, ..)) -> {
          let new_sym_props = dict.delete(symbol_properties, key)
          let h =
            heap.write(
              heap,
              ref,
              ObjectSlot(
                kind:,
                properties:,
                elements:,
                prototype:,
                symbol_properties: new_sym_props,
              ),
            )
          #(h, True)
        }
        Ok(DataProperty(configurable: False, ..)) -> #(heap, False)
        Error(_) -> #(heap, True)
      }
    _ -> #(heap, True)
  }
}

// ============================================================================
// Inspect — debugging/REPL representation (read-only, no VM re-entry)
// ============================================================================

/// Produce a human-readable representation of a JS value (for REPL / console.log).
/// Read-only — does NOT call toString/valueOf or any JS code.
pub fn inspect(val: value.JsValue, heap: Heap) -> String {
  inspect_inner(val, heap, 0, set.new())
}

fn inspect_inner(
  val: value.JsValue,
  heap: Heap,
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case val {
    value.JsUndefined -> "undefined"
    value.JsNull -> "null"
    value.JsBool(True) -> "true"
    value.JsBool(False) -> "false"
    value.JsNumber(value.Finite(n)) -> value.js_format_number(n)
    value.JsNumber(value.NaN) -> "NaN"
    value.JsNumber(value.Infinity) -> "Infinity"
    value.JsNumber(value.NegInfinity) -> "-Infinity"
    value.JsString(s) -> "'" <> s <> "'"
    value.JsSymbol(id) ->
      case value.well_known_symbol_description(id) {
        option.Some(desc) -> desc
        option.None -> "Symbol()"
      }
    value.JsBigInt(value.BigInt(n)) -> int.to_string(n) <> "n"
    value.JsUninitialized -> "<uninitialized>"
    value.JsObject(value.Ref(id:) as ref) ->
      case set.contains(visited, id) {
        True -> "[Circular]"
        False ->
          case depth > 2 {
            True -> "[Object]"
            False -> inspect_object(heap, ref, depth, set.insert(visited, id))
          }
      }
  }
}

fn inspect_object(
  heap: Heap,
  ref: value.Ref,
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, ..)) ->
      case kind {
        ArrayObject(length:) ->
          inspect_array(heap, elements, length, depth, visited)
        FunctionObject(..) -> {
          let name = case dict.get(properties, "name") {
            Ok(DataProperty(value: JsString(n), ..)) -> n
            _ -> "anonymous"
          }
          "[Function: " <> name <> "]"
        }
        NativeFunction(_) -> {
          let name = case dict.get(properties, "name") {
            Ok(DataProperty(value: JsString(n), ..)) -> n
            _ -> "native"
          }
          "[Function: " <> name <> "]"
        }
        PromiseObject(_) -> "Promise {}"
        GeneratorObject(_) -> "Object [Generator] {}"
        OrdinaryObject -> inspect_plain_object(heap, properties, depth, visited)
      }
    _ -> "[Object]"
  }
}

fn inspect_array(
  heap: Heap,
  elements: JsElements,
  length: Int,
  depth: Int,
  visited: set.Set(Int),
) -> String {
  let items = inspect_array_loop(heap, elements, 0, length, depth, visited, [])
  "[ " <> string.join(items, ", ") <> " ]"
}

fn inspect_array_loop(
  heap: Heap,
  elements: JsElements,
  idx: Int,
  length: Int,
  depth: Int,
  visited: set.Set(Int),
  acc: List(String),
) -> List(String) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let item = case js_elements.get_option(elements, idx) {
        Some(val) -> inspect_inner(val, heap, depth + 1, visited)
        None -> "<empty>"
      }
      inspect_array_loop(heap, elements, idx + 1, length, depth, visited, [
        item,
        ..acc
      ])
    }
  }
}

fn inspect_plain_object(
  heap: Heap,
  properties: dict.Dict(String, value.Property),
  depth: Int,
  visited: set.Set(Int),
) -> String {
  let entries =
    dict.to_list(properties)
    |> list.filter_map(fn(pair) {
      let #(key, prop) = pair
      case prop {
        DataProperty(enumerable: True, value: val, ..) ->
          Ok(key <> ": " <> inspect_inner(val, heap, depth + 1, visited))
        _ -> Error(Nil)
      }
    })
  case entries {
    [] -> "{}"
    _ -> "{ " <> string.join(entries, ", ") <> " }"
  }
}
