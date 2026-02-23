import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import lumen/vm/builtins.{type Builtins}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type JsValue, type Ref, ArrayObject, Finite, FunctionObject, JsNumber,
  JsObject, JsString, NativeFunction, ObjectSlot, OrdinaryObject,
}

/// Walk the prototype chain to find a property by key.
/// Checks own properties first, then follows the prototype link.
/// For ArrayObject: handles numeric index lookup and "length".
/// Returns Error(Nil) if the property is not found anywhere in the chain.
pub fn get_property(heap: Heap, ref: Ref, key: String) -> Result(JsValue, Nil) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) ->
      case kind {
        ArrayObject(length:) ->
          // Array: check numeric index in elements, then "length", then properties, then prototype
          case key {
            "length" -> Ok(JsNumber(Finite(int.to_float(length))))
            _ ->
              case int.parse(key) {
                Ok(idx) ->
                  case dict.get(elements, idx) {
                    Ok(val) -> Ok(val)
                    Error(_) -> Ok(value.JsUndefined)
                  }
                Error(_) ->
                  case dict.get(properties, key) {
                    Ok(val) -> Ok(val)
                    Error(_) -> walk_prototype(heap, prototype, key)
                  }
              }
          }
        OrdinaryObject | FunctionObject(..) | NativeFunction(_) ->
          // Ordinary object / function / native: check properties, then prototype chain
          case dict.get(properties, key) {
            Ok(val) -> Ok(val)
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
/// For ArrayObject: numeric keys go to elements (updating length), string keys go to properties.
/// Returns the updated heap. No-op if ref doesn't point to an ObjectSlot.
pub fn set_property(heap: Heap, ref: Ref, key: String, val: JsValue) -> Heap {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) ->
      case kind {
        ArrayObject(length:) ->
          case int.parse(key) {
            Ok(idx) -> {
              let new_elements = dict.insert(elements, idx, val)
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
                ),
              )
            }
            Error(_) -> {
              let new_props = dict.insert(properties, key, val)
              heap.write(
                heap,
                ref,
                ObjectSlot(kind:, properties: new_props, elements:, prototype:),
              )
            }
          }
        OrdinaryObject | FunctionObject(..) | NativeFunction(_) -> {
          let new_props = dict.insert(properties, key, val)
          heap.write(
            heap,
            ref,
            ObjectSlot(kind:, properties: new_props, elements:, prototype:),
          )
        }
      }
    _ -> heap
  }
}

/// Collect enumerable own property keys from an object.
/// For arrays, includes numeric indices as strings plus any string properties.
/// NOTE: Does NOT walk the prototype chain. We skip inherited properties because
/// we lack property descriptors to distinguish enumerable from non-enumerable
/// (e.g. Object.prototype.constructor should NOT appear in for-in output).
pub fn enumerate_keys(heap: Heap, ref: Ref) -> List(String) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, ..)) -> {
      // Collect element keys (for arrays: numeric indices in order)
      let elem_keys = case kind {
        ArrayObject(length:) -> collect_element_keys(elements, 0, length, [])
        _ -> []
      }
      // Collect string property keys
      let prop_keys = dict.keys(properties)
      list.append(elem_keys, prop_keys)
    }
    _ -> []
  }
}

fn collect_element_keys(
  elements: dict.Dict(Int, JsValue),
  idx: Int,
  length: Int,
  acc: List(String),
) -> List(String) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let key = int.to_string(idx)
      case dict.has_key(elements, idx) {
        True -> collect_element_keys(elements, idx + 1, length, [key, ..acc])
        False -> collect_element_keys(elements, idx + 1, length, acc)
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
fn make_error(h: Heap, proto: Ref, message: String) -> #(Heap, JsValue) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list([#("message", JsString(message))]),
        elements: dict.new(),
        prototype: Some(proto),
      ),
    )
  #(h, JsObject(ref))
}
