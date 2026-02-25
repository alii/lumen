import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsElements, type JsValue, type Ref, ArrayObject, DataProperty,
  FunctionObject, GeneratorObject, JsBool, JsNull, JsNumber, JsObject, JsString,
  JsSymbol, JsUndefined, NativeFunction, NativeObjectConstructor,
  NativeObjectDefineProperty, NativeObjectGetOwnPropertyDescriptor,
  NativeObjectGetOwnPropertyNames, NativeObjectKeys,
  NativeObjectPrototypeHasOwnProperty, NativeObjectPrototypePropertyIsEnumerable,
  NativeObjectPrototypeToString, NativeObjectPrototypeValueOf, ObjectSlot,
  OrdinaryObject, PromiseObject,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{Some}

/// Set up Object constructor and Object.prototype methods.
/// Object.prototype is already allocated (it's the root of all chains).
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("getOwnPropertyDescriptor", NativeObjectGetOwnPropertyDescriptor, 2),
      #("defineProperty", NativeObjectDefineProperty, 3),
      #("getOwnPropertyNames", NativeObjectGetOwnPropertyNames, 1),
      #("keys", NativeObjectKeys, 1),
    ])
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("hasOwnProperty", NativeObjectPrototypeHasOwnProperty, 1),
      #("propertyIsEnumerable", NativeObjectPrototypePropertyIsEnumerable, 1),
      #("toString", NativeObjectPrototypeToString, 0),
      #("valueOf", NativeObjectPrototypeValueOf, 0),
    ])
  common.init_type_on(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(_) { NativeObjectConstructor },
    "Object",
    1,
    static_methods,
  )
}

/// Object() / new Object() — creates a plain empty object,
/// or returns the argument if it's already an object.
pub fn call_native(
  args: List(JsValue),
  _this: JsValue,
  state: State,
  object_proto: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  case args {
    [JsObject(_) as obj, ..] -> #(state, Ok(obj))
    _ -> {
      let #(heap, ref) =
        heap.alloc(
          heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            symbol_properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(object_proto),
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

/// Object.getOwnPropertyDescriptor(obj, key)
/// Returns a descriptor object {value, writable, enumerable, configurable} or undefined.
pub fn get_own_property_descriptor(
  args: List(JsValue),
  state: State,
  object_proto: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref), key_val, ..] -> {
      case frame.to_string(state, key_val) {
        Ok(#(key_str, state)) -> {
          let heap = state.heap
          case get_own_property(heap, ref, key_str) {
            Ok(prop) -> {
              let #(heap, desc_ref) =
                make_descriptor_object(heap, prop, object_proto)
              #(State(..state, heap:), Ok(JsObject(desc_ref)))
            }
            Error(_) -> #(state, Ok(JsUndefined))
          }
        }
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    }
    [JsObject(ref)] -> {
      // Missing key arg — key is undefined, convert to string "undefined"
      let heap = state.heap
      case get_own_property(heap, ref, "undefined") {
        Ok(prop) -> {
          let #(heap, desc_ref) =
            make_descriptor_object(heap, prop, object_proto)
          #(State(..state, heap:), Ok(JsObject(desc_ref)))
        }
        Error(_) -> #(state, Ok(JsUndefined))
      }
    }
    _ -> #(state, Ok(JsUndefined))
  }
}

/// Look up an OWN property (NOT prototype chain) by string key.
/// Handles arrays (numeric indices, "length") and regular objects.
fn get_own_property(
  heap: Heap,
  ref: Ref,
  key: String,
) -> Result(value.Property, Nil) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, ..)) ->
      case kind {
        ArrayObject(length:) ->
          case key {
            "length" ->
              Ok(DataProperty(
                value: JsNumber(value.Finite(int.to_float(length))),
                writable: True,
                enumerable: False,
                configurable: False,
              ))
            _ ->
              case int.parse(key) {
                Ok(idx) ->
                  case js_elements.get_option(elements, idx) {
                    option.Some(val) ->
                      Ok(DataProperty(
                        value: val,
                        writable: True,
                        enumerable: True,
                        configurable: True,
                      ))
                    option.None -> Error(Nil)
                  }
                Error(_) -> dict.get(properties, key)
              }
          }
        _ -> dict.get(properties, key)
      }
    _ -> Error(Nil)
  }
}

/// Allocate a property descriptor object: {value, writable, enumerable, configurable}
fn make_descriptor_object(
  heap: Heap,
  prop: value.Property,
  object_proto: Ref,
) -> #(Heap, Ref) {
  case prop {
    DataProperty(value: val, writable:, enumerable:, configurable:) ->
      heap.alloc(
        heap,
        ObjectSlot(
          kind: OrdinaryObject,
          properties: dict.from_list([
            #("value", value.data_property(val)),
            #("writable", value.data_property(JsBool(writable))),
            #("enumerable", value.data_property(JsBool(enumerable))),
            #("configurable", value.data_property(JsBool(configurable))),
          ]),
          symbol_properties: dict.new(),
          elements: js_elements.new(),
          prototype: Some(object_proto),
        ),
      )
  }
}

/// Object.defineProperty(obj, key, descriptor)
/// Defines/modifies a property on an object from a descriptor.
/// Returns the target object.
pub fn define_property(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsObject(ref) as obj, key_val, JsObject(desc_ref), ..] -> {
      case frame.to_string(state, key_val) {
        Ok(#(key_str, state)) -> {
          let heap = apply_descriptor(state.heap, ref, key_str, desc_ref)
          #(State(..state, heap:), Ok(obj))
        }
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    }
    [JsObject(_) as obj, _, _, ..] ->
      // descriptor is not an object — per spec should throw TypeError
      // but many engines are lenient; we'll just return obj unchanged
      #(state, Ok(obj))
    _ -> #(state, Ok(JsUndefined))
  }
}

/// Read the descriptor object's fields and apply to the target property.
fn apply_descriptor(
  heap: Heap,
  target_ref: Ref,
  key: String,
  desc_ref: Ref,
) -> Heap {
  // Read descriptor fields
  let desc_value = read_desc_field(heap, desc_ref, "value")
  let desc_writable = read_desc_bool(heap, desc_ref, "writable")
  let desc_enumerable = read_desc_bool(heap, desc_ref, "enumerable")
  let desc_configurable = read_desc_bool(heap, desc_ref, "configurable")

  case heap.read(heap, target_ref) {
    Ok(ObjectSlot(kind:, properties:, symbol_properties:, elements:, prototype:)) -> {
      // Get existing property to merge with
      let existing = dict.get(properties, key)

      let final_value = case desc_value {
        Some(v) -> v
        _ ->
          case existing {
            Ok(DataProperty(value: v, ..)) -> v
            _ -> JsUndefined
          }
      }
      let final_writable = case desc_writable {
        Some(w) -> w
        _ ->
          case existing {
            Ok(DataProperty(writable: w, ..)) -> w
            _ -> False
          }
      }
      let final_enumerable = case desc_enumerable {
        Some(e) -> e
        _ ->
          case existing {
            Ok(DataProperty(enumerable: e, ..)) -> e
            _ -> False
          }
      }
      let final_configurable = case desc_configurable {
        Some(c) -> c
        _ ->
          case existing {
            Ok(DataProperty(configurable: c, ..)) -> c
            _ -> False
          }
      }

      let new_prop =
        DataProperty(
          value: final_value,
          writable: final_writable,
          enumerable: final_enumerable,
          configurable: final_configurable,
        )
      let new_props = dict.insert(properties, key, new_prop)
      heap.write(
        heap,
        target_ref,
        ObjectSlot(
          kind:,
          properties: new_props,
          symbol_properties:,
          elements:,
          prototype:,
        ),
      )
    }
    _ -> heap
  }
}

/// Read a field from a descriptor object, returning Some(value) or None.
fn read_desc_field(
  heap: Heap,
  desc_ref: Ref,
  key: String,
) -> option.Option(JsValue) {
  case heap.read(heap, desc_ref) {
    Ok(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, key) {
        Ok(DataProperty(value: val, ..)) -> Some(val)
        _ -> option.None
      }
    _ -> option.None
  }
}

/// Read a boolean field from a descriptor object.
fn read_desc_bool(heap: Heap, desc_ref: Ref, key: String) -> option.Option(Bool) {
  case read_desc_field(heap, desc_ref, key) {
    Some(JsBool(b)) -> Some(b)
    Some(JsUndefined) -> Some(False)
    Some(JsNull) -> Some(False)
    Some(JsNumber(value.Finite(0.0))) -> Some(False)
    Some(JsNumber(value.NaN)) -> Some(False)
    Some(JsString("")) -> Some(False)
    Some(_) -> Some(True)
    option.None -> option.None
  }
}

/// Object.getOwnPropertyNames(obj) — returns array of ALL own string keys
/// (both enumerable and non-enumerable).
pub fn get_own_property_names(
  args: List(JsValue),
  state: State,
  array_proto: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  case args {
    [JsObject(ref), ..] -> {
      let keys = collect_own_keys(heap, ref, False)
      let #(heap, arr_ref) =
        common.alloc_array(heap, list.map(keys, JsString), array_proto)
      #(State(..state, heap:), Ok(JsObject(arr_ref)))
    }
    _ -> #(state, Ok(JsUndefined))
  }
}

/// Object.keys(obj) — returns array of enumerable own string keys.
pub fn keys(
  args: List(JsValue),
  state: State,
  array_proto: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  case args {
    [JsObject(ref), ..] -> {
      let ks = collect_own_keys(heap, ref, True)
      let #(heap, arr_ref) =
        common.alloc_array(heap, list.map(ks, JsString), array_proto)
      #(State(..state, heap:), Ok(JsObject(arr_ref)))
    }
    _ -> #(state, Ok(JsUndefined))
  }
}

/// Collect own property keys from an object.
/// If enumerable_only is True, only includes enumerable keys.
fn collect_own_keys(heap: Heap, ref: Ref, enumerable_only: Bool) -> List(String) {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, ..)) -> {
      // For arrays: collect numeric index keys first, then "length", then string properties
      let index_keys = case kind {
        ArrayObject(length:) -> collect_index_keys(elements, 0, length, [])
        _ -> []
      }
      // String property keys
      let prop_keys =
        dict.to_list(properties)
        |> list.filter_map(fn(pair) {
          let #(key, prop) = pair
          case enumerable_only {
            True ->
              case prop {
                DataProperty(enumerable: True, ..) -> Ok(key)
                _ -> Error(Nil)
              }
            False -> Ok(key)
          }
        })
      // For arrays: "length" is non-enumerable, include in getOwnPropertyNames but not keys
      let length_key = case kind {
        ArrayObject(_) ->
          case enumerable_only {
            True -> []
            False -> ["length"]
          }
        _ -> []
      }
      list.flatten([index_keys, length_key, prop_keys])
    }
    _ -> []
  }
}

/// Collect string representations of array indices that exist in elements.
fn collect_index_keys(
  elements: JsElements,
  idx: Int,
  length: Int,
  acc: List(String),
) -> List(String) {
  case idx >= length {
    True -> list.reverse(acc)
    False ->
      case js_elements.has(elements, idx) {
        True ->
          collect_index_keys(elements, idx + 1, length, [
            int.to_string(idx),
            ..acc
          ])
        False -> collect_index_keys(elements, idx + 1, length, acc)
      }
  }
}

/// Object.prototype.hasOwnProperty(key)
/// Checks if the object has an own property with the given key (NOT prototype chain).
pub fn has_own_property(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> {
      let key_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      case frame.to_string(state, key_val) {
        Ok(#(key_str, state)) -> {
          let heap = state.heap
          let result = case get_own_property(heap, ref, key_str) {
            Ok(_) -> JsBool(True)
            Error(_) -> JsBool(False)
          }
          #(state, Ok(result))
        }
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    }
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// Object.prototype.propertyIsEnumerable(key)
/// Returns true if the object has an own enumerable property with the given key.
pub fn property_is_enumerable(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> {
      let key_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      case frame.to_string(state, key_val) {
        Ok(#(key_str, state)) -> {
          let heap = state.heap
          let result = case get_own_property(heap, ref, key_str) {
            Ok(DataProperty(enumerable: True, ..)) -> JsBool(True)
            _ -> JsBool(False)
          }
          #(state, Ok(result))
        }
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    }
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// Object.prototype.toString() — ES2024 ss19.1.3.6
/// Returns "[object Tag]" where Tag is determined by:
/// 1. null -> "Null", undefined -> "Undefined"
/// 2. Primitives -> their type name
/// 3. Objects: check Symbol.toStringTag, then classify by kind
pub fn object_to_string(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  let tag = case this {
    JsNull -> "Null"
    JsUndefined -> "Undefined"
    JsBool(_) -> "Boolean"
    JsNumber(_) -> "Number"
    JsString(_) -> "String"
    JsSymbol(_) -> "Symbol"
    value.JsBigInt(_) -> "BigInt"
    JsObject(ref) -> object_tag(heap, ref)
    value.JsUninitialized -> "Undefined"
  }
  #(state, Ok(JsString("[object " <> tag <> "]")))
}

/// Determine the [[Class]] / toStringTag for an object ref.
fn object_tag(heap: Heap, ref: Ref) -> String {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, symbol_properties:, ..)) ->
      // First check Symbol.toStringTag
      case dict.get(symbol_properties, value.symbol_to_string_tag) {
        Ok(DataProperty(value: JsString(tag), ..)) -> tag
        _ ->
          // Classify by kind
          case kind {
            ArrayObject(_) -> "Array"
            FunctionObject(..) | NativeFunction(_) -> "Function"
            PromiseObject(_) -> "Promise"
            GeneratorObject(_) -> "Generator"
            OrdinaryObject -> "Object"
          }
      }
    _ -> "Object"
  }
}

/// Object.prototype.valueOf() — ES2024 ss19.1.3.7
/// Returns `this` unchanged.
pub fn object_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  #(state, Ok(this))
}
