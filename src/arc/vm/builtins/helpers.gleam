/// Shared helpers for builtins that can't import higher-level modules
/// like object.gleam (due to import cycles: object -> builtins -> builtins/*).
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, DataProperty, JsObject, JsString, NativeFunction,
  ObjectSlot,
}
import gleam/dict
import gleam/int
import gleam/option.{None, Some}

/// Walk the prototype chain to find a property by key.
/// Lightweight version of object.get_property that avoids the import cycle.
/// Checks own properties, then follows prototype links.
pub fn get_property_chain(
  h: Heap,
  ref: Ref,
  key: String,
) -> Result(JsValue, Nil) {
  case heap.read(h, ref) {
    Ok(ObjectSlot(properties:, prototype:, ..)) ->
      case dict.get(properties, key) {
        Ok(DataProperty(value: val, ..)) -> Ok(val)
        Error(_) ->
          case prototype {
            Some(proto_ref) -> get_property_chain(h, proto_ref, key)
            None -> Error(Nil)
          }
      }
    _ -> Error(Nil)
  }
}

/// Check if a JsValue is callable (a function object or native function).
pub fn is_callable(h: Heap, val: JsValue) -> Bool {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Ok(ObjectSlot(kind: value.FunctionObject(..), ..)) -> True
        Ok(ObjectSlot(kind: NativeFunction(_), ..)) -> True
        _ -> False
      }
    _ -> False
  }
}

/// Allocate a NativeFunction ObjectSlot with standard name/length properties.
pub fn alloc_native_fn(
  h: Heap,
  function_proto: Ref,
  native: value.NativeFn,
  name: String,
  arity: Int,
) -> #(Heap, Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(native),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString(name))),
          #(
            "length",
            value.builtin_property(
              value.JsNumber(value.Finite(int.to_float(arity))),
            ),
          ),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Add a method property (writable, configurable, NOT enumerable) to an object.
pub fn add_method(h: Heap, obj_ref: Ref, name: String, fn_ref: Ref) -> Heap {
  case heap.read(h, obj_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let new_props =
        dict.insert(properties, name, value.builtin_property(JsObject(fn_ref)))
      heap.write(
        h,
        obj_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }
}
