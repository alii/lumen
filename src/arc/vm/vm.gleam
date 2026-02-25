import arc/vm/array
import arc/vm/builtins/array as builtins_array
import arc/vm/builtins/boolean as builtins_boolean
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/error as builtins_error
import arc/vm/builtins/math as builtins_math
import arc/vm/builtins/number as builtins_number
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/promise as builtins_promise
import arc/vm/builtins/string as builtins_string
import arc/vm/builtins/symbol as builtins_symbol
import arc/vm/frame.{
  type FinallyCompletion, type State, type TryFrame, SavedFrame, State, TryFrame,
}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/object
import arc/vm/opcode.{
  type BinOpKind, type FuncTemplate, type Op, type UnaryOpKind, Add, ArrayFrom,
  Await, BinOp, BitAnd, BitNot, BitOr, BitXor, BoxLocal, Call, CallConstructor,
  CallMethod, CallSuper, DefineField, DefineMethod, DeleteElem, DeleteField, Div,
  Dup, EnterFinallyThrow, Eq, Exp, ForInNext, ForInStart, GetBoxed, GetElem,
  GetElem2, GetField, GetField2, GetGlobal, GetIterator, GetLocal, GetThis, Gt,
  GtEq, InitialYield, IteratorClose, IteratorNext, Jump, JumpIfFalse,
  JumpIfNullish, JumpIfTrue, LogicalNot, Lt, LtEq, MakeClosure, MarkGlobalConst,
  Mod, Mul, Neg, NewObject, NotEq, Pop, Pos, PushConst, PushTry, PutBoxed,
  PutElem, PutField, PutGlobal, PutLocal, Return, SetupDerivedClass, ShiftLeft,
  ShiftRight, StrictEq, StrictNotEq, Sub, Swap, TypeOf, TypeofGlobal,
  UShiftRight, UnaryOp, UnmarkGlobalConst, Void, Yield,
}
import arc/vm/value.{
  type JsNum, type JsValue, type Ref, ArrayIteratorSlot, ArrayObject,
  AsyncFunctionSlot, BigInt, DataProperty, Finite, ForInIteratorSlot,
  FunctionObject, GeneratorObject, GeneratorSlot, Infinity, JsBigInt, JsBool,
  JsNull, JsNumber, JsObject, JsString, JsSymbol, JsUndefined, JsUninitialized,
  NaN, NativeFunction, NegInfinity, ObjectSlot, OrdinaryObject, PromiseObject,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/result
import gleam/set
import gleam/string

// ============================================================================
// Public types
// ============================================================================

/// JS-level completion — either normal return or uncaught exception.
pub type Completion {
  NormalCompletion(value: JsValue, heap: Heap)
  ThrowCompletion(value: JsValue, heap: Heap)
  /// Generator yielded a value — execution is suspended, not completed.
  /// The full State is available in the second element of the returned tuple.
  YieldCompletion(value: JsValue, heap: Heap)
}

/// Internal VM error — these are bugs in the VM, not JS-level errors.
pub type VmError {
  /// Tried to read past end of bytecode
  PcOutOfBounds(pc: Int)
  /// Stack underflow
  StackUnderflow(op: String)
  /// Local variable index out of bounds
  LocalIndexOutOfBounds(index: Int)
  /// Unimplemented opcode
  Unimplemented(op: String)
}

// ============================================================================
// Internal state (types defined in frame.gleam for cross-module access)
// ============================================================================

/// The js_to_string callback that gets stored in State.
/// Delegates to the VM's internal js_to_string implementation.
fn js_to_string_callback(
  state: State,
  val: JsValue,
) -> Result(#(String, State), #(JsValue, State)) {
  js_to_string(state, val)
}

/// Signals from step() — either continue with new state, or stop.
type StepResult {
  Done
  VmError(VmError)
  Thrown
  /// Generator suspension — yielded a value (or initial suspend).
  Yielded
}

// ============================================================================
// Public API
// ============================================================================

/// Run a function template and return its completion + updated heap.
pub fn run(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
) -> Result(#(Completion, State), VmError) {
  run_with_globals(func, heap, builtins, dict.new())
}

fn init_state(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  globals: dict.Dict(String, JsValue),
) {
  let locals = array.repeat(JsUndefined, func.local_count)
  State(
    stack: [],
    locals:,
    constants: func.constants,
    globals:,
    func:,
    code: func.bytecode,
    heap:,
    pc: 0,
    call_stack: [],
    try_stack: [],
    finally_stack: [],
    builtins:,
    closure_templates: dict.new(),
    this_binding: JsUndefined,
    callee_ref: None,
    job_queue: [],
    const_globals: set.new(),
    next_symbol_id: 100,
    symbol_descriptions: dict.new(),
    js_to_string: js_to_string_callback,
  )
}

/// Run a function template with pre-populated global variables.
pub fn run_with_globals(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  globals: dict.Dict(String, JsValue),
) -> Result(#(Completion, State), VmError) {
  init_state(func, heap, builtins, globals) |> execute_inner()
}

/// Run a function template with globals, then drain the promise job queue.
/// Use this when you need promise reactions to execute (e.g., .then callbacks).
pub fn run_and_drain(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  globals: dict.Dict(String, JsValue),
) -> Result(Completion, VmError) {
  let result = init_state(func, heap, builtins, globals) |> execute_inner()
  use #(completion, final_state) <- result.try(result)
  let drained_state = drain_jobs(final_state)
  case completion {
    NormalCompletion(val, _) -> Ok(NormalCompletion(val, drained_state.heap))
    ThrowCompletion(val, _) -> Ok(ThrowCompletion(val, drained_state.heap))
    YieldCompletion(_, _) ->
      panic as "YieldCompletion should not appear at script level"
  }
}

/// Persistent REPL environment carried between evaluations.
pub type ReplEnv {
  ReplEnv(
    globals: dict.Dict(String, JsValue),
    closure_templates: dict.Dict(Int, FuncTemplate),
    const_globals: set.Set(String),
    next_symbol_id: Int,
    symbol_descriptions: dict.Dict(Int, String),
  )
}

/// Like run_and_drain, but persists globals and closure_templates across calls.
/// Used by the REPL so var declarations and function definitions survive.
pub fn run_and_drain_repl(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  env: ReplEnv,
) -> Result(#(Completion, ReplEnv), VmError) {
  let locals = array.repeat(JsUndefined, func.local_count)
  let state =
    State(
      stack: [],
      locals:,
      constants: func.constants,
      globals: env.globals,
      func:,
      code: func.bytecode,
      heap:,
      pc: 0,
      call_stack: [],
      try_stack: [],
      finally_stack: [],
      builtins:,
      closure_templates: env.closure_templates,
      this_binding: JsUndefined,
      callee_ref: None,
      job_queue: [],
      const_globals: env.const_globals,
      next_symbol_id: env.next_symbol_id,
      symbol_descriptions: env.symbol_descriptions,
      js_to_string: js_to_string_callback,
    )
  use #(completion, final_state) <- result.try(execute_inner(state))
  let drained_state = drain_jobs(final_state)
  let new_env =
    ReplEnv(
      globals: drained_state.globals,
      closure_templates: drained_state.closure_templates,
      const_globals: drained_state.const_globals,
      next_symbol_id: drained_state.next_symbol_id,
      symbol_descriptions: drained_state.symbol_descriptions,
    )
  case completion {
    NormalCompletion(val, _) ->
      Ok(#(NormalCompletion(val, drained_state.heap), new_env))
    ThrowCompletion(val, _) ->
      Ok(#(ThrowCompletion(val, drained_state.heap), new_env))
    YieldCompletion(_, _) ->
      panic as "YieldCompletion should not appear at script level"
  }
}

/// Get the fulfilled value of a promise JsValue, or Error if not fulfilled.
pub fn promise_result(h: Heap, val: JsValue) -> Result(JsValue, Nil) {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Ok(ObjectSlot(kind: value.PromiseObject(promise_data:), ..)) ->
          case heap.read(h, promise_data) {
            Ok(value.PromiseSlot(state: value.PromiseFulfilled(v), ..)) -> Ok(v)
            Ok(value.PromiseSlot(state: value.PromiseRejected(r), ..)) -> Ok(r)
            _ -> Error(Nil)
          }
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

// ============================================================================
// Execution loop
// ============================================================================

/// Main execution loop. Tail-recursive.
/// Returns the completion and the final state (for job queue access).
fn execute_inner(state: State) -> Result(#(Completion, State), VmError) {
  case array.get(state.pc, state.code) {
    None -> {
      // Reached end of bytecode — return top of stack or undefined
      case state.stack {
        [top, ..] -> Ok(#(NormalCompletion(top, state.heap), state))
        [] -> Ok(#(NormalCompletion(JsUndefined, state.heap), state))
      }
    }
    Some(op) -> {
      case step(state, op) {
        Ok(new_state) -> execute_inner(new_state)
        Error(#(Done, result, heap)) ->
          Ok(#(NormalCompletion(result, heap), State(..state, heap:)))
        Error(#(VmError(err), _, _)) -> Error(err)
        Error(#(Yielded, yielded_value, heap)) -> {
          // Generator yielded or async awaited — build suspended state.
          // For Yield/Await: pop the yielded value from stack, advance pc.
          // For InitialYield: stack unchanged, just advance pc.
          let suspended_state = case op {
            Yield | Await ->
              State(
                ..state,
                heap:,
                stack: case state.stack {
                  [_, ..rest] -> rest
                  [] -> []
                },
                pc: state.pc + 1,
              )
            _ -> State(..state, heap:, pc: state.pc + 1)
          }
          Ok(#(YieldCompletion(yielded_value, heap), suspended_state))
        }
        Error(#(Thrown, thrown_value, heap)) -> {
          // Try to unwind to a catch handler
          let updated_state = State(..state, heap:)
          case unwind_to_catch(updated_state, thrown_value) {
            Ok(caught_state) -> execute_inner(caught_state)
            Error(_) ->
              Ok(#(ThrowCompletion(thrown_value, heap), State(..state, heap:)))
          }
        }
      }
    }
  }
}

/// Wrapper that discards the state for backward compatibility.
/// Try to find a catch handler on the try_stack.
/// If found: restore stack to saved depth, push thrown value, jump to catch_target.
/// If not found: return Error(Nil) → uncaught exception.
fn unwind_to_catch(state: State, thrown_value: JsValue) -> Result(State, Nil) {
  case state.try_stack {
    [] -> Error(Nil)
    [TryFrame(catch_target:, stack_depth:), ..rest_try] -> {
      let restored_stack = truncate_stack(state.stack, stack_depth)
      Ok(
        State(
          ..state,
          stack: [thrown_value, ..restored_stack],
          try_stack: rest_try,
          pc: catch_target,
        ),
      )
    }
  }
}

/// Truncate stack to a given depth.
fn truncate_stack(stack: List(JsValue), depth: Int) -> List(JsValue) {
  case list.length(stack) > depth {
    True -> truncate_stack(list.drop(stack, 1), depth)
    False -> stack
  }
}

/// Pop top of stack and jump to `target` if `condition(value)` is true,
/// otherwise advance to next instruction.
fn conditional_jump(
  state: State,
  target: Int,
  condition: fn(JsValue) -> Bool,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case state.stack {
    [top, ..rest] ->
      case condition(top) {
        True -> Ok(State(..state, stack: rest, pc: target))
        False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
      }
    [] ->
      Error(#(
        VmError(StackUnderflow("ConditionalJump")),
        JsUndefined,
        state.heap,
      ))
  }
}

// ============================================================================
// Step — single instruction dispatch
// ============================================================================

/// Execute a single instruction. Returns Ok(new_state) to continue,
/// or Error(#(signal, value, heap)) to stop.
fn step(state: State, op: Op) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    PushConst(index) -> {
      case array.get(index, state.constants) {
        Some(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        None -> {
          let #(heap, err) =
            object.make_range_error(
              state.heap,
              state.builtins,
              "constant index out of bounds: " <> int.to_string(index),
            )
          Error(#(Thrown, err, heap))
        }
      }
    }

    Pop -> {
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> Error(#(VmError(StackUnderflow("Pop")), JsUndefined, state.heap))
      }
    }

    Dup -> {
      case state.stack {
        [top, ..] ->
          Ok(State(..state, stack: [top, ..state.stack], pc: state.pc + 1))
        [] -> Error(#(VmError(StackUnderflow("Dup")), JsUndefined, state.heap))
      }
    }

    Swap -> {
      case state.stack {
        [a, b, ..rest] ->
          Ok(State(..state, stack: [b, a, ..rest], pc: state.pc + 1))
        _ -> Error(#(VmError(StackUnderflow("Swap")), JsUndefined, state.heap))
      }
    }

    GetLocal(index) -> {
      case array.get(index, state.locals) {
        Some(JsUninitialized) -> {
          let #(heap, err) =
            object.make_reference_error(
              state.heap,
              state.builtins,
              "Cannot access variable before initialization (TDZ)",
            )
          Error(#(Thrown, err, heap))
        }
        Some(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        None ->
          Error(#(
            VmError(LocalIndexOutOfBounds(index)),
            JsUndefined,
            state.heap,
          ))
      }
    }

    PutLocal(index) -> {
      case state.stack {
        [value, ..rest] -> {
          case array.set(index, value, state.locals) {
            Ok(new_locals) ->
              Ok(
                State(
                  ..state,
                  stack: rest,
                  locals: new_locals,
                  pc: state.pc + 1,
                ),
              )
            Error(_) ->
              Error(#(
                VmError(LocalIndexOutOfBounds(index)),
                JsUndefined,
                state.heap,
              ))
          }
        }
        [] ->
          Error(#(VmError(StackUnderflow("PutLocal")), JsUndefined, state.heap))
      }
    }

    GetGlobal(name) -> {
      case dict.get(state.globals, name) {
        Ok(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        Error(_) ->
          Ok(
            State(
              ..state,
              stack: [JsUndefined, ..state.stack],
              pc: state.pc + 1,
            ),
          )
      }
    }

    PutGlobal(name) -> {
      case state.stack {
        [value, ..rest] ->
          case set.contains(state.const_globals, name) {
            True -> {
              let #(heap, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  "Assignment to constant variable.",
                )
              Error(#(Thrown, err, heap))
            }
            False ->
              Ok(
                State(
                  ..state,
                  stack: rest,
                  globals: dict.insert(state.globals, name, value),
                  pc: state.pc + 1,
                ),
              )
          }
        [] ->
          Error(#(VmError(StackUnderflow("PutGlobal")), JsUndefined, state.heap))
      }
    }

    MarkGlobalConst(name) ->
      Ok(
        State(
          ..state,
          const_globals: set.insert(state.const_globals, name),
          pc: state.pc + 1,
        ),
      )

    UnmarkGlobalConst(name) ->
      Ok(
        State(
          ..state,
          const_globals: set.delete(state.const_globals, name),
          pc: state.pc + 1,
        ),
      )

    TypeOf -> {
      case state.stack {
        [val, ..rest] -> {
          let type_str = typeof_value(val, state.heap)
          Ok(
            State(
              ..state,
              stack: [JsString(type_str), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        [] ->
          Error(#(VmError(StackUnderflow("TypeOf")), JsUndefined, state.heap))
      }
    }

    TypeofGlobal(name) -> {
      let val = case dict.get(state.globals, name) {
        Ok(v) -> v
        Error(_) -> JsUndefined
      }
      let type_str = typeof_value(val, state.heap)
      Ok(
        State(
          ..state,
          stack: [JsString(type_str), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    BinOp(kind) -> {
      case state.stack {
        [right, left, ..rest] -> {
          // instanceof and in need heap access
          case kind {
            opcode.InstanceOf -> {
              case js_instanceof(state.heap, left, right) {
                Ok(result) ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(result), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                Error(msg) -> {
                  let #(heap, err) =
                    object.make_type_error(state.heap, state.builtins, msg)
                  Error(#(Thrown, err, heap))
                }
              }
            }
            opcode.In -> {
              // left = key, right = object
              case right {
                JsObject(ref) ->
                  case js_to_string(state, left) {
                    Ok(#(key_str, state)) -> {
                      let result = object.has_property(state.heap, ref, key_str)
                      Ok(
                        State(
                          ..state,
                          stack: [JsBool(result), ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    }
                    Error(#(thrown, state)) ->
                      Error(#(Thrown, thrown, state.heap))
                  }
                _ -> {
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      "Cannot use 'in' operator to search for '"
                        <> object.inspect(left, state.heap)
                        <> "' in "
                        <> object.inspect(right, state.heap),
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            }
            // Add needs ToPrimitive for object operands (ES2024 §13.15.3)
            Add ->
              case left, right {
                // Fast path: both primitives — no ToPrimitive needed
                JsObject(_), _ | _, JsObject(_) ->
                  binop_add_with_to_primitive(state, left, right, rest)
                // Both primitives — fast path, no ToPrimitive needed
                JsString(a), JsString(b) ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsString(a <> b), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                JsString(a), _ ->
                  case js_to_string(state, right) {
                    Ok(#(b, state)) ->
                      Ok(
                        State(
                          ..state,
                          stack: [JsString(a <> b), ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    Error(#(thrown, state)) ->
                      Error(#(Thrown, thrown, state.heap))
                  }
                _, JsString(b) ->
                  case js_to_string(state, left) {
                    Ok(#(a, state)) ->
                      Ok(
                        State(
                          ..state,
                          stack: [JsString(a <> b), ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    Error(#(thrown, state)) ->
                      Error(#(Thrown, thrown, state.heap))
                  }
                _, _ ->
                  case num_binop(left, right, num_add) {
                    Ok(result) ->
                      Ok(
                        State(
                          ..state,
                          stack: [result, ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    Error(msg) -> {
                      let #(heap, err) =
                        object.make_type_error(state.heap, state.builtins, msg)
                      Error(#(Thrown, err, heap))
                    }
                  }
              }
            _ ->
              case exec_binop(kind, left, right) {
                Ok(result) ->
                  Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
                Error(msg) -> {
                  let #(heap, err) =
                    object.make_type_error(state.heap, state.builtins, msg)
                  Error(#(Thrown, err, heap))
                }
              }
          }
        }
        _ -> Error(#(VmError(StackUnderflow("BinOp")), JsUndefined, state.heap))
      }
    }

    UnaryOp(kind) -> {
      case state.stack {
        [operand, ..rest] -> {
          case exec_unaryop(kind, operand) {
            Ok(result) ->
              Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
            Error(msg) -> {
              let #(heap, err) =
                object.make_type_error(state.heap, state.builtins, msg)
              Error(#(Thrown, err, heap))
            }
          }
        }
        [] ->
          Error(#(VmError(StackUnderflow("UnaryOp")), JsUndefined, state.heap))
      }
    }

    Return -> {
      let return_value = case state.stack {
        [value, ..] -> value
        [] -> JsUndefined
      }
      case state.call_stack {
        // No caller — top-level return, we're done
        [] -> Error(#(Done, return_value, state.heap))
        // Pop call frame, restore caller, push return value onto caller's stack
        [
          SavedFrame(
            func:,
            locals:,
            stack:,
            pc:,
            try_stack:,
            this_binding: saved_this,
            constructor_this:,
            callee_ref: saved_callee_ref,
          ),
          ..rest_frames
        ] -> {
          // Constructor return semantics
          case constructor_this {
            Some(constructed_obj) -> {
              // Base constructor: use the constructed object unless the
              // function explicitly returned an object.
              let effective_return = case return_value {
                JsObject(_) -> return_value
                _ -> constructed_obj
              }
              Ok(
                State(
                  ..state,
                  stack: [effective_return, ..stack],
                  locals:,
                  func:,
                  code: func.bytecode,
                  constants: func.constants,
                  pc:,
                  call_stack: rest_frames,
                  try_stack:,
                  this_binding: saved_this,
                  callee_ref: saved_callee_ref,
                ),
              )
            }
            None ->
              case state.func.is_derived_constructor {
                True -> {
                  // Derived constructor return semantics:
                  // - If return value is an object, use it
                  // - If return value is undefined, use this_binding (set by super())
                  // - If this_binding is still uninitialized, throw ReferenceError
                  // - If return value is non-object non-undefined, throw TypeError
                  case return_value {
                    JsObject(_) ->
                      Ok(
                        State(
                          ..state,
                          stack: [return_value, ..stack],
                          locals:,
                          func:,
                          code: func.bytecode,
                          constants: func.constants,
                          pc:,
                          call_stack: rest_frames,
                          try_stack:,
                          this_binding: saved_this,
                          callee_ref: saved_callee_ref,
                        ),
                      )
                    JsUndefined ->
                      case state.this_binding {
                        JsUninitialized -> {
                          let #(heap, err) =
                            object.make_reference_error(
                              state.heap,
                              state.builtins,
                              "Must call super constructor in derived class before returning from derived constructor",
                            )
                          Error(#(Thrown, err, heap))
                        }
                        this_val ->
                          Ok(
                            State(
                              ..state,
                              stack: [this_val, ..stack],
                              locals:,
                              func:,
                              code: func.bytecode,
                              constants: func.constants,
                              pc:,
                              call_stack: rest_frames,
                              try_stack:,
                              this_binding: saved_this,
                              callee_ref: saved_callee_ref,
                            ),
                          )
                      }
                    _ -> {
                      let #(heap, err) =
                        object.make_type_error(
                          state.heap,
                          state.builtins,
                          "Derived constructors may only return object or undefined",
                        )
                      Error(#(Thrown, err, heap))
                    }
                  }
                }
                False ->
                  // Regular function return
                  Ok(
                    State(
                      ..state,
                      stack: [return_value, ..stack],
                      locals:,
                      func:,
                      code: func.bytecode,
                      constants: func.constants,
                      pc:,
                      call_stack: rest_frames,
                      try_stack:,
                      this_binding: saved_this,
                      callee_ref: saved_callee_ref,
                    ),
                  )
              }
          }
        }
      }
    }

    MakeClosure(func_index) -> {
      case array.get(func_index, state.func.functions) {
        Some(child_template) -> {
          // Capture values from current frame according to env_descriptors.
          // For boxed captured vars, the local holds a JsObject(box_ref) —
          // copying that ref means the closure shares the same BoxSlot.
          let captured_values =
            list.map(child_template.env_descriptors, fn(desc) {
              case desc {
                opcode.CaptureLocal(parent_index) ->
                  case array.get(parent_index, state.locals) {
                    Some(val) -> val
                    None -> JsUndefined
                  }
                opcode.CaptureEnv(_parent_env_index) ->
                  // Transitive capture not yet implemented
                  JsUndefined
              }
            })
          let #(heap, env_ref) =
            heap.alloc(state.heap, value.EnvSlot(captured_values))
          // Set .name from the template
          let fn_name = case child_template.name {
            Some(n) -> JsString(n)
            None -> JsString("")
          }
          // For non-arrow functions, pre-populate .prototype with a fresh object
          // so that `Foo.prototype.bar = ...` and `new Foo()` work.
          // .constructor on prototype is set after we have the closure ref.
          let #(heap, fn_properties, proto_ref) = case child_template.is_arrow {
            True -> #(
              heap,
              dict.from_list([
                #("name", value.data(fn_name) |> value.configurable()),
              ]),
              None,
            )
            False -> {
              let #(h, proto_obj_ref) =
                heap.alloc(
                  heap,
                  ObjectSlot(
                    kind: OrdinaryObject,
                    properties: dict.new(),
                    elements: js_elements.new(),
                    prototype: Some(state.builtins.object.prototype),
                    symbol_properties: dict.new(),
                  ),
                )
              #(
                h,
                dict.from_list([
                  #(
                    "prototype",
                    value.data(JsObject(proto_obj_ref)) |> value.writable(),
                  ),
                  #("name", value.data(fn_name) |> value.configurable()),
                ]),
                Some(proto_obj_ref),
              )
            }
          }
          let #(heap, closure_ref) =
            heap.alloc(
              heap,
              ObjectSlot(
                kind: FunctionObject(func_index:, env: env_ref),
                properties: fn_properties,
                elements: js_elements.new(),
                prototype: Some(state.builtins.function.prototype),
                symbol_properties: dict.new(),
              ),
            )
          // Set .constructor on the prototype pointing back to this function
          let heap = case proto_ref {
            Some(pr) ->
              case heap.read(heap, pr) {
                Ok(ObjectSlot(
                  kind:,
                  properties: props,
                  elements:,
                  prototype:,
                  symbol_properties:,
                )) ->
                  heap.write(
                    heap,
                    pr,
                    ObjectSlot(
                      kind:,
                      properties: dict.insert(
                        props,
                        "constructor",
                        value.builtin_property(JsObject(closure_ref)),
                      ),
                      elements:,
                      prototype:,
                      symbol_properties:,
                    ),
                  )
                _ -> heap
              }
            None -> heap
          }
          // Cache the template so it can be found when the closure is called
          // from a different scope (after the defining function has returned)
          let closure_templates =
            dict.insert(state.closure_templates, closure_ref.id, child_template)
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(closure_ref), ..state.stack],
              pc: state.pc + 1,
              closure_templates:,
            ),
          )
        }
        None -> {
          let #(heap, err) =
            object.make_range_error(
              state.heap,
              state.builtins,
              "invalid function index: " <> int.to_string(func_index),
            )
          Error(#(Thrown, err, heap))
        }
      }
    }

    BoxLocal(index) -> {
      // Wrap the current value in locals[index] into a BoxSlot on the heap.
      // Replace the local with a JsObject(box_ref).
      case array.get(index, state.locals) {
        Some(current_value) -> {
          let #(heap, box_ref) =
            heap.alloc(state.heap, value.BoxSlot(current_value))
          case array.set(index, JsObject(box_ref), state.locals) {
            Ok(locals) -> Ok(State(..state, heap:, locals:, pc: state.pc + 1))
            Error(_) ->
              Error(#(VmError(LocalIndexOutOfBounds(index)), JsUndefined, heap))
          }
        }
        None ->
          Error(#(
            VmError(LocalIndexOutOfBounds(index)),
            JsUndefined,
            state.heap,
          ))
      }
    }

    GetBoxed(index) -> {
      // Read locals[index] (a JsObject(box_ref)), dereference BoxSlot, push value.
      case array.get(index, state.locals) {
        Some(JsObject(box_ref)) -> {
          case heap.read(state.heap, box_ref) {
            Ok(value.BoxSlot(val)) ->
              Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
            _ ->
              Error(#(
                VmError(Unimplemented("GetBoxed: not a BoxSlot")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        _ ->
          Error(#(
            VmError(Unimplemented("GetBoxed: local is not a box ref")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    PutBoxed(index) -> {
      // Pop value from stack, write into the BoxSlot pointed to by locals[index].
      case state.stack {
        [new_value, ..rest_stack] -> {
          case array.get(index, state.locals) {
            Some(JsObject(box_ref)) -> {
              let heap =
                heap.write(state.heap, box_ref, value.BoxSlot(new_value))
              Ok(State(..state, heap:, stack: rest_stack, pc: state.pc + 1))
            }
            _ ->
              Error(#(
                VmError(Unimplemented("PutBoxed: local is not a box ref")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        [] ->
          Error(#(VmError(StackUnderflow("PutBoxed")), JsUndefined, state.heap))
      }
    }

    Call(arity) -> {
      // Stack layout: [arg_n, ..., arg_1, callee, ...rest]
      // Pop arity args, then callee
      case pop_n(state.stack, arity) {
        Ok(#(args, after_args)) -> {
          case after_args {
            [JsObject(obj_ref), ..rest_stack] -> {
              case heap.read(state.heap, obj_ref) {
                Ok(ObjectSlot(
                  kind: FunctionObject(func_index: _, env: env_ref),
                  ..,
                )) ->
                  call_function(
                    state,
                    obj_ref,
                    env_ref,
                    args,
                    rest_stack,
                    JsUndefined,
                    None,
                    None,
                  )
                Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
                  call_native(state, native, args, rest_stack, JsUndefined)
                _ -> {
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      object.inspect(JsObject(obj_ref), state.heap)
                        <> " is not a function",
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            }
            [non_func, ..] -> {
              let #(heap, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  object.inspect(non_func, state.heap) <> " is not a function",
                )
              Error(#(Thrown, err, heap))
            }
            [] ->
              Error(#(
                VmError(StackUnderflow("Call: no callee")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        Error(_) ->
          Error(#(
            VmError(StackUnderflow("Call: not enough args")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    Jump(target) -> Ok(State(..state, pc: target))

    JumpIfFalse(target) -> {
      use v <- conditional_jump(state, target)
      !value.is_truthy(v)
    }

    JumpIfTrue(target) -> conditional_jump(state, target, value.is_truthy)

    JumpIfNullish(target) -> {
      use v <- conditional_jump(state, target)
      case v {
        JsNull | JsUndefined -> True
        _ -> False
      }
    }

    // -- Exception handling --
    PushTry(catch_target) -> {
      let frame = TryFrame(catch_target:, stack_depth: list.length(state.stack))
      Ok(
        State(..state, try_stack: [frame, ..state.try_stack], pc: state.pc + 1),
      )
    }

    opcode.PopTry -> {
      case state.try_stack {
        [_, ..rest] -> Ok(State(..state, try_stack: rest, pc: state.pc + 1))
        [] ->
          Error(#(
            VmError(StackUnderflow("PopTry: empty try_stack")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    opcode.Throw -> {
      case state.stack {
        [value, ..] -> Error(#(Thrown, value, state.heap))
        [] ->
          Error(#(VmError(StackUnderflow("Throw")), JsUndefined, state.heap))
      }
    }

    opcode.EnterFinally -> {
      Ok(
        State(
          ..state,
          finally_stack: [frame.NormalCompletion, ..state.finally_stack],
          pc: state.pc + 1,
        ),
      )
    }

    opcode.EnterFinallyThrow -> {
      // Pop thrown value from stack, push ThrowCompletion to finally_stack
      case state.stack {
        [thrown_value, ..rest_stack] ->
          Ok(
            State(
              ..state,
              stack: rest_stack,
              finally_stack: [
                frame.ThrowCompletion(thrown_value),
                ..state.finally_stack
              ],
              pc: state.pc + 1,
            ),
          )
        [] ->
          Error(#(
            VmError(StackUnderflow("EnterFinallyThrow")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    opcode.LeaveFinally -> {
      case state.finally_stack {
        [frame.NormalCompletion, ..rest] ->
          Ok(State(..state, finally_stack: rest, pc: state.pc + 1))
        [frame.ThrowCompletion(value:), ..] ->
          Error(#(Thrown, value, state.heap))
        [frame.ReturnCompletion(value:), ..] ->
          Error(#(Done, value, state.heap))
        [] ->
          Error(#(
            VmError(StackUnderflow("LeaveFinally: empty finally_stack")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Property access --
    NewObject -> {
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(state.builtins.object.prototype),
            symbol_properties: dict.new(),
          ),
        )
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(ref), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    GetField(name) -> {
      case state.stack {
        [JsNull, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of null (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsUndefined, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of undefined (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsObject(ref), ..rest] -> {
          let val = case object.get_property(state.heap, ref, name) {
            Ok(v) -> v
            Error(_) -> JsUndefined
          }
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        }
        [JsString(s), ..rest] -> {
          let val =
            get_string_field(
              s,
              name,
              state.heap,
              state.builtins.string.prototype,
            )
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        }
        [_, ..rest] -> {
          // Number/Bool/other primitives: no prototype yet
          Ok(State(..state, stack: [JsUndefined, ..rest], pc: state.pc + 1))
        }
        [] ->
          Error(#(VmError(StackUnderflow("GetField")), JsUndefined, state.heap))
      }
    }

    PutField(name) -> {
      // Consumes [value, obj] and pushes value back (assignment is an expression).
      // Consistent with PutElem which also leaves the value on the stack.
      case state.stack {
        [value, JsObject(ref), ..rest] -> {
          let heap = object.set_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: [value, ..rest], pc: state.pc + 1))
        }
        [value, _, ..rest] -> {
          // PutField on non-object: silently ignore, still return value
          Ok(State(..state, stack: [value, ..rest], pc: state.pc + 1))
        }
        _ ->
          Error(#(VmError(StackUnderflow("PutField")), JsUndefined, state.heap))
      }
    }

    DefineField(name) -> {
      // Like PutField but keeps the object on the stack (for object literal construction)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let heap = object.set_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> {
          // DefineField on non-object: no-op, keep object on stack
          Ok(State(..state, pc: state.pc + 1))
        }
        _ ->
          Error(#(
            VmError(StackUnderflow("DefineField")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DefineMethod(name) -> {
      // Like DefineField but creates a non-enumerable property (for class methods)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let heap = object.define_method_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ ->
          Error(#(
            VmError(StackUnderflow("DefineMethod")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Array construction --
    ArrayFrom(count) -> {
      case pop_n(state.stack, count) {
        Ok(#(elements, rest)) -> {
          // elements are in order [first, ..., last]
          let #(heap, ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: ArrayObject(count),
                properties: dict.new(),
                elements: js_elements.from_list(elements),
                prototype: Some(state.builtins.array.prototype),
                symbol_properties: dict.new(),
              ),
            )
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        Error(_) ->
          Error(#(VmError(StackUnderflow("ArrayFrom")), JsUndefined, state.heap))
      }
    }

    // -- Computed property access --
    GetElem -> {
      case state.stack {
        [key, JsObject(ref), ..rest] ->
          case get_elem_value(state, ref, key) {
            Ok(#(val, state)) ->
              Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            Error(#(thrown, state)) -> Error(#(Thrown, thrown, state.heap))
          }
        [_, JsNull, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of null",
            )
          Error(#(Thrown, err, heap))
        }
        [_, JsUndefined, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of undefined",
            )
          Error(#(Thrown, err, heap))
        }
        [key, JsString(s), ..rest] -> {
          let val =
            get_string_elem(s, key, state.heap, state.builtins.string.prototype)
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        }
        [_, _, ..rest] -> {
          // Non-object: return undefined
          Ok(State(..state, stack: [JsUndefined, ..rest], pc: state.pc + 1))
        }
        _ ->
          Error(#(VmError(StackUnderflow("GetElem")), JsUndefined, state.heap))
      }
    }

    GetElem2 -> {
      // Like GetElem but keeps obj+key on stack: [key, obj, ...] -> [value, key, obj, ...]
      case state.stack {
        [key, JsObject(ref) as obj, ..rest] ->
          case get_elem_value(state, ref, key) {
            Ok(#(val, state)) ->
              Ok(
                State(..state, stack: [val, key, obj, ..rest], pc: state.pc + 1),
              )
            Error(#(thrown, state)) -> Error(#(Thrown, thrown, state.heap))
          }
        [key, JsString(s) as str, ..rest] -> {
          let val =
            get_string_elem(s, key, state.heap, state.builtins.string.prototype)
          Ok(State(..state, stack: [val, key, str, ..rest], pc: state.pc + 1))
        }
        _ ->
          Error(#(VmError(StackUnderflow("GetElem2")), JsUndefined, state.heap))
      }
    }

    PutElem -> {
      // Stack: [value, key, obj, ...rest]
      case state.stack {
        [val, key, JsObject(ref), ..rest] ->
          case put_elem_value(state, ref, key, val) {
            Ok(state) ->
              Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            Error(#(thrown, state)) -> Error(#(Thrown, thrown, state.heap))
          }
        [_, _, _, ..rest] -> {
          // PutElem on non-object: silently ignore (JS sloppy mode)
          Ok(State(..state, stack: rest, pc: state.pc + 1))
        }
        _ ->
          Error(#(VmError(StackUnderflow("PutElem")), JsUndefined, state.heap))
      }
    }

    // -- this / method calls / constructors --
    GetThis ->
      case state.this_binding {
        // TDZ check: in derived constructors, this is uninitialized until super() is called
        JsUninitialized -> {
          let #(heap, err) =
            object.make_reference_error(
              state.heap,
              state.builtins,
              "Must call super constructor in derived class before accessing 'this'",
            )
          Error(#(Thrown, err, heap))
        }
        _ ->
          Ok(
            State(
              ..state,
              stack: [state.this_binding, ..state.stack],
              pc: state.pc + 1,
            ),
          )
      }

    GetField2(name) -> {
      // Like GetField but keeps the object on the stack for CallMethod.
      // Stack: [obj, ..rest] → [prop_value, obj, ..rest]
      case state.stack {
        [JsNull, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of null (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsUndefined, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of undefined (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsObject(ref) as obj, ..rest] -> {
          let val = case object.get_property(state.heap, ref, name) {
            Ok(v) -> v
            Error(_) -> JsUndefined
          }
          Ok(State(..state, stack: [val, obj, ..rest], pc: state.pc + 1))
        }
        [JsString(s) as str, ..rest] -> {
          let val =
            get_string_field(
              s,
              name,
              state.heap,
              state.builtins.string.prototype,
            )
          Ok(State(..state, stack: [val, str, ..rest], pc: state.pc + 1))
        }
        [other, ..rest] -> {
          // Non-object: property is undefined, keep object on stack
          Ok(
            State(
              ..state,
              stack: [JsUndefined, other, ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        [] ->
          Error(#(VmError(StackUnderflow("GetField2")), JsUndefined, state.heap))
      }
    }

    CallMethod(_name, arity) -> {
      // Stack: [arg_n, ..., arg_1, method, receiver, ...rest]
      // Pop arity args, then method, then receiver
      case pop_n(state.stack, arity) {
        Ok(#(args, after_args)) -> {
          case after_args {
            [JsObject(method_ref), receiver, ..rest_stack] -> {
              case heap.read(state.heap, method_ref) {
                Ok(ObjectSlot(
                  kind: FunctionObject(func_index: _, env: env_ref),
                  ..,
                )) ->
                  call_function(
                    state,
                    method_ref,
                    env_ref,
                    args,
                    rest_stack,
                    // Method call: this = receiver
                    receiver,
                    None,
                    None,
                  )
                Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
                  call_native(state, native, args, rest_stack, receiver)
                _ -> {
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      object.inspect(JsObject(method_ref), state.heap)
                        <> " is not a function",
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            }
            [non_func, _, ..] -> {
              let #(heap, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  object.inspect(non_func, state.heap) <> " is not a function",
                )
              Error(#(Thrown, err, heap))
            }
            _ ->
              Error(#(
                VmError(StackUnderflow("CallMethod")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        Error(_) ->
          Error(#(
            VmError(StackUnderflow("CallMethod: not enough args")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    CallConstructor(arity) -> {
      // Stack: [arg_n, ..., arg_1, constructor, ...rest]
      case pop_n(state.stack, arity) {
        Ok(#(args, after_args)) -> {
          case after_args {
            [JsObject(ctor_ref), ..rest_stack] -> {
              case heap.read(state.heap, ctor_ref) {
                Ok(ObjectSlot(
                  kind: FunctionObject(func_index: _, env: env_ref),
                  properties:,
                  ..,
                )) -> {
                  // Check if this is a derived constructor
                  let is_derived = case
                    dict.get(state.closure_templates, ctor_ref.id)
                  {
                    Ok(tmpl) -> tmpl.is_derived_constructor
                    Error(_) -> False
                  }
                  case is_derived {
                    True ->
                      // Derived constructor: don't allocate object yet.
                      // this = JsUninitialized (TDZ until super() is called).
                      // constructor_this = None signals derived constructor mode.
                      call_function(
                        state,
                        ctor_ref,
                        env_ref,
                        args,
                        rest_stack,
                        JsUninitialized,
                        None,
                        Some(ctor_ref),
                      )
                    False -> {
                      // Base constructor: allocate the new object
                      let proto = case dict.get(properties, "prototype") {
                        Ok(DataProperty(value: JsObject(proto_ref), ..)) ->
                          Some(proto_ref)
                        _ -> Some(state.builtins.object.prototype)
                      }
                      let #(heap, new_obj_ref) =
                        heap.alloc(
                          state.heap,
                          ObjectSlot(
                            kind: OrdinaryObject,
                            properties: dict.new(),
                            elements: js_elements.new(),
                            prototype: proto,
                            symbol_properties: dict.new(),
                          ),
                        )
                      let new_obj = JsObject(new_obj_ref)
                      call_function(
                        State(..state, heap:),
                        ctor_ref,
                        env_ref,
                        args,
                        rest_stack,
                        new_obj,
                        Some(new_obj),
                        None,
                      )
                    }
                  }
                }
                // Bound function used as constructor: resolve target, prepend args
                Ok(ObjectSlot(
                  kind: NativeFunction(value.NativeBoundFunction(
                    target:,
                    bound_args:,
                    ..,
                  )),
                  ..,
                )) -> {
                  let final_args = list.append(bound_args, args)
                  construct_value(state, target, final_args, rest_stack)
                }
                Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
                  call_native(state, native, args, rest_stack, JsUndefined)
                _ -> {
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      object.inspect(JsObject(ctor_ref), state.heap)
                        <> " is not a constructor",
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            }
            [non_func, ..] -> {
              let #(heap, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  object.inspect(non_func, state.heap)
                    <> " is not a constructor",
                )
              Error(#(Thrown, err, heap))
            }
            _ ->
              Error(#(
                VmError(StackUnderflow("CallConstructor")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        Error(_) ->
          Error(#(
            VmError(StackUnderflow("CallConstructor: not enough args")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Iteration --
    ForInStart -> {
      case state.stack {
        [obj, ..rest] -> {
          // Collect enumerable keys from the object (or empty for non-objects)
          let keys = case obj {
            JsObject(ref) -> object.enumerate_keys(state.heap, ref)
            // for-in on null/undefined produces no iterations
            JsNull | JsUndefined -> []
            // Primitives: no enumerable properties
            _ -> []
          }
          // Wrap string keys as JsString values for ForInIteratorSlot
          let key_values = list.map(keys, JsString)
          let #(heap, iter_ref) =
            heap.alloc(state.heap, ForInIteratorSlot(keys: key_values))
          Ok(
            State(
              ..state,
              stack: [JsObject(iter_ref), ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
        _ ->
          Error(#(
            VmError(StackUnderflow("ForInStart")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    ForInNext -> {
      case state.stack {
        [JsObject(iter_ref), ..rest] ->
          case heap.read(state.heap, iter_ref) {
            Ok(ForInIteratorSlot(keys:)) ->
              case keys {
                [val, ..remaining] -> {
                  // Advance the iterator
                  let heap =
                    heap.write(
                      state.heap,
                      iter_ref,
                      ForInIteratorSlot(keys: remaining),
                    )
                  // Push: iterator stays, key, done=false
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
                [] -> {
                  // No more keys — push undefined + done=true
                  Ok(
                    State(
                      ..state,
                      stack: [
                        JsBool(True),
                        JsUndefined,
                        JsObject(iter_ref),
                        ..rest
                      ],
                      pc: state.pc + 1,
                    ),
                  )
                }
              }
            _ ->
              Error(#(
                VmError(Unimplemented("ForInNext: not a ForInIteratorSlot")),
                JsUndefined,
                state.heap,
              ))
          }
        _ ->
          Error(#(VmError(StackUnderflow("ForInNext")), JsUndefined, state.heap))
      }
    }

    GetIterator -> {
      case state.stack {
        [iterable, ..rest] ->
          case iterable {
            JsObject(ref) ->
              case heap.read(state.heap, ref) {
                Ok(ObjectSlot(kind: ArrayObject(_), ..)) -> {
                  // Lazy array iterator — stores source ref + index, reads elements one at a time
                  let #(heap, iter_ref) =
                    heap.alloc(
                      state.heap,
                      ArrayIteratorSlot(source: ref, index: 0),
                    )
                  Ok(
                    State(
                      ..state,
                      stack: [JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
                Ok(ObjectSlot(kind: GeneratorObject(_), ..)) -> {
                  // Generators are their own iterators
                  Ok(
                    State(
                      ..state,
                      stack: [JsObject(ref), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                }
                _ -> {
                  // Non-array object — throw TypeError
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      object.inspect(JsObject(ref), state.heap)
                        <> " is not iterable",
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            _ -> {
              let #(heap, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  object.inspect(iterable, state.heap) <> " is not iterable",
                )
              Error(#(Thrown, err, heap))
            }
          }
        _ ->
          Error(#(
            VmError(StackUnderflow("GetIterator")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    IteratorNext -> {
      case state.stack {
        [JsObject(iter_ref), ..rest] ->
          case heap.read(state.heap, iter_ref) {
            Ok(ArrayIteratorSlot(source:, index:)) -> {
              // Re-read the array length each time (handles mutations during iteration)
              let #(length, elements) = case heap.read(state.heap, source) {
                Ok(ObjectSlot(kind: ArrayObject(len), elements: elems, ..)) -> #(
                  len,
                  elems,
                )
                _ -> #(0, js_elements.new())
              }
              case index >= length {
                True ->
                  // Done — push undefined + done=true
                  Ok(
                    State(
                      ..state,
                      stack: [
                        JsBool(True),
                        JsUndefined,
                        JsObject(iter_ref),
                        ..rest
                      ],
                      pc: state.pc + 1,
                    ),
                  )
                False -> {
                  // Read element at current index
                  let val = js_elements.get(elements, index)
                  // Advance iterator index
                  let heap =
                    heap.write(
                      state.heap,
                      iter_ref,
                      ArrayIteratorSlot(source:, index: index + 1),
                    )
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
              }
            }
            Ok(ObjectSlot(kind: GeneratorObject(_), ..)) -> {
              // Generator iterator: call .next() and extract {value, done}
              // Use a temporary stack with just the iterator, since
              // call_native_generator_next will push result onto rest_stack.
              {
                use next_state <- result.try(
                  call_native_generator_next(state, JsObject(iter_ref), [], []),
                )
                // next_state.stack has [result_obj, ...], extract value and done
                case next_state.stack {
                  [JsObject(result_ref), ..] ->
                    case heap.read(next_state.heap, result_ref) {
                      Ok(ObjectSlot(properties: props, ..)) -> {
                        let val = case dict.get(props, "value") {
                          Ok(DataProperty(value: v, ..)) -> v
                          _ -> JsUndefined
                        }
                        let done = case dict.get(props, "done") {
                          Ok(DataProperty(value: JsBool(d), ..)) -> d
                          _ -> False
                        }
                        Ok(
                          State(
                            ..state,
                            heap: next_state.heap,
                            stack: [
                              JsBool(done),
                              val,
                              JsObject(iter_ref),
                              ..rest
                            ],
                            pc: state.pc + 1,
                            closure_templates: next_state.closure_templates,
                            globals: next_state.globals,
                            job_queue: next_state.job_queue,
                          ),
                        )
                      }
                      _ ->
                        Error(#(
                          VmError(Unimplemented(
                            "IteratorNext: generator .next() returned non-object",
                          )),
                          JsUndefined,
                          next_state.heap,
                        ))
                    }
                  _ ->
                    Error(#(
                      VmError(Unimplemented(
                        "IteratorNext: generator .next() empty stack",
                      )),
                      JsUndefined,
                      next_state.heap,
                    ))
                }
              }
            }
            _ ->
              Error(#(
                VmError(Unimplemented("IteratorNext: not an iterator")),
                JsUndefined,
                state.heap,
              ))
          }
        _ ->
          Error(#(
            VmError(StackUnderflow("IteratorNext")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    IteratorClose -> {
      // MVP: just pop the iterator from the stack
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ ->
          Error(#(
            VmError(StackUnderflow("IteratorClose")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Delete operator --
    DeleteField(name) -> {
      case state.stack {
        [obj, ..rest] ->
          case obj {
            JsObject(ref) -> {
              let #(heap, success) =
                object.delete_property(state.heap, ref, name)
              Ok(
                State(
                  ..state,
                  stack: [JsBool(success), ..rest],
                  heap:,
                  pc: state.pc + 1,
                ),
              )
            }
            // delete on non-object returns true
            _ ->
              Ok(
                State(..state, stack: [JsBool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ ->
          Error(#(
            VmError(StackUnderflow("DeleteField")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DeleteElem -> {
      case state.stack {
        [key, obj, ..rest] ->
          case obj {
            JsObject(ref) ->
              case js_to_string(state, key) {
                Ok(#(key_str, state)) -> {
                  let #(heap, success) =
                    object.delete_property(state.heap, ref, key_str)
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(success), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
                Error(#(thrown, state)) -> Error(#(Thrown, thrown, state.heap))
              }
            _ ->
              Ok(
                State(..state, stack: [JsBool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ ->
          Error(#(
            VmError(StackUnderflow("DeleteElem")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Class Inheritance --
    SetupDerivedClass -> {
      // Stack: [ctor, parent, ..rest] → [ctor, ..rest]
      // Wire ctor.prototype.__proto__ = parent.prototype
      // Wire ctor.__proto__ = parent (for static inheritance)
      case state.stack {
        [JsObject(ctor_ref), JsObject(parent_ref), ..rest] -> {
          case heap.read(state.heap, parent_ref) {
            Ok(ObjectSlot(kind: FunctionObject(..), ..)) -> {
              let parent_proto =
                get_field_ref(state.heap, parent_ref, "prototype")
                |> result.unwrap(state.builtins.object.prototype)
              // Set ctor.prototype.__proto__ = parent.prototype
              let heap =
                get_field_ref(state.heap, ctor_ref, "prototype")
                |> result.map(set_slot_prototype(
                  state.heap,
                  _,
                  Some(parent_proto),
                ))
                |> result.unwrap(state.heap)
              // Set ctor.__proto__ = parent (for static inheritance)
              let heap = set_slot_prototype(heap, ctor_ref, Some(parent_ref))
              Ok(
                State(
                  ..state,
                  heap:,
                  stack: [JsObject(ctor_ref), ..rest],
                  pc: state.pc + 1,
                ),
              )
            }
            _ ->
              Ok(
                State(
                  ..state,
                  stack: [JsObject(ctor_ref), ..rest],
                  pc: state.pc + 1,
                ),
              )
          }
        }
        [JsObject(ctor_ref), JsNull, ..rest] -> {
          // extends null — ctor.prototype.__proto__ = null
          let heap =
            get_field_ref(state.heap, ctor_ref, "prototype")
            |> result.map(set_slot_prototype(state.heap, _, None))
            |> result.unwrap(state.heap)
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ctor_ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        _ -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Class extends value is not a constructor or null",
            )
          Error(#(Thrown, err, heap))
        }
      }
    }

    CallSuper(arity) -> {
      // Stack: [arg_n, ..., arg_1, ..rest] → [new_obj, ..rest]
      // Find parent constructor via callee_ref.__proto__
      case state.callee_ref {
        Some(my_ctor_ref) -> {
          case pop_n(state.stack, arity) {
            Ok(#(args, rest_stack)) -> {
              // Find parent constructor: callee_ref.__proto__
              case heap.read(state.heap, my_ctor_ref) {
                Ok(ObjectSlot(
                  prototype: Some(parent_ref),
                  properties: my_props,
                  ..,
                )) -> {
                  // For multi-level inheritance: only allocate a new object
                  // when this_binding == JsUninitialized (first super() in chain).
                  // Intermediate derived constructors reuse the existing this.
                  let #(heap, this_val) = case state.this_binding {
                    JsUninitialized -> {
                      // First super() in chain — allocate new object
                      let derived_proto = case dict.get(my_props, "prototype") {
                        Ok(DataProperty(value: JsObject(dp_ref), ..)) ->
                          Some(dp_ref)
                        _ -> Some(state.builtins.object.prototype)
                      }
                      let #(h, new_obj_ref) =
                        heap.alloc(
                          state.heap,
                          ObjectSlot(
                            kind: OrdinaryObject,
                            properties: dict.new(),
                            elements: js_elements.new(),
                            prototype: derived_proto,
                            symbol_properties: dict.new(),
                          ),
                        )
                      #(h, JsObject(new_obj_ref))
                    }
                    existing_this -> {
                      // Intermediate super() — reuse existing this
                      #(state.heap, existing_this)
                    }
                  }
                  // Call parent constructor, passing parent_ref as callee_ref
                  // so further super() calls in the chain can find their parent
                  case heap.read(heap, parent_ref) {
                    Ok(ObjectSlot(
                      kind: FunctionObject(func_index: _, env: env_ref),
                      ..,
                    )) ->
                      call_function(
                        State(..state, heap:, this_binding: this_val),
                        parent_ref,
                        env_ref,
                        args,
                        rest_stack,
                        this_val,
                        Some(this_val),
                        Some(parent_ref),
                      )
                    Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
                      call_native(
                        State(..state, heap:, this_binding: this_val),
                        native,
                        args,
                        rest_stack,
                        this_val,
                      )
                    _ -> {
                      let #(heap2, err) =
                        object.make_type_error(
                          heap,
                          state.builtins,
                          "Super constructor is not a constructor",
                        )
                      Error(#(Thrown, err, heap2))
                    }
                  }
                }
                _ -> {
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      "Super constructor is not a constructor",
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            }
            Error(_) ->
              Error(#(
                VmError(StackUnderflow("CallSuper: not enough args")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        None -> {
          let #(heap, err) =
            object.make_reference_error(
              state.heap,
              state.builtins,
              "'super' keyword unexpected here",
            )
          Error(#(Thrown, err, heap))
        }
      }
    }

    // -- Generator / Async suspension --
    InitialYield ->
      // Suspend immediately at start of generator body.
      // PC advances past InitialYield so resumption starts at the next op.
      Error(#(Yielded, JsUndefined, state.heap))

    Yield -> {
      // Pop value from stack and suspend the generator.
      // On resume, .next(arg) value will be pushed onto the stack.
      // Note: the actual stack pop happens in execute_inner's Yielded handler,
      // not here — we just extract the value to return.
      case state.stack {
        [yielded_value, ..] -> Error(#(Yielded, yielded_value, state.heap))
        [] -> Error(#(Yielded, JsUndefined, state.heap))
      }
    }

    Await -> {
      // Pop the awaited value from the stack and suspend the async function.
      // The caller wraps it in Promise.resolve() and attaches .then() callbacks
      // to resume execution when settled.
      // Note: the actual stack pop happens in execute_inner's Yielded handler.
      case state.stack {
        [awaited_value, ..] -> Error(#(Yielded, awaited_value, state.heap))
        [] -> Error(#(Yielded, JsUndefined, state.heap))
      }
    }

    _ ->
      Error(#(
        VmError(Unimplemented("opcode: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

// ============================================================================
// Call helpers
// ============================================================================

/// Shared logic for Call, CallMethod, and CallConstructor.
/// Looks up the callee template, saves the caller frame, sets up locals,
/// and transitions to the callee's code.
fn call_function(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
  constructor_this: option.Option(JsValue),
  new_callee_ref: option.Option(Ref),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case dict.get(state.closure_templates, fn_ref.id) {
    Ok(callee_template) ->
      case callee_template.is_generator, callee_template.is_async {
        // Note: async generators (True, True) not yet implemented — they'll
        // fall through to call_generator_function which is incorrect but harmless
        // until async generators are properly supported.
        True, _ ->
          call_generator_function(
            state,
            fn_ref,
            env_ref,
            callee_template,
            args,
            rest_stack,
            this_val,
          )
        _, True ->
          call_async_function(
            state,
            fn_ref,
            env_ref,
            callee_template,
            args,
            rest_stack,
            this_val,
          )
        _, _ ->
          call_regular_function(
            state,
            fn_ref,
            env_ref,
            callee_template,
            args,
            rest_stack,
            this_val,
            constructor_this,
            new_callee_ref,
          )
      }
    Error(_) -> {
      let #(heap, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "closure template not found",
        )
      Error(#(Thrown, err, heap))
    }
  }
}

/// Regular (non-generator) function call: save frame, enter callee.
fn call_regular_function(
  state: State,
  _fn_ref: value.Ref,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
  constructor_this: option.Option(JsValue),
  new_callee_ref: option.Option(Ref),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // Save caller frame
  let saved =
    SavedFrame(
      func: state.func,
      locals: state.locals,
      stack: rest_stack,
      pc: state.pc + 1,
      try_stack: state.try_stack,
      this_binding: state.this_binding,
      constructor_this:,
      callee_ref: state.callee_ref,
    )
  let locals = setup_locals(state.heap, env_ref, callee_template, args)
  // Arrow functions inherit this from their enclosing scope
  let new_this = case callee_template.is_arrow {
    True -> state.this_binding
    False -> this_val
  }
  Ok(
    State(
      ..state,
      stack: [],
      locals:,
      func: callee_template,
      code: callee_template.bytecode,
      constants: callee_template.constants,
      pc: 0,
      call_stack: [saved, ..state.call_stack],
      try_stack: [],
      this_binding: new_this,
      callee_ref: new_callee_ref,
    ),
  )
}

/// Generator function call: execute until InitialYield, save state to
/// GeneratorSlot, create GeneratorObject, return it to caller.
fn call_generator_function(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let locals = setup_locals(state.heap, env_ref, callee_template, args)
  // Set up an isolated execution state for the generator body
  let gen_state =
    State(
      ..state,
      stack: [],
      locals:,
      func: callee_template,
      code: callee_template.bytecode,
      constants: callee_template.constants,
      pc: 0,
      call_stack: [],
      try_stack: [],
      finally_stack: [],
      this_binding: this_val,
      callee_ref: Some(fn_ref),
    )
  // Execute until InitialYield (which fires immediately at the start)
  case execute_inner(gen_state) {
    Ok(#(YieldCompletion(_, _), suspended)) -> {
      // Save the suspended state into a GeneratorSlot on the heap
      let #(saved_try, saved_finally) =
        save_stacks(suspended.try_stack, suspended.finally_stack)
      let #(h, data_ref) =
        heap.alloc(
          suspended.heap,
          GeneratorSlot(
            gen_state: value.SuspendedStart,
            func_template_id: fn_ref.id,
            env_ref:,
            saved_pc: suspended.pc,
            saved_locals: suspended.locals,
            saved_stack: suspended.stack,
            saved_try_stack: saved_try,
            saved_finally_stack: saved_finally,
            saved_this: suspended.this_binding,
          ),
        )
      // Create the generator object with Generator.prototype
      let #(h, gen_obj_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: GeneratorObject(generator_data: data_ref),
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(state.builtins.generator.prototype),
            symbol_properties: dict.new(),
          ),
        )
      // Return to caller with the generator object on the stack
      Ok(
        State(
          ..state,
          heap: h,
          stack: [JsObject(gen_obj_ref), ..rest_stack],
          pc: state.pc + 1,
          closure_templates: suspended.closure_templates,
          globals: suspended.globals,
          job_queue: suspended.job_queue,
        ),
      )
    }
    Ok(#(NormalCompletion(_, h), _)) -> {
      // Generator returned without yielding — shouldn't happen with InitialYield
      // but handle gracefully: create a completed generator
      let #(h, data_ref) =
        heap.alloc(
          h,
          GeneratorSlot(
            gen_state: value.Completed,
            func_template_id: fn_ref.id,
            env_ref:,
            saved_pc: 0,
            saved_locals: array.from_list([]),
            saved_stack: [],
            saved_try_stack: [],
            saved_finally_stack: [],
            saved_this: JsUndefined,
          ),
        )
      let #(h, gen_obj_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: GeneratorObject(generator_data: data_ref),
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(state.builtins.generator.prototype),
            symbol_properties: dict.new(),
          ),
        )
      Ok(
        State(
          ..state,
          heap: h,
          stack: [JsObject(gen_obj_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
    Ok(#(ThrowCompletion(thrown, h), _)) -> Error(#(Thrown, thrown, h))
    Error(vm_err) -> Error(#(VmError(vm_err), JsUndefined, state.heap))
  }
}

/// Async function call: create a promise, execute body eagerly.
/// If the body completes synchronously, resolve/reject the promise immediately.
/// If the body hits `await`, save state and set up promise callbacks to resume.
fn call_async_function(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // Create the outer promise that the async function returns
  let #(h, promise_ref, data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let #(h, resolve_fn, reject_fn) =
    builtins_promise.create_resolving_functions(
      h,
      state.builtins.function.prototype,
      promise_ref,
      data_ref,
    )
  // Set up locals and execute body eagerly
  let locals = setup_locals(h, env_ref, callee_template, args)
  let async_state =
    State(
      ..state,
      heap: h,
      stack: [],
      locals:,
      func: callee_template,
      code: callee_template.bytecode,
      constants: callee_template.constants,
      pc: 0,
      call_stack: [],
      try_stack: [],
      finally_stack: [],
      this_binding: this_val,
      callee_ref: Some(fn_ref),
    )
  case execute_inner(async_state) {
    Ok(#(YieldCompletion(awaited_value, h2), suspended)) -> {
      // Body hit `await` — save state, set up promise resolution
      let #(saved_try, saved_finally) =
        save_stacks(suspended.try_stack, suspended.finally_stack)
      let #(h2, async_data_ref) =
        heap.alloc(
          h2,
          AsyncFunctionSlot(
            promise_data_ref: data_ref,
            resolve: resolve_fn,
            reject: reject_fn,
            func_template_id: fn_ref.id,
            env_ref:,
            saved_pc: suspended.pc,
            saved_locals: suspended.locals,
            saved_stack: suspended.stack,
            saved_try_stack: saved_try,
            saved_finally_stack: saved_finally,
            saved_this: suspended.this_binding,
          ),
        )
      let #(h2, jobs) =
        async_setup_await(h2, state.builtins, async_data_ref, awaited_value)
      Ok(
        State(
          ..state,
          heap: h2,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
          closure_templates: suspended.closure_templates,
          globals: suspended.globals,
          job_queue: list.append(suspended.job_queue, jobs),
        ),
      )
    }
    Ok(#(NormalCompletion(return_value, h2), final_state)) -> {
      // Async function completed without awaiting — resolve the promise
      let #(h2, jobs) =
        builtins_promise.fulfill_promise(h2, data_ref, return_value)
      Ok(
        State(
          ..state,
          heap: h2,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
          closure_templates: final_state.closure_templates,
          globals: final_state.globals,
          job_queue: list.append(final_state.job_queue, jobs),
        ),
      )
    }
    Ok(#(ThrowCompletion(thrown, h2), final_state)) -> {
      // Async function threw without awaiting — reject the promise
      let #(h2, jobs) = builtins_promise.reject_promise(h2, data_ref, thrown)
      Ok(
        State(
          ..state,
          heap: h2,
          stack: [JsObject(promise_ref), ..rest_stack],
          pc: state.pc + 1,
          closure_templates: final_state.closure_templates,
          globals: final_state.globals,
          job_queue: list.append(final_state.job_queue, jobs),
        ),
      )
    }
    Error(vm_err) -> Error(#(VmError(vm_err), JsUndefined, state.heap))
  }
}

/// Set up promise resolution for an awaited value in an async function.
/// Wraps the value in Promise.resolve(), creates resume callbacks, attaches .then().
/// Returns the updated heap and any jobs to schedule.
fn async_setup_await(
  h: Heap,
  builtins: Builtins,
  async_data_ref: Ref,
  awaited_value: JsValue,
) -> #(Heap, List(value.Job)) {
  // Wrap awaited_value in Promise.resolve() if not already a promise
  let #(h, promise_data_ref) = case awaited_value {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Ok(ObjectSlot(kind: PromiseObject(pdata_ref), ..)) -> #(h, pdata_ref)
        _ -> {
          let #(h, _, dr) = create_resolved_promise(h, builtins, awaited_value)
          #(h, dr)
        }
      }
    _ -> {
      let #(h, _, dr) = create_resolved_promise(h, builtins, awaited_value)
      #(h, dr)
    }
  }
  // Create NativeAsyncResume callbacks
  let #(h, fulfill_resume_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(value.NativeAsyncResume(
          async_data_ref:,
          is_reject: False,
        )),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(builtins.function.prototype),
        symbol_properties: dict.new(),
      ),
    )
  let #(h, reject_resume_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(value.NativeAsyncResume(
          async_data_ref:,
          is_reject: True,
        )),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(builtins.function.prototype),
        symbol_properties: dict.new(),
      ),
    )
  // Attach .then(fulfillResume, rejectResume) to the awaited promise
  let #(h, child_ref, child_data_ref) =
    builtins_promise.create_promise(h, builtins.promise.prototype)
  let #(h, child_resolve, child_reject) =
    builtins_promise.create_resolving_functions(
      h,
      builtins.function.prototype,
      child_ref,
      child_data_ref,
    )
  builtins_promise.perform_promise_then(
    h,
    promise_data_ref,
    JsObject(fulfill_resume_ref),
    JsObject(reject_resume_ref),
    child_resolve,
    child_reject,
  )
}

/// NativeAsyncResume handler: called when an awaited promise settles.
/// Restores the async function's execution state and continues.
fn call_native_async_resume(
  state: State,
  async_data_ref: Ref,
  is_reject: Bool,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let settled_value = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case heap.read(state.heap, async_data_ref) {
    Ok(AsyncFunctionSlot(
      promise_data_ref:,
      resolve: slot_resolve,
      reject: slot_reject,
      func_template_id:,
      env_ref: slot_env_ref,
      saved_pc:,
      saved_locals:,
      saved_stack:,
      saved_try_stack:,
      saved_finally_stack:,
      saved_this:,
    )) -> {
      // Look up the func template
      let assert Ok(func_template) =
        dict.get(state.closure_templates, func_template_id)
      // Restore try/finally stacks
      let #(restored_try, restored_finally) =
        restore_stacks(saved_try_stack, saved_finally_stack)
      // Build the resume stack: push resolved value for fulfillment
      let resume_stack = case is_reject {
        False -> [settled_value, ..saved_stack]
        True -> saved_stack
      }
      let exec_state =
        State(
          ..state,
          stack: resume_stack,
          locals: saved_locals,
          func: func_template,
          code: func_template.bytecode,
          constants: func_template.constants,
          pc: saved_pc,
          call_stack: [],
          try_stack: restored_try,
          finally_stack: restored_finally,
          this_binding: saved_this,
          // TODO: save/restore callee_ref for new.target support after await
          callee_ref: None,
        )
      // For rejection, throw the value so try/catch inside async fn can handle it
      let exec_result = case is_reject {
        False -> execute_inner(exec_state)
        True -> {
          case unwind_to_catch(exec_state, settled_value) {
            Ok(caught_state) -> execute_inner(caught_state)
            Error(_) ->
              Ok(#(ThrowCompletion(settled_value, exec_state.heap), exec_state))
          }
        }
      }
      case exec_result {
        Ok(#(NormalCompletion(return_value, h2), final_state)) -> {
          // Async function completed — resolve the outer promise
          let #(h2, jobs) =
            builtins_promise.fulfill_promise(h2, promise_data_ref, return_value)
          Ok(
            State(
              ..state,
              heap: h2,
              stack: [JsUndefined, ..rest_stack],
              pc: state.pc + 1,
              closure_templates: final_state.closure_templates,
              globals: final_state.globals,
              job_queue: list.append(final_state.job_queue, jobs),
            ),
          )
        }
        Ok(#(ThrowCompletion(thrown, h2), final_state)) -> {
          // Async function threw — reject the outer promise
          let #(h2, jobs) =
            builtins_promise.reject_promise(h2, promise_data_ref, thrown)
          Ok(
            State(
              ..state,
              heap: h2,
              stack: [JsUndefined, ..rest_stack],
              pc: state.pc + 1,
              closure_templates: final_state.closure_templates,
              globals: final_state.globals,
              job_queue: list.append(final_state.job_queue, jobs),
            ),
          )
        }
        Ok(#(YieldCompletion(awaited_value, h2), suspended)) -> {
          // Hit another `await` — save state and set up promise resolution
          let #(saved_try, saved_finally) =
            save_stacks(suspended.try_stack, suspended.finally_stack)
          let h2 =
            heap.write(
              h2,
              async_data_ref,
              AsyncFunctionSlot(
                promise_data_ref:,
                resolve: slot_resolve,
                reject: slot_reject,
                func_template_id:,
                env_ref: slot_env_ref,
                saved_pc: suspended.pc,
                saved_locals: suspended.locals,
                saved_stack: suspended.stack,
                saved_try_stack: saved_try,
                saved_finally_stack: saved_finally,
                saved_this: suspended.this_binding,
              ),
            )
          let #(h2, jobs) =
            async_setup_await(h2, state.builtins, async_data_ref, awaited_value)
          Ok(
            State(
              ..state,
              heap: h2,
              stack: [JsUndefined, ..rest_stack],
              pc: state.pc + 1,
              closure_templates: suspended.closure_templates,
              globals: suspended.globals,
              job_queue: list.append(suspended.job_queue, jobs),
            ),
          )
        }
        Error(vm_err) -> Error(#(VmError(vm_err), JsUndefined, state.heap))
      }
    }
    _ ->
      Error(#(
        VmError(Unimplemented(
          "async resume: invalid slot for ref "
          <> string.inspect(async_data_ref),
        )),
        JsUndefined,
        state.heap,
      ))
  }
}

/// Helper: create a resolved promise wrapping a value.
/// Note: _jobs is always [] because this is a brand-new promise with no reactions.
fn create_resolved_promise(
  h: Heap,
  builtins: Builtins,
  val: JsValue,
) -> #(Heap, JsValue, Ref) {
  let #(h, promise_ref, data_ref) =
    builtins_promise.create_promise(h, builtins.promise.prototype)
  let #(h, _jobs) = builtins_promise.fulfill_promise(h, data_ref, val)
  #(h, JsObject(promise_ref), data_ref)
}

/// Set up locals for a function call: [env_values, padded_args, uninitialized].
fn setup_locals(
  h: Heap,
  env_ref: value.Ref,
  callee_template: FuncTemplate,
  args: List(JsValue),
) -> array.Array(JsValue) {
  let env_values = case heap.read(h, env_ref) {
    Ok(value.EnvSlot(slots)) -> slots
    _ -> []
  }
  let env_count = list.length(env_values)
  let padded_args = pad_args(args, callee_template.arity)
  let remaining =
    callee_template.local_count - env_count - callee_template.arity
  list.flatten([
    env_values,
    padded_args,
    list.repeat(JsUndefined, remaining),
  ])
  |> array.from_list
}

/// Save try/finally stacks from frame types to value types for generator suspension.
fn save_stacks(
  try_stack: List(TryFrame),
  finally_stack: List(FinallyCompletion),
) -> #(List(value.SavedTryFrame), List(value.SavedFinallyCompletion)) {
  let saved_try =
    list.map(try_stack, fn(tf) {
      value.SavedTryFrame(
        catch_target: tf.catch_target,
        stack_depth: tf.stack_depth,
      )
    })
  let saved_finally = list.map(finally_stack, convert_finally_completion)
  #(saved_try, saved_finally)
}

/// Restore saved try/finally stacks back to frame types for generator resumption.
fn restore_stacks(
  saved_try_stack: List(value.SavedTryFrame),
  saved_finally_stack: List(value.SavedFinallyCompletion),
) -> #(List(TryFrame), List(FinallyCompletion)) {
  let restored_try =
    list.map(saved_try_stack, fn(stf) {
      TryFrame(catch_target: stf.catch_target, stack_depth: stf.stack_depth)
    })
  let restored_finally =
    list.map(saved_finally_stack, restore_finally_completion)
  #(restored_try, restored_finally)
}

/// Convert frame.FinallyCompletion to value.SavedFinallyCompletion for storage.
fn convert_finally_completion(
  fc: FinallyCompletion,
) -> value.SavedFinallyCompletion {
  case fc {
    frame.NormalCompletion -> value.SavedNormalCompletion
    frame.ThrowCompletion(v) -> value.SavedThrowCompletion(v)
    frame.ReturnCompletion(v) -> value.SavedReturnCompletion(v)
  }
}

/// Convert value.SavedFinallyCompletion back to frame.FinallyCompletion.
fn restore_finally_completion(
  sfc: value.SavedFinallyCompletion,
) -> FinallyCompletion {
  case sfc {
    value.SavedNormalCompletion -> frame.NormalCompletion
    value.SavedThrowCompletion(v) -> frame.ThrowCompletion(v)
    value.SavedReturnCompletion(v) -> frame.ReturnCompletion(v)
  }
}

/// Call a native (Gleam-implemented) function. Most natives execute synchronously
/// and push their result onto the stack. However, call/apply/bind need special
/// handling because they invoke other functions (potentially pushing call frames).
fn call_native(
  state: State,
  native: value.NativeFn,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case native {
    // Function.prototype.call(thisArg, ...args)
    // `this` is the target function, args[0] is the thisArg
    value.NativeFunctionCall -> {
      let #(this_arg, call_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(JsUndefined, [])
      }
      call_value(State(..state, stack: rest_stack), this, call_args, this_arg)
    }
    // Function.prototype.apply(thisArg, argsArray)
    // `this` is the target function, args[0] is thisArg, args[1] is array
    value.NativeFunctionApply -> {
      let this_arg = case args {
        [t, ..] -> t
        _ -> JsUndefined
      }
      let call_args = case args {
        [_, JsObject(arr_ref), ..] -> extract_array_args(state.heap, arr_ref)
        // null/undefined argsArray → no args
        _ -> []
      }
      call_value(State(..state, stack: rest_stack), this, call_args, this_arg)
    }
    // Function.prototype.bind(thisArg, ...args)
    // Creates a bound function object
    value.NativeFunctionBind -> {
      let #(this_arg, bound_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(JsUndefined, [])
      }
      case this {
        JsObject(target_ref) -> {
          // Get the target's name for "bound <name>"
          let name = case heap.read(state.heap, target_ref) {
            Ok(ObjectSlot(properties:, ..)) ->
              case dict.get(properties, "name") {
                Ok(DataProperty(value: JsString(n), ..)) -> "bound " <> n
                _ -> "bound "
              }
            _ -> "bound "
          }
          let #(h, bound_ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: NativeFunction(value.NativeBoundFunction(
                  target: target_ref,
                  bound_this: this_arg,
                  bound_args:,
                )),
                properties: dict.from_list([
                  #("name", value.builtin_property(JsString(name))),
                ]),
                elements: js_elements.new(),
                prototype: Some(state.builtins.function.prototype),
                symbol_properties: dict.new(),
              ),
            )
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(bound_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        _ -> {
          let #(h, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Bind must be called on a function",
            )
          Error(#(Thrown, err, h))
        }
      }
    }
    // Bound function: prepend bound_args, use bound_this
    value.NativeBoundFunction(target:, bound_this:, bound_args:) -> {
      let final_args = list.append(bound_args, args)
      call_value(
        State(..state, stack: rest_stack),
        JsObject(target),
        final_args,
        bound_this,
      )
    }
    // Promise constructor: new Promise(executor)
    value.NativePromiseConstructor ->
      call_native_promise_constructor(state, args, rest_stack)
    // Promise resolve/reject internal functions
    value.NativePromiseResolveFunction(
      promise_ref:,
      data_ref:,
      already_resolved_ref:,
    ) ->
      call_native_promise_resolve_fn(
        state,
        promise_ref,
        data_ref,
        already_resolved_ref,
        args,
        rest_stack,
      )
    value.NativePromiseRejectFunction(
      promise_ref: _,
      data_ref:,
      already_resolved_ref:,
    ) ->
      call_native_promise_reject_fn(
        state,
        data_ref,
        already_resolved_ref,
        args,
        rest_stack,
      )
    // Promise.prototype.then(onFulfilled, onRejected)
    value.NativePromiseThen ->
      call_native_promise_then(state, this, args, rest_stack)
    // Promise.prototype.catch(onRejected) — sugar for .then(undefined, onRejected)
    value.NativePromiseCatch ->
      call_native_promise_then(state, this, [JsUndefined, ..args], rest_stack)
    // Promise.prototype.finally(onFinally)
    value.NativePromiseFinally ->
      call_native_promise_finally(state, this, args, rest_stack)
    // Promise.resolve(value)
    value.NativePromiseResolveStatic ->
      call_native_promise_resolve_static(state, args, rest_stack)
    // Promise.reject(reason)
    value.NativePromiseRejectStatic ->
      call_native_promise_reject_static(state, args, rest_stack)
    // Promise.prototype.finally wrapper functions
    value.NativePromiseFinallyFulfill(on_finally:) ->
      call_native_finally_fulfill(state, on_finally, args, rest_stack)
    value.NativePromiseFinallyReject(on_finally:) ->
      call_native_finally_reject(state, on_finally, args, rest_stack)
    value.NativePromiseFinallyValueThunk(value: captured_value) -> {
      // Ignore argument, return the captured value
      Ok(
        State(..state, stack: [captured_value, ..rest_stack], pc: state.pc + 1),
      )
    }
    value.NativePromiseFinallyThrower(reason:) -> {
      // Ignore argument, throw the captured reason
      Error(#(Thrown, reason, state.heap))
    }
    // Async function resume (called when awaited promise settles)
    value.NativeAsyncResume(async_data_ref:, is_reject:) ->
      call_native_async_resume(
        state,
        async_data_ref,
        is_reject,
        args,
        rest_stack,
      )
    // Generator prototype methods
    value.NativeGeneratorNext ->
      call_native_generator_next(state, this, args, rest_stack)
    value.NativeGeneratorReturn ->
      call_native_generator_return(state, this, args, rest_stack)
    value.NativeGeneratorThrow ->
      call_native_generator_throw(state, this, args, rest_stack)
    // Symbol() constructor — callable but NOT new-able
    value.NativeSymbolConstructor -> {
      let #(next_id, new_descs, sym_val) =
        builtins_symbol.call_symbol(
          args,
          state.next_symbol_id,
          state.symbol_descriptions,
        )
      Ok(
        State(
          ..state,
          stack: [sym_val, ..rest_stack],
          pc: state.pc + 1,
          next_symbol_id: next_id,
          symbol_descriptions: new_descs,
        ),
      )
    }
    // String() constructor — uses full ToString (ToPrimitive for objects)
    value.NativeStringConstructor ->
      case args {
        [] ->
          Ok(
            State(
              ..state,
              stack: [JsString(""), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        [val, ..] ->
          case js_to_string(state, val) {
            Ok(#(s, new_state)) ->
              Ok(
                State(
                  ..new_state,
                  stack: [JsString(s), ..rest_stack],
                  pc: state.pc + 1,
                ),
              )
            Error(#(thrown, new_state)) ->
              Error(#(Thrown, thrown, new_state.heap))
          }
      }
    // All other native functions: synchronous dispatch
    _ -> {
      let #(new_state, result) = dispatch_native(native, args, this, state)
      case result {
        Ok(return_value) ->
          Ok(
            State(
              ..new_state,
              stack: [return_value, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        Error(thrown) -> Error(#(Thrown, thrown, new_state.heap))
      }
    }
  }
}

/// Construct a new object using the target function ref.
/// Used by CallConstructor when the constructor is a bound function.
fn construct_value(
  state: State,
  target_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case heap.read(state.heap, target_ref) {
    Ok(ObjectSlot(
      kind: FunctionObject(func_index: _, env: env_ref),
      properties:,
      ..,
    )) -> {
      let proto = case dict.get(properties, "prototype") {
        Ok(DataProperty(value: JsObject(proto_ref), ..)) -> Some(proto_ref)
        _ -> Some(state.builtins.object.prototype)
      }
      let #(h, new_obj_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: proto,
            symbol_properties: dict.new(),
          ),
        )
      let new_obj = JsObject(new_obj_ref)
      call_function(
        State(..state, heap: h),
        target_ref,
        env_ref,
        args,
        rest_stack,
        new_obj,
        Some(new_obj),
        None,
      )
    }
    // Chained bound function: resolve further
    Ok(ObjectSlot(
      kind: NativeFunction(value.NativeBoundFunction(target:, bound_args:, ..)),
      ..,
    )) -> {
      let final_args = list.append(bound_args, args)
      construct_value(state, target, final_args, rest_stack)
    }
    Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
      call_native(state, native, args, rest_stack, JsUndefined)
    _ -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          object.inspect(JsObject(target_ref), state.heap)
            <> " is not a constructor",
        )
      Error(#(Thrown, err, h))
    }
  }
}

/// Call an arbitrary JsValue as a function with the given this and args.
/// Used by Function.prototype.call/apply and bound function invocation.
fn call_value(
  state: State,
  callee: JsValue,
  args: List(JsValue),
  this_val: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case callee {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Ok(ObjectSlot(kind: FunctionObject(func_index: _, env: env_ref), ..)) ->
          call_function(
            state,
            ref,
            env_ref,
            args,
            state.stack,
            this_val,
            None,
            None,
          )
        Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
          call_native(state, native, args, state.stack, this_val)
        _ -> {
          let #(h, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              object.inspect(callee, state.heap) <> " is not a function",
            )
          Error(#(Thrown, err, h))
        }
      }
    _ -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          object.inspect(callee, state.heap) <> " is not a function",
        )
      Error(#(Thrown, err, h))
    }
  }
}

/// Extract elements from an array object as a list of JsValues.
/// Used by Function.prototype.apply to unpack the args array.
fn extract_array_args(h: Heap, ref: Ref) -> List(JsValue) {
  case heap.read(h, ref) {
    Ok(ObjectSlot(kind: ArrayObject(length:), elements:, ..)) ->
      extract_elements_loop(elements, 0, length, [])
    _ -> []
  }
}

fn extract_elements_loop(
  elements: value.JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let val = js_elements.get(elements, idx)
      extract_elements_loop(elements, idx + 1, length, [val, ..acc])
    }
  }
}

/// Route a NativeFn call to the correct builtin module.
fn dispatch_native(
  native: value.NativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    value.NativeObjectConstructor ->
      builtins_object.call_native(
        args,
        this,
        state,
        state.builtins.object.prototype,
      )
    value.NativeFunctionConstructor -> {
      let #(heap, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "Function constructor is not supported",
        )
      #(State(..state, heap:), Error(err))
    }
    value.NativeArrayConstructor ->
      builtins_array.construct(args, state, state.builtins.array.prototype)
    value.NativeArrayIsArray -> builtins_array.is_array(args, state)
    value.NativeErrorConstructor(proto:) ->
      builtins_error.call_native(proto, args, this, state)
    value.NativeObjectGetOwnPropertyDescriptor ->
      builtins_object.get_own_property_descriptor(
        args,
        state,
        state.builtins.object.prototype,
      )
    value.NativeObjectDefineProperty ->
      builtins_object.define_property(args, state)
    value.NativeObjectGetOwnPropertyNames ->
      builtins_object.get_own_property_names(
        args,
        state,
        state.builtins.array.prototype,
      )
    value.NativeObjectKeys ->
      builtins_object.keys(args, state, state.builtins.array.prototype)
    value.NativeObjectPrototypeHasOwnProperty ->
      builtins_object.has_own_property(this, args, state)
    value.NativeObjectPrototypePropertyIsEnumerable ->
      builtins_object.property_is_enumerable(this, args, state)
    value.NativeObjectPrototypeToString ->
      builtins_object.object_to_string(this, args, state)
    value.NativeObjectPrototypeValueOf ->
      builtins_object.object_value_of(this, args, state)
    value.NativeArrayPrototypeJoin ->
      builtins_array.array_join(this, args, state)
    value.NativeArrayPrototypePush ->
      builtins_array.array_push(this, args, state)
    value.NativeMathPow -> builtins_math.math_pow(args, state)
    // Math methods
    value.NativeMathAbs -> builtins_math.math_abs(args, state)
    value.NativeMathFloor -> builtins_math.math_floor(args, state)
    value.NativeMathCeil -> builtins_math.math_ceil(args, state)
    value.NativeMathRound -> builtins_math.math_round(args, state)
    value.NativeMathTrunc -> builtins_math.math_trunc(args, state)
    value.NativeMathSqrt -> builtins_math.math_sqrt(args, state)
    value.NativeMathMax -> builtins_math.math_max(args, state)
    value.NativeMathMin -> builtins_math.math_min(args, state)
    value.NativeMathLog -> builtins_math.math_log(args, state)
    value.NativeMathSin -> builtins_math.math_sin(args, state)
    value.NativeMathCos -> builtins_math.math_cos(args, state)
    // String.prototype methods
    value.NativeStringPrototypeCharAt ->
      builtins_string.string_char_at(this, args, state)
    value.NativeStringPrototypeCharCodeAt ->
      builtins_string.string_char_code_at(this, args, state)
    value.NativeStringPrototypeIndexOf ->
      builtins_string.string_index_of(this, args, state)
    value.NativeStringPrototypeLastIndexOf ->
      builtins_string.string_last_index_of(this, args, state)
    value.NativeStringPrototypeIncludes ->
      builtins_string.string_includes(this, args, state)
    value.NativeStringPrototypeStartsWith ->
      builtins_string.string_starts_with(this, args, state)
    value.NativeStringPrototypeEndsWith ->
      builtins_string.string_ends_with(this, args, state)
    value.NativeStringPrototypeSlice ->
      builtins_string.string_slice(this, args, state)
    value.NativeStringPrototypeSubstring ->
      builtins_string.string_substring(this, args, state)
    value.NativeStringPrototypeToLowerCase ->
      builtins_string.string_to_lower_case(this, args, state)
    value.NativeStringPrototypeToUpperCase ->
      builtins_string.string_to_upper_case(this, args, state)
    value.NativeStringPrototypeTrim ->
      builtins_string.string_trim(this, args, state)
    value.NativeStringPrototypeTrimStart ->
      builtins_string.string_trim_start(this, args, state)
    value.NativeStringPrototypeTrimEnd ->
      builtins_string.string_trim_end(this, args, state)
    value.NativeStringPrototypeSplit ->
      builtins_string.string_split(
        this,
        args,
        state,
        state.builtins.array.prototype,
      )
    value.NativeStringPrototypeConcat ->
      builtins_string.string_concat(this, args, state)
    value.NativeStringPrototypeToString ->
      builtins_string.string_to_string(this, args, state)
    value.NativeStringPrototypeValueOf ->
      builtins_string.string_value_of(this, args, state)
    value.NativeStringPrototypeRepeat ->
      builtins_string.string_repeat(this, args, state)
    value.NativeStringPrototypePadStart ->
      builtins_string.string_pad_start(this, args, state)
    value.NativeStringPrototypePadEnd ->
      builtins_string.string_pad_end(this, args, state)
    value.NativeStringPrototypeAt ->
      builtins_string.string_at(this, args, state)
    // Number/Boolean constructors (type coercion)
    // NativeStringConstructor is handled in call_native (needs ToPrimitive)
    value.NativeStringConstructor ->
      panic as "NativeStringConstructor should be handled in call_native"
    value.NativeNumberConstructor ->
      builtins_number.call_as_function(args, state)
    value.NativeBooleanConstructor ->
      builtins_boolean.call_as_function(args, state)
    // Global utility functions
    value.NativeParseInt -> builtins_number.parse_int(args, state)
    value.NativeParseFloat -> builtins_number.parse_float(args, state)
    value.NativeIsNaN -> builtins_number.js_is_nan(args, state)
    value.NativeIsFinite -> builtins_number.js_is_finite(args, state)
    // Number static methods (strict — no coercion)
    value.NativeNumberIsNaN -> builtins_number.number_is_nan(args, state)
    value.NativeNumberIsFinite -> builtins_number.number_is_finite(args, state)
    value.NativeNumberIsInteger ->
      builtins_number.number_is_integer(args, state)
    // Number.parseInt/parseFloat are identical to global ones per spec
    value.NativeNumberParseInt -> builtins_number.parse_int(args, state)
    value.NativeNumberParseFloat -> builtins_number.parse_float(args, state)
    // These are handled in call_native before reaching dispatch_native.
    // If we ever get here, it's a bug.
    value.NativeFunctionCall
    | value.NativeFunctionApply
    | value.NativeFunctionBind
    | value.NativeBoundFunction(..) ->
      panic as "Function call/apply/bind/bound should be handled in call_native"
    value.NativePromiseConstructor
    | value.NativePromiseThen
    | value.NativePromiseCatch
    | value.NativePromiseFinally
    | value.NativePromiseResolveStatic
    | value.NativePromiseRejectStatic
    | value.NativePromiseResolveFunction(..)
    | value.NativePromiseRejectFunction(..)
    | value.NativePromiseFinallyFulfill(..)
    | value.NativePromiseFinallyReject(..)
    | value.NativePromiseFinallyValueThunk(..)
    | value.NativePromiseFinallyThrower(..) ->
      panic as "Promise natives should be handled in call_native"
    value.NativeGeneratorNext
    | value.NativeGeneratorReturn
    | value.NativeGeneratorThrow ->
      panic as "Generator natives should be handled in call_native"
    value.NativeAsyncResume(..) ->
      panic as "NativeAsyncResume should be handled in call_native"
    value.NativeSymbolConstructor ->
      panic as "NativeSymbolConstructor should be handled in call_native"
    // %IteratorPrototype%[Symbol.iterator]() — returns `this`
    value.NativeIteratorSymbolIterator -> #(state, Ok(this))
  }
}

// ============================================================================
// Promise native function implementations
// ============================================================================

/// new Promise(executor) — create promise, call executor(resolve, reject),
/// catch throws and reject.
fn call_native_promise_constructor(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let executor = case args {
    [f, ..] -> f
    [] -> JsUndefined
  }
  // Verify executor is callable
  case is_callable_value(state.heap, executor) {
    False -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "Promise resolver is not a function",
        )
      Error(#(Thrown, err, h))
    }
    True -> {
      let #(h, promise_ref, data_ref) =
        builtins_promise.create_promise(
          state.heap,
          state.builtins.promise.prototype,
        )
      let #(h, resolve_fn, reject_fn) =
        builtins_promise.create_resolving_functions(
          h,
          state.builtins.function.prototype,
          promise_ref,
          data_ref,
        )
      // Run executor inline — its return value is discarded, the promise is the result.
      let new_state = State(..state, heap: h, stack: rest_stack)
      case
        run_handler_with_this(new_state, executor, JsUndefined, [
          resolve_fn,
          reject_fn,
        ])
      {
        Ok(#(_, after_state)) ->
          Ok(
            State(
              ..after_state,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        Error(#(thrown, after_state)) -> {
          let #(h, jobs) =
            builtins_promise.reject_promise(after_state.heap, data_ref, thrown)
          Ok(
            State(
              ..after_state,
              heap: h,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
              job_queue: list.append(after_state.job_queue, jobs),
            ),
          )
        }
      }
    }
  }
}

/// Internal resolve function — check already-resolved, then fulfill/reject.
fn call_native_promise_resolve_fn(
  state: State,
  promise_ref: Ref,
  data_ref: Ref,
  already_resolved_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let resolution = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Check if already resolved
  case heap.read(state.heap, already_resolved_ref) {
    Ok(value.BoxSlot(value: JsBool(True))) ->
      // Already resolved — ignore
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    _ -> {
      // Mark as resolved
      let h =
        heap.write(
          state.heap,
          already_resolved_ref,
          value.BoxSlot(value: JsBool(True)),
        )
      // Check for self-resolution
      case resolution == JsObject(promise_ref) {
        True -> {
          let #(h, err) =
            object.make_type_error(
              h,
              state.builtins,
              "Chaining cycle detected for promise",
            )
          let #(h, jobs) = builtins_promise.reject_promise(h, data_ref, err)
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsUndefined, ..rest_stack],
              pc: state.pc + 1,
              job_queue: list.append(state.job_queue, jobs),
            ),
          )
        }
        False -> {
          // Check if resolution is a thenable
          case builtins_promise.get_thenable_then(h, resolution) {
            Ok(then_fn) -> {
              // Create resolving functions for assimilation
              let #(h, resolve_fn, reject_fn) =
                builtins_promise.create_resolving_functions(
                  h,
                  state.builtins.function.prototype,
                  promise_ref,
                  data_ref,
                )
              let job =
                value.PromiseResolveThenableJob(
                  thenable: resolution,
                  then_fn:,
                  resolve: resolve_fn,
                  reject: reject_fn,
                )
              Ok(
                State(
                  ..state,
                  heap: h,
                  stack: [JsUndefined, ..rest_stack],
                  pc: state.pc + 1,
                  job_queue: list.append(state.job_queue, [job]),
                ),
              )
            }
            Error(_) -> {
              // Not a thenable — fulfill directly
              let #(h, jobs) =
                builtins_promise.fulfill_promise(h, data_ref, resolution)
              Ok(
                State(
                  ..state,
                  heap: h,
                  stack: [JsUndefined, ..rest_stack],
                  pc: state.pc + 1,
                  job_queue: list.append(state.job_queue, jobs),
                ),
              )
            }
          }
        }
      }
    }
  }
}

/// Internal reject function — check already-resolved, then reject.
fn call_native_promise_reject_fn(
  state: State,
  data_ref: Ref,
  already_resolved_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let reason = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Check if already resolved
  case heap.read(state.heap, already_resolved_ref) {
    Ok(value.BoxSlot(value: JsBool(True))) ->
      Ok(State(..state, stack: [JsUndefined, ..rest_stack], pc: state.pc + 1))
    _ -> {
      let h =
        heap.write(
          state.heap,
          already_resolved_ref,
          value.BoxSlot(value: JsBool(True)),
        )
      let #(h, jobs) = builtins_promise.reject_promise(h, data_ref, reason)
      Ok(
        State(
          ..state,
          heap: h,
          stack: [JsUndefined, ..rest_stack],
          pc: state.pc + 1,
          job_queue: list.append(state.job_queue, jobs),
        ),
      )
    }
  }
}

/// Promise.prototype.then(onFulfilled, onRejected)
fn call_native_promise_then(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let on_fulfilled = case args {
    [f, ..] -> f
    [] -> JsUndefined
  }
  let on_rejected = case args {
    [_, r, ..] -> r
    _ -> JsUndefined
  }
  // Get the promise data ref from `this`
  case this {
    JsObject(this_ref) ->
      case builtins_promise.get_data_ref(state.heap, this_ref) {
        Ok(data_ref) -> {
          // Create child promise (the one returned by .then)
          let #(h, child_ref, child_data_ref) =
            builtins_promise.create_promise(
              state.heap,
              state.builtins.promise.prototype,
            )
          // Create resolving functions for the child
          let #(h, child_resolve, child_reject) =
            builtins_promise.create_resolving_functions(
              h,
              state.builtins.function.prototype,
              child_ref,
              child_data_ref,
            )
          // Perform the .then logic
          let #(h, jobs) =
            builtins_promise.perform_promise_then(
              h,
              data_ref,
              on_fulfilled,
              on_rejected,
              child_resolve,
              child_reject,
            )
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(child_ref), ..rest_stack],
              pc: state.pc + 1,
              job_queue: list.append(state.job_queue, jobs),
            ),
          )
        }
        Error(_) -> {
          let #(h, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "then called on non-promise",
            )
          Error(#(Thrown, err, h))
        }
      }
    _ -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "then called on non-promise",
        )
      Error(#(Thrown, err, h))
    }
  }
}

/// Promise.prototype.finally(onFinally) — per spec, wraps the handler
/// to preserve the resolution value. Creates wrapper functions that call
/// onFinally(), then pass through the original value/reason via
/// Promise.resolve(result).then(thunk).
fn call_native_promise_finally(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let on_finally = case args {
    [f, ..] -> f
    [] -> JsUndefined
  }
  // If onFinally is not callable, pass-through (like .then(onFinally, onFinally))
  case is_callable_value(state.heap, on_finally) {
    False ->
      call_native_promise_then(
        state,
        this,
        [on_finally, on_finally],
        rest_stack,
      )
    True -> {
      // Create fulfill wrapper: calls onFinally(), then returns original value
      let #(h, fulfill_ref) =
        heap.alloc(
          state.heap,
          value.ObjectSlot(
            kind: value.NativeFunction(value.NativePromiseFinallyFulfill(
              on_finally:,
            )),
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(state.builtins.function.prototype),
            symbol_properties: dict.new(),
          ),
        )
      // Create reject wrapper: calls onFinally(), then re-throws original reason
      let #(h, reject_ref) =
        heap.alloc(
          h,
          value.ObjectSlot(
            kind: value.NativeFunction(value.NativePromiseFinallyReject(
              on_finally:,
            )),
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(state.builtins.function.prototype),
            symbol_properties: dict.new(),
          ),
        )
      call_native_promise_then(
        State(..state, heap: h),
        this,
        [JsObject(fulfill_ref), JsObject(reject_ref)],
        rest_stack,
      )
    }
  }
}

/// Promise.prototype.finally fulfill wrapper — called when promise fulfills.
/// Calls onFinally(), then Promise.resolve(result).then(() => original_value).
fn call_native_finally_fulfill(
  state: State,
  on_finally: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let original_value = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Call onFinally() with no arguments
  let result = run_handler_with_this(state, on_finally, JsUndefined, [])
  case result {
    Ok(#(finally_result, new_state)) ->
      // Create Promise.resolve(finally_result).then(value_thunk)
      finally_chain_value(new_state, finally_result, original_value, rest_stack)
    Error(#(thrown, new_state)) ->
      // onFinally() threw — propagate the throw
      Error(#(Thrown, thrown, new_state.heap))
  }
}

/// Promise.prototype.finally reject wrapper — called when promise rejects.
/// Calls onFinally(), then Promise.resolve(result).then(() => { throw reason }).
fn call_native_finally_reject(
  state: State,
  on_finally: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let original_reason = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Call onFinally() with no arguments
  let result = run_handler_with_this(state, on_finally, JsUndefined, [])
  case result {
    Ok(#(finally_result, new_state)) ->
      // Create Promise.resolve(finally_result).then(thrower)
      finally_chain_throw(
        new_state,
        finally_result,
        original_reason,
        rest_stack,
      )
    Error(#(thrown, new_state)) ->
      // onFinally() threw — propagate the throw (overrides original reason)
      Error(#(Thrown, thrown, new_state.heap))
  }
}

/// Create Promise.resolve(value).then(thunk) where thunk returns captured_value.
fn finally_chain_value(
  state: State,
  resolve_value: JsValue,
  captured_value: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // Promise.resolve(resolve_value)
  let #(h, resolved_ref, resolved_data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let #(h, resolve_fn, _reject_fn) =
    builtins_promise.create_resolving_functions(
      h,
      state.builtins.function.prototype,
      resolved_ref,
      resolved_data_ref,
    )
  // Call resolve(resolve_value)
  let state1 =
    call_native_for_job(State(..state, heap: h), resolve_fn, [
      resolve_value,
    ])
  // Create the value thunk
  let #(h2, thunk_ref) =
    heap.alloc(
      state1.heap,
      value.ObjectSlot(
        kind: value.NativeFunction(value.NativePromiseFinallyValueThunk(
          value: captured_value,
        )),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(state.builtins.function.prototype),
        symbol_properties: dict.new(),
      ),
    )
  // Chain .then(thunk) on the resolved promise
  call_native_promise_then(
    State(..state1, heap: h2),
    JsObject(resolved_ref),
    [JsObject(thunk_ref), JsUndefined],
    rest_stack,
  )
}

/// Create Promise.resolve(value).then(thrower) where thrower re-throws captured_reason.
fn finally_chain_throw(
  state: State,
  resolve_value: JsValue,
  captured_reason: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  // Promise.resolve(resolve_value)
  let #(h, resolved_ref, resolved_data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let #(h, resolve_fn, _reject_fn) =
    builtins_promise.create_resolving_functions(
      h,
      state.builtins.function.prototype,
      resolved_ref,
      resolved_data_ref,
    )
  // Call resolve(resolve_value)
  let state1 =
    call_native_for_job(State(..state, heap: h), resolve_fn, [
      resolve_value,
    ])
  // Create the thrower
  let #(h2, thrower_ref) =
    heap.alloc(
      state1.heap,
      value.ObjectSlot(
        kind: value.NativeFunction(value.NativePromiseFinallyThrower(
          reason: captured_reason,
        )),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(state.builtins.function.prototype),
        symbol_properties: dict.new(),
      ),
    )
  // Chain .then(thrower) on the resolved promise
  call_native_promise_then(
    State(..state1, heap: h2),
    JsObject(resolved_ref),
    [JsObject(thrower_ref), JsUndefined],
    rest_stack,
  )
}

/// Promise.resolve(value) — if value is already a promise with same constructor,
/// return it. Otherwise create and resolve a new promise.
fn call_native_promise_resolve_static(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // If val is already a Promise, return it directly
  case builtins_promise.is_promise(state.heap, val) {
    True -> Ok(State(..state, stack: [val, ..rest_stack], pc: state.pc + 1))
    False -> {
      // Create new promise and resolve it
      let #(h, promise_ref, data_ref) =
        builtins_promise.create_promise(
          state.heap,
          state.builtins.promise.prototype,
        )
      // Check for thenable
      case builtins_promise.get_thenable_then(h, val) {
        Ok(then_fn) -> {
          let #(h, resolve_fn, reject_fn) =
            builtins_promise.create_resolving_functions(
              h,
              state.builtins.function.prototype,
              promise_ref,
              data_ref,
            )
          let job =
            value.PromiseResolveThenableJob(
              thenable: val,
              then_fn:,
              resolve: resolve_fn,
              reject: reject_fn,
            )
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
              job_queue: list.append(state.job_queue, [job]),
            ),
          )
        }
        Error(_) -> {
          let #(h, jobs) = builtins_promise.fulfill_promise(h, data_ref, val)
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(promise_ref), ..rest_stack],
              pc: state.pc + 1,
              job_queue: list.append(state.job_queue, jobs),
            ),
          )
        }
      }
    }
  }
}

/// Promise.reject(reason) — create a new rejected promise.
fn call_native_promise_reject_static(
  state: State,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let reason = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  let #(h, promise_ref, data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let #(h, jobs) = builtins_promise.reject_promise(h, data_ref, reason)
  Ok(
    State(
      ..state,
      heap: h,
      stack: [JsObject(promise_ref), ..rest_stack],
      pc: state.pc + 1,
      job_queue: list.append(state.job_queue, jobs),
    ),
  )
}

/// Helper: check if a JsValue is callable.
fn is_callable_value(h: Heap, val: JsValue) -> Bool {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Ok(ObjectSlot(kind: FunctionObject(..), ..)) -> True
        Ok(ObjectSlot(kind: NativeFunction(_), ..)) -> True
        _ -> False
      }
    _ -> False
  }
}

// ============================================================================
// Generator native function implementations
// ============================================================================

/// Generator.prototype.next(value) — resume a suspended generator.
fn call_native_generator_next(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let next_arg = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case get_generator_data(state.heap, this) {
    Ok(gen) ->
      case gen.gen_state {
        value.Completed -> {
          // Already done — return {value: undefined, done: true}
          let #(h, result) =
            create_iterator_result(
              state.heap,
              state.builtins,
              JsUndefined,
              True,
            )
          Ok(
            State(
              ..state,
              heap: h,
              stack: [result, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        value.Executing -> {
          let #(h, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Generator is already running",
            )
          Error(#(Thrown, err, h))
        }
        value.SuspendedStart | value.SuspendedYield -> {
          // Mark as executing
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Executing),
            )
          // Restore the generator's execution state
          let #(restored_try, restored_finally) =
            restore_stacks(gen.saved_try_stack, gen.saved_finally_stack)
          // For SuspendedYield, push the .next() arg onto the saved stack
          // (the Yield opcode left pc pointing past Yield, stack has value popped)
          let gen_stack = case gen.gen_state {
            value.SuspendedYield -> [next_arg, ..gen.saved_stack]
            _ -> gen.saved_stack
          }
          // Look up the func template from closure_templates
          let assert Ok(func_template) =
            dict.get(state.closure_templates, gen.func_template_id)
          let gen_exec_state =
            State(
              ..state,
              heap: h,
              stack: gen_stack,
              locals: gen.saved_locals,
              func: func_template,
              code: func_template.bytecode,
              constants: func_template.constants,
              pc: gen.saved_pc,
              call_stack: [],
              try_stack: restored_try,
              finally_stack: restored_finally,
              this_binding: gen.saved_this,
              // TODO: save/restore callee_ref for new.target support after yield
              callee_ref: None,
            )
          // Execute until yield/return/throw
          case execute_inner(gen_exec_state) {
            Ok(#(YieldCompletion(yielded_value, h2), suspended)) -> {
              // Generator yielded — save state back
              let #(saved_try2, saved_finally2) =
                save_stacks(suspended.try_stack, suspended.finally_stack)
              let h3 =
                heap.write(
                  h2,
                  gen.data_ref,
                  GeneratorSlot(
                    gen_state: value.SuspendedYield,
                    func_template_id: gen.func_template_id,
                    env_ref: gen.env_ref,
                    saved_pc: suspended.pc,
                    saved_locals: suspended.locals,
                    saved_stack: suspended.stack,
                    saved_try_stack: saved_try2,
                    saved_finally_stack: saved_finally2,
                    saved_this: suspended.this_binding,
                  ),
                )
              let #(h3, result) =
                create_iterator_result(h3, state.builtins, yielded_value, False)
              Ok(
                State(
                  ..state,
                  heap: h3,
                  stack: [result, ..rest_stack],
                  pc: state.pc + 1,
                  closure_templates: suspended.closure_templates,
                  globals: suspended.globals,
                  job_queue: suspended.job_queue,
                ),
              )
            }
            Ok(#(NormalCompletion(return_value, h2), final_state)) -> {
              // Generator returned — mark completed
              let h3 =
                heap.write(
                  h2,
                  gen.data_ref,
                  gen_with_state(gen, value.Completed),
                )
              let #(h3, result) =
                create_iterator_result(h3, state.builtins, return_value, True)
              Ok(
                State(
                  ..state,
                  heap: h3,
                  stack: [result, ..rest_stack],
                  pc: state.pc + 1,
                  closure_templates: final_state.closure_templates,
                  globals: final_state.globals,
                  job_queue: final_state.job_queue,
                ),
              )
            }
            Ok(#(ThrowCompletion(thrown, h2), _final_state)) -> {
              // Generator threw — mark completed and propagate
              let h3 =
                heap.write(
                  h2,
                  gen.data_ref,
                  gen_with_state(gen, value.Completed),
                )
              Error(#(Thrown, thrown, h3))
            }
            Error(_vm_err) -> {
              let h2 =
                heap.write(
                  state.heap,
                  gen.data_ref,
                  gen_with_state(gen, value.Completed),
                )
              Error(#(
                VmError(Unimplemented("generator execution failed")),
                JsUndefined,
                h2,
              ))
            }
          }
        }
      }
    Error(_) -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "not a generator object",
        )
      Error(#(Thrown, err, h))
    }
  }
}

/// Generator.prototype.return(value) — complete the generator with a return value.
fn call_native_generator_return(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let return_val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case get_generator_data(state.heap, this) {
    Ok(gen) ->
      case gen.gen_state {
        value.Completed | value.SuspendedStart -> {
          // Mark completed and return {value, done: true}
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Completed),
            )
          let #(h, result) =
            create_iterator_result(h, state.builtins, return_val, True)
          Ok(
            State(
              ..state,
              heap: h,
              stack: [result, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        value.Executing -> {
          let #(h, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Generator is already running",
            )
          Error(#(Thrown, err, h))
        }
        value.SuspendedYield -> {
          // Full spec: resume with return completion so finally blocks run.
          // Mark as executing, restore generator state, then process through
          // any enclosing finally blocks before completing.
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Executing),
            )
          // Restore the generator's execution state
          let #(restored_try, restored_finally) =
            restore_stacks(gen.saved_try_stack, gen.saved_finally_stack)
          let assert Ok(func_template) =
            dict.get(state.closure_templates, gen.func_template_id)
          let gen_exec_state =
            State(
              ..state,
              heap: h,
              stack: gen.saved_stack,
              locals: gen.saved_locals,
              func: func_template,
              code: func_template.bytecode,
              constants: func_template.constants,
              pc: gen.saved_pc,
              call_stack: [],
              try_stack: restored_try,
              finally_stack: restored_finally,
              this_binding: gen.saved_this,
              callee_ref: None,
            )
          // Process through any enclosing finally blocks, then complete.
          process_generator_return(
            gen_exec_state,
            state,
            gen,
            return_val,
            rest_stack,
          )
        }
      }
    Error(_) -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "not a generator object",
        )
      Error(#(Thrown, err, h))
    }
  }
}

/// Generator.prototype.throw(exception) — throw into the generator.
fn call_native_generator_throw(
  state: State,
  this: JsValue,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  let throw_val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case get_generator_data(state.heap, this) {
    Ok(gen) ->
      case gen.gen_state {
        value.Completed | value.SuspendedStart -> {
          // Mark completed and throw the exception
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Completed),
            )
          Error(#(Thrown, throw_val, h))
        }
        value.Executing -> {
          let #(h, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Generator is already running",
            )
          Error(#(Thrown, err, h))
        }
        value.SuspendedYield -> {
          // Mark as executing
          let h =
            heap.write(
              state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Executing),
            )
          // Restore the generator's execution state
          let #(restored_try, restored_finally) =
            restore_stacks(gen.saved_try_stack, gen.saved_finally_stack)
          let assert Ok(func_template) =
            dict.get(state.closure_templates, gen.func_template_id)
          let gen_exec_state =
            State(
              ..state,
              heap: h,
              stack: gen.saved_stack,
              locals: gen.saved_locals,
              func: func_template,
              code: func_template.bytecode,
              constants: func_template.constants,
              pc: gen.saved_pc,
              call_stack: [],
              try_stack: restored_try,
              finally_stack: restored_finally,
              this_binding: gen.saved_this,
              callee_ref: None,
            )
          // Try to unwind to a catch handler within the generator
          case unwind_to_catch(gen_exec_state, throw_val) {
            Ok(caught_state) ->
              // The generator caught it — continue executing
              case execute_inner(caught_state) {
                Ok(#(YieldCompletion(yielded_value, h2), suspended)) -> {
                  let #(saved_try2, saved_finally2) =
                    save_stacks(suspended.try_stack, suspended.finally_stack)
                  let h3 =
                    heap.write(
                      h2,
                      gen.data_ref,
                      GeneratorSlot(
                        gen_state: value.SuspendedYield,
                        func_template_id: gen.func_template_id,
                        env_ref: gen.env_ref,
                        saved_pc: suspended.pc,
                        saved_locals: suspended.locals,
                        saved_stack: suspended.stack,
                        saved_try_stack: saved_try2,
                        saved_finally_stack: saved_finally2,
                        saved_this: suspended.this_binding,
                      ),
                    )
                  let #(h3, result) =
                    create_iterator_result(
                      h3,
                      state.builtins,
                      yielded_value,
                      False,
                    )
                  Ok(
                    State(
                      ..state,
                      heap: h3,
                      stack: [result, ..rest_stack],
                      pc: state.pc + 1,
                      closure_templates: suspended.closure_templates,
                      globals: suspended.globals,
                      job_queue: suspended.job_queue,
                    ),
                  )
                }
                Ok(#(NormalCompletion(return_value, h2), final_state)) -> {
                  let h3 =
                    heap.write(
                      h2,
                      gen.data_ref,
                      gen_with_state(gen, value.Completed),
                    )
                  let #(h3, result) =
                    create_iterator_result(
                      h3,
                      state.builtins,
                      return_value,
                      True,
                    )
                  Ok(
                    State(
                      ..state,
                      heap: h3,
                      stack: [result, ..rest_stack],
                      pc: state.pc + 1,
                      closure_templates: final_state.closure_templates,
                      globals: final_state.globals,
                      job_queue: final_state.job_queue,
                    ),
                  )
                }
                Ok(#(ThrowCompletion(thrown, h2), _final_state)) -> {
                  let h3 =
                    heap.write(
                      h2,
                      gen.data_ref,
                      gen_with_state(gen, value.Completed),
                    )
                  Error(#(Thrown, thrown, h3))
                }
                Error(_vm_err) -> {
                  let h2 =
                    heap.write(
                      state.heap,
                      gen.data_ref,
                      gen_with_state(gen, value.Completed),
                    )
                  Error(#(
                    VmError(Unimplemented("generator throw execution failed")),
                    JsUndefined,
                    h2,
                  ))
                }
              }
            Error(_) -> {
              // No catch handler — mark completed and propagate the throw
              let h2 =
                heap.write(
                  h,
                  gen.data_ref,
                  gen_with_state(gen, value.Completed),
                )
              Error(#(Thrown, throw_val, h2))
            }
          }
        }
      }
    Error(_) -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "not a generator object",
        )
      Error(#(Thrown, err, h))
    }
  }
}

/// Extract the GeneratorSlot from a generator `this` value.
/// Extracted generator data — avoids Gleam's "don't know type of variant field" issue.
type GenData {
  GenData(
    data_ref: Ref,
    gen_state: value.GeneratorState,
    func_template_id: Int,
    env_ref: Ref,
    saved_pc: Int,
    saved_locals: array.Array(JsValue),
    saved_stack: List(JsValue),
    saved_try_stack: List(value.SavedTryFrame),
    saved_finally_stack: List(value.SavedFinallyCompletion),
    saved_this: JsValue,
  )
}

fn get_generator_data(h: Heap, this: JsValue) -> Result(GenData, Nil) {
  case this {
    JsObject(obj_ref) ->
      case heap.read(h, obj_ref) {
        Ok(ObjectSlot(kind: GeneratorObject(generator_data: data_ref), ..)) ->
          case heap.read(h, data_ref) {
            Ok(GeneratorSlot(
              gen_state:,
              func_template_id:,
              env_ref:,
              saved_pc:,
              saved_locals:,
              saved_stack:,
              saved_try_stack:,
              saved_finally_stack:,
              saved_this:,
            )) ->
              Ok(GenData(
                data_ref:,
                gen_state:,
                func_template_id:,
                env_ref:,
                saved_pc:,
                saved_locals:,
                saved_stack:,
                saved_try_stack:,
                saved_finally_stack:,
                saved_this:,
              ))
            _ -> Error(Nil)
          }
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// Create a GeneratorSlot with only the gen_state changed.
fn gen_with_state(
  gen: GenData,
  new_state: value.GeneratorState,
) -> value.HeapSlot {
  GeneratorSlot(
    gen_state: new_state,
    func_template_id: gen.func_template_id,
    env_ref: gen.env_ref,
    saved_pc: gen.saved_pc,
    saved_locals: gen.saved_locals,
    saved_stack: gen.saved_stack,
    saved_try_stack: gen.saved_try_stack,
    saved_finally_stack: gen.saved_finally_stack,
    saved_this: gen.saved_this,
  )
}

/// Walk the try_stack, skipping catch-only entries, looking for the first
/// try/finally handler (identified by EnterFinallyThrow at catch_target).
/// Returns Some(#(catch_target, stack_depth, remaining_try_stack)) or None.
fn find_next_finally(
  code: array.Array(Op),
  try_stack: List(TryFrame),
) -> Result(#(Int, Int, List(TryFrame)), Nil) {
  case try_stack {
    [] -> Error(Nil)
    [TryFrame(catch_target:, stack_depth:), ..rest] ->
      case array.get(catch_target, code) {
        Some(EnterFinallyThrow) -> Ok(#(catch_target, stack_depth, rest))
        _ -> find_next_finally(code, rest)
      }
  }
}

/// Process generator.return(val) by unwinding through any enclosing finally blocks.
/// This runs each finally block in order (innermost to outermost) and handles:
/// - Normal completion: continue to next finally, or mark completed
/// - Yield inside finally: save generator state, return {value, done: false}
/// - Throw inside finally: mark completed, propagate the throw
fn process_generator_return(
  gen_state: State,
  outer_state: State,
  gen: GenData,
  return_val: JsValue,
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case find_next_finally(gen_state.code, gen_state.try_stack) {
    Error(_) -> {
      // No more finally blocks. Mark completed and return {value, done: true}.
      let h =
        heap.write(
          gen_state.heap,
          gen.data_ref,
          gen_with_state(gen, value.Completed),
        )
      let #(h, result) =
        create_iterator_result(h, outer_state.builtins, return_val, True)
      Ok(
        State(
          ..outer_state,
          heap: h,
          stack: [result, ..rest_stack],
          pc: outer_state.pc + 1,
          closure_templates: gen_state.closure_templates,
          globals: gen_state.globals,
          job_queue: gen_state.job_queue,
        ),
      )
    }
    Ok(#(catch_target, stack_depth, remaining_try)) -> {
      // Found a finally handler. Set up state to execute the finally body.
      // Skip EnterFinallyThrow (at catch_target), jump to catch_target + 1.
      // Push ReturnCompletion onto finally_stack so LeaveFinally knows to return.
      let restored_stack = truncate_stack(gen_state.stack, stack_depth)
      let finally_state =
        State(
          ..gen_state,
          try_stack: remaining_try,
          stack: restored_stack,
          finally_stack: [
            frame.ReturnCompletion(return_val),
            ..gen_state.finally_stack
          ],
          pc: catch_target + 1,
        )
      case execute_inner(finally_state) {
        Ok(#(NormalCompletion(_val, h2), final_state)) -> {
          // Finally completed normally. LeaveFinally saw ReturnCompletion → Done.
          // Continue processing any remaining outer finally blocks.
          let updated_gen_state =
            State(
              ..gen_state,
              heap: h2,
              try_stack: final_state.try_stack,
              finally_stack: final_state.finally_stack,
              stack: final_state.stack,
              locals: final_state.locals,
              closure_templates: final_state.closure_templates,
              globals: final_state.globals,
              job_queue: final_state.job_queue,
            )
          process_generator_return(
            updated_gen_state,
            outer_state,
            gen,
            return_val,
            rest_stack,
          )
        }
        Ok(#(YieldCompletion(yielded_value, h2), suspended)) -> {
          // Generator yielded from inside the finally block.
          // Save state so next .next() resumes inside the finally.
          let #(saved_try2, saved_finally2) =
            save_stacks(suspended.try_stack, suspended.finally_stack)
          let h3 =
            heap.write(
              h2,
              gen.data_ref,
              GeneratorSlot(
                gen_state: value.SuspendedYield,
                func_template_id: gen.func_template_id,
                env_ref: gen.env_ref,
                saved_pc: suspended.pc,
                saved_locals: suspended.locals,
                saved_stack: suspended.stack,
                saved_try_stack: saved_try2,
                saved_finally_stack: saved_finally2,
                saved_this: suspended.this_binding,
              ),
            )
          let #(h3, result) =
            create_iterator_result(
              h3,
              outer_state.builtins,
              yielded_value,
              False,
            )
          Ok(
            State(
              ..outer_state,
              heap: h3,
              stack: [result, ..rest_stack],
              pc: outer_state.pc + 1,
              closure_templates: suspended.closure_templates,
              globals: suspended.globals,
              job_queue: suspended.job_queue,
            ),
          )
        }
        Ok(#(ThrowCompletion(thrown, h2), _suspended)) -> {
          // Finally block threw. Mark completed and propagate the throw.
          let h3 =
            heap.write(h2, gen.data_ref, gen_with_state(gen, value.Completed))
          Error(#(Thrown, thrown, h3))
        }
        Error(vm_err) -> {
          let h2 =
            heap.write(
              gen_state.heap,
              gen.data_ref,
              gen_with_state(gen, value.Completed),
            )
          Error(#(VmError(vm_err), JsUndefined, h2))
        }
      }
    }
  }
}

/// Create a {value: val, done: bool} iterator result object.
fn create_iterator_result(
  h: Heap,
  builtins: Builtins,
  val: JsValue,
  done: Bool,
) -> #(Heap, JsValue) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list([
          #("value", value.data_property(val)),
          #("done", value.data_property(JsBool(done))),
        ]),
        elements: js_elements.new(),
        prototype: Some(builtins.object.prototype),
        symbol_properties: dict.new(),
      ),
    )
  #(h, JsObject(ref))
}

// ============================================================================
// Promise job queue draining
// ============================================================================

/// Drain all jobs in the job queue, processing any new jobs that get enqueued
/// during execution. Loops until the queue is empty.
fn drain_jobs(state: State) -> State {
  case state.job_queue {
    [] -> state
    [job, ..rest] -> {
      let state = State(..state, job_queue: rest)
      let state = execute_job(state, job)
      drain_jobs(state)
    }
  }
}

/// Execute a single job from the promise job queue.
fn execute_job(state: State, job: value.Job) -> State {
  case job {
    value.PromiseReactionJob(handler:, arg:, resolve:, reject:) ->
      execute_reaction_job(state, handler, arg, resolve, reject)
    value.PromiseResolveThenableJob(thenable:, then_fn:, resolve:, reject:) ->
      execute_thenable_job(state, thenable, then_fn, resolve, reject)
  }
}

/// Execute a promise reaction job:
/// - If handler is undefined/not callable: pass-through (resolve/reject with arg)
/// - If handler is callable: call handler(arg), resolve child with result,
///   or reject child if handler throws
fn execute_reaction_job(
  state: State,
  handler: JsValue,
  arg: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> State {
  case is_callable_value(state.heap, handler) {
    False -> {
      // JsUndefined = fulfill pass-through, JsNull = reject pass-through
      let target = case handler {
        JsNull -> reject
        _ -> resolve
      }
      call_native_for_job(state, target, [arg])
    }
    True -> {
      // Call handler(arg)
      let result = run_handler(state, handler, arg)
      case result {
        Ok(#(return_val, new_state)) ->
          // Resolve child with handler's return value
          call_native_for_job(new_state, resolve, [return_val])
        Error(#(thrown, new_state)) ->
          // Handler threw — reject child
          call_native_for_job(new_state, reject, [thrown])
      }
    }
  }
}

/// Execute a thenable job: call thenable.then(resolve, reject)
fn execute_thenable_job(
  state: State,
  thenable: JsValue,
  then_fn: JsValue,
  resolve: JsValue,
  reject: JsValue,
) -> State {
  let result =
    run_handler_with_this(state, then_fn, thenable, [resolve, reject])
  case result {
    Ok(#(_return_val, new_state)) -> new_state
    Error(#(thrown, new_state)) ->
      // then() threw — reject the promise
      call_native_for_job(new_state, reject, [thrown])
  }
}

/// Run a JS handler function with a single argument, returning the result.
/// Creates a minimal call frame to execute the handler.
fn run_handler(
  state: State,
  handler: JsValue,
  arg: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  run_handler_with_this(state, handler, JsUndefined, [arg])
}

/// Run a JS handler function with a this value and args.
/// Returns Ok(return_value, state) on success, Error(thrown, state) on throw.
fn run_handler_with_this(
  state: State,
  handler: JsValue,
  this_val: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case handler {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Ok(ObjectSlot(kind: FunctionObject(func_index: _, env: env_ref), ..)) ->
          run_closure_for_job(state, ref, env_ref, args, this_val)
        Ok(ObjectSlot(kind: NativeFunction(native), ..)) -> {
          // For native functions (like resolve/reject), call directly
          let job_state =
            State(
              ..state,
              stack: [],
              pc: 0,
              code: array.from_list([]),
              call_stack: [],
              try_stack: [],
            )
          case call_native(job_state, native, args, [], this_val) {
            Ok(new_state) ->
              case new_state.stack {
                [result, ..] ->
                  Ok(#(
                    result,
                    State(
                      ..state,
                      heap: new_state.heap,
                      job_queue: new_state.job_queue,
                    ),
                  ))
                [] ->
                  Ok(#(
                    JsUndefined,
                    State(
                      ..state,
                      heap: new_state.heap,
                      job_queue: new_state.job_queue,
                    ),
                  ))
              }
            Error(#(Thrown, thrown, h)) ->
              Error(#(thrown, State(..state, heap: h)))
            Error(#(VmError(vm_err), _, _heap)) ->
              panic as {
                "VM error in native call during job: " <> string.inspect(vm_err)
              }
            Error(#(_, _, h)) -> Error(#(JsUndefined, State(..state, heap: h)))
          }
        }
        _ -> Ok(#(JsUndefined, state))
      }
    _ -> Ok(#(JsUndefined, state))
  }
}

/// Run a JS closure for a job. Sets up a temporary execution context.
fn run_closure_for_job(
  state: State,
  fn_ref: value.Ref,
  env_ref: value.Ref,
  args: List(JsValue),
  this_val: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case dict.get(state.closure_templates, fn_ref.id) {
    Ok(callee_template) -> {
      let env_values = case heap.read(state.heap, env_ref) {
        Ok(value.EnvSlot(slots)) -> slots
        _ -> []
      }
      let env_count = list.length(env_values)
      let padded_args = pad_args(args, callee_template.arity)
      let remaining =
        callee_template.local_count - env_count - callee_template.arity
      let locals =
        list.flatten([
          env_values,
          padded_args,
          list.repeat(JsUndefined, remaining),
        ])
        |> array.from_list
      let new_this = case callee_template.is_arrow {
        True -> state.this_binding
        False -> this_val
      }
      let job_state =
        State(
          ..state,
          stack: [],
          locals:,
          constants: callee_template.constants,
          func: callee_template,
          code: callee_template.bytecode,
          pc: 0,
          call_stack: [],
          try_stack: [],
          finally_stack: [],
          this_binding: new_this,
          callee_ref: Some(fn_ref),
        )
      case execute_inner(job_state) {
        Ok(#(NormalCompletion(val, h), final_state)) ->
          Ok(#(
            val,
            State(
              ..state,
              heap: h,
              job_queue: final_state.job_queue,
              closure_templates: final_state.closure_templates,
              globals: final_state.globals,
            ),
          ))
        Ok(#(ThrowCompletion(thrown, h), final_state)) ->
          Error(#(
            thrown,
            State(
              ..state,
              heap: h,
              job_queue: final_state.job_queue,
              closure_templates: final_state.closure_templates,
              globals: final_state.globals,
            ),
          ))
        Ok(#(YieldCompletion(_, _), _)) ->
          panic as "YieldCompletion should not appear in job execution"
        Error(vm_err) ->
          panic as { "VM error in promise job: " <> string.inspect(vm_err) }
      }
    }
    Error(_) -> Ok(#(JsUndefined, state))
  }
}

/// Helper: Call a native function during job execution (fire-and-forget style).
/// Used for calling resolve/reject on child promises after a handler runs.
fn call_native_for_job(
  state: State,
  target: JsValue,
  args: List(JsValue),
) -> State {
  case run_handler_with_this(state, target, JsUndefined, args) {
    Ok(#(_, new_state)) -> new_state
    Error(#(_, new_state)) -> new_state
  }
}

/// Get the Ref of a named property's JsObject value from a heap object.
/// Returns Error(Nil) if the object doesn't exist, the property is missing,
/// or the property value is not a JsObject.
fn get_field_ref(
  h: Heap,
  obj_ref: value.Ref,
  name: String,
) -> Result(value.Ref, Nil) {
  use slot <- result.try(heap.read(h, obj_ref))
  case slot {
    ObjectSlot(properties: props, ..) ->
      case dict.get(props, name) {
        Ok(DataProperty(value: JsObject(ref), ..)) -> Ok(ref)
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// Update the prototype of a heap object in-place, returning the new heap.
/// If the ref doesn't point to an ObjectSlot, returns the heap unchanged.
fn set_slot_prototype(
  h: Heap,
  ref: value.Ref,
  new_proto: option.Option(value.Ref),
) -> Heap {
  case heap.read(h, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, symbol_properties:, ..)) ->
      heap.write(
        h,
        ref,
        ObjectSlot(
          kind:,
          properties:,
          elements:,
          prototype: new_proto,
          symbol_properties:,
        ),
      )
    _ -> h
  }
}

/// Pop n items from stack. Returns #(popped_items_in_order, remaining_stack).
fn pop_n(
  stack: List(JsValue),
  n: Int,
) -> Result(#(List(JsValue), List(JsValue)), Nil) {
  pop_n_loop(stack, n, [])
}

fn pop_n_loop(
  stack: List(JsValue),
  remaining: Int,
  acc: List(JsValue),
) -> Result(#(List(JsValue), List(JsValue)), Nil) {
  case remaining {
    0 -> Ok(#(acc, stack))
    _ ->
      case stack {
        [top, ..rest] -> pop_n_loop(rest, remaining - 1, [top, ..acc])
        [] -> Error(Nil)
      }
  }
}

/// Pad args to exactly `arity` length — truncate extras, fill missing with undefined.
fn pad_args(args: List(JsValue), arity: Int) -> List(JsValue) {
  let len = list.length(args)
  case len >= arity {
    True -> list.take(args, arity)
    False -> list.append(args, list.repeat(JsUndefined, arity - len))
  }
}

// ============================================================================
// Primitive property access helpers
// ============================================================================

/// Look up a named property on a string primitive.
/// Handles "length" directly, delegates the rest to String.prototype.
fn get_string_field(
  s: String,
  name: String,
  heap: Heap,
  string_proto: Ref,
) -> JsValue {
  case name {
    "length" -> JsNumber(Finite(int.to_float(string.length(s))))
    _ ->
      case object.get_property(heap, string_proto, name) {
        Ok(v) -> v
        Error(_) -> JsUndefined
      }
  }
}

/// Look up a computed property on a string primitive.
/// Numeric index → character access, "length" → length, else String.prototype.
fn get_string_elem(
  s: String,
  key: JsValue,
  heap: Heap,
  string_proto: Ref,
) -> JsValue {
  case to_array_index(key) {
    Ok(idx) -> {
      let len = string.length(s)
      case idx >= 0 && idx < len {
        True -> JsString(string.slice(s, idx, 1))
        False -> JsUndefined
      }
    }
    Error(_) ->
      case key {
        JsString(name) -> get_string_field(s, name, heap, string_proto)
        _ -> JsUndefined
      }
  }
}

// ============================================================================
// Computed property access helpers
// ============================================================================

/// Read a property from a unified object using a JsValue key.
/// Dispatches on ExoticKind: arrays use elements dict, others use properties.
/// Returns Result to handle thrown exceptions from js_to_string (ToPrimitive).
fn get_elem_value(
  state: State,
  ref: value.Ref,
  key: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  // Symbol keys use the separate symbol_properties dict
  case key {
    value.JsSymbol(sym_id) -> {
      let val =
        object.get_symbol_property(state.heap, ref, sym_id)
        |> result.unwrap(JsUndefined)
      Ok(#(val, state))
    }
    _ ->
      case heap.read(state.heap, ref) {
        Ok(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) ->
          case kind {
            ArrayObject(length:) ->
              case to_array_index(key) {
                Ok(idx) -> Ok(#(js_elements.get(elements, idx), state))
                Error(_) ->
                  // Non-numeric key on array — check "length", then properties, then prototype
                  case key {
                    JsString("length") ->
                      Ok(#(JsNumber(Finite(int.to_float(length))), state))
                    _ ->
                      case js_to_string(state, key) {
                        Ok(#(key_str, state)) ->
                          case dict.get(properties, key_str) {
                            Ok(DataProperty(value: val, ..)) ->
                              Ok(#(val, state))
                            Error(_) ->
                              case prototype {
                                Some(proto_ref) ->
                                  get_elem_value(state, proto_ref, key)
                                None -> Ok(#(JsUndefined, state))
                              }
                          }
                        Error(#(thrown, state)) -> Error(#(thrown, state))
                      }
                  }
              }
            OrdinaryObject
            | FunctionObject(..)
            | NativeFunction(_)
            | value.PromiseObject(_)
            | value.GeneratorObject(_) ->
              case js_to_string(state, key) {
                Ok(#(key_str, state)) ->
                  case object.get_property(state.heap, ref, key_str) {
                    Ok(val) -> Ok(#(val, state))
                    Error(_) -> Ok(#(JsUndefined, state))
                  }
                Error(#(thrown, state)) -> Error(#(thrown, state))
              }
          }
        _ -> Ok(#(JsUndefined, state))
      }
  }
}

/// Write a property to a unified object using a JsValue key.
/// Returns Result to handle thrown exceptions from js_to_string (ToPrimitive).
fn put_elem_value(
  state: State,
  ref: value.Ref,
  key: JsValue,
  val: JsValue,
) -> Result(State, #(JsValue, State)) {
  // Symbol keys use the separate symbol_properties dict
  case key {
    value.JsSymbol(sym_id) -> {
      let new_heap = object.set_symbol_property(state.heap, ref, sym_id, val)
      Ok(State(..state, heap: new_heap))
    }
    _ ->
      case heap.read(state.heap, ref) {
        Ok(ObjectSlot(
          kind:,
          properties:,
          elements:,
          prototype:,
          symbol_properties:,
        )) ->
          case kind {
            ArrayObject(length:) ->
              case to_array_index(key) {
                Ok(idx) -> {
                  let new_elements = js_elements.set(elements, idx, val)
                  let new_length = case idx >= length {
                    True -> idx + 1
                    False -> length
                  }
                  let new_heap =
                    heap.write(
                      state.heap,
                      ref,
                      ObjectSlot(
                        kind: ArrayObject(new_length),
                        properties:,
                        elements: new_elements,
                        prototype:,
                        symbol_properties:,
                      ),
                    )
                  Ok(State(..state, heap: new_heap))
                }
                Error(_) ->
                  case js_to_string(state, key) {
                    Ok(#(key_str, state)) -> {
                      let new_heap =
                        object.set_property(state.heap, ref, key_str, val)
                      Ok(State(..state, heap: new_heap))
                    }
                    Error(#(thrown, state)) -> Error(#(thrown, state))
                  }
              }
            OrdinaryObject
            | FunctionObject(..)
            | NativeFunction(_)
            | value.PromiseObject(_)
            | value.GeneratorObject(_) ->
              case js_to_string(state, key) {
                Ok(#(key_str, state)) -> {
                  let new_heap =
                    object.set_property(state.heap, ref, key_str, val)
                  Ok(State(..state, heap: new_heap))
                }
                Error(#(thrown, state)) -> Error(#(thrown, state))
              }
          }
        _ -> Ok(state)
      }
  }
}

/// Try to convert a JsValue to an array index (non-negative integer).
fn to_array_index(key: JsValue) -> Result(Int, Nil) {
  case key {
    JsNumber(Finite(n)) -> {
      let i = float.truncate(n)
      case int.to_float(i) == n && i >= 0 {
        True -> Ok(i)
        False -> Error(Nil)
      }
    }
    JsString(s) ->
      case int.parse(s) {
        Ok(i) if i >= 0 -> Ok(i)
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

// ============================================================================
// JS type coercion and operators
// ============================================================================

/// JS typeof — needs heap access to distinguish "function" from "object".
fn typeof_value(val: JsValue, heap: Heap) -> String {
  case val {
    JsUndefined | JsUninitialized -> "undefined"
    JsNull -> "object"
    JsBool(_) -> "boolean"
    JsNumber(_) -> "number"
    JsString(_) -> "string"
    JsBigInt(_) -> "bigint"
    JsSymbol(_) -> "symbol"
    JsObject(ref) ->
      case heap.read(heap, ref) {
        Ok(ObjectSlot(kind: FunctionObject(..), ..)) -> "function"
        Ok(ObjectSlot(kind: NativeFunction(..), ..)) -> "function"
        _ -> "object"
      }
  }
}

/// Execute a binary operation on two JsValues.
fn exec_binop(
  kind: BinOpKind,
  left: JsValue,
  right: JsValue,
) -> Result(JsValue, String) {
  case kind {
    // Add is handled directly in the BinOp dispatcher with ToPrimitive
    Add -> panic as "Add should be handled in BinOp dispatcher"
    Sub -> num_binop(left, right, num_sub)
    Mul -> num_binop(left, right, num_mul)
    Div -> num_binop(left, right, num_div)
    Mod -> num_binop(left, right, num_mod)
    Exp -> num_binop(left, right, num_exp)

    // Bitwise — convert to i32, operate, convert back
    BitAnd -> bitwise_binop(left, right, int.bitwise_and)
    BitOr -> bitwise_binop(left, right, int.bitwise_or)
    BitXor -> bitwise_binop(left, right, int.bitwise_exclusive_or)
    ShiftLeft -> {
      use a, b <- bitwise_binop(left, right)
      int.bitwise_shift_left(a, int.bitwise_and(b, 31))
    }
    ShiftRight -> {
      use a, b <- bitwise_binop(left, right)
      int.bitwise_shift_right(a, int.bitwise_and(b, 31))
    }
    UShiftRight -> {
      use a, b <- bitwise_binop(left, right)
      int.bitwise_shift_right(
        int.bitwise_and(a, 0xFFFFFFFF),
        int.bitwise_and(b, 31),
      )
    }

    // Comparison
    StrictEq -> Ok(JsBool(strict_equal(left, right)))
    StrictNotEq -> Ok(JsBool(!strict_equal(left, right)))
    Eq -> Ok(JsBool(abstract_equal(left, right)))
    NotEq -> Ok(JsBool(!abstract_equal(left, right)))

    Lt -> {
      use ord <- compare_values(left, right)
      ord == LtOrd
    }
    LtEq -> {
      use ord <- compare_values(left, right)
      ord == LtOrd || ord == EqOrd
    }
    Gt -> {
      use ord <- compare_values(left, right)
      ord == GtOrd
    }
    GtEq -> {
      use ord <- compare_values(left, right)
      ord == GtOrd || ord == EqOrd
    }

    // In and InstanceOf handled in BinOp dispatcher (needs heap access)
    opcode.In -> Error("in: unreachable — handled in dispatcher")
    opcode.InstanceOf ->
      Error("instanceof: unreachable — handled in dispatcher")
  }
}

/// Execute a unary operation.
fn exec_unaryop(kind: UnaryOpKind, operand: JsValue) -> Result(JsValue, String) {
  case kind {
    Neg -> {
      use n <- result.map(to_number(operand))
      JsNumber(num_negate(n))
    }
    Pos -> {
      use n <- result.map(to_number(operand))
      JsNumber(n)
    }
    BitNot -> {
      use n <- result.map(to_number(operand))
      JsNumber(Finite(int.to_float(int.bitwise_not(num_to_int32(n)))))
    }
    LogicalNot -> Ok(JsBool(!value.is_truthy(operand)))
    Void -> Ok(JsUndefined)
  }
}

// ============================================================================
// JsNum arithmetic — IEEE 754 semantics without BEAM floats for special values
// ============================================================================

fn num_add(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, NegInfinity | NegInfinity, Infinity -> NaN
    Infinity, _ | _, Infinity -> Infinity
    NegInfinity, _ | _, NegInfinity -> NegInfinity
    Finite(x), Finite(y) -> Finite(x +. y)
  }
}

fn num_sub(a: JsNum, b: JsNum) -> JsNum {
  num_add(a, num_negate(b))
}

fn num_mul(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, Finite(0.0) | Finite(0.0), Infinity -> NaN
    NegInfinity, Finite(0.0) | Finite(0.0), NegInfinity -> NaN
    Infinity, Finite(x) | Finite(x), Infinity ->
      case x >. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    NegInfinity, Finite(x) | Finite(x), NegInfinity ->
      case x >. 0.0 {
        True -> NegInfinity
        False -> Infinity
      }
    Infinity, Infinity | NegInfinity, NegInfinity -> Infinity
    Infinity, NegInfinity | NegInfinity, Infinity -> NegInfinity
    Finite(x), Finite(y) -> Finite(x *. y)
  }
}

fn num_div(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, Infinity
    | Infinity, NegInfinity
    | NegInfinity, Infinity
    | NegInfinity, NegInfinity
    -> NaN
    Infinity, Finite(x) ->
      case x >=. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    NegInfinity, Finite(x) ->
      case x >=. 0.0 {
        True -> NegInfinity
        False -> Infinity
      }
    Finite(_), Infinity | Finite(_), NegInfinity -> Finite(0.0)
    Finite(0.0), Finite(0.0) -> NaN
    Finite(x), Finite(0.0) ->
      case x >. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    Finite(x), Finite(y) -> Finite(x /. y)
  }
}

fn num_mod(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, _ | NegInfinity, _ -> NaN
    _, Infinity | _, NegInfinity -> a
    Finite(_), Finite(0.0) -> NaN
    Finite(0.0), Finite(_) -> Finite(0.0)
    Finite(x), Finite(y) ->
      Finite(x -. int.to_float(float.truncate(x /. y)) *. y)
  }
}

fn num_exp(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    _, Finite(0.0) -> Finite(1.0)
    _, NaN -> NaN
    NaN, _ -> NaN
    Finite(x), Finite(y) -> Finite(float_power(x, y))
    Infinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    NegInfinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    _, Infinity -> NaN
    _, NegInfinity -> NaN
  }
}

fn num_negate(n: JsNum) -> JsNum {
  case n {
    Finite(x) -> Finite(float.negate(x))
    NaN -> NaN
    Infinity -> NegInfinity
    NegInfinity -> Infinity
  }
}

/// Apply a JsNum binary operation after coercing both operands to numbers.
fn num_binop(
  left: JsValue,
  right: JsValue,
  op: fn(JsNum, JsNum) -> JsNum,
) -> Result(JsValue, String) {
  use a <- result.try(to_number(left))
  use b <- result.map(to_number(right))
  JsNumber(op(a, b))
}

/// Apply a bitwise binary operation (convert to i32, operate, convert back).
fn bitwise_binop(
  left: JsValue,
  right: JsValue,
  op: fn(Int, Int) -> Int,
) -> Result(JsValue, String) {
  use a <- result.try(to_number(left))
  use b <- result.map(to_number(right))
  JsNumber(Finite(int.to_float(op(num_to_int32(a), num_to_int32(b)))))
}

/// JS ToNumber: https://tc39.es/ecma262/#sec-tonumber
fn to_number(val: JsValue) -> Result(JsNum, String) {
  case val {
    JsNumber(n) -> Ok(n)
    JsUndefined -> Ok(NaN)
    JsNull -> Ok(Finite(0.0))
    JsBool(True) -> Ok(Finite(1.0))
    JsBool(False) -> Ok(Finite(0.0))
    JsString("") -> Ok(Finite(0.0))
    JsString(s) ->
      case float.parse(s) {
        Ok(n) -> Ok(Finite(n))
        Error(_) ->
          case int.parse(s) {
            Ok(n) -> Ok(Finite(int.to_float(n)))
            Error(_) -> Ok(NaN)
          }
      }
    JsBigInt(_) -> Error("Cannot convert BigInt to number")
    JsSymbol(_) -> Error("Cannot convert Symbol to number")
    JsObject(_) -> Ok(NaN)
    JsUninitialized -> Error("Cannot access before initialization")
  }
}

// ============================================================================
// ToPrimitive / ToString with VM re-entry (ES2024 §7.1.1, §7.1.12)
// ============================================================================

type ToPrimitiveHint {
  StringHint
  NumberHint
  DefaultHint
}

/// ES2024 §7.1.1 ToPrimitive(input, preferredType)
/// For primitives, returns as-is. For objects, calls Symbol.toPrimitive
/// or falls back to OrdinaryToPrimitive.
fn to_primitive(
  state: State,
  val: JsValue,
  hint: ToPrimitiveHint,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case val {
    // Primitives pass through
    JsUndefined
    | JsNull
    | JsBool(_)
    | JsNumber(_)
    | JsString(_)
    | JsSymbol(_)
    | JsBigInt(_)
    | JsUninitialized -> Ok(#(val, state))
    // Objects: try Symbol.toPrimitive, then OrdinaryToPrimitive
    JsObject(ref) -> {
      // §7.1.1 step 2.a: check @@toPrimitive
      case
        object.get_symbol_property(state.heap, ref, value.symbol_to_primitive)
      {
        Ok(exotic_fn) ->
          case is_callable_value(state.heap, exotic_fn) {
            True -> {
              let hint_str = case hint {
                StringHint -> "string"
                NumberHint -> "number"
                DefaultHint -> "default"
              }
              case
                run_handler_with_this(state, exotic_fn, val, [
                  JsString(hint_str),
                ])
              {
                Ok(#(result, new_state)) ->
                  case result {
                    JsObject(_) -> {
                      let #(h, err) =
                        object.make_type_error(
                          new_state.heap,
                          new_state.builtins,
                          "Cannot convert object to primitive value",
                        )
                      Error(#(err, State(..new_state, heap: h)))
                    }
                    _ -> Ok(#(result, new_state))
                  }
                Error(#(thrown, new_state)) -> Error(#(thrown, new_state))
              }
            }
            False -> {
              let #(h, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  "@@toPrimitive is not callable",
                )
              Error(#(err, State(..state, heap: h)))
            }
          }
        Error(_) -> ordinary_to_primitive(state, val, ref, hint)
      }
    }
  }
}

/// ES2024 §7.1.1.1 OrdinaryToPrimitive(O, hint)
/// Tries toString/valueOf (or valueOf/toString for number hint).
fn ordinary_to_primitive(
  state: State,
  val: JsValue,
  ref: Ref,
  hint: ToPrimitiveHint,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let method_names = case hint {
    StringHint -> ["toString", "valueOf"]
    NumberHint | DefaultHint -> ["valueOf", "toString"]
  }
  try_to_primitive_methods(state, val, ref, method_names)
}

/// Try each method name in order; return the first primitive result.
fn try_to_primitive_methods(
  state: State,
  val: JsValue,
  ref: Ref,
  method_names: List(String),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case method_names {
    [] -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "Cannot convert object to primitive value",
        )
      Error(#(err, State(..state, heap: h)))
    }
    [name, ..rest] ->
      case object.get_property(state.heap, ref, name) {
        Ok(method) ->
          case is_callable_value(state.heap, method) {
            True ->
              case run_handler_with_this(state, method, val, []) {
                Ok(#(result, new_state)) ->
                  case result {
                    JsObject(_) ->
                      try_to_primitive_methods(new_state, val, ref, rest)
                    _ -> Ok(#(result, new_state))
                  }
                Error(#(thrown, new_state)) -> Error(#(thrown, new_state))
              }
            False -> try_to_primitive_methods(state, val, ref, rest)
          }
        Error(_) -> try_to_primitive_methods(state, val, ref, rest)
      }
  }
}

/// ES2024 §7.1.12 ToString with VM re-entry for ToPrimitive.
/// For primitives, converts directly. For objects, calls ToPrimitive(string) first.
fn js_to_string(
  state: State,
  val: JsValue,
) -> Result(#(String, State), #(JsValue, State)) {
  case val {
    JsObject(_) -> {
      use #(prim, new_state) <- result.try(to_primitive(state, val, StringHint))
      js_to_string(new_state, prim)
    }
    JsSymbol(_) -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "Cannot convert a Symbol value to a string",
        )
      Error(#(err, State(..state, heap: h)))
    }
    JsString(s) -> Ok(#(s, state))
    JsNumber(Finite(n)) -> Ok(#(value.js_format_number(n), state))
    JsNumber(NaN) -> Ok(#("NaN", state))
    JsNumber(Infinity) -> Ok(#("Infinity", state))
    JsNumber(NegInfinity) -> Ok(#("-Infinity", state))
    JsBool(True) -> Ok(#("true", state))
    JsBool(False) -> Ok(#("false", state))
    JsNull -> Ok(#("null", state))
    JsUndefined -> Ok(#("undefined", state))
    JsUninitialized -> Ok(#("undefined", state))
    JsBigInt(BigInt(n)) -> Ok(#(int.to_string(n), state))
  }
}

/// BinOp Add with ToPrimitive for object operands.
/// ES2024 §13.15.3: ToPrimitive(default) both sides, then string-concat or numeric-add.
fn binop_add_with_to_primitive(
  state: State,
  left: JsValue,
  right: JsValue,
  rest: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case to_primitive(state, left, DefaultHint) {
    Ok(#(lprim, s1)) ->
      case to_primitive(s1, right, DefaultHint) {
        Ok(#(rprim, s2)) ->
          case lprim, rprim {
            JsString(a), JsString(b) ->
              Ok(
                State(..s2, stack: [JsString(a <> b), ..rest], pc: state.pc + 1),
              )
            JsString(a), _ ->
              case js_to_string(s2, rprim) {
                Ok(#(b, s3)) ->
                  Ok(
                    State(
                      ..s3,
                      stack: [JsString(a <> b), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                Error(#(thrown, s3)) -> Error(#(Thrown, thrown, s3.heap))
              }
            _, JsString(b) ->
              case js_to_string(s2, lprim) {
                Ok(#(a, s3)) ->
                  Ok(
                    State(
                      ..s3,
                      stack: [JsString(a <> b), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                Error(#(thrown, s3)) -> Error(#(Thrown, thrown, s3.heap))
              }
            _, _ -> {
              let a = to_number_for_binop(lprim)
              let b = to_number_for_binop(rprim)
              Ok(
                State(
                  ..s2,
                  stack: [JsNumber(num_add(a, b)), ..rest],
                  pc: state.pc + 1,
                ),
              )
            }
          }
        Error(#(thrown, s2)) -> Error(#(Thrown, thrown, s2.heap))
      }
    Error(#(thrown, s1)) -> Error(#(Thrown, thrown, s1.heap))
  }
}

/// Convert a primitive JsValue to JsNum for arithmetic (ToNumber lite).
fn to_number_for_binop(val: JsValue) -> JsNum {
  case to_number(val) {
    Ok(n) -> n
    Error(_) -> NaN
  }
}

/// JS instanceof operator.
/// Walks the left operand's prototype chain looking for constructor.prototype.
fn js_instanceof(
  heap: Heap,
  left: JsValue,
  constructor: JsValue,
) -> Result(Bool, String) {
  // Right side must be a callable object (function)
  case constructor {
    JsObject(ctor_ref) ->
      case heap.read(heap, ctor_ref) {
        Ok(ObjectSlot(kind: FunctionObject(..), ..)) -> {
          // Get the constructor's .prototype property
          case object.get_property(heap, ctor_ref, "prototype") {
            Ok(JsObject(proto_ref)) ->
              // Walk left's prototype chain
              case left {
                JsObject(obj_ref) -> instanceof_walk(heap, obj_ref, proto_ref)
                // Primitives are never instances
                _ -> Ok(False)
              }
            Ok(_) ->
              Error("Function has non-object prototype in instanceof check")
            Error(_) -> Ok(False)
          }
        }
        _ -> Error("Right-hand side of instanceof is not callable")
      }
    _ -> Error("Right-hand side of instanceof is not callable")
  }
}

/// Walk an object's [[Prototype]] chain looking for a target ref.
fn instanceof_walk(
  heap: Heap,
  obj_ref: Ref,
  target_proto: Ref,
) -> Result(Bool, String) {
  case heap.read(heap, obj_ref) {
    Ok(ObjectSlot(prototype: Some(proto_ref), ..)) ->
      case proto_ref.id == target_proto.id {
        True -> Ok(True)
        False -> instanceof_walk(heap, proto_ref, target_proto)
      }
    Ok(ObjectSlot(prototype: None, ..)) -> Ok(False)
    _ -> Ok(False)
  }
}

/// JS === (strict equality)
fn strict_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    JsUndefined, JsUndefined -> True
    JsNull, JsNull -> True
    JsBool(a), JsBool(b) -> a == b
    // NaN !== NaN
    JsNumber(NaN), _ | _, JsNumber(NaN) -> False
    JsNumber(a), JsNumber(b) -> a == b
    JsString(a), JsString(b) -> a == b
    JsBigInt(a), JsBigInt(b) -> a == b
    // Object identity (same Ref) — covers functions and arrays too
    JsObject(a), JsObject(b) -> a == b
    JsSymbol(a), JsSymbol(b) -> a == b
    _, _ -> False
  }
}

/// JS == (abstract equality, simplified)
fn abstract_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    // Same type — use strict equality
    JsNull, JsNull
    | JsUndefined, JsUndefined
    | JsNull, JsUndefined
    | JsUndefined, JsNull
    -> True
    JsNumber(_), JsNumber(_)
    | JsBool(_), JsBool(_)
    | JsString(_), JsString(_)
    | JsObject(_), JsObject(_)
    | JsSymbol(_), JsSymbol(_)
    | JsBigInt(_), JsBigInt(_)
    -> strict_equal(left, right)
    // Number vs String — coerce string to number
    JsNumber(_), JsString(s) ->
      case to_number(JsString(s)) {
        Ok(n) -> strict_equal(left, JsNumber(n))
        Error(_) -> False
      }
    JsString(_), JsNumber(_) -> abstract_equal(right, left)
    // Bool vs anything — coerce bool to number
    JsBool(_), _ ->
      case to_number(left) {
        Ok(n) -> abstract_equal(JsNumber(n), right)
        Error(_) -> False
      }
    _, JsBool(_) -> abstract_equal(right, left)
    _, _ -> False
  }
}

/// Comparison order for relational ops.
type CompareOrd {
  LtOrd
  EqOrd
  GtOrd
}

/// Compare two values for relational operators (<, <=, >, >=).
fn compare_values(
  left: JsValue,
  right: JsValue,
  pred: fn(CompareOrd) -> Bool,
) -> Result(JsValue, String) {
  case left, right {
    JsString(a), JsString(b) -> {
      let ord = case string.compare(a, b) {
        order.Lt -> LtOrd
        order.Eq -> EqOrd
        order.Gt -> GtOrd
      }
      Ok(JsBool(pred(ord)))
    }
    _, _ -> {
      use a <- result.try(to_number(left))
      use b <- result.try(to_number(right))
      case a, b {
        NaN, _ | _, NaN -> Ok(JsBool(False))
        _, _ -> Ok(JsBool(pred(compare_nums(a, b))))
      }
    }
  }
}

/// Compare two JsNums (neither is NaN).
fn compare_nums(a: JsNum, b: JsNum) -> CompareOrd {
  case a, b {
    Infinity, Infinity | NegInfinity, NegInfinity -> EqOrd
    Infinity, _ -> GtOrd
    _, Infinity -> LtOrd
    NegInfinity, _ -> LtOrd
    _, NegInfinity -> GtOrd
    Finite(x), Finite(y) ->
      case x == y {
        True -> EqOrd
        False ->
          case x <. y {
            True -> LtOrd
            False -> GtOrd
          }
      }
    // NaN cases handled by caller
    NaN, _ | _, NaN -> EqOrd
  }
}

// ============================================================================
// Helpers
// ============================================================================

/// Convert JsNum to int32 (JS ToInt32).
fn num_to_int32(n: JsNum) -> Int {
  case n {
    NaN | Infinity | NegInfinity -> 0
    Finite(f) -> {
      let i = float.truncate(f)
      // Wrap to 32 bits
      let wrapped = int.bitwise_and(i, 0xFFFFFFFF)
      // Sign extend if needed
      case wrapped > 0x7FFFFFFF {
        True -> wrapped - 0x100000000
        False -> wrapped
      }
    }
  }
}

// ============================================================================
// Float helpers — only power needs FFI now
// ============================================================================

@external(erlang, "arc_vm_ffi", "float_power")
fn float_power(base: Float, exp: Float) -> Float
