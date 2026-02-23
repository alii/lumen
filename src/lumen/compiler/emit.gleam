/// Phase 1: AST Emission
///
/// Walks the AST and produces a list of EmitterOps — symbolic IR instructions
/// mixed with scope markers. Variable references use string names (IrScopeGetVar),
/// jump targets use integer label IDs (IrJump). These are resolved in Phase 2 and 3.
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import lumen/ast
import lumen/vm/opcode.{
  type IrOp, IrArrayFrom, IrBinOp, IrCallConstructor, IrCallMethod,
  IrDefineField, IrDup, IrForInNext, IrForInStart, IrGetElem, IrGetElem2,
  IrGetField, IrGetField2, IrGetIterator, IrGetThis, IrIteratorNext, IrJump,
  IrJumpIfFalse, IrJumpIfNullish, IrJumpIfTrue, IrLabel, IrMakeClosure,
  IrNewObject, IrPop, IrPopTry, IrPushConst, IrPushTry, IrPutElem, IrPutField,
  IrReturn, IrScopeGetVar, IrScopePutVar, IrScopeTypeofVar, IrSwap, IrThrow,
  IrTypeOf, IrUnaryOp,
}
import lumen/vm/value.{
  type JsValue, Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined,
}

// ============================================================================
// Types
// ============================================================================

/// An instruction in the emitter output — either a real IR op or a scope marker.
pub type EmitterOp {
  /// A real IR instruction (passed to Phase 2 → Phase 3)
  Ir(IrOp)
  /// Open a new scope
  EnterScope(kind: ScopeKind)
  /// Close the current scope
  LeaveScope
  /// Declare a variable in the current scope
  DeclareVar(name: String, kind: BindingKind)
}

pub type ScopeKind {
  FunctionScope
  BlockScope
}

pub type BindingKind {
  VarBinding
  LetBinding
  ConstBinding
  ParamBinding
  CatchBinding
  CaptureBinding
}

/// A compiled child function (before Phase 2/3).
pub type CompiledChild {
  CompiledChild(
    name: Option(String),
    arity: Int,
    code: List(EmitterOp),
    constants: List(JsValue),
    constants_map: Dict(JsValue, Int),
    functions: List(CompiledChild),
    is_strict: Bool,
    is_arrow: Bool,
  )
}

/// Loop context for break/continue targets.
pub type LoopContext {
  LoopContext(break_label: Int, continue_label: Int)
}

/// The emitter state, threaded through all emit functions.
pub opaque type Emitter {
  Emitter(
    code: List(EmitterOp),
    constants_map: Dict(JsValue, Int),
    constants_list: List(JsValue),
    next_const: Int,
    next_label: Int,
    loop_stack: List(LoopContext),
    functions: List(CompiledChild),
    next_func: Int,
  )
}

/// Compile error from the emitter.
pub type EmitError {
  BreakOutsideLoop
  ContinueOutsideLoop
  Unsupported(description: String)
}

// ============================================================================
// Public API
// ============================================================================

/// Emit IR for a list of top-level statements (script body).
/// Returns the emitter ops, constants, and child functions.
pub fn emit_program(
  stmts: List(ast.Statement),
) -> Result(
  #(List(EmitterOp), List(JsValue), Dict(JsValue, Int), List(CompiledChild)),
  EmitError,
) {
  let e = new_emitter()

  // Wrap in function scope
  let e = emit_op(e, EnterScope(FunctionScope))

  // Hoisting pre-pass: collect var declarations and function declarations
  let hoisted_vars = collect_hoisted_vars(stmts)
  let #(e, hoisted_funcs) = collect_hoisted_funcs(e, stmts)

  // Emit var declarations at top
  let e =
    list.fold(hoisted_vars, e, fn(e, name) {
      emit_op(e, DeclareVar(name, VarBinding))
    })

  // Emit hoisted function declarations
  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(name, func_idx) = hf
      let e = emit_ir(e, IrMakeClosure(func_idx))
      let e = emit_ir(e, IrScopePutVar(name))
      e
    })

  // Emit body — last expression statement's value is the program result
  use e <- result.try(emit_program_body(stmts, e))

  let e = emit_op(e, LeaveScope)
  Ok(finish(e))
}

// ============================================================================
// Emitter helpers
// ============================================================================

fn new_emitter() -> Emitter {
  Emitter(
    code: [],
    constants_map: dict.new(),
    constants_list: [],
    next_const: 0,
    next_label: 0,
    loop_stack: [],
    functions: [],
    next_func: 0,
  )
}

fn emit_op(e: Emitter, op: EmitterOp) -> Emitter {
  Emitter(..e, code: [op, ..e.code])
}

fn emit_ir(e: Emitter, op: IrOp) -> Emitter {
  emit_op(e, Ir(op))
}

fn add_constant(e: Emitter, val: JsValue) -> #(Emitter, Int) {
  case dict.get(e.constants_map, val) {
    Ok(idx) -> #(e, idx)
    Error(_) -> {
      let idx = e.next_const
      let e =
        Emitter(
          ..e,
          constants_map: dict.insert(e.constants_map, val, idx),
          constants_list: [val, ..e.constants_list],
          next_const: idx + 1,
        )
      #(e, idx)
    }
  }
}

fn push_const(e: Emitter, val: JsValue) -> Emitter {
  let #(e, idx) = add_constant(e, val)
  emit_ir(e, IrPushConst(idx))
}

fn fresh_label(e: Emitter) -> #(Emitter, Int) {
  let label = e.next_label
  #(Emitter(..e, next_label: label + 1), label)
}

fn push_loop(e: Emitter, break_label: Int, continue_label: Int) -> Emitter {
  Emitter(..e, loop_stack: [
    LoopContext(break_label:, continue_label:),
    ..e.loop_stack
  ])
}

fn pop_loop(e: Emitter) -> Emitter {
  case e.loop_stack {
    [_, ..rest] -> Emitter(..e, loop_stack: rest)
    [] -> e
  }
}

fn add_child_function(e: Emitter, child: CompiledChild) -> #(Emitter, Int) {
  let idx = e.next_func
  #(
    Emitter(
      ..e,
      functions: list.append(e.functions, [child]),
      next_func: idx + 1,
    ),
    idx,
  )
}

/// Extract final results from the emitter.
fn finish(
  e: Emitter,
) -> #(List(EmitterOp), List(JsValue), Dict(JsValue, Int), List(CompiledChild)) {
  #(
    list.reverse(e.code),
    list.reverse(e.constants_list),
    e.constants_map,
    e.functions,
  )
}

/// Emit program body: all statements, but the last statement is emitted in
/// "tail" position — its completion value stays on the stack as the program result.
fn emit_program_body(
  stmts: List(ast.Statement),
  e: Emitter,
) -> Result(Emitter, EmitError) {
  case stmts {
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_stmt_tail(e, only)
    [first, ..rest] -> {
      use e <- result.try(emit_stmt(e, first))
      emit_program_body(rest, e)
    }
  }
}

/// Emit a statement in "tail" position — its completion value stays on stack.
/// For expression statements, this means NOT emitting IrPop.
/// For compound statements (blocks, if/else, try/catch), propagates tail into
/// the inner last statement.
fn emit_stmt_tail(e: Emitter, stmt: ast.Statement) -> Result(Emitter, EmitError) {
  case stmt {
    ast.ExpressionStatement(expr) ->
      // Tail position: keep value on stack (no IrPop)
      emit_expr(e, expr)

    ast.BlockStatement(body) -> {
      let e = emit_op(e, EnterScope(BlockScope))
      use e <- result.map(emit_stmts_tail(e, body))
      emit_op(e, LeaveScope)
    }

    ast.IfStatement(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_stmt_tail(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      let e = case alternate {
        Some(alt) -> {
          case emit_stmt_tail(e, alt) {
            Ok(e) -> e
            Error(_) -> e
          }
        }
        None -> push_const(e, JsUndefined)
      }
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    ast.TryStatement(block, handler, _finalizer) -> {
      case handler {
        Some(ast.CatchClause(param, catch_body)) -> {
          let #(e, catch_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          let e = emit_ir(e, IrPushTry(catch_label, -1))
          use e <- result.try(emit_stmt_tail(e, block))
          let e = emit_ir(e, IrPopTry)
          let e = emit_ir(e, IrJump(end_label))

          let e = emit_ir(e, IrLabel(catch_label))
          let e = emit_op(e, EnterScope(BlockScope))

          let e = case param {
            Some(pattern) -> {
              case emit_destructuring_bind(e, pattern, CatchBinding) {
                Ok(e) -> e
                Error(_) -> e
              }
            }
            None -> emit_ir(e, IrPop)
          }

          use e <- result.try(emit_stmt_tail(e, catch_body))
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }
        None -> emit_stmt_tail(e, block)
      }
    }

    // All other statements: delegate to regular emit_stmt, then push undefined
    // as the completion value
    _ -> {
      use e <- result.map(emit_stmt(e, stmt))
      push_const(e, JsUndefined)
    }
  }
}

/// Like emit_stmts but the last statement is emitted in tail position.
fn emit_stmts_tail(
  e: Emitter,
  stmts: List(ast.Statement),
) -> Result(Emitter, EmitError) {
  case stmts {
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_stmt_tail(e, only)
    [first, ..rest] -> {
      use e <- result.try(emit_stmt(e, first))
      emit_stmts_tail(e, rest)
    }
  }
}

// ============================================================================
// Hoisting
// ============================================================================

/// Collect all var-declared names in a function body (not entering nested functions).
fn collect_hoisted_vars(stmts: List(ast.Statement)) -> List(String) {
  list.flat_map(stmts, collect_vars_stmt)
  |> list.unique()
}

/// Recursively extract all bound variable names from a pattern.
fn collect_pattern_names(pattern: ast.Pattern) -> List(String) {
  case pattern {
    ast.IdentifierPattern(name) -> [name]
    ast.ArrayPattern(elements) ->
      list.flat_map(elements, fn(elem) {
        case elem {
          Some(p) -> collect_pattern_names(p)
          None -> []
        }
      })
    ast.ObjectPattern(properties) ->
      list.flat_map(properties, fn(prop) {
        case prop {
          ast.PatternProperty(_, value:, ..) -> collect_pattern_names(value)
          ast.RestProperty(argument) -> collect_pattern_names(argument)
        }
      })
    ast.AssignmentPattern(left, _) -> collect_pattern_names(left)
    ast.RestElement(argument) -> collect_pattern_names(argument)
  }
}

fn collect_vars_stmt(stmt: ast.Statement) -> List(String) {
  case stmt {
    ast.VariableDeclaration(ast.Var, declarators) ->
      list.flat_map(declarators, fn(d) {
        case d {
          ast.VariableDeclarator(pattern, _) -> collect_pattern_names(pattern)
        }
      })
    ast.BlockStatement(body) -> list.flat_map(body, collect_vars_stmt)
    ast.IfStatement(_, consequent, alternate) -> {
      let then_vars = collect_vars_stmt(consequent)
      let else_vars = case alternate {
        Some(alt) -> collect_vars_stmt(alt)
        None -> []
      }
      list.append(then_vars, else_vars)
    }
    ast.WhileStatement(_, body) -> collect_vars_stmt(body)
    ast.DoWhileStatement(_, body) -> collect_vars_stmt(body)
    ast.ForStatement(init, _, _, body) -> {
      let init_vars = case init {
        Some(ast.ForInitDeclaration(ast.VariableDeclaration(ast.Var, decls))) ->
          list.flat_map(decls, fn(d) {
            case d {
              ast.VariableDeclarator(pattern, _) ->
                collect_pattern_names(pattern)
            }
          })
        _ -> []
      }
      list.append(init_vars, collect_vars_stmt(body))
    }
    ast.TryStatement(block, handler, finalizer) -> {
      let block_vars = case block {
        ast.BlockStatement(body) -> list.flat_map(body, collect_vars_stmt)
        _ -> collect_vars_stmt(block)
      }
      let handler_vars = case handler {
        Some(ast.CatchClause(_, body)) ->
          case body {
            ast.BlockStatement(b) -> list.flat_map(b, collect_vars_stmt)
            _ -> collect_vars_stmt(body)
          }
        None -> []
      }
      let finally_vars = case finalizer {
        Some(f) ->
          case f {
            ast.BlockStatement(b) -> list.flat_map(b, collect_vars_stmt)
            _ -> collect_vars_stmt(f)
          }
        None -> []
      }
      list.flatten([block_vars, handler_vars, finally_vars])
    }
    ast.ForInStatement(left, _, body) | ast.ForOfStatement(left, _, body, ..) -> {
      let left_vars = case left {
        ast.ForInitDeclaration(ast.VariableDeclaration(ast.Var, decls)) ->
          list.flat_map(decls, fn(d) {
            case d {
              ast.VariableDeclarator(pattern, _) ->
                collect_pattern_names(pattern)
            }
          })
        _ -> []
      }
      list.append(left_vars, collect_vars_stmt(body))
    }
    ast.LabeledStatement(_, body) -> collect_vars_stmt(body)
    ast.SwitchStatement(_, cases) ->
      list.flat_map(cases, fn(c) {
        case c {
          ast.SwitchCase(_, consequent) ->
            list.flat_map(consequent, collect_vars_stmt)
        }
      })
    // Don't enter nested functions
    ast.FunctionDeclaration(..) -> []
    _ -> []
  }
}

/// Collect and compile hoisted function declarations.
/// Returns updated emitter + list of (name, func_index) pairs.
fn collect_hoisted_funcs(
  e: Emitter,
  stmts: List(ast.Statement),
) -> #(Emitter, List(#(String, Int))) {
  list.fold(stmts, #(e, []), fn(acc, stmt) {
    let #(e, funcs) = acc
    case stmt {
      ast.FunctionDeclaration(Some(name), params, body, _is_gen, _is_async) -> {
        let child = compile_function_body(e, Some(name), params, body, False)
        let #(e, idx) = add_child_function(e, child)
        #(e, list.append(funcs, [#(name, idx)]))
      }
      _ -> #(e, funcs)
    }
  })
}

/// Compile a function body into a CompiledChild.
fn compile_function_body(
  parent: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: ast.Statement,
  is_arrow: Bool,
) -> CompiledChild {
  // Use a fresh emitter inheriting nothing from parent (except label counter for uniqueness)
  let e = Emitter(..new_emitter(), next_label: parent.next_label)

  let e = emit_op(e, EnterScope(FunctionScope))

  // Phase 1: Declare parameters (identifier or synthetic for destructuring)
  let #(e, destructured_params) =
    list.index_fold(params, #(e, []), fn(acc, param, idx) {
      let #(e, destr) = acc
      case param {
        ast.IdentifierPattern(pname) -> #(
          emit_op(e, DeclareVar(pname, ParamBinding)),
          destr,
        )
        _ -> {
          let synthetic = "$param_" <> int.to_string(idx)
          let e = emit_op(e, DeclareVar(synthetic, ParamBinding))
          #(e, list.append(destr, [#(synthetic, param)]))
        }
      }
    })

  // Phase 2: Emit destructuring for non-identifier params
  let e =
    list.fold(destructured_params, e, fn(e, dp) {
      let #(synthetic, pattern) = dp
      let e = emit_ir(e, IrScopeGetVar(synthetic))
      case emit_destructuring_bind(e, pattern, LetBinding) {
        Ok(e) -> e
        Error(_) -> e
      }
    })

  let stmts = case body {
    ast.BlockStatement(s) -> s
    other -> [other]
  }

  // Hoisting for the function body
  let hoisted_vars = collect_hoisted_vars(stmts)
  let #(e, hoisted_funcs) = collect_hoisted_funcs(e, stmts)

  let e =
    list.fold(hoisted_vars, e, fn(e, vname) {
      emit_op(e, DeclareVar(vname, VarBinding))
    })

  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(fname, func_idx) = hf
      let e = emit_ir(e, IrMakeClosure(func_idx))
      let e = emit_ir(e, IrScopePutVar(fname))
      e
    })

  // Emit body statements
  let e = case emit_stmts(e, stmts) {
    Ok(e) -> e
    Error(_) -> e
    // For MVP, compilation errors in function bodies are ignored
  }

  // Implicit return undefined at end
  let e = push_const(e, JsUndefined)
  let e = emit_ir(e, IrReturn)

  let e = emit_op(e, LeaveScope)
  let #(code, constants, constants_map, children) = finish(e)

  CompiledChild(
    name:,
    arity: list.length(params),
    code:,
    constants:,
    constants_map:,
    functions: children,
    is_strict: False,
    is_arrow:,
  )
}

// ============================================================================
// Statement emission
// ============================================================================

fn emit_stmts(
  e: Emitter,
  stmts: List(ast.Statement),
) -> Result(Emitter, EmitError) {
  list.try_fold(stmts, e, emit_stmt)
}

fn emit_stmt(e: Emitter, stmt: ast.Statement) -> Result(Emitter, EmitError) {
  case stmt {
    ast.EmptyStatement -> Ok(e)

    ast.ExpressionStatement(expr) -> {
      use e <- result.map(emit_expr(e, expr))
      emit_ir(e, IrPop)
    }

    ast.BlockStatement(body) -> {
      let e = emit_op(e, EnterScope(BlockScope))
      use e <- result.map(emit_stmts(e, body))
      emit_op(e, LeaveScope)
    }

    ast.VariableDeclaration(kind, declarators) -> {
      let binding_kind = case kind {
        ast.Var -> VarBinding
        ast.Let -> LetBinding
        ast.Const -> ConstBinding
      }
      list.try_fold(declarators, e, fn(e, decl) {
        case decl {
          ast.VariableDeclarator(ast.IdentifierPattern(name), init) -> {
            // For let/const, emit declaration marker (var already hoisted)
            let e = case kind {
              ast.Let | ast.Const -> emit_op(e, DeclareVar(name, binding_kind))
              ast.Var -> e
            }
            // Emit initializer if present
            case init {
              Some(init_expr) -> {
                use e <- result.map(emit_expr(e, init_expr))
                emit_ir(e, IrScopePutVar(name))
              }
              None -> Ok(e)
            }
          }
          // Destructuring patterns
          ast.VariableDeclarator(pattern, init) -> {
            use e <- result.try(case init {
              Some(init_expr) -> emit_expr(e, init_expr)
              None -> Ok(push_const(e, JsUndefined))
            })
            emit_destructuring_bind(e, pattern, binding_kind)
          }
        }
      })
    }

    ast.IfStatement(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_stmt(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      let e = case alternate {
        Some(alt) -> {
          case emit_stmt(e, alt) {
            Ok(e) -> e
            Error(_) -> e
          }
        }
        None -> e
      }
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    ast.WhileStatement(condition, body) -> {
      let #(e, loop_start) = fresh_label(e)
      let #(e, loop_end) = fresh_label(e)
      let e = push_loop(e, loop_end, loop_start)
      let e = emit_ir(e, IrLabel(loop_start))
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(loop_end))
      use e <- result.try(emit_stmt(e, body))
      let e = emit_ir(e, IrJump(loop_start))
      let e = emit_ir(e, IrLabel(loop_end))
      let e = pop_loop(e)
      Ok(e)
    }

    ast.DoWhileStatement(condition, body) -> {
      let #(e, loop_start) = fresh_label(e)
      let #(e, loop_cond) = fresh_label(e)
      let #(e, loop_end) = fresh_label(e)
      let e = push_loop(e, loop_end, loop_cond)
      let e = emit_ir(e, IrLabel(loop_start))
      use e <- result.try(emit_stmt(e, body))
      let e = emit_ir(e, IrLabel(loop_cond))
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfTrue(loop_start))
      let e = emit_ir(e, IrLabel(loop_end))
      let e = pop_loop(e)
      Ok(e)
    }

    ast.ForStatement(init, condition, update, body) -> {
      let #(e, loop_start) = fresh_label(e)
      let #(e, loop_continue) = fresh_label(e)
      let #(e, loop_end) = fresh_label(e)

      // For-init may create a block scope (for let/const)
      let e = emit_op(e, EnterScope(BlockScope))

      // Init
      let e = case init {
        Some(ast.ForInitExpression(expr)) -> {
          case emit_expr(e, expr) {
            Ok(e) -> emit_ir(e, IrPop)
            Error(_) -> e
          }
        }
        Some(ast.ForInitDeclaration(decl)) -> {
          case emit_stmt(e, decl) {
            Ok(e) -> e
            Error(_) -> e
          }
        }
        _ -> e
      }

      let e = push_loop(e, loop_end, loop_continue)
      let e = emit_ir(e, IrLabel(loop_start))

      // Condition
      let e = case condition {
        Some(cond) -> {
          case emit_expr(e, cond) {
            Ok(e) -> emit_ir(e, IrJumpIfFalse(loop_end))
            Error(_) -> e
          }
        }
        None -> e
      }

      // Body
      let e = case emit_stmt(e, body) {
        Ok(e) -> e
        Error(_) -> e
      }

      // Continue target
      let e = emit_ir(e, IrLabel(loop_continue))

      // Update
      let e = case update {
        Some(upd) -> {
          case emit_expr(e, upd) {
            Ok(e) -> emit_ir(e, IrPop)
            Error(_) -> e
          }
        }
        None -> e
      }

      let e = emit_ir(e, IrJump(loop_start))
      let e = emit_ir(e, IrLabel(loop_end))
      let e = pop_loop(e)
      let e = emit_op(e, LeaveScope)
      Ok(e)
    }

    ast.ReturnStatement(arg) -> {
      let e = case arg {
        Some(expr) -> {
          case emit_expr(e, expr) {
            Ok(e) -> e
            Error(_) -> push_const(e, JsUndefined)
          }
        }
        None -> push_const(e, JsUndefined)
      }
      let e = emit_ir(e, IrReturn)
      Ok(e)
    }

    ast.ThrowStatement(arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_ir(e, IrThrow)
    }

    ast.TryStatement(block, handler, _finalizer) -> {
      // MVP: try-catch only (no finally)
      case handler {
        Some(ast.CatchClause(param, catch_body)) -> {
          let #(e, catch_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          let e = emit_ir(e, IrPushTry(catch_label, -1))
          use e <- result.try(emit_stmt(e, block))
          let e = emit_ir(e, IrPopTry)
          let e = emit_ir(e, IrJump(end_label))

          let e = emit_ir(e, IrLabel(catch_label))
          let e = emit_op(e, EnterScope(BlockScope))

          // Bind catch parameter (thrown value is on stack from unwind)
          let e = case param {
            Some(pattern) -> {
              case emit_destructuring_bind(e, pattern, CatchBinding) {
                Ok(e) -> e
                Error(_) -> e
              }
            }
            None -> emit_ir(e, IrPop)
          }

          use e <- result.try(emit_stmt(e, catch_body))
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }
        None ->
          // try-finally without catch: just emit the block for MVP
          emit_stmt(e, block)
      }
    }

    ast.SwitchStatement(discriminant, cases) -> {
      emit_switch(e, discriminant, cases)
    }

    ast.BreakStatement(None) -> {
      case e.loop_stack {
        [ctx, ..] -> {
          let e = emit_ir(e, IrJump(ctx.break_label))
          Ok(e)
        }
        [] -> Error(BreakOutsideLoop)
      }
    }

    ast.ContinueStatement(None) -> {
      case e.loop_stack {
        [ctx, ..] -> {
          let e = emit_ir(e, IrJump(ctx.continue_label))
          Ok(e)
        }
        [] -> Error(ContinueOutsideLoop)
      }
    }

    ast.FunctionDeclaration(..) -> {
      // Already hoisted — nothing to emit here
      Ok(e)
    }

    ast.ForInStatement(left, right, body) -> emit_for_in(e, left, right, body)

    ast.ForOfStatement(left, right, body, _is_await) ->
      emit_for_of(e, left, right, body)

    _ -> Error(Unsupported("statement: " <> string_inspect_stmt_kind(stmt)))
  }
}

// ============================================================================
// Expression emission
// ============================================================================

fn emit_expr(e: Emitter, expr: ast.Expression) -> Result(Emitter, EmitError) {
  case expr {
    // Literals
    ast.NumberLiteral(value) -> Ok(push_const(e, JsNumber(Finite(value))))
    ast.StringExpression(value) -> Ok(push_const(e, JsString(value)))
    ast.BooleanLiteral(value) -> Ok(push_const(e, JsBool(value)))
    ast.NullLiteral -> Ok(push_const(e, JsNull))
    ast.UndefinedExpression -> Ok(push_const(e, JsUndefined))

    // Identifier
    ast.Identifier("undefined") -> Ok(push_const(e, JsUndefined))
    ast.Identifier(name) -> Ok(emit_ir(e, IrScopeGetVar(name)))

    // Binary expressions
    ast.BinaryExpression(op, left, right) -> {
      use e <- result.try(emit_expr(e, left))
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrBinOp(translate_binop(op)))
    }

    // Logical expressions (short-circuit)
    ast.LogicalExpression(ast.LogicalAnd, left, right) -> {
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfFalse(end_label))
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    ast.LogicalExpression(ast.LogicalOr, left, right) -> {
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfTrue(end_label))
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    ast.LogicalExpression(ast.NullishCoalescing, left, right) -> {
      let #(e, use_right_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfNullish(use_right_label))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(use_right_label))
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    // Other logical ops are just binary ops
    ast.LogicalExpression(op, left, right) -> {
      use e <- result.try(emit_expr(e, left))
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrBinOp(translate_binop(op)))
    }

    // Unary expressions
    ast.UnaryExpression(ast.TypeOf, _, ast.Identifier(name)) -> {
      // typeof x must NOT throw for undeclared variables
      Ok(emit_ir(e, IrScopeTypeofVar(name)))
    }

    ast.UnaryExpression(ast.TypeOf, _, arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_ir(e, IrTypeOf)
    }

    ast.UnaryExpression(op, _, arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_ir(e, IrUnaryOp(translate_unaryop(op)))
    }

    // Update expressions (++/--)
    ast.UpdateExpression(op, prefix, ast.Identifier(name)) -> {
      let one = JsNumber(Finite(1.0))
      let bin_kind = case op {
        ast.Increment -> opcode.Add
        ast.Decrement -> opcode.Sub
      }
      case prefix {
        True -> {
          // ++x: get, add 1, dup (keep result), store
          let e = emit_ir(e, IrScopeGetVar(name))
          let e = push_const(e, one)
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrScopePutVar(name))
          Ok(e)
        }
        False -> {
          // x++: get, dup (old value stays as result), add 1, store
          let e = emit_ir(e, IrScopeGetVar(name))
          let e = emit_ir(e, IrDup)
          let e = push_const(e, one)
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrScopePutVar(name))
          Ok(e)
        }
      }
    }

    // Assignment to identifier
    ast.AssignmentExpression(ast.Assign, ast.Identifier(name), right) -> {
      use e <- result.map(emit_expr(e, right))
      let e = emit_ir(e, IrDup)
      emit_ir(e, IrScopePutVar(name))
    }

    // Compound assignment to identifier
    ast.AssignmentExpression(op, ast.Identifier(name), right) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          let e = emit_ir(e, IrScopeGetVar(name))
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrDup)
          emit_ir(e, IrScopePutVar(name))
        }
        Error(_) -> Error(Unsupported("assignment op"))
      }
    }

    // Assignment to dot member expression (obj.prop = val)
    ast.AssignmentExpression(
      ast.Assign,
      ast.MemberExpression(obj, ast.Identifier(prop), False),
      right,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, right))
      // Stack: [val, obj, ...] — PutField pops both, leaves val
      emit_ir(e, IrPutField(prop))
    }

    // Assignment to computed member expression (obj[key] = val)
    ast.AssignmentExpression(
      ast.Assign,
      ast.MemberExpression(obj, key, True),
      right,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, right))
      // Stack: [obj, key, val] — PutElem expects [val, key, obj]
      emit_ir(e, IrPutElem)
    }

    // Compound assignment to computed member (obj[key] += val)
    ast.AssignmentExpression(op, ast.MemberExpression(obj, key, True), right) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          use e <- result.try(emit_expr(e, obj))
          use e <- result.try(emit_expr(e, key))
          // GetElem2 reads obj[key] but keeps obj+key on stack
          let e = emit_ir(e, IrGetElem2)
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          // Stack: [obj, key, result] — PutElem consumes all three
          emit_ir(e, IrPutElem)
        }
        Error(_) -> Error(Unsupported("assignment op"))
      }
    }

    // Method call: obj.method(args) — emits GetField2 + CallMethod for this binding
    ast.CallExpression(
      ast.MemberExpression(obj, ast.Identifier(method_name), False),
      args,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      let e = emit_ir(e, IrGetField2(method_name))
      use e <- result.map(list.try_fold(args, e, emit_expr))
      emit_ir(e, IrCallMethod(method_name, list.length(args)))
    }
    // Regular call expression
    ast.CallExpression(callee, args) -> {
      use e <- result.try(emit_expr(e, callee))
      use e <- result.map(list.try_fold(args, e, emit_expr))
      emit_ir(e, opcode.IrCall(list.length(args)))
    }

    // Conditional (ternary)
    ast.ConditionalExpression(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_expr(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      use e <- result.map(emit_expr(e, alternate))
      emit_ir(e, IrLabel(end_label))
    }

    // Sequence expression (comma operator)
    ast.SequenceExpression(exprs) -> emit_sequence(e, exprs)

    // Object literal
    ast.ObjectExpression(properties) -> {
      let e = emit_ir(e, IrNewObject)
      list.try_fold(properties, e, fn(e, prop) {
        case prop {
          ast.Property(
            key: ast.Identifier(name),
            value:,
            kind: ast.Init,
            computed: False,
            ..,
          )
          | ast.Property(
              key: ast.StringExpression(name),
              value:,
              kind: ast.Init,
              computed: False,
              ..,
            ) -> {
            use e <- result.map(emit_expr(e, value))
            emit_ir(e, IrDefineField(name))
          }
          _ -> Error(Unsupported("computed/getter/setter property"))
        }
      })
    }

    // Member expression (dot access)
    ast.MemberExpression(object, ast.Identifier(prop), False) -> {
      use e <- result.map(emit_expr(e, object))
      emit_ir(e, IrGetField(prop))
    }

    // Computed member expression (obj[key])
    ast.MemberExpression(object, property, True) -> {
      use e <- result.try(emit_expr(e, object))
      use e <- result.map(emit_expr(e, property))
      emit_ir(e, IrGetElem)
    }

    // Array literal
    ast.ArrayExpression(elements) -> {
      let count = list.length(elements)
      use e <- result.map(
        list.try_fold(elements, e, fn(e, elem) {
          case elem {
            Some(expr) -> emit_expr(e, expr)
            // Hole in array — push undefined
            None -> Ok(push_const(e, JsUndefined))
          }
        }),
      )
      emit_ir(e, IrArrayFrom(count))
    }

    // Function expression
    ast.FunctionExpression(name, params, body, _is_gen, _is_async) -> {
      let child = compile_function_body(e, name, params, body, False)
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }

    // Arrow function expression
    ast.ArrowFunctionExpression(params, body, _is_async) -> {
      let body_stmt = case body {
        ast.ArrowBodyExpression(expr) ->
          ast.BlockStatement([ast.ReturnStatement(Some(expr))])
        ast.ArrowBodyBlock(stmt) -> stmt
      }
      let child = compile_function_body(e, None, params, body_stmt, True)
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }

    // This expression
    ast.ThisExpression -> Ok(emit_ir(e, IrGetThis))

    // New expression: new Foo(args)
    ast.NewExpression(callee, args) -> {
      use e <- result.try(emit_expr(e, callee))
      use e <- result.map(list.try_fold(args, e, emit_expr))
      emit_ir(e, IrCallConstructor(list.length(args)))
    }

    // Template literal: `text ${expr} more`
    // Desugar to string concatenation: "" + "text " + expr + " more"
    ast.TemplateLiteral(quasis, expressions) ->
      emit_template_literal(e, quasis, expressions)

    _ -> Error(Unsupported("expression: " <> string_inspect_expr_kind(expr)))
  }
}

fn emit_template_literal(
  e: Emitter,
  quasis: List(String),
  expressions: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  // Template literal `a${x}b${y}c` has quasis=["a","b","c"], expressions=[x,y]
  // Desugar to: "a" + x + "b" + y + "c"
  case quasis {
    [] -> Ok(push_const(e, JsString("")))
    [first, ..rest_quasis] -> {
      // Start with the first quasi string
      let e = push_const(e, JsString(first))
      // Interleave: for each expression, Add it, then Add the next quasi
      emit_template_parts(e, expressions, rest_quasis)
    }
  }
}

fn emit_template_parts(
  e: Emitter,
  expressions: List(ast.Expression),
  quasis: List(String),
) -> Result(Emitter, EmitError) {
  case expressions, quasis {
    [expr, ..rest_exprs], [quasi, ..rest_quasis] -> {
      // Emit expression, concat with accumulator
      use e <- result.try(emit_expr(e, expr))
      let e = emit_ir(e, IrBinOp(opcode.Add))
      // Emit next quasi string, concat
      let e = push_const(e, JsString(quasi))
      let e = emit_ir(e, IrBinOp(opcode.Add))
      emit_template_parts(e, rest_exprs, rest_quasis)
    }
    // If there are trailing expressions without quasis (shouldn't happen but safe)
    [expr, ..rest_exprs], [] -> {
      use e <- result.try(emit_expr(e, expr))
      let e = emit_ir(e, IrBinOp(opcode.Add))
      emit_template_parts(e, rest_exprs, [])
    }
    // Done
    [], _ -> Ok(e)
  }
}

fn emit_switch(
  e: Emitter,
  discriminant: ast.Expression,
  cases: List(ast.SwitchCase),
) -> Result(Emitter, EmitError) {
  let #(e, end_label) = fresh_label(e)

  // Push break context for switch (break; exits the switch)
  // Preserve parent continue_label if inside a loop
  let parent_continue = case e.loop_stack {
    [ctx, ..] -> ctx.continue_label
    [] -> -1
  }
  let e = push_loop(e, end_label, parent_continue)

  // Emit discriminant — stays on stack through comparison phase
  use e <- result.try(emit_expr(e, discriminant))

  // Allocate labels: each non-default case gets a "found" trampoline label
  // and a "body" label. Default cases only get a "body" label.
  // The trampoline pops the discriminant then jumps to the body label.
  // This ensures the discriminant is off the stack for all body code,
  // allowing fall-through between case bodies to work correctly.
  let #(e, body_labels) =
    list.fold(cases, #(e, []), fn(acc, _case) {
      let #(e, labels) = acc
      let #(e, label) = fresh_label(e)
      #(e, list.append(labels, [label]))
    })

  // Allocate found (trampoline) labels for non-default cases
  let #(e, found_labels) =
    list.fold(cases, #(e, []), fn(acc, c) {
      let #(e, labels) = acc
      case c {
        ast.SwitchCase(Some(_), _) -> {
          let #(e, label) = fresh_label(e)
          #(e, list.append(labels, [Some(label)]))
        }
        ast.SwitchCase(None, _) -> #(e, list.append(labels, [None]))
      }
    })

  // Phase 1: Emit comparison jumps
  // For each case with a test: Dup discriminant, emit test, StrictEq, JumpIfTrue(found_N)
  let #(e, default_body_label) =
    list.index_fold(cases, #(e, option.None), fn(acc, c, idx) {
      let #(e, default_lbl) = acc
      case c {
        ast.SwitchCase(Some(test_expr), _) -> {
          let e = emit_ir(e, IrDup)
          case emit_expr(e, test_expr) {
            Ok(e) -> {
              let e = emit_ir(e, IrBinOp(opcode.StrictEq))
              let found_lbl = case list.drop(found_labels, idx) {
                [Some(l), ..] -> l
                _ -> end_label
              }
              let e = emit_ir(e, IrJumpIfTrue(found_lbl))
              #(e, default_lbl)
            }
            Error(_) -> #(e, default_lbl)
          }
        }
        ast.SwitchCase(None, _) -> {
          // Default case — record its body label
          let body_lbl = case list.drop(body_labels, idx) {
            [l, ..] -> Some(l)
            [] -> Some(end_label)
          }
          #(e, body_lbl)
        }
      }
    })

  // No match: pop discriminant and jump to default body or end
  let e = emit_ir(e, IrPop)
  let e = case default_body_label {
    Some(dl) -> emit_ir(e, IrJump(dl))
    None -> emit_ir(e, IrJump(end_label))
  }

  // Phase 2: Emit trampolines — each pops discriminant and jumps to body
  let e =
    list.index_fold(cases, e, fn(e, _c, idx) {
      case list.drop(found_labels, idx) {
        [Some(found_lbl), ..] -> {
          let e = emit_ir(e, IrLabel(found_lbl))
          let e = emit_ir(e, IrPop)
          let body_lbl = case list.drop(body_labels, idx) {
            [l, ..] -> l
            [] -> end_label
          }
          emit_ir(e, IrJump(body_lbl))
        }
        _ -> e
      }
    })

  // Phase 3: Emit case bodies (fall-through between them)
  let e =
    list.index_fold(cases, e, fn(e, c, idx) {
      let label = case list.drop(body_labels, idx) {
        [l, ..] -> l
        [] -> end_label
      }
      let e = emit_ir(e, IrLabel(label))
      case c {
        ast.SwitchCase(_, consequent) -> {
          case list.try_fold(consequent, e, emit_stmt) {
            Ok(e) -> e
            Error(_) -> e
          }
        }
      }
    })

  let e = emit_ir(e, IrLabel(end_label))
  let e = pop_loop(e)
  Ok(e)
}

fn emit_sequence(
  e: Emitter,
  exprs: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  case exprs {
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_expr(e, only)
    [first, ..rest] -> {
      use e <- result.try(emit_expr(e, first))
      let e = emit_ir(e, IrPop)
      emit_sequence(e, rest)
    }
  }
}

// ============================================================================
// For-in / for-of loops
// ============================================================================

/// Emit a for-in loop: `for (lhs in rhs) body`
///
/// Stack pattern:
///   [obj] → ForInStart → [iterator]
///   loop: ForInNext → [iterator, key, done]
///   JumpIfTrue(cleanup) → [iterator, key]
///   bind key → [iterator]
///   body
///   Jump(loop) → cleanup: Pop key → loop_end: Pop iterator
fn emit_for_in(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, loop_start) = fresh_label(e)
  let #(e, loop_continue) = fresh_label(e)
  let #(e, cleanup) = fresh_label(e)
  let #(e, loop_end) = fresh_label(e)

  // Block scope for let/const
  let e = emit_op(e, EnterScope(BlockScope))

  // Evaluate the right-hand side (object to iterate)
  use e <- result.try(emit_expr(e, right))
  // ForInStart: pops object, pushes iterator ref
  let e = emit_ir(e, IrForInStart)

  let e = push_loop(e, loop_end, loop_continue)
  let e = emit_ir(e, IrLabel(loop_start))

  // ForInNext: peeks iterator, pushes key + done
  let e = emit_ir(e, IrForInNext)
  // If done, jump to cleanup (where we pop the unused key)
  let e = emit_ir(e, IrJumpIfTrue(cleanup))

  // Bind the key to the left-hand side variable
  use e <- result.try(emit_for_lhs_bind(e, left))

  // Body
  use e <- result.try(emit_stmt(e, body))

  // Continue point
  let e = emit_ir(e, IrLabel(loop_continue))
  let e = emit_ir(e, IrJump(loop_start))

  // cleanup: pop the key (done=true left it on stack)
  let e = emit_ir(e, IrLabel(cleanup))
  let e = emit_ir(e, IrPop)

  // loop_end: pop the iterator
  let e = emit_ir(e, IrLabel(loop_end))
  let e = emit_ir(e, IrPop)

  let e = pop_loop(e)
  let e = emit_op(e, LeaveScope)
  Ok(e)
}

/// Emit a for-of loop: `for (lhs of rhs) body`
///
/// Same stack pattern as for-in but uses GetIterator/IteratorNext.
fn emit_for_of(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, loop_start) = fresh_label(e)
  let #(e, loop_continue) = fresh_label(e)
  let #(e, cleanup) = fresh_label(e)
  let #(e, loop_end) = fresh_label(e)

  // Block scope for let/const
  let e = emit_op(e, EnterScope(BlockScope))

  // Evaluate the iterable
  use e <- result.try(emit_expr(e, right))
  // GetIterator: pops iterable, pushes iterator ref
  let e = emit_ir(e, IrGetIterator)

  let e = push_loop(e, loop_end, loop_continue)
  let e = emit_ir(e, IrLabel(loop_start))

  // IteratorNext: peeks iterator, pushes value + done
  let e = emit_ir(e, IrIteratorNext)
  // If done, jump to cleanup (where we pop the unused value)
  let e = emit_ir(e, IrJumpIfTrue(cleanup))

  // Bind the value to the left-hand side
  use e <- result.try(emit_for_lhs_bind(e, left))

  // Body
  use e <- result.try(emit_stmt(e, body))

  // Continue point
  let e = emit_ir(e, IrLabel(loop_continue))
  let e = emit_ir(e, IrJump(loop_start))

  // cleanup: pop the value (done=true left it on stack)
  let e = emit_ir(e, IrLabel(cleanup))
  let e = emit_ir(e, IrPop)

  // loop_end: pop the iterator
  let e = emit_ir(e, IrLabel(loop_end))
  let e = emit_ir(e, IrPop)

  let e = pop_loop(e)
  let e = emit_op(e, LeaveScope)
  Ok(e)
}

/// Bind the current value (on top of stack) to the for-in/for-of LHS.
/// The LHS can be:
///   - ForInitDeclaration(VariableDeclaration(...)) e.g. `for (let x ...)`
///   - ForInitExpression(Identifier(name)) e.g. `for (x ...)`
///   - ForInitPattern(pattern) e.g. `for ({a, b} ...)`
/// Consumes the value on top of stack.
fn emit_for_lhs_bind(
  e: Emitter,
  left: ast.ForInit,
) -> Result(Emitter, EmitError) {
  case left {
    ast.ForInitDeclaration(ast.VariableDeclaration(kind, declarators)) -> {
      let binding_kind = case kind {
        ast.Var -> VarBinding
        ast.Let -> LetBinding
        ast.Const -> ConstBinding
      }
      case declarators {
        [ast.VariableDeclarator(pattern, _)] ->
          emit_destructuring_bind(e, pattern, binding_kind)
        _ -> Error(Unsupported("for-in/of with multiple declarators"))
      }
    }
    ast.ForInitExpression(ast.Identifier(name)) -> {
      Ok(emit_ir(e, IrScopePutVar(name)))
    }
    ast.ForInitExpression(ast.MemberExpression(obj, ast.Identifier(prop), False)) -> {
      // e.g. for (obj.prop in ...) — rare but valid
      use e <- result.try(emit_expr(e, obj))
      let e = emit_ir(e, IrSwap)
      Ok(emit_ir(e, IrPutField(prop)))
    }
    ast.ForInitPattern(pattern) ->
      emit_destructuring_bind(e, pattern, VarBinding)
    _ -> Error(Unsupported("for-in/of left-hand side"))
  }
}

// ============================================================================
// Destructuring patterns
// ============================================================================

/// Emit code to destructure a value on top of stack into a pattern.
/// Consumes the value (pops it when done).
fn emit_destructuring_bind(
  e: Emitter,
  pattern: ast.Pattern,
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case pattern {
    ast.IdentifierPattern(name) -> {
      let e = case binding_kind {
        LetBinding | ConstBinding | ParamBinding | CatchBinding ->
          emit_op(e, DeclareVar(name, binding_kind))
        VarBinding | CaptureBinding -> e
      }
      Ok(emit_ir(e, IrScopePutVar(name)))
    }

    ast.ObjectPattern(properties) ->
      emit_object_destructure(e, properties, binding_kind)

    ast.ArrayPattern(elements) ->
      emit_array_destructure(e, elements, binding_kind)

    ast.AssignmentPattern(left, default_expr) -> {
      // Check if value === undefined, if so use default
      let #(e, has_val) = fresh_label(e)
      let e = emit_ir(e, IrDup)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrBinOp(opcode.StrictEq))
      let e = emit_ir(e, IrJumpIfFalse(has_val))
      // Value is undefined — pop it and use default
      let e = emit_ir(e, IrPop)
      use e <- result.try(emit_expr(e, default_expr))
      let e = emit_ir(e, IrLabel(has_val))
      // Now the value (original or default) is on stack
      emit_destructuring_bind(e, left, binding_kind)
    }

    ast.RestElement(_) -> {
      let _e = emit_ir(e, IrPop)
      Error(Unsupported("rest element in destructuring"))
    }
  }
}

/// Destructure an object: for each property, Dup obj, GetField, recurse; then Pop obj.
fn emit_object_destructure(
  e: Emitter,
  properties: List(ast.PatternProperty),
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  use e <- result.map(emit_object_props(e, properties, binding_kind))
  emit_ir(e, IrPop)
}

fn emit_object_props(
  e: Emitter,
  properties: List(ast.PatternProperty),
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case properties {
    [] -> Ok(e)
    [prop, ..rest] -> {
      use e <- result.try(emit_single_object_prop(e, prop, binding_kind))
      emit_object_props(e, rest, binding_kind)
    }
  }
}

fn emit_single_object_prop(
  e: Emitter,
  prop: ast.PatternProperty,
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case prop {
    ast.PatternProperty(key:, value:, computed: False, ..) -> {
      let field_name = case key {
        ast.Identifier(name) -> Ok(name)
        ast.StringExpression(name) -> Ok(name)
        _ -> Error(Unsupported("computed property key in destructuring"))
      }
      use name <- result.try(field_name)
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrGetField(name))
      emit_destructuring_bind(e, value, binding_kind)
    }
    ast.PatternProperty(computed: True, ..) ->
      Error(Unsupported("computed property in destructuring"))
    ast.RestProperty(_) -> Error(Unsupported("rest property in destructuring"))
  }
}

/// Destructure an array: for each element, Dup arr, PushConst(index), GetElem, recurse; then Pop arr.
fn emit_array_destructure(
  e: Emitter,
  elements: List(Option(ast.Pattern)),
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  use e <- result.map(emit_array_elements(e, elements, 0, binding_kind))
  emit_ir(e, IrPop)
}

fn emit_array_elements(
  e: Emitter,
  elements: List(Option(ast.Pattern)),
  index: Int,
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case elements {
    [] -> Ok(e)
    [None, ..rest] -> emit_array_elements(e, rest, index + 1, binding_kind)
    [Some(pattern), ..rest] -> {
      let e = emit_ir(e, IrDup)
      let e = push_const(e, JsNumber(Finite(int.to_float(index))))
      let e = emit_ir(e, IrGetElem)
      use e <- result.try(emit_destructuring_bind(e, pattern, binding_kind))
      emit_array_elements(e, rest, index + 1, binding_kind)
    }
  }
}

// ============================================================================
// Operator translation
// ============================================================================

fn translate_binop(op: ast.BinaryOp) -> opcode.BinOpKind {
  case op {
    ast.Add -> opcode.Add
    ast.Subtract -> opcode.Sub
    ast.Multiply -> opcode.Mul
    ast.Divide -> opcode.Div
    ast.Modulo -> opcode.Mod
    ast.Exponentiation -> opcode.Exp
    ast.StrictEqual -> opcode.StrictEq
    ast.StrictNotEqual -> opcode.StrictNotEq
    ast.Equal -> opcode.Eq
    ast.NotEqual -> opcode.NotEq
    ast.LessThan -> opcode.Lt
    ast.GreaterThan -> opcode.Gt
    ast.LessThanEqual -> opcode.LtEq
    ast.GreaterThanEqual -> opcode.GtEq
    ast.LeftShift -> opcode.ShiftLeft
    ast.RightShift -> opcode.ShiftRight
    ast.UnsignedRightShift -> opcode.UShiftRight
    ast.BitwiseAnd -> opcode.BitAnd
    ast.BitwiseOr -> opcode.BitOr
    ast.BitwiseXor -> opcode.BitXor
    ast.In -> opcode.In
    ast.InstanceOf -> opcode.InstanceOf
    // Logical ops should not reach here (handled separately)
    ast.LogicalAnd | ast.LogicalOr | ast.NullishCoalescing -> opcode.Add
  }
}

fn translate_unaryop(op: ast.UnaryOp) -> opcode.UnaryOpKind {
  case op {
    ast.Negate -> opcode.Neg
    ast.UnaryPlus -> opcode.Pos
    ast.LogicalNot -> opcode.LogicalNot
    ast.BitwiseNot -> opcode.BitNot
    ast.Void -> opcode.Void
    // TypeOf handled separately, Delete not in MVP
    ast.TypeOf -> opcode.Void
    ast.Delete -> opcode.Void
  }
}

fn compound_to_binop(op: ast.AssignmentOp) -> Result(opcode.BinOpKind, Nil) {
  case op {
    ast.AddAssign -> Ok(opcode.Add)
    ast.SubtractAssign -> Ok(opcode.Sub)
    ast.MultiplyAssign -> Ok(opcode.Mul)
    ast.DivideAssign -> Ok(opcode.Div)
    ast.ModuloAssign -> Ok(opcode.Mod)
    ast.ExponentiationAssign -> Ok(opcode.Exp)
    ast.LeftShiftAssign -> Ok(opcode.ShiftLeft)
    ast.RightShiftAssign -> Ok(opcode.ShiftRight)
    ast.UnsignedRightShiftAssign -> Ok(opcode.UShiftRight)
    ast.BitwiseAndAssign -> Ok(opcode.BitAnd)
    ast.BitwiseOrAssign -> Ok(opcode.BitOr)
    ast.BitwiseXorAssign -> Ok(opcode.BitXor)
    ast.Assign -> Error(Nil)
    ast.LogicalAndAssign | ast.LogicalOrAssign | ast.NullishCoalesceAssign ->
      Error(Nil)
  }
}

// ============================================================================
// Debug helpers
// ============================================================================

fn string_inspect_stmt_kind(stmt: ast.Statement) -> String {
  case stmt {
    ast.EmptyStatement -> "EmptyStatement"
    ast.ExpressionStatement(_) -> "ExpressionStatement"
    ast.BlockStatement(_) -> "BlockStatement"
    ast.VariableDeclaration(..) -> "VariableDeclaration"
    ast.ReturnStatement(_) -> "ReturnStatement"
    ast.IfStatement(..) -> "IfStatement"
    ast.ThrowStatement(_) -> "ThrowStatement"
    ast.WhileStatement(..) -> "WhileStatement"
    ast.DoWhileStatement(..) -> "DoWhileStatement"
    ast.ForStatement(..) -> "ForStatement"
    ast.ForInStatement(..) -> "ForInStatement"
    ast.ForOfStatement(..) -> "ForOfStatement"
    ast.SwitchStatement(..) -> "SwitchStatement"
    ast.TryStatement(..) -> "TryStatement"
    ast.BreakStatement(_) -> "BreakStatement"
    ast.ContinueStatement(_) -> "ContinueStatement"
    ast.DebuggerStatement -> "DebuggerStatement"
    ast.LabeledStatement(..) -> "LabeledStatement"
    ast.WithStatement(..) -> "WithStatement"
    ast.FunctionDeclaration(..) -> "FunctionDeclaration"
    ast.ClassDeclaration(..) -> "ClassDeclaration"
  }
}

fn string_inspect_expr_kind(expr: ast.Expression) -> String {
  case expr {
    ast.Identifier(_) -> "Identifier"
    ast.NumberLiteral(_) -> "NumberLiteral"
    ast.StringExpression(_) -> "StringExpression"
    ast.BooleanLiteral(_) -> "BooleanLiteral"
    ast.NullLiteral -> "NullLiteral"
    ast.UndefinedExpression -> "UndefinedExpression"
    ast.BinaryExpression(..) -> "BinaryExpression"
    ast.LogicalExpression(..) -> "LogicalExpression"
    ast.UnaryExpression(..) -> "UnaryExpression"
    ast.UpdateExpression(..) -> "UpdateExpression"
    ast.AssignmentExpression(..) -> "AssignmentExpression"
    ast.CallExpression(..) -> "CallExpression"
    ast.MemberExpression(..) -> "MemberExpression"
    ast.OptionalMemberExpression(..) -> "OptionalMemberExpression"
    ast.OptionalCallExpression(..) -> "OptionalCallExpression"
    ast.ConditionalExpression(..) -> "ConditionalExpression"
    ast.NewExpression(..) -> "NewExpression"
    ast.ThisExpression -> "ThisExpression"
    ast.SuperExpression -> "SuperExpression"
    ast.ArrayExpression(_) -> "ArrayExpression"
    ast.ObjectExpression(_) -> "ObjectExpression"
    ast.FunctionExpression(..) -> "FunctionExpression"
    ast.ArrowFunctionExpression(..) -> "ArrowFunctionExpression"
    ast.ClassExpression(..) -> "ClassExpression"
    ast.YieldExpression(..) -> "YieldExpression"
    ast.AwaitExpression(_) -> "AwaitExpression"
    ast.SequenceExpression(_) -> "SequenceExpression"
    ast.SpreadElement(_) -> "SpreadElement"
    ast.TemplateLiteral(..) -> "TemplateLiteral"
    ast.TaggedTemplateExpression(..) -> "TaggedTemplateExpression"
    ast.MetaProperty(..) -> "MetaProperty"
    ast.ImportExpression(_) -> "ImportExpression"
    ast.RegExpLiteral(..) -> "RegExpLiteral"
  }
}

// Need to import result for map/try
import gleam/result
