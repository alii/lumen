/// Phase 2: Scope Resolution
///
/// Walks the EmitterOp list from Phase 1 and resolves symbolic variable names
/// to local slot indices. Consumes scope markers (EnterScope/LeaveScope/DeclareVar)
/// and replaces IrScopeGetVar/IrScopePutVar/IrScopeTypeofVar with concrete
/// GetLocal/PutLocal/GetGlobal/PutGlobal/TypeofGlobal ops.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import lumen/compiler/emit.{
  type BindingKind, type EmitterOp, BlockScope, CatchBinding, ConstBinding,
  DeclareVar, EnterScope, FunctionScope, Ir, LeaveScope, LetBinding,
  ParamBinding, VarBinding,
}
import lumen/vm/opcode.{
  type IrOp, IrGetGlobal, IrGetLocal, IrPushConst, IrPutGlobal, IrPutLocal,
  IrScopeGetVar, IrScopePutVar, IrScopeTypeofVar, IrTypeOf, IrTypeofGlobal,
}
import lumen/vm/value.{type JsValue, JsUndefined, JsUninitialized}

// ============================================================================
// Types
// ============================================================================

/// A binding in a scope — maps name to local slot index.
type Binding {
  Binding(index: Int, kind: BindingKind)
}

/// A single scope level.
type Scope {
  Scope(kind: emit.ScopeKind, bindings: Dict(String, Binding))
}

/// The scope resolver state.
type Resolver {
  Resolver(
    scopes: List(Scope),
    next_local: Int,
    max_locals: Int,
    output: List(IrOp),
    constants: List(JsValue),
    constants_map: Dict(JsValue, Int),
    next_const: Int,
  )
}

// ============================================================================
// Public API
// ============================================================================

/// Resolve scopes in a list of EmitterOps.
/// Returns resolved IrOps (no scope markers), local_count, and updated constants.
pub fn resolve(
  code: List(EmitterOp),
  constants: List(JsValue),
  constants_map: Dict(JsValue, Int),
) -> #(List(IrOp), Int, List(JsValue), Dict(JsValue, Int)) {
  let r =
    Resolver(
      scopes: [],
      next_local: 0,
      max_locals: 0,
      output: [],
      constants:,
      constants_map:,
      next_const: list.length(constants),
    )
  let r = resolve_ops(r, code)
  #(list.reverse(r.output), r.max_locals, r.constants, r.constants_map)
}

// ============================================================================
// Resolution loop
// ============================================================================

fn resolve_ops(r: Resolver, ops: List(EmitterOp)) -> Resolver {
  case ops {
    [] -> r
    [op, ..rest] -> {
      let r = resolve_one(r, op)
      resolve_ops(r, rest)
    }
  }
}

fn resolve_one(r: Resolver, op: EmitterOp) -> Resolver {
  case op {
    EnterScope(kind) -> {
      let scope = Scope(kind:, bindings: dict.new())
      Resolver(..r, scopes: [scope, ..r.scopes])
    }

    LeaveScope -> {
      case r.scopes {
        [_, ..rest] -> Resolver(..r, scopes: rest)
        [] -> r
      }
    }

    DeclareVar(name, kind) -> {
      let index = r.next_local
      let binding = Binding(index:, kind:)
      let new_max = case index + 1 > r.max_locals {
        True -> index + 1
        False -> r.max_locals
      }
      let r = Resolver(..r, next_local: index + 1, max_locals: new_max)

      // Add binding to the appropriate scope
      let r = case kind {
        VarBinding | ParamBinding -> add_to_function_scope(r, name, binding)
        LetBinding | ConstBinding | CatchBinding ->
          add_to_current_scope(r, name, binding)
      }

      // Emit initialization for var (undefined) and let/const (uninitialized/TDZ)
      case kind {
        VarBinding -> {
          let #(r, idx) = ensure_constant(r, JsUndefined)
          emit(emit(r, IrPushConst(idx)), IrPutLocal(index))
        }
        LetBinding | ConstBinding -> {
          let #(r, idx) = ensure_constant(r, JsUninitialized)
          emit(emit(r, IrPushConst(idx)), IrPutLocal(index))
        }
        ParamBinding | CatchBinding -> r
        // Params are set by call convention, catch by unwind
      }
    }

    Ir(IrScopeGetVar(name)) -> {
      case lookup(r.scopes, name) {
        Some(Binding(index:, ..)) -> emit(r, IrGetLocal(index))
        None -> emit(r, IrGetGlobal(name))
      }
    }

    Ir(IrScopePutVar(name)) -> {
      case lookup(r.scopes, name) {
        Some(Binding(index:, ..)) -> emit(r, IrPutLocal(index))
        None -> emit(r, IrPutGlobal(name))
      }
    }

    Ir(IrScopeTypeofVar(name)) -> {
      case lookup(r.scopes, name) {
        Some(Binding(index:, ..)) -> {
          let r = emit(r, IrGetLocal(index))
          emit(r, IrTypeOf)
        }
        None -> emit(r, IrTypeofGlobal(name))
      }
    }

    // All other IR ops: pass through
    Ir(ir_op) -> emit(r, ir_op)
  }
}

// ============================================================================
// Scope helpers
// ============================================================================

fn add_to_current_scope(r: Resolver, name: String, binding: Binding) -> Resolver {
  case r.scopes {
    [scope, ..rest] -> {
      let scope =
        Scope(..scope, bindings: dict.insert(scope.bindings, name, binding))
      Resolver(..r, scopes: [scope, ..rest])
    }
    [] -> r
  }
}

fn add_to_function_scope(
  r: Resolver,
  name: String,
  binding: Binding,
) -> Resolver {
  let scopes = add_to_func_scope_inner(r.scopes, name, binding)
  Resolver(..r, scopes:)
}

fn add_to_func_scope_inner(
  scopes: List(Scope),
  name: String,
  binding: Binding,
) -> List(Scope) {
  case scopes {
    [] -> []
    [scope, ..rest] ->
      case scope.kind {
        FunctionScope -> {
          // Check if already declared (var can be declared multiple times)
          case dict.get(scope.bindings, name) {
            Ok(_) -> [scope, ..rest]
            // Already exists, reuse
            Error(_) -> {
              let scope =
                Scope(
                  ..scope,
                  bindings: dict.insert(scope.bindings, name, binding),
                )
              [scope, ..rest]
            }
          }
        }
        BlockScope -> [scope, ..add_to_func_scope_inner(rest, name, binding)]
      }
  }
}

fn lookup(scopes: List(Scope), name: String) -> Option(Binding) {
  case scopes {
    [] -> None
    [scope, ..rest] ->
      case dict.get(scope.bindings, name) {
        Ok(binding) -> Some(binding)
        Error(_) -> lookup(rest, name)
      }
  }
}

fn emit(r: Resolver, op: IrOp) -> Resolver {
  Resolver(..r, output: [op, ..r.output])
}

fn ensure_constant(r: Resolver, val: JsValue) -> #(Resolver, Int) {
  case dict.get(r.constants_map, val) {
    Ok(idx) -> #(r, idx)
    Error(_) -> {
      let idx = r.next_const
      let r =
        Resolver(
          ..r,
          constants: list.append(r.constants, [val]),
          constants_map: dict.insert(r.constants_map, val, idx),
          next_const: idx + 1,
        )
      #(r, idx)
    }
  }
}
