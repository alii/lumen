/// Bytecode Compiler
///
/// Translates a parsed AST into a FuncTemplate that the VM can execute.
/// Three-phase pipeline:
///   Phase 1 (emit): AST → EmitterOp (symbolic names + label IDs)
///   Phase 2 (scope): EmitterOp → IrOp (resolved local indices + label IDs)
///   Phase 3 (resolve): IrOp → Op (absolute PC addresses)
import gleam/list
import gleam/option.{None}
import lumen/ast
import lumen/compiler/emit
import lumen/compiler/resolve
import lumen/compiler/scope
import lumen/vm/opcode.{type FuncTemplate}

/// Compilation errors.
pub type CompileError {
  BreakOutsideLoop
  ContinueOutsideLoop
  Unsupported(description: String)
}

/// Compile a parsed program into a FuncTemplate the VM can execute.
pub fn compile(program: ast.Program) -> Result(FuncTemplate, CompileError) {
  case program {
    ast.Script(body) -> compile_script(body)
    ast.Module(_) -> Error(Unsupported("modules not supported"))
  }
}

fn compile_script(
  stmts: List(ast.Statement),
) -> Result(FuncTemplate, CompileError) {
  // Phase 1: Emit IR from AST
  case emit.emit_program(stmts) {
    Ok(#(emitter_ops, constants, constants_map, children)) -> {
      // Phase 2: Resolve scopes (names → local indices)
      let #(ir_ops, local_count, constants, _constants_map) =
        scope.resolve(emitter_ops, constants, constants_map)

      // Process child functions through Phase 2 + Phase 3 recursively
      let child_templates = list.map(children, compile_child)

      // Phase 3: Resolve labels (label IDs → PC addresses)
      let template =
        resolve.resolve(
          ir_ops,
          constants,
          local_count,
          child_templates,
          None,
          0,
          [],
          False,
          False,
        )
      Ok(template)
    }
    Error(emit.BreakOutsideLoop) -> Error(BreakOutsideLoop)
    Error(emit.ContinueOutsideLoop) -> Error(ContinueOutsideLoop)
    Error(emit.Unsupported(desc)) -> Error(Unsupported(desc))
  }
}

fn compile_child(child: emit.CompiledChild) -> FuncTemplate {
  // Phase 2: Resolve scopes
  let #(ir_ops, local_count, constants, _constants_map) =
    scope.resolve(child.code, child.constants, child.constants_map)

  // Recursively compile grandchildren
  let grandchild_templates = list.map(child.functions, compile_child)

  // Phase 3: Resolve labels
  resolve.resolve(
    ir_ops,
    constants,
    local_count,
    grandchild_templates,
    child.name,
    child.arity,
    [],
    child.is_strict,
    child.is_arrow,
  )
}
