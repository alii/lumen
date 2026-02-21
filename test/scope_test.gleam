import lumen/parser

fn should_fail(source: String) -> String {
  case parser.parse(source, parser.Script) {
    Ok(_) -> "PASS (BAD - should fail)"
    Error(_) -> "FAIL (GOOD)"
  }
}

fn should_pass(source: String) -> String {
  case parser.parse(source, parser.Script) {
    Ok(_) -> "PASS (GOOD)"
    Error(_) -> "FAIL (BAD - should pass)"
  }
}

pub fn scope_test() {
  // Cross-statement duplicates (should fail)
  echo "let a; let a; => " <> should_fail("let a; let a;")
  echo "let a; var a; => " <> should_fail("let a; var a;")
  echo "var a; let a; => " <> should_fail("var a; let a;")
  echo "{ var a; const a = 1; } => " <> should_fail("{ var a; const a = 1; }")
  echo "{ let a; { var a; } } => " <> should_fail("{ let a; { var a; } }")

  // Function param + body
  echo "function a(b){ let b; } => " <> should_fail("function a(b){ let b; }")
  echo "(a) => { let a; } => " <> should_fail("(a) => { let a; }")

  // Switch cases share scope
  echo "switch case dup => "
    <> should_fail("switch(1) { case 2: let a; case 3: let a; }")

  // Catch clause
  echo "catch dup => " <> should_fail("try {} catch(a) { let a; }")

  // for destructuring (should already work)
  echo "for const {a,a} => " <> should_fail("for(const {a, a} of 1);")

  // Block-level function declarations - duplicates should fail
  echo "{ func a; func a; } => "
    <> should_fail("{ function a(){} function a(){} }")
  echo "{ func a; func* a; } => "
    <> should_fail("{ function a(){} function* a(){} }")

  // Valid cases that should pass
  echo "func a; func a; (sloppy) => "
    <> should_pass("function a() {} function a() {}")
  echo "func a(b,b) (sloppy) => " <> should_pass("function a(b, b) {}")
  echo "func a([b]){var b} => " <> should_pass("function a([b]) { var b; }")
  echo "var a; var a; => " <> should_pass("var a; var a;")
  echo "for(let a;;); let a; => " <> should_pass("for(let a;;); let a;")
}
