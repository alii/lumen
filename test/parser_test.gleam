/// Test262 parser conformance tests.
/// Each test262 .js file becomes an individual EUnit test descriptor,
/// so the output shows real pass/fail counts per file.
import gleam/list
import gleam/string
import lumen/parser
import test_runner

/// Tests in the fail/ directory that are outdated due to spec changes.
/// These expect parse errors for syntax that is now valid in modern JS.
const outdated_fail_tests = [
  // Class fields (ES2022) — class body with bare field name or field initializer
  // is valid syntax now, but test262-parser-tests predates this spec addition.
  "98204d734f8c72b3.js",
  "ef81b93cf9bdb4ec.js",
]

/// Determine parse mode from filename — files with ".module." parse as module.
fn parse_mode(filename: String) -> parser.ParseMode {
  case string.contains(filename, ".module.") {
    True -> parser.Module
    False -> parser.Script
  }
}

/// For "pass" tests: parsing should succeed.
fn pass_test_fn(filename: String, source: String) -> Result(Nil, String) {
  let mode = parse_mode(filename)
  case parser.parse(source, mode) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(parser.parse_error_to_string(err))
  }
}

/// For "fail" tests: parsing should produce an error.
/// Outdated tests (valid in modern JS) are skipped.
fn fail_test_fn(filename: String, source: String) -> Result(Nil, String) {
  case list.contains(outdated_fail_tests, filename) {
    True -> Ok(Nil)
    False -> {
      let mode = parse_mode(filename)
      case parser.parse(source, mode) {
        Error(_) -> Ok(Nil)
        Ok(_) -> Error("Expected parse error, got success")
      }
    }
  }
}

/// For "early" tests: parsing should produce an early error.
fn early_test_fn(filename: String, source: String) -> Result(Nil, String) {
  let mode = parse_mode(filename)
  case parser.parse(source, mode) {
    Error(_) -> Ok(Nil)
    Ok(_) -> Error("Expected early error, got success")
  }
}

/// EUnit test generator for the "pass" suite.
pub fn pass_test_() {
  test_runner.generate_file_tests(
    "vendor/test262-parser-tests/pass",
    pass_test_fn,
  )
}

/// EUnit test generator for the "fail" suite.
pub fn fail_test_() {
  test_runner.generate_file_tests(
    "vendor/test262-parser-tests/fail",
    fail_test_fn,
  )
}

/// EUnit test generator for the "early" suite.
pub fn early_test_() {
  test_runner.generate_file_tests(
    "vendor/test262-parser-tests/early",
    early_test_fn,
  )
}
