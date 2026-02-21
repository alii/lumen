/// Generic file-based test runner for EUnit.
/// Reads all .js files from a directory and runs a callback on each one,
/// generating individual EUnit test descriptors.
/// Opaque type representing a list of EUnit test descriptors.
pub type EunitTests

/// Generate EUnit test descriptors for all .js files in a directory.
///
/// `test_fn` receives `(filename, source)` and should return
/// `Ok(Nil)` on pass or `Error(reason)` on failure.
pub fn generate_file_tests(
  dir: String,
  test_fn: fn(String, String) -> Result(Nil, String),
) -> EunitTests {
  case generate_eunit_tests_ffi(dir, test_fn) {
    Ok(tests) -> tests
    Error(err) ->
      panic as { "Failed to generate tests for " <> dir <> ": " <> err }
  }
}

@external(erlang, "test_runner_ffi", "generate_eunit_tests")
@external(javascript, "../test/test_runner_ffi.mjs", "generate_eunit_tests")
fn generate_eunit_tests_ffi(
  dir: String,
  test_fn: fn(String, String) -> Result(Nil, String),
) -> Result(EunitTests, String)
