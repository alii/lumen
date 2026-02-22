# lumen

(Highly experimental) JavaScript on the BEAM

## test262 Conformance

![test262 conformance chart](.github/test262/conformance.png)

Conformance is measured daily against the full [test262](https://github.com/nicolo-ribaudo/tc39-test262) suite by compiling and executing each test through lumen's parser, compiler, and VM.

## Development

```sh
gleam test  # Run the tests
```

### Running test262

```sh
# Run the full test262 execution suite
TEST262_EXEC=1 gleam test

# Also write results to a JSON file
TEST262_EXEC=1 RESULTS_FILE=results.json gleam test

# Run parser-only test262 (faster, parse conformance only)
TEST262=1 gleam test
```
