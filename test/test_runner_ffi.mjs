import { Error } from "../gleam.mjs";

export function generate_eunit_tests(_dir, _test_fn) {
  return new Error("EUnit tests are only available on the Erlang target");
}
