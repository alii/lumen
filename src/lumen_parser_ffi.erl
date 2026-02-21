-module(lumen_parser_ffi).
-export([parse_float/1]).

parse_float(S) ->
    try
        {ok, erlang:binary_to_float(S)}
    catch
        error:badarg ->
            %% Try adding .0 for integer-like strings with exponent
            try
                {ok, erlang:binary_to_float(<<S/binary, ".0">>)}
            catch
                error:badarg -> {error, nil}
            end
    end.
