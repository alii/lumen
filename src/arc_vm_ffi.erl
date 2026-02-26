-module(arc_vm_ffi).
-export([float_power/2, math_sqrt/1, math_log/1, math_sin/1, math_cos/1, math_floor/1, math_ceil/1, read_line/1]).
-export([array_from_list/1, array_to_list/1, array_get/2, array_set/3, array_size/1, array_repeat/2, array_grow/3]).
-export([spawn_process/1, self_pid/0, send_message/2, receive_message_infinite/0, receive_message_timeout/1, pid_to_string/1]).
-export([get_script_args/0, sleep/1]).

float_power(Base, Exp) ->
    math:pow(Base, Exp).

math_sqrt(X) -> math:sqrt(X).
math_log(X) -> math:log(X).
math_sin(X) -> math:sin(X).
math_cos(X) -> math:cos(X).
math_floor(X) -> math:floor(X).
math_ceil(X) -> math:ceil(X).

read_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> {error, nil};
        {error, _} -> {error, nil};
        Line -> {ok, Line}
    end.

%% Array (tuple-backed) operations
array_from_list(List) -> list_to_tuple(List).
array_to_list(Tuple) -> tuple_to_list(Tuple).
array_get(Index, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {some, element(Index + 1, Tuple)};
        false -> none
    end.
array_set(Index, Value, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {ok, setelement(Index + 1, Tuple, Value)};
        false -> {error, nil}
    end.
array_size(Tuple) -> tuple_size(Tuple).
array_repeat(Value, Count) -> erlang:make_tuple(Count, Value).

%% Grow a tuple to NewSize, filling new slots with Default.
%% If NewSize =< current size, returns Tuple unchanged.
%% O(NewSize) — converts to list, pads, converts back. No repeated setelement.
array_grow(Tuple, NewSize, Default) ->
    Old = tuple_size(Tuple),
    case NewSize =< Old of
        true -> Tuple;
        false ->
            Pad = lists:duplicate(NewSize - Old, Default),
            list_to_tuple(tuple_to_list(Tuple) ++ Pad)
    end.

%% Process primitives for Arc.spawn/send/receive/self
spawn_process(Fun) -> spawn(Fun).
self_pid() -> self().
send_message(Pid, Msg) -> Pid ! Msg, ok.
receive_message_infinite() ->
    receive Msg -> Msg end.
receive_message_timeout(Timeout) ->
    receive Msg -> {ok, Msg}
    after Timeout -> {error, nil}
    end.
pid_to_string(Pid) -> list_to_binary(pid_to_list(Pid)).
get_script_args() -> [list_to_binary(A) || A <- init:get_plain_arguments()].
sleep(Ms) -> timer:sleep(Ms), nil.
