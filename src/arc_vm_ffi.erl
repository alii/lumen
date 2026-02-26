-module(arc_vm_ffi).
-export([read_line/1]).
-export([array_get/2, array_set/3, array_repeat/2, array_grow/3]).
-export([send_message/2, receive_message_infinite/0, receive_message_timeout/1, pid_to_string/1]).
-export([get_script_args/0, sleep/1]).

read_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> {error, nil};
        {error, _} -> {error, nil};
        Line -> {ok, Line}
    end.

%% Array (tuple-backed) operations
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

%% Process primitives for Arc.send/receive
send_message(Pid, Msg) -> Pid ! Msg, nil.
receive_message_infinite() ->
    receive Msg -> Msg end.
receive_message_timeout(Timeout) ->
    receive Msg -> {ok, Msg}
    after Timeout -> {error, nil}
    end.
pid_to_string(Pid) -> list_to_binary(pid_to_list(Pid)).
get_script_args() -> [list_to_binary(A) || A <- init:get_plain_arguments()].
sleep(Ms) -> timer:sleep(Ms), nil.
