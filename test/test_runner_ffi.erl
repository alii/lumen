-module(test_runner_ffi).

-export([list_files/1, generate_eunit_tests/2]).

%% List all .js files in a directory, returning their filenames (not full paths).
list_files(Dir) ->
    DirStr = binary_to_list(Dir),
    case file:list_dir(DirStr) of
        {ok, Files} ->
            JsFiles = [list_to_binary(F)
                       || F <- lists:sort(Files),
                          lists:suffix(".js", F)],
            {ok, JsFiles};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Generate EUnit test descriptors — one per .js file in a directory.
%% Pre-reads all files upfront so test funs only run the callback (no disk I/O).
%%
%% TestFn: fun(Filename, Source) -> {ok, nil} | {error, Reason}
%%   where Filename and Source are binaries, Reason is a binary.
%%
%% Returns {ok, {inparallel, [{TestName, Fun}]}} for use as EUnit test generators.
generate_eunit_tests(Dir, TestFn) ->
    DirStr = binary_to_list(Dir),
    case file:list_dir(DirStr) of
        {ok, AllFiles} ->
            JsFiles = [F || F <- lists:sort(AllFiles), lists:suffix(".js", F)],
            %% Pre-read all files
            FilesWithSource = lists:filtermap(
                fun(Filename) ->
                    FilePath = filename:join(DirStr, Filename),
                    case file:read_file(FilePath) of
                        {ok, Source} -> {true, {Filename, Source}};
                        {error, _} -> false
                    end
                end,
                JsFiles
            ),
            Tests = lists:map(
                fun({Filename, Source}) ->
                    FilenameBin = list_to_binary(Filename),
                    TestName = binary_to_list(FilenameBin),
                    {TestName, fun() ->
                        case TestFn(FilenameBin, Source) of
                            {ok, nil} -> ok;
                            {error, Reason} ->
                                erlang:error({assertion_failed,
                                    [{file, FilenameBin},
                                     {reason, Reason}]})
                        end
                    end}
                end,
                FilesWithSource
            ),
            {ok, {inparallel, Tests}};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.
