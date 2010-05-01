#!/usr/bin/env escript
%%% File    : cover.sh
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% Description : Wrapper for running coverize from the command line
%%% Created : 09 Mar 2010 by Michael Mullis <michael@mullistechnologies.com>

-author("michael@mullistechnologies.com").
%% TODO: the SourceDirsString tokenization is flakey with different apos combos

-spec main(any()) -> any().
main([SourceDirsString, Module, Function, OutputDir])  ->
    code:add_path("./ebin"),
    Tokens = string:tokens(SourceDirsString, "'"),
    StripFun = fun (E) -> string:strip(E) end,
    SourceDirs = lists:map(StripFun, Tokens),
    io:format("Running Coverize ~p ~p:~p OutputDir=~p",[SourceDirs, Module, Function,OutputDir]),
    CompileOptions = [ debug_info,
                       { i, "./include" },
                       { outdir, "./ebin" }],
    coverize:run(SourceDirs,CompileOptions,list_to_atom(Module),list_to_atom(Function), OutputDir);

main(Args)->
    io:format("Usage: ./cover.sh \"'source/dir/1' 'source/dir/1'\" Module Function~n"),
    io:format("Args passed: ~p",[Args]).
