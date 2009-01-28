%%% File    : eunit_helper.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% Description : Some helper functions to enhance tests and coverage analysis
%%% Created : 27 Jan 2009 by Michael Mullis <michael@mullistechnologies.com>

-module(eunit_helper).
-author("michael@mullistechnologies.com").
-compile([export_all]).  % its just for testing so dont blow a gasket!

%% Wrapper for cover to make command line calling easy
run_cover() ->
   %% TODO: Make this list dynamic
  SourceDirs = [
    "./test/src"
  ],

  coverize:run(SourceDirs,test_suite).
