-module(simple_test).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [?_assertEqual(" ", " " )
  ].

