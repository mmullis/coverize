-module(test_suite).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, simple_test}
  ].
