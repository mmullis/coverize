-module(simple_test).

-author("michael@mullistechnologies.com").

-spec test() -> any().
-include_lib("eunit/include/eunit.hrl").

-spec basic_test_() -> [{integer(),fun(() -> 'ok')},...].
basic_test_() ->
  [?_assertEqual(" ", " " )
  ].

-spec mod_summary_file_test_() -> [{integer(),fun(() -> 'ok')},...].
mod_summary_file_test_() ->
    F = fun coverize:module_summary_file/1,
    % With top level erlang modules
    [?_assertEqual("lists.COVER.html", F(lists)),
     ?_assertEqual("q.COVER.html", F(q)),
    % With a module contained within an erlang package
     ?_assertEqual("foo.bar.COVER.html", F(foo.bar)),
     ?_assertEqual("a.b_c.COVER.html", F(a.b_c))
    ].
