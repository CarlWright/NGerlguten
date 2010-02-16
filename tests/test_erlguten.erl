-module (test_erlguten).

-include_lib ("eunit/include/eunit.hrl").

crazy_test_() ->
    ?_assert(1 + 1 =:= 2).
    
crazy2_test() ->
    ?_assert(1 + 1 =:= 3).
    
crazy3_test_() ->
    ?_assert( false ).