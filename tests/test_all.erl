-module (test_all).

-include_lib ("eunit/include/eunit.hrl").


all_test_() ->
  [{module, test_erlguten},
  {module, test_erlguten_afm}].