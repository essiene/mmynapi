-module(mmynapi_encode_tests).
-include("../include/mmynapi.hrl").
-include_lib("eunit/include/eunit.hrl").

to_json_form_test_() ->
	[
		{"Convert an ATOM to a binary",
            ?_assertEqual(<<"ok">>, mmynapi_encode:to_json_form(ok))},
		{"Convert an Integer to an Integer",
            ?_assertEqual(123, mmynapi_encode:to_json_form(123))},
        {"Convert a Float to a Float",
            ?_assertEqual(123.4, mmynapi_encode:to_json_form(123.4))},
        {"Convert 'true' to 'true'",
                ?_assertEqual(true, mmynapi_encode:to_json_form(true))},
        {"Convert 'false' to 'false'",
                ?_assertEqual(false, mmynapi_encode:to_json_form(false))},
        {"Convert 'null' to 'null'",
            ?_assertEqual(null, mmynapi_encode:to_json_form(null))}
   ].

