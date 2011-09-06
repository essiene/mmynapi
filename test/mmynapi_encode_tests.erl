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
            ?_assertEqual(null, mmynapi_encode:to_json_form(null))},
        {"Convert Binary to Binary",
            ?_assertEqual(<<"foobar">>, mmynapi_encode:to_json_form(<<"foobar">>))},
        {"Convert List items properly",
            ?_assertEqual([1, <<"foobar">>, false, null, <<"really-now?">>], mmynapi_encode:to_json_form([1,<<"foobar">>, false, null, 'really-now?']))},
        {"Convert '#res.notify{}' record",
            ?_assertEqual(
                {[
                        {<<"status">>, 0},
                        {<<"detail">>, <<"All okay">>},
                        {<<"wait_for_reply">>, true},
                        {<<"ttl">>, 30}]},
                mmynapi_encode:to_json_form(#'res.notify'{
                        status=0,detail = <<"All okay">>,
                        wait_for_reply=true, ttl=30}))},
        {"Convert '#req.notify{}' record",
            ?_assertEqual(
                {[
                        {<<"id">>, <<"0xdeadbeef">>},
                        {<<"shortcode">>, 4000},
                        {<<"keyword">>, <<"akeyword">>},
                        {<<"msisdn">>, <<"+1234567">>},
                        {<<"message">>, <<"some dumb message">>},
                        {<<"max_ttl">>, 30}]},
                mmynapi_encode:to_json_form(#'req.notify'{
                        id= <<"0xdeadbeef">>, shortcode=4000,
                        keyword= <<"akeyword">>, msisdn= <<"+1234567">>,
                        message= <<"some dumb message">>, max_ttl=30}))},
        {"Convert '#res.reply{}' record",
            ?_assertEqual(
                {[
                        {<<"status">>, 0},
                        {<<"detail">>, <<"All okay">>}]},
                mmynapi_encode:to_json_form(#'res.reply'{
                        status=0,detail = <<"All okay">>}))},
        {"Convert '#req.reply{}' record",
            ?_assertEqual(
                {[
                        {<<"id">>, <<"0xdeadbeef">>},
                        {<<"sender">>, <<"ASENDER">>},
                        {<<"msisdn">>, <<"+123456">>},
                        {<<"message">>, <<"a dumb message">>}]},
                mmynapi_encode:to_json_form(#'req.reply'{
                        id= <<"0xdeadbeef">>, sender= <<"ASENDER">>,
                        msisdn= <<"+123456">>, message= <<"a dumb message">>}))}
   ].

