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
        {"Convert '#mmyn.fault{}' record",
            ?_assertEqual(
                {[
                        {<<"code">>, 0},
                        {<<"detail">>, <<"All okay">>}]},
                mmynapi_encode:to_json_form(#'mmyn.fault'{
                        code=0,detail = <<"All okay">>}))},
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
        {"Convert '#req.notify{}' record, with single keyword",
            ?_assertEqual(
                {[
                        {<<"id">>, <<"0xdeadbeef">>},
                        {<<"shortcode">>, 4000},
                        {<<"keywords">>, [<<"akeyword">>]},
                        {<<"msisdn">>, <<"+1234567">>},
                        {<<"message">>, <<"some dumb message">>},
                        {<<"max_ttl">>, 30}]},
                mmynapi_encode:to_json_form(#'req.notify'{
                        id= <<"0xdeadbeef">>, shortcode=4000,
                        keywords= [<<"akeyword">>], msisdn= <<"+1234567">>,
                        message= <<"some dumb message">>, max_ttl=30}))},
        {"Convert '#req.notify{}' record, with multiple keywords",
            ?_assertEqual(
                {[
                        {<<"id">>, <<"0xdeadbeef">>},
                        {<<"shortcode">>, 4000},
                        {<<"keywords">>, [<<"kwd1">>, <<"kwd2">>]},
                        {<<"msisdn">>, <<"+1234567">>},
                        {<<"message">>, <<"some dumb message">>},
                        {<<"max_ttl">>, 30}]},
                mmynapi_encode:to_json_form(#'req.notify'{
                        id= <<"0xdeadbeef">>, shortcode=4000,
                        keywords= [<<"kwd1">>, <<"kwd2">>], msisdn= <<"+1234567">>,
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
                        msisdn= <<"+123456">>, message= <<"a dumb message">>}))},
        {"Convert '#res.sendsms{}' record",
            ?_assertEqual(
                {[
                        {<<"status">>, 0},
                        {<<"detail">>, <<"All okay">>}]},
                mmynapi_encode:to_json_form(#'res.sendsms'{
                        status=0,detail = <<"All okay">>}))},
        {"Convert '#req.sendsms{}' record with SINGLE MSISDN",
            ?_assertEqual(
                {[
                        {<<"sender">>, <<"ASENDER">>},
                        {<<"msisdn">>, <<"+123456">>},
                        {<<"message">>, <<"a dumb message">>}]},
                mmynapi_encode:to_json_form(#'req.sendsms'{
                        sender= <<"ASENDER">>, msisdn= <<"+123456">>, 
                        message= <<"a dumb message">>}))},
        {"Convert '#req.sendsms{}' record with MULTIPLE MSISDN",
            ?_assertEqual(
                {[
                        {<<"sender">>, <<"ASENDER">>},
                        {<<"msisdn">>, [<<"+23481618">>, <<"+123456">>]},
                        {<<"message">>, <<"a dumb message">>}]},
                mmynapi_encode:to_json_form(#'req.sendsms'{
                        sender= <<"ASENDER">>, msisdn= [<<"+23481618">>, <<"+123456">>], 
                        message= <<"a dumb message">>}))},
        {"Convert '#mmyn.header{}' record",
            ?_assertEqual(
                {[
                        {<<"vsn">>, [2,0,1]},
                        {<<"type">>, <<"res.sendsms">>},
                        {<<"system">>, <<"mmyn">>},
                        {<<"transaction_id">>, <<"0xdeadbeef">>}]},
                mmynapi_encode:to_json_form(#'mmyn.header'{
                        vsn=[2,0,1], type= <<"res.sendsms">>,
                        system= <<"mmyn">>, transaction_id= <<"0xdeadbeef">>}))},
        {"Convert '#mmyn.message{}' record",
            ?_assertEqual(
                {[
                        {<<"header">>, 
                            {[
                            {<<"vsn">>, [2,0,1]},
                            {<<"type">>, <<"req.sendsms">>},
                            {<<"system">>, <<"mmyn">>},
                            {<<"transaction_id">>, <<"0xdeadbeef">>}]}}, 
                        {<<"body">>, 
                            {[ 
                            {<<"sender">>, <<"ASENDER">>}, 
                            {<<"msisdn">>, <<"+23481618">>},
                            {<<"message">>, <<"a dumb message">>}]}}]},
                mmynapi_encode:to_json_form(#'mmyn.message'{
                        h=#'mmyn.header'{ 
                            vsn= [2,0,1], type= <<"req.sendsms">>, 
                            system= <<"mmyn">>, transaction_id= <<"0xdeadbeef">>},
                        b=#'req.sendsms'{
                            sender= <<"ASENDER">>, msisdn= <<"+23481618">>, 
                            message= <<"a dumb message">>}}))}
   ].

