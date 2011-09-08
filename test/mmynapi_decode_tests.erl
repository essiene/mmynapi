-module(mmynapi_decode_tests).
-include("../include/mmynapi.hrl").
-include_lib("eunit/include/eunit.hrl").

decode_header_test_() ->
	[
        {"Convert a correct header in JSON form to #'mmyn.header'{} record",
            ?_assertEqual({ok, #'mmyn.header'{vsn=?MMYN_MSG_VSN, type= <<"req.sendsms">>, 
                        system= <<"mmyn">>, transaction_id= <<"0xdeadbeef">>}}, 
                        mmynapi_decode:to_header({[ 
                                    {<<"vsn">>, [2,0,1]}, 
                                    {<<"type">>, <<"req.sendsms">>}, 
                                    {<<"system">>, <<"mmyn">>}, 
                                    {<<"transaction_id">>, <<"0xdeadbeef">>}]}))},
        {"Error out when header JSON form is missing a 'vsn' field",
            ?_assertEqual({error, no_version}, 
                        mmynapi_decode:to_header({[ 
                                    {<<"type">>, <<"req.sendsms">>}, 
                                    {<<"system">>, <<"mmyn">>}, 
                                    {<<"transaction_id">>, <<"0xdeadbeef">>}]}))},
        {"Error out when header 'vsn' field is wrong",
            ?_assertEqual({error, {wrong_msg_vsn, [{current_msg_vsn, [2,0,1]}, {parsed_msg_vsn,[3,2,1]}]}}, 
                        mmynapi_decode:to_header({[ 
                                    {<<"type">>, <<"req.sendsms">>}, 
                                    {<<"vsn">>, [3,2,1]},
                                    {<<"system">>, <<"mmyn">>}, 
                                    {<<"transaction_id">>, <<"0xdeadbeef">>}]}))},
        {"Error out when header JSON form is missing a 'type' field",
            ?_assertEqual({error, no_type}, 
                        mmynapi_decode:to_header({[ 
                                    {<<"vsn">>, [2,0,1]}, 
                                    {<<"system">>, <<"mmyn">>}, 
                                    {<<"transaction_id">>, <<"0xdeadbeef">>}]}))},
        {"Error out when header JSON form is missing a 'system' field",
            ?_assertEqual({error, no_system}, 
                        mmynapi_decode:to_header({[ 
                                    {<<"vsn">>, [2,0,1]}, 
                                    {<<"type">>, <<"res.sendsms">>},
                                    {<<"transaction_id">>, <<"0xdeadbeef">>}]}))},
        {"Error out when header JSON form is missing a 'transaction_id' field",
            ?_assertEqual({error, no_transaction_id}, 
                        mmynapi_decode:to_header({[ 
                                    {<<"vsn">>, [2,0,1]}, 
                                    {<<"system">>, <<"mmyn">>}, 
                                    {<<"type">>, <<"res.sendsms">>}]}))} 
    ].


decode_body_test_() ->
	[
        {"Convert a correct req.sendsms in JSON form to #'req.sendsms'{} record",
            ?_assertEqual({ok, #'req.sendsms'{
                        sender= <<"ASENDER">>, 
                        msisdn= [<<"+123456">>,<<"+2345678">>], 
                        message= <<"a dumb message">>}}, 
                        mmynapi_decode:to_body(<<"req.sendsms">>, {[ 
                                    {<<"sender">>, <<"ASENDER">>}, 
                                    {<<"msisdn">>, [<<"+123456">>, <<"+2345678">>]}, 
                                    {<<"message">>, <<"a dumb message">>}]}))},
        {"Error out when req.sendsms JSON form is missing a 'sender' field",
            ?_assertEqual({error, no_sender}, 
                        mmynapi_decode:to_body(<<"req.sendsms">>, {[ 
                                    {<<"msisdn">>, [<<"+123456">>, <<"+2345678">>]}, 
                                    {<<"message">>, <<"a dumb message">>}]}))},
        {"Error out when req.sendsms JSON form is missing a 'msisdn' field",
            ?_assertEqual({error, no_msisdn}, 
                        mmynapi_decode:to_body(<<"req.sendsms">>, {[ 
                                    {<<"sender">>, <<"ASENDER">>}, 
                                    {<<"message">>, <<"a dumb message">>}]}))},
        {"Error out when req.sendsms JSON form is missing a 'message' field",
            ?_assertEqual({error, no_message}, 
                        mmynapi_decode:to_body(<<"req.sendsms">>, {[ 
                                    {<<"sender">>, <<"ASENDER">>}, 
                                    {<<"msisdn">>, [<<"+123456">>, <<"+2345678">>]}]}))}
    ].

