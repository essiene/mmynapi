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
                                    {<<"msisdn">>, [<<"+123456">>, <<"+2345678">>]}]}))},
        %%
        %% res.sendsms tests start from here
        %%
        {"Convert a correct res.sendsms in JSON form to #'res.sendsms'{} record",
            ?_assertEqual({ok, #'res.sendsms'{
                        status= 15,
                        detail= <<"oops! my bad...">>}}, 
                        mmynapi_decode:to_body(<<"res.sendsms">>, {[ 
                                    {<<"status">>, 15}, 
                                    {<<"detail">>, <<"oops! my bad...">>}]}))},
        {"Error out when res.sendsms JSON form is missing a 'status' field",
            ?_assertEqual({error, no_status}, 
                        mmynapi_decode:to_body(<<"res.sendsms">>, {[{<<"detail">>, <<"oops! my bad...">>}]}))},
        {"Error out when res.sendsms JSON form is missing a 'detail' field",
            ?_assertEqual({error, no_detail}, 
                        mmynapi_decode:to_body(<<"res.sendsms">>, {[{<<"status">>, 15}]}))},
        %%
        %% req.reply tests start from here
        %%
        {"Convert a correct req.reply in JSON form to #'req.reply'{} record",
            ?_assertEqual({ok, #'req.reply'{
                        id= <<"0xcafebabe">>,
                        sender= <<"ASENDER">>, 
                        msisdn= <<"+123456">>, 
                        message= <<"a dumb message">>}}, 
                        mmynapi_decode:to_body(<<"req.reply">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"sender">>, <<"ASENDER">>}, 
                                    {<<"msisdn">>, <<"+123456">>}, 
                                    {<<"message">>, <<"a dumb message">>}]}))},
        {"Error out when req.reply JSON form is missing a 'id' field",
            ?_assertEqual({error, no_id}, 
                        mmynapi_decode:to_body(<<"req.reply">>, {[ 
                                    {<<"sender">>, <<"ASENDER">>},
                                    {<<"msisdn">>, <<"+123456">>}, 
                                    {<<"message">>, <<"a dumb message">>}]}))},
        {"Error out when req.reply JSON form is missing a 'sender' field",
            ?_assertEqual({error, no_sender}, 
                        mmynapi_decode:to_body(<<"req.reply">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"msisdn">>, <<"+123456">>}, 
                                    {<<"message">>, <<"a dumb message">>}]}))},
        {"Error out when req.reply JSON form is missing a 'msisdn' field",
            ?_assertEqual({error, no_msisdn}, 
                        mmynapi_decode:to_body(<<"req.reply">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"sender">>, <<"ASENDER">>}, 
                                    {<<"message">>, <<"a dumb message">>}]}))},
        {"Error out when req.reply JSON form is missing a 'message' field",
            ?_assertEqual({error, no_message}, 
                        mmynapi_decode:to_body(<<"req.reply">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"sender">>, <<"ASENDER">>}, 
                                    {<<"msisdn">>, <<"+123456">>}]}))},
        %%
        %% res.reply tests start from here
        %%
        {"Convert a correct res.reply in JSON form to #'res.reply'{} record",
            ?_assertEqual({ok, #'res.reply'{
                        status= 15,
                        detail= <<"oops! my bad...">>}}, 
                        mmynapi_decode:to_body(<<"res.reply">>, {[ 
                                    {<<"status">>, 15}, 
                                    {<<"detail">>, <<"oops! my bad...">>}]}))},
        {"Error out when res.reply JSON form is missing a 'status' field",
            ?_assertEqual({error, no_status}, 
                        mmynapi_decode:to_body(<<"res.reply">>, {[{<<"detail">>, <<"oops! my bad...">>}]}))},
        {"Error out when res.reply JSON form is missing a 'detail' field",
            ?_assertEqual({error, no_detail}, 
                        mmynapi_decode:to_body(<<"res.reply">>, {[{<<"status">>, 15}]}))},
        %%
        %% req.notify tests start from here
        %%

        {"Convert a correct req.notify in JSON form to #'req.notify'{} record",
            ?_assertEqual({ok, #'req.notify'{
                        id= <<"0xcafebabe">>,
                        shortcode = 5999,
                        keywords = [<<"kwd1">>, <<"kwd2">>],
                        msisdn= <<"+123456">>, 
                        message= <<"a dumb message">>,
                        max_ttl= 60}}, 
                        mmynapi_decode:to_body(<<"req.notify">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"shortcode">>, 5999}, 
                                    {<<"keywords">>, [<<"kwd1">>, <<"kwd2">>]},
                                    {<<"msisdn">>, <<"+123456">>},
                                    {<<"message">>, <<"a dumb message">>},
                                    {<<"max_ttl">>, 60}]}))},
        {"Error out when req.notify JSON form is missing a 'sender' field",
            ?_assertEqual({error, no_id}, 
                        mmynapi_decode:to_body(<<"req.notify">>, {[ 
                                    {<<"shortcode">>, 5999}, 
                                    {<<"keywords">>, [<<"kwd1">>, <<"kwd2">>]},
                                    {<<"msisdn">>, <<"+123456">>},
                                    {<<"message">>, <<"a dumb message">>},
                                    {<<"max_ttl">>, 60}]}))},
        {"Error out when req.notify JSON form is missing a 'shortcode' field",
            ?_assertEqual({error, no_shortcode}, 
                        mmynapi_decode:to_body(<<"req.notify">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"keywords">>, [<<"kwd1">>, <<"kwd2">>]},
                                    {<<"msisdn">>, <<"+123456">>},
                                    {<<"message">>, <<"a dumb message">>},
                                    {<<"max_ttl">>, 60}]}))},
        {"Error out when req.notify JSON form is missing a 'keywords' field",
            ?_assertEqual({error, no_keywords}, 
                        mmynapi_decode:to_body(<<"req.notify">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"shortcode">>, 5999}, 
                                    {<<"msisdn">>, <<"+123456">>},
                                    {<<"message">>, <<"a dumb message">>},
                                    {<<"max_ttl">>, 60}]}))},
        {"Error out when req.notify JSON form is missing a 'msisdn' field",
            ?_assertEqual({error, no_msisdn}, 
                        mmynapi_decode:to_body(<<"req.notify">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"shortcode">>, 5999}, 
                                    {<<"keywords">>, [<<"kwd1">>, <<"kwd2">>]},
                                    {<<"message">>, <<"a dumb message">>},
                                    {<<"max_ttl">>, 60}]}))},
        {"Error out when req.notify JSON form is missing a 'message' field",
            ?_assertEqual({error, no_message}, 
                        mmynapi_decode:to_body(<<"req.notify">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"shortcode">>, 5999}, 
                                    {<<"keywords">>, [<<"kwd1">>, <<"kwd2">>]},
                                    {<<"msisdn">>, <<"+123456">>},
                                    {<<"max_ttl">>, 60}]}))},
        {"Error out when req.notify JSON form is missing a 'max_ttl' field",
            ?_assertEqual({error, no_max_ttl}, 
                        mmynapi_decode:to_body(<<"req.notify">>, {[ 
                                    {<<"id">>, <<"0xcafebabe">>},
                                    {<<"shortcode">>, 5999}, 
                                    {<<"keywords">>, [<<"kwd1">>, <<"kwd2">>]},
                                    {<<"msisdn">>, <<"+123456">>},
                                    {<<"message">>, <<"a dumb message">>}]}))}
    ].

