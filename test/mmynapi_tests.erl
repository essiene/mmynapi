-module(mmynapi_tests).
-include("../include/mmynapi.hrl").
-include_lib("eunit/include/eunit.hrl").

to_json_test_() ->
	[
        {"Convert '#mmyn.message{}' to JSON document",
            ?_assertEqual(<<"{\"header\":{\"vsn\":\"2.0.1\",\"type\":\"req.sendsms\",\"system\":\"mmyn\",\"transaction_id\":\"0xdeadbeef\"},\"body\":{\"sender\":\"ASENDER\",\"msisdn\":\"+23481618\",\"message\":\"a dumb message\"}}">>,
                mmynapi:to_json(#'mmyn.message'{
                        h=#'mmyn.header'{ 
                            vsn= <<"2.0.1">>, type= <<"req.sendsms">>, 
                            system= <<"mmyn">>, transaction_id= <<"0xdeadbeef">>},
                        b=#'req.sendsms'{
                            sender= <<"ASENDER">>, msisdn= <<"+23481618">>, 
                            message= <<"a dumb message">>}}))},
        {"Convert #'req.sendsms'{} to JSON via mmynapi:to_json/3",
            ?_assertEqual(<<"{\"header\":{\"vsn\":\"2.0.1\",\"type\":\"req.sendsms\",\"system\":\"mmyn\",\"transaction_id\":\"0xdeadbeef\"},\"body\":{\"sender\":\"ASENDER\",\"msisdn\":\"+23481618\",\"message\":\"a dumb message\"}}">>,
                mmynapi:to_json('mmyn', '0xdeadbeef', #'req.sendsms'{
                            sender= <<"ASENDER">>, msisdn= <<"+23481618">>, 
                            message= <<"a dumb message">>}))},
        {"Convert #'res.sendsms'{} to JSON via mmynapi:to_json/3",
            ?_assertEqual(<<"{\"header\":{\"vsn\":\"2.0.1\",\"type\":\"res.sendsms\",\"system\":\"mmyn\",\"transaction_id\":\"0xdeadbeef\"},\"body\":{\"status\":0,\"detail\":\"All Okay\"}}">>,
                mmynapi:to_json('mmyn', '0xdeadbeef', #'res.sendsms'{
                            status=0, detail= <<"All Okay">>}))},
        {"Convert #'req.reply'{} to JSON via mmynapi:to_json/3",
            ?_assertEqual(<<"{\"header\":{\"vsn\":\"2.0.1\",\"type\":\"req.reply\",\"system\":\"mmyn\",\"transaction_id\":\"0xdeadbeef\"},\"body\":{\"id\":\"0xcafebabe\",\"sender\":\"ASENDER\",\"msisdn\":\"+23481618\",\"message\":\"a dumb message\"}}">>,
                mmynapi:to_json('mmyn', '0xdeadbeef', #'req.reply'{
                            id= <<"0xcafebabe">>, sender= <<"ASENDER">>, msisdn= <<"+23481618">>, 
                            message= <<"a dumb message">>}))},
        {"Convert #'res.reply'{} to JSON via mmynapi:to_json/3",
            ?_assertEqual(<<"{\"header\":{\"vsn\":\"2.0.1\",\"type\":\"res.reply\",\"system\":\"mmyn\",\"transaction_id\":\"0xdeadbeef\"},\"body\":{\"status\":0,\"detail\":\"All Okay\"}}">>,
                mmynapi:to_json('mmyn', '0xdeadbeef', #'res.reply'{
                            status=0, detail= <<"All Okay">>}))},
        {"Convert #'res.notify'{} to JSON via mmynapi:to_json/3",
            ?_assertEqual(<<"{\"header\":{\"vsn\":\"2.0.1\",\"type\":\"res.notify\",\"system\":\"mmyn\",\"transaction_id\":\"0xdeadbeef\"},\"body\":{\"status\":0,\"detail\":\"All Okay\",\"wait_for_reply\":false,\"ttl\":60}}">>,
                mmynapi:to_json('mmyn', '0xdeadbeef', #'res.notify'{
                            status=0, detail= <<"All Okay">>,
                            wait_for_reply=false, ttl=60}))}
   ].

