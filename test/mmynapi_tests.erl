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
                            message= <<"a dumb message">>}}))}
   ].

