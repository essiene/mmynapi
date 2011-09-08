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
                                    {<<"transaction_id">>, <<"0xdeadbeef">>}]}))}
   ].

