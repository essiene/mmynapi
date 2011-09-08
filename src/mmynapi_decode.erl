-module(mmynapi_decode).
-export([decode_message/1, decode_header/1, decode_body/2]).
-include("mmynapi.hrl").


decode_message({PropList}) ->
    case proplists:get_value(<<"header">>, PropList) of
        undefined ->
            {error, no_header};
        Header ->
            case decode_header(Header) of 
                {error, Reason} ->
                    {error, Reason};
                {ok, #'mmyn.header'{type=MsgType}=Hrecord} -> 
                    case proplists:get_value(<<"body">>, PropList) of 
                        undefined -> 
                            {error, no_body}; 
                        Body -> 
                            case decode_body(MsgType, Body) of
                                {error, Reason} ->
                                    {error, Reason};
                                {ok, Brecord} ->
                                    {ok, #'mmyn.message'{h=Hrecord, b=Brecord}}
                            end
                    end
            end
    end.

decode_header({PropList}) ->
    case proplists:get_value(<<"vsn">>, PropList) of
        undefined ->
            {error, no_version};
        ?MMYN_MSG_VSN->
            case proplists:get_value(<<"type">>, PropList) of
                undefined ->
                    {error, no_type};
                Type ->
                    case proplists:get_value(<<"system">>, PropList) of
                        undefined ->
                            {error, no_system};
                        Sid ->
                            case proplists:get_value(<<"transaction_id">>, PropList) of
                                undefined ->
                                    {error, no_transaction_id};
                                Tid ->
                                    {ok, #'mmyn.header'{vsn=?MMYN_MSG_VSN, type=Type, system=Sid, transaction_id=Tid}}
                            end
                    end
            end;
        N ->
            {error, {wrong_msg_vsn, [{current_msg_vsn, ?MMYN_MSG_VSN}, {parsed_msg_vsn, N}]}}
    end.

decode_body(<<"req.sendsms">>, {PropList}) ->
    case proplists:get_value(<<"sender">>, PropList) of
        undefined ->
            {error, no_sender};
        Sender ->
            case proplists:get_value(<<"msisdn">>, PropList) of
                undefined ->
                    {error, no_msisdn};
                Msisdn ->
                    case proplists:get_value(<<"message">>, PropList) of
                        undefined ->
                            {error, no_message};
                        Message ->
                            {ok, #'req.sendsms'{sender=Sender, msisdn=Msisdn, message=Message}}
                    end
            end
    end.
