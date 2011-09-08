-module(mmynapi_encode).
-export([to_json_form/1]).
-include("mmynapi.hrl").


to_json_form(#'mmyn.message'{h=Header, b=Body}) ->
    {[
            {<<"header">>,to_json_form(Header)}, 
            {<<"body">>,to_json_form(Body)}]};
to_json_form(#'mmyn.header'{vsn=V, type=T, system=S, transaction_id=Tid}) ->
    {[
            {<<"vsn">>,to_json_form(V)},
            {<<"type">>,to_json_form(T)},
            {<<"system">>,to_json_form(S)},
            {<<"transaction_id">>,to_json_form(Tid)}]};
to_json_form(#'req.sendsms'{sender=S, msisdn=M, message=Msg}) ->
    {[
            {<<"sender">>,to_json_form(S)},
            {<<"msisdn">>,to_json_form(M)},
            {<<"message">>,to_json_form(Msg)}]};
to_json_form(#'res.sendsms'{status=S, detail=D}) ->
    {[
            {<<"status">>, to_json_form(S)},
            {<<"detail">>, to_json_form(D)}]};
to_json_form(#'req.reply'{id=Id, sender=S, msisdn=M, message=Msg}) ->
    {[
            {<<"id">>, to_json_form(Id)},
            {<<"sender">>, to_json_form(S)},
            {<<"msisdn">>, to_json_form(M)},
            {<<"message">>, to_json_form(Msg)}]};
to_json_form(#'res.reply'{status=S, detail=D}) ->
    {[
            {<<"status">>, to_json_form(S)},
            {<<"detail">>, to_json_form(D)}]};
to_json_form(#'req.notify'{id=Id, shortcode=S, keywords=K, msisdn=M, message=Msg, max_ttl=MaxTtl}) ->
    {[
            {<<"id">>, to_json_form(Id)},
            {<<"shortcode">>, to_json_form(S)},
            {<<"keywords">>, to_json_form(K)},
            {<<"msisdn">>, to_json_form(M)},
            {<<"message">>, to_json_form(Msg)},
            {<<"max_ttl">>, to_json_form(MaxTtl)}]};
to_json_form(#'res.notify'{status=S, detail=D, wait_for_reply=W, ttl=T}) ->
    {[
            {<<"status">>, to_json_form(S)},
            {<<"detail">>, to_json_form(D)},
            {<<"wait_for_reply">>, to_json_form(W)},
            {<<"ttl">>, to_json_form(T)}]};
to_json_form(null=M) ->
    M;
to_json_form(M) when is_boolean(M) ->
    M;
to_json_form(M) when is_atom(M) ->
    list_to_binary(atom_to_list(M));
to_json_form(L) when is_list(L) ->
    to_json_form(L, []);
to_json_form(N) ->
    N.

to_json_form([], Accm) ->
    lists:reverse(Accm);
to_json_form([H|T], Accm) ->
    F = to_json_form(H),
    to_json_form(T, [F|Accm]).
