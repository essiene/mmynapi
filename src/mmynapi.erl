-module(mmynapi).
-export([to_json/1, to_json/3, from_json/1,call/2]).

-include("mmynapi.hrl").

call({Url, User, Pass}, #'mmyn.message'{}=Msg) ->
    C = #callopts{url=Url, opts=[{basic_auth, {User, Pass}}]},
    http_call(C, Msg);
call(Url, #'mmyn.message'{}=Msg) ->
    C = #callopts{url=Url},
    http_call(C, Msg).

http_call(#callopts{}=C, #'mmyn.message'{}=Msg) ->
    Req = to_json(Msg),
    case http_call(C, Req) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            from_json(Json)
    end;
http_call(#callopts{url=Url, headers=Headers0, opts=Opts0}, Body) ->
    Opts1 = [{response_format, binary}|Opts0],
    Headers1 = [{"Content-Type", "application/json"}, {"Accept", "application/json"}|Headers0],
    try ibrowse:send_req(Url, Headers1, post, Body, Opts1) of
        {error, Reason} ->
            {error, Reason};
        {ok, "200", _, ResponseBody} ->
            {ok, ResponseBody};
        {ok, StatusCode, _, _StatusDetail} ->
            {error, list_to_atom(lists:concat([http_,StatusCode]))}
        catch 
            _:Exception ->
                {error, Exception}
        end.


to_json(System, Tid, #'req.sendsms'{}=B) ->
    H = #'mmyn.header'{
        type = 'req.sendsms',
        system = System,
        transaction_id = Tid},
    M = #'mmyn.message'{h=H, b=B},
    to_json(M);
to_json(System, Tid, #'res.sendsms'{}=B) ->
    H = #'mmyn.header'{
        type = 'res.sendsms',
        system = System,
        transaction_id = Tid},
    M = #'mmyn.message'{h=H, b=B},
    to_json(M);
to_json(System, Tid, #'req.reply'{}=B) ->
    H = #'mmyn.header'{
        type = 'req.reply',
        system = System,
        transaction_id = Tid},
    M = #'mmyn.message'{h=H, b=B},
    to_json(M);
to_json(System, Tid, #'res.reply'{}=B) ->
    H = #'mmyn.header'{
        type = 'res.reply',
        system = System,
        transaction_id = Tid},
    M = #'mmyn.message'{h=H, b=B},
    to_json(M);
to_json(System, Tid, #'req.notify'{}=B) ->
    H = #'mmyn.header'{
        type = 'req.notify',
        system = System,
        transaction_id = Tid},
    M = #'mmyn.message'{h=H, b=B},
    to_json(M);
to_json(System, Tid, #'res.notify'{}=B) ->
    H = #'mmyn.header'{
        type = 'res.notify',
        system = System,
        transaction_id = Tid},
    M = #'mmyn.message'{h=H, b=B},
    to_json(M);
to_json(System, Tid, #'mmyn.fault'{}=B) ->
    H = #'mmyn.header'{
        type = 'mmyn.fault',
        system = System,
        transaction_id = Tid},
    M = #'mmyn.message'{h=H, b=B},
    to_json(M).

to_json(#'mmyn.message'{}=Msg) ->
    jiffy:encode(mmynapi_encode:to_json_form(Msg));
to_json(_) ->
    {error, invalid_message}.

from_json(Json) ->
    mmynapi_decode:to_message(jiffy:decode(Json)).
