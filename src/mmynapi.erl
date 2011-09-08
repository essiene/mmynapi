-module(mmynapi).
-export([to_json/1, to_json/3, from_json/1]).

-include("mmynapi.hrl").


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
    to_json(M).

to_json(#'mmyn.message'{}=Msg) ->
    jiffy:encode(mmynapi_encode:to_json_form(Msg));
to_json(_) ->
    {error, invalid_message}.

from_json(Json) ->
    mmynapi_decode:to_message(jiffy:decode(Json)).
