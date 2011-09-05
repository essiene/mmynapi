-module(mmynapi).
-export([to_json/1]).

-include("mmynapi.hrl").


to_json(#'mmyn.message'{}=Msg) ->
    json:encode(mmynapi_encode:to_json_form(Msg));
to_json(_) ->
    {error, invalid_message}.
