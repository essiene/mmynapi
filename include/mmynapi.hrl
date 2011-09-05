-ifndef(mmynapi).
-define(mmynapi, true).

-define(MMYN_MSG_VSN, '2.0.1').

-define(MMYN_REQ_SENDSMS, "request.sendsms").
-define(MMYN_RES_SENDSMS, "response.sendsms").
-define(MMYN_REQ_REPLY, "request.reply").
-define(MMYN_RES_REPLY, "response.reply").
-define(MMYN_REQ_NOTIFY, "request.notify").
-define(MMYN_RES_NOTIFY, "response.notify").

-record('mmyn.message', {h, b}).

-record('mmyn.header', {
            vsn=?MMYN_MSG_VSN,
            type,
            system,
            transaction_id}).

-record('req.sendsms', {
        sender,
        msisdn,
        message}).

-record('res.sendsms', {
        status,
        detail}).

-record('req.reply', {
        id,
        sender,
        msisdn,
        message}).

-record('res.reply', {
        status,
        detail}).

-record('req.notify', {
        id,
        shortcode,
        keyword,
        msisdn,
        message,
        max_ttl}).

-record('res.notify', {
        status,
        detail,
        wait_for_reply,
        ttl}).



-endif.
