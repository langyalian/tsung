%%%-------------------------------------------------------------------
%%% @author kantappa
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2017 下午8:04
%%%-------------------------------------------------------------------
-author("kantappa").
-ifndef(TS_MONGO).
-define(TS_MONGO, true).

-record(mongo_request, {
    type,
    database,
    collection,
    documents
}).

-record(mongo_session,
{
}
).

%%
-record(mongo_dyndata,
{
    none
}
).

%% unused
-record(mongo,
{
    fixme
}
).

-endif.