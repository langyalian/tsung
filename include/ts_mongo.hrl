%%%-------------------------------------------------------------------
%%% @author kantappa
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2017 下午8:04
%%%-------------------------------------------------------------------
-author("kantappa").

-record(mongo_request, {
    type,
    arith,
    data             % may be a string or two numbers
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

%%% Supported byte code instructions
-define(ECHO, 0).
-define(ADD, 1).
-define(SUB, 2).