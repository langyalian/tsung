%%%-------------------------------------------------------------------
%%% @author kantappa
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2017 下午8:06
%%%-------------------------------------------------------------------
-module(ts_mongo).
-author("kantappa").

-behavior(ts_plugin).

-include("ts_macros.hrl").
-include("ts_profile.hrl").
-include("ts_mongo.hrl").
-include("mongo_protocol.hrl").

%% API
-export([document_user/1]).

%% ts_plugin callback
-export([add_dynparams/4,
    get_message/2,
    session_defaults/0,
    dump/2,
    parse/2,
    parse_bidi/2,
    parse_config/2,
    decode_buffer/2,
    new_session/0]).


document_user({Session, DynData}) ->
    io:format("~n[~p ~p] document_user = ~p ~n", [?MODULE, ?LINE, {Session, DynData}]),
    Docs = [#{name=><<"lisi">>, age=>32, sex=><<"male">>}],
    <<<<<<(bson_binary:put_document(Doc))/binary>> || Doc <- Docs>>/binary>>.


add_dynparams(false, {_DynVars, Session}, Param, HostData) ->
    io:format("~n[~p ~p]_DynVars=~p,Session=~p ~n", [?MODULE, ?LINE, _DynVars, Session]),
    Param;
add_dynparams(true, {DynVars, Session}, #mongo_request{documents = Documents} = Param, HostData) ->
    io:format("~n[~p ~p]add_dynparams ~n", [?MODULE, ?LINE]),
    io:format("~n[~p ~p] Documents=~p,~n ~n  DynVars=~p,~n ~nSession=~p,~n ~n Param=~p,~n ~n HostData=~p ~n", [?MODULE, ?LINE, Documents, DynVars, Session, Param, HostData]),
    subst(Param, DynVars).


get_message(Req = #mongo_request{type = insert, database = Db, collection = Collection, documents = Docs}, #state_rcv{session = S}) ->
    io:format("~n[~p ~p]get_message ~n", [?MODULE, ?LINE]),
%%    io:format("~n[~p ~p]req = ~p~n", [?MODULE, ?LINE, Req]),
    Bin = mongo_protocol:encode(Db, #insert{collection = Collection, documents = Docs}, 1),
    {Bin, S};
get_message(#mongo_request{type = close}, State) ->
    io:format("~n[~p ~p] mongo closed!!~n", [?MODULE, ?LINE]),
    {<<>>, [], true};
get_message(#mongo_request{} = Req, #state_rcv{session = S}) ->
    io:format("~n[~p ~p] mongo Req = ~p!!~n", [?MODULE, ?LINE, Req]),
    {<<>>, S}.


session_defaults() ->
    io:format("~n[~p ~p]session_defaults ~n", [?MODULE, ?LINE]),
    {ok, true}.


dump(A, B) ->
    io:format("~n[~p ~p]dump ~n", [?MODULE, ?LINE]),
    ts_plugin:dump(A, B).


parse(closed, State) ->
    io:format("~n[~p ~p] mongo closed!!~n", [?MODULE, ?LINE]),
    {State#state_rcv{ack_done = true, datasize = 0}, [], true};
parse(Data, State) ->
    io:format("~n[~p ~p]data = ~p~n", [?MODULE, ?LINE, Data]),
    {State#state_rcv{}, [], false}.

parse_bidi(Data, State) ->
    io:format("~n[~p ~p]parse_bidi ~n", [?MODULE, ?LINE]),
    ts_plugin:parse_bidi(Data, State).

parse_config(Element, Conf) ->
%%    io:format("~n[~p ~p]element = ~p,conf = ~p~n", [?MODULE, ?LINE, Element, Conf]),
    io:format("~n[~p ~p]parse_config ~n", [?MODULE, ?LINE]),
    ts_config_mongo:parse_config(Element, Conf).

decode_buffer(Buffer, #mongo_session{}) ->
    io:format("~n[~p ~p]decode_buffer ~n", [?MODULE, ?LINE]),
    Buffer.

new_session() ->
    io:format("~n[~p ~p]new_session ~n", [?MODULE, ?LINE]),
    #mongo_session{}.


subst(Req = #mongo_request{documents = Documents}, DynVars) ->
    Docs = ts_search:subst(Documents, DynVars),
    DocJson = mochijson2:decode(Docs, [{format, proplist}]),
    Jsons =
        case DocJson of
            [[_ | _] | _] -> DocJson;
            [{_, _} | _] -> [DocJson]
        end,
    DocsMap = [maps:from_list(Json) || Json <- Jsons],
    Req#mongo_request{documents = DocsMap}.