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


document_user({Pid, DynData}) ->
    "hello".


add_dynparams(Bool, DynData, Param, HostData) ->
    Param#mongo_request{}.


get_message(Req = #mongo_request{type = insert, database = Db, collection = Collection}, #state_rcv{session = S}) ->
    Docs = [#{name=><<"lisi">>, age=>12}],
    io:format("~n[~p ~p]req = ~p~n", [?MODULE, ?LINE, Req]),
    Bin = mongo_protocol:encode(Db, #insert{collection = Collection, documents = Docs}, 1),
    {Bin, S};
get_message(#mongo_request{type = close}, #state_rcv{} = State) ->
    {State#state_rcv{ack_done = true, datasize = 0}, [], true};
get_message(#mongo_request{}, #state_rcv{session = S}) ->
    {<<"mongo">>, S}.


session_defaults() ->
    {ok, true}.


dump(A, B) ->
    ts_plugin:dump(A, B).


parse(closed, State) ->
    {State, true};
parse(Data, State) ->
    io:format("~n[~p ~p]data = ~p~n", [?MODULE, ?LINE, Data]),
    {State#state_rcv{}, [], false}.

parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data, State).

parse_config(Element, Conf) ->
    io:format("~n[~p ~p]element = ~p,conf = ~p~n", [?MODULE, ?LINE, Element, Conf]),
    ts_config_mongo:parse_config(Element, Conf).

decode_buffer(Buffer, #mongo_session{}) ->
    Buffer.

new_session() ->
    #mongo_session{}.