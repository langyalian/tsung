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

%% API
-export([add_dynparams/4,
    get_message/2,
    session_defaults/0,
    dump/2,
    parse/2,
    parse_bidi/2,
    parse_config/2,
    decode_buffer/2,
    new_session/0]).


add_dynparams(Bool, DynData, Param, HostData) ->
    Param#mongo_request{}.


get_message(#mongo_request{}, #state_rcv{session = S}) ->
    {<<"mongo">>, S}.


session_defaults() ->
    {ok, true}.


dump(A, B) ->
    ts_plugin:dump(A, B).


parse(closed, State) ->
    {State, true};
parse(Data, State) ->
    {State#state_rcv{}, [], false}.

parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data,State).

parse_config(Element, Conf) ->
    ts_config_mongo:parse_config(Element, Conf).

decode_buffer(Buffer,#mongo_session{}) ->
    Buffer.

new_session() ->
    #mongo_session{}.