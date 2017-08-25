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
-export([object_id/0]).
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


add_dynparams(false, {_DynVars, Session}, Param, HostData) ->
%%    io:format("~n[~p ~p]_DynVars=~p,Session=~p ~n", [?MODULE, ?LINE, _DynVars, Session]),
    Param;
add_dynparams(true, {DynVars, Session}, #mongo_request{documents = Documents} = Param, HostData) ->
%%    io:format("~n[~p ~p]add_dynparams ~n", [?MODULE, ?LINE]),
%%    io:format("~n[~p ~p] Documents=~p,~n ~n  DynVars=~p,~n ~nSession=~p,~n ~n Param=~p,~n ~n HostData=~p ~n", [?MODULE, ?LINE, Documents, DynVars, Session, Param, HostData]),
    subst(Param, DynVars).


get_message(Req = #mongo_request{type = insert, database = Db, collection = Collection, documents = Docs}, #state_rcv{session = #mongo_session{requests = Requests} = S}) ->
%%    io:format("~n[~p ~p]get_message ~n", [?MODULE, ?LINE]),
%%    io:format("~n[~p ~p]req = ~p~n", [?MODULE, ?LINE, Req]),
    RequestId = request_id(),
    {Message, NowDocs} = insert(Collection, Docs),
%%    io:format("~n[~p ~p]Message = ~p~n", [?MODULE, ?LINE, Message]),
    Bin = mongo_protocol:encode(Db, Message, RequestId),
    {Bin, S};
get_message(Req, #state_rcv{session = S}) ->
    io:format("~n[~p ~p]invalid req = ~p~n",[?MODULE,?LINE,Req]),
    {<<>>, S}.


session_defaults() ->
%%    io:format("~n[~p ~p]session_defaults ~n", [?MODULE, ?LINE]),
    {ok, true}.


dump(A, B) ->
%%    io:format("~n[~p ~p]dump ~n", [?MODULE, ?LINE]),
    ts_plugin:dump(A, B).



parse(Data, #state_rcv{session = S = #mongo_session{requests = Requests}} = State) ->
%%    io:format("~n[~p ~p]data = ~p~n", [?MODULE, ?LINE, Data]),
    case Data of
        <<Length:32/signed-little, DataBody/binary>> when byte_size(DataBody) >= (Length - 4) ->
            PayloadLength = Length - 4,
            <<Payload:PayloadLength/binary, Rest/binary>> = DataBody,
            {Id, Response, <<>>} = mongo_protocol:get_reply(Payload),
            if erlang:byte_size(Rest) > 0 ->
                io:format("~n[~p ~p]response:~p,rest size = ~p~n", [?MODULE, ?LINE, Response, erlang:byte_size(Rest)]);
                true -> ok end,
%%            S2 = S#mongo_session{requests = dict:erase(Id, Requests)},
            {State#state_rcv{ack_done = true,datasize = 0}, [], false};
        _ ->
            io:format("~n[~p ~p]ack_done = false ~n", [?MODULE, ?LINE]),
            {State#state_rcv{ack_done = false}, [], false}
    end.

parse_bidi(Data, State) ->
%%    io:format("~n[~p ~p]parse_bidi ~n", [?MODULE, ?LINE]),
    ts_plugin:parse_bidi(Data, State).

parse_config(Element, Conf) ->
%%    io:format("~n[~p ~p]element = ~p,conf = ~p~n", [?MODULE, ?LINE, Element, Conf]),
%%    io:format("~n[~p ~p]parse_config ~n", [?MODULE, ?LINE]),
    ts_config_mongo:parse_config(Element, Conf).

decode_buffer(Buffer, #mongo_session{}) ->
%%    io:format("~n[~p ~p]decode_buffer ~n", [?MODULE, ?LINE]),
    Buffer.

new_session() ->
%%    init(),
    #mongo_session{requests = dict:new()}.


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



init() ->
    %% 创建ets表，递归增加变量值
    case ets:info(?MODULE) of
        undefined ->
            ?MODULE = ets:new(?MODULE, [named_table, private, {write_concurrency, true}, {read_concurrency, true}]),
            ets:insert(?MODULE, [
                {oid_counter, 0},
                {oid_machineprocid, oid_machineprocid()},
                {requestid_counter, 0}
            ]);
        _ -> ok
    end.


-spec oid_machineprocid() -> <<_:40>>.
oid_machineprocid() ->
    OSPid = list_to_integer(os:getpid()),
    {ok, Hostname} = inet:gethostname(),
    <<MachineId:3/binary, _/binary>> = erlang:md5(Hostname),
    <<MachineId:3/binary, OSPid:16/big>>.


%% @doc 递增生成object id
-spec object_id() -> bson:objectid().
object_id() ->
    Now = bson:unixtime_to_secs(bson:timenow()),
    MPid = ets:lookup_element(?MODULE, oid_machineprocid, 2),
    N = ets:update_counter(?MODULE, oid_counter, 1),
    bson:objectid(Now, MPid, N).


%% @doc Fresh request id
-spec request_id() -> pos_integer().
request_id() ->
    erlang:system_time().
%%    ets:update_counter(?MODULE, requestid_counter, {2, 1, trunc(math:pow(2, 31)) - 1, 0}).

insert(Collection, Docs) ->
    insert(Collection, Docs, {<<"w">>, 1}).


insert(Collection, Doc, WC) when is_tuple(Doc); is_map(Doc) ->
    {Result, [Converted | _]} = insert(Collection, [Doc], WC),
    {Result, Converted};
insert(Collection, Docs, WC) ->
    Converted = prepare(Docs, fun assign_id/1),
    {command({<<"insert">>, Collection, <<"documents">>, Converted, <<"writeConcern">>, WC}), Converted}.



command(Query) when is_record(Query, query) ->
    Query#query{batchsize = -1};
command(Command) ->
    command(
        #'query'{
            collection = <<"$cmd">>,
            selector = Command
        }).

command(Command, _IsSlaveOk = true) ->
    command(
        #'query'{
            collection = <<"$cmd">>,
            selector = Command,
            slaveok = true,
            sok_overriden = true
        });
command(Command, _IsSlaveOk = false) ->
    command(Command).


-spec prepare(tuple() | list() | map(), fun()) -> list().
prepare(Docs, AssignFun) when is_tuple(Docs) -> %bson
    case element(1, Docs) of
        <<"$", _/binary>> -> Docs;  %command
        _ ->  %document
            case prepare_doc(Docs, AssignFun) of
                Res when is_tuple(Res) -> [Res];
                List -> List
            end
    end;
prepare(Doc, AssignFun) when is_map(Doc), map_size(Doc) == 1 ->
    case maps:keys(Doc) of
        [<<"$", _/binary>>] -> Doc; %command
        _ ->  %document
            case prepare_doc(Doc, AssignFun) of
                Res when is_tuple(Res) -> [Res];
                List -> List
            end
    end;
prepare(Doc, AssignFun) when is_map(Doc) ->
    Keys = maps:keys(Doc),
    case [K || <<"$", _/binary>> = K <- Keys] of
        Keys -> Doc; % multiple commands
        _ ->  % document
            case prepare_doc(Doc, AssignFun) of
                Res when is_tuple(Res) -> [Res];
                List -> List
            end
    end;
prepare(Docs, AssignFun) when is_list(Docs) ->
    case prepare_doc(Docs, AssignFun) of
        Res when not is_list(Res) -> [Res];
        List -> List
    end.


%% @private
%% Convert maps or proplists to bson
prepare_doc(Docs, AssignFun) when is_list(Docs) ->  %list of documents
    case is_proplist(Docs) of
        true -> prepare_doc(maps:from_list(Docs), AssignFun); %proplist
        false -> lists:map(fun(Doc) -> prepare_doc(Doc, AssignFun) end, Docs)
    end;
prepare_doc(Doc, AssignFun) ->
    AssignFun(Doc).

%% @private
-spec assign_id(bson:document() | map()) -> bson:document().
assign_id(Map) when is_map(Map) ->
    case maps:is_key(<<"_id">>, Map) of
        true -> Map;
%%        false -> Map#{<<"_id">> =>  object_id()}
        false -> Map
    end;
assign_id(Doc) ->
    case bson:lookup(<<"_id">>, Doc) of
%%        {} -> bson:update(<<"_id">>, object_id(), Doc);
        {} -> Doc;
        _Value -> Doc
    end.


is_proplist(List) ->
    Check = fun({X, _}) when is_atom(X) -> true;(_) -> false end,
    lists:all(Check, List).