%%%-------------------------------------------------------------------
%%% @author kantappa
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2017 下午8:08
%%%-------------------------------------------------------------------
-module(ts_config_mongo).
-author("kantappa").

-include("ts_profile.hrl").
-include("ts_mongo.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").

%% API
-export([parse_config/2]).

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name = dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element, Conf);
parse_config(Element = #xmlElement{name = mongo},
    Config = #config{curid = Id, session_tab = Tab,
        sessions = [CurS | _], dynvar = DynVar,
        subst = SubstFlag, match = MatchRegExp}) ->
    {Ack, Request} = case ts_config:getAttr(atom, Element#xmlElement.attributes, type) of
                         insert ->
                             Database = ts_config:getAttr(atom, Element#xmlElement.attributes, database),
                             Collection = ts_config:getAttr(atom, Element#xmlElement.attributes, collection),
                             Content = ts_config:getText(Element#xmlElement.content),
                             Documents = list_to_binary(ts_utils:clean_str(Content)),
                             {parse, #mongo_request{type = insert, database = to_binary(Database), collection = to_binary(Collection), documents = Documents}};
                         close ->
                             {no_ack, #mongo_request{}};
                         _ ->
                             {no_ack, #mongo_request{}}
                     end,
    Msg = #ts_request{ack = Ack,
        endpage = true,
        dynvar_specs = DynVar,
        subst = SubstFlag,
        match = MatchRegExp,
        param = Request},

    ts_config:mark_prev_req(Id - 1, Tab, CurS),
    ets:insert(Tab, {{CurS#session.id, Id}, Msg}),
    lists:foldl(fun(A, B) -> ts_config:parse(A, B) end,
        Config#config{dynvar = undefined},
        Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element, Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.


to_binary(X) when is_atom(X) ->
    to_binary(atom_to_list(X));
to_binary(X) when is_integer(X) ->
    to_binary(integer_to_list(X));
to_binary(X) when is_float(X) ->
    float_to_binary(X);
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_binary(X) ->
    X.
