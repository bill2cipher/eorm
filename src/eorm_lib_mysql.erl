%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(eorm_lib_mysql).
-author("jellybean4").
-include("eorm_internal.hrl").
%% API
-export([
  prepare/2,
  execute/2,
  exec_prepare/2
]).

-export([
  build_prepare/2,
  build_prepare_args/2
]).

-spec(prepare(Prepare::atom(), Cmd::string()) -> ok).
prepare(Prepare, Cmd) ->
  emysql:prepare(Prepare, Cmd).

execute(Cmd, Args) ->
  emysql:execute(?MYSQL_POOL_NAME, Cmd, Args).

exec_prepare(Prepare, Args) ->
  emysql:execute(?MYSQL_POOL_NAME, Prepare, Args).

-spec(build_prepare(Action::string(), Spec::spec()) -> {error, any()} | binary()).
build_prepare(?DB_ACTION_INSERT, #data_spec{fields = Fields, name = TableName}) ->
  Fields2 = [N || {_, N, _} <- Fields, N =/= ignore],
  FieldList = string:join(Fields2, ", "),
  Args = string:join(lists:duplicate(length(Fields2), "?"), ", "),
  ?FORMAT_BIN("REPLACE INTO ~s (~s) VALUES(~s)", [TableName, FieldList, Args]);
build_prepare(?DB_ACTION_LOOKUP, Spec = #data_spec{name = TableName, fields = Fields}) ->
  KeyName = eorm_lib_spec:key_name(Spec),
  SelectFields = string:join([N || {_, N, _} <- Fields, N =/= ignore], ", "),
  ?FORMAT_BIN("SELECT ~s FROM ~s WHERE ~s = ?", [SelectFields, TableName, KeyName]);
build_prepare(_, _) ->
  {error, ?ER_UNSUPPORTED_PREPARE}.

-spec(build_prepare_args(Info::#exec_info{}, Spec::spec()) -> {ok, list()} | {error, any()}).
build_prepare_args(#exec_info{action = ?DB_ACTION_INSERT, data = Data}, Spec) ->
  #data_spec{fields = Fields, type = Type} = Spec,
  Args = case Type of
    tuple -> build_tuple_args(Fields, Data, []);
    map   -> build_map_args(Fields, Data, [])
  end,
  {ok, Args};
build_prepare_args(#exec_info{action = ?DB_ACTION_LOOKUP, key = Key}, _Spec) ->
  {ok, [Key]};
build_prepare_args(#exec_info{action = Action}, _Spec) ->
  ?ERROR("build prepare for action ~p failed, not supported", [Action]),
  {error, ?ER_UNSUPPORTED_PREPARE}.

build_tuple_args([], _Data, Rslt) -> lists:reverse(Rslt);
build_tuple_args([{_, ignore, _} | L], Data, Rslt) ->
  build_tuple_args(L, Data, Rslt);
build_tuple_args([{K, _, _} | L], Data, Rslt) ->
  V = element(K, Data),
  build_tuple_args(L, Data, [V | Rslt]).

build_map_args([], _Data, Rslt) -> lists:reverse(Rslt);
build_map_args([{_, ignore, _} | L], Data, Rslt) ->
  build_map_args(L, Data, Rslt);
build_map_args([{K, _, _} | L], Data, Rslt) ->
  #{K := V} = Data,
  build_map_args(L, Data, [V | Rslt]).
