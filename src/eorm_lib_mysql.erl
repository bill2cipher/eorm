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
  build_loadkeys/1,
  build_update_cmd/2,
  build_update_args/2,
  build_prepare_args/2
]).

-spec(prepare(Prepare::atom(), Cmd::string()) -> ok).
prepare(Prepare, Cmd) ->
  emysql:prepare(Prepare, Cmd).

execute(Cmd, Args) ->
  emysql:execute(?MYSQL_POOL_NAME, Cmd, Args).

exec_prepare(Prepare, Args) ->
  emysql:execute(?MYSQL_POOL_NAME, Prepare, Args).

-spec(build_loadkeys(Spec::spec()) -> binary()).
build_loadkeys(Spec) ->
  ?FORMAT_BIN("SELECT ~s FROM ~s", [lib_spec:key_name(Spec), Spec#data_spec.name]).

-spec(build_prepare(Action::string(), Spec::spec()) -> {error, any()} | binary()).
build_prepare(?DB_ACTION_INSERT, #data_spec{fields = Fields, name = TableName}) ->
  Fields2 = [N || {_, N, _} <- Fields, N =/= ignore],
  FieldList = string:join(Fields2, ", "),
  Args = string:join(lists:duplicate(length(Fields2), "?"), ", "),
  ?FORMAT_BIN("REPLACE INTO ~s (~s) VALUES(~s)", [TableName, FieldList, Args]);
build_prepare(?DB_ACTION_DELETE, Spec) ->
  KeyName = lib_spec:key_name(Spec),
  ?FORMAT_BIN("DELETE FROM ~s WHERE ~s = ?", [Spec#data_spec.name, KeyName]);
build_prepare(?DB_ACTION_LOOKUP, Spec = #data_spec{name = TableName, fields = Fields}) ->
  KeyName = lib_spec:key_name(Spec),
  SelectFields = string:join([N || {_, N, _} <- Fields, N =/= ignore], ", "),
  ?FORMAT_BIN("SELECT ~s FROM ~s WHERE ~s = ?", [SelectFields, TableName, KeyName]);
build_prepare(?DB_ACTION_UPDATE, _) ->
  {error, ?ER_UNSUPPORTED_PREPARE}.

-spec(build_update_cmd(Spec::spec(), UpdateSpec::list()) -> Cmd::string()).
build_update_cmd(Spec = #data_spec{fields = Fields, name = TableName}, UpdateSpec) ->
  KeyName = lib_spec:key_name(Spec),
  UpdateCmd = build_update_cmd(UpdateSpec, Fields, []),
  UpdateCmd2 = ?FORMAT_BIN("UPDATE ~s SET ~p WHERE ~p = ?", [TableName, UpdateCmd, KeyName]),
  UpdateCmd2.
build_update_cmd([], _Fields, Rslt) ->
  Rslt2 = [io_lib:format("~s = ?", N) || N <- lists:reverse(Rslt)],
  string:join(Rslt2, ", ");
build_update_cmd([{K, _} | L], Fields, Rslt) ->
  {_, N, _} = lists:keyfind(K, 1, Fields),
  build_update_cmd(L, Fields, [N | Rslt]).

build_update_args(UpdateSpec, Key) ->
  [V || {_, V} <- UpdateSpec] ++ [Key].


-spec(build_prepare_args(Info::#exec_info{}, Spec::spec()) -> {ok, list()} | {error, any()}).
build_prepare_args(#exec_info{action = ?DB_ACTION_INSERT, data = Data}, Spec) ->
  #data_spec{fields = Fields, type = Type} = Spec,
  Args = case Type of
    tuple -> build_tuple_args(Fields, Data, []);
    map   -> build_map_args(Fields, Data, [])
  end,
  {ok, Args};
build_prepare_args(#exec_info{action = ?DB_ACTION_DELETE, key = Key}, _Spec) ->
  {ok, [Key]};
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
