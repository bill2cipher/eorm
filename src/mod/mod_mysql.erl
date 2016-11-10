%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(mod_mysql).
-author("jellybean4").
-include("eorm_internal.hrl").

%% API
-export([
  prepare/2,
  load_keys/1,
  execute/2,
  exec_prepare/3
]).

-export([start/0]).

-spec(start() -> ok | {error, any()}).
start() ->
  application:ensure_all_started(emysql),
  Host = application:get_env(eorm, db_host, "127.0.0.1"),
  Port = application:get_env(eorm, db_port, 3306),
  DBName = application:get_env(eorm, db_name, "eorm_test"),
  DBUser = application:get_env(eorm, db_user, "root"),
  DBPass = application:get_env(eorm, db_passwd, "123456"),
  Opts = [{size, ?MYSQL_POOL_SIZE}, {host, Host}, {port, Port},
    {user, DBUser}, {password, DBPass}, {database, DBName}, {encoding,utf8}],
  try emysql:add_pool(?MYSQL_POOL_NAME, Opts) of
    {error, Reason} -> {error, Reason};
    Result ->
      ?INFO("mod_mysql start service result ~p", [Result]),
      ok
  catch Error : Reason ->
    ?ERROR("mod_mysql start service failed, reason ~p", [{Error, Reason}]),
    {error, Reason}
  end.

-spec(prepare(Action::atom(), Spec::spec()) -> {error, any()} | {ok, atom()}).
prepare(Action, Spec) ->
  Prepare = prepare_name(Spec#data_spec.name, Action),
  case lib_mysql:build_prepare(Action, Spec) of
    {error, Reason} -> {error, Reason};
    Cmd ->
      lib_mysql:prepare(Prepare, Cmd),
      {ok, Prepare}
  end.

-spec(load_keys(Spec::spec()) -> {error, any()} | {ok, #db_data_rslt{}}).
load_keys(Spec) ->
  Cmd = lib_mysql:build_loadkeys(Spec),
  case lib_mysql:execute(Cmd, []) of
    #error_packet{msg = Reason} ->
      ?ERROR("load keys for spec ~p failed, reason ~p", [Spec, Reason]),
      {error, Reason};
    #ok_packet{} ->
      ?ERROR("load keys get ok_packet result for spec", [Spec]),
      {error, ?ER_RESULT_FORMAT_ERROR};
    #result_packet{rows = Rows, field_list = Fields} ->
      ?DEBUG("load keys for spec ~p success", [Spec]),
      {ok, #db_data_rslt{rows = Rows, fields = Fields}}
  end.

-spec(execute(Spec::spec(), Info::#exec_info{}) ->
  {error, any()} | {ok, #db_ok_rslt{}}).
execute(Spec, #exec_info{data = UpdateSpec, action = ?DB_ACTION_UPDATE, key = Key}) ->
  Cmd = lib_mysql:build_update_cmd(Spec, UpdateSpec),
  Args = lib_mysql:build_update_args(UpdateSpec, Key),
  case lib_mysql:execute(Cmd, Args) of
    #error_packet{msg = Reason} ->
      ?ERROR("mod_mysql execute update for spec ~p failed, reason ~p", [Spec, Reason]),
      {error, Reason};
    #result_packet{} ->
      ?ERROR("mod_mysql execute update for spec ~p get result packet", [Spec]),
      {error, ?ER_RESULT_FORMAT_ERROR};
    #ok_packet{} ->
      ?DEBUG("mod_mysql execute update success"),
      {ok, #db_ok_rslt{}}
  end;
execute(Spec, #exec_info{action = Action}) ->
  ?ERROR("mod_mysql execute ~p for spec ~p error, unsupported", [Action, Spec]),
  {error, ?ER_UNSUPPORTED_EXECUTE}.

-spec(exec_prepare(Prepare::atom(), Spec::spec(), Info::#exec_info{}) ->
  {error, any()} | {ok, #db_ok_rslt{}} | {ok, #db_data_rslt{}}).
exec_prepare(Prepare, Spec, Info) ->
  Args = lib_mysql:build_prepare_args(Info, Spec),
  case lib_mysql:exec_prepare(Prepare, Args) of
    #error_packet{msg = Reason} ->
      ?ERROR("mod_mysql exec_prepare ~p for spec ~p failed, reason ~p", [Prepare, Spec, Reason]),
      {error, Reason};
    #result_packet{rows = Rows, field_list = Fields} ->
      ?DEBUG("mod_mysql exec_prepare ~p success", [Prepare]),
      {ok, #db_data_rslt{rows = Rows , fields = Fields}};
    #ok_packet{} ->
      ?DEBUG("mod_mysql exec_prepare ~p success", [Prepare]),
      {ok, #db_ok_rslt{}}
  end.
prepare_name(TableName, Action) ->
  list_to_atom(io_lib:format("~s_~s", [TableName, Action])).
