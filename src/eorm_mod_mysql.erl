%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(eorm_mod_mysql).
-author("jellybean4").
-include("eorm_internal.hrl").

%% API
-export([
  prepare/2,
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
  case eorm_lib_mysql:build_prepare(Action, Spec) of
    {error, Reason} -> {error, Reason};
    Cmd ->
      ?DEBUG("build prepare ~p for action ~p with cmd ~p", [Prepare, Action, Cmd]),
      eorm_lib_mysql:prepare(Prepare, Cmd),
      {ok, Prepare}
  end.


-spec(execute(Spec::spec(), Info::#exec_info{}) ->
  {error, any()} | {ok, #db_ok_rslt{}}).
execute(Spec, #exec_info{action = Action}) ->
  ?ERROR("mod_mysql execute ~p for spec ~p error, unsupported", [Action, Spec]),
  {error, ?ER_UNSUPPORTED_EXECUTE}.

-spec(exec_prepare(Prepare::atom(), Spec::spec(), Info::#exec_info{}) ->
  {error, any()} | {ok, #db_ok_rslt{}} | {ok, #db_data_rslt{}}).
exec_prepare(Prepare, Spec, Info) ->
  case eorm_lib_mysql:build_prepare_args(Info, Spec) of
    {error, Reason} -> {error, Reason};
    {ok, Args} ->
      ?DEBUG("execute prepare ~p with args ~p", [Prepare, Args]),
      case eorm_lib_mysql:exec_prepare(Prepare, Args) of
        #error_packet{msg = Reason} ->
          ?ERROR("mod_mysql exec_prepare ~p for spec ~p failed, reason ~p", [Prepare, Spec, Reason]),
          {error, Reason};
        #result_packet{rows = Rows, field_list = Fields} ->
          ?DEBUG("mod_mysql exec_prepare ~p success", [Prepare]),
          {ok, #db_data_rslt{rows = Rows , fields = Fields}};
        #ok_packet{} ->
          ?DEBUG("mod_mysql exec_prepare ~p success", [Prepare]),
          {ok, #db_ok_rslt{}}
      end
  end.

prepare_name(TableName, Action) ->
  erlang:binary_to_atom(?FORMAT_BIN("~s_~s", [TableName, Action]), latin1).
