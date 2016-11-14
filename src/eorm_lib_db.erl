%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(eorm_lib_db).
-author("jellybean4").
-include("eorm_internal.hrl").
%% API
-export([start/0]).

-export([
  prepare/3,
  load_keys/2,
  execute/3,
  exec_prepare/4
]).

-spec(start() -> {error, any()} | ok).
start() ->
  DBModule = application:get_env(eorm, db_module, mod_mysql),
  DBModule:start().

-spec(prepare(DBModule::module(), Action::atom(), Spec::spec()) ->
  {error, any()} | {ok, atom()}).
prepare(DBModule, Action, Spec) ->
  DBModule:prepare(Action, Spec).

-spec(load_keys(DBModule::module(), Spec::spec()) ->
  {error, any()} | {ok, list()}).
load_keys(DBModule, Spec) ->
  case DBModule:load_keys(Spec) of
    {ok, #db_data_rslt{rows = Rows}} -> {ok, Rows};
    Error -> Error
  end.

-spec(execute(DBModule::module(), Spec::spec(), Info::#exec_info{}) ->
  {error, any()} | {ok, #db_ok_rslt{}} | {ok, #db_data_rslt{}}).
execute(DBModule, Spec, Info) ->
  DBModule:execute(Spec, Info).

-spec(exec_prepare(DBModule::module(), Prepare::atom(), Spec::spec(), Info::#exec_info{}) ->
  {error, any()} | {ok, #db_ok_rslt{}} | {ok, #db_data_rslt{}}).
exec_prepare(DBModule, Prepare, Spec, Info) ->
  DBModule:exec_prepare(Prepare, Spec, Info).
