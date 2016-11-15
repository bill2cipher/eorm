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
  prepare/2,
  execute/2,
  exec_prepare/3
]).

-spec(start() -> {error, any()} | ok).
start() ->
  DBModule = application:get_env(eorm, db_module, mod_mysql),
  DBModule:start().

-spec(prepare(Action::atom(), Spec::spec()) ->
  {error, any()} | {ok, atom()}).
prepare(Action, Spec) ->
  ?DBModule:prepare(Action, Spec).

-spec(execute(Spec::spec(), Info::#exec_info{}) ->
  {error, any()} | {ok, #db_ok_rslt{}} | {ok, #db_data_rslt{}}).
execute(Spec, Info) ->
  ?DBModule:execute(Spec, Info).

-spec(exec_prepare(Prepare::atom(), Spec::spec(), Info::#exec_info{}) ->
  {error, any()} | {ok, #db_ok_rslt{}} | {ok, #db_data_rslt{}}).
exec_prepare(Prepare, Spec, Info) ->
  ?DBModule:exec_prepare(Prepare, Spec, Info).
