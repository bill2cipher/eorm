%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(lib_db).
-author("jellybean4").

%% API
-export([
  prepare/3,
  load_keys/2,
  execute/2,
  exec_prepare/3
]).

prepare(DBModule, Action, Spec) ->
  ignore.

load_keys(DBModule, Spec) ->
  ignore.

execute(DBModule, Info) ->
  ignore.

exec_prepare(DBModule, Prepare, Info) ->
  ignore.
