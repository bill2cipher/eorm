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
  load_keys/2,
  execute/2,
  exec_prepare/3
]).


prepare(Action, Spec) ->
  Prepare = prepare_name(Spec#data_spec.name, Action),
  case lib_mysql:build_prepare(Action, Spec) of
    {error, Reason} -> {error, Reason};
    Cmd ->
      lib_mysql:prepare(Prepare, Cmd),
      {ok, Prepare}
  end.

load_keys(Spec) ->
  Cmd = lib_mysql:build_loadkeys(Spec),
  case lib_mysql:execute(Cmd) of

  end

prepare_name(TableName, Action) ->
  list_to_atom(io_lib:format("~s_~s", [TableName, Action])).
