%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(lib_mysql).
-author("jellybean4").
-include("eorm_internal.hrl").
%% API
-export([
  prepare/2
]).

-export([
  build_prepare/2,
  build_loadkeys/1
]).

-spec(prepare(Prepare::atom(), Cmd::string()) -> ok).
prepare(Prepare, Cmd) ->
  emysql:prepare(Prepare, Cmd).

build_loadkeys(Spec) ->
  ?FORMAT_BIN("SELECT ~s FROM ~s", lib_spec:key_name(Spec), Spec#data_spec.name).

-spec(build_prepare(Action::string(), Spec::spec()) -> {error, any()} | binary()).
build_prepare(?DB_ACTION_INSERT, #data_spec{fields = Fields, name = TableName}) ->
  FieldList = string:join([N || {_, N, _} <- Fields, N =/= ignore], ", "),
  Args = string:join(lists:duplicate(length(FieldList), "?"), ", "),
  ?FORMAT_BIN("REPLACE INTO ~s (~s) VALUES(~s)", [TableName, FieldList, Args]);

build_prepare(?DB_ACTION_DELETE, Spec) ->
  KeyName = lib_spec:key_name(Spec),
  ?FORMAT_BIN("DELETE FROM ~s WHERE ~p = ?", [Spec#data_spec.name, KeyName]);

build_prepare(?DB_ACTION_LOOKUP, Spec = #data_spec{name = TableName, fields = Fields}) ->
  KeyName = lib_spec:key_name(Spec),
  SelectFields = string:join([N || {_, N, _} <- Fields, N =/= ignore], ", "),
  ?FORMAT_BIN("SELECT ~s FROM ~p WHERE ~p = ?", [SelectFields, TableName, KeyName]);

build_prepare(?DB_ACTION_UPDATE, _) ->
  {error, ?ER_UNSUPPORTED_PREPARE}.