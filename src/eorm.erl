%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(eorm).
-author("jellybean4").
-include("eorm_internal.hrl").
%% API
-export([
  new/3,
  close/1,
  lookup/2,
  insert/2
]).

-spec(new(Table::atom(), Spec :: spec(), Opts::list()) ->
  {error, any()} | ok).
new(Table, Spec, Opts) ->
  case eorm_server:new_table(Table, Spec, Opts) of
    {error, Reason} -> {error, Reason};
    {ok, _Ref} -> ok
  end.

-spec(close(Table::atom()) -> {error, any()} | ok).
close(Table) ->
  case eorm_server:close_table(Table) of
    {error, Reason} -> {error, Reason};
    ok -> ok
  end.

-spec(lookup(Table::atom(), Key ::any()) ->
  {error, any()} | {ok, undefined} | {ok, Data :: any()}).
lookup(Table, Key) ->
  case eorm_server:get_table(Table) of
    {error, Reason} -> {error, Reason};
    {ok, Ref} -> eorm_mod_table:lookup(Ref, Key)
  end.

-spec(insert(Table::atom(), Data::data()) ->
  {error, any()} | ok).
insert(Table, Data) ->
  case eorm_server:get_table(Table) of
    {error, Reason} -> {error, Reason};
    {ok, Ref} -> eorm_mod_table:insert(Ref, Data)
  end.
