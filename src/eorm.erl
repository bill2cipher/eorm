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
  insert/2,
  insert_new/2,
  delete/2,
  update_element/3
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
    {ok, Ref} -> mod_table:lookup(Ref, Key)
  end.

-spec(insert(Table::atom(), Data::data()) ->
  {error, any()} | ok).
insert(Table, Data) ->
  case eorm_server:get_table(Table) of
    {error, Reason} -> {error, Reason};
    {ok, Ref} -> mod_table:insert(Ref, Data)
  end.

-spec(insert_new(Table::atom(), Data::data()) ->
  {error, any()} | ok).
insert_new(Table, Data) ->
  case eorm_server:get_table(Table) of
    {error, Reason} -> {error, Reason};
    {ok, Ref} -> mod_table:insert_new(Ref, Data)
  end.

-spec(delete(Table::atom(), Key::any()) ->
  {error, any()} | ok).
delete(Table, Key) ->
  case eorm_server:get_table(Table) of
    {error, Reason} -> {error, Reason};
    {ok, Ref} -> mod_table:delete(Ref, Key)
  end.

-spec(update_element(Table::atom(), Key::any(), ElementSpec::tuple() | list()) ->
  {error, Reason::any()} | {ok, Data::any()}).
update_element(Table, Key, ElementSpec) ->
  case eorm_server:get_table(Table) of
    {error, Reason} -> {error, Reason};
    {ok, Ref} -> mod_table:update_element(Ref, Key, ElementSpec)
  end.
