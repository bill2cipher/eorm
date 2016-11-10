%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(mod_table).
-author("jellybean4").
-include("eorm_internal.hrl").

%% API
-export([lookup/2,
  insert/2,
  insert_new/2,
  delete/2,
  update_element/3]).

-spec(lookup(Ref::#table_ref{}, Key::any()) -> {ok, undefined | term()} | {error, any()}).
lookup(#table_ref{pid = PID, spec = Spec}, Key) ->
  Key2 = lib_spec:encode_key(Spec, Key),
  case gen_server:call(PID, {lookup, Key2, util:unixtime()}) of
    {ok, Data} -> {ok, lib_spec:decode_data(Spec, Data)};
    Error -> Error
  end.

-spec(insert(Ref::#table_ref{}, Data::tuple() | map()) -> ok | {error, any()}).
insert(#table_ref{pid = PID, spec = Spec}, Data) ->
  case lib_spec:check_spec_item(Spec, Data) of
    true ->
      Data2 = lib_spec:encode_data(Spec, Data),
      gen_server:call(PID, {insert, Data2, util:unixtime()});
    Error -> Error
  end.

-spec(insert_new(Ref::#table_ref{}, Data::tuple() | map()) -> ok | {error, any()}).
insert_new(#table_ref{pid = PID, spec = Spec}, Data) ->
  case lib_spec:check_spec_item(Spec, Data) of
    true ->
      Data2 = lib_spec:encode_data(Spec, Data),
      gen_server:call(PID, {insert_new, Data2, util:unixtime()});
    Error -> Error
  end.

-spec(delete(Ref::#table_ref{}, Key::any()) -> ok | {error, any()}).
delete(#table_ref{pid = PID, spec = Spec}, Key) ->
  Key2 = lib_spec:encode_key(Spec, Key),
  gen_server:call(PID, {delete, Key2, util:unixtime()}).

-spec(update_element(Ref::#table_ref{}, Key::any(), ElementSpec::tuple() | list()) ->
  {error, any()} | {ok, Data::any()}).
update_element(Ref, Key, ElementSpec) when is_tuple(ElementSpec) ->
  update_element(Ref, Key, [ElementSpec]);
update_element(#table_ref{pid = PID, spec = Spec}, Key, ElementSpec)
    when is_list(ElementSpec) ->
  case lib_spec:check_element_spec(Spec, ElementSpec) of
    {false, Reason} ->
      ?ERROR("check element spec ~p failed, reason ~p", [Reason]),
      {error, Reason};
    true ->
      ElementSpec2 = lib_spec:encode_element_spec(Spec, ElementSpec),
      ElementSpec3 = lib_table:filter_dup_element_spec(ElementSpec2),
      Key2 = lib_spec:encode_key(Spec, Key),
      case gen_server:call(PID, {update_element, Key2, ElementSpec3, util:unixtime()}) of
        {ok, Data} -> {ok, lib_spec:decode_data(Spec, Data)};
        Error -> Error
      end
  end.
