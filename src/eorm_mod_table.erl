%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(eorm_mod_table).
-author("jellybean4").
-include("eorm_internal.hrl").

%% API
-export([
  lookup/2,
  insert/2]).

-spec(lookup(Ref::#part_table_ref{}, Key::any()) -> {ok, undefined | term()} | {error, any()}).
lookup(#part_table_ref{pid = PID, spec = Spec}, Key) ->
  Key2 = eorm_lib_spec:encode_key(Spec, Key),
  case gen_server:call(PID, {lookup, Key2, util:unixtime()}) of
    {ok, Data} -> {ok, eorm_lib_spec:decode_data(Spec, Data)};
    Error -> Error
  end.

-spec(insert(Ref::#part_table_ref{}, Data::tuple() | map()) -> ok | {error, any()}).
insert(#part_table_ref{pid = PID, spec = Spec}, Data) ->
  case eorm_lib_spec:check_spec_item(Spec, Data) of
    true ->
      Data2 = eorm_lib_spec:encode_data(Spec, Data),
      gen_server:call(PID, {insert, Data2, util:unixtime()});
    Error -> Error
  end.
