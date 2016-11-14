%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(eorm_lib_table).
-author("jellybean4").
-include("eorm_internal.hrl").
%% API
-export([
  set_prepare/2,
  get_prepare/1,
  resent_ts/2,
  update_data/2,
  build_data/2,
  swap_dets_name/1,
  filter_dup_element_spec/1]).

set_prepare(Action, Name) ->
  util:set_dict(Action, Name).

get_prepare(Action) ->
  util:get_dict(Action, undefined).

resent_ts(Xmit, Current) ->
  if
    Xmit =< 5 -> Xmit * 30 + 30 + Current;
    Xmit =< 10 -> (5 * 30 + 30) + (Xmit - 5) * 60 + Current;
    true -> (5 * 30 + 30) + 5 * 60 + (Xmit - 10) * 120 + Current
  end.

update_data(Data, ElementSpec) when is_tuple(Data) ->
  update_tuple_data(Data, ElementSpec);
update_data(Data, ElementSpec) when is_map(Data) ->
  update_map_data(Data, ElementSpec);
update_data(Data, _) -> Data.

update_tuple_data(Data, []) -> Data;
update_tuple_data(Data, [{K, V} | L]) ->
  Data2 = setelement(K, Data, V),
  update_tuple_data(Data2, L).

update_map_data(Data, []) -> Data;
update_map_data(Data, [{K, V} | L]) ->
  Data2 = Data#{K => V},
  update_map_data(Data2, L).

filter_dup_element_spec(ElementSpec) ->
  filter_dup_element_spec(ElementSpec, []).
filter_dup_element_spec([], Rslt) -> Rslt;
filter_dup_element_spec([{K, V} | L], Rslt) ->
  case lists:keyfind(K, 1, Rslt) of
    false -> filter_dup_element_spec(L, [{K, V} | Rslt]);
    _     -> filter_dup_element_spec(L, Rslt)
  end.

build_data(#data_spec{type = tuple, fields = Fields}, #db_data_rslt{rows = Rows, fields = FieldList}) ->
  ?DEBUG("build tuple data for rows ~p and fields ~p", [Rows, FieldList]),
  {Rslt, _} = lists:foldl(fun({_, N, D}, {Data, Idx}) ->
    case N of
      ignore -> {[D | Data], Idx};
      _      -> {[lists:nth(Idx, Rows) | Data], Idx + 1}
    end end, {[], 1}, Fields),
  list_to_tuple(lists:reverse(Rslt));
build_data(#data_spec{type = map, fields = Fields}, #db_data_rslt{rows = Rows, fields = FieldList}) ->
  ?DEBUG("build tuple data for rows ~p and fields ~p", [Rows, FieldList]),
  {Rslt, _} = lists:foldl(fun({K, N, D}, {Data, Idx}) ->
    case N of
      ignore -> {Data#{K => D}, Idx};
      _      -> {Data#{K => lists:nth(Idx, Rows)}, Idx + 1}
    end end, {#{}, 1}, Fields),
  Rslt.

swap_dets_name(Table) ->
  erlang:binary_to_atom(?FORMAT_BIN("~s_swap", [Table]), latin1).
