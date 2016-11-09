%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(lib_table).
-author("jellybean4").

%% API
-export([
  set_prepare/2,
  get_prepare/1,
  resent_ts/2,
  update_data/2]).

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
