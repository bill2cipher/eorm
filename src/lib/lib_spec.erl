%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:04
%%%-------------------------------------------------------------------
-module(lib_spec).
-author("jellybean4").
-include("eorm_internal.hrl").
%% API
-export([
  key_value/2,
  key_name/1
]).

-export([
  sort_fields/1,
  check_spec/1,
  check_spec_item/2,
  encode_data/2,
  decode_data/2,
  encode_key/2,
  encode_element_spec/2,
  load_spec_module/1,
  module_name/1]).

key_value(#data_spec{type = tuple, key = Key}, Data) ->
  element(Key, Data);
key_value(#data_spec{type = map, key = Key}, Data) ->
  #{Key := V} = Data, V.

key_name(#data_spec{key = Key, fields = Fields}) ->
  {_, N, _} = lists:keyfind(Key, 1, Fields), N.

module_name(#data_spec{module = {Name, _}}) -> Name.

sort_fields(Spec = #data_spec{fields = Fields, type = tuple}) ->
  Fields2 = lists:keysort(1, Fields),
  Spec#data_spec{fields = Fields2};
sort_fields(Spec = #data_spec{}) -> Spec.

-spec(check_spec(Spec::spec()) -> {false, any()} | true).
check_spec(Spec = #data_spec{name = TableName, fields = Fields, key = Key, module = Module, type = tuple}) ->
  case is_list(TableName) andalso is_list(Fields) andalso is_integer(Key) andalso is_tuple(Module) of
    false -> {false, ?ER_SPEC_FIELD_TYPE_ERROR};
    true  ->
      CheckList = [fun check_tuple_fields/1, fun check_spec_key/1, fun check_spec_module/1],
      check_spec_proc(CheckList, Spec)
  end;
check_spec(Spec = #data_spec{name = TableName, fields = Fields, key = Key, module = Module, type = map}) ->
  case is_list(TableName) andalso is_list(Fields) andalso is_atom(Key) andalso is_tuple(Module) of
    false -> {false, ?ER_SPEC_FIELD_TYPE_ERROR};
    true  ->
      CheckList = [fun check_map_fields/1, fun check_spec_key/1, fun check_spec_module/1],
      check_spec_proc(CheckList, Spec)
  end;
check_spec(_) ->
  {false, ?ER_SPEC_TYPE_ERROR}.

check_spec_proc([], _Spec) -> true;
check_spec_proc([F | L], Spec) ->
  case F(Spec) of
    true -> check_spec_proc(L, Spec);
    Error -> Error
  end.

check_spec_module(Spec) ->
  case Spec#data_spec.module of
    {Name, Bin} when is_atom(Name) andalso is_binary(Bin) -> true;
    _ -> {false, ?ER_SPEC_MODULE_TYPE_ERROR}
  end.

check_spec_key(#data_spec{key = Key, fields = Fields}) ->
  case lists:keyfind(Key, 1, Fields) of
    false -> {error, ?ER_SPEC_KEY_NOT_FOUND};
    true  -> true
  end.

check_tuple_fields(#data_spec{fields = Fields}) ->
  case check_tuple_fields_type(Fields) of
    {false, Reason} -> {false, Reason};
    true ->
      Fields2 = lists:keysort(1, Fields),
      check_tuple_fields_key(Fields2, 1)
  end.

check_tuple_fields_type([]) -> true;
check_tuple_fields_type([{K, N, _} | L]) ->
  case is_integer(K) andalso (is_list(N) orelse N =:= ignore) of
    true  -> check_tuple_fields_type(L);
    false -> {false, ?ER_SPEC_FIELDS_ERROR}
  end;
check_tuple_fields_type([_ | _]) ->
  {false, ?ER_SPEC_FIELDS_ERROR}.

check_tuple_fields_key([], _) -> true;
check_tuple_fields_key([{K, _, _} | L], Idx) ->
  case K =:= Idx of
    true -> check_tuple_fields_key(L, Idx + 1);
    false -> {false, ?ER_SPEC_FIELDS_ERROR}
  end.

check_map_fields(#data_spec{fields = Fields}) ->
  check_map_fields_type(Fields).
check_map_fields_type([]) -> true;
check_map_fields_type([{K, N, _} | L]) ->
  case is_atom(K) andalso (is_list(N) orelse N =:= ignore) of
    true  -> check_map_fields_type(L);
    false -> {false, ?ER_DATA_NOT_MATCH_SPEC}
  end;
check_map_fields_type([_ | _]) ->
  {false, ?ER_SPEC_FIELDS_ERROR}.

check_spec_item(#data_spec{fields = Fields, type = tuple}, Data) ->
  case is_tuple(Data) of
    false -> {false, ?ER_DATA_NOT_MATCH_SPEC};
    true  ->
      case tuple_size(Data) =:= length(Fields) of
        true  -> true;
        false -> {false, ?ER_DATA_NOT_MATCH_SPEC}
      end
  end;
check_spec_item(#data_spec{fields = Fields, type = map}, Data) ->
  case is_map(Data) of
    false -> {false, ?ER_DATA_NOT_MATCH_SPEC};
    true  ->
      case lists:all(fun({K, _, _}) -> maps:is_key(K, Data) end, Fields) of
        false -> {false, ?ER_DATA_NOT_MATCH_SPEC};
        true  -> true
      end
  end.

encode_data(#data_spec{module = {Module, _}, type = tuple}, Data) when is_tuple(Data) ->
  encode_tuple_data(Module, tuple_to_list(Data), 1, []);
encode_data(#data_spec{module = {Module, _}, type = map}, Data) when is_map(Data) ->
  maps:map(fun Module:encode/2, Data);
encode_data(_, Data) -> Data.

encode_tuple_data(_Module, [], _Idx, Rslt) ->
  list_to_tuple(lists:reverse(Rslt));
encode_tuple_data(Module, [V | L], Idx, Rslt) ->
  V2 = Module:encode(Idx, V),
  encode_tuple_data(Module, L, Idx + 1, [V2 | Rslt]).

decode_data(#data_spec{module = {Module, _}, type = tuple}, Data) when is_tuple(Data) ->
  decode_tuple_data(Module, tuple_to_list(Data), 1, []);
decode_data(#data_spec{module = {Module, _}, type = map}, Data) when is_map(Data) ->
  maps:map(fun Module:decode/2, Data);
decode_data(_, Data) -> Data.

decode_tuple_data(_Module, [], _Idx, Rslt) ->
  list_to_tuple(lists:reverse(Rslt));
decode_tuple_data(Module, [V | L], Idx, Rslt) ->
  V2 = Module:decode(Idx, V),
  decode_tuple_data(Module, L, Idx + 1, [V2 | Rslt]).

encode_key(#data_spec{key = Key, module = {Module, _}}, Value) ->
  Module:encode(Key, Value).

encode_element_spec(#data_spec{module = {Module, _}}, ElementSpec) ->
  [{K, Module:encode(K, V)} || {K, V} <- ElementSpec].

load_spec_module(#data_spec{module = {Module, Bin}}) ->
  case code:load_binary(Module, atom_to_list(Module), Bin) of
    {ok, Module} -> ok;
    {error, Reason} -> {error, Reason};
    _ -> {error, ?ER_LOAD_ENCODE_MODULE_FAILED}
  end.
