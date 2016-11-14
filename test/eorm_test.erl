%%%-------------------------------------------------------------------
%%% @author jacky
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十一月 2016 15:33
%%%-------------------------------------------------------------------
-module(eorm_test).
-author("jacky").
-include("eorm_test.hrl").
-export([people/0]).

people() ->
  people_basic(),
  people_big().

people_init() ->
  application:ensure_all_started(eorm),
  code:ensure_loaded(people_encode),
  {people_encode, Bin, _} = code:get_object_code(people_encode),
  Spec = #data_spec{
    name = "people",
    key = #people.id,
    type = tuple,
    module = {people_encode, Bin},
    fields = [{1, ignore, people},
      {#people.id, "id", undefined},
      {#people.account, "account", undefined},
      {#people.daily_data, "daily_data", undefined},
      {#people.create_time, "create_time", undefined}]
  },
  ok = eorm:new(people, Spec, []).

people_basic() ->
  people_init(),
  B = #people{id = 2, daily_data = {a,b,c}, account = "111", create_time = 12},
  ok = eorm:insert(people, #people{id = 1, daily_data = {a,b,c}, account = "111", create_time = 12}),
  ok = eorm:insert(people, #people{id = 2, daily_data = {a,b,c}, account = "111", create_time = 12}),
  {ok, B} = eorm:lookup(people, 2),
  C = B#people{daily_data = {c, d, e}},
  {ok, C} = eorm:update_element(people, 2, [{#people.daily_data, {c,d,e}}]),
  ok = eorm:delete(people, 2),
  ok = eorm:delete(people, 2),
  ok = eorm:close(people).

people_big() ->
  people_init(),
  P = #people{id = 2, daily_data = {a,b,c}, account = "111", create_time = 12},
  [eorm:insert(people, P#people{id = I, create_time = I + 12}) || I <- lists:seq(200, 20000)],
  eorm:close(people).
