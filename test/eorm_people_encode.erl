%%%-------------------------------------------------------------------
%%% @author jacky
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十一月 2016 15:02
%%%-------------------------------------------------------------------
-module(eorm_people_encode).
-author("jacky").
-include("eorm_test.hrl").

%% API
-export([
  hash/2,
  encode/2,
  decode/2]).

hash(Size, Key) -> Key rem Size.

encode(#people.id, ID) -> ID;
encode(#people.account, Account) -> unicode:characters_to_binary(Account);
encode(#people.create_time, Time) -> Time;
encode(#people.daily_data, Daily) -> util:term_to_bitstring(Daily);
encode(_, V) -> V.

decode(#people.id, ID) -> ID;
decode(#people.account, Account) -> unicode:characters_to_list(Account);
decode(#people.create_time, Time) -> Time;
decode(#people.daily_data, Daily) -> util:bitstring_to_term(Daily);
decode(_, V) -> V.

