%%%-------------------------------------------------------------------
%%% @author jacky
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十一月 2016 15:34
%%%-------------------------------------------------------------------
-author("jacky").
-include_lib("eunit/include/eunit.hrl").
-include("eorm_internal.hrl").

-record(people, {id, account, create_time, daily_data}).