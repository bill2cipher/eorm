%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 22:51
%%%-------------------------------------------------------------------
-author("jellybean4").

-type spec() :: #data_spec{}.
-type data() :: list() | tuple().

-record(data_spec, {
  name :: string(),
  type :: tuple | map,
  fields :: {integer(), string(), undefined} | {integer(), ignore, any()},
  key :: integer(),
  module :: {module(), binary()}
}).
