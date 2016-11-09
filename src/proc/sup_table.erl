%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:11
%%%-------------------------------------------------------------------
-module(sup_table).
-author("jellybean4").
-include("eorm_internal.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0,
  start_table/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec(start_table(Table::atom(), Spec::spec(), Opts::list()) ->
  {ok, pid()} | {error, any()}).
start_table(Table, Spec, Opts) ->
  supervisor:start_child(?SERVER, [Table, Spec, Opts]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  SupFlags = {simple_one_for_one, ?MAX_RESTARTS, ?MAX_SECONDS_RESTARTS},
  ProcCache = {proc_table, {proc_table, start_link, []},
    transient, infinity, worker, [proc_table]},

  {ok, {SupFlags, [ProcCache]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
