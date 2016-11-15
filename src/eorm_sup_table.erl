%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:11
%%%-------------------------------------------------------------------
-module(eorm_sup_table).
-author("jellybean4").
-include("eorm_internal.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0,
  get_table_mgr/1,
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
  TableMgr = {Table, {eorm_mgr_table, start_link, [Table, Spec, Opts]},
    transient, infinity, worker, [eorm_mgr_table]},
  supervisor:start_child(?SERVER, TableMgr).

get_table_mgr(Table) ->
  Children = supervisor:which_children(?SERVER),
  case lists:keyfind(Table, 1, Children) of
    false -> {error, ?ER_TABLE_NOT_EXIST};
    {Table, PID, _, _} -> PID
  end.
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
  SupFlags = {one_for_one, ?MAX_RESTARTS, ?MAX_SECONDS_RESTARTS},
  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
