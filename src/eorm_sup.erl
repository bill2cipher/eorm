%%%-------------------------------------------------------------------
%% @doc eorm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eorm_sup).

-behaviour(supervisor).
-include("eorm_internal.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  case lib_db:start() of
    {error, Reason} ->
      ?ERROR("db service start failed, reason ~p", [Reason]),
      exit(Reason);
    ok ->
      ets:new(?REGISTRY, [public, named_table, {keypos, #table_ref.name}]),
      ets:new(?STATUS_NOTIFY, [public, named_table, {keypos, 1}]),
      supervisor:start_link({local, ?SERVER}, ?MODULE, [])
  end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = {one_for_one, ?MAX_RESTARTS, ?MAX_SECONDS_RESTARTS},
  SupCache = {eorm_sup_table, {eorm_sup_table, start_link, []},
    transient, infinity, supervisor, [eorm_sup_table]},
  EormServer = {eorm_server, {eorm_server, start_link, []},
    transient, infinity, worker, [eorm_server]},
  {ok, {SupFlags, [SupCache, EormServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
