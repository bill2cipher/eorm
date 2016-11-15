%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(eorm_proc_table).
-author("jellybean4").
-include("eorm_internal.hrl").
-behaviour(gen_server).

%% API
-export([start_link/3,
  proc_execinfo/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  id :: integer(),
  name :: atom(),
  spec :: spec(),
  ets  :: ets:tab(),
  dets :: dets:tab_name(),
  mgr_pid :: pid(),

  lookup  :: string(),
  replace :: string()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Table::atom(), Spec::spec(), Args::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Table, ID, Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Table, ID, Args], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Table, ID, Args]) ->
  erlang:process_flag(trap_exit, true),
  [Spec, _, Replace, Lookup, EtsTable, DetsTable, MgrPID] = Args,
  State = #state{name = Table, id = ID, spec = Spec, replace = Replace,
    lookup = Lookup, ets = EtsTable, dets = DetsTable, mgr_pid = MgrPID},
  eorm_server:update_part_ref(Table, ID),
  ?IF(Lookup =:= undefined, ignore, eorm_lib_table:set_prepare(?DB_ACTION_LOOKUP, Lookup)),
  ?IF(Replace =:= undefined, ignore, eorm_lib_table:set_prepare(?DB_ACTION_INSERT, Replace)),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({lookup, Key, Current}, _From, State) ->
  case proc_lookup(State, Key) of
    {ok, Data} ->
      ets:update_element(State#state.ets, Key, [{#data_cache.ts, Current}]),
      {reply, {ok, Data}, State};
    Error -> {reply, Error, State}
  end;
handle_call({insert, Data, Current}, _From, State) ->
  #state{ets = EtsTable, dets = DetsTable, spec = Spec} = State,
  KeyValue = eorm_lib_spec:key_value(Spec, Data),
  Version = case ets:lookup_element(EtsTable, KeyValue, #data_cache.ver) of
    [] -> 1;
    [V] -> V + 1
  end,
  ets:insert(EtsTable, #data_cache{key = KeyValue, ts = Current, data = Data, ver = Version}),
  Info = #exec_info{key = KeyValue, xmit = 0, resent_ts = Current, data = Data, ver = Version,
    action = ?DB_ACTION_INSERT},
  case proc_execinfo(Info, Spec) of
    {ok, _, _} -> ignore;
    {error, _} -> dets:insert(DetsTable, Info)
  end,
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({stop, Reason}, State = #state{name = Name, id = ID}) ->
  ?INFO("table ~p id ~p process shutdown, reason ~p", [Name, ID, Reason]),
  {stop, {shutdown, Reason}, State};
handle_info({'EXIT', From, Reason}, State = #state{mgr_pid = From, name = Table}) ->
  ?ERROR("table ~p mgr process ~p exit, reason ~p, stopping proc table", [Table, From, Reason]),
  {stop, shutdown, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(proc_lookup(State::#state{}, Key::any()) ->
  {error, any()} | {ok, undefined} | {ok, term()}).
proc_lookup(State = #state{ets = EtsTable}, Key) ->
  case ets:lookup(EtsTable, Key) of
    [#data_cache{data = Data}] -> {ok, Data};
    [] -> proc_db_lookup(State, Key)
  end.

-spec(proc_db_lookup(State::#state{}, Key::any()) ->
  {error, any()} | {ok, undefined} | {ok, term()}).
proc_db_lookup(#state{lookup = Lookup, ets = EtsTable, spec = Spec}, Key) ->
  Rslt = case Lookup of
    undefined -> eorm_lib_db:execute(Spec, #exec_info{key = Key, action = ?DB_ACTION_LOOKUP});
    _ -> eorm_lib_db:exec_prepare(Lookup, Spec, #exec_info{key = Key, action = ?DB_ACTION_LOOKUP})
  end,
  case Rslt of
    {ok, #db_data_rslt{}} ->
      Data = eorm_lib_table:build_data(Spec, Rslt),
      ets:insert(EtsTable, #data_cache{key = Key, data = Data}), {ok, Data};
    {ok, #db_ok_rslt{}} ->
      {error, ?ER_RESULT_FORMAT_ERROR};
    Error -> Error
  end.

proc_execinfo(Info = #exec_info{action = Action, key = Key, ver = Ver}, Spec) ->
  Rslt = case eorm_lib_table:get_prepare(Action) of
    undefined -> eorm_lib_db:execute(Spec, Info);
    Prepare   -> eorm_lib_db:exec_prepare(Prepare, Spec, Info)
  end,
  case Rslt of
    {ok, _} -> {ok, Key, Ver};
    Error   -> Error
  end.
