%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:02
%%%-------------------------------------------------------------------
-module(eorm_server).
-author("jellybean4").
-include("eorm_internal.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
  new_table/3,
  close_table/1,
  get_table/1,
  update_table_status/2,
  update_part_ref/2
]).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {monitors}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(new_table(Table::atom(), Spec::spec(), Opts::list()) ->
  {ok, #table_ref{}} | {error, any()}).
new_table(Table, Spec, Opts) when is_atom(Table) andalso is_list(Opts) ->
  case eorm_lib_spec:check_spec(Spec) of
    {false, Reason} -> {error, Reason};
    true ->
      case eorm_lib_spec:load_spec_module(Spec) of
        {error, Reason} -> {error, Reason};
        ok -> new_table_proc(Table, Spec, Opts)
      end
  end.

new_table_proc(Table, Spec, Opts) ->
  Spec2 = Spec#data_spec{module = {eorm_lib_spec:module_name(Spec), undefined}},
  Spec3 = eorm_lib_spec:sort_fields(Spec2),
  case gen_server:call(?SERVER, {new_table, Table, Spec3, Opts}, infinity) of
    {error, Reason} -> {error, Reason};
    {ok, Ref} -> wait4_notify(Table, ?TABLE_STATUS_RUN), {ok, Ref}
  end.

-spec(close_table(Table::atom()) -> ok | {error, any()}).
close_table(Table) when is_atom(Table) ->
  case gen_server:call(?SERVER, {close_table, Table}, infinity) of
    {error, Reason} -> {error, Reason};
    ok ->
      wait4_notify(Table, ?TABLE_STATUS_DOWN),
      ok
  end.

-spec(get_table(Table::atom()) -> {error, any()} | {ok, Ref::any()}).
get_table(Table) when is_atom(Table) ->
  case ets:lookup(?REGISTRY, Table) of
    [] -> {error, ?ER_TABLE_NOT_EXIST};
    [Ref = #table_ref{status = ?TABLE_STATUS_RUN}] -> {ok, Ref};
    [_] -> {error, ?ER_TABLE_STATUS_NOT_RUN}
  end.

-spec(update_table_status(Table::atom(), Status::table_status()) -> ok).
update_table_status(Table, Status) when is_atom(Table) andalso is_atom(Status) ->
  gen_server:cast(?SERVER, {update_status, Table, Status}),
  ok.

update_part_ref(Table, ID) ->
  gen_server:cast(?SERVER, {update_part_status, Table, ID, self()}),
  ok.

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
init([]) ->
  [Finished, Monitors] = ets:foldl(fun(#table_ref{pid = PID, name = Name}, [F, M]) ->
    case erlang:is_process_alive(PID) of
      true  -> [F, [{erlang:monitor(process, PID), Name} | M]];
      false -> [[Name | F], M]
    end
  end, [[], []], ?REGISTRY),
  [ets:delete(?REGISTRY, T) || T <- Finished],
  {ok, #state{monitors = Monitors}}.

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
handle_call({new_table, Table, Spec, Opts}, {ReqPID, _}, State = #state{monitors = Monitors}) ->
  case ets:lookup(?REGISTRY, Table) of
    [#table_ref{}] -> {reply, {error, ?ER_TABLE_EXIST}, State};
    [] ->
      case start_table(Table, Spec, Opts) of
        {error, Reason} -> {reply, {error, Reason}, State};
        {ok, Ref = #table_ref{pid = PID}} ->
          registry_notify(Table, ?TABLE_STATUS_RUN, ReqPID),
          notify_registers(Table, ?TABLE_STATUS_INIT),
          MonitorRef = erlang:monitor(process, PID),
          {reply, {ok, Ref}, State#state{monitors = [{MonitorRef, Table} | Monitors]}}
      end
  end;
handle_call({close_table, Table}, {ReqPID, _}, State) ->
  case ets:lookup(?REGISTRY, Table) of
    [] -> {reply, {error, ?ER_TABLE_NOT_EXIST}, State};
    [#table_ref{status = ?TABLE_STATUS_CLOSE}] -> {reply, {error, ?ER_TABLE_IS_CLOSE}, State};
    [#table_ref{pid = PID}] ->
      gen_server:cast(PID, {stop, close_table}),
      registry_notify(Table, ?TABLE_STATUS_DOWN, ReqPID),
      notify_registers(Table, ?TABLE_STATUS_CLOSE),
      ets:update_element(?REGISTRY, Table, [{#table_ref.status, ?TABLE_STATUS_CLOSE}]),
      {reply, ok, State}
  end;
handle_call(Request, _From, State) ->
  ?INFO("? recv unknown call request ~p", [?SERVER, Request]),
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
handle_cast({update_status, Table, Status}, State) ->
  ?DEBUG("table ~p status changed into ~p", [Table, Status]),
  case ets:lookup(?REGISTRY, Table) of
    [] -> {noreply, State};
    [#table_ref{}] ->
      ets:update_element(?REGISTRY, Table, [{#table_ref.status, Status}]),
      notify_registers(Table, Status),
      {noreply, State}
  end;
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
handle_info({'DOWN', Ref, process, _PID, Reason}, State = #state{monitors = Monitors}) ->
  case lists:keyfind(Ref, 1, Monitors) of
    false -> {noreply, State};
    {Ref, Table} ->
      ?INFO("table ~p exit with reason ~p", [Table, Reason]),
      ets:delete(?REGISTRY, Table),
      Monitors2 = lists:keydelete(Ref, 1, Monitors),
      notify_registers(Table, ?TABLE_STATUS_DOWN),
      {noreply, State#state{monitors = Monitors2}}
  end;
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
-spec(start_table(Table::atom(), Spec::spec(), Opts::list()) ->
  {error, any()} | {ok, #table_ref{}}).
start_table(Table, Spec, Opts) ->
  case eorm_sup_table:start_table(Table, Spec, Opts) of
    {error, Reason} -> {error, Reason};
    {ok, PID} ->
      Ref = #table_ref{name = Table, pid = PID, spec = Spec, status = ?TABLE_STATUS_INIT},
      ets:insert(?REGISTRY, Ref),
      {ok, Ref}
  end.

registry_notify(Table, Status, PID) ->
  Key = {Table, Status},
  case ets:lookup(?STATUS_NOTIFY, Key) of
    [] -> ets:insert(?STATUS_NOTIFY, {Key, [PID]});
    [{Key, PList}] ->
      ets:insert(?STATUS_NOTIFY, {Key, [PID | PList]})
  end.

notify_registers(Table, Status) ->
  ?DEBUG("notify table ~p for status change to ~p", [Table, Status]),
  Key = {Table, Status},
  case ets:lookup(?STATUS_NOTIFY, Key) of
    [] -> ignore;
    [{_, PList}] ->
      [P ! Key || P <- PList],
      ets:delete(?STATUS_NOTIFY, Key)
  end.

wait4_notify(Table, Status) ->
  receive
    {Table, Status} -> ignore
  end.
