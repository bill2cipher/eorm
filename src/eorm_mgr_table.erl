%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 十一月 2016 16:24
%%%-------------------------------------------------------------------
-module(eorm_mgr_table).
-author("jellybean4").
-include("eorm_internal.hrl").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  name :: atom(),
  spec :: spec(),
  ets  :: ets:tab(),
  dets :: dets:tab_name(),
  opts :: list(),
  status :: table_status(),
  lookup = undefined :: string(),
  replace = undefined :: string(),
  children = [] :: list()
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
-spec(start_link(Table::atom(), Spec::spec(), Opts::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Table, Spec, Opts) ->
  gen_server:start_link(?MODULE, [Table, Spec, Opts], []).

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
init([Table, Spec, _Opts]) ->
  timer:send_after(0, self(), clean_dets),
  erlang:process_flag(trap_exit, true),
  State = #state{name = Table, spec = Spec, ets = init_cache(Table), status = ?TABLE_STATUS_INIT},
  case init_table([fun init_dets/1, fun init_prepare/1], State) of
    {ok, State2}    ->
      eorm_server:update_table_status(Table, ?TABLE_STATUS_INIT),
      {ok, State2};
    {error, Reason} -> {stop, Reason}
  end.

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
handle_info(clean_dets, State) ->
  #state{ets = EtsTable, dets = DetsTable, spec = Spec, name = Table} = State,
  TravFun = fun(#exec_info{data = Data, action = ?DB_ACTION_INSERT}) ->
    ets:insert(EtsTable, Data)
  end,
  dets:traverse(TravFun, DetsTable),
  TableCnt = Spec#data_spec.proc_cnt,
  Args = build_child_args(State),
  Children = [eorm_proc_table:start_link(Table, ID, Args) || ID <- lists:seq(1, TableCnt)],
  case lists:keyfind(error, 1, Children) of
    false ->
      Children2 = lists:zip([CPID || {ok, CPID} <- Children], lists:seq(1, TableCnt)),
      eorm_server:update_table_status(Table, ?TABLE_STATUS_RUN),
      State#state{children = Children2};
    _     ->
      ?ERROR("not all children start success, reason ~p", [R || {error, R} <- Children]),
      Children2 = [CPID || {ok, CPID} <- Children],
      eorm_server:update_table_status(Table, ?TABLE_STATUS_CLOSE),
      [CPID ! {stop, ?ER_MGR_START_CHILDREN_ERROR} || CPID <- Children2],
      {stop, shutdown}
  end;
handle_info({'EXIT', From, Reason}, State = #state{children = Children, name = Table}) ->
  case lists:keyfind(From, 1, Children) of
    false -> {noreply, State};
    {From, ID} ->
      ?ERROR("table ~p part ~p stopped, reason ~p. Restarting...", [Table, ID, Reason]),
      Args = build_child_args(State),
      case eorm_proc_table:start_link(Table, ID, Args) of
        {ok, PID} ->
          Children2 = lists:keyreplace(ID, 2, Children, {PID, ID}),
          {noreply, State#state{children = Children2}};
        {error, Reason} ->
          ?ERROR("restarting table ~p part ~p failed reason ~p, closing table...", [Table, ID, Reason]),
          eorm_server:update_table_status(Table, ?TABLE_STATUS_CLOSE),
          {stop, shutdown}
      end
  end;
handle_info(flush, State) ->
  proc_flush(State),
  {noreply, State};
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
-spec(init_table(Funs::list(), State::#state{}) -> {ok, #state{}} | {error, any()}).
init_table([], State) -> {ok, State};
init_table([F|L], State) ->
  case F(State) of
    {ok, State2} -> init_table(L, State2);
    Error -> Error
  end.

-spec(init_cache(Table::atom()) -> ets:tab()).
init_cache(Table) ->
  ets:new(Table, [public, {keypos, #data_cache.key}]).

-spec(init_dets(State::#state{}) -> {error, any()} | {ok, #state{}}).
init_dets(State = #state{name = Table}) ->
  Dir = application:get_env(eorm, cache_dir, ?CACHE_DIR),
  TableFile = Dir ++ "/" ++ atom_to_list(Table),
  case dets:open_file(Table, [{file, TableFile}, {keypos, #data_cache.key}]) of
    {ok, DetsTable} -> {ok, State#state{dets = DetsTable}};
    Error -> Error
  end.

-spec(init_prepare(State::#state{}) -> {ok, #state{}}).
init_prepare(State) ->
  #state{spec = Spec} = State,
  Lookup = case eorm_lib_db:prepare(?DB_ACTION_LOOKUP, Spec) of
    {ok, LN} -> eorm_lib_table:set_prepare(?DB_ACTION_LOOKUP, LN), LN;
    _ -> undefined
  end,
  Insert = case eorm_lib_db:prepare(?DB_ACTION_INSERT, Spec) of
    {ok, IN} -> eorm_lib_table:set_prepare(?DB_ACTION_INSERT, IN), IN;
    _ -> undefined
  end,
  {ok, State#state{lookup = Lookup, replace = Insert}}.

build_child_args(State) ->
  #state{ets = EtsTable, dets = DetsTable, spec = Spec, opts = Opts, replace = Replace,
    lookup = Lookup} = State,
  [Spec, Opts, Replace, Lookup, EtsTable, DetsTable, self()].

-spec(proc_flush(State::#state{}) -> #state{}).
proc_flush(State = #state{dets = DetsTable}) ->
  case dets:match(DetsTable, '$1', ?FLUSH_PROC_CNT) of
    '$end_of_table' -> ignore;
    {error, Reason} ->
      ?ERROR("dets ~p match failed, reason ~p", [DetsTable, Reason]);
    {Infos, Cont} ->
      Finished = proc_flush2(State, Infos, Cont, []),
      ?DEBUG("proc flush success for ~p", [Finished]),
      [dets:match_delete(DetsTable, #exec_info{key = K, ver = V}) || {K, V} <- Finished]
  end,
  State.

proc_flush2(State = #state{dets = DetsTable, spec = Spec}, Infos, Cont, Finished) ->
  ExecRslt = [eorm_proc_table:proc_execinfo(I, Spec) || [I] <- Infos],
  Finished2 = [{K, V} || {ok, K, V} <- ExecRslt] ++ Finished,
  case dets:match(Cont) of
    '$end_of_table' -> Finished2;
    {error, Reason} ->
      ?ERROR("dets ~p match failed, reason ~p", [DetsTable, Reason]),
      Finished2;
    {Infos2, Cont2} ->
      proc_flush2(State, Infos2, Cont2, Finished2)
  end.

