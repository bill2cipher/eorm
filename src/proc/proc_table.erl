%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 23:03
%%%-------------------------------------------------------------------
-module(proc_table).
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
  status  :: table_status(),
  key_ets :: ets:tab(),
  db_module :: module(),

  lookup  :: string(),
  replace :: string(),
  delete  :: string(),
  update  :: string()
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
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Table, Spec, Opts], []).

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
init([Table, Spec, Opts]) ->
  DBModule = proplists:get_value(db_module, Opts, mod_mysql),
  timer:send_after(0, self(), {clean_dets}),
  State = #state{name = Table, spec = Spec, db_module = DBModule, ets = init_cache(Table),
    status = ?TABLE_STATUS_INIT},
  case init_table([fun init_dets/1, fun init_key_ets/1, fun init_prepare/1], State) of
    {ok, State2} -> {ok, State2};
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
handle_call({lookup, Key, Current}, _From, State) ->
  case ets:lookup(State#state.key_ets, Key) of
    [] -> {reply, {ok, undefined}, State};
    _  ->
      case proc_lookup(State, Key) of
        {ok, Data} ->
          ets:update_element(State#state.ets, Key, [{#data_cache.ts, Current}]),
          {reply, {ok, Data}, State};
        Error -> {reply, Error, State}
      end
  end;
handle_call({insert, Data, Current}, _From, State) ->
  #state{key_ets = KeyTable, ets = EtsTable, dets = DetsTable, spec = Spec} = State,
  KeyValue = lib_spec:key_value(Spec, Data),
  ets:insert(KeyTable, {KeyValue, true}),
  ets:insert(EtsTable, #data_cache{key = KeyValue, ts = Current, data = Data}),
  dets:insert(DetsTable, #exec_info{key = KeyValue, xmit = 0, resent_ts = Current,
    data = Data, action = ?DB_ACTION_INSERT}),
  {reply, ok, State};
handle_call({insert_new, Data, Current}, _From, State) ->
  #state{key_ets = KeyTable, ets = EtsTable, dets = DetsTable, spec = Spec} = State,
  KeyValue = lib_spec:key_value(Spec, Data),
  case ets:lookup(KeyTable, KeyValue) of
    [{KeyValue, true}] -> {reply, {error, ?ER_DATA_EXIST}, State};
    _ ->
      ets:insert(KeyTable, {KeyValue, true}),
      ets:insert(EtsTable, #data_cache{key = KeyValue, ts = Current, data = Data}),
      dets:insert(DetsTable, #exec_info{key = KeyValue, xmit = 0, resent_ts = Current,
        data = Data, action = ?DB_ACTION_INSERT}),
      {reply, ok, State}
  end;
handle_call({delete, Key, Current}, _From, State) ->
  #state{key_ets = KeyTable, ets = EtsTable, dets = DetsTable} = State,
  case ets:lookup(KeyTable, Key) of
    [] -> {reply, ok, State};
    _  ->
      ets:delete(KeyTable, Key),
      ets:delete(EtsTable, Key),
      dets:insert(DetsTable, #exec_info{key = Key, xmit = 0, resent_ts = Current, action = ?DB_ACTION_DELETE}),
      {reply, ok, State}
  end;
handle_call({update_element, Key, ElementSpec, Current}, _From, State) ->
  #state{key_ets = KeyTable, ets = EtsTable, dets = DetsTable} = State,
  case ets:lookup(KeyTable, Key) of
    [] -> {reply, {error, ?ER_DATA_NOT_EXIST}, State};
    _  ->
      case proc_lookup(State, Key) of
        {ok, undefined} -> {reply, {error, ?ER_DATA_NOT_EXIST}, State};
        {ok, Data} ->
          Data2 = lib_table:update_data(Data, ElementSpec),
          ets:update_element(EtsTable, Key, [{#data_cache.ts, Current}, {#data_cache.data, Data2}]),
          dets:insert(DetsTable, #exec_info{key = Key, xmit = 0, resent_ts = Current,
            data = ElementSpec, action = ?DB_ACTION_UPDATE})
      end,
      {reply, {ok, Data2}, State}
  end;
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
handle_cast({stop, Reason}, State = #state{name = Name}) ->
  ?INFO("table ~p process shutdown, reason ~p", [Name, Reason]),
  State2 = proc_flush(State),
  {noreply, {shutdown, Reason}, State2};
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
  State2 = proc_flush(State),
  timer:send_after(?FLUSH_INTERVAL, self(), flush),
  eorm_server:update_table_status(State#state.name, ?TABLE_STATUS_RUN),
  {noreply, State2};
handle_info(flush, State) ->
  State2 = proc_flush(State),
  timer:send_after(?FLUSH_INTERVAL, self(), flush),
  {noreply, State2};
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
  ets:new(Table, [{keypos, #data_cache.key}]).

-spec(init_dets(State::#state{}) -> {error, any()} | {ok, #state{}}).
init_dets(State = #state{name = Table}) ->
  Dir = application:get_env(eorm, cache_dir, ?CACHE_DIR),
  TableFile = Dir ++ "/" ++ atom_to_list(Table),
  case dets:open_file(Table, [{file, TableFile}, {keypos, #data_cache.key}]) of
    {ok, DetsTable} -> {ok, State#state{dets = DetsTable}};
    Error -> Error
  end.

-spec(init_key_ets(State::#state{}) -> {error, any()} | {ok, #state{}}).
init_key_ets(State = #state{name = Table, spec = Spec, db_module = DBModule}) ->
  KeyTable = ets:new(Table, [{keypos, 1}]),
  case lib_db:load_keys(DBModule, Spec) of
    {ok, Rows} ->
      [ets:insert(KeyTable, {K, true}) || [K] <- Rows],
      {ok, State#state{key_ets = KeyTable}};
    Error -> Error
  end.

-spec(init_prepare(State::#state{}) -> {ok, #state{}}).
init_prepare(State) ->
  #state{db_module = DBModule, spec = Spec} = State,
  Lookup = case lib_db:prepare(DBModule, ?DB_ACTION_LOOKUP, Spec) of
    {ok, LN} -> lib_table:set_prepare(?DB_ACTION_LOOKUP, LN), LN;
    _ -> undefined
  end,
  Insert = case lib_db:prepare(DBModule, ?DB_ACTION_INSERT, Spec) of
    {ok, IN} -> lib_table:set_prepare(?DB_ACTION_INSERT, IN), IN;
    _ -> undefined
  end,
  Update = case lib_db:prepare(DBModule, ?DB_ACTION_UPDATE, Spec) of
    {ok, UN} -> lib_table:set_prepare(?DB_ACTION_UPDATE, UN), UN;
    _ -> undefined
  end,
  Delete = case lib_db:prepare(DBModule, ?DB_ACTION_DELETE, Spec) of
    {ok, DN} -> lib_table:set_prepare(?DB_ACTION_DELETE, DN), DN;
    _ -> undefined
  end,
  {ok, State#state{lookup = Lookup, delete = Delete, update = Update, replace = Insert}}.


-spec(proc_lookup(State::#state{}, Key::any()) ->
  {error, any()} | {ok, undefined} | {ok, term()}).
proc_lookup(State = #state{ets = EtsTable}, Key) ->
  case ets:lookup(EtsTable, Key) of
    [#data_cache{data = Data}] -> {ok, Data};
    [] -> proc_db_lookup(State, Key)
  end.

-spec(proc_db_lookup(State::#state{}, Key::any()) ->
  {error, any()} | {ok, undefined} | {ok, term()}).
proc_db_lookup(#state{lookup = Lookup, db_module = DBModule, ets = EtsTable}, Key) ->
  Rslt = case Lookup of
    undefined -> lib_db:execute(DBModule, #exec_info{key = Key});
    _ -> lib_db:exec_prepare(DBModule, Lookup, Key)
  end,
  case Rslt of
    {ok, undefined} -> {ok, undefined};
    {ok, Data} ->
      ets:insert(EtsTable, #data_cache{key = Key, data = Data}), {ok, Data};
    Error -> Error
  end.

-spec(proc_flush(State::#state{}) -> #state{}).
proc_flush(State = #state{dets = DetsTable}) ->
  case dets:match(DetsTable, '$1', ?FLUSH_PROC_CNT) of
    '$end_of_table' -> ignore;
    {error, Reason} ->
      ?ERROR("dets ~p match failed, reason ~p", [DetsTable, Reason]);
    {Infos, Cont} ->
      Finished = proc_flush2(State, Infos, Cont, []),
      [dets:delete(DetsTable, K) || K <- Finished]
  end,
  State.

proc_flush2(State = #state{dets = DetsTable, db_module = DBModule}, Infos, Cont, Finished) ->
  ExecRslt = [proc_flush3(I, DBModule) || I <- Infos],
  Finished2 = [K || {ok, K} <- ExecRslt] ++ Finished,
  case dets:match(Cont) of
    '$end_of_table' -> State;
    {error, Reason} ->
      ?ERROR("dets ~p match failed, reason ~p", [DetsTable, Reason]),
      State;
    {Infos2, Cont2} ->
      proc_flush2(State, Infos2, Cont2, Finished2)
  end.

proc_flush3(Info = #exec_info{action = Action, key = Key}, DBModule) ->
  Rslt = case lib_table:get_prepare(Action) of
    undefined -> lib_db:execute(DBModule, Info);
    Prepare   -> lib_db:exec_prepare(DBModule, Prepare, Info)
  end,
  case Rslt of
    ok -> {ok, Key};
    _  -> {error, Key}
  end.
