%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十一月 2016 22:51
%%%-------------------------------------------------------------------
-author("jellybean4").
-include("eorm.hrl").
-include("error.hrl").
-include_lib("emysql/include/emysql.hrl").

-define(ERROR(F), error_logger:error_msg(F)).
-define(ERROR(F, A), error_logger:error_msg(F, A)).
-define(DEBUG(F), error_logger:info_msg(F)).
-define(DEBUG(F, A), error_logger:info_msg(F, A)).
-define(INFO(F), error_logger:info_msg(F)).
-define(INFO(F, A), error_logger:info_msg(F, A)).
-define(IF(C, T, F), case C of true -> T; false -> F end).
-define(FORMAT_BIN(F, A), list_to_binary(io_lib:format(F, A))).

-define(FLUSH_INTERVAL, 5 * 60 * 1000).
-define(FLUSH_PROC_CNT, 100).

-define(MAX_RESTARTS, 10).
-define(MAX_SECONDS_RESTARTS, 30).
-define(CACHE_DIR, "/var/mgame/cache/").

-define(DB_ACTION_LOOKUP, lookup).
-define(DB_ACTION_INSERT, insert).
-define(DB_ACTION_DELETE, delete).
-define(DB_ACTION_UPDATE, update).

-define(REGISTRY, registry_ets).

-type table_status() :: init | run | close.
-define(TABLE_STATUS_INIT, init).
-define(TABLE_STATUS_RUN, run).
-define(TABLE_STATUS_CLOSE, close).

-record(table_ref, {name, pid, spec, status}).

-record(data_cache, {key, data, ts}).
-record(exec_info, {key, xmit, resent_ts, data, action}).
-record(db_ok_rslt, {}).
-record(db_data_rslt, {fields, rows}).
