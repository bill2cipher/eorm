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
-include_lib("eunit/include/eunit.hrl").
-include_lib("emysql/include/emysql.hrl").

-define(ERROR(F), lager:error(F)).
-define(ERROR(F, A), lager:error(F, A)).
-define(DEBUG(F), lager:debug(F)).
-define(DEBUG(F, A), lager:debug(F, A)).
-define(INFO(F), lager:info(F)).
-define(INFO(F, A), lager:info(F, A)).
-define(FORMAT_BIN(F, A), list_to_binary(io_lib:format(F, A))).

-define(REGISTRY, registry_ets).
-define(STATUS_NOTIFY, status_notify_ets).

-define(MYSQL_POOL_NAME, eorm_emysql_pool).
-define(MYSQL_POOL_SIZE, 100).

-define(FLUSH_INTERVAL, 5 * 60 * 1000).
-define(FLUSH_PROC_CNT, 100).

-define(MAX_RESTARTS, 10).
-define(MAX_SECONDS_RESTARTS, 30).
-define(CACHE_DIR, "/tmp/mgame/cache/").

-define(DBModule, eorm_mod_mysql).
-define(DB_ACTION_LOOKUP, lookup).
-define(DB_ACTION_INSERT, insert).


-type table_status() :: init | run | close.
-define(TABLE_STATUS_INIT, init).
-define(TABLE_STATUS_RUN, run).
-define(TABLE_STATUS_CLOSE, close).
-define(TABLE_STATUS_DOWN, down).

-record(table_ref, {name, pid, spec, status}).
-record(part_table_ref, {id, pid, spec}).

-record(data_cache, {key, data, ts, ver}).
-record(exec_info, {key, xmit, resent_ts, data, action, ver}).
-record(db_ok_rslt, {}).
-record(db_data_rslt, {fields, rows}).
