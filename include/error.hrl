%%%-------------------------------------------------------------------
%%% @author jellybean4
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十一月 2016 0:00
%%%-------------------------------------------------------------------
-author("jellybean4").

-define(ER_TABLE_EXIST, table_exist).
-define(ER_TABLE_NOT_EXIST, table_not_exist).
-define(ER_TABLE_STATUS_NOT_RUN, table_not_run).
-define(ER_TABLE_IS_CLOSE, table_is_close).

-define(ER_SPEC_TYPE_ERROR, spec_type_error).
-define(ER_SPEC_FIELD_TYPE_ERROR, spec_field_type_error).
-define(ER_SPEC_FIELDS_ERROR, spec_fields_error).
-define(ER_SPEC_KEY_NOT_FOUND, spec_key_not_found).
-define(ER_SPEC_MODULE_TYPE_ERROR, spec_module_type_error).

-define(ER_DATA_NOT_MATCH_SPEC, data_not_match_spec).
-define(ER_DATA_EXIST, data_exist).
-define(ER_DATA_NOT_EXIST, data_not_eixst).
-define(ER_LOAD_ENCODE_MODULE_FAILED, load_encode_module_failed).


-define(ER_UNSUPPORTED_PREPARE, unsupported_prepare).
-define(ER_UNSUPPORTED_EXECUTE, unsupported_execute).
-define(ER_RESULT_FORMAT_ERROR, result_format_error).
-define(ER_UPDATE_ELEMENT_NOT_FOUND, update_element_not_found).
-define(ER_UPDATE_IGNORE_ELEMENT, update_ignore_element).

-define(ER_MGR_START_CHILDREN_ERROR, mgr_start_children_error).
