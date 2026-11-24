-module(otp29_record_info).
-export([f/0]).
-record(used, {field}).
-record(unused, {field}).

f() -> {record_info(size, used), record_info(fields, used)}.
