-module(otp29_imported_fun_capture).
-import(lists, [reverse/1]).
-export([bad/0, good/0]).

bad() -> fun reverse/1.
good() -> {fun lists:reverse/1, fun local/0, fun length/1}.
local() -> ok.
