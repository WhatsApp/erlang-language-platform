-module(based_float).
-export([values/0]).

%% Based floating point literals (EEP-75), introduced in Erlang/OTP 28.
values() ->
    [16#ff.ff,
     16#abc.def,
     2#101.101,
     2#1.0#e+3,
     16#f.e#e+3,
     10#1.0#E17,
     16#0.0e0,
     16#0.3243f6a8885a30#e+1,
     1_6#000_100.0_0].
