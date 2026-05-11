-module(my_mock).
-export([setup/2]).

%% Stub for a custom mock library that accepts [Module] lists.
setup(_ModuleOrModules, _Opts) ->
    ok.
