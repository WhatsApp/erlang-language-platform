% Dummy module for code completion tests.
% It is an invalid module as-is, to mimick real-life code editing.
-module(code_completion).
-compile(nowarn_missing_spec).

f() ->
    % When `:` is typed, lists functions (all, any, append...)
    % should be proposed.
    % It can also be trigerred with CONTROL-SPACE.
    lists:
