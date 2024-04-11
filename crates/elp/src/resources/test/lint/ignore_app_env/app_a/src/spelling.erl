-module(spelling).
% elp:ignore W0013 (misspelled_attribute)
-dyalizer({nowarn_function, [{nowarn_function, spelling_test, 0}]}).
-export([spelling_test/0]).

spelling_test() -> ok.
