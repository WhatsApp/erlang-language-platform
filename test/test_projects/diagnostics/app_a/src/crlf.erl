%% @generated
%% Note: marking generated to skip the dos-newlines linter
-module(crlf).
%% This file tests that we report diagnostic locations correctly when the file
%% is using CRLF line endings. As such, it is configured that way, please do not
%% change it.
%% In VS Code, there should be an indicator in the bottom right status bar saying
%% CRLF.  It is only LF, this test should fail.
%%
%% Note: we need enough blank lines so that the extra CR's are as long as the text
%% of the functions we are testing.
-export([h/0]).

unused_fn() -> ok.




h() -> k.










another_fun() -> ok.



%%
