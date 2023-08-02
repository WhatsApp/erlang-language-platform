-module(structured_comment).

test(X) ->
    %$eqwalizer: dynamic(X)
    X.

test2(X) ->
    %$eqwalizer: X :: dynamic()
    X.
