-module(head_mismatch).
-compile(nowarn_missing_spec).
% We'll check that "assist" code action will provide change from foo to bar.
bar(0) -> 1;
bar(1) -> 2;
foo(X) -> X.
