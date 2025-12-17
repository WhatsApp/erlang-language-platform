% In-module definitions for test purpose.
-module(local_def).
-compile(nowarn_missing_spec).

f() -> ok.

g() -> f().
