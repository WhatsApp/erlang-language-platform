-module(app_a_errors_generated).
% this fixture exercises our handling of @generated files

-compile([export_all, nowarn_export_all]).
-typing([eqwalizer]).

-spec foo() -> foo.
foo() -> wrong_ret.
