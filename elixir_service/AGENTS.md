# Elixir service: rules for agents

## The service stays general

The Elixir service, like ELP, its Rust translator and the Scala checker
eqWAlizer, is a general tool for every Elixir project. Never add a rule,
AST rewrite or code transformation whose justification is that some
particular project then type-checks. A change is general when it is
correct for every project and follows from the semantics of Elixir or of
the checker, not from one codebase's idioms.

When a diagnostic on real code looks wrong or noisy:

- a checker imprecision is fixed in the checker, as a rule that holds for
  all input, never papered over where the input is produced;
- an Elixir standard-library spec or type that misleads the checker is
  overlaid in the support library, as below;
- a project's own code is the project's to fix.

## Standard-library overlays carry general Elixir knowledge

Elixir's standard library declares many types and specs in a way eqWAlizer
cannot use well. Overriding them is general, since it pertains to Elixir
as a whole, and it lives in the support library and nowhere else:
`priv/eqwalizer_support/src/eqwalizer_types.erl` for types,
`eqwalizer_specs.erl` for specs.

## Rewrites of Elixir code or forms need explicit approval

The service serves the Erlang forms the Elixir compiler recorded. Any
rewrite of Elixir code, or of those forms, before they reach the Rust
translator or the checker must be put to the maintainer first: show the
form before and after, and argue from Elixir's semantics why the
rewritten form is what the code means. A rewrite that exists because the
checker mishandles the original form is not acceptable; fix the checker.

Two rewrites are approved so far:

- the bodies of compiler-generated functions are left out, keeping their
  specs and exports (`reject_generated_functions` in
  `lib/elixir_service/lint.ex`);
- standard-library types Elixir declares as `any()` are overlaid as
  `dynamic()`. Even there, prefer overriding the specs of the functions
  involved with type variables, and keep `dynamic()` only for what no
  spec can express.
