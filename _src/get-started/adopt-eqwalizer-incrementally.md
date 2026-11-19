---
sidebar_position: 7.5
---

# Adopting eqWAlizer Incrementally

The first run of [eqWAlizer](/docs/eqwalizer/reference) on an existing code base
usually reports many type errors. Instead of fixing all of them before turning
on the check, you can make eqWAlizer _opt-in_: CI type checks only the modules
that ask for it, and you add modules one at a time as they become type-correct.
Every opted-in module then stays type-correct, because a new type error fails
the build.

Adding types to a code base step by step, while typed and untyped code keep
working together, is the whole point of _gradual typing_, the discipline
eqWAlizer is built on. See [Gradual typing](/docs/eqwalizer/reference/gradual)
for how eqWAlizer implements it with the `dynamic()` type, and
[Gradual Typing for Functional Languages](https://schemeworkshop.org/2006/13-siek.pdf)
by Jeremy Siek and Walid Taha for the paper that introduced the idea.

This tutorial sets up incremental adoption for a project on GitHub Actions. See
[GitHub Actions](github-actions.md) for how to install ELP in a workflow and
how to choose the ELP and Erlang/OTP versions.

## 1. Make eqWAlizer Opt-In

Add an `[eqwalizer]` section to the [`.elp.toml`](configure-project/elp-toml.md)
file at the root of the project, creating the file if needed:

```toml
[eqwalizer]
enable_all = false
```

From now on, eqWAlizer type checks only the modules that contain the
`-typing([eqwalizer]).` attribute, both in your editor and in
`elp eqwalize-all`. For a rebar3 project, a `.elp.toml` file without a
`build_info` section does not change how ELP discovers the project.

## 2. Opt In the Modules that Already Type Check

`elp eqwalizer-onboard` type checks the modules that are not opted in yet. It
lists the ones that already type check, then the ones with errors, fewest
errors first:

```bash
$ elp eqwalizer-onboard
Modules that type check (2):
  src/my_app_config.erl
  src/my_app_sup.erl
Modules with errors (2):
  src/my_app_server.erl: 4 errors
  src/my_app_parser.erl: 37 errors
```

Run it with `--opt-in` to add the `-typing([eqwalizer]).` attribute after the
`-module` attribute of each module that type checks:

```bash
$ elp eqwalizer-onboard --opt-in
```

```erlang
-module(my_app_sup).
-typing([eqwalizer]).
```

In a large project, pass `--app <APP>`, or `--target <TARGET>` in a Buck
project, to onboard one application or target at a time.

The Erlang compiler accepts and ignores attributes it does not know, so the
attribute has no effect on the compiled code. `elp eqwalizer-onboard` is
available in ELP releases newer than `2026-11-19`, so install one of those
locally to run it. CI only needs `elp eqwalize-all`, which earlier releases
provide too.

Check that the opted-in modules type check:

```bash
$ elp eqwalize-all --no-connect --bail-on-error
```

Opt in at least one module before you enable the CI step: when no module is
opted in, `elp eqwalize-all --bail-on-error` fails with
`No files to eqWAlize detected`.

## 3. Type Check on CI

Run `elp eqwalize-all` in your workflow:

```yaml
name: eqWAlizer

on:
  - push
  - pull_request

jobs:
  eqwalizer:
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4

      - uses: erlef/setup-beam@v1
        with:
          otp-version: "28.5"
          rebar3-version: "3"

      - uses: erlef/setup-elp@v1
        with:
          elp-version: "2026-11-19"

      - name: Compile
        run: rebar3 as test compile

      - name: eqWAlizer
        run: elp eqwalize-all --no-connect --bail-on-error --format github
```

- `--bail-on-error` makes the step fail when eqWAlizer reports an error.
  Without it, `elp eqwalize-all` always exits with a zero status.
- `--format github` reports each error as a
  [GitHub Actions annotation](github-actions.md#annotating-pull-requests), so
  it shows up on the offending line of the pull request:

  ```
  ::error file=src/my_app_sup.erl,line=42,col=5,endLine=42,endColumn=14,title=eqWAlizer%3A incompatible_types::`'undefined'`.%0A%0AExpression has type:   'undefined'%0AContext expected type: boolean()%0A%0AFor more information see: https://fb.me/eqwalizer_errors#incompatible_types
  ```

## 4. Opt In More Modules

The modules with errors that `elp eqwalizer-onboard` lists are the next
candidates, the ones with the fewest errors first. `elp eqwalize` type checks
the modules you name, whether they are opted in or not, and shows their errors:

```bash
$ elp eqwalize --no-connect my_app_server
```

Fix the reported errors, usually by adding or correcting specs and types, then
opt the module in within the same change, by hand or with
`elp eqwalizer-onboard --opt-in`.

When an error cannot be fixed yet, use one of eqWAlizer's
[escape hatches](/docs/eqwalizer/reference/escape) rather than leaving the
whole module out:

- `% eqwalizer:fixme <reason>` on the line above the error, for code that
  should be fixed later.
- `% eqwalizer:ignore <reason>`, for code that deliberately goes against its
  types and specs.
- the [`dynamic()`](/docs/eqwalizer/reference/gradual) type, to make a spec more
  lenient.

eqWAlizer reports a `fixme` or `ignore` comment that no longer suppresses any
error, so they do not outlive the problem they work around.

To take a module out of the check temporarily, for example when a change you
cannot fix right away breaks it, list it in `ignore_modules`. This takes
precedence over the `-typing([eqwalizer]).` attribute:

```toml
[eqwalizer]
enable_all = false
ignore_modules = ["my_app_server"]
```

## 5. Flip the Default

Once most modules are opted in, make eqWAlizer type check every module by
default, so that new modules are type checked from the start:

1. Remove `enable_all = false` from `.elp.toml`: `enable_all` defaults to
   `true`.
2. Opt out each module that does not type check yet, with the
   `-eqwalizer(ignore).` attribute or with `ignore_modules`. This includes
   generated modules: `elp eqwalizer-onboard` never lists them, but with
   `enable_all = true` eqWAlizer type checks them too. List them in
   `ignore_modules` rather than adding the attribute, which the next
   regeneration would remove.
3. Optionally, remove the `-typing([eqwalizer]).` attributes: with
   `enable_all = true`, they only matter for test modules when
   `include_tests = false` (see below).

With `enable_all = true`, set `include_tests = false` in the `[eqwalizer]`
section to leave test suites and test helpers out, if they are not ready yet.
Test modules that carry the `-typing([eqwalizer]).` attribute are still type
checked: keep the attribute on the test modules you want to check, and remove
it from the others.
