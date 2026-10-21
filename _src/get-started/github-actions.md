---
sidebar_position: 7
---

# GitHub Actions

The [`erlef/setup-elp`](https://github.com/erlef/setup-elp) action installs ELP
in a GitHub Actions workflow, so you can run [ELP CLI](cli.md) commands such as
`elp lint` and `elp eqwalize-all` on every push and pull request.

## Usage

`setup-elp` downloads the ELP build that matches the Erlang/OTP version
installed on the runner, so set up Erlang/OTP first, for example with
[`erlef/setup-beam`](https://github.com/erlef/setup-beam):

```yaml
name: ELP

on:
  - push
  - pull_request

jobs:
  elp:
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4

      - uses: erlef/setup-beam@v1
        with:
          otp-version: "28.5"
          rebar3-version: "3"

      - uses: erlef/setup-elp@main
        with:
          elp-version: "2026-10-14"

      - name: Compile
        run: rebar3 as test compile

      - name: ELP lint
        run: elp lint --no-connect

      - name: eqWAlizer
        run: elp eqwalize-all --no-connect --bail-on-error
```

`--no-connect` runs each command standalone instead of through the ELP daemon,
which brings no benefit to a one-off run on a fresh runner.

## Inputs

| Input         | Description                                                                                                                           | Default  |
| ------------- | ------------------------------------------------------------------------------------------------------------------------------------- | -------- |
| `elp-version` | The ELP version to install: a tag from the [releases](https://github.com/WhatsApp/erlang-language-platform/releases) page (e.g. `2026-10-14`) or `latest`. | `latest` |

The action supports Linux (`ubuntu-*`) and macOS (`macos-*`) runners, on both
x64 and arm64.

## Choosing Versions

Each ELP release ships builds for a small set of Erlang/OTP versions, listed in
the _Assets_ section of the release. For example, `2026-10-14` provides builds
for Erlang/OTP 27.3, 28.5 and 29.1. `setup-elp` picks the build for the exact
`<major>.<minor>` version of Erlang/OTP on the runner, and the step fails if the
release has no such build. For this reason:

- Pin `otp-version` to a `<major>.<minor>` version that the ELP release was built
  for (`"28.5"` resolves to the latest 28.5.x patch release). A major version
  alone (`"28"`) resolves to the newest 28.x minor, which may not have an ELP
  build yet.
- Prefer a release tag over `latest` for `elp-version`. `latest` resolves at run
  time, so a new ELP release can introduce new diagnostics or change the set of
  Erlang/OTP builds without any change in your repository. Bump the tag
  deliberately instead.
- To make the workflow fully reproducible, reference the action by commit SHA
  rather than by branch: `erlef/setup-elp@<commit-sha> # main`.

## Running ELP in a Matrix

If your CI already tests against several Erlang/OTP versions, add the ELP steps
to the same job and use the versions the ELP release was built for in the
matrix:

```yaml
jobs:
  ci:
    runs-on: ubuntu-24.04
    name: Erlang ${{matrix.otp}}
    env:
      ELP_VERSION: "2026-10-14"
    strategy:
      matrix:
        otp: ["27.3", "28.5", "29.1"]
    steps:
      - uses: actions/checkout@v4

      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{matrix.otp}}
          rebar3-version: "3"

      - name: Compile
        run: rebar3 do clean, compile

      - uses: erlef/setup-elp@main
        with:
          elp-version: ${{env.ELP_VERSION}}

      - name: ELP lint
        run: elp lint --no-connect

      - name: eqWAlizer
        run: elp eqwalize-all --no-connect --bail-on-error

      - name: Test
        run: rebar3 do eunit, ct
```

## Failing the Build

- `elp lint` exits with a non-zero status when it reports at least one
  diagnostic with `error` severity. Diagnostics with a lower severity are
  reported but do not fail the step. Use the
  [`.elp_lint.toml`](configure-project/elp-lint-toml.md) configuration file to
  enable or disable linters and to change their severity.
- `elp eqwalize-all` exits with a zero status even when it reports type errors,
  unless you pass `--bail-on-error`.

When adopting ELP in a project with existing findings, you can make the ELP
steps non-blocking until the findings are fixed:

```yaml
      - name: ELP lint
        run: elp lint --no-connect
        continue-on-error: true
```
