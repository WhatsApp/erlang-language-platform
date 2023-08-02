# Contributing to Erlang Language Platform

We want to make contributing to this project as easy and transparent as possible.

## Our Development Process

ELP is currently developed in Meta's internal repositories and then exported
out to GitHub by a Meta team member; however, we invite you to submit pull
requests as described below.

## Developing Locally

The gold standard is the current config in [ci.yml](.github/workflows/ci.yml).

A summary of these is

Check out [eqwalizer](https://github.com/WhatsApp/eqwalizer/) next to erlang-language-platform (this repo).

Install [sbt](https://www.scala-sbt.org/), and java >= 11

```sh
cd $PATH_TO_REPO/eqwalizer/eqwalizer
sbt assembly
```

It gives a path to the file

```sh
export ELP_EQWALIZER_PATH=<the path>
```

e.g. on a particular machine this would be

```sh
export ELP_EQWALIZER_PATH=/home/alanz/mysrc/github/WhatsApp/eqwalizer/eqwalizer/target/scala-2.13/eqwalizer.jar
```

Then use `cargo build` as usual from this repository.

Note: if you set `ELP_EQWALIZER_PATH` in your shell profile, it will be used by rust_analyzer in your IDE too.

An alternative way to build is

```sh
ELP_EQWALIZER_PATH=../../../eqwalizer/eqwalizer/target/scala-2.13/eqwalizer.jar cargo build
```

## Pull Requests

We actively welcome your pull requests.

1. Fork the repo and create your branch from `main`.
2. If you've added code that should be tested, add tests.
3. Ensure the test suite passes.
4. If you haven't already, complete the Contributor License Agreement ("CLA").

## Contributor License Agreement ("CLA")

In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>

## Issues

We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

## License

By contributing to Erlang Language Platform, you agree that your contributions will be
licensed under the [APACHE2](LICENSE-APACHE) and [MIT](LICENSE-MIT) licenses in the root
directory of this source tree.
