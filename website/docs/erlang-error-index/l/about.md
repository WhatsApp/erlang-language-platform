---
sidebar_position: -1
---

# About

The `L` namespace is reserved for error codes emitted by the Erlang code linter [erl_lint](https://www.erlang.org/doc/man/erl_lint).

The `erl_lint` module is used to check Erlang code for illegal syntax and other bugs. It also warns against coding practices that are not recommended.

The errors detected include:

* Redefined and undefined functions
* Unbound and unsafe variables
* Illegal record use

The warnings detected include:

* Unused functions and imports
* Unused variables
* Variables imported into matches
* Variables exported from `if/case/receive`
* Variables shadowed in funs and list comprehensions

Some of the warnings are optional, and can be turned on by specifying the appropriate option, described below.

For more information about `erl_lint`, please refer to [the official documentation](https://www.erlang.org/doc/man/erl_lint).
