---
sidebar_position: 4
title: 'Document your linter'
---

Each new linter must be accompanied by a documentation file in Markdown format.
Create the documentation file corresponding to your linter in:
`elp/website/docs/erlang-error-index/w/W0055.md` (where `W0055` is the code we
introduced):

````markdown
---
sidebar_position: 55
---

# W0055 - Unsafe operation

This diagnostic warns about calls to `unsafe:operation/1`, which should be
avoided in production code due to potential security or stability risks.
`safe:operation/1` is a safer alternative that should be used instead.

## Example

```erlang showLineNumbers
-module(example).
-export([do/1]).

do(Data) ->
    unsafe:operation(Data).
%%  ^^^^^^^^^^^^^^^^^^^^^^ warning: Do not use unsafe:operation/1 in production code.
```

## Recommended fix

Replace calls to `unsafe:operation/1` with a safer alternative, such as
`safe:operation/1`.

```erlang
-module(example).
-export([do/1]).

do(Data) ->
    safe:operation(Data).
```
````

The documentation will be presented to the user in the IDE and will also be
included in the
[Erlang Error Index](https://whatsapp.github.io/erlang-language-platform/docs/erlang-error-index/).
