---
sidebar_position: 3.5
---

# Structural Search (SSR)

ELP can search Erlang code **structurally** — by the shape of the syntax tree
rather than by raw text. This is exposed on the command line as:

```
$ elp search 'PATTERN'
```

`elp search` is the primary command; `elp ssr` is an alias for it (SSR stands for
*Structural Search and Replace*). The two are identical — `search` is just the
friendlier name for the read-only "find me code shaped like this" use case.

Because matching happens on the syntax tree, structural search ignores
formatting, whitespace, and comments. Parentheses are ignored too by default,
though you can make them significant (see
[Macro and parenthesis handling](#macro-and-parenthesis-handling)). A single
pattern reliably finds code that a pile of regular expressions never could.

:::tip

If you only remember one thing: a pattern is just **Erlang code with
placeholders**. Write the code you are looking for, and replace the parts that
vary with `_@Name`.

:::

## Quick start

Find every double `lists:reverse/1` (a common no-op):

```
$ elp search 'lists:reverse(lists:reverse(_@List))'
```

Find a redundant `case` whose branches are identical:

```
$ elp search 'case _@Cond of true -> _@Body; false -> _@Body end'
```

Find all calls to `foo` regardless of arity, as JSON:

```
$ elp search --format json 'foo(_@@Args)'
```

By default the search runs over the whole project discovered from the current
directory. See [Scoping the search](#scoping-the-search) to narrow it down.

## Placeholders

Placeholders are how you say "match anything here". They are written using a
`_@` prefix so they can never collide with real Erlang variables.

| Placeholder | Matches | Notes |
|---|---|---|
| `_@Name` | Any single expression or pattern | Bound to `Name`; reuse the name to require equality (see below) |
| `_@_` | Anything | Anonymous wildcard — matches but binds nothing |
| `_@@Name` | Zero or more sibling elements | A **glob**; see [Globs](#globs) |
| `@Name` | An atom | Atom placeholder, used where only an atom is syntactically valid |

### Reusing a placeholder requires equality

When the same placeholder name appears more than once in a pattern, every
occurrence must match **structurally equal** code. This is what makes the
identical-branches example above work:

```erlang
%% _@Body appears twice, so both branches must be the same expression
case _@Cond of true -> _@Body; false -> _@Body end
```

If you do not care whether two sub-expressions are equal, give them different
names (`_@A`, `_@B`) or use the anonymous `_@_`.

### Globs

A glob placeholder, written `_@@Name`, matches **zero or more** sibling elements
in an ordered sequence — the elements of a tuple or list, the arguments of a
call, or the expressions in a clause body. This mirrors Erlang
[Merl](https://www.erlang.org/doc/man/merl.html)-style matching.

```erlang
%% Match a tuple tagged `a`, capturing the rest of the elements
{a, _@@Rest}        %% matches {a, b, c, d}; Rest = [b, c, d]

%% Match a call to foo with any number of arguments
foo(_@@Args)        %% matches foo(1, 2, 3); Args = [1, 2, 3]
```

Globs also work in maps, where a glob entry absorbs the unmatched fields:

```erlang
#{tag => error, _@@K => _@@V}   %% bind the remaining keys and values
#{_@@K => _}                    %% bind keys, don't-care values
#{_ => _@@V}                    %% don't-care keys, bind values
#{_@@_ => _}                    %% just absorb the extras
```

Rules and limitations for globs:

- **At most two globs per sequence, with at least one fixed element between
  them.** So `{a, _@@A, b, _@@B, c}` is allowed (the `b` anchors the two globs),
  but `{_@@A, _@@B}` is not.
- **Maps allow at most one glob entry** — an entry such as `_@@K => _@@V` counts
  as a single entry.
- **Not allowed in records** or in `when`/`where` guards.
- When the same glob name appears in two sequences, both bindings must be
  element-wise equivalent.

## Constraints with `where`

Append a `where` clause to constrain what a placeholder may match. There are two
kinds of condition, and they can be combined.

### Equality / inequality against a literal

```erlang
_@X where _@X == foo
_@X where _@X =/= undefined
```

Supported operators are `==`, `=:=`, `/=`, and `=/=`. The right-hand side must be
a literal (or another placeholder).

### Type / kind predicates

Constrain a placeholder to a particular kind of node using a local-call
predicate (note: **no** `erlang:` prefix):

```erlang
meck:expect(_@Mod, _@Fun, _@Impl) where is_function(_@Impl)
```

Available predicates:

| Predicate | Matches |
|---|---|
| `is_atom/1` | An atom |
| `is_integer/1` | An integer literal |
| `is_function/1` | A `fun` expression or reference |
| `is_list/1` | A list |
| `is_tuple/1` | A tuple |
| `is_map/1` | A map |
| `is_binary/1` | A binary |
| `is_var/1` | A variable (SSR extension) |
| `is_call/1` | A function call (SSR extension) |

Combine conditions with `,` for AND and `;` for OR, and negate with `not`:

```erlang
f(_@A) where is_atom(_@A), _@A =/= undefined
f(_@A) where is_integer(_@A); is_atom(_@A)
f(_@A) where not is_list(_@A)
```

:::note

The qualified form (`erlang:is_atom(_@X)`) is intentionally rejected — use the
bare predicate name. Globs are not allowed inside a `where` clause.

:::

## Pattern spellings

A positional argument to `elp search` is accepted in three shapes. They are
equivalent ways of writing the same thing:

| You type | Interpreted as | When to use |
|---|---|---|
| `PATTERN` | `ssr: PATTERN.` | The common case — just write the code |
| `ssr: PATTERN.` | itself | The canonical rule form |
| `LABEL:ssr: PATTERN.` | a labelled rule | Tag matches with `LABEL` |

The bare form is shorthand: ELP wraps it as `ssr: PATTERN.` for you (adding the
`ssr:` prefix and the trailing `.`).

The `LABEL:ssr:` form attaches a label to the rule. The label surfaces as
`patternLabel` in [JSON output](#json-output), which is handy when you pass
several patterns at once and want to know which one matched:

```
$ elp search 'double_reverse:ssr: lists:reverse(lists:reverse(_@L)).' \
             'noop_case:ssr: case _@C of true -> _@B; false -> _@B end.'
```

The delimiter is the literal substring `:ssr: ` (with the trailing space), which
cannot occur in Erlang code, so module-qualified calls such as `meck:expect(...)`
are never mistaken for a label.

## Searching, not replacing

The SSR rule grammar also accepts a replacement template, written
`ssr: SEARCH ==>> REPLACE.`. **Replacement is not currently implemented** — the
`==>>` template is parsed, but no tool acts on it yet. SSR is search-only today:
`elp search` / `elp ssr` *matches* code and never rewrites it.

So think of `elp search` as *grep for syntax trees*, not sed. (ELP's own linters
do detect and fix code, but they apply their own fixes rather than the SSR
`==>>` template — see [Patterns in linters](#patterns-in-linters).)

## Output

### Default output

Matches are printed grouped by module. With no matches you get a
`No matches found` message; otherwise ELP reports how many modules contained
matches.

### Showing source context

Like `grep`, you can print surrounding lines:

| Flag | Meaning |
|---|---|
| `--show-source` | Show the matched source |
| `-A NUM` / `--after-context` | Print `NUM` trailing lines |
| `-B NUM` / `--before-context` | Print `NUM` leading lines |
| `-C NUM` / `--context` | Print `NUM` lines on both sides |
| `--group-separator SEP` | Separator printed between match groups |
| `--no-group-separator` | Do not print a separator |

Any of the context flags implies `--show-source`.

### JSON output

```
$ elp search --format json 'foo(_@@Args)'
```

JSON output is line-delimited and machine-readable, and includes the
`patternLabel` for [labelled patterns](#pattern-spellings) — ideal for scripting
or for feeding results to another tool.

## Scoping the search

By default ELP loads the whole project from the current directory. You can narrow
the input:

| Flag | Effect |
|---|---|
| `--project PATH` | Path to a project directory or a project JSON file (default `.`) |
| `--module NAME` | Search a single module |
| `--app NAME` | Search a single application |
| `--file PATH` | Search a single file (include file, escript, …) |
| `--include-generated` | Also search generated files |
| `--rebar` / `--as PROFILE` | Load via rebar3 with the given profile |

## Macro and parenthesis handling

- `--macros STRATEGY` controls macro expansion before matching: `expand`
  (default), `no-expand`, or `visible-expand`. Use `no-expand` to match the code
  as written, with macros intact.
- `--parens` makes parentheses significant. By default they are ignored, so
  `(X + Y)` and `X + Y` match the same pattern.

## Generating a lint config

`--dump-config` turns your pattern into a snippet you can paste into
`.elp_lint.toml`, so a one-off search can become a persistent, project-wide
check:

```
$ elp search --dump-config 'lists:reverse(lists:reverse(_@List))'

# Add this to your .elp_lint.toml
[[ad_hoc_lints.lints]]
...
```

## Patterns in linters

Many built-in ELP diagnostics are implemented as SSR patterns under the hood
(the `SsrPatternsLinter` infrastructure). For example, the lint that suggests a
list comprehension instead of `lists:map/2` is driven by patterns like:

```erlang
ssr: lists:map(fun (_@Var) -> _@Body end, _@List).
ssr: lists:map(_@Fun, _@List).
```

If you find yourself running the same `elp search` repeatedly, it may be a good
candidate for a real linter — see the
[contributing guide](contributing/contributing.md) for how linters are written.

## Examples

```erlang
%% Any call to io:format, regardless of arguments
io:format(_@@Args)

%% Any gen_server:call, regardless of arguments
gen_server:call(_@@Args)

%% Comparing a term to the atom `true` (often redundant)
_@X == true

%% A meck:expect where the implementation is a fun
meck:expect(_@Mod, _@Fun, _@Impl) where is_function(_@Impl)

%% A tuple tagged `error` with any payload
{error, _@@Rest}
```

## See also

- [Find References](find-references.md) — the complementary "where is this symbol
  used" feature.
- `elp search --help` — the same cheat-sheet, always in sync with your installed
  version.
