---
sidebar_position: 3
---

# Find References

ELP provides cross-referencing capabilities that power several IDE features:

- **Find References** — show all usages of a symbol
- **Go To Definition** — navigate to where a symbol is defined
- **Rename** — rename a symbol across the project
- **Call Hierarchy** — navigate callers and callees
- **Diagnostics** — linters such as [W0077](erlang-error-index/w/W0077.md) that detect potentially unused code

All of these features share the same underlying **usage search** algorithm,
described below.

## Supported Symbols

References can be found for:

- Modules
- Functions (local and remote)
- Records and record fields
- Types
- Behaviours
- Macros
- Headers
- Variables

## How Usage Search Works

The search is implemented by the `FindUsages` infrastructure.
It uses a **text-based scan with semantic verification**, trading a small amount
of redundant work for simplicity and speed.

### Step 1 — Determine Search Scope

The scope depends on the symbol kind:

| Symbol kind | Search scope |
|---|---|
| Local variable | Enclosing function clause |
| Unexported (local) function, record, type, macro | Defining module + all included headers |
| Symbol defined in a header | All modules that (transitively) include the header |
| Exported function, module, type | Entire project |

For exported symbols, the scope covers every source file in the project because
cross-application calls are allowed.

### Step 2 — Fast Text Scan

The function or symbol name is searched for as a **literal byte pattern** (using
`memchr`) in every file within the search scope. This is a simple substring
match — no parsing is involved at this stage — which makes it very fast even for
large codebases.

### Step 3 — Semantic Classification

Each textual hit is located in the syntax tree and classified using
`SymbolClass::classify`. The classifier resolves the token in context and
returns one of:

| Classification | Meaning | Counted as usage? |
|---|---|---|
| `Definition` | This is where the symbol is defined | No |
| `Reference(Direct)` | An unambiguous reference (call, fun capture, import, etc.) | Yes |
| `Reference(Other)` | A structural reference (export entry, spec, etc.) | Yes |
| `Reference(Fuzzy)` | An ambiguous or indirect reference | No |

Only references that resolve to the **same definition** as the target symbol are
counted.

### Step 4 — Early Termination

Many callers only need to know whether _at least one_ (or _at least N_)
references exist. The search supports early termination via `at_least_one()` and
`at_least_n(n)` to avoid scanning the entire project when the answer is already
known.

## What Counts as a Reference

| Pattern | Example | Type |
|---|---|---|
| Qualified call | `mod:foo()` | Direct |
| Unqualified local call | `foo()` | Direct |
| Fun capture | `fun foo/1`, `fun mod:foo/1` | Direct |
| `-import` | `-import(mod, [foo/0]).` | Other |
| `-export` entry | `-export([foo/0]).` | Other |
| `-spec` | `-spec foo() -> ok.` | Other |
| Dynamic call with literals | `apply(mod, foo, [1,2])` | Direct |
| Dynamic call with variables | `apply(Mod, Fun, Args)` | Not tracked |

### Recognised Dynamic Call Patterns

ELP maintains a table of well-known dynamic dispatch functions. When the
module, function, and arity arguments are **literal atoms or integers**, the call
is resolved statically and treated as a direct reference.

For example, the following patterns are recognised:

- **`erlang`** (implicit or explicit): `apply/2,3`, `spawn/3,4`,
  `spawn_link/3,4`, `spawn_monitor/3,4`, `spawn_opt/4,5`,
  `spawn_request/5`, `hibernate/3`, `function_exported/3`, `is_builtin/3`
- **`rpc`**: `call/4,5`, `async_call/4`, `cast/4`, `multicall/3,4,5`
- **`erpc`**: `call/4,5`, `cast/4`, `multicall/4,5`, `multicast/4`,
  `send_request/6`
- **`peer`**: `call/4,5`, `cast/4`

## Limitations

These limitations apply to every feature that relies on the usage search
(find references, rename, unused-code diagnostics, etc.):

1. **Dynamic dispatch with variables** — Calls like `apply(Mod, Fun, Args)` or
   `Module:Function()` where any argument is a variable cannot be resolved
   statically. The callee will not appear as used.

2. **Higher-order indirection** — A function reference stored in a data
   structure or threaded through multiple layers of higher-order calls may not
   be traced back to its origin.
