---
llms-gk: 'devmate_elp_development_md'
apply_to_regex: '^(.*\.rs|.*\.md)$'
oncalls: ['vscode_erlang']
---
# ELP Development Rules for LLMs (OSS)

## Diagnostic Code Management

### Adding New Diagnostic Codes

- `W0000-W9999` range for native ELP diagnostics
- Never change the number or label of an existing diagnostic code
- Always add new constructors to the end of the `DiagnosticCode` enum
- Create doc file: `website/docs/erlang-error-index/{namespace}/{code}.md` (e.g., `W0051` → `w/W0051.md`)

### Creating Linters

After implementing `Linter` + a specialized trait (`FunctionCallLinter`, `SsrPatternsLinter`, or `GenericLinter`), register the `pub static LINTER` in `crates/ide/src/diagnostics.rs` in `FUNCTION_CALL_LINTERS`, `SSR_PATTERN_LINTERS`, or `GENERIC_LINTERS`.

## Anti-Patterns

### Use `FxHashMap`, not `std::HashMap`

`FxHashMap`/`FxHashSet` are used project-wide. Do not introduce `std::collections::HashMap`.

### Use `known` module statics for atom comparison

```rust
// Correct (known entries are `LazyLock<Name>`, so deref with `*`)
if db.lookup_atom(*atom) == *known::erlang { ... }

// Wrong — do not use .as_str() string comparisons
if db.lookup_atom(*atom).as_str() == "erlang" { ... }
```

Add missing atoms to `known_names!` in `crates/hir/src/name.rs` (alphabetical within category).

### Use `assert_eq_expected!`, not `assert_eq!`

First argument must be a constant or named `expected` or start with `expected`:
```rust
use elp_base_db::assert_eq_expected;
assert_eq_expected!(expected_range, actual_range);
```

### Snapshot updates: use `UPDATE_EXPECT=1`, never manually edit

Do NOT manually edit `expect![[...]]` strings.

Updating expectations is expensive, so do it only once you expect a snapshot to
change, and scope it to the failing test, or at most its crate:

```bash
UPDATE_EXPECT=1 cargo test -p <crate_name> -- <test_name>   # preferred
UPDATE_EXPECT=1 cargo test -p <crate_name>                  # acceptable
```

Run the tests normally first and update only the snapshots the failures name. An
update run records whatever the test produced, so a test failing for an unrelated
reason has that failure written in as its new expectation. Diff the snapshot files
afterwards and revert anything you did not mean to change.

### Crate names ≠ directory names

Crate names use `elp_` prefix on directory names (e.g., `crates/ide` → `elp_ide`, `crates/syntax` → `elp_syntax`). Exceptions: `crates/elp` → `elp`, `crates/hir` → `hir`. Check `Cargo.toml` for exact crate name.

### Do not change existing tests without asking

### CLI argument deprecation

Make removed CLI arguments a no-op with a stderr warning. Remove after at least one month.

### Avoid code duplication

Before adding a function similar to an existing one, check if they can be merged.
