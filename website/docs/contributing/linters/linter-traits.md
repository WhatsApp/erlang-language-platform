---
sidebar_position: 6
title: 'Linter Traits'
---

Linters are implemented using Rust
[traits](https://doc.rust-lang.org/book/ch10-02-traits.html) (think interfaces,
or Erlang behaviours). This allows linter authors to minimize the amount of
boilerplate code they need to write, and makes it easier to write and maintain
linters.

## The `Linter` trait

Each linter must implement the `Linter` trait, which has the following mandatory
methods:

```rust
// A unique identifier for the linter.
fn id(&self) -> DiagnosticCode;

// A plain-text description for the linter. Displayed to the end user.
fn description(&self) -> &'static str;
```

The `Linter` trait also has a few optional methods that can be used to customize
the linter's behavior, if needed:

```rust

// The severity for the lint issue. It defaults to `Warning`.
fn severity(&self) -> Severity {
    Severity::Warning
}

// For CLI, when using the --use-cli-severity flag. It defaults to `severity()`
fn cli_severity(&self) -> Severity {
    self.severity()
}

// Specify if the linter issues can be suppressed via a `% elp:ignore` comment.
fn can_be_suppressed(&self) -> bool {
    true
}

// Specify if the linter should only run when the `--experimental` flag is specified.
fn is_experimental(&self) -> bool {
    false
}

// Specify if the linter is enabled by default.
fn is_enabled(&self) -> bool {
    true
}

// Specify if the linter should process generated files.
fn should_process_generated_files(&self) -> bool {
    false
}

// Specify if the linter should process generated test files (including test helpers)
fn should_process_test_files(&self) -> bool {
    true
}

// Specify if the linter should process the given file id.
fn should_process_file_id(&self, _sema: &Semantic, _file_id: FileId) -> bool {
    true
}
```

## Additional linter traits

In addition to the `Linter` trait, ELP provides a few additional traits, each
suitable for a specific type of linter. These traits are:

- `FunctionCallLinter`
- `SsrPatternsLinter`
- `GenericLinter`

Let's look at each of these traits in more detail.

### FunctionCallLinter

The `FunctionCallLinter` trait provides an efficient way to match against
function calls by name, module, and arity. This is the most common type of
linter in ELP.

#### Trait Methods

The trait provides the following methods:

```rust showLineNumbers
/// Associated type - each linter defines its own context for passing data between callbacks
type Context: Clone + fmt::Debug + PartialEq + Default;

/// Specify which functions to match against
fn matches_functions(&self) -> Vec<FunctionMatch>;

/// Specify functions to exclude from matching (optional)
fn excludes_functions(&self) -> Vec<FunctionMatch>;

/// Custom validation logic for each matched call (optional)
fn check_match(&self, _check_call_context: &CheckCallCtx<'_, ()>) -> Option<Self::Context>;

/// Provide quick-fixes for matched calls (optional)
fn fixes(&self, _match_context: &MatchCtx<Self::Context>, _sema: &Semantic, _file_id: FileId) -> Option<Vec<Assist>>;

/// Customize the diagnostic message per match (optional)
fn match_description(&self, _context: &Self::Context) -> Cow<'_, str>;
```

#### Example Usage

Here's how you might implement a linter that flags calls to
`erlang:garbage_collect/0`:

```rust showLineNumbers
use crate::lazy_function_matches;

pub(crate) struct NoGarbageCollectLinter;

impl Linter for NoGarbageCollectLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::NoGarbageCollect
    }

    fn description(&self) -> &'static str {
        "Avoid forcing garbage collection."
    }
}

impl FunctionCallLinter for NoGarbageCollectLinter {
    type Context = ();

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![
            FunctionMatch::mfa("erlang", "garbage_collect", 0),
        ]
    }
}
```

### SsrPatternsLinter

The `SsrPatternsLinter` trait uses Structural Search and Replace (SSR) patterns
to match complex code structures, that go beyond simple function calls. SSR
provides a powerful way to express patterns in Erlang syntax, but it can be
slower than other approaches.

Ssr patterns can also be used to match function calls, but they require a
pattern for each function arity. So, if you need to match on multiple function
arities, you should consider the `FunctionCallLinter` trait instead.

The `SsrPatternsLinter` trait is ideal for:

- Complex pattern matching beyond simple function calls
- Matching code structures like specific usage patterns
- Finding and replacing complex expressions
- Matching code patterns with placeholders and variables

#### Trait Methods

```rust showLineNumbers
/// Associated type for the pattern context - each linter defines its own
type Context: Clone + fmt::Debug + PartialEq;

/// Specify the SSR patterns to match. Each pattern is paired with a context
/// for distinguishing between different patterns.
fn patterns(&self) -> Vec<(String, Self::Context)>;

/// Customize the diagnostic message for each pattern (optional)
fn pattern_description(&self, _context: &Self::Context) -> &'static str;

/// Validate that a match is legitimate (optional)
fn is_match_valid(&self, _context: &Self::Context, _matched: &Match, _sema: &Semantic, _file_id: FileId) -> Option<bool>;

/// Provide quick-fixes for matched patterns (optional)
fn fixes(&self, _context: &Self::Context, _matched: &Match, _sema: &Semantic, _file_id: FileId) -> Option<Vec<Assist>>;

/// Specify additional diagnostic categories (optional)
fn add_categories(&self, _context: &Self::Context) -> Vec<Category>;

/// Configure how macros and parentheses are handled (optional)
fn strategy(&self) -> Strategy;

/// Define the search scope - entire file or functions only (optional)
fn scope(&self, file_id: FileId) -> SsrSearchScope;

/// Customize the diagnostic range (optional)
fn range(&self, _sema: &Semantic, _matched: &Match) -> Option<TextRange>;
```

#### SSR Pattern Syntax

SSR patterns use a special syntax where:

- `$var` represents a placeholder that matches any expression
- `_@` represents a wildcard that matches anything
- You can use regular Erlang syntax for the rest of the pattern

#### Example Usage

Here's how you might implement a linter that detects inefficient map-to-list
conversions in comprehensions:

```rust showLineNumbers
#[derive(Debug, Clone, PartialEq)]
enum MapToListPattern {
    MapToList,
}

pub(crate) struct UnnecessaryMapToListLinter;

impl Linter for UnnecessaryMapToListLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnnecessaryMapToListInComprehension
    }

    fn description(&self) -> &'static str {
        "Avoid maps:to_list/1 in list comprehensions"
    }
}

impl SsrPatternsLinter for UnnecessaryMapToListLinter {
    type Context = MapToListPattern;

    fn patterns(&self) -> Vec<(String, Self::Context)> {
        vec![
            (
                "ssr: [V || {_@, V} <- maps:to_list($M)]".to_string(),
                MapToListPattern::MapToList,
            ),
        ]
    }

    fn pattern_description(&self, _context: &Self::Context) -> &'static str {
        "Use maps:values/1 instead of list comprehension with maps:to_list/1"
    }
}
```

### GenericLinter

The `GenericLinter` trait provides the most flexibility, allowing you to
implement completely custom matching logic. You should use this trait only when
the other two options don't fit your use case.

The `GenericLinter` trait is ideal for:

- Custom traversal and analysis of the AST
- Complex cross-function or cross-module analysis
- Linters that need fine-grained control over the matching process
- Situations where neither function call matching nor SSR patterns are
  sufficient

#### Trait Methods

```rust showLineNumbers
/// Associated type for passing context between the matching and diagnostic phases
type Context: Clone + fmt::Debug + PartialEq + Default;

/// Return a list of matches found in the file
/// This is where you implement your custom matching logic
fn matches(&self, _sema: &Semantic, _file_id: FileId) -> Option<Vec<GenericLinterMatchContext<Self::Context>>>;

/// Customize the diagnostic message for each match (optional)
fn match_description(&self, _context: &Self::Context) -> Cow<'_, str>;

/// Add diagnostic tags like Unused or Deprecated (optional)
fn tag(&self, _context: &Self::Context) -> Option<DiagnosticTag>;

/// Provide quick-fixes for each match (optional)
fn fixes(&self, _context: &Self::Context, _sema: &Semantic, _file_id: FileId) -> Option<Vec<Assist>>;
```

#### GenericLinterMatchContext

When returning matches from the `matches()` method, you need to wrap your
context in a `GenericLinterMatchContext`:

```rust showLineNumbers
GenericLinterMatchContext {
    range: TextRange,    // The text range where the diagnostic should appear
    context: Context,    // Your custom context data
}
```

#### Example Usage

Here's how you might implement a linter that detects unused macros:

```rust showLineNumbers
#[derive(Debug, Clone, PartialEq, Default)]
struct UnusedMacroContext {
    macro_name: String,
}

pub(crate) struct UnusedMacroLinter;

impl Linter for UnusedMacroLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedMacro
    }

    fn description(&self) -> &'static str {
        "This macro is never used"
    }
}

impl GenericLinter for UnusedMacroLinter {
    type Context = UnusedMacroContext;

    fn matches(&self, sema: &Semantic, file_id: FileId) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let mut matches = Vec::new();

        // Custom logic to find unused macros
        let source_file = sema.parse(file_id);
        let form_list = sema.form_list(file_id);

        // Collect all macro definitions
        let defined_macros = collect_defined_macros(&form_list, &source_file.value);

        // Collect all macro usages
        let used_macros = collect_used_macros(&source_file.value);

        // Find unused macros
        for (name, range) in defined_macros {
            if !used_macros.contains(&name) {
                matches.push(GenericLinterMatchContext {
                    range,
                    context: UnusedMacroContext { macro_name: name },
                });
            }
        }

        if matches.is_empty() {
            None
        } else {
            Some(matches)
        }
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!("Macro '{}' is never used", context.macro_name))
    }

    fn tag(&self, _context: &Self::Context) -> Option<DiagnosticTag> {
        Some(DiagnosticTag::Unused)
    }
}

// Helper functions would be implemented here
fn collect_defined_macros(form_list: &FormList, source_file: &SourceFile) -> Vec<(String, TextRange)> {
    // Implementation details...
    vec![]
}

fn collect_used_macros(source_file: &SourceFile) -> HashSet<String> {
    // Implementation details...
    HashSet::new()
}
```

## Choosing the Right Trait

When deciding which trait to use for your linter:

1. **Use `FunctionCallLinter`** if you need to match specific function calls by
   module, name, and arity
2. **Use `SsrPatternsLinter`** if you need to match complex code patterns that
   go beyond simple function calls
3. **Use `GenericLinter`** if you need complete control over the matching logic
   or need to perform complex analysis

Most linters in ELP use the `FunctionCallLinter` trait since it covers the
common case efficiently.
