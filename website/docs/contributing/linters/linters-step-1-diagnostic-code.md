---
sidebar_position: 1
title: 'Add a Diagnostic Code'
---

We will create a new linter, which warns about calls to a hypothetical
`unsafe:operation/1` API that should be avoided in production code.

Start by adding a new variant to the `DiagnosticCode` enum in
[`crates/ide_db/src/diagnostic_code.rs`](https://github.com/WhatsApp/erlang-language-platform/blob/main/crates/ide_db/src/diagnostic_code.rs):

```rust {3} showLineNumbers
pub enum DiagnosticCode {
    // ... existing codes ...
    NoUnsafeOperation, // This is the new code

    // Wrapper for erlang service diagnostic codes
    ErlangService(String),
    // Wrapper for EqWAlizer diagnostic codes
    Eqwalizer(String),
    // Used for ad-hoc diagnostics via lints/codemods
    AdHoc(String),
}
```

Ensure the code name is unique and descriptive. We will call it the _"No Unsafe
Operation"_ linter. Then add a clause for each of the required methods:

```rust {10,24,37} showLineNumbers
impl DiagnosticCode {
    // The `as_code` method returns a string representation of the code.
    // This is used to identify the diagnostic in the UI and in the CLI.
    // Each code emitted by ELP starts with a `W` prefix, followed by a 4-digit number.
    // The number is assigned by the linter author and should be unique within the linter.
    // Use the next available number in the sequence for your new linter.
    pub fn as_code(&self) -> &'static str {
        match self {
            // ... existing cases ...
            DiagnosticCode::NoUnsafeOperation => "W0055", // Use next available number here

            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::Eqwalizer(c) => format!("eqwalizer: {c}"),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}"),
        }
    }

    // The `as_label` method returns a human-readable label for the diagnostic.
    // This is used to identify the diagnostic in the UI and in the CLI.
    // The label should be unique and descriptive.
    pub fn as_label(&self) -> &'static str {
        match self {
            // ... existing cases ...
            DiagnosticCode::NoUnsafeOperation => "no_unsafe_operation", // Use snake_case here

            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::Eqwalizer(c) => c.to_string(),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}"),
        }
    }

    // The `allows_fixme_comment` method determines if it should be possible to temporarily
    // suppress the diagnostic by a `% elp:fixme` annotation.
    pub fn allows_fixme_comment(&self) -> bool {
        match self {
            // ... existing cases ...
            DiagnosticCode::NoUnsafeOperationLinter => false,

            DiagnosticCode::ErlangService(_) => false,
            DiagnosticCode::Eqwalizer(_) => false,
            DiagnosticCode::AdHoc(_) => false,
        }
    }

}
```
