---
sidebar_position: 2
title: 'Create the linter module'
---

Create a new file `elp/crates/ide/src/diagnostics/no_unsafe_operation.rs`, with
the following content. The code has comments to explain each part.

```rust showLineNumbers
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::DiagnosticCode;

use crate::diagnostics::Linter;
use crate::diagnostics::SsrPatternsLinter;

// We define a new linter, named `NoUnsafeOperationLinter`
pub(crate) struct NoUnsafeOperationLinter;

// We implement the `Linter` trait. This is common to all kinds of ELP linters.
impl Linter for NoUnsafeOperationLinter {
    // A unique identifier for the linter. This is the `DiagnosticCode` we just added.
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::NoUnsafeOperationLinter
    }
    // A human-readable description for the linter, which will be displayed in the IDE
    fn description(&self) -> String {
        "Do not use unsafe:operation/1 in production code.".to_string()
    }
    // By default test files (i.e. test suites and helpers) are processed by linters.
    // Since we only care about "unsafe calls" from production code,
    // here we override the default behaviour by skipping test files.
    fn should_process_test_files(&self) -> bool {
        false
    }
}

// We will write our linter using the so-called "SSR" syntax.
// SSR is a convenient way to match on parts of the Erlang AST.
// Because of this, we decide to implement the additional `SsrPatternsLinter` trait.
impl SsrPatternsLinter for NoUnsafeOperationLinter {
    // Linters can specify a custom context type. For now, we ignore it.
    type Context = ();

    // Here we specify which patterns we want to match.
    // The function returns a vector of tuples, each one containing
    // a SSR pattern and a context.
    // For the time being, let's ignore the context.
    // For SSR syntax, please refer to the `ide_ssr` crate.
    // Here, we are matching on calls to the fully-qualified `unsafe:operation/1`
    // function, ignoring its only argument.
    fn patterns(&self) -> Vec<(String, Self::Context)> {
        // We use the SSR mechanism to match calls to `unsafe:operation/1`.
        vec![("ssr: unsafe:operation(_@).".to_string(), ())]
    }
}
```

The only thing left to do is to register the linter in the
[`elp/crates/ide/src/diagnostics.rs`](https://github.com/WhatsApp/erlang-language-platform/blob/main/crates/ide/src/diagnostics.rs)
file:

```rust {8} showLineNumbers
// Add the module declaration
mod no_unsafe_operation;

// Register the linter in the `ssr_linters` function
pub(crate) fn ssr_linters() -> Vec<&'static dyn FunctionCallLinter> {
    vec![
        // ... existing linters ...
        &no_unsafe_operation::LINTER, // We add the linter here
    ]
}
```

In this case we have been using the `SsrPatternsLinter` trait, which is a
convenient way to match on parts of the Erlang AST. You can learn more about the
`SsrPatternsLinter` and other available linter traits in the
[linter traits](/docs/contributing/linters/linter-traits) section.
