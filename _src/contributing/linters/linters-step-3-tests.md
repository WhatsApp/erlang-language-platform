---
sidebar_position: 3
title: 'Test your linter'
---

### Add a snapshot test

At the bottom of the `no_unsafe_operation.rs` file, add the following:

```rust showLineNumbers
#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;

    #[test]
    fn test_no_unsafe_operation() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([warn/0]).
            warn() ->
                unsafe:operation(data),
            %%  ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Do not use unsafe:operation/1 in production code.
                safe:operation(data).
            //- /src/unsafe.erl
            -module(unsafe).
            -export([operation/1]).
            operation(_) -> ok.
            //- /src/safe.erl
            -module(safe).
            -export([operation/1]).
            operation(_) -> ok.
            "#,
        );
    }
}
```

ELP uses convenient snapshot tests for checking diagnostics. The test above
checks that the diagnostic is correctly reported when calling
`unsafe:operation/1`. It also checks that the diagnostic is not reported when
calling `safe:operation/1`.

Always include comprehensive tests in your diagnostic module:

1. **Positive cases** - Code that should trigger the diagnostic
2. **Negative cases** - Similar code that should NOT trigger the diagnostic
3. **Edge cases** - Boundary conditions and unusual syntax

You can run the tests with the following command:

```
cargo test --package elp_ide --lib -- diagnostics::no_unsafe_operation::tests::test_no_unsafe_operation --exact --show-output
```

### Try the new linter from the CLI

You can also try the new linter against a hypothetical `my_module` Erlang
module:

```
cargo run --bin elp -- lint --project /path/to/your/project --module my_module --diagnostic-filter no_unsafe_operation
```

```
Diagnostics reported in 1 modules:
  my_module: 1
      29:4-29:25::[Warning] [W0055] Do not use unsafe:operation/1 in production code.
```
