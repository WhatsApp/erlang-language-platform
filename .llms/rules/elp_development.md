---
llms-gk: 'devmate_elp_development_md'
apply_to_regex: '^(.*\.rs|.*\.md)$'
---

# ELP Development Rules for LLMs

## Project Overview

ELP (Erlang Language Platform) is a language server and development tools suite for Erlang, built in Rust. This project provides IDE features, diagnostics, and code analysis for Erlang codebases.

## Diagnostic Code Management

### Adding New Diagnostic Codes

When adding new diagnostic codes to `DiagnosticCode` enum:

1. **Naming Convention**: Use descriptive PascalCase names that clearly indicate the issue
   - Good: `UnusedFunctionArg`, `MissingCompileWarnMissingSpec`
   - Bad: `Error1`, `BadCode`

2. **Code Assignment**: Follow the established numbering scheme
   - `W0000-W9999`: Native ELP diagnostics, visible in the OSS version
   - `WA000-WA999`: WhatsApp-specific warnings, only visible in Meta builds
   - Use the next available number in the appropriate range
   - Never change the number of an existing diagnostic code
   - Never change the label of an existing diagnostic code
   - Always add the new diagnostic constructor to the end of the list

3. **Required Methods**: When adding a new variant, update ALL match statements:
   - `as_code()`: Return the diagnostic code (e.g., "W0053")
   - `as_label()`: Return snake_case label (e.g., "unused_function_arg")
   - `allows_fixme_comment()`: Determine if FIXME comments are allowed
   - `is_syntax_error()`: Mark if this is a syntax error

4. **Documentation**: Add comments explaining complex diagnostic codes

5. **Documentation File**: Create a corresponding documentation file in the website
   - Location: `website/docs/erlang-error-index/{namespace}/{code}.md`
   - Example: `W0051` → `website/docs/erlang-error-index/w/W0051.md`
   - Include frontmatter with `sidebar_position` matching the code number
   - Structure should include:
     - Title with code and brief description
     - Severity level (Error, Warning, WeakWarning, Information)
     - Code example showing the diagnostic in action
     - Explanation section describing the issue and why it matters
     - Optional: Fix suggestions or alternatives
   - The `as_uri()` method automatically generates URLs pointing to these docs

### Creating DiagnosticDescriptor

Every diagnostic must have a corresponding `DiagnosticDescriptor` that defines when and how the diagnostic runs:

1. **Static Descriptor Declaration**: Create a public static descriptor in your diagnostic module
   - Use `pub(crate) static DESCRIPTOR: DiagnosticDescriptor` pattern
   - Define `DiagnosticConditions` with appropriate flags
   - Provide a checker function that implements the diagnostic logic

2. **Diagnostic Conditions**: Configure when the diagnostic should run
   - `experimental`: Mark as true for experimental/unstable diagnostics
   - `include_generated`: Set to false if diagnostic shouldn't run on generated code
   - `include_tests`: Set to false if diagnostic shouldn't run on test files
   - `default_disabled`: Set to true if diagnostic requires explicit enabling

3. **Checker Function**: Implement the diagnostic logic
   - Must match signature: `&dyn AdhocSemanticDiagnostics`
   - Push diagnostics to the `diags` vector using `Diagnostic::new()`
   - Use helper functions to keep the checker clean and focused

4. **Registration**: Add the descriptor to `diagnostics_descriptors()` function in `diagnostics.rs`
   - Include your module's `DESCRIPTOR` in the returned vector

5. **Module Structure**: Follow the established pattern
   - Create separate module files for each diagnostic type
   - Export the `DESCRIPTOR` as `pub(crate) static`
   - Include comprehensive tests with `#[cfg(test)]`
   - Use SSR patterns when appropriate for complex matching

### Meta-Only vs OSS Code

- Use `@fb-only` and `@oss-only` comments to mark platform-specific code
- Meta-only diagnostics should use `MetaOnlyDiagnosticCode` wrapper
- Ensure OSS builds work by providing fallbacks for Meta-only features

## Rust Code Style

### Error Handling

- Use `Result<T, E>` for fallible operations
- Prefer `?` operator over explicit match for error propagation
- Use descriptive error messages with context

### Pattern Matching

- Use exhaustive matches for enums to catch new variants at compile time
- Add explicit comments when intentionally using catch-all patterns
- Prefer early returns to reduce nesting

### String Handling

- Use `&str` for borrowed strings, `String` for owned
- Use `format!()` for complex string formatting
- Use `to_string()` for simple conversions

### Collections

- Use `FxHashMap` instead of `std::HashMap` for better performance
- Use `lazy_static!` for expensive static computations
- Prefer iterators over manual loops where possible

## Testing Guidelines

### Test Structure

- Use `expect_test` for snapshot testing of complex outputs
- Group related tests in the same module
- Use descriptive test names that explain the scenario

### Test Data

- Create minimal test cases that focus on specific functionality
- Use realistic Erlang code examples in tests
- Test both positive and negative cases

### Existing tests

- Do not change existing tests without asking

## Documentation

### Code Comments

- Document complex algorithms and business logic
- Explain WHY, not just WHAT the code does
- Use `///` for public API documentation
- Use `//` for internal implementation notes

### Error Messages

- Make error messages actionable and user-friendly
- Include context about what was expected vs. what was found
- Provide suggestions for fixing the issue when possible

## Performance Considerations

### Memory Usage

- Use `Box<T>` for large enum variants to keep enum size small
- Consider using `Cow<str>` for strings that might be borrowed or owned
- Use `Arc<T>` for shared immutable data

### Computation

- Cache expensive computations using `lazy_static!` or `once_cell`
- Use appropriate data structures (HashMap for lookups, Vec for sequences)
- Profile code paths that handle large Erlang codebases

## Integration Guidelines

### Erlang Service Integration

- Handle Erlang service errors gracefully
- Use appropriate namespaces for different error sources
- Maintain backward compatibility with existing error codes

### IDE Integration

- Provide rich diagnostic information (ranges, severity, fixes)
- Support quick fixes and code actions where appropriate
- Ensure diagnostics are fast enough for real-time feedback

## Maintenance

### Backward Compatibility

- Don't change existing diagnostic codes or their meanings
- Deprecate old codes before removing them
- Maintain serialization compatibility for configuration files

### Code Organization

- Keep related functionality together in modules
- Use clear module boundaries and public APIs
- Minimize dependencies between modules

### Version Management

- Follow semantic versioning for public APIs
- Document breaking changes in release notes
- Provide migration guides for major changes

## Common Patterns

### Regex Usage

- Use `lazy_static!` for compiled regexes
- Prefer specific patterns over overly broad ones
- Test regex patterns thoroughly with edge cases

### Configuration

- Support both code-based and label-based diagnostic references
- Use serde for serialization/deserialization
- Provide sensible defaults for all configuration options

### Error Recovery

- Continue processing after encountering errors when possible
- Collect multiple errors rather than failing on the first one
- Provide partial results when full analysis isn't possible

### Tools

- ELP uses a cargo workspace.
- Inside Meta, use `./meta/cargo.sh` instead of `cargo`
- Inside Meta, use `./meta/clippy.sh` to run clippy
- Use `arc lint --apply-patches` for formatting.

### Process

- Always run tests before finishing.
- Always run `./meta/cargo.sh clippy --tests` before submitting a diff
