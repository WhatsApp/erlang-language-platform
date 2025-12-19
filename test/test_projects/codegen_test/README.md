# Code Generation Test Project

This test project demonstrates Buck2-based code generation for Erlang
applications using `genrule`.

## Project Structure

```
codegen_test/
├── .elp.toml                      # ELP configuration
├── BUCK                           # Buck build configuration with genrule
├── README.md                      # This file
├── templates/                     # Erlang template files (input)
│   ├── example_service_types.erl  # Type definitions template
│   ├── example_service_client.erl # Client stubs template
│   └── example_service_types.hrl  # Header file template
├── generated/                     # Pre-generated code (for reference)
│   ├── example_service_types.erl  # Generated type module
│   ├── example_service_client.erl # Generated client stubs
│   └── example_service_types.hrl  # Generated header file
└── app_a/
    ├── src/
    │   ├── codegen_test.app.src   # Application metadata
    │   └── example_usage.erl      # Example using generated code
    └── test/
        └── codegen_test_SUITE.erl # Test suite
```

## How It Works

### 1. Template Files

The templates directory contains well-formed Erlang files that serve as input to
the code generation process. These are complete, valid Erlang files that are
copied to the output directory during the build.

### 2. Code Generation

The code is generated automatically during the build process using a
`genrule`:

```python
genrule(
    name = "example_service_types_erl",
    srcs = [
        "templates/example_service_types.erl",
    ],
    outs = {
        "example_service_types.erl": ["example_service_types.erl"],
    },
    cmd = "cp $SRCDIR/templates/example_service_types.erl $OUT/example_service_types.erl",
)
```

The genrule:

- Takes the template file as input (`srcs`)
- Defines named outputs using `outs` parameter (creates subtargets for each
  file)
- Uses `cp` command to copy template files to `$OUT`
- Outputs files to `$OUT` directory in `buck-out/`

### 3. Build Integration

The `BUCK` file references the generated files using subtarget syntax:

```python
erlang_app(
    name = "example_service_generated",
    srcs = [
        ":example_service_types_erl[example_service_types.erl]",
        ":example_service_client_erl[example_service_client.erl]",
    ],
    includes = [
        ":example_service_types_hrl[example_service_types.hrl]",
    ],
)
```

When Buck builds the application:

1. The genrule runs first, copying the template Erlang files to the output
2. The generated files are placed in
   `buck-out/v2/gen/.../example_service_types_erl/`
3. The `erlang_app` consumes these files via subtarget references (`[filename]`
   syntax)
4. The files are copied to the application's build directory in `buck-out/`

### 4. Using Generated Code

Application code includes the generated header and uses the types:

```erlang
-module(example_usage).
-include("example_service_types.hrl").

create_sample_user() ->
    #user_info{
        user_id = <<"user_123">>,
        username = <<"john_doe">>,
        age = 25,
        is_active = true
    }.

get_user_by_id(UserId) ->
    example_service_client:get_user(UserId).
```

## Building and Testing

```bash
# Build just the code generation step
buck2 build fbcode//whatsapp/elp/test/test_projects/codegen_test:example_service_types_erl

# Build the application (automatically runs code generation)
buck2 build fbcode//whatsapp/elp/test/test_projects/codegen_test:codegen_test_app

# Run tests
buck2 test fbcode//whatsapp/elp/test/test_projects/codegen_test:codegen_test_SUITE

# View generated files in buck-out
find buck-out -path "*codegen_test*" -name "example_service_*.erl"
```

## Test Coverage

The test suite (`app_a/test/codegen_test_SUITE.erl`) verifies:

- Generated record types work correctly
- Generated client functions are callable
- Record fields have correct types and defaults
- Integration with application code

All tests pass:

```
Tests finished: Pass 3. Fail 0. Fatal 0. Skip 0. Build failure 0
```
