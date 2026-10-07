# eqwalizer_support library

This library provides API and integrations points for eqWAlizer, including alternative (more type-checking-friendly) specs for some essential functions and definitions for some types from OTP libraries.

ELP bundles this library and adds it to every project. Specs that refer to `eqwalizer` types compile and run without the library, but other tools, such as Dialyzer, report those types as unknown. If you use such tools, add the library to your dependencies, and ELP uses your copy.

Minimal rebar3 config:

```
{deps, [
    {eqwalizer_support, {git_subdir, "https://github.com/whatsapp/erlang-language-platform.git", {branch, "main"}, "eqwalizer/eqwalizer_support"}}
]}.
```
