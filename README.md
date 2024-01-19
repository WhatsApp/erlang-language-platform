# Erlang Language Platform (ELP)

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="./logo/elp_final_Full_Logo_White_Text.png">
  <img alt="ELP logo" src="./logo/elp_final_Full_Logo_Color.png" width="100%">
</picture>

## Description

ELP integrates Erlang into modern IDEs via the language server protocol.

ELP was inspired by [rust-analyzer](https://github.com/rust-lang/rust-analyzer).

## Terms of Use

You are free to copy, modify, and distribute ELP with attribution under the
terms of the Apache-2.0 and MIT licences.  See [LICENCE-APACHE](./LICENCE-APACHE) and
[LICENCE-MIT](./LICENSE-MIT) for details.

## Install

There are installation instructions for:

* [Emacs](https://whatsapp.github.io/erlang-language-platform/docs/get-started/emacs/)
* [VS Code](https://whatsapp.github.io/erlang-language-platform/docs/get-started/vscode/)

## How to use ELP

### Using it with rebar3 projects

1. Use OTP 25
2. Download the `elp` binary for your system from https://github.com/WhatsApp/erlang-language-platform/releases

    > On Mac you will probably get the following message when trying to run the executable the first time: "elp cannot be opened because the developer cannot be verified.".
    To solve this, go to Preferences > Security and Privacy and add an exception. Alternatively, you can build elp from source.

3. Add `eqwalizer_support` dependency and `eqwalizer_rebar3` plugin
   to your rebar3 project definition (see below)
4. From the project directory run:
  - `elp eqwalize <module>` to type-check a single module
  - `elp eqwalize-all` to type-check all `src` modules in the project


Adding `eqwalizer_support` and `eqwalizer_rebar3`:

```
{deps, [
  {eqwalizer_support,
    {git_subdir,
        "https://github.com/whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_support"}}
]}.

{project_plugins, [
  {eqwalizer_rebar3,
    {git_subdir,
        "https://github.com/whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_rebar3"}}
]}.
```

## References

* [rust-analyzer](https://github.com/rust-lang/rust-analyzer)

## Contributing

* [CONTRIBUTING.md](./CONTRIBUTING.md): Provides an overview of how to contribute changes to ELP (e.g., diffs, testing, etc)

## FAQ

Please refer to [the FAQ document](./FAQ.md) for answers to some common questions,
including:

- What's the difference between ELP and Erlang LS?
- Why not extend Erlang LS, rather than creating a new tool?
- Why is ELP implemented in Rust, rather than Erlang?

## License

erlang-language-platform is dual-licensed
* [Apache](./LICENSE-APACHE).
* [MIT](./LICENSE-MIT).
