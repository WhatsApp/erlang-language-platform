---
sidebar_position: 2
---

# Install ELP

The easiest way to install ELP on Mac is [using Homebrew](#using-homebrew-mac).
For other platforms, you can install [from binary](#from-binary). It is also
possible to compile it [from source](#from-source).

## Using Homebrew (Mac)

Mac users can install ELP using the dedicated Homebrew formula:

```
brew install erlang-language-platform
```

This will install the latest version of ELP and make it available in your PATH.

For more information about the Homebrew formula, visit:
https://formulae.brew.sh/formula/erlang-language-platform

Follow [these steps](cli.md#verify-elp-is-correctly-installed) to verify ELP is
correctly installed.

## From Binary

Visit our
[releases](https://github.com/WhatsApp/erlang-language-platform/releases) page
and download the `elp` tarball for the latest releases.

Packages are available in the _Assets_ section below each release. Each tarball
name has the format:

```
elp-[OS]-[ARCH]-[c]-otp-[OTP_VERSION].tar.gz
```

Pick the appropriate version according to your Operating System (OS),
Architecture (ARCH) and Erlang/OTP version (OTP_VERSION). For example, for my
Darwin MacBook Pro where I have installed Erlang/OTP 25, I will pick:

```
elp-macos-x86_64-apple-darwin-otp-25.3.tar.gz
```

:::tip

Unsure about your architecture? Try the `uname -a` command. Unsure about your
Erlang/OTP version? Try
`erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell`

:::

Untar the package and place the `elp` binary in a place visible by your `PATH``.
For example:

```
cd ~/Downloads
tar -xvzf elp-macos-x86_64-apple-darwin-otp-25.3.tar.gz
mv elp ~/bin
export PATH=$PATH:~/bin
```

Follow [these steps](cli.md#verify-elp-is-correctly-installed) to verify ELP is
correctly installed.

## From Source

Clone the ELP repository, including submodules:

```
git clone --recurse-submodules https://github.com/WhatsApp/erlang-language-platform.git
cd erlang-language-platform
```

Enter the `eqwalizer` submodule and build it. Notice the double `eqwalizer` in
the `pushd` command.

```
pushd eqwalizer/eqwalizer
sbt assembly
popd
```

Point the `ELP_EQWALIZER_PATH` environment variable to the path of the produced
`eqwalizer.jar` file:

```
export ELP_EQWALIZER_PATH=$(find "$(pwd)" -name eqwalizer.jar)
```

Point the `EQWALIZER_DIR` environment variable to the path of the
`eqwalizer_support` directory:

```
export EQWALIZER_DIR=$(find "$(pwd)" -name eqwalizer_support)
```

Now we can compile ELP:

:::tip

The commands below will produce a release build, which has the best runtime
performance, at the price of a slower compilation time. If you are developing
ELP, consider using `cargo build --profile release-thin`, as it provides a much
faster development loop while still producing a performant binary. You will find
the corresponding binary in `target/release-thin/elp`.

:::

```
cargo build --release
```

The produced executable will be available in: `target/release/elp`, so ensure it
is included in your `PATH`. E.g.:

```
mkdir -p ~/bin
mv target/release/elp ~/bin
export PATH=$PATH:~/bin
```
