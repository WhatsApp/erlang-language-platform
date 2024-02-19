---
sidebar_position: 2
---

# Install ELP

The easiest way to install to ELP is [from binary](#from-binary). It is also
possible to compile it [from source](#from-source).

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

### Prerequisites

To be able to compile ELP from source, you need a copy of the
[eqWAlizer](https://github.com/WhatsApp/eqwalizer) typechecker for Erlang.

Clone the eqWAlizer repository:

```
git clone https://github.com/WhatsApp/eqwalizer.git
```

Enter the `eqwalizer` repository and build it. Notice the double `eqwalizer` in
the `pushd` command.

```
pushd eqwalizer/eqwalizer
sbt assembly
popd
```

Get the path of the produced `eqwalizer.jar` file:

```
find . -name eqwalizer.jar | readlink -f
```

Point the `ELP_EQWALIZER_PATH` environment variable to the path returned above:

```
export ELP_EQWALIZER_PATH=/path/to/eqwalizer.jar
```

### Compile ELP

Clone the ELP repository:

```
git clone https://github.com/WhatsApp/erlang-language-platform.git
```

Enter the ELP repo and compile it:

```
cd erlang-language-platform
cargo build --release
```

The produced executable will be available in: `target/release/elp`, so ensure it is included in your `PATH`. E.g.:

```
mkdir -p ~/bin
mv target/releases/elp ~/bin
export PATH=$PATH:~/bin
```
