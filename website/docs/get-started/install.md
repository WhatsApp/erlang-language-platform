---
sidebar_position: 2
---

# Install ELP

The easiest way to install to ELP is [from binary](#from-binary). It is also possible to compile it [from source](#from-source).

## From Binary

Visit our [releases](https://github.com/WhatsApp/erlang-language-platform/releases) page and download the `elp` tarball for the latest releases.

Packages are available in the _Assets_ section below each release. Each tarball name has the format:

```
elp-[OS]-[ARCH]-[c]-otp-[OTP_VERSION].tar.gz
```

Pick the appropriate version according to your Operating System (OS), Architecture (ARCH) and Erlang/OTP version (OTP_VERSION). For example, for my Darwin MacBook Pro where I have installed Erlang/OTP 25, I will pick:

```
elp-macos-x86_64-apple-darwin-otp-25.3.tar.gz
```

:::tip

Unsure about your architecture? Try the `uname -a` command.
Unsure about your Erlang/OTP version? Try `erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell`

:::

Untar the package and place the `elp` binary in a place visible by your `PATH``. For example:

```
cd ~/Downloads
tar -xvzf elp-macos-x86_64-apple-darwin-otp-25.3.tar.gz
mv elp ~/bin
export PATH=$PATH:~/bin
```

## From Source

TBD.

## Verify `elp` is correctly installed

:::tip

On Mac you may get a warning, saying "elp cannot be opened because the developer cannot be verified". To solve this, go to `Preferences -> Security and Privacy -> General` and add an exception for `elp`. Alternatively, you can build the project from source (see below)

:::

Open a terminal and run:

```
elp version
```

You should see something like:

```
elp 1.1.0+build-2024-01-18
```

If that's the case, you're ready to roll!
