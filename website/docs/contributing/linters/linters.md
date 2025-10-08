---
sidebar_position: 1
title: 'Linters'
---

# Linters

ELP provides a flexible framework for adding custom linters (diagnostics) to
detect issues in Erlang code. This guide walks you through the process of
creating a new linter.

## Overview

Linters in ELP are implemented as diagnostics that analyze Erlang code and
report issues. Today ELP counts over
[50 linters](https://github.com/WhatsApp/erlang-language-platform/tree/main/crates/ide/src/diagnostics),
and the number is growing. It is usually a good idea to look at existing linters
to ensure a similar functionality is not already provided and to get inspiration
for a new linter.

At high level, each linter consists of:

1. A **DiagnosticCode** - A unique identifier for the diagnostic
2. A linter **module** - A file where the linter is implemented
3. **Documentation** - A user-facing explanation of the diagnostic
4. **Tests** - A number of test cases for the diagnostic

In the following sections, we will walk through each of these steps in detail,
learning how to create a new linter.
