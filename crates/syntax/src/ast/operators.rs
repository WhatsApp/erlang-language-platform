/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `bnot`
    Bnot,
    /// `not`
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            UnaryOp::Plus => "+",
            UnaryOp::Minus => "-",
            UnaryOp::Bnot => "bnot",
            UnaryOp::Not => "not",
        };
        f.write_str(str)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    LogicOp(LogicOp),
    ArithOp(ArithOp),
    ListOp(ListOp),
    CompOp(CompOp),
    /// `!`
    Send,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::LogicOp(logic_op) => logic_op.fmt(f),
            BinaryOp::ArithOp(arith_op) => arith_op.fmt(f),
            BinaryOp::ListOp(list_op) => list_op.fmt(f),
            BinaryOp::CompOp(comp_op) => comp_op.fmt(f),
            BinaryOp::Send => f.write_str("!"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum LogicOp {
    /// `and` and `andalso`
    And { lazy: bool },
    /// `or` and `orelse`
    Or { lazy: bool },
    /// `xor`
    Xor,
}

impl fmt::Display for LogicOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            LogicOp::And { lazy: true } => "andalso",
            LogicOp::And { lazy: false } => "and",
            LogicOp::Or { lazy: true } => "orelse",
            LogicOp::Or { lazy: false } => "or",
            LogicOp::Xor => "xor",
        };
        f.write_str(str)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArithOp {
    /// `+`
    Add,
    /// `*`
    Mul,
    /// `-`
    Sub,
    /// `/`
    FloatDiv,
    /// `div`
    Div,
    /// `rem`
    Rem,
    /// `band`
    Band,
    /// `bor`
    Bor,
    /// `bxor`
    Bxor,
    /// `bsr`
    Bsr,
    /// `bsl`
    Bsl,
}

impl fmt::Display for ArithOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            ArithOp::Add => "+",
            ArithOp::Mul => "*",
            ArithOp::Sub => "-",
            ArithOp::FloatDiv => "/",
            ArithOp::Div => "div",
            ArithOp::Rem => "rem",
            ArithOp::Band => "band",
            ArithOp::Bor => "bor",
            ArithOp::Bxor => "bxor",
            ArithOp::Bsr => "bsr",
            ArithOp::Bsl => "bsl",
        };
        f.write_str(str)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ListOp {
    /// `++`
    Append,
    /// `--`
    Subtract,
}

impl fmt::Display for ListOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            ListOp::Append => "++",
            ListOp::Subtract => "--",
        };
        f.write_str(str)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ordering {
    Less,
    Greater,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompOp {
    /// `==`, `/=` `=:=` and `=/=`
    Eq { strict: bool, negated: bool },
    /// `=<`, `<`, `>=`, `>`
    Ord { ordering: Ordering, strict: bool },
}

impl fmt::Display for CompOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[rustfmt::skip]
        let str = match self {
            CompOp::Eq { strict: true, negated: false } => "=:=",
            CompOp::Eq { strict: false, negated: false } => "==",
            CompOp::Eq { strict: true, negated: true } => "=/=",
            CompOp::Eq { strict: false, negated: true } => "/=",
            CompOp::Ord { ordering: Ordering::Greater, strict: true } => ">",
            CompOp::Ord { ordering: Ordering::Greater, strict: false } => ">=",
            CompOp::Ord { ordering: Ordering::Less, strict: true } => "<",
            CompOp::Ord { ordering: Ordering::Less, strict: false } => "=<",
        };
        f.write_str(str)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum MapOp {
    /// `=>`
    Assoc,
    /// `:=`
    Exact,
}

impl fmt::Display for MapOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            MapOp::Assoc => "=>",
            MapOp::Exact => ":=",
        };
        f.write_str(str)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GeneratorOp {
    Plain { strict: bool },
    Binary { strict: bool },
}

impl fmt::Display for GeneratorOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            GeneratorOp::Plain { strict: false } => "<-",
            GeneratorOp::Plain { strict: true } => "<:-",
            GeneratorOp::Binary { strict: false } => "<=",
            GeneratorOp::Binary { strict: true } => "<:=",
        };
        f.write_str(str)
    }
}
