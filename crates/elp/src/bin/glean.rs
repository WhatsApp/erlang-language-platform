/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code, unused)]
use std::io::Write;

use anyhow::Result;
use elp::cli::Cli;
use serde::Serialize;

use crate::args::Glean;

#[derive(Serialize, Debug)]
pub(crate) struct FileFact {
    #[serde(rename = "id")]
    file_id: u32,
    #[serde(rename = "key")]
    file_path: String,
}

#[derive(Serialize, Debug)]
pub(crate) struct FileLinesFact {
    key: FileLinesFactKey,
}

impl FileLinesFact {
    fn new(file_id: u32, lengths: Vec<u32>) -> Self {
        FileLinesFact {
            key: FileLinesFactKey {
                file_id,
                lengths,
                ends_with_new_line: true,
                unicode_or_tabs: true,
            },
        }
    }
}

#[derive(Serialize, Debug)]
struct FileLinesFactKey {
    #[serde(rename = "file")]
    file_id: u32,
    lengths: Vec<u32>,
    #[serde(rename = "endsInNewline")]
    ends_with_new_line: bool,
    #[serde(rename = "hasUnicodeOrTabs")]
    unicode_or_tabs: bool,
}

#[derive(Serialize, Debug)]
pub(crate) struct FunctionDeclarationFact {
    key: FunctionDeclarationKey,
}

impl FunctionDeclarationFact {
    fn new(file_id: u32, fqn: MFA, span: Location) -> Self {
        Self {
            key: FunctionDeclarationKey { file_id, fqn, span },
        }
    }
}

#[derive(Serialize, Debug)]
struct FunctionDeclarationKey {
    #[serde(rename = "file")]
    file_id: u32,
    fqn: MFA,
    span: Location,
}

#[derive(Serialize, Debug)]
pub(crate) struct XRefFact {
    key: XRefFactKey,
}

impl XRefFact {
    fn new(file_id: u32, xrefs: Vec<XRefFactVal>) -> Self {
        Self {
            key: XRefFactKey { file_id, xrefs },
        }
    }
}

#[derive(Serialize, Debug)]
struct XRefFactKey {
    #[serde(rename = "file")]
    file_id: u32,
    xrefs: Vec<XRefFactVal>,
}

#[derive(Serialize, Debug)]
struct XRefFactVal {
    source: Location,
    target: MFA,
}

impl XRefFactVal {
    fn new(source: Location, target: MFA) -> Self {
        Self { source, target }
    }
}

#[derive(Serialize, Debug, Clone)]
struct MFA {
    module: String,
    name: String,
    arity: u32,
}

impl MFA {
    fn new(module: String, name: String, arity: u32) -> Self {
        Self {
            module,
            name,
            arity,
        }
    }
}

#[derive(Serialize, Debug, Clone)]
struct Location {
    start: u32,
    length: u32,
}

#[derive(Serialize, Debug)]
#[serde(tag = "predicate")]
pub(crate) enum Fact {
    #[serde(rename = "src.File")]
    File { facts: Vec<FileFact> },
    #[serde(rename = "src.FileLines")]
    FileLine { facts: Vec<FileLinesFact> },
    #[serde(rename = "erlang.FunctionDeclaration")]
    FunctionDeclaration { facts: Vec<FunctionDeclarationFact> },
    #[serde(rename = "erlang.XRefsViaFqnByFile")]
    XRef { facts: Vec<XRefFact> },
}

pub(crate) fn index_facts(_args: &Glean, _cli: &mut dyn Cli) -> Result<Vec<Fact>> {
    Ok(vec![])
}

pub(crate) fn write_results(args: &Glean, cli: &mut dyn Cli, facts: Vec<Fact>) -> Result<()> {
    let mut writer: Box<dyn Write> = match &args.to {
        Some(to) => Box::new(
            std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(to)?,
        ),
        None => Box::new(cli),
    };
    let content = serde_json::to_string(&facts)?;
    writer.write_all(&content.as_bytes())?;
    Ok(())
}

pub fn index(args: &Glean, cli: &mut dyn Cli) -> Result<()> {
    let facts = index_facts(args, cli)?;
    write_results(args, cli, facts)
}

#[cfg(test)]
mod tests {

    use std::path::PathBuf;

    use elp::cli::Fake;
    use expect_test::expect_file;

    use super::*;

    #[test]
    fn serialization_test() {
        let mut cli = Fake::default();
        let args = Glean {
            project: PathBuf::from("."),
            module: None,
            to: None,
        };
        let file_id = 10071;
        let location = Location {
            start: 0,
            length: 10,
        };
        let mfa = MFA::new(
            "smax_product_catalog".into(),
            "product_visibility_update_request_iq".into(),
            0,
        );

        let file_fact = Fact::File {
            facts: vec![FileFact {
                file_id,
                file_path: "/local/whatsapp/server/erl/groupd_service/test/p13n/grpd_p13n_new_create_group_SUITE.erl".into(),
                }
            ],
        };
        let file_line_fact = Fact::FileLine {
            facts: vec![FileLinesFact::new(file_id, vec![71, 42])],
        };
        let func_decl_fact = Fact::FunctionDeclaration {
            facts: vec![FunctionDeclarationFact::new(
                file_id,
                mfa.clone(),
                location.clone(),
            )],
        };

        let xref_fact = Fact::XRef {
            facts: vec![XRefFact::new(
                file_id,
                vec![XRefFactVal::new(location, mfa)],
            )],
        };
        let facts = vec![file_fact, file_line_fact, func_decl_fact, xref_fact];

        write_results(&args, &mut cli, facts).unwrap();

        let (out, err) = cli.to_strings();
        let expected = expect_file!["../resources/test/glean/serialization_test.out"];
        expected.assert_eq(&out);
        assert!(err.is_empty())
    }
}
