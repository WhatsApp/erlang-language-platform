/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use eetf::Term;
use elp_syntax::SmolStr;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TestDef {
    TestName(SmolStr),
    GroupName(SmolStr),
    GroupDef(SmolStr, Vec<TestDef>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GroupDef {
    pub name: SmolStr,
    pub content: Vec<TestDef>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ConversionError {
    InvalidAllCallback,
    InvalidGroupsCallback,
    InvalidTestDefinition,
    InvalidGroupDefinition,
    InvalidGroupContentEntry,
}

impl fmt::Display for ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Common Test conversion failed with {:?}", self)
    }
}

pub fn all(term: &Term) -> Result<FxHashSet<TestDef>, ConversionError> {
    let mut res = FxHashSet::default();
    match term {
        Term::List(list) => {
            for td in &list.elements {
                match parse_test_definition(td) {
                    Ok(td) => res.insert(td),
                    Err(err) => return Err(err),
                };
            }
        }
        _ => return Err(ConversionError::InvalidAllCallback),
    }
    Ok(res)
}

pub fn parse_test_definition(term: &Term) -> Result<TestDef, ConversionError> {
    match term {
        Term::Atom(testcase) => Ok(TestDef::TestName(testcase.name.clone().into())),
        Term::Tuple(tuple) => match &tuple.elements[..] {
            [Term::Atom(label), Term::Atom(group)] if label.name == "group" => {
                Ok(TestDef::GroupName(group.name.clone().into()))
            }
            [Term::Atom(label), Term::Atom(group), _properties] if label.name == "group" => {
                Ok(TestDef::GroupName(group.name.clone().into()))
            }
            [Term::Atom(label), Term::Atom(testcase), _properties] if label.name == "testcase" => {
                Ok(TestDef::TestName(testcase.name.clone().into()))
            }
            [
                Term::Atom(label),
                Term::Atom(group),
                _properties,
                _sub_properties,
            ] if label.name == "group" => Ok(TestDef::GroupName(group.name.clone().into())),
            _ => Err(ConversionError::InvalidTestDefinition),
        },
        _ => Err(ConversionError::InvalidTestDefinition),
    }
}

pub fn groups(term: &Term) -> Result<FxHashMap<SmolStr, GroupDef>, ConversionError> {
    let mut res = FxHashMap::default();

    match term {
        Term::List(list) => {
            for gd in &list.elements {
                let gd = parse_group(gd)?;
                res.insert(gd.name.clone(), gd);
            }
        }
        _ => return Err(ConversionError::InvalidGroupsCallback),
    }
    Ok(res)
}

pub fn parse_group(term: &Term) -> Result<GroupDef, ConversionError> {
    match term {
        Term::Tuple(tuple) => match &tuple.elements[..] {
            [Term::Atom(group), group_content] => Ok(GroupDef {
                name: group.name.clone().into(),
                content: parse_group_content(group_content)?,
            }),
            [Term::Atom(group), _properties, group_content] => Ok(GroupDef {
                name: group.name.clone().into(),
                content: parse_group_content(group_content)?,
            }),
            _ => Err(ConversionError::InvalidGroupDefinition),
        },
        _ => Err(ConversionError::InvalidGroupDefinition),
    }
}

pub fn parse_group_content(term: &Term) -> Result<Vec<TestDef>, ConversionError> {
    let mut res = Vec::new();
    match term {
        Term::List(list) => {
            for ce in &list.elements {
                res.push(parse_group_content_entry(ce)?)
            }
        }
        _ => return Err(ConversionError::InvalidGroupDefinition),
    };
    Ok(res)
}

pub fn parse_group_content_entry(term: &Term) -> Result<TestDef, ConversionError> {
    match term {
        Term::Atom(testcase) => Ok(TestDef::TestName(testcase.name.clone().into())),
        Term::Tuple(tuple) => match &tuple.elements[..] {
            [Term::Atom(label), Term::Atom(group)] if label.name == "group" => {
                Ok(TestDef::GroupName(group.name.clone().into()))
            }
            [Term::Atom(group), _properties, group_content] => {
                let content = parse_group_content(group_content)?;
                Ok(TestDef::GroupDef(group.name.clone().into(), content))
            }
            _ => Err(ConversionError::InvalidGroupContentEntry),
        },
        _ => Err(ConversionError::InvalidGroupContentEntry),
    }
}
