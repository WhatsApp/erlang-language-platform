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
        write!(f, "Common Test conversion failed with {self:?}")
    }
}

pub fn all(term: &Term) -> Result<FxHashSet<TestDef>, ConversionError> {
    let mut res = FxHashSet::default();
    match term {
        Term::List(list) => {
            for td in &list.elements {
                let td = parse_test_definition(td)?;
                res.insert(td);
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
            // `ct_groups:find/7` reads `{Name, Tests}` as `{Name, [], Tests}`.
            [Term::Atom(group), group_content @ Term::List(_)] => {
                let content = parse_group_content(group_content)?;
                Ok(TestDef::GroupDef(group.name.clone().into(), content))
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

#[cfg(test)]
mod tests {
    use elp_base_db::assert_eq_expected;

    use super::*;

    fn atom(name: &str) -> Term {
        eetf::Atom::from(name).into()
    }

    fn tuple(elements: Vec<Term>) -> Term {
        eetf::Tuple::from(elements).into()
    }

    fn list(elements: Vec<Term>) -> Term {
        eetf::List::from(elements).into()
    }

    fn test_name(name: &str) -> TestDef {
        TestDef::TestName(name.into())
    }

    #[test]
    fn nested_group_without_properties_is_a_group_definition() {
        let expected = Ok(TestDef::GroupDef(
            "header".into(),
            vec![test_name("test_oneway"), test_name("test_rsocket")],
        ));
        assert_eq_expected!(
            expected,
            parse_group_content_entry(&tuple(vec![
                atom("header"),
                list(vec![atom("test_oneway"), atom("test_rsocket")]),
            ]))
        );
    }

    #[test]
    fn nested_group_with_properties_is_a_group_definition() {
        let expected = Ok(TestDef::GroupDef(
            "header".into(),
            vec![test_name("test_oneway")],
        ));
        assert_eq_expected!(
            expected,
            parse_group_content_entry(&tuple(vec![
                atom("header"),
                list(vec![atom("parallel")]),
                list(vec![atom("test_oneway")]),
            ]))
        );
    }

    #[test]
    fn group_reference_is_not_a_group_definition() {
        let expected = Ok(TestDef::GroupName("header".into()));
        assert_eq_expected!(
            expected,
            parse_group_content_entry(&tuple(vec![atom("group"), atom("header")]))
        );
    }

    #[test]
    fn external_testcase_reference_is_not_a_group_definition() {
        let expected = Err(ConversionError::InvalidGroupContentEntry);
        assert_eq_expected!(
            expected,
            parse_group_content_entry(&tuple(vec![atom("other_SUITE"), atom("a_test")]))
        );
    }

    #[test]
    fn groups_callback_accepts_a_nested_group_without_properties() {
        // groups() -> [{tracer, [{header, [test_oneway]}]}].
        let term = list(vec![tuple(vec![
            atom("tracer"),
            list(vec![tuple(vec![
                atom("header"),
                list(vec![atom("test_oneway")]),
            ])]),
        ])]);
        let expected = Ok(vec![GroupDef {
            name: "tracer".into(),
            content: vec![TestDef::GroupDef(
                "header".into(),
                vec![test_name("test_oneway")],
            )],
        }]);
        assert_eq_expected!(
            expected,
            groups(&term).map(|groups| groups.into_values().collect::<Vec<_>>())
        );
    }
}
