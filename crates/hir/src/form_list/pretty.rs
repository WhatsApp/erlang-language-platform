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
use std::fmt::Formatter;
use std::fmt::Write as _;

use la_arena::IdxRange;
use la_arena::RawIdx;

use super::DocAttribute;
use super::DocMetadataAttribute;
use super::FeatureAttribute;
use super::ModuleDocAttribute;
use super::ModuleDocMetadataAttribute;
use super::SsrDefinition;
use crate::Attribute;
use crate::Behaviour;
use crate::Callback;
use crate::CompileOption;
use crate::Define;
use crate::Export;
use crate::FaEntry;
use crate::FormIdx;
use crate::FormList;
use crate::FunctionClause;
use crate::Import;
use crate::IncludeAttribute;
use crate::ModuleAttribute;
use crate::OptionalCallbacks;
use crate::PPCondition;
use crate::PPConditionId;
use crate::PPDirective;
use crate::Record;
use crate::Spec;
use crate::TypeAlias;
use crate::TypeExport;
use crate::form_list::DeprecatedAttribute;
use crate::form_list::DeprecatedDesc;
use crate::form_list::DeprecatedFa;

pub fn print(forms: &FormList) -> String {
    let mut printer = Printer {
        forms,
        buf: String::new(),
    };

    for &form in forms.forms() {
        printer.print_form(form).unwrap();
    }

    printer.buf.truncate(printer.buf.trim_end().len());
    printer.buf.push('\n');
    printer.buf
}

struct Printer<'a> {
    forms: &'a FormList,
    buf: String,
}

impl Printer<'_> {
    pub fn print_form(&mut self, form: FormIdx) -> fmt::Result {
        match form {
            FormIdx::ModuleAttribute(idx) => self.print_module_attribute(&self.forms[idx])?,
            FormIdx::FunctionClause(idx) => self.print_function_clause(&self.forms[idx])?,
            FormIdx::PPDirective(idx) => self.print_pp_directive(&self.forms[idx])?,
            FormIdx::PPCondition(idx) => self.print_pp_condition(&self.forms[idx], idx)?,
            FormIdx::Export(idx) => self.print_export(&self.forms[idx])?,
            FormIdx::Import(idx) => self.print_import(&self.forms[idx])?,
            FormIdx::TypeExport(idx) => self.print_type_export(&self.forms[idx])?,
            FormIdx::Behaviour(idx) => self.print_behaviour(&self.forms[idx])?,
            FormIdx::TypeAlias(idx) => self.print_type_alias(&self.forms[idx])?,
            FormIdx::OptionalCallbacks(idx) => self.print_optional_callbacks(&self.forms[idx])?,
            FormIdx::Spec(idx) => self.print_spec(&self.forms[idx])?,
            FormIdx::Callback(idx) => self.print_callback(&self.forms[idx])?,
            FormIdx::Record(idx) => self.print_record(&self.forms[idx])?,
            FormIdx::CompileOption(idx) => self.print_compile(&self.forms[idx])?,
            FormIdx::Attribute(idx) => self.print_attribute(&self.forms[idx])?,
            FormIdx::ModuleDocAttribute(idx) => self.print_moduledoc_attribute(&self.forms[idx])?,
            FormIdx::ModuleDocMetadataAttribute(idx) => {
                self.print_moduledoc_metadata_attribute(&self.forms[idx])?
            }
            FormIdx::DocAttribute(idx) => self.print_doc_attribute(&self.forms[idx])?,
            FormIdx::DocMetadataAttribute(idx) => {
                self.print_doc_metadata_attribute(&self.forms[idx])?
            }
            FormIdx::FeatureAttribute(idx) => self.print_feature_attribute(&self.forms[idx])?,
            FormIdx::DeprecatedAttribute(idx) => self.print_deprecated(&self.forms[idx])?,
            FormIdx::SsrDefinition(idx) => self.print_ssr(&self.forms[idx])?,
        }
        writeln!(self)
    }

    fn print_module_attribute(&mut self, module_attribute: &ModuleAttribute) -> fmt::Result {
        writeln!(
            self,
            "-module({}). %% cond: {:?}",
            module_attribute.name,
            raw_cond(&module_attribute.cond)
        )
    }

    fn print_include(&mut self, include: &IncludeAttribute) -> fmt::Result {
        match include {
            IncludeAttribute::Include {
                path,
                cond,
                form_id: _,
            } => {
                writeln!(self, "-include({:?}). %% cond: {:?}", path, raw_cond(cond))
            }
            IncludeAttribute::IncludeLib {
                path,
                cond,
                form_id: _,
            } => {
                writeln!(
                    self,
                    "-inlcude_lib({:?}). %% cond: {:?}",
                    path,
                    raw_cond(cond)
                )
            }
        }
    }

    fn print_function_clause(&mut self, function: &FunctionClause) -> fmt::Result {
        let args = BlankArgs(function.name.arity());
        writeln!(
            self,
            "{}({}) -> .... %% cond: {:?}",
            function.name.name(),
            args,
            raw_cond(&function.cond)
        )
    }

    fn print_pp_directive(&mut self, pp: &PPDirective) -> fmt::Result {
        match pp {
            PPDirective::Define(idx) => self.print_define(&self.forms[*idx]),
            PPDirective::Undef {
                name,
                cond,
                form_id: _,
            } => {
                writeln!(self, "-undef({}). %% cond: {:?}", name, raw_cond(cond))
            }
            PPDirective::Include(idx) => self.print_include(&self.forms[*idx]),
        }
    }

    fn print_define(&mut self, define: &Define) -> fmt::Result {
        if let Some(arity) = define.name.arity() {
            writeln!(
                self,
                "-define({}({}), ...). %% cond: {:?}",
                define.name.name(),
                BlankArgs(arity),
                raw_cond(&define.cond)
            )
        } else {
            writeln!(
                self,
                "-define({}, ...). %% cond: {:?}",
                define.name.name(),
                raw_cond(&define.cond)
            )
        }
    }

    fn print_pp_condition(&mut self, pp: &PPCondition, idx: PPConditionId) -> fmt::Result {
        match pp {
            PPCondition::Ifdef {
                cond,
                name,
                form_id: _,
            } => writeln!(
                self,
                "-ifdef({}). %% {:?}, cond: {:?}",
                name,
                idx.into_raw(),
                raw_cond(cond)
            ),
            PPCondition::Ifndef {
                cond,
                name,
                form_id: _,
            } => writeln!(
                self,
                "-ifndef({}). %% {:?}, cond: {:?}",
                name,
                idx.into_raw(),
                raw_cond(cond)
            ),
            PPCondition::Endif { prev, form_id: _ } => {
                writeln!(self, "-endif. %% prev: {:?}", prev.into_raw())
            }
            PPCondition::If { cond, form_id: _ } => {
                writeln!(
                    self,
                    "-if(...). %% {:?}, cond: {:?}",
                    idx.into_raw(),
                    raw_cond(cond)
                )
            }
            PPCondition::Elif { prev, form_id: _ } => {
                writeln!(
                    self,
                    "-elif(...). %% {:?}, prev: {:?}",
                    idx.into_raw(),
                    prev.into_raw()
                )
            }
            PPCondition::Else { prev, form_id: _ } => {
                writeln!(
                    self,
                    "-else. %% {:?}, prev: {:?}",
                    idx.into_raw(),
                    prev.into_raw()
                )
            }
        }
    }

    fn print_export(&mut self, export: &Export) -> fmt::Result {
        write!(self, "-export(")?;
        self.print_entries(&export.entries, export.cond)
    }

    fn print_import(&mut self, import: &Import) -> fmt::Result {
        write!(self, "-import({}, ", import.from)?;
        self.print_entries(&import.entries, import.cond)
    }

    fn print_type_export(&mut self, export: &TypeExport) -> fmt::Result {
        write!(self, "-export_type(")?;
        self.print_entries(&export.entries, export.cond)
    }

    fn print_entries(
        &mut self,
        entries: &IdxRange<FaEntry>,
        cond: Option<PPConditionId>,
    ) -> fmt::Result {
        if entries.is_empty() {
            writeln!(self, "[]). %% cond: {:?}", raw_cond(&cond))
        } else {
            writeln!(self, "[ %% cond: {:?}", raw_cond(&cond))?;
            let mut sep = "";
            for entry_id in entries.clone() {
                write!(self, "{}    {}", sep, self.forms[entry_id].name)?;
                sep = ",\n";
            }
            writeln!(self, "\n]).")
        }
    }

    fn print_behaviour(&mut self, behaviour: &Behaviour) -> fmt::Result {
        writeln!(
            self,
            "-behaviour({}). %% cond: {:?}",
            behaviour.name,
            raw_cond(&behaviour.cond)
        )
    }

    fn print_type_alias(&mut self, alias: &TypeAlias) -> fmt::Result {
        let (attr, name, cond) = match alias {
            TypeAlias::Regular {
                name,
                cond,
                form_id: _,
            } => ("type", name, cond),
            TypeAlias::Nominal {
                name,
                cond,
                form_id: _,
            } => ("nominal", name, cond),
            TypeAlias::Opaque {
                name,
                cond,
                form_id: _,
            } => ("opaque", name, cond),
        };
        let args = BlankArgs(name.arity());
        let name = name.name();

        writeln!(
            self,
            "-{} {}({}) :: .... %% cond: {:?}",
            attr,
            name,
            args,
            raw_cond(cond)
        )
    }

    fn print_optional_callbacks(&mut self, cbs: &OptionalCallbacks) -> fmt::Result {
        write!(self, "-optional_callbacks(")?;
        self.print_entries(&cbs.entries, cbs.cond)
    }

    fn print_spec(&mut self, spec: &Spec) -> fmt::Result {
        let args = BlankArgs(spec.name.arity());
        let name = spec.name.name();
        writeln!(
            self,
            "-spec {}({}) -> .... %% cond: {:?}",
            name,
            args,
            raw_cond(&spec.cond)
        )
    }

    fn print_callback(&mut self, callback: &Callback) -> fmt::Result {
        let args = BlankArgs(callback.name.arity());
        let name = callback.name.name();
        writeln!(
            self,
            "-callback {}({}) -> .... %% cond: {:?}",
            name,
            args,
            raw_cond(&callback.cond)
        )
    }

    fn print_record(&mut self, record: &Record) -> fmt::Result {
        if record.fields.is_empty() {
            writeln!(
                self,
                "-record({}, {{}}). %% cond: {:?}",
                record.name,
                raw_cond(&record.cond)
            )
        } else {
            writeln!(
                self,
                "-record({}, {{ %% cond: {:?}",
                record.name,
                raw_cond(&record.cond)
            )?;
            let mut sep = "";
            for field_id in record.fields.clone() {
                write!(self, "{}    {}", sep, self.forms[field_id].name)?;
                sep = ",\n";
            }
            writeln!(self, "\n}}).")
        }
    }

    fn print_compile(&mut self, compile: &CompileOption) -> fmt::Result {
        writeln!(
            self,
            "-compile(...). %% cond: {:?}",
            raw_cond(&compile.cond)
        )
    }

    fn print_attribute(&mut self, attribute: &Attribute) -> fmt::Result {
        writeln!(
            self,
            "-{}(...). %% cond: {:?}",
            attribute.name,
            raw_cond(&attribute.cond)
        )
    }

    fn print_moduledoc_attribute(&mut self, attribute: &ModuleDocAttribute) -> fmt::Result {
        writeln!(
            self,
            "-moduledoc(...). %% cond: {:?}",
            raw_cond(&attribute.cond)
        )
    }

    fn print_moduledoc_metadata_attribute(
        &mut self,
        attribute: &ModuleDocMetadataAttribute,
    ) -> fmt::Result {
        writeln!(
            self,
            "-moduledoc(...). %% cond: {:?}",
            raw_cond(&attribute.cond)
        )
    }

    fn print_doc_attribute(&mut self, attribute: &DocAttribute) -> fmt::Result {
        writeln!(self, "-doc(...). %% cond: {:?}", raw_cond(&attribute.cond))
    }

    fn print_doc_metadata_attribute(&mut self, attribute: &DocMetadataAttribute) -> fmt::Result {
        writeln!(self, "-doc(...). %% cond: {:?}", raw_cond(&attribute.cond))
    }

    fn print_feature_attribute(&mut self, attribute: &FeatureAttribute) -> fmt::Result {
        writeln!(
            self,
            "-feature(...). %% cond: {:?}",
            raw_cond(&attribute.cond)
        )
    }

    fn print_deprecated(&mut self, attribute: &DeprecatedAttribute) -> fmt::Result {
        match attribute {
            DeprecatedAttribute::Module { cond, .. } => {
                writeln!(self, "-deprecated(module). %% cond: {:?}", raw_cond(cond))
            }
            DeprecatedAttribute::Fa { fa, cond, .. } => {
                writeln!(self, "-deprecated({}). %% cond: {:?}", fa, raw_cond(cond))
            }
            DeprecatedAttribute::Fas { fas, cond, .. } => {
                writeln!(
                    self,
                    "-deprecated({}). %% cond: {:?}",
                    DeprecatedFas(fas),
                    raw_cond(cond)
                )
            }
        }
    }
    fn print_ssr(&mut self, ssr: &SsrDefinition) -> fmt::Result {
        writeln!(self, "ssr:(...). %% cond: {:?}", raw_cond(&ssr.cond))
    }
}

impl fmt::Display for DeprecatedDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DeprecatedDesc::Str(s) => write!(f, "\"{s}\""),
            DeprecatedDesc::Atom(s) => fmt::Display::fmt(s, f),
        }
    }
}

impl fmt::Display for DeprecatedFa {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut name = &self.name.as_str();
        if name == &"_" {
            name = &"'_'";
        }
        match (&self.arity, &self.desc) {
            (Some(arity), Some(desc)) => write!(f, "{{{name}, {arity}, {desc}}}"),
            (Some(arity), None) => write!(f, "{{{name}, {arity}}}"),
            (None, Some(desc)) => write!(f, "{{{name}, '_', {desc}}}"),
            (None, None) => write!(f, "{{{name}, '_'}}"),
        }
    }
}

struct DeprecatedFas<'a>(&'a Vec<DeprecatedFa>);

impl fmt::Display for DeprecatedFas<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, fa) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            write!(f, "{fa}")?;
        }
        write!(f, "]")
    }
}

struct BlankArgs(u32);

impl fmt::Display for BlankArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut join = "";
        for _ in 0..self.0 {
            write!(f, "{join}_")?;
            join = ", "
        }
        Ok(())
    }
}

fn raw_cond(cond: &Option<PPConditionId>) -> Option<RawIdx> {
    cond.map(|cond| cond.into_raw())
}

impl fmt::Write for Printer<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buf.push_str(s);
        Ok(())
    }
}
