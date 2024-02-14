/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_syntax::ast;
use elp_syntax::ast::DeprecatedFunArity;
use elp_syntax::ast::Desc;
use elp_syntax::unescape;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::SmolStr;
use elp_syntax::SyntaxNode;
use fxhash::FxHashMap;
use la_arena::Idx;
use la_arena::IdxRange;
use la_arena::RawIdx;
use profile::Count;

use super::form_id::FormIdMap;
use super::FeatureAttribute;
use super::FormIdx;
use super::FormListData;
use super::ParamName;
use crate::db::DefDatabase;
use crate::form_list::DeprecatedAttribute;
use crate::form_list::DeprecatedDesc;
use crate::form_list::DeprecatedFa;
use crate::macro_exp::MacroExpCtx;
use crate::name::AsName;
use crate::Attribute;
use crate::Behaviour;
use crate::Callback;
use crate::CompileOption;
use crate::Define;
use crate::DefineId;
use crate::Diagnostic;
use crate::DiagnosticMessage;
use crate::Export;
use crate::FaEntry;
use crate::FaEntryId;
use crate::FormList;
use crate::FunctionClause;
use crate::Import;
use crate::IncludeAttribute;
use crate::MacroName;
use crate::ModuleAttribute;
use crate::Name;
use crate::NameArity;
use crate::OptionalCallbacks;
use crate::PPCondition;
use crate::PPConditionId;
use crate::PPDirective;
use crate::Record;
use crate::RecordField;
use crate::RecordFieldId;
use crate::Spec;
use crate::TypeAlias;
use crate::TypeExport;

pub struct Ctx<'a> {
    db: &'a dyn DefDatabase,
    source_file: &'a ast::SourceFile,
    id_map: FormIdMap,
    map_back: FxHashMap<AstPtr<ast::Form>, FormIdx>,
    define_id_map: FxHashMap<DefineId, FormIdx>,
    data: Box<FormListData>,
    diagnostics: Vec<Diagnostic>,
    conditions: Vec<PPConditionId>,
}

impl<'a> Ctx<'a> {
    pub fn new(db: &'a dyn DefDatabase, source_file: &'a ast::SourceFile) -> Self {
        Self {
            db,
            source_file,
            id_map: FormIdMap::from_source_file(source_file),
            map_back: FxHashMap::default(),
            define_id_map: FxHashMap::default(),
            data: Box::default(),
            diagnostics: Vec::new(),
            conditions: Vec::new(),
        }
    }

    pub fn lower_forms(mut self) -> FormList {
        let forms = self
            .source_file
            .forms()
            .flat_map(|form| {
                let idx = match &form {
                    ast::Form::ModuleAttribute(module_attr) => self.lower_module_attr(module_attr),
                    ast::Form::FunDecl(function) => self.lower_function_clause(function),
                    ast::Form::PreprocessorDirective(pp) => self.lower_pp_directive(pp),
                    ast::Form::BehaviourAttribute(behaviour) => self.lower_behaviour(behaviour),
                    ast::Form::Callback(callback) => self.lower_callback(callback),
                    ast::Form::CompileOptionsAttribute(copt) => self.lower_compile(copt),
                    ast::Form::ExportAttribute(export) => self.lower_export(export),
                    ast::Form::ExportTypeAttribute(export) => self.lower_type_export(export),
                    ast::Form::FeatureAttribute(attribute) => {
                        self.lower_feature_attribute(attribute)
                    }
                    ast::Form::FileAttribute(_) => None,
                    ast::Form::ImportAttribute(import) => self.lower_import(import),
                    ast::Form::Opaque(opaque) => self.lower_opaque(opaque),
                    ast::Form::OptionalCallbacksAttribute(cbs) => {
                        self.lower_optional_callbacks(cbs)
                    }
                    ast::Form::RecordDecl(record) => self.lower_record(record),
                    ast::Form::Spec(spec) => self.lower_spec(spec),
                    ast::Form::TypeAlias(alias) => self.lower_type_alias(alias),
                    ast::Form::WildAttribute(attribute) => self.lower_attribute(attribute),
                    ast::Form::DeprecatedAttribute(deprecated_attr) => {
                        self.lower_deprecated_attr(deprecated_attr)
                    }
                }?;
                self.map_back.insert(AstPtr::new(&form), idx);
                Some(idx)
            })
            .collect();

        self.data.shrink_to_fit();
        FormList {
            _c: Count::default(),
            data: self.data,
            forms,
            diagnostics: self.diagnostics,
            map_back: self.map_back,
            define_id_map: self.define_id_map,
        }
    }

    fn lower_deprecated_attr(
        &mut self,
        deprecated_attr: &ast::DeprecatedAttribute,
    ) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        match deprecated_attr.attr() {
            None => None,
            Some(ast::DeprecatedDetails::DeprecatedFa(fa)) => {
                let fa = self.lower_deprecated_fa(&fa)?;
                let form_id = self.id_map.get_id(deprecated_attr);
                let fa_attr = DeprecatedAttribute::Fa { fa, form_id, cond };
                Some(FormIdx::DeprecatedAttribute(
                    self.data.deprecates.alloc(fa_attr),
                ))
            }
            Some(ast::DeprecatedDetails::DeprecatedFas(attrs)) => {
                let fas = attrs
                    .fa()
                    .flat_map(|fa| self.lower_deprecated_fa(&fa))
                    .collect();
                let form_id = self.id_map.get_id(deprecated_attr);
                let fas_attr = DeprecatedAttribute::Fas { fas, form_id, cond };
                Some(FormIdx::DeprecatedAttribute(
                    self.data.deprecates.alloc(fas_attr),
                ))
            }
            Some(ast::DeprecatedDetails::DeprecatedModule(module)) => {
                if module.module()?.text()? == "module" {
                    let form_id = self.id_map.get_id(deprecated_attr);
                    let module = DeprecatedAttribute::Module { form_id, cond };
                    Some(FormIdx::DeprecatedAttribute(
                        self.data.deprecates.alloc(module),
                    ))
                } else {
                    None
                }
            }
        }
    }

    fn lower_deprecated_fa(&mut self, fa: &ast::DeprecatedFa) -> Option<DeprecatedFa> {
        let name = fa.fun()?.as_name();
        let arity = match fa.arity()? {
            DeprecatedFunArity::Integer(i) => Some(u32::from(i)),
            DeprecatedFunArity::DeprecatedWildcard(_) => None, //'_'
        };
        let desc = match fa.desc() {
            None => None,
            Some(desc) => match desc.desc() {
                None => None,
                Some(Desc::MultiString(desc_str)) => {
                    lower_string_like(&desc_str).map(|desc| DeprecatedDesc::Str(SmolStr::new(desc)))
                }
                //https://www.erlang.org/doc/man/xref.html
                //next_version or eventually
                //we don't use it so far
                Some(Desc::Atom(atom)) => Some(DeprecatedDesc::Atom(atom.as_name().raw())),
            },
        };
        Some(DeprecatedFa { name, arity, desc })
    }

    fn lower_module_attr(&mut self, module_attr: &ast::ModuleAttribute) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let name = self.resolve_name(&module_attr.name()?);
        let form_id = self.id_map.get_id(module_attr);
        let res = ModuleAttribute {
            name,
            cond,
            form_id,
        };
        Some(FormIdx::ModuleAttribute(
            self.data.module_attribute.alloc(res),
        ))
    }

    fn lower_function_clause(&mut self, function: &ast::FunDecl) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let (name, param_names, is_macro) = function.clauses().find_map(|clause| match clause {
            ast::FunctionOrMacroClause::FunctionClause(clause) => {
                let name = clause.name()?;
                let args = clause.args()?;
                let name = self.resolve_name(&name);
                let param_names: Vec<ParamName> = args
                    .args()
                    .enumerate()
                    .map(
                        |(i, param)| match ast::Var::cast(param.syntax().to_owned()) {
                            Some(var) if var.as_name() != "_" => ParamName::Name(var.as_name()),
                            _ => ParamName::Default(Name::arg(i + 1)),
                        },
                    )
                    .collect();
                let arity = args.args().count().try_into().ok()?;
                let name = NameArity::new(name, arity);
                Some((name, param_names, false))
            }
            ast::FunctionOrMacroClause::MacroCallExpr(call) => {
                let name = call.name()?.as_name();
                let arity = if let Some(args) = call.args() {
                    args.args().count().try_into().ok()?
                } else {
                    0
                };
                let name = NameArity::new(name, arity);
                Some((name, vec![], true))
            }
        })?;

        let form_id = self.id_map.get_id(function);
        let res = FunctionClause {
            name,
            param_names,
            is_macro,
            cond,
            form_id,
            separator: function.separator().map(|(s, t)| (s, t.text_range())),
        };
        Some(FormIdx::FunctionClause(
            self.data.function_clauses.alloc(res),
        ))
    }

    fn lower_pp_directive(&mut self, pp: &ast::PreprocessorDirective) -> Option<FormIdx> {
        match pp {
            ast::PreprocessorDirective::PpInclude(include) => self.lower_include(include),
            ast::PreprocessorDirective::PpIncludeLib(include) => self.lower_include_lib(include),
            ast::PreprocessorDirective::PpDefine(define) => self.lower_define(define),
            ast::PreprocessorDirective::PpUndef(undef) => self.lower_undef(undef),
            ast::PreprocessorDirective::PpElif(elif) => self.lower_elif(elif),
            ast::PreprocessorDirective::PpElse(pp_else) => self.lower_else(pp_else),
            ast::PreprocessorDirective::PpEndif(endif) => self.lower_endif(endif),
            ast::PreprocessorDirective::PpIf(pp_id) => self.lower_if(pp_id),
            ast::PreprocessorDirective::PpIfdef(ifdef) => self.lower_ifdef(ifdef),
            ast::PreprocessorDirective::PpIfndef(ifndef) => self.lower_ifndef(ifndef),
        }
    }

    fn lower_include(&mut self, include: &ast::PpInclude) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let path: String = include
            .file()
            .flat_map(|detail| match detail {
                ast::IncludeDetail::String(str) => Some(String::from(str)),
                // TODO: macro expansion
                ast::IncludeDetail::MacroCallExpr(_) => None,
            })
            .collect();
        let form_id = self.id_map.get_id(include);
        let res = IncludeAttribute::Include {
            path: SmolStr::new(path),
            cond,
            form_id,
        };
        self.alloc_include(res)
    }

    fn lower_include_lib(&mut self, include: &ast::PpIncludeLib) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let path: String = include
            .file()
            .flat_map(|detail| match detail {
                ast::IncludeDetail::String(str) => Some(String::from(str)),
                // TODO: macro expansion
                ast::IncludeDetail::MacroCallExpr(_) => None,
            })
            .collect();
        let form_id = self.id_map.get_id(include);
        let res = IncludeAttribute::IncludeLib {
            path: SmolStr::new(path),
            cond,
            form_id,
        };
        self.alloc_include(res)
    }

    fn alloc_include(&mut self, res: IncludeAttribute) -> Option<FormIdx> {
        let include_idx = self.data.includes.alloc(res);
        let idx = self
            .data
            .pp_directives
            .alloc(PPDirective::Include(include_idx));
        Some(FormIdx::PPDirective(idx))
    }

    fn lower_define(&mut self, define: &ast::PpDefine) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let definition = define.lhs()?;
        let name = definition.name()?.as_name();
        let arity = definition
            .args()
            .and_then(|args| args.args().count().try_into().ok());
        let name = MacroName::new(name, arity);
        let form_id = self.id_map.get_id(define);
        let res = Define {
            name,
            cond,
            form_id,
        };
        let define_idx = self.data.defines.alloc(res);
        let idx = self
            .data
            .pp_directives
            .alloc(PPDirective::Define(define_idx));
        let form_id = FormIdx::PPDirective(idx);
        self.define_id_map.insert(define_idx, form_id);
        Some(form_id)
    }

    fn lower_undef(&mut self, undef: &ast::PpUndef) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let name = undef.name()?.as_name();
        let form_id = self.id_map.get_id(undef);
        let res = PPDirective::Undef {
            name,
            cond,
            form_id,
        };
        Some(FormIdx::PPDirective(self.data.pp_directives.alloc(res)))
    }

    fn lower_ifdef(&mut self, ifdef: &ast::PpIfdef) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let name = ifdef.name()?.as_name();
        let form_id = self.id_map.get_id(ifdef);
        let res = PPCondition::Ifdef {
            cond,
            name,
            form_id,
        };
        let id = self.data.pp_conditions.alloc(res);
        self.conditions.push(id);
        Some(FormIdx::PPCondition(id))
    }

    fn lower_ifndef(&mut self, ifndef: &ast::PpIfndef) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let name = ifndef.name()?.as_name();
        let form_id = self.id_map.get_id(ifndef);
        let res = PPCondition::Ifndef {
            cond,
            name,
            form_id,
        };
        let id = self.data.pp_conditions.alloc(res);
        self.conditions.push(id);
        Some(FormIdx::PPCondition(id))
    }

    fn lower_endif(&mut self, endif: &ast::PpEndif) -> Option<FormIdx> {
        let prev = self.conditions.pop()?;
        let form_id = self.id_map.get_id(endif);
        let res = PPCondition::Endif { prev, form_id };
        Some(FormIdx::PPCondition(self.data.pp_conditions.alloc(res)))
    }

    fn lower_if(&mut self, pp_if: &ast::PpIf) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let form_id = self.id_map.get_id(pp_if);
        let res = PPCondition::If { cond, form_id };
        let id = self.data.pp_conditions.alloc(res);
        self.conditions.push(id);
        Some(FormIdx::PPCondition(id))
    }

    fn lower_elif(&mut self, pp_elif: &ast::PpElif) -> Option<FormIdx> {
        let prev = self.conditions.pop()?;
        let form_id = self.id_map.get_id(pp_elif);
        let res = PPCondition::Elif { prev, form_id };
        let id = self.data.pp_conditions.alloc(res);
        self.conditions.push(id);
        Some(FormIdx::PPCondition(id))
    }

    fn lower_else(&mut self, pp_else: &ast::PpElse) -> Option<FormIdx> {
        let prev = self.conditions.pop()?;
        let form_id = self.id_map.get_id(pp_else);
        let res = PPCondition::Else { prev, form_id };
        let id = self.data.pp_conditions.alloc(res);
        self.conditions.push(id);
        Some(FormIdx::PPCondition(id))
    }

    fn lower_export(&mut self, export: &ast::ExportAttribute) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let entries = self.lower_fa_entries(export.funs());
        let form_id = self.id_map.get_id(export);
        let res = Export {
            entries,
            cond,
            form_id,
        };
        Some(FormIdx::Export(self.data.exports.alloc(res)))
    }

    fn lower_import(&mut self, import: &ast::ImportAttribute) -> Option<FormIdx> {
        // Import attribute with missing name is still useful - it defines local functions
        let from = import
            .module()
            .map(|name| self.resolve_name(&name))
            .unwrap_or(Name::MISSING);
        let entries = self.lower_fa_entries(import.funs());
        let cond = self.conditions.last().copied();
        let form_id = self.id_map.get_id(import);
        let res = Import {
            from,
            entries,
            cond,
            form_id,
        };
        Some(FormIdx::Import(self.data.imports.alloc(res)))
    }

    fn lower_type_export(&mut self, export: &ast::ExportTypeAttribute) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let entries = self.lower_fa_entries(export.types());
        let form_id = self.id_map.get_id(export);
        let res = TypeExport {
            entries,
            cond,
            form_id,
        };
        Some(FormIdx::TypeExport(self.data.type_exports.alloc(res)))
    }

    fn lower_fa_entries(&mut self, entries: impl Iterator<Item = ast::Fa>) -> IdxRange<FaEntry> {
        let mut entries = entries
            .enumerate()
            .filter_map(|(idx, field)| self.lower_fa_entry(field, idx as u32));

        if let Some(first) = entries.next() {
            let last = entries.last().unwrap_or(first);
            IdxRange::new_inclusive(first..=last)
        } else {
            let zero = Idx::from_raw(RawIdx::from(0));
            IdxRange::new(zero..zero)
        }
    }

    fn lower_fa_entry(&mut self, entry: ast::Fa, idx: u32) -> Option<FaEntryId> {
        let name = self.resolve_name(&entry.fun()?);
        let arity = self.resolve_arity(&entry.arity()?.value()?)?;
        let name = NameArity::new(name, arity);
        let res = FaEntry { name, idx };
        Some(self.data.fa_entries.alloc(res))
    }

    fn lower_behaviour(&mut self, behaviour: &ast::BehaviourAttribute) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let form_id = self.id_map.get_id(behaviour);
        let name = self.resolve_name(&behaviour.name()?);
        let res = Behaviour {
            name,
            cond,
            form_id,
        };
        Some(FormIdx::Behaviour(self.data.behaviours.alloc(res)))
    }

    fn lower_type_alias(&mut self, alias: &ast::TypeAlias) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let type_name = alias.name()?;
        let name = self.resolve_name(&type_name.name()?);
        let arity = type_name.args()?.args().count().try_into().ok()?;
        let name = NameArity::new(name, arity);

        let form_id = self.id_map.get_id(alias);
        let res = TypeAlias::Regular {
            name,
            cond,
            form_id,
        };
        Some(FormIdx::TypeAlias(self.data.type_aliases.alloc(res)))
    }

    fn lower_opaque(&mut self, opaque: &ast::Opaque) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let type_name = opaque.name()?;
        let name = self.resolve_name(&type_name.name()?);
        let arity = type_name.args()?.args().count().try_into().ok()?;
        let name = NameArity::new(name, arity);

        let form_id = self.id_map.get_id(opaque);
        let res = TypeAlias::Opaque {
            name,
            cond,
            form_id,
        };
        Some(FormIdx::TypeAlias(self.data.type_aliases.alloc(res)))
    }

    fn lower_optional_callbacks(
        &mut self,
        cbs: &ast::OptionalCallbacksAttribute,
    ) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let entries = self.lower_fa_entries(cbs.callbacks());
        let form_id = self.id_map.get_id(cbs);
        let res = OptionalCallbacks {
            entries,
            cond,
            form_id,
        };
        Some(FormIdx::OptionalCallbacks(
            self.data.optional_callbacks.alloc(res),
        ))
    }

    fn lower_spec(&mut self, spec: &ast::Spec) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        if spec.module().is_some() {
            return None;
        }
        let name = self.resolve_name(&spec.fun()?);
        let args = spec.sigs().find_map(|sig| sig.args())?;
        let arity = args.args().count().try_into().ok()?;
        let name = NameArity::new(name, arity);

        let form_id = self.id_map.get_id(spec);
        let res = Spec {
            name,
            cond,
            form_id,
        };
        Some(FormIdx::Spec(self.data.specs.alloc(res)))
    }

    fn lower_callback(&mut self, callback: &ast::Callback) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        if callback.module().is_some() {
            return None;
        }
        let name = self.resolve_name(&callback.fun()?);
        let args = callback.sigs().find_map(|sig| sig.args())?;
        let arity = args.args().count().try_into().ok()?;
        let name = NameArity::new(name, arity);

        let form_id = self.id_map.get_id(callback);
        let res = Callback {
            name,
            cond,
            form_id,
        };
        Some(FormIdx::Callback(self.data.callbacks.alloc(res)))
    }

    fn lower_record(&mut self, record: &ast::RecordDecl) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let name = self.resolve_name(&record.name()?);

        let mut fields = record
            .fields()
            .enumerate()
            .filter_map(|(idx, field)| self.lower_record_field(field, idx as u32));

        let fields = if let Some(first) = fields.next() {
            let last = fields.last().unwrap_or(first);
            IdxRange::new_inclusive(first..=last)
        } else {
            let zero = Idx::from_raw(RawIdx::from(0));
            IdxRange::new(zero..zero)
        };

        let form_id = self.id_map.get_id(record);
        let res = Record {
            name,
            fields,
            cond,
            form_id,
        };
        Some(FormIdx::Record(self.data.records.alloc(res)))
    }

    fn lower_record_field(&mut self, field: ast::RecordField, idx: u32) -> Option<RecordFieldId> {
        let name = self.resolve_name(&field.name()?);
        let res = RecordField { name, idx };
        Some(self.data.record_fields.alloc(res))
    }

    fn lower_compile(&mut self, copt: &ast::CompileOptionsAttribute) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let form_id = self.id_map.get_id(copt);
        let res = CompileOption { cond, form_id };
        Some(FormIdx::CompileOption(self.data.compile_options.alloc(res)))
    }

    fn lower_attribute(&mut self, attribute: &ast::WildAttribute) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let name = self.resolve_name(&attribute.name()?.name()?);
        let form_id = self.id_map.get_id(attribute);
        let res = Attribute {
            cond,
            form_id,
            name,
        };
        Some(FormIdx::Attribute(self.data.attributes.alloc(res)))
    }

    fn lower_feature_attribute(&mut self, attribute: &ast::FeatureAttribute) -> Option<FormIdx> {
        let cond = self.conditions.last().copied();
        let form_id = self.id_map.get_id(attribute);
        let res = FeatureAttribute { cond, form_id };
        Some(FormIdx::FeatureAttribute(
            self.data.feature_attributes.alloc(res),
        ))
    }

    fn resolve_name(&mut self, name: &ast::Name) -> Name {
        match name {
            ast::Name::Atom(atom) => atom.as_name(),
            ast::Name::Var(var) => {
                self.add_diagnostic(var.syntax(), DiagnosticMessage::VarNameOutsideMacro);
                Name::MISSING
            }
            ast::Name::MacroCallExpr(macro_call) => {
                let exp_ctx = MacroExpCtx::new(&self.data, self.db);
                exp_ctx
                    .expand_atom(macro_call, self.source_file)
                    .map_or(Name::MISSING, |atom| atom.as_name())
            }
        }
    }

    fn resolve_arity(&mut self, arity: &ast::ArityValue) -> Option<u32> {
        // TODO: macro resolution
        match arity {
            ast::ArityValue::Integer(int) => {
                let text = int.text();
                if text.contains('_') {
                    let str = text.replace('_', "");
                    str.parse().ok()
                } else {
                    text.parse().ok()
                }
            }
            ast::ArityValue::MacroCallExpr(_) => None,
            ast::ArityValue::Var(_) => None,
        }
    }

    fn add_diagnostic(&mut self, node: &SyntaxNode, message: DiagnosticMessage) {
        self.diagnostics.push(Diagnostic {
            location: node.text_range(),
            message,
        });
    }
}

fn lower_string_like(concat: &ast::MultiString) -> Option<String> {
    let mut buf = String::new();

    for concatable in concat.elems() {
        // TODO: macro resolution
        match concatable {
            ast::StringLike::MacroCallExpr(_) => return None,
            ast::StringLike::MacroString(_) => return None,
            ast::StringLike::String(str) => buf.push_str(&unescape::unescape_string(&str.text())?),
        }
    }

    Some(buf)
}
