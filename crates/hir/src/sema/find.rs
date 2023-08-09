/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_base_db::FileId;
use elp_syntax::ast;
use elp_syntax::AstNode;

use crate::Behaviour;
use crate::Callback;
use crate::Define;
use crate::Export;
use crate::FormIdx;
use crate::Import;
use crate::InFile;
use crate::IncludeAttributeId;
use crate::ModuleAttribute;
use crate::OptionalCallbacks;
use crate::Record;
use crate::Semantic;
use crate::Spec;
use crate::TypeAlias;
use crate::TypeExport;

pub trait FindForm: Clone {
    type Form;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form>;
}

impl FindForm for ast::ModuleAttribute {
    type Form = ModuleAttribute;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast::Form::ModuleAttribute(ast.value.clone()))?;
        let form = match form_idx {
            FormIdx::ModuleAttribute(idx) => {
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::TypeName {
    type Form = TypeAlias;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let ast_form = ast::Form::cast(ast.value.syntax().parent()?)?;
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast_form)?;
        let form = match form_idx {
            FormIdx::TypeAlias(idx) => {
                if &form_list[idx].form_id().get(&source_file) == &ast_form {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::RecordDecl {
    type Form = Record;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast::Form::RecordDecl(ast.value.clone()))?;
        let form = match form_idx {
            FormIdx::Record(idx) => {
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::Callback {
    type Form = Callback;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast::Form::Callback(ast.value.clone()))?;
        let form = match form_idx {
            FormIdx::Callback(idx) => {
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::PpDefine {
    type Form = Define;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast::Form::cast(ast.value.syntax().clone())?)?;
        let form = match form_idx {
            FormIdx::PPDirective(idx) => {
                let idx = form_list[idx].as_define()?;
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::BehaviourAttribute {
    type Form = Behaviour;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast::Form::BehaviourAttribute(ast.value.clone()))?;
        let form = match form_idx {
            FormIdx::Behaviour(idx) => {
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::ImportAttribute {
    type Form = Import;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast::Form::ImportAttribute(ast.value.clone()))?;
        let form = match form_idx {
            FormIdx::Import(idx) => {
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::ExportAttribute {
    type Form = Export;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast::Form::ExportAttribute(ast.value.clone()))?;
        let form = match form_idx {
            FormIdx::Export(idx) => {
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::ExportTypeAttribute {
    type Form = TypeExport;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast::Form::ExportTypeAttribute(ast.value.clone()))?;
        let form = match form_idx {
            FormIdx::TypeExport(idx) => {
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::Spec {
    type Form = Spec;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx = form_list.find_form(&ast::Form::Spec(ast.value.clone()))?;
        let form = match form_idx {
            FormIdx::Spec(idx) => {
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::OptionalCallbacksAttribute {
    type Form = OptionalCallbacks;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let source_file = sema.parse(ast.file_id).value;
        let form_list = sema.form_list(ast.file_id);
        let form_idx =
            form_list.find_form(&ast::Form::OptionalCallbacksAttribute(ast.value.clone()))?;
        let form = match form_idx {
            FormIdx::OptionalCallbacks(idx) => {
                if &form_list[idx].form_id.get(&source_file) == ast.value {
                    Some(idx)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        Some(form_list[form].clone())
    }
}

impl FindForm for ast::PpInclude {
    type Form = IncludeAttributeId;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let form = ast::Form::cast(ast.value.syntax().clone())?;
        find_any_include(sema, ast.file_id, &form)
    }
}

impl FindForm for ast::PpIncludeLib {
    type Form = IncludeAttributeId;

    fn find(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Form> {
        let form = ast::Form::cast(ast.value.syntax().clone())?;
        find_any_include(sema, ast.file_id, &form)
    }
}

fn find_any_include(
    sema: &Semantic<'_>,
    file_id: FileId,
    ast_form: &ast::Form,
) -> Option<IncludeAttributeId> {
    let source_file = sema.parse(file_id).value;
    let form_list = sema.form_list(file_id);
    let form_idx = form_list.find_form(ast_form)?;
    let form = match form_idx {
        FormIdx::PPDirective(idx) => {
            let include_idx = form_list[idx].as_include()?;
            if &form_list[include_idx].form_id().get(&source_file) == ast_form {
                Some(include_idx)
            } else {
                None
            }
        }
        _ => None,
    }?;
    Some(form)
}
