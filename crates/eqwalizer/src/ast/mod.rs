/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::io::BufRead;
use std::io::Cursor;
use std::io::Read;
use std::path::PathBuf;

use eetf;
use eetf::Term;
use elp_syntax::SmolStr;
use elp_types_db::eqwalizer::form::ExternalForm;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
pub use elp_types_db::eqwalizer::Id;
pub use elp_types_db::eqwalizer::Pos;
pub use elp_types_db::eqwalizer::RemoteId;
use elp_types_db::eqwalizer::AST;
use fxhash::FxHashSet;

pub mod auto_import;
pub mod compiler_macro;
pub mod contractivity;
pub mod convert;
pub mod convert_types;
pub mod db;
pub mod expand;
pub mod preprocess;
pub mod stub;
pub mod subst;
pub mod trans_valid;
pub mod variance_check;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    ParseError,
    DecodeError(String),
    ModuleNotFound(String),
    BEAMNotFound(PathBuf),
    InvalidBEAM,
    ConversionError(ConversionError),
    TypeConversionError(TypeConversionError),
    ContractivityError(ContractivityCheckError),
    VarianceCheckError(VarianceCheckError),
    TransitiveCheckError(TransitiveCheckError),
}

impl From<eetf::DecodeError> for Error {
    fn from(err: eetf::DecodeError) -> Self {
        let message = err.to_string();
        Error::DecodeError(message)
    }
}

impl From<ConversionError> for Error {
    fn from(err: ConversionError) -> Self {
        Error::ConversionError(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message: String = match self {
            Error::DecodeError(msg) => {
                format!("EETF decoding failed with {}", msg)
            }
            err => format!("{:?}", err),
        };
        write!(f, "eqWAlizer error:\n{}", message)
    }
}

impl std::error::Error for Error {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConversionError {
    InvalidForm,
    InvalidFile,
    InvalidDecode,
    InvalidLocation,
    InvalidID,
    InvalidVarName,
    InvalidName,
    InvalidFixme,
    InvalidClause,
    InvalidRecordField,
    InvalidAtomLit,
    InvalidIntLit,
    InvalidExpr,
    InvalidRecordUpdateField,
    InvalidMapAssoc,
    InvalidKV,
    InvalidRecordFieldExpr,
    InvalidRecordFieldName,
    InvalidBinaryElem,
    InvalidPattern,
    InvalidPatBinaryElem,
    InvalidPatRecordFieldGen,
    InvalidPatRecordFieldNamed,
    InvalidKVPattern,
    InvalidGuard,
    InvalidTest,
    InvalidRecordFieldTest,
    InvalidKVTest,
    InvalidFunSpec,
    InvalidFunConstraint,
    InvalidPropType,
    InvalidRecordRefinedField,
    InvalidType,
    InvalidForms,
    UnknownBuiltin(String, usize),
}

impl fmt::Display for ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message: String = match self {
            ConversionError::UnknownBuiltin(name, arity) => {
                format!("unknown builtin {}/{}", name, arity)
            }
            // All other cases are variants without parameters,
            // printing their name is enough info to debug
            err => format!("{:?}", err),
        };
        write!(f, "eqWAlizer AST conversion failed with {}", message)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConversionError {
    ErrorInFunType(Invalid),
    ErrorInTypeDecl(Invalid),
    UnexpectedVariable(SmolStr),
    UnexpectedEmptyMap,
    UnexpectedType,
    UnknownBuiltin(String, usize),
    UnexpectedShapeProp,
}

impl fmt::Display for TypeConversionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message: String = match self {
            TypeConversionError::UnknownBuiltin(name, arity) => {
                format!("unknown builtin {}/{}", name, arity)
            }
            err => format!("{:?}", err),
        };
        write!(f, "eqWAlizer stub expansion failed with {}", message)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContractivityCheckError {
    UnexpectedType,
    ErrorExpandingID(RemoteId, Box<Error>),
    NonEmptyForall,
}

impl fmt::Display for ContractivityCheckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message: String = match self {
            ContractivityCheckError::ErrorExpandingID(rid, err) => {
                format!("error when expanding ID {}\n{}", rid, err)
            }
            err => format!("{:?}", err),
        };
        write!(f, "eqWAlizer contractivity check failed with {}", message)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarianceCheckError {
    ErrorExpandingID(RemoteId, Box<Error>),
}

impl fmt::Display for VarianceCheckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message: String = match self {
            VarianceCheckError::ErrorExpandingID(rid, err) => {
                format!("error when expanding ID {}\n{}", rid, err)
            }
        };
        write!(f, "eqWAlizer variance check failed with {}", message)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TransitiveCheckError {
    UnexpectedOpaqueType,
}

impl fmt::Display for TransitiveCheckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message: String = format!("{:?}", self);
        write!(f, "eqWAlizer transitive check failed with {}", message)
    }
}

pub fn from_bytes(bytes: &Vec<u8>, filter_stub: bool) -> Result<AST, Error> {
    let term = eetf::Term::decode(Cursor::new(bytes))?;
    if let Term::Tuple(res) = term {
        if let [Term::Atom(ok), forms, _] = &res.elements[..] {
            if ok.name == "ok" {
                let converted_forms = convert::convert_forms(forms, false, filter_stub)?;
                return Ok(preprocess::preprocess(converted_forms));
            }
        }
    }
    Err(Error::ConversionError(ConversionError::InvalidDecode))
}

pub fn from_beam(bytes: &Vec<u8>) -> Result<AST, Error> {
    let mut cursor = Cursor::new(bytes);
    let mut buf: [u8; 4] = [0; 4];
    let mut tag: [u8; 4] = [0; 4];

    // "FOR1"
    cursor.read(&mut buf).map_err(|_| Error::InvalidBEAM)?;
    if &buf != b"FOR1" {
        return Err(Error::InvalidBEAM);
    }
    // length
    cursor.read(&mut buf).map_err(|_| Error::InvalidBEAM)?;
    // BEAM
    cursor.read(&mut buf).map_err(|_| Error::InvalidBEAM)?;
    if &buf != b"BEAM" {
        return Err(Error::InvalidBEAM);
    }
    while (bytes.len() as u64) - cursor.position() > 8 {
        cursor.read(&mut tag).map_err(|_| Error::InvalidBEAM)?;
        cursor.read(&mut buf).map_err(|_| Error::InvalidBEAM)?;
        let length = u32::from_be_bytes(buf);
        if &tag == b"Dbgi" || &tag == b"Abst" {
            let t1 = Term::decode(&mut cursor)?;
            if let Term::Tuple(terms) = t1 {
                if let Term::Tuple(terms) = &terms.elements[2] {
                    let ast = &terms.elements[0];
                    return Ok(convert::convert_forms(ast, true, true)?);
                }
            }
        } else {
            cursor.consume(((length + 3) & !3) as usize);
        }
    }
    Err(Error::InvalidBEAM)
}

pub fn type_ids(ast: &AST) -> FxHashSet<Id> {
    ast.iter()
        .filter_map(|form| match form {
            ExternalForm::ExternalTypeDecl(d) => Some(d.id.clone()),
            ExternalForm::ExternalOpaqueDecl(d) => Some(d.id.clone()),
            _ => None,
        })
        .collect()
}

pub fn exported_type_ids(ast: &AST) -> FxHashSet<Id> {
    ast.iter()
        .flat_map(|form| match form {
            ExternalForm::ExportType(tys) => tys.types.clone(),
            _ => vec![],
        })
        .collect()
}

pub fn to_bytes(ast: &Vec<&ExternalForm>) -> Vec<u8> {
    serde_json::to_vec(ast).unwrap()
}
