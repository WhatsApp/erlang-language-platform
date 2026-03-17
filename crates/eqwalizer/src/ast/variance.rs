/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Variance computation for type alias parameters.
//!
//! This module provides the core computation. The orchestration (processing
//! SCCs in topological order, fixed-point iteration) is done by
//! `TransitiveChecker::compute_variances` in `trans_valid.rs`.

use std::collections::BTreeMap;

use elp_types_db::eqwalizer::RemoteId;
use elp_types_db::eqwalizer::types::Type;
use elp_types_db::eqwalizer::types::Variance;

pub fn variance_of(
    ty: &Type,
    tv: u32,
    positive: bool,
    precomputed: &BTreeMap<RemoteId, Vec<Variance>>,
) -> Variance {
    match ty {
        Type::FreeVarType(fv) if fv.n == tv => {
            if positive {
                Variance::Covariant
            } else {
                Variance::Contravariant
            }
        }
        Type::FunType(ft) => {
            let res = variance_of(&ft.res_ty, tv, positive, precomputed);
            let args = ft
                .arg_tys
                .iter()
                .map(|arg| variance_of(arg, tv, !positive, precomputed));
            args.fold(res, Variance::combine)
        }
        Type::RemoteType(rt) if rt.id.arity == 0 => Variance::Constant,
        Type::RemoteType(rt) => {
            let pvs = precomputed
                .get(&rt.id)
                .expect("the type variances should be computed");
            combine_all(rt.arg_tys.iter().zip(pvs.iter()).map(|(arg, pv)| match pv {
                Variance::Covariant => variance_of(arg, tv, positive, precomputed),
                Variance::Contravariant => variance_of(arg, tv, !positive, precomputed),
                Variance::Invariant => {
                    if contains_var(arg, tv) {
                        Variance::Invariant
                    } else {
                        Variance::Constant
                    }
                }
                Variance::Constant => Variance::Constant,
            }))
        }
        // All other types: recurse into children with same polarity
        _ => {
            let mut result = Variance::Constant;
            let _ = ty.walk(&mut |child| {
                result = result.combine(variance_of(child, tv, positive, precomputed));
                Ok::<(), ()>(())
            });
            result
        }
    }
}

/// Check whether a type contains a reference to the given type variable.
fn contains_var(ty: &Type, tv: u32) -> bool {
    let mut found = false;
    let _ = ty.traverse::<()>(&mut |sub_ty| {
        if let Type::FreeVarType(fv) = sub_ty {
            found = found || (fv.n == tv);
        }
        Ok(())
    });
    found
}

fn combine_all(variances: impl Iterator<Item = Variance>) -> Variance {
    variances.fold(Variance::Constant, Variance::combine)
}
