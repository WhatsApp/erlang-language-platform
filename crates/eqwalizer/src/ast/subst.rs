/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_types_db::eqwalizer::types::AnyArityFunType;
use elp_types_db::eqwalizer::types::BoundedDynamicType;
use elp_types_db::eqwalizer::types::FunType;
use elp_types_db::eqwalizer::types::ListType;
use elp_types_db::eqwalizer::types::MapType;
use elp_types_db::eqwalizer::types::OpaqueType;
use elp_types_db::eqwalizer::types::Prop;
use elp_types_db::eqwalizer::types::RefinedRecordType;
use elp_types_db::eqwalizer::types::RemoteType;
use elp_types_db::eqwalizer::types::TupleType;
use elp_types_db::eqwalizer::types::Type;
use elp_types_db::eqwalizer::types::UnionType;
use fxhash::FxHashMap;

pub struct Subst<'a> {
    pub sub: FxHashMap<u32, &'a Type>,
}

impl<'a> Subst<'a> {
    pub fn apply(&self, t: Type) -> Type {
        match t {
            Type::FunType(ft) => {
                let subst = self.subtract(&ft.forall);
                Type::FunType(FunType {
                    forall: ft.forall,
                    arg_tys: subst.apply_all(ft.arg_tys),
                    res_ty: Box::new(subst.apply(*ft.res_ty)),
                })
            }
            Type::AnyArityFunType(ft) => Type::AnyArityFunType(AnyArityFunType {
                res_ty: Box::new(self.apply(*ft.res_ty)),
            }),
            Type::TupleType(tt) => Type::TupleType(TupleType {
                arg_tys: self.apply_all(tt.arg_tys),
            }),
            Type::ListType(lt) => Type::ListType(ListType {
                t: Box::new(self.apply(*lt.t)),
            }),
            Type::UnionType(ut) => Type::UnionType(UnionType {
                tys: self.apply_all(ut.tys),
            }),
            Type::RemoteType(rt) => Type::RemoteType(RemoteType {
                id: rt.id,
                arg_tys: self.apply_all(rt.arg_tys),
            }),
            Type::OpaqueType(ot) => Type::OpaqueType(OpaqueType {
                id: ot.id,
                arg_tys: self.apply_all(ot.arg_tys),
            }),
            Type::VarType(n) => {
                if let Some(&typ) = self.sub.get(&n.n) {
                    typ.to_owned()
                } else {
                    Type::VarType(n)
                }
            }
            Type::MapType(m) => Type::MapType(MapType {
                props: m
                    .props
                    .into_iter()
                    .map(|(k, p)| {
                        (
                            k,
                            Prop {
                                req: p.req,
                                tp: self.apply(p.tp),
                            },
                        )
                    })
                    .collect(),
                k_type: Box::new(self.apply(*m.k_type)),
                v_type: Box::new(self.apply(*m.v_type)),
            }),
            Type::RefinedRecordType(rt) => Type::RefinedRecordType(RefinedRecordType {
                rec_type: rt.rec_type,
                fields: rt
                    .fields
                    .into_iter()
                    .map(|(k, v)| (k, self.apply(v)))
                    .collect(),
            }),
            Type::BoundedDynamicType(bd) => Type::BoundedDynamicType(BoundedDynamicType {
                bound: Box::new(self.apply(*bd.bound)),
            }),
            _ => t,
        }
    }

    fn apply_all(&self, ts: Vec<Type>) -> Vec<Type> {
        ts.into_iter().map(|t| self.apply(t)).collect()
    }

    fn subtract(&self, vars: &[u32]) -> Subst {
        let mut sub = self.sub.to_owned();
        vars.iter().for_each(|v| {
            sub.remove(v);
        });
        Subst { sub }
    }
}
