/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use fxhash::FxHashMap;

use super::types::AnyArityFunType;
use super::types::BoundedDynamicType;
use super::types::DictMap;
use super::types::FunType;
use super::types::ListType;
use super::types::OpaqueType;
use super::types::OptProp;
use super::types::Prop;
use super::types::RefinedRecordType;
use super::types::RemoteType;
use super::types::ReqProp;
use super::types::ShapeMap;
use super::types::TupleType;
use super::types::Type;
use super::types::UnionType;

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
            Type::ShapeMap(m) => Type::ShapeMap(ShapeMap {
                props: m.props.into_iter().map(|p| self.apply_prop(p)).collect(),
            }),
            Type::DictMap(m) => Type::DictMap(DictMap {
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

    fn apply_prop(&self, prop: Prop) -> Prop {
        match prop {
            Prop::OptProp(p) => Prop::OptProp(OptProp {
                key: p.key,
                tp: self.apply(p.tp),
            }),
            Prop::ReqProp(p) => Prop::ReqProp(ReqProp {
                key: p.key,
                tp: self.apply(p.tp),
            }),
        }
    }

    fn subtract(&self, vars: &[u32]) -> Subst {
        let mut sub = self.sub.to_owned();
        vars.iter().for_each(|v| {
            sub.remove(v);
        });
        Subst { sub }
    }
}
