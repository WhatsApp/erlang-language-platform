/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! This module performs the fourth and last step of stubs validation
//!
//! It ensures that declarations are transitively valid by propagating
//! all invalid declarations. I.e., if a type t1 depends on a type t2
//! and t2 is invalid, then t1 will be tagged as invalid.
//!
//! The algorithm works in three phases:
//! 1. Build a dependency graph where edges mean "A depends on B"
//! 2. Propagate invalidity using SCC computation (via petgraph)
//! 3. Apply results to the module stub

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_syntax::SmolStr;
use elp_types_db::StringId;
use elp_types_db::eqwalizer::Pos;
use elp_types_db::eqwalizer::form::Callback;
use elp_types_db::eqwalizer::form::FunSpec;
use elp_types_db::eqwalizer::form::OverloadedFunSpec;
use elp_types_db::eqwalizer::form::RecDecl;
use elp_types_db::eqwalizer::form::TypeDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::eqwalizer::invalid_diagnostics::InvalidRefInTypeCast;
use elp_types_db::eqwalizer::invalid_diagnostics::TransitiveInvalid;
use elp_types_db::eqwalizer::types::Type;
use elp_types_db::eqwalizer::types::Variance;
use fxhash::FxHashMap;
use petgraph::algo::tarjan_scc;
use petgraph::graph::DiGraph;
use petgraph::graph::NodeIndex;

use super::Id;
use super::RemoteId;
use super::stub::ModuleStub;
use super::stub::VStub;
use crate::db::EqwalizerDiagnosticsDatabase;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Ref {
    RidRef(RemoteId),
    RecRef(StringId, StringId),
}

impl Ref {
    fn module(&self) -> StringId {
        match self {
            Ref::RidRef(rid) => rid.module,
            Ref::RecRef(module, _) => *module,
        }
    }
}

pub struct TransitiveChecker<'d> {
    db: &'d dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: StringId,
    // Dependency graph: edge A -> B means "A depends on B"
    graph: DiGraph<Ref, ()>,
    ref_to_node: BTreeMap<Ref, NodeIndex>,
    explored: BTreeSet<Ref>,
    // Invalidity tracking
    directly_invalid: BTreeSet<NodeIndex>,
    // For each invalid ref, the set of root-cause invalid refs
    invalid_reasons: BTreeMap<Ref, BTreeSet<Ref>>,
}

impl TransitiveChecker<'_> {
    pub fn new(
        db: &dyn EqwalizerDiagnosticsDatabase,
        project_id: ProjectId,
        module: StringId,
    ) -> TransitiveChecker<'_> {
        TransitiveChecker {
            db,
            project_id,
            module,
            graph: DiGraph::new(),
            ref_to_node: BTreeMap::new(),
            explored: BTreeSet::new(),
            directly_invalid: BTreeSet::new(),
            invalid_reasons: BTreeMap::new(),
        }
    }

    fn get_or_create_node(&mut self, rref: Ref) -> NodeIndex {
        let graph = &mut self.graph;
        *self
            .ref_to_node
            .entry(rref.clone())
            .or_insert_with(|| graph.add_node(rref))
    }

    /// Collect all type/record references from a type tree.
    fn collect_refs_from_type(module: StringId, ty: &Type) -> Vec<Ref> {
        let mut refs = Vec::new();
        let _ = ty.traverse::<()>(&mut |sub_ty| {
            match sub_ty {
                Type::RemoteType(rt) => refs.push(Ref::RidRef(rt.id.clone())),
                Type::RecordType(rt) => refs.push(Ref::RecRef(module, rt.name)),
                Type::RefinedRecordType(rt) => refs.push(Ref::RecRef(module, rt.rec_type.name)),
                _ => {}
            }
            Ok(())
        });
        refs
    }

    /// Resolve the dependencies of a single ref by looking up its definition.
    /// Returns `Some(deps)` if the ref resolves, `None` if it should be marked invalid.
    fn resolve_deps(&self, rref: &Ref) -> Option<Vec<Ref>> {
        let v_stub = self
            .db
            .contractive_stub(self.project_id, ModuleName::new(rref.module().as_str()))
            .ok()?;
        match rref {
            Ref::RidRef(rid) => {
                let id = Id {
                    name: rid.name,
                    arity: rid.arity,
                };
                let tdecl = v_stub.get_type(&id)?;
                Some(Self::collect_refs_from_type(rid.module, &tdecl.body))
            }
            Ref::RecRef(module, rec_name) => {
                let rdecl = v_stub.get_record(*rec_name)?;
                Some(
                    rdecl
                        .fields
                        .iter()
                        .flat_map(|field| Self::collect_refs_from_type(*module, &field.tp))
                        .collect(),
                )
            }
        }
    }

    /// Explore a ref and all its transitive dependencies, building the
    /// dependency graph. Uses an iterative worklist to avoid stack overflow.
    fn explore_ref(&mut self, initial: Ref) {
        let mut worklist = vec![initial];
        while let Some(rref) = worklist.pop() {
            if !self.explored.insert(rref.clone()) {
                continue;
            }
            let source_node = self.get_or_create_node(rref.clone());
            match self.resolve_deps(&rref) {
                Some(deps) => {
                    for dep in deps {
                        let dep_node = self.get_or_create_node(dep.clone());
                        self.graph.add_edge(source_node, dep_node, ());
                        worklist.push(dep);
                    }
                }
                None => {
                    self.directly_invalid.insert(source_node);
                }
            }
        }
    }

    /// Explore all refs collected from the given fun types.
    fn explore_fun_types<'a>(
        &mut self,
        tys: impl Iterator<Item = &'a elp_types_db::eqwalizer::types::FunType>,
    ) {
        for ty in tys {
            for rref in Self::collect_refs_from_type(self.module, &Type::FunType(ty.clone())) {
                self.explore_ref(rref);
            }
        }
    }

    /// Build the dependency graph starting from all entities in the module.
    fn build_graph(&mut self, v_stub: &VStub) {
        for decl in v_stub.types() {
            let rref = Ref::RidRef(RemoteId {
                module: self.module,
                name: decl.id.name,
                arity: decl.id.arity,
            });
            self.explore_ref(rref);
        }

        for decl in v_stub.records() {
            self.explore_ref(Ref::RecRef(self.module, decl.name));
        }

        self.explore_fun_types(v_stub.specs().map(|s| &s.ty));
        self.explore_fun_types(v_stub.overloaded_specs().flat_map(|s| s.tys.iter()));
        self.explore_fun_types(v_stub.callbacks().flat_map(|cb| cb.tys.iter()));
    }

    /// Propagate invalidity through the dependency graph using SCCs.
    /// After this method, `invalid_reasons` maps each transitively-invalid
    /// ref to the set of root-cause (directly) invalid refs it depends on.
    fn propagate_invalidity(&mut self, sccs: &[Vec<NodeIndex>]) {
        let node_to_scc: FxHashMap<NodeIndex, usize> = sccs
            .iter()
            .enumerate()
            .flat_map(|(i, scc)| scc.iter().map(move |&node| (node, i)))
            .collect();

        let mut scc_root_causes: Vec<BTreeSet<Ref>> = vec![BTreeSet::new(); sccs.len()];

        for (scc_idx, scc) in sccs.iter().enumerate() {
            // Collect root causes from directly invalid nodes in this SCC
            scc_root_causes[scc_idx].extend(
                scc.iter()
                    .filter(|node| self.directly_invalid.contains(node))
                    .map(|&node| self.graph[node].clone()),
            );

            // Collect root causes from dependency SCCs (outgoing edges)
            let dep_causes: BTreeSet<Ref> = scc
                .iter()
                .flat_map(|&node| self.graph.neighbors(node))
                .map(|n| node_to_scc[&n])
                .filter(|&dep_scc| dep_scc != scc_idx)
                .collect::<BTreeSet<_>>()
                .into_iter()
                .flat_map(|dep_scc| scc_root_causes[dep_scc].iter().cloned())
                .collect();
            scc_root_causes[scc_idx].extend(dep_causes);
        }

        for (scc_idx, scc) in sccs.iter().enumerate() {
            if !scc_root_causes[scc_idx].is_empty() {
                for &node in scc {
                    self.invalid_reasons
                        .insert(self.graph[node].clone(), scc_root_causes[scc_idx].clone());
                }
            }
        }
    }

    fn collect_root_causes_from_type(&self, ty: &Type) -> BTreeSet<Ref> {
        Self::collect_refs_from_type(self.module, ty)
            .iter()
            .filter_map(|rref| self.invalid_reasons.get(rref))
            .flat_map(|reasons| reasons.iter().cloned())
            .collect()
    }

    fn show(&self, rref: &Ref) -> SmolStr {
        match rref {
            Ref::RidRef(rid) if rid.module == self.module => Id {
                name: rid.name,
                arity: rid.arity,
            }
            .to_string()
            .into(),
            Ref::RidRef(rid) => rid.to_string().into(),
            Ref::RecRef(_, name) => format!("#{name}{{}}").into(),
        }
    }

    fn push_transitive_invalid(
        &self,
        stub: &mut ModuleStub,
        root_causes: &BTreeSet<Ref>,
        pos: Pos,
        name: SmolStr,
    ) {
        let references = root_causes.iter().map(|r| self.show(r)).collect();
        stub.invalids
            .push(Invalid::TransitiveInvalid(TransitiveInvalid::new(
                pos, name, references,
            )));
    }

    fn check_type_decl(&self, stub: &mut ModuleStub, t: &TypeDecl) {
        let rref = Ref::RidRef(RemoteId {
            module: self.module,
            name: t.id.name,
            arity: t.id.arity,
        });
        if let Some(causes) = self.invalid_reasons.get(&rref) {
            stub.types.remove(&t.id);
            self.push_transitive_invalid(stub, causes, t.pos.clone(), t.id.to_string().into());
        }
    }

    fn check_record_decl(&self, stub: &mut ModuleStub, t: &RecDecl) {
        let rref = Ref::RecRef(self.module, t.name);
        if let Some(causes) = self.invalid_reasons.get(&rref) {
            // Replacing all the fields with dynamic type
            if let Some(rec_decl) = stub.records.get_mut(&t.name) {
                Arc::make_mut(rec_decl)
                    .fields
                    .iter_mut()
                    .for_each(|field| field.tp = Type::DynamicType);
            }
            self.push_transitive_invalid(stub, causes, t.pos.clone(), t.name.into());
        }
    }

    fn check_spec(&self, stub: &mut ModuleStub, spec: &FunSpec) {
        let causes = self.collect_root_causes_from_type(&Type::FunType(spec.ty.to_owned()));
        if !causes.is_empty() {
            stub.specs.remove(&spec.id);
            self.push_transitive_invalid(
                stub,
                &causes,
                spec.pos.clone(),
                spec.id.to_string().into(),
            );
        }
    }

    fn check_overloaded_spec(&self, stub: &mut ModuleStub, spec: &OverloadedFunSpec) {
        let causes: BTreeSet<Ref> = spec
            .tys
            .iter()
            .flat_map(|ty| self.collect_root_causes_from_type(&Type::FunType(ty.to_owned())))
            .collect();
        if !causes.is_empty() {
            stub.overloaded_specs.remove(&spec.id);
            self.push_transitive_invalid(
                stub,
                &causes,
                spec.pos.clone(),
                spec.id.to_string().into(),
            );
        }
    }

    fn check_callback(&self, stub: &mut ModuleStub, cb: &Callback, callbacks: &mut Vec<Callback>) {
        let causes: BTreeSet<Ref> = cb
            .tys
            .iter()
            .flat_map(|ty| self.collect_root_causes_from_type(&Type::FunType(ty.to_owned())))
            .collect();
        if !causes.is_empty() {
            self.push_transitive_invalid(stub, &causes, cb.pos.clone(), cb.id.to_string().into());
            callbacks.push(Callback {
                pos: cb.pos.clone(),
                id: cb.id.clone(),
                tys: Vec::new(),
            });
        } else {
            callbacks.push(cb.clone());
        }
    }

    /// Compute param variances for all valid, parameterized types in the
    /// dependency graph, processing SCCs in topological order (postorder).
    /// Within each SCC, uses fixed-point iteration starting from all-Constant.
    fn compute_variances(&self, sccs: &[Vec<NodeIndex>]) -> BTreeMap<RemoteId, Vec<Variance>> {
        let mut computed: BTreeMap<RemoteId, Vec<Variance>> = BTreeMap::new();

        for scc in sccs {
            // Collect valid, parameterized type refs in this SCC.
            // We keep the Arc<VStub> alive so we can borrow params/body
            // without cloning.
            let type_refs: Vec<_> = scc
                .iter()
                .filter_map(|&node| {
                    let rref = &self.graph[node];
                    if self.invalid_reasons.contains_key(rref) {
                        return None;
                    }
                    if let Ref::RidRef(rid) = rref
                        && rid.arity > 0
                    {
                        let v_stub = self
                            .db
                            .contractive_stub(self.project_id, ModuleName::new(rid.module.as_str()))
                            .ok()?;
                        let id = Id {
                            name: rid.name,
                            arity: rid.arity,
                        };
                        Some((rid.clone(), id, v_stub))
                    } else {
                        None
                    }
                })
                .collect();

            if type_refs.is_empty() {
                continue;
            }

            // Initialize to Constant
            for (rid, id, v_stub) in &type_refs {
                let tdecl = v_stub
                    .get_type(id)
                    .expect("types were validated, so it should exist");
                computed.insert(rid.clone(), vec![Variance::Constant; tdecl.params.len()]);
            }

            // Fixed-point iteration within SCC
            loop {
                let mut changed = false;
                for (rid, id, v_stub) in &type_refs {
                    let tdecl = v_stub
                        .get_type(id)
                        .expect("types were validated, so it should exist");
                    let new_variances: Vec<Variance> = tdecl
                        .params
                        .iter()
                        .map(|param| {
                            super::variance::variance_of(&tdecl.body, param.n, true, &computed)
                        })
                        .collect();
                    if computed[rid] != new_variances {
                        computed.insert(rid.clone(), new_variances);
                        changed = true;
                    }
                }
                if !changed {
                    break;
                }
            }
        }

        computed
    }

    pub fn check(&mut self, v_stub: &VStub) -> ModuleStub {
        let mut stub_result = v_stub.into_normalized_stub();

        self.build_graph(v_stub);
        // tarjan_scc returns SCCs in postorder (reverse topological order):
        // dependencies come before dependents.
        let sccs = tarjan_scc(&self.graph);
        self.propagate_invalidity(&sccs);

        for decl in v_stub.types() {
            self.check_type_decl(&mut stub_result, decl)
        }
        for decl in v_stub.records() {
            self.check_record_decl(&mut stub_result, decl)
        }
        for spec in v_stub.specs() {
            self.check_spec(&mut stub_result, spec)
        }
        for spec in v_stub.overloaded_specs() {
            self.check_overloaded_spec(&mut stub_result, spec)
        }
        let mut callbacks = Vec::new();
        for cb in v_stub.callbacks() {
            self.check_callback(&mut stub_result, cb, &mut callbacks)
        }
        stub_result.callbacks = Arc::new(callbacks);

        // Compute param variances using the dependency graph's SCCs.
        // The graph covers all reachable types (including cross-module),
        // so cross-module recursive types are handled correctly.
        let variances = self.compute_variances(&sccs);
        for (rid, vars) in variances {
            if rid.module == self.module {
                let id = Id {
                    name: rid.name,
                    arity: rid.arity,
                };
                if stub_result.types.contains_key(&id) {
                    stub_result.variances.insert(id, vars);
                }
            }
        }

        stub_result
    }

    pub fn check_type(&mut self, pos: Pos, ty: Type) -> Result<Type, Invalid> {
        for rref in Self::collect_refs_from_type(self.module, &ty) {
            self.explore_ref(rref);
        }
        let sccs = tarjan_scc(&self.graph);
        self.propagate_invalidity(&sccs);

        let root_causes = self.collect_root_causes_from_type(&ty);
        if !root_causes.is_empty() {
            let references = root_causes.iter().map(|r| self.show(r)).collect();
            return Err(Invalid::InvalidRefInTypeCast(InvalidRefInTypeCast::new(
                pos, references,
            )));
        }
        Ok(ty)
    }
}
