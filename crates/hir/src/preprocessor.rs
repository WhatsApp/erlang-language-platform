/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Preprocessor state machine for conditional compilation.

use std::collections::BTreeSet;
use std::sync::Arc;

use elp_base_db::AppDataId;
use elp_base_db::FileId;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

use crate::DefineId;
use crate::FormIdx;
use crate::InFile;
use crate::IncludeAttributeId;
use crate::MacroName;
use crate::Name;
use crate::PPConditionId;
use crate::condition_expr::ConditionDiagnostic;
use crate::condition_expr::ConditionExpr;
use crate::db::DefDatabase;
use crate::form_list::ConditionEnvId;
use crate::form_list::PPConditionResult;
#[cfg(test)]
use crate::known;

// ============================================================================
// Macro environment for preprocessing
// ============================================================================

/// The set of macros predefined for preprocessing.
///
/// This struct holds the predefined macro names and the module name context
/// that are used when evaluating preprocessor conditions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct MacroEnvironment {
    /// Macros that are defined outside of the current file,
    /// typically via project configuration.
    pub externally_defined: BTreeSet<MacroName>,
    pub module_name: Option<Name>,
    /// When true, enables ifdef/ifndef condition evaluation (experimental).
    /// When false, all forms are treated as active (legacy behavior).
    /// Default: false (disabled - matches legacy behavior)
    pub ifdef: bool,
    /// The originating app's `AppDataId` for include resolution.
    /// Used to resolve cross-app `include_lib` directives using the
    /// originating app's dependency graph instead of the current file's.
    pub orig_app_data_id: Option<AppDataId>,
}

impl MacroEnvironment {
    /// Create a new empty macro environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a macro in this environment.
    pub fn define(&mut self, name: MacroName) {
        self.externally_defined.insert(name);
    }

    /// Set the module name for this environment.
    pub fn set_module_name(&mut self, name: Name) {
        self.module_name = Some(name);
    }

    /// Get the module name for this environment.
    pub fn module_name(&self) -> Option<&Name> {
        self.module_name.as_ref()
    }

    /// Set whether ifdef/ifndef condition evaluation is enabled.
    pub fn set_ifdef(&mut self, value: bool) {
        self.ifdef = value;
    }

    /// Create an environment with common test macros defined.
    ///
    /// This is useful for testing preprocessor behavior where
    /// certain macros are expected to be predefined.
    #[cfg(test)]
    pub fn with_test_macros() -> Self {
        let mut env = Self::new();
        env.define(MacroName::new(known::TEST, None));
        env.define(MacroName::new(known::DEBUG, None));
        env
    }
}

/// Map from condition ID to its diagnostics
pub type ConditionDiagnosticsMap = FxHashMap<PPConditionId, Vec<ConditionDiagnostic>>;

/// State during sequential file processing.
#[derive(Debug, Clone)]
pub struct PreprocessorState {
    condition_stack: Vec<ConditionFrame>,
    defined_macros: BTreeSet<MacroName>,
    define_attr_macros: FxHashMap<MacroName, InFile<DefineId>>,
    is_active: bool,
    module_name: Option<Name>,
}

#[derive(Debug, Clone)]
struct ConditionFrame {
    has_taken_branch: bool,
    parent_active: bool,
    is_unknown: bool,
}

/// Snapshot of preprocessor state at a point in time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocessorSnapshot {
    pub defined: Arc<BTreeSet<MacroName>>,
    pub define_attr_macros: Arc<FxHashMap<MacroName, InFile<DefineId>>>,
    pub is_active: bool,
    pub is_unknown: bool,
}

/// Result of preprocessor analysis for a file.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PreprocessorAnalysis {
    condition_results: FxHashMap<PPConditionId, PPConditionResult>,
    pub final_state: Option<PreprocessorSnapshot>,
    /// The macro environment used for each include directive during
    /// sequential processing. Allows callers to determine the exact
    /// macro state at each include point.
    include_envs: FxHashMap<IncludeAttributeId, Arc<MacroEnvironment>>,
    /// Snapshot of `-define` macro definitions at the point each `-if`/`-elif`
    /// condition was encountered. Keyed by condition ID so that downstream
    /// Salsa queries can resolve user-defined macros with the correct
    /// point-in-time state instead of falling back to `db.resolve_macro()`.
    condition_macro_defs: FxHashMap<PPConditionId, Arc<FxHashMap<MacroName, InFile<DefineId>>>>,
    /// Snapshot of macro definitions at each ConditionEnvId point.
    /// Body lowering uses this to resolve macros with the correct
    /// point-in-file state instead of the end-of-file final_state.
    env_macro_defs: FxHashMap<ConditionEnvId, Arc<FxHashMap<MacroName, InFile<DefineId>>>>,
}

impl PreprocessorState {
    pub fn new(env: &MacroEnvironment) -> Self {
        Self {
            condition_stack: Vec::new(),
            defined_macros: env.externally_defined.clone(),
            define_attr_macros: FxHashMap::default(),
            is_active: true,
            module_name: env.module_name.clone(),
        }
    }

    pub fn is_active(&self) -> bool {
        self.is_active
    }

    pub fn is_defined(&self, name: &MacroName) -> bool {
        self.defined_macros.contains(name)
            || crate::macro_exp::BuiltInMacro::is_built_in_name(name.name())
    }

    pub fn defined_macros(&self) -> &BTreeSet<MacroName> {
        &self.defined_macros
    }

    pub fn define_attr_macros(&self) -> &FxHashMap<MacroName, InFile<DefineId>> {
        &self.define_attr_macros
    }

    pub fn enter_ifdef(&mut self, name: &MacroName) {
        let condition_met = self.is_defined(name);
        self.push_condition(condition_met);
    }

    pub fn enter_ifndef(&mut self, name: &MacroName) {
        let condition_met = !self.is_defined(name);
        self.push_condition(condition_met);
    }

    pub fn enter_if(&mut self, expr: &ConditionExpr) {
        match expr.evaluate(&self.defined_macros) {
            Some(true) => self.push_condition(true),
            Some(false) => self.push_condition(false),
            None => self.push_unknown_condition(),
        }
    }

    pub fn enter_else(&mut self) {
        if let Some(frame) = self.condition_stack.last_mut() {
            if frame.is_unknown {
                // If the chain is unknown, else is also unknown
                self.is_active = false;
                // Keep the frame marked as unknown
            } else {
                let should_take = frame.parent_active && !frame.has_taken_branch;
                self.is_active = should_take;
                if should_take {
                    frame.has_taken_branch = true;
                }
            }
        }
    }

    pub fn enter_elif(&mut self, expr: &ConditionExpr) {
        // Check if the current frame is already unknown
        let is_chain_unknown = self
            .condition_stack
            .last()
            .map(|f| f.is_unknown)
            .unwrap_or(false);

        // Check if we should evaluate this branch based on current stack state
        let should_evaluate = self
            .condition_stack
            .last()
            .map(|f| f.parent_active && !f.has_taken_branch && !f.is_unknown)
            .unwrap_or(false);

        if is_chain_unknown {
            // If the chain is already unknown, this elif is also unknown
            self.is_active = false;
            // Keep the frame marked as unknown
        } else if should_evaluate {
            match expr.evaluate(&self.defined_macros) {
                Some(true) => {
                    self.is_active = true;
                    if let Some(frame) = self.condition_stack.last_mut() {
                        frame.has_taken_branch = true;
                    }
                }
                Some(false) => {
                    self.is_active = false;
                }
                None => {
                    // Condition couldn't be evaluated - mark as unknown
                    self.is_active = false;
                    if let Some(frame) = self.condition_stack.last_mut() {
                        frame.is_unknown = true;
                    }
                }
            }
        } else {
            self.is_active = false;
        }
    }

    pub fn exit_condition(&mut self) {
        if let Some(frame) = self.condition_stack.pop() {
            self.is_active = frame.parent_active;
        }
    }

    pub fn define_macro(&mut self, name: MacroName, definition: Option<InFile<DefineId>>) {
        self.defined_macros.insert(name.clone());
        if let Some(def) = definition {
            self.define_attr_macros.insert(name, def);
        }
    }

    pub fn undefine_macro(&mut self, name: &MacroName) {
        // Erlang's -undef(NAME) removes ALL arity variants, not just the
        // exact MacroName. Remove all entries whose name matches.
        let undef_name = name.name();
        self.defined_macros.retain(|k| k.name() != undef_name);
        self.define_attr_macros
            .retain(|k, _| k.name() != undef_name);
    }

    pub fn module_name(&self) -> Option<&Name> {
        self.module_name.as_ref()
    }

    pub fn snapshot(&self) -> PreprocessorSnapshot {
        PreprocessorSnapshot {
            defined: Arc::new(self.defined_macros.clone()),
            define_attr_macros: Arc::new(self.define_attr_macros.clone()),
            is_active: self.is_active,
            is_unknown: self.is_current_unknown(),
        }
    }

    pub fn merge_from_include(
        &mut self,
        other_macros: &BTreeSet<MacroName>,
        other_definitions: &FxHashMap<MacroName, InFile<DefineId>>,
    ) {
        self.defined_macros.extend(other_macros.iter().cloned());
        self.define_attr_macros
            .extend(other_definitions.iter().map(|(k, v)| (k.clone(), *v)));
    }

    fn push_condition(&mut self, condition_met: bool) {
        let parent_active = self.is_active;
        let should_take = parent_active && condition_met;

        self.condition_stack.push(ConditionFrame {
            has_taken_branch: should_take,
            parent_active,
            is_unknown: false,
        });

        self.is_active = should_take;
    }

    fn push_unknown_condition(&mut self) {
        let parent_active = self.is_active;

        self.condition_stack.push(ConditionFrame {
            has_taken_branch: false,
            parent_active,
            is_unknown: true,
        });

        // Unknown conditions are treated as inactive for form activity purposes,
        // but we track that they're unknown so we can report the correct result
        self.is_active = false;
    }

    /// Check if the current condition frame (if any) has an unknown result
    pub fn is_current_unknown(&self) -> bool {
        self.condition_stack
            .last()
            .map(|f| f.is_unknown)
            .unwrap_or(false)
    }
}

impl PreprocessorAnalysis {
    pub fn new() -> Self {
        Self::default()
    }

    /// Query the result of a specific preprocessor condition.
    /// Returns `Active` if the condition is not found (default behavior).
    pub fn is_condition_active(&self, cond_id: PPConditionId) -> PPConditionResult {
        self.condition_results
            .get(&cond_id)
            .copied()
            .unwrap_or(PPConditionResult::Active)
    }

    /// Get the macro environment that was used for a specific include.
    /// Returns `None` if the include was not active or not found.
    pub fn include_env(&self, include_id: IncludeAttributeId) -> Option<&Arc<MacroEnvironment>> {
        self.include_envs.get(&include_id)
    }

    /// Get the macro definitions snapshot for a specific `-if`/`-elif` condition.
    /// Returns `None` if the condition was not found (e.g. ifdef/ifndef/else/endif).
    pub fn condition_macro_defs(
        &self,
        cond_id: PPConditionId,
    ) -> Option<&Arc<FxHashMap<MacroName, InFile<DefineId>>>> {
        self.condition_macro_defs.get(&cond_id)
    }

    /// Get the macro definitions snapshot for a specific `ConditionEnvId`.
    /// Body lowering uses this to resolve macros with the correct point-in-file
    /// state instead of the end-of-file final_state.
    pub fn macro_defs_for_env(
        &self,
        env_id: ConditionEnvId,
    ) -> Option<&Arc<FxHashMap<MacroName, InFile<DefineId>>>> {
        self.env_macro_defs.get(&env_id)
    }
}

pub fn file_preprocessor_analysis_with_diagnostics_query(
    db: &dyn DefDatabase,
    file_id: FileId,
    env: Arc<MacroEnvironment>,
) -> (Arc<PreprocessorAnalysis>, Arc<ConditionDiagnosticsMap>) {
    let form_list = db.file_form_list(file_id);
    let mut state = PreprocessorState::new(&env);
    let mut analysis = PreprocessorAnalysis::new();
    let mut diagnostics_map = ConditionDiagnosticsMap::default();

    // Track which ConditionEnvIds we've already snapshotted macro state for.
    let mut recorded_envs: FxHashSet<ConditionEnvId> = FxHashSet::default();

    // Process each form in order, updating preprocessor state
    for &form_idx in form_list.forms() {
        // Snapshot macro state for each unique ConditionEnvId.
        // This must happen BEFORE processing the form, so directive forms
        // get the state from before their own effect.
        // Only snapshot when ifdef is enabled — these snapshots are only
        // consumed by ifdef-aware code paths.
        if env.ifdef
            && let Some(pp_ctx) = form_list.get(form_idx).pp_ctx(&form_list)
            && recorded_envs.insert(pp_ctx.env)
        {
            analysis
                .env_macro_defs
                .insert(pp_ctx.env, Arc::new(state.define_attr_macros.clone()));
        }

        // Process the form based on its type
        match form_idx {
            FormIdx::PPCondition(cond_id) => {
                // When ifdef is disabled, skip all condition processing.
                // state.is_active() stays true, so all directives
                // (defines, undefs, includes) are processed unconditionally.
                // condition_results remains empty — is_condition_active()
                // defaults to Active for missing entries.
                if env.ifdef {
                    // Snapshot macro defs for -if/-elif conditions so that
                    // downstream Salsa queries can resolve user macros with
                    // the correct point-in-time state.
                    // Only consumed by condition_body_with_source_query when
                    // ifdef is active.
                    if matches!(
                        &form_list[cond_id],
                        crate::form_list::PPCondition::If { .. }
                            | crate::form_list::PPCondition::Elif { .. }
                    ) {
                        analysis
                            .condition_macro_defs
                            .insert(cond_id, Arc::new(state.define_attr_macros.clone()));
                    }

                    let diagnostics =
                        process_pp_condition(db, file_id, &form_list, cond_id, &mut state);
                    // Record diagnostics if any
                    if !diagnostics.is_empty() {
                        diagnostics_map.insert(cond_id, diagnostics);
                    }
                    // Record the condition result after processing
                    let result = if state.is_current_unknown() {
                        PPConditionResult::Unknown
                    } else if state.is_active() {
                        PPConditionResult::Active
                    } else {
                        PPConditionResult::Inactive
                    };
                    analysis.condition_results.insert(cond_id, result);
                }
            }
            FormIdx::PPDirective(directive_id) => {
                process_pp_directive(
                    db,
                    file_id,
                    &form_list,
                    directive_id,
                    &mut state,
                    &env,
                    &mut analysis.include_envs,
                );
            }
            _ => {
                // Other forms don't affect preprocessor state
            }
        }
    }

    analysis.final_state = Some(state.snapshot());
    (Arc::new(analysis), Arc::new(diagnostics_map))
}

/// Process a preprocessor condition (-ifdef, -ifndef, -if, -elif, -else, -endif).
/// Returns any diagnostics generated during condition lowering.
pub(crate) fn process_pp_condition(
    db: &dyn DefDatabase,
    file_id: FileId,
    form_list: &crate::form_list::FormList,
    cond_id: crate::PPConditionId,
    state: &mut PreprocessorState,
) -> Vec<ConditionDiagnostic> {
    use crate::form_list::PPCondition;

    match &form_list[cond_id] {
        PPCondition::Ifdef { name, .. } => {
            let macro_name = MacroName::new(name.clone(), None);
            state.enter_ifdef(&macro_name);
            vec![]
        }
        PPCondition::Ifndef { name, .. } => {
            let macro_name = MacroName::new(name.clone(), None);
            state.enter_ifndef(&macro_name);
            vec![]
        }
        PPCondition::If { form_id, .. } => {
            // Lower the condition expression and evaluate it.
            // Pass the preprocessor's accumulated macro definitions so that
            // macro resolution during lowering uses local state instead of
            // calling db.resolve_macro(), breaking the Salsa cycle.
            let source = db.parse(file_id).tree();
            let pp_if = form_id.get(&source);
            if let Some(expr_ast) = pp_if.cond() {
                let lower_result = crate::condition_expr::lower_condition_expr(
                    db,
                    file_id,
                    cond_id,
                    &expr_ast,
                    state.module_name().cloned(),
                    Some(&state.define_attr_macros),
                );
                state.enter_if(&lower_result.expr);
                lower_result.diagnostics
            } else {
                // No condition expression, treat as invalid (push inactive frame)
                state.enter_if(&crate::condition_expr::ConditionExpr::Invalid);
                vec![]
            }
        }
        PPCondition::Elif { form_id, .. } => {
            let source = db.parse(file_id).tree();
            let pp_elif = form_id.get(&source);
            if let Some(expr_ast) = pp_elif.cond() {
                let lower_result = crate::condition_expr::lower_condition_expr(
                    db,
                    file_id,
                    cond_id,
                    &expr_ast,
                    state.module_name().cloned(),
                    Some(&state.define_attr_macros),
                );
                state.enter_elif(&lower_result.expr);
                lower_result.diagnostics
            } else {
                state.enter_elif(&crate::condition_expr::ConditionExpr::Invalid);
                vec![]
            }
        }
        PPCondition::Else { .. } => {
            state.enter_else();
            vec![]
        }
        PPCondition::Endif { .. } => {
            state.exit_condition();
            vec![]
        }
    }
}

/// Process a preprocessor directive (-define, -undef, -include, -include_lib).
fn process_pp_directive(
    db: &dyn DefDatabase,
    file_id: FileId,
    form_list: &crate::form_list::FormList,
    directive_id: crate::PPDirectiveId,
    state: &mut PreprocessorState,
    env: &MacroEnvironment,
    include_envs: &mut FxHashMap<IncludeAttributeId, Arc<MacroEnvironment>>,
) {
    use crate::form_list::PPDirective;

    // Only process directives when in active code
    if !state.is_active() {
        return;
    }

    match &form_list[directive_id] {
        PPDirective::Define(define_id) => {
            let define = &form_list[*define_id];
            let definition = InFile::new(file_id, *define_id);
            state.define_macro(define.name.clone(), Some(definition));
        }
        PPDirective::Undef { name, .. } => {
            let macro_name = MacroName::new(name.clone(), None);
            state.undefine_macro(&macro_name);
        }
        PPDirective::Include(include_id) => {
            // Resolve the include and process it recursively
            if let Some(included_file_id) =
                db.resolve_include(env.orig_app_data_id, InFile::new(file_id, *include_id))
            {
                // Guard against cycles
                if included_file_id != file_id {
                    // Create an environment for the included file that inherits
                    // the current module name context and originating app identity
                    let mut include_env = MacroEnvironment::new();
                    include_env.externally_defined = state.defined_macros().clone();
                    include_env.orig_app_data_id = env.orig_app_data_id;
                    include_env.ifdef = env.ifdef;
                    if let Some(module_name) = env.module_name() {
                        include_env.set_module_name(module_name.clone());
                    } else if let Some(module_name) = state.module_name() {
                        include_env.set_module_name(module_name.clone());
                    }

                    let include_env = Arc::new(include_env);
                    // Record the environment used for this include
                    include_envs.insert(*include_id, Arc::clone(&include_env));

                    // Recursively analyze the included file
                    let include_analysis =
                        db.file_preprocessor_analysis(included_file_id, include_env);

                    // Merge the final state from the included file
                    if let Some(ref final_state) = include_analysis.final_state {
                        state.merge_from_include(
                            &final_state.defined,
                            &final_state.define_attr_macros,
                        );
                    }
                }
            }
        }
    }
}

pub(crate) fn recover_cycle_with_diagnostics(
    _db: &dyn DefDatabase,
    _cycle: &[String],
    _file_id: &FileId,
    _env: &Arc<MacroEnvironment>,
) -> (Arc<PreprocessorAnalysis>, Arc<ConditionDiagnosticsMap>) {
    // On cycle, return empty analysis and diagnostics
    (
        Arc::new(PreprocessorAnalysis::new()),
        Arc::new(ConditionDiagnosticsMap::default()),
    )
}

pub(crate) fn recover_cycle(
    _db: &dyn DefDatabase,
    _cycle: &[String],
    _file_id: &FileId,
    _env: &Arc<MacroEnvironment>,
) -> Arc<PreprocessorAnalysis> {
    // On cycle, return empty analysis
    Arc::new(PreprocessorAnalysis::new())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use elp_base_db::SourceDatabase;
    use elp_base_db::fixture::WithFixture;

    use crate::FormIdx;
    use crate::MacroName;
    use crate::Name;
    use crate::PPConditionResult;
    use crate::condition_expr::ConditionExpr;
    use crate::db::DefDatabase;
    use crate::preprocessor::MacroEnvironment;
    use crate::preprocessor::PreprocessorState;
    use crate::test_db::TestDB;

    fn name(s: &str) -> Name {
        Name::from_erlang_service(s)
    }

    fn macro_name(s: &str) -> MacroName {
        MacroName::new(name(s), None)
    }

    // ========================================================================
    // MacroEnvironment tests
    // ========================================================================

    #[test]
    fn test_macro_environment_new() {
        let env = MacroEnvironment::new();
        assert!(env.externally_defined.is_empty());
        assert!(env.module_name.is_none());
    }

    #[test]
    fn test_macro_environment_define() {
        let mut env = MacroEnvironment::new();
        let foo = macro_name("FOO");
        let bar = macro_name("BAR");

        env.define(foo.clone());
        assert!(env.externally_defined.contains(&foo));
        assert!(!env.externally_defined.contains(&bar));

        env.define(bar.clone());
        assert!(env.externally_defined.contains(&foo));
        assert!(env.externally_defined.contains(&bar));
    }

    #[test]
    fn test_macro_environment_module_name() {
        let mut env = MacroEnvironment::new();
        assert!(env.module_name().is_none());

        env.set_module_name(name("my_module"));
        assert_eq!(env.module_name(), Some(&name("my_module")));
    }

    // ========================================================================
    // PreprocessorState tests
    // ========================================================================

    #[test]
    fn test_preprocessor_state_new() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("TEST"));
        env.set_module_name(name("my_module"));

        let state = PreprocessorState::new(&env);

        assert!(state.is_active());
        assert!(state.is_defined(&macro_name("TEST")));
        assert_eq!(state.module_name(), Some(&name("my_module")));
    }

    #[test]
    fn test_preprocessor_state_ifdef_defined() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("TEST"));
        let mut state = PreprocessorState::new(&env);

        state.enter_ifdef(&macro_name("TEST"));
        // Should be active because TEST is defined
        assert!(state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_ifdef_undefined() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        state.enter_ifdef(&macro_name("TEST"));
        // Should be inactive because TEST is not defined
        assert!(!state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_ifndef_defined() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("TEST"));
        let mut state = PreprocessorState::new(&env);

        state.enter_ifndef(&macro_name("TEST"));
        // Should be inactive because TEST IS defined
        assert!(!state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_ifndef_undefined() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        state.enter_ifndef(&macro_name("TEST"));
        // Should be active because TEST is not defined
        assert!(state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_if_true() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        let expr = ConditionExpr::LiteralBool(true);
        state.enter_if(&expr);
        assert!(state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_if_false() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        let expr = ConditionExpr::LiteralBool(false);
        state.enter_if(&expr);
        assert!(!state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_if_else() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // if false -> inactive
        let expr = ConditionExpr::LiteralBool(false);
        state.enter_if(&expr);
        assert!(!state.is_active());

        // else -> should become active
        state.enter_else();
        assert!(state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_if_else_when_if_was_true() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // if true -> active
        let expr = ConditionExpr::LiteralBool(true);
        state.enter_if(&expr);
        assert!(state.is_active());

        // else -> should become inactive since if branch was taken
        state.enter_else();
        assert!(!state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_elif() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // if false
        state.enter_if(&ConditionExpr::LiteralBool(false));
        assert!(!state.is_active());

        // elif true -> should become active
        state.enter_elif(&ConditionExpr::LiteralBool(true));
        assert!(state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_elif_not_taken_when_if_was_true() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // if true
        state.enter_if(&ConditionExpr::LiteralBool(true));
        assert!(state.is_active());

        // elif true -> should be inactive because if was already taken
        state.enter_elif(&ConditionExpr::LiteralBool(true));
        assert!(!state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_nested_conditions() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("OUTER"));
        let mut state = PreprocessorState::new(&env);

        // ifdef OUTER (defined) -> active
        state.enter_ifdef(&macro_name("OUTER"));
        assert!(state.is_active());

        // ifdef INNER (not defined) -> inactive
        state.enter_ifdef(&macro_name("INNER"));
        assert!(!state.is_active());

        // exit inner
        state.exit_condition();
        assert!(state.is_active());

        // exit outer
        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_nested_conditions_parent_inactive() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // ifdef OUTER (not defined) -> inactive
        state.enter_ifdef(&macro_name("OUTER"));
        assert!(!state.is_active());

        // ifdef INNER -> should remain inactive regardless
        // (even if INNER were defined, parent is inactive)
        state.enter_ifdef(&macro_name("INNER"));
        assert!(!state.is_active());

        // exit inner
        state.exit_condition();
        assert!(!state.is_active());

        // exit outer
        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_define_macro() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        assert!(!state.is_defined(&macro_name("NEW_MACRO")));

        state.define_macro(macro_name("NEW_MACRO"), None);

        assert!(state.is_defined(&macro_name("NEW_MACRO")));
    }

    #[test]
    fn test_preprocessor_state_undefine_macro() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("EXISTING"));
        let mut state = PreprocessorState::new(&env);

        assert!(state.is_defined(&macro_name("EXISTING")));

        state.undefine_macro(&macro_name("EXISTING"));

        assert!(!state.is_defined(&macro_name("EXISTING")));
    }

    #[test]
    fn test_preprocessor_state_undefine_macro_all_arities() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // Define the same macro name with different arities
        let foo_none = MacroName::new(name("FOO"), None);
        let foo_0 = MacroName::new(name("FOO"), Some(0));
        let foo_2 = MacroName::new(name("FOO"), Some(2));
        state.define_macro(foo_none.clone(), None);
        state.define_macro(foo_0.clone(), None);
        state.define_macro(foo_2.clone(), None);

        assert!(state.is_defined(&foo_none));
        assert!(state.is_defined(&foo_0));
        assert!(state.is_defined(&foo_2));

        // Erlang's -undef(FOO) should remove ALL arity variants
        state.undefine_macro(&MacroName::new(name("FOO"), None));

        assert!(!state.is_defined(&foo_none));
        assert!(!state.is_defined(&foo_0));
        assert!(!state.is_defined(&foo_2));
    }

    #[test]
    fn test_preprocessor_state_snapshot() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("FOO"));
        let mut state = PreprocessorState::new(&env);

        // Take snapshot when active
        let snapshot1 = state.snapshot();
        assert!(snapshot1.is_active);
        assert!(snapshot1.defined.contains(&macro_name("FOO")));

        // Make inactive
        state.enter_ifdef(&macro_name("UNDEFINED"));
        let snapshot2 = state.snapshot();
        assert!(!snapshot2.is_active);
    }

    #[test]
    fn test_preprocessor_state_merge_from_include() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        let mut include_macros = std::collections::BTreeSet::new();
        include_macros.insert(macro_name("INCLUDED_MACRO"));

        state.merge_from_include(&include_macros, &fxhash::FxHashMap::default());

        assert!(state.is_defined(&macro_name("INCLUDED_MACRO")));
    }

    // ========================================================================
    // Integration tests with database
    // ========================================================================

    #[test]
    fn test_file_preprocessor_analysis_simple() {
        let (db, file_id) = TestDB::with_single_file(
            r#"
-module(test).
-ifdef(TEST).
foo() -> test.
-else.
foo() -> prod.
-endif.
"#,
        );

        let env = db.project_macro_environment(file_id);
        let analysis = db.file_preprocessor_analysis(file_id, env);

        // The analysis should complete without errors
        assert!(analysis.final_state.is_some());
    }

    #[test]
    fn test_file_preprocessor_analysis_with_define() {
        let (db, file_id) = TestDB::with_single_file(
            r#"
-module(test).
-define(MY_MACRO, true).
-ifdef(MY_MACRO).
foo() -> defined.
-endif.
"#,
        );

        let env = db.project_macro_environment(file_id);
        let analysis = db.file_preprocessor_analysis(file_id, env);

        // After processing -define, MY_MACRO should be defined
        if let Some(ref final_state) = analysis.final_state {
            assert!(final_state.defined.contains(&macro_name("MY_MACRO")));
        }
    }

    #[test]
    fn test_file_preprocessor_analysis_with_undef() {
        let (db, file_id) = TestDB::with_single_file(
            r#"
-module(test).
-define(TEMP, 1).
-undef(TEMP).
-ifdef(TEMP).
should_not_be_active() -> ok.
-endif.
"#,
        );

        let env = db.project_macro_environment(file_id);
        let analysis = db.file_preprocessor_analysis(file_id, env);

        // After -undef, TEMP should not be defined
        if let Some(ref final_state) = analysis.final_state {
            assert!(!final_state.defined.contains(&macro_name("TEMP")));
        }
    }

    #[test]
    fn test_project_macro_environment_module_name() {
        let (db, file_id) = TestDB::with_single_file(
            r#"
-module(my_test_module).
foo() -> ok.
"#,
        );

        let env = db.project_macro_environment(file_id);

        assert_eq!(env.module_name(), Some(&name("my_test_module")));
    }

    #[test]
    fn test_project_macro_environment_external_defines() {
        // This test verifies that external defines are picked up
        // (requires fixture with external defines configured)
        let (db, file_id) = TestDB::with_single_file(
            r#"
-module(test).
foo() -> ok.
"#,
        );

        let env = db.project_macro_environment(file_id);
        // By default, no external defines
        // The environment should at least be created successfully
        assert!(env.module_name().is_some());
    }

    // ========================================================================
    // Tests for condition expression evaluation with preprocessor state
    // ========================================================================

    #[test]
    fn test_preprocessor_state_if_with_defined() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("MY_FLAG"));
        let mut state = PreprocessorState::new(&env);

        // defined(MY_FLAG) -> true
        let expr = ConditionExpr::Defined(name("MY_FLAG"));
        state.enter_if(&expr);
        assert!(state.is_active());

        state.exit_condition();
    }

    #[test]
    fn test_preprocessor_state_if_with_not_defined() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // not defined(MY_FLAG) -> true (since MY_FLAG is not defined)
        let expr = ConditionExpr::Not(Box::new(ConditionExpr::Defined(name("MY_FLAG"))));
        state.enter_if(&expr);
        assert!(state.is_active());

        state.exit_condition();
    }

    #[test]
    fn test_preprocessor_state_if_with_andalso() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("A"));
        env.define(macro_name("B"));
        let mut state = PreprocessorState::new(&env);

        // defined(A) andalso defined(B) -> true
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::Defined(name("A"))),
            Box::new(ConditionExpr::Defined(name("B"))),
        );
        state.enter_if(&expr);
        assert!(state.is_active());

        state.exit_condition();
    }

    #[test]
    fn test_preprocessor_state_if_with_orelse() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("A"));
        let mut state = PreprocessorState::new(&env);

        // defined(A) orelse defined(B) -> true (A is defined)
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::Defined(name("A"))),
            Box::new(ConditionExpr::Defined(name("B"))),
        );
        state.enter_if(&expr);
        assert!(state.is_active());

        state.exit_condition();
    }

    #[test]
    fn test_preprocessor_include_propagates_macros() {
        // Create a fixture with an include file
        // Note: files are returned in order they appear in the fixture
        // The include_path directive specifies where to search for includes
        let fixture = r#"
//- /src/main.erl include_path:/include
-module(main).
-include("macros.hrl").
-ifdef(FROM_INCLUDE).
foo() -> included.
-endif.

//- /include/macros.hrl
-define(FROM_INCLUDE, 1).
"#;
        let (db, files, _) = TestDB::with_many_files(fixture);

        // files[0] = main.erl, files[1] = macros.hrl
        let main_file = files[0];

        let env = db.project_macro_environment(main_file);
        let analysis = db.file_preprocessor_analysis(main_file, env);

        // After including macros.hrl, FROM_INCLUDE should be defined
        if let Some(ref final_state) = analysis.final_state {
            assert!(
                final_state.defined.contains(&macro_name("FROM_INCLUDE")),
                "FROM_INCLUDE should be defined after including macros.hrl"
            );
        }
    }

    #[test]
    fn test_module_name_propagates_to_include() {
        // Test that module name from the including file propagates to the header
        // Note: files are returned in order they appear in the fixture
        // The include_path directive specifies where to search for includes
        let fixture = r#"
//- /src/main_module.erl include_path:/include
-module(main_module).
-include("header.hrl").
foo() -> ok.

//- /include/header.hrl
%% Header file - ?MODULE should resolve to main_module
"#;
        let (db, files, _) = TestDB::with_many_files(fixture);

        // files[0] = main_module.erl
        let main_file = files[0];

        let env = db.project_macro_environment(main_file);

        // The environment should have the module name set
        assert_eq!(env.module_name(), Some(&name("main_module")));
    }

    #[test]
    fn test_preprocessor_analysis_tracks_inactive_forms() {
        let (db, file_id) = TestDB::with_single_file(
            r#"
-module(test).
-ifdef(UNDEFINED_MACRO).
this_is_inactive() -> ok.
-endif.
"#,
        );

        let env = db.project_macro_environment(file_id);
        let analysis = db.file_preprocessor_analysis(file_id, env);
        let form_list = db.file_form_list(file_id);

        // Find at least one condition that is inactive
        let has_inactive_condition = form_list.forms().iter().any(|&form_idx| {
            if let FormIdx::PPCondition(cond_id) = form_idx {
                analysis.is_condition_active(cond_id) == PPConditionResult::Inactive
            } else {
                false
            }
        });

        assert!(
            has_inactive_condition,
            "Should have at least one inactive condition inside the ifdef block"
        );
    }

    #[test]
    fn test_macro_environment_with_test_macros() {
        let env = MacroEnvironment::with_test_macros();

        assert!(env.externally_defined.contains(&macro_name("TEST")));
        assert!(env.externally_defined.contains(&macro_name("DEBUG")));
    }

    #[test]
    fn test_preprocessor_state_chained_elif() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // if false
        state.enter_if(&ConditionExpr::LiteralBool(false));
        assert!(!state.is_active());

        // elif false
        state.enter_elif(&ConditionExpr::LiteralBool(false));
        assert!(!state.is_active());

        // elif true -> should take this branch
        state.enter_elif(&ConditionExpr::LiteralBool(true));
        assert!(state.is_active());

        // else -> should NOT take since elif was taken
        state.enter_else();
        assert!(!state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_invalid_condition() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // Invalid condition should evaluate to false
        state.enter_if(&ConditionExpr::Invalid);
        assert!(!state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_cycle_recovery() {
        use crate::preprocessor::recover_cycle_with_diagnostics;

        // Test that cycle recovery returns an empty analysis
        let db = TestDB::default();
        let file_id = elp_base_db::FileId::from_raw(0);
        let env = Arc::new(MacroEnvironment::new());

        let (analysis, diagnostics) = recover_cycle_with_diagnostics(&db, &[], &file_id, &env);

        // Cycle recovery should return empty/default analysis and empty diagnostics
        assert!(analysis.final_state.is_none());
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_preprocessor_chained_cross_app_include_lib_buck() {
        // Tests that orig_app_data_id propagation in the preprocessor
        // enables chained cross-app includes through the buck
        // IncludeMapping resolution path.
        //
        // Scenario:
        // - app_a depends on app_b and app_c
        // - app_b does NOT depend on app_c
        // - app_a includes app_b's header, which includes app_c's header
        // - app_c's header defines a macro used in app_a via ifdef
        //
        // App directory names intentionally differ from app names
        // (e.g., app_b lives under /b_real/) so the rebar local
        // include_path fallback cannot resolve the include_lib.
        // Only the buck IncludeMapping remote path works, which
        // requires the dep check to pass.
        //
        // Without orig_app_data_id propagation, the nested include_lib
        // from app_b's header uses file_app_data(bridge.hrl) → app_b's
        // deps (no app_c) → dep check fails → include unresolved.
        // With the fix, orig_app_data_id carries app_a's identity
        // through the recursive include → app_a's deps (includes
        // app_c) → dep check passes → include resolved.
        let fixture = r#"
//- /a_real/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b,app_c
-module(main).
-include_lib("app_b/include/bridge.hrl").
-ifdef(FROM_APP_C).
guarded() -> from_app_c.
-endif.

//- /b_real/include/bridge.hrl app:app_b buck_target:cell//app_b:lib
-include_lib("app_c/include/defs.hrl").

//- /c_real/include/defs.hrl app:app_c buck_target:cell//app_c:lib
-define(FROM_APP_C, true).
"#;
        let (db, files, _) = TestDB::with_many_files(fixture);

        let main_file = files[0];
        let env = db.project_macro_environment(main_file);
        let analysis = db.file_preprocessor_analysis(main_file, env);

        // After the chained include (app_a → app_b → app_c),
        // FROM_APP_C should be defined in the preprocessor state
        let final_state = analysis
            .final_state
            .as_ref()
            .expect("preprocessor analysis should have final state");
        assert!(
            final_state.defined.contains(&macro_name("FROM_APP_C")),
            "FROM_APP_C should be defined after chained cross-app include_lib.\n\
             This requires orig_app_data_id propagation: app_a deps include app_c,\n\
             but app_b (the intermediate header's app) does not depend on app_c."
        );
    }

    #[test]
    fn test_preprocessor_chained_cross_app_include_lib_buck_transitive_dep() {
        // Tests the reverse scenario: the intermediate app (app_b) has
        // the dep on app_c, but the originating app (app_a) does NOT
        // directly depend on app_c.
        //
        // Scenario:
        // - app_a depends on app_b only
        // - app_b depends on app_c
        // - app_a includes app_b's header, which includes app_c's header
        // - app_c's header defines a macro used in app_a via ifdef
        //
        // The dep check in resolve_remote_query must accept the include
        // if *either* the originating app or the current file's app has
        // the target as a dependency.
        let fixture = r#"
//- /a_real/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
-module(main).
-include_lib("app_b/include/bridge.hrl").
-ifdef(FROM_APP_C).
guarded() -> from_app_c.
-endif.

//- /b_real/include/bridge.hrl app:app_b buck_target:cell//app_b:lib deps:app_c
-include_lib("app_c/include/defs.hrl").

//- /c_real/include/defs.hrl app:app_c buck_target:cell//app_c:lib
-define(FROM_APP_C, true).
"#;
        let (db, files, _) = TestDB::with_many_files(fixture);

        let main_file = files[0];
        let env = db.project_macro_environment(main_file);
        let analysis = db.file_preprocessor_analysis(main_file, env);

        // After the chained include (app_a → app_b → app_c),
        // FROM_APP_C should be defined in the preprocessor state.
        // app_b deps on app_c, so the nested include_lib from app_b's
        // header should resolve using app_b's deps even though
        // app_a (the originating app) does not dep on app_c.
        let final_state = analysis
            .final_state
            .as_ref()
            .expect("preprocessor analysis should have final state");
        assert!(
            final_state.defined.contains(&macro_name("FROM_APP_C")),
            "FROM_APP_C should be defined after chained cross-app include_lib.\n\
             app_b deps on app_c, so the nested include from app_b's header should\n\
             resolve even though app_a (the originating app) does not dep on app_c."
        );
    }

    // ========================================================================
    // Tests for skipping condition processing when ifdef=false
    // ========================================================================

    #[test]
    fn test_ifdef_false_skips_condition_processing() {
        // When ifdef=false, condition_results should be empty
        // (no condition processing happens) and all defines should
        // be visible regardless of ifdef guards.
        let (mut db, file_id) = TestDB::with_single_file(
            r#"
-module(test).
-ifdef(UNDEFINED_MACRO).
-define(GUARDED_DEFINE, 1).
-endif.
-define(NORMAL_DEFINE, 2).
"#,
        );

        // Run with ifdef=false
        db.set_ifdef_enabled(false);
        let env = db.project_macro_environment(file_id);
        assert!(!env.ifdef);

        let analysis = db.file_preprocessor_analysis(file_id, env);

        // condition_results should be empty — no condition processing
        let final_state = analysis
            .final_state
            .as_ref()
            .expect("should have final state");

        // Both defines should be visible: the ifdef guard was not evaluated
        assert!(
            final_state
                .define_attr_macros
                .contains_key(&macro_name("GUARDED_DEFINE")),
            "GUARDED_DEFINE should be visible when ifdef=false (guard not evaluated)"
        );
        assert!(
            final_state
                .define_attr_macros
                .contains_key(&macro_name("NORMAL_DEFINE")),
            "NORMAL_DEFINE should always be visible"
        );
    }

    #[test]
    fn test_ifdef_false_no_condition_results() {
        // Verify that condition_results map is empty when ifdef=false
        let (mut db, file_id) = TestDB::with_single_file(
            r#"
-module(test).
-ifdef(TEST).
foo() -> test.
-else.
foo() -> prod.
-endif.
"#,
        );

        db.set_ifdef_enabled(false);
        let env = db.project_macro_environment(file_id);
        let (analysis, diagnostics) = db.file_preprocessor_analysis_with_diagnostics(file_id, env);

        // No condition results should be recorded
        let form_list = db.file_form_list(file_id);
        for &form_idx in form_list.forms() {
            if let FormIdx::PPCondition(cond_id) = form_idx {
                // is_condition_active defaults to Active for missing entries
                assert_eq!(
                    analysis.is_condition_active(cond_id),
                    PPConditionResult::Active,
                    "All conditions should default to Active when ifdef=false"
                );
            }
        }

        // No diagnostics should be generated
        assert!(
            diagnostics.is_empty(),
            "No condition diagnostics when ifdef=false"
        );
    }

    #[test]
    fn test_ifdef_false_undef_still_works() {
        // Verify that -undef still removes macros when ifdef=false
        // (directives are still processed, only conditions are skipped)
        let (mut db, file_id) = TestDB::with_single_file(
            r#"
-module(test).
-define(TEMP, 1).
-undef(TEMP).
"#,
        );

        db.set_ifdef_enabled(false);
        let env = db.project_macro_environment(file_id);
        let analysis = db.file_preprocessor_analysis(file_id, env);

        let final_state = analysis
            .final_state
            .as_ref()
            .expect("should have final state");

        assert!(
            !final_state.defined.contains(&macro_name("TEMP")),
            "TEMP should be undefined after -undef even when ifdef=false"
        );
    }

    #[test]
    fn test_ifdef_true_still_evaluates_conditions() {
        // Sanity check: ifdef=true should still evaluate conditions normally
        let (db, file_id) = TestDB::with_single_file(
            r#"
-module(test).
-ifdef(UNDEFINED_MACRO).
-define(GUARDED_DEFINE, 1).
-endif.
-define(NORMAL_DEFINE, 2).
"#,
        );

        // ifdef=true is the default for test fixtures
        let env = db.project_macro_environment(file_id);
        assert!(env.ifdef);

        let analysis = db.file_preprocessor_analysis(file_id, env);

        let final_state = analysis
            .final_state
            .as_ref()
            .expect("should have final state");

        // GUARDED_DEFINE should NOT be visible because UNDEFINED_MACRO is not defined
        assert!(
            !final_state
                .define_attr_macros
                .contains_key(&macro_name("GUARDED_DEFINE")),
            "GUARDED_DEFINE should be hidden when ifdef=true and guard is false"
        );
        assert!(
            final_state
                .define_attr_macros
                .contains_key(&macro_name("NORMAL_DEFINE")),
            "NORMAL_DEFINE should always be visible"
        );
    }
}
