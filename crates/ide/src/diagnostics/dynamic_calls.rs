/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::elp_base_db::DynamicCallPatternInput;
use serde::Deserialize;
use serde::Serialize;

#[derive(Deserialize, Serialize, Default, Debug, Clone)]
pub struct DynamicCallsConfig {
    #[serde(default)]
    pub patterns: Vec<String>,
}

impl DynamicCallsConfig {
    pub fn parse(&self) -> Vec<DynamicCallPatternInput> {
        self.patterns
            .iter()
            .filter_map(|s| match parse_dynamic_call_pattern(s) {
                Ok(p) => Some(p),
                Err(e) => {
                    log::warn!("Invalid dynamic call pattern '{}': {}", s, e);
                    None
                }
            })
            .collect()
    }
}

pub fn parse_dynamic_call_pattern(input: &str) -> anyhow::Result<DynamicCallPatternInput> {
    elp_ide_db::elp_base_db::parse_dynamic_call_pattern(input)
}

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::ModuleArgShape;

    use super::parse_dynamic_call_pattern;

    #[test]
    fn parse_remote_call_with_all_args() {
        let p = parse_dynamic_call_pattern("my_rpc:call(_Node, Module, Function, Args)").unwrap();
        assert_eq!(p.caller_module.as_deref(), Some("my_rpc"));
        assert_eq!(p.caller_function, "call");
        assert_eq!(p.caller_arity, 4);
        assert_eq!(p.module_arg_index, Some(1));
        assert_eq!(p.module_arg_shape, ModuleArgShape::Atom);
        assert_eq!(p.function_arg_index, Some(2));
        assert_eq!(p.args_list_index, Some(3));
        assert!(!p.direct_arity);
    }

    #[test]
    fn parse_local_call_with_direct_arity() {
        let p = parse_dynamic_call_pattern("check_exported(Module, Function, Arity)").unwrap();
        assert_eq!(p.caller_module, None);
        assert_eq!(p.caller_function, "check_exported");
        assert_eq!(p.caller_arity, 3);
        assert_eq!(p.module_arg_index, Some(0));
        assert_eq!(p.module_arg_shape, ModuleArgShape::Atom);
        assert_eq!(p.function_arg_index, Some(1));
        assert_eq!(p.args_list_index, Some(2));
        assert!(p.direct_arity);
    }

    #[test]
    fn parse_local_call_no_module() {
        let p = parse_dynamic_call_pattern("my_apply(Function, Args)").unwrap();
        assert_eq!(p.caller_module, None);
        assert_eq!(p.caller_function, "my_apply");
        assert_eq!(p.caller_arity, 2);
        assert_eq!(p.module_arg_index, None);
        // No module argument — shape default (Atom) is unobservable.
        assert_eq!(p.module_arg_shape, ModuleArgShape::Atom);
        assert_eq!(p.function_arg_index, Some(0));
        assert_eq!(p.args_list_index, Some(1));
        assert!(!p.direct_arity);
    }

    #[test]
    fn error_no_function_arg() {
        // Args present without Function → error (vice versa rule)
        let err = parse_dynamic_call_pattern("foo(_, Args)")
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("Function"),
            "expected Function error, got: {err}"
        );
    }

    #[test]
    fn error_both_args_and_arity() {
        let err = parse_dynamic_call_pattern("foo(Function, Args, Arity)")
            .unwrap_err()
            .to_string();
        assert!(err.contains("both"), "expected both error, got: {err}");
    }

    #[test]
    fn error_no_args_or_arity() {
        // Function present without Args/Arity → error
        let err = parse_dynamic_call_pattern("foo(_, Function)")
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("Args") || err.contains("Arity"),
            "expected Args/Arity error, got: {err}"
        );
    }

    #[test]
    fn error_missing_parens() {
        assert!(parse_dynamic_call_pattern("foo").is_err());
        assert!(parse_dynamic_call_pattern("foo(Bar").is_err());
    }

    #[test]
    fn error_empty_function_name() {
        assert!(parse_dynamic_call_pattern("(Function, Args)").is_err());
    }

    #[test]
    fn error_empty_module_name() {
        assert!(parse_dynamic_call_pattern(":foo(Function, Args)").is_err());
    }

    #[test]
    fn parse_with_whitespace() {
        let p =
            parse_dynamic_call_pattern("  rpc:call( _Node , Module , Function , Args )  ").unwrap();
        assert_eq!(p.caller_module.as_deref(), Some("rpc"));
        assert_eq!(p.caller_function, "call");
        assert_eq!(p.caller_arity, 4);
    }

    #[test]
    fn parse_with_underscore_args() {
        let p = parse_dynamic_call_pattern("rpc:call(_, Module, Function, Args)").unwrap();
        assert_eq!(p.caller_module.as_deref(), Some("rpc"));
        assert_eq!(p.caller_function, "call");
        assert_eq!(p.caller_arity, 4);
        assert_eq!(p.module_arg_index, Some(1));
        assert_eq!(p.module_arg_shape, ModuleArgShape::Atom);
        assert_eq!(p.function_arg_index, Some(2));
        assert_eq!(p.args_list_index, Some(3));
        assert!(!p.direct_arity);
    }

    // --- [Module] syntax tests ---

    #[test]
    fn parse_bracket_module_syntax() {
        let p = parse_dynamic_call_pattern("meck:new([Module], _)").unwrap();
        assert_eq!(p.caller_module.as_deref(), Some("meck"));
        assert_eq!(p.caller_function, "new");
        assert_eq!(p.caller_arity, 2);
        assert_eq!(p.module_arg_index, Some(0));
        assert_eq!(p.module_arg_shape, ModuleArgShape::List);
        assert_eq!(p.function_arg_index, None);
        assert_eq!(p.args_list_index, None);
    }

    #[test]
    fn parse_bracket_module_with_whitespace() {
        let p = parse_dynamic_call_pattern("meck:new([ Module ], _)").unwrap();
        assert_eq!(p.module_arg_index, Some(0));
        assert_eq!(p.module_arg_shape, ModuleArgShape::List);
    }

    #[test]
    fn error_duplicate_module_bracket_and_bare() {
        let err = parse_dynamic_call_pattern("foo(Module, [Module])")
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("multiple"),
            "expected multiple Module error, got: {err}"
        );
    }

    // --- Module-arg-only pattern tests ---

    #[test]
    fn parse_module_arg_only_pattern() {
        let p = parse_dynamic_call_pattern("code:load_file(Module)").unwrap();
        assert_eq!(p.caller_module.as_deref(), Some("code"));
        assert_eq!(p.caller_function, "load_file");
        assert_eq!(p.caller_arity, 1);
        assert_eq!(p.module_arg_index, Some(0));
        assert_eq!(p.module_arg_shape, ModuleArgShape::Atom);
        assert_eq!(p.function_arg_index, None);
        assert_eq!(p.args_list_index, None);
        assert!(!p.direct_arity);
    }

    #[test]
    fn parse_module_arg_only_with_extra_args() {
        let p = parse_dynamic_call_pattern("meck:called(Module, _, _)").unwrap();
        assert_eq!(p.caller_module.as_deref(), Some("meck"));
        assert_eq!(p.caller_function, "called");
        assert_eq!(p.caller_arity, 3);
        assert_eq!(p.module_arg_index, Some(0));
        assert_eq!(p.module_arg_shape, ModuleArgShape::Atom);
        assert_eq!(p.function_arg_index, None);
        assert_eq!(p.args_list_index, None);
    }

    #[test]
    fn parse_bracket_module_arg_only() {
        let p = parse_dynamic_call_pattern("meck:new([Module])").unwrap();
        assert_eq!(p.caller_module.as_deref(), Some("meck"));
        assert_eq!(p.caller_function, "new");
        assert_eq!(p.caller_arity, 1);
        assert_eq!(p.module_arg_index, Some(0));
        assert_eq!(p.module_arg_shape, ModuleArgShape::List);
        assert_eq!(p.function_arg_index, None);
        assert_eq!(p.args_list_index, None);
    }

    #[test]
    fn error_no_meaningful_keyword() {
        let err = parse_dynamic_call_pattern("foo(_, _)")
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("Module") || err.contains("Function"),
            "expected meaningful keyword error, got: {err}"
        );
    }

    #[test]
    fn error_function_without_args() {
        let err = parse_dynamic_call_pattern("foo(Module, Function)")
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("Args") || err.contains("Arity"),
            "expected Args/Arity error, got: {err}"
        );
    }

    #[test]
    fn error_args_without_function() {
        let err = parse_dynamic_call_pattern("foo(Module, Args)")
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("Function"),
            "expected Function error, got: {err}"
        );
    }
}
