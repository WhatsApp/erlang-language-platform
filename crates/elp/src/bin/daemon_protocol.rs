/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp::arc_types;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub(crate) struct DoneMessage<T = ()> {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) outcome: Option<T>,
}

impl<T> DoneMessage<T> {
    pub(crate) fn ok(outcome: Option<T>) -> Self {
        Self { outcome }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "type", rename_all = "snake_case")]
pub(crate) enum DaemonResponse<T = ()> {
    Done {
        #[serde(flatten)]
        message: DoneMessage<T>,
    },
    Error {
        message: String,
    },
    Unavailable {
        message: String,
    },
    Restart {
        reason: String,
    },
    Info {
        message: String,
    },
    Diagnostic {
        diagnostic: arc_types::Diagnostic,
        rendered: Option<RenderedDiagnostic>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub(crate) struct RenderedDiagnostic {
    pub(crate) plain: String,
    pub(crate) ansi: String,
}

impl<T> DaemonResponse<T> {
    pub(crate) fn success(outcome: Option<T>) -> Self {
        Self::Done {
            message: DoneMessage::ok(outcome),
        }
    }

    pub(crate) fn error(message: impl Into<String>) -> Self {
        Self::Error {
            message: message.into(),
        }
    }

    pub(crate) fn unavailable(message: impl Into<String>) -> Self {
        Self::Unavailable {
            message: message.into(),
        }
    }

    pub(crate) fn restart(reason: impl Into<String>) -> Self {
        Self::Restart {
            reason: reason.into(),
        }
    }

    pub(crate) fn info(message: impl Into<String>) -> Self {
        Self::Info {
            message: message.into(),
        }
    }

    pub(crate) fn diagnostic(
        diagnostic: arc_types::Diagnostic,
        rendered: Option<RenderedDiagnostic>,
    ) -> Self {
        Self::Diagnostic {
            diagnostic,
            rendered,
        }
    }
}

impl RenderedDiagnostic {
    pub(crate) fn new(plain: String, ansi: String) -> Self {
        Self { plain, ansi }
    }

    pub(crate) fn output(&self, use_color: bool) -> &str {
        if use_color { &self.ansi } else { &self.plain }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

    use super::*;
    use crate::lint_cli::LintOutcome;

    #[test]
    fn done_response_has_no_payload() {
        let actual = serde_json::to_value(DaemonResponse::<()>::success(None))
            .expect("response should serialize");
        let expected = serde_json::json!({
            "type": "done",
        });
        assert_eq_expected!(expected, actual);
    }

    #[test]
    fn done_response_lint_outcome_round_trips() {
        for expected_outcome in [
            LintOutcome::Clean,
            LintOutcome::Findings { has_errors: true },
        ] {
            let response = DaemonResponse::<LintOutcome>::success(Some(expected_outcome));
            let json = serde_json::to_string(&response).expect("response should serialize");
            let decoded: DaemonResponse<LintOutcome> =
                serde_json::from_str(&json).expect("response should deserialize");
            let DaemonResponse::Done {
                message:
                    DoneMessage {
                        outcome: Some(actual_outcome),
                    },
            } = decoded
            else {
                panic!("lint completion should carry a typed outcome");
            };

            assert_eq_expected!(expected_outcome, actual_outcome);
        }
    }

    #[test]
    fn error_response_carries_message() {
        let response = DaemonResponse::<()>::error("bad thing");
        let actual = serde_json::to_value(response).expect("response should serialize");
        let expected = serde_json::json!({
            "type": "error",
            "message": "bad thing",
        });
        assert_eq_expected!(expected, actual);
    }

    #[test]
    fn unavailable_response_carries_message() {
        let response = DaemonResponse::<()>::unavailable("reload failed");
        let actual = serde_json::to_value(response).expect("response should serialize");
        let expected = serde_json::json!({
            "type": "unavailable",
            "message": "reload failed",
        });
        assert_eq_expected!(expected, actual);
    }

    #[test]
    fn restart_response_carries_reason() {
        let response = DaemonResponse::<()>::restart("ELP config changed");
        let actual = serde_json::to_value(response).expect("response should serialize");
        let expected = serde_json::json!({
            "type": "restart",
            "reason": "ELP config changed",
        });
        assert_eq_expected!(expected, actual);
    }

    #[test]
    fn diagnostic_response_round_trips_as_typed_payload() {
        let diagnostic = arc_types::Diagnostic::new(
            Path::new("src/foo.erl"),
            1,
            Some(1),
            arc_types::Severity::Error,
            "incompatible_types".to_string(),
            "expected integer".to_string(),
            None,
            None,
        );
        let response = DaemonResponse::<()>::diagnostic(
            diagnostic,
            Some(RenderedDiagnostic::new(
                "plain\n".to_string(),
                "ansi\n".to_string(),
            )),
        );

        let actual = serde_json::to_value(&response).expect("response should serialize");
        let expected = serde_json::json!({
            "type": "diagnostic",
            "diagnostic": {
                "path": "src/foo.erl",
                "line": 1,
                "char": 1,
                "code": "ELP",
                "severity": "error",
                "name": "incompatible_types",
                "original": null,
                "replacement": null,
                "description": "expected integer",
                "docPath": null,
            },
            "rendered": {
                "plain": "plain\n",
                "ansi": "ansi\n",
            },
        });
        assert_eq_expected!(expected, actual);

        let json = serde_json::to_string(&response).expect("response should serialize");
        let decoded: DaemonResponse<()> =
            serde_json::from_str(&json).expect("response should deserialize");
        assert_eq!(
            response, decoded,
            "the typed daemon response should round-trip"
        );
    }
}
