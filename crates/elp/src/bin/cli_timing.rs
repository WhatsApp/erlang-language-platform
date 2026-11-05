/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct CommandDurations {
    pub(crate) load_duration_ms: u32,
    pub(crate) operation_duration_ms: u32,
}

pub(crate) struct CommandTimer {
    start_time: SystemTime,
    invocation_start: Instant,
}

impl CommandTimer {
    pub(crate) fn new() -> Self {
        let start = Instant::now();
        // @fb-only: #[cfg(buck_build)]
        // @fb-only: let start = crate::meta_only::invocation_start().unwrap_or(start);
        Self {
            start_time: SystemTime::now(),
            invocation_start: start,
        }
    }

    pub(crate) fn start_time(&self) -> SystemTime {
        self.start_time
    }

    pub(crate) fn load_duration(&self) -> u32 {
        self.load_duration_at(Instant::now())
    }

    fn load_duration_at(&self, loaded: Instant) -> u32 {
        duration_ms(loaded.saturating_duration_since(self.invocation_start))
    }

    pub(crate) fn start_operation(&self) -> OperationTimer {
        self.start_operation_at(Instant::now())
    }

    fn start_operation_at(&self, loaded: Instant) -> OperationTimer {
        OperationTimer {
            load_duration_ms: self.load_duration_at(loaded),
            start: loaded,
        }
    }
}

pub(crate) struct OperationTimer {
    load_duration_ms: u32,
    start: Instant,
}

impl OperationTimer {
    pub(crate) fn finish(self) -> CommandDurations {
        self.finish_at(Instant::now())
    }

    fn finish_at(self, end: Instant) -> CommandDurations {
        CommandDurations {
            load_duration_ms: self.load_duration_ms,
            operation_duration_ms: duration_ms(end.saturating_duration_since(self.start)),
        }
    }
}

pub(crate) fn operation_duration(start: Instant) -> u32 {
    duration_ms(start.elapsed())
}

fn duration_ms(duration: Duration) -> u32 {
    duration.as_millis().try_into().unwrap_or(u32::MAX)
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

    use super::*;

    fn durations(load: Duration, operation: Duration) -> CommandDurations {
        let start = Instant::now();
        let timer = CommandTimer {
            start_time: SystemTime::UNIX_EPOCH,
            invocation_start: start,
        };
        timer
            .start_operation_at(start + load)
            .finish_at(start + load + operation)
    }

    #[test]
    fn phase_durations_saturate_without_wrapping() {
        assert_eq_expected!(u32::MAX, duration_ms(Duration::from_secs(u64::MAX)));
    }

    #[test]
    fn slow_loading_does_not_inflate_operation_duration() {
        assert_eq_expected!(
            CommandDurations {
                load_duration_ms: 180_000,
                operation_duration_ms: 25,
            },
            durations(Duration::from_secs(180), Duration::from_millis(25))
        );
    }

    #[test]
    fn slow_operation_does_not_inflate_loading_duration() {
        assert_eq_expected!(
            CommandDurations {
                load_duration_ms: 25,
                operation_duration_ms: 180_000,
            },
            durations(Duration::from_millis(25), Duration::from_secs(180))
        );
    }

    #[test]
    fn elapsed_phases_do_not_depend_on_wall_clock() {
        assert_eq_expected!(
            CommandDurations {
                load_duration_ms: 0,
                operation_duration_ms: 10,
            },
            durations(Duration::ZERO, Duration::from_millis(10))
        );
    }

    #[test]
    fn fallback_preparation_keeps_the_original_invocation_start() {
        let original_start = Instant::now();
        let fallback = CommandTimer {
            start_time: SystemTime::UNIX_EPOCH + Duration::from_secs(240),
            invocation_start: original_start,
        };
        assert_eq_expected!(
            CommandDurations {
                load_duration_ms: 260_000,
                operation_duration_ms: 15,
            },
            fallback
                .start_operation_at(original_start + Duration::from_secs(260))
                .finish_at(original_start + Duration::from_secs(260) + Duration::from_millis(15))
        );
    }
}
