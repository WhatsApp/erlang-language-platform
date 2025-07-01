/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Simple logger that logs using lsp logs using `env_logger` filter syntax

use crossbeam_channel::Sender;
use elp_log::Builder;
use elp_log::Filter;
use elp_log::ReconfigureLog;
use lsp_server::Message;
use lsp_types::LogMessageParams;
use lsp_types::MessageType;
use lsp_types::notification::LogMessage;
use lsp_types::notification::Notification;

pub struct LspLogger {
    filter: Filter,
    sender: Sender<Message>,
}

impl LspLogger {
    pub fn new(sender: Sender<Message>, filter: Option<&str>) -> Self {
        let filter = {
            let mut builder = Builder::new();
            if let Some(filter) = filter {
                builder.parse(filter);
            } else {
                builder.filter_level(log::LevelFilter::Error);
            }
            filter_server_logs(&mut builder);
            builder.build()
        };

        Self { sender, filter }
    }
}

impl ReconfigureLog for LspLogger {
    fn filter(&self) -> &Filter {
        &self.filter
    }

    fn reconfigure(&mut self, mut filter: Builder) {
        filter_server_logs(&mut filter);
        self.filter = filter.build();
    }

    fn write(&self, record: &log::Record) {
        let message = format!("[{}] {}", record.target(), record.args());

        let typ = match record.level() {
            log::Level::Error => MessageType::ERROR,
            log::Level::Warn => MessageType::WARNING,
            log::Level::Info => MessageType::INFO,
            log::Level::Debug => MessageType::LOG,
            log::Level::Trace => MessageType::LOG,
        };

        let params = LogMessageParams { typ, message };
        let not = lsp_server::Notification::new(LogMessage::METHOD.to_string(), params);
        self.sender.send(not.into()).unwrap();
    }

    fn flush(&self) {}
}

fn filter_server_logs(builder: &mut Builder) {
    // Disable logs from `lsp_server` crate - since we're logging using code
    // from that crate, this can lead to cyclic dependencies and ultimately
    // locking up the server in an infinite loop
    builder.filter_module("lsp_server", log::LevelFilter::Off);
}

#[cfg(test)]
mod tests {
    use elp_log::Logger;
    use expect_test::expect;

    use super::*;

    #[test]
    fn it_works3() {
        let (sender, receiver) = crossbeam_channel::unbounded();

        let lsp_logger = LspLogger::new(sender, None);

        let logger = Logger::default();
        logger.register_logger("test", Box::new(lsp_logger));
        logger.install();

        log::error!("An error occured!");
        log::trace!("This won't be logged");

        let msg = receiver.try_recv().unwrap();
        expect![[r#"
            Notification(
                Notification {
                    method: "window/logMessage",
                    params: Object {
                        "message": String("[elp::server::logger::tests] An error occured!"),
                        "type": Number(1),
                    },
                },
            )
        "#]]
        .assert_debug_eq(&msg);

        assert!(receiver.try_recv().is_err());
    }
}
