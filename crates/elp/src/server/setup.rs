/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::fs;

use anyhow::Context;
use anyhow::Result;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::loader;
use elp_log::Builder;
use elp_log::FileLogger;
use elp_log::Logger;
use elp_log::ReconfigureLog;
use elp_log::timeit_with_telemetry;
use elp_project_model::otp::Otp;
use lsp_server::Connection;
use lsp_server::Notification;
use lsp_types::InitializeParams;
use lsp_types::InitializeResult;
use lsp_types::ServerInfo;
use lsp_types::notification::Notification as _;
use threadpool::ThreadPool;

use super::FILE_WATCH_LOGGER_NAME;
use super::logger::LspLogger;
use crate::config::Config;
use crate::from_json;
use crate::server::Handle;
use crate::server::LOGGER_NAME;
use crate::server::Server;
use crate::server::TaskHandle;
use crate::server::VfsHandle;
use crate::server::capabilities;
use crate::snapshot::TelemetryData;
use crate::task_pool::TaskPool;

pub struct ServerSetup {
    connection: Connection,
    logger: Logger,
}

impl ServerSetup {
    pub fn new(connection: Connection, logger: Logger) -> ServerSetup {
        ServerSetup { connection, logger }
    }

    pub fn to_server(self) -> Result<Server> {
        let config = self.initialize()?;
        self.set_up_logger();
        setup_server(config, self.connection, self.logger)
    }

    fn initialize(&self) -> Result<Config> {
        let _timer = timeit_with_telemetry!(TelemetryData::Initialize);
        let (id, params) = self.connection.initialize_start()?;
        let params = from_json::<lsp_types::InitializeParams>("InitializeParams", params)?;

        let server_capabilities = capabilities::compute(&params.capabilities);

        let server_info = ServerInfo {
            name: "elp".to_string(),
            version: Some(crate::version()),
        };

        let result = InitializeResult {
            capabilities: server_capabilities,
            server_info: Some(server_info),
            offset_encoding: None,
        };

        self.connection
            .initialize_finish(id, serde_json::to_value(result.clone()).unwrap())
            .with_context(|| format!("during initialization finish: {:?}", result))?;

        let otp_details =
            Otp::system_version().unwrap_or_else(|err| format!("Could not find OTP: {}", err));
        let message = format!(
            "ELP version: {}, OTP version: {}",
            crate::version(),
            otp_details
        );
        let show_message_params = lsp_types::ShowMessageParams {
            typ: lsp_types::MessageType::INFO,
            message,
        };
        let notif = Notification::new(
            lsp_types::notification::ShowMessage::METHOD.to_string(),
            show_message_params,
        );
        _ = self.connection.sender.send(notif.into());

        // At this point the Client is able to start sending us normal
        // operational requests.

        if let Some(client_info) = &params.client_info {
            let client_version = client_info.version.as_deref().unwrap_or("<unknown>");
            log::info!("Client '{}' {}", client_info.name, client_version);
        }

        let root_path = root_path(&params)?;
        // Note: the LSP spec says initialization_options can be
        // anything.  If they match config, that is because we
        // choose this to be so in the client.
        let mut config = Config::new(root_path, params.capabilities);
        if let Some(options) = params.initialization_options {
            config.update_gks(options.clone());
            config.update(options);
        }

        Ok(config)
    }

    fn set_up_logger(&self) {
        let sender = self.connection.sender.clone();
        let logger = LspLogger::new(sender, None);
        self.logger.register_logger(LOGGER_NAME, Box::new(logger));

        // Set up a logger for tracking down why we are seeing stale
        // results when branches are switched, as per T218973130
        let log_dir = "/tmp/elp";
        let _ = fs::create_dir_all(log_dir);
        let log_file = format!(
            "{}/elp_file_watch_reports-{}.log",
            log_dir,
            std::process::id()
        );
        if let Ok(file) = fs::File::create(log_file) {
            let no_buffering = true;
            let mut file_logger = FileLogger::new(Some(file), no_buffering, None);
            let mut builder = Builder::new();
            builder.filter(Some(FILE_WATCH_LOGGER_NAME), log::LevelFilter::Info);
            file_logger.reconfigure(builder);
            self.logger
                .register_logger(FILE_WATCH_LOGGER_NAME, Box::new(file_logger));

            log::info!(target: FILE_WATCH_LOGGER_NAME, "Server starting");
        }
    }
}

pub fn setup_server(config: Config, connection: Connection, logger: Logger) -> Result<Server> {
    let vfs_loader = set_up_vfs_loader();
    let task_pool = set_up_task_pool();
    let cache_pool = set_up_single_thread_pool();
    let eqwalizer_pool = set_up_eqwalizer_pool();
    let project_pool = set_up_single_thread_pool();
    log::debug!("initial state: {:#?}", config);

    Ok(Server::new(
        connection,
        vfs_loader,
        task_pool,
        project_pool,
        cache_pool,
        eqwalizer_pool,
        logger,
        config,
    ))
}

fn set_up_vfs_loader() -> VfsHandle {
    let (sender, receiver) = crossbeam_channel::unbounded();
    let handle: vfs_notify::NotifyHandle = loader::Handle::spawn(sender);
    let handle = Box::new(handle) as Box<dyn loader::Handle>;
    Handle { handle, receiver }
}

fn set_up_task_pool() -> TaskHandle {
    let (sender, receiver) = crossbeam_channel::unbounded();
    let handle = TaskPool::new(sender);
    Handle { handle, receiver }
}

fn set_up_eqwalizer_pool() -> TaskHandle {
    let (sender, receiver) = crossbeam_channel::unbounded();
    let handle = TaskPool::new(sender);
    Handle { handle, receiver }
}

fn set_up_single_thread_pool() -> TaskHandle {
    let (sender, receiver) = crossbeam_channel::unbounded();
    let pool = ThreadPool::new(1);
    let handle = TaskPool::new_with_pool(sender, pool);
    Handle { handle, receiver }
}

fn root_path(params: &InitializeParams) -> Result<AbsPathBuf> {
    match params
        .root_uri
        .as_ref()
        .and_then(|uri| uri.to_file_path().ok())
        .map(AbsPathBuf::assert_utf8)
    {
        Some(path) => Ok(path),
        None => {
            let cwd = env::current_dir()?;
            Ok(AbsPathBuf::assert_utf8(cwd))
        }
    }
}
