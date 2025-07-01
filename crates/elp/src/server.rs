/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use core::str;
use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use always_assert::always;
use anyhow::Result;
use anyhow::bail;
use capabilities::text_document_symbols_dynamic_registration;
use crossbeam_channel::Receiver;
use crossbeam_channel::Sender;
use crossbeam_channel::select;
use dispatch::NotificationDispatcher;
use elp_eqwalizer::ast::Pos;
use elp_eqwalizer::types::Type;
use elp_ide::Analysis;
use elp_ide::AnalysisHost;
use elp_ide::diagnostics;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::LabeledDiagnostics;
use elp_ide::diagnostics::LintConfig;
use elp_ide::diagnostics_collection::DiagnosticCollection;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::AppDataId;
use elp_ide::elp_ide_db::elp_base_db::AppDataIndex;
use elp_ide::elp_ide_db::elp_base_db::ChangedFile;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FileKind;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ProjectApps;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide::elp_ide_db::elp_base_db::SourceRoot;
use elp_ide::elp_ide_db::elp_base_db::SourceRootId;
use elp_ide::elp_ide_db::elp_base_db::Vfs;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::elp_base_db::loader;
use elp_ide::elp_ide_db::elp_base_db::set_app_data_id_by_file;
use elp_log::Logger;
use elp_log::TimeIt;
use elp_log::telemetry;
use elp_log::telemetry::TelemetryMessage;
use elp_log::timeit;
use elp_log::timeit_exceeds;
use elp_project_model::ElpConfig;
use elp_project_model::Project;
use elp_project_model::ProjectManifest;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::buck::BuckQueryError;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use indexmap::map::Entry;
use lsp_server::Connection;
use lsp_server::ErrorCode;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_server::Response;
use lsp_types;
use lsp_types::FileChangeType;
use lsp_types::FileEvent;
use lsp_types::MessageActionItem;
use lsp_types::MessageActionItemProperty;
use lsp_types::ShowDocumentParams;
use lsp_types::ShowMessageParams;
use lsp_types::ShowMessageRequestParams;
use lsp_types::Url;
use lsp_types::notification;
use lsp_types::notification::Notification as _;
use lsp_types::request;
use lsp_types::request::Request as _;
use parking_lot::Mutex;
use parking_lot::RwLock;
use parking_lot::RwLockWriteGuard;
use telemetry_manager::TelemetryManager;
use vfs::Change;
use vfs::loader::LoadingProgress;

use self::dispatch::RequestDispatcher;
use self::progress::ProgressBar;
use self::progress::ProgressManager;
use self::progress::ProgressTask;
use self::progress::Spinner;
use crate::config::Config;
use crate::convert;
use crate::convert::ide_to_lsp_diagnostic;
use crate::document::Document;
use crate::handlers;
use crate::line_endings::LineEndings;
use crate::lsp_ext;
use crate::mem_docs::DocumentData;
use crate::mem_docs::MemDocs;
use crate::project_loader::ProjectLoader;
use crate::project_loader::ReloadManager;
use crate::read_lint_config_file;
use crate::reload::ProjectFolders;
use crate::snapshot::SharedMap;
use crate::snapshot::Snapshot;
use crate::task_pool::TaskPool;

mod capabilities;
mod dispatch;
mod logger;
mod progress;
pub mod setup;
mod telemetry_manager;

const LOGGER_NAME: &str = "lsp";
const FILE_WATCH_LOGGER_NAME: &str = "watched_files";
const ERLANG_SERVICE_SUPPORTED_EXTENSIONS: &[FileKind] = &[
    FileKind::SrcModule,
    FileKind::TestModule,
    FileKind::Header,
    FileKind::Escript,
];
const SLOW_DURATION: Duration = Duration::from_millis(300);
/// If the main loop exceeds this time, log the specific request causing the problem
const TOO_SLOW_DURATION: Duration = Duration::from_millis(3000);
const INCLUDE_GENERATED: bool = true;

enum Event {
    Lsp(lsp_server::Message),
    Vfs(loader::Message),
    Task(Task),
    Telemetry(TelemetryMessage),
}

#[derive(Debug)]
#[allow(clippy::type_complexity)]
pub enum Task {
    Response(lsp_server::Response),
    ShowMessage(lsp_types::ShowMessageParams),
    ShowMessageRequest(lsp_types::ShowMessageRequestParams),
    FetchProject(Spinner, Vec<Project>),
    NativeDiagnostics(Vec<(FileId, LabeledDiagnostics)>),
    EqwalizerDiagnostics(
        Spinner,
        Vec<(FileId, Vec<diagnostics::Diagnostic>, Arc<Vec<(Pos, Type)>>)>,
    ),
    EqwalizerProjectDiagnostics(
        Spinner,
        Vec<(ProjectId, Vec<(FileId, Vec<diagnostics::Diagnostic>)>)>,
    ),
    EdocDiagnostics(Spinner, Vec<(FileId, Vec<diagnostics::Diagnostic>)>),
    CommonTestDiagnostics(Spinner, Vec<(FileId, Vec<diagnostics::Diagnostic>)>),
    ErlangServiceDiagnostics(Vec<(FileId, LabeledDiagnostics)>),
    CompileDeps(Spinner),
    Progress(ProgressTask),
    ScheduleCache,
    UpdateCache(Vec<FileId>),
    ScheduleEqwalizeAll(ProjectId),
    UpdateEqwalizeAll(ProgressBar, ProjectId, String, Vec<FileId>),
}

impl fmt::Debug for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Event::Lsp(lsp_server::Message::Notification(notif))
                if notif.method == notification::DidOpenTextDocument::METHOD
                    || notif.method == notification::DidChangeTextDocument::METHOD
                    || notif.method == notification::LogMessage::METHOD =>
            {
                f.debug_struct("Notification")
                    .field("method", &notif.method)
                    .finish_non_exhaustive()
            }
            Event::Lsp(it) => fmt::Debug::fmt(it, f),
            Event::Vfs(it) => fmt::Debug::fmt(it, f),
            Event::Task(it) => fmt::Debug::fmt(it, f),
            Event::Telemetry(it) => fmt::Debug::fmt(it, f),
        }
    }
}

type ReqHandler = fn(&mut Server, Response) -> Result<()>;
type ReqQueue = lsp_server::ReqQueue<(String, TimeIt), ReqHandler>;

#[derive(Debug)]
pub enum Status {
    Initialising,
    Loading(ProgressBar),
    Running,
    ShuttingDown,
}

impl Status {
    pub fn as_lsp_status(&self) -> lsp_ext::Status {
        match self {
            Status::Initialising => lsp_ext::Status::Loading,
            Status::Loading(_) => lsp_ext::Status::Loading,
            Status::Running => lsp_ext::Status::Running,
            Status::ShuttingDown => lsp_ext::Status::ShuttingDown,
        }
    }
}

impl PartialEq for Status {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

pub struct Handle<H, C> {
    pub(crate) handle: H,
    pub(crate) receiver: C,
}

pub type VfsHandle = Handle<Box<dyn loader::Handle>, Receiver<loader::Message>>;
pub type TaskHandle = Handle<TaskPool<Task>, Receiver<Task>>;

pub type EqwalizerTypes = FxHashMap<FileId, Arc<Vec<(Pos, Type)>>>;

pub struct Server {
    connection: Connection,
    vfs_loader: VfsHandle,
    task_pool: TaskHandle,
    project_pool: TaskHandle,
    cache_pool: TaskHandle,
    eqwalizer_pool: TaskHandle,
    diagnostics: Arc<DiagnosticCollection>,
    eqwalizer_types: Arc<EqwalizerTypes>,
    req_queue: ReqQueue,
    progress: ProgressManager,
    mem_docs: Arc<RwLock<MemDocs>>,
    newly_opened_documents: Vec<ChangedFile>,
    vfs: Arc<RwLock<Vfs>>,
    file_set_config: FileSetConfig,
    line_ending_map: SharedMap<FileId, LineEndings>,
    config: Arc<Config>,
    diagnostics_config: Arc<DiagnosticsConfig>,
    lint_config: Arc<LintConfig>,
    analysis_host: AnalysisHost,
    status: Status,
    projects: Arc<Vec<Project>>,
    project_loader: Arc<Mutex<ProjectLoader>>,
    reload_manager: Arc<Mutex<ReloadManager>>,
    unresolved_app_id_paths: Arc<FxHashMap<AbsPathBuf, AppDataId>>,
    update_app_data_ids: bool,
    reset_source_roots: bool,
    native_diagnostics_requested: bool,
    eqwalizer_and_erlang_service_diagnostics_requested: bool,
    eqwalizer_project_diagnostics_requested: bool,
    edoc_diagnostics_requested: bool,
    ct_diagnostics_requested: bool,
    cache_scheduled: bool,
    eqwalize_all_scheduled: FxHashSet<ProjectId>,
    eqwalize_all_completed: bool,
    logger: Logger,
    telemetry_manager: TelemetryManager,
    // We record the highest FileId index seen as a count of the number of files recorded
    highest_file_id: u32,

    // Progress reporting
    vfs_config_version: u32,
}

impl Drop for Server {
    fn drop(&mut self) {
        // Remove the LSP logger. This prevents a deadlock because the logger has a strong
        // reference to the `Connection`'s `Sender`.
        self.logger.remove_logger(LOGGER_NAME);
        // Cancel any ongoing analyses.
        self.analysis_host.request_cancellation();
    }
}

impl Server {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        connection: Connection,
        vfs_loader: VfsHandle,
        task_pool: TaskHandle,
        project_pool: TaskHandle,
        cache_pool: TaskHandle,
        eqwalizer_pool: TaskHandle,
        logger: Logger,
        config: Config,
    ) -> Server {
        let mut this = Server {
            connection,
            progress: ProgressManager::default(),
            vfs_loader,
            task_pool,
            project_pool,
            cache_pool,
            eqwalizer_pool,
            diagnostics: Arc::new(DiagnosticCollection::default()),
            eqwalizer_types: Arc::new(FxHashMap::default()),
            req_queue: ReqQueue::default(),
            mem_docs: Arc::new(RwLock::new(MemDocs::default())),
            newly_opened_documents: Vec::default(),
            vfs: Arc::new(RwLock::new(Vfs::default())),
            file_set_config: FileSetConfig::default(),
            line_ending_map: SharedMap::default(),
            config: Arc::new(config.clone()),
            diagnostics_config: Arc::new(DiagnosticsConfig::default()),
            lint_config: Arc::new(LintConfig::default()),
            analysis_host: AnalysisHost::default(),
            status: Status::Initialising,
            projects: Arc::new(vec![]),
            project_loader: Arc::new(Mutex::new(ProjectLoader::new())),
            reload_manager: Arc::new(Mutex::new(ReloadManager::new())),
            unresolved_app_id_paths: Arc::new(FxHashMap::default()),
            update_app_data_ids: false,
            reset_source_roots: false,
            native_diagnostics_requested: false,
            eqwalizer_and_erlang_service_diagnostics_requested: false,
            eqwalizer_project_diagnostics_requested: false,
            edoc_diagnostics_requested: false,
            ct_diagnostics_requested: false,
            cache_scheduled: false,
            eqwalize_all_scheduled: FxHashSet::default(),
            eqwalize_all_completed: false,
            logger,
            telemetry_manager: TelemetryManager::new(),
            vfs_config_version: 0,
            highest_file_id: 0,
        };

        // Run config-based initialisation
        this.update_configuration(config);
        this
    }

    pub fn snapshot(&self) -> Snapshot {
        Snapshot::new(
            Arc::clone(&self.config),
            Arc::clone(&self.diagnostics_config),
            self.analysis_host.analysis(),
            Arc::clone(&self.diagnostics),
            Arc::clone(&self.eqwalizer_types),
            Arc::clone(&self.vfs),
            Arc::clone(&self.mem_docs),
            Arc::clone(&self.line_ending_map),
            Arc::clone(&self.projects),
        )
    }

    pub fn main_loop(mut self) -> Result<()> {
        if self.config.did_save_text_document_dynamic_registration() {
            let save_registration_options = lsp_types::TextDocumentSaveRegistrationOptions {
                include_text: Some(false),
                text_document_registration_options: lsp_types::TextDocumentRegistrationOptions {
                    document_selector: Some(vec![lsp_types::DocumentFilter {
                        language: None,
                        scheme: None,
                        pattern: Some("**/*.{e,h}rl".to_string()),
                    }]),
                },
            };

            let registration = lsp_types::Registration {
                id: notification::DidSaveTextDocument::METHOD.to_string(),
                method: notification::DidSaveTextDocument::METHOD.to_string(),
                register_options: Some(serde_json::to_value(save_registration_options).unwrap()),
            };

            self.send_request::<request::RegisterCapability>(
                lsp_types::RegistrationParams {
                    registrations: vec![registration],
                },
                |_, _| Ok(()),
            )
        }

        while let Some(event) = self.next_event() {
            if let Event::Lsp(lsp_server::Message::Notification(notif)) = &event {
                if notif.method == notification::Exit::METHOD {
                    return Ok(());
                }
            }
            let _timer1 = timeit_exceeds!("main_loop_health", SLOW_DURATION);
            let _timer2 = timeit_exceeds!(format!("slow_event:{:?}", event), TOO_SLOW_DURATION);
            self.handle_event(event)?;
            self.telemetry_manager
                .on_periodic(self.mem_docs.read().len(), self.highest_file_id);
        }

        bail!("client exited without proper shutdown sequence");
    }

    fn next_event(&self) -> Option<Event> {
        select! {
            recv(self.connection.receiver) -> msg => {
                msg.ok().map(Event::Lsp)
            }

            recv(self.vfs_loader.receiver) -> msg => {
                Some(Event::Vfs(msg.unwrap()))
            }

            recv(self.task_pool.receiver) -> msg => {
                Some(Event::Task(msg.unwrap()))
            }

            recv(self.progress.receiver()) -> msg => {
                Some(Event::Task(Task::Progress(msg.unwrap())))
            }

            recv(telemetry::receiver()) -> msg => {
                Some(Event::Telemetry(msg.unwrap()))
            }

            recv (self.project_pool.receiver) -> msg => {
                Some(Event::Task(msg.unwrap()))
            }

            recv (self.cache_pool.receiver) -> msg => {
                Some(Event::Task(msg.unwrap()))
            }

            recv (self.eqwalizer_pool.receiver) -> msg => {
                Some(Event::Task(msg.unwrap()))
            }

        }
    }

    fn handle_event(&mut self, event: Event) -> Result<()> {
        log::info!("handle_event {:?}", event);

        match event {
            Event::Lsp(msg) => match msg {
                lsp_server::Message::Request(req) => self.on_request(req)?,
                lsp_server::Message::Notification(notif) => self.on_notification(notif)?,
                lsp_server::Message::Response(resp) => self.complete_request(resp)?,
            },
            Event::Vfs(mut msg) => loop {
                match msg {
                    loader::Message::Loaded { files } => self.on_loader_loaded(files, false),
                    loader::Message::Changed { files } => self.on_loader_loaded(files, true),
                    loader::Message::Progress {
                        n_total,
                        n_done,
                        config_version,
                        dir: _,
                    } => self.on_loader_progress(n_total, n_done, config_version),
                }

                // Coalesce many VFS event into a single main loop turn
                msg = match self.vfs_loader.receiver.try_recv() {
                    Ok(msg) => msg,
                    Err(_) => break,
                }
            },
            Event::Task(task) => match task {
                Task::Response(response) => self.send_response(response),
                Task::FetchProject(spinner, projects) => {
                    self.fetch_project_completed(&spinner, projects)?;
                    spinner.end();
                    self.reload_manager.lock().set_reload_done();
                }
                Task::NativeDiagnostics(diags) => self.native_diagnostics_completed(diags),
                Task::EqwalizerDiagnostics(spinner, diags_types) => {
                    spinner.end();
                    self.eqwalizer_diagnostics_completed(diags_types)
                }
                Task::EqwalizerProjectDiagnostics(spinner, diags) => {
                    spinner.end();
                    self.eqwalizer_project_diagnostics_completed(diags)
                }
                Task::EdocDiagnostics(spinner, diags) => {
                    spinner.end();
                    self.edoc_diagnostics_completed(diags)
                }
                Task::CommonTestDiagnostics(spinner, diags) => {
                    spinner.end();
                    self.ct_diagnostics_completed(diags)
                }
                Task::ErlangServiceDiagnostics(diags) => {
                    self.erlang_service_diagnostics_completed(diags)
                }
                Task::CompileDeps(spinner) => {
                    self.analysis_host
                        .raw_database()
                        .update_erlang_service_paths();
                    spinner.end();
                    self.eqwalizer_and_erlang_service_diagnostics_requested = true;
                    self.native_diagnostics_requested = true;
                }
                Task::Progress(progress) => self.report_progress(progress),
                Task::UpdateCache(files) => self.update_cache(files),
                Task::ScheduleCache => self.schedule_cache(),
                Task::UpdateEqwalizeAll(spinner, project_id, project_name, files) => {
                    self.update_eqwalize_all(spinner, project_id, project_name, files)
                }
                Task::ScheduleEqwalizeAll(project_id) => self.schedule_eqwalize_all(project_id),
                Task::ShowMessage(params) => self.show_message(params),
                Task::ShowMessageRequest(params) => self.show_message_request(params),
            },
            Event::Telemetry(message) => self.on_telemetry(message),
        }

        if self.status == Status::ShuttingDown {
            return Ok(());
        }

        let to_reload = self.reload_manager.lock().query_changed_files();
        if let Some(to_reload) = to_reload {
            self.reload_manager.lock().set_reload_active();
            self.reload_project(to_reload);
        }

        let (changed, highest_file_id) = self.process_changes_to_vfs_store();
        // This has to be outside of process_changes_to_vfs_store to
        // avoid mutability clashes
        self.record_highest_file_id(highest_file_id);

        if self.status == Status::Running {
            if mem::take(&mut self.native_diagnostics_requested)
                || (!self.config.native_diagnostics_on_save_only() && changed)
            {
                self.update_native_diagnostics();
            }

            if mem::take(&mut self.eqwalizer_and_erlang_service_diagnostics_requested) {
                self.update_eqwalizer_diagnostics();
                self.update_erlang_service_diagnostics();
            }

            if mem::take(&mut self.eqwalizer_project_diagnostics_requested) {
                self.update_eqwalizer_project_diagnostics();
            }

            if mem::take(&mut self.edoc_diagnostics_requested) {
                self.update_edoc_diagnostics();
            }

            if mem::take(&mut self.ct_diagnostics_requested) {
                self.update_ct_diagnostics();
            }
        }

        if let Some(diagnostic_changes) = Arc::make_mut(&mut self.diagnostics).take_changes() {
            log::info!("changed diagnostics: {:?}", diagnostic_changes);

            let snapshot = self.snapshot();
            for file_id in &diagnostic_changes {
                let url = file_id_to_url(&self.vfs.read(), *file_id);
                let line_index = match snapshot.analysis.line_index(*file_id) {
                    Ok(line_index) => line_index,
                    Err(err) => {
                        // We have had a cancellation (How?). Restore
                        // the list of files to be published, and try
                        // again next time around the loop
                        log::warn!("Snapshot cancelled while publishing diagnostics: {}", err);
                        Arc::make_mut(&mut self.diagnostics).return_changes(diagnostic_changes);
                        break;
                    }
                };
                let diagnostics = self
                    .diagnostics
                    .diagnostics_for(*file_id)
                    .iter()
                    .map(|d| ide_to_lsp_diagnostic(&line_index, &url, d))
                    .collect();
                let version = convert::vfs_path(&url)
                    .map(|path| self.mem_docs.read().get(&path).cloned())
                    .unwrap_or_default()
                    .map(|doc_info| doc_info.version);

                self.send_notification::<notification::PublishDiagnostics>(
                    lsp_types::PublishDiagnosticsParams {
                        uri: url,
                        diagnostics,
                        version,
                    },
                );
            }
        }

        Ok(())
    }

    fn on_request(&mut self, req: Request) -> Result<()> {
        let request_timer = timeit!("handle req {}#{}", req.method, req.id);
        self.register_request(&req, request_timer);

        match self.status {
            Status::Initialising | Status::Loading(_)
                if req.method != request::Shutdown::METHOD && !req.method.starts_with("elp/") =>
            {
                // We can process document symbols while loading.
                if !(self.status != Status::Initialising
                    && req.method == request::DocumentSymbolRequest::METHOD)
                {
                    let id = req.id.clone();
                    self.send_response(Response::new_err(
                        id,
                        ErrorCode::ContentModified as i32,
                        "elp is still loading".to_string(),
                    ));
                    return Ok(());
                }
            }
            Status::ShuttingDown => {
                self.send_response(Response::new_err(
                    req.id.clone(),
                    ErrorCode::InvalidRequest as i32,
                    "shutdown already requested".to_string(),
                ));

                return Ok(());
            }
            _ => {}
        }

        RequestDispatcher::new(self, req)
            .on_sync::<request::Shutdown>(|this, ()| {
                this.transition(Status::ShuttingDown);
                Ok(())
            })?
            .on::<request::CodeActionRequest>(handlers::handle_code_action)
            .on::<request::CodeActionResolveRequest>(handlers::handle_code_action_resolve)
            .on::<request::GotoDefinition>(handlers::handle_goto_definition)
            .on::<request::GotoTypeDefinition>(handlers::handle_goto_type_definition)
            .on::<request::References>(handlers::handle_references)
            .on::<request::Completion>(handlers::handle_completion)
            .on::<request::ResolveCompletionItem>(handlers::handle_completion_resolve)
            .on::<request::DocumentSymbolRequest>(handlers::handle_document_symbol)
            .on::<request::WorkspaceSymbol>(handlers::handle_workspace_symbol)
            .on::<request::Rename>(handlers::handle_rename)
            .on::<lsp_ext::HoverRequest>(handlers::handle_hover)
            .on::<request::FoldingRangeRequest>(handlers::handle_folding_range)
            .on::<request::DocumentHighlightRequest>(handlers::handle_document_highlight)
            .on::<lsp_types::request::CallHierarchyPrepare>(handlers::handle_call_hierarchy_prepare)
            .on::<lsp_types::request::CallHierarchyIncomingCalls>(
                handlers::handle_call_hierarchy_incoming,
            )
            .on::<lsp_types::request::CallHierarchyOutgoingCalls>(
                handlers::handle_call_hierarchy_outgoing,
            )
            .on::<request::SignatureHelpRequest>(handlers::handle_signature_help)
            .on::<request::SelectionRangeRequest>(handlers::handle_selection_range)
            .on::<request::SemanticTokensFullRequest>(handlers::handle_semantic_tokens_full)
            .on::<request::SemanticTokensFullDeltaRequest>(
                handlers::handle_semantic_tokens_full_delta,
            )
            .on::<request::SemanticTokensRangeRequest>(handlers::handle_semantic_tokens_range)
            .on::<request::CodeLensRequest>(handlers::handle_code_lens)
            .on::<request::InlayHintRequest>(handlers::handle_inlay_hints)
            .on::<request::InlayHintResolveRequest>(handlers::handle_inlay_hints_resolve)
            .on::<lsp_ext::ExpandMacro>(handlers::handle_expand_macro)
            .on::<lsp_ext::Ping>(handlers::pong)
            .on::<lsp_ext::ExternalDocs>(handlers::handle_external_docs)
            .finish();

        Ok(())
    }

    fn on_notification(&mut self, notif: Notification) -> Result<()> {
        NotificationDispatcher::new(self, notif)
            .on::<notification::Cancel>(|this, params| {
                let id = parse_id(params.id);
                this.cancel(id);
                Ok(())
            })?
            .on::<notification::DidOpenTextDocument>(|this, params| {
                this.eqwalizer_and_erlang_service_diagnostics_requested = true;
                if this.config.eqwalizer().all {
                    this.eqwalizer_project_diagnostics_requested = true;
                }
                if this.config.edoc() {
                    this.edoc_diagnostics_requested = true;
                }
                this.ct_diagnostics_requested = true;
                this.native_diagnostics_requested = true;
                if let Ok(path) = convert::abs_path(&params.text_document.uri) {
                    this.fetch_projects_if_needed(&path);
                    let path = VfsPath::from(path);
                    let already_exists = this
                        .mem_docs
                        .write()
                        .insert(
                            path.clone(),
                            DocumentData::new(
                                params.text_document.version,
                                params.text_document.text.clone().into_bytes(),
                            ),
                        )
                        .is_err();
                    if already_exists {
                        tracing::error!("duplicate DidOpenTextDocument: {}", path);
                        log::error!("duplicate DidOpenTextDocument: {}", path);
                    }

                    let mut vfs = this.vfs.write();
                    let bytes = params.text_document.text.into_bytes();
                    vfs.set_file_contents(path.clone(), Some(bytes.clone()));

                    // Until we bring over the full rust-analyzer
                    // style change processing, make a list of files
                    // that are freshly opened so diagnostics are
                    // generated for them, despite no changes being
                    // registered in vfs.
                    let (file_id, _) = vfs.file_id(&path).unwrap();
                    this.newly_opened_documents.push(ChangedFile {
                        file_id,
                        change: Change::Modify(bytes, params.text_document.version as u64),
                    });
                } else {
                    log::error!(
                        "DidOpenTextDocument: could not get vfs path for {}",
                        params.text_document.uri
                    );
                }

                Ok(())
            })?
            .on::<notification::DidChangeTextDocument>(|this, params| {
                if let Ok(path) = convert::vfs_path(&params.text_document.uri) {
                    let mut mem_docs = this.mem_docs.write();
                    let Some(DocumentData { version, data }) = mem_docs.get_mut(&path) else {
                        tracing::error!(?path, "unexpected DidChangeTextDocument");
                        return Ok(());
                    };
                    // The version passed in DidChangeTextDocument is
                    // the version after all edits are applied so we
                    // should apply it before the vfs is notified.
                    *version = params.text_document.version;

                    let mut document = Document::from_bytes(data);
                    document.apply_changes(params.content_changes);
                    let new_contents = document.into_bytes();

                    if *data != new_contents {
                        data.clone_from(&new_contents);
                        this.vfs.write().set_file_contents(path, Some(new_contents));
                    }
                }
                Ok(())
            })?
            .on::<notification::DidCloseTextDocument>(|this, params| {
                let url = params.text_document.uri;
                let analysis = this.snapshot().analysis;
                let mut diagnostics = Vec::new();
                if let Ok(path) = convert::vfs_path(&url) {
                    if this.mem_docs.write().remove(&path).is_err() {
                        tracing::error!("orphan DidCloseTextDocument: {}", path);
                        log::error!("unexpected DidCloseTextDocument: {}", path);
                    }

                    if let Some(path) = path.as_path() {
                        this.vfs_loader.handle.invalidate(path.to_path_buf());
                    }

                    // If project-wide diagnostics are enabled, ensure we don't lose the eqwalizer ones.
                    if this.config.eqwalizer().all {
                        let vfs = this.vfs.read();
                        if let Some((file_id, _)) = vfs.file_id(&path) {
                            Arc::make_mut(&mut this.diagnostics)
                                .move_eqwalizer_diagnostics_to_project_diagnostics(file_id);
                            if let Ok(line_index) = analysis.line_index(file_id) {
                                diagnostics = this
                                    .diagnostics
                                    .project_diagnostics_for(file_id)
                                    .iter()
                                    .map(|d| ide_to_lsp_diagnostic(&line_index, &url, d))
                                    .collect()
                            }
                        }
                    }
                }

                // Clear the diagnostics for the previously known version of the file.
                this.send_notification::<lsp_types::notification::PublishDiagnostics>(
                    lsp_types::PublishDiagnosticsParams {
                        uri: url,
                        diagnostics,
                        version: None,
                    },
                );

                Ok(())
            })?
            .on::<notification::DidSaveTextDocument>(|this, params| {
                let change = FileEvent::new(params.text_document.uri, FileChangeType::CHANGED);
                if let Ok(path) = convert::abs_path(&change.uri) {
                    // We only put .erl/.hrl files into the
                    // `mem_docs` store, but will get changes for
                    // other kinds of files. So we neede to do
                    // this check.
                    let opened = convert::vfs_path(&change.uri)
                        .map(|vfs_path| this.mem_docs.read().contains(&vfs_path))
                        .unwrap_or(false);
                    if opened {
                        if this.should_reload_config_for_path(&path) {
                            // e.g. `.elp_lint.toml`
                            this.refresh_config();
                        }
                        if this.should_reload_project_for_path(&path, &change) {
                            this.reload_manager.lock().add(path.clone());
                        }
                        this.eqwalizer_and_erlang_service_diagnostics_requested = true;
                        if this.config.eqwalizer().all {
                            this.eqwalizer_project_diagnostics_requested = true;
                        }
                        if this.config.edoc() {
                            this.edoc_diagnostics_requested = true;
                        }
                        this.ct_diagnostics_requested = true;
                        this.native_diagnostics_requested = true;
                    } else {
                        this.vfs_loader.handle.invalidate(path);
                    }
                };
                Ok(())
            })?
            .on::<notification::DidChangeWatchedFiles>(|this, params| {
                let changes: &[FileEvent] = &params.changes;
                let mut refresh_config = false;
                for change in changes {
                    if let Ok(path) = convert::abs_path(&change.uri) {
                        let opened = convert::vfs_path(&change.uri)
                            .map(|vfs_path| this.mem_docs.read().contains(&vfs_path))
                            .unwrap_or(false);
                        log::info!(target: FILE_WATCH_LOGGER_NAME, "DidChangeWatchedFiles:{}:{}", &opened, &path);
                        if !opened {
                            if this.should_reload_project_for_path(&path, change) {
                                this.reload_manager.lock().add(path.clone());
                            }
                            if this.should_reload_config_for_path(&path) {
                                // e.g. `.elp_lint.toml`
                                refresh_config = true;
                            }
                            this.vfs_loader.handle.invalidate(path);
                        }
                    }
                }
                if refresh_config {
                    this.refresh_config();
                }
                Ok(())
            })?
            .on::<notification::DidChangeConfiguration>(|this, _params| {
                // As stated in https://github.com/microsoft/language-server-protocol/issues/676,
                // this notification's parameters should be ignored and the actual config queried separately.
                this.refresh_config();

                Ok(())
            })?
            .on::<notification::SetTrace>(|_, _| {
                // Nothing to do for now
                Ok(())
            })?
            .finish();

        Ok(())
    }

    fn should_reload_project_for_path(&self, path: &AbsPath, change: &FileEvent) -> bool {
        let path_ref: &Path = path.as_ref();
        let file_name = path.file_stem();
        let ext = path.extension();
        let result = match (file_name, ext) {
            (Some("BUCK"), None) => true,
            (Some("TARGETS"), None) => true,
            (Some("TARGETS"), Some("v2")) => true,
            (Some("rebar"), Some("config")) => true,
            (Some("rebar.config"), Some("script")) => true,
            (Some(".elp"), Some("toml")) => true,
            (Some(file), Some("erl"))
                if change.typ == FileChangeType::CREATED && file.ends_with("_SUITE") =>
            {
                true
            }
            _ => false,
        };
        result && path_ref.is_file()
    }

    fn should_reload_config_for_path(&self, path: &AbsPath) -> bool {
        let path_ref: &Path = path.as_ref();
        let file_name = path.file_stem();
        let ext = path.extension();
        let result = match (file_name, ext) {
            (Some(".elp_lint"), Some("toml")) => true,
            _ => false,
        };
        result && path_ref.is_file()
    }

    fn on_loader_progress(
        &mut self,
        n_total: usize,
        progress: LoadingProgress,
        config_version: u32,
    ) {
        // report progress
        always!(config_version <= self.vfs_config_version);
        match progress {
            LoadingProgress::Started => {
                let pb = self
                    .progress
                    .begin_bar_with_telemetry("Loading source files".into(), Some(n_total));
                self.transition(Status::Loading(pb));
            }
            LoadingProgress::Progress(n_done) => {
                if let Status::Loading(pb) = &self.status {
                    pb.report(n_done, n_total);
                }
            }
            LoadingProgress::Finished => {
                if n_total == 0 {
                    let params = lsp_types::ShowMessageParams {
                        typ: lsp_types::MessageType::WARNING,
                        message: "Loaded zero files, check your project config".to_owned(),
                    };
                    self.show_message(params);
                }
                self.transition(Status::Running);
                self.schedule_compile_deps();
                self.schedule_cache();
                // Not all clients send config in the `initialize` message, request it
                self.refresh_config();
                self.refresh_lens();
                if !self.unresolved_app_id_paths.is_empty() {
                    log::warn!(
                        "Loading finished with {} unresolved app ID paths",
                        self.unresolved_app_id_paths.len()
                    );
                }
            }
        }
    }

    fn on_loader_loaded(&mut self, files: Vec<(AbsPathBuf, Option<Vec<u8>>)>, changed: bool) {
        let mut vfs = self.vfs.write();
        for (path, contents) in files {
            let path = VfsPath::from(path);
            let opened = self.mem_docs.read().contains(&path);
            if changed {
                log::info!(target: FILE_WATCH_LOGGER_NAME, "VFS change:{}:{}", &opened, &path);
            }
            if !opened {
                // This call will add the file to the changed_files, picked
                // up in `process_changes`.
                vfs.set_file_contents(path, contents);
            }
        }
    }

    fn process_changes_to_vfs_store(&mut self) -> (bool, u32) {
        // We need to guard against a file being created/modified and
        // then deleted within a change processing cycle. This is
        // problematic because the task generating the vfs changes
        // reported in `vfs.take_changes()` immediately updates the
        // file contents, setting it to None for a delete. When we are
        // processing an earlier change, the underlying vfs has
        // already deleted it, and we will get a panic trying to call
        // `vfs.file_contents()` for it.
        // The protection takes two forms
        // 1. Make sure we lock vfs for the duration of processing the
        //    changes, so we are not victim of a delete we have not yet
        //    been notified of.
        // 2. Make sure that the file actually has content when we try
        //    to process it.

        let mut guard = self.vfs.write();
        let mut changed_files = guard.take_changes();
        let docs = mem::take(&mut self.newly_opened_documents);
        for change in docs {
            match changed_files.entry(change.file_id) {
                Entry::Occupied(_) => {}
                Entry::Vacant(v) => {
                    v.insert(change);
                }
            }
        }

        if changed_files.is_empty() && !self.reset_source_roots {
            return (false, 0);
        }

        // downgrade to read lock to allow more readers while we are processing the changes
        let guard = RwLockWriteGuard::downgrade_to_upgradable(guard);
        let vfs: &Vfs = &guard;

        let raw_database = self.analysis_host.raw_database_mut();

        // The writes to salsa as these changes are applied below will
        // trigger Cancellation any pending processing.  This makes
        // sure all calculations see a consistent view of the
        // database.

        let mut highest_file_id: u32 = 0;
        for (_, file) in &changed_files {
            highest_file_id = highest_file_id.max(file.file_id.index());
            let file_exists = vfs.exists(file.file_id);

            if file.change != vfs::Change::Delete && file_exists {
                // Temporary for T183487471
                let _pctx = stdx::panic_context::enter(format!(
                    "\nserver::process_changes_to_vfs_store:{:?}:{:?}",
                    &file.file_id, &file.change
                ));
                if let vfs::Change::Create(v, _) | vfs::Change::Modify(v, _) = &file.change {
                    let document = Document::from_bytes(v);
                    let (text, line_endings) = document.vfs_to_salsa();
                    raw_database.set_file_text(file.file_id, Arc::from(text));
                    self.line_ending_map
                        .write()
                        .insert(file.file_id, line_endings);
                }
                if let Some(path) = vfs.file_path(file.file_id).as_path() {
                    if let Some(app_data_id) = self.unresolved_app_id_paths.get(&path.to_path_buf())
                    {
                        set_app_data_id_by_file(raw_database, file.file_id, *app_data_id);
                        // This is not really necessary, but we do it
                        // to be able to check that we resolve them
                        // all eventually
                        Arc::make_mut(&mut self.unresolved_app_id_paths)
                            .remove(&path.to_path_buf());
                    }
                }

                // causes us to remove stale squiggles from the UI.
                // We remove all that are only updated on save
                Arc::make_mut(&mut self.diagnostics).set_eqwalizer(file.file_id, vec![]);
                Arc::make_mut(&mut self.diagnostics)
                    .set_eqwalizer_project(vec![(file.file_id, vec![])]);
                Arc::make_mut(&mut self.eqwalizer_types).insert(file.file_id, Arc::new(vec![]));
                Arc::make_mut(&mut self.diagnostics)
                    .set_erlang_service(file.file_id, LabeledDiagnostics::default());
                Arc::make_mut(&mut self.diagnostics).set_edoc(file.file_id, vec![]);
                Arc::make_mut(&mut self.diagnostics).set_ct(file.file_id, vec![]);
            } else {
                // We can't actually delete things from salsa, just set it to empty
                raw_database.set_file_text(file.file_id, Arc::from(""));
            };
        }

        if self.update_app_data_ids {
            // We process the changes for files opened before project
            // discovery is complete, so cannot update their app_data_id (for
            // Buck projects). When the information *is* available, update
            // any missing ones.
            let vfs = self.vfs.read();
            let mut app_data_index: Arc<AppDataIndex> = raw_database.app_index();
            let mut paths_to_remove = vec![];
            self.unresolved_app_id_paths
                .iter()
                .for_each(|(path, app_data_id)| {
                    let vfs_path = VfsPath::from(path.clone());
                    if let Some((file_id, _)) = vfs.file_id(&vfs_path) {
                        paths_to_remove.push(path.clone());
                        Arc::make_mut(&mut app_data_index)
                            .map
                            .insert(file_id, *app_data_id);
                    }
                });
            raw_database.set_app_index(app_data_index);
            for remove in paths_to_remove {
                Arc::make_mut(&mut self.unresolved_app_id_paths).remove(&remove);
            }
            self.update_app_data_ids = false;
        }

        if self.reset_source_roots
            || changed_files
                .into_values()
                .any(|file| file.is_created_or_deleted())
        {
            let sets = self.file_set_config.partition(vfs);
            for (idx, set) in sets.into_iter().enumerate() {
                let root_id = SourceRootId(idx as u32);
                for file_id in set.iter() {
                    raw_database.set_file_source_root(file_id, root_id);
                }
                let root = SourceRoot::new(set);
                raw_database.set_source_root(root_id, Arc::new(root));
            }
            self.reset_source_roots = false;
        }

        (true, highest_file_id)
    }

    fn opened_documents(&self) -> Vec<FileId> {
        let vfs = self.vfs.read();
        self.mem_docs
            .read()
            .iter()
            .map(|path| vfs.file_id(path).unwrap().0)
            .collect()
    }

    fn update_native_diagnostics(&mut self) {
        let opened_documents = self.opened_documents();
        let snapshot = self.snapshot();

        let include_otp = self.config.enable_otp_diagnostics();
        self.task_pool.handle.spawn(move || {
            let diagnostics = opened_documents
                .into_iter()
                .filter_map(|file_id| {
                    Some((file_id, snapshot.native_diagnostics(file_id, include_otp)?))
                })
                .collect();

            Task::NativeDiagnostics(diagnostics)
        });
    }

    fn native_diagnostics_completed(&mut self, diags: Vec<(FileId, LabeledDiagnostics)>) {
        for (file_id, diagnostics) in diags {
            Arc::make_mut(&mut self.diagnostics).set_native(file_id, diagnostics);
        }
    }

    fn update_eqwalizer_diagnostics(&mut self) {
        if self.status != Status::Running {
            return;
        }

        log::info!("Recomputing EqWAlizer diagnostics");

        let opened_documents = self.opened_documents();
        let snapshot = self.snapshot();

        let spinner = self.progress.begin_spinner("EqWAlizing".to_string());

        let include_otp = self.config.enable_otp_diagnostics();
        self.task_pool.handle.spawn(move || {
            let diagnostics_types = opened_documents
                .into_iter()
                .filter_map(|file_id| {
                    let diags = snapshot
                        .eqwalizer_diagnostics(file_id, include_otp)
                        .unwrap_or_default();
                    let types = snapshot
                        .eqwalizer_types(file_id, include_otp)
                        .unwrap_or_default();
                    if diags.is_empty() && types.is_empty() {
                        None
                    } else {
                        Some((file_id, diags, types))
                    }
                })
                .collect();

            Task::EqwalizerDiagnostics(spinner, diagnostics_types)
        });
    }

    fn update_eqwalizer_project_diagnostics(&mut self) {
        if self.status != Status::Running || !self.eqwalize_all_completed {
            return;
        }

        log::info!("Recomputing EqWAlizer (project-wide) diagnostics");

        let snapshot = self.snapshot();
        let spinner = self
            .progress
            .begin_spinner("EqWAlizing All (project-wide)".to_string());
        let max_tasks = self.config.eqwalizer().max_tasks;

        self.eqwalizer_pool.handle.spawn(move || {
            let diagnostics = snapshot
                .projects
                .iter()
                .enumerate()
                .filter_map(|(id, _project)| {
                    let project_id = ProjectId(id as u32);
                    Some((
                        project_id,
                        snapshot.eqwalizer_project_diagnostics(project_id, max_tasks)?,
                    ))
                })
                .collect();

            Task::EqwalizerProjectDiagnostics(spinner, diagnostics)
        });
    }

    fn update_edoc_diagnostics(&mut self) {
        if self.status != Status::Running {
            return;
        }

        log::info!("Recomputing EDoc diagnostics");

        let opened_documents = self.opened_documents();
        let snapshot = self.snapshot();

        let spinner = self.progress.begin_spinner("EDoc".to_string());

        let config = self.diagnostics_config.clone();
        let include_otp = self.config.enable_otp_diagnostics();
        self.task_pool.handle.spawn(move || {
            let diagnostics = opened_documents
                .into_iter()
                .filter_map(|file_id| snapshot.edoc_diagnostics(file_id, include_otp, &config))
                .flatten()
                .collect();

            Task::EdocDiagnostics(spinner, diagnostics)
        });
    }

    fn update_ct_diagnostics(&mut self) {
        if self.status != Status::Running {
            return;
        }

        log::info!("Recomputing CT diagnostics");

        let opened_documents = self.opened_documents();
        let snapshot = self.snapshot();

        let spinner = self.progress.begin_spinner("Common Test".to_string());

        let config = self.diagnostics_config.clone();
        self.task_pool.handle.spawn(move || {
            let diagnostics = opened_documents
                .into_iter()
                .filter_map(|file_id| Some((file_id, snapshot.ct_diagnostics(file_id, &config)?)))
                .collect();

            Task::CommonTestDiagnostics(spinner, diagnostics)
        });
    }

    #[allow(clippy::type_complexity)]
    fn eqwalizer_diagnostics_completed(
        &mut self,
        diags_types: Vec<(FileId, Vec<diagnostics::Diagnostic>, Arc<Vec<(Pos, Type)>>)>,
    ) {
        let highlight_dynamic = self.config.highlight_dynamic();
        for (file_id, diagnostics, types) in diags_types {
            Arc::make_mut(&mut self.diagnostics).set_eqwalizer(file_id, diagnostics);
            if highlight_dynamic {
                Arc::make_mut(&mut self.eqwalizer_types).insert(file_id, types);
            }
        }
        if highlight_dynamic {
            self.refresh_semantic_tokens();
        }
    }

    #[allow(clippy::type_complexity)]
    fn eqwalizer_project_diagnostics_completed(
        &mut self,
        diags: Vec<(ProjectId, Vec<(FileId, Vec<diagnostics::Diagnostic>)>)>,
    ) {
        for (_project_id, diagnostics) in diags {
            Arc::make_mut(&mut self.diagnostics).set_eqwalizer_project(diagnostics);
        }
    }

    fn edoc_diagnostics_completed(&mut self, diags: Vec<(FileId, Vec<diagnostics::Diagnostic>)>) {
        for (file_id, diagnostics) in diags {
            Arc::make_mut(&mut self.diagnostics).set_edoc(file_id, diagnostics);
        }
    }

    fn ct_diagnostics_completed(&mut self, diags: Vec<(FileId, Vec<diagnostics::Diagnostic>)>) {
        for (file_id, diagnostics) in diags {
            Arc::make_mut(&mut self.diagnostics).set_ct(file_id, diagnostics);
        }
    }

    fn update_erlang_service_diagnostics(&mut self) {
        if self.status != Status::Running {
            return;
        }

        log::info!("Recomputing Erlang Service diagnostics");

        let opened_documents = self.opened_documents();
        let snapshot = self.snapshot();
        let supported_opened_documents: Vec<FileId> = opened_documents
            .into_iter()
            .filter(|file_id| is_supported_by_erlang_service(&snapshot.analysis, *file_id))
            .collect();
        let diagnostics_config = self.diagnostics_config.clone();
        self.task_pool.handle.spawn(move || {
            let diagnostics = supported_opened_documents
                .into_iter()
                .filter_map(|file_id| {
                    snapshot.erlang_service_diagnostics(file_id, &diagnostics_config)
                })
                .flatten()
                .collect();

            Task::ErlangServiceDiagnostics(diagnostics)
        });
    }

    fn erlang_service_diagnostics_completed(&mut self, diags: Vec<(FileId, LabeledDiagnostics)>) {
        for (file_id, diagnostics) in diags {
            Arc::make_mut(&mut self.diagnostics).set_erlang_service(file_id, diagnostics);
        }
    }

    fn switch_workspaces(&mut self, spinner: &Spinner, new_projects: Vec<Project>) -> Result<()> {
        if new_projects.is_empty() {
            log::info!("nothing new, not switching workspaces");
            return Ok(());
        }
        log::info!("will switch workspaces");

        let mut projects: Vec<Project> = self.projects.iter().cloned().collect();
        for project in new_projects {
            let idx = projects.iter().enumerate().find_map(|(id, p)| {
                if p.root() == project.root() {
                    Some(id)
                } else {
                    None
                }
            });
            match idx {
                Some(idx) => projects[idx] = project,
                None => projects.push(project),
            }
        }

        let raw_db = self.analysis_host.raw_database_mut();
        raw_db.clear_erlang_services();

        let project_apps = ProjectApps::new(&projects, IncludeOtp::Yes);
        spinner.report("Gathering file paths".to_string());
        let folders = ProjectFolders::new(&project_apps);
        // We will set the FileId -> AppDataIndex structure when the file
        // loads
        self.unresolved_app_id_paths =
            Arc::new(project_apps.app_structure().apply(raw_db, &|_path| None));

        self.file_set_config = folders.file_set_config;

        let register_options = lsp_types::DidChangeWatchedFilesRegistrationOptions {
            watchers: folders.watch,
        };

        let registrations = vec![lsp_types::Registration {
            id: "workspace/didChangeWatchedFiles".to_string(),
            method: notification::DidChangeWatchedFiles::METHOD.to_string(),
            register_options: Some(serde_json::to_value(register_options).unwrap()),
        }];

        self.send_request::<request::RegisterCapability>(
            lsp_types::RegistrationParams { registrations },
            |_, _| Ok(()),
        );

        let vfs_loader_config = loader::Config {
            load: folders.load,
            watch: vec![],
            version: 0,
        };
        self.vfs_loader.handle.set_config(vfs_loader_config);

        self.projects = Arc::new(projects);
        self.project_loader.lock().load_completed();
        self.reset_source_roots = true;
        self.update_app_data_ids = true;
        Ok(())
    }

    pub fn refresh_lens(&mut self) {
        self.send_request::<request::CodeLensRefresh>((), |_, _| Ok(()));
    }

    pub fn refresh_semantic_tokens(&mut self) {
        if self.config.refresh_semantic_tokens() {
            self.send_request::<request::SemanticTokensRefresh>((), |_, _| Ok(()));
        }
    }

    pub fn refresh_config(&mut self) {
        self.send_request::<request::WorkspaceConfiguration>(
            lsp_types::ConfigurationParams {
                items: vec![lsp_types::ConfigurationItem {
                    scope_uri: None,
                    section: Some("elp".to_string()),
                }],
            },
            |this, resp| {
                log::debug!("config update response: '{:?}", resp);
                let lsp_server::Response { error, result, .. } = resp;

                match (error, result) {
                    (Some(err), _) => {
                        log::error!("failed to fetch the server settings: {:?}", err)
                    }
                    (None, Some(mut configs)) => {
                        if let Some(json) = configs.get_mut(0) {
                            // Note that json can be null according to the spec if the client can't
                            // provide a configuration. This is handled in Config::update below.
                            let mut config = Config::clone(&*this.config);
                            config.update(json.take());
                            this.update_configuration(config);
                        }
                    }
                    (None, None) => {
                        log::error!("received empty server settings response from the client")
                    }
                }

                Ok(())
            },
        );
    }

    fn update_configuration(&mut self, config: Config) {
        let _p = tracing::info_span!("Server::update_configuration").entered();
        let _old_config = mem::replace(&mut self.config, Arc::new(config));

        self.logger
            .reconfigure(LOGGER_NAME, self.config.log_filter());
        self.logger.reconfigure("default", self.config.log_filter());

        // Read the lint config file
        let loader = self.project_loader.clone();
        let loader = loader.lock();
        // The OTP root is added with a project root value of None, skip it
        if let Some((path, _)) = loader.project_roots.iter().find(|(_k, v)| v.is_some()) {
            let path_buf: PathBuf = path.clone().into();
            if let Ok(lint_config) = read_lint_config_file(&path_buf, &None) {
                log::warn!("update_configuration: read lint file: {:?}", lint_config);
                self.lint_config = Arc::new(lint_config);

                // Diagnostic config may have changed, regen
                self.native_diagnostics_requested = true;
                self.eqwalizer_and_erlang_service_diagnostics_requested = true;
                if self.config.edoc() {
                    self.edoc_diagnostics_requested = true;
                }
            }
        }
        self.diagnostics_config = Arc::new(self.make_diagnostics_config());
    }

    fn make_diagnostics_config(&self) -> DiagnosticsConfig {
        self.config
            .diagnostics_config(self.lint_config.clone(), INCLUDE_GENERATED)
    }

    fn transition(&mut self, status: Status) {
        if self.status != status {
            log::info!("transitioning from {:?} to {:?}", self.status, status);
            self.status = status;
            if self.config.server_status_notification() {
                self.send_notification::<lsp_ext::StatusNotification>(lsp_ext::StatusParams {
                    status: self.status.as_lsp_status(),
                });
            }
        }
    }

    fn show_message(&mut self, params: ShowMessageParams) {
        self.send_notification::<lsp_types::notification::ShowMessage>(params)
    }

    fn show_message_request(&mut self, params: ShowMessageRequestParams) {
        self.send_request::<request::ShowMessageRequest>(params, |this, resp| {
            if let Some(res) = resp.result {
                if let Ok(hm) = serde_json::from_value::<HashMap<String, String>>(res) {
                    if let Some(url) = hm.get("URL") {
                        if let Ok(uri) = Url::from_str(url) {
                            this.send_request::<request::ShowDocument>(
                                ShowDocumentParams {
                                    uri,
                                    external: Some(true),
                                    take_focus: Some(true),
                                    selection: None,
                                },
                                |_, _| Ok(()),
                            );
                        }
                    }
                }
            }

            Ok(())
        });
    }

    fn send_response(&mut self, response: Response) {
        if let Some((method, request_timer)) = self.req_queue.incoming.complete(&response.id) {
            log::debug!("response {}#{}: {:?}", method, response.id, response);
            // logs time to complete request
            drop(request_timer);
            self.send(response.into());
        }
    }

    fn cancel(&mut self, request_id: RequestId) {
        if let Some(response) = self.req_queue.incoming.cancel(request_id) {
            // Temporary for T180205228 / #17
            let _pctx = stdx::panic_context::enter("\nserver::cancel".to_string());
            self.send(response.into());
        }
    }

    fn send_request<R: request::Request>(&mut self, params: R::Params, handler: ReqHandler) {
        let request = self
            .req_queue
            .outgoing
            .register(R::METHOD.to_string(), params, handler);
        // Temporary for T180205228 / #17
        let _pctx = stdx::panic_context::enter("\nserver::send_request".to_string());
        self.send(request.into());
    }

    fn complete_request(&mut self, response: Response) -> Result<()> {
        if let Some(handler) = self.req_queue.outgoing.complete(response.id.clone()) {
            handler(self, response)?;
        }
        Ok(())
    }

    fn send_notification<N: notification::Notification>(&mut self, params: N::Params) {
        let not = Notification::new(N::METHOD.to_string(), params);
        // Temporary for T180205228 / #17
        let _pctx = stdx::panic_context::enter("\nserver::send:notification".to_string());
        self.send(not.into());
    }

    fn send(&self, message: lsp_server::Message) {
        let _pctx = stdx::panic_context::enter(format!(
            "\nserver::send:msg={}",
            lsp_msg_for_context(&message)
        ));
        match self.connection.sender.send(message) {
            Ok(_) => {}
            Err(err) => {
                if self.status != Status::ShuttingDown {
                    // We have set the panic context to include the
                    // message, no need to repeat here.
                    // This way we do not have to clone it on the send
                    panic!("Could not send message, got: {}", err);
                }
            }
        }
    }

    fn register_request(&mut self, request: &Request, received_timer: TimeIt) {
        self.req_queue
            .incoming
            .register(request.id.clone(), (request.method.clone(), received_timer))
    }

    fn reload_project(&mut self, paths: FxHashSet<AbsPathBuf>) {
        if !paths.is_empty() {
            let loader = self.project_loader.clone();
            let query_config = self.config.buck_query();
            let spinner = self
                .progress
                .begin_spinner_with_telemetry("ELP reloading project config".to_string());
            self.project_pool.handle.spawn_with_sender({
                move |sender| {
                    let mut loader = loader.lock();
                    let mut projects = vec![];
                    if loader.clear(&paths) {
                        for path in paths {
                            let manifest = loader.load_manifest_if_new(&path);
                            if let Some((elp_config, main, fallback)) = manifest {
                                if let Ok(project) = Server::load_project_or_fallback(
                                    &path,
                                    elp_config,
                                    main,
                                    fallback,
                                    &sender,
                                    &query_config,
                                    &spinner,
                                ) {
                                    projects.push(project);
                                }
                            }
                        }
                        log::info!("did reload projects");
                        log::debug!("reloaded projects {:?}", &projects);
                    }
                    sender.send(Task::FetchProject(spinner, projects)).unwrap();
                }
            });
        }
    }

    fn load_project_or_fallback(
        path: &AbsPath,
        elp_config: ElpConfig,
        main: Result<ProjectManifest>,
        fallback: ProjectManifest,
        sender: &Sender<Task>,
        query_config: &BuckQueryConfig,
        spinner: &Spinner,
    ) -> Result<Project> {
        let mut fallback_used = false;
        let mut errors = vec![];
        let manifest = match main {
            Ok(main) => main,
            Err(err) => {
                log::error!(
                    "Failed to discover manifest for path: {:?}, error: {:?}",
                    path,
                    err
                );
                fallback_used = true;
                errors.push((err.to_string(), None));
                fallback.clone()
            }
        };

        spinner.report("Loading project".to_string());
        let mut project = Project::load(
            &manifest,
            elp_config.eqwalizer.clone(),
            query_config,
            &|message| spinner.report(message.to_string()),
        );
        if let Err(err) = &project {
            log::error!(
                "Failed to load project for manifest {:?}, error: {:?}",
                manifest,
                err
            );
            match err.downcast_ref::<BuckQueryError>() {
                Some(e) => {
                    errors.push((
                        "Project Initialisation Failed: invalid or missing buck 2 configuration"
                            .to_string(),
                        e.buck_ui_url.clone(),
                    ));
                }
                None => {
                    errors.push((err.to_string(), None));
                }
            }
            if !fallback_used {
                project =
                    Project::load(&fallback, elp_config.eqwalizer, query_config, &|message| {
                        spinner.report(message.to_string())
                    });
                if let Err(err) = &project {
                    log::error!(
                        "Failed to load project for fallback manifest {:?}, error: {:?}",
                        manifest,
                        err
                    );
                    errors.push((err.to_string(), None));
                }
            }
        }
        for (err, uri) in errors {
            if let Some(uri) = uri {
                let params = lsp_types::ShowMessageRequestParams {
                    typ: lsp_types::MessageType::ERROR,
                    message: err,
                    actions: Some(vec![MessageActionItem {
                        title: "Open Buck UI".to_string(),
                        properties: HashMap::from_iter(vec![(
                            "URL".to_string(),
                            MessageActionItemProperty::String(uri),
                        )]),
                    }]),
                };
                sender.send(Task::ShowMessageRequest(params))?;
            } else {
                let params = lsp_types::ShowMessageParams {
                    typ: lsp_types::MessageType::ERROR,
                    message: err,
                };
                sender.send(Task::ShowMessage(params))?;
            }
        }
        spinner.report("Project config loaded".to_string());
        project
    }

    fn fetch_projects_if_needed(&mut self, path: &AbsPath) {
        let path = path.to_path_buf();
        let loader = self.project_loader.clone();
        let query_config = self.config.buck_query();
        let spinner = self
            .progress
            .begin_spinner_with_telemetry("ELP loading project config".to_string());
        self.project_pool.handle.spawn_with_sender({
            move |sender| {
                let manifest = loader.lock().load_manifest_if_new(&path);
                let project = match manifest {
                    Some((elp_config, main, fallback)) => Server::load_project_or_fallback(
                        &path,
                        elp_config,
                        main,
                        fallback,
                        &sender,
                        &query_config,
                        &spinner,
                    ),
                    None => return,
                };

                log::info!("did fetch project");
                log::debug!("fetched projects {:?}", project);
                if let Ok(project) = project {
                    sender
                        .send(Task::FetchProject(spinner, vec![project]))
                        .unwrap();
                }
            }
        })
    }

    fn fetch_project_completed(&mut self, spinner: &Spinner, projects: Vec<Project>) -> Result<()> {
        if self.reload_manager.lock().has_changed_files() {
            // There are other changed files, abort this reload, to
            // allow the next one.
            return Ok(());
        }
        if let Err(err) = self.switch_workspaces(spinner, projects) {
            let params = lsp_types::ShowMessageParams {
                typ: lsp_types::MessageType::ERROR,
                message: err.to_string(),
            };
            self.show_message(params);
        }

        self.register_dynamic_now_operational();
        Ok(())
    }

    fn register_dynamic_now_operational(&mut self) {
        if text_document_symbols_dynamic_registration(&self.config.caps) {
            let registration_options = lsp_types::GenericRegistrationOptions {
                text_document_registration_options: lsp_types::TextDocumentRegistrationOptions {
                    document_selector: None,
                },
                options: lsp_types::GenericOptions {
                    work_done_progress_options: lsp_types::WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                },
                static_registration_options: lsp_types::StaticRegistrationOptions { id: None },
            };

            // Note that although the `register_options` are empty, and setting
            // the field to None works for VS Code, it breaks the haskell elp_tests.
            // TODO: once they are removed, or the haskell LSP library is updated,
            //       set it to None instead. T209094420
            let register_document_symbols = lsp_types::Registration {
                id: request::DocumentSymbolRequest::METHOD.to_string(),
                method: request::DocumentSymbolRequest::METHOD.to_string(),
                register_options: Some(serde_json::to_value(registration_options).unwrap()),
            };

            self.send_request::<request::RegisterCapability>(
                lsp_types::RegistrationParams {
                    registrations: vec![register_document_symbols],
                },
                |_, rsp| {
                    if rsp.error.is_some() {
                        log::warn!("Dynamic registration failed, got {:?}", rsp);
                    }
                    Ok(())
                },
            )
        }
    }

    fn schedule_compile_deps(&mut self) {
        let snapshot = self.snapshot();

        let spinner = self
            .progress
            .begin_spinner_with_telemetry("ELP compiling dependencies for EqWAlizer".to_string());

        self.task_pool.handle.spawn_with_sender(move |sender| {
            snapshot.set_up_projects();

            sender.send(Task::CompileDeps(spinner)).unwrap();
        });
    }

    fn schedule_cache(&mut self) {
        if self.cache_scheduled {
            return;
        }
        let snapshot = self.snapshot();

        self.cache_pool.handle.spawn_with_sender(move |sender| {
            let mut files = vec![];
            for (i, _) in snapshot.projects.iter().enumerate() {
                let module_index = match snapshot.analysis.module_index(ProjectId(i as u32)) {
                    Ok(module_index) => module_index,
                    //rescheduling canceled
                    Err(_) => {
                        sender.send(Task::ScheduleCache).unwrap();
                        return;
                    }
                };

                for (_, _, file_id) in module_index.iter_own() {
                    files.push(file_id);
                }
            }
            sender.send(Task::UpdateCache(files)).unwrap();
        });
    }

    fn update_cache(&mut self, mut files: Vec<FileId>) {
        if files.is_empty() {
            self.cache_scheduled = true;
            if self.config.eqwalizer().all {
                for (i, _) in self.snapshot().projects.iter().enumerate() {
                    let project_id = ProjectId(i as u32);
                    self.schedule_eqwalize_all(project_id);
                }
            }
            return;
        }
        let snapshot = self.snapshot();
        let eqwalize_all = self.config.eqwalizer().all;
        self.cache_pool.handle.spawn_with_sender(move |sender| {
            while !files.is_empty() {
                let file_id = files.remove(files.len() - 1);
                match snapshot.update_cache_for_file(file_id, eqwalize_all) {
                    Ok(_) => {}
                    Err(_) => {
                        // Got canceled
                        files.push(file_id);
                        break;
                    }
                }
            }
            sender.send(Task::UpdateCache(files)).unwrap();
        });
    }

    fn schedule_eqwalize_all(&mut self, project_id: ProjectId) {
        if self.eqwalize_all_scheduled.contains(&project_id) {
            return;
        }
        let snapshot = self.snapshot();
        let project_name = match snapshot.get_project(project_id) {
            Some(project) => project.name(),
            None => "undefined".to_string(),
        };
        let message = format!("Eqwalize All ({})", project_name);
        let bar = self.progress.begin_bar(message, None);

        self.eqwalizer_pool.handle.spawn_with_sender(move |sender| {
            let mut files = vec![];
            let module_index = match snapshot.analysis.module_index(project_id) {
                Ok(module_index) => module_index,
                //rescheduling canceled
                Err(_) => {
                    sender.send(Task::ScheduleEqwalizeAll(project_id)).unwrap();
                    return;
                }
            };

            for (_, _, file_id) in module_index.iter_own() {
                match snapshot.analysis.should_eqwalize(file_id) {
                    Ok(true) => {
                        files.push(file_id);
                    }
                    Ok(false) => {}
                    Err(_) => {
                        sender.send(Task::ScheduleEqwalizeAll(project_id)).unwrap();
                        return;
                    }
                }
            }
            sender
                .send(Task::UpdateEqwalizeAll(
                    bar,
                    project_id,
                    project_name,
                    files,
                ))
                .unwrap();
        });
    }

    fn update_eqwalize_all(
        &mut self,
        bar: ProgressBar,
        project_id: ProjectId,
        project_name: String,
        mut files: Vec<FileId>,
    ) {
        if files.is_empty() {
            bar.end();
            self.eqwalize_all_scheduled.insert(project_id);
            if self.projects.len() == self.eqwalize_all_scheduled.len() {
                self.eqwalize_all_completed = true;
            }
            return;
        }
        let snapshot = self.snapshot();
        let chunk_size = self.config.eqwalizer().chunk_size;
        let max_tasks = self.config.eqwalizer().max_tasks;
        self.eqwalizer_pool.handle.spawn_with_sender(move |sender| {
            let total = files.len();
            let mut done = 0;
            while !files.is_empty() {
                let len = files.len();
                let file_ids = if chunk_size < len {
                    files.split_off(len - chunk_size)
                } else {
                    std::mem::take(&mut files)
                };
                if snapshot
                    .analysis
                    .eqwalizer_diagnostics_by_project(project_id, file_ids.clone(), max_tasks)
                    .is_err()
                {
                    //got canceled
                    for file_id in file_ids {
                        files.push(file_id);
                    }
                    break;
                } else {
                    done += file_ids.len();
                    bar.report(done, total);
                }
            }
            sender
                .send(Task::UpdateEqwalizeAll(
                    bar,
                    project_id,
                    project_name,
                    files,
                ))
                .unwrap();
        });
    }

    fn report_progress(&mut self, task: ProgressTask) {
        let params = match task {
            ProgressTask::BeginNotify(params) => {
                self.send_request::<request::WorkDoneProgressCreate>(
                    lsp_types::WorkDoneProgressCreateParams {
                        token: params.token.clone(),
                    },
                    |_, _| Ok(()),
                );
                params
            }
            ProgressTask::Notify(params) => params,
        };
        self.send_notification::<lsp_types::notification::Progress>(params);
    }

    fn on_telemetry(&mut self, message: TelemetryMessage) {
        match serde_json::to_value(message.clone()) {
            Ok(params) => self.send_notification::<lsp_types::notification::TelemetryEvent>(params),
            Err(err) => log::warn!(
                "Error serializing telemetry. message: {:?} error: {}",
                message,
                err
            ),
        }
    }

    fn record_highest_file_id(&mut self, latest_high: u32) {
        self.highest_file_id = self.highest_file_id.max(latest_high);
    }
}

fn lsp_msg_for_context(message: &lsp_server::Message) -> String {
    match message {
        lsp_server::Message::Request(m) => m.method.clone(),
        lsp_server::Message::Response(m) => format!("{}", m.id),
        lsp_server::Message::Notification(m) => m.method.clone(),
    }
}

fn parse_id(id: lsp_types::NumberOrString) -> RequestId {
    match id {
        lsp_types::NumberOrString::Number(id) => id.into(),
        lsp_types::NumberOrString::String(id) => id.into(),
    }
}

pub fn file_id_to_path(vfs: &Vfs, id: FileId) -> Result<AbsPathBuf> {
    let url = file_id_to_url(vfs, id);
    convert::abs_path(&url)
}

pub fn file_id_to_url(vfs: &Vfs, id: FileId) -> Url {
    let path = vfs.file_path(id);
    let path = path.as_path().unwrap();
    convert::url_from_abs_path(path)
}

pub fn is_supported_by_erlang_service(analysis: &Analysis, id: FileId) -> bool {
    match analysis.file_kind(id) {
        Ok(kind) => ERLANG_SERVICE_SUPPORTED_EXTENSIONS.contains(&kind),
        Err(_) => false,
    }
}
