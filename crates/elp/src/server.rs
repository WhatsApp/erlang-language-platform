/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::mem;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use always_assert::always;
use anyhow::bail;
use anyhow::Result;
use crossbeam_channel::select;
use crossbeam_channel::Receiver;
use crossbeam_channel::Sender;
use dispatch::NotificationDispatcher;
use elp_ai::AiCompletion;
use elp_ide::diagnostics;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::LabeledDiagnostics;
use elp_ide::diagnostics::LintsFromConfig;
use elp_ide::diagnostics_collection::DiagnosticCollection;
use elp_ide::elp_ide_db::elp_base_db::bump_file_revision;
use elp_ide::elp_ide_db::elp_base_db::loader;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::ChangeKind;
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
use elp_ide::erlang_service::CompileOption;
use elp_ide::Analysis;
use elp_ide::AnalysisHost;
use elp_log::telemetry;
use elp_log::telemetry::TelemetryMessage;
use elp_log::timeit;
use elp_log::timeit_exceeds;
use elp_log::Logger;
use elp_log::TimeIt;
use elp_project_model::ElpConfig;
use elp_project_model::Project;
use elp_project_model::ProjectManifest;
use lsp_server::Connection;
use lsp_server::ErrorCode;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_server::Response;
use lsp_types;
use lsp_types::notification;
use lsp_types::notification::Notification as _;
use lsp_types::request;
use lsp_types::request::Request as _;
use lsp_types::FileChangeType;
use lsp_types::FileEvent;
use lsp_types::ShowMessageParams;
use lsp_types::Url;
use parking_lot::Mutex;
use parking_lot::RwLock;

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
use crate::project_loader::ProjectLoader;
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

const LOGGER_NAME: &str = "lsp";
const PARSE_SERVER_SUPPORTED_EXTENSIONS: &[FileKind] = &[
    FileKind::SrcModule,
    FileKind::TestModule,
    FileKind::Header,
    FileKind::Escript,
];
const EDOC_SUPPORTED_EXTENSIONS: &[FileKind] = &[FileKind::SrcModule, FileKind::TestModule];
const CT_SUPPORTED_EXTENSIONS: &[FileKind] = &[FileKind::TestModule];
const SLOW_DURATION: Duration = Duration::from_millis(300);

enum Event {
    Lsp(lsp_server::Message),
    Vfs(loader::Message),
    Task(Task),
    Telemetry(TelemetryMessage),
}

#[derive(Debug)]
pub enum Task {
    Response(lsp_server::Response),
    ShowMessage(lsp_types::ShowMessageParams),
    FetchProject(Vec<Project>),
    NativeDiagnostics(Vec<(FileId, LabeledDiagnostics)>),
    EqwalizerDiagnostics(Spinner, Vec<(FileId, Vec<diagnostics::Diagnostic>)>),
    EdocDiagnostics(Spinner, Vec<(FileId, Vec<diagnostics::Diagnostic>)>),
    CommonTestDiagnostics(Spinner, Vec<(FileId, Vec<diagnostics::Diagnostic>)>),
    ErlangServiceDiagnostics(Vec<(FileId, LabeledDiagnostics)>),
    CompileDeps(Spinner),
    Progress(ProgressTask),
    ScheduleCache,
    UpdateCache(ProgressBar, Vec<FileId>),
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
    Invalid,
}

impl Status {
    pub fn as_lsp_status(&self) -> lsp_ext::Status {
        match self {
            Status::Initialising => lsp_ext::Status::Loading,
            Status::Loading(_) => lsp_ext::Status::Loading,
            Status::Running => lsp_ext::Status::Running,
            Status::ShuttingDown => lsp_ext::Status::ShuttingDown,
            Status::Invalid => lsp_ext::Status::Invalid,
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

pub struct Server {
    connection: Connection,
    vfs_loader: VfsHandle,
    task_pool: TaskHandle,
    project_pool: TaskHandle,
    cache_pool: TaskHandle,
    diagnostics: Arc<DiagnosticCollection>,
    req_queue: ReqQueue,
    progress: ProgressManager,
    open_document_versions: SharedMap<VfsPath, i32>,
    newly_opened_documents: Vec<ChangedFile>,
    vfs: Arc<RwLock<Vfs>>,
    file_set_config: FileSetConfig,
    line_ending_map: SharedMap<FileId, LineEndings>,
    config: Arc<Config>,
    ad_hoc_lints: Arc<LintsFromConfig>,
    analysis_host: AnalysisHost,
    status: Status,
    projects: Arc<Vec<Project>>,
    project_loader: Arc<Mutex<ProjectLoader>>,
    eqwalizer_diagnostics_requested: bool,
    edoc_diagnostics_requested: bool,
    ct_diagnostics_requested: bool,
    cache_scheduled: bool,
    logger: Logger,
    ai_completion: Arc<Mutex<AiCompletion>>,
    include_generated: bool,
    compile_options: Vec<CompileOption>,

    // Progress reporting
    vfs_config_version: u32,
}

impl Server {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        connection: Connection,
        vfs_loader: VfsHandle,
        task_pool: TaskHandle,
        project_pool: TaskHandle,
        cache_pool: TaskHandle,
        logger: Logger,
        config: Config,
        ai_completion: AiCompletion,
    ) -> Server {
        let mut this = Server {
            connection,
            progress: ProgressManager::default(),
            vfs_loader,
            task_pool,
            project_pool,
            cache_pool,
            diagnostics: Arc::new(DiagnosticCollection::default()),
            req_queue: ReqQueue::default(),
            open_document_versions: SharedMap::default(),
            newly_opened_documents: Vec::default(),
            vfs: Arc::new(RwLock::new(Vfs::default())),
            file_set_config: FileSetConfig::default(),
            line_ending_map: SharedMap::default(),
            config: Arc::new(config.clone()),
            ad_hoc_lints: Arc::new(LintsFromConfig::default()),
            analysis_host: AnalysisHost::default(),
            status: Status::Initialising,
            projects: Arc::new(vec![]),
            project_loader: Arc::new(Mutex::new(ProjectLoader::new())),
            eqwalizer_diagnostics_requested: false,
            edoc_diagnostics_requested: false,
            ct_diagnostics_requested: false,
            cache_scheduled: false,
            logger,
            ai_completion: Arc::new(Mutex::new(ai_completion)),
            vfs_config_version: 0,
            include_generated: true,
            compile_options: vec![],
        };

        // Run config-based initialisation
        this.update_configuration(config);
        this
    }

    pub fn snapshot(&self) -> Snapshot {
        Snapshot::new(
            Arc::clone(&self.config),
            Arc::clone(&self.ad_hoc_lints),
            self.analysis_host.analysis(),
            Arc::clone(&self.diagnostics),
            Arc::clone(&self.vfs),
            Arc::clone(&self.open_document_versions),
            Arc::clone(&self.line_ending_map),
            Arc::clone(&self.projects),
            Arc::clone(&self.ai_completion),
        )
    }

    pub fn main_loop(mut self) -> Result<()> {
        if self.config.did_save_text_document_dynamic_registration() {
            let save_registration_options = lsp_types::TextDocumentSaveRegistrationOptions {
                include_text: Some(false),
                text_document_registration_options: lsp_types::TextDocumentRegistrationOptions {
                    document_selector: Some(vec![
                        lsp_types::DocumentFilter {
                            language: None,
                            scheme: None,
                            pattern: Some("**/*.{e,h}rl".to_string()),
                        },
                        lsp_types::DocumentFilter {
                            language: None,
                            scheme: None,
                            pattern: Some("**/rebar.{config,config.script,lock}".to_string()),
                        },
                        lsp_types::DocumentFilter {
                            language: None,
                            scheme: None,
                            pattern: Some("**/BUCK".to_string()),
                        },
                        lsp_types::DocumentFilter {
                            language: None,
                            scheme: None,
                            pattern: Some("**/TARGETS".to_string()),
                        },
                        lsp_types::DocumentFilter {
                            language: None,
                            scheme: None,
                            pattern: Some("**/TARGETS.v2".to_string()),
                        },
                    ]),
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
            let _timer = timeit_exceeds!("main_loop_health", SLOW_DURATION);
            self.handle_event(event)?;
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
                    loader::Message::Loaded { files } => self.on_loader_loaded(files),
                    loader::Message::Progress {
                        n_total,
                        n_done,
                        config_version,
                    } => self.on_loader_progress(n_total, n_done, config_version),
                }

                // Coalesce many VFS event into a single main loop turn
                msg = match self.vfs_loader.receiver.try_recv() {
                    Ok(msg) => msg,
                    Err(_) => break,
                }
            },
            Event::Task(mut task) => loop {
                match task {
                    Task::Response(response) => self.send_response(response),
                    Task::FetchProject(projects) => self.fetch_project_completed(projects)?,
                    Task::NativeDiagnostics(diags) => self.native_diagnostics_completed(diags),
                    Task::EqwalizerDiagnostics(spinner, diags) => {
                        spinner.end();
                        self.eqwalizer_diagnostics_completed(diags)
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
                        self.eqwalizer_diagnostics_requested = true;
                    }
                    Task::Progress(progress) => self.report_progress(progress),
                    Task::UpdateCache(spinner, files) => self.update_cache(spinner, files),
                    Task::ScheduleCache => self.schedule_cache(),
                    Task::ShowMessage(params) => self.show_message(params),
                }

                // Coalesce many tasks into a single main loop turn
                task = match self.task_pool.receiver.try_recv() {
                    Ok(task) => task,
                    Err(_) => break,
                }
            },
            Event::Telemetry(message) => self.on_telemetry(message),
        }

        if self.status == Status::ShuttingDown {
            return Ok(());
        }

        let changed = self.process_changes_to_vfs_store();

        if self.status == Status::Running {
            if changed {
                self.update_native_diagnostics();
            }

            if mem::take(&mut self.eqwalizer_diagnostics_requested) {
                self.update_eqwalizer_diagnostics();
                self.update_erlang_service_diagnostics();
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
            for file_id in diagnostic_changes {
                let url = file_id_to_url(&self.vfs.read(), file_id);
                let line_index = snapshot.analysis.line_index(file_id)?;
                let diagnostics = self
                    .diagnostics
                    .diagnostics_for(file_id)
                    .iter()
                    .map(|d| ide_to_lsp_diagnostic(&line_index, &url, d))
                    .collect();
                let version = convert::vfs_path(&url)
                    .map(|path| self.open_document_versions.read().get(&path).cloned())
                    .unwrap_or_default();

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
                let id = req.id.clone();
                self.send_response(Response::new_err(
                    id,
                    ErrorCode::ContentModified as i32,
                    "elp is still loading".to_string(),
                ));
                return Ok(());
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
                this.analysis_host.request_cancellation();
                Ok(())
            })?
            .on::<request::CodeActionRequest>(handlers::handle_code_action)
            .on::<request::CodeActionResolveRequest>(handlers::handle_code_action_resolve)
            .on::<request::GotoDefinition>(handlers::handle_goto_definition)
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
                this.eqwalizer_diagnostics_requested = true;
                this.edoc_diagnostics_requested = true;
                this.ct_diagnostics_requested = true;
                if let Ok(path) = convert::abs_path(&params.text_document.uri) {
                    this.fetch_projects_if_needed(&path);
                    let path = VfsPath::from(path);
                    if this
                        .open_document_versions
                        .write()
                        .insert(path.clone(), params.text_document.version)
                        .is_some()
                    {
                        log::error!("duplicate DidOpenTextDocument: {}", path);
                    }

                    let mut vfs = this.vfs.write();
                    vfs.set_file_contents(
                        path.clone(),
                        Some(params.text_document.text.into_bytes()),
                    );

                    // Until we bring over the full rust-analyzer
                    // style change processing, make a list of files
                    // that are freshly opened so diagnostics are
                    // generated for them, despite no changes being
                    // registered in vfs.
                    let file_id = vfs.file_id(&path).unwrap();
                    this.newly_opened_documents.push(ChangedFile {
                        file_id,
                        change_kind: ChangeKind::Modify,
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
                    match this.open_document_versions.write().get_mut(&path) {
                        Some(doc) => {
                            // The version passed in DidChangeTextDocument is the version after all edits are applied
                            // so we should apply it before the vfs is notified.
                            *doc = params.text_document.version;
                        }
                        None => {
                            log::error!("unexpected DidChangeTextDocument: {}", path);
                            return Ok(());
                        }
                    };
                    let mut vfs = this.vfs.write();
                    let file_id = vfs.file_id(&path).unwrap();
                    let mut document = Document::from_bytes(vfs.file_contents(file_id).to_vec());
                    document.apply_changes(params.content_changes);

                    vfs.set_file_contents(path, Some(document.into_bytes()));
                }
                Ok(())
            })?
            .on::<notification::DidCloseTextDocument>(|this, params| {
                if let Ok(path) = convert::vfs_path(&params.text_document.uri) {
                    if this.open_document_versions.write().remove(&path).is_none() {
                        log::error!("unexpected DidCloseTextDocument: {}", path);
                    }
                }

                // Clear the diagnostics for the previously known version of the file.
                this.send_notification::<lsp_types::notification::PublishDiagnostics>(
                    lsp_types::PublishDiagnosticsParams {
                        uri: params.text_document.uri,
                        diagnostics: Vec::new(),
                        version: None,
                    },
                );

                Ok(())
            })?
            .on::<notification::DidSaveTextDocument>(|this, params| {
                process_changed_files(
                    this,
                    &[FileEvent::new(
                        params.text_document.uri,
                        FileChangeType::CHANGED,
                    )],
                );
                Ok(())
            })?
            .on::<notification::DidChangeWatchedFiles>(|this, params| {
                process_changed_files(this, &params.changes);
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
        let file_name = path.file_stem().and_then(|name| name.to_str());
        let ext = path.extension().and_then(|ext| ext.to_str());
        let result = match (file_name, ext) {
            (Some("BUCK"), None) => true,
            (Some("TARGETS"), None) => true,
            (Some("TARGETS"), Some("v2")) => true,
            (Some("rebar"), Some("config")) => true,
            (Some("rebar.config"), Some("script")) => true,
            (Some(file), Some("erl"))
                if change.typ == FileChangeType::CREATED && file.ends_with("_SUITE") =>
            {
                true
            }
            _ => false,
        };
        result && path_ref.is_file()
    }

    fn on_loader_progress(&mut self, n_total: usize, n_done: usize, config_version: u32) {
        // report progress
        always!(config_version <= self.vfs_config_version);

        if n_total == 0 {
            self.transition(Status::Invalid);
        } else if n_done == 0 {
            let pb = self
                .progress
                .begin_bar("Applications loaded".into(), Some(n_total));
            self.transition(Status::Loading(pb));
        } else if n_done < n_total {
            if let Status::Loading(pb) = &self.status {
                pb.report(n_done, n_total);
            }
        } else {
            assert_eq!(n_done, n_total);
            self.transition(Status::Running);
            self.schedule_compile_deps();
            self.schedule_cache();
            // Not all clients send config in the `initialize` message, request it
            self.refresh_config();
            self.refresh_lens();
        }
    }

    fn on_loader_loaded(&mut self, files: Vec<(AbsPathBuf, Option<Vec<u8>>)>) {
        let mut vfs = self.vfs.write();
        for (path, contents) in files {
            let path = VfsPath::from(path);
            if !self.open_document_versions.read().contains_key(&path) {
                // This call will add the file to the changed_files, picked
                // up in `process_changes`.
                vfs.set_file_contents(path, contents);
            }
        }
    }

    fn process_changes_to_vfs_store(&mut self) -> bool {
        let changed_files = {
            // Don't hold write lock, while modifying db - this can lead to deadlocks!
            let mut vfs = self.vfs.write();
            let mut changed_files = vfs.take_changes();
            // Note: the append operations clears out self.newly_opened_documents too
            changed_files.append(&mut self.newly_opened_documents);
            changed_files
        };

        if changed_files.is_empty() {
            return false;
        }

        // The writes to salsa as these changes are applied below will
        // trigger Cancellation any pending processing.  This makes
        // sure all calculations see a consistent view of the
        // database.

        let vfs = self.vfs.read();
        let raw_database = self.analysis_host.raw_database_mut();

        for file in &changed_files {
            if file.exists() {
                let bytes = vfs.file_contents(file.file_id).to_vec();
                let document = Document::from_bytes(bytes);
                let (text, line_ending) = LineEndings::normalize(document.content);
                self.line_ending_map
                    .write()
                    .insert(file.file_id, line_ending);
                raw_database.set_file_text(file.file_id, Arc::from(text));
                // causes us to remove stale squiggles from the UI
                Arc::make_mut(&mut self.diagnostics).set_eqwalizer(file.file_id, vec![]);
            } else {
                // TODO (T105975906): Clean up stale .etf files

                // We can't actually delete things from salsa, just set it to empty
                raw_database.set_file_text(file.file_id, Arc::from(""));
            };
            if file.change_kind == ChangeKind::Create {
                raw_database.set_file_revision(file.file_id, 0);
            } else {
                bump_file_revision(file.file_id, raw_database);
            }
        }

        if changed_files
            .iter()
            .any(|file| file.is_created_or_deleted())
        {
            let sets = self.file_set_config.partition(&vfs);
            for (idx, set) in sets.into_iter().enumerate() {
                let root_id = SourceRootId(idx as u32);
                for file_id in set.iter() {
                    raw_database.set_file_source_root(file_id, root_id);
                }
                let root = SourceRoot::new(set);
                raw_database.set_source_root(root_id, Arc::new(root));
            }
        }

        true
    }

    fn opened_documents(&self) -> Vec<FileId> {
        let vfs = self.vfs.read();
        self.open_document_versions
            .read()
            .keys()
            .map(|path| vfs.file_id(path).unwrap())
            .collect()
    }

    fn update_native_diagnostics(&mut self) {
        let opened_documents = self.opened_documents();
        let snapshot = self.snapshot();

        self.task_pool.handle.spawn(move || {
            let diagnostics = opened_documents
                .into_iter()
                .filter_map(|file_id| Some((file_id, snapshot.native_diagnostics(file_id)?)))
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

        self.task_pool.handle.spawn(move || {
            let diagnostics = opened_documents
                .into_iter()
                .filter_map(|file_id| Some((file_id, snapshot.eqwalizer_diagnostics(file_id)?)))
                .collect();

            Task::EqwalizerDiagnostics(spinner, diagnostics)
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

        let supported_opened_documents: Vec<FileId> = opened_documents
            .into_iter()
            .filter(|file_id| is_supported_by_edoc(&snapshot.analysis, *file_id))
            .collect();
        self.task_pool.handle.spawn(move || {
            let diagnostics = supported_opened_documents
                .into_iter()
                .filter_map(|file_id| snapshot.edoc_diagnostics(file_id))
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

        let supported_opened_documents: Vec<FileId> = opened_documents
            .into_iter()
            .filter(|file_id| is_supported_by_ct(&snapshot.analysis, *file_id))
            .collect();
        self.task_pool.handle.spawn(move || {
            let diagnostics = supported_opened_documents
                .into_iter()
                .filter_map(|file_id| Some((file_id, snapshot.ct_diagnostics(file_id)?)))
                .collect();

            Task::CommonTestDiagnostics(spinner, diagnostics)
        });
    }

    fn eqwalizer_diagnostics_completed(
        &mut self,
        diags: Vec<(FileId, Vec<diagnostics::Diagnostic>)>,
    ) {
        for (file_id, diagnostics) in diags {
            Arc::make_mut(&mut self.diagnostics).set_eqwalizer(file_id, diagnostics);
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
            .filter(|file_id| is_supported_by_parse_server(&snapshot.analysis, *file_id))
            .collect();
        let diagnostics_config = DiagnosticsConfig::default()
            .set_include_generated(self.include_generated)
            .set_compile_options(self.compile_options.clone());
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

    fn switch_workspaces(&mut self, new_projects: Vec<Project>) -> Result<()> {
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
        let folders = ProjectFolders::new(&project_apps);
        project_apps.app_structure().apply(raw_db);

        for (project_id, _) in projects.iter().enumerate() {
            let project_id = ProjectId(project_id as u32);
            raw_db.ensure_erlang_service(project_id)?;
        }
        if let Some(otp_project_id) = project_apps.otp_project_id {
            raw_db.ensure_erlang_service(otp_project_id)?;
        }

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
        Ok(())
    }

    pub fn refresh_lens(&mut self) {
        self.send_request::<request::CodeLensRefresh>((), |_, _| Ok(()));
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
        let _p = profile::span("Server::update_configuration");
        let _old_config = mem::replace(&mut self.config, Arc::new(config));

        self.logger
            .reconfigure(LOGGER_NAME, self.config.log_filter());
        self.logger.reconfigure("default", self.config.log_filter());

        if self.config.enable_experimental_diagnostics() {
            // Read the lint config file
            let loader = self.project_loader.clone();
            let loader = loader.lock();
            //TODO not correct, there is always OTP path in project roots
            if let Some(path) = loader.project_roots.keys().next() {
                let ppp: PathBuf = path.clone().into();
                if let Ok(lint_config) = read_lint_config_file(&ppp, &None) {
                    log::warn!("update_configuration: read lint file: {:?}", lint_config);
                    self.ad_hoc_lints = Arc::new(lint_config.ad_hoc_lints);
                }
            }
        }
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

    fn send_response(&mut self, response: Response) {
        if let Some((method, request_timer)) = self.req_queue.incoming.complete(response.id.clone())
        {
            log::debug!("response {}#{}: {:?}", method, response.id, response);
            // logs time to complete request
            drop(request_timer);
            self.send(response.into());
        }
    }

    fn cancel(&mut self, request_id: RequestId) {
        if let Some(response) = self.req_queue.incoming.cancel(request_id) {
            // Temporary for T180205228 / #17
            let _pctx = stdx::panic_context::enter(format!("\nserver::cancel"));
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
        // Temporary for T180205228 / #17
        let _pctx = stdx::panic_context::enter(format!(
            "\nserver::send:msg={}",
            lsp_msg_for_context(&message)
        ));
        self.connection.sender.send(message).unwrap()
    }

    fn register_request(&mut self, request: &Request, received_timer: TimeIt) {
        self.req_queue
            .incoming
            .register(request.id.clone(), (request.method.clone(), received_timer))
    }

    fn reload_project(&mut self, paths: Vec<AbsPathBuf>) {
        let loader = self.project_loader.clone();
        if !paths.is_empty() {
            self.project_pool.handle.spawn_with_sender({
                move |sender| {
                    let mut loader = loader.lock();
                    if loader.clear(&paths) {
                        let mut projects = vec![];
                        for path in paths {
                            let manifest = loader.load_manifest_if_new(&path);
                            if let Some((elp_config, main, fallback)) = manifest {
                                if let Ok(project) = Server::load_project_or_fallback(
                                    &path, elp_config, main, fallback, &sender,
                                ) {
                                    projects.push(project);
                                }
                            }
                        }
                        log::info!("did reload projects");
                        log::debug!("reloaded projects {:?}", &projects);

                        sender.send(Task::FetchProject(projects)).unwrap();
                    }
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
                errors.push(err.to_string());
                fallback.clone()
            }
        };
        let mut project = Project::load(&manifest, elp_config.eqwalizer.clone());
        if let Err(err) = &project {
            log::error!(
                "Failed to load project for manifest {:?}, error: {:?}",
                manifest,
                err
            );
            errors.push(err.to_string());
            if !fallback_used {
                project = Project::load(&fallback, elp_config.eqwalizer);
                if let Err(err) = &project {
                    log::error!(
                        "Failed to load project for fallback manifest {:?}, error: {:?}",
                        manifest,
                        err
                    );
                    errors.push(err.to_string());
                }
            }
        }
        for err in errors {
            let params = lsp_types::ShowMessageParams {
                typ: lsp_types::MessageType::ERROR,
                message: err,
            };
            sender.send(Task::ShowMessage(params))?;
        }
        project
    }

    fn fetch_projects_if_needed(&mut self, path: &AbsPath) {
        let path = path.to_path_buf();
        let loader = self.project_loader.clone();
        self.project_pool.handle.spawn_with_sender({
            move |sender| {
                let manifest = loader.lock().load_manifest_if_new(&path);
                let project = match manifest {
                    Some((elp_config, main, fallback)) => {
                        Server::load_project_or_fallback(&path, elp_config, main, fallback, &sender)
                    }
                    None => return,
                };

                log::info!("did fetch project");
                log::debug!("fetched projects {:?}", project);
                if let Ok(project) = project {
                    sender.send(Task::FetchProject(vec![project])).unwrap();
                }
            }
        })
    }

    fn fetch_project_completed(&mut self, projects: Vec<Project>) -> Result<()> {
        if let Err(err) = self.switch_workspaces(projects) {
            let params = lsp_types::ShowMessageParams {
                typ: lsp_types::MessageType::ERROR,
                message: err.to_string(),
            };
            self.show_message(params);
        }
        Ok(())
    }

    fn schedule_compile_deps(&mut self) {
        let snapshot = self.snapshot();

        let spinner = self
            .progress
            .begin_spinner("ELP compiling dependencies for EqWAlizer".to_string());

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
        let bar = self
            .progress
            .begin_bar("Background parsing".to_string(), None);

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
            sender.send(Task::UpdateCache(bar, files)).unwrap();
        });
    }

    fn update_cache(&mut self, bar: ProgressBar, mut files: Vec<FileId>) {
        if files.is_empty() {
            bar.end();
            self.cache_scheduled = true;
            return;
        }
        let snapshot = self.snapshot();
        self.cache_pool.handle.spawn_with_sender(move |sender| {
            let total = files.len();
            let mut done = 0;
            while !files.is_empty() {
                let file_id = files.remove(files.len() - 1);
                if snapshot.analysis.def_map(file_id).is_err() {
                    //got canceled
                    files.push(file_id);
                    break;
                } else {
                    done += 1;
                    bar.report(done, total);
                }
            }
            sender.send(Task::UpdateCache(bar, files)).unwrap();
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
}

fn lsp_msg_for_context(message: &lsp_server::Message) -> String {
    match message {
        lsp_server::Message::Request(m) => m.method.clone(),
        lsp_server::Message::Response(m) => format!("{}", m.id),
        lsp_server::Message::Notification(m) => m.method.clone(),
    }
}

fn process_changed_files(this: &mut Server, changes: &[FileEvent]) {
    let mut to_reload = vec![];
    for change in changes {
        if let Ok(path) = convert::abs_path(&change.uri) {
            if this.should_reload_project_for_path(&path, change) {
                to_reload.push(path.clone());
            }
            let opened = convert::vfs_path(&change.uri)
                .map(|vfs_path| this.open_document_versions.read().contains_key(&vfs_path))
                .unwrap_or(false);
            if opened {
                // Bump the file revision so that the salsa cache
                // is invalidated for processes reading the
                // on-disk version
                let vfs = this.vfs.read();
                let vfs_path = VfsPath::from(path);
                if let Some(file_id) = vfs.file_id(&vfs_path) {
                    bump_file_revision(file_id, this.analysis_host.raw_database_mut());
                }
            } else {
                this.vfs_loader.handle.invalidate(path);
            }
        }
    }
    this.reload_project(to_reload);
    this.eqwalizer_diagnostics_requested = true;
    this.edoc_diagnostics_requested = true;
    this.ct_diagnostics_requested = true;
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

pub fn is_supported_by_parse_server(analysis: &Analysis, id: FileId) -> bool {
    match analysis.file_kind(id) {
        Ok(kind) => PARSE_SERVER_SUPPORTED_EXTENSIONS.contains(&kind),
        Err(_) => false,
    }
}

pub fn is_supported_by_edoc(analysis: &Analysis, id: FileId) -> bool {
    match analysis.file_kind(id) {
        Ok(kind) => EDOC_SUPPORTED_EXTENSIONS.contains(&kind),
        Err(_) => false,
    }
}

pub fn is_supported_by_ct(analysis: &Analysis, id: FileId) -> bool {
    match analysis.file_kind(id) {
        Ok(kind) => CT_SUPPORTED_EXTENSIONS.contains(&kind),
        Err(_) => false,
    }
}
