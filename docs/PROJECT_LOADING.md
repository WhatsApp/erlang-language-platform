# Investigating how the initial startup process happens in ELP LSP server

## Notes on server initialisation

### Initialize

It starts in `elp::bin::main::run_server`.

This calls `ServerSetup.to_server()`, which initiates the LSP
`initialize` process.

`ServerSetup::initialize()` prepares a response for the client with
the server capabilities and other info. It calls
`Connection::initialize_finish()` to send this to the client and wait
for the returned `initialized` notification. This allows the client to
start sending request messages to ELP

It then sets up a default configuration, and initiates project
discovery on the workspace roots provided in the initialization
message from the client.  This calls
`ProjectManifest::discover_buck_all` on the workspace roots, and if
that fails falls back to rebar3 with `ProjectManifest::discover_all`.
These are intended to be quick queries to identify that a project of
the given type exists.

The `Vec<ProjectManifest>` is stored in
`config.discovered_projects` and returned from
`ServerSetup::initialize()`.

When that ends it sets up the logger and calls
`server::setup::setup_server` with the config and logger. This creates
a vfs loader and a task pool, and returns the result of
`Server::new()`, which populates the `Server` data structure.

Part of the `Server` state setup is to set `Server::status` to
`Status::Initialising`.

It then enters `Server::main_loop()`.

### Main Loop Startup

The fetch projects operation request is signalled by a call to
`fetch_projects_request`, which signals to the
`Server::fetch_projects_queue` that there is work to be done.

This is immediately followed by `self.fetch_projects_if_needed()`.

If the `Server::fetch_projects_queue` flag is set (which it is, the
prior line call did it), spawn a task in the task pool to call
`project_model::Project::load` on all of them. This does the initial
project load process. For buck2, this does a series of queries to buck
to populate the `Project` structure, and similarly for legacy rebar3
projects, using the rebar `project_info` plugin.

When done, the task spawned by `fetch_projects_if_needed` sends
`Task::FetchProjects(projects)` to the main loop.

Once the loader task is spawned, the server main loop message pump
starts running, for the life of the server.

**So at this point the main loop is live, and the client is sending us
requests.**

**BUT WE HAVE NOT YET CONFIGURED OUR PROJECTS, AS WE ARE WAITING FOR
THE PROJECTS TO BE FETCHED**

### Main Loop Normal Operation

On entry the server is still in `Status::Initialising`.

The main loop message pump receives messages from the async tasks, vfs
loader, LSP client, and sub lsp, as well as a progress reporter.

It dispatches according to the message type, and then processes any
changes generated.

If an LSP request message comes in,

- if the status is `Status::Initialising` or `Status::Loading` it is
  forwarded to the sub lsp without being processed by ELP.
- if the status is `Status::ShuttingDown` an error response is
  returned.
- otherwise it is processed by ELP.

#### Project Fetching Completes

Eventually the project fetching completes, and injects a message into the
main event loop, handled as

```rust
Task::FetchProjects(projects) => self.fetch_projects_completed(projects)?,
```

`fetch_projects_completed` calls `switch_workspaces()`.

This is the place where the all the ELP project information is put
into place, except source roots.  This includes calling salsa
`set_app_data` and `set_project_data` for all discovered projects.

The final step is to activate the vfs file loader, according to a
config generated from the project info.

#### VFS File Loading

The Vfs file loader is an asynchronous task, and it sends messages as
it works, one to indicate a file is loaded, and one to indicate
progress on loading the initial watch list.  It seems that the
progress message is always sent after the loaded message for a given
file.

The Vfs loaded message is dispatched to `Server.on_loader_loaded`.

This calls `vfs.set_file_contents(path, contents);` for each loaded
file, which causes a change event to be stored in Vfs.

Peridically Vfs sends a message handled by
`Server.on_loader_progress()`.  This is used to initially set
`Status::Loading`, and when notification of the last file loaded
arrives, it transitions to `Status::Running`.  At the same time, it
schedules compilation of the dependencies required by Eqwalizer via
the `erlang_service`, by calling `self.schedule_compile_deps()`.

The normal main loop processing after handling the message picks up
the change, and at the end of `Server.process_changes` sets the source
roots for the given file.

So the process of receiving notification that a file is loaded,
setting its vfs contents and source root is atomic in terms of main
loop message handling.

### Places where server state changes

`Status::Initialising`: set in server startup
`Status::Loading`: set when the initial `on_loader_progress` message is processed.
`Status::Running`: set when the vfs loader has finished loading.
   THIS TRANSITION IS THE PROBLEMATIC ONE
`Status::ShuttingDown`: set if a shutdown request comes from the LSP client
