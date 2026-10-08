%% % @format
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is dual-licensed under either the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree or the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree. You may select, at your option, one of the
%%% above-listed licenses.

-module(eqwalizer_types).
-moduledoc """
This module provides a means to override types from standard OTP libraries for
better type-checking with eqWAlizer.

Every type is named 'module:type'. Helper types not present in OTP follow the
same convention and are marked as not exported.
""".

-export_type([
    'argparse:arg_map'/0,
    'ct_suite:ct_config'/0,
    'ct_suite:ct_group_def'/0,
    'digraph:edge'/0,
    'digraph:label'/0,
    'digraph:vertex'/0,
    'erl_parse:form_info'/0,
    'erl_syntax:annotation_or_location'/0,
    'erlang:process_info_result_item'/0,
    'erlang:stacktrace'/0,
    'gen_server:format_status'/0,
    'inet:module_socket'/0,
    'logger:filter_arg'/0,
    'logger:metadata'/0,
    'logger:report_cb'/0,
    'logger_handler:config'/0,
    'ssl:sslsocket'/0,
    'supervisor:startchild_err'/0,
    'sys:dbg_fun'/0
]).

%% -------- argparse --------

-type 'argparse:arg_map'() :: #{eqwalizer:dynamic() => eqwalizer:dynamic()}.

%% -------- ct_suite --------

-type 'ct_suite:ct_config'() :: [{Key :: atom(), Value :: eqwalizer:dynamic()}].

% Adds the {Name, Tests} form, which groups/0 may return.
-type 'ct_suite:ct_group_def'() ::
    {ct_suite:ct_groupname(), ['ct_suite:ct_group_entry'()]}
    | {ct_suite:ct_groupname(), 'ct_suite:ct_group_props'(), ['ct_suite:ct_group_entry'()]}.

% not exported
-type 'ct_suite:ct_group_entry'() ::
    ct_suite:ct_testname()
    | ct_suite:ct_group_def()
    | {group, ct_suite:ct_groupname()}
    | {testcase, ct_suite:ct_testname(), 'ct_suite:ct_testcase_repeat_prop'()}.

% not exported
-type 'ct_suite:ct_group_props'() :: [
    parallel
    | sequence
    | shuffle
    | {shuffle, Seed :: {integer(), integer(), integer()}}
    | {'ct_suite:ct_group_repeat_type'(), 'ct_suite:ct_test_repeat'()}
].

% not exported
-type 'ct_suite:ct_group_repeat_type'() ::
    repeat
    | repeat_until_all_ok
    | repeat_until_all_fail
    | repeat_until_any_ok
    | repeat_until_any_fail.

% not exported
-type 'ct_suite:ct_testcase_repeat_prop'() :: [
    {repeat, 'ct_suite:ct_test_repeat'()}
    | {repeat_until_ok, 'ct_suite:ct_test_repeat'()}
    | {repeat_until_fail, 'ct_suite:ct_test_repeat'()}
].

% not exported
-type 'ct_suite:ct_test_repeat'() :: integer() | forever.

%% -------- digraph --------

-type 'digraph:edge'() :: eqwalizer:dynamic().
-type 'digraph:label'() :: eqwalizer:dynamic().
-type 'digraph:vertex'() :: eqwalizer:dynamic().

%% -------- erl_parse --------

-type 'erl_parse:form_info'() :: eqwalizer:dynamic().

%% -------- erl_syntax --------

-type 'erl_syntax:annotation_or_location'() :: eqwalizer:dynamic().

%% -------- erlang --------

-type 'erlang:process_info_result_item'() ::
    {async_dist, Enabled :: boolean()}
    | {backtrace, Bin :: binary()}
    | {binary, BinInfo :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}]}
    | {catchlevel, CatchLevel :: non_neg_integer()}
    | {current_function, {Module :: module(), Function :: atom(), Arity :: arity()} | undefined}
    | {current_location, {
        Module :: module(),
        Function :: atom(),
        Arity :: arity(),
        Location :: [{file, Filename :: string()} | {line, Line :: pos_integer()}]
    }}
    | {current_stacktrace, Stack :: ['erlang:process_info_stack_item'()]}
    | {dictionary, Dictionary :: [{Key :: eqwalizer:dynamic(), Value :: eqwalizer:dynamic()}]}
    | {{dictionary, Key :: eqwalizer:dynamic()}, Value :: eqwalizer:dynamic()}
    | {error_handler, Module :: module()}
    | {garbage_collection, GCInfo :: [{atom(), non_neg_integer()}]}
    | {garbage_collection_info, GCInfo :: [{atom(), non_neg_integer()}]}
    | {group_leader, GroupLeader :: pid()}
    | {heap_size, Size :: non_neg_integer()}
    | {initial_call, mfa()}
    | {links, PidsAndPorts :: [pid() | port()]}
    | {label, eqwalizer:dynamic()}
    | {last_calls, false | (Calls :: [mfa()])}
    | {memory, Size :: non_neg_integer()}
    | {message_queue_len, MessageQueueLen :: non_neg_integer()}
    | {messages, MessageQueue :: [eqwalizer:dynamic()]}
    | {min_heap_size, MinHeapSize :: non_neg_integer()}
    | {min_bin_vheap_size, MinBinVHeapSize :: non_neg_integer()}
    | {max_heap_size, MaxHeapSize :: erlang:max_heap_size()}
    | {monitored_by, MonitoredBy :: [pid() | port() | erlang:nif_resource()]}
    | {monitors, Monitors :: [{process | port, Pid :: pid() | port() | {RegName :: atom(), Node :: node()}}]}
    | {message_queue_data, MQD :: erlang:message_queue_data()}
    | {parent, pid() | undefined}
    | {priority, Level :: erlang:priority_level()}
    | {priority_messages, Enabled :: boolean()}
    | {reductions, Number :: non_neg_integer()}
    | {registered_name, [] | (Atom :: atom())}
    | {sequential_trace_token, [] | (SequentialTraceToken :: eqwalizer:dynamic())}
    | {stack_size, Size :: non_neg_integer()}
    | {status, Status :: exiting | garbage_collecting | waiting | running | runnable | suspended}
    | {suspending,
        SuspendeeList :: [
            {Suspendee :: pid(), ActiveSuspendCount :: non_neg_integer(), OutstandingSuspendCount :: non_neg_integer()}
        ]}
    | {total_heap_size, Size :: non_neg_integer()}
    | {trace, InternalTraceFlags :: non_neg_integer()}
    | {trap_exit, Boolean :: boolean()}.

% not exported
-type 'erlang:process_info_stack_item'() :: {
    Module :: module(),
    Function :: atom(),
    Arity :: arity() | (Args :: [eqwalizer:dynamic()]),
    Location :: [{file, Filename :: string()} | {line, Line :: pos_integer()}]
}.

-type 'erlang:stacktrace'() :: [
    {module(), atom(), arity() | [eqwalizer:dynamic()], [
        StackTraceExtraInfo ::
            {line, pos_integer()}
            | {file, unicode:chardata()}
            | {error_info, #{module => module(), function => atom(), cause => eqwalizer:dynamic()}}
            | {atom(), eqwalizer:dynamic()}
    ]}
    | {function(), arity() | [eqwalizer:dynamic()], [
        StackTraceExtraInfo ::
            {line, pos_integer()}
            | {file, unicode:chardata()}
            | {error_info, #{module => module(), function => atom(), cause => eqwalizer:dynamic()}}
            | {atom(), eqwalizer:dynamic()}
    ]}
].

%% -------- gen_server --------

-type 'gen_server:format_status'() :: #{
    state => eqwalizer:dynamic(),
    message => eqwalizer:dynamic(),
    reason => eqwalizer:dynamic(),
    log => [sys:system_event()]
}.

%% -------- inet --------

-type 'inet:module_socket'() :: {'$inet', Handler :: eqwalizer:dynamic(module()), Handle :: eqwalizer:dynamic()}.

%% -------- logger --------

-type 'logger:filter_arg'() :: eqwalizer:dynamic().

-type 'logger:metadata'() :: #{
    pid => pid(),
    gl => pid(),
    time => logger:timestamp(),
    mfa => {module(), atom(), non_neg_integer()},
    file => file:filename(),
    line => non_neg_integer(),
    domain => [eqwalizer:dynamic(atom())],
    report_cb => logger:report_cb(),
    atom() => eqwalizer:dynamic()
}.

-type 'logger:report_cb'() ::
    fun((eqwalizer:dynamic()) -> {io:format(), [term()]})
    | fun((eqwalizer:dynamic(), logger:report_cb_config()) -> unicode:chardata()).

%% -------- logger_handler --------

-type 'logger_handler:config'() :: #{
    id => logger_handler:id(),
    config => eqwalizer:dynamic(),
    level => logger:level() | all | none,
    module => module(),
    filter_default => log | stop,
    filters => [{logger:filter_id(), logger:filter()}],
    formatter => {module(), logger:formatter_config()}
}.

%% -------- ssl --------

-type 'ssl:sslsocket'() :: eqwalizer:dynamic().

%% -------- supervisor --------

-type 'supervisor:startchild_err'() ::
    already_present
    | {already_started, Child :: undefined | pid()}
    | eqwalizer:dynamic().

%% -------- sys --------

-type 'sys:dbg_fun'() :: fun(
    (FuncState :: eqwalizer:dynamic(), Event :: sys:system_event(), ProcState :: eqwalizer:dynamic()) ->
        done | (NewFuncState :: eqwalizer:dynamic())
).
