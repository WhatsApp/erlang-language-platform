/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use fxhash::FxHashSet;
use lazy_static::lazy_static;

use crate::ast;

lazy_static! {
    static ref FUNS: FxHashSet<ast::Id> = {
        vec![
            ast::Id {
                name: "abs".into(),
                arity: 1,
            },
            ast::Id {
                name: "apply".into(),
                arity: 2,
            },
            ast::Id {
                name: "apply".into(),
                arity: 3,
            },
            ast::Id {
                name: "atom_to_binary".into(),
                arity: 1,
            },
            ast::Id {
                name: "atom_to_binary".into(),
                arity: 2,
            },
            ast::Id {
                name: "atom_to_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "binary_part".into(),
                arity: 2,
            },
            ast::Id {
                name: "binary_part".into(),
                arity: 3,
            },
            ast::Id {
                name: "binary_to_atom".into(),
                arity: 1,
            },
            ast::Id {
                name: "binary_to_atom".into(),
                arity: 2,
            },
            ast::Id {
                name: "binary_to_existing_atom".into(),
                arity: 1,
            },
            ast::Id {
                name: "binary_to_existing_atom".into(),
                arity: 2,
            },
            ast::Id {
                name: "binary_to_integer".into(),
                arity: 1,
            },
            ast::Id {
                name: "binary_to_integer".into(),
                arity: 2,
            },
            ast::Id {
                name: "binary_to_float".into(),
                arity: 1,
            },
            ast::Id {
                name: "binary_to_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "binary_to_list".into(),
                arity: 3,
            },
            ast::Id {
                name: "binary_to_term".into(),
                arity: 1,
            },
            ast::Id {
                name: "binary_to_term".into(),
                arity: 2,
            },
            ast::Id {
                name: "bitsize".into(),
                arity: 1,
            },
            ast::Id {
                name: "bit_size".into(),
                arity: 1,
            },
            ast::Id {
                name: "bitstring_to_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "byte_size".into(),
                arity: 1,
            },
            ast::Id {
                name: "ceil".into(),
                arity: 1,
            },
            ast::Id {
                name: "check_old_code".into(),
                arity: 1,
            },
            ast::Id {
                name: "check_process_code".into(),
                arity: 2,
            },
            ast::Id {
                name: "check_process_code".into(),
                arity: 3,
            },
            ast::Id {
                name: "date".into(),
                arity: 0,
            },
            ast::Id {
                name: "delete_module".into(),
                arity: 1,
            },
            ast::Id {
                name: "demonitor".into(),
                arity: 1,
            },
            ast::Id {
                name: "demonitor".into(),
                arity: 2,
            },
            ast::Id {
                name: "disconnect_node".into(),
                arity: 1,
            },
            ast::Id {
                name: "element".into(),
                arity: 2,
            },
            ast::Id {
                name: "erase".into(),
                arity: 0,
            },
            ast::Id {
                name: "erase".into(),
                arity: 1,
            },
            ast::Id {
                name: "error".into(),
                arity: 1,
            },
            ast::Id {
                name: "error".into(),
                arity: 2,
            },
            ast::Id {
                name: "exit".into(),
                arity: 1,
            },
            ast::Id {
                name: "exit".into(),
                arity: 2,
            },
            ast::Id {
                name: "float".into(),
                arity: 1,
            },
            ast::Id {
                name: "float_to_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "float_to_list".into(),
                arity: 2,
            },
            ast::Id {
                name: "float_to_binary".into(),
                arity: 1,
            },
            ast::Id {
                name: "float_to_binary".into(),
                arity: 2,
            },
            ast::Id {
                name: "floor".into(),
                arity: 1,
            },
            ast::Id {
                name: "garbage_collect".into(),
                arity: 0,
            },
            ast::Id {
                name: "garbage_collect".into(),
                arity: 1,
            },
            ast::Id {
                name: "garbage_collect".into(),
                arity: 2,
            },
            ast::Id {
                name: "get".into(),
                arity: 0,
            },
            ast::Id {
                name: "get".into(),
                arity: 1,
            },
            ast::Id {
                name: "get_keys".into(),
                arity: 0,
            },
            ast::Id {
                name: "get_keys".into(),
                arity: 1,
            },
            ast::Id {
                name: "group_leader".into(),
                arity: 0,
            },
            ast::Id {
                name: "group_leader".into(),
                arity: 2,
            },
            ast::Id {
                name: "halt".into(),
                arity: 0,
            },
            ast::Id {
                name: "halt".into(),
                arity: 1,
            },
            ast::Id {
                name: "halt".into(),
                arity: 2,
            },
            ast::Id {
                name: "hd".into(),
                arity: 1,
            },
            ast::Id {
                name: "integer_to_binary".into(),
                arity: 1,
            },
            ast::Id {
                name: "integer_to_binary".into(),
                arity: 2,
            },
            ast::Id {
                name: "integer_to_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "integer_to_list".into(),
                arity: 2,
            },
            ast::Id {
                name: "iolist_size".into(),
                arity: 1,
            },
            ast::Id {
                name: "iolist_to_binary".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_alive".into(),
                arity: 0,
            },
            ast::Id {
                name: "is_process_alive".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_atom".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_boolean".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_binary".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_bitstring".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_float".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_function".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_function".into(),
                arity: 2,
            },
            ast::Id {
                name: "is_integer".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_map".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_map_key".into(),
                arity: 2,
            },
            ast::Id {
                name: "is_number".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_pid".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_port".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_reference".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_tuple".into(),
                arity: 1,
            },
            ast::Id {
                name: "is_record".into(),
                arity: 2,
            },
            ast::Id {
                name: "is_record".into(),
                arity: 3,
            },
            ast::Id {
                name: "length".into(),
                arity: 1,
            },
            ast::Id {
                name: "link".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_atom".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_binary".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_bitstring".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_existing_atom".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_float".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_integer".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_integer".into(),
                arity: 2,
            },
            ast::Id {
                name: "list_to_pid".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_port".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_ref".into(),
                arity: 1,
            },
            ast::Id {
                name: "list_to_tuple".into(),
                arity: 1,
            },
            ast::Id {
                name: "load_module".into(),
                arity: 2,
            },
            ast::Id {
                name: "make_ref".into(),
                arity: 0,
            },
            ast::Id {
                name: "map_size".into(),
                arity: 1,
            },
            ast::Id {
                name: "map_get".into(),
                arity: 2,
            },
            ast::Id {
                name: "max".into(),
                arity: 2,
            },
            ast::Id {
                name: "min".into(),
                arity: 2,
            },
            ast::Id {
                name: "module_loaded".into(),
                arity: 1,
            },
            ast::Id {
                name: "monitor".into(),
                arity: 2,
            },
            ast::Id {
                name: "monitor_node".into(),
                arity: 2,
            },
            ast::Id {
                name: "node".into(),
                arity: 0,
            },
            ast::Id {
                name: "node".into(),
                arity: 1,
            },
            ast::Id {
                name: "nodes".into(),
                arity: 0,
            },
            ast::Id {
                name: "nodes".into(),
                arity: 1,
            },
            ast::Id {
                name: "now".into(),
                arity: 0,
            },
            ast::Id {
                name: "open_port".into(),
                arity: 2,
            },
            ast::Id {
                name: "pid_to_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "port_to_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "port_close".into(),
                arity: 1,
            },
            ast::Id {
                name: "port_command".into(),
                arity: 2,
            },
            ast::Id {
                name: "port_command".into(),
                arity: 3,
            },
            ast::Id {
                name: "port_connect".into(),
                arity: 2,
            },
            ast::Id {
                name: "port_control".into(),
                arity: 3,
            },
            ast::Id {
                name: "pre_loaded".into(),
                arity: 0,
            },
            ast::Id {
                name: "process_flag".into(),
                arity: 2,
            },
            ast::Id {
                name: "process_flag".into(),
                arity: 3,
            },
            ast::Id {
                name: "process_info".into(),
                arity: 1,
            },
            ast::Id {
                name: "process_info".into(),
                arity: 2,
            },
            ast::Id {
                name: "processes".into(),
                arity: 0,
            },
            ast::Id {
                name: "purge_module".into(),
                arity: 1,
            },
            ast::Id {
                name: "put".into(),
                arity: 2,
            },
            ast::Id {
                name: "ref_to_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "register".into(),
                arity: 2,
            },
            ast::Id {
                name: "registered".into(),
                arity: 0,
            },
            ast::Id {
                name: "round".into(),
                arity: 1,
            },
            ast::Id {
                name: "self".into(),
                arity: 0,
            },
            ast::Id {
                name: "setelement".into(),
                arity: 3,
            },
            ast::Id {
                name: "size".into(),
                arity: 1,
            },
            ast::Id {
                name: "spawn".into(),
                arity: 1,
            },
            ast::Id {
                name: "spawn".into(),
                arity: 2,
            },
            ast::Id {
                name: "spawn".into(),
                arity: 3,
            },
            ast::Id {
                name: "spawn".into(),
                arity: 4,
            },
            ast::Id {
                name: "spawn_link".into(),
                arity: 1,
            },
            ast::Id {
                name: "spawn_link".into(),
                arity: 2,
            },
            ast::Id {
                name: "spawn_link".into(),
                arity: 3,
            },
            ast::Id {
                name: "spawn_link".into(),
                arity: 4,
            },
            ast::Id {
                name: "spawn_request".into(),
                arity: 1,
            },
            ast::Id {
                name: "spawn_request".into(),
                arity: 2,
            },
            ast::Id {
                name: "spawn_request".into(),
                arity: 3,
            },
            ast::Id {
                name: "spawn_request".into(),
                arity: 4,
            },
            ast::Id {
                name: "spawn_request".into(),
                arity: 5,
            },
            ast::Id {
                name: "spawn_request_abandon".into(),
                arity: 1,
            },
            ast::Id {
                name: "spawn_monitor".into(),
                arity: 1,
            },
            ast::Id {
                name: "spawn_monitor".into(),
                arity: 2,
            },
            ast::Id {
                name: "spawn_monitor".into(),
                arity: 3,
            },
            ast::Id {
                name: "spawn_monitor".into(),
                arity: 4,
            },
            ast::Id {
                name: "spawn_opt".into(),
                arity: 2,
            },
            ast::Id {
                name: "spawn_opt".into(),
                arity: 3,
            },
            ast::Id {
                name: "spawn_opt".into(),
                arity: 4,
            },
            ast::Id {
                name: "spawn_opt".into(),
                arity: 5,
            },
            ast::Id {
                name: "split_binary".into(),
                arity: 2,
            },
            ast::Id {
                name: "statistics".into(),
                arity: 1,
            },
            ast::Id {
                name: "term_to_binary".into(),
                arity: 1,
            },
            ast::Id {
                name: "term_to_binary".into(),
                arity: 2,
            },
            ast::Id {
                name: "term_to_iovec".into(),
                arity: 1,
            },
            ast::Id {
                name: "term_to_iovec".into(),
                arity: 2,
            },
            ast::Id {
                name: "throw".into(),
                arity: 1,
            },
            ast::Id {
                name: "time".into(),
                arity: 0,
            },
            ast::Id {
                name: "tl".into(),
                arity: 1,
            },
            ast::Id {
                name: "trunc".into(),
                arity: 1,
            },
            ast::Id {
                name: "tuple_size".into(),
                arity: 1,
            },
            ast::Id {
                name: "tuple_to_list".into(),
                arity: 1,
            },
            ast::Id {
                name: "unlink".into(),
                arity: 1,
            },
            ast::Id {
                name: "unregister".into(),
                arity: 1,
            },
            ast::Id {
                name: "whereis".into(),
                arity: 1,
            },
        ]
        .into_iter()
        .collect()
    };
}

pub fn is_auto_imported(id: &ast::Id) -> bool {
    return FUNS.contains(id);
}
