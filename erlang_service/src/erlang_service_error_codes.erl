%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.
-module(erlang_service_error_codes).

-export([make_code/2]).

%-----------------------------------------------------------------------

%% The make_code/2 function is based on the one in erlang_ls

%% @doc Return a unique code for each of the possible errors returned by the
%% compiler or its subsystems.

-spec make_code(atom(), any()) -> string().
%% This file
make_code(?MODULE, _) ->
    "L0000";
%% compiler-8.0.2/src/compile.erl
make_code(compile, no_crypto) ->
    "C1000";
make_code(compile, bad_crypto_key) ->
    "C1001";
make_code(compile, no_crypto_key) ->
    "C1002";
make_code(compile, {open, _E}) ->
    "C1003";
make_code(compile, {epp, _E}) ->
    make_code(epp, compile);
make_code(compile, write_error) ->
    "C1004";
make_code(compile, {write_error, _Error}) ->
    "C1005";
make_code(compile, {rename, _From, _To, _Error}) ->
    "C1006";
make_code(compile, {parse_transform, _M, _R}) ->
    "C1007";
make_code(compile, {undef_parse_transform, _M}) ->
    "C1008";
make_code(compile, {core_transform, _M, _R}) ->
    "C1009";
make_code(compile, {crash, _Pass, _Reason, _Stk}) ->
    "C1010";
make_code(compile, {bad_return, _Pass, _Reason}) ->
    "C1011";
make_code(compile, {module_name, _Mod, _Filename}) ->
    "C1012";
make_code(compile, _Other) ->
    "C1099";
%% syntax_tools-2.6/src/epp_dodger.erl
make_code(epp_dodger, macro_args) ->
    "D1100";
make_code(epp_dodger, {error, _Error}) ->
    "D1101";
make_code(epp_dodger, {warning, _Error}) ->
    "D1102";
make_code(epp_dodger, {unknown, _Reason}) ->
    "D1103";
make_code(epp_dodger, _Other) ->
    "D1199";
%% stdlib-3.15.2/src/erl_lint.erl
make_code(elp_lint, undefined_module) ->
    "L1201";
make_code(elp_lint, redefine_module) ->
    "L1202";
make_code(elp_lint, pmod_unsupported) ->
    "L1203";
make_code(elp_lint, non_latin1_module_unsupported) ->
    "L1204";
make_code(elp_lint, invalid_call) ->
    "L1205";
make_code(elp_lint, invalid_record) ->
    "L1206";
make_code(elp_lint, {attribute, _A}) ->
    "L1207";
make_code(elp_lint, {missing_qlc_hrl, _A}) ->
    "L1208";
make_code(elp_lint, {redefine_import, {{_F, _A}, _M}}) ->
    "L1209";
make_code(elp_lint, {bad_inline, {_F, _A}}) ->
    "L1210";
make_code(elp_lint, {invalid_deprecated, _D}) ->
    "L1211";
make_code(elp_lint, {bad_deprecated, {_F, _A}}) ->
    "L1212";
make_code(elp_lint, {invalid_removed, _D}) ->
    "L1213";
make_code(elp_lint, {bad_removed, {F, A}}) when F =:= '_'; A =:= '_' ->
    "L1214";
make_code(elp_lint, {bad_removed, {_F, _A}}) ->
    "L1215";
make_code(elp_lint, {bad_nowarn_unused_function, {_F, _A}}) ->
    "L1216";
make_code(elp_lint, {bad_nowarn_bif_clash, {_F, _A}}) ->
    "L1217";
make_code(elp_lint, disallowed_nowarn_bif_clash) ->
    "L1218";
make_code(elp_lint, {bad_on_load, _Term}) ->
    "L1219";
make_code(elp_lint, multiple_on_loads) ->
    "L1220";
make_code(elp_lint, {bad_on_load_arity, {_F, _A}}) ->
    "L1221";
make_code(elp_lint, {undefined_on_load, {_F, _A}}) ->
    "L1222";
make_code(elp_lint, nif_inline) ->
    "L1223";
make_code(elp_lint, export_all) ->
    "L1224";
make_code(elp_lint, {duplicated_export, {_F, _A}}) ->
    "L1225";
make_code(elp_lint, {unused_import, {{_F, _A}, _M}}) ->
    "L1226";
make_code(elp_lint, {undefined_function, {_F, _A}}) ->
    "L1227";
make_code(elp_lint, {redefine_function, {_F, _A}}) ->
    "L1228";
make_code(elp_lint, {define_import, {_F, _A}}) ->
    "L1229";
make_code(elp_lint, {unused_function, {_F, _A}}) ->
    "L1230";
make_code(elp_lint, {call_to_redefined_bif, {_F, _A}}) ->
    "L1231";
make_code(elp_lint, {call_to_redefined_old_bif, {_F, _A}}) ->
    "L1232";
make_code(elp_lint, {redefine_old_bif_import, {_F, _A}}) ->
    "L1233";
make_code(elp_lint, {redefine_bif_import, {_F, _A}}) ->
    "L1234";
make_code(elp_lint, {deprecated, _MFA, _String, _Rel}) ->
    "L1235";
make_code(elp_lint, {deprecated, _MFA, String}) when is_list(String) ->
    "L1236";
make_code(elp_lint, {deprecated_type, {_M1, _F1, _A1}, _String, _Rel}) ->
    "L1237";
make_code(elp_lint, {deprecated_type, {_M1, _F1, _A1}, String}) when is_list(String) ->
    "L1238";
make_code(elp_lint, {removed, _MFA, _ReplacementMFA, _Rel}) ->
    "L1239";
make_code(elp_lint, {removed, _MFA, String}) when is_list(String) ->
    "L1240";
make_code(elp_lint, {removed_type, _MNA, _String}) ->
    "L1241";
make_code(elp_lint, {obsolete_guard, {_F, _A}}) ->
    "L1242";
make_code(elp_lint, {obsolete_guard_overridden, _Test}) ->
    "L1243";
make_code(elp_lint, {too_many_arguments, _Arity}) ->
    "L1244";
make_code(elp_lint, illegal_pattern) ->
    "L1245";
make_code(elp_lint, illegal_map_key) ->
    "L1246";
make_code(elp_lint, illegal_bin_pattern) ->
    "L1247";
make_code(elp_lint, illegal_expr) ->
    "L1248";
make_code(elp_lint, {illegal_guard_local_call, {_F, _A}}) ->
    "L1249";
make_code(elp_lint, illegal_guard_expr) ->
    "L1250";
make_code(elp_lint, illegal_map_construction) ->
    "L1251";
make_code(elp_lint, {undefined_record, _T}) ->
    "L1252";
make_code(elp_lint, {redefine_record, _T}) ->
    "L1253";
make_code(elp_lint, {redefine_field, _T, _F}) ->
    "L1254";
make_code(elp_lint, bad_multi_field_init) ->
    "L1255";
make_code(elp_lint, {undefined_field, _T, _F}) ->
    "L1256";
make_code(elp_lint, illegal_record_info) ->
    "L1257";
make_code(elp_lint, {field_name_is_variable, _T, _F}) ->
    "L1258";
make_code(elp_lint, {wildcard_in_update, _T}) ->
    "L1259";
make_code(elp_lint, {unused_record, _T}) ->
    "L1260";
make_code(elp_lint, {untyped_record, _T}) ->
    "L1261";
make_code(elp_lint, {unbound_var, _V}) ->
    "L1262";
make_code(elp_lint, {unsafe_var, _V, {_What, _Where}}) ->
    "L1263";
make_code(elp_lint, {exported_var, _V, {_What, _Where}}) ->
    "L1264";
make_code(elp_lint, {match_underscore_var, _V}) ->
    "L1265";
make_code(elp_lint, {match_underscore_var_pat, _V}) ->
    "L1266";
make_code(elp_lint, {shadowed_var, _V, _In}) ->
    "L1267";
make_code(elp_lint, {unused_var, _V}) ->
    "L1268";
make_code(elp_lint, {variable_in_record_def, _V}) ->
    "L1269";
make_code(elp_lint, {stacktrace_guard, _V}) ->
    "L1270";
make_code(elp_lint, {stacktrace_bound, _V}) ->
    "L1271";
make_code(elp_lint, {undefined_bittype, _Type}) ->
    "L1272";
make_code(elp_lint, {bittype_mismatch, _Val1, _Val2, _What}) ->
    "L1273";
make_code(elp_lint, bittype_unit) ->
    "L1274";
make_code(elp_lint, illegal_bitsize) ->
    "L1275";
make_code(elp_lint, {illegal_bitsize_local_call, {_F, _A}}) ->
    "L1276";
make_code(elp_lint, non_integer_bitsize) ->
    "L1277";
make_code(elp_lint, unsized_binary_not_at_end) ->
    "L1278";
make_code(elp_lint, typed_literal_string) ->
    "L1279";
make_code(elp_lint, utf_bittype_size_or_unit) ->
    "L1280";
make_code(elp_lint, {bad_bitsize, _Type}) ->
    "L1281";
make_code(elp_lint, unsized_binary_in_bin_gen_pattern) ->
    "L1282";
make_code(elp_lint, {conflicting_behaviours, {_Name, _Arity}, _B, _FirstL, _FirstB}) ->
    "L1283";
make_code(elp_lint, {undefined_behaviour_func, {_Func, _Arity}, _Behaviour}) ->
    "L1284";
make_code(elp_lint, {undefined_behaviour, _Behaviour}) ->
    "L1285";
make_code(elp_lint, {undefined_behaviour_callbacks, _Behaviour}) ->
    "L1286";
make_code(elp_lint, {ill_defined_behaviour_callbacks, _Behaviour}) ->
    "L1287";
make_code(elp_lint, {ill_defined_optional_callbacks, _Behaviour}) ->
    "L1288";
make_code(elp_lint, {behaviour_info, {_M, _F, _A}}) ->
    "L1289";
make_code(elp_lint, {redefine_optional_callback, {_F, _A}}) ->
    "L1290";
make_code(elp_lint, {undefined_callback, {_M, _F, _A}}) ->
    "L1291";
make_code(elp_lint, {singleton_typevar, _Name}) ->
    "L1292";
make_code(elp_lint, {bad_export_type, _ETs}) ->
    "L1293";
make_code(elp_lint, {duplicated_export_type, {_T, _A}}) ->
    "L1294";
make_code(elp_lint, {undefined_type, {_TypeName, _Arity}}) ->
    "L1295";
make_code(elp_lint, {unused_type, {_TypeName, _Arity}}) ->
    "L1296";
make_code(elp_lint, {new_builtin_type, {_TypeName, _Arity}}) ->
    "L1297";
make_code(elp_lint, {builtin_type, {_TypeName, _Arity}}) ->
    "L1298";
make_code(elp_lint, {renamed_type, _OldName, _NewName}) ->
    "L1299";
make_code(elp_lint, {redefine_type, {_TypeName, _Arity}}) ->
    "L1300";
make_code(elp_lint, {type_syntax, _Constr}) ->
    "L1301";
make_code(elp_lint, old_abstract_code) ->
    "L1302";
make_code(elp_lint, {redefine_spec, {_M, _F, _A}}) ->
    "L1303";
make_code(elp_lint, {redefine_spec, {_F, _A}}) ->
    "L1304";
make_code(elp_lint, {redefine_callback, {_F, _A}}) ->
    "L1305";
make_code(elp_lint, {bad_callback, {_M, _F, _A}}) ->
    "L1306";
make_code(elp_lint, {bad_module, {_M, _F, _A}}) ->
    "L1307";
make_code(elp_lint, {spec_fun_undefined, {_F, _A}}) ->
    "L1308";
make_code(elp_lint, {missing_spec, {_F, _A}}) ->
    "L1309";
make_code(elp_lint, spec_wrong_arity) ->
    "L1310";
make_code(elp_lint, callback_wrong_arity) ->
    "L1311";
make_code(elp_lint, {deprecated_builtin_type, {_Name, _Arity}, _Replacement, _Rel}) ->
    "L1312";
make_code(elp_lint, {not_exported_opaque, {_TypeName, _Arity}}) ->
    "L1313";
make_code(elp_lint, {underspecified_opaque, {_TypeName, _Arity}}) ->
    "L1314";
make_code(elp_lint, {bad_dialyzer_attribute, _Term}) ->
    "L1315";
make_code(elp_lint, {bad_dialyzer_option, _Term}) ->
    "L1316";
make_code(elp_lint, {format_error, {_Fmt, _Args}}) ->
    "L1317";
make_code(elp_lint, _Other) ->
    "L1399";
%% stdlib-3.15.2/src/erl_scan.erl
make_code(elp_scan, {string, _Quote, _Head}) ->
    "S1400";
make_code(elp_scan, {illegal, _Type}) ->
    "S1401";
make_code(elp_scan, char) ->
    "S1402";
make_code(elp_scan, {base, _Base}) ->
    "S1403";
make_code(elp_scan, _Other) ->
    "S1499";
%% stdlib-3.15.2/src/epp.erl
make_code(elp_epp, cannot_parse) ->
    "E1500";
make_code(elp_epp, {bad, _W}) ->
    "E1501";
make_code(elp_epp, {duplicated_argument, _Arg}) ->
    "E1502";
make_code(elp_epp, missing_parenthesis) ->
    "E1503";
make_code(elp_epp, missing_comma) ->
    "E1504";
make_code(elp_epp, premature_end) ->
    "E1505";
make_code(elp_epp, {call, _What}) ->
    "E1506";
make_code(elp_epp, {undefined, _M, none}) ->
    "E1507";
make_code(elp_epp, {undefined, _M, _A}) ->
    "E1508";
make_code(elp_epp, {depth, _What}) ->
    "E1509";
make_code(elp_epp, {mismatch, _M}) ->
    "E1510";
make_code(elp_epp, {arg_error, _M}) ->
    "E1511";
make_code(elp_epp, {redefine, _M}) ->
    "E1512";
make_code(elp_epp, {redefine_predef, _M}) ->
    "E1513";
make_code(elp_epp, {circular, _M, none}) ->
    "E1514";
make_code(elp_epp, {circular, _M, _A}) ->
    "E1515";
make_code(elp_epp, {include, _W, _F}) ->
    "E1516";
make_code(elp_epp, {illegal, _How, _What}) ->
    "E1517";
make_code(elp_epp, {illegal_function, _Macro}) ->
    "E1518";
make_code(elp_epp, {illegal_function_usage, _Macro}) ->
    "E1519";
make_code(elp_epp, elif_after_else) ->
    "E1520";
make_code(elp_epp, {'NYI', _What}) ->
    "E1521";
make_code(elp_epp, {error, _Term}) ->
    "E1522";
make_code(elp_epp, {warning, _Term}) ->
    "E1523";
make_code(elp_epp, _E) ->
    "E1599";
%% stdlib-3.15.2/src/qlc.erl
make_code(qlc, not_a_query_list_comprehension) ->
    "Q1600";
make_code(qlc, {used_generator_variable, _V}) ->
    "Q1601";
make_code(qlc, binary_generator) ->
    "Q1602";
make_code(qlc, too_complex_join) ->
    "Q1603";
make_code(qlc, too_many_joins) ->
    "Q1604";
make_code(qlc, nomatch_pattern) ->
    "Q1605";
make_code(qlc, nomatch_filter) ->
    "Q1606";
make_code(qlc, {Location, _Mod, _Reason}) when is_integer(Location) ->
    "Q1607";
make_code(qlc, {bad_object, _FileName}) ->
    "Q1608";
make_code(qlc, bad_object) ->
    "Q1609";
make_code(qlc, {file_error, _FileName, _Reason}) ->
    "Q1610";
make_code(qlc, {premature_eof, _FileName}) ->
    "Q1611";
make_code(qlc, {tmpdir_usage, _Why}) ->
    "Q1612";
make_code(qlc, {error, _Module, _Reason}) ->
    "Q1613";
make_code(qlc, _E) ->
    "Q1699";
%% stdlib-3.15.2/src/erl_parse.yrl
make_code(elp_parse, "head mismatch") ->
    "P1700";
make_code(elp_parse, "bad type variable") ->
    "P1701";
make_code(elp_parse, "bad attribute") ->
    "P1702";
make_code(elp_parse, "unsupported constraint" ++ _) ->
    "P1703";
make_code(elp_parse, "bad binary type") ->
    "P1704";
make_code(elp_parse, "bad variable list") ->
    "P1705";
make_code(elp_parse, "bad function arity") ->
    "P1706";
make_code(elp_parse, "bad function name") ->
    "P1707";
make_code(elp_parse, "bad Name/Arity") ->
    "P1708";
make_code(elp_parse, "bad record declaration") ->
    "P1709";
make_code(elp_parse, "bad record field") ->
    "P1710";
make_code(elp_parse, ["syntax error before: ", _]) ->
    "P1711";
%% Matching 'io_lib:format("bad ~tw declaration", [S])).', must come last
make_code(elp_parse, "bad " ++ _Str) ->
    "P1798";
make_code(elp_parse, _Other) ->
    "P1799";
%% Erlang EDoc
make_code(edoc, "XML parse error: ~p.") ->
    "O0001";
make_code(edoc, "error in XML parser: ~P.") ->
    "O0002";
make_code(edoc, "nocatch in XML parser: ~P.") ->
    "O0003";
make_code(edoc, "heading end marker mismatch: ~s...~s") ->
    "O0004";
make_code(edoc, "`-quote ended unexpectedly at line ~w") ->
    "O0005";
make_code(edoc, "``-quote ended unexpectedly at line ~w") ->
    "O0006";
make_code(edoc, "```-quote ended unexpectedly at line ~w") ->
    "O0007";
make_code(edoc, "reference '[~ts:...' ended unexpectedly") ->
    "O0008";
make_code(edoc, "cannot handle guard") ->
    "O0009";
make_code(edoc, "error reading file '~ts': ~w") ->
    "O0010";
make_code(edoc, "file not found: ~ts") ->
    "O0011";
make_code(edoc, "expected file name as a string") ->
    "O0012";
make_code(edoc, "@spec arity does not match.") ->
    "O0013";
make_code(edoc, "@spec name does not match.") ->
    "O0014";
make_code(edoc, "must specify name or e-mail.") ->
    "O0015";
make_code(edoc, "redefining built-in type '~w'.") ->
    "O0016";
make_code(edoc, "multiple '<...>' sections.") ->
    "O0017";
make_code(edoc, "multiple '[...]' sections.") ->
    "O0018";
make_code(edoc, "missing '~c'.") ->
    "O0019";
make_code(edoc, "unexpected end of expression.") ->
    "O0020";
make_code(edoc, "multiple @~s tag.") ->
    "O0021";
make_code(edoc, "tag @~s not allowed here.") ->
    "O0022";
make_code(edoc, "bad macro definition: ~P.") ->
    "O0023";
make_code(edoc, "cannot find application directory for '~s'.") ->
    "O0024";
make_code(edoc, "recursive macro expansion of {@~s}.") ->
    "O0025";
make_code(edoc, "undefined macro {@~s}.") ->
    "O0026";
make_code(edoc, "unexpected end of macro.") ->
    "O0027";
make_code(edoc, "missing macro name.") ->
    "O0028";
make_code(edoc, "bad macro name: '@~s...'.") ->
    "O0029";
make_code(edoc, "reference to untyped record ~w") ->
    "O0030";
make_code(edoc, "'~s' is not allowed - skipping tag, extracting content") ->
    "O0031";
make_code(edoc,
    "cannot handle spec with constraints - arity mismatch.\n"
    "This is a bug in EDoc spec formatter - please report it at "
    "https://bugs.erlang.org/\n"
    "Identified arguments: ~p\n"
    "Original spec: ~s\n"
) ->
    "O0032";
make_code(edoc,
    "cannot annotate spec: "
    "function and spec clause numbers do not match\n"
) ->
    "O0033";
make_code(edoc,
    "EDoc @spec tags are deprecated. "
    "Please use -spec attributes instead."
) ->
    "O0034";
make_code(edoc,
    "EDoc @type tags are deprecated. "
    "Please use -type attributes instead."
) ->
    "O0035";
make_code(edoc, "redefining built-in type '~w'.") ->
    "O0036";
make_code(edoc, "duplicated type ~w~s") ->
    "O0037";
make_code(edoc, "missing type ~w~s") ->
    "O0038";
make_code(edoc, "tag @~s not recognized.") ->
    "O0039";
make_code(edoc, _NoCategory) ->
    "O0000";
make_code(Module, _Reason) ->
    unicode:characters_to_list(
        io_lib:format("~p", [Module])
    ).
