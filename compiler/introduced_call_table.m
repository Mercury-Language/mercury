%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: introduced_call_table.m.
%
% This module contains a table of the predicates and functions
% that compiler passes may introduce calls to.
%
%-----------------------------------------------------------------------------%

:- module hlds.introduced_call_table.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%-----------------------------------------------------------------------------%

    % may_introduce_calls_to(PredOrFunc, StdLibModuleNameStr, Name, OrigArity):
    %
    % Succeed if a compiler pass may introduce calls to a predicate or
    % function from the given standard library module with the given name
    % and original (pred-form) arity.
    %
    % dead_proc_elim.m calls this predicate to avoid deleting predicates
    % that are unused when it is first run, but which may have calls to them
    % added later on.
    %
:- pred may_introduce_calls_to(pred_or_func::in, string::in,
    string::in, arity::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

may_introduce_calls_to(PredOrFunc, ModuleName, PredName, OrigArity) :-
    % XXX Many operations in the standard library are available
    % both as a function and as a predicate. If the compiler consistently
    % introduces calls to one form but not the other, then the other
    % need not be listed here.
    (
        ModuleName = "builtin",
        mict_builtin(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "exception",
        mict_exception(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "int",
        ( mict_int(PredName, OrigArity)
        ; mict_ints_uints(PredName, OrigArity)
        ),
        PredOrFunc = pf_function
    ;
        ( ModuleName = "uint"
        ; ModuleName = "int8"
        ; ModuleName = "uint8"
        ; ModuleName = "int16"
        ; ModuleName = "uint16"
        ; ModuleName = "int32"
        ; ModuleName = "uint32"
        ; ModuleName = "int64"
        ; ModuleName = "uint64"
        ),
        mict_ints_uints(PredName, OrigArity),
        PredOrFunc = pf_function
    ;
        ModuleName = "io",
        mict_io(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "par_builtin",
        mict_par_builtin(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "private_builtin",
        mict_private_builtin(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "profiling_builtin",
        mict_profiling_builtin(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "region_builtin",
        mict_region_builtin(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "ssdb",
        mict_ssdb(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "stm_builtin",
        mict_stm_builtin(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "stream",
        mict_stream(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "string",
        mict_string(PredName, OrigArity, PredOrFunc)
    ;
        ModuleName = "string.format",
        mict_string_format(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "table_builtin",
        mict_table_builtin(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "term_size_prof_builtin",
        mict_term_size_prof_builtin(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ;
        ModuleName = "univ",
        mict_univ(PredName, OrigArity),
        PredOrFunc = pf_predicate
    ).

:- pred mict_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_builtin/2)).

mict_builtin("compare",                3).
mict_builtin("compare_representation", 3).
mict_builtin("copy",                   2).
mict_builtin("unify",                  2).

:- pred mict_exception(string::in, int::in) is semidet.
:- pragma inline(pred(mict_exception/2)).

mict_exception("rethrow",        1).
mict_exception("throw",          1).
mict_exception("try",            2).
mict_exception("try_io",         4).
mict_exception("unreachable",    0).
mict_exception("unsafe_try_stm", 4).

:- pred mict_int(string::in, int::in) is semidet.
:- pragma inline(pred(mict_int/2)).

mict_int("minus", 3).

:- pred mict_ints_uints(string::in, int::in) is semidet.
:- pragma inline(pred(mict_ints_uints/2)).

% For some reason, the compiler records the original arity of
% int.unchecked_quotient as 3, not 2. Don't check the arities
% until this is fixed.
%
% XXX The original arity we are given is a pred-form arity,
% which includes the function result.
mict_ints_uints("*",                     3).
mict_ints_uints("unchecked_quotient",    3).
mict_ints_uints("unchecked_rem",         3).
mict_ints_uints("unchecked_left_shift",  3).
mict_ints_uints("unchecked_right_shift", 3).

:- pred mict_io(string::in, int::in) is semidet.
:- pragma inline(pred(mict_io/2)).

mict_io("write_string", 3).
mict_io("write_string", 4).

:- pred mict_par_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_par_builtin/2)).

mict_par_builtin("evaluate_parallelism_condition", 0).
mict_par_builtin("get_future",                     2).
mict_par_builtin("lc_create",                      2).
mict_par_builtin("lc_default_num_contexts",        1).
mict_par_builtin("lc_finish",                      1).
mict_par_builtin("lc_join_and_terminate",          2).
mict_par_builtin("lc_wait_free_slot",              2).
mict_par_builtin("new_future",                     2).
mict_par_builtin("signal_future",                  2).
mict_par_builtin("wait_future",                    2).

:- pred mict_private_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_private_builtin/2)).

mict_private_builtin("builtin_compare",     3).
mict_private_builtin("builtin_compare_character", 3).
mict_private_builtin("builtin_compare_float", 3).
mict_private_builtin("builtin_compare_int", 3).
mict_private_builtin("builtin_compare_int16", 3).
mict_private_builtin("builtin_compare_int32", 3).
mict_private_builtin("builtin_compare_int64", 3).
mict_private_builtin("builtin_compare_int8",  3).
mict_private_builtin("builtin_compare_non_canonical_type", 2).
mict_private_builtin("builtin_compare_pred", 3).
mict_private_builtin("builtin_compare_solver_type", 3).
mict_private_builtin("builtin_compare_string", 3).
mict_private_builtin("builtin_compare_uint",  3).
mict_private_builtin("builtin_compare_uint16", 3).
mict_private_builtin("builtin_compare_uint32", 3).
mict_private_builtin("builtin_compare_uint64", 3).
mict_private_builtin("builtin_compare_uint8", 3).
mict_private_builtin("builtin_index",       2).
mict_private_builtin("builtin_int16_gt",    2).
mict_private_builtin("builtin_int16_lt",    2).
mict_private_builtin("builtin_int32_gt",    2).
mict_private_builtin("builtin_int32_lt",    2).
mict_private_builtin("builtin_int64_gt",    2).
mict_private_builtin("builtin_int64_lt",    2).
mict_private_builtin("builtin_int8_gt",     2).
mict_private_builtin("builtin_int8_lt",     2).
mict_private_builtin("builtin_int_gt",      2).
mict_private_builtin("builtin_int_lt",      2).
mict_private_builtin("builtin_uint16_gt",   2).
mict_private_builtin("builtin_uint16_lt",   2).
mict_private_builtin("builtin_uint32_gt",   2).
mict_private_builtin("builtin_uint32_lt",   2).
mict_private_builtin("builtin_uint64_gt",   2).
mict_private_builtin("builtin_uint64_lt",   2).
mict_private_builtin("builtin_uint8_gt",    2).
mict_private_builtin("builtin_uint8_lt",    2).
mict_private_builtin("builtin_uint_gt",     2).
mict_private_builtin("builtin_uint_lt",     2).
mict_private_builtin("builtin_unify",       2).
mict_private_builtin("builtin_unify_character", 2).
mict_private_builtin("builtin_unify_float", 2).
mict_private_builtin("builtin_unify_int",   2).
mict_private_builtin("builtin_unify_int16", 2).
mict_private_builtin("builtin_unify_int32", 2).
mict_private_builtin("builtin_unify_int64", 2).
mict_private_builtin("builtin_unify_int8",  2).
mict_private_builtin("builtin_unify_pred",  2).
mict_private_builtin("builtin_unify_solver_type", 2).
mict_private_builtin("builtin_unify_string", 2).
mict_private_builtin("builtin_unify_uint",  2).
mict_private_builtin("builtin_unify_uint16", 2).
mict_private_builtin("builtin_unify_uint32", 2).
mict_private_builtin("builtin_unify_uint64", 2).
mict_private_builtin("builtin_unify_uint8", 2).
mict_private_builtin("compare_error",       0).
mict_private_builtin("discard_ticket",      0).
mict_private_builtin("instance_constraint_from_typeclass_info", 3).
mict_private_builtin("mark_hp",             1).
mict_private_builtin("mark_ticket_stack",   1).
mict_private_builtin("no_clauses",          1).
mict_private_builtin("partial_inst_copy",   2).
mict_private_builtin("prune_ticket",        0).
mict_private_builtin("prune_tickets_to",    1).
mict_private_builtin("reset_ticket_commit", 1).
mict_private_builtin("reset_ticket_solve",  1).
mict_private_builtin("reset_ticket_undo",   1).
mict_private_builtin("restore_hp",          1).
mict_private_builtin("sorry",               1).
mict_private_builtin("store_at_ref_impure", 2).
mict_private_builtin("store_ticket",        1).
mict_private_builtin("superclass_from_typeclass_info", 3).
mict_private_builtin("trace_evaluate_runtime_condition", 0).
mict_private_builtin("type_info_from_typeclass_info", 3).
mict_private_builtin("typed_compare",       3).
mict_private_builtin("typed_unify",         2).
mict_private_builtin("unconstrained_type_info_from_typeclass_info", 3).
mict_private_builtin("unify_remote_arg_words", 4).
mict_private_builtin("unused",              0).

:- pred mict_profiling_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_profiling_builtin/2)).

mict_profiling_builtin("det_call_port_code_ac", 3).
mict_profiling_builtin("det_call_port_code_sr", 4).
mict_profiling_builtin("det_exit_port_code_ac", 2).
mict_profiling_builtin("det_exit_port_code_sr", 3).
mict_profiling_builtin("increment_dynamic_coverage_point_count", 1).
mict_profiling_builtin("increment_static_coverage_point_count", 2).
mict_profiling_builtin("non_call_port_code_ac", 4).
mict_profiling_builtin("non_call_port_code_sr", 5).
mict_profiling_builtin("non_exit_port_code_ac", 2).
mict_profiling_builtin("non_exit_port_code_sr", 3).
mict_profiling_builtin("non_fail_port_code_ac", 2).
mict_profiling_builtin("non_fail_port_code_sr", 3).
mict_profiling_builtin("non_redo_port_code_ac",    2).
mict_profiling_builtin("non_redo_port_code_sr",    2).
mict_profiling_builtin("prepare_for_callback",     1).
mict_profiling_builtin("prepare_for_ho_call",      2).
mict_profiling_builtin("prepare_for_method_call",  3).
mict_profiling_builtin("prepare_for_normal_call",  1).
mict_profiling_builtin("prepare_for_special_call", 2).
mict_profiling_builtin("prepare_for_tail_call",    1).
mict_profiling_builtin("reset_activation_info_ac", 2).
mict_profiling_builtin("reset_activation_info_sr", 1).
mict_profiling_builtin("restore_recursion_depth_exit_1", 3).
mict_profiling_builtin("restore_recursion_depth_exit_2", 4).
mict_profiling_builtin("restore_recursion_depth_exit_3", 5).
mict_profiling_builtin("restore_recursion_depth_exit_4", 6).
mict_profiling_builtin("restore_recursion_depth_exit_5", 7).
mict_profiling_builtin("restore_recursion_depth_exit_6", 8).
mict_profiling_builtin("restore_recursion_depth_exit_7", 9).
mict_profiling_builtin("restore_recursion_depth_exit_8", 10).
mict_profiling_builtin("restore_recursion_depth_exit_9", 11).
mict_profiling_builtin("restore_recursion_depth_fail_1", 3).
mict_profiling_builtin("restore_recursion_depth_fail_2", 4).
mict_profiling_builtin("restore_recursion_depth_fail_3", 5).
mict_profiling_builtin("restore_recursion_depth_fail_4", 6).
mict_profiling_builtin("restore_recursion_depth_fail_5", 7).
mict_profiling_builtin("restore_recursion_depth_fail_6", 8).
mict_profiling_builtin("restore_recursion_depth_fail_7", 9).
mict_profiling_builtin("restore_recursion_depth_fail_8", 10).
mict_profiling_builtin("restore_recursion_depth_fail_9", 11).
mict_profiling_builtin("rezero_activation_info_ac", 0).
mict_profiling_builtin("rezero_activation_info_sr", 0).
mict_profiling_builtin("save_and_zero_activation_info_ac", 2).
mict_profiling_builtin("save_and_zero_activation_info_sr", 1).
mict_profiling_builtin("save_recursion_depth_1", 3).
mict_profiling_builtin("save_recursion_depth_2", 4).
mict_profiling_builtin("save_recursion_depth_3", 5).
mict_profiling_builtin("save_recursion_depth_4", 6).
mict_profiling_builtin("save_recursion_depth_5", 7).
mict_profiling_builtin("save_recursion_depth_6", 8).
mict_profiling_builtin("save_recursion_depth_7", 9).
mict_profiling_builtin("save_recursion_depth_8", 10).
mict_profiling_builtin("save_recursion_depth_9", 11).
mict_profiling_builtin("semi_call_port_code_ac", 3).
mict_profiling_builtin("semi_call_port_code_sr", 4).
mict_profiling_builtin("semi_exit_port_code_ac", 2).
mict_profiling_builtin("semi_exit_port_code_sr", 3).
mict_profiling_builtin("semi_fail_port_code_ac", 2).
mict_profiling_builtin("semi_fail_port_code_sr", 3).

:- pred mict_region_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_region_builtin/2)).

mict_region_builtin("create_region", 1).
mict_region_builtin("remove_region", 1).

:- pred mict_ssdb(string::in, int::in) is semidet.
:- pragma inline(pred(mict_ssdb/2)).

mict_ssdb("set_context",       2).
mict_ssdb("handle_event_call", 3).
mict_ssdb("handle_event_exit", 3).
mict_ssdb("handle_event_fail", 3).
mict_ssdb("handle_event_call_nondet", 3).
mict_ssdb("handle_event_exit_nondet", 2).
mict_ssdb("handle_event_redo_nondet", 2).
mict_ssdb("handle_event_fail_nondet", 3).

:- pred mict_stm_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_stm_builtin/2)).

mict_stm_builtin("stm_from_outer_to_inner",     2).
mict_stm_builtin("stm_from_inner_to_outer",     2).
mict_stm_builtin("stm_block",                   1).
mict_stm_builtin("stm_commit",                  1).
mict_stm_builtin("stm_lock",                    0).
mict_stm_builtin("stm_merge_nested_logs",       3).
mict_stm_builtin("stm_unlock",                  0).
mict_stm_builtin("stm_validate",                2).
mict_stm_builtin("stm_create_transaction_log",  1).
mict_stm_builtin("stm_create_nested_transaction_log", 2).
mict_stm_builtin("stm_discard_transaction_log", 1).

:- pred mict_stream(string::in, int::in) is semidet.
:- pragma inline(pred(mict_stream/2)).

mict_stream("put", 4).

:- pred mict_string(string::in, int::in, pred_or_func::in) is semidet.
:- pragma inline(pred(mict_string/3)).

% XXX I (zs) don't know whether we generate calls to the predicate
% or to the function versions of the operations whose pred_or_func arg
% is left as an underscore. It is even possible that we do not even
% generate calls to these three operations. It could be that they are
% on this list because an early version of format_call.m *did* generate
% calls to them, but that code was replaced by code that now generates calls
% to the format_* predicates in string.format.m instead, since these allow
% control of such things as the width and the precision.
mict_string("int_to_string",    2, _).
mict_string("char_to_string",   2, _).
mict_string("float_to_string",  2, _).
mict_string("++",               3, pf_function).

:- pred mict_string_format(string::in, int::in) is semidet.
:- pragma inline(pred(mict_string_format/2)).

mict_string_format("format_cast_int16_to_int", 2).
mict_string_format("format_cast_int32_to_int", 2).
mict_string_format("format_cast_int8_to_int", 2).
mict_string_format("format_cast_uint16_to_uint", 2).
mict_string_format("format_cast_uint32_to_uint", 2).
mict_string_format("format_cast_uint8_to_uint", 2).
mict_string_format("format_char_component_nowidth", 3).
mict_string_format("format_char_component_width", 4).
mict_string_format("format_float_component_nowidth_noprec", 4).
mict_string_format("format_float_component_nowidth_prec", 5).
mict_string_format("format_float_component_width_noprec", 5).
mict_string_format("format_float_component_width_prec", 6).
mict_string_format("format_signed_int64_component_nowidth_noprec", 3).
mict_string_format("format_signed_int64_component_nowidth_prec", 4).
mict_string_format("format_signed_int64_component_width_noprec", 4).
mict_string_format("format_signed_int64_component_width_prec", 5).
mict_string_format("format_signed_int_component_nowidth_noprec", 3).
mict_string_format("format_signed_int_component_nowidth_prec", 4).
mict_string_format("format_signed_int_component_width_noprec", 4).
mict_string_format("format_signed_int_component_width_prec", 5).
mict_string_format("format_string_component_nowidth_noprec", 3).
mict_string_format("format_string_component_nowidth_prec", 4).
mict_string_format("format_string_component_width_noprec", 4).
mict_string_format("format_string_component_width_prec", 5).
mict_string_format("format_uint64_component_nowidth_noprec", 4).
mict_string_format("format_uint64_component_nowidth_prec", 5).
mict_string_format("format_uint64_component_width_noprec", 5).
mict_string_format("format_uint64_component_width_prec", 6).
mict_string_format("format_uint_component_nowidth_noprec", 4).
mict_string_format("format_uint_component_nowidth_prec", 5).
mict_string_format("format_uint_component_width_noprec", 5).
mict_string_format("format_uint_component_width_prec", 6).
mict_string_format("format_unsigned_int64_component_nowidth_noprec", 4).
mict_string_format("format_unsigned_int64_component_nowidth_prec", 5).
mict_string_format("format_unsigned_int64_component_width_noprec", 5).
mict_string_format("format_unsigned_int64_component_width_prec", 6).
mict_string_format("format_unsigned_int_component_nowidth_noprec", 4).
mict_string_format("format_unsigned_int_component_nowidth_prec", 5).
mict_string_format("format_unsigned_int_component_width_noprec", 5).
mict_string_format("format_unsigned_int_component_width_prec", 6).

:- pred mict_table_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_table_builtin/2)).

% XXX The entries here that have an underscore for the arity are entries
% that I (zs) do not believe we generate calls to anymore.
% Instead of generating a call to one of the predicates, even in
% call_foreign_proc form, we generate just C code for them,
% and we then include several such C code fragments in a call_foreign_proc
% that names just one of the predicates involved, passing the arguments
% that only the others need using the extra_args slot.
% However, I could be wrong :-(
mict_table_builtin("generate_simple_call_table_lookup_goal", 3).
mict_table_builtin("table_error", 1).
mict_table_builtin("table_io_copy_io_state", 2).
mict_table_builtin("table_io_has_occurred", 1).
mict_table_builtin("table_io_in_range", 3).
mict_table_builtin("table_io_left_bracket_unitized_goal", 1).
mict_table_builtin("table_io_right_bracket_unitized_goal", 1).
mict_table_builtin("table_lookup_insert_addr", _).
mict_table_builtin("table_lookup_insert_char", _).
mict_table_builtin("table_lookup_insert_enum", _).
mict_table_builtin("table_lookup_insert_float", _).
mict_table_builtin("table_lookup_insert_foreign_enum", _).
mict_table_builtin("table_lookup_insert_gen", _).
mict_table_builtin("table_lookup_insert_int", _).
mict_table_builtin("table_lookup_insert_poly", _).
mict_table_builtin("table_lookup_insert_poly_addr", _).
mict_table_builtin("table_lookup_insert_start_int", 4).
mict_table_builtin("table_lookup_insert_string", _).
mict_table_builtin("table_lookup_insert_typeclassinfo", _).
mict_table_builtin("table_lookup_insert_typeinfo", _).
mict_table_builtin("table_lookup_restore_any_answer", _).
mict_table_builtin("table_lookup_restore_char_answer", _).
mict_table_builtin("table_lookup_restore_float_answer", _).
mict_table_builtin("table_lookup_restore_int_answer", _).
mict_table_builtin("table_lookup_restore_io_state_answer", _).
mict_table_builtin("table_lookup_restore_string_answer", _).
mict_table_builtin("table_lookup_save_any_answer", _).
mict_table_builtin("table_lookup_save_char_answer", _).
mict_table_builtin("table_lookup_save_float_answer", _).
mict_table_builtin("table_lookup_save_int_answer", _).
mict_table_builtin("table_lookup_save_io_state_answer", _).
mict_table_builtin("table_lookup_save_string_answer", _).
mict_table_builtin("table_loop_mark_as_active_and_fail", 1).
mict_table_builtin("table_loop_mark_as_inactive", 1).
mict_table_builtin("table_loop_mark_as_inactive_and_fail", 1).
mict_table_builtin("table_loop_setup", _).
mict_table_builtin("table_loop_setup_shortcut", 3).
mict_table_builtin("table_memo_create_answer_block", _).
mict_table_builtin("table_memo_det_setup", _).
mict_table_builtin("table_memo_det_setup_shortcut", 3).
mict_table_builtin("table_memo_fill_answer_block_shortcut", 1).
mict_table_builtin("table_memo_get_answer_block", _).
mict_table_builtin("table_memo_get_answer_block_shortcut", 1).
mict_table_builtin("table_memo_mark_as_active_and_fail", 1).
mict_table_builtin("table_memo_mark_as_complete_and_fail", 1).
mict_table_builtin("table_memo_mark_as_failed", 1).
mict_table_builtin("table_memo_mark_as_incomplete", 1).
mict_table_builtin("table_memo_mark_as_succeeded", 1).
mict_table_builtin("table_memo_non_answer_is_not_duplicate", _).
mict_table_builtin("table_memo_non_answer_is_not_duplicate_shortcut", 1).
mict_table_builtin("table_memo_non_get_answer_table", _).
mict_table_builtin("table_memo_non_return_all_shortcut", 1).
mict_table_builtin("table_memo_non_setup", 3).
mict_table_builtin("table_memo_return_all_answers_multi", 2).
mict_table_builtin("table_memo_return_all_answers_nondet", 2).
mict_table_builtin("table_memo_semi_setup", _).
mict_table_builtin("table_memo_semi_setup_shortcut", 3).
mict_table_builtin("table_mm_answer_is_not_duplicate", _).
mict_table_builtin("table_mm_answer_is_not_duplicate_shortcut", 1).
mict_table_builtin("table_mm_completion", 1).
mict_table_builtin("table_mm_create_answer_block", _).
mict_table_builtin("table_mm_fill_answer_block_shortcut", _).
mict_table_builtin("table_mm_get_answer_table", _).
mict_table_builtin("table_mm_return_all_multi", 2).
mict_table_builtin("table_mm_return_all_nondet", 2).
mict_table_builtin("table_mm_return_all_shortcut", 1).
mict_table_builtin("table_mm_setup", 3).
mict_table_builtin("table_mm_suspend_consumer", 2).
mict_table_builtin("table_mmos_answer_is_not_duplicate", _).
mict_table_builtin("table_mmos_answer_is_not_duplicate_shortcut", 1).
mict_table_builtin("table_mmos_completion", 1).
mict_table_builtin("table_mmos_consume_next_answer_multi", 2).
mict_table_builtin("table_mmos_consume_next_answer_nondet", 2).
mict_table_builtin("table_mmos_create_answer_block", _).
mict_table_builtin("table_mmos_pickup_inputs", 1).
mict_table_builtin("table_mmos_restore_answers", 1).
mict_table_builtin("table_mmos_return_answer", _).
mict_table_builtin("table_mmos_save_inputs", _).
mict_table_builtin("table_mmos_setup_consumer", 3).

:- pred mict_term_size_prof_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_term_size_prof_builtin/2)).

mict_term_size_prof_builtin("increment_size",       2).
mict_term_size_prof_builtin("term_size_plus",       3).
mict_term_size_prof_builtin("measure_size_acc",     3).
mict_term_size_prof_builtin("measure_size",         2).
mict_term_size_prof_builtin("complexity_is_active", 1).
mict_term_size_prof_builtin("complexity_exit_proc", 1).
mict_term_size_prof_builtin("complexity_fail_proc", 1).
mict_term_size_prof_builtin("complexity_redo_proc", 1).
mict_term_size_prof_builtin("complexity_call_proc", 1).

:- pred mict_univ(string::in, int::in) is semidet.
:- pragma inline(pred(mict_univ/2)).

mict_univ("type_to_univ", 2).

%-----------------------------------------------------------------------------%
:- end_module hlds.introduced_call_table.
%-----------------------------------------------------------------------------%
