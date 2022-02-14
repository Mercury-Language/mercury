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

may_introduce_calls_to(_PredOrFunc, ModuleName, PredName, OrigArity) :-
    % XXX Many operations in the standard library are available
    % both as a function and as a predicate. If the compiler consistently
    % introduces calls to one form but not the other, then the other
    % need not be listed here.
    (
        ModuleName = "builtin"
    ;
        ModuleName = "exception",
        mict_exception(PredName, OrigArity)
    ;
        ( ModuleName = "int"
        ; ModuleName = "uint"
        ; ModuleName = "int8"
        ; ModuleName = "uint8"
        ; ModuleName = "int16"
        ; ModuleName = "uint16"
        ; ModuleName = "int32"
        ; ModuleName = "uint32"
        ; ModuleName = "int64"
        ; ModuleName = "uint64"
        ),
        mict_ints_uints(PredName, OrigArity)
    ;
        ModuleName = "io",
        mict_io(PredName, OrigArity)
    ;
        ModuleName = "private_builtin"
        % mict_private_builtin(PredName, OrigArity)
    ;
        ModuleName = "profiling_builtin"
    ;
        ModuleName = "region_builtin"
    ;
        ModuleName = "ssdb"
    ;
        ModuleName = "stm_builtin"
    ;
        ModuleName = "string",
        mict_string(PredName, OrigArity)
    ;
        ModuleName = "string.format",
        mict_string_format(PredName, OrigArity)
    ;
        ModuleName = "stream",
        mict_stream(PredName, OrigArity)
    ;
        ModuleName = "table_builtin"
        % mict_table_builtin(PredName, OrigArity)
    ;
        ModuleName = "term_size_builtin"
    ).

:- pred mict_exception(string::in, int::in) is semidet.
:- pragma inline(pred(mict_exception/2)).

mict_exception("try",         2).
mict_exception("try_io",      4).
mict_exception("unreachable", 0).

:- pred mict_ints_uints(string::in, int::in) is semidet.
:- pragma inline(pred(mict_ints_uints/2)).

% For some reason, the compiler records the original arity of
% int.unchecked_quotient as 3, not 2. Don't check the arities
% until this is fixed.
%
% XXX The original arity we are given is a pred-form arity,
% which includes the function result.
mict_ints_uints("*", _).
mict_ints_uints("unchecked_quotient", _).
mict_ints_uints("unchecked_rem", _).
mict_ints_uints("unchecked_left_shift", _).
mict_ints_uints("unchecked_right_shift", _).

:- pred mict_io(string::in, int::in) is semidet.
:- pragma inline(pred(mict_io/2)).

mict_io("write_string", _).

:- pred mict_private_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_private_builtin/2)).
:- pragma consider_used(pred(mict_private_builtin/2)).

mict_private_builtin("builtin_compound_eq", _).
mict_private_builtin("builtin_compound_lt", _).
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
mict_private_builtin("partial_inst_copy",   2).

:- pred mict_string(string::in, int::in) is semidet.
:- pragma inline(pred(mict_string/2)).

mict_string("int_to_string",    2).
mict_string("char_to_string",   2).
mict_string("float_to_string",  2).
mict_string("++",               3).

:- pred mict_string_format(string::in, int::in) is semidet.
:- pragma inline(pred(mict_string_format/2)).

mict_string_format("format_char_component_nowidth", _).
mict_string_format("format_char_component_width", _).
mict_string_format("format_string_component_nowidth_noprec", _).
mict_string_format("format_string_component_nowidth_prec", _).
mict_string_format("format_string_component_width_noprec", _).
mict_string_format("format_string_component_width_prec", _).
mict_string_format("format_signed_int_component_nowidth_noprec", _).
mict_string_format("format_signed_int_component_nowidth_prec", _).
mict_string_format("format_signed_int_component_width_noprec", _).
mict_string_format("format_signed_int_component_width_prec", _).
mict_string_format("format_unsigned_int_component_nowidth_noprec", _).
mict_string_format("format_unsigned_int_component_nowidth_prec", _).
mict_string_format("format_unsigned_int_component_width_noprec", _).
mict_string_format("format_unsigned_int_component_width_prec", _).
mict_string_format("format_signed_int64_component_nowidth_noprec", _).
mict_string_format("format_signed_int64_component_nowidth_prec", _).
mict_string_format("format_signed_int64_component_width_noprec", _).
mict_string_format("format_signed_int64_component_width_prec", _).
mict_string_format("format_unsigned_int64_component_nowidth_noprec", _).
mict_string_format("format_unsigned_int64_component_nowidth_prec", _).
mict_string_format("format_unsigned_int64_component_width_noprec", _).
mict_string_format("format_unsigned_int64_component_width_prec", _).
mict_string_format("format_uint_component_nowidth_noprec", _).
mict_string_format("format_uint_component_nowidth_prec", _).
mict_string_format("format_uint_component_width_noprec", _).
mict_string_format("format_uint_component_width_prec", _).
mict_string_format("format_uint64_component_nowidth_noprec", _).
mict_string_format("format_uint64_component_nowidth_prec", _).
mict_string_format("format_uint64_component_width_noprec", _).
mict_string_format("format_uint64_component_width_prec", _).
mict_string_format("format_float_component_nowidth_noprec", _).
mict_string_format("format_float_component_nowidth_prec", _).
mict_string_format("format_float_component_width_noprec", _).
mict_string_format("format_float_component_width_prec", _).
mict_string_format("format_cast_int8_to_int", _).
mict_string_format("format_cast_int16_to_int", _).
mict_string_format("format_cast_int32_to_int", _).
mict_string_format("format_cast_uint8_to_uint", _).
mict_string_format("format_cast_uint16_to_uint", _).
mict_string_format("format_cast_uint32_to_uint", _).

:- pred mict_stream(string::in, int::in) is semidet.
:- pragma inline(pred(mict_stream/2)).

mict_stream("put", _).

:- pred mict_table_builtin(string::in, int::in) is semidet.
:- pragma inline(pred(mict_table_builtin/2)).
:- pragma consider_used(pred(mict_table_builtin/2)).

mict_table_builtin("table_lookup_insert_start_int", _).
mict_table_builtin("table_lookup_insert_int", _).
mict_table_builtin("table_lookup_insert_float", _).
mict_table_builtin("table_lookup_insert_char", _).
mict_table_builtin("table_lookup_insert_string", _).
mict_table_builtin("table_lookup_insert_enum", _).
mict_table_builtin("table_lookup_insert_foreign_enum", _).
mict_table_builtin("table_lookup_insert_gen", _).
mict_table_builtin("table_lookup_insert_addr", _).
mict_table_builtin("table_lookup_insert_poly", _).
mict_table_builtin("table_lookup_insert_poly_addr", _).
mict_table_builtin("table_lookup_insert_typeinfo", _).
mict_table_builtin("table_lookup_insert_typeclassinfo", _).

mict_table_builtin("table_lookup_save_int_answer", _).
mict_table_builtin("table_lookup_save_char_answer", _).
mict_table_builtin("table_lookup_save_string_answer", _).
mict_table_builtin("table_lookup_save_float_answer", _).
mict_table_builtin("table_lookup_save_io_state_answer", _).
mict_table_builtin("table_lookup_save_any_answer", _).

mict_table_builtin("table_lookup_restore_int_answer", _).
mict_table_builtin("table_lookup_restore_char_answer", _).
mict_table_builtin("table_lookup_restore_string_answer", _).
mict_table_builtin("table_lookup_restore_float_answer", _).
mict_table_builtin("table_lookup_restore_io_state_answer", _).
mict_table_builtin("table_lookup_restore_any_answer", _).

mict_table_builtin("table_loop_setup", _).
mict_table_builtin("table_loop_setup_shortcut", _).
mict_table_builtin("table_loop_mark_as_inactive", _).
mict_table_builtin("table_loop_mark_as_inactive_and_fail", _).
mict_table_builtin("table_loop_mark_as_active_and_fail", _).

mict_table_builtin("table_memo_det_setup", _).
mict_table_builtin("table_memo_det_setup_shortcut", _).
mict_table_builtin("table_memo_semi_setup", _).
mict_table_builtin("table_memo_semi_setup_shortcut", _).
mict_table_builtin("table_memo_non_setup", _).
mict_table_builtin("table_memo_mark_as_failed", _).
mict_table_builtin("table_memo_mark_as_succeeded", _).
mict_table_builtin("table_memo_mark_as_incomplete", _).
mict_table_builtin("table_memo_mark_as_active_and_fail", _).
mict_table_builtin("table_memo_mark_as_complete_and_fail", _).
mict_table_builtin("table_memo_create_answer_block", _).
mict_table_builtin("table_memo_get_answer_block", _).
mict_table_builtin("table_memo_non_get_answer_table", _).
mict_table_builtin("table_memo_non_answer_is_not_duplicate", _).
mict_table_builtin("table_memo_non_answer_is_not_duplicate_shortcut", _).
mict_table_builtin("table_memo_return_all_answers_nondet", _).
mict_table_builtin("table_memo_return_all_answers_multi", _).
mict_table_builtin("table_memo_non_return_all_shortcut", _).

mict_table_builtin("table_io_in_range", _).
mict_table_builtin("table_io_has_occurred", _).
mict_table_builtin("table_io_copy_io_state", _).
mict_table_builtin("table_io_left_bracket_unitized_goal", _).
mict_table_builtin("table_io_right_bracket_unitized_goal", _).

mict_table_builtin("table_mm_setup", _).
mict_table_builtin("table_mm_suspend_consumer", _).
mict_table_builtin("table_mm_completion", _).
mict_table_builtin("table_mm_get_answer_table", _).
mict_table_builtin("table_mm_answer_is_not_duplicate", _).
mict_table_builtin("table_mm_answer_is_not_duplicate_shortcut", _).
mict_table_builtin("table_mm_create_answer_block", _).
mict_table_builtin("table_mm_fill_answer_block_shortcut", _).
mict_table_builtin("table_mm_return_all_nondet", _).
mict_table_builtin("table_mm_return_all_multi", _).
mict_table_builtin("table_mm_return_all_shortcut", _).

mict_table_builtin("table_mmos_save_inputs", _).
mict_table_builtin("table_mmos_setup_consumer", _).
mict_table_builtin("table_mmos_answer_is_not_duplicate", _).
mict_table_builtin("table_mmos_answer_is_not_duplicate_shortcut", _).
mict_table_builtin("table_mmos_consume_next_answer_nondet", _).
mict_table_builtin("table_mmos_consume_next_answer_multi", _).
mict_table_builtin("table_mmos_restore_answers", _).
mict_table_builtin("table_mmos_pickup_inputs", _).
mict_table_builtin("table_mmos_create_answer_block", _).
mict_table_builtin("table_mmos_return_answer", _).
mict_table_builtin("table_mmos_completion", _).

mict_table_builtin("table_error", _).

%-----------------------------------------------------------------------------%
:- end_module hlds.introduced_call_table.
%-----------------------------------------------------------------------------%
