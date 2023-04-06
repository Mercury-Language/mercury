%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_tree_out_cons_id.m.
% Main author: fjh.
%
% This module converts cons_ids that can occur in Mercury source code
% back into Mercury source text. (For cons_ids that can be constructed
% only by the compiler, the result won't be valid Mercury code.)
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_cons_id.
:- interface.

:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module io.

%---------------------------------------------------------------------------%

:- type needs_brackets
    --->    needs_brackets
            % Needs brackets, if it is an op.
    ;       does_not_need_brackets.
            % Doesn't need brackets.

    % Output a cons_id, parenthesizing it if necessary.
    %
:- pred mercury_output_cons_id(output_lang::in, needs_brackets::in,
    cons_id::in, io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_cons_id_to_string(output_lang, needs_brackets, cons_id)
    = string.
:- pred mercury_format_cons_id(output_lang::in, needs_brackets::in,
    cons_id::in, S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_sym_name.

:- import_module list.
:- import_module string.
:- import_module term_io.
:- import_module unit.

%---------------------------------------------------------------------------%

mercury_output_cons_id(Lang, NeedsBrackets, ConsId, Stream, !IO) :-
    mercury_format_cons_id(Lang, NeedsBrackets, ConsId, Stream, !IO).

mercury_cons_id_to_string(Lang, NeedsBrackets, ConsId) = String :-
    mercury_format_cons_id(Lang, NeedsBrackets, ConsId, unit, "", String).

mercury_format_cons_id(Lang, NeedsBrackets, ConsId, S, !U) :-
    (
        ConsId = cons(Name, _, _),
        (
            NeedsBrackets = needs_brackets,
            mercury_format_bracketed_sym_name(Name, S, !U)
        ;
            NeedsBrackets = does_not_need_brackets,
            mercury_format_sym_name(Name, S, !U)
        )
    ;
        ConsId = tuple_cons(_),
        (
            Lang = output_mercury,
            add_string("{}", S, !U)
        ;
            Lang = output_debug,
            add_string("tuple{}", S, !U)
        )
    ;
        ConsId = some_int_const(IntConst),
        (
            IntConst = int_const(Int),
            add_int(Int, S, !U)
        ;
            IntConst = uint_const(UInt),
            add_uint(UInt, S, !U)
        ;
            IntConst = int8_const(Int8),
            add_int8(Int8, S, !U)
        ;
            IntConst = uint8_const(UInt8),
            add_uint8(UInt8, S, !U)
        ;
            IntConst = int16_const(Int16),
            add_int16(Int16, S, !U)
        ;
            IntConst = uint16_const(UInt16),
            add_uint16(UInt16, S, !U)
        ;
            IntConst = int32_const(Int32),
            add_int32(Int32, S, !U)
        ;
            IntConst = uint32_const(UInt32),
            add_uint32(UInt32, S, !U)
        ;
            IntConst = int64_const(Int64),
            add_int64(Int64, S, !U)
        ;
            IntConst = uint64_const(UInt64),
            add_uint64(UInt64, S, !U)
        )
    ;
        ConsId = float_const(Float),
        add_float(Float, S, !U)
    ;
        ConsId = char_const(Char),
        add_string(term_io.quoted_char(Char), S, !U)
    ;
        ConsId = string_const(Str),
        add_quoted_string(Str, S, !U)
    ;
        ConsId = impl_defined_const(IDCKind),
        add_string(impl_defined_const_kind_to_str(IDCKind), S, !U)
    ;
        ConsId = closure_cons(ShroudedPredProcId, _EvalMethod),
        % XXX Should probably print this out in name/arity form.
        ShroudedPredProcId = shrouded_pred_proc_id(PredInt, ProcInt),
        add_string("<closure_cons(", S, !U),
        add_int(PredInt, S, !U),
        add_string(", ", S, !U),
        add_int(ProcInt, S, !U),
        % add_string(", ", S, !U),
        % add_lambda_eval_method(EvalMethod, S, !U),
        add_string(")>", S, !U)
    ;
        ConsId = type_ctor_info_const(ModuleName, Type, Arity),
        ModuleString = sym_name_to_string(ModuleName),
        string.int_to_string(Arity, ArityString),
        add_strings(["<type_ctor_info for ",
            ModuleString, ".", Type, "/", ArityString, ">"], S, !U)
    ;
        ConsId = base_typeclass_info_const(ModuleSymName, ClassId,
            InstanceNum, InstanceStr),
        ModuleNameStr = sym_name_to_string(ModuleSymName),
        ClassId = class_id(ClassName, ClassArity),
        string.format("class_id(%s, %d)",
            [s(mercury_sym_name_to_string(ClassName)), i(ClassArity)],
            ClassStr),
        string.format("from module %s, instance number %d (%s)",
            [s(ModuleNameStr), i(InstanceNum), s(InstanceStr)],
            ModuleInstanceStr),
        string.format("<base_typeclass_info for %s, %s>",
            [s(ClassStr), s(ModuleInstanceStr)], ConsIdStr),
        add_string(ConsIdStr, S, !U)
    ;
        ConsId = type_info_cell_constructor(_),
        add_string("<type_info_cell_constructor>", S, !U)
    ;
        ConsId = typeclass_info_cell_constructor,
        add_string("<typeclass_info_cell_constructor>", S, !U)
    ;
        ConsId = type_info_const(TIConstNum),
        add_string("<type_info_cell_constructor " ++
            int_to_string(TIConstNum) ++ ">", S, !U)
    ;
        ConsId = typeclass_info_const(TCIConstNum),
        add_string("<typeclass_info_cell_constructor " ++
            int_to_string(TCIConstNum) ++ ">", S, !U)
    ;
        ConsId = ground_term_const(ConstNum, SubConsId),
        add_string("<ground_term_cell_constructor " ++
            int_to_string(ConstNum) ++ ", ", S, !U),
        mercury_format_cons_id(Lang, does_not_need_brackets, SubConsId, S, !U),
        add_string(">", S, !U)
    ;
        ConsId = tabling_info_const(_),
        add_string("<tabling info>", S, !U)
    ;
        ConsId = table_io_entry_desc(_),
        add_string("<table_io_entry_desc>", S, !U)
    ;
        ConsId = deep_profiling_proc_layout(_),
        add_string("<deep_profiling_proc_layout>", S, !U)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_cons_id.
%---------------------------------------------------------------------------%
