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
% NOTE All the predicates and functions below whose names have
% the "mercury_" prefix were originally in a module, mercury_to_mercury.m,
% whose documentation said that it "converts cons_ids back into
% Mercury source text", though this claim was true only for cons_ids
% that *can* appear in Mercury source text. (For cons_ids that can be
% constructed only by the compiler, the result won't be valid Mercury code.)
%
% The other predicates and functions originally came from prog_out.m,
% which made no such claim.
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
:- func mercury_cons_id_to_string(output_lang, needs_brackets, cons_id)
    = string.
:- pred mercury_output_cons_id(output_lang::in, needs_brackets::in,
    cons_id::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_cons_id(output_lang::in, needs_brackets::in,
    cons_id::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % Convert a cons_id to a string.
    %
    % The maybe_quoted_cons_id_and_arity_to_string version is for use
    % in error messages, while the cons_id_and_arity_to_string version
    % is for use when generating target language code. The differences are
    % that
    %
    % - the former puts quotation marks around user-defined cons_ids
    %   (i.e. those that are represented by cons/3), as opposed to
    %   builtin cons_ids such as integers, while the latter does not, and
    %
    % - the latter mangles user-defined cons_ids to ensure that they
    %   are acceptable in our target languages e.g. in comments,
    %   while the former does no mangling.
    %
    % The difference in the names refers to the first distinction above.
    %
:- func maybe_quoted_cons_id_and_arity_to_string(cons_id) = string.
:- func cons_id_and_arity_to_string(cons_id) = string.

:- pred int_const_to_string_and_suffix(some_int_const::in,
    string::out, string::out) is det.

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

mercury_cons_id_to_string(Lang, NeedsBrackets, ConsId) = String :-
    mercury_format_cons_id(Lang, NeedsBrackets, ConsId, unit, "", String).

mercury_output_cons_id(Lang, NeedsBrackets, ConsId, Stream, !IO) :-
    mercury_format_cons_id(Lang, NeedsBrackets, ConsId, Stream, !IO).

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
        add_string(term_io.quoted_char_to_string(Char), S, !U)
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

maybe_quoted_cons_id_and_arity_to_string(ConsId) =
    cons_id_and_arity_to_string_maybe_quoted(dont_mangle_cons, quote_cons,
        ConsId).

cons_id_and_arity_to_string(ConsId) =
    cons_id_and_arity_to_string_maybe_quoted(mangle_cons, dont_quote_cons,
        ConsId).

:- type maybe_quote_cons
    --->    dont_quote_cons
    ;       quote_cons.

:- type maybe_mangle_cons
    --->    dont_mangle_cons
    ;       mangle_cons.

:- func cons_id_and_arity_to_string_maybe_quoted(maybe_mangle_cons,
    maybe_quote_cons, cons_id) = string.

cons_id_and_arity_to_string_maybe_quoted(MangleCons, QuoteCons, ConsId)
        = String :-
    (
        ConsId = cons(SymName, Arity, _TypeCtor),
        SymNameString0 = sym_name_to_string(SymName),
        (
            MangleCons = dont_mangle_cons,
            SymNameString = SymNameString0
        ;
            MangleCons = mangle_cons,
            ( if string.contains_char(SymNameString0, '*') then
                % We need to protect against the * appearing next to a /.
                Stuff =
                    ( pred(Char::in, Str0::in, Str::out) is det :-
                        ( if Char = ('*') then
                            string.append(Str0, "star", Str)
                        else
                            string.char_to_string(Char, CharStr),
                            string.append(Str0, CharStr, Str)
                        )
                    ),
                string.foldl(Stuff, SymNameString0, "", SymNameString1)
            else
                SymNameString1 = SymNameString0
            ),
            SymNameString = term_io.escaped_string(SymNameString1)
        ),
        string.int_to_string(Arity, ArityString),
        (
            QuoteCons = dont_quote_cons,
            String = SymNameString ++ "/" ++ ArityString
        ;
            QuoteCons = quote_cons,
            String = "`" ++ SymNameString ++ "'/" ++ ArityString
        )
    ;
        ConsId = tuple_cons(Arity),
        String = "{}/" ++ string.int_to_string(Arity)
    ;
        ConsId = some_int_const(IntConst),
        int_const_to_string_and_suffix(IntConst, String, _Suffix)
    ;
        ConsId = float_const(Float),
        String = float_to_string(Float)
    ;
        ConsId = char_const(CharConst),
        String = term_io.quoted_char_to_string(CharConst)
    ;
        ConsId = string_const(StringConst),
        String = term_io.quoted_string(StringConst)
    ;
        ConsId = impl_defined_const(IDCKind),
        (
            QuoteCons = dont_quote_cons,
            String = impl_defined_const_kind_to_str(IDCKind)
        ;
            QuoteCons = quote_cons,
            String = "`" ++ impl_defined_const_kind_to_str(IDCKind) ++ "'"
        )
    ;
        ConsId = closure_cons(PredProcId, _),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "closure_cons<pred " ++ int_to_string(PredId) ++
            " proc " ++ int_to_string(ProcId) ++ ">"
    ;
        ConsId = type_ctor_info_const(Module, Ctor, Arity),
        String =
            "<type_ctor_info " ++ sym_name_to_string(Module) ++ "." ++
            Ctor ++ "/" ++ int_to_string(Arity) ++ ">"
    ;
        ConsId = base_typeclass_info_const(_, _, _, _),
        String = "<base_typeclass_info>"
    ;
        ConsId = type_info_cell_constructor(_),
        String = "<type_info_cell_constructor>"
    ;
        ConsId = typeclass_info_cell_constructor,
        String = "<typeclass_info_cell_constructor>"
    ;
        ConsId = type_info_const(_),
        String = "<type_info_const>"
    ;
        ConsId = typeclass_info_const(_),
        String = "<typeclass_info_const>"
    ;
        ConsId = ground_term_const(_, _),
        String = "<ground_term_const>"
    ;
        ConsId = tabling_info_const(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<tabling_info " ++ int_to_string(PredId) ++
            ", " ++ int_to_string(ProcId) ++ ">"
    ;
        ConsId = table_io_entry_desc(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<table_io_entry_desc " ++ int_to_string(PredId) ++ ", " ++
            int_to_string(ProcId) ++ ">"
    ;
        ConsId = deep_profiling_proc_layout(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<deep_profiling_proc_layout " ++ int_to_string(PredId) ++ ", " ++
            int_to_string(ProcId) ++ ">"
    ).

int_const_to_string_and_suffix(IntConst, Str, Suffix) :-
    (
        IntConst = int_const(Int),
        Str = string.int_to_string(Int),        Suffix = ""
    ;
        IntConst = uint_const(UInt),
        Str = string.uint_to_string(UInt),      Suffix = "u"
    ;
        IntConst = int8_const(Int8),
        Str = string.int8_to_string(Int8),      Suffix = "i8"
    ;
        IntConst = uint8_const(UInt8),
        Str = string.uint8_to_string(UInt8),    Suffix = "u8"
    ;
        IntConst = int16_const(Int16),
        Str = string.int16_to_string(Int16),    Suffix = "i16"
    ;
        IntConst = uint16_const(UInt16),
        Str = string.uint16_to_string(UInt16),  Suffix = "u16"
    ;
        IntConst = int32_const(Int32),
        Str = string.int32_to_string(Int32),    Suffix = "i32"
    ;
        IntConst = uint32_const(UInt32),
        Str = string.uint32_to_string(UInt32),  Suffix = "u32"
    ;
        IntConst = int64_const(Int64),
        Str = string.int64_to_string(Int64),    Suffix = "i64"
    ;
        IntConst = uint64_const(UInt64),
        Str = string.uint64_to_string(UInt64),  Suffix = "u64"
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_cons_id.
%---------------------------------------------------------------------------%
