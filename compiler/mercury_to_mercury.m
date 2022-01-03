%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_to_mercury.m.
% Main author: fjh.
%
% This module converts miscellaneous parts of the parse tree structure
% back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.mercury_to_mercury.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- func mercury_type_list_to_string(tvarset, list(mer_type)) = string.

:- pred mercury_output_type(tvarset::in, var_name_print::in, mer_type::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_type_to_string(tvarset, var_name_print, mer_type) = string.
:- pred mercury_format_type(tvarset::in, var_name_print::in, mer_type::in,
    S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_output_det(determinism::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- func mercury_det_to_string(determinism) = string.
:- pred mercury_format_det(determinism::in, S::in, U::di, U::uo) is det
    <= output(S, U).

%---------------------------------------------------------------------------%

    % Output an existential quantifier.
    %
:- pred mercury_output_quantifier(tvarset::in, var_name_print::in,
    existq_tvars::in, io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_quantifier_to_string(tvarset, var_name_print, existq_tvars)
    = string.
:- pred mercury_format_quantifier(tvarset::in, var_name_print::in,
    existq_tvars::in, S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%

    % Similar to mercury_output_vars/3, but prefixes each variable
    % with `!' to indicate that it is a state variable.
    %
:- pred mercury_output_state_vars(varset(T)::in, var_name_print::in,
    list(var(T))::in, io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Output a cons_id, parenthesizing it if necessary.
    %
:- pred mercury_output_cons_id(output_lang::in, needs_brackets::in,
    cons_id::in, io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_cons_id_to_string(output_lang, needs_brackets, cons_id)
    = string.
:- pred mercury_format_cons_id(output_lang::in, needs_brackets::in,
    cons_id::in, S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%
%
% Output sym_names, maybe with their arities.
%
% Use mercury_output_bracketed_sym_name when the sym_name has no arguments,
% otherwise use mercury_output_sym_name.
%

:- type needs_brackets
    --->    needs_brackets
            % Needs brackets, if it is an op.
    ;       does_not_need_brackets.
            % Doesn't need brackets.

:- pred mercury_output_sym_name(sym_name::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- func mercury_sym_name_to_string(sym_name) = string.
:- pred mercury_format_sym_name(sym_name::in, S::in,
    U::di, U::uo) is det <= output(S, U).

:- pred mercury_format_sym_name_ngt(needs_quotes::in, sym_name::in, S::in,
    U::di, U::uo) is det <= output(S, U).

:- func mercury_sym_name_arity_to_string(sym_name_arity) = string.
:- pred mercury_format_sym_name_arity(sym_name_arity::in, S::in,
    U::di, U::uo) is det <= output(S, U).

%---------------------%

:- pred mercury_output_bracketed_sym_name(sym_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_bracketed_sym_name_to_string(sym_name) = string.
:- pred mercury_format_bracketed_sym_name(sym_name::in, S::in,
    U::di, U::uo) is det <= output(S, U).

%---------------------%

:- pred mercury_output_bracketed_sym_name_ngt(needs_quotes::in, sym_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_bracketed_sym_name_to_string_ngt(needs_quotes, sym_name)
    = string.
:- pred mercury_format_bracketed_sym_name_ngt(needs_quotes::in, sym_name::in,
    S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_output_constraint(tvarset::in, var_name_print::in,
    prog_constraint::in, io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_constraint_to_string(tvarset, prog_constraint) = string.
:- pred mercury_format_constraint(tvarset::in, var_name_print::in,
    prog_constraint::in, S::in, U::di, U::uo) is det <= output(S, U).

:- func mercury_prog_constraint_list_to_string(tvarset, var_name_print,
    string, list(prog_constraint)) = string.
:- pred mercury_format_prog_constraint_list(tvarset::in, var_name_print::in,
    string::in, list(prog_constraint)::in, S::in, U::di, U::uo) is det
    <= output(S, U).

:- type maybe_exist_constraints
    --->    no_exist_constraints
    ;       have_exist_constraints_print_paren(list(prog_constraint)).

    % mercury_output_class_context(TVarSet, VarNamePrint,
    %   UnivConstraints, MaybeExistConstraints, !IO)
    % mercury_format_class_context(TVarSet, VarNamePrint,
    %   UnivConstraints, MaybeExistConstraints, !U)
    %
:- pred mercury_output_class_context(tvarset::in, var_name_print::in,
    list(prog_constraint)::in, maybe_exist_constraints::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_class_context(tvarset::in, var_name_print::in,
    list(prog_constraint)::in, maybe_exist_constraints::in,
    S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_output_foreign_language_string(foreign_language::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_foreign_language_to_string(foreign_language) = string.
:- pred mercury_format_foreign_language_string(foreign_language::in,
    S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_output_newline(int::in, io.text_output_stream::in,
    io::di, io::uo) is det.

:- pred mercury_format_tabs(int::in, S::in, U::di, U::uo) is det
    <= output(S, U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_to_term.

:- import_module int.
:- import_module string.
:- import_module term_io.
:- import_module unit.

%---------------------------------------------------------------------------%

mercury_type_list_to_string(_, []) = "".
mercury_type_list_to_string(VarSet, [Type | Types]) = String :-
    HeadString = mercury_type_to_string(VarSet, print_name_only, Type),
    TailString = mercury_comma_type_list_to_string(VarSet, Types),
    String = HeadString ++ TailString.

:- func mercury_comma_type_list_to_string(tvarset, list(mer_type)) = string.

mercury_comma_type_list_to_string(_, []) = "".
mercury_comma_type_list_to_string(VarSet, [Type | Types]) = String :-
    HeadString = mercury_type_to_string(VarSet, print_name_only, Type),
    TailString = mercury_comma_type_list_to_string(VarSet, Types),
    String = ", " ++ HeadString ++ TailString.

mercury_output_type(VarSet, VarNamePrint, Type, Stream, !IO) :-
    mercury_format_type(VarSet, VarNamePrint, Type, Stream, !IO).

mercury_type_to_string(VarSet, VarNamePrint, Type) = String :-
    mercury_format_type(VarSet, VarNamePrint, Type, unit, "", String).

mercury_format_type(TypeVarSet, VarNamePrint, Type, S, !U) :-
    % We convert to a term and then use mercury_format_term. The reason
    % for this is that we have to be very careful about handling operators
    % and precedence properly, and it is better to have the code to manage
    % that in one place, rather than duplicated here.
    %
    unparse_type(Type, Term),
    VarSet = varset.coerce(TypeVarSet),
    mercury_format_term(VarSet, VarNamePrint, Term, S, !U).

%---------------------------------------------------------------------------%

mercury_output_det(Detism, Stream, !UI) :-
    mercury_format_det(Detism, Stream, !UI).

mercury_det_to_string(detism_det) = "det".
mercury_det_to_string(detism_semi) = "semidet".
mercury_det_to_string(detism_non) = "nondet".
mercury_det_to_string(detism_multi) = "multi".
mercury_det_to_string(detism_cc_multi) = "cc_multi".
mercury_det_to_string(detism_cc_non) = "cc_nondet".
mercury_det_to_string(detism_failure) = "failure".
mercury_det_to_string(detism_erroneous) = "erroneous".

mercury_format_det(Detism, S, !U) :-
    add_string(mercury_det_to_string(Detism), S, !U).

%---------------------------------------------------------------------------%

mercury_output_quantifier(TypeVarSet, VarNamePrint, ExistQVars, Stream, !IO) :-
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars,
        Stream, !IO).

mercury_quantifier_to_string(TypeVarSet, VarNamePrint, ExistQVars) = String :-
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars,
        unit, "", String).

mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars, S, !U) :-
    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        add_string("some [", S, !U),
        mercury_format_vars(TypeVarSet, VarNamePrint, ExistQVars, S, !U),
        add_string("] ", S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_state_vars(VarSet, VarNamePrint, StateVars, Stream, !IO) :-
    write_out_list(mercury_output_state_var(VarSet, VarNamePrint),
        ", ", StateVars, Stream, !IO).

:- pred mercury_output_state_var(varset(T)::in, var_name_print::in, var(T)::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_state_var(VarSet, VarNamePrint, Var, Stream, !IO) :-
    io.write_string(Stream, "!", !IO),
    mercury_output_var(VarSet, VarNamePrint, Var, Stream, !IO).

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
        ConsId = base_typeclass_info_const(ModuleName, ClassId, InstanceNum,
            InstanceString),
        ModuleString = sym_name_to_string(ModuleName),
        ClassId = class_id(ClassName, ClassArity),
        add_string("<base_typeclass_info for ", S, !U),
        add_string("class_id(", S, !U),
        mercury_format_sym_name(ClassName, S, !U),
        add_string(", ", S, !U),
        add_int(ClassArity, S, !U),
        add_string(")", S, !U),
        ( if ModuleString \= "some bogus module name" then
            add_strings([" from module ", ModuleString], S, !U)
        else
            true
        ),
        add_format(", instance number %d (%s)>",
            [i(InstanceNum), s(InstanceString)], S, !U)
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

mercury_output_sym_name(SymName, Stream, !IO) :-
    mercury_format_sym_name_ngt(not_next_to_graphic_token, SymName,
        Stream, !IO).

mercury_sym_name_to_string(SymName) = Str :-
    mercury_format_sym_name(SymName, unit, "", Str).

mercury_format_sym_name(SymName, S, !U) :-
    mercury_format_sym_name_ngt(not_next_to_graphic_token, SymName, S, !U).

mercury_format_sym_name_ngt(NextToGraphicToken, SymName, S, !U) :-
    (
        SymName = qualified(ModuleName, PredName),
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token,
            ModuleName, S, !U),
        add_string(".", S, !U),
        mercury_format_quoted_atom(next_to_graphic_token, PredName, S, !U)
    ;
        SymName = unqualified(PredName),
        mercury_format_quoted_atom(NextToGraphicToken, PredName, S, !U)
    ).

%---------------------%

mercury_sym_name_arity_to_string(SNA) = Str :-
    mercury_format_sym_name_arity(SNA, unit, "", Str).

mercury_format_sym_name_arity(sym_name_arity(SymName, Arity), S, !U) :-
    mercury_format_sym_name(SymName, S, !U),
    add_char('/', S, !U),
    add_int(Arity, S, !U).

%---------------------%

mercury_output_bracketed_sym_name(SymName, Stream, !IO) :-
    mercury_output_bracketed_sym_name_ngt(not_next_to_graphic_token, SymName,
        Stream, !IO).

mercury_bracketed_sym_name_to_string(SymName) =
    mercury_bracketed_sym_name_to_string_ngt(not_next_to_graphic_token,
        SymName).

mercury_format_bracketed_sym_name(SymName, S, !U) :-
    mercury_format_bracketed_sym_name_ngt(not_next_to_graphic_token, SymName,
        S, !U).

%---------------------%

mercury_output_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        Stream, !IO) :-
    mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        Stream, !IO).

mercury_bracketed_sym_name_to_string_ngt(NextToGraphicToken, SymName) = Str :-
    mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        unit, "", Str).

mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName, S, !U) :-
    (
        SymName = qualified(ModuleName, Name),
        add_string("(", S, !U),
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token,
            ModuleName, S, !U),
        add_string(".", S, !U),
        mercury_format_bracketed_atom(next_to_graphic_token, Name, S, !U),
        add_string(")", S, !U)
    ;
        SymName = unqualified(Name),
        mercury_format_bracketed_atom(NextToGraphicToken, Name, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_constraint(TypeVarSet, VarNamePrint, Constraint, Stream, !IO) :-
    mercury_format_constraint(TypeVarSet, VarNamePrint,
        Constraint, Stream, !IO).

mercury_constraint_to_string(TypeVarSet, Constraint) = String :-
    mercury_format_constraint(TypeVarSet, print_name_only, Constraint,
        unit, "", String).

mercury_format_constraint(TypeVarSet, VarNamePrint, Constraint, S, !U) :-
    Constraint = constraint(Name, Types),
    mercury_format_sym_name(Name, S, !U),
    add_string("(", S, !U),
    add_list(mercury_format_type(TypeVarSet, VarNamePrint), ", ", Types,
        S, !U),
    add_string(")", S, !U).

mercury_prog_constraint_list_to_string(TypeVarSet, VarNamePrint,
        Operator, Constraints) = String :-
    mercury_format_prog_constraint_list(TypeVarSet, VarNamePrint,
        Operator, Constraints, unit, "", String).

mercury_format_prog_constraint_list(TypeVarSet, VarNamePrint, Operator,
        Constraints, S, !U) :-
    (
        Constraints = []
    ;
        Constraints = [_ | _],
        add_strings([" ", Operator, " ("], S, !U),
        add_list(mercury_format_constraint(TypeVarSet, VarNamePrint),
            ", ", Constraints, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_class_context(TypeVarSet, VarNamePrint,
        UnivConstraints, MaybeExistConstraints, Stream, !IO) :-
    mercury_format_class_context(TypeVarSet, VarNamePrint,
        UnivConstraints, MaybeExistConstraints, Stream, !IO).

mercury_format_class_context(TypeVarSet, VarNamePrint,
        UnivConstraints, MaybeExistConstraints, S, !U) :-
    (
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints =
            have_exist_constraints_print_paren(ExistConstraints),
        mercury_format_prog_constraint_list(TypeVarSet, VarNamePrint, "=>",
            ExistConstraints, S, !U),
        % The code that passed us have_exist_constraints_print_paren
        % should have printed the open paren that this closes.
        add_string(")", S, !U)
    ),
    mercury_format_prog_constraint_list(TypeVarSet, VarNamePrint, "<=",
        UnivConstraints, S, !U).

%---------------------------------------------------------------------------%

mercury_output_foreign_language_string(Lang, Stream, !IO) :-
    mercury_format_foreign_language_string(Lang, Stream, !IO).

mercury_foreign_language_to_string(Lang) = String :-
    mercury_format_foreign_language_string(Lang, unit, "", String).

mercury_format_foreign_language_string(Lang, S, !U) :-
    add_string("""" ++ foreign_language_string(Lang) ++ """", S, !U).

%---------------------------------------------------------------------------%

mercury_output_newline(Indent, Stream, !IO) :-
    io.write_char(Stream, '\n', !IO),
    mercury_format_tabs(Indent, Stream, !IO).

mercury_format_tabs(Indent, S, !U) :-
    ( if Indent > 0 then
        add_string("\t", S, !U),
        mercury_format_tabs(Indent - 1, S, !U)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.mercury_to_mercury.
%---------------------------------------------------------------------------%
