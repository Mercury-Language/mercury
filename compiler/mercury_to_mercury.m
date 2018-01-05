%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
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
    io::di, io::uo) is det.
:- func mercury_type_to_string(tvarset, var_name_print, mer_type) = string.
:- pred mercury_format_type(tvarset::in, var_name_print::in, mer_type::in,
    U::di, U::uo) is det <= output(U).

%---------------------------------------------------------------------------%

:- pred mercury_output_det(determinism::in, io::di, io::uo) is det.
:- func mercury_det_to_string(determinism) = string.
:- pred mercury_format_det(determinism::in, U::di, U::uo) is det <= output(U).

%---------------------------------------------------------------------------%

    % Output an existential quantifier.
    %
:- pred mercury_output_quantifier(tvarset::in, var_name_print::in,
    existq_tvars::in, io::di, io::uo) is det.
:- func mercury_quantifier_to_string(tvarset, var_name_print, existq_tvars)
    = string.
:- pred mercury_format_quantifier(tvarset::in, var_name_print::in,
    existq_tvars::in, U::di, U::uo) is det <= output(U).

%---------------------------------------------------------------------------%

    % Similar to mercury_output_vars/3, but prefixes each variable
    % with `!' to indicate that it is a state variable.
    %
:- pred mercury_output_state_vars(varset(T)::in, var_name_print::in,
    list(var(T))::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Output a cons_id, parenthesizing it if necessary.
    %
:- pred mercury_output_cons_id(needs_brackets::in, cons_id::in,
    io::di, io::uo) is det.
:- func mercury_cons_id_to_string(needs_brackets, cons_id) = string.
:- pred mercury_format_cons_id(needs_brackets::in, cons_id::in, U::di, U::uo)
    is det <= output(U).

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

:- pred mercury_output_sym_name(sym_name::in, io::di, io::uo) is det.
:- pred mercury_format_sym_name(sym_name::in, U::di, U::uo)
    is det <= output(U).

:- pred mercury_format_sym_name_ngt(needs_quotes::in, sym_name::in,
    U::di, U::uo) is det <= output(U).

:- pred mercury_format_sym_name_and_arity(sym_name_and_arity::in, U::di, U::uo)
    is det <= output(U).

%---------------------%

:- pred mercury_output_bracketed_sym_name(sym_name::in,
    io::di, io::uo) is det.
:- func mercury_bracketed_sym_name_to_string(sym_name) = string.
:- pred mercury_format_bracketed_sym_name(sym_name::in,
    U::di, U::uo) is det <= output(U).

%---------------------%

:- pred mercury_output_bracketed_sym_name_ngt(needs_quotes::in, sym_name::in,
    io::di, io::uo) is det.
:- func mercury_bracketed_sym_name_to_string_ngt(needs_quotes, sym_name)
    = string.
:- pred mercury_format_bracketed_sym_name_ngt(needs_quotes::in, sym_name::in,
    U::di, U::uo) is det <= output(U).

%---------------------------------------------------------------------------%

:- pred mercury_output_constraint(tvarset::in, var_name_print::in,
    prog_constraint::in, io::di, io::uo) is det.
:- func mercury_constraint_to_string(tvarset, prog_constraint) = string.
:- pred mercury_format_constraint(tvarset::in, var_name_print::in,
    prog_constraint::in, U::di, U::uo) is det <= output(U).

:- pred mercury_format_prog_constraint_list(tvarset::in, var_name_print::in,
    string::in, list(prog_constraint)::in, U::di, U::uo) is det
    <= output(U).

:- pred mercury_output_class_context(tvarset::in, var_name_print::in,
    prog_constraints::in, existq_tvars::in, io::di, io::uo) is det.
:- pred mercury_format_class_context(tvarset::in, var_name_print::in,
    prog_constraints::in, existq_tvars::in, U::di, U::uo) is det <= output(U).

%---------------------------------------------------------------------------%

:- pred mercury_output_foreign_language_string(foreign_language::in,
    io::di, io::uo) is det.
:- func mercury_foreign_language_to_string(foreign_language) = string.
:- pred mercury_format_foreign_language_string(foreign_language::in,
    U::di, U::uo) is det <= output(U).

%---------------------------------------------------------------------------%

:- pred mercury_output_newline(int::in, io::di, io::uo) is det.

:- pred mercury_format_tabs(int::in,
    U::di, U::uo) is det <= output(U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_to_term.

:- import_module int.
:- import_module string.
:- import_module term_io.

%---------------------------------------------------------------------------%

mercury_type_list_to_string(_, []) = "".
mercury_type_list_to_string(VarSet, [Type | Types]) = String :-
    String0 = mercury_type_to_string(VarSet, print_name_only, Type),
    String1 = mercury_comma_type_list_to_string(VarSet, Types),
    string.append(String0, String1, String).

:- func mercury_comma_type_list_to_string(tvarset, list(mer_type)) = string.

mercury_comma_type_list_to_string(_, []) = "".
mercury_comma_type_list_to_string(VarSet, [Type | Types]) = String :-
    String0 = mercury_type_to_string(VarSet, print_name_only, Type),
    String1 = mercury_comma_type_list_to_string(VarSet, Types),
    string.append_list([", ", String0, String1], String).

mercury_output_type(VarSet, VarNamePrint, Type, !IO) :-
    mercury_format_type(VarSet, VarNamePrint, Type, !IO).

mercury_type_to_string(VarSet, VarNamePrint, Type) = String :-
    mercury_format_type(VarSet, VarNamePrint, Type, "", String).

mercury_format_type(TypeVarSet, VarNamePrint, Type, !U) :-
    % We convert to a term and then use mercury_format_term. The reason
    % for this is that we have to be very careful about handling operators
    % and precedence properly, and it is better to have the code to manage
    % that in one place, rather than duplicated here.
    %
    unparse_type(Type, Term),
    VarSet = varset.coerce(TypeVarSet),
    mercury_format_term(VarSet, VarNamePrint, Term, !U).

%---------------------------------------------------------------------------%

mercury_output_det(Detism, !UI) :-
    mercury_format_det(Detism, !UI).

mercury_det_to_string(detism_det) = "det".
mercury_det_to_string(detism_semi) = "semidet".
mercury_det_to_string(detism_non) = "nondet".
mercury_det_to_string(detism_multi) = "multi".
mercury_det_to_string(detism_cc_multi) = "cc_multi".
mercury_det_to_string(detism_cc_non) = "cc_nondet".
mercury_det_to_string(detism_failure) = "failure".
mercury_det_to_string(detism_erroneous) = "erroneous".

mercury_format_det(Detism, !U) :-
    add_string(mercury_det_to_string(Detism), !U).

%---------------------------------------------------------------------------%

mercury_output_quantifier(TypeVarSet, VarNamePrint, ExistQVars, !IO) :-
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars, !IO).

mercury_quantifier_to_string(TypeVarSet, VarNamePrint, ExistQVars) = String :-
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars,
        "", String).

mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars, !U) :-
    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        add_string("some [", !U),
        mercury_format_vars(TypeVarSet, VarNamePrint, ExistQVars, !U),
        add_string("] ", !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_state_vars(VarSet, VarNamePrint, StateVars, !IO) :-
    io.write_list(StateVars, ", ",
        mercury_output_state_var(VarSet, VarNamePrint), !IO).

:- pred mercury_output_state_var(varset(T)::in, var_name_print::in, var(T)::in,
    io::di, io::uo) is det.

mercury_output_state_var(VarSet, VarNamePrint, Var, !IO) :-
    io.write_string("!", !IO),
    mercury_output_var(VarSet, VarNamePrint, Var, !IO).

%---------------------------------------------------------------------------%

mercury_output_cons_id(NeedsBrackets, ConsId, !IO) :-
    mercury_format_cons_id(NeedsBrackets, ConsId, !IO).

mercury_cons_id_to_string(NeedsBrackets, ConsId) = String :-
    mercury_format_cons_id(NeedsBrackets, ConsId, "", String).

mercury_format_cons_id(NeedsBrackets, ConsId, !U) :-
    (
        ConsId = cons(Name, _, _),
        (
            NeedsBrackets = needs_brackets,
            mercury_format_bracketed_sym_name(Name, !U)
        ;
            NeedsBrackets = does_not_need_brackets,
            mercury_format_sym_name(Name, !U)
        )
    ;
        ConsId = tuple_cons(_),
        add_string("{}", !U)
    ;
        ConsId = int_const(Int),
        add_int(Int, !U)
    ;
        ConsId = uint_const(UInt),
        add_uint(UInt, !U)
    ;
        ConsId = int8_const(Int8),
        add_int8(Int8, !U)
    ;
        ConsId = uint8_const(UInt8),
        add_uint8(UInt8, !U)
    ;
        ConsId = int16_const(Int16),
        add_int16(Int16, !U)
    ;
        ConsId = uint16_const(UInt16),
        add_uint16(UInt16, !U)
    ;
        ConsId = int32_const(Int32),
        add_int32(Int32, !U)
    ;
        ConsId = uint32_const(UInt32),
        add_uint32(UInt32, !U)
    ;
        ConsId = int64_const(Int64),
        add_int(Int64, !U) % XXX INT64
    ;
        ConsId = uint64_const(UInt64),
        add_int(UInt64, !U) % XXX INT64
    ;
        ConsId = float_const(Float),
        add_float(Float, !U)
    ;
        ConsId = char_const(Char),
        add_string(term_io.quoted_char(Char), !U)
    ;
        ConsId = string_const(Str),
        add_quoted_string(Str, !U)
    ;
        ConsId = impl_defined_const(Name),
        add_string("$", !U),
        add_string(Name, !U)
    ;
        ConsId = closure_cons(ShroudedPredProcId, _EvalMethod),
        % XXX Should probably print this out in name/arity form.
        ShroudedPredProcId = shrouded_pred_proc_id(PredInt, ProcInt),
        add_string("<closure_cons(", !U),
        add_int(PredInt, !U),
        add_string(", ", !U),
        add_int(ProcInt, !U),
        % add_string(", ", !U),
        % add_lambda_eval_method(EvalMethod, !U),
        add_string(")>", !U)
    ;
        ConsId = type_ctor_info_const(ModuleName, Type, Arity),
        ModuleString = sym_name_to_string(ModuleName),
        string.int_to_string(Arity, ArityString),
        add_strings(["<type_ctor_info for ",
            ModuleString, ".", Type, "/", ArityString, ">"], !U)
    ;
        ConsId = base_typeclass_info_const(ModuleName, ClassId, InstanceNum,
            InstanceString),
        ModuleString = sym_name_to_string(ModuleName),
        ClassId = class_id(ClassName, ClassArity),
        add_string("<base_typeclass_info for ", !U),
        add_string("class_id(", !U),
        mercury_format_sym_name(ClassName, !U),
        add_string(", ", !U),
        add_int(ClassArity, !U),
        add_string(")", !U),
        ( if ModuleString \= "some bogus module name" then
            add_strings([" from module ", ModuleString], !U)
        else
            true
        ),
        add_format(", instance number %d (%s)>",
            [i(InstanceNum), s(InstanceString)], !U)
    ;
        ConsId = type_info_cell_constructor(_),
        add_string("<type_info_cell_constructor>", !U)
    ;
        ConsId = typeclass_info_cell_constructor,
        add_string("<typeclass_info_cell_constructor>", !U)
    ;
        ConsId = type_info_const(TIConstNum),
        add_string("<type_info_cell_constructor " ++
            int_to_string(TIConstNum) ++ ">", !U)
    ;
        ConsId = typeclass_info_const(TCIConstNum),
        add_string("<typeclass_info_cell_constructor " ++
            int_to_string(TCIConstNum) ++ ">", !U)
    ;
        ConsId = ground_term_const(ConstNum, SubConsId),
        add_string("<ground_term_cell_constructor " ++
            int_to_string(ConstNum) ++ ", ", !U),
        mercury_format_cons_id(does_not_need_brackets, SubConsId, !U),
        add_string(">", !U)
    ;
        ConsId = tabling_info_const(_),
        add_string("<tabling info>", !U)
    ;
        ConsId = table_io_entry_desc(_),
        add_string("<table_io_entry_desc>", !U)
    ;
        ConsId = deep_profiling_proc_layout(_),
        add_string("<deep_profiling_proc_layout>", !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_sym_name(SymName, !IO) :-
    mercury_format_sym_name_ngt(not_next_to_graphic_token, SymName, !IO).

mercury_format_sym_name(SymName, !U) :-
    mercury_format_sym_name_ngt(not_next_to_graphic_token, SymName, !U).

mercury_format_sym_name_ngt(NextToGraphicToken, SymName, !U) :-
    (
        SymName = qualified(ModuleName, PredName),
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token,
            ModuleName, !U),
        add_string(".", !U),
        mercury_format_quoted_atom(next_to_graphic_token, PredName, !U)
    ;
        SymName = unqualified(PredName),
        mercury_format_quoted_atom(NextToGraphicToken, PredName, !U)
    ).

mercury_format_sym_name_and_arity(sym_name_arity(SymName, Arity), !U) :-
    mercury_format_sym_name(SymName, !U),
    add_char('/', !U),
    add_int(Arity, !U).

%---------------------%

mercury_output_bracketed_sym_name(SymName, !IO) :-
    mercury_output_bracketed_sym_name_ngt(not_next_to_graphic_token, SymName,
        !IO).

mercury_bracketed_sym_name_to_string(SymName) =
    mercury_bracketed_sym_name_to_string_ngt(not_next_to_graphic_token,
        SymName).

mercury_format_bracketed_sym_name(SymName, !U) :-
    mercury_format_bracketed_sym_name_ngt(not_next_to_graphic_token, SymName,
        !U).

%---------------------%

mercury_output_bracketed_sym_name_ngt(NextToGraphicToken, SymName, !IO) :-
    mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName, !IO).

mercury_bracketed_sym_name_to_string_ngt(NextToGraphicToken, SymName) = Str :-
    mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        "", Str).

mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName, !U) :-
    (
        SymName = qualified(ModuleName, Name),
        add_string("(", !U),
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token,
            ModuleName, !U),
        add_string(".", !U),
        mercury_format_bracketed_atom(next_to_graphic_token, Name, !U),
        add_string(")", !U)
    ;
        SymName = unqualified(Name),
        mercury_format_bracketed_atom(NextToGraphicToken, Name, !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_constraint(TypeVarSet, VarNamePrint, Constraint, !IO) :-
    mercury_format_constraint(TypeVarSet, VarNamePrint, Constraint, !IO).

mercury_constraint_to_string(TypeVarSet, Constraint) = String :-
    mercury_format_constraint(TypeVarSet, print_name_only, Constraint,
        "", String).

mercury_format_constraint(TypeVarSet, VarNamePrint, Constraint, !U) :-
    Constraint = constraint(Name, Types),
    mercury_format_sym_name(Name, !U),
    add_string("(", !U),
    add_list(Types, ", ", mercury_format_type(TypeVarSet, VarNamePrint), !U),
    add_string(")", !U).

mercury_format_prog_constraint_list(TypeVarSet, VarNamePrint, Operator,
        Constraints, !U) :-
    (
        Constraints = []
    ;
        Constraints = [_ | _],
        add_strings([" ", Operator, " ("], !U),
        add_list(Constraints, ", ",
            mercury_format_constraint(TypeVarSet, VarNamePrint), !U),
        add_string(")", !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_class_context(TypeVarSet, VarNamePrint,
        ClassContext, ExistQVars, !IO) :-
    mercury_format_class_context(TypeVarSet, VarNamePrint,
        ClassContext, ExistQVars, !IO).

mercury_format_class_context(TypeVarSet, VarNamePrint,
        ClassContext, ExistQVars, !U) :-
    ClassContext = constraints(UnivCs, ExistCs),
    mercury_format_prog_constraint_list(TypeVarSet, VarNamePrint, "=>",
        ExistCs, !U),
    ( if
        ExistQVars = [],
        ExistCs = []
    then
        true
    else
        % XXX What prints the matching open parenthesis?
        % And does it print the open in *exactly* the same set of situations
        % in which we print the close?
        add_string(")", !U)
    ),
    mercury_format_prog_constraint_list(TypeVarSet, VarNamePrint, "<=",
        UnivCs, !U).

%---------------------------------------------------------------------------%

mercury_output_foreign_language_string(Lang, !IO) :-
    mercury_format_foreign_language_string(Lang, !IO).

mercury_foreign_language_to_string(Lang) = String :-
    mercury_format_foreign_language_string(Lang, "", String).

mercury_format_foreign_language_string(Lang, !U) :-
    add_string("""" ++ foreign_language_string(Lang) ++ """", !U).

%---------------------------------------------------------------------------%

mercury_output_newline(Indent, !IO) :-
    io.write_char('\n', !IO),
    mercury_format_tabs(Indent, !IO).

mercury_format_tabs(Indent, !U) :-
    ( if Indent > 0 then
        add_string("\t", !U),
        mercury_format_tabs(Indent - 1, !U)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.mercury_to_mercury.
%---------------------------------------------------------------------------%
