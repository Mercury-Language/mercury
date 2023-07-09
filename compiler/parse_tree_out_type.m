%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_tree_out_type.m.
% Main author: fjh.
%
% With one exception (which is marked as such), the predicates and functions
% in this  module convert types back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_type.
:- interface.

:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module varset.

%---------------------------------------------------------------------------%

:- func mercury_type_list_to_string(tvarset, list(mer_type)) = string.

:- func mercury_type_to_string(tvarset, var_name_print, mer_type) = string.
:- pred mercury_output_type(tvarset::in, var_name_print::in, mer_type::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_type(tvarset::in, var_name_print::in, mer_type::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- func mercury_constraint_to_string(tvarset, var_name_print, prog_constraint)
    = string.
:- pred mercury_output_constraint(tvarset::in, var_name_print::in,
    prog_constraint::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_constraint(tvarset::in, var_name_print::in,
    prog_constraint::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_prog_constraint_list_to_string(tvarset, var_name_print,
    string, list(prog_constraint)) = string.
:- pred mercury_format_prog_constraint_list(tvarset::in, var_name_print::in,
    string::in, list(prog_constraint)::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

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
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % Convert a type to a string. The result is NOT guaranteed to be
    % valid Mercury; the intended use case is helping to construct
    % log messages to help debug the compiler itself.
    %
:- pred type_to_debug_string(tvarset::in, mer_type::in, string::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.

:- import_module string.
:- import_module string.builder.
:- import_module term.

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

%---------------------------------------------------------------------------%

mercury_type_to_string(VarSet, VarNamePrint, Type) = Str :-
    State0 = string.builder.init,
    mercury_format_type(VarSet, VarNamePrint, Type, string.builder.handle,
        State0, State),
    Str = string.builder.to_string(State).

mercury_output_type(VarSet, VarNamePrint, Type, Stream, !IO) :-
    mercury_format_type(VarSet, VarNamePrint, Type, Stream, !IO).

mercury_format_type(TypeVarSet, VarNamePrint, Type, S, !U) :-
    % We convert to a term and then use mercury_format_term. The reason
    % for this is that we have to be very careful about handling operators
    % and precedence properly, and it is better to have the code to manage
    % that in one place, rather than duplicated here.
    unparse_type(Type, Term),
    VarSet = varset.coerce(TypeVarSet),
    mercury_format_term_vs(VarSet, VarNamePrint, Term, S, !U).

%---------------------------------------------------------------------------%

mercury_constraint_to_string(TypeVarSet, VarNamePrint, Constraint) = Str :-
    State0 = string.builder.init,
    mercury_format_constraint(TypeVarSet, VarNamePrint, Constraint,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_constraint(TypeVarSet, VarNamePrint, Constraint, Stream, !IO) :-
    mercury_format_constraint(TypeVarSet, VarNamePrint,
        Constraint, Stream, !IO).

mercury_format_constraint(TypeVarSet, VarNamePrint, Constraint, S, !U) :-
    Constraint = constraint(Name, Types),
    mercury_format_sym_name(Name, S, !U),
    add_string("(", S, !U),
    add_list(mercury_format_type(TypeVarSet, VarNamePrint), ", ", Types,
        S, !U),
    add_string(")", S, !U).

mercury_prog_constraint_list_to_string(TypeVarSet, VarNamePrint,
        Operator, Constraints) = Str :-
    State0 = string.builder.init,
    mercury_format_prog_constraint_list(TypeVarSet, VarNamePrint,
        Operator, Constraints, string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

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

type_to_debug_string(TVarSet, Type, Name) :-
    (
        Type = type_variable(TVar,_),
        Name = mercury_var_to_string_vs(TVarSet, print_name_and_num, TVar)
    ;
        Type = defined_type(SymName, Subtypes, _),
        list.map(type_to_debug_string(TVarSet), Subtypes, SubtypeNames),
        SubtypeName = string.join_list(", ", SubtypeNames),
        Name = sym_name_to_string(SymName) ++ "(" ++ SubtypeName ++ ")"
    ;
        Type = builtin_type(builtin_type_int(IntType)),
        int_type_module_name(IntType, Name)
    ;
        Type = builtin_type(builtin_type_float),
        Name = "float"
    ;
        Type = builtin_type(builtin_type_string),
        Name = "string"
    ;
        Type = builtin_type(builtin_type_char),
        Name = "character"
    ;
        Type = tuple_type(Subtypes, _),
        list.map(type_to_debug_string(TVarSet), Subtypes, SubtypeNames),
        Name = "{" ++  string.join_list(", ", SubtypeNames) ++ "}"
    ;
        Type = higher_order_type(PorF, Types, _, _, _),
        list.map(type_to_debug_string(TVarSet), Types, TypeNames),
        (
            PorF = pf_predicate,
            Name = "pred(" ++  string.join_list(", ", TypeNames) ++ ")"
        ;
            PorF = pf_function,
            list.det_split_last(TypeNames, ArgTypeNames, ReturnTypeName),
            Name = "func(" ++  string.join_list(", ", ArgTypeNames) ++ ") = "
                ++ ReturnTypeName
        )
    ;
        Type = apply_n_type(_, Subtypes, _),
        list.map(type_to_debug_string(TVarSet), Subtypes, SubtypeNames),
        Name = "func(" ++  string.join_list(", ", SubtypeNames) ++ ")"
    ;
        Type = kinded_type(Type0, _),
        type_to_debug_string(TVarSet, Type0, Name)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_type.
%---------------------------------------------------------------------------%
