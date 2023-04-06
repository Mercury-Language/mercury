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
% This module converts types back into Mercury source text.
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

:- pred mercury_output_type(tvarset::in, var_name_print::in, mer_type::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_type_to_string(tvarset, var_name_print, mer_type) = string.
:- pred mercury_format_type(tvarset::in, var_name_print::in, mer_type::in,
    S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_output_constraint(tvarset::in, var_name_print::in,
    prog_constraint::in, io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_constraint_to_string(tvarset, var_name_print, prog_constraint)
    = string.
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
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.

:- import_module string.
:- import_module term.
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
    unparse_type(Type, Term),
    VarSet = varset.coerce(TypeVarSet),
    mercury_format_term_vs(VarSet, VarNamePrint, Term, S, !U).

%---------------------------------------------------------------------------%

mercury_output_constraint(TypeVarSet, VarNamePrint, Constraint, Stream, !IO) :-
    mercury_format_constraint(TypeVarSet, VarNamePrint,
        Constraint, Stream, !IO).

mercury_constraint_to_string(TypeVarSet, VarNamePrint, Constraint) = String :-
    mercury_format_constraint(TypeVarSet, VarNamePrint, Constraint,
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
:- end_module parse_tree.parse_tree_out_type.
%---------------------------------------------------------------------------%
