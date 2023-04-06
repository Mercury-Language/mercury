%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2015, 2018, 2020-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module has code that helps debug the type checker.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_debug.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred type_checkpoint(string::in, typecheck_info::in, prog_varset::in,
    type_assign_set::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module libs.
:- import_module libs.file_util.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.vartypes.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

type_checkpoint(Msg, Info, VarSet, TypeAssignSet, !IO) :-
    typecheck_info_get_debug_info(Info, Debug),
    (
        Debug = typecheck_debug(DetailedStats),
        io.output_stream(Stream, !IO),
        do_type_checkpoint(Stream, Msg, DetailedStats, VarSet,
            TypeAssignSet, !IO)
    ;
        Debug = no_typecheck_debug
    ).

:- pred do_type_checkpoint(io.text_output_stream::in, string::in,
    bool::in, prog_varset::in, type_assign_set::in, io::di, io::uo) is det.

do_type_checkpoint(Stream, Msg, Statistics, VarSet, TypeAssignSet, !IO) :-
    io.format(Stream, "At %s:", [s(Msg)], !IO),
    maybe_report_stats(Stream, Statistics, !IO),
    io.nl(Stream, !IO),
    ( if
        Statistics = yes,
        TypeAssignSet = [TypeAssign | _]
    then
        type_assign_get_var_types(TypeAssign, VarTypes),
        vartypes_count(VarTypes, VarTypesCount),
        io.format(Stream, "\t`var -> type' map: count = %d\n",
            [i(VarTypesCount)], !IO),
        type_assign_get_type_bindings(TypeAssign, TypeBindings),
        map.count(TypeBindings, TypeBindingsCount),
        io.format(Stream, "\t`type var -> type' map: count = %d\n",
            [i(TypeBindingsCount)], !IO)
    else
        true
    ),
    write_type_assign_set(Stream, TypeAssignSet, VarSet, !IO).

:- pred write_type_assign_set(io.text_output_stream::in,
    type_assign_set::in, prog_varset::in, io::di, io::uo) is det.

write_type_assign_set(_, [], _, !IO).
write_type_assign_set(Stream, [TypeAssign | TypeAssigns], VarSet, !IO) :-
    io.write_string(Stream, "\t", !IO),
    write_type_assign(Stream, TypeAssign, VarSet, !IO),
    io.write_string(Stream, "\n", !IO),
    write_type_assign_set(Stream, TypeAssigns, VarSet, !IO).

:- pred write_type_assign(io.text_output_stream::in,
    type_assign::in, prog_varset::in, io::di, io::uo) is det.

write_type_assign(Stream, TypeAssign, VarSet, !IO) :-
    type_assign_get_existq_tvars(TypeAssign, ExistQTVars),
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TypeVarSet),
    vartypes_vars(VarTypes, Vars),
    (
        ExistQTVars = []
    ;
        ExistQTVars = [_ | _],
        io.write_string(Stream, "some [", !IO),
        mercury_output_vars_vs(TypeVarSet, debug_varnums, ExistQTVars,
            Stream, !IO),
        io.write_string(Stream, "]\n\t", !IO)
    ),
    write_type_assign_types(Stream, VarSet, TypeVarSet, VarTypes, TypeBindings,
        no, Vars, !IO),
    write_type_assign_hlds_constraints(Stream, TypeVarSet, TypeBindings,
        Constraints, !IO),
    io.write_string(Stream, "\n", !IO).

:- pred write_type_assign_types(io.text_output_stream::in,
    prog_varset::in, tvarset::in, vartypes::in, tsubst::in, bool::in,
    list(prog_var)::in, io::di, io::uo) is det.

write_type_assign_types(Stream, _, _, _, _, FoundOne, [], !IO) :-
    (
        FoundOne = no,
        io.write_string(Stream, "(No variables were assigned a type)", !IO)
    ;
        FoundOne = yes
    ).
write_type_assign_types(Stream, VarSet, TypeVarSet, VarTypes, TypeBindings,
        FoundOne, [Var | Vars], !IO) :-
    ( if search_var_type(VarTypes, Var, Type) then
        (
            FoundOne = yes,
            io.write_string(Stream, "\n\t", !IO)
        ;
            FoundOne = no
        ),
        mercury_output_var_vs(VarSet, debug_varnums, Var, Stream, !IO),
        io.write_string(Stream, ": ", !IO),
        write_type_with_bindings(Stream, TypeVarSet, TypeBindings, Type, !IO),
        write_type_assign_types(Stream, VarSet, TypeVarSet, VarTypes,
            TypeBindings, yes, Vars, !IO)
    else
        write_type_assign_types(Stream, VarSet, TypeVarSet, VarTypes,
            TypeBindings, FoundOne, Vars, !IO)
    ).

    % write_type_with_bindings writes out a type after applying the
    % type bindings.
    %
:- pred write_type_with_bindings(io.text_output_stream::in, tvarset::in,
    tsubst::in, mer_type::in, io::di, io::uo) is det.

write_type_with_bindings(Stream, TypeVarSet, TypeBindings, Type0, !IO) :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_module_names_from_type(strip_builtin_module_name, Type1, Type),
    mercury_output_type(TypeVarSet, print_name_and_num, Type, Stream, !IO).

:- pred write_type_assign_hlds_constraints(io.text_output_stream::in,
    tvarset::in, tsubst::in, hlds_constraints::in, io::di, io::uo) is det.

write_type_assign_hlds_constraints(Stream, TypeVarSet, TypeBindings,
        Constraints, !IO) :-
    Constraints =
        hlds_constraints(ConstraintsToProve, AssumedConstraints, _, _),
    write_type_assign_constraints(Stream, TypeVarSet, TypeBindings,
        AssumedConstraints, "&", !IO),
    write_type_assign_constraints(Stream, TypeVarSet, TypeBindings,
        ConstraintsToProve, "<=", !IO).

:- pred write_type_assign_constraints(io.text_output_stream::in, tvarset::in,
    tsubst::in, list(hlds_constraint)::in, string::in, io::di, io::uo) is det.

write_type_assign_constraints(_, _, _, [], _, !IO).
write_type_assign_constraints(Stream, TypeVarSet, TypeBindings,
        [Constraint | Constraints], MaybeOperator, !IO) :-
    % Write & or <= only before the first constraint; put only a comma
    % before the later constraints.
    ( if MaybeOperator = "" then
        io.write_string(Stream, ",\n\t   ", !IO)
    else
        io.format(Stream, "\n\t%s ", [s(MaybeOperator)], !IO)
    ),
    apply_rec_subst_to_constraint(TypeBindings, Constraint, BoundConstraint),
    retrieve_prog_constraint(BoundConstraint, ProgConstraint),
    mercury_output_constraint(TypeVarSet, debug_varnums, ProgConstraint,
        Stream, !IO),
    write_type_assign_constraints(Stream, TypeVarSet, TypeBindings,
        Constraints, "", !IO).

%---------------------------------------------------------------------------%

:- func debug_varnums = var_name_print.

debug_varnums = print_name_and_num.

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_debug.
%---------------------------------------------------------------------------%
