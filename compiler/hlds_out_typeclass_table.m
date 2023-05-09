%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_typeclass_table.
:- interface.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_out.hlds_out_util.

:- import_module io.

:- pred write_classes(hlds_out_info::in, io.text_output_stream::in,
    class_table::in, io::di, io::uo) is det.

:- pred write_instances(hlds_out_info::in, io.text_output_stream::in,
    instance_table::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.indent.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_data.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Write out the typeclass table.
%

write_classes(Info, Stream, ClassTable, !IO) :-
    io.write_string(Stream, "%-------- Classes --------\n", !IO),
    map.to_assoc_list(ClassTable, ClassTableList),
    list.foldl(write_class_defn(Info, Stream), ClassTableList, !IO),
    io.nl(Stream, !IO).

:- pred write_class_defn(hlds_out_info::in, io.text_output_stream::in,
    pair(class_id, hlds_class_defn)::in, io::di, io::uo) is det.

write_class_defn(Info, Stream, ClassId - ClassDefn, !IO) :-
    ClassDefn = hlds_class_defn(_, TVarSet, _, Vars, Constraints, FunDeps,
        _, _, MethodInfos, Context, _),

    io.format(Stream, "\n%% %s:\n", [s(class_id_to_string(ClassId))], !IO),
    maybe_output_context_comment(Stream, 0, "", Context, !IO),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'v') then
        VarNamePrint = print_name_and_num
    else
        VarNamePrint = print_name_only
    ),

    io.format(Stream, "%% Vars: %s\n",
        [s(mercury_vars_to_string_vs(TVarSet, VarNamePrint, Vars))], !IO),

    IndentStr = "",
    io.write_string(Stream, "% Functional dependencies:\n", !IO),
    list.foldl(hlds_output_fundep(Stream, IndentStr), FunDeps, !IO),

    io.write_string(Stream, "% Constraints:\n", !IO),
    list.foldl(
        hlds_output_constraint(Stream, IndentStr, TVarSet, VarNamePrint),
        Constraints, !IO),

    io.write_string(Stream, "% Class Methods:\n", !IO),
    list.foldl(write_method_info(Stream, IndentStr), MethodInfos, !IO).

:- pred hlds_output_fundep(io.text_output_stream::in, string::in,
    hlds_class_fundep::in, io::di, io::uo) is det.

hlds_output_fundep(Stream, IndentStr, fundep(Domain, Range), !IO) :-
    DomainList = set.to_sorted_list(Domain),
    RangeList = set.to_sorted_list(Range),
    DomainStrs = list.map(string.int_to_string, DomainList),
    RangeStrs = list.map(string.int_to_string, RangeList),
    DomainStr = string.join_list(", ", DomainStrs),
    RangeStr = string.join_list(", ", RangeStrs),
    io.format(Stream, "%s%%   (%s -> %s)\n",
        [s(IndentStr), s(DomainStr), s(RangeStr)], !IO).

:- pred hlds_output_constraint(io.text_output_stream::in, string::in,
    tvarset::in, var_name_print::in, prog_constraint::in,
    io::di, io::uo) is det.

hlds_output_constraint(Stream, IndentStr, TVarSet, VarNamePrint,
        Constraint, !IO) :-
    ConstraintStr = mercury_constraint_to_string(TVarSet, VarNamePrint,
        Constraint),
    io.format(Stream, "%s%%   %s\n", [s(IndentStr), s(ConstraintStr)], !IO).

:- pred write_method_info(io.text_output_stream::in, string::in,
    method_info::in, io::di, io::uo) is det.

write_method_info(Stream, IndentStr, MethodInfo, !IO) :-
    MethodInfo = method_info(method_proc_num(MethodProcNum), PredPfNameArity,
        OrigPredProcId, _CurPredProcId),
    NameStr = pf_sym_name_user_arity_to_unquoted_string(PredPfNameArity),
    OrigPredProcId = proc(OrigPredId, OrigProcId),
    pred_id_to_int(OrigPredId, OrigPredInt),
    proc_id_to_int(OrigProcId, OrigProcInt),
    io.format(Stream, "%s%%   method %2d: %s, proc(%d, %d)\n",
        [s(IndentStr), i(MethodProcNum), s(NameStr),
        i(OrigPredInt), i(OrigProcInt)], !IO).

%---------------------------------------------------------------------------%
%
% Write out the instance table.
%

write_instances(Info, Stream, InstanceTable, !IO) :-
    io.write_string(Stream, "%-------- Instances --------\n", !IO),
    map.to_assoc_list(InstanceTable, InstanceTableList),
    list.foldl(write_instance_defns(Info, Stream), InstanceTableList, !IO),
    io.nl(Stream, !IO).

:- pred write_instance_defns(hlds_out_info::in, io.text_output_stream::in,
    pair(class_id, list(hlds_instance_defn))::in, io::di, io::uo) is det.

write_instance_defns(Info, Stream, ClassId - InstanceDefns, !IO) :-
    io.format(Stream, "\n%% Instances for class %s:\n",
        [s(class_id_to_string(ClassId))], !IO),
    list.foldl(write_instance_defn(Info, Stream), InstanceDefns, !IO).

:- pred write_instance_defn(hlds_out_info::in, io.text_output_stream::in,
    hlds_instance_defn::in, io::di, io::uo) is det.

write_instance_defn(Info, Stream, InstanceDefn, !IO) :-
    InstanceDefn = hlds_instance_defn(_InstanceModule, InstanceStatus,
        TVarSet, OriginalTypes, Types, Constraints, MaybeSubsumedContext,
        ProofMap, Body, MaybeMethodInfos, Context),

    % Separate this instance from any previous ones, or the class id.
    io.nl(Stream, !IO),
    maybe_output_context_comment(Stream, 1, "", Context, !IO),

    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'v') then
        VarNamePrint = print_name_and_num
    else
        VarNamePrint = print_name_only
    ),

    IndentStr = indent2_string(1),
    TypeStrs = list.map(mercury_type_to_string(TVarSet, VarNamePrint),
        Types),
    OriginalTypeStrs = list.map(mercury_type_to_string(TVarSet, VarNamePrint),
        OriginalTypes),
    TypesStr = string.join_list(", ", TypeStrs),
    OriginalTypesStr = string.join_list(", ", OriginalTypeStrs),
    io.format(Stream, "%s%% Types: %s\n",
        [s(IndentStr), s(TypesStr)], !IO),
    io.format(Stream, "%s%% Original types: %s\n",
        [s(IndentStr), s(OriginalTypesStr)], !IO),

    InstanceStatusStr = instance_import_status_to_string(InstanceStatus),
    io.format(Stream, "%s%% Status: %s\n",
        [s(IndentStr), s(InstanceStatusStr)], !IO),

    (
        MaybeSubsumedContext = no
    ;
        MaybeSubsumedContext = yes(SubsumedContext),
        io.format(Stream, "%s%% Subsumed context: %s\n",
            [s(IndentStr), s(context_to_brief_string(SubsumedContext))], !IO)
    ),

    ConstraintStrs =
        list.map(mercury_constraint_to_string(TVarSet, VarNamePrint),
        Constraints),
    ConstraintsStr = string.join_list(", ", ConstraintStrs),
    io.format(Stream, "%s%% Constraints: %s\n",
        [s(IndentStr), s(ConstraintsStr)], !IO),

    (
        Body = instance_body_abstract,
        io.format(Stream, "%s%% abstract\n", [s(IndentStr)], !IO)
    ;
        Body = instance_body_concrete(Methods),
        io.format(Stream, "%s%% Instance methods:\n", [s(IndentStr)], !IO),
        write_instance_methods(Stream, IndentStr, Methods, 1, !IO)
    ),

    (
        MaybeMethodInfos = yes(MethodInfos),
        io.format(Stream, "%s%% Procedures:\n", [s(IndentStr)], !IO),
        list.foldl(write_method_info(Stream, IndentStr), MethodInfos, !IO)
    ;
        MaybeMethodInfos = no
    ),
    write_constraint_proof_map(Stream, 1, VarNamePrint, TVarSet,
        ProofMap, !IO).

:- pred write_instance_methods(io.text_output_stream::in, string::in,
    list(instance_method)::in, int::in, io::di, io::uo) is det.

write_instance_methods(_, _, [], _, !IO).
write_instance_methods(Stream, IndentStr, [Method | Methods],
        CurMethodNum, !IO) :-
    Method = instance_method(MethodName, _Defn, _Context),
    MethodName = pred_pf_name_arity(PredOrFunc, MethodSymName, UserArity),
    UserArity = user_arity(UserArityInt),
    io.format(Stream, "%s%%   method %d, %s %s/%d\n",
        [s(IndentStr), i(CurMethodNum), s(pred_or_func_to_str(PredOrFunc)),
        s(sym_name_to_string(MethodSymName)), i(UserArityInt)], !IO),
    io.format(Stream, "%s%%   ", [s(IndentStr)], !IO),
    % XXX This sort-of does the right thing when Method is bound to
    % instance_proc_def_name (only sort-of because we shouldn't print tabs),
    % but it does the wrong things when Method is instance_proc_def_clauses.
    % Specifically, only the first line in the printout of the clause
    % will start with a percent sign (the one printed just above); all
    % the others will lack this sign. I (zs) see no simple way of fixing this.
    mercury_output_instance_method(Method, Stream, !IO),
    io.nl(Stream, !IO),
    write_instance_methods(Stream, IndentStr, Methods, CurMethodNum + 1, !IO).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_typeclass_table.
%---------------------------------------------------------------------------%
