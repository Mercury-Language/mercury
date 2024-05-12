%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_typeclass_table.
:- interface.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_out.hlds_out_util.

:- import_module io.
:- import_module string.
:- import_module string.builder.

:- pred write_classes(hlds_out_info::in, io.text_output_stream::in,
    class_table::in, io::di, io::uo) is det.
:- pred format_classes(hlds_out_info::in, class_table::in,
    string.builder.state::di, string.builder.state::uo) is det.

:- pred write_instances(hlds_out_info::in, io.text_output_stream::in,
    instance_table::in, io::di, io::uo) is det.
:- pred format_instances(hlds_out_info::in, instance_table::in,
    string.builder.state::di, string.builder.state::uo) is det.

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
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_item.
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
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
%
% Write out the typeclass table.
%

write_classes(Info, Stream, ClassTable, !IO) :-
    State0 = string.builder.init,
    format_classes(Info, ClassTable, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_classes(Info, ClassTable, !State) :-
    string.builder.append_string("%-------- Classes --------\n", !State),
    map.to_assoc_list(ClassTable, ClassTableList),
    list.foldl(format_class_defn(Info), ClassTableList, !State),
    string.builder.append_string("\n", !State).

:- pred format_class_defn(hlds_out_info::in,
    pair(class_id, hlds_class_defn)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_class_defn(Info, ClassId - ClassDefn, !State) :-
    ClassDefn = hlds_class_defn(_, TVarSet, _, Vars, SuperClassConstraints,
        FunDeps, _, _, MethodInfos, Context, _),

    string.builder.format("\n%% %s:\n",
        [s(class_id_to_string(ClassId))], !State),
    maybe_format_context_comment(0u, "", Context, !State),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'v') then
        VarNamePrint = print_name_and_num
    else
        VarNamePrint = print_name_only
    ),

    string.builder.format("%% Vars: %s\n",
        [s(mercury_vars_to_string_vs(TVarSet, VarNamePrint, Vars))], !State),

    IndentStr = "",
    string.builder.append_string("% Functional dependencies:\n", !State),
    list.foldl(format_fundep(IndentStr), FunDeps, !State),

    string.builder.append_string("% Superclass constraints:\n", !State),
    list.foldl(
        format_constraint(IndentStr, TVarSet, VarNamePrint),
        SuperClassConstraints, !State),

    string.builder.append_string("% Class methods:\n", !State),
    list.foldl(format_method_info(IndentStr), MethodInfos, !State).

:- pred format_fundep(string::in, hlds_class_fundep::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_fundep(IndentStr, fundep(Domain, Range), !State) :-
    DomainList = set.to_sorted_list(Domain),
    RangeList = set.to_sorted_list(Range),
    DomainStrs = list.map(string.int_to_string, DomainList),
    RangeStrs = list.map(string.int_to_string, RangeList),
    DomainStr = string.join_list(", ", DomainStrs),
    RangeStr = string.join_list(", ", RangeStrs),
    string.builder.format("%s%%   (%s -> %s)\n",
        [s(IndentStr), s(DomainStr), s(RangeStr)], !State).

:- pred format_constraint(string::in, tvarset::in, var_name_print::in,
    prog_constraint::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_constraint(IndentStr, TVarSet, VarNamePrint, Constraint, !State) :-
    ConstraintStr = mercury_constraint_to_string(TVarSet, VarNamePrint,
        Constraint),
    string.builder.format("%s%%   %s\n",
        [s(IndentStr), s(ConstraintStr)], !State).

:- pred format_method_info(string::in, method_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_method_info(IndentStr, MethodInfo, !State) :-
    MethodInfo = method_info(method_proc_num(MethodProcNum), PredPfNameArity,
        OrigPredProcId, _CurPredProcId),
    NameStr = pf_sym_name_user_arity_to_unquoted_string(PredPfNameArity),
    OrigPredProcId = proc(OrigPredId, OrigProcId),
    pred_id_to_int(OrigPredId, OrigPredInt),
    proc_id_to_int(OrigProcId, OrigProcInt),
    string.builder.format("%s%%   method %2d: %s, proc(%d, %d)\n",
        [s(IndentStr), i(MethodProcNum), s(NameStr),
        i(OrigPredInt), i(OrigProcInt)], !State).

%---------------------------------------------------------------------------%
%
% Write out the instance table.
%

write_instances(Info, Stream, ClassTable, !IO) :-
    State0 = string.builder.init,
    format_instances(Info, ClassTable, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_instances(Info, InstanceTable, !State) :-
    string.builder.append_string("%-------- Instances --------\n", !State),
    map.to_assoc_list(InstanceTable, InstanceTableEntries),
    list.foldl(format_instance_defns_for_class(Info),
        InstanceTableEntries, !State),
    string.builder.append_string("\n", !State).

:- pred format_instance_defns_for_class(hlds_out_info::in,
    pair(class_id, list(hlds_instance_defn))::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_instance_defns_for_class(Info, ClassId - InstanceDefns, !State) :-
    string.builder.format("\n%% Instances for class %s:\n",
        [s(class_id_to_string(ClassId))], !State),
    list.foldl(format_instance_defn(Info), InstanceDefns, !State).

:- pred format_instance_defn(hlds_out_info::in, hlds_instance_defn::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_instance_defn(Info, InstanceDefn, !State) :-
    InstanceDefn = hlds_instance_defn(_InstanceModule, InstanceStatus,
        TVarSet, OriginalTypes, Types, Constraints, MaybeSubsumedContext,
        ProofMap, Body, MaybeMethodInfos, Context),

    % Separate this instance from any previous ones, or the class id.
    string.builder.append_string("\n", !State),
    maybe_format_context_comment(1u, "", Context, !State),

    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'v') then
        VarNamePrint = print_name_and_num
    else
        VarNamePrint = print_name_only
    ),

    IndentStr = indent2_string(1u),
    TypeStrs = list.map(mercury_type_to_string(TVarSet, VarNamePrint),
        Types),
    OriginalTypeStrs = list.map(mercury_type_to_string(TVarSet, VarNamePrint),
        OriginalTypes),
    TypesStr = string.join_list(", ", TypeStrs),
    OriginalTypesStr = string.join_list(", ", OriginalTypeStrs),
    string.builder.format("%s%% Types: %s\n",
        [s(IndentStr), s(TypesStr)], !State),
    string.builder.format("%s%% Original types: %s\n",
        [s(IndentStr), s(OriginalTypesStr)], !State),

    InstanceStatusStr = instance_import_status_to_string(InstanceStatus),
    string.builder.format("%s%% Status: %s\n",
        [s(IndentStr), s(InstanceStatusStr)], !State),

    (
        MaybeSubsumedContext = no
    ;
        MaybeSubsumedContext = yes(SubsumedContext),
        string.builder.format("%s%% Subsumed context: %s\n",
            [s(IndentStr), s(context_to_brief_string(SubsumedContext))], !State)
    ),

    ConstraintStrs =
        list.map(mercury_constraint_to_string(TVarSet, VarNamePrint),
        Constraints),
    ConstraintsStr = string.join_list(", ", ConstraintStrs),
    string.builder.format("%s%% Constraints: %s\n",
        [s(IndentStr), s(ConstraintsStr)], !State),

    (
        Body = instance_body_abstract,
        string.builder.format("%s%% abstract\n", [s(IndentStr)], !State)
    ;
        Body = instance_body_concrete(Methods),
        string.builder.format("%s%% Instance methods:\n",
            [s(IndentStr)], !State),
        format_instance_methods(IndentStr, Methods, 1, !State)
    ),

    (
        MaybeMethodInfos = yes(MethodInfos),
        string.builder.format("%s%% Procedures:\n", [s(IndentStr)], !State),
        list.foldl(format_method_info(IndentStr), MethodInfos, !State)
    ;
        MaybeMethodInfos = no
    ),
    format_constraint_proof_map(1u, VarNamePrint, TVarSet, ProofMap, !State).

:- pred format_instance_methods(string::in, list(instance_method)::in, int::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_instance_methods(_, [], _, !State).
format_instance_methods(IndentStr, [Method | Methods], CurMethodNum, !State) :-
    Method = instance_method(MethodName, _Defn, _Context),
    MethodName = pred_pf_name_arity(PredOrFunc, MethodSymName, UserArity),
    UserArity = user_arity(UserArityInt),
    string.builder.format("%s%%   method %d, %s %s/%d\n",
        [s(IndentStr), i(CurMethodNum), s(pred_or_func_to_str(PredOrFunc)),
        s(sym_name_to_string(MethodSymName)), i(UserArityInt)], !State),
    string.builder.format("%s%%   ", [s(IndentStr)], !State),
    % XXX This sort-of does the right thing when Method is bound to
    % instance_proc_def_name (only sort-of because we shouldn't print tabs),
    % but it does the wrong things when Method is instance_proc_def_clauses.
    % Specifically, only the first line in the printout of the clause
    % will start with a percent sign (the one printed just above); all
    % the others will lack this sign. I (zs) see no simple way of fixing this.
    mercury_format_instance_method(Method, string.builder.handle, !State),
    string.builder.append_string("\n", !State),
    format_instance_methods(IndentStr, Methods, CurMethodNum + 1, !State).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_typeclass_table.
%---------------------------------------------------------------------------%
