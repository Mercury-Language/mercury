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
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.

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
    io.write_string(Stream, "% ", !IO),
    write_class_id(Stream, ClassId, !IO),
    io.write_string(Stream, ":\n", !IO),

    ClassDefn = hlds_class_defn(_, Constraints, FunDeps, _, Vars, _, _,
        Interface, VarSet, Context, _),

    maybe_output_context_comment(Stream, 0, "", Context, !IO),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'v') then
        VarNamePrint = print_name_and_num
    else
        VarNamePrint = print_name_only
    ),

    io.write_string(Stream, "% Vars: ", !IO),
    mercury_output_vars(VarSet, VarNamePrint, Vars, Stream, !IO),
    io.nl(Stream, !IO),

    io.write_string(Stream, "% Functional dependencies: ", !IO),
    write_out_list(hlds_output_fundep, ", ", FunDeps, Stream, !IO),
    io.nl(Stream, !IO),

    io.write_string(Stream, "% Constraints: ", !IO),
    write_out_list(mercury_output_constraint(VarSet, VarNamePrint),
        ", ", Constraints, Stream, !IO),
    io.nl(Stream, !IO),

    io.write_string(Stream, "% Class Methods: ", !IO),
    write_out_list(write_class_proc, ", ", Interface, Stream, !IO),
    io.nl(Stream, !IO),
    io.nl(Stream, !IO).

:- pred hlds_output_fundep(hlds_class_fundep::in,
    io.text_output_stream::in, io::di, io::uo) is det.

hlds_output_fundep(fundep(Domain, Range), Stream, !IO) :-
    DomainList = set.to_sorted_list(Domain),
    RangeList = set.to_sorted_list(Range),
    DomainStrs = list.map(string.int_to_string, DomainList),
    RangeStrs = list.map(string.int_to_string, RangeList),
    DomainStr = string.join_list(", ", DomainStrs),
    RangeStr = string.join_list(", ", RangeStrs),
    io.format(Stream, "(%s -> %s)", [s(DomainStr), s(RangeStr)], !IO).

    % Just output the class methods as pred_ids and proc_ids because it is
    % probably not that useful to have the names. If that information is
    % needed, it shouldn't be a very difficult fix.
    %
:- pred write_class_proc(pred_proc_id::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_class_proc(proc(PredId, ProcId), Stream, !IO) :-
    pred_id_to_int(PredId, PredInt),
    proc_id_to_int(ProcId, ProcInt),
    io.format(Stream, "proc(pred_id:%d, proc_id:%d)",
        [i(PredInt), i(ProcInt)], !IO).

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
    io.nl(Stream, !IO),
    io.write_string(Stream, "% Instances for class ", !IO),
    write_class_id(Stream, ClassId, !IO),
    io.write_string(Stream, ":\n", !IO),
    list.foldl(write_instance_defn(Info, Stream), InstanceDefns, !IO).

:- pred write_instance_defn(hlds_out_info::in, io.text_output_stream::in,
    hlds_instance_defn::in, io::di, io::uo) is det.

write_instance_defn(Info, Stream, InstanceDefn, !IO) :-
    InstanceDefn = hlds_instance_defn(_InstanceModule, Types, OriginalTypes,
        InstanceStatus, Context, Constraints, Body, MaybePredProcIds,
        VarSet, ProofMap),

    % Separate this instance from any previous ones, or the class id.
    io.nl(Stream, !IO),
    maybe_output_context_comment(Stream, 1, "", Context, !IO),

    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'v') then
        VarNamePrint = print_name_and_num
    else
        VarNamePrint = print_name_only
    ),

    % Curry the varset for term_io.write_variable/4.
    PrintTerm = mercury_output_type(VarSet, VarNamePrint),
    write_indent(Stream, 1, !IO),
    io.write_string(Stream, "% Types: ", !IO),
    write_out_list(mercury_output_type(VarSet, VarNamePrint), ", ", Types,
        Stream, !IO),
    io.nl(Stream, !IO),
    write_indent(Stream, 1, !IO),
    io.write_string(Stream, "% Original types: ", !IO),
    write_out_list(PrintTerm, ", ", OriginalTypes, Stream, !IO),
    io.nl(Stream, !IO),

    write_indent(Stream, 1, !IO),
    io.write_string(Stream, "% Status: ", !IO),
    io.write_string(Stream,
        instance_import_status_to_string(InstanceStatus), !IO),
    io.nl(Stream, !IO),

    write_indent(Stream, 1, !IO),
    io.write_string(Stream, "% Constraints: ", !IO),
    write_out_list(mercury_output_constraint(VarSet, VarNamePrint),
        ", ", Constraints, Stream, !IO),
    io.nl(Stream, !IO),

    write_indent(Stream, 1, !IO),
    (
        Body = instance_body_abstract,
        io.write_string(Stream, "% abstract", !IO)
    ;
        Body = instance_body_concrete(Methods),
        io.write_string(Stream, "% Instance methods:\n", !IO),
        write_instance_methods(Stream, Methods, 1, !IO)
    ),
    io.nl(Stream, !IO),

    (
        MaybePredProcIds = yes(PredProcIds),
        write_indent(Stream, 1, !IO),
        io.write_string(Stream, "% Procedures: ", !IO),
        io.write(Stream, PredProcIds, !IO),
        io.nl(Stream, !IO)
    ;
        MaybePredProcIds = no
    ),
    write_constraint_proof_map(Stream, 1, VarNamePrint, VarSet,
        ProofMap, !IO),
    io.nl(Stream, !IO).

:- pred write_instance_methods(io.text_output_stream::in,
    list(instance_method)::in, int::in, io::di, io::uo) is det.

write_instance_methods(_, [], _, !IO).
write_instance_methods(Stream, [Method | Methods], !.CurMethodNum, !IO) :-
    Method = instance_method(PredOrFunc, MethodName, _Defn, Arity, _Context),
    write_indent(Stream, 1, !IO),
    io.format(Stream, "%% method %d, %s %s/%d\n",
        [i(!.CurMethodNum), s(pred_or_func_to_str(PredOrFunc)),
        s(sym_name_to_string(MethodName)), i(Arity)], !IO),
    mercury_output_instance_method(Method, Stream, !IO),
    (
        Methods = [_ | _],
        io.write_string(Stream, ",\n", !IO),
        !:CurMethodNum = !.CurMethodNum + 1,
        write_instance_methods(Stream, Methods, !.CurMethodNum, !IO)
    ;
        Methods = []
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_typeclass_table.
%---------------------------------------------------------------------------%
