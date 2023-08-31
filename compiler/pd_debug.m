%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pd_debug_m
% Main author: stayl.
%
% Debugging routines for partial deduction.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.pd_debug.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.pd_info.

:- import_module io.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

:- pred pd_debug_message(pd_info::in,
    string::in, list(string.poly_type)::in, io::di, io::uo) is det.

:- pred pd_debug_message_context(pd_info::in, prog_context::in,
    string::in, list(string.poly_type)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred pd_debug_search_version_result(pd_info::in, maybe_version::in,
    io::di, io::uo) is det.

:- pred pd_debug_register_version(pd_info::in, pred_proc_id::in,
    version_info::in, io::di, io::uo) is det.

:- pred pd_debug_write_instmap(pd_info::in, io::di, io::uo) is det.

:- pred pd_debug_write_pred_proc_id_list(pd_info::in, list(pred_proc_id)::in,
    io::di, io::uo) is det.

:- pred pd_debug_output_goal(pd_info::in, string::in, hlds_goal::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module set.

%---------------------------------------------------------------------------%

pd_debug_message(PDInfo, Fmt, Args, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    (
        DebugPD = no
    ;
        DebugPD = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        get_debug_output_stream(Globals, ModuleName, Stream, !IO),
        disable_warning [unknown_format_calls] (
            io.format(Stream, Fmt, Args, !IO)
        ),
        io.flush_output(Stream, !IO)
    ).

pd_debug_message_context(PDInfo, Context, Fmt, Args, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    (
        DebugPD = no
    ;
        DebugPD = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        get_debug_output_stream(Globals, ModuleName, Stream, !IO),
        parse_tree_out_misc.write_context(Stream, Context, !IO),
        disable_warning [unknown_format_calls] (
            io.format(Stream, Fmt, Args, !IO)
        ),
        io.flush_output(Stream, !IO)
    ).

%---------------------------------------------------------------------------%

pd_debug_search_version_result(PDInfo, MaybeVersion, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    (
        DebugPD = no
    ;
        DebugPD = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        get_debug_output_stream(Globals, ModuleName, Stream, !IO),
        (
            MaybeVersion = no_version,
            io.write_string(Stream, "Specialised version not found.\n", !IO)
        ;
            MaybeVersion = version(exact, _, _, _, _),
            io.write_string(Stream, "Exact match found.\n", !IO)
        ;
            MaybeVersion = version(more_general, PredProcId, Version, _, _),
            io.write_string(Stream, "More general version.\n", !IO),
            pd_debug_output_version(Stream, ModuleInfo, PredProcId, Version,
                no, !IO)
        )
    ).

%------------%

pd_debug_register_version(PDInfo, PredProcId, Version, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    (
        DebugPD = no
    ;
        DebugPD = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        get_debug_output_stream(Globals, ModuleName, Stream, !IO),

        io.write_string(Stream, "Registering version:\n", !IO),
        pd_debug_output_version(Stream, ModuleInfo, PredProcId, Version,
            no, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred pd_debug_output_version(io.text_output_stream::in, module_info::in,
    pred_proc_id::in, version_info::in, bool::in, io::di, io::uo) is det.

pd_debug_output_version(Stream, ModuleInfo, PredProcId, Version,
        WriteUnfoldedGoal, !IO) :-
    Version = version_info(Goal, _, Args, _, InstMap,
        InitialCost, CostDelta, Parents, _),
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    PredName = predicate_name(ModuleInfo, PredId),
    PredProcId = proc(PredId, ProcId),
    pred_id_to_int(PredId, PredInt),
    proc_id_to_int(ProcId, ProcInt),

    io.format(Stream, "%s: (PredProcId :%d-%d)\n",
        [s(PredName), i(PredInt), i(ProcInt)], !IO),
    io.format(Stream, " initial cost: %d\n", [i(InitialCost)], !IO),
    io.format(Stream, " cost delta: %d\n", [i(CostDelta)], !IO),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    instmap_restrict(NonLocals, InstMap, InstMap1),
    ArgsStr = mercury_vars_to_string(VarTable, print_name_and_num, Args),
    InstMap1Str = instmap_to_string(VarTable, print_name_and_num, 1, InstMap1),
    io.format(Stream, " args: %s\n%s\n", [s(ArgsStr), s(InstMap1Str)], !IO),
    module_info_get_globals(ModuleInfo, Globals),
    OutInfo = init_hlds_out_info(Globals, output_debug),
    VarNameSrc = vns_var_table(VarTable),
    pred_info_get_typevarset(PredInfo, TVarSet),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    write_goal_nl(OutInfo, Stream, ModuleInfo, VarNameSrc, print_name_and_num,
        TVarSet, InstVarSet, 1, "\n", Goal, !IO),
    set.to_sorted_list(Parents, ParentsList),
    ParentStrs = list.map(pred_proc_id_to_dev_string(ModuleInfo), ParentsList),
    ParentsStr = string.join_list(", ", ParentStrs),
    io.format(Stream, "Parents: %s\n", [s(ParentsStr)], !IO),
    % XXX Neither of our callers specify WriteUnfoldedGoal = yes.
    (
        WriteUnfoldedGoal = yes,
        proc_info_get_goal(ProcInfo, ProcGoal),
        io.write_string(Stream, "Unfolded goal\n", !IO),
        write_goal_nl(OutInfo, Stream, ModuleInfo, VarNameSrc,
            print_name_and_num, TVarSet, InstVarSet, 1, "\n", ProcGoal, !IO)
    ;
        WriteUnfoldedGoal = no
    ).

%------------%

pd_debug_write_instmap(PDInfo, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    (
        DebugPD = no
    ;
        DebugPD = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        get_debug_output_stream(Globals, ModuleName, Stream, !IO),

        pd_info_get_proc_info(PDInfo, ProcInfo),
        proc_info_get_var_table(ProcInfo, VarTable),
        pd_info_get_instmap(PDInfo, InstMap),
        InstMapStr = instmap_to_string(VarTable, print_name_and_num,
            1, InstMap),
        io.write_string(Stream, InstMapStr, !IO),
        io.flush_output(Stream, !IO)
    ).

%------------%

pd_debug_write_pred_proc_id_list(PDInfo, PredProcIds, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    (
        DebugPD = no
    ;
        DebugPD = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        get_debug_output_stream(Globals, ModuleName, Stream, !IO),

        ProcStrs =
            list.map(pred_proc_id_to_dev_string(ModuleInfo), PredProcIds),
        ProcsStr = string.join_list(", ", ProcStrs),
        io.write_string(Stream, ProcsStr, !IO),
        io.flush_output(Stream, !IO)
    ).

%------------%

pd_debug_output_goal(PDInfo, Msg, Goal, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    (
        DebugPD = no
    ;
        DebugPD = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        get_debug_output_stream(Globals, ModuleName, Stream, !IO),

        Goal = hlds_goal(GoalExpr, GoalInfo),
        pd_info_get_pred_info(PDInfo, PredInfo),
        pd_info_get_proc_info(PDInfo, ProcInfo),
        proc_info_get_var_table(ProcInfo, VarTable),
        pd_info_get_instmap(PDInfo, InstMap),
        goal_util.goal_vars(hlds_goal(GoalExpr, GoalInfo), Vars),
        instmap_restrict(Vars, InstMap, VarsInstMap),

        OutInfo = init_hlds_out_info(Globals, output_debug),
        InstmapStr = instmap_to_string(VarTable, print_name_and_num,
            1, VarsInstMap),
        io.format(Stream, "%s%s\n", [s(InstmapStr), s(Msg)], !IO),
        pred_info_get_typevarset(PredInfo, TVarSet),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        write_goal_nl(OutInfo, Stream, ModuleInfo, vns_var_table(VarTable),
            print_name_and_num, TVarSet, InstVarSet, 1, "\n", Goal, !IO),
        io.flush_output(Stream, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.pd_debug.
%---------------------------------------------------------------------------%
