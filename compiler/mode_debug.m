%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-1997, 2003-2009, 2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2020-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_debug.m.
% Main author: fjh.
%
% This module contains code for tracing the actions of the mode checker.
%
%---------------------------------------------------------------------------%

:- module check_hlds.mode_debug.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module list.

%---------------------------------------------------------------------------%

:- type mode_checkpoint_port
    --->    enter
    ;       exit
    ;       wakeup.

:- inst non_wakeup_port for mode_checkpoint_port/0
    --->    enter
    ;       exit.

    % If `--debug-modes' is enabled, then print a debugging message
    % which includes
    %
    % - the given port,
    %
    % - the goal id of the given goal (the goal the port is for),
    %   if --debug-modes-goal-ids is specified,
    %
    % - the given message string, followed by the given sym_name
    %   with the _sn version,
    %
    % - the word "unique" if we are tracing unique mode analysis,
    %
    % - performance statistics for enter and exit port events,
    %   if --debug-modes-statistics is specified,
    %
    % - the names and (if --debug-mode-verbose is specified) the insts
    %   of the variables in the current instmap, for enter and exit
    %   port events,
    %
    % - for delay events if --debug-modes-delay-vars is specified,
    %   the nonlocal vars of the delayed goal, and the set of waiting-on
    %   variables for each error that is causing the delay.
    %
:- pred mode_checkpoint(mode_checkpoint_port::in(non_wakeup_port),
    string::in, hlds_goal_info::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_checkpoint_sn(mode_checkpoint_port::in(non_wakeup_port),
    string::in, sym_name::in, hlds_goal_info::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_checkpoint_wakeups(hlds_goal::in, list(hlds_goal)::in,
    mode_info::in, mode_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_errors.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

mode_checkpoint(Port, Msg, GoalInfo, !ModeInfo) :-
    mode_info_get_debug_modes(!.ModeInfo, DebugModes),
    (
        DebugModes = no
    ;
        DebugModes = yes(DebugFlags),
        do_mode_checkpoint(Port, Msg, GoalInfo, DebugFlags, !ModeInfo)
    ).

mode_checkpoint_sn(Port, MsgA, SymName, GoalInfo, !ModeInfo) :-
    mode_info_get_debug_modes(!.ModeInfo, DebugModes),
    (
        DebugModes = no
    ;
        DebugModes = yes(DebugFlags),
        Msg = MsgA ++ " " ++ sym_name_to_string(SymName),
        do_mode_checkpoint(Port, Msg, GoalInfo, DebugFlags, !ModeInfo)
    ).

mode_checkpoint_wakeups(HeadWokenGoal, TailWokenGoals, !ModeInfo) :-
    mode_info_get_debug_modes(!.ModeInfo, DebugModes),
    (
        DebugModes = no
    ;
        DebugModes = yes(DebugFlags),
        HeadWokenGoal = hlds_goal(_, HeadWokenGoalInfo),
        do_mode_checkpoint(wakeup, "first goal",
            HeadWokenGoalInfo, DebugFlags, !ModeInfo),
        mode_checkpoint_tail_wakeups(TailWokenGoals, DebugFlags, !ModeInfo)
    ).

:- pred mode_checkpoint_tail_wakeups(list(hlds_goal)::in, mode_debug_flags::in,
    mode_info::in, mode_info::out) is det.

mode_checkpoint_tail_wakeups([], _, !ModeInfo).
mode_checkpoint_tail_wakeups([WokenGoal | WokenGoals], DebugFlags,
        !ModeInfo) :-
    WokenGoal = hlds_goal(_, WokenGoalInfo),
    do_mode_checkpoint(wakeup, "later goal",
        WokenGoalInfo, DebugFlags, !ModeInfo),
    mode_checkpoint_tail_wakeups(WokenGoals, DebugFlags, !ModeInfo).

%---------------------------------------------------------------------------%

:- type maybe_print_insts_stats
    --->    do_not_print_insts_stats(maybe(one_or_more(mode_error_info)))
    ;       print_insts_stats.

:- pred do_mode_checkpoint(mode_checkpoint_port::in, string::in,
    hlds_goal_info::in, mode_debug_flags::in,
    mode_info::in, mode_info::out) is det.

do_mode_checkpoint(Port, Msg, GoalInfo, DebugFlags, !ModeInfo) :-
    (
        Port = enter,
        PortStr = "Enter ",
        MaybePrint = print_insts_stats
    ;
        Port = wakeup,
        PortStr = "Wake ",
        MaybePrint = do_not_print_insts_stats(no)
    ;
        Port = exit,
        mode_info_get_errors(!.ModeInfo, Errors0),
        (
            Errors0 = [],
            PortStr = "Exit ",
            MaybePrint = print_insts_stats
        ;
            Errors0 = [HeadError0 | TailErrors0],
            PortStr = "Delay ",
            OoMErrors0 = one_or_more(HeadError0, TailErrors0),
            MaybePrint = do_not_print_insts_stats(yes(OoMErrors0))
        )
    ),
    DebugFlags = mode_debug_flags(UniquePrefix, mdf_statistics(Statistics),
        DebugVerbose, DebugMinimal, DebugGoalId, DebugDelayVars),
    (
        DebugGoalId = mdf_no_goal_ids,
        GoalIdStr = ""
    ;
        DebugGoalId = mdf_goal_ids,
        goal_id(GoalId) = goal_info_get_goal_id(GoalInfo),
        string.format("for goal #%u: ", [u(GoalId)], GoalIdStr)
    ),
    % Note that the calls to io.format below work because
    % - PortStr always ends with a space,
    % - if nonempty, GoalIdStr and UniquePrefix also always end with a space.
    (
        MaybePrint = print_insts_stats,
        mode_info_get_instmap(!.ModeInfo, InstMap),
        trace [io(!IO)] (
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            module_info_get_globals(ModuleInfo, Globals),
            module_info_get_name(ModuleInfo, ModuleName),
            get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
            io.format(DebugStream, "\n%s%s%s%s:\n",
                [s(PortStr), s(GoalIdStr), s(UniquePrefix), s(Msg)], !IO),
            maybe_report_stats(DebugStream, Statistics, !IO),
            maybe_flush_output(DebugStream, Statistics, !IO),
            ( if instmap_is_reachable(InstMap) then
                instmap_to_assoc_list(InstMap, NewInsts),
                mode_info_get_last_checkpoint_insts(!.ModeInfo, OldInstMap),
                mode_info_get_var_table(!.ModeInfo, VarTable),
                mode_info_get_instvarset(!.ModeInfo, InstVarSet),
                write_var_insts(DebugStream, VarTable, InstVarSet,
                    OldInstMap, DebugVerbose, DebugMinimal, NewInsts, !IO)
            else
                io.write_string(DebugStream, "\tUnreachable\n", !IO)
            ),
            io.flush_output(DebugStream, !IO)
        ),
        mode_info_set_last_checkpoint_insts(InstMap, !ModeInfo)
    ;
        MaybePrint = do_not_print_insts_stats(MaybeHeadError),
        trace [io(!IO)] (
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            module_info_get_globals(ModuleInfo, Globals),
            module_info_get_name(ModuleInfo, ModuleName),
            get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
            io.format(DebugStream, "\n%s%s%s%s:\n",
                [s(PortStr), s(GoalIdStr), s(UniquePrefix), s(Msg)], !IO),
            (
                MaybeHeadError = no
            ;
                MaybeHeadError = yes(OoMErrors),
                (
                    DebugDelayVars = mdf_no_delay_vars
                ;
                    DebugDelayVars = mdf_delay_vars,
                    mode_info_get_var_table(!.ModeInfo, VarTable),
                    NonLocals = goal_info_get_nonlocals(GoalInfo),

                    Desc = "nonlocal vars:",
                    write_var_list(DebugStream, VarTable, Desc,
                        NonLocals, !IO),
                    OoMErrors = one_or_more(HeadError, TailErrors),
                    write_error_vars(DebugStream, VarTable, 1,
                        [HeadError | TailErrors], !IO)
                )
            ),
            io.flush_output(DebugStream, !IO)
        )
    ).

%---------------------%

:- pred write_var_insts(io.text_output_stream::in,
    var_table::in, inst_varset::in, instmap::in,
    mode_debug_flag_verbose::in, mode_debug_flag_minimal::in,
    assoc_list(prog_var, mer_inst)::in, io::di, io::uo) is det.

write_var_insts(_, _, _, _, _, _, [], !IO).
write_var_insts(Stream, VarTable, InstVarSet, OldInstMap,
        DebugVerbose, DebugMinimal, [Var - Inst | VarInsts], !IO) :-
    instmap_lookup_var(OldInstMap, Var, OldInst),
    ( if
        (
            identical_insts(Inst, OldInst)
        ;
            Inst = OldInst
        )
    then
        (
            DebugVerbose = mdf_not_verbose
        ;
            DebugVerbose = mdf_verbose,
            io.write_string(Stream, "\t", !IO),
            mercury_output_var(VarTable, print_name_only, Var, Stream, !IO),
            io.write_string(Stream, " :: unchanged", !IO)
        )
    else
        io.write_string(Stream, "\t", !IO),
        mercury_output_var(VarTable, print_name_only, Var, Stream, !IO),
        (
            DebugMinimal = mdf_minimal,
            io.write_string(Stream, " :: changed\n", !IO)
        ;
            DebugMinimal = mdf_not_minimal,
            io.write_string(Stream, " ::\n", !IO),
            mercury_output_structured_inst(Stream, output_debug, InstVarSet,
                do_not_incl_addr, 2u, Inst, !IO)
        )
    ),
    write_var_insts(Stream, VarTable, InstVarSet, OldInstMap,
        DebugVerbose, DebugMinimal, VarInsts, !IO).

%---------------------%

:- pred write_error_vars(io.text_output_stream::in, var_table::in,
    int::in, list(mode_error_info)::in, io::di, io::uo) is det.

write_error_vars(_DebugStream, _VarTable, _ErrorNum, [], !IO).
write_error_vars(DebugStream, VarTable, ErrorNum, [Error | Errors], !IO) :-
    string.format("vars for error #%d:", [i(ErrorNum)], Desc),
    Error = mode_error_info(Vars, _, _, _),
    write_var_list(DebugStream, VarTable, Desc, Vars, !IO),
    write_error_vars(DebugStream, VarTable, ErrorNum + 1, Errors, !IO).

:- pred write_var_list(io.text_output_stream::in, var_table::in,
    string::in, set_of_progvar::in, io::di, io::uo) is det.

write_var_list(DebugStream, VarTable, Desc, Vars, !IO) :-
    io.format(DebugStream, "%-20s", [s(Desc)], !IO),
    mercury_output_vars(VarTable, print_name_and_num,
       set_of_var.to_sorted_list(Vars), DebugStream, !IO),
    io.nl(DebugStream, !IO).

%---------------------%

    % In the usual case of a C backend, this predicate allows us to conclude
    % that two insts are identical without traversing them. Since the terms
    % can be very large, this is a big gain; it can turn the complexity
    % of printing a checkpoint from quadratic in the number of variables
    % live at the checkpoint (when the variables are e.g. all part of a
    % single long list) to linear. The minor increase in the constant factor
    % in cases where identical_insts fails is much easier to live with.
    %
:- pred identical_insts(mer_inst::in, mer_inst::in) is semidet.

identical_insts(_, _) :-
    semidet_fail.

:- pragma foreign_proc("C",
    identical_insts(InstA::in, InstB::in),
    [will_not_call_mercury, promise_pure],
"
    if (InstA == InstB) {
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

%---------------------------------------------------------------------------%
:- end_module check_hlds.mode_debug.
%---------------------------------------------------------------------------%
