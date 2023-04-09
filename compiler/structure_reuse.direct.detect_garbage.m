%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: structure_reuse.direct.detect_garbage.m.
% Main authors: nancy.
%
% Detect where datastructures become garbage in a given procedure: construct
% a dead cell table.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.direct.detect_garbage.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.

%---------------------------------------------------------------------------%

    % Using the sharing table listing all the structure sharing of all
    % the known procedures, return a table of all data structures that may
    % become available for reuse (i.e. cells that may become dead) of a given
    % procedure goal. The table also records the reuse condition associated
    % with each of the dead cells.
    %
:- pred determine_dead_deconstructions(module_info::in, pred_info::in,
    proc_info::in, sharing_as_table::in, hlds_goal::in,
    dead_cell_table::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.ctgc.datastruct.
:- import_module transform_hlds.ctgc.livedata.

:- import_module bool.
:- import_module io.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- type detect_bg_info
    --->    detect_bg_info(
                dbgi_module_info        :: module_info,
                dbgi_pred_info          :: pred_info,
                dbgi_proc_info          :: proc_info,
                dbgi_sharing_table      :: sharing_as_table,
                dbgi_very_verbose       :: bool
            ).

:- func detect_bg_info_init(module_info, pred_info, proc_info,
    sharing_as_table) = detect_bg_info.

detect_bg_info_init(ModuleInfo, PredInfo, ProcInfo, SharingTable) = BG :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    BG = detect_bg_info(ModuleInfo, PredInfo, ProcInfo, SharingTable,
        VeryVerbose).

determine_dead_deconstructions(ModuleInfo, PredInfo, ProcInfo, SharingTable,
        Goal, DeadCellTable) :-
    Background = detect_bg_info_init(ModuleInfo, PredInfo, ProcInfo,
        SharingTable),
    % In this process we need to know the sharing at each program point,
    % which boils down to reconstructing that sharing information based on
    % the sharing recorded in the sharing table.
    determine_dead_deconstructions_2(Background, Goal,
        sharing_as_init, _, dead_cell_table_init, DeadCellTable),

    % Add a newline after the "progress dots".
    VeryVerbose = Background ^ dbgi_very_verbose,
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            io.nl(StdErr, !IO)
        )
    ;
        VeryVerbose = no
    ).

    % Process a procedure goal, determining the sharing at each subgoal,
    % as well as constructing the table of dead cells.
    %
    % This means:
    %   - at each program point: compute sharing
    %   - at deconstruction unifications: check for a dead cell.
    %
:- pred determine_dead_deconstructions_2(detect_bg_info::in, hlds_goal::in,
    sharing_as::in, sharing_as::out, dead_cell_table::in,
    dead_cell_table::out) is det.

determine_dead_deconstructions_2(Background, TopGoal, !SharingAs,
        !DeadCellTable) :-
    TopGoal = hlds_goal(GoalExpr, GoalInfo),
    ModuleInfo = Background ^ dbgi_module_info,
    PredInfo = Background ^ dbgi_pred_info,
    ProcInfo = Background ^ dbgi_proc_info,
    SharingTable = Background ^ dbgi_sharing_table,
    (
        GoalExpr = conj(_, Goals),
        list.foldl2(determine_dead_deconstructions_2_with_progress(Background),
            Goals, !SharingAs, !DeadCellTable)
    ;
        GoalExpr = plain_call(PredId, ProcId, ActualVars, _, _, _),
        lookup_sharing_and_comb(ModuleInfo, PredInfo, ProcInfo, SharingTable,
            PredId, ProcId, ActualVars, !SharingAs)
    ;
        GoalExpr = generic_call(GenDetails, CallArgs, Modes, _MaybeArgRegs,
            _Detism),
        determine_dead_deconstructions_generic_call(ModuleInfo, ProcInfo,
            GenDetails, CallArgs, Modes, GoalInfo, !SharingAs)
    ;
        GoalExpr = unify(_, _, _, Unification, _),
        unification_verify_reuse(ModuleInfo, ProcInfo, GoalInfo, Unification,
            program_point_init(GoalInfo), !.SharingAs, !DeadCellTable),
        !:SharingAs = add_unify_sharing(ModuleInfo, ProcInfo, Unification,
            GoalInfo, !.SharingAs)
    ;
        GoalExpr = disj(Goals),
        determine_dead_deconstructions_2_disj(Background, Goals, !SharingAs,
            !DeadCellTable)
    ;
        GoalExpr = switch(_, _, Cases),
        determine_dead_deconstructions_2_disj(Background,
            list.map(func(C) = G :- (G = C ^ case_goal), Cases), !SharingAs,
            !DeadCellTable)
    ;
        % XXX To check and compare with the theory.
        GoalExpr = negation(_Goal)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            true
        else
            determine_dead_deconstructions_2(Background, SubGoal, !SharingAs,
                !DeadCellTable)
        )
    ;
        GoalExpr = if_then_else(_, CondGoal, ThenGoal, ElseGoal),
        determine_dead_deconstructions_2(Background, CondGoal, !.SharingAs,
            CondSharingAs, !DeadCellTable),
        determine_dead_deconstructions_2(Background, ThenGoal, CondSharingAs,
            ThenSharingAs, !DeadCellTable),
        determine_dead_deconstructions_2(Background, ElseGoal, !.SharingAs,
            ElseSharingAs, !DeadCellTable),
        !:SharingAs = sharing_as_least_upper_bound(ModuleInfo, ProcInfo,
            ThenSharingAs, ElseSharingAs)
    ;
        GoalExpr = call_foreign_proc(Attributes, ForeignPredId, ForeignProcId,
            Args, _ExtraArgs, _MaybeTraceRuntimeCond, _Impl),
        ForeignPPId = proc(ForeignPredId, ForeignProcId),
        Context = goal_info_get_context(GoalInfo),
        add_foreign_proc_sharing(ModuleInfo, PredInfo, ProcInfo, ForeignPPId,
            Attributes, Args, Context, !SharingAs)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred determine_dead_deconstructions_2_with_progress(detect_bg_info::in,
    hlds_goal::in, sharing_as::in, sharing_as::out, dead_cell_table::in,
    dead_cell_table::out) is det.

determine_dead_deconstructions_2_with_progress(Background, TopGoal,
        !SharingAs, !DeadCellTable) :-
    VeryVerbose = Background ^ dbgi_very_verbose,
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            io.write_char(StdErr, '.', !IO),
            io.flush_output(StdErr, !IO)
        )
    ;
        VeryVerbose = no
    ),
    determine_dead_deconstructions_2(Background, TopGoal, !SharingAs,
        !DeadCellTable).

:- pred determine_dead_deconstructions_2_disj(detect_bg_info::in,
    hlds_goals::in, sharing_as::in, sharing_as::out,
    dead_cell_table::in, dead_cell_table::out) is det.

determine_dead_deconstructions_2_disj(Background, Goals,
        !SharingAs, !DeadCellTable) :-
    list.foldl2(determine_dead_deconstructions_2_disj_goal(Background,
        !.SharingAs), Goals, !SharingAs, !DeadCellTable).

:- pred determine_dead_deconstructions_2_disj_goal(detect_bg_info::in,
    sharing_as::in, hlds_goal::in, sharing_as::in, sharing_as::out,
    dead_cell_table::in, dead_cell_table::out) is det.

determine_dead_deconstructions_2_disj_goal(Background, SharingBeforeDisj,
        Goal, !SharingAs, !DeadCellTable) :-
    determine_dead_deconstructions_2(Background, Goal, SharingBeforeDisj,
        GoalSharing, !DeadCellTable),
    !:SharingAs = sharing_as_least_upper_bound(Background ^ dbgi_module_info,
        Background ^ dbgi_proc_info, !.SharingAs, GoalSharing).

:- pred determine_dead_deconstructions_generic_call(module_info::in,
    proc_info::in, generic_call::in, prog_vars::in, list(mer_mode)::in,
    hlds_goal_info::in, sharing_as::in, sharing_as::out) is det.

determine_dead_deconstructions_generic_call(ModuleInfo, ProcInfo,
        GenDetails, CallArgs, Modes, GoalInfo, !SharingAs) :-
    (
        ( GenDetails = higher_order(_, _, _, _)
        ; GenDetails = class_method(_, _, _, _)
        ),
        proc_info_get_var_table(ProcInfo, CallerVarTable),
        lookup_var_types(CallerVarTable, CallArgs, ActualTypes),
        ( if
            bottom_sharing_is_safe_approximation_by_args(ModuleInfo, Modes,
                ActualTypes)
        then
            SetToTop = no
        else
            SetToTop = yes
        )
    ;
        ( GenDetails = event_call(_) % XXX too conservative
        ; GenDetails = cast(_)
        ),
        SetToTop = yes
    ),
    (
        SetToTop = yes,
        Context = goal_info_get_context(GoalInfo),
        context_to_string(Context, ContextString),
        !:SharingAs = sharing_as_top_sharing_accumulate(
            top_cannot_improve("generic call (" ++ ContextString ++ ")"),
            !.SharingAs)
    ;
        SetToTop = no
    ).

    % Verify whether the unification is a deconstruction in which the
    % deconstructed data structure becomes garbage (under some reuse
    % conditions).
    %
    % XXX Different implementation from the reuse-branch implementation.
    %
:- pred unification_verify_reuse(module_info::in, proc_info::in,
    hlds_goal_info::in, unification::in, program_point::in,
    sharing_as::in, dead_cell_table::in, dead_cell_table::out) is det.

unification_verify_reuse(ModuleInfo, ProcInfo, GoalInfo, Unification,
        PP, Sharing, !DeadCellTable):-
    (
        Unification = deconstruct(Var, ConsId, _, _, _, _),
        LFU = goal_info_get_lfu(GoalInfo),
        LBU = goal_info_get_lbu(GoalInfo),
        ( if
            % Reuse is only relevant for real constructors, with nonzero
            % arities.
            ConsId = cons(_, Arity, _),
            Arity \= 0,

            % No-tag values don't have a cell to reuse.
            proc_info_get_var_table(ProcInfo, VarTable),
            lookup_var_type(VarTable, Var, Type),
            not type_is_no_tag_type(ModuleInfo, Type),

            % Check if the top cell datastructure of Var is not live.
            % If Sharing is top, then everything should automatically
            % be considered live, hence no reuse possible.
            not sharing_as_is_top(Sharing),

            % Check the live set of data structures at this program point.
            var_not_live(ModuleInfo, VarTable, GoalInfo, Sharing, Var)
        then
            % If all the above conditions are met, then the top cell
            % data structure based on Var is dead right after this
            % deconstruction, which means it may be reused.
            NewCondition = reuse_condition_init(ModuleInfo, ProcInfo, Var,
                LFU, LBU, Sharing),
            dead_cell_table_set(PP, NewCondition, !DeadCellTable)
        else
            true
        )
    ;
        Unification = construct(_, _, _, _, _, _, _)
    ;
        Unification = assign(_, _)
    ;
        Unification = simple_test(_, _)
    ;
        Unification = complicated_unify(_, _, _),
        unexpected($pred, "complicated_unify")
    ).

:- pred var_not_live(module_info::in, var_table::in,
    hlds_goal_info::in, sharing_as::in, prog_var::in) is semidet.

var_not_live(ModuleInfo, VarTable, GoalInfo, Sharing, Var) :-
    LiveData = livedata_init_at_goal(ModuleInfo, VarTable, GoalInfo, Sharing),
    nodes_are_not_live(ModuleInfo, VarTable, [datastruct_init(Var)], LiveData,
        NotLive),
    (
        NotLive = nodes_are_live([])
    ;
        ( NotLive = nodes_all_live
        ; NotLive = nodes_are_live([_ | _])
        ),
        fail
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.direct.detect_garbage.
%---------------------------------------------------------------------------%
