%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: simplify_polymorphism.m.
%
% This module generates warnings if the output of the polymorphism pass
% can cause problems for later passes, and specifically for code generation
% in the presence of type_info liveness. The issue we are looking for
% if the one described by Mantis bug #585, which is caused by
%
% - different branches of a disjunction, switch or if-then-else
%   have different variables representing the typeclass_info of
%   the same constraint, but
%
% - the rtti_varmaps of the procedure can record only of those variables.
%
% With typeinfo liveness, accesses from any non-chosen branch
% will attempt to access a variable that is not live in that branch.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_polymorphism.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc.
:- import_module hlds.pred_proc_id.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred maybe_check_typeclass_records(pred_id::in, proc_id::in,
    pred_info::in, pred_info::out, proc_info::in, proc_info::out,
    module_info::in, module_info::out,
    list(diag_spec)::in, list(diag_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.arg_info.
:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

maybe_check_typeclass_records(PredId, ProcId,
        !PredInfo, !ProcInfo, !ModuleInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, body_typeinfo_liveness,
        BodyTypeInfoLiveness),
    (
        BodyTypeInfoLiveness = no
    ;
        BodyTypeInfoLiveness = yes,
        % If there are no constraints that have two or more typeclass_info
        % variables, then having two branches of a branched control
        % structure generating different typeclass_infos for the same
        % constraint is not possible.
        %
        % This is a cheap test to make, because its cost is bounded
        % by the size of the RttiVarMaps, which is just about always small.
        % This is why we start with that, and go on to traverse the body goal
        % (which may be MUCH bigger) only if we have to.
        proc_info_get_var_table(!.ProcInfo, VarTable),
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps),
        compute_duplicate_typeclass_info_vars(VarTable, RttiVarMaps,
            DupTCIVarsToConstraintMap),
        ( if map.is_empty(DupTCIVarsToConstraintMap) then
            true
        else
            check_typeclass_records(!.ModuleInfo, PredId, ProcId, !.PredInfo,
                DupTCIVarsToConstraintMap, !ProcInfo, !Specs),
            module_info_set_pred_proc_info(PredId, ProcId,
                !PredInfo, !.ProcInfo, !ModuleInfo)
        )
    ).

:- pred check_typeclass_records(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, map(prog_var, prog_constraint)::in,
    proc_info::in, proc_info::out,
    list(diag_spec)::in, list(diag_spec)::out) is det.

check_typeclass_records(ModuleInfo, PredId, ProcId, PredInfo,
        DupTCIVarsToConstraintMap, !ProcInfo, !Specs) :-
    fill_goal_id_slots_in_proc(ModuleInfo, _ContainingGoalMap, !ProcInfo),
    compute_proc_tci_vars(!.ProcInfo, TCIVars0),
    % ZZZ Changed
    elim_conjunctive_tci_var_renames(TCIVars0, _ElimTCIVarsConj, !ProcInfo),
    elim_disjunctive_tci_var_renames(ModuleInfo, TCIVars0,
        DupTCIVarsToConstraintMap, ConflictConstraints, _ElimTCIVarsDisj,
        !ProcInfo),
    % ZZZ delete _ElimTCIVars from var_table and rtti_varmaps (if not empty).
    ( if one_or_more_map.is_empty(ConflictConstraints) then
        true
    else
        report_typeclass_info_problem(ModuleInfo, PredId, ProcId,
            PredInfo, !.ProcInfo, ConflictConstraints, !Specs)
    ).

%---------------------------------------------------------------------------%

% ZZZ move later
:- type tci_vars_used_after_map == map(goal_id, set_of_progvar).

:- pred build_tci_vars_used_after_map(module_info::in, proc_info::in,
    set_of_progvar::in, tci_vars_used_after_map::out) is det.

build_tci_vars_used_after_map(ModuleInfo, ProcInfo, TCIVars, UsedAfterMap) :-
    proc_info_get_var_table(ProcInfo, VarTable),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    compute_in_and_out_vars(ModuleInfo, VarTable, HeadVars, ArgModes,
        _InputHeadVars, OutputHeadVars),
    list.filter(set_of_var.contains(TCIVars), OutputHeadVars,
        OutputTCIHeadVars),
    set_of_var.list_to_set(OutputTCIHeadVars, OutputTCIHeadVarSet),
    proc_info_get_goal(ProcInfo, Goal),
    % The OutputTCIHeadVars in effect occur *after* Goal.
    record_used_tci_vars_in_goal(TCIVars, Goal,
        OutputTCIHeadVarSet, _LaterTCIVars, map.init, UsedAfterMap).

:- pred record_used_tci_vars_in_goal(set_of_progvar::in,
    hlds_goal::in, set_of_progvar::in, set_of_progvar::out,
    tci_vars_used_after_map::in, tci_vars_used_after_map::out) is det.

record_used_tci_vars_in_goal(TCIVars, Goal, !LaterTCIVars, !UsedAfterMap) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    GoalId = goal_info_get_goal_id(GoalInfo),
    map.det_insert(GoalId, !.LaterTCIVars, !UsedAfterMap),
    (
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        set_of_var.intersect(TCIVars, ChangedVars, ChangedTCIVars),
        set_of_var.union(ChangedTCIVars, !LaterTCIVars)
    ;
        GoalExpr = conj(ConjKind, Conjuncts),
        (
            ConjKind = plain_conj,
            list.reverse(Conjuncts, RevConjuncts),
            record_used_tci_vars_in_rev_plain_conj(TCIVars, RevConjuncts,
                !LaterTCIVars, !UsedAfterMap)
        ;
            ConjKind = parallel_conj,
            unexpected($pred, "parallel_conj")
        )
    ;
        GoalExpr = disj(Disjuncts),
        record_used_tci_vars_in_disj(TCIVars, Disjuncts,
            !.LaterTCIVars, LaterTCIVarsList, !UsedAfterMap),
        set_of_var.union_list(LaterTCIVarsList, !:LaterTCIVars)
    ;
        GoalExpr = switch(_, _, Cases),
        record_used_tci_vars_in_cases(TCIVars, Cases,
            !.LaterTCIVars, LaterTCIVarsList, !UsedAfterMap),
        set_of_var.union_list(LaterTCIVarsList, !:LaterTCIVars)
    ;
        GoalExpr = if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal),
        record_used_tci_vars_in_goal(TCIVars, ThenGoal,
            !.LaterTCIVars, LaterTCIVarsBeforeThen, !UsedAfterMap),
        record_used_tci_vars_in_goal(TCIVars, CondGoal,
            LaterTCIVarsBeforeThen, LaterTCIVarsBeforeCond, !UsedAfterMap),
        record_used_tci_vars_in_goal(TCIVars, ElseGoal,
            !.LaterTCIVars, LaterTCIVarsBeforeElse, !UsedAfterMap),
        set_of_var.union(LaterTCIVarsBeforeCond, LaterTCIVarsBeforeElse,
            !:LaterTCIVars)
    ;
        GoalExpr = negation(SubGoal),
        record_used_tci_vars_in_goal(TCIVars, SubGoal,
            !LaterTCIVars, !UsedAfterMap)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            true
        else
            record_used_tci_vars_in_goal(TCIVars, SubGoal,
                !LaterTCIVars, !UsedAfterMap)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, _Outer, _Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            Disjuncts = [MainGoal | OrElseGoals],
            record_used_tci_vars_in_disj(TCIVars, Disjuncts,
                !.LaterTCIVars, LaterTCIVarsList, !UsedAfterMap),
            set_of_var.union_list(LaterTCIVarsList, !:LaterTCIVars)
        ;
            ShortHand = try_goal(_, _, _),
            % These should have been expanded out by now.
            unexpected($pred, "try_goal")
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred record_used_tci_vars_in_rev_plain_conj(set_of_progvar::in,
    list(hlds_goal)::in, set_of_progvar::in, set_of_progvar::out,
    tci_vars_used_after_map::in, tci_vars_used_after_map::out) is det.

record_used_tci_vars_in_rev_plain_conj(_TCIVars, [],
        !LaterTCIVars, !UsedAfterMap).
record_used_tci_vars_in_rev_plain_conj(TCIVars, [RevGoal | RevGoals],
        !LaterTCIVars, !UsedAfterMap) :-
    record_used_tci_vars_in_goal(TCIVars, RevGoal,
        !LaterTCIVars, !UsedAfterMap),
    record_used_tci_vars_in_rev_plain_conj(TCIVars, RevGoals,
        !LaterTCIVars, !UsedAfterMap).

:- pred record_used_tci_vars_in_disj(set_of_progvar::in,
    list(hlds_goal)::in, set_of_progvar::in, list(set_of_progvar)::out,
    tci_vars_used_after_map::in, tci_vars_used_after_map::out) is det.

record_used_tci_vars_in_disj(_TCIVars, [], _LaterTCIVars0, [], !UsedAfterMap).
record_used_tci_vars_in_disj(TCIVars, [Goal | Goals],
        LaterTCIVars0, [HeadLaterTCIVars | TailLaterTCIVars], !UsedAfterMap) :-
    record_used_tci_vars_in_goal(TCIVars, Goal,
        LaterTCIVars0, HeadLaterTCIVars, !UsedAfterMap),
    record_used_tci_vars_in_disj(TCIVars, Goals,
        LaterTCIVars0, TailLaterTCIVars, !UsedAfterMap).

:- pred record_used_tci_vars_in_cases(set_of_progvar::in,
    list(case)::in, set_of_progvar::in, list(set_of_progvar)::out,
    tci_vars_used_after_map::in, tci_vars_used_after_map::out) is det.

record_used_tci_vars_in_cases(_TCIVars, [], _LaterTCIVars0, [], !UsedAfterMap).
record_used_tci_vars_in_cases(TCIVars, [Case | Cases],
        LaterTCIVars0, [HeadLaterTCIVars | TailLaterTCIVars], !UsedAfterMap) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    record_used_tci_vars_in_goal(TCIVars, Goal,
        LaterTCIVars0, HeadLaterTCIVars, !UsedAfterMap),
    record_used_tci_vars_in_cases(TCIVars, Cases,
        LaterTCIVars0, TailLaterTCIVars, !UsedAfterMap).

%---------------------------------------------------------------------------%

:- pred elim_conjunctive_tci_var_renames(set_of_progvar::in,
    set(prog_var)::out, proc_info::in, proc_info::out) is det.

elim_conjunctive_tci_var_renames(TCIVars0, ElimTCIVars, !ProcInfo) :-
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    rtti_varmaps_components(RttiVarMaps0, _, _, _, TCIVarToConstraintMap),
    proc_info_get_goal(!.ProcInfo, Goal0),
    acc_conjunctive_tci_var_renames_in_goal(TCIVars0, TCIVarToConstraintMap,
        Goal0, map.init, _SeenConstraintToVarMap, map.init, Renaming),
    rename_some_vars_in_goal(Renaming, Goal0, Goal),
    proc_info_set_goal(Goal, !ProcInfo),
    map.keys_as_set(Renaming, ElimTCIVars).

:- pred acc_conjunctive_tci_var_renames_in_goal(set_of_progvar::in,
    tci_var_to_constraint_map::in, hlds_goal::in,
    tci_constraint_to_var_map::in, tci_constraint_to_var_map::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

acc_conjunctive_tci_var_renames_in_goal(TCIVars, TCIVarToConstraintMap, Goal,
        !SeenConstraintToVarMap, !Renaming) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        set_of_var.intersect(TCIVars, ChangedVars, ChangedTCIVars),
        set_of_var.fold2(
            record_tci_var_bind_in_conjunctive(TCIVarToConstraintMap),
            ChangedTCIVars, !SeenConstraintToVarMap, !Renaming)
    ;
        GoalExpr = conj(ConjKind, Conjuncts),
        (
            ConjKind = plain_conj,
            acc_conjunctive_tci_var_renames_in_plain_conj(TCIVars,
                TCIVarToConstraintMap, Conjuncts,
                !SeenConstraintToVarMap, !Renaming)
        ;
            ConjKind = parallel_conj,
            unexpected($pred, "parallel_conj")
        )
    ;
        GoalExpr = disj(Disjuncts),
        acc_conjunctive_tci_var_renames_in_disj(TCIVars,
            TCIVarToConstraintMap, Disjuncts,
            !.SeenConstraintToVarMap, SeenConstraintToVarMaps, !Renaming),
        !:SeenConstraintToVarMap =
            map.common_subset_list(SeenConstraintToVarMaps)
    ;
        GoalExpr = switch(_, _, Cases),
        acc_conjunctive_tci_var_renames_in_cases(TCIVars,
            TCIVarToConstraintMap, Cases,
            !.SeenConstraintToVarMap, SeenConstraintToVarMaps, !Renaming),
        !:SeenConstraintToVarMap =
            map.common_subset_list(SeenConstraintToVarMaps)
    ;
        GoalExpr = if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal),
        acc_conjunctive_tci_var_renames_in_goal(TCIVars,
            TCIVarToConstraintMap, CondGoal,
            !.SeenConstraintToVarMap, SeenConstraintToVarMapC, !Renaming),
        acc_conjunctive_tci_var_renames_in_goal(TCIVars,
            TCIVarToConstraintMap, ThenGoal,
            SeenConstraintToVarMapC, SeenConstraintToVarMapT, !Renaming),
        acc_conjunctive_tci_var_renames_in_goal(TCIVars,
            TCIVarToConstraintMap, ElseGoal,
            !.SeenConstraintToVarMap, SeenConstraintToVarMapE, !Renaming),
        !:SeenConstraintToVarMap = map.common_subset_list(
            [SeenConstraintToVarMapT, SeenConstraintToVarMapE])
    ;
        GoalExpr = negation(SubGoal),
        acc_conjunctive_tci_var_renames_in_goal(TCIVars,
            TCIVarToConstraintMap, SubGoal,
            !SeenConstraintToVarMap, !Renaming)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            true
        else
            acc_conjunctive_tci_var_renames_in_goal(TCIVars,
                TCIVarToConstraintMap, SubGoal,
                !SeenConstraintToVarMap, !Renaming)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, _Outer, _Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            Disjuncts = [MainGoal | OrElseGoals],
            acc_conjunctive_tci_var_renames_in_disj(TCIVars,
                TCIVarToConstraintMap, Disjuncts,
                !.SeenConstraintToVarMap, SeenConstraintToVarMaps, !Renaming),
            !:SeenConstraintToVarMap =
                map.common_subset_list(SeenConstraintToVarMaps)
        ;
            ShortHand = try_goal(_, _, _),
            % These should have been expanded out by now.
            unexpected($pred, "try_goal")
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred acc_conjunctive_tci_var_renames_in_plain_conj(set_of_progvar::in,
    tci_var_to_constraint_map::in, list(hlds_goal)::in,
    tci_constraint_to_var_map::in, tci_constraint_to_var_map::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

acc_conjunctive_tci_var_renames_in_plain_conj(_, _,
        [], !SeenConstraintToVarMap, !Renaming).
acc_conjunctive_tci_var_renames_in_plain_conj(TCIVars, TCIVarToConstraintMap,
        [Conjunct | Conjuncts], !SeenConstraintToVarMap, !Renaming) :-
    acc_conjunctive_tci_var_renames_in_goal(TCIVars,
        TCIVarToConstraintMap, Conjunct, !SeenConstraintToVarMap, !Renaming),
    acc_conjunctive_tci_var_renames_in_plain_conj(TCIVars,
        TCIVarToConstraintMap, Conjuncts, !SeenConstraintToVarMap, !Renaming).

:- pred acc_conjunctive_tci_var_renames_in_disj(set_of_progvar::in,
    tci_var_to_constraint_map::in, list(hlds_goal)::in,
    tci_constraint_to_var_map::in, list(tci_constraint_to_var_map)::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

acc_conjunctive_tci_var_renames_in_disj(_, _, [], _, [], !Renaming).
acc_conjunctive_tci_var_renames_in_disj(TCIVars, TCIVarToConstraintMap,
        [Disjunct | Disjuncts], SeenConstraintToVarMap0,
        [SeenConstraintToVarMap | SeenConstraintToVarMaps], !Renaming) :-
    acc_conjunctive_tci_var_renames_in_goal(TCIVars, TCIVarToConstraintMap,
        Disjunct,
        SeenConstraintToVarMap0, SeenConstraintToVarMap, !Renaming),
    acc_conjunctive_tci_var_renames_in_disj(TCIVars, TCIVarToConstraintMap,
        Disjuncts,
        SeenConstraintToVarMap0, SeenConstraintToVarMaps, !Renaming).

:- pred acc_conjunctive_tci_var_renames_in_cases(set_of_progvar::in,
    tci_var_to_constraint_map::in, list(case)::in,
    tci_constraint_to_var_map::in, list(tci_constraint_to_var_map)::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

acc_conjunctive_tci_var_renames_in_cases(_, _, [], _, [], !Renaming).
acc_conjunctive_tci_var_renames_in_cases(TCIVars, TCIVarToConstraintMap,
        [Case | Cases], SeenConstraintToVarMap0,
        [SeenConstraintToVarMap | SeenConstraintToVarMaps], !Renaming) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    acc_conjunctive_tci_var_renames_in_goal(TCIVars, TCIVarToConstraintMap,
        Goal,
        SeenConstraintToVarMap0, SeenConstraintToVarMap, !Renaming),
    acc_conjunctive_tci_var_renames_in_cases(TCIVars, TCIVarToConstraintMap,
        Cases,
        SeenConstraintToVarMap0, SeenConstraintToVarMaps, !Renaming).

:- pred record_tci_var_bind_in_conjunctive(tci_var_to_constraint_map::in,
    prog_var::in,
    tci_constraint_to_var_map::in, tci_constraint_to_var_map::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

record_tci_var_bind_in_conjunctive(TCIVarToConstraintMap, TCIVar,
        !SeenConstraintToVarMap, !Renaming) :-
    map.lookup(TCIVarToConstraintMap, TCIVar, Constraint),
    ( if map.search(!.SeenConstraintToVarMap, Constraint, OldTCIVar) then
        ( if TCIVar = OldTCIVar then
            % ZZZ We should delete this atomic goal, PROVIDED
            % that TCIVar is its only output.
            true
        else
            map.det_insert(TCIVar, OldTCIVar, !Renaming)
        )
    else
        map.det_insert(Constraint, TCIVar, !SeenConstraintToVarMap)
    ).

%---------------------------------------------------------------------------%

:- pred elim_disjunctive_tci_var_renames(module_info::in, set_of_progvar::in,
     map(prog_var, prog_constraint)::in,
     conflict_constraints::out, set(prog_var)::out,
     proc_info::in, proc_info::out) is det.

elim_disjunctive_tci_var_renames(ModuleInfo, TCIVars0,
        DupTCIVarsToConstraintMap, ConflictConstraints, ElimTCIVars,
        !ProcInfo) :-
    build_tci_vars_used_after_map(ModuleInfo, !.ProcInfo, TCIVars0,
        TCIVarsUsedAfterMap),
    map.keys_as_set(DupTCIVarsToConstraintMap, DupTCIVarsSet),
    DupTCIVars = set_of_var.set_to_bitset(DupTCIVarsSet),
    proc_info_get_initial_instmap(ModuleInfo, !.ProcInfo, InstMap0),
    proc_info_get_goal(!.ProcInfo, Goal0),
    look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
        DupTCIVars, DupTCIVarsToConstraintMap, InstMap0, Goal0,
        one_or_more_map.init, _, one_or_more_map.init, ConflictConstraints,
        map.init, Renaming),
    rename_some_vars_in_goal(Renaming, Goal0, Goal),
    proc_info_set_goal(Goal, !ProcInfo),
    map.keys_as_set(Renaming, ElimTCIVars).

:- type constraint_tci_db == one_or_more_map(prog_constraint, constraint_tci).
:- type constraint_tci
    --->    constraint_tci(prog_var, prog_context).

:- type conflict_constraints == constraint_tci_db.

    % Look for branched goals in which two different branches
    % produce typeclass_infos for the same constraint.
    %
    % Because the polymorphism pass always generates code that either
    % constructs or retrieves typeclass_infos just before goals that
    % need them, and only ever reuses the variables that hold them
    % in straight line code, the typeclass_infos generated in
    % different branches are guaranteed be in different variables.
    % This is a problem, because the current design of the rtti_varmaps
    % structure allows us to record only one of those typeclass_info vars
    % as the holder of the available information about that constraint.
    % This is the cause of Mantis bug #585, whose entry discusses the issue
    % in detail.
    %
:- pred look_for_typeclass_info_conflict_in_goal(tci_vars_used_after_map::in,
    set_of_progvar::in, map(prog_var, prog_constraint)::in, instmap::in,
    hlds_goal::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

% ZZZ InstMap0 may be unused
look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0, Goal,
        !TCIConstraints, !ConflictConstraints, !Renaming) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        set_of_var.intersect(DupTCIVars, ChangedVars, ChangedDupTCIVars),
        ( if set_of_var.is_non_empty(ChangedDupTCIVars) then
            Context = goal_info_get_context(GoalInfo),
            set_of_var.to_sorted_list(ChangedDupTCIVars, ChangedDupTCIVarList),
            list.foldl(
                record_delta_typeclass_info_var(TCIVarToConstraintMap,
                    Context),
                ChangedDupTCIVarList, !TCIConstraints)
        else
            true
        )
    ;
        GoalExpr = conj(_ConjKind, Conjuncts),
        % Neither debugging nor accurate gc, the two reasons for
        % turning on typeinfo liveness, support parallel conjunctions.
        % Therefore we should not get here if _ConjKind = parallel_conj.
        % However, the code intended for plain_conjs should also work
        % for parallel_conjs as well.
        look_for_typeclass_info_conflict_in_plain_conj(TCIVarsUsedAfterMap,
            DupTCIVars, TCIVarToConstraintMap, InstMap0, Conjuncts,
            !TCIConstraints, !ConflictConstraints, !Renaming)
    ;
        GoalExpr = disj(Disjuncts),
        GoalId = goal_info_get_goal_id(GoalInfo),
        look_for_typeclass_info_conflict_in_disj(TCIVarsUsedAfterMap,
            DupTCIVars, TCIVarToConstraintMap, InstMap0, GoalId, Disjuncts,
            !TCIConstraints, !ConflictConstraints, !Renaming)
    ;
        GoalExpr = switch(_, _, Cases),
        GoalId = goal_info_get_goal_id(GoalInfo),
        look_for_typeclass_info_conflict_in_switch(TCIVarsUsedAfterMap,
            DupTCIVars, TCIVarToConstraintMap, InstMap0, GoalId, Cases,
            !TCIConstraints, !ConflictConstraints, !Renaming)
    ;
        GoalExpr = if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal),
        look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
            DupTCIVars, TCIVarToConstraintMap, InstMap0, CondGoal,
            one_or_more_map.init, CondTCIConstraints,
            !ConflictConstraints, !Renaming),
        apply_goal_instmap_delta(CondGoal, InstMap0, InstMapAfterCond),
        look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
            DupTCIVars, TCIVarToConstraintMap, InstMapAfterCond, ThenGoal,
            CondTCIConstraints, ThenTCIConstraints,
            !ConflictConstraints, !Renaming),
        look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
            DupTCIVars, TCIVarToConstraintMap, InstMap0, ElseGoal,
            one_or_more_map.init, ElseTCIConstraints,
            !ConflictConstraints, !Renaming),
        GoalId = goal_info_get_goal_id(GoalInfo),
        map.lookup(TCIVarsUsedAfterMap, GoalId, TCIVarsUsedAfter),
        detect_conflicts_in_arms(TCIVarsUsedAfter,
            [ThenTCIConstraints, ElseTCIConstraints],
            !TCIConstraints, !ConflictConstraints, !Renaming)
    ;
        GoalExpr = negation(SubGoal),
        look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
            DupTCIVars, TCIVarToConstraintMap, InstMap0, SubGoal,
            !TCIConstraints, !ConflictConstraints, !Renaming)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            true
        else
            look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
                DupTCIVars, TCIVarToConstraintMap, InstMap0, SubGoal,
                !TCIConstraints, !ConflictConstraints, !Renaming)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, _Outer, _Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            GoalId = goal_info_get_goal_id(GoalInfo),
            Disjuncts = [MainGoal | OrElseGoals],
            look_for_typeclass_info_conflict_in_disj(TCIVarsUsedAfterMap,
                DupTCIVars, TCIVarToConstraintMap, InstMap0, GoalId, Disjuncts,
                !TCIConstraints, !ConflictConstraints, !Renaming)
        ;
            ShortHand = try_goal(_, _, _),
            % These should have been expanded out by now.
            unexpected($pred, "try_goal")
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred record_delta_typeclass_info_var(map(prog_var, prog_constraint)::in,
    prog_context::in, prog_var::in,
    constraint_tci_db::in, constraint_tci_db::out) is det.

record_delta_typeclass_info_var(TCIVarToConstraintMap, Context, TCIVar,
        !TCIConstraints) :-
    map.lookup(TCIVarToConstraintMap, TCIVar, Constraint),
    ConstraintTCI = constraint_tci(TCIVar, Context),
    one_or_more_map.add(Constraint, ConstraintTCI, !TCIConstraints).

%---------------------%

:- pred look_for_typeclass_info_conflict_in_plain_conj(
    tci_vars_used_after_map::in, set_of_progvar::in,
    map(prog_var, prog_constraint)::in, instmap::in, list(hlds_goal)::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

look_for_typeclass_info_conflict_in_plain_conj(_, _, _, _, [],
        !TCIConstraints, !ConflictConstraints, !Renaming).
look_for_typeclass_info_conflict_in_plain_conj(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0, [Conjunct | Conjuncts],
        !TCIConstraints, !ConflictConstraints, !Renaming) :-
    look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0, Conjunct,
        !TCIConstraints, !ConflictConstraints, !Renaming),
    apply_goal_instmap_delta(Conjunct, InstMap0, InstMap1),
    look_for_typeclass_info_conflict_in_plain_conj(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap1, Conjuncts,
        !TCIConstraints, !ConflictConstraints, !Renaming).

%---------------------%

:- pred look_for_typeclass_info_conflict_in_disj(tci_vars_used_after_map::in,
    set_of_progvar::in, map(prog_var, prog_constraint)::in,
    instmap::in, goal_id::in, list(hlds_goal)::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

look_for_typeclass_info_conflict_in_disj(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0, GoalId, Disjuncts,
        !TCIConstraints, !ConflictConstraints, !Renaming) :-
    look_for_typeclass_info_conflict_in_disjuncts(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0,
        Disjuncts, ArmTCIConstraints, !ConflictConstraints, !Renaming),
    map.lookup(TCIVarsUsedAfterMap, GoalId, TCIVarsUsedAfter),
    detect_conflicts_in_arms(TCIVarsUsedAfter, ArmTCIConstraints,
        !TCIConstraints, !ConflictConstraints, !Renaming).

:- pred look_for_typeclass_info_conflict_in_disjuncts(
    tci_vars_used_after_map::in, set_of_progvar::in,
    map(prog_var, prog_constraint)::in, instmap::in,
    list(hlds_goal)::in, list(constraint_tci_db)::out,
    conflict_constraints::in, conflict_constraints::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

look_for_typeclass_info_conflict_in_disjuncts(_, _, _, _, [], [],
        !ConflictConstraints, !Renaming).
look_for_typeclass_info_conflict_in_disjuncts(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0,
        [HeadDisjunct | TailDisjuncts],
        [HeadTCIConstraints | TailTCIConstraints],
        !ConflictConstraints, !Renaming) :-
    look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0, HeadDisjunct,
        one_or_more_map.init, HeadTCIConstraints,
        !ConflictConstraints, !Renaming),
    look_for_typeclass_info_conflict_in_disjuncts(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0,
        TailDisjuncts, TailTCIConstraints, !ConflictConstraints, !Renaming).

%---------------------%

:- pred look_for_typeclass_info_conflict_in_switch(tci_vars_used_after_map::in,
    set_of_progvar::in, map(prog_var, prog_constraint)::in,
    instmap::in, goal_id::in, list(case)::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

look_for_typeclass_info_conflict_in_switch(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0, GoalId, Disjuncts,
        !TCIConstraints, !ConflictConstraints, !Renaming) :-
    look_for_typeclass_info_conflict_in_cases(TCIVarsUsedAfterMap, DupTCIVars,
        TCIVarToConstraintMap, InstMap0, Disjuncts, ArmTCIConstraints,
        !ConflictConstraints, !Renaming),
    map.lookup(TCIVarsUsedAfterMap, GoalId, TCIVarsUsedAfter),
    detect_conflicts_in_arms(TCIVarsUsedAfter, ArmTCIConstraints,
        !TCIConstraints, !ConflictConstraints, !Renaming).

:- pred look_for_typeclass_info_conflict_in_cases(tci_vars_used_after_map::in,
    set_of_progvar::in, map(prog_var, prog_constraint)::in,
    instmap::in, list(case)::in, list(constraint_tci_db)::out,
    conflict_constraints::in, conflict_constraints::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

look_for_typeclass_info_conflict_in_cases(_, _, _, _, [], [],
        !ConflictConstraints, !Renaming).
look_for_typeclass_info_conflict_in_cases(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0,
        [HeadCase | TailCases], [HeadTCIConstraints | TailTCIConstraints],
        !ConflictConstraints, !Renaming) :-
    HeadCase = case(_MainConsId, _OtherConsIds, HeadGoal),
    % We ignore the instmap changes resulting from binding the switched-on
    % variable to MainConsId or OtherConsIds, because they cannot affect
    % the compiler-generated code that binds typeclass infos.
    look_for_typeclass_info_conflict_in_goal(TCIVarsUsedAfterMap,
        DupTCIVars, TCIVarToConstraintMap, InstMap0, HeadGoal,
        one_or_more_map.init, HeadTCIConstraints,
        !ConflictConstraints, !Renaming),
    look_for_typeclass_info_conflict_in_cases(TCIVarsUsedAfterMap, DupTCIVars,
        TCIVarToConstraintMap, InstMap0, TailCases, TailTCIConstraints,
        !ConflictConstraints, !Renaming).

%---------------------%

:- pred detect_conflicts_in_arms(set_of_progvar::in,
    list(constraint_tci_db)::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

% ZZZ do more
detect_conflicts_in_arms(_TCIVarsUsedAfter, ArmTCIConstraints,
        !AllArmTCIConstraints, !ConflictConstraints, Renaming, Renaming) :-
    detect_conflict_constraints_in_arms(ArmTCIConstraints,
        set.init, set.init, DupConstraints, !AllArmTCIConstraints),
    collect_conflict_constraints_in_arms(DupConstraints,
        ArmTCIConstraints, one_or_more_map.init, NewConflictConstraints),
    one_or_more_map.merge(NewConflictConstraints, !ConflictConstraints).

:- pred detect_conflict_constraints_in_arms(list(constraint_tci_db)::in,
    set(prog_constraint)::in,
    set(prog_constraint)::in, set(prog_constraint)::out,
    constraint_tci_db::in, constraint_tci_db::out) is det.

% ZZZ leave computing !AllArmTCIConstraints till AFTER
% we rename tci vars
detect_conflict_constraints_in_arms([], _,
        !DupConstraints, !AllArmTCIConstraints).
detect_conflict_constraints_in_arms(
        [HeadArmTCIConstraints | TailArmTCIConstraints],
        !.SeenConstraints, !DupConstraints, !AllArmTCIConstraints) :-
    one_or_more_map.keys_as_set(HeadArmTCIConstraints, HeadConstraints),
    set.intersect(!.SeenConstraints, HeadConstraints, SeenHeadConstraints),
    ( if set.is_non_empty(SeenHeadConstraints) then
        % This arm and one (or more) of the previous arms both define
        % typeclass_infos for the constraints in SeenHeadConstraints.
        %
        % We *could* record HeadArmTCIConstraints in !ConflictConstraints,
        % but we are too late to record in !ConflictConstraints the
        % constraint_tcis for the *earlier* branch. This is why we just
        % record this as a duplicate constraint, and let the later
        % collect_conflict_constraints_in_arms pass collect constraint_tcis
        % for the DupConstraints from *all* the branches.
        set.union(SeenHeadConstraints, !DupConstraints)
    else
        true
    ),
    set.union(HeadConstraints, !SeenConstraints),
    one_or_more_map.merge(HeadArmTCIConstraints, !AllArmTCIConstraints),
    detect_conflict_constraints_in_arms(TailArmTCIConstraints,
        !.SeenConstraints, !DupConstraints, !AllArmTCIConstraints).

:- pred collect_conflict_constraints_in_arms(set(prog_constraint)::in,
    list(constraint_tci_db)::in,
    conflict_constraints::in, conflict_constraints::out) is det.

collect_conflict_constraints_in_arms(_, [], !ConflictConstraints).
collect_conflict_constraints_in_arms(DupConstraints,
        [HeadArmTCIConstraints | TailArmTCIConstraints],
        !ConflictConstraints) :-
    one_or_more_map.select(HeadArmTCIConstraints, DupConstraints,
        HeadDupTCIConstraints),
    one_or_more_map.merge(HeadDupTCIConstraints, !ConflictConstraints),
    collect_conflict_constraints_in_arms(DupConstraints,
        TailArmTCIConstraints, !ConflictConstraints).

%---------------------%

:- pred report_typeclass_info_problem(module_info::in,
    pred_id::in, proc_id::in, pred_info::in, proc_info::in,
    conflict_constraints::in,
    list(diag_spec)::in, list(diag_spec)::out) is det.

report_typeclass_info_problem(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
        ConflictConstraintSet, !Specs) :-
    ProcPieces = describe_one_proc_name_maybe_argmodes(ModuleInfo,
        output_debug, yes(color_subject), should_not_module_qualify, [],
        proc(PredId, ProcId)),
    pred_info_get_typevarset(PredInfo, TVarSet),
    one_or_more_map.to_sorted_assoc_list(ConflictConstraintSet,
        ConflictConstraints),
    list.map(conflict_constraint_to_piece(TVarSet),
        ConflictConstraints, ConflictConstraintPieceLists0),
    list.intersperse([nl],
        ConflictConstraintPieceLists0, ConflictConstraintPieceLists),
    list.condense(ConflictConstraintPieceLists, ConflictConstraintPieces),
    ( if list.length(ConflictConstraints) > 1 then
        ConflictPieces =
            [words("The constraints involved are:"), nl_indent_delta(1)] ++
            ConflictConstraintPieces ++
            [nl_indent_delta(-1)]
    else
        ConflictPieces =
            [words("The constraint involved is:"), nl_indent_delta(1)] ++
            ConflictConstraintPieces ++
            [nl_indent_delta(-1)]
    ),
    proc_info_get_context(ProcInfo, Context),
    % We say "debugging enabled", because this is by far
    % the most common way for body_typeinfo_liveness to be set.
    % Both alternatives, the use of .agc grades and manual setting
    % of the option, are extremely rare.
    MainPieces = [words("Sorry: the compiler")] ++
        color_as_incorrect([words("cannot correctly compile")]) ++
        ProcPieces ++ [words("with debugging enabled."),
        words("This is due to a known limitation that concerns"),
        words("the mapping between typeclass constraints on the one hand,"),
        words("and the hidden, compiler-generated variables"),
        words("storing information about them on the other hand."),
        nl] ++
        ConflictPieces,
    VerbosePieces =
        [words("The limitation occurs when the definition"),
        words("of a predicate or function contains"),
        words("both the deconstructions of terms"),
        words("that contain existentially typed arguments,"),
        words("and branched code, such as if-then-elses,"),
        words("disjunctions and/or switches."),
        words("You can work around the limitation"),
        words("by moving such deconstruction unifications"),
        words("to helper predicates that contain no branching."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(verbose_once, VerbosePieces)]),
    Phase = phase_simplify(report_in_any_mode),
    Spec = gen_spec($pred, severity_error, Phase, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred conflict_constraint_to_piece(tvarset::in,
    pair(prog_constraint, one_or_more(constraint_tci))::in,
    list(format_piece)::out) is det.

conflict_constraint_to_piece(TVarSet, Constraint - OoMConstraintTCIs,
        Pieces) :-
    strip_module_names_from_constraint(strip_all_module_names,
        set_default_func, Constraint, StrippedConstraint),
    ConstraintStr = mercury_constraint_to_string(TVarSet, print_name_only,
        StrippedConstraint),
    ContextToLineNumberPiece =
        ( func(ConstraintTCI) = LineNumberPiece :-
            ConstraintTCI = constraint_tci(_TCIVar, Context),
            LineNumber = context_line(Context),
            LineNumberPiece = int_fixed(LineNumber)
        ),
    ConstraintTCIs = one_or_more_to_list(OoMConstraintTCIs),
    LineNumberPieces0 = list.map(ContextToLineNumberPiece, ConstraintTCIs),
    list.sort_and_remove_dups(LineNumberPieces0, LineNumberPieces),
    ( if list.length(LineNumberPieces) > 1 then
        OnLineS = "on lines",
        LineSuffixPieces = []
    else
        OnLineS = "on line",
        LineSuffixPieces = [words("(The code on that line"),
            words("may have been duplicated by the compiler.)"), nl]
    ),
    LineNumbersPieces = piece_list_to_pieces("and", LineNumberPieces),
    Pieces = [words(ConstraintStr), nl_indent_delta(1)] ++
        [words(OnLineS)] ++ LineNumbersPieces ++ [nl_indent_delta(-1)] ++
        LineSuffixPieces.

%---------------------------------------------------------------------------%

:- pred compute_proc_tci_vars(proc_info::in, set_of_progvar::out) is det.

compute_proc_tci_vars(ProcInfo, TCIVars) :-
    proc_info_get_var_table(ProcInfo, VarTable),
    foldl_var_table(maybe_acc_tci_var, VarTable, set_of_var.init, TCIVars).

:- pred maybe_acc_tci_var(prog_var::in, var_table_entry::in,
    set_of_progvar::in, set_of_progvar::out) is det.

maybe_acc_tci_var(Var, VTE, !TCIVars) :-
    VTE = vte(Name, Type, _),
    ( if
        Type = typeclass_info_type,
        % polymorphism_type_class_info.m gives the typeclass_info_type
        % to BOTH typeclass_infos AND base_typeclass_infos. The only
        % way to distinguish them is the prefix.
        string.prefix(Name, "TypeClassInfo_for_")
    then
        set_of_var.insert(Var, !TCIVars)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_polymorphism.
%---------------------------------------------------------------------------%
