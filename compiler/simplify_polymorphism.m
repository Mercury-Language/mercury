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
:- import_module hlds.hlds_rtti.
:- import_module hlds.pred_proc_id.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.var_table.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred check_typeclass_records(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, var_table::in, rtti_varmaps::in,
    list(diag_spec)::in, list(diag_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

check_typeclass_records(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
        VarTable, RttiVarMaps, !Specs) :-
    module_info_get_globals(ModuleInfo, Globals),
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
        compute_duplicate_typeclass_info_vars(VarTable, RttiVarMaps,
            DupTCIVarsToConstraintMap),
        ( if map.is_empty(DupTCIVarsToConstraintMap) then
            true
        else
            map.keys_as_set(DupTCIVarsToConstraintMap, DupTCIVarsSet),
            DupTCIVars = set_of_var.set_to_bitset(DupTCIVarsSet),
            proc_info_get_initial_instmap(ModuleInfo, ProcInfo, InstMap0),
            proc_info_get_goal(ProcInfo, Goal),
            look_for_typeclass_info_conflict_in_goal(DupTCIVars,
                DupTCIVarsToConstraintMap, InstMap0, Goal,
                multi_map.init, _, multi_map.init, ConflictConstraints),
            ( if multi_map.is_empty(ConflictConstraints) then
                true
            else
                report_typeclass_info_problem(ModuleInfo, PredId, ProcId,
                    PredInfo, ProcInfo, ConflictConstraints, !Specs)
            )
        )
    ).

:- type constraint_tci_db == multi_map(prog_constraint, constraint_tci).
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
:- pred look_for_typeclass_info_conflict_in_goal(set_of_progvar::in,
    map(prog_var, prog_constraint)::in, instmap::in, hlds_goal::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out) is det.

look_for_typeclass_info_conflict_in_goal(DupTCIVars, TCIVarToConstraintMap,
        InstMap0, Goal, !TCIConstraints, !ConflictConstraints) :-
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
        look_for_typeclass_info_conflict_in_plain_conj(DupTCIVars,
            TCIVarToConstraintMap, InstMap0, Conjuncts,
            !TCIConstraints, !ConflictConstraints)
    ;
        GoalExpr = disj(Disjuncts),
        look_for_typeclass_info_conflict_in_disj(DupTCIVars,
            TCIVarToConstraintMap, InstMap0, Disjuncts,
            !TCIConstraints, !ConflictConstraints)
    ;
        GoalExpr = switch(_, _, Cases),
        look_for_typeclass_info_conflict_in_switch(DupTCIVars,
            TCIVarToConstraintMap, InstMap0, Cases,
            !TCIConstraints, !ConflictConstraints)
    ;
        GoalExpr = if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal),
        look_for_typeclass_info_conflict_in_goal(DupTCIVars,
            TCIVarToConstraintMap, InstMap0, CondGoal,
            multi_map.init, CondTCIConstraints, !ConflictConstraints),
        apply_goal_instmap_delta(CondGoal, InstMap0, InstMapAfterCond),
        look_for_typeclass_info_conflict_in_goal(DupTCIVars,
            TCIVarToConstraintMap, InstMapAfterCond, ThenGoal,
            CondTCIConstraints, ThenTCIConstraints, !ConflictConstraints),
        look_for_typeclass_info_conflict_in_goal(DupTCIVars,
            TCIVarToConstraintMap, InstMap0, ElseGoal,
            multi_map.init, ElseTCIConstraints, !ConflictConstraints),
        detect_conflicts_in_arms([ThenTCIConstraints, ElseTCIConstraints],
            !TCIConstraints, !ConflictConstraints)
    ;
        GoalExpr = negation(SubGoal),
        look_for_typeclass_info_conflict_in_goal(DupTCIVars,
            TCIVarToConstraintMap, InstMap0, SubGoal,
            !TCIConstraints, !ConflictConstraints)
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
            look_for_typeclass_info_conflict_in_goal(DupTCIVars,
                TCIVarToConstraintMap, InstMap0, SubGoal,
                !TCIConstraints, !ConflictConstraints)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, _Outer, _Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            Disjuncts = [MainGoal | OrElseGoals],
            look_for_typeclass_info_conflict_in_disj(DupTCIVars,
                TCIVarToConstraintMap, InstMap0, Disjuncts,
                !TCIConstraints, !ConflictConstraints)
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
    multi_map.add(Constraint, ConstraintTCI, !TCIConstraints).

%---------------------%

:- pred look_for_typeclass_info_conflict_in_plain_conj(set_of_progvar::in,
    map(prog_var, prog_constraint)::in, instmap::in, list(hlds_goal)::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out) is det.

look_for_typeclass_info_conflict_in_plain_conj(_, _, _, [],
        !TCIConstraints, !ConflictConstraints).
look_for_typeclass_info_conflict_in_plain_conj(DupTCIVars,
        TCIVarToConstraintMap, InstMap0, [Conjunct | Conjuncts],
        !TCIConstraints, !ConflictConstraints) :-
    look_for_typeclass_info_conflict_in_goal(DupTCIVars, TCIVarToConstraintMap,
        InstMap0, Conjunct, !TCIConstraints, !ConflictConstraints),
    apply_goal_instmap_delta(Conjunct, InstMap0, InstMap1),
    look_for_typeclass_info_conflict_in_plain_conj(DupTCIVars,
        TCIVarToConstraintMap, InstMap1, Conjuncts,
        !TCIConstraints, !ConflictConstraints).

%---------------------%

:- pred look_for_typeclass_info_conflict_in_disj(set_of_progvar::in,
    map(prog_var, prog_constraint)::in, instmap::in, list(hlds_goal)::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out) is det.

look_for_typeclass_info_conflict_in_disj(DupTCIVars, TCIVarToConstraintMap,
        InstMap0, Disjuncts, !TCIConstraints, !ConflictConstraints) :-
    look_for_typeclass_info_conflict_in_disjuncts(DupTCIVars,
        TCIVarToConstraintMap, InstMap0, Disjuncts, ArmTCIConstraints,
        !ConflictConstraints),
    detect_conflicts_in_arms(ArmTCIConstraints,
        !TCIConstraints, !ConflictConstraints).

:- pred look_for_typeclass_info_conflict_in_disjuncts(set_of_progvar::in,
    map(prog_var, prog_constraint)::in, instmap::in, list(hlds_goal)::in,
    list(constraint_tci_db)::out,
    conflict_constraints::in, conflict_constraints::out) is det.

look_for_typeclass_info_conflict_in_disjuncts(_, _, _, [], [],
        !ConflictConstraints).
look_for_typeclass_info_conflict_in_disjuncts(DupTCIVars,
        TCIVarToConstraintMap, InstMap0, [HeadDisjunct | TailDisjuncts],
        [HeadTCIConstraints | TailTCIConstraints], !ConflictConstraints) :-
    look_for_typeclass_info_conflict_in_goal(DupTCIVars, TCIVarToConstraintMap,
        InstMap0, HeadDisjunct, multi_map.init, HeadTCIConstraints,
        !ConflictConstraints),
    look_for_typeclass_info_conflict_in_disjuncts(DupTCIVars,
        TCIVarToConstraintMap, InstMap0,
        TailDisjuncts, TailTCIConstraints, !ConflictConstraints).

%---------------------%

:- pred look_for_typeclass_info_conflict_in_switch(set_of_progvar::in,
    map(prog_var, prog_constraint)::in, instmap::in, list(case)::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out) is det.

look_for_typeclass_info_conflict_in_switch(DupTCIVars, TCIVarToConstraintMap,
        InstMap0, Disjuncts, !TCIConstraints, !ConflictConstraints) :-
    look_for_typeclass_info_conflict_in_cases(DupTCIVars,
        TCIVarToConstraintMap, InstMap0, Disjuncts, ArmTCIConstraints,
        !ConflictConstraints),
    detect_conflicts_in_arms(ArmTCIConstraints,
        !TCIConstraints, !ConflictConstraints).

:- pred look_for_typeclass_info_conflict_in_cases(set_of_progvar::in,
    map(prog_var, prog_constraint)::in, instmap::in, list(case)::in,
    list(constraint_tci_db)::out,
    conflict_constraints::in, conflict_constraints::out) is det.

look_for_typeclass_info_conflict_in_cases(_, _, _, [], [],
        !ConflictConstraints).
look_for_typeclass_info_conflict_in_cases(DupTCIVars, TCIVarToConstraintMap,
        InstMap0, [HeadCase | TailCases],
        [HeadTCIConstraints | TailTCIConstraints], !ConflictConstraints) :-
    HeadCase = case(_MainConsId, _OtherConsIds, HeadGoal),
    % We ignore the instmap changes resulting from binding the switched-on
    % variable to MainConsId or OtherConsIds, because they cannot affect
    % the compiler-generated code that binds typeclass infos.
    look_for_typeclass_info_conflict_in_goal(DupTCIVars, TCIVarToConstraintMap,
        InstMap0, HeadGoal, multi_map.init, HeadTCIConstraints,
        !ConflictConstraints),
    look_for_typeclass_info_conflict_in_cases(DupTCIVars,
        TCIVarToConstraintMap, InstMap0,
        TailCases, TailTCIConstraints, !ConflictConstraints).

%---------------------%

:- pred detect_conflicts_in_arms(list(constraint_tci_db)::in,
    constraint_tci_db::in, constraint_tci_db::out,
    conflict_constraints::in, conflict_constraints::out) is det.

detect_conflicts_in_arms(ArmTCIConstraints,
        !AllArmTCIConstraints, !ConflictConstraints) :-
    detect_conflict_constraints_in_arms(ArmTCIConstraints,
        set.init, set.init, DupConstraints, !AllArmTCIConstraints),
    collect_conflict_constraints_in_arms(DupConstraints,
        ArmTCIConstraints, multi_map.init, NewConflictConstraints),
    multi_map.merge(NewConflictConstraints, !ConflictConstraints).

:- pred detect_conflict_constraints_in_arms(list(constraint_tci_db)::in,
    set(prog_constraint)::in,
    set(prog_constraint)::in, set(prog_constraint)::out,
    constraint_tci_db::in, constraint_tci_db::out) is det.

detect_conflict_constraints_in_arms([], _,
        !DupConstraints, !AllArmTCIConstraints).
detect_conflict_constraints_in_arms(
        [HeadArmTCIConstraints | TailArmTCIConstraints],
        !.SeenConstraints, !DupConstraints, !AllArmTCIConstraints) :-
    multi_map.keys_as_set(HeadArmTCIConstraints, HeadConstraints),
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
    multi_map.merge(HeadArmTCIConstraints, !AllArmTCIConstraints),
    detect_conflict_constraints_in_arms(TailArmTCIConstraints,
        !.SeenConstraints, !DupConstraints, !AllArmTCIConstraints).

:- pred collect_conflict_constraints_in_arms(set(prog_constraint)::in,
    list(constraint_tci_db)::in,
    conflict_constraints::in, conflict_constraints::out) is det.

collect_conflict_constraints_in_arms(_, [], !ConflictConstraints).
collect_conflict_constraints_in_arms(DupConstraints,
        [HeadArmTCIConstraints | TailArmTCIConstraints],
        !ConflictConstraints) :-
    multi_map.select(HeadArmTCIConstraints, DupConstraints,
        HeadDupTCIConstraints),
    multi_map.merge(HeadDupTCIConstraints, !ConflictConstraints),
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
    multi_map.to_sorted_assoc_list(ConflictConstraintSet, ConflictConstraints),
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
    pair(prog_constraint, list(constraint_tci))::in,
    list(format_piece)::out) is det.

conflict_constraint_to_piece(TVarSet, Constraint - ConstraintTCIs, Pieces) :-
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
:- end_module check_hlds.simplify.simplify_polymorphism.
%---------------------------------------------------------------------------%
