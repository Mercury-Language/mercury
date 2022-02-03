%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: headvar_names.m.
% Main author: zs.
%
% This file contains code for improving the names of head variables,
% replacing HeadVar__n with user-given names whereever the clauses
% agree on the names.

:- module hlds.headvar_names.

:- interface.

:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.

    % If all clauses give a given head variables the same name, use this name
    % instead of the introduced `HeadVar__n' names for the head variables
    % in the pred_info. This gives better error messages, more meaningful
    % variable names in the debugger and slightly faster compilation.
    %
:- pred maybe_improve_headvar_names(globals::in, pred_info::in, pred_info::out)
    is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module libs.op_mode.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module varset.

maybe_improve_headvar_names(Globals, !PredInfo) :-
    globals.get_op_mode(Globals, OpMode),
    ( if OpMode = opm_top_args(opma_augment(opmau_make_plain_opt)) then
        % Don't change headvar names when making a `.opt' file, because
        % intermod.m needs to perform a similar transformation which THIS
        % transformation would interfere with. (intermod.m places the
        % original argument terms, not just the argument variables,
        % in the clause head, and this pass would make it difficult to
        % work out what were the original arguments).
        true
    else
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers),
        clauses_info_get_headvars(ClausesInfo0, HeadVars0),
        clauses_info_get_varset(ClausesInfo0, VarSet0),
        get_clause_list_for_replacement(ClausesRep0, Clauses0),
        (
            Clauses0 = []
        ;
            Clauses0 = [SingleClause0],
            Goal0 = SingleClause0 ^ clause_body,

            Goal0 = hlds_goal(_, GoalInfo0),
            goal_to_conj_list(Goal0, Conj0),
            improve_single_clause_headvars(Conj0, HeadVars0, [],
                VarSet0, VarSet, map.init, Subst, [], RevConj),

            NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
            rename_vars_in_set_of_var(need_not_rename, Subst,
                NonLocals0, NonLocals),
            goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
            conj_list_to_goal(list.reverse(RevConj), GoalInfo, Goal),

            apply_renaming_to_proc_arg_vector(Subst, HeadVars0, HeadVars),
            clauses_info_set_headvars(HeadVars, ClausesInfo0, ClausesInfo1),

            SingleClause = SingleClause0 ^ clause_body := Goal,
            set_clause_list([SingleClause], ClausesRep),
            clauses_info_set_clauses_rep(ClausesRep, ItemNumbers,
                ClausesInfo1, ClausesInfo2),
            clauses_info_set_varset(VarSet, ClausesInfo2, ClausesInfo),
            pred_info_set_clauses_info(ClausesInfo, !PredInfo)
        ;
            Clauses0 = [_, _ | _],
            % If a headvar is assigned to a variable with the same name
            % (or no name) in every clause, rename it to have that name.
            list.map2(find_headvar_names_in_clause(VarSet0, HeadVars0),
                Clauses0, VarNameInfoMaps, VarsInMapSets),
            ConsensusMap = find_consensus_headvar_names(VarsInMapSets,
                VarNameInfoMaps),

            % We don't apply the renaming right now, because that could lead to
            % error messages about unifications of the form X = X instead of
            % HeadVar__n = X, which would be confusing.
            %
            % Instead, we record the renaming, and apply it only when we
            % generate the data structures that record variable names for
            % the debugger.
            %
            % We put the renaming into both all the proc_infos of the predicate
            % (which is where stack_layout.m gets them from), and into the
            % pred_info (so that any later procedures and/or predicates created
            % from this one will get the rename map as well).

            pred_info_set_var_name_remap(ConsensusMap, !PredInfo),
            ProcIds = pred_info_all_procids(!.PredInfo),
            pred_info_get_proc_table(!.PredInfo, ProcTable0),
            list.foldl(set_var_name_remap_in_proc(ConsensusMap), ProcIds,
                ProcTable0, ProcTable),
            pred_info_set_proc_table(ProcTable, !PredInfo)
        )
    ).

:- pred set_var_name_remap_in_proc(map(prog_var, string)::in, proc_id::in,
    proc_table::in, proc_table::out) is det.

set_var_name_remap_in_proc(ConsensusMap, ProcId, !ProcTable) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    proc_info_set_var_name_remap(ConsensusMap, ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcTable).

:- pred improve_single_clause_headvars(list(hlds_goal)::in,
    proc_arg_vector(prog_var)::in, list(prog_var)::in,
    prog_varset::in, prog_varset::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

improve_single_clause_headvars([], _, _, !VarSet, !Subst, !RevConj).
improve_single_clause_headvars([Goal | Conj0], HeadVars, SeenVars0,
        !VarSet, !Subst, !RevConj) :-
    ( if
        goal_is_headvar_unification(HeadVars, Goal, HeadVar, yes(OtherVar))
    then
        % If the headvar doesn't appear elsewhere the unification
        % can be removed.
        ( if
            % The headvars must be distinct variables, so check that this
            % variable doesn't already appear in the argument list.
            not proc_arg_vector_member(HeadVars, OtherVar),
            not list.member(OtherVar, SeenVars0),

            not (
                some [OtherGoal] (
                    ( list.member(OtherGoal, Conj0)
                    ; list.member(OtherGoal, !.RevConj)
                    ),
                    OtherGoal = hlds_goal(_, OtherGoalInfo),
                    OtherNonLocals = goal_info_get_nonlocals(OtherGoalInfo),
                    set_of_var.member(OtherNonLocals, HeadVar)
                )
            )
        then
            SeenVars = [OtherVar | SeenVars0],
            map.det_insert(HeadVar, OtherVar, !Subst),

            % If the variable wasn't named, use the `HeadVar__n' name.
            ( if
                not varset.search_name(!.VarSet, OtherVar, _),
                varset.search_name(!.VarSet, HeadVar, HeadVarName)
            then
                varset.name_var(OtherVar, HeadVarName, !VarSet)
            else
                true
            )
        else
            !:RevConj = [Goal | !.RevConj],
            SeenVars = SeenVars0,
            ( if varset.search_name(!.VarSet, OtherVar, OtherVarName) then
                % The unification can't be eliminated,
                % so just rename the head variable.
                varset.name_var(HeadVar, OtherVarName, !VarSet)
            else if varset.search_name(!.VarSet, HeadVar, HeadVarName) then
                % If the variable wasn't named, use the `HeadVar__n' name.
                varset.name_var(OtherVar, HeadVarName, !VarSet)
            else
                true
            )
        )
    else
        !:RevConj = [Goal | !.RevConj],
        SeenVars = SeenVars0
    ),
    improve_single_clause_headvars(Conj0, HeadVars, SeenVars, !VarSet,
        !Subst, !RevConj).

:- type var_name_info_map == map(prog_var, var_name_info).
:- type var_name_info
    --->    var_name_info(
                % Is the head variable unified with a functor?
                vni_unified_with_functor    :: bool,

                % What are the names of the named variables it is unified with?
                vni_unified_with_vars       :: set(string)
            ).

    % Head variables that have the same name in each clause
    % will have an entry of `yes(Name)' in the result map.
    %
:- pred find_headvar_names_in_clause(prog_varset::in,
    proc_arg_vector(prog_var)::in, clause::in,
    var_name_info_map::out, set(prog_var)::out) is det.

find_headvar_names_in_clause(VarSet, HeadVars, Clause, VarNameInfoMap,
        VarsInMap) :-
    Goal = Clause ^ clause_body,
    goal_to_conj_list(Goal, Conj),
    list.foldl2(find_headvar_names_in_goal(VarSet, HeadVars), Conj,
        map.init, VarNameInfoMap, set.init, VarsInMap).

:- pred find_headvar_names_in_goal(prog_varset::in,
    proc_arg_vector(prog_var)::in, hlds_goal::in,
    var_name_info_map::in, var_name_info_map::out,
    set(prog_var)::in, set(prog_var)::out) is det.

find_headvar_names_in_goal(VarSet, HeadVars, Goal, !VarNameInfoMap,
        !VarsInMap) :-
    ( if
        goal_is_headvar_unification(HeadVars, Goal, HeadVar, MaybeOtherVar)
    then
        set.insert(HeadVar, !VarsInMap),
        (
            MaybeOtherVar = no,
            ( if map.search(!.VarNameInfoMap, HeadVar, VarNameInfo0) then
                VarNameInfo0 = var_name_info(_UnifiedFunctor, VarNames),
                VarNameInfo = var_name_info(yes, VarNames),
                map.det_update(HeadVar, VarNameInfo, !VarNameInfoMap)
            else
                VarNameInfo = var_name_info(yes, set.init),
                map.det_insert(HeadVar, VarNameInfo, !VarNameInfoMap)
            )
        ;
            MaybeOtherVar = yes(OtherVar),
            ( if varset.search_name(VarSet, OtherVar, OtherVarName) then
                ( if map.search(!.VarNameInfoMap, HeadVar, VarNameInfo0) then
                    VarNameInfo0 = var_name_info(UnifiedFunctor, VarNames0),
                    set.insert(OtherVarName, VarNames0, VarNames),
                    VarNameInfo = var_name_info(UnifiedFunctor, VarNames),
                    map.det_update(HeadVar, VarNameInfo, !VarNameInfoMap)
                else
                    VarNames = set.make_singleton_set(OtherVarName),
                    VarNameInfo = var_name_info(no, VarNames),
                    map.det_insert(HeadVar, VarNameInfo, !VarNameInfoMap)
                )
            else
                true
            )
        )
    else
        true
    ).

:- pred goal_is_headvar_unification(proc_arg_vector(prog_var)::in,
    hlds_goal::in, prog_var::out, maybe(prog_var)::out) is semidet.

goal_is_headvar_unification(HeadVars, Goal, HeadVar, MaybeOtherVar) :-
    Goal = hlds_goal(GoalExpr, _),
    GoalExpr = unify(LVar, RHS, _, _, _),
    (
        RHS = rhs_var(RVar),
        ( if proc_arg_vector_member(HeadVars, LVar) then
            HeadVar = LVar,
            MaybeOtherVar = yes(RVar)
        else if proc_arg_vector_member(HeadVars, RVar) then
            HeadVar = RVar,
            MaybeOtherVar = yes(LVar)
        else
            fail
        )
    ;
        RHS = rhs_functor(_, _, _),
        ( if proc_arg_vector_member(HeadVars, LVar) then
            HeadVar = LVar,
            MaybeOtherVar = no
        else
            fail
        )
    ).

:- func find_consensus_headvar_names(list(set(prog_var)),
    list(var_name_info_map)) = map(prog_var, string).

find_consensus_headvar_names(VarsInMapSets, VarNameInfoMaps) = ConsensusMap :-
    VarsInMapSet = set.union_list(VarsInMapSets),
    set.to_sorted_list(VarsInMapSet, VarsInMaps),
    list.foldl(update_consensus_map_for_headvar(VarNameInfoMaps), VarsInMaps,
        map.init, ConsensusMap).

:- pred update_consensus_map_for_headvar(list(var_name_info_map)::in,
    prog_var::in,
    map(prog_var, string)::in, map(prog_var, string)::out) is det.

update_consensus_map_for_headvar(VarNameInfos, HeadVar, !ConsensusMap) :-
    MaybeName = find_consensus_name_for_headvar(VarNameInfos, HeadVar),
    (
        MaybeName = no
    ;
        MaybeName = yes(Name),
        map.det_insert(HeadVar, Name, !ConsensusMap)
    ).

:- func find_consensus_name_for_headvar(list(var_name_info_map), prog_var)
    = maybe(string).

find_consensus_name_for_headvar(VarNameInfoMaps, HeadVar)
        = MaybeConsensusName :-
    group_var_infos(VarNameInfoMaps, HeadVar,
        [], Inconsistents, [], Consistents, [], FunctorOnlys),
    (
        Inconsistents = [_ | _],
        % Some clauses give two or more different names to HeadVar.
        % If even a single clause cannot agree what HeadVar's name should be,
        % the procedure as a whole cannot agree either.
        MaybeConsensusName = no
    ;
        Inconsistents = [],
        (
            Consistents = [],
            % There is no name we *can* give.
            MaybeConsensusName = no
        ;
            Consistents = [_ | _],
            (
                FunctorOnlys = [],
                list.sort_and_remove_dups(Consistents, SortedConsistents),
                ( if SortedConsistents = [ConsensusName] then
                    MaybeConsensusName = yes(ConsensusName)
                else
                    MaybeConsensusName = no
                )
            ;
                FunctorOnlys = [_ | _],
                % There is no consensus as to whether the variable *should*
                % have a name. Given a predicate definition like this:
                %
                %   p(A, _) :- ...
                %   p([_ | _], _) :- ...
                %
                % referring to the first head variable in the second clause
                % by the name "A" would be confusing.
                MaybeConsensusName = no
            )
        )
    ).

:- pred group_var_infos(list(var_name_info_map)::in, prog_var::in,
    list(var_name_info)::in, list(var_name_info)::out,
    list(string)::in, list(string)::out,
    list(var_name_info)::in, list(var_name_info)::out) is det.

group_var_infos([], _, !Inconsistents, !Consistents, !FunctorOnlys).
group_var_infos([VarNameInfoMap | VarNameInfoMaps], HeadVar,
        !Inconsistents, !Consistents, !FunctorOnlys) :-
    ( if map.search(VarNameInfoMap, HeadVar, VarInfo) then
        VarInfo = var_name_info(UnifiedFunctor, VarNameSet),
        set.count(VarNameSet, NameCount),
        ( if NameCount = 0 then
            (
                UnifiedFunctor = yes,
                !:FunctorOnlys = [VarInfo | !.FunctorOnlys]
            ;
                UnifiedFunctor = no
                % The variable was unified only with anonymous variables.
            )
        else if NameCount = 1 then
            % If the clause gave the variable a name, we don't care that
            % it also unified the variable with a functor.
            set.to_sorted_list(VarNameSet, VarNameList),
            ( if VarNameList = [VarName] then
                !:Consistents = [VarName | !.Consistents]
            else
                unexpected($pred, "bad singleton set")
            )
        else
            % NameCount > 1, so this *single clause* calls HeadVar
            % by more than one name.
            !:Inconsistents = [VarInfo | !.Inconsistents]
        )
    else
        true
    ),
    group_var_infos(VarNameInfoMaps, HeadVar,
        !Inconsistents, !Consistents, !FunctorOnlys).

%-----------------------------------------------------------------------------%
:- end_module hlds.headvar_names.
%-----------------------------------------------------------------------------%
