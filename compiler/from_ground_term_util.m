%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: from_ground_term_util.m.
% Author: zs.
%
% This module provides utility types and predicates that are useful
% when compiler modules process from_ground_term scopes.
%
% Specifically, the contents of this module are designed
% to help test whether after a program transformation,
% a from_ground_term_{initial,construct} scope goal can still remain
% a from_ground_term scope of the same kind, and if not, to wrap smaller
% from_ground_term_{initial,construct} scopes around subsequences of the
% original conjuncts whereever the status of the invariants permit it.
%

:- module hlds.from_ground_term_util.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

    % Describe whether a goal or sequence of goals inside a from_ground_term
    % scope of the initial or construct kind still obeys the invariants
    % required of such scopes after a program transformation.
    %
:- type fgt_invariants_status
    --->    fgt_invariants_kept
    ;       fgt_invariants_broken.

    % A conjunct inside a from_ground_term_{initial,construct} scope must be
    % a unification of the form X = f(Y1, ..., Yn). The X is constructed
    % by this unification and by the goals that construct the Yi.
    %
    % Values of this type record both the relevant information about the input
    % of a program transformation that operates on such conjuncts (the second
    % and third fields of each functor), the result of the transformation
    % (the first field of each functor) and whether the transformation
    % breaks the scope's invariants (the functor itself).
    %
:- type fgt_marked_goal
    --->    fgt_kept_goal(
                % Either the original goal, or a version that is transformed
                % in a way that keeps the from_ground_term_{initial,construct}
                % invariants.
                hlds_goal,

                % The variable from the lhs of the original unification (X).
                prog_var,

                % The variables from the rhs of the original unification (Yi).
                list(prog_var)
            )
    ;       fgt_broken_goal(
                % A version of the original goal that is transformed
                % in a way that breaks some of the
                % from_ground_term_{initial,construct} invariants.
                hlds_goal,

                % The variable from the lhs of the original unification (X).
                prog_var,

                % The variables from the rhs of the original unification (Yi).
                list(prog_var)
            ).

    % Return the goal from a goal marked as kept. Abort if the goal is marked
    % as broken.
    %
:- pred project_kept_goal(fgt_marked_goal::in, hlds_goal::out) is det.

    % A value of type fgt_build_info maps a variable to the code needed
    % to build it, and to some other information that is of interest
    % only to this module.
    %
:- type fgt_build_info.
:- type fgt_build_info_map == map(prog_var, fgt_build_info).

:- type goal_order
    --->    construct_bottom_up
    ;       deconstruct_top_down.

    % introduce_partial_fgt_scopes(GoalInfo0, SubGoalInfo0,
    %   ConstructOrderMarkedSubGoals, Order, SubGoal):
    %
    % The inputs to this predicate are:
    %
    % - The original goal infos for a fgt{i,c} goal and for its subgoal
    %   (GoalInfo0 and SubGoalInfo0).
    %
    % - A list (ConstructOrderMarkedSubGoals) of the transformed versions
    %   of the conjuncts that were originally in that scope, in bottom up
    %   (construct) order, marked up in the way required by the comment
    %   on the definition of the fgt_marked_goal type). This predicate
    %   assumes that this list contains some violations of the fgt{i,c}
    %   invariants (other than the order invariant for initial scopes);
    %   if it doesn't, then the WHOLE conjunction can have a fgt{i,c} scope
    %   wrapped around it, using code much simpler than what is in this
    %   predicate.
    %
    % - The order into which the goals should be put (Order).
    %
    % The SubGoal returned by this predicate will contain the code in the
    % MarkedSubGoals in the desired order, but with any sequence of goals that
    % does obey the fgt{i,c} invariants will be wrapped in a fgt scope;
    % initial if Order = deconstruct_top_down, construct if Order =
    % construct_bottom_up.
    %
    % This allows us to preserve the effect of the compiler optimizations of
    % fgt{i,c} scopes for as large a portion of the original scope as possible.
    %
:- pred introduce_partial_fgt_scopes(hlds_goal_info::in, hlds_goal_info::in,
    list(fgt_marked_goal)::in, goal_order::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.set_of_var.

:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%

project_kept_goal(MarkedGoal, Goal) :-
    (
        MarkedGoal = fgt_kept_goal(Goal, _, _)
    ;
        MarkedGoal = fgt_broken_goal(_, _, _),
        unexpected($module, $pred, "broken goal")
    ).

%---------------------------------------------------------------------------%

    % Has a goal or sequence of goals broken the fgt{i,c} invariants?
    %
:- type maybe_kept
    --->    kept
    ;       broken.

    % Has a goal kept the fgt{i,c} invariants?
    %
:- type maybe_kept_goal_info
    --->    kept_old_gi(
                % Yes. But if its parent or one of its siblings has not,
                % then we want to wrap it in an fgt scope if it is big
                % enough (which is why we record the size of the goal,
                % as a count of the unifications in it). The wrapping process
                % needs the goal info of the unification that puts
                % the top function symbol of the ground term in place.
                mkgi_size           :: int,
                mkgi_old_goal_info  :: hlds_goal_info
            )
    ;       broken_no_gi.
            % No, the goal has not kept the fgt{i,c} invariants.

    % The code needed to build a variable, and whether it breaks the fgt{i,c}
    % invariants.
    %
:- type fgt_build_info
    --->    fgt_build_info(
                maybe_kept_goal_info,
                cord(hlds_goal)
            ).

%---------------------------------------------------------------------------%

introduce_partial_fgt_scopes(GoalInfo0, SubGoalInfo0, RevMarkedSubGoals,
        Order, SubGoal) :-
    introduce_partial_fgt_scopes_loop(RevMarkedSubGoals, Order,
        map.init, BuildInfoMap),
    map.values(BuildInfoMap, BuildInfos),
    list.map(project_build_info_goal_cord, BuildInfos, BuildGoalCords),
    BuildGoalCord = cord_list_to_cord(BuildGoalCords),
    BuildGoals = cord.list(BuildGoalCord),
    (
        BuildGoals = [],
        unexpected($module, $pred, "BuildGoals = []")
    ;
        BuildGoals = [SubGoal1]
    ;
        BuildGoals = [_, _ | _],
        SubGoalExpr1 = conj(plain_conj, BuildGoals),
        SubGoal1 = hlds_goal(SubGoalExpr1, SubGoalInfo0)
    ),
    ( if goal_info_has_feature(GoalInfo0, feature_from_head) then
        attach_features_to_all_goals([feature_from_head],
            attach_in_from_ground_term, SubGoal1, SubGoal)
    else
        SubGoal = SubGoal1
    ).

    % The main input to this predicate is a list of the goals in an fgt{i,c}
    % scope, as transformed and marked up by a compiler pass.
    % All the original goals were of the form X = f(Y1, ..., Yn), and
    % the list is in construct order, so that the code constructing the Yi
    % precedes the code constructing X. Each of the Yi will have appeared
    % exactly twice in the original goal sequence: once when constructed,
    % and once when used.
    %
    % Process this sequence by creating an entry for a variable in
    % !BuildInfoMap when it is constructed, and removing it when it is used.
    % At the end, the final BuildInfoMap should contain exactly one entry,
    % for the variable that the fgt scope is for.
    %
    % Each entry gives the code sequence required to build the variable
    % and all its components, and says whether that code obeys the required
    % fgt{i,c} invariants. In process of building the code for an X that
    % breaks these invariants, we wrap fgt{i,c} scopes around the code
    % subsequences that construct any of the Yi that keep those invariants.
    %
:- pred introduce_partial_fgt_scopes_loop(list(fgt_marked_goal)::in,
    goal_order::in, fgt_build_info_map::in, fgt_build_info_map::out) is det.

introduce_partial_fgt_scopes_loop([], _Order, !BuildInfoMap).
introduce_partial_fgt_scopes_loop([RevMarkedGoal | RevMarkedGoals], Order,
        !BuildInfoMap) :-
    (
        RevMarkedGoal = fgt_kept_goal(Goal, Var, ArgVars),
        SavedBuildInfoMap = !.BuildInfoMap,
        lookup_and_remove_arg_vars(ArgVars, cord.init, ArgsGoalCord0,
            kept, Kept, 0, TotalArgSize, !BuildInfoMap),
        (
            Kept = kept,
            ArgsGoalCord = ArgsGoalCord0,
            Goal = hlds_goal(_, OldGoalInfo),
            TotalSize = TotalArgSize + 1,
            KeptGI = kept_old_gi(TotalSize, OldGoalInfo)
        ;
            Kept = broken,
            !:BuildInfoMap = SavedBuildInfoMap,
            lookup_and_remove_arg_vars_insert_fgt(ArgVars, Order,
                cord.init, ArgsGoalCord, !BuildInfoMap),
            KeptGI = broken_no_gi
        )
    ;
        RevMarkedGoal = fgt_broken_goal(Goal, Var, ArgVars),
        lookup_and_remove_arg_vars_insert_fgt(ArgVars, Order,
            cord.init, ArgsGoalCord, !BuildInfoMap),
        KeptGI = broken_no_gi
    ),
    (
        Order = construct_bottom_up,
        GoalCord = cord.snoc(ArgsGoalCord, Goal)
    ;
        Order = deconstruct_top_down,
        GoalCord = cord.cons(Goal, ArgsGoalCord)
    ),
    VarBuildInfo = fgt_build_info(KeptGI, GoalCord),
    map.det_insert(Var, VarBuildInfo, !BuildInfoMap),
    introduce_partial_fgt_scopes_loop(RevMarkedGoals, Order, !BuildInfoMap).

    % Loop over all the Yi in a unification of the form X = f(Y1, ..., Yn),
    % and report the goal cord needed to build all the Yi (!:GoalCord),
    % whether all parts of that code keep the fgt{i,c} invariants (!:Kept).
    % In the process, remove all the Yi from !:BuildInfoMap, since they
    % should never appear in the reverse goal sequence again.
    %
:- pred lookup_and_remove_arg_vars(list(prog_var)::in,
    cord(hlds_goal)::in, cord(hlds_goal)::out,
    maybe_kept::in, maybe_kept::out, int::in, int::out,
    fgt_build_info_map::in, fgt_build_info_map::out) is det.

lookup_and_remove_arg_vars([], !GoalCord, !Kept, !TotalSize, !BuildInfoMap).
lookup_and_remove_arg_vars([Var | Vars], !GoalCord, !Kept, !TotalSize,
        !BuildInfoMap) :-
    map.det_remove(Var, BuildInfo, !BuildInfoMap),
    BuildInfo = fgt_build_info(VarKept, VarGoalCord),
    (
        VarKept = kept_old_gi(Size, _),
        !:TotalSize = !.TotalSize + Size
    ;
        VarKept = broken_no_gi,
        % The final TotalSize won't be consulted, so it is OK not to
        % update it here.
        !:Kept = broken
    ),
    !:GoalCord = !.GoalCord ++ VarGoalCord,
    lookup_and_remove_arg_vars(Vars, !GoalCord, !Kept, !TotalSize,
        !BuildInfoMap).

    % Loop over all the Yi in a unification of the form X = f(Y1, ..., Yn).
    % We know that either the code that builds the top functor of X or
    % the code that builds one or more of the Yi breaks the fgt{i,c}
    % invariants, so wrap the codes that build the OTHER Yi in fgt{i,c}
    % scopes. Return the code needed to build all Yi (in !:GoalCord).
    % In the process, remove all the Yi from !:BuildInfoMap, since they
    % should never appear in the reverse goal sequence again.
    %
:- pred lookup_and_remove_arg_vars_insert_fgt(list(prog_var)::in,
    goal_order::in, cord(hlds_goal)::in, cord(hlds_goal)::out,
    fgt_build_info_map::in, fgt_build_info_map::out) is det.

lookup_and_remove_arg_vars_insert_fgt([], _Order, !GoalCord, !BuildInfoMap).
lookup_and_remove_arg_vars_insert_fgt([Var | Vars], Order, !GoalCord,
        !BuildInfoMap) :-
    map.det_remove(Var, BuildInfo, !BuildInfoMap),
    BuildInfo = fgt_build_info(VarKept, VarGoalCord0),
    (
        VarKept = kept_old_gi(Size0, GoalInfo0),
        ( if cord.is_empty(VarGoalCord0) then
            unexpected($module, $pred, "VarGoalCord0 is empty")
        else
            MaybeThreshold = get_maybe_from_ground_term_threshold,
            ( if
                MaybeThreshold = yes(Threshold),
                Size0 >= Threshold
            then
                goal_info_set_nonlocals(set_of_var.make_singleton(Var),
                    GoalInfo0, GoalInfo),
                VarGoals0 = cord.list(VarGoalCord0),
                ConjGoalExpr = conj(plain_conj, VarGoals0),
                ConjGoal = hlds_goal(ConjGoalExpr, GoalInfo),
                (
                    Order = deconstruct_top_down,
                    Kind = from_ground_term_initial
                ;
                    Order = construct_bottom_up,
                    Kind = from_ground_term_construct
                ),
                Reason = from_ground_term(Var, Kind),
                ScopeGoalExpr = scope(Reason, ConjGoal),
                ScopeGoal = hlds_goal(ScopeGoalExpr, GoalInfo),
                VarGoalCord = cord.singleton(ScopeGoal)
            else
                VarGoalCord = VarGoalCord0
            )
        )
    ;
        VarKept = broken_no_gi,
        VarGoalCord = VarGoalCord0
    ),
    !:GoalCord = !.GoalCord ++ VarGoalCord,
    lookup_and_remove_arg_vars_insert_fgt(Vars, Order, !GoalCord,
        !BuildInfoMap).

%---------------------------------------------------------------------------%

    % Return the hlds goal cord part of a fgt_build_info.
    %
:- pred project_build_info_goal_cord(fgt_build_info::in, cord(hlds_goal)::out)
    is det.

project_build_info_goal_cord(fgt_build_info(_, GoalCord), GoalCord).

%---------------------------------------------------------------------------%
:- end_module hlds.from_ground_term_util.
%---------------------------------------------------------------------------%
