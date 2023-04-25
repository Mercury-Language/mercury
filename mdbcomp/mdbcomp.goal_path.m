%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: goal_path.m
% Authors: zs, pbone
%
% This module defines the representation of goal paths and goal ids.
% These identify locations within a procedure body.
%
% We can think of the goal that defines a procedure to be a tree, whose leaves
% are primitive goals and whose interior nodes are compound goals. The goal_id,
% forward_goal_path and reverse_goal_path types describe the position of a goal
% in this tree. Therefore values of these three types can uniquely identify
% a goal within its defining procedure.
%
% Goal ids are allocated in a depth-first manner that guarantees the following
% invariants:
%
% - the goal id of a goal representing the procedure body will be 0, and
% - the goal id of a goal will be greater than the goal ids of all the goals
%   that contain it.
%
% A goal_path_step type says which branch to take at an interior node;
% the integer counts inside steps start at one. For switches, the second int,
% if present, gives the total number of function symbols in the type of the
% switched-on var. For builtin types such as integer and string, for which
% this number is effectively infinite, the second number won't be present.
%
% A forward goal path lists the steps from the root of the tree to the goal
% being identified.
%
% A reverse goal path lists the steps from to the goal being identified to
% the root of the tree.
%
% The code in the compiler that allocates goal ids also returns a containing
% goal map, which maps each goal id to the id of its innermost containing goal
% (if there is one). When possible, new code should use this data structure,
% though code that needs to identify goals in files outside the compiler
% will probably continue to need to use goal paths. The string representations
% of goal paths always list the steps in the forward order. In contrast,
% most operations inside the compiler use reverse goal paths, because most
% operations on goal paths focus on the last element, not the first.
%
%---------------------------------------------------------------------------%

:- module mdbcomp.goal_path.
:- interface.

:- import_module array.
:- import_module bimap.
:- import_module char.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type forward_goal_path
    --->    fgp_nil
    ;       fgp_cons(goal_path_step, forward_goal_path).

:- type reverse_goal_path
    --->    rgp_nil
    ;       rgp_cons(reverse_goal_path, goal_path_step).

:- type goal_path_string == string.

:- type goal_path_step
    --->    step_conj(int)
    ;       step_disj(int)
    ;       step_switch(int, maybe_switch_num_functors)
    ;       step_ite_cond
    ;       step_ite_then
    ;       step_ite_else
    ;       step_neg
    ;       step_scope(maybe_cut)
    ;       step_lambda
    ;       step_try
    ;       step_atomic_main
    ;       step_atomic_orelse(int).

    % The number of functors in the type of the switched-on variable, if known.
    %
:- type maybe_switch_num_functors
    --->    unknown_num_functors_in_type
    ;       known_num_functors_in_type(int).

    % Does the scope goal have a different determinism inside than outside?
:- type maybe_cut
    --->    scope_is_cut
    ;       scope_is_no_cut.

%---------------------%

    % Append a goal path step onto the end of a goal path.
    %
:- func goal_path_add_at_end(forward_goal_path, goal_path_step) =
    forward_goal_path.

    % Append a goal path step onto the end of a reverse goal path.
    %
:- func rev_goal_path_add_at_end(reverse_goal_path, goal_path_step) =
    reverse_goal_path.

    % Remove the last item from the goal path, returning it and the new
    % goal path.
    %
:- pred goal_path_remove_last(forward_goal_path::in, forward_goal_path::out,
    goal_path_step::out) is semidet.

    % Get the last item from the goal path. This fails if the goal path is
    % empty.
    %
:- pred goal_path_get_last(forward_goal_path::in, goal_path_step::out)
    is semidet.

    % Remove the first item from the goal path, returning it and the new
    % goal path.
    %
:- pred goal_path_remove_first(forward_goal_path::in, forward_goal_path::out,
    goal_path_step::out) is semidet.

    % Get the first item from the goal path. This fails if the goal path is
    % empty.
    %
:- pred goal_path_get_first(forward_goal_path::in, goal_path_step::out)
    is semidet.

    % Remove the last item from the goal path, returning it and the new
    % goal path.
    %
:- pred rev_goal_path_remove_last(reverse_goal_path::in,
    reverse_goal_path::out, goal_path_step::out) is semidet.

    % Get the last item from the goal path. This fails if the goal path is
    % empty.
    %
:- pred rev_goal_path_get_last(reverse_goal_path::in, goal_path_step::out)
    is semidet.

%---------------------%

    % goal_path_inside(PathA, PathB):
    %
    % Succeed if PathB denotes a goal *inside* the goal denoted by PathA.
    % (It considers a goal to be inside itself.)
    %
:- pred goal_path_inside(forward_goal_path::in, forward_goal_path::in)
    is semidet.
:- pred rev_goal_path_inside(reverse_goal_path::in, reverse_goal_path::in)
    is semidet.

    % goal_path_inside_relative(PathA, PathB, RelativePath):
    %
    % As goal_path_inside, except that it also returns RelativePath, which
    % denotes the same goal that PathB denotes, only from GoalA's perspective.
    %
:- pred goal_path_inside_relative(forward_goal_path::in,
    forward_goal_path::in, forward_goal_path::out) is semidet.
:- pred rev_goal_path_inside_relative(reverse_goal_path::in,
    reverse_goal_path::in, reverse_goal_path::out) is semidet.

%---------------------%

    % Convert one kind of goal path into the other.
    %
:- pred rgp_to_fgp(reverse_goal_path::in, forward_goal_path::out) is det.
:- pred fgp_to_rgp(forward_goal_path::in, reverse_goal_path::out) is det.

%---------------------%

    % Remove information from the goal path that depends on type information.
    %
    % This is necessary when using goal paths to lookup a map within the deep
    % profiler. The goal paths used to perform the query cannot construct the
    % parts of the goal paths that depend on type information.
    %
:- pred rev_goal_path_remove_type_info(reverse_goal_path::in,
    reverse_goal_path::out) is det.

%---------------------%

    % Converts a string to a forward goal path, failing if the string
    % is not a valid goal path.
    %
:- pred goal_path_from_string(string::in, forward_goal_path::out) is semidet.

    % Converts a string to a forward goal path, aborting if the string
    % is not a valid goal path.
    %
:- pred goal_path_from_string_det(string::in, forward_goal_path::out) is det.

    % Converts a string to a reverse goal path, failing if the string
    % is not a valid goal path.
    %
:- pred rev_goal_path_from_string(string::in, reverse_goal_path::out)
    is semidet.

    % Converts a string to a reverse goal path, aborting if the string
    % is not a valid goal path.
    %
:- pred rev_goal_path_from_string_det(string::in, reverse_goal_path::out)
    is det.

    % Converts a string to a goal path step, failing if the string is not
    % a valid goal path step.
    %
:- pred goal_path_step_from_string(string::in, goal_path_step::out) is semidet.

    % Is this character the one that ends each goal path step?
    %
:- pred is_goal_path_separator(char::in) is semidet.

%---------------------%

    % Convert the goal path to its string representation. The resulting string
    % is guaranteed to be acceptable to path_from_string_det.
    %
:- func goal_path_to_string(forward_goal_path) = string.

    % Convert the goal path to its string representation. The resulting string
    % is guaranteed to be acceptable to rev_path_from_string_det.
    %
:- func rev_goal_path_to_string(reverse_goal_path) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type goal_id
    --->    goal_id(int).

    % Return the goal_id that identifies the whole of a procedure's body.
    %
:- func whole_body_goal_id = goal_id.

    % Succeed iff the given goal id is valid.
    %
:- pred is_valid_goal_id(goal_id::in) is semidet.

    % Return a goal id on which the typeclass checking code can hang
    % the constraints that need to be proven for clause heads.
    %
    % This goal id will be *distinct* from all goal_ids that can be hung
    % on goals in the bodies of clauses, i.e. it will NOT be a valid goal id.
    %
:- func goal_id_for_head_constraints = goal_id.

%---------------------------------------------------------------------------%

:- type containing_goal
    --->    whole_body_goal
            % This goal is the entire body of its procedure;
            % there is no larger goal containing it.
    ;       containing_goal(goal_id, goal_path_step).
            % This goal is contained immediately inside the larger goal
            % identified by the goal_id, from which you need to take the
            % given goal_path_step to get to this goal.
            %
            % The goal_id of the containing goal is guaranteed to be
            % less than the goal_id of this goal.

:- type containing_goal_map == map(goal_id, containing_goal).
:- type goal_forward_path_map == map(goal_id, forward_goal_path).
:- type goal_reverse_path_map == map(goal_id, reverse_goal_path).
:- type goal_reverse_path_bimap == bimap(goal_id, reverse_goal_path).

    % Convert a goal_id to a forward goal path.
    %
:- func goal_id_to_forward_path(containing_goal_map, goal_id) =
    forward_goal_path.

    % Convert a goal_id to a reverse goal path.
    %
:- func goal_id_to_reverse_path(containing_goal_map, goal_id) =
    reverse_goal_path.

    % Given a containing_goal_map, create a map that maps each goal_id in it
    % to a forward goal path.
    %
:- func create_forward_goal_path_map(containing_goal_map) =
    goal_forward_path_map.

    % Given a containing_goal_map, create a map that maps each goal_id in it
    % to a reverse goal path.
    %
:- func create_reverse_goal_path_map(containing_goal_map) =
    goal_reverse_path_map.

    % Given a containing_goal_map, create a map that maps each goal_id in it
    % to a reverse goal path, and back.
    %
:- func create_reverse_goal_path_bimap(containing_goal_map) =
    goal_reverse_path_bimap.

    % goal_id_inside(ContainingGoalMap, GoalIdA, GoalIdB):
    %
    % Succeeds if GoalIdB denotes a goal *inside* the goal denoted by GoalIdA.
    % (It considers a goal to be inside itself.)
    %
:- pred goal_id_inside(containing_goal_map::in,
    goal_id::in, goal_id::in) is semidet.

%---------------------------------------------------------------------------%

:- type goal_attr_array(T)
    --->    goal_attr_array(array(maybe(T))).

    % This isn't really unique. See the comments on the `uniq_array' type
    % in library/array.m.
    %
:- inst uniq_goal_attr_array for goal_attr_array/1
    --->    goal_attr_array(uniq_array).

:- mode gaa_di == di(uniq_goal_attr_array).
:- mode gaa_uo == out(uniq_goal_attr_array).

    % create_goal_id_array(LastGoalId) = Array.
    %
    % Create an array of the correct size to label all the goals
    % up to and including LastGoalId.
    %
:- func create_goal_id_array(goal_id) = goal_attr_array(T).
:- mode create_goal_id_array(in) = gaa_uo is det.

    % create_goal_id_array(LastGoalId, Default) = Array.
    %
    % As above, except a default value is provided for array elements.
    %
:- func create_goal_id_array(goal_id, T) = goal_attr_array(T).
:- mode create_goal_id_array(in, in) = gaa_uo is det.

    % update_goal_attribute(GoalId, Attribute, !Array),
    %
    % Make Attribute the new attribute for GoalId in !:Array.
    %
:- pred update_goal_attribute(goal_id::in, T::in,
    goal_attr_array(T)::gaa_di, goal_attr_array(T)::gaa_uo) is det.

    % get_goal_attribute(Array, GoalId) = Attribute.
    %
    % Get a goal attribute.
    %
:- func get_goal_attribute_det(goal_attr_array(T), goal_id) = T.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

goal_path_add_at_end(fgp_nil, NewStep) = fgp_cons(NewStep, fgp_nil).
goal_path_add_at_end(fgp_cons(OldStep, GoalPath0), NewStep) =
        fgp_cons(OldStep, GoalPath) :-
    GoalPath = goal_path_add_at_end(GoalPath0, NewStep).

rev_goal_path_add_at_end(GoalPath0, NewStep) = GoalPath :-
    GoalPath = rgp_cons(GoalPath0, NewStep).

goal_path_remove_last(fgp_cons(HeadStep, TailSteps),
        AllButLastGoalPath, LastStep) :-
    goal_path_remove_last_loop(HeadStep, TailSteps,
        AllButLastGoalPath, LastStep).

:- pred goal_path_remove_last_loop(goal_path_step::in, forward_goal_path::in,
    forward_goal_path::out, goal_path_step::out) is det.

goal_path_remove_last_loop(Head, fgp_nil, fgp_nil, Head).
goal_path_remove_last_loop(Head, fgp_cons(TailHead, TailTail),
        AllButLastGoalPath, LastStep) :-
    goal_path_remove_last_loop(TailHead, TailTail,
        AllButLastGoalPath0, LastStep),
    AllButLastGoalPath = fgp_cons(Head, AllButLastGoalPath0).

goal_path_get_last(fgp_cons(HeadStep, TailSteps), LastStep) :-
    goal_path_last_loop(HeadStep, TailSteps, LastStep).

:- pred goal_path_last_loop(goal_path_step::in, forward_goal_path::in,
    goal_path_step::out) is det.

goal_path_last_loop(Head, fgp_nil, Head).
goal_path_last_loop(_Head, fgp_cons(TailHead, TailTail), LastStep) :-
    goal_path_last_loop(TailHead, TailTail, LastStep).

goal_path_remove_first(fgp_cons(FirstStep, OtherSteps), OtherSteps,
    FirstStep).

goal_path_get_first(GoalPath, FirstStep) :-
    goal_path_remove_first(GoalPath, _, FirstStep).

rev_goal_path_remove_last(rgp_cons(GoalPath, LastStep), GoalPath, LastStep).

rev_goal_path_get_last(rgp_cons(_, LastStep), LastStep).

%---------------------------------------------------------------------------%

goal_path_inside(PathA, PathB) :-
    goal_path_inside_relative(PathA, PathB, _).

rev_goal_path_inside(RevPathA, RevPathB) :-
    rev_goal_path_inside_relative(RevPathA, RevPathB, _).

goal_path_inside_relative(fgp_nil, PathB, PathB).
goal_path_inside_relative(fgp_cons(StepA, PathA), fgp_cons(StepB, PathB),
        RelativePath) :-
    StepA = StepB,
    goal_path_inside_relative(PathA, PathB, RelativePath).

rev_goal_path_inside_relative(RevPathA, RevPathB, RevRelativePath) :-
    % XXX It would be more efficient if we could do this test
    % without having to translate twice the part of RevPathB that
    % goal_path_inside_relative returns but does not test.
    rgp_to_fgp(RevPathA, PathA),
    rgp_to_fgp(RevPathB, PathB),
    goal_path_inside_relative(PathA, PathB, RelativePath),
    fgp_to_rgp(RelativePath, RevRelativePath).

%---------------------------------------------------------------------------%

rgp_to_fgp(ReverseGoalPath, ForwardGoalPath) :-
    rgp_to_fgp_acc(ReverseGoalPath, fgp_nil, ForwardGoalPath).

:- pred rgp_to_fgp_acc(reverse_goal_path::in,
    forward_goal_path::in, forward_goal_path::out) is det.

rgp_to_fgp_acc(rgp_nil, !ForwardGoalPath).
rgp_to_fgp_acc(rgp_cons(EarlierSteps, LastStep), !ForwardGoalPath) :-
    !:ForwardGoalPath = fgp_cons(LastStep, !.ForwardGoalPath),
    rgp_to_fgp_acc(EarlierSteps, !ForwardGoalPath).

fgp_to_rgp(ForwardGoalPath, ReverseGoalPath) :-
    fgp_to_rgp_acc(ForwardGoalPath, rgp_nil, ReverseGoalPath).

:- pred fgp_to_rgp_acc(forward_goal_path::in,
    reverse_goal_path::in, reverse_goal_path::out) is det.

fgp_to_rgp_acc(fgp_nil, !ReverseGoalPath).
fgp_to_rgp_acc(fgp_cons(FirstStep, LaterSteps), !ReverseGoalPath) :-
    !:ReverseGoalPath = rgp_cons(!.ReverseGoalPath, FirstStep),
    fgp_to_rgp_acc(LaterSteps, !ReverseGoalPath).

%---------------------------------------------------------------------------%

rev_goal_path_remove_type_info(rgp_nil, rgp_nil).
rev_goal_path_remove_type_info(rgp_cons(Steps0, Step0),
        rgp_cons(Steps, Step)) :-
    goal_path_step_remove_type_info(Step0, Step),
    rev_goal_path_remove_type_info(Steps0, Steps).

:- pred goal_path_step_remove_type_info(goal_path_step::in,
    goal_path_step::out) is det.

goal_path_step_remove_type_info(!Step) :-
    (
        ( !.Step = step_conj(_)
        ; !.Step = step_disj(_)
        ; !.Step = step_ite_cond
        ; !.Step = step_ite_then
        ; !.Step = step_ite_else
        ; !.Step = step_neg
        ; !.Step = step_scope(_)
        ; !.Step = step_lambda
        ; !.Step = step_try
        ; !.Step = step_atomic_main
        ; !.Step = step_atomic_orelse(_)
        )
    ;
        !.Step = step_switch(N, _),
        !:Step = step_switch(N, unknown_num_functors_in_type)
    ).

%---------------------------------------------------------------------------%

goal_path_from_string(GoalPathStr, GoalPath) :-
    StepStrs = string.words_separator(is_goal_path_separator, GoalPathStr),
    goal_path_from_strings(StepStrs, GoalPath).

:- pred goal_path_from_strings(list(string)::in, forward_goal_path::out)
    is semidet.

goal_path_from_strings([], fgp_nil).
goal_path_from_strings([Str | Strs], fgp_cons(HeadStep, LaterSteps)) :-
    goal_path_step_from_string(Str, HeadStep),
    goal_path_from_strings(Strs, LaterSteps).

goal_path_from_string_det(GoalPathStr, GoalPath) :-
    ( if goal_path_from_string(GoalPathStr, GoalPathPrime) then
        GoalPath = GoalPathPrime
    else
        unexpected($pred, "goal_path_from_string failed")
    ).

rev_goal_path_from_string(GoalPathStr, GoalPath) :-
    StepStrs = string.words_separator(is_goal_path_separator, GoalPathStr),
    list.reverse(StepStrs, RevStepStrs),
    rev_goal_path_from_rev_strings(RevStepStrs, GoalPath).

:- pred rev_goal_path_from_rev_strings(list(string)::in,
    reverse_goal_path::out) is semidet.

rev_goal_path_from_rev_strings([], rgp_nil).
rev_goal_path_from_rev_strings([Str | Strs], rgp_cons(HeadSteps, TailStep)) :-
    rev_goal_path_from_rev_strings(Strs, HeadSteps),
    goal_path_step_from_string(Str, TailStep).

rev_goal_path_from_string_det(GoalPathStr, GoalPath) :-
    ( if rev_goal_path_from_string(GoalPathStr, GoalPathPrime) then
        GoalPath = GoalPathPrime
    else
        unexpected($pred, "rev_goal_path_from_string failed")
    ).

%---------------------%

goal_path_step_from_string(String, Step) :-
    string.first_char(String, FirstChar, RestStr),
    (
        ( FirstChar = '?',   Step = step_ite_cond
        ; FirstChar = 't',   Step = step_ite_then
        ; FirstChar = 'e',   Step = step_ite_else
        ; FirstChar = ('~'), Step = step_neg
        ; FirstChar = 'r',   Step = step_try
        ; FirstChar = ('='), Step = step_lambda
        ; FirstChar = 'a',   Step = step_atomic_main
        ),
        RestStr = ""
    ;
        FirstChar = 'c',
        string.to_int(RestStr, N),
        Step = step_conj(N)
    ;
        FirstChar = 'd',
        string.to_int(RestStr, N),
        Step = step_disj(N)
    ;
        FirstChar = 'o',
        string.to_int(RestStr, N),
        Step = step_atomic_orelse(N)
    ;
        FirstChar = 's',
        string.words_separator(unify('-'), RestStr) = [NStr, MStr],
        string.to_int(NStr, N),
        ( if MStr = "na" then   % "na" is short for "not applicable"
            MaybeM = unknown_num_functors_in_type
        else
            string.to_int(MStr, M),
            MaybeM = known_num_functors_in_type(M)
        ),
        Step = step_switch(N, MaybeM)
    ;
        FirstChar = 'q',
        (
            RestStr = "",
            Step = step_scope(scope_is_no_cut)
        ;
            RestStr = "!",
            Step = step_scope(scope_is_cut)
        )
    ).

is_goal_path_separator(';').

%---------------------------------------------------------------------------%

goal_path_to_string(GoalPath) = GoalPathStr :-
    StepStrs = goal_path_to_strings(GoalPath),
    string.append_list(StepStrs, GoalPathStr).

:- func goal_path_to_strings(forward_goal_path) = list(string).

goal_path_to_strings(fgp_nil) = [].
goal_path_to_strings(fgp_cons(Step, Steps)) = [Str | Strs] :-
    Str = goal_path_step_to_string(Step),
    Strs = goal_path_to_strings(Steps).

rev_goal_path_to_string(GoalPath) = GoalPathStr :-
    RevStepStrs = rev_goal_path_to_strings(GoalPath),
    list.reverse(RevStepStrs, StepStrs),
    string.append_list(StepStrs, GoalPathStr).

:- func rev_goal_path_to_strings(reverse_goal_path) = list(string).

rev_goal_path_to_strings(rgp_nil) = [].
rev_goal_path_to_strings(rgp_cons(Steps, Step)) = [Str | Strs] :-
    Str = goal_path_step_to_string(Step),
    Strs = rev_goal_path_to_strings(Steps).

:- func goal_path_step_to_string(goal_path_step) = string.

goal_path_step_to_string(step_conj(N)) = "c" ++ int_to_string(N) ++ ";".
goal_path_step_to_string(step_disj(N)) = "d" ++ int_to_string(N) ++ ";".
goal_path_step_to_string(step_switch(N, known_num_functors_in_type(M))) =
    "s" ++ int_to_string(N) ++ "-" ++ int_to_string(M) ++ ";".
goal_path_step_to_string(step_switch(N, unknown_num_functors_in_type)) =
    "s" ++ int_to_string(N) ++ "-na;".      % short for "not applicable"
goal_path_step_to_string(step_ite_cond) = "?;".
goal_path_step_to_string(step_ite_then) = "t;".
goal_path_step_to_string(step_ite_else) = "e;".
goal_path_step_to_string(step_neg) = "~;".
goal_path_step_to_string(step_scope(scope_is_cut)) = "q!;".
goal_path_step_to_string(step_scope(scope_is_no_cut)) = "q;".
goal_path_step_to_string(step_try) = "r;".
goal_path_step_to_string(step_lambda) = "=;".
goal_path_step_to_string(step_atomic_main) = "a;".
goal_path_step_to_string(step_atomic_orelse(N)) =
    "o" ++ int_to_string(N) ++ ";".

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

whole_body_goal_id = goal_id(0).

is_valid_goal_id(goal_id(GoalIdNum)) :-
    GoalIdNum >= 0.

goal_id_for_head_constraints = goal_id(-1).
    % Note that this is NOT a valid goal_id for a goal. Not being able
    % to confuse the goal_id on which head constraints are hung with the
    % goal_id of an actual goal is the POINT of this function.

%---------------------------------------------------------------------------%

goal_id_to_forward_path(ContainingGoalMap, GoalId) = GoalPath :-
    RevGoalPath = goal_id_to_reverse_path(ContainingGoalMap, GoalId),
    rgp_to_fgp(RevGoalPath, GoalPath).

goal_id_to_reverse_path(ContainingGoalMap, GoalId) = GoalPath :-
    map.lookup(ContainingGoalMap, GoalId, ContainingGoal),
    (
        ContainingGoal = whole_body_goal,
        GoalPath = rgp_nil
    ;
        ContainingGoal = containing_goal(ParentGoalId, LastStep),
        EarlierPath = goal_id_to_reverse_path(ContainingGoalMap, ParentGoalId),
        GoalPath = rgp_cons(EarlierPath, LastStep)
    ).

create_forward_goal_path_map(ContainingGoalMap) = ForwardGoalPathMap :-
    ReverseGoalPathMap = create_reverse_goal_path_map(ContainingGoalMap),
    map.map_values_only(rgp_to_fgp, ReverseGoalPathMap, ForwardGoalPathMap).

create_reverse_goal_path_map(ContainingGoalMap) = ReverseGoalPathMap :-
    map.to_assoc_list(ContainingGoalMap, ContainingGoalList),
    create_reverse_goal_path_map_acc(ContainingGoalList,
        map.init, ReverseGoalPathMap).

:- pred create_reverse_goal_path_map_acc(
    assoc_list(goal_id, containing_goal)::in,
    map(goal_id, reverse_goal_path)::in, map(goal_id, reverse_goal_path)::out)
    is det.

create_reverse_goal_path_map_acc([], !ReverseGoalPathMap).
create_reverse_goal_path_map_acc([Head | Tail], !ReverseGoalPathMap) :-
    Head = GoalId - ContainingGoal,
    (
        ContainingGoal = whole_body_goal,
        GoalReversePath = rgp_nil
    ;
        ContainingGoal = containing_goal(ContainingGoalId, Step),
        map.lookup(!.ReverseGoalPathMap, ContainingGoalId,
            ContainingGoalReversePath),
        GoalReversePath = rgp_cons(ContainingGoalReversePath, Step)
    ),
    map.det_insert(GoalId, GoalReversePath, !ReverseGoalPathMap),
    create_reverse_goal_path_map_acc(Tail, !ReverseGoalPathMap).

create_reverse_goal_path_bimap(ContainingGoalMap) = ReverseGoalPathBiMap :-
    map.to_assoc_list(ContainingGoalMap, ContainingGoalList),
    create_reverse_goal_path_bimap_acc(ContainingGoalList,
        bimap.init, ReverseGoalPathBiMap).

:- pred create_reverse_goal_path_bimap_acc(
    assoc_list(goal_id, containing_goal)::in,
    bimap(goal_id, reverse_goal_path)::in,
    bimap(goal_id, reverse_goal_path)::out) is det.

create_reverse_goal_path_bimap_acc([], !ReverseGoalPathBiMap).
create_reverse_goal_path_bimap_acc([Head | Tail], !ReverseGoalPathBiMap) :-
    Head = GoalId - ContainingGoal,
    (
        ContainingGoal = whole_body_goal,
        GoalReversePath = rgp_nil
    ;
        ContainingGoal = containing_goal(ContainingGoalId, Step),
        bimap.lookup(!.ReverseGoalPathBiMap, ContainingGoalId,
            ContainingGoalReversePath),
        GoalReversePath = rgp_cons(ContainingGoalReversePath, Step)
    ),
    bimap.det_insert(GoalId, GoalReversePath, !ReverseGoalPathBiMap),
    create_reverse_goal_path_bimap_acc(Tail, !ReverseGoalPathBiMap).

goal_id_inside(ContainingGoalId, GoalIdA, GoalIdB) :-
    (
        GoalIdB = GoalIdA
    ;
        map.lookup(ContainingGoalId, GoalIdB, GoalContainingB),
        GoalContainingB = containing_goal(ParentGoalIdB, _),
        goal_id_inside(ContainingGoalId, GoalIdA, ParentGoalIdB)
    ).

%---------------------------------------------------------------------------%

create_goal_id_array(goal_id(LastGoalIdNum)) =
    goal_attr_array(array.init(LastGoalIdNum + 1, no)).

create_goal_id_array(goal_id(LastGoalIdNum), Default) =
    goal_attr_array(array.init(LastGoalIdNum + 1, yes(Default))).

update_goal_attribute(goal_id(Index), Value, goal_attr_array(!.Array),
        goal_attr_array(!:Array)) :-
    array.set(Index, yes(Value), !Array).

get_goal_attribute_det(goal_attr_array(Array), goal_id(Index)) = Attr :-
    MaybeAttr = array.lookup(Array, Index),
    (
        MaybeAttr = yes(Attr)
    ;
        MaybeAttr = no,
        unexpected($pred, "Goal attribute array slot empty")
    ).

%---------------------------------------------------------------------------%
:- end_module mdbcomp.goal_path.
%---------------------------------------------------------------------------%
