%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
% in this tree. Therefore value of three types can uniquely identify a goal
% within its defining procedure.
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
% A forward goal path lists the step from the root of the tree to the goal
% being identified.
%
% A reverse goal path lists the step from to the goal being identified to
% the root of the tree.
%
% The code in the compiler that allocates goal ids also returns a containing
% goal map, which maps each goal id to the id of its innermost containing goal
% (if there is one). When possible, new code should use this data structure,
% though code that needs to identify goals in files outside the compiler
% will probably continue to need to use goal paths. The string representations
% of goal paths always list the steps in the forward order, even though
% most operations inside the compiler use reverse goal paths, because most
% operations on goal paths focus on the last element, not the first.
%
%-----------------------------------------------------------------------------%

:- module mdbcomp.goal_path.
:- interface.

:- import_module array.
:- import_module bimap.
:- import_module char.
:- import_module list.
:- import_module map.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type goal_id
    --->    goal_id(int).

:- type forward_goal_path
    --->    fgp(list(goal_path_step)).

:- type reverse_goal_path
    --->    rgp(list(goal_path_step)).

:- type goal_path_string == string.

:- type goal_path_step
    --->    step_conj(int)
    ;       step_disj(int)
    ;       step_switch(int, maybe(int))
    ;       step_ite_cond
    ;       step_ite_then
    ;       step_ite_else
    ;       step_neg
    ;       step_scope(maybe_cut)
    ;       step_lambda
    ;       step_try
    ;       step_atomic_main
    ;       step_atomic_orelse(int).

    % Does the scope goal have a different determinism inside than outside?
:- type maybe_cut
    --->    scope_is_cut
    ;       scope_is_no_cut.

:- func whole_body_goal_id = goal_id.

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

    % Convert the goal path to its string representation. The resulting string
    % is guaranteed to be acceptable to path_from_string_det.
    %
:- func goal_path_to_string(forward_goal_path) = string.

    % Convert the goal path to its string representation. The resulting string
    % is guaranteed to be acceptable to rev_path_from_string_det.
    %
:- func rev_goal_path_to_string(reverse_goal_path) = string.

    % Is this character the one that ends each goal path step?
    %
:- pred is_goal_path_separator(char::in) is semidet.

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

    % Remove information from the goal path that depends on type information.
    %
    % This is necessary when using goal paths to lookup a map within the deep
    % profiler.  The goal paths used to perform the query cannot construct the
    % parts of the goal paths that depend on type information.
    %
:- pred rev_goal_path_remove_type_info(reverse_goal_path::in,
    reverse_goal_path::out) is det.

%----------------------------------------------------------------------------%

:- type containing_goal
    --->    whole_body_goal
            % This goal is the entire body of its procedure.
    ;       containing_goal(goal_id, goal_path_step).
            % This goal is an contained immediately inside the larger goal
            % identified by the goal_id, from which you need to take the
            % given goal_path step to get to this goal.
            %
            % The goal_id of the containing goal is guaranteed to be always
            % less than the goal_id of this goal.

:- type containing_goal_map == map(goal_id, containing_goal).
:- type goal_forward_path_map == map(goal_id, forward_goal_path).
:- type goal_reverse_path_map == map(goal_id, reverse_goal_path).
:- type goal_reverse_path_bimap == bimap(goal_id, reverse_goal_path).

    % goal_id_inside(ContainingGoalMap, GoalIdA, GoalIdB):
    %
    % Succeeds if GoalIdB denotes a goal *inside* the goal denoted by GoalIdA.
    % (It considers a goal to be inside itself.)
    %
:- pred goal_id_inside(containing_goal_map::in,
    goal_id::in, goal_id::in) is semidet.

    % Convert a goal_id to a forward goal path.
    %
:- func goal_id_to_forward_path(containing_goal_map, goal_id) =
    forward_goal_path.

    % Convert a goal_id to a reverse goal path.
    %
:- func goal_id_to_reverse_path(containing_goal_map, goal_id) =
    reverse_goal_path.

    % Given a containing_goal_map, create a map that maps each goal_id in it
    % to a forwward goal path.
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

%-----------------------------------------------------------------------------%

:- type goal_attr_array(T)
    --->    goal_attr_array(array(maybe(T))).

    % This isn't really unique, see the commends at the type of library/array.m
    %
:- inst uniq_goal_attr_array
    --->    goal_attr_array(uniq_array).

:- mode gaa_di == di(uniq_goal_attr_array).
:- mode gaa_uo == out(uniq_goal_attr_array).

    % create_goal_id_array(LastGoalId) = Array.
    %
    % Create an array of the correct size to label all the goals up to and
    % including LastGoalId.
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
    % Make Attirubte the new attribute for GoalId in !:Array.
    %
:- pred update_goal_attribute(goal_id::in, T::in,
    goal_attr_array(T)::gaa_di, goal_attr_array(T)::gaa_uo) is det.

    % get_goal_attribute(Arra, GoalId) = Attribute.
    %
    % Get a goal attribute.
    %
:- func get_goal_attribute_det(goal_attr_array(T), goal_id) = T.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module cord.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module svbimap.
:- import_module svmap.

whole_body_goal_id = goal_id(0).

goal_path_add_at_end(GoalPath0, GoalPathStep) = GoalPath :-
    GoalPath0 = fgp(Steps0),
    Steps = Steps0 ++ [GoalPathStep],
    GoalPath = fgp(Steps).

rev_goal_path_add_at_end(GoalPath0, GoalPathStep) = GoalPath :-
    GoalPath0 = rgp(Steps0),
    Steps = [GoalPathStep | Steps0],
    GoalPath = rgp(Steps).

goal_path_remove_last(GoalPath0, GoalPath, LastStep) :-
    GoalPath0 = fgp(Steps0),
    list.split_last(Steps0, Steps, LastStep),
    GoalPath = fgp(Steps).

goal_path_get_last(GoalPath, LastStep) :-
    goal_path_remove_last(GoalPath, _, LastStep).

rev_goal_path_remove_last(GoalPath0, GoalPath, LastStep) :-
    GoalPath0 = rgp(Steps0),
    Steps0 = [LastStep | Steps],
    GoalPath = rgp(Steps).

rev_goal_path_get_last(GoalPath, LastStep) :-
    rev_goal_path_remove_last(GoalPath, _, LastStep).

goal_path_inside_relative(PathA, PathB, RelativePath) :-
    PathA = fgp(StepsA),
    PathB = fgp(StepsB),
    list.append(StepsA, RelativeSteps, StepsB),
    RelativePath = fgp(RelativeSteps).

rev_goal_path_inside_relative(RevPathA, RevPathB, RevRelative) :-
    RevPathA = rgp(RevStepsA),
    RevPathB = rgp(RevStepsB),
    list.remove_suffix(RevStepsB, RevStepsA, RevRelativeSteps),
    RevRelative = rgp(RevRelativeSteps).

goal_path_inside(PathA, PathB) :-
    goal_path_inside_relative(PathA, PathB, _).

rev_goal_path_inside(RevPathA, RevPathB) :-
    rev_goal_path_inside_relative(RevPathA, RevPathB, _).

goal_path_from_string(GoalPathStr, GoalPath) :-
    StepStrs = string.words_separator(is_goal_path_separator, GoalPathStr),
    list.map(goal_path_step_from_string, StepStrs, Steps),
    GoalPath = fgp(Steps).

goal_path_from_string_det(GoalPathStr, GoalPath) :-
    ( goal_path_from_string(GoalPathStr, GoalPathPrime) ->
        GoalPath = GoalPathPrime
    ;
        unexpected($module, $pred, "goal_path_from_string failed")
    ).

rev_goal_path_from_string(GoalPathStr, GoalPath) :-
    StepStrs = string.words_separator(is_goal_path_separator, GoalPathStr),
    list.map(goal_path_step_from_string, StepStrs, Steps),
    list.reverse(Steps, RevSteps),
    GoalPath = rgp(RevSteps).

rev_goal_path_from_string_det(GoalPathStr, GoalPath) :-
    ( rev_goal_path_from_string(GoalPathStr, GoalPathPrime) ->
        GoalPath = GoalPathPrime
    ;
        unexpected($module, $pred, "rev_goal_path_from_string failed")
    ).

goal_path_step_from_string(String, Step) :-
    string.first_char(String, First, Rest),
    goal_path_step_from_string_2(First, Rest, Step).

:- pred goal_path_step_from_string_2(char::in, string::in, goal_path_step::out)
    is semidet.

goal_path_step_from_string_2('c', NStr, step_conj(N)) :-
    string.to_int(NStr, N).
goal_path_step_from_string_2('d', NStr, step_disj(N)) :-
    string.to_int(NStr, N).
goal_path_step_from_string_2('s', Str, step_switch(N, MaybeM)) :-
    string.words_separator(unify('-'), Str) = [NStr, MStr],
    string.to_int(NStr, N),
    % "na" is short for "not applicable"
    ( MStr = "na" ->
        MaybeM = no
    ;
        string.to_int(MStr, M),
        MaybeM = yes(M)
    ).
goal_path_step_from_string_2('?', "", step_ite_cond).
goal_path_step_from_string_2('t', "", step_ite_then).
goal_path_step_from_string_2('e', "", step_ite_else).
goal_path_step_from_string_2('~', "", step_neg).
goal_path_step_from_string_2('q', "!", step_scope(scope_is_cut)).
goal_path_step_from_string_2('q', "", step_scope(scope_is_no_cut)).
goal_path_step_from_string_2('r', "", step_try).
goal_path_step_from_string_2('=', "", step_lambda).
goal_path_step_from_string_2('a', "", step_atomic_main).
goal_path_step_from_string_2('o', NStr, step_atomic_orelse(N)) :-
    string.to_int(NStr, N).

is_goal_path_separator(';').

goal_path_to_string(GoalPath) = GoalPathStr :-
    GoalPath = fgp(Steps),
    StepStrs = list.map(goal_path_step_to_string, Steps),
    string.append_list(StepStrs, GoalPathStr).

rev_goal_path_to_string(GoalPath) = GoalPathStr :-
    GoalPath = rgp(RevSteps),
    list.reverse(RevSteps, Steps),
    StepStrs = list.map(goal_path_step_to_string, Steps),
    string.append_list(StepStrs, GoalPathStr).

:- func goal_path_step_to_string(goal_path_step) = string.

goal_path_step_to_string(step_conj(N)) = "c" ++ int_to_string(N) ++ ";".
goal_path_step_to_string(step_disj(N)) = "d" ++ int_to_string(N) ++ ";".
goal_path_step_to_string(step_switch(N, yes(M))) = "s" ++ int_to_string(N)
    ++ "-" ++ int_to_string(M) ++ ";".
goal_path_step_to_string(step_switch(N, no)) = "s" ++ int_to_string(N)
    ++ "-na;".      % short for "not applicable"
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

rev_goal_path_remove_type_info(rgp(Steps0), rgp(Steps)) :-
    map(goal_path_step_remove_type_info, Steps0, Steps).

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
        !:Step = step_switch(N, no)
    ).

%-----------------------------------------------------------------------------%

goal_id_inside(ContainingGoalId, GoalIdA, GoalIdB) :-
    (
        GoalIdB = GoalIdA
    ;
        map.lookup(ContainingGoalId, GoalIdB, GoalContainingB),
        GoalContainingB = containing_goal(ParentGoalIdB, _),
        goal_id_inside(ContainingGoalId, GoalIdA, ParentGoalIdB)
    ).

goal_id_to_forward_path(ContainingGoalMap, GoalId) = GoalPath :-
    StepsCord = goal_id_to_steps(ContainingGoalMap, GoalId),
    Steps = cord.list(StepsCord),
    GoalPath = fgp(Steps).

goal_id_to_reverse_path(ContainingGoalMap, GoalId) = GoalPath :-
    StepsCord = goal_id_to_steps(ContainingGoalMap, GoalId),
    Steps = cord.list(StepsCord),
    list.reverse(Steps, RevSteps),
    GoalPath = rgp(RevSteps).

:- func goal_id_to_steps(containing_goal_map, goal_id) =
    cord(goal_path_step).

goal_id_to_steps(ContainingGoalMap, GoalId) = Steps :-
    map.lookup(ContainingGoalMap, GoalId, ContainingGoal),
    (
        ContainingGoal = whole_body_goal,
        Steps = cord.empty
    ;
        ContainingGoal = containing_goal(ParentGoalId, LastStep),
        EarlierSteps = goal_id_to_steps(ContainingGoalMap, ParentGoalId),
        Steps = cord.snoc(EarlierSteps, LastStep)
    ).

create_forward_goal_path_map(ContainingGoalMap) = ForwardGoalPathMap :-
    ReverseGoalPathMap = create_reverse_goal_path_map(ContainingGoalMap),
    map.map_values_only(rgp_to_fgp, ReverseGoalPathMap, ForwardGoalPathMap).

:- pred rgp_to_fgp(reverse_goal_path::in, forward_goal_path::out) is det.

rgp_to_fgp(rgp(RevSteps), fgp(Steps)) :-
    list.reverse(RevSteps, Steps).

create_reverse_goal_path_map(ContainingGoalMap) = ReverseGoalPathMap :-
    map.to_assoc_list(ContainingGoalMap, ContainingGoalList),
    create_reverse_goal_path_map_2(ContainingGoalList,
        map.init, ReverseGoalPathMap).

:- pred create_reverse_goal_path_map_2(
    assoc_list(goal_id, containing_goal)::in,
    map(goal_id, reverse_goal_path)::in, map(goal_id, reverse_goal_path)::out)
    is det.

create_reverse_goal_path_map_2([], !ReverseGoalPathMap).
create_reverse_goal_path_map_2([Head | Tail], !ReverseGoalPathMap) :-
    Head = GoalId - ContainingGoal,
    (
        ContainingGoal = whole_body_goal,
        GoalReversePath = rgp([])
    ;
        ContainingGoal = containing_goal(ContainingGoalId, Step),
        map.lookup(!.ReverseGoalPathMap, ContainingGoalId,
            ContainingGoalReversePath),
        ContainingGoalReversePath = rgp(ContainingGoalReverseSteps),
        GoalReverseSteps = [Step | ContainingGoalReverseSteps],
        GoalReversePath = rgp(GoalReverseSteps)
    ),
    svmap.det_insert(GoalId, GoalReversePath, !ReverseGoalPathMap),
    create_reverse_goal_path_map_2(Tail, !ReverseGoalPathMap).

create_reverse_goal_path_bimap(ContainingGoalMap) = ReverseGoalPathBiMap :-
    map.to_assoc_list(ContainingGoalMap, ContainingGoalList),
    create_reverse_goal_path_bimap_2(ContainingGoalList,
        bimap.init, ReverseGoalPathBiMap).

:- pred create_reverse_goal_path_bimap_2(
    assoc_list(goal_id, containing_goal)::in,
    bimap(goal_id, reverse_goal_path)::in,
    bimap(goal_id, reverse_goal_path)::out) is det.

create_reverse_goal_path_bimap_2([], !ReverseGoalPathBiMap).
create_reverse_goal_path_bimap_2([Head | Tail], !ReverseGoalPathBiMap) :-
    Head = GoalId - ContainingGoal,
    (
        ContainingGoal = whole_body_goal,
        GoalReversePath = rgp([])
    ;
        ContainingGoal = containing_goal(ContainingGoalId, Step),
        bimap.lookup(!.ReverseGoalPathBiMap, ContainingGoalId,
            ContainingGoalReversePath),
        ContainingGoalReversePath = rgp(ContainingGoalReverseSteps),
        GoalReverseSteps = [Step | ContainingGoalReverseSteps],
        GoalReversePath = rgp(GoalReverseSteps)
    ),
    svbimap.det_insert(GoalId, GoalReversePath, !ReverseGoalPathBiMap),
    create_reverse_goal_path_bimap_2(Tail, !ReverseGoalPathBiMap).

%-----------------------------------------------------------------------------%

create_goal_id_array(goal_id(LastGoalIdNum)) =
    goal_attr_array(array.init(LastGoalIdNum + 1, no)).

create_goal_id_array(goal_id(LastGoalIdNum), Default) =
    goal_attr_array(array.init(LastGoalIdNum + 1, yes(Default))).

update_goal_attribute(goal_id(Index), Value, goal_attr_array(!.Array),
        goal_attr_array(!:Array)) :-
    array.svset(Index, yes(Value), !Array).

get_goal_attribute_det(goal_attr_array(Array), goal_id(Index)) = Attr :-
    MaybeAttr = array.lookup(Array, Index),
    (
        MaybeAttr = yes(Attr)
    ;
        MaybeAttr = no,
        unexpected($module, $pred, "Goal attribute array slot empty")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
