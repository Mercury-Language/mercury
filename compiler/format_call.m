%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: format_call.m.
% Author: zs.
% 
% The job of this module is to generate warnings about calls to string.format
% and io.format in which the format string and the supplied lists of values
% do not agree. The difficult part of this job is actually finding the values
% of the variables representing the format string and the list of values to
% be printed.
%
% The general approach is a backwards traversal of the procedure body. During
% this traversal, we assign an id to every conjunction (considering a cond and
% then parts of an if-then-else to be a conjunction). When we find a call to
% string.format or io.format, we remember the call site together with the
% identities of the variables holding the format string and the values to
% be printed, and include both variables in the set of variables whose values
% we want to track. As we traverse unifications that bind variables we want to
% track, we record their value in a map specific to the conjunction containing
% the unification. Actually, we keep four such maps. Three record information
% about bindings to function symbols: one for format strings, one for the
% skeletons of the lists of values, and one for the elements of those lists.
% The fourth map is for variable equivalences.
%
% We also record relationships between the conjunctions. Consider the code
% structure below, which contains two relevant conjunctions: the outer one,
% and the one containing the cond and then parts of the inner if-then-else.
% Any attempt to trace the value of V2 requires also knowing the value of V1.
% We therefore record that if you can't find the value of a variable such as V1
% in the inner conjunction, you should continue the search in the outer
% conjunction. We call this relationship "predecessor", since the only relevant
% part of the outer conjunction is the one that appears before the inner one.
% This is enforced by the mode system.
%
%   (
%       ...,
%       V1 = ...,
%       ...,
%       (
%           ...
%       ->
%           V2 = ... V1 ...,
%           string.format(..., V2, ...)
%       ;
%           V3 = ... V1 ...,
%           string.format(..., V3, ...)
%       ),
%       ...
%   )
%
% This design is about as cheap in terms of compilation time as we can make it.
% Its cost has two components. The first component is the traversal, and its
% cost is roughly proportional to the size of the procedure body. The second
% cost is the checking of each call to string.format or io.format. The expected
% complexity of this part is proportional to the number of such calls
% multiplied by the average number of arguments they print. In the worst case,
% this can be multiplied again by the number of conjunctions in the procedure
% body, but I expect that in most cases the variables involved in the relevant
% calls will be found in the same conjunction as the call itself, so the
% typical number of conjunctions that has to be searched will in fact be one.
%
% Note that if the value of e.g. a format string is an input to the procedure 
% or is computed by a call rather than a unification, we won't be able to check
% whether the values match the format string. Whether we give a warning in such
% cases is controlled by a separate option, which is consulted in det_report.m.
%
% We could in theory track e.g. format strings through calls to library
% functions such as string.append. However, there is no convenient way to
% evaluate the extent of a need for this capability until this change is
% bootstrapped, so that is left for future work.
% 
%-----------------------------------------------------------------------------%

:- module check_hlds.format_call.
:- interface.

:- import_module check_hlds.det_report.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.

:- import_module mdbcomp.prim_data.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

:- pred is_format_call(module_name::in, string::in, list(prog_var)::in,
    prog_var::out, prog_var::out) is semidet.

:- pred find_format_call_errors(module_info::in, hlds_goal::in,
    set(context_det_msg)::in, set(context_det_msg)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.

:- import_module counter.
:- import_module exception.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module svmap.
:- import_module svset.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type format_call_site
    --->    format_call_site(
                format_string_var       :: prog_var,
                formatted_values_var    :: prog_var,
                called_pred_module      :: module_name,
                called_pred_name        :: string,
                called_pred_arity       :: arity,
                call_context            :: prog_context,
                containing_conj         :: conj_id
            ).

:- type list_skeleton_state
    --->    list_skeleton_nil
    ;       list_skeleton_cons(
                head    :: prog_var,
                tail    :: prog_var
            ).

    % Maps each variable representing a format string to the format string
    % itself.
:- type string_map          == map(prog_var, string).

    % Maps each variable participating in the skeleton of the list of values to
    % be printed to its value.
:- type list_skeleton_map   == map(prog_var, list_skeleton_state).

    % Maps each variable representing a value in the list of values to be
    % printed to a dummy value of the same kind. We don't include the actual
    % value to be printed, since (a) in almost all cases that won't be
    % available statically in the program, and (b) we don't actually need it.
:- type list_element_map    == map(prog_var, string.poly_type).

    % Maps each variable defined in terms of another variable to the variable
    % it is assigned from.
:- type eqv_map             == map(prog_var, prog_var).

    % The knowledge we have recorded from assign and construct unifications in
    % a given conjunction.
:- type conj_map
    --->    conj_map(
                string_map          :: string_map,
                list_skeleton_map   :: list_skeleton_map,
                list_element_map    :: list_element_map,
                eqv_map             :: eqv_map
            ).

:- type conj_id
    --->    conj_id(int).

    % Maps the id of each conjunction to the knowledge we have derived from
    % unifications in that conjunction.
:- type conj_maps == map(conj_id, conj_map).

    % Maps each conjunction to its predecessor (if any) in the sense documented
    % above.
:- type conj_pred_map == map(conj_id, conj_id).

%-----------------------------------------------------------------------------%

is_format_call(ModuleName, Name, Args, FormatStringVar, FormattedValuesVar) :-
    Name = "format",
    ( ModuleName = mercury_std_lib_module_name("string") ->
        % We have these arguments regardless of whether we call the
        % predicate or function version of string.format.
        Args = [FormatStringVar, FormattedValuesVar, _ResultVar]
    ; ModuleName = mercury_std_lib_module_name("io") ->
        ( Args = [FormatStringVar, FormattedValuesVar, _IOIn, _IOOut]
        ; Args = [_Stream, FormatStringVar, FormattedValuesVar, _IOIn, _IOOut]
        )
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

find_format_call_errors(ModuleInfo, Goal, !Msgs) :-
    map.init(ConjMaps0),
    counter.init(0, Counter0),
    traverse_goal(Goal, _, [], FormatCallSites, Counter0, _Counter,
        ConjMaps0, ConjMaps, map.init, PredMap, set.init, _, ModuleInfo),
    list.foldl(check_format_call_site(ConjMaps, PredMap), FormatCallSites,
        !Msgs).

:- pred check_format_call_site(conj_maps::in, conj_pred_map::in,
    format_call_site::in, set(context_det_msg)::in, set(context_det_msg)::out)
    is det.

check_format_call_site(ConjMaps, PredMap, FormatCallSite, !Msgs) :-
    FormatCallSite = format_call_site(StringVar, ValuesVar,
        ModuleName, Name, Arity, Context, CurId),
    SymName = qualified(ModuleName, Name),

    (
        follow_format_string(ConjMaps, PredMap, CurId, StringVar,
            yes(FormatString0))
    ->
        MaybeFormatString = yes(FormatString0)
    ;
        MaybeFormatString = no,
        StringMsg = unknown_format_string(SymName, Arity),
        ContextStringMsg = context_det_msg(Context, StringMsg),
        svset.insert(ContextStringMsg, !Msgs)
    ),

    (
        follow_list(ConjMaps, PredMap, CurId, ValuesVar, yes(Skeleton)),
        list.map(follow_list_value(ConjMaps, PredMap, CurId), Skeleton,
            MaybeValueList),
        project_all_yes(MaybeValueList, Values0)
    ->
        MaybeValues = yes(Values0)
    ;
        MaybeValues = no,
        ValuesMsg = unknown_format_values(SymName, Arity),
        ContextValuesMsg = context_det_msg(Context, ValuesMsg),
        svset.insert(ContextValuesMsg, !Msgs)
    ),

    (
        MaybeFormatString = yes(FormatString),
        MaybeValues = yes(Values)
    ->
        promise_equivalent_solutions [Result] (
            try(string.format(FormatString, Values), Result)
        ),
        (
            Result = exception(ExceptionUniv),
            ( univ_to_type(ExceptionUniv, ExceptionError) ->
                ExceptionError = software_error(ExceptionMsg0),
                ( string.append("string.format: ", Msg, ExceptionMsg0) ->
                    ExceptionMsg = Msg
                ;
                    ExceptionMsg = ExceptionMsg0
                ),
                BadMsg = bad_format(SymName, Arity, ExceptionMsg),
                ContextBadMsg = context_det_msg(Context, BadMsg),
                svset.insert(ContextBadMsg, !Msgs)
            ;
                % We can't decode arbitrary exception values, but string.m
                % shouldn't throw anything but software_errors, so ignoring
                % the exception should be ok.
                true
            )
        ;
            % There is no need for any error message; the format works.
            Result = succeeded(_)
        )
    ;
        % Any error message has already been generated, if asked for.
        true
    ).

:- pred follow_format_string(conj_maps::in, conj_pred_map::in, conj_id::in,
    prog_var::in, maybe(string)::out) is det.

follow_format_string(ConjMaps, PredMap, CurId, StringVar, MaybeString) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(StringMap, _, _, EqvMap),
    ( map.search(EqvMap, StringVar, EqvVar) ->
        follow_format_string(ConjMaps, PredMap, CurId, EqvVar, MaybeString)
    ; map.search(StringMap, StringVar, String) ->
        MaybeString = yes(String)
    ; map.search(PredMap, CurId, PredId) ->
        follow_format_string(ConjMaps, PredMap, PredId, StringVar, MaybeString)
    ;
        MaybeString = no
    ).

:- pred follow_list(conj_maps::in, conj_pred_map::in, conj_id::in,
    prog_var::in, maybe(list(prog_var))::out) is det.

follow_list(ConjMaps, PredMap, CurId, ListVar, MaybeSkeleton) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(_, ListMap, _, EqvMap),
    ( map.search(EqvMap, ListVar, EqvVar) ->
        follow_list(ConjMaps, PredMap, CurId, EqvVar, MaybeSkeleton)
    ; map.search(ListMap, ListVar, ListState) ->
        (
            ListState = list_skeleton_nil,
            Skeleton = [],
            MaybeSkeleton = yes(Skeleton)
        ;
            ListState = list_skeleton_cons(HeadVar, TailVar),
            follow_list(ConjMaps, PredMap, CurId, TailVar, MaybeSkeletonTail),
            (
                MaybeSkeletonTail = no,
                MaybeSkeleton = no
            ;
                MaybeSkeletonTail = yes(SkeletonTail),
                Skeleton = [HeadVar | SkeletonTail],
                MaybeSkeleton = yes(Skeleton)
            )
        )
    ; map.search(PredMap, CurId, PredId) ->
        follow_list(ConjMaps, PredMap, PredId, ListVar, MaybeSkeleton)
    ;
        MaybeSkeleton = no
    ).

:- pred follow_list_value(conj_maps::in, conj_pred_map::in, conj_id::in,
    prog_var::in, maybe(string.poly_type)::out) is det.

follow_list_value(ConjMaps, PredMap, CurId, ElementVar, MaybeValue) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(_, _, ElementMap, EqvMap),
    ( map.search(EqvMap, ElementVar, EqvVar) ->
        follow_list_value(ConjMaps, PredMap, CurId, EqvVar, MaybeValue)
    ; map.search(ElementMap, ElementVar, Value) ->
        MaybeValue = yes(Value)
    ; map.search(PredMap, CurId, PredId) ->
        follow_list_value(ConjMaps, PredMap, PredId, ElementVar, MaybeValue)
    ;
        MaybeValue = no
    ).

:- pred project_all_yes(list(maybe(T))::in, list(T)::out) is semidet.

project_all_yes([], []).
project_all_yes([yes(Value) | TailMaybes], [Value | Tail]) :-
    project_all_yes(TailMaybes, Tail).

%-----------------------------------------------------------------------------%

:- pred traverse_goal(hlds_goal::in, conj_id::out,
    list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    set(prog_var)::in, set(prog_var)::out, module_info::in) is det.

traverse_goal(Goal, CurId, !FormatCallSites, !Counter, !ConjMaps, !PredMap,
        !RelevantVars, ModuleInfo) :-
    alloc_id(CurId, !Counter),
    goal_to_conj_list(Goal, GoalConj),
    traverse_conj(GoalConj, CurId, !FormatCallSites, !Counter,
        !ConjMaps, !PredMap, !RelevantVars, ModuleInfo).

:- pred traverse_conj(list(hlds_goal)::in, conj_id::in,
    list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    set(prog_var)::in, set(prog_var)::out, module_info::in) is det.

traverse_conj([], _CurId, !FormatCallSites, !Counter,
        !ConjMaps, !PredMap, !RelevantVars, _ModuleInfo).
traverse_conj([Goal | Goals], CurId, !FormatCallSites, !Counter,
        !ConjMaps, !PredMap, !RelevantVars, ModuleInfo) :-
    traverse_conj(Goals, CurId, !FormatCallSites, !Counter,
        !ConjMaps, !PredMap, !RelevantVars, ModuleInfo),
    Goal = GoalExpr - GoalInfo,
    (
        GoalExpr = conj(_, Conjuncts),
        traverse_conj(Conjuncts, CurId, !FormatCallSites, !Counter,
            !ConjMaps, !PredMap, !RelevantVars, ModuleInfo)
    ;
        GoalExpr = disj(Disjuncts),
        traverse_disj(Disjuncts, CurId, !FormatCallSites, !Counter,
            !ConjMaps, !PredMap, !RelevantVars, ModuleInfo)
    ;
        GoalExpr = switch(_, _, Cases),
        Disjuncts = list.map(project_case_goal, Cases),
        traverse_disj(Disjuncts, CurId, !FormatCallSites, !Counter,
            !ConjMaps, !PredMap, !RelevantVars, ModuleInfo)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),

        traverse_goal(Else, ElseId, !FormatCallSites, !Counter, !ConjMaps,
            !PredMap, !RelevantVars, ModuleInfo),
        svmap.det_insert(ElseId, CurId, !PredMap),

        alloc_id(CondThenId, !Counter),
        goal_to_conj_list(Then, ThenConj),
        goal_to_conj_list(Cond, CondConj),
        traverse_conj(CondConj ++ ThenConj, CondThenId, !FormatCallSites,
            !Counter, !ConjMaps, !PredMap, !RelevantVars, ModuleInfo),
        svmap.det_insert(CondThenId, CurId, !PredMap)
    ;
        GoalExpr = negation(SubGoal),
        traverse_goal(SubGoal, SubGoalId, !FormatCallSites,
            !Counter, !ConjMaps, !PredMap, !RelevantVars, ModuleInfo),
        svmap.det_insert(SubGoalId, CurId, !PredMap)
    ;
        GoalExpr = scope(_, SubGoal),
        traverse_conj([SubGoal], CurId, !FormatCallSites, !Counter,
            !ConjMaps, !PredMap, !RelevantVars, ModuleInfo)
    ;
        GoalExpr = generic_call(_, _, _, _)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = plain_call(PredId, _ProcId, Args, _, _, _),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        ( is_format_call(ModuleName, Name, Args, StringVar, ValuesVar) ->
            Arity = pred_info_orig_arity(PredInfo),
            goal_info_get_context(GoalInfo, Context),
            FormatCallSite = format_call_site(StringVar, ValuesVar,
                ModuleName, Name, Arity, Context, CurId),
            !:FormatCallSites = [FormatCallSite | !.FormatCallSites],
            svset.insert_list([StringVar, ValuesVar], !RelevantVars)
        ;
            true
        )
    ;
        GoalExpr = unify(_, _, _, Unification, _),
        traverse_unify(Unification, CurId, !ConjMaps, !PredMap, !RelevantVars)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded by now.
        unexpected(this_file, "traverse_conj: shorthand")
    ).

:- pred traverse_unify(unification::in, conj_id::in,
    conj_maps::in, conj_maps::out, conj_pred_map::in, conj_pred_map::out,
    set(prog_var)::in, set(prog_var)::out) is det.

traverse_unify(Unification, CurId, !ConjMaps, !PredMap, !RelevantVars) :-
    (
        Unification = assign(TargetVar, SourceVar),
        ( set.member(TargetVar, !.RelevantVars) ->
            svset.delete(TargetVar, !RelevantVars),
            svset.insert(SourceVar, !RelevantVars),
            ConjMap0 = get_conj_map(!.ConjMaps, CurId),
            ConjMap0 = conj_map(StringMap, ListMap, ElementMap, EqvMap0),
            map.set(EqvMap0, TargetVar, SourceVar, EqvMap),
            ConjMap = conj_map(StringMap, ListMap, ElementMap, EqvMap),
            svmap.set(CurId, ConjMap, !ConjMaps)
        ;
            true
        )
    ;
        Unification = construct(CellVar, ConsId, ArgVars, _, _, _, _),
        ( set.member(CellVar, !.RelevantVars) ->
            ConjMap0 = get_conj_map(!.ConjMaps, CurId),
            ConjMap0 = conj_map(StringMap0, ListMap0, ElementMap0, EqvMap0),
            (
                ConsId = string_const(StringConst)
            ->
                svset.delete(CellVar, !RelevantVars),
                map.set(StringMap0, CellVar, StringConst, StringMap),
                ConjMap = conj_map(StringMap, ListMap0, ElementMap0, EqvMap0)
            ;
                ConsId = cons(SymName, _Arity),
                StringModule = mercury_std_lib_module_name("list"),
                SymName = qualified(StringModule, Functor),
                (
                    Functor = "[|]",
                    ArgVars = [ArgVar1, ArgVar2],
                    List = list_skeleton_cons(ArgVar1, ArgVar2)
                ;
                    Functor = "[]",
                    ArgVars = [],
                    List = list_skeleton_nil
                )
            ->
                svset.delete(CellVar, !RelevantVars),
                svset.insert_list(ArgVars, !RelevantVars),
                map.set(ListMap0, CellVar, List, ListMap),
                ConjMap = conj_map(StringMap0, ListMap, ElementMap0, EqvMap0)
            ;
                ConsId = cons(SymName, Arity),
                Arity = 1,
                StringModule = mercury_std_lib_module_name("string"),
                SymName = qualified(StringModule, Functor),
                (
                    Functor = "f",
                    PolyType = f(0.0)
                ;
                    Functor = "i",
                    PolyType = i(0)
                ;
                    Functor = "s",
                    PolyType = s("0")
                ;
                    Functor = "c",
                    PolyType = c('0')
                )
            ->
                svset.delete(CellVar, !RelevantVars),
                map.set(ElementMap0, CellVar, PolyType, ElementMap),
                ConjMap = conj_map(StringMap0, ListMap0, ElementMap, EqvMap0)
            ;
                ConjMap = ConjMap0
            ),
            svmap.set(CurId, ConjMap, !ConjMaps)
        ;
            true
        )
    ;
        Unification = deconstruct(_, _, _, _, _, _)
    ;
        Unification = simple_test(_, _)
    ;
        Unification = complicated_unify(_, _, _)
    ).

:- func project_case_goal(case) = hlds_goal.

project_case_goal(case(_, Goal)) = Goal.

:- pred traverse_disj(list(hlds_goal)::in, conj_id::in,
    list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    set(prog_var)::in, set(prog_var)::out, module_info::in) is det.

traverse_disj(Disjuncts, CurId, !FormatCallSites, !Counter,
        !ConjMaps, !PredMap, !RelevantVars, ModuleInfo) :-
    traverse_disj_arms(Disjuncts, CurId, DisjFormatCallSitesLists,
        !Counter, !ConjMaps, !PredMap, DisjRelevantVarSets, ModuleInfo),
    list.condense(DisjFormatCallSitesLists, DisjFormatCallSites),
    !:FormatCallSites = !.FormatCallSites ++ DisjFormatCallSites,
    DisjRelevantVars = set.union_list(DisjRelevantVarSets),
    set.union(DisjRelevantVars, !RelevantVars).

:- pred traverse_disj_arms(list(hlds_goal)::in, conj_id::in,
    list(list(format_call_site))::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out, list(set(prog_var))::out,
    module_info::in) is det.

traverse_disj_arms([], _, [], !Counter, !ConjMaps, !PredMap, [], _).
traverse_disj_arms([Goal | Goals], ContainingId,
        [FormatCallSites | FormatCallSitesTail], !Counter,
        !ConjMaps, !PredMap, [RelevantVars | RelevantVarSets], ModuleInfo) :-
    traverse_goal(Goal, DisjId, [], FormatCallSites, !Counter,
        !ConjMaps, !PredMap, set.init, RelevantVars, ModuleInfo),
    svmap.det_insert(DisjId, ContainingId, !PredMap),
    traverse_disj_arms(Goals, ContainingId, FormatCallSitesTail, !Counter,
        !ConjMaps, !PredMap, RelevantVarSets, ModuleInfo).

:- func get_conj_map(conj_maps, conj_id) = conj_map.

get_conj_map(ConjMaps, ConjId) = ConjMap :-
    ( map.search(ConjMaps, ConjId, ConjMapPrime) ->
        ConjMap = ConjMapPrime
    ;
        ConjMap = conj_map(map.init, map.init, map.init, map.init)
    ).

:- pred alloc_id(conj_id::out, counter::in, counter::out) is det.

alloc_id(ConjId, !Counter) :-
    counter.allocate(N, !Counter),
    ConjId = conj_id(N).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "format_call.m".

%-----------------------------------------------------------------------------%
