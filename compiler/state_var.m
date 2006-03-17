%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: state_var.m.
% Main author: rafe.

:- module hlds.make_hlds.state_var.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.

    % This synonym improves code legibility.
    %
:- type svar == prog_var.

:- type svars == list(svar).

    % A set of state variables.
    %
:- type svar_set == set(svar).

    % A mapping from state variables to logical variables.
    %
:- type svar_map == map(svar, prog_var).

    % This controls how state variables are dealt with.
    %
:- type svar_ctxt
    --->    in_head
            % In the head of a clause or lambda.

    ;       in_body
            % In the body of a clause or lambda.

    ;       in_atom(
                % In the context of an atomic goal at the level of the
                % source code.
                had_colon_reference :: svar_set,
                                    % The set of state variables X that
                                    % have been referenced as !:X in the
                                    % parameters of the atomic goal.
                parent_svar_info    :: svar_info
                                    % The parent svar_info, used to keep
                                    % track of nesting in subterms of
                                    % an atomic formula.
            ).

:- type svar_info
    --->    svar_info(
                ctxt            ::  svar_ctxt,

                num             ::  int,
                                % This is used to number state variables and
                                % is incremented for each source-level
                                % conjunct.

                external_dot    ::  svar_map,
                                % The "read only" state variables in
                                % scope (e.g. external state variables
                                % visible from within a lambda body or
                                % condition of an if-then-else expression.)

                dot             ::  svar_map,
                colon           ::  svar_map
                                % The "read/write" state variables in scope.
            ).

    % When collecting the arms of a disjunction we also need to
    % collect the resulting svar_infos.
    %
:- type hlds_goal_svar_info == {hlds_goal, svar_info}.

:- type hlds_goal_svar_infos == list(hlds_goal_svar_info).

    % Create a new svar_info set up to start processing a clause head.
    %
:- func new_svar_info = svar_info.

    % Obtain the mapping for a !.X state variable reference and
    % update the svar_info.
    %
    % If we are processing the head of a clause or lambda, we
    % incrementally accumulate the mappings.
    %
    % Otherwise, the mapping must already be present for a local
    % or `external' state variable (i.e. one that may be visible,
    % but not updatable, in the current context.)
    %
    % Note that if !.X does not appear in the head then !:X must
    % appear before !.X can be referenced.
    %
:- pred dot(prog_context::in, svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

    % Obtain the mapping for a !:X state variable reference.
    %
    % If we are processing the head of a clause or lambda, we
    % incrementally accumulate the mappings.
    %
    % Otherwise, the mapping must already be present for a local
    % state variable (`externally' visible state variables cannot
    % be updated.)
    %
    % We also keep track of which state variables have been updated
    % in an atomic context.
    %
:- pred colon(prog_context::in, svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

    % Prepare for the head of a new clause.
    %
:- pred prepare_for_head(svar_info::out) is det.

    % We need to make the current !.Xs external
    % ("read-only") and clear the !.Xs and !:Xs.
    %
    % While processing the head, any state variables therein are
    % implicitly scoped over the body and have !. and !: mappings
    % set up.
    %
:- pred prepare_for_lambda(svar_info::in, svar_info::out) is det.

    % Having processed the head of a clause, prepare for the first
    % (source-level) atomic conjunct.  We return the final !:
    % mappings identified while processing the head.
    %
:- pred prepare_for_body(svar_map::out, prog_varset::in, prog_varset::out,
    svar_info::in, svar_info::out) is det.

    % We have to conjoin the goals and add unifiers to tie up all
    % the final values of the state variables to the head variables.
    %
:- pred finish_goals(prog_context::in, svar_map::in,
    list(hlds_goal)::in, hlds_goal::out, svar_info::in) is det.

    % Add some local state variables.
    %
:- pred prepare_for_local_state_vars(svars::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

    % Remove some local state variables.
    %
:- pred finish_local_state_vars(svars::in, prog_vars::out,
    svar_info::in, svar_info::in, svar_info::out) is det.

    % We have to add unifiers to the Then and Else arms of an
    % if-then-else to make sure all the state variables match up.
    %
    % More to the point, we have to add unifiers to the Then arm
    % for any new state variable mappings produced in the condition.
    %
    % We construct new mappings for the state variables and then
    % add unifiers.
    %
:- pred finish_if_then_else(prog_context::in, hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, svar_info::in,
    svar_info::in, svar_info::in, svar_info::in, svar_info::out,
    prog_varset::in, prog_varset::out) is det.

:- pred finish_if_then_else_goal_condition(svars::in,
    svar_info::in, svar_info::in, svar_info::out, svar_info::out) is det.

:- pred finish_if_then_else_expr_condition(svar_info::in,
    svar_info::in, svar_info::out) is det.

:- pred finish_if_then_else_expr_then_goal(svars::in,
    svar_info::in, svar_info::in, svar_info::out) is det.

    % We assume that a negation updates all state variables in scope,
    % so we construct new mappings for the state variables and then
    % add unifiers from their pre-negated goal mappings.
    %
:- pred finish_negation(svar_info::in, svar_info::in, svar_info::out) is det.

    % We have to make sure that all arms of a disjunction produce the
    % same state variable bindings by adding unifiers as necessary.
    %
:- pred finish_disjunction(prog_context::in, prog_varset::in,
    hlds_goal_svar_infos::in, hlds_goals::out, svar_info::out) is det.

    % We treat equivalence goals as if they were negations (they are
    % in a negated context after all.)
    %
:- pred finish_equivalence(svar_info::in, svar_info::in, svar_info::out)
    is det.

    % We prepare for a call by setting the ctxt to in_atom.  If we're
    % already in an atom then we inherit the parent's set of "updated"
    % state variables.
    %
:- pred prepare_for_call(svar_info::in, svar_info::out) is det.

    % When we finish a call, we're either still inside the
    % atomic formula, in which case we simply propagate the set of
    % "updated" state variables, or we've just emerged, in which case
    % we need to set up the svar_info for the next conjunct.
    %
    % (We can still be in an atomic context if, for example, we've
    % been processing a function call which must appear as an
    % expression and hence occur inside an atomic context.)
    %
:- pred finish_call(prog_varset::in, prog_varset::out,
    svar_info::in, svar_info::out) is det.

:- pred prepare_for_if_then_else_goal(svars::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

:- pred finish_if_then_else_goal_then_goal(svars::in,
    svar_info::in, svar_info::in, svar_info::out) is det.

    % The condition of an if-then-else expression is a goal in which
    % only !.X state variables in scope are visible (although the goal
    % may use local state variables introduced via an explicit
    % quantifier.)  The StateVars are local to the condition and then-goal.
    %
:- pred prepare_for_if_then_else_expr(svars::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

    % Having finished processing one source-level atomic conjunct, prepare
    % for the next.  Note that if !:X was not seen in the conjunct we've
    % just processed, then we can reuse the !.X and !:X mappings.
    %
    %   p(!.X) where [!.X -> X0, !:X -> X1]
    %
    % can yield
    %
    %   p(X0) and [!.X -> X0, !:X -> X2]
    %
    % but
    %
    %   p(!.X, !:X) where [!.X -> X0, !:X -> X1]
    %
    % will yield
    %
    %   p(X0, X1) and [!.X -> X1, !:X -> X2]
    %
:- pred prepare_for_next_conjunct(svar_set::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
    is det.

    % Given a list of argument terms, substitute !.X and !:X with
    % the corresponding state variable mappings.  Any !X should
    % already have been expanded into !.X, !:X via a call to
    % expand_bang_state_var_args/1.
    %
:- pred substitute_state_var_mappings(list(prog_term)::in,
    list(prog_term)::out, prog_varset::in, prog_varset::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

:- pred substitute_state_var_mapping(prog_term::in, prog_term::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

    % Replace !X args with two args !.X, !:X in that order.
    %
:- func expand_bang_state_var_args(list(prog_term)) = list(prog_term).

:- func expand_bang_state_var_args_in_instance_method_heads(instance_body) =
    instance_body.

:- pred illegal_state_var_func_result(pred_or_func::in, list(prog_term)::in,
    svar::out) is semidet.

    % We do not allow !X to appear as a lambda head argument.
    % We might extend the syntax still further to accommodate
    % this as an option, e.g. !IO::(di, uo).
    %
:- pred lambda_args_contain_bang_state_var(list(prog_term)::in, prog_var::out)
    is semidet.

:- pred report_illegal_state_var_update(prog_context::in, prog_varset::in,
    svar::in, io::di, io::uo) is det.

:- pred report_illegal_func_svar_result(prog_context::in, prog_varset::in,
    svar::in, io::di, io::uo) is det.

:- pred report_illegal_bang_svar_lambda_arg(prog_context::in, prog_varset::in,
    svar::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_util.

:- import_module char.
:- import_module int.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

new_svar_info = svar_info(in_head, 0, map.init, map.init, map.init).

:- pred svar_info `has_svar_colon_mapping_for` svar.
:- mode in `has_svar_colon_mapping_for` in is semidet.

SInfo `has_svar_colon_mapping_for` StateVar :-
    SInfo ^ colon `contains` StateVar.
SInfo `has_svar_colon_mapping_for` StateVar :-
    SInfo ^ ctxt = in_atom(_, ParentSInfo),
    ParentSInfo `has_svar_colon_mapping_for` StateVar.

:- func svar_info `with_updated_svar` svar = svar_info.

SInfo `with_updated_svar` StateVar =
    ( SInfo ^ ctxt =  in_atom(UpdatedStateVars, ParentSInfo) ->
        SInfo ^ ctxt := in_atom(set.insert(UpdatedStateVars, StateVar),
            ParentSInfo)
    ;
        SInfo
    ).

%-----------------------------------------------------------------------------%

dot(Context, StateVar, Var, !VarSet, !SInfo, !IO) :-
    ( !.SInfo ^ ctxt = in_head ->
        ( !.SInfo ^ dot ^ elem(StateVar) = Var0 ->
            Var = Var0
        ;
            new_dot_state_var(StateVar, Var, !VarSet, !SInfo)
        )
    ;
        ( !.SInfo ^ dot ^ elem(StateVar) = Var0 ->
            Var = Var0
        ; !.SInfo ^ external_dot ^ elem(StateVar) = Var0 ->
            Var = Var0
        ; !.SInfo `has_svar_colon_mapping_for` StateVar ->
            new_dot_state_var(StateVar, Var, !VarSet, !SInfo),
            report_unitialized_state_var(Context, !.VarSet, StateVar, !IO)
        ;
            Var = StateVar,
            report_non_visible_state_var(".", Context, !.VarSet, StateVar, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

colon(Context, StateVar, Var, !VarSet, !SInfo, !IO) :-
    ( !.SInfo ^ ctxt = in_head ->
        ( !.SInfo ^ colon ^ elem(StateVar) = Var0 ->
            Var = Var0
        ;
            new_final_state_var(StateVar, Var, !VarSet, !SInfo)
        )
    ;
        ( !.SInfo ^ colon ^ elem(StateVar) = Var0 ->
            Var = Var0,
            !:SInfo = !.SInfo `with_updated_svar` StateVar
        ;
            Var = StateVar,
            % Set up a dummy mapping: there's no point
            % in mentioning this error twice.
            !:SInfo = ( !.SInfo ^ colon ^ elem(StateVar) := Var ),
            ( !.SInfo ^ external_dot `contains` StateVar ->
                PError = report_illegal_state_var_update
            ;
                PError = report_non_visible_state_var(":")
            ),
            PError(Context, !.VarSet, StateVar, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

    % Construct the initial and final mappings for a state variable.
    %
:- pred new_local_state_var(svar::in, prog_var::out, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

new_local_state_var(StateVar, VarD, VarC, !VarSet, !SInfo) :-
    new_dot_state_var(StateVar, VarD, !VarSet, !SInfo),
    new_final_state_var(StateVar, VarC, !VarSet, !SInfo).

    % Construct the initial and final mappings for a state variable.
    %
:- pred new_dot_state_var(svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

new_dot_state_var(StateVar, VarD, !VarSet, !SInfo) :-
    N     = !.SInfo ^ num,
    Name  = varset.lookup_name(!.VarSet, StateVar),
    NameD = string.format("STATE_VARIABLE_%s_%d", [s(Name), i(N)]),
    varset.new_named_var(!.VarSet, NameD, VarD, !:VarSet),
    !:SInfo = ( !.SInfo ^ dot ^ elem(StateVar) := VarD ).

:- pred new_colon_state_var(svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

new_colon_state_var(StateVar, VarC, !VarSet, !SInfo) :-
    N     = !.SInfo ^ num,
    Name  = varset.lookup_name(!.VarSet, StateVar),
    NameC = string.format("STATE_VARIABLE_%s_%d", [s(Name), i(N)]),
    varset.new_named_var(!.VarSet, NameC, VarC, !:VarSet),
    !:SInfo = ( !.SInfo ^ colon ^ elem(StateVar) := VarC ).

:- pred new_final_state_var(svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

new_final_state_var(StateVar, VarC, !VarSet, !SInfo) :-
    Name  = varset.lookup_name(!.VarSet, StateVar),
    NameC = string.format("STATE_VARIABLE_%s",    [s(Name)]),
    varset.new_named_var(!.VarSet, NameC, VarC, !:VarSet),
    !:SInfo = ( !.SInfo ^ colon ^ elem(StateVar) := VarC ).

%-----------------------------------------------------------------------------%

prepare_for_head(new_svar_info).

%-----------------------------------------------------------------------------%

prepare_for_lambda(!SInfo) :-
    !:SInfo = ( new_svar_info ^ external_dot := !.SInfo ^ dot ).

%-----------------------------------------------------------------------------%

prepare_for_body(FinalMap, !VarSet, !SInfo) :-
    FinalMap  = !.SInfo ^ colon,
    N         = !.SInfo ^ num + 1,
    StateVars = list.merge_and_remove_dups(map.keys(!.SInfo ^ colon),
        map.keys(!.SInfo ^ dot)),
    next_svar_mappings(N, StateVars, !VarSet, Colon),
    !:SInfo   = !.SInfo ^ ctxt  := in_body,
    !:SInfo   = !.SInfo ^ num   := N,
    !:SInfo   = !.SInfo ^ colon := Colon.

%-----------------------------------------------------------------------------%

finish_goals(Context, FinalSVarMap, Goals0, Goal, SInfo) :-
    goal_info_init(Context, GoalInfo),
    list.map(goal_to_conj_list, Goals0, GoalsAsConjList),
    Unifiers = svar_unifiers(yes(dont_warn_singleton), Context, FinalSVarMap,
        SInfo ^ dot),
    Goals1 = list.condense(GoalsAsConjList),
    Goals  = Goals1 ++ Unifiers,
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- func svar_unifiers(maybe(goal_feature), prog_context, svar_map, svar_map)
    = hlds_goals.

svar_unifiers(MaybeFeature, Context, LHSMap, RHSMap) =
    map.foldl(add_svar_unifier(MaybeFeature, RHSMap, Context), LHSMap, []).

:- func add_svar_unifier(maybe(goal_feature), svar_map, prog_context,
    svar, prog_var, hlds_goals) = hlds_goals.

add_svar_unifier(MaybeFeature, RHSMap, Context, StateVar, Var, Unifiers0)
        = Unifiers :-
    ( RHSVar = RHSMap ^ elem(StateVar) ->
        Unifier = svar_unification(MaybeFeature, Context, Var, RHSVar),
        Unifiers = [Unifier | Unifiers0]
    ;
        Unifiers = Unifiers0
    ).

%-----------------------------------------------------------------------------%

:- func svar_unification(maybe(goal_feature), prog_context, prog_var, prog_var)
    = hlds_goal.

svar_unification(MaybeFeature, Context, SVar, Var) = Unification :-
    create_atomic_complicated_unification(SVar, var(Var), Context,
        implicit("state variable"), [], Unification0),
    (
        MaybeFeature = no,
        Unification = Unification0
    ;
        MaybeFeature = yes(Feature),
        goal_add_feature(Feature, Unification0, Unification)
    ).

%-----------------------------------------------------------------------------%

prepare_for_local_state_vars(StateVars, !VarSet, !SInfo) :-
    list.foldl2(add_new_local_state_var, StateVars, !VarSet, !SInfo).

:- pred add_new_local_state_var(svar::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

add_new_local_state_var(StateVar, !VarSet, !SInfo) :-
    new_colon_state_var(StateVar, _, !VarSet, !SInfo).

%-----------------------------------------------------------------------------%

finish_local_state_vars(StateVars, Vars, SInfoBefore, !SInfo) :-
    InitDot   = !.SInfo ^ dot,
    InitColon = !.SInfo ^ colon,
    Dots    = svar_mappings(InitDot, StateVars),
    Colons  = svar_mappings(InitColon, StateVars),
    Vars    = list.sort_and_remove_dups(Dots ++ Colons),
    !:SInfo = !.SInfo ^ dot :=
        del_locals(StateVars, SInfoBefore ^ dot, InitDot),
    !:SInfo = !.SInfo ^ colon :=
        del_locals(StateVars, SInfoBefore ^ colon, InitColon).

:- func svar_mappings(svar_map, svars) = svars.

svar_mappings(_, []) = [].
svar_mappings(Map, [StateVar | StateVars]) =
    ( Map ^ elem(StateVar) = Var ->
        [Var | svar_mappings(Map, StateVars)]
    ;
        svar_mappings(Map, StateVars)
    ).

:- func del_locals(svars, svar_map, svar_map) = svar_map.

del_locals(StateVars, MapBefore, Map) =
    list.foldl(
        func(K, M) =
            ( if   MapBefore ^ elem(K) =  V
              then M         ^ elem(K) := V
              else map.delete(M, K)
            ),
        StateVars,
        Map
    ).

%-----------------------------------------------------------------------------%

finish_if_then_else(Context, Then0, Then, Else0, Else,
        SInfo0, SInfoC, SInfoT0, SInfoE, SInfo, !VarSet) :-

        % Add unifiers to the Then arm for state variables that
        % acquired new mappings in the condition, but not in the
        % Them arm itself.  This is because the new mappings
        % appear only in a negated context.
        %
    StateVars = list.merge_and_remove_dups(map.keys(SInfoT0 ^ dot),
         map.keys(SInfoE  ^ dot)),
    Then0 = _ - GoalInfo,
    goal_to_conj_list(Then0, Thens0),
    add_then_arm_specific_unifiers(Context, StateVars,
        SInfo0, SInfoC, SInfoT0, SInfoT, Thens0, Thens, !VarSet),
    conj_list_to_goal(Thens, GoalInfo, Then1),

        % Calculate the svar_info with the highest numbered
        % mappings from each arm.
        %
    DisjSInfos = [{Then1, SInfoT}, {Else0, SInfoE}],
    SInfo      = reconciled_disj_svar_info(!.VarSet, DisjSInfos),

        % Add unifiers to each arm to ensure they both construct
        % the same final state variable mappings.
        %
    Then       = add_disj_unifiers(Context, SInfo, StateVars,
                {Then1, SInfoT}),
    Else       = add_disj_unifiers(Context, SInfo, StateVars,
                {Else0, SInfoE}).

    % If a new mapping was produced for state variable X in the
    % condition-goal (i.e. the condition refers to !:X), but not
    % in the then-goal, then we have to add a new unifier !:X = !.X
    % to the then-goal because the new mapping was created in a
    % negated context.
    %
:- pred add_then_arm_specific_unifiers(prog_context::in, svars::in,
    svar_info::in, svar_info::in, svar_info::in, svar_info::out,
    hlds_goals::in, hlds_goals::out, prog_varset::in, prog_varset::out) is det.

add_then_arm_specific_unifiers(_, [], _, _, SInfoT, SInfoT,
        Thens, Thens, VarSet, VarSet).

add_then_arm_specific_unifiers(Context, [StateVar | StateVars],
        SInfo0, SInfoC, !SInfoT, !Thens, !VarSet) :-
    (   % the condition refers to !:X, but the then-goal doesn't
        SInfoC ^ dot ^ elem(StateVar) \= SInfo0 ^ dot ^ elem(StateVar),
        !.SInfoT ^ dot ^ elem(StateVar) = SInfoC ^ dot ^ elem(StateVar)
    ->
        % add a new unifier !:X = !.X
        Dot0 = !.SInfoT ^ dot ^ det_elem(StateVar),
        new_colon_state_var(StateVar, Dot, !VarSet, !SInfoT),
        !:Thens = [svar_unification(yes(dont_warn_singleton), Context,
            Dot, Dot0) | !.Thens],
        prepare_for_next_conjunct(set.make_singleton_set(StateVar),
            !VarSet, !SInfoT)
    ;
        true
    ),
    add_then_arm_specific_unifiers(Context, StateVars,
        SInfo0, SInfoC, !SInfoT, !Thens, !VarSet).

%-----------------------------------------------------------------------------%

:- pred next_svar_mappings(int::in, svars::in,
    prog_varset::in, prog_varset::out, svar_map::out) is det.

next_svar_mappings(N, StateVars, VarSet0, VarSet, Map) :-
    next_svar_mappings_2(N, StateVars, VarSet0, VarSet, map.init, Map).

:- pred next_svar_mappings_2(int::in, svars::in,
    prog_varset::in, prog_varset::out, svar_map::in, svar_map::out) is det.

next_svar_mappings_2(_, [], !VarSet, !Map).
next_svar_mappings_2(N, [StateVar | StateVars], !VarSet, !Map) :-
    next_svar_mapping(N, StateVar, _, !VarSet, !Map),
    next_svar_mappings_2(N, StateVars, !VarSet, !Map).

%-----------------------------------------------------------------------------%

finish_negation(SInfoBefore, SInfoNeg, SInfo) :-
    SInfo = (( SInfoBefore ^ num   := SInfoNeg ^ num   )
                           ^ colon := SInfoNeg ^ colon ).

%-----------------------------------------------------------------------------%

finish_disjunction(Context, VarSet, DisjSInfos, Disjs, SInfo) :-
    SInfo      = reconciled_disj_svar_info(VarSet, DisjSInfos),
    StateVars  = map.keys(SInfo ^ dot),
    Disjs      = list.map( add_disj_unifiers(Context, SInfo, StateVars),
        DisjSInfos).

    % Each arm of a disjunction may have a different mapping for
    % !.X and/or !:X.  The reconciled svar_info for the disjunction
    % takes the highest numbered mapping for each disjunct (each
    % state variable mapping for !.X or !:X will have a name of
    % the form `STATE_VARIABLE_X_n' for some number `n'.)
    %
:- func reconciled_disj_svar_info(prog_varset, hlds_goal_svar_infos) =
    svar_info.

reconciled_disj_svar_info(_, []) = _ :-
    unexpected(this_file, "reconciled_disj_svar_info: empty disjunct list").

reconciled_disj_svar_info(VarSet, [{_, SInfo0} | DisjSInfos]) = SInfo :-

        % We compute the set of final !. and !: state variables
        % over the whole disjunction (not all arms will necessarily
        % include !. and !: mappings for all state variables).
        %
    Dots0   = set.sorted_list_to_set(map.keys(SInfo0 ^ dot)),
    Colons0 = set.sorted_list_to_set(map.keys(SInfo0 ^ colon)),
    Dots    = union_dot_svars(Dots0, DisjSInfos),
    Colons  = union_colon_svars(Colons0, DisjSInfos),

        % Then we update SInfo0 to take the highest numbered
        % !. and !: mapping for each state variable.
        %
    SInfo   = list.foldl(reconciled_svar_infos(VarSet, Dots, Colons),
        DisjSInfos, SInfo0).

:- func union_dot_svars(svar_set, hlds_goal_svar_infos) = svar_set.

union_dot_svars(Dots, []                       ) = Dots.
union_dot_svars(Dots, [{_, SInfo} | DisjSInfos]) =
    union_dot_svars(
        Dots `union` set.sorted_list_to_set(map.keys(SInfo ^ dot)),
        DisjSInfos
    ).

:- func union_colon_svars(svar_set, hlds_goal_svar_infos) = svar_set.

union_colon_svars(Colons, []                       ) = Colons.
union_colon_svars(Colons, [{_, SInfo} | DisjSInfos]) =
    union_colon_svars(
        Colons `union` set.sorted_list_to_set(map.keys(SInfo ^ colon)),
        DisjSInfos
    ).

:- func reconciled_svar_infos(prog_varset, svar_set, svar_set,
    hlds_goal_svar_info, svar_info) = svar_info.

reconciled_svar_infos(VarSet, Dots, Colons,
        {_, SInfoX}, SInfo0) = SInfo :-
    SInfo1 = set.fold(reconciled_svar_infos_dots(VarSet, SInfoX),
        Dots, SInfo0),
    SInfo2 = set.fold(reconciled_svar_infos_colons(VarSet, SInfoX),
        Colons, SInfo1),
    SInfo  = ( SInfo2 ^ num := max(SInfo0 ^ num, SInfoX ^ num) ).

:- func reconciled_svar_infos_dots(prog_varset, svar_info, svar, svar_info)
    = svar_info.

reconciled_svar_infos_dots(VarSet, SInfoX, StateVar, SInfo0) = SInfo :-
    (
        DotX = SInfoX ^ dot ^ elem(StateVar),
        Dot0 = SInfo0 ^ dot ^ elem(StateVar)
    ->
        NameX = varset.lookup_name(VarSet, DotX) `with_type` string,
        Name0 = varset.lookup_name(VarSet, Dot0) `with_type` string,
        compare_svar_names(RDot, NameX, Name0),
        (
            RDot  = (<),
            SInfo = ( SInfo0 ^ dot ^ elem(StateVar) := Dot0 )
        ;
            RDot  = (=),
            SInfo = SInfo0
        ;
            RDot  = (>),
            SInfo = ( SInfo0 ^ dot ^ elem(StateVar) := DotX )
        )
    ;
        SInfo = SInfo0
    ).

:- func reconciled_svar_infos_colons(prog_varset, svar_info, svar, svar_info)
    = svar_info.

reconciled_svar_infos_colons(VarSet, SInfoX, StateVar, SInfo0) = SInfo :-
    (
        ColonX = SInfoX ^ colon ^ elem(StateVar),
        Colon0 = SInfo0 ^ colon ^ elem(StateVar)
    ->
        NameX = varset.lookup_name(VarSet, ColonX) `with_type` string,
        Name0 = varset.lookup_name(VarSet, Colon0) `with_type` string,
        compare_svar_names(RColon, NameX, Name0),
        (
            RColon = (<),
            SInfo  = ( SInfo0 ^ colon ^ elem(StateVar) := Colon0 )
        ;
            RColon = (=),
            SInfo  = SInfo0
        ;
            RColon = (>),
            SInfo  = ( SInfo0 ^ colon ^ elem(StateVar) := ColonX )
        )
    ;
        SInfo = SInfo0
    ).

:- func add_disj_unifiers(prog_context, svar_info, svars, hlds_goal_svar_info)
    = hlds_goal.

add_disj_unifiers(Context, SInfo, StateVars, {GoalX, SInfoX}) = Goal :-
    Unifiers = list.foldl(add_disj_unifier(Context, SInfo, SInfoX),
        StateVars, []),
    GoalX = _ - GoalInfo,
    goal_to_conj_list(GoalX, GoalsX),
    conj_list_to_goal(GoalsX ++ Unifiers, GoalInfo, Goal).

:- func add_disj_unifier(prog_context, svar_info, svar_info, svar, hlds_goals)
    = hlds_goals.

add_disj_unifier(Context, SInfo, SInfoX, StateVar, Unifiers) =
    (
        Dot  = SInfo  ^ dot ^ elem(StateVar),
        DotX = SInfoX ^ dot ^ elem(StateVar),
        Dot \= DotX
    ->
        [svar_unification(yes(dont_warn_singleton), Context, Dot, DotX)
            | Unifiers]
    ;
        Unifiers
    ).

%-----------------------------------------------------------------------------%

    % We implement a special purpose comparison for state variable
    % names that compares the numbers appended at the right hand
    % ends of the name strings.
    %
    % NOTE state variable names are either "..._X" or "..._X_N"
    % where X is the name of the program variable used for the
    % state variable and N is a decimal number with no leading
    % zeroes.
    %
:- pred compare_svar_names(comparison_result::out, string::in, string::in)
    is det.

compare_svar_names(R, A, B) :-
    compare(R, int_suffix_of(A), int_suffix_of(B)).

    % Find the number suffix at the end of a string as an int.
    %
:- func int_suffix_of(string) = int.

int_suffix_of(S) = int_suffix_2(S, length(S) - 1, 1, 0).

    % int_suffix_2(String, Index, RadixOfIndexDigit, IntSoFar) = IntSuffix
    %
:- func int_suffix_2(string, int, int, int) = int.

int_suffix_2(S, I, R, N) =
    (
        0 =< I,
        digit_to_int(S `unsafe_index` I, D),
        D < 10
    ->
        int_suffix_2(S, I - 1, 10 * R, (R * D) + N)
    ;
        N
    ).

%-----------------------------------------------------------------------------%

finish_equivalence(SInfoBefore, SInfoEqv, SInfo) :-
    finish_negation(SInfoBefore, SInfoEqv, SInfo).

%-----------------------------------------------------------------------------%

prepare_for_call(ParentSInfo, SInfo) :-
    ( ParentSInfo ^ ctxt = in_atom(UpdatedStateVars, _GrandparentSInfo) ->
        Ctxt = in_atom(UpdatedStateVars, ParentSInfo)
    ;
        Ctxt = in_atom(set.init, ParentSInfo)
    ),
    SInfo = ParentSInfo ^ ctxt := Ctxt.

%-----------------------------------------------------------------------------%

finish_call(!VarSet, !SInfo) :-
    ( !.SInfo ^ ctxt = in_atom(UpdatedStateVars, ParentSInfo0) ->
        ParentSInfo = ( ParentSInfo0 ^ dot := !.SInfo ^ dot ),
        ( ParentSInfo ^ ctxt = in_atom(_, GrandParentSInfo) ->
            !:SInfo  = ( ParentSInfo ^ ctxt :=
                in_atom(UpdatedStateVars, GrandParentSInfo) )
        ;
            prepare_for_next_conjunct(UpdatedStateVars, !VarSet, ParentSInfo,
                !:SInfo)
        )
    ;
        unexpected(this_file, "finish_call: ctxt is not in_atom")
    ).

%-----------------------------------------------------------------------------%

prepare_for_if_then_else_goal(StateVars, !VarSet, !SInfo) :-
    prepare_for_local_state_vars(StateVars, !VarSet, !SInfo).

%-----------------------------------------------------------------------------%

finish_if_then_else_goal_condition(StateVars, SInfoBefore, SInfoA0, SInfoA,
        SInfoB) :-
    SInfoB = SInfoA0,
    finish_local_state_vars(StateVars, _, SInfoBefore, SInfoA0, SInfoA).

%-----------------------------------------------------------------------------%

finish_if_then_else_goal_then_goal(StateVars, SInfoBefore, SInfoB0, SInfoB) :-
    finish_local_state_vars(StateVars, _, SInfoBefore, SInfoB0, SInfoB).

%-----------------------------------------------------------------------------%

prepare_for_if_then_else_expr(StateVars, !VarSet, !SInfo) :-
    SInfo0 = !.SInfo,
    !:SInfo = new_svar_info ^ ctxt := in_body,
    !:SInfo = !.SInfo ^ external_dot := SInfo0 ^ dot,
    !:SInfo = !.SInfo ^ num := SInfo0 ^ num,
    prepare_for_local_state_vars(StateVars, !VarSet, !SInfo).

%-----------------------------------------------------------------------------%

finish_if_then_else_expr_condition(Before, !SInfo) :-
    SInfo0 = !.SInfo,
    !:SInfo = !.SInfo ^ external_dot := Before ^ external_dot,
    !:SInfo = !.SInfo ^ dot := (SInfo0 ^ dot) `overlay` (Before ^ dot),
    !:SInfo = !.SInfo ^ colon := (SInfo0 ^ colon) `overlay` (Before ^ colon),
    !:SInfo = !.SInfo ^ ctxt := Before ^ ctxt.

%-----------------------------------------------------------------------------%

finish_if_then_else_expr_then_goal(StateVars, SInfoBefore, !SInfo) :-
    finish_local_state_vars(StateVars, _, SInfoBefore, !SInfo).

%-----------------------------------------------------------------------------%

prepare_for_next_conjunct(UpdatedStateVars, !VarSet, !SInfo) :-
    Dot0   = !.SInfo ^ dot,
    Colon0 = !.SInfo ^ colon,
    N      = !.SInfo ^ num + 1,
    map.init(Nil),
    map.foldl(next_dot_mapping(UpdatedStateVars, Dot0, Colon0), Colon0,
        Nil, Dot),
    map.foldl2(next_colon_mapping(UpdatedStateVars, Colon0, N), Colon0,
        !VarSet, Nil, Colon),
    !:SInfo  = !.SInfo ^ ctxt  := in_body,
    !:SInfo  = !.SInfo ^ num   := N,
    !:SInfo  = !.SInfo ^ dot   := Dot,
    !:SInfo  = !.SInfo ^ colon := Colon.

    % If the state variable has been updated (i.e. there was a !:X
    % reference) then the next !.X mapping will be the current !:X
    % mapping.
    % Otherwise, preserve the current !.X mapping, if any (there
    % may be none if, for example, the head only references !:X
    % and there have been no prior references to !:X in the body.)
    %
:- pred next_dot_mapping(svar_set::in, svar_map::in, svar_map::in, svar::in,
    prog_var::in, svar_map::in, svar_map::out) is det.

next_dot_mapping(UpdatedStateVars, OldDot, OldColon, StateVar, _, Dot0, Dot) :-
    ( UpdatedStateVars `contains` StateVar ->
        Var = OldColon ^ det_elem(StateVar),
        Dot = ( Dot0 ^ elem(StateVar) := Var )
    ; Var = OldDot ^ elem(StateVar) ->
        Dot = ( Dot0 ^ elem(StateVar) := Var )
    ;
        Dot = Dot0
    ).

    % If the state variable has been updated (i.e. there was a !:X
    % reference) then create a new mapping for the next !:X.
    % Otherwise, the next !:X mapping is the same as the current
    % !:X mapping.
    %
:- pred next_colon_mapping(svar_set::in, svar_map::in, int::in, svar::in,
    prog_var::in, prog_varset::in, prog_varset::out,
    svar_map::in, svar_map::out) is det.

next_colon_mapping(UpdatedStateVars, OldColon, N, StateVar, _,
        !VarSet, !Colon) :-
    ( UpdatedStateVars `contains` StateVar ->
        next_svar_mapping(N, StateVar, _Var, !VarSet, !Colon)
    ;
        !:Colon = ( !.Colon ^ elem(StateVar) := OldColon ^ det_elem(StateVar) )
    ).

:- pred next_svar_mapping(int::in, svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_map::in, svar_map::out) is det.

next_svar_mapping(N, StateVar, Var, !VarSet, !Map) :-
    Name = string.format("STATE_VARIABLE_%s_%d",
        [s(varset.lookup_name(!.VarSet, StateVar)), i(N)]),
    varset.new_named_var(!.VarSet, Name, Var, !:VarSet),
    !:Map  = ( !.Map ^ elem(StateVar) := Var ).

%-----------------------------------------------------------------------------%

expand_bang_state_var_args(Args) =
    list.foldr(expand_bang_state_var, Args, []).

:- func expand_bang_state_var(prog_term, list(prog_term)) = list(prog_term).

expand_bang_state_var(T @ variable(_), Ts) = [T | Ts].

expand_bang_state_var(T @ functor(Const, Args, Ctxt), Ts) =
    (
        Const = atom("!"),
        Args = [variable(_StateVar)]
    ->
        [functor(atom("!."), Args, Ctxt), functor(atom("!:"), Args, Ctxt) | Ts]
    ;
        [T | Ts]
    ).

%-----------------------------------------------------------------------------%

expand_bang_state_var_args_in_instance_method_heads(abstract) = abstract.

expand_bang_state_var_args_in_instance_method_heads(concrete(Methods)) =
    concrete(list.map(expand_method_bsvs, Methods)).

:- func expand_method_bsvs(instance_method) = instance_method.

expand_method_bsvs(IM) = IM :-
    IM = instance_method(_, _, name(_), _, _).

expand_method_bsvs(IM0) = IM :-
    IM0 = instance_method(PredOrFunc, Method, clauses(Cs0), Arity0, Ctxt),
    Cs  = list.map(expand_item_bsvs, Cs0),
        % Note that the condition should always succeed...
        %
    ( Cs = [clause(_, _, _, _, Args, _) | _] ->
        adjust_func_arity(PredOrFunc, Arity, list.length(Args))
    ;
        Arity = Arity0
    ),
    IM  = instance_method(PredOrFunc, Method, clauses(Cs), Arity, Ctxt).

    % The instance method clause items will all be clause items.
    %
:- func expand_item_bsvs(item) = item.

expand_item_bsvs(Item) =
    ( Item = clause(Origin, VarSet, PredOrFunc, SymName, Args, Body) ->
        clause(Origin, VarSet, PredOrFunc, SymName,
            expand_bang_state_var_args(Args), Body)
    ;
        Item
    ).

%-----------------------------------------------------------------------------%

substitute_state_var_mappings([], [], !VarSet, !SInfo, !IO).
substitute_state_var_mappings([Arg0 | Args0], [Arg | Args],
        !VarSet, !SInfo, !IO) :-
    substitute_state_var_mapping(Arg0, Arg, !VarSet, !SInfo, !IO),
    substitute_state_var_mappings(Args0, Args, !VarSet, !SInfo, !IO).

substitute_state_var_mapping(Arg0, Arg, !VarSet, !SInfo, !IO) :-
    (
        Arg0 = functor(atom("!."), [variable(StateVar)], Context)
    ->
        dot(Context, StateVar, Var, !VarSet, !SInfo, !IO),
        Arg  = variable(Var)
    ;
        Arg0 = functor(atom("!:"), [variable(StateVar)], Context)
    ->
        colon(Context, StateVar, Var, !VarSet, !SInfo, !IO),
        Arg  = variable(Var)
    ;
        Arg  = Arg0
    ).

%-----------------------------------------------------------------------------%

illegal_state_var_func_result(function, Args, StateVar) :-
    list.last(Args, functor(atom("!"), [variable(StateVar)], _Ctxt)).

%-----------------------------------------------------------------------------%

lambda_args_contain_bang_state_var([Arg | Args], StateVar) :-
    ( Arg      = functor(atom("!"), [variable(StateVar0)], _) ->
        StateVar = StateVar0
    ;
        lambda_args_contain_bang_state_var(Args, StateVar)
    ).

%-----------------------------------------------------------------------------%

report_illegal_state_var_update(Context, VarSet, StateVar, !IO) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error: cannot use"), fixed("!:" ++ Name),
        words("in this context;"), nl,
        words("however"), fixed("!." ++ Name), words("may be used here.")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

:- pred report_non_visible_state_var(string::in, prog_context::in,
    prog_varset::in, svar::in, io::di, io::uo) is det.

report_non_visible_state_var(DorC, Context, VarSet, StateVar, !IO) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error: state variable"),
        fixed("!" ++ DorC ++ Name), words("is not visible in this context.")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

:- pred report_unitialized_state_var(prog_context::in, prog_varset::in,
    svar::in, io::di, io::uo) is det.

report_unitialized_state_var(Context, VarSet, StateVar, !IO) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: reference to unitialized state variable"),
        fixed("!." ++ Name), suffix("."), nl],
    write_error_pieces(Context, 0, Pieces, !IO),
    record_warning(!IO).

%-----------------------------------------------------------------------------%

report_illegal_func_svar_result(Context, VarSet, StateVar, !IO) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:"), fixed("!" ++ Name),
        words("cannot be a function result."), nl,
        words("You probably meant"), fixed("!." ++ Name),
        words("or"), fixed("!:" ++ Name), suffix("."), nl],
    write_error_pieces(Context, 0, Pieces, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

report_illegal_bang_svar_lambda_arg(Context, VarSet, StateVar, !IO) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:"), fixed("!" ++ Name),
        words("cannot be a lambda argument."), nl,
        words("Perhaps you meant"), fixed("!." ++ Name),
        words("or"), fixed("!:" ++ Name), suffix("."), nl],
    write_error_pieces(Context, 0, Pieces, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "state_var.m".

%-----------------------------------------------------------------------------%
