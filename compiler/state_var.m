%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: state_var.m.
% Main author: rafe.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.state_var.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module set.

%-----------------------------------------------------------------------------%

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
                svar_ctxt           ::  svar_ctxt,

                % This is used to number state variables and is incremented
                % for each source-level conjunct.
                svar_num            ::  int,

                % The "read only" state variables in scope (e.g. external state
                % variables visible from within a lambda body or condition
                % of an if-then-else expression.)
                svar_readonly_dot   ::  svar_map,

                % The "read/write" state variables in scope.
                svar_dot            ::  svar_map,
                svar_colon          ::  svar_map
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
    % If we are processing the head of a clause or lambda, we incrementally
    % accumulate the mappings.
    %
    % Otherwise, the mapping must already be present for a local or `external'
    % state variable (i.e. one that may be visible, but not updatable, in the
    % current context.)
    %
    % Note that if !.X does not appear in the head then !:X must appear
    % before !.X can be referenced.
    %
:- pred svar_dot(prog_context::in, svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Obtain the mapping for a !:X state variable reference.
    %
    % If we are processing the head of a clause or lambda, we incrementally
    % accumulate the mappings.
    %
    % Otherwise, the mapping must already be present for a local state variable
    % (`externally' visible state variables cannot be updated.)
    %
    % We also keep track of which state variables have been updated
    % in an atomic context.
    %
:- pred svar_colon(prog_context::in, svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Prepare for the head of a new clause.
    %
:- pred svar_prepare_for_head(svar_info::out) is det.

    % We need to add the current !.Xs to the set of external ("read-only")
    % state variables and clear the !.Xs and !:Xs.
    %
    % While processing the head, any state variables therein are implicitly
    % scoped over the body and have !. and !: mappings set up.
    %
:- pred svar_prepare_for_lambda(svar_info::in, svar_info::out) is det.

    % Having processed the head of a clause, prepare for the first
    % (source-level) atomic conjunct.  We return the final !:
    % mappings identified while processing the head.
    %
:- pred svar_prepare_for_body(svar_map::out, prog_varset::in, prog_varset::out,
    svar_info::in, svar_info::out) is det.

    % We have to conjoin the goals and add unifiers to tie up all
    % the final values of the state variables to the head variables.
    %
:- pred svar_finish_goals(prog_context::in, svar_map::in,
    list(hlds_goal)::in, hlds_goal::out, svar_info::in) is det.

    % Add some local state variables.
    %
:- pred prepare_for_local_state_vars(svars::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

    % Remove some local state variables.
    %
:- pred finish_local_state_vars(svars::in, prog_vars::out,
    svar_info::in, svar_info::in, svar_info::out) is det.

:- type svar_outer_atomic_scope_info.

    % svar_start_outer_atomic_scope(Context, OuterStateVar, OuterDI, OuterUO,
    %   OuterScopeInfo, !VarSet, !SInfo, !Specs):
    %
    % This predicate converts a !OuterStateVar specification in an atomic scope
    % to a pair of outer state variables, OuterDI and OuterUO. Since
    % !OuterStateVar should *not* be accessible inside the atomic scope,
    % we delete it, but record it in OuterScopeInfo. The accessibility of
    % !OuterStateVar will be restored when you call svar_finish_atomic_scope
    % with OuterScopeInfo.
    %  
:- pred svar_start_outer_atomic_scope(prog_context::in, prog_var::in,
    prog_var::out, prog_var::out, svar_outer_atomic_scope_info::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % svar_finish_outer_atomic_scope(OuterScopeInfo, !SInfo):
    %
    % Restore the accessibility of !OuterStateVar that was disabled by
    % svar_start_atomic_scope.
    %
:- pred svar_finish_outer_atomic_scope(svar_outer_atomic_scope_info::in,
    svar_info::in, svar_info::out) is det.

:- type svar_inner_atomic_scope_info.

    % svar_start_inner_atomic_scope(Context, InnerStateVar, InnerScopeInfo,
    %   !VarSet, !SInfo, !Specs):
    %
    % This predicate prepares for an atomic scope with an !InnerStateVar
    % specification by making that state var available.
    %
:- pred svar_start_inner_atomic_scope(prog_context::in, prog_var::in,
    svar_inner_atomic_scope_info::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % svar_finish_inner_atomic_scope(Context, InnerScopeInfo, InnerDI, InnerUO,
    %   !VarSet, !SInfo, !Specs):
    %
    % This predicate ends an atomic scope with an !InnerStateVar
    % specification by making that state var unavailable, and returning
    % the two variables InnerDI and InnerUO representing the initial and final
    % states of this state variable.
    %
:- pred svar_finish_inner_atomic_scope(prog_context::in,
    svar_inner_atomic_scope_info::in, prog_var::out, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % We have to add unifiers to the Then and Else arms of an
    % if-then-else to make sure all the state variables match up.
    %
    % More to the point, we have to add unifiers to the Then arm
    % for any new state variable mappings produced in the condition.
    %
    % We construct new mappings for the state variables and then
    % add unifiers.
    %
:- pred svar_finish_if_then_else(prog_context::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    svar_info::in, svar_info::in, svar_info::in, svar_info::in, svar_info::out,
    prog_varset::in, prog_varset::out) is det.

:- pred svar_finish_if_then_else_goal_condition(svars::in,
    svar_info::in, svar_info::in, svar_info::out, svar_info::out) is det.

:- pred svar_finish_if_then_else_expr_condition(svar_info::in,
    svar_info::in, svar_info::out) is det.

:- pred svar_finish_if_then_else_expr_then_goal(svars::in,
    svar_info::in, svar_info::in, svar_info::out) is det.

    % We assume that a negation updates all state variables in scope,
    % so we construct new mappings for the state variables and then
    % add unifiers from their pre-negated goal mappings.
    %
:- pred svar_finish_negation(svar_info::in, svar_info::in, svar_info::out)
    is det.

    % We have to make sure that all arms of a disjunction produce the
    % same state variable bindings by adding unifiers as necessary.
    %
:- pred svar_finish_disjunction(prog_context::in, prog_varset::in,
    hlds_goal_svar_infos::in, list(hlds_goal)::out, svar_info::out) is det.

    % We treat equivalence goals as if they were negations (they are
    % in a negated context after all.)
    %
:- pred svar_finish_equivalence(svar_info::in, svar_info::in, svar_info::out)
    is det.

    % We prepare for a call by setting the ctxt to in_atom.  If we're
    % already in an atom then we inherit the parent's set of "updated"
    % state variables.
    %
:- pred svar_prepare_for_call(svar_info::in, svar_info::out) is det.

    % When we finish a call, we're either still inside the
    % atomic formula, in which case we simply propagate the set of
    % "updated" state variables, or we've just emerged, in which case
    % we need to set up the svar_info for the next conjunct.
    %
    % (We can still be in an atomic context if, for example, we've
    % been processing a function call which must appear as an
    % expression and hence occur inside an atomic context.)
    %
:- pred svar_finish_call(prog_varset::in, prog_varset::out,
    svar_info::in, svar_info::out) is det.

:- pred svar_prepare_for_if_then_else_goal(svars::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

:- pred svar_finish_if_then_else_goal_then_goal(svars::in,
    svar_info::in, svar_info::in, svar_info::out) is det.

    % The condition of an if-then-else expression is a goal in which
    % only !.X state variables in scope are visible (although the goal
    % may use local state variables introduced via an explicit quantifier.)
    % The StateVars are local to the condition and then-goal.
    %
:- pred svar_prepare_for_if_then_else_expr(svars::in,
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
:- pred svar_prepare_for_next_conjunct(svar_set::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

    % Given a list of argument terms, substitute !.X and !:X with
    % the corresponding state variable mappings.  Any !X should
    % already have been expanded into !.X, !:X via a call to
    % expand_bang_state_var_args/1.
    %
:- pred substitute_state_var_mappings(list(prog_term)::in,
    list(prog_term)::out, prog_varset::in, prog_varset::out,
    svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred substitute_state_var_mapping(prog_term::in, prog_term::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

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
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

:- pred report_illegal_func_svar_result(prog_context::in, prog_varset::in,
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

:- pred report_illegal_bang_svar_lambda_arg(prog_context::in, prog_varset::in,
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.prog_util.

:- import_module char.
:- import_module int.
:- import_module pair.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

new_svar_info = svar_info(in_head, 0, map.init, map.init, map.init).

:- pred svar_info `has_svar_colon_mapping_for` svar.
:- mode in `has_svar_colon_mapping_for` in is semidet.

SInfo `has_svar_colon_mapping_for` StateVar :-
    SInfo ^ svar_colon `contains` StateVar.
SInfo `has_svar_colon_mapping_for` StateVar :-
    SInfo ^ svar_ctxt = in_atom(_, ParentSInfo),
    ParentSInfo `has_svar_colon_mapping_for` StateVar.

:- func svar_info `with_updated_svar` svar = svar_info.

SInfo `with_updated_svar` StateVar =
    ( SInfo ^ svar_ctxt =  in_atom(UpdatedStateVars, ParentSInfo) ->
        SInfo ^ svar_ctxt := in_atom(set.insert(UpdatedStateVars, StateVar),
            ParentSInfo)
    ;
        SInfo
    ).

%-----------------------------------------------------------------------------%

svar_dot(Context, StateVar, Var, !VarSet, !SInfo, !Specs) :-
    SVarContext = !.SInfo ^ svar_ctxt,
    DotMap = !.SInfo ^ svar_dot,
    (
        SVarContext = in_head,
        ( map.search(DotMap, StateVar, VarPrime) ->
            Var = VarPrime
        ;
            new_dot_state_var(StateVar, Var, !VarSet, !SInfo)
        )
    ;
        ( SVarContext = in_body
        ; SVarContext = in_atom(_, _)
        ),
        ( map.search(DotMap, StateVar, VarPrime) ->
            Var = VarPrime
        ; map.search(!.SInfo ^ svar_readonly_dot, StateVar, VarPrime) ->
            Var = VarPrime
        ; !.SInfo `has_svar_colon_mapping_for` StateVar ->
            new_dot_state_var(StateVar, Var, !VarSet, !SInfo),
            report_uninitialized_state_var(Context, !.VarSet, StateVar, !Specs)
        ;
            Var = StateVar,
            report_non_visible_state_var(".", Context, !.VarSet, StateVar,
                !Specs)
        )
    ).

%-----------------------------------------------------------------------------%

svar_colon(Context, StateVar, Var, !VarSet, !SInfo, !Specs) :-
    SVarContext = !.SInfo ^ svar_ctxt,
    ColonMap0 = !.SInfo ^ svar_colon,
    (
        SVarContext = in_head,
        ( map.search(ColonMap0, StateVar, VarPrime) ->
            Var = VarPrime
        ;
            new_final_state_var(StateVar, Var, !VarSet, !SInfo)
        )
    ;
        ( SVarContext = in_body
        ; SVarContext = in_atom(_, _)
        ),
        ( map.search(ColonMap0, StateVar, VarPrime) ->
            Var = VarPrime,
            !:SInfo = !.SInfo `with_updated_svar` StateVar
        ;
            % Return a dummy variable, and set up a dummy mapping: there is
            % no point in mentioning this error twice.
            Var = StateVar,
            map.det_insert(ColonMap0, StateVar, Var, ColonMap),
            !SInfo ^ svar_colon := ColonMap,
            ( map.contains(!.SInfo ^ svar_readonly_dot, StateVar) ->
                report_illegal_state_var_update(Context, !.VarSet, StateVar,
                    !Specs)
            ;
                report_non_visible_state_var(":", Context, !.VarSet, StateVar,
                    !Specs)
            )
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
    N     = !.SInfo ^ svar_num,
    Name  = varset.lookup_name(!.VarSet, StateVar),
    NameD = string.format("STATE_VARIABLE_%s_%d", [s(Name), i(N)]),
    varset.new_named_var(!.VarSet, NameD, VarD, !:VarSet),
    !:SInfo = ( !.SInfo ^ svar_dot ^ elem(StateVar) := VarD ).

:- pred new_colon_state_var(svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

new_colon_state_var(StateVar, VarC, !VarSet, !SInfo) :-
    N     = !.SInfo ^ svar_num,
    Name  = varset.lookup_name(!.VarSet, StateVar),
    NameC = string.format("STATE_VARIABLE_%s_%d", [s(Name), i(N)]),
    varset.new_named_var(!.VarSet, NameC, VarC, !:VarSet),
    !:SInfo = ( !.SInfo ^ svar_colon ^ elem(StateVar) := VarC ).

:- pred new_final_state_var(svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out) is det.

new_final_state_var(StateVar, VarC, !VarSet, !SInfo) :-
    Name  = varset.lookup_name(!.VarSet, StateVar),
    NameC = string.format("STATE_VARIABLE_%s",    [s(Name)]),
    varset.new_named_var(!.VarSet, NameC, VarC, !:VarSet),
    !:SInfo = ( !.SInfo ^ svar_colon ^ elem(StateVar) := VarC ).

%-----------------------------------------------------------------------------%

svar_prepare_for_head(new_svar_info).

%-----------------------------------------------------------------------------%

svar_prepare_for_lambda(!SInfo) :-
    % Construct the new readonly_dots mapping by overlaying the current dots
    % mapping onto the existing readonly_dots mapping.  We cannot just throw
    % the existing readonly_dots mapping away because otherwise referring to
    % externals from within closures that are nested more than one level deep
    % will not work.
    NewExternals =
        map.overlay(!.SInfo ^ svar_readonly_dot, !.SInfo ^ svar_dot),
    !:SInfo = new_svar_info,
    !SInfo ^ svar_readonly_dot := NewExternals.

%-----------------------------------------------------------------------------%

svar_prepare_for_body(FinalMap, !VarSet, !SInfo) :-
    FinalMap  = !.SInfo ^ svar_colon,
    N         = !.SInfo ^ svar_num + 1,
    ColonKeys = map.keys(!.SInfo ^ svar_colon),
    DotKeys   = map.keys(!.SInfo ^ svar_dot),
    StateVars = list.merge_and_remove_dups(ColonKeys, DotKeys),
    next_svar_mappings(N, StateVars, !VarSet, Colon),
    !:SInfo   = !.SInfo ^ svar_ctxt  := in_body,
    !:SInfo   = !.SInfo ^ svar_num   := N,
    !:SInfo   = !.SInfo ^ svar_colon := Colon.

%-----------------------------------------------------------------------------%

svar_finish_goals(Context, FinalSVarMap, Goals0, Goal, SInfo) :-
    goal_info_init(Context, GoalInfo),
    list.map(goal_to_conj_list, Goals0, GoalsAsConjList),
    Unifiers = svar_unifiers(yes(feature_dont_warn_singleton), Context,
        FinalSVarMap, SInfo ^ svar_dot),
    Goals1 = list.condense(GoalsAsConjList),
    Goals  = Goals1 ++ Unifiers,
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- func svar_unifiers(maybe(goal_feature), prog_context, svar_map, svar_map)
    = list(hlds_goal).

svar_unifiers(MaybeFeature, Context, LHSMap, RHSMap) = Unifiers :-
    map.foldl(add_svar_unifier(MaybeFeature, RHSMap, Context), LHSMap,
        [], Unifiers).

:- pred add_svar_unifier(maybe(goal_feature)::in, svar_map::in,
    prog_context::in, svar::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

add_svar_unifier(MaybeFeature, RHSMap, Context, StateVar, Var, !Unifiers) :-
    ( map.search(RHSMap, StateVar, RHSVar) ->
         Unifier = svar_unification(MaybeFeature, Context, Var, RHSVar),
        !:Unifiers = [Unifier | !.Unifiers]
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- func svar_unification(maybe(goal_feature), prog_context, prog_var, prog_var)
    = hlds_goal.

svar_unification(MaybeFeature, Context, SVar, Var) = Unification :-
    create_pure_atomic_complicated_unification(SVar, rhs_var(Var), Context,
        umc_implicit("state variable"), [], Unification0),
    (
        MaybeFeature = no,
        Unification = Unification0
    ;
        MaybeFeature = yes(Feature),
        goal_add_feature(Feature, Unification0, Unification)
    ).

%-----------------------------------------------------------------------------%

prepare_for_local_state_vars(StateVars, !VarSet, !SInfo) :-
    list.map_foldl2(new_colon_state_var, StateVars, _, !VarSet, !SInfo).

%-----------------------------------------------------------------------------%

finish_local_state_vars(StateVars, Vars, SInfoBefore, !SInfo) :-
    CurDotMap   = !.SInfo ^ svar_dot,
    CurColonMap = !.SInfo ^ svar_colon,
    DotVars    = svar_mappings(CurDotMap, StateVars),
    ColonVars  = svar_mappings(CurColonMap, StateVars),
    Vars    = list.sort_and_remove_dups(DotVars ++ ColonVars),
    NewDotMap   = del_locals(StateVars, SInfoBefore ^ svar_dot, CurDotMap),
    NewColonMap = del_locals(StateVars, SInfoBefore ^ svar_colon, CurColonMap),
    !SInfo ^ svar_dot   := NewDotMap,
    !SInfo ^ svar_colon := NewColonMap.

:- func svar_mappings(svar_map, svars) = svars.

svar_mappings(_, []) = [].
svar_mappings(Map, [StateVar | StateVars]) =
    ( map.search(Map, StateVar, Var) ->
        [Var | svar_mappings(Map, StateVars)]
    ;
        svar_mappings(Map, StateVars)
    ).

:- func del_locals(svars, svar_map, svar_map) = svar_map.

del_locals(StateVars, MapBefore, Map) =
    list.foldl(
        func(K, M0) = M :-
            ( map.search(MapBefore, K, V) ->
                map.set(M0, K, V, M)
            ;
                map.delete(M0, K, M)
            ),
        StateVars,
        Map
    ).

%-----------------------------------------------------------------------------%

:- type svar_outer_atomic_scope_info
    --->    svar_outer_atomic_scope_info(
                outer_state_var             :: prog_var,
                maybe_outer_read_only_dot   :: maybe(prog_var),
                maybe_outer_dot             :: maybe(prog_var),
                maybe_outer_colon           :: maybe(prog_var)
            ).

svar_start_outer_atomic_scope(Context, OuterStateVar, OuterDI, OuterUO,
        OuterScopeInfo, !VarSet, !SInfo, !Specs) :-
    svar_prepare_for_call(!SInfo),
    svar_dot(Context, OuterStateVar, OuterDI, !VarSet, !SInfo, !Specs),
    svar_colon(Context, OuterStateVar, OuterUO, !VarSet, !SInfo, !Specs),
    svar_finish_call(!VarSet, !SInfo),
    !.SInfo = svar_info(SVarContext, SVarNum, RODotMap0, DotMap0, ColonMap0),
    ( map.remove(RODotMap0, OuterStateVar, OuterRODot, RODotMap1) ->
        MaybeOuterRODot = yes(OuterRODot),
        RODotMap = RODotMap1
    ;
        MaybeOuterRODot = no,
        RODotMap = RODotMap0
    ),
    ( map.remove(DotMap0, OuterStateVar, OuterDot, DotMap1) ->
        MaybeOuterDot = yes(OuterDot),
        DotMap = DotMap1
    ;
        MaybeOuterDot = no,
        DotMap = DotMap0
    ),
    ( map.remove(ColonMap0, OuterStateVar, OuterColon, ColonMap1) ->
        MaybeOuterColon = yes(OuterColon),
        ColonMap = ColonMap1
    ;
        MaybeOuterColon = no,
        ColonMap = ColonMap0
    ),
    OuterScopeInfo = svar_outer_atomic_scope_info(OuterStateVar,
        MaybeOuterRODot, MaybeOuterDot, MaybeOuterColon),
    !:SInfo = svar_info(SVarContext, SVarNum, RODotMap, DotMap, ColonMap).

svar_finish_outer_atomic_scope(OuterScopeInfo, !SInfo) :-
    OuterScopeInfo = svar_outer_atomic_scope_info(OuterStateVar,
        MaybeOuterRODot, MaybeOuterDot, MaybeOuterColon),
    !.SInfo = svar_info(SVarContext, SVarNum, RODotMap0, DotMap0, ColonMap0),
    % For each of the "yes" cases below, we deleted the corresponding entry
    % in svar_start_atomic_scope. While a goal inside the atomic state could
    % have introduced a state variable with the same name again, that could
    % have been done only in a scope which also deletes the state variable.
    % Hence the use of det_inserts below.
    (
        MaybeOuterRODot = yes(OuterRODot),
        map.det_insert(RODotMap0, OuterStateVar, OuterRODot, RODotMap)
    ;
        MaybeOuterRODot = no,
        RODotMap = RODotMap0
    ),
    (
        MaybeOuterDot = yes(OuterDot),
        map.det_insert(DotMap0, OuterStateVar, OuterDot, DotMap)
    ;
        MaybeOuterDot = no,
        DotMap = DotMap0
    ),
    (
        MaybeOuterColon = yes(OuterColon),
        map.det_insert(ColonMap0, OuterStateVar, OuterColon, ColonMap)
    ;
        MaybeOuterColon = no,
        ColonMap = ColonMap0
    ),
    !:SInfo = svar_info(SVarContext, SVarNum, RODotMap, DotMap, ColonMap).

%-----------------------------------------------------------------------------%

:- type svar_inner_atomic_scope_info
    --->    svar_inner_atomic_scope_info(
                inner_state_var             :: prog_var,
                inner_di_var                :: prog_var,
                before_svar_info            :: svar_info
            ).

svar_start_inner_atomic_scope(Context, InnerStateVar, InnerScopeInfo,
        !VarSet, !SInfo, !Specs) :-
    prepare_for_local_state_vars([InnerStateVar], !VarSet, !SInfo),
    % This mention of !:InnerStateVar is to allow code in the atomic scope
    % to access !.InnerStateVar.
    svar_colon(Context, InnerStateVar, InnerDI, !VarSet, !SInfo, !Specs),
    InnerScopeInfo = svar_inner_atomic_scope_info(InnerStateVar, InnerDI,
        !.SInfo).

svar_finish_inner_atomic_scope(Context, InnerScopeInfo, InnerDI, InnerUO,
        !VarSet, !SInfo, !Specs) :-
    InnerScopeInfo = svar_inner_atomic_scope_info(InnerStateVar, InnerDI,
        BeforeSInfo),
    % XXX Should this be svar_dot?
    svar_colon(Context, InnerStateVar, InnerUO, !VarSet, !SInfo, !Specs),
    finish_local_state_vars([InnerStateVar], Vars, BeforeSInfo, !SInfo),
    trace [compiletime(flag("atomic_scope_syntax")), io(!IO)] (
        ( Vars = [Var1, Var2] ->
            io.write_string("dot/colon:\n", !IO),
            io.write(InnerDI, !IO),
            io.nl(!IO),
            io.write(InnerUO, !IO),
            io.nl(!IO),
            io.write_string("finish", !IO),
            io.write(Var1, !IO),
            io.nl(!IO),
            io.write(Var2, !IO),
            io.nl(!IO)
        ;
            unexpected(this_file, "transform_goal_2: |Vars| != 2")
        )
    ).

%-----------------------------------------------------------------------------%

svar_finish_if_then_else(Context, Then0, Then, Else0, Else,
        SInfo0, SInfoC, SInfoT0, SInfoE, SInfo, !VarSet) :-
    % Add unifiers to the Then arm for state variables that acquired
    % new mappings in the condition, but not in the Them arm itself.
    % This is because the new mappings appear only in a negated context.
    StateVars = list.merge_and_remove_dups(map.keys(SInfoT0 ^ svar_dot),
         map.keys(SInfoE  ^ svar_dot)),
    Then0 = hlds_goal(_, GoalInfo),
    goal_to_conj_list(Then0, Thens0),
    add_then_arm_specific_unifiers(Context, StateVars,
        SInfo0, SInfoC, SInfoT0, SInfoT, Thens0, Thens, !VarSet),
    conj_list_to_goal(Thens, GoalInfo, Then1),

    % Calculate the svar_info with the highest numbered mappings from each arm.
    DisjSInfos = [{Then1, SInfoT}, {Else0, SInfoE}],
    SInfo      = reconcile_disj_svar_info(!.VarSet, DisjSInfos),

    % Add unifiers to each arm to ensure they both construct the same
    % final state variable mappings.
    Then = add_disj_unifiers(Context, SInfo, StateVars, {Then1, SInfoT}),
    Else = add_disj_unifiers(Context, SInfo, StateVars, {Else0, SInfoE}).

    % If a new mapping was produced for state variable X in the condition-goal
    % (i.e. the condition refers to !:X), but not in the then-goal, then
    % we have to add a new unifier !:X = !.X to the then-goal because the
    % new mapping was created in a negated context.
    %
:- pred add_then_arm_specific_unifiers(prog_context::in, svars::in,
    svar_info::in, svar_info::in, svar_info::in, svar_info::out,
    hlds_goals::in, hlds_goals::out, prog_varset::in, prog_varset::out) is det.

add_then_arm_specific_unifiers(_, [], _, _, !SInfoT, !Thens, !VarSet).
add_then_arm_specific_unifiers(Context, [StateVar | StateVars],
        SInfo0, SInfoC, !SInfoT, !Thens, !VarSet) :-
    (
        % The condition refers to !:X, but the then-goal doesn't.

        % If the condition refers to !:X, then X will appear in the map after
        % the condition, and therefore in the following map at the end of the
        % else too.
        map.search(SInfoC ^ svar_dot, StateVar, DotC),
        map.search(!.SInfoT ^ svar_dot, StateVar, DotT),
        DotT = DotC,

        % We know that the condition refers to !:X if either X did not appear
        % in the map before the if-then-else, or if it did, but with a
        % different program variable than at the end of the condition.
        \+ (
            map.search(SInfo0 ^ svar_dot, StateVar, Dot0),
            DotC = Dot0
        )
    ->
        % Add a new unifier !:X = !.X.
        new_colon_state_var(StateVar, NewDotT, !VarSet, !SInfoT),
        ThenUnifier = svar_unification(yes(feature_dont_warn_singleton),
            Context, NewDotT, DotT),
        !:Thens = [ThenUnifier | !.Thens],
        svar_prepare_for_next_conjunct(set.make_singleton_set(StateVar),
            !VarSet, !SInfoT)
    ;
        true
    ),
    add_then_arm_specific_unifiers(Context, StateVars,
        SInfo0, SInfoC, !SInfoT, !Thens, !VarSet).

%-----------------------------------------------------------------------------%

:- pred next_svar_mappings(int::in, svars::in,
    prog_varset::in, prog_varset::out, svar_map::out) is det.

next_svar_mappings(N, StateVars, !VarSet, Map) :-
    next_svar_mappings_2(N, StateVars, !VarSet, map.init, Map).

:- pred next_svar_mappings_2(int::in, svars::in,
    prog_varset::in, prog_varset::out, svar_map::in, svar_map::out) is det.

next_svar_mappings_2(_, [], !VarSet, !Map).
next_svar_mappings_2(N, [StateVar | StateVars], !VarSet, !Map) :-
    next_svar_mapping(N, StateVar, _, !VarSet, !Map),
    next_svar_mappings_2(N, StateVars, !VarSet, !Map).

%-----------------------------------------------------------------------------%

svar_finish_negation(SInfoBefore, SInfoNeg, !:SInfo) :-
    !:SInfo = SInfoBefore,
    !SInfo ^ svar_num   := SInfoNeg ^ svar_num,
    !SInfo ^ svar_colon := SInfoNeg ^ svar_colon.

%-----------------------------------------------------------------------------%

svar_finish_disjunction(Context, VarSet, DisjSInfos, Disjs, SInfo) :-
    SInfo = reconcile_disj_svar_info(VarSet, DisjSInfos),
    map.keys(SInfo ^ svar_dot, StateVars),
    Disjs = list.map(add_disj_unifiers(Context, SInfo, StateVars), DisjSInfos).

    % Each arm of a disjunction may have a different mapping for
    % !.X and/or !:X. The reconciled svar_info for the disjunction takes
    % the highest numbered mapping for each disjunct (each state variable
    % mapping for !.X or !:X will have a name of the form `STATE_VARIABLE_X_n'
    % for some number `n'.)
    %
:- func reconcile_disj_svar_info(prog_varset, hlds_goal_svar_infos) =
    svar_info.

reconcile_disj_svar_info(_, []) = _ :-
    unexpected(this_file, "reconcile_disj_svar_info: empty disjunct list").
reconcile_disj_svar_info(VarSet, [DisjSInfo | DisjSInfos]) = SInfo :-
    % We compute the set of final !. and !: state variables over the whole
    % disjunction (not all arms will necessarily include !. and !: mappings
    % for all state variables).
    DisjSInfo = {_, SInfo0},
    Dots0   = set.sorted_list_to_set(map.keys(SInfo0 ^ svar_dot)),
    Colons0 = set.sorted_list_to_set(map.keys(SInfo0 ^ svar_colon)),
    union_dot_colon_svars(DisjSInfos, Dots0, Dots, Colons0, Colons),

    % Then we update SInfo0 to take the highest numbered !. and !: mapping
    % for each state variable.
    list.foldl(reconcile_svar_infos(VarSet, Dots, Colons), DisjSInfos,
        SInfo0, SInfo).

:- pred union_dot_colon_svars(hlds_goal_svar_infos::in,
    svar_set::in, svar_set::out, svar_set::in, svar_set::out) is det.

union_dot_colon_svars([], !Dots, !Colons).
union_dot_colon_svars([DisjSInfo | DisjSInfos], !Dots, !Colons) :-
    DisjSInfo = {_, SInfo},
    set.union(set.sorted_list_to_set(map.keys(SInfo ^ svar_dot)), !Dots),
    set.union(set.sorted_list_to_set(map.keys(SInfo ^ svar_colon)), !Colons),
    union_dot_colon_svars(DisjSInfos, !Dots, !Colons).

:- pred reconcile_svar_infos(prog_varset::in, svar_set::in, svar_set::in,
    hlds_goal_svar_info::in, svar_info::in, svar_info::out) is det.

reconcile_svar_infos(VarSet, Dots, Colons, {_, SInfoX}, !SInfo) :-
    InitNum = !.SInfo ^ svar_num,
    XNum = SInfoX ^ svar_num,
    set.fold(reconcile_svar_infos_dots(VarSet, SInfoX), Dots, !SInfo),
    set.fold(reconcile_svar_infos_colons(VarSet, SInfoX), Colons, !SInfo),
    !SInfo ^ svar_num := max(InitNum, XNum).

:- pred reconcile_svar_infos_dots(prog_varset::in, svar_info::in, svar::in,
    svar_info::in, svar_info::out) is det.

reconcile_svar_infos_dots(VarSet, SInfoX, StateVar, !SInfo) :-
    DotMapX = SInfoX ^ svar_dot,
    DotMap0 = !.SInfo ^ svar_dot,
    (
        map.search(DotMapX, StateVar, DotX),
        map.search(DotMap0, StateVar, Dot0)
    ->
        varset.lookup_name(VarSet, DotX, NameX),
        varset.lookup_name(VarSet, Dot0, Name0),
        compare_svar_names(RDot, NameX, Name0),
        (
            ( RDot  = (<)
            ; RDot  = (=)
            )
        ;
            RDot  = (>),
            map.det_update(DotMap0, StateVar, DotX, DotMap),
            !SInfo ^ svar_dot := DotMap
        )
    ;
        true
    ).

:- pred reconcile_svar_infos_colons(prog_varset::in, svar_info::in, svar::in,
    svar_info::in, svar_info::out) is det.

reconcile_svar_infos_colons(VarSet, SInfoX, StateVar, !SInfo) :-
    ColonMapX = SInfoX ^ svar_colon,
    ColonMap0 = !.SInfo ^ svar_colon,
    (
        map.search(ColonMapX, StateVar, ColonX),
        map.search(ColonMap0, StateVar, Colon0)
    ->
        varset.lookup_name(VarSet, ColonX, NameX),
        varset.lookup_name(VarSet, Colon0, Name0),
        compare_svar_names(RColon, NameX, Name0),
        (
            ( RColon = (<)
            ; RColon = (=)
            )
        ;
            RColon = (>),
            map.det_update(ColonMap0, StateVar, ColonX, ColonMap),
            !SInfo ^ svar_colon := ColonMap
        )
    ;
        true
    ).

:- func add_disj_unifiers(prog_context, svar_info, svars, hlds_goal_svar_info)
    = hlds_goal.

add_disj_unifiers(Context, SInfo, StateVars, {GoalX, SInfoX}) = Goal :-
    Unifiers0 = [],
    list.foldl(add_disj_unifier(Context, SInfo, SInfoX), StateVars,
        Unifiers0, Unifiers),
    GoalX = hlds_goal(_, GoalInfo),
    goal_to_conj_list(GoalX, GoalsX),
    conj_list_to_goal(GoalsX ++ Unifiers, GoalInfo, Goal).

:- pred add_disj_unifier(prog_context::in, svar_info::in, svar_info::in,
    svar::in, list(hlds_goal)::in, list(hlds_goal)::out) is det.

add_disj_unifier(Context, SInfo, SInfoX, StateVar, !Unifiers) :-
    (
        map.search(SInfo ^ svar_dot, StateVar, Dot),
        map.search(SInfoX ^ svar_dot, StateVar, DotX),
        Dot \= DotX
    ->
        Unifier = svar_unification(yes(feature_dont_warn_singleton), Context,
            Dot, DotX),
        !:Unifiers = [Unifier | !.Unifiers]
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % We implement a special purpose comparison for state variable names
    % that compares the numbers appended at the right hand ends of the
    % name strings.
    %
    % NOTE State variable names are either "..._X" or "..._X_N" where X is
    % the name of the program variable used for the state variable and
    % N is a decimal number with no leading zeroes.
    %
    % NOTE The code below looks a bit slow, since it extracts the numbers
    % from the ends of variable names repeatedly. I (zs) tried to avoid
    % this cost by making the read_only_dot, dot and colon maps in svar_infos
    % map not just to a variable but a pair of a variable and a number,
    % the number being the one we extract below. This even allowed us to avoid
    % passing varsets in lots of places, since in many predicates varsets
    % are needed only for looking up the variable names passed to
    % compare_svar_names. However, I found that the result was an overall
    % slowdown. Apparently, the overhead of recording the numbers, and of
    % extracting the variables from the variable-number pairs in lookups,
    % is higher than the overhead of compare_svar_names.
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

svar_finish_equivalence(SInfoBefore, SInfoEqv, SInfo) :-
    svar_finish_negation(SInfoBefore, SInfoEqv, SInfo).

%-----------------------------------------------------------------------------%

svar_prepare_for_call(ParentSInfo, SInfo) :-
    ( ParentSInfo ^ svar_ctxt = in_atom(UpdatedStateVars, _GrandparentSInfo) ->
        Ctxt = in_atom(UpdatedStateVars, ParentSInfo)
    ;
        Ctxt = in_atom(set.init, ParentSInfo)
    ),
    SInfo = ParentSInfo ^ svar_ctxt := Ctxt.

%-----------------------------------------------------------------------------%

svar_finish_call(!VarSet, !SInfo) :-
    ( !.SInfo ^ svar_ctxt = in_atom(UpdatedStateVars, ParentSInfo0) ->
        ParentSInfo = ( ParentSInfo0 ^ svar_dot := !.SInfo ^ svar_dot ),
        ( ParentSInfo ^ svar_ctxt = in_atom(_, GrandParentSInfo) ->
            !:SInfo = ( ParentSInfo ^ svar_ctxt :=
                in_atom(UpdatedStateVars, GrandParentSInfo) )
        ;
            svar_prepare_for_next_conjunct(UpdatedStateVars, !VarSet,
                ParentSInfo, !:SInfo)
        )
    ;
        unexpected(this_file, "svar_finish_call: ctxt is not in_atom")
    ).

%-----------------------------------------------------------------------------%

svar_prepare_for_if_then_else_goal(StateVars, !VarSet, !SInfo) :-
    prepare_for_local_state_vars(StateVars, !VarSet, !SInfo).

%-----------------------------------------------------------------------------%

svar_finish_if_then_else_goal_condition(StateVars, SInfoBefore,
        SInfoA0, SInfoA, SInfoB) :-
    SInfoB = SInfoA0,
    finish_local_state_vars(StateVars, _, SInfoBefore, SInfoA0, SInfoA).

%-----------------------------------------------------------------------------%

svar_finish_if_then_else_goal_then_goal(StateVars,
        SInfoBefore, SInfoB0, SInfoB) :-
    finish_local_state_vars(StateVars, _, SInfoBefore, SInfoB0, SInfoB).

%-----------------------------------------------------------------------------%

svar_prepare_for_if_then_else_expr(StateVars, !VarSet, !SInfo) :-
    SInfo0 = !.SInfo,
    !:SInfo = new_svar_info ^ svar_ctxt := in_body,
    !:SInfo = !.SInfo ^ svar_readonly_dot := SInfo0 ^ svar_dot,
    !:SInfo = !.SInfo ^ svar_num := SInfo0 ^ svar_num,
    prepare_for_local_state_vars(StateVars, !VarSet, !SInfo).

%-----------------------------------------------------------------------------%

svar_finish_if_then_else_expr_condition(Before, !SInfo) :-
    SInfo0 = !.SInfo,
    !:SInfo = !.SInfo ^ svar_readonly_dot := Before ^ svar_readonly_dot,
    !:SInfo = !.SInfo ^ svar_dot :=
        (SInfo0 ^ svar_dot) `overlay` (Before ^ svar_dot),
    !:SInfo = !.SInfo ^ svar_colon :=
        (SInfo0 ^ svar_colon) `overlay` (Before ^ svar_colon),
    !:SInfo = !.SInfo ^ svar_ctxt := Before ^ svar_ctxt.

%-----------------------------------------------------------------------------%

svar_finish_if_then_else_expr_then_goal(StateVars, SInfoBefore, !SInfo) :-
    finish_local_state_vars(StateVars, _, SInfoBefore, !SInfo).

%-----------------------------------------------------------------------------%

svar_prepare_for_next_conjunct(UpdatedStateVars, !VarSet, !SInfo) :-
    DotMap0   = !.SInfo ^ svar_dot,
    ColonMap0 = !.SInfo ^ svar_colon,
    N = !.SInfo ^ svar_num + 1,
    map.init(Nil),
    map.foldl(next_dot_mapping(UpdatedStateVars, DotMap0, ColonMap0),
        ColonMap0, Nil, DotMap),
    map.foldl2(next_colon_mapping(UpdatedStateVars, ColonMap0, N),
        ColonMap0, !VarSet, Nil, ColonMap),
    !:SInfo  = !.SInfo ^ svar_ctxt  := in_body,
    !:SInfo  = !.SInfo ^ svar_num   := N,
    !:SInfo  = !.SInfo ^ svar_dot   := DotMap,
    !:SInfo  = !.SInfo ^ svar_colon := ColonMap.

    % If the state variable has been updated (i.e. there was a !:X reference)
    % then the next !.X mapping will be the current !:X mapping. Otherwise,
    % preserve the current !.X mapping, if any (there may be none if,
    % for example, the head only references !:X and there have been no prior
    % references to !:X in the body.)
    %
:- pred next_dot_mapping(svar_set::in, svar_map::in, svar_map::in, svar::in,
    prog_var::in, svar_map::in, svar_map::out) is det.

next_dot_mapping(UpdatedStateVars, OldDotMap, OldColonMap, StateVar, _,
        !DotMap) :-
    % XXX Should either of these svmap.sets be det_update or det_insert?
    ( UpdatedStateVars `contains` StateVar ->
        map.lookup(OldColonMap, StateVar, Var),
        svmap.set(StateVar, Var, !DotMap)
    ; map.search(OldDotMap, StateVar, Var) ->
        svmap.set(StateVar, Var, !DotMap)
    ;
        true
    ).

    % If the state variable has been updated (i.e. there was a !:X reference)
    % then create a new mapping for the next !:X. Otherwise, the next !:X
    % mapping is the same as the current !:X mapping.
    %
:- pred next_colon_mapping(svar_set::in, svar_map::in, int::in, svar::in,
    prog_var::in, prog_varset::in, prog_varset::out,
    svar_map::in, svar_map::out) is det.

next_colon_mapping(UpdatedStateVars, OldColon, N, StateVar, _,
        !VarSet, !ColonMap) :-
    ( UpdatedStateVars `contains` StateVar ->
        next_svar_mapping(N, StateVar, _Var, !VarSet, !ColonMap)
    ;
        map.lookup(OldColon, StateVar, Var),
        % XXX Should this be svmap.det_update?
        svmap.set(StateVar, Var, !ColonMap)
    ).

:- pred next_svar_mapping(int::in, svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_map::in, svar_map::out) is det.

next_svar_mapping(N, StateVar, Var, !VarSet, !Map) :-
    Name = string.format("STATE_VARIABLE_%s_%d",
        [s(varset.lookup_name(!.VarSet, StateVar)), i(N)]),
    varset.new_named_var(!.VarSet, Name, Var, !:VarSet),
    map.set(!.Map, StateVar, Var, !:Map).

%-----------------------------------------------------------------------------%

expand_bang_state_var_args([]) = [].
expand_bang_state_var_args([HeadArg0 | TailArgs0]) = Args :-
    TailArgs = expand_bang_state_var_args(TailArgs0),
    (
        HeadArg0 = variable(_, _),
        Args = [HeadArg0 | TailArgs]
    ;
        HeadArg0 = functor(Const, FunctorArgs, Ctxt),
        (
            Const = atom("!"),
            FunctorArgs = [variable(_StateVar, _)]
        ->
            HeadArg1 = functor(atom("!."), FunctorArgs, Ctxt),
            HeadArg2 = functor(atom("!:"), FunctorArgs, Ctxt),
            Args = [HeadArg1, HeadArg2 | TailArgs]
        ;
            Args = [HeadArg0 | TailArgs]
        )
    ).

%-----------------------------------------------------------------------------%

expand_bang_state_var_args_in_instance_method_heads(InstanceBody) = Expanded :-
    (
        InstanceBody = instance_body_abstract,
        Expanded = instance_body_abstract
    ;
        InstanceBody = instance_body_concrete(Methods),
        Expanded = instance_body_concrete(
            list.map(expand_method_bsvs, Methods))
    ).

:- func expand_method_bsvs(instance_method) = instance_method.

expand_method_bsvs(IM) = IM :-
    IM = instance_method(_, _, instance_proc_def_name(_), _, _).

expand_method_bsvs(IM0) = IM :-
    IM0 = instance_method(PredOrFunc, Method, instance_proc_def_clauses(Cs0),
        Arity0, Ctxt),
    Cs  = list.map(expand_item_bsvs, Cs0),
    % Note that the condition should always succeed...
    ( Cs = [ItemClause | _] ->
        Args = ItemClause ^ cl_head_args,
        adjust_func_arity(PredOrFunc, Arity, list.length(Args))
    ;
        Arity = Arity0
    ),
    IM  = instance_method(PredOrFunc, Method, instance_proc_def_clauses(Cs),
        Arity, Ctxt).

:- func expand_item_bsvs(item_clause_info) = item_clause_info.

expand_item_bsvs(ItemClause0) = ItemClause :-
    ItemClause0 = item_clause_info(Origin, VarSet, PredOrFunc, SymName,
        Args0, Body, Context, SeqNum),
    Args = expand_bang_state_var_args(Args0),
    ItemClause = item_clause_info(Origin, VarSet, PredOrFunc, SymName,
        Args, Body, Context, SeqNum).

%-----------------------------------------------------------------------------%

substitute_state_var_mappings([], [], !VarSet, !SInfo, !Specs).
substitute_state_var_mappings([Arg0 | Args0], [Arg | Args], !VarSet, !SInfo,
        !Specs) :-
    substitute_state_var_mapping(Arg0, Arg, !VarSet, !SInfo, !Specs),
    substitute_state_var_mappings(Args0, Args, !VarSet, !SInfo, !Specs).

substitute_state_var_mapping(Arg0, Arg, !VarSet, !SInfo, !Specs) :-
    ( Arg0 = functor(atom("!."), [variable(StateVar, _)], Context) ->
        svar_dot(Context, StateVar, Var, !VarSet, !SInfo, !Specs),
        Arg = variable(Var, context_init)
    ; Arg0 = functor(atom("!:"), [variable(StateVar, _)], Context) ->
        svar_colon(Context, StateVar, Var, !VarSet, !SInfo, !Specs),
        Arg = variable(Var, context_init)
    ;
        Arg = Arg0
    ).

%-----------------------------------------------------------------------------%

illegal_state_var_func_result(pf_function, Args, StateVar) :-
    list.last(Args, functor(atom("!"), [variable(StateVar, _)], _Ctxt)).

%-----------------------------------------------------------------------------%

lambda_args_contain_bang_state_var([Arg | Args], StateVar) :-
    ( Arg = functor(atom("!"), [variable(StateVar0, _)], _) ->
        StateVar = StateVar0
    ;
        lambda_args_contain_bang_state_var(Args, StateVar)
    ).

%-----------------------------------------------------------------------------%

report_illegal_state_var_update(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error: cannot use"), fixed("!:" ++ Name),
        words("in this context;"), nl,
        words("however"), fixed("!." ++ Name), words("may be used here."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_non_visible_state_var(string::in, prog_context::in,
    prog_varset::in, svar::in, list(error_spec)::in, list(error_spec)::out)
    is det.

report_non_visible_state_var(DorC, Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error: state variable"), fixed("!" ++ DorC ++ Name),
        words("is not visible in this context."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_uninitialized_state_var(prog_context::in, prog_varset::in,
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

report_uninitialized_state_var(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: reference to uninitialized state variable"),
        fixed("!." ++ Name), suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_warning, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

report_illegal_func_svar_result(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:"), fixed("!" ++ Name),
        words("cannot be a function result."), nl,
        words("You probably meant"), fixed("!." ++ Name),
        words("or"), fixed("!:" ++ Name), suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

report_illegal_bang_svar_lambda_arg(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:"), fixed("!" ++ Name),
        words("cannot be a lambda argument."), nl,
        words("Perhaps you meant"), fixed("!." ++ Name),
        words("or"), fixed("!:" ++ Name), suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "state_var.m".

%-----------------------------------------------------------------------------%
