%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2011 The University of Melbourne.
% Copyright (C) 2014-2016, 2018-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: state_var.m.
% Main author of original version: rafe.
% Main author of the current version, rewritten in 2011: zs.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.state_var.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.goal_expr_to_goal.
:- import_module hlds.make_hlds.qual_info.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module map.
:- import_module one_or_more_map.

%---------------------------------------------------------------------------%

    % This type describes the state of the code that converts goals
    % from their parse tree form to their HLDS form. Almost all the code
    % in all the modules of the make_hlds package that handle goals
    % pass around values of this type as effectively global state,
    % with persistent updates. (The state of the state var transformation
    % itself is threaded through that code in a different manner;
    % see the definition of the svar_state type below.)
    %
    % With one exception, all of the fields are writeable.
:- type unravel_info
    --->    unravel_info(
                % The module_info, which we use for several purposes.
                % Most uses are readonly, including getting the globals
                % for option lookup, and the module name for creating
                % debug output streams.
                %
                % The only situation in which we update the module_info field
                % is when processing disable_warning scopes that disable
                % the warning for occurs check violations. In that case,
                % we set the option controlling that warning to "no"
                % while processing the goal in the scope, and reset it
                % afterwards. Such scopes are rare enough that storing the
                % value of that option as a separate field in this structure
                % would not be worthwhile.
                ui_module_info      :: module_info,

                % The value of the from_ground_term_threshold option.
                % This field duplicates the value stored in the globals
                % structure, but it is needed often enough that a separate
                % fast-access copy is worthwhile.
                % This field is read-only.
                ui_fgt_threshold    :: int,

                % The store where we record information about what entities
                % imported from other modules are used. We use that info
                % to generate warnings about unused imports.
                ui_qual_info        :: qual_info,

                % The varset of the clause whose goal we are converting.
                % New instances of state variables are allocated from here.
                ui_varset           :: prog_varset,

                % The part of the state of the state var transformation
                % that is updated persistently (meaning, that once we create
                % a new version, we don't go back to look at previous
                % versions.)
                ui_state_var_store  :: svar_store,

                % The errrors and warnings that we definitely want to print.
                % (The svar_store also contains error_specs, but we print those
                % only as hints *if and when* we later find certain other kinds
                % of errors.)
                ui_error_specs      :: list(error_spec)
            ).

%---------------------------------------------------------------------------%

:- pred create_new_unravel_var(prog_var::out,
    unravel_info::in, unravel_info::out) is det.

:- pred create_new_named_unravel_var(string::in, prog_var::out,
    unravel_info::in, unravel_info::out) is det.

:- pred record_unravel_found_syntax_error(
    unravel_info::in, unravel_info::out) is det.

:- pred add_unravel_spec(error_spec::in,
    unravel_info::in, unravel_info::out) is det.
:- pred add_unravel_specs(list(error_spec)::in,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%

    % This synonym improves code legibility. The intention is that we use
    % svar instead of prog_var in pred type declarations for any variables X
    % that represent state variables !X.
:- type svar == prog_var.

    % When collecting the arms of a disjunction, we also need to collect
    % the resulting svar_states.
:- type hlds_goal_svar_state
    --->    hlds_goal_svar_state(hlds_goal, svar_state).

    % The state of the currently visible state variables. The state gets
    % updated differently along differently execution paths. When execution
    % paths rejoin, you need to create the state after the rejoin from the
    % states being rejoined (which is what we use hlds_goal_svar_state for)
    % using their last common ancestor state as a basis.
:- type svar_state.

    % The persistent information needed by the state variable transformation.
    % The store should always be threaded straight through all computations
    % involved in the translation of the parse tree to the HLDS, with all
    % updates being permanent.
:- type svar_store.

%---------------------------------------------------------------------------%

    % Given the user-given name of a state variable, return the name
    % of the program variable representing its initial version.
    %
:- func initial_state_var_name(string) = string.

    % is_prog_var_for_some_state_var(VarSet, ProgVar, StateVarName):
    %
    % Succeed if and only if ProgVar's name indicates that it represents
    % a version of a state variable named StateVarName. Succeed for
    % any version: initial, middle, or final.
    %
:- pred is_prog_var_for_some_state_var(prog_varset::in, prog_var::in,
    string::out) is semidet.

%---------------------------------------------------------------------------%

    % Replace !X args with two args !.X, !:X in that order.
    %
:- pred expand_bang_state_pairs_in_terms(list(prog_term)::in,
    list(prog_term)::out) is det.
:- pred expand_bang_state_pairs_in_instance_method(instance_method::in,
    instance_method::out) is det.

%---------------------------------------------------------------------------%

:- type new_statevar_map.

    % Prepare for processing a clause by processing its head.
    % If the head contains any references to !.S or !:S or both,
    % make state variable S known in the body of the clause.
    % (The head should not contain any references to !S; those should
    % have been expanded out by calling expand_bang_state_pairs BEFORE calling
    % this predicate.)
    %
    % Given the original list of args, we return a version in which state
    % variable references have been replaced. Since we don't yet know what
    % the final values of the state variables will be, we create prog_vars
    % to represent these values, and return a mapping from the state vars
    % to these designated-final-value prog_vars.
    %
:- pred svar_prepare_for_clause_head(module_info::in, qual_info::in,
    prog_varset::in, list(prog_term)::in, list(prog_term)::out,
    map(svar, prog_var)::out, new_statevar_map::out,
    svar_state::out, unravel_info::out) is det.

    % Prepare for processing a lambda expression by processing its head.
    %
    % In most ways, this is very similar to processing the head of a clause,
    % but we also need to handle state variables which are visible in the scope
    % that encloses the lambda expression. We make those state vars read-only
    % within the lambda expression.
    %
:- pred svar_prepare_for_lambda_head(prog_context::in,
    list(prog_term)::in, list(prog_term)::out,
    map(svar, prog_var)::out, new_statevar_map::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

%---------------------%

    % Finish processing a clause. Make the final values of the clause's state
    % vars match the mapping we decided on when processing the head.
    %
:- pred svar_finish_clause_body(prog_context::in, new_statevar_map::in,
    map(svar, prog_var)::in, svar_state::in, svar_state::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::out,
    list(error_spec)::out, unused_statevar_arg_map::out,
    unravel_info::in, unravel_info::out) is det.

    % Finish processing a lambda expression.
    %
:- pred svar_finish_lambda_body(prog_context::in, list(mer_mode)::in,
    new_statevar_map::in, map(svar, prog_var)::in, goal::in,
    list(hlds_goal)::in, hlds_goal::out, svar_state::in, svar_state::in,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%

    % Finish the execution of an atomic goal. If this goal was not inside
    % another atomic goal, then make any updates to state variables performed
    % by the atomic goal take effect: make the value assigned to !:S inside
    % the goal the new !.S.
    %
:- pred svar_finish_atomic_goal(loc_kind::in, svar_state::in, svar_state::out)
    is det.

%---------------------------------------------------------------------------%

    % Add some local state variables.
    %
:- pred svar_prepare_for_local_state_vars(prog_context::in, list(svar)::in,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

    % Remove some local state variables.
    %
:- pred svar_finish_local_state_vars(unravel_info::in,
    list(svar)::in, svar_state::in, svar_state::in, svar_state::out) is det.

%---------------------------------------------------------------------------%

    % Make sure that all arms of a disjunction produce the same state variable
    % bindings, by adding unifiers as necessary.
    %
:- pred svar_finish_disjunction(list(hlds_goal_svar_state)::in,
    list(hlds_goal)::out, svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%

    % Add unifiers to the Then and Else arms of an if-then-else as needed
    % to ensure that all the state variables match up.
    %
    % We also add unifiers to the Then arm for any new state variable
    % mappings produced in the condition.
    %
:- pred svar_finish_if_then_else(loc_kind::in, prog_context::in,
    list(svar)::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    svar_state::in, svar_state::in, svar_state::in, svar_state::in,
    svar_state::out,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%

:- type svar_outer_atomic_scope_info.
:- type svar_inner_atomic_scope_info.

    % svar_start_outer_atomic_scope(Context, OuterStateVar, OuterDI, OuterUO,
    %   OuterScopeInfo, !SVarState, !UrInfo):
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
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

    % svar_finish_outer_atomic_scope(OuterScopeInfo, !SInfo):
    %
    % Restore the accessibility of !OuterStateVar that was disabled by
    % svar_start_atomic_scope.
    %
:- pred svar_finish_outer_atomic_scope(svar_outer_atomic_scope_info::in,
    svar_state::in, svar_state::out) is det.

    % svar_start_inner_atomic_scope(Context, InnerStateVar, InnerScopeInfo,
    %   !SVarState, !UrInfo):
    %
    % This predicate prepares for an atomic scope with an !InnerStateVar
    % specification by making that state var available.
    %
:- pred svar_start_inner_atomic_scope(prog_context::in, prog_var::in,
    svar_inner_atomic_scope_info::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

    % svar_finish_inner_atomic_scope(Context, InnerScopeInfo, InnerDI, InnerUO,
    %   !SVarState, !UrInfo):
    %
    % This predicate ends an atomic scope with an !InnerStateVar
    % specification by making that state var unavailable, and returning
    % the two variables InnerDI and InnerUO representing the initial and final
    % states of this state variable.
    %
:- pred svar_finish_inner_atomic_scope(prog_context::in,
    svar_inner_atomic_scope_info::in, prog_var::out, prog_var::out,
    svar_state::in, svar_state::out) is det.

%---------------------------------------------------------------------------%

    % Given a list of argument terms, replace !.X and !:X with the
    % ordinary variables corresponding to them, updating the svar_state
    % as appropriate. Any occurrence of !X should already have been
    % expanded into a <!.X, !:X> pair by a call to expand_bang_state_pairs.
    %
:- pred replace_any_dot_colon_state_var_in_terms(
    list(prog_term)::in, list(prog_term)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

    % Same as replace_any_dot_colon_state_var_in_terms, but for only one term.
    %
:- pred replace_any_dot_colon_state_var_in_term(prog_term::in, prog_term::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

    % Look up the prog_var that represents the current state of the given
    % state variable.
    %
:- pred lookup_dot_state_var(prog_context::in, svar::in, prog_var::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

    % Look up the prog_var that represents the next state of the given
    % state variable.
    %
:- pred lookup_colon_state_var(prog_context::in, svar::in, prog_var::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%

    % Flatten a conjunction while preserving the invariants that the state
    % variable transformation cares about.
    %
:- pred svar_flatten_conj(prog_context::in,
    list(hlds_goal)::in, hlds_goal::out,
    unravel_info::in, unravel_info::out) is det.

    % Flatten a goal into a conjunction while preserving the invariants that
    % the state variable transformation cares about.
    %
:- pred svar_goal_to_conj_list(hlds_goal::in, list(hlds_goal)::out,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%

    % Does the given argument list have a function result term
    % that tries to use state var notation to refer to *two* terms?
    %
    % If yes, return the state variable involved, and the context of the
    % reference.
    %
:- pred illegal_state_var_func_result(pred_or_func::in, list(prog_term)::in,
    svar::out, prog_context::out) is semidet.

    % Does the given term have the form a !X, i.e. does it represent
    % *two* arguments? This is not acceptable in some contexts, such as
    % function results and lambda expression arguments.
    %
    % If yes, return the state variable involved, and the context of the
    % reference.
    %
:- pred is_term_a_bang_state_pair(prog_term::in,
    svar::out, prog_context::out) is semidet.

%---------------------------------------------------------------------------%

:- pred report_illegal_state_var_update(prog_context::in,
    string::in, prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

:- pred report_illegal_func_svar_result(prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.
:- func report_illegal_func_svar_result_raw(prog_context,
    prog_varset, svar) = error_spec.

:- pred report_illegal_bang_svar_lambda_arg(prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.
:- func report_illegal_bang_svar_lambda_arg_raw(prog_context,
    prog_varset, svar) = error_spec.

:- pred report_svar_unify_error(prog_context::in, svar::in,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% XXX Having hlds.make_hlds.state_var depend on the check_hlds
% is undesirable. Maybe inst_util and mode_util, and maybe inst_test and
% mode_test, should be moved to the hlds package.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module hlds.goal_vars.
:- import_module hlds.hlds_markers.
:- import_module hlds.make_goal.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module io.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%

create_new_unravel_var(Var, !UrInfo) :-
    VarSet0 = !.UrInfo ^ ui_varset,
    varset.new_var(Var, VarSet0, VarSet),
    !UrInfo ^ ui_varset := VarSet.

create_new_named_unravel_var(Name, Var, !UrInfo) :-
    VarSet0 = !.UrInfo ^ ui_varset,
    varset.new_named_var(Name, Var, VarSet0, VarSet),
    !UrInfo ^ ui_varset := VarSet.

record_unravel_found_syntax_error(!UrInfo) :-
    QualInfo0 = !.UrInfo ^ ui_qual_info,
    qual_info_set_found_syntax_error(yes, QualInfo0, QualInfo),
    !UrInfo ^ ui_qual_info := QualInfo.

add_unravel_spec(NewSpec, !UrInfo) :-
    Specs0 = !.UrInfo ^ ui_error_specs,
    Specs = [NewSpec | Specs0],
    !UrInfo ^ ui_error_specs := Specs.

add_unravel_specs(NewSpecs, !UrInfo) :-
    (
        NewSpecs = []
    ;
        NewSpecs = [_ | _],
        Specs0 = !.UrInfo ^ ui_error_specs,
        Specs = NewSpecs ++ Specs0,
        !UrInfo ^ ui_error_specs := Specs
    ).

%---------------------------------------------------------------------------%
%
% Define the main data structures used by the implementation of state vars.
%

    % State vars defined outside a lambda goal become readonly when we move
    % inside the lambda goal. Inside the lambda goal, it makes sense to access
    % the current value of such a state var, but it does not make sense
    % to try to update its value.
    %
    % We should make negations behave similarly: it should not be possible
    % to update an outside state var inside a negation. However, for now,
    % the language reference manual allows such updates. This type is here
    % in case that changes.
:- type readonly_context_kind
    --->    roc_lambda.

:- type svar_status
    % The two updated statuses may legally be present in a status map
    % only DURING the processing of an atomic goal. At the end of each
    % atomic goal, such statuses are always reset to status_known.
    --->    status_unknown
            % We are in a scope that allows use of this state var,
            % but it has not been given a value yet. This could be because
            % the scope of the state var was established with !:S, not !S
            % or !.S, in a clause head, or because it was established
            % in a `some [!S]' scope.

    ;       status_unknown_updated(prog_var)
            % Before this atomic goal, this state var was status_unknown,
            % but it was initialized by the current atomic goal to the given
            % prog_var.

    ;       status_known_ro(prog_var, readonly_context_kind, prog_context)
            % The given prog_var is the current version of this state var,
            % but the variable is readonly (ro); the program CANNOT create
            % new versions of the state var. The second argument says WHY
            % new versions cannot be created, and the third says where
            % the construct named by the second argument occurs.

    ;       status_known(prog_var)
            % The given prog_var is the current version of this state var;
            % the program can create new versions of the state var,
            % but has not done so yet.

    ;       status_known_updated(prog_var, prog_var).
            % The first prog_var is the current version of this state var,
            % and the second is the new, updated version, which will become
            % the current version when we finish executing the current
            % atomic goal.

:- type svar_state
    --->    svar_state(
                state_status_map    ::  map(svar, svar_status)
            ).

:- type last_id_map == map(prog_var, uint).

:- type svar_store
    --->    svar_store(
                store_next_goal_id          ::  ucounter,
                store_final_remap           ::  incremental_rename_map,

                % This maps each state variable that has had N name_middle
                % versions created to N, *provided* that N > 0. We use this
                % for two purposes:
                %
                % - Giving a name to the variables representing the middle
                %   (meaning non-initial and non-final) versions of each
                %   state variable that is affected only by references to
                %   that state variable. We used to use as the variable
                %   id assigned by the current varset, but this meant that
                %   changes in the number and/or handling of non-state
                %   variables in the clause could affect the variable names
                %   reported by error messages, requiring annoying updates to
                %   test cases .err_exp* files.
                %
                % - We use the absence of an entry for a state variable
                %   as an indication that this state variable has not been
                %   updated. This can seem a bit strange, since it is possible
                %   for a clause body to invoke a predicate that takes the
                %   state variable from its initial state to its final state
                %   directly, but even if that ends up being the case,
                %   it will not have started out that way. Instead, when
                %   processing the call, we will allocate a new middle version
                %   for the state var, and only when we finish the processing
                %   of the clause as a whole will we replace that middle
                %   version with the final version. And when we do, the
                %   record of the allocated-but-substitured-out middle version
                %   will remain in this field.
                store_last_id_map           ::  last_id_map,

                % We use this slot to store informational messages that
                % we want to print *only* in the presence of mode errors,
                % because
                %
                % - in the presence of mode errors, they can sometimes
                %   pinpoint the *cause* of some of those errors, but
                % - in the absence of mode errors, the messages would be
                %   only clutter.
                %
                % As of 2025 feb 20, these messages are all about situations
                % in which a disjunction or an if-then-else initialises
                % a state variable in some branches but not in others.
                store_missing_init_specs    ::  list(error_spec)
            ).

    % Create a new svar_state/store set up to start processing a clause head.
    %
:- func new_svar_state = svar_state.
:- func new_svar_store = svar_store.

new_svar_state = svar_state(map.init).
new_svar_store = svar_store(counter.uinit(1u), map.init, map.init, []).

:- type state_var_name_source
    --->    name_initial
    ;       name_middle
    ;       name_final.

:- pred new_state_var_instance(svar::in, state_var_name_source::in,
    prog_var::out, unravel_info::in, unravel_info::out) is det.

new_state_var_instance(StateVar, NameSource, Var, !UrInfo) :-
    VarSet0 = !.UrInfo ^ ui_varset,
    SVarName = varset.lookup_name(VarSet0, StateVar),
    (
        NameSource = name_initial,
        ProgVarName = initial_state_var_name(SVarName),
        varset.new_named_var(ProgVarName, Var, VarSet0, VarSet)
    ;
        NameSource = name_middle,
        SVarState0 = !.UrInfo ^ ui_state_var_store,
        LastIdMap0 = SVarState0 ^ store_last_id_map,
        ( if map.search(LastIdMap0, StateVar, LastId0) then
            CurId = LastId0 + 1u,
            map.det_update(StateVar, CurId, LastIdMap0, LastIdMap)
        else
            CurId = 1u,
            map.det_insert(StateVar, 1u, LastIdMap0, LastIdMap)
        ),
        SVarState = SVarState0 ^ store_last_id_map := LastIdMap,
        !UrInfo ^ ui_state_var_store := SVarState,
        ProgVarName = string.format("STATE_VARIABLE_%s_%u",
            [s(SVarName), u(CurId)]),
        varset.new_named_var(ProgVarName, Var, VarSet0, VarSet)
    ;
        NameSource = name_final,
        ProgVarName = string.format("STATE_VARIABLE_%s", [s(SVarName)]),
        varset.new_named_var(ProgVarName, Var, VarSet0, VarSet)
    ),
    !UrInfo ^ ui_varset := VarSet.

initial_state_var_name(SVarName) = ProgVarName :-
    ProgVarName = string.format("STATE_VARIABLE_%s_0", [s(SVarName)]).

is_prog_var_for_some_state_var(VarSet, Var, SVarName) :-
    % All prog_vars representing state vars are named.
    varset.search_name(VarSet, Var, VarName),
    string.remove_prefix("STATE_VARIABLE_", VarName, AfterStdPrefix),
    UnderscoreSeparatedPieces = string.split_at_char('_', AfterStdPrefix),
    (
        UnderscoreSeparatedPieces = [],
        % There is no underscore, so there can be no numerical suffix.
        SVarName = AfterStdPrefix
    ;
        UnderscoreSeparatedPieces = [_ | _],
        list.det_last(UnderscoreSeparatedPieces, LastPiece),
        string.to_int(LastPiece, _N),
        % This is either the initial or a middle version of SVarName.
        % (Initial if _N is zero, and middle otherwise)
        NumericalSuffix = "_" ++ LastPiece,
        SVarName = string.det_remove_suffix(AfterStdPrefix, NumericalSuffix)
    ).

    % is_prog_var_for_state_var(VarSet, StateVarName, ProgVar):
    %
    % Succeed if and only if ProgVar's name indicates that it represents
    % a version of the state variable named !StateVarName. Succeed for
    % any version: initial, middle, or final.
    %
    % This was a first attempt at solving the problem that the predicate above
    % is also addressing, before I realized that collecting all referenced
    % state variables permits a simpler design in our caller than having to
    % repeat the test (on *every* variable in a clauses_info's varset) for
    % every state var of interest to the code generating unneeded state
    % warnings.
    %
:- pred is_prog_var_for_state_var(prog_varset::in, string::in, prog_var::in)
   is semidet.
:- pragma consider_used(pred(is_prog_var_for_state_var/3)).

is_prog_var_for_state_var(VarSet, SVarName, Var) :-
    % All prog_vars representing state vars are named.
    varset.search_name(VarSet, Var, VarName),
    string.remove_prefix("STATE_VARIABLE_", VarName, AfterStdPrefix),
    string.remove_prefix(SVarName, AfterStdPrefix, Suffix),
    (
        Suffix = ""
        % This is the final version of  SVarName.
    ;
        string.remove_prefix("_", Suffix, SuffixAfterUnderscore),
        string.to_int(SuffixAfterUnderscore, _N)
        % This is either the initial or a middle version of SVarName.
        % (Initial if _N is zero, and middle otherwise)
    ).

%---------------------------------------------------------------------------%
%
% Expand !S into !.S, !:S pairs.
%

expand_bang_state_pairs_in_terms([], []).
expand_bang_state_pairs_in_terms([HeadArg0 | TailArgs0], Args) :-
    expand_bang_state_pairs_in_terms(TailArgs0, TailArgs),
    (
        HeadArg0 = variable(_, _),
        Args = [HeadArg0 | TailArgs]
    ;
        HeadArg0 = functor(Const, FunctorArgs, Context),
        ( if
            Const = atom("!"),
            FunctorArgs = [variable(_StateVar, _)]
        then
            HeadArg1 = functor(atom("!."), FunctorArgs, Context),
            HeadArg2 = functor(atom("!:"), FunctorArgs, Context),
            Args = [HeadArg1, HeadArg2 | TailArgs]
        else
            Args = [HeadArg0 | TailArgs]
        )
    ).

expand_bang_state_pairs_in_instance_method(IM0, IM) :-
    IM0 = instance_method(MethodId0, ProcDef0, Context),
    MethodId0 = pred_pf_name_arity(PredOrFunc, MethodSymName, _UserArity0),
    (
        ProcDef0 = instance_proc_def_name(_),
        IM = IM0
    ;
        ProcDef0 = instance_proc_def_clauses(ItemClausesCord0),
        cord.map_pred(expand_bang_state_pairs_in_clause,
            ItemClausesCord0, ItemClausesCord),
        % Note that ItemClausesCord0 should never be empty...
        ( if cord.head(ItemClausesCord, ItemClause) then
            Args = ItemClause ^ cl_head_args,
            PredFormArity = arg_list_arity(Args),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            MethodId = pred_pf_name_arity(PredOrFunc, MethodSymName, UserArity)
        else
            MethodId = MethodId0
        ),
        ProcDef = instance_proc_def_clauses(ItemClausesCord),
        IM = instance_method(MethodId, ProcDef, Context)
    ).

:- pred expand_bang_state_pairs_in_clause(item_clause_info::in,
    item_clause_info::out) is det.

expand_bang_state_pairs_in_clause(ItemClause0, ItemClause) :-
    ItemClause0 = item_clause_info(PredOrFunc, SymName, Args0, VarSet,
        MaybeBody, Context, SeqNum),
    expand_bang_state_pairs_in_terms(Args0, Args),
    ItemClause = item_clause_info(PredOrFunc, SymName, Args, VarSet,
        MaybeBody, Context, SeqNum).

%---------------------------------------------------------------------------%
%
% Handle the start of processing a clause.
%

:- type maybe_statevar_arg_pos
    --->    arg_old(uint)
    ;       arg_new(uint)
    ;       non_arg.

:- type new_statevar_map ==
    one_or_more_map(prog_var, maybe_statevar_arg_pos).

:- func init_new_statevar_map = new_statevar_map.

init_new_statevar_map = one_or_more_map.init.

svar_prepare_for_clause_head(ModuleInfo0, QualInfo0, VarSet0,
        Args0, Args, FinalMap, NewSVars, !:SVarState, !:UrInfo) :-
    module_info_get_globals(ModuleInfo0, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    Threshold = OptTuple ^ ot_from_ground_term_threshold,
    !:SVarState = new_svar_state,
    SVarStore0 = new_svar_store,
    Specs0 = [],
    !:UrInfo = unravel_info(ModuleInfo0, Threshold, QualInfo0, VarSet0,
        SVarStore0, Specs0),
    svar_prepare_head_terms(0u, 1u, Args0, Args, map.init, FinalMap,
        !SVarState, init_new_statevar_map, NewSVars, !UrInfo).

:- pred svar_prepare_head_terms(uint::in, uint::in,
    list(prog_term)::in, list(prog_term)::out,
    map(svar, prog_var)::in, map(svar, prog_var)::out,
    svar_state::in, svar_state::out,
    new_statevar_map::in, new_statevar_map::out,
    unravel_info::in, unravel_info::out) is det.

svar_prepare_head_terms(_, _, [], [],
        !FinalMap, !SVarState, !NewSVars, !UrInfo).
svar_prepare_head_terms(CurDepth, CurArgNum, [Term0 | Terms0], [Term | Terms],
        !FinalMap, !SVarState, !NewSVars, !UrInfo) :-
    svar_prepare_head_term(CurDepth, CurArgNum, Term0, Term,
        !FinalMap, !SVarState, !NewSVars, !UrInfo),
    svar_prepare_head_terms(CurDepth, CurArgNum + 1u, Terms0, Terms,
        !FinalMap, !SVarState, !NewSVars, !UrInfo).

:- pred svar_prepare_head_term(uint::in, uint::in,
    prog_term::in, prog_term::out,
    map(svar, prog_var)::in, map(svar, prog_var)::out,
    svar_state::in, svar_state::out,
    new_statevar_map::in, new_statevar_map::out,
    unravel_info::in, unravel_info::out) is det.

svar_prepare_head_term(CurDepth, CurArgNum, Term0, Term,
        !FinalMap, !SVarState, !NewSVars, !UrInfo) :-
    (
        Term0 = variable(_, _),
        Term = Term0
    ;
        Term0 = functor(Functor, SubTerms0, Context),
        ( if
            Functor = atom("!."),
            SubTerms0 = [variable(StateVar, _)]
        then
            !.SVarState = svar_state(StatusMap0),
            ( if map.search(StatusMap0, StateVar, OldStatus) then
                (
                    OldStatus = status_unknown,
                    % !:S happened to precede !.S in the head, which is ok.
                    new_state_var_instance(StateVar, name_initial, Var,
                        !UrInfo),
                    Term = variable(Var, Context),
                    Status = status_known(Var),
                    map.det_update(StateVar, Status, StatusMap0, StatusMap)
                ;
                    OldStatus = status_known(Var),
                    Term = variable(Var, Context),
                    StatusMap = StatusMap0
                ;
                    OldStatus = status_unknown_updated(_),
                    unexpected($pred, "status_unknown_updated for !.")
                ;
                    OldStatus = status_known_updated(_, _),
                    unexpected($pred, "status_known_updated for !.")
                ;
                    OldStatus = status_known_ro(_, _, _),
                    % This can happen if the context outside a lambda
                    % expression has a state variable named StateVar,
                    % which make_svars_read_only has given this status,
                    % and the lambda expression itself also has !.StateVar.
                    new_state_var_instance(StateVar, name_initial, Var,
                        !UrInfo),
                    Term = variable(Var, Context),
                    Status = status_known(Var),
                    map.det_update(StateVar, Status, StatusMap0, StatusMap)
                )
            else
                new_state_var_instance(StateVar, name_initial, Var, !UrInfo),
                Term = variable(Var, Context),
                Status = status_known(Var),
                map.det_insert(StateVar, Status, StatusMap0, StatusMap)
            ),
            !:SVarState = svar_state(StatusMap),
            ( if CurDepth = 0u then
                MaybeArgPos = arg_old(CurArgNum)
            else
                MaybeArgPos = non_arg
            ),
            one_or_more_map.add(StateVar, MaybeArgPos, !NewSVars)
        else if
            Functor = atom("!:"),
            SubTerms0 = [variable(StateVar, _)]
        then
            new_state_var_instance(StateVar, name_final, Var, !UrInfo),
            Term = variable(Var, Context),
            Status = status_unknown,

            !.SVarState = svar_state(StatusMap0),
            ( if map.search(StatusMap0, StateVar, OldStatus) then
                (
                    OldStatus = status_unknown,
                    % This is the second occurrence of !:StateVar.
                    % Since !.FinalMap will contain StateVar, we will generate
                    % the error message below.
                    StatusMap = StatusMap0
                ;
                    OldStatus = status_known(_),
                    % The !. part of this state var has already been processed.
                    % We have nothing more to do.
                    StatusMap = StatusMap0
                ;
                    OldStatus = status_unknown_updated(_),
                    unexpected($pred, "status_unknown_updated for !:")
                ;
                    OldStatus = status_known_updated(_, _),
                    unexpected($pred, "status_known_updated for !:")
                ;
                    OldStatus = status_known_ro(_, _, _),
                    % This can happen if the context outside a lambda
                    % expression has a state variable named StateVar,
                    % which make_svars_read_only has given this status,
                    % and the lambda expression itself also has !:StateVar.
                    map.det_update(StateVar, Status, StatusMap0, StatusMap)
                )
            else
                map.det_insert(StateVar, Status, StatusMap0, StatusMap)
            ),
            !:SVarState = svar_state(StatusMap),
            map.search_insert(StateVar, Var, MaybeOldVar, !FinalMap),
            (
                MaybeOldVar = yes(_),
                report_repeated_head_state_var(Context, StateVar, !UrInfo)
            ;
                MaybeOldVar = no
            ),
            ( if CurDepth = 0u then
                MaybeArgPos = arg_new(CurArgNum)
            else
                MaybeArgPos = non_arg
            ),
            one_or_more_map.add(StateVar, MaybeArgPos, !NewSVars)
        else
            svar_prepare_head_terms(CurDepth + 1u, 1u, SubTerms0, SubTerms,
                !FinalMap, !SVarState, !NewSVars, !UrInfo),
            Term = functor(Functor, SubTerms, Context)
        )
    ).

%---------------------------------------------------------------------------%
%
% Handle the start of processing a lambda expression.
%

svar_prepare_for_lambda_head(Context, Args0, Args, FinalMap,
        NewSVars, OutsideState, InsideState, !UrInfo) :-
    % Make all currently visible state vars readonly, since they cannot
    % be updated inside the lambda expression.
    %
    % Note that some of these state vars may already be readonly, since
    % we may already be inside e.g. a lambda expression. We must make sure
    % that readonly references work even from code that is inside two or more
    % lambda expressions.
    OutsideState = svar_state(OutsideStatusMap),
    map.to_sorted_assoc_list(OutsideStatusMap, OutsideStatusList),
    make_svars_read_only(roc_lambda, Context,
        OutsideStatusList, InsideStatusList),
    map.from_sorted_assoc_list(InsideStatusList, InsideStatusMap),
    InsideState0 = svar_state(InsideStatusMap),

    % Handle the arguments of the lambda expression as if they were the head
    % of a clause.
    svar_prepare_head_terms(0u, 1u, Args0, Args, map.init, FinalMap,
        InsideState0, InsideState, init_new_statevar_map, NewSVars, !UrInfo).

:- pred make_svars_read_only(readonly_context_kind::in, prog_context::in,
    assoc_list(svar, svar_status)::in, assoc_list(svar, svar_status)::out)
    is det.

make_svars_read_only(_ROC, _Context, [], []).
make_svars_read_only(ROC, Context, [SVar - CurStatus | CurTail], LambdaList) :-
    make_svars_read_only(ROC, Context, CurTail, LambdaTail),
    (
        ( CurStatus = status_unknown
        ; CurStatus = status_unknown_updated(_)
        ),
        LambdaList = LambdaTail
    ;
        CurStatus = status_known_ro(_, _, _),
        LambdaList = [SVar - CurStatus | LambdaTail]
    ;
        ( CurStatus = status_known(Var)
        ; CurStatus = status_known_updated(Var, _)
        ),
        LambdaStatus = status_known_ro(Var, ROC, Context),
        LambdaList = [SVar - LambdaStatus | LambdaTail]
    ).

%---------------------------------------------------------------------------%
%
% Handle the end of processing a clause or lambda expression.
%

svar_finish_clause_body(Context, NewSVars, FinalMap,
        InitialSVarState, FinalSVarState,
        HeadUnificationsGoal, BodyGoal0, Goal,
        StateVarSpecs, UnusedSVarDescs, !UrInfo) :-
    svar_finish_body(Context, FinalMap, [HeadUnificationsGoal, BodyGoal0],
        Goal1, InitialSVarState, FinalSVarState, !UrInfo),
    SVarStore1 = !.UrInfo ^ ui_state_var_store,
    SVarStore1 = svar_store(_, DelayedRenamings, LastIdMap, StateVarSpecs),
    ( if
        map.is_empty(FinalMap),
        map.is_empty(DelayedRenamings)
    then
        Goal = Goal1
    else
        trace [compiletime(flag("state-var-lambda")), io(!IO)] (
            ModuleInfo = !.UrInfo ^ ui_module_info,
            module_info_get_globals(ModuleInfo, Globals),
            module_info_get_name(ModuleInfo, ModuleName),
            get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
            map.to_assoc_list(FinalMap, FinalList),
            map.to_assoc_list(DelayedRenamings, DelayedList),
            InitialSVarState = svar_state(InitialSVarStateMap),
            map.to_assoc_list(InitialSVarStateMap, InitialSVarStateList),
            FinalSVarState = svar_state(FinalSVarStateMap),
            map.to_assoc_list(FinalSVarStateMap, FinalSVarStateList),

            io.write_string(DebugStream, "\nFINISH CLAUSE BODY in ", !IO),
            io.write_line(DebugStream, Context, !IO),
            io.write_string(DebugStream, "applying subn\n", !IO),
            io.write_line(DebugStream, FinalList, !IO),
            io.write_string(DebugStream, "with incremental subn\n", !IO),
            io.write_line(DebugStream, DelayedList, !IO),
            io.write_string(DebugStream, "with initial svar states\n", !IO),
            io.write_line(DebugStream, InitialSVarStateList, !IO),
            io.write_string(DebugStream, "with final svar states\n", !IO),
            io.write_line(DebugStream, FinalSVarStateList, !IO),
            io.nl(DebugStream, !IO)
        ),
        incremental_rename_vars_in_goal(map.init, DelayedRenamings,
            Goal1, Goal2),
        delete_unneeded_copy_goals_in_clause(HeadUnificationsGoal,
            Goal2, Goal)
    ),
    VarSet = !.UrInfo ^ ui_varset,
    find_unused_statevar_args(VarSet, NewSVars, LastIdMap, UnusedSVarDescs).

svar_finish_lambda_body(Context, Modes, NewSVars, FinalMap, ParseTreeGoal,
        Goals0, Goal, InitialSVarState, FinalSVarState, !UrInfo) :-
    svar_finish_body(Context, FinalMap, Goals0, Goal,
        InitialSVarState, FinalSVarState, !UrInfo),
    VarSet = !.UrInfo ^ ui_varset,
    LastIdMap = !.UrInfo ^ ui_state_var_store ^ store_last_id_map,
    find_unused_statevar_args(VarSet, NewSVars, LastIdMap, UnusedSVarDescs),
    report_any_unneeded_svars_in_lambda(Context, Modes, ParseTreeGoal, Goal,
        UnusedSVarDescs, !UrInfo).

:- pred svar_finish_body(prog_context::in, map(svar, prog_var)::in,
    list(hlds_goal)::in, hlds_goal::out, svar_state::in, svar_state::in,
    unravel_info::in, unravel_info::out) is det.

svar_finish_body(Context, FinalMap, Goals0, Goal,
        InitialSVarState, FinalSVarState, !UrInfo) :-
    map.to_assoc_list(FinalMap, FinalAssocList),
    InitialSVarState = svar_state(InitialSVarStatusMap),
    FinalSVarState = svar_state(FinalSVarStatusMap),
    svar_find_final_renames_and_copy_goals(FinalAssocList,
        InitialSVarStatusMap, FinalSVarStatusMap,
        [], FinalSVarSubn, [], CopyGoals),
    (
        CopyGoals = [],
        Goals1 = Goals0
    ;
        CopyGoals = [_ | _],
        Goals1 = Goals0 ++ CopyGoals
    ),
    svar_flatten_conj(Context, Goals1, Goal1, !UrInfo),

    Goal1 = hlds_goal(GoalExpr1, GoalInfo1),
    GoalId1 = goal_info_get_goal_id(GoalInfo1),
    SVarStore1 = !.UrInfo ^ ui_state_var_store,
    SVarStore1 = svar_store(NextGoalId1, DelayedRenamingMap1,
        LastIdMap1, StateVarSpecs1),
    ( if map.search(DelayedRenamingMap1, GoalId1, DelayedRenaming0) then
        trace [compiletime(flag("state-var-lambda")), io(!IO)] (
            ModuleInfo = !.UrInfo ^ ui_module_info,
            module_info_get_globals(ModuleInfo, Globals),
            module_info_get_name(ModuleInfo, ModuleName),
            get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
            io.write_string(DebugStream, "\nfinishing body, ", !IO),
            io.write_string(DebugStream,
                "attaching subn to existing goal_id ", !IO),
            io.write_line(DebugStream, GoalId1, !IO),
            io.write_string(DebugStream, "subn is ", !IO),
            io.write_line(DebugStream, FinalSVarSubn, !IO)
        ),

        map.det_update(GoalId1, DelayedRenaming0 ++ FinalSVarSubn,
        DelayedRenamingMap1, DelayedRenamingMap),
        NextGoalId = NextGoalId1,
        Goal = Goal1
    else
        (
            FinalSVarSubn = [],
            NextGoalId = NextGoalId1,
            DelayedRenamingMap = DelayedRenamingMap1,
            Goal = Goal1
        ;
            FinalSVarSubn = [_ | _],
            counter.uallocate(GoalIdNum, NextGoalId1, NextGoalId),
            GoalId = goal_id(GoalIdNum),

            trace [compiletime(flag("state-var-lambda")), io(!IO)] (
                ModuleInfo = !.UrInfo ^ ui_module_info,
                module_info_get_globals(ModuleInfo, Globals),
                module_info_get_name(ModuleInfo, ModuleName),
                get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
                io.write_string(DebugStream, "\nfinishing body, ", !IO),
                io.write_string(DebugStream,
                    "attaching subn to new goal_id ", !IO),
                io.write_line(DebugStream, GoalId, !IO),
                io.write_string(DebugStream, "subn is ", !IO),
                io.write_line(DebugStream, FinalSVarSubn, !IO)
            ),

            map.det_insert(GoalId, FinalSVarSubn,
                DelayedRenamingMap1, DelayedRenamingMap),
            goal_info_set_goal_id(GoalId, GoalInfo1, GoalInfo),
            Goal = hlds_goal(GoalExpr1, GoalInfo)
        )
    ),
    SVarStore = svar_store(NextGoalId, DelayedRenamingMap,
        LastIdMap1, StateVarSpecs1),
    !UrInfo ^ ui_state_var_store := SVarStore.

:- pred svar_find_final_renames_and_copy_goals(assoc_list(svar, prog_var)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::in,
    assoc_list(prog_var, prog_var)::in, assoc_list(prog_var, prog_var)::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

svar_find_final_renames_and_copy_goals([], _, _, !FinalSVarSubn, !CopyGoals).
svar_find_final_renames_and_copy_goals([Head | Tail],
        InitialStatusMap, FinalStatusMap, !FinalSVarSubn, !CopyGoals) :-
    Head = SVar - FinalHeadVar,
    map.lookup(InitialStatusMap, SVar, InitialStatus),
    map.lookup(FinalStatusMap, SVar, FinalStatus),
    (
        FinalStatus = status_known(LastVar),
        ( if FinalStatus = InitialStatus then
            % The state variable was not updated by the body.
            % Leaving the unification between two headvars representing the
            % initial and final states to the done at the start of the clause
            % causes problems at the moment for the mode checker in the
            % presence of unique modes.
            make_copy_goal(LastVar, FinalHeadVar, CopyGoal),
            !:CopyGoals = [CopyGoal | !.CopyGoals]
        else
            !:FinalSVarSubn = [LastVar - FinalHeadVar | !.FinalSVarSubn]
        )
    ;
        FinalStatus = status_unknown
        % The state variable was never defined.
        % The clause head already refers to the final version.
    ;
        FinalStatus = status_known_ro(_, _, _),
        unexpected($pred, "readonly status")
    ;
        ( FinalStatus = status_known_updated(_, _)
        ; FinalStatus = status_unknown_updated(_)
        ),
        unexpected($pred, "updated status")
    ),
    svar_find_final_renames_and_copy_goals(Tail,
        InitialStatusMap, FinalStatusMap, !FinalSVarSubn, !CopyGoals).

%---------------------------------------------------------------------------%
%
% Handle the completion of an atomic goal. Any variable that was updated in the
% goal gets the updated value as its new current value. The Loc argument is
% needed because sometimes what looks like an atomic goal (such as the
% condition of an if-then-else) is inside another atomic goal (such as an
% if-then-else expression). In such cases, the end of the inside atomic goal
% does NOT mean that we finished the containing atomic goal.
%

svar_finish_atomic_goal(Loc, !SVarState) :-
    (
        Loc = loc_whole_goal,
        !.SVarState = svar_state(StatusMap0),
        map.map_values_only(reset_updated_status, StatusMap0, StatusMap),
        !:SVarState = svar_state(StatusMap)
    ;
        Loc = loc_inside_atomic_goal
    ).

:- pred reset_updated_status(svar_status::in, svar_status::out) is det.

reset_updated_status(!Status) :-
    (
        ( !.Status = status_unknown
        ; !.Status = status_known_ro(_, _, _)
        ; !.Status = status_known(_)
        )
    ;
        !.Status = status_unknown_updated(NewProgVar),
        !:Status = status_known(NewProgVar)
    ;
        !.Status = status_known_updated(_OldProgVar, NewProgVar),
        !:Status = status_known(NewProgVar)
    ).

%---------------------------------------------------------------------------%
%
% Handle scopes that introduce state variables.
%

svar_prepare_for_local_state_vars(Context, StateVars,
        OutsideState, InsideState, !UrInfo) :-
    OutsideState = svar_state(StatusMapOutside),
    prepare_svars_for_scope(Context, StateVars,
        StatusMapOutside, StatusMapInside, !UrInfo),
    InsideState = svar_state(StatusMapInside).

:- pred prepare_svars_for_scope(prog_context::in, list(svar)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::out,
    unravel_info::in, unravel_info::out) is det.

prepare_svars_for_scope(_Context, [], !StatusMap, !UrInfo).
prepare_svars_for_scope(Context, [SVar | SVars],
        !StatusMap, !UrInfo) :-
    ( if map.search(!.StatusMap, SVar, _OldStatus) then
        report_state_var_shadow(Context, SVar, !UrInfo),
        map.det_update(SVar, status_unknown, !StatusMap)
    else
        map.det_insert(SVar, status_unknown, !StatusMap)
    ),
    prepare_svars_for_scope(Context, SVars, !StatusMap, !UrInfo).

svar_finish_local_state_vars(UrInfo, StateVars,
        StateBeforeOutside, StateAfterInside, StateAfterOutside) :-
    StateBeforeOutside = svar_state(StatusMapBeforeOutside),
    StateAfterInside = svar_state(StatusMapAfterInside),
    trace [compiletime(flag("state-var-scope")), io(!IO)] (
        ModuleInfo = UrInfo ^ ui_module_info,
        module_info_get_globals(ModuleInfo, Globals),
        module_info_get_name(ModuleInfo, ModuleName),
        get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
        map.to_assoc_list(StatusMapBeforeOutside, BeforeOutsideStatuses),
        map.to_assoc_list(StatusMapAfterInside, AfterInsideStatuses),
        io.write_string(DebugStream, "Finish of scope\n", !IO),
        io.write_string(DebugStream, "quantified state vars\n", !IO),
        io.write_line(DebugStream, StateVars, !IO),
        io.write_string(DebugStream, "status before outside\n", !IO),
        list.foldl(io.write_line(DebugStream), BeforeOutsideStatuses, !IO),
        io.write_string(DebugStream, "status after inside\n", !IO),
        list.foldl(io.write_line(DebugStream), AfterInsideStatuses, !IO)
    ),
    % Remove access to the state vars introduced in the scope.
    % Leave the status of all other state vars unaffected.
    StatusMapAfterOutside0 = StatusMapAfterInside,
    finish_svars_for_scope(StateVars, StatusMapBeforeOutside,
        StatusMapAfterOutside0, StatusMapAfterOutside),
    StateAfterOutside = svar_state(StatusMapAfterOutside).

:- pred finish_svars_for_scope(list(svar)::in, map(svar, svar_status)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::out) is det.

finish_svars_for_scope([], _, !StatusMapAfterOutside).
finish_svars_for_scope([SVar | SVars], StatusMapBeforeOutside,
        !StatusMapAfterOutside) :-
    ( if map.search(StatusMapBeforeOutside, SVar, BeforeOutsideStatus) then
        % The state var was visible before the scope. The outside state var
        % was shadowed by a state var in the scope. Now that we are leaving
        % the scope, restore access to the outside state var. Due to the
        % shadowing, its status couldn't have changed inside the scope.
        map.det_update(SVar, BeforeOutsideStatus, !StatusMapAfterOutside)
    else
        % The state var introduced in the scope wasn't visible before it.
        map.det_remove(SVar, _, !StatusMapAfterOutside)
    ),
    finish_svars_for_scope(SVars, StatusMapBeforeOutside,
        !StatusMapAfterOutside).

%---------------------------------------------------------------------------%
%
% Handle disjunctions. The algorithm we use has two passes over the disjuncts.
%
% - Pass 1 finds out, for each state variable known at the start of the
%   disjunction, whether it was updated by any arms, and if yes, it picks
%   the final prog_var from one of the updated arms to represent the state var
%   after the disjunction.
%
% - Pass 2 processes the arms to ensure that the picked prog_var represents
%   the final value of the state variable in all the arms. In arms that do not
%   update the state variable, it introduces unifications to copy the initial
%   value of the state var to be the final value. In arms that do update the
%   state var, it schedules the prog_var representing the final value in
%   that arm to be renamed to the picked prog_var.

svar_finish_disjunction(DisjStates, Disjs, StateBefore, StateAfter, !UrInfo) :-
    StateBefore = svar_state(StatusMapBefore),
    ( if map.is_empty(StatusMapBefore) then
        % Optimize the common case.
        get_disjuncts_with_empty_states(DisjStates, [], RevDisjs),
        list.reverse(RevDisjs, Disjs),
        StateAfter = StateBefore
    else
        map.to_sorted_assoc_list(StatusMapBefore, StatusListBefore),
        compute_status_after_arms(StatusListBefore, DisjStates,
            map.init, ChangedStatusMapAfter, StatusMapBefore, StatusMapAfter),
        map.to_sorted_assoc_list(ChangedStatusMapAfter,
            ChangedStatusListAfter),
        StateAfter = svar_state(StatusMapAfter),

        VarSet0 = !.UrInfo ^ ui_varset,
        SVarStore0 = !.UrInfo ^ ui_state_var_store,
        SVarStore0 = svar_store(NextGoalId0, DelayedRenamings0,
            LastIdMap0, SVarSpecs0),
        merge_changes_made_by_arms(VarSet0, DisjStates, StatusMapBefore,
            ChangedStatusListAfter, [], RevDisjs,
            NextGoalId0, NextGoalId, DelayedRenamings0, DelayedRenamings,
            SVarSpecs0, SVarSpecs),
        list.reverse(RevDisjs, Disjs),
        SVarStore = svar_store(NextGoalId, DelayedRenamings,
            LastIdMap0, SVarSpecs),
        !UrInfo ^ ui_state_var_store := SVarStore
    ).

:- pred get_disjuncts_with_empty_states(list(hlds_goal_svar_state)::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

get_disjuncts_with_empty_states([], !RevDisjuncts).
get_disjuncts_with_empty_states([GoalState | GoalStates], !RevDisjuncts) :-
    GoalState = hlds_goal_svar_state(Goal, State),
    StatusMapAfterGoal = State ^ state_status_map,
    expect(map.is_empty(StatusMapAfterGoal), $pred,
        "map after goal not empty"),
    !:RevDisjuncts = [Goal | !.RevDisjuncts],
    get_disjuncts_with_empty_states(GoalStates, !RevDisjuncts).

    % Pass 1. Compute the changes in the status map.
    %
:- pred compute_status_after_arms(assoc_list(svar, svar_status)::in,
    list(hlds_goal_svar_state)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::out,
    map(svar, svar_status)::in, map(svar, svar_status)::out) is det.

compute_status_after_arms(_StatusListBefore, [],
        !ChangedStatusMapAfter, !StatusMapAfter).
compute_status_after_arms(StatusListBefore, [ArmState | ArmStates],
        !ChangedStatusMapAfter, !StatusMapAfter) :-
    ArmState = hlds_goal_svar_state(_Armunct, StateAfterArm),
    StatusMapAfterArm = StateAfterArm ^ state_status_map,
    find_changes_in_arm_and_update_changed_status_map(StatusListBefore,
        StatusMapAfterArm, !ChangedStatusMapAfter, !StatusMapAfter),
    compute_status_after_arms(StatusListBefore, ArmStates,
        !ChangedStatusMapAfter, !StatusMapAfter).

:- pred find_changes_in_arm_and_update_changed_status_map(
    assoc_list(svar, svar_status)::in, map(svar, svar_status)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::out,
    map(svar, svar_status)::in, map(svar, svar_status)::out) is det.

find_changes_in_arm_and_update_changed_status_map([], _,
        !ChangedStatusMapAfter, !StatusMapAfter).
find_changes_in_arm_and_update_changed_status_map([Before | Befores],
        StatusMapAfterArm, !ChangedStatusMapAfter, !StatusMapAfter) :-
    Before = SVar - StatusBefore,
    map.lookup(StatusMapAfterArm, SVar, StatusAfter),
    ( if StatusBefore = StatusAfter then
        true
    else
        ( if map.search(!.ChangedStatusMapAfter, SVar, _AlreadyUpdated) then
            true
        else
            map.det_insert(SVar, StatusAfter, !ChangedStatusMapAfter),
            map.det_update(SVar, StatusAfter, !StatusMapAfter)
        )
    ),
    find_changes_in_arm_and_update_changed_status_map(Befores,
        StatusMapAfterArm, !ChangedStatusMapAfter, !StatusMapAfter).

    % Pass 2. Effect the computed changes in the status map.
    %
:- pred merge_changes_made_by_arms(prog_varset::in,
    list(hlds_goal_svar_state)::in,
    map(svar, svar_status)::in, assoc_list(svar, svar_status)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    ucounter::in, ucounter::out,
    incremental_rename_map::in, incremental_rename_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

merge_changes_made_by_arms(_, [], _, _,
        !RevArms, !NextGoalId, !DelayedRenamings, !Specs).
merge_changes_made_by_arms(VarSet, [ArmState | ArmStates],
        StatusMapBefore, ChangedStatusListAfter,
        !RevArms, !NextGoalId, !DelayedRenamings, !Specs) :-
    ArmState = hlds_goal_svar_state(Arm0, StateAfterArm),
    StatusMapAfterArm = StateAfterArm ^ state_status_map,
    counter.uallocate(ArmIdNum, !NextGoalId),
    ArmId = goal_id(ArmIdNum),
    handle_arm_updated_state_vars(VarSet,
        ChangedStatusListAfter, StatusMapBefore, StatusMapAfterArm,
        UninitVarNames, CopyGoals, ArmRenames),
    map.det_insert(ArmId, ArmRenames, !DelayedRenamings),
    Arm0 = hlds_goal(ArmExpr0, ArmInfo0),
    (
        CopyGoals = [],
        ArmExpr = ArmExpr0
    ;
        CopyGoals = [_ | _],
        svar_goal_to_conj_list_internal(Arm0, ArmGoals0,
            !NextGoalId, !DelayedRenamings),
        ArmExpr = conj(plain_conj, ArmGoals0 ++ CopyGoals)
    ),
    (
        UninitVarNames = []
    ;
        UninitVarNames = [_ | _],
        % It is ok for an arm that cannot succeed not to initialize
        % a variable, but we record an informational message anyway,
        % to be printed in case the procedure has a mode error.
        ArmContext = goal_info_get_context(ArmInfo0),
        report_missing_inits_in_disjunct(ArmContext, UninitVarNames, !Specs)
    ),
    goal_info_set_goal_id(ArmId, ArmInfo0, ArmInfo),
    Arm = hlds_goal(ArmExpr, ArmInfo),
    !:RevArms = [Arm | !.RevArms],
    merge_changes_made_by_arms(VarSet, ArmStates,
        StatusMapBefore, ChangedStatusListAfter,
        !RevArms, !NextGoalId, !DelayedRenamings, !Specs).

:- pred handle_arm_updated_state_vars(prog_varset::in,
    assoc_list(svar, svar_status)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::in, list(string)::out,
    list(hlds_goal)::out, assoc_list(prog_var, prog_var)::out) is det.

handle_arm_updated_state_vars(_, [], _, _, [], [], []).
handle_arm_updated_state_vars(VarSet, [Change | Changes], StatusMapBefore,
        StatusMapAfterArm, UninitVarNames, CopyGoals, Renames) :-
    handle_arm_updated_state_vars(VarSet, Changes,
        StatusMapBefore, StatusMapAfterArm,
        UninitVarNamesTail, CopyGoalsTail, RenamesTail),
    Change = StateVar - AfterAllArmsStatus,
    map.lookup(StatusMapBefore, StateVar, BeforeStatus),
    map.lookup(StatusMapAfterArm, StateVar, AfterArmStatus),
    ( if AfterArmStatus = BeforeStatus then
        expect_not(unify(AfterArmStatus, AfterAllArmsStatus),
            $pred, "AfterArmStatus = AfterAllArmsStatus"),
        (
            % If the state var is readonly in this context, then it shouldn't
            % have been updated by any arms. However, if it was, then we have
            % (a) already generated an error message for it, and (b) changed
            % its status to writeable to suppress duplicate error messages.
            % This is why this code treats known_ro the same as known.
            ( BeforeStatus = status_known(BeforeVar)
            ; BeforeStatus = status_known_ro(BeforeVar, _, _)
            ),
            (
                AfterAllArmsStatus = status_known(AfterAllVar),
                make_copy_goal(BeforeVar, AfterAllVar, CopyGoal),
                CopyGoals = [CopyGoal | CopyGoalsTail],
                UninitVarNames = UninitVarNamesTail,
                Renames = RenamesTail
            ;
                ( AfterAllArmsStatus = status_known_ro(_, _, _)
                ; AfterAllArmsStatus = status_known_updated(_, _)
                ; AfterAllArmsStatus = status_unknown
                ; AfterAllArmsStatus = status_unknown_updated(_)
                ),
                unexpected($pred,
                    "AfterAllArmsStatus != status_known (Before == After)")
            )
        ;
            BeforeStatus = status_unknown,
            varset.lookup_name(VarSet, StateVar, Name),
            UninitVarName = "!:" ++ Name,
            CopyGoals = CopyGoalsTail,
            UninitVarNames = [UninitVarName | UninitVarNamesTail],
            Renames = RenamesTail
        ;
            ( BeforeStatus = status_known_updated(_, _)
            ; BeforeStatus = status_unknown_updated(_)
            ),
            % If the state var was updated before this disjunction,
            % then any reference to !:StateVar should refer to the already
            % known updated prog_var, and thus AfterAllArmsStatus should be
            % the same as StatusBefore, which means we shouldn't get here.
            unexpected($pred, "BeforeStatus is updated")
        )
    else
        (
            AfterArmStatus = status_known(AfterArmVar),
            (
                AfterAllArmsStatus = status_known(AfterAllVar),
                CopyGoals = CopyGoalsTail,
                UninitVarNames = UninitVarNamesTail,
                ( if AfterArmVar = AfterAllVar then
                    Renames = RenamesTail
                else
                    Renames = [AfterArmVar - AfterAllVar | RenamesTail]
                )
            ;
                ( AfterAllArmsStatus = status_known_ro(_, _, _)
                ; AfterAllArmsStatus = status_known_updated(_, _)
                ; AfterAllArmsStatus = status_unknown
                ; AfterAllArmsStatus = status_unknown_updated(_)
                ),
                unexpected($pred,
                    "AfterAllArmsStatus != status_known (Before != After)")
            )
        ;
            AfterArmStatus = status_known_ro(_, _, _),
            unexpected($pred, "AfterArmStatus = status_known_ro")
        ;
            AfterArmStatus = status_known_updated(_, _),
            unexpected($pred, "AfterArmStatus = status_known_updated")
        ;
            AfterArmStatus = status_unknown,
            unexpected($pred, "AfterArmStatus = status_unknown")
        ;
            AfterArmStatus = status_unknown_updated(_),
            unexpected($pred, "AfterArmStatus = status_unknown")
        )
    ).

:- pred make_copy_goal(prog_var::in, prog_var::in, hlds_goal::out) is det.

make_copy_goal(FromVar, ToVar, CopyGoal) :-
    % We can do the copying in one of two ways. Using unifications
    % can cause problems because the (plain, non-unique) mode analysis pass
    % feels free to schedule them in places where the unique mode analysis pass
    % does not like them; specifically, it can cause a di reference to a
    % variable to appear before a ui reference.
    %
    % The alternative is to add a builtin predicate to the standard library
    % that just does copying, and to make make_copy_goal construct a call to
    % that predicate. That predicate would need to be able to be called in
    % three modes: di/uo, mdi/muo and in/out. However, it needs to have inst
    % parameters so that whatever shape information we have about the source
    % (subtype info, higher order mode info), we copy to the target.
    %
    % We generate a unification, and try to ensure that we don't generate
    % di references to state variables before possible ui references. See the
    % comment in svar_find_final_renames_and_copy_goals before the call to
    % make_copy_goal.

    create_pure_atomic_complicated_unification(ToVar, rhs_var(FromVar),
        dummy_context, umc_implicit("state variable"), [], CopyGoal0),
    goal_add_features([feature_do_not_warn_singleton, feature_state_var_copy],
        CopyGoal0, CopyGoal).

%---------------------------------------------------------------------------%
%
% Handle if-then-else goals. The basic idea is the same as for disjunctions,
% but we also have to handle three complications.
%
% First, the first disjunct consists of two parts: the condition and the then
% part, with data flowing between them.
%
% Second, variables can be quantified over the condition and the then part.
%
% Third, the if-then-else need not be a goal; it can also be an expression.
% This means that it is ok for variables to have status known_updated or
% unknown_updated in any of the status maps we handle.
%

svar_finish_if_then_else(LocKind, Context, QuantStateVars,
        ThenGoal0, ThenGoal, ElseGoal0, ElseGoal,
        StateBefore, StateAfterCond, StateAfterThen, StateAfterElse,
        StateAfterITE, !UrInfo) :-
    StateBefore = svar_state(StatusMapBefore),
    StatusMapAfterCond = StateAfterCond ^ state_status_map,
    StatusMapAfterThen = StateAfterThen ^ state_status_map,
    StatusMapAfterElse = StateAfterElse ^ state_status_map,
    map.keys(StatusMapBefore, SVarsBefore),
    map.keys(StatusMapAfterCond, SVarsAfterCond),
    map.keys(StatusMapAfterThen, SVarsAfterThen),
    map.keys(StatusMapAfterElse, SVarsAfterElse),
    expect(list.sublist(SVarsBefore, SVarsAfterCond), $pred,
        "vars Before not sublist of Cond"),
    expect(unify(SVarsBefore, SVarsAfterThen), $pred,
        "vars Before != AfterThen"),
    expect(unify(SVarsBefore, SVarsAfterElse), $pred,
        "vars Before != AfterElse"),

    handle_state_vars_in_ite(LocKind, QuantStateVars,
        SVarsBefore, StatusMapBefore, StatusMapAfterCond,
        StatusMapAfterThen, StatusMapAfterElse,
        map.init, StatusMapAfterITE,
        [], NeckCopyGoals, [], ThenEndCopyGoals, [], ElseEndCopyGoals,
        [], ThenRenames, [], ElseRenames,
        [], ThenMissingInits, [], ElseMissingInits, !UrInfo),
    StateAfterITE = svar_state(StatusMapAfterITE),

    % It is ok for an arm that cannot succeed not to initialize a variable,
    % but we record warnings for them anyway, to be printed in case the
    % procedure has a mode error.
    (
        ThenMissingInits = []
    ;
        ThenMissingInits = [_ | _],
        report_missing_inits_in_ite(Context, ThenMissingInits,
            "succeeds", "fails", !UrInfo)
    ),
    (
        ElseMissingInits = []
    ;
        ElseMissingInits = [_ | _],
        report_missing_inits_in_ite(Context, ElseMissingInits,
            "fails", "succeeds", !UrInfo)
    ),

    svar_goal_to_conj_list(ThenGoal0, ThenGoals0, !UrInfo),
    svar_goal_to_conj_list(ElseGoal0, ElseGoals0, !UrInfo),
    ThenGoals = NeckCopyGoals ++ ThenGoals0 ++ ThenEndCopyGoals,
    ElseGoals = ElseGoals0 ++ ElseEndCopyGoals,
    ThenGoal0 = hlds_goal(_ThenExpr0, ThenInfo0),
    ElseGoal0 = hlds_goal(_ElseExpr0, ElseInfo0),
    conj_list_to_goal(ThenGoals, ThenInfo0, ThenGoal1),
    conj_list_to_goal(ElseGoals, ElseInfo0, ElseGoal1),

    SVarStore0 = !.UrInfo ^ ui_state_var_store,
    SVarStore0 = svar_store(NextGoalId0, DelayedRenamings0,
        LastIdMap0, SVarSpecs0),
    counter.uallocate(ThenGoalIdNum, NextGoalId0, NextGoalId1),
    counter.uallocate(ElseGoalIdNum, NextGoalId1, NextGoalId),
    ThenGoalId = goal_id(ThenGoalIdNum),
    ElseGoalId = goal_id(ElseGoalIdNum),
    goal_set_goal_id(ThenGoalId, ThenGoal1, ThenGoal),
    goal_set_goal_id(ElseGoalId, ElseGoal1, ElseGoal),
    map.det_insert(ThenGoalId, ThenRenames,
        DelayedRenamings0, DelayedRenamings1),
    map.det_insert(ElseGoalId, ElseRenames,
        DelayedRenamings1, DelayedRenamings),
    SVarStore = svar_store(NextGoalId, DelayedRenamings,
        LastIdMap0, SVarSpecs0),
    !UrInfo ^ ui_state_var_store := SVarStore.

:- pred handle_state_vars_in_ite(loc_kind::in, list(svar)::in, list(svar)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    assoc_list(prog_var, prog_var)::in, assoc_list(prog_var, prog_var)::out,
    assoc_list(prog_var, prog_var)::in, assoc_list(prog_var, prog_var)::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out,
    unravel_info::in, unravel_info::out) is det.

handle_state_vars_in_ite(_, _, [], _, _, _, _, !StatusMapAfterITE,
        !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
        !ThenRenames, !ElseRenames, !ThenMissingInits, !ElseMissingInits,
        !UrInfo).
handle_state_vars_in_ite(LocKind, QuantStateVars, [SVar | SVars],
        StatusMapBefore, StatusMapAfterCond,
        StatusMapAfterThen, StatusMapAfterElse, !StatusMapAfterITE,
        !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
        !ThenRenames, !ElseRenames, !ThenMissingInits, !ElseMissingInits,
        !UrInfo) :-
    map.lookup(StatusMapBefore, SVar, StatusBefore),
    map.lookup(StatusMapAfterCond, SVar, StatusAfterCond),
    map.lookup(StatusMapAfterThen, SVar, StatusAfterThen),
    map.lookup(StatusMapAfterElse, SVar, StatusAfterElse),

    ( if list.member(SVar, QuantStateVars) then
        expect(unify(StatusBefore, StatusAfterThen), $pred,
            "state var shadowed in if-then-else is nevertheless updated"),
        % SVar is quantified in the if-then-else. That means that Cond and Then
        % may update a state variable with the same name as SVar, but this
        % won't be SVar itself. The status of SVar itself after Cond and after
        % Then will thus be unchanged. This is why we pass StatusBefore
        % not just for itself, but in place of StatusAfterCond and
        % StatusAfterThen as well.
        handle_state_var_in_ite(LocKind, SVar,
            StatusBefore, StatusBefore, StatusBefore,
            StatusAfterElse, StatusAfterITE,
            !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
            !ElseRenames, !ThenMissingInits, !ElseMissingInits, !UrInfo)
    else
        % If StatusBefore = status_known_ro(_, _, _), then we would expect
        % StatusBefore = StatusAfterCond
        % StatusBefore = StatusAfterThen
        % StatusBefore = StatusAfterElse
        % However, if the user program actually updates a state variable
        % that should be readonly in this scope, then our recovery from that
        % error would invalidate these expectations.

        handle_state_var_in_ite(LocKind, SVar,
            StatusBefore, StatusAfterCond, StatusAfterThen,
            StatusAfterElse, StatusAfterITE,
            !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
            !ElseRenames, !ThenMissingInits, !ElseMissingInits, !UrInfo)
    ),
    map.det_insert(SVar, StatusAfterITE, !StatusMapAfterITE),
    handle_state_vars_in_ite(LocKind, QuantStateVars, SVars,
        StatusMapBefore, StatusMapAfterCond,
        StatusMapAfterThen, StatusMapAfterElse, !StatusMapAfterITE,
        !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
        !ThenRenames, !ElseRenames, !ThenMissingInits, !ElseMissingInits,
        !UrInfo).

:- pred handle_state_var_in_ite(loc_kind::in, svar::in,
    svar_status::in, svar_status::in, svar_status::in, svar_status::in,
    svar_status::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    assoc_list(prog_var, prog_var)::in, assoc_list(prog_var, prog_var)::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out,
    unravel_info::in, unravel_info::out) is det.

handle_state_var_in_ite(LocKind, SVar, StatusBefore,
        StatusAfterCond, StatusAfterThen, StatusAfterElse, StatusAfterITE,
        !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
        !ElseRenames, !ThenMissingInits, !ElseMissingInits, !UrInfo) :-
    trace [compiletime(flag("state-var-ite")), io(!IO)] (
        ModuleInfo = !.UrInfo ^ ui_module_info,
        module_info_get_globals(ModuleInfo, Globals),
        module_info_get_name(ModuleInfo, ModuleName),
        get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
        io.write_string(DebugStream, "state variable ", !IO),
        io.write_line(DebugStream, SVar, !IO),
        io.write_string(DebugStream, "status before: ", !IO),
        io.write_line(DebugStream, StatusBefore, !IO),
        io.write_string(DebugStream, "status after cond: ", !IO),
        io.write_line(DebugStream, StatusAfterCond, !IO),
        io.write_string(DebugStream, "status after then: ", !IO),
        io.write_line(DebugStream, StatusAfterThen, !IO),
        io.write_string(DebugStream, "status after else: ", !IO),
        io.write_line(DebugStream, StatusAfterElse, !IO)
    ),

    % There are eight cases depending on which of Then, Else and Cond
    % update the state variable:
    %
    % #  Then Else  Cond Action
    % A1 no   no    no   do nothing
    % A2 no   no    yes  copy from cond at start of then, copy at end of else
    % B1 no   yes   no   copy at end of then
    % B2 no   yes   yes  copy from cond at start of then
    % C1 yes  no    no   copy at end of else
    % C2 yes  no    yes  copy at end of else
    % D1 yes  yes   no   rename else to match then
    % D2 yes  yes   yes  rename else to match then
    %
    % The nesting structure is:
    %
    % - Did Then update SVar? (Is StatusAfterThen = StatusAfterCond?)
    % - Did Else update SVar? (Is StatusAfterElse = StatusBefore?)
    % - did Cond update SVar? (Is StatusAfterCond = StatusBefore?)
    %
    % Note that if Then updates SVar, as it does in these cases, then
    % it does not matter whether Cond also updates SVar, since its final state
    % will not depend on that intermediate state. This is why we do not
    % differentiate between C1 and C2, and between D1 and D2. And this
    % in turn is the reason for why we test for updates by Then (and Else)
    % before we test for updates by Cond.

    ( if StatusAfterThen = StatusAfterCond then
        % Cases A-B.
        ( if StatusAfterElse = StatusBefore then
            % Case A.
            ( if StatusAfterCond = StatusBefore then
                % Case A1.
                StatusAfterITE = StatusBefore
            else
                % Case A2.
                handle_state_var_in_ite_cond(LocKind, SVar,
                    StatusBefore, StatusAfterCond, StatusAfterITE,
                    !NeckCopyGoals, !ElseEndCopyGoals, !ElseMissingInits,
                    !UrInfo)
            )
        else
            % Case B.
            ( if StatusAfterCond = StatusBefore then
                % Case B1.
                handle_state_var_in_ite_else(!.UrInfo, LocKind, SVar,
                    StatusBefore, StatusAfterElse, StatusAfterITE,
                    !ThenEndCopyGoals, !ThenMissingInits)
            else
                % Case B2.
                handle_state_var_in_ite_cond_else(LocKind,
                    StatusAfterCond, StatusAfterElse, StatusAfterITE,
                    !NeckCopyGoals)
            )
        )
    else
        % Cases C-D.
        ( if StatusAfterElse = StatusBefore then
            % Case C. We handle C1 and C2 the same way.
            handle_state_var_in_ite_maybe_cond_then(!.UrInfo, LocKind,
                SVar, StatusBefore, StatusAfterThen, StatusAfterITE,
                !ElseEndCopyGoals, !ElseMissingInits)
        else
            % Case D. We handle D1 and D2 the same way.
            handle_state_var_in_ite_maybe_cond_then_else(LocKind,
                StatusAfterThen, StatusAfterElse, StatusAfterITE, !ElseRenames)
        )
    ).

:- pred handle_state_var_in_ite_cond(loc_kind::in, svar::in,
    svar_status::in, svar_status::in, svar_status::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(string)::in, list(string)::out,
    unravel_info::in, unravel_info::out) is det.
:- pragma inline(pred(handle_state_var_in_ite_cond/13)).

handle_state_var_in_ite_cond(LocKind, SVar,
        StatusBefore, StatusAfterCond, StatusAfterITE,
        !NeckCopyGoals, !ElseEndCopyGoals, !ElseMissingInits, !UrInfo) :-
    (
        StatusBefore = status_known(VarBefore),
        new_state_var_instance(SVar, name_middle, FinalVar, !UrInfo),
        VarAfterCond = svar_get_current_progvar(LocKind, StatusAfterCond),
        make_copy_goal(VarAfterCond, FinalVar, NeckCopyGoal),
        !:NeckCopyGoals = [NeckCopyGoal | !.NeckCopyGoals],
        make_copy_goal(VarBefore, FinalVar, ElseCopyGoal),
        !:ElseEndCopyGoals = [ElseCopyGoal | !.ElseEndCopyGoals],
        StatusAfterITE = status_known(FinalVar)
    ;
        StatusBefore = status_unknown,
        VarSet = !.UrInfo ^ ui_varset,
        varset.lookup_name(VarSet, SVar, SVarName),
        !:ElseMissingInits = ["!:" ++ SVarName | !.ElseMissingInits],
        % We pretend the else part defines StateVar, since this is
        % the right thing to do when the else part cannot succeed.
        % If it can, we will generate an error message during
        % mode analysis.
        new_state_var_instance(SVar, name_middle, FinalVar, !UrInfo),
        VarAfterCond = svar_get_current_progvar(LocKind, StatusAfterCond),
        make_copy_goal(VarAfterCond, FinalVar, NeckCopyGoal),
        !:NeckCopyGoals = [NeckCopyGoal | !.NeckCopyGoals],
        StatusAfterITE = status_known(FinalVar)
    ;
        StatusBefore = status_known_ro(_, _, _),
        % The update of !SVar in the condition was an error, for which
        % we have already generated an error message. Because of that,
        % this dummy value won't be used.
        % XXX Returning StatusAfterCond would cause fewer cascading
        % error messages, but are those messages useful?
        StatusAfterITE = StatusBefore
    ;
        ( StatusBefore = status_known_updated(_, _)
        ; StatusBefore = status_unknown_updated(_)
        ),
        % This can happen if LocKind = loc_inside_atomic_goal, but
        % any reference to !:SVar in the condition should have
        % just returned the new progvar for SVar.
        unexpected($pred, "updated before (case 5)")
    ).

:- pred handle_state_var_in_ite_else(unravel_info::in, loc_kind::in, svar::in,
    svar_status::in, svar_status::in, svar_status::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(string)::in, list(string)::out) is det.
:- pragma inline(pred(handle_state_var_in_ite_else/10)).

handle_state_var_in_ite_else(UrInfo, LocKind, SVar, StatusBefore,
        StatusAfterElse, StatusAfterITE,
        !ThenEndCopyGoals, !ThenMissingInits) :-
    (
        StatusBefore = status_known(VarBefore),
        VarAfterElse = svar_get_current_progvar(LocKind, StatusAfterElse),
        make_copy_goal(VarBefore, VarAfterElse, CopyGoal),
        !:ThenEndCopyGoals = [CopyGoal | !.ThenEndCopyGoals],
        StatusAfterITE = StatusAfterElse
    ;
        StatusBefore = status_unknown,
        VarSet = UrInfo ^ ui_varset,
        varset.lookup_name(VarSet, SVar, SVarName),
        !:ThenMissingInits = ["!:" ++ SVarName | !.ThenMissingInits],
        % We pretend the then part defines StateVar, since this is the right
        % thing to do when the then part cannot succeed. If it can, we will
        % generate an error message during mode analysis.
        StatusAfterITE = StatusAfterElse
    ;
        StatusBefore = status_known_ro(_, _, _),
        % The update of !SVar in the else case was an error, for which
        % we have already generated an error message. Because of that,
        % this dummy value won't be used.
        % XXX Returning StatusAfterElse would cause fewer cascading
        % error messages, but are those messages useful?
        StatusAfterITE = StatusBefore
    ;
        ( StatusBefore = status_known_updated(_, _)
        ; StatusBefore = status_unknown_updated(_)
        ),
        % This can happen if LocKind = loc_inside_atomic_goal, but
        % any reference to !:SVar in the else case should have just returned
        % the new progvar for SVar.
        unexpected($pred, "updated before (case 2)")
    ).

:- pred handle_state_var_in_ite_cond_else(loc_kind::in,
    svar_status::in, svar_status::in, svar_status::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.
:- pragma inline(pred(handle_state_var_in_ite_cond_else/6)).

handle_state_var_in_ite_cond_else(LocKind,
        StatusAfterCond, StatusAfterElse, StatusAfterITE, !NeckCopyGoals) :-
    VarAfterCond = svar_get_current_progvar(LocKind, StatusAfterCond),
    VarAfterElse = svar_get_current_progvar(LocKind, StatusAfterElse),
    make_copy_goal(VarAfterCond, VarAfterElse, CopyGoal),
    !:NeckCopyGoals = [CopyGoal | !.NeckCopyGoals],
    StatusAfterITE = StatusAfterElse.

:- pred handle_state_var_in_ite_maybe_cond_then(unravel_info::in,
    loc_kind::in, svar::in,
    svar_status::in, svar_status::in, svar_status::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(string)::in, list(string)::out) is det.
:- pragma inline(pred(handle_state_var_in_ite_maybe_cond_then/10)).

handle_state_var_in_ite_maybe_cond_then(UrInfo, LocKind, SVar,
        StatusBefore, StatusAfterThen, StatusAfterITE,
        !ElseEndCopyGoals, !ElseMissingInits) :-
    (
        StatusBefore = status_known(VarBefore),
        VarAfterThen = svar_get_current_progvar(LocKind, StatusAfterThen),
        make_copy_goal(VarBefore, VarAfterThen, CopyGoal),
        !:ElseEndCopyGoals = [CopyGoal | !.ElseEndCopyGoals],
        StatusAfterITE = StatusAfterThen
    ;
        StatusBefore = status_unknown,
        VarSet = UrInfo ^ ui_varset,
        varset.lookup_name(VarSet, SVar, SVarName),
        !:ElseMissingInits = ["!:" ++ SVarName | !.ElseMissingInits],
        % We pretend the else part defines StateVar, since this is the
        % right thing to do when the else part cannot succeed. If it can,
        % we will generate an error message during mode analysis.
        StatusAfterITE = StatusAfterThen
    ;
        StatusBefore = status_known_ro(_, _, _),
        % The updates of !SVar in the condition and then cases were errors,
        % for which we already generated messages. Because of that,
        % this dummy value won't be used.
        % XXX Returning StatusAfterThen would cause fewer cascading
        % error messages, but are those messages useful?
        StatusAfterITE = StatusBefore
    ;
        ( StatusBefore = status_known_updated(_, _)
        ; StatusBefore = status_unknown_updated(_)
        ),
        % This can happen if LocKind = loc_inside_atomic_goal, but
        % any reference to !:SVar in the condition and then case
        % should have just returned the new progvar for SVar.
        unexpected($pred, "updated before (case 7)")
    ).

:- pred handle_state_var_in_ite_maybe_cond_then_else(loc_kind::in,
    svar_status::in, svar_status::in, svar_status::out,
    assoc_list(prog_var, prog_var)::in, assoc_list(prog_var, prog_var)::out)
    is det.
:- pragma inline(pred(handle_state_var_in_ite_maybe_cond_then_else/6)).

handle_state_var_in_ite_maybe_cond_then_else(LocKind,
        StatusAfterThen, StatusAfterElse, StatusAfterITE, !ElseRenames) :-
    VarAfterThen = svar_get_current_progvar(LocKind, StatusAfterThen),
    VarAfterElse = svar_get_current_progvar(LocKind, StatusAfterElse),
    !:ElseRenames = [VarAfterElse - VarAfterThen | !.ElseRenames],
    StatusAfterITE = StatusAfterThen.

%---------------------------------------------------------------------------%
%
% Handle atomic goals. Atomic goals are basically a disjunction between
% the main goal and the orelse goals.
%

:- type svar_outer_atomic_scope_info
    --->    svar_outer_atomic_scope_info(
                soasi_state_var         :: svar,
                soasi_before_status     :: svar_status,
                soasi_after_status      :: svar_status
            )
    ;       no_svar_outer_atomic_scope_info.

svar_start_outer_atomic_scope(Context, OuterStateVar, OuterDIVar, OuterUOVar,
        OuterScopeInfo, !SVarState, !UrInfo) :-
    StatusMap0 = !.SVarState ^ state_status_map,
    ( if map.remove(OuterStateVar, BeforeStatus, StatusMap0, StatusMap) then
        !SVarState ^ state_status_map := StatusMap,
        (
            BeforeStatus = status_unknown,
            report_uninitialized_state_var(warn_dodgy_simple_code, Context,
                OuterStateVar, !UrInfo),
            new_state_var_instance(OuterStateVar, name_middle, OuterDIVar,
                !UrInfo),
            new_state_var_instance(OuterStateVar, name_middle, OuterUOVar,
                !UrInfo),
            OuterScopeInfo = svar_outer_atomic_scope_info(OuterStateVar,
                BeforeStatus, BeforeStatus)
        ;
            BeforeStatus = status_known_ro(OuterDIVar, RO_Construct,
                RO_Context),
            report_illegal_state_var_update(Context,
                ro_construct_name(RO_Construct), RO_Context, OuterStateVar,
                !UrInfo),
            new_state_var_instance(OuterStateVar, name_middle, OuterUOVar,
                !UrInfo),
            OuterScopeInfo = svar_outer_atomic_scope_info(OuterStateVar,
                BeforeStatus, BeforeStatus)
        ;
            BeforeStatus = status_known(OuterDIVar),
            new_state_var_instance(OuterStateVar, name_middle, OuterUOVar,
                !UrInfo),
            AfterStatus = status_known(OuterUOVar),
            OuterScopeInfo = svar_outer_atomic_scope_info(OuterStateVar,
                BeforeStatus, AfterStatus)
        ;
            ( BeforeStatus = status_known_updated(_, _)
            ; BeforeStatus = status_unknown_updated(_)
            ),
            % This status should exist in a status map only when we are in the
            % middle of processing an atomic goal.
            unexpected($pred, "status updated")
        )
    else
        report_non_visible_state_var("", Context, OuterStateVar, !UrInfo),
        new_state_var_instance(OuterStateVar, name_middle, OuterDIVar,
            !UrInfo),
        new_state_var_instance(OuterStateVar, name_middle, OuterUOVar,
            !UrInfo),
        OuterScopeInfo = no_svar_outer_atomic_scope_info
    ).

svar_finish_outer_atomic_scope(OuterScopeInfo, !SVarState) :-
    (
        OuterScopeInfo = svar_outer_atomic_scope_info(OuterStateVar,
            _BeforeStatus, AfterStatus),
        StatusMap0 = !.SVarState ^ state_status_map,
        map.det_insert(OuterStateVar, AfterStatus, StatusMap0, StatusMap),
        !SVarState ^ state_status_map := StatusMap
    ;
        OuterScopeInfo = no_svar_outer_atomic_scope_info
    ).

%---------------------------------------------------------------------------%

:- type svar_inner_atomic_scope_info
    --->    svar_inner_atomic_scope_info(
                siasi_state_var             :: svar,
                siasi_di_var                :: prog_var,
                siasi_state_before          :: svar_state
            ).

svar_start_inner_atomic_scope(_Context, InnerStateVar, InnerScopeInfo,
        !SVarState, !UrInfo) :-
    SVarStateBefore = !.SVarState,
    new_state_var_instance(InnerStateVar, name_initial, InnerDIVar, !UrInfo),
    StatusMap0 = !.SVarState ^ state_status_map,
    map.set(InnerStateVar, status_known(InnerDIVar), StatusMap0, StatusMap),
    !SVarState ^ state_status_map := StatusMap,
    InnerScopeInfo = svar_inner_atomic_scope_info(InnerStateVar, InnerDIVar,
        SVarStateBefore).

svar_finish_inner_atomic_scope(_Context, InnerScopeInfo,
        InnerDIVar, InnerUOVar, !SVarState) :-
    InnerScopeInfo = svar_inner_atomic_scope_info(InnerStateVar, InnerDIVar,
        SVarStateBefore),
    StatusMap0 = !.SVarState ^ state_status_map,
    map.lookup(StatusMap0, InnerStateVar, Status),
    (
        Status = status_known(InnerUOVar)
    ;
        ( Status = status_unknown
        ; Status = status_unknown_updated(_)
        ; Status = status_known_ro(_, _, _)
        ; Status = status_known_updated(_, _)
        ),
        unexpected($pred, "status != known")
    ),
    !:SVarState = SVarStateBefore.

%---------------------------------------------------------------------------%
%
% Look up prog_vars for a state_var.
%

replace_any_dot_colon_state_var_in_terms([], [], !SVarState, !UrInfo).
replace_any_dot_colon_state_var_in_terms([Arg0 | Args0], [Arg | Args],
        !SVarState, !UrInfo) :-
    replace_any_dot_colon_state_var_in_term(Arg0, Arg, !SVarState, !UrInfo),
    replace_any_dot_colon_state_var_in_terms(Args0, Args, !SVarState, !UrInfo).

replace_any_dot_colon_state_var_in_term(Arg0, Arg, !SVarState, !UrInfo) :-
    ( if Arg0 = functor(atom("!."), [variable(StateVar, _)], Context) then
        lookup_dot_state_var(Context, StateVar, Var, !SVarState, !UrInfo),
        Arg = variable(Var, Context)
    else if Arg0 = functor(atom("!:"), [variable(StateVar, _)], Context) then
        lookup_colon_state_var(Context, StateVar, Var, !SVarState, !UrInfo),
        Arg = variable(Var, Context)
    else
        Arg = Arg0
    ).

lookup_dot_state_var(Context, StateVar, Var, !SVarState, !UrInfo) :-
    StatusMap0 = !.SVarState ^ state_status_map,
    ( if map.search(StatusMap0, StateVar, Status) then
        (
            Status = status_unknown,
            report_uninitialized_state_var(warn_dodgy_simple_code, Context,
                StateVar, !UrInfo),
            % We make StateVar known to avoid duplicate reports.
            new_state_var_instance(StateVar, name_middle, Var, !UrInfo),
            map.det_update(StateVar, status_known(Var),
                StatusMap0, StatusMap),
            !SVarState ^ state_status_map := StatusMap
        ;
            Status = status_unknown_updated(NewVar),
            report_uninitialized_state_var(warn_dodgy_simple_code, Context,
                StateVar, !UrInfo),
            % We make StateVar known to avoid duplicate reports.
            new_state_var_instance(StateVar, name_middle, Var, !UrInfo),
            map.det_update(StateVar, status_known_updated(Var, NewVar),
                StatusMap0, StatusMap),
            !SVarState ^ state_status_map := StatusMap
        ;
            ( Status = status_known(Var)
            ; Status = status_known_ro(Var, _, _)
            ; Status = status_known_updated(Var, _)
            )
        )
    else
        report_non_visible_state_var(".", Context, StateVar, !UrInfo),
        Var = StateVar
    ).

lookup_colon_state_var(Context, StateVar, Var, !SVarState, !UrInfo) :-
    StatusMap0 = !.SVarState ^ state_status_map,
    ( if map.search(StatusMap0, StateVar, Status) then
        (
            Status = status_unknown,
            new_state_var_instance(StateVar, name_middle, Var, !UrInfo),
            map.det_update(StateVar, status_unknown_updated(Var),
                StatusMap0, StatusMap),
            !SVarState ^ state_status_map := StatusMap
        ;
            Status = status_known(OldVar),
            new_state_var_instance(StateVar, name_middle, Var, !UrInfo),
            map.det_update(StateVar, status_known_updated(OldVar, Var),
                StatusMap0, StatusMap),
            !SVarState ^ state_status_map := StatusMap
        ;
            Status = status_known_ro(OldVar, RO_Construct, RO_Context),
            (
                RO_Construct = roc_lambda,
                RO_ConstructName = "lambda expression"
            ),
            report_illegal_state_var_update(Context, RO_ConstructName,
                RO_Context, StateVar, !UrInfo),
            % We remove the readonly notation to avoid duplicate reports.
            new_state_var_instance(StateVar, name_middle, Var, !UrInfo),
            map.det_update(StateVar, status_known_updated(OldVar, Var),
                StatusMap0, StatusMap),
            !SVarState ^ state_status_map := StatusMap
        ;
            Status = status_known_updated(_OldVar, Var)
        ;
            Status = status_unknown_updated(Var)
        )
    else
        report_non_visible_state_var(":", Context, StateVar, !UrInfo),
        % We could make StateVar known to avoid duplicate reports.
        % new_state_var_instance(StateVar, name_initial, Var, !UrInfo),
        % map.det_insert(StateVar, status_known_updated(Var, Var),
        %     StatusMap0, StatusMap),
        % !SVarState ^ state_status_map := StatusMap
        Var = StateVar
    ).

    % Look up the prog_var representing the current state of the state_var
    % whose status is given as the second argument.
    %
:- func svar_get_current_progvar(loc_kind, svar_status) = prog_var.

svar_get_current_progvar(LocKind, Status) = ProgVar :-
    (
        LocKind = loc_whole_goal,
        (
            Status = status_known(ProgVar)
        ;
            ( Status = status_known_ro(_, _, _)
            ; Status = status_known_updated(_, _)
            ; Status = status_unknown
            ; Status = status_unknown_updated(_)
            ),
            unexpected($pred, "Status not known")
        )
    ;
        LocKind = loc_inside_atomic_goal,
        (
            Status = status_known(ProgVar)
        ;
            Status = status_known_updated(_, ProgVar)
        ;
            Status = status_unknown_updated(ProgVar)
        ;
            ( Status = status_known_ro(_, _, _)
            ; Status = status_unknown
            ),
            unexpected($pred, "Status not known or updated")
        )
    ).

%---------------------------------------------------------------------------%
%
% Code to handle the flattening of conjunctions. We need to be careful when we
% do so, since the goal we flatten could have a goal id, which would mean that
% the svar_store could have a delayed remapping for that goal_id. Just
% flattening the goal would remove the goal_info containing the goal_id from
% the HLDS, and the delayed renaming would not get done.
%
% Therefore when we flatten such a goal, we ensure that its subgoals
% all have goal_ids (creating new ones if needed), and that the delayed
% renaming that now won't get done on the conjunction as a whole
% *will* get done on each conjunct.
%

svar_flatten_conj(Context, Goals, Goal, !UrInfo) :-
    list.map_foldl(svar_goal_to_conj_list, Goals, GoalConjuncts, !UrInfo),
    list.condense(GoalConjuncts, Conjuncts),
    GoalExpr = conj(plain_conj, Conjuncts),
    goal_info_init(Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

svar_goal_to_conj_list(Goal, Conjuncts, !UrInfo) :-
    % The code here is the same as in svar_goal_to_conj_list_internal,
    % modulo the differences in the argument list.
    Goal = hlds_goal(GoalExpr, GoalInfo),
    ( if GoalExpr = conj(plain_conj, Conjuncts0) then
        SVarStore0 = !.UrInfo ^ ui_state_var_store,
        SVarStore0 = svar_store(NextGoalId0, DelayedRenamingMap0,
            LastIdMap0, SVarSpecs0),
        GoalId = goal_info_get_goal_id(GoalInfo),
        ( if map.search(DelayedRenamingMap0, GoalId, GoalDelayedRenaming) then
            list.map_foldl2(
                add_conjunct_delayed_renames(GoalDelayedRenaming),
                    Conjuncts0, Conjuncts, NextGoalId0, NextGoalId,
                    DelayedRenamingMap0, DelayedRenamingMap),
            SVarStore = svar_store(NextGoalId, DelayedRenamingMap,
                LastIdMap0, SVarSpecs0),
            !UrInfo ^ ui_state_var_store := SVarStore
        else
            Conjuncts = Conjuncts0
        )
    else
        Conjuncts = [Goal]
    ).

:- pred svar_goal_to_conj_list_internal(hlds_goal::in, list(hlds_goal)::out,
    ucounter::in, ucounter::out,
    incremental_rename_map::in, incremental_rename_map::out) is det.

svar_goal_to_conj_list_internal(Goal, Conjuncts,
        !NextGoalId, !DelayedRenamingMap) :-
    % The code here is the same as in svar_goal_to_conj_list,
    % modulo the differences in the argument list.
    Goal = hlds_goal(GoalExpr, GoalInfo),
    ( if GoalExpr = conj(plain_conj, Conjuncts0) then
        GoalId = goal_info_get_goal_id(GoalInfo),
        ( if map.search(!.DelayedRenamingMap, GoalId, GoalDelayedRenaming) then
            list.map_foldl2(
                add_conjunct_delayed_renames(GoalDelayedRenaming),
                Conjuncts0, Conjuncts, !NextGoalId, !DelayedRenamingMap)
        else
            Conjuncts = Conjuncts0
        )
    else
        Conjuncts = [Goal]
    ).

:- pred add_conjunct_delayed_renames(assoc_list(prog_var, prog_var)::in,
    hlds_goal::in, hlds_goal::out, ucounter::in, ucounter::out,
    incremental_rename_map::in, incremental_rename_map::out) is det.

add_conjunct_delayed_renames(DelayedRenamingToAdd, Goal0, Goal,
        !NextGoalId, !DelayedRenamingMap) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    GoalId0 = goal_info_get_goal_id(GoalInfo0),
    ( if map.search(!.DelayedRenamingMap, GoalId0, DelayedRenaming0) then
        % The goal id must be valid.
        DelayedRenaming = DelayedRenamingToAdd ++ DelayedRenaming0,
        map.det_update(GoalId0, DelayedRenaming, !DelayedRenamingMap),
        Goal = Goal0
    else
        % The goal id must be invalid, since the only thing that attaches goal
        % ids to goals at this stage of the compilation process is this module,
        % and it attaches goal_ids to goals only if it also puts them the
        % delayed renaming map.
        counter.uallocate(GoalIdNum, !NextGoalId),
        GoalId = goal_id(GoalIdNum),
        goal_info_set_goal_id(GoalId, GoalInfo0, GoalInfo),
        map.det_insert(GoalId, DelayedRenamingToAdd, !DelayedRenamingMap),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

%---------------------------------------------------------------------------%
%
% A post-pass to delete unneeded copy unifications. Such unifications
% can hide singleton variable problems.
%

    % Suppose we have a goal such as this:
    %
    %   ( if p(..., !.S, !:S) then
    %       <then_part>
    %   else
    %       <else_part>
    %   )
    %
    % and that !:S is not referred to anywhere in the clause
    % (not in then_part, not in else_part, not in the following code).
    %
    % In this form, warn_singletons can generate a warning about !:S
    % being a singleton. However, the state variable transformation
    % transforms it to code like this:
    %
    %   ( if p(..., STATE_VARIABLE_S_5, STATE_VARIABLE_S_6) then
    %       <then_part>, STATE_VARIABLE_S_7 = STATE_VARIABLE_S_6
    %   else
    %       <else_part>, STATE_VARIABLE_S_7 = STATE_VARIABLE_S_5
    %   )
    %
    % and marks both assignments to STATE_VARIABLE_S_7 as not being subject
    % to singleton warnings.
    %
    % We don't actually *want* to generate warnings about the assignments
    % to STATE_VARIABLE_S_7, because the problem is not in those assignments
    % or in the if-then-else arms that contain them. Instead, it is
    % in the condition. However, with this version of the code, the occurrence
    % of STATE_VARIABLE_S_6 in the condition is *not* a singleton.
    %
    % To allow us to generate a warning about !:S in the condition,
    % we delete all copy unifications inserted by the state variable
    % transformation that assign to a variable that is not referred to
    % either in the code that follows the assignment, or in the head.
    % (The head contains the output arguments, which will live
    % beyond the lifetime of anything in the clause body.)
    %
    % To find which variables occur after a copy goal, we have
    % delete_unneeded_copy_goals do a backwards traversal of the clause body,
    % keeping track of all the variables it has seen.
    %
    % To find which variables occur in the head, we use the call to goal_vars
    % below. The variables in HeadUnificationsGoal contain not just the
    % head_vars of the clause, but also the state variable instances any
    % of them are unified with. (This includes the input arguments as well as
    % the output arguments, but the clause body won't contain any assignments
    % to either the input arguments or the state variable instances
    % they are unified with, so including them in SeenLater0 is harmless.
    % As it happens, we have to include them because we don't know
    % which arguments are input and which are output, a distinction
    % that in any case may be mode-dependent.)
    %
    % We cannot count on the definitions of those state var instances being
    % before any of the occurrences of the head vars they are unified with.
    % For example, if a fact contains an !S argument pair, the call to
    % svar_finish_body in our caller will put the unification of the state
    % var instances representing !.S and !:S *after* HeadUnificationsGoal.
    % If we initialized SeenLater0 to just the head_vars of the clause,
    % this unification would assign to a variable that is *not* in SeenLater0,
    % and would thus be eliminated, which would be a bug.
    %
:- pred delete_unneeded_copy_goals_in_clause(hlds_goal::in,
    hlds_goal::in, hlds_goal::out) is det.

delete_unneeded_copy_goals_in_clause(HeadUnificationsGoal, Goal0, Goal) :-
    vars_in_goal(HeadUnificationsGoal, HeadUnificationsGoalVars),
    SeenLater0 = HeadUnificationsGoalVars,
    delete_unneeded_copy_goals(Goal0, Goal, SeenLater0, _SeenLater).

:- pred delete_unneeded_copy_goals(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out) is det.

delete_unneeded_copy_goals(Goal0, Goal, SeenAfter, SeenBefore) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = unify(LHSVar, _, _, _, _),
        vars_in_goal(Goal0, GoalVars0),
        ( if
            goal_info_has_feature(GoalInfo, feature_state_var_copy),
            not set_of_var.member(SeenAfter, LHSVar)
        then
            Goal = hlds_goal(true_goal_expr, GoalInfo),
            SeenBefore = SeenAfter
        else
            set_of_var.union(GoalVars0, SeenAfter, SeenBefore),
            Goal = Goal0
        )
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        vars_in_goal(Goal0, GoalVars0),
        set_of_var.union(GoalVars0, SeenAfter, SeenBefore),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjKind, Conjuncts0),
        % Processing Conjuncts0 without reversing it would lead to recursion
        % as deep as Conjuncts0 is long. Since Conjuncts0 can be very long,
        % we prefer to pay the price of reversing and unreversing the list
        % to achieve tail recursion.
        list.reverse(Conjuncts0, RevConjuncts0),
        delete_unneeded_copy_goals_rev_conj(RevConjuncts0, RevConjuncts,
            SeenAfter, SeenBefore),
        list.reverse(RevConjuncts, Conjuncts),
        GoalExpr = conj(ConjKind, Conjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = disj(Disjuncts0),
        delete_unneeded_copy_goals_disj(Disjuncts0, Disjuncts,
            SeenAfter, SeenBefores),
        GoalExpr = disj(Disjuncts),
        set_of_var.union_list(SeenBefores, SeenBefore),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        % Switches should not exist at this point in the compilation process,
        % but it is simple enough to prepare here for the eventuality that
        % this may change in the future.
        delete_unneeded_copy_goals_switch(Cases0, Cases,
            SeenAfter, SeenBefores),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        set_of_var.union_list(SeenBefores, SeenBefore0),
        set_of_var.insert(SwitchVar, SeenBefore0, SeenBefore),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = if_then_else(ITEVars, Cond0, Then0, Else0),
        delete_unneeded_copy_goals(Else0, Else, SeenAfter, SeenBeforeElse),
        delete_unneeded_copy_goals(Then0, Then, SeenAfter, SeenAfterThen),
        delete_unneeded_copy_goals(Cond0, Cond, SeenAfterThen, SeenBeforeCond),
        GoalExpr = if_then_else(ITEVars, Cond, Then, Else),
        set_of_var.union(SeenBeforeCond, SeenBeforeElse, SeenBefore0),
        set_of_var.insert_list(ITEVars, SeenBefore0, SeenBefore),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = negation(SubGoal0),
        delete_unneeded_copy_goals(SubGoal0, SubGoal, SeenAfter, SeenBefore),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            Reason = from_ground_term(TermVar, _Kind),
            % There won't be any feature_state_var_copy goals inside SubGoal0.
            SubGoal = SubGoal0,
            % None of the variables in SubGoal can occur in the rest of the
            % procedure body, with the exception of TermVar.
            set_of_var.insert(TermVar, SeenAfter, SeenBefore)
        ;
            ( Reason = require_complete_switch(ScopeVar)
            ; Reason = require_switch_arms_detism(ScopeVar, _Detism)
            ),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore0),
            set_of_var.insert(ScopeVar, SeenBefore0, SeenBefore)
        ;
            Reason = loop_control(LCVar, LCSVar, _UseParentStack),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore0),
            set_of_var.insert_list([LCVar, LCSVar], SeenBefore0, SeenBefore)
        ;
            ( Reason = exist_quant(ScopeVars, _)
            ; Reason = promise_solutions(ScopeVars, _PromiseKind)
            ; Reason = trace_goal(_Comp, _Run, _MaybeIO, _Mutables, ScopeVars)
            ),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore0),
            set_of_var.insert_list(ScopeVars, SeenBefore0, SeenBefore)
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore)
        ),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(AtomicType,
                atomic_interface_vars(OuterInitVar, OuterFinalVar),
                atomic_interface_vars(InnerInitVar, InnerFinalVar),
                MaybeOutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            expect(unify(OrElseInners, []), $pred, "OrElseInners != []"),
            Disjuncts0 = [MainGoal0 | OrElseGoals0],
            delete_unneeded_copy_goals_disj(Disjuncts0, Disjuncts,
                SeenAfter, SeenBefores),
            (
                Disjuncts = [],
                unexpected($pred, "Disjuncts = []")
            ;
                Disjuncts = [MainGoal | OrElseGoals]
            ),
            ShortHand = atomic_goal(AtomicType,
                atomic_interface_vars(OuterInitVar, OuterFinalVar),
                atomic_interface_vars(InnerInitVar, InnerFinalVar),
                MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners),
            set_of_var.union_list(SeenBefores, SeenBefore0),
            set_of_var.insert_list([OuterInitVar, OuterFinalVar,
                InnerInitVar, InnerFinalVar], SeenBefore0, SeenBefore1),
            (
                MaybeOutputVars = no,
                SeenBefore = SeenBefore1
            ;
                MaybeOutputVars = yes(OutputVars),
                set_of_var.insert_list(OutputVars, SeenBefore1, SeenBefore)
            )
        ;
            ShortHand0 = try_goal(MaybeIOStateVars, ResultVar, SubGoal0),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore0),
            set_of_var.insert(ResultVar, SeenBefore0, SeenBefore1),
            (
                MaybeIOStateVars = no,
                SeenBefore = SeenBefore1
            ;
                MaybeIOStateVars = yes(try_io_state_vars(InitVar, FinalVar)),
                set_of_var.insert(InitVar, SeenBefore1, SeenBefore2),
                set_of_var.insert(FinalVar, SeenBefore2, SeenBefore)
            ),
            ShortHand = try_goal(MaybeIOStateVars, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(LeftGoal0, RightGoal0),
            delete_unneeded_copy_goals(LeftGoal0, LeftGoal,
                SeenAfter, SeenBeforeLeft),
            delete_unneeded_copy_goals(RightGoal0, RightGoal,
                SeenAfter, SeenBeforeRight),
            set_of_var.union(SeenBeforeLeft, SeenBeforeRight, SeenBefore),
            ShortHand = bi_implication(LeftGoal, RightGoal)
        ),
        GoalExpr = shorthand(ShortHand),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

:- pred delete_unneeded_copy_goals_rev_conj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out) is det.

delete_unneeded_copy_goals_rev_conj([], [], SeenAfter, SeenBefore) :-
    SeenBefore = SeenAfter.
delete_unneeded_copy_goals_rev_conj(
        [RevConjunct0 | RevConjuncts0], [RevConjunct | RevConjuncts],
        SeenAfter, SeenBefore) :-
    delete_unneeded_copy_goals(RevConjunct0, RevConjunct,
        SeenAfter, SeenBetween),
    delete_unneeded_copy_goals_rev_conj(RevConjuncts0, RevConjuncts,
        SeenBetween, SeenBefore).

:- pred delete_unneeded_copy_goals_disj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, list(set_of_progvar)::out) is det.

delete_unneeded_copy_goals_disj([], [], _, []).
delete_unneeded_copy_goals_disj(
        [Disjunct0 | Disjuncts0], [Disjunct | Disjuncts],
        SeenAfter, [SeenBefore | SeenBefores]) :-
    delete_unneeded_copy_goals(Disjunct0, Disjunct, SeenAfter, SeenBefore),
    delete_unneeded_copy_goals_disj(Disjuncts0, Disjuncts,
        SeenAfter, SeenBefores).

:- pred delete_unneeded_copy_goals_switch(list(case)::in, list(case)::out,
    set_of_progvar::in, list(set_of_progvar)::out) is det.

delete_unneeded_copy_goals_switch([], [], _, []).
delete_unneeded_copy_goals_switch([Case0 | Cases0], [Case | Cases],
        SeenAfter, [SeenBefore | SeenBefores]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    delete_unneeded_copy_goals(Goal0, Goal, SeenAfter, SeenBefore),
    Case = case(MainConsId, OtherConsIds, Goal),
    delete_unneeded_copy_goals_switch(Cases0, Cases, SeenAfter, SeenBefores).

%---------------------------------------------------------------------------%
%
% Test for various kinds of errors.
%

illegal_state_var_func_result(pf_function, ArgTerms, StateVar, Context) :-
    list.last(ArgTerms, LastArgTerm),
    is_term_a_bang_state_pair(LastArgTerm, StateVar, Context).

is_term_a_bang_state_pair(ArgTerm, StateVar, Context) :-
    ArgTerm = functor(atom("!"), [variable(StateVar, Context)], _).

%---------------------------------------------------------------------------%
%
% Report various kinds of errors.
%

report_illegal_state_var_update(Context, RO_Construct, RO_Context,
        StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces1 = [words("Error: you cannot use")] ++
        color_as_incorrect([quote("!:" ++ Name)]) ++
        [words("here due to the surrounding"), words(RO_Construct),
            suffix(";"),
        words("you may only refer to")] ++
        color_as_correct([quote("!." ++ Name), suffix(".")]) ++ [nl],
    Msg1 = msg(Context, Pieces1),
    Pieces2 = [words("Here is the surrounding context that makes"),
        words("state variable"), quote(Name), words("readonly."), nl],
    Msg2 = msg(RO_Context, Pieces2),
    Spec = error_spec($pred, severity_error, phase_pt2h, [Msg1, Msg2]),
    add_unravel_spec(Spec, !UrInfo).

:- func ro_construct_name(readonly_context_kind) = string.

ro_construct_name(roc_lambda) = "lambda expression".

%---------------------------------------------------------------------------%

report_illegal_func_svar_result(Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Spec = report_illegal_func_svar_result_raw(Context, VarSet, StateVar),
    add_unravel_spec(Spec, !UrInfo).

report_illegal_func_svar_result_raw(Context, VarSet, StateVar) = Spec :-
    Name = varset.lookup_name(VarSet, StateVar),
    % While having !.Var appear as a function argument is quite ordinary,
    % having it appear as a function *result* is not. We therefore do not
    % suggest it as a likely correction.
    Pieces = [words("Error: since it represents two arguments, not one,")] ++
        color_as_incorrect([quote("!" ++ Name)]) ++
        [words("cannot be a function result. You probably meant")] ++
        color_as_correct([fixed("!:" ++ Name), suffix(".")]) ++ [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces).

%---------------------------------------------------------------------------%

report_illegal_bang_svar_lambda_arg(Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Spec = report_illegal_bang_svar_lambda_arg_raw(Context, VarSet, StateVar),
    add_unravel_spec(Spec, !UrInfo).

report_illegal_bang_svar_lambda_arg_raw(Context, VarSet, StateVar) = Spec :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:")] ++
        color_as_incorrect([quote("!" ++ Name)]) ++
        [words("cannot be a lambda argument."), nl,
        words("Perhaps you meant")] ++
        color_as_correct([quote("!." ++ Name)]) ++
        [words("or")] ++
        color_as_correct([quote("!:" ++ Name), suffix(".")]) ++ [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces).

%---------------------------------------------------------------------------%

:- pred report_non_visible_state_var(string::in, prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

report_non_visible_state_var(DorC, Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error: state variable")] ++
        color_as_incorrect([quote("!" ++ DorC ++ Name)]) ++
        [words("is not visible in this context."), nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    add_unravel_spec(Spec, !UrInfo).

%---------------------------------------------------------------------------%

:- pred report_uninitialized_state_var(option::in, prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

report_uninitialized_state_var(WarnOption, Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: you cannot refer to")] ++
        color_as_subject([quote("!." ++ Name)]) ++
        [words("here, because that state variable has")] ++
        color_as_incorrect([words("not been initialized")]) ++
        [words("yet."), nl],
    Spec = spec($pred, severity_warning(WarnOption), phase_pt2h,
        Context, Pieces),
    add_unravel_spec(Spec, !UrInfo).

%---------------------------------------------------------------------------%

:- pred report_repeated_head_state_var(prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

report_repeated_head_state_var(Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: clause head introduces")] ++
        color_as_incorrect([words("state variable"), quote(Name)]) ++
        [words("more than once."), nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    add_unravel_spec(Spec, !UrInfo).

%---------------------------------------------------------------------------%

:- pred report_state_var_shadow(prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

report_state_var_shadow(Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: new state variable")] ++
        color_as_subject([quote(Name)]) ++
        color_as_incorrect([words("shadows old one.")]) ++ [nl],
    Spec = spec($pred, severity_warning(warn_state_var_shadowing), phase_pt2h,
        Context, Pieces),
    add_unravel_spec(Spec, !UrInfo).

%---------------------------------------------------------------------------%

:- pred report_missing_inits_in_ite(prog_context::in, list(string)::in,
    string::in, string::in, unravel_info::in, unravel_info::out) is det.

report_missing_inits_in_ite(Context, NextStateVars,
        WhenMissing, WhenNotMissing, !UrInfo) :-
    NextStateVarsPieces = quote_list_to_color_pieces(color_subject, "and",
        [suffix(",")], NextStateVars),
    Pieces = [words("When the condition"), words(WhenNotMissing), suffix(","),
        words("the if-then-else")] ++
        color_as_inconsistent([words("defines")]) ++
        NextStateVarsPieces ++
        [words("but when the condition"), words(WhenMissing), suffix(",")] ++
        color_as_inconsistent([words("it does not.")]) ++ [nl],
    Spec = spec($pred, severity_warning(warn_missing_state_var_init),
        phase_pt2h, Context, Pieces),
    Specs0 = !.UrInfo ^ ui_state_var_store ^ store_missing_init_specs,
    Specs = [Spec | Specs0],
    !UrInfo ^ ui_state_var_store ^ store_missing_init_specs := Specs.

:- pred report_missing_inits_in_disjunct(prog_context::in, list(string)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_missing_inits_in_disjunct(Context, NextStateVars, !Specs) :-
    Pieces = [words("Other disjuncts define")] ++
        quote_list_to_color_pieces(color_subject, "and", [suffix(",")],
            NextStateVars) ++
        color_as_incorrect([words("but not this one.")]) ++ [nl],
    Spec = spec($pred, severity_warning(warn_missing_state_var_init),
        phase_pt2h, Context, Pieces),
    % The intention is that our caller got !.Specs from the state var store's
    % store_missing_init_specs field, and will put the updated list back there.
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

report_svar_unify_error(Context, StateVar, !SVarState, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:")] ++
        color_as_incorrect([fixed("!" ++ Name)]) ++
        [words("cannot appear as a unification argument."), nl,
        words("You probably meant")] ++
        color_as_correct([fixed("!." ++ Name)]) ++ [words("or")] ++
        color_as_correct([fixed("!:" ++ Name), suffix(".")]) ++ [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    add_unravel_spec(Spec, !UrInfo),
    !.SVarState = svar_state(StatusMap0),
    % If StateVar was not known before, then this is the first occurrence
    % of this state variable, and the user almost certainly intended it
    % to define its initial value. Any messages from later goals complaining
    % about the variable not being defined there would only be a distraction.
    %
    % Adding this dummy entry to the state, means we cannot generate valid
    % HLDS goals, but the error reported just above ensures that we will
    % throw away the HLDS goals we generate, so this is ok.
    ( if
        map.search(StatusMap0, StateVar, OldStatus),
        OldStatus \= status_unknown
    then
        % The state variable is already known.
        true
    else
        new_state_var_instance(StateVar, name_initial, Var, !UrInfo),
        Status = status_known(Var),
        map.set(StateVar, Status, StatusMap0, StatusMap),
        !:SVarState = svar_state(StatusMap)
    ).

%---------------------------------------------------------------------------%

:- pred find_unused_statevar_args(prog_varset::in, new_statevar_map::in,
    last_id_map::in, unused_statevar_arg_map::out) is det.

find_unused_statevar_args(VarSet, NewSVars, LastIdMap, UnusedSVarArgMap) :-
    map.foldl(record_statevar_if_unused(VarSet, LastIdMap), NewSVars,
        map.init, UnusedSVarArgMap).

:- pred record_statevar_if_unused(prog_varset::in, last_id_map::in,
    prog_var::in, one_or_more(maybe_statevar_arg_pos)::in,
    unused_statevar_arg_map::in, unused_statevar_arg_map::out) is det.

record_statevar_if_unused(VarSet, LastIdMap, SVar, OoMArgPos,
        !UnusedSVarArgMap) :-
    list.sort(one_or_more_to_list(OoMArgPos), SortedArgPoss),
    ( if
        not map.search(LastIdMap, SVar, _),
        (
            SortedArgPoss = [arg_old(InitPos), arg_new(FinalPos)],
            ArgPos = InitPos,
            ArgKind = init_and_final_arg(FinalPos)
        ;
            SortedArgPoss = [arg_old(InitPos)],
            ArgPos = InitPos,
            ArgKind = init_arg_only
        ;
            SortedArgPoss = [arg_new(FinalPos)],
            ArgPos = FinalPos,
            ArgKind = final_arg_only
        )
    then
        varset.lookup_name(VarSet, SVar, SVarName),
        SVarArgDesc = statevar_arg_desc(ArgKind, SVarName),
        map.det_insert(ArgPos, SVarArgDesc, !UnusedSVarArgMap)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred report_any_unneeded_svars_in_lambda(prog_context::in,
    list(mer_mode)::in, goal::in, hlds_goal::in, unused_statevar_arg_map::in,
    unravel_info::in, unravel_info::out) is det.

report_any_unneeded_svars_in_lambda(Context, Modes, ParseTreeGoal, Goal,
        UnusedSVarArgMap, !UrInfo) :-
    ( if map.is_empty(UnusedSVarArgMap) then
        true
    else
        VarSet = !.UrInfo ^ ui_varset,
        non_svar_copy_vars_in_goal(Goal, GoalVarsSet),
        set_of_var.to_sorted_list(GoalVarsSet, GoalVars),
        list.filter_map(is_prog_var_for_some_state_var(VarSet),
            GoalVars, GoalVarSVarNames),
        map.foldl(
            report_unneeded_svar_in_lambda(Context, Modes,
                ParseTreeGoal, GoalVarSVarNames),
            UnusedSVarArgMap, !UrInfo)
    ).

:- pred report_unneeded_svar_in_lambda(prog_context::in, list(mer_mode)::in,
    goal::in, list(string)::in, uint::in, statevar_arg_desc::in,
    unravel_info::in, unravel_info::out) is det.

report_unneeded_svar_in_lambda(Context, Modes, ParseTreeGoal, GoalVarSVarNames,
        ArgNum, SVarArgDesc, !UrInfo) :-
    SVarArgDesc = statevar_arg_desc(InitOrFinal, SVarName),
    % Please keep the wording of the three warnings generated here
    % in sync with the code of the following predicates in pre_typecheck.m:
    % - warn_about_any_unneeded_initial_statevars
    % - warn_about_unneeded_final_statevar
    % - warn_about_unneeded_initial_final_statevar.
    (
        ( InitOrFinal = init_arg_only,  Prefix = "!."
        ; InitOrFinal = final_arg_only, Prefix = "!:"
        ),
        Pieces = [words("Warning: the state variable")] ++
            color_as_subject([quote(Prefix ++ SVarName)]) ++ [words("is")] ++
            color_as_incorrect([words("never updated")]) ++
            [words("in this lambda expressions, so it should be"),
            words("replaced with an ordinary variable."), nl],
        Severity = severity_warning(warn_unneeded_initial_statevars_lambda),
        Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
        add_unravel_spec(Spec, !UrInfo)
    ;
        InitOrFinal = init_and_final_arg(_),
        % Please keep this wording in sync with the code of the
        % warn_about_unneeded_final_statevar predicate in pre_typecheck.m.
        InitOrFinal = init_and_final_arg(FinalArgNum),
        ( if list.member(SVarName, GoalVarSVarNames) then
            % The initial version of SVarName is used by user-written code
            % in the lambda goal, so only the final version of SVarName
            % is unneeded.
            ModuleInfo = !.UrInfo ^ ui_module_info,
            FinalArgNumI = uint.cast_to_int(FinalArgNum),
            InitArgNumI = uint.cast_to_int(ArgNum),
            list.det_index1(Modes, InitArgNumI, InitArgMode),
            list.det_index1(Modes, FinalArgNumI, FinalArgMode),
            ( if
                % See the comments in warn_about_any_unneeded_statevars
                % for the reasoning behind this test.
                %
                % Note that we cannot test the HLDS goal from which our caller
                % derived GoalVarSVarNames, because that contains the
                % unifications implicitly added by the state variable
                % transformation itself. We need the goal from *before*
                % that transformation.
                not ( ParseTreeGoal = true_expr(_) ),
                % See the comments in maybe_warn_about_unneeded_final_statevar
                % for the reasoning behind this test.
                mode_is_free_of_uniqueness(ModuleInfo, InitArgMode),
                mode_is_free_of_uniqueness(ModuleInfo, FinalArgMode)
            then
                Pieces = [words("Warning: the argument")] ++
                    color_as_subject([quote("!:" ++ SVarName)]) ++
                    [words("in this lambda expression")] ++
                    color_as_incorrect([words("could be deleted,")]) ++
                    [words("because its value"),
                    words("is always the same as its initial value."), nl],
                Severity =
                    severity_warning(warn_unneeded_final_statevars_lambda),
                Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
                add_unravel_spec(Spec, !UrInfo)
            else
                true
            )
        else
            % The initial version of SVarName is NOT used by user-written code
            % in the lambda goal, so both the initial and final versions
            % of SVarName are unneeded.
            Pieces = [words("Warning: the arguments")] ++
                color_as_subject([quote("!." ++ SVarName)]) ++
                [words("and")] ++
                color_as_subject([quote("!:" ++ SVarName)]) ++
                [words("in this lambda expression")] ++
                color_as_incorrect([words("could be deleted,")]) ++
                [words("because they are not used in the lambda goal,"),
                words("and because the final value"),
                words("is always the same as the initial value."), nl],
            Severity = severity_warning(warn_unneeded_final_statevars_lambda),
            Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
            add_unravel_spec(Spec, !UrInfo)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.state_var.
%---------------------------------------------------------------------------%
