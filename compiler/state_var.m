%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2011,2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: state_var.m.
% Main author of original version: rafe.
% Main author of the current version, rewritten in 2011: zs.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.state_var.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.make_hlds.goal_expr_to_goal.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

    % This synonym improves code legibility. The intention is that we use
    % svar instead of prog_var in pred type declarations for any variables X
    % that represent state variables !X.
    %
:- type svar == prog_var.

    % When collecting the arms of a disjunction, we also need to collect
    % the resulting svar_states.
    %
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

%-----------------------------------------------------------------------------%

    % Replace !X args with two args !.X, !:X in that order.
    %
:- pred expand_bang_state_pairs_in_terms(list(prog_term)::in,
    list(prog_term)::out) is det.
:- pred expand_bang_state_pairs_in_instance_method(instance_method::in,
    instance_method::out) is det.

%-----------------------------------------------------------------------------%

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
:- pred svar_prepare_for_clause_head(list(prog_term)::in, list(prog_term)::out,
    prog_varset::in, prog_varset::out, map(svar, prog_var)::out,
    svar_state::out, svar_store::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Prepare for processing a lambda expression by processing its head.
    %
    % In most ways, this is very similar to processing the head of a clause,
    % but we also need to handle state variables which are visible in the scope
    % that encloses the lambda expression. We make those state vars read-only
    % within the lambda expression.
    %
:- pred svar_prepare_for_lambda_head(prog_context::in,
    list(prog_term)::in, list(prog_term)::out,
    map(svar, prog_var)::out, svar_state::in, svar_state::out,
    prog_varset::in, prog_varset::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Finish processing a clause. Make the final values of the clause's state
    % vars match the mapping we decided on when processing the head.
    %
:- pred svar_finish_clause_body(globals::in, module_name::in, prog_context::in,
    map(svar, prog_var)::in, hlds_goal::in, hlds_goal::in, hlds_goal::out,
    svar_state::in, svar_state::in, svar_store::in,
    list(error_spec)::out, list(error_spec)::out) is det.

    % Finish processing a lambda expression.
    %
:- pred svar_finish_lambda_body(globals::in, module_name::in, prog_context::in,
    map(svar, prog_var)::in, list(hlds_goal)::in, hlds_goal::out,
    svar_state::in, svar_state::in, svar_store::in, svar_store::out) is det.

%-----------------------------------------------------------------------------%

    % Finish the execution of an atomic goal. If this goal was not inside
    % another atomic goal, then make any updates to state variables performed
    % by the atomic goal take effect: make the value assigned to !:S inside
    % the goal the new !.S.
    %
:- pred svar_finish_atomic_goal(loc_kind::in, svar_state::in, svar_state::out)
    is det.

%-----------------------------------------------------------------------------%

    % Add some local state variables.
    %
:- pred svar_prepare_for_local_state_vars(prog_context::in, prog_varset::in,
    list(svar)::in, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Remove some local state variables.
    %
:- pred svar_finish_local_state_vars(globals::in, module_name::in,
    list(svar)::in, svar_state::in, svar_state::in, svar_state::out) is det.

%-----------------------------------------------------------------------------%

    % Make sure that all arms of a disjunction produce the same state variable
    % bindings, by adding unifiers as necessary.
    %
:- pred svar_finish_disjunction(prog_context::in,
    list(hlds_goal_svar_state)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    svar_store::in, svar_store::out) is det.

%-----------------------------------------------------------------------------%

    % Add unifiers to the Then and Else arms of an if-then-else to make sure
    % that all the state variables match up.
    %
    % We also add unifiers to the Then arm for any new state variable
    % mappings produced in the condition.
    %
:- pred svar_finish_if_then_else(globals::in, module_name::in,
    loc_kind::in, prog_context::in, list(svar)::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    svar_state::in, svar_state::in, svar_state::in, svar_state::in,
    svar_state::out, prog_varset::in, prog_varset::out,
    svar_store::in, svar_store::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

:- type svar_outer_atomic_scope_info.
:- type svar_inner_atomic_scope_info.

    % svar_start_outer_atomic_scope(Context, OuterStateVar, OuterDI, OuterUO,
    %   OuterScopeInfo, !State, !VarSet, !Specs):
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
    svar_state::in, svar_state::out, prog_varset::in, prog_varset::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % svar_finish_outer_atomic_scope(OuterScopeInfo, !SInfo):
    %
    % Restore the accessibility of !OuterStateVar that was disabled by
    % svar_start_atomic_scope.
    %
:- pred svar_finish_outer_atomic_scope(svar_outer_atomic_scope_info::in,
    svar_state::in, svar_state::out) is det.

    % svar_start_inner_atomic_scope(Context, InnerStateVar, InnerScopeInfo,
    %   !State, !VarSet, !Specs):
    %
    % This predicate prepares for an atomic scope with an !InnerStateVar
    % specification by making that state var available.
    %
:- pred svar_start_inner_atomic_scope(prog_context::in, prog_var::in,
    svar_inner_atomic_scope_info::out,
    svar_state::in, svar_state::out, prog_varset::in, prog_varset::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % svar_finish_inner_atomic_scope(Context, InnerScopeInfo, InnerDI, InnerUO,
    %   !State, !VarSet, !Specs):
    %
    % This predicate ends an atomic scope with an !InnerStateVar
    % specification by making that state var unavailable, and returning
    % the two variables InnerDI and InnerUO representing the initial and final
    % states of this state variable.
    %
:- pred svar_finish_inner_atomic_scope(prog_context::in,
    svar_inner_atomic_scope_info::in, prog_var::out, prog_var::out,
    svar_state::in, svar_state::out, prog_varset::in, prog_varset::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

    % Given a list of argument terms, substitute !.X and !:X with the
    % corresponding state variable mappings. Any !X should already have been
    % expanded into !.X, !:X via a call to expand_bang_state_pairs.
    %
:- pred substitute_state_var_mappings(
    list(prog_term)::in, list(prog_term)::out,
    prog_varset::in, prog_varset::out,
    svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Same as substitute_state_var_mappings, but for only one term.
    %
:- pred substitute_state_var_mapping(prog_term::in, prog_term::out,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Look up the prog_var that represents the current state of the given
    % state variable.
    %
:- pred lookup_dot_state_var(prog_context::in, svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Look up the prog_var that represents the next state of the given
    % state variable.
    %
:- pred lookup_colon_state_var(prog_context::in, svar::in, prog_var::out,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

    % Flatten a conjunction while preserving the invariants that the state
    % variable transformation cares about.
    %
:- pred svar_flatten_conj(prog_context::in,
    list(hlds_goal)::in, hlds_goal::out,
    svar_store::in, svar_store::out) is det.

    % Flatten a goal into a conjunction while preserving the invariants that
    % the state variable transformation cares about.
    %
:- pred svar_goal_to_conj_list(hlds_goal::in, list(hlds_goal)::out,
    svar_store::in, svar_store::out) is det.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

:- pred report_illegal_state_var_update(prog_context::in,
    string::in, prog_context::in, prog_varset::in,
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

:- pred report_illegal_func_svar_result(prog_context::in, prog_varset::in,
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

:- pred report_illegal_bang_svar_lambda_arg(prog_context::in, prog_varset::in,
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

:- pred report_svar_unify_error(prog_context::in, svar::in,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.make_goal.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module io.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% Define the main data structures used by the implementation of state vars.
%

    % State vars defined outside a lambda goal become readonly when we move
    % inside the lambda goal. Inside the lambda goal, it makes sense to access
    % the current value of such state vars, but not to update it.
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

:- type svar_store
    --->    svar_store(
                store_next_goal_id  ::  counter,
                store_final_remap   ::  incremental_rename_map,
                store_specs         ::  list(error_spec)
            ).

    % Create a new svar_state/store set up to start processing a clause head.
    %
:- func new_svar_state = svar_state.
:- func new_svar_store = svar_store.

new_svar_state = svar_state(map.init).
new_svar_store = svar_store(counter.init(1), map.init, []).

:- type state_var_name_source
    --->    name_initial
    ;       name_middle
    ;       name_final.

:- pred new_state_var_instance(svar::in, state_var_name_source::in,
    prog_var::out, prog_varset::in, prog_varset::out) is det.

new_state_var_instance(StateVar, NameSource, Var, !VarSet) :-
    SVarName = varset.lookup_name(!.VarSet, StateVar),
    (
        NameSource = name_initial,
        ProgVarName = string.format("STATE_VARIABLE_%s_0", [s(SVarName)]),
        varset.new_named_var(ProgVarName, Var, !VarSet)
    ;
        NameSource = name_middle,
        ProgVarBaseName = string.format("STATE_VARIABLE_%s", [s(SVarName)]),
        varset.new_uniquely_named_var(ProgVarBaseName, Var, !VarSet)
    ;
        NameSource = name_final,
        ProgVarName = string.format("STATE_VARIABLE_%s", [s(SVarName)]),
        varset.new_named_var(ProgVarName, Var, !VarSet)
    ).

%-----------------------------------------------------------------------------%
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
        HeadArg0 = functor(Const, FunctorArgs, Ctxt),
        ( if
            Const = atom("!"),
            FunctorArgs = [variable(_StateVar, _)]
        then
            HeadArg1 = functor(atom("!."), FunctorArgs, Ctxt),
            HeadArg2 = functor(atom("!:"), FunctorArgs, Ctxt),
            Args = [HeadArg1, HeadArg2 | TailArgs]
        else
            Args = [HeadArg0 | TailArgs]
        )
    ).

expand_bang_state_pairs_in_instance_method(IM0, IM) :-
    IM0 = instance_method(PredOrFunc, MethodSymName, UserArity0,
        ProcDef0, Ctxt),
    (
        ProcDef0 = instance_proc_def_name(_),
        IM = IM0
    ;
        ProcDef0 = instance_proc_def_clauses(ItemClauses0),
        list.map(expand_bang_state_pairs_in_clause, ItemClauses0, ItemClauses),
        % Note that ItemClauses should never be empty...
        (
            ItemClauses = [ItemClause | _],
            Args = ItemClause ^ cl_head_args,
            PredFormArity = arg_list_arity(Args),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity)
        ;
            ItemClauses = [],
            UserArity = UserArity0
        ),
        ProcDef = instance_proc_def_clauses(ItemClauses),
        IM = instance_method(PredOrFunc, MethodSymName, UserArity,
            ProcDef, Ctxt)
    ).

:- pred expand_bang_state_pairs_in_clause(item_clause_info::in,
    item_clause_info::out) is det.

expand_bang_state_pairs_in_clause(ItemClause0, ItemClause) :-
    ItemClause0 = item_clause_info(PredOrFunc, SymName, Args0, VarSet,
        MaybeBody, Context, SeqNum),
    expand_bang_state_pairs_in_terms(Args0, Args),
    ItemClause = item_clause_info(PredOrFunc, SymName, Args, VarSet,
        MaybeBody, Context, SeqNum).

%-----------------------------------------------------------------------------%
%
% Handle the start of processing a clause.
%

svar_prepare_for_clause_head(Args0, Args, !VarSet, FinalMap,
        !:State, !:Store, !Specs) :-
    !:State = new_svar_state,
    !:Store = new_svar_store,
    svar_prepare_head_terms(Args0, Args, map.init, FinalMap,
        !State, !VarSet, !Specs).

:- pred svar_prepare_head_terms(list(prog_term)::in, list(prog_term)::out,
    map(svar, prog_var)::in, map(svar, prog_var)::out,
    svar_state::in, svar_state::out, prog_varset::in, prog_varset::out,
    list(error_spec)::in, list(error_spec)::out) is det.

svar_prepare_head_terms([], [], !FinalMap, !State, !VarSet, !Specs).
svar_prepare_head_terms([Term0 | Terms0], [Term | Terms],
        !FinalMap, !State, !VarSet, !Specs) :-
    svar_prepare_head_term(Term0, Term, !FinalMap, !State, !VarSet, !Specs),
    svar_prepare_head_terms(Terms0, Terms, !FinalMap, !State, !VarSet, !Specs).

:- pred svar_prepare_head_term(prog_term::in, prog_term::out,
    map(svar, prog_var)::in, map(svar, prog_var)::out,
    svar_state::in, svar_state::out,
    prog_varset::in, prog_varset::out,
    list(error_spec)::in, list(error_spec)::out) is det.

svar_prepare_head_term(Term0, Term, !FinalMap, !State, !VarSet, !Specs) :-
    (
        Term0 = variable(_, _),
        Term = Term0
    ;
        Term0 = functor(Functor, SubTerms0, Context),
        ( if
            Functor = atom("!."),
            SubTerms0 = [variable(StateVar, _)]
        then
            !.State = svar_state(StatusMap0),
            ( if map.search(StatusMap0, StateVar, OldStatus) then
                (
                    OldStatus = status_unknown,
                    % !:S happened to precede !.S in the head, which is ok.
                    new_state_var_instance(StateVar, name_initial, Var,
                        !VarSet),
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
                        !VarSet),
                    Term = variable(Var, Context),
                    Status = status_known(Var),
                    map.det_update(StateVar, Status, StatusMap0, StatusMap)
                )
            else
                new_state_var_instance(StateVar, name_initial, Var, !VarSet),
                Term = variable(Var, Context),
                Status = status_known(Var),
                map.det_insert(StateVar, Status, StatusMap0, StatusMap)
            ),
            !:State = svar_state(StatusMap)
        else if
            Functor = atom("!:"),
            SubTerms0 = [variable(StateVar, _)]
        then
            new_state_var_instance(StateVar, name_final, Var, !VarSet),
            Term = variable(Var, Context),
            Status = status_unknown,

            !.State = svar_state(StatusMap0),
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
            !:State = svar_state(StatusMap),
            map.search_insert(StateVar, Var, MaybeOldVar, !FinalMap),
            (
                MaybeOldVar = yes(_),
                report_repeated_head_state_var(Context, !.VarSet, StateVar,
                    !Specs)
            ;
                MaybeOldVar = no
            )
        else
            svar_prepare_head_terms(SubTerms0, SubTerms,
                !FinalMap, !State, !VarSet, !Specs),
            Term = functor(Functor, SubTerms, Context)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Handle the start of processing a lambda expression.
%

svar_prepare_for_lambda_head(Context, Args0, Args, FinalMap,
        OutsideState, InsideState, !VarSet, !Specs) :-
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
    svar_prepare_head_terms(Args0, Args, map.init, FinalMap,
        InsideState0, InsideState, !VarSet, !Specs).

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

%-----------------------------------------------------------------------------%
%
% Handle the end of processing a clause or lambda expression.
%

svar_finish_clause_body(Globals, ModuleName, Context, FinalMap,
        HeadGoal0, BodyGoal0, Goal, InitialSVarState, FinalSVarState,
        !.SVarStore, WarningSpecs, ErrorSpecs) :-
    svar_finish_body(Globals, ModuleName, Context, FinalMap,
        [HeadGoal0, BodyGoal0], Goal1, InitialSVarState, FinalSVarState,
        !SVarStore),
    !.SVarStore = svar_store(_, DelayedRenamings, Specs),
    list.filter(severity_is_error(Globals), Specs, ErrorSpecs, WarningSpecs),
    ( if
        map.is_empty(FinalMap),
        map.is_empty(DelayedRenamings)
    then
        Goal = Goal1
    else
        trace [compiletime(flag("state-var-lambda")), io(!IO)] (
            get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
            map.to_assoc_list(FinalMap, FinalList),
            map.to_assoc_list(DelayedRenamings, DelayedList),
            io.write_string(DebugStream,
                "\nFINISH CLAUSE BODY in context ", !IO),
            io.write_line(DebugStream, Context, !IO),
            io.write_string(DebugStream, "applying subn\n", !IO),
            io.write_line(DebugStream, FinalList, !IO),
            io.write_string(DebugStream, "with incremental subn\n", !IO),
            io.write_line(DebugStream, DelayedList, !IO)
        ),
        incremental_rename_vars_in_goal(map.init, DelayedRenamings,
            Goal1, Goal2),

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
        % and marks both assignments to STATE_VARIABLE_S_7 as not being
        % subject to singleton warnings.
        %
        % We don't actually *want* to generate warnings about the assignments
        % to STATE_VARIABLE_S_7, because the problem not in those assignments
        % or in the if-then-else arms that contain them. Instead, it is
        % in the condition, However, with this version of the code,
        % the occurrence of STATE_VARIABLE_S_6 in the condition is *not*
        % a singleton.
        %
        % To allow us to generate a warning about !:S in the condition,
        % we delete all copy unifications inserted by the state variable
        % transformation that assign to a variable that is not referred to
        % either in the code that follows the assignment, or in the head.
        % (The head contains the output arguments, which will live
        % beyond the lifetime of anything in the clause body.)
        %
        % To find which variables occur after a copy goal, we have
        % delete_unneeded_copy_goals do a backwards traversal of the clause
        % body, keeping track of all the variables it has seen.
        %
        % To find which variables occur in the head, we use the call to
        % goal_vars below. The variables in HeadGoal0 contain not just
        % the head_vars of the clause, but also the state variable instances
        % any of them are unified with. (This includes the input arguments
        % as well as the output arguments, but the clause body won't contain
        % any assignments to either the input arguments or the state variable
        % instances they are unified with, so including them in SeenLater0
        % is harmless. As it happens, we have to include them because we
        % don't know which arguments are input and which are output,
        % a distinction that in any case may be mode-dependent.)
        %
        % We cannot count on the definitions of those state var instances
        % being before any of the occurrences of the head vars they are
        % unified with. For example, if a fact contains an !S argument pair,
        % the call to svar_finish_body above will put the unification of
        % the state var instances representing !.S and !:S *after*
        % HeadGoal0. If we initialized SeenLater0 to just the head_vars
        % of the clause, this unification would assign to a variable
        % that is *not* in SeenLater0, and would thus be eliminated,
        % which would be a bug.
        goal_vars(HeadGoal0, HeadGoal0Vars),
        SeenLater0 = HeadGoal0Vars,
        delete_unneeded_copy_goals(Goal2, Goal, SeenLater0, _SeenLater)
    ).

svar_finish_lambda_body(Globals, ModuleName, Context, FinalMap, Goals0, Goal,
        InitialSVarState, FinalSVarState, !SVarStore) :-
    svar_finish_body(Globals, ModuleName, Context, FinalMap, Goals0, Goal,
        InitialSVarState, FinalSVarState, !SVarStore).

:- pred svar_finish_body(globals::in, module_name::in, prog_context::in,
    map(svar, prog_var)::in, list(hlds_goal)::in, hlds_goal::out,
    svar_state::in, svar_state::in, svar_store::in, svar_store::out) is det.

svar_finish_body(Globals, ModuleName, Context, FinalMap, Goals0, Goal,
        InitialSVarState, FinalSVarState, !Store) :-
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
    svar_flatten_conj(Context, Goals1, Goal1, !Store),

    Goal1 = hlds_goal(GoalExpr1, GoalInfo1),
    GoalId1 = goal_info_get_goal_id(GoalInfo1),
    !.Store = svar_store(NextGoalId1, DelayedRenamingMap1, Specs),
    ( if map.search(DelayedRenamingMap1, GoalId1, DelayedRenaming0) then
        trace [compiletime(flag("state-var-lambda")), io(!IO)] (
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
            counter.allocate(GoalIdNum, NextGoalId1, NextGoalId),
            GoalId = goal_id(GoalIdNum),

            trace [compiletime(flag("state-var-lambda")), io(!IO)] (
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
    !:Store = svar_store(NextGoalId, DelayedRenamingMap, Specs).

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

%-----------------------------------------------------------------------------%
%
% Handle the completion of an atomic goal. Any variable that was updated in the
% goal gets the updated value as its new current value. The Loc argument is
% needed because sometimes what looks like an atomic goal (such as the
% condition of an if-then-else) is inside another atomic goal (such as an
% if-then-else expression). In such cases, the end of the inside atomic goal
% does NOT mean that we finished the containing atomic goal.
%

svar_finish_atomic_goal(Loc, !State) :-
    (
        Loc = loc_whole_goal,
        !.State = svar_state(StatusMap0),
        map.map_values_only(reset_updated_status, StatusMap0, StatusMap),
        !:State = svar_state(StatusMap)
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

%-----------------------------------------------------------------------------%
%
% Handle scopes that introduce state variables.
%

svar_prepare_for_local_state_vars(Context, VarSet, StateVars,
        OutsideState, InsideState, !Specs) :-
    OutsideState = svar_state(StatusMapOutside),
    prepare_svars_for_scope(Context, VarSet, StateVars,
        StatusMapOutside, StatusMapInside, !Specs),
    InsideState = svar_state(StatusMapInside).

:- pred prepare_svars_for_scope(prog_context::in, prog_varset::in,
    list(svar)::in, map(svar, svar_status)::in, map(svar, svar_status)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

prepare_svars_for_scope(_Context, _VarSet, [], !StatusMap, !Specs).
prepare_svars_for_scope(Context, VarSet, [SVar | SVars],
        !StatusMap, !Specs) :-
    ( if map.search(!.StatusMap, SVar, _OldStatus) then
        report_state_var_shadow(Context, VarSet, SVar, !Specs),
        map.det_update(SVar, status_unknown, !StatusMap)
    else
        map.det_insert(SVar, status_unknown, !StatusMap)
    ),
    prepare_svars_for_scope(Context, VarSet, SVars, !StatusMap, !Specs).

svar_finish_local_state_vars(Globals, ModuleName, StateVars,
        StateBeforeOutside, StateAfterInside, StateAfterOutside) :-
    StateBeforeOutside = svar_state(StatusMapBeforeOutside),
    StateAfterInside = svar_state(StatusMapAfterInside),
    trace [compiletime(flag("state-var-scope")), io(!IO)] (
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

%-----------------------------------------------------------------------------%
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

svar_finish_disjunction(_Context, DisjStates, Disjs, !VarSet,
        StateBefore, StateAfter, !Store) :-
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

        !.Store = svar_store(NextGoalId0, DelayedRenamings0, Specs0),
        merge_changes_made_by_arms(DisjStates, StatusMapBefore,
            ChangedStatusListAfter, !.VarSet, [], RevDisjs,
            NextGoalId0, NextGoalId, DelayedRenamings0, DelayedRenamings,
            Specs0, Specs),
        list.reverse(RevDisjs, Disjs),
        !:Store = svar_store(NextGoalId, DelayedRenamings, Specs)
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
:- pred merge_changes_made_by_arms(list(hlds_goal_svar_state)::in,
    map(svar, svar_status)::in, assoc_list(svar, svar_status)::in,
    prog_varset::in, list(hlds_goal)::in, list(hlds_goal)::out,
    counter::in, counter::out,
    incremental_rename_map::in, incremental_rename_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

merge_changes_made_by_arms([], _StatusMapBefore, _ChangedStatusListAfter,
        _VarSet, !RevArms, !NextGoalId, !DelayedRenamings, !Specs).
merge_changes_made_by_arms([ArmState | ArmStates],
        StatusMapBefore, ChangedStatusListAfter, VarSet, !RevArms,
        !NextGoalId, !DelayedRenamings, !Specs) :-
    ArmState = hlds_goal_svar_state(Arm0, StateAfterArm),
    StatusMapAfterArm = StateAfterArm ^ state_status_map,
    counter.allocate(ArmIdNum, !NextGoalId),
    ArmId = goal_id(ArmIdNum),
    handle_arm_updated_state_vars(ChangedStatusListAfter, StatusMapBefore,
        StatusMapAfterArm, VarSet, UninitVarNames, CopyGoals, ArmRenames),
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
    merge_changes_made_by_arms(ArmStates, StatusMapBefore,
        ChangedStatusListAfter, VarSet, !RevArms,
        !NextGoalId, !DelayedRenamings, !Specs).

:- pred handle_arm_updated_state_vars(assoc_list(svar, svar_status)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::in,
    prog_varset::in, list(string)::out,
    list(hlds_goal)::out, assoc_list(prog_var, prog_var)::out) is det.

handle_arm_updated_state_vars([], _, _, _, [], [], []).
handle_arm_updated_state_vars([Change | Changes], StatusMapBefore,
        StatusMapAfterArm, VarSet, UninitVarNames, CopyGoals, Renames) :-
    handle_arm_updated_state_vars(Changes, StatusMapBefore, StatusMapAfterArm,
        VarSet, UninitVarNamesTail, CopyGoalsTail, RenamesTail),
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
        term.context_init, umc_implicit("state variable"), [], CopyGoal0),
    goal_add_features([feature_dont_warn_singleton, feature_state_var_copy],
        CopyGoal0, CopyGoal).

%-----------------------------------------------------------------------------%
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

svar_finish_if_then_else(Globals, ModuleName, LocKind, Context, QuantStateVars,
        ThenGoal0, ThenGoal, ElseGoal0, ElseGoal,
        StateBefore, StateAfterCond, StateAfterThen, StateAfterElse,
        StateAfterITE, !VarSet, !Store, !Specs) :-
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

    handle_state_vars_in_ite(Globals, ModuleName, LocKind, QuantStateVars,
        SVarsBefore, StatusMapBefore, StatusMapAfterCond,
        StatusMapAfterThen, StatusMapAfterElse,
        map.init, StatusMapAfterITE, !VarSet,
        [], NeckCopyGoals, [], ThenEndCopyGoals, [], ElseEndCopyGoals,
        [], ThenRenames, [], ElseRenames,
        [], ThenMissingInits, [], ElseMissingInits),
    StateAfterITE = svar_state(StatusMapAfterITE),

    % It is ok for an arm that cannot succeed not to initialize a variable,
    % but we record warnings for them anyway, to be printed in case the
    % procedure has a mode error.
    (
        ThenMissingInits = []
    ;
        ThenMissingInits = [_ | _],
        ThenSpecs0 = !.Store ^ store_specs,
        report_missing_inits_in_ite(Context, ThenMissingInits,
            "succeeds", "fails", ThenSpecs0, ThenSpecs),
        !Store ^ store_specs := ThenSpecs
    ),
    (
        ElseMissingInits = []
    ;
        ElseMissingInits = [_ | _],
        ElseSpecs0 = !.Store ^ store_specs,
        report_missing_inits_in_ite(Context, ElseMissingInits,
            "fails", "succeeds", ElseSpecs0, ElseSpecs),
        !Store ^ store_specs := ElseSpecs
    ),

    svar_goal_to_conj_list(ThenGoal0, ThenGoals0, !Store),
    svar_goal_to_conj_list(ElseGoal0, ElseGoals0, !Store),
    ThenGoals = NeckCopyGoals ++ ThenGoals0 ++ ThenEndCopyGoals,
    ElseGoals = ElseGoals0 ++ ElseEndCopyGoals,
    ThenGoal0 = hlds_goal(_ThenExpr0, ThenInfo0),
    ElseGoal0 = hlds_goal(_ElseExpr0, ElseInfo0),
    conj_list_to_goal(ThenGoals, ThenInfo0, ThenGoal1),
    conj_list_to_goal(ElseGoals, ElseInfo0, ElseGoal1),

    !.Store = svar_store(NextGoalId0, DelayedRenamings0, Specs),
    counter.allocate(ThenGoalIdNum, NextGoalId0, NextGoalId1),
    counter.allocate(ElseGoalIdNum, NextGoalId1, NextGoalId),
    ThenGoalId = goal_id(ThenGoalIdNum),
    ElseGoalId = goal_id(ElseGoalIdNum),
    goal_set_goal_id(ThenGoalId, ThenGoal1, ThenGoal),
    goal_set_goal_id(ElseGoalId, ElseGoal1, ElseGoal),
    map.det_insert(ThenGoalId, ThenRenames,
        DelayedRenamings0, DelayedRenamings1),
    map.det_insert(ElseGoalId, ElseRenames,
        DelayedRenamings1, DelayedRenamings),
    !:Store = svar_store(NextGoalId, DelayedRenamings, Specs).

:- pred handle_state_vars_in_ite(globals::in, module_name::in,
    loc_kind::in, list(svar)::in, list(svar)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::in,
    map(svar, svar_status)::in, map(svar, svar_status)::out,
    prog_varset::in, prog_varset::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    assoc_list(prog_var, prog_var)::in, assoc_list(prog_var, prog_var)::out,
    assoc_list(prog_var, prog_var)::in, assoc_list(prog_var, prog_var)::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out)
    is det.

handle_state_vars_in_ite(_, _, _, _, [], _, _, _, _, !StatusMapAfterITE,
        !VarSet, !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
        !ThenRenames, !ElseRenames, !ThenMissingInits, !ElseMissingInits).
handle_state_vars_in_ite(Globals, ModuleName, LocKind, QuantStateVars,
        [SVar | SVars], StatusMapBefore, StatusMapAfterCond,
        StatusMapAfterThen, StatusMapAfterElse, !StatusMapAfterITE,
        !VarSet, !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
        !ThenRenames, !ElseRenames, !ThenMissingInits, !ElseMissingInits) :-
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
        handle_state_var_in_ite(Globals, ModuleName, LocKind, SVar,
            StatusBefore, StatusBefore, StatusBefore,
            StatusAfterElse, StatusAfterITE,
            !VarSet, !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
            !ThenRenames, !ElseRenames, !ThenMissingInits, !ElseMissingInits)
    else
        % If StatusBefore = status_known_ro(_, _, _), then we would expect
        % StatusBefore = StatusAfterCond
        % StatusBefore = StatusAfterThen
        % StatusBefore = StatusAfterElse
        % However, if the user program actually updates a state variable
        % that should be readonly in this scope, then our recovery from that
        % error would invalidate these expectations.

        handle_state_var_in_ite(Globals, ModuleName, LocKind, SVar,
            StatusBefore, StatusAfterCond, StatusAfterThen,
            StatusAfterElse, StatusAfterITE,
            !VarSet, !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
            !ThenRenames, !ElseRenames, !ThenMissingInits, !ElseMissingInits)
    ),
    map.det_insert(SVar, StatusAfterITE, !StatusMapAfterITE),
    handle_state_vars_in_ite(Globals, ModuleName, LocKind,
        QuantStateVars, SVars, StatusMapBefore, StatusMapAfterCond,
        StatusMapAfterThen, StatusMapAfterElse, !StatusMapAfterITE,
        !VarSet, !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
        !ThenRenames, !ElseRenames, !ThenMissingInits, !ElseMissingInits).

:- pred handle_state_var_in_ite(globals::in, module_name::in,
    loc_kind::in, svar::in,
    svar_status::in, svar_status::in, svar_status::in, svar_status::in,
    svar_status::out, prog_varset::in, prog_varset::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    assoc_list(prog_var, prog_var)::in, assoc_list(prog_var, prog_var)::out,
    assoc_list(prog_var, prog_var)::in, assoc_list(prog_var, prog_var)::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out)
    is det.

handle_state_var_in_ite(Globals, ModuleName, LocKind, SVar, StatusBefore,
        StatusAfterCond, StatusAfterThen, StatusAfterElse, StatusAfterITE,
        !VarSet, !NeckCopyGoals, !ThenEndCopyGoals, !ElseEndCopyGoals,
        !ThenRenames, !ElseRenames, !ThenMissingInits, !ElseMissingInits) :-
    % There are eight cases depending on which of Cond, Then and Else
    % update the state variable:
    %
    % # Cond Then Else  Action
    % 1 no   no   no    do nothing
    % 2 no   no   yes   copy at end of then
    % 3 no   yes  no    copy at end of else
    % 4 no   yes  yes   rename else to match then
    % 5 yes  no   no    copy from cond at start of then, copy at end of else
    % 6 yes  no   yes   copy from cond at start of then
    % 7 yes  yes  no    copy at end of else
    % 8 yes  yes  yes   rename else to match then

    trace [compiletime(flag("state-var-ite")), io(!IO)] (
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

    ( if StatusAfterCond = StatusBefore then
        % Cases 1-4.
        ( if StatusAfterThen = StatusAfterCond then
            % Cases 1-2.
            ( if StatusAfterElse = StatusBefore then
                % Case 1.
                StatusAfterITE = StatusBefore
            else
                % Case 2.
                (
                    StatusBefore = status_known(VarBefore),
                    VarAfterElse =
                        svar_get_current_progvar(LocKind, StatusAfterElse),
                    make_copy_goal(VarBefore, VarAfterElse, CopyGoal),
                    !:ThenEndCopyGoals = [CopyGoal | !.ThenEndCopyGoals],
                    StatusAfterITE = StatusAfterElse
                ;
                    StatusBefore = status_unknown,
                    varset.lookup_name(!.VarSet, SVar, SVarName),
                    !:ThenMissingInits =
                        ["!:" ++ SVarName | !.ThenMissingInits],
                    % We pretend the then part defines StateVar, since this is
                    % the right thing to do when the then part cannot succeed.
                    % If it can, we will generate an error message during
                    % mode analysis.
                    StatusAfterITE = StatusAfterElse
                ;
                    StatusBefore = status_known_ro(_, _, _),
                    % The update of !SVar in the else case was an error,
                    % for which we have already generated an error message.
                    % Because of that, this dummy value won't be used.
                    % XXX Returning StatusAfterElse would cause fewer cascading
                    % error messages, but are those messages useful or not?
                    StatusAfterITE = StatusBefore
                ;
                    ( StatusBefore = status_known_updated(_, _)
                    ; StatusBefore = status_unknown_updated(_)
                    ),
                    % This can happen if LocKind = loc_inside_atomic_goal,
                    % but any reference to !:SVar in the else case should
                    % have just returned the new progvar for SVar.
                    unexpected($pred, "updated before (case 2)")
                )
            )
        else
            % Cases 3-4.
            ( if StatusAfterElse = StatusBefore then
                % Case 3.
                (
                    StatusBefore = status_known(VarBefore),
                    VarAfterThen =
                        svar_get_current_progvar(LocKind, StatusAfterThen),
                    make_copy_goal(VarBefore, VarAfterThen, CopyGoal),
                    !:ElseEndCopyGoals = [CopyGoal | !.ElseEndCopyGoals],
                    StatusAfterITE = StatusAfterThen
                ;
                    StatusBefore = status_unknown,
                    varset.lookup_name(!.VarSet, SVar, SVarName),
                    !:ElseMissingInits =
                        ["!:" ++ SVarName | !.ElseMissingInits],
                    % We pretend the else part defines StateVar, since this is
                    % the right thing to do when the else part cannot succeed.
                    % If it can, we will generate an error message during
                    % mode analysis.
                    StatusAfterITE = StatusAfterThen
                ;
                    StatusBefore = status_known_ro(_, _, _),
                    % The update of !SVar in the then case was an error,
                    % for which we have already generated an error message.
                    % Because of that, this dummy value won't be used.
                    % XXX Returning StatusAfterThen would cause fewer cascading
                    % error messages, but are those messages useful or not?
                    StatusAfterITE = StatusBefore
                ;
                    ( StatusBefore = status_known_updated(_, _)
                    ; StatusBefore = status_unknown_updated(_)
                    ),
                    % This can happen if LocKind = loc_inside_atomic_goal,
                    % but any reference to !:SVar in the then case should
                    % have just returned the new progvar for SVar.
                    unexpected($pred, "updated before (case 3)")
                )
            else
                % Case 4.
                VarAfterThen =
                    svar_get_current_progvar(LocKind, StatusAfterThen),
                VarAfterElse =
                    svar_get_current_progvar(LocKind, StatusAfterElse),
                !:ElseRenames = [VarAfterElse - VarAfterThen | !.ElseRenames],
                StatusAfterITE = StatusAfterThen
            )
        )
    else
        % Cases 5-8.
        ( if StatusAfterThen = StatusAfterCond then
            % Cases 5-6.
            ( if StatusAfterElse = StatusBefore then
                % Case 5.
                (
                    StatusBefore = status_known(VarBefore),
                    new_state_var_instance(SVar, name_middle, FinalVar,
                        !VarSet),
                    VarAfterCond =
                        svar_get_current_progvar(LocKind, StatusAfterCond),

                    make_copy_goal(VarAfterCond, FinalVar, NeckCopyGoal),
                    !:NeckCopyGoals = [NeckCopyGoal | !.NeckCopyGoals],
                    make_copy_goal(VarBefore, FinalVar, ElseCopyGoal),
                    !:ElseEndCopyGoals = [ElseCopyGoal | !.ElseEndCopyGoals],
                    StatusAfterITE = status_known(FinalVar)
                ;
                    StatusBefore = status_unknown,
                    varset.lookup_name(!.VarSet, SVar, SVarName),
                    !:ElseMissingInits =
                        ["!:" ++ SVarName | !.ElseMissingInits],
                    % We pretend the else part defines StateVar, since this is
                    % the right thing to do when the else part cannot succeed.
                    % If it can, we will generate an error message during
                    % mode analysis.
                    new_state_var_instance(SVar, name_middle, FinalVar,
                        !VarSet),
                    VarAfterCond =
                        svar_get_current_progvar(LocKind, StatusAfterCond),
                    make_copy_goal(VarAfterCond, FinalVar, NeckCopyGoal),
                    !:NeckCopyGoals = [NeckCopyGoal | !.NeckCopyGoals],
                    StatusAfterITE = status_known(FinalVar)
                ;
                    StatusBefore = status_known_ro(_, _, _),
                    % The update of !SVar in the condition was an error,
                    % for which we have already generated an error message.
                    % Because of that, this dummy value won't be used.
                    % XXX Returning StatusAfterCond would cause fewer cascading
                    % error messages, but are those messages useful or not?
                    StatusAfterITE = StatusBefore
                ;
                    ( StatusBefore = status_known_updated(_, _)
                    ; StatusBefore = status_unknown_updated(_)
                    ),
                    % This can happen if LocKind = loc_inside_atomic_goal,
                    % but any reference to !:SVar in the condition should
                    % have just returned the new progvar for SVar.
                    unexpected($pred, "updated before (case 5)")
                )
            else
                % Case 6.
                VarAfterCond =
                    svar_get_current_progvar(LocKind, StatusAfterCond),
                VarAfterElse =
                    svar_get_current_progvar(LocKind, StatusAfterElse),
                make_copy_goal(VarAfterCond, VarAfterElse, CopyGoal),
                !:NeckCopyGoals = [CopyGoal | !.NeckCopyGoals],
                StatusAfterITE = StatusAfterElse
            )
        else
            % Cases 7-8.
            ( if StatusAfterElse = StatusBefore then
                % Case 7.
                (
                    StatusBefore = status_known(VarBefore),
                    VarAfterThen =
                        svar_get_current_progvar(LocKind, StatusAfterThen),
                    make_copy_goal(VarBefore, VarAfterThen, CopyGoal),
                    !:ElseEndCopyGoals = [CopyGoal | !.ElseEndCopyGoals],
                    StatusAfterITE = StatusAfterThen
                ;
                    StatusBefore = status_unknown,
                    varset.lookup_name(!.VarSet, SVar, SVarName),
                    !:ElseMissingInits =
                        ["!:" ++ SVarName | !.ElseMissingInits],
                    % We pretend the else part defines StateVar, since this is
                    % the right thing to do when the else part cannot succeed.
                    % If it can, we will generate an error message during
                    % mode analysis.
                    StatusAfterITE = StatusAfterThen
                ;
                    StatusBefore = status_known_ro(_, _, _),
                    % The updates of !SVar in the condition and then cases
                    % were errors, for which we already generated messages.
                    % Because of that, this dummy value won't be used.
                    % XXX Returning StatusAfterThen would cause fewer cascading
                    % error messages, but are those messages useful or not?
                    StatusAfterITE = StatusBefore
                ;
                    ( StatusBefore = status_known_updated(_, _)
                    ; StatusBefore = status_unknown_updated(_)
                    ),
                    % This can happen if LocKind = loc_inside_atomic_goal,
                    % but any reference to !:SVar in the condition and
                    % then case should have just returned the new progvar
                    % for SVar.
                    unexpected($pred, "updated before (case 7)")
                )
            else
                % Case 8.
                VarAfterThen =
                    svar_get_current_progvar(LocKind, StatusAfterThen),
                VarAfterElse =
                    svar_get_current_progvar(LocKind, StatusAfterElse),
                !:ElseRenames = [VarAfterElse - VarAfterThen | !.ElseRenames],
                StatusAfterITE = StatusAfterThen
            )
        )
    ).

%-----------------------------------------------------------------------------%
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
        OuterScopeInfo, !State, !VarSet, !Specs) :-
    StatusMap0 = !.State ^ state_status_map,
    ( if map.remove(OuterStateVar, BeforeStatus, StatusMap0, StatusMap) then
        !State ^ state_status_map := StatusMap,
        (
            BeforeStatus = status_unknown,
            report_uninitialized_state_var(Context, !.VarSet, OuterStateVar,
                !Specs),
            new_state_var_instance(OuterStateVar, name_middle, OuterDIVar,
                !VarSet),
            new_state_var_instance(OuterStateVar, name_middle, OuterUOVar,
                !VarSet),
            OuterScopeInfo = svar_outer_atomic_scope_info(OuterStateVar,
                BeforeStatus, BeforeStatus)
        ;
            BeforeStatus = status_known_ro(OuterDIVar, RO_Construct,
                RO_Context),
            report_illegal_state_var_update(Context,
                ro_construct_name(RO_Construct), RO_Context, !.VarSet,
                OuterStateVar, !Specs),
            new_state_var_instance(OuterStateVar, name_middle, OuterUOVar,
                !VarSet),
            OuterScopeInfo = svar_outer_atomic_scope_info(OuterStateVar,
                BeforeStatus, BeforeStatus)
        ;
            BeforeStatus = status_known(OuterDIVar),
            new_state_var_instance(OuterStateVar, name_middle, OuterUOVar,
                !VarSet),
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
        report_non_visible_state_var("", Context, !.VarSet, OuterStateVar,
            !Specs),
        new_state_var_instance(OuterStateVar, name_middle, OuterDIVar,
            !VarSet),
        new_state_var_instance(OuterStateVar, name_middle, OuterUOVar,
            !VarSet),
        OuterScopeInfo = no_svar_outer_atomic_scope_info
    ).

svar_finish_outer_atomic_scope(OuterScopeInfo, !State) :-
    (
        OuterScopeInfo = svar_outer_atomic_scope_info(OuterStateVar,
            _BeforeStatus, AfterStatus),
        StatusMap0 = !.State ^ state_status_map,
        map.det_insert(OuterStateVar, AfterStatus, StatusMap0, StatusMap),
        !State ^ state_status_map := StatusMap
    ;
        OuterScopeInfo = no_svar_outer_atomic_scope_info
    ).

%-----------------------------------------------------------------------------%

:- type svar_inner_atomic_scope_info
    --->    svar_inner_atomic_scope_info(
                siasi_state_var             :: svar,
                siasi_di_var                :: prog_var,
                siasi_state_before          :: svar_state
            ).

svar_start_inner_atomic_scope(_Context, InnerStateVar, InnerScopeInfo,
        !State, !VarSet, !Specs) :-
    StateBefore = !.State,
    new_state_var_instance(InnerStateVar, name_initial, InnerDIVar, !VarSet),
    StatusMap0 = !.State ^ state_status_map,
    map.set(InnerStateVar, status_known(InnerDIVar), StatusMap0, StatusMap),
    !State ^ state_status_map := StatusMap,
    InnerScopeInfo = svar_inner_atomic_scope_info(InnerStateVar, InnerDIVar,
        StateBefore).

svar_finish_inner_atomic_scope(_Context, InnerScopeInfo,
        InnerDIVar, InnerUOVar, !State, !VarSet, !Specs) :-
    InnerScopeInfo = svar_inner_atomic_scope_info(InnerStateVar, InnerDIVar,
        StateBefore),
    StatusMap0 = !.State ^ state_status_map,
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
    !:State = StateBefore.

%-----------------------------------------------------------------------------%
%
% Look up prog_vars for a state_var.
%

substitute_state_var_mappings([], [], !VarSet, !State, !Specs).
substitute_state_var_mappings([Arg0 | Args0], [Arg | Args], !VarSet, !State,
        !Specs) :-
    substitute_state_var_mapping(Arg0, Arg, !VarSet, !State, !Specs),
    substitute_state_var_mappings(Args0, Args, !VarSet, !State, !Specs).

substitute_state_var_mapping(Arg0, Arg, !VarSet, !State, !Specs) :-
    ( if Arg0 = functor(atom("!."), [variable(StateVar, _)], Context) then
        lookup_dot_state_var(Context, StateVar, Var, !VarSet, !State, !Specs),
        Arg = variable(Var, Context)
    else if Arg0 = functor(atom("!:"), [variable(StateVar, _)], Context) then
        lookup_colon_state_var(Context, StateVar, Var, !VarSet, !State,
            !Specs),
        Arg = variable(Var, Context)
    else
        Arg = Arg0
    ).

lookup_dot_state_var(Context, StateVar, Var, !VarSet, !State, !Specs) :-
    StatusMap0 = !.State ^ state_status_map,
    ( if map.search(StatusMap0, StateVar, Status) then
        (
            Status = status_unknown,
            report_uninitialized_state_var(Context, !.VarSet, StateVar,
                !Specs),
            % We make StateVar known to avoid duplicate reports.
            new_state_var_instance(StateVar, name_middle, Var, !VarSet),
            map.det_update(StateVar, status_known(Var),
                StatusMap0, StatusMap),
            !State ^ state_status_map := StatusMap
        ;
            Status = status_unknown_updated(NewVar),
            report_uninitialized_state_var(Context, !.VarSet, StateVar,
                !Specs),
            % We make StateVar known to avoid duplicate reports.
            new_state_var_instance(StateVar, name_middle, Var, !VarSet),
            map.det_update(StateVar, status_known_updated(Var, NewVar),
                StatusMap0, StatusMap),
            !State ^ state_status_map := StatusMap
        ;
            ( Status = status_known(Var)
            ; Status = status_known_ro(Var, _, _)
            ; Status = status_known_updated(Var, _)
            )
        )
    else
        report_non_visible_state_var(".", Context, !.VarSet, StateVar, !Specs),
        Var = StateVar
    ).

lookup_colon_state_var(Context, StateVar, Var, !VarSet, !State, !Specs) :-
    StatusMap0 = !.State ^ state_status_map,
    ( if map.search(StatusMap0, StateVar, Status) then
        (
            Status = status_unknown,
            new_state_var_instance(StateVar, name_middle, Var, !VarSet),
            map.det_update(StateVar, status_unknown_updated(Var),
                StatusMap0, StatusMap),
            !State ^ state_status_map := StatusMap
        ;
            Status = status_known(OldVar),
            new_state_var_instance(StateVar, name_middle, Var, !VarSet),
            map.det_update(StateVar, status_known_updated(OldVar, Var),
                StatusMap0, StatusMap),
            !State ^ state_status_map := StatusMap
        ;
            Status = status_known_ro(OldVar, RO_Construct, RO_Context),
            (
                RO_Construct = roc_lambda,
                RO_ConstructName = "lambda expression"
            ),
            report_illegal_state_var_update(Context, RO_ConstructName,
                RO_Context, !.VarSet, StateVar, !Specs),
            % We remove the readonly notation to avoid duplicate reports.
            new_state_var_instance(StateVar, name_middle, Var, !VarSet),
            map.det_update(StateVar, status_known_updated(OldVar, Var),
                StatusMap0, StatusMap),
            !State ^ state_status_map := StatusMap
        ;
            Status = status_known_updated(_OldVar, Var)
        ;
            Status = status_unknown_updated(Var)
        )
    else
        report_non_visible_state_var(":", Context, !.VarSet, StateVar, !Specs),
        % We could make StateVar known to avoid duplicate reports.
        % new_state_var_instance(StateVar, name_initial, Var, !VarSet),
        % map.det_insert(StateVar, status_known_updated(Var, Var),
        %     StatusMap0, StatusMap),
        % !State ^ state_status_map := StatusMap
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

%-----------------------------------------------------------------------------%
%
% Code to handle the flattening of conjunctions. We need to be careful when we
% do so, since the goal we flatten could have a goal id, which would mean that
% the svar_store could have a delayed remapping for that goal_id. Just
% flattening the goal would remove the goal_info containing the goal_id from
% the HLDS, and the delayed renaming would not get done.
%
% We therefore make sure that when we flatten such a goal, we ensure that
% its subgoals all have goal_ids (creating new ones if needed), and that
% the delayed renaming that now won't get done on the conjunction as a whole
% *will* get done on each conjunct.
%

svar_flatten_conj(Context, Goals, Goal, !Store) :-
    list.map_foldl(svar_goal_to_conj_list, Goals, GoalConjuncts, !Store),
    list.condense(GoalConjuncts, Conjuncts),
    GoalExpr = conj(plain_conj, Conjuncts),
    goal_info_init(Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

svar_goal_to_conj_list(Goal, Conjuncts, !Store) :-
    % The code here is the same as in svar_goal_to_conj_list_internal,
    % modulo the differences in the argument list.
    Goal = hlds_goal(GoalExpr, GoalInfo),
    ( if GoalExpr = conj(plain_conj, Conjuncts0) then
        !.Store = svar_store(NextGoalId0, DelayedRenamingMap0, Specs),
        GoalId = goal_info_get_goal_id(GoalInfo),
        ( if map.search(DelayedRenamingMap0, GoalId, GoalDelayedRenaming) then
            list.map_foldl2(
                add_conjunct_delayed_renames(GoalDelayedRenaming),
                    Conjuncts0, Conjuncts, NextGoalId0, NextGoalId,
                    DelayedRenamingMap0, DelayedRenamingMap),
            !:Store = svar_store(NextGoalId, DelayedRenamingMap, Specs)
        else
            Conjuncts = Conjuncts0
        )
    else
        Conjuncts = [Goal]
    ).

:- pred svar_goal_to_conj_list_internal(hlds_goal::in, list(hlds_goal)::out,
    counter::in, counter::out,
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
    hlds_goal::in, hlds_goal::out, counter::in, counter::out,
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
        counter.allocate(GoalIdNum, !NextGoalId),
        GoalId = goal_id(GoalIdNum),
        goal_info_set_goal_id(GoalId, GoalInfo0, GoalInfo),
        map.det_insert(GoalId, DelayedRenamingToAdd, !DelayedRenamingMap),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

%-----------------------------------------------------------------------------%
%
% A post-pass to delete unneeded copy unifications. Such unifications
% can hide singleton variable problems.
%

:- pred delete_unneeded_copy_goals(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out) is det.

delete_unneeded_copy_goals(Goal0, Goal, SeenAfter, SeenBefore) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = unify(LHSVar, _, _, _, _),
        goal_vars(Goal0, GoalVars0),
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
        goal_vars(Goal0, GoalVars0),
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
            ( Reason = exist_quant(ScopeVars)
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

%-----------------------------------------------------------------------------%
%
% Test for various kinds of errors.
%

illegal_state_var_func_result(pf_function, ArgTerms, StateVar, Context) :-
    list.last(ArgTerms, LastArgTerm),
    is_term_a_bang_state_pair(LastArgTerm, StateVar, Context).

is_term_a_bang_state_pair(ArgTerm, StateVar, Context) :-
    ArgTerm = functor(atom("!"), [variable(StateVar, Context)], _).

%-----------------------------------------------------------------------------%
%
% Report various kinds of errors.
%

report_illegal_state_var_update(Context, RO_Construct, RO_Context, VarSet,
        StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces1 = [words("Error: cannot use"), fixed("!:" ++ Name),
        words("here due to the surrounding"), words(RO_Construct), suffix(";"),
        words("you may only refer to"), fixed("!." ++ Name), suffix("."), nl],
    Msg1 = simplest_msg(Context, Pieces1),
    Pieces2 = [words("Here is the surrounding context that makes"),
        words("state variable"), fixed(Name), words("readonly."), nl],
    Msg2 = simplest_msg(RO_Context, Pieces2),
    Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
        [Msg1, Msg2]),
    !:Specs = [Spec | !.Specs].

:- func ro_construct_name(readonly_context_kind) = string.

ro_construct_name(roc_lambda) = "lambda expression".

%-----------------------------------------------------------------------------%

report_illegal_func_svar_result(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    % While having !.Var appear as a function argument is quite ordinary,
    % having it appear as a function *result* is not. We therefore do not
    % suggest it as a likely correction.
    Pieces = [words("Error:"), fixed("!" ++ Name),
        words("cannot be a function result."), nl,
        words("You probably meant"), fixed("!:" ++ Name), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

report_illegal_bang_svar_lambda_arg(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:"), fixed("!" ++ Name),
        words("cannot be a lambda argument."), nl,
        words("Perhaps you meant"), fixed("!." ++ Name),
        words("or"), fixed("!:" ++ Name), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_non_visible_state_var(string::in, prog_context::in,
    prog_varset::in, svar::in, list(error_spec)::in, list(error_spec)::out)
    is det.

report_non_visible_state_var(DorC, Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error: state variable"), fixed("!" ++ DorC ++ Name),
        words("is not visible in this context."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_uninitialized_state_var(prog_context::in, prog_varset::in,
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

report_uninitialized_state_var(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: reference to uninitialized state variable"),
        fixed("!." ++ Name), suffix("."), nl],
    Spec = simplest_spec($pred, severity_warning, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_repeated_head_state_var(prog_context::in, prog_varset::in,
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

report_repeated_head_state_var(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: clause head introduces"),
        words("state variable"), fixed(Name), words("more than once."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_state_var_shadow(prog_context::in, prog_varset::in,
    svar::in, list(error_spec)::in, list(error_spec)::out) is det.

report_state_var_shadow(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: new state variable"), fixed(Name),
        words("shadows old one."), nl],
    Spec = conditional_spec($pred, warn_state_var_shadowing, yes,
        severity_warning, phase_parse_tree_to_hlds,
        [simplest_msg(Context, Pieces)]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_missing_inits_in_ite(prog_context::in, list(string)::in,
    string::in, string::in, list(error_spec)::in, list(error_spec)::out)
    is det.

report_missing_inits_in_ite(Context, NextStateVars,
        WhenMissing, WhenNotMissing, !Specs) :-
    Pieces = [words("When the condition"), words(WhenNotMissing), suffix(","),
        words("the if-then-else defines")] ++
        list_to_pieces(NextStateVars) ++ [suffix(","),
        words("but when the condition"), words(WhenMissing), suffix(","),
        words("it does not."), nl],
    Spec = simplest_spec($pred, severity_informational,
        phase_parse_tree_to_hlds, Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred report_missing_inits_in_disjunct(prog_context::in, list(string)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_missing_inits_in_disjunct(Context, NextStateVars, !Specs) :-
    Pieces = [words("Other disjuncts define")] ++
        list_to_pieces(NextStateVars) ++ [suffix(","),
        words("but not this one."), nl],
    Spec = simplest_spec($pred, severity_informational,
        phase_parse_tree_to_hlds, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

report_svar_unify_error(Context, StateVar, !VarSet, !State, !Specs) :-
    Name = varset.lookup_name(!.VarSet, StateVar),
    Pieces = [words("Error:"), fixed("!" ++ Name),
        words("cannot appear as a unification argument."), nl,
        words("You probably meant"), fixed("!." ++ Name),
        words("or"), fixed("!:" ++ Name), suffix(".")],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs],
    !.State = svar_state(StatusMap0),
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
        new_state_var_instance(StateVar, name_initial, Var, !VarSet),
        Status = status_known(Var),
        map.set(StateVar, Status, StatusMap0, StatusMap),
        !:State = svar_state(StatusMap)
    ).

%-----------------------------------------------------------------------------%

:- pred severity_is_error(globals::in, error_spec::in) is semidet.

severity_is_error(Globals, Spec) :-
    actual_spec_severity(Globals, Spec) = yes(actual_severity_error).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.state_var.
%-----------------------------------------------------------------------------%
