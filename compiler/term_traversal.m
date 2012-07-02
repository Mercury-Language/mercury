%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_traversal.m.
% Main author: crs.
% Significant rewrite by zs.
%
% This module contains the code used to traverse procedure bodies
% for both passes of termination analysis.
%
% For details, please refer to the papers mentioned in termination.m.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_traversal.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.term_errors.
:- import_module transform_hlds.term_norm.
:- import_module transform_hlds.term_util.

:- import_module bag.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type term_traversal_info
    --->    term_traversal_ok(
                % Information about the paths we have followed. With a
                % conjunction of length N, each of whose elements is a
                % branched control structure, the number of paths through
                % the conjunction is 2^N. The reason why we use a set of
                % term_path_infos instead of a list is that this can postpone
                % the representation getting too big if (as is at least
                % moderately likely) many of the paths have identical
                % properties.
                set(term_path_info),

                % Have we processed a call to a procedure whose maybe
                % termination info was yes(can_loop(_))? If yes, record
                % the error here. (This is not an error in pass 1, but
                % we want to find this out in pass 1 so we can avoid
                % doing pass 2.)
                list(termination_error_context)
            )
    ;       term_traversal_error(
                % Errors which are fatal in both passes.
                list(termination_error_context),

                % Have we processed a call to a procedure whose maybe
                % termination info was yes(can_loop(_))? If yes, record
                % the error here. (This is not an error in pass 1, but
                % we want to find this out in pass 1 so we can avoid
                % doing pass 2.)
                list(termination_error_context)
            ).

:- type term_path_info
    --->    term_path_info(
                % The identity of the procedure that this path is within.
                pred_proc_id,

                % If no, path was started at the end of the procedure
                % given by field 1. If yes, the arg names the procedure
                % at the call to which the path started and the context
                % of the call.
                %
                % In pass 1, all starts should be no.
                % In pass 2, all starts should be yes.
                maybe(pair(pred_proc_id, prog_context)),

                % These three fields describe the right hand side
                % of the inequation we are propagating.
                int,
                list(pred_proc_id),
                bag(prog_var)
            ).

:- type term_traversal_params.

:- pred init_term_traversal_params(functor_info::in,
    pred_proc_id::in, prog_context::in, vartypes::in,
    used_args::in, used_args::in, int::in, int::in,
    term_traversal_params::out) is det.

:- pred term_traverse_goal(hlds_goal::in, term_traversal_params::in,
    term_traversal_info::in, term_traversal_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred upper_bound_active_vars(list(term_path_info)::in, bag(prog_var)::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%

term_traverse_goal(Goal, Params, !Info, !ModuleInfo, !IO) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        Detism = goal_info_get_determinism(GoalInfo),
        determinism_components(Detism, _, at_most_zero)
    ->
        cannot_succeed(!Info)
    ;
        true
    ),
    (
        GoalExpr = unify(_Var, _RHS, _UniMode, Unification, _Context),
        (
            Unification = construct(OutVar, ConsId, Args, Modes, _, _, _),
            (
                unify_change(!.ModuleInfo, OutVar, ConsId, Args, Modes, Params,
                    Gamma, InVars, OutVars0)
            ->
                bag.insert(OutVar, OutVars0, OutVars),
                record_change(InVars, OutVars, Gamma, [], !Info)
            ;
                % length(Args) is not necessarily equal to length(Modes)
                % for higher order constructions.
                true
            )
        ;
            Unification = deconstruct(InVar, ConsId, Args, Modes, _, _),
            (
                unify_change(!.ModuleInfo, InVar, ConsId, Args, Modes, Params,
                    Gamma0, InVars0, OutVars)
            ->
                bag.insert(InVar, InVars0, InVars),
                Gamma = 0 - Gamma0,
                record_change(InVars, OutVars, Gamma, [], !Info)
            ;
                unexpected($module, $pred, "higher order deconstruction")
            )
        ;
            Unification = assign(OutVar, InVar),
            bag.init(Empty),
            bag.insert(InVar, Empty, InVars),
            bag.insert(OutVar, Empty, OutVars),
            record_change(InVars, OutVars, 0, [], !Info)
        ;
            Unification = simple_test(_InVar1, _InVar2)
        ;
            Unification = complicated_unify(_, _, _),
            unexpected($module, $pred, "complicated unify")
        )
    ;
        GoalExpr = plain_call(CallPredId, CallProcId, Args, _, _, _),
        Context = goal_info_get_context(GoalInfo),
        params_get_ppid(Params, PPId),
        CallPPId = proc(CallPredId, CallProcId),

        module_info_pred_proc_info(!.ModuleInfo, CallPredId, CallProcId, _,
            CallProcInfo),
        proc_info_get_argmodes(CallProcInfo, CallArgModes),
        % XXX intermod
        proc_info_get_maybe_arg_size_info(CallProcInfo, CallArgSizeInfo),
        proc_info_get_maybe_termination_info(CallProcInfo, CallTerminationInfo),

        partition_call_args(!.ModuleInfo, CallArgModes, Args, InVars, OutVars),

        % Handle existing paths.
        (
            CallArgSizeInfo = yes(finite(CallGamma, OutputSuppliers)),
            remove_unused_args(InVars, Args, OutputSuppliers, UsedInVars),
            record_change(UsedInVars, OutVars, CallGamma, [], !Info)
        ;
            CallArgSizeInfo = yes(infinite(_)),
            error_if_intersect(OutVars, Context,
                inf_termination_const(PPId, CallPPId), !Info)
        ;
            CallArgSizeInfo = no,
            % We should get to this point only in pass 1. In pass 2,
            % OutputSuppliersMap will be empty, which will lead to
            % a runtime abort in map.lookup.
            params_get_output_suppliers(Params, OutputSuppliersMap),
            map.lookup(OutputSuppliersMap, CallPPId, OutputSuppliers),
            remove_unused_args(InVars, Args, OutputSuppliers, UsedInVars),
            record_change(UsedInVars, OutVars, 0, [CallPPId], !Info)
        ),

        % Did we call a non-terminating procedure?
        ( CallTerminationInfo = yes(can_loop(_)) ->
            called_can_loop(Context, can_loop_proc_called(PPId, CallPPId),
                Params, !Info)
        ;
            true
        ),

        % Did we call a procedure with some procedure-valued arguments?
        (
            % XXX This is an overapproximation, since it includes
            % higher order outputs.
            params_get_var_types(Params, VarTypes),
            horder_vars(Args, VarTypes)
        ->
            add_error(Context, horder_args(PPId, CallPPId), Params, !Info)
        ;
            true
        ),

        % Do we start another path?
        (
            params_get_rec_input_suppliers(Params, RecInputSuppliersMap),
            map.search(RecInputSuppliersMap, CallPPId, RecInputSuppliers)
        ->
            % We should get to this point only in pass 2, and then
            % only if this call is to a procedure in the current SCC.
            % In pass 1, RecInputSuppliersMap will be empty.
            compute_rec_start_vars(Args, RecInputSuppliers, Bag),
            PathStart = yes(CallPPId - Context),
            NewPath = term_path_info(PPId, PathStart, 0, [], Bag),
            add_path(NewPath, !Info)
        ;
            true
        )
    ;
        GoalExpr = call_foreign_proc(Attributes, CallPredId, CallProcId, Args,
            _, _, _),
        module_info_pred_proc_info(!.ModuleInfo, CallPredId, CallProcId, _,
            CallProcInfo),
        proc_info_get_argmodes(CallProcInfo, CallArgModes),
        ArgVars = list.map(foreign_arg_var, Args),
        partition_call_args(!.ModuleInfo, CallArgModes, ArgVars,
            _InVars, OutVars),
        Context = goal_info_get_context(GoalInfo),

        ( is_termination_known(!.ModuleInfo, proc(CallPredId, CallProcId)) ->
            error_if_intersect(OutVars, Context, pragma_foreign_code, !Info)
        ;
            ( attributes_imply_termination(Attributes) ->
                error_if_intersect(OutVars, Context, pragma_foreign_code,
                    !Info)
            ;
                add_error(Context, does_not_term_pragma(CallPredId), Params,
                    !Info)
            )
        )
    ;
        GoalExpr = generic_call(Details, Args, ArgModes, _, _),
        Context = goal_info_get_context(GoalInfo),
        (
            Details = higher_order(Var, _, _, _),
            ClosureValueMap = goal_info_get_ho_values(GoalInfo),

            % If closure analysis has identified a set of values this
            % higher-order variable can take, then we can check if they all
            % terminate. We cannot find out anything about the sizes of the
            % arguments of the higher-order call, so we assume that they are
            % unbounded.
            ( map.search(ClosureValueMap, Var, ClosureValues0) ->
                ClosureValues = set.to_sorted_list(ClosureValues0),
                % XXX intermod
                list.filter(pred_proc_id_terminates(!.ModuleInfo),
                    ClosureValues, Terminating, NonTerminating),
                (
                    NonTerminating = [],
                    partition_call_args(!.ModuleInfo, ArgModes, Args,
                        _InVars, OutVars),
                    params_get_ppid(Params, PPId),
                    Error = ho_inf_termination_const(PPId, Terminating),
                    error_if_intersect(OutVars, Context, Error, !Info)
                ;
                    NonTerminating = [_ | _],
                    % XXX We should tell the user what the
                    % non-terminating closures are.
                    add_error(Context, horder_call, Params, !Info)
                )
            ;
                add_error(Context, horder_call, Params, !Info)
            )
        ;
            Details = class_method(_, _, _, _),
            % For class method calls, we could probably analyse further
            % than this, since we know that the method being called must
            % come from one of the instance declarations, and we could
            % potentially (globally) analyse these.
            add_error(Context, method_call, Params, !Info)
        ;
            Details = event_call(_)
        ;
            Details = cast(_)
        )
    ;
        GoalExpr = conj(_, Goals),
        list.reverse(Goals, RevGoals),
        term_traverse_conj(RevGoals, Params, !Info, !ModuleInfo, !IO)
    ;
        GoalExpr = disj(Goals),
        term_traverse_disj(Goals, Params, !Info, !ModuleInfo, !IO)
    ;
        GoalExpr = switch(_, _, Cases),
        term_traverse_switch(Cases, Params, !Info, !ModuleInfo, !IO)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        term_traverse_conj([Then, Cond], Params, !.Info, CondThenInfo,
            !ModuleInfo, !IO),
        term_traverse_goal(Else, Params, !.Info, ElseInfo, !ModuleInfo, !IO),
        combine_paths(CondThenInfo, ElseInfo, Params, !:Info)
    ;
        GoalExpr = negation(SubGoal),
        % The negated goal will not affect the argument sizes since
        % it cannot bind any active variables. However, we must traverse it
        % during pass 1 to ensure that it does not call any non-terminating
        % procedures. Pass 2 relies on pass 1 having done this.
        term_traverse_goal(SubGoal, Params, !Info, !ModuleInfo, !IO)
    ;
        GoalExpr = scope(_, SubGoal),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        term_traverse_goal(SubGoal, Params, !Info, !ModuleInfo, !IO)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, $pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

    % traverse_conj should be invoked with a reversed list of goals.
    % This is to keep stack consumption down.
    %
:- pred term_traverse_conj(hlds_goals::in, term_traversal_params::in,
    term_traversal_info::in, term_traversal_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

term_traverse_conj([], _, !Info, !ModuleInfo, !IO).
term_traverse_conj([Goal | Goals], Params, !Info, !ModuleInfo, !IO) :-
    term_traverse_goal(Goal, Params, !Info, !ModuleInfo, !IO),
    term_traverse_conj(Goals, Params, !Info, !ModuleInfo, !IO).

:- pred term_traverse_disj(hlds_goals::in, term_traversal_params::in,
    term_traversal_info::in, term_traversal_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

term_traverse_disj([], _, _, term_traversal_ok(Empty, []), !ModuleInfo, !IO) :-
    set.init(Empty).
term_traverse_disj([Goal | Goals], Params, !Info, !ModuleInfo, !IO) :-
    term_traverse_goal(Goal, Params, !.Info, GoalInfo, !ModuleInfo, !IO),
    term_traverse_disj(Goals, Params, !.Info, GoalsInfo, !ModuleInfo, !IO),
    combine_paths(GoalInfo, GoalsInfo, Params, !:Info).

:- pred term_traverse_switch(list(case)::in, term_traversal_params::in,
    term_traversal_info::in, term_traversal_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

term_traverse_switch([], _, _, term_traversal_ok(Empty, []),
        !ModuleInfo, !IO) :-
    set.init(Empty).
term_traverse_switch([case(_, _, Goal) | Cases], Params, !Info,
        !ModuleInfo, !IO) :-
    term_traverse_goal(Goal, Params, !.Info, GoalInfo, !ModuleInfo, !IO),
    term_traverse_switch(Cases, Params, !.Info, CasesInfo, !ModuleInfo, !IO),
    combine_paths(GoalInfo, CasesInfo, Params, !:Info).

%-----------------------------------------------------------------------------%

:- pred cannot_succeed(term_traversal_info::in, term_traversal_info::out)
    is det.

cannot_succeed(Info0, Info) :-
    (
        Info0 = term_traversal_error(_, _),
        Info = Info0
    ;
        Info0 = term_traversal_ok(_, CanLoop),
        Info = term_traversal_ok(set.init, CanLoop)
    ).

:- pred add_path(term_path_info::in,
    term_traversal_info::in, term_traversal_info::out) is det.

add_path(Path, Info0, Info) :-
    (
        Info0 = term_traversal_error(_, _),
        Info = Info0
    ;
        Info0 = term_traversal_ok(Paths0, CanLoop),
        set.insert(Path, Paths0, Paths),
        Info = term_traversal_ok(Paths, CanLoop)
    ).

:- pred add_error(prog_context::in, termination_error::in,
    term_traversal_params::in,
    term_traversal_info::in, term_traversal_info::out) is det.

add_error(Context, Error, Params, Info0, Info) :-
    (
        Info0 = term_traversal_error(Errors0, CanLoop),
        Errors1 = [termination_error_context(Error, Context) | Errors0],
        params_get_max_errors(Params, MaxErrors),
        list.take_upto(MaxErrors, Errors1, Errors),
        Info = term_traversal_error(Errors, CanLoop)
    ;
        Info0 = term_traversal_ok(_, CanLoop),
        ErrorContext = termination_error_context(Error, Context),
        Info = term_traversal_error([ErrorContext], CanLoop)
    ).

:- pred called_can_loop(prog_context::in, termination_error::in,
    term_traversal_params::in,
    term_traversal_info::in, term_traversal_info::out) is det.

called_can_loop(Context, Error, Params, Info0, Info) :-
    (
        Info0 = term_traversal_error(Errors, CanLoop0),
        CanLoop1 = [termination_error_context(Error, Context) | CanLoop0],
        params_get_max_errors(Params, MaxErrors),
        list.take_upto(MaxErrors, CanLoop1, CanLoop),
        Info = term_traversal_error(Errors, CanLoop)
    ;
        Info0 = term_traversal_ok(Paths, CanLoop0),
        CanLoop1 = [termination_error_context(Error, Context) | CanLoop0],
        params_get_max_errors(Params, MaxErrors),
        list.take_upto(MaxErrors, CanLoop1, CanLoop),
        Info = term_traversal_ok(Paths, CanLoop)
    ).

:- pred combine_paths(term_traversal_info::in, term_traversal_info::in,
    term_traversal_params::in, term_traversal_info::out) is det.

combine_paths(InfoA, InfoB, Params, Info) :-
    (
        InfoA = term_traversal_error(ErrorsA, CanLoopA),
        InfoB = term_traversal_error(ErrorsB, CanLoopB),
        params_get_max_errors(Params, MaxErrors),
        list.take_upto(MaxErrors, ErrorsA ++ ErrorsB, Errors),
        list.take_upto(MaxErrors, CanLoopA ++ CanLoopB, CanLoop),
        Info = term_traversal_error(Errors, CanLoop)
    ;
        InfoA = term_traversal_error(ErrorsA, CanLoopA),
        InfoB = term_traversal_ok(_, CanLoopB),
        params_get_max_errors(Params, MaxErrors),
        list.take_upto(MaxErrors, CanLoopA ++ CanLoopB, CanLoop),
        Info = term_traversal_error(ErrorsA, CanLoop)
    ;
        InfoA = term_traversal_ok(_, CanLoopA),
        InfoB = term_traversal_error(ErrorsB, CanLoopB),
        params_get_max_errors(Params, MaxErrors),
        list.take_upto(MaxErrors, CanLoopA ++ CanLoopB, CanLoop),
        Info = term_traversal_error(ErrorsB, CanLoop)
    ;
        InfoA = term_traversal_ok(PathsA, CanLoopA),
        InfoB = term_traversal_ok(PathsB, CanLoopB),
        params_get_max_errors(Params, MaxErrors),
        list.take_upto(MaxErrors, CanLoopA ++ CanLoopB, CanLoop),
        set.union(PathsB, PathsA, Paths),
        params_get_max_paths(Params, MaxPaths),
        (
            % Don't try to track the state of too many paths;
            % doing so can require too much memory.
            set.count(Paths, Count),
            Count =< MaxPaths
        ->
            Info = term_traversal_ok(Paths, CanLoop)
        ;
            params_get_context(Params, Context),
            ErrorContext = termination_error_context(too_many_paths, Context),
            Info = term_traversal_error([ErrorContext], CanLoop)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred compute_rec_start_vars(list(prog_var)::in, list(bool)::in,
    bag(prog_var)::out) is det.

compute_rec_start_vars([], [], Out) :-
    bag.init(Out).
compute_rec_start_vars([_ | _], [], _Out) :-
    unexpected($module, $pred, "unmatched variables").
compute_rec_start_vars([], [_ | _], _Out) :-
    unexpected($module, $pred, "unmatched variables").
compute_rec_start_vars([Var | Vars], [RecInputSupplier | RecInputSuppliers],
        Out) :-
    compute_rec_start_vars(Vars, RecInputSuppliers, Out1),
    (
        RecInputSupplier = yes,
        bag.insert(Var, Out1, Out)
    ;
        RecInputSupplier = no,
        Out = Out1
    ).

%-----------------------------------------------------------------------------%

    % unify_change is invoked for unifications of the form X = f(Yi),
    % with the first argument giving the identity of X, the second the
    % identity of f, the third and fourth the identity and modes of the Yi.
    % unify_change returns the norm of f and the bags of input and output
    % variables among the Yi. It is up to the caller to look after the
    % sign of the norm of f and after the membership of X in either the
    % input or output bags. The predicate fails if invoked on a higher
    % order unification.
    %
:- pred unify_change(module_info::in, prog_var::in, cons_id::in,
    list(prog_var)::in, list(uni_mode)::in, term_traversal_params::in,
    int::out, bag(prog_var)::out, bag(prog_var)::out) is semidet.

unify_change(ModuleInfo, OutVar, ConsId, Args0, Modes0, Params, Gamma,
        InVars, OutVars) :-
    params_get_functor_info(Params, FunctorInfo),
    params_get_var_types(Params, VarTypes),
    lookup_var_type(VarTypes, OutVar, Type),
    \+ type_is_higher_order(Type),
    \+ (
        ConsId = type_info_const(_)
    ;
        ConsId = typeclass_info_const(_)
    ),
    require_det (
        type_to_ctor_det(Type, TypeCtor),
        filter_typeinfos_from_args_and_modes(VarTypes, Args0, Args1,
            Modes0, Modes1),
        functor_norm(ModuleInfo, FunctorInfo, TypeCtor, ConsId, Gamma,
            Args1, Args, Modes1, Modes),
        split_unification_vars(ModuleInfo, Args, Modes, InVars, OutVars)
    ).

:- pred filter_typeinfos_from_args_and_modes(vartypes::in,
    list(prog_var)::in, list(prog_var)::out,
    list(uni_mode)::in, list(uni_mode)::out) is det.

filter_typeinfos_from_args_and_modes(_, [], [], [], []).
filter_typeinfos_from_args_and_modes(_, [], _, [_ | _], _) :-
    unexpected($module, $pred, "list length mismatch").
filter_typeinfos_from_args_and_modes(_, [_ | _], _, [], _) :-
    unexpected($module, $pred, "list length mismatch").
filter_typeinfos_from_args_and_modes(VarTypes, [Arg0 | Args0], Args,
        [Mode0 | Modes0], Modes) :-
    filter_typeinfos_from_args_and_modes(VarTypes, Args0, TailArgs,
        Modes0, TailModes),
    lookup_var_type(VarTypes, Arg0, Type),
    ( is_introduced_type_info_type(Type) ->
        Args = TailArgs,
        Modes = TailModes
    ;
        Args = [Arg0 | TailArgs],
        Modes = [Mode0 | TailModes]
    ).

%-----------------------------------------------------------------------------%

:- pred record_change(bag(prog_var)::in, bag(prog_var)::in, int::in,
    list(pred_proc_id)::in, term_traversal_info::in, term_traversal_info::out)
    is det.

record_change(InVars, OutVars, Gamma, CalledPPIds, Info0, Info) :-
    (
        Info0 = term_traversal_error(_, _),
        Info = Info0
    ;
        Info0 = term_traversal_ok(Paths0, CanLoop),
        set.to_sorted_list(Paths0, PathsList0),
        set.init(NewPaths0),
        record_change_2(PathsList0, InVars, OutVars, Gamma, CalledPPIds,
            NewPaths0, NewPaths),
        Info = term_traversal_ok(NewPaths, CanLoop)
    ).

:- pred record_change_2(list(term_path_info)::in, bag(prog_var)::in,
    bag(prog_var)::in, int::in, list(pred_proc_id)::in,
    set(term_path_info)::in, set(term_path_info)::out) is det.

record_change_2([], _, _, _, _, !PathSet).
record_change_2([Path0 | Paths0], InVars, OutVars, CallGamma, CallPPIds,
        !PathSet) :-
    Path0 = term_path_info(ProcData, Start, Gamma0, PPIds0, Vars0),
    ( bag.intersect(OutVars, Vars0) ->
        % The change produces some active variables.
        Gamma = CallGamma + Gamma0,
        list.append(CallPPIds, PPIds0, PPIds),
        bag.subtract(Vars0, OutVars, Vars1),
        bag.union(InVars, Vars1, Vars),
        Path = term_path_info(ProcData, Start, Gamma, PPIds, Vars)
    ;
        % The change produces no active variables.
        Path = Path0
    ),
    set.insert(Path, !PathSet),
    record_change_2(Paths0, InVars, OutVars, CallGamma, CallPPIds, !PathSet).

%-----------------------------------------------------------------------------%

:- pred error_if_intersect(bag(prog_var)::in, prog_context::in,
    termination_error::in, term_traversal_info::in, term_traversal_info::out)
    is det.

error_if_intersect(OutVars, Context, ErrorMsg, Info0, Info) :-
    (
        Info0 = term_traversal_error(_, _),
        Info = Info0
    ;
        Info0 = term_traversal_ok(Paths, CanLoop),
        (
            set.to_sorted_list(Paths, PathList),
            some_active_vars_in_bag(PathList, OutVars)
        ->
            ErrorContext = termination_error_context(ErrorMsg, Context),
            Info = term_traversal_error([ErrorContext], CanLoop)
        ;
            Info = term_traversal_ok(Paths, CanLoop)
        )
    ).

:- pred some_active_vars_in_bag(list(term_path_info)::in,
    bag(prog_var)::in) is semidet.

some_active_vars_in_bag([Path | Paths], OutVars) :-
    (
        Path = term_path_info(_, _, _, _, Vars),
        bag.intersect(Vars, OutVars)
    ;
        some_active_vars_in_bag(Paths, OutVars)
    ).

%-----------------------------------------------------------------------------%

upper_bound_active_vars([], ActiveVars) :-
    bag.init(ActiveVars).
upper_bound_active_vars([Path | Paths], ActiveVars) :-
    upper_bound_active_vars(Paths, ActiveVars1),
    Path = term_path_info(_, _, _, _, ActiveVars2),
    bag.least_upper_bound(ActiveVars1, ActiveVars2, ActiveVars).

%-----------------------------------------------------------------------------%

:- type term_traversal_params
    --->    term_traversal_params(
                term_trav_functor_info      :: functor_info,

                % The procedure we are tracing through.
                term_trav_ppid              :: pred_proc_id,

                % The context of the procedure.
                term_trav_context           :: prog_context,

                term_trav_vartypes          :: vartypes,

                % Output suppliers of each procedure.
                % Empty during pass 2.
                term_trav_output_suppliers  :: map(pred_proc_id, list(bool)),

                % Recursive input suppliers of each procedure.
                % Empty during pass 1.
                term_trav_rec_input_supplier :: map(pred_proc_id, list(bool)),

                % Maximum number of errors to gather.
                term_trav_max_errors        :: int,

                % Maximum number of paths to analyze.
                term_trav_max_paths         :: int
        ).

init_term_traversal_params(FunctorInfo, PredProcId, Context, VarTypes,
        OutputSuppliers, RecInputSuppliers, MaxErrors, MaxPaths,
        Params) :-
    Params = term_traversal_params(FunctorInfo, PredProcId, Context,
        VarTypes, OutputSuppliers, RecInputSuppliers,
        MaxErrors, MaxPaths).

:- pred params_get_functor_info(term_traversal_params::in, functor_info::out)
    is det.
:- pred params_get_ppid(term_traversal_params::in, pred_proc_id::out)
    is det.
:- pred params_get_context(term_traversal_params::in, prog_context::out)
    is det.
:- pred params_get_var_types(term_traversal_params::in, vartypes::out)
    is det.
:- pred params_get_output_suppliers(term_traversal_params::in,
    map(pred_proc_id, list(bool))::out) is det.
:- pred params_get_rec_input_suppliers(term_traversal_params::in,
    map(pred_proc_id, list(bool))::out) is det.
:- pred params_get_max_errors(term_traversal_params::in, int::out) is det.
:- pred params_get_max_paths(term_traversal_params::in, int::out) is det.

params_get_functor_info(Params, Params ^ term_trav_functor_info).
params_get_ppid(Params, Params ^ term_trav_ppid).
params_get_context(Params, Params ^ term_trav_context).
params_get_var_types(Params, Params ^ term_trav_vartypes).
params_get_output_suppliers(Params, Params ^ term_trav_output_suppliers).
params_get_rec_input_suppliers(Params, Params ^ term_trav_rec_input_supplier).
params_get_max_errors(Params, Params ^ term_trav_max_errors).
params_get_max_paths(Params, Params ^ term_trav_max_paths).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_traversal.
%-----------------------------------------------------------------------------%
