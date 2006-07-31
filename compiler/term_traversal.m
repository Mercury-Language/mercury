%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2006 The University of Melbourne.
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

:- type traversal_info
    --->    ok(
                set(path_info),
                    % Information about the paths we have followed. With
                    % a conjunction of length N, each of whose elements
                    % is a branched control structure, the number of
                    % paths through the conjunction is 2^N. The reason
                    % why we use a set of path_infos instead of a list
                    % is that this can postpone the representation
                    % getting too big if (as is at least moderately
                    % likely) many of the paths have identical
                    % properties.
                
                list(termination_error_context)
                    % Have we processed a call to a procedure whose
                    % maybe termination info was yes(can_loop(_))?  If
                    % yes, record the error here.  (This is not an error
                    % in pass 1, but we want to find this out in pass 1
                    % so we can avoid doing pass 2.)
        )
    ;   
        error(
                list(termination_error_context),
                    % Errors which are fatal in both passes.
                
                list(termination_error_context)
                    % Have we processed a call to a procedure whose
                    % maybe termination info was yes(can_loop(_))?  If
                    % yes, record the error here.  (This is not an error
                    % in pass 1, but we want to find this out in pass 1
                    % so we can avoid doing pass 2.)
        ).

:- type path_info
    --->    path_info(
            
                pred_proc_id,
                    % The identify of the procedure
                    % that this path is within.
            
                maybe(pair(pred_proc_id, prog_context)),
                    % If no, path was started at the end
                    % of the procedure given by field 1.
                    % If yes, the arg names the procedure
                    % at the call to which the path started
                    % and the context of the call.
                    % In pass 1, all starts should be no.
                    % In pass 2, all starts should be yes.
            
                int,
                list(pred_proc_id),
                bag(prog_var)
                    % These three fields describe the right hand side
                    % of the inequation we are propagating.
        ).

:- type traversal_params.

:- pred init_traversal_params(functor_info::in,
    pred_proc_id::in, prog_context::in, vartypes::in,
    used_args::in, used_args::in, int::in, int::in,
    traversal_params::out) is det.

:- pred traverse_goal(
    hlds_goal::in, traversal_params::in,
    traversal_info::in, traversal_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred upper_bound_active_vars(list(path_info)::in, bag(prog_var)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module string.
:- import_module svset.

%-----------------------------------------------------------------------------%

traverse_goal(Goal, Params, !Info, !ModuleInfo, !IO) :-
    Goal = GoalExpr - GoalInfo,
    (
        goal_info_get_determinism(GoalInfo, Detism),
        determinism_components(Detism, _, at_most_zero)
    ->
        cannot_succeed(!Info)
    ;
        true
    ),
    traverse_goal_2(GoalExpr, GoalInfo, Params, !Info, !ModuleInfo, !IO).

:- pred traverse_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
    traversal_params::in, traversal_info::in, traversal_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

traverse_goal_2(Goal, _GoalInfo, Params, !Info, !ModuleInfo, !IO) :-
    Goal = unify(_Var, _RHS, _UniMode, Unification, _Context),
    (
        Unification = construct(OutVar, ConsId, Args, Modes, _, _, _),
        (
            unify_change(!.ModuleInfo, OutVar, ConsId, Args, Modes, Params,
                Gamma, InVars, OutVars0)
        ->
            bag.insert(OutVars0, OutVar, OutVars),
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
            bag.insert(InVars0, InVar, InVars),
            Gamma = 0 - Gamma0,
            record_change(InVars, OutVars, Gamma, [], !Info)
        ;
            unexpected(this_file,
                "traverse_goal_2/5: higher order deconstruction.")
        )
    ;
        Unification = assign(OutVar, InVar),
        bag.init(Empty),
        bag.insert(Empty, InVar, InVars),
        bag.insert(Empty, OutVar, OutVars),
        record_change(InVars, OutVars, 0, [], !Info)
    ;
        Unification = simple_test(_InVar1, _InVar2)
    ;
        Unification = complicated_unify(_, _, _),
        unexpected(this_file, "traverse_goal_2/5: complicated unify.")
    ).

traverse_goal_2(conj(_, Goals), _, Params, !Info, !ModuleInfo, !IO) :-
    list.reverse(Goals, RevGoals),
    traverse_conj(RevGoals, Params, !Info, !ModuleInfo, !IO).

traverse_goal_2(switch(_, _, Cases), _, Params, !Info, !ModuleInfo, !IO) :-
    traverse_switch(Cases, Params, !Info, !ModuleInfo, !IO).

traverse_goal_2(disj(Goals), _, Params, !Info, !ModuleInfo, !IO) :-
    traverse_disj(Goals, Params, !Info, !ModuleInfo, !IO).

traverse_goal_2(negation(Goal), _, Params, !Info, !ModuleInfo, !IO) :-
    % The negated goal will not affect the argument sizes since it cannot bind
    % any active variables.  However, we must traverse it during pass 1 to
    % ensure that it does not call any non-terminating procedures.  Pass 2
    % relies on pass 1 having done this.
    traverse_goal(Goal, Params, !Info, !ModuleInfo, !IO).

traverse_goal_2(scope(_, Goal), _GoalInfo, Params, !Info, !ModuleInfo, !IO) :-
    traverse_goal(Goal, Params, !Info, !ModuleInfo, !IO).

traverse_goal_2(Goal, _, Params, !Info, !ModuleInfo, !IO) :-
    Goal = if_then_else(_, Cond, Then, Else),
    traverse_conj([Then, Cond], Params, !.Info, CondThenInfo, !ModuleInfo,
        !IO),
    traverse_goal(Else, Params, !.Info, ElseInfo, !ModuleInfo, !IO),
    combine_paths(CondThenInfo, ElseInfo, Params, !:Info).

traverse_goal_2(Goal, GoalInfo, Params, !Info, !ModuleInfo, !IO) :-
    Goal = call_foreign_proc(Attributes, CallPredId, CallProcId, Args,
        _, _, _),
    module_info_pred_proc_info(!.ModuleInfo, CallPredId, CallProcId, _,
        CallProcInfo),
    proc_info_get_argmodes(CallProcInfo, CallArgModes),
    ArgVars = list.map(foreign_arg_var, Args),
    partition_call_args(!.ModuleInfo, CallArgModes, ArgVars, _InVars, OutVars),
    goal_info_get_context(GoalInfo, Context),

    ( is_termination_known(!.ModuleInfo, proc(CallPredId, CallProcId)) ->
        error_if_intersect(OutVars, Context, pragma_foreign_code, !Info)
    ;
        ( attributes_imply_termination(Attributes) ->
            error_if_intersect(OutVars, Context, pragma_foreign_code, !Info)
        ;
            add_error(Context, does_not_term_pragma(CallPredId), Params, !Info)
        )
    ).
            
traverse_goal_2(Goal, GoalInfo, Params, !Info, !ModuleInfo, !IO) :-
    Goal = generic_call(Details, Args, ArgModes, _),
    goal_info_get_context(GoalInfo, Context),
    (
        Details = higher_order(Var, _, _, _),
        ClosureValueMap = goal_info_get_ho_values(GoalInfo),
        %
        % If closure analysis has identified a set of values this higher-order
        % variable can take, then we can check if they terminate.  We cannot
        % anything about the size of the arguments of the higher-order call,
        % so we assume that they are unbounded.
        %
        ( ClosureValues0 = ClosureValueMap ^ elem(Var) ->
            ClosureValues = set.to_sorted_list(ClosureValues0),
            % XXX intermod
            list.filter(terminates(!.ModuleInfo), ClosureValues,
                Terminating, NonTerminating),
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
        %
        % For class method calls, we could probably analyse further than this,
        % since we know that the method being called must come from one of the
        % instance declarations, and we could potentially (globally) analyse
        % these.
        %
        add_error(Context, method_call, Params, !Info)
    ;
        Details = cast(_)
    ).

traverse_goal_2(Goal, GoalInfo, Params, !Info, !ModuleInfo, !IO) :-
    Goal = plain_call(CallPredId, CallProcId, Args, _, _, _),
    goal_info_get_context(GoalInfo, Context),
    params_get_ppid(Params, PPId),
    CallPPId = proc(CallPredId, CallProcId),

    module_info_pred_proc_info(!.ModuleInfo, CallPredId, CallProcId, _,
        CallProcInfo),
    proc_info_get_argmodes(CallProcInfo, CallArgModes),
    % XXX intermod
    proc_info_get_maybe_arg_size_info(CallProcInfo, CallArgSizeInfo),
    proc_info_get_maybe_termination_info(CallProcInfo, CallTerminationInfo),

    partition_call_args(!.ModuleInfo, CallArgModes, Args, InVars, OutVars),

    % Handle existing paths
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
        % We should get to this point only in pass 1.  In pass 2,
        % OutputSuppliersMap will be empty, which will lead to a runtime abort
        % in map.lookup.
        params_get_output_suppliers(Params, OutputSuppliersMap),
        map.lookup(OutputSuppliersMap, CallPPId, OutputSuppliers),
        remove_unused_args(InVars, Args, OutputSuppliers, UsedInVars),
        record_change(UsedInVars, OutVars, 0, [CallPPId], !Info)
    ),

    % Did we call a non-terminating procedure?
    (
        CallTerminationInfo = yes(can_loop(_))
    ->
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
        %
        compute_rec_start_vars(Args, RecInputSuppliers, Bag),
        PathStart = yes(CallPPId - Context),
        NewPath = path_info(PPId, PathStart, 0, [], Bag),
        add_path(NewPath, !Info)
    ;
        true
    ).

traverse_goal_2(shorthand(_), _, _, _, _, _, _, _, _) :-
    % These should have been expanded out by now.
    unexpected(this_file, "traverse_goal_2/5: shorthand goal.").

%-----------------------------------------------------------------------------%

    % traverse_conj should be invoked with a reversed list of goals.
    % This is to keep stack consumption down.
    %
:- pred traverse_conj(hlds_goals::in, traversal_params::in,
    traversal_info::in, traversal_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

traverse_conj([], _, !Info, !ModuleInfo, !IO).
traverse_conj([Goal | Goals], Params, !Info, !ModuleInfo, !IO) :-
    traverse_goal(Goal, Params, !Info, !ModuleInfo, !IO),
    traverse_conj(Goals, Params, !Info, !ModuleInfo, !IO).

:- pred traverse_disj(hlds_goals::in, traversal_params::in,
    traversal_info::in, traversal_info::out, module_info::in,
    module_info::out, io::di, io::uo) is det.

traverse_disj([], _, _, ok(Empty, []), !ModuleInfo, !IO) :-
    set.init(Empty).
traverse_disj([Goal | Goals], Params, !Info, !ModuleInfo, !IO) :-
    traverse_goal(Goal, Params, !.Info, GoalInfo, !ModuleInfo, !IO),
    traverse_disj(Goals, Params, !.Info, GoalsInfo, !ModuleInfo, !IO),
    combine_paths(GoalInfo, GoalsInfo, Params, !:Info).

:- pred traverse_switch(list(case)::in, traversal_params::in,
    traversal_info::in, traversal_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

traverse_switch([], _, _, ok(Empty, []), !ModuleInfo, !IO) :-
    set.init(Empty).
traverse_switch([case(_, Goal) | Cases], Params, !Info, !ModuleInfo, !IO) :-
    traverse_goal(Goal, Params, !.Info, GoalInfo, !ModuleInfo, !IO),
    traverse_switch(Cases, Params, !.Info, CasesInfo, !ModuleInfo, !IO),
    combine_paths(GoalInfo, CasesInfo, Params, !:Info).

%-----------------------------------------------------------------------------%

:- pred cannot_succeed(traversal_info::in, traversal_info::out) is det.

cannot_succeed(error(Errors, CanLoop), error(Errors, CanLoop)).
cannot_succeed(ok(_, CanLoop), ok(Empty, CanLoop)) :-
    set.init(Empty).

:- pred add_path(path_info::in, traversal_info::in, traversal_info::out) is det.

add_path(_, error(Errors, CanLoop), error(Errors, CanLoop)).
add_path(Path, ok(Paths0, CanLoop), ok(Paths, CanLoop)) :-
    set.insert(Paths0, Path, Paths).

:- pred add_error(prog_context::in, termination_error::in,
    traversal_params::in, traversal_info::in, traversal_info::out) is det.

add_error(Context, Error, Params, error(Errors0, CanLoop),
        error(Errors, CanLoop)) :-
    Errors1 = [Context - Error | Errors0],
    params_get_max_errors(Params, MaxErrors),
    list.take_upto(MaxErrors, Errors1, Errors).
add_error(Context, Error, _, ok(_, CanLoop),
        error([Context - Error], CanLoop)).

:- pred called_can_loop(prog_context::in, termination_error::in,
    traversal_params::in, traversal_info::in, traversal_info::out) is det.

called_can_loop(Context, Error, Params, error(Errors, CanLoop0),
        error(Errors, CanLoop)) :-
    CanLoop1 = [Context - Error | CanLoop0],
    params_get_max_errors(Params, MaxErrors),
    list.take_upto(MaxErrors, CanLoop1, CanLoop).
called_can_loop(Context, Error, Params, ok(Paths, CanLoop0),
        ok(Paths, CanLoop)) :-
    CanLoop1 = [Context - Error | CanLoop0],
    params_get_max_errors(Params, MaxErrors),
    list.take_upto(MaxErrors, CanLoop1, CanLoop).

:- pred combine_paths(traversal_info::in, traversal_info::in,
    traversal_params::in, traversal_info::out) is det.

combine_paths(error(Errors1, CanLoop1), error(Errors2, CanLoop2), Params,
        error(Errors, CanLoop)) :-
    params_get_max_errors(Params, MaxErrors),
    list.append(Errors1, Errors2, Errors3),
    list.take_upto(MaxErrors, Errors3, Errors),
    list.append(CanLoop1, CanLoop2, CanLoop3),
    list.take_upto(MaxErrors, CanLoop3, CanLoop).
combine_paths(error(Errors1, CanLoop1), ok(_, CanLoop2), Params,
        error(Errors1, CanLoop)) :-
    params_get_max_errors(Params, MaxErrors),
    list.append(CanLoop1, CanLoop2, CanLoop3),
    list.take_upto(MaxErrors, CanLoop3, CanLoop).
combine_paths(ok(_, CanLoop1), error(Errors2, CanLoop2), Params,
        error(Errors2, CanLoop)) :-
    params_get_max_errors(Params, MaxErrors),
    list.append(CanLoop1, CanLoop2, CanLoop3),
    list.take_upto(MaxErrors, CanLoop3, CanLoop).
combine_paths(ok(Paths1, CanLoop1), ok(Paths2, CanLoop2), Params,
        Info) :-
    params_get_max_errors(Params, MaxErrors),
    list.append(CanLoop1, CanLoop2, CanLoop3),
    list.take_upto(MaxErrors, CanLoop3, CanLoop),
    set.union(Paths2, Paths1, Paths),
    params_get_max_paths(Params, MaxPaths),
    (
        % Don't try to track the state of too many paths;
        % doing so can require too much memory.
        set.count(Paths, Count),
        Count =< MaxPaths
    ->
        Info = ok(Paths, CanLoop)
    ;
        params_get_context(Params, Context),
        Info = error([Context - too_many_paths], CanLoop)
    ).

%-----------------------------------------------------------------------------%

:- pred compute_rec_start_vars(list(prog_var)::in, list(bool)::in,
    bag(prog_var)::out) is det.

compute_rec_start_vars([], [], Out) :-
    bag.init(Out).
compute_rec_start_vars([_ | _], [], _Out) :-
    unexpected(this_file,
        "compute_rec_start_vars/3: unmatched variables.").
compute_rec_start_vars([], [_ | _], _Out) :-
    unexpected(this_file,
        "compute_rec_start_vars/3: unmatched variables.").
compute_rec_start_vars([Var | Vars], [RecInputSupplier | RecInputSuppliers],
        Out) :-
    compute_rec_start_vars(Vars, RecInputSuppliers, Out1),
    ( RecInputSupplier = yes ->
        bag.insert(Out1, Var, Out)
    ;
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
    list(prog_var)::in, list(uni_mode)::in, traversal_params::in, int::out,
    bag(prog_var)::out, bag(prog_var)::out) is semidet.

unify_change(ModuleInfo, OutVar, ConsId, Args0, Modes0, Params, Gamma,
        InVars, OutVars) :-
    params_get_functor_info(Params, FunctorInfo),
    params_get_var_types(Params, VarTypes),
    map.lookup(VarTypes, OutVar, Type),
    \+ type_is_higher_order(Type, _, _, _, _),
    ( type_to_ctor_and_args(Type, TypeCtor, _) ->
        filter_args_and_modes(VarTypes, Args0, Args1, Modes0, Modes1),
        functor_norm(FunctorInfo, TypeCtor, ConsId, ModuleInfo,
            Gamma, Args1, Args, Modes1, Modes),
        split_unification_vars(Args, Modes, ModuleInfo, InVars, OutVars)
    ;
        unexpected(this_file, "unify_change/8: variable type.")
    ).

:- pred filter_args_and_modes(vartypes::in, list(prog_var)::in,
    list(prog_var)::out, list(uni_mode)::in, list(uni_mode)::out) is det.

filter_args_and_modes(VarTypes, Args0, Args, Modes0, Modes) :-
    assoc_list.from_corresponding_lists(Args0, Modes0, ArgsAndModes0),
    IsNotTypeInfo = (pred(ArgMode::in) is semidet :-
        map.lookup(VarTypes, fst(ArgMode), Type),
        not is_introduced_type_info_type(Type)
    ),
    list.filter(IsNotTypeInfo, ArgsAndModes0, ArgsAndModes),
    assoc_list.keys_and_values(ArgsAndModes, Args, Modes).

%-----------------------------------------------------------------------------%

:- pred record_change(bag(prog_var)::in, bag(prog_var)::in, int::in,
    list(pred_proc_id)::in, traversal_info::in, traversal_info::out) is det.

record_change(_, _, _, _, error(Errors, CanLoop), error(Errors, CanLoop)).
record_change(InVars, OutVars, Gamma, CalledPPIds, ok(Paths0, CanLoop),
        ok(NewPaths, CanLoop)) :-
    set.to_sorted_list(Paths0, PathsList0),
    set.init(NewPaths0),
    record_change_2(PathsList0, InVars, OutVars, Gamma, CalledPPIds,
        NewPaths0, NewPaths).

:- pred record_change_2(list(path_info)::in, bag(prog_var)::in,
    bag(prog_var)::in, int::in, list(pred_proc_id)::in,
    set(path_info)::in, set(path_info)::out) is det.

record_change_2([], _, _, _, _, !PathSet).
record_change_2([Path0 | Paths0], InVars, OutVars, CallGamma, CallPPIds,
        !PathSet) :-
    Path0 = path_info(ProcData, Start, Gamma0, PPIds0, Vars0),
    ( bag.intersect(OutVars, Vars0) ->
        % The change produces some active variables.
        Gamma = CallGamma + Gamma0,
        list.append(CallPPIds, PPIds0, PPIds),
        bag.subtract(Vars0, OutVars, Vars1),
        bag.union(InVars, Vars1, Vars),
        Path = path_info(ProcData, Start, Gamma, PPIds, Vars)
    ;
        % The change produces no active variables.
        Path = Path0
    ),
    svset.insert(Path, !PathSet),
    record_change_2(Paths0, InVars, OutVars, CallGamma, CallPPIds, !PathSet).

%-----------------------------------------------------------------------------%

:- pred error_if_intersect(bag(prog_var)::in, prog_context::in,
    termination_error::in, traversal_info::in, traversal_info::out) is det.

error_if_intersect(_, _, _, error(Errors, CanLoop), error(Errors, CanLoop)).
error_if_intersect(OutVars, Context, ErrorMsg, ok(Paths, CanLoop), Info) :-
    (
        set.to_sorted_list(Paths, PathList),
        some_active_vars_in_bag(PathList, OutVars)
    ->
        Info = error([Context - ErrorMsg], CanLoop)
    ;
        Info = ok(Paths, CanLoop)
    ).

:- pred some_active_vars_in_bag(list(path_info)::in,
    bag(prog_var)::in) is semidet.

some_active_vars_in_bag([Path | Paths], OutVars) :-
    (
        Path = path_info(_, _, _, _, Vars),
        bag.intersect(Vars, OutVars)
    ;
        some_active_vars_in_bag(Paths, OutVars)
    ).

%-----------------------------------------------------------------------------%

upper_bound_active_vars([], ActiveVars) :-
    bag.init(ActiveVars).
upper_bound_active_vars([Path | Paths], ActiveVars) :-
    upper_bound_active_vars(Paths, ActiveVars1),
    Path = path_info(_, _, _, _, ActiveVars2),
    bag.least_upper_bound(ActiveVars1, ActiveVars2, ActiveVars).

%-----------------------------------------------------------------------------%

:- type traversal_params
    --->    traversal_params(
                functor_info :: functor_info,
                
                ppid :: pred_proc_id,
                % The procedure we are tracing through.
                
                context :: prog_context,
                % The context of the procedure.
                
                vartypes :: vartypes,
                
                output_suppliers :: map(pred_proc_id, list(bool)),
                % Output suppliers of each procedure.
                % Empty during pass 2.
                    
                rec_input_supplier  :: map(pred_proc_id, list(bool)),
                % Recursive input suppliers of each procedure.
                % Empty during pass 1.
                
                max_errors :: int,
                % Maximum number of errors to gather.
                
                max_paths :: int
                % Maximum number of paths to analyze.
        ).

init_traversal_params(FunctorInfo, PredProcId, Context, VarTypes,
        OutputSuppliers, RecInputSuppliers, MaxErrors, MaxPaths,
        Params) :-
    Params = traversal_params(FunctorInfo, PredProcId, Context,
        VarTypes, OutputSuppliers, RecInputSuppliers,
        MaxErrors, MaxPaths).

:- pred params_get_functor_info(traversal_params::in, functor_info::out)
    is det.
:- pred params_get_ppid(traversal_params::in, pred_proc_id::out)
    is det.
:- pred params_get_context(traversal_params::in, prog_context::out)
    is det.
:- pred params_get_var_types(traversal_params::in, vartypes::out)
    is det.
:- pred params_get_output_suppliers(traversal_params::in,
    map(pred_proc_id, list(bool))::out) is det.
:- pred params_get_rec_input_suppliers(traversal_params::in,
    map(pred_proc_id, list(bool))::out) is det.
:- pred params_get_max_errors(traversal_params::in, int::out) is det.
:- pred params_get_max_paths(traversal_params::in, int::out) is det.

params_get_functor_info(Params, Params ^ functor_info).
params_get_ppid(Params, Params ^ ppid).
params_get_context(Params, Params ^ context).
params_get_var_types(Params, Params ^ vartypes).
params_get_output_suppliers(Params, Params ^ output_suppliers).
params_get_rec_input_suppliers(Params, Params ^ rec_input_supplier).
params_get_max_errors(Params, Params ^ max_errors).
params_get_max_paths(Params, Params ^ max_paths).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "term_traversal.m".

%-----------------------------------------------------------------------------%
:- end_module term_traversal.
%-----------------------------------------------------------------------------%
