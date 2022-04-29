%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003-2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_pass1.m.
% Main author: crs.
% Significant parts rewritten by zs.
%
% This file contains the first pass of the termination analysis, whose job is
% to discover an upper bound on the difference between the sizes of the output
% arguments of a procedure on the one hand and the sizes of a selected set of
% input arguments of the procedure on the other hand. We refer to this
% selected set of input arguments as the "output suppliers".
%
% For details, please refer to the papers mentioned in termination.m.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_pass1.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.term_errors.
:- import_module transform_hlds.term_util.

:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- type arg_size_result
    --->    arg_size_ok(
                % Gives the gamma of each procedure in the SCC.
                list(pair(pred_proc_id, int)),

                % Gives the output suppliers of each procedure in the SCC.
                used_args
            )
    ;       arg_size_error(
                list(term_error)
            ).

:- pred find_arg_sizes_in_scc(module_info::in, pass_info::in,
    scc::in, arg_size_result::out, list(term_error)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module libs.
:- import_module libs.lp.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.term_traversal.

:- import_module bag.
:- import_module bool.
:- import_module float.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type pass1_result
    --->    term_pass1_ok(
                % One entry for each path through the code.
                list(term_path_info),

                % The next output_supplier map.
                used_args,

                % There is an entry in this list for each procedure in the SCC
                % in which the set of active vars is not a subset of the
                % input arguments.
                list(term_error)
            )
    ;       term_pass1_error(
                list(term_error)
            ).

find_arg_sizes_in_scc(ModuleInfo, PassInfo, SCC, ArgSize, TermErrors) :-
    set.to_sorted_list(SCC, SCCProcs),
    init_output_suppliers(ModuleInfo, SCCProcs, InitOutputSupplierMap),
    find_arg_sizes_in_scc_fixpoint(ModuleInfo, PassInfo, SCCProcs,
        InitOutputSupplierMap, Result, TermErrors),
    (
        Result = term_pass1_ok(Paths, OutputSupplierMap, SubsetErrors),
        (
            SubsetErrors = [_ | _],
            ArgSize = arg_size_error(SubsetErrors)
        ;
            SubsetErrors = [],
            (
                Paths = [],
                get_context_from_scc(ModuleInfo, SCC, Context),
                ArgSizeError = term_error(Context, no_eqns),
                ArgSize = arg_size_error([ArgSizeError])
            ;
                Paths = [_ | _],
                solve_equations(Paths, SCCProcs, MaybeSolution),
                (
                    MaybeSolution = yes(Solution),
                    ArgSize = arg_size_ok(Solution, OutputSupplierMap)
                ;
                    MaybeSolution = no,
                    get_context_from_scc(ModuleInfo, SCC, Context),
                    ArgSizeError = term_error(Context, solver_failed),
                    ArgSize = arg_size_error([ArgSizeError])
                )
            )
        )
    ;
        Result = term_pass1_error(Errors),
        ArgSize = arg_size_error(Errors)
    ).

%-----------------------------------------------------------------------------%

    % Initialise the output suppliers map.
    % Initially, we consider that no input arguments contribute their size
    % to the output arguments.
    %
:- pred init_output_suppliers(module_info::in, list(pred_proc_id)::in,
    used_args::out) is det.

init_output_suppliers(_, [], InitMap) :-
    map.init(InitMap).
init_output_suppliers(ModuleInfo, [PPId | PPIds], OutputSupplierMap) :-
    init_output_suppliers(ModuleInfo, PPIds, OutputSupplierMap0),
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    MapToNo = (pred(_HeadVar::in, Bool::out) is det :- Bool = no),
    list.map(MapToNo, HeadVars, BoolList),
    map.det_insert(PPId, BoolList, OutputSupplierMap0, OutputSupplierMap).

%-----------------------------------------------------------------------------%

:- pred find_arg_sizes_in_scc_fixpoint(module_info::in, pass_info::in,
    list(pred_proc_id)::in, used_args::in,
    pass1_result::out, list(term_error)::out) is det.

find_arg_sizes_in_scc_fixpoint(ModuleInfo, PassInfo, SCC, OutputSupplierMap0,
        Result, TermErrors) :-
    find_arg_sizes_in_scc_pass(ModuleInfo, PassInfo, SCC, OutputSupplierMap0,
        [], [], Result0, [], TermErrors0),
    (
        Result0 = term_pass1_error(_),
        Result = Result0,
        TermErrors = TermErrors0
    ;
        Result0 = term_pass1_ok(_, OutputSupplierMap1, _),
        ( if OutputSupplierMap1 = OutputSupplierMap0 then
            Result = Result0,
            TermErrors = TermErrors0
        else
            find_arg_sizes_in_scc_fixpoint(ModuleInfo, PassInfo, SCC,
                OutputSupplierMap1, Result, TermErrors)
        )
    ).

:- pred find_arg_sizes_in_scc_pass(module_info::in, pass_info::in,
    list(pred_proc_id)::in, used_args::in, list(term_path_info)::in,
    list(term_error)::in, pass1_result::out,
    list(term_error)::in, list(term_error)::out) is det.

find_arg_sizes_in_scc_pass(_, _, [], OutputSupplierMap, Paths, SubsetErrors,
        Result, !TermErrors) :-
    Result = term_pass1_ok(Paths, OutputSupplierMap, SubsetErrors).
find_arg_sizes_in_scc_pass(ModuleInfo, PassInfo, [PPId | PPIds],
        OutputSupplierMap0, Paths0, SubsetErrors0, Result, !TermErrors) :-
    find_arg_sizes_pred(ModuleInfo, PassInfo, PPId, OutputSupplierMap0,
        Result1, ProcTermErrors),
    !:TermErrors = !.TermErrors ++ ProcTermErrors,
    PassInfo = pass_info(_, MaxErrors, _),
    list.take_upto(MaxErrors, !TermErrors),
    (
        Result1 = term_pass1_error(_),
        Result = Result1,

        % The error does not necessarily mean that this SCC is nonterminating.
        % We need to check that the remainder of this SCC does not make any
        % nonterminating calls otherwise we might get a software error during
        % pass 2.
        % (See below for details).

        list.foldl(check_proc_non_term_calls(ModuleInfo), PPIds,
            [], OtherTermErrors),
        list.append(OtherTermErrors, !TermErrors)
    ;
        Result1 = term_pass1_ok(Paths1, OutputSupplierMap1, SubsetErrors1),
        Paths = Paths0 ++ Paths1,
        SubsetErrors = SubsetErrors0 ++ SubsetErrors1,
        find_arg_sizes_in_scc_pass(ModuleInfo, PassInfo, PPIds,
            OutputSupplierMap1, Paths, SubsetErrors, Result, !TermErrors)
    ).

%-----------------------------------------------------------------------------%

:- pred find_arg_sizes_pred(module_info::in, pass_info::in, pred_proc_id::in,
    used_args::in, pass1_result::out, list(term_error)::out) is det.

find_arg_sizes_pred(ModuleInfo, PassInfo, PPId, OutputSupplierMap0, Result,
        TermErrors) :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    pred_info_get_context(PredInfo, Context),
    proc_info_get_headvars(ProcInfo, Args),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_var_table(ModuleInfo, ProcInfo, VarTable),
    proc_info_get_goal(ProcInfo, Goal0),
    % The pretest code we add for compiler-generated unification and comparison
    % predicates uses type casts. It uses them in a way that is guaranteed
    % to terminate, but our analysis is not (yet) able to find this out for
    % itself. We therefore analyse only the non-pretest parts of such goals.
    Goal = maybe_strip_equality_pretest(Goal0),
    map.init(EmptyMap),
    PassInfo = pass_info(FunctorInfo, MaxErrors, MaxPaths),
    init_term_traversal_params(FunctorInfo, PPId, Context, VarTable,
        OutputSupplierMap0, EmptyMap, MaxErrors, MaxPaths, Params),

    partition_call_args(ModuleInfo, ArgModes, Args, InVars, OutVars),
    Path0 = term_path_info(PPId, no, 0, [], OutVars),
    PathSet0 = set.make_singleton_set(Path0),
    Info0 = term_traversal_ok(PathSet0, []),
    term_traverse_goal(ModuleInfo, Params, Goal, Info0, Info),

    (
        Info = term_traversal_ok(Paths, TermErrors),
        set.to_sorted_list(Paths, PathList),
        upper_bound_active_vars(PathList, AllActiveVars),
        map.lookup(OutputSupplierMap0, PPId, OutputSuppliers0),
        update_output_suppliers(Args, AllActiveVars,
            OutputSuppliers0, OutputSuppliers),
        map.det_update(PPId, OutputSuppliers,
            OutputSupplierMap0, OutputSupplierMap),
        ( if bag.is_subbag(AllActiveVars, InVars) then
            SubsetErrors = []
        else
            SubsetErrorKind = not_subset(PPId, AllActiveVars, InVars),
            SubsetError = term_error(Context, SubsetErrorKind),
            SubsetErrors = [SubsetError]
        ),
        Result = term_pass1_ok(PathList, OutputSupplierMap, SubsetErrors)
    ;
        Info = term_traversal_error(Errors, TermErrors),
        Result = term_pass1_error(Errors)
    ).

:- pred update_output_suppliers(list(prog_var)::in, bag(prog_var)::in,
    list(bool)::in, list(bool)::out) is det.

update_output_suppliers([], _ActiveVars, [], []).
update_output_suppliers([_ | _], _ActiveVars, [], []) :-
    unexpected($pred, "umatched variables").
update_output_suppliers([], _ActiveVars, [_ | _], []) :-
    unexpected($pred, "umatched variables").
update_output_suppliers([Arg | Args], ActiveVars,
        [OutputSupplier0 | OutputSuppliers0],
        [OutputSupplier | OutputSuppliers]) :-
    ( if bag.contains(ActiveVars, Arg) then
        OutputSupplier = yes
    else
        % This guarantees that the set of output suppliers can only increase,
        % which in turn guarantees that our fixpoint computation is
        % monotonic and therefore terminates.
        OutputSupplier = OutputSupplier0
    ),
    update_output_suppliers(Args, ActiveVars,
        OutputSuppliers0, OutputSuppliers).

%-----------------------------------------------------------------------------%
%
% Check if a procedure makes any nonterminating calls.
%

% We only use this if we have detected an error at some point during the
% argument size analysis.  The idea is to quickly analyse a procedure and see
% if it does anything that would prevent us from running pass 2.  We cannot
% run pass 2 if the procedure contains any calls to nonterminating procedures
% lower down the call-graph (see term_pass2.m for details).

:- pred check_proc_non_term_calls(module_info::in, pred_proc_id::in,
    list(term_error)::in, list(term_error)::out) is det.

check_proc_non_term_calls(ModuleInfo, PPId, !Errors) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_goal(ProcInfo, Goal),
    proc_info_get_var_table(ModuleInfo, ProcInfo, VarTable),
    check_goal_non_term_calls(ModuleInfo, PPId, VarTable, Goal, !Errors).

:- pred check_goal_non_term_calls(module_info::in, pred_proc_id::in,
    var_table::in, hlds_goal::in,
    list(term_error)::in, list(term_error)::out) is det.

check_goal_non_term_calls(ModuleInfo, PPId, VarTable, Goal, !Errors) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(_, _, _, _, _)
        % Do nothing.
    ;
        GoalExpr = plain_call(CallPredId, CallProcId, Args, _, _, _),
        CallPPId = proc(CallPredId, CallProcId),
        module_info_pred_proc_info(ModuleInfo, CallPPId, _, ProcInfo),
        proc_info_get_maybe_termination_info(ProcInfo, TerminationInfo),
        Context = goal_info_get_context(GoalInfo),
        (
            TerminationInfo = yes(can_loop(_)),
            CanLoopErrorKind = can_loop_proc_called(PPId, CallPPId),
            CanLoopError = term_error(Context, CanLoopErrorKind),
            list.cons(CanLoopError, !Errors)
        ;
            ( TerminationInfo = yes(cannot_loop(_))
            ; TerminationInfo = no
            )
        ),
        ( if some_var_is_higher_order(VarTable, Args) then
            HigherOrderErrorKind = horder_args(PPId, CallPPId),
            HigherOrderError = term_error(Context, HigherOrderErrorKind),
            list.cons(HigherOrderError, !Errors)
        else
            true
        )
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        % XXX This looks incomplete - juliensf.
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        % XXX We should use any results from closure analysis here.
        Context = goal_info_get_context(GoalInfo),
        Error = term_error(Context, horder_call),
        list.cons(Error, !Errors)
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        list.foldl(check_goal_non_term_calls(ModuleInfo, PPId, VarTable),
            Goals, !Errors)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl(check_cases_non_term_calls(ModuleInfo, PPId, VarTable),
            Cases, !Errors)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        Goals = [Cond, Then, Else],
        list.foldl(check_goal_non_term_calls(ModuleInfo, PPId, VarTable),
            Goals, !Errors)
    ;
        GoalExpr = negation(SubGoal),
        check_goal_non_term_calls(ModuleInfo, PPId, VarTable, SubGoal, !Errors)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % The scope has no calls, let alone nonterminating calls.
            true
        else
            check_goal_non_term_calls(ModuleInfo, PPId, VarTable, SubGoal,
                !Errors)
        )
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred check_cases_non_term_calls(module_info::in,
    pred_proc_id::in, var_table::in, case::in,
    list(term_error)::in, list(term_error)::out) is det.

check_cases_non_term_calls(ModuleInfo, PPId, VarTable, Case, !Errors) :-
    Case = case(_, _, Goal),
    check_goal_non_term_calls(ModuleInfo, PPId, VarTable, Goal, !Errors).

%-----------------------------------------------------------------------------%
%
% Solve the list of constraints.
%

:- pred solve_equations(list(term_path_info)::in, list(pred_proc_id)::in,
    maybe(list(pair(pred_proc_id, int)))::out) is det.

solve_equations(Paths, PPIds, Result) :-
    ( if convert_equations(Paths, Varset, Equations, Objective, PPVars) then
        map.values(PPVars, AllVars0),
        list.sort_and_remove_dups(AllVars0, AllVars),
        lp_solve(Equations, min, Objective, Varset, AllVars, Soln),
        (
            Soln = unsatisfiable,
            Result = no
        ;
            Soln = satisfiable(_ObjVal, SolnVal),
            list.map(lookup_coeff(PPVars, SolnVal), PPIds, SolutionList),
            Result = yes(SolutionList)
        )
    else
        Result = no
    ).

:- pred convert_equations(list(term_path_info)::in, varset::out,
    lp.equations::out, objective::out, map(pred_proc_id, var)::out) is semidet.

convert_equations(Paths, Varset, Equations, Objective, PPVars) :-
    varset.init(Varset0),
    map.init(PPVars0),
    set.init(EqnSet0),
    convert_equations_2(Paths, PPVars0, PPVars, Varset0, Varset,
        EqnSet0, EqnSet),
    set.to_sorted_list(EqnSet, Equations),
    map.values(PPVars, Vars),
    Convert = (pred(Var::in, Coeff::out) is det :- Coeff = Var - 1.0),
    list.map(Convert, Vars, Objective).

:- pred convert_equations_2(list(term_path_info)::in,
    map(pred_proc_id, var)::in, map(pred_proc_id, var)::out,
    varset::in, varset::out,
    set(lp.equation)::in, set(lp.equation)::out) is semidet.

convert_equations_2([], !PPVars, !Varset, !Eqns).
convert_equations_2([Path | Paths], !PPVars, !Varset, !Eqns) :-
    Path = term_path_info(ThisPPId, _, IntGamma, PPIds, _),
    FloatGamma = float(IntGamma),
    Eqn = eqn(Coeffs, op_ge, FloatGamma),
    pred_proc_var(ThisPPId, ThisVar, !Varset, !PPVars),
    Coeffs = [ThisVar - 1.0 | RestCoeffs],
    Convert = (pred(PPId::in, Coeff::out, !.VS::in, !:VS::out,
            !.PPV::in, !:PPV::out) is det :-
        pred_proc_var(PPId, Var, !VS, !PPV),
        Coeff = Var - (-1.0)
    ),
    list.map_foldl2(Convert, PPIds, RestCoeffs, !Varset, !PPVars),
    set.insert(Eqn, !Eqns),
    convert_equations_2(Paths, !PPVars, !Varset, !Eqns).

:- pred lookup_coeff(map(pred_proc_id, var)::in, map(var, float)::in,
    pred_proc_id::in, pair(pred_proc_id, int)::out) is det.

lookup_coeff(PPIds, Soln, PPId, PPId - ICoeff) :-
    map.lookup(PPIds, PPId, Var),
    map.lookup(Soln, Var, Coeff),
    ICoeff = float.ceiling_to_int(Coeff).

:- pred pred_proc_var(pred_proc_id::in, var::out, varset::in, varset::out,
    map(pred_proc_id, var)::in, map(pred_proc_id, var)::out) is det.

pred_proc_var(PPId, Var, !Varset, !PPVars) :-
    ( if map.search(!.PPVars, PPId, Var0) then
        Var = Var0
    else
        varset.new_var(Var, !Varset),
        map.det_insert(PPId, Var, !PPVars)
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_pass1.
%-----------------------------------------------------------------------------%
