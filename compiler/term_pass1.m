%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% term_pass1.m
%
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

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.term_errors.
:- import_module transform_hlds.term_util.

:- import_module io.
:- import_module list.
:- import_module std_util.

%-----------------------------------------------------------------------------%

:- type arg_size_result
    --->    ok(
            list(pair(pred_proc_id, int)),
                    % Gives the gamma of each procedure
                    % in the SCC.
            used_args
                    % Gives the output suppliers of
                    % each procedure in the SCC.
        )
    ;   error(
            list(termination_error_context)
        ).

:- pred find_arg_sizes_in_scc(list(pred_proc_id)::in, module_info::in,
    pass_info::in, arg_size_result::out, list(termination_error_context)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module libs.compiler_util.
:- import_module libs.lp.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.term_traversal.

:- import_module bag.
:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svset.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type pass1_result
    --->    ok(
            list(path_info),
                    % One entry for each path through the
                    % code.
            used_args,
                    % The next output_supplier map.
            list(termination_error_context)
                    % There is an entry in this list for
                    % each procedure in the SCC in which
                    % the set of active vars is not
                    % a subset of the input arguments.
        )
    ;   error(
            list(termination_error_context)
        ).

find_arg_sizes_in_scc(SCC, ModuleInfo, PassInfo, ArgSize, TermErrors, !IO) :-
    init_output_suppliers(SCC, ModuleInfo, InitOutputSupplierMap),
    find_arg_sizes_in_scc_fixpoint(SCC, ModuleInfo, PassInfo,
        InitOutputSupplierMap, Result, TermErrors),
    (
        Result = ok(Paths, OutputSupplierMap, SubsetErrors),
        (
            SubsetErrors = [_|_],
            ArgSize = error(SubsetErrors)
        ;
            SubsetErrors = [],
            (
                Paths = [],
                get_context_from_scc(SCC, ModuleInfo, Context),
                ArgSize = error([Context - no_eqns])
            ;
                Paths = [_|_],
                solve_equations(Paths, SCC, MaybeSolution, !IO),
                (
                    MaybeSolution = yes(Solution),
                    ArgSize = ok(Solution, OutputSupplierMap)
                ;
                    MaybeSolution = no,
                    get_context_from_scc(SCC, ModuleInfo, Context),
                    ArgSize = error([Context - solver_failed])
                )
            )
        )
    ;
        Result = error(Errors),
        ArgSize = error(Errors)
    ).

%-----------------------------------------------------------------------------%

    % Initialise the output suppliers map.
    % Initially, we consider that no input arguments contribute their size
    % to the output arguments.
    %
:- pred init_output_suppliers(list(pred_proc_id)::in, module_info::in,
    used_args::out) is det.

init_output_suppliers([], _, InitMap) :-
    map.init(InitMap).
init_output_suppliers([PPId | PPIds], ModuleInfo, OutputSupplierMap) :-
    init_output_suppliers(PPIds, ModuleInfo, OutputSupplierMap0),
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_headvars(ProcInfo, HeadVars),
    MapToNo = (pred(_HeadVar::in, Bool::out) is det :- Bool = no),
    list.map(MapToNo, HeadVars, BoolList),
    map.det_insert(OutputSupplierMap0, PPId, BoolList, OutputSupplierMap).

%-----------------------------------------------------------------------------%

:- pred find_arg_sizes_in_scc_fixpoint(list(pred_proc_id)::in,
    module_info::in, pass_info::in, used_args::in, pass1_result::out,
    list(termination_error_context)::out) is det.

find_arg_sizes_in_scc_fixpoint(SCC, ModuleInfo, PassInfo, OutputSupplierMap0,
        Result, TermErrors) :-
    find_arg_sizes_in_scc_pass(SCC, ModuleInfo, PassInfo,
        OutputSupplierMap0, [], [], Result1, [], TermErrors1),
    (
        Result1 = error(_),
        Result = Result1,
        TermErrors = TermErrors1
    ;
        Result1 = ok(_, OutputSupplierMap1, _),
        ( OutputSupplierMap1 = OutputSupplierMap0 ->
            Result = Result1,
            TermErrors = TermErrors1
        ;
            find_arg_sizes_in_scc_fixpoint(SCC, ModuleInfo, PassInfo,
                OutputSupplierMap1, Result, TermErrors)
        )
    ).

:- pred find_arg_sizes_in_scc_pass(list(pred_proc_id)::in,
    module_info::in, pass_info::in, used_args::in, list(path_info)::in,
    list(termination_error_context)::in, pass1_result::out,
    list(termination_error_context)::in,
    list(termination_error_context)::out) is det.

find_arg_sizes_in_scc_pass([], _, _, OutputSupplierMap, Paths, SubsetErrors,
        Result, !TermErrors) :-
    Result = ok(Paths, OutputSupplierMap, SubsetErrors).
find_arg_sizes_in_scc_pass([PPId | PPIds], ModuleInfo, PassInfo,
        OutputSupplierMap0, Paths0, SubsetErrors0, Result,
        !TermErrors) :-
    find_arg_sizes_pred(PPId, ModuleInfo, PassInfo, OutputSupplierMap0,
        Result1, ProcTermErrors),
    !:TermErrors = !.TermErrors ++ ProcTermErrors,
    PassInfo = pass_info(_, MaxErrors, _),
    list.take_upto(MaxErrors, !TermErrors),
    (
        Result1 = error(_),
        Result = Result1,
        % The error does not necessarily mean that this SCC is nonterminating.
        % We need to check that the remainder of this SCC does not make any
        % nonterminating calls otherwise we might get a software error during
        % pass 2.
        % (See below for details).
        list.foldl(check_proc_non_term_calls(ModuleInfo), PPIds, [],
            OtherTermErrors),
        list.append(OtherTermErrors, !TermErrors)
    ;
        Result1 = ok(Paths1, OutputSupplierMap1, SubsetErrors1),
        Paths = Paths0 ++ Paths1,
        SubsetErrors = SubsetErrors0 ++ SubsetErrors1,
        find_arg_sizes_in_scc_pass(PPIds, ModuleInfo, PassInfo,
            OutputSupplierMap1, Paths, SubsetErrors, Result,
            !TermErrors)
    ).

%-----------------------------------------------------------------------------%

:- pred find_arg_sizes_pred(pred_proc_id::in, module_info::in,
    pass_info::in, used_args::in, pass1_result::out,
    list(termination_error_context)::out) is det.

find_arg_sizes_pred(PPId, ModuleInfo, PassInfo, OutputSupplierMap0, Result,
        TermErrors) :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    pred_info_context(PredInfo, Context),
    proc_info_headvars(ProcInfo, Args),
    proc_info_argmodes(ProcInfo, ArgModes),
    proc_info_vartypes(ProcInfo, VarTypes),
    proc_info_goal(ProcInfo, Goal),
    map.init(EmptyMap),
    PassInfo = pass_info(FunctorInfo, MaxErrors, MaxPaths),
    init_traversal_params(ModuleInfo, FunctorInfo, PPId, Context, VarTypes,
        OutputSupplierMap0, EmptyMap, MaxErrors, MaxPaths, Params),

    partition_call_args(ModuleInfo, ArgModes, Args, InVars, OutVars),
    Path0 = path_info(PPId, no, 0, [], OutVars),
    set.singleton_set(PathSet0, Path0),
    Info0 = ok(PathSet0, []),
    traverse_goal(Goal, Params, Info0, Info),

    (
        Info = ok(Paths, TermErrors),
        set.to_sorted_list(Paths, PathList),
        upper_bound_active_vars(PathList, AllActiveVars),
        map.lookup(OutputSupplierMap0, PPId,
            OutputSuppliers0),
        update_output_suppliers(Args, AllActiveVars,
            OutputSuppliers0, OutputSuppliers),
        map.det_update(OutputSupplierMap0, PPId,
            OutputSuppliers, OutputSupplierMap),
        ( bag.is_subbag(AllActiveVars, InVars) ->
            SubsetErrors = []
        ;
            SubsetErrors = [Context -
                not_subset(PPId, AllActiveVars, InVars)]
        ),
        Result = ok(PathList, OutputSupplierMap, SubsetErrors)
    ;
        Info = error(Errors, TermErrors),
        Result = error(Errors)
    ).

:- pred update_output_suppliers(list(prog_var)::in, bag(prog_var)::in,
        list(bool)::in, list(bool)::out) is det.

update_output_suppliers([], _ActiveVars, [], []).
update_output_suppliers([_ | _], _ActiveVars, [], []) :-
    unexpected(this_file,
        "update_output_suppliers/4: umatched variables.").
update_output_suppliers([], _ActiveVars, [_ | _], []) :-
    unexpected(this_file,
        "update_output_suppliers/4: umatched variables.").
update_output_suppliers([Arg | Args], ActiveVars,
        [OutputSupplier0 | OutputSuppliers0],
        [OutputSupplier | OutputSuppliers]) :-
    ( bag.contains(ActiveVars, Arg) ->
        OutputSupplier = yes
    ;
        % This guarantees that the set of output suppliers can only
        % increase, which in turn guarantees that our fixpoint
        % computation is monotonic and therefore terminates.
        OutputSupplier = OutputSupplier0
    ),
    update_output_suppliers(Args, ActiveVars,
        OutputSuppliers0, OutputSuppliers).

%-----------------------------------------------------------------------------%
%
% Check if a procedure makes any nonterminating calls
%

% We only use this if we have detected an error at some point during the
% argument size analysis.  The idea is to quickly analyse a procedure and see
% if it does anything that would prevent us from running pass 2.  We cannot
% run pass 2 if the procedure contains any calls to nonterminating procedures
% lower down the call-graph (see term_pass2.m for details).

:- pred check_proc_non_term_calls(module_info::in, pred_proc_id::in,
    list(termination_error_context)::in,
    list(termination_error_context)::out) is det.

check_proc_non_term_calls(ModuleInfo, PPId, !Errors) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_goal(ProcInfo, Goal),
    proc_info_vartypes(ProcInfo, VarTypes),
    check_goal_non_term_calls(ModuleInfo, PPId, VarTypes, Goal, !Errors).

:- pred check_goal_non_term_calls(module_info::in, pred_proc_id::in,
    vartypes::in, hlds_goal::in,
    list(termination_error_context)::in, list(termination_error_context)::out)
    is det.

check_goal_non_term_calls(ModuleInfo, PPId, VarTypes, Goal, !Errors) :-
    Goal = GoalExpr - GoalInfo,
    check_goal_expr_non_term_calls(ModuleInfo, PPId, VarTypes, GoalExpr, GoalInfo,
        !Errors).

:- pred check_goal_expr_non_term_calls(module_info::in, pred_proc_id::in,
    vartypes::in, hlds_goal_expr::in, hlds_goal_info::in,
    list(termination_error_context)::in, list(termination_error_context)::out)
    is det.

check_goal_expr_non_term_calls(ModuleInfo, PPId, VarTypes, Goal, _, !Errors):-
    (
        Goal = conj(Goals)
    ;
        Goal = par_conj(Goals)
    ;
        Goal = disj(Goals)
    ),
    list.foldl(check_goal_non_term_calls(ModuleInfo, PPId, VarTypes), Goals,
        !Errors).
check_goal_expr_non_term_calls(ModuleInfo, PPId, VarTypes, Goal, GoalInfo,
       !Errors) :-
    Goal = call(CallPredId, CallProcId, Args, _, _, _),
    CallPPId = proc(CallPredId, CallProcId),
    module_info_pred_proc_info(ModuleInfo, CallPPId, _, ProcInfo),
    proc_info_get_maybe_termination_info(ProcInfo, TerminationInfo),
    goal_info_get_context(GoalInfo, Context),
    (
        TerminationInfo = yes(can_loop(_)),
        TermError = Context - can_loop_proc_called(PPId, CallPPId),
        list.cons(TermError, !Errors)
    ;
        ( TerminationInfo = yes(cannot_loop(_))
        ; TerminationInfo = no)
    ),
    (
        horder_vars(Args, VarTypes)
    ->
        HigherOrderError = Context - horder_args(PPId, CallPPId),
        list.cons(HigherOrderError, !Errors)
    ;
        true
    ).
check_goal_expr_non_term_calls(_, _, _, Goal, GoalInfo, !Errors) :-
    Goal = generic_call(_, _, _, _),
    goal_info_get_context(GoalInfo, Context),
    list.cons(Context - horder_call, !Errors).
check_goal_expr_non_term_calls(ModuleInfo, PPId, VarTypes, Goal, _, !Errors) :-
    Goal = switch(_, _, Cases),
    list.foldl(check_cases_non_term_calls(ModuleInfo, PPId, VarTypes), Cases,
        !Errors).
check_goal_expr_non_term_calls(_, _, _, unify(_, _, _, _, _), _, !Errors).
check_goal_expr_non_term_calls(ModuleInfo, PPId, VarTypes, not(Goal), _,
        !Errors) :-
    check_goal_non_term_calls(ModuleInfo, PPId, VarTypes, Goal, !Errors).
check_goal_expr_non_term_calls(ModuleInfo, PPId, VarTypes, Goal, _, !Errors) :-
    Goal = scope(_, ScopeGoal),
    check_goal_non_term_calls(ModuleInfo, PPId, VarTypes, ScopeGoal, !Errors).
check_goal_expr_non_term_calls(ModuleInfo, PPId, VarTypes, Goal, _, !Errors) :-
    Goal = if_then_else(_, Cond, Then, Else),
    Goals = [ Cond, Then, Else ],
    list.foldl(check_goal_non_term_calls(ModuleInfo, PPId, VarTypes), Goals,
        !Errors).
check_goal_expr_non_term_calls(_, _, _, Goal, _, !Errors) :-
        Goal = foreign_proc(_, _, _, _, _, _).
check_goal_expr_non_term_calls(_, _, _, shorthand(_), _, _, _) :-
    unexpected(this_file,
        "shorthand goal encountered during termination analysis.").

:- pred check_cases_non_term_calls(module_info::in, pred_proc_id::in,
    vartypes::in, case::in,
    list(termination_error_context)::in,
    list(termination_error_context)::out) is det.

check_cases_non_term_calls(ModuleInfo, PPId, VarTypes, case(_, Goal), !Errors) :-
    check_goal_non_term_calls(ModuleInfo, PPId, VarTypes, Goal, !Errors).

%-----------------------------------------------------------------------------%
%
% Solve the list of constraints
%

:- pred solve_equations(list(path_info)::in, list(pred_proc_id)::in,
    maybe(list(pair(pred_proc_id, int)))::out, io::di, io::uo) is det.

solve_equations(Paths, PPIds, Result, !IO) :-
    (
        convert_equations(Paths, Varset, Equations, Objective, PPVars)
    ->
        map.values(PPVars, AllVars0),
        list.sort_and_remove_dups(AllVars0, AllVars),
        lp_solve(Equations, min, Objective, Varset, AllVars, Soln, !IO),
        (
            Soln = unsatisfiable,
            Result = no
        ;
            Soln = satisfiable(_ObjVal, SolnVal),
            list.map(lookup_coeff(PPVars, SolnVal), PPIds, SolutionList),
            Result = yes(SolutionList)
        )
    ;
        Result = no
    ).

:- pred convert_equations(list(path_info)::in, varset::out, lp.equations::out,
    objective::out, map(pred_proc_id, var)::out) is semidet.

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

:- pred convert_equations_2(list(path_info)::in,
    map(pred_proc_id, var)::in, map(pred_proc_id, var)::out,
    varset::in, varset::out,
    set(lp.equation)::in, set(lp.equation)::out) is semidet.

convert_equations_2([], !PPVars, !Varset, !Eqns).
convert_equations_2([Path | Paths], !PPVars, !Varset, !Eqns) :-
    Path = path_info(ThisPPId, _, IntGamma, PPIds, _),
    FloatGamma = float(IntGamma),
    Eqn = eqn(Coeffs, (>=), FloatGamma),
    pred_proc_var(ThisPPId, ThisVar, !Varset, !PPVars),
    Coeffs = [ThisVar - 1.0 | RestCoeffs],
    Convert = (pred(PPId::in, Coeff::out, !.VS::in, !:VS::out, !.PPV::in,
            !:PPV::out) is det :-
        pred_proc_var(PPId, Var, !VS, !PPV),
        Coeff = Var - (-1.0)
    ),
    list.map_foldl2(Convert, PPIds, RestCoeffs, !Varset, !PPVars),
    svset.insert(Eqn, !Eqns),
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
    ( map.search(!.PPVars, PPId, Var0) ->
        Var = Var0
    ;
        svvarset.new_var(Var, !Varset),
        svmap.det_insert(PPId, Var, !PPVars)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "term_pass1.m".

%-----------------------------------------------------------------------------%
:- end_module term_pass1.
%-----------------------------------------------------------------------------%
