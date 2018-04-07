%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File rbmm.live_variable_analysis.m.
% Main author: Quan Phan.
%
% This module implements the live variable analysis.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.rbmm.live_variable_analysis.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module transform_hlds.rbmm.region_liveness_info.

    % Collects live variable sets.
    %
:- pred live_variable_analysis(module_info::in, execution_path_table::in,
    proc_pp_varset_table::out, proc_pp_varset_table::out,
    proc_pp_varset_table::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.smm_common.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% Live variable analysis
%

% For each procedure, compute the sets of live variables before and after
% each program point.
% Currently, it also calculates set of void variables (i.e., ones whose names
% start with "_") after each program point. Those variables are considered
% dead at that point.
%

live_variable_analysis(ModuleInfo, ExecPathTable, LVBeforeTable,
        LVAfterTable, VoidVarTable) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    map.init(LVBeforeTable0),
    map.init(LVAfterTable0),
    map.init(VoidVarTable0),
    list.foldl3(live_variable_analysis_pred(ModuleInfo, ExecPathTable),
        PredIds, LVBeforeTable0, LVBeforeTable, LVAfterTable0, LVAfterTable,
        VoidVarTable0, VoidVarTable).

:- pred live_variable_analysis_pred(module_info::in, execution_path_table::in,
    pred_id::in, proc_pp_varset_table::in, proc_pp_varset_table::out,
    proc_pp_varset_table::in, proc_pp_varset_table::out,
    proc_pp_varset_table::in, proc_pp_varset_table::out) is det.

live_variable_analysis_pred(ModuleInfo, ExecPathTable, PredId,
        !LVBeforeTable, !LVAfterTable, !VoidVarTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_non_imported_procids(PredInfo) = ProcIds,
    list.foldl3(
        live_variable_analysis_proc(ModuleInfo, ExecPathTable, PredId),
        ProcIds, !LVBeforeTable, !LVAfterTable, !VoidVarTable).

:- pred live_variable_analysis_proc(module_info::in,
    execution_path_table::in, pred_id::in, proc_id::in,
    proc_pp_varset_table::in, proc_pp_varset_table::out,
    proc_pp_varset_table::in, proc_pp_varset_table::out,
    proc_pp_varset_table::in, proc_pp_varset_table::out) is det.

live_variable_analysis_proc(ModuleInfo, ExecPathTable, PredId, ProcId,
        !LVBeforeTable, !LVAfterTable, !VoidVarTable) :-
    PPId = proc(PredId, ProcId),
    ( some_are_special_preds([PPId], ModuleInfo) ->
        true
    ;
        module_info_proc_info(ModuleInfo, PPId, ProcInfo),
        find_input_output_args(ModuleInfo, ProcInfo, Inputs, Outputs),
        map.lookup(ExecPathTable, PPId, ExecPaths),
        live_variable_analysis_exec_paths(ExecPaths, Inputs, Outputs,
            ModuleInfo, ProcInfo, map.init, ProcLVBefore,
            map.init, ProcLVAfter, map.init, ProcVoidVar),

        map.set(PPId, ProcLVBefore, !LVBeforeTable),
        map.set(PPId, ProcLVAfter, !LVAfterTable),
        map.set(PPId, ProcVoidVar, !VoidVarTable)
    ).

:- pred live_variable_analysis_exec_paths(list(execution_path)::in,
    list(prog_var)::in, list(prog_var)::in, module_info::in, proc_info::in,
    pp_varset_table::in, pp_varset_table::out, pp_varset_table::in,
    pp_varset_table::out, pp_varset_table::in, pp_varset_table::out) is det.

    % Live variable analysis is backward, so we reverse the execution path
    % before starting. We have specific treatment for execution paths with
    % only one program point, which means the last program point is also the
    % first one.
    %
live_variable_analysis_exec_paths([], _, _, _, _, !ProcLVBefore,
        !ProcLVAfter, !ProcVoidVar).
live_variable_analysis_exec_paths([ExecPath0 | ExecPaths], Inputs, Outputs,
        ModuleInfo, ProcInfo, !ProcLVBefore, !ProcLVAfter, !ProcVoidVar) :-
    list.reverse(ExecPath0, ExecPath),
    ( list.length(ExecPath) = 1 ->
        live_variable_analysis_singleton_exec_path(ExecPath, Inputs, Outputs,
            ModuleInfo, ProcInfo, !ProcLVBefore, !ProcLVAfter, !ProcVoidVar)
    ;
        % Start with the last program point.
        live_variable_analysis_exec_path(ExecPath, Inputs, Outputs,
            ModuleInfo, ProcInfo, yes, set.init, !ProcLVBefore, !ProcLVAfter,
            !ProcVoidVar)
    ),
    live_variable_analysis_exec_paths(ExecPaths, Inputs, Outputs,
        ModuleInfo, ProcInfo, !ProcLVBefore, !ProcLVAfter, !ProcVoidVar).

:- pred live_variable_analysis_exec_path(execution_path::in,
    list(prog_var)::in, list(prog_var)::in, module_info::in, proc_info::in,
    bool::in, set(prog_var)::in, pp_varset_table::in, pp_varset_table::out,
    pp_varset_table::in, pp_varset_table::out,
    pp_varset_table::in, pp_varset_table::out) is det.

live_variable_analysis_exec_path([], _, _, _, _,_, _, !ProcLVBefore,
        !ProcLVAfter, !ProcVoidVar).
    % XXX Exactly what piece of code does this comment apply to?
    % Process the last program point in an execution path. The live variable
    % set of the last program point is always the set of output variables
    % of the procedure.
    %
live_variable_analysis_exec_path([(LastProgPoint - Goal) | ProgPointGoals],
        Inputs, Outputs, ModuleInfo, ProcInfo, yes, _LVBeforeNext,
        !ProcLVBefore, !ProcLVAfter, !ProcVoidVar) :-
    ( map.search(!.ProcLVAfter, LastProgPoint, LVAfterLast0) ->
        LVAfterLast = LVAfterLast0
    ;
        LVAfterLast = set.list_to_set(Outputs),
        map.set(LastProgPoint, LVAfterLast, !ProcLVAfter)
    ),

    % Compute live variable before this last program point.
    compute_useds_produceds(ModuleInfo, Goal, UsedSet, ProducedSet),
    set.union(set.difference(LVAfterLast, ProducedSet), UsedSet,
        LVBeforeLastInThisExecPath),
    record_live_vars_at_prog_point(LastProgPoint, LVBeforeLastInThisExecPath,
        !ProcLVBefore),

    % Collect void variables after this program point.
    collect_void_vars(LastProgPoint, ProducedSet, ProcInfo, !ProcVoidVar),

    live_variable_analysis_exec_path(ProgPointGoals, Inputs, Outputs,
        ModuleInfo, ProcInfo, no, LVBeforeLastInThisExecPath, !ProcLVBefore,
        !ProcLVAfter, !ProcVoidVar).

    % Process a middle program point.
    %
live_variable_analysis_exec_path(
        [(ProgPoint - Goal), ProgPointGoal | ProgPointGoals], Inputs,
        Outputs, ModuleInfo, ProcInfo, no, LVBeforeNext, !ProcLVBefore,
        !ProcLVAfter, !ProcVoidVar) :-
    % The live variable set after this program point is the union of the
    % live variable sets after it in all execution paths to which it belongs.
    record_live_vars_at_prog_point(ProgPoint, LVBeforeNext, !ProcLVAfter),

    % Compute LV before this program point.
    compute_useds_produceds(ModuleInfo, Goal, UsedSet, ProducedSet),
    set.union(set.difference(LVBeforeNext, ProducedSet), UsedSet,
        LVBeforeInThisExecPath),
    record_live_vars_at_prog_point(ProgPoint, LVBeforeInThisExecPath,
        !ProcLVBefore),

    % Collect void variables after this program point.
    collect_void_vars(ProgPoint, ProducedSet, ProcInfo, !ProcVoidVar),

    live_variable_analysis_exec_path([ProgPointGoal | ProgPointGoals],
        Inputs, Outputs, ModuleInfo, ProcInfo, no, LVBeforeInThisExecPath,
        !ProcLVBefore, !ProcLVAfter, !ProcVoidVar).

    % The live variable set before the first program point is ALWAYS
    % Inputs.
    %
live_variable_analysis_exec_path([FirstProgPoint - Goal], Inputs, _Outputs,
        ModuleInfo, ProcInfo, no, LVBeforeNext, !ProcLVBefore,
        !ProcLVAfter, !ProcVoidVar) :-
    ( map.search(!.ProcLVBefore, FirstProgPoint, _LVBeforeFirst) ->
        true
    ;
        LVBeforeFirst = set.list_to_set(Inputs),
        map.set(FirstProgPoint, LVBeforeFirst, !ProcLVBefore)
    ),

    % Live variable set after the first program point.
    record_live_vars_at_prog_point(FirstProgPoint, LVBeforeNext, !ProcLVAfter),

    % Collect void vars after this program point.
    compute_useds_produceds(ModuleInfo, Goal, _UsedSet, ProducedSet),
    collect_void_vars(FirstProgPoint, ProducedSet, ProcInfo, !ProcVoidVar).

    % This predicate analyses execution paths with only one program point.
    % So it must be called in a context that matches that condition.
    %
:- pred live_variable_analysis_singleton_exec_path(execution_path::in,
    list(prog_var)::in, list(prog_var)::in, module_info::in, proc_info::in,
    pp_varset_table::in, pp_varset_table::out, pp_varset_table::in,
    pp_varset_table::out, pp_varset_table::in, pp_varset_table::out) is det.

live_variable_analysis_singleton_exec_path([ProgPoint - Goal | _], Inputs,
        Outputs, ModuleInfo, ProcInfo, !ProcLVBefore, !ProcLVAfter,
        !ProcVoidVar) :-
    LVBefore = set.list_to_set(Inputs),
    map.set(ProgPoint, LVBefore, !ProcLVBefore),
    LVAfter = set.list_to_set(Outputs),
    map.set(ProgPoint, LVAfter, !ProcLVAfter),

    % Collect void vars after this program point.
    compute_useds_produceds(ModuleInfo, Goal, _UsedSet, ProducedSet),
    collect_void_vars(ProgPoint, ProducedSet, ProcInfo, !ProcVoidVar).
live_variable_analysis_singleton_exec_path([], _, _, _, _,
        !ProcLVBefore, !ProcLVAfter, !ProcVoidVar) :-
    unexpected($pred, "empty list").

    % A variable is live at a program point if it is live in one of
    % the execution paths that covers the program point.
    % Therefore we need to union the existing live variable set at a program
    % point with the newly found.
    %
:- pred record_live_vars_at_prog_point(program_point::in, variable_set::in,
    pp_varset_table::in, pp_varset_table::out) is det.

record_live_vars_at_prog_point(ProgPoint, LV, !ProcLV) :-
    ( map.search(!.ProcLV, ProgPoint, ExistingLV) ->
        map.set(ProgPoint, set.union(ExistingLV, LV), !ProcLV)
    ;
        map.set(ProgPoint, LV, !ProcLV)
    ).

    % Compute used and produced variables in an atomic goal, which
    % has been recorded alongside a program point in an execution_path.
    % A variable is used in an atomic goal if it is input to the goal.
    % It is produced in the atomic goal if it is output of the goal.
    %
:- pred compute_useds_produceds(module_info::in, hlds_goal::in,
    variable_set::out, variable_set::out) is det.

compute_useds_produceds(ModuleInfo, Goal, UsedSet, ProducedSet) :-
    ( if
        % a removed switch
        Goal = hlds_goal(switch(Var, _, _), _SwitchInfo)
      then
        Useds = [Var],
        Produceds = []
      else
        Goal = hlds_goal(Expr, _Info),
        (
            Expr = plain_call(CalleePredId, CalleeProcId, Args,
                _BuiltIn, _Context, _Name)
        ->
            get_inputs_outputs_proc_call(Args,
                proc(CalleePredId, CalleeProcId), ModuleInfo,
                Useds, Produceds)
        ;
            Expr = unify(_, _, _, Unification, _)
        ->
            get_inputs_outputs_unification(Unification, Useds,
                Produceds)
        ;
            ( Expr = conj(_, [])
            ; Expr = disj([])
            )
        ->
            Useds = [],
            Produceds = []
        ;
            unexpected($pred,
                "the expression must be either call, unify, true, or fail")
        )
    ),
    set.list_to_set(Useds, UsedSet),
    set.list_to_set(Produceds, ProducedSet).

    % Divide the variables appearing in a unification into lists of input
    % variables and output variables.
    %
:- pred get_inputs_outputs_unification(unification::in,
    list(prog_var)::out, list(prog_var)::out) is det.

get_inputs_outputs_unification(construct(LVar, _, Args, _, _, _, _),
        Args, [LVar]).
get_inputs_outputs_unification(deconstruct(LVar, _, Args, _, _, _),
        [LVar], Args).
get_inputs_outputs_unification(assign(LVar, RVar), [RVar], [LVar]).
get_inputs_outputs_unification(simple_test(LVar, RVar), [LVar, RVar], []).
get_inputs_outputs_unification(complicated_unify(_, _, _), [], []).

    % Divide the arguments in a procedure call into lists of input
    % variables and output variables.
    %
:- pred get_inputs_outputs_proc_call(list(prog_var)::in, pred_proc_id::in,
    module_info::in, list(prog_var)::out, list(prog_var)::out) is det.

get_inputs_outputs_proc_call(ActualArgs, CalleeId, ModuleInfo,
        ActualInputs, ActualOutputs) :-
    module_info_pred_proc_info(ModuleInfo, CalleeId, _PredInfo, CalleeInfo),
    find_input_output_args(ModuleInfo, CalleeInfo, Inputs, Outputs),

    proc_info_get_headvars(CalleeInfo, FormalArgs),
    get_inputs_outputs_proc_call_2(FormalArgs, ActualArgs,
        Inputs, Outputs, [], ActualInputs, [], ActualOutputs).

:- pred get_inputs_outputs_proc_call_2(list(prog_var)::in,
    list(prog_var)::in, list(prog_var)::in, list(prog_var)::in,
    list(prog_var)::in, list(prog_var)::out, list(prog_var)::in,
    list(prog_var)::out) is det.

get_inputs_outputs_proc_call_2([], [], _, _, !ActualInputs, !ActualOutputs).
get_inputs_outputs_proc_call_2([], [_ | _], _, _, !ActualInputs,
        !ActualOutputs) :-
    unexpected($pred, "mismatched lists").
get_inputs_outputs_proc_call_2([_ | _], [], _, _, !ActualInputs,
        !ActualOutputs) :-
    unexpected($pred, "mismatched lists").
get_inputs_outputs_proc_call_2([FormalArg | FormalArgs],
        [ActualArg | ActualArgs], Inputs, Outputs, !ActualInputs,
        !ActualOutputs) :-
    ( list.member(FormalArg, Inputs) ->
        % This formal argument is an input, so the correspondig argument
        % is an actual input argument.
        ActualInputs1 = [ActualArg | !.ActualInputs],
        ActualOutputs1 = !.ActualOutputs
    ; list.member(FormalArg, Outputs) ->
        % This formal argument is an output, so the corresponding argument
        % is an actual output argument.
        ActualOutputs1 = [ActualArg | !.ActualOutputs],
        ActualInputs1 = !.ActualInputs
    ;
        % This formal param is neither an output nor an input, so ignore
        % the corresponding arg.
        ActualInputs1 = !.ActualInputs,
        ActualOutputs1 = !.ActualOutputs
    ),
    get_inputs_outputs_proc_call_2(FormalArgs, ActualArgs, Inputs, Outputs,
        ActualInputs1, !:ActualInputs, ActualOutputs1, !:ActualOutputs).

    % Collect variables whose names start with _, i.e., void variables.
    % I am considering those variables dead right after created in the live
    % variable and region analyses.
    %
:- pred collect_void_vars(program_point::in, variable_set::in, proc_info::in,
    pp_varset_table::in, pp_varset_table::out) is det.

collect_void_vars(ProgPoint, ProducedSet, ProcInfo, !ProcVoidVar) :-
    ( map.search(!.ProcVoidVar, ProgPoint, _DeadVars) ->
        true
    ;
        proc_info_get_varset(ProcInfo, VarSet),
        set.fold(void_var(VarSet), ProducedSet, set.init, VoidVars),
        map.set(ProgPoint, VoidVars, !ProcVoidVar)
    ).

    % To be used with the fold above: if Var is a void variable,
    % add it to VoidVars set.
    %
:- pred void_var(prog_varset::in, prog_var::in,
    variable_set::in, variable_set::out) is det.

void_var(VarSet, Var, !VoidVars) :-
    VarName = mercury_var_to_name_only(VarSet, Var),
    ( string.index(VarName, 0, '_') ->
        set.insert(Var, !VoidVars)
    ;
        true
    ).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.live_variable_analysis.
%----------------------------------------------------------------------------%
