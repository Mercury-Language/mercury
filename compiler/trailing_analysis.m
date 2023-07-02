%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: trailing_analysis.m.
% Author: juliensf.
%
% This module implements trail usage analysis. It annotates the HLDS with
% information about which procedures will not modify the trail.
%
% The compiler can use this information to omit redundant trailing operations
% in trailing grades. After running the analysis the trailing status of each
% procedure is one of:
%
%   (1) trail_will_not_modify
%   (2) trail_may_modify
%   (3) trail_conditional
%
% These have the following meaning:
%
%   (1) for all inputs the procedure will not modify the trail.
%   (2) for some inputs the procedure may modify the trail.
%   (3) the procedure is polymorphic and whether it may modify the trail
%       depends upon the instantiation of the type variables. We need
%       this because we can define types with user-defined equality or
%       comparison that modify the trail.
%
% NOTE: to be `trail_conditional' a procedure cannot modify the trail itself,
% any trail modifications that occur through the conditional procedure
% must result from a higher-order call or a call to a user-defined equality
% or comparison predicate.
%
% For procedures defined using the foreign language interface we rely upon
% the user annotations `will_not_modify_trail' and `may_not_modify_trail'.
%
% The predicates for determining if individual goals modify the trail
% are in goal_form.m.
%
% TODO:
%
%   - Use the results of closure analysis to determine the trailing
%     status of higher-order calls.
%   - Improve the analysis in the presence of solver types.
%   - Create specialised versions of higher-order procedures based on
%     whether or not their arguments modify the trail.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.trailing_analysis.
:- interface.

:- import_module analysis.
:- import_module hlds.
:- import_module hlds.hlds_module.

%----------------------------------------------------------------------------%

    % Perform trail usage analysis on a module.
    %
:- pred analyse_trail_usage_in_module(module_info::in, module_info::out)
    is det.

    % Types and instances for the intermodule analysis framework.
    %
:- type trailing_analysis_answer.
:- instance analysis(no_func_info, any_call, trailing_analysis_answer).
:- instance partial_order(no_func_info, trailing_analysis_answer).
:- instance answer_pattern(no_func_info, trailing_analysis_answer).
:- instance to_term(trailing_analysis_answer).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.
:- import_module parse_tree.write_error_spec.
:- import_module transform_hlds.intermod_analysis.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_context.

%----------------------------------------------------------------------------%
%
% Perform trail usage analysis on a module.
%

% The analysis is carried out in two passes. Both passes do a bottom-up
% traversal of the callgraph, one SCC at a time. For each SCC the first
% pass works out the trailing_status for each procedure in the SCC.
% The second pass then uses this information to annotate the goals in each
% procedure with trail usage information.
%
% The second pass is only run if we are going to use the information,
% that is if we are generating code as opposed to building the optimization
% interfaces.

analyse_trail_usage_in_module(!ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    (
        % Only run the analysis in trailing grades.
        UseTrail = yes,
        globals.get_op_mode(Globals, OpMode),
        ( if
            OpMode = opm_top_args(opma_augment(OpModeAugment), _),
            ( OpModeAugment = opmau_make_plain_opt
            ; OpModeAugment = opmau_make_trans_opt
            ; OpModeAugment = opmau_make_analysis_registry
            )
        then
            Pass1Only = yes
        else
            Pass1Only = no
        ),
        module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
        SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
        globals.lookup_bool_option(Globals, debug_trail_usage, Debug),
        list.foldl(trail_analyse_scc(Debug, Pass1Only), SCCs, !ModuleInfo),

        module_info_get_proc_analysis_kinds(!.ModuleInfo, ProcAnalysisKinds0),
        set.insert(pak_trailing, ProcAnalysisKinds0, ProcAnalysisKinds),
        module_info_set_proc_analysis_kinds(ProcAnalysisKinds, !ModuleInfo),

        % Record results if making the analysis registry. We do this in
        % a separate pass so that we record results for exported procedures
        % which have a `:- pragma external_{pred/func}', which don't analyse
        % because we don't have clauses for them.
        ( if
            OpMode = opm_top_args(OpModeArgs, _),
            OpModeArgs = opma_augment(opmau_make_analysis_registry)
        then
            module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
            module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
            list.foldl(maybe_record_trailing_result(!.ModuleInfo),
                PredIds, AnalysisInfo0, AnalysisInfo),
            module_info_set_analysis_info(AnalysisInfo, !ModuleInfo)
        else
            true
        )
    ;
        UseTrail = no
    ).

%----------------------------------------------------------------------------%
%
% Perform trail usage analysis on a SCC.
%

:- type trail_proc_result
    --->    trail_proc_result(
                tpr_ppid                        :: pred_proc_id,
                tpr_status                      :: trailing_status,
                tpr_maybe_analysis_status       :: maybe(analysis_status)
            ).

:- pred trail_analyse_scc(bool::in, bool::in, scc::in,
    module_info::in, module_info::out) is det.

trail_analyse_scc(Debug, Pass1Only, SCC, !ModuleInfo) :-
    check_procs_for_trail_mods(SCC, ProcResults, !ModuleInfo),

    % The `Results' above are the results of analysing each individual
    % procedure in the SCC - we now have to combine them in a meaningful way.
    trail_combine_individual_proc_results(ProcResults,
        TrailingStatus, MaybeAnalysisStatus),

    (
        Debug = yes,
        trace [io(!IO)] (
            get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
            dump_trail_usage_debug_info(DebugStream, !.ModuleInfo, SCC,
                TrailingStatus, !IO)
        )
    ;
        Debug = no
    ),

    ProcTrailingInfo = proc_trailing_info(TrailingStatus, MaybeAnalysisStatus),
    set.foldl(set_trailing_info(ProcTrailingInfo), SCC, !ModuleInfo),

    (
        Pass1Only = no,
        set.foldl(trail_annotate_proc, SCC, !ModuleInfo)
    ;
        Pass1Only = yes
    ).

:- pred set_trailing_info(proc_trailing_info::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

set_trailing_info(ProcTrailingInfo, PPId, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    proc_info_set_trailing_info(yes(ProcTrailingInfo), ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo0, ProcInfo, !ModuleInfo).

    % Check each procedure in the SCC individually.
    %
:- pred check_procs_for_trail_mods(scc::in, list(trail_proc_result)::out,
    module_info::in, module_info::out) is det.

check_procs_for_trail_mods(SCC, Result, !ModuleInfo) :-
    set.foldl2(check_proc_for_trail_mods(SCC), SCC, [], Result, !ModuleInfo).

    % Examine how the procedures interact with other procedures that
    % are mutually-recursive to them.
    %
:- pred trail_combine_individual_proc_results(list(trail_proc_result)::in,
    trailing_status::out, maybe(analysis_status)::out) is det.

trail_combine_individual_proc_results([], _, _) :-
    unexpected($pred, "empty SCC").
trail_combine_individual_proc_results(ProcResults @ [_ | _], SCC_Result,
        MaybeAnalysisStatus) :-
    ( if
        % If none of the procedures modifies the trail or is conditional then
        % the SCC cannot modify the trail.
        all [ProcResult] (
            list.member(ProcResult, ProcResults)
        =>
            ProcResult ^ tpr_status = trail_will_not_modify
        )
    then
        SCC_Result = trail_will_not_modify
    else if
        all [EResult] (
            list.member(EResult, ProcResults)
        =>
            EResult ^ tpr_status \= trail_may_modify
        ),
        some [CResult] (
            list.member(CResult, ProcResults),
            CResult ^ tpr_status = trail_conditional
        )
    then
        SCC_Result = trail_conditional
    else
        % Otherwise the SCC may modify the trail.
        SCC_Result = trail_may_modify
    ),
    trail_combine_proc_result_maybe_analysis_statuses(ProcResults,
        MaybeAnalysisStatus).

:- pred trail_combine_proc_result_maybe_analysis_statuses(
    list(trail_proc_result)::in, maybe(analysis_status)::out) is det.

trail_combine_proc_result_maybe_analysis_statuses(ProcResults,
        MaybeAnalysisStatus) :-
    list.map(trail_maybe_analysis_status, ProcResults, MaybeAnalysisStatuses),
    list.foldl(combine_maybe_trail_analysis_status, MaybeAnalysisStatuses,
        yes(optimal), MaybeAnalysisStatus).

:- pred trail_maybe_analysis_status(trail_proc_result::in,
    maybe(analysis_status)::out) is det.

trail_maybe_analysis_status(ProcResult, AnalysisStatus) :-
    AnalysisStatus = ProcResult ^ tpr_maybe_analysis_status.

%----------------------------------------------------------------------------%
%
% Perform trail usage analysis on a procedure.
%

:- pred check_proc_for_trail_mods(scc::in, pred_proc_id::in,
    list(trail_proc_result)::in, list(trail_proc_result)::out,
    module_info::in, module_info::out) is det.

check_proc_for_trail_mods(SCC, PPId, !Results, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_goal(ProcInfo, Body),
    proc_info_get_var_table(ProcInfo, VarTable),
    check_goal_for_trail_mods(SCC, VarTable, Body,
        Result, MaybeAnalysisStatus, !ModuleInfo),
    TrailProcResult = trail_proc_result(PPId, Result, MaybeAnalysisStatus),
    !:Results = [TrailProcResult | !.Results].

%----------------------------------------------------------------------------%
%
% Perform trail usage analysis of a goal.
%

:- pred check_goal_for_trail_mods(scc::in, var_table::in, hlds_goal::in,
    trailing_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

check_goal_for_trail_mods(SCC, VarTable, Goal, Result, MaybeAnalysisStatus,
        !ModuleInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(_, _, _, Kind, _),
        Result = trail_will_not_modify,
        MaybeAnalysisStatus = yes(optimal),
        (
            ( Kind = construct(_, _, _, _, _, _, _)
            ; Kind = deconstruct(_, _, _, _, _, _)
            ; Kind = assign(_, _)
            ; Kind = simple_test(_, _)
            )
        ;
            Kind = complicated_unify(_, _, _),
            unexpected($pred, "complicated unify")
        )
    ;
        GoalExpr = plain_call(CallPredId, CallProcId, CallArgs, _, _, _),
        CallPPId = proc(CallPredId, CallProcId),
        module_info_pred_info(!.ModuleInfo, CallPredId, CallPredInfo),
        ( if
            % Handle (mutually-)recursive calls.
            set.member(CallPPId, SCC)
        then
            lookup_var_types(VarTable, CallArgs, Types),
            TrailingStatus = types_trailing_status(!.ModuleInfo, Types),
            Result = TrailingStatus,
            MaybeAnalysisStatus = yes(optimal)
        else if
            pred_info_is_builtin(CallPredInfo)
        then
            % There are no builtins that will modify the trail.
            Result = trail_will_not_modify,
            MaybeAnalysisStatus = yes(optimal)
        else if
            % Handle builtin unify and compare.
            % NOTE: the type specific unify and compare predicates are just
            % treated as though they were normal predicates.
            ModuleName = pred_info_module(CallPredInfo),
            any_mercury_builtin_module(ModuleName),
            Name = pred_info_name(CallPredInfo),
            pred_info_get_orig_arity(CallPredInfo,
                pred_form_arity(CallPredFormArityInt)),
            ( SpecialPredId = spec_pred_compare
            ; SpecialPredId = spec_pred_unify
            ),
            special_pred_name_arity(SpecialPredId, Name, _,
                CallPredFormArityInt)
        then
            % XXX We should examine the argument types of calls to
            % builtin.unify/2 and builtin.compare/3 and then make a decision
            % based on those.
            Result = trail_may_modify,
            MaybeAnalysisStatus = yes(optimal)
        else if
            % Handle library predicates whose trailing status
            % can be looked up in the known procedures table.
            pred_info_has_known_trail_status(CallPredInfo, Result0)
        then
            Result = Result0,
            MaybeAnalysisStatus = yes(optimal)
        else
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, intermodule_analysis,
                Intermod),
            ( if
                Intermod = yes,
                pred_info_is_imported_not_external(CallPredInfo)
            then
                % With --intermodule-analysis use trail_check_call_2 to look up
                % results for locally defined procedures, otherwise we use
                % the intermodule analysis framework.
                search_trail_analysis_status(CallPPId, Result0, AnalysisStatus,
                    !ModuleInfo),
                (
                    Result0 = trail_conditional,
                    Result = trail_check_vars(!.ModuleInfo, VarTable, CallArgs)
                ;
                    ( Result0 = trail_may_modify
                    ; Result0 = trail_will_not_modify
                    ),
                    Result = Result0
                ),
                MaybeAnalysisStatus = yes(AnalysisStatus)
            else
                trail_check_call_2(!.ModuleInfo, VarTable, CallPPId, CallArgs,
                    MaybeResult),
                (
                    MaybeResult = yes(proc_trailing_info(Result,
                        MaybeAnalysisStatus))
                ;
                    MaybeResult = no,
                    % If we do not have any information about the callee
                    % procedure then assume that it modifies the trail.
                    Result = trail_may_modify,
                    (
                        Intermod = yes,
                        MaybeAnalysisStatus = yes(optimal)
                    ;
                        Intermod = no,
                        MaybeAnalysisStatus = no
                    )
                )
            )
        )
    ;
        GoalExpr = generic_call(Details, _Args, _ArgModes, _, _),
        (
            % XXX Use results of closure analysis to handle this.
            Details = higher_order(_Var, _, _, _),
            Result = trail_may_modify,
            MaybeAnalysisStatus = yes(optimal)
        ;
            % XXX We could do better with class methods.
            Details = class_method(_, _, _, _),
            Result = trail_may_modify,
            MaybeAnalysisStatus = yes(optimal)
        ;
            ( Details = cast(_)
            ; Details = event_call(_)
            ),
            Result = trail_will_not_modify,
            MaybeAnalysisStatus = yes(optimal)
        )
    ;
        GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        Result = attributes_imply_trail_mod(Attributes),
        MaybeAnalysisStatus = yes(optimal)
    ;
        GoalExpr = conj(_ConjType, Goals),
        check_goals_for_trail_mods(SCC, VarTable, Goals,
            Result, MaybeAnalysisStatus, !ModuleInfo)
    ;
        GoalExpr = disj(Goals),
        check_goals_for_trail_mods(SCC, VarTable, Goals,
            _Result0, MaybeAnalysisStatus, !ModuleInfo),
        % XXX Currently we have to put trailing code around disjunctions.
        % If we introduce trail specialisation, it may be possible to omit it.
        Result = trail_may_modify
    ;
        GoalExpr = switch(_, _, Cases),
        CaseGoals = list.map((func(case(_, _, CaseGoal)) = CaseGoal), Cases),
        check_goals_for_trail_mods(SCC, VarTable, CaseGoals,
            Result, MaybeAnalysisStatus, !ModuleInfo)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        check_goals_for_trail_mods(SCC, VarTable, [Cond, Then, Else],
            Result0, MaybeAnalysisStatus, !ModuleInfo),
        ( if
            % If the condition of an if-then-else does not modify the trail
            % and is not model_non then we can omit the trailing ops around
            % the condition.
            %
            % NOTE: any changes here may need to be reflected in the handling
            % of if_then-elses in add_trail_ops.m.

            Result0 = trail_will_not_modify,
            Cond = hlds_goal(_CondGoalExpr, CondGoalInfo),
            goal_info_get_code_model(CondGoalInfo) \= model_non
        then
            Result = trail_will_not_modify
        else
            % If the condition modifies the trail, is model_non or both,
            % then we need to emit trailing ops around the condition. If the
            % if-then-else has status `trail_conditional', then we also need
            % to emit the trail ops because we cannot be sure that calls to
            % builtin.{unify,compare} won't call user-defined equality or
            % comparison predicates that modify the trail.
            %
            % NOTE: Conditional procedures whose status is changed here
            % are candidates for generating specialized versions that omit
            % the trailing code.

            Result = trail_may_modify
        )
    ;
        GoalExpr = negation(SubGoal),
        check_goal_for_trail_mods(SCC, VarTable, SubGoal, Result,
            MaybeAnalysisStatus, !ModuleInfo)
    ;
        GoalExpr = scope(Reason, InnerGoal),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % The construction of ground terms will not modify the trail.
            Result = trail_will_not_modify,
            MaybeAnalysisStatus = yes(optimal)
        else
            OuterGoalInfo = GoalInfo,
            check_goal_for_trail_mods(SCC, VarTable, InnerGoal, Result0,
                MaybeAnalysisStatus, !ModuleInfo),
            InnerGoal = hlds_goal(_, InnerGoalInfo),
            InnerCodeModel = goal_info_get_code_model(InnerGoalInfo),
            OuterCodeModel = goal_info_get_code_model(OuterGoalInfo),

            % `trail_conditional' scope goals (of the type that require extra
            % trailing code) will have their status changed to
            % `trail_may_modify'. See the comment in the code handling
            % if-then-elses above for the reason why.
            Result = scope_implies_trail_mod(InnerCodeModel, OuterCodeModel,
                Result0)
        )
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred check_goals_for_trail_mods(scc::in, var_table::in,
    hlds_goals::in, trailing_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

check_goals_for_trail_mods(SCC, VarTable, Goals,
        Result, MaybeAnalysisStatus, !ModuleInfo) :-
    list.map2_foldl(check_goal_for_trail_mods(SCC, VarTable), Goals,
        Results, MaybeAnalysisStatuses, !ModuleInfo),
    list.foldl(combine_trailing_status, Results, trail_will_not_modify,
        Result),
    list.foldl(combine_maybe_trail_analysis_status, MaybeAnalysisStatuses,
        yes(optimal), MaybeAnalysisStatus).

%----------------------------------------------------------------------------%
%
% Utility procedure for processing goals.
%

:- func attributes_imply_trail_mod(pragma_foreign_proc_attributes) =
    trailing_status.

attributes_imply_trail_mod(Attributes) =
    ( if get_may_modify_trail(Attributes) = proc_may_modify_trail then
        trail_may_modify
    else
        trail_will_not_modify
    ).

:- func scope_implies_trail_mod(code_model, code_model, trailing_status)
    = trailing_status.

scope_implies_trail_mod(InnerCodeModel, OuterCodeModel, InnerStatus) =
    ( if
        % If we're at a commit for a goal that might modify the trail
        % then we need to emit some trailing code around the scope goal.
        InnerCodeModel = model_non,
        OuterCodeModel \= model_non
    then
        trail_may_modify
    else
        InnerStatus
    ).

%----------------------------------------------------------------------------%
%
% "Known" library procedures.
%

    % Succeeds if the given pred_info is for a predicate or function
    % whose trailing status can be looked up in the known procedures table.
    % Returns the trailing status corresponding to that procedure.
    % Fails if there was no corresponding entry in the table.
    %
:- pred pred_info_has_known_trail_status(pred_info::in, trailing_status::out)
    is semidet.

pred_info_has_known_trail_status(PredInfo, Status) :-
    Name = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    ModuleName = unqualified(ModuleNameStr),
    pred_info_get_orig_arity(PredInfo, pred_form_arity(Arity)),
    trail_known_procedure(PredOrFunc, ModuleNameStr, Name, Arity, Status).

    % known_procedure/4 is a table of library predicates whose trailing
    % status is hardcoded into the analyser. For a few predicates this
    % information can make a big difference (particularly in the absence
    % of any form of intermodule analysis).
    %
:- pred trail_known_procedure(pred_or_func::in, string::in,
    string::in, int::in, trailing_status::out) is semidet.

trail_known_procedure(pf_predicate, "require", "error", 1,
    trail_will_not_modify).
trail_known_procedure(pf_function,  "require", "func_error", 1,
    trail_will_not_modify).
trail_known_procedure(_, "exception", "throw", 1, trail_will_not_modify).
trail_known_procedure(_, "exception", "rethrow", 1, trail_will_not_modify).

%----------------------------------------------------------------------------%
%
% Code to handle higher-order variables.
%

    % Extract those procedures whose trailing_status has been set to
    % `conditional'. Fails if one of the procedures in the set
    % is known to modify the trail or if the trailing status is not
    % yet been set for one or more of the procedures.
    %
    % XXX The latter case probably shouldn't happen but may at the
    % moment because the construction of the dependency graph doesn't
    % take higher-order calls into account.
    %
:- pred trail_get_conditional_closures(module_info::in, set(pred_proc_id)::in,
    list(pred_proc_id)::out) is semidet.
:- pragma consider_used(pred(trail_get_conditional_closures/3)).

trail_get_conditional_closures(ModuleInfo, Closures, Conditionals) :-
    set.fold(trail_get_conditional_closure(ModuleInfo), Closures,
        [], Conditionals).

:- pred trail_get_conditional_closure(module_info::in, pred_proc_id::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is semidet.

trail_get_conditional_closure(ModuleInfo, PPId, !Conditionals) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _PredInfo, ProcInfo),
    proc_info_get_trailing_info(ProcInfo, MaybeProcTrailingInfo),
    MaybeProcTrailingInfo = yes(ProcTrailingInfo),
    ProcTrailingInfo = proc_trailing_info(Status, _),
    (
        Status = trail_conditional,
        list.cons(PPId, !Conditionals)
    ;
        Status = trail_will_not_modify
    ).

%----------------------------------------------------------------------------%

:- pred combine_trailing_status(trailing_status::in, trailing_status::in,
    trailing_status::out) is det.

combine_trailing_status(trail_will_not_modify, Status, Status).
combine_trailing_status(trail_may_modify, _, trail_may_modify).
combine_trailing_status(trail_conditional, trail_will_not_modify,
    trail_conditional).
combine_trailing_status(trail_conditional, trail_conditional,
    trail_conditional).
combine_trailing_status(trail_conditional, trail_may_modify, trail_may_modify).

:- pred combine_maybe_trail_analysis_status(maybe(analysis_status)::in,
    maybe(analysis_status)::in, maybe(analysis_status)::out) is det.

combine_maybe_trail_analysis_status(MaybeStatusA, MaybeStatusB, MaybeStatus) :-
    ( if
        MaybeStatusA = yes(StatusA),
        MaybeStatusB = yes(StatusB)
    then
        MaybeStatus = yes(analysis.lub(StatusA, StatusB))
    else
        MaybeStatus = no
    ).

%----------------------------------------------------------------------------%
%
% Extra procedures for handling calls.
%

    % Check the trailing status of a call.
    %
:- pred trail_check_call(module_info::in, var_table::in,
    pred_proc_id::in, prog_vars::in, trailing_status::out) is det.

trail_check_call(ModuleInfo, VarTable, PPId, Args, Result) :-
    trail_check_call_2(ModuleInfo, VarTable, PPId, Args, MaybeResult),
    (
        MaybeResult = yes(proc_trailing_info(Result, _))
    ;
        MaybeResult = no,
        % If we do not have any information about the callee procedure,
        % then we have to assume that it may modify the trail.
        Result = trail_may_modify
    ).

:- pred trail_check_call_2(module_info::in, var_table::in,
    pred_proc_id::in, prog_vars::in, maybe(proc_trailing_info)::out) is det.

trail_check_call_2(ModuleInfo, VarTable, PPId, Args, MaybeResult) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _PredInfo, ProcInfo),
    proc_info_get_trailing_info(ProcInfo, MaybeCalleeTrailingInfo),
    (
        MaybeCalleeTrailingInfo = yes(CalleeTrailingInfo),
        CalleeTrailingInfo = proc_trailing_info(CalleeTrailingStatus,
            AnalysisStatus),
        (
            CalleeTrailingStatus = trail_will_not_modify,
            MaybeResult = yes(CalleeTrailingInfo)
        ;
            CalleeTrailingStatus = trail_may_modify,
            MaybeResult = yes(CalleeTrailingInfo)
        ;
            CalleeTrailingStatus = trail_conditional,
            % This is a call to a polymorphic procedure. We need to make sure
            % that none of the types involved has a user-defined equality
            % or comparison predicate that modifies the trail.
            % XXX Need to handle higher-order args here as well.
            MaybeResult = yes(proc_trailing_info(TrailingStatus,
                AnalysisStatus)),
            TrailingStatus = trail_check_vars(ModuleInfo, VarTable, Args)
        )
    ;
        MaybeCalleeTrailingInfo = no,
        MaybeResult = no
    ).

:- func trail_check_vars(module_info, var_table, prog_vars) = trailing_status.

trail_check_vars(ModuleInfo, VarTable, Vars) = Result :-
    lookup_var_types(VarTable, Vars, Types),
    Result = types_trailing_status(ModuleInfo, Types).

%----------------------------------------------------------------------------%
%
% Stuff for processing types.
%

% This is used in the analysis of calls to polymorphic procedures.
%
% By saying that a "type may modify the trail" we mean that tail modification
% may occur as a result of a unification or comparison involving the type
% because it has a user-defined equality/comparison predicate that modifies
% the trail.
%
% XXX We don't actually need to examine all the types, just those
% that are potentially going to be involved in unification/comparisons.
% (The exception and termination analyses have the same problem.)
%
% At the moment we don't keep track of that information so the current
% procedure is as follows:
%
% Examine the functor and then recursively examine the arguments.
%
% * If everything will not trail_will_not_modify then the type will not
%   modify the trail.
%
% * If at least one of the types may modify the trail then the type will
%   will modify the trail.
%
% * If at least one of the types is conditional and none of them modify
%   the trail then the type is conditional.

    % Return the collective trailing status of a list of types.
    %
:- func types_trailing_status(module_info, list(mer_type)) = trailing_status.

types_trailing_status(ModuleInfo, Types) = Status :-
    list.foldl(acc_type_trailing_status(ModuleInfo), Types,
        trail_will_not_modify, Status).

:- pred acc_type_trailing_status(module_info::in, mer_type::in,
    trailing_status::in, trailing_status::out) is det.

acc_type_trailing_status(ModuleInfo, Type, !Status) :-
    combine_trailing_status(type_trailing_status(ModuleInfo, Type), !Status).

    % Return the trailing status of an individual type.
    %
:- func type_trailing_status(module_info, mer_type) = trailing_status.

type_trailing_status(ModuleInfo, Type) = Status :-
    ( if
        ( type_is_solver_type(ModuleInfo, Type)
        ; type_is_existq_type(ModuleInfo, Type)
        )
     then
        % XXX At the moment we just assume that existential
        % types and solver types may modify the trail.
        Status = trail_may_modify
    else
        TypeCtorCategory = classify_type(ModuleInfo, Type),
        Status = type_and_cat_trailing_status(ModuleInfo, Type,
            TypeCtorCategory)
    ).

:- func type_and_cat_trailing_status(module_info, mer_type, type_ctor_category)
    = trailing_status.

type_and_cat_trailing_status(ModuleInfo, Type, TypeCtorCat) = Status :-
    (
        ( TypeCtorCat = ctor_cat_builtin(_)
        ; TypeCtorCat = ctor_cat_higher_order
        ; TypeCtorCat = ctor_cat_system(_)
        ; TypeCtorCat = ctor_cat_void
        ; TypeCtorCat = ctor_cat_builtin_dummy
        ; TypeCtorCat = ctor_cat_user(cat_user_direct_dummy)
        ; TypeCtorCat = ctor_cat_user(cat_user_abstract_dummy)
        ),
        Status = trail_will_not_modify
    ;
        TypeCtorCat = ctor_cat_variable,
        Status = trail_conditional
    ;
        ( TypeCtorCat = ctor_cat_tuple
        ; TypeCtorCat = ctor_cat_enum(_)
        ; TypeCtorCat = ctor_cat_user(cat_user_notag)
        ; TypeCtorCat = ctor_cat_user(cat_user_abstract_notag)
        ; TypeCtorCat = ctor_cat_user(cat_user_general)
        ),
        type_to_ctor_and_args_det(Type, _TypeCtor, Args),
        ( if
            type_has_user_defined_equality_pred(ModuleInfo, Type,
                _UnifyCompare)
        then
            % XXX We can do better than this by examining what these preds
            % actually do. Something similar needs to be sorted out for
            % termination analysis as well, so we'll wait until that is done.
            Status = trail_may_modify
        else
            Status = types_trailing_status(ModuleInfo, Args)
        )
    ).

%----------------------------------------------------------------------------%
%
% Code for attaching trail usage information to goals.
%

    % Traverse the body of the procedure and attach will_not_modify trail
    % features to the goal_infos of those procedure that cannot modify the
    % trail.
    %
:- pred trail_annotate_proc(pred_proc_id::in,
    module_info::in, module_info::out) is det.

trail_annotate_proc(PPId, !ModuleInfo) :-
    some [!ProcInfo, !Body] (
      module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, !:ProcInfo),
      proc_info_get_goal(!.ProcInfo, !:Body),
      proc_info_get_var_table(!.ProcInfo, VarTable),
      trail_annotate_goal(VarTable, !Body, _Status, !ModuleInfo),
      proc_info_set_goal(!.Body, !ProcInfo),
      module_info_set_pred_proc_info(PPId, PredInfo, !.ProcInfo, !ModuleInfo)
    ).

:- pred trail_annotate_goal(var_table::in, hlds_goal::in, hlds_goal::out,
    trailing_status::out, module_info::in, module_info::out) is det.

trail_annotate_goal(VarTable, !Goal, Status, !ModuleInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    trail_annotate_goal_2(VarTable, GoalInfo0, GoalExpr0, GoalExpr, Status,
        !ModuleInfo),
    (
        Status = trail_will_not_modify,
        goal_info_add_feature(feature_will_not_modify_trail,
            GoalInfo0, GoalInfo)
    ;
        ( Status = trail_may_modify
        ; Status = trail_conditional
        ),
        GoalInfo = GoalInfo0
    ),
    !:Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred trail_annotate_goal_2(var_table::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out, trailing_status::out,
    module_info::in, module_info::out) is det.

trail_annotate_goal_2(VarTable, GoalInfo, !GoalExpr, Status, !ModuleInfo) :-
    (
        !.GoalExpr = unify(_, _, _, Kind, _),
        (
            ( Kind = construct(_, _, _, _, _, _, _)
            ; Kind = deconstruct(_, _, _, _, _, _)
            ; Kind = assign(_, _)
            ; Kind = simple_test(_, _)
            )
        ;
            Kind = complicated_unify(_, _, _),
            unexpected($pred, "complicated unify")
        ),
        Status = trail_will_not_modify
    ;
        !.GoalExpr = plain_call(CallPredId, CallProcId, CallArgs, _, _, _),
        CallPPId = proc(CallPredId, CallProcId),
        module_info_pred_info(!.ModuleInfo, CallPredId, CallPredInfo),
        ( if
            pred_info_is_builtin(CallPredInfo)
        then
            Status = trail_will_not_modify
        else if
            % Handle builtin unify and compare.
            ModuleName = pred_info_module(CallPredInfo),
            any_mercury_builtin_module(ModuleName),
            Name = pred_info_name(CallPredInfo),
            pred_info_get_orig_arity(CallPredInfo,
                pred_form_arity(CallPredFormArityInt)),
            ( SpecialPredId = spec_pred_compare
            ; SpecialPredId = spec_pred_unify
            ),
            special_pred_name_arity(SpecialPredId, Name, _,
                CallPredFormArityInt)
        then
            Status = trail_may_modify
        else if
            % Handle library predicates whose trailing status
            % can be looked up in the known procedure table.
            pred_info_has_known_trail_status(CallPredInfo, Status0)
        then
            Status = Status0
        else
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, intermodule_analysis,
                IntermodAnalysis),
            ( if
                IntermodAnalysis = yes,
                pred_info_is_imported(CallPredInfo)
            then
                search_trail_analysis_status(CallPPId, Result, AnalysisStatus,
                    !ModuleInfo),

                % XXX We shouldn't be getting invalid analysis results at this
                % stage so maybe we should just call unexpected/2 here?
                (
                    AnalysisStatus = invalid,
                    Status = trail_may_modify
                ;
                    ( AnalysisStatus = suboptimal
                    ; AnalysisStatus = optimal
                    ),
                    (
                        Result = trail_conditional,
                        Status = trail_check_vars(!.ModuleInfo, VarTable,
                            CallArgs)
                    ;
                        ( Result = trail_may_modify
                        ; Result = trail_will_not_modify
                        ),
                        Status = Result
                    )
                )
            else
                % This time around we will be checking recursive calls as well.
                trail_check_call(!.ModuleInfo, VarTable, CallPPId, CallArgs,
                    Status)
            )
        )
    ;
        !.GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        Status = attributes_imply_trail_mod(Attributes)
    ;
        % XXX We should use any results from closure analysis here.
        !.GoalExpr = generic_call(GenericCall, _, _, _, _),
        (
            GenericCall = higher_order(_, _, _, _),
            Status = trail_may_modify
        ;
            GenericCall = class_method(_, _, _, _),
            Status = trail_may_modify
        ;
            GenericCall = event_call(_),
            Status = trail_will_not_modify
        ;
            GenericCall = cast(_),
            Status = trail_will_not_modify
        )
    ;
        !.GoalExpr = conj(ConjType, Conjuncts0),
        trail_annotate_goal_list(VarTable, Conjuncts0, Conjuncts, Status,
            !ModuleInfo),
        !:GoalExpr = conj(ConjType, Conjuncts)
    ;
        !.GoalExpr = disj(Disjuncts0),
        trail_annotate_goal_list(VarTable, Disjuncts0, Disjuncts, Status,
            !ModuleInfo),
        !:GoalExpr = disj(Disjuncts)
    ;
        !.GoalExpr = switch(Var, CanFail, Cases0),
        trail_annotate_cases(VarTable, Cases0, Cases, Status, !ModuleInfo),
        !:GoalExpr = switch(Var, CanFail, Cases)
    ;
        !.GoalExpr = if_then_else(Vars, If0, Then0, Else0),
        trail_annotate_goal(VarTable, If0, If, IfStatus, !ModuleInfo),
        trail_annotate_goal(VarTable, Then0, Then, ThenStatus, !ModuleInfo),
        trail_annotate_goal(VarTable, Else0, Else, ElseStatus, !ModuleInfo),
        ( if
            IfStatus   = trail_will_not_modify,
            ThenStatus = trail_will_not_modify,
            ElseStatus = trail_will_not_modify
        then
            Status = trail_will_not_modify
        else
            Status = trail_may_modify
        ),
        !:GoalExpr = if_then_else(Vars, If, Then, Else)
    ;
        !.GoalExpr = negation(SubGoal0),
        trail_annotate_goal(VarTable, SubGoal0, SubGoal, Status, !ModuleInfo),
        !:GoalExpr = negation(SubGoal)
    ;
        !.GoalExpr = scope(Reason, InnerGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            Status = trail_will_not_modify
        else
            OuterGoalInfo = GoalInfo,
            trail_annotate_goal(VarTable, InnerGoal0, InnerGoal, Status0,
                !ModuleInfo),
            InnerGoal = hlds_goal(_, InnerGoalInfo),
            InnerCodeModel = goal_info_get_code_model(InnerGoalInfo),
            OuterCodeModel = goal_info_get_code_model(OuterGoalInfo),
            Status = scope_implies_trail_mod(InnerCodeModel, OuterCodeModel,
                Status0),
            !:GoalExpr = scope(Reason, InnerGoal)
        )
    ;
        !.GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred trail_annotate_goal_list(var_table::in,
    list(hlds_goal)::in, list(hlds_goal)::out, trailing_status::out,
    module_info::in, module_info::out) is det.

trail_annotate_goal_list(VarTable, !Goals, Status, !ModuleInfo) :-
    list.map2_foldl(trail_annotate_goal(VarTable), !Goals, Statuses,
        !ModuleInfo),
    list.foldl(combine_trailing_status, Statuses, trail_will_not_modify,
        Status).

:- pred trail_annotate_cases(var_table::in, list(case)::in, list(case)::out,
    trailing_status::out, module_info::in, module_info::out) is det.

trail_annotate_cases(VarTable, !Cases, Status, !ModuleInfo) :-
    list.map2_foldl(trail_annotate_case(VarTable), !Cases, Statuses,
        !ModuleInfo),
    list.foldl(combine_trailing_status, Statuses, trail_will_not_modify,
        Status).

:- pred trail_annotate_case(var_table::in, case::in, case::out,
    trailing_status::out, module_info::in, module_info::out) is det.

trail_annotate_case(VarTable, !Case, Status, !ModuleInfo) :-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    trail_annotate_goal(VarTable, Goal0, Goal, Status, !ModuleInfo),
    !:Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%
%
% Stuff for the intermodule analysis framework.
%

:- type trailing_analysis_answer
    --->    trailing_analysis_answer(trailing_status).

:- func trail_analysis_name = string.

trail_analysis_name = "trail_usage".

:- instance analysis(no_func_info, any_call, trailing_analysis_answer) where [
    analysis_name(_, _) = trail_analysis_name,
    analysis_version_number(_, _) = 1,
    preferred_fixpoint_type(_, _) = least_fixpoint,
    bottom(_, _) = trailing_analysis_answer(trail_will_not_modify),
    top(_, _) = trailing_analysis_answer(trail_may_modify),
    get_func_info(_, _, _, _, _, no_func_info)
].

:- instance answer_pattern(no_func_info, trailing_analysis_answer) where [].
:- instance partial_order(no_func_info, trailing_analysis_answer) where [
    ( more_precise_than(no_func_info, Answer1, Answer2) :-
        Answer1 = trailing_analysis_answer(Status1),
        Answer2 = trailing_analysis_answer(Status2),
        trailing_status_more_precise_than(Status1, Status2)
    ),
    equivalent(no_func_info, Status, Status)
].

:- pred trailing_status_more_precise_than(trailing_status::in,
        trailing_status::in) is semidet.

trailing_status_more_precise_than(trail_will_not_modify, trail_may_modify).
trailing_status_more_precise_than(trail_will_not_modify, trail_conditional).
trailing_status_more_precise_than(trail_conditional, trail_may_modify).

:- instance to_term(trailing_analysis_answer) where [
    func(to_term/1) is trailing_analysis_answer_to_term,
    pred(from_term/2) is trailing_analysis_answer_from_term
].

:- func trailing_analysis_answer_to_term(trailing_analysis_answer) = term.

trailing_analysis_answer_to_term(trailing_analysis_answer(Status)) = Term :-
    trailing_status_to_string(Status, String),
    Term = term.functor(atom(String), [], dummy_context).

:- pred trailing_analysis_answer_from_term(term::in,
    trailing_analysis_answer::out) is semidet.

trailing_analysis_answer_from_term(Term, trailing_analysis_answer(Status)) :-
    Term = term.functor(atom(String), [], _),
    trailing_status_to_string(Status, String).

:- pred trailing_status_to_string(trailing_status, string).
:- mode trailing_status_to_string(in, out) is det.
:- mode trailing_status_to_string(out, in) is semidet.

trailing_status_to_string(trail_may_modify, "may_modify_trail").
trailing_status_to_string(trail_will_not_modify, "will_not_modify_trail").
trailing_status_to_string(trail_conditional, "conditional").

:- pred search_trail_analysis_status(pred_proc_id::in, trailing_status::out,
    analysis_status::out, module_info::in, module_info::out) is det.

search_trail_analysis_status(PPId, Result, AnalysisStatus, !ModuleInfo) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    search_trail_analysis_status_2(!.ModuleInfo, PPId, Result, AnalysisStatus,
        AnalysisInfo0, AnalysisInfo),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred search_trail_analysis_status_2(module_info::in, pred_proc_id::in,
    trailing_status::out, analysis_status::out,
    analysis_info::in, analysis_info::out) is det.

search_trail_analysis_status_2(ModuleInfo, PPId, Result, AnalysisStatus,
        !AnalysisInfo) :-
    mmc_analysis.module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
    Call = any_call,
    analysis.lookup_best_result(!.AnalysisInfo, ModuleName, FuncId,
        no_func_info, Call, MaybeBestStatus),
    (
        MaybeBestStatus = yes(analysis_result(BestCall,
            trailing_analysis_answer(Result), AnalysisStatus)),
        record_dependency(ModuleName, FuncId, no_func_info, BestCall,
            _ : trailing_analysis_answer, !AnalysisInfo)
    ;
        MaybeBestStatus = no,
        % If we do not have any information about the callee procedure
        % then assume that it modifies the trail.
        top(no_func_info, Call) = Answer,
        Answer = trailing_analysis_answer(Result),
        AnalysisStatus = optimal,
        record_request(trail_analysis_name, ModuleName, FuncId, Call,
            !AnalysisInfo)
    ).

:- pred maybe_record_trailing_result(module_info::in, pred_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_trailing_result(ModuleInfo, PredId, !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_procids(PredInfo),
    list.foldl(maybe_record_trailing_result_2(ModuleInfo, PredId, PredInfo),
        ProcIds, !AnalysisInfo).

:- pred maybe_record_trailing_result_2(module_info::in, pred_id::in,
    pred_info::in, proc_id::in, analysis_info::in, analysis_info::out) is det.

maybe_record_trailing_result_2(ModuleInfo, PredId, PredInfo, ProcId,
        !AnalysisInfo) :-
    should_write_trailing_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_analysis_framework, ShouldWrite),
    (
        ShouldWrite = should_write,
        PPId = proc(PredId, ProcId),
        lookup_proc_trailing_info(ModuleInfo, PPId, Status, ResultStatus),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, any_call,
            trailing_analysis_answer(Status), ResultStatus, !AnalysisInfo)
    ;
        ShouldWrite = should_not_write
    ).

:- pred lookup_proc_trailing_info(module_info::in, pred_proc_id::in,
    trailing_status::out, analysis_status::out) is det.

lookup_proc_trailing_info(ModuleInfo, PPId, Status, ResultStatus) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _PredInfo, ProcInfo),
    proc_info_get_trailing_info(ProcInfo, MaybeProcTrailingInfo),
    (
        MaybeProcTrailingInfo = yes(ProcTrailingInfo),
        ProcTrailingInfo = proc_trailing_info(Status, MaybeResultStatus),
        (
            MaybeResultStatus = yes(ResultStatus)
        ;
            MaybeResultStatus = no,
            unexpected($pred, "no result status")
        )
    ;
        MaybeProcTrailingInfo = no,
        % Probably an exported `:- pragma external_{pred/func}' procedure,
        % which wouldn't have been analysed.
        Status = trail_may_modify,
        ResultStatus = optimal
    ).

%----------------------------------------------------------------------------%
%
% Code for printing out debugging traces.
%

:- pred dump_trail_usage_debug_info(io.text_output_stream::in, module_info::in,
    scc::in, trailing_status::in, io::di, io::uo) is det.

dump_trail_usage_debug_info(Stream, ModuleInfo, SCC, Status, !IO) :-
    io.write_string(Stream, "SCC: ", !IO),
    io.write_line(Stream, Status, !IO),
    output_proc_names(Stream, ModuleInfo, SCC, !IO),
    io.nl(Stream, !IO).

:- pred output_proc_names(io.text_output_stream::in, module_info::in,
    scc::in, io::di, io::uo) is det.

output_proc_names(Stream, ModuleInfo, SCC, !IO) :-
    set.foldl(output_proc_name(Stream, ModuleInfo), SCC, !IO).

:- pred output_proc_name(io.text_output_stream::in, module_info::in,
    pred_proc_id::in, io::di, io::uo) is det.

output_proc_name(Stream, Moduleinfo, PPId, !IO) :-
   Pieces = describe_one_proc_name(Moduleinfo, should_module_qualify, PPId),
   Str = error_pieces_to_string(Pieces),
   io.format(Stream, "\t%s\n", [s(Str)], !IO).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.trailing_analysis.
%----------------------------------------------------------------------------%
