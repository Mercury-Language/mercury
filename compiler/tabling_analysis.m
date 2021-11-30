%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: tabling_analysis.m.
% Author: juliensf.
%
% This module contains an analysis that identifies those goals that cannot
% call minimal model tabled procedures.  We can optimize the code generated
% for such goals by omitting any pneg context wrappers (see ite_gen.m).
% The analysis has two passes.
%
% The first pass marks each procedure in the module as one of:
%
%   * mm_tabled_will_not_call
%   * mm_tabled_may_call
%   * mm_tabled_conditional
%
% Procedures marked `mm_tabled_will_not_call' are guaranteed not to call
% procedures that are minimal model tabled (or use minimal model tabling
% themselves), while procedures marked `mm_tabled_may_call' may call procedures
% that are minimal model tabled (or use it themselves).
%
% Procedures marked `mm_tabled_conditional' will not call minimal model tabled
% procedure directly but may do so indirectly through either a higher-order
% call or via a user-defined equality or comparison predicate.
%
% `mm_tabled_conditional' is a promise that we can determine whether the
% procedure may call minimal model tabled procedures by only examining the
% values of any higher-order arguments and the types that any polymorphic
% arguments become bound to.
%
% For procedures defined using the foreign language interface we rely upon
% the foreign_proc attributes `will_not_call_mm_tabled' and
% `may_call_mm_tabled'.
%
% The second pass of the analysis marks individual goals with a feature
% that indicates that they do not call minimal model tabled procedures.
% This pass is only run when we are generating code.
%
% TODO
% - improve handle higher-order constructs / type class method calls
% - handle user-defined equality and comparison correctly
% - the bits marked `XXX user-defined uc' need to be changed
%
%----------------------------------------------------------------------------%

:- module transform_hlds.tabling_analysis.
:- interface.

:- import_module analysis.
:- import_module hlds.
:- import_module hlds.hlds_module.

%----------------------------------------------------------------------------%

    % Analyse minimal model tabling in a module.
    %
:- pred analyse_mm_tabling_in_module(module_info::in, module_info::out) is det.

%----------------------------------------------------------------------------%
%
% Types and instances for the intermodule analysis framework
%

:- type mm_tabling_analysis_answer.
:- instance analysis(no_func_info, any_call, mm_tabling_analysis_answer).
:- instance partial_order(no_func_info, mm_tabling_analysis_answer).
:- instance answer_pattern(no_func_info, mm_tabling_analysis_answer).
:- instance to_term(mm_tabling_analysis_answer).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module hlds.vartypes.
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
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
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

%----------------------------------------------------------------------------%
%
% Perform minimal model tabling analysis on a module
%

analyse_mm_tabling_in_module(!ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_minimal_model_stack_copy,
        UseMinimalModel),
    (
        % Only run the analysis in .mm grades.
        UseMinimalModel = yes,
        globals.get_op_mode(Globals, OpMode),
        ( if
            OpMode = opm_top_args(opma_augment(OpModeAugment)),
            ( OpModeAugment = opmau_make_opt_int
            ; OpModeAugment = opmau_make_trans_opt_int
            ; OpModeAugment = opmau_make_analysis_registry
            )
        then
            Pass1Only = yes
        else
            Pass1Only = no
        ),
        module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
        SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
        globals.lookup_bool_option(Globals, debug_mm_tabling_analysis, Debug),
        list.foldl(analyse_mm_tabling_in_scc(Debug, Pass1Only), SCCs,
            !ModuleInfo),

        module_info_get_proc_analysis_kinds(!.ModuleInfo, ProcAnalysisKinds0),
        set.insert(pak_mm_tabling, ProcAnalysisKinds0, ProcAnalysisKinds),
        module_info_set_proc_analysis_kinds(ProcAnalysisKinds, !ModuleInfo),

        % Record results if making the analysis registry.  We do this in a
        % separate pass so that we record results for exported procedures
        % that have a `:- pragma external_{pred/func}', which don't analyse
        % because we don't have clauses for them.
        ( if
            OpMode = opm_top_args(opma_augment(opmau_make_analysis_registry))
        then
            module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
            module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
            list.foldl(maybe_record_mm_tabling_result(!.ModuleInfo),
                PredIds, AnalysisInfo0, AnalysisInfo),
            module_info_set_analysis_info(AnalysisInfo, !ModuleInfo)
        else
            true
        )
    ;
        UseMinimalModel = no
    ).

%----------------------------------------------------------------------------%
%
% Perform minimal model tabling analysis on a SCC.
%

:- type mm_tabling_proc_result
    --->    mm_tabling_proc_result(
                mtpr_ppid                       :: pred_proc_id,
                mtpr_status                     :: mm_tabling_status,
                mtpr_maybe_analysis_status      :: maybe(analysis_status)
            ).

:- pred analyse_mm_tabling_in_scc(bool::in, bool::in, scc::in,
    module_info::in, module_info::out) is det.

analyse_mm_tabling_in_scc(Debug, Pass1Only, SCC, !ModuleInfo) :-
    % Begin by analysing each procedure in the SCC.
    set.foldl2(check_proc_for_mm_tabling(SCC), SCC, [], ProcResults,
        !ModuleInfo),
    mm_tabling_combine_individual_proc_results(ProcResults, TablingStatus,
        MaybeAnalysisStatus),

    % Print out debugging information.
    (
        Debug = yes,
        trace [io(!IO)] (
            get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
            dump_mm_tabling_analysis_debug_info(DebugStream, !.ModuleInfo, SCC,
                TablingStatus, !IO)
        )
    ;
        Debug = no
    ),

    ProcTablingInfo = proc_mm_tabling_info(TablingStatus, MaybeAnalysisStatus),
    set.foldl(set_mm_tabling_info(ProcTablingInfo), SCC, !ModuleInfo),
    (
        Pass1Only = no,
        set.foldl(mm_tabling_annotate_proc, SCC, !ModuleInfo)
    ;
        Pass1Only = yes
    ).

:- pred set_mm_tabling_info(proc_mm_tabling_info::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

set_mm_tabling_info(ProcTablingInfo, PPId, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    proc_info_set_mm_tabling_info(yes(ProcTablingInfo), ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo0, ProcInfo, !ModuleInfo).

    % Examine how procedures interact with other procedures that are
    % mutually-recursive to them.
    %
:- pred mm_tabling_combine_individual_proc_results(
    list(mm_tabling_proc_result)::in,
   mm_tabling_status::out, maybe(analysis_status)::out) is det.

mm_tabling_combine_individual_proc_results([], _, _) :-
    unexpected($pred, "empty SCC").
mm_tabling_combine_individual_proc_results(ProcResults @ [_ | _], SCC_Result,
        MaybeAnalysisStatus) :-
    ( if
        % If none of the procedures calls tabled procedures or is conditional
        % then the SCC cannot call tabled procedures.
        all [ProcResult] (
            list.member(ProcResult, ProcResults)
        =>
            ProcResult ^ mtpr_status = mm_tabled_will_not_call
        )
    then
        SCC_Result = mm_tabled_will_not_call
    else if
        all [ProcResult] (
            list.member(ProcResult, ProcResults)
        =>
            ProcResult ^ mtpr_status \= mm_tabled_may_call
        ),
        some [ConditionalResult] (
            list.member(ConditionalResult, ProcResults),
            ConditionalResult ^ mtpr_status = mm_tabled_conditional
        )
    then
        SCC_Result = mm_tabled_conditional
    else
        % Otherwise the SCC might call tabled procedures.
        SCC_Result = mm_tabled_may_call
    ),
    mm_tabling_combine_proc_result_maybe_analysis_statuses(ProcResults,
        MaybeAnalysisStatus).

:- pred mm_tabling_combine_proc_result_maybe_analysis_statuses(
    list(mm_tabling_proc_result)::in, maybe(analysis_status)::out) is det.

mm_tabling_combine_proc_result_maybe_analysis_statuses(ProcResults,
        MaybeAnalysisStatus) :-
    list.map(maybe_mm_tabling_analysis_status, ProcResults,
        MaybeAnalysisStatuses),
    list.foldl(combine_maybe_mm_tabling_analysis_status, MaybeAnalysisStatuses,
        yes(optimal), MaybeAnalysisStatus).

:- pred maybe_mm_tabling_analysis_status(mm_tabling_proc_result::in,
    maybe(analysis_status)::out) is det.

maybe_mm_tabling_analysis_status(ProcResult, MaybeAnalysisStatus) :-
    MaybeAnalysisStatus = ProcResult ^ mtpr_maybe_analysis_status.

%----------------------------------------------------------------------------%

    % Perform minimal model tabling analysis on a procedure.
    %
:- pred check_proc_for_mm_tabling(scc::in, pred_proc_id::in,
    list(mm_tabling_proc_result)::in, list(mm_tabling_proc_result)::out,
    module_info::in, module_info::out) is det.

check_proc_for_mm_tabling(SCC, PPId, !Results, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    (
        EvalMethod = eval_tabled(tabled_minimal(_)),
        Result = mm_tabled_may_call,
        MaybeAnalysisStatus = yes(optimal)
    ;
        ( EvalMethod = eval_normal
        ; EvalMethod = eval_tabled(tabled_loop_check)
        ; EvalMethod = eval_tabled(tabled_memo(_))
        ; EvalMethod = eval_tabled(tabled_io(_, _))
        ),
        proc_info_get_goal(ProcInfo, Body),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        check_goal_for_mm_tabling(SCC, VarTypes, Body, Result,
            MaybeAnalysisStatus, !ModuleInfo)
    ),
    list.cons(mm_tabling_proc_result(PPId, Result, MaybeAnalysisStatus),
        !Results).

%----------------------------------------------------------------------------%

    % Perform minimal model tabling analysis of a goal.
    %
:- pred check_goal_for_mm_tabling(scc::in, vartypes::in, hlds_goal::in,
    mm_tabling_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

check_goal_for_mm_tabling(SCC, VarTypes, Goal, Result, MaybeAnalysisStatus,
        !ModuleInfo) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(_, _, _, Kind, _),
        Result = mm_tabled_will_not_call,
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
        GoalExpr = plain_call(CalleePredId, CalleeProcId, CallArgs, _, _, _),
        CalleePPId = proc(CalleePredId, CalleeProcId),
        check_call_for_mm_tabling(CalleePPId, CallArgs, SCC, VarTypes, Result,
            MaybeAnalysisStatus, !ModuleInfo)
    ;
        GoalExpr = generic_call(Details, _Args, _ArgModes, _, _),
        (
            % XXX We should use any results from closure analysis here.
            Details = higher_order(_Var, _, _, _),
            Result  = mm_tabled_may_call
        ;
            Details = class_method(_, _, _, _),
            Result  = mm_tabled_may_call
        ;
            Details = event_call(_),
            Result = mm_tabled_will_not_call
        ;
            Details = cast(_),
            Result = mm_tabled_will_not_call
        ),
        MaybeAnalysisStatus = yes(optimal)
    ;
        GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        Result = get_mm_tabling_status_from_attributes(Attributes),
        MaybeAnalysisStatus = yes(optimal)
    ;
        GoalExpr = negation(SubGoal),
        check_goal_for_mm_tabling(SCC, VarTypes, SubGoal, Result,
            MaybeAnalysisStatus, !ModuleInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            Result = mm_tabled_will_not_call,
            MaybeAnalysisStatus = yes(optimal)
        else
            check_goal_for_mm_tabling(SCC, VarTypes, SubGoal, Result,
                MaybeAnalysisStatus, !ModuleInfo)
        )
    ;
        (
            GoalExpr = conj(_, Goals)
        ;
            GoalExpr = disj(Goals)
        ;
            GoalExpr = if_then_else(_, Cond, Then, Else),
            Goals = [Cond, Then, Else]
        ;
            GoalExpr = switch(_, _, Cases),
            Goals = list.map((func(case(_, _, CaseGoal)) = CaseGoal), Cases)
        ),
        check_goals_for_mm_tabling(SCC, VarTypes, Goals, Result,
            MaybeAnalysisStatus, !ModuleInfo)
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred check_goals_for_mm_tabling(scc::in, vartypes::in,
    hlds_goals::in, mm_tabling_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

check_goals_for_mm_tabling(SCC, VarTypes, Goals, Result, MaybeAnalysisStatus,
        !ModuleInfo) :-
    list.map2_foldl(check_goal_for_mm_tabling(SCC, VarTypes), Goals,
        Results, MaybeAnalysisStatuses, !ModuleInfo),
    list.foldl(combine_mm_tabling_status, Results, mm_tabled_will_not_call,
        Result),
    list.foldl(combine_maybe_mm_tabling_analysis_status, MaybeAnalysisStatuses,
        yes(optimal), MaybeAnalysisStatus).

%----------------------------------------------------------------------------%

:- pred check_call_for_mm_tabling(pred_proc_id::in, prog_vars::in,
    scc::in, vartypes::in, mm_tabling_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

check_call_for_mm_tabling(CalleePPId, CallArgs, SCC, VarTypes, Result,
        MaybeAnalysisStatus, !ModuleInfo) :-
    CalleePPId = proc(CalleePredId, _),
    module_info_pred_info(!.ModuleInfo, CalleePredId, CalleePredInfo),
    ( if
        % Handle (mutually-)recursive calls.
        set.member(CalleePPId, SCC)
    then
        % XXX user-defined uc - need to handle polymorphic recursion here.
        Result = mm_tabled_will_not_call,
        MaybeAnalysisStatus = yes(optimal)
    else if
        pred_info_is_builtin(CalleePredInfo)
    then
        Result = mm_tabled_will_not_call,
        MaybeAnalysisStatus = yes(optimal)
    else if
        % Handle builtin unify and compare.
        %
        % NOTE: The type specific unify and compare predicates are just
        % treated as though they were normal predicates.

        ModuleName = pred_info_module(CalleePredInfo),
        any_mercury_builtin_module(ModuleName),
        Name = pred_info_name(CalleePredInfo),
        Arity = pred_info_orig_arity(CalleePredInfo),
        ( SpecialPredId = spec_pred_compare
        ; SpecialPredId = spec_pred_unify
        ),
        special_pred_name_arity(SpecialPredId, GenericPredName,
            _TypeSpecificPredName, Arity),
        Name = GenericPredName
    then
        % XXX user-defined uc
        Result = mm_tabled_may_call,
        MaybeAnalysisStatus = yes(optimal)
    else
        % Handle normal calls.
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, intermodule_analysis, Intermod),
        ( if
            Intermod = yes,
            pred_info_is_imported_not_external(CalleePredInfo),
            not is_unify_index_or_compare_pred(CalleePredInfo)
        then
            % Use the intermodule analysis framework if this is an imported
            % procedure and `--intermodule-analysis' is enabled.
            %
            search_mm_tabling_analysis_status(CalleePPId, Result0,
                AnalysisStatus, !ModuleInfo),
            (
                Result0 = mm_tabled_conditional,
                % XXX user-defined uc
                Result = mm_tabled_will_not_call
            ;
                ( Result0 = mm_tabled_may_call
                ; Result0 = mm_tabled_will_not_call
                ),
                Result = Result0
            ),
            MaybeAnalysisStatus = yes(AnalysisStatus)
        else
            % Otherwise, the information (if we have any) will be in the
            % mm_tabling_info table.
            check_call_for_mm_tabling_calls(!.ModuleInfo, VarTypes,
                CalleePPId, CallArgs, MaybeResult),
            (
                MaybeResult = yes(ProcTablingInfo),
                ProcTablingInfo = proc_mm_tabling_info(Result,
                    MaybeAnalysisStatus)
            ;
                MaybeResult = no,
                % If we do not have any information about the callee procedure
                % then assume that it calls minimal model tabled procedures.
                Result = mm_tabled_may_call,
                (
                    Intermod = yes,
                    MaybeAnalysisStatus = yes(optimal)
                ;
                    Intermod = no,
                    MaybeAnalysisStatus = no
                )
            )
        )
    ).

%----------------------------------------------------------------------------%

    % Utility procedure for processing goals.
    %
:- func get_mm_tabling_status_from_attributes(pragma_foreign_proc_attributes)
    = mm_tabling_status.

get_mm_tabling_status_from_attributes(Attributes) =
    ( if
        (
            get_may_call_mm_tabled(Attributes) = proc_will_not_call_mm_tabled
        ;
            get_may_call_mm_tabled(Attributes) = proc_default_calls_mm_tabled,
            get_may_call_mercury(Attributes) = proc_will_not_call_mercury
        )
    then
        mm_tabled_will_not_call
    else
        mm_tabled_may_call
    ).

%----------------------------------------------------------------------------%

    % Additional code for handling calls.
    %
:- pred check_call_for_mm_tabling_calls(module_info::in, vartypes::in,
    pred_proc_id::in, prog_vars::in, maybe(proc_mm_tabling_info)::out) is det.

check_call_for_mm_tabling_calls(ModuleInfo, _VarTypes, PPId, _CallArgs,
        MaybeProcTablingInfo) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _PredInfo, ProcInfo),
    proc_info_get_mm_tabling_info(ProcInfo, MaybeProcTablingInfo).
    % XXX user-defined uc (and higher-order args too)

%----------------------------------------------------------------------------%

:- pred combine_mm_tabling_status(mm_tabling_status::in,
    mm_tabling_status::in, mm_tabling_status::out) is det.

combine_mm_tabling_status(mm_tabled_will_not_call, Status, Status).
combine_mm_tabling_status(mm_tabled_may_call, _, mm_tabled_may_call).
combine_mm_tabling_status(mm_tabled_conditional, mm_tabled_will_not_call,
        mm_tabled_conditional).
combine_mm_tabling_status(mm_tabled_conditional, mm_tabled_conditional,
        mm_tabled_conditional).
combine_mm_tabling_status(mm_tabled_conditional, mm_tabled_may_call,
        mm_tabled_may_call).

:- pred combine_maybe_mm_tabling_analysis_status(maybe(analysis_status)::in,
    maybe(analysis_status)::in, maybe(analysis_status)::out) is det.

combine_maybe_mm_tabling_analysis_status(MaybeStatusA, MaybeStatusB,
        MaybeStatus) :-
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
% Code for attaching tabling analysis information to goals
%

    % Traverse the body of the procedure and attach the
    % `will_not_call_mm_tabled' feature to the goal_infos of those goals that
    % do not make calls to minimal model tabled procedures.
    %
:- pred mm_tabling_annotate_proc(pred_proc_id::in,
    module_info::in, module_info::out) is det.

mm_tabling_annotate_proc(PPId, !ModuleInfo) :-
    some [!ProcInfo, !Body] (
        module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, !:Body),
        proc_info_get_vartypes(!.ProcInfo, VarTypes),
        mm_tabling_annotate_goal(VarTypes, !Body, _Status, !ModuleInfo),
        proc_info_set_goal(!.Body, !ProcInfo),
        module_info_set_pred_proc_info(PPId, PredInfo, !.ProcInfo, !ModuleInfo)
    ).

:- pred mm_tabling_annotate_goal(vartypes::in, hlds_goal::in, hlds_goal::out,
    mm_tabling_status::out, module_info::in, module_info::out) is det.

mm_tabling_annotate_goal(VarTypes, !Goal, Status, !ModuleInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    mm_tabling_annotate_goal_2(VarTypes, GoalExpr0, GoalExpr, Status,
        !ModuleInfo),
    (
        Status = mm_tabled_will_not_call,
        goal_info_add_feature(feature_will_not_call_mm_tabled,
            GoalInfo0, GoalInfo)
    ;
        ( Status = mm_tabled_may_call
        ; Status = mm_tabled_conditional
        ),
        GoalInfo = GoalInfo0
    ),
    !:Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred mm_tabling_annotate_goal_2(vartypes::in,
    hlds_goal_expr::in, hlds_goal_expr::out, mm_tabling_status::out,
    module_info::in, module_info::out) is det.

mm_tabling_annotate_goal_2(VarTypes, !GoalExpr, Status, !ModuleInfo) :-
    (
        !.GoalExpr = unify(_, _, _, Kind, _),
        (
            Kind = complicated_unify(_, _, _),
            unexpected($pred, "complicated unify")
        ;
            ( Kind = construct(_, _, _, _, _, _, _)
            ; Kind = deconstruct(_, _, _, _, _, _)
            ; Kind = assign(_, _)
            ; Kind = simple_test(_, _)
            )
        ),
        Status = mm_tabled_will_not_call
    ;
        !.GoalExpr = plain_call(CalleePredId, CalleeProcId, CallArgs, _, _, _),
        CalleePPId = proc(CalleePredId, CalleeProcId),
        mm_tabling_annotate_call(CalleePPId, CallArgs, VarTypes, Status,
            !ModuleInfo)
    ;
        !.GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        Status = get_mm_tabling_status_from_attributes(Attributes)
    ;
        !.GoalExpr = generic_call(GenericCall, _Args, _Modes, _, _Detism),
        (
            % XXX We should use any results from closure analysis here.
            GenericCall = higher_order(_Var, _, _, _),
            Status = mm_tabled_may_call
        ;
            GenericCall = class_method(_, _, _, _),
            Status = mm_tabled_may_call
        ;
            GenericCall = event_call(_),
            Status = mm_tabled_will_not_call
        ;
            GenericCall = cast(_),
            Status = mm_tabled_will_not_call
        )
    ;
        !.GoalExpr = conj(ConjType, Conjuncts0),
        mm_tabling_annotate_goal_list(VarTypes, Conjuncts0, Conjuncts, Status,
            !ModuleInfo),
        !:GoalExpr = conj(ConjType, Conjuncts)
    ;
        !.GoalExpr = disj(Disjuncts0),
        mm_tabling_annotate_goal_list(VarTypes, Disjuncts0, Disjuncts, Status,
            !ModuleInfo),
        !:GoalExpr = disj(Disjuncts)
    ;
        !.GoalExpr = switch(Var, CanFail, Cases0),
        mm_tabling_annotate_cases(VarTypes, Cases0, Cases, Status,
            !ModuleInfo),
        !:GoalExpr = switch(Var, CanFail, Cases)
    ;
        !.GoalExpr = if_then_else(Vars, Cond0, Then0, Else0),
        mm_tabling_annotate_goal(VarTypes, Cond0, Cond, CondStatus,
            !ModuleInfo),
        mm_tabling_annotate_goal(VarTypes, Then0, Then, ThenStatus,
            !ModuleInfo),
        mm_tabling_annotate_goal(VarTypes, Else0, Else, ElseStatus,
            !ModuleInfo),
        ( if
            CondStatus = mm_tabled_will_not_call,
            ThenStatus = mm_tabled_will_not_call,
            ElseStatus = mm_tabled_will_not_call
        then
            Status = mm_tabled_will_not_call
        else
            Status = mm_tabled_may_call
        ),
        !:GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        !.GoalExpr = negation(SubGoal0),
        mm_tabling_annotate_goal(VarTypes, SubGoal0, SubGoal, Status,
            !ModuleInfo),
        !:GoalExpr = negation(SubGoal)
    ;
        !.GoalExpr = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            Status = mm_tabled_will_not_call
        else
            mm_tabling_annotate_goal(VarTypes, SubGoal0, SubGoal, Status,
                !ModuleInfo),
            !:GoalExpr = scope(Reason, SubGoal)
        )
    ;
        !.GoalExpr = shorthand(_),
        unexpected($pred, "shorthand goal")
    ).

:- pred mm_tabling_annotate_goal_list(vartypes::in,
    list(hlds_goal)::in, list(hlds_goal)::out, mm_tabling_status::out,
    module_info::in, module_info::out) is det.

mm_tabling_annotate_goal_list(VarTypes, !Goals, Status, !ModuleInfo) :-
    list.map2_foldl(mm_tabling_annotate_goal(VarTypes), !Goals, Statuses,
        !ModuleInfo),
    list.foldl(combine_mm_tabling_status, Statuses, mm_tabled_will_not_call,
        Status).

:- pred mm_tabling_annotate_cases(vartypes::in,
    list(case)::in, list(case)::out, mm_tabling_status::out,
    module_info::in, module_info::out) is det.

mm_tabling_annotate_cases(VarTypes, !Cases, Status, !ModuleInfo) :-
    list.map2_foldl(mm_tabling_annotate_case(VarTypes), !Cases, Statuses,
        !ModuleInfo),
    list.foldl(combine_mm_tabling_status, Statuses, mm_tabled_will_not_call,
        Status).

:- pred mm_tabling_annotate_case(vartypes::in, case::in, case::out,
    mm_tabling_status::out, module_info::in, module_info::out)
    is det.

mm_tabling_annotate_case(VarTypes, !Case, Status, !ModuleInfo) :-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    mm_tabling_annotate_goal(VarTypes, Goal0, Goal, Status, !ModuleInfo),
    !:Case = case(MainConsId, OtherConsIds, Goal).

:- pred mm_tabling_annotate_call(pred_proc_id::in, prog_vars::in, vartypes::in,
    mm_tabling_status::out, module_info::in, module_info::out) is det.

mm_tabling_annotate_call(CalleePPId, CallArgs, VarTypes, Status,
        !ModuleInfo) :-
    CalleePPId = proc(CalleePredId, _),
    module_info_pred_info(!.ModuleInfo, CalleePredId, CalleePredInfo),
    ( if
        pred_info_is_builtin(CalleePredInfo)
    then
        Status = mm_tabled_will_not_call
    else if
        % Handle builtin unify and compare.
        ModuleName = pred_info_module(CalleePredInfo),
        any_mercury_builtin_module(ModuleName),
        Name = pred_info_name(CalleePredInfo),
        Arity = pred_info_orig_arity(CalleePredInfo),
        ( SpecialPredId = spec_pred_compare
        ; SpecialPredId = spec_pred_unify
        ),
        special_pred_name_arity(SpecialPredId, GenericPredName,
            _TypeSpecPredName, Arity),
        Name = GenericPredName
    then
        % XXX user-defined uc
        Status = mm_tabled_may_call
    else
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        ( if
            IntermodAnalysis = yes,
            pred_info_is_imported_not_external(CalleePredInfo)
        then
            search_mm_tabling_analysis_status(CalleePPId, Result,
                AnalysisStatus, !ModuleInfo),
            (
                AnalysisStatus = invalid,
                unexpected($pred,
                    "invalid analysis result while annotating goals")
            ;
                ( AnalysisStatus = optimal
                ; AnalysisStatus = suboptimal
                ),
                % XXX user-defined uc
                (
                    Result = mm_tabled_conditional,
                    Status = mm_tabled_will_not_call
                ;
                    ( Result = mm_tabled_may_call
                    ; Result = mm_tabled_will_not_call
                    ),
                    Status = Result
                )
            )
        else
            check_call_for_mm_tabling_calls(!.ModuleInfo, VarTypes,
                CalleePPId, CallArgs, MaybeResult),
            (
                MaybeResult = yes(CalleeProcTablingInfo),
                CalleeProcTablingInfo = proc_mm_tabling_info(Status, _)
            ;
                MaybeResult = no,
                Status = mm_tabled_may_call
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% Stuff for the intermodule analysis framework.
%

:- type mm_tabling_analysis_answer
    --->    mm_tabling_analysis_answer(mm_tabling_status).

:- func mm_tabling_analysis_name = string.

mm_tabling_analysis_name = "mm_tabling_analysis".

:- instance analysis(no_func_info, any_call, mm_tabling_analysis_answer) where
[
    analysis_name(_, _) = mm_tabling_analysis_name,
    analysis_version_number(_, _) = 1,
    preferred_fixpoint_type(_, _) = least_fixpoint,
    bottom(_, _) = mm_tabling_analysis_answer(mm_tabled_will_not_call),
    top(_, _) = mm_tabling_analysis_answer(mm_tabled_may_call),
    get_func_info(_, _, _, _, _, no_func_info)
].

:- instance answer_pattern(no_func_info, mm_tabling_analysis_answer) where [].
:- instance partial_order(no_func_info, mm_tabling_analysis_answer) where [
    ( more_precise_than(no_func_info, Answer1, Answer2) :-
        Answer1 = mm_tabling_analysis_answer(Status1),
        Answer2 = mm_tabling_analysis_answer(Status2),
        mm_tabling_status_more_precise_than(Status1, Status2)
    ),

    equivalent(no_func_info, Status, Status)
].

:- pred mm_tabling_status_more_precise_than(mm_tabling_status::in,
    mm_tabling_status::in) is semidet.

mm_tabling_status_more_precise_than(mm_tabled_will_not_call,
    mm_tabled_may_call).
mm_tabling_status_more_precise_than(mm_tabled_will_not_call,
    mm_tabled_conditional).
mm_tabling_status_more_precise_than(mm_tabled_conditional,
    mm_tabled_may_call).

:- instance to_term(mm_tabling_analysis_answer) where [
    func(to_term/1) is mm_tabling_analysis_answer_to_term,
    pred(from_term/2) is mm_tabling_analysis_answer_from_term
].

:- func mm_tabling_analysis_answer_to_term(mm_tabling_analysis_answer)
    = term.

mm_tabling_analysis_answer_to_term(Answer) = Term :-
    Answer = mm_tabling_analysis_answer(Status),
    mm_tabling_status_to_string(Status, String),
    Term = term.functor(atom(String), [], context_init).

:- pred mm_tabling_analysis_answer_from_term(term::in,
    mm_tabling_analysis_answer::out) is semidet.

mm_tabling_analysis_answer_from_term(Term, Answer) :-
    Term = term.functor(atom(String), [], _),
    mm_tabling_status_to_string(Status, String),
    Answer = mm_tabling_analysis_answer(Status).

:- pred mm_tabling_status_to_string(mm_tabling_status, string).
:- mode mm_tabling_status_to_string(in, out) is det.
:- mode mm_tabling_status_to_string(out, in) is semidet.

mm_tabling_status_to_string(mm_tabled_may_call,
    "mm_tabled_may_call").
mm_tabling_status_to_string(mm_tabled_will_not_call,
    "mm_tabled_will_not_call").
mm_tabling_status_to_string(mm_tabled_conditional,
    "mm_tabled_conditional").

:- pred search_mm_tabling_analysis_status(pred_proc_id::in,
    mm_tabling_status::out,
    analysis_status::out, module_info::in, module_info::out) is det.

search_mm_tabling_analysis_status(PPId, Result, AnalysisStatus, !ModuleInfo) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    search_mm_tabling_analysis_status_2(!.ModuleInfo, PPId, Result,
        AnalysisStatus, AnalysisInfo0, AnalysisInfo),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred search_mm_tabling_analysis_status_2(module_info::in, pred_proc_id::in,
    mm_tabling_status::out, analysis_status::out,
    analysis_info::in, analysis_info::out) is det.

search_mm_tabling_analysis_status_2(ModuleInfo, PPId, Result, AnalysisStatus,
        !AnalysisInfo) :-
    mmc_analysis.module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
    Call = any_call,
    lookup_best_result(!.AnalysisInfo, ModuleName, FuncId, no_func_info, Call,
        MaybeBestStatus),
    (
        MaybeBestStatus = yes(analysis_result(BestCall, BestAnswer,
            AnalysisStatus)),
        BestAnswer = mm_tabling_analysis_answer(Result),
        record_dependency(ModuleName, FuncId, no_func_info, BestCall,
            _ : mm_tabling_analysis_answer, !AnalysisInfo)
    ;
        MaybeBestStatus = no,
        % If we do not have any information about the callee procedure
        % then assume that it modifies the calls a minimal model tabled
        % procedure.
        top(no_func_info, Call) = Answer,
        Answer = mm_tabling_analysis_answer(Result),
        AnalysisStatus = optimal,
        record_request(mm_tabling_analysis_name, ModuleName, FuncId, Call,
            !AnalysisInfo)
    ).

:- pred maybe_record_mm_tabling_result(module_info::in, pred_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_mm_tabling_result(ModuleInfo, PredId, !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_procids(PredInfo),
    list.foldl(maybe_record_mm_tabling_result_2(ModuleInfo, PredId, PredInfo),
        ProcIds, !AnalysisInfo).

:- pred maybe_record_mm_tabling_result_2(module_info::in, pred_id::in,
    pred_info::in, proc_id::in, analysis_info::in, analysis_info::out) is det.

maybe_record_mm_tabling_result_2(ModuleInfo, PredId, PredInfo, ProcId,
        !AnalysisInfo) :-
    should_write_mm_tabling_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_analysis_framework, ShouldWrite),
    (
        ShouldWrite = should_write,
        PPId = proc(PredId, ProcId),
        lookup_proc_mm_tabling_info(ModuleInfo, PPId, Status, ResultStatus),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, any_call,
            mm_tabling_analysis_answer(Status), ResultStatus, !AnalysisInfo)
    ;
        ShouldWrite = should_not_write
    ).

:- pred lookup_proc_mm_tabling_info(module_info::in, pred_proc_id::in,
    mm_tabling_status::out, analysis_status::out) is det.

lookup_proc_mm_tabling_info(ModuleInfo, PPId, Status, ResultStatus) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _PredInfo, ProcInfo),
    proc_info_get_mm_tabling_info(ProcInfo, MaybeProcTablingInfo),
    (
        MaybeProcTablingInfo = yes(ProcTablingInfo),
        ProcTablingInfo = proc_mm_tabling_info(Status, MaybeResultStatus),
        (
            MaybeResultStatus = yes(ResultStatus)
        ;
            MaybeResultStatus = no,
            unexpected($pred, "no result status")
        )
    ;
        MaybeProcTablingInfo = no,
        % Probably an exported `:- pragma external_{pred/func}' procedure.
        Status = mm_tabled_may_call,
        ResultStatus = optimal
    ).

%----------------------------------------------------------------------------%
%
% Code for printing out debugging traces.
%

:- pred dump_mm_tabling_analysis_debug_info(io.text_output_stream::in,
    module_info::in, scc::in, mm_tabling_status::in, io::di, io::uo) is det.

dump_mm_tabling_analysis_debug_info(Stream, ModuleInfo, SCC, Status, !IO) :-
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
:- end_module transform_hlds.tabling_analysis.
%----------------------------------------------------------------------------%
