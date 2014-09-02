%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
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
%   - improve handle higher-order constructs / type class method calls
%   - handle user-defined equality and comparison correctly
%       - the bits marked `XXX user-defined uc' need to be changed
%
%----------------------------------------------------------------------------%

:- module transform_hlds.tabling_analysis.
:- interface.

:- import_module analysis.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

%----------------------------------------------------------------------------%

    % Analyse minimal model tabling in a module.
    %
:- pred analyse_mm_tabling_in_module(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Write out the mm_tabling_info pragma for this module.
    %
:- pred write_pragma_mm_tabling_info(module_info::in, mm_tabling_info::in,
    pred_id::in, io::di, io::uo) is det.

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

:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module transform_hlds.dependency_graph.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%----------------------------------------------------------------------------%
%
% Perform minimal model tabling analysis on a module
%

analyse_mm_tabling_in_module(!ModuleInfo, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_minimal_model_stack_copy,
        UseMinimalModel),
    (
        % Only run the analysis in .mm grades.
        UseMinimalModel = yes,
        globals.lookup_bool_option(Globals, make_optimization_interface,
            MakeOptInt),
        globals.lookup_bool_option(Globals, make_transitive_opt_interface,
            MakeTransOptInt),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        globals.lookup_bool_option(Globals, make_analysis_registry,
            MakeAnalysisReg),
        Pass1Only = MakeOptInt `bool.or` MakeTransOptInt
            `bool.or` MakeAnalysisReg,
        module_info_ensure_dependency_info(!ModuleInfo),
        module_info_dependency_info(!.ModuleInfo, DepInfo),
        hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
        globals.lookup_bool_option(Globals, debug_mm_tabling_analysis, Debug),
        list.foldl(analyse_mm_tabling_in_scc(Debug, Pass1Only), SCCs,
            !ModuleInfo),

        % Only write mm_tabling_info pragmas to `.opt' files for
        % `--intermodule-optimisation' not `--intermodule-analysis'.
        (
            MakeOptInt = yes,
            IntermodAnalysis = no
        ->
            make_opt_int(!.ModuleInfo, !IO)
        ;
            true
        ),

        % Record results if making the analysis registry.  We do this in a
        % separate pass so that we record results for exported `:- external'
        % procedures, which don't get analysed because we don't have clauses
        % for them.
        (
            MakeAnalysisReg = yes,
            module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
            module_info_get_valid_predids(PredIds, !ModuleInfo),
            list.foldl(maybe_record_mm_tabling_result(!.ModuleInfo),
                PredIds, AnalysisInfo0, AnalysisInfo),
            module_info_set_analysis_info(AnalysisInfo, !ModuleInfo)
        ;
            MakeAnalysisReg = no
        )
    ;
        UseMinimalModel = no
    ).

%----------------------------------------------------------------------------%
%
% Perform minimal model tabling analysis on a SCC
%

:- type scc == list(pred_proc_id).

:- type proc_results == list(proc_result).

:- type proc_result
    --->    proc_result(
                ppid                    :: pred_proc_id,
                status                  :: mm_tabling_status,
                maybe_analysis_status   :: maybe(analysis_status)
            ).

:- pred analyse_mm_tabling_in_scc(bool::in, bool::in, scc::in,
    module_info::in, module_info::out) is det.

analyse_mm_tabling_in_scc(Debug, Pass1Only, SCC, !ModuleInfo) :-
    %
    % Begin by analysing each procedure in the SCC.
    %
    list.foldl2(check_proc_for_mm_tabling(SCC), SCC, [], ProcResults,
        !ModuleInfo),
    combine_individual_proc_results(ProcResults, TablingStatus,
        MaybeAnalysisStatus),
    %
    % Print out debugging information.
    %
    (
        Debug = yes,
        trace [io(!IO)] (
            dump_mm_tabling_analysis_debug_info(!.ModuleInfo, SCC,
                TablingStatus, !IO)
        )
    ;
        Debug = no
    ),
    %
    % Update the mm_tabling_info table with information about this SCC.
    %
    module_info_get_mm_tabling_info(!.ModuleInfo, TablingInfo0),
    Update = (pred(PPId::in, Info0::in, Info::out) is det :-
        Info = Info0 ^ elem(PPId) :=
            proc_mm_tabling_info(TablingStatus, MaybeAnalysisStatus)
    ),
    list.foldl(Update, SCC, TablingInfo0, TablingInfo),
    module_info_set_mm_tabling_info(TablingInfo, !ModuleInfo),
    (
        Pass1Only = no,
        list.foldl(annotate_proc, SCC, !ModuleInfo)
    ;
        Pass1Only = yes
    ).

    % Examine how procedures interact with other procedures that are
    % mutually-recursive to them.
    %
:- pred combine_individual_proc_results(proc_results::in,
   mm_tabling_status::out, maybe(analysis_status)::out) is det.

combine_individual_proc_results([], _, _) :-
    unexpected($module, $pred, "empty SCC").
combine_individual_proc_results(ProcResults @ [_ | _], SCC_Result,
        MaybeAnalysisStatus) :-
    (
        % If none of the procedures calls tabled procedures or is conditional
        % then the SCC cannot call tabled procedures.
        all [ProcResult] (
            list.member(ProcResult, ProcResults)
        =>
            ProcResult ^ status = mm_tabled_will_not_call
        )
    ->
        SCC_Result = mm_tabled_will_not_call
    ;
        all [ProcResult] (
            list.member(ProcResult, ProcResults)
        =>
            ProcResult ^ status \= mm_tabled_may_call
        ),
        some [ConditionalResult] (
            list.member(ConditionalResult, ProcResults),
            ConditionalResult ^ status = mm_tabled_conditional
        )
    ->
        SCC_Result = mm_tabled_conditional
    ;
        % Otherwise the SCC might call tabled procedures.
        SCC_Result = mm_tabled_may_call
    ),
    combine_proc_result_maybe_analysis_statuses(ProcResults,
        MaybeAnalysisStatus).

:- pred combine_proc_result_maybe_analysis_statuses(proc_results::in,
    maybe(analysis_status)::out) is det.

combine_proc_result_maybe_analysis_statuses(ProcResults,
        MaybeAnalysisStatus) :-
    list.map(maybe_analysis_status, ProcResults, MaybeAnalysisStatuses),
    list.foldl(combine_maybe_analysis_status, MaybeAnalysisStatuses,
        yes(optimal), MaybeAnalysisStatus).

:- pred maybe_analysis_status(proc_result::in, maybe(analysis_status)::out)
    is det.

maybe_analysis_status(ProcResult, ProcResult ^ maybe_analysis_status).

%----------------------------------------------------------------------------%
%
% Perform minimal model tabling analysis on a procedure
%

:- pred check_proc_for_mm_tabling(scc::in, pred_proc_id::in, proc_results::in,
    proc_results::out, module_info::in, module_info::out) is det.

check_proc_for_mm_tabling(SCC, PPId, !Results, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    (
        EvalMethod = eval_minimal(_),
        Result = mm_tabled_may_call,
        MaybeAnalysisStatus = yes(optimal)
    ;
        ( EvalMethod = eval_normal
        ; EvalMethod = eval_loop_check
        ; EvalMethod = eval_memo
        ; EvalMethod = eval_table_io(_, _)
        ),
        proc_info_get_goal(ProcInfo, Body),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        check_goal_for_mm_tabling(SCC, VarTypes, Body, Result,
            MaybeAnalysisStatus, !ModuleInfo)
    ),
    list.cons(proc_result(PPId, Result, MaybeAnalysisStatus), !Results).

%----------------------------------------------------------------------------%
%
% Perform minimal model tabling analysis of a goal
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
            unexpected($module, $pred, "complicated unify")
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
        (
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        ->
            Result = mm_tabled_will_not_call,
            MaybeAnalysisStatus = yes(optimal)
        ;
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
        unexpected($module, $pred, "shorthand")
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
    list.foldl(combine_maybe_analysis_status, MaybeAnalysisStatuses,
        yes(optimal), MaybeAnalysisStatus).

%----------------------------------------------------------------------------%
%
% Code for checking calls
%

:- pred check_call_for_mm_tabling(pred_proc_id::in, prog_vars::in,
    scc::in, vartypes::in, mm_tabling_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

check_call_for_mm_tabling(CalleePPId, CallArgs, SCC, VarTypes, Result,
        MaybeAnalysisStatus, !ModuleInfo) :-
    CalleePPId = proc(CalleePredId, _),
    module_info_pred_info(!.ModuleInfo, CalleePredId, CalleePredInfo),
    (
        % Handle (mutually-)recursive calls.
        list.member(CalleePPId, SCC)
    ->
        % XXX user-defined uc - need to handle polymorphic recursion here.
        Result = mm_tabled_will_not_call,
        MaybeAnalysisStatus = yes(optimal)
    ;
        pred_info_is_builtin(CalleePredInfo)
    ->
        Result = mm_tabled_will_not_call,
        MaybeAnalysisStatus = yes(optimal)
    ;
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
    ->
        % XXX user-defined uc
        Result = mm_tabled_may_call,
        MaybeAnalysisStatus = yes(optimal)
    ;
        % Handle normal calls.
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, intermodule_analysis, Intermod),
        (
            Intermod = yes,
            pred_info_is_imported_not_external(CalleePredInfo),
            not is_unify_or_compare_pred(CalleePredInfo)
        ->
            % Use the intermodule analysis framework if this is an imported
            % procedure and `--intermodule-analysis' is enabled.
            %
            search_analysis_status(CalleePPId, Result0, AnalysisStatus,
                !ModuleInfo),
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
        ;
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
%
% Utility procedure for processing goals
%

:- func get_mm_tabling_status_from_attributes(pragma_foreign_proc_attributes)
    = mm_tabling_status.

get_mm_tabling_status_from_attributes(Attributes) =
    (
        (
            get_may_call_mm_tabled(Attributes) = will_not_call_mm_tabled
        ;
            get_may_call_mm_tabled(Attributes) = default_calls_mm_tabled,
            get_may_call_mercury(Attributes) = proc_will_not_call_mercury
        )
    ->
        mm_tabled_will_not_call
    ;
        mm_tabled_may_call
    ).

%----------------------------------------------------------------------------%
%
% Additional code for handling calls
%

:- pred check_call_for_mm_tabling_calls(module_info::in, vartypes::in,
    pred_proc_id::in, prog_vars::in, maybe(proc_mm_tabling_info)::out) is det.

check_call_for_mm_tabling_calls(ModuleInfo, _VarTypes, PPId, _CallArgs,
        MaybeResult) :-
    module_info_get_mm_tabling_info(ModuleInfo, TablingInfo),
    ( map.search(TablingInfo, PPId, CalleeTablingInfo) ->
        MaybeResult = yes(CalleeTablingInfo)
        % XXX user-defined uc (and higher-order args too)
    ;
        MaybeResult = no
    ).

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

:- pred combine_maybe_analysis_status(maybe(analysis_status)::in,
    maybe(analysis_status)::in, maybe(analysis_status)::out) is det.

combine_maybe_analysis_status(MaybeStatusA, MaybeStatusB, MaybeStatus) :-
    (
        MaybeStatusA = yes(StatusA),
        MaybeStatusB = yes(StatusB)
    ->
        MaybeStatus = yes(analysis.lub(StatusA, StatusB))
    ;
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
:- pred annotate_proc(pred_proc_id::in, module_info::in, module_info::out)
    is det.

annotate_proc(PPId, !ModuleInfo) :-
    some [!ProcInfo, !Body] (
      module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, !:ProcInfo),
      proc_info_get_goal(!.ProcInfo, !:Body),
      proc_info_get_vartypes(!.ProcInfo, VarTypes),
      annotate_goal(VarTypes, !Body, _Status, !ModuleInfo),
      proc_info_set_goal(!.Body, !ProcInfo),
      module_info_set_pred_proc_info(PPId, PredInfo, !.ProcInfo, !ModuleInfo)
    ).

:- pred annotate_goal(vartypes::in, hlds_goal::in, hlds_goal::out,
    mm_tabling_status::out, module_info::in, module_info::out) is det.

annotate_goal(VarTypes, !Goal, Status, !ModuleInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    annotate_goal_2(VarTypes, GoalExpr0, GoalExpr, Status, !ModuleInfo),
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

:- pred annotate_goal_2(vartypes::in, hlds_goal_expr::in, hlds_goal_expr::out,
    mm_tabling_status::out, module_info::in, module_info::out) is det.

annotate_goal_2(VarTypes, !GoalExpr, Status, !ModuleInfo) :-
    (
        !.GoalExpr = unify(_, _, _, Kind, _),
        (
            Kind = complicated_unify(_, _, _),
            unexpected($module, $pred, "complicated unify")
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
        annotate_call(CalleePPId, CallArgs, VarTypes, Status, !ModuleInfo)
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
        annotate_goal_list(VarTypes, Conjuncts0, Conjuncts, Status,
            !ModuleInfo),
        !:GoalExpr = conj(ConjType, Conjuncts)
    ;
        !.GoalExpr = disj(Disjuncts0),
        annotate_goal_list(VarTypes, Disjuncts0, Disjuncts, Status,
            !ModuleInfo),
        !:GoalExpr = disj(Disjuncts)
    ;
        !.GoalExpr = switch(Var, CanFail, Cases0),
        annotate_cases(VarTypes, Cases0, Cases, Status, !ModuleInfo),
        !:GoalExpr = switch(Var, CanFail, Cases)
    ;
        !.GoalExpr = if_then_else(Vars, Cond0, Then0, Else0),
        annotate_goal(VarTypes, Cond0, Cond, CondStatus, !ModuleInfo),
        annotate_goal(VarTypes, Then0, Then, ThenStatus, !ModuleInfo),
        annotate_goal(VarTypes, Else0, Else, ElseStatus, !ModuleInfo),
        (
            CondStatus = mm_tabled_will_not_call,
            ThenStatus = mm_tabled_will_not_call,
            ElseStatus = mm_tabled_will_not_call
        ->
            Status = mm_tabled_will_not_call
        ;
            Status = mm_tabled_may_call
        ),
        !:GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        !.GoalExpr = negation(SubGoal0),
        annotate_goal(VarTypes, SubGoal0, SubGoal, Status, !ModuleInfo),
        !:GoalExpr = negation(SubGoal)
    ;
        !.GoalExpr = scope(Reason, SubGoal0),
        (
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        ->
            Status = mm_tabled_will_not_call
        ;
            annotate_goal(VarTypes, SubGoal0, SubGoal, Status, !ModuleInfo),
            !:GoalExpr = scope(Reason, SubGoal)
        )
    ;
        !.GoalExpr = shorthand(_),
        unexpected($module, $pred, "shorthand goal")
    ).

:- pred annotate_goal_list(vartypes::in, hlds_goals::in, hlds_goals::out,
    mm_tabling_status::out, module_info::in, module_info::out) is det.

annotate_goal_list(VarTypes, !Goals, Status, !ModuleInfo) :-
    list.map2_foldl(annotate_goal(VarTypes), !Goals, Statuses, !ModuleInfo),
    list.foldl(combine_mm_tabling_status, Statuses, mm_tabled_will_not_call,
        Status).

:- pred annotate_cases(vartypes::in, list(case)::in, list(case)::out,
    mm_tabling_status::out, module_info::in, module_info::out) is det.

annotate_cases(VarTypes, !Cases, Status, !ModuleInfo) :-
    list.map2_foldl(annotate_case(VarTypes), !Cases, Statuses, !ModuleInfo),
    list.foldl(combine_mm_tabling_status, Statuses, mm_tabled_will_not_call,
        Status).

:- pred annotate_case(vartypes::in, case::in, case::out,
    mm_tabling_status::out, module_info::in, module_info::out)
    is det.

annotate_case(VarTypes, !Case, Status, !ModuleInfo) :-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    annotate_goal(VarTypes, Goal0, Goal, Status, !ModuleInfo),
    !:Case = case(MainConsId, OtherConsIds, Goal).

:- pred annotate_call(pred_proc_id::in, prog_vars::in, vartypes::in,
    mm_tabling_status::out, module_info::in, module_info::out) is det.

annotate_call(CalleePPId, CallArgs, VarTypes, Status, !ModuleInfo) :-
    CalleePPId = proc(CalleePredId, _),
    module_info_pred_info(!.ModuleInfo, CalleePredId, CalleePredInfo),
    (
        pred_info_is_builtin(CalleePredInfo)
    ->
        Status = mm_tabled_will_not_call
    ;
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
    ->
        % XXX user-defined uc
        Status = mm_tabled_may_call
    ;
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        (
            IntermodAnalysis = yes,
            pred_info_is_imported_not_external(CalleePredInfo)
        ->
            search_analysis_status(CalleePPId, Result, AnalysisStatus,
                !ModuleInfo),
            (
                AnalysisStatus = invalid,
                unexpected($module, $pred,
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
        ;
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

%----------------------------------------------------------------------------%
%
% Stuff for intermodule optimization.
%

:- pred make_opt_int(module_info::in, io::di, io::uo) is det.

make_opt_int(ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, ModuleName, ".opt.tmp",
        do_not_create_dirs, OptFileName, !IO),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(Verbose, "% Appending mm_tabling_info pragmas to `",
        !IO),
    maybe_write_string(Verbose, OptFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        module_info_get_mm_tabling_info(ModuleInfo, TablingInfo),
        module_info_get_valid_predids(PredIds, ModuleInfo, _ModuleInfo),
        list.foldl(write_pragma_mm_tabling_info(ModuleInfo, TablingInfo),
            PredIds, !IO),
        io.set_output_stream(OldStream, _, !IO),
        io.close_output(OptFile, !IO),
        maybe_write_string(Verbose, " done.\n", !IO)
    ;
        OptFileRes = error(IOError),
        maybe_write_string(Verbose, " failed!\n", !IO),
        io.error_message(IOError, IOErrorMessage),
        io.write_strings(["Error opening file `",
            OptFileName, "' for output: ", IOErrorMessage], !IO),
        io.set_exit_status(1, !IO)
    ).

write_pragma_mm_tabling_info(ModuleInfo, TablingInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    list.foldl(
        write_pragma_mm_tabling_info_2(ModuleInfo, TablingInfo, PredId,
            PredInfo),
        ProcIds, !IO).

:- pred write_pragma_mm_tabling_info_2(module_info::in, mm_tabling_info::in,
    pred_id::in, pred_info::in, proc_id::in, io::di, io::uo) is det.

write_pragma_mm_tabling_info_2(ModuleInfo, TablingInfo, PredId, PredInfo,
        ProcId, !IO) :-
    should_write_mm_tabling_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_pragma, ShouldWrite),
    (
        ShouldWrite  = yes,
        ModuleName   = pred_info_module(PredInfo),
        Name         = pred_info_name(PredInfo),
        Arity        = pred_info_orig_arity(PredInfo),
        PredOrFunc   = pred_info_is_pred_or_func(PredInfo),
        proc_id_to_int(ProcId, ModeNum),
        (
            map.search(TablingInfo, proc(PredId, ProcId), ProcTablingInfo),
            ProcTablingInfo = proc_mm_tabling_info(Status, _)
        ->
            PredSymName = qualified(ModuleName, Name),
            PredNameArityPFMn = pred_name_arity_pf_mn(PredSymName, Arity,
                PredOrFunc, ModeNum),
            MMTablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
                Status),
            mercury_output_pragma_mm_tabling_info(MMTablingInfo, !IO)
        ;
            true
        )
    ;
        ShouldWrite = no
    ).

:- type should_write_for
    --->    for_analysis_framework
    ;       for_pragma.

:- pred should_write_mm_tabling_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, bool::out) is det.

should_write_mm_tabling_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    (
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_or_compare_pred(PredInfo),
        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds),
            %
            % XXX Writing out pragmas for the automatically generated class
            % instance methods causes the compiler to abort when it reads them
            % back in.
            %
            pred_info_get_markers(PredInfo, Markers),
            not check_marker(Markers, marker_class_instance_method),
            not check_marker(Markers, marker_named_class_instance_method)
        )
    ->
        ShouldWrite = yes
    ;
        ShouldWrite = no
    ).

%-----------------------------------------------------------------------------%
%
% Stuff for the intermodule analysis framework.
%

:- type mm_tabling_analysis_answer
    --->    mm_tabling_analysis_answer(mm_tabling_status).

:- func analysis_name = string.

analysis_name = "mm_tabling_analysis".

:- instance analysis(no_func_info, any_call, mm_tabling_analysis_answer) where
[
    analysis_name(_, _) = analysis_name,
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

:- pred search_analysis_status(pred_proc_id::in, mm_tabling_status::out,
    analysis_status::out, module_info::in, module_info::out) is det.

search_analysis_status(PPId, Result, AnalysisStatus, !ModuleInfo) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    search_analysis_status_2(!.ModuleInfo, PPId, Result, AnalysisStatus,
        AnalysisInfo0, AnalysisInfo),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred search_analysis_status_2(module_info::in, pred_proc_id::in,
    mm_tabling_status::out, analysis_status::out,
    analysis_info::in, analysis_info::out) is det.

search_analysis_status_2(ModuleInfo, PPId, Result, AnalysisStatus,
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
        record_request(analysis_name, ModuleName, FuncId, Call, !AnalysisInfo)
    ).

:- pred maybe_record_mm_tabling_result(module_info::in, pred_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_mm_tabling_result(ModuleInfo, PredId, !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    list.foldl(maybe_record_mm_tabling_result_2(ModuleInfo, PredId, PredInfo),
        ProcIds, !AnalysisInfo).

:- pred maybe_record_mm_tabling_result_2(module_info::in, pred_id::in,
    pred_info::in, proc_id::in, analysis_info::in, analysis_info::out) is det.

maybe_record_mm_tabling_result_2(ModuleInfo, PredId, PredInfo, ProcId,
        !AnalysisInfo) :-
    should_write_mm_tabling_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_analysis_framework, ShouldWrite),
    (
        ShouldWrite = yes,
        PPId = proc(PredId, ProcId),
        module_info_get_mm_tabling_info(ModuleInfo, TablingInfo),
        lookup_proc_mm_tabling_info(TablingInfo, PPId, Status, ResultStatus),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, any_call,
            mm_tabling_analysis_answer(Status), ResultStatus, !AnalysisInfo)
    ;
        ShouldWrite = no
    ).

:- pred lookup_proc_mm_tabling_info(mm_tabling_info::in, pred_proc_id::in,
    mm_tabling_status::out, analysis_status::out) is det.

lookup_proc_mm_tabling_info(TablingInfo, PPId, Status, ResultStatus) :-
    ( map.search(TablingInfo, PPId, ProcTablingInfo) ->
        ProcTablingInfo = proc_mm_tabling_info(Status, MaybeResultStatus),
        (
            MaybeResultStatus = yes(ResultStatus)
        ;
            MaybeResultStatus = no,
            unexpected($module, $pred, "no result status")
        )
    ;
        % Probably an exported `:- external' procedure.
        Status = mm_tabled_may_call,
        ResultStatus = optimal
    ).

%----------------------------------------------------------------------------%
%
% Code for printing out debugging traces.
%

:- pred dump_mm_tabling_analysis_debug_info(module_info::in, scc::in,
    mm_tabling_status::in, io::di, io::uo) is det.

dump_mm_tabling_analysis_debug_info(ModuleInfo, SCC, Status, !IO) :-
    io.write_string("SCC: ", !IO),
    io.write(Status, !IO),
    io.nl(!IO),
    output_proc_names(ModuleInfo, SCC, !IO),
    io.nl(!IO).

:- pred output_proc_names(module_info::in, scc::in, io::di, io::uo) is det.

output_proc_names(ModuleInfo, SCC, !IO) :-
    list.foldl(output_proc_name(ModuleInfo), SCC, !IO).

:- pred output_proc_name(module_info::in, pred_proc_id::in, io::di, io::uo)
    is det.

output_proc_name(Moduleinfo, PPId, !IO) :-
   Pieces = describe_one_proc_name(Moduleinfo, should_module_qualify, PPId),
   Str = error_pieces_to_string(Pieces),
   io.format("\t%s\n", [s(Str)], !IO).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.tabling_analysis.
%----------------------------------------------------------------------------%
