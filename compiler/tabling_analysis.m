%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
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
:- instance analysis(any_call, mm_tabling_analysis_answer).
:- instance partial_order(mm_tabling_analysis_answer).
:- instance answer_pattern(mm_tabling_analysis_answer).
:- instance to_string(mm_tabling_analysis_answer).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.
:- import_module hlds.passes_aux.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.dependency_graph.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.

%----------------------------------------------------------------------------%
%
% Perform minimal model tabling analysis on a module
%

analyse_mm_tabling_in_module(!ModuleInfo, !IO) :-
    globals.io_lookup_bool_option(use_minimal_model_stack_copy,
        UseMinimalModel, !IO),
    (
        % Only run the analysis in .mm grades.
        UseMinimalModel = yes,
        globals.io_lookup_bool_option(make_optimization_interface,
            MakeOptInt, !IO),
        globals.io_lookup_bool_option(make_transitive_opt_interface,
            MakeTransOptInt, !IO),
        globals.io_lookup_bool_option(make_analysis_registry,
            MakeAnalysisReg, !IO),
        Pass1Only = MakeOptInt `bool.or` MakeTransOptInt
            `bool.or` MakeAnalysisReg,
        module_info_ensure_dependency_info(!ModuleInfo),
        module_info_dependency_info(!.ModuleInfo, DepInfo),
        hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
        globals.io_lookup_bool_option(debug_mm_tabling_analysis, Debug, !IO),
        list.foldl2(analyse_mm_tabling_in_scc(Debug, Pass1Only), SCCs,
            !ModuleInfo, !IO),
        (
            MakeOptInt = yes,
            make_opt_int(!.ModuleInfo, !IO)
        ;
            MakeOptInt = no
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
    module_info::in, module_info::out, io::di, io::uo) is det.

analyse_mm_tabling_in_scc(Debug, Pass1Only, SCC, !ModuleInfo, !IO) :-
    %
    % Begin by analysing each procedure in the SCC.
    %
    list.foldl3(check_proc_for_mm_tabling(SCC), SCC, [], ProcResults,
        !ModuleInfo, !IO),
    combine_individual_proc_results(ProcResults, TablingStatus,
        MaybeAnalysisStatus),
    %
    % Print out debugging information.
    %
    (
        Debug = yes,
        dump_mm_tabling_analysis_debug_info(!.ModuleInfo, SCC,
            TablingStatus, !IO)
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
    %
    % Record the analysis results for the intermodule analysis
    %
    globals.io_lookup_bool_option(make_analysis_registry,
        MakeAnalysisRegistry, !IO),
    (
        MakeAnalysisRegistry = yes,
        (
            MaybeAnalysisStatus = yes(AnalysisStatus),
            record_mm_tabling_analysis_results(TablingStatus, AnalysisStatus,
                SCC, !ModuleInfo)
        ;
            MaybeAnalysisStatus = no,
            unexpected(this_file, "process_scc: no analysis status")
        )
    ;
        MakeAnalysisRegistry = no
    ),
    (
        Pass1Only = no,
        list.foldl2(annotate_proc, SCC, !ModuleInfo, !IO)
    ;
        Pass1Only = yes
    ).

    % Examine how procedures interact with other procedures that are
    % mutually-recursive to them.
    %
:- pred combine_individual_proc_results(proc_results::in,
   mm_tabling_status::out, maybe(analysis_status)::out) is det.

combine_individual_proc_results([], _, _) :-
    unexpected(this_file, "Empty SCC during mm tabling analysis.").
combine_individual_proc_results(ProcResults @ [_ | _], SCC_Result,
        MaybeAnalysisStatus) :- 
    (
        % If none of the procedures calls tabled procedures or is conditional
        % then the SCC cannot call tabled procedures.
        all [ProcResult] list.member(ProcResult, ProcResults) =>
            ProcResult ^ status = mm_tabled_will_not_call
    ->
        SCC_Result = mm_tabled_will_not_call
    ;
        all [ProcResult] list.member(ProcResult, ProcResults) =>
                ProcResult ^ status \= mm_tabled_may_call,
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
    proc_results::out, module_info::in, module_info::out, io::di, io::uo)
    is det.

check_proc_for_mm_tabling(SCC, PPId, !Results, !ModuleInfo, !IO) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    ( EvalMethod = eval_minimal(_) ->
        Result = mm_tabled_may_call,
        MaybeAnalysisStatus = yes(optimal)
    ;
        proc_info_get_goal(ProcInfo, Body),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        check_goal_for_mm_tabling(SCC, VarTypes, Body, Result,
            MaybeAnalysisStatus, !ModuleInfo, !IO)
    ),
    list.cons(proc_result(PPId, Result, MaybeAnalysisStatus), !Results).

%----------------------------------------------------------------------------%
%
% Perform minimal model tabling analysis of a goal
%

:- pred check_goal_for_mm_tabling(scc::in, vartypes::in, hlds_goal::in,
    mm_tabling_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.  

check_goal_for_mm_tabling(SCC, VarTypes, Goal - GoalInfo, Result, MaybeStatus,
        !ModuleInfo, !IO) :-
    check_goal_for_mm_tabling_2(SCC, VarTypes, Goal, GoalInfo, Result,
        MaybeStatus, !ModuleInfo, !IO).

:- pred check_goal_for_mm_tabling_2(scc::in, vartypes::in, hlds_goal_expr::in,
    hlds_goal_info::in, mm_tabling_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_goal_for_mm_tabling_2(_, _, Goal, _, mm_tabled_will_not_call,
        yes(optimal), !ModuleInfo, !IO) :-
    Goal = unify(_, _, _, Kind, _),
    ( Kind = complicated_unify(_, _, _) ->
        unexpected(this_file, "complicated unify during mm tabling analysis.")
    ;
        true
    ).
check_goal_for_mm_tabling_2(SCC, VarTypes, Goal, _, Result,
        MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    Goal = plain_call(CalleePredId, CalleeProcId, CallArgs, _, _, _),
    CalleePPId = proc(CalleePredId, CalleeProcId),
    check_call_for_mm_tabling(CalleePPId, CallArgs, SCC, VarTypes, Result,
        MaybeAnalysisStatus, !ModuleInfo, !IO).
check_goal_for_mm_tabling_2(_, _VarTypes, Goal, _GoalInfo, Result,
        MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    Goal = generic_call(Details, _Args, _ArgModes, _),
    check_generic_call_for_mm_tabling(Details, Result, MaybeAnalysisStatus,
        !ModuleInfo, !IO).
check_goal_for_mm_tabling_2(SCC, VarTypes, negation(Goal), _, Result,
        MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    check_goal_for_mm_tabling(SCC, VarTypes, Goal, Result,
        MaybeAnalysisStatus, !ModuleInfo, !IO).
check_goal_for_mm_tabling_2(SCC, VarTypes, Goal, _, Result,
        MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    Goal = scope(_, InnerGoal),
    check_goal_for_mm_tabling(SCC, VarTypes, InnerGoal, Result,
        MaybeAnalysisStatus, !ModuleInfo, !IO).
check_goal_for_mm_tabling_2(_, _, Goal, _, Result, MaybeAnalysisStatus,
        !ModuleInfo, !IO) :-
    Goal = call_foreign_proc(Attributes, _, _, _, _, _, _),
    Result = get_mm_tabling_status_from_attributes(Attributes),
    MaybeAnalysisStatus = yes(optimal).
check_goal_for_mm_tabling_2(_, _, shorthand(_), _, _, _, !ModuleInfo, !IO) :-
    unexpected(this_file,
        "shorthand goal encountered during mm tabling analysis.").
check_goal_for_mm_tabling_2(SCC, VarTypes, Goal, _,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    (
        Goal = conj(_, Goals)
    ;
        Goal = disj(Goals)
    ;
        Goal = if_then_else(_, If, Then, Else),
        Goals = [If, Then, Else]
    ;
        Goal = switch(_, _, Cases),
        Goals = list.map((func(case(_, CaseGoal)) = CaseGoal), Cases)
    ),
    check_goals_for_mm_tabling(SCC, VarTypes, Goals, Result,
        MaybeAnalysisStatus, !ModuleInfo, !IO).

:- pred check_goals_for_mm_tabling(scc::in, vartypes::in,
    hlds_goals::in, mm_tabling_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_goals_for_mm_tabling(SCC, VarTypes, Goals, Result, MaybeAnalysisStatus,
        !ModuleInfo, !IO) :-
    list.map2_foldl2(check_goal_for_mm_tabling(SCC, VarTypes), Goals,
        Results, MaybeAnalysisStatuses, !ModuleInfo, !IO),
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
    module_info::in, module_info::out, io::di, io::uo) is det.

check_call_for_mm_tabling(CalleePPId @ proc(CalleePredId, _),
        CallArgs, SCC, VarTypes, Result, MaybeAnalysisStatus, !ModuleInfo,
        !IO) :-
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
        % NOTE: the type specific unify and compare predicates are just
        %       treated as though they were normal predicates.
        %
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
        globals.io_lookup_bool_option(intermodule_analysis, Intermod, !IO),
        (
            Intermod = yes,
            pred_info_is_imported(CalleePredInfo)
        ->
            % Use the intermodule analysis framework if this is an imported
            % procedure and `--intermodule-analysis' is enabled.
            % 
            search_analysis_status(CalleePPId, Result0, AnalysisStatus, SCC,
                !ModuleInfo, !IO),
            ( Result0 = mm_tabled_conditional  ->
                % XXX user-defined uc
                Result = mm_tabled_will_not_call
            ;
                Result = Result0
            ),
            MaybeAnalysisStatus = yes(AnalysisStatus)
        ;
            % Otherwise, the information (if we have any) will be in the
            % mm_tabling_info table.
            %
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
                    MaybeAnalysisStatus = yes(suboptimal)
                ;
                    Intermod = no,
                    MaybeAnalysisStatus = no
                )
            )
        )
    ).

%----------------------------------------------------------------------------%
%
% Code for checking generic calls
%

:- pred check_generic_call_for_mm_tabling(generic_call::in,
    mm_tabling_status::out, maybe(analysis_status)::out, module_info::in,
    module_info::out, io::di, io::uo) is det.

check_generic_call_for_mm_tabling(Details, Result, MaybeAnalysisStatus,
        !ModuleInfo, !IO) :-
    (
        % XXX use results of closure analysis here.
        Details = higher_order(_Var, _, _, _),
        Result  = mm_tabled_may_call,
        MaybeAnalysisStatus = yes(optimal)
    ;
        Details = class_method(_, _, _, _),
        Result  = mm_tabled_may_call,
        MaybeAnalysisStatus = yes(optimal)
    ;
        Details = cast(_),
        Result = mm_tabled_will_not_call,
        MaybeAnalysisStatus = yes(optimal)
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
            may_call_mm_tabled(Attributes) = will_not_call_mm_tabled
        ;
            may_call_mm_tabled(Attributes) = default_calls_mm_tabled,
            may_call_mercury(Attributes) = will_not_call_mercury
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
combine_mm_tabling_status(mm_tabled_may_call, _,
        mm_tabled_may_call).
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
:- pred annotate_proc(pred_proc_id::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

annotate_proc(PPId, !ModuleInfo, !IO) :-
    some [!ProcInfo, !Body] (
      module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, !:ProcInfo),
      proc_info_get_goal(!.ProcInfo, !:Body),
      proc_info_get_vartypes(!.ProcInfo, VarTypes),
      annotate_goal(VarTypes, !Body, _Status, !ModuleInfo, !IO), 
      proc_info_set_goal(!.Body, !ProcInfo),
      module_info_set_pred_proc_info(PPId, PredInfo, !.ProcInfo, !ModuleInfo)
    ).

:- pred annotate_goal(vartypes::in, hlds_goal::in, hlds_goal::out,
    mm_tabling_status::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

annotate_goal(VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = GoalExpr0 - GoalInfo0,
    annotate_goal_2(VarTypes, GoalExpr0, GoalExpr, Status, !ModuleInfo, !IO),
    ( Status = mm_tabled_will_not_call ->
        goal_info_add_feature(will_not_call_mm_tabled, GoalInfo0, GoalInfo)
    ;
        GoalInfo = GoalInfo0
    ),
    !:Goal = GoalExpr - GoalInfo.
        
:- pred annotate_goal_2(vartypes::in, hlds_goal_expr::in, hlds_goal_expr::out,
    mm_tabling_status::out, module_info::in, module_info::out, io::di, io::uo)
    is det.

annotate_goal_2(VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = conj(ConjType, Conjuncts0),
    annotate_goal_list(VarTypes, Conjuncts0, Conjuncts, Status, !ModuleInfo,
        !IO),
    !:Goal = conj(ConjType, Conjuncts). 
annotate_goal_2(VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = plain_call(CalleePredId, CalleeProcId, CallArgs, _, _, _),
    CalleePPId = proc(CalleePredId, CalleeProcId),
    annotate_call(CalleePPId, CallArgs, VarTypes, Status, !ModuleInfo, !IO).
annotate_goal_2(_VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = generic_call(Details, _Args, _Modes, _Detism),
    annotate_generic_call(Details, Status, !ModuleInfo, !IO).
annotate_goal_2(VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = switch(Var, CanFail, Cases0),
    annotate_cases(VarTypes, Cases0, Cases, Status, !ModuleInfo, !IO),
    !:Goal = switch(Var, CanFail, Cases).
annotate_goal_2(_VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = unify(_, _, _, Kind, _),
    ( Kind = complicated_unify(_, _, _) ->
        unexpected(this_file, "complicated unify during tabling analysis.")
    ;
        true
    ),
    Status = mm_tabled_will_not_call.
annotate_goal_2(VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = disj(Disjuncts0),
    annotate_goal_list(VarTypes, Disjuncts0, Disjuncts, Status, !ModuleInfo,
        !IO),
    !:Goal = disj(Disjuncts).
annotate_goal_2(VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = negation(NegGoal0),
    annotate_goal(VarTypes, NegGoal0, NegGoal, Status, !ModuleInfo, !IO),
    !:Goal = negation(NegGoal).
annotate_goal_2(VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = scope(Reason, InnerGoal0),
    annotate_goal(VarTypes, InnerGoal0, InnerGoal, Status, !ModuleInfo, !IO),
    !:Goal = scope(Reason, InnerGoal).
annotate_goal_2(VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = if_then_else(Vars, If0, Then0, Else0),
    annotate_goal(VarTypes, If0, If, IfStatus, !ModuleInfo, !IO),
    annotate_goal(VarTypes, Then0, Then, ThenStatus, !ModuleInfo, !IO),
    annotate_goal(VarTypes, Else0, Else, ElseStatus, !ModuleInfo, !IO),
    (
        IfStatus   = mm_tabled_will_not_call,
        ThenStatus = mm_tabled_will_not_call,
        ElseStatus = mm_tabled_will_not_call
    ->
        Status = mm_tabled_will_not_call
    ;
        Status = mm_tabled_may_call
    ), 
    !:Goal = if_then_else(Vars, If, Then, Else).
annotate_goal_2(_, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = call_foreign_proc(Attributes, _, _, _, _, _, _),
    Status = get_mm_tabling_status_from_attributes(Attributes).
annotate_goal_2(_, shorthand(_), _, _, _, _, _, _) :-
    unexpected(this_file, "shorthand goal").
    
:- pred annotate_goal_list(vartypes::in, hlds_goals::in, hlds_goals::out,
    mm_tabling_status::out, module_info::in, module_info::out, io::di, io::uo)
    is det.

annotate_goal_list(VarTypes, !Goals, Status, !ModuleInfo, !IO) :-
    list.map2_foldl2(annotate_goal(VarTypes), !Goals, Statuses, !ModuleInfo,
        !IO), 
    list.foldl(combine_mm_tabling_status, Statuses, mm_tabled_will_not_call,
        Status).
    
:- pred annotate_cases(vartypes::in, list(case)::in, list(case)::out,
    mm_tabling_status::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

annotate_cases(VarTypes, !Cases, Status, !ModuleInfo, !IO) :-
    list.map2_foldl2(annotate_case(VarTypes), !Cases, Statuses, !ModuleInfo,
        !IO),
    list.foldl(combine_mm_tabling_status, Statuses, mm_tabled_will_not_call,
        Status).

:- pred annotate_case(vartypes::in, case::in, case::out,
    mm_tabling_status::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

annotate_case(VarTypes, !Case, Status, !ModuleInfo, !IO) :-
    !.Case = case(ConsId, Goal0),
    annotate_goal(VarTypes, Goal0, Goal, Status, !ModuleInfo, !IO),
    !:Case = case(ConsId, Goal).

:- pred annotate_call(pred_proc_id::in, prog_vars::in, vartypes::in,
    mm_tabling_status::out, module_info::in, module_info::out, io::di, io::uo)
    is det.

annotate_call(CalleePPId @ proc(CalleePredId, _), CallArgs, VarTypes, Status,
        !ModuleInfo, !IO) :-
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
        globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis,
            !IO),
        (
            IntermodAnalysis = yes,
            pred_info_is_imported(CalleePredInfo)
        ->  
            % NOTE: we set the value of SCC to a dummy value here.
            % This is okay because it only needs a meaningful value when
            % building the analysis files; it won't be used when compiling to
            % target code.
            SCC = [],   
            search_analysis_status(CalleePPId, Result, AnalysisStatus, SCC,
                !ModuleInfo, !IO),
            
            ( AnalysisStatus = invalid ->
                unexpected(this_file,
                    "invalid analysis result while annotating goals")
            ;
                % XXX user-defined uc
                ( Result = mm_tabled_conditional ->
                    Status = mm_tabled_will_not_call
                ;
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

:- pred annotate_generic_call(generic_call::in, mm_tabling_status::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

annotate_generic_call(GenericCall, Status, !ModuleInfo, !IO) :-
    (
        % XXX use results of closure analysis here.
        GenericCall = higher_order(_Var, _, _, _),
        Status = mm_tabled_may_call
    ;
        GenericCall = class_method(_, _, _, _),
        Status = mm_tabled_may_call
    ;     
        GenericCall = cast(_),
        Status = mm_tabled_will_not_call
    ).

%----------------------------------------------------------------------------%
%
% Stuff for intermodule optimization
% 

:- pred make_opt_int(module_info::in, io::di, io::uo) is det.

make_opt_int(ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(ModuleName, ".opt.tmp", no, OptFileName, !IO),
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
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
        module_info_predids(ModuleInfo, PredIds),   
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
    should_write_mm_tabling_info(ModuleInfo, PredId, PredInfo, ShouldWrite),
    (   
        ShouldWrite  = yes,
        ModuleName   = pred_info_module(PredInfo),
        Name         = pred_info_name(PredInfo),
        Arity        = pred_info_orig_arity(PredInfo),
        PredOrFunc   = pred_info_is_pred_or_func(PredInfo),
        ProcIds      = pred_info_procids(PredInfo),
        OutputPragma = (pred(ProcId::in, !.IO::di, !:IO::uo) is det :-
            proc_id_to_int(ProcId, ModeNum),
            ( 
                map.search(TablingInfo, proc(PredId, ProcId), ProcTablingInfo),
                ProcTablingInfo = proc_mm_tabling_info(Status, _)
            ->
                mercury_output_pragma_mm_tabling_info(PredOrFunc, 
                    qualified(ModuleName, Name), Arity, ModeNum, Status, !IO)
            ;
                true
            )
        ),
        list.foldl(OutputPragma, ProcIds, !IO)
    ;
        ShouldWrite = no
    ).          

:- pred should_write_mm_tabling_info(module_info::in, pred_id::in, 
    pred_info::in, bool::out) is det.

should_write_mm_tabling_info(ModuleInfo, PredId, PredInfo, ShouldWrite) :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    (   
        ( ImportStatus = exported 
        ; ImportStatus = opt_exported 
        ),
        not is_unify_or_compare_pred(PredInfo),
        module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
        not set.member(PredId, TypeSpecForcePreds),
        %
        % XXX Writing out pragmas for the automatically generated class
        % instance methods causes the compiler to abort when it reads them
        % back in.
        %
        pred_info_get_markers(PredInfo, Markers),
        not check_marker(Markers, class_instance_method),
        not check_marker(Markers, named_class_instance_method)
    ->
        ShouldWrite = yes
    ;
        ShouldWrite = no
    ).          

%-----------------------------------------------------------------------------%
%
% Stuff for the intermodule analysis framework
%

:- type mm_tabling_analysis_answer
    --->    mm_tabling_analysis_answer(mm_tabling_status).

:- func analysis_name = string.

analysis_name = "mm_tabling_analysis".

:- instance analysis(any_call, mm_tabling_analysis_answer) where [
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 1,
    preferred_fixpoint_type(_, _) = least_fixpoint,
    bottom(_) = mm_tabling_analysis_answer(mm_tabled_will_not_call),
    top(_) = mm_tabling_analysis_answer(mm_tabled_may_call)
].

:- instance answer_pattern(mm_tabling_analysis_answer) where [].
:- instance partial_order(mm_tabling_analysis_answer) where [
    (more_precise_than(
            mm_tabling_analysis_answer(Status1),
            mm_tabling_analysis_answer(Status2)) :-
        mm_tabling_status_more_precise_than(Status1, Status2)),
    equivalent(Status, Status)
].

:- pred mm_tabling_status_more_precise_than(mm_tabling_status::in,
    mm_tabling_status::in) is semidet.

mm_tabling_status_more_precise_than(mm_tabled_will_not_call,
    mm_tabled_may_call).
mm_tabling_status_more_precise_than(mm_tabled_will_not_call,
    mm_tabled_conditional).
mm_tabling_status_more_precise_than(mm_tabled_conditional,
    mm_tabled_may_call).

:- instance to_string(mm_tabling_analysis_answer) where [
    func(to_string/1) is mm_tabling_analysis_answer_to_string,
    func(from_string/1) is mm_tabling_analysis_answer_from_string
].

:- func mm_tabling_analysis_answer_to_string(mm_tabling_analysis_answer)
    = string.

mm_tabling_analysis_answer_to_string(mm_tabling_analysis_answer(Status))
        = Str :-
    mm_tabling_status_to_string(Status, Str).

:- func mm_tabling_analysis_answer_from_string(string) =
    mm_tabling_analysis_answer is semidet.

mm_tabling_analysis_answer_from_string(Str)
        = mm_tabling_analysis_answer(Status) :-
    mm_tabling_status_to_string(Status, Str).

:- pred mm_tabling_status_to_string(mm_tabling_status, string).
:- mode mm_tabling_status_to_string(in, out) is det.
:- mode mm_tabling_status_to_string(out, in) is semidet.

mm_tabling_status_to_string(mm_tabled_may_call,
    "mm_tabled_may_call").
mm_tabling_status_to_string(mm_tabled_will_not_call,
    "mm_tabled_will_not_call").
mm_tabling_status_to_string(mm_tabled_conditional,
    "mm_tabled_conditional").

:- pred search_analysis_status(pred_proc_id::in,
        mm_tabling_status::out, analysis_status::out, scc::in,
        module_info::in, module_info::out, io::di, io::uo) is det.

search_analysis_status(PPId, Result, AnalysisStatus, CallerSCC,
        !ModuleInfo, !IO) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    search_analysis_status_2(!.ModuleInfo, PPId, Result, AnalysisStatus,
        CallerSCC, AnalysisInfo0, AnalysisInfo, !IO),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred search_analysis_status_2(module_info::in, pred_proc_id::in,
    mm_tabling_status::out, analysis_status::out, scc::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

search_analysis_status_2(ModuleInfo, PPId, Result, AnalysisStatus, CallerSCC,
        !AnalysisInfo, !IO) :-
    mmc_analysis.module_id_func_id(ModuleInfo, PPId, ModuleId, FuncId),
    Call = any_call,
    analysis.lookup_best_result(ModuleId, FuncId, Call,
        MaybeBestStatus, !AnalysisInfo, !IO),
    globals.io_lookup_bool_option(make_analysis_registry,
        MakeAnalysisRegistry, !IO),
    (
        MaybeBestStatus = yes({BestCall, mm_tabling_analysis_answer(Result),
            AnalysisStatus}),
        (
            MakeAnalysisRegistry = yes,
            record_dependencies(ModuleId, FuncId, BestCall,
                ModuleInfo, CallerSCC, !AnalysisInfo)
        ;
            MakeAnalysisRegistry = no
        )
    ;
        MaybeBestStatus = no,
        % If we do not have any information about the callee procedure
        % then assume that it modifies the calls a minimal model tabled
        % procedure.
        top(Call) = Answer,
        Answer = mm_tabling_analysis_answer(Result),
        module_is_local(mmc, ModuleId, IsLocal, !IO),
        (
            IsLocal = yes,
            AnalysisStatus = suboptimal,
            (
                MakeAnalysisRegistry = yes,
                analysis.record_result(ModuleId, FuncId, Call, Answer,
                    AnalysisStatus, !AnalysisInfo),
                analysis.record_request(analysis_name, ModuleId, FuncId, Call,
                    !AnalysisInfo),
                record_dependencies(ModuleId, FuncId, Call,
                    ModuleInfo, CallerSCC, !AnalysisInfo)
            ;
                MakeAnalysisRegistry = no
            )
        ;
            IsLocal = no,
            % We can't do any better anyway.
            AnalysisStatus = optimal
        )
    ).

    % XXX if the procedures in CallerSCC definitely come from the
    % same module then we don't need to record the dependency so many
    % times, at least while we only have module-level granularity.
    %
:- pred record_dependencies(module_id::in, func_id::in, Call::in,
    module_info::in, scc::in, analysis_info::in, analysis_info::out)
    is det <= call_pattern(Call).

record_dependencies(ModuleId, FuncId, Call, ModuleInfo, CallerSCC,
        !AnalysisInfo) :-
    RecordDependency = (pred(CallerPPId::in, Info0::in, Info::out) is det :-
        module_id_func_id(ModuleInfo, CallerPPId, CallerModuleId, _),
        record_dependency(CallerModuleId, analysis_name, ModuleId, FuncId,
            Call, Info0, Info)
    ),
    list.foldl(RecordDependency, CallerSCC, !AnalysisInfo).

:- pred record_mm_tabling_analysis_results(mm_tabling_status::in, 
    analysis_status::in, scc::in, module_info::in, module_info::out) is det.

record_mm_tabling_analysis_results(Status, ResultStatus, SCC, !ModuleInfo) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    list.foldl(
        record_mm_tabling_analysis_result(!.ModuleInfo, Status, ResultStatus),
        SCC, AnalysisInfo0, AnalysisInfo),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred record_mm_tabling_analysis_result(module_info::in,
    mm_tabling_status::in, analysis_status::in, pred_proc_id::in,
    analysis_info::in, analysis_info::out) is det.

record_mm_tabling_analysis_result(ModuleInfo, Status, ResultStatus,
        PPId @ proc(PredId, _ProcId), !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    should_write_mm_tabling_info(ModuleInfo, PredId, PredInfo, ShouldWrite),
    (   
        ShouldWrite = yes,
        mmc_analysis.module_id_func_id(ModuleInfo, PPId, ModuleId, FuncId),
        Answer = mm_tabling_analysis_answer(Status),
        record_result(ModuleId, FuncId, any_call, Answer, ResultStatus,
            !AnalysisInfo)
    ;
        ShouldWrite = no
    ).

%----------------------------------------------------------------------------%
%
% Code for printing out debugging traces
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

:- func this_file = string.

this_file = "tabling_analysis.m".

%----------------------------------------------------------------------------%
:- end_module tabling_analysis.
%----------------------------------------------------------------------------%
