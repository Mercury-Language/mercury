%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: trailing_analysis.m.
% Author: juliensf.
%
% This module implements trail usage analysis.  It annotates the HLDS with
% information about which procedures will not modify the trail.
%
% The compiler can use this information to omit redundant trailing operations
% in trailing grades.  After running the analysis the trailing status of each
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
%       depends upon the instantiation of the type variables.  We need
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
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

%----------------------------------------------------------------------------%

    % Perform trail usage analysis on a module.
    %
:- pred analyse_trail_usage(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Write out the trailing_info pragma for this module.
    %
:- pred write_pragma_trailing_info(module_info::in, trailing_info::in,
    pred_id::in, io::di, io::uo) is det.

    % Types and instances for the intermodule analysis framework.
    %
:- type trailing_analysis_answer.
:- instance analysis(any_call, trailing_analysis_answer).
:- instance partial_order(trailing_analysis_answer).
:- instance answer_pattern(trailing_analysis_answer).
:- instance to_string(trailing_analysis_answer).

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
% Perform trail usage analysis on a module
%

% The analysis is carried out in two passes.  Both passes do a bottom-up
% traversal of the callgraph, one SCC at a time.  For each SCC the first
% pass works out the trailing_status for each procedure in the SCC.  The
% second pass then uses this information to annotate the goals in each
% procedure with trail usage information.
%
% The second pass is only run if we are going to use the information,
% that is if we are generating code as opposed to building the optimization
% interfaces.

analyse_trail_usage(!ModuleInfo, !IO) :-
    globals.io_lookup_bool_option(use_trail, UseTrail, !IO),
    (
        % Only run the analysis in trailing grades.
        UseTrail = yes,
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
        globals.io_lookup_bool_option(debug_trail_usage, Debug, !IO),
        list.foldl2(process_scc(Debug, Pass1Only), SCCs, !ModuleInfo, !IO),
        (
            MakeOptInt = yes,
            make_opt_int(!.ModuleInfo, !IO)
        ;
            MakeOptInt = no
        )
    ;
        UseTrail = no
    ).
%----------------------------------------------------------------------------%
%
% Perform trail usage analysis on a SCC
%

:- type scc == list(pred_proc_id).

:- type proc_results == list(proc_result).

:- type proc_result
    --->    proc_result(
                ppid                    :: pred_proc_id,
                status                  :: trailing_status,
                maybe_analysis_status   :: maybe(analysis_status)
            ).

:- pred process_scc(bool::in, bool::in, scc::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

process_scc(Debug, Pass1Only, SCC, !ModuleInfo, !IO) :-
    check_procs_for_trail_mods(SCC, ProcResults, !ModuleInfo, !IO),
    %
    % The `Results' above are the results of analysing each individual
    % procedure in the SCC - we now have to combine them in a meaningful way.
    %
    combine_individual_proc_results(ProcResults,
        TrailingStatus, MaybeAnalysisStatus),
    %
    % Print out debugging information.
    %
    (
        Debug = yes,
        dump_trail_usage_debug_info(!.ModuleInfo, SCC, TrailingStatus, !IO)
    ;
        Debug = no
    ),
    %
    % Update the trailing_info with information about this SCC.
    %
    module_info_get_trailing_info(!.ModuleInfo, TrailingInfo0),
    Update = (pred(PPId::in, Info0::in, Info::out) is det :-
        Info = Info0 ^ elem(PPId) :=
            proc_trailing_info(TrailingStatus, MaybeAnalysisStatus)
    ),
    list.foldl(Update, SCC, TrailingInfo0, TrailingInfo),
    module_info_set_trailing_info(TrailingInfo, !ModuleInfo),
    %
    % Record the analysis results for the intermodule analysis
    %
    globals.io_lookup_bool_option(make_analysis_registry,
        MakeAnalysisRegistry, !IO),
    (
        MakeAnalysisRegistry = yes,
        (
            MaybeAnalysisStatus = yes(AnalysisStatus),
            record_trailing_analysis_results(TrailingStatus, AnalysisStatus,
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

    % Check each procedure in the SCC individually.
    %
:- pred check_procs_for_trail_mods(scc::in, proc_results::out,
        module_info::in, module_info::out, io::di, io::uo) is det.

check_procs_for_trail_mods(SCC, Result, !ModuleInfo, !IO) :-
    list.foldl3(check_proc_for_trail_mods(SCC), SCC, [], Result,
        !ModuleInfo, !IO).

    % Examine how the procedures interact with other procedures that
    % are mutually-recursive to them.
    %
:- pred combine_individual_proc_results(proc_results::in,
    trailing_status::out, maybe(analysis_status)::out) is det.

combine_individual_proc_results([], _, _) :-
    unexpected(this_file, "Empty SCC during trailing analysis.").
combine_individual_proc_results(ProcResults @ [_|_],
        SCC_Result, MaybeAnalysisStatus) :-
    (
        % If none of the procedures modifies the trail or is conditional then
        % the SCC cannot modify the trail.
        all [ProcResult] list.member(ProcResult, ProcResults) =>
            ProcResult ^ status = trail_will_not_modify
    ->
        SCC_Result = trail_will_not_modify
    ;
        all [EResult] list.member(EResult, ProcResults) =>
                EResult ^ status \= trail_may_modify,
        some [CResult] (
            list.member(CResult, ProcResults),
            CResult ^ status = trail_conditional
        )
    ->
        SCC_Result = trail_conditional
    ;
        % Otherwise the SCC may modify the trail.
        SCC_Result = trail_may_modify
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
% Perform trail usage analysis on a procedure
%

:- pred check_proc_for_trail_mods(scc::in,
    pred_proc_id::in, proc_results::in, proc_results::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_proc_for_trail_mods(SCC, PPId, !Results, !ModuleInfo, !IO) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_goal(ProcInfo, Body),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    check_goal_for_trail_mods(SCC, VarTypes, Body,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO),
    list.cons(proc_result(PPId, Result, MaybeAnalysisStatus), !Results).

%----------------------------------------------------------------------------%
%
% Perform trail usage analysis of a goal
%

:- pred check_goal_for_trail_mods(scc::in, vartypes::in, hlds_goal::in,
    trailing_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_goal_for_trail_mods(SCC, VarTypes, Goal - GoalInfo,
        Result, MaybeStatus, !ModuleInfo, !IO) :-
    check_goal_for_trail_mods_2(SCC, VarTypes, Goal, GoalInfo,
        Result, MaybeStatus, !ModuleInfo, !IO).

:- pred check_goal_for_trail_mods_2(scc::in,
    vartypes::in, hlds_goal_expr::in, hlds_goal_info::in,
    trailing_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_goal_for_trail_mods_2(_, _, Goal, _, trail_will_not_modify, yes(optimal),
        !ModuleInfo, !IO) :-
    Goal = unify(_, _, _, Kind, _),
    ( Kind = complicated_unify(_, _, _) ->
        unexpected(this_file, "complicated unify during trail usage analysis.")
    ;
        true
    ).
check_goal_for_trail_mods_2(SCC, VarTypes, Goal, _,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    Goal = plain_call(CallPredId, CallProcId, CallArgs, _, _, _),
    CallPPId = proc(CallPredId, CallProcId),
    module_info_pred_info(!.ModuleInfo, CallPredId, CallPredInfo),
    (
        % Handle (mutually-)recursive calls.
        list.member(CallPPId, SCC)
    ->
        Types = list.map((func(Var) = VarTypes ^ det_elem(Var)), CallArgs),
        TrailingStatus = check_types(!.ModuleInfo, Types),
        Result = TrailingStatus,
        MaybeAnalysisStatus = yes(optimal)
    ;
        pred_info_is_builtin(CallPredInfo)
    ->
        % There are no builtins that will modify the trail.
        Result = trail_will_not_modify,
        MaybeAnalysisStatus = yes(optimal)
    ;
        % Handle builtin unify and compare.
        % NOTE: the type specific unify and compare predicates are just
        % treated as though they were normal predicates.
        ModuleName = pred_info_module(CallPredInfo),
        any_mercury_builtin_module(ModuleName),
        Name = pred_info_name(CallPredInfo),
        Arity = pred_info_orig_arity(CallPredInfo),
        ( SpecialPredId = spec_pred_compare
        ; SpecialPredId = spec_pred_unify
        ),
        special_pred_name_arity(SpecialPredId, Name, _, Arity)
    ->
        % XXX We should examine the argument types of calls to builtin.unify/2
        % and builtin.compare/3 and then make a decision based on those.
        Result = trail_may_modify,
        MaybeAnalysisStatus = yes(optimal)
    ;
        % Handle library predicates whose trailing status
        % can be looked up in the known procedures table.
        pred_info_has_known_status(CallPredInfo, Result0)
    ->
        Result = Result0,
        MaybeAnalysisStatus = yes(optimal)
    ;
        globals.io_lookup_bool_option(intermodule_analysis, Intermod, !IO),
        (
            Intermod = yes,
            pred_info_is_imported(CallPredInfo)
        ->
            % With --intermodule-analysis use check_call_2 to look up results
            % for locally defined procedures, otherwise we use the intermodule
            % analysis framework.
            search_analysis_status(CallPPId, Result0, AnalysisStatus, SCC,
                !ModuleInfo, !IO),
            ( Result0 = trail_conditional ->
                Result = check_vars(!.ModuleInfo, VarTypes, CallArgs)
            ;
                Result = Result0
            ),
            MaybeAnalysisStatus = yes(AnalysisStatus)
        ;
            check_call_2(!.ModuleInfo, VarTypes, CallPPId, CallArgs,
                MaybeResult),
            (
                MaybeResult = yes(proc_trailing_info(Result,
                    MaybeAnalysisStatus))
            ;
                MaybeResult = no,
                % If we do not have any information about the callee procedure
                % then assume that it modifies the trail.
                Result = trail_may_modify,
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

check_goal_for_trail_mods_2(_, _VarTypes, Goal, _GoalInfo,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    Goal = generic_call(Details, _Args, _ArgModes, _),
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
        Details = cast(_),
        Result  = trail_will_not_modify,
        MaybeAnalysisStatus = yes(optimal)
    ).
check_goal_for_trail_mods_2(SCC, VarTypes, negation(Goal), _,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    check_goal_for_trail_mods(SCC, VarTypes, Goal, Result, MaybeAnalysisStatus,
        !ModuleInfo, !IO).
check_goal_for_trail_mods_2(SCC, VarTypes, Goal, OuterGoalInfo,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    Goal = scope(_, InnerGoal),
    check_goal_for_trail_mods(SCC, VarTypes, InnerGoal, Result0,
        MaybeAnalysisStatus, !ModuleInfo, !IO),
    InnerGoal = _ - InnerGoalInfo,
    goal_info_get_code_model(InnerGoalInfo, InnerCodeModel),
    goal_info_get_code_model(OuterGoalInfo, OuterCodeModel),
    %
    % `trail_conditional' scope goals (of the type that require extra trailing
    % code) will have there status changed to `trail_may_modify'.
    % See the comment in the clause that handles if-then-elses below
    % for the reason why.
    %
    Result = scope_implies_trail_mod(InnerCodeModel, OuterCodeModel, Result0).
check_goal_for_trail_mods_2(_, _, Goal, _, Result, MaybeAnalysisStatus,
        !ModuleInfo, !IO) :-
    Goal = call_foreign_proc(Attributes, _, _, _, _, _, _),
    Result = attributes_imply_trail_mod(Attributes),
    MaybeAnalysisStatus = yes(optimal).
check_goal_for_trail_mods_2(_, _, shorthand(_), _, _, _, !ModuleInfo, !IO) :-
    unexpected(this_file,
        "shorthand goal encountered during trail usage analysis.").
check_goal_for_trail_mods_2(SCC, VarTypes, Goal, _,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    Goal = switch(_, _, Cases),
    CaseGoals = list.map((func(case(_, CaseGoal)) = CaseGoal), Cases),
    check_goals_for_trail_mods(SCC, VarTypes, CaseGoals,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO).
check_goal_for_trail_mods_2(SCC, VarTypes, Goal, _,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    Goal = if_then_else(_, If, Then, Else),
    check_goals_for_trail_mods(SCC, VarTypes, [If, Then, Else],
        Result0, MaybeAnalysisStatus, !ModuleInfo, !IO),
    (
        % If none of the disjuncts can modify the trail then we don't need
        % to emit trailing code around this disjunction.
        Result0 = trail_will_not_modify,
        Result  = trail_will_not_modify
    ;
        % If some of the goals modify the trail then we need to emit trailing
        % code around the if-then-else.  If the if-then-else has status
        % `trail_conditional' then we also need to emit trailing code around it
        % because we cannot be sure that calls to builtin.{unify,compare}
        % won't call user-defined equality/comparison predicates that modify
        % the trail.
        %
        % XXX We change the status from `trail_conditional' to
        % `trail_may_modify' here because `trail_conditional' currently means
        % that the code for a procedure does not modify the state of the trail
        % at all.  Since we emit trailing code for this if-then-else then
        % by this definition it does modify the trail.  We may be able to relax
        % this restriction in future, but at the moment doing this helps keep
        % the contents of the .opt/.trans_opt/.analysis files consistent
        % with the actual code we generate.
        %
        % NOTE: conditional procedures whose status is changed here are
        % candidates for generating specialized versions that omit
        % the trailing code.
        %
        ( Result0 = trail_conditional
        ; Result0 = trail_may_modify
        ),
        Result = trail_may_modify
    ).
check_goal_for_trail_mods_2(SCC, VarTypes, conj(_, Goals), _,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    check_goals_for_trail_mods(SCC, VarTypes, Goals,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO).
check_goal_for_trail_mods_2(SCC, VarTypes, disj(Goals), _,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    check_goals_for_trail_mods(SCC, VarTypes, Goals,
        Result0, MaybeAnalysisStatus, !ModuleInfo, !IO),
    (
        % If none of the disjuncts can modify the trail then we don't need
        % to emit trailing code around this disjunction.
        Result0 = trail_will_not_modify,
        Result  = trail_will_not_modify
    ;
        % See the comment regarding if-then-elses above for the reason
        % we treat `conditional' procedures like this.
        ( Result0 = trail_conditional
        ; Result0 = trail_may_modify
        ),
        Result = trail_may_modify
    ).

:- pred check_goals_for_trail_mods(scc::in, vartypes::in,
    hlds_goals::in, trailing_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_goals_for_trail_mods(SCC, VarTypes, Goals,
        Result, MaybeAnalysisStatus, !ModuleInfo, !IO) :-
    list.map2_foldl2(check_goal_for_trail_mods(SCC, VarTypes), Goals,
        Results, MaybeAnalysisStatuses, !ModuleInfo, !IO),
    list.foldl(combine_trailing_status, Results, trail_will_not_modify,
        Result),
    list.foldl(combine_maybe_analysis_status, MaybeAnalysisStatuses,
        yes(optimal), MaybeAnalysisStatus).

%----------------------------------------------------------------------------%
%
% Utility procedure for processing goals
%

:- func attributes_imply_trail_mod(pragma_foreign_proc_attributes) =
    trailing_status.

attributes_imply_trail_mod(Attributes) =
    ( get_may_modify_trail(Attributes) = proc_may_modify_trail ->
        trail_may_modify
    ;
        trail_will_not_modify
    ).

:- func scope_implies_trail_mod(code_model, code_model, trailing_status)
    = trailing_status.

scope_implies_trail_mod(InnerCodeModel, OuterCodeModel, InnerStatus) =
    (
        % If we're at a commit for a goal that might modify the trail
        % then we need to emit some trailing code around the scope goal.
        InnerCodeModel = model_non,
        OuterCodeModel \= model_non,
        InnerStatus \= trail_will_not_modify
    ->
        trail_may_modify
    ;
        InnerStatus
    ).

%----------------------------------------------------------------------------%
%
% "Known" library procedures
%
    % Succeeds if the given pred_info is for a predicate or function
    % whose trailing status can be looked up in the known procedures
    % table.  Returns the trailing status corresponding to that procedure.
    % Fails if there was no corresponding entry in the table.
    %
:- pred pred_info_has_known_status(pred_info::in, trailing_status::out)
    is semidet.

pred_info_has_known_status(PredInfo, Status) :-
    Name = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    ModuleName = unqualified(ModuleNameStr),
    Arity = pred_info_orig_arity(PredInfo),
    known_procedure(PredOrFunc, ModuleNameStr, Name, Arity, Status).

    % known_procedure/4 is a table of library predicates whose trailing
    % status is hardcoded into the analyser.  For a few predicates this
    % information can make a big difference (particularly in the absence
    % of any form of intermodule analysis).
    %
:- pred known_procedure(pred_or_func::in, string::in, string::in, int::in,
    trailing_status::out) is semidet.

known_procedure(predicate, "require", "error", 1, trail_will_not_modify).
known_procedure(function,  "require", "func_error", 1, trail_will_not_modify).
known_procedure(_, "exception", "throw", 1, trail_will_not_modify).
known_procedure(_, "exception", "rethrow", 1, trail_will_not_modify).

%----------------------------------------------------------------------------%
%
% Further code to handle higher-order variables
%
    % Extract those procedures whose trailing_status has been set to
    % `conditional'.  Fails if one of the procedures in the set
    % is known to modify the trail or if the trailing status is not
    % yet been set for one or more of the procedures.
    %
    % XXX The latter case probably shouldn't happen but may at the
    % moment because the construction of the dependency graph doesn't
    % take higher-order calls into account.
    %
:- pred get_conditional_closures(module_info::in, set(pred_proc_id)::in,
    list(pred_proc_id)::out) is semidet.

get_conditional_closures(ModuleInfo, Closures, Conditionals) :-
    module_info_get_trailing_info(ModuleInfo, TrailingInfo),
    set.fold(get_conditional_closure(TrailingInfo), Closures,
        [], Conditionals).

:- pred get_conditional_closure(trailing_info::in, pred_proc_id::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is semidet.

get_conditional_closure(TrailingInfo, PPId, !Conditionals) :-
    TrailingInfo ^ elem(PPId) = proc_trailing_info(Status, _),
    (
        Status = trail_conditional,
        list.cons(PPId, !Conditionals)
    ;
        Status = trail_will_not_modify
    ).

%----------------------------------------------------------------------------%

:- pred combine_trailing_status(trailing_status::in, trailing_status::in,
    trailing_status::out) is det.

combine_trailing_status(trail_will_not_modify, Y, Y).
combine_trailing_status(trail_may_modify, _, trail_may_modify).
combine_trailing_status(trail_conditional, trail_will_not_modify,
    trail_conditional).
combine_trailing_status(trail_conditional, trail_conditional,
    trail_conditional).
combine_trailing_status(trail_conditional, trail_may_modify, trail_may_modify).

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
% Extra procedures for handling calls
%

    % Check the trailing status of a call.
    %
:- pred check_call(module_info::in, vartypes::in,
    pred_proc_id::in, prog_vars::in, trailing_status::out) is det.

check_call(ModuleInfo, VarTypes, PPId, Args, Result) :-
    check_call_2(ModuleInfo, VarTypes, PPId, Args, MaybeResult),
    (
        MaybeResult = yes(proc_trailing_info(Result, _))
    ;
        MaybeResult = no,
        % If we do not have any information about the callee procedure then
        % assume that it modifies the trail.
        Result = trail_may_modify
    ).

:- pred check_call_2(module_info::in, vartypes::in,
    pred_proc_id::in, prog_vars::in, maybe(proc_trailing_info)::out) is det.

check_call_2(ModuleInfo, VarTypes, PPId, Args, MaybeResult) :-
    module_info_get_trailing_info(ModuleInfo, TrailingInfo),
    ( map.search(TrailingInfo, PPId, CalleeTrailingInfo) ->
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
            %
            % This is a call to a polymorphic procedure.  We need to make
            % sure that none of the types involved has a user-defined
            % equality or comparison predicate that modifies the trail.
            % XXX Need to handle higher-order args here as well.
            MaybeResult = yes(proc_trailing_info(TrailingStatus,
                AnalysisStatus)),
            TrailingStatus = check_vars(ModuleInfo, VarTypes, Args)
        )
    ;
        MaybeResult = no
    ).

:- func check_vars(module_info, vartypes, prog_vars) = trailing_status.

check_vars(ModuleInfo, VarTypes, Vars) = Result :-
    Types = list.map((func(Var) = VarTypes ^ det_elem(Var)), Vars),
    Result = check_types(ModuleInfo, Types).

%----------------------------------------------------------------------------%
%
% Stuff for processing types
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
:- func check_types(module_info, list(mer_type)) = trailing_status.

check_types(ModuleInfo, Types) = Status :-
    list.foldl(check_type(ModuleInfo), Types, trail_will_not_modify, Status).

:- pred check_type(module_info::in, mer_type::in, trailing_status::in,
    trailing_status::out) is det.

check_type(ModuleInfo, Type, !Status) :-
    combine_trailing_status(check_type(ModuleInfo, Type), !Status).

    % Return the trailing status of an individual type.
    %
:- func check_type(module_info, mer_type) = trailing_status.

check_type(ModuleInfo, Type) = Status :-
    (
        ( is_solver_type(ModuleInfo, Type)
        ; is_existq_type(ModuleInfo, Type))
     ->
        % XXX At the moment we just assume that existential
        % types and solver types may modify the trail.
        Status = trail_may_modify
    ;
        TypeCategory = classify_type(ModuleInfo, Type),
        Status = check_type_2(ModuleInfo, Type, TypeCategory)
    ).

:- func check_type_2(module_info, mer_type, type_category) = trailing_status.

check_type_2(_, _, type_cat_int) = trail_will_not_modify.
check_type_2(_, _, type_cat_char) = trail_will_not_modify.
check_type_2(_, _, type_cat_string) = trail_will_not_modify.
check_type_2(_, _, type_cat_float) = trail_will_not_modify.
check_type_2(_, _, type_cat_higher_order) = trail_will_not_modify.
check_type_2(_, _, type_cat_type_info) = trail_will_not_modify.
check_type_2(_, _, type_cat_type_ctor_info) = trail_will_not_modify.
check_type_2(_, _, type_cat_typeclass_info) = trail_will_not_modify.
check_type_2(_, _, type_cat_base_typeclass_info) = trail_will_not_modify.
check_type_2(_, _, type_cat_void) = trail_will_not_modify.
check_type_2(_, _, type_cat_dummy) = trail_will_not_modify.

check_type_2(_, _, type_cat_variable) = trail_conditional.

check_type_2(ModuleInfo, Type, type_cat_tuple) =
    check_user_type(ModuleInfo, Type).
check_type_2(ModuleInfo, Type, type_cat_enum) =
    check_user_type(ModuleInfo, Type).
check_type_2(ModuleInfo, Type, type_cat_user_ctor) =
    check_user_type(ModuleInfo, Type).

:- func check_user_type(module_info, mer_type) = trailing_status.

check_user_type(ModuleInfo, Type) = Status :-
    ( type_to_ctor_and_args(Type, _TypeCtor, Args) ->
        (
            type_has_user_defined_equality_pred(ModuleInfo, Type,
                _UnifyCompare)
        ->
            % XXX We can do better than this by examining
            % what these preds actually do.  Something
            % similar needs to be sorted out for termination
            % analysis as well, so we'll wait until that is
            % done.
            Status = trail_may_modify
        ;
            Status = check_types(ModuleInfo, Args)
        )
    ;
        unexpected(this_file, "Unable to get ctor and args.")
    ).

%----------------------------------------------------------------------------%
%
% Code for attaching trail usage information to goals
%

    % Traverse the body of the procedure and attach will_not_modify trail
    % features to the goal_infos of those procedure that cannot modify the
    % trail.
    %
:- pred annotate_proc(pred_proc_id::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

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
    trailing_status::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

annotate_goal(VarTypes, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = GoalExpr0 - GoalInfo0,
    annotate_goal_2(VarTypes, GoalInfo0, GoalExpr0, GoalExpr, Status,
        !ModuleInfo, !IO),
    ( Status = trail_will_not_modify ->
        goal_info_add_feature(feature_will_not_modify_trail,
            GoalInfo0, GoalInfo)
    ;
        GoalInfo = GoalInfo0
    ),
    !:Goal = GoalExpr - GoalInfo.

:- pred annotate_goal_2(vartypes::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out, trailing_status::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

annotate_goal_2(VarTypes, _, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = conj(ConjType, Conjuncts0),
    annotate_goal_list(VarTypes, Conjuncts0, Conjuncts, Status, !ModuleInfo,
        !IO),
    !:Goal = conj(ConjType, Conjuncts).
annotate_goal_2(VarTypes, _, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = plain_call(CallPredId, CallProcId, CallArgs, _, _, _),
    CallPPId = proc(CallPredId, CallProcId),
    module_info_pred_info(!.ModuleInfo, CallPredId, CallPredInfo),
    (
        pred_info_is_builtin(CallPredInfo)
    ->
        Status = trail_will_not_modify
    ;
        % Handle builtin unify and compare.
        ModuleName = pred_info_module(CallPredInfo),
        any_mercury_builtin_module(ModuleName),
        Name = pred_info_name(CallPredInfo),
        Arity = pred_info_orig_arity(CallPredInfo),
        ( SpecialPredId = spec_pred_compare
        ; SpecialPredId = spec_pred_unify
        ),
        special_pred_name_arity(SpecialPredId, Name, _, Arity)
    ->
        Status = trail_may_modify
    ;
        % Handle library predicates whose trailing status
        % can be looked up in the known procedure table.
        pred_info_has_known_status(CallPredInfo, Status0)
    ->
        Status = Status0
    ;
        globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis,
            !IO),
        (
            IntermodAnalysis = yes,
            pred_info_is_imported(CallPredInfo)
        ->
            % NOTE: we set the value of SCC to a dummy value here.  This is OK
            % because it only needs a meaningful value when building the
            % analysis files; it won't be used when compiling to target code.
            SCC = [],
            search_analysis_status(CallPPId, Result, AnalysisStatus, SCC,
                !ModuleInfo, !IO),

            % XXX We shouldn't be getting invalid analysis results at this
            % stage so maybe we should just call unexpected/2 here?
            ( AnalysisStatus = invalid ->
                Status = trail_may_modify
            ;
                ( Result = trail_conditional ->
                    Status = check_vars(!.ModuleInfo, VarTypes, CallArgs)
                ;
                    Status = Result
                )
            )
        ;
            % This time around we will be checking recursive calls as well.
            check_call(!.ModuleInfo, VarTypes, CallPPId, CallArgs, Status)
        )
    ).
annotate_goal_2(_VarTypes, _, !Goal, Status, !ModuleInfo, !IO) :-
    % XXX Use closure analysis results here.
    !.Goal = generic_call(Details, _, _, _),
    (
        Details = higher_order(_, _, _, _),
        Status = trail_may_modify
    ;
        Details = class_method(_, _, _, _),
        Status = trail_may_modify
    ;
        Details = cast(_),
        Status = trail_will_not_modify
    ).
annotate_goal_2(VarTypes, _, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = switch(Var, CanFail, Cases0),
    annotate_cases(VarTypes, Cases0, Cases, Status, !ModuleInfo, !IO),
    !:Goal = switch(Var, CanFail, Cases).
annotate_goal_2(_VarTypes, _, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = unify(_, _, _, Kind, _),
    ( Kind = complicated_unify(_, _, _) ->
        unexpected(this_file, "complicated unify during trail usage analysis.")
    ;
        true
    ),
    Status = trail_will_not_modify.
annotate_goal_2(VarTypes, _, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = disj(Disjuncts0),
    annotate_goal_list(VarTypes, Disjuncts0, Disjuncts, Status,
        !ModuleInfo, !IO),
    !:Goal = disj(Disjuncts).
annotate_goal_2(VarTypes, _, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = negation(NegGoal0),
    annotate_goal(VarTypes, NegGoal0, NegGoal, Status, !ModuleInfo, !IO),
    !:Goal = negation(NegGoal).
annotate_goal_2(VarTypes, OuterGoalInfo, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = scope(Reason, InnerGoal0),
    annotate_goal(VarTypes, InnerGoal0, InnerGoal, Status0, !ModuleInfo, !IO),
    InnerGoal = _ - InnerGoalInfo,
    goal_info_get_code_model(InnerGoalInfo, InnerCodeModel),
    goal_info_get_code_model(OuterGoalInfo, OuterCodeModel),
    Status = scope_implies_trail_mod(InnerCodeModel, OuterCodeModel, Status0),
    !:Goal = scope(Reason, InnerGoal).
annotate_goal_2(VarTypes, _, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = if_then_else(Vars, If0, Then0, Else0),
    annotate_goal(VarTypes, If0, If, IfStatus, !ModuleInfo, !IO),
    annotate_goal(VarTypes, Then0, Then, ThenStatus, !ModuleInfo, !IO),
    annotate_goal(VarTypes, Else0, Else, ElseStatus, !ModuleInfo, !IO),
    (
        IfStatus   = trail_will_not_modify,
        ThenStatus = trail_will_not_modify,
        ElseStatus = trail_will_not_modify
    ->
        Status = trail_will_not_modify
    ;
        Status = trail_may_modify
    ),
    !:Goal = if_then_else(Vars, If, Then, Else).
annotate_goal_2(_, _, !Goal, Status, !ModuleInfo, !IO) :-
    !.Goal = call_foreign_proc(Attributes, _, _, _, _, _, _),
    Status = attributes_imply_trail_mod(Attributes).
annotate_goal_2(_, _, shorthand(_), _, _, _, _, _, _) :-
    unexpected(this_file, "shorthand goal").

:- pred annotate_goal_list(vartypes::in, hlds_goals::in,
    hlds_goals::out, trailing_status::out, module_info::in,
    module_info::out, io::di, io::uo) is det.

annotate_goal_list(VarTypes, !Goals, Status, !ModuleInfo, !IO) :-
    list.map2_foldl2(annotate_goal(VarTypes), !Goals, Statuses, !ModuleInfo,
        !IO),
    list.foldl(combine_trailing_status, Statuses, trail_will_not_modify,
        Status).

:- pred annotate_cases(vartypes::in, list(case)::in, list(case)::out,
    trailing_status::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

annotate_cases(VarTypes, !Cases, Status, !ModuleInfo, !IO) :-
    list.map2_foldl2(annotate_case(VarTypes), !Cases, Statuses, !ModuleInfo,
        !IO),
    list.foldl(combine_trailing_status, Statuses, trail_will_not_modify,
        Status).

:- pred annotate_case(vartypes::in, case::in, case::out,
    trailing_status::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

annotate_case(VarTypes, !Case, Status, !ModuleInfo, !IO) :-
    !.Case = case(ConsId, Goal0),
    annotate_goal(VarTypes, Goal0, Goal, Status, !ModuleInfo, !IO),
    !:Case = case(ConsId, Goal).

%----------------------------------------------------------------------------%
%
% Stuff for intermodule optimization
%

:- pred make_opt_int(module_info::in, io::di, io::uo) is det.

make_opt_int(ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(ModuleName, ".opt.tmp", no, OptFileName, !IO),
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Appending trailing_info pragmas to `", !IO),
    maybe_write_string(Verbose, OptFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        module_info_get_trailing_info(ModuleInfo, TrailingInfo),
        module_info_predids(ModuleInfo, PredIds),
        list.foldl(write_pragma_trailing_info(ModuleInfo, TrailingInfo),
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

write_pragma_trailing_info(ModuleInfo, TrailingInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    should_write_trailing_info(ModuleInfo, PredId, PredInfo, ShouldWrite),
    (
        ShouldWrite = yes,
        ModuleName = pred_info_module(PredInfo),
        Name       = pred_info_name(PredInfo),
        Arity      = pred_info_orig_arity(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        ProcIds    = pred_info_procids(PredInfo),
        list.foldl((pred(ProcId::in, !.IO::di, !:IO::uo) is det :-
            proc_id_to_int(ProcId, ModeNum),
            (
                map.search(TrailingInfo, proc(PredId, ProcId), ProcTrailInfo),
                ProcTrailInfo = proc_trailing_info(Status, _)
            ->
                mercury_output_pragma_trailing_info(PredOrFunc,
                    qualified(ModuleName, Name), Arity, ModeNum, Status, !IO)
            ;
                true
            )), ProcIds, !IO)
    ;
        ShouldWrite = no
    ).

:- pred should_write_trailing_info(module_info::in, pred_id::in,
    pred_info::in, bool::out) is det.

should_write_trailing_info(ModuleInfo, PredId, PredInfo, ShouldWrite) :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    (
        ( ImportStatus = status_exported
        ; ImportStatus = status_opt_exported
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
        not check_marker(Markers, marker_class_instance_method),
        not check_marker(Markers, marker_named_class_instance_method)
    ->
        ShouldWrite = yes
    ;
        ShouldWrite = no
    ).

%-----------------------------------------------------------------------------%
%
% Stuff for the intermodule analysis framework
%

:- type trailing_analysis_answer
    --->    trailing_analysis_answer(trailing_status).

:- func analysis_name = string.
analysis_name = "trail_usage".

:- instance analysis(any_call, trailing_analysis_answer) where [
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 1,
    preferred_fixpoint_type(_, _) = least_fixpoint,
    bottom(_) = trailing_analysis_answer(trail_will_not_modify),
    top(_) = trailing_analysis_answer(trail_may_modify)
].

:- instance answer_pattern(trailing_analysis_answer) where [].
:- instance partial_order(trailing_analysis_answer) where [
    (more_precise_than(
            trailing_analysis_answer(Status1),
            trailing_analysis_answer(Status2)) :-
        trailing_status_more_precise_than(Status1, Status2)),
    equivalent(Status, Status)
].

:- pred trailing_status_more_precise_than(trailing_status::in,
        trailing_status::in) is semidet.

trailing_status_more_precise_than(trail_will_not_modify, trail_may_modify).
trailing_status_more_precise_than(trail_will_not_modify, trail_conditional).
trailing_status_more_precise_than(trail_conditional, trail_may_modify).

:- instance to_string(trailing_analysis_answer) where [
    func(to_string/1) is trailing_analysis_answer_to_string,
    func(from_string/1) is trailing_analysis_answer_from_string
].

:- func trailing_analysis_answer_to_string(trailing_analysis_answer) = string.

trailing_analysis_answer_to_string(trailing_analysis_answer(Status)) = Str :-
    trailing_status_to_string(Status, Str).

:- func trailing_analysis_answer_from_string(string) =
        trailing_analysis_answer is semidet.

trailing_analysis_answer_from_string(Str) = trailing_analysis_answer(Status) :-
    trailing_status_to_string(Status, Str).

:- pred trailing_status_to_string(trailing_status, string).
:- mode trailing_status_to_string(in, out) is det.
:- mode trailing_status_to_string(out, in) is semidet.

trailing_status_to_string(trail_may_modify, "may_modify_trail").
trailing_status_to_string(trail_will_not_modify, "will_not_modify_trail").
trailing_status_to_string(trail_conditional, "conditional").

:- pred search_analysis_status(pred_proc_id::in,
    trailing_status::out, analysis_status::out, scc::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

search_analysis_status(PPId, Result, AnalysisStatus, CallerSCC,
        !ModuleInfo, !IO) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    search_analysis_status_2(!.ModuleInfo, PPId, Result, AnalysisStatus,
        CallerSCC, AnalysisInfo0, AnalysisInfo, !IO),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred search_analysis_status_2(module_info::in, pred_proc_id::in,
    trailing_status::out, analysis_status::out, scc::in,
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
        MaybeBestStatus = yes({BestCall, trailing_analysis_answer(Result),
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
        % then assume that it modifies the trail.
        top(Call) = Answer,
        Answer = trailing_analysis_answer(Result),
        module_is_local(mmc, ModuleId, IsLocal, !IO),
        (
            IsLocal = yes,
            AnalysisStatus = suboptimal,
            (
                MakeAnalysisRegistry = yes,
                PPId = proc(PredId, _),
                module_info_pred_info(ModuleInfo, PredId, PredInfo),
                should_write_trailing_info(ModuleInfo, PredId, PredInfo,
                    ShouldWrite),
                (
                    ShouldWrite = yes,
                    analysis.record_result(ModuleId, FuncId,
                        Call, Answer, AnalysisStatus, !AnalysisInfo),
                    analysis.record_request(analysis_name, ModuleId, FuncId,
                        Call, !AnalysisInfo),
                    record_dependencies(ModuleId, FuncId, Call,
                        ModuleInfo, CallerSCC, !AnalysisInfo)
                ;
                    ShouldWrite = no
                )
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

record_dependencies(ModuleId, FuncId, Call,
        ModuleInfo, CallerSCC, !AnalysisInfo) :-
    list.foldl((pred(CallerPPId::in, Info0::in, Info::out) is det :-
        mmc_analysis.module_id_func_id(ModuleInfo, CallerPPId,
            CallerModuleId, _),
        analysis.record_dependency(CallerModuleId,
            analysis_name, ModuleId, FuncId, Call, Info0, Info)
    ), CallerSCC, !AnalysisInfo).

:- pred record_trailing_analysis_results(trailing_status::in,
        analysis_status::in, scc::in,
        module_info::in, module_info::out) is det.

record_trailing_analysis_results(Status, ResultStatus, SCC, !ModuleInfo) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    list.foldl(
        record_trailing_analysis_result(!.ModuleInfo, Status, ResultStatus),
        SCC, AnalysisInfo0, AnalysisInfo),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred record_trailing_analysis_result(module_info::in, trailing_status::in,
        analysis_status::in, pred_proc_id::in,
        analysis_info::in, analysis_info::out) is det.

record_trailing_analysis_result(ModuleInfo, Status, ResultStatus,
        PPId @ proc(PredId, _ProcId), AnalysisInfo0, AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    should_write_trailing_info(ModuleInfo, PredId, PredInfo, ShouldWrite),
    (
        ShouldWrite = yes,
        mmc_analysis.module_id_func_id(ModuleInfo, PPId, ModuleId, FuncId),
        analysis.record_result(ModuleId, FuncId, any_call,
            trailing_analysis_answer(Status), ResultStatus,
            AnalysisInfo0, AnalysisInfo)
    ;
        ShouldWrite = no,
        AnalysisInfo = AnalysisInfo0
    ).

%----------------------------------------------------------------------------%
%
% Code for printing out debugging traces
%

:- pred dump_trail_usage_debug_info(module_info::in, scc::in,
    trailing_status::in, io::di, io::uo) is det.

dump_trail_usage_debug_info(ModuleInfo, SCC, Status, !IO) :-
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

this_file = "trailing_analysis.m".

%----------------------------------------------------------------------------%
:- end_module trailing_analysis.
%----------------------------------------------------------------------------%
