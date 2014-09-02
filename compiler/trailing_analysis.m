%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
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
:- instance analysis(no_func_info, any_call, trailing_analysis_answer).
:- instance partial_order(no_func_info, trailing_analysis_answer).
:- instance answer_pattern(no_func_info, trailing_analysis_answer).
:- instance to_term(trailing_analysis_answer).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
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
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type.
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
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    (
        % Only run the analysis in trailing grades.
        UseTrail = yes,
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
        globals.lookup_bool_option(Globals, debug_trail_usage, Debug),
        list.foldl(process_scc(Debug, Pass1Only), SCCs, !ModuleInfo),

        % Only write trailing analysis pragmas to `.opt' files for
        % `--intermodule-optimization', not `--intermodule-analysis'.
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
            list.foldl(maybe_record_trailing_result(!.ModuleInfo),
                PredIds, AnalysisInfo0, AnalysisInfo),
            module_info_set_analysis_info(AnalysisInfo, !ModuleInfo)
        ;
            MakeAnalysisReg = no
        )
    ;
        UseTrail = no
    ).

%----------------------------------------------------------------------------%
%
% Perform trail usage analysis on a SCC.
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
    module_info::in, module_info::out) is det.

process_scc(Debug, Pass1Only, SCC, !ModuleInfo) :-
    check_procs_for_trail_mods(SCC, ProcResults, !ModuleInfo),

    % The `Results' above are the results of analysing each individual
    % procedure in the SCC - we now have to combine them in a meaningful way.
    combine_individual_proc_results(ProcResults,
        TrailingStatus, MaybeAnalysisStatus),

    (
        Debug = yes,
        trace [io(!IO)] (
            dump_trail_usage_debug_info(!.ModuleInfo, SCC, TrailingStatus, !IO)
        )
    ;
        Debug = no
    ),

    % Update the trailing_info with information about this SCC.
    module_info_get_trailing_info(!.ModuleInfo, TrailingInfo0),
    Update = (pred(PPId::in, Info0::in, Info::out) is det :-
        Info = Info0 ^ elem(PPId) :=
            proc_trailing_info(TrailingStatus, MaybeAnalysisStatus)
    ),
    list.foldl(Update, SCC, TrailingInfo0, TrailingInfo),
    module_info_set_trailing_info(TrailingInfo, !ModuleInfo),

    (
        Pass1Only = no,
        list.foldl(annotate_proc, SCC, !ModuleInfo)
    ;
        Pass1Only = yes
    ).

    % Check each procedure in the SCC individually.
    %
:- pred check_procs_for_trail_mods(scc::in, proc_results::out,
        module_info::in, module_info::out) is det.

check_procs_for_trail_mods(SCC, Result, !ModuleInfo) :-
    list.foldl2(check_proc_for_trail_mods(SCC), SCC, [], Result, !ModuleInfo).

    % Examine how the procedures interact with other procedures that
    % are mutually-recursive to them.
    %
:- pred combine_individual_proc_results(proc_results::in,
    trailing_status::out, maybe(analysis_status)::out) is det.

combine_individual_proc_results([], _, _) :-
    unexpected($module, $pred, "empty SCC").
combine_individual_proc_results(ProcResults @ [_|_],
        SCC_Result, MaybeAnalysisStatus) :-
    (
        % If none of the procedures modifies the trail or is conditional then
        % the SCC cannot modify the trail.
        all [ProcResult] (
            list.member(ProcResult, ProcResults)
        =>
            ProcResult ^ status = trail_will_not_modify
        )
    ->
        SCC_Result = trail_will_not_modify
    ;
        all [EResult] (
            list.member(EResult, ProcResults)
        =>
            EResult ^ status \= trail_may_modify
        ),
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
% Perform trail usage analysis on a procedure.
%

:- pred check_proc_for_trail_mods(scc::in,
    pred_proc_id::in, proc_results::in, proc_results::out,
    module_info::in, module_info::out) is det.

check_proc_for_trail_mods(SCC, PPId, !Results, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_goal(ProcInfo, Body),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    check_goal_for_trail_mods(SCC, VarTypes, Body,
        Result, MaybeAnalysisStatus, !ModuleInfo),
    list.cons(proc_result(PPId, Result, MaybeAnalysisStatus), !Results).

%----------------------------------------------------------------------------%
%
% Perform trail usage analysis of a goal.
%

:- pred check_goal_for_trail_mods(scc::in, vartypes::in, hlds_goal::in,
    trailing_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

check_goal_for_trail_mods(SCC, VarTypes, Goal, Result, MaybeAnalysisStatus,
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
            unexpected($module, $pred, "complicated unify")
        )
    ;
        GoalExpr = plain_call(CallPredId, CallProcId, CallArgs, _, _, _),
        CallPPId = proc(CallPredId, CallProcId),
        module_info_pred_info(!.ModuleInfo, CallPredId, CallPredInfo),
        (
            % Handle (mutually-)recursive calls.
            list.member(CallPPId, SCC)
        ->
            lookup_var_types(VarTypes, CallArgs, Types),
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
            % XXX We should examine the argument types of calls to
            % builtin.unify/2 and builtin.compare/3 and then make a decision
            % based on those.
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
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, intermodule_analysis,
                Intermod),
            (
                Intermod = yes,
                pred_info_is_imported_not_external(CallPredInfo)
            ->
                % With --intermodule-analysis use check_call_2 to look up
                % results for locally defined procedures, otherwise we use
                % the intermodule analysis framework.
                search_analysis_status(CallPPId, Result0, AnalysisStatus,
                    !ModuleInfo),
                (
                    Result0 = trail_conditional,
                    Result = check_vars(!.ModuleInfo, VarTypes, CallArgs)
                ;
                    ( Result0 = trail_may_modify
                    ; Result0 = trail_will_not_modify
                    ),
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
            Result  = trail_will_not_modify,
            MaybeAnalysisStatus = yes(optimal)
        )
    ;
        GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        Result = attributes_imply_trail_mod(Attributes),
        MaybeAnalysisStatus = yes(optimal)
    ;
        GoalExpr = conj(_ConjType, Goals),
        check_goals_for_trail_mods(SCC, VarTypes, Goals,
            Result, MaybeAnalysisStatus, !ModuleInfo)
    ;
        GoalExpr = disj(Goals),
        check_goals_for_trail_mods(SCC, VarTypes, Goals,
            _Result0, MaybeAnalysisStatus, !ModuleInfo),
        % XXX Currently we have to put trailing code around disjunctions.
        % If we introduce trail specialisation, it may be possible to omit it.
        Result = trail_may_modify
    ;
        GoalExpr = switch(_, _, Cases),
        CaseGoals = list.map((func(case(_, _, CaseGoal)) = CaseGoal), Cases),
        check_goals_for_trail_mods(SCC, VarTypes, CaseGoals,
            Result, MaybeAnalysisStatus, !ModuleInfo)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        check_goals_for_trail_mods(SCC, VarTypes, [Cond, Then, Else],
            Result0, MaybeAnalysisStatus, !ModuleInfo),
        (
            % If the condition of an if-then-else does not modify the trail
            % and is not model_non then we can omit the trailing ops around
            % the condition.
            %
            % NOTE: any changes here may need to be reflected in the handling
            % of if_then-elses in add_trail_ops.m.

            Result0 = trail_will_not_modify,
            Cond = hlds_goal(_CondGoalExpr, CondGoalInfo),
            goal_info_get_code_model(CondGoalInfo) \= model_non
        ->
            Result = trail_will_not_modify
        ;
            % If the condition modifies the trail, is model_non or both,
            % then we need to emit trailing ops around the conditoin. If the
            % if-then-else has status `trail_conditional' then we also need
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
        check_goal_for_trail_mods(SCC, VarTypes, SubGoal, Result,
            MaybeAnalysisStatus, !ModuleInfo)
    ;
        GoalExpr = scope(Reason, InnerGoal),
        ( Reason = from_ground_term(_, from_ground_term_construct) ->
            % The construction of ground terms will not modify the trail.
            Result = trail_will_not_modify,
            MaybeAnalysisStatus = yes(optimal)
        ;
            OuterGoalInfo = GoalInfo,
            check_goal_for_trail_mods(SCC, VarTypes, InnerGoal, Result0,
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
        unexpected($module, $pred, "shorthand")
    ).

:- pred check_goals_for_trail_mods(scc::in, vartypes::in,
    hlds_goals::in, trailing_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

check_goals_for_trail_mods(SCC, VarTypes, Goals,
        Result, MaybeAnalysisStatus, !ModuleInfo) :-
    list.map2_foldl(check_goal_for_trail_mods(SCC, VarTypes), Goals,
        Results, MaybeAnalysisStatuses, !ModuleInfo),
    list.foldl(combine_trailing_status, Results, trail_will_not_modify,
        Result),
    list.foldl(combine_maybe_analysis_status, MaybeAnalysisStatuses,
        yes(optimal), MaybeAnalysisStatus).

%----------------------------------------------------------------------------%
%
% Utility procedure for processing goals.
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
        OuterCodeModel \= model_non
    ->
        trail_may_modify
    ;
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

known_procedure(pf_predicate, "require", "error", 1,
    trail_will_not_modify).
known_procedure(pf_function,  "require", "func_error", 1,
    trail_will_not_modify).
known_procedure(_, "exception", "throw", 1, trail_will_not_modify).
known_procedure(_, "exception", "rethrow", 1, trail_will_not_modify).

%----------------------------------------------------------------------------%
%
% Code to handle higher-order variables.
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

combine_trailing_status(trail_will_not_modify, Status, Status).
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
% Extra procedures for handling calls.
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
            % This is a call to a polymorphic procedure. We need to make sure
            % that none of the types involved has a user-defined equality
            % or comparison predicate that modifies the trail.
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
    lookup_var_types(VarTypes, Vars, Types),
    Result = check_types(ModuleInfo, Types).

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
        ( type_is_solver_type(ModuleInfo, Type)
        ; type_is_existq_type(ModuleInfo, Type)
        )
     ->
        % XXX At the moment we just assume that existential
        % types and solver types may modify the trail.
        Status = trail_may_modify
    ;
        TypeCtorCategory = classify_type(ModuleInfo, Type),
        Status = check_type_2(ModuleInfo, Type, TypeCtorCategory)
    ).

:- func check_type_2(module_info, mer_type, type_ctor_category)
    = trailing_status.

check_type_2(ModuleInfo, Type, TypeCtorCat) = Status :-
    (
        ( TypeCtorCat = ctor_cat_builtin(_)
        ; TypeCtorCat = ctor_cat_higher_order
        ; TypeCtorCat = ctor_cat_system(_)
        ; TypeCtorCat = ctor_cat_void
        ; TypeCtorCat = ctor_cat_builtin_dummy
        ; TypeCtorCat = ctor_cat_user(cat_user_direct_dummy)
        ),
        Status = trail_will_not_modify
    ;
        TypeCtorCat = ctor_cat_variable,
        Status = trail_conditional
    ;
        ( TypeCtorCat = ctor_cat_tuple
        ; TypeCtorCat = ctor_cat_enum(_)
        ; TypeCtorCat = ctor_cat_user(cat_user_notag)
        ; TypeCtorCat = ctor_cat_user(cat_user_general)
        ),
        type_to_ctor_and_args_det(Type, _TypeCtor, Args),
        (
            type_has_user_defined_equality_pred(ModuleInfo, Type,
                _UnifyCompare)
        ->
            % XXX We can do better than this by examining what these preds
            % actually do. Something similar needs to be sorted out for
            % termination analysis as well, so we'll wait until that is done.
            Status = trail_may_modify
        ;
            Status = check_types(ModuleInfo, Args)
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
    trailing_status::out, module_info::in, module_info::out) is det.

annotate_goal(VarTypes, !Goal, Status, !ModuleInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    annotate_goal_2(VarTypes, GoalInfo0, GoalExpr0, GoalExpr, Status,
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

:- pred annotate_goal_2(vartypes::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out, trailing_status::out,
    module_info::in, module_info::out) is det.

annotate_goal_2(VarTypes, GoalInfo, !GoalExpr, Status, !ModuleInfo) :-
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
            unexpected($module, $pred, "complicated unify")
        ),
        Status = trail_will_not_modify
    ;
        !.GoalExpr = plain_call(CallPredId, CallProcId, CallArgs, _, _, _),
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
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, intermodule_analysis,
                IntermodAnalysis),
            (
                IntermodAnalysis = yes,
                pred_info_is_imported(CallPredInfo)
            ->
                search_analysis_status(CallPPId, Result, AnalysisStatus,
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
                        Status = check_vars(!.ModuleInfo, VarTypes, CallArgs)
                    ;
                        ( Result = trail_may_modify
                        ; Result = trail_will_not_modify
                        ),
                        Status = Result
                    )
                )
            ;
                % This time around we will be checking recursive calls as well.
                check_call(!.ModuleInfo, VarTypes, CallPPId, CallArgs, Status)
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
        !.GoalExpr = if_then_else(Vars, If0, Then0, Else0),
        annotate_goal(VarTypes, If0, If, IfStatus, !ModuleInfo),
        annotate_goal(VarTypes, Then0, Then, ThenStatus, !ModuleInfo),
        annotate_goal(VarTypes, Else0, Else, ElseStatus, !ModuleInfo),
        (
            IfStatus   = trail_will_not_modify,
            ThenStatus = trail_will_not_modify,
            ElseStatus = trail_will_not_modify
        ->
            Status = trail_will_not_modify
        ;
            Status = trail_may_modify
        ),
        !:GoalExpr = if_then_else(Vars, If, Then, Else)
    ;
        !.GoalExpr = negation(SubGoal0),
        annotate_goal(VarTypes, SubGoal0, SubGoal, Status, !ModuleInfo),
        !:GoalExpr = negation(SubGoal)
    ;
        !.GoalExpr = scope(Reason, InnerGoal0),
        ( Reason = from_ground_term(_, from_ground_term_construct) ->
            Status = trail_will_not_modify
        ;
            OuterGoalInfo = GoalInfo,
            annotate_goal(VarTypes, InnerGoal0, InnerGoal, Status0,
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
        unexpected($module, $pred, "shorthand")
    ).

:- pred annotate_goal_list(vartypes::in, hlds_goals::in,
    hlds_goals::out, trailing_status::out, module_info::in,
    module_info::out) is det.

annotate_goal_list(VarTypes, !Goals, Status, !ModuleInfo) :-
    list.map2_foldl(annotate_goal(VarTypes), !Goals, Statuses, !ModuleInfo),
    list.foldl(combine_trailing_status, Statuses, trail_will_not_modify,
        Status).

:- pred annotate_cases(vartypes::in, list(case)::in, list(case)::out,
    trailing_status::out, module_info::in, module_info::out) is det.

annotate_cases(VarTypes, !Cases, Status, !ModuleInfo) :-
    list.map2_foldl(annotate_case(VarTypes), !Cases, Statuses, !ModuleInfo),
    list.foldl(combine_trailing_status, Statuses, trail_will_not_modify,
        Status).

:- pred annotate_case(vartypes::in, case::in, case::out,
    trailing_status::out, module_info::in, module_info::out) is det.

annotate_case(VarTypes, !Case, Status, !ModuleInfo) :-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    annotate_goal(VarTypes, Goal0, Goal, Status, !ModuleInfo),
    !:Case = case(MainConsId, OtherConsIds, Goal).

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
    maybe_write_string(Verbose, "% Appending trailing_info pragmas to `", !IO),
    maybe_write_string(Verbose, OptFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        module_info_get_trailing_info(ModuleInfo, TrailingInfo),
        module_info_get_valid_predids(PredIds, ModuleInfo, _ModuleInfo),
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
    ProcIds = pred_info_procids(PredInfo),
    list.foldl(
        write_pragma_trailing_info_2(ModuleInfo, TrailingInfo, PredId,
            PredInfo),
        ProcIds, !IO).

:- pred write_pragma_trailing_info_2(module_info::in, trailing_info::in,
    pred_id::in, pred_info::in, proc_id::in, io::di, io::uo) is det.

write_pragma_trailing_info_2(ModuleInfo, TrailingMap, PredId, PredInfo,
        ProcId, !IO) :-
    should_write_trailing_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_pragma, ShouldWrite),
    (
        ShouldWrite = yes,
        ModuleName = pred_info_module(PredInfo),
        Name       = pred_info_name(PredInfo),
        Arity      = pred_info_orig_arity(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        proc_id_to_int(ProcId, ModeNum),
        (
            map.search(TrailingMap, proc(PredId, ProcId), ProcTrailInfo),
            ProcTrailInfo = proc_trailing_info(Status, _)
        ->
            PredSymName = qualified(ModuleName, Name),
            PredNameArityPFMn = pred_name_arity_pf_mn(PredSymName, Arity,
                PredOrFunc, ModeNum),
            TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn,
                Status),
            mercury_output_pragma_trailing_info(TrailingInfo, !IO)
        ;
            true
        )
    ;
        ShouldWrite = no
    ).

:- type should_write_for
    --->    for_analysis_framework
    ;       for_pragma.

:- pred should_write_trailing_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, bool::out) is det.

should_write_trailing_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
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

:- type trailing_analysis_answer
    --->    trailing_analysis_answer(trailing_status).

:- func analysis_name = string.

analysis_name = "trail_usage".

:- instance analysis(no_func_info, any_call, trailing_analysis_answer) where [
    analysis_name(_, _) = analysis_name,
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
    Term = term.functor(atom(String), [], context_init).

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

:- pred search_analysis_status(pred_proc_id::in, trailing_status::out,
    analysis_status::out, module_info::in, module_info::out) is det.

search_analysis_status(PPId, Result, AnalysisStatus, !ModuleInfo) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    search_analysis_status_2(!.ModuleInfo, PPId, Result, AnalysisStatus,
        AnalysisInfo0, AnalysisInfo),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred search_analysis_status_2(module_info::in, pred_proc_id::in,
    trailing_status::out, analysis_status::out,
    analysis_info::in, analysis_info::out) is det.

search_analysis_status_2(ModuleInfo, PPId, Result, AnalysisStatus,
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
        record_request(analysis_name, ModuleName, FuncId, Call, !AnalysisInfo)
    ).

:- pred maybe_record_trailing_result(module_info::in, pred_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_trailing_result(ModuleInfo, PredId, !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    list.foldl(maybe_record_trailing_result_2(ModuleInfo, PredId, PredInfo),
        ProcIds, !AnalysisInfo).

:- pred maybe_record_trailing_result_2(module_info::in, pred_id::in,
    pred_info::in, proc_id::in, analysis_info::in, analysis_info::out) is det.

maybe_record_trailing_result_2(ModuleInfo, PredId, PredInfo, ProcId,
        !AnalysisInfo) :-
    should_write_trailing_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_analysis_framework, ShouldWrite),
    (
        ShouldWrite = yes,
        PPId = proc(PredId, ProcId),
        module_info_get_trailing_info(ModuleInfo, TrailingInfo),
        lookup_proc_trailing_info(TrailingInfo, PPId, Status, ResultStatus),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, any_call,
            trailing_analysis_answer(Status), ResultStatus, !AnalysisInfo)
    ;
        ShouldWrite = no
    ).

:- pred lookup_proc_trailing_info(trailing_info::in, pred_proc_id::in,
    trailing_status::out, analysis_status::out) is det.

lookup_proc_trailing_info(TrailingInfo, PPId, Status, ResultStatus) :-
    ( map.search(TrailingInfo, PPId, ProcTrailingInfo) ->
        ProcTrailingInfo = proc_trailing_info(Status, MaybeResultStatus),
        (
            MaybeResultStatus = yes(ResultStatus)
        ;
            MaybeResultStatus = no,
            unexpected($module, $pred, "no result status")
        )
    ;
        % Probably an exported `:- external' procedure wouldn't have been
        % analysed.
        Status = trail_may_modify,
        ResultStatus = optimal
    ).

%----------------------------------------------------------------------------%
%
% Code for printing out debugging traces.
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
:- end_module transform_hlds.trailing_analysis.
%----------------------------------------------------------------------------%
