%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: exception_analysis.m.
% Author: juliensf.
%
% This module performs an exception tracing analysis.  The aim is to annotate
% the HLDS with information about whether each procedure might or will not
% throw an exception.
%
% This information can be useful to the compiler when applying certain types
% of optimization.
%
% After running the analysis the exception behaviour of each procedure
% is one of:
%
%   (1) will_not_throw_exception
%   (2) may_throw_an_exception
%   (3) conditional
%
% (1) guarantees that, for all inputs, the procedure will not throw an
%     exception.
%
% (2) means that a call to that procedure might result in an exception
%     being thrown for at least some inputs.
%
%     We distinguish between two kinds of exception.  Those that
%     are ultimately a result of a call to exception.throw/1, which
%     we refer to as "user exceptions" and those that result from a
%     unification or comparison where one of the types involved has
%     a user-defined equality/comparison predicate that throws
%     an exception.  We refer to the latter kind, as "type exceptions".
%
%     This means that for some polymorphic procedures we cannot
%     say what will happen until we know the values of the type variables.
%     And so we have ...
%
% (3) means that the exception status of the procedure is dependent upon the
%     values of some higher-order variables, or the values of some type
%     variables or both.  This means that we cannot say anything definite
%     about the procedure but for calls to the procedure where have the
%     necessary information we can say what will happen.
%
% In the event that we cannot determine the exception status we just assume
% the worst and mark the procedure as maybe throwing a user exception.
%
% For procedures that are defined using the FFI we currently assume that if a
% procedure will not make calls back to Mercury then it cannot throw
% a Mercury exception; if it does make calls to Mercury then it might
% throw an exception.
%
% NOTE: Some backends, e.g the Java backend, use exceptions in the target
%       language for various things but we're not interested in that here.
%
% TODO:
%   - improve handling of polymorphic procedures (requires type features
%     analysis)
%   - higher order stuff
%   - check what user-defined equality and comparison preds
%     actually do rather than assuming that they always
%     may throw exceptions.
%   - handle existential and solver types - currently we just
%     assume that any call to unify or compare for these types
%     might result in an exception being thrown.
%   - Fix optimizations to use exception information from the analysis
%     registry correctly - predicates in goal_form.m and the optimizations
%     that use them need to be updated.
%
% XXX We need to be a bit careful with transformations like tabling that
% might add calls to exception.throw - at the moment this isn't a problem
% because exception analysis takes place after the tabling transformation.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.exception_analysis.
:- interface.

:- import_module analysis.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module io.

%----------------------------------------------------------------------------%

    % Perform the exception analysis on a module.
    %
:- pred analyse_exceptions_in_module(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Write out the exception pragmas for this module.
    %
:- pred write_pragma_exceptions(module_info::in, exception_info::in,
    pred_id::in, io::di, io::uo) is det.

    % Look the exception status of the given procedure.  This predicate
    % is intended to be used by optimisations that use exception analysis
    % information, *not* for use within the exception analysis itself.
    % This predicate abstracts away differences between
    % intermodule-optimization and intermodule-analysis.
    %
    % NOTE: if intermodule-analysis is enabled then this procedure will
    %       update the IMDG as well.
    %
:- pred lookup_exception_analysis_result(pred_proc_id::in,
    exception_status::out, module_info::in, module_info::out) is det.

%----------------------------------------------------------------------------%
%
% Types and instances for the intermodule analysis framework
%

:- type exception_analysis_answer.
:- instance analysis(no_func_info, any_call, exception_analysis_answer).
:- instance partial_order(no_func_info, exception_analysis_answer).
:- instance answer_pattern(no_func_info, exception_analysis_answer).
:- instance to_term(exception_analysis_answer).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.dependency_graph.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.

%----------------------------------------------------------------------------%
%
% Perform exception analysis on a module
%

analyse_exceptions_in_module(!ModuleInfo, !IO) :-
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
    list.foldl(check_scc_for_exceptions, SCCs, !ModuleInfo),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    globals.lookup_bool_option(Globals, make_analysis_registry,
        MakeAnalysisReg),

    % Only write exception analysis pragmas to `.opt' files for
    % `--intermodule-optimization', not `--intermodule-analysis'.
    (
        MakeOptInt = yes,
        IntermodAnalysis = no
    ->
        make_optimization_interface(!.ModuleInfo, !IO)
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
        list.foldl(maybe_record_exception_result(!.ModuleInfo),
            PredIds, AnalysisInfo0, AnalysisInfo),
        module_info_set_analysis_info(AnalysisInfo, !ModuleInfo)
    ;
        MakeAnalysisReg = no
    ).

%----------------------------------------------------------------------------%
%
% Perform exception analysis on a SCC
%

:- type scc == list(pred_proc_id).

:- type proc_results == list(proc_result).

:- type proc_result
    --->    proc_result(
                ppid   :: pred_proc_id,
                % The ppid of the procedure whose analysis results are
                % stored in this structure.

                status :: exception_status,
                % Exception status of this procedure not counting any input
                % from (mutually-)recursive inputs.

                rec_calls :: type_status,
                % The collective type status of the types of the terms that
                % are arguments of (mutually-)recursive calls.

                maybe_analysis_status :: maybe(analysis_status)
                % The analysis status used for intermodule-analysis.  This
                % should be `no' if we are not compiling with
                % intermodule-analysis enabled.
            ).

:- pred check_scc_for_exceptions(scc::in,
    module_info::in, module_info::out) is det.

check_scc_for_exceptions(SCC, !ModuleInfo) :-
    check_procs_for_exceptions(SCC, ProcResults, !ModuleInfo),
    %
    % The `Results' above are the results of analysing each individual
    % procedure in the SCC - we now have to combine them in a meaningful way.
    %
    combine_individual_proc_results(ProcResults, Status, MaybeAnalysisStatus),
    %
    % Update the exception_info table with information about this SCC.
    %
    module_info_get_exception_info(!.ModuleInfo, ExceptionInfo0),
    Update = (pred(PPId::in, Info0::in, Info::out) is det :-
        Info = Info0 ^ elem(PPId) :=
            proc_exception_info(Status, MaybeAnalysisStatus)
    ),
    list.foldl(Update, SCC, ExceptionInfo0, ExceptionInfo),
    module_info_set_exception_info(ExceptionInfo, !ModuleInfo).

    % Check each procedure in the SCC individually.
    %
:- pred check_procs_for_exceptions(scc::in, proc_results::out,
    module_info::in, module_info::out) is det.

check_procs_for_exceptions(SCC, Result, !ModuleInfo) :-
    list.foldl2(check_proc_for_exceptions(SCC), SCC, [], Result,
        !ModuleInfo).

    % Examine how procedures interact with other procedures that are
    % mutually-recursive to them.
    %
:- pred combine_individual_proc_results(proc_results::in,
    exception_status::out, maybe(analysis_status)::out) is det.

combine_individual_proc_results([], _, _) :-
    unexpected($module, $pred, "Empty SCC during exception analysis.").
combine_individual_proc_results(ProcResults @ [_|_], SCC_Result,
        MaybeAnalysisStatus) :-
    (
        % If none of the procedures may throw an exception or are conditional
        % then the SCC cannot throw an exception either.
        all [ProcResult] list.member(ProcResult, ProcResults) =>
            ProcResult ^ status = will_not_throw
    ->
        SCC_Result = will_not_throw
    ;
        % If none of the procedures may throw an exception but at least one of
        % them is conditional then somewhere in the SCC there is a call to
        % unify or compare that may rely on the types of the polymorphically
        % typed arguments.
        %
        % We need to check that any recursive calls do not introduce types
        % that might have user-defined equality or comparison predicate that
        % throw exceptions.
        %
        all [EResult] (
            list.member(EResult, ProcResults)
        =>
            EResult ^ status \= may_throw(_)
        ),
        some [CResult] (
            list.member(CResult, ProcResults),
            CResult ^ status = throw_conditional
        )
    ->
        SCC_Result = handle_mixed_conditional_scc(ProcResults)
    ;
        % If none of the procedures can throw a user_exception but one or more
        % can throw a type_exception then mark the SCC as maybe throwing a
        % type_exception.

        all [EResult] (
            list.member(EResult, ProcResults)
        =>
            EResult ^ status \= may_throw(user_exception)
        ),
        some [TResult] (
            list.member(TResult, ProcResults),
            TResult ^ status = may_throw(type_exception)
        )
    ->
        SCC_Result = may_throw(type_exception)
    ;
        SCC_Result = may_throw(user_exception)
    ),
    combine_proc_result_maybe_analysis_statuses(ProcResults,
        MaybeAnalysisStatus).

    % XXX There is some code duplication with trailing_analysis.m
    % here ... we should factor out this code into a utility module
    % for intermodule-analysis at some point.
    %
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
% Process individual procedures
%

:- pred check_proc_for_exceptions(scc::in, pred_proc_id::in,
    proc_results::in, proc_results::out, module_info::in, module_info::out)
    is det.

check_proc_for_exceptions(SCC, PPId, !Results, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_goal(ProcInfo, Body),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    MaybeAnalysisStatus0 = maybe_optimal(IntermodAnalysis),
    Result0 = proc_result(PPId, will_not_throw, type_will_not_throw,
        MaybeAnalysisStatus0),
    check_goal_for_exceptions(SCC, VarTypes, Body, Result0, Result,
        !ModuleInfo),
    list.cons(Result, !Results).

:- pred check_goal_for_exceptions(scc::in, vartypes::in, hlds_goal::in,
    proc_result::in, proc_result::out, module_info::in, module_info::out)
    is det.

check_goal_for_exceptions(SCC, VarTypes, hlds_goal(GoalExpr, GoalInfo),
        !Result, !ModuleInfo) :-
    ( goal_info_get_determinism(GoalInfo) = detism_erroneous ->
        !Result ^ status := may_throw(user_exception)
    ;
        check_goal_for_exceptions_2(SCC, VarTypes, GoalExpr, GoalInfo,
            !Result, !ModuleInfo)
    ).

:- pred check_goal_for_exceptions_2(scc::in, vartypes::in,
    hlds_goal_expr::in, hlds_goal_info::in, proc_result::in, proc_result::out,
    module_info::in, module_info::out) is det.

check_goal_for_exceptions_2(SCC, VarTypes, GoalExpr, GoalInfo,
        !Result, !ModuleInfo) :-
    (
        GoalExpr = unify(_, _, _, Kind, _),
        (
            Kind = complicated_unify(_, _, _),
            unexpected($module, $pred,
                "complicated unify during exception analysis.")
        ;
            ( Kind = construct(_, _, _, _, _, _, _)
            ; Kind = deconstruct(_, _, _, _, _, _)
            ; Kind = assign(_, _)
            ; Kind = simple_test(_, _)
            )
        )
    ;
        GoalExpr = plain_call(CallPredId, CallProcId, Args, _, _, _),
        check_goal_for_exceptions_plain_call(SCC, VarTypes,
            CallPredId, CallProcId, Args, !Result, !ModuleInfo)
    ;
        GoalExpr = generic_call(Details, Args, _, _, _),
        check_goal_for_exceptions_generic_call(VarTypes, Details, Args,
            GoalInfo, !Result, !ModuleInfo)
    ;
        GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),

        % NOTE: for --intermodule-analysis the results for for foreign_procs
        % will *always* be optimal (since we always rely on user annotation),
        % so there's nothing to do here.
        MayCallMercury = get_may_call_mercury(Attributes),
        (
            MayCallMercury = proc_may_call_mercury,
            get_may_throw_exception(Attributes) = MayThrowException,
            % We do not need to deal with erroneous predicates here because
            % they will have already been processed.
            (
                MayThrowException = default_exception_behaviour,
                !Result ^ status := may_throw(user_exception)
            ;
                MayThrowException = proc_will_not_throw_exception
            )
        ;
            MayCallMercury = proc_will_not_call_mercury
        )
    ;
        ( GoalExpr = disj(Goals)
        ; GoalExpr = conj(_, Goals)
        ),
        check_goals_for_exceptions(SCC, VarTypes, Goals, !Result, !ModuleInfo)
    ;
        GoalExpr = switch(_, _, Cases),
        CaseGoals = list.map((func(case(_, _, CaseGoal)) = CaseGoal), Cases),
        check_goals_for_exceptions(SCC, VarTypes, CaseGoals, !Result,
            !ModuleInfo)
    ;
        GoalExpr = if_then_else(_, If, Then, Else),
        check_goals_for_exceptions(SCC, VarTypes, [If, Then, Else],
            !Result, !ModuleInfo)
    ;
        GoalExpr = negation(SubGoal),
        check_goal_for_exceptions(SCC, VarTypes, SubGoal, !Result, !ModuleInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        ->
            true
        ;
            check_goal_for_exceptions(SCC, VarTypes, SubGoal, !Result,
                !ModuleInfo)
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, $pred,
            "shorthand goal encountered during exception analysis.")
    ).

:- pred check_goal_for_exceptions_plain_call(scc::in, vartypes::in,
    pred_id::in, proc_id::in, list(prog_var)::in,
    proc_result::in, proc_result::out, module_info::in, module_info::out)
    is det.

check_goal_for_exceptions_plain_call(SCC, VarTypes, CallPredId, CallProcId,
        CallArgs, !Result, !ModuleInfo) :-
    CallPPId = proc(CallPredId, CallProcId),
    module_info_pred_info(!.ModuleInfo, CallPredId, CallPredInfo),
    (
        % Handle (mutually-)recursive calls.
        list.member(CallPPId, SCC)
    ->
        lookup_var_types(VarTypes, CallArgs, Types),
        TypeStatus = check_types(!.ModuleInfo, Types),
        combine_type_status(TypeStatus, !.Result ^ rec_calls, NewTypeStatus),
        !Result ^ rec_calls := NewTypeStatus
    ;
        pred_info_is_builtin(CallPredInfo)
    ->
        % Builtins won't throw exceptions.
        true
    ;
        % Handle unify and compare.
        (
            ModuleName = pred_info_module(CallPredInfo),
            any_mercury_builtin_module(ModuleName),
            Name = pred_info_name(CallPredInfo),
            Arity = pred_info_orig_arity(CallPredInfo),
            ( SpecialPredId = spec_pred_compare
            ; SpecialPredId = spec_pred_unify
            ),
            special_pred_name_arity(SpecialPredId, Name, _, Arity)
        ;
            pred_info_get_origin(CallPredInfo, Origin),
            Origin = origin_special_pred(SpecialPredId - _),
            ( SpecialPredId = spec_pred_compare
            ; SpecialPredId = spec_pred_unify
            )
        )
    ->
        % For unification/comparison the exception status depends upon the the
        % types of the arguments.  In particular whether some component of
        % that type has a user-defined equality/comparison predicate that
        % throws an exception.
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        MaybeAnalysisStatus = maybe_optimal(IntermodAnalysis),
        check_vars(!.ModuleInfo, VarTypes, CallArgs, MaybeAnalysisStatus,
            !Result)
    ;
        check_nonrecursive_call(VarTypes, CallPPId, CallArgs, CallPredInfo,
            !Result, !ModuleInfo)
    ).

:- pred check_goal_for_exceptions_generic_call(vartypes::in,
    generic_call::in, list(prog_var)::in, hlds_goal_info::in,
    proc_result::in, proc_result::out, module_info::in, module_info::out)
    is det.

check_goal_for_exceptions_generic_call(VarTypes, Details, Args, GoalInfo,
        !Result, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        Details = higher_order(Var, _, _,  _),
        ClosureValueMap = goal_info_get_ho_values(GoalInfo),
        ( ClosureValues = ClosureValueMap ^ elem(Var) ->
            get_closures_exception_status(IntermodAnalysis, ClosureValues,
                MaybeWillNotThrow, MaybeAnalysisStatus, !ModuleInfo),
            (
                MaybeWillNotThrow = maybe_will_not_throw(ConditionalProcs),
                (
                    ConditionalProcs = []
                    % The possible values of the higher-order variable are all
                    % procedures that are known not to throw exceptions.
                ;
                    ConditionalProcs = [_ | _],

                    % For 'conditional' procedures we need to make sure that
                    % if any type variables are bound at the generic_call
                    % site, then this does not cause the closure to throw an
                    % exception (because of a user-defined equality or
                    % comparison predicate that throws an exception.)
                    %
                    % If we can resolve all of the polymorphism at this
                    % generic_call site, then we can reach a definite
                    % conclusion about it.
                    %
                    % If we cannot do so, then we propagate the 'conditional'
                    % status to the current predicate if all the type
                    % variables involved are universally quantified, or mark
                    % it as throwing an exception if some of them are
                    % existentially quantified.
                    %
                    % XXX This is too conservative but we don't currently
                    % perform a fine-grained enough analysis of where
                    % out-of-line unifications/comparisons occur to be able to
                    % do better.

                    check_vars(!.ModuleInfo, VarTypes, Args,
                        MaybeAnalysisStatus, !Result)
                )
            ;
                MaybeWillNotThrow = may_throw,
                !Result ^ status := may_throw(user_exception)
            )
        ;
            !Result ^ status := may_throw(user_exception)
        )
    ;
        % XXX We could do better with class methods.
        Details = class_method(_, _, _, _),
        !Result ^ status := may_throw(user_exception)
    ;
        Details = event_call(_)
    ;
        Details = cast(_)
    ).

:- pred check_goals_for_exceptions(scc::in, vartypes::in,
    hlds_goals::in, proc_result::in, proc_result::out,
    module_info::in, module_info::out) is det.

check_goals_for_exceptions(_, _, [], !Result, !ModuleInfo).
check_goals_for_exceptions(SCC, VarTypes, [Goal | Goals], !Result,
        !ModuleInfo) :-
    check_goal_for_exceptions(SCC, VarTypes, Goal, !Result, !ModuleInfo),

    % We can stop searching if we find a user exception.  However if we find
    % a type exception then we still need to check that there is not a user
    % exception somewhere in the rest of the SCC.

    CurrentStatus = !.Result ^ status,
    (
        CurrentStatus = may_throw(user_exception)
    ;
        ( CurrentStatus = will_not_throw
        ; CurrentStatus = throw_conditional
        ; CurrentStatus = may_throw(type_exception)
        ),
        check_goals_for_exceptions(SCC, VarTypes, Goals, !Result, !ModuleInfo)
    ).

%----------------------------------------------------------------------------%
%
% Further code to handle higher-order variables
%

    % The exception status of a collection of procedures that can be called
    % through a higher-order variable.
    %
:- type closures_exception_status
    --->    may_throw
            % One or more of the closures throws an exception.

    ;       maybe_will_not_throw(list(pred_proc_id)).
            % None of the procedures throws a user exception, but the ones in
            % the list are conditional.  Any polymorphic/higher-order
            % args needed to either be checked at the generic_call site or
            % the conditional status needs to be propagated up the call-graph
            % to a point where it can be resolved.

    % For the set of procedures that might be called through a particular
    % higher-order variable at a particular program point (as determined by
    % closure analysis), work out what the overall exception and analysis
    % status is going to be.
    %
:- pred get_closures_exception_status(bool::in, set(pred_proc_id)::in,
    closures_exception_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

get_closures_exception_status(IntermodAnalysis, Closures,
        Conditionals, AnalysisStatus, !ModuleInfo) :-
    module_info_get_exception_info(!.ModuleInfo, ExceptionInfo),
    AnalysisStatus0 = maybe_optimal(IntermodAnalysis),
    set.fold3(
        get_closure_exception_status(IntermodAnalysis, ExceptionInfo),
        Closures, maybe_will_not_throw([]), Conditionals,
        AnalysisStatus0, AnalysisStatus, !ModuleInfo).

:- pred get_closure_exception_status(
    bool::in, exception_info::in, pred_proc_id::in,
    closures_exception_status::in, closures_exception_status::out,
    maybe(analysis_status)::in, maybe(analysis_status)::out,
    module_info::in, module_info::out) is det.

get_closure_exception_status(IntermodAnalysis, ExceptionInfo, PPId,
        !MaybeWillNotThrow, !AS, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, _),
    (
        IntermodAnalysis = yes,
        pred_info_is_imported_not_external(PredInfo)
    ->
        search_analysis_status(PPId, ExceptionStatus, AnalysisStatus,
            !ModuleInfo),
        MaybeAnalysisStatus = yes(AnalysisStatus)
    ;
        ( ProcExceptionInfo = ExceptionInfo ^ elem(PPId) ->
            ProcExceptionInfo = proc_exception_info(ExceptionStatus,
                MaybeAnalysisStatus)
        ;
            ExceptionStatus = may_throw(user_exception),
            MaybeAnalysisStatus = maybe_optimal(IntermodAnalysis)
        )
    ),
    (
        !.MaybeWillNotThrow = may_throw
    ;
        !.MaybeWillNotThrow = maybe_will_not_throw(Conditionals),
        (
            ExceptionStatus = throw_conditional,
            !:MaybeWillNotThrow = maybe_will_not_throw([PPId | Conditionals])
        ;
            ExceptionStatus = will_not_throw
        ;
            ExceptionStatus = may_throw(_),
            !:MaybeWillNotThrow = may_throw
        )
    ),
    combine_maybe_analysis_status(MaybeAnalysisStatus, !AS).

%----------------------------------------------------------------------------%

:- pred update_proc_result(exception_status::in, maybe(analysis_status)::in,
    proc_result::in, proc_result::out) is det.

update_proc_result(CurrentStatus, CurrentAnalysisStatus, !Result) :-
    OldStatus = !.Result ^ status,
    OldAnalysisStatus = !.Result ^ maybe_analysis_status,
    NewStatus = combine_exception_status(CurrentStatus, OldStatus),
    combine_maybe_analysis_status(CurrentAnalysisStatus, OldAnalysisStatus,
        NewAnalysisStatus),
    !Result ^ status := NewStatus,
    !Result ^ maybe_analysis_status := NewAnalysisStatus.

:- func combine_exception_status(exception_status, exception_status)
    = exception_status.

combine_exception_status(will_not_throw, Y) = Y.
combine_exception_status(X @ may_throw(user_exception), _) = X.
combine_exception_status(X @ may_throw(type_exception), will_not_throw) = X.
combine_exception_status(X @ may_throw(type_exception), throw_conditional) = X.
combine_exception_status(may_throw(type_exception), Y @ may_throw(_)) = Y.
combine_exception_status(throw_conditional, throw_conditional) =
    throw_conditional.
combine_exception_status(throw_conditional, will_not_throw) =
    throw_conditional.
combine_exception_status(throw_conditional, Y @ may_throw(_)) = Y.

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

:- pred check_nonrecursive_call(vartypes::in,
    pred_proc_id::in, prog_vars::in, pred_info::in,
    proc_result::in, proc_result::out,
    module_info::in, module_info::out) is det.

check_nonrecursive_call(VarTypes, PPId, Args, PredInfo, !Result,
        !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        % If we are using `--intermodule-analysis' then use the analysis
        % framework for imported procedures.
        IntermodAnalysis = yes,
        pred_info_is_imported_not_external(PredInfo)
    ->
        search_analysis_status(PPId, CalleeResult, AnalysisStatus,
            !ModuleInfo),
        MaybeAnalysisStatus = yes(AnalysisStatus),
        update_proc_result(CalleeResult, MaybeAnalysisStatus, !Result)
    ;
        module_info_get_exception_info(!.ModuleInfo, ExceptionInfo),
        ( map.search(ExceptionInfo, PPId, CalleeExceptionInfo) ->
            CalleeExceptionInfo = proc_exception_info(CalleeExceptionStatus,
                MaybeAnalysisStatus),
            (
                CalleeExceptionStatus = will_not_throw,
                update_proc_result(will_not_throw, MaybeAnalysisStatus,
                    !Result)
            ;
                CalleeExceptionStatus = may_throw(ExceptionType),
                update_proc_result(may_throw(ExceptionType),
                    MaybeAnalysisStatus, !Result)
            ;
                CalleeExceptionStatus = throw_conditional,
                check_vars(!.ModuleInfo, VarTypes, Args, MaybeAnalysisStatus,
                    !Result)
            )
        ;
            % If we do not have any information about the callee procedure
            % then assume that it might throw an exception.
            % Analysis statuses on individual results are meaningless now.
            MaybeAnalysisStatus = maybe_optimal(IntermodAnalysis),
            update_proc_result(may_throw(user_exception), MaybeAnalysisStatus,
                !Result)
        )
    ).

:- pred check_vars(module_info::in, vartypes::in, prog_vars::in,
    maybe(analysis_status)::in, proc_result::in, proc_result::out) is det.

check_vars(ModuleInfo, VarTypes, Vars, MaybeAnalysisStatus, !Result) :-
    lookup_var_types(VarTypes, Vars, Types),
    TypeStatus = check_types(ModuleInfo, Types),
    (
        TypeStatus = type_will_not_throw
    ;
        TypeStatus = type_may_throw,
        update_proc_result(may_throw(type_exception), MaybeAnalysisStatus,
            !Result)
    ;
        TypeStatus = type_conditional,
        update_proc_result(throw_conditional, MaybeAnalysisStatus, !Result)
    ).

%----------------------------------------------------------------------------%
%
% Predicates for checking mixed SCCs
%

% A "mixed SCC" is one where at least one of the procedures in the SCC is
% known not to throw an exception, at least one of them is conditional and
% none of them may throw an exception (of either sort).
%
% In order to determine the status of such a SCC we also need to take the
% effect of the recursive calls into account.  This is because calls to a
% conditional procedure from a procedure that is mutually recursive to it may
% introduce types that could cause a type_exception to be thrown.
%
% We currently assume that if these types are introduced somewhere in the SCC
% then they may be propagated around the entire SCC - hence if a part of the
% SCC is conditional we need to make sure other parts don't supply it with
% input whose types may have user-defined equality/comparison predicates.

% NOTE: it is possible to write rather contrived programs that can exhibit
% rather strange behaviour which is why all this is necessary.

:- func handle_mixed_conditional_scc(proc_results) = exception_status.

handle_mixed_conditional_scc(Results) =
    (
        all [TypeStatus] (
            list.member(Result, Results)
        =>
            Result ^ rec_calls \= type_may_throw
        )
    ->
        throw_conditional
    ;
        % Somewhere a type that causes an exception is being
        % passed around the SCC via one or more of the recursive
        % calls.
        may_throw(type_exception)
    ).

%----------------------------------------------------------------------------%
%
% Stuff for processing types.
%

% This is used in the analysis of calls to polymorphic procedures.
%
% By saying a `type can throw an exception' we mean that an exception might be
% thrown as a result of a unification or comparison involving the type because
% it has a user-defined equality/comparison predicate that may throw an
% exception.
%
% XXX We don't actually need to examine all the types, just those that are
% potentially going to be involved in unification/comparisons.  At the moment
% we don't keep track of that information so the current procedure is as
% follows:
%
% Examine the functor and then recursively examine the arguments.
% * If everything will not throw then the type will not throw
% * If at least one of the types may_throw then the type will throw
% * If at least one of the types is conditional  and none of them throw then
%   the type is conditional.

:- type type_status
    --->    type_will_not_throw
            % This type does not have user-defined equality
            % or comparison predicates.
            % XXX (Or it has ones that are known not to throw
            %      exceptions).

    ;       type_may_throw
            % This type has a user-defined equality or comparison
            % predicate that is known to throw an exception.

    ;       type_conditional.
            % This type is polymorphic.  We cannot say anything about
            % it until we know the values of the type-variables.

    % Return the collective type status of a list of types.
    %
:- func check_types(module_info, list(mer_type)) = type_status.

check_types(ModuleInfo, Types) = Status :-
    list.foldl(check_type(ModuleInfo), Types, type_will_not_throw, Status).

:- pred check_type(module_info::in, mer_type::in, type_status::in,
    type_status::out) is det.

check_type(ModuleInfo, Type, !Status) :-
    combine_type_status(check_type(ModuleInfo, Type), !Status).

:- pred combine_type_status(type_status::in, type_status::in,
    type_status::out) is det.

combine_type_status(type_will_not_throw, type_will_not_throw,
        type_will_not_throw).
combine_type_status(type_will_not_throw, type_conditional, type_conditional).
combine_type_status(type_will_not_throw, type_may_throw, type_may_throw).
combine_type_status(type_conditional, type_will_not_throw, type_conditional).
combine_type_status(type_conditional, type_conditional, type_conditional).
combine_type_status(type_conditional, type_may_throw, type_may_throw).
combine_type_status(type_may_throw, _, type_may_throw).

    % Return the type status of an individual type.
    %
:- func check_type(module_info, mer_type) = type_status.

check_type(ModuleInfo, Type) = Status :-
    (
        ( type_is_solver_type(ModuleInfo, Type)
        ; type_is_existq_type(ModuleInfo, Type)
        )
     ->
        % XXX At the moment we just assume that existential types and
        % solver types result in a type exception being thrown.
        Status = type_may_throw
    ;
        TypeCategory = classify_type(ModuleInfo, Type),
        Status = check_type_2(ModuleInfo, Type, TypeCategory)
    ).

:- func check_type_2(module_info, mer_type, type_ctor_category) = type_status.

check_type_2(ModuleInfo, Type, CtorCat) = WillThrow :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_builtin_dummy
        ),
        WillThrow = type_will_not_throw
    ;
        CtorCat = ctor_cat_variable,
        WillThrow = type_conditional
    ;
        CtorCat = ctor_cat_tuple,
        ( type_to_ctor_and_args(Type, _TypeCtor, Args) ->
            WillThrow = check_types(ModuleInfo, Args)
        ;
            unexpected($module, $pred, "expected tuple type")
        )
    ;
        CtorCat = ctor_cat_enum(_),
        ( type_has_user_defined_equality_pred(ModuleInfo, Type, _UC) ->
            % XXX This is very conservative.
            WillThrow = type_may_throw
        ;
            WillThrow = type_will_not_throw
        )
    ;
        CtorCat = ctor_cat_user(_),
        type_to_ctor_and_args_det(Type, TypeCtor, Args),
        ( type_has_user_defined_equality_pred(ModuleInfo, Type, _UC) ->
            % XXX We can do better than this by examining what these preds
            % actually do. Something similar needs to be sorted out for
            % termination analysis as well, so we'll wait until that is done.
            WillThrow = type_may_throw
        ;
            ( type_ctor_is_safe(TypeCtor) ->
                WillThrow = check_types(ModuleInfo, Args)
            ;
                WillThrow = type_may_throw
            )
        )
    ).

    % Succeeds if the exception status of the type represented by the given
    % type_ctor can be determined by examining the exception status of the
    % arguments, if any.
    %
    % NOTE: This list does not need to include enumerations since they
    % are already handled above. Also, this list does not need to include
    % non-abstract equivalence types.
    %
:- pred type_ctor_is_safe(type_ctor::in) is semidet.

type_ctor_is_safe(TypeCtor) :-
    TypeCtor = type_ctor(qualified(unqualified(ModuleName), CtorName), Arity),
    type_ctor_is_safe_2(ModuleName, CtorName, Arity).

:- pred type_ctor_is_safe_2(string::in, string::in, arity::in) is semidet.

type_ctor_is_safe_2("assoc_list",    "assoc_list",    1).
type_ctor_is_safe_2("bag",           "bag",           1).
type_ctor_is_safe_2("bimap",         "bimap",         2).
type_ctor_is_safe_2("builtin",       "c_pointer",     0).
type_ctor_is_safe_2("cord",          "cord",          1).
type_ctor_is_safe_2("eqvclass",      "eqvclass",      1).
type_ctor_is_safe_2("injection",     "injection",     2).
type_ctor_is_safe_2("integer",       "integer",       0).
type_ctor_is_safe_2("io",            "input_stream",  0).
type_ctor_is_safe_2("io",            "output_stream", 0).
type_ctor_is_safe_2("io",            "binary_stream", 0).
type_ctor_is_safe_2("io",            "stream_id",     0).
type_ctor_is_safe_2("io",            "res",           0).
type_ctor_is_safe_2("io",            "res",           1).
type_ctor_is_safe_2("io",            "maybe_partial_res", 1).
type_ctor_is_safe_2("io",            "result",            0).
type_ctor_is_safe_2("io",            "result",            1).
type_ctor_is_safe_2("io",            "read_result",       1).
type_ctor_is_safe_2("io",            "error",         0).
type_ctor_is_safe_2("list",          "list",          1).
type_ctor_is_safe_2("map",           "map",           2).
type_ctor_is_safe_2("maybe",         "maybe",         1).
type_ctor_is_safe_2("maybe_error",   "maybe_error",   1).
type_ctor_is_safe_2("multi_map",     "multi_map",     2).
type_ctor_is_safe_2("pair",          "pair",          2).
type_ctor_is_safe_2("pqueue",        "pqueue",        2).
type_ctor_is_safe_2("queue",         "queue",         1).
type_ctor_is_safe_2("rational",      "rational",      0).
type_ctor_is_safe_2("rbtree",        "rbtree",        2).
type_ctor_is_safe_2("rtree",         "rtree",         2).
type_ctor_is_safe_2("set",           "set",           1).
type_ctor_is_safe_2("set_bbbtree",   "set_bbbtree",   1).
type_ctor_is_safe_2("set_ctree234",  "set_ctree234",  1).
type_ctor_is_safe_2("set_ordlist",   "set_ordlist",   1).
type_ctor_is_safe_2("set_tree234",   "set_tree234",   1).
type_ctor_is_safe_2("set_unordlist", "set_unordlist", 1).
type_ctor_is_safe_2("stack",         "stack",         1).
type_ctor_is_safe_2("string",        "poly_type",     0).
type_ctor_is_safe_2("string",        "justified_column", 0).
type_ctor_is_safe_2("term",          "term",          1).
type_ctor_is_safe_2("term",          "const",         0).
type_ctor_is_safe_2("term",          "context",       0).
type_ctor_is_safe_2("term",          "var",           1).
type_ctor_is_safe_2("term",          "var_supply",    1).
type_ctor_is_safe_2("varset",        "varset",        1).

%----------------------------------------------------------------------------%
%
% Types and instances for the intermodule analysis framework
%

:- type exception_analysis_answer
    --->    exception_analysis_answer(exception_status).

:- func analysis_name = string.

analysis_name = "exception_analysis".

:- instance analysis(no_func_info, any_call, exception_analysis_answer)
        where [
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 1,
    preferred_fixpoint_type(_, _) = least_fixpoint,
    bottom(_, _) = exception_analysis_answer(will_not_throw),
    top(_, _) = exception_analysis_answer(may_throw(user_exception)),
    get_func_info(_, _, _, _, _, no_func_info)
].

:- instance answer_pattern(no_func_info, exception_analysis_answer) where [].
:- instance partial_order(no_func_info, exception_analysis_answer) where [
    ( more_precise_than(no_func_info, Answer1, Answer2) :-
        Answer1 = exception_analysis_answer(Status1),
        Answer2 = exception_analysis_answer(Status2),
        exception_status_more_precise_than(Status1, Status2)
    ),

    equivalent(no_func_info, Status, Status)
].

:- pred exception_status_more_precise_than(exception_status::in,
    exception_status::in) is semidet.

exception_status_more_precise_than(will_not_throw, throw_conditional).
exception_status_more_precise_than(will_not_throw, may_throw(_)).
exception_status_more_precise_than(throw_conditional, may_throw(_)).
exception_status_more_precise_than(may_throw(type_exception),
    may_throw(user_exception)).

:- instance to_term(exception_analysis_answer) where [
    func(to_term/1) is answer_to_term,
    pred(from_term/2) is answer_from_term
].

:- func answer_to_term(exception_analysis_answer) = term.

answer_to_term(Answer) = Term :-
    Answer = exception_analysis_answer(Status),
    exception_status_to_string(Status, String),
    Term = term.functor(atom(String), [], context_init).

:- pred answer_from_term(term::in, exception_analysis_answer::out) is semidet.

answer_from_term(Term, exception_analysis_answer(Status)) :-
    Term = term.functor(atom(String), [], _),
    exception_status_to_string(Status, String).

:- pred exception_status_to_string(exception_status, string).
:- mode exception_status_to_string(in, out) is det.
:- mode exception_status_to_string(out, in) is semidet.

exception_status_to_string(will_not_throw, "will_not_throw").
exception_status_to_string(throw_conditional, "conditional").
exception_status_to_string(may_throw(type_exception),
    "may_throw_type_exception").
exception_status_to_string(may_throw(user_exception),
    "may_throw_user_exception").

%----------------------------------------------------------------------------%
%
% Additional predicates used for intermodule analysis
%

:- pred search_analysis_status(pred_proc_id::in,
    exception_status::out, analysis_status::out,
    module_info::in, module_info::out) is det.

search_analysis_status(PPId, Result, AnalysisStatus, !ModuleInfo) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    search_analysis_status_2(!.ModuleInfo, PPId, Result, AnalysisStatus,
        AnalysisInfo0, AnalysisInfo),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred search_analysis_status_2(module_info::in, pred_proc_id::in,
    exception_status::out, analysis_status::out,
    analysis_info::in, analysis_info::out) is det.

search_analysis_status_2(ModuleInfo, PPId, Result, AnalysisStatus,
        !AnalysisInfo) :-
    module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
    Call = any_call,
    lookup_best_result(!.AnalysisInfo, ModuleName, FuncId, no_func_info, Call,
        MaybeBestResult),
    (
        MaybeBestResult = yes(analysis_result(BestCall, BestAnswer,
            AnalysisStatus)),
        BestAnswer = exception_analysis_answer(Result),
        record_dependency(ModuleName, FuncId, no_func_info, BestCall,
            _ : exception_analysis_answer, !AnalysisInfo)
    ;
        MaybeBestResult = no,
        % If we do not have any information about the callee procedure then
        % assume that it throws an exception.
        top(no_func_info, Call) = Answer,
        Answer = exception_analysis_answer(Result),
        AnalysisStatus = optimal,
        record_request(analysis_name, ModuleName,  FuncId, Call, !AnalysisInfo)
    ).

:- pred maybe_record_exception_result(module_info::in, pred_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_exception_result(ModuleInfo, PredId, !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    list.foldl(maybe_record_exception_result_2(ModuleInfo, PredId, PredInfo),
        ProcIds, !AnalysisInfo).

:- pred maybe_record_exception_result_2(module_info::in, pred_id::in,
    pred_info::in, proc_id::in, analysis_info::in, analysis_info::out) is det.

maybe_record_exception_result_2(ModuleInfo, PredId, PredInfo, ProcId,
        !AnalysisInfo) :-
    should_write_exception_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_analysis_framework, ShouldWrite),
    (
        ShouldWrite = yes,
        PPId = proc(PredId, ProcId),
        module_info_get_exception_info(ModuleInfo, ExceptionInfo),
        lookup_proc_exception_info(ExceptionInfo, PPId, Status, ResultStatus),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, any_call,
            exception_analysis_answer(Status), ResultStatus, !AnalysisInfo)
    ;
        ShouldWrite = no
    ).

:- pred lookup_proc_exception_info(exception_info::in, pred_proc_id::in,
    exception_status::out, analysis_status::out) is det.

lookup_proc_exception_info(ExceptionInfo, PPId, Status, ResultStatus) :-
    ( map.search(ExceptionInfo, PPId, ProcExceptionInfo) ->
        ProcExceptionInfo = proc_exception_info(Status, MaybeResultStatus),
        (
            MaybeResultStatus = yes(ResultStatus)
        ;
            MaybeResultStatus = no,
            unexpected($module, $pred, "no result status")
        )
    ;
        % Probably an exported `:- external' procedure.
        Status = may_throw(user_exception),
        ResultStatus = optimal
    ).

:- type should_write_for
    --->    for_analysis_framework
    ;       for_pragma.

:- pred should_write_exception_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, bool::out) is det.

should_write_exception_info(ModuleInfo, PredId, ProcId, PredInfo,
        WhatFor, ShouldWrite) :-
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

:- func maybe_optimal(bool) = maybe(analysis_status).

maybe_optimal(no)  = no.
maybe_optimal(yes) = yes(optimal).

%----------------------------------------------------------------------------%
%
% Stuff for intermodule optimization.
%

:- pred make_optimization_interface(module_info::in, io::di, io::uo)
    is det.

make_optimization_interface(ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, ModuleName, ".opt.tmp",
        do_not_create_dirs, OptFileName, !IO),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(Verbose, "% Appending exceptions pragmas to `", !IO),
    maybe_write_string(Verbose, OptFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        module_info_get_exception_info(ModuleInfo, ExceptionInfo),
        module_info_get_valid_predids(PredIds, ModuleInfo, _ModuleInfo),
        list.foldl(write_pragma_exceptions(ModuleInfo, ExceptionInfo),
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

write_pragma_exceptions(ModuleInfo, ExceptionInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    list.foldl(
        write_pragma_exceptions_2(ModuleInfo, ExceptionInfo, PredId, PredInfo),
        ProcIds, !IO).

:- pred write_pragma_exceptions_2(module_info::in, exception_info::in,
    pred_id::in, pred_info::in, proc_id::in, io::di, io::uo) is det.

write_pragma_exceptions_2(ModuleInfo, ExceptionMap, PredId, PredInfo, ProcId,
        !IO) :-
    should_write_exception_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_pragma, ShouldWrite),
    (
        ShouldWrite = yes,
        ModuleName = pred_info_module(PredInfo),
        Name       = pred_info_name(PredInfo),
        Arity      = pred_info_orig_arity(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        proc_id_to_int(ProcId, ModeNum),
        ( map.search(ExceptionMap, proc(PredId, ProcId), ProcExceptionInfo) ->
            ProcExceptionInfo = proc_exception_info(Status, _),
            PredSymName = qualified(ModuleName, Name),
            PredNameArityPFMn = pred_name_arity_pf_mn(PredSymName, Arity,
                PredOrFunc, ModeNum),
            ExceptionInfo = pragma_info_exceptions(PredNameArityPFMn, Status),
            mercury_output_pragma_exceptions(ExceptionInfo, !IO)
        ;
            true
        )
    ;
        ShouldWrite = no
    ).

%----------------------------------------------------------------------------%
%
% External interface to exception analysis information
%

lookup_exception_analysis_result(PPId, ExceptionStatus, !ModuleInfo) :-
    PPId = proc(PredId, _),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    IsImported = pred_to_bool(pred_info_is_imported_not_external(PredInfo)),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    globals.lookup_bool_option(Globals, analyse_exceptions,
        ExceptionAnalysis),

    % If we the procedure we are calling is imported and we are using
    % intermodule-analysis then we need to look up the exception status in the
    % analysis registry; otherwise we look it up in the the exception_info
    % table.

    UseAnalysisRegistry = IsImported `bool.and` IntermodAnalysis
        `bool.and` ExceptionAnalysis,
    (
        % If the procedure is not imported then it's exception_status
        % will be in the exception_info table.
        UseAnalysisRegistry = no,
        module_info_get_exception_info(!.ModuleInfo, ExceptionInfo),
        (
            map.search(ExceptionInfo, PPId, ProcExceptionInfo)
        ->
            ProcExceptionInfo = proc_exception_info(ExceptionStatus, _)
        ;
            ExceptionStatus = may_throw(user_exception)
        )
    ;
        UseAnalysisRegistry = yes,
        some [!AnalysisInfo] (
            module_info_get_analysis_info(!.ModuleInfo, !:AnalysisInfo),
            module_name_func_id(!.ModuleInfo, PPId, ModuleName, FuncId),
            lookup_best_result(!.AnalysisInfo, ModuleName, FuncId,
                no_func_info, any_call, MaybeBestResult),
            (
                MaybeBestResult = yes(analysis_result(_Call, Answer,
                    AnalysisStatus)),
                (
                    AnalysisStatus = invalid,
                    unexpected($module, $pred,
                        "invalid exception_analysis answer")
                ;
                    ( AnalysisStatus = optimal
                    ; AnalysisStatus = suboptimal
                    ),
                    Answer = exception_analysis_answer(ExceptionStatus)
                )
            ;
                MaybeBestResult = no,
                ExceptionStatus = may_throw(user_exception)
            ),
            record_dependency(ModuleName, FuncId, no_func_info, any_call,
                _ : exception_analysis_answer, !AnalysisInfo),
            module_info_set_analysis_info(!.AnalysisInfo, !ModuleInfo)
        )
    ).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.exception_analysis.
%----------------------------------------------------------------------------%
