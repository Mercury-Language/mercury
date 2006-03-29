%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: exception_analysis.m.
% Author: juliensf.

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

% XXX We need to be a bit careful with transformations like tabling that
% might add calls to exception.throw - at the moment this isn't a problem
% because exception analysis takes place after the tabling transformation.

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
    exception_status::out, module_info::in, module_info::out, io::di, io::uo)
    is det.

%----------------------------------------------------------------------------%
%
% Types and instances for the intermodule analysis framework
%

:- type exception_analysis_answer.
:- instance analysis(any_call, exception_analysis_answer).
:- instance partial_order(exception_analysis_answer).
:- instance answer_pattern(exception_analysis_answer).
:- instance to_string(exception_analysis_answer).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.
:- import_module hlds.passes_aux.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
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
% Perform exception analysis on a module
%

analyse_exceptions_in_module(!ModuleInfo, !IO) :-
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
    list.foldl2(check_scc_for_exceptions, SCCs, !ModuleInfo, !IO),
    globals.io_lookup_bool_option(make_optimization_interface, MakeOptInt,
        !IO),
    (
        MakeOptInt = yes,
        make_optimization_interface(!.ModuleInfo, !IO)
    ;
        MakeOptInt = no
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
    module_info::in, module_info::out, io::di, io::uo) is det.

check_scc_for_exceptions(SCC, !ModuleInfo, !IO) :-
    check_procs_for_exceptions(SCC, ProcResults, !ModuleInfo, !IO),
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
    module_info_set_exception_info(ExceptionInfo, !ModuleInfo),
    %
    % Record the analysis results for intermodule analysis.
    %
    globals.io_lookup_bool_option(make_analysis_registry,
        MakeAnalysisRegistry, !IO),
    (
        MakeAnalysisRegistry = yes,
        (
            MaybeAnalysisStatus = yes(AnalysisStatus),
            record_exception_analysis_results(Status, AnalysisStatus, SCC,
                !ModuleInfo)
        ;
            MaybeAnalysisStatus = no,
            unexpected(this_file,
                "check_scc_for_exceptions: no analysis status.")
        )
    ;
        MakeAnalysisRegistry = no
    ).

    % Check each procedure in the SCC individually.
    %
:- pred check_procs_for_exceptions(scc::in, proc_results::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_procs_for_exceptions(SCC, Result, !ModuleInfo, !IO) :-
    list.foldl3(check_proc_for_exceptions(SCC), SCC, [], Result,
        !ModuleInfo, !IO).

    % Examine how procedures interact with other procedures that are
    % mutually-recursive to them.
    %
:- pred combine_individual_proc_results(proc_results::in,
    exception_status::out, maybe(analysis_status)::out) is det.

combine_individual_proc_results([], _, _) :-
    unexpected(this_file, "Empty SCC during exception analysis.").
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
        all [EResult] list.member(EResult, ProcResults) =>
            EResult ^ status \= may_throw(_),
        some [CResult] (
            list.member(CResult, ProcResults),
            CResult ^ status = conditional
        )
    ->
        SCC_Result = handle_mixed_conditional_scc(ProcResults)
    ;
        % If none of the procedures can throw a user_exception but one or more
        % can throw a type_exception then mark the SCC as maybe throwing a
        % type_exception.
        %
        all [EResult] list.member(EResult, ProcResults) =>
            EResult ^ status \= may_throw(user_exception),
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
    proc_results::in, proc_results::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

check_proc_for_exceptions(SCC, PPId, !Results, !ModuleInfo, !IO) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_goal(ProcInfo, Body),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis,
        !IO),
    MaybeAnalysisStatus0 = maybe_optimal(IntermodAnalysis),
    Result0 = proc_result(PPId, will_not_throw, type_will_not_throw,
        MaybeAnalysisStatus0),
    check_goal_for_exceptions(SCC, VarTypes, Body, Result0, Result,
        !ModuleInfo, !IO),
    list.cons(Result, !Results).

:- pred check_goal_for_exceptions(scc::in, vartypes::in,
    hlds_goal::in, proc_result::in, proc_result::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_goal_for_exceptions(SCC, VarTypes, Goal - GoalInfo, !Result,
        !ModuleInfo, !IO) :-
    ( goal_info_get_determinism(GoalInfo, erroneous) ->
        !:Result = !.Result ^ status := may_throw(user_exception)
    ;
        check_goal_for_exceptions_2(SCC, VarTypes, Goal, GoalInfo, !Result,
            !ModuleInfo, !IO)
    ).

:- pred check_goal_for_exceptions_2(scc::in, vartypes::in,
    hlds_goal_expr::in, hlds_goal_info::in, proc_result::in, proc_result::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_goal_for_exceptions_2(_, _, Goal, _, !Result, !ModuleInfo, !IO) :-
    Goal = unify(_, _, _, Kind, _),
    ( 
        Kind = complicated_unify(_, _, _),
        unexpected(this_file, "complicated unify during exception analysis.")
    ;
        ( Kind = construct(_, _, _, _, _, _, _)
        ; Kind = deconstruct(_, _, _, _, _, _)
        ; Kind = assign(_, _)
        ; Kind = simple_test(_, _)
        )
    ).
check_goal_for_exceptions_2(SCC, VarTypes, Goal, _, !Result,
        !ModuleInfo, !IO) :-
    Goal = call(CallPredId, CallProcId, CallArgs, _, _, _),
    CallPPId = proc(CallPredId, CallProcId),
    module_info_pred_info(!.ModuleInfo, CallPredId, CallPredInfo),
    (
        % Handle (mutually-)recursive calls.
        list.member(CallPPId, SCC)
    ->
        Types = list.map((func(Var) = VarTypes ^ det_elem(Var)), CallArgs),
        TypeStatus = check_types(!.ModuleInfo, Types),
        combine_type_status(TypeStatus, !.Result ^ rec_calls, NewTypeStatus),
        !:Result = !.Result ^ rec_calls := NewTypeStatus
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
            Origin = special_pred(SpecialPredId - _),
            ( SpecialPredId = spec_pred_compare
            ; SpecialPredId = spec_pred_unify
            )
        )
    ->
        % For unification/comparison the exception status depends upon the the
        % types of the arguments.  In particular whether some component of
        % that type has a user-defined equality/comparison predicate that
        % throws an exception.
        globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis,
            !IO),
        MaybeAnalysisStatus = maybe_optimal(IntermodAnalysis),
        check_vars(!.ModuleInfo, VarTypes, CallArgs, MaybeAnalysisStatus,
            !Result)
    ;
        Imported = pred_to_bool(pred_info_is_imported(CallPredInfo)),
        check_nonrecursive_call(SCC, VarTypes, CallPPId, CallArgs,
            Imported, !Result, !ModuleInfo, !IO)
    ).
check_goal_for_exceptions_2(SCC, VarTypes, Goal, GoalInfo,
        !Result, !ModuleInfo, !IO) :-
    Goal = generic_call(Details, Args, _ArgModes, _),
    globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis,
        !IO),
    (
        Details = higher_order(Var, _, _,  _),
        ClosureValueMap = goal_info_get_ho_values(GoalInfo),
        ( ClosureValues = ClosureValueMap ^ elem(Var) ->
            get_closures_exception_status(IntermodAnalysis, SCC, ClosureValues,
                MaybeWillNotThrow, MaybeAnalysisStatus, !ModuleInfo, !IO),
            (
                MaybeWillNotThrow = maybe_will_not_throw(ConditionalProcs),
                (
                    ConditionalProcs = []
                    % The possible values of the higher-order variable are all
                    % procedures that are known not to throw exceptions.
                ;
                    ConditionalProcs = [_|_],
                    %
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
                    %
                    check_vars(!.ModuleInfo, VarTypes, Args,
                        MaybeAnalysisStatus, !Result)
                )
            ;
                MaybeWillNotThrow = may_throw,
                !:Result = !.Result ^ status := may_throw(user_exception)
            )
        ;
            !:Result = !.Result ^ status := may_throw(user_exception)
        )
    ;
        % XXX We could do better with class methods.
        Details = class_method(_, _, _, _),
        !:Result = !.Result ^ status := may_throw(user_exception)
    ;
        Details = cast(_)
    ).
check_goal_for_exceptions_2(SCC, VarTypes, not(Goal), _,
        !Result, !ModuleInfo, !IO) :-
    check_goal_for_exceptions(SCC, VarTypes, Goal, !Result, !ModuleInfo, !IO).
check_goal_for_exceptions_2(SCC, VarTypes, Goal, _,
        !Result, !ModuleInfo, !IO) :-
    Goal = scope(_, ScopeGoal),
    check_goal_for_exceptions(SCC, VarTypes, ScopeGoal, !Result,
        !ModuleInfo, !IO).
check_goal_for_exceptions_2(_, _, Goal, _, !Result, !ModuleInfo ,!IO) :-
    Goal = foreign_proc(Attributes, _, _, _, _, _),
    %    
    % NOTE: for --intermodule-analysis the results for for foreign_procs will
    % *always* be optimal (since we always rely on user annotation), so
    % there's nothing to do here.
    %
    MayCallMercury = may_call_mercury(Attributes),
    ( 
        MayCallMercury = may_call_mercury,
        may_throw_exception(Attributes) = MayThrowException,
        %
        % We do not need to deal with erroneous predicates here because they
        % will have already been processed.
        %
        ( 
            MayThrowException = default_exception_behaviour,
            !:Result = !.Result ^ status := may_throw(user_exception)
        ;
            MayThrowException = will_not_throw_exception 
        )
    ;
        MayCallMercury = will_not_call_mercury
    ).
check_goal_for_exceptions_2(_, _, shorthand(_), _, _, _, _, _, _, _) :-
    unexpected(this_file,
        "shorthand goal encountered during exception analysis.").
check_goal_for_exceptions_2(SCC, VarTypes, Goal, _, !Result, !ModuleInfo,
        !IO) :-
    Goal = switch(_, _, Cases),
    CaseGoals = list.map((func(case(_, CaseGoal)) = CaseGoal), Cases),
    check_goals_for_exceptions(SCC, VarTypes, CaseGoals, !Result, !ModuleInfo,
        !IO).
check_goal_for_exceptions_2(SCC, VarTypes, Goal, _, !Result,
        !ModuleInfo, !IO) :-
    Goal = if_then_else(_, If, Then, Else),
    check_goals_for_exceptions(SCC, VarTypes, [If, Then, Else],
        !Result, !ModuleInfo, !IO).
check_goal_for_exceptions_2(SCC, VarTypes, Goal, _, !Result, !ModuleInfo,
        !IO) :-
    ( Goal = disj(Goals)
    ; Goal = conj(_, Goals)
    ),
    check_goals_for_exceptions(SCC, VarTypes, Goals, !Result, !ModuleInfo,
        !IO).

:- pred check_goals_for_exceptions(scc::in, vartypes::in,
    hlds_goals::in, proc_result::in, proc_result::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_goals_for_exceptions(_, _, [], !Result, !ModuleInfo, !IO).
check_goals_for_exceptions(SCC, VarTypes, [ Goal | Goals ], !Result,
        !ModuleInfo, !IO) :-
    check_goal_for_exceptions(SCC, VarTypes, Goal, !Result, !ModuleInfo, !IO),
    %
    % We can stop searching if we find a user exception.  However if we
    % find a type exception then we still need to check that there is
    % not a user exception somewhere in the rest of the SCC.
    %
    CurrentStatus = !.Result ^ status,
    (
        CurrentStatus = may_throw(user_exception)
    ;
        ( CurrentStatus = will_not_throw
        ; CurrentStatus = conditional
        ; CurrentStatus = may_throw(type_exception)
        ),
        check_goals_for_exceptions(SCC, VarTypes, Goals, !Result, !ModuleInfo,
            !IO)
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
:- pred get_closures_exception_status(bool::in, scc::in, set(pred_proc_id)::in,
    closures_exception_status::out, maybe(analysis_status)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

get_closures_exception_status(IntermodAnalysis, SCC, Closures,
        Conditionals, AnalysisStatus, !ModuleInfo, !IO) :-
    module_info_get_exception_info(!.ModuleInfo, ExceptionInfo),
    AnalysisStatus0 = maybe_optimal(IntermodAnalysis),
    set.fold4(
        get_closure_exception_status(IntermodAnalysis, SCC, ExceptionInfo),
        Closures, maybe_will_not_throw([]), Conditionals,
        AnalysisStatus0, AnalysisStatus, !ModuleInfo, !IO).

:- pred get_closure_exception_status(
    bool::in, scc::in, exception_info::in, pred_proc_id::in,
    closures_exception_status::in, closures_exception_status::out,
    maybe(analysis_status)::in, maybe(analysis_status)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

get_closure_exception_status(IntermodAnalysis, SCC, ExceptionInfo, PPId,
        !MaybeWillNotThrow, !AS, !ModuleInfo, !IO) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, _),
    (
        IntermodAnalysis = yes,
        pred_info_is_imported(PredInfo)
    ->
        search_analysis_status(PPId, ExceptionStatus, AnalysisStatus, SCC,
            !ModuleInfo, !IO),
        MaybeAnalysisStatus = yes(AnalysisStatus)
    ;
        ( ProcExceptionInfo = ExceptionInfo ^ elem(PPId) ->
            ProcExceptionInfo = proc_exception_info(ExceptionStatus,
                MaybeAnalysisStatus)
        ;
            ExceptionStatus = may_throw(user_exception),
            MaybeAnalysisStatus = maybe_suboptimal(IntermodAnalysis)
        )
    ),
    (
        !.MaybeWillNotThrow = may_throw
    ;
        !.MaybeWillNotThrow = maybe_will_not_throw(Conditionals),
        (
            ExceptionStatus = conditional,
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
    !:Result = !.Result ^ status := NewStatus,
    !:Result = !.Result ^ maybe_analysis_status := NewAnalysisStatus.

:- func combine_exception_status(exception_status, exception_status)
    = exception_status.

combine_exception_status(will_not_throw, Y) = Y.
combine_exception_status(X @ may_throw(user_exception), _) = X.
combine_exception_status(X @ may_throw(type_exception), will_not_throw) = X.
combine_exception_status(X @ may_throw(type_exception), conditional) = X.
combine_exception_status(may_throw(type_exception), Y @ may_throw(_)) = Y.
combine_exception_status(conditional, conditional) = conditional.
combine_exception_status(conditional, will_not_throw) = conditional.
combine_exception_status(conditional, Y @ may_throw(_)) = Y.

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

:- pred check_nonrecursive_call(scc::in, vartypes::in,
    pred_proc_id::in, prog_vars::in, bool::in, proc_result::in,
    proc_result::out, module_info::in, module_info::out, io::di, io::uo) is det.

check_nonrecursive_call(SCC, VarTypes, PPId, Args, Imported, !Result,
        !ModuleInfo, !IO) :-
    globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis, !IO),
    (
        % If we are using `--intermodule-analysis' then use the analysis
        % framework for imported procedures.
        IntermodAnalysis = yes,
        Imported = yes
    ->
        search_analysis_status(PPId, CalleeResult, AnalysisStatus, SCC,
            !ModuleInfo, !IO),
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
                CalleeExceptionStatus = conditional,
                check_vars(!.ModuleInfo, VarTypes, Args, MaybeAnalysisStatus,
                    !Result)
            )
        ;
            % If we do not have any information about the callee procedure
            % then assume that it might throw an exception.
            MaybeAnalysisStatus = maybe_suboptimal(IntermodAnalysis),
            update_proc_result(may_throw(user_exception), MaybeAnalysisStatus,
                !Result)
        )
    ).
:- pred check_vars(module_info::in, vartypes::in, prog_vars::in,
    maybe(analysis_status)::in, proc_result::in, proc_result::out) is det.

check_vars(ModuleInfo, VarTypes, Vars, MaybeAnalysisStatus, !Result) :-
    Types = list.map((func(Var) = VarTypes ^ det_elem(Var)), Vars),
    TypeStatus = check_types(ModuleInfo, Types),
    (
        TypeStatus = type_will_not_throw
    ;
        TypeStatus = type_may_throw,
        update_proc_result(may_throw(type_exception), MaybeAnalysisStatus,
            !Result)
    ;
        TypeStatus = type_conditional,
        update_proc_result(conditional, MaybeAnalysisStatus,
            !Result)
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
        all [TypeStatus] list.member(Result, Results) =>
            Result ^ rec_calls \= type_may_throw
    ->
        conditional
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

    ;   type_may_throw
            % This type has a user-defined equality or comparison
            % predicate that is known to throw an exception.

    ;   type_conditional.
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
        ( is_solver_type(ModuleInfo, Type)
        ; is_existq_type(ModuleInfo, Type))
     ->
        % XXX At the moment we just assume that existential
        % types and solver types result in a type exception
        % being thrown.
        Status = type_may_throw
    ;
        TypeCategory = classify_type(ModuleInfo, Type),
        Status = check_type_2(ModuleInfo, Type, TypeCategory)
    ).

:- func check_type_2(module_info, mer_type, type_category) = type_status.

check_type_2(_, _, type_cat_int) = type_will_not_throw.
check_type_2(_, _, type_cat_char) = type_will_not_throw.
check_type_2(_, _, type_cat_string) = type_will_not_throw.
check_type_2(_, _, type_cat_float) = type_will_not_throw.
check_type_2(_, _, type_cat_higher_order) = type_will_not_throw.
check_type_2(_, _, type_cat_type_info) = type_will_not_throw.
check_type_2(_, _, type_cat_type_ctor_info) = type_will_not_throw.
check_type_2(_, _, type_cat_typeclass_info) = type_will_not_throw.
check_type_2(_, _, type_cat_base_typeclass_info) = type_will_not_throw.
check_type_2(_, _, type_cat_void) = type_will_not_throw.
check_type_2(_, _, type_cat_dummy) = type_will_not_throw.

check_type_2(_, _, type_cat_variable) = type_conditional.

check_type_2(ModuleInfo, Type, type_cat_tuple) =
    check_user_type(ModuleInfo, Type).
check_type_2(ModuleInfo, Type, type_cat_enum) =
    check_user_type(ModuleInfo, Type).
check_type_2(ModuleInfo, Type, type_cat_user_ctor) =
    check_user_type(ModuleInfo, Type).

:- func check_user_type(module_info, mer_type) = type_status.

check_user_type(ModuleInfo, Type) = Status :-
    ( type_to_ctor_and_args(Type, _TypeCtor, Args) ->
        (
            type_has_user_defined_equality_pred(ModuleInfo, Type,
                _UnifyCompare)
        ->
            % XXX We can do better than this by examining what these preds
            % actually do. Something similar needs to be sorted out for
            % termination analysis as well, so we'll wait until that is done.
            Status = type_may_throw
        ;
            Status = check_types(ModuleInfo, Args)
        )
    ;
        unexpected(this_file, "Unable to get ctor and args.")
    ).

%----------------------------------------------------------------------------%
%
% Types and instances for the intermodule analysis framework
%

:- type exception_analysis_answer
    --->    exception_analysis_answer(exception_status).

:- func analysis_name = string.

analysis_name = "exception_analysis".

:- instance analysis(any_call, exception_analysis_answer) where [
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 1,
    preferred_fixpoint_type(_, _) = least_fixpoint,
    bottom(_) = exception_analysis_answer(will_not_throw),
    top(_) = exception_analysis_answer(may_throw(user_exception))
].

:- instance answer_pattern(exception_analysis_answer) where [].
:- instance partial_order(exception_analysis_answer) where [
    (more_precise_than(
            exception_analysis_answer(Status1),
            exception_analysis_answer(Status2)) :-
        exception_status_more_precise_than(Status1, Status2)),
    equivalent(Status, Status)
].

:- pred exception_status_more_precise_than(exception_status::in,
    exception_status::in) is semidet.

exception_status_more_precise_than(will_not_throw, conditional).
exception_status_more_precise_than(will_not_throw, may_throw(_)).
exception_status_more_precise_than(conditional, may_throw(_)).
exception_status_more_precise_than(may_throw(type_exception),
    may_throw(user_exception)).

:- instance to_string(exception_analysis_answer) where [
    func(to_string/1) is answer_to_string,
    func(from_string/1) is answer_from_string
].

:- func answer_to_string(exception_analysis_answer) = string.

answer_to_string(Answer) = String :-
    Answer = exception_analysis_answer(Status),
    exception_status_to_string(Status, String).

:- func answer_from_string(string) = exception_analysis_answer is semidet.

answer_from_string(String) = exception_analysis_answer(Status) :-
    exception_status_to_string(Status, String).

:- pred exception_status_to_string(exception_status, string).
:- mode exception_status_to_string(in, out) is det.
:- mode exception_status_to_string(out, in) is semidet.

exception_status_to_string(will_not_throw, "will_not_throw").
exception_status_to_string(conditional, "conditional").
exception_status_to_string(may_throw(type_exception),
    "may_throw(type_exception)").
exception_status_to_string(may_throw(user_exception),
    "may_throw(user_exception)").

%----------------------------------------------------------------------------%
%
% Additional predicates used for intermodule analysis
%

:- pred search_analysis_status(pred_proc_id::in,
    exception_status::out, analysis_status::out, scc::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

search_analysis_status(PPId, Result, AnalysisStatus, CallerSCC,
        !ModuleInfo, !IO) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    search_analysis_status_2(!.ModuleInfo, PPId, Result, AnalysisStatus,
        CallerSCC, AnalysisInfo0, AnalysisInfo, !IO),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred search_analysis_status_2(module_info::in, pred_proc_id::in,
    exception_status::out, analysis_status::out, scc::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

search_analysis_status_2(ModuleInfo, PPId, Result, AnalysisStatus, CallerSCC,
        !AnalysisInfo, !IO) :-
    module_id_func_id(ModuleInfo, PPId, ModuleId, FuncId),
    Call = any_call,
    lookup_best_result(ModuleId, FuncId, Call, MaybeBestStatus, !AnalysisInfo,
        !IO),
    globals__io_lookup_bool_option(make_analysis_registry,
        MakeAnalysisRegistry, !IO),
    (
        MaybeBestStatus = yes({BestCall, exception_analysis_answer(Result),
            AnalysisStatus}),
        (
            MakeAnalysisRegistry = yes,
            record_dependencies(ModuleId, FuncId, BestCall, ModuleInfo,
                CallerSCC, !AnalysisInfo)
        ;
            MakeAnalysisRegistry = no
        )
    ;
        MaybeBestStatus = no,
        % If we do not have any information about the callee procedure then
        % assume that it throws an exception.
        top(Call) = Answer,
        Answer = exception_analysis_answer(Result),
        module_is_local(mmc, ModuleId, IsLocal, !IO),
        (
            IsLocal = yes,
            AnalysisStatus = suboptimal,
            (
                MakeAnalysisRegistry = yes,
                analysis.record_result(ModuleId, FuncId,
                    Call, Answer, AnalysisStatus, !AnalysisInfo),
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

    % XXX If the procedures in CallerSCC definitely come from the
    % same module then we don't need to record the dependency so many
    % times, at least while we only have module-level granularity.
    %
:- pred record_dependencies(module_id::in, func_id::in, Call::in,
    module_info::in, scc::in, analysis_info::in, analysis_info::out)
    is det <= call_pattern(Call).

record_dependencies(ModuleId, FuncId, Call,
        ModuleInfo, CallerSCC, !AnalysisInfo) :-
    list.foldl((pred(CallerPPId::in, Info0::in, Info::out) is det :-
        module_id_func_id(ModuleInfo, CallerPPId,
            CallerModuleId, _),
        record_dependency(CallerModuleId,
            analysis_name, ModuleId, FuncId, Call, Info0, Info)
    ), CallerSCC, !AnalysisInfo).

:- pred record_exception_analysis_results(exception_status::in, 
    analysis_status::in, scc::in, module_info::in, module_info::out) is det.

record_exception_analysis_results(Status, ResultStatus, SCC, !ModuleInfo) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    list.foldl(
        record_exception_analysis_result(!.ModuleInfo, Status, ResultStatus),
        SCC, AnalysisInfo0, AnalysisInfo),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred record_exception_analysis_result(module_info::in, exception_status::in, 
    analysis_status::in, pred_proc_id::in,
    analysis_info::in, analysis_info::out) is det.

record_exception_analysis_result(ModuleInfo, Status, ResultStatus, PPId,
        !AnalysisInfo) :-
    PPId = proc(PredId, _ProcId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    should_write_exception_info(ModuleInfo, PredId, PredInfo, ShouldWrite),
    (   
        ShouldWrite = yes,
        module_id_func_id(ModuleInfo, PPId, ModuleId, FuncId),
        record_result(ModuleId, FuncId, any_call,
            exception_analysis_answer(Status), ResultStatus,
            !AnalysisInfo)
    ;
        ShouldWrite = no
    ).

:- pred should_write_exception_info(module_info::in, pred_id::in, 
        pred_info::in, bool::out) is det.

should_write_exception_info(ModuleInfo, PredId, PredInfo, ShouldWrite) :-
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

:- func maybe_optimal(bool) = maybe(analysis_status).

maybe_optimal(no)  = no.
maybe_optimal(yes) = yes(optimal). 

:- func maybe_suboptimal(bool) = maybe(analysis_status).

maybe_suboptimal(no)  = no.
maybe_suboptimal(yes) = yes(suboptimal).

%----------------------------------------------------------------------------%
%
% Stuff for intermodule optimization.
%

:- pred make_optimization_interface(module_info::in, io::di, io::uo)
    is det.

make_optimization_interface(ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(ModuleName, ".opt.tmp", no, OptFileName, !IO),
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose,
        "% Appending exceptions pragmas to `", !IO),
    maybe_write_string(Verbose, OptFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        module_info_get_exception_info(ModuleInfo, ExceptionInfo),
        module_info_predids(ModuleInfo, PredIds),
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
    should_write_exception_info(ModuleInfo, PredId, PredInfo, ShouldWrite),
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
                map.search(ExceptionInfo, proc(PredId, ProcId),
                    ProcExceptionInfo)
            ->
                ProcExceptionInfo = proc_exception_info(Status, _), 
                mercury_output_pragma_exceptions(PredOrFunc,
                    qualified(ModuleName, Name), Arity, ModeNum, Status, !IO)
            ;
                true
            )), ProcIds, !IO)
    ;
        ShouldWrite = no      
    ).

%----------------------------------------------------------------------------%
% 
% External interface to exception analysis information
%

lookup_exception_analysis_result(PPId, ExceptionStatus, !ModuleInfo, !IO) :-
    PPId = proc(PredId, _),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    IsImported = pred_to_bool(pred_info_is_imported(PredInfo)),
    globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis,
        !IO),
    %
    % If we the procedure we are calling is imported and we are using
    % intermodule-analysis then we need to look up the exception status in the
    % analysis registry; otherwise we look it up in the the exception_info
    % table.
    %
    UseAnalysisRegistry = IsImported `bool.and` IntermodAnalysis,
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
            module_id_func_id(!.ModuleInfo, PPId, ModuleId, FuncId),
            lookup_best_result(ModuleId, FuncId, any_call, MaybeBestStatus,
                !AnalysisInfo, !IO),
            (
                MaybeBestStatus = yes({_Call, Answer, AnalysisStatus}),
                ( AnalysisStatus = invalid ->
                    unexpected(this_file,
                        "invalid exception_analysis answer")
                ;
                    Answer = exception_analysis_answer(ExceptionStatus)
                )
            ;
                MaybeBestStatus = no,
                ExceptionStatus = may_throw(user_exception) 
            ),
            module_info_get_name(!.ModuleInfo, ThisModuleName),
            ThisModuleId = module_name_to_module_id(ThisModuleName),
            record_dependency(ThisModuleId, analysis_name, ModuleId, FuncId,
                any_call, !AnalysisInfo),
            module_info_set_analysis_info(!.AnalysisInfo, !ModuleInfo)
        )
    ).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "exception_analysis.m".

%----------------------------------------------------------------------------%
:- end_module exception_analysis.
%----------------------------------------------------------------------------%
