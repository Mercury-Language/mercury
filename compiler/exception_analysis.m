%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File    : exception_analysis.m
% Author  : juliensf
%
% This module performs an exception tracing analysis.  The aim is to
% annotate the HLDS with information about whether each procedure
% might or will not throw an exception.
% 
% This information can be useful to the compiler when applying
% certain types of optimization.
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
%   - use intermodule-analysis framework
%   - check what user-defined equality and comparison preds
%     actually do rather than assuming that they always
%     may throw exceptions.
%   - handle existential and solver types - currently we just
%     assume that any call to unify or compare for these types
%     might result in an exception being thrown. 
%
% XXX We need to be a bit careful with transformations like tabling that
% might add calls to exception.throw - at the moment this isn't a problem
% because exception analysis takes place after the tabling transformation.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.exception_analysis.

:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

    % Perform the exception analysis on a module.
    %
:- pred exception_analysis.process_module(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Write out the exception pragmas for this module.
    %
:- pred exception_analysis.write_pragma_exceptions(module_info::in,
    exception_info::in, pred_id::in, io::di, io::uo) is det.

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
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.dependency_graph.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.

%----------------------------------------------------------------------------%
%
% Perform exception analysis on a module. 
%

exception_analysis.process_module(!ModuleInfo, !IO) :-
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
    list.foldl(process_scc, SCCs, !ModuleInfo),
    globals.io_lookup_bool_option(make_optimization_interface,
        MakeOptInt, !IO),
    ( if    MakeOptInt = yes
      then  exception_analysis.make_opt_int(!.ModuleInfo, !IO)
      else  true
    ).  

%----------------------------------------------------------------------------%
% 
% Perform exception analysis on a SCC. 
%

:- type scc == list(pred_proc_id).

:- type proc_results == list(proc_result).

:- type proc_result 
    ---> proc_result(
            ppid   :: pred_proc_id,
 
            status :: exception_status,
                    % Exception status of this procedure
                    % not counting any input from
                    % (mutually-)recursive inputs.    
            rec_calls :: type_status
                    % The collective type status of the 
                    % types of the terms that are arguments
                    % of (mutually-)recursive calls. 
    ).

:- pred process_scc(scc::in, module_info::in, module_info::out) is det.

process_scc(SCC, !ModuleInfo) :-
    ProcResults = check_procs_for_exceptions(SCC, !.ModuleInfo),
    % 
    % The `Results' above are the results of analysing each
    % individual procedure in the SCC - we now have to combine
    % them in a meaningful way.   
    %
    Status = combine_individual_proc_results(ProcResults),
    %
    % Update the exception info. with information about this SCC.
    %
    module_info_exception_info(!.ModuleInfo, ExceptionInfo0),
    Update = (pred(PPId::in, Info0::in, Info::out) is det :-
        Info = Info0 ^ elem(PPId) := Status
    ),
    list.foldl(Update, SCC, ExceptionInfo0, ExceptionInfo),
    module_info_set_exception_info(ExceptionInfo, !ModuleInfo). 

    % Check each procedure in the SCC individually.
    %
:- func check_procs_for_exceptions(scc, module_info) = proc_results.

check_procs_for_exceptions(SCC, ModuleInfo) = Result :-
    list.foldl(check_proc_for_exceptions(SCC, ModuleInfo), SCC, [], Result).

    % Examine how the procedures interact with other procedures that
    % are mutually-recursive to them.
    %
:- func combine_individual_proc_results(proc_results) = exception_status.

combine_individual_proc_results([]) = _ :-
    unexpected(this_file, "Empty SCC during exception analysis.").
combine_individual_proc_results(ProcResults @ [_|_]) = SCC_Result :- 
    (
        % If none of the procedures may throw an exception or 
        % are conditional then the SCC cannot throw an exception
        % either.
        all [ProcResult] list.member(ProcResult, ProcResults) =>
            ProcResult ^ status = will_not_throw    
    ->
        SCC_Result = will_not_throw 
    ;
        % If none of the procedures may throw an exception but
        % at least one of them is conditional then somewhere in
        % the SCC there is a call to unify or compare that may
        % rely on the types of the polymorphically typed
        % arguments.  
        %
        % We need to check that any recursive calls
        % do not introduce types that might have user-defined
        % equality or comparison predicate that throw
        % exceptions. 
        all [EResult] list.member(EResult, ProcResults) =>
            EResult ^ status \= may_throw(_),
        some [CResult] (
            list.member(CResult, ProcResults),
            CResult ^ status = conditional
        )
    ->
        SCC_Result = handle_mixed_conditional_scc(ProcResults)
    ;
        % If none of the procedures can throw a user_exception
        % but one or more can throw a type_exception then mark
        % the SCC as maybe throwing a type_exception.
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
    ).

%----------------------------------------------------------------------------%
%
% Process individual procedures.
% 

:- pred check_proc_for_exceptions(scc::in, module_info::in,
    pred_proc_id::in, proc_results::in, proc_results::out) is det.

check_proc_for_exceptions(SCC, ModuleInfo, PPId, !Results) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_goal(ProcInfo, Body),
    proc_info_vartypes(ProcInfo, VarTypes),
    Result0 = proc_result(PPId, will_not_throw, type_will_not_throw),
    check_goal_for_exceptions(SCC, ModuleInfo, VarTypes, Body, Result0,
        Result),
    list.cons(Result, !Results).

:- pred check_goal_for_exceptions(scc::in, module_info::in, vartypes::in,
    hlds_goal::in, proc_result::in, proc_result::out) is det.  

check_goal_for_exceptions(SCC, ModuleInfo, VarTypes, Goal - GoalInfo,
        !Result) :-
    ( goal_info_get_determinism(GoalInfo, erroneous) ->
        !:Result = !.Result ^ status := may_throw(user_exception)
    ;
        check_goal_for_exceptions_2(SCC, ModuleInfo, VarTypes, Goal, GoalInfo,
            !Result)
    ).  

:- pred check_goal_for_exceptions_2(scc::in, module_info::in, vartypes::in,
    hlds_goal_expr::in, hlds_goal_info::in, proc_result::in, proc_result::out)
    is det.

check_goal_for_exceptions_2(_, _, _, Goal, _, !Result) :-
    Goal = unify(_, _, _, Kind, _),
    ( Kind = complicated_unify(_, _, _) ->
        unexpected(this_file,
            "complicated unify during exception analysis.") 
    ;
        true
    ).
check_goal_for_exceptions_2(SCC, ModuleInfo, VarTypes, Goal, _, !Result) :-
    Goal = call(CallPredId, CallProcId, CallArgs, _, _, _),
    CallPPId = proc(CallPredId, CallProcId),    
    module_info_pred_info(ModuleInfo, CallPredId, CallPredInfo),
    (
        % Handle (mutually-)recursive calls.
        list.member(CallPPId, SCC) 
    ->
        Types = list.map((func(Var) = VarTypes ^ det_elem(Var)),
            CallArgs),
        TypeStatus = check_types(ModuleInfo, Types),
        combine_type_status(TypeStatus, !.Result ^ rec_calls,
            NewTypeStatus),
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
            ( SpecialPredId = compare ; SpecialPredId = unify ),
            special_pred_name_arity(SpecialPredId, Name, Arity)
        ;
            pred_info_get_origin(CallPredInfo, Origin),
            Origin = special_pred(SpecialPredId - _),
            ( SpecialPredId = compare ; SpecialPredId = unify )
        )   
    ->
        % For unification/comparison the exception status depends
        % upon the the types of the arguments.  In particular
        % whether some component of that type has a user-defined
        % equality/comparison predicate that throws an exception.
        check_vars(ModuleInfo, VarTypes, CallArgs, !Result) 
    ;
        check_nonrecursive_call(ModuleInfo, VarTypes, CallPPId, CallArgs,
            !Result)
    ).
check_goal_for_exceptions_2(_, ModuleInfo, VarTypes, Goal, GoalInfo,
        !Result) :-
    Goal = generic_call(Details, Args, _ArgModes, _),
    (
        Details = higher_order(Var, _, _,  _),
        ClosureValueMap = goal_info_get_ho_values(GoalInfo),
        ( ClosureValues = ClosureValueMap ^ elem(Var) ->
                (
                    get_conditional_closures(ModuleInfo, ClosureValues,
                        Conditional)
                ->
                    (
                        Conditional = []
                        % The possible values of the higher-order variable
                        % are all procedures that are known not to throw
                        % exceptions.
                    ;
                        Conditional = [_|_],
                        %
                        % For 'conditional' procedures we need to make
                        % sure that if any type variables are bound at
                        % the generic_call site, then this does not
                        % cause the closure to throw an exception
                        % (because of a user-defined equality or
                        % comparison predicate that throws an
                        % exception.)
                        %
                        % If we can resolve all of the polymorphism at
                        % this generic_call site, then we can reach a
                        % definite conclusion about it.
                        % 
                        % If we cannot do so, then we propagate the
                        % 'conditional' status to the current predicate
                        % if all the type variables involved are
                        % universally quantified, or mark it as throwing
                        % an exception if some of them are existentially
                        % quantified. 
                        %
                        % XXX This is too conservative but we don't
                        % currently perform a fine-grained enough
                        % analysis of where out-of-line
                        % unifications/comparisons occur to be able to
                        % do better.
                        %
                        check_vars(ModuleInfo, VarTypes, Args, !Result)
                    )
                ;
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
    ;
        Details = aditi_builtin(_, _),
        !:Result = !.Result ^ status := may_throw(user_exception)
    ).
check_goal_for_exceptions_2(SCC, ModuleInfo, VarTypes, not(Goal), _,
        !Result) :-
    check_goal_for_exceptions(SCC, ModuleInfo, VarTypes, Goal, !Result).
check_goal_for_exceptions_2(SCC, ModuleInfo, VarTypes, Goal, _,
        !Result) :-
    Goal = scope(_, ScopeGoal),
    check_goal_for_exceptions(SCC, ModuleInfo, VarTypes, ScopeGoal, !Result).
check_goal_for_exceptions_2(_, _, _, Goal, _, !Result) :-
    Goal = foreign_proc(Attributes, _, _, _, _, _),
    ( may_call_mercury(Attributes) = may_call_mercury -> 
        may_throw_exception(Attributes) = MayThrowException,
        %
        % We do not need to deal with erroneous predicates
        % here because they will have already been processed.
        %
        ( MayThrowException = default_exception_behaviour ->
            !:Result = !.Result ^ status := may_throw(user_exception)
        ;
            true
        )   
    ;
        true
    ).
check_goal_for_exceptions_2(_, _, _, shorthand(_), _, _, _) :-
    unexpected(this_file,
        "shorthand goal encountered during exception analysis.").
check_goal_for_exceptions_2(SCC, ModuleInfo, VarTypes, Goal, _, !Result) :-
    Goal = switch(_, _, Cases),
    CaseGoals = list.map((func(case(_, CaseGoal)) = CaseGoal), Cases),
    check_goals_for_exceptions(SCC, ModuleInfo, VarTypes, CaseGoals, !Result).
check_goal_for_exceptions_2(SCC, ModuleInfo, VarTypes, Goal, _, !Result) :-
    Goal = if_then_else(_, If, Then, Else),
    check_goals_for_exceptions(SCC, ModuleInfo, VarTypes, [If, Then, Else],
        !Result).   
check_goal_for_exceptions_2(SCC, ModuleInfo, VarTypes, disj(Goals), _,
        !Result) :-
    check_goals_for_exceptions(SCC, ModuleInfo, VarTypes, Goals, !Result).
check_goal_for_exceptions_2(SCC, ModuleInfo, VarTypes, par_conj(Goals), _,
        !Result) :-
    check_goals_for_exceptions(SCC, ModuleInfo, VarTypes, Goals, !Result).
check_goal_for_exceptions_2(SCC, ModuleInfo, VarTypes, conj(Goals), _,
        !Result) :-
    check_goals_for_exceptions(SCC, ModuleInfo, VarTypes, Goals, !Result).

:- pred check_goals_for_exceptions(scc::in, module_info::in, vartypes::in,
    hlds_goals::in, proc_result::in, proc_result::out) is det.

check_goals_for_exceptions(_, _, _, [], !Result).
check_goals_for_exceptions(SCC, ModuleInfo, VarTypes, [ Goal | Goals ],
        !Result) :-
    check_goal_for_exceptions(SCC, ModuleInfo, VarTypes, Goal, !Result),
    %
    % We can stop searching if we find a user exception.  However if we
    % find a type exception then we still need to check that there is 
    % not a user exception somewhere in the rest of the SCC.
    %
    ( if    !.Result ^ status = may_throw(user_exception)
      then  true
      else  check_goals_for_exceptions(SCC, ModuleInfo, VarTypes, Goals,
                !Result)
    ).

%----------------------------------------------------------------------------%
%
% Further code to handle higher-order variables
% 

    % Given a list of procedure ids extract those whose exception status
    % has been set to 'conditional'.  Fails if one of the procedures in
    % the set has an exception status that indicates it may throw an
    % exception, or if the exception status for a procedure has not yet
    % been set.
    %
:- pred get_conditional_closures(module_info::in, set(pred_proc_id)::in,
    list(pred_proc_id)::out) is semidet.

get_conditional_closures(ModuleInfo, Closures, Conditionals) :-
    module_info_exception_info(ModuleInfo, ExceptionInfo),
    set.fold(get_conditional_closure(ExceptionInfo), Closures,
        [], Conditionals).

:- pred get_conditional_closure(exception_info::in, pred_proc_id::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is semidet.

get_conditional_closure(ExceptionInfo, PPId, !Conditionals) :-
    ExceptionInfo ^ elem(PPId) = Status,
    (
        Status = conditional,
        list.cons(PPId, !Conditionals)
    ;
        Status = will_not_throw
    ).

%----------------------------------------------------------------------------%

:- pred update_proc_result(exception_status::in, proc_result::in,
    proc_result::out) is det.

update_proc_result(CurrentStatus, !Result) :-
    OldStatus = !.Result ^ status,
    NewStatus = combine_exception_status(CurrentStatus, OldStatus),
    !:Result  = !.Result ^ status := NewStatus. 

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

%----------------------------------------------------------------------------%
% 
% Extra procedures for handling calls.
%

:- pred check_nonrecursive_call(module_info::in, vartypes::in,
    pred_proc_id::in, prog_vars::in, proc_result::in,
    proc_result::out) is det.

check_nonrecursive_call(ModuleInfo, VarTypes, PPId, Args, !Result) :-
    module_info_exception_info(ModuleInfo, ExceptionInfo),
    ( map.search(ExceptionInfo, PPId, CalleeExceptionStatus) ->
        (
            CalleeExceptionStatus = will_not_throw
        ;
            CalleeExceptionStatus = may_throw(ExceptionType),
            update_proc_result(may_throw(ExceptionType), !Result)
        ;
            CalleeExceptionStatus = conditional,
            check_vars(ModuleInfo, VarTypes, Args, !Result) 
        )
    ;
        % If we do not have any information about the callee procedure
        % then assume that it might throw an exception. 
        update_proc_result(may_throw(user_exception), !Result)
    ).

:- pred check_vars(module_info::in, vartypes::in, prog_vars::in, 
    proc_result::in, proc_result::out) is det.

check_vars(ModuleInfo, VarTypes, Vars, !Result) :- 
    Types = list.map((func(Var) = VarTypes ^ det_elem(Var)), Vars),
    TypeStatus = check_types(ModuleInfo, Types),
    (
        TypeStatus = type_will_not_throw
    ;
        TypeStatus = type_may_throw,
        update_proc_result(may_throw(type_exception), !Result)
    ;   
        TypeStatus = type_conditional,
        update_proc_result(conditional, !Result)
    ).

%----------------------------------------------------------------------------%
%
% Predicates for checking mixed SCCs. 
%
% A "mixed SCC" is one where at least one of the procedures in the SCC is
% known not to throw an exception, at least one of them is conditional
% and none of them may throw an exception (of either sort).
%
% In order to determine the status of such a SCC we also need to take the
% effect of the recursive calls into account.  This is because calls to a
% conditional procedure from a procedure that is mutually recursive to it may 
% introduce types that could cause a type_exception to be thrown.  
%
% We currently assume that if these types are introduced
% somewhere in the SCC then they may be propagated around the entire
% SCC - hence if a part of the SCC is conditional we need to make
% sure other parts don't supply it with input whose types may have
% user-defined equality/comparison predicates. 
%
% NOTE: it is possible to write rather contrived programs that can 
% exhibit rather strange behaviour which is why all this is necessary. 
    
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
% By saying a `type can throw an exception' we mean that an exception
% might be thrown as a result of a unification or comparison involving
% the type because it has a user-defined equality/comparison predicate
% that may throw an exception. 
%
% XXX We don't actually need to examine all the types, just those
% that are potentially going to be involved in unification/comparisons.
% At the moment we don't keep track of that information so the current
% procedure is as follows:
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
:- func check_types(module_info, list((type))) = type_status.

check_types(ModuleInfo, Types) = Status :-
    list.foldl(check_type(ModuleInfo), Types, type_will_not_throw, Status).

:- pred check_type(module_info::in, (type)::in, type_status::in,
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
:- func check_type(module_info, (type)) = type_status.

check_type(ModuleInfo, Type) = Status :-
    ( 
        ( type_util.is_solver_type(ModuleInfo, Type)
        ; type_util.is_existq_type(ModuleInfo, Type))
     ->
        % XXX At the moment we just assume that existential
        % types and solver types result in a type exception
        % being thrown.
        Status = type_may_throw
    ;   
        TypeCategory = type_util.classify_type(ModuleInfo, Type),
        Status = check_type_2(ModuleInfo, Type, TypeCategory)
    ).

:- func check_type_2(module_info, (type), type_category) = type_status.

check_type_2(_, _, int_type) = type_will_not_throw.
check_type_2(_, _, char_type) = type_will_not_throw.
check_type_2(_, _, str_type) = type_will_not_throw.
check_type_2(_, _, float_type) = type_will_not_throw.
check_type_2(_, _, higher_order_type) = type_will_not_throw.
check_type_2(_, _, type_info_type) = type_will_not_throw.
check_type_2(_, _, type_ctor_info_type) = type_will_not_throw.
check_type_2(_, _, typeclass_info_type) = type_will_not_throw.
check_type_2(_, _, base_typeclass_info_type) = type_will_not_throw.
check_type_2(_, _, void_type) = type_will_not_throw.

check_type_2(_, _, variable_type) = type_conditional.

check_type_2(ModuleInfo, Type, tuple_type) = check_user_type(ModuleInfo, Type).
check_type_2(ModuleInfo, Type, enum_type)  = check_user_type(ModuleInfo, Type). 
check_type_2(ModuleInfo, Type, user_ctor_type) =
    check_user_type(ModuleInfo, Type). 

:- func check_user_type(module_info, (type)) = type_status.

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
            Status = type_may_throw
        ;
            Status = check_types(ModuleInfo, Args)
        )
    
    ;
        unexpected(this_file, "Unable to get ctor and args.")
    ). 

%----------------------------------------------------------------------------%
%
% Stuff for intermodule optimization.
% 

:- pred exception_analysis.make_opt_int(module_info::in, io::di, io::uo) is det.

exception_analysis.make_opt_int(ModuleInfo, !IO) :-
    module_info_name(ModuleInfo, ModuleName),
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
        module_info_exception_info(ModuleInfo, ExceptionInfo), 
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
    pred_info_import_status(PredInfo, ImportStatus),
    (   
        ( ImportStatus = exported 
        ; ImportStatus = opt_exported 
        ),
        not is_unify_or_compare_pred(PredInfo),
        module_info_type_spec_info(ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
        not set.member(PredId, TypeSpecForcePreds),
        %
        % XXX Writing out pragmas for the automatically
        % generated class instance methods causes the
        % compiler to abort when it reads them back in.
        %
        pred_info_get_markers(PredInfo, Markers),
        not check_marker(Markers, class_instance_method),
        not check_marker(Markers, named_class_instance_method)
    ->
        ModuleName = pred_info_module(PredInfo),
        Name       = pred_info_name(PredInfo),
        Arity      = pred_info_orig_arity(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        ProcIds    = pred_info_procids(PredInfo),
        %
        % XXX The termination analyser outputs pragmas even if
        % it doesn't have any information - should we be doing
        % this?
        %
        list.foldl((pred(ProcId::in, !.IO::di, !:IO::uo) is det :-
            proc_id_to_int(ProcId, ModeNum),
            ( 
                map.search(ExceptionInfo, proc(PredId, ProcId),
                    Status)
            ->
                mercury_output_pragma_exceptions(PredOrFunc, 
                    qualified(ModuleName, Name), Arity,
                    ModeNum, Status, !IO)
            ;
                true
            )), ProcIds, !IO)
    ;
        true
    ).          

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "exception_analysis.m".

%----------------------------------------------------------------------------%
:- end_module exception_analysis.
%----------------------------------------------------------------------------%
