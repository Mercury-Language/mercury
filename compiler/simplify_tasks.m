%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: simplify_tasks.m.
%
% This module handles the specification of what tasks the submodules of
% simplify.m should perform.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_tasks.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.

:- import_module list.

    % Each value of this type represents a task, or a group of related tasks,
    % that simplification should perform.
:- type simplify_task
    --->    simptask_warn_simple_code
            % --warn-simple-code

    ;       simptask_warn_duplicate_calls
            % --warn-duplicate-calls

    ;       simptask_warn_implicit_stream_calls
            % --warn-implicit-stream-calls

    ;       simptask_format_calls
            % Invoke format_call.m.

    ;       simptask_warn_obsolete
            % --warn-obsolete

    ;       simptask_mark_code_model_changes
            % Some compiler passes generate HLDS in which changes in code model
            % are implicit in the determinisms stored in the goal_infos of
            % nested goals (such as a conjunction without outputs being
            % semidet, and some conjuncts being nondet). Make these code model
            % changes visible by adding a scope wrapper.

    ;       simptask_after_front_end
            % Do the tasks that should be done at the end of the front end.

    ;       simptask_excess_assigns
            % Remove excess assignment unifications.

    ;       simptask_test_after_switch
            % Optimize away test unifications after switches whose arms
            % do nothing except set the to-be-tested variable.

    ;       simptask_elim_removable_scopes
            % Remove scopes that do not need processing during LLDS code
            % generation.

    ;       simptask_opt_duplicate_calls
            % Optimize duplicate calls.

    ;       simptask_constant_prop
            % Partially evaluate calls.

    ;       simptask_common_structs
            % Common structure elimination.

    ;       simptask_extra_common_structs
            % Do common structure elimination even when it might
            % increase stack usage (used by deforestation).

    ;       simptask_try_opt_const_structs
            % If the backend and the options allow it, replace unifications
            % that construct constant terms with the assignment of a
            % ground_term_const cons_id to the relevant variable.

    ;       simptask_ignore_par_conjs
            % Replace parallel conjunctions with plain conjunctions.

    ;       simptask_warn_suspicious_recursion
            % With simptask_warn_simple_code, we generate warnings
            % for recursive calls in which all input arguments are
            % the same as in the clause head. These calls are guaranteed
            % to represent an infinite loop.
            % 
            % If simptask_warn_suspicious_recursion is also set, we also
            % generate a weaker version of that warning for recursive calls
            % in which all *non-state-var* input arguments are the same as
            % in the clause head. In the usual case where the recursion is
            % controlled by the non-state-var input arguments, these calls
            % also represent an infinite loop, but we cannot say that for
            % certain, because in some cases, the predicate is recursing
            % on a data structure that the code *does* refer to using
            % state variable notation.

    ;       simptask_warn_no_solution_disjunct
            % Warn about disjuncts that can have no solution.

    ;       simptask_split_switch_arms.
            % Invoke split_switch_arms.m to perform its transformation,
            % if the main part of simplification discovers that it has
            % some redundant switches to optimize.
            %
            % For the details of the transformation done by
            % split_switch_arms.m, see its top-of-module comment.

%---------------------%

:- type maybe_warn_simple_code
    --->    do_not_warn_simple_code
    ;       warn_simple_code.

:- type maybe_warn_duplicate_calls
    --->    do_not_warn_duplicate_calls
    ;       warn_duplicate_calls.

:- type maybe_warn_implicit_streams
    --->    do_not_warn_implicit_streams
    ;       warn_implicit_streams.

:- type maybe_invoke_format_call
    --->    do_not_invoke_format_call
    ;       invoke_format_call.

:- type maybe_warn_obsolete
    --->    do_not_warn_obsolete
    ;       warn_obsolete.

:- type maybe_mark_cm_changes
    --->    do_not_mark_code_model_changes
    ;       mark_code_model_changes.

:- type maybe_after_front_end
    --->    not_after_front_end
    ;       after_front_end.

:- type maybe_elim_removable_scopes
    --->    do_not_elim_removable_scopes
    ;       elim_removable_scopes.

:- type maybe_opt_extra_structs
    --->    do_not_opt_extra_structs
    ;       opt_extra_structs.

:- type maybe_try_opt_const_structs
    --->    do_not_try_opt_const_structs
    ;       try_opt_const_structs.

:- type maybe_opt_const_structs
    --->    do_not_opt_const_structs
    ;       opt_const_structs.

:- type maybe_ignore_par_conjs
    --->    do_not_ignore_par_conjs
    ;       ignore_par_conjs.

:- type maybe_warn_suspicious_rec
    --->    do_not_warn_suspicious_rec
    ;       warn_suspicious_rec.

:- type maybe_warn_no_soln_disjunct
    --->    do_not_warn_no_soln_disjunct
    ;       warn_no_soln_disjunct.

:- type maybe_opt_split_switch_arms
    --->    do_not_opt_split_switch_arms
    ;       split_opt_switch_arms.

    % Each value of this type represents the full set of tasks
    % that simplification should perform. The submodules of simplify.m
    % use it to find out whether they should perform a specific task
    % without having to search a list of simplifications.
    %
    % The definition of this type does not need to be visible to modules
    % outside simplify.m, but avoiding such visibility would cost more
    % in extra complexity than it would gain.
:- type simplify_tasks
    --->    simplify_tasks(
                do_warn_simple_code             :: maybe_warn_simple_code,
                do_warn_duplicate_calls         :: maybe_warn_duplicate_calls,
                do_warn_implicit_streams        :: maybe_warn_implicit_streams,
                do_invoke_format_call           :: maybe_invoke_format_call,
                do_warn_obsolete                :: maybe_warn_obsolete,
                do_mark_code_model_changes      :: maybe_mark_cm_changes,
                do_after_front_end              :: maybe_after_front_end,
                do_excess_assign                :: maybe_elim_excess_assigns,
                do_test_after_switch            :: maybe_opt_test_after_switch,
                do_elim_removable_scopes        :: maybe_elim_removable_scopes,
                do_opt_duplicate_calls          :: maybe_opt_dup_calls,
                do_constant_prop                :: maybe_prop_constants,
                do_opt_common_structs           :: maybe_opt_common_structs,
                do_opt_extra_structs            :: maybe_opt_extra_structs,
                do_try_opt_const_structs        :: maybe_try_opt_const_structs,
                do_opt_const_structs            :: maybe_opt_const_structs,
                do_ignore_par_conjunctions      :: maybe_ignore_par_conjs,
                do_warn_suspicious_recursion    :: maybe_warn_suspicious_rec,
                do_warn_no_solution_disjunct    :: maybe_warn_no_soln_disjunct,
                do_switch_split_arms            :: maybe_split_switch_arms
            ).

    % XXX Now that each field of the simplify_tasks structure
    % has its own type and thus fields cannot be mistaken for each other,
    % we should consider requiring the compiler modules that invoke
    % simplification to construct a simplify_tasks structure directly,
    % *without* going through simplify task list.
    %
:- func simplify_tasks_to_list(simplify_tasks) = list(simplify_task).
:- func list_to_simplify_tasks(globals, list(simplify_task)) = simplify_tasks.

:- type maybe_generate_warnings
    --->    do_not_generate_warnings
    ;       generate_warnings.

    % Find out which simplifications should be run from the options table
    % stored in the globals. The second argument states whether we should
    % generate warnings during this pass of simplification.
    %
:- pred find_simplify_tasks(globals::in, maybe_generate_warnings::in,
    simplify_tasks::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.

:- import_module bool.

%---------------------------------------------------------------------------%

simplify_tasks_to_list(SimplifyTasks) = !:List :-
    SimplifyTasks = simplify_tasks(WarnSimpleCode, WarnDupCalls,
        WarnImplicitStreamCalls, DoFormatCalls, WarnObsolete,
        MarkCodeModelChanges, AfterFrontEnd, ExcessAssign, TestAfterSwitch,
        ElimRemovableScopes, OptDuplicateCalls, ConstantProp,
        OptCommonStructs, OptExtraStructs,
        TryOptConstStructs, _OptConstStructs, IgnoreParConjs,
        WarnSuspiciousRecursion, WarnNoSolutionDisjunct, SplitSwitchArms),
    !:List = [],
    ( if WarnSimpleCode = warn_simple_code
        then list.cons(simptask_warn_simple_code, !List) else true ),
    ( if WarnDupCalls = warn_duplicate_calls
        then list.cons(simptask_warn_duplicate_calls, !List) else true ),
    ( if WarnImplicitStreamCalls = warn_implicit_streams
        then list.cons(simptask_warn_implicit_stream_calls, !List) else true ),
    ( if DoFormatCalls = invoke_format_call
        then list.cons(simptask_format_calls, !List) else true ),
    ( if WarnObsolete = warn_obsolete
        then list.cons(simptask_warn_obsolete, !List) else true ),
    ( if MarkCodeModelChanges = mark_code_model_changes
        then list.cons(simptask_mark_code_model_changes, !List) else true ),
    ( if AfterFrontEnd = after_front_end
        then list.cons(simptask_after_front_end, !List) else true ),
    ( if ExcessAssign = elim_excess_assigns
        then list.cons(simptask_excess_assigns, !List) else true ),
    ( if TestAfterSwitch = opt_test_after_switch
        then list.cons(simptask_test_after_switch, !List) else true ),
    ( if ElimRemovableScopes = elim_removable_scopes
        then list.cons(simptask_elim_removable_scopes, !List) else true ),
    ( if OptDuplicateCalls = opt_dup_calls
        then list.cons(simptask_opt_duplicate_calls, !List) else true ),
    ( if ConstantProp = prop_constants
        then list.cons(simptask_constant_prop, !List) else true ),
    ( if OptCommonStructs = opt_common_structs
        then list.cons(simptask_common_structs, !List) else true ),
    ( if OptExtraStructs = opt_extra_structs
        then list.cons(simptask_extra_common_structs, !List) else true ),
    ( if TryOptConstStructs = try_opt_const_structs
        then list.cons(simptask_try_opt_const_structs, !List) else true ),
    ( if IgnoreParConjs = ignore_par_conjs
        then list.cons(simptask_ignore_par_conjs, !List) else true ),
    ( if WarnSuspiciousRecursion = warn_suspicious_rec
        then list.cons(simptask_warn_suspicious_recursion, !List) else true ),
    ( if WarnNoSolutionDisjunct = warn_no_soln_disjunct
        then list.cons(simptask_warn_no_solution_disjunct, !List) else true ),
    ( if SplitSwitchArms = split_switch_arms
        then list.cons(simptask_split_switch_arms, !List) else true ).

list_to_simplify_tasks(Globals, List) = Tasks :-
    globals.get_opt_tuple(Globals, OptTuple),
    Tasks = simplify_tasks(
        ( if list.member(simptask_warn_simple_code, List)
            then warn_simple_code else do_not_warn_simple_code ),
        ( if list.member(simptask_warn_duplicate_calls, List)
            then warn_duplicate_calls else do_not_warn_duplicate_calls ),
        ( if list.member(simptask_warn_implicit_stream_calls, List)
            then warn_implicit_streams else do_not_warn_implicit_streams ),
        ( if list.member(simptask_format_calls, List)
            then invoke_format_call else do_not_invoke_format_call ),
        ( if list.member(simptask_warn_obsolete, List)
            then warn_obsolete else do_not_warn_obsolete ),
        ( if list.member(simptask_mark_code_model_changes, List)
            then mark_code_model_changes else do_not_mark_code_model_changes ),
        ( if list.member(simptask_after_front_end, List)
            then after_front_end else not_after_front_end ),
        ( if list.member(simptask_excess_assigns, List)
            then elim_excess_assigns else do_not_elim_excess_assigns ),
        ( if list.member(simptask_test_after_switch, List)
            then opt_test_after_switch else do_not_opt_test_after_switch ),
        ( if list.member(simptask_elim_removable_scopes, List)
            then elim_removable_scopes else do_not_elim_removable_scopes ),
        ( if list.member(simptask_opt_duplicate_calls, List)
            then opt_dup_calls else do_not_opt_dup_calls ),
        ( if list.member(simptask_constant_prop, List)
            then prop_constants else do_not_prop_constants ),
        ( if list.member(simptask_common_structs, List)
            then opt_common_structs else do_not_opt_common_structs ),
        ( if list.member(simptask_extra_common_structs, List)
            then opt_extra_structs else do_not_opt_extra_structs ),
        ( if list.member(simptask_try_opt_const_structs, List)
            then try_opt_const_structs else do_not_try_opt_const_structs ),
        ( if
            list.member(simptask_try_opt_const_structs, List),
            OptTuple ^ ot_enable_const_struct_user = enable_const_struct_user
        then
            opt_const_structs
        else
            do_not_opt_const_structs
        ),
        ( if list.member(simptask_ignore_par_conjs, List)
            then ignore_par_conjs else do_not_ignore_par_conjs ),
        ( if list.member(simptask_warn_suspicious_recursion, List)
            then warn_suspicious_rec else do_not_warn_suspicious_rec ),
        ( if list.member(simptask_warn_no_solution_disjunct, List)
            then warn_no_soln_disjunct else do_not_warn_no_soln_disjunct ),
        ( if list.member(simptask_split_switch_arms, List)
            then split_switch_arms else do_not_split_switch_arms )
    ).

find_simplify_tasks(Globals, WarnThisPass, SimplifyTasks) :-
    globals.lookup_bool_option(Globals, warn_simple_code, WarnSimple),
    globals.lookup_bool_option(Globals, warn_duplicate_calls, WarnDupCalls),
    globals.lookup_bool_option(Globals, warn_implicit_stream_calls,
        WarnImplicitStreamCalls),
    globals.lookup_bool_option(Globals, warn_known_bad_format_calls,
        WarnKnownBadFormat),
    globals.lookup_bool_option(Globals, warn_unknown_format_calls,
        WarnUnknownFormat),
    globals.get_opt_tuple(Globals, OptTuple),
    OptFormatCalls = OptTuple ^ ot_opt_format_calls,
    ( if
        (
            WarnThisPass = generate_warnings,
            ( WarnKnownBadFormat = yes ; WarnUnknownFormat = yes  )
        ;
            OptFormatCalls = opt_format_calls
        )
    then
        InvokeFormatCall = invoke_format_call
    else
        InvokeFormatCall = do_not_invoke_format_call
    ),
    globals.lookup_bool_option(Globals, warn_obsolete, WarnObsolete),
    ExcessAssign = OptTuple ^ ot_elim_excess_assigns,
    TestAfterSwitch = OptTuple ^ ot_opt_test_after_switch,
    OptDuplicateCalls = OptTuple ^ ot_opt_dup_calls,
    ConstantProp = OptTuple ^ ot_prop_constants,
    MarkCodeModelChanges = do_not_mark_code_model_changes,
    AfterFrontEnd = not_after_front_end,
    ElimRemovableScopes = do_not_elim_removable_scopes,
    CommonStructs = OptTuple ^ ot_opt_common_structs,
    ExtraCommonStructs = do_not_opt_extra_structs,
    TryOptConstStructs = do_not_try_opt_const_structs,
    OptConstStructs = do_not_opt_const_structs,
    globals.lookup_bool_option(Globals, ignore_par_conjunctions,
        RemoveParConjunctions),
    globals.lookup_bool_option(Globals, warn_suspicious_recursion,
        WarnSuspiciousRecursion),
    SplitSwitchArms = OptTuple ^ ot_split_switch_arms,

    SimplifyTasks = simplify_tasks(
        ( if WarnSimple = yes, WarnThisPass = generate_warnings
            then warn_simple_code else do_not_warn_simple_code ),
        ( if WarnDupCalls = yes, WarnThisPass = generate_warnings
            then warn_duplicate_calls else do_not_warn_duplicate_calls ),
        ( if WarnImplicitStreamCalls = yes, WarnThisPass = generate_warnings
            then warn_implicit_streams else do_not_warn_implicit_streams ),
        InvokeFormatCall,
        ( if WarnObsolete = yes, WarnThisPass = generate_warnings
            then warn_obsolete else do_not_warn_obsolete ),
        MarkCodeModelChanges,
        AfterFrontEnd,
        ExcessAssign,
        TestAfterSwitch,
        ElimRemovableScopes,
        OptDuplicateCalls,
        ConstantProp,
        CommonStructs,
        ExtraCommonStructs,
        TryOptConstStructs,
        OptConstStructs,
        ( if RemoveParConjunctions = yes
            then ignore_par_conjs else do_not_ignore_par_conjs ),
        ( if WarnSuspiciousRecursion = yes
            then warn_suspicious_rec else do_not_warn_suspicious_rec ),
        % Warnings about "no solution disjuncts" are a category of warnings
        % about simple code that happens to have its own disabling mechanism.
        ( if WarnSimple = yes, WarnThisPass = generate_warnings
            then warn_no_soln_disjunct else do_not_warn_no_soln_disjunct ),
        SplitSwitchArms
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_tasks.
%---------------------------------------------------------------------------%
