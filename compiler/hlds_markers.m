%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_markers.m.
%
% This module defines the pred_marker and goal_feature types,
% and their operations.
% (Actually, all the operations are on pred_markers.)
%
% The presence or absence of each pred_marker in a pred_info, or
% of a goal_feature in a goal_info says something about that predicate,
% function, or goal.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_markers.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- interface.

    % A set of pred_markers.
:- type pred_markers == set(pred_marker).

:- type pred_marker
    --->    marker_stub
            % The predicate has no clauses. typecheck.m will generate a body
            % for the predicate which just throws an exception. This marker
            % is used to tell purity analysis and determinism analysis
            % not to issue warnings for these predicates.

    ;       marker_builtin_stub
            % This predicate is a builtin but has no clauses for whatever
            % reason. typecheck.m should generate a stub clause for it but no
            % warn about it.

    ;       marker_infer_type
            % Requests type inference for the predicate. These markers are
            % inserted by make_hlds for undeclared predicates.

    ;       marker_infer_modes
            % Requests mode inference for the predicate. These markers are
            % inserted by make_hlds for undeclared predicates.

    ;       marker_no_pred_decl
            % This predicate had no (valid) `:- pred' or `:- func' declaration.
            % Since we have generated an error message about this, suppress
            % the generation of any similar messages about missing mode
            % declarations, since the missing (or invalid) declaration
            % could have been a combined predmode declaration.

    ;       marker_no_detism_warning
            % Requests no warnings about the determinism of this predicate
            % being too loose.
            % Used for pragma(no_determinism_warning).

    ;       marker_user_marked_inline
            % The user requests that this be predicate should be inlined,
            % even if it exceeds the usual size limits. Used for
            % pragma(inline). Mutually exclusive with
            % marker_user_marked_no_inline.

    ;       marker_heuristic_inline
            % The compiler (meaning probably inlining.m) requests that this
            % predicate be inlined. Does not override
            % marker_user_marked_no_inline.

    ;       marker_user_marked_no_inline
            % The user requests that this be predicate should not be inlined.
            % Used for pragma(no_inline). Mutually exclusive with
            % marker_user_marked_inline.

    ;       marker_mmc_marked_no_inline
            % The compiler requests that this be predicate should not be
            % inlined. Used for pragma(mode_check_clauses). Mutually exclusive
            % with marker_user_marked_inline.

    ;       marker_consider_used
            % The user has requested that this predicate be considered used
            % when we consider which procedures are dead, so we can generate
            % dead procedure warnings for them. If this marker is present
            % on a predicate, then neither the procedures of this predicate
            % nor the other procedures they call, directly or indirectly,
            % should get dead procedure warnings.

    ;       marker_class_method
            % Requests that this predicate be transformed into the appropriate
            % call to a class method.

    ;       marker_class_instance_method
            % This predicate was automatically generated for the implementation
            % of a class method for an instance.

    ;       marker_named_class_instance_method
            % This predicate was automatically generated for the implementation
            % of a class method for an instance, and the instance was defined
            % using the named syntax (e.g. "pred(...) is ...") rather than
            % the clause syntax. (For such predicates, we output slightly
            % different error messages.)

    ;       marker_is_impure
            % Requests that no transformation that would be inappropriate for
            % impure code be performed on calls to this predicate. This
            % includes reordering calls to it relative to other goals
            % (in both conjunctions and disjunctions), and removing
            % redundant calls to it.

    ;       marker_is_semipure
            % Requests that no transformation that would be inappropriate
            % for semipure code be performed on calls to this predicate.
            % This includes removing redundant calls to it on different sides
            % of an impure goal.

    ;       marker_promised_pure
            % Requests that calls to this predicate be transformed as usual,
            % despite any impure or semipure markers present.

    ;       marker_promised_semipure
            % Requests that calls to this predicate be treated as semipure,
            % despite any impure calls in the body.

    ;       marker_promised_equivalent_clauses
            % Promises that all modes of the predicate have equivalent
            % semantics, event if they are defined by different sets of
            % mode-specific clauses.

    % The terminates and does_not_terminate pragmas are kept as markers
    % to ensure that conflicting declarations are not made by the user.
    % Otherwise, the information could be added to the ProcInfos directly.

    ;       marker_terminates
            % The user guarantees that this predicate will terminate
            % for all (finite?) input.

    ;       marker_does_not_terminate
            % States that this predicate does not terminate. This is useful
            % for pragma foreign_code, which the compiler assumes to be
            % terminating.

    ;       marker_check_termination
            % The user requires the compiler to guarantee the termination
            % of this predicate. If the compiler cannot guarantee termination
            % then it must give an error message.

    ;       marker_calls_are_fully_qualified
            % All calls in this predicate are fully qualified. This occurs for
            % predicates read from `.opt' files and compiler-generated
            % predicates.

    ;       marker_mode_check_clauses
            % Each clause of the predicate should be modechecked separately.
            % Used for predicates defined by lots of clauses (usually facts)
            % for which the compiler's quadratic behavior during mode checking
            % (in inst_match.bound_inst_list_contains_instname and
            % instmap.merge) would be unacceptable.

    ;       marker_mutable_access_pred
            % This predicate is part of the machinery used to access mutables.
            % This marker is used to inform inlining that we should _always_
            % attempt to inline this predicate across module boundaries.

    ;       marker_has_require_scope
            % The body of this predicate contains a require_complete_switch
            % or require_detism scope. This marker is set if applicable during
            % determinism inference. It is used during determinism reporting:
            % procedures in predicates that have this marker are checked
            % for violations of the requirements of these scopes even if
            % the overall determinism of the procedure body is correct.

    ;       marker_has_incomplete_switch
            % The body of this predicate contains an incomplete switch
            % (one for which the switched-on variable may have a value
            % that does not match any of the cases). This marker is set
            % if applicable during determinism inference. It is used during
            % determinism reporting: if the inform_incomplete_switch option
            % is set, then procedures in predicates that have this marker
            % are traversed again to generate informational messages about
            % these incomplete switches, even if the overall determinism
            % of the procedure body is correct.

    ;       marker_has_format_call
            % The body of this predicate contains calls to predicates
            % recognized by format_call.is_format_call. This marker is set
            % (if applicable) during determinism analysis, when the predicate
            % body has to be traversed anyway. It is used by the simplification
            % pass at the end of semantic analysis, both to warn about
            % incorrect (or at least not verifiably correct) format calls,
            % and to optimize correct format calls. Neither the warnings
            % nor the optimizations can be applicable to predicates that
            % do not contain format calls, as shown by not having this marker.

    ;       marker_has_rhs_lambda
            % The body of this predicate contains a unification whose
            % right hand side is a lambda goal. This marker is set by
            % the typecheck pass, and it is used (as of this writing)
            % only by the post-typecheck pass.

    ;       marker_fact_table_semantic_errors.
            % This predicate has a fact_table pragma for it, so it is
            % *expected* not to have any clauses in the program itself,
            % but the compiler found some problems with its declaration,
            % and so the compiler did not generate clauses (actually,
            % foreign_procs) for it either. Therefore its procedures
            % have no implementations, but there should be no separate
            % error message about this: since they would probably generate
            % more confusion than enlightenment. The error messages generated
            % by fact_table.m should be entirely sufficient.

%---------------------------------------------------------------------------%

:- pred marker_name(pred_marker::in, string::out) is det.

%---------------------------------------------------------------------------%

    % Create an empty set of markers.
    %
:- pred init_markers(pred_markers::out) is det.

    % Check if a particular is in the set.
    %
:- pred marker_is_present(pred_markers::in, pred_marker::in) is semidet.

    % Add some markers to the set.
    %
:- pred add_marker(pred_marker::in,
    pred_markers::in, pred_markers::out) is det.
:- pred add_markers(list(pred_marker)::in,
    pred_markers::in, pred_markers::out) is det.

    % Remove a marker from the set.
    %
:- pred remove_marker(pred_marker::in,
    pred_markers::in, pred_markers::out) is det.

%---------------------------------------------------------------------------%

    % Convert the set to and from a list.
    %
:- pred markers_to_marker_list(pred_markers::in, list(pred_marker)::out)
    is det.
:- pred marker_list_to_markers(list(pred_marker)::in, pred_markers::out)
    is det.

%---------------------------------------------------------------------------%

:- pred purity_to_markers(purity::in, list(pred_marker)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type goal_feature
    --->    feature_constraint
            % This is included if the goal is a constraint. See constraint.m
            % for the definition of this.

    ;       feature_from_head
            % This goal was originally in the head of the clause, and was
            % put into the body by the superhomogeneous form transformation.

    ;       feature_not_impure_for_determinism
            % This goal should not be treated as impure for the purpose of
            % computing its determinism. This is intended to be used by program
            % transformations that insert impure code into existing goals,
            % and wish to keep the old determinism of those goals.

    ;       feature_stack_opt
            % This goal was created by stack slot optimization. Other
            % optimizations should assume that it is there for a reason, and
            % therefore should refrain from "optimizing" it away, even though
            % it is a copy of another, previous goal.

    ;       feature_tuple_opt
            % This goal was created by the tupling optimization.
            % The comment for the stack slot optimization above applies here.

    ;       feature_call_table_gen
            % This goal generates the variable that represents the call table
            % tip. If debugging is enabled, the code generator needs to save
            % the value of this variable in its stack slot as soon as it is
            % generated; this marker tells the code generator when this
            % happens.

    ;       feature_preserve_backtrack_into
            % Determinism analysis should preserve backtracking into goals
            % marked with this feature, even if their determinism puts an
            % at_most_zero upper bound on the number of solutions they have.

    ;       feature_save_deep_excp_vars
            % This goal generates the deep profiling variables that the
            % exception handler needs to execute the exception port code.

    ;       feature_hide_debug_event
            % The events associated with this goal should be hidden. This is
            % used e.g. by the tabling transformation to preserve the set
            % of events generated by a tabled procedure.

    ;       feature_deep_self_tail_rec_call
            % This goal represents a self-tail-recursive call. This marker
            % is used by deep profiling.

    ;       feature_debug_self_tail_rec_call
            % This goal represents a self-tail-recursive call. This marker
            % is used by the LLDS code generator for generating TAIL events
            % for the debugger.

    ;       feature_self_or_mutual_tail_rec_call
            % This goal represents a tail-recursive call, which may be
            % either self-recursive or mutually-recursive (you have to compare
            % the identities of the caller and the callee to figure out which).
            % This marker is used by inlining, and (in the future) by the MLDS
            % code generator.

    ;       feature_obvious_nontail_rec_call
            % This goal represents a recursive call that is not a tail call,
            % but we don't necessarily want to generate a warning for it,
            % since it is followed by a later recursive call (which may or
            % may not be a tail call).

    ;       feature_keep_constant_binding
            % This feature should only be attached to unsafe_cast goals
            % that cast a value of an user-defined type to an integer.
            % It tells the mode checker that if the first variable is known
            % to be bound to a given constant, then the second variable
            % should be set to the corresponding local tag value.

    ;       feature_do_not_warn_singleton
            % Don't warn about singletons in this goal. Intended to be used
            % by the state variable transformation, for situations such as the
            % following:
            %
            % p(X, !.S, ...) :-
            %   (
            %       X = a,
            %       !:S = f(!.S, ...)
            %   ;
            %       X = b,
            %       <code A>
            %   ),
            %   <code B>.
            %
            % The state variable transformation creates a new variable for
            % the new value of !:S in the disjunction. If code A doesn't define
            % !:S, the state variable transformation inserts an unification
            % after it, unifying the variables representing !.S and !:S.
            % If code B doesn't refer to S, then quantification will restrict
            % the scope of the variable representing !:S to each disjunct,
            % and the unification inserted after code A will refer to a
            % singleton variable.
            %
            % Since it is not reasonable to expect the state variable
            % transformation to do the job of quantification as well,
            % we simply make it mark the unifications it creates, and get
            % the singleton warning code to respect it.
            %
            % On the other hand, see the next feature.

    ;       feature_state_var_copy
            % This goal is one of the unifications mentioned in the comment
            % immediately above. A post-pass in the state variable
            % transformation deletes unification goals with this feature
            % if the variable on the LHS (which should be the variable
            % representing the updated version of the state variable)
            % if not used in later code.
            %
            % This allows us to report at least some places where the
            % new version of a state variable is a singleton variable
            % (which in practice virtually always means that it is computed,
            % but never used).

    ;       feature_duplicated_for_switch
            % This goal was created by switch detection by duplicating
            % the source code written by the user.

    ;       feature_mode_check_clauses_goal
            % This goal is the main disjunction of a predicate with the
            % mode_check_clauses pragma. No compiler pass should try to invoke
            % quadratic or worse algorithms on the arms of this goal, since it
            % probably has many arms (possibly several thousand). This feature
            % may be attached to switches as well as disjunctions.

    ;       feature_will_not_modify_trail
            % This goal will not modify the trail, so it is safe for the
            % compiler to omit trailing primitives when generating code
            % for this goal.

    ;       feature_will_not_call_mm_tabled
            % This goal will never call a procedure that is evaluated using
            % minimal model tabling. It is safe for the code generator to omit
            % the pneg context wrappers when generating code for this goal.

    ;       feature_contains_trace
            % This goal contains a scope goal whose scope_reason is
            % trace_goal(...).

    ;       feature_pretest_equality
            % This goal is an if-then-else in a compiler-generated
            % type-constructor-specific unify or compare predicate
            % whose condition is a test of whether the two input arguments
            % are equal or not. The goal feature exists because in some
            % circumstances we need to strip off this pretest, and replace
            % the if-then-else with just its else branch.

    ;       feature_pretest_equality_condition
            % This goal is the unification in the condition of a
            % pretest-equality if-then-else goal. The goal feature is required
            % to allow pointer comparisons generated by the compiler.

    ;       feature_lambda_undetermined_mode
            % This goal is a lambda goal converted from a higher order term
            % for which we don't know the mode of the call to the underlying
            % predicate. These can be produced by the polymorphism
            % transformation but should be removed by the end of mode
            % checking.

    ;       feature_contains_stm_inner_outer
            % This goal is a goal inside an atomic scope, for which the calls
            % to convert inner and outer variables have been inserted.

    ;       feature_do_not_tailcall
            % This goal is a call that should not be executed as a tail call.
            % Currently this is only used by the loop control optimization
            % since a spawned off task may need to use the parent's stack frame
            % even after the parent makes a tail call.

    ;       feature_do_not_warn_implicit_stream
            % Even if this call is to a predicate that operates on an implicit
            % stream, do not generate a warning about that. This feature
            % should be set on calls that are constructed by the compiler
            % - to calls which may possibly be subject to that warning, and
            % - for which the code from which this call has been constructed
            %   has already had generated for it all the warnings of this type
            %   that it deserved.

    ;       feature_lifted_by_cse
            % This goal is a deconstruction unification that has been lifted
            % out of each arm of a switch by cse_detection.m. Used to avoid
            % spurious warnings about the goal inside a scope such as
            % `require_complete_switch [X] (...)' not being a switch
            % in situations where the `...' starts out as a switch on X,
            % but where cse_detection.m turns it into a conjunction,
            % inserting one or more of these lifted-out deconstructions
            % before the original switch.

    ;       feature_lambda_from_try.
            % This lambda goal wraps the main part of a try goal,
            % the part that does the main job but may throw an exception.
            % This feaure is used to inform the code that warns about
            % infinite recursion that the lambda goal *will* be executed
            % in context in which it is constructed.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% For markers that we add to a predicate because of a pragma on that predicate,
% the marker name MUST correspond to the name of the pragma.
marker_name(marker_stub, "stub").
marker_name(marker_builtin_stub, "builtin_stub").
marker_name(marker_infer_type, "infer_type").
marker_name(marker_infer_modes, "infer_modes").
marker_name(marker_user_marked_inline, "inline").
marker_name(marker_heuristic_inline, "heuristic_inline").
marker_name(marker_no_pred_decl, "no_pred_decl").
marker_name(marker_user_marked_no_inline, "no_inline").
marker_name(marker_mmc_marked_no_inline, "mmc_no_inline").
marker_name(marker_consider_used, "consider_used").
marker_name(marker_no_detism_warning, "no_determinism_warning").
marker_name(marker_class_method, "class_method").
marker_name(marker_class_instance_method, "class_instance_method").
marker_name(marker_named_class_instance_method, "named_class_instance_method").
marker_name(marker_is_impure, "impure").
marker_name(marker_is_semipure, "semipure").
marker_name(marker_promised_pure, "promise_pure").
marker_name(marker_promised_semipure, "promise_semipure").
marker_name(marker_promised_equivalent_clauses, "promise_equivalent_clauses").
marker_name(marker_terminates, "terminates").
marker_name(marker_check_termination, "check_termination").
marker_name(marker_does_not_terminate, "does_not_terminate").
marker_name(marker_calls_are_fully_qualified, "calls_are_fully_qualified").
marker_name(marker_mode_check_clauses, "mode_check_clauses").
marker_name(marker_mutable_access_pred, "mutable_access_pred").
marker_name(marker_has_require_scope, "has_require_scope").
marker_name(marker_has_incomplete_switch, "has_incomplete_switch").
marker_name(marker_has_format_call, "has_format_call").
marker_name(marker_has_rhs_lambda, "has_rhs_lambda").
marker_name(marker_fact_table_semantic_errors, "fact_table_semantic_errors").

%---------------------------------------------------------------------------%

init_markers(set.init).

marker_is_present(MarkerSet, Marker) :-
    set.member(Marker, MarkerSet).

add_marker(Marker, !MarkerSet) :-
    set.insert(Marker, !MarkerSet).

add_markers(Markers, !MarkerSet) :-
    set.insert_list(Markers, !MarkerSet).

remove_marker(Marker, !MarkerSet) :-
    set.delete(Marker, !MarkerSet).

%---------------------------------------------------------------------------%

markers_to_marker_list(MarkerSet, Markers) :-
    set.to_sorted_list(MarkerSet, Markers).

marker_list_to_markers(Markers, MarkerSet) :-
    set.list_to_set(Markers, MarkerSet).

%---------------------------------------------------------------------------%

purity_to_markers(purity_pure, []).
purity_to_markers(purity_semipure, [marker_is_semipure]).
purity_to_markers(purity_impure, [marker_is_impure]).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_markers.
%---------------------------------------------------------------------------%
