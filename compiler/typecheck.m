%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typecheck.m.
% Main author: fjh.
%
% This file contains the Mercury type-checker.
%
% The predicates in this module are named as follows:
%
%   Predicates that type check a particular language
%   construct (goal, clause, etc.) are called typecheck_*.
%   These will eventually have to iterate over every type
%   assignment in the type assignment set.
%
%   Predicates that unify two things with respect to a
%   single type assignment, as opposed to a type assignment set
%   are called type_assign_*.
%
%   Access predicates for the typecheck_info data structure are called
%   typecheck_info_*.
%
% There are four sorts of types:
%
% 1) discriminated unions:
%   :- type tree(T) ---> nil ; t(tree(T), T, tree(T)).
%
% 2) equivalent types (treated identically, ie, same name.  Any number
%   of types can be equivalent; the *canonical* one is the one
%   which is not defined using ==):
%   :- type real == float.
%
%   Currently references to equivalence types are expanded
%   in a separate pass by mercury_compile.m.  It would be better
%   to avoid expanding them (and instead modify the type unification
%   algorithm to handle equivalent types) because this would
%   give better error messages.  However, this is not a high
%   priority.
%
% 3) higher-order predicate and function types
%   pred, pred(T), pred(T1, T2), pred(T1, T2, T3), ...
%   func(T1) = T2, func(T1, T2) = T3, ...
%
% 4) builtin types
%   character, int, float, string
%       These types have special syntax for constants.
%   There may be other types (list(T), unit, univ,
%   etc.) provided by the system, but they can just
%   be part of the standard library.
%
% Each exported predicate must have a `:- pred' declaration specifying the
% types of the arguments for that predicate.  For predicates that are
% local to a module, we infer the types.
%
%-----------------------------------------------------------------------------%
%
% Known Bugs:
%
% XXX   Type inference doesn't handle ambiguity as well as it could do.
%   We should do a topological sort, and then typecheck it all
%   bottom-up.  If we infer an ambiguous type for a pred, we should
%   not reject it immediately; instead we should give it an overloaded
%   type, and keep going.  When we've finished type inference, we should
%   then delete unused overloadings, and only then should we report
%   ambiguity errors, if any overloading still remains.
%
% Wish list:
%
%   we should handle equivalence types here
%
%-----------------------------------------------------------------------------%

:- module check_hlds.typecheck.

:- interface.

:- import_module hlds.hlds_module.

:- import_module bool.
:- import_module io.

    % typecheck(Module0, Module, FoundError, ExceededIterationLimit, !IO)
    %
    % Type-checks Module0 and annotates it with variable typings
    % (returning the result in Module), printing out appropriate
    % error messages.
    % FoundError is set to `yes' if there are any errors and
    % `no' otherwise.
    % ExceededIterationLimit is set to `yes' if the type inference
    % iteration limit was reached and `no' otherwise.
    %
:- pred typecheck(module_info::in, module_info::out, bool::out, bool::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.goal_path.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.type_util.
:- import_module check_hlds.typecheck_errors.
:- import_module check_hlds.typecheck_info.
:- import_module check_hlds.typeclasses.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module getopt_io.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

typecheck(!Module, FoundError, ExceededIterationLimit, !IO) :-
    globals.io_lookup_bool_option(statistics, Statistics, !IO),
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Type-checking clauses...\n", !IO),
    typecheck_module(!Module, FoundError, ExceededIterationLimit, !IO),
    maybe_report_stats(Statistics, !IO).

%-----------------------------------------------------------------------------%

    % Type-check the code for all the predicates in a module.
    %
:- pred typecheck_module(module_info::in, module_info::out,
    bool::out, bool::out, io::di, io::uo) is det.

typecheck_module(!Module, FoundError, ExceededIterationLimit, !IO) :-
    module_info_predids(!.Module, PredIds),
    globals.io_lookup_int_option(type_inference_iteration_limit,
        MaxIterations, !IO),
    typecheck_to_fixpoint(1, MaxIterations, PredIds, !Module,
        FoundError, ExceededIterationLimit, !IO),
    write_type_inference_messages(PredIds, !.Module, !IO).

    % Repeatedly typecheck the code for a group of predicates
    % until a fixpoint is reached, or until some errors are detected.
    %
:- pred typecheck_to_fixpoint(int::in, int::in, list(pred_id)::in,
    module_info::in, module_info::out, bool::out, bool::out,
    io::di, io::uo) is det.

typecheck_to_fixpoint(Iteration, NumIterations, PredIds, !Module,
        FoundError, ExceededIterationLimit, !IO) :-
    typecheck_module_one_iteration(Iteration, PredIds, !Module,
        no, FoundError1, no, Changed, !IO),
    (
        ( Changed = no
        ; FoundError1 = yes
        )
    ->
        FoundError = FoundError1,
        ExceededIterationLimit = no
    ;
        globals.io_lookup_bool_option(debug_types, DebugTypes, !IO),
        (
            DebugTypes = yes,
            write_type_inference_messages(PredIds, !.Module, !IO)
        ;
            DebugTypes = no
        ),
        ( Iteration < NumIterations ->
            typecheck_to_fixpoint(Iteration + 1, NumIterations, PredIds,
                !Module, FoundError, ExceededIterationLimit, !IO)
        ;
            typecheck_report_max_iterations_exceeded(!IO),
            FoundError = yes,
            ExceededIterationLimit = yes
        )
    ).

    % Write out the inferred `pred' or `func' declarations for a list of
    % predicates.  Don't write out the inferred types for assertions.
    %
:- pred write_type_inference_messages(list(pred_id)::in, module_info::in,
    io::di, io::uo) is det.

write_type_inference_messages([], _, !IO).
write_type_inference_messages([PredId | PredIds], ModuleInfo, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    (
        check_marker(Markers, marker_infer_type),
        module_info_predids(ModuleInfo, ValidPredIds),
        list.member(PredId, ValidPredIds),
        \+ pred_info_get_goal_type(PredInfo, goal_type_promise(_))
    ->
        write_type_inference_message(PredInfo, !IO)
    ;
        true
    ),
    write_type_inference_messages(PredIds, ModuleInfo, !IO).

    % Write out the inferred `pred' or `func' declaration
    % for a single predicate.
    %
:- pred write_type_inference_message(pred_info::in, io::di, io::uo) is det.

write_type_inference_message(PredInfo, !IO) :-
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name = unqualified(PredName),
    pred_info_context(PredInfo, Context),
    pred_info_get_arg_types(PredInfo, VarSet, ExistQVars, Types0),
    strip_builtin_qualifiers_from_type_list(Types0, Types),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_purity(PredInfo, Purity),
    MaybeDet = no,
    prog_out.write_context(Context, !IO),
    io.write_string("Inferred ", !IO),
    AppendVarNums = no,
    (
        PredOrFunc = predicate,
        mercury_output_pred_type(VarSet, ExistQVars, Name, Types,
            MaybeDet, Purity, ClassContext, Context, AppendVarNums, !IO)
    ;
        PredOrFunc = function,
        pred_args_to_func_args(Types, ArgTypes, RetType),
        mercury_output_func_type(VarSet, ExistQVars, Name, ArgTypes, RetType,
            MaybeDet, Purity, ClassContext, Context, AppendVarNums, !IO)
    ).

:- pred typecheck_report_max_iterations_exceeded(io::di, io::uo) is det.

typecheck_report_max_iterations_exceeded(!IO) :-
    globals.io_lookup_int_option(type_inference_iteration_limit,
        MaxIterations, !IO),
    Pieces = [words("Type inference iteration limit exceeded."),
        words("This probably indicates that your program has a type error."),
        words("You should declare the types explicitly."),
        words("(The current limit is"), int_fixed(MaxIterations),
        words("iterations."),
        words("You can use the `--type-inference-iteration-limit' option"),
        words("to increase the limit).")],
    Msg = error_msg(no, no, 0, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]),
    % XXX _NumErrors
    write_error_spec(Spec, 0, _NumWarnings, 0, _NumErrors, !IO).

%-----------------------------------------------------------------------------%

    % Iterate over the list of pred_ids in a module.
    %
:- pred typecheck_module_one_iteration(int::in, list(pred_id)::in,
    module_info::in, module_info::out, bool::in, bool::out,
    bool::in, bool::out, io::di, io::uo) is det.

typecheck_module_one_iteration(_, [], !ModuleInfo, !Error, !Changed, !IO).
typecheck_module_one_iteration(Iteration, [PredId | PredIds], !ModuleInfo,
        !Error, !Changed, !IO) :-
    module_info_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    ( pred_info_is_imported(PredInfo0) ->
        true
    ;
        typecheck_pred_if_needed(Iteration, PredId, PredInfo0,
            PredInfo1, !ModuleInfo, NewError, NewChanged, !IO),
        (
            NewError = no,
            map.det_update(Preds0, PredId, PredInfo1, Preds),
            module_info_set_preds(Preds, !ModuleInfo)
        ;
            NewError = yes,
%       /********************
%       This code is not needed at the moment,
%       since currently we don't run mode analysis if
%       there are any type errors.
%       And this code also causes problems:
%       if there are undefined modes,
%       this code can end up calling error/1,
%       since post_finish_ill_typed_pred
%       assumes that there are no undefined modes.
%           %
%           % if we get an error, we need to call
%           % post_finish_ill_typed_pred on the
%           % pred, to ensure that its mode declaration gets
%           % properly module qualified; then we call
%           % `remove_predid', so that the predicate's definition
%           % will be ignored by later passes (the declaration
%           % will still be used to check any calls to it).
%           %
%           post_finish_ill_typed_pred(ModuleInfo0,
%               PredId, PredInfo1, PredInfo),
%           map.det_update(Preds0, PredId, PredInfo, Preds),
%       *******************/
            map.det_update(Preds0, PredId, PredInfo1, Preds),
            module_info_set_preds(Preds, !ModuleInfo),
            module_info_remove_predid(PredId, !ModuleInfo)
        ),
        bool.or(NewError, !Error),
        bool.or(NewChanged, !Changed)
    ),
    typecheck_module_one_iteration(Iteration, PredIds, !ModuleInfo,
        !Error, !Changed, !IO).

:- pred typecheck_pred_if_needed(int::in, pred_id::in,
    pred_info::in, pred_info::out, module_info::in, module_info::out,
    bool::out, bool::out, io::di, io::uo) is det.

typecheck_pred_if_needed(Iteration, PredId, !PredInfo, !ModuleInfo,
        Error, Changed, !IO) :-
    (
        % Compiler-generated predicates are created already type-correct,
        % so there's no need to typecheck them. The same is true for builtins.
        % But, compiler-generated unify predicates are not guaranteed to be
        % type-correct if they call a user-defined equality or comparison
        % predicate or if it is a special pred for an existentially typed
        % data type.
        (
            is_unify_or_compare_pred(!.PredInfo),
            \+ special_pred_needs_typecheck(!.PredInfo, !.ModuleInfo)
        ;
            pred_info_is_builtin(!.PredInfo)
        )
    ->
        pred_info_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0),
        IsEmpty = clause_list_is_empty(ClausesRep0),
        (
            IsEmpty = yes,
            pred_info_mark_as_external(!PredInfo)
        ;
            IsEmpty = no
        ),
        Error = no,
        Changed = no
    ;
        typecheck_pred(Iteration, PredId, !PredInfo, !ModuleInfo,
            Error, Changed, !IO)
    ).

:- pred typecheck_pred(int::in, pred_id::in,
    pred_info::in, pred_info::out, module_info::in, module_info::out,
    bool::out, bool::out, io::di, io::uo) is det.

typecheck_pred(Iteration, PredId, !PredInfo, !ModuleInfo, Error, Changed,
        !IO) :-
    globals.io_get_globals(Globals, !IO),
    ( Iteration = 1 ->
        % Goal paths are used to identify typeclass constraints.
        fill_goal_path_slots_in_clauses(!.ModuleInfo, no, !PredInfo),
        maybe_add_field_access_function_clause(!.ModuleInfo, !PredInfo),
        maybe_improve_headvar_names(Globals, !PredInfo),

        % The goal_type of the pred_info may have been changed
        % by maybe_add_field_access_function_clause.
        module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
    ;
        true
    ),
    pred_info_get_arg_types(!.PredInfo, _ArgTypeVarSet, ExistQVars0, ArgTypes0),
    some [!ClausesInfo, !Info, !HeadTypeParams] (
        pred_info_clauses_info(!.PredInfo, !:ClausesInfo),
        clauses_info_get_clauses_rep(!.ClausesInfo, ClausesRep0),
        clauses_info_get_headvars(!.ClausesInfo, HeadVars),
        clauses_info_get_varset(!.ClausesInfo, VarSet0),
        clauses_info_get_explicit_vartypes(!.ClausesInfo, ExplicitVarTypes0),
        pred_info_get_markers(!.PredInfo, Markers0),
        % Handle the --allow-stubs and --warn-stubs options.
        % If --allow-stubs is set, and there are no clauses,
        % issue a warning if --warn-stubs is set, and then
        % generate a "stub" clause that just throws an exception.
        (
            clause_list_is_empty(ClausesRep0) = yes,
            globals.lookup_bool_option(Globals, allow_stubs, yes),
            \+ check_marker(Markers0, marker_class_method)
        ->
            globals.lookup_bool_option(Globals, warn_stubs, WarnStubs),
            (
                WarnStubs = yes,
                report_no_clauses("Warning", PredId, !.PredInfo, !.ModuleInfo,
                    !IO)
            ;
                WarnStubs = no
            ),
            PredPieces = describe_one_pred_name(!.ModuleInfo,
                should_module_qualify, PredId),
            PredName = error_pieces_to_string(PredPieces),
            generate_stub_clause(PredName, !PredInfo, !.ModuleInfo, StubClause,
                VarSet0, VarSet),
            set_clause_list([StubClause], ClausesRep1),
            clauses_info_set_clauses([StubClause], !ClausesInfo),
            clauses_info_set_varset(VarSet, !ClausesInfo)
        ;
            VarSet = VarSet0,
            ClausesRep1 = ClausesRep0
        ),
        clause_list_is_empty(ClausesRep1) = ClausesRep1IsEmpty,
        (
            ClausesRep1IsEmpty = yes,
            % There are no clauses for class methods. The clauses are generated
            % later on, in polymorphism.expand_class_method_bodies.
            ( check_marker(Markers0, marker_class_method) ->
                % For the moment, we just insert the types of the head varss
                % into the clauses_info.
                map.from_corresponding_lists(HeadVars, ArgTypes0, VarTypes),
                clauses_info_set_vartypes(VarTypes, !ClausesInfo),
                pred_info_set_clauses_info(!.ClausesInfo, !PredInfo),
                % We also need to set the head_type_params field to indicate
                % that all the existentially quantified tvars in the head
                % of this pred are indeed bound by this predicate.
                prog_type.vars_list(ArgTypes0, HeadVarsIncludingExistentials),
                pred_info_set_head_type_params(HeadVarsIncludingExistentials,
                    !PredInfo),
                Error = no,
                Changed = no
            ;
                report_no_clauses("Error", PredId, !.PredInfo, !.ModuleInfo,
                    !IO),
                Error = yes,
                Changed = no
            )
        ;
            ClausesRep1IsEmpty = no,
            pred_info_get_typevarset(!.PredInfo, TypeVarSet0),
            pred_info_get_import_status(!.PredInfo, Status),
            ( check_marker(Markers0, marker_infer_type) ->
                % For a predicate whose type is inferred, the predicate is
                % allowed to bind the type variables in the head of the
                % predicate's type declaration. Such predicates are given an
                % initial type declaration of `pred foo(T1, T2, ..., TN)'
                % by make_hlds.m.
                Inferring = yes,
                write_pred_progress_message("% Inferring type of ",
                    PredId, !.ModuleInfo, !IO),
                !:HeadTypeParams = [],
                PredConstraints = constraints([], [])
            ;
                Inferring = no,
                write_pred_progress_message("% Type-checking ", PredId,
                    !.ModuleInfo, !IO),
                prog_type.vars_list(ArgTypes0, !:HeadTypeParams),
                pred_info_get_class_context(!.PredInfo, PredConstraints),
                constraint_list_get_tvars(PredConstraints ^ univ_constraints,
                    UnivTVars),
                list.append(UnivTVars, !HeadTypeParams),
                list.sort_and_remove_dups(!HeadTypeParams),
                list.delete_elems(!.HeadTypeParams, ExistQVars0,
                    !:HeadTypeParams)
            ),

            module_info_get_class_table(!.ModuleInfo, ClassTable),
            make_head_hlds_constraints(ClassTable, TypeVarSet0,
                PredConstraints, Constraints),
            ( pred_info_is_field_access_function(!.ModuleInfo, !.PredInfo) ->
                IsFieldAccessFunction = yes
            ;
                IsFieldAccessFunction = no
            ),
            pred_info_get_markers(!.PredInfo, Markers),
            typecheck_info_init(!.ModuleInfo, PredId, IsFieldAccessFunction,
                TypeVarSet0, VarSet, ExplicitVarTypes0, !.HeadTypeParams,
                Constraints, Status, Markers, !:Info),
            typecheck_info_get_type_assign_set(!.Info, OrigTypeAssignSet),
            get_clause_list(ClausesRep1, Clauses1),
            typecheck_clause_list(HeadVars, ArgTypes0, Clauses1, Clauses,
                !Info, !IO),
            % we need to perform a final pass of context reduction
            % at the end, before checking the typeclass constraints
            perform_context_reduction(OrigTypeAssignSet, !Info, !IO),
            typecheck_check_for_ambiguity(whole_pred, HeadVars, !Info, !IO),
            typecheck_info_get_final_info(!.Info, !.HeadTypeParams,
                ExistQVars0, ExplicitVarTypes0, TypeVarSet,
                !:HeadTypeParams, InferredVarTypes0,
                InferredTypeConstraints0, ConstraintProofs,
                ConstraintMap, TVarRenaming, ExistTypeRenaming),
            map.optimize(InferredVarTypes0, InferredVarTypes),
            clauses_info_set_vartypes(InferredVarTypes, !ClausesInfo),

            % Apply substitutions to the explicit vartypes.
            (
                ExistQVars0 = [],
                ExplicitVarTypes1 = ExplicitVarTypes0
            ;
                ExistQVars0 = [_ | _],
                apply_variable_renaming_to_vartypes(ExistTypeRenaming,
                    ExplicitVarTypes0, ExplicitVarTypes1)
            ),
            apply_variable_renaming_to_vartypes(TVarRenaming,
                ExplicitVarTypes1, ExplicitVarTypes),

            clauses_info_set_explicit_vartypes(ExplicitVarTypes, !ClausesInfo),
            clauses_info_set_clauses(Clauses, !ClausesInfo),
            pred_info_set_clauses_info(!.ClausesInfo, !PredInfo),
            pred_info_set_typevarset(TypeVarSet, !PredInfo),
            pred_info_set_constraint_proofs(ConstraintProofs, !PredInfo),
            pred_info_set_constraint_map(ConstraintMap, !PredInfo),

            % Split the inferred type class constraints into those that
            % apply only to the head variables, and those that apply to
            % type variables which occur only in the body.
            map.apply_to_list(HeadVars, InferredVarTypes, ArgTypes),
            prog_type.vars_list(ArgTypes, ArgTypeVars),
            restrict_to_head_vars(InferredTypeConstraints0, ArgTypeVars,
                InferredTypeConstraints, UnprovenBodyConstraints),

            % If there are any as-yet-unproven constraints on type variables
            % in the body, then save these in the pred_info. If it turns out
            % that this pass was the last pass of type inference, the
            % post_typecheck.m will report an error. But we can't report
            % an error now, because a later pass of type inference could cause
            % some type variables to become bound to types that make the
            % constraints satisfiable, causing the error to go away.
            pred_info_set_unproven_body_constraints(UnprovenBodyConstraints,
                !PredInfo),

            (
                Inferring = yes,
                % We need to infer which of the head variable types must be
                % existentially quantified.
                infer_existential_types(ArgTypeVars, ExistQVars,
                    !HeadTypeParams),

                % Now save the information we inferred in the pred_info
                pred_info_set_head_type_params(!.HeadTypeParams, !PredInfo),
                pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes,
                    !PredInfo),
                pred_info_get_class_context(!.PredInfo, OldTypeConstraints),
                pred_info_set_class_context(InferredTypeConstraints,
                    !PredInfo),

                % Check if anything changed.
                (
                    % If the argument types and the type constraints are
                    % identical up to renaming, then nothing has changed.
                    pred_info_get_tvar_kinds(!.PredInfo, TVarKinds),
                    argtypes_identical_up_to_renaming(TVarKinds, ExistQVars0,
                        ArgTypes0, OldTypeConstraints, ExistQVars, ArgTypes,
                        InferredTypeConstraints)
                ->
                    Changed = no
                ;
                    Changed = yes
                )
            ;
                Inferring = no,
                pred_info_set_head_type_params(!.HeadTypeParams, !PredInfo),
                pred_info_get_origin(!.PredInfo, Origin0),

                % Leave the original argtypes etc., but apply any substititions
                % that map existentially quantified type variables to other
                % type vars, and then rename them all to match the new
                % typevarset, so that the type variables names match up
                % (e.g. with the type variables in the constraint_proofs)

                % Apply any type substititions that map existentially
                % quantified type variables to other type vars.
                (
                    ExistQVars0 = [],
                    % Optimize common case.
                    ExistQVars1 = [],
                    ArgTypes1 = ArgTypes0,
                    PredConstraints1 = PredConstraints,
                    Origin1 = Origin0
                ;
                    ExistQVars0 = [_ | _],
                    list.foldl(
                        check_existq_clause(!.Info, TypeVarSet, ExistQVars0),
                        Clauses, !IO),

                    apply_var_renaming_to_var_list(ExistQVars0,
                        ExistTypeRenaming, ExistQVars1),
                    apply_variable_renaming_to_type_list(ExistTypeRenaming,
                        ArgTypes0, ArgTypes1),
                    apply_variable_renaming_to_prog_constraints(
                        ExistTypeRenaming, PredConstraints, PredConstraints1),
                    rename_instance_method_constraints(ExistTypeRenaming,
                        Origin0, Origin1)
                ),

                % Rename them all to match the new typevarset.
                apply_var_renaming_to_var_list(ExistQVars1,
                    TVarRenaming, ExistQVars),
                apply_variable_renaming_to_type_list(TVarRenaming, ArgTypes1,
                    RenamedOldArgTypes),
                apply_variable_renaming_to_prog_constraints(TVarRenaming,
                    PredConstraints1, RenamedOldConstraints),
                rename_instance_method_constraints(TVarRenaming,
                    Origin1, Origin),

                % Save the results in the pred_info.
                pred_info_set_arg_types(TypeVarSet, ExistQVars,
                    RenamedOldArgTypes, !PredInfo),
                pred_info_set_class_context(RenamedOldConstraints, !PredInfo),
                pred_info_set_origin(Origin, !PredInfo),

                Changed = no
            ),
            typecheck_info_get_found_error(!.Info, Error)
        )
    ).

:- pred check_existq_clause(typecheck_info::in, tvarset::in, existq_tvars::in,
    clause::in, io::di, io::uo) is det.

check_existq_clause(Info, TypeVarSet, ExistQVars, Clause, !IO) :-
    Goal = Clause ^ clause_body,
    ( Goal = call_foreign_proc(_, _, _, _, _, _, Impl) - _ ->
        list.foldl(check_mention_existq_var(Info, TypeVarSet, Impl),
            ExistQVars, !IO)
    ;
        true
    ).

:- pred check_mention_existq_var(typecheck_info::in, tvarset::in,
    pragma_foreign_code_impl::in, tvar::in, io::di, io::uo) is det.

check_mention_existq_var(Info, TypeVarSet, Impl, TVar, !IO) :-
    varset.lookup_name(TypeVarSet, TVar, Name),
    VarName = "TypeInfo_for_" ++ Name,
    ( foreign_code_uses_variable(Impl, VarName) ->
        true
    ;
        report_missing_tvar_in_foreign_code(Info, VarName, !IO)
    ).

    % Mark the predicate as a stub, and generate a clause of the form
    %   <p>(...) :-
    %       PredName = "<Predname>",
    %       private_builtin.no_clauses(PredName).
    % or
    %   <p>(...) :-
    %       PredName = "<Predname>",
    %       private_builtin.sorry(PredName).
    % depending on whether the predicate is part of
    % the Mercury standard library or not.
:- pred generate_stub_clause(string::in, pred_info::in, pred_info::out,
    module_info::in, clause::out, prog_varset::in, prog_varset::out) is det.

generate_stub_clause(PredName, !PredInfo, ModuleInfo, StubClause, !VarSet) :-
    % Mark the predicate as a stub
    % (i.e. record that it originally had no clauses)
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(marker_stub, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo),

    % Generate `PredName = "<PredName>"'.
    varset.new_named_var(!.VarSet, "PredName", PredNameVar, !:VarSet),
    make_string_const_construction(PredNameVar, PredName, UnifyGoal),

    % Generate `private_builtin.no_clauses(PredName)'
    % or `private_builtin.sorry(PredName)'
    ModuleName = pred_info_module(!.PredInfo),
    ( mercury_std_library_module_name(ModuleName) ->
        CalleeName = "sorry"
    ;
        CalleeName = "no_clauses"
    ),
    pred_info_context(!.PredInfo, Context),
    generate_simple_call(mercury_private_builtin_module, CalleeName,
        predicate, only_mode, detism_det, purity_pure, [PredNameVar], [], [],
        ModuleInfo, Context, CallGoal),

    % Combine the unification and call into a conjunction.
    goal_info_init(Context, GoalInfo),
    Body = conj(plain_conj, [UnifyGoal, CallGoal]) - GoalInfo,
    StubClause = clause([], Body, impl_lang_mercury, Context).

:- pred rename_instance_method_constraints(tvar_renaming::in,
    pred_origin::in, pred_origin::out) is det.

rename_instance_method_constraints(Renaming, Origin0, Origin) :-
    ( Origin0 = origin_instance_method(Constraints0) ->
        Constraints0 = instance_method_constraints(ClassId, InstanceTypes0,
            InstanceConstraints0, ClassMethodClassContext0),
        apply_variable_renaming_to_type_list(Renaming, InstanceTypes0,
            InstanceTypes),
        apply_variable_renaming_to_prog_constraint_list(Renaming,
            InstanceConstraints0, InstanceConstraints),
        apply_variable_renaming_to_prog_constraints(Renaming,
            ClassMethodClassContext0, ClassMethodClassContext),
        Constraints = instance_method_constraints(ClassId,
            InstanceTypes, InstanceConstraints, ClassMethodClassContext),
        Origin = origin_instance_method(Constraints)
    ;
        Origin = Origin0
    ).

    % Infer which of the head variable types must be existentially quantified.
    %
:- pred infer_existential_types(list(tvar)::in, existq_tvars::out,
    head_type_params::in, head_type_params::out) is det.

infer_existential_types(ArgTypeVars, ExistQVars,
        HeadTypeParams0, HeadTypeParams) :-
    % First, infer which of the head variable types must be existentially
    % quantified: anything that was inserted into the HeadTypeParams0 set must
    % have been inserted due to an existential type in something we called,
    % and thus must be existentially quantified. (Note that concrete types
    % are "more general" than existentially quantified types, so we prefer to
    % infer a concrete type if we can rather than an existential type.)

    set.list_to_set(ArgTypeVars, ArgTypeVarsSet),
    set.list_to_set(HeadTypeParams0, HeadTypeParamsSet),
    set.intersect(ArgTypeVarsSet, HeadTypeParamsSet, ExistQVarsSet),
    set.difference(ArgTypeVarsSet, ExistQVarsSet, UnivQVarsSet),
    set.to_sorted_list(ExistQVarsSet, ExistQVars),
    set.to_sorted_list(UnivQVarsSet, UnivQVars),

    % Then we need to insert the universally quantified head variable types
    % into the HeadTypeParams set, which will now contain all the type
    % variables that are produced either by stuff we call or by our caller.
    % This is needed so that it has the right value when post_typecheck.m
    % uses it to check for unbound type variables.

    list.append(UnivQVars, HeadTypeParams0, HeadTypeParams).

    % restrict_to_head_vars(Constraints0, HeadVarTypes, Constraints,
    %       UnprovenConstraints):
    %
    % Constraints is the subset of Constraints0 which contain no type variables
    % other than those in HeadVarTypes. UnprovenConstraints is any unproven
    % (universally quantified) type constraints on variables not in
    % HeadVarTypes.
    %
:- pred restrict_to_head_vars(prog_constraints::in, list(tvar)::in,
    prog_constraints::out, list(prog_constraint)::out) is det.

restrict_to_head_vars(constraints(UnivCs0, ExistCs0), ArgVarTypes,
        constraints(UnivCs, ExistCs), UnprovenCs) :-
    restrict_to_head_vars_2(UnivCs0, ArgVarTypes, UnivCs, UnprovenCs),
    restrict_to_head_vars_2(ExistCs0, ArgVarTypes, ExistCs, _).

:- pred restrict_to_head_vars_2(list(prog_constraint)::in, list(tvar)::in,
    list(prog_constraint)::out, list(prog_constraint)::out) is det.

restrict_to_head_vars_2(ClassConstraints, HeadTypeVars, HeadClassConstraints,
        OtherClassConstraints) :-
    list.filter(is_head_class_constraint(HeadTypeVars),
        ClassConstraints, HeadClassConstraints, OtherClassConstraints).

:- pred is_head_class_constraint(list(tvar)::in, prog_constraint::in)
    is semidet.

is_head_class_constraint(HeadTypeVars, constraint(_Name, Types)) :-
    all [TVar] (
            prog_type.type_list_contains_var(Types, TVar)
        =>
            list.member(TVar, HeadTypeVars)
    ).

    % Check whether the argument types, type quantifiers, and type
    % constraints are identical up to renaming.
    %
    % Note that we can't compare each of the parts separately, since
    % we need to ensure that the renaming (if any) is consistent
    % over all the arguments and all the constraints.  So we need
    % to append all the relevant types into one big type list and
    % then compare them in a single call to identical_up_to_renaming.
    %
:- pred argtypes_identical_up_to_renaming(tvar_kind_map::in,
    existq_tvars::in, list(mer_type)::in, prog_constraints::in,
    existq_tvars::in, list(mer_type)::in, prog_constraints::in) is semidet.

argtypes_identical_up_to_renaming(KindMap, ExistQVarsA, ArgTypesA,
        TypeConstraintsA, ExistQVarsB, ArgTypesB, TypeConstraintsB) :-
    same_structure(TypeConstraintsA, TypeConstraintsB,
        ConstrainedTypesA, ConstrainedTypesB),
    prog_type.var_list_to_type_list(KindMap, ExistQVarsA, ExistQVarTypesA),
    prog_type.var_list_to_type_list(KindMap, ExistQVarsB, ExistQVarTypesB),
    list.condense([ExistQVarTypesA, ArgTypesA, ConstrainedTypesA], TypesListA),
    list.condense([ExistQVarTypesB, ArgTypesB, ConstrainedTypesB], TypesListB),
    identical_up_to_renaming(TypesListA, TypesListB).

    % Check if two sets of type class constraints have the same structure
    % (i.e. they specify the same list of type classes with the same
    % arities) and if so, concatenate the argument types for all the
    % type classes in each set of type class constraints and return them.
    %
:- pred same_structure(prog_constraints::in, prog_constraints::in,
    list(mer_type)::out, list(mer_type)::out) is semidet.

same_structure(ConstraintsA, ConstraintsB, TypesA, TypesB) :-
    ConstraintsA = constraints(UnivCsA, ExistCsA),
    ConstraintsB = constraints(UnivCsB, ExistCsB),
    % these calls to same_length are just an optimization,
    % to catch the simple cases quicker
    list.same_length(UnivCsA, UnivCsB),
    list.same_length(ExistCsA, ExistCsB),
    same_structure_2(UnivCsA, UnivCsB, UnivTypesA, UnivTypesB),
    same_structure_2(ExistCsA, ExistCsB, ExistTypesA, ExistTypesB),
    TypesA = ExistTypesA ++ UnivTypesA,
    TypesB = ExistTypesB ++ UnivTypesB.

:- pred same_structure_2(list(prog_constraint)::in, list(prog_constraint)::in,
    list(mer_type)::out, list(mer_type)::out) is semidet.

same_structure_2([], [], [], []).
same_structure_2([ConstraintA | ConstraintsA], [ConstraintB | ConstraintsB],
        TypesA, TypesB) :-
    ConstraintA = constraint(ClassName, ArgTypesA),
    ConstraintB = constraint(ClassName, ArgTypesB),
    list.same_length(ArgTypesA, ArgTypesB),
    same_structure_2(ConstraintsA, ConstraintsB, TypesA0, TypesB0),
    TypesA = ArgTypesA ++ TypesA0,
    TypesB = ArgTypesB ++ TypesB0.

    % Check whether two lists of types are identical up to renaming.
    %
:- pred identical_up_to_renaming(list(mer_type)::in, list(mer_type)::in)
    is semidet.

identical_up_to_renaming(TypesList1, TypesList2) :-
    % They are identical up to renaming if they each subsume each other.
    type_list_subsumes(TypesList1, TypesList2, _),
    type_list_subsumes(TypesList2, TypesList1, _).

    % A compiler-generated predicate only needs type checking if
    % (a) it is a user-defined equality pred, or
    % (b) it is the unification or comparison predicate for an existially
    %     quantified type.
    %
    % In case (b), we need to typecheck it to fill in the head_type_params
    % field in the pred_info.
    %
:- pred special_pred_needs_typecheck(pred_info::in, module_info::in)
    is semidet.

special_pred_needs_typecheck(PredInfo, ModuleInfo) :-
    % Check if the predicate is a compiler-generated special
    % predicate, and if so, for which type.
    pred_info_get_origin(PredInfo, Origin),
    Origin = origin_special_pred(SpecialPredId - TypeCtor),

    % Check that the special pred isn't one of the builtin types which don't
    % have a hlds_type_defn.
    \+ list.member(TypeCtor, builtin_type_ctors_with_no_hlds_type_defn),

    % Check whether that type is a type for which there is a user-defined
    % equality predicate, or which is existentially typed.
    module_info_get_type_table(ModuleInfo, TypeTable),
    map.lookup(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, Body),
    special_pred_for_type_needs_typecheck(ModuleInfo, SpecialPredId, Body).

%-----------------------------------------------------------------------------%

    % For a field access function for which the user has supplied
    % a declaration but no clauses, add a clause
    % 'foo :='(X, Y) = 'foo :='(X, Y).
    % As for the default clauses added for builtins, this is not a recursive
    % call -- post_typecheck.m will expand the body into unifications.
    %
:- pred maybe_add_field_access_function_clause(module_info::in,
    pred_info::in, pred_info::out) is det.

maybe_add_field_access_function_clause(ModuleInfo, !PredInfo) :-
    pred_info_get_import_status(!.PredInfo, ImportStatus),
    pred_info_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0),
    (
        pred_info_is_field_access_function(ModuleInfo, !.PredInfo),
        clause_list_is_empty(ClausesRep0) = yes,
        status_defined_in_this_module(ImportStatus) = yes
    ->
        clauses_info_get_headvars(ClausesInfo0, HeadVars),
        pred_args_to_func_args(HeadVars, FuncArgs, FuncRetVal),
        pred_info_context(!.PredInfo, Context),
        FuncModule = pred_info_module(!.PredInfo),
        FuncName = pred_info_name(!.PredInfo),
        PredArity = pred_info_orig_arity(!.PredInfo),
        adjust_func_arity(function, FuncArity, PredArity),
        FuncSymName = qualified(FuncModule, FuncName),
        create_atomic_complicated_unification(FuncRetVal,
            rhs_functor(cons(FuncSymName, FuncArity), no, FuncArgs),
            Context, umc_explicit, [], Goal0),
        Goal0 = GoalExpr - GoalInfo0,
        set.list_to_set(HeadVars, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        Goal = GoalExpr - GoalInfo,
        ProcIds = [], % the clause applies to all procedures.
        Clause = clause(ProcIds, Goal, impl_lang_mercury, Context),
        clauses_info_set_clauses([Clause], ClausesInfo0, ClausesInfo),
        pred_info_update_goal_type(goal_type_clause_and_foreign, !PredInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo),
        pred_info_get_markers(!.PredInfo, Markers0),
        add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
        pred_info_set_markers(Markers, !PredInfo)
    ;
        true
    ).

    % If there is only one clause, use the original head variables
    % from the clause rather than the introduced `HeadVar__n' variables
    % as the head variables in the proc_info.
    % This gives better error messages, more meaningful variable
    % names in the debugger and slightly faster compilation.
    %
:- pred maybe_improve_headvar_names(globals::in, pred_info::in, pred_info::out)
    is det.

maybe_improve_headvar_names(Globals, !PredInfo) :-
    pred_info_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_clauses_only(ClausesInfo0, Clauses0),
    clauses_info_get_headvars(ClausesInfo0, HeadVars0),
    clauses_info_get_varset(ClausesInfo0, VarSet0),
    (
        % Don't do this when making a `.opt' file.
        % intermod.m needs to perform a similar transformation
        % which this transformation would interfere with (intermod.m
        % places the original argument terms, not just the argument
        % variables in the clause head, and this pass would make it
        % difficult to work out what were the original arguments).
        globals.lookup_bool_option(Globals, make_optimization_interface, yes)
    ->
        true
    ;
        Clauses0 = [SingleClause0]
    ->
        SingleClause0 = clause(ApplicableProcs, Goal0, Language, Context),

        Goal0 = _ - GoalInfo0,
        goal_to_conj_list(Goal0, Conj0),
        improve_single_clause_headvars(Conj0, HeadVars0, [],
            VarSet0, VarSet, map.init, Subst, [], RevConj),

        goal_info_get_nonlocals(GoalInfo0, NonLocals0),
        goal_util.rename_vars_in_var_set(no, Subst, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        conj_list_to_goal(list.reverse(RevConj), GoalInfo, Goal),

        apply_partial_map_to_list(Subst, HeadVars0, HeadVars),
        clauses_info_set_headvars(HeadVars, ClausesInfo0, ClausesInfo1),

        SingleClause = clause(ApplicableProcs, Goal, Language, Context),
        clauses_info_set_clauses([SingleClause], ClausesInfo1, ClausesInfo2),
        clauses_info_set_varset(VarSet, ClausesInfo2, ClausesInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo)
    ;
        % If a headvar is assigned to a variable with the same name
        % (or no name) in every clause, rename it to have that name.
        list.foldl2(find_headvar_names_in_clause(VarSet0, HeadVars0),
            Clauses0, map.init, HeadVarNames, yes, _),
        map.foldl(maybe_update_headvar_name, HeadVarNames, VarSet0, VarSet),
        clauses_info_set_varset(VarSet, ClausesInfo0, ClausesInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo)
    ).

:- pred maybe_update_headvar_name(prog_var::in, maybe(string)::in,
    prog_varset::in, prog_varset::out) is det.

maybe_update_headvar_name(HeadVar, MaybeHeadVarName, VarSet0, VarSet) :-
    (
        MaybeHeadVarName = yes(HeadVarName),
        varset.name_var(VarSet0, HeadVar, HeadVarName, VarSet)
    ;
        MaybeHeadVarName = no,
        VarSet = VarSet0
    ).

:- pred improve_single_clause_headvars(list(hlds_goal)::in, list(prog_var)::in,
    list(prog_var)::in, prog_varset::in, prog_varset::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

improve_single_clause_headvars([], _, _, !VarSet, !Subst, !RevConj).
improve_single_clause_headvars([Goal | Conj0], HeadVars, SeenVars0,
        !VarSet, !Subst, !RevConj) :-
    ( goal_is_headvar_unification(HeadVars, Goal, HeadVar, OtherVar) ->
        % If the headvar doesn't appear elsewhere the unification
        % can be removed.
        (
            % The headvars must be distinct variables, so check that this
            % variable doesn't already appear in the argument list.
            \+ list.member(OtherVar, HeadVars),
            \+ list.member(OtherVar, SeenVars0),

            \+ ( some [OtherGoal] (
                ( list.member(OtherGoal, Conj0)
                ; list.member(OtherGoal, !.RevConj)
                ),
                OtherGoal = _ - OtherGoalInfo,
                goal_info_get_nonlocals(OtherGoalInfo, OtherNonLocals),
                set.member(HeadVar, OtherNonLocals)
            ))
        ->
            SeenVars = [OtherVar | SeenVars0],
            !:Subst = map.det_insert(!.Subst, HeadVar, OtherVar),

            % If the variable wasn't named, use the `HeadVar__n' name.
            (
                \+ varset.search_name(!.VarSet, OtherVar, _),
                varset.search_name(!.VarSet, HeadVar, HeadVarName)
            ->
                varset.name_var(!.VarSet, OtherVar, HeadVarName, !:VarSet)
            ;
                true
            )
        ;
            !:RevConj = [Goal | !.RevConj],
            SeenVars = SeenVars0,
            ( varset.search_name(!.VarSet, OtherVar, OtherVarName) ->
                % The unification can't be eliminated,
                % so just rename the head variable.
                varset.name_var(!.VarSet, HeadVar, OtherVarName, !:VarSet)
            ; varset.search_name(!.VarSet, HeadVar, HeadVarName) ->
                % If the variable wasn't named, use the `HeadVar__n' name.
                varset.name_var(!.VarSet, OtherVar, HeadVarName, !:VarSet)
            ;
                true
            )
        )
    ;
        !:RevConj = [Goal | !.RevConj],
        SeenVars = SeenVars0
    ),
    improve_single_clause_headvars(Conj0, HeadVars, SeenVars,
        !VarSet, !Subst, !RevConj).

    % Head variables that have the same name in each clause
    % will have an entry of `yes(Name)' in the result map.
    %
:- pred find_headvar_names_in_clause(prog_varset::in,
    list(prog_var)::in, clause::in,
    map(prog_var, maybe(string))::in, map(prog_var, maybe(string))::out,
    bool::in, bool::out) is det.

find_headvar_names_in_clause(VarSet, HeadVars, Clause, HeadVarMap0, HeadVarMap,
        IsFirstClause, no) :-
    Goal = Clause ^ clause_body,
    goal_to_conj_list(Goal, Conj),
    ClauseHeadVarMap = list.foldl(
        find_headvar_names_in_goal(VarSet, HeadVars), Conj, map.init),
    (
        IsFirstClause = yes,
        HeadVarMap = ClauseHeadVarMap
    ;
        IsFirstClause = no,
        % Check that the variables in this clause match
        % the names in previous clauses.
        HeadVarMap1 = map.foldl(
            (func(HeadVar, MaybeHeadVarName, Map) =
                (
                    map.search(Map, HeadVar, MaybeClauseHeadVarName),
                    MaybeHeadVarName = MaybeClauseHeadVarName
                ->
                    Map
                ;
                    map.set(Map, HeadVar, no)
                )
            ), HeadVarMap0, ClauseHeadVarMap),

        % Check for variables which weren't named in previous
        % clauses. It would be confusing to refer to variable
        % `A' in the second clause below.
        %   p(A, _).
        %   p([_ | _], _).
        HeadVarMap = map.foldl(
            (func(HeadVar, _, Map) =
                ( map.contains(HeadVarMap0, HeadVar) ->
                    Map
                ;
                    map.set(Map, HeadVar, no)
                )
            ), HeadVarMap1, HeadVarMap1)
    ).

:- func find_headvar_names_in_goal(prog_varset, list(prog_var), hlds_goal,
    map(prog_var, maybe(string))) = map(prog_var, maybe(string)).

find_headvar_names_in_goal(VarSet, HeadVars, Goal, HeadVarMap0) = HeadVarMap :-
    ( goal_is_headvar_unification(HeadVars, Goal, HeadVar, OtherVar) ->
        maybe_pred(varset.search_name(VarSet), OtherVar, MaybeOtherVarName),
        ( map.search(HeadVarMap0, HeadVar, MaybeHeadVarName) ->
            ( MaybeOtherVarName = MaybeHeadVarName ->
                HeadVarMap = HeadVarMap0
            ;
                HeadVarMap = map.det_update(HeadVarMap0, HeadVar, no)
            )
        ;
            HeadVarMap = map.set(HeadVarMap0, HeadVar, MaybeOtherVarName)
        )
    ;
        HeadVarMap = HeadVarMap0
    ).

:- pred goal_is_headvar_unification(list(prog_var)::in, hlds_goal::in,
    prog_var::out, prog_var::out) is semidet.

goal_is_headvar_unification(HeadVars, Goal, HeadVar, OtherVar) :-
    Goal = unify(LVar, rhs_var(RVar), _, _, _) - _,
    ( list.member(LVar, HeadVars) ->
        HeadVar = LVar,
        OtherVar = RVar
    ; list.member(RVar, HeadVars) ->
        HeadVar = RVar,
        OtherVar = LVar
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

    % Iterate over the list of clauses for a predicate.
    %
:- pred typecheck_clause_list(list(prog_var)::in, list(mer_type)::in,
    list(clause)::in, list(clause)::out,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_clause_list(_, _, [], [], !Info, !IO).
typecheck_clause_list(HeadVars, ArgTypes, [Clause0 | Clauses0],
        [Clause | Clauses], !Info, !IO) :-
    typecheck_clause(HeadVars, ArgTypes, Clause0, Clause, !Info, !IO),
    typecheck_clause_list(HeadVars, ArgTypes, Clauses0, Clauses, !Info, !IO).

%-----------------------------------------------------------------------------%

    % Type-check a single clause.

    % As we go through a clause, we determine the possible type assignments
    % for the clause. A type assignment is an assignment of a type to each
    % variable in the clause.
    %
    % Note that this may cause exponential time & space usage in the presence
    % of overloading of predicates and/or functors. This is a potentially
    % serious problem, but there's no easy solution apparent.
    %
    % It would be more natural to use non-determinism to write this code,
    % and perhaps even more efficient. But doing it nondeterministically
    % would make good error messages very difficult.
    %
    % We should perhaps do manual garbage collection here.
    %
:- pred typecheck_clause(list(prog_var)::in, list(mer_type)::in,
    clause::in, clause::out,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_clause(HeadVars, ArgTypes, !Clause, !Info, !IO) :-
    Body0 = !.Clause ^ clause_body,
    Context = !.Clause ^clause_context,
    typecheck_info_set_context(Context, !Info),

    % Typecheck the clause - first the head unification, and then the body.
    typecheck_var_has_type_list(HeadVars, ArgTypes, 1, !Info, !IO),
    typecheck_goal(Body0, Body, !Info, !IO),
    type_checkpoint("end of clause", !.Info, !IO),
    !:Clause = !.Clause ^ clause_body := Body,
    typecheck_info_set_context(Context, !Info),
    typecheck_check_for_ambiguity(clause_only, HeadVars, !Info, !IO).

%-----------------------------------------------------------------------------%

    % typecheck_check_for_ambiguity/3:
    % If there are multiple type assignments,
    % then we issue an error message here.

    % If stuff-to-check = whole_pred, report an error for any ambiguity,
    % and also check for unbound type variables.
    % But if stuff-to-check = clause_only, then only report
    % errors for type ambiguities that don't involve the head vars,
    % because we may be able to resolve a type ambiguity for a head var
    % in one clause by looking at later clauses.
    % (Ambiguities in the head variables can only arise if we are
    % inferring the type for this pred.)
    %
:- type stuff_to_check
    --->    clause_only
    ;       whole_pred.

:- pred typecheck_check_for_ambiguity(stuff_to_check::in, list(prog_var)::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_check_for_ambiguity(StuffToCheck, HeadVars, !Info, !IO) :-
    typecheck_info_get_type_assign_set(!.Info, TypeAssignSet),
    (
        % There should always be a type assignment, because if there is
        % an error somewhere, instead of setting the current type assignment
        % set to the empty set, the type-checker should continue with the
        % previous type assignment set (so that it can detect other errors
        % in the same clause).
        TypeAssignSet = [],
        unexpected(this_file,
            "internal error in typechecker: no type-assignment")
    ;
        TypeAssignSet = [_SingleTypeAssign]
    ;
        TypeAssignSet = [TypeAssign1, TypeAssign2 | _],

        % We only report an ambiguity error if
        % (a) we haven't encountered any other errors and if
        %     StuffToCheck = clause_only(_), and also
        % (b) the ambiguity occurs only in the body, rather than in the
        %     head variables (and hence can't be resolved by looking at
        %     later clauses).

        typecheck_info_get_found_error(!.Info, FoundError),
        (
            FoundError = no,
            (
                StuffToCheck = whole_pred
            ;
                StuffToCheck = clause_only,

                % Only report an error if the headvar types are identical
                % (which means that the ambiguity must have occurred
                % in the body)
                type_assign_get_var_types(TypeAssign1, VarTypes1),
                type_assign_get_var_types(TypeAssign2, VarTypes2),
                type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
                type_assign_get_type_bindings(TypeAssign2, TypeBindings2),
                map.apply_to_list(HeadVars, VarTypes1, HeadTypes1),
                map.apply_to_list(HeadVars, VarTypes2, HeadTypes2),
                apply_rec_subst_to_type_list(TypeBindings1, HeadTypes1,
                    FinalHeadTypes1),
                apply_rec_subst_to_type_list(TypeBindings2, HeadTypes2,
                    FinalHeadTypes2),
                identical_up_to_renaming(FinalHeadTypes1, FinalHeadTypes2)
            )
        ->
            typecheck_info_set_found_error(yes, !Info),
            report_ambiguity_error(!.Info, TypeAssign1, TypeAssign2, !IO)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal(hlds_goal::in, hlds_goal::out,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

    % Typecheck a goal.
    % Note that we save the context of the goal in the typeinfo for
    % use in error messages.  Also, if the context of the goal is empty,
    % we set the context of the goal from the surrounding
    % context saved in the type-info.  (That should probably be done
    % in make_hlds, but it was easier to do here.)
    %
typecheck_goal(Goal0 - GoalInfo0, Goal - GoalInfo, !Info, !IO) :-
    goal_info_get_context(GoalInfo0, Context),
    term.context_init(EmptyContext),
    ( Context = EmptyContext ->
        typecheck_info_get_context(!.Info, EnclosingContext),
        goal_info_set_context(EnclosingContext, GoalInfo0, GoalInfo)
    ;
        GoalInfo = GoalInfo0,
        typecheck_info_set_context(Context, !Info)
    ),
    typecheck_goal_2(Goal0, Goal, GoalInfo, !Info, !IO),
    check_warn_too_much_overloading(!Info, !IO).

:- pred typecheck_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, typecheck_info::in, typecheck_info::out,
    io::di, io::uo) is det.

typecheck_goal_2(GoalExpr0, GoalExpr, GoalInfo, !Info, !IO) :-
    (
        GoalExpr0 = conj(ConjType, List0),
        type_checkpoint("conj", !.Info, !IO),
        typecheck_goal_list(List0, List, !Info, !IO),
        GoalExpr = conj(ConjType, List)
    ;
        GoalExpr0 = disj(List0),
        type_checkpoint("disj", !.Info, !IO),
        typecheck_goal_list(List0, List, !Info, !IO),
        GoalExpr = disj(List)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        type_checkpoint("if", !.Info, !IO),
        typecheck_goal(Cond0, Cond, !Info, !IO),
        type_checkpoint("then", !.Info, !IO),
        typecheck_goal(Then0, Then, !Info, !IO),
        type_checkpoint("else", !.Info, !IO),
        typecheck_goal(Else0, Else, !Info, !IO),
        ensure_vars_have_a_type(Vars, !Info, !IO),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        type_checkpoint("not", !.Info, !IO),
        typecheck_goal(SubGoal0, SubGoal, !Info, !IO),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        type_checkpoint("scope", !.Info, !IO),
        typecheck_goal(SubGoal0, SubGoal, !Info, !IO),
        (
            Reason = exist_quant(Vars),
            ensure_vars_have_a_type(Vars, !Info, !IO)
        ;
            Reason = promise_purity(_, _)
        ;
            Reason = promise_solutions(Vars, _),
            ensure_vars_have_a_type(Vars, !Info, !IO)
        ;
            Reason = commit(_)
        ;
            Reason = barrier(_)
        ;
            Reason = from_ground_term(_)
        ;
            Reason = trace_goal(_, _, _, _)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = plain_call(_, ProcId, Args, BI, UC, Name),
        type_checkpoint("call", !.Info, !IO),
        list.length(Args, Arity),
        CurCall = simple_call_id(predicate, Name, Arity),
        typecheck_info_set_called_predid(plain_call_id(CurCall), !Info),
        goal_info_get_goal_path(GoalInfo, GoalPath),
        typecheck_call_pred(CurCall, Args, GoalPath, PredId, !Info, !IO),
        GoalExpr = plain_call(PredId, ProcId, Args, BI, UC, Name)
    ;
        GoalExpr0 = generic_call(GenericCall0, Args, Modes, Detism),
        hlds_goal.generic_call_id(GenericCall0, CallId),
        typecheck_info_set_called_predid(CallId, !Info),
        (
            GenericCall0 = higher_order(PredVar, Purity, _, _),
            GenericCall = GenericCall0,
            type_checkpoint("higher-order call", !.Info, !IO),
            typecheck_higher_order_call(PredVar, Purity, Args, !Info, !IO)
        ;
            GenericCall0 = class_method(_, _, _, _),
            unexpected(this_file,
                "typecheck_goal_2: unexpected class method call")
        ;
            GenericCall0 = event_call(EventName),
            GenericCall = GenericCall0,
            type_checkpoint("event call", !.Info, !IO),
            typecheck_event_call(EventName, Args, !Info, !IO)
        ;
            GenericCall0 = cast(_),
            % A cast imposes no restrictions on its argument types,
            % so nothing needs to be done here.
            GenericCall = GenericCall0
        ),
        GoalExpr = generic_call(GenericCall, Args, Modes, Detism)
    ;
        GoalExpr0 = unify(LHS, RHS0, UnifyMode, Unification, UnifyContext),
        type_checkpoint("unify", !.Info, !IO),
        typecheck_info_set_arg_num(0, !Info),
        typecheck_info_set_unify_context(UnifyContext, !Info),
        goal_info_get_goal_path(GoalInfo, GoalPath),
        typecheck_unification(LHS, RHS0, RHS, GoalPath, !Info, !IO),
        GoalExpr = unify(LHS, RHS, UnifyMode, Unification, UnifyContext)
    ;
        GoalExpr0 = switch(_, _, _),
        unexpected(this_file, "typecheck_goal_2: unexpected switch")
    ;
        GoalExpr0 = call_foreign_proc(_, PredId, _, Args, _, _, _),
        % Foreign_procs are automatically generated, so they will always be
        % type-correct, but we need to do the type analysis in order to
        % correctly compute the HeadTypeParams that result from existentially
        % typed foreign_procs. (We could probably do that more efficiently
        % than the way it is done below, though.)
        typecheck_info_get_type_assign_set(!.Info, OrigTypeAssignSet),
        ArgVars = list.map(foreign_arg_var, Args),
        goal_info_get_goal_path(GoalInfo, GoalPath),
        typecheck_call_pred_id(PredId, ArgVars, GoalPath, !Info, !IO),
        perform_context_reduction(OrigTypeAssignSet, !Info, !IO),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShorthandGoal0),
        typecheck_goal_2_shorthand(ShorthandGoal0, ShorthandGoal, !Info, !IO),
        GoalExpr = shorthand(ShorthandGoal)
    ).

:- pred typecheck_goal_2_shorthand(shorthand_goal_expr::in,
    shorthand_goal_expr::out,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_goal_2_shorthand(bi_implication(LHS0, RHS0),
        bi_implication(LHS, RHS), !Info, !IO) :-
    type_checkpoint("<=>", !.Info, !IO),
    typecheck_goal(LHS0, LHS, !Info, !IO),
    typecheck_goal(RHS0, RHS, !Info, !IO).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal_list(list(hlds_goal)::in, list(hlds_goal)::out,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_goal_list([], [], !Info, !IO).
typecheck_goal_list([Goal0 | Goals0], [Goal | Goals], !Info, !IO) :-
    typecheck_goal(Goal0, Goal, !Info, !IO),
    typecheck_goal_list(Goals0, Goals, !Info, !IO).

%-----------------------------------------------------------------------------%

    % Ensure that each variable in Vars has been assigned a type.
    %
:- pred ensure_vars_have_a_type(list(prog_var)::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

ensure_vars_have_a_type(Vars, !Info, !IO) :-
    (
        Vars = []
    ;
        Vars = [_ | _],
        % Invent some new type variables to use as the types of these
        % variables. Since each type is the type of a program variable,
        % each must have kind `star'.
        list.length(Vars, NumVars),
        varset.init(TypeVarSet0),
        varset.new_vars(TypeVarSet0, NumVars, TypeVars, TypeVarSet),
        prog_type.var_list_to_type_list(map.init, TypeVars, Types),
        empty_hlds_constraints(EmptyConstraints),
        typecheck_var_has_polymorphic_type_list(Vars, TypeVarSet, [],
            Types, EmptyConstraints, !Info, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred typecheck_higher_order_call(prog_var::in, purity::in,
    list(prog_var)::in, typecheck_info::in, typecheck_info::out,
    io::di, io::uo) is det.

typecheck_higher_order_call(PredVar, Purity, Args, !Info, !IO) :-
    list.length(Args, Arity),
    higher_order_pred_type(Purity, Arity, lambda_normal,
        TypeVarSet, PredVarType, ArgTypes),
    % The class context is empty because higher-order predicates
    % are always monomorphic. Similarly for ExistQVars.
    empty_hlds_constraints(EmptyConstraints),
    ExistQVars = [],
    typecheck_var_has_polymorphic_type_list([PredVar | Args], TypeVarSet,
        ExistQVars, [PredVarType | ArgTypes], EmptyConstraints, !Info, !IO).

:- pred higher_order_pred_type(purity::in, int::in, lambda_eval_method::in,
    tvarset::out, mer_type::out, list(mer_type)::out) is det.

    % higher_order_pred_type(Purity, N, EvalMethod,
    %   TypeVarSet, PredType, ArgTypes):
    %
    % Given an arity N, let TypeVarSet = {T1, T2, ..., TN},
    % PredType = `Purity EvalMethod pred(T1, T2, ..., TN)', and
    % ArgTypes = [T1, T2, ..., TN].
    %
higher_order_pred_type(Purity, Arity, EvalMethod, TypeVarSet, PredType,
        ArgTypes) :-
    varset.init(TypeVarSet0),
    varset.new_vars(TypeVarSet0, Arity, ArgTypeVars, TypeVarSet),
    % Argument types always have kind `star'.
    prog_type.var_list_to_type_list(map.init, ArgTypeVars, ArgTypes),
    construct_higher_order_type(Purity, predicate, EvalMethod, ArgTypes,
        PredType).

:- pred higher_order_func_type(purity::in, int::in, lambda_eval_method::in,
    tvarset::out, mer_type::out, list(mer_type)::out, mer_type::out) is det.

    % higher_order_func_type(Purity, N, EvalMethod, TypeVarSet,
    %   FuncType, ArgTypes, RetType):
    %
    % Given an arity N, let TypeVarSet = {T0, T1, T2, ..., TN},
    % FuncType = `Purity EvalMethod func(T1, T2, ..., TN) = T0',
    % ArgTypes = [T1, T2, ..., TN], and
    % RetType = T0.
    %
higher_order_func_type(Purity, Arity, EvalMethod, TypeVarSet,
        FuncType, ArgTypes, RetType) :-
    varset.init(TypeVarSet0),
    varset.new_vars(TypeVarSet0, Arity, ArgTypeVars, TypeVarSet1),
    varset.new_var(TypeVarSet1, RetTypeVar, TypeVarSet),
    % Argument and return types always have kind `star'.
    prog_type.var_list_to_type_list(map.init, ArgTypeVars, ArgTypes),
    RetType = type_variable(RetTypeVar, kind_star),
    construct_higher_order_func_type(Purity, EvalMethod, ArgTypes, RetType,
        FuncType).

%-----------------------------------------------------------------------------%

:- pred typecheck_event_call(string::in, list(prog_var)::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_event_call(EventName, Args, !Info, !IO) :-
    ( event_arg_types(EventName, EventArgTypes) ->
        typecheck_var_has_type_list(Args, EventArgTypes, 1, !Info, !IO)
    ;
        report_unknown_event_call_error(EventName, !Info, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred typecheck_call_pred(simple_call_id::in, list(prog_var)::in,
    goal_path::in, pred_id::out, typecheck_info::in, typecheck_info::out,
    io::di, io::uo) is det.

typecheck_call_pred(CallId, Args, GoalPath, PredId, !Info, !IO) :-
    typecheck_info_get_type_assign_set(!.Info, OrigTypeAssignSet),

    % Look up the called predicate's arg types.
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    (
        CallId = simple_call_id(PorF, SymName, Arity),
        predicate_table_search_pf_sym_arity(PredicateTable,
            calls_are_fully_qualified(!.Info ^ pred_markers),
            PorF, SymName, Arity, PredIdList)
    ->
        % Handle the case of a non-overloaded predicate specially
        % (so that we can optimize the case of a non-overloaded,
        % non-polymorphic predicate).
        ( PredIdList = [PredId0] ->
            PredId = PredId0,
            typecheck_call_pred_id(PredId, Args, GoalPath, !Info, !IO)
        ;
            typecheck_call_overloaded_pred(CallId, PredIdList, Args,
                GoalPath, !Info, !IO),

            % In general, we can't figure out which predicate it is until
            % after we have resolved any overloading, which may require
            % type-checking the entire clause. Hence, for the moment, we just
            % record an invalid pred_id in the HLDS. This will be rectified
            % by modes.m during mode-checking; at that point, enough
            % information is available to determine which predicate it is.
            PredId = invalid_pred_id
        ),

        % Arguably, we could do context reduction at a different point.
        % See the paper: "Type classes: an exploration of the design space",
        % S. Peyton-Jones, M. Jones 1997, for a discussion of some of the
        % issues.
        perform_context_reduction(OrigTypeAssignSet, !Info, !IO)
    ;
        PredId = invalid_pred_id,
        report_pred_call_error(CallId, !Info, !IO)
    ).

    % Typecheck a call to a specific predicate.
    %
:- pred typecheck_call_pred_id(pred_id::in, list(prog_var)::in, goal_path::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_call_pred_id(PredId, Args, GoalPath, !Info, !IO) :-
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        PredArgTypes),
    pred_info_get_class_context(PredInfo, PredClassContext),

    % Rename apart the type variables in the called predicate's arg types
    % and then unify the types of the call arguments with the called
    % predicates' arg types (optimize for the common case of a non-polymorphic,
    % non-constrained predicate).
    (
        varset.is_empty(PredTypeVarSet),
        PredClassContext = constraints([], [])
    ->
        typecheck_var_has_type_list(Args, PredArgTypes, 1, !Info, !IO)
    ;
        module_info_get_class_table(ModuleInfo, ClassTable),
        make_body_hlds_constraints(ClassTable, PredTypeVarSet,
            GoalPath, PredClassContext, PredConstraints),
        typecheck_var_has_polymorphic_type_list(Args, PredTypeVarSet,
            PredExistQVars, PredArgTypes, PredConstraints, !Info, !IO)
    ).

:- pred typecheck_call_overloaded_pred(simple_call_id::in, list(pred_id)::in,
    list(prog_var)::in, goal_path::in, typecheck_info::in, typecheck_info::out,
    io::di, io::uo) is det.

typecheck_call_overloaded_pred(CallId, PredIdList, Args, GoalPath,
        !Info, !IO) :-
    typecheck_info_get_context(!.Info, Context),
    Symbol = overloaded_pred(CallId, PredIdList),
    typecheck_info_add_overloaded_symbol(Symbol, Context, !Info),

    % Let the new arg_type_assign_set be the cross-product of the current
    % type_assign_set and the set of possible lists of argument types
    % for the overloaded predicate, suitable renamed apart.
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
    get_overloaded_pred_arg_types(PredIdList, Preds, ClassTable, GoalPath,
        TypeAssignSet0, [], ArgsTypeAssignSet),

    % Then unify the types of the call arguments with the
    % called predicates' arg types.
    typecheck_var_has_arg_type_list(Args, 1, ArgsTypeAssignSet, !Info, !IO).

:- pred get_overloaded_pred_arg_types(list(pred_id)::in, pred_table::in,
    class_table::in, goal_path::in, type_assign_set::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

get_overloaded_pred_arg_types([], _Preds, _ClassTable, _GoalPath,
        _TypeAssignSet0, !ArgsTypeAssignSet).
get_overloaded_pred_arg_types([PredId | PredIds], Preds, ClassTable, GoalPath,
        TypeAssignSet0, !ArgsTypeAssignSet) :-
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        PredArgTypes),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_typevarset(PredInfo, TVarSet),
    make_body_hlds_constraints(ClassTable, TVarSet, GoalPath,
        PredClassContext, PredConstraints),
    rename_apart(TypeAssignSet0, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, !ArgsTypeAssignSet),
    get_overloaded_pred_arg_types(PredIds, Preds, ClassTable, GoalPath,
        TypeAssignSet0, !ArgsTypeAssignSet).

%-----------------------------------------------------------------------------%

    % Rename apart the type variables in called predicate's arg types
    % separately for each type assignment, resulting in an "arg type
    % assignment set", and then for each arg type assignment in the
    % arg type assignment set, check that the argument variables have
    % the expected types.
    % A set of class constraints are also passed in, which must have the
    % types contained within renamed apart.
    %
:- pred typecheck_var_has_polymorphic_type_list(list(prog_var)::in,
    tvarset::in, existq_tvars::in, list(mer_type)::in, hlds_constraints::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_polymorphic_type_list(Args, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, !Info, !IO) :-
    typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
    rename_apart(TypeAssignSet0, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, [], ArgsTypeAssignSet),
    typecheck_var_has_arg_type_list(Args, 1, ArgsTypeAssignSet, !Info, !IO).

:- pred rename_apart(type_assign_set::in, tvarset::in, existq_tvars::in,
    list(mer_type)::in, hlds_constraints::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

rename_apart([], _, _, _, _, !ArgTypeAssigns).
rename_apart([TypeAssign0 | TypeAssigns0], PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, !ArgTypeAssigns) :-
    % Rename everything apart.
    type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes,
        TypeAssign1, ParentArgTypes, Renaming),
    apply_variable_renaming_to_tvar_list(Renaming, PredExistQVars,
        ParentExistQVars),
    apply_variable_renaming_to_constraints(Renaming, PredConstraints,
        ParentConstraints),

    % Insert the existentially quantified type variables for the called
    % predicate into HeadTypeParams (which holds the set of type
    % variables which the caller is not allowed to bind).
    type_assign_get_head_type_params(TypeAssign1, HeadTypeParams0),
    list.append(ParentExistQVars, HeadTypeParams0, HeadTypeParams),
    type_assign_set_head_type_params(HeadTypeParams, TypeAssign1, TypeAssign),

    % Save the results and recurse.
    NewArgTypeAssign = args(TypeAssign, ParentArgTypes, ParentConstraints),
    !:ArgTypeAssigns = [NewArgTypeAssign | !.ArgTypeAssigns],
    rename_apart(TypeAssigns0, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, !ArgTypeAssigns).

:- pred type_assign_rename_apart(type_assign::in, tvarset::in,
    list(mer_type)::in, type_assign::out, list(mer_type)::out,
    tvar_renaming::out) is det.

type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes,
        TypeAssign, ParentArgTypes, Renaming) :-
    type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
    tvarset_merge_renaming(TypeVarSet0, PredTypeVarSet, TypeVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, PredArgTypes,
        ParentArgTypes),
    type_assign_set_typevarset(TypeVarSet, TypeAssign0, TypeAssign).

%-----------------------------------------------------------------------------%

    % Given a list of variables and a list of types, ensure that each variable
    % has the corresponding type.
    %
:- pred typecheck_var_has_arg_type_list(list(prog_var)::in, int::in,
    args_type_assign_set::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_arg_type_list([], _, ArgTypeAssignSet, !Info, !IO) :-
    TypeAssignSet =
        convert_args_type_assign_set_check_empty_args(ArgTypeAssignSet),
    typecheck_info_set_type_assign_set(TypeAssignSet, !Info).

typecheck_var_has_arg_type_list([Var | Vars], ArgNum, ArgTypeAssignSet0, !Info,
        !IO) :-
    typecheck_info_set_arg_num(ArgNum, !Info),
    typecheck_var_has_arg_type(Var, ArgTypeAssignSet0, ArgTypeAssignSet1,
        !Info, !IO),
    typecheck_var_has_arg_type_list(Vars, ArgNum + 1, ArgTypeAssignSet1,
        !Info, !IO).

:- pred typecheck_var_has_arg_type(prog_var::in,
    args_type_assign_set::in, args_type_assign_set::out,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_arg_type(Var, ArgTypeAssignSet0, ArgTypeAssignSet, !Info,
        !IO) :-
    typecheck_var_has_arg_type_2(ArgTypeAssignSet0,
        Var, [], ArgTypeAssignSet1),
    (
        ArgTypeAssignSet1 = [],
        ArgTypeAssignSet0 = [_ | _]
    ->
        skip_arg(ArgTypeAssignSet0, ArgTypeAssignSet),
        report_error_arg_var(!.Info, Var, ArgTypeAssignSet0, !IO),
        typecheck_info_set_found_error(yes, !Info)
    ;
        ArgTypeAssignSet = ArgTypeAssignSet1
    ).

:- pred skip_arg(args_type_assign_set::in, args_type_assign_set::out) is det.

skip_arg([], []).
skip_arg([ArgTypeAssign0 | ArgTypeAssigns0],
        [ArgTypeAssign | ArgTypeAssigns]) :-
    ArgTypeAssign0 = args(TypeAssign, Args0, Constraints),
    ( Args0 = [_ | Args1] ->
        Args = Args1
    ;
        % this should never happen
        unexpected(this_file, "skip_arg")
    ),
    ArgTypeAssign = args(TypeAssign, Args, Constraints),
    skip_arg(ArgTypeAssigns0, ArgTypeAssigns).

:- pred typecheck_var_has_arg_type_2(args_type_assign_set::in, prog_var::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_has_arg_type_2([], _, !ArgTypeAssignSet).
typecheck_var_has_arg_type_2([ArgsTypeAssign | ArgsTypeAssignSets], Var,
        !ArgsTypeAssignSet) :-
    ArgsTypeAssign = args(TypeAssign0, ArgTypes0, ClassContext),
    arg_type_assign_var_has_type(TypeAssign0, ArgTypes0,
        Var, ClassContext, !ArgsTypeAssignSet),
    typecheck_var_has_arg_type_2(ArgsTypeAssignSets, Var,
        !ArgsTypeAssignSet).

:- pred arg_type_assign_var_has_type(type_assign::in, list(mer_type)::in,
    prog_var::in, hlds_constraints::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

arg_type_assign_var_has_type(TypeAssign0, ArgTypes0, Var, ClassContext,
        !ArgTypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    (
        ArgTypes0 = [Type | ArgTypes],
        ( map.search(VarTypes0, Var, VarType) ->
            (
                type_assign_unify_type(TypeAssign0, VarType, Type, TypeAssign1)
            ->
                NewTypeAssign = args(TypeAssign1, ArgTypes, ClassContext),
                !:ArgTypeAssignSet = [NewTypeAssign | !.ArgTypeAssignSet]
            ;
                true
            )
        ;
            map.det_insert(VarTypes0, Var, Type, VarTypes),
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            NewTypeAssign = args(TypeAssign, ArgTypes, ClassContext),
            !:ArgTypeAssignSet = [NewTypeAssign | !.ArgTypeAssignSet]
        )
    ;
        ArgTypes0 = [],
        unexpected(this_file, "arg_type_assign_var_has_type")
    ).

%-----------------------------------------------------------------------------%

    % Given a list of variables and a list of types, ensure
    % that each variable has the corresponding type.
    %
:- pred typecheck_var_has_type_list(list(prog_var)::in, list(mer_type)::in,
    int::in, typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_type_list([], [_ | _], _, !Info, !IO) :-
    unexpected(this_file, "typecheck_var_has_type_list: length mismatch").
typecheck_var_has_type_list([_ | _], [], _, !Info, !IO) :-
    unexpected(this_file, "typecheck_var_has_type_list: length mismatch").
typecheck_var_has_type_list([], [], _, !Info, !IO).
typecheck_var_has_type_list([Var | Vars], [Type | Types], ArgNum, !Info,
        !IO) :-
    typecheck_info_set_arg_num(ArgNum, !Info),
    typecheck_var_has_type(Var, Type, !Info, !IO),
    typecheck_var_has_type_list(Vars, Types, ArgNum + 1, !Info, !IO).

:- pred typecheck_var_has_type(prog_var::in, mer_type::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_type(Var, Type, !Info, !IO) :-
    typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
    typecheck_var_has_type_2(TypeAssignSet0, Var, Type, [],
        TypeAssignSet),
    (
        TypeAssignSet = [],
        TypeAssignSet0 = [_ | _]
    ->
        report_error_var(!.Info, Var, Type, TypeAssignSet0, !IO),
        typecheck_info_set_found_error(yes, !Info)
    ;
        typecheck_info_set_type_assign_set(TypeAssignSet, !Info)
    ).

:- pred typecheck_var_has_type_2(type_assign_set::in, prog_var::in,
    mer_type::in, type_assign_set::in, type_assign_set::out) is det.

typecheck_var_has_type_2([], _, _, !TypeAssignSet).
typecheck_var_has_type_2([TypeAssign0 | TypeAssignSet0], Var, Type,
        !TypeAssignSet) :-
    type_assign_var_has_type(TypeAssign0, Var, Type, !TypeAssignSet),
    typecheck_var_has_type_2(TypeAssignSet0, Var, Type, !TypeAssignSet).

:- pred type_assign_var_has_type(type_assign::in, prog_var::in, mer_type::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_var_has_type(TypeAssign0, Var, Type, !TypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    ( map.search(VarTypes0, Var, VarType) ->
        ( type_assign_unify_type(TypeAssign0, VarType, Type, TypeAssign1) ->
            !:TypeAssignSet = [TypeAssign1 | !.TypeAssignSet]
        ;
            !:TypeAssignSet = !.TypeAssignSet
        )
    ;
        map.det_insert(VarTypes0, Var, Type, VarTypes),
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    ).

%-----------------------------------------------------------------------------%

    % type_assign_var_has_type_list(Vars, Types, TypeAssign, Info,
    %       TypeAssignSet0, TypeAssignSet):
    % Let TAs = { TA | TA is an extension of TypeAssign
    %       for which the types of the Vars unify with
    %       their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assign_var_has_type_list(list(prog_var)::in, list(mer_type)::in,
    type_assign::in, typecheck_info::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_var_has_type_list([], [_ | _], _, _, _, _) :-
    unexpected(this_file, "type_assign_var_has_type_list: length mis-match").
type_assign_var_has_type_list([_ | _], [], _, _, _, _) :-
    unexpected(this_file, "type_assign_var_has_type_list: length mis-match").
type_assign_var_has_type_list([], [], TypeAssign, _,
        TypeAssignSet, [TypeAssign | TypeAssignSet]).
type_assign_var_has_type_list([Arg | Args], [Type | Types], TypeAssign0,
        Info, TypeAssignSet0, TypeAssignSet) :-
    type_assign_var_has_type(TypeAssign0, Arg, Type, [], TypeAssignSet1),
    type_assign_list_var_has_type_list(TypeAssignSet1,
        Args, Types, Info, TypeAssignSet0, TypeAssignSet).

    % type_assign_list_var_has_type_list(TAs, Terms, Types,
    %       Info, TypeAssignSet0, TypeAssignSet):
    % Let TAs2 = { TA | TA is an extension of a member of TAs
    %       for which the types of the Terms unify with
    %       their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assign_list_var_has_type_list(type_assign_set::in,
    list(prog_var)::in, list(mer_type)::in, typecheck_info::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_list_var_has_type_list([], _, _, _, !TypeAssignSet).
type_assign_list_var_has_type_list([TA | TAs], Args, Types, Info,
        !TypeAssignSet) :-
    type_assign_var_has_type_list(Args, Types, TA, Info, !TypeAssignSet),
    type_assign_list_var_has_type_list(TAs, Args, Types, Info, !TypeAssignSet).

%-----------------------------------------------------------------------------%

    % Because we allow overloading, type-checking is NP-complete.
    % Rather than disallow it completely, we issue a warning
    % whenever "too much" overloading is used.  "Too much"
    % is arbitrarily defined as anything which results in
    % more than 50 possible type assignments.
    %
:- pred check_warn_too_much_overloading(typecheck_info::in,
    typecheck_info::out, io::di, io::uo) is det.

check_warn_too_much_overloading(!Info, !IO) :-
    (
        typecheck_info_get_warned_about_overloading(!.Info, AlreadyWarned),
        AlreadyWarned = no,
        typecheck_info_get_type_assign_set(!.Info, TypeAssignSet),
        list.length(TypeAssignSet, Count),
        Count > 50
    ->
        report_warning_too_much_overloading(!.Info, !IO),
        typecheck_info_set_warned_about_overloading(yes, !Info)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % Type check a unification.
    % Get the type assignment set from the type info and then just
    % iterate over all the possible type assignments.
    %
:- pred typecheck_unification(prog_var::in, unify_rhs::in, unify_rhs::out,
    goal_path::in, typecheck_info::in, typecheck_info::out,
    io::di, io::uo) is det.

typecheck_unification(X, rhs_var(Y), rhs_var(Y), _, !Info, !IO) :-
    typecheck_unify_var_var(X, Y, !Info, !IO).
typecheck_unification(X, rhs_functor(Functor, ExistConstraints, Args),
        rhs_functor(Functor, ExistConstraints, Args), GoalPath, !Info, !IO) :-
    typecheck_info_get_type_assign_set(!.Info, OrigTypeAssignSet),
    typecheck_unify_var_functor(X, Functor, Args, GoalPath, !Info, !IO),
    perform_context_reduction(OrigTypeAssignSet, !Info, !IO).
typecheck_unification(X,
        rhs_lambda_goal(Purity, PredOrFunc, EvalMethod,
            NonLocals, Vars, Modes, Det, Goal0),
        rhs_lambda_goal(Purity, PredOrFunc, EvalMethod,
            NonLocals, Vars, Modes, Det, Goal), _, !Info, !IO) :-
    typecheck_lambda_var_has_type(Purity, PredOrFunc, EvalMethod, X, Vars,
        !Info, !IO),
    typecheck_goal(Goal0, Goal, !Info, !IO).

:- pred typecheck_unify_var_var(prog_var::in, prog_var::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_unify_var_var(X, Y, !Info, !IO) :-
    typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
    typecheck_unify_var_var_2(TypeAssignSet0, X, Y, [], TypeAssignSet),
    (
        TypeAssignSet = [],
        TypeAssignSet0 = [_ | _]
    ->
        report_error_unif_var_var(!.Info, X, Y, TypeAssignSet0, !IO),
        typecheck_info_set_found_error(yes, !Info)
    ;
        typecheck_info_set_type_assign_set(TypeAssignSet, !Info)
    ).

:- pred typecheck_unify_var_functor(prog_var::in, cons_id::in,
    list(prog_var)::in, goal_path::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_unify_var_functor(Var, Functor, Args, GoalPath, !Info, !IO) :-
    % Get the list of possible constructors that match this functor/arity.
    % If there aren't any, report an undefined constructor error.
    list.length(Args, Arity),
    typecheck_info_get_ctor_list(!.Info, Functor, Arity, GoalPath,
        ConsDefnList, InvalidConsDefnList),
    (
        ConsDefnList = [],
        report_error_undef_cons(!.Info, InvalidConsDefnList, Functor, Arity,
            !IO),
        typecheck_info_set_found_error(yes, !Info)
    ;
        (
            ConsDefnList = [_]
        ;
            ConsDefnList = [_, _ | _],
            typecheck_info_get_context(!.Info, Context),
            Sources = list.map(project_cons_type_info_source, ConsDefnList),
            Symbol = overloaded_func(Functor, Sources),
            typecheck_info_add_overloaded_symbol(Symbol, Context, !Info)
        ),

        % Produce the ConsTypeAssignSet, which is essentially the
        % cross-product of the TypeAssignSet0 and the ConsDefnList.
        typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
        typecheck_unify_var_functor_get_ctors(TypeAssignSet0,
            !.Info, ConsDefnList, [], ConsTypeAssignSet),
        (
            ConsTypeAssignSet = [],
            TypeAssignSet0 = [_ | _]
        ->
            % This should never happen, since undefined ctors
            % should be caught by the check just above.
            unexpected(this_file,
                "typecheck_unify_var_functor: undefined cons?")
        ;
            true
        ),

        % Check that the type of the functor matches the type of the variable.
        typecheck_functor_type(ConsTypeAssignSet, Var, [], ArgsTypeAssignSet),
        (
            ArgsTypeAssignSet = [],
            ConsTypeAssignSet = [_ | _]
        ->
            report_error_functor_type(!.Info, Var, ConsDefnList,
                Functor, Arity, TypeAssignSet0, !IO),
            typecheck_info_set_found_error(yes, !Info)
        ;
            true
        ),

        % Check that the type of the arguments of the functor matches
        % their expected type for this functor.
        typecheck_functor_arg_types(ArgsTypeAssignSet, Args, !.Info,
            [], TypeAssignSet),
        (
            TypeAssignSet = [],
            ArgsTypeAssignSet = [_ | _]
        ->
            report_error_functor_arg_types(!.Info, Var, ConsDefnList,
                Functor, Args, ArgsTypeAssignSet, !IO),
            typecheck_info_set_found_error(yes, !Info)
        ;
            true
        ),

        % If we encountered an error, continue checking with the
        % original type assign set.
        (
            TypeAssignSet = [],
            typecheck_info_set_type_assign_set(TypeAssignSet0, !Info)
        ;
            TypeAssignSet = [_ | _],
            typecheck_info_set_type_assign_set(TypeAssignSet, !Info)
        )
    ).

:- type cons_type
    --->    cons_type(mer_type, list(mer_type)).

:- type cons_type_assign_set == list(pair(type_assign, cons_type)).

    % typecheck_unify_var_functor_get_ctors(TypeAssignSet, Info, ConsDefns):
    %
    % Iterate over all the different possible type assignments and
    % constructor definitions.
    % For each type assignment in `TypeAssignSet', and constructor
    % definition in `ConsDefns', produce a pair
    %
    %   TypeAssign - cons_type(Type, ArgTypes)
    %
    % where `cons_type(Type, ArgTypes)' records one of the possible types
    % for the constructor in `ConsDefns', and where `TypeAssign' is the type
    % assignment renamed apart from the types of the constructors.
    %
:- pred typecheck_unify_var_functor_get_ctors(type_assign_set::in,
    typecheck_info::in, list(cons_type_info)::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

    % Iterate over the type assign sets.
    %
typecheck_unify_var_functor_get_ctors([], _, _, !TypeAssignSet).
typecheck_unify_var_functor_get_ctors([TypeAssign | TypeAssigns], Info,
        ConsDefns, !TypeAssignSet) :-
    typecheck_unify_var_functor_get_ctors_2(ConsDefns, Info, TypeAssign,
        !TypeAssignSet),
    typecheck_unify_var_functor_get_ctors(TypeAssigns, Info, ConsDefns,
        !TypeAssignSet).

    % Iterate over all the different cons defns.
    %
:- pred typecheck_unify_var_functor_get_ctors_2(list(cons_type_info)::in,
    typecheck_info::in, type_assign::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

typecheck_unify_var_functor_get_ctors_2([], _, _, !ConsTypeAssignSet).
typecheck_unify_var_functor_get_ctors_2([ConsDefn | ConsDefns], Info,
        TypeAssign0, !ConsTypeAssignSet) :-
    get_cons_stuff(ConsDefn, TypeAssign0, Info, ConsType, ArgTypes,
        TypeAssign1),
    list.append([TypeAssign1 - cons_type(ConsType, ArgTypes)],
        !ConsTypeAssignSet),
    typecheck_unify_var_functor_get_ctors_2(ConsDefns, Info, TypeAssign0,
        !ConsTypeAssignSet).

    % typecheck_functor_type(ConsTypeAssignSet, Var):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor type,
    % check that the type of `Var' matches this type.
    %
:- pred typecheck_functor_type(cons_type_assign_set::in, prog_var::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_functor_type([], _, !ArgsTypeAssignSet).
typecheck_functor_type([TypeAssign - ConsType | ConsTypeAssigns], Var,
        !ArgsTypeAssignSet) :-
    ConsType = cons_type(Type, ArgTypes),
    type_assign_check_functor_type(Type, ArgTypes, Var, TypeAssign,
        !ArgsTypeAssignSet),
    typecheck_functor_type(ConsTypeAssigns, Var, !ArgsTypeAssignSet).

    % typecheck_functor_arg_types(ConsTypeAssignSet, Var, Args, ...):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor argument types,
    % check that the types of `Args' matches these types.
    %
:- pred typecheck_functor_arg_types(args_type_assign_set::in,
    list(prog_var)::in, typecheck_info::in,
    type_assign_set::in, type_assign_set::out) is det.

typecheck_functor_arg_types([], _, _, !TypeAssignSet).
typecheck_functor_arg_types([ConsTypeAssign | ConsTypeAssigns], Args, Info,
        !TypeAssignSet) :-
    ConsTypeAssign = args(TypeAssign, ArgTypes, _),
    type_assign_var_has_type_list(Args, ArgTypes, TypeAssign, Info,
        !TypeAssignSet),
    typecheck_functor_arg_types(ConsTypeAssigns, Args, Info, !TypeAssignSet).

    % Iterate over all the possible type assignments.
    %
:- pred typecheck_unify_var_var_2(type_assign_set::in,
    prog_var::in, prog_var::in,
    type_assign_set::in, type_assign_set::out) is det.

typecheck_unify_var_var_2([], _, _, !TypeAssignSet).
typecheck_unify_var_var_2([TypeAssign0 | TypeAssigns0], X, Y,
        !TypeAssignSet) :-
    type_assign_unify_var_var(X, Y, TypeAssign0, !TypeAssignSet),
    typecheck_unify_var_var_2(TypeAssigns0, X, Y, !TypeAssignSet).

%-----------------------------------------------------------------------------%

    % Type-check the unification of two variables,
    % and update the type assignment.
    % TypeAssign0 is the type assignment we are updating,
    % TypeAssignSet0 is an accumulator for the list of possible
    % type assignments so far, and TypeAssignSet is TypeAssignSet plus
    % any type assignment(s) resulting from TypeAssign0 and this
    % unification.
    %
:- pred type_assign_unify_var_var(prog_var::in, prog_var::in, type_assign::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_unify_var_var(X, Y, TypeAssign0, !TypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    ( map.search(VarTypes0, X, TypeX) ->
        ( map.search(VarTypes0, Y, TypeY) ->
            % Both X and Y already have types - just unify their types.
            ( type_assign_unify_type(TypeAssign0, TypeX, TypeY, TypeAssign3) ->
                !:TypeAssignSet = [TypeAssign3 | !.TypeAssignSet]
            ;
                !:TypeAssignSet = !.TypeAssignSet
            )
        ;
            % Y is a fresh variable which hasn't been assigned a type yet.
            map.det_insert(VarTypes0, Y, TypeX, VarTypes),
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        )
    ;
        ( map.search(VarTypes0, Y, TypeY) ->
            % X is a fresh variable which hasn't been assigned a type yet.
            map.det_insert(VarTypes0, X, TypeY, VarTypes),
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        ;
            % Both X and Y are fresh variables - introduce a fresh type
            % variable with kind `star' to represent their type.
            type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
            varset.new_var(TypeVarSet0, TypeVar, TypeVarSet),
            type_assign_set_typevarset(TypeVarSet, TypeAssign0, TypeAssign1),
            Type = type_variable(TypeVar, kind_star),
            map.det_insert(VarTypes0, X, Type, VarTypes1),
            ( X \= Y ->
                map.det_insert(VarTypes1, Y, Type, VarTypes)
            ;
                VarTypes = VarTypes1
            ),
            type_assign_set_var_types(VarTypes, TypeAssign1, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        )
    ).

%-----------------------------------------------------------------------------%

:- pred type_assign_check_functor_type(mer_type::in, list(mer_type)::in,
    prog_var::in, type_assign::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

type_assign_check_functor_type(ConsType, ArgTypes, Y, TypeAssign1,
        !TypeAssignSet) :-
    % Unify the type of Var with the type of the constructor.
    type_assign_get_var_types(TypeAssign1, VarTypes0),
    ( map.search(VarTypes0, Y, TypeY) ->
        ( type_assign_unify_type(TypeAssign1, ConsType, TypeY, TypeAssign2) ->
            % The constraints are empty here because none are added by
            % unification with a functor.
            empty_hlds_constraints(EmptyConstraints),
            ArgsTypeAssign = args(TypeAssign2, ArgTypes, EmptyConstraints),
            !:TypeAssignSet = [ArgsTypeAssign | !.TypeAssignSet]
        ;
            true
        )
    ;
        % The constraints are empty here because none are added by
        % unification with a functor.
        map.det_insert(VarTypes0, Y, ConsType, VarTypes),
        type_assign_set_var_types(VarTypes, TypeAssign1, TypeAssign3),
        empty_hlds_constraints(EmptyConstraints),
        ArgsTypeAssign = args(TypeAssign3, ArgTypes, EmptyConstraints),
        !:TypeAssignSet = [ArgsTypeAssign | !.TypeAssignSet]
    ).

%-----------------------------------------------------------------------------%

    % Given an cons_type_info, construct a type for the constructor
    % and a list of types of the arguments, suitable renamed apart
    % from the current type_assign's typevarset.
    %
:- pred get_cons_stuff(cons_type_info::in, type_assign::in, typecheck_info::in,
    mer_type::out, list(mer_type)::out, type_assign::out) is det.

get_cons_stuff(ConsDefn, TypeAssign0, _Info, ConsType, ArgTypes, TypeAssign) :-
    ConsDefn = cons_type_info(ConsTypeVarSet, ConsExistQVars0,
        ConsType0, ArgTypes0, ClassConstraints0, _Source),

    % Rename apart the type vars in the type of the constructor
    % and the types of its arguments.
    % (Optimize the common case of a non-polymorphic type)
    ( varset.is_empty(ConsTypeVarSet) ->
        ConsType = ConsType0,
        ArgTypes = ArgTypes0,
        TypeAssign2 = TypeAssign0,
        ConstraintsToAdd = ClassConstraints0
    ;
        type_assign_rename_apart(TypeAssign0, ConsTypeVarSet,
            [ConsType0 | ArgTypes0], TypeAssign1, [ConsType1 | ArgTypes1],
            Renaming)
    ->
        apply_variable_renaming_to_tvar_list(Renaming,
            ConsExistQVars0, ConsExistQVars),
        apply_variable_renaming_to_constraints(Renaming,
            ClassConstraints0, ConstraintsToAdd),
        type_assign_get_head_type_params(TypeAssign1, HeadTypeParams0),
        HeadTypeParams = ConsExistQVars ++ HeadTypeParams0,
        type_assign_set_head_type_params(HeadTypeParams,
            TypeAssign1, TypeAssign2),

        ConsType = ConsType1,
        ArgTypes = ArgTypes1
    ;
        unexpected(this_file,
            "get_cons_stuff: type_assign_rename_apart failed")
    ),

    % Add the constraints for this functor to the current constraint set.
    % Note that there can still be (ground) constraints even if the varset
    % is empty.
    %
    % For functors which are data constructors, the fact that we don't take
    % the dual corresponds to assuming that they will be used as deconstructors
    % rather than as constructors.

    type_assign_get_typeclass_constraints(TypeAssign2, OldConstraints),
    merge_hlds_constraints(ConstraintsToAdd, OldConstraints, ClassConstraints),
    type_assign_set_typeclass_constraints(ClassConstraints, TypeAssign2,
        TypeAssign).

:- pred apply_substitution_to_var_list(list(var(T))::in, map(var(T),
    term(T))::in, list(var(T))::out) is det.

apply_substitution_to_var_list(Vars0, RenameSubst, Vars) :-
    term.var_list_to_term_list(Vars0, Terms0),
    term.apply_substitution_to_list(Terms0, RenameSubst, Terms),
    term.term_list_to_var_list(Terms, Vars).

:- pred apply_var_renaming_to_var_list(list(var(T))::in, map(var(T),
    var(T))::in, list(var(T))::out) is det.

apply_var_renaming_to_var_list(Vars0, RenameSubst, Vars) :-
    list.map(apply_var_renaming_to_var(RenameSubst), Vars0, Vars).

:- pred apply_var_renaming_to_var(map(var(T), var(T))::in, var(T)::in,
    var(T)::out) is det.

apply_var_renaming_to_var(RenameSubst, Var0, Var) :-
    ( map.search(RenameSubst, Var0, Var1) ->
        Var = Var1
    ;
        Var = Var0
    ).

%-----------------------------------------------------------------------------%

    % typecheck_lambda_var_has_type(Var, ArgVars, ...):
    %
    % Check that `Var' has type `pred(T1, T2, ...)' where T1, T2, ...
    % are the types of the `ArgVars'.
    %
:- pred typecheck_lambda_var_has_type(purity::in, pred_or_func::in,
    lambda_eval_method::in, prog_var::in, list(prog_var)::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_lambda_var_has_type(Purity, PredOrFunc, EvalMethod, Var, ArgVars,
        !Info, !IO) :-
    typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
    typecheck_lambda_var_has_type_2(TypeAssignSet0, Purity, PredOrFunc,
        EvalMethod, Var, ArgVars, [], TypeAssignSet),
    (
        TypeAssignSet = [],
        TypeAssignSet0 = [_ | _]
    ->
        report_error_lambda_var(!.Info, PredOrFunc, EvalMethod,
            Var, ArgVars, TypeAssignSet0, !IO),
        typecheck_info_set_found_error(yes, !Info)
    ;
        typecheck_info_set_type_assign_set(TypeAssignSet, !Info)
    ).

:- pred typecheck_lambda_var_has_type_2(type_assign_set::in, purity::in,
    pred_or_func::in, lambda_eval_method::in, prog_var::in,
    list(prog_var)::in, type_assign_set::in, type_assign_set::out) is det.

typecheck_lambda_var_has_type_2([], _, _, _, _, _, !TypeAssignSet).
typecheck_lambda_var_has_type_2([TypeAssign0 | TypeAssignSet0], Purity,
        PredOrFunc, EvalMethod, Var, ArgVars, !TypeAssignSet) :-
    type_assign_get_types_of_vars(ArgVars, ArgVarTypes,
        TypeAssign0, TypeAssign1),
    construct_higher_order_type(Purity, PredOrFunc, EvalMethod,
        ArgVarTypes, LambdaType),
    type_assign_var_has_type(TypeAssign1, Var, LambdaType, !TypeAssignSet),
    typecheck_lambda_var_has_type_2(TypeAssignSet0,
        Purity, PredOrFunc, EvalMethod, Var, ArgVars, !TypeAssignSet).

:- pred type_assign_get_types_of_vars(list(prog_var)::in, list(mer_type)::out,
    type_assign::in, type_assign::out) is det.

type_assign_get_types_of_vars([], [], !TypeAssign).
type_assign_get_types_of_vars([Var | Vars], [Type | Types], !TypeAssign) :-
    % Check whether the variable already has a type.
    type_assign_get_var_types(!.TypeAssign, VarTypes0),
    ( map.search(VarTypes0, Var, VarType) ->
        % If so, use that type.
        Type = VarType
    ;
        % Otherwise, introduce a fresh type variable with kind `star' to use
        % as the type of that variable.
        type_assign_get_typevarset(!.TypeAssign, TypeVarSet0),
        varset.new_var(TypeVarSet0, TypeVar, TypeVarSet),
        type_assign_set_typevarset(TypeVarSet, !TypeAssign),
        Type = type_variable(TypeVar, kind_star),
        map.det_insert(VarTypes0, Var, Type, VarTypes1),
        type_assign_set_var_types(VarTypes1, !TypeAssign)
    ),
    % Recursively process the rest of the variables.
    type_assign_get_types_of_vars(Vars, Types, !TypeAssign).

%-----------------------------------------------------------------------------%

    % Unify (with occurs check) two types in a type assignment
    % and update the type bindings.
    %
:- pred type_assign_unify_type(type_assign::in, mer_type::in, mer_type::in,
    type_assign::out) is semidet.

type_assign_unify_type(TypeAssign0, X, Y, TypeAssign) :-
    type_assign_get_head_type_params(TypeAssign0, HeadTypeParams),
    type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
    type_unify(X, Y, HeadTypeParams, TypeBindings0, TypeBindings),
    type_assign_set_type_bindings(TypeBindings, TypeAssign0, TypeAssign).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % builtin_atomic_type(Const, TypeName):
    %
    % If Const is a constant of a builtin atomic type, instantiates TypeName
    % to the name of that type, otherwise fails.
    %
:- pred builtin_atomic_type(cons_id::in, string::out) is semidet.

builtin_atomic_type(int_const(_), "int").
builtin_atomic_type(float_const(_), "float").
builtin_atomic_type(string_const(_), "string").
builtin_atomic_type(cons(unqualified(String), 0), "character") :-
    string.char_to_string(_, String).

    % builtin_pred_type(Info, Functor, Arity, GoalPath, PredConsInfoList):
    %
    % If Functor/Arity is a constant of a pred type, instantiates
    % the output parameters, otherwise fails.
    %
    % Instantiates PredConsInfoList to the set of cons_type_info structures
    % for each predicate with name `Functor' and arity greater than
    % or equal to Arity.  GoalPath is used to identify any constraints
    % introduced.
    %
    % For example, functor `map.search/1' has type `pred(K, V)'
    % (hence PredTypeParams = [K, V]) and argument types [map(K, V)].
    %
:- pred builtin_pred_type(typecheck_info::in, cons_id::in, int::in,
    goal_path::in, list(cons_type_info)::out) is semidet.

builtin_pred_type(Info, Functor, Arity, GoalPath, PredConsInfoList) :-
    Functor = cons(SymName, _),
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    (
        predicate_table_search_sym(PredicateTable,
            calls_are_fully_qualified(Info ^ pred_markers),
            SymName, PredIdList)
    ->
        predicate_table_get_preds(PredicateTable, Preds),
        make_pred_cons_info_list(Info, PredIdList, Preds, Arity,
            GoalPath, [], PredConsInfoList)
    ;
        PredConsInfoList = []
    ).

:- pred make_pred_cons_info_list(typecheck_info::in, list(pred_id)::in,
    pred_table::in, int::in, goal_path::in,
    list(cons_type_info)::in, list(cons_type_info)::out) is det.

make_pred_cons_info_list(_, [], _, _, _, !ConsTypeInfos).
make_pred_cons_info_list(Info, [PredId | PredIds], PredTable, Arity,
        GoalPath, !ConsTypeInfos) :-
    make_pred_cons_info(Info, PredId, PredTable, Arity,
        GoalPath, !ConsTypeInfos),
    make_pred_cons_info_list(Info, PredIds, PredTable, Arity,
        GoalPath, !ConsTypeInfos).

:- pred make_pred_cons_info(typecheck_info::in, pred_id::in, pred_table::in,
    int::in, goal_path::in,
    list(cons_type_info)::in, list(cons_type_info)::out) is det.

make_pred_cons_info(Info, PredId, PredTable, FuncArity, GoalPath,
        !ConsInfos) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(PredTable, PredId, PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    IsPredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        CompleteArgTypes),
    pred_info_get_purity(PredInfo, Purity),
    (
        IsPredOrFunc = predicate,
        PredArity >= FuncArity,
        % We don't support first-class polymorphism, so you can't take the
        % address of an existentially quantified predicate.
        PredExistQVars = []
    ->
        (
            list.split_list(FuncArity, CompleteArgTypes,
                ArgTypes, PredTypeParams)
        ->
            construct_higher_order_pred_type(Purity, lambda_normal,
                PredTypeParams, PredType),
            make_body_hlds_constraints(ClassTable, PredTypeVarSet,
                GoalPath, PredClassContext, PredConstraints),
            ConsInfo = cons_type_info(PredTypeVarSet, PredExistQVars,
                PredType, ArgTypes, PredConstraints, source_pred(PredId)),
            !:ConsInfos = [ConsInfo | !.ConsInfos]
        ;
            unexpected(this_file, "make_pred_cons_info: split_list failed")
        )
    ;
        IsPredOrFunc = function,
        PredAsFuncArity = PredArity - 1,
        PredAsFuncArity >= FuncArity,
        % We don't support first-class polymorphism, so you can't take
        % the address of an existentially quantified function. You can however
        % call such a function, so long as you pass *all* the parameters.
        ( PredExistQVars = []
        ; PredAsFuncArity = FuncArity
        )
    ->
        (
            list.split_list(FuncArity, CompleteArgTypes,
                FuncArgTypes, FuncTypeParams),
            pred_args_to_func_args(FuncTypeParams,
                FuncArgTypeParams, FuncReturnTypeParam)
        ->
            (
                FuncArgTypeParams = [],
                FuncType = FuncReturnTypeParam
            ;
                FuncArgTypeParams = [_ | _],
                construct_higher_order_func_type(Purity, lambda_normal,
                    FuncArgTypeParams, FuncReturnTypeParam, FuncType)
            ),
            make_body_hlds_constraints(ClassTable, PredTypeVarSet,
                GoalPath, PredClassContext, PredConstraints),
            ConsInfo = cons_type_info(PredTypeVarSet,
                PredExistQVars, FuncType, FuncArgTypes, PredConstraints,
                source_pred(PredId)),
            !:ConsInfos = [ConsInfo | !.ConsInfos]
        ;
            unexpected(this_file, "make_pred_cons_info: split_list failed")
        )
    ;
        true
    ).

    % builtin_apply_type(Info, Functor, Arity, ConsTypeInfos):
    %
    % Succeed if Functor is the builtin apply/N or ''/N (N>=2),
    % which is used to invoke higher-order functions.
    % If so, bind ConsTypeInfos to a singleton list containing
    % the appropriate type for apply/N of the specified Arity.
    %
:- pred builtin_apply_type(typecheck_info::in, cons_id::in, int::in,
    list(cons_type_info)::out) is semidet.

builtin_apply_type(_Info, Functor, Arity, ConsTypeInfos) :-
    Functor = cons(unqualified(ApplyName), _),
    % XXX FIXME handle impure apply/N more elegantly (e.g. nicer syntax)
    (
        ApplyName = "apply",
        ApplyNameToUse = ApplyName,
        Purity = purity_pure
    ;
        ApplyName = "",
        ApplyNameToUse = "apply",
        Purity = purity_pure
    ;
        ApplyName = "impure_apply",
        ApplyNameToUse = ApplyName,
        Purity = purity_impure
    ;
        ApplyName = "semipure_apply",
        ApplyNameToUse = ApplyName,
        Purity = purity_semipure
    ),
    Arity >= 1,
    Arity1 = Arity - 1,
    higher_order_func_type(Purity, Arity1, lambda_normal, TypeVarSet, FuncType,
        ArgTypes, RetType),
    ExistQVars = [],
    empty_hlds_constraints(EmptyConstraints),
    ConsTypeInfos = [cons_type_info(TypeVarSet, ExistQVars, RetType,
        [FuncType | ArgTypes], EmptyConstraints,
        source_apply(ApplyNameToUse))].

    % builtin_field_access_function_type(Info, GoalPath, Functor,
    %   Arity, ConsTypeInfos):
    %
    % Succeed if Functor is the name of one the automatically
    % generated field access functions (fieldname, '<fieldname> :=').
    %
:- pred builtin_field_access_function_type(typecheck_info::in, goal_path::in,
    cons_id::in, arity::in, list(maybe_cons_type_info)::out) is semidet.

builtin_field_access_function_type(Info, GoalPath, Functor, Arity,
        MaybeConsTypeInfos) :-
    % Taking the address of automatically generated field access functions
    % is not allowed, so currying does have to be considered here.
    Functor = cons(Name, Arity),
    typecheck_info_get_module_info(Info, ModuleInfo),
    is_field_access_function_name(ModuleInfo, Name, Arity, AccessType,
        FieldName),

    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    map.search(CtorFieldTable, FieldName, FieldDefns),

    list.filter_map(
        make_field_access_function_cons_type_info(Info, GoalPath, Name,
            Arity, AccessType, FieldName),
        FieldDefns, MaybeConsTypeInfos).

:- pred make_field_access_function_cons_type_info(typecheck_info::in,
    goal_path::in, sym_name::in, arity::in, field_access_type::in,
    ctor_field_name::in, hlds_ctor_field_defn::in,
    maybe_cons_type_info::out) is semidet.

make_field_access_function_cons_type_info(Info, GoalPath, FuncName, Arity,
        AccessType, FieldName, FieldDefn, ConsTypeInfo) :-
    get_field_access_constructor(Info, GoalPath, FuncName, Arity,
        AccessType, FieldDefn, OrigExistTVars,
        MaybeFunctorConsTypeInfo),
    (
        MaybeFunctorConsTypeInfo = ok(FunctorConsTypeInfo),
        module_info_get_class_table(Info ^ module_info, ClassTable),
        convert_field_access_cons_type_info(ClassTable, AccessType,
            FieldName, FieldDefn, FunctorConsTypeInfo,
            OrigExistTVars, ConsTypeInfo)
    ;
        MaybeFunctorConsTypeInfo = error(_),
        ConsTypeInfo = MaybeFunctorConsTypeInfo
    ).

:- pred get_field_access_constructor(typecheck_info::in, goal_path::in,
    sym_name::in, arity::in, field_access_type::in, hlds_ctor_field_defn::in,
    existq_tvars::out, maybe_cons_type_info::out) is semidet.

get_field_access_constructor(Info, GoalPath, FuncName, Arity, AccessType,
        FieldDefn, OrigExistTVars, FunctorConsTypeInfo) :-
    FieldDefn = hlds_ctor_field_defn(_, _, TypeCtor, ConsId, _),
    TypeCtor = type_ctor(qualified(TypeModule, _), _),

    % If the user has supplied a declaration, we use that instead
    % of the automatically generated version, unless we are typechecking
    % the clause introduced for the user-supplied declaration.
    % The user-declared version will be picked up by builtin_pred_type.

    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredTable),
    UnqualFuncName = unqualify_name(FuncName),
    (
        Info ^ is_field_access_function = no,
        \+ predicate_table_search_func_m_n_a(PredTable, is_fully_qualified,
            TypeModule, UnqualFuncName, Arity, _)
    ;
        Info ^ is_field_access_function = yes
    ),
    module_info_get_cons_table(ModuleInfo, Ctors),
    map.lookup(Ctors, ConsId, ConsDefns0),
    list.filter(
        (pred(CtorDefn::in) is semidet :-
            TypeCtor = CtorDefn ^ cons_type_ctor
        ), ConsDefns0, ConsDefns),
    ConsDefns = [ConsDefn],
    (
        AccessType = get,
        ConsAction = do_not_flip_constraints
    ;
        AccessType = set,
        ConsAction = flip_constraints_for_field_set
    ),
    OrigExistTVars = ConsDefn ^ cons_exist_tvars,
    convert_cons_defn(Info, GoalPath, ConsAction, ConsDefn,
        FunctorConsTypeInfo).

:- type maybe_cons_type_info
    --->    ok(cons_type_info)
    ;       error(cons_error).

:- pred convert_field_access_cons_type_info(class_table::in,
    field_access_type::in, ctor_field_name::in, hlds_ctor_field_defn::in,
    cons_type_info::in, existq_tvars::in, maybe_cons_type_info::out) is det.

convert_field_access_cons_type_info(ClassTable, AccessType, FieldName,
        FieldDefn, FunctorConsTypeInfo, OrigExistTVars, ConsTypeInfo) :-
    FunctorConsTypeInfo = cons_type_info(TVarSet0, ExistQVars,
        FunctorType, ConsArgTypes, Constraints0, Source0),
    ( Source0 = source_type(SourceTypePrime) ->
        SourceType = SourceTypePrime
    ;
        unexpected(this_file, "convert_field_access_cons_type_info: not type")
    ),
    FieldDefn = hlds_ctor_field_defn(_, _, _, _, FieldNumber),
    list.index1_det(ConsArgTypes, FieldNumber, FieldType),
    (
        AccessType = get,
        Source = source_get_field_access(SourceType),
        RetType = FieldType,
        ArgTypes = [FunctorType],
        TVarSet = TVarSet0,
        Constraints = Constraints0,
        ConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
            RetType, ArgTypes, Constraints, Source))
    ;
        AccessType = set,
        Source = source_set_field_access(SourceType),

        % When setting a polymorphic field, the type of the field in the result
        % is not necessarily the same as in the input. If a type variable
        % occurs only in the field being set, create a new type variable for it
        % in the result type.
        %
        % This allows code such as
        % :- type pair(T, U)
        %   ---> '-'(fst::T, snd::U).
        %
        %   Pair0 = 1 - 'a',
        %   Pair = Pair0 ^ snd := 2.

        prog_type.vars(FieldType, TVarsInField),
        (
            TVarsInField = [],
            TVarSet = TVarSet0,
            RetType = FunctorType,
            ArgTypes = [FunctorType, FieldType],

            % None of the constraints are affected by the updated field,
            % so the constraints are unchanged.
            Constraints = Constraints0,

            ConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
                RetType, ArgTypes, Constraints, Source))
        ;
            TVarsInField = [_ | _],

            % XXX This demonstrates a problem - if a type variable occurs
            % in the types of multiple fields, any predicates changing values
            % of one of these fields cannot change their types. This especially
            % a problem for existentially typed fields, because setting the
            % field always changes the type.
            %
            % Haskell gets around this problem by allowing multiple fields
            % to be set by the same expression. Haskell doesn't handle all
            % cases -- it is not possible to get multiple existentially typed
            % fields using record syntax and pass them to a function whose type
            % requires that the fields are of the same type. It probably won't
            % come up too often.
            %
            list.replace_nth_det(ConsArgTypes, FieldNumber, int_type,
                ArgTypesWithoutField),
            prog_type.vars_list(ArgTypesWithoutField, TVarsInOtherArgs),
            set.intersect(
                set.list_to_set(TVarsInField),
                set.intersect(
                    set.list_to_set(TVarsInOtherArgs),
                    set.list_to_set(OrigExistTVars)
                ),
                ExistQVarsInFieldAndOthers),
            ( set.empty(ExistQVarsInFieldAndOthers) ->
                % Rename apart type variables occurring only in the field
                % to be replaced - the values of those type variables will be
                % supplied by the replacement field value.
                list.delete_elems(TVarsInField,
                    TVarsInOtherArgs, TVarsOnlyInField0),
                list.sort_and_remove_dups(TVarsOnlyInField0, TVarsOnlyInField),
                list.length(TVarsOnlyInField, NumNewTVars),
                varset.new_vars(TVarSet0, NumNewTVars, NewTVars, TVarSet),
                map.from_corresponding_lists(TVarsOnlyInField,
                    NewTVars, TVarRenaming),
                apply_variable_renaming_to_type(TVarRenaming, FieldType,
                    RenamedFieldType),
                apply_variable_renaming_to_type(TVarRenaming, FunctorType,
                    OutputFunctorType),

                % Rename the class constraints, projecting the constraints
                % onto the set of type variables occuring in the types of the
                % arguments of the call to `'field :='/2'. Note that we have
                % already flipped the constraints.
                prog_type.vars_list([FunctorType, FieldType], CallTVars0),
                set.list_to_set(CallTVars0, CallTVars),
                project_and_rename_constraints(ClassTable, TVarSet, CallTVars,
                    TVarRenaming, Constraints0, Constraints),

                RetType = OutputFunctorType,
                ArgTypes = [FunctorType, RenamedFieldType],
                ConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
                    RetType, ArgTypes, Constraints, Source))
            ;
                % This field cannot be set. Pass out some information so that
                % we can give a better error message. Errors involving changing
                % the types of universally quantified type variables will be
                % caught by typecheck_functor_arg_types.
                set.to_sorted_list(ExistQVarsInFieldAndOthers,
                    ExistQVarsInFieldAndOthers1),
                ConsTypeInfo = error(invalid_field_update(FieldName,
                    FieldDefn, TVarSet0, ExistQVarsInFieldAndOthers1))
            )
        )
    ).

    % Add new universal constraints for constraints containing variables that
    % have been renamed.  These new constraints are the ones that will need
    % to be supplied by the caller.  The other constraints will be supplied
    % from non-updated fields.
    %
:- pred project_and_rename_constraints(class_table::in, tvarset::in,
    set(tvar)::in, tvar_renaming::in,
    hlds_constraints::in, hlds_constraints::out) is det.

project_and_rename_constraints(ClassTable, TVarSet, CallTVars, TVarRenaming,
        !Constraints) :-
    !.Constraints = constraints(Unproven0, Assumed, Redundant0),

    % Project the constraints down onto the list of tvars in the call.
    list.filter(project_constraint(CallTVars), Unproven0, NewUnproven0),
    list.filter_map(rename_constraint(TVarRenaming), NewUnproven0,
        NewUnproven),
    update_redundant_constraints(ClassTable, TVarSet, NewUnproven,
        Redundant0, Redundant),
    list.append(NewUnproven, Unproven0, Unproven),
    !:Constraints = constraints(Unproven, Assumed, Redundant).

:- pred project_constraint(set(tvar)::in, hlds_constraint::in) is semidet.

project_constraint(CallTVars, Constraint) :-
    Constraint = constraint(_, _, TypesToCheck),
    prog_type.vars_list(TypesToCheck, TVarsToCheck0),
    set.list_to_set(TVarsToCheck0, TVarsToCheck),
    set.intersect(TVarsToCheck, CallTVars, RelevantTVars),
    \+ set.empty(RelevantTVars).

:- pred rename_constraint(tvar_renaming::in, hlds_constraint::in,
    hlds_constraint::out) is semidet.

rename_constraint(TVarRenaming, Constraint0, Constraint) :-
    Constraint0 = constraint(Ids, Name, Types0),
    some [Var] (
        type_list_contains_var(Types0, Var),
        map.contains(TVarRenaming, Var)
    ),
    apply_variable_renaming_to_type_list(TVarRenaming, Types0, Types),
    Constraint = constraint(Ids, Name, Types).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Note: changes here may require changes to
    % post_resolve_unify_functor,
    % intermod.module_qualify_unify_rhs,
    % recompilation.usage.find_matching_constructors
    % and recompilation.check.check_functor_ambiguities.
    %
:- pred typecheck_info_get_ctor_list(typecheck_info::in, cons_id::in, int::in,
    goal_path::in, list(cons_type_info)::out, list(cons_error)::out) is det.

typecheck_info_get_ctor_list(Info, Functor, Arity, GoalPath, ConsInfos,
        ConsErrors) :-
    (
        % If we're typechecking the clause added for a field access function
        % for which the user has supplied type or mode declarations, the goal
        % should only contain an application of the field access function,
        % not constructor applications or function calls. The clauses in
        % `.opt' files will already have been expanded into unifications.
        Info ^ is_field_access_function = yes,
        Info ^ import_status \= status_opt_imported
    ->
        (
            builtin_field_access_function_type(Info, GoalPath,
                Functor, Arity, FieldAccessConsInfos)
        ->
            split_cons_errors(FieldAccessConsInfos, ConsInfos, ConsErrors)
        ;
            ConsInfos = [],
            ConsErrors = []
        )
    ;
        typecheck_info_get_ctor_list_2(Info, Functor, Arity, GoalPath,
            ConsInfos, ConsErrors)
    ).

:- pred typecheck_info_get_ctor_list_2(typecheck_info::in, cons_id::in,
    int::in, goal_path::in, list(cons_type_info)::out, list(cons_error)::out)
    is det.

typecheck_info_get_ctor_list_2(Info, Functor, Arity, GoalPath, ConsInfos,
        DataConsErrors) :-
    empty_hlds_constraints(EmptyConstraints),

    % Check if `Functor/Arity' has been defined as a constructor in some
    % discriminated union type(s).  This gives us a list of possible
    % cons_type_infos.
    typecheck_info_get_ctors(Info, Ctors),
    (
        Functor = cons(_, _),
        map.search(Ctors, Functor, HLDS_ConsDefns)
    ->
        convert_cons_defn_list(Info, GoalPath, do_not_flip_constraints,
            HLDS_ConsDefns, PlainMaybeConsInfos)
    ;
        PlainMaybeConsInfos = []
    ),

    % For "existentially typed" functors, whether the functor is actually
    % existentially typed depends on whether it is used as a constructor
    % or as a deconstructor. As a constructor, it is universally typed,
    % but as a deconstructor, it is existentially typed. But type checking
    % and polymorphism need to know whether it is universally or existentially
    % quantified _before_ mode analysis has inferred the mode of the
    % unification. Therefore, we use a special syntax for construction
    % unifications with existentially quantified functors: instead of
    % just using the functor name (e.g. "Y = foo(X)", the programmer must use
    % the special functor name "new foo" (e.g. "Y = 'new foo'(X)").
    %
    % Here we check for occurrences of functor names starting with "new ".
    % For these, we look up the original functor in the constructor symbol
    % table, and for any occurrences of that functor we flip the quantifiers on
    % the type definition (i.e. convert the existential quantifiers and
    % constraints into universal ones).
    (
        Functor = cons(Name, Arity),
        remove_new_prefix(Name, OrigName),
        OrigFunctor = cons(OrigName, Arity),
        map.search(Ctors, OrigFunctor, HLDS_ExistQConsDefns)
    ->
        convert_cons_defn_list(Info, GoalPath, flip_constraints_for_new,
            HLDS_ExistQConsDefns, UnivQuantifiedMaybeConsInfos)
    ;
        UnivQuantifiedMaybeConsInfos = []
    ),

    % Check if Functor is a field access function for which the user
    % has not supplied a declaration.
    (
        builtin_field_access_function_type(Info, GoalPath, Functor,
            Arity, FieldAccessMaybeConsInfosPrime)
    ->
        FieldAccessMaybeConsInfos = FieldAccessMaybeConsInfosPrime
    ;
        FieldAccessMaybeConsInfos = []
    ),

    DataMaybeConsInfos = PlainMaybeConsInfos ++ UnivQuantifiedMaybeConsInfos
        ++ FieldAccessMaybeConsInfos,
    split_cons_errors(DataMaybeConsInfos, DataConsInfos, DataConsErrors),

    % Check if Functor is a constant of one of the builtin atomic types
    % (string, float, int, character). If so, insert the resulting
    % cons_type_info at the start of the list.
    (
        Arity = 0,
        builtin_atomic_type(Functor, BuiltInTypeName)
    ->
        TypeCtor = type_ctor(unqualified(BuiltInTypeName), 0),
        construct_type(TypeCtor, [], ConsType),
        varset.init(ConsTypeVarSet),
        ConsInfo = cons_type_info(ConsTypeVarSet, [], ConsType, [],
            EmptyConstraints, source_builtin_type(BuiltInTypeName)),
        BuiltinConsInfos = [ConsInfo]
    ;
        BuiltinConsInfos = []
    ),

    % Check if Functor is a tuple constructor.
    ( Functor = cons(unqualified("{}"), TupleArity) ->
        % Make some fresh type variables for the argument types. These have
        % kind `star' since there are values (namely the arguments of the
        % tuple constructor) which have these types.

        varset.init(TupleConsTypeVarSet0),
        varset.new_vars(TupleConsTypeVarSet0, TupleArity, TupleArgTVars,
            TupleConsTypeVarSet),
        prog_type.var_list_to_type_list(map.init, TupleArgTVars,
            TupleArgTypes),

        TupleTypeCtor = type_ctor(unqualified("{}"), TupleArity),
        construct_type(TupleTypeCtor, TupleArgTypes, TupleConsType),

        % Tuples can't have existentially typed arguments.
        TupleExistQVars = [],
        TupleConsInfo = cons_type_info(TupleConsTypeVarSet, TupleExistQVars,
            TupleConsType, TupleArgTypes, EmptyConstraints,
            source_builtin_type("tuple")),
        TupleConsInfos = [TupleConsInfo]
    ;
        TupleConsInfos = []
    ),

    % Check if Functor is the name of a predicate which takes at least
    % Arity arguments. If so, insert the resulting cons_type_info
    % at the start of the list.
    ( builtin_pred_type(Info, Functor, Arity, GoalPath, PredConsInfosPrime) ->
        PredConsInfos = PredConsInfosPrime
    ;
        PredConsInfos = []
    ),

    % Check for higher-order function calls.
    ( builtin_apply_type(Info, Functor, Arity, ApplyConsInfosPrime) ->
        ApplyConsInfos = ApplyConsInfosPrime
    ;
        ApplyConsInfos = []
    ),

    OtherConsInfos = BuiltinConsInfos ++ TupleConsInfos
        ++ PredConsInfos ++ ApplyConsInfos,
    ConsInfos = DataConsInfos ++ OtherConsInfos.

:- pred split_cons_errors(list(maybe_cons_type_info)::in,
    list(cons_type_info)::out, list(cons_error)::out) is det.

split_cons_errors(MaybeConsInfoList, ConsInfoList, ConsErrors) :-
    % Filter out the errors (they aren't actually reported as errors
    % unless there was no other matching constructor).
    list.filter_map(
        (pred(ok(ConsInfo)::in, ConsInfo::out) is semidet),
        MaybeConsInfoList, ConsInfoList, ConsErrors0),
    (
        list.map((pred(error(ConsError)::in, ConsError::out) is semidet),
            ConsErrors0, ConsErrors1)
    ->
        ConsErrors = ConsErrors1
    ;
        unexpected(this_file, "typecheck_info_get_ctor_list")
    ).

%-----------------------------------------------------------------------------%

:- type cons_constraints_action
    --->    flip_constraints_for_new
    ;       flip_constraints_for_field_set
    ;       do_not_flip_constraints.

:- pred convert_cons_defn_list(typecheck_info::in, goal_path::in,
    cons_constraints_action::in, list(hlds_cons_defn)::in,
    list(maybe_cons_type_info)::out) is det.

convert_cons_defn_list(_Info, _GoalPath, _Action, [], []).
convert_cons_defn_list(Info, GoalPath, Action, [X | Xs], [Y | Ys]) :-
    convert_cons_defn(Info, GoalPath, Action, X, Y),
    convert_cons_defn_list(Info, GoalPath, Action, Xs, Ys).

:- pred convert_cons_defn(typecheck_info::in, goal_path::in,
    cons_constraints_action::in, hlds_cons_defn::in,
    maybe_cons_type_info::out) is det.

convert_cons_defn(Info, GoalPath, Action, HLDS_ConsDefn, ConsTypeInfo) :-
    HLDS_ConsDefn = hlds_cons_defn(ExistQVars0, ExistProgConstraints, Args,
        TypeCtor, _),
    assoc_list.values(Args, ArgTypes),
    typecheck_info_get_types(Info, Types),
    map.lookup(Types, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_tvarset(TypeDefn, ConsTypeVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, ConsTypeParams),
    hlds_data.get_type_defn_kind_map(TypeDefn, ConsTypeKinds),
    hlds_data.get_type_defn_body(TypeDefn, Body),

    % If this type has `:- pragma foreign_type' declarations, we
    % can only use its constructors in predicates which have foreign
    % clauses and in the unification and comparison predicates for
    % the type (otherwise the code wouldn't compile when using a
    % back-end which caused another version of the type to be selected).
    % The constructors may also appear in the automatically generated
    % unification and comparison predicates.
    %
    % XXX This check isn't quite right -- we really need to check for
    % each procedure that there is a foreign_proc declaration for all
    % languages for which this type has a foreign_type declaration, but
    % this will do for now. Such a check may be difficult because by
    % this point we've thrown away the clauses which we aren't using
    % in the current compilation.
    %
    % The `.opt' files don't contain the foreign clauses from the source
    % file that aren't used when compiling in the current grade, so we
    % allow foreign type constructors in `opt_imported' predicates even
    % if there are no foreign clauses. Errors will be caught when creating
    % the `.opt' file.
    %
    typecheck_info_get_predid(Info, PredId),
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    (
        Body ^ du_type_is_foreign_type = yes(_),
        \+ pred_info_get_goal_type(PredInfo, goal_type_clause_and_foreign),
        \+ is_unify_or_compare_pred(PredInfo),
        \+ pred_info_get_import_status(PredInfo, status_opt_imported)
    ->
        ConsTypeInfo = error(foreign_type_constructor(TypeCtor, TypeDefn))
    ;
        % Do not allow constructors for abstract_imported types unless
        % the current predicate is opt_imported.
        hlds_data.get_type_defn_status(TypeDefn, status_abstract_imported),
        \+ is_unify_or_compare_pred(PredInfo),
        \+ pred_info_get_import_status(PredInfo, status_opt_imported)
    ->
        ConsTypeInfo = error(abstract_imported_type)
    ;
        Action = flip_constraints_for_new,
        ExistQVars0 = []
    ->
        % Do not allow 'new' constructors except on existential types.
        ConsTypeInfo = error(new_on_non_existential_type(TypeCtor))
    ;
        prog_type.var_list_to_type_list(ConsTypeKinds, ConsTypeParams,
            ConsTypeArgs),
        construct_type(TypeCtor, ConsTypeArgs, ConsType),
        UnivProgConstraints = [],
        (
            Action = do_not_flip_constraints,
            ProgConstraints = constraints(UnivProgConstraints,
                ExistProgConstraints),
            ExistQVars = ExistQVars0
        ;
            Action = flip_constraints_for_new,
            % Make the existential constraints into universal ones, and discard
            % the existentially quantified variables (since they are now
            % universally quantified).
            ProgConstraints = constraints(ExistProgConstraints,
                UnivProgConstraints),
            ExistQVars = []
        ;
            Action = flip_constraints_for_field_set,
            % The constraints are existential for the deconstruction, and
            % universal for the construction.  Even though all of the unproven
            % constraints here can be trivially reduced by the assumed ones,
            % we still need to process them so that the appropriate tables
            % get updated.
            %
            ProgConstraints = constraints(ExistProgConstraints,
                ExistProgConstraints),
            ExistQVars = ExistQVars0
        ),
        make_body_hlds_constraints(ClassTable, ConsTypeVarSet,
            GoalPath, ProgConstraints, Constraints),
        ConsTypeInfo = ok(cons_type_info(ConsTypeVarSet, ExistQVars,
            ConsType, ArgTypes, Constraints, source_type(TypeCtor)))
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "typecheck.m".

%-----------------------------------------------------------------------------%
