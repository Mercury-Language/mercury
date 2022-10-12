%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.goal_expr_to_goal.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.state_var.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type loc_kind
    --->    loc_whole_goal
    ;       loc_inside_atomic_goal.

    % Convert goals from the `goal' structure of the parse tree
    % into the `hlds_goal' structure of the HLDS. At the same time,
    %
    % - convert it to super-homogeneous form by unravelling all the complex
    %   unifications, and annotate those unifications with a unify_context
    %   so that we can still give good error messages;
    %
    % - apply the given substitution to the goal, to rename it apart
    %   from the other clauses; and
    %
    % - expand references to state variables.
    %
:- pred transform_parse_tree_goal_to_hlds(loc_kind::in, goal::in,
    prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_goal.
:- import_module hlds.make_hlds.field_access.
:- import_module hlds.make_hlds.superhomogeneous.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_db.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module getopt.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    (
        (
            Goal = fail_expr(Context),
            GoalExpr = disj([])
        ;
            Goal = true_expr(Context),
            GoalExpr = conj(plain_conj, [])
        ),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = unify_expr(_, _, _, _),
        transform_parse_tree_goal_to_hlds_unify(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = call_expr(_, _, _, _),
        transform_parse_tree_goal_to_hlds_call(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = conj_expr(_, _, _),
        transform_parse_tree_goal_to_hlds_conj(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = par_conj_expr(_, _, _),
        transform_parse_tree_goal_to_hlds_par_conj(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = disj_expr(_, _, _, _),
        transform_parse_tree_goal_to_hlds_disj(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = if_then_else_expr(_, _, _, _, _, _),
        transform_parse_tree_goal_to_hlds_ite(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = not_expr(_, _),
        transform_parse_tree_goal_to_hlds_not(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = implies_expr(Context, P, Q),
        % `P => Q' is defined as `not (P, not Q)'
        TransformedGoal =
            not_expr(Context,
                conj_expr(Context, P, [not_expr(Context, Q)])),
        transform_parse_tree_goal_to_hlds(LocKind, TransformedGoal,
            Renaming, HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = equivalent_expr(_, _, _),
        transform_parse_tree_goal_to_hlds_equivalent(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = quant_expr(_, _, _, _, _),
        transform_parse_tree_goal_to_hlds_quant(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        (
            Goal = promise_purity_expr(Context, Purity, SubGoal),
            Reason = promise_purity(Purity)
        ;
            Goal = require_detism_expr(Context, Detism, SubGoal),
            Reason = require_detism(Detism)
        ),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        (
            Goal = require_complete_switch_expr(Context, PODVar0, SubGoal),
            rename_and_maybe_expand_dot_var(Context, need_not_rename, Renaming,
                PODVar0, Var, !SVarState, !VarSet, !Specs),
            Reason = require_complete_switch(Var)
        ;
            Goal = require_switch_arms_detism_expr(Context, PODVar0, Detism,
                SubGoal),
            rename_and_maybe_expand_dot_var(Context, need_not_rename, Renaming,
                PODVar0, Var, !SVarState, !VarSet, !Specs),
            Reason = require_switch_arms_detism(Var, Detism)
        ),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        (
            Goal = promise_equivalent_solutions_expr(Context, Vars, StateVars,
                DotSVars, ColonSVars, SubGoal),
            PromiseKind = equivalent_solutions
        ;
            Goal = promise_equivalent_solution_sets_expr(Context,
                Vars, StateVars, DotSVars, ColonSVars, SubGoal),
            PromiseKind = equivalent_solution_sets
        ;
            Goal = promise_equivalent_solution_arbitrary_expr(Context,
                Vars, StateVars, DotSVars, ColonSVars, SubGoal),
            PromiseKind = equivalent_solution_sets_arbitrary
        ),
        transform_promise_eqv_goal(LocKind, Vars, StateVars,
            DotSVars, ColonSVars, Context, Renaming, PromiseVars,
            SubGoal, HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        Reason = promise_solutions(PromiseVars, PromiseKind),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = try_expr(_, _, _, _, _, _, _),
        transform_parse_tree_goal_to_hlds_try(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = atomic_expr(_, _, _, _, _, _),
        transform_parse_tree_goal_to_hlds_atomic(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = disable_warnings_expr(_, _, _, _),
        transform_parse_tree_goal_to_hlds_disable_warnings(LocKind, Goal,
            Renaming, HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = trace_expr(_, _, _, _, _, _),
        transform_parse_tree_goal_to_hlds_trace(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = event_expr(_, _, _),
        transform_parse_tree_goal_to_hlds_event(LocKind, Goal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ).

%----------------------------------------------------------------------------%

:- inst goal_unify_expr for goal/0
    --->    unify_expr(ground, ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_unify(loc_kind::in,
    goal::in(goal_unify_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_unify/16)).

transform_parse_tree_goal_to_hlds_unify(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = unify_expr(Context, TermA0, TermB0, Purity),
    rename_vars_in_term(need_not_rename, Renaming, TermA0, TermA),
    rename_vars_in_term(need_not_rename, Renaming, TermB0, TermB),
    % It is an error for the left or right hand side of a unification
    % to be !A (although it may be !.A or !:A).
    ( if TermA = functor(atom("!"), [variable(StateVarA, _)], _) then
        report_svar_unify_error(Context, StateVarA,
            !VarSet, !SVarState, !Specs),
        ( if TermB = functor(atom("!"), [variable(StateVarB, _)], _) then
            report_svar_unify_error(Context, StateVarB,
                !VarSet, !SVarState, !Specs)
        else
            true
        ),
        HLDSGoal = true_goal_with_context(Context)
    else if TermB = functor(atom("!"), [variable(StateVarB, _)], _) then
        report_svar_unify_error(Context, StateVarB,
            !VarSet, !SVarState, !Specs),
        HLDSGoal = true_goal_with_context(Context)
    else
        unravel_unification(TermA, TermB, Context, umc_explicit, [],
            Purity, HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        svar_finish_atomic_goal(LocKind, !SVarState)
    ).

%----------------------------------------------------------------------------%

:- inst goal_call_expr for goal/0
    --->    call_expr(ground, ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_call(loc_kind::in,
    goal::in(goal_call_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_call/16)).

transform_parse_tree_goal_to_hlds_call(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = call_expr(Context, SymName, ArgTerms0, Purity),
    expand_bang_state_pairs_in_terms(ArgTerms0, ArgTerms1),
    ( if
        SymName = unqualified("\\="),
        ArgTerms1 = [LHSTerm, RHSTerm]
    then
        % `LHS \= RHS' is defined as `not (LHS = RHS)'
        TransformedGoal = not_expr(Context,
            unify_expr(Context, LHSTerm, RHSTerm, Purity)),
        transform_parse_tree_goal_to_hlds(LocKind, TransformedGoal,
            Renaming, HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    else if
        % check for a state var record assignment:
        % !Var ^ field := Value
        SymName = unqualified(":="),
        ArgTerms1 = [LHSTerm0, RHSTerm0],
        LHSTerm0 = functor(atom("^"), [StateVar0, Remainder],
            FieldListContext),
        StateVar0 = functor(atom("!"), StateVarNameTerms, StateVarContext),
        StateVarNameTerms = [variable(_, _)]
    then
        % !Var ^ field := Value is defined as
        % !:Var = !.Var ^ field := Value.
        LHSTerm = functor(atom("!:"), StateVarNameTerms, StateVarContext),
        StateVar = functor(atom("!."), StateVarNameTerms, StateVarContext),
        FieldList = functor(atom("^"), [StateVar, Remainder],
            FieldListContext),
        RHSTerm = functor(atom(":="), [FieldList, RHSTerm0], Context),
        TransformedGoal = unify_expr(Context, LHSTerm, RHSTerm, Purity),
        transform_parse_tree_goal_to_hlds(LocKind, TransformedGoal,
            Renaming, HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    else if
        % check for a DCG field access goal:
        % get: Field =^ field
        % set: ^ field := Field
        SymName = unqualified(Operator),
        ( Operator = "=^", AccessType = get
        ; Operator = ":=", AccessType = set
        )
    then
        rename_vars_in_term_list(need_not_rename, Renaming,
            ArgTerms1, ArgTerms),
        transform_dcg_record_syntax(LocKind, AccessType, ArgTerms, Context,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    else
        rename_vars_in_term_list(need_not_rename, Renaming,
            ArgTerms1, ArgTerms),
        make_fresh_arg_vars_subst_svars(ArgTerms, HeadVars, HeadVarsArgTerms,
            !VarSet, !SVarState, !Specs),
        PredFormArity = arg_list_arity(ArgTerms),
        ( if
            % Check for a higher-order call,
            % i.e. a call to either call/N or ''/N.
            ( SymName = unqualified("call")
            ; SymName = unqualified("")
            ),
            HeadVars = [PredVar | RealHeadVars]
        then
            % Initialize some fields to junk.
            Modes = [],
            MaybeArgRegs = arg_reg_types_unset,
            Det = detism_erroneous,
            GenericCall = higher_order(PredVar, Purity, pf_predicate,
                PredFormArity),
            GoalExpr = generic_call(GenericCall, RealHeadVars, Modes,
                MaybeArgRegs, Det),
            hlds_goal.generic_call_to_id(GenericCall, GenericCallId),
            CallId = generic_call_id(GenericCallId)
        else
            % Initialize some fields to junk.
            PredId = invalid_pred_id,
            ModeId = invalid_proc_id,
            MaybeUnifyContext = no,
            GoalExpr = plain_call(PredId, ModeId, HeadVars, not_builtin,
                MaybeUnifyContext, SymName),
            PFSymNameArity =
                pf_sym_name_arity(pf_predicate, SymName, PredFormArity),
            CallId = plain_call_id(PFSymNameArity)
        ),
        goal_info_init_context_purity(Context, Purity, GoalInfo),
        HLDSGoal0 = hlds_goal(GoalExpr, GoalInfo),
        user_arity_pred_form_arity(pf_predicate, UserArity, PredFormArity),
        record_called_pred_or_func(pf_predicate, SymName, UserArity,
            !QualInfo),
        insert_arg_unifications(HeadVarsArgTerms, Context,
            ac_call(CallId), HLDSGoal0, HLDSGoal, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ),
    svar_finish_atomic_goal(LocKind, !SVarState).

:- pred transform_dcg_record_syntax(loc_kind::in,
    field_access_type::in, list(prog_term)::in, prog_context::in,
    hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_dcg_record_syntax(LocKind, AccessType, ArgTerms0, Context, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    goal_info_init(Context, GoalInfo),
    ( if
        ArgTerms0 = [LHSTerm, RHSTerm, TermInputTerm, TermOutputTerm],
        (
            AccessType = get,
            FieldNameTerm = RHSTerm,
            FieldValueTerm = LHSTerm
        ;
            AccessType = set,
            LHSTerm = term.functor(term.atom("^"), [FieldNameTerm0], _),
            FieldNameTerm = FieldNameTerm0,
            FieldValueTerm = RHSTerm
        )
    then
        ContextPieces = dcg_field_error_context_pieces(AccessType),
        parse_field_list(FieldNameTerm, !.VarSet, ContextPieces,
            MaybeFieldNames),
        (
            MaybeFieldNames = ok1(FieldNames),
            ArgTerms = [FieldValueTerm, TermInputTerm, TermOutputTerm],
            transform_dcg_record_syntax_2(AccessType, FieldNames, ArgTerms,
                Context, HLDSGoal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            svar_finish_atomic_goal(LocKind, !SVarState)
        ;
            MaybeFieldNames = error1(FieldNamesSpecs),
            !:Specs = FieldNamesSpecs ++ !.Specs,
            invalid_goal("^", ArgTerms0, GoalInfo, HLDSGoal, !VarSet,
                !SVarState, !Specs),
            qual_info_set_found_syntax_error(yes, !QualInfo)
        )
    else
        invalid_goal("^", ArgTerms0, GoalInfo, HLDSGoal, !VarSet, !SVarState,
            !Specs),
        qual_info_set_found_syntax_error(yes, !QualInfo),
        Pieces = [words("Error: expected"),
            words_quote("Field =^ field1 ^ ... ^ fieldN"),
            words("or"), words_quote("^ field1 ^ ... ^ fieldN := Field"),
            words("in DCG field access goal."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred transform_dcg_record_syntax_2(field_access_type::in, field_list::in,
    list(prog_term)::in, prog_context::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_dcg_record_syntax_2(AccessType, FieldNames, ArgTerms, Context,
        HLDSGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    make_fresh_arg_vars_subst_svars(ArgTerms, _ArgVars, ArgVarsTerms,
        !VarSet, !SVarState, !Specs),
    (
        ArgVarsTerms = [FieldValueVarTerm, TermInputVarTerm, TermOutputVarTerm]
    ;
        ( ArgVarsTerms = []
        ; ArgVarsTerms = [_]
        ; ArgVarsTerms = [_, _]
        ; ArgVarsTerms = [_, _, _, _ | _]
        ),
        unexpected($pred, "arity != 3")
    ),
    FieldValueVarTerm = unify_var_term(FieldValueVar, FieldValueTerm),
    TermInputVarTerm = unify_var_term(TermInputVar, TermInputTerm),
    TermOutputVarTerm = unify_var_term(TermOutputVar, TermOutputTerm),
    InputTermArgNumber = 1,
    InputTermArgContext = ac_functor(Functor, umc_explicit, []),
    (
        AccessType = set,
        expand_set_field_function_call(Context, umc_explicit, [],
            FieldNames, FieldValueVar, TermInputVar, TermOutputVar,
            Functor, InnermostFunctor - InnermostSubContext, HLDSGoal0,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        FieldArgNumber = 2,
        FieldArgContext = ac_functor(InnermostFunctor, umc_explicit,
            InnermostSubContext),
        ( if Functor = cons(ConsNamePrime, ConsArityPrime, _TypeCtor) then
            ConsName = ConsNamePrime,
            ConsArity = ConsArityPrime
        else
            unexpected($pred, "not cons")
        ),
        PFSymNameArity = pf_sym_name_arity(pf_function, ConsName,
            pred_form_arity(ConsArity)),
        % DCG arguments should always be distinct variables,
        % so this context should never be used.
        OutputTermArgNumber = 3,
        OutputTermArgContext = ac_call(plain_call_id(PFSymNameArity))
    ;
        AccessType = get,
        expand_dcg_field_extraction_goal(Context, umc_explicit, [],
            FieldNames, FieldValueVar, TermInputVar, TermOutputVar,
            Functor, InnermostFunctor - _InnermostSubContext, HLDSGoal0,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        ( if InnermostFunctor = cons(ConsNamePrime, ConsArityPrime, _TC) then
            ConsName = ConsNamePrime,
            ConsArity = ConsArityPrime
        else
            unexpected($pred, "not cons")
        ),
        PFSymNameArity = pf_sym_name_arity(pf_function, ConsName,
            pred_form_arity(ConsArity)),
        FieldArgNumber = 2,
        FieldArgContext = ac_call(plain_call_id(PFSymNameArity)),
        % DCG arguments should always be distinct variables,
        % so this context should never be used.
        OutputTermArgNumber = 3,
        OutputTermArgContext = ac_functor(Functor, umc_explicit, [])
    ),

    FieldValueVTNC = unify_var_term_num_context(FieldValueVar,
        FieldValueTerm, FieldArgNumber, FieldArgContext),
    TermInputVTNC = unify_var_term_num_context(TermInputVar,
        TermInputTerm, InputTermArgNumber, InputTermArgContext),
    TermOutputVTNC = unify_var_term_num_context(TermOutputVar,
        TermOutputTerm, OutputTermArgNumber, OutputTermArgContext),
    ArgVarsTermsNumsContexts = [FieldValueVTNC, TermInputVTNC, TermOutputVTNC],
    insert_arg_unifications_with_contexts(ArgVarsTermsNumsContexts,
        Context, HLDSGoal0, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

:- func dcg_field_error_context_pieces(field_access_type) =
    list(format_piece).

dcg_field_error_context_pieces(AccessType) = ContextPieces :-
    (
        AccessType = set,
        ContextPieces = [words("In DCG field update goal:"), nl]
    ;
        AccessType = get,
        ContextPieces = [words("In DCG field extraction goal:"), nl]
    ).

%----------------------------------------------------------------------------%

:- inst goal_conj_expr for goal/0
    --->    conj_expr(ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_conj(loc_kind::in,
    goal::in(goal_conj_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_conj/16)).

transform_parse_tree_goal_to_hlds_conj(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = conj_expr(Context, ConjunctA, ConjunctsB),
    accumulate_plain_or_par_conjunct(LocKind, plain_conj, Renaming,
        ConjunctA, cord.init, HLDSConjunctsCordA,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    list.foldl7(
        accumulate_plain_or_par_conjunct(LocKind, plain_conj, Renaming),
        ConjunctsB, HLDSConjunctsCordA, HLDSConjunctsCord,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    HLDSConjuncts = cord.list(HLDSConjunctsCord),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(HLDSConjuncts, GoalInfo, HLDSGoal).

%----------------------------------------------------------------------------%

:- inst goal_par_conj_expr for goal/0
    --->    par_conj_expr(ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_par_conj(loc_kind::in,
    goal::in(goal_par_conj_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_par_conj/16)).

transform_parse_tree_goal_to_hlds_par_conj(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = par_conj_expr(Context, ConjunctA, ConjunctsB),
    accumulate_plain_or_par_conjunct(LocKind, parallel_conj, Renaming,
        ConjunctA, cord.init, HLDSConjunctsCordA,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    list.foldl7(
        accumulate_plain_or_par_conjunct(LocKind, parallel_conj, Renaming),
        ConjunctsB, HLDSConjunctsCordA, HLDSConjunctsCord,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    HLDSConjuncts = cord.list(HLDSConjunctsCord),
    goal_info_init(Context, GoalInfo),
    par_conj_list_to_goal(HLDSConjuncts, GoalInfo, HLDSGoal).

%----------------------------------------------------------------------------%

    % accumulate_plain_or_par_conjunct(LocKind, PlainOrPar Renaming,
    %   Goal, !HLDSConjunctsCord, ...):
    %
    % Goal is a tree of conjuncts. Flatten it into a list (applying Renaming),
    % and append the result to the end of !HLDSConjunctsCord.
    %
:- pred accumulate_plain_or_par_conjunct(loc_kind::in, conj_type::in,
    prog_var_renaming::in, goal::in, cord(hlds_goal)::in, cord(hlds_goal)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

accumulate_plain_or_par_conjunct(LocKind, ConjType, Renaming,
        Goal, !HLDSConjunctsCord, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    HLDSGoal = hlds_goal(HLDSGoalExpr, _),
    ( if HLDSGoalExpr = conj(ConjType, HLDSConjuncts) then
        !:HLDSConjunctsCord = !.HLDSConjunctsCord ++
            cord.from_list(HLDSConjuncts)
    else
        cord.snoc(HLDSGoal, !HLDSConjunctsCord)
    ).

%----------------------------------------------------------------------------%

:- inst goal_disj_expr for goal/0
    --->    disj_expr(ground, ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_disj(loc_kind::in,
    goal::in(goal_disj_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_disj/16)).

transform_parse_tree_goal_to_hlds_disj(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = disj_expr(Context, Disjunct1, Disjunct2, Disjuncts3plus),
    SVarStateBefore = !.SVarState,
    accumulate_disjunct(LocKind, Renaming, SVarStateBefore,
        Disjunct1, [], RevDisjunctsSVarStates1,
        !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    accumulate_disjunct(LocKind, Renaming, SVarStateBefore,
        Disjunct2, RevDisjunctsSVarStates1, RevDisjunctsSVarStates2,
        !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    list.foldl6(
        accumulate_disjunct(LocKind, Renaming, SVarStateBefore),
        Disjuncts3plus, RevDisjunctsSVarStates2, RevDisjunctsSVarStates,
        !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    list.reverse(RevDisjunctsSVarStates, DisjunctsSVarStates),
    svar_finish_disjunction(DisjunctsSVarStates, Disjuncts,
        !VarSet, SVarStateBefore, SVarStateAfter, !SVarStore),
    !:SVarState = SVarStateAfter,
    goal_info_init(Context, GoalInfo),
    disj_list_to_goal(Disjuncts, GoalInfo, HLDSGoal).

:- pred accumulate_disjunct(loc_kind::in, prog_var_renaming::in,
    svar_state::in, goal::in,
    list(hlds_goal_svar_state)::in, list(hlds_goal_svar_state)::out,
    svar_store::in, svar_store::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

accumulate_disjunct(LocKind, Renaming, SVarStateBefore, Goal,
        !RevDisjStates, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
        SVarStateBefore, SVarStateAfterDisjunct,
        !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    DisjState = hlds_goal_svar_state(HLDSGoal, SVarStateAfterDisjunct),
    !:RevDisjStates = [DisjState | !.RevDisjStates].

%----------------------------------------------------------------------------%

:- inst goal_ite_expr for goal/0
    --->    if_then_else_expr(ground, ground, ground, ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_ite(loc_kind::in,
    goal::in(goal_ite_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_ite/16)).

transform_parse_tree_goal_to_hlds_ite(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = if_then_else_expr(Context, Vars0, StateVars0, Cond, Then, Else),
    BeforeSVarState = !.SVarState,
    rename_var_list(need_not_rename, Renaming, Vars0, Vars),
    rename_var_list(need_not_rename, Renaming, StateVars0, StateVars),
    svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
        BeforeSVarState, BeforeCondSVarState, !Specs),
    transform_parse_tree_goal_to_hlds(LocKind, Cond, Renaming, HLDSCond,
        BeforeCondSVarState, AfterCondSVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),
    transform_parse_tree_goal_to_hlds(LocKind, Then, Renaming, HLDSThen0,
        AfterCondSVarState, AfterThenSVarState0, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),
    module_info_get_globals(!.ModuleInfo, Globals),
    module_info_get_name(!.ModuleInfo, ModuleName),
    svar_finish_local_state_vars(Globals, ModuleName, StateVars,
        BeforeSVarState, AfterThenSVarState0, AfterThenSVarState),
    transform_parse_tree_goal_to_hlds(LocKind, Else, Renaming, HLDSElse0,
        BeforeSVarState, AfterElseSVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),
    svar_finish_if_then_else(Globals, ModuleName, LocKind, Context, StateVars,
        HLDSThen0, HLDSThen, HLDSElse0, HLDSElse,
        BeforeSVarState, AfterCondSVarState, AfterThenSVarState,
        AfterElseSVarState, !:SVarState, !VarSet, !SVarStore, !Specs),
    GoalExpr = if_then_else(Vars, HLDSCond, HLDSThen, HLDSElse),
    goal_info_init(Context, GoalInfo),
    HLDSGoal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%

:- inst goal_not_expr for goal/0
    --->    not_expr(ground, ground).

:- pred transform_parse_tree_goal_to_hlds_not(loc_kind::in,
    goal::in(goal_not_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_not/16)).

transform_parse_tree_goal_to_hlds_not(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = not_expr(Context, SubGoal),
    BeforeOutsideState = !.SVarState,
    transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming, HLDSSubGoal,
        !.SVarState, _, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    !:SVarState = BeforeOutsideState,
    GoalExpr = negation(HLDSSubGoal),
    goal_info_init(Context, GoalInfo),
    HLDSGoal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%

:- inst goal_equivalent_expr for goal/0
    --->    equivalent_expr(ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_equivalent(loc_kind::in,
    goal::in(goal_equivalent_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_equivalent/16)).

transform_parse_tree_goal_to_hlds_equivalent(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = equivalent_expr(Context, SubGoalA, SubGoalB),
    % `P <=> Q' is defined as `(P => Q), (Q => P)',
    % but that transformation must not be done until after quantification,
    % lest the duplication of the goals concerned affect the implicit
    % quantification of the variables inside them.
    SVarStateBefore = !.SVarState,
    transform_parse_tree_goal_to_hlds(LocKind, SubGoalA, Renaming,
        HLDSSubGoalA, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),
    transform_parse_tree_goal_to_hlds(LocKind, SubGoalB, Renaming,
        HLDSSubGoalB, !.SVarState, _, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),
    !:SVarState = SVarStateBefore,
    GoalExpr = shorthand(bi_implication(HLDSSubGoalA, HLDSSubGoalB)),
    goal_info_init(Context, GoalInfo),
    HLDSGoal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%

:- inst goal_quant_expr for goal/0
    --->    quant_expr(ground, ground, ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_quant(loc_kind::in,
    goal::in(goal_quant_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_quant/16)).

transform_parse_tree_goal_to_hlds_quant(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = quant_expr(QuantType, VarsKind, Context, Vars0, SubGoal),
    (
        QuantType = quant_all,
        % Convert
        %   `all [Vars0] SubGoal'
        % into
        %   `not (some [Vars0] (not SubGoal))'.
        %
        % The Renaming will be applied in the recursive call.
        TransformedGoal =
            not_expr(Context,
                quant_expr(quant_some, VarsKind, Context, Vars0,
                    not_expr(Context, SubGoal))),
        transform_parse_tree_goal_to_hlds(LocKind, TransformedGoal,
            Renaming, HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        QuantType = quant_some,
        rename_var_list(need_not_rename, Renaming, Vars0, Vars),
        (
            VarsKind = quant_ordinary_vars,
            transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
                HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            Reason = exist_quant(Vars)
        ;
            VarsKind = quant_state_vars,
            BeforeOutsideSVarState = !.SVarState,
            StateVars = Vars,
            svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
                BeforeOutsideSVarState, BeforeInsideSVarState, !Specs),
            transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
                HLDSSubGoal, BeforeInsideSVarState, AfterInsideSVarState,
                !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
            module_info_get_globals(!.ModuleInfo, Globals),
            module_info_get_name(!.ModuleInfo, ModuleName),
            svar_finish_local_state_vars(Globals, ModuleName, StateVars,
                BeforeOutsideSVarState, AfterInsideSVarState,
                AfterOutsideSVarState),
            !:SVarState = AfterOutsideSVarState,
            Reason = exist_quant([])
        ),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ).

%----------------------------------------------------------------------------%

:- inst goal_try_expr for goal/0
    --->    try_expr(ground, ground, ground, ground, ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_try(loc_kind::in,
    goal::in(goal_try_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_try/16)).

transform_parse_tree_goal_to_hlds_try(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = try_expr(Context, MaybeIO0, SubGoal0, Then0, MaybeElse0,
        Catches0, MaybeCatchAny0),
    (
        MaybeIO0 = yes(IOStateVar0),
        (
            MaybeElse0 = no,
            rename_var(need_not_rename, Renaming, IOStateVar0, IOStateVar),
            transform_try_expr_with_io(LocKind, IOStateVar0, IOStateVar,
                SubGoal0, Then0, Catches0, MaybeCatchAny0, Context,
                Renaming, HLDSGoal, !SVarState, !SVarStore,
                !VarSet, !ModuleInfo, !QualInfo, !Specs)
        ;
            MaybeElse0 = yes(_),
            Pieces = [words("Error: a"), quote("try"), words("goal"),
                words("with an"), quote("io"), words("parameter"),
                words("cannot have an"), quote("else"), words("part."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs],
            HLDSGoal = true_goal_with_context(Context)
        )
    ;
        MaybeIO0 = no,
        transform_try_expr_without_io(LocKind, SubGoal0, Then0, MaybeElse0,
            Catches0, MaybeCatchAny0, Context, Renaming, HLDSGoal,
            !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ).

    % Transform a try_expr which needs to perform I/O. The end result looks
    % like this:
    %
    %   magic_exception_result(TryResult),
    %   (
    %       TryResult = succeeded({}),
    %       some [] (
    %           !:IO = !.IO,
    %           Goal
    %       ),
    %       some [] ( Then )
    %   ;
    %       TryResult = exception(Excp),
    %       ExcpHandling
    %   )
    %
    % Unlike in the non-I/O case, we have to transform the three pieces Goal,
    % Then, ExcpHandling separately, then stitch them together into HLDS goals.
    % This is because we need to find out the variable for !.IO at the end of
    % Goal, before entering Then. The variable will be used in the later
    % post-transformation.
    %
:- pred transform_try_expr_with_io(loc_kind::in, svar::in, svar::in,
    goal::in, goal::in, list(catch_expr)::in, maybe(catch_any_expr)::in,
    prog_context::in, prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_try_expr_with_io(LocKind, IOStateVarUnrenamed, IOStateVar, Goal0,
        Then0, Catches0, MaybeCatchAny0, Context, Renaming, TryGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("TryResult", ResultVar, !VarSet),
    varset.new_var(ExcpVar, !VarSet),

    ResultVarTerm = variable(ResultVar, Context),
    ExcpVarTerm = variable(ExcpVar, Context),
    NullTupleTerm = functor(atom("{}"), [], Context),

    goal_info_init(Context, GoalInfo),

    % Make the call to magic_exception_result.
    CallMagicGoal = call_expr(Context, magic_exception_result_sym_name,
        [ResultVarTerm], purity_pure),
    transform_parse_tree_goal_to_hlds(LocKind, CallMagicGoal, Renaming,
        HLDSCallMagicGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),

    % Get the variable for !.IO before the (eventual) try_io call.
    lookup_dot_state_var(Context, IOStateVar, IOStateVarBefore,
        !VarSet, !SVarState, !Specs),

    SVarStateBeforeDisjunction = !.SVarState,

    % Build "TryResult = succeeded({})".
    ResultIsSucceededUnifyGoal = unify_expr(Context,
        ResultVarTerm,
        exception_functor("succeeded", NullTupleTerm, Context),
        purity_pure),
    transform_parse_tree_goal_to_hlds(LocKind, ResultIsSucceededUnifyGoal,
        Renaming, HLDSResultIsSucceededUnifyGoal, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),

    % Build "some [] ( !:IO = !.IO, Goal )".
    %
    % The explicit unification avoids a degenerate case where Goal doesn't bind
    % the final !:IO variable, which would lead to trouble later when we move
    % Goal into its own lambda.
    IOUnify = unify_expr(Context,
        functor(atom("!:"), [variable(IOStateVarUnrenamed, Context)], Context),
        functor(atom("!."), [variable(IOStateVarUnrenamed, Context)], Context),
        purity_pure),
    ScopedGoal = quant_expr(quant_some, quant_ordinary_vars, Context, [],
        conj_expr(Context, IOUnify, [Goal0])),
    transform_parse_tree_goal_to_hlds(LocKind, ScopedGoal, Renaming,
        HLDSScopedGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),

    % Remember the variable for !.IO after the (eventual) try_io Goal.
    lookup_dot_state_var(Context, IOStateVar, IOStateVarAfter,
        !VarSet, !SVarState, !Specs),

    % Build "some [] ( Then )".
    ScopedThenGoal = quant_expr(quant_some, quant_ordinary_vars, Context, [],
        Then0),
    transform_parse_tree_goal_to_hlds(LocKind, ScopedThenGoal, Renaming,
        HLDSScopedThenGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),

    % Build:
    %
    %   TryResult = succeeded({}),
    %   some [] ( !:IO = !.IO, Goal ),
    %   some [] ( Then )
    %
    conj_list_to_goal(
        [HLDSResultIsSucceededUnifyGoal, HLDSScopedGoal, HLDSScopedThenGoal],
        GoalInfo, HLDSResultIsSucceededDisjunctGoal),

    SVarStateAfterResultIsSucceededDisjunct = !.SVarState,
    !:SVarState = SVarStateBeforeDisjunction,

    % Build the disjunct for "TryResult = exception(Excp), ...".
    make_exception_handling_disjunct(ResultVarTerm, ExcpVarTerm, Catches0,
        MaybeCatchAny0, Context, ResultIsExceptionDisjunctGoal),
    transform_parse_tree_goal_to_hlds(LocKind, ResultIsExceptionDisjunctGoal,
        Renaming, HLDSResultIsExceptionDisjunctGoal, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),

    SVarStateAfterResultIsExceptionDisjunct = !.SVarState,

    % Get the disjuncts.
    DisjunctSVarStates = [
        hlds_goal_svar_state(HLDSResultIsSucceededDisjunctGoal,
            SVarStateAfterResultIsSucceededDisjunct),
        hlds_goal_svar_state(HLDSResultIsExceptionDisjunctGoal,
            SVarStateAfterResultIsExceptionDisjunct)
    ],
    svar_finish_disjunction(DisjunctSVarStates, HLDSDisjuncts,
        !VarSet, SVarStateBeforeDisjunction, !:SVarState, !SVarStore),
    disj_list_to_goal(HLDSDisjuncts, GoalInfo, HLDSDisjunction),

    % Build the call to magic_exception_result followed by the disjunction.
    conj_list_to_goal([HLDSCallMagicGoal, HLDSDisjunction], GoalInfo,
        CallMagicThenDisjunction),

    IOStateVars = try_io_state_vars(IOStateVarBefore, IOStateVarAfter),
    GoalExpr = shorthand(try_goal(yes(IOStateVars), ResultVar,
        CallMagicThenDisjunction)),
    TryGoal = hlds_goal(GoalExpr, GoalInfo).

    % Transform a try_expr which does not need I/O.
    %
    % If the try goal has an else part, the end result looks like:
    %
    %   magic_exception_result(TryResult),
    %   (
    %       TryResult = succeeded({}),
    %       ( if Goal then
    %           Then
    %       else
    %           Else
    %       )
    %   ;
    %       TryResult = exception(Excp),
    %       ExcpHandling
    %   )
    %
    % If the try goal does not have an else part, the end result looks like:
    %
    %   magic_exception_result(TryResult),
    %   (
    %       TryResult = succeeded({}),
    %       some [] ( Goal ),
    %       some [] ( Then )
    %   ;
    %       TryResult = exception(Excp),
    %       ExcpHandling
    %   )
    %
:- pred transform_try_expr_without_io(loc_kind::in, goal::in, goal::in,
    maybe(goal)::in, list(catch_expr)::in, maybe(catch_any_expr)::in,
    prog_context::in, prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_try_expr_without_io(LocKind, SubGoal, ThenGoal, MaybeElseGoal,
        Catches, MaybeCatchAny, Context, Renaming, TryGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("TryResult", ResultVar, !VarSet),
    varset.new_var(ExcpVar, !VarSet),

    ResultVarTerm = variable(ResultVar, Context),
    ExcpVarTerm = variable(ExcpVar, Context),
    NullTupleTerm = functor(atom("{}"), [], Context),

    goal_info_init(Context, GoalInfo),

    % Build the call to magic_exception_result.
    CallMagicGoal = call_expr(Context, magic_exception_result_sym_name,
        [ResultVarTerm], purity_pure),

    % Build "TryResult = succeeded({}), ..." disjunct.
    ResultIsSucceededUnifyGoal = unify_expr(Context,
        ResultVarTerm,
        exception_functor("succeeded", NullTupleTerm, Context),
        purity_pure),
    (
        MaybeElseGoal = yes(ElseGoal),
        SucceededSubGoal = if_then_else_expr(Context, [], [],
            SubGoal, ThenGoal, ElseGoal)
    ;
        MaybeElseGoal = no,
        SucceededSubGoal =
            conj_expr(Context,
                quant_expr(quant_some, quant_ordinary_vars, Context, [],
                    SubGoal),
                [quant_expr(quant_some, quant_ordinary_vars, Context, [],
                    ThenGoal)]
            )
    ),
    ResultIsSucceededDisjunctGoal =
        conj_expr(Context, ResultIsSucceededUnifyGoal, [SucceededSubGoal]),

    % Build the disjunct for "TryResult = exception(Excp), ...".
    make_exception_handling_disjunct(ResultVarTerm, ExcpVarTerm,
        Catches, MaybeCatchAny, Context, ResultIsExceptionDisjunctGoal),

    % Build the call followed by the disjunction.
    CallMagicThenDisjunctionGoal =
        conj_expr(Context,
            CallMagicGoal,
            [disj_expr(Context,
                ResultIsSucceededDisjunctGoal,
                ResultIsExceptionDisjunctGoal,
                []
            )]
        ),
    transform_parse_tree_goal_to_hlds(LocKind, CallMagicThenDisjunctionGoal,
        Renaming, HLDSCallMagicThenDisjunctionGoal, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),

    ShortHand = try_goal(no, ResultVar, HLDSCallMagicThenDisjunctionGoal),
    GoalExpr = shorthand(ShortHand),
    TryGoal = hlds_goal(GoalExpr, GoalInfo).

:- pred make_exception_handling_disjunct(prog_term::in, prog_term::in,
    list(catch_expr)::in, maybe(catch_any_expr)::in, prog_context::in,
    goal::out) is det.

make_exception_handling_disjunct(ResultVarTerm, ExcpVarTerm, Catches,
        MaybeCatchAny, Context, Goal) :-
    ResultIsExceptionUnify = unify_expr(Context,
        ResultVarTerm,
        exception_functor("exception", ExcpVarTerm, Context),
        purity_pure),
    make_catch_ite_chain(ResultVarTerm, ExcpVarTerm, Catches, MaybeCatchAny,
        CatchChain),
    Goal = conj_expr(Context, ResultIsExceptionUnify, [CatchChain]).

:- pred make_catch_ite_chain(prog_term::in, prog_term::in,
    list(catch_expr)::in, maybe(catch_any_expr)::in, goal::out) is det.

make_catch_ite_chain(ResultVarTerm, ExcpVarTerm, Catches, MaybeCatchAny,
        Goal) :-
    (
        Catches = [catch_expr(FirstPattern, FirstGoal) | RestCatches],
        make_catch_ite_chain(ResultVarTerm, ExcpVarTerm, RestCatches,
            MaybeCatchAny, ElseGoal),
        make_catch_pattern_unify_goal(FirstPattern, ExcpVarTerm,
            FirstPatternGoal),
        Goal = if_then_else_expr(get_term_context(FirstPattern), [], [],
            FirstPatternGoal, FirstGoal, ElseGoal)
    ;
        Catches = [],
        (
            MaybeCatchAny = yes(catch_any_expr(CatchAnyVar, CatchAnyGoal)),
            % With a catch_any part, end the if-then-else chain with:
            %   CatchAnyVar = exc_univ_value(Excp),
            %   CatchAnyGoal
            Context = get_goal_context(CatchAnyGoal),
            GetUnivValue = unify_expr(Context,
                variable(CatchAnyVar, Context),
                exception_functor("exc_univ_value", ExcpVarTerm, Context),
                purity_pure),
            Goal = conj_expr(Context, GetUnivValue, [CatchAnyGoal])
        ;
            MaybeCatchAny = no,
            % Without a catch_any part, end the if-then-else chain
            % by rethrowing the exception.
            Rethrow = qualified(mercury_exception_module, "rethrow"),
            Goal = call_expr( get_term_context(ExcpVarTerm),
                Rethrow, [ResultVarTerm], purity_pure)
        )
    ).

:- pred make_catch_pattern_unify_goal(prog_term::in, prog_term::in,
    goal::out) is det.

make_catch_pattern_unify_goal(CatchPatternTerm, ExcpVarTerm, Goal) :-
    Goal = call_expr(get_term_context(CatchPatternTerm),
        qualified(mercury_exception_module, "exc_univ_to_type"),
        [ExcpVarTerm, CatchPatternTerm], purity_pure).

:- func magic_exception_result_sym_name = sym_name.

magic_exception_result_sym_name =
    qualified(mercury_exception_module, "magic_exception_result").

:- func exception_functor(string, prog_term, term.context) = prog_term.

exception_functor(Atom, Arg, Context) = Term :-
    SymName = qualified(mercury_exception_module, Atom),
    construct_qualified_term_with_context(SymName, [Arg], Context, Term).

%----------------------------------------------------------------------------%

:- inst goal_atomic_expr for goal/0
    --->    atomic_expr(ground, ground, ground, ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_atomic(loc_kind::in,
    goal::in(goal_atomic_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_atomic/16)).

transform_parse_tree_goal_to_hlds_atomic(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = atomic_expr(Context, Outer0, Inner0, MaybeOutputVars0,
        MainGoal, OrElseGoals),
    (
        MaybeOutputVars0 = no,
        MaybeOutputVars = no
    ;
        MaybeOutputVars0 = yes(OutputVars0),
        rename_var_list(need_not_rename, Renaming,
            OutputVars0, OutputVars),
        MaybeOutputVars = yes(OutputVars)
    ),
    (
        Outer0 = atomic_state_var(OuterStateVar0),
        rename_var(need_not_rename, Renaming,
            OuterStateVar0, OuterStateVar),
        svar_start_outer_atomic_scope(Context, OuterStateVar,
            OuterDI, OuterUO, OuterScopeInfo, !SVarState, !VarSet, !Specs),
        MaybeOuterScopeInfo = yes(OuterScopeInfo),
        Outer = atomic_interface_vars(OuterDI, OuterUO)
    ;
        Outer0 = atomic_var_pair(OuterDI0, OuterUO0),
        rename_var(need_not_rename, Renaming, OuterDI0, OuterDI),
        rename_var(need_not_rename, Renaming, OuterUO0, OuterUO),
        Outer = atomic_interface_vars(OuterDI, OuterUO),
        MaybeOuterScopeInfo = no
    ),
    (
        Inner0 = atomic_state_var(InnerStateVar0),
        rename_var(need_not_rename, Renaming,
            InnerStateVar0, InnerStateVar),
        svar_start_inner_atomic_scope(Context, InnerStateVar,
            InnerScopeInfo, !SVarState, !VarSet, !Specs),
        MaybeInnerScopeInfo = yes(InnerScopeInfo)
    ;
        Inner0 = atomic_var_pair(_InnerDI0, _InnerUO0),
        MaybeInnerScopeInfo = no
    ),
    BeforeDisjSVarState = !.SVarState,
    transform_parse_tree_goal_to_hlds(LocKind, MainGoal, Renaming,
        HLDSMainGoal0, BeforeDisjSVarState, AfterMainSVarState,
        !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    MainDisjState =
        hlds_goal_svar_state(HLDSMainGoal0, AfterMainSVarState),
    transform_orelse_goals(LocKind, OrElseGoals, Renaming,
        OrElseDisjStates, BeforeDisjSVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),
    AllDisjStates = [MainDisjState | OrElseDisjStates],
    svar_finish_disjunction(AllDisjStates, HLDSGoals, !VarSet,
        BeforeDisjSVarState, !:SVarState, !SVarStore),
    (
        HLDSGoals = [HLDSMainGoal | HLDSOrElseGoals]
    ;
        HLDSGoals = [],
        unexpected($pred, "atomic HLDSGoals = []")
    ),
    (
        Inner0 = atomic_state_var(_),
        (
            MaybeInnerScopeInfo = yes(InnerScopeInfo2),
            svar_finish_inner_atomic_scope(Context, InnerScopeInfo2,
                InnerDI, InnerUO, !SVarState, !VarSet, !Specs),
            Inner = atomic_interface_vars(InnerDI, InnerUO)
        ;
            MaybeInnerScopeInfo = no,
            unexpected($pred, "MaybeInnerScopeInfo = no")
        )
    ;
        Inner0 = atomic_var_pair(InnerDI0, InnerUO0),
        rename_var(need_not_rename, Renaming, InnerDI0, InnerDI),
        rename_var(need_not_rename, Renaming, InnerUO0, InnerUO),
        Inner = atomic_interface_vars(InnerDI, InnerUO)
    ),
    (
        MaybeOuterScopeInfo = yes(OuterScopeInfo2),
        svar_finish_outer_atomic_scope(OuterScopeInfo2, !SVarState)
    ;
        MaybeOuterScopeInfo = no
    ),
    ShortHand = atomic_goal(unknown_atomic_goal_type, Outer, Inner,
        MaybeOutputVars, HLDSMainGoal, HLDSOrElseGoals, []),
    GoalExpr = shorthand(ShortHand),
    goal_info_init(Context, GoalInfo),
    HLDSGoal = hlds_goal(GoalExpr, GoalInfo),
    trace [compiletime(flag("atomic_scope_syntax")), io(!IO)] (
        get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
        module_info_get_globals(!.ModuleInfo, Globals),
        OutInfo = init_hlds_out_info(Globals, output_debug),
        io.write_string(DebugStream, "atomic:\n", !IO),
        write_goal_nl(OutInfo, DebugStream, !.ModuleInfo, vns_varset(!.VarSet),
            print_name_and_num, 0, "\n", HLDSGoal, !IO)
    ).

:- pred transform_orelse_goals(loc_kind::in, list(goal)::in,
    prog_var_renaming::in, list(hlds_goal_svar_state)::out,
    svar_state::in, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_orelse_goals(_, [], _, [], _SVarStateBefore, !SVarState,
        !VarSet, !ModuleInfo, !QualInfo, !Specs).
transform_orelse_goals(LocKind, [Goal | Goals], Renaming,
        [DisjState | DisjStates], SVarStateBefore, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
        SVarStateBefore, SVarStateAfterDisjunct, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),
    DisjState = hlds_goal_svar_state(HLDSGoal, SVarStateAfterDisjunct),
    transform_orelse_goals(LocKind, Goals, Renaming, DisjStates,
        SVarStateBefore, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

%----------------------------------------------------------------------------%

:- inst goal_disable_warnings_expr for goal/0
    --->    disable_warnings_expr(ground, ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_disable_warnings(loc_kind::in,
    goal::in(goal_disable_warnings_expr), prog_var_renaming::in,
    hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_disable_warnings/16)).

transform_parse_tree_goal_to_hlds_disable_warnings(LocKind, Goal,
        Renaming, HLDSGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    Goal = disable_warnings_expr(Context, HeadWarning, TailWarnings,
        SubGoal),
    ( if
        ( HeadWarning = goal_warning_occurs_check
        ; list.member(goal_warning_occurs_check, TailWarnings)
        )
    then
        module_info_get_globals(!.ModuleInfo, Globals0),
        globals.lookup_bool_option(Globals0,
            warn_suspected_occurs_check_failure, WarnOccursCheck0),
        globals.set_option(warn_suspected_occurs_check_failure,
            bool(no), Globals0, Globals1),
        module_info_set_globals(Globals1, !ModuleInfo),

        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),

        module_info_get_globals(!.ModuleInfo, Globals2),
        globals.set_option(warn_suspected_occurs_check_failure,
            bool(WarnOccursCheck0), Globals2, Globals3),
        module_info_set_globals(Globals3, !ModuleInfo)
    else
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ),
    GoalExpr = scope(disable_warnings(HeadWarning, TailWarnings), HLDSSubGoal),
    goal_info_init(Context, GoalInfo),
    HLDSGoal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%

:- inst goal_trace_expr for goal/0
    --->    trace_expr(ground, ground, ground, ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_trace(loc_kind::in,
    goal::in(goal_trace_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_trace/16)).

transform_parse_tree_goal_to_hlds_trace(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = trace_expr(Context, MaybeCompileTime, MaybeRunTime, MaybeIO0,
        Mutables0, SubGoal0),
    list.map4(extract_trace_mutable_var(Context, !.VarSet, Renaming),
        Mutables0, MutableHLDSs, MutableStateVars,
        MutableGetGoals, MutableSetGoals),
    (
        MaybeIO0 = yes(IOStateVar0),
        extract_trace_io_var(Context, !.VarSet, Renaming,
            IOStateVar0, IOStateVar, IOStateVarName, IOGetGoal, IOSetGoal),
        MaybeIOHLDS = yes(IOStateVarName),
        StateVars = [IOStateVar | MutableStateVars],
        GetGoals = [IOGetGoal | MutableGetGoals],
        SetGoals = [IOSetGoal | MutableSetGoals]
    ;
        MaybeIO0 = no,
        MaybeIOHLDS = no,
        StateVars = MutableStateVars,
        GetGoals = MutableGetGoals,
        SetGoals = MutableSetGoals
    ),
    SubGoal1 = goal_list_to_conj(Context, GetGoals ++ [SubGoal0] ++ SetGoals),
    BeforeSVarState = !.SVarState,
    svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
        BeforeSVarState, BeforeInsideSVarState, !Specs),
    transform_parse_tree_goal_to_hlds(LocKind, SubGoal1, Renaming,
        HLDSSubGoal, BeforeInsideSVarState, AfterInsideSVarState,
        !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    module_info_get_globals(!.ModuleInfo, Globals),
    module_info_get_name(!.ModuleInfo, ModuleName),
    svar_finish_local_state_vars(Globals, ModuleName, StateVars,
        BeforeSVarState, AfterInsideSVarState, AfterSVarState),
    !:SVarState = AfterSVarState,
    qual_info_set_found_trace_goal(yes, !QualInfo),
    % The QuantVars field is a lie, but a white lie.
    Reason = trace_goal(MaybeCompileTime, MaybeRunTime, MaybeIOHLDS,
        MutableHLDSs, []),
    GoalExpr = scope(Reason, HLDSSubGoal),
    goal_info_init(Context, GoalInfo),
    HLDSGoal = hlds_goal(GoalExpr, GoalInfo).

:- pred extract_trace_mutable_var(prog_context::in, prog_varset::in,
    prog_var_renaming::in, trace_mutable_var::in, trace_mutable_var_hlds::out,
    prog_var::out, goal::out, goal::out) is det.

extract_trace_mutable_var(Context, VarSet, Renaming, Mutable, MutableHLDS,
        StateVar, GetGoal, SetGoal) :-
    Mutable = trace_mutable_var(MutableName, StateVar0),
    rename_var(need_not_rename, Renaming, StateVar0, StateVar),
    varset.lookup_name(VarSet, StateVar, StateVarName),
    MutableHLDS = trace_mutable_var_hlds(MutableName, StateVarName),
    GetPredName = unqualified("get_" ++ MutableName),
    SetPredName = unqualified("set_" ++ MutableName),
    % We create the get and set goals with the original, unrenamed version
    % of the state variable, because our caller will rename the whole goal
    % in the scope of the trace expression, and the get and set goals
    % we construct here will go into that scope.
    SetVar = functor(atom("!:"), [variable(StateVar0, Context)], Context),
    UseVar = functor(atom("!."), [variable(StateVar0, Context)], Context),
    GetPurity = purity_semipure,
    SetPurity = purity_impure,
    GetGoal = call_expr(Context, GetPredName, [SetVar], GetPurity),
    SetGoal = call_expr(Context, SetPredName, [UseVar], SetPurity).

:- pred extract_trace_io_var(prog_context::in, prog_varset::in,
    prog_var_renaming::in, prog_var::in, prog_var::out, string::out,
    goal::out, goal::out) is det.

extract_trace_io_var(Context, VarSet, Renaming, StateVar0, StateVar,
        StateVarName, GetGoal, SetGoal) :-
    rename_var(need_not_rename, Renaming, StateVar0, StateVar),
    varset.lookup_name(VarSet, StateVar, StateVarName),
    IO = mercury_io_module,
    GetPredName = qualified(IO, "unsafe_get_io_state"),
    SetPredName = qualified(IO, "unsafe_set_io_state"),
    % We create the get and set goals with the original, unrenamed version
    % of the state variable, because our caller will rename the whole goal
    % in the scope of the trace expression, and the get and set goals
    % we construct here will go into that scope.
    SetVar = functor(atom("!:"), [variable(StateVar0, Context)], Context),
    UseVar = functor(atom("!."), [variable(StateVar0, Context)], Context),
    GetPurity = purity_semipure,
    SetPurity = purity_impure,
    GetGoal = call_expr(Context, GetPredName, [SetVar], GetPurity),
    SetGoal = call_expr(Context, SetPredName, [UseVar], SetPurity).

%----------------------------------------------------------------------------%

:- inst goal_event_expr for goal/0
    --->    event_expr(ground, ground, ground).

:- pred transform_parse_tree_goal_to_hlds_event(loc_kind::in,
    goal::in(goal_event_expr), prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(transform_parse_tree_goal_to_hlds_event/16)).

transform_parse_tree_goal_to_hlds_event(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    Goal = event_expr(Context, EventName, ArgTerms0),
    expand_bang_state_pairs_in_terms(ArgTerms0, ArgTerms1),
    rename_vars_in_term_list(need_not_rename, Renaming,
        ArgTerms1, ArgTerms),
    make_fresh_arg_vars_subst_svars(ArgTerms, HeadVars, HeadVarsArgTerms,
        !VarSet, !SVarState, !Specs),
    list.length(HeadVars, Arity),
    list.duplicate(Arity, in_mode, Modes),
    Details = event_call(EventName),
    GoalExpr0 = generic_call(Details, HeadVars, Modes, arg_reg_types_unset,
        detism_det),
    goal_info_init(Context, GoalInfo),
    HLDSGoal0 = hlds_goal(GoalExpr0, GoalInfo),
    CallId = generic_call_id(gcid_event_call(EventName)),
    insert_arg_unifications(HeadVarsArgTerms, Context, ac_call(CallId),
        HLDSGoal0, HLDSGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),
    svar_finish_atomic_goal(LocKind, !SVarState).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- pred rename_and_maybe_expand_dot_var(prog_context::in,
    must_rename::in, prog_var_renaming::in,
    plain_or_dot_var::in, prog_var::out,
    svar_state::in, svar_state::out, prog_varset::in, prog_varset::out,
    list(error_spec)::in, list(error_spec)::out) is det.

rename_and_maybe_expand_dot_var(Context, MustRename, Renaming, PODVar0, Var,
        !SVarState, !VarSet, !Specs) :-
    (
        PODVar0 = podv_plain(Var0),
        rename_var(MustRename, Renaming, Var0, Var)
    ;
        PODVar0 = podv_dot(DotVar0),
        rename_var(MustRename, Renaming, DotVar0, DotVar),
        lookup_dot_state_var(Context, DotVar, Var, !VarSet, !SVarState, !Specs)
    ).

%----------------------------------------------------------------------------%

:- pred transform_promise_eqv_goal(loc_kind::in,
    list(prog_var)::in, list(prog_var)::in,
    list(prog_var)::in, list(prog_var)::in,
    prog_context::in, prog_var_renaming::in, list(prog_var)::out,
    goal::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_promise_eqv_goal(LocKind, Vars0, StateVars0, DotSVars0, ColonSVars0,
        Context, Renaming, QuantVars, Goal, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    rename_var_list(need_not_rename, Renaming, Vars0, Vars),
    rename_var_list(need_not_rename, Renaming, StateVars0, StateVars1),
    rename_var_list(need_not_rename, Renaming, DotSVars0, DotSVars1),
    rename_var_list(need_not_rename, Renaming, ColonSVars0, ColonSVars1),
    list.map_foldl3(lookup_dot_state_var(Context), StateVars1, OldStateVars,
        !VarSet, !SVarState, !Specs),
    list.map_foldl3(lookup_dot_state_var(Context), DotSVars1, DotSVars,
        !VarSet, !SVarState, !Specs),
    transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    list.map_foldl3(lookup_dot_state_var(Context), StateVars1, NewStateVars,
        !VarSet, !SVarState, !Specs),
    list.map_foldl3(lookup_dot_state_var(Context), ColonSVars1, ColonSVars,
        !VarSet, !SVarState, !Specs),
    QuantVars = Vars ++ OldStateVars ++ NewStateVars ++ DotSVars ++ ColonSVars.

%----------------------------------------------------------------------------%

    % Produce an invalid goal.
    %
:- pred invalid_goal(string::in, list(prog_term)::in, hlds_goal_info::in,
    hlds_goal::out, prog_varset::in, prog_varset::out,
    svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

invalid_goal(UpdateStr, Args0, GoalInfo, Goal, !VarSet, !SVarState, !Specs) :-
    make_fresh_arg_vars_subst_svars(Args0, HeadVars, _HeadVarsArgs0,
        !VarSet, !SVarState, !Specs),
    MaybeUnifyContext = no,
    GoalExpr = plain_call(invalid_pred_id, invalid_proc_id, HeadVars,
        not_builtin, MaybeUnifyContext, unqualified(UpdateStr)),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.goal_expr_to_goal.
%----------------------------------------------------------------------------%
