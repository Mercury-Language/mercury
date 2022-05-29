%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module produces definitions for the predicates that implement
% instance methods.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.instance_method_clauses.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module term.

    % Given the instance_proc_def for a predicate or function from an instance
    % declaration, produce the clauses_info for that instance_proc_def.
    %
:- pred produce_instance_method_clauses(instance_proc_def::in,
    pred_or_func::in, list(mer_type)::in, pred_markers::in, term.context::in,
    instance_status::in, clauses_info::out, tvarset::in, tvarset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.state_var.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.

:- import_module map.
:- import_module require.
:- import_module set.
:- import_module varset.

%-----------------------------------------------------------------------------%

produce_instance_method_clauses(InstanceProcDefn, PredOrFunc, ArgTypes,
        Markers, Context, InstanceStatus, ClausesInfo,
        !TVarSet, !ModuleInfo, !QualInfo, !Specs) :-
    PredFormArity = arg_list_arity(ArgTypes),
    (
        % Handle the `pred(<MethodName>/<Arity>) is <ImplName>' syntax.
        InstanceProcDefn = instance_proc_def_name(InstancePredName),
        % Add the body of the introduced pred.
        % First the goal info, ...
        PredFormArity = pred_form_arity(PredFormArityInt),
        varset.init(VarSet0),
        make_n_fresh_vars("HeadVar__", PredFormArityInt, HeadVars,
            VarSet0, VarSet),
        set_of_var.list_to_set(HeadVars, NonLocals),
        ( if check_marker(Markers, marker_is_impure) then
            Purity = purity_impure
        else if check_marker(Markers, marker_is_semipure) then
            Purity = purity_semipure
        else
            Purity = purity_pure
        ),
        instmap_delta_init_unreachable(DummyInstMapDelta),
        DummyDetism = detism_erroneous,
        goal_info_init(NonLocals, DummyInstMapDelta, DummyDetism, Purity,
            Context, GoalInfo),
        % ... and then the goal itself.
        construct_and_record_pred_or_func_call(invalid_pred_id, PredOrFunc,
            InstancePredName, HeadVars, GoalInfo, IntroducedGoal, !QualInfo),
        IntroducedClause = clause(all_modes, IntroducedGoal, impl_lang_mercury,
            Context, []),

        vartypes_from_corresponding_lists(HeadVars, ArgTypes,
            ExplicitVarTypes),
        init_var_table(VarTable),
        rtti_varmaps_init(RttiVarMaps),
        map.init(TVarNameMap),
        HeadVarVec = proc_arg_vector_init(PredOrFunc, HeadVars),
        set_clause_list([IntroducedClause], ClausesRep),
        ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
            VarTable, RttiVarMaps, TVarNameMap, HeadVarVec, ClausesRep,
            init_clause_item_numbers_comp_gen,
            no_foreign_lang_clauses, no_clause_syntax_errors)
    ;
        % Handle the arbitrary clauses syntax.
        InstanceProcDefn = instance_proc_def_clauses(InstanceClauses),
        % XXX CIT_TYPES: should be cit_types(ArgTypes)
        clauses_info_init(PredOrFunc, cit_no_types(PredFormArity),
            init_clause_item_numbers_comp_gen, ClausesInfo0),
        list.foldl5(
            produce_instance_method_clause(PredOrFunc, Context,
                InstanceStatus),
            InstanceClauses, !TVarSet, !ModuleInfo, !QualInfo,
            ClausesInfo0, ClausesInfo, !Specs)
    ).

:- pred produce_instance_method_clause(pred_or_func::in,
    prog_context::in, instance_status::in, item_clause_info::in,
    tvarset::in, tvarset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, clauses_info::in, clauses_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

produce_instance_method_clause(PredOrFunc, Context, InstanceStatus,
        InstanceClause, TVarSet0, TVarSet, !ModuleInfo, !QualInfo,
        !ClausesInfo, !Specs) :-
    InstanceClause = item_clause_info(ClausePredOrFunc, PredSymName,
        HeadTerms0, ClauseVarSet, MaybeBodyGoal, _ClauseContext, _SeqNum),
    % XXX Can this ever fail? If yes, we should generate an error message
    % instead of aborting.
    expect(unify(PredOrFunc, ClausePredOrFunc), $pred, "PredOrFunc mismatch"),
    ( if
        illegal_state_var_func_result(PredOrFunc, HeadTerms0, StateVar,
            StateVarContext)
    then
        TVarSet = TVarSet0,
        report_illegal_func_svar_result(StateVarContext, ClauseVarSet,
            StateVar, !Specs),
        !:Specs = get_any_errors_warnings2(MaybeBodyGoal) ++ !.Specs
    else
        (
            MaybeBodyGoal = error2(BodyGoalSpecs),
            TVarSet = TVarSet0,
            !:Specs = BodyGoalSpecs ++ !.Specs
        ;
            MaybeBodyGoal = ok2(BodyGoal, BodyGoalWarningSpecs),
            !:Specs = BodyGoalWarningSpecs ++ !.Specs,
            expand_bang_state_pairs_in_terms(HeadTerms0, HeadTerms),
            % AllProcIds is only used when the predicate has foreign procs,
            % which the instance method pred should not have, so this
            % dummy value should be ok.
            AllProcIds = [],
            % XXX STATUS
            InstanceStatus = instance_status(OldImportStatus),
            PredStatus = pred_status(OldImportStatus),
            clauses_info_add_clause(all_modes, AllProcIds, PredStatus,
                clause_not_for_promise, PredOrFunc, PredSymName, HeadTerms,
                Context, item_no_seq_num, Warnings,
                BodyGoal, Goal, ClauseVarSet, VarSet, TVarSet0, TVarSet,
                !ClausesInfo, !ModuleInfo, !QualInfo, !Specs),

            PredFormArity = arg_list_arity(HeadTerms),
            PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName,
                PredFormArity),
            % Warn about singleton variables.
            warn_singletons(!.ModuleInfo, PFSymNameArity, VarSet, Goal,
                !Specs),
            % Warn about variables with overlapping scopes.
            add_quant_warnings(PFSymNameArity, VarSet, Warnings, !Specs)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.instance_method_clauses.
%-----------------------------------------------------------------------------%
