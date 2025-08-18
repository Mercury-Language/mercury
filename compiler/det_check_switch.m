%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2013-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: det_check_goal.m.
% Author: zs.
%
% This module handles reporting of errors and warnings about switches,
% such as not being on the right variable, missing some cases, or their arms
% not having the right determinism.
%
%---------------------------------------------------------------------------%

:- module check_hlds.det_check_switch.
:- interface.

:- import_module check_hlds.det_util.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % There are two reasons why we may want to report that a switch
    % is incomplete:
    %
    % - because the switch is wrapped in a require_complete_switch scope, and
    % - because the --inform-incomplete-switch option is set.
    %
    % This type says whether the --inform-incomplete-switch option is set.
    %
:- type maybe_inform_incomplete_switches
    --->    do_not_inform_incomplete_switches
    ;       inform_incomplete_switches.

    % This type says which of the above two reasons causes us to report
    % the given incomplete switch.
    %
:- type why_report_incomplete_switch
    --->    switch_required_to_be_complete
    ;       inform_incomplete_switch_option.

    % It is possible for *both* reasons to apply to the same incomplete switch.
    % In such cases, we want to generate only the error required by the scope,
    % and not the information message that the option calls for. We process
    % procedure bodies top down, so we process the scope goal before
    % the switch it wraps, so when we generate an error report for an
    % incomplete switch, we pass along its details when we process the
    % goal inside the scope (which may either be the incomplete switch,
    % or a conjunction of feature_lifted_by_cse deconstruction unifications
    % followed by the incomplete switch).
    %
:- type reported_switch
    --->    reported_switch(
                prog_context,
                prog_var,
                list(case)
            ).

    % Do we need to check switches to see whether the cons_ids in their arms
    % follow the order of those cons_ids in their type definition?
:- type maybe_req_arms_in_type_order
    --->    no_req_arms_in_type_order
    ;       req_arms_in_type_order.

:- type reqscope_params
    --->    reqscope_params(
                maybe_inform_incomplete_switches,
                maybe_req_arms_in_type_order
            ).

    % Check that the switches in all require_complete_switch scopes are
    % actually complete. If they are not, add an error message to !DetInfo.
    %
    % If IIS = inform_incomplete_switches, do this for *all* switches.
    %
:- pred reqscope_check_goal(reqscope_params::in, instmap::in,
    maybe(reported_switch)::in, list(switch_context)::in, hlds_goal::in,
    det_info::in, det_info::out) is det.

%---------------------------------------------------------------------------%

    % Given the list of conjuncts in a switch arm, find the unifications that
    % unify the switched-on variable or its synonyms with the arm's cons_ids.
    % The reason why we look for this is to get access to the argument
    % variables of that unification, in case code inside the arm has errors
    % in switches on those arguments. We don't collect argument variables
    % from unifications in which they are all local, since in that case
    % there is no chance of that happening, which means that printing
    % the argument variables in that case would not be helpful, and
    % would instead be only clutter.
    %
:- pred find_switch_var_matches(list(hlds_goal)::in, list(prog_var)::in,
    cons_id::in, list(cons_id)::in,
    switch_match::out, list(switch_match)::out) is det.

%---------------------------------------------------------------------------%

:- type missing_cons_id_info
    --->    missing_cons_id_info(
                % The number of cons_ids that the switch variable could
                % be bound to on entry to the switch. May be the number of
                % function symbols in the variable's type, or it may be
                % the number of cons_ids that appear in the variable's
                % initial bound(...) inst.
                int,

                % The number of these cons_ids that do NOT occur
                % in any of the cases.
                int,

                % The diagnostic listing the missing cons_ids.
                %
                % If there are more missing cons_ids than the
                % specified limit, we return two different versions
                % of the error message fragment. The first is the version
                % to use if the user doesn't want verbose errors,
                % the second is the version to use if he/she does want them.
                %
                % If the number of missing cons_ids is below the limit,
                % or if the caller did not specify the limit,
                % the second list will be empty.
                list(format_piece),
                list(format_piece)
            ).

:- pred find_missing_cons_ids(det_info::in, maybe(int)::in, instmap::in,
    list(switch_context)::in, prog_var::in, list(case)::in,
    list(format_piece)::out, string::out,
    maybe(missing_cons_id_info)::out) is det.

%---------------------------------------------------------------------------%

:- type switch_context
    --->    switch_context(
                % The variable being switched on.
                prog_var,

                % The match info for the first cons_id of this case.
                switch_match,

                % The match info for the other cons_ids of this case.
                list(switch_match)
            ).

    % A switch arm is for one or more cons_ids. A switch match can record,
    % for one of these cons_ids, the variables on the right hand side of
    % the unification that told switch detection that this disjunction
    % can succeed only if the switched-on variable is bound to one of these
    % cons_ids. For example, in a switch on X, if the switch arm is
    % for the cons_id f/2, this means that the switch arm should have
    % a unification of the form X = f(A, B). Likewise, if the switch arm
    % is for more than one cons_id, such as f/2 and g/1, then the switch arm
    % should contain a disjunction such as ( X = f(A, B) ; X = g(C) ).
    % The switch match data type records the argument variables of these
    % unifications PROVIDED that some of them are visible from the outside,
    % which means that later error messages (e.g. about missing arms in
    % switches on them) can refer to them.
    %
:- type switch_match
    --->    switch_match(cons_id, maybe(list(prog_var))).

:- pred det_diagnose_switch_context(det_info::in, list(switch_context)::in,
    list(format_piece)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_check_goal.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.
:- import_module hlds.goal_transform.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module edit_seq.
:- import_module int.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

reqscope_check_goal(Params, InstMap0, MaybeReportedSwitch, SwitchContexts,
        Goal, !DetInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(_, RHS, _, _, _),
        (
            ( RHS = rhs_var(_)
            ; RHS = rhs_functor(_, _, _)
            )
        ;
            RHS = rhs_lambda_goal(_Purity, _Groundness, _PorF,
                _LambdaNonLocals, ArgVarsModes, _Detism, LambdaGoal),
            det_info_get_module_info(!.DetInfo, ModuleInfo),
            lambda_update_instmap(ModuleInfo, ArgVarsModes,
                InstMap0, LambdaInstMap0),
            reqscope_check_goal(Params, LambdaInstMap0, no, [],
                LambdaGoal, !DetInfo)
        )
    ;
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        )
    ;
        GoalExpr = conj(_, Goals),
        reqscope_check_conj(Params, InstMap0,
            MaybeReportedSwitch, SwitchContexts, Goals, !DetInfo)
    ;
        GoalExpr = disj(Goals),
        reqscope_check_disj(Params, InstMap0, SwitchContexts, Goals, !DetInfo)
    ;
        GoalExpr = switch(Var, CanFail, Cases),
        Params = reqscope_params(IIS, ReqArmsTypeOrder),
        (
            CanFail = cannot_fail
        ;
            CanFail = can_fail,
            Context = goal_info_get_context(GoalInfo),
            ( if
                (
                    IIS = do_not_inform_incomplete_switches
                ;
                    MaybeReportedSwitch = yes(ReportedSwitch),
                    ReportedSwitch = reported_switch(ReportedContext,
                        ReportedVar, ReportedCases),
                    ReportedContext = Context,
                    ReportedVar = Var,
                    ReportedCases = Cases
                )
            then
                % We have already reported an error for this incomplete switch.
                true
            else
                generate_incomplete_switch_spec(
                    inform_incomplete_switch_option, yes(10),
                    InstMap0, SwitchContexts, Var, Cases, Context, !DetInfo)
            )
        ),
        det_info_get_var_table(!.DetInfo, VarTable),
        lookup_var_entry(VarTable, Var, VarEntry),
        VarEntry = vte(VarName, VarType, _VarIsDummy),
        ( if
            ReqArmsTypeOrder = req_arms_in_type_order,
            does_switch_violate_type_order(!.DetInfo, VarType, Cases,
                VarTypeCtor, TypeSNAs, CaseSNAs)
        then
            generate_type_order_switch_spec(GoalInfo, VarTypeCtor, VarName,
                TypeSNAs, CaseSNAs, !DetInfo)
        else
            true
        ),
        reqscope_check_cases(Params, InstMap0, SwitchContexts,
            Var, VarType, Cases, !DetInfo)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        reqscope_check_goal(Params, InstMap0, no, SwitchContexts,
            Cond, !DetInfo),
        apply_goal_instmap_delta(Cond, InstMap0, InstMap1),
        reqscope_check_goal(Params, InstMap1, no, SwitchContexts,
            Then, !DetInfo),
        reqscope_check_goal(Params, InstMap0, no, SwitchContexts,
            Else, !DetInfo)
    ;
        GoalExpr = negation(SubGoal),
        reqscope_check_goal(Params, InstMap0, no, SwitchContexts,
            SubGoal, !DetInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        reqscope_check_scope(SwitchContexts, Reason, SubGoal, GoalInfo,
            InstMap0, ScopeMaybeReportedSwitch, !DetInfo),
        reqscope_check_goal(Params, InstMap0, ScopeMaybeReportedSwitch,
            SwitchContexts, SubGoal, !DetInfo)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            reqscope_check_goal(Params, InstMap0, no, SwitchContexts,
                MainGoal, !DetInfo),
            reqscope_check_disj(Params, InstMap0, SwitchContexts,
                OrElseGoals, !DetInfo)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            reqscope_check_goal(Params, InstMap0, no, SwitchContexts,
                SubGoal, !DetInfo)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

%---------------------------------------------------------------------------%

:- pred lambda_update_instmap(module_info::in,
    assoc_list(prog_var, mer_mode)::in, instmap::in, instmap::out) is det.

lambda_update_instmap(_ModuleInfo, [], !InstMap).
lambda_update_instmap(ModuleInfo, [Var - Mode | VarsModes], !InstMap) :-
    mode_get_insts(ModuleInfo, Mode, InitInst, _FinalInst),
    instmap_set_var(Var, InitInst, !InstMap),
    lambda_update_instmap(ModuleInfo, VarsModes, !InstMap).

%---------------------------------------------------------------------------%

:- pred reqscope_check_conj(reqscope_params::in, instmap::in,
    maybe(reported_switch)::in, list(switch_context)::in, list(hlds_goal)::in,
    det_info::in, det_info::out) is det.

reqscope_check_conj(_Params, _InstMap0, _MaybeReportedSwitch, _SwitchContexts,
        [], !DetInfo).
reqscope_check_conj(Params, InstMap0, MaybeReportedSwitch, SwitchContexts,
        [Goal | Goals], !DetInfo) :-
    reqscope_check_goal(Params, InstMap0, MaybeReportedSwitch, SwitchContexts,
        Goal, !DetInfo),
    apply_goal_instmap_delta(Goal, InstMap0, InstMap1),
    reqscope_check_conj(Params, InstMap1, MaybeReportedSwitch, SwitchContexts,
        Goals, !DetInfo).

%---------------------------------------------------------------------------%

:- pred reqscope_check_disj(reqscope_params::in, instmap::in,
    list(switch_context)::in, list(hlds_goal)::in,
    det_info::in, det_info::out) is det.

reqscope_check_disj(_Params, _InstMap0, _SwitchContexts, [], !DetInfo).
reqscope_check_disj(Params, InstMap0, SwitchContexts, [Goal | Goals],
        !DetInfo) :-
    reqscope_check_goal(Params, InstMap0, no, SwitchContexts, Goal, !DetInfo),
    reqscope_check_disj(Params, InstMap0, SwitchContexts, Goals, !DetInfo).

%---------------------------------------------------------------------------%

:- pred reqscope_check_cases(reqscope_params::in, instmap::in,
    list(switch_context)::in, prog_var::in, mer_type::in, list(case)::in,
    det_info::in, det_info::out) is det.

reqscope_check_cases(_Params, _InstMap0, _SwitchContexts0, _Var, _VarType,
        [], !DetInfo).
reqscope_check_cases(Params, InstMap0, SwitchContexts0, Var, VarType,
        [Case | Cases], !DetInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    goal_to_conj_list(Goal, GoalSeq),
    find_switch_var_matches(GoalSeq, [Var], MainConsId, OtherConsIds,
        MainMatch, OtherMatches),
    NewSwitchContext = switch_context(Var, MainMatch, OtherMatches),
    SwitchContexts1 = [NewSwitchContext | SwitchContexts0],
    det_info_get_module_info(!.DetInfo, ModuleInfo0),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    det_info_set_module_info(ModuleInfo, !DetInfo),
    reqscope_check_goal(Params, InstMap1, no, SwitchContexts1, Goal, !DetInfo),
    reqscope_check_cases(Params, InstMap0, SwitchContexts0, Var, VarType,
        Cases, !DetInfo).

%---------------------------------------------------------------------------%

:- pred gather_switch_arms_cons_ids_in_order(list(case)::in,
    map(prog_context, list(cons_id))::in,
    map(prog_context, list(cons_id))::out) is det.

gather_switch_arms_cons_ids_in_order([], !ContextMap).
gather_switch_arms_cons_ids_in_order([Case | Cases], !ContextMap) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    CaseConsIds = [MainConsId | OtherConsIds],
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    ( if map.search(!.ContextMap, Context, OldConsIds) then
        NewConsIds = OldConsIds ++ CaseConsIds,
        map.det_update(Context, NewConsIds, !ContextMap)
    else
        map.det_insert(Context, CaseConsIds, !ContextMap)
    ),
    gather_switch_arms_cons_ids_in_order(Cases, !ContextMap).

:- pred constructor_to_sym_name_arity(constructor::in,
    sym_name_arity::out) is det.

constructor_to_sym_name_arity(Ctor, SNA) :-
    Ctor = ctor(_Ordinal, _MaybeExist, SymName, _Args, Arity, _Context),
    SNA = sym_name_arity(SymName, Arity).

:- pred cons_id_to_sym_name_arity(cons_id::in, sym_name_arity::out) is det.

cons_id_to_sym_name_arity(ConsId, SNA) :-
    % This may fail if ConsId is not from a du type, but we have already
    % checked that they come from a du type ...
    ( if ConsId = du_data_ctor(DuCtor) then
        DuCtor = du_ctor(SymName, Arity, _TypeCtor),
        SNA = sym_name_arity(SymName, Arity)
    else
        unexpected($pred, "not du_data_ctor")
    ).

%---------------------------------------------------------------------------%

:- pred generate_incomplete_switch_spec(why_report_incomplete_switch::in,
    maybe(int)::in, instmap::in, list(switch_context)::in,
    prog_var::in, list(case)::in, prog_context::in,
    det_info::in, det_info::out) is det.

generate_incomplete_switch_spec(Why, MaybeLimit, InstMap0, SwitchContexts,
        SwitchVar, Cases, Context, !DetInfo) :-
    find_missing_cons_ids(!.DetInfo, MaybeLimit, InstMap0, SwitchContexts,
        SwitchVar, Cases, NestingPieces, SwitchVarStr, MaybeMissingInfo),
    (
        MaybeMissingInfo = yes(MissingInfo),
        MissingInfo = missing_cons_id_info(NumPossibleConsIds,
            NumUncoveredConsIds, MainPieces, VerbosePieces),
        (
            Why = switch_required_to_be_complete,
            ErrorPieces = [lower_case_next_if_not_first,
                words("Error: the")] ++
                color_as_subject([words("switch on"), quote(SwitchVarStr)]) ++
                [words("is required to be complete,"),
                words("but it does not cover")],
            append_prefix_and_maybe_verbose(yes(color_incorrect),
                NestingPieces ++ ErrorPieces, [],
                MainPieces, VerbosePieces, Component),
            MaybeSeverityComponents = yes({severity_error, [Component]})
        ;
            Why = inform_incomplete_switch_option,
            det_info_get_module_info(!.DetInfo, ModuleInfo),
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_int_option(Globals,
                inform_incomplete_switch_threshold, Threshold),
            NumCoveredConsIds = NumPossibleConsIds - NumUncoveredConsIds,
            ( if NumCoveredConsIds * 100 >= NumPossibleConsIds * Threshold then
                % The number of covered cons_ids is above the threshold,
                % so it is reasonably likely to that the switch is *meant*
                % to be complete.
                NoCoverPieces = [lower_case_next_if_not_first,
                    words("The switch on"), quote(SwitchVarStr),
                    words("does not cover")],
                append_prefix_and_maybe_verbose(no, NestingPieces,
                    NoCoverPieces, MainPieces, VerbosePieces, Component),
                Severity0 = severity_informational(inform_incomplete_switch),
                MaybeSeverityComponents = yes({Severity0, [Component]})
            else
                MaybeSeverityComponents = no
            )
        )
    ;
        MaybeMissingInfo = no,
        (
            Why = switch_required_to_be_complete,
            NoCoverPieces = [lower_case_next_if_not_first,
                words("Error: the")] ++
                color_as_subject([words("switch on"), quote(SwitchVarStr)]) ++
                [words("is required to be complete, but it is not."), nl],
            Component = always(NestingPieces ++ NoCoverPieces),
            MaybeSeverityComponents = yes({severity_error, [Component]})
        ;
            Why = inform_incomplete_switch_option,
            MaybeSeverityComponents = no
        )
    ),
    (
        MaybeSeverityComponents = yes({Severity, SpecComponents}),
        Msg = simple_msg(Context, SpecComponents),
        Spec = error_spec($pred, Severity, phase_detism_check, [Msg]),
        det_info_add_error_spec(Spec, !DetInfo)
    ;
        MaybeSeverityComponents = no
    ).

%---------------------------------------------------------------------------%

:- pred does_switch_violate_type_order(det_info::in, mer_type::in,
    list(case)::in, type_ctor::out,
    list(sym_name_arity)::out, list(sym_name_arity)::out) is semidet.

does_switch_violate_type_order(DetInfo, VarType, Cases,
        VarTypeCtor, TypeSNAs, CaseSNAs) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    module_info_get_type_table(ModuleInfo, TypeTable),
    type_to_ctor_det(VarType, VarTypeCtor),
    search_type_ctor_defn(TypeTable, VarTypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    TypeBody = hlds_du_type(TypeBodyDu),
    TypeBodyDu = type_body_du(OoMTypeConstructors, _, _, _, _),
    TypeConstructors = one_or_more_to_list(OoMTypeConstructors),
    gather_switch_arms_cons_ids_in_order(Cases,
        map.init, ContextMap),
    map.values(ContextMap, ContextConsIdLists),
    list.condense(ContextConsIdLists, CaseConsIds),
    list.map(constructor_to_sym_name_arity,
        TypeConstructors, TypeSNAs),
    list.map(cons_id_to_sym_name_arity,
        CaseConsIds, CaseSNAs),
    TypeSNAs \= CaseSNAs.

:- pred generate_type_order_switch_spec(hlds_goal_info::in, type_ctor::in,
    string::in, list(sym_name_arity)::in, list(sym_name_arity)::in,
    det_info::in, det_info::out) is det.

generate_type_order_switch_spec(GoalInfo, TypeCtor, VarName,
        TypeSNAs, CaseSNAs, !DetInfo) :-
    Context = goal_info_get_context(GoalInfo),
    TypeSNAStrs = list.map(mercury_sym_name_arity_to_string, TypeSNAs),
    CaseSNAStrs = list.map(mercury_sym_name_arity_to_string, CaseSNAs),
    EditParams = edit_params(1, 1, 2),
    construct_diff_for_string_seqs(EditParams, TypeSNAStrs, CaseSNAStrs,
        DiffPieces),
% Possible alternate wording.
%   Pieces = [words("Warning: the arms of the")] ++
%       color_as_subject([words("switch on"), quote(VarName)]) ++
%       [words("process the constructors of the"), unqual_type_ctor(TypeCtor),
%       words("type")] ++
%       color_as_incorrect([words("in an order that differs"),
%           words("from the type definition.")]) ++ [nl,
%       words("The difference between the type order"),
%       words("and the switch arm order is the following:"), nl] ++
%       DiffPieces,
    Pieces = [words("Warning: the order of the arms of this")] ++
        color_as_subject([words("switch on"), quote(VarName)]) ++
        color_as_incorrect([words("differs")]) ++ [words("from")] ++
        color_as_correct([words("the order of the constructors"),
            words("in the definition of the"),
            unqual_type_ctor(TypeCtor), words("type.")]) ++ [nl,
        words("The difference between the type definition order"),
        words("and the switch arm order is the following:"), nl] ++
        DiffPieces,
    Spec = spec($pred, severity_warning(warn_requested_by_code),
        phase_detism_check, Context, Pieces),
    det_info_add_error_spec(Spec, !DetInfo).

%---------------------------------------------------------------------------%

:- pred reqscope_check_scope(list(switch_context)::in,
    scope_reason::in, hlds_goal::in, hlds_goal_info::in, instmap::in,
    maybe(reported_switch)::out, det_info::in, det_info::out) is det.

reqscope_check_scope(SwitchContexts, Reason, SubGoal, ScopeGoalInfo, InstMap0,
        MaybeReportedSwitch, !DetInfo) :-
    (
        Reason = require_detism(RequiredDetism),
        reqscope_check_goal_detism(RequiredDetism, SubGoal,
            require_detism_scope(ScopeGoalInfo), InstMap0, !DetInfo),
        MaybeReportedSwitch = no
    ;
        Reason = require_complete_switch(RequiredVar),
        % We must test the version of the subgoal that has not yet been
        % simplified, since simplification can convert a complete switch
        % into an incomplete switch by deleting an arm that contains
        % only `fail'.
        ( if
            is_scope_subgoal_a_sortof_switch(SubGoal,
                SwitchGoalContext, SwitchVar, CanFail, Cases),
            SwitchVar = RequiredVar
        then
            (
                CanFail = cannot_fail,
                MaybeReportedSwitch = no
            ;
                CanFail = can_fail,
                ScopeContext = goal_info_get_context(ScopeGoalInfo),
                generate_incomplete_switch_spec(
                    switch_required_to_be_complete, no, InstMap0,
                    SwitchContexts, RequiredVar, Cases, ScopeContext,
                    !DetInfo),
                ReportedSwitch =
                    reported_switch(SwitchGoalContext, SwitchVar, Cases),
                MaybeReportedSwitch = yes(ReportedSwitch)
            )
        else
            generate_error_not_switch_on_required_var(SwitchContexts,
                RequiredVar, "require_complete_switch", ScopeGoalInfo,
                !DetInfo),
            MaybeReportedSwitch = no
        )
    ;
        Reason = require_switch_arms_detism(RequiredVar, RequiredDetism),
        ( if
            is_scope_subgoal_a_sortof_switch(SubGoal,
                _SwitchContext, SwitchVar, _CanFail, Cases)
        then
            det_info_get_var_table(!.DetInfo, VarTable),
            lookup_var_type(VarTable, SwitchVar, SwitchVarType),
            reqscope_check_goal_detism_for_cases(RequiredDetism,
                SwitchVar, SwitchVarType, Cases, InstMap0, !DetInfo)
        else
            (
                RequiredDetism = detism_det,
                ScopeWord = "require_switch_arms_det"
            ;
                RequiredDetism = detism_semi,
                ScopeWord = "require_switch_arms_semidet"
            ;
                RequiredDetism = detism_multi,
                ScopeWord = "require_switch_arms_multi"
            ;
                RequiredDetism = detism_non,
                ScopeWord = "require_switch_arms_nondet"
            ;
                RequiredDetism = detism_cc_multi,
                ScopeWord = "require_switch_arms_cc_multi"
            ;
                RequiredDetism = detism_cc_non,
                ScopeWord = "require_switch_arms_cc_nondet"
            ;
                RequiredDetism = detism_erroneous,
                ScopeWord = "require_switch_arms_erroneous"
            ;
                RequiredDetism = detism_failure,
                ScopeWord = "require_switch_arms_failure"
            ),
            generate_error_not_switch_on_required_var(SwitchContexts,
                RequiredVar, ScopeWord, ScopeGoalInfo, !DetInfo)
        ),
        MaybeReportedSwitch = no
    ;
        Reason = loop_control(_, _, _),
        SubGoal = hlds_goal(_, SubGoalInfo),
        Detism = goal_info_get_determinism(SubGoalInfo),
        (
            ( Detism = detism_det
            ; Detism = detism_cc_multi
            )
        ;
            ( Detism = detism_semi
            ; Detism = detism_multi
            ; Detism = detism_non
            ; Detism = detism_cc_non
            ; Detism = detism_failure
            % Note: One day we should make exceptions in parallel
            % conjunctions work.
            ; Detism = detism_erroneous
            ),
            % Since loop control structures are generated only by the compiler,
            % it is reasonable to abort here, and it we don't want to present
            % the user with what would be a very confusing error message.
            unexpected($pred, "Loop control scope with strange determinism")
        ),
        MaybeReportedSwitch = no
    ;
        ( Reason = exist_quant(_, _)
        ; Reason = disable_warnings(_, _)
        ; Reason = commit(_)
        ; Reason = barrier(_)
        ; Reason = promise_purity(_)
        ; Reason = promise_solutions(_, _)
        ; Reason = from_ground_term(_, _)
        ; Reason = trace_goal(_, _, _, _, _)
        ),
        MaybeReportedSwitch = no
    ).

%---------------------%

    % Are we checking the determinism of a goal for a require_{det,...} scope,
    % or for a require_switch_arms_{det,...} scope?
    %
:- type detism_check_kind
    --->    require_detism_scope(hlds_goal_info)
    ;       require_detism_switch_arm(prog_var, cons_id, list(cons_id)).

:- pred reqscope_check_goal_detism(determinism::in, hlds_goal::in,
    detism_check_kind::in, instmap::in, det_info::in, det_info::out) is det.

reqscope_check_goal_detism(RequiredDetism, Goal, CheckKind, InstMap0,
        !DetInfo) :-
    Goal = hlds_goal(_, GoalInfo),
    ActualDetism = goal_info_get_determinism(GoalInfo),
    compare_determinisms(ActualDetism, RequiredDetism, CompareResult),
    ( if
        (
            CheckKind = require_detism_scope(_),
            % For require_detism scopes, the programmer requires an exact
            % match.
            CompareResult = first_detism_same_as
        ;
            CheckKind = require_detism_switch_arm(_, _, _),
            % For require_switch_arms_detism scopes, the programmer requires
            % only that each switch arm's determinism must be at least as tight
            % as RequiredDetism.
            ( CompareResult = first_detism_tighter_than
            ; CompareResult = first_detism_same_as
            )
        )
    then
        true
    else
        ReqDetismStr = determinism_to_string(RequiredDetism),
        ActDetismStr = determinism_to_string(ActualDetism),
        ReqPieces = color_as_correct([quote(ReqDetismStr), suffix(",")]),
        ActPieces = color_as_incorrect([quote(ActDetismStr), suffix(".")]),
        (
            CheckKind = require_detism_scope(ScopeGoalInfo),
            % For require_detism scopes, the context of the require_detism
            % keyword is the most appropriate scope.
            Context = goal_info_get_context(ScopeGoalInfo),
            Pieces = [words("Error: the required determinism of the goal"),
                words("in this scope is")] ++ ReqPieces ++
                [words("but its actual determinism is")] ++ ActPieces ++ [nl]
        ;
            CheckKind =
                require_detism_switch_arm(SwitchVar, MainConsId, OtherConsIds),
            % For require_switch_arms_detism scopes, the context of the
            % require_switch_arms_detism keyword won't work, since it
            % won't tell the user *which* arm's determinism isn't right.
            % We have to use the context of the switch arm itself.
            Context = goal_info_get_context(GoalInfo),
            det_info_get_var_table(!.DetInfo, VarTable),
            SwitchVarName = var_table_entry_name(VarTable, SwitchVar),
            MainConsIdStr = cons_id_and_arity_to_string(MainConsId),
            OtherConsIdStrs =
                list.map(cons_id_and_arity_to_string, OtherConsIds),
            ConsIdsPieces =
                fixed_list_to_pieces("and", [MainConsIdStr | OtherConsIdStrs]),
            Pieces = [words("Error: the arms of the")] ++
                color_as_subject([words("switch on"), quote(SwitchVarName)]) ++
                [words("are required have a determinism that is"),
                words("acceptable in a")] ++
                color_as_correct([quote(ReqDetismStr)]) ++ [words("context,"),
                words("but the actual determinism"),
                words("of the arm for")] ++ ConsIdsPieces ++
                [words("is")] ++ ActPieces ++ [nl]
        ),
        Msg = msg(Context, Pieces),
        det_diagnose_goal_get_msgs(InstMap0, RequiredDetism, Goal,
            SubMsgs, !DetInfo),
        Spec = error_spec($pred, severity_error, phase_detism_check,
            [Msg | SubMsgs]),
        det_info_add_error_spec(Spec, !DetInfo)
    ).

:- pred reqscope_check_goal_detism_for_cases(determinism::in,
    prog_var::in, mer_type::in, list(case)::in, instmap::in,
    det_info::in, det_info::out) is det.

reqscope_check_goal_detism_for_cases(_RequiredDetism, _Var, _VarType,
        [], _InstMap0, !DetInfo).
reqscope_check_goal_detism_for_cases(RequiredDetism, Var, VarType,
        [Case | Cases], InstMap0, !DetInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    det_info_get_module_info(!.DetInfo, ModuleInfo0),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    det_info_set_module_info(ModuleInfo, !DetInfo),
    CheckKind = require_detism_switch_arm(Var, MainConsId, OtherConsIds),
    reqscope_check_goal_detism(RequiredDetism, Goal, CheckKind, InstMap1,
        !DetInfo),

    reqscope_check_goal_detism_for_cases(RequiredDetism, Var, VarType,
        Cases, InstMap0, !DetInfo).

%---------------------%

    % Is the given goal, which should be the subgoal of either a
    % require_complete_switch or require_switch_arms_detism scope,
    % a switch, or something that started out as a switch before
    % being modified by the compiler passes between switch detection
    % and here?
    %
:- pred is_scope_subgoal_a_sortof_switch(hlds_goal::in,
    prog_context::out, prog_var::out, can_fail::out, list(case)::out)
    is semidet.

is_scope_subgoal_a_sortof_switch(Goal, SwitchContext, SwitchVar,
        CanFail, Cases) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    require_complete_switch [GoalExpr]
    (
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        SwitchContext = goal_info_get_context(GoalInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        % If the switch originally directly inside this scope
        % has some arms that have more solutions than they should
        % but the identity of those solutions does not matter,
        % the main determinism analysis algorithm will wrap the
        % switch with a commit scope. To diagnose the problem
        % that required this scope, we want to look through it.
        Reason = commit(_MaybeForcePruning),
        SubGoal = hlds_goal(SubGoalExpr, SubGoalInfo),
        SubGoalExpr = switch(SwitchVar, CanFail, Cases),
        SwitchContext = goal_info_get_context(SubGoalInfo)
    ;
        GoalExpr = conj(plain_conj, Conjuncts0),
        flatten_conj(Conjuncts0, Conjuncts),
        cse_lifted_then_sortof_switch(Conjuncts, SwitchContext, SwitchVar,
            CanFail, Cases)
    ;
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = disj(_)
        ; GoalExpr = if_then_else(_, _, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = shorthand(_)
        ),
        fail
    ).

:- pred cse_lifted_then_sortof_switch(list(hlds_goal)::in,
    prog_context::out, prog_var::out, can_fail::out, list(case)::out)
    is semidet.

cse_lifted_then_sortof_switch(Conjuncts, SwitchContext, SwitchVar,
        CanFail, Cases) :-
    (
        Conjuncts = [Conjunct],
        is_scope_subgoal_a_sortof_switch(Conjunct,
            SwitchContext, SwitchVar, CanFail, Cases)
    ;
        Conjuncts = [Conjunct1, Conjunct2 | TailConjuncts],
        goal_has_feature(Conjunct1, feature_lifted_by_cse),
        cse_lifted_then_sortof_switch([Conjunct2 | TailConjuncts],
            SwitchContext, SwitchVar, CanFail, Cases)
    ).

%---------------------%

:- pred generate_error_not_switch_on_required_var(list(switch_context)::in,
    prog_var::in, string::in, hlds_goal_info::in,
    det_info::in, det_info::out) is det.

generate_error_not_switch_on_required_var(SwitchContexts, RequiredVar,
        ScopeWord, ScopeGoalInfo, !DetInfo) :-
    det_diagnose_switch_context(!.DetInfo, SwitchContexts, NestingPieces),
    det_info_get_var_table(!.DetInfo, VarTable),
    RequiredVarStr =
        mercury_var_to_string(VarTable, print_name_only, RequiredVar),
    ScopePieces = [words(ScopeWord), fixed("[" ++ RequiredVarStr ++ "]"),
        words("scope")],
    Pieces = NestingPieces ++ [lower_case_next_if_not_first,
        words("Error: the goal inside the")] ++
        color_as_subject(ScopePieces) ++ [words("is")] ++
        color_as_incorrect([words("not a switch on"),
            quote(RequiredVarStr), suffix(".")]) ++
        [nl],
    Context = goal_info_get_context(ScopeGoalInfo),
    Spec = spec($pred, severity_error, phase_detism_check, Context, Pieces),
    det_info_add_error_spec(Spec, !DetInfo).

%---------------------------------------------------------------------------%

find_switch_var_matches([], _, MainConsId, OtherConsIds,
        MainMatch, OtherMatches) :-
    make_switch_match_no_args(MainConsId, MainMatch),
    list.map(make_switch_match_no_args, OtherConsIds, OtherMatches).
find_switch_var_matches([Conjunct | Conjuncts], !.SwitchVarSynonyms,
        MainConsId, OtherConsIds, MainMatch, OtherMatches) :-
    Conjunct = hlds_goal(GoalExpr, GoalInfo),
    ( if
        GoalExpr = unify(_, _, _, Unification, _),
        Unification = deconstruct(Var, MainConsId, ArgVars, _, _, _),
        list.member(Var, !.SwitchVarSynonyms),
        OtherConsIds = []
    then
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        ArgVarsSet = set_of_var.list_to_set(ArgVars),
        ( if
            set_of_var.intersect(NonLocals, ArgVarsSet, NonLocalArgVarsSet),
            set_of_var.is_non_empty(NonLocalArgVarsSet)
        then
            MaybeArgVars = yes(ArgVars)
        else
            MaybeArgVars = no
        ),
        MainMatch = switch_match(MainConsId, MaybeArgVars),
        OtherMatches = []
    else if
        GoalExpr = disj(Disjuncts),
        find_switch_var_submatches(Disjuncts, !.SwitchVarSynonyms,
            yes(MainConsId), OtherConsIds, yes(MainMatch0), OtherMatches0)
    then
        MainMatch = MainMatch0,
        OtherMatches = OtherMatches0
    else
        ( if
            GoalExpr = unify(_, _, _, Unification, _),
            Unification = assign(ToVar, FromVar),
            list.member(FromVar, !.SwitchVarSynonyms)
        then
            !:SwitchVarSynonyms = [ToVar | !.SwitchVarSynonyms]
        else
            true
        ),
        find_switch_var_matches(Conjuncts, !.SwitchVarSynonyms,
            MainConsId, OtherConsIds, MainMatch, OtherMatches)
    ).

    % If a conjunct in a switch arm is disjunction, check whether it is
    % the disjunction that specifies that this is the arm for MainConsId
    % and OtherConsIds. Once we have found a cons_id, we delete it from
    % the list of cons_ids the recursive call should look for. In the case of
    % the main cons_id, we do this by passing `no' as the MaybeMainConsId
    % argument; in the case of the other cons_ids, we do this by deleting it
    % from the OtherConsIds argument.
    %
    % If a call to this predicate succeeds, it should return switch_matches
    % for exactly the set of main and other cons_ids it was invoked with.
    %
:- pred find_switch_var_submatches(list(hlds_goal)::in, list(prog_var)::in,
    maybe(cons_id)::in, list(cons_id)::in,
    maybe(switch_match)::out, list(switch_match)::out) is semidet.

find_switch_var_submatches([], _, no, [], no, []).
find_switch_var_submatches([Disjunct | Disjuncts], SwitchVarSynonyms,
        MaybeMainConsId, OtherConsIds, MaybeMainMatch, OtherMatches) :-
    Disjunct = hlds_goal(GoalExpr, GoalInfo),
    GoalExpr = unify(_, _, _, Unification, _),
    Unification = deconstruct(Var, ConsId, ArgVars, _, _, _),
    list.member(Var, SwitchVarSynonyms),
    ( if
        MaybeMainConsId = yes(MainConsId),
        ConsId = MainConsId
    then
        find_switch_var_submatches(Disjuncts, SwitchVarSynonyms,
            no, OtherConsIds, no, OtherMatches),
        MaybeMainMatch = yes(switch_match(ConsId, yes(ArgVars)))
    else if
        list.delete_first(OtherConsIds, ConsId, LeftOverConsIds)
    then
        find_switch_var_submatches(Disjuncts, SwitchVarSynonyms,
            MaybeMainConsId, LeftOverConsIds, MaybeMainMatch, LeftOverMatches),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        set_of_var.list_to_set(ArgVars, ArgVarsSet),
        ( if
            set_of_var.intersect(NonLocals, ArgVarsSet, NonLocalArgVarsSet),
            set_of_var.is_non_empty(NonLocalArgVarsSet)
        then
            MaybeArgVars = yes(ArgVars)
        else
            MaybeArgVars = no
        ),
        OtherMatches = [switch_match(ConsId, MaybeArgVars) | LeftOverMatches]
    else
        fail
    ).

:- pred make_switch_match_no_args(cons_id::in, switch_match::out) is det.

make_switch_match_no_args(ConsId, Match) :-
    Match = switch_match(ConsId, no).

%---------------------------------------------------------------------------%

find_missing_cons_ids(DetInfo, MaybeLimit, InstMap0, SwitchContexts,
        Var, Cases, NestingPieces, VarStr, MaybeMissingInfo) :-
    det_diagnose_switch_context(DetInfo, SwitchContexts, NestingPieces),

    det_info_get_module_info(DetInfo, ModuleInfo),
    det_info_get_var_table(DetInfo, VarTable),
    VarStr = mercury_var_to_string(VarTable, print_name_only, Var),
    instmap_lookup_var(InstMap0, Var, VarInst),
    ( if
        det_info_get_var_table(DetInfo, VarTable),
        lookup_var_type(VarTable, Var, VarType),
        type_to_ctor_det(VarType, VarTypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        ( if
            inst_is_bound_to_functors(ModuleInfo, VarInst, BoundFunctors)
        then
            bound_functors_to_cons_ids(VarTypeCtor,
                BoundFunctors, BoundConsIds),
            list.sort_and_remove_dups(BoundConsIds, SortedBoundConsIds),
            set_tree234.sorted_list_to_set(SortedBoundConsIds,
                BoundConsIdsSet),
            % We should insist that all insts used in predicate
            % declarations be specific to the type of the arguments
            % they apply to. However, I (zs) don't think we enforce this
            % yet with any consistency in the mode checker. The intersection
            % operation below should compensate for this, but we can invoke it
            % only for the switch variable's type is a du type, and not
            % a builtin type such as int or string.
            ( if
                search_type_ctor_defn(TypeTable, VarTypeCtor, TypeDefn),
                hlds_data.get_type_defn_body(TypeDefn, TypeBody),
                TypeBody = hlds_du_type(TypeBodyDu),
                TypeBodyDu = type_body_du(TypeConstructors, _, _, _, _)
            then
                SortedTypeConsIds =
                    constructor_cons_ids(VarTypeCtor,
                        one_or_more_to_list(TypeConstructors)),
                set_tree234.sorted_list_to_set(SortedTypeConsIds,
                    TypeConsIdsSet),
                set_tree234.intersect(TypeConsIdsSet, BoundConsIdsSet,
                    PossibleConsIdsSet)
            else
                PossibleConsIdsSet = BoundConsIdsSet
            )
        else
            search_type_ctor_defn(TypeTable, VarTypeCtor, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(TypeConstructors, _, _, _, _),
            SortedTypeConsIds =
                constructor_cons_ids(VarTypeCtor,
                    one_or_more_to_list(TypeConstructors)),
            set_tree234.sorted_list_to_set(SortedTypeConsIds, TypeConsIdsSet),
            PossibleConsIdsSet = TypeConsIdsSet
        )
    then
        compute_covered_cons_ids(Cases, set_tree234.init, CoveredConsIdsSet),
        set_tree234.difference(PossibleConsIdsSet, CoveredConsIdsSet,
            UncoveredConsIdsSet),
        NumPossibleConsIds = set_tree234.count(PossibleConsIdsSet),
        NumUncoveredConsIds = set_tree234.count(UncoveredConsIdsSet),

        UncoveredConsIds = set_tree234.to_sorted_list(UncoveredConsIdsSet),
        list.map(strip_module_qualifier_from_cons_id,
            UncoveredConsIds, UnQualConsIds),
        % Just in case the stripping of qualifiers has affected the order.
        % It shouldn't, but this double insurance costs effectively nothing.
        list.sort(UnQualConsIds, SortedUnQualConsIds),

        (
            MaybeLimit = no,
            PrintedConsIds = SortedUnQualConsIds,
            NonPrintedConsIds = []
        ;
            MaybeLimit = yes(Limit),
            list.length(SortedUnQualConsIds, NumConsIds),
            ( if NumConsIds =< Limit then
                PrintedConsIds = SortedUnQualConsIds,
                NonPrintedConsIds = []
            else
                % We use Limit-1 lines for the printed missing cons_ids,
                % and one line for the message "and N more".
                %
                % If there are Limit missing cons_ids, the then-part above
                % will print them all. If there are Limit+1 missing cons_ids,
                % we will print Limit-1 of them here, followed by "and 2 more".
                % We never print "and 1 more", and we don't want to, since
                % anyone who read that would probably (justifiably) wonder
                % "why didn't the compiler use that line to print the one
                % unprinted missing cons_id?".
                list.split_upto(Limit - 1, SortedUnQualConsIds,
                    PrintedConsIds, NonPrintedConsIds)
            )
        ),
        (
            PrintedConsIds = [],
            MaybeMissingInfo = no
        ;
            PrintedConsIds = [_ | _],
            % If we invoked determinism analysis on this procedure, then
            % it must be type correct. Since users will know the type of the
            % switched-on variable, they will know which module defined it,
            % and hence which modules defined its function symbols.
            % Repeating the name of that module for each cons_id is
            % much more likely to be distracting clutter than helpful
            % information.
            WrapConsIdFunc = ( func(C) = [unqual_cons_id_and_maybe_arity(C)] ),
            PrintedConsIdPieces = list.map(WrapConsIdFunc, PrintedConsIds),
            (
                NonPrintedConsIds = [],
                MainPieces =
                    [nl_indent_delta(1)] ++
                    pieces_list_to_color_line_pieces(color_incorrect,
                        [suffix(".")], PrintedConsIdPieces) ++
                    [nl_indent_delta(-1)],
                VerbosePieces = []
            ;
                NonPrintedConsIds = [_ | _],
                NonPrintedConsIdPieces =
                    list.map(WrapConsIdFunc, NonPrintedConsIds),
                list.length(NonPrintedConsIds, NumNonPrintedConsIds),
                MainPieces =
                    [nl_indent_delta(1)] ++
                    pieces_list_to_color_line_pieces(color_incorrect,
                        [suffix(","), fixed("...")], PrintedConsIdPieces) ++
                    [nl_indent_delta(-1)] ++
                    color_as_incorrect([words("and"),
                        int_fixed(NumNonPrintedConsIds), words("more.")]) ++
                    [nl],
                ConsIdPieces = PrintedConsIdPieces ++ NonPrintedConsIdPieces,
                VerbosePieces =
                    [nl_indent_delta(1)] ++
                    pieces_list_to_color_line_pieces(color_incorrect,
                        [suffix(".")], ConsIdPieces) ++
                    [nl_indent_delta(-1)]
            ),
            MissingInfo = missing_cons_id_info(NumPossibleConsIds,
                NumUncoveredConsIds, MainPieces, VerbosePieces),
            MaybeMissingInfo = yes(MissingInfo)
        )
    else
        MaybeMissingInfo = no
    ).

:- pred compute_covered_cons_ids(list(case)::in,
    set_tree234(cons_id)::in, set_tree234(cons_id)::out) is det.

compute_covered_cons_ids([], !CoveredConsIds).
compute_covered_cons_ids([Case | Cases], !CoveredConsIds) :-
    Case = case(MainConsId, OtherConsIds, _Goal),
    set_tree234.insert(MainConsId, !CoveredConsIds),
    set_tree234.insert_list(OtherConsIds, !CoveredConsIds),
    compute_covered_cons_ids(Cases, !CoveredConsIds).

%---------------------------------------------------------------------------%

:- type switch_context
    --->    switch_context(
                % The variable being switched on.
                prog_var,

                % The match info for the first cons_id of this case.
                switch_match,

                % The match info for the other cons_ids of this case.
                list(switch_match)
            ).

    % A switch arm is for one or more cons_ids. A switch match can record,
    % for one of these cons_ids, the variables on the right hand side of
    % the unification that told switch detection that this disjunction
    % can succeed only if the switched-on variable is bound to one of these
    % cons_ids. For example, in a switch on X, if the switch arm is
    % for the cons_id f/2, this means that the switch arm should have
    % a unification of the form X = f(A, B). Likewise, if the switch arm
    % is for more than one cons_id, such as f/2 and g/1, then the switch arm
    % should contain a disjunction such as ( X = f(A, B) ; X = g(C) ).
    % The switch match data type records the argument variables of these
    % unifications PROVIDED that some of them are visible from the outside,
    % which means that later error messages (e.g. about missing arms in
    % switches on them) can refer to them.
    %
:- type switch_match
    --->    switch_match(cons_id, maybe(list(prog_var))).

det_diagnose_switch_context(_, [], []).
det_diagnose_switch_context(DetInfo, [SwitchContext | SwitchContexts],
        Pieces) :-
    det_info_get_var_table(DetInfo, VarTable),
    SwitchContext = switch_context(Var, MainMatch, OtherMatches),
    MainMatchStr = switch_match_to_string(VarTable, MainMatch),
    OtherMatchStrs =
        list.map(switch_match_to_string(VarTable), OtherMatches),
    MatchsStr = string.join_list(", ", [MainMatchStr | OtherMatchStrs]),
    VarStr = mercury_var_to_string(VarTable, print_name_only, Var),
    InnerPieces = [words("Inside the case"), words(MatchsStr),
        words("of the switch on"), fixed(VarStr), suffix(":"), nl],
    det_diagnose_switch_context(DetInfo, SwitchContexts, OuterPieces),
    % We construct the list of switch contexts so that inner contexts come
    % before outer contexts, but we want to print the contexts from the outside
    % towards the inside.
    Pieces = OuterPieces ++ [lower_case_next_if_not_first] ++ InnerPieces.

:- func switch_match_to_string(var_table, switch_match) = string.

switch_match_to_string(VarTable, switch_match(ConsId, MaybeArgVars)) =
    cons_id_and_vars_or_arity_to_string(VarTable, do_not_qualify_cons_id,
        ConsId, MaybeArgVars).

%---------------------------------------------------------------------------%
:- end_module check_hlds.det_check_switch.
%---------------------------------------------------------------------------%
