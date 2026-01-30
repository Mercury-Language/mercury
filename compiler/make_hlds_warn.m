%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make_hlds_warn.m.
%
% Generate whatever warnings the module being transformed to HLDS deserves.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_warn.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%

    % Warn about variables with overlapping scopes.
    %
:- pred add_quant_warnings(module_info::in, pf_sym_name_arity::in,
    prog_varset::in, list(quant_warning)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % Have we seen a quantifier with a nonempty list of variables,
    % either in the form of a "some [Vars]" scope, or an if-then-else
    % with a similarly nonempty list of variables being quantified
    % across the condition and then then-part?
:- type maybe_seen_quant
    --->    have_not_seen_quant
    ;       have_seen_quant.

    % Warn about variables which occur only once but don't start with
    % an underscore, or about variables which do start with an underscore
    % but occur more than once, or about variables that do not occur in
    % target language code strings when they should.
    %
:- pred warn_singletons_in_clause_body(module_info::in, pf_sym_name_arity::in,
    prog_varset::in, hlds_goal::in, maybe_seen_quant::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % This predicate performs the following checks on promise ex declarations
    % (see notes/promise_ex.html).
    %
    % - check for universally quantified variables
    % - check if universal quantification is placed in the wrong position
    %   (i.e. after the `promise_exclusive' rather than before it)
    % - check that its goal is a disjunction and that each arm of the
    %   disjunction has at most one call, and otherwise has only unifications.
    %
:- pred check_promise_ex_decl(list(prog_var)::in, promise_type::in, goal::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % Warn about suspicious things in the bodies of foreign_code pragmas.
    % Currently, this just checks for the presence of the MR_ALLOC_ID macro
    % inside the bodies of a foreign_code pragmas.
    %
:- pred warn_suspicious_foreign_code(foreign_language::in,
    foreign_literal_or_include::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module hlds.goal_vars.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_pred.
:- import_module libs.options.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

add_quant_warnings(ModuleInfo, PfSymNameArity, VarSet, Warnings, !Specs) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, warn_overlapping_scopes, WarnOverlap),
    (
        WarnOverlap = no
    ;
        WarnOverlap = yes,
        WarningSpecs =
            list.map(quant_warning_to_spec(PfSymNameArity, VarSet), Warnings),
        !:Specs = WarningSpecs ++ !.Specs

    ).

:- func quant_warning_to_spec(pf_sym_name_arity, prog_varset, quant_warning)
    = error_spec.

quant_warning_to_spec(PfSymNameArity, VarSet, Warning) = Spec :-
    Warning = warn_overlap(Vars, Context),
    Pieces1 = [words("In clause for"),
        unqual_pf_sym_name_pred_form_arity(PfSymNameArity), suffix(":"), nl],
    (
        Vars = [],
        unexpected($pred, "Vars = []")
    ;
        Vars = [HeadVar | TailVars],
        (
            TailVars = [],
            VarPiece = var_to_quote_piece(VarSet, HeadVar),
            Pieces2 = [words("warning: variable")] ++
                color_as_subject([VarPiece]) ++
                [words("has overlapping scopes."), nl]
        ;
            TailVars = [_ | _],
            VarsPieces = list.map(var_to_quote_piece(VarSet), Vars),
            Pieces2 = [words("warning: variables")] ++
                piece_list_to_color_pieces(color_subject, "and", [],
                    VarsPieces) ++
                [words("each have overlapping scopes."), nl]
        )
    ),
    Severity = severity_warning(warn_overlapping_scopes),
    Spec = spec($pred, Severity, phase_pt2h, Context, Pieces1 ++ Pieces2).

%---------------------------------------------------------------------------%

warn_singletons_in_clause_body(ModuleInfo, PfSymNameArity, VarSet, BodyGoal,
        SeenQuant, !Specs) :-
    % We handle warnings about variables in the clause head specially.
    % This is because superhomogeneous.m transforms clause heads such as
    %
    %   p(X, Y, Z) :- ...
    %
    % into
    %
    %   p(HV1, HV2, HV3) :- HV1 = X, HV2 = Y, HV3 = Z, ...
    %
    % If more than one of the head variables is a singleton, programmers
    % would expect a single warning naming them all, since to programmers,
    % everything in the clause head is part of the same scope, but for the
    % compiler, the singleton nature of e.g. Y is detected in its own scope,
    % to wit, the HV2 = Y unification.
    %
    % Even though we discover the singleton nature of e.g. Y in that
    % unification, we don't generate a warning for that scope. Instead,
    % we gather all the singleton variables in the head, and generate a single
    % message for them all here.
    %
    % We also do the same thing for variables whose names indicate they should
    % be singletons, but aren't.
    %
    % Note that we have to traverse all the parts of BodyGoal that may
    % contain scope goals in order to compute SeenQuant, even if both
    % WarnSingleton0 and WarnMulti0 are "no".

    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, warn_singleton_vars, WarnSingleton0),
    globals.lookup_bool_option(Globals, warn_repeated_singleton_vars,
        WarnMulti0),

    trace [compile_time(flag("warn_singletons")), io(!IO)] (
        io.stderr_stream(StdErr, !IO),
        io.write_string(StdErr,
            "\nWARN_SINGLETONS on the following goal:\n", !IO),
        varset.init(TVarSet),
        varset.init(InstVarSet),
        dump_goal(StdErr, ModuleInfo, vns_varset(VarSet),
            TVarSet, InstVarSet, BodyGoal, !IO)
    ),

    ( WarnSingleton0 = no,  WarnSingleton1 = do_not_warn_singleton
    ; WarnSingleton0 = yes, WarnSingleton1 = warn_singleton
    ),
    ( WarnMulti0 = no,  WarnMulti1 = do_not_warn_multi
    ; WarnMulti0 = yes, WarnMulti1 = warn_multi
    ),

    Params = warn_params(ModuleInfo, PfSymNameArity, VarSet,
        WarnSingleton1, WarnMulti1),
    Info0 = warn_info([], set_of_var.init, set_of_var.init,
        dummy_context, have_not_seen_quant),
    QuantVars = set_of_var.init,
    warn_singletons_in_goal(Params, BodyGoal, QuantVars, Info0, Info),
    Info = warn_info(NewSpecs, SingletonHeadVarsSet, MultiHeadVarsSet,
        HeadContext, SeenQuant),
    !:Specs = NewSpecs ++ !.Specs,
    set_of_var.to_sorted_list(SingletonHeadVarsSet, SingletonHeadVars),
    set_of_var.to_sorted_list(MultiHeadVarsSet, MultiHeadVars),
    (
        SingletonHeadVars = []
    ;
        SingletonHeadVars = [HeadSHV | TailSHVs],
        generate_variable_warning(Params, HeadContext, sm_single,
            PfSymNameArity, HeadSHV, TailSHVs, SingleSpecs),
        !:Specs = SingleSpecs ++ !.Specs
    ),
    (
        MultiHeadVars = []
    ;
        MultiHeadVars = [HeadMHV | TailMHVs],
        generate_variable_warning(Params, HeadContext, sm_multi,
            PfSymNameArity, HeadMHV, TailMHVs, MultiSpecs),
        !:Specs = MultiSpecs ++ !.Specs
    ).

:- type maybe_warn_singleton
    --->    do_not_warn_singleton
    ;       warn_singleton.

:- type maybe_warn_multi
    --->    do_not_warn_multi
    ;       warn_multi.

    % We pass values of this type down during the goal traversal.
    % We can change the last two fields while processing a scope's subgoal,
    % but such changes never propagate back up.
:- type warn_params
    --->    warn_params(
                % The first three fields are readonly after initialization.

                % The current module.
                wp_module_info          :: module_info,

                % The id and the varset of the procedure whose body
                % we are checking.
                wp_pf_sna               :: pf_sym_name_arity,
                wp_varset               :: prog_varset,

                % We can update the last two fields at disable_warnings scopes.

                % Should we generate warnings for variables that are
                % singletons, even though their names say they should not be?
                wp_warn_singleton       :: maybe_warn_singleton,

                % Should we generate warnings for variables that are not
                % singletons, even though their names say they should be?
                wp_warn_multi           :: maybe_warn_multi
            ).

    % We thread values of this type all though during the goal traversal.
    % Changes can flow both down and up.
:- type warn_info
    --->    warn_info(
                % All these fields are writeable.

                % The warnings we have generated while checking.
                wi_specs                :: list(error_spec),

                % The set of variables that occur singleton in the clause head.
                wi_singleton_headvars   :: set_of_progvar,

                % The set of variables that occur more than once in the clause
                % head, even though their names say they SHOULD be singletons.
                wi_multi_headvars       :: set_of_progvar,

                % The context of the clause head. Should be set to a meaningful
                % value if either wi_singleton_headvars or wi_multi_headvars
                % is not empty.
                %
                % It is possible for the clause head to occupy more than one
                % line, and thus for different parts of it to have different
                % contexts. Since we want to generate only a single error_spec,
                % we arbitrarily pick the context of one of those variables.
                wi_head_context         :: prog_context,

                % Have we seen a quantifier with at least one variable listed?
                wi_seen_quant           :: maybe_seen_quant
            ).

:- pred warn_singletons_in_goal(warn_params::in, hlds_goal::in,
    set_of_progvar::in, warn_info::in, warn_info::out) is det.

warn_singletons_in_goal(Params, Goal, QuantVars, !Info) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(Var, RHS, _, _, _),
        warn_singletons_in_unify(Params, Var, RHS, GoalInfo, QuantVars, !Info)
    ;
        GoalExpr = plain_call(_, _, Args, _, _, _),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars(Params, Args, GoalInfo, NonLocals,
            QuantVars, !Info)
    ;
        GoalExpr = generic_call(GenericCall, Args0, _, _, _),
        vars_in_generic_call(GenericCall, Args1),
        Args = Args0 ++ Args1,
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars(Params, Args, GoalInfo, NonLocals,
            QuantVars, !Info)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        % The only exported predicate among our ancestors,
        % warn_singletons_in_clause_body, is only ever invoked
        % on Mercury clauses being added to the HLDS. Such clauses
        % cannot contain call_foreign_proc goals; that is possible
        % only after the compiler has inlined a call to a predicate
        % that is defined by foreign code.
        unexpected($pred, "call_foreign_proc")
    ;
        GoalExpr = conj(_ConjType, Goals),
        warn_singletons_in_goal_list(Params, Goals, QuantVars, !Info)
    ;
        GoalExpr = disj(Goals),
        warn_singletons_in_goal_list(Params, Goals, QuantVars, !Info)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        warn_singletons_in_cases(Params, Cases, QuantVars, !Info)
    ;
        GoalExpr = negation(SubGoal),
        warn_singletons_in_goal(Params, SubGoal, QuantVars, !Info)
    ;
        GoalExpr = scope(Reason, SubGoal),
        warn_singletons_in_scope(Params, Reason, SubGoal, GoalInfo,
            QuantVars, !Info)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        (
            Vars = [_ | _],
            % Warn if any variables quantified by the if-then-else itself
            % do not occur either in the condition, or in the "then" part
            % of the if-then-else.
            !Info ^ wi_seen_quant := have_seen_quant,
            CondVars = free_goal_vars(Cond),
            ThenVars = free_goal_vars(Then),
            set_of_var.union(CondVars, ThenVars, CondThenVars),
            set_of_var.init(EmptySet),
            warn_singletons_goal_vars(Params, Vars, GoalInfo, EmptySet,
                CondThenVars, !Info)
        ;
            Vars = []
        ),
        set_of_var.insert_list(Vars, QuantVars, CondThenQuantVars),
        warn_singletons_in_goal(Params, Cond, CondThenQuantVars, !Info),
        warn_singletons_in_goal(Params, Then, CondThenQuantVars, !Info),
        warn_singletons_in_goal(Params, Else, QuantVars, !Info)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            % XXX STM We need to look at how we should handle Outer, Inner and
            % MaybeOutputVars.
            ShortHand = atomic_goal(_GoalType, _Outer, Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            set_of_var.insert_list([InnerDI, InnerUO],
                QuantVars, InsideQuantVars),
            warn_singletons_in_goal(Params, MainGoal, InsideQuantVars, !Info),
            warn_singletons_in_goal_list(Params, OrElseGoals,
                InsideQuantVars, !Info)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            warn_singletons_in_goal(Params, SubGoal, QuantVars, !Info)
        ;
            ShortHand = bi_implication(GoalA, GoalB),
            warn_singletons_in_goal_list(Params, [GoalA, GoalB],
                QuantVars, !Info)
        )
    ).

%---------------------------------------------------------------------------%

:- pred warn_singletons_in_unify(warn_params::in, prog_var::in,
    unify_rhs::in, hlds_goal_info::in, set_of_progvar::in,
    warn_info::in, warn_info::out) is det.

warn_singletons_in_unify(Params, X, RHS, GoalInfo, QuantVars, !Info) :-
    (
        RHS = rhs_var(Y),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars(Params, [X, Y], GoalInfo, NonLocals,
            QuantVars, !Info)
    ;
        RHS = rhs_functor(_ConsId, _, Ys),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars(Params, [X | Ys], GoalInfo, NonLocals,
            QuantVars, !Info)
    ;
        RHS = rhs_lambda_goal(_Purity, _Groundness, _PredOrFunc,
            _NonLocals, ArgVarsModes, _Det, LambdaGoal),
        assoc_list.keys(ArgVarsModes, ArgVars),
        % Warn if any lambda-quantified variables occur only in the quantifier.
        LambdaGoal = hlds_goal(_, LambdaGoalInfo),
        LambdaNonLocals = goal_info_get_nonlocals(LambdaGoalInfo),
        warn_singletons_goal_vars(Params, ArgVars, GoalInfo, LambdaNonLocals,
            QuantVars, !Info),

        % Warn if X (the variable we are unifying the lambda expression with)
        % is singleton.
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars(Params, [X], GoalInfo, NonLocals,
            QuantVars, !Info),

        % Warn if the lambda-goal contains singletons.
        warn_singletons_in_goal(Params, LambdaGoal, QuantVars, !Info)
    ).

%---------------------------------------------------------------------------%

    % warn_singletons_goal_vars(Params, Vars, GoalInfo, NonLocals,
    %   QuantVars, !Info):
    %
    % Warn if any of the non-underscore variables in Vars don't occur in
    % NonLocals and don't have the same name as any variable in QuantVars,
    % or if any of the underscore variables in Vars do occur in NonLocals.
    % Omit the warning if Params or GoalInfo says we should.
    %
:- pred warn_singletons_goal_vars(warn_params::in, list(prog_var)::in,
    hlds_goal_info::in, set_of_progvar::in, set_of_progvar::in,
    warn_info::in, warn_info::out) is det.

warn_singletons_goal_vars(Params, GoalVars, GoalInfo, NonLocals, QuantVars,
        !Info) :-
    VarSet = Params ^ wp_varset,
    PfSymNameArity = Params ^ wp_pf_sna,
    Context = goal_info_get_context(GoalInfo),

    % Find all the variables in the goal that don't occur outside the goal
    % (i.e. are singleton), have a variable name that doesn't start with "_"
    % or "DCG_", and don't have the same name as any variable in QuantVars
    % (i.e. weren't explicitly quantified). If there are any such variables,
    % generate a warning.
    list.filter(is_singleton_var(NonLocals, QuantVars, VarSet), GoalVars,
        SingleVars),
    (
        SingleVars = []
    ;
        SingleVars = [HeadSV | TailSVs],
        ( if
            goal_info_has_feature(GoalInfo, feature_do_not_warn_singleton)
        then
            true
        else
            ( if goal_info_has_feature(GoalInfo, feature_from_head) then
                SingleHeadVars0 = !.Info ^ wi_singleton_headvars,
                set_of_var.insert_list(SingleVars,
                    SingleHeadVars0, SingleHeadVars),
                !Info ^ wi_singleton_headvars := SingleHeadVars,
                !Info ^ wi_head_context := goal_info_get_context(GoalInfo)
            else
                generate_variable_warning(Params, Context, sm_single,
                    PfSymNameArity, HeadSV, TailSVs, SingleSpecs),
                add_warn_specs(SingleSpecs, !Info)
            )
        )
    ),

    % Find all the variables in the goal that do occur outside the goal
    % (i.e. are not singleton) and have a variable name that starts
    % with "_". If there are any such variables, generate a warning.
    list.filter(is_multi_var(NonLocals, VarSet), GoalVars, MultiVars),
    (
        MultiVars = []
    ;
        MultiVars = [HeadMV | TailMVs],
        ( if goal_info_has_feature(GoalInfo, feature_from_head) then
            MultiHeadVars0 = !.Info ^ wi_multi_headvars,
            set_of_var.insert_list(MultiVars, MultiHeadVars0, MultiHeadVars),
            !Info ^ wi_multi_headvars := MultiHeadVars,
            !Info ^ wi_head_context := goal_info_get_context(GoalInfo)
        else
            generate_variable_warning(Params, Context, sm_multi,
                PfSymNameArity, HeadMV, TailMVs, MultiSpecs),
            add_warn_specs(MultiSpecs, !Info)
        )
    ).

:- pred warn_singletons_in_goal_list(warn_params::in, list(hlds_goal)::in,
    set_of_progvar::in, warn_info::in, warn_info::out) is det.

warn_singletons_in_goal_list(_, [], _, !Info).
warn_singletons_in_goal_list(Params, [Goal | Goals], QuantVars, !Info) :-
    warn_singletons_in_goal(Params, Goal, QuantVars, !Info),
    warn_singletons_in_goal_list(Params, Goals, QuantVars, !Info).

:- pred warn_singletons_in_cases(warn_params::in, list(case)::in,
    set_of_progvar::in, warn_info::in, warn_info::out) is det.

warn_singletons_in_cases(_, [], _, !Info).
warn_singletons_in_cases(Params, [Case | Cases], QuantVars, !Info) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    warn_singletons_in_goal(Params, Goal, QuantVars, !Info),
    warn_singletons_in_cases(Params, Cases, QuantVars, !Info).

%---------------------------------------------------------------------------%

:- pred warn_singletons_in_scope(warn_params::in,
    scope_reason::in, hlds_goal::in, hlds_goal_info::in, set_of_progvar::in,
    warn_info::in, warn_info::out) is det.

warn_singletons_in_scope(Params, Reason, SubGoal, GoalInfo,
        QuantVars, !Info) :-
    (
        Reason = exist_quant(Vars, Creator),
        (
            Vars = [_ | _],
            !Info ^ wi_seen_quant := have_seen_quant,
            SubGoalVars = free_goal_vars(SubGoal),
            set_of_var.init(EmptySet),
            (
                Creator = user_quant,
                % Warn if any quantified variables occur only
                % in the quantifier.
                warn_singletons_goal_vars(Params, Vars, GoalInfo, EmptySet,
                    SubGoalVars, !Info),
                set_of_var.insert_list(Vars, QuantVars, SubQuantVars)
            ;
                Creator = compiler_quant,
                % If the exist_quant scope was created by the compiler,
                % and not by the user, then there two implications.
                %
                % First, there is no point in generating any warnings
                % about variables that occur nowhere else but in Reason,
                % since if there some, (a) it is the fault of the compiler,
                % and not the user, and (b) the user can do nothing
                % to prevent the compiler's screwup. This is why we
                % don't call warn_singletons_goal_vars here.
                %
                % Second, the occurrence of the variable in Reason
                % does not occur in the source code. Therefore a variable
                % that occurs in Reason and has exactly one occurrence
                % elsewhere *should* get a singleton warning generated
                % for it. This is why we don't add Vars to QuantVars.
                SubQuantVars = QuantVars
            )
        ;
            Vars = [],
            SubQuantVars = QuantVars
        ),
        warn_singletons_in_goal(Params, SubGoal, SubQuantVars, !Info)
    ;
        Reason = promise_solutions(Vars, _),
        (
            Vars = [_ | _],
            % Warn if any quantified variables occur only
            % in the quantifier.
            SubGoalVars = free_goal_vars(SubGoal),
            set_of_var.init(EmptySet),
            warn_singletons_goal_vars(Params, Vars, GoalInfo, EmptySet,
                SubGoalVars, !Info),
            set_of_var.insert_list(Vars, QuantVars, SubQuantVars)
        ;
            Vars = [],
            SubQuantVars = QuantVars
        ),
        warn_singletons_in_goal(Params, SubGoal, SubQuantVars, !Info)
    ;
        Reason = disable_warnings(HeadWarning, TailWarnings),
        ( if
            ( HeadWarning = goal_warning_singleton_vars
            ; list.member(goal_warning_singleton_vars, TailWarnings)
            )
        then
            SubParams0 =
                Params ^ wp_warn_singleton := do_not_warn_singleton
        else
            SubParams0 = Params
        ),
        ( if
            ( HeadWarning = goal_warning_repeated_singleton_vars
            ; list.member(goal_warning_repeated_singleton_vars,
                TailWarnings)
            )
        then
            SubParams = SubParams0 ^ wp_warn_multi := do_not_warn_multi
        else
            SubParams = SubParams0
        ),
        % Note that we *have* to process SubGoal even if both kinds of
        % warnings are now off, because if we do not so, we could miss
        % the only scope goal in the original BodyGoal that requires us
        % to set the wi_seen_quant field to have_seen_quant.
        warn_singletons_in_goal(SubParams, SubGoal, QuantVars, !Info)
    ;
        ( Reason = promise_purity(_)
        ; Reason = require_detism(_)
        ; Reason = require_complete_switch(_)
        ; Reason = require_switch_arms_detism(_, _)
        ; Reason = commit(_)
        ; Reason = barrier(_)
        ; Reason = trace_goal(_, _, _, _, _)
        ),
        warn_singletons_in_goal(Params, SubGoal, QuantVars, !Info)
    ;
        Reason = from_ground_term(TermVar, _Kind),
        % By construction, there can be no singleton variables
        % inside these scopes. The only variable involved in the scope that
        % can possibly be subject to either warning is the one that
        % represents the entire ground term.
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars(Params, [TermVar], GoalInfo, NonLocals,
            QuantVars, !Info)
    ;
        Reason = loop_control(_, _, _),
        % Loop control scopes are only ever introduced
        % by compiler passes that execute after us.
        unexpected($pred, "loop_control")
    ).

%---------------------------------------------------------------------------%

:- pred add_warn_specs(list(error_spec)::in,
    warn_info::in, warn_info::out) is det.

add_warn_specs(NewSpecs, !Info) :-
    Specs0 = !.Info ^ wi_specs,
    Specs = NewSpecs ++ Specs0,
    !Info ^ wi_specs := Specs.

:- type single_or_multi
    --->    sm_single
    ;       sm_multi.

:- pred generate_variable_warning(warn_params::in, prog_context::in,
    single_or_multi::in, pf_sym_name_arity::in,
    prog_var::in, list(prog_var)::in, list(error_spec)::out) is det.

generate_variable_warning(Params, Context, SingleMulti, PfSymNameArity,
        Var0, Vars0, Specs) :-
    ( if
        (
            SingleMulti = sm_single,
            Params ^ wp_warn_singleton = do_not_warn_singleton
        ;
            SingleMulti = sm_multi,
            Params ^ wp_warn_multi = do_not_warn_multi
        )
    then
        Specs = []
    else
        PreamblePieces = [words("In clause for"),
            unqual_pf_sym_name_pred_form_arity(PfSymNameArity),
            suffix(":"), nl],
        Vars = [Var0 | Vars0],
        VarSet = Params ^ wp_varset,
        (
            SingleMulti = sm_single,
            WarnOption = warn_singleton_vars,
            varset.var_name_list(VarSet, AllVarNamesAL),
            assoc_list.values(AllVarNamesAL, AllVarNames),
            separate_state_var_names(AllVarNames,
                [], PlainVarNameList, [], BangVarNameList),
            set.list_to_set(PlainVarNameList, PlainVarNames),
            set.list_to_set(BangVarNameList, BangVarNames),
            generate_singleton_variable_warning_dyms(VarSet, Context,
                PreamblePieces, WarnOption, PlainVarNames, BangVarNames,
                Vars, [], NoDymVarNames, [], Specs0),
            OnlyMoreThanOnce = "only once"
        ;
            SingleMulti = sm_multi,
            WarnOption = warn_repeated_singleton_vars,
            OnlyMoreThanOnce = "more than once",
            NoDymVarNames =
                list.map(mercury_var_to_name_only_vs(VarSet), Vars),
            Specs0 = []
        ),
        generate_variable_warning_no_dym(Context, PreamblePieces, WarnOption,
            OnlyMoreThanOnce, NoDymVarNames, Specs0, Specs)
    ).

%---------------------%

:- pred separate_state_var_names(list(string)::in,
    list(string)::in, list(string)::out,
    list(string)::in, list(string)::out) is det.

separate_state_var_names([], !PlainVarNames, !BangVarNames).
separate_state_var_names([Name | Names], !PlainVarNames, !BangVarNames) :-
    ( if var_name_is_state_var_name(Name, StateVarName) then
        !:BangVarNames =
            ["!." ++ StateVarName, "!:" ++ StateVarName | !.BangVarNames]
    else
        !:PlainVarNames = [Name | !.PlainVarNames]
    ),
    separate_state_var_names(Names, !PlainVarNames, !BangVarNames).

%---------------------%

    % For each singleton variable that is close enough to another variable
    % name that a "did you mean" replacement suggestion is worthwhile,
    % generate an error_spec including that suggestions. Return
    %
    % - the list of such singleton-var-specific error_specs, and
    % - the list of singleton vars for which we have no "did you mean"
    %   suggestion.
    %
    % Our caller will then generate a single error_spec that mentions
    % *all* of the variables without their own "did you mean" suggestions.
    %
:- pred generate_singleton_variable_warning_dyms(prog_varset::in,
    prog_context::in, list(format_piece)::in, option::in,
    set(string)::in, set(string)::in, list(prog_var)::in, list(string)::in,
    list(string)::out, list(error_spec)::in, list(error_spec)::out) is det.

generate_singleton_variable_warning_dyms(_, _, _, _, _, _,
        [], !NoDymVarNames, !Specs).
generate_singleton_variable_warning_dyms(VarSet, Context, PreamblePieces,
        WarnOption, PlainVarNames, BangVarNames, [Var | Vars],
        !NoDymVarNames, !Specs) :-
    VarName = mercury_var_to_name_only_vs(VarSet, Var),
    ( if var_name_is_state_var_name(VarName, StateVarName) then
        DotVarName = "!." ++ StateVarName,
        ColonVarName = "!:" ++ StateVarName,
        DymVarName = DotVarName,
        string.count_code_points(StateVarName, RelevantNameLen),
        % People add a !. or !: prefix to a variable name by accident
        % extremely rarely, so if VarName0 is a reference to a state variable,
        % then restrict the set of suggestions to contain only *other*
        % state variable references.
        %
        % Unfortunately, we do not know whether state_var.m created VarName0
        % for a !. or a !: reference. In the absence of that knowledge,
        % the best we can do is to include both kinds of references in
        % CompareToNames0.
        %
        % Note that very early on, we expand !SS into !.SS, !:SS pairs.
        % Therefore !.SS has a quite high chance of having started out
        % as part of a !SS reference. We do not know whether that is the case
        % (we do not record such information anywhere), which is why
        % recommending either !:SS as a dym replacement for !.SS, or
        % vice versa, is not a good idea.
        set.delete_list([DotVarName, ColonVarName],
            BangVarNames, CompareToNames)
    else
        DymVarName = VarName,
        string.count_code_points(VarName, RelevantNameLen),
        % Compare VarName to both the names of other plain variables,
        % and to references to state variables (both !. and !:).
        set.delete(VarName, PlainVarNames, OtherPlainVarNames),
        set.union(OtherPlainVarNames, BangVarNames, CompareToNames)
    ),
    ( if
        % The maybe_construct_did_you_mean_pieces predicate can, and sometimes
        % will, suggest one-character names (such as q) as replacements
        % for another one-character name (such as r). For its original
        % use-case, predicate and function names, this is fine, because
        % the average number of one-character predicate and/or function names
        % in a module is very close to zero. However, one-character variable
        % names occur in real code much more frequently (usually in generic
        % code), and in scopes that contain several such names, having them
        % suggested as replacements for each other is more distracting
        % than useful.
        RelevantNameLen > 1,
        set.to_sorted_list(CompareToNames, CompareToNamesList),
        maybe_construct_did_you_mean_pieces(DymVarName, CompareToNamesList,
            DymPieces),
        % DymPieces will be [] if we cannot suggest any likely replacement.
        DymPieces = [_ | _]
    then
        generate_singleton_variable_warning_dym(Context, PreamblePieces,
            WarnOption, VarName, DymPieces, DymSpec),
        !:Specs = [DymSpec | !.Specs]
    else
        !:NoDymVarNames = [VarName | !.NoDymVarNames]
    ),
    generate_singleton_variable_warning_dyms(VarSet, Context, PreamblePieces,
        WarnOption, PlainVarNames, BangVarNames, Vars, !NoDymVarNames, !Specs).

:- pred generate_variable_warning_no_dym(prog_context::in,
    list(format_piece)::in, option::in, string::in, list(string)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

generate_variable_warning_no_dym(Context, PreamblePieces, WarnOption,
        OnlyMoreThanOnce, VarNames0, !Specs) :-
    list.sort_and_remove_dups(VarNames0, VarNames),
    (
        VarNames = []
    ;
        (
            VarNames = [VarName],
            WarnPieces = single_var_warning_pieces(VarName, OnlyMoreThanOnce)
        ;
            VarNames = [_, _ | _],
            VarsPieces = quote_list_to_color_pieces(color_subject, "and", [],
                VarNames),
            WarnPieces = [words("warning: variables")] ++ VarsPieces ++
                color_as_incorrect([words("occur"),
                    words(OnlyMoreThanOnce)]) ++
                [words("in this scope."), nl]
        ),
        Spec = spec($pred, severity_warning(WarnOption), phase_pt2h, Context,
            PreamblePieces ++ WarnPieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred generate_singleton_variable_warning_dym(prog_context::in,
    list(format_piece)::in, option::in, string::in, list(format_piece)::in,
    error_spec::out) is det.

generate_singleton_variable_warning_dym(Context, PreamblePieces, WarnOption,
        VarName, DymPieces, Spec) :-
    WarnPieces = single_var_warning_pieces(VarName, "only once"),
    Spec = spec($pred, severity_warning(WarnOption), phase_pt2h, Context,
        PreamblePieces ++ WarnPieces ++ DymPieces).

:- func single_var_warning_pieces(string, string) = list(format_piece).

single_var_warning_pieces(VarName, OnlyMoreThanOnce) = WarnPieces :-
    WarnPieces = [words("warning: variable")] ++
        color_as_subject([quote(VarName)]) ++
        color_as_incorrect([words("occurs"), words(OnlyMoreThanOnce)]) ++
        [words("in this scope."), nl].

:- pred var_name_is_state_var_name(string::in, string::out) is semidet.

var_name_is_state_var_name(Name, StateVarName) :-
    string.remove_prefix("STATE_VARIABLE_", Name, StateVarNameNum),
    require_det (
        StateVarNameNumPieces = string.split_at_char('_', StateVarNameNum),
        ( if
            list.split_last(StateVarNameNumPieces,
                NonLastStateVarNameNumPieces, LastStateVarNameNumPieces),
            string.to_int(LastStateVarNameNumPieces, _SuffixNum)
        then
            StateVarName = string.join_list("_", NonLastStateVarNameNumPieces)
        else
            % There is no number at the end of StateVarNameNum.
            StateVarName = StateVarNameNum
        )
    ).

%---------------------------------------------------------------------------%

:- pred is_singleton_var(set_of_progvar::in,
    set_of_progvar::in, prog_varset::in, prog_var::in) is semidet.

is_singleton_var(NonLocals, QuantVars, VarSet, Var) :-
    not set_of_var.member(NonLocals, Var),
    varset.search_name(VarSet, Var, Name),
    not string.prefix(Name, "_"),
    not string.prefix(Name, "DCG_"),
    not (
        set_of_var.member(QuantVars, QuantVar),
        varset.search_name(VarSet, QuantVar, Name)
    ).

:- pred is_multi_var(set_of_progvar::in, prog_varset::in, prog_var::in)
    is semidet.

is_multi_var(NonLocals, VarSet, Var) :-
    set_of_var.member(NonLocals, Var),
    varset.search_name(VarSet, Var, Name),
    string.prefix(Name, "_").

%---------------------------------------------------------------------------%
%
% Promise_ex error checking.
%

check_promise_ex_decl(UnivVars, PromiseType, Goal, Context, !Specs) :-
    % Are universally quantified variables present?
    (
        UnivVars = [],
        promise_ex_error(PromiseType, Context,
            "declaration has no universally quantified variables", !Specs)
    ;
        UnivVars = [_ | _]
    ),
    check_promise_ex_goal(PromiseType, Goal, !Specs).

    % Check for misplaced universal quantification, otherwise find the
    % disjunction, flatten it out into list form and perform further checks.
    %
:- pred check_promise_ex_goal(promise_type::in, goal::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_promise_ex_goal(PromiseType, Goal, !Specs) :-
    ( if
        Goal = quant_expr(quant_some, quant_ordinary_vars, _, _, SubGoal)
    then
        check_promise_ex_goal(PromiseType, SubGoal, !Specs)
    else if
        Goal = disj_expr(_, Disjunct1, Disjunct2, Disjuncts3plus)
    then
        DisjList = [Disjunct1, Disjunct2 | Disjuncts3plus],
        list.map(flatten_to_conj_list, DisjList, DisjConjList),
        check_promise_ex_disjunction(PromiseType, DisjConjList, !Specs)
    else if
        Goal = quant_expr(quant_all, quant_ordinary_vars, Context, _UnivVars,
            SubGoal)
    then
        promise_ex_error(PromiseType, Context,
            "universal quantification should come before " ++
            "the declaration name", !Specs),
        check_promise_ex_goal(PromiseType, SubGoal, !Specs)
    else
        promise_ex_error(PromiseType, get_goal_context(Goal),
            "goal in declaration is not a disjunction", !Specs)
    ).

    % Takes a goal representing an arm of a disjunction and turns it into
    % a list of conjunct goals.
    %
:- pred flatten_to_conj_list(goal::in, list(goal)::out) is det.

flatten_to_conj_list(Goal, GoalList) :-
    ( if Goal = conj_expr(_, ConjunctA, ConjunctsB) then
        list.map(flatten_to_conj_list, [ConjunctA | ConjunctsB],
            ConjunctGoalLists),
        list.condense(ConjunctGoalLists, GoalList)
    else
        GoalList = [Goal]
    ).

    % Taking a list of arms of the disjunction, check each arm individually.
    %
:- pred check_promise_ex_disjunction(promise_type::in, list(list(goal))::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_promise_ex_disjunction(PromiseType, DisjConjList, !Specs) :-
    (
        DisjConjList = []
    ;
        DisjConjList = [ConjList | Rest],
        check_promise_ex_disj_arm(PromiseType, ConjList, no, !Specs),
        check_promise_ex_disjunction(PromiseType, Rest, !Specs)
    ).

    % Only one goal in an arm is allowed to be a call, the rest must be
    % unifications.
    %
:- pred check_promise_ex_disj_arm(promise_type::in, list(goal)::in, bool::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_promise_ex_disj_arm(PromiseType, Goals, CallUsed, !Specs) :-
    (
        Goals = []
    ;
        Goals = [HeadGoal | TailGoals],
        ( if
            HeadGoal = unify_expr(_, _, _, _)
        then
            check_promise_ex_disj_arm(PromiseType, TailGoals,
                CallUsed, !Specs)
        else if
            HeadGoal = quant_expr(quant_some, quant_ordinary_vars, _, _,
                HeadSubGoal)
            then
            check_promise_ex_disj_arm(PromiseType, [HeadSubGoal | TailGoals],
                CallUsed, !Specs)
        else if
            HeadGoal = call_expr(Context, _, _, _)
        then
            (
                CallUsed = no
            ;
                CallUsed = yes,
                promise_ex_error(PromiseType, Context,
                    "disjunct contains more than one call", !Specs)
            ),
            check_promise_ex_disj_arm(PromiseType, TailGoals, yes, !Specs)
        else
            promise_ex_error(PromiseType, get_goal_context(HeadGoal),
                "disjunct is not a call or unification", !Specs),
            check_promise_ex_disj_arm(PromiseType, TailGoals, CallUsed, !Specs)
        )
    ).

    % Called for any error in the above checks.
    %
:- pred promise_ex_error(promise_type::in, prog_context::in, string::in,
    list(error_spec)::in, list(error_spec)::out) is det.

promise_ex_error(PromiseType, Context, Message, !Specs) :-
    Pieces = [words("In"),
        quote(parse_tree_out_misc.promise_to_string(PromiseType)),
        words("declaration:"), nl,
        words("error:"), words(Message), nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

warn_suspicious_foreign_code(Lang, BodyCode, Context, !Specs) :-
    (
        BodyCode = floi_include_file(_)
    ;
        BodyCode = floi_literal(Code),
        (
            Lang = lang_c,
            foreign_code_to_identifiers(Lang, Code, Identifiers),
            ( if list.member("MR_ALLOC_ID", Identifiers) then
                Pieces = [
                    words("Warning: the body of this"),
                    pragma_decl("foreign_code"),
                    words("declaration may refer to the"),
                    quote("MR_ALLOC_ID"), words("macro."),
                    words("That macro is only defined within the body of"),
                    pragma_decl("foreign_proc"), words("declarations.")
                ],
                Severity = severity_warning(warn_suspicious_foreign_code),
                Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
                !:Specs = [Spec | !.Specs]
            else
                true
            )
        ;
            ( Lang = lang_csharp
            ; Lang = lang_java
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_warn.
%---------------------------------------------------------------------------%
