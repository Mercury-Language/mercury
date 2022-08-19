%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
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
:- import_module hlds.hlds_pred.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Warn about variables with overlapping scopes.
    %
:- pred add_quant_warnings(pf_sym_name_arity::in, prog_varset::in,
    list(quant_warning)::in, list(error_spec)::in, list(error_spec)::out)
    is det.

    % Warn about variables which occur only once but don't start with
    % an underscore, or about variables which do start with an underscore
    % but occur more than once, or about variables that do not occur in
    % C code strings when they should.
    %
:- pred warn_singletons(module_info::in, pf_sym_name_arity::in,
    prog_varset::in, hlds_goal::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % warn_singletons_in_pragma_foreign_proc checks to see if each variable
    % is mentioned at least once in the foreign code fragments that ought to
    % mention it. If not, it gives a warning.
    %
    % (Note that for some foreign languages it might not be appropriate
    % to do this check, or you may need to add a transformation to map
    % Mercury variable names into identifiers for that foreign language).
    %
:- pred warn_singletons_in_pragma_foreign_proc(module_info::in,
    pragma_foreign_proc_impl::in, foreign_language::in,
    list(maybe(foreign_arg_name_mode))::in, prog_context::in,
    pf_sym_name_arity::in, pred_id::in, proc_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This predicate performs the following checks on promise ex declarations
    % (see notes/promise_ex.html).
    %
    % - check for universally quantified variables
    % - check if universal quantification is placed in the wrong position
    %   (i.e. after the `promise_exclusive' rather than before it)
    % - check that its goal is a disjunction and that each arm of the
    %   disjunction has at most one call, and otherwise has only unifications.
    %
:- pred check_promise_ex_decl(prog_vars::in, promise_type::in, goal::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

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

:- import_module hlds.goal_util.
:- import_module hlds.status.
:- import_module libs.options.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

add_quant_warnings(PredCallId, VarSet, Warnings, !Specs) :-
    WarningSpecs =
        list.map(quant_warning_to_spec(PredCallId, VarSet), Warnings),
    !:Specs = WarningSpecs ++ !.Specs.

:- func quant_warning_to_spec(pf_sym_name_arity, prog_varset, quant_warning)
    = error_spec.

quant_warning_to_spec(PredCallId, VarSet, Warning) = Spec :-
    Warning = warn_overlap(Vars, Context),
    Pieces1 = [words("In clause for"),
        unqual_pf_sym_name_orig_arity(PredCallId), suffix(":"), nl],
    (
        Vars = [],
        unexpected($pred, "Vars = []")
    ;
        Vars = [Var],
        Pieces2 = [words("warning: variable"),
            quote(mercury_var_to_name_only_vs(VarSet, Var)),
            words("has overlapping scopes."), nl]
    ;
        Vars = [_, _ | _],
        Pieces2 = [words("warning: variables"),
            quote(mercury_vars_to_name_only_vs(VarSet, Vars)),
            words("each have overlapping scopes."), nl]
    ),
    Spec = conditional_spec($pred, warn_overlapping_scopes, yes,
        severity_warning, phase_parse_tree_to_hlds,
        [simplest_msg(Context, Pieces1 ++ Pieces2)]).

%---------------------------------------------------------------------------%

warn_singletons(ModuleInfo, PredCallId, VarSet, Body, !Specs) :-
    % We handle warnings about variables in the clause head specially.
    % This is because the compiler transforms clause heads such as
    %
    % p(X, Y, Z) :- ...
    %
    % into
    %
    % p(HV1, HV2, HV3) :- HV1 = X, HV2 = Y, HV3 = Z, ...
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

    Info0 = warn_info(ModuleInfo, PredCallId, VarSet,
        [], set_of_var.init, set_of_var.init, context_init),
    QuantVars = set_of_var.init,
    warn_singletons_in_goal(Body, QuantVars, Info0, Info),
    Info = warn_info(_ModuleInfo, _PredCallId, _VarSet,
        NewSpecs, SingletonHeadVarsSet, MultiHeadVarsSet, HeadContext),
    !:Specs = NewSpecs ++ !.Specs,
    set_of_var.to_sorted_list(SingletonHeadVarsSet, SingletonHeadVars),
    set_of_var.to_sorted_list(MultiHeadVarsSet, MultiHeadVars),
    (
        SingletonHeadVars = []
    ;
        SingletonHeadVars = [_ | _],
        generate_variable_warning(sm_single, HeadContext, PredCallId, VarSet,
            SingletonHeadVars, SingleSpec),
        !:Specs = [SingleSpec | !.Specs]
    ),
    (
        MultiHeadVars = []
    ;
        MultiHeadVars = [_ | _],
        generate_variable_warning(sm_multi, HeadContext, PredCallId, VarSet,
            MultiHeadVars, MultiSpec),
        !:Specs = [MultiSpec | !.Specs]
    ).

:- type warn_info
    --->    warn_info(
                % The current module.
                wi_module_info          :: module_info,

                % The id and the varset of the procedure whose body
                % we are checking.
                wi_pred_call_id         :: pf_sym_name_arity,
                wi_varset               :: prog_varset,

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
                wi_head_context         :: prog_context
            ).

:- pred warn_singletons_in_goal(hlds_goal::in, set_of_progvar::in,
    warn_info::in, warn_info::out) is det.

warn_singletons_in_goal(Goal, QuantVars, !Info) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = conj(_ConjType, Goals),
        warn_singletons_in_goal_list(Goals, QuantVars, !Info)
    ;
        GoalExpr = disj(Goals),
        warn_singletons_in_goal_list(Goals, QuantVars, !Info)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        warn_singletons_in_cases(Cases, QuantVars, !Info)
    ;
        GoalExpr = negation(SubGoal),
        warn_singletons_in_goal(SubGoal, QuantVars, !Info)
    ;
        GoalExpr = scope(Reason, SubGoal),
        % Warn if any quantified variables occur only in the quantifier.
        (
            ( Reason = exist_quant(Vars)
            ; Reason = promise_solutions(Vars, _)
            ),
            (
                Vars = [_ | _],
                SubGoalVars = free_goal_vars(SubGoal),
                set_of_var.init(EmptySet),
                warn_singletons_goal_vars(Vars, GoalInfo, EmptySet,
                    SubGoalVars, !Info),
                set_of_var.insert_list(Vars, QuantVars, SubQuantVars)
            ;
                Vars = [],
                SubQuantVars = QuantVars
            ),
            warn_singletons_in_goal(SubGoal, SubQuantVars, !Info)
        ;
            Reason = disable_warnings(HeadWarning, TailWarnings),
            ( if
                ( HeadWarning = goal_warning_singleton_vars
                ; list.member(goal_warning_singleton_vars, TailWarnings)
                )
            then
                % Since we don't want to generate any singleton variable
                % warnings inside this scope, there is no point in examining
                % the goals inside this scope.
                true
            else
                warn_singletons_in_goal(SubGoal, QuantVars, !Info)
            )
        ;
            ( Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ),
            warn_singletons_in_goal(SubGoal, QuantVars, !Info)
        ;
            Reason = from_ground_term(TermVar, _Kind),
            % There can be no singleton variables inside the scopes by
            % construction. The only variable involved in the scope that
            % can possibly be singleton is the one representing the entire
            % ground term.
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            warn_singletons_goal_vars([TermVar], GoalInfo, NonLocals,
                QuantVars, !Info)
        ;
            Reason = loop_control(_, _, _),
            % These scopes are introduced only by compiler passes
            % that execute after us.
            sorry($pred, "loop_control")
        )
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),

        % Warn if any quantified variables do not occur in the condition
        % or the "then" part of the if-then-else.
        (
            Vars = [_ | _],
            CondVars = free_goal_vars(Cond),
            ThenVars = free_goal_vars(Then),
            set_of_var.union(CondVars, ThenVars, CondThenVars),
            set_of_var.init(EmptySet),
            warn_singletons_goal_vars(Vars, GoalInfo, EmptySet, CondThenVars,
                !Info)
        ;
            Vars = []
        ),
        set_of_var.insert_list(Vars, QuantVars, CondThenQuantVars),
        warn_singletons_in_goal(Cond, CondThenQuantVars, !Info),
        warn_singletons_in_goal(Then, CondThenQuantVars, !Info),
        warn_singletons_in_goal(Else, QuantVars, !Info)
    ;
        GoalExpr = plain_call(_, _, Args, _, _, _),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars(Args, GoalInfo, NonLocals, QuantVars, !Info)
    ;
        GoalExpr = generic_call(GenericCall, Args0, _, _, _),
        goal_util.generic_call_vars(GenericCall, Args1),
        Args = Args0 ++ Args1,
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars(Args, GoalInfo, NonLocals, QuantVars, !Info)
    ;
        GoalExpr = unify(Var, RHS, _, _, _),
        warn_singletons_in_unify(Var, RHS, GoalInfo, QuantVars, !Info)
    ;
        GoalExpr = call_foreign_proc(Attrs, PredId, ProcId, Args, _, _,
            PragmaImpl),
        Context = goal_info_get_context(GoalInfo),
        Lang = get_foreign_language(Attrs),
        NamesModes = list.map(foreign_arg_maybe_name_mode, Args),
        warn_singletons_in_pragma_foreign_proc(!.Info ^ wi_module_info,
            PragmaImpl, Lang, NamesModes, Context, !.Info ^ wi_pred_call_id,
            PredId, ProcId, [], PragmaSpecs),
        list.foldl(add_warn_spec, PragmaSpecs, !Info)
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
            warn_singletons_in_goal(MainGoal, InsideQuantVars, !Info),
            warn_singletons_in_goal_list(OrElseGoals, InsideQuantVars, !Info)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            warn_singletons_in_goal(SubGoal, QuantVars, !Info)
        ;
            ShortHand = bi_implication(GoalA, GoalB),
            warn_singletons_in_goal_list([GoalA, GoalB], QuantVars, !Info)
        )
    ).

:- pred warn_singletons_in_goal_list(list(hlds_goal)::in, set_of_progvar::in,
    warn_info::in, warn_info::out) is det.

warn_singletons_in_goal_list([], _, !Info).
warn_singletons_in_goal_list([Goal | Goals], QuantVars, !Info) :-
    warn_singletons_in_goal(Goal, QuantVars, !Info),
    warn_singletons_in_goal_list(Goals, QuantVars, !Info).

:- pred warn_singletons_in_cases(list(case)::in, set_of_progvar::in,
    warn_info::in, warn_info::out) is det.

warn_singletons_in_cases([], _, !Info).
warn_singletons_in_cases([Case | Cases], QuantVars, !Info) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    warn_singletons_in_goal(Goal, QuantVars, !Info),
    warn_singletons_in_cases(Cases, QuantVars, !Info).

:- pred warn_singletons_in_unify(prog_var::in,
    unify_rhs::in, hlds_goal_info::in, set_of_progvar::in,
    warn_info::in, warn_info::out) is det.

warn_singletons_in_unify(X, RHS, GoalInfo, QuantVars, !Info) :-
    (
        RHS = rhs_var(Y),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars([X, Y], GoalInfo, NonLocals, QuantVars,
            !Info)
    ;
        RHS = rhs_functor(_ConsId, _, Ys),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars([X | Ys], GoalInfo, NonLocals, QuantVars,
            !Info)
    ;
        RHS = rhs_lambda_goal(_Purity, _Groundness, _PredOrFunc,
            _Eval, _NonLocals, ArgVarsModes, _Det, LambdaGoal),
        assoc_list.keys(ArgVarsModes, ArgVars),
        % Warn if any lambda-quantified variables occur only in the quantifier.
        LambdaGoal = hlds_goal(_, LambdaGoalInfo),
        LambdaNonLocals = goal_info_get_nonlocals(LambdaGoalInfo),
        warn_singletons_goal_vars(ArgVars, GoalInfo, LambdaNonLocals,
            QuantVars, !Info),

        % Warn if X (the variable we're unifying the lambda expression with)
        % is singleton.
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        warn_singletons_goal_vars([X], GoalInfo, NonLocals, QuantVars, !Info),

        % Warn if the lambda-goal contains singletons.
        warn_singletons_in_goal(LambdaGoal, QuantVars, !Info)
    ).

%---------------------------------------------------------------------------%

    % warn_singletons_goal_vars(Vars, GoalInfo, NonLocals, QuantVars, ...):
    %
    % Warn if any of the non-underscore variables in Vars don't occur in
    % NonLocals and don't have the same name as any variable in QuantVars,
    % or if any of the underscore variables in Vars do occur in NonLocals.
    % Omit the warning if GoalInfo says we should.
    %
:- pred warn_singletons_goal_vars(list(prog_var)::in,
    hlds_goal_info::in, set_of_progvar::in, set_of_progvar::in,
    warn_info::in, warn_info::out) is det.

warn_singletons_goal_vars(GoalVars, GoalInfo, NonLocals, QuantVars, !Info) :-
    % Find all the variables in the goal that don't occur outside the goal
    % (i.e. are singleton), have a variable name that doesn't start with "_"
    % or "DCG_", and don't have the same name as any variable in QuantVars
    % (i.e. weren't explicitly quantified).

    VarSet = !.Info ^ wi_varset,
    CallId = !.Info ^ wi_pred_call_id,
    Context = goal_info_get_context(GoalInfo),

    list.filter(is_singleton_var(NonLocals, QuantVars, VarSet), GoalVars,
        SingleVars),

    % If there were any such variables, issue a warning.
    ( if
        ( SingleVars = []
        ; goal_info_has_feature(GoalInfo, feature_dont_warn_singleton)
        )
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
            generate_variable_warning(sm_single, Context, CallId, VarSet,
                SingleVars, SingleSpec),
            add_warn_spec(SingleSpec, !Info)
        )
    ),

    % Find all the variables in the goal that do occur outside the goal
    % (i.e. are not singleton) and have a variable name that starts
    % with "_". If there were any such variables, issue a warning.

    list.filter(is_multi_var(NonLocals, VarSet), GoalVars, MultiVars),
    (
        MultiVars = []
    ;
        MultiVars = [_ | _],
        ( if goal_info_has_feature(GoalInfo, feature_from_head) then
            MultiHeadVars0 = !.Info ^ wi_multi_headvars,
            set_of_var.insert_list(MultiVars, MultiHeadVars0, MultiHeadVars),
            !Info ^ wi_multi_headvars := MultiHeadVars,
            !Info ^ wi_head_context := goal_info_get_context(GoalInfo)
        else
            generate_variable_warning(sm_multi, Context, CallId, VarSet,
                MultiVars, MultiSpec),
            add_warn_spec(MultiSpec, !Info)
        )
    ).

:- type single_or_multi
    --->    sm_single
    ;       sm_multi.

:- pred generate_variable_warning(single_or_multi::in, prog_context::in,
    pf_sym_name_arity::in, prog_varset::in, list(prog_var)::in,
    error_spec::out) is det.

generate_variable_warning(SingleMulti, Context, CallId, VarSet, Vars, Spec) :-
    (
        SingleMulti = sm_single,
        Count = "only once"
    ;
        SingleMulti = sm_multi,
        Count = "more than once"
    ),
    Preamble = [words("In clause for"),
        unqual_pf_sym_name_orig_arity(CallId), suffix(":"), nl],
    VarStrs0 = list.map(mercury_var_to_name_only_vs(VarSet), Vars),
    list.sort_and_remove_dups(VarStrs0, VarStrs),
    VarsStr = "`" ++ string.join_list(", ", VarStrs) ++ "'",
    % We want VarsPiece to be breakable into two or more lines
    % in case VarsStr does not fit on one line.
    VarsPiece = words(VarsStr),
    (
        VarStrs = [],
        unexpected($pred, "VarStrs = []")
    ;
        VarStrs = [_],
        Pieces = [words("warning: variable"), VarsPiece,
            words("occurs"), words(Count), words("in this scope."), nl]
    ;
        VarStrs = [_, _ | _],
        Pieces = [words("warning: variables"), VarsPiece,
            words("occur"), words(Count), words("in this scope."), nl]
    ),
    Spec = conditional_spec($pred, warn_singleton_vars, yes,
        severity_warning, phase_parse_tree_to_hlds,
        [simplest_msg(Context, Preamble ++ Pieces)]).

:- pred add_warn_spec(error_spec::in, warn_info::in, warn_info::out) is det.

add_warn_spec(Spec, !Info) :-
    Specs0 = !.Info ^ wi_specs,
    Specs = [Spec | Specs0],
    !Info ^ wi_specs := Specs.

%---------------------------------------------------------------------------%

warn_singletons_in_pragma_foreign_proc(ModuleInfo, PragmaImpl, Lang,
        Args, Context, PFSymNameArity, PredId, ProcId, !Specs) :-
    LangStr = foreign_language_string(Lang),
    PragmaImpl = fp_impl_ordinary(Code, _),
    c_code_to_name_list(Code, C_CodeList),
    list.filter_map(var_is_unmentioned(C_CodeList), Args, UnmentionedVars),
    (
        UnmentionedVars = []
    ;
        UnmentionedVars = [_ | _],
        Pieces = [words("In the"), words(LangStr), words("code for"),
            unqual_pf_sym_name_orig_arity(PFSymNameArity), suffix(":"), nl] ++
            variable_warning_start(UnmentionedVars) ++
            [words("not occur in the"), words(LangStr), words("code."), nl],
        Spec = conditional_spec($pred, warn_singleton_vars, yes,
            severity_warning, phase_parse_tree_to_hlds,
            [simplest_msg(Context, Pieces)]),
        !:Specs = [Spec | !.Specs]
    ),
    pragma_foreign_proc_body_checks(ModuleInfo, Lang, Context, PFSymNameArity,
        PredId, ProcId, C_CodeList, !Specs).

:- pred var_is_unmentioned(list(string)::in, maybe(foreign_arg_name_mode)::in,
    string::out) is semidet.

var_is_unmentioned(NameList1, MaybeArg, Name) :-
    MaybeArg = yes(foreign_arg_name_mode(Name, _Mode)),
    not string.prefix(Name, "_"),
    not list.member(Name, NameList1).

:- func variable_warning_start(list(string)) = list(format_component).

variable_warning_start(UnmentionedVars) = Pieces :-
    ( if UnmentionedVars = [Var] then
        Pieces = [words("warning: variable"), quote(Var), words("does")]
    else
        Pieces = [words("warning: variables"),
            words(add_quotes(string.join_list(", ", UnmentionedVars))),
            words("do")]
    ).

    % c_code_to_name_list(Code, List) is true iff List is a list of the
    % identifiers used in the C code in Code.
    %
:- pred c_code_to_name_list(string::in, list(string)::out) is det.

c_code_to_name_list(Code, List) :-
    string.to_char_list(Code, CharList),
    c_code_to_name_list_2(CharList, List).

:- pred c_code_to_name_list_2(list(char)::in, list(string)::out) is det.

c_code_to_name_list_2(C_Code, List) :-
    get_first_c_name(C_Code, NameCharList, TheRest),
    (
        NameCharList = [],
        % no names left
        List = []
    ;
        NameCharList = [_ | _],
        c_code_to_name_list_2(TheRest, Names),
        string.from_char_list(NameCharList, Name),
        List = [Name | Names]
    ).

:- pred get_first_c_name(list(char)::in, list(char)::out, list(char)::out)
    is det.

get_first_c_name([], [], []).
get_first_c_name([C | CodeChars], NameCharList, TheRest) :-
    ( if char.is_alnum_or_underscore(C) then
        get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
        NameCharList = [C | NameCharList0]
    else
        % Strip off any characters in the C code which don't form part
        % of an identifier.
        get_first_c_name(CodeChars, NameCharList, TheRest)
    ).

:- pred get_first_c_name_in_word(list(char)::in, list(char)::out,
    list(char)::out) is det.

get_first_c_name_in_word([], [], []).
get_first_c_name_in_word([C | CodeChars], NameCharList, TheRest) :-
    ( if char.is_alnum_or_underscore(C) then
        % There are more characters in the word.
        get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
        NameCharList = [C|NameCharList0]
    else
        % The word is finished.
        NameCharList = [],
        TheRest = CodeChars
    ).

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

:- pred pragma_foreign_proc_body_checks(module_info::in, foreign_language::in,
    prog_context::in, pf_sym_name_arity::in, pred_id::in, proc_id::in,
    list(string)::in, list(error_spec)::in, list(error_spec)::out) is det.

pragma_foreign_proc_body_checks(ModuleInfo, Lang, Context, PFSymNameArity,
        PredId, ProcId, BodyPieces, !Specs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    IsImported = pred_status_is_imported(PredStatus),
    (
        IsImported = yes
    ;
        IsImported = no,
        check_fp_body_for_success_indicator(ModuleInfo, Lang, Context,
            PFSymNameArity, PredId, ProcId, BodyPieces, !Specs),
        check_fp_body_for_return(Lang, Context, PFSymNameArity, BodyPieces,
            !Specs)
    ).

:- pred check_fp_body_for_success_indicator(module_info::in,
    foreign_language::in, prog_context::in, pf_sym_name_arity::in,
    pred_id::in, proc_id::in, list(string)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_fp_body_for_success_indicator(ModuleInfo, Lang, Context, PFSymNameArity,
        PredId, ProcId, BodyPieces, !Specs) :-
    module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_declared_determinism(ProcInfo, MaybeDeclDetism),
    (
        MaybeDeclDetism = yes(Detism),
        SuccIndStr = "SUCCESS_INDICATOR",
        (
            ( Detism = detism_det
            ; Detism = detism_cc_multi
            ; Detism = detism_erroneous
            ),
            ( if list.member(SuccIndStr, BodyPieces) then
                LangStr = foreign_language_string(Lang),
                Pieces = [words("Warning: the"), fixed(LangStr),
                    words("code for"),
                    unqual_pf_sym_name_orig_arity(PFSymNameArity),
                    words("may set"), quote(SuccIndStr), suffix(","),
                    words("but it cannot fail.")],
                Spec = conditional_spec($pred,
                    warn_suspicious_foreign_procs, yes,
                    severity_warning, phase_parse_tree_to_hlds,
                    [simplest_msg(Context, Pieces)]),
                !:Specs = [Spec | !.Specs]
            else
                true
            )
        ;
            ( Detism = detism_semi
            ; Detism = detism_cc_non
            ),
            ( if list.member(SuccIndStr, BodyPieces) then
                true
            else
                LangStr = foreign_language_string(Lang),
                Pieces = [words("Warning: the"), fixed(LangStr),
                    words("code for"),
                    unqual_pf_sym_name_orig_arity(PFSymNameArity),
                    words("does not appear to set"),
                    quote(SuccIndStr), suffix(","),
                    words("but it can fail.")],
                Spec = conditional_spec($pred,
                    warn_suspicious_foreign_procs, yes,
                    severity_warning, phase_parse_tree_to_hlds,
                    [simplest_msg(Context, Pieces)]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            ( Detism = detism_multi
            ; Detism = detism_non
            ; Detism = detism_failure
            )
        )
    ;
        MaybeDeclDetism = no
    ).

    % Check to see if a foreign_proc body contains a return statement
    % (or whatever the foreign language equivalent is).
    %
:- pred check_fp_body_for_return(foreign_language::in, prog_context::in,
    pf_sym_name_arity::in, list(string)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_fp_body_for_return(Lang, Context, PFSymNameArity, BodyPieces, !Specs) :-
    ( if list.member("return", BodyPieces) then
        LangStr = foreign_language_string(Lang),
        Pieces = [words("Warning: the"), fixed(LangStr),
            words("code for"),
            unqual_pf_sym_name_orig_arity(PFSymNameArity),
            words("may contain a"), quote("return"),
            words("statement."), nl],
        Spec = conditional_spec($pred, warn_suspicious_foreign_procs, yes,
            severity_warning, phase_parse_tree_to_hlds,
            [simplest_msg(Context, Pieces)]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

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
    Pieces = [words("In"), quote(prog_out.promise_to_string(PromiseType)),
        words("declaration:"), nl,
        words("error:"), words(Message), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

warn_suspicious_foreign_code(Lang, BodyCode, Context, !Specs) :-
    (
        BodyCode = floi_include_file(_)
    ;
        BodyCode = floi_literal(Code),
        (
            Lang = lang_c,
            c_code_to_name_list(Code, C_CodeList),
            ( if list.member("MR_ALLOC_ID", C_CodeList) then
                Pieces = [
                    words("Warning: the body of this"),
                    pragma_decl("foreign_code"),
                    words("declaration may refer to the"),
                    quote("MR_ALLOC_ID"), words("macro."),
                    words("That macro is only defined within the body of"),
                    pragma_decl("foreign_proc"), words("declarations.")
                ],
                Spec = conditional_spec($pred, warn_suspicious_foreign_code,
                    yes, severity_warning, phase_parse_tree_to_hlds,
                    [simplest_msg(Context, Pieces)]),
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
