%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Generate whatever warnings the module being transformed to HLDS deserves.

:- module hlds__make_hlds__make_hlds_warn.
:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__quantification.
:- import_module libs__globals.
:- import_module parse_tree__prog_data.

:- import_module io.
:- import_module list.
:- import_module std_util.

    % Warn about variables which occur only once but don't start with
    % an underscore, or about variables which do start with an underscore
    % but occur more than once.
    %
:- pred maybe_warn_overlap(list(quant_warning)::in, prog_varset::in,
    simple_call_id::in, io::di, io::uo) is det.

    % Warn about variables which occur only once but don't start with
    % an underscore, or about variables which do start with an underscore
    % but occur more than once, or about variables that do not occur in
    % C code strings when they should.
    %
:- pred maybe_warn_singletons(prog_varset::in, simple_call_id::in,
    module_info::in, hlds_goal::in, io::di, io::uo) is det.

:- pred maybe_warn_pragma_singletons(pragma_foreign_code_impl::in,
    foreign_language::in, list(maybe(pair(string, mode)))::in,
    prog_context::in, simple_call_id::in, module_info::in,
    io::di, io::uo) is det.

    % Perform above checks on a promise ex declaration.
    %
:- pred check_promise_ex_decl(prog_vars::in, promise_type::in, goal::in,
    prog_context::in, io::di, io::uo) is det.

    % Check that clauses are not exported.
    %
:- pred check_not_exported(import_status::in, prog_context::in, string::in,
    io::di, io::uo) is det.

:- implementation.

:- import_module check_hlds__mode_util.
:- import_module libs__options.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_out.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_out.

:- import_module bool.
:- import_module char.
:- import_module set.
:- import_module string.
:- import_module varset.

maybe_warn_overlap(Warnings, VarSet, PredCallId, !IO) :-
    globals__io_lookup_bool_option(warn_overlapping_scopes,
        WarnOverlappingScopes, !IO),
    (
        WarnOverlappingScopes = yes,
        warn_overlap(Warnings, VarSet, PredCallId, !IO)
    ;
        WarnOverlappingScopes = no
    ).

:- pred warn_overlap(list(quant_warning)::in, prog_varset::in,
    simple_call_id::in, io::di, io::uo) is det.

warn_overlap([], _, _, !IO).
warn_overlap([Warn | Warns], VarSet, PredCallId, !IO) :-
    Warn = warn_overlap(Vars, Context),
    Part1 = [words("In clause for"),
        words(simple_call_id_to_string(PredCallId)), suffix(":"), nl],
    ( Vars = [Var] ->
        Part2 = [words("warning: variable"),
            words("`" ++ mercury_var_to_string(Var, VarSet, no) ++ "'"),
            words("has overlapping scopes.")]
    ;
        Part2 = [words("warning: variables"),
            words("`" ++ mercury_vars_to_string(Vars, VarSet, no) ++ "'"),
            words("each have overlapping scopes.")]
    ),
    write_error_pieces(Context, 0, Part1 ++ Part2, !IO),
    record_warning(!IO),
    warn_overlap(Warns, VarSet, PredCallId, !IO).

%-----------------------------------------------------------------------------%

maybe_warn_singletons(VarSet, PredCallId, ModuleInfo, Body, !IO) :-
    globals__io_lookup_bool_option(warn_singleton_vars, WarnSingletonVars,
        !IO),
    (
        WarnSingletonVars = yes,
        set__init(QuantVars),
        warn_singletons_in_goal(Body, QuantVars, VarSet, PredCallId,
            ModuleInfo, !IO)
    ;
        WarnSingletonVars = no
    ).

:- pred warn_singletons_in_goal(hlds_goal::in, set(prog_var)::in,
    prog_varset::in, simple_call_id::in, module_info::in,
    io::di, io::uo) is det.

warn_singletons_in_goal(Goal - GoalInfo, QuantVars, VarSet, PredCallId, MI,
        !IO) :-
    warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet,
        PredCallId, MI, !IO).

:- pred warn_singletons_in_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
    set(prog_var)::in, prog_varset::in, simple_call_id::in,
    module_info::in, io::di, io::uo) is det.

warn_singletons_in_goal_2(Goal, _GoalInfo, QuantVars, VarSet, PredCallId,
        MI, !IO) :-
    Goal = conj(Goals),
    warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId, MI,
        !IO).
warn_singletons_in_goal_2(Goal, _GoalInfo, QuantVars, VarSet, PredCallId,
        MI, !IO) :-
    Goal = par_conj(Goals),
    warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId, MI,
        !IO).
warn_singletons_in_goal_2(Goal, _GoalInfo, QuantVars, VarSet, PredCallId,
        MI, !IO) :-
    Goal = disj(Goals),
    warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId, MI, !IO).
warn_singletons_in_goal_2(Goal, _GoalInfo, QuantVars, VarSet, PredCallId,
        MI, !IO) :-
    Goal = switch(_Var, _CanFail, Cases),
    warn_singletons_in_cases(Cases, QuantVars, VarSet, PredCallId, MI, !IO).
warn_singletons_in_goal_2(Goal, _GoalInfo, QuantVars, VarSet, PredCallId,
        MI, !IO) :-
    Goal = not(SubGoal),
    warn_singletons_in_goal(SubGoal, QuantVars, VarSet, PredCallId, MI, !IO).
warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet, PredCallId,
        MI, !IO) :-
    Goal = scope(Reason, SubGoal),
    %
    % warn if any quantified variables occur only in the quantifier
    %
    (
        ( Reason = exist_quant(Vars)
        ; Reason = promise_equivalent_solutions(Vars)
        ),
        Vars = [_ | _]
    ->
        quantification__goal_vars(SubGoal, SubGoalVars),
        goal_info_get_context(GoalInfo, Context),
        set__init(EmptySet),
        warn_singletons(Vars, GoalInfo, EmptySet, SubGoalVars, VarSet,
            Context, PredCallId, !IO),
        set__insert_list(QuantVars, Vars, SubQuantVars)
    ;
        SubQuantVars = QuantVars
    ),
    warn_singletons_in_goal(SubGoal, SubQuantVars, VarSet, PredCallId, MI,
        !IO).
warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet, PredCallId,
        MI, !IO) :-
    Goal = if_then_else(Vars, Cond, Then, Else),
    %
    % warn if any quantified variables do not occur in the condition
    % or the "then" part of the if-then-else
    %
    (
        Vars = [_ | _],
        quantification__goal_vars(Cond, CondVars),
        quantification__goal_vars(Then, ThenVars),
        set__union(CondVars, ThenVars, CondThenVars),
        goal_info_get_context(GoalInfo, Context),
        set__init(EmptySet),
        warn_singletons(Vars, GoalInfo, EmptySet, CondThenVars, VarSet,
            Context, PredCallId, !IO)
    ;
        Vars = []
    ),
    set__insert_list(QuantVars, Vars, QuantVars1),
    warn_singletons_in_goal(Cond, QuantVars1, VarSet, PredCallId, MI, !IO),
    warn_singletons_in_goal(Then, QuantVars1, VarSet, PredCallId, MI, !IO),
    warn_singletons_in_goal(Else, QuantVars, VarSet, PredCallId, MI, !IO).
warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet, PredCallId,
        _, !IO) :-
    Goal = call(_, _, Args, _, _, _),
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    goal_info_get_context(GoalInfo, Context),
    warn_singletons(Args, GoalInfo, NonLocals, QuantVars, VarSet, Context,
        PredCallId, !IO).
warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet, PredCallId,
        _, !IO) :-
    Goal = generic_call(GenericCall, Args0, _, _),
    goal_util__generic_call_vars(GenericCall, Args1),
    list__append(Args0, Args1, Args),
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    goal_info_get_context(GoalInfo, Context),
    warn_singletons(Args, GoalInfo, NonLocals, QuantVars, VarSet, Context,
        PredCallId, !IO).
warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet, PredCallId,
        MI, !IO) :-
    Goal = unify(Var, RHS, _, _, _),
    warn_singletons_in_unify(Var, RHS, GoalInfo, QuantVars, VarSet,
        PredCallId, MI, !IO).
warn_singletons_in_goal_2(Goal, GoalInfo, _QuantVars, _VarSet, PredCallId,
        MI, !IO) :-
    Goal = foreign_proc(Attrs, _, _, Args, _, PragmaImpl),
    goal_info_get_context(GoalInfo, Context),
    Lang = foreign_language(Attrs),
    NamesModes = list__map(foreign_arg_maybe_name_mode, Args),
    warn_singletons_in_pragma_foreign_proc(PragmaImpl, Lang,
        NamesModes, Context, PredCallId, MI, !IO).
warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet, PredCallId,
        MI, !IO) :-
    Goal = shorthand(ShorthandGoal),
    warn_singletons_in_goal_2_shorthand(ShorthandGoal, GoalInfo,
        QuantVars, VarSet, PredCallId, MI, !IO).

:- pred warn_singletons_in_goal_2_shorthand(shorthand_goal_expr::in,
    hlds_goal_info::in, set(prog_var)::in, prog_varset::in,
    simple_call_id::in, module_info::in, io::di, io::uo) is det.

warn_singletons_in_goal_2_shorthand(bi_implication(LHS, RHS), _GoalInfo,
        QuantVars, VarSet, PredCallId, MI, !IO) :-
    warn_singletons_in_goal_list([LHS, RHS], QuantVars, VarSet,
        PredCallId, MI, !IO).

:- pred warn_singletons_in_goal_list(list(hlds_goal)::in, set(prog_var)::in,
    prog_varset::in, simple_call_id::in, module_info::in,
    io::di, io::uo) is det.

warn_singletons_in_goal_list([], _, _, _, _, !IO).
warn_singletons_in_goal_list([Goal | Goals], QuantVars, VarSet, CallPredId,
        MI, !IO) :-
    warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId, MI, !IO),
    warn_singletons_in_goal_list(Goals, QuantVars, VarSet, CallPredId, MI,
        !IO).

:- pred warn_singletons_in_cases(list(case)::in, set(prog_var)::in,
    prog_varset::in, simple_call_id::in, module_info::in,
    io::di, io::uo) is det.

warn_singletons_in_cases([], _, _, _, _, !IO).
warn_singletons_in_cases([Case | Cases], QuantVars, VarSet, CallPredId, MI,
        !IO) :-
    Case = case(_ConsId, Goal),
    warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId, MI, !IO),
    warn_singletons_in_cases(Cases, QuantVars, VarSet, CallPredId, MI, !IO).

:- pred warn_singletons_in_unify(prog_var::in, unify_rhs::in,
    hlds_goal_info::in, set(prog_var)::in, prog_varset::in,
    simple_call_id::in, module_info::in, io::di, io::uo) is det.

warn_singletons_in_unify(X, var(Y), GoalInfo, QuantVars, VarSet, CallPredId, _,
        !IO) :-
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    goal_info_get_context(GoalInfo, Context),
    warn_singletons([X, Y], GoalInfo, NonLocals, QuantVars, VarSet,
        Context, CallPredId, !IO).
warn_singletons_in_unify(X, functor(_ConsId, _, Vars), GoalInfo,
        QuantVars, VarSet, CallPredId, _, !IO) :-
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    goal_info_get_context(GoalInfo, Context),
    warn_singletons([X | Vars], GoalInfo, NonLocals, QuantVars, VarSet,
        Context, CallPredId, !IO).
warn_singletons_in_unify(X, lambda_goal(_Purity, _PredOrFunc, _Eval, _Fix,
        _NonLocals, LambdaVars, _Modes, _Det, LambdaGoal),
        GoalInfo, QuantVars, VarSet, CallPredId, MI, !IO) :-
    %
    % warn if any lambda-quantified variables occur only in the quantifier
    %
    LambdaGoal = _ - LambdaGoalInfo,
    goal_info_get_nonlocals(LambdaGoalInfo, LambdaNonLocals),
    goal_info_get_context(GoalInfo, Context),
    warn_singletons(LambdaVars, GoalInfo, LambdaNonLocals, QuantVars,
        VarSet, Context, CallPredId, !IO),

    %
    % warn if X (the variable we're unifying the lambda expression with)
    % is singleton
    %
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    warn_singletons([X], GoalInfo, NonLocals, QuantVars, VarSet, Context,
        CallPredId, !IO),
    % Warn if the lambda-goal contains singletons.
    warn_singletons_in_goal(LambdaGoal, QuantVars, VarSet, CallPredId, MI, !IO).

%-----------------------------------------------------------------------------%

maybe_warn_pragma_singletons(PragmaImpl, Lang, ArgInfo, Context, CallId, MI,
        !IO) :-
    globals__io_lookup_bool_option(warn_singleton_vars, WarnSingletonVars,
        !IO),
    ( WarnSingletonVars = yes ->
        warn_singletons_in_pragma_foreign_proc(PragmaImpl, Lang,
            ArgInfo, Context, CallId, MI, !IO)
    ;
        true
    ).

    % warn_singletons_in_pragma_foreign_proc checks to see if each variable
    % is mentioned at least once in the foreign code fragments that ought to
    % mention it. If not, it gives a warning.
    %
    % (Note that for some foreign languages it might not be appropriate
    % to do this check, or you may need to add a transformation to map
    % Mercury variable names into identifiers for that foreign language).
    %
:- pred warn_singletons_in_pragma_foreign_proc(pragma_foreign_code_impl::in,
    foreign_language::in, list(maybe(pair(string, mode)))::in,
    prog_context::in, simple_call_id::in, module_info::in,
    io::di, io::uo) is det.

warn_singletons_in_pragma_foreign_proc(PragmaImpl, Lang, Args, Context,
        PredOrFuncCallId, ModuleInfo, !IO) :-
    LangStr = foreign_language_string(Lang),
    (
        PragmaImpl = ordinary(C_Code, _),
        c_code_to_name_list(C_Code, C_CodeList),
        Filter = (pred(Name::out) is nondet :-
            list__member(yes(Name - _), Args),
            \+ string__prefix(Name, "_"),
            \+ list__member(Name, C_CodeList)
        ),
        solutions(Filter, UnmentionedVars),
        (
            UnmentionedVars = []
        ;
            UnmentionedVars = [_ | _],
            prog_out__write_context(Context, !IO),
            io__write_string("In the " ++ LangStr ++ " code for ", !IO),
            hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
            io__write_string(":\n", !IO),
            prog_out__write_context(Context, !IO),
            write_variable_warning_start(UnmentionedVars, !IO),
            io__write_string("not occur in the " ++ LangStr ++ " code.\n", !IO)
        )
    ;
        PragmaImpl = nondet(_, _, FirstCode, _, LaterCode, _, _, SharedCode,
            _),
        c_code_to_name_list(FirstCode, FirstCodeList),
        c_code_to_name_list(LaterCode, LaterCodeList),
        c_code_to_name_list(SharedCode, SharedCodeList),
        InputFilter = (pred(Name::out) is nondet :-
            list__member(yes(Name - Mode), Args),
            mode_is_input(ModuleInfo, Mode),
            \+ string__prefix(Name, "_"),
            \+ list__member(Name, FirstCodeList)
        ),
        solutions(InputFilter, UnmentionedInputVars),
        ( UnmentionedInputVars = [] ->
            true
        ;
            prog_out__write_context(Context, !IO),
            io__write_string("In the " ++ LangStr ++ " code for ", !IO),
            hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
            io__write_string(":\n", !IO),
            prog_out__write_context(Context, !IO),
            write_variable_warning_start(UnmentionedInputVars, !IO),
            io__write_string("not occur in the first " ++
                LangStr ++ " code.\n ", !IO)
        ),
        FirstOutputFilter = (pred(Name::out) is nondet :-
            list__member(yes(Name - Mode), Args),
                mode_is_output(ModuleInfo, Mode),
                \+ string__prefix(Name, "_"),
                \+ list__member(Name, FirstCodeList),
                \+ list__member(Name, SharedCodeList)
        ),
        solutions(FirstOutputFilter, UnmentionedFirstOutputVars),
        (
            UnmentionedFirstOutputVars = []
        ;
            UnmentionedFirstOutputVars = [_ | _],
            prog_out__write_context(Context, !IO),
            io__write_string("In the " ++ LangStr ++ " code for ", !IO),
            hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
            io__write_string(":\n", !IO),
            prog_out__write_context(Context, !IO),
            write_variable_warning_start(UnmentionedFirstOutputVars, !IO),
            io__write_string("not occur in the first " ++ LangStr ++
                " code or the shared " ++ LangStr ++ " code.\n ", !IO)
        ),
        LaterOutputFilter = (pred(Name::out) is nondet :-
            list__member(yes(Name - Mode), Args),
            mode_is_output(ModuleInfo, Mode),
            \+ string__prefix(Name, "_"),
            \+ list__member(Name, LaterCodeList),
            \+ list__member(Name, SharedCodeList)
        ),
        solutions(LaterOutputFilter, UnmentionedLaterOutputVars),
        (
            UnmentionedLaterOutputVars = []
        ;
            UnmentionedLaterOutputVars = [_ | _],
            prog_out__write_context(Context, !IO),
            io__write_string("In the " ++ LangStr ++ " code for ", !IO),
            hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
            io__write_string(":\n", !IO),
            prog_out__write_context(Context, !IO),
            write_variable_warning_start(UnmentionedLaterOutputVars, !IO),
            io__write_string("not occur in the retry " ++ LangStr ++
                " code or the shared " ++ LangStr ++ " code.\n ", !IO)
        )
    ;
        PragmaImpl = import(_, _, _, _)
    ).

:- pred write_variable_warning_start(list(string)::in, io::di, io::uo) is det.

write_variable_warning_start(UnmentionedVars, !IO) :-
    ( UnmentionedVars = [_] ->
        io__write_string("  warning: variable `", !IO),
        write_string_list(UnmentionedVars, !IO),
        io__write_string("' does ", !IO)
    ;
        io__write_string("  warning: variables `", !IO),
        write_string_list(UnmentionedVars, !IO),
        io__write_string("' do ", !IO)
    ).

    % c_code_to_name_list(Code, List) is true iff List is a list of the
    % identifiers used in the C code in Code.
    %
:- pred c_code_to_name_list(string::in, list(string)::out) is det.

c_code_to_name_list(Code, List) :-
    string__to_char_list(Code, CharList),
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
        string__from_char_list(NameCharList, Name),
        List = [Name | Names]
    ).

:- pred get_first_c_name(list(char)::in, list(char)::out, list(char)::out)
    is det.

get_first_c_name([], [], []).
get_first_c_name([C | CodeChars], NameCharList, TheRest) :-
    ( char__is_alnum_or_underscore(C) ->
        get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
        NameCharList = [C | NameCharList0]
    ;
            % strip off any characters in the C code which
            % don't form part of an identifier.
        get_first_c_name(CodeChars, NameCharList, TheRest)
    ).

:- pred get_first_c_name_in_word(list(char)::in, list(char)::out,
    list(char)::out) is det.

get_first_c_name_in_word([], [], []).
get_first_c_name_in_word([C | CodeChars], NameCharList, TheRest) :-
    ( char__is_alnum_or_underscore(C) ->
            % There are more characters in the word
        get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
        NameCharList = [C|NameCharList0]
    ;
            % The word is finished
        NameCharList = [],
        TheRest = CodeChars
    ).

:- pred generate_singleton_vars(list(prog_var)::in, set(prog_var)::in,
    set(prog_var)::in, prog_varset::in, prog_var::out) is nondet.

generate_singleton_vars(GoalVars, NonLocals, QuantVars, VarSet, Var) :-
    list__member(Var, GoalVars),
    \+ set__member(Var, NonLocals),
    varset__search_name(VarSet, Var, Name),
    \+ string__prefix(Name, "_"),
    \+ string__prefix(Name, "DCG_"),
    \+ (
        set__member(QuantVar, QuantVars),
        varset__search_name(VarSet, QuantVar, Name)
    ).

:- pred generate_multi_vars(list(prog_var)::in, set(prog_var)::in,
    prog_varset::in, prog_var::out) is nondet.

generate_multi_vars(GoalVars, NonLocals, VarSet, Var) :-
    list__member(Var, GoalVars),
    set__member(Var, NonLocals),
    varset__search_name(VarSet, Var, Name),
    string__prefix(Name, "_").

    % warn_singletons(Vars, GoalInfo, NonLocals, QuantVars, ...):
    %
    % Warn if any of the non-underscore variables in Vars don't occur in
    % NonLocals and don't have the same name as any variable in QuantVars,
    % or if any of the underscore variables in Vars do occur in NonLocals.
    % Omit the warning if GoalInfo says we should.
    %
:- pred warn_singletons(list(prog_var)::in, hlds_goal_info::in,
    set(prog_var)::in, set(prog_var)::in, prog_varset::in,
    prog_context::in, simple_call_id::in, io::di, io::uo) is det.

warn_singletons(GoalVars, GoalInfo, NonLocals, QuantVars, VarSet, Context,
        PredOrFuncCallId, !IO) :-
    % Find all the variables in the goal that don't occur outside the goal
    % (i.e. are singleton), have a variable name that doesn't start with "_"
    % or "DCG_", and don't have the same name as any variable in QuantVars
    % (i.e. weren't explicitly quantified).

    solutions(generate_singleton_vars(GoalVars, NonLocals, QuantVars, VarSet),
        SingletonVars),

    % if there were any such variables, issue a warning

    (
        (
            SingletonVars = []
        ;
            goal_info_has_feature(GoalInfo, dont_warn_singleton)
        )
    ->
        true
    ;
        prog_out__write_context(Context, !IO),
        io__write_string("In clause for ", !IO),
        hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
        io__write_string(":\n", !IO),
        prog_out__write_context(Context, !IO),
        ( SingletonVars = [_] ->
            io__write_string("  warning: variable `", !IO),
            mercury_output_vars(SingletonVars, VarSet, no, !IO),
            report_warning("' occurs only once in this scope.\n", !IO)
        ;
            io__write_string("  warning: variables `", !IO),
            mercury_output_vars(SingletonVars, VarSet, no, !IO),
            report_warning("' occur only once in this scope.\n", !IO)
        )
    ),

    % Find all the variables in the goal that do occur outside the goal
    % (i.e. are not singleton) and have a variable name that starts
    % with "_". If there were any such variables, issue a warning.

    solutions(generate_multi_vars(GoalVars, NonLocals, VarSet), MultiVars),
    (
        MultiVars = []
    ;
        MultiVars = [_ | _],
        prog_out__write_context(Context, !IO),
        io__write_string("In clause for ", !IO),
        hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
        io__write_string(":\n", !IO),
        prog_out__write_context(Context, !IO),
        ( MultiVars = [_] ->
            io__write_string("  warning: variable `", !IO),
            mercury_output_vars(MultiVars, VarSet, no, !IO),
            report_warning("' occurs more than once in this scope.\n", !IO)
        ;
            io__write_string("  warning: variables `", !IO),
            mercury_output_vars(MultiVars, VarSet, no, !IO),
            report_warning("' occur more than once in this scope.\n", !IO)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Promise_ex error checking.
%
% The following predicates are used to perform extra error checking specific
% to promise ex declarations (see notes/promise_ex.html). Currently, the
% following checks are performed:
%
% - check for universally quantified variables
% - check if universal quantification is placed in the wrong position
%   (i.e. after the `promise_exclusive' rather than before it)
% - check that its goal is a disjunction and that each arm of the
%   disjunction has at most one call, and otherwise has only unifications.

check_promise_ex_decl(UnivVars, PromiseType, Goal, Context, !IO) :-
        % are universally quantified variables present?
    (
        UnivVars = [],
        promise_ex_error(PromiseType, Context,
            "declaration has no universally quantified variables", !IO)
    ;
        UnivVars = [_ | _]
    ),
    check_promise_ex_goal(PromiseType, Goal, !IO).

    % Check for misplaced universal quantification, otherwise find the
    % disjunction, flatten it out into list form and perform further checks.
    %
:- pred check_promise_ex_goal(promise_type::in, goal::in, io::di, io::uo)
    is det.

check_promise_ex_goal(PromiseType, GoalExpr - Context, !IO) :-
    ( GoalExpr = some(_, Goal) -> check_promise_ex_goal(PromiseType, Goal, !IO)
    ; GoalExpr =  ( _ ; _ ) ->
        flatten_to_disj_list(GoalExpr - Context, DisjList),
        list__map(flatten_to_conj_list, DisjList, DisjConjList),
        check_disjunction(PromiseType, DisjConjList, !IO)
    ; GoalExpr = all(_UnivVars, Goal) ->
        promise_ex_error(PromiseType, Context,
            "universal quantification should come before " ++
            "the declaration name", !IO),
        check_promise_ex_goal(PromiseType, Goal, !IO)
    ;
        promise_ex_error(PromiseType, Context,
            "goal in declaration is not a disjunction", !IO)
    ).

    % Turns the goal of a promise_ex declaration into a list of goals,
    % where each goal is an arm of the disjunction.
    %
:- pred flatten_to_disj_list(goal::in, goals::out) is det.

flatten_to_disj_list(GoalExpr - Context, GoalList) :-
    ( GoalExpr = ( GoalA ; GoalB ) ->
        flatten_to_disj_list(GoalA, GoalListA),
        flatten_to_disj_list(GoalB, GoalListB),
        GoalList = GoalListA ++ GoalListB
    ;
        GoalList = [GoalExpr - Context]
    ).

    % Takes a goal representing an arm of a disjunction and turns it into
    % a list of conjunct goals.
    %
:- pred flatten_to_conj_list(goal::in, goals::out) is det.

flatten_to_conj_list(GoalExpr - Context, GoalList) :-
    ( GoalExpr = ( GoalA , GoalB ) ->
        flatten_to_conj_list(GoalA, GoalListA),
        flatten_to_conj_list(GoalB, GoalListB),
        GoalList = GoalListA ++ GoalListB
    ;
        GoalList = [GoalExpr - Context]
    ).

    % Taking a list of arms of the disjunction, check each arm individually.
    %
:- pred check_disjunction(promise_type::in, list(goals)::in, io::di, io::uo)
    is det.

check_disjunction(PromiseType, DisjConjList, !IO) :-
    (
        DisjConjList = []
    ;
        DisjConjList = [ConjList | Rest],
        check_disj_arm(PromiseType, ConjList, no, !IO),
        check_disjunction(PromiseType, Rest, !IO)
    ).

    % Only one goal in an arm is allowed to be a call, the rest must be
    % unifications.
    %
:- pred check_disj_arm(promise_type::in, goals::in, bool::in,
    io::di, io::uo) is det.

check_disj_arm(PromiseType, Goals, CallUsed, !IO) :-
    (
        Goals = []
    ;
        Goals = [GoalExpr - Context | Rest],
        ( GoalExpr = unify(_, _, _) ->
            check_disj_arm(PromiseType, Rest, CallUsed, !IO)
        ; GoalExpr = some(_, Goal) ->
            check_disj_arm(PromiseType, [Goal | Rest], CallUsed, !IO)
        ; GoalExpr = call(_, _, _) ->
            (
                CallUsed = no
            ;
                CallUsed = yes,
                promise_ex_error(PromiseType, Context,
                    "disjunct contains more than one call", !IO)
            ),
            check_disj_arm(PromiseType, Rest, yes, !IO)
        ;
            promise_ex_error(PromiseType, Context,
                "disjunct is not a call or unification", !IO),
            check_disj_arm(PromiseType, Rest, CallUsed, !IO)
        )
    ).

    % Called for any error in the above checks.
    %
:- pred promise_ex_error(promise_type::in, prog_context::in, string::in,
    io::di, io::uo) is det.

promise_ex_error(PromiseType, Context, Message, !IO) :-
    ErrorPieces = [
        words("In"),
        fixed("`" ++ prog_out__promise_to_string(PromiseType) ++ "'"),
        words("declaration:"),
        nl,
        words("error:"),
        words(Message)
    ],
    error_util__write_error_pieces(Context, 0, ErrorPieces, !IO).

check_not_exported(Status, Context, Message, !IO) :-
    ( Status = exported ->
        prog_out__write_context(Context, !IO),
        string__append_list(["Warning: ", Message, " in module interface.\n"],
            WarningMessage),
        report_warning(WarningMessage, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
