%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: superhomogeneous_lambda.m.
% Main author of the original version of this module: fjh.
% Main author of the current version of this module: zs.
%
% This module performs the conversion of lambda expressions in clause bodies
% to superhomogeneous form.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.superhomogeneous_lambda.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.make_hlds.superhomogeneous_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

:- type lambda_body_kind
    --->    lambda_body_ordinary
    ;       lambda_body_dcg.

:- pred parse_lambda_expr(prog_var::in, purity::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    prog_term::in, maybe({lambda_body_kind, prog_term})::in, expansion::out,
    svar_state::in, unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.mode_util.
:- import_module hlds.goal_vars.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.make_goal.
:- import_module hlds.make_hlds.goal_expr_to_goal.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.superhomogeneous.
:- import_module hlds.passes_aux.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.module_qual.id_set.
:- import_module parse_tree.module_qual.mq_info.
:- import_module parse_tree.module_qual.qualify_items.
:- import_module parse_tree.parse_dcg_goal.
:- import_module parse_tree.parse_goal.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.

:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_vars.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Code for parsing pred/func expressions.
%

parse_lambda_expr(XVar, Purity, Context, MainContext, SubContext,
        PurityPFArgsDetTerm, MaybeLambdaBody, Expansion, SVarState, !UrInfo) :-
    (
        MaybeLambdaBody = no,
        TrueGoal = true_expr(Context),
        MaybeBodyGoal = ok1(TrueGoal),
        MaybeDCGVars = no_dcg_vars
    ;
        MaybeLambdaBody = yes({LambdaBodyKind, BodyGoalTerm}),
        ContextPieces = cord.singleton(
            words("In the body of lambda expression:")),
        term.coerce(BodyGoalTerm, GenericBodyGoalTerm),
        VarSet0 = !.UrInfo ^ ui_varset,
        (
            LambdaBodyKind = lambda_body_ordinary,
            parse_goal(GenericBodyGoalTerm, ContextPieces,
                MaybeBodyGoal0, VarSet0, VarSet),
            MaybeDCGVars = no_dcg_vars
        ;
            LambdaBodyKind = lambda_body_dcg,
            parse_dcg_pred_goal(GenericBodyGoalTerm, ContextPieces,
                MaybeBodyGoal0, DCGVar0, DCGVarN, VarSet0, VarSet),
            MaybeDCGVars = dcg_vars(DCGVar0, DCGVarN)
        ),
        !UrInfo ^ ui_varset := VarSet,
        (
            MaybeBodyGoal0 = ok2(BodyGoal, BodyGoalWarningSpecs),
            add_unravel_specs(BodyGoalWarningSpecs, !UrInfo),
            MaybeBodyGoal = ok1(BodyGoal)
        ;
            MaybeBodyGoal0 = error2(BodyGoalSpecs),
            MaybeBodyGoal = error1(BodyGoalSpecs)
        )
    ),
    parse_lambda_purity_pf_args_det_term(PurityPFArgsDetTerm, MaybeDCGVars,
        MaybeLambdaHead, !UrInfo),
    (
        MaybeLambdaHead = error1(LambdaHeadSpecs),
        add_unravel_specs(LambdaHeadSpecs, !UrInfo),
        record_unravel_found_syntax_error(!UrInfo),
        Expansion = expansion(not_fgti, cord.empty)
    ;
        MaybeLambdaHead = ok1(LambdaHead),
        build_lambda_expression(XVar, Purity, Context, MainContext, SubContext,
            LambdaHead, MaybeBodyGoal, Expansion, SVarState, !UrInfo)
    ).

:- type maybe_dcg_vars
    --->    no_dcg_vars
    ;       dcg_vars(prog_var, prog_var).

:- pred parse_lambda_purity_pf_args_det_term(prog_term::in, maybe_dcg_vars::in,
    maybe1(lambda_head)::out,
    unravel_info::in, unravel_info::out) is det.

parse_lambda_purity_pf_args_det_term(PurityPFArgsDetTerm, MaybeDCGVars,
        MaybeLambdaHead, !UrInfo) :-
    term.coerce(PurityPFArgsDetTerm, GenericPurityPFArgsDetTerm),
    parse_purity_annotation(GenericPurityPFArgsDetTerm, LambdaPurity,
        PFArgsDetTerm),

%   A summary of the term structures that the two conditions of the nested
%   if-then-else below look for:
%
%   (
%       % Condition 1p:
%       PFArgsDetTerm = is(BeforeIsTerm, DetismTerm),
%       ( BeforeIsTerm = pred(...) ; BeforeIsTerm = any_pred(...) )
%   ;
%       % Condition 1f:
%       PFArgsDetTerm = is(BeforeIsTerm, DetismTerm),
%       BeforeIsTerm = "="(FuncArgsTerm, FuncRetTerm),
%       ( FuncArgsTerm = func(...) ; FuncArgsTerm = any_func(...) )
%   ;
%       % Condition 2f:
%       PFArgsDetTerm = "="(FuncArgsTerm, FuncRetTerm),
%       ( FuncArgsTerm = func(...) ; FuncArgsTerm = any_func(...) )
%   )

    ( if
        PFArgsDetTerm = term.functor(term.atom("is"),
            [BeforeIsTerm, DetismTerm], _),
        BeforeIsTerm = term.functor(term.atom(BeforeIsFunctor),
            BeforeIsArgTerms, Context),
        (
            % Condition 1p.
            (
                BeforeIsFunctor = "pred",
                Groundness = ho_ground
            ;
                BeforeIsFunctor = "any_pred",
                Groundness = ho_any
            ),
            ArgModeTerms0 = BeforeIsArgTerms,
            MaybeFuncRetArgModeTerm = no
        ;
            % Condition 1f.
            BeforeIsFunctor = "=",
            BeforeIsArgTerms = [FuncArgsTerm, FuncRetArgModeTerm0],
            FuncArgsTerm = term.functor(term.atom(FuncTermFunctor),
                ArgModeTerms0, _),
            (
                FuncTermFunctor = "func",
                Groundness = ho_ground
            ;
                FuncTermFunctor = "any_func",
                Groundness = ho_any
            ),
            MaybeFuncRetArgModeTerm = yes(FuncRetArgModeTerm0)
        )
    then
        VarSet0 = !.UrInfo ^ ui_varset,
        parse_lambda_detism(VarSet0, DetismTerm, MaybeDetism),
        (
            MaybeFuncRetArgModeTerm = no,
            PredOrFunc = pf_predicate,
            (
                MaybeDCGVars = no_dcg_vars,
                ArgModeTerms = ArgModeTerms0,
                parse_lambda_args_pred(Context, ArgModeTerms,
                    LambdaArgs, BadModeSpecs, SVarSpecs, !UrInfo),
                LambdaHead = lambda_head(LambdaPurity, Groundness, PredOrFunc,
                    LambdaArgs, BadModeSpecs, SVarSpecs, MaybeDetism),
                MaybeLambdaHead = ok1(LambdaHead)
            ;
                MaybeDCGVars = dcg_vars(DCGVar0, DCGVarN),
                (
                    ( ArgModeTerms0 = []
                    ; ArgModeTerms0 = [_]
                    ),
                    Pieces = [words("Error:")] ++
                        color_as_subject([words("the head of a"),
                            words("lambda expression that is defined"),
                            words("by a DCG clause")]) ++
                        color_as_incorrect([words("must have"),
                            words("at least two arguments.")]) ++
                        [nl],
                    Spec = spec($pred, severity_error, phase_pt2h,
                        Context, Pieces),
                    MaybeLambdaHead =
                        error1([Spec | get_any_errors1(MaybeDetism)])
                ;
                    ArgModeTerms0 =
                        [ArgModeTerm1, ArgModeTerm2 | ArgModeTerms3plus],
                    split_last_two(
                        ArgModeTerm1, ArgModeTerm2, ArgModeTerms3plus,
                        NonDCGArgModeTerms, DCGModeTerm0, DCGModeTermN),
                    DCGContext0 = get_term_context(DCGModeTerm0),
                    DCGContextN = get_term_context(DCGModeTermN),
                    DCGVarTerm0 = term.variable(DCGVar0, DCGContext0),
                    DCGVarTermN = term.variable(DCGVarN, DCGContextN),
                    term.coerce(DCGVarTerm0, GenericDCGVarTerm0),
                    term.coerce(DCGVarTermN, GenericDCGVarTermN),
                    DCGArgModeTerm0 = term.functor(term.atom("::"),
                        [GenericDCGVarTerm0, DCGModeTerm0], DCGContext0),
                    DCGArgModeTermN = term.functor(term.atom("::"),
                        [GenericDCGVarTermN, DCGModeTermN], DCGContextN),
                    ArgModeTerms = NonDCGArgModeTerms ++
                        [DCGArgModeTerm0, DCGArgModeTermN],
                    parse_lambda_args_pred(Context, ArgModeTerms,
                        LambdaArgs, BadModeSpecs, SVarSpecs, !UrInfo),
                    LambdaHead = lambda_head(LambdaPurity, Groundness,
                        PredOrFunc, LambdaArgs,
                        BadModeSpecs, SVarSpecs, MaybeDetism),
                    MaybeLambdaHead = ok1(LambdaHead)
                )
            )
        ;
            MaybeFuncRetArgModeTerm = yes(FuncRetArgModeTerm),
            PredOrFunc = pf_function,
            (
                MaybeDCGVars = no_dcg_vars,
                parse_lambda_args_func(Context,
                    ArgModeTerms0, FuncRetArgModeTerm,
                    LambdaArgs, BadModeSpecs, SVarSpecs, !UrInfo),
                LambdaHead = lambda_head(LambdaPurity, Groundness, PredOrFunc,
                    LambdaArgs, BadModeSpecs, SVarSpecs, MaybeDetism),
                MaybeLambdaHead = ok1(LambdaHead)
            ;
                MaybeDCGVars = dcg_vars(_, _),
                Pieces = [words("Error: DCG notation is")] ++
                    color_as_incorrect([words("not allowed")]) ++
                    [words("in")] ++
                    color_as_subject([words("clauses for functions.")]) ++
                    [nl],
                Spec = spec($pred, severity_error, phase_pt2h,
                    Context, Pieces),
                MaybeLambdaHead = error1([Spec | get_any_errors1(MaybeDetism)])
            )
        )
    else if
        % Condition 2f.
        %
        % We are looking for the same term structure as condition 1b,
        % minus the outer "is detism" wrapper. This is why the structure
        % of this code, and the variable names, resemble condition 1b.
        PFArgsDetTerm = term.functor(term.atom(BeforeIsFunctor),
            BeforeIsArgTerms, Context),
        BeforeIsFunctor = "=",
        BeforeIsArgTerms = [FuncArgsTerm, FuncRetArgModeTerm],
        FuncArgsTerm = term.functor(term.atom(FuncTermFunctor),
            ArgModeTerms, _),
        (
            FuncTermFunctor = "func",
            Groundness = ho_ground
        ;
            FuncTermFunctor = "any_func",
            Groundness = ho_any
        )
    then
        PredOrFunc = pf_function,
        % XXX Should we require that ArgModeTerms and FuncRetArgModeTerm
        % *must* have no explicit mode annotations?
        (
            MaybeDCGVars = no_dcg_vars,
            parse_lambda_args_func(Context, ArgModeTerms, FuncRetArgModeTerm,
                LambdaArgs, BadModeSpecs, SVarSpecs, !UrInfo),
            MaybeDetism = ok1(detism_det),
            LambdaHead = lambda_head(LambdaPurity, Groundness, PredOrFunc,
                LambdaArgs, BadModeSpecs, SVarSpecs, MaybeDetism),
            MaybeLambdaHead = ok1(LambdaHead)
        ;
            MaybeDCGVars = dcg_vars(_, _),
            Pieces = [words("Error: DCG notation is")] ++
                color_as_incorrect([words("not allowed")]) ++
                [words("in")] ++
                color_as_subject([words("clauses for functions.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            MaybeLambdaHead = error1([Spec])
        )
    else
        Form1 = "pred(<args>) is <determinism>",
        Form2 = "any_pred(<args>) is <determinism>",
        Form3 = "func(<args>) = <retarg> is <determinism>",
        Form4 = "any_func(<args>) = <retarg> is <determinism>",
        Form5 = "func(<args>) = <retarg>",
        Form6 = "any_func(<args>) = <retarg>",
        Pieces =
            [words("Error: the clause head part of a lambda expression")] ++
            color_as_incorrect(
                [words("must have one of the following forms:")]) ++
            color_as_correct([quote(Form1)]) ++ [nl] ++
            color_as_correct([quote(Form2)]) ++ [nl] ++
            color_as_correct([quote(Form3)]) ++ [nl] ++
            color_as_correct([quote(Form4)]) ++ [nl] ++
            color_as_correct([quote(Form5)]) ++ [nl] ++
            color_as_correct([quote(Form6)]) ++ [suffix(","), nl,
            words("or one of those forms preceded by either"),
            quote("semipure"), words("or"), quote("impure"), suffix("."), nl],
        Context = get_term_context(PFArgsDetTerm),
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        record_unravel_found_syntax_error(!UrInfo),
        MaybeLambdaHead = error1([Spec])
    ).

:- pred split_last_two(T::in, T::in, list(T)::in, list(T)::out, T::out, T::out)
    is det.

split_last_two(Element1, Element2, Elements3plus, Main, LastButOne, Last) :-
    (
        Elements3plus = [],
        Main = [],
        LastButOne = Element1,
        Last = Element2
    ;
        Elements3plus = [Element3 | Elements4plus],
        split_last_two(Element2, Element3, Elements4plus, MainTail,
            LastButOne, Last),
        Main = [Element1 | MainTail]
    ).

%---------------------------------------------------------------------------%

:- pred parse_lambda_args_func(term.context::in, list(term)::in, term::in,
    list(lambda_arg)::out, list(error_spec)::out, list(error_spec)::out,
    unravel_info::in, unravel_info::out) is det.

parse_lambda_args_func(Context, ArgModeTerms, FuncRetArgModeTerm,
        LambdaArgs, !:BadModeSpecs, !:SVarSpecs, !UrInfo) :-
    !:BadModeSpecs = [],
    !:SVarSpecs = [],
    parse_lambda_args(lambda_arg_ordinary,
        ArgModeTerms, OrdinaryLambdaArgs, 1, ResultArgNum,
        !BadModeSpecs, !SVarSpecs, !UrInfo),
    parse_lambda_arg(lambda_arg_func_result,
        FuncRetArgModeTerm, FuncRetLambdaArg, ResultArgNum, _,
        !BadModeSpecs, !SVarSpecs, !UrInfo),
    LambdaArgs = OrdinaryLambdaArgs ++ [FuncRetLambdaArg],
    classify_lambda_arg_modes_present_absent(LambdaArgs,
        PresentArgs, AbsentArgs),
    (
        AbsentArgs = []
        % All arguments have explicit mode annotations.
    ;
        AbsentArgs = [_ | _],
        (
            PresentArgs = []
            % No arguments have explicit mode annotations.
            % The argument modes that together constitute the default
            % function mode have already been filled in.
        ;
            PresentArgs = [_ | _],
            add_some_not_all_args_have_modes_error(Context, AbsentArgs,
                !BadModeSpecs)
        )
    ).

:- pred parse_lambda_args_pred(term.context::in, list(term)::in,
    list(lambda_arg)::out, list(error_spec)::out, list(error_spec)::out,
    unravel_info::in, unravel_info::out) is det.

parse_lambda_args_pred(Context, ArgModeTerms,
        LambdaArgs, !:BadModeSpecs, !:SVarSpecs, !UrInfo) :-
    !:BadModeSpecs = [],
    !:SVarSpecs = [],
    parse_lambda_args(lambda_arg_ordinary, ArgModeTerms, LambdaArgs, 1, _,
        !BadModeSpecs, !SVarSpecs, !UrInfo),
    classify_lambda_arg_modes_present_absent(LambdaArgs,
        PresentArgs, AbsentArgs),
    (
        AbsentArgs = []
        % All arguments have explicit mode annotations.
    ;
        AbsentArgs = [_ | _],
        (
            PresentArgs = [],
            add_pred_no_args_have_modes_error(Context, !BadModeSpecs)
        ;
            PresentArgs = [_ | _],
            add_some_not_all_args_have_modes_error(Context, AbsentArgs,
                !BadModeSpecs)
        )
    ).

:- pred classify_lambda_arg_modes_present_absent(list(lambda_arg)::in,
    list(lambda_arg)::out, list(lambda_arg)::out) is det.

classify_lambda_arg_modes_present_absent([], [], []).
classify_lambda_arg_modes_present_absent([LambdaArg | LambdaArgs],
        PresentArgs, AbsentArgs) :-
    classify_lambda_arg_modes_present_absent(LambdaArgs,
        PresentArgsTail, AbsentArgsTail),
    PresentOrAbsent = LambdaArg ^ la_arg_mode_presence,
    (
        PresentOrAbsent = lam_present,
        PresentArgs = [LambdaArg | PresentArgsTail],
        AbsentArgs = AbsentArgsTail
    ;
        PresentOrAbsent = lam_absent,
        PresentArgs = PresentArgsTail,
        AbsentArgs = [LambdaArg | AbsentArgsTail]
    ).

:- pred add_some_not_all_args_have_modes_error(prog_context::in,
    list(lambda_arg)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_some_not_all_args_have_modes_error(Context, AbsentArgs, !Specs) :-
    AbsentArgPieces =
        list.map(func(Arg) = nth_fixed(Arg ^ la_arg_num), AbsentArgs),
    AbsentArgsDotPieces = piece_list_to_color_pieces(color_incorrect, "and",
        [suffix(".")], AbsentArgPieces),
    Pieces = [words("Error: in head of lambda expression:")] ++
        color_as_incorrect(
            [words("some but not all arguments have modes.")]) ++
        [nl,
        words("The arguments without modes are the")] ++
        AbsentArgsDotPieces ++ [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred add_pred_no_args_have_modes_error(prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pred_no_args_have_modes_error(Context, !Specs) :-
    % We could use _AbsentArgs to make the error message more detailed.
    Pieces = [words("Error: in head of predicate lambda expression:")] ++
        color_as_incorrect([words("none of the arguments have modes.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- type lambda_arg_kind
    --->    lambda_arg_ordinary
    ;       lambda_arg_func_result.

:- type lambda_arg_mode_presence
    --->    lam_absent
    ;       lam_present.

:- type lambda_arg
    --->    lambda_arg(
                la_arg_num              :: int,
                la_arg_term             :: prog_term,
                la_arg_var              :: prog_var,
                la_kind                 :: lambda_arg_kind,

                % If the lambda argument does not have a "::mode" annotation,
                % the la_arg_mode_presence field will contain lam_absent,
                % and the la_arg_mode field will contain the default mode
                % for the argument position ("in" for ordinary arguments,
                % "out" for function results).
                %
                % If the lambda argument does have a "::mode" annotation,
                % the la_arg_mode_presence field will contain lam_present.
                % If the mode annotation can be successfully parsed,
                % the la_arg_mode field will contain that mode.
                % If the mode annotation cannot be parsed, then
                % the la_arg_mode field will contain the default mode
                % for the argument position, as above, but the messages
                % descrbing the error will be added to !BadModeSpecs.
                la_arg_mode_presence    :: lambda_arg_mode_presence,
                la_arg_mode             :: mer_mode,

                % The context of the mode annotation, or if it is absent,
                % the context of the argument.
                la_arg_mode_context     :: prog_context
            ).

:- func project_lambda_arg_term(lambda_arg) = prog_term.

project_lambda_arg_term(LambdaArg) = ArgTerm :-
    ArgTerm = LambdaArg ^ la_arg_term.

:- func project_lambda_var(lambda_arg) = prog_var.

project_lambda_var(LambdaArg) = LambdaVar :-
    LambdaVar = LambdaArg ^ la_arg_var.

:- func project_lambda_arg_mode(lambda_arg) = mer_mode.

project_lambda_arg_mode(LambdaArg) = Mode :-
    Mode = LambdaArg ^ la_arg_mode.

:- func project_lambda_var_arg_mode(lambda_arg) = pair(prog_var, mer_mode).

project_lambda_var_arg_mode(LambdaArg) = LambdaVar - Mode :-
    LambdaVar = LambdaArg ^ la_arg_var,
    Mode = LambdaArg ^ la_arg_mode.

%---------------------------------------------------------------------------%

    % Parse a list of lambda argument terms, each which should be of the form
    % argterm::modeterm.
    %
:- pred parse_lambda_args(lambda_arg_kind::in,
    list(term)::in, list(lambda_arg)::out, int::in, int::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out,
    unravel_info::in, unravel_info::out) is det.

parse_lambda_args(_Kind, [], [], !ArgNum, !BadModeSpecs, !SVarSpecs, !UrInfo).
parse_lambda_args(Kind, [HeadArgModeTerm | TailArgModeTerms],
        [HeadLambdaArg | TailLambdaArgs],
        !ArgNum, !BadModeSpecs, !SVarSpecs, !UrInfo) :-
    parse_lambda_arg(Kind, HeadArgModeTerm, HeadLambdaArg,
        !ArgNum, !BadModeSpecs, !SVarSpecs, !UrInfo),
    parse_lambda_args(Kind, TailArgModeTerms, TailLambdaArgs,
        !ArgNum, !BadModeSpecs, !SVarSpecs, !UrInfo).

:- pred parse_lambda_arg(lambda_arg_kind::in,
    term::in, lambda_arg::out, int::in, int::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out,
    unravel_info::in, unravel_info::out) is det.

parse_lambda_arg(Kind, ArgModeTerm, LambdaArg, !ArgNum,
        !BadModeSpecs, !SVarSpecs, !UrInfo) :-
    ( if
        ArgModeTerm = term.functor(term.atom("::"),
            [ArgTermPrime, ModeTerm], _)
    then
        ArgTerm = ArgTermPrime,
        PresentOrAbsent = lam_present,
        ModeContext = get_term_context(ModeTerm),
        ContextPieces = cord.from_list([words("In the"), nth_fixed(!.ArgNum),
            words("argument of the lambda expression:")]),
        VarSet0 = !.UrInfo ^ ui_varset,
        varset.coerce(VarSet0, GenericVarSet),
        parse_mode(allow_constrained_inst_var, GenericVarSet, ContextPieces,
            ModeTerm, MaybeMode0),
        (
            MaybeMode0 = ok1(Mode0),
            constrain_inst_vars_in_mode(Mode0, Mode)
        ;
            MaybeMode0 = error1(ModeSpecs),
            !:BadModeSpecs = ModeSpecs ++ !.BadModeSpecs,
            Mode = default_mode_for_lambda_arg(Kind)
        )
    else
        ArgTerm = ArgModeTerm,
        PresentOrAbsent = lam_absent,
        Mode = default_mode_for_lambda_arg(Kind),
        ModeContext = get_term_context(ArgModeTerm)
    ),

    term.coerce(ArgTerm, ProgArgTerm),
    % We currently do not allow !X to appear as a lambda head argument, though
    % we might later extend the syntax still further to accommodate this
    % using syntax such as !IO::(di, uo).
    ( if is_term_a_bang_state_pair(ProgArgTerm, StateVar, StateVarContext) then
        VarSet1 = !.UrInfo ^ ui_varset,
        (
            Kind = lambda_arg_ordinary,
            SVarSpec = report_illegal_bang_svar_lambda_arg_raw(StateVarContext,
                VarSet1, StateVar)
        ;
            Kind = lambda_arg_func_result,
            SVarSpec = report_illegal_func_svar_result_raw(StateVarContext,
                VarSet1, StateVar)
        ),
        !:SVarSpecs = [SVarSpec | !.SVarSpecs]
    else
        true
    ),
    % We always allocate a new variable for each lambda argument,
    % even if the argument term is already a variable (which is what
    % make_fresh_arg_vars_subst_svars does). This is because for functions,
    % we need to ensure that the variable corresponding to the function
    % result term is a new variable, to avoid the function result term
    % becoming lambda-quantified.
    LambdaVarName = "LambdaHeadVar__" ++ string.int_to_string(!.ArgNum),
    create_new_named_unravel_var(LambdaVarName, LambdaVar, !UrInfo),
    LambdaArg = lambda_arg(!.ArgNum, ProgArgTerm, LambdaVar, Kind,
        PresentOrAbsent, Mode, ModeContext),
    !:ArgNum = !.ArgNum + 1.

:- func default_mode_for_lambda_arg(lambda_arg_kind) = mer_mode.

default_mode_for_lambda_arg(Kind) = Mode :-
    (
        Kind = lambda_arg_ordinary,
        in_mode(Mode)
    ;
        Kind = lambda_arg_func_result,
        out_mode(Mode)
    ).

%---------------------------------------------------------------------------%

:- pred parse_purity_annotation(term(T)::in, purity::out, term(T)::out) is det.

parse_purity_annotation(Term0, Purity, Term) :-
    ( if
        Term0 = term.functor(term.atom(PurityName), [Term1], _),
        purity_name(Purity0, PurityName)
    then
        Purity = Purity0,
        Term = Term1
    else
        Purity = purity_pure,
        Term = Term0
    ).

:- pred parse_lambda_detism(prog_varset::in, term::in,
    maybe1(determinism)::out) is det.

parse_lambda_detism(VarSet, DetismTerm, MaybeDetism) :-
    ( if
        DetismTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism)
    then
        MaybeDetism = ok1(Detism)
    else
        varset.coerce(VarSet, GenericVarSet),
        TermStr = describe_error_term(GenericVarSet, DetismTerm),
        Pieces = [words("Error:")] ++ color_as_incorrect([words(TermStr)]) ++
            [words("is not a valid determinism."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(DetismTerm), Pieces),
        MaybeDetism = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Code for building lambda expressions.
%

:- type lambda_head
    --->    lambda_head(
                purity,
                ho_groundness,
                pred_or_func,
                list(lambda_arg),
                list(error_spec),       % Errors about unparseable and/or
                                        % missing arg modes.
                list(error_spec),       % Errors about !X arguments.
                maybe1(determinism)     % The determinism of the lambda expr.
            ).

    % In the parse tree, the lambda arguments can be any terms, but in the HLDS
    % they must be distinct variables. So we introduce fresh variables
    % for the lambda arguments, and add appropriate unifications.
    %
    % For example, we convert from:
    %
    %   X = (func(f(A, B), c) = D :- Body )
    %
    % to:
    %
    %   X =
    %       ( func(H1, H2) = H3 :-
    %           some [A, B] (
    %               H1 = f(A, B),
    %               H2 = c,
    %               Body,
    %               H3 = D
    %           )
    %       )
    %
    % Note that the quantification is important here. That is why we need
    % to introduce the explicit `some [...]'. Variables in the argument
    % positions are lambda-quantified, so when we move them to the body,
    % we need to make them explicitly existentially quantified to avoid
    % capturing any variables of the same name that occur outside this scope.
    %
    % Also, note that any introduced unifications that construct the output
    % arguments for the lambda expression, need to occur *after* the body
    % of the lambda expression. This is in case the body of the lambda
    % expression is impure, in which case the mode analyser cannot reorder
    % the unifications; this results in a mode error.
    %
    % XXX The mode analyser *should* be able to reorder such unifications,
    % especially ones that the compiler introduced itself.
    %
    % For predicates, all variables occurring in the lambda arguments are
    % locally quantified to the lambda goal. For functions, we need to
    % be careful because variables in arguments should similarly be quantified,
    % but variables in the function return value term (and not in the
    % arguments) should *not* be locally quantified.
    %
:- pred build_lambda_expression(prog_var::in, purity::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    lambda_head::in, maybe1(goal)::in, expansion::out,
    svar_state::in, unravel_info::in, unravel_info::out) is det.

build_lambda_expression(LHSVar, UnificationPurity,
        Context, MainContext, SubContext, LambdaHead, MaybeBodyGoal,
        Expansion, OutsideSVarState, !UrInfo) :-
    LambdaHead = lambda_head(LambdaPurity, Groundness, PredOrFunc,
        LambdaArgs0, BadModeSpecs, SVarSpecs, MaybeDetism),
    qualify_lambda_arg_modes_if_not_opt_imported(LambdaArgs0, LambdaArgs1,
        Modes, !UrInfo),
    VarSet0 = !.UrInfo ^ ui_varset,
    varset.coerce(VarSet0, TVarSet),
    varset.coerce(VarSet0, InstVarSet),
    warn_about_any_inconsistent_inst_vars(InstVarSet, Context, Modes, !UrInfo),
    (
        MaybeDetism = ok1(Detism)
    ;
        MaybeDetism = error1(DetismSpecs),
        add_unravel_specs(DetismSpecs, !UrInfo),
        % Due to the error, this dummy value won't be used.
        Detism = detism_det
    ),
    (
        MaybeBodyGoal = ok1(BodyGoal)
    ;
        MaybeBodyGoal = error1(BodyGoalSpecs),
        add_unravel_specs(BodyGoalSpecs, !UrInfo),
        record_unravel_found_syntax_error(!UrInfo),
        % Due to the error, this dummy value won't be used.
        BodyGoal = true_expr(Context)
    ),

    ArgSpecs = BadModeSpecs ++ SVarSpecs,
    (
        ArgSpecs = [_ | _],
        add_unravel_specs(ArgSpecs, !UrInfo),
        record_unravel_found_syntax_error(!UrInfo),
        Goal = true_goal_with_context(Context)
    ;
        ArgSpecs = [],
        some [!SVarState] (
            ArgTerms1 = list.map(project_lambda_arg_term, LambdaArgs1),
            svar_prepare_for_lambda_head(Context, ArgTerms1, ArgTerms,
                FinalSVarMap, NewSVars, OutsideSVarState,
                !:SVarState, !UrInfo),
            InitialSVarState = !.SVarState,

            % Partition the arguments (and their corresponding lambda vars)
            % into two sets: those that are not output, i.e. input and unused,
            % and those that are output.
            %
            % The call to svar_prepare_for_lambda_head obsoletes the arg term
            % fields of LambdaArgs1, so we must pass the new arg terms
            % separately. We don't need to put them back into the lambda args,
            % since the lambda args won't be needed later.
            ModuleInfo0 = !.UrInfo ^ ui_module_info,
            partition_args_and_lambda_vars(ModuleInfo0, LambdaArgs1, ArgTerms,
                NonOutputLambdaVarsArgs, OutputLambdaVarsArgs),

            PredFormArity = arg_list_arity(ArgTerms),
            ArgContext = ac_head(PredOrFunc, PredFormArity),

            % Create the unifications that need to come before the body of the
            % lambda expression; those corresponding to args whose mode is
            % input or unused.
            HeadBefore0 = true_goal_with_context(Context),
            insert_arg_unifications(NonOutputLambdaVarsArgs,
                Context, ArgContext, HeadBefore0, HeadBefore,
                !SVarState, !UrInfo),

            map.init(EmptyRenaming),
            transform_parse_tree_goal_to_hlds(loc_whole_goal, EmptyRenaming,
                BodyGoal, Body, !SVarState, !UrInfo),

            % Create the unifications that need to come after the body of the
            % lambda expression; those corresponding to args whose mode is
            % output.
            HeadAfter0 = true_goal_with_context(Context),
            insert_arg_unifications(OutputLambdaVarsArgs, Context, ArgContext,
                HeadAfter0, HeadAfter, !SVarState, !UrInfo),

            LambdaVarsModes =
                list.map(project_lambda_var_arg_mode, LambdaArgs1),
            LambdaVars =
                list.map(project_lambda_var, LambdaArgs1),

            trace [compiletime(flag("debug-statevar-lambda")), io(!IO)] (
                get_debug_output_stream(ModuleInfo0, DebugStream, !IO),
                io.write_string(DebugStream, "\nLAMBDA EXPRESSION\n", !IO),
                io.write_string(DebugStream, "arg terms before:\n", !IO),
                list.foldl(io.write_line(DebugStream), ArgTerms1, !IO),
                io.write_string(DebugStream, "arg terms after:\n", !IO),
                list.foldl(io.write_line(DebugStream), ArgTerms, !IO),
                io.write_string(DebugStream, "lambda arg vars:\n", !IO),
                io.write_line(DebugStream, LambdaVars, !IO),
                io.write_string(DebugStream,
                    "lambda arg unifies before:\n", !IO),
                dump_goal_nl(DebugStream, ModuleInfo0, vns_varset(VarSet0),
                    TVarSet, InstVarSet, HeadBefore, !IO),
                io.write_string(DebugStream, "lambda body:\n", !IO),
                dump_goal_nl(DebugStream, ModuleInfo0, vns_varset(VarSet0),
                    TVarSet, InstVarSet, Body, !IO),
                io.write_string(DebugStream,
                    "lambda arg unifies after:\n", !IO),
                dump_goal_nl(DebugStream, ModuleInfo0, vns_varset(VarSet0),
                    TVarSet, InstVarSet, HeadAfter, !IO),
                map.to_assoc_list(FinalSVarMap, FinalSVarList),
                io.write_string(DebugStream, "FinalSVarMap:\n", !IO),
                io.write_line(DebugStream, FinalSVarList, !IO)
            ),

            % Fix up any state variable unifications.
            FinalSVarState = !.SVarState,
            svar_finish_lambda_body(Context, NewSVars, FinalSVarMap,
                [HeadBefore, Body, HeadAfter], HLDS_Goal0,
                InitialSVarState, FinalSVarState, !UrInfo),

            % Figure out which variables we need to explicitly existentially
            % quantify.
            (
                PredOrFunc = pf_predicate,
                QuantifiedArgTerms = ArgTerms
            ;
                PredOrFunc = pf_function,
                pred_args_to_func_args(ArgTerms, QuantifiedArgTerms,
                    _ReturnValTerm)
            ),
            term_vars.vars_in_terms(QuantifiedArgTerms, QuantifiedVars0),
            list.sort_and_remove_dups(QuantifiedVars0, QuantifiedVars),

            goal_info_init(Context, GoalInfo),
            Reason = exist_quant(QuantifiedVars, compiler_quant),
            HLDS_GoalExpr = scope(Reason, HLDS_Goal0),
            HLDS_Goal = hlds_goal(HLDS_GoalExpr, GoalInfo),

            % We set the lambda nonlocals here to anything that could
            % possibly be nonlocal. Quantification will reduce this down
            % to the proper set of nonlocal arguments.
            some [!LambdaGoalVars] (
                vars_in_goal(HLDS_Goal, !:LambdaGoalVars),
                set_of_var.delete_list(LambdaVars, !LambdaGoalVars),
                set_of_var.delete_list(QuantifiedVars, !LambdaGoalVars),
                LambdaNonLocals = set_of_var.to_sorted_list(!.LambdaGoalVars)
            ),

            LambdaRHS = rhs_lambda_goal(LambdaPurity, Groundness, PredOrFunc,
                LambdaNonLocals, LambdaVarsModes, Detism, HLDS_Goal),
            create_atomic_complicated_unification(LHSVar, LambdaRHS,
                Context, MainContext, SubContext, UnificationPurity, Goal)
        )
    ),
    Expansion = expansion(not_fgti, cord.singleton(Goal)).

:- pred warn_about_any_inconsistent_inst_vars(inst_varset::in,
    prog_context::in, list(mer_mode)::in,
    unravel_info::in, unravel_info::out) is det.

warn_about_any_inconsistent_inst_vars(InstVarSet, Context, Modes, !UrInfo) :-
    inconsistent_constrained_inst_vars_in_modes(Modes, InconsistentVars),
    (
        InconsistentVars = []
    ;
        InconsistentVars = [_ | _],
        VarPieces = list.map(var_to_quote_piece(InstVarSet), InconsistentVars),
        Pieces = [words("Error: the constraints on the inst"),
            words(choose_number(InconsistentVars, "variable", "variables"))] ++
            piece_list_to_color_pieces(color_subject, "and", [], VarPieces) ++
            color_as_incorrect([words("are inconsistent.")]) ++ [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        add_unravel_spec(Spec, !UrInfo)
    ).

    % Partition the lists of arguments and variables into lists
    % of non-output and output arguments and variables.
    %
:- pred partition_args_and_lambda_vars(module_info::in,
    list(lambda_arg)::in, list(prog_term)::in,
    list(unify_var_term)::out, list(unify_var_term)::out) is det.

partition_args_and_lambda_vars(_, [], [], [], []).
partition_args_and_lambda_vars(_, [], [_ | _], _, _) :-
    unexpected($pred, "mismatched lists").
partition_args_and_lambda_vars(_, [_ | _], [], _, _) :-
    unexpected($pred, "mismatched lists").
partition_args_and_lambda_vars(ModuleInfo,
        [LambdaArg | LambdaArgs], [ArgTerm | ArgTerms],
        InputLambdaVarsArgTerms, OutputLambdaVarsArgTerms) :-
    partition_args_and_lambda_vars(ModuleInfo, LambdaArgs, ArgTerms,
        InputLambdaVarsArgTermsTail, OutputLambdaVarsArgTermsTail),

    LambdaArg = lambda_arg(_ArgNum, _SupersededArgTerm, LambdaVar,
        _Kind, _PresentOrAbsent, Mode, _ModeContext),
    LambdaVarArgTerm = unify_var_term(LambdaVar, ArgTerm),

    % If the mode is undefined, calling mode_is_output/2 directly would cause
    % the compiler to abort, so we don't want to do that.
    %
    % It does not really matter whether we consider an argument with an
    % undefined mode input or output, because mode analysis will fail anyway.
    % The code here is slightly simpler if we consider it input.
    ( if
        mode_is_defined(ModuleInfo, Mode),
        mode_is_output(ModuleInfo, Mode)
    then
        % defined and output
        InputLambdaVarsArgTerms  = InputLambdaVarsArgTermsTail,
        OutputLambdaVarsArgTerms =
            [LambdaVarArgTerm | OutputLambdaVarsArgTermsTail]
    else
        % undefined or (defined and not output)
        InputLambdaVarsArgTerms  =
            [LambdaVarArgTerm | InputLambdaVarsArgTermsTail],
        OutputLambdaVarsArgTerms = OutputLambdaVarsArgTermsTail
    ).

    % Succeeds iff the given mode is defined.
    %
:- pred mode_is_defined(module_info::in, mer_mode::in) is semidet.

mode_is_defined(ModuleInfo, Mode) :-
    mode_get_insts_semidet(ModuleInfo, Mode, _, _).

:- pred qualify_lambda_arg_modes_if_not_opt_imported(
    list(lambda_arg)::in, list(lambda_arg)::out, list(mer_mode)::out,
    unravel_info::in, unravel_info::out) is det.

qualify_lambda_arg_modes_if_not_opt_imported(LambdaArgs0, LambdaArgs,
        Modes, !UrInfo) :-
    QualInfo0 = !.UrInfo ^ ui_qual_info,
    qual_info_get_maybe_opt_imported(QualInfo0, MaybeOptImported),
    (
        MaybeOptImported = is_not_opt_imported,
        % Lambda expressions cannot appear in the interface of a module.
        InInt = mq_not_used_in_interface,
        qual_info_get_mq_info(QualInfo0, MQInfo0),
        qualify_lambda_arg_modes(InInt, LambdaArgs0, LambdaArgs, Modes,
            MQInfo0, MQInfo, [], Specs),
        qual_info_set_mq_info(MQInfo, QualInfo0, QualInfo),
        !UrInfo ^ ui_qual_info := QualInfo,
        % Note: Specs will almost always be [].
        add_unravel_specs(Specs, !UrInfo)
    ;
        MaybeOptImported = is_opt_imported,
        % The modes in `.opt' files are already fully module qualified.
        LambdaArgs = LambdaArgs0,
        Modes = list.map(project_lambda_arg_mode, LambdaArgs)
    ).

:- pred qualify_lambda_arg_modes(mq_in_interface::in,
    list(lambda_arg)::in, list(lambda_arg)::out, list(mer_mode)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_lambda_arg_modes(_InInt, [], [], [], !MQInfo, !Specs).
qualify_lambda_arg_modes(InInt, [LambdaArg0 | LambdaArgs0],
        [LambdaArg | LambdaArgs], [Mode | Modes], !MQInfo, !Specs) :-
    LambdaArg0 = lambda_arg(ArgNum, ProgArgTerm, LambdaVar,
        Kind, PresentOrAbsent, Mode0, ModeContext),
    qualify_lambda_mode(InInt, ModeContext, Mode0, Mode, !MQInfo, !Specs),
    LambdaArg = lambda_arg(ArgNum, ProgArgTerm, LambdaVar,
        Kind, PresentOrAbsent, Mode, ModeContext),
    qualify_lambda_arg_modes(InInt, LambdaArgs0,
        LambdaArgs, Modes, !MQInfo, !Specs).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.superhomogeneous_lambda.
%---------------------------------------------------------------------------%
