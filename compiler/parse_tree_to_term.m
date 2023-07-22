%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2015-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unparse.m.
% Main authors: conway, fjh.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_to_term.
:- interface.

:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module term.

%---------------------------------------------------------------------------%

    % Convert a type to a term representation.
    %
:- pred unparse_type(mer_type::in, term::out) is det.

    % Convert a mode to a term representation.
    %
:- func mode_to_term(output_lang, mer_mode) = prog_term.
:- func mode_to_term_with_context(output_lang, prog_context, mer_mode)
    = prog_term.

    % Convert an inst to a term representation.
    %
:- func inst_to_term(output_lang, mer_inst) = prog_term.
:- func inst_to_limited_size_term(output_lang, int, mer_inst) = prog_term.
:- func inst_name_to_term(output_lang, inst_name) = prog_term.
:- func inst_name_to_limited_size_term(output_lang, int, inst_name)
    = prog_term.
:- func inst_test_results_to_term(prog_context, inst_test_results) = prog_term.

    % Convert an integer to a term representation.
    %
:- func int_const_to_decimal_term(some_int_const, term.context) = term(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module term_int.

%---------------------------------------------------------------------------%

:- func make_atom(prog_context, string) = prog_term.

make_atom(Context, Name) =
    term.functor(term.atom(Name), [], Context).

%---------------------------------------------------------------------------%

unparse_type(Type, Term) :-
    Context = dummy_context,
    (
        Type = type_variable(TVar, _),
        Var = term.coerce_var(TVar),
        Term = term.variable(Var, Context)
    ;
        Type = defined_type(SymName, ArgTypes, _),
        unparse_type_list(ArgTypes, ArgTerms),
        unparse_qualified_term(SymName, ArgTerms, Term)
    ;
        Type = builtin_type(BuiltinType),
        builtin_type_name(BuiltinType, Name),
        Term = term.functor(term.atom(Name), [], Context)
    ;
        Type = higher_order_type(PorF, PredArgTypes, HOInstInfo, Purity,
            EvalMethod),
        unparse_type_list(PredArgTypes, PredArgTypeTerms),
        (
            HOInstInfo = higher_order(pred_inst_info(_, PredArgModes, _, _)),
            unparse_mode_list(PredArgModes, PredArgModeTerms),
            combine_type_and_mode_terms(PredArgTypeTerms, PredArgModeTerms,
                PredArgTerms)
        ;
            HOInstInfo = none_or_default_func,
            PredArgTerms = PredArgTypeTerms
        ),
        (
            PorF = pf_predicate,
            Term0 = term.functor(term.atom("pred"), PredArgTerms, Context),
            maybe_add_lambda_eval_method(EvalMethod, Term0, Term2)
        ;
            PorF = pf_function,
            list.det_split_last(PredArgTerms, ArgTerms, RetTerm),
            Term0 = term.functor(term.atom("func"), ArgTerms, Context),
            maybe_add_lambda_eval_method(EvalMethod, Term0, Term1),
            Term2 = term.functor(term.atom("="), [Term1, RetTerm], Context)
        ),
        maybe_add_purity_annotation(Purity, Term2, Term3),
        maybe_add_detism(HOInstInfo, Term3, Term)
    ;
        Type = tuple_type(ArgTypes, _),
        unparse_type_list(ArgTypes, ArgTerms),
        Term = term.functor(term.atom("{}"), ArgTerms, Context)
    ;
        Type = apply_n_type(TVar, ArgTypes, _),
        Var = term.coerce_var(TVar),
        unparse_type_list(ArgTypes, ArgTerms),
        Term = term.functor(term.atom(""),
            [term.variable(Var, Context) | ArgTerms], Context)
    ;
        Type = kinded_type(_, _),
        unexpected($pred, "kind annotation")
    ).

:- pred unparse_type_list(list(mer_type)::in, list(term)::out) is det.

unparse_type_list(Types, Terms) :-
    list.map(unparse_type, Types, Terms).

:- pred unparse_qualified_term(sym_name::in, list(term)::in, term::out) is det.

unparse_qualified_term(unqualified(Name), Args, Term) :-
    Term = term.functor(term.atom(Name), Args, dummy_context).
unparse_qualified_term(qualified(Qualifier, Name), Args, Term) :-
    unparse_qualified_term(Qualifier, [], QualTerm),
    Context = dummy_context,
    Term0 = term.functor(term.atom(Name), Args, Context),
    Term = term.functor(term.atom("."), [QualTerm, Term0], Context).

:- pred combine_type_and_mode_terms(list(term)::in, list(term)::in,
    list(term)::out) is det.

combine_type_and_mode_terms([], [], []).
combine_type_and_mode_terms([], [_ | _], _) :-
    unexpected($pred, "argument length mismatch").
combine_type_and_mode_terms([_ | _], [], _) :-
    unexpected($pred, "argument length mismatch").
combine_type_and_mode_terms([Type | Types], [Mode | Modes], [Term | Terms]) :-
    Term = term.functor(term.atom("::"), [Type, Mode], dummy_context),
    combine_type_and_mode_terms(Types, Modes, Terms).

:- pred maybe_add_lambda_eval_method(lambda_eval_method::in, term::in,
    term::out) is det.

maybe_add_lambda_eval_method(lambda_normal, Term, Term).

:- pred maybe_add_purity_annotation(purity::in, term::in, term::out) is det.

maybe_add_purity_annotation(purity_pure, Term, Term).
maybe_add_purity_annotation(purity_semipure, Term0, Term) :-
    Term = term.functor(term.atom("semipure"), [Term0], dummy_context).
maybe_add_purity_annotation(purity_impure, Term0, Term) :-
    Term = term.functor(term.atom("impure"), [Term0], dummy_context).

:- pred maybe_add_detism(ho_inst_info::in, term::in, term::out) is det.

maybe_add_detism(none_or_default_func, Term, Term).
maybe_add_detism(higher_order(pred_inst_info(_, _, _, Detism)), Term0, Term) :-
    Context = dummy_context,
    DetismTerm0 = det_to_term(Context, Detism),
    term.coerce(DetismTerm0, DetismTerm),
    Term = term.functor(term.atom("is"), [Term0, DetismTerm], Context).

:- pred unparse_mode_list(list(mer_mode)::in, list(term)::out) is det.

unparse_mode_list([], []).
unparse_mode_list([Mode | Modes], [Term | Terms]) :-
    Term0 = mode_to_term(output_mercury, Mode),
    term.coerce(Term0, Term),
    unparse_mode_list(Modes, Terms).

%---------------------------------------------------------------------------%

mode_to_term(Lang, Mode) =
    mode_to_term_with_context(Lang, dummy_context, Mode).

mode_to_term_with_context(Lang, Context, Mode) = ModeTerm :-
    (
        Mode = from_to_mode(InstA, InstB),
        ( if
            % Check for higher-order pred or func modes, and output them
            % in a nice format.
            InstA = ground(_Uniq, higher_order(_)),
            InstB = InstA
        then
            inst_to_term_with_context(Lang, Context, InstA, ModeTerm)
        else
            inst_to_term_with_context(Lang, Context, InstA, InstTermA),
            inst_to_term_with_context(Lang, Context, InstB, InstTermB),
            construct_qualified_term_with_context(unqualified(">>"),
                [InstTermA, InstTermB], Context, ModeTerm)
        )
    ;
        Mode = user_defined_mode(Name, ArgInsts),
        list.map(inst_to_term_with_context(Lang, Context),
            ArgInsts, ArgInstTerms),
        construct_qualified_term_with_context(Name, ArgInstTerms,
            Context, ModeTerm)
    ).

%---------------------------------------------------------------------------%

inst_to_term(Lang, Inst) = InstTerm :-
    inst_to_term_with_context(Lang, dummy_context, Inst, InstTerm).

:- pred inst_to_term_with_context(output_lang::in, prog_context::in,
    mer_inst::in, prog_term::out) is det.

inst_to_term_with_context(Lang, Context, Inst, InstTerm) :-
    (
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            any_pred_inst_info_to_term(Lang, Context, Uniq,
                PredInstInfo, InstTerm)
        ;
            HOInstInfo = none_or_default_func,
            InstTerm = make_atom(Context, any_inst_uniqueness(Uniq))
        )
    ;
        Inst = free,
        InstTerm = make_atom(Context, "free")
    ;
        Inst = bound(Uniq, InstResults, BoundInsts),
        bound_insts_to_term(Lang, Context, BoundInsts, BoundInstsTerm),
        (
            Lang = output_mercury,
            ArgTerms = [BoundInstsTerm]
        ;
            Lang = output_debug,
            ResultsTerm = inst_test_results_to_term(Context, InstResults),
            ArgTerms = [ResultsTerm, BoundInstsTerm]
        ),
        construct_qualified_term_with_context(
            unqualified(inst_uniqueness(Uniq, "bound")),
            ArgTerms, Context, InstTerm)
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            ground_pred_inst_info_to_term(Lang, Context, Uniq,
                PredInstInfo, InstTerm)
        ;
            HOInstInfo = none_or_default_func,
            InstTerm = make_atom(Context, inst_uniqueness(Uniq, "ground"))
        )
    ;
        Inst = inst_var(Var),
        InstTerm = term.coerce(term.variable(Var, dummy_context))
    ;
        Inst = constrained_inst_vars(Vars, SubInst),
        inst_to_term_with_context(Lang, Context, SubInst, SubInstTerm),
        set.foldl(record_constrained_var(Context), Vars,
            SubInstTerm, InstTerm)
    ;
        Inst = defined_inst(InstName),
        inst_name_to_term_with_context(Lang, Context, InstName, InstTerm)
    ;
        Inst = not_reached,
        InstTerm = make_atom(Context, "not_reached")
    ).

%---------------------------------------------------------------------------%

inst_to_limited_size_term(Lang, SizeLeft, Inst) = InstTerm :-
    inst_to_limited_size_term_with_context(Lang, dummy_context,
        Inst, InstTerm, SizeLeft, _).

:- pred inst_to_limited_size_term_with_context(output_lang::in,
    prog_context::in, mer_inst::in, prog_term::out, int::in, int::out) is det.

inst_to_limited_size_term_with_context(Lang, Context, Inst, InstTerm,
        !SizeLeft) :-
    (
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            any_pred_inst_info_to_term(Lang, Context, Uniq,
                PredInstInfo, InstTerm)
        ;
            HOInstInfo = none_or_default_func,
            InstTerm = make_atom(Context, any_inst_uniqueness(Uniq))
        ),
        !:SizeLeft = !.SizeLeft - 1
    ;
        Inst = free,
        InstTerm = make_atom(Context, "free"),
        !:SizeLeft = !.SizeLeft - 1
    ;
        Inst = bound(Uniq, InstResults, BoundInsts),
        bound_insts_to_limited_size_term(Lang, Context,
            BoundInsts, BoundInstsTerm, !SizeLeft),
        (
            Lang = output_mercury,
            ArgTerms = [BoundInstsTerm]
        ;
            Lang = output_debug,
            ResultsTerm = inst_test_results_to_term(Context, InstResults),
            % ZZZ
            !:SizeLeft = !.SizeLeft - 1,
            ArgTerms = [ResultsTerm, BoundInstsTerm]
        ),
        construct_qualified_term_with_context(
            unqualified(inst_uniqueness(Uniq, "bound")),
            ArgTerms, Context, InstTerm),
        !:SizeLeft = !.SizeLeft - 1
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            ground_pred_inst_info_to_term(Lang, Context, Uniq,
                PredInstInfo, InstTerm)
        ;
            HOInstInfo = none_or_default_func,
            InstTerm = make_atom(Context, inst_uniqueness(Uniq, "ground"))
        ),
        !:SizeLeft = !.SizeLeft - 1
    ;
        Inst = inst_var(Var),
        InstTerm = term.coerce(term.variable(Var, dummy_context)),
        !:SizeLeft = !.SizeLeft - 1
    ;
        Inst = constrained_inst_vars(Vars, SubInst),
        inst_to_limited_size_term_with_context(Lang, Context,
            SubInst, SubInstTerm, !SizeLeft),
        set.foldl(record_constrained_var(Context), Vars,
            SubInstTerm, InstTerm),
        !:SizeLeft = !.SizeLeft - set.count(Vars)
    ;
        Inst = defined_inst(InstName),
        inst_name_to_limited_size_term_with_context(Lang, Context,
            InstName, InstTerm, !SizeLeft)
    ;
        Inst = not_reached,
        InstTerm = make_atom(Context, "not_reached")
    ).

%---------------------------------------------------------------------------%

:- pred record_constrained_var(prog_context::in, inst_var::in,
    prog_term::in, prog_term::out) is det.

record_constrained_var(Context, Var, SubInstTerm, InstTerm) :-
    VarTerm = term.coerce(term.variable(Var, dummy_context)),
    InstTerm = term.functor(term.atom("=<"), [VarTerm, SubInstTerm], Context).

%---------------------------------------------------------------------------%

inst_name_to_term(Lang, InstName) = InstNameTerm :-
    inst_name_to_term_with_context(Lang, dummy_context,
        InstName, InstNameTerm).

:- pred inst_name_to_term_with_context(output_lang::in, prog_context::in,
    inst_name::in, prog_term::out) is det.

inst_name_to_term_with_context(Lang, Context, InstName, Term) :-
    (
        InstName = user_inst(Name, ArgInsts),
        list.map(inst_to_term_with_context(Lang, Context),
            ArgInsts, ArgInstTerms),
        construct_qualified_term_with_context(Name, ArgInstTerms,
            Context, Term)
    ;
        InstName = unify_inst(Liveness, Real, InstA, InstB),
        (
            Lang = output_mercury,
            unexpected($pred, "unify_inst")
        ;
            Lang = output_debug,
            LiveTerm = make_atom(Context, is_live_to_str(Liveness)),
            RealTerm = make_atom(Context, unify_is_real_to_str(Real)),
            inst_to_term_with_context(Lang, Context, InstA, InstTermA),
            inst_to_term_with_context(Lang, Context, InstB, InstTermB),
            construct_qualified_term_with_context(unqualified("$unify"),
                [LiveTerm, RealTerm, InstTermA, InstTermB], Context, Term)
        )
    ;
        InstName = merge_inst(InstA, InstB),
        (
            Lang = output_mercury,
            unexpected($pred, "merge_inst")
        ;
            Lang = output_debug,
            inst_to_term_with_context(Lang, Context, InstA, InstTermA),
            inst_to_term_with_context(Lang, Context, InstB, InstTermB),
            construct_qualified_term_with_context(unqualified("$merge_inst"),
                [InstTermA, InstTermB], Context, Term)
        )
    ;
        InstName = ground_inst(SubInstName, Uniq, IsLive, Real),
        (
            Lang = output_mercury,
            unexpected($pred, "ground_inst")
        ;
            Lang = output_debug,
            inst_name_to_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm),
            UniqTerm = make_atom(Context, inst_uniqueness(Uniq, "shared")),
            IsLiveTerm = make_atom(Context, is_live_to_str(IsLive)),
            IsRealTerm = make_atom(Context, unify_is_real_to_str(Real)),
            construct_qualified_term_with_context(unqualified("$ground"),
                [SubInstNameTerm, UniqTerm, IsLiveTerm, IsRealTerm],
                Context, Term)
        )
    ;
        InstName = any_inst(SubInstName, Uniq, IsLive, Real),
        (
            Lang = output_mercury,
            unexpected($pred, "any_inst")
        ;
            Lang = output_debug,
            inst_name_to_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm),
            UniqTerm = make_atom(Context, inst_uniqueness(Uniq, "shared")),
            IsLiveTerm = make_atom(Context, is_live_to_str(IsLive)),
            IsRealTerm = make_atom(Context, unify_is_real_to_str(Real)),
            construct_qualified_term_with_context(unqualified("$any"),
                [SubInstNameTerm, UniqTerm, IsLiveTerm, IsRealTerm],
                Context, Term)
        )
    ;
        InstName = shared_inst(SubInstName),
        (
            Lang = output_mercury,
            unexpected($pred, "shared_inst")
        ;
            Lang = output_debug,
            inst_name_to_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm),
            construct_qualified_term_with_context(unqualified("$shared_inst"),
                [SubInstNameTerm], Context, Term)
        )
    ;
        InstName = mostly_uniq_inst(SubInstName),
        (
            Lang = output_mercury,
            unexpected($pred, "mostly_uniq_inst")
        ;
            Lang = output_debug,
            inst_name_to_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm),
            construct_qualified_term_with_context(
                unqualified("$mostly_uniq_inst"),
                [SubInstNameTerm], Context, Term)
        )
    ;
        InstName = typed_ground(Uniq, Type),
        (
            Lang = output_mercury,
            unexpected($pred, "typed_ground")
        ;
            Lang = output_debug,
            UniqTerm = make_atom(Context, inst_uniqueness(Uniq, "shared")),
            unparse_type(Type, TypeTerm0),
            TypeTerm = term.coerce(TypeTerm0),
            construct_qualified_term_with_context(unqualified("$typed_ground"),
                [UniqTerm, TypeTerm], Context, Term)
        )
    ;
        InstName = typed_inst(Type, SubInstName),
        (
            Lang = output_mercury,
            % Inst names in the inst tables can (and often do) have the types
            % they apply pushed into them by inst_user.m. However, the typed
            % nature of such inst names cannot (yet) be expressed in Mercury
            % source code.
            inst_name_to_term_with_context(Lang, Context, SubInstName, Term)
        ;
            Lang = output_debug,
            unparse_type(Type, TypeTerm0),
            TypeTerm = term.coerce(TypeTerm0),
            inst_name_to_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm),
            construct_qualified_term_with_context(unqualified("$typed_inst"),
                [TypeTerm, SubInstNameTerm], Context, Term)
        )
    ).

inst_name_to_limited_size_term(Lang, SizeLeft, InstName) = InstNameTerm :-
    inst_name_to_limited_size_term_with_context(Lang, dummy_context,
        InstName, InstNameTerm, SizeLeft, _).

:- pred inst_name_to_limited_size_term_with_context(output_lang::in,
    prog_context::in, inst_name::in, prog_term::out, int::in, int::out) is det.

inst_name_to_limited_size_term_with_context(Lang, Context, InstName, Term,
        !SizeLeft) :-
    (
        InstName = user_inst(Name, ArgInsts),
        insts_to_limited_size_terms_with_context(Lang, Context,
            ArgInsts, ArgInstTerms, !SizeLeft),
        construct_qualified_term_with_context(Name, ArgInstTerms,
            Context, Term),
        !:SizeLeft = !.SizeLeft - 1
    ;
        InstName = unify_inst(Liveness, Real, InstA, InstB),
        (
            Lang = output_mercury,
            unexpected($pred, "unify_inst")
        ;
            Lang = output_debug,
            LiveTerm = make_atom(Context, is_live_to_str(Liveness)),
            RealTerm = make_atom(Context, unify_is_real_to_str(Real)),
            !:SizeLeft = !.SizeLeft - 2,
            inst_to_limited_size_term_with_context(Lang, Context,
                InstA, InstTermA, !SizeLeft),
            inst_to_limited_size_term_with_context(Lang, Context,
                InstB, InstTermB, !SizeLeft),
            ( if !.SizeLeft =< 0 then
                construct_qualified_term_with_context(unqualified("$unify"),
                    [ellipsis_term(Context)], Context, Term)
            else
                construct_qualified_term_with_context(unqualified("$unify"),
                    [LiveTerm, RealTerm, InstTermA, InstTermB], Context, Term)
            )
        )
    ;
        InstName = merge_inst(InstA, InstB),
        (
            Lang = output_mercury,
            unexpected($pred, "merge_inst")
        ;
            Lang = output_debug,
            inst_to_limited_size_term_with_context(Lang, Context,
                InstA, InstTermA, !SizeLeft),
            inst_to_limited_size_term_with_context(Lang, Context,
                InstB, InstTermB, !SizeLeft),
            ( if !.SizeLeft =< 0 then
                construct_qualified_term_with_context(
                    unqualified("$merge_inst"),
                    [ellipsis_term(Context)], Context, Term)
            else
                construct_qualified_term_with_context(
                    unqualified("$merge_inst"),
                    [InstTermA, InstTermB], Context, Term)
            )
        )
    ;
        InstName = ground_inst(SubInstName, Uniq, IsLive, Real),
        (
            Lang = output_mercury,
            unexpected($pred, "ground_inst")
        ;
            Lang = output_debug,
            inst_name_to_limited_size_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm, !SizeLeft),
            UniqTerm = make_atom(Context, inst_uniqueness(Uniq, "shared")),
            IsLiveTerm = make_atom(Context, is_live_to_str(IsLive)),
            IsRealTerm = make_atom(Context, unify_is_real_to_str(Real)),
            !:SizeLeft = !.SizeLeft - 3,
            ( if !.SizeLeft =< 0 then
                construct_qualified_term_with_context(unqualified("$ground"),
                    [ellipsis_term(Context)], Context, Term)
            else
                construct_qualified_term_with_context(unqualified("$ground"),
                    [SubInstNameTerm, UniqTerm, IsLiveTerm, IsRealTerm],
                    Context, Term)
            )
        )
    ;
        InstName = any_inst(SubInstName, Uniq, IsLive, Real),
        (
            Lang = output_mercury,
            unexpected($pred, "any_inst")
        ;
            Lang = output_debug,
            inst_name_to_limited_size_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm, !SizeLeft),
            UniqTerm = make_atom(Context, inst_uniqueness(Uniq, "shared")),
            IsLiveTerm = make_atom(Context, is_live_to_str(IsLive)),
            IsRealTerm = make_atom(Context, unify_is_real_to_str(Real)),
            !:SizeLeft = !.SizeLeft - 3,
            ( if !.SizeLeft =< 0 then
                construct_qualified_term_with_context(unqualified("$any"),
                    [ellipsis_term(Context)], Context, Term)
            else
                construct_qualified_term_with_context(unqualified("$any"),
                    [SubInstNameTerm, UniqTerm, IsLiveTerm, IsRealTerm],
                    Context, Term)
            )
        )
    ;
        InstName = shared_inst(SubInstName),
        (
            Lang = output_mercury,
            unexpected($pred, "shared_inst")
        ;
            Lang = output_debug,
            inst_name_to_limited_size_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm, !SizeLeft),
            ( if !.SizeLeft =< 0 then
                construct_qualified_term_with_context(
                    unqualified("$shared_inst"),
                    [ellipsis_term(Context)], Context, Term)
            else
                construct_qualified_term_with_context(
                    unqualified("$shared_inst"),
                    [SubInstNameTerm], Context, Term)
            )
        )
    ;
        InstName = mostly_uniq_inst(SubInstName),
        (
            Lang = output_mercury,
            unexpected($pred, "mostly_uniq_inst")
        ;
            Lang = output_debug,
            inst_name_to_limited_size_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm, !SizeLeft),
            ( if !.SizeLeft =< 0 then
                construct_qualified_term_with_context(
                    unqualified("$mostly_uniq_inst"),
                    [ellipsis_term(Context)], Context, Term)
            else
                construct_qualified_term_with_context(
                    unqualified("$mostly_uniq_inst"),
                    [SubInstNameTerm], Context, Term)
            )
        )
    ;
        InstName = typed_ground(Uniq, Type),
        (
            Lang = output_mercury,
            unexpected($pred, "typed_ground")
        ;
            Lang = output_debug,
            UniqTerm = make_atom(Context, inst_uniqueness(Uniq, "shared")),
            unparse_type(Type, TypeTerm0),
            TypeTerm = term.coerce(TypeTerm0),
            construct_qualified_term_with_context(unqualified("$typed_ground"),
                [UniqTerm, TypeTerm], Context, Term),
            !:SizeLeft = !.SizeLeft - 1
        )
    ;
        InstName = typed_inst(Type, SubInstName),
        (
            Lang = output_mercury,
            % Inst names in the inst tables can (and often do) have the types
            % they apply pushed into them by inst_user.m. However, the typed
            % nature of such inst names cannot (yet) be expressed in Mercury
            % source code.
            inst_name_to_term_with_context(Lang, Context, SubInstName, Term)
        ;
            Lang = output_debug,
            unparse_type(Type, TypeTerm0),
            TypeTerm = term.coerce(TypeTerm0),
            !:SizeLeft = !.SizeLeft - 1,
            inst_name_to_limited_size_term_with_context(Lang, Context,
                SubInstName, SubInstNameTerm, !SizeLeft),
            ( if !.SizeLeft =< 0 then
                construct_qualified_term_with_context(
                    unqualified("$typed_inst"),
                    [ellipsis_term(Context)], Context, Term)
            else
                construct_qualified_term_with_context(
                    unqualified("$typed_inst"),
                    [TypeTerm, SubInstNameTerm], Context, Term)
            )
        )
    ).

%---------------------------------------------------------------------------%

inst_test_results_to_term(Context, InstResults) = Term :-
    (
        InstResults = inst_test_results(GroundnessResult, AnyResult,
            InstNamesResult, InstVarsResult, TypeResult, PropagatedResult),
        SubTerm1 = inst_result_groundness_to_term(Context, GroundnessResult),
        SubTerm2 = inst_result_contains_any_to_term(Context, AnyResult),
        SubTerm3 = inst_result_contains_inst_names_to_term(Context,
            InstNamesResult),
        SubTerm4 = inst_result_contains_inst_vars_to_term(Context,
            InstVarsResult),
        SubTerm5 = inst_result_contains_types_to_term(Context, TypeResult),
        SubTerm6 = inst_result_type_ctor_propagated_to_term(Context,
            PropagatedResult),
        Term = term.functor(term.atom("results"),
            [SubTerm1, SubTerm2, SubTerm3, SubTerm4, SubTerm5, SubTerm6],
            Context)
    ;
        InstResults = inst_test_no_results,
        Term = term.functor(term.atom("no_results"), [], Context)
    ;
        InstResults = inst_test_results_fgtc,
        Term = term.functor(term.atom("fgtc"), [], Context)
    ).

:- func inst_result_groundness_to_term(prog_context, inst_result_groundness)
    = prog_term.

inst_result_groundness_to_term(Context, Groundness) = Term :-
    (
        Groundness = inst_result_is_not_ground,
        Term = term.functor(term.atom("is_not_ground"), [], Context)
    ;
        Groundness = inst_result_is_ground,
        Term = term.functor(term.atom("is_ground"), [], Context)
    ;
        Groundness = inst_result_groundness_unknown,
        Term = term.functor(term.atom("groundness_unknown"), [], Context)
    ).

:- func inst_result_contains_any_to_term(prog_context,
    inst_result_contains_any) = prog_term.

inst_result_contains_any_to_term(Context, ContainsAny) = Term :-
    (
        ContainsAny = inst_result_does_not_contain_any,
        Term = term.functor(term.atom("does_not_contain_any"), [], Context)
    ;
        ContainsAny = inst_result_does_contain_any,
        Term = term.functor(term.atom("does_contain_any"), [], Context)
    ;
        ContainsAny = inst_result_contains_any_unknown,
        Term = term.functor(term.atom("contains_any_unknown"), [], Context)
    ).

:- func inst_result_contains_inst_names_to_term(prog_context,
    inst_result_contains_inst_names) = prog_term.

inst_result_contains_inst_names_to_term(Context, ContainsInstNames) = Term :-
    (
        ContainsInstNames = inst_result_contains_inst_names_unknown,
        Term = term.functor(term.atom("contains_inst_names_unknown"),
            [], Context)
    ;
        ContainsInstNames = inst_result_contains_inst_names_known(InstNameSet),
        % Inst names can be pretty big, so we print only a count.
        % If necessary, we can later modify this code to actually print them.
        set.count(InstNameSet, NumInstNames),
        CountTerm = term_int.int_to_decimal_term(NumInstNames, Context),
        Term = term.functor(term.atom("contains_inst_names_known"),
            [CountTerm], Context)
    ).

:- func inst_result_contains_inst_vars_to_term(prog_context,
    inst_result_contains_inst_vars) = prog_term.

inst_result_contains_inst_vars_to_term(Context, ContainsInstVars) = Term :-
    (
        ContainsInstVars = inst_result_contains_inst_vars_unknown,
        Term = term.functor(term.atom("contains_inst_vars_unknown"),
            [], Context)
    ;
        ContainsInstVars = inst_result_contains_inst_vars_known(InstVarSet),
        set.to_sorted_list(InstVarSet, InstVars),
        InstVarTerms = list.map(inst_var_to_term(Context), InstVars),
        Term = term.functor(term.atom("contains_inst_vars_known"),
            InstVarTerms, Context)
    ).

:- func inst_var_to_term(prog_context, inst_var) = prog_term.

inst_var_to_term(Context, InstVar) = Term :-
    InstVarNum = term.var_to_int(InstVar),
    InstVarNumStr = string.int_to_string(InstVarNum),
    Term = term.functor(string("inst_var_" ++ InstVarNumStr), [], Context).

:- func inst_result_contains_types_to_term(prog_context,
    inst_result_contains_types) = prog_term.

inst_result_contains_types_to_term(Context, ContainsTypes) = Term :-
    (
        ContainsTypes = inst_result_contains_types_unknown,
        Term = term.functor(term.atom("contains_types_unknown"), [], Context)
    ;
        ContainsTypes = inst_result_contains_types_known(TypeCtorSet),
        set.to_sorted_list(TypeCtorSet, TypeCtors),
        TypeCtorTerms = list.map(type_ctor_to_term(Context), TypeCtors),
        Term = term.functor(term.atom("contains_types_known"),
            TypeCtorTerms, Context)
    ).

:- func inst_result_type_ctor_propagated_to_term(prog_context,
    inst_result_type_ctor_propagated) = prog_term.

inst_result_type_ctor_propagated_to_term(Context, PropagatedResult) = Term :-
    (
        PropagatedResult = inst_result_no_type_ctor_propagated,
        Term = term.functor(term.atom("no_type_ctor_propagated"), [], Context)
    ;
        PropagatedResult = inst_result_type_ctor_propagated(TypeCtor),
        Term = term.functor(term.atom("type_ctor_propagated"),
            [type_ctor_to_term(Context, TypeCtor)], Context)
    ).

:- func type_ctor_to_term(prog_context, type_ctor) = prog_term.

type_ctor_to_term(Context, TypeCtor) = Term :-
    TypeCtor = type_ctor(SymName, Arity),
    string.format("%s/%d", [s(sym_name_to_string(SymName)), i(Arity)],
        ConsName),
    Term = term.functor(term.atom(ConsName), [], Context).

%---------------------%

:- pred ground_pred_inst_info_to_term(output_lang::in, prog_context::in,
    uniqueness::in, pred_inst_info::in, prog_term::out) is det.

ground_pred_inst_info_to_term(Lang, Context, _Uniq, PredInstInfo, Term) :-
    % XXX we ignore Uniq
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, _, Det),
    (
        PredOrFunc = pf_predicate,
        construct_qualified_term_with_context(unqualified("pred"),
            list.map(mode_to_term_with_context(Lang, Context), Modes),
            Context, ModesTerm)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        construct_qualified_term_with_context(unqualified("func"),
            list.map(mode_to_term_with_context(Lang, Context), ArgModes),
            Context, ArgModesTerm),
        construct_qualified_term_with_context(unqualified("="),
            [ArgModesTerm, mode_to_term_with_context(Lang, Context, RetMode)],
            Context, ModesTerm)
    ),
    construct_qualified_term_with_context(unqualified("is"),
        [ModesTerm, det_to_term(Context, Det)], Context, Term).

%---------------------%

:- pred any_pred_inst_info_to_term(output_lang::in, prog_context::in,
    uniqueness::in, pred_inst_info::in, prog_term::out) is det.

any_pred_inst_info_to_term(Lang, Context, _Uniq, PredInstInfo, Term) :-
    % XXX we ignore Uniq
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, _, Det),
    (
        PredOrFunc = pf_predicate,
        construct_qualified_term_with_context(unqualified("any_pred"),
            list.map(mode_to_term_with_context(Lang, Context), Modes),
            Context, ModesTerm)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        construct_qualified_term_with_context(unqualified("any_func"),
            list.map(mode_to_term_with_context(Lang, Context), ArgModes),
            Context, ArgModesTerm),
        construct_qualified_term_with_context(unqualified("="),
            [ArgModesTerm, mode_to_term_with_context(Lang, Context, RetMode)],
            Context, ModesTerm)
    ),
    construct_qualified_term_with_context(unqualified("is"),
        [ModesTerm, det_to_term(Context, Det)], Context, Term).

%---------------------------------------------------------------------------%

:- func is_live_to_str(is_live) = string.

is_live_to_str(is_live) = "live".
is_live_to_str(is_dead) = "dead".

:- func unify_is_real_to_str(unify_is_real) = string.

unify_is_real_to_str(real_unify) = "real".
unify_is_real_to_str(fake_unify) = "fake".

:- func any_inst_uniqueness(uniqueness) = string.

any_inst_uniqueness(shared) = "any".
any_inst_uniqueness(unique) = "unique_any".
any_inst_uniqueness(mostly_unique) = "mostly_unique_any".
any_inst_uniqueness(clobbered) = "clobbered_any".
any_inst_uniqueness(mostly_clobbered) = "mostly_clobbered_any".

:- func inst_uniqueness(uniqueness, string) = string.

inst_uniqueness(shared, SharedName) = SharedName.
inst_uniqueness(unique, _) = "unique".
inst_uniqueness(mostly_unique, _) = "mostly_unique".
inst_uniqueness(clobbered, _) = "clobbered".
inst_uniqueness(mostly_clobbered, _) = "mostly_clobbered".

%---------------------%

:- pred bound_insts_to_term(output_lang::in, prog_context::in,
    list(bound_inst)::in, prog_term::out) is det.

bound_insts_to_term(_, Context, [], Term) :-
    % This shouldn't happen, but when it does, the problem is a LOT easier
    % to debug if there is a HLDS dump you can read.
    Term = term.functor(term.atom("EMPTY_BOUND_INSTS"), [], Context).
bound_insts_to_term(Lang, Context, [BoundInst | BoundInsts], Term) :-
    bound_insts_to_term_lag(Lang, Context, BoundInst, BoundInsts, Term).

:- pred bound_insts_to_term_lag(output_lang::in, prog_context::in,
    bound_inst::in, list(bound_inst)::in, prog_term::out) is det.

bound_insts_to_term_lag(Lang, Context, BoundInst, BoundInsts, Term) :-
    BoundInst = bound_functor(ConsId, ArgInsts),
    list.map(inst_to_term_with_context(Lang, Context), ArgInsts, ArgInstTerms),
    cons_id_and_args_to_term_full(ConsId, ArgInstTerms, FirstTerm),
    (
        BoundInsts = [],
        Term = FirstTerm
    ;
        BoundInsts = [HeadBoundInst | TailBoundInsts],
        bound_insts_to_term_lag(Lang, Context,
            HeadBoundInst, TailBoundInsts, SecondTerm),
        construct_qualified_term_with_context(unqualified(";"),
            [FirstTerm, SecondTerm], Context, Term)
    ).

%---------------------%

:- pred bound_insts_to_limited_size_term(output_lang::in, prog_context::in,
    list(bound_inst)::in, prog_term::out, int::in, int::out) is det.

bound_insts_to_limited_size_term(_, Context, [], Term, !SizeLeft) :-
    % This shouldn't happen, but when it does, the problem is a LOT easier
    % to debug if there is a HLDS dump you can read.
    Term = term.functor(term.atom("EMPTY_BOUND_INSTS"), [], Context).
bound_insts_to_limited_size_term(Lang, Context, [BoundInst | BoundInsts],
        Term, !SizeLeft) :-
    bound_insts_to_limited_size_term_lag(Lang, Context, BoundInst, BoundInsts,
        Term, !SizeLeft).

:- pred bound_insts_to_limited_size_term_lag(output_lang::in, prog_context::in,
    bound_inst::in, list(bound_inst)::in, prog_term::out,
    int::in, int::out) is det.

bound_insts_to_limited_size_term_lag(Lang, Context, BoundInst, BoundInsts,
        Term, !SizeLeft) :-
    BoundInst = bound_functor(ConsId, ArgInsts),
    insts_to_limited_size_terms_with_context(Lang, Context,
        ArgInsts, ArgInstTerms, !SizeLeft),
    cons_id_and_args_to_term_full(ConsId, ArgInstTerms, FirstTerm),
    (
        BoundInsts = [],
        Term = FirstTerm
    ;
        BoundInsts = [HeadBoundInst | TailBoundInsts],
        bound_insts_to_limited_size_term_lag(Lang, Context,
            HeadBoundInst, TailBoundInsts, SecondTerm, !SizeLeft),
        construct_qualified_term_with_context(unqualified(";"),
            [FirstTerm, SecondTerm], Context, Term)
    ).

%---------------------------------------------------------------------------%

:- pred insts_to_limited_size_terms_with_context(output_lang::in,
    prog_context::in, list(mer_inst)::in, list(prog_term)::out,
    int::in, int::out) is det.

insts_to_limited_size_terms_with_context(_Lang, _Context, [], [], !SizeLeft).
insts_to_limited_size_terms_with_context(Lang, Context, [HeadInst | TailInsts],
        InstTerms, !SizeLeft) :-
    ( if !.SizeLeft =< 0 then
        InstTerms = [ellipsis_term(Context)]
    else
        inst_to_limited_size_term_with_context(Lang, Context,
            HeadInst, HeadInstTerm, !SizeLeft),
        insts_to_limited_size_terms_with_context(Lang, Context,
            TailInsts, TailInstTerms, !SizeLeft),
        InstTerms = [HeadInstTerm | TailInstTerms]
    ).

:- func ellipsis_term(prog_context) = prog_term.

ellipsis_term(Context) = term.functor(term.atom("..."), [], Context).

%---------------------------------------------------------------------------%

:- pred cons_id_and_args_to_term_full(cons_id::in, list(prog_term)::in,
    prog_term::out) is det.

cons_id_and_args_to_term_full(ConsId, ArgTerms, Term) :-
    Context = dummy_context,
    (
        ConsId = cons(SymName, _Arity, _TypeCtor),
        construct_qualified_term(SymName, ArgTerms, Term)
    ;
        ConsId = tuple_cons(_Arity),
        SymName = unqualified("{}"),
        construct_qualified_term(SymName, ArgTerms, Term)
    ;
        ConsId = closure_cons(_, _),
        FunctorName = "closure_cons",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = some_int_const(IntConst),
        Term = int_const_to_decimal_term(IntConst, Context)
    ;
        ConsId = float_const(Float),
        Term = term.functor(term.float(Float), [], Context)
    ;
        ConsId = string_const(String),
        Term = term.functor(term.string(String), [], Context)
    ;
        ConsId = char_const(Char),
        SymName = unqualified(string.from_char(Char)),
        construct_qualified_term(SymName, [], Term)
    ;
        ConsId = impl_defined_const(IDCKind),
        FunctorName = "ImplDefinedConst: " ++
            impl_defined_const_kind_to_str(IDCKind),
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = type_ctor_info_const(ModuleName, TypeCtorName, Arity),
        string.format("TypeCtorInfo for %s.%s/%d",
            [s(sym_name_to_string(ModuleName)), s(TypeCtorName), i(Arity)],
            FunctorName),
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = base_typeclass_info_const(_, _, _, _),
        FunctorName = "base_typeclass_info_const",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = type_info_cell_constructor(TypeCtor),
        TypeCtor = type_ctor(TypeCtorName, Arity),
        string.format("type_info_cell_constructor for %s/%d",
            [s(sym_name_to_string(TypeCtorName)), i(Arity)], FunctorName),
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = typeclass_info_cell_constructor,
        FunctorName = "typeclass_info_cell_constructor",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = type_info_const(TIConstNum),
        expect(unify(ArgTerms, []), $pred, "type_info_const arity != 0"),
        FunctorName = "type_info_const",
        Arg = term_int.int_to_decimal_term(TIConstNum, Context),
        Term = term.functor(term.string(FunctorName), [Arg], Context)
    ;
        ConsId = typeclass_info_const(TCIConstNum),
        expect(unify(ArgTerms, []), $pred, "typeclass_info_const arity != 0"),
        FunctorName = "typeclass_info_const",
        Arg = term_int.int_to_decimal_term(TCIConstNum, Context),
        Term = term.functor(term.string(FunctorName), [Arg], Context)
    ;
        ConsId = ground_term_const(TCIConstNum, SubConsId),
        expect(unify(ArgTerms, []), $pred, "ground_term_const arity != 0"),
        cons_id_and_args_to_term_full(SubConsId, [], SubArg),
        FunctorName = "ground_term_const",
        NumArg = term_int.int_to_decimal_term(TCIConstNum, Context),
        Term = term.functor(term.string(FunctorName), [NumArg, SubArg],
            Context)
    ;
        ConsId = tabling_info_const(_),
        FunctorName = "tabling_info_const",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = table_io_entry_desc(_),
        FunctorName = "table_io_entry_desc",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = deep_profiling_proc_layout(_),
        FunctorName = "deep_profiling_proc_layout",
        Term = term.functor(term.string(FunctorName), [], Context)
    ).

int_const_to_decimal_term(IntConst, Context) = Term :-
    (
        IntConst = int_const(Int),
        Term = term_int.int_to_decimal_term(Int, Context)
    ;
        IntConst = int8_const(Int8),
        Term = term_int.int8_to_decimal_term(Int8, Context)
    ;
        IntConst = int16_const(Int16),
        Term = term_int.int16_to_decimal_term(Int16, Context)
    ;
        IntConst = int32_const(Int32),
        Term = term_int.int32_to_decimal_term(Int32, Context)
    ;
        IntConst = int64_const(Int64),
        Term = term_int.int64_to_decimal_term(Int64, Context)
    ;
        IntConst = uint_const(UInt),
        Term = term_int.uint_to_decimal_term(UInt, Context)
    ;
        IntConst = uint8_const(UInt8),
        Term = term_int.uint8_to_decimal_term(UInt8, Context)
    ;
        IntConst = uint16_const(UInt16),
        Term = term_int.uint16_to_decimal_term(UInt16, Context)
    ;
        IntConst = uint32_const(UInt32),
        Term = term_int.uint32_to_decimal_term(UInt32, Context)
    ;
        IntConst = uint64_const(UInt64),
        Term = term_int.uint64_to_decimal_term(UInt64, Context)
    ).

:- func det_to_term(prog_context, determinism) = prog_term.

det_to_term(Context, Det) = make_atom(Context, determinism_to_string(Det)).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_to_term.
%---------------------------------------------------------------------------%
