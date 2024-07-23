%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines predicates that parse the names of modes, insts
% and determinisms.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_inst_mode_name.
:- interface.

:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type allow_constrained_inst_var
    --->    allow_constrained_inst_var
    ;       no_allow_constrained_inst_var(why_no_constrained_inst_var).

:- type why_no_constrained_inst_var
    --->    wnciv_constraint_rhs
    ;       wnciv_inst_defn_lhs
    ;       wnciv_eqv_inst_defn_rhs
    ;       wnciv_mode_defn_rhs
    ;       wnciv_type_and_mode
    ;       wnciv_solver_type_inst
    ;       wnciv_mutable_inst.

:- pred parse_modes(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, list(term)::in, maybe1(list(mer_mode))::out)
    is det.
:- pred parse_mode(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, term::in, maybe1(mer_mode)::out) is det.

:- pred is_known_mode_name(string::in) is semidet.

%---------------------------------------------------------------------------%

:- pred parse_insts(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, list(term)::in, maybe1(list(mer_inst))::out)
    is det.
:- pred parse_inst(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, term::in, maybe1(mer_inst)::out) is det.

:- pred is_known_inst_name(string::in) is semidet.

%---------------------------------------------------------------------------%

:- pred parse_determinism(varset::in, term::in,
    maybe1(determinism)::out) is det.

:- pred standard_det(string::in, determinism::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_util.

:- import_module bool.
:- import_module set.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

parse_modes(_, _, _, [], ok1([])).
parse_modes(AllowConstrainedInstVar, VarSet, ContextPieces, [Term | Terms],
        MaybeModes) :-
    % XXX We should pass a ContextPieces updated as the "nth mode in ...".
    parse_mode(AllowConstrainedInstVar, VarSet, ContextPieces,
        Term, MaybeHeadMode),
    parse_modes(AllowConstrainedInstVar, VarSet, ContextPieces,
        Terms, MaybeTailModes),
    ( if
        MaybeHeadMode = ok1(HeadMode),
        MaybeTailModes = ok1(TailModes)
    then
        MaybeModes = ok1([HeadMode | TailModes])
    else
        Specs = get_any_errors1(MaybeHeadMode)
            ++ get_any_errors1(MaybeTailModes),
        MaybeModes = error1(Specs)
    ).

parse_mode(AllowConstrainedInstVar, VarSet, ContextPieces, Term, MaybeMode) :-
    % The code of parse_mode and parse_inst should be kept in sync.
    (
        Term = term.variable(_, Context),
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: a variable such as")] ++
            color_as_subject([quote(TermStr)]) ++
            [words("is")] ++
            color_as_incorrect([words("not a valid mode.")]) ++ [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeMode = error1([Spec])
    ;
        Term = term.functor(TermFunctor, ArgTerms0, Context),
        (
            (
                TermFunctor = term.integer(_, _, _, _),
                Name = "an integer"
            ;
                TermFunctor = term.float(_),
                Name = "a floating point number"
            ;
                TermFunctor = term.string(_),
                Name = "a string"
            ;
                TermFunctor = term.implementation_defined(_),
                Name = "an implementation defined literal"
            ),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error:"), words(Name), words("such as")] ++
                color_as_subject([quote(TermStr)]) ++
                [words("is")] ++
                color_as_incorrect([words("not a valid mode.")]) ++ [nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            MaybeMode = error1([Spec])
        ;
            TermFunctor = term.atom(FunctorName),
            % The is_known_mode_name predicate should succeed for exactly
            % the set of functor atoms recognized here.
            ( if
                FunctorName = ">>",
                ArgTerms0 = [InstTermA, InstTermB]
            then
                % XXX Should update ContextPieces.
                parse_inst(AllowConstrainedInstVar, VarSet, ContextPieces,
                    InstTermA, MaybeInstA),
                parse_inst(AllowConstrainedInstVar, VarSet, ContextPieces,
                    InstTermB, MaybeInstB),
                ( if
                    MaybeInstA = ok1(InstA),
                    MaybeInstB = ok1(InstB)
                then
                    MaybeMode = ok1(from_to_mode(InstA, InstB))
                else
                    Specs = get_any_errors1(MaybeInstA)
                        ++ get_any_errors1(MaybeInstB),
                    MaybeMode = error1(Specs)
                )
            else if
                FunctorName = "is",
                ArgTerms0 = [BeforeIsTerm, DetTerm]
            then
                % XXX Should update ContextPieces.
                parse_higher_order_mode(AllowConstrainedInstVar, VarSet,
                    ContextPieces, BeforeIsTerm, DetTerm, MaybeMode)
            else
                parse_sym_name_and_args(VarSet, ContextPieces,
                    Term, MaybeFunctor),
                (
                    MaybeFunctor = ok2(SymName, ArgTerms),
                    % XXX Should update ContextPieces.
                    parse_insts(AllowConstrainedInstVar, VarSet, ContextPieces,
                        ArgTerms, MaybeArgInsts),
                    (
                        MaybeArgInsts = ok1(ArgInsts),
                        MaybeMode = ok1(user_defined_mode(SymName, ArgInsts))
                    ;
                        MaybeArgInsts = error1(Specs),
                        MaybeMode = error1(Specs)
                    )
                ;
                    MaybeFunctor = error2(Specs),
                    MaybeMode = error1(Specs)
                )
            )
        )
    ).

is_known_mode_name(">>").
is_known_mode_name("is").

:- pred parse_higher_order_mode(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, term::in, term::in, maybe1(mer_mode)::out) is det.

parse_higher_order_mode(AllowConstrainedInstVar, VarSet, ContextPieces,
        BeforeIsTerm, DetTerm, MaybeMode) :-
    parse_higher_order_inst(AllowConstrainedInstVar, VarSet, ContextPieces,
        BeforeIsTerm, DetTerm, MaybeInst),
    (
        MaybeInst = ok1(Inst),
        AllowInstsAsModes = globals.get_allow_ho_insts_as_modes,
        (
            AllowInstsAsModes = yes,
            Mode = from_to_mode(Inst, Inst),
            MaybeMode = ok1(Mode)
        ;
            AllowInstsAsModes = no,
            ErrorTerm = term.functor(atom("is"), [BeforeIsTerm, DetTerm],
                dummy_context),
            ErrorTermStr = describe_error_term(VarSet, ErrorTerm),
            Pieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first, words("Error:"),
                words("the"), quote("--no-llow-ho-insts-as-modes"),
                words("option")] ++
                color_as_incorrect([words("prohibits")]) ++
                [words("higher order insts such as")] ++
                color_as_subject([quote(ErrorTermStr)]) ++
                [words("being used as modes."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(BeforeIsTerm), Pieces),
            MaybeMode = error1([Spec])
        )
    ;
        MaybeInst = error1(Specs),
        MaybeMode = error1(Specs)
    ).

%---------------------------------------------------------------------------%

parse_insts(_, _, _, [], ok1([])).
parse_insts(AllowConstrainedInstVar, VarSet, ContextPieces, [Term | Terms],
        MaybeInsts) :-
    % XXX We should pass a ContextPieces updated as the "nth inst in ...".
    parse_inst(AllowConstrainedInstVar, VarSet, ContextPieces,
        Term, MaybeHeadInst),
    parse_insts(AllowConstrainedInstVar, VarSet, ContextPieces,
        Terms, MaybeTailInsts),
    ( if
        MaybeHeadInst = ok1(HeadInst),
        MaybeTailInsts = ok1(TailInsts)
    then
        MaybeInsts = ok1([HeadInst | TailInsts])
    else
        Specs = get_any_errors1(MaybeHeadInst)
            ++ get_any_errors1(MaybeTailInsts),
        MaybeInsts = error1(Specs)
    ).

parse_inst(AllowConstrainedInstVar, VarSet, ContextPieces, Term, MaybeInst) :-
    % The code of parse_mode and parse_inst should be kept in sync.
    (
        Term = term.variable(Var, _),
        term.coerce_var(Var, InstVar),
        MaybeInst = ok1(inst_var(InstVar))
    ;
        Term = term.functor(TermFunctor, ArgTerms, Context),
        (
            (
                TermFunctor = term.integer(_, _, _, _),
                Name = "an integer"
            ;
                TermFunctor = term.float(_),
                Name = "a floating point number"
            ;
                TermFunctor = term.string(_),
                Name = "a string"
            ;
                TermFunctor = term.implementation_defined(_),
                Name = "an implementation defined literal"
            ),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error:"), words(Name), words("such as")] ++
                color_as_subject([quote(TermStr)]) ++
                color_as_incorrect([words("is not a valid inst.")]) ++ [nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            MaybeInst = error1([Spec])
        ;
            TermFunctor = term.atom(Name),
            parse_inst_atom_functor(AllowConstrainedInstVar, VarSet,
                ContextPieces, Name, ArgTerms, Context, MaybeInst)
        )
    ).

:- pred parse_inst_atom_functor(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, string::in, list(term)::in, term.context::in,
    maybe1(mer_inst)::out) is det.

parse_inst_atom_functor(AllowConstrainedInstVar, VarSet, ContextPieces,
        Name, ArgTerms0, Context, MaybeInst) :-
    % XXX It would be nice if we could merge the switch on Name inside
    % is_simple_builtin_inst with the explicit switch below, *without*
    % duplicating the code of is_simple_builtin_inst.
    ( if is_known_inst_name_args(Name, ArgTerms0, KnownInstKind) then
        (
            KnownInstKind = known_inst_bad_arity(ExpectedArities),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error:")] ++
                color_as_subject([words("the builtin inst"), quote(Name)]) ++
                color_as_incorrect([words("should only be used")]) ++
                [words("with arity")] ++
                fixed_list_to_color_pieces(color_correct, "or", [suffix(".")],
                    ExpectedArities) ++
                [nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            MaybeInst = error1([Spec])
        ;
            KnownInstKind = known_inst_simple(Inst),
            MaybeInst = ok1(Inst)
        ;
            KnownInstKind = known_inst_compound(CompoundInstKind),
            (
                CompoundInstKind = kcik_is(BeforeIsTerm, DetTerm),
                % XXX Should update ContextPieces.
                parse_higher_order_inst(AllowConstrainedInstVar, VarSet,
                    ContextPieces, BeforeIsTerm, DetTerm, MaybeInst)
            ;
                CompoundInstKind = kcik_bound(DisjTerm),
                % XXX Should update ContextPieces.
                parse_bound_functor_list(AllowConstrainedInstVar, VarSet,
                    ContextPieces, DisjTerm, shared, MaybeInst)
            ;
                CompoundInstKind = kcik_unique(DisjTerm),
                % XXX Should update ContextPieces.
                parse_bound_functor_list(AllowConstrainedInstVar, VarSet,
                    ContextPieces, DisjTerm, unique, MaybeInst)
            ;
                CompoundInstKind = kcik_mostly_unique(DisjTerm),
                % XXX Should update ContextPieces.
                parse_bound_functor_list(AllowConstrainedInstVar, VarSet,
                    ContextPieces, DisjTerm, mostly_unique, MaybeInst)
            ;
                CompoundInstKind = kcik_constrained(VarTerm, SubInstTerm),
                (
                    VarTerm = term.variable(Var, _),
                    % Do not allow nested constrained_inst_vars.
                    % XXX Should update ContextPieces.
                    parse_inst(
                        no_allow_constrained_inst_var(wnciv_constraint_rhs),
                        VarSet, ContextPieces, SubInstTerm, MaybeSubInst),
                    (
                        MaybeSubInst = ok1(SubInst),
                        (
                            AllowConstrainedInstVar =
                                allow_constrained_inst_var,
                            Inst = constrained_inst_vars(
                                set.make_singleton_set(term.coerce_var(Var)),
                                SubInst),
                            MaybeInst = ok1(Inst)
                        ;
                            AllowConstrainedInstVar =
                                no_allow_constrained_inst_var(Why),
                            BadTerm = term.functor(term.atom(Name),
                                ArgTerms0, Context),
                            MaybeInst = no_allow_constrained_inst_var_result(
                                ContextPieces, Why, VarSet, BadTerm)
                        )
                    ;
                        MaybeSubInst = error1(Specs),
                        MaybeInst = error1(Specs)
                    )
                ;
                    VarTerm = term.functor(_, _, _),
                    VarTermStr = describe_error_term(VarSet, VarTerm),
                    Pieces = cord.list(ContextPieces) ++
                        [lower_case_next_if_not_first,
                        words("Error: inst constraints can be applied")] ++
                        color_as_incorrect(
                            [words("only to inst variables,")]) ++
                        [words("not to terms such as")] ++
                        color_as_subject([quote(VarTermStr), suffix(".")]) ++
                        [nl],
                    Spec = spec($pred, severity_error, phase_t2pt,
                        get_term_context(VarTerm), Pieces),
                    MaybeInst = error1([Spec])
                )
            )
        )
    else
        % Anything else must be a user-defined inst.
        UserDefnInstTerm = term.functor(term.atom(Name), ArgTerms0, Context),
        parse_sym_name_and_args(VarSet, ContextPieces, UserDefnInstTerm,
            MaybeFunctor),
        (
            MaybeFunctor = ok2(QualifiedName, ArgTerms1),
            ( if
                BuiltinModule = mercury_public_builtin_module,
                sym_name_get_module_name_default(QualifiedName,
                    unqualified(""), BuiltinModule),
                % If the term is qualified with the `builtin' module,
                % then it may be one of the simple builtin insts.
                UnqualifiedName = unqualify_name(QualifiedName),
                is_known_inst_name_args(UnqualifiedName, ArgTerms1,
                    KnownInstKind),
                KnownInstKind = known_inst_simple(Inst),

                % However, if the inst is a user_inst defined inside
                % the `builtin' module, then we need to make sure it is
                % properly module-qualified.
                Inst \= defined_inst(user_inst(_, _))
            then
                MaybeInst = ok1(Inst)
            else
                % XXX Should update ContextPieces.
                parse_insts(AllowConstrainedInstVar, VarSet, ContextPieces,
                    ArgTerms1, MaybeArgInsts),
                (
                    MaybeArgInsts = ok1(ArgInsts),
                    Inst = defined_inst(user_inst(QualifiedName, ArgInsts)),
                    MaybeInst = ok1(Inst)
                ;
                    MaybeArgInsts = error1(Specs),
                    MaybeInst = error1(Specs)
                )
            )
        ;
            MaybeFunctor = error2(Specs),
            MaybeInst = error1(Specs)
        )
    ).

:- func no_allow_constrained_inst_var_result(cord(format_piece),
    why_no_constrained_inst_var, varset, term) = maybe1(mer_inst).

no_allow_constrained_inst_var_result(ContextPieces, Why, VarSet, Term)
        = MaybeInst :-
    TermStr = describe_error_term(VarSet, Term),
    (
        Why = wnciv_constraint_rhs,
        Place = "on the right hand side of an inst constraint"
    ;
        Why = wnciv_inst_defn_lhs,
        Place = "on the left hand side of the definition of a named inst"
    ;
        Why = wnciv_eqv_inst_defn_rhs,
        Place = "on the right hand side of the definition"
            ++ " of a named inst equivalence"
    ;
        Why = wnciv_mode_defn_rhs,
        Place = "on the right hand side of the definition of a named mode"
    ;
        Why = wnciv_type_and_mode,
        % XXX This *is* allowed in *some* cases.
        % XXX Add an argument to dont_constrain_inst_vars.
        Place = "in a mode annotation on a type"
    ;
        Why = wnciv_solver_type_inst,
        Place = "as the ground or any inst of a solver type definition"
    ;
        Why = wnciv_mutable_inst,
        Place = "as the inst of a mutable"
    ),
    Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
        words("Error: a constrained inst variable"), words("such as")] ++
        color_as_subject([quote(TermStr)]) ++
        color_as_incorrect([words("may not appear"), words(Place),
            suffix(".")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt,
        get_term_context(Term), Pieces),
    MaybeInst = error1([Spec]).

:- type known_compound_inst_kind(T)
    --->    kcik_is(T, T)
    ;       kcik_constrained(T, T)
    ;       kcik_bound(T)
    ;       kcik_unique(T)
    ;       kcik_mostly_unique(T).

:- type known_inst_kind(T)
    --->    known_inst_simple(mer_inst)
    ;       known_inst_compound(known_compound_inst_kind(T))
    ;       known_inst_bad_arity(list(string)).
            % The expected arity or arities as strings.

is_known_inst_name(Name) :-
    is_known_inst_name_args(Name, [] : list(mer_inst), _).

    % is_known_inst_name_args(InstName, InstArgs, KnownInstKind):
    %
    % If Name is a known inst name and Name(InstArgs) is a valid inst
    % structure, then return its kind in KnownInstKind.
    %
    % If Name is a known inst name but Name(InstArgs) is NOT a valid inst
    % structure, then return known_inst_bad_arity in KnownInstKind.
    %
    % If Name is not a known inst name, fail.
    %
:- pred is_known_inst_name_args(string::in, list(T)::in,
    known_inst_kind(T)::out) is semidet.

is_known_inst_name_args(Name, Args, KnownInst) :-
    (
        % Known insts which are always simple.
        (
            Name = "free",
            SimpleInst = free
        ;
            Name = "ground",
            SimpleInst = ground(shared, none_or_default_func)
        ;
            Name = "clobbered",
            SimpleInst = ground(clobbered, none_or_default_func)
        ;
            Name = "mostly_clobbered",
            SimpleInst = ground(mostly_clobbered, none_or_default_func)
        ;
            Name = "any",
            SimpleInst = any(shared, none_or_default_func)
        ;
            Name = "unique_any",
            SimpleInst = any(unique, none_or_default_func)
        ;
            Name = "mostly_unique_any",
            SimpleInst = any(mostly_unique, none_or_default_func)
        ;
            Name = "clobbered_any",
            SimpleInst = any(clobbered, none_or_default_func)
        ;
            Name = "mostly_clobbered_any",
            SimpleInst = any(mostly_clobbered, none_or_default_func)
        ;
            Name = "not_reached",
            SimpleInst = not_reached
        ),
        (
            Args = [],
            KnownInst = known_inst_simple(SimpleInst)
        ;
            Args = [_ | _],
            KnownInst = known_inst_bad_arity(["zero"])
        )
    ;
        Name = "unique",
        (
            Args = [],
            KnownInst = known_inst_simple(ground(unique, none_or_default_func))
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_unique(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity(["zero", "one"])
        )
    ;
        Name = "mostly_unique",
        (
            Args = [],
            KnownInst = known_inst_simple(
                ground(mostly_unique, none_or_default_func))
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_mostly_unique(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity(["zero", "one"])
        )
    ;
        Name = "is",
        (
            ( Args = []
            ; Args = [_]
            ; Args = [_, _, _ | _]
            ),
            KnownInst = known_inst_bad_arity(["two"])
        ;
            Args = [Arg1, Arg2],
            KnownInst = known_inst_compound(kcik_is(Arg1, Arg2))
        )
    ;
        Name = "=<",
        (
            ( Args = []
            ; Args = [_]
            ; Args = [_, _, _ | _]
            ),
            KnownInst = known_inst_bad_arity(["two"])
        ;
            Args = [Arg1, Arg2],
            KnownInst = known_inst_compound(kcik_constrained(Arg1, Arg2))
        )
    ;
        Name = "bound",
        (
            ( Args = []
            ; Args = [_, _ | _]
            ),
            KnownInst = known_inst_bad_arity(["one"])
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_bound(Arg1))
        )
    ;
        Name = "bound_unique",
        % `bound_unique' is for backwards compatibility - use `unique'
        % instead.
        (
            ( Args = []
            ; Args = [_, _ | _]
            ),
            KnownInst = known_inst_bad_arity(["one"])
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_unique(Arg1))
        )
    ).

:- pred parse_higher_order_inst(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, term::in, term::in, maybe1(mer_inst)::out) is det.

parse_higher_order_inst(AllowConstrainedInstVar, VarSet, ContextPieces,
        BeforeIsTerm, DetTerm, MaybeInst) :-
    ( if
        BeforeIsTerm =
            term.functor(term.atom(BeforeIsFunctor), BeforeIsArgTerms, _),
        (
            BeforeIsFunctor = "pred",
            % The syntax for a ground higher-order pred inst is
            %
            %   pred(<Mode1>, <Mode2>, ...) is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % and <Detism> is a determinism.
            IsAny = no
        ;
            BeforeIsFunctor = "any_pred",
            % The syntax for an `any' higher-order pred inst is
            %
            %   any_pred(<Mode1>, <Mode2>, ...) is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % and <Detism> is a determinism.
            IsAny = yes
        )
    then
        % XXX Should update ContextPieces.
        parse_modes(AllowConstrainedInstVar, VarSet, ContextPieces,
            BeforeIsArgTerms, MaybeArgModes),
        parse_determinism(VarSet, DetTerm, MaybeDetism),
        ( if
            MaybeArgModes = ok1(ArgModes),
            MaybeDetism = ok1(Detism)
        then
            PredInstInfo = pred_inst_info(pf_predicate, ArgModes,
                arg_reg_types_unset, Detism),
            (
                IsAny = no,
                Inst = ground(shared, higher_order(PredInstInfo))
            ;
                IsAny = yes,
                Inst = any(shared, higher_order(PredInstInfo))
            ),
            MaybeInst = ok1(Inst)
        else
            Specs = get_any_errors1(MaybeArgModes)
                ++ get_any_errors1(MaybeDetism),
            MaybeInst = error1(Specs)
        )
    else if
        BeforeIsTerm =
            term.functor(term.atom(BeforeIsFunctor), BeforeIsArgTerms, _),
        BeforeIsFunctor = "=",
        BeforeIsArgTerms = [FuncTerm, RetModeTerm],
        FuncTerm = term.functor(term.atom(FuncTermFunctor), ArgModesTerms, _),
        (
            FuncTermFunctor = "func",
            % The syntax for a ground higher-order func inst is
            %
            %   func(<Mode1>, <Mode2>, ...) = <RetMode> is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % <RetMode> is a mode, and <Detism> is a determinism.
            IsAny = no
        ;
            FuncTermFunctor = "any_func",
            % The syntax for an `any' higher-order func inst is
            %
            %   any_func(<Mode1>, <Mode2>, ...) = <RetMode> is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % <RetMode> is a mode, and <Detism> is a determinism.
            IsAny = yes
        )
    then
        % XXX Should update ContextPieces.
        parse_modes(AllowConstrainedInstVar, VarSet, ContextPieces,
            ArgModesTerms, MaybeArgModes0),
        parse_mode(AllowConstrainedInstVar, VarSet, ContextPieces,
            RetModeTerm, MaybeRetMode),
        parse_determinism(VarSet, DetTerm, MaybeDetism),
        ( if
            MaybeArgModes0 = ok1(ArgModes0),
            MaybeRetMode = ok1(RetMode),
            MaybeDetism = ok1(Detism)
        then
            ArgModes = ArgModes0 ++ [RetMode],
            FuncInstInfo = pred_inst_info(pf_function, ArgModes,
                arg_reg_types_unset, Detism),
            (
                IsAny = no,
                Inst = ground(shared, higher_order(FuncInstInfo))
            ;
                IsAny = yes,
                Inst = any(shared, higher_order(FuncInstInfo))
            ),
            MaybeInst = ok1(Inst)
        else
            Specs = get_any_errors1(MaybeArgModes0)
                ++ get_any_errors1(MaybeRetMode)
                ++ get_any_errors1(MaybeDetism),
            MaybeInst = error1(Specs)
        )
    else
        Form1 = "pred(<mode1>, ...) is <detism>",
        Form2 = "any_pred(<mode1>, ...) is <detism>",
        Form3 = "func(<mode1>, ...) = <return_mode> is <detism>",
        Form4 = "any_func(<mode1>, ...) = <return_mode> is <detism>",
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: a higher-order inst")] ++
            color_as_incorrect([words("should have"),
                words("one of the following forms:")]) ++
            [nl_indent_delta(1)] ++
            color_as_correct([quote(Form1)]) ++ [nl] ++
            color_as_correct([quote(Form2)]) ++ [nl] ++
            color_as_correct([quote(Form3)]) ++ [nl] ++
            color_as_correct([quote(Form4), suffix(".")]) ++
            [nl_indent_delta(-1)],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(BeforeIsTerm), Pieces),
        MaybeInst = error1([Spec])
    ).

:- pred parse_bound_functor_list(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, term::in, uniqueness::in,
    maybe1(mer_inst)::out) is det.

parse_bound_functor_list(AllowConstrainedInstVar, VarSet, ContextPieces,
        DisjunctionTerm, Uniqueness, MaybeInst) :-
    disjunction_to_list(DisjunctionTerm, DisjunctTerms),
    parse_bound_functors(AllowConstrainedInstVar, VarSet, ContextPieces,
        DisjunctTerms, MaybeBoundFunctors),
    (
        MaybeBoundFunctors = error1(Specs),
        MaybeInst = error1(Specs)
    ;
        MaybeBoundFunctors = ok1(BoundFunctors),
        list.sort(BoundFunctors, SortedBoundFunctors),
        ( if
            % Does the list specify the same functor twice?
            SortedBoundFunctors = [FirstBoundFunctor | LaterBoundFunctors],
            find_duplicate_cons_id_bound_functors(FirstBoundFunctor,
                LaterBoundFunctors, Duplicates0),
            list.sort_and_remove_dups(Duplicates0, Duplicates),
            Duplicates = [_ | _]
        then
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: this bound inst lists")] ++
                piece_list_to_color_pieces(color_subject, "and", [],
                    Duplicates) ++
                color_as_incorrect([words("more than once.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(DisjunctionTerm), Pieces),
            MaybeInst = error1([Spec])
        else
            Inst = bound(Uniqueness, inst_test_no_results,
                SortedBoundFunctors),
            MaybeInst = ok1(Inst)
        )
    ).

:- pred find_duplicate_cons_id_bound_functors(bound_functor::in,
    list(bound_functor)::in, list(format_piece)::out) is det.

find_duplicate_cons_id_bound_functors(_Prev, [], []).
find_duplicate_cons_id_bound_functors(Prev, [Cur | Next], Duplicates) :-
    find_duplicate_cons_id_bound_functors(Cur, Next, DuplicatesTail),
    Prev = bound_functor(PrevConsId, _),
    Cur = bound_functor(CurConsId, _),
    ( if PrevConsId = CurConsId then
        Duplicates =
            [unqual_cons_id_and_maybe_arity(CurConsId) | DuplicatesTail]
    else
        Duplicates = DuplicatesTail
    ).

:- pred parse_bound_functors(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, list(term)::in,
    maybe1(list(bound_functor))::out) is det.

parse_bound_functors(_, _, _, [], ok1([])).
parse_bound_functors(AllowConstrainedInstVar, VarSet, ContextPieces,
        [Term | Terms], MaybeBoundFunctors) :-
    % XXX Should update ContextPieces.
    parse_bound_functor(AllowConstrainedInstVar, VarSet, ContextPieces,
        Term, MaybeHeadBoundFunctor),
    parse_bound_functors(AllowConstrainedInstVar, VarSet, ContextPieces,
        Terms, MaybeTailBoundFunctors),
    ( if
        MaybeHeadBoundFunctor = ok1(HeadBoundFunctor),
        MaybeTailBoundFunctors = ok1(TailBoundFunctors)
    then
        MaybeBoundFunctors = ok1([HeadBoundFunctor | TailBoundFunctors])
    else
        Specs = get_any_errors1(MaybeHeadBoundFunctor)
            ++ get_any_errors1(MaybeTailBoundFunctors),
        MaybeBoundFunctors = error1(Specs)
    ).

:- pred parse_bound_functor(allow_constrained_inst_var::in, varset::in,
    cord(format_piece)::in, term::in, maybe1(bound_functor)::out) is det.

parse_bound_functor(AllowConstrainedInstVar, VarSet, ContextPieces, Term,
        MaybeBoundFunctor) :-
    (
        Term = term.variable(_, Context),
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error:")] ++
            color_as_subject([quote(TermStr)]) ++
            color_as_incorrect([words("is not a bound inst.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeBoundFunctor = error1([Spec])
    ;
        Term = term.functor(Functor, _ArgTerms0, Context),
        require_complete_switch [Functor]
        (
            Functor = term.atom(_),
            parse_sym_name_and_args(VarSet, cord.init, Term, MaybeFunctor),
            (
                MaybeFunctor = ok2(SymName, ArgTerms1),
                % XXX Should update ContextPieces.
                parse_insts(AllowConstrainedInstVar, VarSet, ContextPieces,
                    ArgTerms1, MaybeArgInsts),
                (
                    MaybeArgInsts = ok1(ArgInsts),
                    list.length(ArgTerms1, Arity),
                    ConsId = du_data_ctor(du_ctor(SymName, Arity,
                        cons_id_dummy_type_ctor)),
                    MaybeBoundFunctor = ok1(bound_functor(ConsId, ArgInsts))
                ;
                    MaybeArgInsts = error1(Specs),
                    MaybeBoundFunctor = error1(Specs)
                )
            ;
                MaybeFunctor = error2(Specs),
                MaybeBoundFunctor = error1(Specs)
            )
        ;
            Functor = term.implementation_defined(_),
%           expect(unify(ArgTerms0, []), $pred,
%               "parse_simple_term has given " ++
%               "an implementation_defined arguments"),
            % Implementation-defined literals should not appear in inst
            % definitions.
            TermStr = describe_error_term(VarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: an implementation defined literal"),
                words("such as")] ++
                color_as_subject([quote(TermStr)]) ++
                color_as_incorrect(
                    [words("may not be a used as a bound inst.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            MaybeBoundFunctor = error1([Spec])
        ;
            Functor = term.integer(Base, Integer, Signedness, Size),
%           expect(unify(ArgTerms0, []), $pred,
%               "parse_simple_term has given an integer arguments"),
            parse_integer_cons_id(Base, Integer, Signedness, Size, Context,
                MaybeConsId),
            (
                MaybeConsId = ok1(ConsId),
                BoundFunctor = bound_functor(ConsId, []),
                MaybeBoundFunctor = ok1(BoundFunctor)
            ;
                MaybeConsId = error1(Specs),
                MaybeBoundFunctor = error1(Specs)
            )
        ;
            (
                Functor = term.float(Float),
%               expect(unify(ArgTerms0, []), $pred,
%                   "parse_simple_term has given a float arguments"),
                ConsId = float_const(Float)
            ;
                Functor = term.string(Str),
%               expect(unify(ArgTerms0, []), $pred,
%                   "parse_simple_term has given a string arguments"),
                ConsId = string_const(Str)
            ),
            BoundFunctor = bound_functor(ConsId, []),
            MaybeBoundFunctor = ok1(BoundFunctor)
        )
    ).

%---------------------------------------------------------------------------%

parse_determinism(VarSet, Term, MaybeDetism) :-
    ( if
        Term = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism)
    then
        MaybeDetism = ok1(Detism)
    else
        TermStr = describe_error_term(VarSet, Term),
        DetismPieces = [words("Error:")] ++
            color_as_subject([quote(TermStr)]) ++
            color_as_incorrect([words("is not a valid determinism.")]) ++
            [nl],
        DetismSpec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), DetismPieces),
        MaybeDetism = error1([DetismSpec])
    ).

standard_det("det",       detism_det).
standard_det("cc_nondet", detism_cc_non).
standard_det("cc_multi",  detism_cc_multi).
standard_det("nondet",    detism_non).
standard_det("multi",     detism_multi).
standard_det("multidet",  detism_multi).
standard_det("semidet",   detism_semi).
standard_det("erroneous", detism_erroneous).
standard_det("failure",   detism_failure).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_inst_mode_name.
%---------------------------------------------------------------------------%
