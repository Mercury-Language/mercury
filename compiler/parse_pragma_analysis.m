%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_pragma_analysis.m.
%
% This parses pragmas that record the results of some program analysis,
% though some of them may also be written by hand.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_pragma_analysis.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- pred parse_pragma_unused_args(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

:- pred parse_pragma_termination_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

:- pred parse_pragma_termination2_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

:- pred parse_pragma_structure_sharing(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

:- pred parse_pragma_structure_reuse(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

:- pred parse_pragma_exceptions(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

:- pred parse_pragma_trailing_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

:- pred parse_pragma_mm_tabling_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.rat.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module cord.
:- import_module maybe.
:- import_module term_int.
:- import_module term_vars.
:- import_module unit.

%---------------------------------------------------------------------------%

parse_pragma_unused_args(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    % pragma unused_args should never appear in user programs,
    % only in .opt files.
    (
        PragmaTerms = [PredOrFuncTerm, PredNameTerm, ArityTerm, ModeNumTerm,
            UnusedArgsTerm],
        PredNameContextPieces = cord.from_list(
            [words("In the second argument of"),
            pragma_decl("unused_args"), words("declaration:"), nl]),
        parse_predicate_or_function(VarSet, PredOrFuncTerm, MaybePredOrFunc),
        parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
            VarSet, PredNameContextPieces, PredNameTerm, MaybePredName),
        ArityContextPieces = cord.from_list(
            [words("In the third argument of"),
            pragma_decl("unused_args"), words("declaration:"), nl]),
        parse_decimal_int(ArityContextPieces, VarSet, ArityTerm, MaybeArity),
        ModeNumContextPieces = cord.from_list(
            [words("In the fourth argument of"),
            pragma_decl("unused_args"), words("declaration:"), nl]),
        parse_decimal_int(ModeNumContextPieces, VarSet, ModeNumTerm,
            MaybeModeNum),
        UnusedArgsContextPieces = cord.from_list(
            [words("In the fifth argument of"),
            pragma_decl("unused_args"), words("declaration:"), nl]),
        parse_list_elements("a list of integers",
            parse_decimal_int(UnusedArgsContextPieces),
            VarSet, UnusedArgsTerm, MaybeUnusedArgs),
        ( if
            MaybePredOrFunc = ok1(PredOrFunc),
            MaybePredName = ok1(PredName),
            MaybeArity = ok1(Arity),
            MaybeModeNum = ok1(ModeNum),
            MaybeUnusedArgs = ok1(UnusedArgs)
        then
            PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc,
                PredName, user_arity(Arity), ModeNum),
            Pragma = gen_pragma_unused_args_info(PredNameArityPFMn,
                UnusedArgs, Context, SeqNum),
            Item = item_generated_pragma( gen_pragma_unused_args(Pragma)),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs =
                get_any_errors1(MaybePredOrFunc) ++
                get_any_errors1(MaybePredName) ++
                get_any_errors1(MaybeArity) ++
                get_any_errors1(MaybeModeNum) ++
                get_any_errors1(MaybeUnusedArgs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _]
        ; PragmaTerms = [_, _, _, _]
        ; PragmaTerms = [_, _, _, _, _, _ | _]
        ),
        Pieces = [words("Error: an"), pragma_decl("unused_args"),
            words("declaration must have five arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_pragma_termination_info(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredAndModesTerm0, ArgSizeTerm, TerminationTerm],
        PAMContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("termination_info"), words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PAMContextPieces,
            VarSet, PredAndModesTerm0, MaybeNameAndModes),
        ( if
            ArgSizeTerm = term.functor(term.atom(ArgSizeFunctor),
                ArgSizeArgTerms, _),
            ( ArgSizeFunctor = "not_set"
            ; ArgSizeFunctor = "infinite"
            ; ArgSizeFunctor = "finite"
            )
        then
            ArgSizeContextPieces = cord.from_list(
                [words("In the second argument of"),
                pragma_decl("termination_info"), words("declaration:"), nl]),
            (
                ArgSizeFunctor = "not_set",
                (
                    ArgSizeArgTerms = [],
                    MaybeArgSizeInfo0 = no,
                    MaybeMaybeArgSizeInfo = ok1(MaybeArgSizeInfo0)
                ;
                    ArgSizeArgTerms = [_ | _],
                    NotSetPieces = cord.list(ArgSizeContextPieces) ++
                        [words("error:"), quote("not_set"),
                        words("must have no arguments."), nl],
                    NotSetSpec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(ArgSizeTerm), NotSetPieces),
                    MaybeMaybeArgSizeInfo = error1([NotSetSpec])
                )
            ;
                ArgSizeFunctor = "infinite",
                (
                    ArgSizeArgTerms = [],
                    MaybeArgSizeInfo0 = yes(infinite(unit)),
                    MaybeMaybeArgSizeInfo = ok1(MaybeArgSizeInfo0)
                ;
                    ArgSizeArgTerms = [_ | _],
                    InfinitePieces = cord.list(ArgSizeContextPieces) ++
                        [words("error:"), quote("infinite"),
                        words("must have no arguments."), nl],
                    InfiniteSpec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(ArgSizeTerm), InfinitePieces),
                    MaybeMaybeArgSizeInfo = error1([InfiniteSpec])
                )
            ;
                ArgSizeFunctor = "finite",
                (
                    ArgSizeArgTerms = [IntTerm, UsedArgsTerm],
                    IntContextPieces = ArgSizeContextPieces ++
                        cord.from_list([words("in the first argument:"), nl]),
                    parse_decimal_int(IntContextPieces, VarSet, IntTerm,
                        MaybeInt),
                    BoolContextPieces = ArgSizeContextPieces ++
                        cord.from_list([words("in the second argument:"), nl]),
                    parse_list_elements("a list of booleans",
                        parse_bool(BoolContextPieces),
                        VarSet, UsedArgsTerm, MaybeUsedArgs),
                    ( if
                        MaybeInt = ok1(Int),
                        MaybeUsedArgs = ok1(UsedArgs)
                    then
                        MaybeArgSizeInfo0 = yes(finite(Int, UsedArgs)),
                        MaybeMaybeArgSizeInfo = ok1(MaybeArgSizeInfo0)
                    else
                        FiniteSpecs = get_any_errors1(MaybeInt) ++
                            get_any_errors1(MaybeUsedArgs),
                        MaybeMaybeArgSizeInfo = error1(FiniteSpecs)
                    )
                ;
                    ( ArgSizeArgTerms = []
                    ; ArgSizeArgTerms = [_]
                    ; ArgSizeArgTerms = [_, _, _ | _]
                    ),
                    FinitePieces =
                        [words("Error: in the second argument of"),
                        pragma_decl("termination_info"),
                        words("declaration:"), nl,
                        quote("finite"),
                        words("must have two arguments."), nl],
                    FiniteSpec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(ArgSizeTerm), FinitePieces),
                    MaybeMaybeArgSizeInfo = error1([FiniteSpec])
                )
            )
        else
            ArgSizeTermStr = describe_error_term(VarSet, ArgSizeTerm),
            ArgSizePieces = [words("In the second argument of"),
                pragma_decl("termination_info"), words("declaration:"), nl,
                words("error: expected one of"),
                quote("not_set"), suffix(","),
                quote("infinite"), suffix(","), words("and"),
                quote("finite(N, <used_args>)"), suffix(","),
                words("got"), quote(ArgSizeTermStr), suffix("."), nl],
            ArgSizeSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(ArgSizeTerm),
                ArgSizePieces),
            MaybeMaybeArgSizeInfo = error1([ArgSizeSpec])
        ),
        TIContextPieces = [words("In the third argument of"),
            pragma_decl("termination_info"), words("declaration:"), nl],
        parse_termination_info(TIContextPieces, VarSet, TerminationTerm,
            MaybeMaybeTerminationInfo),
        ( if
            MaybeNameAndModes = ok3(PredName, PredOrFunc, Modes),
            MaybeMaybeArgSizeInfo = ok1(MaybeArgSizeInfo),
            MaybeMaybeTerminationInfo = ok1(MaybeTerminationInfo)
        then
            PredNameModesPF = proc_pf_name_modes(PredOrFunc, PredName, Modes),
            Term = decl_pragma_termination_info(PredNameModesPF,
                MaybeArgSizeInfo, MaybeTerminationInfo, Context, SeqNum),
            Item = item_decl_pragma(decl_pragma_termination(Term)),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors3(MaybeNameAndModes) ++
                get_any_errors1(MaybeMaybeArgSizeInfo) ++
                get_any_errors1(MaybeMaybeTerminationInfo),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("termination_info"),
            words("declaration must have three arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_bool(cord(format_piece)::in, varset::in, term::in,
    maybe1(bool)::out) is det.

parse_bool(ContextPieces, VarSet, Term, MaybeBool) :-
    ( if
        Term = term.functor(term.atom(Name), [], _),
        ( Name = "yes", Bool = yes
        ; Name = "no", Bool = no
        )
    then
        MaybeBool = ok1(Bool)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: expected a boolean (yes or no),"),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), Pieces),
        MaybeBool = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_pragma_termination2_info(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredAndModesTerm0, SuccessArgSizeTerm,
            FailureArgSizeTerm, TerminationTerm],
        PAMContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("termination2_info"), words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PAMContextPieces,
            VarSet, PredAndModesTerm0, MaybeNameAndModes),
        parse_arg_size_constraints(VarSet, SuccessArgSizeTerm,
            MaybeSuccessArgSize),
        parse_arg_size_constraints(VarSet, FailureArgSizeTerm,
            MaybeFailureArgSize),
        TIContextPieces = [words("In the fourth argument of"),
            pragma_decl("termination2_info"), words("declaration:"), nl],
        parse_termination_info(TIContextPieces, VarSet, TerminationTerm,
            MaybeMaybeTerminationInfo),
        ( if
            MaybeNameAndModes = ok3(PredName, PredOrFunc, Modes),
            MaybeSuccessArgSize = ok1(SuccessArgSizeInfo),
            MaybeFailureArgSize = ok1(FailureArgSizeInfo),
            MaybeMaybeTerminationInfo = ok1(MaybeTerminationInfo)
        then
            PredNameModesPF = proc_pf_name_modes(PredOrFunc, PredName, Modes),
            Term2 = decl_pragma_termination2_info(PredNameModesPF,
                SuccessArgSizeInfo, FailureArgSizeInfo, MaybeTerminationInfo,
                Context, SeqNum),
            Item = item_decl_pragma(decl_pragma_termination2(Term2)),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors3(MaybeNameAndModes) ++
                get_any_errors1(MaybeSuccessArgSize) ++
                get_any_errors1(MaybeFailureArgSize) ++
                get_any_errors1(MaybeMaybeTerminationInfo),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _]
        ; PragmaTerms = [_, _, _, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("termination2_info"),
            words("declaration must have four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_termination_info(list(format_piece)::in, varset::in,
    term::in, maybe1(maybe(pragma_termination_info))::out) is det.

parse_termination_info(ContextPieces, VarSet, Term,
        MaybeMaybeTerminationInfo) :-
    ( if
        Term = term.functor(term.atom(Functor), [], _),
        (
            Functor = "not_set",
            MaybeTerminationInfo = no
        ;
            Functor = "can_loop",
            MaybeTerminationInfo = yes(can_loop(unit))
        ;
            Functor = "cannot_loop",
            MaybeTerminationInfo = yes(cannot_loop(unit))
        )
    then
        MaybeMaybeTerminationInfo = ok1(MaybeTerminationInfo)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = ContextPieces ++
            [lower_case_next_if_not_first, words("Error: expected one of"),
            quote("not_set"), suffix(","),
            quote("can_loop"), suffix(","), words("and"),
            quote("cannot_loop"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeMaybeTerminationInfo = error1([Spec])
    ).

:- pred parse_arg_size_constraints(varset::in, term::in,
    maybe1(maybe(list(arg_size_constr)))::out) is det.

parse_arg_size_constraints(VarSet, Term, MaybeMaybeArgSizeConstraints) :-
    ( if
        Term = term.functor(term.atom("not_set"), [], _)
    then
        MaybeMaybeArgSizeConstraints = ok1(no)
    else if
        Term = term.functor(term.atom("constraints"), [ConstraintsTerm], _)
    then
        parse_list_elements("list of argument size constraints",
            parse_arg_size_constraint, VarSet, ConstraintsTerm,
            MaybeConstraints),
        (
            MaybeConstraints = ok1(Constraints),
            MaybeMaybeArgSizeConstraints = ok1(yes(Constraints))
        ;
            MaybeConstraints = error1(Specs),
            MaybeMaybeArgSizeConstraints = error1(Specs)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a description of"),
            words("argument size constraints,"),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), Pieces),
        MaybeMaybeArgSizeConstraints = error1([Spec])
    ).

:- pred parse_arg_size_constraint(varset::in, term::in,
    maybe1(arg_size_constr)::out) is det.

parse_arg_size_constraint(VarSet, Term, MaybeConstr) :-
    ( if
        Term = term.functor(term.atom(Functor), [Terms, ConstantTerm], _),
        ( Functor = "le"
        ; Functor = "eq"
        )
    then
        parse_list_elements("a list of linear terms", parse_lp_term,
            VarSet, Terms, LPTermsResult),
        parse_rational(VarSet, ConstantTerm, ConstantResult),
        ( if
            LPTermsResult = ok1(LPTerms),
            ConstantResult = ok1(Constant)
        then
            (
                Functor = "le",
                Constr = le(LPTerms, Constant)
            ;
                Functor = "eq",
                Constr = eq(LPTerms, Constant)
            ),
            MaybeConstr = ok1(Constr)
        else
            Specs = get_any_errors1(LPTermsResult) ++
                get_any_errors1(ConstantResult),
            MaybeConstr = error1(Specs)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected an argument size constraint,"),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), Pieces),
        MaybeConstr = error1([Spec])
    ).

:- pred parse_lp_term(varset::in, term::in, maybe1(arg_size_term)::out) is det.

parse_lp_term(VarSet, Term, MaybeLpTerm) :-
    ( if
        Term = term.functor(term.atom("term"), [VarIdTerm, CoeffTerm], _)
    then
        ( if term_int.decimal_term_to_int(VarIdTerm, VarId0) then
            MaybeVarId = ok1(VarId0)
        else
            VarIdTermStr = describe_error_term(VarSet, VarIdTerm),
            Pieces = [words("Error: expected an integer,"),
                words("got"), quote(VarIdTermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(VarIdTerm), Pieces),
            MaybeVarId = error1([Spec])
        ),
        parse_rational(VarSet, CoeffTerm, MaybeCoeff),
        ( if
            MaybeVarId = ok1(VarId),
            MaybeCoeff = ok1(Coeff)
        then
            LpTerm = arg_size_term(VarId, Coeff),
            MaybeLpTerm = ok1(LpTerm)
        else
            Specs = get_any_errors1(MaybeVarId) ++
                get_any_errors1(MaybeCoeff),
            MaybeLpTerm = error1(Specs)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a linear term of the form"),
            quote("term(<varnum>, <rational_coeff>)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), Pieces),
        MaybeLpTerm = error1([Spec])
    ).

:- pred parse_rational(varset::in, term::in, maybe1(rat)::out) is det.

parse_rational(VarSet, Term, MaybeRational) :-
    ( if
        Term = term.functor(term.atom("r"), [NumerTerm, DenomTerm], _),
        term_int.decimal_term_to_int(NumerTerm, Numer),
        term_int.decimal_term_to_int(DenomTerm, Denom)
    then
        Rational = rat.rat(Numer, Denom),
        MaybeRational = ok1(Rational)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a rational number of the form"),
            quote("r(N, M)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), Pieces),
        MaybeRational = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_pragma_structure_sharing(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [PredAndModesTerm0, HeadVarsTerm,
            HeadVarTypesTerm, SharingInformationTerm],
        ModesContextPieces = cord.from_list([words("In"),
            pragma_decl("structure_sharing"), words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), ModesContextPieces,
            VarSet, PredAndModesTerm0, MaybeNameAndModes),
        MaybeNameAndModes = ok3(PredName, PredOrFunc, Modes),

        % Parse the head variables:
        HeadVarsTerm = term.functor(term.atom("vars"), ListHVTerms, _),
        term_vars.vars_in_terms(ListHVTerms, HeadVarsGeneric),
        list.map(term.coerce_var, HeadVarsGeneric, HeadVars),

        % Parse the types:
        HeadVarTypesTerm = term.functor(term.atom("types"), ListTypeTerms, _),
        maybe_parse_types(no_allow_ho_inst_info(wnhii_pragma_struct_sharing),
            ListTypeTerms, Types),

        % Parse the actual structure sharing information.

        SharingInformationTerm = term.functor(term.atom(SharingFunctor),
            SharingArgTerms, _),
        (
            SharingFunctor = "not_available",
            % XXX Why don't we test SharingArgTerms?
            MaybeSharingAs = no
        ;
            SharingFunctor = "yes",
            SharingArgTerms = [SharingAsTerm],
            MaybeSharingAs = yes(parse_structure_sharing_domain(SharingAsTerm))
        )
    then
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, PredName, Modes),
        varset.coerce(VarSet, ProgVarSet),
        varset.coerce(VarSet, TVarSet),
        Sharing = decl_pragma_struct_sharing_info(PredNameModesPF, HeadVars,
            Types, ProgVarSet, TVarSet, MaybeSharingAs, Context, SeqNum),
        Item = item_decl_pragma(decl_pragma_struct_sharing(Sharing)),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Syntax error in"),
            pragma_decl("structure_sharing"), words("declaration."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_pragma_structure_reuse(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [PredAndModesTerm0, HeadVarsTerm,
            HeadVarTypesTerm, MaybeStructureReuseTerm],
        ReuseContextPieces = cord.from_list([words("In"),
            pragma_decl("structure_reuse"), words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), ReuseContextPieces,
            VarSet, PredAndModesTerm0, MaybeNameAndModes),
        MaybeNameAndModes = ok3(PredName, PredOrFunc, Modes),

        % Parse the head variables:
        HeadVarsTerm = term.functor(term.atom("vars"), ListHVTerms, _),
        term_vars.vars_in_terms(ListHVTerms, HeadVarsGeneric),
        list.map(term.coerce_var, HeadVarsGeneric, HeadVars),

        % Parse the types:
        HeadVarTypesTerm = term.functor(term.atom("types"), ListTypeTerms, _),
        maybe_parse_types(no_allow_ho_inst_info(wnhii_pragma_struct_reuse),
            ListTypeTerms, Types),

        % Parse the actual structure reuse information.
        MaybeStructureReuseTerm = term.functor(term.atom(ReuseFunctor),
            ReuseArgTerms, _),
        (
            ReuseFunctor = "not_available",
            % XXX Why don't we test ReuseArgTerms?
            MaybeStructureReuse = no
        ;
            ReuseFunctor = "yes",
            ReuseArgTerms = [StructureReuseTerm],
            StructureReuse = parse_structure_reuse_domain(StructureReuseTerm),
            MaybeStructureReuse = yes(StructureReuse)
        )
    then
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, PredName, Modes),
        varset.coerce(VarSet, ProgVarSet),
        varset.coerce(VarSet, TVarSet),
        Reuse = decl_pragma_struct_reuse_info(PredNameModesPF, HeadVars, Types,
            ProgVarSet, TVarSet, MaybeStructureReuse, Context, SeqNum),
        Item = item_decl_pragma(decl_pragma_struct_reuse(Reuse)),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Syntax error in"),
            pragma_decl("structure_reuse"), words("declaration."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_pragma_exceptions(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredOrFuncTerm, PredNameTerm, ArityTerm, ModeNumTerm,
            ThrowStatusTerm],
        parse_predicate_or_function(VarSet, PredOrFuncTerm, MaybePredOrFunc),
        PNContextPieces = cord.from_list(
            [words("In the second argument of"), pragma_decl("exceptions"),
            words("declaration:"), nl]),
        parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
            VarSet, PNContextPieces, PredNameTerm, MaybePredName),
        ArityContextPieces = cord.from_list(
            [words("In the third argument of an"),
            pragma_decl("unused_args"), words("declaration:"), nl]),
        parse_decimal_int(ArityContextPieces, VarSet, ArityTerm, MaybeArity),
        ModeNumContextPieces = cord.from_list(
            [words("In the fourth argument of an"),
            pragma_decl("unused_args"), words("declaration:"), nl]),
        parse_decimal_int(ModeNumContextPieces, VarSet, ModeNumTerm,
            MaybeModeNum),
        ( if
            ThrowStatusTerm = term.functor(term.atom(ThrowStatusFunctor),
                ThrowStatusArgTerms, _),
            ( ThrowStatusFunctor = "will_not_throw"
            ; ThrowStatusFunctor = "may_throw"
            ; ThrowStatusFunctor = "conditional"
            )
        then
            (
                ThrowStatusFunctor = "will_not_throw",
                (
                    ThrowStatusArgTerms = [],
                    MaybeThrowStatus = ok1(will_not_throw)
                ;
                    ThrowStatusArgTerms = [_ | _],
                    WillNotThrowPieces =
                        [words("In the fifth argument of"),
                        pragma_decl("exceptions"), words("declaration:"), nl,
                        words("error:"), quote("will_not_throw"),
                        words("must have no arguments."), nl],
                    WillNotThrowSpec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(ThrowStatusTerm), WillNotThrowPieces),
                    MaybeThrowStatus = error1([WillNotThrowSpec])
                )
            ;
                ThrowStatusFunctor = "may_throw",
                ( if
                    ThrowStatusArgTerms = [ExceptionTypeTerm],
                    ExceptionTypeTerm = term.functor(
                        term.atom(ExceptionFunctor), [], _),
                    (
                        ExceptionFunctor = "user_exception",
                        ExceptionType = user_exception
                    ;
                        ExceptionFunctor = "type_exception",
                        ExceptionType = type_exception
                    )
                then
                    MaybeThrowStatus = ok1(may_throw(ExceptionType))
                else
                    MayThrowPieces =
                        [words("In the fifth argument of"),
                        pragma_decl("exceptions"), words("declaration:"), nl,
                        words("error:"), quote("may_throw"),
                        words("must have one argument,"),
                        words("which must be either"),
                        quote("user_exception"), words("or"),
                        quote("type_exception"), suffix("."), nl],
                    MayThrowSpec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(ThrowStatusTerm), MayThrowPieces),
                    MaybeThrowStatus = error1([MayThrowSpec])
                )
            ;
                ThrowStatusFunctor = "conditional",
                (
                    ThrowStatusArgTerms = [],
                    MaybeThrowStatus = ok1(throw_conditional)
                ;
                    ThrowStatusArgTerms = [_ | _],
                    ConditionalPieces =
                        [words("In the fifth argument of"),
                        pragma_decl("exceptions"), words("declaration:"), nl,
                        words("error:"), quote("conditional"),
                        words("must have no arguments."), nl],
                    ConditionalSpec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(ThrowStatusTerm), ConditionalPieces),
                    MaybeThrowStatus = error1([ConditionalSpec])
                )
            )
        else
            ThrowStatusTermStr = describe_error_term(VarSet, ThrowStatusTerm),
            ThrowStatusPieces = [words("In the fifth argument of"),
                pragma_decl("exceptions"), words("declaration:"), nl,
                words("error: expected one of"),
                quote("will_not_throw"), suffix(","),
                quote("may_throw"), suffix(","), words("and"),
                quote("conditional"), suffix(","),
                words("got"), quote(ThrowStatusTermStr), suffix("."), nl],
            ThrowStatusSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(ThrowStatusTerm),
                ThrowStatusPieces),
            MaybeThrowStatus = error1([ThrowStatusSpec])
        ),
        ( if
            MaybePredOrFunc = ok1(PredOrFunc),
            MaybePredName = ok1(PredName),
            MaybeArity = ok1(Arity),
            MaybeModeNum = ok1(ModeNum),
            MaybeThrowStatus = ok1(ThrowStatus)
        then
            PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc,
                PredName, user_arity(Arity), ModeNum),
            Exceptions = gen_pragma_exceptions_info(PredNameArityPFMn,
                ThrowStatus, Context, SeqNum),
            Item = item_generated_pragma(gen_pragma_exceptions(Exceptions)),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybePredOrFunc) ++
                get_any_errors1(MaybePredName) ++
                get_any_errors1(MaybeArity) ++
                get_any_errors1(MaybeModeNum) ++
                get_any_errors1(MaybeThrowStatus),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _]
        ; PragmaTerms = [_, _, _, _]
        ; PragmaTerms = [_, _, _, _, _, _ | _]
        ),
        Pieces = [words("Error: an"), pragma_decl("exceptions"),
            words("declaration must have five arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_pragma_trailing_info(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredOrFuncTerm, PredNameTerm, ArityTerm, ModeNumTerm,
            TrailingStatusTerm],
        parse_predicate_or_function(VarSet, PredOrFuncTerm, MaybePredOrFunc),
        PNContextPieces = cord.from_list(
            [words("In the second argument of"), pragma_decl("traling_info"),
            words("declaration:"), nl]),
        parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
            VarSet, PNContextPieces, PredNameTerm, MaybePredName),
        ArityContextPieces = cord.from_list(
            [words("In the third argument of an"),
            pragma_decl("unused_args"), words("declaration:"), nl]),
        parse_decimal_int(ArityContextPieces, VarSet, ArityTerm, MaybeArity),
        ModeNumContextPieces = cord.from_list(
            [words("In the fourth argument of an"),
            pragma_decl("unused_args"), words("declaration:"), nl]),
        parse_decimal_int(ModeNumContextPieces, VarSet, ModeNumTerm,
            MaybeModeNum),
        ( if
            TrailingStatusTerm = term.functor(term.atom(TrailingStatusFunctor),
                [], _),
            (
                TrailingStatusFunctor = "will_not_modify_trail",
                TrailingStatus0 = trail_will_not_modify
            ;
                TrailingStatusFunctor = "may_modify_trail",
                TrailingStatus0 = trail_may_modify
            ;
                TrailingStatusFunctor = "conditional",
                TrailingStatus0 = trail_conditional
            )
        then
            MaybeTrailingStatus = ok1(TrailingStatus0)
        else
            TrailingStatusTermStr =
                describe_error_term(VarSet, TrailingStatusTerm),
            TrailingStatusPieces = [words("In the fifth argument of"),
                pragma_decl("trailing_info"), words("declaration:"), nl,
                words("error: expected one of"),
                quote("will_not_modify_trail"), suffix(","),
                quote("may_modify_trail"), suffix(","), words("and"),
                quote("conditional"), suffix(","),
                words("got"), quote(TrailingStatusTermStr), suffix("."), nl],
            TrailingStatusSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(TrailingStatusTerm),
                TrailingStatusPieces),
            MaybeTrailingStatus = error1([TrailingStatusSpec])
        ),
        ( if
            MaybePredOrFunc = ok1(PredOrFunc),
            MaybePredName = ok1(PredName),
            MaybeArity = ok1(Arity),
            MaybeModeNum = ok1(ModeNum),
            MaybeTrailingStatus = ok1(TrailingStatus)
        then
            PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc,
                PredName, user_arity(Arity), ModeNum),
            Trailing = gen_pragma_trailing_info(PredNameArityPFMn,
                TrailingStatus, Context, SeqNum),
            Item = item_generated_pragma(gen_pragma_trailing(Trailing)),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybePredOrFunc) ++
                get_any_errors1(MaybePredName) ++
                get_any_errors1(MaybeArity) ++
                get_any_errors1(MaybeModeNum) ++
                get_any_errors1(MaybeTrailingStatus),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _]
        ; PragmaTerms = [_, _, _, _]
        ; PragmaTerms = [_, _, _, _, _, _ | _]
        ),
        Pieces = [words("Error:"), pragma_decl("trailing_info"),
            words("declaration must have five arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_pragma_mm_tabling_info(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredOrFuncTerm, PredNameTerm, ArityTerm, ModeNumTerm,
            MMTablingStatusTerm],
        parse_predicate_or_function(VarSet, PredOrFuncTerm, MaybePredOrFunc),
        PNContextPieces = cord.from_list(
            [words("In the second argument of"),
            pragma_decl("mm_tabling_info"), words("declaration:"), nl]),
        parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
            VarSet, PNContextPieces, PredNameTerm, MaybePredName),
        ArityContextPieces = cord.from_list(
            [words("In the third argument of an"),
            pragma_decl("mm_tabling_info"), words("declaration:"), nl]),
        parse_decimal_int(ArityContextPieces, VarSet, ArityTerm, MaybeArity),
        ModeNumContextPieces = cord.from_list(
            [words("In the fourth argument of an"),
            pragma_decl("mm_tabling_info"), words("declaration:"), nl]),
        parse_decimal_int(ModeNumContextPieces, VarSet, ModeNumTerm,
            MaybeModeNum),
        ( if
            MMTablingStatusTerm = term.functor(
                term.atom(MMTablingStatusFunctor), [], _),
            (
                MMTablingStatusFunctor = "mm_tabled_will_not_call",
                MMTablingStatus0 = mm_tabled_will_not_call
            ;
                MMTablingStatusFunctor = "mm_tabled_may_call",
                MMTablingStatus0 = mm_tabled_may_call
            ;
                MMTablingStatusFunctor = "mm_tabled_conditional",
                MMTablingStatus0 = mm_tabled_conditional
            )
        then
            MaybeMMTablingStatus = ok1(MMTablingStatus0)
        else
            MMTablingStatusTermStr =
                describe_error_term(VarSet, MMTablingStatusTerm),
            MMTablingStatusPieces = [words("In the fifth argument of"),
                pragma_decl("mm_tabling_info"), words("declaration:"), nl,
                words("error: expected one of"),
                quote("will_not_modify_trail"), suffix(","),
                quote("may_modify_trail"), suffix(","), words("and"),
                quote("conditional"), suffix(","),
                words("got"), quote(MMTablingStatusTermStr), suffix("."), nl],
            MMTablingStatusSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(MMTablingStatusTerm), MMTablingStatusPieces),
            MaybeMMTablingStatus = error1([MMTablingStatusSpec])
        ),
        ( if
            MaybePredOrFunc = ok1(PredOrFunc),
            MaybePredName = ok1(PredName),
            MaybeArity = ok1(Arity),
            MaybeModeNum = ok1(ModeNum),
            MaybeMMTablingStatus = ok1(MMTablingStatus)
        then
            PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc,
                PredName, user_arity(Arity), ModeNum),
            Tabling = gen_pragma_mm_tabling_info(PredNameArityPFMn,
                MMTablingStatus, Context, SeqNum),
            Item = item_generated_pragma(gen_pragma_mm_tabling(Tabling)),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybePredOrFunc) ++
                get_any_errors1(MaybePredName) ++
                get_any_errors1(MaybeArity) ++
                get_any_errors1(MaybeModeNum) ++
                get_any_errors1(MaybeMMTablingStatus),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _]
        ; PragmaTerms = [_, _, _, _]
        ; PragmaTerms = [_, _, _, _, _, _ | _]
        ),
        Pieces = [words("Error: an"), pragma_decl("mm_tabling_info"),
            words("declaration must have five arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_predicate_or_function(varset::in, term::in,
    maybe1(pred_or_func)::out) is det.

parse_predicate_or_function(VarSet, Term, MaybePredOrFunc) :-
    ( if
        Term = term.functor(term.atom(Functor), [], _),
        (
            Functor = "predicate",
            PredOrFunc = pf_predicate
        ;
            Functor = "function",
            PredOrFunc = pf_function
        )
    then
        MaybePredOrFunc = ok1(PredOrFunc)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected either"),
            quote("predicate"), words("or"), quote("function"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), Pieces),
        MaybePredOrFunc = error1([Spec])
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_pragma_analysis.
%---------------------------------------------------------------------------%
