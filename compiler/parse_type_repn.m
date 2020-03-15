%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_type_repn.m.
%
% This module parses type_repn items. When a module exports an abstract type,
% the compiler can put one of these into the interface file to tell other
% modules that refer to that type how values of that type are represented.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_type_repn.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module term.
:- import_module varset.

    % Parse ":- type_representation(...)" items.
    %
:- pred parse_type_repn_item(module_name::in, varset::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.                  % for foreign_language
:- import_module parse_tree.error_util.
:- import_module parse_tree.parse_pragma.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_defn.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.

:- import_module assoc_list.
:- import_module cord.
:- import_module int.
:- import_module integer.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module uint.

%---------------------------------------------------------------------------%

parse_type_repn_item(_ModuleName, VarSet, ArgTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if ArgTerms = [TypeTerm, RepnTerm] then
        TypeContextPieces = cord.from_list([words("In the first argument of"),
            quote("type_representation"), words("item:")]),
        parse_type_defn_head(TypeContextPieces, root_module_name, VarSet,
            TypeTerm, MaybeTypeSymNameAndArgs),
        ( if
            RepnTerm = term.functor(term.atom(AtomStr), RepnArgs, RepnContext),
            ( AtomStr = "is_direct_dummy"
            ; AtomStr = "is_notag"
            ; AtomStr = "is_eqv_to"
            ; AtomStr = "fits_in_n_bits"
            ; AtomStr = "is_word_aligned_ptr"
            ; AtomStr = "has_direct_arg_functors"
            ; AtomStr = "du_repn"
            ; AtomStr = "foreign_type_repn"
            )
        then
            (
                AtomStr = "is_direct_dummy",
                parse_no_arg_type_repn(AtomStr, RepnArgs, RepnContext,
                    tcrepn_is_direct_dummy, MaybeRepn)
            ;
                AtomStr = "is_notag",
                parse_no_arg_type_repn(AtomStr, RepnArgs, RepnContext,
                    tcrepn_is_notag, MaybeRepn)
            ;
                AtomStr = "is_eqv_to",
                parse_type_repn_eqv_to(VarSet, AtomStr, RepnArgs,
                    RepnContext, MaybeRepn)
            ;
                AtomStr = "fits_in_n_bits",
                parse_type_repn_fits_in_n_bits(AtomStr, RepnArgs,
                    RepnContext, MaybeRepn)
            ;
                AtomStr = "is_word_aligned_ptr",
                parse_no_arg_type_repn(AtomStr, RepnArgs, RepnContext,
                    tcrepn_is_word_aligned_ptr, MaybeRepn)
            ;
                AtomStr = "has_direct_arg_functors",
                parse_type_repn_has_direct_arg_functors(AtomStr, RepnArgs,
                    RepnContext, MaybeRepn)
            ;
                AtomStr = "du_repn",
                (
                    RepnArgs = [RepnArg],
                    parse_type_repn_du(VarSet, RepnArg, MaybeDuRepn),
                    (
                        MaybeDuRepn = ok1(DuRepn),
                        MaybeRepn = ok1(tcrepn_du(DuRepn))
                    ;
                        MaybeDuRepn = error1(Specs),
                        MaybeRepn = error1(Specs)
                    )
                ;
                    ( RepnArgs = []
                    ; RepnArgs = [_, _ | _]
                    ),
                    DuPieces = [words("Error: in second argument of a"),
                        decl("type_representation"), words("item:"),
                        quote(AtomStr), words("should have"),
                        words("exactly one argument."), nl],
                    DuSpec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree, RepnContext, DuPieces),
                    MaybeRepn = error1([DuSpec])
                )
            ;
                AtomStr = "foreign_type_repn",
                parse_type_repn_foreign_type(VarSet, AtomStr, RepnArgs,
                    RepnContext, MaybeRepn)
            )
        else
            RepnTermStr = describe_error_term(VarSet, RepnTerm),
            RepnPieces = [words("Error: the second argument of a"),
                decl("type_representation"), words("item"),
                words("should specify the type's representation."),
                words("Expected one of"),
                quote("is_direct_dummy"), suffix(","),
                quote("is_notag"), suffix(","),
                quote("is_eqv_to"), suffix(","),
                quote("fits_in_n_bits"), suffix(","),
                quote("is_word_aligned_ptr"), suffix(","),
                quote("has_direct_arg_functors"), suffix(","),
                quote("du_repn"), words("and"),
                quote("maybe_foreign_type_repn"), suffix(","),
                words("got"), quote(RepnTermStr), suffix("."), nl],
            RepnSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(RepnTerm), RepnPieces),
            MaybeRepn = error1([RepnSpec])
        ),
        ( if
            MaybeTypeSymNameAndArgs = ok2(TypeCtorSymName, TypeArgVars),
            MaybeRepn = ok1(Repn)
        then
            varset.coerce(VarSet, TVarSet),
            ItemRepnInfo = item_type_repn_info(TypeCtorSymName, TypeArgVars,
                Repn, TVarSet, Context, SeqNum),
            Item = item_type_repn(ItemRepnInfo),
            MaybeIOM = ok1(iom_item(Item))
        else
            MaybeIOM = error1([])
        )
    else
        Pieces = [words("Error: a"),
            decl("type_representation"), words("item"),
            words("should have exactly two arguments: the type,"),
            words("and a description of its representation."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_no_arg_type_repn(string::in, list(term)::in,
    term.context::in, type_ctor_repn_info::in,
    maybe1(type_ctor_repn_info)::out) is det.

parse_no_arg_type_repn(RepnStr, RepnArgs, RepnContext,
        NoArgRepn, MaybeRepn) :-
    (
        RepnArgs = [],
        MaybeRepn = ok1(NoArgRepn)
    ;
        RepnArgs = [_ | _],
        Pieces = [words("Error:"), quote(RepnStr),
            words("should not have any arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            RepnContext, Pieces),
        MaybeRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_eqv_to(varset::in, string::in, list(term)::in,
    term.context::in, maybe1(type_ctor_repn_info)::out) is det.

parse_type_repn_eqv_to(VarSet, RepnStr, RepnArgs, RepnContext, MaybeRepn) :-
    (
        RepnArgs = [RepnArg],
        HOInstInfo = no_allow_ho_inst_info(wnhii_eqv_type_defn_body),
        ContextPieces = cord.from_list([words("In the second argument of"),
            quote("type_representation"), words("item:")]),
        parse_type(HOInstInfo, VarSet, ContextPieces, RepnArg, MaybeEqvType),
        (
            MaybeEqvType = ok1(EqvType),
            MaybeRepn = ok1(tcrepn_is_eqv_to(EqvType))
        ;
            MaybeEqvType = error1(Specs),
            MaybeRepn = error1(Specs)
        )
    ;
        ( RepnArgs = []
        ; RepnArgs = [_, _ | _]
        ),
        Pieces = [words("Error:"), quote(RepnStr),
            words("should have exactly one argument, a type."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            RepnContext, Pieces),
        MaybeRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_fits_in_n_bits(string::in, list(term)::in,
    term.context::in, maybe1(type_ctor_repn_info)::out) is det.

parse_type_repn_fits_in_n_bits(RepnStr, RepnArgs, RepnContext, MaybeRepn) :-
    (
        RepnArgs = [RepnArg1, RepnArg2],
        ( if decimal_term_to_int(RepnArg1, N0) then
            MaybeNumBits = ok1(N0)
        else
            NumBitsPieces = [words("Error: the first argument of"),
                quote(RepnStr), words("should be an integer."), nl],
            NumBitsSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(RepnArg1), NumBitsPieces),
            MaybeNumBits = error1([NumBitsSpec])
        ),
        ( if
            RepnArg2 = term.functor(term.atom(AtomStr2), [], _),
            fill_kind_string(FillKind2, AtomStr2)
        then
            MaybeFillKind = ok1(FillKind2)
        else
            FillKindPieces = [words("Error: the second argument of"),
                quote(RepnStr), words("should be a fill kind."), nl],
            FillKindSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(RepnArg2), FillKindPieces),
            MaybeFillKind = error1([FillKindSpec])
        ),
        ( if
            MaybeNumBits = ok1(N),
            MaybeFillKind = ok1(FillKind)
        then
            MaybeRepn = ok1(tcrepn_fits_in_n_bits(N, FillKind))
        else
            Specs = get_any_errors1(MaybeNumBits) ++
                get_any_errors1(MaybeFillKind),
            MaybeRepn = error1(Specs)
        )
    ;
        ( RepnArgs = []
        ; RepnArgs = [_]
        ; RepnArgs = [_, _, _ | _]
        ),
        Pieces = [words("Error:"), quote(RepnStr),
            words("should have exactly two arguments,"),
            words("an integer and a fill kind."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            RepnContext, Pieces),
        MaybeRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_has_direct_arg_functors(string::in, list(term)::in,
    term.context::in, maybe1(type_ctor_repn_info)::out) is det.

parse_type_repn_has_direct_arg_functors(RepnStr, RepnArgs, RepnContext,
        MaybeRepn) :-
    (
        RepnArgs = [RepnArg],
        ( if list_term_to_term_list(RepnArg, FunctorTerms) then
            parse_functor_with_arities(RepnStr, 1, FunctorTerms,
                MaybeFunctors),
            (
                MaybeFunctors = ok1(Functors),
                MaybeRepn = ok1(tcrepn_has_direct_arg_functors(Functors))
            ;
                MaybeFunctors = error1(Specs),
                MaybeRepn = error1(Specs)
            )
        else
            Pieces = [words("Error: the argument of"), quote(RepnStr),
                words("should be a list of function symbols with arities."),
                nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(RepnArg), Pieces),
            MaybeRepn = error1([Spec])
        )
    ;
        ( RepnArgs = []
        ; RepnArgs = [_, _ | _]
        ),
        Pieces = [words("Error:"), quote(RepnStr),
            words("should have exactly one argument,"),
            words("a list of function symbols with arities."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            RepnContext, Pieces),
        MaybeRepn = error1([Spec])
    ).

:- pred parse_functor_with_arities(string::in, int::in, list(term)::in,
    maybe1(list(sym_name_arity))::out) is det.

parse_functor_with_arities(_, _, [], ok1([])).
parse_functor_with_arities(RepnStr, Nth, [Term | Terms], MaybeFunctors) :-
    ( if parse_unqualified_name_and_arity(Term, SymName, Arity) then
        MaybeHeadFunctor = ok1(sym_name_arity(SymName, Arity))
    else
        Pieces = [words("Error: the"), nth_fixed(Nth), words("element"),
            words("of the list in the second argument of"), quote(RepnStr),
            words("should have the form"),
            quote("functorname/arity"), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeHeadFunctor = error1([Spec])
    ),
    parse_functor_with_arities(RepnStr, Nth + 1, Terms, MaybeTailFunctors),
    ( if
        MaybeHeadFunctor = ok1(HeadFunctor),
        MaybeTailFunctors = ok1(TailFunctors)
    then
        MaybeFunctors = ok1([HeadFunctor | TailFunctors])
    else
        Specs = get_any_errors1(MaybeHeadFunctor) ++
            get_any_errors1(MaybeTailFunctors),
        MaybeFunctors = error1(Specs)
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_du(varset::in, term::in, maybe1(du_repn)::out) is det.

parse_type_repn_du(VarSet, Term, MaybeDuRepn) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "notag"
        ; AtomStr = "direct_dummy"
        ; AtomStr = "enum"
        ; AtomStr = "gen_du"
        ; AtomStr = "gen_du_only_functor"
        )
    then
        (
            AtomStr = "notag",
            parse_type_repn_du_notag(VarSet, TermContext, ArgTerms,
                MaybeDuRepn)
        ;
            AtomStr = "direct_dummy",
            parse_type_repn_du_direct_dummy(VarSet, TermContext, ArgTerms,
                MaybeDuRepn)
        ;
            AtomStr = "enum",
            parse_type_repn_du_enum(VarSet, TermContext, ArgTerms, MaybeDuRepn)
        ;
            AtomStr = "gen_du",
            parse_type_repn_du_gen_du(VarSet, TermContext, ArgTerms,
                MaybeDuRepn)
        ;
            AtomStr = "gen_du_only_functor",
            parse_type_repn_du_gen_du_only_functor(VarSet, TermContext,
                ArgTerms, MaybeDuRepn)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Expected one of"),
            quote("notag(...)"), suffix(","),
            quote("direct_dummy(...)"), suffix(","),
            quote("enum(...)"), suffix(","),
            quote("gen_du"),  words("and"),
            quote("gen_du_only_functor"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), malformed_du_prefix ++ Pieces),
        MaybeDuRepn = error1([Spec])
    ).

:- pred parse_type_repn_du_notag(varset::in, prog_context::in, list(term)::in,
    maybe1(du_repn)::out) is det.

parse_type_repn_du_notag(VarSet, TermContext, ArgTerms, MaybeDuRepn) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2],
        ContextPieces1 = [words("In first argument of"),
            quote("notag"), suffix(":"), nl],
        parse_string(VarSet, ContextPieces1, "function symbol",
            ArgTerm1, MaybeFunctorName),
        DescPieces2 = [words("the second argument of"), quote("notag")],
        parse_c_j_cs_e_repn(VarSet, DescPieces2, min_list_length_0,
            ArgTerm2, MaybeCJCsERepn),
        ( if
            MaybeFunctorName = ok1(FunctorName),
            MaybeCJCsERepn = ok1(CJCsERepn)
        then
            NotagRepn = notag_repn(FunctorName, CJCsERepn),
            DuRepn = dur_notag(NotagRepn),
            MaybeDuRepn = ok1(DuRepn)
        else
            Specs =
                get_any_errors1(MaybeFunctorName) ++
                get_any_errors1(MaybeCJCsERepn),
            MaybeDuRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = [quote("notag"),
            words("should have exactly two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, malformed_du_prefix ++ Pieces),
        MaybeDuRepn = error1([Spec])
    ).

:- pred parse_type_repn_du_direct_dummy(varset::in, prog_context::in,
    list(term)::in, maybe1(du_repn)::out) is det.

parse_type_repn_du_direct_dummy(VarSet, TermContext, ArgTerms, MaybeDuRepn) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2],
        ContextPieces1 = [words("In first argument of"),
            quote("direct_dummy"), suffix(":"), nl],
        parse_string(VarSet, ContextPieces1, "function symbol",
            ArgTerm1, MaybeFunctorName),
        DescPieces2 = [words("the second argument of"), quote("direct_dummy")],
        parse_c_j_cs_e_repn_or_enum(VarSet, DescPieces2, min_list_length_0,
            ArgTerm2, MaybeCJCsERepnOrEnum),
        ( if
            MaybeFunctorName = ok1(FunctorName),
            MaybeCJCsERepnOrEnum = ok1(CJCsERepnOrEnum)
        then
            DirectDummyRepn = direct_dummy_repn(FunctorName, CJCsERepnOrEnum),
            DuRepn = dur_direct_dummy(DirectDummyRepn),
            MaybeDuRepn = ok1(DuRepn)
        else
            Specs =
                get_any_errors1(MaybeFunctorName) ++
                get_any_errors1(MaybeCJCsERepnOrEnum),
            MaybeDuRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = [quote("direct_dummy"),
            words("should have exactly one argument."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, malformed_du_prefix ++ Pieces),
        MaybeDuRepn = error1([Spec])
    ).

:- pred parse_type_repn_du_enum(varset::in, prog_context::in, list(term)::in,
    maybe1(du_repn)::out) is det.

parse_type_repn_du_enum(VarSet, TermContext, ArgTerms, MaybeDuRepn) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4],
        ContextPieces1 = [words("In first argument of"), quote("enum"),
            suffix(":")],
        parse_string(VarSet, ContextPieces1, "function symbol",
            ArgTerm1, MaybeEnumFunctorName1),
        ContextPieces2 = [words("In second argument of"), quote("enum"),
            suffix(":")],
        parse_string(VarSet, ContextPieces2, "function symbol",
            ArgTerm2, MaybeEnumFunctorName2),
        ( if list_term_to_term_list(ArgTerm3, LaterEnumFunctorTerms) then
            ContextPieces = [words("third argument of"), quote("enum"),
                suffix(":")],
            parse_strings(VarSet, ContextPieces, "function symbol", 1,
                LaterEnumFunctorTerms, LaterEnumFunctorNames, LaterEnumSpecs)
        else
            LaterEnumFunctorNames = [],
            LaterEnumPieces = [words("Error: the third argument of"),
                quote("enum"), words("should be"),
                words("a list of function symbols."), nl],
            LaterEnumSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(ArgTerm3), LaterEnumPieces),
            LaterEnumSpecs = [LaterEnumSpec]
        ),
        DescPieces4 = [words("the fourth argument of"), quote("enum")],
        parse_c_j_cs_e_repn_or_enum(VarSet, DescPieces4, min_list_length_0,
            ArgTerm4, MaybeCJCsERepnOrEnum),
        ( if
            MaybeEnumFunctorName1 = ok1(EnumFunctorName1),
            MaybeEnumFunctorName2 = ok1(EnumFunctorName2),
            LaterEnumSpecs = [],
            MaybeCJCsERepnOrEnum = ok1(CJCsERepnOrEnum)
        then
            EnumRepn = enum_repn(EnumFunctorName1, EnumFunctorName2,
                LaterEnumFunctorNames, CJCsERepnOrEnum),
            MaybeDuRepn = ok1(dur_enum(EnumRepn))
        else
            Specs =
                get_any_errors1(MaybeEnumFunctorName1) ++
                get_any_errors1(MaybeEnumFunctorName2) ++
                LaterEnumSpecs ++
                get_any_errors1(MaybeCJCsERepnOrEnum),
            MaybeDuRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _]
        ; ArgTerms = [_, _, _]
        ; ArgTerms = [_, _, _, _, _ | _]
        ),
        Pieces = [quote("enum"), words("should have four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, malformed_du_prefix ++ Pieces),
        MaybeDuRepn = error1([Spec])
    ).

:- pred parse_type_repn_du_gen_du(varset::in, prog_context::in, list(term)::in,
    maybe1(du_repn)::out) is det.

parse_type_repn_du_gen_du(VarSet, TermContext, ArgTerms, MaybeDuRepn) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4],
        parse_du_functor(VarSet, ArgTerm1, MaybeFunctor1),
        parse_du_functor(VarSet, ArgTerm2, MaybeFunctor2),
        ( if list_term_to_term_list(ArgTerm3, OtherFunctorTerms) then
            parse_du_functors(VarSet, OtherFunctorTerms,
                OtherFunctors0, OtherFunctorSpecs),
            (
                OtherFunctorSpecs = [],
                MaybeOtherFunctors = ok1(OtherFunctors0)
            ;
                OtherFunctorSpecs = [_ | _],
                MaybeOtherFunctors = error1(OtherFunctorSpecs)
            )
        else
            Pieces = [words("Error: the third argument of"), quote("gen_du"),
                words("should be a list of function symbol representations."),
                nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(ArgTerm3), Pieces),
            MaybeOtherFunctors = error1([Spec])
        ),
        DescPieces4 = [words("fourth argument of"), quote("gen_du")],
        parse_c_j_cs_e_repn(VarSet, DescPieces4, min_list_length_0,
            ArgTerm4, MaybeCJCsERepn),
        ( if
            MaybeFunctor1 = ok1(Functor1),
            MaybeFunctor2 = ok1(Functor2),
            MaybeOtherFunctors = ok1(OtherFunctors),
            MaybeCJCsERepn = ok1(CJCsERepn)
        then
            MoreFunctors = gen_du_repn_more_functors(Functor1, Functor2,
                OtherFunctors, CJCsERepn),
            MaybeDuRepn = ok1(dur_gen_more_functors(MoreFunctors))
        else
            Specs = get_any_errors1(MaybeFunctor1) ++
                get_any_errors1(MaybeFunctor2) ++
                get_any_errors1(MaybeOtherFunctors) ++
                get_any_errors1(MaybeCJCsERepn),
            MaybeDuRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _]
        ; ArgTerms = [_, _, _]
        ; ArgTerms = [_, _, _, _, _ | _]
        ),
        Pieces = [words("Error:"), quote("gen_du"),
            words("should have exactly four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeDuRepn = error1([Spec])
    ).

:- pred parse_type_repn_du_gen_du_only_functor(varset::in, prog_context::in,
    list(term)::in, maybe1(du_repn)::out) is det.

parse_type_repn_du_gen_du_only_functor(VarSet, TermContext, ArgTerms,
        MaybeDuRepn) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4],
        ContextPieces1 = [words("In first argument of"),
            quote("gen_du_only_functor"), suffix(":")],
        parse_string(VarSet, ContextPieces1, "function symbol",
            ArgTerm1, MaybeFunctorName),
        parse_only_functor_args(63u, VarSet, ArgTerm2,
            MaybeOnlyFunctorArgs64),
        parse_only_functor_args(31u, VarSet, ArgTerm3,
            MaybeOnlyFunctorArgs32),
        DescPieces4 = [words("fourth argument of"),
            quote("gen_du_only_functor")],
        parse_c_j_cs_e_repn(VarSet, DescPieces4, min_list_length_0,
            ArgTerm4, MaybeCJCsERepn),
        ( if
            MaybeFunctorName = ok1(FunctorName),
            MaybeOnlyFunctorArgs64 = ok1(OnlyFunctorArgs64),
            MaybeOnlyFunctorArgs32 = ok1(OnlyFunctorArgs32),
            MaybeCJCsERepn = ok1(CJCsERepn)
        then
            OnlyFunctor = gen_du_repn_only_functor(FunctorName,
                OnlyFunctorArgs64, OnlyFunctorArgs32, CJCsERepn),
            MaybeDuRepn = ok1(dur_gen_only_functor(OnlyFunctor))
        else
            Specs = get_any_errors1(MaybeFunctorName) ++
                get_any_errors1(MaybeOnlyFunctorArgs64) ++
                get_any_errors1(MaybeOnlyFunctorArgs32) ++
                get_any_errors1(MaybeCJCsERepn),
            MaybeDuRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _]
        ; ArgTerms = [_, _, _]
        ; ArgTerms = [_, _, _, _, _ | _]
        ),
        Pieces = [words("Error:"), quote("gen_du_only_functor"),
            words("should have exactly four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeDuRepn = error1([Spec])
    ).

%---------------------%

:- func malformed_du_prefix = list(format_component).

malformed_du_prefix =
    [words("Error: malformed du type representation"),
    words("inside the only argument of"), quote("du_repn"), suffix("."), nl].

%---------------------%

:- pred parse_only_functor_args(uint::in, varset::in, term::in,
    maybe1(gen_du_only_functor_args)::out) is det.

parse_only_functor_args(MaxNumBits, VarSet, Term, MaybeOnlyFunctorArgs) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "local_args"
        ; AtomStr = "remote_args"
        )
    then
        (
            ArgTerms = [ArgTerm],
            ( if
                list_term_to_term_list(ArgTerm, ElementTerms),
                ElementTerms = [HeadElementTerm | TailElementTerms]
            then
                (
                    AtomStr = "local_args",
                    parse_local_pos_size(MaxNumBits, VarSet,
                        HeadElementTerm, MaybeHeadArg),
                    parse_local_pos_sizes(MaxNumBits, VarSet,
                        TailElementTerms, TailArgs, TailSpecs),
                    ( if
                        MaybeHeadArg = ok1(HeadArg),
                        TailSpecs = []
                    then
                        OnlyFunctorArgs = gen_du_only_functor_local_args(
                            one_or_more(HeadArg, TailArgs)),
                        MaybeOnlyFunctorArgs = ok1(OnlyFunctorArgs)
                    else
                        Specs = get_any_errors1(MaybeHeadArg) ++ TailSpecs,
                        MaybeOnlyFunctorArgs = error1(Specs)
                    )
                ;
                    AtomStr = "remote_args",
                    parse_arg_pos_size(MaxNumBits, VarSet,
                        HeadElementTerm, MaybeHeadArg),
                    parse_arg_pos_sizes(MaxNumBits, VarSet,
                        TailElementTerms, TailArgs, TailSpecs),
                    ( if
                        MaybeHeadArg = ok1(HeadArg),
                        TailSpecs = []
                    then
                        OnlyFunctorArgs = gen_du_only_functor_remote_args(
                            one_or_more(HeadArg, TailArgs)),
                        MaybeOnlyFunctorArgs = ok1(OnlyFunctorArgs)
                    else
                        Specs = get_any_errors1(MaybeHeadArg) ++ TailSpecs,
                        MaybeOnlyFunctorArgs = error1(Specs)
                    )
                )
            else
                ( AtomStr = "local_args", LR = "local"
                ; AtomStr = "remote_args", LR = "remote"
                ),
                Pieces = [words("Error: the argument of"), quote(AtomStr),
                    words("should be a nonempty list of"),
                    words(LR), words("argument representations."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeOnlyFunctorArgs = error1([Spec])
            )
        ;
            ( ArgTerms = []
            ; ArgTerms = [_, _ | _]
            ),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error:"),
                quote(AtomStr), words("should have exactly one argument."),
                quote("remote_args"), suffix(","),
                words("got"), quote(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, TermContext, Pieces),
            MaybeOnlyFunctorArgs = error1([Spec])
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("local_args"), words("and"),
            quote("remote_args"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeOnlyFunctorArgs = error1([Spec])
    ).

:- pred parse_local_pos_sizes(uint::in, varset::in, list(term)::in,
    list(local_pos_size)::out, list(error_spec)::out) is det.

parse_local_pos_sizes(_, _, [], [], []).
parse_local_pos_sizes(MaxNumBits, VarSet, [Term | Terms],
        !:LocalPosSizes, !:Specs) :-
    parse_local_pos_sizes(MaxNumBits, VarSet, Terms,
        !:LocalPosSizes, !:Specs),
    parse_local_pos_size(MaxNumBits, VarSet, Term, MaybeLocalPosSize),
    (
        MaybeLocalPosSize = ok1(LocalPosSize),
        !:LocalPosSizes = [LocalPosSize | !.LocalPosSizes]
    ;
        MaybeLocalPosSize = error1(Specs),
        !:Specs = Specs ++ !.Specs
    ).

:- pred parse_arg_pos_sizes(uint::in, varset::in, list(term)::in,
    list(arg_pos_size)::out, list(error_spec)::out) is det.

parse_arg_pos_sizes(_, _, [], [], []).
parse_arg_pos_sizes(MaxNumBits, VarSet, [Term | Terms],
        !:ArgPosSizes, !:Specs) :-
    parse_arg_pos_sizes(MaxNumBits, VarSet, Terms,
        !:ArgPosSizes, !:Specs),
    parse_arg_pos_size(MaxNumBits, VarSet, Term, MaybeArgPosSize),
    (
        MaybeArgPosSize = ok1(ArgPosSize),
        !:ArgPosSizes = [ArgPosSize | !.ArgPosSizes]
    ;
        MaybeArgPosSize = error1(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%---------------------%

:- pred parse_strings(varset::in, list(format_component)::in, string::in,
    int::in, list(term)::in, list(string)::out, list(error_spec)::out) is det.

parse_strings(_, _, _, _, [], [], []).
parse_strings(VarSet, ContextPieces, Desc, Nth, [Term | Terms],
        !:Strs, !:Specs) :-
    parse_strings(VarSet, ContextPieces, Desc, Nth + 1, Terms,
        !:Strs, !:Specs),
    ( if Term = term.functor(term.string(Str), [], _) then
        !:Strs = [Str | !.Strs]
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: in the"), nth_fixed(Nth),
            words("element of the list in")] ++ ContextPieces ++
            [words("expected"), words(Desc), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred parse_string(varset::in, list(format_component)::in,
    string::in, term::in, maybe1(string)::out) is det.

parse_string(VarSet, ContextPieces, Desc, Term, MaybeFunctorName) :-
    ( if Term = term.functor(term.string(FunctorName), [], _) then
        MaybeFunctorName = ok1(FunctorName)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = ContextPieces ++ [lower_case_next_if_not_first,
            words("Error: expected"), words(Desc), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeFunctorName = error1([Spec])
    ).

%---------------------%

:- pred parse_du_functors(varset::in, list(term)::in,
    list(gen_du_functor)::out, list(error_spec)::out) is det.

parse_du_functors(_, [], [], []).
parse_du_functors(VarSet, [Term | Terms], !:DuFunctors, !:Specs) :-
    parse_du_functors(VarSet, Terms, !:DuFunctors, !:Specs),
    parse_du_functor(VarSet, Term, MaybeDuFunctor),
    (
        MaybeDuFunctor = ok1(DuFunctor),
        !:DuFunctors = [DuFunctor | !.DuFunctors]
    ;
        MaybeDuFunctor = error1(Specs),
        !:Specs = Specs ++ !.Specs
    ).

:- pred parse_du_functor(varset::in, term::in, maybe1(gen_du_functor)::out)
    is det.

parse_du_functor(VarSet, Term, MaybeDuFunctor) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "constant_functor"
        ; AtomStr = "nonconstant_functor"
        )
    then
        (
            AtomStr = "constant_functor",
            (
                ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3],
                ContextPieces = [words("In first argument of"),
                    quote("constant_functor"), suffix(":")],
                parse_string(VarSet, ContextPieces, "function symbol",
                    ArgTerm1, MaybeFunctorName),
                parse_sectag_size(63u, VarSet, ArgTerm2, MaybeSectagSize64),
                parse_sectag_size(31u, VarSet, ArgTerm3, MaybeSectagSize32),
                ( if
                    MaybeFunctorName = ok1(FunctorName),
                    MaybeSectagSize64 = ok1(SectagSize64),
                    MaybeSectagSize32 = ok1(SectagSize32)
                then
                    DuFunctor = gen_du_constant_functor(FunctorName,
                        SectagSize64, SectagSize32),
                    MaybeDuFunctor = ok1(DuFunctor)
                else
                    Specs = get_any_errors1(MaybeFunctorName) ++
                        get_any_errors1(MaybeSectagSize64) ++
                        get_any_errors1(MaybeSectagSize32),
                    MaybeDuFunctor = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _]
                ; ArgTerms = [_, _, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have exactly three arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeDuFunctor = error1([Spec])
            )
        ;
            AtomStr = "nonconstant_functor",
            (
                ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4, ArgTerm5],
                ContextPieces = [words("In first argument of"),
                    quote("nonconstant_functor"), suffix(":")],
                parse_string(VarSet, ContextPieces, "function symbol",
                    ArgTerm1, MaybeFunctorName),
                parse_ptag_sectag(7u, 63u, VarSet, ArgTerm2,
                    MaybePtagSectag64),
                parse_ptag_sectag(3u, 31u, VarSet, ArgTerm3,
                    MaybePtagSectag32),
                parse_maybe_direct_args(7u, 63u, VarSet, AtomStr, ArgTerm4,
                    MaybeMaybeDirectArgs64),
                parse_maybe_direct_args(3u, 31u, VarSet, AtomStr, ArgTerm5,
                    MaybeMaybeDirectArgs32),
                ( if
                    MaybeFunctorName = ok1(FunctorName),
                    MaybePtagSectag64 = ok1(PtagSectag64),
                    MaybePtagSectag32 = ok1(PtagSectag32),
                    MaybeMaybeDirectArgs64 = ok1(MaybeDirectArgs64),
                    MaybeMaybeDirectArgs32 = ok1(MaybeDirectArgs32)
                then
                    DuFunctor = gen_du_nonconstant_functor(FunctorName,
                        PtagSectag64, PtagSectag32,
                        MaybeDirectArgs64, MaybeDirectArgs32),
                    MaybeDuFunctor = ok1(DuFunctor)
                else
                    Specs = get_any_errors1(MaybeFunctorName) ++
                        get_any_errors1(MaybePtagSectag64) ++
                        get_any_errors1(MaybePtagSectag32) ++
                        get_any_errors1(MaybeMaybeDirectArgs64) ++
                        get_any_errors1(MaybeMaybeDirectArgs32),
                    MaybeDuFunctor = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _]
                ; ArgTerms = [_, _, _]
                ; ArgTerms = [_, _, _, _]
                ; ArgTerms = [_, _, _, _, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have exactly five arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeDuFunctor = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("constant_functor(...)"), words("and"),
            quote("nonconstant_functor(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeDuFunctor = error1([Spec])
    ).

:- pred parse_sectag_size(uint::in, varset::in, term::in,
    maybe1(sectag_size)::out) is det.

parse_sectag_size(MaxNumBits, VarSet, Term, MaybeSectagSize) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "sectag_rest_of_word"
        ; AtomStr = "sectag_bits"
        )
    then
        (
            AtomStr = "sectag_rest_of_word",
            (
                ArgTerms = [],
                MaybeSectagSize = ok1(sectag_rest_of_word)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have no argument."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeSectagSize = error1([Spec])
            )
        ;
            AtomStr = "sectag_bits",
            (
                ArgTerms = [ArgTerm1],
                parse_uint_in_range(MaxNumBits, VarSet, ArgTerm1,
                    MaybeSectagNumBits),
                (
                    MaybeSectagNumBits = ok1(SectagNumBits),
                    MaybeSectagSize = ok1(sectag_bits(SectagNumBits))
                ;
                    MaybeSectagNumBits = error1(Specs),
                    MaybeSectagSize = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have exactly one argument."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeSectagSize = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("sectag_rest_of_word"), words("and"),
            quote("sectag_bits(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeSectagSize = error1([Spec])
    ).

:- pred parse_ptag_sectag(uint::in, uint::in, varset::in, term::in,
    maybe1(ptag_sectag)::out) is det.

parse_ptag_sectag(MaxPtag, MaxNumBits, VarSet, Term, MaybePtagSectag) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "ptag_only"
        ; AtomStr = "ptag_local_sectag"
        ; AtomStr = "ptag_local_sectag_bits"
        ; AtomStr = "ptag_remote_sectag"
        ; AtomStr = "ptag_remote_sectag_bits"
        )
    then
        (
            AtomStr = "ptag_only",
            (
                ArgTerms = [ArgTerm1],
                parse_uint_in_range(MaxPtag, VarSet, ArgTerm1, MaybePtag),
                (
                    MaybePtag = ok1(Ptag),
                    MaybePtagSectag = ok1(ptag_sectag(Ptag, no_sectag))
                ;
                    MaybePtag = error1(Specs),
                    MaybePtagSectag = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have exactly one argument."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybePtagSectag = error1([Spec])
            )
        ;
            ( AtomStr = "ptag_local_sectag"
            ; AtomStr = "ptag_remote_sectag"
            ),
            (
                ArgTerms = [ArgTerm1, ArgTerm2],
                parse_uint_in_range(MaxPtag, VarSet, ArgTerm1, MaybePtag),
                parse_unlimited_uint(VarSet, ArgTerm2, MaybeSectag),
                ( if
                    MaybePtag = ok1(Ptag),
                    MaybeSectag = ok1(Sectag)
                then
                    (
                        AtomStr = "ptag_local_sectag",
                        MaybePtagSectag = ok1(ptag_sectag(Ptag,
                            local_sectag(Sectag, sectag_rest_of_word)))
                    ;
                        AtomStr = "ptag_remote_sectag",
                        MaybePtagSectag = ok1(ptag_sectag(Ptag,
                            remote_sectag(Sectag, sectag_rest_of_word)))
                    )
                else
                    Specs = get_any_errors1(MaybePtag) ++
                        get_any_errors1(MaybeSectag),
                    MaybePtagSectag = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have exactly two arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybePtagSectag = error1([Spec])
            )
        ;
            ( AtomStr = "ptag_local_sectag_bits"
            ; AtomStr = "ptag_remote_sectag_bits"
            ),
            (
                ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3],
                parse_uint_in_range(MaxPtag, VarSet, ArgTerm1, MaybePtag),
                parse_unlimited_uint(VarSet, ArgTerm2, MaybeSectag),
                parse_uint_in_range(MaxNumBits, VarSet, ArgTerm3,
                    MaybeSectagNumBits),
                ( if
                    MaybePtag = ok1(Ptag),
                    MaybeSectag = ok1(Sectag),
                    MaybeSectagNumBits = ok1(SectagNumBits)
                then
                    SectagSize = sectag_bits(SectagNumBits),
                    (
                        AtomStr = "ptag_local_sectag_bits",
                        MaybePtagSectag = ok1(ptag_sectag(Ptag,
                            local_sectag(Sectag, SectagSize)))
                    ;
                        AtomStr = "ptag_remote_sectag_bits",
                        MaybePtagSectag = ok1(ptag_sectag(Ptag,
                            remote_sectag(Sectag, SectagSize)))
                    )
                else
                    Specs = get_any_errors1(MaybePtag) ++
                        get_any_errors1(MaybeSectag) ++
                        get_any_errors1(MaybeSectagNumBits),
                    MaybePtagSectag = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _]
                ; ArgTerms = [_, _, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have exactly three arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybePtagSectag = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("ptag_only(...)"), suffix(","),
            quote("ptag_local_sectag(...)"), suffix(","),
            quote("ptag_local_sectag_bits(...)"), suffix(","),
            quote("ptag_remote_sectag(...)"), words("and"),
            quote("ptag_remote_sectag_bits(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybePtagSectag = error1([Spec])
    ).

%---------------------%

:- pred parse_local_pos_size(uint::in, varset::in, term::in,
    maybe1(local_pos_size)::out) is det.

parse_local_pos_size(MaxNumBits, VarSet, Term, MaybeLocalPosSize) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        AtomStr = "local"
    then
        (
            ArgTerms = [ArgTerm1, ArgTerm2],
            parse_uint_in_range(MaxNumBits, VarSet, ArgTerm1, MaybeShift),
            parse_fill_kind_size(MaxNumBits, VarSet, ArgTerm2,
                MaybeFillKindSize),
            ( if
                MaybeShift = ok1(Shift),
                MaybeFillKindSize = ok1(FillKindSize)
            then
                LocalPosSize = local_pos_size(Shift, FillKindSize),
                MaybeLocalPosSize = ok1(LocalPosSize)
            else
                Specs = get_any_errors1(MaybeShift) ++
                    get_any_errors1(MaybeFillKindSize),
                MaybeLocalPosSize = error1(Specs)
            )
        ;
            ( ArgTerms = []
            ; ArgTerms = [_]
            ; ArgTerms = [_, _, _ | _]
            ),
            Pieces = [words("Error:"), quote(AtomStr),
                words("should have exactly two arguments."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, TermContext, Pieces),
            MaybeLocalPosSize = error1([Spec])
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Expected"), quote("local(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeLocalPosSize = error1([Spec])
    ).

%---------------------%

:- pred parse_maybe_direct_args(uint::in, uint::in, varset::in, string::in,
    term::in, maybe1(one_or_more(maybe_direct_arg))::out) is det.

parse_maybe_direct_args(MaxPtag, MaxNumBits, VarSet, AtomStr, Term,
        MaybeMaybeDirectArgs) :-
    ( if
        list_term_to_term_list(Term, MaybeDirectArgTerms),
        MaybeDirectArgTerms = [HeadMaybeDirectArgTerm | TailMaybeDirectArgTerms]
    then
        parse_maybe_direct_arg(MaxPtag, MaxNumBits, VarSet,
            HeadMaybeDirectArgTerm, MaybeHeadMaybeDirectArg),
        parse_maybe_direct_args_loop(MaxPtag, MaxNumBits, VarSet,
            TailMaybeDirectArgTerms, TailMaybeDirectArgs, TailSpecs),
        ( if
            MaybeHeadMaybeDirectArg = ok1(HeadMaybeDirectArg),
            TailSpecs = []
        then
            MaybeMaybeDirectArgs = ok1(
                one_or_more(HeadMaybeDirectArg, TailMaybeDirectArgs))
        else
            Specs = get_any_errors1(MaybeHeadMaybeDirectArg) ++ TailSpecs,
            MaybeMaybeDirectArgs = error1(Specs)
        )
    else
        Pieces = [words("Error: the argument of"), quote(AtomStr),
            words("should be a nonempty list of"),
            words("argument representations."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeMaybeDirectArgs = error1([Spec])
    ).

:- pred parse_maybe_direct_args_loop(uint::in, uint::in, varset::in,
    list(term)::in, list(maybe_direct_arg)::out, list(error_spec)::out) is det.

parse_maybe_direct_args_loop(_, _, _, [], [], []).
parse_maybe_direct_args_loop(MaxPtag, MaxNumBits, VarSet, [Term | Terms],
        !:MaybeDirectArgs, !:Specs) :-
    parse_maybe_direct_args_loop(MaxPtag, MaxNumBits, VarSet, Terms,
        !:MaybeDirectArgs, !:Specs),
    parse_maybe_direct_arg(MaxPtag, MaxNumBits, VarSet, Term,
        MaybeMaybeDirectArg),
    (
        MaybeMaybeDirectArg = ok1(MaybeDirectArg),
        !:MaybeDirectArgs = [MaybeDirectArg | !.MaybeDirectArgs]
    ;
        MaybeMaybeDirectArg = error1(Specs),
        !:Specs = Specs ++ !.Specs
    ).

:- pred parse_maybe_direct_arg(uint::in, uint::in, varset::in, term::in,
    maybe1(maybe_direct_arg)::out) is det.

parse_maybe_direct_arg(MaxPtag, MaxNumBits, VarSet, Term,
        MaybeMaybeDirectArg) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "direct_arg"
        ; AtomStr = "nondirect_arg"
        )
    then
        (
            ArgTerms = [ArgTerm1],
            (
                AtomStr = "direct_arg",
                parse_uint_in_range(MaxPtag, VarSet, ArgTerm1, MaybePtag),
                (
                    MaybePtag = ok1(Ptag),
                    MaybeMaybeDirectArg = ok1(direct_arg(Ptag))
                ;
                    MaybePtag = error1(Specs),
                    MaybeMaybeDirectArg = error1(Specs)
                )
            ;
                AtomStr = "nondirect_arg",
                parse_arg_pos_size(MaxNumBits, VarSet, ArgTerm1,
                    MaybeArgPosSize),
                (
                    MaybeArgPosSize = ok1(ArgPosSize),
                    MaybeMaybeDirectArg = ok1(nondirect_arg(ArgPosSize))
                ;
                    MaybeArgPosSize = error1(Specs),
                    MaybeMaybeDirectArg = error1(Specs)
                )
            )
        ;
            ( ArgTerms = []
            ; ArgTerms = [_, _ | _]
            ),
            Pieces = [words("Error:"), quote(AtomStr),
                words("should have exactly one argument."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, TermContext, Pieces),
            MaybeMaybeDirectArg = error1([Spec])
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("direct_arg(...)"), words("and"),
            quote("nondirect_arg(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeMaybeDirectArg = error1([Spec])
    ).

%---------------------%

:- pred parse_arg_pos_size(uint::in, varset::in, term::in,
    maybe1(arg_pos_size)::out) is det.

parse_arg_pos_size(MaxNumBits, VarSet, Term, MaybeArgPosSize) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "full"
        ; AtomStr = "double"
        ; AtomStr = "partial_first"
        ; AtomStr = "partial_shifted"
        ; AtomStr = "none_shifted"
        ; AtomStr = "none_nowhere"
        )
    then
        (
            AtomStr = "full",
            parse_arg_pos_size_full_or_none(VarSet, AtomStr, ArgTerms,
                TermContext, MaybeArgPosSize)
        ;
            AtomStr = "double",
            parse_arg_pos_size_double(VarSet, AtomStr, ArgTerms, TermContext,
                MaybeArgPosSize)
        ;
            ( AtomStr = "partial_first"
            ; AtomStr = "partial_shifted"
            ),
            parse_arg_pos_size_partial(MaxNumBits, VarSet, AtomStr, ArgTerms,
                TermContext, MaybeArgPosSize)
        ;
            AtomStr = "none_shifted",
            parse_arg_pos_size_full_or_none(VarSet, AtomStr, ArgTerms,
                TermContext, MaybeArgPosSize)
        ;
            AtomStr = "none_nowhere",
            (
                ArgTerms = [],
                MaybeArgPosSize = ok1(pos_none_nowhere)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have no arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeArgPosSize = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Expected one of"),
            quote("full(...)"), suffix(","),
            quote("double(...)"), suffix(","),
            quote("partial_first(...)"), suffix(","),
            quote("partial_shifted(...)"), suffix(","),
            quote("none_shifted(...)"), words("and"),
            quote("none_nowhere"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeArgPosSize = error1([Spec])
    ).

:- inst full_none_kind for string/0
    --->    "full"
    ;       "none_shifted".

:- pred parse_arg_pos_size_full_or_none(varset::in,
    string::in(full_none_kind), list(term)::in, term.context::in,
    maybe1(arg_pos_size)::out) is det.

parse_arg_pos_size_full_or_none(VarSet, AtomStr, ArgTerms, TermContext,
        MaybeArgPosSize) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2],
        parse_arg_only_offset(VarSet, ArgTerm1, MaybeArgOnlyOffset),
        parse_cell_offset(VarSet, ArgTerm2, MaybeCellOffset),
        ( if
            MaybeArgOnlyOffset = ok1(ArgOnlyOffset),
            MaybeCellOffset = ok1(CellOffset)
        then
            (
                AtomStr = "full",
                ArgPosSize = pos_full(ArgOnlyOffset, CellOffset)
            ;
                AtomStr = "none_shifted",
                ArgPosSize = pos_none_shifted(ArgOnlyOffset, CellOffset)
            ),
            MaybeArgPosSize = ok1(ArgPosSize)
        else
            Specs = get_any_errors1(MaybeArgOnlyOffset) ++
                get_any_errors1(MaybeCellOffset),
            MaybeArgPosSize = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error:"), quote(AtomStr),
            words("should have exactly two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeArgPosSize = error1([Spec])
    ).

:- pred parse_arg_pos_size_double(varset::in, string::in, list(term)::in,
    term.context::in, maybe1(arg_pos_size)::out) is det.

parse_arg_pos_size_double(VarSet, AtomStr, ArgTerms, TermContext,
        MaybeArgPosSize) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3],
        parse_arg_only_offset(VarSet, ArgTerm1, MaybeArgOnlyOffset),
        parse_cell_offset(VarSet, ArgTerm2, MaybeCellOffset),
        ( if
            ArgTerm3 = term.functor(term.atom(Arg3Str), [], _),
            double_word_kind_string(DoubleWordKind0, Arg3Str)
        then
            MaybeDW = ok1(DoubleWordKind0)
        else
            DwPieces = [words("Error: the third argument of"), quote(AtomStr),
                words("must be one of"),
                quote("dw_float"), suffix(","),
                quote("dw_int64"), words("and"),
                quote("dw_uint64"), suffix("."), nl],
            DwSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(ArgTerm3), DwPieces),
            MaybeDW = error1([DwSpec])
        ),
        ( if
            MaybeArgOnlyOffset = ok1(ArgOnlyOffset),
            MaybeCellOffset = ok1(CellOffset),
            MaybeDW = ok1(DoubleWordKind)
        then
            ArgPosSize = pos_double(ArgOnlyOffset, CellOffset, DoubleWordKind),
            MaybeArgPosSize = ok1(ArgPosSize)
        else
            Specs = get_any_errors1(MaybeArgOnlyOffset) ++
                get_any_errors1(MaybeCellOffset),
            MaybeArgPosSize = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _]
        ; ArgTerms = [_, _, _, _ | _]
        ),
        Pieces = [words("Error:"), quote(AtomStr),
            words("should have exactly three arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeArgPosSize = error1([Spec])
    ).

:- inst partial_kind for string/0
    --->    "partial_first"
    ;       "partial_shifted".

:- pred parse_arg_pos_size_partial(uint::in, varset::in,
    string::in(partial_kind), list(term)::in, term.context::in,
    maybe1(arg_pos_size)::out) is det.

parse_arg_pos_size_partial(MaxNumBits, VarSet, AtomStr, ArgTerms, TermContext,
        MaybeArgPosSize) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4],
        parse_arg_only_offset(VarSet, ArgTerm1, MaybeArgOnlyOffset),
        parse_cell_offset(VarSet, ArgTerm2, MaybeCellOffset),
        parse_uint_in_range(MaxNumBits, VarSet, ArgTerm3, MaybeShift),
        parse_fill_kind_size(MaxNumBits, VarSet, ArgTerm4, MaybeFillKindSize),
        ( if
            MaybeArgOnlyOffset = ok1(ArgOnlyOffset),
            MaybeCellOffset = ok1(CellOffset),
            MaybeShift = ok1(Shift),
            MaybeFillKindSize = ok1(FillKindSize)
        then
            (
                AtomStr = "partial_first",
                ArgPosSize = pos_partial_first(ArgOnlyOffset, CellOffset,
                    Shift, FillKindSize)
            ;
                AtomStr = "partial_shifted",
                ArgPosSize = pos_partial_shifted(ArgOnlyOffset, CellOffset,
                    Shift, FillKindSize)
            ),
            MaybeArgPosSize = ok1(ArgPosSize)
        else
            Specs = get_any_errors1(MaybeArgOnlyOffset) ++
                get_any_errors1(MaybeCellOffset) ++
                get_any_errors1(MaybeShift) ++
                get_any_errors1(MaybeFillKindSize),
            MaybeArgPosSize = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _]
        ; ArgTerms = [_, _, _]
        ; ArgTerms = [_, _, _, _, _ | _]
        ),
        Pieces = [words("Error:"), quote(AtomStr),
            words("should have exactly four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeArgPosSize = error1([Spec])
    ).

:- pred parse_arg_only_offset(varset::in, term::in,
    maybe1(arg_only_offset)::out) is det.

parse_arg_only_offset(VarSet, Term, MaybeArgOnlyOffset) :-
    ( if
        Term = term.functor(term.integer(Base, N, Signedness, Size), [], _),
        Base = base_10,
        Signedness = signed,
        Size = size_word,
        integer.to_int(N, IntN),
        IntN >= 0
    then
        MaybeArgOnlyOffset = ok1(arg_only_offset(IntN))
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected nonnegative integer, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeArgOnlyOffset = error1([Spec])
    ).

:- pred parse_cell_offset(varset::in, term::in,
    maybe1(cell_offset)::out) is det.

parse_cell_offset(VarSet, Term, MaybeCellOffset) :-
    ( if
        Term = term.functor(term.integer(Base, N, Signedness, Size), [], _),
        Base = base_10,
        Signedness = signed,
        Size = size_word,
        integer.to_int(N, IntN),
        IntN >= 0
    then
        MaybeCellOffset = ok1(cell_offset(IntN))
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected nonnegative integer, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeCellOffset = error1([Spec])
    ).

:- pred parse_fill_kind_size(uint::in, varset::in, term::in,
    maybe1(fill_kind_size)::out) is det.

parse_fill_kind_size(MaxNumBits, VarSet, Term, MaybeFillKindSize) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        fill_kind_string(FillKind, AtomStr)
    then
        (
            FillKind = fill_enum,
            (
                ArgTerms = [ArgTerm],
                parse_uint_in_range(MaxNumBits, VarSet, ArgTerm, MaybeUint),
                (
                    MaybeUint = ok1(Uint),
                    MaybeFillKindSize = ok1(fk_enum(Uint))
                ;
                    MaybeUint = error1(Specs),
                    MaybeFillKindSize = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("must have exactly one argument."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeFillKindSize = error1([Spec])
            )
        ;
            FillKind = fill_int8,
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_int8,
                MaybeFillKindSize)
        ;
            FillKind = fill_int16,
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_int16,
                MaybeFillKindSize)
        ;
            FillKind = fill_int32,
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_int32,
                MaybeFillKindSize)
        ;
            FillKind = fill_uint8,
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_uint8,
                MaybeFillKindSize)
        ;
            FillKind = fill_uint16,
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_uint16,
                MaybeFillKindSize)
        ;
            FillKind = fill_uint32,
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_uint32,
                MaybeFillKindSize)
        ;
            FillKind = fill_char21,
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_char21,
                MaybeFillKindSize)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a fill kind and size, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeFillKindSize = error1([Spec])
    ).

:- pred ok_if_arity_zero(string::in, term.context::in, list(term)::in,
    fill_kind_size::in, maybe1(fill_kind_size)::out) is det.

ok_if_arity_zero(AtomStr, TermContext, ArgTerms, FillKindSize,
        MaybeFillKindSize) :-
    (
        ArgTerms = [],
        MaybeFillKindSize = ok1(FillKindSize)
    ;
        ArgTerms = [_ | _],
        Pieces = [words("Error:"), quote(AtomStr),
            words("must not have any one arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeFillKindSize = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_foreign_type(varset::in, string::in,
    list(term)::in, term.context::in, maybe1(type_ctor_repn_info)::out) is det.

parse_type_repn_foreign_type(VarSet, RepnStr, RepnArgs, RepnContext,
        MaybeRepn) :-
    (
        RepnArgs = [RepnArg1],
        DescPieces = [words("the argument of"), quote(RepnStr)],
        parse_c_j_cs_e_repn(VarSet, DescPieces, min_list_length_1,
            RepnArg1, MaybeCJCsERepn),
        (
            MaybeCJCsERepn = ok1(CJCsERepn),
            MaybeRepn = ok1(tcrepn_foreign(CJCsERepn))
        ;
            MaybeCJCsERepn = error1(Specs),
            MaybeRepn = error1(Specs)
        )
    ;
        ( RepnArgs = []
        ; RepnArgs = [_, _ | _]
        ),
        Pieces = [words("Error:"), quote(RepnStr),
            words("should have exactly one argument,"),
            words("which should be a nonempty list of foreign language names"),
            words("wrapped around type names and assertions."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            RepnContext, Pieces),
        MaybeRepn = error1([Spec])
    ).

:- type min_list_length
    --->    min_list_length_0
    ;       min_list_length_1.

:- pred parse_c_j_cs_e_repn(varset::in, list(format_component)::in,
    min_list_length::in, term::in, maybe1(c_j_cs_e_repn)::out) is det.

parse_c_j_cs_e_repn(VarSet, DescPieces, MinLength, Term, MaybeCJCsERepn) :-
    ( if list_term_to_term_list(Term, ForeignTerms) then
        parse_foreign_language_type_repns(VarSet, DescPieces, 1,
            ForeignTerms, ForeignPairs, ForeignSpecs),
        (
            MinLength = min_list_length_0,
            MinSpecs = []
        ;
            MinLength = min_list_length_1,
            (
                ForeignPairs = [_ | _],
                MinSpecs = []
            ;
                ForeignPairs = [],
                MinSpec = require_foreign_repn_list_spec(DescPieces,
                    [words("nonempty")], Term),
                MinSpecs = [MinSpec]
            )
        )
    else
        ForeignPairs = [],
        (
            MinLength = min_list_length_0,
            NonEmptyPieces = []
        ;
            MinLength = min_list_length_1,
            NonEmptyPieces = [words("nonempty")]
        ),
        ForeignSpec =
            require_foreign_repn_list_spec(DescPieces, NonEmptyPieces, Term),
        ForeignSpecs = [ForeignSpec],
        MinSpecs = []
    ),
    assoc_list_to_c_j_cs_e(get_term_context(Term), DescPieces, ForeignPairs,
        MaybeCJCsERepn0),
    ( if
        ForeignSpecs = [],
        MinSpecs = [],
        MaybeCJCsERepn0 = ok1(CJCsE)
    then
        MaybeCJCsERepn = ok1(CJCsE)
    else
        Specs = ForeignSpecs ++ MinSpecs ++ get_any_errors1(MaybeCJCsERepn0),
        MaybeCJCsERepn = error1(Specs)
    ).

%---------------------%

:- pred parse_c_j_cs_e_repn_or_enum(varset::in, list(format_component)::in,
    min_list_length::in, term::in, maybe1(c_j_cs_e_enum_repn)::out) is det.

parse_c_j_cs_e_repn_or_enum(VarSet, DescPieces, MinLength, Term,
        MaybeCJCsERepnOrEnum) :-
    ( if list_term_to_term_list(Term, ForeignTerms) then
        parse_foreign_language_type_repn_or_enums(VarSet, DescPieces, 1,
            ForeignTerms, ForeignPairs, ForeignSpecs),
        (
            MinLength = min_list_length_0,
            MinSpecs = []
        ;
            MinLength = min_list_length_1,
            (
                ForeignPairs = [_ | _],
                MinSpecs = []
            ;
                ForeignPairs = [],
                MinSpec = require_foreign_repn_or_enum_list_spec(DescPieces,
                    [words("nonempty")], Term),
                MinSpecs = [MinSpec]
            )
        )
    else
        ForeignPairs = [],
        (
            MinLength = min_list_length_0,
            NonEmptyPieces = []
        ;
            MinLength = min_list_length_1,
            NonEmptyPieces = [words("nonempty")]
        ),
        ForeignSpec = require_foreign_repn_or_enum_list_spec(DescPieces,
            NonEmptyPieces, Term),
        ForeignSpecs = [ForeignSpec],
        MinSpecs = []
    ),
    assoc_list_to_c_j_cs_e(get_term_context(Term), DescPieces, ForeignPairs,
        MaybeCJCsERepnOrEnum0),
    ( if
        ForeignSpecs = [],
        MinSpecs = [],
        MaybeCJCsERepnOrEnum0 = ok1(CJCsE)
    then
        MaybeCJCsERepnOrEnum = ok1(CJCsE)
    else
        Specs = ForeignSpecs ++ MinSpecs ++
            get_any_errors1(MaybeCJCsERepnOrEnum0),
        MaybeCJCsERepnOrEnum = error1(Specs)
    ).

%---------------------%

:- func require_foreign_repn_list_spec(list(format_component),
    list(format_component), term) = error_spec.

require_foreign_repn_list_spec(DescPieces, NonEmptyPieces, Term) = Spec :-
    Pieces = [words("Error:")] ++ DescPieces ++
        [words("should be a")] ++ NonEmptyPieces ++ [words("list"),
        words("of foreign_language names wrapped around"),
        words("foreign type names and assertions."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        get_term_context(Term), Pieces).

:- func require_foreign_repn_or_enum_list_spec(list(format_component),
    list(format_component), term) = error_spec.

require_foreign_repn_or_enum_list_spec(DescPieces, NonEmptyPieces, Term)
        = Spec :-
    Pieces = [words("Error:")] ++ DescPieces ++
        [words("should be a")] ++ NonEmptyPieces ++ [words("list"),
        words("of foreign_language names wrapped around"),
        words("foreign type names and assertions."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        get_term_context(Term), Pieces).

%---------------------%

:- pred parse_foreign_language_type_repns(varset::in,
    list(format_component)::in, int::in,
    list(term)::in, assoc_list(foreign_language, foreign_type_repn)::out,
    list(error_spec)::out) is det.

parse_foreign_language_type_repns(_, _, _, [], [], []).
parse_foreign_language_type_repns(VarSet, BasePieces, ElementNum,
        [Term | Terms], !:ForeignPairs, !:Specs) :-
    parse_foreign_language_type_repns(VarSet, BasePieces, ElementNum + 1,
        Terms, !:ForeignPairs, !:Specs),
    ContextPiecesFunc = ((func) = nth_element_of(BasePieces, ElementNum)),
    parse_foreign_language_type_repn(VarSet, ContextPiecesFunc, Term,
        MaybeForeignLangRepn),
    (
        MaybeForeignLangRepn = ok2(Lang, Repn),
        !:ForeignPairs = [Lang - Repn | !.ForeignPairs]
    ;
        MaybeForeignLangRepn = error2(Specs),
        !:Specs = Specs ++ !.Specs
    ).

:- pred parse_foreign_language_type_repn_or_enums(varset::in,
    list(format_component)::in, int::in,
    list(term)::in, assoc_list(foreign_language, enum_foreign_repn)::out,
    list(error_spec)::out) is det.

parse_foreign_language_type_repn_or_enums(_, _, _, [], [], []).
parse_foreign_language_type_repn_or_enums(VarSet, BasePieces, ElementNum,
        [Term | Terms], !:ForeignPairs, !:Specs) :-
    parse_foreign_language_type_repn_or_enums(VarSet, BasePieces,
        ElementNum + 1, Terms, !:ForeignPairs, !:Specs),
    ContextPiecesFunc = ((func) = nth_element_of(BasePieces, ElementNum)),
    parse_foreign_language_type_or_enum_repn(VarSet, ContextPiecesFunc, Term,
        MaybeForeignLangRepnOrEnum),
    (
        MaybeForeignLangRepnOrEnum = ok2(Lang, RepnOrEnum),
        !:ForeignPairs = [Lang - RepnOrEnum | !.ForeignPairs]
    ;
        MaybeForeignLangRepnOrEnum = error2(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%---------------------%

:- pred parse_foreign_language_type_or_enum_repn(varset::in,
    ((func) = list(format_component))::in,
    term::in, maybe2(foreign_language, enum_foreign_repn)::out) is det.

parse_foreign_language_type_or_enum_repn(VarSet, ContextPiecesFunc, Term,
        MaybeForeignLangRepnOrEnum) :-
    ( if
        Term = term.functor(term.atom(FunctorStr), ArgTerms, TermContext),
        ( FunctorStr = "foreign_type"
        ; FunctorStr = "foreign_enum"
        )
    then
        (
            FunctorStr = "foreign_type",
            (
                ArgTerms = [ArgTerm1],
                parse_foreign_language_type_repn(VarSet, ContextPiecesFunc,
                    ArgTerm1, MaybeForeignLangRepn),
                (
                    MaybeForeignLangRepn = ok2(Lang, ForeignRepn),
                    MaybeForeignLangRepnOrEnum =
                        ok2(Lang, enum_foreign_type(ForeignRepn))
                ;
                    MaybeForeignLangRepn = error2(Specs),
                    MaybeForeignLangRepnOrEnum = error2(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Pieces = [words("In") | apply(ContextPiecesFunc)] ++
                    [suffix(":"), words("error:"),
                    quote("foreign_type"), words("should have"),
                    words("exactly one argument, which should be"),
                    words("a foreign_language name wrapped around"),
                    words("a foreign type name and a list of assertions."),
                    nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeForeignLangRepnOrEnum = error2([Spec])
            )
        ;
            FunctorStr = "foreign_enum",
            (
                ArgTerms = [ArgTerm1, ArgTerm2],
                ( if
                    ArgTerm1 = term.functor(term.atom(LangStr), [], _),
                    ( LangStr = "c", Lang0 = lang_c
                    ; LangStr = "csharp", Lang0 = lang_csharp
                    ; LangStr = "java", Lang0 = lang_java
                    ; LangStr = "erlang", Lang0 = lang_erlang
                    )
                then
                    MaybeLang = ok1(Lang0)
                else
                    ArgTermStr1 = describe_error_term(VarSet, ArgTerm1),
                    Pieces = [words("In") | apply(ContextPiecesFunc)] ++
                        [suffix(":"), words("error: in the first argument of"),
                        quote("foreign_enum"), suffix(","),
                        words("expected one of"),
                        words("one of"), quote("c"), suffix(","),
                        quote("csharp"), suffix(","), quote("java"),
                        words("and"), quote("erlang"), suffix(";"),
                        words("got"), quote(ArgTermStr1), suffix("."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(ArgTerm1), Pieces),
                    MaybeLang = error1([Spec])
                ),
                parse_one_or_more_strings(VarSet, apply(ContextPiecesFunc),
                    "enum values", ArgTerm2, MaybeOoMStrings),
                ( if
                    MaybeLang = ok1(Lang),
                    MaybeOoMStrings = ok1(OoMStrings)
                then
                    MaybeForeignLangRepnOrEnum =
                        ok2(Lang, enum_foreign_enum(OoMStrings))
                else
                    Specs =
                        get_any_errors1(MaybeLang) ++
                        get_any_errors1(MaybeOoMStrings),
                    MaybeForeignLangRepnOrEnum = error2(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _, _ | _]
                ),
                Pieces = [words("In") | apply(ContextPiecesFunc)] ++
                    [suffix(":"), words("error:"),
                    quote("foreign_enum"), words("should have"),
                    words("exactly two arguments,"),
                    words("the name of a foreign language and"),
                    words("a list of the names of the enum values"),
                    words("in that language."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeForeignLangRepnOrEnum = error2([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("In") | apply(ContextPiecesFunc)] ++
            [suffix(":"), words("error: expected either"),
            quote("foreign_type(...)"), words("or"),
            quote("foreign_num(..., ...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeForeignLangRepnOrEnum = error2([Spec])
    ).

%---------------------%

:- pred parse_foreign_language_type_repn(varset::in,
    ((func) = list(format_component))::in,
    term::in, maybe2(foreign_language, foreign_type_repn)::out) is det.

parse_foreign_language_type_repn(VarSet, ContextPiecesFunc, Term,
        MaybeForeignLangRepn) :-
    ( if
        Term = term.functor(term.atom(FunctorStr), ArgTerms, _),
        ( FunctorStr = "c", Lang = lang_c
        ; FunctorStr = "csharp", Lang = lang_csharp
        ; FunctorStr = "java", Lang = lang_java
        ; FunctorStr = "erlang", Lang = lang_erlang
        ),
        ArgTerms = [TypeNameTerm, AssertionTerm]
    then
        parse_foreign_type_repn(VarSet, ContextPiecesFunc, Lang,
            TypeNameTerm, AssertionTerm, MaybeForeignLangRepn)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error:")] ++ apply(ContextPiecesFunc) ++
            [words("is"), quote(TermStr), suffix(","),
            words("which matches none of the permitted forms."),
            words("The permitted forms are"), nl_indent_delta(1),
            quote("c(type_name, assertions_list)"), suffix(","), nl,
            quote("java(type_name, assertions_list)"), suffix(","), nl,
            quote("csharp(type_name, assertions_list)"), words("and"), nl,
            quote("erlang("", assertions_list)"), suffix("."),
                nl_indent_delta(-1)],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeForeignLangRepn = error2([Spec])
    ).

:- pred parse_foreign_type_repn(varset::in,
    ((func) = list(format_component))::in,
    foreign_language::in, term::in, term::in,
    maybe2(foreign_language, foreign_type_repn)::out) is det.

parse_foreign_type_repn(VarSet, ContextPiecesFunc, Lang,
        TypeNameTerm, AssertionTerm, MaybeForeignLangRepn) :-
    ( if
        TypeNameTerm = term.functor(term.string(TypeNameStr), [],
            TypeNameContext)
    then
        (
            ( Lang = lang_c
            ; Lang = lang_csharp
            ; Lang = lang_java
            ),
            MaybeTypeName = ok1(TypeNameStr)
        ;
            Lang = lang_erlang,
            ( if TypeNameStr = "" then
                MaybeTypeName = ok1(TypeNameStr)
            else
                TypeNameTermStr = describe_error_term(VarSet, TypeNameTerm),
                ErlangPieces = [words("Error: expected the empty string"),
                    words("for the Erlang type name, got"),
                    quote(TypeNameTermStr), suffix("."), nl],
                ErlangSpec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TypeNameContext, ErlangPieces),
                MaybeTypeName = error1([ErlangSpec])
            )
        )
    else
        TypeNameTermStr = describe_error_term(VarSet, TypeNameTerm),
        TypeNamePieces = [words("Error: the type name in")] ++
            apply(ContextPiecesFunc) ++
            [words("is"), quote(TypeNameTermStr), suffix(","),
            words("which is not a string"),
            words("and therefore not valid type name."), nl],
        TypeNameSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree,
            get_term_context(TypeNameTerm), TypeNamePieces),
        MaybeTypeName = error1([TypeNameSpec])
    ),
    AssertionContextPieces = cord.from_list([
        words("In the second argument of")] ++ apply(ContextPiecesFunc) ++
        [suffix(":")]),
    parse_foreign_type_assertions(AssertionContextPieces, VarSet,
        AssertionTerm, set.init, AssertionSet, [], AssertionSpecs),
    ( if
        MaybeTypeName = ok1(TypeName),
        AssertionSpecs = []
    then
        Assertions = foreign_type_assertions(AssertionSet),
        Repn = foreign_type_repn(TypeName, Assertions),
        MaybeForeignLangRepn = ok2(Lang, Repn)
    else
        Specs = get_any_errors1(MaybeTypeName) ++ AssertionSpecs,
        MaybeForeignLangRepn = error2(Specs)
    ).

:- func nth_element_of(list(format_component), int) = list(format_component).

nth_element_of(BasePieces, Nth) =
    [words("the"), nth_fixed(Nth), words("element of the list")] ++ BasePieces.

%---------------------%

:- pred parse_one_or_more_strings(varset::in, list(format_component)::in,
    string::in, term::in, maybe1(one_or_more(string))::out) is det.

parse_one_or_more_strings(VarSet, ContextPieces, Desc, Term,
        MaybeOoMStrings) :-
    ( if list_term_to_term_list(Term, StringTerms) then
        parse_strings(VarSet, ContextPieces, Desc, 1, StringTerms,
            Strings, StringSpecs),
        (
            Strings = [],
            (
                StringTerms = [],
                expect(unify(StringSpecs, []), $pred,
                    "StringTerms = [] but StringSpecs != []"),
                Pieces = [words("In") | ContextPieces] ++ [suffix(":"),
                    words("error: expected a nonempty list of strings,"),
                    words("got an empty list."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Term), Pieces),
                MaybeOoMStrings = error1([Spec])
            ;
                StringTerms = [_ | _],
                % None of the terms in StringTerms were actually strings,
                % which means all of them must have caused an error message
                % to be generated. This means StringSpecs cannot be empty.
                expect_not(unify(StringSpecs, []), $pred,
                    "StringTerms != [] and Strings = [] but StringSpecs = []"),
                MaybeOoMStrings = error1(StringSpecs)
            )
        ;
            Strings = [HeadString  | TailStrings],
            (
                StringSpecs = [],
                MaybeOoMStrings = ok1(one_or_more(HeadString, TailStrings))
            ;
                StringSpecs = [_ | _],
                MaybeOoMStrings = error1(StringSpecs)
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("In") | ContextPieces] ++ [suffix(":"),
            words("error: expected a list of strings, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeOoMStrings = error1([Spec])
    ).

%---------------------%

:- pred assoc_list_to_c_j_cs_e(prog_context::in, list(format_component)::in,
    assoc_list(foreign_language, T)::in,
    maybe1(c_java_csharp_erlang(maybe(T)))::out) is det.

assoc_list_to_c_j_cs_e(TermContext, Desc, !.Pairs, MaybeCJCsE) :-
    ( if !.Pairs = [lang_c - ValueC | !:Pairs] then
        MaybeValueC = yes(ValueC)
    else
        MaybeValueC = no
    ),
    ( if !.Pairs = [lang_java - ValueJava | !:Pairs] then
        MaybeValueJava = yes(ValueJava)
    else
        MaybeValueJava = no
    ),
    ( if !.Pairs = [lang_csharp - ValueCsharp | !:Pairs] then
        MaybeValueCsharp = yes(ValueCsharp)
    else
        MaybeValueCsharp = no
    ),
    ( if !.Pairs = [lang_erlang - ValueErlang | !:Pairs] then
        MaybeValueErlang = yes(ValueErlang)
    else
        MaybeValueErlang = no
    ),
    (
        !.Pairs = [],
        CJCsE = c_java_csharp_erlang(MaybeValueC, MaybeValueJava,
            MaybeValueCsharp, MaybeValueErlang),
        MaybeCJCsE = ok1(CJCsE)
    ;
        !.Pairs = [_ | _],
        Pieces = [words("Error: the list of foreign languages in")] ++ Desc ++
            [words("does not follow the required order, which is"),
            words("first c, then java, then csharp, then erlang."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeCJCsE = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_unlimited_uint(varset::in, term::in,
    maybe1(uint)::out) is det.

parse_unlimited_uint(VarSet, Term, MaybeUint) :-
    ( if
        Term = term.functor(term.integer(Base, N, Signedness, Size), [], _),
        Base = base_10,
        Signedness = signed,
        Size = size_word,
        integer.to_int(N, IntN),
        uint.from_int(IntN, UintN)
    then
        MaybeUint = ok1(UintN)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected nonnegative integer,"),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeUint = error1([Spec])
    ).

:- pred parse_uint_in_range(uint::in, varset::in, term::in,
    maybe1(uint)::out) is det.

parse_uint_in_range(Max, VarSet, Term, MaybeUint) :-
    ( if
        Term = term.functor(term.integer(Base, N, Signedness, Size), [], _),
        Base = base_10,
        Signedness = signed,
        Size = size_word,
        integer.to_int(N, IntN),
        uint.from_int(IntN, UintN),
        UintN =< Max
    then
        MaybeUint = ok1(UintN)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected integer between 0 and"),
            int_fixed(uint.cast_to_int(Max)),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeUint = error1([Spec])
    ).

%-----------------------------------------------------------------------------e
:- end_module parse_tree.parse_type_repn.
%-----------------------------------------------------------------------------e
