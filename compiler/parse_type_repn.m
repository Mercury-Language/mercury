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
:- import_module libs.globals.
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
:- import_module uint8.

%---------------------------------------------------------------------------%

parse_type_repn_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if ArgTerms = [TypeTerm, RepnTerm] then
        TypeContextPieces = cord.from_list([words("In the first argument of"),
            quote("type_representation"), words("item:")]),
        parse_type_defn_head(TypeContextPieces, root_module_name, VarSet,
            TypeTerm, MaybeTypeSymNameAndArgs),
        ( if
            RepnTerm = term.functor(term.atom(AtomStr), RepnArgs, RepnContext),
            ( AtomStr = "is_eqv_to"
            ; AtomStr = "is_word_aligned_ptr"
            ; AtomStr = "du_repn"
            ; AtomStr = "foreign_type_repn"
            )
        then
            (
                AtomStr = "is_eqv_to",
                parse_type_repn_eqv_to(VarSet, AtomStr, RepnArgs,
                    RepnContext, MaybeRepn)
            ;
                AtomStr = "is_word_aligned_ptr",
                parse_no_arg_type_repn(AtomStr, RepnArgs, RepnContext,
                    tcrepn_is_word_aligned_ptr, MaybeRepn)
            ;
                AtomStr = "du_repn",
                (
                    RepnArgs = [RepnArg],
                    parse_type_repn_du(VarSet, RepnArg, MaybeDuRepn),
                    (
                        MaybeDuRepn = ok1(DuRepn),
                        MaybeRepn = ok1(tcrepn_du(DuRepn))
                    ;
                        MaybeDuRepn = error1(DuRepnSpecs),
                        MaybeRepn = error1(DuRepnSpecs)
                    )
                ;
                    ( RepnArgs = []
                    ; RepnArgs = [_, _ | _]
                    ),
                    DuPieces = [words("Error: in second argument of a"),
                        decl("type_representation"), words("item:"),
                        quote(AtomStr), words("should have"),
                        words("one argument."), nl],
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
            MaybeTypeSymNameAndArgs = ok2(TypeCtorSymName0, TypeArgVars),
            MaybeRepn = ok1(Repn)
        then
            varset.coerce(VarSet, TVarSet),
            TypeCtorName = unqualify_name(TypeCtorSymName0),
            TypeCtorSymName = qualified(ModuleName, TypeCtorName),
            ItemRepnInfo = item_type_repn_info(TypeCtorSymName, TypeArgVars,
                Repn, TVarSet, Context, SeqNum),
            Item = item_type_repn(ItemRepnInfo),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs =
                get_any_errors2(MaybeTypeSymNameAndArgs) ++
                get_any_errors1(MaybeRepn),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"),
            decl("type_representation"), words("item"),
            words("should have two arguments: the type,"),
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
            words("should have zero arguments."), nl],
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
            words("should have one argument, a type."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            RepnContext, Pieces),
        MaybeRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_du(varset::in, term::in, maybe1(du_repn)::out) is det.

parse_type_repn_du(VarSet, Term, MaybeDuRepn) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "direct_dummy"
        ; AtomStr = "enum"
        ; AtomStr = "notag"
        ; AtomStr = "gen_du_more_functors"
        ; AtomStr = "gen_du_only_functor"
        )
    then
        (
            AtomStr = "direct_dummy",
            parse_type_repn_du_direct_dummy(VarSet, TermContext, ArgTerms,
                MaybeDuRepn)
        ;
            AtomStr = "enum",
            parse_type_repn_du_enum(VarSet, TermContext, ArgTerms, MaybeDuRepn)
        ;
            AtomStr = "notag",
            parse_type_repn_du_notag(VarSet, TermContext, ArgTerms,
                MaybeDuRepn)
        ;
            AtomStr = "gen_du_only_functor",
            parse_type_repn_du_gen_du_only_functor(VarSet, TermContext,
                AtomStr, ArgTerms, MaybeDuRepn)
        ;
            AtomStr = "gen_du_more_functors",
            parse_type_repn_du_gen_du_more_functors(VarSet, TermContext,
                AtomStr, ArgTerms, MaybeDuRepn)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("direct_dummy(...)"), suffix(","),
            quote("enum(...)"), suffix(","),
            quote("notag(...)"), suffix(","),
            quote("gen_du_only_functor(...)"),  words("and"),
            quote("gen_du_more_functors(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeDuRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

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
        parse_c_j_cs_e_repn_or_enum(DescPieces2, VarSet, ArgTerm2,
            MaybeCJCsERepnOrEnum),
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
        Pieces = [words("Error:"), quote("direct_dummy"),
            words("should have two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeDuRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

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
            parse_strings(ContextPieces, 1, VarSet,
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
        parse_c_j_cs_e_repn_or_enum(DescPieces4, VarSet, ArgTerm4,
            MaybeCJCsERepnOrEnum),
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
        Pieces = [words("Error:"), quote("enum"),
            words("should have four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeDuRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_du_notag(varset::in, prog_context::in, list(term)::in,
    maybe1(du_repn)::out) is det.

parse_type_repn_du_notag(VarSet, TermContext, ArgTerms, MaybeDuRepn) :-
    (
        % We used to use output notag_repns that did not specify
        % the argument type. Accept such type_repn items, with a
        % dummy value in place of the missing information. Code using
        % such dummy values won't compute correct answers, but this ok,
        % because for now, the only thing we use the argument type for
        % is computing a description of the representations of complex types
        % to put into .int files, and those descriptions are themselves
        % ignored.
        %
        % XXX Before we can start using those descriptions, we will
        % have to insist on ArgTerm2 being present.
        (
            ArgTerms = [ArgTerm1, ArgTerm3],
            ArgTerm2 = term.functor(atom("void"), [], dummy_context_init)
        ;
            ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3]
        ),
        ContextPieces1 = [words("In first argument of"),
            quote("notag"), suffix(":"), nl],
        parse_string(VarSet, ContextPieces1, "function symbol",
            ArgTerm1, MaybeFunctorName),
        ContextPieces2 = cord.from_list([words("In second argument of"),
            quote("notag"), suffix(":"), nl]),
        parse_type(allow_ho_inst_info, VarSet, ContextPieces2,
            ArgTerm2, MaybeArgType),
        DescPieces3 = [words("the third argument of"), quote("notag")],
        parse_c_j_cs_e_repn(DescPieces3, VarSet, ArgTerm3, MaybeCJCsERepn),
        ( if
            MaybeFunctorName = ok1(FunctorName),
            MaybeArgType = ok1(ArgType),
            MaybeCJCsERepn = ok1(CJCsERepn)
        then
            NotagRepn = notag_repn(FunctorName, ArgType, CJCsERepn),
            DuRepn = dur_notag(NotagRepn),
            MaybeDuRepn = ok1(DuRepn)
        else
            Specs =
                get_any_errors1(MaybeFunctorName) ++
                get_any_errors1(MaybeArgType) ++
                get_any_errors1(MaybeCJCsERepn),
            MaybeDuRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _, _ | _]
        ),
        Pieces = [words("Error:"), quote("notag"),
            words("should have three arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeDuRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_du_gen_du_only_functor(varset::in, prog_context::in,
    string::in, list(term)::in, maybe1(du_repn)::out) is det.

parse_type_repn_du_gen_du_only_functor(VarSet, TermContext, AtomStr, ArgTerms,
        MaybeDuRepn) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4],
        ContextPieces1 = [words("In first argument of"),
            quote(AtomStr), suffix(":")],
        parse_string(VarSet, ContextPieces1, "function symbol",
            ArgTerm1, MaybeFunctorName),
        ( if list_term_to_term_list(ArgTerm2, TypeTerms) then
            ContextPieces2 = cord.from_list([words("In the"),
                words("second argument of"), quote(AtomStr), suffix(":")]),
            parse_types(allow_ho_inst_info, VarSet, ContextPieces2,
                TypeTerms, MaybeArgTypes)
        else
            ArgTypePieces = [words("Error: the second argument of"),
                quote(AtomStr), words("should be a list of types."), nl],
            ArgTypeSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(ArgTerm2),
                ArgTypePieces),
            MaybeArgTypes = error1([ArgTypeSpec])
        ),
        DescPieces3 = [words("third argument of"),
            quote(AtomStr)],
        parse_c_repns(DescPieces3, parse_nonconstant_repn, VarSet,
            ArgTerm3, MaybeCRepns),
        DescPieces4 = [words("fourth argument of"),
            quote(AtomStr)],
        parse_c_j_cs_e_repn(DescPieces4, VarSet, ArgTerm4, MaybeCJCsERepn),
        ( if
            MaybeFunctorName = ok1(FunctorName),
            MaybeCRepns = ok1(CRepns),
            MaybeArgTypes = ok1(ArgTypes),
            MaybeCJCsERepn = ok1(CJCsERepn)
        then
            OnlyFunctor = gen_du_only_functor_repn(FunctorName, ArgTypes,
                CRepns, CJCsERepn),
            MaybeDuRepn = ok1(dur_gen_only_functor(OnlyFunctor))
        else
            Specs =
                get_any_errors1(MaybeFunctorName) ++
                get_any_errors1(MaybeArgTypes) ++
                get_any_errors1(MaybeCRepns) ++
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
            words("should have four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeDuRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_du_gen_du_more_functors(varset::in, prog_context::in,
    string::in, list(term)::in, maybe1(du_repn)::out) is det.

parse_type_repn_du_gen_du_more_functors(VarSet, TermContext, AtomStr, ArgTerms,
        MaybeDuRepn) :-
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
            Pieces = [words("Error: the third argument of"), quote(AtomStr),
                words("should be a list of function symbol representations."),
                nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(ArgTerm3), Pieces),
            MaybeOtherFunctors = error1([Spec])
        ),
        DescPieces4 = [words("fourth argument of"),
            quote(AtomStr)],
        parse_c_j_cs_e_repn(DescPieces4, VarSet, ArgTerm4, MaybeCJCsERepn),
        ( if
            MaybeFunctor1 = ok1(Functor1),
            MaybeFunctor2 = ok1(Functor2),
            MaybeOtherFunctors = ok1(OtherFunctors),
            MaybeCJCsERepn = ok1(CJCsERepn)
        then
            MoreFunctors = gen_du_more_functors_repn(Functor1, Functor2,
                OtherFunctors, CJCsERepn),
            MaybeDuRepn = ok1(dur_gen_more_functors(MoreFunctors))
        else
            Specs =
                get_any_errors1(MaybeFunctor1) ++
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
        Pieces = [words("Error:"), quote(AtomStr),
            words("should have four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeDuRepn = error1([Spec])
    ).

%---------------------%

:- pred parse_du_functors(varset::in, list(term)::in,
    list(gen_du_functor_repn)::out, list(error_spec)::out) is det.

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

:- pred parse_du_functor(varset::in, term::in,
    maybe1(gen_du_functor_repn)::out) is det.

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
                ArgTerms = [ArgTerm1, ArgTerm2],
                ContextPieces1 = [words("In first argument of"),
                    quote("constant_functor"), suffix(":")],
                parse_string(VarSet, ContextPieces1, "function symbol",
                    ArgTerm1, MaybeFunctorName),
                DescPieces2 = [words("second argument of"),
                    quote("constant_functor")],
                parse_c_repns(DescPieces2, parse_constant_repn,
                    VarSet, ArgTerm2, MaybeConstantCRepns),
                ( if
                    MaybeFunctorName = ok1(FunctorName),
                    MaybeConstantCRepns = ok1(ConstantCRepns)
                then
                    DuFunctor = gen_du_constant_functor_repn(FunctorName,
                        ConstantCRepns),
                    MaybeDuFunctor = ok1(DuFunctor)
                else
                    Specs =
                        get_any_errors1(MaybeFunctorName) ++
                        get_any_errors1(MaybeConstantCRepns),
                    MaybeDuFunctor = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have two arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeDuFunctor = error1([Spec])
            )
        ;
            AtomStr = "nonconstant_functor",
            (
                ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3],
                ContextPieces = [words("In first argument of"),
                    quote(AtomStr), suffix(":")],
                parse_string(VarSet, ContextPieces, "function symbol",
                    ArgTerm1, MaybeFunctorName),
                ( if list_term_to_term_list(ArgTerm2, TypeTerms) then
                    ContextPieces2 = cord.from_list([words("In the"),
                        words("second argument of"), quote(AtomStr),
                        suffix(":")]),
                    parse_types(allow_ho_inst_info, VarSet, ContextPieces2,
                        TypeTerms, MaybeArgTypes)
                else
                    ArgTypePieces = [words("Error: the second argument of"),
                        quote(AtomStr),
                        words("should be a list of types."), nl],
                    ArgTypeSpec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree, get_term_context(ArgTerm2),
                        ArgTypePieces),
                    MaybeArgTypes = error1([ArgTypeSpec])
                ),
                DescPieces3 = [words("third argument of"), quote(AtomStr)],
                parse_c_repns(DescPieces3, parse_nonconstant_repn,
                    VarSet, ArgTerm3, MaybeNonConstantCRepns),
                ( if
                    MaybeFunctorName = ok1(FunctorName),
                    MaybeArgTypes = ok1(ArgTypes),
                    MaybeNonConstantCRepns = ok1(NonConstantCRepns)
                then
                    DuFunctor = gen_du_nonconstant_functor_repn(FunctorName,
                        ArgTypes, NonConstantCRepns),
                    MaybeDuFunctor = ok1(DuFunctor)
                else
                    Specs =
                        get_any_errors1(MaybeFunctorName) ++
                        get_any_errors1(MaybeArgTypes) ++
                        get_any_errors1(MaybeNonConstantCRepns),
                    MaybeDuFunctor = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _]
                ; ArgTerms = [_, _, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have three arguments."), nl],
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

:- pred parse_constant_repn(varset::in, term::in,
    maybe1(constant_repn)::out) is det.

parse_constant_repn(VarSet, Term, MaybeConstantRepn) :-
    ( if Term = term.functor(term.atom("constant"), ArgTerms, TermContext) then
        (
            ArgTerms = [ArgTerm1, ArgTerm2],
            parse_unlimited_uint(VarSet, ArgTerm1, MaybeSectag),
            parse_sectag_word_or_size(VarSet, ArgTerm2, MaybeSectagSize),
            ( if
                MaybeSectag = ok1(Sectag),
                MaybeSectagSize = ok1(SectagSize)
            then
                ConstantRepn = constant_repn(Sectag, SectagSize),
                MaybeConstantRepn = ok1(ConstantRepn)
            else
                Specs =
                    get_any_errors1(MaybeSectag) ++
                    get_any_errors1(MaybeSectagSize),
                MaybeConstantRepn = error1(Specs)
            )
        ;
            ( ArgTerms = []
            ; ArgTerms = [_]
            ; ArgTerms = [_, _, _ | _]
            ),
            Pieces = [words("Error:"), quote("constant"),
                words("should have two arguments."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, TermContext, Pieces),
            MaybeConstantRepn = error1([Spec])
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected"),
            quote("constant(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeConstantRepn = error1([Spec])
    ).

:- pred parse_nonconstant_repn(varset::in, term::in,
    maybe1(nonconstant_repn)::out) is det.

parse_nonconstant_repn(VarSet, Term, MaybeNonConstantRepn) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "local_cell"
        ; AtomStr = "remote_cell"
        ; AtomStr = "direct_arg"
        )
    then
        (
            AtomStr = "local_cell",
            (
                ArgTerms = [ArgTerm1, ArgTerm2],
                parse_local_sectag(VarSet, ArgTerm1, MaybeLocalSectag),
                ( if
                    list_term_to_term_list(ArgTerm2, ElementTerms),
                    ElementTerms = [HeadElementTerm | TailElementTerms]
                then
                    parse_local_arg_repn(VarSet, HeadElementTerm,
                        MaybeHeadLocalArg),
                    parse_local_arg_repns(VarSet, TailElementTerms,
                        TailLocalArgs, TailLocalArgSpecs),
                    ( if
                        MaybeLocalSectag = ok1(LocalSectag),
                        MaybeHeadLocalArg = ok1(HeadLocalArg),
                        TailLocalArgSpecs = []
                    then
                        OoMLocalArgs =
                            one_or_more(HeadLocalArg, TailLocalArgs),
                        NonConstantRepn =
                            ncr_local_cell(LocalSectag, OoMLocalArgs),
                        MaybeNonConstantRepn = ok1(NonConstantRepn)
                    else
                        Specs =
                            get_any_errors1(MaybeLocalSectag) ++
                            get_any_errors1(MaybeHeadLocalArg) ++
                            TailLocalArgSpecs,
                        MaybeNonConstantRepn = error1(Specs)
                    )
                else
                    Pieces = [words("Error: the second argument of"),
                        quote(AtomStr), words("should be a nonempty list"),
                        words("of local cell argument representations."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree, TermContext, Pieces),
                    Specs = [Spec | get_any_errors1(MaybeLocalSectag)],
                    MaybeNonConstantRepn = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have two arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeNonConstantRepn = error1([Spec])
            )
        ;
            AtomStr = "remote_cell",
            (
                ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3],
                parse_ptag(VarSet, ArgTerm1, MaybePtag),
                parse_remote_sectag(VarSet, ArgTerm2, MaybeRemoteSectag),
                ( if
                    list_term_to_term_list(ArgTerm3, ElementTerms),
                    ElementTerms = [HeadElementTerm | TailElementTerms]
                then
                    parse_remote_arg_repn(VarSet, HeadElementTerm,
                        MaybeHeadRemoteArg),
                    parse_remote_arg_repns(VarSet, TailElementTerms,
                        TailRemoteArgs, TailRemoteArgSpecs),
                    ( if
                        MaybePtag = ok1(Ptag),
                        MaybeRemoteSectag = ok1(RemoteSectag),
                        MaybeHeadRemoteArg = ok1(HeadRemoteArg),
                        TailRemoteArgSpecs = []
                    then
                        OoMRemoteArgs =
                            one_or_more(HeadRemoteArg, TailRemoteArgs),
                        NonConstantRepn =
                            ncr_remote_cell(Ptag, RemoteSectag, OoMRemoteArgs),
                        MaybeNonConstantRepn = ok1(NonConstantRepn)
                    else
                        Specs =
                            get_any_errors1(MaybePtag) ++
                            get_any_errors1(MaybeRemoteSectag) ++
                            get_any_errors1(MaybeHeadRemoteArg) ++
                            TailRemoteArgSpecs,
                        MaybeNonConstantRepn = error1(Specs)
                    )
                else
                    Pieces = [words("Error: the second argument of"),
                        quote(AtomStr), words("should be a nonempty list"),
                        words("of remote cell argument representations."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree, TermContext, Pieces),
                    Specs = [Spec] ++
                        get_any_errors1(MaybePtag) ++
                        get_any_errors1(MaybeRemoteSectag),
                    MaybeNonConstantRepn = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _]
                ; ArgTerms = [_, _, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have three arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeNonConstantRepn = error1([Spec])
            )
        ;
            AtomStr = "direct_arg",
            (
                ArgTerms = [ArgTerm1],
                parse_ptag(VarSet, ArgTerm1, MaybePtag),
                (
                    MaybePtag = ok1(Ptag),
                    NonConstantRepn = ncr_direct_arg(Ptag),
                    MaybeNonConstantRepn = ok1(NonConstantRepn)
                ;
                    MaybePtag = error1(Specs),
                    MaybeNonConstantRepn = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have one argument."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeNonConstantRepn = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("local_cell(...)"), suffix(","),
            quote("remote_cell(...)"), words("and"),
            quote("direct_arg"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeNonConstantRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_ptag(varset::in, term::in, maybe1(ptag)::out) is det.

parse_ptag(VarSet, Term, MaybePtag) :-
    parse_unlimited_uint(VarSet, Term, MaybeUint),
    (
        MaybeUint = ok1(Uint),
        ( if Uint =< 7u then
            Uint8 = uint8.det_from_int(uint.cast_to_int(Uint)),
            Ptag = ptag(Uint8),
            MaybePtag = ok1(Ptag)
        else
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: expected a ptag value"),
                words("in the range [0 .. 7],"),
                words("got"), quote(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybePtag = error1([Spec])
        )
    ;
        MaybeUint = error1(Specs),
        MaybePtag = error1(Specs)
    ).

:- pred parse_local_sectag(varset::in, term::in,
    maybe1(cell_local_sectag)::out) is det.

parse_local_sectag(VarSet, Term, MaybeLocalSectag) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "local_no_sectag"
        ; AtomStr = "local_sectag"
        )
    then
        (
            AtomStr = "local_no_sectag",
            (
                ArgTerms = [],
                MaybeLocalSectag = ok1(cell_local_no_sectag)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have no arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeLocalSectag = error1([Spec])
            )
        ;
            AtomStr = "local_sectag",
            (
                ArgTerms = [ArgTerm1, ArgTerm2],
                parse_unlimited_uint(VarSet, ArgTerm1, MaybeSectag),
                parse_unlimited_uint(VarSet, ArgTerm2, MaybeSectagNumBits),
                ( if
                    MaybeSectag = ok1(Sectag),
                    MaybeSectagNumBits = ok1(SectagNumBits)
                then
                    LocalSectag = cell_local_sectag(Sectag, SectagNumBits),
                    MaybeLocalSectag = ok1(LocalSectag)
                else
                    Specs =
                        get_any_errors1(MaybeSectag) ++
                        get_any_errors1(MaybeSectagNumBits),
                    MaybeLocalSectag = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have two arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeLocalSectag = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("local_no_sectag"), words("and"),
            quote("local_sectag(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeLocalSectag = error1([Spec])
    ).

:- pred parse_remote_sectag(varset::in, term::in,
    maybe1(cell_remote_sectag)::out) is det.

parse_remote_sectag(VarSet, Term, MaybeRemoteSectag) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "remote_no_sectag"
        ; AtomStr = "remote_sectag"
        )
    then
        (
            AtomStr = "remote_no_sectag",
            (
                ArgTerms = [],
                MaybeRemoteSectag = ok1(cell_remote_no_sectag)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have no arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeRemoteSectag = error1([Spec])
            )
        ;
            AtomStr = "remote_sectag",
            (
                ArgTerms = [ArgTerm1, ArgTerm2],
                parse_unlimited_uint(VarSet, ArgTerm1, MaybeSectag),
                parse_sectag_word_or_size(VarSet, ArgTerm2,
                    MaybeSectagWordOrSize),
                ( if
                    MaybeSectag = ok1(Sectag),
                    MaybeSectagWordOrSize = ok1(SectagWordOrSize)
                then
                    RemoteSectag =
                        cell_remote_sectag(Sectag, SectagWordOrSize),
                    MaybeRemoteSectag = ok1(RemoteSectag)
                else
                    Specs =
                        get_any_errors1(MaybeSectag) ++
                        get_any_errors1(MaybeSectagWordOrSize),
                    MaybeRemoteSectag = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have two arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeRemoteSectag = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("remote_no_sectag"), words("and"),
            quote("remote_sectag(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeRemoteSectag = error1([Spec])
    ).

:- pred parse_sectag_word_or_size(varset::in, term::in,
    maybe1(sectag_word_or_size)::out) is det.

parse_sectag_word_or_size(VarSet, Term, MaybeSectagSize) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "rest"
        ; AtomStr = "part"
        )
    then
        (
            AtomStr = "rest",
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
            AtomStr = "part",
            (
                ArgTerms = [ArgTerm1],
                parse_uint_in_range(63u, VarSet, ArgTerm1,
                    MaybeSectagNumBits),
                (
                    MaybeSectagNumBits = ok1(SectagNumBits),
                    MaybeSectagSize = ok1(sectag_part_of_word(SectagNumBits))
                ;
                    MaybeSectagNumBits = error1(Specs),
                    MaybeSectagSize = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have one argument."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeSectagSize = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("rest"), words("and"),
            quote("part(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeSectagSize = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_local_arg_repns(varset::in, list(term)::in,
    list(local_arg_repn)::out, list(error_spec)::out) is det.

parse_local_arg_repns(_VarSet, [], [], []).
parse_local_arg_repns(VarSet, [HeadTerm | TailTerms], LocalArgRepns, Specs) :-
    parse_local_arg_repns(VarSet, TailTerms, TailLocalArgRepns, TailSpecs),
    parse_local_arg_repn(VarSet, HeadTerm, MaybeHeadLocalArgRepn),
    (
        MaybeHeadLocalArgRepn = ok1(HeadLocalArgRepn),
        LocalArgRepns = [HeadLocalArgRepn | TailLocalArgRepns],
        Specs = TailSpecs
    ;
        MaybeHeadLocalArgRepn = error1(HeadSpecs),
        LocalArgRepns = TailLocalArgRepns,
        Specs = HeadSpecs ++ TailSpecs
    ).

:- pred parse_local_arg_repn(varset::in, term::in,
    maybe1(local_arg_repn)::out) is det.

parse_local_arg_repn(VarSet, Term, MaybeLocalArgRepn) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "local_partial"
        ; AtomStr = "local_none"
        )
    then
        (
            AtomStr = "local_partial",
            parse_local_arg_repn_partial(VarSet, AtomStr, ArgTerms,
                TermContext, MaybeLocalArgRepn)
        ;
            AtomStr = "local_none",
            (
                ArgTerms = [],
                MaybeLocalArgRepn = ok1(local_none)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have no arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeLocalArgRepn = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("local_partial(...)"),  words("and"),
            quote("local_none"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeLocalArgRepn = error1([Spec])
    ).

:- pred parse_local_arg_repn_partial(varset::in,
    string::in, list(term)::in, term.context::in,
    maybe1(local_arg_repn)::out) is det.

parse_local_arg_repn_partial(VarSet, AtomStr, ArgTerms, TermContext,
        MaybeLocalArgRepn) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2],
        parse_unlimited_uint(VarSet, ArgTerm1, MaybeShift),
        parse_fill_kind_size(VarSet, ArgTerm2, MaybeFillKindSize),
        ( if
            MaybeShift = ok1(Shift),
            MaybeFillKindSize = ok1(FillKindSize)
        then
            LocalArgRepn = local_partial(Shift, FillKindSize),
            MaybeLocalArgRepn = ok1(LocalArgRepn)
        else
            Specs =
                get_any_errors1(MaybeShift) ++
                get_any_errors1(MaybeFillKindSize),
            MaybeLocalArgRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error:"), quote(AtomStr),
            words("should have two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeLocalArgRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_remote_arg_repns(varset::in, list(term)::in,
    list(remote_arg_repn)::out, list(error_spec)::out) is det.

parse_remote_arg_repns(_VarSet, [], [], []).
parse_remote_arg_repns(VarSet, [HeadTerm | TailTerms],
        RemoteArgRepns, Specs) :-
    parse_remote_arg_repns(VarSet, TailTerms, TailRemoteArgRepns, TailSpecs),
    parse_remote_arg_repn(VarSet, HeadTerm, MaybeHeadRemoteArgRepn),
    (
        MaybeHeadRemoteArgRepn = ok1(HeadRemoteArgRepn),
        RemoteArgRepns = [HeadRemoteArgRepn | TailRemoteArgRepns],
        Specs = TailSpecs
    ;
        MaybeHeadRemoteArgRepn = error1(HeadSpecs),
        RemoteArgRepns = TailRemoteArgRepns,
        Specs = HeadSpecs ++ TailSpecs
    ).

:- pred parse_remote_arg_repn(varset::in, term::in,
    maybe1(remote_arg_repn)::out) is det.

parse_remote_arg_repn(VarSet, Term, MaybeRemoteArgRepn) :-
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
            parse_remote_arg_repn_full_or_none(VarSet, AtomStr, ArgTerms,
                TermContext, MaybeRemoteArgRepn)
        ;
            AtomStr = "double",
            parse_remote_arg_repn_double(VarSet, AtomStr, ArgTerms,
                TermContext, MaybeRemoteArgRepn)
        ;
            ( AtomStr = "partial_first"
            ; AtomStr = "partial_shifted"
            ),
            parse_remote_arg_repn_partial(VarSet, AtomStr, ArgTerms,
                TermContext, MaybeRemoteArgRepn)
        ;
            AtomStr = "none_shifted",
            parse_remote_arg_repn_full_or_none(VarSet, AtomStr, ArgTerms,
                TermContext, MaybeRemoteArgRepn)
        ;
            AtomStr = "none_nowhere",
            (
                ArgTerms = [],
                MaybeRemoteArgRepn = ok1(remote_none_nowhere)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have no arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeRemoteArgRepn = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("full(...)"), suffix(","),
            quote("double(...)"), suffix(","),
            quote("partial_first(...)"), suffix(","),
            quote("partial_shifted(...)"), suffix(","),
            quote("none_shifted(...)"), words("and"),
            quote("none_nowhere"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeRemoteArgRepn = error1([Spec])
    ).

:- inst full_none_kind for string/0
    --->    "full"
    ;       "none_shifted".

:- pred parse_remote_arg_repn_full_or_none(varset::in,
    string::in(full_none_kind), list(term)::in, term.context::in,
    maybe1(remote_arg_repn)::out) is det.

parse_remote_arg_repn_full_or_none(VarSet, AtomStr, ArgTerms, TermContext,
        MaybeRemoteArgRepn) :-
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
                RemoteArgRepn = remote_full(ArgOnlyOffset, CellOffset)
            ;
                AtomStr = "none_shifted",
                RemoteArgRepn = remote_none_shifted(ArgOnlyOffset, CellOffset)
            ),
            MaybeRemoteArgRepn = ok1(RemoteArgRepn)
        else
            Specs =
                get_any_errors1(MaybeArgOnlyOffset) ++
                get_any_errors1(MaybeCellOffset),
            MaybeRemoteArgRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error:"), quote(AtomStr),
            words("should have two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeRemoteArgRepn = error1([Spec])
    ).

:- pred parse_remote_arg_repn_double(varset::in, string::in, list(term)::in,
    term.context::in, maybe1(remote_arg_repn)::out) is det.

parse_remote_arg_repn_double(VarSet, AtomStr, ArgTerms, TermContext,
        MaybeRemoteArgRepn) :-
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
            RemoteArgRepn =
                remote_double(ArgOnlyOffset, CellOffset, DoubleWordKind),
            MaybeRemoteArgRepn = ok1(RemoteArgRepn)
        else
            Specs =
                get_any_errors1(MaybeArgOnlyOffset) ++
                get_any_errors1(MaybeCellOffset),
            MaybeRemoteArgRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _]
        ; ArgTerms = [_, _, _, _ | _]
        ),
        Pieces = [words("Error:"), quote(AtomStr),
            words("should have three arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeRemoteArgRepn = error1([Spec])
    ).

:- inst remote_partial_kind for string/0
    --->    "partial_first"
    ;       "partial_shifted".

:- pred parse_remote_arg_repn_partial(varset::in,
    string::in(remote_partial_kind), list(term)::in, term.context::in,
    maybe1(remote_arg_repn)::out) is det.

parse_remote_arg_repn_partial(VarSet, AtomStr, ArgTerms, TermContext,
        MaybeRemoteArgRepn) :-
    (
        ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4],
        parse_arg_only_offset(VarSet, ArgTerm1, MaybeArgOnlyOffset),
        parse_cell_offset(VarSet, ArgTerm2, MaybeCellOffset),
        parse_unlimited_uint(VarSet, ArgTerm3, MaybeShift),
        parse_fill_kind_size(VarSet, ArgTerm4, MaybeFillKindSize),
        ( if
            MaybeArgOnlyOffset = ok1(ArgOnlyOffset),
            MaybeCellOffset = ok1(CellOffset),
            MaybeShift = ok1(Shift),
            MaybeFillKindSize = ok1(FillKindSize)
        then
            (
                AtomStr = "partial_first",
                RemoteArgRepn = remote_partial_first(ArgOnlyOffset,
                    CellOffset, Shift, FillKindSize)
            ;
                AtomStr = "partial_shifted",
                RemoteArgRepn = remote_partial_shifted(ArgOnlyOffset,
                    CellOffset, Shift, FillKindSize)
            ),
            MaybeRemoteArgRepn = ok1(RemoteArgRepn)
        else
            Specs =
                get_any_errors1(MaybeArgOnlyOffset) ++
                get_any_errors1(MaybeCellOffset) ++
                get_any_errors1(MaybeShift) ++
                get_any_errors1(MaybeFillKindSize),
            MaybeRemoteArgRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _]
        ; ArgTerms = [_, _, _]
        ; ArgTerms = [_, _, _, _, _ | _]
        ),
        Pieces = [words("Error:"), quote(AtomStr),
            words("should have four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            TermContext, Pieces),
        MaybeRemoteArgRepn = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_arg_only_offset(varset::in, term::in,
    maybe1(arg_only_offset)::out) is det.

parse_arg_only_offset(VarSet, Term, MaybeArgOnlyOffset) :-
    parse_unlimited_int(VarSet, Term, MaybeInt),
    (
        MaybeInt = ok1(Int),
        ( if Int >= -2 then
            MaybeArgOnlyOffset = ok1(arg_only_offset(Int))
        else
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: expected an integer that is"),
                words("at least -2, got"), quote(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybeArgOnlyOffset = error1([Spec])
        )
    ;
        MaybeInt = error1(Specs),
        MaybeArgOnlyOffset = error1(Specs)
    ).

:- pred parse_cell_offset(varset::in, term::in,
    maybe1(cell_offset)::out) is det.

parse_cell_offset(VarSet, Term, MaybeCellOffset) :-
    parse_unlimited_int(VarSet, Term, MaybeInt),
    (
        MaybeInt = ok1(Int),
        ( if Int >= -2 then
            MaybeCellOffset = ok1(cell_offset(Int))
        else
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: expected an integer that is"),
                words("at least -2, got"), quote(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybeCellOffset = error1([Spec])
        )
    ;
        MaybeInt = error1(Specs),
        MaybeCellOffset = error1(Specs)
    ).

:- pred parse_fill_kind_size(varset::in, term::in,
    maybe1(fill_kind_size)::out) is det.

parse_fill_kind_size(VarSet, Term, MaybeFillKindSize) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "enum"
        ; AtomStr = "int8"
        ; AtomStr = "int16"
        ; AtomStr = "int32"
        ; AtomStr = "uint8"
        ; AtomStr = "uint16"
        ; AtomStr = "uint32"
        ; AtomStr = "char21"
        )
    then
        (
            AtomStr = "enum",
            (
                ArgTerms = [ArgTerm],
                parse_uint_in_range(63u, VarSet, ArgTerm, MaybeUint),
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
                    words("should have one argument."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeFillKindSize = error1([Spec])
            )
        ;
            AtomStr = "int8",
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_int8,
                MaybeFillKindSize)
        ;
            AtomStr = "int16",
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_int16,
                MaybeFillKindSize)
        ;
            AtomStr = "int32",
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_int32,
                MaybeFillKindSize)
        ;
            AtomStr = "uint8",
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_uint8,
                MaybeFillKindSize)
        ;
            AtomStr = "uint16",
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_uint16,
                MaybeFillKindSize)
        ;
            AtomStr = "uint32",
            ok_if_arity_zero(AtomStr, TermContext, ArgTerms, fk_uint32,
                MaybeFillKindSize)
        ;
            AtomStr = "char21",
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
%
% We used to write out information about foreign type representations
% (including foreign enums) in the form of a list, with each element of
% that list having a function symbol that is the name of a foreign language
% wrapped around a term that specifies the representation in that language.
% This is reasonably simple, but it does not encode the invariant that
% a type must have at most one representation in each each foreign language.
%
% We now therefore generate this information in a another form that *does*
% encode this invariant. Specifically, we now generate either
%
%   no_c_j_cs_e
%
% or a term of the form
%
%   c_j_cs_e(MaybeC, MaybeJava, MaybeCsharp, MaybeErlang)
%
% where all of the MaybeXs have the same structure and none of them mention
% what foreign language they are for, that information being implicit
% in their position inside c_j_cs_e.
%
% However, while installed compilers still generate the old style descriptions,
% we accept the old form as well, though we don't bother generating useful
% error messages for them. Instead, if there is any error, we generate
% an error about the deviation from the *new* description style.
%

:- pred parse_c_j_cs_e_repn_or_enum(list(format_component)::in,
    varset::in, term::in, maybe1(c_j_cs_e_enum_repn)::out) is det.

parse_c_j_cs_e_repn_or_enum(DescPieces, VarSet, Term, MaybeCJCsERepnOrEnum) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "no_c_j_cs_e"
        ; AtomStr = "c_j_cs_e"
        )
    then
        (
            AtomStr = "no_c_j_cs_e",
            (
                ArgTerms = [],
                CJCsERepnOrEnum = c_java_csharp_erlang(no, no, no, no),
                MaybeCJCsERepnOrEnum = ok1(CJCsERepnOrEnum)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
                    words("error:"), quote(AtomStr),
                    words("should have zero arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeCJCsERepnOrEnum = error1([Spec])
            )
        ;
            AtomStr = "c_j_cs_e",
            (
                ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4],
                parse_maybe_enum_foreign_repn(DescPieces, 1,
                    VarSet, ArgTerm1, MaybeMaybeRepnOrEnumC),
                parse_maybe_enum_foreign_repn(DescPieces, 2,
                    VarSet, ArgTerm2, MaybeMaybeRepnOrEnumJava),
                parse_maybe_enum_foreign_repn(DescPieces, 3,
                    VarSet, ArgTerm3, MaybeMaybeRepnOrEnumCsharp),
                parse_maybe_enum_foreign_repn(DescPieces, 4,
                    VarSet, ArgTerm4, MaybeMaybeRepnOrEnumErlang),
                ( if
                    MaybeMaybeRepnOrEnumC = ok1(MaybeRepnOrEnumC),
                    MaybeMaybeRepnOrEnumJava = ok1(MaybeRepnOrEnumJava),
                    MaybeMaybeRepnOrEnumCsharp = ok1(MaybeRepnOrEnumCsharp),
                    MaybeMaybeRepnOrEnumErlang = ok1(MaybeRepnOrEnumErlang)
                then
                    CJCsERepnOrEnum = c_java_csharp_erlang(
                        MaybeRepnOrEnumC, MaybeRepnOrEnumJava,
                        MaybeRepnOrEnumCsharp, MaybeRepnOrEnumErlang),
                    MaybeCJCsERepnOrEnum = ok1(CJCsERepnOrEnum)
                else
                    Specs =
                        get_any_errors1(MaybeMaybeRepnOrEnumC) ++
                        get_any_errors1(MaybeMaybeRepnOrEnumJava) ++
                        get_any_errors1(MaybeMaybeRepnOrEnumCsharp) ++
                        get_any_errors1(MaybeMaybeRepnOrEnumErlang),
                    MaybeCJCsERepnOrEnum = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _]
                ; ArgTerms = [_, _, _]
                ; ArgTerms = [_, _, _, _, _ | _]
                ),
                Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
                    words("error:"), quote(AtomStr),
                    words("should have four arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeCJCsERepnOrEnum = error1([Spec])
            )
        )
    else if
        list_term_to_term_list(Term, ForeignTerms),
        parse_old_style_foreign_type_repn_or_enums(VarSet, ForeignTerms,
            ForeignPairs, ForeignSpecs),
        ForeignSpecs = [],
        assoc_list_to_c_j_cs_e(ForeignPairs, MaybeOldStyleCJCsERepnOrEnum),
        MaybeOldStyleCJCsERepnOrEnum = ok1(_)
    then
        MaybeCJCsERepnOrEnum = MaybeOldStyleCJCsERepnOrEnum
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
            words("error: expected either"),
            quote("no_c_j_cs_e"), words("or"),
            quote("c_j_cs_e(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeCJCsERepnOrEnum = error1([Spec])
    ).

:- pred parse_c_j_cs_e_repn(list(format_component)::in,
    varset::in, term::in, maybe1(c_j_cs_e_repn)::out) is det.

parse_c_j_cs_e_repn(DescPieces, VarSet, Term, MaybeCJCsERepn) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "no_c_j_cs_e"
        ; AtomStr = "c_j_cs_e"
        )
    then
        (
            AtomStr = "no_c_j_cs_e",
            (
                ArgTerms = [],
                CJCsERepn = c_java_csharp_erlang(no, no, no, no),
                MaybeCJCsERepn = ok1(CJCsERepn)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
                    words("error:"), quote(AtomStr),
                    words("should have zero arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeCJCsERepn = error1([Spec])
            )
        ;
            AtomStr = "c_j_cs_e",
            (
                ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4],
                parse_maybe_foreign_repn(DescPieces, 1,
                    VarSet, ArgTerm1, MaybeMaybeRepnC),
                parse_maybe_foreign_repn(DescPieces, 2,
                    VarSet, ArgTerm2, MaybeMaybeRepnJava),
                parse_maybe_foreign_repn(DescPieces, 3,
                    VarSet, ArgTerm3, MaybeMaybeRepnCsharp),
                parse_maybe_foreign_repn(DescPieces, 4,
                    VarSet, ArgTerm4, MaybeMaybeRepnErlang),
                ( if
                    MaybeMaybeRepnC = ok1(MaybeRepnC),
                    MaybeMaybeRepnJava = ok1(MaybeRepnJava),
                    MaybeMaybeRepnCsharp = ok1(MaybeRepnCsharp),
                    MaybeMaybeRepnErlang = ok1(MaybeRepnErlang)
                then
                    CJCsERepn = c_java_csharp_erlang(
                        MaybeRepnC, MaybeRepnJava,
                        MaybeRepnCsharp, MaybeRepnErlang),
                    MaybeCJCsERepn = ok1(CJCsERepn)
                else
                    Specs =
                        get_any_errors1(MaybeMaybeRepnC) ++
                        get_any_errors1(MaybeMaybeRepnJava) ++
                        get_any_errors1(MaybeMaybeRepnCsharp) ++
                        get_any_errors1(MaybeMaybeRepnErlang),
                    MaybeCJCsERepn = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _]
                ; ArgTerms = [_, _, _]
                ; ArgTerms = [_, _, _, _, _ | _]
                ),
                Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
                    words("error:"), quote(AtomStr),
                    words("should have four arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeCJCsERepn = error1([Spec])
            )
        )
    else if
        list_term_to_term_list(Term, ForeignTerms),
        parse_old_style_foreign_type_repns(VarSet, ForeignTerms,
            ForeignPairs, ForeignSpecs),
        ForeignSpecs = [],
        assoc_list_to_c_j_cs_e(ForeignPairs, MaybeOldStyleCJCsERepn),
        MaybeOldStyleCJCsERepn = ok1(_)
    then
        MaybeCJCsERepn = MaybeOldStyleCJCsERepn
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
            words("error: expected either"),
            quote("no_c_j_cs_e"), words("or"),
            quote("c_j_cs_e(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeCJCsERepn = error1([Spec])
    ).

%---------------------%

:- pred parse_maybe_enum_foreign_repn(list(format_component)::in, int::in,
    varset::in, term::in, maybe1(maybe(enum_foreign_repn))::out) is det.

parse_maybe_enum_foreign_repn(DescPieces, Nth, VarSet, Term,
        MaybeMaybeRepnOrEnum) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "no_foreign"
        ; AtomStr = "foreign_enum"
        ; AtomStr = "foreign_type"
        )
    then
        (
            AtomStr = "no_foreign",
            (
                ArgTerms = [],
                MaybeRepnOrEnum = no,
                MaybeMaybeRepnOrEnum = ok1(MaybeRepnOrEnum)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
                    words("in the"), nth_fixed(Nth), words("argument of"),
                    quote("c_j_cs_e"), suffix(":"),
                    words("error:"), quote(AtomStr),
                    words("should have zero arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeMaybeRepnOrEnum = error1([Spec])
            )
        ;
            AtomStr = "foreign_enum",
            EnumDescPieces = DescPieces ++ [suffix(":"),
                words("in the"), nth_fixed(Nth), words("argument of"),
                quote("c_j_cs_e")],
            (
                ArgTerms = [ArgTerm1],
                parse_one_or_more_strings(EnumDescPieces, VarSet, ArgTerm1,
                    MaybeForeignEnumRepn),
                (
                    MaybeForeignEnumRepn = ok1(ForeignEnumRepn),
                    MaybeRepnOrEnum = yes(enum_foreign_enum(ForeignEnumRepn)),
                    MaybeMaybeRepnOrEnum = ok1(MaybeRepnOrEnum)
                ;
                    MaybeForeignEnumRepn = error1(Specs),
                    MaybeMaybeRepnOrEnum = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
                    words("in the"), nth_fixed(Nth), words("argument of"),
                    quote("c_j_cs_e"), suffix(":"),
                    words("error:"), quote(AtomStr),
                    words("should have one argument."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeMaybeRepnOrEnum = error1([Spec])
            )
        ;
            AtomStr = "foreign_type",
            parse_foreign_type_repn(DescPieces, Nth, VarSet, AtomStr, ArgTerms,
                TermContext, MaybeForeignTypeRepn),
            (
                MaybeForeignTypeRepn = ok1(ForeignTypeRepn),
                MaybeRepnOrEnum = yes(enum_foreign_type(ForeignTypeRepn)),
                MaybeMaybeRepnOrEnum = ok1(MaybeRepnOrEnum)
            ;
                MaybeForeignTypeRepn = error1(Specs),
                MaybeMaybeRepnOrEnum = error1(Specs)
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
            words("in the"), nth_fixed(Nth), words("argument of"),
            quote("c_j_cs_e"), suffix(":"),
            words("error: expected one of"),
            quote("no_foreign"), suffix(","),
            quote("foreign_enum(...)"), words("and"),
            quote("foreign_type(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeMaybeRepnOrEnum = error1([Spec])
    ).

:- pred parse_maybe_foreign_repn(list(format_component)::in, int::in,
    varset::in, term::in, maybe1(maybe(foreign_type_repn))::out) is det.

parse_maybe_foreign_repn(DescPieces, Nth, VarSet, Term,
        MaybeMaybeRepnOrEnum) :-
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, TermContext),
        ( AtomStr = "no_foreign"
        ; AtomStr = "foreign_type"
        )
    then
        (
            AtomStr = "no_foreign",
            (
                ArgTerms = [],
                MaybeRepnOrEnum = no,
                MaybeMaybeRepnOrEnum = ok1(MaybeRepnOrEnum)
            ;
                ArgTerms = [_ | _],
                Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
                    words("in the"), nth_fixed(Nth), words("argument of"),
                    quote("c_j_cs_e"), suffix(":"),
                    words("error:"), quote(AtomStr),
                    words("should have zero arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeMaybeRepnOrEnum = error1([Spec])
            )
        ;
            AtomStr = "foreign_type",
            parse_foreign_type_repn(DescPieces, Nth, VarSet, AtomStr, ArgTerms,
                TermContext, MaybeForeignTypeRepn),
            (
                MaybeForeignTypeRepn = ok1(ForeignTypeRepn),
                MaybeRepnOrEnum = yes(ForeignTypeRepn),
                MaybeMaybeRepnOrEnum = ok1(MaybeRepnOrEnum)
            ;
                MaybeForeignTypeRepn = error1(Specs),
                MaybeMaybeRepnOrEnum = error1(Specs)
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
            words("in the"), nth_fixed(Nth), words("argument of"),
            quote("c_j_cs_e"), suffix(":"),
            words("error: expected one of"),
            quote("no_foreign"), words("and"),
            quote("foreign_type(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeMaybeRepnOrEnum = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_foreign_type(varset::in, string::in,
    list(term)::in, term.context::in, maybe1(type_ctor_repn_info)::out) is det.

parse_type_repn_foreign_type(VarSet, RepnStr, RepnArgs, RepnContext,
        MaybeRepn) :-
    (
        RepnArgs = [RepnArg1],
        DescPieces = [words("the argument of"), quote(RepnStr)],
        parse_c_j_cs_e_repn(DescPieces, VarSet, RepnArg1, MaybeCJCsERepn),
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
            words("should have one argument."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            RepnContext, Pieces),
        MaybeRepn = error1([Spec])
    ).

%---------------------%

:- pred parse_foreign_type_repn(list(format_component)::in, int::in,
    varset::in, string::in, list(term)::in, term.context::in,
    maybe1(foreign_type_repn)::out) is det.

parse_foreign_type_repn(DescPieces, Nth, VarSet, AtomStr, ArgTerms,
        TermContext, MaybeForeignTypeRepn) :-
    (
        ArgTerms = [TypeNameTerm, AssertionTerm],
        ( if
            TypeNameTerm = term.functor(term.string(TypeNameStr0), [], _)
        then
            MaybeTypeName = ok1(TypeNameStr0)
        else
            TermStr = describe_error_term(VarSet, TypeNameTerm),
            TypeNamePieces = [words("In")] ++ DescPieces ++ [suffix(":"),
                words("in the"), nth_fixed(Nth), words("argument of"),
                quote("c_j_cs_e"), suffix(":"),
                words("error: expected the first argument of"), quote(AtomStr),
                words("to be a string, got"), quote(TermStr), suffix("."), nl],
            TypeNameSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(TypeNameTerm),
                TypeNamePieces),
            MaybeTypeName = error1([TypeNameSpec])
        ),
        ContextPieces2 = [words("In")] ++ DescPieces ++ [suffix(":"),
            words("in the"), nth_fixed(Nth), words("argument of"),
            quote("c_j_cs_e"), suffix(":"),
            words("in the second argument of"), quote(AtomStr), suffix(":")],
        ContextPiecesCord2 = cord.from_list(ContextPieces2),
        parse_foreign_type_assertions(ContextPiecesCord2, VarSet,
            AssertionTerm, set.init, AssertionSet, [], AssertionSpecs),
        ( if
            MaybeTypeName = ok1(TypeName),
            AssertionSpecs = []
        then
            Assertions = foreign_type_assertions(AssertionSet),
            Repn = foreign_type_repn(TypeName, Assertions),
            MaybeForeignTypeRepn = ok1(Repn)
        else
            Specs = get_any_errors1(MaybeTypeName) ++ AssertionSpecs,
            MaybeForeignTypeRepn = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
            words("in the"), nth_fixed(Nth), words("argument of"),
            quote("c_j_cs_e"), suffix(":"),
            words("error:"), quote(AtomStr),
            words("should have zero arguments."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, TermContext, Pieces),
        MaybeForeignTypeRepn = error1([Spec])
    ).

%---------------------%

:- pred parse_one_or_more_strings(list(format_component)::in,
    varset::in, term::in, maybe1(one_or_more(string))::out) is det.

parse_one_or_more_strings(DescPieces, VarSet, Term, MaybeOoMStrings) :-
    ( if list_term_to_term_list(Term, StringTerms) then
        parse_strings(DescPieces, 1, VarSet, StringTerms,
            Strings, StringSpecs),
        (
            Strings = [],
            (
                StringTerms = [],
                expect(unify(StringSpecs, []), $pred,
                    "StringTerms = [] but StringSpecs != []"),
                Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
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
        Pieces = [words("In") | DescPieces] ++ [suffix(":"),
            words("error: expected a list of strings, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeOoMStrings = error1([Spec])
    ).

%-----------------------------------------------------------------------------e
%
% XXX The code in this section should be needed only during a changeover
% period while installed compilers may generate old-style terms for describing
% foreign language type representations.
%

:- pred parse_old_style_foreign_type_repn_or_enums(varset::in, list(term)::in,
    assoc_list(foreign_language, enum_foreign_repn)::out,
    list(error_spec)::out) is det.

parse_old_style_foreign_type_repn_or_enums(_, [], [], []).
parse_old_style_foreign_type_repn_or_enums(VarSet, [Term | Terms],
        !:ForeignPairs, !:Specs) :-
    parse_old_style_foreign_type_repn_or_enums(VarSet, Terms,
        !:ForeignPairs, !:Specs),
    parse_old_style_foreign_language_type_repn_or_enum(VarSet, Term,
        MaybeForeignLangRepn),
    (
        MaybeForeignLangRepn = ok2(Lang, Repn),
        !:ForeignPairs = [Lang - Repn | !.ForeignPairs]
    ;
        MaybeForeignLangRepn = error2(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%---------------------%

:- pred parse_old_style_foreign_language_type_repn_or_enum(varset::in,
    term::in, maybe2(foreign_language, enum_foreign_repn)::out) is det.

parse_old_style_foreign_language_type_repn_or_enum(VarSet, Term,
        MaybeForeignLangRepnOrEnum) :-
    ( if
        Term = term.functor(term.atom(FunctorStr), ArgTerms, _),
        ( FunctorStr = "foreign_type"
        ; FunctorStr = "foreign_enum"
        )
    then
        (
            FunctorStr = "foreign_type",
            (
                ArgTerms = [ArgTerm1],
                parse_old_style_foreign_language_type_repn(VarSet, ArgTerm1,
                    MaybeForeignLangRepn),
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
                MaybeForeignLangRepnOrEnum = error2([])
            )
        ;
            FunctorStr = "foreign_enum",
            (
                ArgTerms = [ArgTerm1, ArgTerm2],
                ( if
                    ArgTerm1 = term.functor(term.atom(LangStr), [], _),
                    ( LangStr = "c",      Lang0 = lang_c
                    ; LangStr = "csharp", Lang0 = lang_csharp
                    ; LangStr = "java",   Lang0 = lang_java
                    ; LangStr = "erlang", Lang0 = lang_erlang
                    )
                then
                    MaybeLang = ok1(Lang0)
                else
                    MaybeLang = error1([])
                ),
                parse_one_or_more_strings([], VarSet, ArgTerm2,
                    MaybeOoMStrings),
                ( if
                    MaybeLang = ok1(Lang),
                    MaybeOoMStrings = ok1(OoMStrings)
                then
                    MaybeForeignLangRepnOrEnum =
                        ok2(Lang, enum_foreign_enum(OoMStrings))
                else
                    MaybeForeignLangRepnOrEnum = error2([])
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _, _ | _]
                ),
                MaybeForeignLangRepnOrEnum = error2([])
            )
        )
    else
        MaybeForeignLangRepnOrEnum = error2([])
    ).

%---------------------%

:- pred parse_old_style_foreign_type_repns(varset::in, list(term)::in,
    assoc_list(foreign_language, foreign_type_repn)::out,
    list(error_spec)::out) is det.

parse_old_style_foreign_type_repns(_, [], [], []).
parse_old_style_foreign_type_repns(VarSet, [Term | Terms],
        !:ForeignPairs, !:Specs) :-
    parse_old_style_foreign_type_repns(VarSet, Terms,
        !:ForeignPairs, !:Specs),
    parse_old_style_foreign_language_type_repn(VarSet, Term,
        MaybeForeignLangRepn),
    (
        MaybeForeignLangRepn = ok2(Lang, Repn),
        !:ForeignPairs = [Lang - Repn | !.ForeignPairs]
    ;
        MaybeForeignLangRepn = error2(Specs),
        !:Specs = Specs ++ !.Specs
    ).

:- pred parse_old_style_foreign_language_type_repn(varset::in, term::in,
    maybe2(foreign_language, foreign_type_repn)::out) is det.

parse_old_style_foreign_language_type_repn(VarSet, Term,
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
        parse_old_style_foreign_type_repn(VarSet, Lang,
            TypeNameTerm, AssertionTerm, MaybeForeignLangRepn)
    else
        MaybeForeignLangRepn = error2([])
    ).

:- pred parse_old_style_foreign_type_repn(varset::in,
    foreign_language::in, term::in, term::in,
    maybe2(foreign_language, foreign_type_repn)::out) is det.

parse_old_style_foreign_type_repn(VarSet, Lang,
        TypeNameTerm, AssertionTerm, MaybeForeignLangRepn) :-
    ( if TypeNameTerm = term.functor(term.string(TypeNameStr), [], _) then
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
                MaybeTypeName = error1([])
            )
        )
    else
        MaybeTypeName = error1([])
    ),
    AssertionContextPieces = cord.from_list([]),
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

%---------------------%

:- pred assoc_list_to_c_j_cs_e(assoc_list(foreign_language, T)::in,
    maybe1(c_java_csharp_erlang(maybe(T)))::out) is det.

assoc_list_to_c_j_cs_e(!.Pairs, MaybeCJCsE) :-
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
        MaybeCJCsE = error1([])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_c_repns(list(format_component)::in,
    pred(varset, term, maybe1(T))::in(pred(in, in, out) is det),
    varset::in, term::in, maybe1(c_repns(T))::out) is det.

parse_c_repns(DescPieces, ParseRepn, VarSet, Term, MaybeCRepns) :-
    % XXX Each call to ParseRepn should extend and pass DescPieces.
    ( if
        Term = term.functor(term.atom(AtomStr), ArgTerms, _),
        ( AtomStr = "c_repns_same"
        ; AtomStr = "c_repns_64_32"
        ; AtomStr = "c_repns_all"
        )
    then
        (
            AtomStr = "c_repns_same",
            (
                ArgTerms = [ArgTerm1],
                ParseRepn(VarSet, ArgTerm1, MaybeRepn1),
                ( if
                    MaybeRepn1 = ok1(Repn1)
                then
                    MaybeCRepns = ok1(c_repns_same(Repn1))
                else
                    Specs = get_any_errors1(MaybeRepn1),
                    MaybeCRepns = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Pieces = [words("In") | DescPieces] ++ [suffix(":"),
                    quote("c_repns_same"),
                    words("should have one argument."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Term), Pieces),
                MaybeCRepns = error1([Spec])
            )
        ;
            AtomStr = "c_repns_64_32",
            (
                ArgTerms = [ArgTerm1, ArgTerm2],
                ParseRepn(VarSet, ArgTerm1, MaybeRepn1),
                ParseRepn(VarSet, ArgTerm2, MaybeRepn2),
                ( if
                    MaybeRepn1 = ok1(Repn1),
                    MaybeRepn2 = ok1(Repn2)
                then
                    MaybeCRepns = ok1(c_repns_64_32(Repn1, Repn2))
                else
                    Specs =
                        get_any_errors1(MaybeRepn1) ++
                        get_any_errors1(MaybeRepn2),
                    MaybeCRepns = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _, _ | _]
                ),
                Pieces = [words("In") | DescPieces] ++ [suffix(":"),
                    quote("c_repns_64_32"),
                    words("should have two arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Term), Pieces),
                MaybeCRepns = error1([Spec])
            )
        ;
            AtomStr = "c_repns_all",
            (
                ArgTerms = [ArgTerm1, ArgTerm2, ArgTerm3, ArgTerm4,
                    ArgTerm5, ArgTerm6],
                ParseRepn(VarSet, ArgTerm1, MaybeRepn1),
                ParseRepn(VarSet, ArgTerm2, MaybeRepn2),
                ParseRepn(VarSet, ArgTerm3, MaybeRepn3),
                ParseRepn(VarSet, ArgTerm4, MaybeRepn4),
                ParseRepn(VarSet, ArgTerm5, MaybeRepn5),
                ParseRepn(VarSet, ArgTerm6, MaybeRepn6),
                ( if
                    MaybeRepn1 = ok1(Repn1),
                    MaybeRepn2 = ok1(Repn2),
                    MaybeRepn3 = ok1(Repn3),
                    MaybeRepn4 = ok1(Repn4),
                    MaybeRepn5 = ok1(Repn5),
                    MaybeRepn6 = ok1(Repn6)
                then
                    MaybeCRepns = ok1(c_repns_all(Repn1, Repn2, Repn3,
                        Repn4, Repn5, Repn6))
                else
                    Specs =
                        get_any_errors1(MaybeRepn1) ++
                        get_any_errors1(MaybeRepn2) ++
                        get_any_errors1(MaybeRepn3) ++
                        get_any_errors1(MaybeRepn4) ++
                        get_any_errors1(MaybeRepn5) ++
                        get_any_errors1(MaybeRepn6),
                    MaybeCRepns = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _]
                ; ArgTerms = [_, _, _]
                ; ArgTerms = [_, _, _, _]
                ; ArgTerms = [_, _, _, _, _]
                ; ArgTerms = [_, _, _, _, _, _, _ | _]
                ),
                Pieces = [words("In") | DescPieces] ++ [suffix(":"),
                    quote("c_repns_all"),
                    words("should have six arguments."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Term), Pieces),
                MaybeCRepns = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("In") | DescPieces] ++ [suffix(":"),
            words("error: expected one of"),
            quote("c_repns_same(...),"),
            quote("c_repns_64_32(...)"), words("or"),
            quote("c_repns_all(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeCRepns = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_strings(list(format_component)::in, int::in, varset::in,
    list(term)::in, list(string)::out, list(error_spec)::out) is det.

parse_strings(_, _, _, [], [], []).
parse_strings(DescPieces, Nth, VarSet, [Term | Terms], !:Strs, !:Specs) :-
    parse_strings(DescPieces, Nth + 1, VarSet, Terms, !:Strs, !:Specs),
    ( if Term = term.functor(term.string(Str), [], _) then
        !:Strs = [Str | !.Strs]
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("In")] ++ DescPieces ++ [suffix(":"),
            words("in the"), nth_fixed(Nth), words("element of the list:"),
            words("error: expected a string, got"),
            quote(TermStr), suffix("."), nl],
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

%-----------------------------------------------------------------------------e

:- pred parse_unlimited_int(varset::in, term::in, maybe1(int)::out) is det.

parse_unlimited_int(VarSet, Term, MaybeInt) :-
    ( if
        Term = term.functor(term.integer(Base, N, Signedness, Size), [], _),
        Base = base_10,
        Signedness = signed,
        Size = size_word,
        integer.to_int(N, IntN)
    then
        MaybeInt = ok1(IntN)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected an integer,"),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeInt = error1([Spec])
    ).

:- pred parse_unlimited_uint(varset::in, term::in, maybe1(uint)::out) is det.

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
        Pieces = [words("Error: expected a nonnegative integer,"),
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
