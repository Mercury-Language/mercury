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
            ; AtomStr = "maybe_foreign_type_repn"
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
                    parse_type_repn_du(VarSet, direct_in_item, RepnArg,
                        RepnContext, MaybeDuRepn),
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
                    DuSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(RepnContext, [always(DuPieces)])]),
                    MaybeRepn = error1([DuSpec])
                )
            ;
                AtomStr = "maybe_foreign_type_repn",
                parse_type_repn_maybe_foreign_type(VarSet, AtomStr, RepnArgs,
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
            RepnSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(RepnTerm),
                    [always(RepnPieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(RepnContext, [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(RepnContext, [always(Pieces)])]),
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
            NumBitsSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(RepnArg1),
                    [always(NumBitsPieces)])]),
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
            FillKindSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(RepnArg2),
                    [always(FillKindPieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(RepnContext, [always(Pieces)])]),
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
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(RepnArg), [always(Pieces)])]),
            MaybeRepn = error1([Spec])
        )
    ;
        ( RepnArgs = []
        ; RepnArgs = [_, _ | _]
        ),
        Pieces = [words("Error:"), quote(RepnStr),
            words("should have exactly one argument,"),
            words("a list of function symbols with arities."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(RepnContext, [always(Pieces)])]),
        MaybeRepn = error1([Spec])
    ).

:- pred parse_functor_with_arities(string::in, int::in, list(term)::in,
    maybe1(list(sym_name_and_arity))::out) is det.

parse_functor_with_arities(_, _, [], ok1([])).
parse_functor_with_arities(RepnStr, Nth, [Term | Terms], MaybeFunctors) :-
    ( if parse_name_and_arity_unqualified(Term, SymName, Arity) then
        MaybeHeadFunctor = ok1(sym_name_arity(SymName, Arity))
    else
        Pieces = [words("Error: the"), nth_fixed(Nth), words("element"),
            words("of the list in the second argument of"), quote(RepnStr),
            words("should have the form"),
            quote("functorname/arity"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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

:- type du_where
    --->    direct_in_item
    ;       indirect_in_maybe_foreign_type.

:- pred parse_type_repn_du(varset::in, du_where::in, term::in,
    term.context::in, maybe1(du_repn)::out) is det.

parse_type_repn_du(VarSet, DuWhere, Term, _RepnContext, MaybeDuRepn) :-
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
            (
                ArgTerms = [ArgTerm],
                ContextPieces = [words("In argument of"), quote("notag"),
                    suffix(":"), nl],
                parse_string(VarSet, ContextPieces, "function symbol",
                    ArgTerm, MaybeFunctorName),
                (
                    MaybeFunctorName = ok1(FunctorName),
                    NotagRepn = notag_repn(FunctorName),
                    DuRepn = dur_notag(NotagRepn),
                    MaybeDuRepn = ok1(DuRepn)
                ;
                    MaybeFunctorName = error1(Specs),
                    MaybeDuRepn = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Prefix = du_where_prefix(DuWhere),
                Pieces = [quote("notag"),
                    words("should have exactly one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Prefix ++ Pieces)])]),
                MaybeDuRepn = error1([Spec])
            )
        ;
            AtomStr = "direct_dummy",
            (
                ArgTerms = [ArgTerm],
                ContextPieces = [words("In argument of"),
                    quote("direct_dummy"), suffix(":"), nl],
                parse_string(VarSet, ContextPieces, "function symbol",
                    ArgTerm, MaybeFunctorName),
                (
                    MaybeFunctorName = ok1(FunctorName),
                    DirectDummyRepn = direct_dummy_repn(FunctorName),
                    DuRepn = dur_direct_dummy(DirectDummyRepn),
                    MaybeDuRepn = ok1(DuRepn)
                ;
                    MaybeFunctorName = error1(Specs),
                    MaybeDuRepn = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Prefix = du_where_prefix(DuWhere),
                Pieces = [quote("direct_dummy"),
                    words("should have exactly one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Prefix ++ Pieces)])]),
                MaybeDuRepn = error1([Spec])
            )
        ;
            AtomStr = "enum",
            (
                (
                    ArgTerms = [ArgTerm1],
                    ForeignEnums = [],
                    ForeignEnumSpecs = []
                ;
                    ArgTerms = [ArgTerm1, ArgTerm2],
                    ( if
                        list_term_to_term_list(ArgTerm2, ForeignEnumTerms),
                        ForeignEnumTerms = [_ | _]
                    then
                        parse_foreign_enum_specs(VarSet, AtomStr, 1,
                            ForeignEnumTerms, ForeignEnums, ForeignEnumSpecs)
                    else
                        ForeignEnums = [],
                        ForeignPieces = [words("Error: the second argument"),
                            words("of"), quote(AtomStr), words("should be"),
                            words("a nonempty list of foreign language"),
                            words("enum specifications."), nl],
                        ForeignSpec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(get_term_context(ArgTerm2),
                                [always(ForeignPieces)])]),
                        ForeignEnumSpecs = [ForeignSpec]
                    )
                ),
                ( if
                    list_term_to_term_list(ArgTerm1, EnumFunctorTerms),
                    EnumFunctorTerms = [_ | _]
                then
                    ContextPieces = [words("first argument of"),
                        quote("enum"), suffix(":")],
                    parse_strings(VarSet, ContextPieces, "function symbol", 1,
                        EnumFunctorTerms, EnumFunctorNames, EnumSpecs)
                else
                    EnumFunctorNames = [],
                    EnumPieces = [words("Error: the first argument of"),
                        quote(AtomStr), words("should be"),
                        words("a nonempty list of function symbols."), nl],
                    EnumSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(ArgTerm1),
                            [always(EnumPieces)])]),
                    EnumSpecs = [EnumSpec]
                ),
                ( if
                    EnumSpecs = [],
                    ForeignEnumSpecs = []
                then
                    (
                        EnumFunctorNames = [],
                        % If EnumFunctorTerms = [], then EnumSpecs cannot be
                        % empty, so we should not get here. If EnumFunctorTerms
                        % = [_ | _], then every element of it should turn into
                        % an element of either EnumSpecs or EnumFunctorNames,
                        % so again, we should not get here.
                        unexpected($pred, "EnumFunctorNames = []")
                    ;
                        EnumFunctorNames =
                            [HeadEnumFunctorName | TailEnumFunctorNames],
                        EnumRepn = enum_repn(
                            one_or_more(HeadEnumFunctorName,
                                TailEnumFunctorNames),
                            ForeignEnums),
                        MaybeDuRepn = ok1(dur_enum(EnumRepn))
                    )
                else
                    Specs = EnumSpecs ++ ForeignEnumSpecs,
                    MaybeDuRepn = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _, _ | _]
                ),
                Prefix = du_where_prefix(DuWhere),
                Pieces = [quote("enum"),
                    words("should have one or two arguments."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Prefix ++ Pieces)])]),
                MaybeDuRepn = error1([Spec])
            )
        ;
            AtomStr = "gen_du",
            (
                ArgTerms = [ArgTerm1],
                ( if
                    list_term_to_term_list(ArgTerm1, MaybeDuFunctorTerms),
                    MaybeDuFunctorTerms =
                        [HeadDuFunctorTerm | TailDuFunctorTerms]
                then
                    parse_du_functor(VarSet, HeadDuFunctorTerm,
                        MaybeHeadDuFunctor),
                    parse_du_functors(VarSet, TailDuFunctorTerms,
                        TailDuFunctors, TailSpecs),
                    ( if
                        MaybeHeadDuFunctor = ok1(HeadDuFunctor),
                        TailSpecs = []
                    then
                        MaybeDuRepn = ok1(dur_gen(gen_du_repn_more_functors(
                            one_or_more(HeadDuFunctor, TailDuFunctors))))
                    else
                        Specs = get_any_errors1(MaybeHeadDuFunctor) ++
                            TailSpecs,
                        MaybeDuRepn = error1(Specs)
                    )
                else
                    Pieces = [words("Error: the argument of"), quote(AtomStr),
                        words("should be a nonempty list of"),
                        words("function symbol representations."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(TermContext, [always(Pieces)])]),
                    MaybeDuRepn = error1([Spec])
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have exactly one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeDuRepn = error1([Spec])
            )
        ;
            AtomStr = "gen_du_only_functor",
            (
                ArgTerms = [ArgTerm1,
                    ArgTerm2, ArgTerm3],
                ContextPieces = [words("In first argument of"),
                    quote(AtomStr), suffix(":")],
                parse_string(VarSet, ContextPieces, "function symbol",
                    ArgTerm1, MaybeFunctorName),
                parse_only_functor_args(63u, VarSet, ArgTerm2,
                    MaybeOnlyFunctorArgs64),
                parse_only_functor_args(31u, VarSet, ArgTerm3,
                    MaybeOnlyFunctorArgs32),
                ( if
                    MaybeFunctorName = ok1(FunctorName),
                    MaybeOnlyFunctorArgs64 = ok1(OnlyFunctorArgs64),
                    MaybeOnlyFunctorArgs32 = ok1(OnlyFunctorArgs32)
                then
                    MaybeDuRepn = ok1(dur_gen(gen_du_repn_only_functor(
                        FunctorName, OnlyFunctorArgs64, OnlyFunctorArgs32)))
                else
                    Specs = get_any_errors1(MaybeFunctorName) ++
                        get_any_errors1(MaybeOnlyFunctorArgs64) ++
                        get_any_errors1(MaybeOnlyFunctorArgs32),
                    MaybeDuRepn = error1(Specs)
                )
            ;
                ( ArgTerms = []
                ; ArgTerms = [_]
                ; ArgTerms = [_, _]
                ; ArgTerms = [_, _, _, _ | _]
                ),
                Pieces = [words("Error:"), quote(AtomStr),
                    words("should have exactly three arguments."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeDuRepn = error1([Spec])
            )
        )
    else
        Prefix = du_where_prefix(DuWhere),
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Expected one of"),
            quote("notag(...)"), suffix(","),
            quote("direct_dummy(...)"), suffix(","),
            quote("enum(...)"), suffix(","),
            quote("gen_du"),  words("and"),
            quote("gen_du_only_functor"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Prefix ++ Pieces)])]),
        MaybeDuRepn = error1([Spec])
    ).

:- func du_where_prefix(du_where) = list(format_component).

du_where_prefix(DuWhere) = Prefix :-
    (
        DuWhere = direct_in_item,
        Prefix = [words("Error: malformed du type representation"),
            words("inside the only argument of"),
            quote("du_repn"), suffix("."), nl]
    ;
        DuWhere = indirect_in_maybe_foreign_type,
        Prefix = [words("Error: malformed optional du type representation"),
            words("inside the second argument of"),
            quote("maybe_foreign_type_repn"), suffix("."), nl]
    ).
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
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
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeOnlyFunctorArgs = error1([Spec])
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("local_args"), words("and"),
            quote("remote_args"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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

:- pred parse_foreign_enum_specs(varset::in, string::in, int::in,
    list(term)::in, assoc_list(foreign_language, one_or_more(string))::out,
    list(error_spec)::out) is det.

parse_foreign_enum_specs(_, _, _, [], [], []).
parse_foreign_enum_specs(VarSet, AtomStr, Nth, [Term | Terms],
        !:ForeignEnums, !:Specs) :-
    parse_foreign_enum_specs(VarSet, AtomStr, Nth + 1, Terms,
        !:ForeignEnums, !:Specs),
    ( if
        Term = term.functor(term.string(TermStr), ArgTerms, TermContext),
        simple_foreign_language_string(Lang, TermStr)
    then
        (
            ArgTerms = [ArgTerm1],
            ( if
                list_term_to_term_list(ArgTerm1, IdentTerms),
                IdentTerms = [_ | _]
            then
                ContextPieces = [words("the argument of"),
                    quote(AtomStr), suffix(":")],
                parse_strings(VarSet, ContextPieces, "enum value", 1,
                    IdentTerms, Idents, Specs),
                (
                    Specs = [],
                    (
                        Idents = [],
                        % Every element of IdentTerms should turn into an
                        % element of either Idents or Specs, so we should
                        % not get here.
                        unexpected($pred, "Idents = []")
                    ;
                        Idents = [HeadIdent | TailIdents],
                        ForeignEnum =
                            Lang - one_or_more(HeadIdent, TailIdents),
                        !:ForeignEnums = [ForeignEnum | !.ForeignEnums]
                    )
                ;
                    Specs = [_ | _],
                    !:Specs = Specs ++ !.Specs
                )
            else
                Pieces = [words("Error: the"), nth_fixed(Nth),
                    words("element"), words("of the list"),
                    words("in the second argument of"), quote(AtomStr),
                    words("should be a nonempty list,"),
                    words("with each element being a foreign language name."),
                    words("wrapped around a list of identifiers"),
                    words("in that language."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(ArgTerm1),
                        [always(Pieces)])]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            ( ArgTerms = []
            ; ArgTerms = [_, _ | _]
            ),
            Pieces = [words("Error:"), quote(TermStr),
                words("should have exactly one argument."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        )
    else
        Pieces = [words("Error: the"), nth_fixed(Nth), words("element"),
            words("of the list in the second argument of"), quote(AtomStr),
            words("is not of the required form, which is one of"),
            quote("c(...)"), suffix(","),
            quote("csharp(...)"), suffix(","),
            quote("java(...)"), words("or"),
            quote("erlang(...)"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ).

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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeDuFunctor = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("constant_functor(...)"), words("and"),
            quote("nonconstant_functor(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeSectagSize = error1([Spec])
            )
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("sectag_rest_of_word"), words("and"),
            quote("sectag_bits(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeLocalPosSize = error1([Spec])
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Expected"), quote("local(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term),
                [always(Pieces)])]),
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
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeMaybeDirectArg = error1([Spec])
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected one of"),
            quote("direct_arg(...)"), words("and"),
            quote("nondirect_arg(...)"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(TermContext, [always(Pieces)])]),
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
            DwSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ArgTerm3), [always(DwPieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(TermContext, [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(TermContext, [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(TermContext, [always(Pieces)])]),
        MaybeFillKindSize = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

:- pred parse_type_repn_maybe_foreign_type(varset::in, string::in,
    list(term)::in, term.context::in, maybe1(type_ctor_repn_info)::out)
    is det.

parse_type_repn_maybe_foreign_type(VarSet, RepnStr, RepnArgs, RepnContext,
        MaybeRepn) :-
    (
        RepnArgs = [RepnArg1, RepnArg2],
        ( if
            list_term_to_term_list(RepnArg1, ForeignTerms),
            ForeignTerms = [_ | _]
        then
            parse_foreign_language_types(VarSet, RepnStr, 1, ForeignTerms,
                ForeignPairs, ForeignSpecs)
        else
            ForeignPairs = [],
            ForeignPieces = [words("Error: the first argument of"),
                quote(RepnStr), words("should be a nonempty list of"),
                words("foreign language names wrapped around type names."),
                nl],
            ForeignSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(RepnArg1),
                    [always(ForeignPieces)])]),
            ForeignSpecs = [ForeignSpec]
        ),
        parse_maybe_du_repn(VarSet, RepnStr, RepnArg2, MaybeMaybeDuRepn),
        ( if
            ForeignSpecs = [],
            MaybeMaybeDuRepn = ok1(MaybeDuRepn)
        then
            (
                ForeignPairs = [],
                % If ForeignTerms = [], then ForeignSpecs cannot be empty,
                % so we should not get here. If ForeignTerms = [_ | _],
                % then every element of it should turn into an element of
                % either ForeignSpecs or ForeignPairs, so again, we should
                % not get here.
                unexpected($pred, "ForeignPairs = []")
            ;
                ForeignPairs = [HeadForeignPair | TailForeignPairs],
                Repn = tcrepn_maybe_foreign(
                    one_or_more(HeadForeignPair, TailForeignPairs),
                    MaybeDuRepn),
                MaybeRepn = ok1(Repn)
            )
        else
            Specs = ForeignSpecs ++ get_any_errors1(MaybeMaybeDuRepn),
            MaybeRepn = error1(Specs)
        )
    ;
        ( RepnArgs = []
        ; RepnArgs = [_]
        ; RepnArgs = [_, _, _ | _]
        ),
        Pieces = [words("Error:"), quote(RepnStr),
            words("should have exactly two arguments,"),
            words("the first being a list of foreign language names"),
            words("wrapped around type names,"),
            words("and the second being a description of the representation"),
            words("of the type's Mercury implementation, if any."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(RepnContext, [always(Pieces)])]),
        MaybeRepn = error1([Spec])
    ).

:- pred parse_foreign_language_types(varset::in, string::in, int::in,
    list(term)::in, assoc_list(foreign_language, foreign_type_repn)::out,
    list(error_spec)::out) is det.

parse_foreign_language_types(_, _, _, [], [], []).
parse_foreign_language_types(VarSet, RepnStr, Nth, [Term | Terms],
        !:ForeignPairs, !:Specs) :-
    parse_foreign_language_types(VarSet, RepnStr, Nth + 1, Terms,
        !:ForeignPairs, !:Specs),
    ( if
        Term = term.functor(term.atom(FunctorStr), ArgTerms, _),
        (
            ( FunctorStr = "c", Lang = lang_c
            ; FunctorStr = "csharp", Lang = lang_csharp
            ; FunctorStr = "java", Lang = lang_java
            ),
            ArgTerms = [TypeNameTerm, AssertionTerm]
        ;
            FunctorStr = "erlang",
            Lang = lang_erlang,
            ArgTerms = [AssertionTerm],
            TypeNameTerm = term.functor(term.string(""), [], term.context_init)
        )
    then
        ( if TypeNameTerm = term.functor(term.string(TypeName0), [], _) then
            MaybeTypeName = ok1(TypeName0)
        else
            TypeNameTermStr = describe_error_term(VarSet, TypeNameTerm),
            TypeNamePieces = [words("Error: the type name in the"),
                nth_fixed(Nth), words("element of the list"),
                words("in the first argument of"), quote(RepnStr),
                words("is"), quote(TypeNameTermStr), suffix(","),
                words("which is not a valid type name."), nl],
            TypeNameSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(TypeNameTerm),
                    [always(TypeNamePieces)])]),
            MaybeTypeName = error1([TypeNameSpec])
        ),
        AssertionContextPieces = cord.from_list([
            words("In third argument of the"), nth_fixed(Nth),
            words("element of the list in the first argument of"),
            quote(RepnStr), suffix(":")]),
        parse_foreign_type_assertions(AssertionContextPieces, VarSet,
            AssertionTerm, set.init, AssertionSet,
            [], AssertionSpecs),
        ( if
            MaybeTypeName = ok1(TypeName),
            AssertionSpecs = []
        then
            Assertions = foreign_type_assertions(AssertionSet),
            Repn = foreign_type_repn(TypeName, Assertions),
            !:ForeignPairs = [Lang - Repn | !.ForeignPairs]
        else
            !:Specs = get_any_errors1(MaybeTypeName) ++ AssertionSpecs
                ++ !.Specs
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: the"), nth_fixed(Nth), words("element"),
            words("of the list in the first argument of"), quote(RepnStr),
            words("is"), quote(TermStr), suffix("."),
            words("This is not in one of the required forms, which are"),
            quote("c(c_type_name, assertionslist)"), suffix(","),
            quote("csharp(csharp_type_name, assertionslist)"), suffix(","),
            quote("java(java_type_name, assertionslist)"), words("and"),
            quote("erlang(assertionslist)"),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred parse_maybe_du_repn(varset::in, string::in, term::in,
    maybe1(maybe(du_repn))::out) is det.

parse_maybe_du_repn(VarSet, RepnStr, Term, MaybeMaybeDuRepn) :-
    ( if
        Term = term.functor(term.atom(TermStr), [], _),
        TermStr = "no_du_repn"
    then
        MaybeMaybeDuRepn = ok1(no)
    else if
        Term = term.functor(term.atom(TermStr), [ArgTerm], TermContext),
        TermStr = "du_repn"
    then
        parse_type_repn_du(VarSet, indirect_in_maybe_foreign_type, ArgTerm,
            TermContext, MaybeDuRepn),
        (
            MaybeDuRepn = ok1(DuRepn),
            MaybeMaybeDuRepn = ok1(yes(DuRepn))
        ;
            MaybeDuRepn = error1(Specs),
            MaybeMaybeDuRepn = error1(Specs)
        )
    else
        Pieces = [words("Error: the second argument of"),
            quote(RepnStr), words("should be either"),
            quote("no_du_repn"), words("with no arguments,"),
            words("or"), quote("du_repn"), words("with one argument."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeMaybeDuRepn = error1([Spec])
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeUint = error1([Spec])
    ).

%-----------------------------------------------------------------------------e
:- end_module parse_tree.parse_type_repn.
%-----------------------------------------------------------------------------e
