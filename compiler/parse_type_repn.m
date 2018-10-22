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

:- import_module parse_tree.error_util.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_defn.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module int.

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
            ; AtomStr = "is_equivalent_to"
            ; AtomStr = "fits_in_n_bits"
            ; AtomStr = "has_direct_arg_functors"
            )
        then
            (
                AtomStr = "is_direct_dummy",
                parse_type_repn_direct_dummy(AtomStr, RepnArgs,
                    RepnContext, MaybeRepn)
            ;
                AtomStr = "is_notag",
                parse_type_repn_notag(AtomStr, RepnArgs,
                    RepnContext, MaybeRepn)
            ;
                AtomStr = "is_equivalent_to",
                parse_type_repn_equivalent_to(VarSet, AtomStr, RepnArgs,
                    RepnContext, MaybeRepn)
            ;
                AtomStr = "fits_in_n_bits",
                parse_type_repn_fits_in_n_bits(AtomStr, RepnArgs,
                    RepnContext, MaybeRepn)
            ;
                AtomStr = "has_direct_arg_functors",
                parse_type_repn_has_direct_arg_functors(AtomStr, RepnArgs,
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
                quote("is_equivalent_to"), suffix(","),
                quote("fits_in_n_bits"), words("and"),
                quote("has_direct_arg_functors"), suffix(","),
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

:- pred parse_type_repn_direct_dummy(string::in, list(term)::in,
    term.context::in, maybe1(type_ctor_repn_info)::out) is det.

parse_type_repn_direct_dummy(RepnStr, RepnArgs, RepnContext, MaybeRepn) :-
    (
        RepnArgs = [],
        MaybeRepn = ok1(tcrepn_is_direct_dummy)
    ;
        RepnArgs = [_ | _],
        Pieces = [words("Error:"), quote(RepnStr), 
            words("should not have any arguments."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(RepnContext, [always(Pieces)])]),
        MaybeRepn = error1([Spec])
    ).

:- pred parse_type_repn_notag(string::in, list(term)::in, term.context::in,
    maybe1(type_ctor_repn_info)::out) is det.

parse_type_repn_notag(RepnStr, RepnArgs, RepnContext, MaybeRepn) :-
    (
        RepnArgs = [],
        MaybeRepn = ok1(tcrepn_is_notag)
    ;
        RepnArgs = [_ | _],
        Pieces = [words("Error:"), quote(RepnStr), 
            words("should not have any arguments."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(RepnContext, [always(Pieces)])]),
        MaybeRepn = error1([Spec])
    ).

:- pred parse_type_repn_equivalent_to(varset::in, string::in, list(term)::in,
    term.context::in, maybe1(type_ctor_repn_info)::out) is det.

parse_type_repn_equivalent_to(VarSet, RepnStr, RepnArgs, RepnContext,
        MaybeRepn) :-
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

:- pred parse_type_repn_fits_in_n_bits(string::in, list(term)::in,
    term.context::in, maybe1(type_ctor_repn_info)::out) is det.

parse_type_repn_fits_in_n_bits(RepnStr, RepnArgs, RepnContext, MaybeRepn) :-
    (
        RepnArgs = [RepnArg],
        ( if decimal_term_to_int(RepnArg, N) then
            MaybeRepn = ok1(tcrepn_fits_in_n_bits(N))
        else
            Pieces = [words("Error: the argument of"), quote(RepnStr),
                words("should be an integer."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(RepnArg), [always(Pieces)])]),
            MaybeRepn = error1([Spec])
        )
    ;
        ( RepnArgs = []
        ; RepnArgs = [_, _ | _]
        ),
        Pieces = [words("Error:"), quote(RepnStr), 
            words("should have exactly one argument, an integer."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(RepnContext, [always(Pieces)])]),
        MaybeRepn = error1([Spec])
    ).

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
:- end_module parse_tree.parse_type_repn.
%-----------------------------------------------------------------------------e
