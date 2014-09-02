%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_mutable.m.
%
% This module defines predicates for parsing the parts of Mercury programs
% relating to initialise, finalise and mutable declarations.

:- module parse_tree.prog_io_mutable.

:- interface.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module term.
:- import_module varset.

:- pred parse_initialise_decl(module_name::in, varset::in, term::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

:- pred parse_finalise_decl(module_name::in, varset::in, term::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

:- pred parse_mutable_decl(module_name::in, varset::in, list(term)::in,
    prog_context::in, int::in, maybe1(item)::out) is semidet.

:- pred parse_mutable_decl_info(module_name::in, varset::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_mutable_info)::out) is semidet.

%-----------------------------------------------------------------------------e

:- implementation.

:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_pragma.
:- import_module parse_tree.prog_io_sym_name.

:- import_module bool.
:- import_module pair.

%-----------------------------------------------------------------------------e

parse_initialise_decl(_ModuleName, VarSet, Term, Context, SeqNum, MaybeItem) :-
    parse_symbol_name_specifier(VarSet, Term, MaybeSymNameSpecifier),
    (
        MaybeSymNameSpecifier = error1(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeSymNameSpecifier = ok1(SymNameSpecifier),
        (
            SymNameSpecifier = name(_),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error:"), decl("initialise"),
                words("declaration"), words("requires arity, found"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        ;
            SymNameSpecifier = name_arity(SymName, Arity),
            ( ( Arity = 0 ; Arity = 2 ) ->
                ItemInitialise = item_initialise_info(user, SymName, Arity,
                    Context, SeqNum),
                Item = item_initialise(ItemInitialise),
                MaybeItem = ok1(Item)
            ;
                TermStr = describe_error_term(VarSet, Term),
                Pieces = [words("Error:"), decl("initialise"),
                    words("declaration specifies a predicate"),
                    words("whose arity is not zero or two:"),
                    fixed(TermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        )
    ).

%-----------------------------------------------------------------------------%

parse_finalise_decl(_ModuleName, VarSet, Term, Context, SeqNum, MaybeItem) :-
    parse_symbol_name_specifier(VarSet, Term, MaybeSymNameSpecifier),
    (
        MaybeSymNameSpecifier = error1(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeSymNameSpecifier = ok1(SymNameSpecifier),
        (
            SymNameSpecifier = name(_),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error:"), decl("finalise"),
                words("declaration"), words("requires arity, found"),
                fixed(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        ;
            SymNameSpecifier = name_arity(SymName, Arity),
            ( ( Arity = 0 ; Arity = 2 ) ->
                ItemFinalise = item_finalise_info(user, SymName, Arity,
                    Context, SeqNum),
                Item = item_finalise(ItemFinalise),
                MaybeItem = ok1(Item)
            ;
                TermStr = describe_error_term(VarSet, Term),
                Pieces = [words("Error:"), decl("finalise"),
                    words("declaration specifies a predicate"),
                    words("whose arity is not zero or two:"),
                    words(TermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        )
    ).

parse_mutable_decl(ModuleName, VarSet, Terms, Context, SeqNum, MaybeItem) :-
    parse_mutable_decl_info(ModuleName, VarSet, Terms, Context, SeqNum,
        MaybeMutableInfo),
    (
        MaybeMutableInfo = ok1(MutableInfo),
        MaybeItem = ok1(item_mutable(MutableInfo))
    ;
        MaybeMutableInfo = error1(Specs),
        MaybeItem = error1(Specs)
    ).

parse_mutable_decl_info(_ModuleName, VarSet, Terms, Context, SeqNum,
        MaybeMutableInfo) :-
    Terms = [NameTerm, TypeTerm, ValueTerm, InstTerm | OptMutAttrsTerm],
    parse_mutable_name(NameTerm, MaybeName),
    parse_mutable_type(VarSet, TypeTerm, MaybeType),
    term.coerce(ValueTerm, Value),
    varset.coerce(VarSet, ProgVarSet),
    parse_mutable_inst(VarSet, InstTerm, MaybeInst),

    % The list of attributes is optional.
    (
        OptMutAttrsTerm = [],
        MaybeMutAttrs = ok1(default_mutable_attributes)
    ;
        OptMutAttrsTerm = [MutAttrsTerm],
        parse_mutable_attrs(VarSet, MutAttrsTerm, MaybeMutAttrs)
    ),
    (
        MaybeName = ok1(Name),
        MaybeType = ok1(Type),
        MaybeInst = ok1(Inst),
        MaybeMutAttrs = ok1(MutAttrs)
    ->
        % We *must* attach the varset to the mutable item because if the
        % initial value is non-ground, then the initial value will be a
        % variable and the mutable initialisation predicate will contain
        % references to it. Ignoring the varset may lead to later compiler
        % passes attempting to reuse this variable when fresh variables are
        % allocated.
        MutableInfo = item_mutable_info(Name, Type, Value, Inst, MutAttrs,
            ProgVarSet, Context, SeqNum),
        MaybeMutableInfo = ok1(MutableInfo)
    ;
        Specs = get_any_errors1(MaybeName) ++ get_any_errors1(MaybeType) ++
            get_any_errors1(MaybeInst) ++ get_any_errors1(MaybeMutAttrs),
        MaybeMutableInfo = error1(Specs)
    ).

:- pred parse_mutable_name(term::in, maybe1(string)::out) is det.

parse_mutable_name(NameTerm, MaybeName) :-
    ( NameTerm = term.functor(atom(Name), [], _) ->
        MaybeName = ok1(Name)
    ;
        Pieces = [words("Error: invalid mutable name."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(NameTerm), [always(Pieces)])]),
        MaybeName = error1([Spec])
    ).

:- pred parse_mutable_type(varset::in, term::in, maybe1(mer_type)::out) is det.

parse_mutable_type(VarSet, TypeTerm, MaybeType) :-
    ( term.contains_var(TypeTerm, _) ->
        TypeTermStr = describe_error_term(VarSet, TypeTerm),
        Pieces = [words("Error: the type in a"), decl("mutable"),
            words("declaration cannot contain variables:"),
            words(TypeTermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(TypeTerm), [always(Pieces)])]),
        MaybeType = error1([Spec])
    ;
        ContextPieces = [],
        parse_type(TypeTerm, VarSet, ContextPieces, MaybeType)
    ).

:- pred parse_mutable_inst(varset::in, term::in, maybe1(mer_inst)::out) is det.

parse_mutable_inst(VarSet, InstTerm, MaybeInst) :-
    ( term.contains_var(InstTerm, _) ->
        InstTermStr = describe_error_term(VarSet, InstTerm),
        Pieces = [words("Error: the inst in a"), decl("mutable"),
            words("declaration cannot contain variables:"),
            words(InstTermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(InstTerm), [always(Pieces)])]),
        MaybeInst = error1([Spec])
    ; convert_inst(no_allow_constrained_inst_var, InstTerm, Inst) ->
        MaybeInst = ok1(Inst)
    ;
        Pieces = [words("Error: invalid inst in"), decl("mutable"),
            words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(InstTerm), [always(Pieces)])]),
        MaybeInst = error1([Spec])
    ).

:- type collected_mutable_attribute
    --->    mutable_attr_trailed(mutable_trailed)
    ;       mutable_attr_foreign_name(foreign_name)
    ;       mutable_attr_attach_to_io_state(bool)
    ;       mutable_attr_constant(bool)
    ;       mutable_attr_thread_local(mutable_thread_local).

:- pred parse_mutable_attrs(varset::in, term::in,
    maybe1(mutable_var_attributes)::out) is det.

parse_mutable_attrs(VarSet, MutAttrsTerm, MaybeMutAttrs) :-
    Attributes0 = default_mutable_attributes,
    ConflictingAttributes = [
        mutable_attr_trailed(mutable_trailed) -
            mutable_attr_trailed(mutable_untrailed),
        mutable_attr_trailed(mutable_trailed) -
            mutable_attr_thread_local(mutable_thread_local),
        mutable_attr_constant(yes) - mutable_attr_trailed(mutable_trailed),
        mutable_attr_constant(yes) - mutable_attr_attach_to_io_state(yes),
        mutable_attr_constant(yes) -
            mutable_attr_thread_local(mutable_thread_local)
    ],
    (
        list_term_to_term_list(MutAttrsTerm, MutAttrTerms),
        map_parser(parse_mutable_attr, MutAttrTerms, MaybeAttrList),
        MaybeAttrList = ok1(CollectedMutAttrs)
    ->
        % We check for trailed/untrailed, constant/trailed,
        % trailed/thread_local, constant/attach_to_io_state,
        % constant/thread_local conflicts here and deal with conflicting
        % foreign_name attributes in make_hlds_passes.m.
        (
            list.member(Conflict1 - Conflict2, ConflictingAttributes),
            list.member(Conflict1, CollectedMutAttrs),
            list.member(Conflict2, CollectedMutAttrs)
        ->
            % XXX Should generate more specific error message.
            MutAttrsStr = mercury_term_to_string(VarSet, no, MutAttrsTerm),
            Pieces = [words("Error: conflicting attributes"),
                words("in attribute list:"), nl,
                words(MutAttrsStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(MutAttrsTerm),
                    [always(Pieces)])]),
            MaybeMutAttrs = error1([Spec])
        ;
            list.foldl(process_mutable_attribute, CollectedMutAttrs,
                Attributes0, Attributes),
            MaybeMutAttrs = ok1(Attributes)
        )
    ;
        MutAttrsStr = mercury_term_to_string(VarSet, no, MutAttrsTerm),
        Pieces = [words("Error: malformed attribute list"),
            words("in"), decl("mutable"), words("declaration:"),
            words(MutAttrsStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(MutAttrsTerm), [always(Pieces)])]),
        MaybeMutAttrs = error1([Spec])
    ).

:- pred process_mutable_attribute(collected_mutable_attribute::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

process_mutable_attribute(mutable_attr_trailed(Trailed), !Attributes) :-
    set_mutable_var_trailed(Trailed, !Attributes).
process_mutable_attribute(mutable_attr_foreign_name(ForeignName),
        !Attributes) :-
    set_mutable_add_foreign_name(ForeignName, !Attributes).
process_mutable_attribute(mutable_attr_attach_to_io_state(AttachToIOState),
        !Attributes) :-
    set_mutable_var_attach_to_io_state(AttachToIOState, !Attributes).
process_mutable_attribute(mutable_attr_constant(Constant), !Attributes) :-
    set_mutable_var_constant(Constant, !Attributes),
    (
        Constant = yes,
        set_mutable_var_trailed(mutable_untrailed, !Attributes),
        set_mutable_var_attach_to_io_state(no, !Attributes)
    ;
        Constant = no
    ).
process_mutable_attribute(mutable_attr_thread_local(ThrLocal), !Attributes) :-
    set_mutable_var_thread_local(ThrLocal, !Attributes).

:- pred parse_mutable_attr(term::in,
    maybe1(collected_mutable_attribute)::out) is det.

parse_mutable_attr(MutAttrTerm, MutAttrResult) :-
    (
        MutAttrTerm = term.functor(term.atom(String), [], _),
        (
            String  = "untrailed",
            MutAttr = mutable_attr_trailed(mutable_untrailed)
        ;
            String = "trailed",
            MutAttr = mutable_attr_trailed(mutable_trailed)
        ;
            String  = "attach_to_io_state",
            MutAttr = mutable_attr_attach_to_io_state(yes)
        ;
            String = "constant",
            MutAttr = mutable_attr_constant(yes)
        ;
            String = "thread_local",
            MutAttr = mutable_attr_thread_local(mutable_thread_local)
        )
    ->
        MutAttrResult = ok1(MutAttr)
    ;
        MutAttrTerm = term.functor(term.atom("foreign_name"), Args, _),
        Args = [LangTerm, ForeignNameTerm],
        parse_foreign_language(LangTerm, Lang),
        ForeignNameTerm = term.functor(term.string(ForeignName), [], _)
    ->
        MutAttr = mutable_attr_foreign_name(foreign_name(Lang, ForeignName)),
        MutAttrResult = ok1(MutAttr)
    ;
        Pieces = [words("Error: unrecognised attribute"),
            words("in"), decl("mutable"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(MutAttrTerm), [always(Pieces)])]),
        MutAttrResult = error1([Spec])
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_mutable.
%-----------------------------------------------------------------------------%
