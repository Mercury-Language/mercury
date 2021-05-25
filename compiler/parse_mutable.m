%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_mutable.m.
%
% This module defines predicates for parsing the parts of Mercury programs
% relating to initialise, finalise and mutable declarations.

:- module parse_tree.parse_mutable.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module term.
:- import_module varset.

:- pred parse_initialise_item(module_name::in, varset::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

:- pred parse_finalise_item(module_name::in, varset::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

:- pred parse_mutable_item(module_name::in, varset::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

:- type mutable_locn
    --->    mutable_locn_item
            % The mutable is an item of its own.
    ;       mutable_locn_in_solver_type.
            % The mutable is part of an item that defines a solver type.

:- pred parse_mutable_decl_info(module_name::in, varset::in, list(term)::in,
    prog_context::in, item_seq_num::in, mutable_locn::in,
    maybe1(item_mutable_info)::out) is det.

%-----------------------------------------------------------------------------e

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.error_util.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_pragma_foreign.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.

:- import_module assoc_list.
:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module unit.

%-----------------------------------------------------------------------------e

parse_initialise_item(_ModuleName, VarSet, ArgTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if ArgTerms = [Term] then
        parse_symbol_name_specifier(VarSet, Term, MaybeSymNameSpecifier),
        (
            MaybeSymNameSpecifier = error1(Specs),
            MaybeIOM = error1(Specs)
        ;
            MaybeSymNameSpecifier = ok1(SymNameSpecifier),
            (
                SymNameSpecifier = sym_name_specifier_name(_),
                TermStr = describe_error_term(VarSet, Term),
                Pieces = [words("Error in"), decl("initialise"),
                    words("declaration:"),
                    words("expected"), quote("predname/arity"), suffix(","),
                    words("got"), quote(TermStr), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Term), Pieces),
                MaybeIOM = error1([Spec])
            ;
                SymNameSpecifier =
                    sym_name_specifier_name_arity(SymName, Arity),
                ( if ( Arity = 0 ; Arity = 2 ) then
                    ItemInitialise = item_initialise_info(SymName, Arity,
                        item_origin_user, Context, SeqNum),
                    Item = item_initialise(ItemInitialise),
                    MaybeIOM = ok1(iom_item(Item))
                else
                    TermStr = describe_error_term(VarSet, Term),
                    Pieces = [words("Error:"), decl("initialise"),
                        words("declaration specifies a predicate,"),
                        quote(TermStr), suffix(","), words("whose arity"),
                        words("is not zero or two."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(Term), Pieces),
                    MaybeIOM = error1([Spec])
                )
            )
        )
    else
        Pieces = [words("Error: an"), decl("initialise"), words("declaration"),
            words("should have the form"),
            quote(":- initialise pred_name/pred_arity."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_finalise_item(_ModuleName, VarSet, ArgTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if ArgTerms = [Term] then
        parse_symbol_name_specifier(VarSet, Term, MaybeSymNameSpecifier),
        (
            MaybeSymNameSpecifier = error1(Specs),
            MaybeIOM = error1(Specs)
        ;
            MaybeSymNameSpecifier = ok1(SymNameSpecifier),
            (
                SymNameSpecifier = sym_name_specifier_name(_),
                TermStr = describe_error_term(VarSet, Term),
                Pieces = [words("Error in"), decl("finalise"),
                    words("declaration:"),
                    words("expected"), quote("predname/arity"), suffix(","),
                    words("got"), quote(TermStr), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Term), Pieces),
                MaybeIOM = error1([Spec])
            ;
                SymNameSpecifier =
                    sym_name_specifier_name_arity(SymName, Arity),
                ( if ( Arity = 0 ; Arity = 2 ) then
                    ItemFinalise = item_finalise_info(SymName, Arity,
                        item_origin_user, Context, SeqNum),
                    Item = item_finalise(ItemFinalise),
                    MaybeIOM = ok1(iom_item(Item))
                else
                    TermStr = describe_error_term(VarSet, Term),
                    Pieces = [words("Error:"), decl("finalise"),
                        words("declaration specifies a predicate"),
                        words("whose arity is not zero or two:"),
                        quote(TermStr), suffix("."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(Term), Pieces),
                    MaybeIOM = error1([Spec])
                )
            )
        )
    else
        Pieces = [words("Error: a"), decl("finalise"), words("declaration"),
            words("should have the form"),
            quote(":- finalise pred_name/pred_arity."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_mutable_item(ModuleName, VarSet, ArgTerms, Context, SeqNum, MaybeIOM) :-
    parse_mutable_decl_info(ModuleName, VarSet, ArgTerms, Context, SeqNum,
        mutable_locn_item, MaybeItemMutableInfo),
    (
        MaybeItemMutableInfo = ok1(ItemMutableInfo),
        MaybeIOM = ok1(iom_item(item_mutable(ItemMutableInfo)))
    ;
        MaybeItemMutableInfo = error1(Specs),
        MaybeIOM = error1(Specs)
    ).

%---------------------------------------------------------------------------%

parse_mutable_decl_info(_ModuleName, VarSet, ArgTerms, Context, SeqNum,
        MutableLocn, MaybeItemMutableInfo) :-
    ( if
        ArgTerms = [NameTerm, TypeTerm, ValueTerm, InstTerm | OptMutAttrsTerm],
        % The list of attributes is optional.
        (
            OptMutAttrsTerm = [],
            MaybeAttrsTerm = no
        ;
            OptMutAttrsTerm = [MutAttrsTerm0],
            MaybeAttrsTerm = yes(MutAttrsTerm0)
        )
    then
        parse_mutable_name(NameTerm, MaybeName),
        parse_mutable_type(VarSet, TypeTerm, MaybeType),
        term.coerce(ValueTerm, Value),
        varset.coerce(VarSet, ProgVarSet),
        parse_mutable_inst(VarSet, InstTerm, MaybeInst),

        (
            MaybeAttrsTerm = no,
            MaybeMutAttrs = ok1(default_mutable_attributes)
        ;
            MaybeAttrsTerm = yes(MutAttrsTerm),
            parse_mutable_attrs(VarSet, MutAttrsTerm, MaybeMutAttrs)
        ),
        ( if
            MaybeName = ok1(Name),
            MaybeType = ok1(Type),
            MaybeInst = ok1(Inst),
            MaybeMutAttrs = ok1(MutAttrs)
        then
            % We *must* attach the varset to the mutable item because if the
            % initial value is non-ground, then the initial value will be a
            % variable and the mutable initialisation predicate will contain
            % references to it. Ignoring the varset may lead to later compiler
            % passes attempting to reuse this variable when fresh variables are
            % allocated.
            ItemMutableInfo = item_mutable_info(Name, Type, Type, Inst, Inst,
                Value, ProgVarSet, MutAttrs, Context, SeqNum),
            MaybeItemMutableInfo = ok1(ItemMutableInfo)
        else
            Specs = get_any_errors1(MaybeName) ++ get_any_errors1(MaybeType) ++
                get_any_errors1(MaybeInst) ++ get_any_errors1(MaybeMutAttrs),
            MaybeItemMutableInfo = error1(Specs)
        )
    else
        Form1 = "mutable(name, type, init_value, inst)",
        Form2 = "mutable(name, type, init_value, inst, [attr1, ...])",
        (
            MutableLocn = mutable_locn_item,
            WhatPieces = [words("a"), decl("mutable"), words("declaration")],
            Prefix = ":- ",
            Suffix1 = "."
        ;
            MutableLocn = mutable_locn_in_solver_type,
            WhatPieces = [words("a"), decl("mutable"),
                words("representing part of the constraint store")],
            Prefix = "",
            Suffix1 = ""
        ),
        Pieces = [words("Error:") | WhatPieces] ++
            [words("should have the form"), quote(Prefix ++ Form1 ++ Suffix1),
            words("or the form"), quote(Prefix ++ Form2 ++ "."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeItemMutableInfo = error1([Spec])
    ).

:- pred parse_mutable_name(term::in, maybe1(string)::out) is det.

parse_mutable_name(NameTerm, MaybeName) :-
    ( if NameTerm = term.functor(atom(Name), [], _) then
        MaybeName = ok1(Name)
    else
        Pieces = [words("Error: invalid mutable name."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(NameTerm), Pieces),
        MaybeName = error1([Spec])
    ).

:- pred parse_mutable_type(varset::in, term::in, maybe1(mer_type)::out) is det.

parse_mutable_type(VarSet, TypeTerm, MaybeType) :-
    ( if term.contains_var(TypeTerm, _) then
        TypeTermStr = describe_error_term(VarSet, TypeTerm),
        Pieces = [words("Error: the type in a"), decl("mutable"),
            words("declaration may not contain variables, but"),
            quote(TypeTermStr), words("does."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(TypeTerm), Pieces),
        MaybeType = error1([Spec])
    else
        ContextPieces = cord.init,
        parse_type(no_allow_ho_inst_info(wnhii_mutable_decl),
            VarSet, ContextPieces, TypeTerm, MaybeType)
    ).

:- pred parse_mutable_inst(varset::in, term::in, maybe1(mer_inst)::out) is det.

parse_mutable_inst(VarSet, InstTerm, MaybeInst) :-
    % XXX We should check whether the *inst* contains variables, not whether
    % the *term* does, but (a) inst_contains_inst_var is in inst_match.m,
    % not in inst_util.m, and (b) it is not exported.
    ( if term.contains_var(InstTerm, _) then
        InstTermStr = describe_error_term(VarSet, InstTerm),
        Pieces = [words("Error: the inst in a"), decl("mutable"),
            words("declaration cannot contain variables:"),
            quote(InstTermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(InstTerm), Pieces),
        MaybeInst = error1([Spec])
    else
        ContextPieces = cord.from_list([words("In a"), decl("mutable"),
            words("declaration:")]),
        parse_inst(no_allow_constrained_inst_var(wnciv_mutable_inst),
            VarSet, ContextPieces, InstTerm, MaybeInst)
    ).

:- type collected_mutable_attribute
    --->    mutable_attr_trailed(mutable_trailed)
    ;       mutable_attr_foreign_name(foreign_name)
    ;       mutable_attr_attach_to_io_state     % mutable_attach_to_io_state
    ;       mutable_attr_constant               % mutable_constant
    ;       mutable_attr_thread_local.          % mutable_thread_local.

    % Has the user specified a name for us to use on the target code side
    % of the FLI?
    %
:- type foreign_name
    --->    foreign_name(
                foreign_name_lang :: foreign_language,
                foreign_name_name :: string
            ).

:- pred parse_mutable_attrs(varset::in, term::in,
    maybe1(mutable_var_attributes)::out) is det.

parse_mutable_attrs(VarSet, MutAttrsTerm, MaybeMutAttrs) :-
    ( if list_term_to_term_list(MutAttrsTerm, MutAttrTerms) then
        map_parser(parse_mutable_attr(VarSet), MutAttrTerms, MaybeAttrList),
        (
            MaybeAttrList = ok1(CollectedMutAttrPairs),
            record_mutable_attributes(VarSet, CollectedMutAttrPairs,
                map.init, LangMap,
                maybe.no, MaybeTrailed, maybe.no, MaybeConstant,
                maybe.no, MaybeIO, maybe.no, MaybeLocal, [], RecordSpecs),
            (
                RecordSpecs = [_ | _],
                MaybeMutAttrs = error1(RecordSpecs)
            ;
                RecordSpecs = [],
                OnlyLangMap =
                    map.map_values_only((func(_ - Name) = Name), LangMap),
                check_attribute_fit(VarSet, OnlyLangMap,
                    MaybeTrailed, MaybeConstant, MaybeIO, MaybeLocal,
                    MaybeMutAttrs)
            )
        ;
            MaybeAttrList = error1(Specs),
            MaybeMutAttrs = error1(Specs)
        )
    else
        MutAttrsStr = mercury_term_to_string(VarSet, print_name_only,
            MutAttrsTerm),
        Pieces = [words("In fifth argument of"),
            decl("mutable"), words("declaration:"),
            words("error: expected a list of attributes, got"),
            quote(MutAttrsStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(MutAttrsTerm), Pieces),
        MaybeMutAttrs = error1([Spec])
    ).

:- pred record_mutable_attributes(varset::in,
    assoc_list(term, collected_mutable_attribute)::in,
    map(foreign_language, pair(term, string))::in,
    map(foreign_language, pair(term, string))::out,
    maybe(pair(term, mutable_trailed))::in,
    maybe(pair(term, mutable_trailed))::out,
    maybe(pair(term, unit))::in,
    maybe(pair(term, unit))::out,
    maybe(pair(term, unit))::in,
    maybe(pair(term, unit))::out,
    maybe(pair(term, unit))::in,
    maybe(pair(term, unit))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

record_mutable_attributes(_VarSet, [], !LangMap,
        !MaybeTrailed, !MaybeConstant, !MaybeIO, !MaybeLocal, !Specs).
record_mutable_attributes(VarSet, [Term - Attr | TermAttrs], !LangMap,
        !MaybeTrailed, !MaybeConstant, !MaybeIO, !MaybeLocal, !Specs) :-
    (
        Attr = mutable_attr_foreign_name(ForeignName),
        ForeignName = foreign_name(Lang, Name),
        ( if map.search(!.LangMap, Lang, Term0 - _Name0) then
            TermStr0 = mercury_term_to_string(VarSet, print_name_only, Term0),
            TermStr = mercury_term_to_string(VarSet, print_name_only, Term),
            Pieces = [words("Error: attributes"), quote(TermStr0),
                words("and"), quote(TermStr), words("conflict."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            !:Specs = [Spec | !.Specs]
        else
            map.det_insert(Lang, Term - Name, !LangMap)
        )
    ;
        Attr = mutable_attr_trailed(Trailed),
        (
            !.MaybeTrailed = no,
            !:MaybeTrailed = yes(Term - Trailed)
        ;
            !.MaybeTrailed = yes(Term0 - Trailed0),
            report_repeated_or_conflicting_attributes(VarSet,
                Term0, Trailed0, Term, Trailed, !Specs)
        )
    ;
        Attr = mutable_attr_constant,
        (
            !.MaybeConstant = no,
            !:MaybeConstant = yes(Term - unit)
        ;
            !.MaybeConstant = yes(Term0 - _),
            report_repeated_or_conflicting_attributes(VarSet,
                Term0, unit, Term, unit, !Specs)
        )
    ;
        Attr = mutable_attr_attach_to_io_state,
        (
            !.MaybeIO = no,
            !:MaybeIO = yes(Term - unit)
        ;
            !.MaybeIO = yes(Term0 - _),
            report_repeated_or_conflicting_attributes(VarSet,
                Term0, unit, Term, unit, !Specs)
        )
    ;
        Attr = mutable_attr_thread_local,
        (
            !.MaybeLocal = no,
            !:MaybeLocal = yes(Term - unit)
        ;
            !.MaybeLocal = yes(Term0 - _),
            report_repeated_or_conflicting_attributes(VarSet,
                Term0, unit, Term, unit, !Specs)
        )
    ),
    record_mutable_attributes(VarSet, TermAttrs, !LangMap,
        !MaybeTrailed, !MaybeConstant, !MaybeIO, !MaybeLocal, !Specs).

:- pred check_attribute_fit(varset::in,
    map(foreign_language, string)::in,
    maybe(pair(term, mutable_trailed))::in,
    maybe(pair(term, unit))::in,
    maybe(pair(term, unit))::in,
    maybe(pair(term, unit))::in,
    maybe1(mutable_var_attributes)::out) is det.

check_attribute_fit(VarSet, OnlyLangMap, MaybeTrailed, MaybeConst, MaybeIO,
        MaybeLocal, MaybeMutAttrs) :-
    some [!Specs] (
        !:Specs = [],
        (
            MaybeConst = no,
            (
                MaybeIO = no,
                IO = mutable_dont_attach_to_io_state
            ;
                MaybeIO = yes(_ - unit),
                IO = mutable_attach_to_io_state
            ),
            (
                MaybeLocal = yes(LocalTerm - unit),
                Local = mutable_is_thread_local, % implicitly mutable_untrailed
                (
                    MaybeTrailed = yes(_TrailTerm - mutable_untrailed)
                ;
                    MaybeTrailed = yes(TrailTerm - mutable_trailed),
                    % Local is wrong, but will not be used due to !:Specs.
                    report_conflicting_attributes(VarSet, LocalTerm, TrailTerm,
                        !Specs)
                ;
                    MaybeTrailed = no,
                    % Local is wrong, but will not be used due to !:Specs.
                    LocalTermStr = mercury_term_to_string(VarSet,
                        print_name_only, LocalTerm),
                    Pieces = [words("Error: attribute"), quote(LocalTermStr),
                        words("conflicts with the default,"),
                        words("which is that updates are trailed."),
                        words("You need to specify the"), quote("untrailed"),
                        words("attribute explicitly."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree, get_term_context(LocalTerm),
                        Pieces),
                    !:Specs = [Spec | !.Specs]
                )
            ;
                MaybeLocal = no,
                (
                    MaybeTrailed = yes(_TrailTerm - Trail)
                ;
                    MaybeTrailed = no,
                    Trail = mutable_trailed  % The default.
                ),
                Local = mutable_is_not_thread_local(Trail)
            ),
            Const = mutable_is_not_constant(IO, Local)
        ;
            MaybeConst = yes(ConstTerm - unit),
            (
                MaybeIO = no
            ;
                MaybeIO = yes(IOTerm - unit),
                report_conflicting_attributes(VarSet, ConstTerm, IOTerm,
                    !Specs)
            ),
            (
                MaybeTrailed = no
            ;
                MaybeTrailed = yes(_TrailTerm - mutable_untrailed)
            ;
                MaybeTrailed = yes(TrailTerm - mutable_trailed),
                report_conflicting_attributes(VarSet, ConstTerm, TrailTerm,
                    !Specs)
            ),
            (
                MaybeLocal = no
            ;
                MaybeLocal = yes(LocalTerm - unit),
                report_conflicting_attributes(VarSet, ConstTerm, LocalTerm,
                    !Specs)
            ),
            Const = mutable_is_constant
        ),
        (
            !.Specs = [],
            MutAttrs = mutable_var_attributes(OnlyLangMap, Const),
            MaybeMutAttrs = ok1(MutAttrs)
        ;
            !.Specs = [_ | _],
            MaybeMutAttrs = error1(!.Specs)
        )
    ).

:- func default_mutable_attributes = mutable_var_attributes.

default_mutable_attributes =
    mutable_var_attributes(
        map.init,
        mutable_is_not_constant(
            mutable_dont_attach_to_io_state,
            mutable_is_not_thread_local(mutable_trailed)
        )
    ).

:- pred report_repeated_or_conflicting_attributes(varset::in, term::in, T::in,
    term::in, T::in, list(error_spec)::in, list(error_spec)::out) is det.

report_repeated_or_conflicting_attributes(VarSet, Term0, Attr0, Term, Attr,
        !Specs) :-
    TermStr = mercury_term_to_string(VarSet, print_name_only, Term),
    ( if Attr0 = Attr then
        Pieces = [words("Error: attribute"), quote(TermStr),
            words("is repeated."), nl]
    else
        TermStr0 = mercury_term_to_string(VarSet, print_name_only, Term0),
        Pieces = [words("Error: attributes"), quote(TermStr0),
            words("and"), quote(TermStr), words("conflict."), nl]
    ),
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        get_term_context(Term), Pieces),
    !:Specs = [Spec | !.Specs].

:- pred report_conflicting_attributes(varset::in, term::in, term::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_conflicting_attributes(VarSet, Term0, Term, !Specs) :-
    TermStr0 = mercury_term_to_string(VarSet, print_name_only, Term0),
    TermStr = mercury_term_to_string(VarSet, print_name_only, Term),
    Pieces = [words("Error: attributes"), quote(TermStr0),
        words("and"), quote(TermStr), words("conflict."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        get_term_context(Term), Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

    % NOTE: We return maybe1() wrapped around a pair instead using maybe2
    % because map_parser works only with maybe1.
    %
:- pred parse_mutable_attr(varset::in, term::in,
    maybe1(pair(term, collected_mutable_attribute))::out) is det.

parse_mutable_attr(VarSet, MutAttrTerm, MutAttrResult) :-
    ( if
        MutAttrTerm = term.functor(term.atom(String), [], _),
        (
            String  = "untrailed",
            MutAttr = mutable_attr_trailed(mutable_untrailed)
        ;
            String = "trailed",
            MutAttr = mutable_attr_trailed(mutable_trailed)
        ;
            String  = "attach_to_io_state",
            MutAttr = mutable_attr_attach_to_io_state
        ;
            String = "constant",
            MutAttr = mutable_attr_constant
        ;
            String = "thread_local",
            MutAttr = mutable_attr_thread_local
        )
    then
        MutAttrResult = ok1(MutAttrTerm - MutAttr)
    else if
        MutAttrTerm = term.functor(term.atom("foreign_name"), Args, _),
        Args = [LangTerm, ForeignNameTerm],
        term_to_foreign_language(LangTerm, Lang),
        ForeignNameTerm = term.functor(term.string(ForeignName), [], _)
    then
        MutAttr = mutable_attr_foreign_name(foreign_name(Lang, ForeignName)),
        MutAttrResult = ok1(MutAttrTerm - MutAttr)
    else if
        MutAttrTerm = term.functor(term.atom("foreign_name"), Args, _),
        Args = [LangTerm, _ForeignNameTerm],
        term_to_foreign_language_erlang(LangTerm)
    then
        Pieces = [words("Error in"), decl("mutable"), words("declaration:"),
            nl, words("support for Erlang has been discontinued."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(MutAttrTerm), Pieces),
        MutAttrResult = error1([Spec])
    else
        MutAttrStr = describe_error_term(VarSet, MutAttrTerm),
        Pieces = [words("Error in"), decl("mutable"), words("declaration:"),
            nl, words("unrecognised attribute"), quote(MutAttrStr),
            suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(MutAttrTerm), Pieces),
        MutAttrResult = error1([Spec])
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_mutable.
%---------------------------------------------------------------------------%
