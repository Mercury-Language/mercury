%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_pragma.m.
% Main authors: fjh, dgj, zs.
%
% This module handles the parsing of pragma directives.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_pragma.
:- interface.

:- import_module libs.globals.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Parse the pragma declaration. The item (if any) it returns is not
    % necessarily a pragma item.
    %
:- pred parse_pragma(module_name::in, varset::in, list(term)::in,
    prog_context::in, int::in, maybe1(item)::out) is semidet.

    % Parse a term that represents a foreign language.
    %
:- pred parse_foreign_language(term::in, foreign_language::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.rat.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_type_defn.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

%-----------------------------------------------------------------------------%

parse_pragma(ModuleName, VarSet, PragmaTerms, Context, SeqNum, MaybeItem) :-
    (
        PragmaTerms = [SinglePragmaTerm0],
        parse_type_decl_where_part_if_present(non_solver_type, ModuleName,
            VarSet, SinglePragmaTerm0, SinglePragmaTerm, MaybeWherePart),
        SinglePragmaTerm = term.functor(term.atom(PragmaName), PragmaArgs,
            _Context),
        parse_pragma_type(ModuleName, PragmaName, PragmaArgs, SinglePragmaTerm,
            VarSet, Context, SeqNum, MaybeItem0)
    ->
        (
            % The code to process `where' attributes will return an error
            % result if solver attributes are given for a non-solver type.
            % Because this is a non-solver type, if the unification with
            % MaybeWherePart succeeds then _SolverTypeDetails is guaranteed
            % to be `no'.
            MaybeWherePart = ok3(_SolverTypeDetails, MaybeUserEqComp,
                MaybeDirectArgIs),
            (
                MaybeUserEqComp = yes(_),
                MaybeItem0 = ok1(Item0)
            ->
                (
                    Item0 = item_type_defn(ItemTypeDefn0),
                    ItemTypeDefn0 ^ td_ctor_defn =
                        parse_tree_foreign_type(Type, _, Assertions)
                ->
                    ItemTypeDefn = ItemTypeDefn0 ^ td_ctor_defn :=
                        parse_tree_foreign_type(Type, MaybeUserEqComp,
                            Assertions),
                    Item = item_type_defn(ItemTypeDefn),
                    MaybeItem1 = ok1(Item)
                ;
                    Pieces = [words("Error: unexpected"),
                        quote("where equality/comparison is"),
                        suffix("."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SinglePragmaTerm0),
                            [always(Pieces)])]),
                    MaybeItem1 = error1([Spec])
                )
            ;
                MaybeItem1 = MaybeItem0
            ),
            (
                MaybeDirectArgIs = yes(_),
                MaybeItem1 = ok1(_)
            ->
                PiecesB = [words("Error:"), quote("direct_arg"),
                    words("attribute is not applicable to foreign types."),
                    nl],
                SpecB = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(SinglePragmaTerm0),
                        [always(PiecesB)])]),
                MaybeItem = error1([SpecB])
            ;
                MaybeItem = MaybeItem1
            )
        ;
            MaybeWherePart = error3(Specs),
            MaybeItem = error1(Specs)
        )
    ;
        fail
    ).

%----------------------------------------------------------------------------%

:- pred parse_pragma_type(module_name::in, string::in, list(term)::in,
    term::in, varset::in, prog_context::in, int::in, maybe1(item)::out)
    is semidet.

parse_pragma_type(ModuleName, PragmaName, PragmaTerms, ErrorTerm, VarSet,
        Context, SeqNum, MaybeItem) :-
    (
        PragmaName = "source_file",
        parse_pragma_source_file(PragmaTerms, ErrorTerm,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "foreign_type",
        parse_pragma_foreign_type(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "foreign_decl",
        parse_pragma_foreign_decl_pragma(ModuleName, PragmaName,
            PragmaTerms, ErrorTerm, VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "foreign_code",
        parse_pragma_foreign_code_pragma(ModuleName, PragmaName,
            PragmaTerms, ErrorTerm, VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "foreign_proc",
        parse_pragma_foreign_proc_pragma(ModuleName, PragmaName,
            PragmaTerms, ErrorTerm, VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "foreign_export_enum",
        parse_pragma_foreign_export_enum(PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "foreign_enum",
        parse_pragma_foreign_enum(PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "foreign_export",
        parse_pragma_foreign_export(PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "foreign_import_module",
        parse_pragma_foreign_import_module(PragmaTerms, ErrorTerm,
            Context, SeqNum, MaybeItem)
    ;
        (
            PragmaName = "inline",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_inline(PredNameArity)
            )
        ;
            PragmaName = "no_inline",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_no_inline(PredNameArity)
            )
        ;
            PragmaName = "obsolete",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_obsolete(PredNameArity)
            )
        ;
            PragmaName = "no_determinism_warning",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_no_detism_warning(PredNameArity)
            )
        ;
            PragmaName = "promise_equivalent_clauses",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_promise_eqv_clauses(PredNameArity)
            )
        ;
            PragmaName = "promise_pure",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_promise_pure(PredNameArity)
            )
        ;
            PragmaName = "promise_semipure",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_promise_semipure(PredNameArity)
            )
        ;
            PragmaName = "terminates",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_terminates(PredNameArity)
            )
        ;
            PragmaName = "does_not_terminate",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_does_not_terminate(PredNameArity)
            )
        ;
            PragmaName = "check_termination",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_check_termination(PredNameArity)
            )
        ;
            PragmaName = "mode_check_clauses",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_mode_check_clauses(PredNameArity)
            )
        ),
        parse_name_arity_pragma(ModuleName, PragmaName,
            "predicate or function", MakePragma, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "reserve_tag",
        MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
            TypeCtor = type_ctor(Name, Arity),
            Pragma = pragma_reserve_tag(TypeCtor)
        ),
        parse_name_arity_pragma(ModuleName, PragmaName,
            "type", MakePragma, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "oisu",
        parse_oisu_pragma(ModuleName, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeItem)
    ;
        (
            PragmaName = "memo",
            EvalMethod = eval_memo
        ;
            PragmaName = "loop_check",
            EvalMethod = eval_loop_check
        ;
            PragmaName = "minimal_model",
            % We don't yet know whether we will use the stack_copy or the
            % own_stacks technique for computing minimal models. The decision
            % depends on the grade, and is made in make_hlds.m; the
            % "stack_copy" here is just a placeholder.
            EvalMethod = eval_minimal(stack_copy)
        ),
        parse_tabling_pragma(ModuleName, PragmaName, EvalMethod,
            PragmaTerms, ErrorTerm, VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "unused_args",
        parse_pragma_unused_args(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "type_spec",
        parse_pragma_type_spec(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "fact_table",
        parse_pragma_fact_table(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "termination_info",
        parse_pragma_termination_info(ModuleName, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "termination2_info",
        parse_pragma_termination2_info(ModuleName, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "structure_sharing",
        parse_pragma_structure_sharing(ModuleName, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "structure_reuse",
        parse_pragma_structure_reuse(ModuleName, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeItem)
    ;
        PragmaName = "exceptions",
        parse_pragma_exceptions(ModuleName, PragmaTerms, ErrorTerm,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "trailing_info",
        parse_pragma_trailing_info(ModuleName, PragmaTerms, ErrorTerm,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "mm_tabling_info",
        parse_pragma_mm_tabling_info(ModuleName, PragmaTerms, ErrorTerm,
            Context, SeqNum, MaybeItem)
    ;
        PragmaName = "require_feature_set",
        parse_pragma_require_feature_set(PragmaTerms, VarSet, ErrorTerm,
            Context, SeqNum, MaybeItem)
    ).

%----------------------------------------------------------------------------%

% XXX The predicates in the rest of this module ought to be clustered together
% into groups of related predicates, grouping both parse_pragma_xxx predicates
% together with their helper predicates, and grouping parse_pragma_xxx
% predicates for related xxxs together.

:- pred parse_pragma_source_file(list(term)::in, term::in, prog_context::in,
    int::in, maybe1(item)::out) is det.

parse_pragma_source_file(PragmaTerms, ErrorTerm, Context, SeqNum, MaybeItem) :-
    ( PragmaTerms = [SourceFileTerm] ->
        ( SourceFileTerm = term.functor(term.string(SourceFile), [], _) ->
            PSFInfo = pragma_info_source_file(SourceFile),
            Pragma = pragma_source_file(PSFInfo),
            ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeItem = ok1(Item)
        ;
            Pieces = [words("Error: the argument of a"),
                quote(":- pragma source_file"),
                words("declaration should be a string."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(SourceFileTerm),
                    [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma source_file"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_foreign_type(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_foreign_type(ModuleName, PragmaTerms, ErrorTerm, VarSet, Context,
        SeqNum, MaybeItem) :-
    (
        (
            PragmaTerms = [LangTerm, MercuryTypeTerm, ForeignTypeTerm],
            MaybeAssertionTerm = no
        ;
            PragmaTerms = [LangTerm, MercuryTypeTerm, ForeignTypeTerm,
                AssertionTerm],
            MaybeAssertionTerm = yes(AssertionTerm)
        )
    ->
        (
            parse_foreign_language(LangTerm, Language)
        ->
            parse_foreign_language_type(ForeignTypeTerm, VarSet, Language,
                MaybeForeignType),
            (
                MaybeForeignType = ok1(ForeignType),
                parse_type_defn_head(ModuleName, VarSet, MercuryTypeTerm,
                    MaybeTypeDefnHead),
                (
                    MaybeTypeDefnHead = ok2(MercuryTypeSymName, MercuryParams),
                    varset.coerce(VarSet, TVarSet),
                    (
                        parse_maybe_foreign_type_assertions(MaybeAssertionTerm,
                            Assertions)
                    ->
                        % rafe: XXX I'm not sure that `no' here is right
                        % - we might need some more parsing...
                        ItemTypeDefn = item_type_defn_info(TVarSet,
                            MercuryTypeSymName, MercuryParams,
                            parse_tree_foreign_type(ForeignType, no,
                                Assertions),
                            cond_true, Context, SeqNum),
                        Item = item_type_defn(ItemTypeDefn),
                        MaybeItem = ok1(Item)
                    ;
                        MaybeAssertionTerm = yes(ErrorAssertionTerm)
                    ->
                        Pieces = [words("Error: invalid assertion in"),
                            quote(":- pragma foreign_type"),
                            words("declaration."), nl],
                        Spec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(get_term_context(ErrorAssertionTerm),
                                [always(Pieces)])]),
                        MaybeItem = error1([Spec])
                    ;
                        unexpected($module, $pred,
                            "unexpected failure of " ++
                            "parse_maybe_foreign_type_assertion")
                    )
                ;
                    MaybeTypeDefnHead = error2(Specs),
                    MaybeItem = error1(Specs)
                )
            ;
                MaybeForeignType = error1(Specs),
                MaybeItem = error1(Specs)
            )
        ;
            Pieces = [words("Error: invalid foreign language in"),
                quote(":- pragma foreign_type"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(LangTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma foreign_type"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

%----------------------------------------------------------------------------%
%
% Code for parsing foreign_export_enum pragmas
%

:- pred parse_pragma_foreign_export_enum(list(term)::in, term::in,
    varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_foreign_export_enum(PragmaTerms, ErrorTerm, VarSet, Context,
        SeqNum, MaybeItem) :-
    (
        (
            PragmaTerms = [LangTerm, MercuryTypeTerm],
            MaybeAttributesTerm = no,
            MaybeOverridesTerm = no
        ;
            PragmaTerms = [LangTerm, MercuryTypeTerm, AttributesTerm],
            MaybeAttributesTerm = yes(AttributesTerm),
            MaybeOverridesTerm = no
        ;
            PragmaTerms = [LangTerm, MercuryTypeTerm, AttributesTerm,
                OverridesTerm],
            MaybeAttributesTerm = yes(AttributesTerm),
            MaybeOverridesTerm = yes(OverridesTerm)
        )
    ->
        ( parse_foreign_language(LangTerm, ForeignLang) ->
            parse_export_enum_type_ctor(MercuryTypeTerm, MaybeTypeCtor),
            (
                MaybeTypeCtor = ok1(TypeCtor),
                maybe_parse_export_enum_attributes(VarSet, MaybeAttributesTerm,
                    MaybeAttributes),
                (
                    MaybeAttributes = ok1(Attributes),
                    maybe_parse_export_enum_overrides(VarSet,
                        MaybeOverridesTerm, MaybeOverrides),
                    (
                        MaybeOverrides = ok1(Overrides),
                        FEEInfo = pragma_info_foreign_export_enum(ForeignLang,
                            TypeCtor, Attributes, Overrides),
                        Pragma = pragma_foreign_export_enum(FEEInfo),
                        ItemPragma = item_pragma_info(user, Pragma,
                            Context, SeqNum),
                        Item = item_pragma(ItemPragma),
                        MaybeItem = ok1(Item)
                    ;
                        MaybeOverrides = error1(Specs),
                        MaybeItem = error1(Specs)
                    )
                ;
                    MaybeAttributes = error1(Specs),
                    MaybeItem = error1(Specs)
                )
            ;
                MaybeTypeCtor = error1(Specs),
                MaybeItem = error1(Specs)
            )
        ;
            Pieces = [words("Error: invalid foreign language in"),
                quote(":- pragma foreign_export_enum"), words("declaration."),
                nl],
            % XXX Get_term_context(LangTerm) would be better.
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma foreign_export_enum"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_export_enum_type_ctor(term::in, maybe1(type_ctor)::out) is det.

parse_export_enum_type_ctor(TypeTerm, MaybeTypeCtor) :-
    ( parse_name_and_arity_unqualified(TypeTerm, Name, Arity) ->
        MaybeTypeCtor = ok1(type_ctor(Name, Arity))
    ;
        Pieces = [words("Error: expected name/arity for type in"),
            quote(":- pragma foreign_export_enum"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(TypeTerm), [always(Pieces)])]),
        MaybeTypeCtor = error1([Spec])
    ).

:- pred maybe_parse_export_enum_overrides(varset::in, maybe(term)::in,
    maybe1(assoc_list(sym_name, string))::out) is det.

maybe_parse_export_enum_overrides(_, no, ok1([])).
maybe_parse_export_enum_overrides(VarSet, yes(OverridesTerm),
        MaybeOverrides) :-
    UnrecognizedPieces =
        [words("Error: expected a valid mapping element."), nl],
    PairPieces = [words("In exported enumeration override constructor:")],
    convert_maybe_list("mapping elements", yes(VarSet), OverridesTerm,
        parse_sym_name_string_pair(VarSet, PairPieces),
        UnrecognizedPieces, MaybeOverrides).

:- pred parse_sym_name_string_pair(varset::in, list(format_component)::in,
    term::in, maybe1(pair(sym_name, string))::out) is semidet.

parse_sym_name_string_pair(VarSet, ContextPieces, PairTerm, MaybePair) :-
    PairTerm = functor(Functor, Args, _),
    Functor = term.atom("-"),
    Args = [SymNameTerm, StringTerm],
    StringTerm = functor(term.string(String), _, _),
    parse_sym_name_and_args(SymNameTerm, VarSet, ContextPieces,
        MaybeSymNameResult),
    (
        MaybeSymNameResult = ok2(SymName, []),
        MaybePair = ok1(SymName - String)
    ;
        MaybeSymNameResult = error2(Specs),
        MaybePair = error1(Specs)
    ).

:- pred maybe_parse_export_enum_attributes(varset::in, maybe(term)::in,
    maybe1(export_enum_attributes)::out) is det.

maybe_parse_export_enum_attributes(_, no, ok1(default_export_enum_attributes)).
maybe_parse_export_enum_attributes(VarSet, yes(AttributesTerm),
        MaybeAttributes) :-
    parse_export_enum_attributes(VarSet, AttributesTerm, MaybeAttributes).

:- type collected_export_enum_attribute
    --->    ee_attr_prefix(maybe(string))
    ;       ee_attr_upper(uppercase_export_enum).

:- pred parse_export_enum_attributes(varset::in, term::in,
    maybe1(export_enum_attributes)::out) is det.

parse_export_enum_attributes(VarSet, AttributesTerm, AttributesResult) :-
    Attributes0 = default_export_enum_attributes,
    ConflictingAttributes = [],
    (
        list_term_to_term_list(AttributesTerm, AttributesTerms),
        map_parser(parse_export_enum_attr(VarSet), AttributesTerms,
            MaybeAttrList),
        MaybeAttrList = ok1(CollectedAttributes)
    ->
        (
            list.member(ConflictA - ConflictB, ConflictingAttributes),
            list.member(ConflictA, CollectedAttributes),
            list.member(ConflictB, CollectedAttributes)
        ->
            % XXX Print the conflicting attributes themselves.
            Pieces = [words("Error: conflicting attributes in"),
                quote(":- pragma foreign_export_enum"), words("declaration."),
                nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(AttributesTerm),
                    [always(Pieces)])]),
            AttributesResult = error1([Spec])
        ;
            % Check that the prefix attribute is specified at most once.
            IsPrefixAttr = (pred(A::in) is semidet :-
                A = ee_attr_prefix(_)
            ),
            list.filter(IsPrefixAttr, CollectedAttributes, PrefixAttributes),
            (
                ( PrefixAttributes = []
                ; PrefixAttributes = [_]
                ),
                list.foldl(process_export_enum_attribute, CollectedAttributes,
                    Attributes0, Attributes),
                AttributesResult = ok1(Attributes)
            ;
                PrefixAttributes = [_, _ | _],
                % XXX Print the multiply-occurring attribute.
                Pieces = [words("Error: prefix attribute"),
                    words("occurs multiple times in"),
                    quote(":- pragma foreign_export_enum"),
                    words("declaration."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(AttributesTerm),
                        [always(Pieces)])]),
                AttributesResult = error1([Spec])
            )
        )
    ;
        Pieces = [words("Error: malformed attributes list in"),
            quote(":- pragma foreign_export_enum"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(AttributesTerm), [always(Pieces)])]),
        AttributesResult = error1([Spec])
    ).

:- pred process_export_enum_attribute(collected_export_enum_attribute::in,
    export_enum_attributes::in, export_enum_attributes::out) is det.

process_export_enum_attribute(ee_attr_prefix(MaybePrefix), !Attributes) :-
    % We haved alredy checked that the prefix attribute is not specified
    % multiple times in parse_export_enum_attributes so it is safe to
    % ignore it in the input here.
    !.Attributes = export_enum_attributes(_, MakeUpperCase),
    !:Attributes = export_enum_attributes(MaybePrefix, MakeUpperCase).
process_export_enum_attribute(ee_attr_upper(MakeUpperCase), !Attributes) :-
    !.Attributes = export_enum_attributes(MaybePrefix, _),
    !:Attributes = export_enum_attributes(MaybePrefix, MakeUpperCase).

:- pred parse_export_enum_attr(varset::in, term::in,
    maybe1(collected_export_enum_attribute)::out) is det.

parse_export_enum_attr(VarSet, Term, MaybeAttribute) :-
    (
        Term = functor(atom("prefix"), Args, _),
        Args = [ ForeignNameTerm ],
        ForeignNameTerm = functor(string(Prefix), [], _)
    ->
        MaybeAttribute = ok1(ee_attr_prefix(yes(Prefix)))
    ;
        Term = functor(atom("uppercase"), [], _)
    ->
        MaybeAttribute = ok1(ee_attr_upper(uppercase_export_enum))
    ;
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: unrecognised attribute in"),
            quote(":- pragma foreign_export_enum"), words("declaration:"),
            words(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeAttribute = error1([Spec])
    ).

%----------------------------------------------------------------------------%
%
% Code for parsing foreign_enum pragmas
%

:- pred parse_pragma_foreign_enum(list(term)::in, term::in, varset::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_foreign_enum(PragmaTerms, ErrorTerm, VarSet, Context, SeqNum,
        MaybeItem) :-
    ( PragmaTerms = [LangTerm, MercuryTypeTerm, ValuesTerm] ->
        ( parse_foreign_language(LangTerm, ForeignLang) ->
            parse_export_enum_type_ctor(MercuryTypeTerm, MaybeTypeCtor),
            (
                MaybeTypeCtor = ok1(TypeCtor),
                UnrecognizedPieces =
                    [words("Error: expected a valid mapping element."), nl],
                PairPieces = [words("In foreign_enum constructor name:")],
                convert_maybe_list("mapping elements", yes(VarSet), ValuesTerm,
                    parse_sym_name_string_pair(VarSet, PairPieces),
                    UnrecognizedPieces, MaybeValues),
                (
                    MaybeValues = ok1(Values),
                    FEInfo = pragma_info_foreign_enum(ForeignLang, TypeCtor,
                        Values),
                    Pragma = pragma_foreign_enum(FEInfo),
                    ItemPragma = item_pragma_info(user, Pragma, Context,
                        SeqNum),
                    Item = item_pragma(ItemPragma),
                    MaybeItem = ok1(Item)
                ;
                    MaybeValues = error1(Specs),
                    MaybeItem = error1(Specs)
                )
            ;
                MaybeTypeCtor = error1(Specs),
                MaybeItem = error1(Specs)
            )
        ;
            Pieces = [words("Error: invalid foreign language in"),
                quote(":- pragma foreign_export_enum"), words("declaration."),
                nl],
            % XXX We should use the context of LangTerm.
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma foreign_export_enum"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

%----------------------------------------------------------------------------%
%
% Code for parsing foreign_export pragmas
%

:- pred parse_pragma_foreign_export(list(term)::in, term::in, varset::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_foreign_export(PragmaTerms, ErrorTerm, VarSet, Context, SeqNum,
        MaybeItem) :-
    ( PragmaTerms = [LangTerm, PredAndModesTerm, FunctionTerm] ->
        ( FunctionTerm = term.functor(term.string(Function), [], _) ->
            ContextPieces = [words("In"),
                quote(":- pragma foreign_export"), words("declaration")],
            parse_pred_or_func_and_arg_modes(no, PredAndModesTerm,
                ErrorTerm, VarSet, ContextPieces, MaybePredAndModes),
            (
                MaybePredAndModes = ok2(PredName - PredOrFunc, Modes),
                ( parse_foreign_language(LangTerm, ForeignLanguage) ->
                    PredNameModesPF = pred_name_modes_pf(PredName, Modes,
                        PredOrFunc),
                    FPEInfo = pragma_info_foreign_proc_export(ForeignLanguage,
                        PredNameModesPF, Function),
                    Pragma = pragma_foreign_proc_export(FPEInfo),
                    ItemPragma = item_pragma_info(user, Pragma, Context,
                        SeqNum),
                    Item = item_pragma(ItemPragma),
                    MaybeItem = ok1(Item)
                ;
                    Pieces = [words("Error: invalid foreign language in"),
                        quote(":- pragma foreign_export"),
                        words("declaration."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(LangTerm),
                            [always(Pieces)])]),
                    MaybeItem = error1([Spec])
                )
            ;
                MaybePredAndModes = error2(Specs),
                MaybeItem = error1(Specs)
            )
        ;
            % XXX Why this wording?
            Pieces = [words("Error: expected pragma"),
                words("foreign_export(Lang, PredName(ModeList), Function)."),
                nl],
            % XXX Should we use the context of FunctionTerm?
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(PredAndModesTerm),
                    [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma foreign_export"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

%----------------------------------------------------------------------------%

:- pred parse_pragma_foreign_import_module(list(term)::in, term::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_foreign_import_module(PragmaTerms, ErrorTerm, Context, SeqNum,
        MaybeItem) :-
    (
        PragmaTerms = [LangTerm, ImportTerm],
        try_parse_sym_name_and_no_args(ImportTerm, Import)
    ->
        ( parse_foreign_language(LangTerm, Language) ->
            FIMInfo = pragma_info_foreign_import_module(Language, Import),
            Pragma = pragma_foreign_import_module(FIMInfo),
            ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeItem = ok1(Item)
        ;
            Pieces = [words("Error: invalid foreign language in"),
                quote(":- pragma foreign_import_module"),
                words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(LangTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        Pieces = [words("Error: wrong number of arguments"),
            words("or invalid module name in"),
            quote(":- pragma foreign_import_module"),
            words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_unused_args(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_unused_args(ModuleName, PragmaTerms, ErrorTerm, VarSet, Context,
        SeqNum, MaybeItem) :-
    % pragma unused_args should never appear in user programs,
    % only in .opt files.
    (
        PragmaTerms = [PredOrFuncTerm, PredNameTerm, ArityTerm, ModeNumTerm,
            UnusedArgsTerm],
        ArityTerm = term.functor(term.integer(Arity), [], _),
        ModeNumTerm = term.functor(term.integer(ModeNum), [], _),
        parse_predicate_or_function(PredOrFuncTerm, PredOrFunc),
        try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
            PredNameTerm, PredName),
        convert_int_list(VarSet, UnusedArgsTerm, MaybeUnusedArgs),
        MaybeUnusedArgs = ok1(UnusedArgs)
    ->
        PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity, PredOrFunc,
            ModeNum),
        UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn,
            UnusedArgs),
        Pragma = pragma_unused_args(UnusedArgsInfo),
        ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeItem = ok1(Item)
    ;
        % XXX Improve this message.
        Pieces = [words("Error in"), quote(":- pragma unused_args"),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_type_spec(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_type_spec(ModuleName, PragmaTerms, ErrorTerm, VarSet, Context,
        SeqNum, MaybeItem) :-
    (
        (
            PragmaTerms = [PredAndModesTerm, TypeSubnTerm],
            MaybeName = no
        ;
            PragmaTerms = [PredAndModesTerm, TypeSubnTerm, SpecNameTerm],
            SpecNameTerm = term.functor(_, _, SpecContext),

            % This form of the pragma should not appear in source files.
            term.context_file(SpecContext, FileName),
            \+ string.remove_suffix(FileName, ".m", _),

            try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
                SpecNameTerm, SpecName),
            MaybeName = yes(SpecName)
        )
    ->
        ArityOrModesContextPieces = [words("In"), quote(":- pragma type_spec"),
            words("declaration:")],
        parse_arity_or_modes(ModuleName, PredAndModesTerm, ErrorTerm,
            VarSet, ArityOrModesContextPieces, MaybeArityOrModes),
        (
            MaybeArityOrModes = ok1(ArityOrModes),
            ArityOrModes = arity_or_modes(PredName, Arity, MaybePredOrFunc,
                MaybeModes),
            conjunction_to_list(TypeSubnTerm, TypeSubnList),

            % The varset is actually a tvarset.
            varset.coerce(VarSet, TVarSet),
            ( list.map(convert_type_spec_pair, TypeSubnList, TypeSubn) ->
                (
                    MaybeName = yes(SpecializedName0),
                    SpecializedName = SpecializedName0
                ;
                    MaybeName = no,
                    UnqualName = unqualify_name(PredName),
                    make_pred_name(ModuleName, "TypeSpecOf", MaybePredOrFunc,
                        UnqualName, newpred_type_subst(TVarSet, TypeSubn),
                        SpecializedName)
                ),
                TypeSpecInfo = pragma_info_type_spec(PredName, SpecializedName,
                    Arity, MaybePredOrFunc, MaybeModes, TypeSubn, TVarSet,
                    set.init),
                Pragma = pragma_type_spec(TypeSpecInfo),
                ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
                Item = item_pragma(ItemPragma),
                MaybeItem = ok1(Item)
            ;
                Pieces = [words("Error: expected type substitution in"),
                    quote(":- pragma type_spec"), words("declaration."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(TypeSubnTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        ;
            MaybeArityOrModes = error1(Specs),
            MaybeItem = error1(Specs)
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma type_spec"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_fact_table(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_fact_table(ModuleName, PragmaTerms, ErrorTerm,
        VarSet, Context, SeqNum, MaybeItem) :-
    ( PragmaTerms = [PredAndArityTerm, FileNameTerm] ->
        parse_pred_name_and_arity(ModuleName, "fact_table",
            PredAndArityTerm, ErrorTerm, VarSet, MaybeNameAndArity),
        (
            MaybeNameAndArity = ok2(PredName, Arity),
            ( FileNameTerm = term.functor(term.string(FileName), [], _) ->
                PredNameArity = pred_name_arity(PredName, Arity),
                FactTableInfo = pragma_info_fact_table(PredNameArity,
                    FileName),
                Pragma = pragma_fact_table(FactTableInfo),
                ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
                Item = item_pragma(ItemPragma),
                MaybeItem = ok1(Item)
            ;
                Pieces = [words("Error: expected string"),
                    words("for fact table filename."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(FileNameTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        ;
            MaybeNameAndArity = error2(Specs),
            MaybeItem = error1(Specs)
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma fact_table"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_termination_info(module_name::in, list(term)::in,
    term::in, varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_termination_info(ModuleName, PragmaTerms, ErrorTerm, VarSet,
        Context, SeqNum, MaybeItem) :-
    (
        PragmaTerms = [PredAndModesTerm0, ArgSizeTerm, TerminationTerm],
        ContextPieces = [words("In"),
            quote(":- pragma termination_info"), words("declaration:")],
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            ErrorTerm, VarSet, ContextPieces, MaybeNameAndModes),
        MaybeNameAndModes = ok2(PredName - PredOrFunc, ModeList),
        ArgSizeTerm = term.functor(term.atom(ArgSizeFunctor),
            ArgSizeArgTerms, _),
        (
            ArgSizeFunctor = "not_set",
            ArgSizeArgTerms = [],
            MaybeArgSizeInfo = no
        ;
            ArgSizeFunctor = "infinite",
            ArgSizeArgTerms = [],
            MaybeArgSizeInfo = yes(infinite(unit))
        ;
            ArgSizeFunctor = "finite",
            ArgSizeArgTerms = [IntTerm, UsedArgsTerm],
            IntTerm = term.functor(term.integer(Int), [], _),
            convert_bool_list(VarSet, UsedArgsTerm, UsedArgs),
            MaybeArgSizeInfo = yes(finite(Int, UsedArgs))
        ),
        TerminationTerm = term.functor(term.atom(TerminationFunctor), [], _),
        (
            TerminationFunctor = "not_set",
            MaybeTerminationInfo = no
        ;
            TerminationFunctor = "can_loop",
            MaybeTerminationInfo = yes(can_loop(unit))
        ;
            TerminationFunctor = "cannot_loop",
            MaybeTerminationInfo = yes(cannot_loop(unit))
        )
    ->
        PredNameModesPF = pred_name_modes_pf(PredName, ModeList, PredOrFunc),
        TermInfo = pragma_info_termination_info(PredNameModesPF,
            MaybeArgSizeInfo, MaybeTerminationInfo),
        Pragma = pragma_termination_info(TermInfo),
        ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeItem = ok1(Item)
    ;
        Pieces = [words("Syntax error in"),
            quote(":- pragma termination_info"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_termination2_info(module_name::in, list(term)::in,
    term::in, varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_termination2_info(ModuleName, PragmaTerms, ErrorTerm, VarSet,
        Context, SeqNum, MaybeItem) :-
    (
        PragmaTerms = [PredAndModesTerm0, SuccessArgSizeTerm,
            FailureArgSizeTerm, TerminationTerm],
        ContextPieces = [words("In"), quote(":- pragma termination2_info"),
            words("declaration:")],
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            ErrorTerm, VarSet, ContextPieces, NameAndModesResult),
        NameAndModesResult = ok2(PredName - PredOrFunc, ModeList),
        parse_arg_size_constraints(SuccessArgSizeTerm, SuccessArgSizeResult),
        SuccessArgSizeResult = ok1(SuccessArgSizeInfo),
        parse_arg_size_constraints(FailureArgSizeTerm, FailureArgSizeResult),
        FailureArgSizeResult = ok1(FailureArgSizeInfo),
        TerminationTerm = term.functor(term.atom(TerminationFunctor), [], _),
        (
            TerminationFunctor = "not_set",
            MaybeTerminationInfo = no
        ;
            TerminationFunctor = "can_loop",
            MaybeTerminationInfo = yes(can_loop(unit))
        ;
            TerminationFunctor = "cannot_loop",
            MaybeTerminationInfo = yes(cannot_loop(unit))
        )
    ->
        PredNameModesPF = pred_name_modes_pf(PredName, ModeList, PredOrFunc),
        Term2Info = pragma_info_termination2_info(PredNameModesPF,
            SuccessArgSizeInfo, FailureArgSizeInfo, MaybeTerminationInfo),
        Pragma = pragma_termination2_info(Term2Info),
        ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeItem = ok1(Item)
    ;
        Pieces = [words("Syntax error in"),
            quote(":- pragma termination2_info"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_structure_sharing(module_name::in, list(term)::in,
    term::in, varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_structure_sharing(ModuleName, PragmaTerms, ErrorTerm, VarSet,
        Context, SeqNum, MaybeItem) :-
    (
        PragmaTerms = [PredAndModesTerm0, HeadVarsTerm,
            HeadVarTypesTerm, SharingInformationTerm],
        ModesContextPieces = [words("In"),
            quote(":- pragma structure_sharing"), words("declaration:")],
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            ErrorTerm, VarSet, ModesContextPieces, MaybeNameAndModes),
        MaybeNameAndModes = ok2(PredName - PredOrFunc, ModeList),

        % Parse the head variables:
        HeadVarsTerm = term.functor(term.atom("vars"), ListHVTerm, _),
        term.vars_list(ListHVTerm, HeadVarsGeneric),
        list.map(term.coerce_var, HeadVarsGeneric, HeadVars),

        % Parse the types:
        HeadVarTypesTerm = term.functor(term.atom("types"), ListTypeTerms, _),
        maybe_parse_types(ListTypeTerms, Types),

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
    ->
        PredNameModesPF = pred_name_modes_pf(PredName, ModeList, PredOrFunc),
        SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
            HeadVars, Types, MaybeSharingAs),
        Pragma = pragma_structure_sharing(SharingInfo),
        ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeItem = ok1(Item)
    ;
        Pieces = [words("Syntax error in"),
            quote(":- pragma structure_sharing"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_structure_reuse(module_name::in, list(term)::in,
    term::in, varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_structure_reuse(ModuleName, PragmaTerms, ErrorTerm, VarSet,
        Context, SeqNum, MaybeItem) :-
    (
        PragmaTerms = [PredAndModesTerm0, HeadVarsTerm,
            HeadVarTypesTerm, MaybeStructureReuseTerm],
        ReuseContextPieces = [words("In"), quote(":- pragma structure_reuse"),
            words("declaration:")],
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            ErrorTerm, VarSet, ReuseContextPieces, MaybeNameAndModes),
        MaybeNameAndModes = ok2(PredName - PredOrFunc, ModeList),

        % Parse the head variables:
        HeadVarsTerm = term.functor(term.atom("vars"), ListHVTerm, _),
        term.vars_list(ListHVTerm, HeadVarsGeneric),
        list.map(term.coerce_var, HeadVarsGeneric, HeadVars),

        % Parse the types:
        HeadVarTypesTerm = term.functor(term.atom("types"), ListTypeTerms, _),
        maybe_parse_types(ListTypeTerms, Types),

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
    ->
        PredNameModesPF = pred_name_modes_pf(PredName, ModeList, PredOrFunc),
        ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
            HeadVars, Types, MaybeStructureReuse),
        Pragma = pragma_structure_reuse(ReuseInfo),
        ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeItem = ok1(Item)
    ;
        Pieces = [words("Syntax error in"),
            quote(":- pragma structure_reuse"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_exceptions(module_name::in, list(term)::in, term::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_exceptions(ModuleName, PragmaTerms, ErrorTerm, Context,
        SeqNum, MaybeItem) :-
    (
        PragmaTerms = [PredOrFuncTerm, PredNameTerm, ArityTerm, ModeNumTerm,
            ThrowStatusTerm],
        parse_predicate_or_function(PredOrFuncTerm, PredOrFunc),
        ArityTerm = term.functor(term.integer(Arity), [], _),
        ModeNumTerm = term.functor(term.integer(ModeNum), [], _),
        try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
            PredNameTerm, PredName),
        ThrowStatusTerm = term.functor(term.atom(ThrowStatusFunctor),
            ThrowStatusArgTerms, _),
        (
            ThrowStatusFunctor = "will_not_throw",
            ThrowStatusArgTerms = [],
            ThrowStatus = will_not_throw
        ;
            ThrowStatusFunctor = "may_throw",
            ThrowStatusArgTerms = [ExceptionTypeTerm],
            ExceptionTypeTerm = term.functor(term.atom(ExceptionFunctor),
                [], _),
            (
                ExceptionFunctor = "user_exception",
                ExceptionType = user_exception
            ;
                ExceptionFunctor = "type_exception",
                ExceptionType = type_exception
            ),
            ThrowStatus = may_throw(ExceptionType)
        ;
            ThrowStatusFunctor = "conditional",
            ThrowStatusArgTerms = [],
            ThrowStatus = throw_conditional
        )
    ->
        PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity, PredOrFunc,
            ModeNum),
        ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn,
            ThrowStatus),
        Pragma = pragma_exceptions(ExceptionsInfo),
        ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeItem = ok1(Item)
    ;
        Pieces = [words("Error in"),
            quote(":- pragma exceptions"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_trailing_info(module_name::in, list(term)::in, term::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_trailing_info(ModuleName, PragmaTerms, ErrorTerm, Context,
        SeqNum, MaybeItem) :-
    (
        PragmaTerms = [PredOrFuncTerm, PredNameTerm, ArityTerm, ModeNumTerm,
            TrailingStatusTerm],
        parse_predicate_or_function(PredOrFuncTerm, PredOrFunc),
        ArityTerm = term.functor(term.integer(Arity), [], _),
        ModeNumTerm = term.functor(term.integer(ModeNum), [], _),
        try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
            PredNameTerm, PredName),
        TrailingStatusTerm = term.functor(term.atom(TrailingStatusFunctor),
            [], _),
        (
            TrailingStatusFunctor = "will_not_modify_trail",
            TrailingStatus = trail_will_not_modify
        ;
            TrailingStatusFunctor = "may_modify_trail",
            TrailingStatus = trail_may_modify
        ;
            TrailingStatusFunctor = "conditional",
            TrailingStatus = trail_conditional
        )
    ->
        PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity, PredOrFunc,
            ModeNum),
        TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn,
            TrailingStatus),
        Pragma = pragma_trailing_info(TrailingInfo),
        ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeItem = ok1(Item)
    ;
        Pieces = [words("Error in"), quote(":- pragma trailing_info"),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_mm_tabling_info(module_name::in, list(term)::in, term::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_mm_tabling_info(ModuleName, PragmaTerms, ErrorTerm,
        Context, SeqNum, MaybeItem) :-
    (
        PragmaTerms = [PredOrFuncTerm, PredNameTerm, ArityTerm, ModeNumTerm,
            MM_TablingStatusTerm],
        parse_predicate_or_function(PredOrFuncTerm, PredOrFunc),
        ArityTerm = term.functor(term.integer(Arity), [], _),
        ModeNumTerm = term.functor(term.integer(ModeNum), [], _),
        try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
            PredNameTerm, PredName),
        MM_TablingStatusTerm = term.functor(term.atom(MM_TablingStatusFunctor),
            [], _),
        (
            MM_TablingStatusFunctor = "mm_tabled_will_not_call",
            MM_TablingStatus = mm_tabled_will_not_call
        ;
            MM_TablingStatusFunctor = "mm_tabled_may_call",
            MM_TablingStatus = mm_tabled_may_call
        ;
            MM_TablingStatusFunctor = "mm_tabled_conditional",
            MM_TablingStatus = mm_tabled_conditional
        )
    ->
        PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity, PredOrFunc,
            ModeNum),
        TablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
            MM_TablingStatus),
        Pragma = pragma_mm_tabling_info(TablingInfo),
        ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeItem = ok1(Item)
    ;
        Pieces = [words("Error in"), quote(":- pragma mm_tabling_info"),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_require_feature_set(list(term)::in, varset::in, term::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_require_feature_set(PragmaTerms, VarSet, ErrorTerm, Context,
        SeqNum, MaybeItem) :-
    ( PragmaTerms = [FeatureListTerm] ->
        UnrecognizedPieces = [words("Error: expected a feature"), nl],
        convert_maybe_list("features", yes(VarSet), FeatureListTerm,
            parse_required_feature, UnrecognizedPieces, MaybeFeatureList),
        (
            MaybeFeatureList = ok1(FeatureList),
            ConflictingFeatures = [
                reqf_single_prec_float - reqf_double_prec_float,
                reqf_parallel_conj     - reqf_trailing
            ],
            (
                list.member(ConflictA - ConflictB, ConflictingFeatures),
                list.member(ConflictA, FeatureList),
                list.member(ConflictB, FeatureList)
            ->
                FeatureListStr = describe_error_term(VarSet, FeatureListTerm),
                Pieces = [words("Error: conflicting features in feature set:"),
                    nl, words(FeatureListStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(FeatureListTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            ;
                (
                    FeatureList = [],
                    ItemNothing = item_nothing_info(no, Context, SeqNum),
                    Item = item_nothing(ItemNothing)
                ;
                    FeatureList = [_ | _],
                    FeatureSet = set.from_list(FeatureList),
                    RFSInfo = pragma_info_require_feature_set(FeatureSet),
                    Pragma = pragma_require_feature_set(RFSInfo),
                    ItemPragma = item_pragma_info(user, Pragma, Context,
                        SeqNum),
                    Item = item_pragma(ItemPragma)
                ),
                MaybeItem = ok1(Item)
            )
        ;
            MaybeFeatureList = error1(Specs),
            MaybeItem = error1(Specs)
        )
    ;
        Pieces = [words("Syntax error in"),
            quote(":- pragma require_feature_set"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

%----------------------------------------------------------------------------%

:- pred parse_foreign_decl_is_local(term::in, foreign_decl_is_local::out)
    is semidet.

parse_foreign_decl_is_local(term.functor(Functor, [], _), IsLocal) :-
    (
        Functor = term.string(String)
    ;
        Functor = term.atom(String)
    ),
    (
        String = "local",
        IsLocal = foreign_decl_is_local
    ;
        String = "exported",
        IsLocal = foreign_decl_is_exported
    ).

:- pred parse_foreign_literal_or_include(term::in,
    foreign_literal_or_include::out) is semidet.

parse_foreign_literal_or_include(Term, LiteralOrInclude) :-
    Term = term.functor(Functor, Args, _),
    (
        Functor = term.string(Code),
        Args = [],
        LiteralOrInclude = literal(Code)
    ;
        Functor = term.atom("include_file"),
        Args = [term.functor(term.string(FileName), [], _)],
        LiteralOrInclude = include_file(FileName)
    ).

parse_foreign_language(term.functor(term.string(String), _, _), Lang) :-
    globals.convert_foreign_language(String, Lang).
parse_foreign_language(term.functor(term.atom(String), _, _), Lang) :-
    globals.convert_foreign_language(String, Lang).

:- pred parse_foreign_language_type(term::in, varset::in, foreign_language::in,
    maybe1(foreign_language_type)::out) is det.

parse_foreign_language_type(InputTerm, VarSet, Language,
        MaybeForeignLangType) :-
    (
        Language = lang_il,
        ( InputTerm = term.functor(term.string(ILTypeName), [], _) ->
            parse_il_type_name(ILTypeName, InputTerm, VarSet,
                MaybeForeignLangType)
        ;
            InputTermStr = describe_error_term(VarSet, InputTerm),
            Pieces = [words("Error: invalid backend specification"),
                quote(InputTermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(InputTerm), [always(Pieces)])]),
            MaybeForeignLangType = error1([Spec])
        )
    ;
        Language = lang_c,
        ( InputTerm = term.functor(term.string(CTypeName), [], _) ->
            MaybeForeignLangType = ok1(c(c_type(CTypeName)))
        ;
            InputTermStr = describe_error_term(VarSet, InputTerm),
            Pieces = [words("Error: invalid backend specification"),
                quote(InputTermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(InputTerm), [always(Pieces)])]),
            MaybeForeignLangType = error1([Spec])
        )
    ;
        Language = lang_java,
        ( InputTerm = term.functor(term.string(JavaTypeName), [], _) ->
            MaybeForeignLangType = ok1(java(java_type(JavaTypeName)))
        ;
            InputTermStr = describe_error_term(VarSet, InputTerm),
            Pieces = [words("Error: invalid backend specification"),
                quote(InputTermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(InputTerm), [always(Pieces)])]),
            MaybeForeignLangType = error1([Spec])
        )
    ;
        Language = lang_csharp,
        ( InputTerm = term.functor(term.string(CSharpTypeName), [], _) ->
            MaybeForeignLangType = ok1(csharp(csharp_type(CSharpTypeName)))
        ;
            InputTermStr = describe_error_term(VarSet, InputTerm),
            Pieces = [words("Error: invalid backend specification"),
                quote(InputTermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(InputTerm), [always(Pieces)])]),
            MaybeForeignLangType = error1([Spec])
        )
    ;
        Language = lang_erlang,
        ( InputTerm = term.functor(term.string(_ErlangTypeName), [], _) ->
            % XXX should we check if the type is blank?
            MaybeForeignLangType = ok1(erlang(erlang_type))
        ;
            InputTermStr = describe_error_term(VarSet, InputTerm),
            Pieces = [words("Error: invalid backend specification"),
                quote(InputTermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(InputTerm), [always(Pieces)])]),
            MaybeForeignLangType = error1([Spec])
        )
    ).

:- pred parse_il_type_name(string::in, term::in, varset::in,
    maybe1(foreign_language_type)::out) is det.

parse_il_type_name(String0, ErrorTerm, VarSet, ForeignType) :-
    (
        parse_special_il_type_name(String0, ForeignTypeResult)
    ->
        ForeignType = ok1(il(ForeignTypeResult))
    ;
        string.append("class [", String1, String0),
        string.sub_string_search(String1, "]", Index)
    ->
        string.left(String1, Index, AssemblyName),
        string.split(String1, Index + 1, _, TypeNameStr),
        TypeSymName = string_to_sym_name(TypeNameStr),
        ForeignType = ok1(il(il_type(reference, AssemblyName, TypeSymName)))
    ;
        string.append("valuetype [", String1, String0),
        string.sub_string_search(String1, "]", Index)
    ->
        string.left(String1, Index, AssemblyName),
        string.split(String1, Index + 1, _, TypeNameStr),
        TypeSymName = string_to_sym_name(TypeNameStr),
        ForeignType = ok1(il(il_type(value, AssemblyName, TypeSymName)))
    ;
        TermStr = describe_error_term(VarSet, ErrorTerm),
        Pieces = [words("Error: invalid foreign language type description"),
            quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        ForeignType = error1([Spec])
    ).

    % Parse all the special assembler names for all the builtin types.
    % See Partition I 'Built-In Types' (Section 8.2.2) for the list
    % of all builtin types.
    %
:- pred parse_special_il_type_name(string::in, il_foreign_type::out)
    is semidet.

parse_special_il_type_name("bool", il_type(value, "mscorlib",
        qualified(unqualified("System"), "Boolean"))).
parse_special_il_type_name("char", il_type(value, "mscorlib",
        qualified(unqualified("System"), "Char"))).
parse_special_il_type_name("object", il_type(reference, "mscorlib",
        qualified(unqualified("System"), "Object"))).
parse_special_il_type_name("string", il_type(reference, "mscorlib",
        qualified(unqualified("System"), "String"))).
parse_special_il_type_name("float32", il_type(value, "mscorlib",
        qualified(unqualified("System"), "Single"))).
parse_special_il_type_name("float64", il_type(value, "mscorlib",
        qualified(unqualified("System"), "Double"))).
parse_special_il_type_name("int8", il_type(value, "mscorlib",
        qualified(unqualified("System"), "SByte"))).
parse_special_il_type_name("int16", il_type(value, "mscorlib",
        qualified(unqualified("System"), "Int16"))).
parse_special_il_type_name("int32", il_type(value, "mscorlib",
        qualified(unqualified("System"), "Int32"))).
parse_special_il_type_name("int64", il_type(value, "mscorlib",
        qualified(unqualified("System"), "Int64"))).
parse_special_il_type_name("natural int", il_type(value, "mscorlib",
        qualified(unqualified("System"), "IntPtr"))).
parse_special_il_type_name("native int", il_type(value, "mscorlib",
        qualified(unqualified("System"), "IntPtr"))).
parse_special_il_type_name("natural unsigned int", il_type(value, "mscorlib",
        qualified(unqualified("System"), "UIntPtr"))).
parse_special_il_type_name("native unsigned int", il_type(value, "mscorlib",
        qualified(unqualified("System"), "UIntPtr"))).
parse_special_il_type_name("refany", il_type(value, "mscorlib",
        qualified(unqualified("System"), "TypedReference"))).
parse_special_il_type_name("typedref", il_type(value, "mscorlib",
        qualified(unqualified("System"), "TypedReference"))).
parse_special_il_type_name("unsigned int8", il_type(value, "mscorlib",
        qualified(unqualified("System"), "Byte"))).
parse_special_il_type_name("unsigned int16", il_type(value, "mscorlib",
        qualified(unqualified("System"), "UInt16"))).
parse_special_il_type_name("unsigned int32", il_type(value, "mscorlib",
        qualified(unqualified("System"), "UInt32"))).
parse_special_il_type_name("unsigned int64", il_type(value, "mscorlib",
        qualified(unqualified("System"), "UInt64"))).

:- pred parse_maybe_foreign_type_assertions(maybe(term)::in,
    list(foreign_type_assertion)::out) is semidet.

parse_maybe_foreign_type_assertions(no, []).
parse_maybe_foreign_type_assertions(yes(Term), Assertions) :-
    parse_foreign_type_assertions(Term, Assertions).

:- pred parse_foreign_type_assertions(term::in,
    list(foreign_type_assertion)::out) is semidet.

parse_foreign_type_assertions(Term, Assertions) :-
    ( Term = term.functor(term.atom("[]"), [], _) ->
        Assertions = []
    ;
        Term = term.functor(term.atom("[|]"), [Head, Tail], _),
        parse_foreign_type_assertion(Head, HeadAssertion),
        parse_foreign_type_assertions(Tail, TailAssertions),
        Assertions = [HeadAssertion | TailAssertions]
    ).

:- pred parse_foreign_type_assertion(term::in,
    foreign_type_assertion::out) is semidet.

parse_foreign_type_assertion(Term, Assertion) :-
    Term = term.functor(term.atom(Constant), [], _),
    Constant = "can_pass_as_mercury_type",
    Assertion = foreign_type_can_pass_as_mercury_type.
parse_foreign_type_assertion(Term, Assertion) :-
    Term = term.functor(term.atom(Constant), [], _),
    Constant = "stable",
    Assertion = foreign_type_stable.

    % This predicate parses foreign_decl pragmas.
    %
:- pred parse_pragma_foreign_decl_pragma(module_name::in, string::in,
    list(term)::in, term::in, varset::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_pragma_foreign_decl_pragma(_ModuleName, PragmaName, PragmaTerms,
        ErrorTerm, VarSet, Context, SeqNum, MaybeItem) :-
    InvalidDeclPieces = [words("Error: invalid"),
        quote(":- pragma " ++ PragmaName), words("declaration:")],
    (
        (
            PragmaTerms = [LangTerm, HeaderTerm],
            IsLocal = foreign_decl_is_exported
        ;
            PragmaTerms = [LangTerm, IsLocalTerm, HeaderTerm],
            parse_foreign_decl_is_local(IsLocalTerm, IsLocal)
        )
    ->
        ( parse_foreign_language(LangTerm, ForeignLanguage) ->
            ( parse_foreign_literal_or_include(HeaderTerm, LiteralOrInclude) ->
                FDInfo = pragma_info_foreign_decl(ForeignLanguage, IsLocal,
                    LiteralOrInclude),
                Pragma = pragma_foreign_decl(FDInfo),
                ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
                Item = item_pragma(ItemPragma),
                MaybeItem = ok1(Item)
            ;
                Pieces = InvalidDeclPieces ++
                    [words("expected string or include_file for"),
                    words("foreign declaration code."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeaderTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        ;
            Pieces = InvalidDeclPieces ++
                [words("invalid language parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(LangTerm),
                    [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        TermStr = describe_error_term(VarSet, ErrorTerm),
        Pieces = [words("Error: invalid"), quote(":- pragma " ++ PragmaName),
            words("declaration:"), words(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

    % This predicate parses foreign_code pragmas.
    % Processing of foreign_proc pragmas is handled in
    % parse_pragma_foreign_proc_pragma below.
    %
:- pred parse_pragma_foreign_code_pragma(module_name::in, string::in,
    list(term)::in, term::in, varset::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_pragma_foreign_code_pragma(_ModuleName, PragmaName, PragmaTerms,
        ErrorTerm, _VarSet, Context, SeqNum, MaybeItem) :-
    InvalidDeclPrefix = [words("Error: invalid"),
        quote(":- pragma " ++ PragmaName), words("declaration:")],
    ( PragmaTerms = [LangTerm, CodeTerm] ->
        ( parse_foreign_language(LangTerm, ForeignLanguagePrime) ->
            ForeignLanguage = ForeignLanguagePrime,
            LangSpecs = []
        ;
            ForeignLanguage = lang_c,   % Dummy, ignored when LangSpecs \= []
            LangPieces = InvalidDeclPrefix ++
                [words("invalid language parameter."), nl],
            LangSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(LangTerm),
                    [always(LangPieces)])]),
            LangSpecs = [LangSpec]
        ),
        ( parse_foreign_literal_or_include(CodeTerm, CodePrime) ->
            Code = CodePrime,
            CodeSpecs = []
        ;
            Code = literal(""), % Dummy, ignored when CodeSpecs \= []
            CodePieces = InvalidDeclPrefix ++
                [words("expected string for foreign code."), nl],
            CodeSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(CodeTerm),
                    [always(CodePieces)])]),
            CodeSpecs = [CodeSpec]
        ),
        Specs = LangSpecs ++ CodeSpecs,
        (
            Specs = [],
            FCInfo = pragma_info_foreign_code(ForeignLanguage, Code),
            Pragma = pragma_foreign_code(FCInfo),
            ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeItem = ok1(Item)
        ;
            Specs = [_ | _],
            MaybeItem = error1(Specs)
        )
    ;
        Pieces = InvalidDeclPrefix ++
            [words("wrong number of arguments."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

    % This predicate parses foreign_proc pragmas.
    %
:- pred parse_pragma_foreign_proc_pragma(module_name::in,
    string::in, list(term)::in, term::in, varset::in, prog_context::in,
    int::in, maybe1(item)::out) is det.

parse_pragma_foreign_proc_pragma(ModuleName, PragmaName, PragmaTerms,
        ErrorTerm, VarSet, Context, SeqNum, MaybeItem) :-
    InvalidDeclPrefix = [words("Error: invalid"),
        quote(":- pragma " ++ PragmaName), words("declaration:")],
    (
        PragmaTerms = [LangTerm | RestTerms],
        ( parse_foreign_language(LangTerm, ForeignLanguagePrime) ->
            ForeignLanguage = ForeignLanguagePrime,
            LangSpecs = []
        ;
            ForeignLanguage = lang_c,   % Dummy, ignored when LangSpecs \= []
            LangPieces = InvalidDeclPrefix ++
                [words("invalid language parameter."), nl],
            LangSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(LangTerm),
                    [always(LangPieces)])]),
            LangSpecs = [LangSpec]
        ),
        (
            RestTerms = [PredAndVarsTerm, FlagsTerm, CodeTerm],
            parse_pragma_ordinary_foreign_proc_pragma(ModuleName,
                VarSet, PredAndVarsTerm, FlagsTerm, CodeTerm,
                ForeignLanguage, InvalidDeclPrefix, Context, SeqNum,
                MaybeRestItem)
        ->
            (
                MaybeRestItem = ok1(Item),
                (
                    LangSpecs = [],
                    MaybeItem = ok1(Item)
                ;
                    LangSpecs = [_ | _],
                    MaybeItem = error1(LangSpecs)
                )
            ;
                MaybeRestItem = error1(RestSpecs),
                MaybeItem = error1(LangSpecs ++ RestSpecs)
            )
        ;
            Pieces = InvalidDeclPrefix ++
                [words("wrong number of arguments."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        PragmaTerms = [],
        PragmaTerms = [],
        Pieces = InvalidDeclPrefix ++
            [words("wrong number of arguments."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_pragma_ordinary_foreign_proc_pragma(module_name::in,
    varset::in, term::in, term::in, term::in, foreign_language::in,
    list(format_component)::in, prog_context::in, int::in, maybe1(item)::out)
    is det.

parse_pragma_ordinary_foreign_proc_pragma(ModuleName, VarSet, SecondTerm,
        ThirdTerm, CodeTerm, ForeignLanguage, InvalidDeclPrefix,
        Context, SeqNum, MaybeItem) :-
    CodeContext = get_term_context(CodeTerm),
    ( CodeTerm = term.functor(term.string(CodePrime), [], _) ->
        Code = CodePrime,
        CodeSpecs = []
    ;
        Code = "", % Dummy
        CodePieces = InvalidDeclPrefix ++
            [words("invalid fourth argument,"),
            words("expecting string containing foreign code."), nl],
        CodeSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(CodeContext, [always(CodePieces)])]),
        CodeSpecs = [CodeSpec]
    ),
    ThirdContextPieces =
        InvalidDeclPrefix ++ [words("invalid third argument:")],
    parse_pragma_foreign_proc_attributes_term(ForeignLanguage, VarSet,
        ThirdTerm, ThirdContextPieces, MaybeFlagsThird),
    (
        MaybeFlagsThird = ok1(Flags),
        FlagsSpecs = [],
        PredAndVarsTerm = SecondTerm
    ;
        MaybeFlagsThird = error1(_FlagsThirdSpecs),
        % We report any errors as appropriate to the preferred syntax.
        SecondContextPieces = InvalidDeclPrefix ++
            [lower_case_next_if_not_first, words("Invalid second argument:")],
        parse_pragma_foreign_proc_attributes_term(ForeignLanguage, VarSet,
            SecondTerm, SecondContextPieces, MaybeFlagsSecond),
        (
            MaybeFlagsSecond = ok1(Flags),
            PredAndVarsTerm = ThirdTerm,                % Dummy
            FlagsPieces = InvalidDeclPrefix ++
                [words("invalid second argument,"),
                words("expecting predicate or function mode."), nl],
            FlagsSpec = error_spec(severity_error,
                phase_term_to_parse_tree,
                [simple_msg(get_term_context(SecondTerm),
                    [always(FlagsPieces)])]),
            FlagsSpecs = [FlagsSpec]
        ;
            MaybeFlagsSecond = error1(FlagsSpecs),
            Flags = default_attributes(ForeignLanguage),    % Dummy
            PredAndVarsTerm = SecondTerm                    % Dummy
        )
    ),
    Specs = CodeSpecs ++ FlagsSpecs,
    (
        Specs = [],
        Impl = fp_impl_ordinary(Code, yes(CodeContext)),
        parse_pragma_foreign_proc(ModuleName, Flags, PredAndVarsTerm,
            Impl, VarSet, Context, SeqNum, MaybeItem)
    ;
        Specs = [_ | _],
        MaybeItem = error1(Specs)
    ).

    % Parse the sole argument of a pragma that should contain
    % a symbol name / arity pair.
    %
:- pred parse_name_arity_pragma(module_name::in, string::in, string::in,
    pred(sym_name, int, pragma_type)::(pred(in, in, out) is det),
    list(term)::in, term::in, varset::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_name_arity_pragma(ModuleName, PragmaName, NameKind, MakePragma,
        PragmaTerms, ErrorTerm, VarSet, Context, SeqNum, MaybeItem) :-
    ( PragmaTerms = [NameAndArityTerm] ->
        parse_simple_name_and_arity(ModuleName, PragmaName, NameKind,
            NameAndArityTerm, NameAndArityTerm, VarSet, MaybeNameAndArity),
        (
            MaybeNameAndArity = ok2(Name, Arity),
            MakePragma(Name, Arity, Pragma),
            ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeItem = ok1(Item)
        ;
            MaybeNameAndArity = error2(Specs),
            MaybeItem = error1(Specs)
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma " ++ PragmaName), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
   ).

:- pred parse_pred_name_and_arity(module_name::in, string::in, term::in,
    term::in, varset::in, maybe2(sym_name, arity)::out) is det.

parse_pred_name_and_arity(ModuleName, PragmaName, NameAndArityTerm, ErrorTerm,
        VarSet, MaybeNameAndArity) :-
    parse_simple_name_and_arity(ModuleName, PragmaName,
        "predicate or function", NameAndArityTerm, ErrorTerm, VarSet,
        MaybeNameAndArity).

:- pred parse_simple_name_and_arity(module_name::in, string::in, string::in,
    term::in, term::in, varset::in, maybe2(sym_name, arity)::out) is det.

parse_simple_name_and_arity(ModuleName, PragmaName, NameKind,
        NameAndArityTerm, ErrorTerm, VarSet, MaybeNameAndArity) :-
    ( parse_name_and_arity(ModuleName, NameAndArityTerm, Name, Arity) ->
        MaybeNameAndArity = ok2(Name, Arity)
    ;
        NameAndArityTermStr = describe_error_term(VarSet, NameAndArityTerm),
        Pieces = [words("Error: expected"), words(NameKind),
            words("name/arity for"), quote(":- pragma " ++ PragmaName),
            words("declaration, not"), quote(NameAndArityTermStr),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeNameAndArity = error2([Spec])
    ).

%-----------------------------------------------------------------------------%

:- pred parse_pragma_keyword(string::in, term::in, string::out,
    term.context::out) is semidet.

parse_pragma_keyword(ExpectedKeyword, Term, StringArg, StartContext) :-
    Term = term.functor(term.atom(ExpectedKeyword), [Arg], _),
    Arg = term.functor(term.string(StringArg), [], StartContext).

%-----------------------------------------------------------------------------%

:- type collected_pragma_foreign_proc_attribute
    --->    coll_may_call_mercury(proc_may_call_mercury)
    ;       coll_thread_safe(proc_thread_safe)
    ;       coll_tabled_for_io(proc_tabled_for_io)
    ;       coll_purity(purity)
    ;       coll_user_annotated_sharing(user_annotated_sharing)
    ;       coll_max_stack_size(int)
    ;       coll_backend(backend)
    ;       coll_terminates(proc_terminates)
    ;       coll_will_not_throw_exception
    ;       coll_ordinary_despite_detism
    ;       coll_may_modify_trail(proc_may_modify_trail)
    ;       coll_may_call_mm_tabled(may_call_mm_tabled)
    ;       coll_box_policy(box_policy)
    ;       coll_affects_liveness(proc_affects_liveness)
    ;       coll_allocates_memory(proc_allocates_memory)
    ;       coll_registers_roots(proc_registers_roots)
    ;       coll_may_duplicate(proc_may_duplicate).

:- pred parse_pragma_foreign_proc_attributes_term(foreign_language::in,
    varset::in, term::in, list(format_component)::in,
    maybe1(pragma_foreign_proc_attributes)::out) is det.

parse_pragma_foreign_proc_attributes_term(ForeignLanguage, Varset,
        Term, ContextPieces, MaybeAttributes) :-
    Attributes0 = default_attributes(ForeignLanguage),
    ConflictingAttributes = [
        coll_may_call_mercury(proc_will_not_call_mercury) -
            coll_may_call_mercury(proc_may_call_mercury),
        coll_thread_safe(proc_thread_safe) -
            coll_thread_safe(proc_not_thread_safe),
        coll_tabled_for_io(proc_tabled_for_io) -
            coll_tabled_for_io(proc_tabled_for_io_unitize),
        coll_tabled_for_io(proc_tabled_for_io) -
            coll_tabled_for_io(proc_tabled_for_descendant_io),
        coll_tabled_for_io(proc_tabled_for_io) -
            coll_tabled_for_io(proc_not_tabled_for_io),
        coll_tabled_for_io(proc_tabled_for_io_unitize) -
            coll_tabled_for_io(proc_tabled_for_descendant_io),
        coll_tabled_for_io(proc_tabled_for_io_unitize) -
            coll_tabled_for_io(proc_not_tabled_for_io),
        coll_tabled_for_io(proc_tabled_for_descendant_io) -
            coll_tabled_for_io(proc_not_tabled_for_io),
        coll_purity(purity_pure) - coll_purity(purity_impure),
        coll_purity(purity_pure) - coll_purity(purity_semipure),
        coll_purity(purity_semipure) - coll_purity(purity_impure),
        coll_terminates(proc_terminates) -
            coll_terminates(proc_does_not_terminate),
        coll_terminates(depends_on_mercury_calls) -
            coll_terminates(proc_terminates),
        coll_terminates(depends_on_mercury_calls) -
            coll_terminates(proc_does_not_terminate),
        coll_may_modify_trail(proc_may_modify_trail) -
            coll_may_modify_trail(proc_will_not_modify_trail),
        coll_may_call_mercury(proc_will_not_call_mercury) -
            coll_may_call_mm_tabled(may_call_mm_tabled),
        coll_box_policy(native_if_possible) - coll_box_policy(always_boxed),
        coll_affects_liveness(proc_affects_liveness) -
            coll_affects_liveness(proc_does_not_affect_liveness),
        coll_allocates_memory(proc_does_not_allocate_memory) -
            coll_allocates_memory(proc_allocates_bounded_memory),
        coll_allocates_memory(proc_does_not_allocate_memory) -
            coll_allocates_memory(proc_allocates_unbounded_memory),
        coll_allocates_memory(proc_allocates_bounded_memory) -
            coll_allocates_memory(proc_allocates_unbounded_memory),
        coll_registers_roots(proc_does_not_register_roots) -
            coll_registers_roots(proc_registers_roots),
        coll_registers_roots(proc_does_not_register_roots) -
            coll_registers_roots(proc_does_not_have_roots),
        coll_registers_roots(proc_registers_roots) -
            coll_registers_roots(proc_does_not_have_roots),
        coll_may_duplicate(proc_may_duplicate) -
            coll_may_duplicate(proc_may_not_duplicate)
    ],
    ( parse_pragma_foreign_proc_attributes_term0(Varset, Term, AttrList) ->
        (
            some [Conflict1, Conflict2] (
                list.member(Conflict1 - Conflict2, ConflictingAttributes),
                list.member(Conflict1, AttrList),
                list.member(Conflict2, AttrList)
            )
        ->
            % We could include Conflict1 and Conflict2 in the message,
            % but the conflict is usually very obvious even without this.
            Pieces = ContextPieces ++ [lower_case_next_if_not_first,
                words("Conflicting attributes in attribute list."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeAttributes = error1([Spec])
        ;
            list.foldl(process_attribute, AttrList, Attributes0, Attributes),
            MaybeAttributes = check_required_attributes(ForeignLanguage,
                Attributes, ContextPieces, Term)
        )
    ;
        % XXX We should say we are expecting just a list.
        Pieces = ContextPieces ++ [lower_case_next_if_not_first,
            words("Expecting a foreign proc attribute"),
            words("or list of attributes."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeAttributes = error1([Spec])
    ).

    % Update the pragma_foreign_proc_attributes according to the given
    % collected_pragma_foreign_proc_attribute.
    %
:- pred process_attribute(collected_pragma_foreign_proc_attribute::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

process_attribute(coll_may_call_mercury(MayCallMercury), !Attrs) :-
    set_may_call_mercury(MayCallMercury, !Attrs).
process_attribute(coll_thread_safe(ThreadSafe), !Attrs) :-
    set_thread_safe(ThreadSafe, !Attrs).
process_attribute(coll_tabled_for_io(TabledForIO), !Attrs) :-
    set_tabled_for_io(TabledForIO, !Attrs).
process_attribute(coll_purity(Pure), !Attrs) :-
    set_purity(Pure, !Attrs).
process_attribute(coll_terminates(Terminates), !Attrs) :-
    set_terminates(Terminates, !Attrs).
process_attribute(coll_user_annotated_sharing(UserSharing), !Attrs) :-
    set_user_annotated_sharing(UserSharing, !Attrs).
process_attribute(coll_will_not_throw_exception, !Attrs) :-
    set_may_throw_exception(proc_will_not_throw_exception, !Attrs).
process_attribute(coll_max_stack_size(Size), !Attrs) :-
    add_extra_attribute(max_stack_size(Size), !Attrs).
process_attribute(coll_backend(Backend), !Attrs) :-
    add_extra_attribute(backend(Backend), !Attrs).
process_attribute(coll_ordinary_despite_detism, !Attrs) :-
    set_ordinary_despite_detism(yes, !Attrs).
process_attribute(coll_may_modify_trail(TrailMod), !Attrs) :-
    set_may_modify_trail(TrailMod, !Attrs).
process_attribute(coll_may_call_mm_tabled(MayCallTabled), !Attrs) :-
    set_may_call_mm_tabled(MayCallTabled, !Attrs).
process_attribute(coll_box_policy(BoxPolicy), !Attrs) :-
    set_box_policy(BoxPolicy, !Attrs).
process_attribute(coll_affects_liveness(AffectsLiveness), !Attrs) :-
    set_affects_liveness(AffectsLiveness, !Attrs).
process_attribute(coll_allocates_memory(AllocatesMemory), !Attrs) :-
    set_allocates_memory(AllocatesMemory, !Attrs).
process_attribute(coll_registers_roots(RegistersRoots), !Attrs) :-
    set_registers_roots(RegistersRoots, !Attrs).
process_attribute(coll_may_duplicate(MayDuplicate), !Attrs) :-
    set_may_duplicate(yes(MayDuplicate), !Attrs).

    % Check whether all the required attributes have been set for
    % a particular language
    %
:- func check_required_attributes(foreign_language,
        pragma_foreign_proc_attributes, list(format_component), term)
    = maybe1(pragma_foreign_proc_attributes).

check_required_attributes(lang_c, Attrs, _CP, _Term) = ok1(Attrs).
check_required_attributes(lang_csharp, Attrs, _CP, _Term) = ok1(Attrs).
check_required_attributes(lang_il, Attrs, ContextPieces, Term) =
        MaybeAttributes :-
    MaxStackAttrs = list.filter_map(
        (func(X) = X is semidet :-
            X = max_stack_size(_)),
        get_extra_attributes(Attrs)),
    (
        MaxStackAttrs = [],
        Pieces = ContextPieces ++ [lower_case_next_if_not_first,
            words("Error: expecting max_stack_size attribute for IL code."),
            nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeAttributes = error1([Spec])
    ;
        MaxStackAttrs = [_ | _],
        MaybeAttributes = ok1(Attrs)
    ).
check_required_attributes(lang_java, Attrs, _CP, _Term) = ok1(Attrs).
check_required_attributes(lang_erlang, Attrs, _CP, _Term) = ok1(Attrs).

:- pred parse_pragma_foreign_proc_attributes_term0(varset::in, term::in,
    list(collected_pragma_foreign_proc_attribute)::out) is semidet.

parse_pragma_foreign_proc_attributes_term0(Varset, Term, Flags) :-
    ( parse_single_pragma_foreign_proc_attribute(Varset, Term, Flag) ->
        Flags = [Flag]
    ;
        (
            Term = term.functor(term.atom("[]"), [], _),
            Flags = []
        ;
            Term = term.functor(term.atom("[|]"), [Head, Tail], _),
            parse_single_pragma_foreign_proc_attribute(Varset, Head, HeadFlag),
            parse_pragma_foreign_proc_attributes_term0(Varset, Tail,
                TailFlags),
            Flags = [HeadFlag | TailFlags]
        )
    ).

:- pred parse_single_pragma_foreign_proc_attribute(varset::in, term::in,
    collected_pragma_foreign_proc_attribute::out) is semidet.

parse_single_pragma_foreign_proc_attribute(Varset, Term, Flag) :-
    ( parse_may_call_mercury(Term, MayCallMercury) ->
        Flag = coll_may_call_mercury(MayCallMercury)
    ; parse_threadsafe(Term, ThreadSafe) ->
        Flag = coll_thread_safe(ThreadSafe)
    ; parse_tabled_for_io(Term, TabledForIo) ->
        Flag = coll_tabled_for_io(TabledForIo)
    ; parse_user_annotated_sharing(Varset, Term, UserSharing) ->
        Flag = coll_user_annotated_sharing(UserSharing)
    ; parse_max_stack_size(Term, Size) ->
        Flag = coll_max_stack_size(Size)
    ; parse_backend(Term, Backend) ->
        Flag = coll_backend(Backend)
    ; parse_purity_promise(Term, Purity) ->
        Flag = coll_purity(Purity)
    ; parse_terminates(Term, Terminates) ->
        Flag = coll_terminates(Terminates)
    ; parse_no_exception_promise(Term) ->
        Flag = coll_will_not_throw_exception
    ; parse_ordinary_despite_detism(Term) ->
        Flag = coll_ordinary_despite_detism
    ; parse_may_modify_trail(Term, TrailMod) ->
        Flag = coll_may_modify_trail(TrailMod)
    ; parse_may_call_mm_tabled(Term, CallsTabled) ->
        Flag = coll_may_call_mm_tabled(CallsTabled)
    ; parse_box_policy(Term, BoxPolicy) ->
        Flag = coll_box_policy(BoxPolicy)
    ; parse_affects_liveness(Term, AffectsLiveness) ->
        Flag = coll_affects_liveness(AffectsLiveness)
    ; parse_allocates_memory(Term, AllocatesMemory) ->
        Flag = coll_allocates_memory(AllocatesMemory)
    ; parse_registers_roots(Term, RegistersRoots) ->
        Flag = coll_registers_roots(RegistersRoots)
    ; parse_may_duplicate(Term, MayDuplicate) ->
        Flag = coll_may_duplicate(MayDuplicate)
    ;
        fail
    ).

:- pred parse_may_call_mercury(term::in, proc_may_call_mercury::out)
    is semidet.

parse_may_call_mercury(term.functor(term.atom("recursive"), [], _),
    proc_may_call_mercury).
parse_may_call_mercury(term.functor(term.atom("non_recursive"), [], _),
    proc_will_not_call_mercury).
parse_may_call_mercury(term.functor(term.atom("may_call_mercury"), [], _),
    proc_may_call_mercury).
parse_may_call_mercury(term.functor(term.atom("will_not_call_mercury"), [], _),
    proc_will_not_call_mercury).

:- pred parse_threadsafe(term::in, proc_thread_safe::out) is semidet.

parse_threadsafe(term.functor(term.atom("thread_safe"), [], _),
    proc_thread_safe).
parse_threadsafe(term.functor(term.atom("not_thread_safe"), [], _),
    proc_not_thread_safe).
parse_threadsafe(term.functor(term.atom("maybe_thread_safe"), [], _),
    proc_maybe_thread_safe).

:- pred parse_may_modify_trail(term::in, proc_may_modify_trail::out)
    is semidet.

parse_may_modify_trail(term.functor(term.atom("may_modify_trail"), [], _),
    proc_may_modify_trail).
parse_may_modify_trail(term.functor(term.atom("will_not_modify_trail"), [], _),
    proc_will_not_modify_trail).

:- pred parse_may_call_mm_tabled(term::in, may_call_mm_tabled::out) is semidet.

parse_may_call_mm_tabled(Term, may_call_mm_tabled) :-
    Term = term.functor(term.atom("may_call_mm_tabled"), [], _).
parse_may_call_mm_tabled(Term, will_not_call_mm_tabled) :-
    Term = term.functor(term.atom("will_not_call_mm_tabled"), [], _).

:- pred parse_box_policy(term::in, box_policy::out) is semidet.

parse_box_policy(term.functor(term.atom("native_if_possible"), [], _),
    native_if_possible).
parse_box_policy(term.functor(term.atom("always_boxed"), [], _),
    always_boxed).

:- pred parse_affects_liveness(term::in, proc_affects_liveness::out)
    is semidet.

parse_affects_liveness(Term, AffectsLiveness) :-
    Term = term.functor(term.atom(Functor), [], _),
    (
        Functor = "affects_liveness",
        AffectsLiveness = proc_affects_liveness
    ;
        ( Functor = "doesnt_affect_liveness"
        ; Functor = "does_not_affect_liveness"
        ),
        AffectsLiveness = proc_does_not_affect_liveness
    ).

:- pred parse_allocates_memory(term::in, proc_allocates_memory::out)
    is semidet.

parse_allocates_memory(Term, AllocatesMemory) :-
    Term = term.functor(term.atom(Functor), [], _),
    (
        ( Functor = "doesnt_allocate_memory"
        ; Functor = "does_not_allocate_memory"
        ),
        AllocatesMemory = proc_does_not_allocate_memory
    ;
        Functor = "allocates_bounded_memory",
        AllocatesMemory = proc_allocates_bounded_memory
    ;
        Functor = "allocates_unbounded_memory",
        AllocatesMemory = proc_allocates_unbounded_memory
    ).

:- pred parse_registers_roots(term::in, proc_registers_roots::out) is semidet.

parse_registers_roots(Term, RegistersRoots) :-
    Term = term.functor(term.atom(Functor), [], _),
    (
        Functor = "registers_roots",
        RegistersRoots = proc_registers_roots
    ;
        ( Functor = "doesnt_register_roots"
        ; Functor = "does_not_register_roots"
        ),
        RegistersRoots = proc_does_not_register_roots
    ;
        ( Functor = "doesnt_have_roots"
        ; Functor = "does_not_have_roots"
        ),
        RegistersRoots = proc_does_not_have_roots
    ).

:- pred parse_may_duplicate(term::in, proc_may_duplicate::out) is semidet.

parse_may_duplicate(Term, RegistersRoots) :-
    Term = term.functor(term.atom(Functor), [], _),
    (
        Functor = "may_duplicate",
        RegistersRoots = proc_may_duplicate
    ;
        Functor = "may_not_duplicate",
        RegistersRoots = proc_may_not_duplicate
    ).

:- pred parse_tabled_for_io(term::in, proc_tabled_for_io::out) is semidet.

parse_tabled_for_io(term.functor(term.atom(Str), [], _), TabledForIo) :-
    (
        Str = "tabled_for_io",
        TabledForIo = proc_tabled_for_io
    ;
        Str = "tabled_for_io_unitize",
        TabledForIo = proc_tabled_for_io_unitize
    ;
        Str = "tabled_for_descendant_io",
        TabledForIo = proc_tabled_for_descendant_io
    ;
        Str = "not_tabled_for_io",
        TabledForIo = proc_not_tabled_for_io
    ).

:- pred parse_max_stack_size(term::in, int::out) is semidet.

parse_max_stack_size(term.functor(
        term.atom("max_stack_size"), [SizeTerm], _), Size) :-
    SizeTerm = term.functor(term.integer(Size), [], _).

:- pred parse_backend(term::in, backend::out) is semidet.

parse_backend(term.functor(term.atom(Functor), [], _), Backend) :-
    (
        Functor = "high_level_backend",
        Backend = high_level_backend
    ;
        Functor = "low_level_backend",
        Backend = low_level_backend
    ).

:- pred parse_purity_promise(term::in, purity::out) is semidet.

parse_purity_promise(term.functor(term.atom("promise_pure"), [], _),
        purity_pure).
parse_purity_promise(term.functor(term.atom("promise_semipure"), [], _),
        purity_semipure).

:- pred parse_terminates(term::in, proc_terminates::out) is semidet.

parse_terminates(term.functor(term.atom("terminates"), [], _),
        proc_terminates).
parse_terminates(term.functor(term.atom("does_not_terminate"), [], _),
        proc_does_not_terminate).

:- pred parse_no_exception_promise(term::in) is semidet.

parse_no_exception_promise(term.functor(
    term.atom("will_not_throw_exception"), [], _)).

:- pred parse_ordinary_despite_detism(term::in) is semidet.

parse_ordinary_despite_detism(
        term.functor(term.atom("ordinary_despite_detism"), [], _)).

    % Parse a pragma foreign_proc declaration.
    %
:- pred parse_pragma_foreign_proc(module_name::in,
    pragma_foreign_proc_attributes::in, term::in, pragma_foreign_proc_impl::in,
    varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_pragma_foreign_proc(ModuleName, Flags, PredAndVarsTerm0,
        PragmaImpl, VarSet, Context, SeqNum, MaybeItem) :-
    ContextPieces = [words("In"), quote(":- pragma foreign_proc"),
        words("declaration:")],
    parse_pred_or_func_and_args_general(yes(ModuleName), PredAndVarsTerm0,
        VarSet, ContextPieces, MaybePredAndArgs),
    (
        MaybePredAndArgs = ok2(PredName, VarList0 - MaybeRetTerm),
        % Is this a function or a predicate?
        (
            MaybeRetTerm = yes(FuncResultTerm0),
            PredOrFunc = pf_function,
            VarList = VarList0 ++ [FuncResultTerm0]
        ;
            MaybeRetTerm = no,
            PredOrFunc = pf_predicate,
            VarList = VarList0
        ),
        parse_pragma_foreign_proc_varlist(VarSet, VarList, MaybePragmaVars),
        (
            MaybePragmaVars = ok1(PragmaVars),
            varset.coerce(VarSet, ProgVarSet),
            varset.coerce(VarSet, InstVarSet),
            FPInfo = pragma_info_foreign_proc(Flags, PredName, PredOrFunc,
                PragmaVars, ProgVarSet, InstVarSet, PragmaImpl),
            Pragma = pragma_foreign_proc(FPInfo),
            ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeItem = ok1(Item)
        ;
            MaybePragmaVars = error1(Specs),
            MaybeItem = error1(Specs)
        )
    ;
        MaybePredAndArgs = error2(Specs),
        MaybeItem = error1(Specs)
    ).

    % Parse the variable list in the pragma foreign_proc declaration.
    % The final argument is 'no' for no error, or 'yes(ErrorMessage)'.
    %
:- pred parse_pragma_foreign_proc_varlist(varset::in, list(term)::in,
    maybe1(list(pragma_var))::out) is det.

parse_pragma_foreign_proc_varlist(_, [], ok1([])).
parse_pragma_foreign_proc_varlist(VarSet, [HeadTerm | TailTerm],
        MaybePragmaVars):-
    (
        HeadTerm = term.functor(term.atom("::"), [VarTerm, ModeTerm], _),
        VarTerm = term.variable(Var, VarContext)
    ->
        ( varset.search_name(VarSet, Var, VarName) ->
            ( convert_mode(allow_constrained_inst_var, ModeTerm, Mode0) ->
                constrain_inst_vars_in_mode(Mode0, Mode),
                term.coerce_var(Var, ProgVar),
                HeadPragmaVar = pragma_var(ProgVar, VarName, Mode,
                    native_if_possible),
                parse_pragma_foreign_proc_varlist(VarSet, TailTerm,
                    MaybeTailPragmaVars),
                (
                    MaybeTailPragmaVars = ok1(TailPragmaVars),
                    MaybePragmaVars = ok1([HeadPragmaVar | TailPragmaVars])
                ;
                    MaybeTailPragmaVars = error1(_),
                    MaybePragmaVars = MaybeTailPragmaVars
                )
            ;
                Pieces = [
                    words("Error: unknown mode in pragma foreign_proc."), nl
                ],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(ModeTerm),
                        [always(Pieces)])]),
                MaybePragmaVars = error1([Spec])
            )
        ;
            % If the variable wasn't in the varset it must be an
            % underscore variable.
            Pieces = [words("Sorry, not implemented: "),
                words("anonymous `_' variable in pragma foreign_proc."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(VarContext, [always(Pieces)])]),
            MaybePragmaVars = error1([Spec])
        )
    ;
        Pieces = [words("Error: arguments are not in the form"),
            quote("Var :: mode"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
        MaybePragmaVars = error1([Spec])
    ).

:- pred parse_oisu_pragma(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_oisu_pragma(ModuleName, PragmaTerms, ErrorTerm, VarSet, Context, SeqNum,
        MaybeItem) :-
    (
        PragmaTerms = [TypeCtorTerm, CreatorsTerm, MutatorsTerm | OtherTerms],
        (
            OtherTerms = [],
            MaybeDestructorsTerm = no
        ;
            OtherTerms = [DestructorsTerm],
            MaybeDestructorsTerm = yes(DestructorsTerm)
        )
    ->
        ( parse_name_and_arity(ModuleName, TypeCtorTerm, Name, Arity) ->
            MaybeTypeCtor = ok1(type_ctor(Name, Arity))
        ;
            TypeCtorTermStr = describe_error_term(VarSet, TypeCtorTerm),
            Pieces = [words("Error: expected"),
                words("predicate name/arity for first argument of"),
                quote(":- pragma oisu"), words("declaration, not"),
                quote(TypeCtorTermStr), suffix("."), nl],
            TypeCtorSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
            MaybeTypeCtor = error1([TypeCtorSpec])
        ),
        parse_oisu_preds_term(ModuleName, VarSet, "second", "creators",
            CreatorsTerm, MaybeCreatorsNamesArities),
        parse_oisu_preds_term(ModuleName, VarSet, "third", "mutators",
            MutatorsTerm, MaybeMutatorsNamesArities),
        (
            MaybeDestructorsTerm = yes(DestructorsTerm2),
            parse_oisu_preds_term(ModuleName, VarSet, "fourth", "destructors",
                DestructorsTerm2, MaybeDestructorsNamesArities)
        ;
            MaybeDestructorsTerm = no,
            MaybeDestructorsNamesArities = ok1([])
        ),
        (
            MaybeTypeCtor = ok1(TypeCtor),
            MaybeCreatorsNamesArities = ok1(CreatorsNamesArities),
            MaybeMutatorsNamesArities = ok1(MutatorsNamesArities),
            MaybeDestructorsNamesArities = ok1(DestructorsNamesArities)
        ->
            OISUInfo = pragma_info_oisu(TypeCtor, CreatorsNamesArities,
                MutatorsNamesArities, DestructorsNamesArities),
            Pragma = pragma_oisu(OISUInfo),
            ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeItem = ok1(Item)
        ;
            Specs = get_any_errors1(MaybeTypeCtor) ++
                get_any_errors1(MaybeCreatorsNamesArities) ++
                get_any_errors1(MaybeMutatorsNamesArities) ++
                get_any_errors1(MaybeDestructorsNamesArities),
            MaybeItem = error1(Specs)
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma oisu"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
   ).

:- pred parse_oisu_preds_term(module_name::in, varset::in, string::in,
    string::in, term::in, maybe1(list(pred_name_arity))::out) is det.

parse_oisu_preds_term(ModuleName, VarSet, ArgNum, ExpectedFunctor, Term,
        MaybeNamesArities) :-
    (
        Term = term.functor(term.atom(Functor), Args, _),
        Functor = ExpectedFunctor,
        Args = [Arg]
    ->
        parse_name_and_arity_list(ModuleName, VarSet, ExpectedFunctor,
            Arg, MaybeNamesArities)
    ;
        Pieces = [words("Error:"), words(ArgNum), words("argument of"),
            quote(":- pragma oisu"), words("declaration"),
            words("should have the form"),
            quote(ExpectedFunctor ++ "([pred1/arity1, ..., predn/arityn])"),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeNamesArities = error1([Spec])
    ).

:- pred parse_name_and_arity_list(module_name::in, varset::in, string::in,
    term::in, maybe1(list(pred_name_arity))::out) is det.

parse_name_and_arity_list(ModuleName, VarSet, Wrapper, Term,
        MaybeNamesArities) :-
    (
        Term = term.functor(Functor, Args, _),
        (
            Functor = term.atom("[]"),
            Args = []
        ->
            MaybeNamesArities = ok1([])
        ;
            Functor = term.atom("[|]"),
            Args = [Arg1, Arg2]
        ->
            ( parse_name_and_arity(ModuleName, Arg1, Arg1Name, Arg1Arity) ->
                MaybeHeadNameArity = ok1(pred_name_arity(Arg1Name, Arg1Arity))
            ;
                Arg1Str = describe_error_term(VarSet, Arg1),
                Pieces = [words("Error: expected predname/arity,"),
                    words("not"), quote(Arg1Str), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Arg1), [always(Pieces)])]),
                MaybeHeadNameArity = error1([Spec])
            ),
            parse_name_and_arity_list(ModuleName, VarSet, Wrapper, Arg2,
                MaybeTailNamesArities),
            (
                MaybeHeadNameArity = ok1(HeadNameArity),
                MaybeTailNamesArities = ok1(TailNamesArities)
            ->
                MaybeNamesArities = ok1([HeadNameArity | TailNamesArities])
            ;
                HeadSpecs = get_any_errors1(MaybeHeadNameArity),
                TailSpecs = get_any_errors1(MaybeTailNamesArities),
                MaybeNamesArities = error1(HeadSpecs ++ TailSpecs)
            )
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: expected a list as the argument of"),
                words(Wrapper), suffix(","),
                words("not"), quote(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeNamesArities = error1([Spec])
        )
    ;
        Term = term.variable(_, _),
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a list as the argument of"),
            words(Wrapper), suffix(","),
            words("not"), quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeNamesArities = error1([Spec])
    ).

:- pred parse_tabling_pragma(module_name::in, string::in, eval_method::in,
    list(term)::in, term::in, varset::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_tabling_pragma(ModuleName, PragmaName, EvalMethod, PragmaTerms,
        ErrorTerm, VarSet, Context, SeqNum, MaybeItem) :-
    (
        (
            PragmaTerms = [PredAndModesTerm0],
            MaybeAttrs = no
        ;
            PragmaTerms = [PredAndModesTerm0, AttrListTerm0],
            MaybeAttrs = yes(AttrListTerm0)
        )
    ->
        ContextPieces = [words("In"), quote(":- pragma " ++ PragmaName),
            words("declaration:")],
        parse_arity_or_modes(ModuleName, PredAndModesTerm0, ErrorTerm,
            VarSet, ContextPieces, MaybeArityOrModes),
        (
            MaybeArityOrModes = ok1(ArityOrModes),
            ArityOrModes = arity_or_modes(PredName, Arity, MaybePredOrFunc,
                MaybeModes),
            (
                MaybeAttrs = no,
                PredNameArityMPF = pred_name_arity_mpf(PredName, Arity,
                    MaybePredOrFunc),
                TabledInfo = pragma_info_tabled(EvalMethod, PredNameArityMPF,
                    MaybeModes, no),
                Pragma = pragma_tabled(TabledInfo),
                ItemPragma = item_pragma_info(user, Pragma, Context, SeqNum),
                Item = item_pragma(ItemPragma),
                MaybeItem = ok1(Item)
            ;
                MaybeAttrs = yes(AttrsListTerm),
                UnrecognizedPieces =
                    [words("Error: expected tabling attribute."), nl],
                convert_maybe_list("tabling attributes", yes(VarSet),
                    AttrsListTerm, parse_tabling_attribute(VarSet, EvalMethod),
                    UnrecognizedPieces, MaybeAttributeList),
                (
                    MaybeAttributeList = ok1(AttributeList),
                    update_tabling_attributes(AttributeList,
                        default_memo_table_attributes, MaybeAttributes),
                    (
                        MaybeAttributes = ok1(Attributes),
                        PredNameArityMPF = pred_name_arity_mpf(PredName,
                            Arity, MaybePredOrFunc),
                        TabledInfo = pragma_info_tabled(EvalMethod,
                            PredNameArityMPF, MaybeModes, yes(Attributes)),
                        Pragma = pragma_tabled(TabledInfo),
                        ItemPragma = item_pragma_info(user, Pragma, Context,
                            SeqNum),
                        Item = item_pragma(ItemPragma),
                        MaybeItem = ok1(Item)
                    ;
                        MaybeAttributes = error1(Specs),
                        MaybeItem = error1(Specs)
                    )
                ;
                    MaybeAttributeList = error1(Specs),
                    MaybeItem = error1(Specs)
                )
            )
        ;
            MaybeArityOrModes = error1(Specs),
            MaybeItem = error1(Specs)
        )
    ;
        Pieces = [words("Error: wrong number of arguments in"),
            quote(":- pragma " ++ PragmaName), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- type single_tabling_attribute
    --->    attr_strictness(call_table_strictness)
    ;       attr_size_limit(int)
    ;       attr_statistics
    ;       attr_allow_reset.

:- pred update_tabling_attributes(
    assoc_list(term.context, single_tabling_attribute)::in,
    table_attributes::in, maybe1(table_attributes)::out) is det.

update_tabling_attributes([], Attributes, ok1(Attributes)).
update_tabling_attributes([Context - SingleAttr | TermSingleAttrs],
        !.Attributes, MaybeAttributes) :-
    (
        SingleAttr = attr_strictness(Strictness),
        ( !.Attributes ^ table_attr_strictness = all_strict ->
            !Attributes ^ table_attr_strictness := Strictness,
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        ;
            Pieces = [words("Error: duplicate argument tabling methods"),
                words("attribute in"), quote(":- pragma memo"),
                words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeAttributes = error1([Spec])
        )
    ;
        SingleAttr = attr_size_limit(Limit),
        ( !.Attributes ^ table_attr_size_limit = no ->
            !Attributes ^ table_attr_size_limit := yes(Limit),
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        ;
            Pieces = [words("Error: duplicate size limits attribute in"),
                quote(":- pragma memo"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeAttributes = error1([Spec])
        )
    ;
        SingleAttr = attr_statistics,
        (
            !.Attributes ^ table_attr_statistics = table_dont_gather_statistics
        ->
            !Attributes ^ table_attr_statistics := table_gather_statistics,
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        ;
            Pieces = [words("Error: duplicate statistics attribute in"),
                quote(":- pragma memo"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeAttributes = error1([Spec])
        )
    ;
        SingleAttr = attr_allow_reset,
        ( !.Attributes ^ table_attr_allow_reset = table_dont_allow_reset ->
            !Attributes ^ table_attr_allow_reset := table_allow_reset,
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        ;
            Pieces = [words("Error: duplicate allow_reset attribute in"),
                quote(":- pragma memo"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeAttributes = error1([Spec])
        )
    ).

:- pred parse_tabling_attribute(varset::in, eval_method::in, term::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is semidet.

parse_tabling_attribute(VarSet, EvalMethod, Term, MaybeContextAttribute) :-
    Term = term.functor(term.atom(Functor), Args, Context),
    (
        Functor = "fast_loose",
        Args = [],
        ( eval_method_allows_fast_loose(EvalMethod) = yes ->
            Attribute = attr_strictness(all_fast_loose),
            MaybeContextAttribute = ok1(Context - Attribute)
        ;
            Pieces = [words("Error: evaluation method"),
                fixed(eval_method_to_string(EvalMethod)),
                words("does not allow fast_loose tabling."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeContextAttribute = error1([Spec])
        )
    ;
        Functor = "specified",
        Args = [Arg1 | MoreArgs],
        UnrecognizedPieces =
            [words("Error: expected argument tabling method")],
        convert_list("argument tabling methods", yes(VarSet), Arg1,
            parse_arg_tabling_method, UnrecognizedPieces,
            MaybeMaybeArgMethods),
        (
            MaybeMaybeArgMethods = ok1(MaybeArgMethods),
            AllowsFastLoose = eval_method_allows_fast_loose(EvalMethod),
            (
                AllowsFastLoose = yes,
                (
                    MoreArgs = [],
                    Attribute = attr_strictness(
                        specified(MaybeArgMethods, hidden_arg_value)),
                    MaybeContextAttribute = ok1(Context - Attribute)
                ;
                    MoreArgs = [Arg2],
                    (
                        Arg2 = term.functor(
                            term.atom("hidden_arg_value"), [], _)
                    ->
                        Attribute = attr_strictness(
                            specified(MaybeArgMethods, hidden_arg_value)),
                        MaybeContextAttribute = ok1(Context - Attribute)
                    ;
                        Arg2 = term.functor(
                            term.atom("hidden_arg_addr"), [], _)
                    ->
                        Attribute = attr_strictness(
                            specified(MaybeArgMethods, hidden_arg_addr)),
                        MaybeContextAttribute = ok1(Context - Attribute)
                    ;
                        Arg2Str = describe_error_term(VarSet, Arg2),
                        Pieces = [words("Error: expected hidden argument"),
                            words("tabling method, not"),
                            quote(Arg2Str), suffix("."), nl],
                        Spec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(get_term_context(Arg2),
                                [always(Pieces)])]),
                        MaybeContextAttribute = error1([Spec])
                    )
                ;
                    MoreArgs = [_, _ | _],
                    Pieces = [words("Error: expected one or two arguments"),
                        words("for"), quote("specified"), suffix("."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(Context, [always(Pieces)])]),
                    MaybeContextAttribute = error1([Spec])
                )
            ;
                AllowsFastLoose = no,
                Pieces = [words("Error: evaluation method"),
                    fixed(eval_method_to_string(EvalMethod)),
                    words("does not allow specified tabling methods."), nl],
                % XXX Should we use the context from Arg1?
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeContextAttribute = error1([Spec])
            )
        ;
            MaybeMaybeArgMethods = error1(Specs),
            MaybeContextAttribute = error1(Specs)
        )
    ;
        Functor = "size_limit",
        Args = [Arg],
        Arg = term.functor(term.integer(Limit), [], _),
        AllowsSizeLimit = eval_method_allows_size_limit(EvalMethod),
        (
            AllowsSizeLimit = yes,
            Attribute = attr_size_limit(Limit),
            MaybeContextAttribute = ok1(Context - Attribute)
        ;
            AllowsSizeLimit = no,
            Pieces = [words("Error: evaluation method"),
                fixed(eval_method_to_string(EvalMethod)),
                words("does not allow size limits."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeContextAttribute = error1([Spec])
        )
    ;
        Functor = "statistics",
        Args = [],
        Attribute = attr_statistics,
        MaybeContextAttribute = ok1(Context - Attribute)
    ;
        Functor = "allow_reset",
        Args = [],
        Attribute = attr_allow_reset,
        MaybeContextAttribute = ok1(Context - Attribute)
    ).

:- func eval_method_allows_fast_loose(eval_method) = bool.

eval_method_allows_fast_loose(eval_normal) = no.
eval_method_allows_fast_loose(eval_loop_check) = yes.
eval_method_allows_fast_loose(eval_memo) = yes.
eval_method_allows_fast_loose(eval_table_io(_, _)) = no.
eval_method_allows_fast_loose(eval_minimal(_)) = no.

:- func eval_method_allows_size_limit(eval_method) = bool.

eval_method_allows_size_limit(eval_normal) = no.
eval_method_allows_size_limit(eval_loop_check) = yes.
eval_method_allows_size_limit(eval_memo) = yes.
eval_method_allows_size_limit(eval_table_io(_, _)) = no.
eval_method_allows_size_limit(eval_minimal(_)) = no.

:- pred parse_arg_tabling_method(term::in, maybe(arg_tabling_method)::out)
    is semidet.

parse_arg_tabling_method(term.functor(term.atom("value"), [], _),
    yes(arg_value)).
parse_arg_tabling_method(term.functor(term.atom("addr"), [], _),
    yes(arg_addr)).
parse_arg_tabling_method(term.functor(term.atom("promise_implied"), [], _),
    yes(arg_promise_implied)).
parse_arg_tabling_method(term.functor(term.atom("output"), [], _), no).

:- type arity_or_modes
    --->    arity_or_modes(
                sym_name,
                arity,
                maybe(pred_or_func),
                maybe(list(mer_mode))
            ).

:- pred parse_arity_or_modes(module_name::in, term::in, term::in, varset::in,
    list(format_component)::in, maybe1(arity_or_modes)::out) is det.

parse_arity_or_modes(ModuleName, PredAndModesTerm0, ErrorTerm, VarSet,
        ContextPieces, MaybeArityOrModes) :-
    (
        % Is this a simple pred/arity pragma.
        PredAndModesTerm0 = term.functor(term.atom("/"),
            [PredNameTerm, ArityTerm], _)
    ->
        (
            try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
                PredNameTerm, PredName),
            ArityTerm = term.functor(term.integer(Arity), [], _)
        ->
            MaybeArityOrModes = ok1(arity_or_modes(PredName, Arity, no, no))
        ;
            Pieces = ContextPieces ++ [lower_case_next_if_not_first,
                words("Error: expected predname/arity."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
            MaybeArityOrModes = error1([Spec])
        )
    ;
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            PredAndModesTerm0, VarSet, ContextPieces, MaybePredAndModes),
        (
            MaybePredAndModes = ok2(PredName - PredOrFunc, Modes),
            list.length(Modes, Arity0),
            (
                PredOrFunc = pf_function,
                Arity = Arity0 - 1
            ;
                PredOrFunc = pf_predicate,
                Arity = Arity0
            ),
            ArityOrModes = arity_or_modes(PredName, Arity, yes(PredOrFunc),
                yes(Modes)),
            MaybeArityOrModes = ok1(ArityOrModes)
        ;
            MaybePredAndModes = error2(Specs),
            MaybeArityOrModes = error1(Specs)
        )
    ).

% XXX why not maybe3?
:- type maybe_pred_or_func_modes ==
    maybe2(pair(sym_name, pred_or_func), list(mer_mode)).

:- pred parse_pred_or_func_and_arg_modes(maybe(module_name)::in, term::in,
    term::in, varset::in, list(format_component)::in,
    maybe_pred_or_func_modes::out) is det.

parse_pred_or_func_and_arg_modes(MaybeModuleName, PredAndModesTerm,
        ErrorTerm, VarSet, ContextPieces, MaybeNameAndModes) :-
    parse_pred_or_func_and_args_general(MaybeModuleName, PredAndModesTerm,
        VarSet, ContextPieces, MaybePredAndArgs),
    (
        MaybePredAndArgs = ok2(PredName, ArgModeTerms - MaybeRetModeTerm),
        (
            convert_mode_list(allow_constrained_inst_var, ArgModeTerms,
                ArgModes0)
        ->
            (
                MaybeRetModeTerm = yes(RetModeTerm),
                (
                    convert_mode(allow_constrained_inst_var, RetModeTerm,
                        RetMode)
                ->
                    ArgModes1 = ArgModes0 ++ [RetMode],
                    list.map(constrain_inst_vars_in_mode, ArgModes1, ArgModes),
                    MaybeNameAndModes = ok2(PredName - pf_function, ArgModes)
                ;
                    Pieces = [words("Error in return mode in")] ++
                        ContextPieces ++ [suffix("."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(ErrorTerm),
                            [always(Pieces)])]),
                    MaybeNameAndModes = error2([Spec])
                )
            ;
                MaybeRetModeTerm = no,
                MaybeNameAndModes = ok2(PredName - pf_predicate, ArgModes0)
            )
        ;
            Pieces = [words("Error in arguments modes in")] ++
                ContextPieces ++ [suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
            MaybeNameAndModes = error2([Spec])
        )
    ;
        MaybePredAndArgs = error2(Specs),
        MaybeNameAndModes = error2(Specs)
    ).

:- pred convert_bool(term::in, bool::out) is semidet.

convert_bool(Term, Bool) :-
    Term = term.functor(term.atom(Name), [], _),
    ( Name = "yes", Bool = yes
    ; Name = "no", Bool = no
    ).

% XXX Why does convert_bool_list insist on ok when convert_int_list doesn't?
:- pred convert_bool_list(varset::in, term::in, list(bool)::out) is semidet.

convert_bool_list(VarSet, ListTerm, Bools) :-
    convert_list("booleans", yes(VarSet), ListTerm, convert_bool,
        [words("Error: expected boolean")], ok1(Bools)).

:- pred convert_int(term::in, int::out) is semidet.

convert_int(Term, Int) :-
    Term = term.functor(term.integer(Int), [], _).

:- pred convert_int_list(varset::in, term::in, maybe1(list(int))::out) is det.

convert_int_list(VarSet, ListTerm, Result) :-
    convert_list("integers", yes(VarSet), ListTerm, convert_int,
        [words("Error: expected integer")], Result).

    % convert_list(What, MaybeVarSet, Term, Pred, UnrecognizedPieces, Result):
    %
    % Convert Term into a list of elements where Pred converts each element
    % of the list into the correct type. Result will hold the list if the
    % conversion succeded for each element of M, otherwise it will hold
    % the error. What should be a plural noun or noun phrase describing
    % the expected list. If MaybeVarSet is yes, it should specify the varset
    % for use in describing any unrecognized list elements.
    %
:- pred convert_list(string::in, maybe(varset)::in, term::in,
    pred(term, T)::(pred(in, out) is semidet),
    list(format_component)::in, maybe1(list(T))::out) is det.

convert_list(What, MaybeVarSet, Term, Pred, UnrecognizedPieces, Result) :-
    (
        Term = term.variable(_, Context),
        (
            MaybeVarSet = yes(VarSet),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = UnrecognizedPieces ++ [suffix(","), words("not"),
                quote(TermStr), suffix("."), nl]
        ;
            MaybeVarSet = no,
            Pieces = UnrecognizedPieces ++ [suffix("."), nl]
        ),
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        Result = error1([Spec])
    ;
        Term = term.functor(Functor, Args, Context),
        (
            Functor = term.atom("[|]"),
            Args = [FirstTerm, RestTerm]
        ->
            ( Pred(FirstTerm, FirstElement) ->
                convert_list(What, MaybeVarSet, RestTerm, Pred,
                    UnrecognizedPieces, RestResult),
                (
                    RestResult = ok1(LaterElements),
                    Result = ok1([FirstElement | LaterElements])
                ;
                    RestResult = error1(_),
                    Result = RestResult
                )
            ;
                (
                    MaybeVarSet = yes(VarSet),
                    FirstTermStr = describe_error_term(VarSet, FirstTerm),
                    Pieces = UnrecognizedPieces ++ [suffix(","), words("not"),
                        quote(FirstTermStr), suffix("."), nl]
                ;
                    MaybeVarSet = no,
                    Pieces = UnrecognizedPieces ++ [suffix("."), nl]
                ),
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                Result = error1([Spec])
            )
        ;
            Functor = term.atom("[]"),
            Args = []
        ->
            Result = ok1([])
        ;
            (
                MaybeVarSet = yes(VarSet),
                TermStr = describe_error_term(VarSet, Term),
                Pieces = [words("Error: expected list of"), words(What),
                    suffix(","), words("not"), quote(TermStr), suffix("."), nl]
            ;
                MaybeVarSet = no,
                Pieces = [words("Error: expected list of"), words(What),
                    suffix("."), nl]
            ),
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            Result = error1([Spec])
        )
    ).

    % This predicate does the same job as convert_list, but with a different
    % type of supplied Pred, which returns a maybe(item_type), not item_type.
    %
:- pred convert_maybe_list(string::in, maybe(varset)::in, term::in,
    pred(term, maybe1(T))::(pred(in, out) is semidet),
    list(format_component)::in, maybe1(list(T))::out) is det.

convert_maybe_list(What, MaybeVarSet, Term, Pred, UnrecognizedPieces,
        Result) :-
    (
        Term = term.variable(_, Context),
        (
            MaybeVarSet = yes(VarSet),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = UnrecognizedPieces ++ [suffix(","), words("not"),
                quote(TermStr), suffix("."), nl]
        ;
            MaybeVarSet = no,
            Pieces = UnrecognizedPieces ++ [suffix("."), nl]
        ),
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        Result = error1([Spec])
    ;
        Term = term.functor(Functor, Args, Context),
        (
            Functor = term.atom("[|]"),
            Args = [FirstTerm, RestTerm]
        ->
            ( Pred(FirstTerm, ElementResult) ->
                (
                    ElementResult = ok1(FirstElement),
                    convert_maybe_list(What, MaybeVarSet, RestTerm, Pred,
                        UnrecognizedPieces, RestResult),
                    (
                        RestResult = ok1(LaterElements),
                        Result = ok1([FirstElement | LaterElements])
                    ;
                        RestResult = error1(_),
                        Result = RestResult
                    )
                ;
                    ElementResult = error1(Specs),
                    Result = error1(Specs)
                )
            ;
                (
                    MaybeVarSet = yes(VarSet),
                    FirstTermStr = describe_error_term(VarSet, FirstTerm),
                    Pieces = UnrecognizedPieces ++ [suffix(","), words("not"),
                        quote(FirstTermStr), suffix("."), nl]
                ;
                    MaybeVarSet = no,
                    Pieces = UnrecognizedPieces ++ [suffix("."), nl]
                ),
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                Result = error1([Spec])
            )
        ;
            Functor = term.atom("[]"),
            Args = []
        ->
            Result = ok1([])
        ;
            (
                MaybeVarSet = yes(VarSet),
                TermStr = describe_error_term(VarSet, Term),
                Pieces = [words("Error: expected list of"), words(What),
                    suffix(","), words("not"), quote(TermStr), suffix("."), nl]
            ;
                MaybeVarSet = no,
                Pieces = [words("Error: expected list of"), words(What),
                    suffix("."), nl]
            ),
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            Result = error1([Spec])
        )
    ).

:- pred convert_type_spec_pair(term::in, pair(tvar, mer_type)::out) is semidet.

convert_type_spec_pair(Term, TypeSpec) :-
    Term = term.functor(term.atom("="), [TypeVarTerm, SpecTypeTerm0], _),
    TypeVarTerm = term.variable(TypeVar0, _),
    term.coerce_var(TypeVar0, TypeVar),
    maybe_parse_type(SpecTypeTerm0, SpecType),
    TypeSpec = TypeVar - SpecType.

%-----------------------------------------------------------------------------%
%
% Parsing termination2_info pragmas.
%

:- pred parse_arg_size_constraints(term::in,
    maybe1(maybe(list(arg_size_constr)))::out) is semidet.

parse_arg_size_constraints(ArgSizeTerm, MaybeMaybeArgSizeConstraints) :-
    (
        ArgSizeTerm = term.functor(term.atom("not_set"), [], _),
        MaybeMaybeArgSizeConstraints = ok1(no)
    ;
        ArgSizeTerm = term.functor(term.atom("constraints"),
            [Constraints0], _),
        UnrecognizedPieces = [words("Error: expected constraint."), nl],
        convert_list("arg size constraints", no, Constraints0,
            parse_arg_size_constraint, UnrecognizedPieces, MaybeConstraints),
        MaybeConstraints = ok1(Constraints),
        MaybeMaybeArgSizeConstraints = ok1(yes(Constraints))
    ).

:- pred parse_arg_size_constraint(term::in, arg_size_constr::out) is semidet.

parse_arg_size_constraint(Term, Constr) :-
    (
        Term = term.functor(term.atom("le"), [Terms, ConstantTerm], _),
        UnrecognizedPieces = [words("Error: expected linear term."), nl],
        convert_list("linear terms", no, Terms, parse_lp_term,
            UnrecognizedPieces, TermsResult),
        TermsResult = ok1(LPTerms),
        parse_rational(ConstantTerm, Constant),
        Constr = le(LPTerms, Constant)
    ;
        Term = term.functor(term.atom("eq"), [Terms, ConstantTerm], _),
        UnrecognizedPieces = [words("Error: expected linear term."), nl],
        convert_list("linear terms", no, Terms, parse_lp_term,
            UnrecognizedPieces, TermsResult),
        TermsResult = ok1(LPTerms),
        parse_rational(ConstantTerm, Constant),
        Constr = eq(LPTerms, Constant)
    ).

:- pred parse_lp_term(term::in, arg_size_term::out) is semidet.

parse_lp_term(Term, LpTerm) :-
    Term = term.functor(term.atom("term"), [VarIdTerm, CoeffTerm], _),
    VarIdTerm = term.functor(term.integer(VarId), [], _),
    parse_rational(CoeffTerm, Coeff),
    LpTerm = arg_size_term(VarId, Coeff).

:- pred parse_rational(term::in, rat::out) is semidet.

parse_rational(Term, Rational) :-
    Term = term.functor(term.atom("r"), [NumerTerm, DenomTerm], _),
    NumerTerm = term.functor(term.integer(Numer), [], _),
    DenomTerm = term.functor(term.integer(Denom), [], _),
    Rational = rat.rat(Numer, Denom).

%-----------------------------------------------------------------------------%

:- pred parse_required_feature(term::in,
    maybe1(required_feature)::out) is semidet.

parse_required_feature(ReqFeatureTerm, MaybeReqFeature) :-
    ReqFeatureTerm = term.functor(term.atom(Functor), [], _),
    string_to_required_feature(Functor, ReqFeature),
    MaybeReqFeature = ok1(ReqFeature).

:- pred string_to_required_feature(string::in, required_feature::out)
    is semidet.

string_to_required_feature("concurrency",       reqf_concurrency).
string_to_required_feature("single_prec_float", reqf_single_prec_float).
string_to_required_feature("double_prec_float", reqf_double_prec_float).
string_to_required_feature("memo",              reqf_memo).
string_to_required_feature("parallel_conj",     reqf_parallel_conj).
string_to_required_feature("trailing",          reqf_trailing).
string_to_required_feature("strict_sequential", reqf_strict_sequential).
string_to_required_feature("conservative_gc",   reqf_conservative_gc).

%-----------------------------------------------------------------------------%

:- pred parse_predicate_or_function(term::in, pred_or_func::out) is semidet.

parse_predicate_or_function(PredOrFuncTerm, PredOrFunc) :-
    PredOrFuncTerm = term.functor(term.atom(Functor), [], _),
    (
        Functor = "predicate",
        PredOrFunc = pf_predicate
    ;
        Functor = "function",
        PredOrFunc = pf_function
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_pragma.
%-----------------------------------------------------------------------------%
