%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2008 The University of Melbourne.
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
    prog_context::in, maybe1(item)::out) is semidet.

    % Parse a term that represents a foreign language.
    %
:- pred parse_foreign_language(term::in, foreign_language::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.rat.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module unit.

%-----------------------------------------------------------------------------%

parse_pragma(ModuleName, VarSet, PragmaTerms, Context, Result) :-
    (
        PragmaTerms = [SinglePragmaTerm0],
        parse_type_decl_where_part_if_present(non_solver_type, ModuleName,
            SinglePragmaTerm0, SinglePragmaTerm, WherePartResult),
        SinglePragmaTerm = term.functor(term.atom(PragmaType), PragmaArgs,
            _Context),
        parse_pragma_type(ModuleName, PragmaType, PragmaArgs, SinglePragmaTerm,
            VarSet, Context, Result0)
    ->
        (
            % The code to process `where' attributes will return an error
            % result if solver attributes are given for a non-solver type.
            % Because this is a non-solver type, if the unification with
            % WhereResult succeeds then _NoSolverTypeDetails is guaranteed to
            % be `no'.
            WherePartResult = ok2(_NoSolverTypeDetails, MaybeUserEqComp),
            (
                MaybeUserEqComp = yes(_),
                Result0 = ok1(Item0)
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
                    Result = ok1(Item)
                ;
                    Msg = "unexpected `where equality/comparison is'",
                    Result = error1([Msg - SinglePragmaTerm0])
                )
            ;
                Result = Result0
            )
        ;
            WherePartResult = error2(Errors),
            Result = error1(Errors)
        )
    ;
        fail
    ).

%----------------------------------------------------------------------------%

:- pred parse_pragma_type(module_name::in, string::in, list(term)::in,
    term::in, varset::in, prog_context::in, maybe1(item)::out) is semidet.

parse_pragma_type(ModuleName, PragmaName, PragmaTerms, ErrorTerm, VarSet,
        Context, Result) :-
    (
        PragmaName = "source_file",
        parse_pragma_source_file(PragmaTerms, ErrorTerm, Context, Result)
    ;
        PragmaName = "foreign_type",
        parse_pragma_foreign_type(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, Result)
    ;
        PragmaName = "foreign_decl",
        parse_pragma_foreign_decl_pragma(ModuleName, PragmaName,
            PragmaTerms, ErrorTerm, VarSet, Context, Result)
    ;
        PragmaName = "c_header_code",
        parse_pragma_c_header_code(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, Result)
    ;
        PragmaName = "foreign_code",
        parse_pragma_foreign_code_pragma(ModuleName, PragmaName,
            PragmaTerms, ErrorTerm, VarSet, Context, Result)
    ;
        PragmaName = "foreign_proc",
        parse_pragma_foreign_proc_pragma(ModuleName, PragmaName,
            PragmaTerms, ErrorTerm, VarSet, Context, Result)
    ;
        PragmaName = "foreign_export_enum",
        parse_pragma_foreign_export_enum(PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "foreign_enum",
        parse_pragma_foreign_enum(PragmaTerms, ErrorTerm, Context, Result)
    ;
        PragmaName = "foreign_export",
        parse_pragma_foreign_export(PragmaTerms, ErrorTerm, Context, Result)
    ;
        PragmaName = "c_code",
        parse_pragma_c_code(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, Result)
    ;
        PragmaName = "c_import_module",
        parse_pragma_c_import_module(PragmaTerms, ErrorTerm, Context, Result)
    ;
        PragmaName = "foreign_import_module",
        parse_pragma_foreign_import_module(PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "import",
        parse_pragma_import(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, Result)
    ;
        PragmaName = "export",
        parse_pragma_export(PragmaTerms, ErrorTerm, Context, Result)
    ;
        (
            PragmaName = "inline",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_inline(Name, Arity))
        ;
            PragmaName = "no_inline",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_no_inline(Name, Arity))
        ;
            PragmaName = "obsolete",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_obsolete(Name, Arity))
        ;
            PragmaName = "promise_equivalent_clauses",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_promise_equivalent_clauses(Name, Arity))
        ;
            PragmaName = "promise_pure",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_promise_pure(Name, Arity))
        ;
            PragmaName = "promise_semipure",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_promise_semipure(Name, Arity))
        ;
            PragmaName = "terminates",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_terminates(Name, Arity))
        ;
            PragmaName = "does_not_terminate",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_does_not_terminate(Name, Arity))
        ;
            PragmaName = "check_termination",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_check_termination(Name, Arity))
        ;
            PragmaName = "mode_check_clauses",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                Pragma = pragma_mode_check_clauses(Name, Arity))
        ),
        parse_simple_pragma(ModuleName, PragmaName, MakePragma,
            PragmaTerms, ErrorTerm, Context, Result)
    ;
        PragmaName = "reserve_tag",
        MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = pragma_reserve_tag(Name, Arity)),
        parse_simple_type_pragma(ModuleName, PragmaName, MakePragma,
            PragmaTerms, ErrorTerm, Context, Result)
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
            PragmaTerms, ErrorTerm, Context, Result)
    ;
        PragmaName = "unused_args",
        parse_pragma_unused_args(ModuleName, PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "type_spec",
        parse_pragma_type_spec(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, Result)
    ;
        PragmaName = "fact_table",
        parse_pragma_fact_table(ModuleName, PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "termination_info",
        parse_pragma_termination_info(ModuleName, PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "termination2_info",
        parse_pragma_termination2_info(ModuleName, PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "structure_sharing",
        parse_pragma_structure_sharing(ModuleName, PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "structure_reuse",
        parse_pragma_structure_reuse(ModuleName, PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "exceptions",
        parse_pragma_exceptions(ModuleName, PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "trailing_info",
        parse_pragma_trailing_info(ModuleName, PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "mm_tabling_info",
        parse_pragma_mm_tabling_info(ModuleName, PragmaTerms, ErrorTerm,
            Context, Result)
    ;
        PragmaName = "require_feature_set",
        parse_pragma_require_feature_set(PragmaTerms, ErrorTerm,
            Context, Result)
    ).

%----------------------------------------------------------------------------%

% XXX The predicates in the rest of this module ought to be clustered together
% into groups of related predicates, grouping both parse_pragma_xxx predicates
% together with their helper predicates, and grouping parse_pragma_xxx
% predicates for related xxxs together.

:- pred parse_pragma_source_file(list(term)::in, term::in, prog_context::in,
    maybe1(item)::out) is det.

parse_pragma_source_file(PragmaTerms, ErrorTerm, Context, Result) :-
    ( PragmaTerms = [SourceFileTerm] ->
        ( SourceFileTerm = term.functor(term.string(SourceFile), [], _) ->
            Pragma = pragma_source_file(SourceFile),
            ItemPragma = item_pragma_info(user, Pragma, Context),
            Item = item_pragma(ItemPragma),
            Result = ok1(Item)
        ;
            Msg = "string expected in `:- pragma source_file' declaration",
            Result = error1([Msg - SourceFileTerm])
        )
    ;
        Msg = "wrong number of arguments in " ++
            "`:- pragma source_file' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_foreign_type(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_foreign_type(ModuleName, PragmaTerms, ErrorTerm, VarSet, Context,
        Result) :-
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
            parse_foreign_language_type(ForeignTypeTerm, Language,
                MaybeForeignType),
            (
                MaybeForeignType = ok1(ForeignType),
                parse_type_defn_head(ModuleName, MercuryTypeTerm,
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
                            cond_true, Context),
                        Item = item_type_defn(ItemTypeDefn),
                        Result = ok1(Item)
                    ;
                        MaybeAssertionTerm = yes(ErrorAssertionTerm)
                    ->
                        Msg = "invalid assertion in " ++
                            "`:- pragma foreign_type' declaration",
                        Result = error1([Msg - ErrorAssertionTerm])
                    ;
                        unexpected(this_file,
                            "parse_pragma_type: unexpected failure of " ++
                            "parse_maybe_foreign_type_assertion")
                    )
                ;
                    MaybeTypeDefnHead = error2(Errors),
                    Result = error1(Errors)
                )
            ;
                MaybeForeignType = error1(Errors),
                Result = error1(Errors)
            )
        ;
            Msg = "invalid foreign language in " ++
                "`:- pragma foreign_type' declaration",
            Result = error1([Msg - LangTerm])
        )
    ;
        Msg = "wrong number of arguments in " ++
            "`:- pragma foreign_type' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_c_header_code(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_c_header_code(ModuleName, PragmaTerms, ErrorTerm, VarSet, Context,
        Result) :-
    ( PragmaTerms = [term.functor(_, _, Context) | _] ->
        LangC = term.functor(term.string("C"), [], Context),
        parse_pragma_foreign_decl_pragma(ModuleName, "c_header_code",
            [LangC | PragmaTerms], ErrorTerm, VarSet, Context, Result)
    ;
        Msg = "wrong number of arguments or unexpected variable " ++
            "in `:- pragma c_header_code' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

%----------------------------------------------------------------------------%
%
% Code for parsing foreign_export_enum pragmas
%

:- pred parse_pragma_foreign_export_enum(list(term)::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_pragma_foreign_export_enum(PragmaTerms, ErrorTerm, Context, Result) :-
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
        ( parse_foreign_language(LangTerm, ForeignLanguage) ->
            parse_export_enum_type(MercuryTypeTerm, MaybeType),
            (
                MaybeType = ok2(Name, Arity),
                maybe_parse_export_enum_attributes(MaybeAttributesTerm,
                    MaybeAttributes),
                (
                    MaybeAttributes = ok1(Attributes),
                    maybe_parse_export_enum_overrides(MaybeOverridesTerm,
                        MaybeOverrides),
                    (
                        MaybeOverrides = ok1(Overrides),
                        PragmaExportEnum = pragma_foreign_export_enum(
                            ForeignLanguage, Name, Arity, Attributes,
                            Overrides
                        ),
                        ItemPragma = item_pragma_info(user, PragmaExportEnum,
                            Context),
                        Item = item_pragma(ItemPragma),
                        Result = ok1(Item)
                    ;
                        MaybeOverrides = error1(Errors),
                        Result = error1(Errors)
                    )
                ;
                    MaybeAttributes = error1(Errors),
                    Result = error1(Errors)
                )
            ;
                MaybeType = error2(Errors),
                Result = error1(Errors)
            )
        ;
            Msg = "invalid foreign langauge in " ++
                "`:- pragma foreign_export_enum' declaration",
            Result = error1([Msg - ErrorTerm])
        )
    ;
        Msg = "wrong number of arguments in " ++
            "`:- pragma foreign_export_enum' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_export_enum_type(term::in, maybe2(sym_name, arity)::out) is det.

parse_export_enum_type(TypeTerm, Result) :-
    ( parse_name_and_arity(TypeTerm, Name, Arity) ->
        Result = ok2(Name, Arity)
    ;
        Msg = "expected name/arity for type in " ++
            "`pragma foreign_export_enum' declaration",
        Result = error2([Msg - TypeTerm])
    ).

:- pred maybe_parse_export_enum_overrides(maybe(term)::in,
    maybe1(assoc_list(sym_name, string))::out) is det.

maybe_parse_export_enum_overrides(no, ok1([])).
maybe_parse_export_enum_overrides(yes(OverridesTerm), MaybeOverrides) :-
    ListMsg = "not a valid mapping element",
    PairMsg = "exported enumeration override constructor",
    convert_maybe_list(OverridesTerm, parse_sym_name_string_pair(PairMsg),
        ListMsg, MaybeOverrides).

:- pred parse_sym_name_string_pair(string::in, term::in,
    maybe1(pair(sym_name, string))::out) is semidet.

parse_sym_name_string_pair(Msg, PairTerm, MaybePair) :-
    PairTerm = functor(Functor, Args, _),
    Functor = term.atom("-"),
    Args = [SymNameTerm, StringTerm],
    StringTerm = functor(term.string(String), _, _),
    parse_qualified_term(SymNameTerm, SymNameTerm, Msg,
        MaybeSymNameResult),
    (
        MaybeSymNameResult = ok2(SymName, []),
        MaybePair = ok1(SymName - String)
    ;
        MaybeSymNameResult = error2(Errs),
        MaybePair = error1(Errs)
    ).

:- pred maybe_parse_export_enum_attributes(maybe(term)::in,
    maybe1(export_enum_attributes)::out) is det.

maybe_parse_export_enum_attributes(no, ok1(default_export_enum_attributes)).
maybe_parse_export_enum_attributes(yes(AttributesTerm), MaybeAttributes) :-
    parse_export_enum_attributes(AttributesTerm, MaybeAttributes).

:- type collected_export_enum_attribute
    --->    ee_attr_prefix(maybe(string)).

:- pred parse_export_enum_attributes(term::in,
    maybe1(export_enum_attributes)::out) is det.

parse_export_enum_attributes(AttributesTerm, AttributesResult) :-
    Attributes0 = default_export_enum_attributes,
    ConflictingAttributes = [],
    (
        list_term_to_term_list(AttributesTerm, AttributesTerms),
        map_parser(parse_export_enum_attr, AttributesTerms, MaybeAttrList),
        MaybeAttrList = ok1(CollectedAttributes)
    ->
        (
            list.member(ConflictA - ConflictB, ConflictingAttributes),
            list.member(ConflictA, CollectedAttributes),
            list.member(ConflictB, CollectedAttributes)
        ->
            Msg = "conflicting attributes in attribute list",
            AttributesResult = error1([Msg - AttributesTerm])
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
                Msg = "prefix attribute occurs multiple times in " ++
                    "foreign_export_enum pragma",
                AttributesResult = error1([Msg - AttributesTerm])
            )
        )
    ;
        Msg = "malformed attributes list in foreign_export_enum pragma",
        AttributesResult = error1([Msg - AttributesTerm])
    ).

:- pred process_export_enum_attribute(collected_export_enum_attribute::in,
    export_enum_attributes::in, export_enum_attributes::out) is det.

process_export_enum_attribute(ee_attr_prefix(MaybePrefix), _, Attributes) :-
    Attributes = export_enum_attributes(MaybePrefix).

:- pred parse_export_enum_attr(term::in,
    maybe1(collected_export_enum_attribute)::out) is det.

parse_export_enum_attr(Term, Result) :-
    (
        Term = functor(atom("prefix"), Args, _),
        Args = [ ForeignNameTerm ],
        ForeignNameTerm = functor(string(Prefix), [], _)
    ->
        Result = ok1(ee_attr_prefix(yes(Prefix)))
    ;
        Msg = "unrecognised attribute in foreign_export_enum pragma",
        Result = error1([Msg - Term])
    ).

%----------------------------------------------------------------------------%
%
% Code for parsing foreign_enum pragmas
%

:- pred parse_pragma_foreign_enum(list(term)::in, term::in, prog_context::in,
    maybe1(item)::out) is det.

parse_pragma_foreign_enum(PragmaTerms, ErrorTerm, Context, Result) :-
    ( PragmaTerms = [LangTerm, MercuryTypeTerm, ValuesTerm] ->
        ( parse_foreign_language(LangTerm, ForeignLanguage) ->
            parse_export_enum_type(MercuryTypeTerm, MaybeType),
            (
                MaybeType = ok2(TypeName, TypeArity),
                ListErrMsg = "not a valid mapping element",
                PairErrMsg = "foreign_enum constructor name",
                convert_maybe_list(ValuesTerm,
                    parse_sym_name_string_pair(PairErrMsg),
                    ListErrMsg, MaybeValues),
                (
                    MaybeValues = ok1(Values),
                    PragmaForeignImportEnum = pragma_foreign_enum(
                        ForeignLanguage, TypeName, TypeArity, Values),
                    ItemPragma = item_pragma_info(user,
                        PragmaForeignImportEnum, Context),
                    Item = item_pragma(ItemPragma),
                    Result = ok1(Item)
                ;
                    MaybeValues = error1(Errors),
                    Result = error1(Errors)
                )
            ;
                MaybeType = error2(Errors),
                Result = error1(Errors)
            )
        ;
            Msg = "invalid foreign langauge in " ++
                "`:- pragma foreign_enum' declaration",
            Result = error1([Msg - ErrorTerm])
        )
    ;
        Msg = "wrong number of arguments in " ++
            "`:- pragma foreign_enum' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

%----------------------------------------------------------------------------%
%
% Code for parsing foreign_export pragmas
%

:- pred parse_pragma_foreign_export(list(term)::in, term::in, prog_context::in,
    maybe1(item)::out) is det.

parse_pragma_foreign_export(PragmaTerms, ErrorTerm, Context, Result) :-
    ( PragmaTerms = [LangTerm, PredAndModesTerm, FunctionTerm] ->
        ( FunctionTerm = term.functor(term.string(Function), [], _) ->
            parse_pred_or_func_and_arg_modes(no, PredAndModesTerm,
                ErrorTerm, "`:- pragma foreign_export' declaration",
                PredAndModesResult),
            (
                PredAndModesResult = ok2(PredName - PredOrFunc, Modes),
                ( parse_foreign_language(LangTerm, ForeignLanguage) ->
                    Pragma = pragma_foreign_export(ForeignLanguage, PredName,
                        PredOrFunc, Modes, Function),
                    ItemPragma = item_pragma_info(user, Pragma, Context),
                    Item = item_pragma(ItemPragma),
                    Result = ok1(Item)
                ;
                    Msg = "invalid foreign language in " ++
                        "`:- pragma foreign_export declaration",
                    Result = error1([Msg - LangTerm])
                )
            ;
                PredAndModesResult = error2(Errors),
                Result = error1(Errors)
            )
        ;
            Msg = "expected pragma " ++
                "foreign_export(Lang, PredName(ModeList), Function)",
            Result = error1([Msg - PredAndModesTerm])
        )
    ;
        Msg = "wrong number of arguments in " ++
            "`:- pragma foreign_export' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

%----------------------------------------------------------------------------%

:- pred parse_pragma_c_code(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_c_code(ModuleName, PragmaTerms, ErrorTerm, VarSet, Context,
        Result) :-
    % pragma c_code is almost as if we have written foreign_code
    % or foreign_proc with the language set to "C".
    % There are a few differences (error messages, some deprecated
    % syntax is still supported for c_code) so we pass the original
    % pragma name to parse_pragma_foreign_code_pragma.
    (
        % arity = 1 (same as foreign_code)
        PragmaTerms = [term.functor(_, _, FirstContext)]
    ->
        LangC = term.functor(term.string("C"), [], FirstContext),
        parse_pragma_foreign_code_pragma(ModuleName, "c_code",
            [LangC | PragmaTerms], ErrorTerm, VarSet, Context, Result)
    ;
        % arity > 1 (same as foreign_proc)
        PragmaTerms = [term.functor(_, _, FirstContext) | _]
    ->
        LangC = term.functor(term.string("C"), [], FirstContext),
        parse_pragma_foreign_proc_pragma(ModuleName, "c_code",
            [LangC | PragmaTerms], ErrorTerm, VarSet, Context, Result)
    ;
        Msg = "wrong number of arguments or unexpected variable" ++
            "in `:- pragma c_code' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_c_import_module(list(term)::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_pragma_c_import_module(PragmaTerms, ErrorTerm, Context, Result) :-
    (
        PragmaTerms = [ImportTerm],
        sym_name_and_args(ImportTerm, Import, [])
    ->
        Pragma = pragma_foreign_import_module(lang_c, Import),
        ItemPragma = item_pragma_info(user, Pragma, Context),
        Item = item_pragma(ItemPragma),
        Result = ok1(Item)
    ;
        Msg = "wrong number of arguments or invalid module name " ++
            "in `:- pragma c_import_module' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_foreign_import_module(list(term)::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_pragma_foreign_import_module(PragmaTerms, ErrorTerm, Context, Result) :-
    (
        PragmaTerms = [LangTerm, ImportTerm],
        sym_name_and_args(ImportTerm, Import, [])
    ->
        ( parse_foreign_language(LangTerm, Language) ->
            Pragma = pragma_foreign_import_module(Language, Import),
            ItemPragma = item_pragma_info(user, Pragma, Context),
            Item = item_pragma(ItemPragma),
            Result = ok1(Item)
        ;
            Msg = "invalid foreign language in " ++
                "`:- pragma foreign_import_module' declaration",
            Result = error1([Msg - LangTerm])
        )
    ;
        Msg = "wrong number of arguments or invalid module name " ++
            "in `:- pragma foreign_import_module' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_import(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_import(ModuleName, PragmaTerms, ErrorTerm, VarSet, Context,
        Result) :-
    % XXX We assume all imports are C.
    ForeignLanguage = lang_c,
    (
        (
            PragmaTerms = [PredAndModesTerm, FlagsTerm, FunctionTerm],
            parse_pragma_foreign_proc_attributes_term(ForeignLanguage,
                "import", VarSet, FlagsTerm, MaybeFlags),
            (
                MaybeFlags = error1(FlagsErrors0),
                FlagsErrors = assoc_list.map_keys_only(
                    string.append("invalid second argument in " ++
                    "`:- pragma import/3' declaration : "), FlagsErrors0),
                FlagsResult = error1(FlagsErrors)
            ;
                MaybeFlags = ok1(Flags),
                FlagsResult = ok1(Flags)
            )
        ;
            PragmaTerms = [PredAndModesTerm, FunctionTerm],
            Flags0 = default_attributes(ForeignLanguage),
            % Pragma import uses legacy purity behaviour.
            set_legacy_purity_behaviour(yes, Flags0, Flags),
            FlagsResult = ok1(Flags)
        )
    ->
        ( FunctionTerm = term.functor(term.string(Function), [], _) ->
            parse_pred_or_func_and_arg_modes(yes(ModuleName),
                PredAndModesTerm, ErrorTerm, "`:- pragma import' declaration",
                PredAndArgModesResult),
            (
                PredAndArgModesResult = ok2(PredName - PredOrFunc, ArgModes),
                (
                    FlagsResult = ok1(Attributes),
                    Pragma = pragma_import(PredName, PredOrFunc, ArgModes,
                        Attributes, Function),
                    ItemPragma = item_pragma_info(user, Pragma, Context),
                    Item = item_pragma(ItemPragma),
                    Result = ok1(Item)
                ;
                    FlagsResult = error1(FlagsErrors2),
                    Result = error1(FlagsErrors2)
                )
            ;
                PredAndArgModesResult = error2(PredAndArgModesErrors),
                Result = error1(PredAndArgModesErrors)
            )
        ;
            Msg = "expected pragma import(PredName(ModeList), Function)",
            Result = error1([Msg - PredAndModesTerm])
        )
    ;
        Msg = "wrong number of arguments in `:- pragma import' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_export(list(term)::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_pragma_export(PragmaTerms, ErrorTerm, Context, Result) :-
    ( PragmaTerms = [PredAndModesTerm, FunctionTerm] ->
        ( FunctionTerm = term.functor(term.string(Function), [], _) ->
            parse_pred_or_func_and_arg_modes(no, PredAndModesTerm, ErrorTerm,
                "`:- pragma export' declaration", PredAndModesResult),
            (
                PredAndModesResult = ok2(PredName - PredOrFunc, Modes),
                Pragma = pragma_foreign_export(lang_c, PredName, PredOrFunc,
                    Modes, Function),
                ItemPragma = item_pragma_info(user, Pragma, Context),
                Item = item_pragma(ItemPragma),
                Result = ok1(Item)
            ;
                PredAndModesResult = error2(PredAndModesErrors),
                Result = error1(PredAndModesErrors)
            )
        ;
            Result = error1(
                 ["expected pragma export(PredName(ModeList), Function)" -
                 PredAndModesTerm])
        )
    ;
        Msg = "wrong number of arguments in `:- pragma export' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_unused_args(module_name::in, list(term)::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_pragma_unused_args(ModuleName, PragmaTerms, ErrorTerm, Context,
        Result) :-
    % pragma unused_args should never appear in user programs,
    % only in .opt files.
    (
        PragmaTerms = [
            PredOrFuncTerm,
            PredNameTerm,
            term.functor(term.integer(Arity), [], _),
            term.functor(term.integer(ModeNum), [], _),
            UnusedArgsTerm
        ],
        (
            PredOrFuncTerm = term.functor(term.atom("predicate"), [], _),
            PredOrFunc = pf_predicate
        ;
            PredOrFuncTerm = term.functor(term.atom("function"), [], _),
            PredOrFunc = pf_function
        ),
        parse_implicitly_qualified_term(ModuleName, PredNameTerm, ErrorTerm,
            "`:- pragma unused_args' declaration", PredNameResult),
        PredNameResult = ok2(PredName, []),
        convert_int_list(UnusedArgsTerm, UnusedArgsResult),
        UnusedArgsResult = ok1(UnusedArgs)
    ->
        Pragma = pragma_unused_args(PredOrFunc, PredName, Arity, ModeNum,
            UnusedArgs),
        ItemPragma = item_pragma_info(user, Pragma, Context),
        Item = item_pragma(ItemPragma),
        Result = ok1(Item)
    ;
        Result = error1(["error in `:- pragma unused_args'" - ErrorTerm])
    ).

:- pred parse_pragma_type_spec(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_type_spec(ModuleName, PragmaTerms, ErrorTerm, VarSet0, Context,
        Result) :-
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

            parse_implicitly_qualified_term(ModuleName, SpecNameTerm,
                ErrorTerm, "", NameResult),
            NameResult = ok2(SpecName, []),
            MaybeName = yes(SpecName)
        )
    ->
        parse_arity_or_modes(ModuleName, PredAndModesTerm, ErrorTerm,
            "`:- pragma type_spec' declaration", ArityOrModesResult),
        (
            ArityOrModesResult = ok1(arity_or_modes(PredName, Arity,
                MaybePredOrFunc, MaybeModes)),
            conjunction_to_list(TypeSubnTerm, TypeSubnList),

            % The varset is actually a tvarset.
            varset.coerce(VarSet0, TVarSet),
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
                Pragma = pragma_type_spec(PredName, SpecializedName, Arity,
                    MaybePredOrFunc, MaybeModes, TypeSubn, TVarSet, set.init),
                ItemPragma = item_pragma_info(user, Pragma, Context),
                Item = item_pragma(ItemPragma),
                Result = ok1(Item)
            ;
                Msg = "expected type substitution in " ++
                    "`:- pragma type_spec' declaration",
                Result = error1([Msg - TypeSubnTerm])
            )
        ;
            ArityOrModesResult = error1(Errors),
            Result = error1(Errors)
        )
    ;
        Msg = "wrong number of arguments in `:- pragma type_spec' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_fact_table(module_name::in, list(term)::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_pragma_fact_table(ModuleName, PragmaTerms, ErrorTerm,
        Context, Result) :-
    ( PragmaTerms = [PredAndArityTerm, FileNameTerm] ->
        parse_pred_name_and_arity(ModuleName, "fact_table",
            PredAndArityTerm, ErrorTerm, NameArityResult),
        (
            NameArityResult = ok2(PredName, Arity),
            ( FileNameTerm = term.functor(term.string(FileName), [], _) ->
                Pragma = pragma_fact_table(PredName, Arity, FileName),
                ItemPragma = item_pragma_info(user, Pragma, Context),
                Item = item_pragma(ItemPragma),
                Result = ok1(Item)
            ;
                Result = error1(["expected string for fact table filename" -
                    FileNameTerm])
            )
        ;
            NameArityResult = error2(Errors),
            Result = error1(Errors)
        )
    ;
        Msg = "wrong number of arguments " ++
            "in `:- pragma fact_table' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_termination_info(module_name::in, list(term)::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_pragma_termination_info(ModuleName, PragmaTerms, ErrorTerm, Context,
        Result) :-
    (
        PragmaTerms = [
            PredAndModesTerm0,
            ArgSizeTerm,
            TerminationTerm
        ],
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            ErrorTerm, "`:- pragma termination_info' declaration",
            NameAndModesResult),
        NameAndModesResult = ok2(PredName - PredOrFunc, ModeList),
        (
            ArgSizeTerm = term.functor(term.atom("not_set"), [], _),
            MaybeArgSizeInfo = no
        ;
            ArgSizeTerm = term.functor(term.atom("infinite"), [], _),
            MaybeArgSizeInfo = yes(infinite(unit))
        ;
            ArgSizeTerm = term.functor(term.atom("finite"),
                [IntTerm, UsedArgsTerm], _),
            IntTerm = term.functor(term.integer(Int), [], _),
            convert_bool_list(UsedArgsTerm, UsedArgs),
            MaybeArgSizeInfo = yes(finite(Int, UsedArgs))
        ),
        (
            TerminationTerm = term.functor(term.atom("not_set"), [], _),
            MaybeTerminationInfo = no
        ;
            TerminationTerm = term.functor(term.atom("can_loop"), [], _),
            MaybeTerminationInfo = yes(can_loop(unit))
        ;
            TerminationTerm = term.functor(term.atom("cannot_loop"), [], _),
            MaybeTerminationInfo = yes(cannot_loop(unit))
        ),
        Pragma = pragma_termination_info(PredOrFunc, PredName, ModeList,
            MaybeArgSizeInfo, MaybeTerminationInfo),
        ItemPragma = item_pragma_info(user, Pragma, Context),
        Item = item_pragma(ItemPragma),
        Result0 = ok1(Item)
    ->
        Result = Result0
    ;
        Msg = "syntax error in `:- pragma termination_info' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_termination2_info(module_name::in,
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_termination2_info(ModuleName, PragmaTerms, ErrorTerm, Context,
        Result) :-
    (
        PragmaTerms = [
            PredAndModesTerm0,
            SuccessArgSizeTerm,
            FailureArgSizeTerm,
            TerminationTerm
        ],
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            ErrorTerm, "`:- pragma termination2_info' declaration",
            NameAndModesResult),
        NameAndModesResult = ok2(PredName - PredOrFunc, ModeList),
        parse_arg_size_constraints(SuccessArgSizeTerm, SuccessArgSizeResult),
        SuccessArgSizeResult = ok1(SuccessArgSizeInfo),
        parse_arg_size_constraints(FailureArgSizeTerm, FailureArgSizeResult),
        FailureArgSizeResult = ok1(FailureArgSizeInfo),
        (
            TerminationTerm = term.functor(term.atom("not_set"), [], _),
            MaybeTerminationInfo = no
        ;
            TerminationTerm = term.functor(term.atom("can_loop"), [], _),
            MaybeTerminationInfo = yes(can_loop(unit))
        ;
            TerminationTerm = term.functor(term.atom("cannot_loop"), [], _),
            MaybeTerminationInfo = yes(cannot_loop(unit))
        ),
        Pragma = pragma_termination2_info(PredOrFunc, PredName, ModeList,
            SuccessArgSizeInfo, FailureArgSizeInfo, MaybeTerminationInfo),
        ItemPragma = item_pragma_info(user, Pragma, Context),
        Item = item_pragma(ItemPragma),
        Result0 = ok1(Item)
    ->
        Result = Result0
    ;
        Msg = "syntax error in `:- pragma termination2_info' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_structure_sharing(module_name::in,
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_structure_sharing(ModuleName, PragmaTerms, ErrorTerm, Context,
        Result) :-
    (
        PragmaTerms = [
            PredAndModesTerm0,
            HeadVarsTerm,
            HeadVarTypesTerm,
            SharingInformationTerm
        ],
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            ErrorTerm, "`:- pragma structure_sharing' declaration",
            NameAndModesResult),
        NameAndModesResult = ok2(PredName - PredOrFunc, ModeList),

        % Parse the head variables:
        HeadVarsTerm = term.functor(term.atom("vars"), ListHVTerm, _),
        term.vars_list(ListHVTerm, HeadVarsGeneric),
        list.map(term.coerce_var, HeadVarsGeneric, HeadVars),

        % Parse the types:
        HeadVarTypesTerm = term.functor(term.atom("types"), ListTypeTerms, _),
        parse_types(ListTypeTerms, ok1(Types)),

        % Parse the actual structure sharing information.

        (
            SharingInformationTerm = term.functor(term.atom("not_available"),
                _, _),
            MaybeSharingAs = no
        ;
            SharingInformationTerm = term.functor(term.atom("yes"),
                SharingTerm, _),
            SharingTerm = [SharingAsTerm],
            MaybeSharingAs = yes(parse_structure_sharing_domain(SharingAsTerm))
        )
    ->
        Pragma = pragma_structure_sharing(PredOrFunc, PredName, ModeList,
            HeadVars, Types, MaybeSharingAs),
        ItemPragma = item_pragma_info(user, Pragma, Context),
        Item = item_pragma(ItemPragma),
        Result = ok1(Item)
    ;
        Msg = "syntax error in `:- pragma structure_sharing' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_structure_reuse(module_name::in,
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_structure_reuse(ModuleName, PragmaTerms, ErrorTerm, Context,
        Result) :-
    (
        PragmaTerms = [
            PredAndModesTerm0,
            HeadVarsTerm,
            HeadVarTypesTerm,
            MaybeStructureReuseTerm
        ],
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            ErrorTerm, "`:- pragma structure_reuse' declaration",
            NameAndModesResult),
        NameAndModesResult = ok2(PredName - PredOrFunc, ModeList),

        % Parse the head variables:
        HeadVarsTerm = term.functor(term.atom("vars"), ListHVTerm, _),
        term.vars_list(ListHVTerm, HeadVarsGeneric),
        list.map(term.coerce_var, HeadVarsGeneric, HeadVars),

        % Parse the types:
        HeadVarTypesTerm = term.functor(term.atom("types"), ListTypeTerms, _),
        parse_types(ListTypeTerms, ok1(Types)),

        % Parse the actual structure reuse information.
        (
            MaybeStructureReuseTerm = term.functor(term.atom("not_available"),
                _, _),
            MaybeStructureReuse = no
        ;
            MaybeStructureReuseTerm = term.functor(term.atom("yes"),
                MaybeStructureReuseTermArgs, _),
            MaybeStructureReuseTermArgs = [ StructureReuseTerm ],
            StructureReuse = parse_structure_reuse_domain(StructureReuseTerm),
            MaybeStructureReuse = yes(StructureReuse)
        )
    ->
        Pragma = pragma_structure_reuse(PredOrFunc, PredName, ModeList,
            HeadVars, Types, MaybeStructureReuse),
        ItemPragma = item_pragma_info(user, Pragma, Context),
        Item = item_pragma(ItemPragma),
        Result = ok1(Item)
    ;
        Msg = "syntax error in `:- pragma structure_reuse' declaration",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred parse_pragma_exceptions(module_name::in,
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_exceptions(ModuleName, PragmaTerms, ErrorTerm, Context, Result) :-
    (
        PragmaTerms = [
            PredOrFuncTerm,
            PredNameTerm,
            term.functor(term.integer(Arity), [], _),
            term.functor(term.integer(ModeNum), [], _),
            ThrowStatusTerm
        ],
        (
            PredOrFuncTerm = term.functor(term.atom("predicate"), [], _),
            PredOrFunc = pf_predicate
        ;
            PredOrFuncTerm = term.functor(term.atom("function"), [], _),
            PredOrFunc = pf_function
        ),
        parse_implicitly_qualified_term(ModuleName, PredNameTerm, ErrorTerm,
            "`:- pragma exceptions' declaration", PredNameResult),
        PredNameResult = ok2(PredName, []),
        (
            ThrowStatusTerm = term.functor(term.atom("will_not_throw"), [], _),
            ThrowStatus = will_not_throw
        ;
            ThrowStatusTerm = term.functor(term.atom("may_throw"),
                [ExceptionTypeTerm], _),
            (
                ExceptionTypeTerm = term.functor(
                    term.atom("user_exception"), [], _),
                ExceptionType = user_exception
            ;
                ExceptionTypeTerm = term.functor(
                    term.atom("type_exception"), [], _),
                ExceptionType = type_exception
            ),
            ThrowStatus = may_throw(ExceptionType)
        ;
            ThrowStatusTerm = term.functor( term.atom("conditional"), [], _),
            ThrowStatus = throw_conditional
        )
    ->
        Pragma = pragma_exceptions(PredOrFunc, PredName, Arity, ModeNum,
            ThrowStatus),
        ItemPragma = item_pragma_info(user, Pragma, Context),
        Item = item_pragma(ItemPragma),
        Result = ok1(Item)
    ;
        Result = error1(["error in `:- pragma exceptions'" - ErrorTerm])
    ).

:- pred parse_pragma_trailing_info(module_name::in,
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_trailing_info(ModuleName, PragmaTerms, ErrorTerm, Context,
        Result) :-
    (
        PragmaTerms = [
            PredOrFuncTerm,
            PredNameTerm,
            term.functor(term.integer(Arity), [], _),
            term.functor(term.integer(ModeNum), [], _),
            TrailingStatusTerm
        ],
        (
            PredOrFuncTerm = term.functor(term.atom("predicate"), [], _),
            PredOrFunc = pf_predicate
        ;
            PredOrFuncTerm = term.functor(term.atom("function"), [], _),
            PredOrFunc = pf_function
        ),
        parse_implicitly_qualified_term(ModuleName, PredNameTerm, ErrorTerm,
            "`:- pragma trailing_info' declaration", PredNameResult),
        PredNameResult = ok2(PredName, []),
        (
            TrailingStatusTerm = term.functor(
                term.atom("will_not_modify_trail"), [], _),
            TrailingStatus = trail_will_not_modify
        ;
            TrailingStatusTerm = term.functor(
                term.atom("may_modify_trail"), [], _),
            TrailingStatus = trail_may_modify
        ;
            TrailingStatusTerm = term.functor(
                term.atom("conditional"), [], _),
            TrailingStatus = trail_conditional
        )
    ->
        Pragma = pragma_trailing_info(PredOrFunc, PredName, Arity, ModeNum,
            TrailingStatus),
        ItemPragma = item_pragma_info(user, Pragma, Context),
        Item = item_pragma(ItemPragma),
        Result = ok1(Item)
    ;
        Result = error1(["error in `:- pragma trailing_info'" - ErrorTerm])
    ).

:- pred parse_pragma_mm_tabling_info(module_name::in,
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_mm_tabling_info(ModuleName, PragmaTerms, ErrorTerm, Context,
        Result) :-
    (
        PragmaTerms = [
            PredOrFuncTerm,
            PredNameTerm,
            term.functor(term.integer(Arity), [], _),
            term.functor(term.integer(ModeNum), [], _),
            MM_TablingStatusTerm
        ],
        (
            PredOrFuncTerm = term.functor(term.atom("predicate"), [], _),
            PredOrFunc = pf_predicate
        ;
            PredOrFuncTerm = term.functor(term.atom("function"), [], _),
            PredOrFunc = pf_function
        ),
        parse_implicitly_qualified_term(ModuleName, PredNameTerm,
            ErrorTerm, "`:- pragma mm_tabling_info' declaration",
            PredNameResult),
        PredNameResult = ok2(PredName, []),
        (
            MM_TablingStatusTerm = term.functor(
                term.atom("mm_tabled_will_not_call"), [], _),
            MM_TablingStatus = mm_tabled_will_not_call
        ;
            MM_TablingStatusTerm = term.functor(
                term.atom("mm_tabled_may_call"), [], _),
            MM_TablingStatus = mm_tabled_may_call
        ;
            MM_TablingStatusTerm = term.functor(
                term.atom("mm_tabled_conditional"), [], _),
            MM_TablingStatus = mm_tabled_conditional
        )
    ->
        Pragma = pragma_mm_tabling_info(PredOrFunc, PredName, Arity, ModeNum,
            MM_TablingStatus),
        ItemPragma = item_pragma_info(user, Pragma, Context),
        Item = item_pragma(ItemPragma),
        Result = ok1(Item)
    ;
        Result = error1(["error in `:- pragma mm_tabling_info'" - ErrorTerm])
    ).

:- pred parse_pragma_require_feature_set(list(term)::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_pragma_require_feature_set(PragmaTerms, ErrorTerm, Context, Result) :-
    ( PragmaTerms = [FeatureListTerm] ->
        convert_maybe_list(FeatureListTerm, parse_required_feature,
            "not a feature", MaybeFeatureList),
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
                Msg = "conflicting features in feature set",
                Result = error1([Msg - FeatureListTerm])
            ;
                (
                    FeatureList = [],
                    ItemNothing = item_nothing_info(no, Context),
                    Item = item_nothing(ItemNothing)
                ;
                    FeatureList = [_ | _],
                    FeatureSet = set.from_list(FeatureList),
                    Pragma = pragma_require_feature_set(FeatureSet),
                    ItemPragma = item_pragma_info(user, Pragma, Context),
                    Item = item_pragma(ItemPragma)
                ),
                Result = ok1(Item)
            )
        ;
            MaybeFeatureList = error1(Errors),
            Result = error1(Errors)
        )
    ;
        Msg = "syntax error in `:- pragma require_feature_set' declaration",
        Result = error1([Msg - ErrorTerm])
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

parse_foreign_language(term.functor(term.string(String), _, _), Lang) :-
    globals.convert_foreign_language(String, Lang).
parse_foreign_language(term.functor(term.atom(String), _, _), Lang) :-
    globals.convert_foreign_language(String, Lang).

:- pred parse_foreign_language_type(term::in, foreign_language::in,
    maybe1(foreign_language_type)::out) is det.

parse_foreign_language_type(InputTerm, Language, Result) :-
    (
        Language = lang_il,
        ( InputTerm = term.functor(term.string(ILTypeName), [], _) ->
            parse_il_type_name(ILTypeName, InputTerm, Result)
        ;
            Result = error1(["invalid backend specification term" - InputTerm])
        )
    ;
        Language = lang_c,
        ( InputTerm = term.functor(term.string(CTypeName), [], _) ->
            Result = ok1(c(c_type(CTypeName)))
        ;
            Result = error1(["invalid backend specification term" - InputTerm])
        )
    ;
        Language = lang_java,
        ( InputTerm = term.functor(term.string(JavaTypeName), [], _) ->
            Result = ok1(java(java_type(JavaTypeName)))
        ;
            Result = error1(["invalid backend specification term" - InputTerm])
        )
    ;
        Language = lang_erlang,
        ( InputTerm = term.functor(term.string(_ErlangTypeName), [], _) ->
            % XXX should we check if the type is blank?
            Result = ok1(erlang(erlang_type))
        ;
            Result = error1(["invalid backend specification term" - InputTerm])
        )
    ;
        Language = lang_csharp,
        Msg = "unsupported language specified, unable to parse backend type",
        Result = error1([Msg - InputTerm])
    ).

:- pred parse_il_type_name(string::in, term::in,
    maybe1(foreign_language_type)::out) is det.

parse_il_type_name(String0, ErrorTerm, ForeignType) :-
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
        ForeignType = error1(["invalid foreign language type description" -
            ErrorTerm])
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

    % This predicate parses both c_header_code and foreign_decl pragmas.
    %
:- pred parse_pragma_foreign_decl_pragma(module_name::in, string::in,
    list(term)::in, term::in, varset::in, prog_context::in,
    maybe1(item)::out) is det.

parse_pragma_foreign_decl_pragma(_ModuleName, PragmaName, PragmaTerms,
        ErrorTerm, _VarSet, Context, Result) :-
    string.format("invalid `:- pragma %s' declaration ", [s(PragmaName)],
        InvalidDeclStr),
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
            ( HeaderTerm = term.functor(term.string(HeaderCode), [], _) ->
                Pragma = pragma_foreign_decl(ForeignLanguage, IsLocal,
                    HeaderCode),
                ItemPragma = item_pragma_info(user, Pragma, Context),
                Item = item_pragma(ItemPragma),
                Result = ok1(Item)
            ;
                ErrMsg = "-- expected string for foreign declaration code",
                Result = error1([string.append(InvalidDeclStr, ErrMsg) -
                    HeaderTerm])
            )
        ;
            ErrMsg = "-- invalid language parameter",
            Result = error1([(InvalidDeclStr ++ ErrMsg) - LangTerm])
        )
    ;
        string.format("invalid `:- pragma %s' declaration ",
            [s(PragmaName)], ErrorStr),
        Result = error1([ErrorStr - ErrorTerm])
    ).

    % This predicate parses both c_code and foreign_code pragmas.
    % Processing of foreign_proc (or c_code that defines a procedure)
    % is handled in parse_pragma_foreign_proc_pragma below.
    %
:- pred parse_pragma_foreign_code_pragma(module_name::in, string::in,
    list(term)::in, term::in, varset::in, prog_context::in,
    maybe1(item)::out) is det.

parse_pragma_foreign_code_pragma(_ModuleName, PragmaName, PragmaTerms,
        ErrorTerm, _VarSet, Context, Result) :-
    string.format("invalid `:- pragma %s' declaration ", [s(PragmaName)],
        InvalidDeclStr),
    ( PragmaTerms = [LangTerm, CodeTerm] ->
        ( parse_foreign_language(LangTerm, ForeignLanguagePrime) ->
            ForeignLanguage = ForeignLanguagePrime,
            LangErrs = []
        ;
            LangMsg = InvalidDeclStr ++ "-- invalid language parameter",
            LangErrs = [LangMsg - LangTerm],
            ForeignLanguage = lang_c    % Dummy, ignored when LangErrs \= []
        ),
        ( CodeTerm = term.functor(term.string(CodePrime), [], _) ->
            Code = CodePrime,
            CodeErrs = []
        ;
            Code = "",                  % Dummy, ignored when CodeErrs \= []
            CodeMsg = InvalidDeclStr ++ "-- expected string for foreign code",
            CodeErrs = [CodeMsg - CodeTerm]
        ),
        Errs = LangErrs ++ CodeErrs,
        (
            Errs = [],
            Pragma = pragma_foreign_code(ForeignLanguage, Code),
            ItemPragma = item_pragma_info(user, Pragma, Context),
            Item = item_pragma(ItemPragma),
            Result = ok1(Item)
        ;
            Errs = [_ | _],
            Result = error1(Errs)
        )
    ;
        Msg = InvalidDeclStr ++ "-- wrong number of arguments",
        Result = error1([Msg - ErrorTerm])
    ).

    % This predicate parses both c_code and foreign_proc pragmas.
    %
:- pred parse_pragma_foreign_proc_pragma(module_name::in, string::in,
    list(term)::in, term::in, varset::in, prog_context::in,
    maybe1(item)::out) is det.

parse_pragma_foreign_proc_pragma(ModuleName, PragmaName, PragmaTerms,
        ErrorTerm, VarSet, Context, Result) :-
    string.format("invalid `:- pragma %s' declaration ", [s(PragmaName)],
        InvalidDeclStr),
    (
        PragmaTerms = [LangTerm | RestTerms],
        ( parse_foreign_language(LangTerm, ForeignLanguagePrime) ->
            ForeignLanguage = ForeignLanguagePrime,
            LangErrs = []
        ;
            ForeignLanguage = lang_c,   % Dummy, ignored when LangErrs \= []
            LangMsg = "-- invalid language parameter",
            LangErrs = [(InvalidDeclStr ++ LangMsg) - LangTerm]
        ),
        (
            (
                RestTerms = [PredAndVarsTerm, CodeTerm],
                parse_pragma_ordinary_foreign_proc_pragma_old(ModuleName,
                    PragmaName, VarSet, PredAndVarsTerm, CodeTerm, ErrorTerm,
                    ForeignLanguage, InvalidDeclStr, Context, RestResult)
            ;
                RestTerms = [PredAndVarsTerm, FlagsTerm, CodeTerm],
                parse_pragma_ordinary_foreign_proc_pragma(ModuleName,
                    PragmaName, VarSet, PredAndVarsTerm, FlagsTerm, CodeTerm,
                    ForeignLanguage, InvalidDeclStr, Context, RestResult)
            ;
                RestTerms = [PredAndVarsTerm, FlagsTerm, FieldsTerm,
                    FirstTerm, LaterTerm],
                term.context_init(DummyContext),
                SharedTerm = term.functor(term.atom("common_code"),
                    [term.functor(term.string(""), [], DummyContext)],
                    DummyContext),
                parse_pragma_model_non_foreign_proc_pragma(ModuleName,
                    PragmaName, VarSet, PredAndVarsTerm, FlagsTerm,
                    FieldsTerm, FirstTerm, LaterTerm, SharedTerm,
                    ForeignLanguage, InvalidDeclStr, Context, RestResult)
            ;
                RestTerms = [PredAndVarsTerm, FlagsTerm, FieldsTerm,
                    FirstTerm, LaterTerm, SharedTerm],
                parse_pragma_model_non_foreign_proc_pragma(ModuleName,
                    PragmaName, VarSet, PredAndVarsTerm, FlagsTerm,
                    FieldsTerm, FirstTerm, LaterTerm, SharedTerm,
                    ForeignLanguage, InvalidDeclStr, Context, RestResult)
            )
        ->
            (
                RestResult = ok1(Item),
                (
                    LangErrs = [],
                    Result = ok1(Item)
                ;
                    LangErrs = [_ | _],
                    Result = error1(LangErrs)
                )
            ;
                RestResult = error1(RestErrs),
                Result = error1(LangErrs ++ RestErrs)
            )
        ;
            ErrMsg = "-- wrong number of arguments",
            Result = error1([(InvalidDeclStr ++ ErrMsg) - ErrorTerm])
        )
    ;
        PragmaTerms = [],
        ErrMsg = "-- wrong number of arguments",
        Result = error1([(InvalidDeclStr ++ ErrMsg) - ErrorTerm])
    ).

:- pred parse_pragma_ordinary_foreign_proc_pragma_old(module_name::in,
    string::in, varset::in, term::in, term::in, term::in, foreign_language::in,
    string::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_ordinary_foreign_proc_pragma_old(ModuleName, PragmaName, VarSet,
        PredAndVarsTerm, CodeTerm, ErrorTerm, ForeignLanguage, InvalidDeclStr,
        Context, Result) :-
    % XXX We should issue a warning; this syntax is deprecated. We will
    % continue to accept this if c_code is used, but not with foreign_code.
    ( PragmaName = "c_code" ->
        Attributes0 = default_attributes(ForeignLanguage),
        set_legacy_purity_behaviour(yes, Attributes0, Attributes),
        ( CodeTerm = term.functor(term.string(Code), [], CodeContext) ->
            Impl = fc_impl_ordinary(Code, yes(CodeContext)),
            parse_pragma_foreign_code(ModuleName, Attributes,
                PredAndVarsTerm, Impl, VarSet, Context, Result)
        ;
            ErrMsg = "-- expecting either `may_call_mercury' or "
                ++ "`will_not_call_mercury', and a string for foreign code",
            Result = error1([(InvalidDeclStr ++ ErrMsg) - CodeTerm])
        )
    ;
        ErrMsg = "-- doesn't say whether it can call mercury",
        Result = error1([(InvalidDeclStr ++ ErrMsg) - ErrorTerm])
    ).

:- pred parse_pragma_ordinary_foreign_proc_pragma(module_name::in, string::in,
    varset::in, term::in, term::in, term::in, foreign_language::in,
    string::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_ordinary_foreign_proc_pragma(ModuleName, PragmaName, VarSet,
        SecondTerm, ThirdTerm, CodeTerm, ForeignLanguage, InvalidDeclStr,
        Context, Result) :-
    ( CodeTerm = term.functor(term.string(CodePrime), [], CodeContextPrime) ->
        Code = CodePrime,
        CodeContext = CodeContextPrime,
        CodeErrs = []
    ;
        Code = "",                                      % Dummy
        CodeContext = term.context_init,                % Dummy
        CodeMsg = "-- invalid fourth argument, "
            ++ "expecting string containing foreign code",
        CodeErrs = [(InvalidDeclStr ++ CodeMsg) - CodeTerm]
    ),
    parse_pragma_foreign_proc_attributes_term(ForeignLanguage, PragmaName,
        VarSet, ThirdTerm, MaybeFlagsThird),
    (
        MaybeFlagsThird = ok1(Flags),
        FlagsErrs = [],
        PredAndVarsTerm = SecondTerm
    ;
        MaybeFlagsThird = error1(FlagsThirdErrors),
        parse_pragma_foreign_proc_attributes_term(ForeignLanguage, PragmaName,
            VarSet, SecondTerm, MaybeFlagsSecond),
        (
            MaybeFlagsSecond = ok1(Flags),
            % XXX We should issue a warning; this syntax is deprecated.
            % We will continue to accept this if c_code is used,
            % but not with foreign_code.
            ( PragmaName = "c_code" ->
                PredAndVarsTerm = ThirdTerm,
                FlagsErrs = []
            ;
                PredAndVarsTerm = ThirdTerm,                % Dummy
                FlagsMsg = "-- invalid second argument, "
                    ++ "expecting predicate or function mode",
                FlagsErrs = [(InvalidDeclStr ++ FlagsMsg) - SecondTerm]
            )
        ;
            MaybeFlagsSecond = error1(_),
            Flags = default_attributes(ForeignLanguage),    % Dummy
            PredAndVarsTerm = SecondTerm,                   % Dummy
            % We report the error appropriate to the preferred syntax.
            FlagsErrs = assoc_list.map_keys_only(string.append(
                InvalidDeclStr ++ "-- invalid third argument: "),
                FlagsThirdErrors)
        )
    ),
    Errs = CodeErrs ++ FlagsErrs,
    (
        Errs = [],
        Impl = fc_impl_ordinary(Code, yes(CodeContext)),
        parse_pragma_foreign_code(ModuleName, Flags, PredAndVarsTerm,
            Impl, VarSet, Context, Result)
    ;
        Errs = [_ | _],
        Result = error1(Errs)
    ).

:- pred parse_pragma_model_non_foreign_proc_pragma(module_name::in, string::in,
    varset::in, term::in, term::in, term::in, term::in, term::in,
    term::in, foreign_language::in, string::in, prog_context::in,
    maybe1(item)::out) is det.

parse_pragma_model_non_foreign_proc_pragma(ModuleName, PragmaName, VarSet,
        PredAndVarsTerm, FlagsTerm, FieldsTerm, FirstTerm, LaterTerm,
        SharedTerm, ForeignLanguage, InvalidDeclStr, Context, Result) :-
    parse_pragma_foreign_proc_attributes_term(ForeignLanguage, PragmaName,
        VarSet, FlagsTerm, MaybeFlags),
    (
        MaybeFlags = ok1(Flags),
        FlagsErrs = []
    ;
        MaybeFlags = error1(FlagErrs0),
        Flags = default_attributes(ForeignLanguage),    % Dummy
        FlagsErrs = assoc_list.map_keys_only(string.append(
            InvalidDeclStr ++ "-- invalid third argument: "), FlagErrs0)
    ),
    (
        parse_pragma_keyword("local_vars", FieldsTerm,
            FieldsPrime, FieldsContextPrime)
    ->
        Fields = FieldsPrime,
        FieldsContext = FieldsContextPrime,
        LocalErrs = []
    ;
        Fields = "",                                    % Dummy
        FieldsContext = term.context_init,              % Dummy
        LocalMsg = "-- invalid fourth argument, "
            ++ "expecting `local_vars(<fields>)'",
        LocalErrs = [(InvalidDeclStr ++ LocalMsg) - FieldsTerm]
    ),
    (
        parse_pragma_keyword("first_code", FirstTerm,
            FirstPrime, FirstContextPrime)
    ->
        First = FirstPrime,
        FirstContext = FirstContextPrime,
        FirstErrs = []
    ;
        First = "",                                     % Dummy
        FirstContext = term.context_init,               % Dummy
        FirstMsg = "-- invalid fifth argument, expecting `first_code(<code>)'",
        FirstErrs = [(InvalidDeclStr ++ FirstMsg) - FirstTerm]
    ),
    (
        parse_pragma_keyword("retry_code", LaterTerm,
            LaterPrime, LaterContextPrime)
    ->
        Later = LaterPrime,
        LaterContext = LaterContextPrime,
        LaterErrs = []
    ;
        Later = "",                                     % Dummy
        LaterContext = term.context_init,               % Dummy
        LaterMsg = "-- invalid sixth argument, expecting `retry_code(<code>)'",
        LaterErrs = [(InvalidDeclStr ++ LaterMsg) - LaterTerm]
    ),
    (
        parse_pragma_keyword("shared_code", SharedTerm,
            SharedPrime, SharedContextPrime)
    ->
        Shared = SharedPrime,
        SharedContext = SharedContextPrime,
        Treatment = shared_code_share,
        SharedErrs = []
    ;
        parse_pragma_keyword("duplicated_code", SharedTerm,
            SharedPrime, SharedContextPrime)
    ->
        Shared = SharedPrime,
        SharedContext = SharedContextPrime,
        Treatment = shared_code_duplicate,
        SharedErrs = []
    ;
        parse_pragma_keyword("common_code", SharedTerm,
            SharedPrime, SharedContextPrime)
    ->
        Shared = SharedPrime,
        SharedContext = SharedContextPrime,
        Treatment = shared_code_automatic,
        SharedErrs = []
    ;
        Shared = "",                                    % Dummy
        SharedContext = term.context_init,              % Dummy
        Treatment = shared_code_automatic,              % Dummy
        SharedMsg = "-- invalid seventh argument, "
            ++ "expecting `common_code(<code>)'",
        SharedErrs = [(InvalidDeclStr ++ SharedMsg) - SharedTerm]
    ),
    Errs = FlagsErrs ++ LocalErrs ++ FirstErrs ++ LaterErrs ++ SharedErrs,
    (
        Errs = [],
        Impl = fc_impl_model_non(Fields, yes(FieldsContext),
            First, yes(FirstContext), Later, yes(LaterContext),
            Treatment, Shared, yes(SharedContext)),
        parse_pragma_foreign_code(ModuleName, Flags, PredAndVarsTerm,
            Impl, VarSet, Context, Result)
    ;
        Errs = [_ | _],
        Result = error1(Errs)
    ).

    % This parses a pragma that refers to a predicate or function.
    %
:- pred parse_simple_pragma(module_name::in, string::in,
    pred(sym_name, int, pragma_type)::(pred(in, in, out) is det),
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_simple_pragma(ModuleName, PragmaType, MakePragma, PragmaTerms, ErrorTerm,
        Context, Result) :-
    parse_simple_pragma_base(ModuleName, PragmaType, "predicate or function",
        MakePragma, PragmaTerms, ErrorTerm, Context, Result).

    % This parses a pragma that refers to type.
    %
:- pred parse_simple_type_pragma(module_name::in, string::in,
    pred(sym_name, int, pragma_type)::(pred(in, in, out) is det),
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_simple_type_pragma(ModuleName, PragmaType, MakePragma,
        PragmaTerms, ErrorTerm, Context, Result) :-
    parse_simple_pragma_base(ModuleName, PragmaType, "type", MakePragma,
        PragmaTerms, ErrorTerm, Context, Result).

    % This parses a pragma that refers to symbol name / arity.
    %
:- pred parse_simple_pragma_base(module_name::in, string::in, string::in,
    pred(sym_name, int, pragma_type)::(pred(in, in, out) is det),
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_simple_pragma_base(ModuleName, PragmaType, NameKind, MakePragma,
        PragmaTerms, ErrorTerm, Context, Result) :-
    ( PragmaTerms = [PredAndArityTerm] ->
        parse_simple_name_and_arity(ModuleName, PragmaType, NameKind,
            PredAndArityTerm, PredAndArityTerm, NameArityResult),
        (
            NameArityResult = ok2(PredName, Arity),
            MakePragma(PredName, Arity, Pragma),
            ItemPragma = item_pragma_info(user, Pragma, Context),
            Item = item_pragma(ItemPragma),
            Result = ok1(Item)
        ;
            NameArityResult = error2(Errors),
            Result = error1(Errors)
        )
    ;
        ErrorMsg = "wrong number of arguments in `:- pragma " ++
            PragmaType ++ "' declaration",
        Result = error1([ErrorMsg - ErrorTerm])
   ).

:- pred parse_pred_name_and_arity(module_name::in, string::in, term::in,
    term::in, maybe2(sym_name, arity)::out) is det.

parse_pred_name_and_arity(ModuleName, PragmaType, NameAndArityTerm, ErrorTerm,
        Result) :-
    parse_simple_name_and_arity(ModuleName, PragmaType,
        "predicate or function", NameAndArityTerm, ErrorTerm, Result).

:- pred parse_simple_name_and_arity(module_name::in, string::in, string::in,
    term::in, term::in, maybe2(sym_name, arity)::out) is det.

parse_simple_name_and_arity(ModuleName, PragmaType, NameKind,
        NameAndArityTerm, ErrorTerm, Result) :-
    ( parse_name_and_arity(ModuleName, NameAndArityTerm, Name, Arity) ->
        Result = ok2(Name, Arity)
    ;
        ErrorMsg = "expected " ++ NameKind ++ " name/arity for `pragma " ++
            PragmaType ++ "' declaration",
        Result = error2([ErrorMsg - ErrorTerm])
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
    string::in, varset::in, term::in,
    maybe1(pragma_foreign_proc_attributes)::out) is det.

parse_pragma_foreign_proc_attributes_term(ForeignLanguage, PragmaName, Varset,
        Term, MaybeAttributes) :-
    Attributes0 = default_attributes(ForeignLanguage),
    ( ( PragmaName = "c_code" ; PragmaName = "import" ) ->
        set_legacy_purity_behaviour(yes, Attributes0, Attributes1),
        set_purity(purity_pure, Attributes1, Attributes2)
    ;
        Attributes2 = Attributes0
    ),
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
            Msg = "conflicting attributes in attribute list",
            MaybeAttributes = error1([Msg - Term])
        ;
            list.foldl(process_attribute, AttrList, Attributes2, Attributes),
            MaybeAttributes = check_required_attributes(ForeignLanguage,
                Attributes, Term)
        )
    ;
        ErrMsg = "expecting a foreign proc attribute or list of attributes",
        MaybeAttributes = error1([ErrMsg - Term])
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
        pragma_foreign_proc_attributes, term)
    = maybe1(pragma_foreign_proc_attributes).

check_required_attributes(lang_c, Attrs, _Term) = ok1(Attrs).
check_required_attributes(lang_csharp, Attrs, _Term) = ok1(Attrs).
check_required_attributes(lang_il, Attrs, Term) = Res :-
    MaxStackAttrs = list.filter_map(
        (func(X) = X is semidet :-
            X = max_stack_size(_)),
        get_extra_attributes(Attrs)),
    (
        MaxStackAttrs = [],
        Res = error1(["expecting max_stack_size attribute for IL code" - Term])
    ;
        MaxStackAttrs = [_ | _],
        Res = ok1(Attrs)
    ).
check_required_attributes(lang_java, Attrs, _Term) = ok1(Attrs).
check_required_attributes(lang_erlang, Attrs, _Term) = ok1(Attrs).

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

:- pred parse_affects_liveness(term::in, proc_affects_liveness::out) is semidet.

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

:- pred parse_allocates_memory(term::in, proc_allocates_memory::out) is semidet.

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

    % Parse a pragma foreign_code declaration.
    %
:- pred parse_pragma_foreign_code(module_name::in,
    pragma_foreign_proc_attributes::in, term::in, pragma_foreign_code_impl::in,
    varset::in, prog_context::in, maybe1(item)::out) is det.

parse_pragma_foreign_code(ModuleName, Flags, PredAndVarsTerm0,
        PragmaImpl, VarSet0, Context, Result) :-
    parse_pred_or_func_and_args(yes(ModuleName), PredAndVarsTerm0,
        PredAndVarsTerm0, "`:- pragma c_code' declaration", PredAndArgsResult),
    (
        PredAndArgsResult = ok2(PredName, VarList0 - MaybeRetTerm),
        (
            % Is this a function or a predicate?
            MaybeRetTerm = yes(FuncResultTerm0)
        ->
            PredOrFunc = pf_function,
            list.append(VarList0, [FuncResultTerm0], VarList)
        ;
            PredOrFunc = pf_predicate,
            VarList = VarList0
        ),
        parse_pragma_c_code_varlist(VarSet0, VarList, PragmaVars, Error),
        (
            Error = no,
            varset.coerce(VarSet0, ProgVarSet),
            varset.coerce(VarSet0, InstVarSet),
            Pragma = pragma_foreign_proc(Flags, PredName, PredOrFunc,
                PragmaVars, ProgVarSet, InstVarSet, PragmaImpl),
            ItemPragma = item_pragma_info(user, Pragma, Context),
            Item = item_pragma(ItemPragma),
            Result = ok1(Item)
        ;
            Error = yes(ErrorMessage),
            Result = error1([ErrorMessage - PredAndVarsTerm0])
        )
    ;
        PredAndArgsResult = error2(Errors),
        Result = error1(Errors)
    ).

    % Parse the variable list in the pragma c code declaration.
    % The final argument is 'no' for no error, or 'yes(ErrorMessage)'.
    %
:- pred parse_pragma_c_code_varlist(varset::in, list(term)::in,
    list(pragma_var)::out, maybe(string)::out) is det.

parse_pragma_c_code_varlist(_, [], [], no).
parse_pragma_c_code_varlist(VarSet, [V|Vars], PragmaVars, Error):-
    (
        V = term.functor(term.atom("::"), [VarTerm, ModeTerm], _),
        VarTerm = term.variable(Var, _)
    ->
        ( varset.search_name(VarSet, Var, VarName) ->
            ( convert_mode(allow_constrained_inst_var, ModeTerm, Mode0) ->
                constrain_inst_vars_in_mode(Mode0, Mode),
                term.coerce_var(Var, ProgVar),
                PragmaVar = pragma_var(ProgVar, VarName, Mode,
                    native_if_possible),
                parse_pragma_c_code_varlist(VarSet, Vars, PragmaVars0, Error),
                PragmaVars = [PragmaVar | PragmaVars0]
            ;
                PragmaVars = [],
                Error = yes("unknown mode in pragma c_code")
            )
        ;
            % If the variable wasn't in the varset it must be an
            % underscore variable.
            PragmaVars = [],    % return any old junk for that.
            Error = yes("sorry, not implemented: anonymous " ++
                "`_' variable in pragma c_code")
        )
    ;
        PragmaVars = [],    % Return any old junk in PragmaVars.
        Error = yes("arguments not in form 'Var :: mode'")
    ).

:- pred parse_tabling_pragma(module_name::in, string::in, eval_method::in,
    list(term)::in, term::in, prog_context::in, maybe1(item)::out) is det.

parse_tabling_pragma(ModuleName, PragmaName, TablingType, PragmaTerms,
        ErrorTerm, Context, Result) :-
    (
        (
            PragmaTerms = [PredAndModesTerm0],
            MaybeAttrs = no
        ;
            PragmaTerms = [PredAndModesTerm0, AttrListTerm0],
            MaybeAttrs = yes(AttrListTerm0)
        )
    ->
        ParseMsg = "`:- pragma " ++ PragmaName ++ "' declaration",
        parse_arity_or_modes(ModuleName, PredAndModesTerm0, ErrorTerm,
            ParseMsg, ArityModesResult),
        (
            ArityModesResult = ok1(arity_or_modes(PredName, Arity,
                MaybePredOrFunc, MaybeModes)),
            (
                MaybeAttrs = no,
                Pragma = pragma_tabled(TablingType, PredName, Arity,
                    MaybePredOrFunc, MaybeModes, no),
                ItemPragma = item_pragma_info(user, Pragma, Context),
                Item = item_pragma(ItemPragma),
                Result = ok1(Item)
            ;
                MaybeAttrs = yes(AttrsListTerm),
                convert_maybe_list(AttrsListTerm,
                    parse_tabling_attribute(TablingType),
                    "expected tabling attribute", MaybeAttributeList),
                (
                    MaybeAttributeList = ok1(AttributeList),
                    update_tabling_attributes(AttributeList,
                        default_memo_table_attributes, MaybeAttributes),
                    (
                        MaybeAttributes = ok1(Attributes),
                        Pragma = pragma_tabled(TablingType, PredName,
                            Arity, MaybePredOrFunc, MaybeModes,
                            yes(Attributes)),
                        ItemPragma = item_pragma_info(user, Pragma, Context),
                        Item = item_pragma(ItemPragma),
                        Result = ok1(Item)
                    ;
                        MaybeAttributes = error1(Errors),
                        Result = error1(Errors)
                    )
                ;
                    MaybeAttributeList = error1(Errors),
                    Result = error1(Errors)
                )
            )
        ;
            ArityModesResult = error1(Errors),
            Result = error1(Errors)
        )
    ;
        ErrorMsg = "wrong number of arguments in `:- pragma " ++
            PragmaName ++ "' declaration",
        Result = error1([ErrorMsg - ErrorTerm])
    ).

:- type single_tabling_attribute
    --->    attr_strictness(call_table_strictness)
    ;       attr_size_limit(int)
    ;       attr_statistics
    ;       attr_allow_reset.

:- pred update_tabling_attributes(
    assoc_list(term, single_tabling_attribute)::in,
    table_attributes::in, maybe1(table_attributes)::out) is det.

update_tabling_attributes([], Attributes, ok1(Attributes)).
update_tabling_attributes([Term - SingleAttr | TermSingleAttrs], !.Attributes,
        MaybeAttributes) :-
    (
        SingleAttr = attr_strictness(Strictness),
        ( !.Attributes ^ table_attr_strictness = all_strict ->
            !:Attributes = !.Attributes ^ table_attr_strictness := Strictness,
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        ;
            Msg = "duplicate argument tabling methods attribute"
                ++ " in `:- pragma memo' declaration",
            MaybeAttributes = error1([Msg - Term])
        )
    ;
        SingleAttr = attr_size_limit(Limit),
        ( !.Attributes ^ table_attr_size_limit = no ->
            !:Attributes = !.Attributes ^ table_attr_size_limit := yes(Limit),
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        ;
            Msg = "duplicate size limits attribute"
                ++ " in `:- pragma memo' declaration",
            MaybeAttributes = error1([Msg - Term])
        )
    ;
        SingleAttr = attr_statistics,
        (
            !.Attributes ^ table_attr_statistics
                = table_dont_gather_statistics
        ->
            !:Attributes = !.Attributes ^ table_attr_statistics
                := table_gather_statistics,
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        ;
            Msg = "duplicate statistics attribute"
                ++ " in `:- pragma memo' declaration",
            MaybeAttributes = error1([Msg - Term])
        )
    ;
        SingleAttr = attr_allow_reset,
        ( !.Attributes ^ table_attr_allow_reset = table_dont_allow_reset ->
            !:Attributes = !.Attributes ^ table_attr_allow_reset
                := table_allow_reset,
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        ;
            Msg = "duplicate allow_reset attribute"
                ++ " in `:- pragma memo' declaration",
            MaybeAttributes = error1([Msg - Term])
        )
    ).

:- pred parse_tabling_attribute(eval_method::in, term::in,
    maybe1(pair(term, single_tabling_attribute))::out) is semidet.

parse_tabling_attribute(EvalMethod, Term, MaybeTermAttribute) :-
    Term = term.functor(term.atom(Functor), Args, _),
    (
        Functor = "fast_loose",
        Args = [],
        ( eval_method_allows_fast_loose(EvalMethod) = yes ->
            Attribute = attr_strictness(all_fast_loose),
            MaybeTermAttribute = ok1(Term - Attribute)
        ;
            Msg = "evaluation method " ++ eval_method_to_string(EvalMethod) ++
                " doesn't allow fast_loose tabling",
            MaybeTermAttribute = error1([Msg - Term])
        )
    ;
        Functor = "specified",
        Args = [Arg1 | MoreArgs],
        convert_list(Arg1, parse_arg_tabling_method,
            "expected argument tabling method", MaybeMaybeArgMethods),
        (
            MaybeMaybeArgMethods = ok1(MaybeArgMethods),
            ( eval_method_allows_fast_loose(EvalMethod) = yes ->
                (
                    MoreArgs = [],
                    Attribute = attr_strictness(
                        specified(MaybeArgMethods, hidden_arg_value)),
                    MaybeTermAttribute = ok1(Term - Attribute)
                ;
                    MoreArgs = [Arg2],
                    (
                        Arg2 = term.functor(
                            term.atom("hidden_arg_value"), [], _)
                    ->
                        Attribute = attr_strictness(
                            specified(MaybeArgMethods, hidden_arg_value)),
                        MaybeTermAttribute = ok1(Term - Attribute)
                    ;
                        Arg2 = term.functor(
                            term.atom("hidden_arg_addr"), [], _)
                    ->
                        Attribute = attr_strictness(
                            specified(MaybeArgMethods, hidden_arg_addr)),
                        MaybeTermAttribute = ok1(Term - Attribute)
                    ;
                        Msg = "expected hidden argument tabling method",
                        MaybeTermAttribute = error1([Msg - Arg2])
                    )
                ;
                    MoreArgs = [_, _ | _],
                    Msg = "expected one or two arguments",
                    MaybeTermAttribute = error1([Msg - Term])
                )
            ;
                Msg = "evaluation method " ++
                    eval_method_to_string(EvalMethod) ++
                    " doesn't allow specified tabling methods",
                MaybeTermAttribute = error1([Msg - Term])
            )
        ;
            MaybeMaybeArgMethods = error1(Errors),
            MaybeTermAttribute = error1(Errors)
        )
    ;
        Functor = "size_limit",
        Args = [Arg],
        Arg = term.functor(term.integer(Limit), [], _),
        ( eval_method_allows_size_limit(EvalMethod) = yes ->
            Attribute = attr_size_limit(Limit),
            MaybeTermAttribute = ok1(Term - Attribute)
        ;
            Msg = "evaluation method " ++ eval_method_to_string(EvalMethod) ++
                " doesn't allow size limits",
            MaybeTermAttribute = error1([Msg - Term])
        )
    ;
        Functor = "statistics",
        Args = [],
        Attribute = attr_statistics,
        MaybeTermAttribute = ok1(Term - Attribute)
    ;
        Functor = "allow_reset",
        Args = [],
        Attribute = attr_allow_reset,
        MaybeTermAttribute = ok1(Term - Attribute)
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

:- pred parse_arity_or_modes(module_name::in, term::in, term::in,
    string::in, maybe1(arity_or_modes)::out) is det.

parse_arity_or_modes(ModuleName, PredAndModesTerm0,
        ErrorTerm, ErrorMsg, Result) :-
    (
                % Is this a simple pred/arity pragma.
        PredAndModesTerm0 = term.functor(term.atom("/"),
            [PredNameTerm, ArityTerm], _)
    ->
        (
            parse_implicitly_qualified_term(ModuleName, PredNameTerm,
                PredAndModesTerm0, "", ok2(PredName, [])),
            ArityTerm = term.functor(term.integer(Arity), [], _)
        ->
            Result = ok1(arity_or_modes(PredName, Arity, no, no))
        ;
            Msg = "expected predname/arity for" ++ ErrorMsg,
            Result = error1([Msg - ErrorTerm])
        )
    ;
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            PredAndModesTerm0, ErrorMsg, PredAndModesResult),
        (
            PredAndModesResult = ok2(PredName - PredOrFunc, Modes),
            list.length(Modes, Arity0),
            (
                PredOrFunc = pf_function,
                Arity = Arity0 - 1
            ;
                PredOrFunc = pf_predicate,
                Arity = Arity0
            ),
            Result = ok1(arity_or_modes(PredName, Arity, yes(PredOrFunc),
                yes(Modes)))
        ;
            PredAndModesResult = error2(Errors),
            Result = error1(Errors)
        )
    ).

:- type maybe_pred_or_func_modes ==
        maybe2(pair(sym_name, pred_or_func), list(mer_mode)).

:- pred parse_pred_or_func_and_arg_modes(maybe(module_name)::in, term::in,
    term::in, string::in, maybe_pred_or_func_modes::out) is det.

parse_pred_or_func_and_arg_modes(MaybeModuleName, PredAndModesTerm,
        ErrorTerm, Msg, Result) :-
    parse_pred_or_func_and_args(MaybeModuleName, PredAndModesTerm,
        ErrorTerm, Msg, PredAndArgsResult),
    (
        PredAndArgsResult = ok2(PredName, ArgModeTerms - MaybeRetModeTerm),
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
                    list.append(ArgModes0, [RetMode], ArgModes1),
                    list.map(constrain_inst_vars_in_mode, ArgModes1, ArgModes),
                    Result = ok2(PredName - pf_function, ArgModes)
                ;
                    ErrorMsg = "error in return mode in " ++ Msg,
                    Result = error2([ErrorMsg - ErrorTerm])
                )
            ;
                MaybeRetModeTerm = no,
                Result = ok2(PredName - pf_predicate, ArgModes0)
            )
        ;
            ErrorMsg = "error in argument modes in " ++ Msg,
            Result = error2([ErrorMsg - ErrorTerm])
        )
    ;
        PredAndArgsResult = error2(Errors),
        Result = error2(Errors)
    ).

:- pred convert_bool(term::in, bool::out) is semidet.

convert_bool(Term, Bool) :-
    Term = term.functor(term.atom(Name), [], _),
    ( Name = "yes", Bool = yes
    ; Name = "no", Bool = no
    ).

:- pred convert_bool_list(term::in, list(bool)::out) is semidet.

convert_bool_list(ListTerm, Bools) :-
    convert_list(ListTerm, convert_bool, "expected boolean", ok1(Bools)).

:- pred convert_int(term::in, int::out) is semidet.

convert_int(Term, Int) :-
    Term = term.functor(term.integer(Int), [], _).

:- pred convert_int_list(term::in, maybe1(list(int))::out) is det.

convert_int_list(ListTerm, Result) :-
    convert_list(ListTerm, convert_int, "expected integer", Result).

    % convert_list(T, P, M) will convert a term T into a list of
    % type X where P is a predicate that converts each element of
    % the list into the correct type.  M will hold the list if the
    % conversion succeded for each element of M, otherwise it will
    % hold the error.
    %
:- pred convert_list(term::in, pred(term, T)::(pred(in, out) is semidet),
    string::in, maybe1(list(T))::out) is det.

convert_list(term.variable(V, C), _, UnrecognizedMsg,
        error1([UnrecognizedMsg - term.variable(V, C)])).
convert_list(term.functor(Functor, Args, Context), Pred, UnrecognizedMsg,
        Result) :-
    (
        Functor = term.atom("[|]"),
        Args = [Term, RestTerm]
    ->
        ( Pred(Term, Element) ->
            convert_list(RestTerm, Pred, UnrecognizedMsg, RestResult),
            (
                RestResult = ok1(List0),
                Result = ok1([Element | List0])
            ;
                RestResult = error1(_),
                Result = RestResult
            )
        ;
            Result = error1([UnrecognizedMsg - Term])
        )
    ;
        Functor = term.atom("[]"),
        Args = []
    ->
        Result = ok1([])
    ;
        Result = error1(["error in list" -
            term.functor(Functor, Args, Context)])
    ).

    % convert_list(T, P, M) will convert a term T into a list of
    % type X where P is a predicate that converts each element of
    % the list into the correct type.  M will hold the list if the
    % conversion succeded for each element of M, otherwise it will
    % hold the error.
    %
:- pred convert_maybe_list(term::in,
    pred(term, maybe1(T))::(pred(in, out) is semidet),
    string::in, maybe1(list(T))::out) is det.

convert_maybe_list(term.variable(V, C), _, UnrecognizedMsg,
        error1([UnrecognizedMsg - term.variable(V, C)])).
convert_maybe_list(term.functor(Functor, Args, Context), Pred, UnrecognizedMsg,
        Result) :-
    (
        Functor = term.atom("[|]"),
        Args = [Term, RestTerm]
    ->
        ( Pred(Term, ElementResult) ->
            (
                ElementResult = ok1(Element),
                convert_maybe_list(RestTerm, Pred, UnrecognizedMsg,
                    RestResult),
                (
                    RestResult = ok1(List0),
                    Result = ok1([Element | List0])
                ;
                    RestResult = error1(_),
                    Result = RestResult
                )
            ;
                ElementResult = error1(Errors),
                Result = error1(Errors)
            )
        ;
            Result = error1([UnrecognizedMsg - Term])
        )
    ;
        Functor = term.atom("[]"),
        Args = []
    ->
        Result = ok1([])
    ;
        Result = error1(["error in list" -
            term.functor(Functor, Args, Context)])
    ).

:- pred convert_type_spec_pair(term::in, pair(tvar, mer_type)::out) is semidet.

convert_type_spec_pair(Term, TypeSpec) :-
    Term = term.functor(term.atom("="), [TypeVarTerm, SpecTypeTerm0], _),
    TypeVarTerm = term.variable(TypeVar0, _),
    term.coerce_var(TypeVar0, TypeVar),
    parse_type(SpecTypeTerm0, ok1(SpecType)),
    TypeSpec = TypeVar - SpecType.

%-----------------------------------------------------------------------------%
%
% Parsing termination2_info pragmas.
%

:- pred parse_arg_size_constraints(term::in,
    maybe1(maybe(list(arg_size_constr)))::out) is semidet.

parse_arg_size_constraints(ArgSizeTerm, Result) :-
    (
        ArgSizeTerm = term.functor(term.atom("not_set"), [], _),
        Result = ok1(no)
    ;
        ArgSizeTerm = term.functor(term.atom("constraints"),
            [Constraints0], _),
        convert_list(Constraints0, parse_arg_size_constraint,
            "expected constraint", ConstraintsResult),
        ConstraintsResult = ok1(Constraints),
        Result = ok1(yes(Constraints))
    ).

:- pred parse_arg_size_constraint(term::in, arg_size_constr::out) is semidet.

parse_arg_size_constraint(Term, Constr) :-
    (
        Term = term.functor(term.atom("le"), [Terms, ConstantTerm], _),
        convert_list(Terms, parse_lp_term, "expected linear term",
            TermsResult),
        TermsResult = ok1(LPTerms),
        parse_rational(ConstantTerm, Constant),
        Constr = le(LPTerms, Constant)

    ;
        Term = term.functor(term.atom("eq"), [Terms, ConstantTerm], _),
        convert_list(Terms, parse_lp_term, "expected linear term",
            TermsResult),
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

parse_required_feature(ReqFeatureTerm, Result) :-
    ReqFeatureTerm = term.functor(term.atom(Functor), [], _),
    string_to_required_feature(Functor, ReqFeature),
    Result = ok1(ReqFeature).

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

:- func this_file = string.

this_file = "prog_io_pragma.m".

%-----------------------------------------------------------------------------%
:- end_module prog_io_pragma.
%-----------------------------------------------------------------------------%
