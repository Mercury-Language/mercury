%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_pragma.m.
% Main authors: fjh, dgj, zs.
%
% This module handles the parsing of pragma directives.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_pragma.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % Parse the pragma declaration. What it returns is not necessarily
    % a pragma item, and it may not even be an item.
    %
:- pred parse_pragma(module_name::in, varset::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

:- pred parse_foreign_type_assertions(cord(format_component)::in,
    varset::in, term::in,
    set(foreign_type_assertion)::in, set(foreign_type_assertion)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Parse a term that represents a foreign language.
    %
:- pred term_to_foreign_language(term::in, foreign_language::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.rat.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_defn.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

parse_pragma(ModuleName, VarSet, PragmaTerms, Context, SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [PragmaTerm],
        PragmaTerm = term.functor(term.atom(PragmaName), PragmaArgTerms,
            PragmaContext)
    then
        ( if
            parse_pragma_type(ModuleName, VarSet, PragmaTerm,
                PragmaName, PragmaArgTerms, PragmaContext, SeqNum,
                MaybeIOMPrime)
        then
            MaybeIOM = MaybeIOMPrime
        else
            Pieces = [words("Error:"), quote(PragmaName),
                words("is not a recognized pragma name."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            MaybeIOM = error1([Spec])
        )
    else
        Spec = report_unrecognized_pragma(Context),
        MaybeIOM = error1([Spec])
    ).

:- func report_unrecognized_pragma(prog_context) = error_spec.

report_unrecognized_pragma(Context) = Spec :-
    Pieces = [words("Error: a"), decl("pragma"), words("declaration"),
        words("should have the form"),
        quote(":- pragma pragma_name(pragma_arguments)."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        Context, Pieces).

%---------------------------------------------------------------------------%

:- pred parse_pragma_type(module_name::in, varset::in, term::in,
    string::in, list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is semidet.

parse_pragma_type(ModuleName, VarSet, ErrorTerm, PragmaName, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    require_switch_arms_det [PragmaName]
    (
        PragmaName = "source_file",
        parse_pragma_source_file(PragmaTerms, Context, MaybeIOM)
    ;
        PragmaName = "foreign_type",
        parse_pragma_foreign_type(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, ok1(canon), MaybeIOM)
    ;
        PragmaName = "foreign_decl",
        parse_pragma_foreign_decl(VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_code",
        parse_pragma_foreign_code(VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_proc",
        parse_pragma_foreign_proc(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_export_enum",
        parse_pragma_foreign_export_enum(VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_enum",
        parse_pragma_foreign_enum(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_export",
        parse_pragma_foreign_export(VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_import_module",
        parse_pragma_foreign_import_module(VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        (
            PragmaName = "external_pred",
            PorF = pf_predicate
        ;
            PragmaName = "external_func",
            PorF = pf_function
        ),
        parse_pragma_external_proc(ModuleName, VarSet, ErrorTerm,
            PragmaName, PragmaTerms, Context, SeqNum, PorF, MaybeIOM)
    ;
        PragmaName = "obsolete",
        parse_pragma_obsolete_pred(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "obsolete_proc",
        parse_pragma_obsolete_proc(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeIOM)
    ;
        (
            PragmaName = "terminates",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = decl_pragma_terminates(PredNameArity)
                )
        ;
            PragmaName = "does_not_terminate",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = decl_pragma_does_not_terminate(PredNameArity)
                )
        ;
            PragmaName = "check_termination",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = decl_pragma_check_termination(PredNameArity)
                )
        ),
        parse_name_arity_decl_pragma(ModuleName, PragmaName,
            "predicate or function", MakePragma, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeIOM)
    ;
        (
            PragmaName = "inline",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = impl_pragma_inline(PredNameArity)
                )
        ;
            PragmaName = "no_inline",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = impl_pragma_no_inline(PredNameArity)
                )
        ;
            PragmaName = "consider_used",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = impl_pragma_consider_used(PredNameArity)
                )
        ;
            PragmaName = "no_determinism_warning",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = impl_pragma_no_detism_warning(PredNameArity)
                )
        ;
            PragmaName = "mode_check_clauses",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = impl_pragma_mode_check_clauses(PredNameArity)
                )
        ;
            PragmaName = "promise_pure",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = impl_pragma_promise_pure(PredNameArity)
                )
        ;
            PragmaName = "promise_semipure",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = impl_pragma_promise_semipure(PredNameArity)
                )
        ;
            PragmaName = "promise_equivalent_clauses",
            MakePragma =
                ( pred(Name::in, Arity::in, Pragma::out) is det :-
                    PredNameArity = pred_name_arity(Name, Arity),
                    Pragma = impl_pragma_promise_eqv_clauses(PredNameArity)
                )
        ),
        parse_name_arity_impl_pragma(ModuleName, PragmaName,
            "predicate or function", MakePragma, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "require_tail_recursion",
        parse_pragma_require_tail_recursion(ModuleName, PragmaTerms,
            ErrorTerm, VarSet, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "oisu",
        parse_oisu_pragma(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        (
            PragmaName = "memo",
            % We don't know yet whether the pragma has a
            % disable_warning_if_ignored attribute, but if it does,
            % parse_tabling_pragma will override this placeholder argument.
            EvalMethod = eval_memo(table_attr_ignore_with_warning)
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
        parse_tabling_pragma(ModuleName, VarSet, ErrorTerm,
            PragmaName, PragmaTerms, Context, SeqNum, EvalMethod, MaybeIOM)
    ;
        PragmaName = "unused_args",
        parse_pragma_unused_args(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "type_spec",
        parse_pragma_type_spec(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "fact_table",
        parse_pragma_fact_table(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "termination_info",
        parse_pragma_termination_info(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "termination2_info",
        parse_pragma_termination2_info(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "structure_sharing",
        parse_pragma_structure_sharing(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "structure_reuse",
        parse_pragma_structure_reuse(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "exceptions",
        parse_pragma_exceptions(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "trailing_info",
        parse_pragma_trailing_info(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "mm_tabling_info",
        parse_pragma_mm_tabling_info(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "require_feature_set",
        parse_pragma_require_feature_set(VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "where",
        ( if
            PragmaTerms = [BeforeWhereTerm, WhereTerm],
            BeforeWhereTerm = term.functor(term.atom("foreign_type"),
                BeforeWherePragmaTerms, BeforeWhereContext)
        then
            parse_where_unify_compare(ModuleName, VarSet, WhereTerm,
                MaybeMaybeUC),
            parse_pragma_foreign_type(ModuleName, VarSet, ErrorTerm,
                BeforeWherePragmaTerms, BeforeWhereContext, SeqNum,
                MaybeMaybeUC, MaybeIOM)
        else
            Spec = report_unrecognized_pragma(Context),
            MaybeIOM = error1([Spec])
        )
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_source_file(list(term)::in, prog_context::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_source_file(PragmaTerms, Context, MaybeIOM) :-
    ( if PragmaTerms = [SourceFileTerm] then
        ( if SourceFileTerm = term.functor(term.string(SourceFile), [], _) then
            Marker = iom_marker_src_file(SourceFile),
            MaybeIOM = ok1(Marker)
        else
            Pieces = [words("Error: the argument of a"),
                pragma_decl("source_file"),
                words("declaration should be a string."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            MaybeIOM = error1([Spec])
        )
    else
        Pieces = [words("Error: a"), pragma_decl("source_file"),
            words("declaration must have exactly one argument."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

% XXX The predicates in the rest of this module ought to be clustered together
% into groups of related predicates, grouping both parse_pragma_xxx predicates
% together with their helper predicates, and grouping parse_pragma_xxx
% predicates for related xxxs together.

:- pred parse_pragma_foreign_type(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(maybe_canonical)::in, maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_type(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeMaybeCanonical, MaybeIOM) :-
    ( if
        (
            PragmaTerms = [LangTerm, MercuryTypeTerm, ForeignTypeTerm],
            MaybeAssertionTerm = no
        ;
            PragmaTerms = [LangTerm, MercuryTypeTerm, ForeignTypeTerm,
                AssertionTerm0],
            MaybeAssertionTerm = yes(AssertionTerm0)
        )
    then
        LangContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("foreign_type"), words("declaration:"), nl]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        TypeDefnHeadContextPieces =
            cord.from_list([words("In the second argument of"),
            pragma_decl("foreign_type"), words("declaration:"), nl]),
        parse_type_defn_head(TypeDefnHeadContextPieces,
            ModuleName, VarSet, MercuryTypeTerm, MaybeTypeDefnHead),
        ForeignTypeContextPieces =
            cord.from_list([words("In the third argument of"),
            pragma_decl("foreign_type"), words("declaration:"), nl]),
        parse_foreign_language_type(ForeignTypeContextPieces, ForeignTypeTerm,
            VarSet, MaybeForeignLang, MaybeForeignType),
        (
            MaybeAssertionTerm = no,
            AssertionsSet = set.init,
            AssertionSpecs = []
        ;
            MaybeAssertionTerm = yes(AssertionTerm),
            AssertionContextPieces =
                cord.from_list([words("In the fourth argument of"),
                pragma_decl("foreign_type"), words("declaration:"), nl]),
            parse_foreign_type_assertions(AssertionContextPieces, VarSet,
                AssertionTerm, set.init, AssertionsSet,
                [], AssertionSpecs)
        ),
        Assertions = foreign_type_assertions(AssertionsSet),
        ( if
            MaybeForeignLang = ok1(_),
            MaybeTypeDefnHead = ok2(MercuryTypeSymName, MercuryParams),
            MaybeForeignType = ok1(ForeignType),
            AssertionSpecs = [],
            MaybeMaybeCanonical = ok1(MaybeCanonical)
        then
            varset.coerce(VarSet, TVarSet),
            TypeDetailsForeign =
                type_details_foreign(ForeignType, MaybeCanonical, Assertions),
            ItemTypeDefn = item_type_defn_info(MercuryTypeSymName,
                MercuryParams, parse_tree_foreign_type(TypeDetailsForeign),
                TVarSet, Context, SeqNum),
            Item = item_type_defn(ItemTypeDefn),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++
                get_any_errors2(MaybeTypeDefnHead) ++
                get_any_errors1(MaybeForeignType) ++
                AssertionSpecs ++
                get_any_errors1(MaybeMaybeCanonical),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl("foreign_type"),
            words("declaration must have three or four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

parse_foreign_type_assertions(ContextPieces, VarSet, Term,
        !Assertions, !Specs) :-
    ( if Term = term.functor(term.atom("[]"), [], _) then
        true
    else if Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) then
        ( if parse_foreign_type_assertion(HeadTerm, HeadAssertion) then
            ( if set.insert_new(HeadAssertion, !Assertions) then
                true
            else
                HeadTermStr = mercury_term_to_string(VarSet, print_name_only,
                    HeadTerm),
                Pieces = cord.list(ContextPieces) ++
                    [lower_case_next_if_not_first, words("Error:"),
                    words("foreign type assertion"), quote(HeadTermStr),
                    words("is repeated."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(HeadTerm), Pieces),
                !:Specs = [Spec | !.Specs]
            )
        else
            TermStr = mercury_term_to_string(VarSet, print_name_only, Term),
            Pieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first,
                words("Error: expected a foreign type assertion,"),
                words("got"), quote(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(HeadTerm), Pieces),
            !:Specs = [Spec | !.Specs]
        ),
        parse_foreign_type_assertions(ContextPieces, VarSet, TailTerm,
            !Assertions, !Specs)
    else
        TermStr = mercury_term_to_string(VarSet, print_name_only, Term),
        Pieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first,
            words("Error: expected a list of foreign type assertions,"),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred parse_foreign_type_assertion(term::in,
    foreign_type_assertion::out) is semidet.

parse_foreign_type_assertion(Term, Assertion) :-
    Term = term.functor(term.atom(Constant), [], _),
    (
        Constant = "can_pass_as_mercury_type",
        Assertion = foreign_type_can_pass_as_mercury_type
    ;
        Constant = "stable",
        Assertion = foreign_type_stable
    ;
        Constant = "word_aligned_pointer",
        Assertion = foreign_type_word_aligned_pointer
    ).

%---------------------------------------------------------------------------%
%
% Code for parsing foreign_export_enum pragmas.
%

:- pred parse_pragma_foreign_export_enum(varset::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_export_enum(VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
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
    then
        LangContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("foreign_export_enum"), words("declaration:"), nl]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        TypeContextPieces = cord.from_list([words("In the second argument of"),
            pragma_decl("foreign_export_enum"), words("declaration:"), nl]),
        parse_type_ctor_name_arity(TypeContextPieces, VarSet,
            MercuryTypeTerm, MaybeTypeCtor),
        AttrContextPieces = [words("In the third argument of"),
            pragma_decl("foreign_export_enum"), words("declaration:"), nl],
        maybe_parse_export_enum_attributes(AttrContextPieces, VarSet,
            MaybeAttributesTerm, MaybeAttributes),
        maybe_parse_export_enum_overrides(VarSet, MaybeOverridesTerm,
            MaybeOverrides),
        ( if
            MaybeForeignLang = ok1(ForeignLang),
            MaybeTypeCtor = ok1(TypeCtor),
            MaybeAttributes = ok1(Attributes),
            MaybeOverrides = ok1(Overrides)
        then
            ItemForeignExportEnum = item_foreign_export_enum_info(ForeignLang,
                TypeCtor, Attributes, Overrides, Context, SeqNum),
            Item = item_foreign_export_enum(ItemForeignExportEnum),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++
                get_any_errors1(MaybeTypeCtor) ++
                get_any_errors1(MaybeAttributes) ++
                get_any_errors1(MaybeOverrides),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl("foreign_export_enum"),
            words("declaration must have two, three or four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred maybe_parse_export_enum_overrides(varset::in, maybe(term)::in,
    maybe1(assoc_list(sym_name, string))::out) is det.

maybe_parse_export_enum_overrides(_, no, ok1([])).
maybe_parse_export_enum_overrides(VarSet, yes(OverridesTerm),
        MaybeOverrides) :-
    convert_list("a list of mapping elements", parse_sym_name_string_pair,
        VarSet, OverridesTerm, MaybeOverrides).

:- pred parse_sym_name_string_pair(varset::in, term::in,
    maybe1(pair(sym_name, string))::out) is det.

parse_sym_name_string_pair(VarSet, PairTerm, MaybePair) :-
    ( if
        PairTerm = term.functor(term.atom("-"), ArgTerms, _),
        ArgTerms = [SymNameTerm, StringTerm],
        StringTerm = functor(term.string(String), _, _)
    then
        ( if try_parse_sym_name_and_no_args(SymNameTerm, SymName) then
            MaybePair = ok1(SymName - String)
        else
            SymNameTermStr = describe_error_term(VarSet, SymNameTerm),
            Pieces = [words("Error: expected a possibly qualified name,"),
                words("got"), quote(SymNameTermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(SymNameTerm),
                Pieces),
            MaybePair = error1([Spec])
        )
    else
        PairTermStr = describe_error_term(VarSet, PairTerm),
        Pieces = [words("Error: expected a mapping element"),
            words("of the form"), quote("possibly_qualified_name - string"),
            suffix(","), words("got"), quote(PairTermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(PairTerm), Pieces),
        MaybePair = error1([Spec])
    ).

:- pred maybe_parse_export_enum_attributes(list(format_component)::in,
    varset::in, maybe(term)::in, maybe1(export_enum_attributes)::out) is det.

maybe_parse_export_enum_attributes(_, _, no,
        ok1(default_export_enum_attributes)).
maybe_parse_export_enum_attributes(ContextPieces, VarSet, yes(AttributesTerm),
        MaybeAttributes) :-
    parse_export_enum_attributes(ContextPieces, VarSet, AttributesTerm,
        MaybeAttributes).

:- type collected_export_enum_attribute
    --->    ee_attr_prefix(maybe(string))
    ;       ee_attr_upper(uppercase_export_enum).

:- pred parse_export_enum_attributes(list(format_component)::in, varset::in,
    term::in, maybe1(export_enum_attributes)::out) is det.

parse_export_enum_attributes(ContextPieces, VarSet, AttributesTerm,
        AttributesResult) :-
    Attributes0 = default_export_enum_attributes,
    ( if list_term_to_term_list(AttributesTerm, AttributesTerms) then
        map_parser(parse_export_enum_attr(ContextPieces, VarSet),
            AttributesTerms, MaybeAttrList),
        (
            MaybeAttrList = ok1(CollectedAttributes),
            % Check that the prefix attribute is specified at most once.
            IsPrefixAttr =
                ( pred(A::in) is semidet :-
                    A = ee_attr_prefix(_)
                ),
            list.filter(IsPrefixAttr, CollectedAttributes, PrefixAttributes),
            (
                ( PrefixAttributes = []
                ; PrefixAttributes = [_]
                ),
                list.foldl(process_export_enum_attribute,
                    CollectedAttributes, Attributes0, Attributes),
                AttributesResult = ok1(Attributes)
            ;
                PrefixAttributes = [_, _ | _],
                Pieces = ContextPieces ++
                    [lower_case_next_if_not_first,
                    words("Error: the prefix attribute"),
                    words("may not occur more than once."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(AttributesTerm), Pieces),
                AttributesResult = error1([Spec])
            )
        ;
            MaybeAttrList = error1(AttrSpecs),
            AttributesResult = error1(AttrSpecs)
        )
    else
        AttributesStr = describe_error_term(VarSet, AttributesTerm),
        Pieces = ContextPieces ++
            [lower_case_next_if_not_first,
            words("Error: expected a list of attributes,"),
            words("got"), quote(AttributesStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(AttributesTerm), Pieces),
        AttributesResult = error1([Spec])
    ).

:- pred process_export_enum_attribute(collected_export_enum_attribute::in,
    export_enum_attributes::in, export_enum_attributes::out) is det.

process_export_enum_attribute(ee_attr_prefix(MaybePrefix), !Attributes) :-
    % We have already checked that the prefix attribute is not specified
    % multiple times in parse_export_enum_attributes so it is safe to
    % ignore it in the input here.
    !.Attributes = export_enum_attributes(_, MakeUpperCase),
    !:Attributes = export_enum_attributes(MaybePrefix, MakeUpperCase).
process_export_enum_attribute(ee_attr_upper(MakeUpperCase), !Attributes) :-
    !.Attributes = export_enum_attributes(MaybePrefix, _),
    !:Attributes = export_enum_attributes(MaybePrefix, MakeUpperCase).

:- pred parse_export_enum_attr(list(format_component)::in, varset::in, term::in,
    maybe1(collected_export_enum_attribute)::out) is det.

parse_export_enum_attr(ContextPieces, VarSet, Term, MaybeAttribute) :-
    ( if
        Term = functor(atom("prefix"), Args, _),
        Args = [ForeignNameTerm],
        ForeignNameTerm = functor(string(Prefix), [], _)
    then
        MaybeAttribute = ok1(ee_attr_prefix(yes(Prefix)))
    else if
        Term = functor(atom("uppercase"), [], _)
    then
        MaybeAttribute = ok1(ee_attr_upper(uppercase_export_enum))
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = ContextPieces ++
            [lower_case_next_if_not_first,
            words("Error: expected one of"),
            quote("prefix(<foreign_name>)"), words("and"),
            quote("uppercase"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeAttribute = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Code for parsing foreign_enum pragmas.
%

:- pred parse_pragma_foreign_enum(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_enum(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [LangTerm, MercuryTypeTerm, ValuesTerm],
        LangContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("foreign_enum"), words("declaration:"), nl]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        TypeContextPieces = cord.from_list([words("In the second argument of"),
            pragma_decl("foreign_enum"), words("declaration:"), nl]),
        parse_type_ctor_name_arity(TypeContextPieces, VarSet,
            MercuryTypeTerm, MaybeTypeCtor0),
        (
            MaybeTypeCtor0 = ok1(TypeCtor0),
            TypeCtor0 = type_ctor(SymName0, Arity),
            ( if
                try_to_implicitly_qualify_sym_name(ModuleName,
                    SymName0, SymName)
            then
                TypeCtor1 = type_ctor(SymName, Arity),
                MaybeTypeCtor = ok1(TypeCtor1)
            else
                % Don't split "must be" across lines.
                SymNamePieces =
                    [words("Error: a"), pragma_decl("foreign_enum"),
                    words("declaration"), fixed("must be"),
                    words("for a type that is defined"),
                    words("in the same module."), nl],
                SymNameSpec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(ValuesTerm), SymNamePieces),
                MaybeTypeCtor = error1([SymNameSpec])
            )
        ;
            MaybeTypeCtor0 = error1(_),
            MaybeTypeCtor = MaybeTypeCtor0
        ),

        PairContextPieces = cord.from_list([words("In"),
            pragma_decl("foreign_enum"), words("mapping constructor name:")]),
        % XXX The following doesn't check that foreign values are sensible
        % (e.g. it should reject the empty string).
        convert_list("mapping elements",
            parse_cur_module_sym_name_string_pair(PairContextPieces,
                ModuleName),
            VarSet, ValuesTerm, MaybeValues),
        (
            MaybeValues = ok1(Values),
            (
                Values = [],
                NoValuesPieces =
                    [words("In the third argument of"),
                    pragma_decl("foreign_enum"), words("declaration:"), nl,
                    words("error: the list mapping constructors"),
                    words("to foreign values must not be empty."), nl],
                NoValuesSpec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(ValuesTerm), NoValuesPieces),
                MaybeOoMValues = error1([NoValuesSpec])
            ;
                Values = [HeadValue | TailValues],
                MaybeOoMValues = ok1(one_or_more(HeadValue, TailValues))
            )
        ;
            MaybeValues = error1(ValuesSpecs),
            MaybeOoMValues = error1(ValuesSpecs)
        ),

        ( if
            MaybeForeignLang = ok1(ForeignLang),
            MaybeTypeCtor = ok1(TypeCtor),
            MaybeOoMValues = ok1(OoMValues)
        then
            ItemForeignEnumInfo = item_foreign_enum_info(ForeignLang,
                TypeCtor, OoMValues, Context, SeqNum),
            Item = item_foreign_enum(ItemForeignEnumInfo),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++
                get_any_errors1(MaybeTypeCtor) ++
                get_any_errors1(MaybeOoMValues),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("foreign_enum"),
            words("declaration must have exactly three arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_cur_module_sym_name_string_pair(cord(format_component)::in,
    module_name::in, varset::in, term::in,
    maybe1(pair(sym_name, string))::out) is det.

parse_cur_module_sym_name_string_pair(ContextPieces, ModuleName, VarSet,
        PairTerm, MaybePair) :-
    ( if
        PairTerm = term.functor(term.atom("-"), ArgTerms, _),
        ArgTerms = [SymNameTerm, StringTerm],
        StringTerm = functor(term.string(String), _, _)
    then
        parse_sym_name_and_no_args(VarSet, ContextPieces, SymNameTerm,
            MaybeSymName),
        (
            MaybeSymName = ok1(SymName),
            (
                SymName = qualified(SymNameModuleName, _),
                ( if
                    partial_sym_name_is_part_of_full(SymNameModuleName,
                        ModuleName)
                then
                    MaybePair = ok1(SymName - String)
                else
                    Pieces = [words("Error: a function symbol name in a"),
                        pragma_decl("foreign_enum"), words("pragma"),
                        words("cannot be qualified with any module name"),
                        words("other than the name of the current module."),
                        nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(SymNameTerm), Pieces),
                    MaybePair = error1([Spec])
                )
            ;
                SymName = unqualified(_),
                MaybePair = ok1(SymName - String)
            )
        ;
            MaybeSymName = error1(Specs),
            MaybePair = error1(Specs)
        )
    else
        PairTermStr = describe_error_term(VarSet, PairTerm),
        Pieces = [words("Error: expected a mapping element"),
            words("of the form"), quote("possibly_qualified_name - string"),
            suffix(","), words("got"), quote(PairTermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(PairTerm), Pieces),
        MaybePair = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Common code for parsing foreign language interface pragmas.
%

:- pred parse_foreign_language(cord(format_component)::in, varset::in,
    term::in, maybe1(foreign_language)::out) is det.

parse_foreign_language(ContextPieces, VarSet, LangTerm, MaybeForeignLang) :-
    ( if term_to_foreign_language(LangTerm, ForeignLang) then
        MaybeForeignLang = ok1(ForeignLang)
    else
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: expected the name of a foreign language, got"),
            quote(describe_error_term(VarSet, LangTerm)), suffix("."), nl,
            words("The valid languages are")] ++
            list_to_pieces(all_foreign_language_strings) ++ [suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(LangTerm), Pieces),
        MaybeForeignLang = error1([Spec])
    ).

:- pred parse_type_ctor_name_arity(cord(format_component)::in, varset::in,
    term::in, maybe1(type_ctor)::out) is det.

parse_type_ctor_name_arity(ContextPieces, VarSet, TypeTerm,
        MaybeTypeCtor) :-
    ( if parse_unqualified_name_and_arity(TypeTerm, SymName, Arity) then
        MaybeTypeCtor = ok1(type_ctor(SymName, Arity))
    else
        TypeTermStr = describe_error_term(VarSet, TypeTerm),
        Pieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first,
            words("Error: expected"), quote("type_name/type_arity"),
            suffix(","), words("got"), quote(TypeTermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(TypeTerm), Pieces),
        MaybeTypeCtor = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Code for parsing foreign_export pragmas.
%

:- pred parse_pragma_foreign_export(varset::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_export(VarSet, ErrorTerm, PragmaTerms, Context, SeqNum,
        MaybeIOM) :-
    (
        PragmaTerms = [LangTerm, PredAndModesTerm, FunctionTerm],
        LangContextPieces =
            cord.from_list([words("In the first argument of"),
            pragma_decl("foreign_export"), words("declaration:"), nl]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        PredAndModesContextPieces =
            cord.from_list([words("In the second argument of"),
            pragma_decl("foreign_export"), words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(no, VarSet, PredAndModesContextPieces,
            PredAndModesTerm, MaybePredAndModes),
        ForeignFunctionContextPieces =
            cord.from_list([words("In the third argument of"),
            pragma_decl("foreign_export"), words("declaration:"), nl]),
        parse_foreign_function_name(VarSet, ForeignFunctionContextPieces,
            FunctionTerm, MaybeFunction),
        ( if
            MaybeForeignLang = ok1(ForeignLang),
            MaybePredAndModes = ok3(PredName, PredOrFunc, Modes),
            MaybeFunction = ok1(Function)
        then
            PredNameModesPF = pred_name_modes_pf(PredName, Modes, PredOrFunc),
            FPEInfo = pragma_info_foreign_proc_export(item_origin_user,
                ForeignLang, PredNameModesPF, Function),
            Pragma = impl_pragma_foreign_proc_export(FPEInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_impl_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++
                get_any_errors3(MaybePredAndModes) ++
                get_any_errors1(MaybeFunction),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("foreign_export"),
            words("declaration must have exactly three arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_foreign_function_name(varset::in, cord(format_component)::in,
    term::in, maybe1(string)::out) is det.

parse_foreign_function_name(VarSet, ContextPieces, FunctionTerm,
        MaybeFunction) :-
    ( if FunctionTerm = term.functor(term.string(Function), [], _) then
        ( if Function = "" then
            EmptyNamePieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first,
                words("Error: expected a non-empty string for the"),
                words("foreign language name of the exported procedure,"),
                words("got an empty string."), nl],
            FunctionSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(FunctionTerm), EmptyNamePieces),
            MaybeFunction = error1([FunctionSpec])
        else
            % XXX TODO: if we have a valid foreign language, check that
            % Function is a valid identifier in that language.
            MaybeFunction = ok1(Function)
        )
    else
        FunctionPieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first,
            words("Error: expected a non-empty string for the foreign"),
            words("language name of the exported procedure, got"),
            quote(describe_error_term(VarSet, FunctionTerm)), suffix("."), nl],
        FunctionSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree,
            get_term_context(FunctionTerm), FunctionPieces),
        MaybeFunction = error1([FunctionSpec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_foreign_import_module(varset::in, term::in,
    list(term)::in, prog_context::in, int::in, maybe1(item_or_marker)::out)
    is det.

parse_pragma_foreign_import_module(VarSet, ErrorTerm, PragmaTerms, Context,
        SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [LangTerm, ModuleNameTerm],
        LangContextPieces =
            cord.from_list([words("In the first argument of"),
            pragma_decl("foreign_import_module"), words("declaration:"), nl]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        ( if try_parse_sym_name_and_no_args(ModuleNameTerm, ModuleName0) then
            MaybeModuleName = ok1(ModuleName0)
        else
            ModuleNameTermStr = describe_error_term(VarSet, ModuleNameTerm),
            ModuleNamePieces = [words("In the second argument of"),
                pragma_decl("foreign_import_module"),
                words("declaration:"), nl,
                words("error: expected module name, got"),
                quote(ModuleNameTermStr), suffix("."), nl],
            ModuleNameSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(ModuleNameTerm), ModuleNamePieces),
            MaybeModuleName = error1([ModuleNameSpec])
        ),
        ( if
            MaybeForeignLang = ok1(Language),
            MaybeModuleName = ok1(ModuleName)
        then
            FIM = item_fim(Language, ModuleName, Context, SeqNum),
            MaybeIOM = ok1(iom_marker_fim(FIM))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++
                get_any_errors1(MaybeModuleName),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("foreign_import_module"),
            words("declaration must have two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_external_proc(module_name::in, varset::in, term::in,
    string::in, list(term)::in, prog_context::in, int::in,
    pred_or_func::in, maybe1(item_or_marker)::out) is det.

parse_pragma_external_proc(ModuleName, VarSet, ErrorTerm,
        PragmaName, PragmaTerms, Context, SeqNum, PorF, MaybeIOM) :-
    ( if
        (
            PragmaTerms = [PredTerm],
            MaybeOptionsTerm = no
        ;
            PragmaTerms = [PredTerm, OptionsTerm],
            MaybeOptionsTerm = yes(OptionsTerm)
        )
    then
        ContextPieces1 = cord.from_list([words("first argument of"),
            pragma_decl(PragmaName), words("declaration")]),
        parse_symname_arity(VarSet, PredTerm, ContextPieces1,
            MaybeSymNameArity),
        ContextPieces2 = cord.from_list([words("second argument of"),
            pragma_decl(PragmaName), words("declaration")]),
        parse_pragma_external_options(VarSet, MaybeOptionsTerm, ContextPieces2,
            MaybeMaybeBackend),
        ( if
            MaybeSymNameArity = ok2(SymName, Arity),
            MaybeMaybeBackend = ok1(MaybeBackend)
        then
            BaseName = unqualify_name(SymName),
            FullSymName = qualified(ModuleName, BaseName),
            ( if partial_sym_name_is_part_of_full(SymName, FullSymName) then
                ExternalInfo = pragma_info_external_proc(FullSymName, Arity,
                    PorF, MaybeBackend),
                Pragma = impl_pragma_external_proc(ExternalInfo),
                PragmaInfo = item_pragma_info(Pragma, Context, SeqNum),
                Item = item_impl_pragma(PragmaInfo),
                MaybeIOM = ok1(iom_item(Item))
            else
                Pieces = [words("Error: the predicate name in the")] ++
                    cord.list(ContextPieces1) ++
                    [words("is not for the expected module, which is"),
                    qual_sym_name(ModuleName), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(ErrorTerm), Pieces),
                MaybeIOM = error1([Spec])
            )
        else
            Specs = get_any_errors2(MaybeSymNameArity)
                ++ get_any_errors1(MaybeMaybeBackend),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl(PragmaName),
            words("declaration must have one or two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_symname_arity(varset::in, term::in, cord(format_component)::in,
    maybe2(sym_name, arity)::out) is det.

parse_symname_arity(VarSet, PredTerm, ContextPieces, MaybeSymNameArity) :-
    ( if PredTerm = term.functor(term.atom("/"), [NameTerm, ArityTerm], _) then
        parse_symbol_name(VarSet, NameTerm, MaybeSymName),
        ( if decimal_term_to_int(ArityTerm, ArityPrime) then
            MaybeArity = ok1(ArityPrime)
        else
            ArityPieces = [words("Error: in")] ++ cord.list(ContextPieces) ++
                [suffix(":"), words("the arity must be an integer."), nl],
            AritySpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(ArityTerm), ArityPieces),
            MaybeArity = error1([AritySpec])
        ),
        ( if
            MaybeSymName = ok1(SymName),
            MaybeArity = ok1(Arity)
        then
            MaybeSymNameArity = ok2(SymName, Arity)
        else
            Specs = get_any_errors1(MaybeSymName)
                ++ get_any_errors1(MaybeArity),
            MaybeSymNameArity = error2(Specs)
        )
    else
        Pieces = [words("Error:") | cord.list(ContextPieces)] ++
            [words("should be Name/Arity."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(PredTerm), Pieces),
        MaybeSymNameArity = error2([Spec])
    ).

:- pred parse_pragma_external_options(varset::in, maybe(term)::in,
    cord(format_component)::in, maybe1(maybe(backend))::out) is det.

parse_pragma_external_options(VarSet, MaybeOptionsTerm, ContextPieces,
        MaybeMaybeBackend) :-
    (
        MaybeOptionsTerm = no,
        MaybeMaybeBackend = ok1(no)
    ;
        MaybeOptionsTerm = yes(OptionsTerm),
        ( if
            OptionsTerm = term.functor(term.atom("[]"), [], _)
        then
            MaybeMaybeBackend = ok1(no)
        else if
            OptionsTerm = term.functor(term.atom("[|]"),
                [OptionsTermHead, OptionsTermTail], _),
            (
                OptionsTermHead =
                    term.functor(term.atom("low_level_backend"), [], _),
                Backend = low_level_backend
            ;
                OptionsTermHead =
                    term.functor(term.atom("high_level_backend"), [], _),
                Backend = high_level_backend
            ),
            OptionsTermTail = term.functor(term.atom("[]"), [], _)
        then
            MaybeMaybeBackend = ok1(yes(Backend))
        else
            OptionsTermStr = describe_error_term(VarSet, OptionsTerm),
            Pieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first, words("Error:"),
                words("expected either an empty list,"),
                words("or a singleton list containing either"),
                quote("low_level_backend"), words("or"),
                quote("high_level_backend"), suffix(","),
                words("got"), words(OptionsTermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(OptionsTerm), Pieces),
            MaybeMaybeBackend = error1([Spec])
        )
    ).

%---------------------------------------------------------------------------%
%
% Parse the arguments of an "obsolete" or "obsolete_proc" pragma.
%

:- pred parse_pragma_obsolete_pred(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_obsolete_pred(ModuleName, PragmaTerms, ErrorTerm, VarSet,
        Context, SeqNum, MaybeIOM) :-
    (
        (
            PragmaTerms = [NameAndArityTerm],
            MaybeObsoleteInFavourOf = ok1([])
        ;
            PragmaTerms = [NameAndArityTerm, ObsoleteInFavourOfTerm],
            parse_pragma_obsolete_in_favour_of(ObsoleteInFavourOfTerm,
                VarSet, MaybeObsoleteInFavourOf)
        ),
        parse_simple_name_and_arity(ModuleName, "obsolete",
            "predicate or function", NameAndArityTerm, NameAndArityTerm,
            VarSet, MaybeNameAndArity),
        ( if
            MaybeNameAndArity = ok2(PredName, PredArity),
            MaybeObsoleteInFavourOf = ok1(ObsoleteInFavourOf)
        then
            PredNameArity = pred_name_arity(PredName, PredArity),
            ObsoletePragma =
                pragma_info_obsolete_pred(PredNameArity, ObsoleteInFavourOf),
            Pragma = decl_pragma_obsolete_pred(ObsoletePragma),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_decl_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs =
                get_any_errors2(MaybeNameAndArity) ++
                get_any_errors1(MaybeObsoleteInFavourOf),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: an"), pragma_decl("obsolete"),
            words("declaration must have one or two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

:- pred parse_pragma_obsolete_proc(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_obsolete_proc(ModuleName, PragmaTerms, ErrorTerm, VarSet,
        Context, SeqNum, MaybeIOM) :-
    (
        (
            PragmaTerms = [PredAndModesTerm],
            MaybeObsoleteInFavourOf = ok1([])
        ;
            PragmaTerms = [PredAndModesTerm, ObsoleteInFavourOfTerm],
            parse_pragma_obsolete_in_favour_of(ObsoleteInFavourOfTerm,
                VarSet, MaybeObsoleteInFavourOf)
        ),
        PredAndModesContextPieces = cord.from_list(
            [words("In the first  argument of"), pragma_decl("obsolete_proc"),
            words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), VarSet,
            PredAndModesContextPieces, PredAndModesTerm, MaybePredAndModes),
        ( if
            MaybePredAndModes = ok3(PredName, PredOrFunc, Modes),
            MaybeObsoleteInFavourOf = ok1(ObsoleteInFavourOf)
        then
            PredNameModesPF = pred_name_modes_pf(PredName, Modes, PredOrFunc),
            ObsoletePragma =
                pragma_info_obsolete_proc(PredNameModesPF, ObsoleteInFavourOf),
            Pragma = decl_pragma_obsolete_proc(ObsoletePragma),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_decl_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs =
                get_any_errors3(MaybePredAndModes) ++
                get_any_errors1(MaybeObsoleteInFavourOf),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: an"), pragma_decl("obsolete_proc"),
            words("declaration must have one or two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

:- pred parse_pragma_obsolete_in_favour_of(term::in, varset::in,
    maybe1(list(sym_name_arity))::out) is det.

parse_pragma_obsolete_in_favour_of(Term, VarSet, MaybeObsoleteInFavourOf) :-
    ( if list_term_to_term_list(Term, Terms) then
        parse_pragma_obsolete_in_favour_of_snas(1, Terms, VarSet,
            MaybeObsoleteInFavourOf)
    else
        Pieces = [words("Error: the second argument of a"),
            pragma_decl("obsolete"), words("declaration"),
            words("should be a list of the names and arities of the"),
            words("suggested replacement predicates and/or functions."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeObsoleteInFavourOf = error1([Spec])
    ).

:- pred parse_pragma_obsolete_in_favour_of_snas(int::in, list(term)::in,
    varset::in, maybe1(list(sym_name_arity))::out) is det.

parse_pragma_obsolete_in_favour_of_snas(_ArgNum, [], _VarSet, ok1([])).
parse_pragma_obsolete_in_favour_of_snas(ArgNum, [Term | Terms], VarSet,
        MaybeSNAs) :-
    ( if parse_unqualified_name_and_arity(Term, SymName, Arity) then
        MaybeHeadSNA = ok1(sym_name_arity(SymName, Arity))
    else
        Pieces = [words("In the"), nth_fixed(ArgNum),
            words("element in the second argument of"),
            pragma_decl("obsolete"), words("declaration:"), nl,
            words("error: expected a name/arity pair, got"),
            quote(describe_error_term(VarSet, Term)), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeHeadSNA = error1([Spec])
    ),
    parse_pragma_obsolete_in_favour_of_snas(ArgNum + 1, Terms, VarSet,
        MaybeTailSNAs),
    ( if
        MaybeHeadSNA = ok1(HeadSNA),
        MaybeTailSNAs = ok1(TailSNAs)
    then
        MaybeSNAs = ok1([HeadSNA | TailSNAs])
    else
        Specs =
            get_any_errors1(MaybeHeadSNA) ++
            get_any_errors1(MaybeTailSNAs),
        MaybeSNAs = error1(Specs)
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_require_tail_recursion(module_name::in, list(term)::in,
    term::in, varset::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_require_tail_recursion(ModuleName, PragmaTerms, _ErrorTerm,
        VarSet, Context, SeqNum, MaybeIOM) :-
    PragmaName = "require_tail_recursion",
    ( if
        (
            PragmaTerms = [PredAndModesTerm, OptionsTermPrime],
            MaybeOptionsTerm = yes(OptionsTermPrime)
        ;
            PragmaTerms = [PredAndModesTerm],
            MaybeOptionsTerm = no
        )
    then
        % Parse the procedure name.
        ContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl(PragmaName), words("declaration:"), nl]),
        parse_arity_or_modes(ModuleName, PredAndModesTerm,
            PredAndModesTerm, VarSet, ContextPieces, MaybeProc),

        % Parse the options
        (
            MaybeOptionsTerm = yes(OptionsTerm),
            ( if list_term_to_term_list(OptionsTerm, OptionsTerms) then
                parse_pragma_require_tail_recursion_options(OptionsTerms,
                    have_not_seen_none, no, no, [], Context, MaybeOptions)
            else
                OptionsContext = get_term_context(OptionsTerm),
                OptionsTermStr = describe_error_term(VarSet, OptionsTerm),
                Pieces = [words("In the second argument of"),
                    pragma_decl("require_tail_recursion"),
                    words("declaration:"), nl,
                    words("error: expected attribute list, got"),
                    quote(OptionsTermStr), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, OptionsContext, Pieces),
                MaybeOptions = error1([Spec])
            )
        ;
            MaybeOptionsTerm = no,
            MaybeOptions = ok1(enable_tailrec_warnings(we_warning,
                both_self_and_mutual_recursion_must_be_tail, Context))
        ),

        % Put them together.
        (
            MaybeProc = ok1(Proc),
            (
                MaybeOptions = ok1(RequireTailrecInfo),
                PragmaType = impl_pragma_require_tail_rec(
                    pragma_info_require_tail_rec(Proc, RequireTailrecInfo)),
                PragmaInfo = item_pragma_info(PragmaType, Context, SeqNum),
                MaybeIOM = ok1(iom_item(item_impl_pragma(PragmaInfo)))
            ;
                MaybeOptions = error1(Errors),
                MaybeIOM = error1(Errors)
            )
        ;
            MaybeProc = error1(ProcErrors),
            (
                MaybeOptions = ok1(_),
                MaybeIOM = error1(ProcErrors)
            ;
                MaybeOptions = error1(OptionsErrors),
                MaybeIOM = error1(ProcErrors ++ OptionsErrors)
            )
        )
    else
        Pieces = [words("Error: a"), pragma_decl(PragmaName),
            words("declaration must have one or two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- type seen_none
    --->    seen_none
    ;       have_not_seen_none.

:- pred parse_pragma_require_tail_recursion_options(list(term)::in,
    seen_none::in, maybe(warning_or_error)::in,
    maybe(require_tail_recursion_type)::in, list(error_spec)::in,
    prog_context::in, maybe1(require_tail_recursion)::out) is det.

parse_pragma_require_tail_recursion_options([], SeenNone, MaybeWarnOrError,
        MaybeType, !.Specs, Context, MaybeRTR) :-
    (
        SeenNone = seen_none,
        % Check for conflicts with "none" option.
        (
            MaybeWarnOrError = yes(WarnOrError0),
            warning_or_error_string(WarnOrError0, WarnOrErrorString),
            SpecA = conflicting_attributes_error("none", WarnOrErrorString,
                Context),
            !:Specs = [SpecA | !.Specs]
        ;
            MaybeWarnOrError = no
        ),
        (
            MaybeType = yes(Type0),
            require_tailrec_type_string(Type0, TypeString),
            SpecB = conflicting_attributes_error("none", TypeString,
                Context),
            !:Specs = [SpecB | !.Specs]
        ;
            MaybeType = no
        )
    ;
        SeenNone = have_not_seen_none
    ),
    (
        !.Specs = [_ | _],
        MaybeRTR = error1(!.Specs)
    ;
        !.Specs = [],
        (
            SeenNone = seen_none,
            MaybeRTR = ok1(suppress_tailrec_warnings(Context))
        ;
            SeenNone = have_not_seen_none,
            % If these values were not set then use the defaults.
            (
                MaybeWarnOrError = yes(WarnOrError)
            ;
                MaybeWarnOrError = no,
                WarnOrError = we_warning
            ),
            (
                MaybeType = yes(Type)
            ;
                MaybeType = no,
                Type = both_self_and_mutual_recursion_must_be_tail
            ),
            MaybeRTR = ok1(enable_tailrec_warnings(WarnOrError, Type,
                Context))
        )
    ).
parse_pragma_require_tail_recursion_options([Term | Terms], SeenNone0,
        MaybeWarnOrError0, MaybeType0, !.Specs, PragmaContext, MaybeRTR) :-
    (
        Term = functor(Functor, _Args, Context),
        ( if
            Functor = atom(Name),
            warning_or_error_string(WarnOrError, Name)
        then
            (
                MaybeWarnOrError0 = no,
                MaybeWarnOrError = yes(WarnOrError)
            ;
                MaybeWarnOrError0 = yes(WarnOrErrorFirst),
                warning_or_error_string(WarnOrErrorFirst,
                    WarnOrErrorFirstString),
                Spec = conflicting_attributes_error(Name,
                    WarnOrErrorFirstString, Context),
                MaybeWarnOrError = MaybeWarnOrError0,
                !:Specs = [Spec | !.Specs]
            ),
            MaybeType = MaybeType0,
            SeenNone = SeenNone0
        else if
            Functor = atom(Name),
            require_tailrec_type_string(Type, Name)
        then
            (
                MaybeType0 = no,
                MaybeType = yes(Type)
            ;
                MaybeType0 = yes(TypeFirst),
                require_tailrec_type_string(TypeFirst, TypeFirstString),
                Spec = conflicting_attributes_error(Name,
                    TypeFirstString, Context),
                MaybeType = MaybeType0,
                !:Specs = [Spec | !.Specs]
            ),
            MaybeWarnOrError = MaybeWarnOrError0,
            SeenNone = SeenNone0
        else if
            Functor = atom("none")
        then
            SeenNone = seen_none,
            MaybeWarnOrError = MaybeWarnOrError0,
            MaybeType = MaybeType0
        else
            Spec = pragma_require_tailrec_unknown_term_error(Term, Context),
            !:Specs = [Spec | !.Specs],
            SeenNone = SeenNone0,
            MaybeType = MaybeType0,
            MaybeWarnOrError = MaybeWarnOrError0
        )
    ;
        Term = variable(_, Context),
        Spec = pragma_require_tailrec_unknown_term_error(Term, Context),
        !:Specs = [Spec | !.Specs],
        SeenNone = SeenNone0,
        MaybeType = MaybeType0,
        MaybeWarnOrError = MaybeWarnOrError0
    ),
    parse_pragma_require_tail_recursion_options(Terms, SeenNone,
        MaybeWarnOrError, MaybeType, !.Specs, PragmaContext, MaybeRTR).

:- func conflicting_attributes_error(string, string, prog_context) =
    error_spec.

conflicting_attributes_error(ThisName, EarlierName, Context) = Spec :-
    Pieces = [words("Error: conflicting "),
        pragma_decl("require_tail_recursion"), words("attributes: "),
        quote(ThisName), words("conflicts with earlier attribute"),
        quote(EarlierName), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        Context, Pieces).

:- func pragma_require_tailrec_unknown_term_error(term, prog_context) =
    error_spec.

pragma_require_tailrec_unknown_term_error(Term, Context) = Spec :-
    varset.init(VarSet),
    Pieces = [words("Error: unrecognised "),
        pragma_decl("require_tail_recursion"), words("attribute: "),
        quote(describe_error_term(VarSet, Term)), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        Context, Pieces).

%---------------------------------------------------------------------------%

:- pred parse_pragma_unused_args(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

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
            PredNameTerm, VarSet, PredNameContextPieces, MaybePredName),
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
        convert_list("a list of integers",
            parse_decimal_int(UnusedArgsContextPieces),
            VarSet, UnusedArgsTerm, MaybeUnusedArgs),
        ( if
            MaybePredOrFunc = ok1(PredOrFunc),
            MaybePredName = ok1(PredName),
            MaybeArity = ok1(Arity),
            MaybeModeNum = ok1(ModeNum),
            MaybeUnusedArgs = ok1(UnusedArgs)
        then
            PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity,
                PredOrFunc, ModeNum),
            UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn,
                UnusedArgs),
            Pragma = gen_pragma_unused_args(UnusedArgsInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_generated_pragma(ItemPragma),
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

:- pred parse_pragma_type_spec(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_type_spec(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
        (
            PragmaTerms = [PredAndModesTerm, TypeSubnTerm],
            MaybeName = no
        ;
            PragmaTerms = [PredAndModesTerm, TypeSubnTerm, SpecNameTerm],

            % This form of the pragma should not appear in source files.
            SpecNameTerm = term.functor(_, _, SpecContext),
            term.context_file(SpecContext, FileName),
            not string.remove_suffix(FileName, ".m", _),

            try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
                SpecNameTerm, SpecName),
            MaybeName = yes(SpecName)
        )
    then
        ArityOrModesContextPieces = cord.from_list(
            [words("In the first argument"), pragma_decl("type_spec"),
            words("declaration:"), nl]),
        parse_arity_or_modes(ModuleName, PredAndModesTerm, ErrorTerm,
            VarSet, ArityOrModesContextPieces, MaybeArityOrModes),
        (
            MaybeArityOrModes = ok1(ArityOrModes),
            ArityOrModes = pred_name_arity_mpf_mmode(PredName, Arity,
                MaybePredOrFunc, MaybeModes),
            conjunction_to_list(TypeSubnTerm, TypeSubnTerms),

            % The varset is actually a tvarset.
            varset.coerce(VarSet, TVarSet),
            ( if
                list.map(convert_type_spec_pair, TypeSubnTerms, TypeSubns)
            then
                (
                    MaybeName = yes(SpecializedName0),
                    SpecializedName = SpecializedName0
                ;
                    MaybeName = no,
                    UnqualName = unqualify_name(PredName),
                    make_pred_name(ModuleName, "TypeSpecOf", MaybePredOrFunc,
                        UnqualName, newpred_type_subst(TVarSet, TypeSubns),
                        SpecializedName)
                ),
                TypeSpecInfo = pragma_info_type_spec(PredName, SpecializedName,
                    Arity, MaybePredOrFunc, MaybeModes, TypeSubns, TVarSet,
                    set.init),
                Pragma = decl_pragma_type_spec(TypeSpecInfo),
                ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
                Item = item_decl_pragma(ItemPragma),
                MaybeIOM = ok1(iom_item(Item))
            else
                TypeSubnTermStr = describe_error_term(VarSet, TypeSubnTerm),
                Pieces = [words("In the second argument of"),
                    pragma_decl("type_spec"), words("declaration:"), nl,
                    words("error: expected a type substitution, got"),
                    quote(TypeSubnTermStr), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(TypeSubnTerm), Pieces),
                MaybeIOM = error1([Spec])
            )
        ;
            MaybeArityOrModes = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl("type_spec"),
            words("declaration must have two or three arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_fact_table(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_fact_table(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredAndArityTerm, FileNameTerm],
        parse_pred_name_and_arity(ModuleName, "fact_table",
            PredAndArityTerm, ErrorTerm, VarSet, MaybeNameAndArity),
        (
            MaybeNameAndArity = ok2(PredName, Arity),
            ( if FileNameTerm = term.functor(term.string(FileName), [], _) then
                PredNameArity = pred_name_arity(PredName, Arity),
                FactTableInfo = pragma_info_fact_table(PredNameArity,
                    FileName),
                Pragma = impl_pragma_fact_table(FactTableInfo),
                ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
                Item = item_impl_pragma(ItemPragma),
                MaybeIOM = ok1(iom_item(Item))
            else
                FileNameTermStr = describe_error_term(VarSet, FileNameTerm),
                Pieces = [words("In the second argument of"),
                    pragma_decl("fact_table"), words("declaration:"), nl,
                    words("error: expected a string specifying a filename,"),
                    words("got"), quote(FileNameTermStr), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(FileNameTerm), Pieces),
                MaybeIOM = error1([Spec])
            )
        ;
            MaybeNameAndArity = error2(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("fact_table"),
            words("declaration must have two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_termination_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_termination_info(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredAndModesTerm0, ArgSizeTerm, TerminationTerm],
        PAMContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("termination_info"), words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), VarSet,
            PAMContextPieces, PredAndModesTerm0, MaybeNameAndModes),
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
                    convert_list("a list of booleans",
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
            MaybeNameAndModes = ok3(PredName, PredOrFunc, ModeList),
            MaybeMaybeArgSizeInfo = ok1(MaybeArgSizeInfo),
            MaybeMaybeTerminationInfo = ok1(MaybeTerminationInfo)
        then
            PredNameModesPF = pred_name_modes_pf(PredName, ModeList,
                PredOrFunc),
            TermInfo = pragma_info_termination_info(PredNameModesPF,
                MaybeArgSizeInfo, MaybeTerminationInfo),
            Pragma = decl_pragma_termination_info(TermInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_decl_pragma(ItemPragma),
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

%---------------------------------------------------------------------------%

:- pred parse_pragma_termination2_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_termination2_info(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredAndModesTerm0, SuccessArgSizeTerm,
            FailureArgSizeTerm, TerminationTerm],
        PAMContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("termination2_info"), words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), VarSet,
            PAMContextPieces, PredAndModesTerm0, MaybeNameAndModes),
        parse_arg_size_constraints(VarSet, SuccessArgSizeTerm,
            MaybeSuccessArgSize),
        parse_arg_size_constraints(VarSet, FailureArgSizeTerm,
            MaybeFailureArgSize),
        TIContextPieces = [words("In the fourth argument of"),
            pragma_decl("termination2_info"), words("declaration:"), nl],
        parse_termination_info(TIContextPieces, VarSet, TerminationTerm,
            MaybeMaybeTerminationInfo),
        ( if
            MaybeNameAndModes = ok3(PredName, PredOrFunc, ModeList),
            MaybeSuccessArgSize = ok1(SuccessArgSizeInfo),
            MaybeFailureArgSize = ok1(FailureArgSizeInfo),
            MaybeMaybeTerminationInfo = ok1(MaybeTerminationInfo)
        then
            PredNameModesPF = pred_name_modes_pf(PredName, ModeList,
                PredOrFunc),
            Term2Info = pragma_info_termination2_info(PredNameModesPF,
                SuccessArgSizeInfo, FailureArgSizeInfo, MaybeTerminationInfo),
            Pragma = decl_pragma_termination2_info(Term2Info),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_decl_pragma(ItemPragma),
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

:- pred parse_termination_info(list(format_component)::in, varset::in,
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

%---------------------------------------------------------------------------%

:- pred parse_pragma_structure_sharing(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_structure_sharing(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [PredAndModesTerm0, HeadVarsTerm,
            HeadVarTypesTerm, SharingInformationTerm],
        ModesContextPieces = cord.from_list([words("In"),
            pragma_decl("structure_sharing"), words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), VarSet,
            ModesContextPieces, PredAndModesTerm0, MaybeNameAndModes),
        MaybeNameAndModes = ok3(PredName, PredOrFunc, ModeList),

        % Parse the head variables:
        HeadVarsTerm = term.functor(term.atom("vars"), ListHVTerm, _),
        term.vars_list(ListHVTerm, HeadVarsGeneric),
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
        PredNameModesPF = pred_name_modes_pf(PredName, ModeList, PredOrFunc),
        varset.coerce(VarSet, ProgVarSet),
        varset.coerce(VarSet, TVarSet),
        SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
            HeadVars, Types, ProgVarSet, TVarSet, MaybeSharingAs),
        Pragma = decl_pragma_structure_sharing(SharingInfo),
        ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
        Item = item_decl_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Syntax error in"),
            pragma_decl("structure_sharing"), words("declaration."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_structure_reuse(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_structure_reuse(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [PredAndModesTerm0, HeadVarsTerm,
            HeadVarTypesTerm, MaybeStructureReuseTerm],
        ReuseContextPieces = cord.from_list([words("In"),
            pragma_decl("structure_reuse"), words("declaration:"), nl]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), VarSet,
            ReuseContextPieces, PredAndModesTerm0, MaybeNameAndModes),
        MaybeNameAndModes = ok3(PredName, PredOrFunc, ModeList),

        % Parse the head variables:
        HeadVarsTerm = term.functor(term.atom("vars"), ListHVTerm, _),
        term.vars_list(ListHVTerm, HeadVarsGeneric),
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
        PredNameModesPF = pred_name_modes_pf(PredName, ModeList, PredOrFunc),
        varset.coerce(VarSet, ProgVarSet),
        varset.coerce(VarSet, TVarSet),
        ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
            HeadVars, Types, ProgVarSet, TVarSet, MaybeStructureReuse),
        Pragma = decl_pragma_structure_reuse(ReuseInfo),
        ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
        Item = item_decl_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Syntax error in"),
            pragma_decl("structure_reuse"), words("declaration."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_exceptions(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

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
            PredNameTerm, VarSet, PNContextPieces, MaybePredName),
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
            PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity,
                PredOrFunc, ModeNum),
            ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn,
                ThrowStatus),
            Pragma = gen_pragma_exceptions(ExceptionsInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_generated_pragma(ItemPragma),
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

:- pred parse_pragma_trailing_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

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
            PredNameTerm, VarSet, PNContextPieces, MaybePredName),
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
            PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity,
                PredOrFunc, ModeNum),
            TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn,
                TrailingStatus),
            Pragma = gen_pragma_trailing_info(TrailingInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_generated_pragma(ItemPragma),
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

:- pred parse_pragma_mm_tabling_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

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
            PredNameTerm, VarSet, PNContextPieces, MaybePredName),
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
            PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity,
                PredOrFunc, ModeNum),
            TablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
                MMTablingStatus),
            Pragma = gen_pragma_mm_tabling_info(TablingInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_generated_pragma(ItemPragma),
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

:- pred parse_pragma_require_feature_set(varset::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_require_feature_set(VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [FeatureListTerm],
        convert_list("a list of features", parse_required_feature,
            VarSet, FeatureListTerm, MaybeFeatureList),
        (
            MaybeFeatureList = ok1(FeatureList),
            ConflictingFeatures = [
                conflict(reqf_single_prec_float, reqf_double_prec_float,
                    "floats cannot be both single- and double-precision"),
                conflict(reqf_parallel_conj, reqf_trailing,
                    "trailing works only with sequential conjunctions")
            ],
            FeatureListContext = get_term_context(FeatureListTerm),
            report_any_conflicts(FeatureListContext,
                "conflicting features in feature set",
                ConflictingFeatures, FeatureList, ConflictSpecs),
            (
                ConflictSpecs = [_ | _],
                MaybeIOM = error1(ConflictSpecs)
            ;
                ConflictSpecs = [],
                (
                    FeatureList = [],
                    MaybeIOM = ok1(iom_handled([]))
                ;
                    FeatureList = [_ | _],
                    FeatureSet = set.list_to_set(FeatureList),
                    RFSInfo = pragma_info_require_feature_set(FeatureSet),
                    Pragma = impl_pragma_require_feature_set(RFSInfo),
                    ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
                    Item = item_impl_pragma(ItemPragma),
                    MaybeIOM = ok1(iom_item(Item))
                )
            )
        ;
            MaybeFeatureList = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("require_feature_set"),
            words("declaration must have exactly one argument."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_foreign_decl_is_local(term::in, foreign_decl_is_local::out)
    is semidet.

parse_foreign_decl_is_local(term.functor(Functor, [], _), IsLocal) :-
    ( Functor = term.string(String)
    ; Functor = term.atom(String)
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
        LiteralOrInclude = floi_literal(Code)
    ;
        Functor = term.atom("include_file"),
        Args = [term.functor(term.string(FileName), [], _)],
        LiteralOrInclude = floi_include_file(FileName)
    ).

term_to_foreign_language(term.functor(term.string(String), _, _), Lang) :-
    globals.convert_foreign_language(String, Lang).
term_to_foreign_language(term.functor(term.atom(String), _, _), Lang) :-
    globals.convert_foreign_language(String, Lang).

:- pred parse_foreign_language_type(cord(format_component)::in, term::in,
    varset::in, maybe1(foreign_language)::in,
    maybe1(generic_language_foreign_type)::out) is det.

parse_foreign_language_type(ContextPieces, InputTerm, VarSet, MaybeLanguage,
        MaybeForeignLangType) :-
    ( if InputTerm = term.functor(term.string(ForeignTypeName), [], _) then
        (
            MaybeLanguage = ok1(Language),
            (
                (
                    Language = lang_c,
                    ForeignLangType = c(c_type(ForeignTypeName))
                ;
                    Language = lang_java,
                    ForeignLangType = java(java_type(ForeignTypeName))
                ;
                    Language = lang_csharp,
                    ForeignLangType = csharp(csharp_type(ForeignTypeName))
                ),
                ( if ForeignTypeName = "" then
                    Pieces = cord.list(ContextPieces) ++
                        [lower_case_next_if_not_first,
                        words("Error: foreign type descriptor for language"),
                        quote(foreign_language_string(Language)),
                        words("must be a non-empty string."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(InputTerm), Pieces),
                    MaybeForeignLangType = error1([Spec])
                else
                    MaybeForeignLangType = ok1(ForeignLangType)
                )
            ;
                Language = lang_erlang,
                ( if ForeignTypeName = "" then
                    MaybeForeignLangType = ok1(erlang(erlang_type))
                else
                    Pieces = cord.list(ContextPieces) ++
                        [lower_case_next_if_not_first,
                        words("Error: foreign type descriptor for language"),
                        quote(foreign_language_string(Language)),
                        words("must be an empty string."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(InputTerm), Pieces),
                    MaybeForeignLangType = error1([Spec])
                )
            )
        ;
            % NOTE: if we get here then MaybeFooreignLang will be an error and
            % will give the user the required error message.
            MaybeLanguage = error1(_),
            MaybeForeignLangType = error1([])   % Dummy value.
        )
    else
        InputTermStr = describe_error_term(VarSet, InputTerm),
        Pieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first, words("Error: expected a string"),
            words("specifying the foreign type descriptor,"),
            words("got"), quote(InputTermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(InputTerm), Pieces),
        MaybeForeignLangType = error1([Spec])
    ).

    % This predicate parses foreign_decl pragmas.
    %
:- pred parse_pragma_foreign_decl(varset::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_decl(VarSet, ErrorTerm, PragmaTerms, Context, SeqNum,
        MaybeIOM) :-
    InvalidDeclPieces = [words("Error: invalid"),
        pragma_decl("foreign_decl"), words("declaration:"), nl],
    (
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ),
        Pieces = [words("Error: a"), pragma_decl("foreign_decl"),
            words("declaration requires at least two arguments"),
            words("(a language specification and"),
            words("the foreign language declaration itself)."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ;
        (
            PragmaTerms = [LangTerm, HeaderTerm],
            IsLocal = foreign_decl_is_exported,
            IsLocalSpecs = []
        ;
            PragmaTerms = [LangTerm, IsLocalTerm, HeaderTerm],
            ( if parse_foreign_decl_is_local(IsLocalTerm, IsLocalPrime) then
                IsLocal = IsLocalPrime,
                IsLocalSpecs = []
            else
                IsLocal = foreign_decl_is_exported, % Dummy, won't be used.
                IsLocalStr = describe_error_term(VarSet, IsLocalTerm),
                IsLocalPieces = [words("Error: the second argument"),
                    words("of a"), pragma_decl("foreign_decl"),
                    words("declaration must be either"), quote("local"),
                    words("or"), quote("exported"), suffix(":"),
                    words("got"), quote(IsLocalStr), suffix("."), nl],
                IsLocalSpec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(IsLocalTerm), IsLocalPieces),
                IsLocalSpecs = [IsLocalSpec]
            )
        ),
        ( if term_to_foreign_language(LangTerm, ForeignLanguagePrime) then
            ForeignLanguage = ForeignLanguagePrime,
            LangSpecs = []
        else
            ForeignLanguage = lang_c,   % Dummy, won't be used.
            LangStr = describe_error_term(VarSet, LangTerm),
            LangPieces = InvalidDeclPieces ++
                [words("invalid language parameter"),
                quote(LangStr), suffix("."), nl],
            LangSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(LangTerm), LangPieces),
            LangSpecs = [LangSpec]
        ),
        ( if parse_foreign_literal_or_include(HeaderTerm, LitOrInclPrime) then
            LiteralOrInclude = LitOrInclPrime,
            LitInclSpecs = []
        else
            LiteralOrInclude = floi_literal(""),    % Dummy, won't be used.
            LitInclStr = describe_error_term(VarSet, HeaderTerm),
            LitInclPieces = InvalidDeclPieces ++
                [words("expected string or include_file for"),
                words("foreign declaration code, got"),
                quote(LitInclStr), suffix("."), nl],
            LitInclSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(HeaderTerm), LitInclPieces),
            LitInclSpecs = [LitInclSpec]
        ),
        ( if
            IsLocalSpecs = [],
            LangSpecs = [],
            LitInclSpecs = []
        then
            FDInfo = pragma_info_foreign_decl(ForeignLanguage, IsLocal,
                LiteralOrInclude),
            Pragma = impl_pragma_foreign_decl(FDInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_impl_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            MaybeIOM = error1(IsLocalSpecs ++ LangSpecs ++ LitInclSpecs)
        )
    ;
        PragmaTerms = [_, _, _, _ | _],
        Pieces = [words("Error: a"), pragma_decl("foreign_decl"),
            words("declaration may have at most three arguments"),
            words("(a language specification,"),
            words("a local/exported indication, and"),
            words("the foreign language declaration itself)."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

    % This predicate parses foreign_code pragmas.
    % Processing of foreign_proc pragmas is handled in
    % parse_pragma_foreign_proc below.
    %
:- pred parse_pragma_foreign_code(varset::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_code(VarSet, ErrorTerm, PragmaTerms, Context, SeqNum,
        MaybeIOM) :-
    (
        PragmaTerms = [LangTerm, CodeTerm],
        ContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("foreign_code"), words("declaration:"), nl]),
        parse_foreign_language(ContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        ( if parse_foreign_literal_or_include(CodeTerm, CodePrime) then
            Code = CodePrime,
            CodeSpecs = []
        else
            Code = floi_literal(""), % Dummy, ignored when CodeSpecs \= []
            CodeTermStr = describe_error_term(VarSet, CodeTerm),
            CodePieces = [words("In the second argument of"),
                pragma_decl("foreign_code"), words("declaration:"), nl,
                words("error: expected a string containing foreign code,"),
                words("got"), quote(CodeTermStr), suffix("."), nl],
            CodeSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(CodeTerm), CodePieces),
            CodeSpecs = [CodeSpec]
        ),
        ( if
            MaybeForeignLang = ok1(ForeignLanguage),
            CodeSpecs = []
        then
            FCInfo = pragma_info_foreign_code(ForeignLanguage, Code),
            Pragma = impl_pragma_foreign_code(FCInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_impl_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++ CodeSpecs,
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("foreign_code"),
            words("declaration must have exactly two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

    % This predicate parses foreign_proc pragmas.
    %
:- pred parse_pragma_foreign_proc(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_proc(ModuleName, VarSet, ErrorTerm, PragmaTerms, Context,
        SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [LangTerm, PredAndVarsTerm, FlagsTerm, CodeTerm],
        LangContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("foreign_proc"), words("declaration:"), nl]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLanguage),
        (
            MaybeForeignLanguage = ok1(ForeignLanguage),
            LangSpecs = []
        ;
            MaybeForeignLanguage = error1(LangSpecs),
            ForeignLanguage = lang_c  % Dummy, ignored when LangSpecs \= []
        ),
        parse_pragma_ordinary_foreign_proc(ModuleName, VarSet,
            ForeignLanguage, PredAndVarsTerm, FlagsTerm, CodeTerm, Context,
            SeqNum, MaybeRestIOM),
        ( if
            LangSpecs = [],
            MaybeRestIOM = ok1(IOM)
        then
            MaybeIOM = ok1(IOM)
        else
            Specs = LangSpecs ++ get_any_errors1(MaybeRestIOM),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _]
        ; PragmaTerms = [_, _, _, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("foreign_proc"),
            words("declaration must have four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_pragma_ordinary_foreign_proc(module_name::in, varset::in,
    foreign_language::in, term::in, term::in, term::in, prog_context::in,
    int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_ordinary_foreign_proc(ModuleName, VarSet, ForeignLanguage,
        PredAndVarsTerm, FlagsTerm, CodeTerm, Context, SeqNum, MaybeIOM) :-
    PredAndVarsContextPieces =
        cord.from_list([words("In the second argument of"),
            pragma_decl("foreign_proc"), words("declaration:"), nl]),
    parse_pred_or_func_and_args_general(yes(ModuleName), PredAndVarsTerm,
        VarSet, PredAndVarsContextPieces, MaybePredAndArgs),
    (
        MaybePredAndArgs =
            ok2(PredName0, NonFuncArgTerms - MaybeFuncResultTerm),
        % Is this a function or a predicate?
        (
            MaybeFuncResultTerm = yes(FuncResultTerm),
            PredOrFunc0 = pf_function,
            ArgTerms = NonFuncArgTerms ++ [FuncResultTerm]
        ;
            MaybeFuncResultTerm = no,
            PredOrFunc0 = pf_predicate,
            ArgTerms = NonFuncArgTerms
        ),
        parse_pragma_foreign_proc_varlist(VarSet, PredAndVarsContextPieces,
            ArgTerms, 1, MaybePragmaVars),
        (
            MaybePragmaVars = ok1(PragmaVars0),
            MaybeNamePFPragmaVars = ok3(PredName0, PredOrFunc0, PragmaVars0)
        ;
            MaybePragmaVars = error1(PragmaVarsSpecs),
            MaybeNamePFPragmaVars = error3(PragmaVarsSpecs)
        )
    ;
        MaybePredAndArgs = error2(PredAndArgsSpecs),
        MaybeNamePFPragmaVars = error3(PredAndArgsSpecs)
    ),

    FlagsContextPieces = cord.from_list([words("In the third argument of"),
        pragma_decl("foreign_proc"), words("declaration:"), nl]),
    parse_and_check_pragma_foreign_proc_attributes_term(ForeignLanguage,
        VarSet, FlagsTerm, FlagsContextPieces, MaybeFlags),

    CodeContext = get_term_context(CodeTerm),
    ( if CodeTerm = term.functor(term.string(Code), [], _) then
        Impl0 = fp_impl_ordinary(Code, yes(CodeContext)),
        MaybeImpl = ok1(Impl0)
    else
        CodeTermStr = describe_error_term(VarSet, CodeTerm),
        ImplPieces = [words("In the fourth argument of"),
            pragma_decl("foreign_proc"), words("declaration:"), nl,
            words("error: expected a string containing foreign code, got"),
            quote(CodeTermStr), suffix("."), nl],
        ImplSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, CodeContext, ImplPieces),
        MaybeImpl = error1([ImplSpec])
    ),

    ( if
        MaybeNamePFPragmaVars = ok3(PredName, PredOrFunc, PragmaVars),
        MaybeFlags = ok1(Flags),
        MaybeImpl = ok1(Impl)
    then
        varset.coerce(VarSet, ProgVarSet),
        varset.coerce(VarSet, InstVarSet),
        FPInfo = pragma_info_foreign_proc(Flags, PredName, PredOrFunc,
            PragmaVars, ProgVarSet, InstVarSet, Impl),
        Pragma = impl_pragma_foreign_proc(FPInfo),
        ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
        Item = item_impl_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        AllSpecs = get_any_errors1(MaybeImpl) ++
            get_any_errors3(MaybeNamePFPragmaVars) ++
            get_any_errors1(MaybeFlags),
        MaybeIOM = error1(AllSpecs)
    ).

    % Parse the sole argument of a (decl or impl) pragma that should contain
    % a symbol name / arity pair.

:- pred parse_name_arity_decl_pragma(module_name::in, string::in, string::in,
    pred(sym_name, int, decl_pragma)::(pred(in, in, out) is det),
    list(term)::in, term::in, varset::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_name_arity_decl_pragma(ModuleName, PragmaName, NameKind, MakePragma,
        PragmaTerms, ErrorTerm, VarSet, Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [NameAndArityTerm],
        parse_simple_name_and_arity(ModuleName, PragmaName, NameKind,
            NameAndArityTerm, NameAndArityTerm, VarSet, MaybeNameAndArity),
        (
            MaybeNameAndArity = ok2(Name, Arity),
            MakePragma(Name, Arity, Pragma),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_decl_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybeNameAndArity = error2(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl(PragmaName),
            words("declaration must have exactly one argument."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

:- pred parse_name_arity_impl_pragma(module_name::in, string::in, string::in,
    pred(sym_name, int, impl_pragma)::(pred(in, in, out) is det),
    list(term)::in, term::in, varset::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_name_arity_impl_pragma(ModuleName, PragmaName, NameKind, MakePragma,
        PragmaTerms, ErrorTerm, VarSet, Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [NameAndArityTerm],
        parse_simple_name_and_arity(ModuleName, PragmaName, NameKind,
            NameAndArityTerm, NameAndArityTerm, VarSet, MaybeNameAndArity),
        (
            MaybeNameAndArity = ok2(Name, Arity),
            MakePragma(Name, Arity, Pragma),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_impl_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybeNameAndArity = error2(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl(PragmaName),
            words("declaration must have exactly one argument."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
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
    ( if
        parse_implicitly_qualified_name_and_arity(ModuleName,
            NameAndArityTerm, Name, Arity)
    then
        MaybeNameAndArity = ok2(Name, Arity)
    else
        NameAndArityTermStr = describe_error_term(VarSet, NameAndArityTerm),
        Pieces = [words("Error: expected"), words(NameKind),
            words("name/arity for"), pragma_decl(PragmaName),
            words("declaration, got"), quote(NameAndArityTermStr),
            suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeNameAndArity = error2([Spec])
    ).

%---------------------------------------------------------------------------%

:- type collected_pragma_foreign_proc_attribute
    --->    coll_may_call_mercury(proc_may_call_mercury)
    ;       coll_thread_safe(proc_thread_safe)
    ;       coll_tabled_for_io(proc_tabled_for_io)
    ;       coll_purity(purity)
    ;       coll_user_annotated_sharing(user_annotated_sharing)
    ;       coll_backend(backend)
    ;       coll_terminates(proc_terminates)
    ;       coll_will_not_throw_exception
    ;       coll_ordinary_despite_detism
    ;       coll_may_modify_trail(proc_may_modify_trail)
    ;       coll_may_call_mm_tabled(proc_may_call_mm_tabled)
    ;       coll_box_policy(box_policy)
    ;       coll_affects_liveness(proc_affects_liveness)
    ;       coll_allocates_memory(proc_allocates_memory)
    ;       coll_registers_roots(proc_registers_roots)
    ;       coll_may_duplicate(proc_may_duplicate).

:- pred parse_and_check_pragma_foreign_proc_attributes_term(
    foreign_language::in, varset::in, term::in, cord(format_component)::in,
    maybe1(pragma_foreign_proc_attributes)::out) is det.

parse_and_check_pragma_foreign_proc_attributes_term(ForeignLanguage, VarSet,
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
            coll_may_call_mm_tabled(proc_may_call_mm_tabled),
        coll_box_policy(bp_native_if_possible) -
            coll_box_policy(bp_always_boxed),
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
    parse_pragma_foreign_proc_attributes_term(ContextPieces, VarSet, Term,
        MaybeAttrList),
    (
        MaybeAttrList = ok1(AttrList),
        ( if
            % XXX Consider using report_any_conflicts instead.
            some [Conflict1, Conflict2] (
                list.member(Conflict1 - Conflict2, ConflictingAttributes),
                list.member(Conflict1, AttrList),
                list.member(Conflict2, AttrList)
            )
        then
            % We could include Conflict1 and Conflict2 in the message,
            % but the conflict is usually very obvious even without this.
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: conflicting attributes in attribute list."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybeAttributes = error1([Spec])
        else
            list.foldl(process_attribute, AttrList, Attributes0, Attributes),
            MaybeAttributes = check_required_attributes(ForeignLanguage,
                Attributes, get_term_context(Term))
        )
    ;
        MaybeAttrList = error1(Specs),
        MaybeAttributes = error1(Specs)
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
    % a particular language.
    %
:- func check_required_attributes(foreign_language,
        pragma_foreign_proc_attributes, term.context)
    = maybe1(pragma_foreign_proc_attributes).

check_required_attributes(Lang, Attrs, _Context) = MaybeAttrs :-
    (
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_java
        ; Lang = lang_erlang
        ),
        MaybeAttrs = ok1(Attrs)
    ).

:- pred parse_pragma_foreign_proc_attributes_term(cord(format_component)::in,
    varset::in, term::in,
    maybe1(list(collected_pragma_foreign_proc_attribute))::out) is det.

parse_pragma_foreign_proc_attributes_term(ContextPieces, VarSet, Term,
        MaybeAttrs) :-
    ( if parse_single_pragma_foreign_proc_attribute(VarSet, Term, Attr) then
        MaybeAttrs = ok1([Attr])
    else
        parse_pragma_foreign_proc_attributes_list(ContextPieces, VarSet,
            Term, 1, MaybeAttrs)
    ).

:- pred parse_pragma_foreign_proc_attributes_list(cord(format_component)::in,
    varset::in, term::in, int::in,
    maybe1(list(collected_pragma_foreign_proc_attribute))::out) is det.

parse_pragma_foreign_proc_attributes_list(ContextPieces, VarSet,
        Term, HeadAttrNum, MaybeAttrs) :-
    ( if
        Term = term.functor(term.atom("[]"), [], _)
    then
        MaybeAttrs = ok1([])
    else if
        Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _)
    then
        parse_pragma_foreign_proc_attributes_list(ContextPieces, VarSet,
            TailTerm, HeadAttrNum + 1, MaybeTailAttrs),
        ( if
            parse_single_pragma_foreign_proc_attribute(VarSet, HeadTerm,
                HeadAttr)
        then
            (
                MaybeTailAttrs = ok1(TailAttrs),
                MaybeAttrs = ok1([HeadAttr | TailAttrs])
            ;
                MaybeTailAttrs = error1(TailSpecs),
                MaybeAttrs = error1(TailSpecs)
            )
        else
            HeadTermStr = mercury_limited_term_to_string(VarSet,
                print_name_only, 80, HeadTerm),
            HeadPieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first,
                words("Error: the"), nth_fixed(HeadAttrNum),
                words("element of the attribute list,"),
                quote(HeadTermStr), suffix(","),
                words("is not a valid foreign_proc attribute."), nl],
            HeadSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(HeadTerm), HeadPieces),
            MaybeAttrs = error1([HeadSpec | get_any_errors1(MaybeTailAttrs)])
        )
    else
        TermStr = mercury_limited_term_to_string(VarSet, print_name_only,
            80, Term),
        TermPieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first,
            words("Error: expected an attribute list, got"),
            quote(TermStr), suffix("."), nl],
        TermSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), TermPieces),
        MaybeAttrs = error1([TermSpec])
    ).

:- pred parse_single_pragma_foreign_proc_attribute(varset::in, term::in,
    collected_pragma_foreign_proc_attribute::out) is semidet.

parse_single_pragma_foreign_proc_attribute(VarSet, Term, Flag) :-
    ( if parse_may_call_mercury(Term, MayCallMercury) then
        Flag = coll_may_call_mercury(MayCallMercury)
    else if parse_threadsafe(Term, ThreadSafe) then
        Flag = coll_thread_safe(ThreadSafe)
    else if parse_tabled_for_io(Term, TabledForIo) then
        Flag = coll_tabled_for_io(TabledForIo)
    else if parse_user_annotated_sharing(VarSet, Term, UserSharing) then
        Flag = coll_user_annotated_sharing(UserSharing)
    else if parse_backend(Term, Backend) then
        Flag = coll_backend(Backend)
    else if parse_purity_promise(Term, Purity) then
        Flag = coll_purity(Purity)
    else if parse_terminates(Term, Terminates) then
        Flag = coll_terminates(Terminates)
    else if parse_no_exception_promise(Term) then
        Flag = coll_will_not_throw_exception
    else if parse_ordinary_despite_detism(Term) then
        Flag = coll_ordinary_despite_detism
    else if parse_may_modify_trail(Term, TrailMod) then
        Flag = coll_may_modify_trail(TrailMod)
    else if parse_may_call_mm_tabled(Term, CallsTabled) then
        Flag = coll_may_call_mm_tabled(CallsTabled)
    else if parse_box_policy(Term, BoxPolicy) then
        Flag = coll_box_policy(BoxPolicy)
    else if parse_affects_liveness(Term, AffectsLiveness) then
        Flag = coll_affects_liveness(AffectsLiveness)
    else if parse_allocates_memory(Term, AllocatesMemory) then
        Flag = coll_allocates_memory(AllocatesMemory)
    else if parse_registers_roots(Term, RegistersRoots) then
        Flag = coll_registers_roots(RegistersRoots)
    else if parse_may_duplicate(Term, MayDuplicate) then
        Flag = coll_may_duplicate(MayDuplicate)
    else
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

:- pred parse_may_call_mm_tabled(term::in, proc_may_call_mm_tabled::out)
    is semidet.

parse_may_call_mm_tabled(Term, proc_may_call_mm_tabled) :-
    Term = term.functor(term.atom("may_call_mm_tabled"), [], _).
parse_may_call_mm_tabled(Term, proc_will_not_call_mm_tabled) :-
    Term = term.functor(term.atom("will_not_call_mm_tabled"), [], _).

:- pred parse_box_policy(term::in, box_policy::out) is semidet.

parse_box_policy(Term, bp_native_if_possible) :-
    Term = term.functor(term.atom("native_if_possible"), [], _).
parse_box_policy(Term, bp_always_boxed) :-
    Term = term.functor(term.atom("always_boxed"), [], _).

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

    % Parse the variable list in the pragma foreign_proc declaration.
    % The final argument is 'no' for no error, or 'yes(ErrorMessage)'.
    %
:- pred parse_pragma_foreign_proc_varlist(varset::in,
    cord(format_component)::in,list(term)::in, int::in,
    maybe1(list(pragma_var))::out) is det.

parse_pragma_foreign_proc_varlist(_, _, [], _, ok1([])).
parse_pragma_foreign_proc_varlist(VarSet, ContextPieces,
        [HeadTerm | TailTerm], ArgNum, MaybePragmaVars):-
    parse_pragma_foreign_proc_varlist(VarSet, ContextPieces,
        TailTerm, ArgNum + 1, MaybeTailPragmaVars),
    ( if
        HeadTerm = term.functor(term.atom("::"), [VarTerm, ModeTerm], _),
        VarTerm = term.variable(Var, VarContext)
    then
        ( if varset.search_name(VarSet, Var, VarName0) then
            MaybeVarName = ok1(VarName0)
        else
            % If the variable wasn't in the varset it must be an
            % underscore variable.
            UnnamedPieces = [words("Sorry, not implemented: "),
                words("anonymous"), quote("_"),
                words("variable in pragma foreign_proc."), nl],
            UnnamedSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, VarContext, UnnamedPieces),
            MaybeVarName = error1([UnnamedSpec])
        ),
        ArgContextPieces = ContextPieces ++ cord.from_list(
            [words("in the"), nth_fixed(ArgNum), words("argument:")]),
        parse_mode(allow_constrained_inst_var, VarSet, ArgContextPieces,
            ModeTerm, MaybeMode0),
        ( if
            MaybeMode0 = ok1(Mode0),
            MaybeVarName = ok1(VarName),
            MaybeTailPragmaVars = ok1(TailPragmaVars)
        then
            constrain_inst_vars_in_mode(Mode0, Mode),
            term.coerce_var(Var, ProgVar),
            HeadPragmaVar = pragma_var(ProgVar, VarName, Mode,
                bp_native_if_possible),
            MaybePragmaVars = ok1([HeadPragmaVar | TailPragmaVars])
        else
            Specs = get_any_errors1(MaybeTailPragmaVars)
                ++ get_any_errors1(MaybeVarName)
                ++ get_any_errors1(MaybeTailPragmaVars),
            MaybePragmaVars = error1(Specs)
        )
    else
        Pieces = [words("Error: the"), nth_fixed(ArgNum), words("argument is"),
            words("not in the form"), quote("Var :: mode"), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(HeadTerm), Pieces),
        MaybePragmaVars = error1([Spec | get_any_errors1(MaybeTailPragmaVars)])
    ).

:- pred parse_oisu_pragma(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_oisu_pragma(ModuleName, VarSet, ErrorTerm, PragmaTerms, Context, SeqNum,
        MaybeIOM) :-
    (
        PragmaTerms = [TypeCtorTerm, CreatorsTerm, MutatorsTerm | OtherTerms],
        (
            OtherTerms = [],
            MaybeDestructorsTerm = no
        ;
            OtherTerms = [DestructorsTerm],
            MaybeDestructorsTerm = yes(DestructorsTerm)
        ),
        ( if
            parse_implicitly_qualified_name_and_arity(ModuleName, TypeCtorTerm,
                Name, Arity)
        then
            MaybeTypeCtor = ok1(type_ctor(Name, Arity))
        else
            TypeCtorTermStr = describe_error_term(VarSet, TypeCtorTerm),
            Pieces = [words("In the first argument of"),
                pragma_decl("oisu"), words("declaration:"), nl,
                words("error: expected predicate name/arity, got"),
                quote(TypeCtorTermStr), suffix("."), nl],
            TypeCtorSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(ErrorTerm), Pieces),
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
        ( if
            MaybeTypeCtor = ok1(TypeCtor),
            MaybeCreatorsNamesArities = ok1(CreatorsNamesArities),
            MaybeMutatorsNamesArities = ok1(MutatorsNamesArities),
            MaybeDestructorsNamesArities = ok1(DestructorsNamesArities)
        then
            OISUInfo = pragma_info_oisu(TypeCtor, CreatorsNamesArities,
                MutatorsNamesArities, DestructorsNamesArities),
            Pragma = decl_pragma_oisu(OISUInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_decl_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeTypeCtor) ++
                get_any_errors1(MaybeCreatorsNamesArities) ++
                get_any_errors1(MaybeMutatorsNamesArities) ++
                get_any_errors1(MaybeDestructorsNamesArities),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("oisu"),
            words("declaration must have three or four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

:- pred parse_oisu_preds_term(module_name::in, varset::in, string::in,
    string::in, term::in, maybe1(list(pred_name_arity))::out) is det.

parse_oisu_preds_term(ModuleName, VarSet, ArgNum, ExpectedFunctor, Term,
        MaybeNamesArities) :-
    ( if
        Term = term.functor(term.atom(Functor), Args, _),
        Functor = ExpectedFunctor,
        Args = [Arg]
    then
        parse_name_and_arity_list(ModuleName, VarSet, ExpectedFunctor,
            Arg, MaybeNamesArities)
    else
        Pieces = [words("Error:"), words(ArgNum), words("argument of"),
            pragma_decl("oisu"), words("declaration"),
            words("should have the form"),
            quote(ExpectedFunctor ++ "([pred1/arity1, ..., predn/arityn])"),
            suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeNamesArities = error1([Spec])
    ).

:- pred parse_name_and_arity_list(module_name::in, varset::in, string::in,
    term::in, maybe1(list(pred_name_arity))::out) is det.

parse_name_and_arity_list(ModuleName, VarSet, Wrapper, Term,
        MaybeNamesArities) :-
    (
        Term = term.functor(Functor, Args, _),
        ( if
            Functor = term.atom("[]"),
            Args = []
        then
            MaybeNamesArities = ok1([])
        else if
            Functor = term.atom("[|]"),
            Args = [Arg1, Arg2]
        then
            ( if
                parse_implicitly_qualified_name_and_arity(ModuleName,
                    Arg1, Arg1Name, Arg1Arity)
            then
                MaybeHeadNameArity = ok1(pred_name_arity(Arg1Name, Arg1Arity))
            else
                Arg1Str = describe_error_term(VarSet, Arg1),
                Pieces = [words("Error: expected predname/arity,"),
                    words("got"), quote(Arg1Str), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Arg1), Pieces),
                MaybeHeadNameArity = error1([Spec])
            ),
            parse_name_and_arity_list(ModuleName, VarSet, Wrapper, Arg2,
                MaybeTailNamesArities),
            ( if
                MaybeHeadNameArity = ok1(HeadNameArity),
                MaybeTailNamesArities = ok1(TailNamesArities)
            then
                MaybeNamesArities = ok1([HeadNameArity | TailNamesArities])
            else
                HeadSpecs = get_any_errors1(MaybeHeadNameArity),
                TailSpecs = get_any_errors1(MaybeTailNamesArities),
                MaybeNamesArities = error1(HeadSpecs ++ TailSpecs)
            )
        else
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: expected a list as the argument of"),
                words(Wrapper), suffix(","),
                words("got"), quote(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybeNamesArities = error1([Spec])
        )
    ;
        Term = term.variable(_, _),
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a list as the argument of"),
            words(Wrapper), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeNamesArities = error1([Spec])
    ).

:- pred parse_tabling_pragma(module_name::in, varset::in, term::in,
    string::in, list(term)::in, prog_context::in, int::in,
    eval_method::in, maybe1(item_or_marker)::out) is det.

parse_tabling_pragma(ModuleName, VarSet, ErrorTerm, PragmaName, PragmaTerms,
        Context, SeqNum, EvalMethod0, MaybeIOM) :-
    (
        (
            PragmaTerms = [PredAndModesTerm0],
            MaybeAttrs = no
        ;
            PragmaTerms = [PredAndModesTerm0, AttrListTerm0],
            MaybeAttrs = yes(AttrListTerm0)
        ),
        ContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl(PragmaName), words("declaration:"), nl]),
        parse_arity_or_modes(ModuleName, PredAndModesTerm0, ErrorTerm,
            VarSet, ContextPieces, MaybeArityOrModes),
        (
            MaybeArityOrModes = ok1(ArityOrModes),
            ArityOrModes = pred_name_arity_mpf_mmode(PredName, Arity,
                MaybePredOrFunc, MaybeModes),
            (
                MaybeAttrs = no,
                PredNameArityMPF = pred_name_arity_mpf(PredName, Arity,
                    MaybePredOrFunc),
                TabledInfo = pragma_info_tabled(EvalMethod0, PredNameArityMPF,
                    MaybeModes, no),
                Pragma = impl_pragma_tabled(TabledInfo),
                ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
                Item = item_impl_pragma(ItemPragma),
                MaybeIOM = ok1(iom_item(Item))
            ;
                MaybeAttrs = yes(AttrsListTerm),
                AttrContextPieces = cord.from_list(
                    [words("In the second argument of"),
                    pragma_decl(PragmaName), words("declaration:"), nl]),
                convert_list("tabling attributes",
                    parse_tabling_attribute(AttrContextPieces, EvalMethod0),
                    VarSet, AttrsListTerm, MaybeAttributeList),
                (
                    MaybeAttributeList = ok1(AttributeList),
                    update_tabling_attributes(AttributeList,
                        default_memo_table_attributes, Attributes,
                        [], DuplicateSpecs),
                    (
                        DuplicateSpecs = [],
                        DisableWarning =
                            Attributes ^ table_attr_backend_warning,
                        (
                            DisableWarning = table_attr_ignore_with_warning,
                            EvalMethod = EvalMethod0
                        ;
                            DisableWarning = table_attr_ignore_without_warning,
                            (
                                EvalMethod0 = eval_memo(_),
                                EvalMethod = eval_memo(
                                    table_attr_ignore_without_warning)
                            ;
                                ( EvalMethod0 = eval_loop_check
                                ; EvalMethod0 = eval_minimal(_)
                                ),
                                EvalMethod = EvalMethod0
                            ;
                                ( EvalMethod0 = eval_table_io(_, _)
                                ; EvalMethod0 = eval_normal
                                ),
                                unexpected($pred, "non-pragma eval method")
                            )
                        ),
                        PredNameArityMPF = pred_name_arity_mpf(PredName,
                            Arity, MaybePredOrFunc),
                        TabledInfo = pragma_info_tabled(EvalMethod,
                            PredNameArityMPF, MaybeModes, yes(Attributes)),
                        Pragma = impl_pragma_tabled(TabledInfo),
                        ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
                        Item = item_impl_pragma(ItemPragma),
                        MaybeIOM = ok1(iom_item(Item))
                    ;
                        DuplicateSpecs = [_ | _],
                        MaybeIOM = error1(DuplicateSpecs)
                    )
                ;
                    MaybeAttributeList = error1(Specs),
                    MaybeIOM = error1(Specs)
                )
            )
        ;
            MaybeArityOrModes = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl(PragmaName),
            words("declaration must have one or two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- type single_tabling_attribute
    --->    attr_strictness(call_table_strictness)
    ;       attr_size_limit(int)
    ;       attr_statistics
    ;       attr_allow_reset
    ;       attr_ignore_without_warning.

:- pred update_tabling_attributes(
    assoc_list(term.context, single_tabling_attribute)::in,
    table_attributes::in, table_attributes::out,
    list(error_spec)::in, list(error_spec)::out) is det.

update_tabling_attributes([], !Attributes, !Specs).
update_tabling_attributes([Context - Attr | ContextAttrs],
        !Attributes, !Specs) :-
    (
        Attr = attr_strictness(Strictness),
        ( if !.Attributes ^ table_attr_strictness = cts_all_strict then
            !Attributes ^ table_attr_strictness := Strictness
        else
            Pieces = [words("Error: duplicate argument tabling methods"),
                words("attribute in"), pragma_decl("memo"),
                words("declaration."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Attr = attr_size_limit(Limit),
        ( if !.Attributes ^ table_attr_size_limit = no then
            !Attributes ^ table_attr_size_limit := yes(Limit)
        else
            Pieces = [words("Error: duplicate size limits attribute in"),
                pragma_decl("memo"), words("declaration."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Attr = attr_statistics,
        ( if
            !.Attributes ^ table_attr_statistics = table_dont_gather_statistics
        then
            !Attributes ^ table_attr_statistics := table_gather_statistics
        else
            Pieces = [words("Error: duplicate statistics attribute in"),
                pragma_decl("memo"), words("declaration."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Attr = attr_allow_reset,
        ( if
            !.Attributes ^ table_attr_allow_reset = table_dont_allow_reset
        then
            !Attributes ^ table_attr_allow_reset := table_allow_reset
        else
            Pieces = [words("Error: duplicate allow_reset attribute in"),
                pragma_decl("memo"), words("declaration."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Attr = attr_ignore_without_warning,
        ( if
            !.Attributes ^ table_attr_backend_warning =
                table_attr_ignore_with_warning
        then
            !Attributes ^ table_attr_backend_warning :=
                table_attr_ignore_without_warning
        else
            Pieces = [words("Error: duplicate disable_warning_if_ignored"),
                words("attribute in"),
                pragma_decl("memo"), words("declaration."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ),
    update_tabling_attributes(ContextAttrs, !Attributes, !Specs).

:- pred parse_tabling_attribute(cord(format_component)::in, eval_method::in,
    varset::in, term::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attribute(ContextPieces, EvalMethod, VarSet, Term,
        MaybeContextAttribute) :-
    ( if
        Term = term.functor(term.atom(Functor), ArgTerms, Context),
        ( Functor = "fast_loose"
        ; Functor = "specified"
        ; Functor = "size_limit"
        ; Functor = "statistics"
        ; Functor = "allow_reset"
        ; Functor = "disable_warning_if_ignored"
        )
    then
        (
            Functor = "fast_loose",
            parse_tabling_attr_fast_loose(ContextPieces, EvalMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "specified",
            parse_tabling_attr_specified(ContextPieces, EvalMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "size_limit",
            parse_tabling_attr_size_limit(ContextPieces, EvalMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "statistics",
            parse_tabling_attr_statistics(ContextPieces, EvalMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "allow_reset",
            parse_tabling_attr_allow_reset(ContextPieces, EvalMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "disable_warning_if_ignored",
            parse_tabling_attr_backend_warning(ContextPieces, EvalMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: expected one of"),
            quote("fast_loose"), suffix(","),
            quote("specified(...)"), suffix(","),
            quote("size_limit(...)"), suffix(","),
            quote("statistics"), suffix(","),
            quote("allow_reset"), suffix(","), words("and"),
            quote("disable_warning_if_ignored"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

:- pred parse_tabling_attr_fast_loose(cord(format_component)::in,
    eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_fast_loose(ContextPieces, EvalMethod, _VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [],
        require_tabling_fast_loose(ContextPieces, EvalMethod, Context,
            FastLooseSpecs),
        (
            FastLooseSpecs = [],
            Attribute = attr_strictness(cts_all_fast_loose),
            MaybeContextAttribute = ok1(Context - Attribute)
        ;
            FastLooseSpecs = [_ | _],
            MaybeContextAttribute = error1(FastLooseSpecs)
        )
    ;
        ArgTerms = [_ | _],
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: fast_loose"),
            words("must have no arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

:- pred parse_tabling_attr_specified(cord(format_component)::in,
    eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_specified(ContextPieces, EvalMethod, VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        (
            ArgTerms = [MethodsTerm],
            MaybeHiddenArg = ok1(table_hidden_arg_value)
        ;
            ArgTerms = [MethodsTerm, HiddenArgTerm],
            ( if
                HiddenArgTerm = term.functor(
                    term.atom("hidden_arg_value"), [], _)
            then
                MaybeHiddenArg = ok1(table_hidden_arg_value)
            else if
                HiddenArgTerm = term.functor(
                    term.atom("hidden_arg_addr"), [], _)
            then
                MaybeHiddenArg = ok1(table_hidden_arg_addr)
            else
                HiddenArgTermStr = describe_error_term(VarSet, HiddenArgTerm),
                HiddenArgPieces = cord.list(ContextPieces) ++
                    [lower_case_next_if_not_first,
                    words("In the second argument of specified:"), nl,
                    words("error: expected either"),
                    quote("hidden_arg_value"), words("or"),
                    quote("hidden_arg_addr"), suffix(","),
                    words("got"), quote(HiddenArgTermStr), suffix("."), nl],
                HiddenArgSpec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(HiddenArgTerm), HiddenArgPieces),
                MaybeHiddenArg = error1([HiddenArgSpec])
            )
        ),
        MethodsContextPieces = ContextPieces ++
            cord.from_list([lower_case_next_if_not_first,
            words("In the first argument of specified:"), nl]),
        convert_list("argument tabling methods",
            parse_arg_tabling_method(MethodsContextPieces),
            VarSet, MethodsTerm, MaybeMaybeArgMethods),
        require_tabling_fast_loose(ContextPieces, EvalMethod, Context,
            FastLooseSpecs),
        ( if
            MaybeMaybeArgMethods = ok1(MaybeArgMethods),
            MaybeHiddenArg = ok1(HiddenArg),
            FastLooseSpecs = []
        then
            Attribute = attr_strictness(
                cts_specified(MaybeArgMethods, HiddenArg)),
            MaybeContextAttribute = ok1(Context - Attribute)
        else
            Specs = get_any_errors1(MaybeMaybeArgMethods) ++
                get_any_errors1(MaybeHiddenArg) ++
                FastLooseSpecs,
            MaybeContextAttribute = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: specified must have one or two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

:- pred parse_tabling_attr_size_limit(cord(format_component)::in,
    eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_size_limit(ContextPieces, EvalMethod, VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [LimitTerm],
        LimitContextPieces = ContextPieces ++ cord.from_list(
            [lower_case_next_if_not_first,
            words("In the first argument of size_limit:"), nl]),
        parse_decimal_int(LimitContextPieces, VarSet, LimitTerm, MaybeLimit),
        AllowsSizeLimit = eval_method_allows_size_limit(EvalMethod),
        (
            AllowsSizeLimit = yes,
            AllowSpecs = []
        ;
            AllowsSizeLimit = no,
            AllowPieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first,
                words("Error: evaluation method"),
                fixed(eval_method_to_string(EvalMethod)),
                words("does not allow size limits."), nl],
            AllowSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, AllowPieces),
            AllowSpecs = [AllowSpec]
        ),
        ( if
            MaybeLimit = ok1(Limit),
            AllowSpecs = []
        then
            Attribute = attr_size_limit(Limit),
            MaybeContextAttribute = ok1(Context - Attribute)
        else
            Specs = get_any_errors1(MaybeLimit) ++ AllowSpecs,
            MaybeContextAttribute = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_, _ | _]
        ),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: size_limit must have one argument."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

:- pred parse_tabling_attr_statistics(cord(format_component)::in,
    eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_statistics(ContextPieces, _EvalMethod, _VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [],
        Attribute = attr_statistics,
        MaybeContextAttribute = ok1(Context - Attribute)
    ;
        ArgTerms = [_ | _],
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: statistics must have no arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

:- pred parse_tabling_attr_allow_reset(cord(format_component)::in,
    eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_allow_reset(ContextPieces, _EvalMethod, _VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [],
        Attribute = attr_allow_reset,
        MaybeContextAttribute = ok1(Context - Attribute)
    ;
        ArgTerms = [_ | _],
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: allow_reset must have no arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

:- pred parse_tabling_attr_backend_warning(cord(format_component)::in,
    eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_backend_warning(ContextPieces, EvalMethod, _VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [],
        AllowsDisableWarning =
            eval_method_allows_disable_warning_if_ignored(EvalMethod),
        (
            AllowsDisableWarning = yes,
            Attribute = attr_ignore_without_warning,
            MaybeContextAttribute = ok1(Context - Attribute)
        ;
            AllowsDisableWarning = no,
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: evaluation method"),
                fixed(eval_method_to_string(EvalMethod)),
                words("does not allow disable_warning_if_ignored."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            MaybeContextAttribute = error1([Spec])
        )
    ;
        ArgTerms = [_ | _],
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: allow_reset must have no arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

:- pred require_tabling_fast_loose(cord(format_component)::in, eval_method::in,
    term.context::in, list(error_spec)::out) is det.

require_tabling_fast_loose(ContextPieces, EvalMethod, Context, Specs) :-
    AllowsFastLoose = eval_method_allows_fast_loose(EvalMethod),
    (
        AllowsFastLoose = yes,
        Specs = []
    ;
        AllowsFastLoose = no,
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: evaluation method"),
            fixed(eval_method_to_string(EvalMethod)),
            words("does not allow fast_loose tabling."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, Context, Pieces),
        Specs = [Spec]
    ).

:- func eval_method_allows_fast_loose(eval_method) = bool.

eval_method_allows_fast_loose(eval_normal) = no.
eval_method_allows_fast_loose(eval_loop_check) = yes.
eval_method_allows_fast_loose(eval_memo(_)) = yes.
eval_method_allows_fast_loose(eval_table_io(_, _)) = no.
eval_method_allows_fast_loose(eval_minimal(_)) = no.

:- func eval_method_allows_size_limit(eval_method) = bool.

eval_method_allows_size_limit(eval_normal) = no.
eval_method_allows_size_limit(eval_loop_check) = yes.
eval_method_allows_size_limit(eval_memo(_)) = yes.
eval_method_allows_size_limit(eval_table_io(_, _)) = no.
eval_method_allows_size_limit(eval_minimal(_)) = no.

:- func eval_method_allows_disable_warning_if_ignored(eval_method) = bool.

eval_method_allows_disable_warning_if_ignored(eval_normal) = no.
eval_method_allows_disable_warning_if_ignored(eval_loop_check) = no.
eval_method_allows_disable_warning_if_ignored(eval_memo(_)) = yes.
eval_method_allows_disable_warning_if_ignored(eval_table_io(_, _)) = no.
eval_method_allows_disable_warning_if_ignored(eval_minimal(_)) = no.

:- pred parse_arg_tabling_method(cord(format_component)::in,
    varset::in, term::in, maybe1(maybe(arg_tabling_method))::out) is det.

parse_arg_tabling_method(ContextPieces, VarSet, Term,
        MaybeMaybeArgTablingMethod) :-
    ( if
        Term = term.functor(term.atom(Functor), [], _),
        (
            Functor = "value",
            MaybeArgTablingMethod = yes(arg_value)
        ;
            Functor = "addr",
            MaybeArgTablingMethod = yes(arg_addr)
        ;
            Functor = "promise_implied",
            MaybeArgTablingMethod = yes(arg_promise_implied)
        ;
            Functor = "output",
            MaybeArgTablingMethod = no
        )
    then
        MaybeMaybeArgTablingMethod = ok1(MaybeArgTablingMethod)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: expected one of"),
            quote("value"), suffix(","),
            quote("addr"), suffix(","),
            quote("promise_implied"), suffix(","), words("and"),
            quote("output"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeMaybeArgTablingMethod = error1([Spec])
    ).

:- pred parse_arity_or_modes(module_name::in, term::in, term::in, varset::in,
    cord(format_component)::in, maybe1(pred_name_arity_mpf_mmode)::out) is det.

parse_arity_or_modes(ModuleName, PredAndModesTerm0, ErrorTerm, VarSet,
        ContextPieces, MaybeArityOrModes) :-
    ( if
        % Is this a simple pred/arity pragma.
        PredAndModesTerm0 = term.functor(term.atom("/"),
            [PredNameTerm, ArityTerm], _)
    then
        ( if
            try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
                PredNameTerm, PredName),
            decimal_term_to_int(ArityTerm, Arity)
        then
            MaybeArityOrModes = ok1(pred_name_arity_mpf_mmode(PredName,
                Arity, no, no))
        else
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: expected predname/arity."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(ErrorTerm), Pieces),
            MaybeArityOrModes = error1([Spec])
        )
    else
        parse_pred_or_func_and_arg_modes(yes(ModuleName), VarSet,
            ContextPieces, PredAndModesTerm0, MaybePredAndModes),
        (
            MaybePredAndModes = ok3(PredName, PredOrFunc, Modes),
            list.length(Modes, Arity0),
            (
                PredOrFunc = pf_function,
                Arity = Arity0 - 1
            ;
                PredOrFunc = pf_predicate,
                Arity = Arity0
            ),
            ArityOrModes = pred_name_arity_mpf_mmode(PredName, Arity,
                yes(PredOrFunc), yes(Modes)),
            MaybeArityOrModes = ok1(ArityOrModes)
        ;
            MaybePredAndModes = error3(Specs),
            MaybeArityOrModes = error1(Specs)
        )
    ).

:- type maybe_pred_or_func_modes ==
    maybe3(sym_name, pred_or_func, list(mer_mode)).

:- pred parse_pred_or_func_and_arg_modes(maybe(module_name)::in,
    varset::in, cord(format_component)::in, term::in,
    maybe_pred_or_func_modes::out) is det.

parse_pred_or_func_and_arg_modes(MaybeModuleName, VarSet, ContextPieces,
        PredAndModesTerm, MaybeNameAndModes) :-
    parse_pred_or_func_and_args_general(MaybeModuleName, PredAndModesTerm,
        VarSet, ContextPieces, MaybePredAndArgs),
    (
        MaybePredAndArgs = ok2(PredName, ArgModeTerms - MaybeRetModeTerm),
        (
            MaybeRetModeTerm = no,
            parse_modes(allow_constrained_inst_var, VarSet, ContextPieces,
                ArgModeTerms, MaybeArgModes),
            (
                MaybeArgModes = ok1(ArgModes),
                % For predicates, we don't call constrain_inst_vars_in_mode
                % on ArgModes. XXX Why precisely?
                MaybeNameAndModes = ok3(PredName, pf_predicate, ArgModes)
            ;
                MaybeArgModes = error1(Specs),
                MaybeNameAndModes = error3(Specs)
            )
        ;
            MaybeRetModeTerm = yes(RetModeTerm),
            parse_modes(allow_constrained_inst_var, VarSet, ContextPieces,
                ArgModeTerms, MaybeArgModes0),
            RetContextPieces = ContextPieces ++
                cord.singleton(words("in the return value:")),
            parse_mode(allow_constrained_inst_var, VarSet, RetContextPieces,
                RetModeTerm, MaybeRetMode),
            ( if
                MaybeArgModes0 = ok1(ArgModes0),
                MaybeRetMode = ok1(RetMode)
            then
                ArgModes1 = ArgModes0 ++ [RetMode],
                list.map(constrain_inst_vars_in_mode, ArgModes1, ArgModes),
                MaybeNameAndModes = ok3(PredName, pf_function, ArgModes)
            else
                Specs = get_any_errors1(MaybeArgModes0)
                    ++ get_any_errors1(MaybeRetMode),
                MaybeNameAndModes = error3(Specs)
            )
        )
    ;
        MaybePredAndArgs = error2(Specs),
        MaybeNameAndModes = error3(Specs)
    ).

:- pred parse_bool(cord(format_component)::in, varset::in, term::in,
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

:- pred parse_decimal_int(cord(format_component)::in, varset::in, term::in,
    maybe1(int)::out) is det.

parse_decimal_int(ContextPieces, VarSet, Term, MaybeInt) :-
    ( if decimal_term_to_int(Term, Int) then
        MaybeInt = ok1(Int)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: expected a decimal integer,"),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), Pieces),
        MaybeInt = error1([Spec])
    ).

    % convert_list(What, VarSet, Term, Pred, Result):
    %
    % Convert Term into a list of elements, where Pred converts each element
    % of the list into the correct type. Result will hold the list if the
    % conversion succeeded for each every of M, otherwise it will hold
    % the errors resulting from the failed conversion attempts.
    %
    % This predicate generates error messages for malformed lists. To do that,
    % it uses the What argument, which should have the form "a list of xyzs".
    % The job of generating error messages for any malformed elements
    % is up to Pred.
    %
:- pred convert_list(string::in,
    pred(varset, term, maybe1(T))::(pred(in, in, out) is det),
    varset::in, term::in, maybe1(list(T))::out) is det.

convert_list(What, Pred, VarSet, Term, Result) :-
    (
        Term = term.variable(_, _),
        make_expected_got_spec(VarSet, What, Term, Spec),
        Result = error1([Spec])
    ;
        Term = term.functor(Functor, Args, _Context),
        ( if
            Functor = term.atom("[|]"),
            Args = [HeadTerm, TailTerm]
        then
            Pred(VarSet, HeadTerm, HeadResult),
            convert_list(What, Pred, VarSet, TailTerm, TailResult),
            ( if
                HeadResult = ok1(HeadElement),
                TailResult = ok1(TailElements)
            then
                Result = ok1([HeadElement | TailElements])
            else
                Specs = get_any_errors1(HeadResult) ++
                    get_any_errors1(TailResult),
                Result = error1(Specs)
            )
        else if
            Functor = term.atom("[]"),
            Args = []
        then
            Result = ok1([])
        else
            make_expected_got_spec(VarSet, What, Term, Spec),
            Result = error1([Spec])
        )
    ).

:- pred make_expected_got_spec(varset::in, string::in, term::in,
    error_spec::out) is det.

make_expected_got_spec(VarSet, What, Term, Spec) :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = [words("Error: expected"), words(What), suffix(","),
        words("got"), quote(TermStr), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error,
        phase_term_to_parse_tree, get_term_context(Term), Pieces).

:- pred convert_type_spec_pair(term::in, pair(tvar, mer_type)::out) is semidet.

convert_type_spec_pair(Term, TypeSpec) :-
    Term = term.functor(term.atom("="), [TypeVarTerm, SpecTypeTerm0], _),
    TypeVarTerm = term.variable(TypeVar0, _),
    term.coerce_var(TypeVar0, TypeVar),
    % XXX We should call parse_type instead.
    maybe_parse_type(no_allow_ho_inst_info(wnhii_pragma_type_spec),
        SpecTypeTerm0, SpecType),
    TypeSpec = TypeVar - SpecType.

%---------------------------------------------------------------------------%
%
% Parsing termination2_info pragmas.
%

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
        convert_list("list of argument size constraints",
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
        convert_list("a list of linear terms", parse_lp_term, VarSet, Terms,
            LPTermsResult),
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
        ( if decimal_term_to_int(VarIdTerm, VarId0) then
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
        decimal_term_to_int(NumerTerm, Numer),
        decimal_term_to_int(DenomTerm, Denom)
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

:- pred parse_required_feature(varset::in, term::in,
    maybe1(required_feature)::out) is det.

parse_required_feature(VarSet, Term, MaybeReqFeature) :-
    ( if
        Term = term.functor(term.atom(Functor), [], _),
        string_to_required_feature(Functor, ReqFeature)
    then
        MaybeReqFeature = ok1(ReqFeature)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected the name of a required feature,"),
            words("which must be one of"),
            quote("concurrency"), suffix(","),
            quote("single_prec_float"), suffix(","),
            quote("double_prec_float"), suffix(","),
            quote("memo"), suffix(","),
            quote("parallel_conj"), suffix(","),
            quote("trailing"), suffix(","),
            quote("strict_sequential"), suffix(","), words("and"),
            quote("conservative_gc"), suffix(","),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeReqFeature = error1([Spec])
    ).

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
:- end_module parse_tree.parse_pragma.
%---------------------------------------------------------------------------%
