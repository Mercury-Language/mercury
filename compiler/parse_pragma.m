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
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % Parse the pragma declaration. What it returns is not necessarily
    % a pragma item, and it may not even be an item.
    %
:- pred parse_pragma(module_name::in, varset::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

    % Parse a term that represents a foreign language.
    %
:- pred term_to_foreign_language(term::in, foreign_language::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.rat.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
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
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

parse_pragma(ModuleName, VarSet, PragmaTerms, Context, SeqNum, MaybeIOM) :-
    % XXX ITEM_LIST We should do this ONLY if the top level functor
    % of SinglePragmaTerm0, (PragmaName below) says that this is
    % the kind of pragma for which a "where part" may LEGALLY be present.
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
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
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
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

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
            PragmaTerms, Context, SeqNum, ok1(no), MaybeIOM)
    ;
        PragmaName = "foreign_decl",
        parse_pragma_foreign_decl_pragma(VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_code",
        parse_pragma_foreign_code_pragma(ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_proc",
        parse_pragma_foreign_proc_pragma(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_export_enum",
        parse_pragma_foreign_export_enum(VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "foreign_enum",
        parse_pragma_foreign_enum(VarSet, ErrorTerm,
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
            PragmaName = "consider_used",
            MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
                PredNameArity = pred_name_arity(Name, Arity),
                Pragma = pragma_consider_used(PredNameArity)
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
            VarSet, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "require_tail_recursion",
        parse_pragma_require_tail_recursion(ModuleName, PragmaTerms,
            ErrorTerm, VarSet, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "reserve_tag",
        MakePragma = (pred(Name::in, Arity::in, Pragma::out) is det :-
            TypeCtor = type_ctor(Name, Arity),
            Pragma = pragma_reserve_tag(TypeCtor)
        ),
        parse_name_arity_pragma(ModuleName, PragmaName,
            "type", MakePragma, PragmaTerms, ErrorTerm,
            VarSet, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "oisu",
        parse_oisu_pragma(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
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
        parse_pragma_exceptions(ModuleName, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "trailing_info",
        parse_pragma_trailing_info(ModuleName, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "mm_tabling_info",
        parse_pragma_mm_tabling_info(ModuleName, ErrorTerm,
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
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeIOM = error1([Spec])
        )
    else
        Pieces = [words("Error: a"), pragma_decl("source_file"),
            words("declaration must have exactly one argument."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

% XXX The predicates in the rest of this module ought to be clustered together
% into groups of related predicates, grouping both parse_pragma_xxx predicates
% together with their helper predicates, and grouping parse_pragma_xxx
% predicates for related xxxs together.

:- pred parse_pragma_foreign_type(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(maybe(unify_compare))::in, maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_type(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeMaybeUC, MaybeIOM) :-
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
        LangContextPieces = cord.from_list([
            words("In first argument of"), pragma_decl("foreign_type"),
            words("declaration:")
        ]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        TypeDefnHeadContextPieces = cord.from_list([
            words("In second argument of"), pragma_decl("foreign_type"),
            words("declaration:")
        ]),
        parse_type_defn_head(
            tdhpc_foreign_type_pragma(TypeDefnHeadContextPieces),
            ModuleName, VarSet, MercuryTypeTerm, MaybeTypeDefnHead),
        ForeignTypeContextPieces = cord.from_list([
            words("In third argument of"), pragma_decl("foreign_type"),
            words("declaration:")
        ]),
        parse_foreign_language_type(ForeignTypeContextPieces, ForeignTypeTerm,
            VarSet, MaybeForeignLang, MaybeForeignType),
        (
            MaybeAssertionTerm = no,
            AssertionsSet = set.init,
            AssertionSpecs = []
        ;
            MaybeAssertionTerm = yes(AssertionTerm),
            AssertionContextPieces = cord.from_list([
                words("In fourth argument of"), pragma_decl("foreign_type"),
                words("declaration:")
            ]),
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
            MaybeMaybeUC = ok1(MaybeUC)
        then
            varset.coerce(VarSet, TVarSet),
            ItemTypeDefn = item_type_defn_info(MercuryTypeSymName,
                MercuryParams,
                parse_tree_foreign_type(ForeignType, MaybeUC, Assertions),
                TVarSet, Context, SeqNum),
            Item = item_type_defn(ItemTypeDefn),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++
                get_any_errors2(MaybeTypeDefnHead) ++
                get_any_errors1(MaybeForeignType) ++
                AssertionSpecs ++
                get_any_errors1(MaybeMaybeUC),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl("foreign_type"),
            words("declaration must have three or four arguments."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_foreign_type_assertions(cord(format_component)::in,
    varset::in, term::in,
    set(foreign_type_assertion)::in, set(foreign_type_assertion)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_foreign_type_assertions(ContextPieces, VarSet, Term, !Assertions,
        !Specs) :-
    ( if Term = term.functor(term.atom("[]"), [], _) then
        true
    else if Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) then
        ( if
            parse_foreign_type_assertion(HeadTerm, HeadAssertion)
        then
            ( if
                set.insert_new(HeadAssertion, !Assertions)
            then
                true
            else
                HeadTermStr = mercury_term_to_string(VarSet, print_name_only,
                    HeadTerm),
                Pieces = cord.list(ContextPieces) ++ [
                    lower_case_next_if_not_first,
                    words("Error:"), words("foreign type assertion"),
                    quote(HeadTermStr), words("is repeated.")
                ],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                !:Specs = [Spec | !.Specs]
            )
        else
            TermStr = mercury_term_to_string(VarSet, print_name_only, Term),
            Pieces = cord.list(ContextPieces) ++ [
                lower_case_next_if_not_first,
                words("Error: expected a foreign type assertion,"),
                words("got"), quote(TermStr), suffix(".")
            ],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        ),
        parse_foreign_type_assertions(ContextPieces, VarSet, TailTerm,
            !Assertions, !Specs)
    else
        TermStr = mercury_term_to_string(VarSet, print_name_only, Term),
        Pieces = cord.list(ContextPieces) ++ [
            lower_case_next_if_not_first,
            words("Error: expected a list of foreign type assertions,"),
            words("got"), quote(TermStr), suffix(".")
        ],
        Spec = error_spec(severity_error,
            phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
        LangContextPieces = cord.from_list([
            words("In first argument of"),
            pragma_decl("foreign_export_enum"), words("declaration:")
        ]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        TypeContextPieces = cord.from_list([
            words("In second argument of"),
            pragma_decl("foreign_export_enum"), words("declaration:")
        ]),
        parse_type_ctor_name_arity(TypeContextPieces, VarSet, MercuryTypeTerm,
            MaybeTypeCtor),
        maybe_parse_export_enum_attributes(VarSet, MaybeAttributesTerm,
            MaybeAttributes),
        maybe_parse_export_enum_overrides(VarSet, MaybeOverridesTerm,
            MaybeOverrides),
        ( if
            MaybeForeignLang = ok1(ForeignLang),
            MaybeTypeCtor = ok1(TypeCtor),
            MaybeAttributes = ok1(Attributes),
            MaybeOverrides = ok1(Overrides)
        then
            FEEInfo = pragma_info_foreign_export_enum(ForeignLang, TypeCtor,
                Attributes, Overrides),
            Pragma = pragma_foreign_export_enum(FEEInfo),
            ItemPragma = item_pragma_info(Pragma, item_origin_user, Context,
                SeqNum),
            Item = item_pragma(ItemPragma),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

:- pred maybe_parse_export_enum_overrides(varset::in, maybe(term)::in,
    maybe1(assoc_list(sym_name, string))::out) is det.

maybe_parse_export_enum_overrides(_, no, ok1([])).
maybe_parse_export_enum_overrides(VarSet, yes(OverridesTerm),
        MaybeOverrides) :-
    UnrecognizedPieces =
        [words("Error: expected a valid mapping element."), nl],
    PairContextPieces =
        cord.singleton(words("In exported enumeration override constructor:")),
    convert_maybe_list("mapping elements", yes(VarSet), OverridesTerm,
        parse_sym_name_string_pair(VarSet, PairContextPieces),
        UnrecognizedPieces, MaybeOverrides).

:- pred parse_sym_name_string_pair(varset::in, cord(format_component)::in,
    term::in, maybe1(pair(sym_name, string))::out) is semidet.

parse_sym_name_string_pair(VarSet, ContextPieces, PairTerm, MaybePair) :-
    PairTerm = functor(Functor, Args, _),
    Functor = term.atom("-"),
    Args = [SymNameTerm, StringTerm],
    StringTerm = functor(term.string(String), _, _),
    parse_sym_name_and_args(VarSet, ContextPieces, SymNameTerm,
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
    ( if
        list_term_to_term_list(AttributesTerm, AttributesTerms),
        map_parser(parse_export_enum_attr(VarSet), AttributesTerms,
            MaybeAttrList),
        MaybeAttrList = ok1(CollectedAttributes)
    then
        ( if
            list.member(ConflictA - ConflictB, ConflictingAttributes),
            list.member(ConflictA, CollectedAttributes),
            list.member(ConflictB, CollectedAttributes)
        then
            % XXX Print the conflicting attributes themselves.
            Pieces = [words("Error: conflicting attributes in"),
                pragma_decl("foreign_export_enum"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(AttributesTerm),
                    [always(Pieces)])]),
            AttributesResult = error1([Spec])
        else
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
                list.foldl(process_export_enum_attribute, CollectedAttributes,
                    Attributes0, Attributes),
                AttributesResult = ok1(Attributes)
            ;
                PrefixAttributes = [_, _ | _],
                % XXX Print the multiply-occurring attribute.
                Pieces = [words("Error: prefix attribute"),
                    words("occurs multiple times in"),
                    pragma_decl("foreign_export_enum"),
                    words("declaration."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(AttributesTerm),
                        [always(Pieces)])]),
                AttributesResult = error1([Spec])
            )
        )
    else
        Pieces = [words("Error: malformed attributes list in"),
            pragma_decl("foreign_export_enum"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(AttributesTerm), [always(Pieces)])]),
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

:- pred parse_export_enum_attr(varset::in, term::in,
    maybe1(collected_export_enum_attribute)::out) is det.

parse_export_enum_attr(VarSet, Term, MaybeAttribute) :-
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
        Pieces = [words("Error: unrecognised attribute in"),
            pragma_decl("foreign_export_enum"), words("declaration:"),
            words(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeAttribute = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Code for parsing foreign_enum pragmas.
%

:- pred parse_pragma_foreign_enum(varset::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_enum(VarSet, ErrorTerm, PragmaTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if PragmaTerms = [LangTerm, MercuryTypeTerm, ValuesTerm] then

        LangContextPieces = cord.from_list([
            words("In first argument of"), pragma_decl("foreign_enum"),
            words("declaration:")
        ]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        TypeContextPieces = cord.from_list([
            words("In second argument of"), pragma_decl("foreign_enum"),
            words("declaration:")
        ]),
        parse_type_ctor_name_arity(TypeContextPieces, VarSet, MercuryTypeTerm,
            MaybeTypeCtor),

        UnrecognizedPieces =
            [words("Error: expected a valid mapping element")],
        PairContextPieces = cord.from_list([
            words("In"), pragma_decl("foreign_enum"),
            words("mapping constructor name:")
        ]),
        % XXX the following doesn't check that foreign values are sensible
        % (e.g. it should reject the empty string).
        convert_maybe_list("mapping elements", yes(VarSet), ValuesTerm,
            parse_sym_name_string_pair(VarSet, PairContextPieces),
            UnrecognizedPieces, MaybeValues0),
        (
            MaybeValues0 = ok1(Values0),
            (
                Values0 = [],
                NoValuesPieces = [
                    words("Error: expected a non-empty list"),
                    words("mapping constructors to foreign values in"),
                    pragma_decl("foreign_enum"), words("declaration."), nl
                ],
                NoValuesSpec = error_spec(severity_error,
                    phase_term_to_parse_tree,
                    [simple_msg(get_term_context(ValuesTerm),
                        [always(NoValuesPieces)])]),
                MaybeValues = error1([NoValuesSpec])
            ;
                Values0 = [_ | _],
                MaybeValues = MaybeValues0
            )
        ;
            MaybeValues0 = error1(_),
            MaybeValues = MaybeValues0
        ),

        ( if
            MaybeForeignLang = ok1(ForeignLang),
            MaybeTypeCtor = ok1(TypeCtor),
            MaybeValues = ok1(Values)
        then
            FEInfo = pragma_info_foreign_enum(ForeignLang, TypeCtor, Values),
            Pragma = pragma_foreign_enum(FEInfo),
            ItemPragma = item_pragma_info(Pragma, item_origin_user, Context,
                SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++
                get_any_errors1(MaybeTypeCtor) ++
                get_any_errors1(MaybeValues),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: wrong number of arguments in"),
            pragma_decl("foreign_enum"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
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
        LangPieces = cord.list(ContextPieces) ++ [
            lower_case_next_if_not_first,
            words("Error: invalid foreign language"),
            quote(describe_error_term(VarSet, LangTerm)), suffix(".")
        ],
        LangSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(LangTerm), [always(LangPieces)])]),
            MaybeForeignLang = error1([LangSpec])
    ).

:- pred parse_type_ctor_name_arity(cord(format_component)::in, varset::in,
    term::in, maybe1(type_ctor)::out) is det.

parse_type_ctor_name_arity(ContextPieces, VarSet, TypeTerm, MaybeTypeCtor) :-
    ( if parse_name_and_arity_unqualified(TypeTerm, Name, Arity) then
        MaybeTypeCtor = ok1(type_ctor(Name, Arity))
    else
        TypeTermStr = describe_error_term(VarSet, TypeTerm),
        Pieces = cord.list(ContextPieces) ++ [
            lower_case_next_if_not_first,
            words("Error: expected name/arity for type,"),
            words("got"), quote(TypeTermStr), suffix(".")
        ],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(TypeTerm), [always(Pieces)])]),
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
    ( if PragmaTerms = [LangTerm, PredAndModesTerm, FunctionTerm] then
        LangContextPieces = cord.from_list([
            words("In first argument of"), pragma_decl("foreign_export"),
            words("declaration:")
        ]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        PredAndModesContextPieces = cord.from_list([
            words("In second argument of"), pragma_decl("foreign_export"),
            words("declaration:")
        ]),
        parse_pred_or_func_and_arg_modes(no, VarSet, PredAndModesContextPieces,
            PredAndModesTerm, MaybePredAndModes),
        ForeignFunctionContextPieces = cord.from_list([
            words("In third argument of"), pragma_decl("foreign_export"),
            words("declaration:")
        ]),
        parse_foreign_function_name(VarSet, ForeignFunctionContextPieces,
            FunctionTerm, MaybeFunction),
        ( if
            MaybeForeignLang = ok1(ForeignLang),
            MaybePredAndModes = ok3(PredName, PredOrFunc, Modes),
            MaybeFunction = ok1(Function)
        then
            PredNameModesPF = pred_name_modes_pf(PredName, Modes, PredOrFunc),
            FPEInfo = pragma_info_foreign_proc_export(ForeignLang,
                PredNameModesPF, Function),
            Pragma = pragma_foreign_proc_export(FPEInfo),
            ItemPragma = item_pragma_info(Pragma, item_origin_user, Context,
                SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++
                get_any_errors3(MaybePredAndModes) ++
                get_any_errors1(MaybeFunction),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: wrong number of arguments in"),
            pragma_decl("foreign_export"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_foreign_function_name(varset::in, cord(format_component)::in,
    term::in, maybe1(string)::out) is det.

parse_foreign_function_name(VarSet, ContextPieces, FunctionTerm,
        MaybeFunction) :-
    ( if FunctionTerm = term.functor(term.string(Function), [], _) then
        ( if Function = "" then
            EmptyNamePieces = cord.list(ContextPieces) ++ [
                lower_case_next_if_not_first,
                words("Error: expected a non-empty string for the"),
                words("foreign language name of the exported procedure,"),
                words("got empty string.")
            ],
            FunctionSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(FunctionTerm),
                    [always(EmptyNamePieces)])]),
            MaybeFunction = error1([FunctionSpec])
        else
            % XXX TODO: if we have a valid foreign language, check that
            % Function is a valid identifier in that language.
            MaybeFunction = ok1(Function)
        )
    else
        FunctionPieces = cord.list(ContextPieces) ++ [
            lower_case_next_if_not_first,
            words("Error: expected a non-empty string for the foreign"),
            words("language name of the exported procedure, got"),
            quote(describe_error_term(VarSet, FunctionTerm)),
            suffix("."), nl
        ],
        FunctionSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(FunctionTerm),
                [always(FunctionPieces)])]),
        MaybeFunction = error1([FunctionSpec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_foreign_import_module(varset::in, term::in,
    list(term)::in, prog_context::in, int::in, maybe1(item_or_marker)::out)
    is det.

parse_pragma_foreign_import_module(VarSet, ErrorTerm, PragmaTerms, Context,
        SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [LangTerm, ImportTerm]
    then
        LangContextPieces = cord.from_list([
            words("In first argument of"),
            pragma_decl("foreign_import_module"), words("declaration:")
        ]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLang),
        ( if try_parse_sym_name_and_no_args(ImportTerm, Import0) then
            MaybeImportModule = ok1(Import0)
        else
            ImportModulePieces = [
                words("Error: invalid module name"),
                quote(describe_error_term(VarSet, ImportTerm)),
                words("in"), pragma_decl("foreign_import_module"),
                words("declaration."), nl],
            ImportModuleSpec = error_spec(severity_error,
                phase_term_to_parse_tree,
                [simple_msg(get_term_context(ImportTerm),
                [always(ImportModulePieces)])]),
            MaybeImportModule = error1([ImportModuleSpec])
        ),
        ( if
            MaybeForeignLang = ok1(Language),
            MaybeImportModule = ok1(Import)
        then
            FIM = foreign_import_module_info(Language, Import),
            FIMInfo = pragma_info_foreign_import_module(FIM),
            Pragma = pragma_foreign_import_module(FIMInfo),
            ItemPragma = item_pragma_info(Pragma, item_origin_user, Context,
                SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeForeignLang) ++
                get_any_errors1(MaybeImportModule),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl("foreign_import_module"),
            words("declaration must have two arguments."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
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
                Pragma = pragma_external_proc(ExternalInfo),
                PragmaInfo = item_pragma_info(Pragma, item_origin_user,
                    Context, SeqNum),
                Item = item_pragma(PragmaInfo),
                MaybeIOM = ok1(iom_item(Item))
            else
                Pieces = [words("Error: the predicate name in the")] ++
                    cord.list(ContextPieces1) ++
                    [words("is not for the expected module, which is"),
                    sym_name(ModuleName), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(ErrorTerm),
                        [always(Pieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_symname_arity(varset::in, term::in, cord(format_component)::in,
    maybe2(sym_name, arity)::out) is det.

parse_symname_arity(VarSet, PredTerm, ContextPieces, MaybeSymNameArity) :-
    ( if PredTerm = term.functor(term.atom("/"), [NameTerm, ArityTerm], _) then
        parse_symbol_name(VarSet, NameTerm, MaybeSymName),
        ( if ArityTerm = term.functor(term.integer(ArityPrime), [], _) then
            MaybeArity = ok1(ArityPrime)
        else
            ArityPieces = [words("Error: in")] ++ cord.list(ContextPieces) ++
                [suffix(":"), words("the arity must be an integer."), nl],
            AritySpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ArityTerm),
                    [always(ArityPieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(PredTerm), [always(Pieces)])]),
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
                words("expected a singleton list containing either"),
                quote("low_level_backend"), words("or"),
                quote("high_level_backend"), suffix(","),
                words("got"), words(OptionsTermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(OptionsTerm), [always(Pieces)])]),
            MaybeMaybeBackend = error1([Spec])
        )
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
        ContextPieces = cord.from_list([words("In"),
            pragma_decl(PragmaName), words("declaration:")]),
        parse_arity_or_modes(ModuleName, PredAndModesTerm,
            PredAndModesTerm, VarSet, ContextPieces, MaybeProc),

        % Parse the options
        (
            MaybeOptionsTerm = yes(OptionsTerm),
            ( if list_term_to_term_list(OptionsTerm, OptionsTerms)
            then
                parse_pragma_require_tail_recursion_options(OptionsTerms,
                    have_not_seen_none, no, no, [], Context, MaybeOptions)
            else
                OptionsContext = get_term_context(OptionsTerm),
                Pieces1 = [words("Error: expected attribute list for"),
                    pragma_decl("require_tail_recursion"),
                    words("declaration, got"),
                    quote(describe_error_term(VarSet, OptionsTerm)),
                    suffix("."), nl],
                Message1 = simple_msg(OptionsContext, [always(Pieces1)]),
                MaybeOptions = error1([error_spec(severity_error,
                    phase_term_to_parse_tree, [Message1])])
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
                PragmaType = pragma_require_tail_recursion(
                    pragma_info_require_tail_recursion(Proc,
                        RequireTailrecInfo)),
                MaybeIOM = ok1(iom_item(item_pragma(
                    item_pragma_info(PragmaType, item_origin_user, Context,
                    SeqNum))))
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
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

conflicting_attributes_error(ThisName, EarlierName, Context) = ErrorSpec :-
    Pieces = [words("Error: Conflicting "),
        pragma_decl("require_tail_recursion"), words("attributes: "),
        quote(ThisName), words("conflicts with earlier attribute"),
        quote(EarlierName), suffix("."), nl],
    Message = simple_msg(Context, [always(Pieces)]),
    ErrorSpec = error_spec(severity_error,
        phase_term_to_parse_tree, [Message]).

:- func pragma_require_tailrec_unknown_term_error(term, prog_context) =
    error_spec.

pragma_require_tailrec_unknown_term_error(Term, Context) = ErrorSpec :-
    varset.init(VarSet),
    Pieces = [words("Error: unrecognised "),
        pragma_decl("require_tail_recursion"), words("attribute: "),
        quote(describe_error_term(VarSet, Term)), suffix("."), nl],
    Message = simple_msg(Context, [always(Pieces)]),
    ErrorSpec = error_spec(severity_error,
        phase_term_to_parse_tree, [Message]).

%---------------------------------------------------------------------------%

:- pred parse_pragma_unused_args(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_unused_args(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    % pragma unused_args should never appear in user programs,
    % only in .opt files.
    ( if
        PragmaTerms = [PredOrFuncTerm, PredNameTerm, ArityTerm, ModeNumTerm,
            UnusedArgsTerm],
        ArityTerm = term.functor(term.integer(Arity), [], _),
        ModeNumTerm = term.functor(term.integer(ModeNum), [], _),
        parse_predicate_or_function(PredOrFuncTerm, PredOrFunc),
        try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
            PredNameTerm, PredName),
        convert_int_list(VarSet, UnusedArgsTerm, MaybeUnusedArgs),
        MaybeUnusedArgs = ok1(UnusedArgs)
    then
        PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity, PredOrFunc,
            ModeNum),
        UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn,
            UnusedArgs),
        Pragma = pragma_unused_args(UnusedArgsInfo),
        ItemPragma = item_pragma_info(Pragma, item_origin_user,
            Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        % XXX Improve this message.
        Pieces = [words("Error in"), pragma_decl("unused_args"),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
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
            SpecNameTerm = term.functor(_, _, SpecContext),

            % This form of the pragma should not appear in source files.
            term.context_file(SpecContext, FileName),
            not string.remove_suffix(FileName, ".m", _),

            try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
                SpecNameTerm, SpecName),
            MaybeName = yes(SpecName)
        )
    then
        ArityOrModesContextPieces = cord.from_list([words("In"),
            pragma_decl("type_spec"), words("declaration:")]),
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
                Pragma = pragma_type_spec(TypeSpecInfo),
                ItemPragma = item_pragma_info(Pragma, item_origin_user,
                    Context, SeqNum),
                Item = item_pragma(ItemPragma),
                MaybeIOM = ok1(iom_item(Item))
            else
                Pieces = [words("Error: expected type substitution in"),
                    pragma_decl("type_spec"), words("declaration."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(TypeSubnTerm),
                        [always(Pieces)])]),
                MaybeIOM = error1([Spec])
            )
        ;
            MaybeArityOrModes = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl("type_spec"),
            words("declaration must have two or three arguments."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_fact_table(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_fact_table(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if PragmaTerms = [PredAndArityTerm, FileNameTerm] then
        parse_pred_name_and_arity(ModuleName, "fact_table",
            PredAndArityTerm, ErrorTerm, VarSet, MaybeNameAndArity),
        (
            MaybeNameAndArity = ok2(PredName, Arity),
            ( if FileNameTerm = term.functor(term.string(FileName), [], _) then
                PredNameArity = pred_name_arity(PredName, Arity),
                FactTableInfo = pragma_info_fact_table(PredNameArity,
                    FileName),
                Pragma = pragma_fact_table(FactTableInfo),
                ItemPragma = item_pragma_info(Pragma, item_origin_user,
                    Context, SeqNum),
                Item = item_pragma(ItemPragma),
                MaybeIOM = ok1(iom_item(Item))
            else
                Pieces = [words("Error: expected string"),
                    words("for fact table filename."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(FileNameTerm),
                        [always(Pieces)])]),
                MaybeIOM = error1([Spec])
            )
        ;
            MaybeNameAndArity = error2(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl("fact_table"),
            words("declaration must have two arguments."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_termination_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_termination_info(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [PredAndModesTerm0, ArgSizeTerm, TerminationTerm],
        ContextPieces = cord.from_list([words("In"),
            pragma_decl("termination_info"), words("declaration:")]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), VarSet,
            ContextPieces, PredAndModesTerm0, MaybeNameAndModes),
        MaybeNameAndModes = ok3(PredName, PredOrFunc, ModeList),
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
    then
        PredNameModesPF = pred_name_modes_pf(PredName, ModeList, PredOrFunc),
        TermInfo = pragma_info_termination_info(PredNameModesPF,
            MaybeArgSizeInfo, MaybeTerminationInfo),
        Pragma = pragma_termination_info(TermInfo),
        ItemPragma = item_pragma_info(Pragma, item_origin_user,
            Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Syntax error in"),
            pragma_decl("termination_info"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_termination2_info(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_termination2_info(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [PredAndModesTerm0, SuccessArgSizeTerm,
            FailureArgSizeTerm, TerminationTerm],
        ContextPieces = cord.from_list([words("In"),
            pragma_decl("termination2_info"), words("declaration:")]),
        parse_pred_or_func_and_arg_modes(yes(ModuleName), VarSet,
            ContextPieces, PredAndModesTerm0, NameAndModesResult),
        NameAndModesResult = ok3(PredName, PredOrFunc, ModeList),
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
    then
        PredNameModesPF = pred_name_modes_pf(PredName, ModeList, PredOrFunc),
        Term2Info = pragma_info_termination2_info(PredNameModesPF,
            SuccessArgSizeInfo, FailureArgSizeInfo, MaybeTerminationInfo),
        Pragma = pragma_termination2_info(Term2Info),
        ItemPragma = item_pragma_info(Pragma, item_origin_user,
            Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Syntax error in"),
            pragma_decl("termination2_info"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
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
            pragma_decl("structure_sharing"), words("declaration:")]),
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
        SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
            HeadVars, Types, MaybeSharingAs),
        Pragma = pragma_structure_sharing(SharingInfo),
        ItemPragma = item_pragma_info(Pragma, item_origin_user,
            Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Syntax error in"),
            pragma_decl("structure_sharing"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
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
            pragma_decl("structure_reuse"), words("declaration:")]),
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
        ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
            HeadVars, Types, MaybeStructureReuse),
        Pragma = pragma_structure_reuse(ReuseInfo),
        ItemPragma = item_pragma_info(Pragma, item_origin_user,
            Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Syntax error in"),
            pragma_decl("structure_reuse"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_exceptions(module_name::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_exceptions(ModuleName, ErrorTerm, PragmaTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if
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
    then
        PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity, PredOrFunc,
            ModeNum),
        ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn,
            ThrowStatus),
        Pragma = pragma_exceptions(ExceptionsInfo),
        ItemPragma = item_pragma_info(Pragma, item_origin_user,
            Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Error in"),
            pragma_decl("exceptions"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_trailing_info(module_name::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_trailing_info(ModuleName, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
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
    then
        PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity, PredOrFunc,
            ModeNum),
        TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn,
            TrailingStatus),
        Pragma = pragma_trailing_info(TrailingInfo),
        ItemPragma = item_pragma_info(Pragma, item_origin_user,
            Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Error in"), pragma_decl("trailing_info"),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_mm_tabling_info(module_name::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_mm_tabling_info(ModuleName, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if
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
    then
        PredNameArityPFMn = pred_name_arity_pf_mn(PredName, Arity, PredOrFunc,
            ModeNum),
        TablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
            MM_TablingStatus),
        Pragma = pragma_mm_tabling_info(TablingInfo),
        ItemPragma = item_pragma_info(Pragma, item_origin_user,
            Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        Pieces = [words("Error in"), pragma_decl("mm_tabling_info"),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_pragma_require_feature_set(varset::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_require_feature_set(VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if PragmaTerms = [FeatureListTerm] then
        UnrecognizedPieces = [words("Error: expected a feature"), nl],
        convert_maybe_list("features", yes(VarSet), FeatureListTerm,
            parse_required_feature, UnrecognizedPieces, MaybeFeatureList),
        (
            MaybeFeatureList = ok1(FeatureList),
            ConflictingFeatures = [
                reqf_single_prec_float - reqf_double_prec_float,
                reqf_parallel_conj     - reqf_trailing
            ],
            ( if
                list.member(ConflictA - ConflictB, ConflictingFeatures),
                list.member(ConflictA, FeatureList),
                list.member(ConflictB, FeatureList)
            then
                FeatureListStr = describe_error_term(VarSet, FeatureListTerm),
                Pieces = [words("Error: conflicting features in feature set:"),
                    nl, words(FeatureListStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(FeatureListTerm),
                        [always(Pieces)])]),
                MaybeIOM = error1([Spec])
            else
                (
                    FeatureList = [],
                    ItemNothing = item_nothing_info(no, Context, SeqNum),
                    Item = item_nothing(ItemNothing)
                ;
                    FeatureList = [_ | _],
                    FeatureSet = set.from_list(FeatureList),
                    RFSInfo = pragma_info_require_feature_set(FeatureSet),
                    Pragma = pragma_require_feature_set(RFSInfo),
                    ItemPragma = item_pragma_info(Pragma, item_origin_user,
                        Context, SeqNum),
                    Item = item_pragma(ItemPragma)
                ),
                MaybeIOM = ok1(iom_item(Item))
            )
        ;
            MaybeFeatureList = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl("require_feature_set"),
            words("declaration must have exactly one argument."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

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
    maybe1(foreign_language_type)::out) is det.

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
                    Pieces = cord.list(ContextPieces) ++ [
                        lower_case_next_if_not_first,
                        words("Error: foreign type descriptor for language"),
                        quote(foreign_language_string(Language)),
                        words("must be a non-empty string.")
                    ],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(InputTerm),
                        [always(Pieces)])]),
                    MaybeForeignLangType = error1([Spec])
                else
                    MaybeForeignLangType = ok1(ForeignLangType)
                )
            ;
                Language = lang_erlang,
                ( if ForeignTypeName = "" then
                    MaybeForeignLangType = ok1(erlang(erlang_type))
                else
                    Pieces = cord.list(ContextPieces) ++ [
                        lower_case_next_if_not_first,
                        words("Error: foreign type descriptor for language"),
                        quote(foreign_language_string(Language)),
                        words("must be an empty string.")
                    ],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(InputTerm),
                        [always(Pieces)])]),
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
        Pieces = cord.list(ContextPieces) ++ [
            lower_case_next_if_not_first,
            words("Error: expected a string specifying the"),
            words("foreign type descriptor, got"), quote(InputTermStr),
            suffix(".")
        ],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(InputTerm), [always(Pieces)])]),
        MaybeForeignLangType = error1([Spec])
    ).

    % This predicate parses foreign_decl pragmas.
    %
:- pred parse_pragma_foreign_decl_pragma(varset::in, term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_decl_pragma(VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    InvalidDeclPieces = [words("Error: invalid"),
        pragma_decl("foreign_decl"), words("declaration:")],
    ( if
        (
            PragmaTerms = [LangTerm, HeaderTerm],
            IsLocal = foreign_decl_is_exported
        ;
            PragmaTerms = [LangTerm, IsLocalTerm, HeaderTerm],
            parse_foreign_decl_is_local(IsLocalTerm, IsLocal)
        )
    then
        ( if term_to_foreign_language(LangTerm, ForeignLanguage) then
            ( if
                parse_foreign_literal_or_include(HeaderTerm, LiteralOrInclude)
            then
                FDInfo = pragma_info_foreign_decl(ForeignLanguage, IsLocal,
                    LiteralOrInclude),
                Pragma = pragma_foreign_decl(FDInfo),
                ItemPragma = item_pragma_info(Pragma, item_origin_user,
                    Context, SeqNum),
                Item = item_pragma(ItemPragma),
                MaybeIOM = ok1(iom_item(Item))
            else
                Pieces = InvalidDeclPieces ++
                    [words("expected string or include_file for"),
                    words("foreign declaration code."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeaderTerm),
                        [always(Pieces)])]),
                MaybeIOM = error1([Spec])
            )
        else
            Pieces = InvalidDeclPieces ++
                [words("invalid language parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(LangTerm),
                    [always(Pieces)])]),
            MaybeIOM = error1([Spec])
        )
    else
        TermStr = describe_error_term(VarSet, ErrorTerm),
        Pieces = [words("Error: invalid"), pragma_decl("foreign_decl"),
            words("declaration:"), words(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

    % This predicate parses foreign_code pragmas.
    % Processing of foreign_proc pragmas is handled in
    % parse_pragma_foreign_proc_pragma below.
    %
:- pred parse_pragma_foreign_code_pragma(term::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_code_pragma(ErrorTerm,
        PragmaTerms, Context, SeqNum, MaybeIOM) :-
    InvalidDeclPrefix = [words("Error: invalid"),
        pragma_decl("foreign_code"), words("declaration:")],
    ( if PragmaTerms = [LangTerm, CodeTerm] then
        ( if term_to_foreign_language(LangTerm, ForeignLanguagePrime) then
            ForeignLanguage = ForeignLanguagePrime,
            LangSpecs = []
        else
            ForeignLanguage = lang_c,   % Dummy, ignored when LangSpecs \= []
            LangPieces = InvalidDeclPrefix ++
                [words("invalid language parameter."), nl],
            LangSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(LangTerm),
                    [always(LangPieces)])]),
            LangSpecs = [LangSpec]
        ),
        ( if parse_foreign_literal_or_include(CodeTerm, CodePrime) then
            Code = CodePrime,
            CodeSpecs = []
        else
            Code = floi_literal(""), % Dummy, ignored when CodeSpecs \= []
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
            ItemPragma = item_pragma_info(Pragma, item_origin_user,
                Context, SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        ;
            Specs = [_ | _],
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = InvalidDeclPrefix ++
            [words("wrong number of arguments."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

    % This predicate parses foreign_proc pragmas.
    %
:- pred parse_pragma_foreign_proc_pragma(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_foreign_proc_pragma(ModuleName, VarSet, ErrorTerm,
        PragmaTerms, Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [LangTerm | RestTerms],
        LangContextPieces = cord.from_list([
            words("In first argument of"), pragma_decl("foreign_proc"),
            words("declaration:")
        ]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm,
            MaybeForeignLanguage),
        (
            MaybeForeignLanguage = ok1(ForeignLanguage),
            LangSpecs = []
        ;
            MaybeForeignLanguage = error1(LangSpecs),
            ForeignLanguage = lang_c  % Dummy, ignored when LangSpecs \= []
        ),
        ( if
            RestTerms = [PredAndVarsTerm, FlagsTerm, CodeTerm],
            parse_pragma_ordinary_foreign_proc_pragma(ModuleName,
                VarSet, ForeignLanguage, PredAndVarsTerm, FlagsTerm, CodeTerm,
                Context, SeqNum, MaybeRestIOM)
        then
            (
                MaybeRestIOM = ok1(IOM),
                (
                    LangSpecs = [],
                    MaybeIOM = ok1(IOM)
                ;
                    LangSpecs = [_ | _],
                    MaybeIOM = error1(LangSpecs)
                )
            ;
                MaybeRestIOM = error1(RestSpecs),
                MaybeIOM = error1(LangSpecs ++ RestSpecs)
            )
        else
            Pieces = [
                words("Error: a "), pragma_decl("foreign_proc"),
                words("declaration must have four arguments.")
            ],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
            MaybeIOM = error1([Spec])
        )
    ;
        PragmaTerms = [],
        Pieces = [
            words("Error: a "), pragma_decl("foreign_proc"),
            words("declaration must have four arguments.")
        ],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_pragma_ordinary_foreign_proc_pragma(module_name::in,
    varset::in, foreign_language::in, term::in, term::in, term::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_pragma_ordinary_foreign_proc_pragma(ModuleName, VarSet,
        ForeignLanguage, PredAndVarsTerm, FlagsTerm, CodeTerm,
        Context, SeqNum, MaybeIOM) :-
    PredAndVarsContextPieces =
        cord.from_list([words("In the second argument of"),
            pragma_decl("foreign_proc"), words("declaration:")]),
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
        pragma_decl("foreign_proc"), words("declaration:")]),
    parse_and_check_pragma_foreign_proc_attributes_term(ForeignLanguage,
        VarSet, FlagsTerm, FlagsContextPieces, MaybeFlags),

    CodeContext = get_term_context(CodeTerm),
    ( if CodeTerm = term.functor(term.string(Code), [], _) then
        Impl0 = fp_impl_ordinary(Code, yes(CodeContext)),
        MaybeImpl = ok1(Impl0)
    else
        CodeTermStr = describe_error_term(VarSet, CodeTerm),
        ImplPieces = [words("In the fourth argument of"),
            pragma_decl("foreign_proc"), words("declaration:"),
            words("error: expected a string containing foreign code, got"),
            quote(CodeTermStr), suffix("."), nl],
        ImplSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(CodeContext, [always(ImplPieces)])]),
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
        Pragma = pragma_foreign_proc(FPInfo),
        ItemPragma = item_pragma_info(Pragma, item_origin_user,
            Context, SeqNum),
        Item = item_pragma(ItemPragma),
        MaybeIOM = ok1(iom_item(Item))
    else
        AllSpecs = get_any_errors1(MaybeImpl) ++
            get_any_errors3(MaybeNamePFPragmaVars) ++
            get_any_errors1(MaybeFlags),
        MaybeIOM = error1(AllSpecs)
    ).

    % Parse the sole argument of a pragma that should contain
    % a symbol name / arity pair.
    %
:- pred parse_name_arity_pragma(module_name::in, string::in, string::in,
    pred(sym_name, int, pragma_type)::(pred(in, in, out) is det),
    list(term)::in, term::in, varset::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_name_arity_pragma(ModuleName, PragmaName, NameKind, MakePragma,
        PragmaTerms, ErrorTerm, VarSet, Context, SeqNum, MaybeIOM) :-
    ( if PragmaTerms = [NameAndArityTerm] then
        parse_simple_name_and_arity(ModuleName, PragmaName, NameKind,
            NameAndArityTerm, NameAndArityTerm, VarSet, MaybeNameAndArity),
        (
            MaybeNameAndArity = ok2(Name, Arity),
            MakePragma(Name, Arity, Pragma),
            ItemPragma = item_pragma_info(Pragma, item_origin_user,
                Context, SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybeNameAndArity = error2(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: wrong number of arguments in"),
            pragma_decl(PragmaName), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
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
    ( if parse_name_and_arity(ModuleName, NameAndArityTerm, Name, Arity) then
        MaybeNameAndArity = ok2(Name, Arity)
    else
        NameAndArityTermStr = describe_error_term(VarSet, NameAndArityTerm),
        Pieces = [words("Error: expected"), words(NameKind),
            words("name/arity for"), pragma_decl(PragmaName),
            words("declaration, not"), quote(NameAndArityTermStr),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
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
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
    % a particular language
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
            HeadSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(HeadTerm),
                    [always(HeadPieces)])]),
            MaybeAttrs = error1([HeadSpec | get_any_errors1(MaybeTailAttrs)])
        )
    else
        TermStr = mercury_limited_term_to_string(VarSet, print_name_only,
            80, Term),
        TermPieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first,
            words("Error: expected an attribute list, found"),
            words(TermStr), suffix("."), nl],
        TermSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(TermPieces)])]),
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
            UnnamedSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(VarContext, [always(UnnamedPieces)])]),
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
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
        MaybePragmaVars = error1([Spec | get_any_errors1(MaybeTailPragmaVars)])
    ).

:- pred parse_oisu_pragma(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_oisu_pragma(ModuleName, VarSet, ErrorTerm, PragmaTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if
        PragmaTerms = [TypeCtorTerm, CreatorsTerm, MutatorsTerm | OtherTerms],
        (
            OtherTerms = [],
            MaybeDestructorsTerm = no
        ;
            OtherTerms = [DestructorsTerm],
            MaybeDestructorsTerm = yes(DestructorsTerm)
        )
    then
        ( if parse_name_and_arity(ModuleName, TypeCtorTerm, Name, Arity) then
            MaybeTypeCtor = ok1(type_ctor(Name, Arity))
        else
            TypeCtorTermStr = describe_error_term(VarSet, TypeCtorTerm),
            Pieces = [words("Error: expected"),
                words("predicate name/arity for first argument of"),
                pragma_decl("oisu"), words("declaration, not"),
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
        ( if
            MaybeTypeCtor = ok1(TypeCtor),
            MaybeCreatorsNamesArities = ok1(CreatorsNamesArities),
            MaybeMutatorsNamesArities = ok1(MutatorsNamesArities),
            MaybeDestructorsNamesArities = ok1(DestructorsNamesArities)
        then
            OISUInfo = pragma_info_oisu(TypeCtor, CreatorsNamesArities,
                MutatorsNamesArities, DestructorsNamesArities),
            Pragma = pragma_oisu(OISUInfo),
            ItemPragma = item_pragma_info(Pragma, item_origin_user,
                Context, SeqNum),
            Item = item_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeTypeCtor) ++
                get_any_errors1(MaybeCreatorsNamesArities) ++
                get_any_errors1(MaybeMutatorsNamesArities) ++
                get_any_errors1(MaybeDestructorsNamesArities),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: wrong number of arguments in"),
            pragma_decl("oisu"), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
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
                parse_name_and_arity(ModuleName, Arg1, Arg1Name, Arg1Arity)
            then
                MaybeHeadNameArity = ok1(pred_name_arity(Arg1Name, Arg1Arity))
            else
                Arg1Str = describe_error_term(VarSet, Arg1),
                Pieces = [words("Error: expected predname/arity,"),
                    words("not"), quote(Arg1Str), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Arg1), [always(Pieces)])]),
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

:- pred parse_tabling_pragma(module_name::in, varset::in, term::in,
    string::in, list(term)::in, prog_context::in, int::in,
    eval_method::in, maybe1(item_or_marker)::out) is det.

parse_tabling_pragma(ModuleName, VarSet, ErrorTerm, PragmaName, PragmaTerms,
        Context, SeqNum, EvalMethod, MaybeIOM) :-
    ( if
        (
            PragmaTerms = [PredAndModesTerm0],
            MaybeAttrs = no
        ;
            PragmaTerms = [PredAndModesTerm0, AttrListTerm0],
            MaybeAttrs = yes(AttrListTerm0)
        )
    then
        ContextPieces = cord.from_list([words("In"),
            pragma_decl(PragmaName), words("declaration:")]),
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
                TabledInfo = pragma_info_tabled(EvalMethod, PredNameArityMPF,
                    MaybeModes, no),
                Pragma = pragma_tabled(TabledInfo),
                ItemPragma = item_pragma_info(Pragma, item_origin_user,
                    Context, SeqNum),
                Item = item_pragma(ItemPragma),
                MaybeIOM = ok1(iom_item(Item))
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
                        ItemPragma = item_pragma_info(Pragma, item_origin_user,
                            Context, SeqNum),
                        Item = item_pragma(ItemPragma),
                        MaybeIOM = ok1(iom_item(Item))
                    ;
                        MaybeAttributes = error1(Specs),
                        MaybeIOM = error1(Specs)
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
    else
        Pieces = [words("Error: wrong number of arguments in"),
            pragma_decl(PragmaName), words("declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeIOM = error1([Spec])
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
        ( if !.Attributes ^ table_attr_strictness = cts_all_strict then
            !Attributes ^ table_attr_strictness := Strictness,
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        else
            Pieces = [words("Error: duplicate argument tabling methods"),
                words("attribute in"), pragma_decl("memo"),
                words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeAttributes = error1([Spec])
        )
    ;
        SingleAttr = attr_size_limit(Limit),
        ( if !.Attributes ^ table_attr_size_limit = no then
            !Attributes ^ table_attr_size_limit := yes(Limit),
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        else
            Pieces = [words("Error: duplicate size limits attribute in"),
                pragma_decl("memo"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeAttributes = error1([Spec])
        )
    ;
        SingleAttr = attr_statistics,
        ( if
            !.Attributes ^ table_attr_statistics = table_dont_gather_statistics
        then
            !Attributes ^ table_attr_statistics := table_gather_statistics,
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        else
            Pieces = [words("Error: duplicate statistics attribute in"),
                pragma_decl("memo"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeAttributes = error1([Spec])
        )
    ;
        SingleAttr = attr_allow_reset,
        ( if
            !.Attributes ^ table_attr_allow_reset = table_dont_allow_reset
        then
            !Attributes ^ table_attr_allow_reset := table_allow_reset,
            update_tabling_attributes(TermSingleAttrs, !.Attributes,
                MaybeAttributes)
        else
            Pieces = [words("Error: duplicate allow_reset attribute in"),
                pragma_decl("memo"), words("declaration."), nl],
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
        ( if eval_method_allows_fast_loose(EvalMethod) = yes then
            Attribute = attr_strictness(cts_all_fast_loose),
            MaybeContextAttribute = ok1(Context - Attribute)
        else
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
                        cts_specified(MaybeArgMethods,
                            table_hidden_arg_value)),
                    MaybeContextAttribute = ok1(Context - Attribute)
                ;
                    MoreArgs = [Arg2],
                    ( if
                        Arg2 = term.functor(
                            term.atom("hidden_arg_value"), [], _)
                    then
                        Attribute = attr_strictness(
                            cts_specified(MaybeArgMethods,
                                table_hidden_arg_value)),
                        MaybeContextAttribute = ok1(Context - Attribute)
                    else if
                        Arg2 = term.functor(
                            term.atom("hidden_arg_addr"), [], _)
                    then
                        Attribute = attr_strictness(
                            cts_specified(MaybeArgMethods,
                                table_hidden_arg_addr)),
                        MaybeContextAttribute = ok1(Context - Attribute)
                    else
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
            ArityTerm = term.functor(term.integer(Arity), [], _)
        then
            MaybeArityOrModes = ok1(pred_name_arity_mpf_mmode(PredName,
                Arity, no, no))
        else
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: expected predname/arity."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
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
    % conversion succeeded for each element of M, otherwise it will hold
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
        ( if
            Functor = term.atom("[|]"),
            Args = [FirstTerm, RestTerm]
        then
            ( if Pred(FirstTerm, FirstElement) then
                convert_list(What, MaybeVarSet, RestTerm, Pred,
                    UnrecognizedPieces, RestResult),
                (
                    RestResult = ok1(LaterElements),
                    Result = ok1([FirstElement | LaterElements])
                ;
                    RestResult = error1(_),
                    Result = RestResult
                )
            else
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
        else if
            Functor = term.atom("[]"),
            Args = []
        then
            Result = ok1([])
        else
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
        ( if
            Functor = term.atom("[|]"),
            Args = [FirstTerm, RestTerm]
        then
            ( if Pred(FirstTerm, ElementResult) then
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
            else
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
        else if
            Functor = term.atom("[]"),
            Args = []
        then
            Result = ok1([])
        else
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
    % XXX We should call parse_type instead.
    maybe_parse_type(no_allow_ho_inst_info(wnhii_pragma_type_spec),
        SpecTypeTerm0, SpecType),
    TypeSpec = TypeVar - SpecType.

%---------------------------------------------------------------------------%
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_pragma.
%---------------------------------------------------------------------------%
