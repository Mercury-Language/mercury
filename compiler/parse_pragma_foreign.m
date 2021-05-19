%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2020-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_pragma_foreign.m.
%
% This module parses pragmas involving foreign code.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_pragma_foreign.
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

    % Parse foreign_type pragmas.
    %
:- pred parse_pragma_foreign_type(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(maybe_canonical)::in, maybe1(item_or_marker)::out) is det.

:- pred parse_foreign_type_assertions(cord(format_component)::in,
    varset::in, term::in,
    set(foreign_type_assertion)::in, set(foreign_type_assertion)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

    % Parse foreign_decl pragmas.
    %
:- pred parse_pragma_foreign_decl(varset::in, term::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

%---------------------%

    % Parse foreign_code pragmas.
    %
:- pred parse_pragma_foreign_code(varset::in, term::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

%---------------------%

    % Parse foreign_proc pragmas.
    %
:- pred parse_pragma_foreign_proc(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

%---------------------%

    % Parse foreign_export pragmas.
    %
:- pred parse_pragma_foreign_export(varset::in, term::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

%---------------------%

    % Parse foreign_export_enum pragmas.
    %
:- pred parse_pragma_foreign_export_enum(varset::in, term::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

%---------------------%

    % Parse foreign_enum pragmas.
    %
:- pred parse_pragma_foreign_enum(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

%---------------------%

    % Parse foreign_import_module pragmas.
    %
:- pred parse_pragma_foreign_import_module(varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

%---------------------------------------------------------------------------%

    % Parse a term that represents a foreign language.
    %
:- pred term_to_foreign_language(term::in, foreign_language::out) is semidet.

    % Does the term represent the recently deleted lang_erlang?
    %
:- pred term_to_foreign_language_erlang(term::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_defn.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_mode.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

% The predicates in this module are to be clustered together into groups.
% All but the last group have the job of parsing on particular kind of pragma,
% containing parse_pragma_foreign_xxx and its dedicated helper predicates,
% while the last group contains the helper predicates that are needed
% by more than one kind of pragma.
%
% Please keep things this way.

%---------------------------------------------------------------------------%
%
% Parse foreign_type pragmas.
%

parse_pragma_foreign_type(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeMaybeCanonical, MaybeIOM) :-
    (
        (
            PragmaTerms = [LangTerm, MercuryTypeTerm, ForeignTypeTerm],
            MaybeAssertionTerm = no
        ;
            PragmaTerms = [LangTerm, MercuryTypeTerm, ForeignTypeTerm,
                AssertionTerm0],
            MaybeAssertionTerm = yes(AssertionTerm0)
        ),
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
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _]
        ; PragmaTerms = [_, _, _, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("foreign_type"),
            words("declaration must have three or four arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_foreign_language_type(cord(format_component)::in, term::in,
    varset::in, maybe1(foreign_language)::in,
    maybe1(generic_language_foreign_type)::out) is det.

parse_foreign_language_type(ContextPieces, InputTerm, VarSet, MaybeLanguage,
        MaybeForeignLangType) :-
    ( if InputTerm = term.functor(term.string(ForeignTypeName), [], _) then
        (
            MaybeLanguage = ok1(Language),
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
            % NOTE: if we get here then MaybeForeignLang will be an error and
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
% Parse foreign_decl pragmas.
%

parse_pragma_foreign_decl(VarSet, ErrorTerm, PragmaTerms, Context, SeqNum,
        MaybeIOM) :-
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
            HeaderArgNum = "second",
            MaybeIsLocal = ok1(foreign_decl_is_exported)
        ;
            PragmaTerms = [LangTerm, IsLocalTerm, HeaderTerm],
            HeaderArgNum = "third",
            ( if parse_foreign_decl_is_local(IsLocalTerm, IsLocal0) then
                MaybeIsLocal = ok1(IsLocal0)
            else
                IsLocalStr = describe_error_term(VarSet, IsLocalTerm),
                IsLocalPieces = [words("Error: the second argument"),
                    words("of a"), pragma_decl("foreign_decl"),
                    words("declaration must be either"), quote("local"),
                    words("or"), quote("exported"), suffix(":"),
                    words("got"), quote(IsLocalStr), suffix("."), nl],
                IsLocalSpec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(IsLocalTerm), IsLocalPieces),
                MaybeIsLocal = error1([IsLocalSpec])
            )
        ),
        LangContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl("foreign_decl"), words("declaration:"), nl]),
        parse_foreign_language(LangContextPieces, VarSet, LangTerm, MaybeLang),
        ( if parse_foreign_literal_or_include(HeaderTerm, LitOrIncl0) then
            MaybeLitOrIncl = ok1(LitOrIncl0)
        else
            LitOrInclStr = describe_error_term(VarSet, HeaderTerm),
            LitOrInclPieces = [words("In the"), words(HeaderArgNum),
                words("argument of"), pragma_decl("foreign_decl"),
                words("declaration:"), nl,
                words("error: expected either a string containing code,"),
                words("or a term of the form"), quote("include_file(...)"),
                words("naming a file to include,"),
                words("got"), quote(LitOrInclStr), suffix("."), nl],
            LitOrInclSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree,
                get_term_context(HeaderTerm), LitOrInclPieces),
            MaybeLitOrIncl = error1([LitOrInclSpec])
        ),
        ( if
            MaybeIsLocal = ok1(IsLocal),
            MaybeLang = ok1(Lang),
            MaybeLitOrIncl = ok1(LitOrIncl)
        then
            FDInfo = pragma_info_foreign_decl(Lang, IsLocal, LitOrIncl),
            Pragma = impl_pragma_foreign_decl(FDInfo),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_impl_pragma(ItemPragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybeIsLocal) ++
                get_any_errors1(MaybeLang) ++ get_any_errors1(MaybeLitOrIncl),
            MaybeIOM = error1(Specs)
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

%---------------------------------------------------------------------------%
%
% Parse foreign_code pragmas.
%

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

%---------------------------------------------------------------------------%
%
% Parse foreign_proc pragmas.
%

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
    foreign_language::in, term::in, term::in, term::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

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

%---------------------%

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

%---------------------%

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
    ;       coll_may_duplicate(proc_may_duplicate)
    ;       coll_may_export_body(proc_may_export_body).

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
            coll_may_duplicate(proc_may_not_duplicate),
        coll_may_export_body(proc_may_export_body) -
            coll_may_export_body(proc_may_not_export_body),
        coll_may_duplicate(proc_may_not_duplicate) -
            coll_may_export_body(proc_may_export_body)
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

%---------------------%

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
    else if parse_may_export_body(Term, MayExport) then
        Flag = coll_may_export_body(MayExport)
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

parse_may_duplicate(Term, MayDuplicate) :-
    Term = term.functor(term.atom(Functor), [], _),
    (
        Functor = "may_duplicate",
        MayDuplicate = proc_may_duplicate
    ;
        Functor = "may_not_duplicate",
        MayDuplicate = proc_may_not_duplicate
    ).

:- pred parse_may_export_body(term::in, proc_may_export_body::out) is semidet.

parse_may_export_body(Term, MayExportBody) :-
    Term = term.functor(term.atom(Functor), [], _),
    (
        Functor = "may_export_body",
        MayExportBody = proc_may_export_body
    ;
        Functor = "may_not_export_body",
        MayExportBody = proc_may_not_export_body
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

parse_purity_promise(term.functor(term.atom(Functor), [], _), Purity) :-
    (
        Functor = "promise_pure",
        Purity = purity_pure
    ;
        Functor = "promise_semipure",
        Purity = purity_semipure
    ).

:- pred parse_terminates(term::in, proc_terminates::out) is semidet.

parse_terminates(term.functor(term.atom(Functor), [], _), Terminates) :-
    (
        Functor = "terminates",
        Terminates = proc_terminates
    ;
        Functor = "does_not_terminate",
        Terminates = proc_does_not_terminate
    ).

:- pred parse_no_exception_promise(term::in) is semidet.

parse_no_exception_promise(term.functor(term.atom(Functor), [], _)) :-
    Functor = "will_not_throw_exception".

:- pred parse_ordinary_despite_detism(term::in) is semidet.

parse_ordinary_despite_detism(term.functor(term.atom(Functor), [], _)) :-
    Functor = "ordinary_despite_detism".

%---------------------%

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
process_attribute(coll_may_export_body(MayExport), !Attrs) :-
    set_may_export_body(yes(MayExport), !Attrs).

%---------------------%

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
        ),
        MaybeAttrs = ok1(Attrs)
    ).

%---------------------------------------------------------------------------%
%
% Parse foreign_export pragmas.
%

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
        parse_pred_or_func_and_arg_modes(no, PredAndModesContextPieces, VarSet,
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
            PredNameModesPF = proc_pf_name_modes(PredOrFunc, PredName, Modes),
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
%
% Parse foreign_export_enum pragmas.
%

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
    parse_list_elements("a list of mapping elements",
        parse_sym_name_string_pair, VarSet, OverridesTerm, MaybeOverrides).

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

:- pred parse_export_enum_attr(list(format_component)::in,
    varset::in, term::in, maybe1(collected_export_enum_attribute)::out) is det.

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
% Parse foreign_enum pragmas.
%

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
        parse_list_elements("mapping elements",
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
% Parse foreign_import_module pragmas.
%

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
        MainPieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: expected the name of a foreign language, got"),
            quote(describe_error_term(VarSet, LangTerm)), suffix("."), nl,
            words("The valid languages are")] ++
            list_to_pieces(all_foreign_language_strings) ++ [suffix("."), nl],
        ( if term_to_foreign_language_erlang(LangTerm) then
            Pieces = MainPieces ++
                [words("Support for Erlang has been discontinued."), nl]
        else
            Pieces = MainPieces
        ),
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(LangTerm), Pieces),
        MaybeForeignLang = error1([Spec])
    ).

%---------------------%

:- pred parse_type_ctor_name_arity(cord(format_component)::in, varset::in,
    term::in, maybe1(type_ctor)::out) is det.

parse_type_ctor_name_arity(ContextPieces, VarSet, TypeTerm, MaybeTypeCtor) :-
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
%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

term_to_foreign_language(Term, Lang) :-
    ( Term = term.functor(term.string(String), _, _)
    ; Term = term.functor(term.atom(String), _, _)
    ),
    globals.convert_foreign_language(String, Lang).

term_to_foreign_language_erlang(Term) :-
    ( Term = term.functor(term.string(String), _, _)
    ; Term = term.functor(term.atom(String), _, _)
    ),
    string.to_lower(String) = "erlang".
