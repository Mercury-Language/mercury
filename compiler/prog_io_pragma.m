%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 expandtab
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_pragma.m.
% Main authors: fjh, dgj.
%
% This module handles the parsing of pragma directives.

:- module parse_tree__prog_io_pragma.

:- interface.

:- import_module libs.globals.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_util.

:- import_module list.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Parse the pragma declaration.
    %
:- pred parse_pragma(module_name::in, varset::in, list(term)::in,
    maybe1(item)::out) is semidet.

    % Parse a term that represents a foreign language.
    %
:- pred parse_foreign_language(term::in, foreign_language::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.lp_rational.
:- import_module libs.rat.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

parse_pragma(ModuleName, VarSet, PragmaTerms, Result) :-
    (
        PragmaTerms = [SinglePragmaTerm0],
        parse_type_decl_where_part_if_present(non_solver_type, ModuleName,
            SinglePragmaTerm0, SinglePragmaTerm, WherePartResult),
        SinglePragmaTerm = term__functor(term__atom(PragmaType),
            PragmaArgs, _),
        parse_pragma_type(ModuleName, PragmaType, PragmaArgs, SinglePragmaTerm,
            VarSet, Result0)
    ->
        (
            % The code to process `where' attributes will return an error
            % result if solver attributes are given for a non-solver type.
            % Because this is a non-solver type, if the unification with
            % WhereResult succeeds then _NoSolverTypeDetails is guaranteed to
            % be `no'.
            WherePartResult = ok(_NoSolverTypeDetails, MaybeUserEqComp),
            (
                MaybeUserEqComp = yes(_),
                Result0 = ok(Item0)
            ->
                (
                    Item0 = type_defn(_, _, _, _, _),
                    foreign_type(Type, _, Assertions) = Item0 ^ td_ctor_defn
                ->
                    Result = ok(Item0 ^ td_ctor_defn :=
                        foreign_type(Type, MaybeUserEqComp, Assertions))
                ;
                    Result = error("unexpected `where equality/comparison is'",
                        SinglePragmaTerm0)
                )
            ;
                Result = Result0
            )
        ;
            WherePartResult = error(String, Term),
            Result          = error(String, Term)
        )
    ;
        fail
    ).

:- pred parse_pragma_type(module_name::in, string::in, list(term)::in,
    term::in, varset::in, maybe1(item)::out) is semidet.

parse_pragma_type(_, "source_file", PragmaTerms, ErrorTerm, _VarSet, Result) :-
    ( PragmaTerms = [SourceFileTerm] ->
        (
            SourceFileTerm = term__functor(term__string(SourceFile), [], _)
        ->
            Result = ok(pragma(user, source_file(SourceFile)))
        ;
            Result = error("string expected in `:- pragma " ++
                "source_file' declaration", SourceFileTerm)
        )
    ;
        Result = error("wrong number of arguments in " ++
            "`:- pragma source_file' declaration", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "foreign_type", PragmaTerms, ErrorTerm, VarSet,
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
                MaybeForeignType = ok(ForeignType),
                parse_type_defn_head(ModuleName, MercuryTypeTerm, ErrorTerm,
                    MaybeTypeDefnHead),
                (
                    MaybeTypeDefnHead = ok(MercuryTypeSymName, MercuryParams),
                    varset__coerce(VarSet, TVarSet),
                    (
                        parse_maybe_foreign_type_assertions(MaybeAssertionTerm,
                            Assertions)
                    ->
                            % rafe: XXX I'm not sure that `no' here is right
                            % - we might need some more parsing...
                        Result = ok(type_defn(TVarSet, MercuryTypeSymName,
                            MercuryParams,
                            foreign_type( ForeignType, no, Assertions),
                            true))
                    ;
                        MaybeAssertionTerm = yes(ErrorAssertionTerm)
                    ->
                        Result = error("invalid assertion in " ++
                            "`:- pragma foreign_type' declaration",
                            ErrorAssertionTerm)
                    ;
                        error("parse_pragma_type: unexpected failure of " ++
                            "parse_maybe_foreign_type_assertion")
                    )
                ;
                    MaybeTypeDefnHead = error(String, Term),
                    Result = error(String, Term)
                )
            ;
                MaybeForeignType = error(String, Term),
                Result = error(String, Term)
            )
        ;
            Result = error("invalid foreign language in " ++
                "`:- pragma foreign_type' declaration", LangTerm)
        )
    ;
        Result = error("wrong number of arguments in " ++
            "`:- pragma foreign_type' declaration", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "foreign_decl", PragmaTerms, ErrorTerm,
        VarSet, Result) :-
    parse_pragma_foreign_decl_pragma(ModuleName, "foreign_decl",
        PragmaTerms, ErrorTerm, VarSet, Result).

parse_pragma_type(ModuleName, "c_header_code", PragmaTerms, ErrorTerm,
        VarSet, Result) :-
    (
        PragmaTerms = [term__functor(_, _, Context) | _]
    ->
        LangC = term__functor(term__string("C"), [], Context),
        parse_pragma_foreign_decl_pragma(ModuleName, "c_header_code",
            [LangC | PragmaTerms], ErrorTerm, VarSet, Result)
    ;
        Result = error("wrong number of arguments or unexpected " ++
            "variable in `:- pragma c_header_code' declaration",
            ErrorTerm)
    ).

parse_pragma_type(ModuleName, "foreign_code", PragmaTerms, ErrorTerm,
        VarSet, Result) :-
    parse_pragma_foreign_code_pragma(ModuleName, "foreign_code",
        PragmaTerms, ErrorTerm, VarSet, Result).

parse_pragma_type(ModuleName, "foreign_proc", PragmaTerms, ErrorTerm,
        VarSet, Result) :-
    parse_pragma_foreign_proc_pragma(ModuleName, "foreign_proc",
        PragmaTerms, ErrorTerm, VarSet, Result).

    % pragma c_code is almost as if we have written foreign_code
    % or foreign_proc with the language set to "C".
    % There are a few differences (error messages, some deprecated
    % syntax is still supported for c_code) so we pass the original
    % pragma name to parse_pragma_foreign_code_pragma.
parse_pragma_type(ModuleName, "c_code", PragmaTerms, ErrorTerm,
        VarSet, Result) :-
    (
            % arity = 1 (same as foreign_code)
        PragmaTerms = [term__functor(_, _, Context)]
    ->
        LangC = term__functor(term__string("C"), [], Context),
        parse_pragma_foreign_code_pragma(ModuleName, "c_code",
            [LangC | PragmaTerms], ErrorTerm, VarSet, Result)
    ;
            % arity > 1 (same as foreign_proc)
        PragmaTerms = [term__functor(_, _, Context) | _]
    ->
        LangC = term__functor(term__string("C"), [], Context),
        parse_pragma_foreign_proc_pragma(ModuleName, "c_code",
            [LangC | PragmaTerms], ErrorTerm, VarSet, Result)
    ;
        Result = error("wrong number of arguments or unexpected " ++
            "variable in `:- pragma c_code' declaration", ErrorTerm)
    ).

parse_pragma_type(_ModuleName, "c_import_module", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
    (
        PragmaTerms = [ImportTerm],
        sym_name_and_args(ImportTerm, Import, [])
    ->
        Result = ok(pragma(user, foreign_import_module(c, Import)))
    ;
        Result = error("wrong number of arguments or invalid " ++
            "module name in `:- pragma c_import_module' " ++
            "declaration", ErrorTerm)
    ).

parse_pragma_type(_ModuleName, "foreign_import_module", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
    (
        PragmaTerms = [LangTerm, ImportTerm],
        sym_name_and_args(ImportTerm, Import, [])
    ->
        ( parse_foreign_language(LangTerm, Language) ->
            Result = ok(pragma(user,
                foreign_import_module(Language, Import)))
        ;
            Result = error("invalid foreign language in " ++
                "`:- pragma foreign_import_module' declaration", LangTerm)
        )
    ;
        Result = error("wrong number of arguments or invalid module name " ++
            "in `:- pragma foreign_import_module' declaration", ErrorTerm)
    ).

:- pred parse_foreign_decl_is_local(term::in, foreign_decl_is_local::out)
    is semidet.

parse_foreign_decl_is_local(term__functor(Functor, [], _), IsLocal) :-
    (
        Functor = term__string(String)
    ;
        Functor = term__atom(String)
    ),
    (
        String = "local",
        IsLocal = foreign_decl_is_local
    ;
        String = "exported",
        IsLocal = foreign_decl_is_exported
    ).

parse_foreign_language(term__functor(term__string(String), _, _), Lang) :-
    globals__convert_foreign_language(String, Lang).
parse_foreign_language(term__functor(term__atom(String), _, _), Lang) :-
    globals__convert_foreign_language(String, Lang).

:- pred parse_foreign_language_type(term::in, foreign_language::in,
    maybe1(foreign_language_type)::out) is det.

parse_foreign_language_type(InputTerm, Language, Result) :-
    ( Language = il ->
        ( InputTerm = term__functor(term__string(ILTypeName), [], _) ->
            parse_il_type_name(ILTypeName, InputTerm, Result)
        ;
            Result = error("invalid backend specification term", InputTerm)
        )
    ; Language = c ->
        ( InputTerm = term__functor(term__string(CTypeName), [], _) ->
            Result = ok(c(c(CTypeName)))
        ;
            Result = error("invalid backend specification term", InputTerm)
        )
    ; Language = java ->
        ( InputTerm = term__functor(term__string(JavaTypeName), [], _) ->
            Result = ok(java(java(JavaTypeName)))
        ;
            Result = error("invalid backend specification term", InputTerm)
        )
    ;
        Result = error("unsupported language specified, " ++
            "unable to parse backend type", InputTerm)
    ).

:- pred parse_il_type_name(string::in, term::in,
    maybe1(foreign_language_type)::out) is det.

parse_il_type_name(String0, ErrorTerm, ForeignType) :-
    (
        parse_special_il_type_name(String0, ForeignTypeResult)
    ->
        ForeignType = ok(il(ForeignTypeResult))
    ;
        string__append("class [", String1, String0),
        string__sub_string_search(String1, "]", Index)
    ->
        string__left(String1, Index, AssemblyName),
        string__split(String1, Index + 1, _, TypeNameStr),
        string_to_sym_name(TypeNameStr, ".", TypeSymName),
        ForeignType = ok(il(il(reference, AssemblyName, TypeSymName)))
    ;
        string__append("valuetype [", String1, String0),
        string__sub_string_search(String1, "]", Index)
    ->
        string__left(String1, Index, AssemblyName),
        string__split(String1, Index + 1, _, TypeNameStr),
        string_to_sym_name(TypeNameStr, ".", TypeSymName),
        ForeignType = ok(il(il(value, AssemblyName, TypeSymName)))
    ;
        ForeignType = error("invalid foreign language type description",
            ErrorTerm)
    ).

    % Parse all the special assembler names for all the builtin types.
    % See Partition I 'Built-In Types' (Section 8.2.2) for the list
    % of all builtin types.
    %
:- pred parse_special_il_type_name(string::in, il_foreign_type::out)
    is semidet.

parse_special_il_type_name("bool", il(value, "mscorlib",
        qualified(unqualified("System"), "Boolean"))).
parse_special_il_type_name("char", il(value, "mscorlib",
        qualified(unqualified("System"), "Char"))).
parse_special_il_type_name("object", il(reference, "mscorlib",
        qualified(unqualified("System"), "Object"))).
parse_special_il_type_name("string", il(reference, "mscorlib",
        qualified(unqualified("System"), "String"))).
parse_special_il_type_name("float32", il(value, "mscorlib",
        qualified(unqualified("System"), "Single"))).
parse_special_il_type_name("float64", il(value, "mscorlib",
        qualified(unqualified("System"), "Double"))).
parse_special_il_type_name("int8", il(value, "mscorlib",
        qualified(unqualified("System"), "SByte"))).
parse_special_il_type_name("int16", il(value, "mscorlib",
        qualified(unqualified("System"), "Int16"))).
parse_special_il_type_name("int32", il(value, "mscorlib",
        qualified(unqualified("System"), "Int32"))).
parse_special_il_type_name("int64", il(value, "mscorlib",
        qualified(unqualified("System"), "Int64"))).
parse_special_il_type_name("natural int", il(value, "mscorlib",
        qualified(unqualified("System"), "IntPtr"))).
parse_special_il_type_name("native int", il(value, "mscorlib",
        qualified(unqualified("System"), "IntPtr"))).
parse_special_il_type_name("natural unsigned int", il(value, "mscorlib",
        qualified(unqualified("System"), "UIntPtr"))).
parse_special_il_type_name("native unsigned int", il(value, "mscorlib",
        qualified(unqualified("System"), "UIntPtr"))).
parse_special_il_type_name("refany", il(value, "mscorlib",
        qualified(unqualified("System"), "TypedReference"))).
parse_special_il_type_name("typedref", il(value, "mscorlib",
        qualified(unqualified("System"), "TypedReference"))).
parse_special_il_type_name("unsigned int8", il(value, "mscorlib",
        qualified(unqualified("System"), "Byte"))).
parse_special_il_type_name("unsigned int16", il(value, "mscorlib",
        qualified(unqualified("System"), "UInt16"))).
parse_special_il_type_name("unsigned int32", il(value, "mscorlib",
        qualified(unqualified("System"), "UInt32"))).
parse_special_il_type_name("unsigned int64", il(value, "mscorlib",
        qualified(unqualified("System"), "UInt64"))).

:- pred parse_maybe_foreign_type_assertions(maybe(term)::in,
    list(foreign_type_assertion)::out) is semidet.

parse_maybe_foreign_type_assertions(no, []).
parse_maybe_foreign_type_assertions(yes(Term), Assertions) :-
    parse_foreign_type_assertions(Term, Assertions).

:- pred parse_foreign_type_assertions(term::in,
    list(foreign_type_assertion)::out) is semidet.

parse_foreign_type_assertions(Term, Assertions) :-
    ( Term = term__functor(term__atom("[]"), [], _) ->
        Assertions = []
    ;
        Term = term__functor(term__atom("[|]"), [Head, Tail], _),
        parse_foreign_type_assertion(Head, HeadAssertion),
        parse_foreign_type_assertions(Tail, TailAssertions),
        Assertions = [HeadAssertion | TailAssertions]
    ).

:- pred parse_foreign_type_assertion(term::in,
    foreign_type_assertion::out) is semidet.

parse_foreign_type_assertion(Term, Assertion) :-
    Term = term__functor(term__atom(Constant), [], _),
    Constant = "can_pass_as_mercury_type",
    Assertion = can_pass_as_mercury_type.
parse_foreign_type_assertion(Term, Assertion) :-
    Term = term__functor(term__atom(Constant), [], _),
    Constant = "stable",
    Assertion = stable.

    % This predicate parses both c_header_code and foreign_decl pragmas.
    %
:- pred parse_pragma_foreign_decl_pragma(module_name::in, string::in,
    list(term)::in, term::in, varset::in, maybe1(item)::out) is det.

parse_pragma_foreign_decl_pragma(_ModuleName, Pragma, PragmaTerms,
        ErrorTerm, _VarSet, Result) :-
    string__format("invalid `:- pragma %s' declaration ", [s(Pragma)],
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
            ( HeaderTerm = term__functor(term__string( HeaderCode), [], _) ->
                DeclCode = foreign_decl(ForeignLanguage, IsLocal, HeaderCode),
                Result = ok(pragma(user, DeclCode))
            ;
                ErrMsg = "-- expected string for foreign declaration code",
                Result = error(string__append(InvalidDeclStr, ErrMsg),
                    HeaderTerm)
            )
        ;
            ErrMsg = "-- invalid language parameter",
            Result = error(InvalidDeclStr ++ ErrMsg, LangTerm)
        )
    ;
        string__format("invalid `:- pragma %s' declaration ",
            [s(Pragma)], ErrorStr),
        Result = error(ErrorStr, ErrorTerm)
    ).

    % This predicate parses both c_code and foreign_code pragmas.
    % Processing of foreign_proc (or c_code that defines a procedure)
    % is handled in parse_pragma_foreign_proc_pragma below.
    %
:- pred parse_pragma_foreign_code_pragma(module_name::in, string::in,
    list(term)::in, term::in, varset::in, maybe1(item)::out) is det.

parse_pragma_foreign_code_pragma(_ModuleName, Pragma, PragmaTerms,
        ErrorTerm, _VarSet, Result) :-
    string__format("invalid `:- pragma %s' declaration ", [s(Pragma)],
        InvalidDeclStr),
    Check1 = (func(PTerms1, ForeignLanguage) = Res is semidet :-
        PTerms1 = [Just_Code_Term],
        ( Just_Code_Term = term__functor(term__string(Just_Code), [], _) ->
            Res = ok(pragma(user, foreign_code(ForeignLanguage, Just_Code)))
        ;
            ErrMsg = "-- expected string for foreign code",
            Res = error(string__append(InvalidDeclStr, ErrMsg), ErrorTerm)
        )
    ),
    CheckLength = (func(PTermsLen, ForeignLanguage) = Res :-
        ( Res0 = Check1(PTermsLen, ForeignLanguage) ->
            Res = Res0
        ;
            ErrMsg = "-- wrong number of arguments",
            Res = error(string__append(InvalidDeclStr, ErrMsg), ErrorTerm)
        )
    ),
    CheckLanguage = (func(PTermsLang) = Res is semidet :-
        PTermsLang = [Lang | Rest],
        ( parse_foreign_language(Lang, ForeignLanguage) ->
            Res = CheckLength(Rest, ForeignLanguage)
        ;
            ErrMsg = "-- invalid language parameter",
            Res = error(string__append(InvalidDeclStr, ErrMsg), Lang)
        )
    ),
    ( Result0 = CheckLanguage(PragmaTerms) ->
        Result = Result0
    ;
        ErrMsg0 = "-- wrong number of arguments",
        Result = error(string__append(InvalidDeclStr, ErrMsg0), ErrorTerm)
    ).

    % This predicate parses both c_code and foreign_proc pragmas.
    %
:- pred parse_pragma_foreign_proc_pragma(module_name::in, string::in,
    list(term)::in, term::in, varset::in, maybe1(item)::out) is det.

parse_pragma_foreign_proc_pragma(ModuleName, Pragma, PragmaTerms,
        ErrorTerm, VarSet, Result) :-
    string__format("invalid `:- pragma %s' declaration ", [s(Pragma)],
        InvalidDeclStr),
    Check6 = (func(PTerms6, ForeignLanguage) = Res is semidet :-
        PTerms6 = [PredAndVarsTerm, FlagsTerm, FieldsTerm,
            FirstTerm, LaterTerm, SharedTerm],
        parse_pragma_foreign_proc_attributes_term(ForeignLanguage, Pragma,
            FlagsTerm, MaybeFlags),
        ( MaybeFlags = ok(Flags) ->
            (
                parse_pragma_keyword("local_vars", FieldsTerm, Fields,
                    FieldsContext)
            ->
                (
                    parse_pragma_keyword("first_code", FirstTerm, First,
                        FirstContext)
                ->
                    (
                        parse_pragma_keyword("retry_code", LaterTerm,
                            Later, LaterContext)
                    ->
                        (
                            parse_pragma_keyword("shared_code", SharedTerm,
                                Shared, SharedContext)
                        ->
                            parse_pragma_foreign_code(ModuleName, Flags,
                                PredAndVarsTerm,
                                nondet(Fields, yes(FieldsContext),
                                First, yes(FirstContext),
                                Later, yes(LaterContext),
                                share, Shared, yes(SharedContext)),
                                VarSet, Res)
                        ;
                            parse_pragma_keyword("duplicated_code",
                                SharedTerm, Shared, SharedContext)
                        ->
                            parse_pragma_foreign_code(ModuleName, Flags,
                                PredAndVarsTerm,
                                nondet(Fields, yes(FieldsContext),
                                First, yes(FirstContext),
                                Later, yes(LaterContext),
                                duplicate, Shared, yes(SharedContext)),
                                VarSet, Res)
                        ;
                            parse_pragma_keyword("common_code", SharedTerm,
                                Shared, SharedContext)
                        ->
                            parse_pragma_foreign_code(ModuleName, Flags,
                                PredAndVarsTerm,
                                nondet(Fields, yes(FieldsContext),
                                First, yes(FirstContext),
                                Later, yes(LaterContext),
                                automatic, Shared, yes(SharedContext)),
                                VarSet, Res)
                        ;
                            ErrMsg = "-- invalid seventh argument, "
                                ++ "expecting `common_code(<code>)'",
                            Res = error(string__append(InvalidDeclStr,
                                ErrMsg), SharedTerm)
                        )
                    ;
                        ErrMsg = "-- invalid sixth argument, "
                            ++ "expecting `retry_code(<code>)'",
                        Res = error(string__append(InvalidDeclStr, ErrMsg),
                            LaterTerm)
                    )
                ;
                    ErrMsg = "-- invalid fifth argument, "
                        ++ "expecting `first_code(<code>)'",
                    Res = error(string__append(InvalidDeclStr, ErrMsg),
                        FirstTerm)
                )
            ;
                ErrMsg = "-- invalid fourth argument, "
                    ++ "expecting `local_vars(<fields>)'",
                Res = error(string__append(InvalidDeclStr, ErrMsg),
                    FieldsTerm)
            )
        ;
            MaybeFlags = error(FlagsErrorStr, ErrorTerm),
            ErrMsg = "-- invalid third argument: " ++ FlagsErrorStr,
            Res = error(string__append(InvalidDeclStr, ErrMsg), ErrorTerm)
        )
    ),
    Check5 = (func(PTerms5, ForeignLanguage) = Res is semidet :-
        PTerms5 = [PredAndVarsTerm, FlagsTerm, FieldsTerm,
            FirstTerm, LaterTerm],
        term__context_init(DummyContext),
        SharedTerm = term__functor(term__atom("common_code"),
            [term__functor(term__string(""), [], DummyContext)],
            DummyContext),
        Res = Check6([PredAndVarsTerm, FlagsTerm, FieldsTerm, FirstTerm,
            LaterTerm, SharedTerm], ForeignLanguage)
    ),
    Check3 = (func(PTerms3, ForeignLanguage) = Res is semidet :-
        PTerms3 = [PredAndVarsTerm, FlagsTerm, CodeTerm],
        ( CodeTerm = term__functor(term__string(Code), [], Context) ->
            parse_pragma_foreign_proc_attributes_term(ForeignLanguage,
                Pragma, FlagsTerm, MaybeFlags),
            (
                MaybeFlags = ok(Flags),
                parse_pragma_foreign_code(ModuleName, Flags,
                    PredAndVarsTerm, ordinary(Code, yes(Context)),
                    VarSet, Res)
            ;
                MaybeFlags = error(FlagsErr, FlagsErrTerm),
                parse_pragma_foreign_proc_attributes_term(
                    ForeignLanguage, Pragma, PredAndVarsTerm,
                    MaybeFlags2),
                (
                    MaybeFlags2 = ok(Flags),
                    % XXX we should issue a warning; this syntax is
                    % deprecated We will continue to accept this if
                    % c_code is used, but not with foreign_code
                    ( Pragma = "c_code" ->
                        parse_pragma_foreign_code(ModuleName,
                            Flags, FlagsTerm, ordinary(Code, yes(Context)),
                            VarSet, Res)
                    ;
                        ErrMsg = "-- invalid second argument, "
                            ++ "expecting predicate "
                            ++ "or function mode",
                        Res = error(string__append(InvalidDeclStr, ErrMsg),
                            PredAndVarsTerm)
                    )
                ;
                    MaybeFlags2 = error(_, _),
                    ErrMsg = "-- invalid third argument: ",
                    Res = error(InvalidDeclStr ++ ErrMsg ++ FlagsErr,
                        FlagsErrTerm)
                )
            )
        ;
            ErrMsg = "-- invalid fourth argument, "
                ++ "expecting string containing foreign code",
            Res = error(string__append(InvalidDeclStr, ErrMsg), CodeTerm)
        )
    ),
    Check2 = (func(PTerms2, ForeignLanguage) = Res is semidet :-
        PTerms2 = [PredAndVarsTerm, CodeTerm],
        % XXX we should issue a warning; this syntax is deprecated.
        % We will continue to accept this if c_code is used, but
        % not with foreign_code
        ( Pragma = "c_code" ->
            % may_call_mercury is a conservative default.
            Attributes0 = default_attributes(ForeignLanguage),
            set_legacy_purity_behaviour(yes, Attributes0, Attributes),
            ( CodeTerm = term__functor(term__string(Code), [], Context) ->
                parse_pragma_foreign_code(ModuleName, Attributes,
                    PredAndVarsTerm, ordinary(Code, yes(Context)), VarSet, Res)
            ;
                ErrMsg = "-- expecting either "
                    ++ "`may_call_mercury' or "
                    ++ "`will_not_call_mercury', "
                    ++ "and a string for foreign code",
                Res = error(string__append(InvalidDeclStr, ErrMsg), CodeTerm)
            )
        ;
            ErrMsg = "-- doesn't say whether it can call mercury",
            Res = error(string__append(InvalidDeclStr, ErrMsg), ErrorTerm)
        )
    ),
    CheckLength = (func(PTermsLen, ForeignLanguage) = Res :-
        ( Res0 = Check2(PTermsLen, ForeignLanguage) ->
            Res = Res0
        ; Res0 = Check3(PTermsLen, ForeignLanguage) ->
            Res = Res0
        ; Res0 = Check5(PTermsLen, ForeignLanguage) ->
            Res = Res0
        ; Res0 = Check6(PTermsLen, ForeignLanguage) ->
            Res = Res0
        ;
            ErrMsg = "-- wrong number of arguments",
            Res = error(string__append(InvalidDeclStr, ErrMsg), ErrorTerm)
        )
    ),
    CheckLanguage = (func(PTermsLang) = Res is semidet :-
        PTermsLang = [Lang | Rest],
        ( parse_foreign_language(Lang, ForeignLanguage) ->
            Res = CheckLength(Rest, ForeignLanguage)
        ;
            ErrMsg = "-- invalid language parameter",
            Res = error(string__append(InvalidDeclStr, ErrMsg), Lang)
        )
    ),
    ( Result0 = CheckLanguage(PragmaTerms) ->
        Result = Result0
    ;
        ErrMsg0 = "-- wrong number of arguments",
        Result = error(string__append(InvalidDeclStr, ErrMsg0), ErrorTerm)
    ).

parse_pragma_type(ModuleName, "import", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
        % XXX we assume all imports are C
    ForeignLanguage = c,
    (
        (
            PragmaTerms = [PredAndModesTerm, FlagsTerm, FunctionTerm],
            parse_pragma_foreign_proc_attributes_term(ForeignLanguage,
                "import", FlagsTerm, MaybeFlags),
            (
                MaybeFlags = error(FlagError, ErrorTerm),
                FlagsResult = error("invalid second argument in "
                    ++ "`:- pragma import/3' declaration : "
                    ++ FlagError, ErrorTerm)
            ;
                MaybeFlags = ok(Flags),
                FlagsResult = ok(Flags)
            )
        ;
            PragmaTerms = [PredAndModesTerm, FunctionTerm],
            Flags0 = default_attributes(ForeignLanguage),
                % pragma import uses legacy purity behaviour
            set_legacy_purity_behaviour(yes, Flags0, Flags),
            FlagsResult = ok(Flags)
        )
    ->
        ( FunctionTerm = term__functor(term__string(Function), [], _) ->
            parse_pred_or_func_and_arg_modes(yes(ModuleName),
                PredAndModesTerm, ErrorTerm, "`:- pragma import' declaration",
                PredAndArgModesResult),
            (
                PredAndArgModesResult = ok(PredName - PredOrFunc, ArgModes),
                (
                    FlagsResult = ok(Attributes),
                    Result = ok(pragma(user, import(PredName, PredOrFunc,
                        ArgModes, Attributes, Function)))
                ;
                    FlagsResult = error(Msg, Term),
                    Result = error(Msg, Term)
                )
            ;
                PredAndArgModesResult = error(Msg, Term),
                Result = error(Msg, Term)
            )
        ;
            Result = error("expected pragma import(PredName(ModeList), "
                ++ "Function)", PredAndModesTerm)
        )
    ;
        Result = error("wrong number of arguments in " ++
            "`:- pragma import' declaration", ErrorTerm)
    ).

parse_pragma_type(_ModuleName, "export", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    % XXX we implicitly assume exports are only for C
    ( PragmaTerms = [PredAndModesTerm, FunctionTerm] ->
        ( FunctionTerm = term__functor(term__string(Function), [], _) ->
            parse_pred_or_func_and_arg_modes(no, PredAndModesTerm,
                ErrorTerm, "`:- pragma export' declaration",
                PredAndModesResult),
            (
                PredAndModesResult = ok(PredName - PredOrFunc, Modes),
                Result = ok(pragma(user, export(PredName, PredOrFunc, Modes,
                    Function)))
            ;
                PredAndModesResult = error(Msg, Term),
                Result = error(Msg, Term)
            )
        ;
            Result = error(
                 "expected pragma export(PredName(ModeList), Function)",
                 PredAndModesTerm)
        )
    ;
        Result = error("wrong number of arguments in " ++
            "`:- pragma export' declaration", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "inline", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    parse_simple_pragma(ModuleName, "inline",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = inline(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "no_inline", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    parse_simple_pragma(ModuleName, "no_inline",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = no_inline(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "memo", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    % The eval_memo(all_strict) could be converted to eval_memo(specified(_))
    % if the pragma specifies the ways to table the arguments.
    parse_tabling_pragma(ModuleName, "memo",
        eval_memo(all_strict), PragmaTerms, ErrorTerm, Result).
parse_pragma_type(ModuleName, "fast_loose_memo", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
    parse_tabling_pragma(ModuleName, "fast_loose_memo",
        eval_memo(all_fast_loose), PragmaTerms, ErrorTerm, Result).
parse_pragma_type(ModuleName, "loop_check", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    parse_tabling_pragma(ModuleName, "loop_check", eval_loop_check,
        PragmaTerms, ErrorTerm, Result).
parse_pragma_type(ModuleName, "minimal_model", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    % We don't yet know whether we will use the stack_copy or the
    % own_stacks technique for computing minimal models. The decision
    % depends on the grade, and is made in make_hlds.m; the stack_copy here
    % is just a placeholder.
    parse_tabling_pragma(ModuleName, "minimal_model",
        eval_minimal(stack_copy), PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "obsolete", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    parse_simple_pragma(ModuleName, "obsolete",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = obsolete(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

    % pragma unused_args should never appear in user programs,
    % only in .opt files.
parse_pragma_type(ModuleName, "unused_args", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    (
        PragmaTerms = [
            PredOrFuncTerm,
            PredNameTerm,
            term__functor(term__integer(Arity), [], _),
            term__functor(term__integer(ModeNum), [], _),
            UnusedArgsTerm
        ],
        (
            PredOrFuncTerm = term__functor(term__atom("predicate"), [], _),
            PredOrFunc = predicate
        ;
            PredOrFuncTerm = term__functor(term__atom("function"), [], _),
            PredOrFunc = function
        ),
        parse_implicitly_qualified_term(ModuleName, PredNameTerm, ErrorTerm,
            "`:- pragma unused_args' declaration", PredNameResult),
        PredNameResult = ok(PredName, []),
        convert_int_list(UnusedArgsTerm, UnusedArgsResult),
        UnusedArgsResult = ok(UnusedArgs)
    ->
        Result = ok(pragma(user, unused_args(PredOrFunc, PredName, Arity,
            ModeNum, UnusedArgs)))
    ;
        Result = error("error in `:- pragma unused_args'", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "type_spec", PragmaTerms, ErrorTerm, VarSet0,
        Result) :-
    (
        (
            PragmaTerms = [PredAndModesTerm, TypeSubnTerm],
            MaybeName = no
        ;
            PragmaTerms = [PredAndModesTerm, TypeSubnTerm, SpecNameTerm],
            SpecNameTerm = term__functor(_, _, SpecContext),

            % This form of the pragma should not appear in source files.
            term__context_file(SpecContext, FileName),
            \+ string__remove_suffix(FileName, ".m", _),

            parse_implicitly_qualified_term(ModuleName,
                SpecNameTerm, ErrorTerm, "", NameResult),
            NameResult = ok(SpecName, []),
            MaybeName = yes(SpecName)
        )
    ->
        parse_arity_or_modes(ModuleName, PredAndModesTerm, ErrorTerm,
            "`:- pragma type_spec' declaration",
            ArityOrModesResult),
        (
            ArityOrModesResult = ok(arity_or_modes(PredName, Arity,
                MaybePredOrFunc, MaybeModes)),
            conjunction_to_list(TypeSubnTerm, TypeSubnList),

            % The varset is actually a tvarset.
            varset__coerce(VarSet0, TVarSet),
            ( list__map(convert_type_spec_pair, TypeSubnList, TypeSubn) ->
                (
                    MaybeName = yes(SpecializedName0),
                    SpecializedName = SpecializedName0
                ;
                    MaybeName = no,
                    unqualify_name(PredName, UnqualName),
                    make_pred_name(ModuleName, "TypeSpecOf", MaybePredOrFunc,
                        UnqualName, type_subst(TVarSet, TypeSubn),
                        SpecializedName)
                ),
                TypeSpecPragma = type_spec(PredName, SpecializedName, Arity,
                    MaybePredOrFunc, MaybeModes, TypeSubn, TVarSet,
                    set__init),
                Result = ok(pragma(user, TypeSpecPragma))
            ;
                Result = error("expected type substitution in " ++
                    "`:- pragma type_spec' declaration", TypeSubnTerm)
            )
        ;
            ArityOrModesResult = error(Msg, Term),
            Result = error(Msg, Term)
        )
    ;
        Result = error("wrong number of arguments in " ++
            "`:- pragma type_spec' declaration", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "reserve_tag", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    parse_simple_type_pragma(ModuleName, "reserve_tag",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = reserve_tag(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "fact_table", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    ( PragmaTerms = [PredAndArityTerm, FileNameTerm] ->
        parse_pred_name_and_arity(ModuleName, "fact_table",
        PredAndArityTerm, ErrorTerm, NameArityResult),
        (
            NameArityResult = ok(PredName, Arity),
            ( FileNameTerm = term__functor(term__string(FileName), [], _) ->
                Result = ok(pragma(user,
                    fact_table(PredName, Arity, FileName)))
            ;
                Result = error("expected string for fact table filename",
                    FileNameTerm)
            )
        ;
            NameArityResult = error(ErrorMsg, _),
            Result = error(ErrorMsg, PredAndArityTerm)
        )
    ;
        Result = error("wrong number of arguments in " ++
            "`:- pragma fact_table' declaration", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "aditi", PragmaTerms, ErrorTerm, _, Result) :-
    parse_simple_pragma(ModuleName, "aditi",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = aditi(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "base_relation", PragmaTerms, ErrorTerm, _,
        Result) :-
    parse_simple_pragma(ModuleName, "base_relation",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = base_relation(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "aditi_index", PragmaTerms, ErrorTerm, _,
        Result) :-
    ( PragmaTerms = [PredNameArityTerm, IndexTypeTerm, AttributesTerm] ->
        parse_pred_name_and_arity(ModuleName, "aditi_index",
        PredNameArityTerm, ErrorTerm, NameArityResult),
        (
            NameArityResult = ok(PredName, PredArity),
            (
                IndexTypeTerm = term__functor(term__atom(IndexTypeStr), [], _),
                (
                    IndexTypeStr = "unique_B_tree",
                    IndexType = unique_B_tree
                ;
                    IndexTypeStr = "non_unique_B_tree",
                    IndexType = non_unique_B_tree
                )
            ->
                convert_int_list(AttributesTerm, AttributeResult),
                (
                    AttributeResult = ok(Attributes),
                    Result = ok(pragma(user, aditi_index(PredName, PredArity,
                        index_spec(IndexType, Attributes))))
                ;
                    AttributeResult = error(_, AttrErrorTerm),
                    Result = error("expected attribute list for " ++
                        "`:- pragma aditi_index' declaration", AttrErrorTerm)
                )
            ;
                Result = error("expected index type for " ++
                    "`:- pragma aditi_index' declaration", IndexTypeTerm)
                )
        ;
            NameArityResult = error(NameErrorMsg, NameErrorTerm),
            Result = error(NameErrorMsg, NameErrorTerm)
        )
    ;
        Result = error("wrong number of arguments in " ++
            "`:- pragma aditi_index' declaration", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "naive", PragmaTerms, ErrorTerm, _, Result) :-
    parse_simple_pragma(ModuleName, "naive",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = naive(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "psn", PragmaTerms, ErrorTerm, _, Result) :-
    parse_simple_pragma(ModuleName, "psn",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = psn(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "aditi_memo", PragmaTerms, ErrorTerm, _,
        Result) :-
    parse_simple_pragma(ModuleName, "aditi_memo",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = aditi_memo(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "aditi_no_memo", PragmaTerms, ErrorTerm, _,
        Result) :-
    parse_simple_pragma(ModuleName, "aditi_no_memo",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = aditi_no_memo(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "supp_magic", PragmaTerms, ErrorTerm, _,
        Result) :-
    parse_simple_pragma(ModuleName, "supp_magic",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = supp_magic(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "context", PragmaTerms, ErrorTerm, _, Result) :-
    parse_simple_pragma(ModuleName, "context",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = context(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "owner", PragmaTerms, ErrorTerm, _, Result) :-
    ( PragmaTerms = [SymNameAndArityTerm, OwnerTerm] ->
        ( OwnerTerm = term__functor(term__atom(Owner), [], _) ->
            parse_simple_pragma(ModuleName, "owner",
                (pred(Name::in, Arity::in, Pragma::out) is det :-
                    Pragma = owner(Name, Arity, Owner)),
                [SymNameAndArityTerm], ErrorTerm, Result)
        ;
            ErrorMsg = "expected owner name for `:- pragma owner' declaration",
            Result = error(ErrorMsg, OwnerTerm)
        )
    ;
        ErrorMsg = "wrong number of arguments in " ++
            "`:- pragma owner' declaration",
        Result = error(ErrorMsg, ErrorTerm)
    ).

parse_pragma_type(ModuleName, "promise_pure", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    parse_simple_pragma(ModuleName, "promise_pure",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = promise_pure(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "promise_semipure", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
    parse_simple_pragma(ModuleName, "promise_semipure",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = promise_semipure(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "termination_info", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
    (
        PragmaTerms = [
            PredAndModesTerm0,
            ArgSizeTerm,
            TerminationTerm
        ],
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            ErrorTerm, "`:- pragma termination_info' declaration",
            NameAndModesResult),
        NameAndModesResult = ok(PredName - PredOrFunc, ModeList),
        (
            ArgSizeTerm = term__functor(term__atom("not_set"), [], _),
            MaybeArgSizeInfo = no
        ;
            ArgSizeTerm = term__functor(term__atom("infinite"), [], _),
            MaybeArgSizeInfo = yes(infinite(unit))
        ;
            ArgSizeTerm = term__functor(term__atom("finite"),
                [IntTerm, UsedArgsTerm], _),
            IntTerm = term__functor(term__integer(Int), [], _),
            convert_bool_list(UsedArgsTerm, UsedArgs),
            MaybeArgSizeInfo = yes(finite(Int, UsedArgs))
        ),
        (
            TerminationTerm = term__functor(term__atom("not_set"), [], _),
            MaybeTerminationInfo = no
        ;
            TerminationTerm = term__functor(term__atom("can_loop"), [], _),
            MaybeTerminationInfo = yes(can_loop(unit))
        ;
            TerminationTerm = term__functor(term__atom("cannot_loop"), [], _),
            MaybeTerminationInfo = yes(cannot_loop(unit))
        ),
        Result0 = ok(pragma(user, termination_info(PredOrFunc, PredName,
            ModeList, MaybeArgSizeInfo, MaybeTerminationInfo)))
    ->
        Result = Result0
    ;
        Result = error("syntax error in `:- pragma termination_info' " ++
            "declaration", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "termination2_info", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
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
        NameAndModesResult = ok(PredName - PredOrFunc, ModeList),
        parse_arg_size_constraints(SuccessArgSizeTerm, SuccessArgSizeResult),
        SuccessArgSizeResult = ok(SuccessArgSizeInfo),
        parse_arg_size_constraints(FailureArgSizeTerm, FailureArgSizeResult),
        FailureArgSizeResult = ok(FailureArgSizeInfo),
        (
            TerminationTerm = term__functor(term__atom("not_set"), [], _),
            MaybeTerminationInfo = no
        ;
            TerminationTerm = term__functor(term__atom("can_loop"), [], _),
            MaybeTerminationInfo = yes(can_loop(unit))
        ;
            TerminationTerm = term__functor(term__atom("cannot_loop"), [], _),
            MaybeTerminationInfo = yes(cannot_loop(unit))
        ),
        Result0 = ok(pragma(user, termination2_info(PredOrFunc, PredName,
            ModeList, SuccessArgSizeInfo, FailureArgSizeInfo,
            MaybeTerminationInfo)))
    ->
        Result = Result0
    ;
        Result = error(
            "syntax error in `:- pragma termination2_info' declaration",
        ErrorTerm)
    ).

parse_pragma_type(ModuleName, "terminates", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
    parse_simple_pragma(ModuleName, "terminates",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = terminates(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "does_not_terminate", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
    parse_simple_pragma(ModuleName, "does_not_terminate",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = does_not_terminate(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "check_termination", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
    parse_simple_pragma(ModuleName, "check_termination",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = check_termination(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "exceptions", PragmaTerms, ErrorTerm, _VarSet,
        Result) :-
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
            PredOrFunc = predicate
        ;
            PredOrFuncTerm = term.functor(term.atom("function"), [], _),
            PredOrFunc = function
        ),
        parse_implicitly_qualified_term(ModuleName, PredNameTerm,
            ErrorTerm, "`:- pragma exceptions' declaration",
            PredNameResult),
        PredNameResult = ok(PredName, []),
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
            ThrowStatus = conditional
        )
    ->
        Result = ok(pragma(user, exceptions(PredOrFunc, PredName,
            Arity, ModeNum, ThrowStatus)))
    ;
        Result = error("error in `:- pragma exceptions'", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "trailing_info", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
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
            PredOrFunc = predicate
        ;
            PredOrFuncTerm = term.functor(term.atom("function"), [], _),
            PredOrFunc = function
        ),
        parse_implicitly_qualified_term(ModuleName, PredNameTerm,
            ErrorTerm, "`:- pragma trailing_info' declaration",
            PredNameResult),
        PredNameResult = ok(PredName, []),
        (
            TrailingStatusTerm = term.functor(
                term.atom("will_not_modify_trail"), [], _),
            TrailingStatus = will_not_modify_trail
        ;
            TrailingStatusTerm = term.functor(
                term.atom("may_modify_trail"), [], _),
            TrailingStatus = may_modify_trail
        ;
            TrailingStatusTerm = term.functor(
                term.atom("conditional"), [], _),
            TrailingStatus = conditional
        )
    ->
        Result = ok(pragma(user, trailing_info(PredOrFunc, PredName,
            Arity, ModeNum, TrailingStatus)))
    ;
        Result = error("error in `:- pragma trailing_info'", ErrorTerm)
    ).

parse_pragma_type(ModuleName, "mode_check_clauses", PragmaTerms, ErrorTerm,
        _VarSet, Result) :-
    parse_simple_pragma(ModuleName, "mode_check_clauses",
        (pred(Name::in, Arity::in, Pragma::out) is det :-
            Pragma = mode_check_clauses(Name, Arity)),
        PragmaTerms, ErrorTerm, Result).

    % This parses a pragma that refers to a predicate or function.
    %
:- pred parse_simple_pragma(module_name::in, string::in,
    pred(sym_name, int, pragma_type)::(pred(in, in, out) is det),
    list(term)::in, term::in, maybe1(item)::out) is det.

parse_simple_pragma(ModuleName, PragmaType, MakePragma, PragmaTerms, ErrorTerm,
        Result) :-
    parse_simple_pragma_base(ModuleName, PragmaType,
        "predicate or function", MakePragma, PragmaTerms, ErrorTerm, Result).

    % This parses a pragma that refers to type.
    %
:- pred parse_simple_type_pragma(module_name::in, string::in,
    pred(sym_name, int, pragma_type)::(pred(in, in, out) is det),
    list(term)::in, term::in, maybe1(item)::out) is det.

parse_simple_type_pragma(ModuleName, PragmaType, MakePragma,
        PragmaTerms, ErrorTerm, Result) :-
    parse_simple_pragma_base(ModuleName, PragmaType, "type", MakePragma,
        PragmaTerms, ErrorTerm, Result).

    % This parses a pragma that refers to symbol name / arity.
    %
:- pred parse_simple_pragma_base(module_name::in, string::in, string::in,
    pred(sym_name, int, pragma_type)::(pred(in, in, out) is det),
    list(term)::in, term::in, maybe1(item)::out) is det.

parse_simple_pragma_base(ModuleName, PragmaType, NameKind, MakePragma,
        PragmaTerms, ErrorTerm, Result) :-
    ( PragmaTerms = [PredAndArityTerm] ->
        parse_simple_name_and_arity(ModuleName, PragmaType, NameKind,
            PredAndArityTerm, ErrorTerm, NameArityResult),
        (
            NameArityResult = ok(PredName, Arity),
            call(MakePragma, PredName, Arity, Pragma),
            Result = ok(pragma(user, Pragma))
        ;
            NameArityResult = error(ErrorMsg, _),
            Result = error(ErrorMsg, PredAndArityTerm)
        )
    ;
        string__append_list(["wrong number of arguments in `:- pragma ",
            PragmaType, "' declaration"], ErrorMsg),
        Result = error(ErrorMsg, ErrorTerm)
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
        Result = ok(Name, Arity)
    ;
        string__append_list(["expected ", NameKind, " name/arity for `pragma ",
            PragmaType, "' declaration"], ErrorMsg),
        Result = error(ErrorMsg, ErrorTerm)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_pragma_keyword(string::in, term::in, string::out,
    term__context::out) is semidet.

parse_pragma_keyword(ExpectedKeyword, Term, StringArg, StartContext) :-
    Term = term__functor(term__atom(ExpectedKeyword), [Arg], _),
    Arg = term__functor(term__string(StringArg), [], StartContext).

%-----------------------------------------------------------------------------%

:- type collected_pragma_foreign_proc_attribute
    --->    may_call_mercury(may_call_mercury)
    ;       thread_safe(thread_safe)
    ;       tabled_for_io(tabled_for_io)
    ;       purity(purity)
    ;       aliasing
    ;       max_stack_size(int)
    ;       backend(backend)
    ;       terminates(terminates)
    ;       will_not_throw_exception
    ;       ordinary_despite_detism
    ;       may_modify_trail(may_modify_trail).

:- pred parse_pragma_foreign_proc_attributes_term(foreign_language::in,
    string::in, term::in, maybe1(pragma_foreign_proc_attributes)::out)
    is det.

parse_pragma_foreign_proc_attributes_term(ForeignLanguage, Pragma, Term,
        MaybeAttributes) :-
    Attributes0 = default_attributes(ForeignLanguage),
    ( ( Pragma = "c_code" ; Pragma = "import" ) ->
        set_legacy_purity_behaviour(yes, Attributes0, Attributes1),
        set_purity(purity_pure, Attributes1, Attributes2)
    ;
        Attributes2 = Attributes0
    ),
    ConflictingAttributes = [
        may_call_mercury(will_not_call_mercury) -
            may_call_mercury(may_call_mercury),
        thread_safe(thread_safe) - thread_safe(not_thread_safe),
        tabled_for_io(tabled_for_io) -
            tabled_for_io(tabled_for_io_unitize),
        tabled_for_io(tabled_for_io) -
            tabled_for_io(tabled_for_descendant_io),
        tabled_for_io(tabled_for_io) -
            tabled_for_io(not_tabled_for_io),
        tabled_for_io(tabled_for_io_unitize) -
            tabled_for_io(tabled_for_descendant_io),
        tabled_for_io(tabled_for_io_unitize) -
            tabled_for_io(not_tabled_for_io),
        tabled_for_io(tabled_for_descendant_io) -
            tabled_for_io(not_tabled_for_io),
        purity(purity_pure) - purity(purity_impure),
        purity(purity_pure) - purity(purity_semipure),
        purity(purity_semipure) - purity(purity_impure),
        terminates(terminates) - terminates(does_not_terminate),
        terminates(depends_on_mercury_calls) - terminates(terminates),
        terminates(depends_on_mercury_calls) - terminates(does_not_terminate),
        may_modify_trail(may_modify_trail) -
            may_modify_trail(will_not_modify_trail)
    ],
    (
        parse_pragma_foreign_proc_attributes_term0(Term, AttrList)
    ->
        (
            list__member(Conflict1 - Conflict2, ConflictingAttributes),
            list__member(Conflict1, AttrList),
            list__member(Conflict2, AttrList)
        ->
            MaybeAttributes = error("conflicting attributes " ++
                "in attribute list", Term)
        ;
            list__foldl(process_attribute, AttrList, Attributes2, Attributes),
            MaybeAttributes = check_required_attributes(ForeignLanguage,
                Attributes, Term)
        )
    ;
        ErrMsg = "expecting a foreign proc attribute or list of attributes",
        MaybeAttributes = error(ErrMsg, Term)
    ).

    % Update the pragma_foreign_proc_attributes according to the given
    % collected_pragma_foreign_proc_attribute.
    %
:- pred process_attribute(collected_pragma_foreign_proc_attribute::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

process_attribute(may_call_mercury(MayCallMercury), !Attrs) :-
    set_may_call_mercury(MayCallMercury, !Attrs).
process_attribute(thread_safe(ThreadSafe), !Attrs) :-
    set_thread_safe(ThreadSafe, !Attrs).
process_attribute(tabled_for_io(TabledForIO), !Attrs) :-
    set_tabled_for_io(TabledForIO, !Attrs).
process_attribute(purity(Pure), !Attrs) :-
    set_purity(Pure, !Attrs).
process_attribute(terminates(Terminates), !Attrs) :-
    set_terminates(Terminates, !Attrs).
process_attribute(will_not_throw_exception, !Attrs) :-
    set_may_throw_exception(will_not_throw_exception, !Attrs).
process_attribute(max_stack_size(Size), !Attrs) :-
    add_extra_attribute(max_stack_size(Size), !Attrs).
process_attribute(backend(Backend), !Attrs) :-
    add_extra_attribute(backend(Backend), !Attrs).
process_attribute(ordinary_despite_detism, !Attrs) :-
    set_ordinary_despite_detism(yes, !Attrs).
process_attribute(may_modify_trail(TrailMod), !Attrs) :-
    set_may_modify_trail(TrailMod, !Attrs).

    % Aliasing is currently ignored in the main branch compiler.
    %
process_attribute(aliasing, Attrs, Attrs).

    % Check whether all the required attributes have been set for
    % a particular language
    %
:- func check_required_attributes(foreign_language,
        pragma_foreign_proc_attributes, term)
    = maybe1(pragma_foreign_proc_attributes).

check_required_attributes(c, Attrs, _Term) = ok(Attrs).
check_required_attributes(managed_cplusplus, Attrs, _Term) = ok(Attrs).
check_required_attributes(csharp, Attrs, _Term) = ok(Attrs).
check_required_attributes(il, Attrs, Term) = Res :-
    MaxStackAttrs = list__filter_map(
        (func(X) = X is semidet :- X = max_stack_size(_)),
        Attrs ^ extra_attributes),
    (
        MaxStackAttrs = [],
        Res = error("expecting max_stack_size attribute for IL code", Term)
    ;
        MaxStackAttrs = [_ | _],
        Res = ok(Attrs)
    ).
check_required_attributes(java, Attrs, _Term) = ok(Attrs).

:- pred parse_pragma_foreign_proc_attributes_term0(term::in,
    list(collected_pragma_foreign_proc_attribute)::out) is semidet.

parse_pragma_foreign_proc_attributes_term0(Term, Flags) :-
    ( parse_single_pragma_foreign_proc_attribute(Term, Flag) ->
        Flags = [Flag]
    ;
        (
            Term = term__functor(term__atom("[]"), [], _),
            Flags = []
        ;
            Term = term__functor(term__atom("[|]"), [Head, Tail], _),
            parse_single_pragma_foreign_proc_attribute(Head, HeadFlag),
            parse_pragma_foreign_proc_attributes_term0(Tail, TailFlags),
            Flags = [HeadFlag | TailFlags]
        )
    ).

:- pred parse_single_pragma_foreign_proc_attribute(term::in,
    collected_pragma_foreign_proc_attribute::out) is semidet.

parse_single_pragma_foreign_proc_attribute(Term, Flag) :-
    ( parse_may_call_mercury(Term, MayCallMercury) ->
        Flag = may_call_mercury(MayCallMercury)
    ; parse_threadsafe(Term, ThreadSafe) ->
        Flag = thread_safe(ThreadSafe)
    ; parse_tabled_for_io(Term, TabledForIo) ->
        Flag = tabled_for_io(TabledForIo)
    ; parse_aliasing(Term) ->
        Flag = aliasing
    ; parse_max_stack_size(Term, Size) ->
        Flag = max_stack_size(Size)
    ; parse_backend(Term, Backend) ->
        Flag = backend(Backend)
    ; parse_purity_promise(Term, Purity) ->
        Flag = purity(Purity)
    ; parse_terminates(Term, Terminates) ->
        Flag = terminates(Terminates)
    ; parse_no_exception_promise(Term) ->
        Flag = will_not_throw_exception
    ; parse_ordinary_despite_detism(Term) ->
        Flag = ordinary_despite_detism
    ; parse_may_modify_trail(Term, TrailMod) ->
        Flag = may_modify_trail(TrailMod)
    ; 
        fail
    ).

:- pred parse_may_call_mercury(term::in, may_call_mercury::out) is semidet.

parse_may_call_mercury(term__functor(term__atom("recursive"), [], _),
    may_call_mercury).
parse_may_call_mercury(term__functor(term__atom("non_recursive"), [], _),
    will_not_call_mercury).
parse_may_call_mercury(term__functor(term__atom("may_call_mercury"), [], _),
    may_call_mercury).
parse_may_call_mercury(term__functor(term__atom("will_not_call_mercury"), [],
    _), will_not_call_mercury).

:- pred parse_threadsafe(term::in, thread_safe::out) is semidet.

parse_threadsafe(term__functor(term__atom("thread_safe"), [], _),
    thread_safe).
parse_threadsafe(term__functor(term__atom("not_thread_safe"), [], _),
    not_thread_safe).
parse_threadsafe(term__functor(term__atom("maybe_thread_safe"), [], _),
    maybe_thread_safe).
    
:- pred parse_may_modify_trail(term::in, may_modify_trail::out) is semidet.

parse_may_modify_trail(term.functor(term.atom("may_modify_trail"), [], _),
    may_modify_trail).
parse_may_modify_trail(term.functor(term.atom("will_not_modify_trail"), [], _),
    will_not_modify_trail).

:- pred parse_tabled_for_io(term::in, tabled_for_io::out) is semidet.

parse_tabled_for_io(term__functor(term__atom(Str), [], _), TabledForIo) :-
    (
        Str = "tabled_for_io",
        TabledForIo = tabled_for_io
    ;
        Str = "tabled_for_io_unitize",
        TabledForIo = tabled_for_io_unitize
    ;
        Str = "tabled_for_descendant_io",
        TabledForIo = tabled_for_descendant_io
    ;
        Str = "not_tabled_for_io",
        TabledForIo = not_tabled_for_io
    ).

    % XXX For the moment we just ignore the following attributes.
    % These attributes are used for aliasing on the reuse branch,
    % and ignoring them allows the main branch compiler to compile
    % the reuse branch.
    %
:- pred parse_aliasing(term::in) is semidet.

parse_aliasing(term__functor(term__atom("no_aliasing"), [], _)).
parse_aliasing(term__functor(term__atom("unknown_aliasing"), [], _)).
parse_aliasing(term__functor(term__atom("alias"), [_Types, _Alias], _)).

:- pred parse_max_stack_size(term::in, int::out) is semidet.

parse_max_stack_size(term__functor(
        term__atom("max_stack_size"), [SizeTerm], _), Size) :-
    SizeTerm = term__functor(term__integer(Size), [], _).

:- pred parse_backend(term::in, backend::out) is semidet.

parse_backend(term__functor(term__atom(Functor), [], _), Backend) :-
    (
        Functor = "high_level_backend",
        Backend = high_level_backend
    ;
        Functor = "low_level_backend",
        Backend = low_level_backend
    ).

:- pred parse_purity_promise(term::in, purity::out) is semidet.

parse_purity_promise(term__functor(term__atom("promise_pure"), [], _),
        purity_pure).
parse_purity_promise(term__functor(term__atom("promise_semipure"), [], _),
        purity_semipure).

:- pred parse_terminates(term::in, terminates::out) is semidet.

parse_terminates(term__functor(term__atom("terminates"), [], _),
        terminates).
parse_terminates(term__functor(term__atom("does_not_terminate"), [], _),
        does_not_terminate).

:- pred parse_no_exception_promise(term::in) is semidet.

parse_no_exception_promise(term.functor(
    term.atom("will_not_throw_exception"), [], _)).

:- pred parse_ordinary_despite_detism(term::in) is semidet.

parse_ordinary_despite_detism(
        term__functor(term__atom("ordinary_despite_detism"), [], _)).

    % Parse a pragma foreign_code declaration.
    %
:- pred parse_pragma_foreign_code(module_name::in,
    pragma_foreign_proc_attributes::in, term::in,
    pragma_foreign_code_impl::in, varset::in, maybe1(item)::out) is det.

parse_pragma_foreign_code(ModuleName, Flags, PredAndVarsTerm0,
        PragmaImpl, VarSet0, Result) :-
    parse_pred_or_func_and_args(yes(ModuleName), PredAndVarsTerm0,
        PredAndVarsTerm0, "`:- pragma c_code' declaration", PredAndArgsResult),
    (
        PredAndArgsResult = ok(PredName, VarList0 - MaybeRetTerm),
        (
            % Is this a function or a predicate?
            MaybeRetTerm = yes(FuncResultTerm0)
        ->
            PredOrFunc = function,
            list__append(VarList0, [FuncResultTerm0], VarList)
        ;
            PredOrFunc = predicate,
            VarList = VarList0
        ),
        parse_pragma_c_code_varlist(VarSet0, VarList, PragmaVars, Error),
        (
            Error = no,
            varset__coerce(VarSet0, ProgVarSet),
            varset__coerce(VarSet0, InstVarSet),
            Result = ok(pragma(user, foreign_proc(Flags, PredName, PredOrFunc,
                PragmaVars, ProgVarSet, InstVarSet, PragmaImpl)))
        ;
            Error = yes(ErrorMessage),
            Result = error(ErrorMessage, PredAndVarsTerm0)
        )
    ;
        PredAndArgsResult = error(Msg, Term),
        Result = error(Msg, Term)
    ).

    % Parse the variable list in the pragma c code declaration.
    % The final argument is 'no' for no error, or 'yes(ErrorMessage)'.
    %
:- pred parse_pragma_c_code_varlist(varset::in, list(term)::in,
    list(pragma_var)::out, maybe(string)::out) is det.

parse_pragma_c_code_varlist(_, [], [], no).
parse_pragma_c_code_varlist(VarSet, [V|Vars], PragmaVars, Error):-
    (
        V = term__functor(term__atom("::"), [VarTerm, ModeTerm], _),
        VarTerm = term__variable(Var)
    ->
        ( varset__search_name(VarSet, Var, VarName) ->
            ( convert_mode(allow_constrained_inst_var, ModeTerm, Mode0) ->
                constrain_inst_vars_in_mode(Mode0, Mode),
                term__coerce_var(Var, ProgVar),
                P = (pragma_var(ProgVar, VarName, Mode)),
                parse_pragma_c_code_varlist(VarSet, Vars, PragmaVars0, Error),
                PragmaVars = [P|PragmaVars0]
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
    list(term)::in, term::in, maybe1(item)::out) is det.

parse_tabling_pragma(ModuleName, PragmaName, TablingType, PragmaTerms,
        ErrorTerm, Result) :-
    (
        (
            PragmaTerms = [PredAndModesTerm0],
            MaybeSpec = no
        ;
            PragmaTerms = [PredAndModesTerm0, SpecListTerm0],
            TablingType = eval_memo(all_strict),
            MaybeSpec = yes(SpecListTerm0)
        )
    ->
        string__append_list(["`:- pragma ", PragmaName, "' declaration"],
            ParseMsg),
        parse_arity_or_modes(ModuleName, PredAndModesTerm0, ErrorTerm,
            ParseMsg, ArityModesResult),
        (
            ArityModesResult = ok(arity_or_modes(PredName, Arity,
                MaybePredOrFunc, MaybeModes)),
            (
                MaybeSpec = yes(SpecListTerm),
                convert_list(SpecListTerm, parse_arg_tabling_method,
                    "expected argument tabling method", MaybeArgMethods),
                (
                    MaybeArgMethods = ok(ArgMethods),
                    Result = ok(pragma(user,
                        tabled(eval_memo(specified(ArgMethods)),
                        PredName, Arity, MaybePredOrFunc, MaybeModes)))
                ;
                    MaybeArgMethods = error(Msg, Term),
                    Result = error(Msg, Term)
                )
            ;
                MaybeSpec = no,
                Result = ok(pragma(user,
                    tabled(TablingType, PredName, Arity,
                    MaybePredOrFunc, MaybeModes)))
            )
        ;
            ArityModesResult = error(Msg, Term),
            Result = error(Msg, Term)
        )
    ;
        string__append_list(["wrong number of arguments in `:- pragma ",
            PragmaName, "' declaration"], ErrorMessage),
        Result = error(ErrorMessage, ErrorTerm)
    ).

:- pred parse_arg_tabling_method(term::in, maybe(arg_tabling_method)::out)
    is semidet.

parse_arg_tabling_method(term__functor(term__atom("value"), [], _),
    yes(arg_value)).
parse_arg_tabling_method(term__functor(term__atom("addr"), [], _),
    yes(arg_addr)).
parse_arg_tabling_method(term__functor(term__atom("promise_implied"), [], _),
    yes(arg_promise_implied)).
parse_arg_tabling_method(term__functor(term__atom("output"), [], _), no).

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
        PredAndModesTerm0 = term__functor(term__atom("/"),
            [PredNameTerm, ArityTerm], _)
    ->
        (
            parse_implicitly_qualified_term(ModuleName, PredNameTerm,
                PredAndModesTerm0, "", ok(PredName, [])),
            ArityTerm = term__functor(term__integer(Arity), [], _)
        ->
            Result = ok(arity_or_modes(PredName, Arity, no, no))
        ;
            string__append("expected predname/arity for", ErrorMsg, Msg),
            Result = error(Msg, ErrorTerm)
        )
    ;
        parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
            PredAndModesTerm0, ErrorMsg, PredAndModesResult),
        (
            PredAndModesResult = ok(PredName - PredOrFunc, Modes),
            list__length(Modes, Arity0),
            ( PredOrFunc = function ->
                Arity = Arity0 - 1
            ;
                Arity = Arity0
            ),
            Result = ok(arity_or_modes(PredName, Arity, yes(PredOrFunc),
                yes(Modes)))
        ;
            PredAndModesResult = error(Msg, Term),
            Result = error(Msg, Term)
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
        PredAndArgsResult =
        ok(PredName, ArgModeTerms - MaybeRetModeTerm),
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
                    list__append(ArgModes0, [RetMode], ArgModes1),
                    list__map(constrain_inst_vars_in_mode, ArgModes1,
                        ArgModes),
                    Result = ok(PredName - function, ArgModes)
                ;
                    string__append("error in return mode in ", Msg, ErrorMsg),
                    Result = error(ErrorMsg, ErrorTerm)
                )
            ;
                MaybeRetModeTerm = no,
                Result = ok(PredName - predicate, ArgModes0)
            )
        ;
            string__append("error in argument modes in ", Msg,
                ErrorMsg),
            Result = error(ErrorMsg, ErrorTerm)
        )
    ;
        PredAndArgsResult = error(ErrorMsg, Term),
        Result = error(ErrorMsg, Term)
    ).

:- pred convert_bool(term::in, bool::out) is semidet.

convert_bool(Term, Bool) :-
    Term = term__functor(term__atom(Name), [], _),
    ( Name = "yes", Bool = yes
    ; Name = "no", Bool = no
    ).

:- pred convert_bool_list(term::in, list(bool)::out) is semidet.

convert_bool_list(ListTerm, Bools) :-
    convert_list(ListTerm, convert_bool, "expected boolean", ok(Bools)).

:- pred convert_int(term::in, int::out) is semidet.

convert_int(Term, Int) :-
    Term = term__functor(term__integer(Int), [], _).

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

convert_list(term__variable(V), _, UnrecognizedMsg,
        error(UnrecognizedMsg, term__variable(V))).
convert_list(term__functor(Functor, Args, Context), Pred, UnrecognizedMsg,
        Result) :-
    (
        Functor = term__atom("[|]"),
        Args = [Term, RestTerm]
    ->
        ( call(Pred, Term, Element) ->
            convert_list(RestTerm, Pred, UnrecognizedMsg, RestResult),
            (
                RestResult = ok(List0),
                Result = ok([Element | List0])
            ;
                RestResult = error(_, _),
                Result = RestResult
            )
        ;
            Result = error(UnrecognizedMsg, Term)
        )
    ;
        Functor = term__atom("[]"),
        Args = []
    ->
        Result = ok([])
    ;
        Result = error("error in list", term__functor(Functor, Args, Context))
    ).

:- pred convert_type_spec_pair(term::in, pair(tvar, mer_type)::out) is semidet.

convert_type_spec_pair(Term, TypeSpec) :-
    Term = term__functor(term__atom("="), [TypeVarTerm, SpecTypeTerm0], _),
    TypeVarTerm = term__variable(TypeVar0),
    term__coerce_var(TypeVar0, TypeVar),
    parse_type(SpecTypeTerm0, ok(SpecType)),
    TypeSpec = TypeVar - SpecType.

%-----------------------------------------------------------------------------%
%
% Parsing termination2_info pragmas.
%

:- pred parse_arg_size_constraints(term::in,
    maybe1(maybe(list(arg_size_constr)))::out) is semidet.

parse_arg_size_constraints(ArgSizeTerm, Result) :-
    (
        ArgSizeTerm = term__functor(term__atom("not_set"), [], _),
        Result = ok(no)
    ;
        ArgSizeTerm = term__functor(term__atom("constraints"),
            [Constraints0], _),
        convert_list(Constraints0, parse_arg_size_constraint,
            "expected constraint", ConstraintsResult),
        ConstraintsResult = ok(Constraints),
        Result = ok(yes(Constraints))
    ).

:- pred parse_arg_size_constraint(term::in, arg_size_constr::out) is semidet.

parse_arg_size_constraint(Term, Constr) :-
    (
        Term = term__functor(term__atom("le"), [Terms, ConstantTerm], _),
        convert_list(Terms, parse_lp_term, "expected linear term",
            TermsResult),
        TermsResult = ok(LPTerms),
        parse_rational(ConstantTerm, Constant),
        Constr = le(LPTerms, Constant)

    ;
        Term = term__functor(term__atom("eq"), [Terms, ConstantTerm], _),
        convert_list(Terms, parse_lp_term, "expected linear term",
            TermsResult),
        TermsResult = ok(LPTerms),
        parse_rational(ConstantTerm, Constant),
        Constr = eq(LPTerms, Constant)
    ).

:- pred parse_lp_term(term::in, pair(int, rat)::out) is semidet.

parse_lp_term(Term, LpTerm) :-
    Term = term__functor(term__atom("term"), [VarIdTerm, CoeffTerm], _),
    VarIdTerm = term__functor(term__integer(VarId), [], _),
    parse_rational(CoeffTerm, Coeff),
    LpTerm = VarId - Coeff.

:- pred parse_rational(term::in, rat::out) is semidet.

parse_rational(Term, Rational) :-
    Term = term__functor(term__atom("r"), [NumerTerm, DenomTerm], _),
    NumerTerm = term__functor(term__integer(Numer), [], _),
    DenomTerm = term__functor(term__integer(Denom), [], _),
    Rational = rat__rat(Numer, Denom).

%-----------------------------------------------------------------------------%
:- end_module prog_io_pragma.
%-----------------------------------------------------------------------------%
