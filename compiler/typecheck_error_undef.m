%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_error_undef.m.
%
% This file contains predicates to report references to undefined predicates,
% functions, data constructors and events.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_error_undef.
:- interface.

:- import_module check_hlds.typecheck_error_util.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- func report_error_call_to_undef_pred(type_error_clause_context,
    prog_context, sym_name_pred_form_arity) = error_spec.

%---------------------------------------------------------------------------%

:- type cons_error
    --->    other_lang_foreign_type_constructor(type_ctor, hlds_type_defn)
    ;       abstract_imported_type
    ;       invalid_field_update(sym_name, hlds_ctor_field_defn,
                tvarset, list(tvar))
    ;       new_on_non_existential_type(type_ctor).

:- func report_error_undef_cons(type_error_clause_context,
    type_error_goal_context, prog_context, list(cons_error), cons_id, arity)
    = error_spec.

%---------------------------------------------------------------------------%

    % Report that there is no event with the given name.
    %
:- func report_error_undef_event(prog_context, string) = error_spec.

    % Report that there is an event with the given name, but not with
    % the given arity.
    %
:- func report_error_undef_event_arity(prog_context, string, list(mer_type),
    list(prog_var)) = error_spec.

%---------------------------------------------------------------------------%

:- func maybe_report_no_clauses(module_info, pred_id, pred_info)
    = list(error_spec).

:- func maybe_report_no_clauses_stub(module_info, pred_id, pred_info)
    = list(error_spec).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The implementation of report_error_undef_pred.
%

report_error_call_to_undef_pred(ClauseContext, Context, SymNameArity) = Spec :-
    SymNameArity = sym_name_pred_form_arity(SymName, _PredFormArity),
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    PredMarkers = ClauseContext ^ tecc_pred_markers,
    IsFullyQualified = calls_are_fully_qualified(PredMarkers),
    predicate_table_lookup_pf_sym(PredicateTable, IsFullyQualified,
        pf_predicate, SymName, OtherIds),
    (
        OtherIds = [_ | _],
        predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
        find_pred_arities(PredIdTable, OtherIds, PredFormArities),
        Spec = report_error_pred_wrong_arity(ClauseContext, Context,
            SymNameArity, PredFormArities)
    ;
        OtherIds = [],
        is_undef_pred_reference_special(ClauseContext, SymNameArity,
            UndefClass),
        (
            UndefClass = undef_special(SpecialComponents),
            InClauseForPieces = in_clause_for_pieces(ClauseContext),
            InClauseForComponent = always(InClauseForPieces),
            Msg = simple_msg(Context,
                [InClauseForComponent | SpecialComponents]),
            Spec = error_spec($pred, severity_error, phase_type_check, [Msg])
        ;
            UndefClass = undef_ordinary(MissingImportModules, AddeddumPieces),
            Spec = report_error_pred_wrong_full_name(ClauseContext, Context,
                PredicateTable, SymNameArity, MissingImportModules,
                AddeddumPieces)
        )
    ).

%---------------------%

:- func report_error_pred_wrong_arity(type_error_clause_context, prog_context,
    sym_name_pred_form_arity, list(pred_form_arity)) = error_spec.

report_error_pred_wrong_arity(ClauseContext, Context, SymNameArity,
        AllPredFormArities) = Spec :-
    SymNameArity = sym_name_pred_form_arity(SymName, PredFormArity),
    PredFormArity = pred_form_arity(PredFormArityInt),
    AllPredFormArityInts =
        list.map(project_pred_form_arity_int, AllPredFormArities),
    MainPieces = in_clause_for_pieces(ClauseContext) ++
        [words("error:")] ++
        arity_error_to_pieces(pf_predicate, PredFormArityInt,
            AllPredFormArityInts) ++ [nl] ++
        [words("in call to"), p_or_f(pf_predicate)] ++
        color_as_subject([qual_sym_name(SymName), suffix(".")]) ++
        [nl],
    ( if
        % A call to process_options or to process_options_track in getopt_io
        % may appear in the source code either explicitly qualified,
        % or unqualified. If not explicitly qualified by the user,
        % it won't be qualified by the compiler either, due to
        % the arity mismatch.
        (
            SymName = unqualified(PredName),
            StdLibModuleName = "getopt_io"
        ;
            SymName = qualified(ModuleName, PredName),
            is_std_lib_module_name(ModuleName, StdLibModuleName),
            StdLibModuleName = "getopt_io"
        ),
        % We add SpecialPieces if these predicates are called
        % with (one of) their old arities.
        (
            PredName = "process_options",
            ( PredFormArityInt = 6 ; PredFormArityInt = 7 )
        ;
            PredName = "process_options_track",
            PredFormArityInt = 9
        )
    then
        SpecialPieces =
            [words("One possible reason for the error is that"),
            words("the predicate in the"), quote(StdLibModuleName),
            words("module that used to be named"), quote(PredName)] ++
            color_as_possible_cause([words("has been renamed to"),
                quote(PredName ++ "_io"), suffix(".")]) ++
            [nl]
    else
        SpecialPieces = []
    ),
    Spec = spec($pred, severity_error, phase_type_check,
        Context, MainPieces ++ SpecialPieces).

%---------------------%

:- type undef_class
    --->    undef_special(list(error_msg_component))
            % This is not a reference to an undefined predicate,
            % but a badly-executed attempt at using Mercury's "keywords".
    ;       undef_ordinary(list(module_name), list(format_piece)).
            % This is an ordinary reference to an undefined predicate.
            % The first argument lists modules whose import may fix
            % the reference, and the second may contain an addendum
            % to be printed after the usual diagnostic messages.

:- pred is_undef_pred_reference_special(type_error_clause_context::in,
    sym_name_pred_form_arity::in, undef_class::out) is det.

is_undef_pred_reference_special(ClauseContext, SymNameArity, UndefClass) :-
    SymNameArity = sym_name_pred_form_arity(PredSymName, PredFormArity),
    PredFormArity = pred_form_arity(PredFormArityInt),
    ( if
        PredSymName = unqualified(PredName),
        is_undef_pred_a_syntax_error(PredName, PredFormArityInt,
            UndefSyntaxComponents)
    then
        UndefClass = undef_special(UndefSyntaxComponents)
    else
        (
            PredSymName = qualified(ModuleQualifier, _),
            maybe_report_missing_import_addendum(ClauseContext,
                ModuleQualifier,
                MissingImportAddeddumPieces, MissingImportModules)
        ;
            PredSymName = unqualified(_),
            MissingImportAddeddumPieces = [],
            MissingImportModules = []
        ),
        maybe_warn_about_getopt_changes(PredSymName, PredFormArityInt,
            GetoptPieces),
        AddeddumPieces = MissingImportAddeddumPieces ++ GetoptPieces,
        UndefClass = undef_ordinary(MissingImportModules, AddeddumPieces)
    ).

:- pred is_undef_pred_a_syntax_error(string::in, int::in,
    list(error_msg_component)::out) is semidet.

is_undef_pred_a_syntax_error(PredName, PredFormArityInt, Components) :-
    (
        PredName = "->",
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 ),
        MainPieces = [words("error: this")] ++
            color_as_subject([quote("->")]) ++
            [words("has no matching")] ++
            color_as_incorrect([quote(";"), suffix(".")]) ++
            [nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the"), quote("else"), words("part is not optional."),
            nl, words("Every if-then must have an"),
            quote("else"), suffix("."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        Components = [MainComponent, VerboseComponent]
    ;
        PredName = "else",
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 ),
        Pieces = [words("error: this")] ++
            color_as_subject([quote("else")]) ++
            color_as_incorrect([words("has no matching")]) ++
            color_as_possible_cause([quote("if")]) ++
            [words("or")] ++
            color_as_possible_cause([quote("then"), suffix(".")]) ++
            [nl],
        Components = [always(Pieces)]
    ;
        PredName = "if",
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 ),
        Pieces = [words("error: this")] ++
            color_as_subject([quote("if")]) ++
            [words("has no matching")] ++
            color_as_possible_cause([quote("then")]) ++
            [words("or")] ++
            color_as_possible_cause([quote("else"), suffix(".")]) ++
            [nl],
        Components = [always(Pieces)]
    ;
        PredName = "then",
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 ),
        MainPieces = [words("error: this")] ++
            color_as_subject([quote("then")]) ++
            [words("has no matching")] ++
            color_as_possible_cause([quote("if")]) ++
            [words("or")] ++
            color_as_possible_cause([quote("else"), suffix(".")]) ++
            [nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the"), quote("else"), words("part is not optional."),
            nl, words("Every if-then must have an"),
            quote("else"), suffix("."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        Components = [MainComponent, VerboseComponent]
    ;
        PredName = "apply",
        PredFormArityInt >= 1,
        Components = report_apply_instead_of_pred
    ;
        ( PredName = "semipure"
        ; PredName = "impure"
        ),
        PredFormArityInt = 1,
        Pieces = [words("error: this")] ++
            color_as_subject([quote(PredName), words("marker")]) ++
            [words("is")] ++
            color_as_incorrect([words("not before a predicate call.")]) ++
            [nl],
        Components = [always(Pieces)]
    ;
        PredName = "some",
        PredFormArityInt = 2,
        Pieces = [words("syntax error in existential quantification:")] ++
            [words("the")] ++
            color_as_subject([words("first argument of"), quote("some")]) ++
            color_as_incorrect([words("should be a list of variables.")]) ++
            [nl],
        Components = [always(Pieces)]
    ).

:- func report_apply_instead_of_pred = list(error_msg_component).

report_apply_instead_of_pred = Components :-
    MainPieces = [words("error: the language construct")] ++
        color_as_subject([quote("apply")]) ++
        color_as_incorrect([words("should be used as an expression,"),
            words("not as a goal.")]) ++
        [nl],
    MainComponent = always(MainPieces),
    VerbosePieces =
        [words("Perhaps you forgot to add"), quote(" = ..."), suffix("?)"), nl,
        words("If you are trying to invoke a higher-order predicate,"),
        words("use"), quote("call"), suffix(","),
            words("not"), quote("apply"), suffix("."), nl,
        words("If you are trying to curry a higher-order function,"),
        words("use a forwarding function:"), nl,
        words("e.g. instead of "), quote("NewFunc = apply(OldFunc, X)"),
        words("use"), quote("NewFunc = my_apply(OldFunc, X)"),
        words("where"), quote("my_apply"), words("is defined"),
        words("with the appropriate arity, e.g."),
        quote("my_apply(Func, X, Y) :- apply(Func, X, Y)."), nl],
    VerboseComponent = verbose_only(verbose_always, VerbosePieces),
    Components = [MainComponent, VerboseComponent].

%---------------------%

:- pred maybe_warn_about_getopt_changes(sym_name::in, int::in,
    list(format_piece)::out) is det.

maybe_warn_about_getopt_changes(PredSymName, PredFormArityInt, GetoptPieces) :-
    ( if
        % A call to process_options_se or to process_options_track_se
        % in getopt or getopt_io may appear in the source code either
        % explicitly qualified, or unqualified. If not explicitly
        % qualified by the user, it won't be qualified by the compiler
        % either, due to the wrong name.
        (
            PredSymName = unqualified(PredName)
        ;
            PredSymName = qualified(ModuleName, PredName),
            is_std_lib_module_name(ModuleName, StdLibModuleName),
            ( StdLibModuleName = "getopt"
            ; StdLibModuleName = "getopt_io"
            )
        ),
        % We add SpecialPieces if these predicates are called
        % with (one of) their old arities. (If they are called
        % with any other arity, then the caller didn't work
        % with the old contents of the getopt modules either.)
        (
            PredName = "process_options_se",
            ( PredFormArityInt = 4
            ; PredFormArityInt = 5
            ; PredFormArityInt = 6
            ; PredFormArityInt = 7
            ),
            NewPredName = "process_options"
        ;
            PredName = "process_options_track_se",
            ( PredFormArityInt = 7
            ; PredFormArityInt = 9
            ),
            NewPredName = "process_options_track"
        )
    then
        GetoptPieces =
            [words("One possible reason for the error is that"),
            words("the predicate"), quote(PredName),
            words("in the Mercury standard library has been")] ++
            color_as_possible_cause([words("renamed to"),
                quote(NewPredName), suffix(".")]) ++
            [nl]
    else
        GetoptPieces = []
    ).

%---------------------%

:- func report_error_pred_wrong_full_name(type_error_clause_context,
    prog_context, predicate_table, sym_name_pred_form_arity, list(module_name),
    list(format_piece)) = error_spec.

report_error_pred_wrong_full_name(ClauseContext, Context, PredicateTable,
        SymNamePredFormArity, MissingImportModules, AddeddumPieces) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    InClauseForComponent = always(InClauseForPieces),
    SymNamePredFormArity = sym_name_pred_form_arity(SymName, PredFormArity),
    PredFormArity = pred_form_arity(Arity),
    SNA = sym_name_arity(SymName, Arity),
    MainPieces = [words("error: call to")] ++
        color_as_incorrect([words("undefined")]) ++
        [words("predicate")] ++
        color_as_subject([qual_sym_name_arity(SNA), suffix(".")]) ++
        [nl],
    UndefMsg = simple_msg(Context,
        [InClauseForComponent, always(MainPieces ++ AddeddumPieces)]),

    predicate_table_lookup_pf_sym(PredicateTable, may_be_partially_qualified,
        pf_function, SymName, FuncOtherIds),
    (
        FuncOtherIds = [_ | _],
        KindMsg = report_error_func_instead_of_pred(Context),
        KindMsgs = [KindMsg]
    ;
        FuncOtherIds = [],
        KindMsgs = []
    ),
    PossibleModuleQuals =
        find_possible_pf_missing_module_qualifiers(PredicateTable,
            pf_predicate, SymName),
    set.list_to_set(PossibleModuleQuals, PossibleModuleQualsSet0),
    set.delete_list(MissingImportModules,
        PossibleModuleQualsSet0, PossibleModuleQualsSet),
    QualMsgs = report_any_missing_module_qualifiers(ClauseContext,
        Context, "predicate", PossibleModuleQualsSet),
    KindQualMsgs = KindMsgs ++ QualMsgs,
    ( if
        AddeddumPieces = [],
        KindQualMsgs = []
    then
        KindQualMsgs = [],
        % It seems that regardless of missing qualifications or equal signs,
        % the reference is to the wrong name. See if we can mention some
        % similar names that could be the one they intended.
        get_known_pred_info_names(PredicateTable, pf_predicate,
            KnownPredNames),
        BaseName = unqualify_name(SymName),
        maybe_construct_did_you_mean_pieces(BaseName, KnownPredNames,
            DidYouMeanPieces),
        (
            DidYouMeanPieces = [],
            Msgs = [UndefMsg]
        ;
            DidYouMeanPieces = [_ | _],
            DidyouMeanMsg = msg(Context, DidYouMeanPieces),
            Msgs = [UndefMsg, DidyouMeanMsg]
        )
    else
        % The AddeddumPieces part of UndefMsg and/or KindQualMsgs
        % offer hints about the error that allow for the base name being right.
        % Print just those.
        % XXX Should we print SuggestedNamesMsg as well even in this case?
        Msgs = [UndefMsg] ++ KindQualMsgs
    ),
    Spec = error_spec($pred, severity_error, phase_type_check, Msgs).

%---------------------%

:- func report_error_func_instead_of_pred(prog_context) = error_msg.

report_error_func_instead_of_pred(Context) = Msg :-
    Pieces = [words("(There is a *function* with that name, however."), nl] ++
        color_as_possible_cause([words("Perhaps you forgot to add"),
            quote(" = ..."), suffix("?)")]) ++
        [nl],
    Msg = msg(Context, Pieces).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The implementation of report_error_undef_cons.
%

report_error_undef_cons(ClauseContext, GoalContext, Context,
        ConsErrors, Functor, Arity) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),
    InitComp = always(InClauseForPieces ++ GoalContextPieces),
    % Check for some special cases, so that we can give clearer error messages.
    ( if
        Functor = cons(unqualified(FunctorName), FunctorArity, _),
        expect(unify(Arity, FunctorArity), $pred, "arity mismatch"),
        language_builtin_functor(FunctorName, FunctorArity)
    then
        language_builtin_functor_components(FunctorName, FunctorArity,
            FunctorComps),
        Spec = error_spec($pred, severity_error, phase_type_check,
            [simple_msg(Context, [InitComp | FunctorComps])])
    else if
        Functor = cons(unqualified(FunctorName), FunctorArity, _),
        expect(unify(Arity, FunctorArity), $pred, "arity mismatch"),
        syntax_functor_components(FunctorName, FunctorArity, FunctorComps)
    then
        Spec = error_spec($pred, severity_error, phase_type_check,
            [simple_msg(Context, [InitComp | FunctorComps])])
    else
        report_error_undef_cons_std(ClauseContext, Context, InitComp,
            ConsErrors, Functor, Arity, Spec)
    ).

%---------------------%

    % language_builtin_functor(Name, Arity) is true iff Name/Arity is the name
    % of a builtin language construct that should be used as a goal,
    % not as an expression.
    %
:- pred language_builtin_functor(string::in, arity::in) is semidet.

language_builtin_functor("=", 2).
language_builtin_functor("\\=", 2).
language_builtin_functor(",", 2).
language_builtin_functor(";", 2).
language_builtin_functor("\\+", 1).
language_builtin_functor("not", 1).
language_builtin_functor("<=>", 2).
language_builtin_functor("=>", 2).
language_builtin_functor("<=", 2).
language_builtin_functor("call", _).
language_builtin_functor("impure", 1).
language_builtin_functor("semipure", 1).
language_builtin_functor("all", 2).
language_builtin_functor("some", 2).

:- pred language_builtin_functor_components(string::in, arity::in,
    list(error_msg_component)::out) is det.

language_builtin_functor_components(Name, Arity, Components) :-
    NameArity = name_arity(Name, Arity),
    % XXX For many of the strings that we can get as Name, such as ";" and
    % "impure", the phrase "should be used as a goal" is somewhat misleading.
    % This is because neither is a goal; a semicolon is a connective
    % *between* two goals, while "impure" is a prefix *before* a goal.
    %
    % is_undef_pred_a_syntax_error has some wording we may want to use here
    % for such cases. It would be nice if we could do that without any
    % code duplication.
    MainPieces = [words("error: the language construct")] ++
        color_as_subject([name_arity(NameArity)]) ++
        color_as_incorrect([words("should be used as a goal,"),
            words("not as an expression.")]) ++
        [nl],
    VerbosePieces = [words("If you are trying to use a goal"),
        words("as a boolean function, you should write")] ++
        color_as_possible_cause([words_quote("if <goal> then yes else no"),
            words("instead.")]) ++
        [nl],
    ( if Name = "call" then
        VerboseCallPieces =
            [words("If you are trying to invoke a higher-order function,"),
            words("you should use")] ++
            color_as_correct([quote("apply"), suffix(",")]) ++
            [words("not")] ++
            color_as_incorrect([quote("call"), suffix(".")]) ++
            [nl,
            words("If you are trying to curry a higher-order function,"),
            words("see the ""Creating higher-order terms"" section"),
            words("of the Mercury Language Reference Manual."), nl,
            words("If you really are trying to use"),
            quote("call"), words("as an expression"),
            words("and not as an application of the language builtin call/N,"),
            words("make sure that you have the arity correct,"),
            words("and that the functor"), quote("call"),
            words("is actually defined."),
            words("(If it is defined in a separate module,"),
            words("check that the module is correctly imported.)"), nl]
    else
        VerboseCallPieces = []
    ),
    Components = [always(MainPieces),
        verbose_only(verbose_always, VerbosePieces),
        verbose_only(verbose_once, VerboseCallPieces)].

%---------------------%

:- pred syntax_functor_components(string::in, arity::in,
    list(error_msg_component)::out) is semidet.

syntax_functor_components(FunctorName, Arity, Components) :-
    (
        Arity = 1,
        FunctorName = "!",
        Pieces1 = [words("error:")] ++
            color_as_incorrect([words("invalid use of the"), quote("!"),
                words("state variable operator.")]) ++
            [nl],
        Pieces2 = color_as_possible_cause([words("You probably meant to use"),
            quote("!."), words("or"), quote("!:"), suffix(".")]) ++
            [nl],
        Components = [always(Pieces1), verbose_only(verbose_always, Pieces2)]
    ;
        Arity = 2,
        (
            (
                FunctorName = "if",
                Pieces = [words("error: this")] ++
                    color_as_subject([quote("if")]) ++
                    [words("has no matching")] ++
                    color_as_possible_cause([quote("then")]) ++
                    [words("or")] ++
                    color_as_possible_cause([quote("else"), suffix(".")]) ++
                    [nl]
            ;
                FunctorName = "else",
                Pieces = [words("error: this")] ++
                    color_as_subject([quote("else")]) ++
                    color_as_incorrect([words("has no matching")]) ++
                    color_as_possible_cause([quote("if")]) ++
                    [words("or")] ++
                    color_as_possible_cause([quote("then"), suffix(".")]) ++
                    [nl]
            ;
                FunctorName = ":-",
                Pieces = [words("syntax error in lambda expression"),
                     prefix("("), quote(":-"), suffix(")."), nl]
            ;
                FunctorName = "-->",
                Pieces = [words("syntax error in DCG lambda expression"),
                    prefix("("), quote("-->"), suffix(")."), nl]
            ;
                FunctorName = ".",
                NameArity = name_arity("[|]", 2),
                Pieces = [words("error: the list constructor is")] ++
                    color_as_correct([name_arity(NameArity), suffix(",")]) ++
                    [words("not")] ++
                    color_as_incorrect([quote("./2"), suffix(".")]) ++
                    [nl]
            ),
            Components = [always(Pieces)]
        ;
            (
                FunctorName = "then",
                MainPieces = [words("error: this")] ++
                    color_as_subject([quote("then")]) ++
                    [words("has no matching")] ++
                    color_as_possible_cause([quote("if")]) ++
                    [words("or")] ++
                    color_as_possible_cause([quote("else"), suffix(".")]) ++
                    [nl],
                VerbosePieces =
                    [words("Note: the"), quote("else"),
                    words("part is not optional."),
                    nl, words("Every if-then must have an"),
                    quote("else"), suffix("."), nl]
            ;
                FunctorName = "->",
                MainPieces = [words("error: this")] ++
                    color_as_subject([quote("->")]) ++
                    [words("has no matching")] ++
                    color_as_incorrect([quote(";"), suffix(".")]) ++
                    [nl],
                VerbosePieces =
                    [words("Note: the"), quote("else"),
                    words("part is not optional."), nl,
                    words("Every if-then must have an"),
                    quote("else"), suffix("."), nl]
            ;
                FunctorName = "^",
                MainPieces = [words("error:")] ++
                    color_as_incorrect([words("invalid use of the"),
                        words("field selection operator"),
                        prefix("("), quote("^"), suffix(").")]) ++
                    [nl],
                VerbosePieces =
                    color_as_possible_cause([words("This is probably"),
                        words("some kind of syntax error."),
                        words("The field name must be an atom,"),
                        words("not a variable or other term.")]) ++
                    [nl]
            ;
                FunctorName = ":=",
                MainPieces = [words("error:")] ++
                    color_as_incorrect([words("invalid use of the"),
                        words("field update operator"),
                        prefix("("), quote(":="), suffix(").")]) ++
                    [nl],
                VerbosePieces =
                    color_as_possible_cause([words("This is probably"),
                        words("some kind of syntax error.")]) ++
                    [nl]
            ),
            Components =
                [always(MainPieces),
                verbose_only(verbose_always, VerbosePieces)]
        )
    ).

%---------------------%

:- pred report_error_undef_cons_std(type_error_clause_context::in,
    prog_context::in, error_msg_component::in, list(cons_error)::in,
    cons_id::in, arity::in, error_spec::out) is det.

report_error_undef_cons_std(ClauseContext, Context, InitComp, ConsErrors,
        Functor, Arity, Spec) :-
    (
        ConsErrors = [],
        ConsMsgs = []
    ;
        ConsErrors = [_ | _],
        ConsMsgLists = list.map(report_cons_error(Context), ConsErrors),
        list.condense(ConsMsgLists, ConsMsgs)
    ),
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_cons_table(ModuleInfo, ConsTable),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    ( if
        Functor = cons(FunctorSymName, FunctorArity, _),
        expect(unify(Arity, FunctorArity), $pred, "arity mismatch"),

        return_cons_arities(ConsTable, FunctorSymName, ConsArities),

        predicate_table_lookup_sym(PredicateTable, may_be_partially_qualified,
            FunctorSymName, PredIds),
        return_function_arities(ModuleInfo, PredIds, [], FuncArities),

        list.sort_and_remove_dups(ConsArities ++ FuncArities, AllArities),
        list.delete_all(AllArities, Arity, OtherArities),
        OtherArities = [_ | _]
    then
        FunctorPieces = wrong_arity_constructor_to_pieces(FunctorSymName,
            Arity, OtherArities),
        FunctorComps = [always(FunctorPieces)],
        % The code that constructs QualMsgs below uses wording that
        % can be misleading in the presence of arity mismatches.
        QualSuggestionMsgs = []
    else
        UndefSymbolPieces = [words("error:")] ++
            color_as_incorrect([words("undefined")]) ++
            [words("symbol")] ++
            color_as_subject([qual_cons_id_and_maybe_arity(Functor),
                suffix(".")]) ++
            [nl],
        ( if
            Functor = cons(FunctorSymName, _, _),
            FunctorSymName = qualified(ModQual, _)
        then
            maybe_report_missing_import_addendum(ClauseContext, ModQual,
                AddeddumPieces, MissingImportModules)
        else if
            Functor = cons(unqualified("[|]"), 2, _)
        then
            maybe_report_missing_import_addendum(ClauseContext,
                unqualified("list"), AddeddumPieces, MissingImportModules)
        else if
            Functor = cons(FunctorSymName, FunctorArity, _),
            FunctorSymName = unqualified("coerce")
        then
            AddeddumPieces = [words("(The builtin")] ++
                color_as_subject([words("coerce")]) ++
                [words("operator expects")] ++
                color_as_correct([words("one")]) ++
                [words("argument, not")] ++
                color_as_incorrect([int_name(FunctorArity), suffix(".)")]) ++
                [nl],
            MissingImportModules = []
        else
            AddeddumPieces = [],
            MissingImportModules = []
        ),
        FunctorComps = [always(UndefSymbolPieces ++ AddeddumPieces)],
        ( if Functor = cons(FunctorName, _, _) then
            BaseName = unqualify_name(FunctorName),
            return_cons_defns_with_given_name(ConsTable, BaseName, ConsDefns),
            list.foldl(accumulate_matching_cons_module_names(FunctorName),
                ConsDefns, [], ConsModuleNames),
            PredModuleNames =
                find_possible_pf_missing_module_qualifiers(PredicateTable,
                    pf_predicate, FunctorName),
            FuncModuleNames =
                find_possible_pf_missing_module_qualifiers(PredicateTable,
                    pf_function, FunctorName),
            ModuleNames =
                ConsModuleNames ++ PredModuleNames ++ FuncModuleNames,
            set.list_to_set(ModuleNames, ModuleNamesSet0),
            set.delete_list(MissingImportModules,
                ModuleNamesSet0, ModuleNamesSet),
            QualMsgs = report_any_missing_module_qualifiers(ClauseContext,
                Context, "symbol", ModuleNamesSet),
            ( if
                ConsMsgs = [],
                QualMsgs = []
            then
                % It seems that the reference is to the wrong name.
                % See if we can mention some similar names that could be
                % the one they intended.
                cons_table_names(ConsTable, ConsTableNameSet),
                get_known_pred_info_names(PredicateTable, pf_function,
                    KnownFuncNames0),
                set.sorted_list_to_set(KnownFuncNames0, KnownFuncNamesSet0),
                set.union(ConsTableNameSet,
                    KnownFuncNamesSet0, KnownFuncNamesSet),
                set.to_sorted_list(KnownFuncNamesSet, KnownFuncNames),
                maybe_construct_did_you_mean_pieces(BaseName, KnownFuncNames,
                    DidYouMeanPieces),
                (
                    DidYouMeanPieces = [],
                    QualSuggestionMsgs = []
                ;
                    DidYouMeanPieces = [_ | _],
                    DidyouMeanMsg = msg(Context, DidYouMeanPieces),
                    QualSuggestionMsgs = [DidyouMeanMsg]
                )
            else
                QualSuggestionMsgs = QualMsgs
            )
        else
            QualSuggestionMsgs = []
        )
    ),
    FirstMsg = simple_msg(Context, [InitComp | FunctorComps]),
    Spec = error_spec($pred, severity_error, phase_type_check,
        [FirstMsg | ConsMsgs] ++ QualSuggestionMsgs).

:- pred return_function_arities(module_info::in, list(pred_id)::in,
    list(int)::in, list(int)::out) is det.

return_function_arities(_, [], !FuncArities).
return_function_arities(ModuleInfo, [PredId | PredIds], !FuncArities) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        pred_info_get_orig_arity(PredInfo, PredFormArity),
        user_arity_pred_form_arity(PredOrFunc,
            user_arity(FuncUserArityInt), PredFormArity),
        !:FuncArities = [FuncUserArityInt | !.FuncArities]
    ),
    return_function_arities(ModuleInfo, PredIds, !FuncArities).

:- func wrong_arity_constructor_to_pieces(sym_name, arity, list(int))
    = list(format_piece).

wrong_arity_constructor_to_pieces(Name, Arity, ActualArities) = Pieces :-
    % Constructors' arities should be treated the same way as
    % predicates' arities, in that neither has a distinguished "return value"
    % argument.
    NumArgsPieces = arity_error_to_pieces(pf_predicate, Arity, ActualArities),
    Pieces = [words("error:")] ++ NumArgsPieces ++
        [words("in use of constructor")] ++
        color_as_subject([qual_sym_name(Name), suffix(".")]) ++
        [nl].

:- pred accumulate_matching_cons_module_names(sym_name::in, hlds_cons_defn::in,
    list(module_name)::in, list(module_name)::out) is det.

accumulate_matching_cons_module_names(FunctorSymName, ConsDefn,
        !ModuleNames) :-
    type_ctor(TypeCtorSymName, _) = ConsDefn ^ cons_type_ctor,
    (
        TypeCtorSymName = unqualified(_)
        % There can be no problem with use_module replacing import_module
        % if the hlds_cons_defn is for a builtin data constructor.
    ;
        TypeCtorSymName = qualified(TypeCtorModuleName, _),
        FunctorName = unqualify_name(FunctorSymName),
        FullSymName = qualified(TypeCtorModuleName, FunctorName),
        ( if partial_sym_name_matches_full(FunctorSymName, FullSymName) then
            !:ModuleNames = [TypeCtorModuleName | !.ModuleNames]
        else
            true
        )
    ).

:- func report_cons_error(prog_context, cons_error) = list(error_msg).

report_cons_error(Context, ConsError) = Msgs :-
    (
        ConsError = other_lang_foreign_type_constructor(TypeCtor, _),
        %   Pieces = [words("There are"),
        %       pragma_decl("foreign_type"), words("declarations"),
        %       words("for type"), qual_type_ctor(TypeCtor), suffix(","),
        %       words("so it is treated as an abstract type"),
        %       words("in all predicates and functions"),
        %       words("which are not implemented"),
        %       words("for those foreign types."), nl],
        %
        % XXX Until 2024 may 20, we used the above wording for this message,
        % even though it was quite confused, and also quite confusing.
        % I (zs) *think* that this replacement text is a faithful description
        % of the problem it attempted to describe, but I don't know for sure.
        %
        % I also don't like the fact that
        %
        % - starting with a Mercury type t being used in predicate p
        %   written in Mercury,
        %
        % - adding e.g. a Java definition for type t and a Java foreign_proc
        %   for predicate p
        %
        % - would make references to the function symbols of the Mercury
        %   definition of type t suddently invalid in the Mercury code of p.
        %
        % The presence of a definition of a type in foreign language L
        % should affect the validity of a piece of Mercury code using that
        % type *only* when the current grade targets L.
        Pieces = [words("There are"),
            pragma_decl("foreign_type"), words("declarations"),
            words("for type"), qual_type_ctor(TypeCtor),
            words("for at least one target language other than the one"),
            words("targeted by the current grade."),
            words("Due to a limitation of the Mercury implementation,"),
            words("this fact requires the compiler to treat this type"),
            words("as an abstract type in all Mercury code, which")] ++
            color_as_incorrect([words("disallows references to the"),
                words("function symbols of the type.")]) ++
            [nl],
        Msgs = [msg(Context, Pieces)]
    ;
        ConsError = abstract_imported_type,
        % For `abstract_imported_type' errors, the "undefined symbol" error
        % written by `report_error_undef_cons' is sufficient, so we do not
        % print an additional error message here.
        Msgs = []
    ;
        ConsError = invalid_field_update(FieldName, FieldDefn, TVarSet, TVars),
        FieldDefn = hlds_ctor_field_defn(DefnContext, _, _, ConsId, _),
        (
            TVars = [],
            unexpected($pred, "no type variables")
        ;
            TVars = [TVar],
            VarVars  = "variable",
            TVarPiece = var_to_quote_piece(TVarSet, TVar),
            TVarsPieces = color_as_subject([TVarPiece]),
            OccurOccurs = "occurs"
        ;
            TVars = [_, _ | _],
            VarVars  = "variables",
            TVarPieces = list.map(var_to_quote_piece(TVarSet), TVars),
            TVarsPieces = component_list_to_color_pieces(yes(color_subject),
                "and", [], TVarPieces),
            OccurOccurs = "occur"
        ),
        Pieces = [words("Field")] ++
            color_as_subject([unqual_sym_name(FieldName)]) ++
            color_as_incorrect([words("cannot be updated")]) ++
            [words("because the existentially quantified type"),
            words(VarVars)] ++
            TVarsPieces ++
            [words(OccurOccurs), words("not just in the type of this field,"),
            words("but also in the types of some of the other fields of"),
            qual_cons_id_and_maybe_arity(ConsId), suffix("."), nl],
        Msgs = [msg(DefnContext, Pieces)]
    ;
        ConsError = new_on_non_existential_type(TypeCtor),
        Pieces = [words("Invalid use of the")] ++
            % The space after "new" is deliberate.
            color_as_subject([quote("new "), words("prefix")]) ++
            [words("on a constructor of type"), qual_type_ctor(TypeCtor),
            suffix(","), words("which is")] ++
            color_as_incorrect([words("not existentially typed.")]) ++
            [nl],
        Msgs = [msg(Context, Pieces)]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The implementation of report_error_undef_event.
%

report_error_undef_event(Context, EventName) = Spec :-
    Pieces = [words("Error: there is")] ++
        color_as_incorrect([words("no event named"),
            quote(EventName), suffix(".")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_type_check, Context, Pieces).

report_error_undef_event_arity(Context, EventName, EventArgTypes, Args)
        = Spec :-
    pred_form_arity(ActualArity) = arg_list_arity(Args),
    pred_form_arity(ExpectedArity) = arg_list_arity(EventArgTypes),
    Pieces = [words("Error:")] ++
        arity_error_to_pieces(pf_predicate, ActualArity, [ExpectedArity]) ++
        [words("in")] ++
        color_as_subject([words("event"), quote(EventName), suffix(".")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_type_check, Context, Pieces).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The implementation of maybe_report_no_clauses*.
%

maybe_report_no_clauses(ModuleInfo, PredId, PredInfo) = Specs :-
    ShouldReport = should_report_no_clauses(ModuleInfo, PredInfo),
    (
        ShouldReport = yes,
        PredPieces = describe_one_pred_name(ModuleInfo, yes(color_subject),
            should_not_module_qualify, [], PredId),
        Pieces = [words("Error:")] ++ PredPieces ++
            color_as_incorrect([words("has no clauses.")]) ++
            [nl],
        pred_info_get_context(PredInfo, Context),
        Spec = spec($pred, severity_error, phase_type_check, Context, Pieces),
        Specs = [Spec]
    ;
        ShouldReport = no,
        Specs = []
    ).

maybe_report_no_clauses_stub(ModuleInfo, PredId, PredInfo) = Specs :-
    ShouldReport = should_report_no_clauses(ModuleInfo, PredInfo),
    (
        ShouldReport = yes,
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, warn_stubs, WarnStubs),
        (
            WarnStubs = yes,
            PredDotPieces = describe_one_pred_name(ModuleInfo,
                yes(color_subject), should_not_module_qualify,
                [suffix(".")], PredId),
            Pieces = [words("Warning:")] ++
                color_as_incorrect([words("no clauses")]) ++
                [words("for")] ++ PredDotPieces ++ [nl],
            pred_info_get_context(PredInfo, Context),
            Spec = spec($pred, severity_warning, phase_type_check,
                Context, Pieces),
            Specs = [Spec]
        ;
            WarnStubs = no,
            Specs = []
        )
    ;
        ShouldReport = no,
        Specs = []
    ).

:- func should_report_no_clauses(module_info, pred_info) = bool.

should_report_no_clauses(ModuleInfo, PredInfo) = ShouldReport :-
    module_info_get_int_bad_clauses(ModuleInfo, IntBadClauses),
    module_info_get_name(ModuleInfo, ModuleName),
    pred_info_get_module_name(PredInfo, PredModuleName),
    pred_info_get_name(PredInfo, PredName),
    UserArity = pred_info_user_arity(PredInfo),
    pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
    SymName = qualified(ModuleName, PredName),
    Id = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
    pred_info_get_markers(PredInfo, PredMarkers),
    ( if
        ( set.contains(IntBadClauses, Id)
        ; check_marker(PredMarkers, marker_fact_table_semantic_errors)
        ; check_marker(PredMarkers, marker_no_pred_decl)
        ; ModuleName \= PredModuleName
        )
    then
        ShouldReport = no
    else
        ShouldReport = yes
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utility predicates useful in more than one of the exported predicates.
%

:- func find_possible_pf_missing_module_qualifiers(predicate_table,
    pred_or_func, sym_name) = list(module_name).

find_possible_pf_missing_module_qualifiers(PredicateTable,
        PredOrFunc, SymName) = ModuleNames :-
    predicate_table_lookup_pf_raw_name(PredicateTable, PredOrFunc,
        unqualify_name(SymName), PredIds),
    list.foldl(accumulate_matching_pf_module_names(PredicateTable, SymName),
        PredIds, [], ModuleNames).

:- pred accumulate_matching_pf_module_names(predicate_table::in, sym_name::in,
    pred_id::in, list(module_name)::in, list(module_name)::out) is det.

accumulate_matching_pf_module_names(PredicateTable, SymName, PredId,
        !ModuleNames) :-
    predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
    map.lookup(PredIdTable, PredId, PredInfo),
    pred_info_get_module_name(PredInfo, ModuleName),
    (
        SymName = unqualified(_),
        !:ModuleNames = [ModuleName | !.ModuleNames]
    ;
        SymName = qualified(SymModuleName, _),
        ( if partial_sym_name_matches_full(SymModuleName, ModuleName) then
            !:ModuleNames = [ModuleName | !.ModuleNames]
        else
            true
        )
    ).

%---------------------%

:- func report_any_missing_module_qualifiers(type_error_clause_context,
    prog_context, string, set(module_name)) = list(error_msg).

report_any_missing_module_qualifiers(ClauseContext, Context,
        ItemName, ModuleNamesSet0) = Msgs :-
    % We take a set of module names instead of a list, because we want
    % to report the name of each module just once, even if it defines
    % more than one entity with the name that got the error.
    % (Our caller can put the module name into the set more than once:
    % once for each such entity.)

    % For entities defined in the current module and in its ancestors,
    % access via use_module vs import_module is irrelevant.
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_name(ModuleInfo, ModuleName),
    set.delete_list([ModuleName | get_ancestors(ModuleName)],
        ModuleNamesSet0, ModuleNamesSet1),
    % Users are not supposed to know about private_builtin.m at all.
    set.delete(mercury_private_builtin_module,
        ModuleNamesSet1, ModuleNamesSet),
    set.to_sorted_list(ModuleNamesSet, ModuleNames),
    (
        ModuleNames = [],
        Msgs = []
    ;
        ModuleNames = [HeadModuleName | TailModuleNames],
        IsDefinedInPieces =
            [words("That"), words(ItemName), words("is defined in")],
        (
            TailModuleNames = [],
            ModuleNamesPieces =
                color_as_subject([qual_sym_name(HeadModuleName), suffix(",")]),
            ModulesPieces = [words("module")] ++ ModuleNamesPieces,
            NoImportsPieces =
                [words("which does not have an"),
                decl("import_module"), words("declaration.")]
        ;
            TailModuleNames = [_ | TailTailModuleNames],
            ModuleNamePieces =
                list.map(func(MN) = qual_sym_name(MN), ModuleNames),
            ModuleNamesPieces =
                component_list_to_color_pieces(yes(color_subject), "and",
                    [suffix(",")], ModuleNamePieces),
            ModulesPieces = [words("modules")] ++ ModuleNamesPieces,
            (
                TailTailModuleNames = [],
                NoImportsPieces =
                    [words("neither of which has an"),
                    decl("import_module"), words("declaration.")]
            ;
                TailTailModuleNames = [_ | _],
                NoImportsPieces =
                    [words("none of which have"),
                    decl("import_module"), words("declarations."), nl]
            )
        ),
        MainPieces = IsDefinedInPieces ++ ModulesPieces ++
            color_as_possible_cause(NoImportsPieces) ++ [nl],
        MainMsg = msg(Context, MainPieces),
        VerbosePieces = [words("Note that symbols defined in modules"),
            words("accessed via"), decl("use_module"), words("declarations"),
            words("must always be fully module qualified."), nl],
        VerboseMsg = simple_msg(Context,
            [verbose_only(verbose_once, VerbosePieces)]),
        Msgs = [MainMsg, VerboseMsg]
    ).

%---------------------------------------------------------------------------%

:- pred maybe_report_missing_import_addendum(type_error_clause_context::in,
    module_name::in, list(format_piece)::out, list(module_name)::out)
    is det.

maybe_report_missing_import_addendum(ClauseContext, ModuleQualifier,
        Pieces, MissingImportModules) :-
    % First check if this module wasn't imported.
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_visible_modules(ModuleInfo, VisibleModules),

    set.filter(partial_sym_name_matches_full(ModuleQualifier),
        VisibleModules, MatchingVisibleModules),
    ( if set.is_empty(MatchingVisibleModules) then
        % The module qualifier does not match any of the visible modules,
        % so we report that the module has not been imported.
        Pieces = [nl,
            words("(The module")] ++
            color_as_subject([qual_sym_name(ModuleQualifier)]) ++
            color_as_possible_cause([words("has not been imported.)")]) ++
            [nl],
        MissingImportModules = [ModuleQualifier]
    else
        % The module qualifier matches one or more of the visible modules.
        % But maybe the user forgot to import the parent module(s) of that
        % module...
        find_unimported_ancestors(VisibleModules, MatchingVisibleModules,
            UnimportedAncestorsSet),
        set.to_sorted_list(UnimportedAncestorsSet, UnimportedAncestors),
        (
            UnimportedAncestors = [_ | _],
            Pieces = report_unimported_ancestors(UnimportedAncestors),
            % Since ModuleQualifier has unimported parents, its own import,
            % if any, has no effect.
            ModuleQualifierList = sym_name_to_list(ModuleQualifier),
            AddAncestor =
                ( func(AncestorMN) = MN :-
                    AncestorMNList = sym_name_to_list(AncestorMN),
                    FullList = AncestorMNList ++ ModuleQualifierList,
                    det_list_to_sym_name(FullList, MN)
                ),
            MissingImportModules =
                [ModuleQualifier | list.map(AddAncestor, UnimportedAncestors)]
        ;
            UnimportedAncestors = [],
            Pieces = [],
            MissingImportModules = []
        )
    ).

    % Return all the ancestors of modules in MatchingModuleNames
    % which are not imported (i.e. not visible).
    %
:- pred find_unimported_ancestors(set(module_name)::in, set(module_name)::in,
    set(module_name)::out) is det.

find_unimported_ancestors(VisibleModules, MatchingModuleNames,
        UnimportedAncestors) :-
    MatchingModuleNamesAncestorSets =
        set.map(get_ancestors_set, MatchingModuleNames),
    set.power_union(MatchingModuleNamesAncestorSets,
        MatchingModuleNamesAncestors),
    set.difference(MatchingModuleNamesAncestors, VisibleModules,
        UnimportedAncestors).

:- func report_unimported_ancestors(list(module_name))
    = list(format_piece).

report_unimported_ancestors(UnimportedAncestors) = Pieces :-
    UnimportedAncestorPieces =
        list.map((func(M) = qual_sym_name(M)), UnimportedAncestors),
    UnimportedAncestorListPieces =
        component_list_to_color_pieces(yes(color_subject), "and", [],
            UnimportedAncestorPieces),
    ( if UnimportedAncestors = [_] then
        Pieces = [words("(The possible parent module")] ++
            UnimportedAncestorListPieces ++
            color_as_incorrect([words("has not been imported.)")]) ++
            [nl]
    else
        Pieces = [words("(The possible parent modules")] ++
            UnimportedAncestorListPieces ++
            color_as_incorrect([words("have not been imported.)")]) ++
            [nl]
    ).

%---------------------------------------------------------------------------%

    % arity_error_to_pieces(PredOrFunc, Arity, CorrectArities):
    %
    % Return a description for the error message
    % "wrong number of arguments (<Arity>; should be <CorrectArities>)",
    % adjusting `Arity' and `CorrectArities' if `MaybePredOrFunc' is
    % `yes(function)'.
    %
:- func arity_error_to_pieces(pred_or_func, int, list(int)) =
    list(format_piece).

arity_error_to_pieces(PredOrFunc, Arity0, Arities0) = Pieces :-
    (
        PredOrFunc = pf_predicate,
        Arity = Arity0,
        Arities = Arities0
    ;
        PredOrFunc = pf_function,
        % Adjust arities for functions.
        adjust_func_arity(pf_function, Arity, Arity0),
        ReverseAdjust =
            ( pred(OtherArity0::in, OtherArity::out) is det :-
                adjust_func_arity(pf_function, OtherArity, OtherArity0)
            ),
        list.map(ReverseAdjust, Arities0, Arities)
    ),
    ActualArityPieces = color_as_incorrect([suffix(int_to_string(Arity))]),
    ExpectedArityPieces = list.map((func(N) = int_fixed(N)), Arities),
    ExpectedAritiesPieces = component_list_to_color_pieces(yes(color_correct),
        "or", [], ExpectedArityPieces),
    Pieces = color_as_incorrect([words("wrong number of arguments")]) ++
        [prefix("(") | ActualArityPieces] ++ [suffix(";"),
        words("should be") | ExpectedAritiesPieces] ++ [suffix(")")].

%---------------------------------------------------------------------------%

:- pred get_known_pred_info_names(predicate_table::in, pred_or_func::in,
    list(string)::out) is det.

get_known_pred_info_names(PredicateTable, RequiredPredOrFunc, KnownNames) :-
    predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
    map.values(PredIdTable, PredInfos),
    list.foldl(acc_known_pred_info_names(RequiredPredOrFunc), PredInfos,
        [], KnownNames0),
    list.sort_and_remove_dups(KnownNames0, KnownNames).

:- pred acc_known_pred_info_names(pred_or_func::in, pred_info::in,
    list(string)::in, list(string)::out) is det.

acc_known_pred_info_names(RequiredPredOrFunc, PredInfo, !KnownNames) :-
    pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
    % Note that we don't care whether PredInfo is valid or not; even if
    % it is invalid, it could be the one the programmer intended to reference.
    ( if PredOrFunc = RequiredPredOrFunc then
        pred_info_get_name(PredInfo, Name),
        !:KnownNames = [Name | !.KnownNames]
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_error_undef.
%---------------------------------------------------------------------------%
