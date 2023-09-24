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

:- func report_error_undef_pred(type_error_clause_context,
    prog_context, sym_name_pred_form_arity) = error_spec.

%---------------------------------------------------------------------------%

:- type cons_error
    --->    foreign_type_constructor(type_ctor, hlds_type_defn)
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
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
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

report_error_undef_pred(ClauseContext, Context, SymNameArity) = Spec :-
    SymNameArity = sym_name_pred_form_arity(SymName, PredFormArity),
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    PredMarkers = ClauseContext ^ tecc_pred_markers,
    IsFullyQualified = calls_are_fully_qualified(PredMarkers),
    predicate_table_lookup_pf_sym(PredicateTable, IsFullyQualified,
        pf_predicate, SymName, OtherIds),
    PFSymNameArity = pf_sym_name_arity(pf_predicate, SymName, PredFormArity),
    (
        OtherIds = [_ | _],
        predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
        find_pred_arities(PredIdTable, OtherIds, PredFormArities),
        Spec = report_error_pred_num_args(ClauseContext, Context,
            PFSymNameArity, PredFormArities)
    ;
        OtherIds = [],
        InClauseForPieces = in_clause_for_pieces(ClauseContext),
        InClauseForComponent = always(InClauseForPieces),

        is_undef_pred_reference_special(ClauseContext, PFSymNameArity,
            UndefClass),
        (
            UndefClass = undef_special(SpecialComponents),
            UndefMsg = simple_msg(Context,
                [InClauseForComponent | SpecialComponents]),
            Msgs = [UndefMsg]
        ;
            UndefClass = undef_ordinary(MissingImportModules, AddeddumPieces),
            MainPieces = [words("error: undefined"),
                qual_pf_sym_name_pred_form_arity(PFSymNameArity),
                suffix("."), nl],
            UndefMsg = simple_msg(Context,
                [InClauseForComponent, always(MainPieces ++ AddeddumPieces)]),

            predicate_table_lookup_pf_sym(PredicateTable,
                may_be_partially_qualified, pf_function, SymName,
                FuncOtherIds),
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
            Msgs = [UndefMsg] ++ KindMsgs ++ QualMsgs
        ),
        Spec = error_spec($pred, severity_error, phase_type_check, Msgs)
    ).

%---------------------%

:- func report_error_pred_num_args(type_error_clause_context, prog_context,
    pf_sym_name_arity, list(pred_form_arity)) = error_spec.

report_error_pred_num_args(ClauseContext, Context, PFSymNameArity,
        AllPredFormArities) = Spec :-
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, SymName, PredFormArity),
    PredFormArity = pred_form_arity(PredFormArityInt),
    AllPredFormArityInts =
        list.map(project_pred_form_arity_int, AllPredFormArities),
    MainPieces = in_clause_for_pieces(ClauseContext) ++
        [words("error:")] ++
        arity_error_to_pieces(PredOrFunc, PredFormArityInt,
            AllPredFormArityInts) ++ [nl] ++
        [words("in call to"), p_or_f(PredOrFunc), qual_sym_name(SymName),
        suffix("."), nl],
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
            words("module that used to be named"), quote(PredName),
            words("has been renamed to"), quote(PredName ++ "_io"),
            suffix("."), nl]
    else
        SpecialPieces = []
    ),
    Spec = simplest_spec($pred, severity_error, phase_type_check,
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
    pf_sym_name_arity::in, undef_class::out) is det.

is_undef_pred_reference_special(ClauseContext, PFSymNameArity, UndefClass) :-
    PFSymNameArity =
        pf_sym_name_arity(_PredOrFunc, PredSymName, PredFormArity),
    PredFormArity = pred_form_arity(PredFormArityInt),
    ( if
        PredSymName = unqualified("->"),
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 )
    then
        MainPieces = [words("error:"), quote("->"), words("without"),
            quote(";"), suffix("."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the else part is not optional."), nl,
            words("Every if-then must have an else."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        Components = [MainComponent, VerboseComponent],
        UndefClass = undef_special(Components)
    else if
        PredSymName = unqualified("else"),
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 )
    then
        Components = [always([words("error: unmatched"), quote("else"),
            suffix("."), nl])],
        UndefClass = undef_special(Components)
    else if
        PredSymName = unqualified("if"),
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 )
    then
        Pieces = [words("error:"), quote("if"), words("without"),
            quote("then"), words("or"), quote("else"), suffix("."), nl],
        UndefClass = undef_special([always(Pieces)])
    else if
        PredSymName = unqualified("then"),
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 )
    then
        MainPieces = [words("error:"), quote("then"), words("without"),
            quote("if"), words("or"), quote("else"), suffix("."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the"), quote("else"), words("part is not optional."),
            nl, words("Every if-then must have an"),
            quote("else"), suffix("."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        UndefClass = undef_special([MainComponent, VerboseComponent])
    else if
        PredSymName = unqualified("apply"),
        PredFormArityInt >= 1
    then
        UndefClass = undef_special(report_apply_instead_of_pred)
    else if
        PredSymName = unqualified(PurityString),
        PredFormArityInt = 1,
        ( PurityString = "impure" ; PurityString = "semipure" )
    then
        MainPieces = [words("error:"), quote(PurityString),
            words("marker in an inappropriate place."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Such markers only belong before predicate calls."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        UndefClass = undef_special([MainComponent, VerboseComponent])
    else if
        PredSymName = unqualified("some"),
        PredFormArityInt = 2
    then
        Pieces = [words("syntax error in existential quantification:"),
            words("first argument of"), quote("some"),
            words("should be a list of variables."), nl],
        UndefClass = undef_special([always(Pieces)])
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
                ( PredFormArityInt = 4 ; PredFormArityInt = 5 ;
                PredFormArityInt = 6 ; PredFormArityInt = 7 ),
                NewPredName = "process_options"
            ;
                PredName = "process_options_track_se",
                ( PredFormArityInt = 7 ; PredFormArityInt = 9 ),
                NewPredName = "process_options_track"
            )
        then
            GetoptPieces =
                [words("One possible reason for the error is that"),
                words("the predicate"), quote(PredName),
                words("in the Mercury standard library has been renamed to"),
                quote(NewPredName), suffix("."), nl]
        else
            GetoptPieces = []
        ),
        AddeddumPieces = MissingImportAddeddumPieces ++ GetoptPieces,
        UndefClass = undef_ordinary(MissingImportModules, AddeddumPieces)
    ).

:- func report_apply_instead_of_pred = list(error_msg_component).

report_apply_instead_of_pred = Components :-
    MainPieces = [words("error: the language construct"), quote("apply"),
        words("should be used as an expression, not as a goal."), nl],
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

:- func report_error_func_instead_of_pred(prog_context) = error_msg.

report_error_func_instead_of_pred(Context) = Msg :-
    Pieces = [words("(There is a *function* with that name, however."), nl,
        words("Perhaps you forgot to add"), quote(" = ..."), suffix("?)"), nl],
    Msg = simplest_msg(Context, Pieces).

%---------------------%

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
    MainPieces = [words("error: the language construct"),
        unqual_sym_name_arity(sym_name_arity(unqualified(Name), Arity)),
        words("should be used as a goal, not as an expression."), nl],
    VerbosePieces = [words("If you are trying to use a goal"),
        words("as a boolean function, you should write"),
        words_quote("if <goal> then yes else no"), words("instead."), nl],
    ( if Name = "call" then
        VerboseCallPieces =
            [words("If you are trying to invoke a higher-order function,"),
            words("you should use"), quote("apply"), suffix(","),
            words("not"), quote("call"), suffix("."), nl,
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

syntax_functor_components("else", 2, Components) :-
    Pieces = [words("error: unmatched"), quote("else"), suffix("."), nl],
    Components = [always(Pieces)].
syntax_functor_components("if", 2, Components) :-
    Pieces = [words("error:"), quote("if"), words("without"), quote("then"),
         words("or"), quote("else"), suffix("."), nl],
    Components = [always(Pieces)].
syntax_functor_components("then", 2, Components) :-
    Pieces1 = [words("error:"), quote("then"), words("without"),
        quote("if"), words("or"), quote("else"), suffix("."), nl],
    Pieces2 = [words("Note: the"), quote("else"),
        words("part is not optional."),
        words("Every if-then must have an"), quote("else"), suffix("."), nl],
    Components = [always(Pieces1), verbose_only(verbose_once, Pieces2)].
syntax_functor_components("->", 2, Components) :-
    Pieces1 = [words("error:"), quote("->"), words("without"),
        quote(";"), suffix("."), nl],
    Pieces2 = [words("Note: the else part is not optional."),
        words("Every if-then must have an else."), nl],
    Components = [always(Pieces1), verbose_only(verbose_once, Pieces2)].
syntax_functor_components("^", 2, Components) :-
    Pieces1 = [words("error: invalid use of field selection operator"),
        prefix("("), quote("^"), suffix(")."), nl],
    Pieces2 = [words("This is probably some kind of syntax error."),
        words("The field name must be an atom,"),
        words("not a variable or other term."), nl],
    Components = [always(Pieces1), verbose_only(verbose_always, Pieces2)].
syntax_functor_components(":=", 2, Components) :-
    Pieces1 = [words("error: invalid use of field update operator"),
        prefix("("), quote(":="), suffix(")."), nl],
    Pieces2 = [words("This is probably some kind of syntax error."), nl],
    Components = [always(Pieces1), verbose_only(verbose_always, Pieces2)].
syntax_functor_components(":-", 2, Components) :-
    Pieces = [words("syntax error in lambda expression"),
         prefix("("), quote(":-"), suffix(")."), nl],
    Components = [always(Pieces)].
syntax_functor_components("-->", 2, Components) :-
    Pieces = [words("syntax error in DCG lambda expression"),
        prefix("("), quote("-->"), suffix(")."), nl],
    Components = [always(Pieces)].
syntax_functor_components(".", 2, Components) :-
    Pieces = [words("error: the list constructor is now"),
        unqual_sym_name_arity(sym_name_arity(unqualified("[|]"), 2)),
        suffix(","), words("not"), quote("./2"),
        suffix("."), nl],
    Components = [always(Pieces)].
syntax_functor_components("!", 1, Components) :-
    Pieces1 = [words("error: invalid use of"), quote("!"),
        words("state variable operator."), nl],
    Pieces2 = [words("You probably meant to use"), quote("!."),
        words("or"), quote("!:"), suffix("."), nl],
    Components = [always(Pieces1), verbose_only(verbose_always, Pieces2)].

%---------------------%

:- pred report_error_undef_cons_std(type_error_clause_context::in,
    prog_context::in, error_msg_component::in, list(cons_error)::in,
    cons_id::in, arity::in, error_spec::out) is det.

report_error_undef_cons_std(ClauseContext, Context, InitComp, ConsErrors,
        Functor, Arity, Spec) :-
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_cons_table(ModuleInfo, ConsTable),
    ( if
        Functor = cons(Constructor, FunctorArity, _),
        expect(unify(Arity, FunctorArity), $pred, "arity mismatch"),

        return_cons_arities(ConsTable, Constructor, ConsArities),

        module_info_get_predicate_table(ModuleInfo, PredTable),
        predicate_table_lookup_sym(PredTable, may_be_partially_qualified,
            Constructor, PredIds),
        return_function_arities(ModuleInfo, PredIds, [], FuncArities),

        list.sort_and_remove_dups(ConsArities ++ FuncArities, AllArities),
        list.delete_all(AllArities, Arity, OtherArities),
        OtherArities = [_ | _]
    then
        FunctorPieces = wrong_arity_constructor_to_pieces(Constructor,
            Arity, OtherArities),
        FunctorComps = [always(FunctorPieces)],
        % The code that constructs QualMsgs below uses wording that
        % can be misleading in the presence of arity mismatches.
        QualMsgs = []
    else
        Pieces1 = [words("error: undefined symbol"),
            qual_cons_id_and_maybe_arity(Functor), suffix("."), nl],
        ( if
            Functor = cons(Constructor, _, _),
            Constructor = qualified(ModQual, _)
        then
            maybe_report_missing_import_addendum(ClauseContext, ModQual,
                Pieces2, MissingImportModules)
        else if
            Functor = cons(unqualified("[|]"), 2, _)
        then
            maybe_report_missing_import_addendum(ClauseContext,
                unqualified("list"), Pieces2, MissingImportModules)
        else
            Pieces2 = [],
            MissingImportModules = []
        ),
        FunctorComps = [always(Pieces1 ++ Pieces2)],
        ( if Functor = cons(FunctorName, _, _) then
            return_cons_defns_with_given_name(ConsTable,
                unqualify_name(FunctorName), ConsDefns),
            list.foldl(accumulate_matching_cons_module_names(FunctorName),
                ConsDefns, [], ConsModuleNames),
            module_info_get_predicate_table(ModuleInfo, PredicateTable),
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
                Context, "symbol", ModuleNamesSet)
        else
            QualMsgs = []
        )
    ),
    (
        ConsErrors = [],
        ConsMsgs = []
    ;
        ConsErrors = [_ | _],
        ConsMsgLists = list.map(report_cons_error(Context), ConsErrors),
        list.condense(ConsMsgLists, ConsMsgs)
    ),
    Spec = error_spec($pred, severity_error, phase_type_check,
        [simple_msg(Context,
            [InitComp | FunctorComps]) | ConsMsgs] ++ QualMsgs).

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
    % predicates' ariries.
    NumArgsPieces =
        arity_error_to_pieces(pf_predicate, Arity, ActualArities),
    Pieces = [words("error: ")] ++ NumArgsPieces ++
        [words("in use of constructor"), qual_sym_name(Name), suffix(".")].

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
        ConsError = foreign_type_constructor(TypeCtor, _),
        Pieces = [words("There are"),
            pragma_decl("foreign_type"), words("declarations"),
            words("for type"), qual_type_ctor(TypeCtor), suffix(","),
            words("so it is treated as an abstract type"),
            words("in all predicates and functions"),
            words("which are not implemented"),
            words("for those foreign types."), nl],
        Msgs = [simplest_msg(Context, Pieces)]
    ;
        ConsError = abstract_imported_type,
        % For `abstract_imported_type' errors, the "undefined symbol"
        % error written by `report_error_undef_cons' is sufficient so
        % we do not print an additional error message here.
        Msgs = []
    ;
        ConsError = invalid_field_update(FieldName, FieldDefn, TVarSet, TVars),
        FieldDefn = hlds_ctor_field_defn(DefnContext, _, _, ConsId, _),
        Pieces1 = [words("Field"), unqual_sym_name(FieldName),
            words("cannot be updated because"),
            words("the existentially quantified type")],
        (
            TVars = [],
            unexpected($pred, "no type variables")
        ;
            TVars = [TVar],
            TVarsStr = mercury_var_to_name_only_vs(TVarSet, TVar),
            Pieces2 = [words("variable"), quote(TVarsStr), words("occurs")]
        ;
            TVars = [_, _ | _],
            TVarsStr = mercury_vars_to_name_only_vs(TVarSet, TVars),
            Pieces2 = [words("variables"), quote(TVarsStr), words("occur")]
        ),
        Pieces3 = [words("in the types of field"), unqual_sym_name(FieldName),
            words("and some other field"),
            words("in definition of constructor"),
            qual_cons_id_and_maybe_arity(ConsId), suffix("."), nl],
        Pieces = Pieces1 ++ Pieces2 ++ Pieces3,
        Msgs = [simplest_msg(DefnContext, Pieces)]
    ;
        ConsError = new_on_non_existential_type(TypeCtor),
        Pieces = [words("Invalid use of"), quote("new"),
            words("on a constructor of type"), qual_type_ctor(TypeCtor),
            words("which is not existentially typed."), nl],
        Msgs = [simplest_msg(Context, Pieces)]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The implementation of report_error_undef_event.
%

report_error_undef_event(Context, EventName) = Spec :-
    Pieces = [words("Error: there is no event named"),
        quote(EventName), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_type_check,
        Context, Pieces).

report_error_undef_event_arity(Context, EventName, EventArgTypes, Args)
        = Spec :-
    pred_form_arity(ActualArity) = arg_list_arity(Args),
    pred_form_arity(ExpectedArity) = arg_list_arity(EventArgTypes),
    Pieces = [words("Error:")] ++
        arity_error_to_pieces(pf_predicate, ActualArity, [ExpectedArity]) ++
        [words("in event"), quote(EventName), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_type_check,
        Context, Pieces).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The implementation of maybe_report_no_clauses*.
%

maybe_report_no_clauses(ModuleInfo, PredId, PredInfo) = Specs :-
    ShouldReport = should_report_no_clauses(ModuleInfo, PredInfo),
    (
        ShouldReport = yes,
        PredPieces = describe_one_pred_name(ModuleInfo,
            should_not_module_qualify, PredId),
        Pieces = [words("Error: no clauses for") | PredPieces] ++
            [suffix("."), nl],
        pred_info_get_context(PredInfo, Context),

        Spec = simplest_spec($pred, severity_error, phase_type_check,
            Context, Pieces),
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
            PredPieces = describe_one_pred_name(ModuleInfo,
                should_not_module_qualify, PredId),
            Pieces = [words("Warning: no clauses for ") | PredPieces] ++
                [suffix("."), nl],
            pred_info_get_context(PredInfo, Context),
            Spec = simplest_spec($pred, severity_warning,
                phase_type_check, Context, Pieces),
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
        (
            TailModuleNames = [],
            MainPieces = [words("That"), words(ItemName), words("is defined"),
                words("in module"), qual_sym_name(HeadModuleName), suffix(","),
                words("which does not have an"),
                decl("import_module"), words("declaration."), nl]
        ;
            TailModuleNames = [_ | _],
            ModuleNamePieces =
                list.map(func(MN) = qual_sym_name(MN), ModuleNames),
            ModuleNamesPieces =
                component_list_to_pieces("and", ModuleNamePieces),
            MainPieces = [words("That"), words(ItemName), words("is defined"),
                words("in modules")] ++ ModuleNamesPieces ++ [suffix(","),
                words("none of which have"),
                decl("import_module"), words("declarations."), nl]
        ),
        MainMsg = simplest_msg(Context, MainPieces),
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
        Pieces = [nl, words("(The module"), qual_sym_name(ModuleQualifier),
            words("has not been imported.)"), nl],
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
    UnimportedAncestorDescs = list.map(describe_sym_name, UnimportedAncestors),
    AllUnimportedAncestors = list_to_pieces(UnimportedAncestorDescs),
    ( if AllUnimportedAncestors = [_] then
        Pieces = [words("(The possible parent module")] ++
            AllUnimportedAncestors ++ [words("has not been imported.)"), nl]
    else
        Pieces = [words("(The possible parent modules")] ++
            AllUnimportedAncestors ++ [words("have not been imported.)"), nl]
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
    RightAritiesPieces = arities_to_pieces(Arities),
    Pieces = [words("wrong number of arguments ("),
        suffix(int_to_string(Arity)), suffix(";"),
        words("should be") | RightAritiesPieces] ++ [suffix(")")].

:- func arities_to_pieces(list(int)) = list(format_piece).

arities_to_pieces([]) = [].
arities_to_pieces([Arity | Arities]) = Pieces :-
    TailPieces = arities_to_pieces(Arities),
    ArityPiece = fixed(int_to_string(Arity)),
    (
        Arities = [],
        Pieces = [ArityPiece | TailPieces]
    ;
        Arities = [_],
        Pieces = [ArityPiece, words("or") | TailPieces]
    ;
        Arities = [_, _ | _],
        Pieces = [ArityPiece, suffix(",") | TailPieces]
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_error_undef.
%---------------------------------------------------------------------------%
