%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2016-2024 The Mercury team.
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
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_pragma_analysis.
:- import_module parse_tree.parse_pragma_foreign.
:- import_module parse_tree.parse_pragma_tabling.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_defn.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_test.

:- import_module cord.
:- import_module counter.
:- import_module int.
:- import_module maybe.
:- import_module one_or_more.
:- import_module set.
:- import_module string.
:- import_module term_int.

%---------------------------------------------------------------------------%

parse_pragma(ModuleName, VarSet, PragmaTerms, Context, SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [PragmaTerm],
        PragmaTerm = term.functor(term.atom(PragmaName), PragmaArgTerms,
            PragmaContext)
    then
        ( if
            parse_named_pragma(ModuleName, VarSet, PragmaTerm,
                PragmaName, PragmaArgTerms, PragmaContext, SeqNum,
                MaybeIOMPrime)
        then
            MaybeIOM = MaybeIOMPrime
        else
            Pieces = [words("Error:"), quote(PragmaName),
                words("is not a recognized pragma name."), nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            MaybeIOM = error1([Spec])
        )
    else
        Spec = report_unrecognized_pragma(Context),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_named_pragma(module_name::in, varset::in, term::in,
    string::in, list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is semidet.

parse_named_pragma(ModuleName, VarSet, ErrorTerm, PragmaName, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    % The Context argument is redundant, since we can always compute it
    % as get_term_context(ErrorTerm). However, we need to compute the context
    % both when a parse fails (to report the error) and when it succeeds
    % (to set the context of the item we construct and return).
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
        parse_pragma_external(ModuleName, VarSet, ErrorTerm,
            PragmaName, PragmaTerms, Context, SeqNum, PorF, MaybeIOM)
    ;
        PragmaName = "obsolete",
        parse_pragma_obsolete(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "obsolete_proc",
        parse_pragma_obsolete_proc(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "format_call",
        parse_pragma_format_call(ModuleName, PragmaTerms, ErrorTerm, VarSet,
            Context, SeqNum, MaybeIOM)
    ;
        (
            PragmaName = "terminates",
            MarkerKind = dpmk_terminates
        ;
            PragmaName = "does_not_terminate",
            MarkerKind = dpmk_does_not_terminate
        ;
            PragmaName = "check_termination",
            MarkerKind = dpmk_check_termination
        ),
        parse_name_arity_decl_pragma(ModuleName, PragmaName, MarkerKind,
            VarSet, ErrorTerm, PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        (
            PragmaName = "inline",
            MarkerKind = ipmk_inline
        ;
            PragmaName = "no_inline",
            MarkerKind = ipmk_no_inline
        ;
            PragmaName = "consider_used",
            MarkerKind = ipmk_consider_used
        ;
            PragmaName = "mode_check_clauses",
            MarkerKind = ipmk_mode_check_clauses
        ;
            PragmaName = "no_determinism_warning",
            MarkerKind = ipmk_no_detism_warning
        ;
            PragmaName = "promise_pure",
            MarkerKind = ipmk_promise_pure
        ;
            PragmaName = "promise_semipure",
            MarkerKind = ipmk_promise_semipure
        ;
            PragmaName = "promise_equivalent_clauses",
            MarkerKind = ipmk_promise_eqv_clauses
        ),
        parse_name_arity_impl_pragma(ModuleName, PragmaName, MarkerKind,
            VarSet, ErrorTerm, PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "require_tail_recursion",
        parse_pragma_require_tail_recursion(ModuleName, PragmaName,
            PragmaTerms, ErrorTerm, VarSet, Context, SeqNum, MaybeIOM)
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
            TabledMethod = tabled_memo(table_attr_ignore_with_warning)
        ;
            PragmaName = "loop_check",
            TabledMethod = tabled_loop_check
        ;
            PragmaName = "minimal_model",
            % We don't yet know whether we will use the stack_copy or the
            % own_stacks technique for computing minimal models. The decision
            % depends on the grade, and is made in make_hlds.m; the
            % "stack_copy" here is just a placeholder.
            TabledMethod = tabled_minimal(stack_copy)
        ),
        parse_tabling_pragma(ModuleName, VarSet, ErrorTerm,
            PragmaName, PragmaTerms, Context, SeqNum, TabledMethod, MaybeIOM)
    ;
        PragmaName = "unused_args",
        parse_pragma_unused_args(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        PragmaName = "type_spec_constrained_preds",
        parse_pragma_type_spec_constr(ModuleName, VarSet, ErrorTerm,
            PragmaTerms, SeqNum, MaybeIOM)
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

:- func report_unrecognized_pragma(prog_context) = error_spec.

report_unrecognized_pragma(Context) = Spec :-
    Pieces = [words("Error: a"), decl("pragma"), words("declaration"),
        words("should have the form"),
        quote(":- pragma pragma_name(pragma_arguments)."), nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

%---------------------------------------------------------------------------%
%
% Parse the sole argument of a (decl or impl) pragma that should contain
% a symbol name / arity pair.
%

:- pred parse_name_arity_decl_pragma(module_name::in, string::in,
    decl_pragma_marker_kind::in,
    varset::in, term::in, list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_name_arity_decl_pragma(ModuleName, PragmaName, MarkerKind,
        VarSet, ErrorTerm, PragmaTerms, Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PragmaTerm],
        parse_pred_pfu_name_arity(ModuleName, PragmaName, VarSet,
            PragmaTerm, MaybePredSpec),
        (
            MaybePredSpec = ok1(PredSpec),
            Marker = item_decl_marker_info(MarkerKind, PredSpec,
                Context, SeqNum),
            Item = item_decl_marker(Marker),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybePredSpec = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl(PragmaName),
            words("declaration must have exactly one argument."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

:- pred parse_name_arity_impl_pragma(module_name::in, string::in,
    impl_pragma_marker_kind::in,
    varset::in, term::in, list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_name_arity_impl_pragma(ModuleName, PragmaName, MarkerKind,
        VarSet, ErrorTerm, PragmaTerms, Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PragmaTerm],
        parse_pred_pfu_name_arity(ModuleName, PragmaName, VarSet,
            PragmaTerm, MaybePredSpec),
        (
            MaybePredSpec = ok1(PredSpec),
            Marker = item_impl_marker_info(MarkerKind, PredSpec,
                Context, SeqNum),
            Item = item_impl_marker(Marker),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybePredSpec = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl(PragmaName),
            words("declaration must have exactly one argument."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

%---------------------------------------------------------------------------%

% The predicates in the rest of this module are to be clustered together
% into groups of related predicates. All groups but the last contain
% the main predicate for parsing one kind of pragma, followed by its
% dedicated helper predicates.

%---------------------------------------------------------------------------%
%
% Parse source_file pragmas.
%

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
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            MaybeIOM = error1([Spec])
        )
    else
        Pieces = [words("Error: a"), pragma_decl("source_file"),
            words("declaration must have exactly one argument."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Parse external_pred and external_proc pragmas.
%

:- pred parse_pragma_external(module_name::in, varset::in, term::in,
    string::in, list(term)::in, prog_context::in, item_seq_num::in,
    pred_or_func::in, maybe1(item_or_marker)::out) is det.

parse_pragma_external(ModuleName, VarSet, ErrorTerm, PragmaName, PragmaTerms,
        Context, SeqNum, PorF, MaybeIOM) :-
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
                PFNameArity = pred_pf_name_arity(PorF, FullSymName,
                    user_arity(Arity)),
                External = impl_pragma_external_proc_info(PFNameArity,
                    MaybeBackend, Context, SeqNum),
                Item = item_impl_pragma(impl_pragma_external_proc(External)),
                MaybeIOM = ok1(iom_item(Item))
            else
                Pieces = [words("Error: the predicate name in the")] ++
                    cord.list(ContextPieces1) ++
                    [words("is not for the expected module, which is"),
                    qual_sym_name(ModuleName), suffix("."), nl],
                Spec = spec($pred, severity_error, phase_t2pt,
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
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_symname_arity(varset::in, term::in, cord(format_piece)::in,
    maybe2(sym_name, arity)::out) is det.

parse_symname_arity(VarSet, PredTerm, ContextPieces, MaybeSymNameArity) :-
    ( if PredTerm = term.functor(term.atom("/"), [NameTerm, ArityTerm], _) then
        parse_symbol_name(VarSet, NameTerm, MaybeSymName),
        ( if term_int.decimal_term_to_int(ArityTerm, ArityPrime) then
            MaybeArity = ok1(ArityPrime)
        else
            ArityPieces = [words("Error: in")] ++ cord.list(ContextPieces) ++
                [suffix(":"), words("the arity must be an integer."), nl],
            AritySpec = spec($pred, severity_error, phase_t2pt,
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
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(PredTerm), Pieces),
        MaybeSymNameArity = error2([Spec])
    ).

:- pred parse_pragma_external_options(varset::in, maybe(term)::in,
    cord(format_piece)::in, maybe1(maybe(backend))::out) is det.

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
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(OptionsTerm), Pieces),
            MaybeMaybeBackend = error1([Spec])
        )
    ).

%---------------------------------------------------------------------------%
%
% Parse obsolete and obsolete_proc pragmas.
%

:- pred parse_pragma_obsolete(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_obsolete(ModuleName, PragmaTerms, ErrorTerm, VarSet,
        Context, SeqNum, MaybeIOM) :-
    (
        (
            PragmaTerms = [PredSpecTerm],
            MaybeObsoleteInFavourOf = ok1([])
        ;
            PragmaTerms = [PredSpecTerm, ObsoleteInFavourOfTerm],
            parse_pragma_obsolete_in_favour_of(ObsoleteInFavourOfTerm,
                VarSet, MaybeObsoleteInFavourOf)
        ),
        parse_pred_pfu_name_arity(ModuleName, "obsolete",
            VarSet, PredSpecTerm, MaybePredSpec),
        ( if
            MaybePredSpec = ok1(PredSpec),
            MaybeObsoleteInFavourOf = ok1(ObsoleteInFavourOf)
        then
            Obsolete = decl_pragma_obsolete_pred_info(PredSpec,
                ObsoleteInFavourOf, Context, SeqNum),
            Item = item_decl_pragma(decl_pragma_obsolete_pred(Obsolete)),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs =
                get_any_errors1(MaybePredSpec) ++
                get_any_errors1(MaybeObsoleteInFavourOf),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: an"), pragma_decl("obsolete"),
            words("declaration must have one or two arguments."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

:- pred parse_pragma_obsolete_proc(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

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
        parse_pred_or_func_and_arg_modes(yes(ModuleName),
            PredAndModesContextPieces, VarSet, PredAndModesTerm,
            MaybePredAndModes),
        ( if
            MaybePredAndModes = ok3(PredName, PredOrFunc, Modes),
            MaybeObsoleteInFavourOf = ok1(ObsoleteInFavourOf)
        then
            PredNameModesPF = proc_pf_name_modes(PredOrFunc, PredName, Modes),
            Obsolete = decl_pragma_obsolete_proc_info(PredNameModesPF,
                ObsoleteInFavourOf, Context, SeqNum),
            Item = item_decl_pragma(decl_pragma_obsolete_proc(Obsolete)),
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
        Spec = spec($pred, severity_error, phase_t2pt,
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
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        MaybeObsoleteInFavourOf = error1([Spec])
    ).

:- pred parse_pragma_obsolete_in_favour_of_snas(int::in, list(term)::in,
    varset::in, maybe1(list(sym_name_arity))::out) is det.

parse_pragma_obsolete_in_favour_of_snas(_ArgNum, [], _VarSet, ok1([])).
parse_pragma_obsolete_in_favour_of_snas(ArgNum, [Term | Terms], VarSet,
        MaybeSNAs) :-
    ( if parse_sym_name_and_arity(Term, SymName, Arity) then
        MaybeHeadSNA = ok1(sym_name_arity(SymName, Arity))
    else
        Pieces = [words("In the"), nth_fixed(ArgNum),
            words("element in the second argument of"),
            pragma_decl("obsolete"), words("declaration:"), nl,
            words("error: expected a name/arity pair, got"),
            quote(describe_error_term(VarSet, Term)), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
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
%
% Parse format_call pragmas.
%

:- pred parse_pragma_format_call(module_name::in, list(term)::in, term::in,
    varset::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_format_call(ModuleName, PragmaTerms, ErrorTerm, VarSet,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredSpecTerm, FormatCallTerm],
        parse_pred_pf_name_arity(ModuleName, "format_call",
            VarSet, PredSpecTerm, MaybePredSpec),
        ( if
            maybe_parse_format_string_values(FormatCallTerm,
                MaybeFormatCallPrime)
        then
            MaybeFormatCall = MaybeFormatCallPrime
        else if
            list_term_to_term_list(FormatCallTerm, FormatCallTerms),
            FormatCallTerms = [HeadFormatCallTerm | TailFormatCallTerms]
        then
            parse_format_string_values_terms(VarSet, 1, HeadFormatCallTerm,
                TailFormatCallTerms, MaybeFormatCall)
        else
            FormatCallPieces = [words("Error: the second argument of a"),
                pragma_decl("format_call"), words("declaration"),
                words("either must be a term of the form"),
                quote("format_string_values(N, M)"),
                words("where N and M are strictly positive integers"),
                words("or a nonempty list of such terms."), nl],
            FormatCallSpec = spec($pred, severity_error, phase_t2pt,
                get_term_context(FormatCallTerm), FormatCallPieces),
            MaybeFormatCall = error1([FormatCallSpec])
        ),
        ( if
            MaybePredSpec = ok1(PredSpec),
            MaybeFormatCall = ok1(FormatCall)
        then
            FormatCallPragma = decl_pragma_format_call_info(PredSpec,
                FormatCall, Context, SeqNum),
            Item = item_decl_pragma(decl_pragma_format_call(FormatCallPragma)),
            MaybeIOM = ok1(iom_item(Item))
        else
            IOMSpecs =
                get_any_errors1(MaybePredSpec) ++
                get_any_errors1(MaybeFormatCall),
            MaybeIOM = error1(IOMSpecs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("format_call"),
            words("declaration must have two arguments."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

:- pred maybe_parse_format_string_values(term::in,
    maybe1(one_or_more(format_string_values))::out) is semidet.

maybe_parse_format_string_values(Term, MaybeOoMFormatStringValues) :-
    Term = term.functor(Functor, ArgTerms, _Context),
    Functor = term.atom("format_string_values"),
    require_det (
        parse_format_string_values_args(no, Term, ArgTerms,
            MaybeFormatStringValues),
        (
            MaybeFormatStringValues = ok1(FormatStringValues),
            MaybeOoMFormatStringValues =
                ok1(one_or_more(FormatStringValues, []))
        ;
            MaybeFormatStringValues = error1(Specs),
            MaybeOoMFormatStringValues = error1(Specs)
        )
    ).

:- pred parse_format_string_values_terms(varset::in, int::in,
    term::in, list(term)::in,
    maybe1(one_or_more(format_string_values))::out) is det.

parse_format_string_values_terms(VarSet, ListPos, HeadTerm, TailTerms,
        MaybeOoMFormatStringValues) :-
    (
        TailTerms = [],
        TailFormatStringValues = [],
        TailSpecs = []
    ;
        TailTerms = [HeadTailTerm | TailTailTerms],
        parse_format_string_values_terms(VarSet, ListPos + 1,
            HeadTailTerm, TailTailTerms, MaybeOoMTailFormatStringValues),
        (
            MaybeOoMTailFormatStringValues = ok1(OoMTailFormatStringValues),
            TailFormatStringValues =
                one_or_more_to_list(OoMTailFormatStringValues),
            TailSpecs = []
        ;
            MaybeOoMTailFormatStringValues = error1(TailSpecs),
            TailFormatStringValues = []
        )
    ),
    ( if
        HeadTerm = term.functor(HeadFunctor, HeadArgTerms, _Context),
        HeadFunctor = term.atom("format_string_values")
    then
        parse_format_string_values_args(yes(ListPos), HeadTerm, HeadArgTerms,
            MaybeHeadFormatStringValues),
        ( if
            MaybeHeadFormatStringValues = ok1(HeadFormatStringValues),
            TailSpecs = []
        then
            OoMFormatStringValues =
                one_or_more(HeadFormatStringValues, TailFormatStringValues),
            MaybeOoMFormatStringValues = ok1(OoMFormatStringValues)
        else
            Specs = get_any_errors1(MaybeHeadFormatStringValues) ++ TailSpecs,
            MaybeOoMFormatStringValues = error1(Specs)
        )
    else
        ErrorTermStr = describe_error_term(VarSet, HeadTerm),
        HeadPieces = format_string_values_context(yes(ListPos)) ++
            [words("expected a term of the form"),
            quote("format_string_values(N, M)"),
            words("where N and M are strictly positive integers,"),
            words("got"), quote(ErrorTermStr), suffix("."), nl],
        HeadSpec = spec($pred, severity_error, phase_t2pt,
            get_term_context(HeadTerm), HeadPieces),
        Specs = [HeadSpec | TailSpecs],
        MaybeOoMFormatStringValues = error1(Specs)
    ).

:- pred parse_format_string_values_args(maybe(int)::in, term::in,
    list(term)::in, maybe1(format_string_values)::out) is det.

parse_format_string_values_args(MaybeListPos, ErrorTerm, ArgTerms,
        MaybeFormatStringValues) :-
    (
        ArgTerms = [TermFS, TermVL],
        parse_arg_num(MaybeListPos, fs, TermFS, MaybeArgNumFS),
        parse_arg_num(MaybeListPos, vl, TermVL, MaybeArgNumVL),
        ( if
            MaybeArgNumFS = ok1(ArgNumFS),
            MaybeArgNumVL = ok1(ArgNumVL)
        then
            FormatStringValues = format_string_values(ArgNumFS, ArgNumVL,
                ArgNumFS, ArgNumVL),
            MaybeFormatStringValues = ok1(FormatStringValues)
        else
            Specs =
                get_any_errors1(MaybeArgNumFS) ++
                get_any_errors1(MaybeArgNumVL),
            MaybeFormatStringValues = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = format_string_values_context(MaybeListPos) ++
            [words("format_string_values must have two arguments."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeFormatStringValues = error1([Spec])
    ).

:- pred parse_arg_num(maybe(int)::in, fs_vl::in, term::in,
    maybe1(int)::out) is det.

parse_arg_num(MaybeListPos, FS_VL, Term, MaybeArgNum) :-
    % The wording of the error messages is a bit strained,
    % because we are talking at an *argument* of the format_string_values
    % function symbol that is itself an *argument number*.
    ( if term_int.decimal_term_to_int(Term, Int) then
        % We could check that Int > 0 here, but we won't know the upper bound
        % we want to check against until later. It is simpler to have code
        % in check_pragma_format_call_preds.m to check the argument number
        % against both the lower and upper bounds.
        MaybeArgNum = ok1(Int)
    else
        Pieces = arg_num_context(MaybeListPos, FS_VL) ++
            [words("the argument number must be an integer."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        MaybeArgNum = error1([Spec])
    ).

:- func format_string_values_context(maybe(int)) = list(format_piece).

format_string_values_context(MaybeListPos) = Pieces :-
    Pieces0 = [words("Error: in the second argument of a"),
        pragma_decl("format_call"), words("declaration:")],
    (
        MaybeListPos = no,
        Pieces = Pieces0
    ;
        MaybeListPos = yes(ListPos),
        Pieces = [words("in the"), nth_fixed(ListPos),
            words("element of the list:") | Pieces0]
    ).

:- type fs_vl
    --->    fs  % The format string argument of format_string_values.
    ;       vl. % The value list argument of format_string_values.

:- func arg_num_context(maybe(int), fs_vl) = list(format_piece).

arg_num_context(MaybeListPos, FS_VL) = Pieces :-
    Pieces0 = format_string_values_context(MaybeListPos),
    (
        FS_VL = fs,
        FS_VL_Str = "first"
    ;
        FS_VL = vl,
        FS_VL_Str = "second"
    ),
    Pieces = [words("in the"), words(FS_VL_Str), words("argument"),
        words("of format_string_values:") | Pieces0].

%---------------------------------------------------------------------------%
%
% Parse require_tail_recursion pragmas.
%

:- pred parse_pragma_require_tail_recursion(module_name::in, string::in,
    list(term)::in, term::in, varset::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_require_tail_recursion(ModuleName, PragmaName, PragmaTerms,
        _ErrorTerm, VarSet, Context, SeqNum, MaybeIOM) :-
    (
        (
            PragmaTerms = [PredOrProcSpecTerm],
            MaybeOptionsTerm = no
        ;
            PragmaTerms = [PredOrProcSpecTerm, OptionsTermPrime],
            MaybeOptionsTerm = yes(OptionsTermPrime)
        ),
        % Parse the procedure name.
        ContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl(PragmaName), words("declaration:"), nl]),
        parse_pred_pfu_name_arity_maybe_modes(ModuleName, ContextPieces,
            VarSet, PredOrProcSpecTerm, MaybePredOrProcSpec),
        % Parse the options.
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
                Spec = spec($pred, severity_error, phase_t2pt,
                    OptionsContext, Pieces),
                MaybeOptions = error1([Spec])
            )
        ;
            MaybeOptionsTerm = no,
            MaybeOptions = ok1(enable_tailrec_warnings(we_warning,
                both_self_and_mutual_recursion_must_be_tail, Context))
        ),
        ( if
            MaybePredOrProcSpec = ok1(PredOrProcSpec),
            MaybeOptions = ok1(Options)
        then
            TailRec = impl_pragma_req_tail_rec_info(PredOrProcSpec, Options,
                Context, SeqNum),
            Item = item_impl_pragma(impl_pragma_req_tail_rec(TailRec)),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = get_any_errors1(MaybePredOrProcSpec) ++
                get_any_errors1(MaybeOptions),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl(PragmaName),
            words("declaration must have one or two arguments."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
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
            RTR = enable_tailrec_warnings(WarnOrError, Type, Context),
            MaybeRTR = ok1(RTR)
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
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

:- func pragma_require_tailrec_unknown_term_error(term, prog_context) =
    error_spec.

pragma_require_tailrec_unknown_term_error(Term, Context) = Spec :-
    varset.init(VarSet),
    Pieces = [words("Error: unrecognised "),
        pragma_decl("require_tail_recursion"), words("attribute: "),
        quote(describe_error_term(VarSet, Term)), suffix("."), nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

%---------------------------------------------------------------------------%
%
% Parse oisu (order-independent state update) pragmas.
%

:- pred parse_oisu_pragma(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
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
        ( if parse_sym_name_and_arity(TypeCtorTerm, SymName0, Arity) then
            implicitly_qualify_sym_name(ModuleName, TypeCtorTerm,
                SymName0, MaybeSymName),
            (
                MaybeSymName = ok1(SymName),
                MaybeTypeCtor = ok1(type_ctor(SymName, Arity))
            ;
                MaybeSymName = error1(SymNameSpecs),
                MaybeTypeCtor = error1(SymNameSpecs)
            )
        else
            TypeCtorTermStr = describe_error_term(VarSet, TypeCtorTerm),
            Pieces = [words("In the first argument of"),
                pragma_decl("oisu"), words("declaration:"), nl,
                words("error: expected predicate name/arity, got"),
                quote(TypeCtorTermStr), suffix("."), nl],
            TypeCtorSpec = spec($pred, severity_error, phase_t2pt,
                get_term_context(ErrorTerm), Pieces),
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
            OISU = decl_pragma_oisu_info(TypeCtor, CreatorsNamesArities,
                MutatorsNamesArities, DestructorsNamesArities,
                Context, SeqNum),
            Item = item_decl_pragma(decl_pragma_oisu(OISU)),
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
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

:- pred parse_oisu_preds_term(module_name::in, varset::in, string::in,
    string::in, term::in, maybe1(list(pred_pf_name_arity))::out) is det.

parse_oisu_preds_term(ModuleName, VarSet, ArgNum, ExpectedFunctor, Term,
        MaybePredSpecs) :-
    ( if
        Term = term.functor(term.atom(Functor), ArgTerms, _),
        Functor = ExpectedFunctor,
        ArgTerms = [ArgTerm]
    then
        parse_list_elements("list of predicate or function names/arities",
            parse_pred_pf_name_arity(ModuleName, "oisu"), VarSet,
            ArgTerm, MaybePredSpecs)
    else
        Pieces = [words("Error:"), words(ArgNum), words("argument of"),
            pragma_decl("oisu"), words("declaration"),
            words("should have the form"),
            quote(ExpectedFunctor ++
                "([pred(name1/arity1), ..., pred(namen/arityn)])"),
            suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        MaybePredSpecs = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Parse type_spec_constrained_preds pragmas.
%

:- pred parse_pragma_type_spec_constr(module_name::in, varset::in, term::in,
    list(term)::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_type_spec_constr(ModuleName, VarSet0, ErrorTerm, PragmaTerms,
        SeqNum, MaybeIOM) :-
    ( if
        PragmaTerms = [ConstraintsTerm, ApplyToSupersTerm, TypeSubstsTerm]
    then
        acc_var_names_in_term(VarSet0, ConstraintsTerm,
            set.init, NamedVarNames1),
        acc_var_names_in_term(VarSet0, TypeSubstsTerm,
            NamedVarNames1, NamedVarNames),
        parse_var_or_ground_constraint_list(NamedVarNames, ConstraintsTerm,
            Constraints, ConstraintSpecs,
            counter.init(1), Counter1, VarSet0, VarSet1),
        parse_apply_to_supers(ApplyToSupersTerm, MaybeApplyToSupers),
        parse_type_subst_list(NamedVarNames, TypeSubstsTerm,
            TypeSubsts, TypeSubstsSpecs, Counter1, _Counter, VarSet1, VarSet),
        varset.coerce(VarSet0, TVarSet0),
        list.foldl(var_or_ground_constraint_acc_tvars, Constraints,
            set.init, ConstraintTVars),
        check_type_substs(TVarSet0, ConstraintTVars, TypeSubstsTerm, 1,
            TypeSubsts, [], TypeSubstTVarSpecs),
        ( if
            ConstraintSpecs = [],
            MaybeApplyToSupers = ok1(ApplyToSupers),
            TypeSubstsSpecs = [],
            TypeSubstTVarSpecs = []
        then
            det_list_to_one_or_more(Constraints, OoMConstraints),
            det_list_to_one_or_more(TypeSubsts, OoMTypeSubsts),
            varset.coerce(VarSet, TVarSet),
            TypeSpecConstr = decl_pragma_type_spec_constr_info(ModuleName,
                OoMConstraints, ApplyToSupers, OoMTypeSubsts, TVarSet,
                set.init, get_term_context(ErrorTerm), SeqNum),
            Pragma = decl_pragma_type_spec_constr(TypeSpecConstr),
            Item = item_decl_pragma(Pragma),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = ConstraintSpecs ++ get_any_errors1(MaybeApplyToSupers) ++
                TypeSubstsSpecs ++ TypeSubstTVarSpecs,
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"),
            pragma_decl("type_spec_constrained_preds"),
            words("declaration must have three arguments."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------%

:- pred parse_var_or_ground_constraint_list(set(string)::in, term::in,
    list(var_or_ground_constraint)::out, list(error_spec)::out,
    counter::in, counter::out, varset::in, varset::out) is det.

parse_var_or_ground_constraint_list(NamedVarNames, Term, Constraints, Specs,
        !Counter, !VarSet) :-
    ( if list_term_to_term_list(Term, ConstraintTerms) then
        (
            ConstraintTerms = [],
            Constraints = [],
            Pieces = [words("In the first argument of a"),
                pragma_decl("type_spec_constrained_preds"),
                words("declaration:"),
                words("error: the list of type class constraints"),
                words("must not be empty."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(Term), Pieces),
            Specs = [Spec]
        ;
            ConstraintTerms = [HeadConstraintTerm | TailConstraintTerms],
            parse_var_or_ground_constraint_acc(NamedVarNames,
                HeadConstraintTerm, cord.init, ConstraintCord1,
                [], Specs1, !Counter, !VarSet),
            list.foldl4(parse_var_or_ground_constraint_acc(NamedVarNames),
                TailConstraintTerms, ConstraintCord1, ConstraintCord,
                Specs1, Specs, !Counter, !VarSet),
            Constraints = cord.list(ConstraintCord)
        )
    else
        Constraints = [],
        Pieces = [words("In the first argument of a"),
            pragma_decl("type_spec_constrained_preds"),
            words("declaration:"),
            words("error: expected a list of type class constraints, got"),
            quote(describe_error_term(!.VarSet, Term)), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        Specs = [Spec]
    ).

:- pred parse_var_or_ground_constraint_acc(set(string)::in, term::in,
    cord(var_or_ground_constraint)::in, cord(var_or_ground_constraint)::out,
    list(error_spec)::in, list(error_spec)::out,
    counter::in, counter::out, varset::in, varset::out) is det.

parse_var_or_ground_constraint_acc(NamedVarNames, Term,
        !ConstraintCord, !Specs, !Counter, !VarSet) :-
    ( if try_parse_sym_name_and_args(Term, ClassSymName, ArgTerms) then
        (
            ArgTerms = [],
            Pieces = [words("In the first argument of a"),
                pragma_decl("type_spec_constrained_preds"),
                words("declaration:"),
                words("error: expected a typeclass constraint consisting of"),
                words("a class_name applied to one or more argument types,"),
                words("got the class name"),
                quote(sym_name_to_string(ClassSymName)),
                words("without any argument types."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(Term), Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            ArgTerms = [_ | _],
            list.foldl2(name_unnamed_vars_in_term(NamedVarNames),
                ArgTerms, !Counter, !VarSet),
            ContextPieces = cord.from_list(
                [words("In the first argument of a"),
                pragma_decl("type_spec_constrained_preds"),
                words("declaration:")]),
            AllowHOInstInfo =
                no_allow_ho_inst_info(wnhii_pragma_type_spec_constr),
            list.length(ArgTerms, NumArgTerms),
            ClassId = class_id(ClassSymName, NumArgTerms),
            parse_var_or_ground_types(AllowHOInstInfo, !.VarSet, ContextPieces,
                ClassId, ArgTerms, MaybeArgs),
            (
                MaybeArgs = ok1(Args),
                Context = get_term_context(Term),
                Constraint =
                    var_or_ground_constraint(ClassSymName, Args, Context),
                cord.snoc(Constraint, !ConstraintCord)
            ;
                MaybeArgs = error1(ArgSpecs),
                !:Specs = ArgSpecs ++ !.Specs
            )
        )
    else
        Pieces = [words("Error in the first argument of a"),
            pragma_decl("type_spec_constrained_preds"),
            words("declaration:"),
            words("expected a typeclass constraint of the form"),
            quote("class_name(argtype1, argtype2, ...)"),
            words("got"), quote(describe_error_term(!.VarSet, Term)),
            suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred parse_var_or_ground_types(allow_ho_inst_info::in, varset::in,
    cord(format_piece)::in, class_id::in, list(term)::in,
    maybe1(list(var_or_ground_type))::out) is det.

parse_var_or_ground_types(_, _, _, _, [], ok1([])).
parse_var_or_ground_types(AllowHOInstInfo, VarSet, ContextPieces, ClassId,
        [HeadTerm | TailTerms], Result) :-
    parse_var_or_ground_types(AllowHOInstInfo, VarSet, ContextPieces, ClassId,
        TailTerms, TailResult),
    parse_type(AllowHOInstInfo, VarSet, ContextPieces, HeadTerm, HeadResult0),
    (
        HeadResult0 = ok1(HeadType),
        ( if HeadType = type_variable(HeadVar, _) then
            varset.coerce(VarSet, TVarSet),
            varset.lookup_name(TVarSet, HeadVar, HeadVarName),
            HeadArg0 = type_var_name(HeadVar, HeadVarName),
            HeadResult = ok1(HeadArg0)
        else if type_is_ground(HeadType, HeadGroundType) then
            HeadArg0 = ground_type(HeadGroundType),
            HeadResult = ok1(HeadArg0)
        else
            Pieces = [words("Error in the first argument of a"),
                pragma_decl("type_spec_constrained_preds"),
                words("declaration:"),
                words("in the constraint using type class"),
                unqual_class_id(ClassId), suffix(","),
                quote("expect ground types as arguments,"),
                words("got"), quote(describe_error_term(VarSet, HeadTerm)),
                suffix("."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(HeadTerm), Pieces),
            HeadResult = error1([Spec])
        )
    ;
        HeadResult0 = error1(HeadSpecs),
        HeadResult = error1(HeadSpecs)
    ),
    ( if
        HeadResult = ok1(HeadArg),
        TailResult = ok1(TailArgs)
    then
        Result = ok1([HeadArg | TailArgs])
    else
        Specs = get_any_errors1(HeadResult) ++ get_any_errors1(TailResult),
        Result = error1(Specs)
    ).

:- pred parse_type_subst_list(set(string)::in, term::in,
    list(type_subst)::out, list(error_spec)::out,
    counter::in, counter::out, varset::in, varset::out) is det.

parse_type_subst_list(NamedVarNames, Term, TypeSubsts, Specs,
        !Counter, !VarSet) :-
    ( if list_term_to_term_list(Term, TypeSubstTerms) then
        (
            TypeSubstTerms = [],
            TypeSubsts = [],
            Pieces = [words("In the third argument of a"),
                pragma_decl("type_spec_constrained_preds"),
                words("declaration:"),
                words("error: the list of type substitutions"),
                words("must not be empty."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(Term), Pieces),
            Specs = [Spec]
        ;
            TypeSubstTerms = [HeadTypeSubstTerm | TailTypeSubstTerms],
            PrefixPieces = [words("In the third argument of a"),
                pragma_decl("type_spec_constrained_preds"),
                words("declaration:"), nl],
            WNHII = wnhii_pragma_type_spec_constr,
            parse_type_subst_acc(WNHII, PrefixPieces, NamedVarNames,
                HeadTypeSubstTerm, cord.init, TypeSubstCord1,
                [], Specs1, !Counter, !VarSet),
            list.foldl4(
                parse_type_subst_acc(WNHII, PrefixPieces, NamedVarNames),
                TailTypeSubstTerms, TypeSubstCord1, TypeSubstCord,
                Specs1, Specs, !Counter, !VarSet),
            TypeSubsts = cord.list(TypeSubstCord)
        )
    else
        TypeSubsts = [],
        Pieces = [words("In the third argument of a"),
            pragma_decl("type_spec_constrained_preds"),
            words("declaration:"),
            words("error: expected a list of type substitutions, got"),
            quote(describe_error_term(!.VarSet, Term)), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        Specs = [Spec]
    ).

:- pred parse_type_subst_acc(why_no_ho_inst_info::in, list(format_piece)::in,
    set(string)::in, term::in, cord(type_subst)::in, cord(type_subst)::out,
    list(error_spec)::in, list(error_spec)::out,
    counter::in, counter::out, varset::in, varset::out) is det.

parse_type_subst_acc(WNHII, PrefixPieces, NamedVarNames, Term,
        !TypeSubstCord, !Specs, !Counter, !VarSet) :-
    parse_type_subst(WNHII, PrefixPieces, NamedVarNames, Term, MaybeTypeSubst,
        !Counter, !VarSet),
    (
        MaybeTypeSubst = ok1(TypeSubst),
        cord.snoc(TypeSubst, !TypeSubstCord)
    ;
        MaybeTypeSubst = error1(TypeSubstSpecs),
        !:Specs = TypeSubstSpecs ++ !.Specs
    ).

:- pred parse_type_subst(why_no_ho_inst_info::in, list(format_piece)::in,
    set(string)::in, term::in, maybe1(type_subst)::out,
    counter::in, counter::out, varset::in, varset::out) is det.

parse_type_subst(WNHII, PrefixPieces, NamedVarNames, Term, MaybeTypeSubst,
        !Counter, !VarSet) :-
    ( if Term = term.functor(atom("subst"), ArgTerms, _) then
        (
            ( ArgTerms = []
            ; ArgTerms = [_, _ | _]
            ),
            Pieces = PrefixPieces ++
                [words("error:"), quote("subst"),
                words("must have exactly one argument,"),
                words("which should have the form"), nl_indent_delta(1),
                quote("[V1 = <type1>, ...]"), suffix("."),
                nl_indent_delta(-1)],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(Term), Pieces),
            MaybeTypeSubst = error1([Spec])
        ;
            ArgTerms = [ArgTerm],
            ( if list_term_to_term_list(ArgTerm, TypeSubstTerms) then
                (
                    TypeSubstTerms = [],
                    Pieces = PrefixPieces ++
                        [words("error: the list of type variable"),
                        words("substitutions must not be empty."), nl],
                    Spec = spec($pred, severity_error, phase_t2pt,
                        get_term_context(Term), Pieces),
                    MaybeTypeSubst = error1([Spec])
                ;
                    TypeSubstTerms = [HeadTypeSubstTerm | TailTypeSubstTerms],
                    VarSet0 = !.VarSet,
                    name_unnamed_vars_in_term(NamedVarNames,
                        HeadTypeSubstTerm, !Counter, !VarSet),
                    list.foldl2(name_unnamed_vars_in_term(NamedVarNames),
                        TailTypeSubstTerms, !Counter, !VarSet),
                    parse_tvar_subst_acc(WNHII, PrefixPieces, VarSet0,
                        HeadTypeSubstTerm, cord.init, TVarSubstCord1,
                        [], TVarSpecs1),
                    list.foldl2(
                        parse_tvar_subst_acc(WNHII, PrefixPieces, VarSet0),
                        TailTypeSubstTerms, TVarSubstCord1, TVarSubstCord,
                        TVarSpecs1, TVarSpecs),
                    (
                        TVarSpecs = [],
                        TVarSubsts = cord.list(TVarSubstCord),
                        det_list_to_one_or_more(TVarSubsts, TypeSubst),
                        MaybeTypeSubst = ok1(TypeSubst)
                    ;
                        TVarSpecs = [_ | _],
                        MaybeTypeSubst = error1(TVarSpecs)
                    )
                )
            else
                ErrorTermStr = describe_error_term(!.VarSet, Term),
                Pieces = PrefixPieces ++
                    [words("error: expected a list of the form,"),
                    nl_indent_delta(1),
                    quote("[V1 = <type1>, ...]"), suffix(","),
                    nl_indent_delta(-1),
                    words("got"), quote(ErrorTermStr), suffix("."), nl],
                Spec = spec($pred, severity_error, phase_t2pt,
                    get_term_context(Term), Pieces),
                MaybeTypeSubst = error1([Spec])
            )
        )
    else
        ErrorTermStr = describe_error_term(!.VarSet, Term),
        Pieces = PrefixPieces ++
            [words("error: expected a term of the form"), nl_indent_delta(1),
            quote("subst([V1 = <type1>, ...])"), suffix(","),
            nl_indent_delta(-1),
            words("got"), quote(ErrorTermStr), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        MaybeTypeSubst = error1([Spec])
    ).

%---------------------%

:- pred parse_apply_to_supers(term::in, maybe1(maybe_apply_to_supers)::out)
    is det.

parse_apply_to_supers(Term, MaybeApplyToSupers) :-
    ( if
        Term = term.functor(Functor, Args, _),
        Functor = term.atom(AtomStr),
        (
            AtomStr = "do_not_apply_to_superclasses",
            ApplyToSupers0 = do_not_apply_to_supers
        ;
            AtomStr = "apply_to_superclasses",
            ApplyToSupers0 = apply_to_supers
        )
    then
        (
            Args = [],
            MaybeApplyToSupers = ok1(ApplyToSupers0)
        ;
            Args = [_ | _],
            Pieces = [words("Error in the second argument of"),
                pragma_decl("type_spec_constrained_preds"),
                words("declaration:"), quote(AtomStr),
                words("may not have any arguments."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(Term), Pieces),
            MaybeApplyToSupers = error1([Spec])
        )
    else
        Pieces = [words("Error: the second argument of a"),
            pragma_decl("type_spec_constrained_preds"),
            words("declaration must be either"),
            quote("do_not_apply_to_superclasses"), words("or"),
            quote("apply_to_superclasses"), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        MaybeApplyToSupers = error1([Spec])
    ).

%---------------------%

:- pred var_or_ground_constraint_acc_tvars(var_or_ground_constraint::in,
    set(tvar)::in, set(tvar)::out) is det.

var_or_ground_constraint_acc_tvars(Constraint, !TVars) :-
    Constraint = var_or_ground_constraint(_ClassName, VoGTypes, _Context),
    list.foldl(var_or_ground_type_acc_tvars, VoGTypes, !TVars).

:- pred var_or_ground_type_acc_tvars(var_or_ground_type::in,
    set(tvar)::in, set(tvar)::out) is det.

var_or_ground_type_acc_tvars(VoGType, !TVars) :-
    (
        VoGType = type_var_name(TVar, _TVarName),
        set.insert(TVar, !TVars)
    ;
        VoGType = ground_type(_GroundType)
    ).

:- pred check_type_substs(tvarset::in, set(tvar)::in, term::in, int::in,
    list(type_subst)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_type_substs(_, _, _, _, [], !TypeSubstTVarSpecs).
check_type_substs(TVarSet, ConstraintTVars, ErrorTerm,
        SubstNum, [TypeSubst | TypeSubsts], !TypeSubstTVarSpecs) :-
    check_type_subst(TVarSet, ConstraintTVars, ErrorTerm,
        SubstNum, TypeSubst, !TypeSubstTVarSpecs),
    check_type_substs(TVarSet, ConstraintTVars, ErrorTerm,
        SubstNum + 1, TypeSubsts, !TypeSubstTVarSpecs).

:- pred check_type_subst(tvarset::in, set(tvar)::in, term::in, int::in,
    type_subst::in, list(error_spec)::in, list(error_spec)::out) is det.

check_type_subst(TVarSet, ConstraintTVars, ErrorTerm, SubstNum, TypeSubst,
        !Specs) :-
    TypeSubst = one_or_more(HeadTVarSubst, TailTVarSubsts),
    check_tvar_subst(TVarSet, ConstraintTVars, HeadTVarSubst,
        set.init, BadLHSTVars1, set.init, BadRHSTVars1),
    list.foldl2(check_tvar_subst(TVarSet, ConstraintTVars), TailTVarSubsts,
        BadLHSTVars1, BadLHSTVars, BadRHSTVars1, BadRHSTVars),
    BadLHSTVarList = set.to_sorted_list(BadLHSTVars),
    BadRHSTVarList = set.to_sorted_list(BadRHSTVars),
    % If a type_spec_constrained_preds pragma contains N substitutions
    % in its third argument, then the process of checking those substitutions
    % can generate up to 2N error_specs: one for each lhs or rhs in those
    % N substitutions. We don't have a context for any part of the third
    % argument, just the context of the third argument as a whole (our caller
    % passes us the term containing that argument as ErrorTerm). We want to
    % generate any error messages for the parts of that third argument
    % in an order which matches the order of the complained-about entities
    % in that argument, which we achieve through the use of the invis order
    % format pieces below.
    (
        BadLHSTVarList = []
    ;
        BadLHSTVarList = [_HeadBadLHSTVar | TailBadLHSTVars],
        (
            TailBadLHSTVars = [],
            TheTypeVar = "the left-hand-side type variable",
            ItDoesNot = "it does not."
        ;
            TailBadLHSTVars = [_ | _],
            TheTypeVar = "the left-hand-side type variables",
            ItDoesNot = "they do not."
        ),
        BadLHSTVarStrs =
            list.map(mercury_var_to_string_vs(TVarSet, print_name_only),
            BadLHSTVarList),
        BadLHSTVarsPieces = list_to_pieces(BadLHSTVarStrs),
        LHSPieces = [invis_order_default_start(SubstNum, "lhs"),
            words("Error: in the third argument of a"),
            pragma_decl("type_spec_constrained_preds"),
            words("declaration:"), nl,
            words("in the"), nth_fixed(SubstNum), words("substitution:"), nl,
            words(TheTypeVar)] ++ BadLHSTVarsPieces ++ [words("must occur"),
            words("in the constraints listed in the first argument,"),
            words("but"), words(ItDoesNot), nl],
        LHSSpec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), LHSPieces),
        !:Specs = [LHSSpec | !.Specs]
    ),
    (
        BadRHSTVarList = []
    ;
        BadRHSTVarList = [_HeadBadRHSTVar | TailBadRHSTVars],
        (
            TailBadRHSTVars = [],
            IsNot = "is not."
        ;
            TailBadRHSTVars = [_ | _],
            IsNot = "are not."
        ),
        BadRHSTVarStrs =
            list.map(mercury_var_to_string_vs(TVarSet, print_name_only),
            BadRHSTVarList),
        BadRHSTVarsPieces = list_to_pieces(BadRHSTVarStrs),
        RHSPieces = [invis_order_default_start(SubstNum, "rhs"),
            words("Error: in the third argument of a"),
            pragma_decl("type_spec_constrained_preds"),
            words("declaration:"), nl,
            words("in the"), nth_fixed(SubstNum), words("substitution:"), nl,
            words("any type variables that occur on the right hand side"),
            words("of a substitution must be anonymous, but")] ++
            BadRHSTVarsPieces ++ [words(IsNot), nl],
        RHSSpec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), RHSPieces),
        !:Specs = [RHSSpec | !.Specs]
    ).

:- pred check_tvar_subst(tvarset::in, set(tvar)::in, tvar_subst::in,
    set(tvar)::in, set(tvar)::out, set(tvar)::in, set(tvar)::out) is det.

check_tvar_subst(TVarSet, ConstraintTVars, TVarSubst,
        !BadLHSTVars, !BadRHSTVars) :-
    TVarSubst = tvar_subst(LHSTVar, RHSType),
    ( if set.contains(ConstraintTVars, LHSTVar) then
        true
    else
        set.insert(LHSTVar, !BadLHSTVars)
    ),
    set_of_type_vars_in_type(RHSType, RHSTVars),
    set.foldl(check_tvar_subst_rhs_tvar(TVarSet), RHSTVars, !BadRHSTVars).

:- pred check_tvar_subst_rhs_tvar(tvarset::in, tvar::in,
    set(tvar)::in, set(tvar)::out) is det.

check_tvar_subst_rhs_tvar(TVarSet, TVar, !BadRHSTVars) :-
    ( if varset.search_name(TVarSet, TVar, _VarName) then
        set.insert(TVar, !BadRHSTVars)
    else
        true
    ).

%---------------------------------------------------------------------------%
%
% Parse type_spec pragmas.
%

:- pred parse_pragma_type_spec(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_type_spec(ModuleName, VarSet0, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    ( if PragmaTerms = [PredAndModesTerm, TypeSubstTerm] then
        ArityOrModesContextPieces = cord.from_list(
            [words("In the first argument of"), pragma_decl("type_spec"),
            words("declaration:"), nl]),
        parse_pred_pfu_name_arity_maybe_modes(ModuleName,
            ArityOrModesContextPieces, VarSet0, PredAndModesTerm,
            MaybePredOrProcSpec),
        (
            MaybePredOrProcSpec = ok1(PredOrProcSpec),
            PredOrProcSpec = pred_or_proc_pfumm_name(PFUMM, PredName),

            WNHII = wnhii_pragma_type_spec,
            TypeContextPieces =
                [words("In the second argument of"), pragma_decl("type_spec"),
                words("declaration:"), nl],
            acc_var_names_in_term(VarSet0, TypeSubstTerm,
                set.init, NamedVarNames),
            ( if TypeSubstTerm = term.functor(atom("subst"), _, _) then
                parse_type_subst(WNHII, TypeContextPieces, NamedVarNames,
                    TypeSubstTerm, MaybeTypeSubst,
                    counter.init(1), _, VarSet0, VarSet),
                (
                    MaybeTypeSubst = ok1(OoMTVarSubsts),
                    % The varset is actually a tvarset.
                    varset.coerce(VarSet, TVarSet),
                    TypeSpec = decl_pragma_type_spec_info(PFUMM, PredName,
                        ModuleName, OoMTVarSubsts, TVarSet,
                        set.init, Context, SeqNum),
                    Item = item_decl_pragma(decl_pragma_type_spec(TypeSpec)),
                    MaybeIOM = ok1(iom_item(Item))
                ;
                    MaybeTypeSubst = error1(TypeSubstSpecs),
                    MaybeIOM = error1(TypeSubstSpecs)
                )
            else
                % Give any anonymous variables in TypeSubstTerm names that
                % do not conflict with the names of any named variables,
                % nor, due to the use of sequence numbers, with each other.
                % (In the then branch, this is done by parse_type_subst_acc.)
                name_unnamed_vars_in_term(NamedVarNames, TypeSubstTerm,
                    counter.init(1), _, VarSet0, VarSet),
                conjunction_to_one_or_more(TypeSubstTerm, TypeSubstTerms),
                TypeSubstTerms =
                    one_or_more(HeadTypeSubstTerm, TailTypeSubstTerms),
                parse_tvar_substs(WNHII, TypeContextPieces, VarSet0,
                    HeadTypeSubstTerm, TailTypeSubstTerms,
                    TVarSubsts, [], TypeSpecs),
                (
                    TypeSpecs = [],
                    % The varset is actually a tvarset.
                    varset.coerce(VarSet, TVarSet),
                    det_list_to_one_or_more(TVarSubsts, OoMTVarSubsts),
                    TypeSpec = decl_pragma_type_spec_info(PFUMM, PredName,
                        ModuleName, OoMTVarSubsts, TVarSet,
                        set.init, Context, SeqNum),
                    Item = item_decl_pragma(decl_pragma_type_spec(TypeSpec)),
                    MaybeIOM = ok1(iom_item(Item))
                ;
                    TypeSpecs = [_ | _],
                    MaybeIOM = error1(TypeSpecs)
                )
            )
        ;
            MaybePredOrProcSpec = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), pragma_decl("type_spec"),
            words("declaration must have two arguments."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates needed for both type_spec_constrained_preds
% and type_spec pragmas.
%

:- pred parse_tvar_substs(why_no_ho_inst_info::in, list(format_piece)::in,
    varset::in, term::in, list(term)::in, list(tvar_subst)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_tvar_substs(WNHII, ContextPieces, VarSet, HeadTerm, TailTerms,
        TVarSubsts, !Specs) :-
    TVarSubstCord0 = cord.init,
    parse_tvar_subst_acc(WNHII, ContextPieces, VarSet,
        HeadTerm, TVarSubstCord0, TVarSubstCord1, !Specs),
    list.foldl2(parse_tvar_subst_acc(WNHII, ContextPieces, VarSet),
        TailTerms, TVarSubstCord1, TVarSubstCord, !Specs),
    TVarSubsts = cord.list(TVarSubstCord).

:- pred parse_tvar_subst_acc(why_no_ho_inst_info::in, list(format_piece)::in,
    varset::in, term::in,
    cord(tvar_subst)::in, cord(tvar_subst)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_tvar_subst_acc(WNHII, ContextPieces, VarSet, Term,
        !TVarSubstCord, !Specs) :-
    ( if
        Term = term.functor(term.atom(Atom), [TypeVarTerm, TypeTerm], _),
        ( Atom = "=", AtomStr = "equals sign"
        ; Atom = "=>", AtomStr = "arrow"
        )
    then
        ( if TypeVarTerm = term.variable(TypeVar0, _) then
            RHSPieces = [words("on the right hand side of the"),
                words(AtomStr), suffix(":"), nl],
            TypeContextPieces = cord.from_list(ContextPieces) ++
                cord.from_list(RHSPieces),
            parse_type(no_allow_ho_inst_info(WNHII), VarSet, TypeContextPieces,
                TypeTerm, MaybeType),
            (
                MaybeType = ok1(Type),
                % XXX Having Type be a type variable would not make sense.
                % The reference manual does not prohibit it, but it does not
                % explicitly allow it either.
                %
                % The two usual shapes of Type are
                %
                % - an arity-zero type constructor, and
                % - an arity-N type constructor for N > 0 being applied
                %   to N anonymous variables.
                %
                % A type containing more than one nested type constructor
                % also makes sense. Types containing *named* variables can
                % also make sense, but only if each such variable occurs
                % exactly once not just in this tvar_subst, but in the list
                % of one or more tvar_substs that a type_subst consists of.
                % This is because repeated variables restrict the applicability
                % of the type substitution in a way.
                %
                % XXX We should consider adding code here to check for
                % the cases that do not make sense, *after* documenting
                % those restrictions in the reference manual.
                term.coerce_var(TypeVar0, TypeVar),
                TVarSubst = tvar_subst(TypeVar, Type),
                cord.snoc(TVarSubst, !TVarSubstCord)
            ;
                MaybeType = error1(TypeSpecs),
                !:Specs = TypeSpecs ++ !.Specs
            )
        else
            TypeVarTermStr = describe_error_term(VarSet, TypeVarTerm),
            Pieces = ContextPieces ++
                [words("on the left hand side of the"),
                words(AtomStr), suffix(":"), nl,
                words("error: expected a variable, got"),
                quote(TypeVarTermStr), suffix("."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(TypeVarTerm), Pieces),
            !:Specs = [Spec | !.Specs]
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = ContextPieces ++
            [words("error: expected a term of the form"), nl_indent_delta(1),
            % XXX We should replace the = with => when we start recommending
            % that syntax for both type_spec and type_spec_constrained_preds
            % pragmas.
            quote("V1 = <type1>"), suffix(","), nl_indent_delta(-11),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

:- pred acc_var_names_in_term(varset::in, term::in,
    set(string)::in, set(string)::out) is det.

acc_var_names_in_term(VarSet, Term, !VarNames) :-
    (
        Term = term.variable(Var, _Context),
        ( if varset.search_name(VarSet, Var, VarName) then
            set.insert(VarName, !VarNames)
        else
            true
        )
    ;
        Term = term.functor(_Functor, ArgTerms, _Context),
        acc_var_names_in_terms(VarSet, ArgTerms, !VarNames)
    ).

:- pred acc_var_names_in_terms(varset::in, list(term)::in,
    set(string)::in, set(string)::out) is det.

acc_var_names_in_terms(_, [], !VarNames).
acc_var_names_in_terms(VarSet, [Term | Terms], !VarNames) :-
    acc_var_names_in_term(VarSet, Term, !VarNames),
    acc_var_names_in_terms(VarSet, Terms, !VarNames).

%---------------------%

:- pred name_unnamed_vars_in_term(set(string)::in, term::in,
    counter::in, counter::out, varset::in, varset::out) is det.

name_unnamed_vars_in_term(NamedVarNames, Term, !Counter, !VarSet) :-
    (
        Term = term.variable(Var, _Context),
        ( if varset.search_name(!.VarSet, Var, _VarName) then
            true
        else
            name_anonymous_variable(NamedVarNames, Var, !Counter, !VarSet)
        )
    ;
        Term = term.functor(_Functor, ArgTerms, _Context),
        name_unnamed_vars_in_terms(NamedVarNames, ArgTerms, !Counter, !VarSet)
    ).

:- pred name_unnamed_vars_in_terms(set(string)::in, list(term)::in,
    counter::in, counter::out, varset::in, varset::out) is det.

name_unnamed_vars_in_terms(_, [], !Counter, !VarSet).
name_unnamed_vars_in_terms(NamedVarNames, [Term | Terms], !Counter, !VarSet) :-
    name_unnamed_vars_in_term(NamedVarNames, Term, !Counter, !VarSet),
    name_unnamed_vars_in_terms(NamedVarNames, Terms, !Counter, !VarSet).

:- pred name_anonymous_variable(set(string)::in, var::in,
    counter::in, counter::out, varset::in, varset::out) is det.

name_anonymous_variable(NamedVarNames, AnonVar, !Counter, !VarSet) :-
    counter.allocate(SeqNum, !Counter),
    VarName = "Anon" ++ int_to_string(SeqNum),
    ( if set.contains(NamedVarNames, VarName) then
        % VarName is in use; try again with the updated counter.
        name_anonymous_variable(NamedVarNames, AnonVar, !Counter, !VarSet)
    else
        varset.name_var(AnonVar, VarName, !VarSet)
    ).

%---------------------------------------------------------------------------%
%
% Parse fact_table pragmas.
%

:- pred parse_pragma_fact_table(module_name::in, varset::in, term::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_pragma_fact_table(ModuleName, VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PredAndArityTerm, FileNameTerm],
        parse_pred_pfu_name_arity(ModuleName, "fact_table",
            VarSet, PredAndArityTerm, MaybePredSpec),
        (
            MaybePredSpec = ok1(PredSpec),
            ( if FileNameTerm = term.functor(term.string(FileName), [], _) then
                FactTable = impl_pragma_fact_table_info(PredSpec, FileName,
                    Context, SeqNum),
                Item = item_impl_pragma(impl_pragma_fact_table(FactTable)),
                MaybeIOM = ok1(iom_item(Item))
            else
                FileNameTermStr = describe_error_term(VarSet, FileNameTerm),
                Pieces = [words("In the second argument of"),
                    pragma_decl("fact_table"), words("declaration:"), nl,
                    words("error: expected a string specifying a filename,"),
                    words("got"), quote(FileNameTermStr), suffix("."), nl],
                Spec = spec($pred, severity_error, phase_t2pt,
                    get_term_context(FileNameTerm), Pieces),
                MaybeIOM = error1([Spec])
            )
        ;
            MaybePredSpec = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_]
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: a"), pragma_decl("fact_table"),
            words("declaration must have two arguments."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Parse require_feature_set pragmas.
%

:- pred parse_pragma_require_feature_set(varset::in, term::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

parse_pragma_require_feature_set(VarSet, ErrorTerm, PragmaTerms,
        Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [FeatureListTerm],
        parse_list_elements("list of features", parse_required_feature,
            VarSet, FeatureListTerm, MaybeFeatureList),
        (
            MaybeFeatureList = ok1(FeatureList),
            FloatPieces =
                color_as_subject([words("floats")]) ++
                color_as_incorrect([words("cannot be both")]) ++
                [words("single- and double-precision.")],
            TrailPieces =
                color_as_subject([words("trailing")]) ++
                [words("works only with")] ++
                color_as_incorrect([words("sequential conjunctions.")]),
            ConflictingFeatures = [
                conflict(reqf_single_prec_float, reqf_double_prec_float,
                    FloatPieces),
                conflict(reqf_parallel_conj, reqf_trailing, TrailPieces)
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
                    MaybeIOM = ok1(iom_handled_no_error)
                ;
                    FeatureList = [_ | _],
                    FeatureSet = set.list_to_set(FeatureList),
                    RFSInfo = impl_pragma_req_feature_set_info(FeatureSet,
                        Context, SeqNum),
                    Pragma = impl_pragma_req_feature_set(RFSInfo),
                    Item = item_impl_pragma(Pragma),
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
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

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
        Spec = spec($pred, severity_error, phase_t2pt,
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
:- end_module parse_tree.parse_pragma.
%---------------------------------------------------------------------------%
