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
:- import_module parse_tree.error_util.
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

:- import_module cord.
:- import_module counter.
:- import_module int.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
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

:- pred parse_pragma_type(module_name::in, varset::in, term::in,
    string::in, list(term)::in, prog_context::in, item_seq_num::in,
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
        (
            PragmaName = "terminates",
            MakePragma =
                (func(PredSpec) = decl_pragma_terminates(PredSpec))
        ;
            PragmaName = "does_not_terminate",
            MakePragma =
                (func(PredSpec) = decl_pragma_does_not_terminate(PredSpec))
        ;
            PragmaName = "check_termination",
            MakePragma =
                (func(PredSpec) = decl_pragma_check_termination(PredSpec))
        ),
        parse_name_arity_decl_pragma(ModuleName, PragmaName, MakePragma,
            VarSet, ErrorTerm, PragmaTerms, Context, SeqNum, MaybeIOM)
    ;
        (
            PragmaName = "inline",
            MakePragma =
                (func(PredSpec) = impl_pragma_inline(PredSpec))
        ;
            PragmaName = "no_inline",
            MakePragma =
                (func(PredSpec) = impl_pragma_no_inline(PredSpec))
        ;
            PragmaName = "consider_used",
            MakePragma =
                (func(PredSpec) = impl_pragma_consider_used(PredSpec))
        ;
            PragmaName = "no_determinism_warning",
            MakePragma =
                (func(PredSpec) = impl_pragma_no_detism_warning(PredSpec))
        ;
            PragmaName = "mode_check_clauses",
            MakePragma =
                (func(PredSpec) = impl_pragma_mode_check_clauses(PredSpec))
        ;
            PragmaName = "promise_pure",
            MakePragma =
                (func(PredSpec) = impl_pragma_promise_pure(PredSpec))
        ;
            PragmaName = "promise_semipure",
            MakePragma =
                (func(PredSpec) = impl_pragma_promise_semipure(PredSpec))
        ;
            PragmaName = "promise_equivalent_clauses",
            MakePragma =
                (func(PredSpec) = impl_pragma_promise_eqv_clauses(PredSpec))
        ),
        parse_name_arity_impl_pragma(ModuleName, PragmaName, MakePragma,
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
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        Context, Pieces).

%---------------------------------------------------------------------------%
%
% Parse the sole argument of a (decl or impl) pragma that should contain
% a symbol name / arity pair.
%

:- pred parse_name_arity_decl_pragma(module_name::in, string::in,
    (func(pred_pfu_name_arity) = decl_pragma)::in(func(in) = out is det),
    varset::in, term::in, list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_name_arity_decl_pragma(ModuleName, PragmaName, MakePragma,
        VarSet, ErrorTerm, PragmaTerms, Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PragmaTerm],
        parse_pred_pfu_name_arity(ModuleName, PragmaName, VarSet,
            PragmaTerm, MaybePredSpec),
        (
            MaybePredSpec = ok1(PredSpec),
            Pragma = MakePragma(PredSpec),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_decl_pragma(ItemPragma),
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
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
   ).

:- pred parse_name_arity_impl_pragma(module_name::in, string::in,
    (func(pred_pfu_name_arity) = impl_pragma)::in(func(in) = out is det),
    varset::in, term::in, list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_name_arity_impl_pragma(ModuleName, PragmaName, MakePragma,
        VarSet, ErrorTerm, PragmaTerms, Context, SeqNum, MaybeIOM) :-
    (
        PragmaTerms = [PragmaTerm],
        parse_pred_pfu_name_arity(ModuleName, PragmaName, VarSet,
            PragmaTerm, MaybePredSpec),
        (
            MaybePredSpec = ok1(PredSpec),
            Pragma = MakePragma(PredSpec),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_impl_pragma(ItemPragma),
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
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
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
                ExternalInfo =
                    pragma_info_external_proc(PFNameArity, MaybeBackend),
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
        ( if term_int.decimal_term_to_int(ArityTerm, ArityPrime) then
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
            ObsoletePragma =
                pragma_info_obsolete_pred(PredSpec, ObsoleteInFavourOf),
            Pragma = decl_pragma_obsolete_pred(ObsoletePragma),
            ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
            Item = item_decl_pragma(ItemPragma),
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
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
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
    ( if parse_sym_name_and_arity(Term, SymName, Arity) then
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
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, OptionsContext, Pieces),
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
            PragmaType = impl_pragma_require_tail_rec(
                pragma_info_require_tail_rec(PredOrProcSpec, Options)),
            PragmaInfo = item_pragma_info(PragmaType, Context, SeqNum),
            MaybeIOM = ok1(iom_item(item_impl_pragma(PragmaInfo)))
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
    string::in, term::in, maybe1(list(pred_pf_name_arity))::out) is det.

parse_oisu_preds_term(ModuleName, VarSet, ArgNum, ExpectedFunctor, Term,
        MaybePredSpecs) :-
    ( if
        Term = term.functor(term.atom(Functor), ArgTerms, _),
        Functor = ExpectedFunctor,
        ArgTerms = [ArgTerm]
    then
        parse_list_elements("a list of predicate or function names/arities",
            parse_pred_pf_name_arity(ModuleName, "oisu"), VarSet,
            ArgTerm, MaybePredSpecs)
    else
        Pieces = [words("Error:"), words(ArgNum), words("argument of"),
            pragma_decl("oisu"), words("declaration"),
            words("should have the form"),
            quote(ExpectedFunctor ++
                "([pred(name1/arity1), ..., pred(namen/arityn)])"),
            suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybePredSpecs = error1([Spec])
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
    ( if
        ( PragmaTerms = [PredAndModesTerm, TypeSubnTerm]
        ; PragmaTerms = [PredAndModesTerm, TypeSubnTerm, _]
        )
    then
        ArityOrModesContextPieces = cord.from_list(
            [words("In the first argument"), pragma_decl("type_spec"),
            words("declaration:"), nl]),
        parse_pred_pfu_name_arity_maybe_modes(ModuleName,
            ArityOrModesContextPieces, VarSet0, PredAndModesTerm,
            MaybePredOrProcSpec),
        (
            MaybePredOrProcSpec = ok1(PredOrProcSpec),
            PredOrProcSpec = pred_or_proc_pfumm_name(PFUMM, PredName),

            % Give any anonymous variables in TypeSubnTerm names that
            % do not conflict with the names of any named variables,
            % nor, due to the use of sequence numbers, with each other.
            acc_var_names_in_term(VarSet0, TypeSubnTerm,
                set.init, NamedVarNames),
            name_unnamed_vars_in_term(NamedVarNames, TypeSubnTerm,
                counter.init(1), _, VarSet0, VarSet),
            conjunction_to_one_or_more(TypeSubnTerm, TypeSubnTerms),
            TypeSubnTerms = one_or_more(HeadSubnTerm, TailSubnTerms),
            ( if
                parse_type_spec_pair(HeadSubnTerm, HeadTypeSubn),
                list.map(parse_type_spec_pair, TailSubnTerms, TailTypeSubns)
            then
                % The varset is actually a tvarset.
                varset.coerce(VarSet, TVarSet),
                TypeSubns = one_or_more(HeadTypeSubn, TailTypeSubns),
                TypeSpecInfo = pragma_info_type_spec(PFUMM, PredName,
                    ModuleName, TypeSubns, TVarSet, set.init),
                Pragma = decl_pragma_type_spec(TypeSpecInfo),
                ItemPragma = item_pragma_info(Pragma, Context, SeqNum),
                Item = item_decl_pragma(ItemPragma),
                MaybeIOM = ok1(iom_item(Item))
            else
                TypeSubnTermStr = describe_error_term(VarSet0, TypeSubnTerm),
                Pieces = [words("In the second argument of"),
                    pragma_decl("type_spec"), words("declaration:"), nl,
                    words("error: expected a type substitution, got"),
                    quote(TypeSubnTermStr), suffix("."), nl],
                TypeSubnContext = get_term_context(TypeSubnTerm),
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TypeSubnContext, Pieces),
                MaybeIOM = error1([Spec])
            )
        ;
            MaybePredOrProcSpec = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        % XXX We allow three as a bootstrapping measure.
        Pieces = [words("Error: a"), pragma_decl("type_spec"),
            words("declaration must have two arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_type_spec_pair(term::in, pair(tvar, mer_type)::out) is semidet.

parse_type_spec_pair(Term, TypeSpec) :-
    Term = term.functor(term.atom("="), [TypeVarTerm, SpecTypeTerm], _),
    TypeVarTerm = term.variable(TypeVar0, _),
    term.coerce_var(TypeVar0, TypeVar),
    % XXX We should call parse_type instead.
    maybe_parse_type(no_allow_ho_inst_info(wnhii_pragma_type_spec),
        SpecTypeTerm, SpecType),
    TypeSpec = TypeVar - SpecType.

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
                FactTableInfo = pragma_info_fact_table(PredSpec, FileName),
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
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
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
        parse_list_elements("a list of features", parse_required_feature,
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
                    MaybeIOM = ok1(iom_handled_no_error)
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
:- end_module parse_tree.parse_pragma.
%---------------------------------------------------------------------------%
