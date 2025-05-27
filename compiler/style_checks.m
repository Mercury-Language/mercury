%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: style_checks.m.
% Main author: zs.
%
% This module generates warnings for some aspects of bad style.
% Specifically, it implements the following compiler options
% (and their synonyms).
%
% --warn-non-contiguous-decls
%
%   Each predicate or function has declarations that specify
%   - the types of its arguments, and
%   - the modes of its arguments.
%   Generate a warning if these declarations are not all contiguous.
%   This is Task 1 below.
%
% --warn-non-contiguous-clauses
% --warn-non-contiguous-foreign-procs
%
%   The definition of each predicate or function consists of one or more
%   clauses and/or foreign_procs.
%   Generate a warning if these clauses and/or foreign_procs
%   are not all contiguous.
%   This is Task 2 below.
%
% --warn-inconsistent-pred-order-clauses
% --warn-inconsistent-pred-order-foreign-procs
%
%   Each section of a module, the interface section and the implementation
%   section, will in general declare several predicates and/or functions.
%   Generate a warning if the predicates and/or functions declared in
%   a given section have their definitions in a order that is different
%   from the order of the declarations.
%   This is Task 3 below.

:- module check_hlds.style_checks.

:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------------------------------------------------------------%

:- type warnings_we_want.

:- type maybe_want_style_warnings
    --->    do_not_want_style_warnings
    ;       want_style_warnings(warnings_we_want).

:- pred do_we_want_style_warnings(globals::in,
    maybe_want_style_warnings::out) is det.

%---------------------------------------------------------------------------%

:- pred generate_any_style_warnings(module_info::in, warnings_we_want::in,
    list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_pred.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module edit_seq.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module set_tree234.

%---------------------------------------------------------------------------%

:- type warnings_we_want
    --->    warnings_we_want(
                maybe_warn_non_contiguous_pred_decls,
                maybe_warn_non_contiguous_pred_defns,
                maybe_warn_pred_decl_vs_defn_order
            ).

:- type maybe_warn_non_contiguous_pred_decls
    --->    do_not_warn_non_contiguous_pred_decls
    ;       warn_non_contiguous_pred_decls.

:- type maybe_warn_non_contiguous_pred_defns
    --->    do_not_warn_non_contiguous_pred_defns
    ;       warn_non_contiguous_pred_defns(clause_item_number_types).

:- type maybe_warn_pred_decl_vs_defn_order
    --->    do_not_warn_pred_decl_vs_defn_order
    ;       warn_pred_decl_vs_defn_order(clause_item_number_types).

do_we_want_style_warnings(Globals, DoWeWantStyleWarnings) :-
    % Task 1: generate warnings if the ":- pred/func" and ":- mode"
    % declarations of this predicate or function are not contiguous.
    globals.lookup_bool_option(Globals,
        warn_non_contiguous_decls, NonContigDeclsOpt),
    (
        NonContigDeclsOpt = no,
        NonContigDecls = do_not_warn_non_contiguous_pred_decls
    ;
        NonContigDeclsOpt = yes,
        NonContigDecls = warn_non_contiguous_pred_decls
    ),

    % Task 2: generate warnings about gaps in the set of clauses
    % of this predicate or function.
    globals.lookup_bool_option(Globals, warn_non_contiguous_foreign_procs,
        WarnNonContigForeignProcs),
    (
        WarnNonContigForeignProcs = yes,
        NonContigDefns =
            warn_non_contiguous_pred_defns(clauses_and_foreign_procs)
    ;
        WarnNonContigForeignProcs = no,
        globals.lookup_bool_option(Globals, warn_non_contiguous_clauses,
            WarnNonContigClauses),
        (
            WarnNonContigClauses = yes,
            NonContigDefns = warn_non_contiguous_pred_defns(only_clauses)
        ;
            WarnNonContigClauses = no,
            NonContigDefns = do_not_warn_non_contiguous_pred_defns
        )
    ),

    % Task 3: gather info that will allow our ancestor to detect
    % situations in which the order of the declarations of predicates and
    % functions in a module section is not matched by the order of the
    % definitions of the definitions of those predicates/functions.
    globals.lookup_bool_option(Globals,
        warn_inconsistent_pred_order_clauses, InconsistentPredOrderClauses),
    globals.lookup_bool_option(Globals,
        warn_inconsistent_pred_order_foreign_procs,
        InconsistentPredOrderForeignProcs),
    (
        InconsistentPredOrderForeignProcs = no,
        (
            InconsistentPredOrderClauses = no,
            PredDeclDefnOrder = do_not_warn_pred_decl_vs_defn_order
        ;
            InconsistentPredOrderClauses = yes,
            PredDeclDefnOrder = warn_pred_decl_vs_defn_order(only_clauses)
        )
    ;
        InconsistentPredOrderForeignProcs = yes,
        PredDeclDefnOrder =
            warn_pred_decl_vs_defn_order(clauses_and_foreign_procs)
    ),

    ( if
        % No task 1.
        NonContigDecls = do_not_warn_non_contiguous_pred_decls,

        % No task 2.
        NonContigDefns = do_not_warn_non_contiguous_pred_defns,

        % No task 3.
        PredDeclDefnOrder = do_not_warn_pred_decl_vs_defn_order
    then
        DoWeWantStyleWarnings = do_not_want_style_warnings
    else

        WarningsWeWant = warnings_we_want(NonContigDecls, NonContigDefns,
            PredDeclDefnOrder),
        DoWeWantStyleWarnings = want_style_warnings(WarningsWeWant)
    ).

%---------------------------------------------------------------------------%

generate_any_style_warnings(ModuleInfo, WarningsWeWant, !:Specs) :-
    WarningsWeWant = warnings_we_want(NonContigDecls, NonContigDefns,
        PredDeclDefnOrder),
    module_info_get_valid_pred_id_set(ModuleInfo, ValidPredIds),
    StyleInfo0 = style_info(NonContigDecls, NonContigDefns, PredDeclDefnOrder,
        ValidPredIds, [], [], [], []),
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.foldl(gather_style_info, PredIdTable, StyleInfo0, StyleInfo),
    StyleInfo = style_info(_, _, _, _, ExportedPreds, NonExportedPreds,
        ModeDeclItemNumberSpecs, ClauseGapSpecs),
    !:Specs = ModeDeclItemNumberSpecs ++ ClauseGapSpecs,
    (
        PredDeclDefnOrder = do_not_warn_pred_decl_vs_defn_order
    ;
        PredDeclDefnOrder = warn_pred_decl_vs_defn_order(_DefnKind),
        % We can ignore _DefnKind here because we paid attention to it
        % when computing ExportedPreds and NonExportedPreds.
        module_info_get_name_context(ModuleInfo, ModuleContext),
        generate_inconsistent_pred_order_warnings(ModuleContext,
            "exported", ExportedPreds, !Specs),
        generate_inconsistent_pred_order_warnings(ModuleContext,
            "nonexported", NonExportedPreds, !Specs)
    ).

%---------------------------------------------------------------------------%

:- type pred_decl_item_numbers
    --->    pred_decl_item_numbers(
                % The id and pred_info of a predicate together with ...
                pdin_pred_id                    :: pred_id,
                pdin_pred_info                  :: pred_info,

                % ... the item number (which must be valid) of
                % - its pred or func declaration, and
                % - its first clause.
                pdin_decl_item_number           :: int,
                pdin_first_defn_item_number     :: int
            ).

:- type style_info
    --->    style_info(
                style_non_contig_decls ::
                                        maybe_warn_non_contiguous_pred_decls,
                style_non_contig_defns ::
                                        maybe_warn_non_contiguous_pred_defns,
                style_warn_decl_vs_defn ::
                                        maybe_warn_pred_decl_vs_defn_order,
                style_valid_pred_ids        :: set_tree234(pred_id),
                style_exported_preds        :: list(pred_decl_item_numbers),
                style_nonexported_preds     :: list(pred_decl_item_numbers),
                style_decl_gap_specs        :: list(error_spec),
                style_clause_gap_specs      :: list(error_spec)
            ).

:- pred gather_style_info(pred_id::in, pred_info::in,
    style_info::in, style_info::out) is det.

gather_style_info(PredId, PredInfo, !StyleInfo) :-
    pred_info_get_cur_user_decl_info(PredInfo, MaybeDeclInfo),
    ValidPredIds = !.StyleInfo ^ style_valid_pred_ids,
    ( if
        MaybeDeclInfo = yes(DeclInfo),
        DeclInfo = cur_user_decl_info(DeclSection, _, MaybePredDeclItemNumber),
        % If there is no PredDeclItemNumber, then PredInfo was generated
        % by the compiler, and any warnings we could generate below
        % would be non-actionable by the programmer.
        MaybePredDeclItemNumber = item_seq_num(PredDeclItemNumber),

        % If this predicate or function has semantic errors, then
        % the programmer would probably consider any style warnings
        % to be just clutter.
        set_tree234.contains(ValidPredIds, PredId)
    then
        pred_info_get_clauses_info(PredInfo, ClausesInfo),
        clauses_info_get_clauses_rep(ClausesInfo, _ClausesRep,
            ClauseItemNumbers),

        % Task 1: generate warnings if the ":- pred/func" and ":- mode"
        % declarations of this predicate or function are not contiguous.
        maybe_warn_about_any_decl_gap(PredInfo, PredDeclItemNumber,
            !StyleInfo),

        % Task 2: generate warnings about gaps in the set of clauses
        % of this predicate or function.
        %
        % NOTE The info we now generate consists of warnings, but
        % this should change soon. Hence the predicate name.
        %
        % Also note that tests/invalid/types.m used to expect the warnings
        % that this call generates, its predecessor used to be called
        % *before* typecheck deleted PredId from the set of valid PredIds.
        %
        maybe_gather_clause_gap_info(PredInfo, ClauseItemNumbers, !StyleInfo),

        % Task 3: gather info that will allow our ancestor to detect
        % situations in which the order of the declarations of predicates and
        % functions in a module section is not matched by the order of the
        % definitions of the definitions of those predicates/functions.
        maybe_gather_decl_vs_defn_order_info(PredId, PredInfo,
            DeclSection, PredDeclItemNumber, ClauseItemNumbers, !StyleInfo)
    else
        true
    ).

%---------------------------------------------------------------------------%
%
% Code for task 1.
%

    % In general, a predicate or function with N modes may have
    % up to N+1 declarations: one declaring the types of its arguments,
    % and N declaring the modes of those arguments. All these declarations
    % should be next to each other. If they are not, generating a warning
    % for each gap.
    %
:- pred maybe_warn_about_any_decl_gap(pred_info::in, int::in,
    style_info::in, style_info::out) is det.

maybe_warn_about_any_decl_gap(PredInfo, PredDeclItemNumber, !StyleInfo) :-
    NonContigDecls = !.StyleInfo ^ style_non_contig_decls,
    (
        NonContigDecls = do_not_warn_non_contiguous_pred_decls
    ;
        NonContigDecls = warn_non_contiguous_pred_decls,
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.foldl3(gather_proc_decl_item_number, ProcTable, 0, _,
            [], UnsortedProcINCs, warning_makes_sense, MakesSense),
        list.sort(UnsortedProcINCs, ProcINCs),
        ( if
            MakesSense = warning_makes_sense,
            ProcINCs = [HeadProcINC | TailProcINCs]
        then
            pred_info_get_context(PredInfo, PredDeclContext),
            PredINC = inc(PredDeclItemNumber, PredDeclContext),
            report_any_inc_gaps(PredInfo, PredINC, HeadProcINC, TailProcINCs,
                0, !StyleInfo)
        else
            true
        )
    ).

    % The item number of a declaration, and its context ("inc" is shorthand
    % for "item number and context"). The declaration may be a predicate's
    % `:- pred' or `:- func' declaration, or a procedure's
    % `:- mode' declaration.
:- type inc
    --->    inc(int, prog_context).

:- type does_warning_make_sense
    --->    warning_makes_sense
    ;       warning_does_not_makes_sense.

:- pred gather_proc_decl_item_number(proc_id::in, proc_info::in,
    int::in, int::out, list(inc)::in, list(inc)::out,
    does_warning_make_sense::in, does_warning_make_sense::out) is det.

gather_proc_decl_item_number(ProcId, ProcInfo, !ExpectedProcNum,
        !ProcINCs, !MakesSense) :-
    ( if proc_id_to_int(ProcId) = !.ExpectedProcNum then
        !:ExpectedProcNum = !.ExpectedProcNum + 1,
        proc_info_get_item_number(ProcInfo, ItemNumber),
        (
            ItemNumber = item_seq_num(SeqNum),
            proc_info_get_context(ProcInfo, Context),
            ProcINC = inc(SeqNum, Context),
            !:ProcINCs = [ProcINC | !.ProcINCs]
        ;
            ItemNumber = item_no_seq_num,
            % The procedure was declared either as part of a predmode
            % declaration, in which case requiring it to have an item number
            % that *follows* the item number of the pred or func declaration
            % does not make sense, or the procedure was declared by the
            % compiler itself, in which case the notion of contiguity
            % in source code does not apply to this predicate.
            !:MakesSense = warning_does_not_makes_sense
        )
    else
        % There is a missing procedure number in the sequence of procedures
        % in this predicate. We delete procedures from predicates only when
        % we detect an error in them. When we did so, we must have already
        % generated a message for that error. Fixing that error may also
        % fix any current non-contiguity in the procedures' item numbers.
        % Generating an error message for such non-contiguities here
        % could therefore be misleading.
        %
        % XXX If the only procedure in a predicate that has been deleted
        % due to a mode error is the last one, then execution won't get here,
        % and so in that case we *can* generate just such a potentially
        % misleading message.
        %
        % Given this fact, should we just generate warnings regardless of
        % the presence of any invalid modes?
        !:MakesSense = warning_does_not_makes_sense
    ).

:- pred report_any_inc_gaps(pred_info::in, inc::in, inc::in, list(inc)::in,
    int::in, style_info::in, style_info::out) is det.

report_any_inc_gaps(PredInfo, FirstINC, SecondINC, LaterINCs,
        FirstProcNum, !StyleInfo) :-
    % If FirstProcNum = 0, then FirstINC is for the predicate or function
    % declaration.
    % If FirstProcNum > 0, then FirstINC is for FirstProcNum'th
    % mode declaration.
    FirstINC = inc(FirstItemNumber, FirstContext),
    SecondINC = inc(SecondItemNumber, SecondContext),
    ( if
        (
            % The usual case: a predicate declaration followed immediately
            % by one or more mode declaration for the predicate.
            FirstItemNumber + 1 = SecondItemNumber
        ;
            % If the predicate has a predmode declaration, then
            % the item number of the first (and only) mode declaration
            % can be same as the item number of the predicate declaration.
            FirstProcNum = 0,
            FirstItemNumber = SecondItemNumber,
            LaterINCs = []
        )
    then
        true
    else
        PredPieces = describe_one_pred_info_name(yes(color_subject),
            should_not_module_qualify, [], PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        ( if FirstProcNum = 0 then
            FirstPieces = [words("Warning: the"), words(PredOrFuncStr),
                words("declaration for")] ++ PredPieces ++
                [words("is")] ++
                color_as_incorrect([words("not followed immediately by"),
                    words("its first mode declaration.")]) ++
                [nl],
            SecondPieces = [words("The first mode declaration"),
                words("is here.")]
        else
            FirstPieces = [words("Warning: the"), nth_fixed(FirstProcNum),
                words("mode declaration for")] ++ PredPieces ++
                [words("is")] ++
                color_as_incorrect([words("not followed immediately by its"),
                    nth_fixed(FirstProcNum + 1),
                    words("mode declaration.")]) ++
                [nl],
            SecondPieces = [words("The"), nth_fixed(FirstProcNum + 1),
                words("mode declaration is here."), nl]
        ),
        FirstMsg = msg(FirstContext, FirstPieces),
        SecondMsg = msg(SecondContext, SecondPieces),
        Spec = error_spec($pred, severity_warning, phase_style,
            [FirstMsg, SecondMsg]),
        Specs0 = !.StyleInfo ^ style_decl_gap_specs,
        Specs = [Spec | Specs0],
        !StyleInfo ^ style_decl_gap_specs := Specs
    ),
    (
        LaterINCs = []
    ;
        LaterINCs = [ThirdINC | LaterLaterINCs],
        report_any_inc_gaps(PredInfo, SecondINC, ThirdINC, LaterLaterINCs,
            FirstProcNum + 1, !StyleInfo)
    ).

%---------------------------------------------------------------------------%
%
% Code for task 2.
%

:- pred maybe_gather_clause_gap_info(pred_info::in, clause_item_numbers::in,
    style_info::in, style_info::out) is det.

maybe_gather_clause_gap_info(PredInfo, ItemNumbers, !StyleInfo) :-
    MaybeNonContigDefns = !.StyleInfo ^ style_non_contig_defns,
    (
        MaybeNonContigDefns = do_not_warn_non_contiguous_pred_defns
    ;
        MaybeNonContigDefns = warn_non_contiguous_pred_defns(NumberTypes),
        ( if
            clauses_are_non_contiguous(ItemNumbers, NumberTypes,
                FirstRegion, SecondRegion, LaterRegions)
            % XXX Currently, despite the existence of a field in ClausesInfo
            % to record whether PredInfo had a clause that is NOT in
            % ClausesInfo because it had a syntax error, we never set
            % that field to "some_clause_syntax_errors". If we ever do,
            % we may want to add a check for no_clause_syntax_errors here,
            % *if and only if* that malformed clause would cause a gap
            % in the item number sequence.
        then
            Spec = report_non_contiguous_clauses(PredInfo,
                FirstRegion, SecondRegion, LaterRegions),
            ClauseGapSpecs0 = !.StyleInfo ^ style_clause_gap_specs,
            ClauseGapSpecs = [Spec | ClauseGapSpecs0],
            !StyleInfo ^ style_clause_gap_specs := ClauseGapSpecs
        else
            true
        )
    ).

:- func report_non_contiguous_clauses(pred_info,
    clause_item_number_region, clause_item_number_region,
    list(clause_item_number_region)) = error_spec.

report_non_contiguous_clauses(PredInfo, FirstRegion, SecondRegion,
        LaterRegions) = Spec :-
    PredPieces = describe_one_pred_info_name(no,
        should_not_module_qualify, [], PredInfo),
    PredDotPieces = describe_one_pred_info_name(yes(color_subject),
        should_not_module_qualify, [suffix(".")], PredInfo),
    FrontPieces = [words("Warning:")] ++
        color_as_incorrect([words("non-contiguous clauses")]) ++
        [words("for")] ++ PredDotPieces ++ [nl],
    pred_info_get_context(PredInfo, Context),
    FrontMsg = msg(Context, FrontPieces),
    report_non_contiguous_clause_contexts(PredPieces, 1,
        FirstRegion, SecondRegion, LaterRegions, ContextMsgs),
    Msgs = [FrontMsg | ContextMsgs],
    Spec = error_spec($pred, severity_warning, phase_type_check, Msgs).

:- pred report_non_contiguous_clause_contexts(list(format_piece)::in,
    int::in, clause_item_number_region::in, clause_item_number_region::in,
    list(clause_item_number_region)::in, list(error_msg)::out) is det.

report_non_contiguous_clause_contexts(PredPieces, GapNumber,
        FirstRegion, SecondRegion, LaterRegions, Msgs) :-
    FirstRegion =
        clause_item_number_region(_FirstLowerNumber, _FirstUpperNumber,
        _FirstLowerContext, FirstUpperContext),
    SecondRegion =
        clause_item_number_region(_SecondLowerNumber, _SecondUpperNumber,
        SecondLowerContext, _SecondUpperContext),
    ( if
        GapNumber = 1,
        LaterRegions = []
    then
        % There is only one gap, so don't number it.
        GapPieces = []
    else
        GapPieces = [int_fixed(GapNumber)]
    ),
    % The wording here is chosen be non-confusing even if a clause has a gap
    % both before and after it, so that gaps both end and start at the context
    % of that clause. We could do better if we had separate contexts for the
    % start and the end of the clause, but we don't.
    FirstPieces = [words("Gap") | GapPieces] ++
        [words("in clauses of") | PredPieces] ++
        [words("starts after this clause."), nl],
    SecondPieces = [words("Gap") | GapPieces] ++
        [words("in clauses of") | PredPieces] ++
        [words("ends with this clause."), nl],
    FirstMsg = msg(FirstUpperContext, FirstPieces),
    SecondMsg = msg(SecondLowerContext, SecondPieces),
    (
        LaterRegions = [],
        Msgs = [FirstMsg, SecondMsg]
    ;
        LaterRegions = [FirstLaterRegion | LaterLaterRegions],
        report_non_contiguous_clause_contexts(PredPieces, GapNumber + 1,
            SecondRegion, FirstLaterRegion, LaterLaterRegions, LaterMsgs),
        Msgs = [FirstMsg, SecondMsg | LaterMsgs]
    ).


%---------------------------------------------------------------------------%
%
% Code for task 3.
%

:- pred maybe_gather_decl_vs_defn_order_info(pred_id::in, pred_info::in,
    decl_section::in, int::in, clause_item_numbers::in,
    style_info::in, style_info::out) is det.

maybe_gather_decl_vs_defn_order_info(PredId, PredInfo,
        DeclSection, PredDeclItemNumber, ClauseItemNumbers, !StyleInfo) :-
    WarnPredDeclDefnOrder = !.StyleInfo ^ style_warn_decl_vs_defn,
    (
        WarnPredDeclDefnOrder = do_not_warn_pred_decl_vs_defn_order
    ;
        WarnPredDeclDefnOrder = warn_pred_decl_vs_defn_order(DefnKind),
        % Gather information for our caller to use in generating warnings
        % for --warn-inconsistent-pred-order-clauses if warranted.
        clause_item_number_regions(ClauseItemNumbers, DefnKind, Regions),
        (
            Regions = []
            % This can happen for predicates implemented via external code.
            % For these, there is no visible "definition" to be
            % out-of-order with respect to the declaration.
        ;
            Regions = [FirstRegion | _],
            FirstRegion = clause_item_number_region(FirstClauseItemNumber,
                _, _, _),
            PredDeclItemNumbers = pred_decl_item_numbers(PredId, PredInfo,
                PredDeclItemNumber, FirstClauseItemNumber),
            (
                DeclSection = decl_interface,
                ExportedPDINs0 = !.StyleInfo ^ style_exported_preds,
                ExportedPDINs = [PredDeclItemNumbers | ExportedPDINs0],
                !StyleInfo ^ style_exported_preds := ExportedPDINs
            ;
                DeclSection = decl_implementation,
                NonExportedPDINs0 = !.StyleInfo ^ style_nonexported_preds,
                NonExportedPDINs = [PredDeclItemNumbers | NonExportedPDINs0],
                !StyleInfo ^ style_nonexported_preds := NonExportedPDINs
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred generate_inconsistent_pred_order_warnings(prog_context::in,
    string::in, list(pred_decl_item_numbers)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

generate_inconsistent_pred_order_warnings(ModuleContext, ExportedOrNotStr,
        PredItemNumbers, !Specs) :-
    list.sort(compare_decl_item_number, PredItemNumbers, DeclOrder),
    list.sort(compare_defn_item_number, PredItemNumbers, DefnOrder),
    ( if DeclOrder = DefnOrder then
        true
    else
        list.map(desc_pred_decl_item_numbers, DeclOrder, DeclStrs),
        list.map(desc_pred_decl_item_numbers, DefnOrder, DefnStrs),
        CostDelete = 1,
        CostInsert = 1,
        CostReplace = 1,
        Params = edit_params(CostDelete, CostInsert, CostReplace),
        find_shortest_edit_seq(Params, DeclStrs, DefnStrs, EditSeq),
        find_diff_seq(DeclStrs, EditSeq, DiffSeq),
        find_change_hunks(3, DiffSeq, ChangeHunks),
        chunks_to_spec(ModuleContext, ExportedOrNotStr, ChangeHunks, WarnSpec),
        !:Specs = [WarnSpec | !.Specs]
    ).

%---------------------%

:- pred compare_decl_item_number(
    pred_decl_item_numbers::in, pred_decl_item_numbers::in,
    comparison_result::out) is det.

compare_decl_item_number(A, B, R) :-
    A = pred_decl_item_numbers(_, _, ItemNumberA, _),
    B = pred_decl_item_numbers(_, _, ItemNumberB, _),
    compare(R, ItemNumberA, ItemNumberB).

:- pred compare_defn_item_number(
    pred_decl_item_numbers::in, pred_decl_item_numbers::in,
    comparison_result::out) is det.

compare_defn_item_number(A, B, R) :-
    A = pred_decl_item_numbers(_, _, _, ItemNumberA),
    B = pred_decl_item_numbers(_, _, _, ItemNumberB),
    compare(R, ItemNumberA, ItemNumberB).

%---------------------%

:- pred desc_pred_decl_item_numbers(pred_decl_item_numbers::in,
    string::out) is det.

desc_pred_decl_item_numbers(PredItemNumbers, PredDescStr) :-
    PredItemNumbers = pred_decl_item_numbers(_, PredInfo, _, _),
    PredPieces = describe_one_pred_info_name(no, should_not_module_qualify,
        [], PredInfo),
    PredDescStr = error_pieces_to_one_line_string(PredPieces).

%---------------------%

:- pred chunks_to_spec(prog_context::in, string::in,
    list(change_hunk(string))::in, error_spec::out) is det.

chunks_to_spec(ModuleContext, ExportedOrNotStr, ChangeHunks, Spec) :-
    HeadPieces = [words("Warning:")] ++
        color_as_subject([words("the order of"),
            words("the declarations and definitions")]) ++
        [words("of the"), words(ExportedOrNotStr), words("predicates"),
            words("is")] ++
        color_as_incorrect([words("inconsistent,")]) ++
        [words("as shown by this diff:"), nl,
        blank_line,
        fixed("--- declaration order"), nl,
        fixed("+++ definition order"), nl],
    list.map(change_hunk_to_pieces, ChangeHunks, ChangeHunkPieceLists),
    list.condense(ChangeHunkPieceLists, ChangeHunkPieces),
    Pieces = HeadPieces ++ ChangeHunkPieces,
    Spec = spec($pred, severity_warning, phase_style, ModuleContext, Pieces).

%---------------------------------------------------------------------------%
:- end_module check_hlds.style_checks.
%---------------------------------------------------------------------------%
