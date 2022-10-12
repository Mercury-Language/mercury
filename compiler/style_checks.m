%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: style_checks.m.
% Main author: zs.
%
% This module generates warnings for some aspects of bad style.
% Specifically, it implements the following compiler options:
%
% --warn-inconsistent-pred-order
% --warn-inconsistent-pred-order-clauses
% --warn-inconsistent-pred-order-foreign-procs

:- module check_hlds.style_checks.

:- interface.

:- import_module hlds.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

:- type maybe_warn_non_contiguous_pred_decls
    --->    do_not_warn_non_contiguous_pred_decls
    ;       warn_non_contiguous_pred_decls.

:- type maybe_warn_pred_decl_vs_defn_order
    --->    do_not_warn_pred_decl_vs_defn_order
    ;       warn_pred_decl_vs_defn_order(clause_item_number_types).

:- pred generate_style_warnings(module_info::in,
    maybe_warn_non_contiguous_pred_decls::in,
    maybe_warn_pred_decl_vs_defn_order::in,
    list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.write_error_spec.

:- import_module edit_seq.
:- import_module int.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

generate_style_warnings(ModuleInfo, WarnNonContigPreds, WarnPredDeclDefnOrder,
        !:Specs) :-
    module_info_get_valid_pred_ids(ModuleInfo, ValidPredIds),
    StyleInfo0 = pred_style_info([], [], []),
    list.foldl(
        detect_non_contiguous_pred_decls(ModuleInfo, WarnPredDeclDefnOrder),
        ValidPredIds, StyleInfo0, StyleInfo),
    StyleInfo = pred_style_info(ExportedPreds, NonExportedPreds,
        ModeDeclItemNumberSpecs),
    (
        WarnNonContigPreds = do_not_warn_non_contiguous_pred_decls,
        % Even though we are throwing away ModeDeclItemNumberSpecs,
        % We still execute the code that computes it, because it also
        % computes ExportedPreds and NonExportedPreds, which we need.
        !:Specs = []
    ;
        WarnNonContigPreds = warn_non_contiguous_pred_decls,
        !:Specs = ModeDeclItemNumberSpecs
    ),
    (
        WarnPredDeclDefnOrder = do_not_warn_pred_decl_vs_defn_order,
        !:Specs = ModeDeclItemNumberSpecs
    ;
        WarnPredDeclDefnOrder = warn_pred_decl_vs_defn_order(_),
        module_info_get_name_context(ModuleInfo, ModuleContext),
        generate_inconsistent_pred_order_warnings(ModuleContext,
            "exported", ExportedPreds, !Specs),
        generate_inconsistent_pred_order_warnings(ModuleContext,
            "nonexported", NonExportedPreds, !Specs)
    ).

%---------------------------------------------------------------------------%

    % The id and pred_info of a predicate together with the item number
    % (which must be valid) of
    %
    % - its pred or func declaration, and
    % - its first clause.
    %
:- type pred_item_numbers
    --->    pred_item_numbers(
                piwin_pred_id                   :: pred_id,
                piwin_pred_info                 :: pred_info,
                piwin_decl_item_number          :: int,
                piwin_first_defn_item_number    :: int
            ).

:- type pred_style_info
    --->    pred_style_info(
                style_exported_preds            :: list(pred_item_numbers),
                style_nonexported_preds         :: list(pred_item_numbers),
                style_specs                     :: list(error_spec)
            ).

:- pred detect_non_contiguous_pred_decls(module_info::in,
    maybe_warn_pred_decl_vs_defn_order::in, pred_id::in,
    pred_style_info::in, pred_style_info::out) is det.

detect_non_contiguous_pred_decls(ModuleInfo, WarnPredDeclDefnOrder, PredId,
        !StyleInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_cur_user_decl_info(PredInfo, MaybeDeclInfo),
    % Generate warnings for --warn-non-contiguous-decls if warranted.
    ( if
        MaybeDeclInfo = yes(DeclInfo),
        DeclInfo = cur_user_decl_info(DeclSection, _, MaybePredDeclItemNumber),
        MaybePredDeclItemNumber = item_seq_num(PredDeclItemNumber)
    then
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.foldl2_values(gather_proc_item_numbers, ProcTable,
            [], UnsortedProcINCs, proc_contiguity_makes_sense, MakesSense),
        list.sort(UnsortedProcINCs, ProcINCs),
        ( if
            MakesSense = proc_contiguity_makes_sense,
            ProcINCs = [HeadProcINC | TailProcINCs]
        then
            pred_info_get_context(PredInfo, PredDeclContext),
            PredINC = inc(PredDeclItemNumber, PredDeclContext),
            report_any_inc_gaps(PredInfo, PredINC, HeadProcINC, TailProcINCs,
                0, !StyleInfo)
        else
            true
        ),

        (
            WarnPredDeclDefnOrder = do_not_warn_pred_decl_vs_defn_order
        ;
            WarnPredDeclDefnOrder = warn_pred_decl_vs_defn_order(DefnKind),
            % Gather information for our caller to use in generating warnings
            % for --warn-inconsistent-pred-order if warranted.
            pred_info_get_clauses_info(PredInfo, ClausesInfo),
            clauses_info_get_clauses_rep(ClausesInfo, _ClausesRep,
                ClauseItemNumbers),
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
                PredItemNumbers = pred_item_numbers(PredId, PredInfo,
                    PredDeclItemNumber, FirstClauseItemNumber),
                (
                    DeclSection = decl_interface,
                    ExportedPINs0 = !.StyleInfo ^ style_exported_preds,
                    ExportedPINs = [PredItemNumbers | ExportedPINs0],
                    !StyleInfo ^ style_exported_preds := ExportedPINs
                ;
                    DeclSection = decl_implementation,
                    NonExportedPINs0 = !.StyleInfo ^ style_nonexported_preds,
                    NonExportedPINs = [PredItemNumbers | NonExportedPINs0],
                    !StyleInfo ^ style_nonexported_preds := NonExportedPINs
                )
            )
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

    % The item number of a declaration, and its context ("inc" is shorthand
    % for "item number and context"). The declaration may be a predicate's
    % `:- pred' or `:- func' declaration, or a procedure's
    % `:- mode' declaration.
:- type inc
    --->    inc(int, prog_context).

:- type proc_contiguity
    --->    proc_contiguity_makes_sense
    ;       proc_contiguity_does_not_makes_sense.

:- pred gather_proc_item_numbers(proc_info::in,
    list(inc)::in, list(inc)::out,
    proc_contiguity::in, proc_contiguity::out) is det.

gather_proc_item_numbers(ProcInfo, !ProcINCs, !MakesSense) :-
    ( if proc_info_is_valid_mode(ProcInfo) then
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
            !:MakesSense = proc_contiguity_does_not_makes_sense
        )
    else
        % We must have already generated an error message that says
        % *why* the invalid procedure is invalid. Fixing that error
        % may also fix any current non-contiguity in the procedures'
        % item numbers. Generating an error message for such
        % non-contiguities here could therefore be misleading.
        !:MakesSense = proc_contiguity_does_not_makes_sense
    ).

:- pred report_any_inc_gaps(pred_info::in, inc::in, inc::in, list(inc)::in,
    int::in, pred_style_info::in, pred_style_info::out) is det.

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
        PredPieces = describe_one_pred_info_name(should_not_module_qualify,
            PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        ( if FirstProcNum = 0 then
            FirstPieces = [words("Warning: the"), words(PredOrFuncStr),
                words("declaration for") | PredPieces] ++
                [words("is not followed immediately by"),
                words("its first mode declaration."), nl],
            SecondPieces = [words("The first mode declaration"),
                words("is here.")]
        else
            FirstPieces = [words("Warning: the"), nth_fixed(FirstProcNum),
                words("mode declaration for") | PredPieces] ++
                [words("is not followed immediately by its"),
                nth_fixed(FirstProcNum + 1), words("mode declaration."), nl],
            SecondPieces = [words("The"), nth_fixed(FirstProcNum + 1),
                words("mode declaration is here."), nl]
        ),
        FirstMsg = simplest_msg(FirstContext, FirstPieces),
        SecondMsg = simplest_msg(SecondContext, SecondPieces),
        Spec = error_spec($pred, severity_warning, phase_style,
            [FirstMsg, SecondMsg]),
        Specs0 = !.StyleInfo ^ style_specs,
        Specs = [Spec | Specs0],
        !StyleInfo ^ style_specs := Specs
    ),
    (
        LaterINCs = []
    ;
        LaterINCs = [ThirdINC | LaterLaterINCs],
        report_any_inc_gaps(PredInfo, SecondINC, ThirdINC, LaterLaterINCs,
            FirstProcNum + 1, !StyleInfo)
    ).

%---------------------------------------------------------------------------%

:- pred generate_inconsistent_pred_order_warnings(prog_context::in,
    string::in, list(pred_item_numbers)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

generate_inconsistent_pred_order_warnings(ModuleContext, ExportedOrNotStr,
        PredItemNumbers, !Specs) :-
    list.sort(compare_decl_item_number, PredItemNumbers, DeclOrder),
    list.sort(compare_defn_item_number, PredItemNumbers, DefnOrder),
    ( if DeclOrder = DefnOrder then
        true
    else
        list.map(desc_pred_item_numbers, DeclOrder, DeclStrs),
        list.map(desc_pred_item_numbers, DefnOrder, DefnStrs),
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

%---------------------------------------------------------------------------%

:- pred compare_decl_item_number(pred_item_numbers::in, pred_item_numbers::in,
    comparison_result::out) is det.

compare_decl_item_number(A, B, R) :-
    A = pred_item_numbers(_, _, ItemNumberA, _),
    B = pred_item_numbers(_, _, ItemNumberB, _),
    compare(R, ItemNumberA, ItemNumberB).

:- pred compare_defn_item_number(pred_item_numbers::in, pred_item_numbers::in,
    comparison_result::out) is det.

compare_defn_item_number(A, B, R) :-
    A = pred_item_numbers(_, _, _, ItemNumberA),
    B = pred_item_numbers(_, _, _, ItemNumberB),
    compare(R, ItemNumberA, ItemNumberB).

%---------------------------------------------------------------------------%

:- pred desc_pred_item_numbers(pred_item_numbers::in, string::out) is det.

desc_pred_item_numbers(PredItemNumbers, PredDescStr) :-
    PredItemNumbers = pred_item_numbers(_, PredInfo, _, _),
    PredPieces =
        describe_one_pred_info_name(should_not_module_qualify, PredInfo),
    PredDescStr = error_pieces_to_string(PredPieces).

%---------------------------------------------------------------------------%

:- pred chunks_to_spec(prog_context::in, string::in,
    list(change_hunk(string))::in, error_spec::out) is det.

chunks_to_spec(ModuleContext, ExportedOrNotStr, ChangeHunks, Spec) :-
    HeadPieces = [words("Warning: the order of"),
        words("the declarations and definitions"),
        words("of the"), words(ExportedOrNotStr), words("predicates"),
        words("is inconsistent, as shown by this diff:"), nl,
        blank_line,
        fixed("--- declaration order"), nl,
        fixed("+++ definition order"), nl],
    list.map(change_hunk_to_pieces, ChangeHunks, ChangeHunkPieceLists),
    list.condense(ChangeHunkPieceLists, ChangeHunkPieces),
    Pieces = HeadPieces ++ ChangeHunkPieces,
    Spec = simplest_spec($pred, severity_warning, phase_style,
        ModuleContext, Pieces).

%---------------------------------------------------------------------------%
:- end_module check_hlds.style_checks.
%---------------------------------------------------------------------------%
