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
:- import_module parse_tree.error_util.

:- import_module io.
:- import_module list.

:- type style_warnings_task
    --->    non_contiguous_decls_only
    ;       inconsistent_pred_order_only(
                clause_item_number_types
            )
    ;       non_contiguous_decls_and_inconsistent_pred_order(
                clause_item_number_types
            ).

:- pred generate_style_warnings(module_info::in, style_warnings_task::in,
    list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

generate_style_warnings(ModuleInfo, Task, !:Specs, !IO) :-
    module_info_get_valid_pred_ids(ModuleInfo, ValidPredIds),
    StyleInfo0 = pred_style_info([], [], []),
    (
        Task = non_contiguous_decls_only,
        MaybeDefnKind = no
    ;
        ( Task = inconsistent_pred_order_only(DefnKind)
        ; Task = non_contiguous_decls_and_inconsistent_pred_order(DefnKind)
        ),
        MaybeDefnKind = yes(DefnKind)
    ),
    list.foldl(detect_non_contiguous_pred_decls(ModuleInfo, MaybeDefnKind),
        ValidPredIds, StyleInfo0, StyleInfo),
    StyleInfo = pred_style_info(ExportedPreds, NonExportedPreds,
        ModeDeclItemNumberSpecs),
    (
        Task = non_contiguous_decls_only,
        !:Specs = ModeDeclItemNumberSpecs
    ;
        (
            Task = inconsistent_pred_order_only(_),
            !:Specs = []
        ;
            Task = non_contiguous_decls_and_inconsistent_pred_order(_),
            !:Specs = ModeDeclItemNumberSpecs
        ),
        module_info_get_name_context(ModuleInfo, ModuleContext),
        generate_inconsistent_pred_order_warnings(ModuleContext,
            "exported", ExportedPreds, !Specs, !IO),
        generate_inconsistent_pred_order_warnings(ModuleContext,
            "nonexported", NonExportedPreds, !Specs, !IO)
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
    maybe(clause_item_number_types)::in, pred_id::in,
    pred_style_info::in, pred_style_info::out) is det.

detect_non_contiguous_pred_decls(ModuleInfo, MaybeDefnKind, PredId,
        !StyleInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_cur_user_decl_info(PredInfo, MaybeDeclInfo),
    (
        MaybeDeclInfo = no
    ;
        MaybeDeclInfo = yes(DeclInfo),

        % Generate warnings for --warn-non-contiguous-decls if warranted.
        DeclInfo = cur_user_decl_info(DeclSection, _, PredDeclItemNumber),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.foldl2_values(gather_proc_item_numbers, ProcTable,
            [], UnsortedProcINCs, proc_contiguity_makes_sense, MakesSense),
        list.sort(UnsortedProcINCs, ProcINCs),
        ( if
            MakesSense = proc_contiguity_makes_sense,
            ProcINCs = [HeadProcINC | TailProcINCs]
        then
            % invis_order; "ends here" first
            pred_info_get_context(PredInfo, PredDeclContext),
            PredINC = inc(PredDeclItemNumber, PredDeclContext),
            Specs0 = !.StyleInfo ^ style_specs,
            report_any_inc_gaps(PredInfo, PredINC, HeadProcINC, TailProcINCs,
                0, Specs0, Specs),
            !StyleInfo ^ style_specs := Specs
        else
            true
        ),

        (
            MaybeDefnKind = no
        ;
            MaybeDefnKind = yes(DefnKind),
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
    ).

%---------------------------------------------------------------------------%

    % The item number of a declaration, and its context ("inc" is
    % shorthand for "item number and context"). The declaration
    % may be a predicate's `:- pred' or `:- func' declaration,
    % or a procedure's `:- mode' declaration.
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
        ( if ItemNumber > 1 then
            proc_info_get_context(ProcInfo, Context),
            ProcINC = inc(ItemNumber, Context),
            !:ProcINCs = [ProcINC | !.ProcINCs]
        else
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
    int::in, list(error_spec)::in, list(error_spec)::out) is det.

report_any_inc_gaps(PredInfo, FirstINC, SecondINC, LaterINCs,
        FirstProcNum, !Specs) :-
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
        FirstMsg = simple_msg(FirstContext, [always(FirstPieces)]),
        SecondMsg = simple_msg(SecondContext, [always(SecondPieces)]),
        Spec = error_spec(severity_warning, phase_style,
            [FirstMsg, SecondMsg]),
        !:Specs = [Spec | !.Specs]
    ),
    (
        LaterINCs = []
    ;
        LaterINCs = [ThirdINC | LaterLaterINCs],
        report_any_inc_gaps(PredInfo, SecondINC, ThirdINC, LaterLaterINCs,
            FirstProcNum + 1, !Specs)
    ).

%---------------------------------------------------------------------------%

:- pred generate_inconsistent_pred_order_warnings(prog_context::in,
    string::in, list(pred_item_numbers)::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

generate_inconsistent_pred_order_warnings(ModuleContext, ExportedOrNotStr,
        PredItemNumbers, !Specs, !IO) :-
    list.sort(compare_decl_item_number, PredItemNumbers, DeclOrder),
    list.sort(compare_defn_item_number, PredItemNumbers, DefnOrder),
    ( if DeclOrder = DefnOrder then
        true
    else
        some [!TempFileNames] (
            make_order_temp_file(ExportedOrNotStr, "declaration_order",
                DeclOrder, DeclResult, !IO),
            make_order_temp_file(ExportedOrNotStr, "definition_order",
                DefnOrder, DefnResult, !IO),
            (
                DeclResult = error_specs(HeadDeclSpec, TailDeclSpecs),
                DefnResult = error_specs(HeadDefnSpec, TailDefnSpecs),
                !:TempFileNames = [],
                !:Specs =
                    [HeadDeclSpec | TailDeclSpecs] ++
                    [HeadDefnSpec | TailDefnSpecs] ++ !.Specs
            ;
                DeclResult = error_specs(HeadDeclSpec, TailDeclSpecs),
                DefnResult = ok_no_spec(DefnFileName),
                !:TempFileNames = [DefnFileName],
                !:Specs = [HeadDeclSpec | TailDeclSpecs] ++ !.Specs
            ;
                DeclResult = ok_no_spec(DeclFileName),
                DefnResult = error_specs(HeadDefnSpec, TailDefnSpecs),
                !:TempFileNames = [DeclFileName],
                !:Specs = [HeadDefnSpec | TailDefnSpecs] ++ !.Specs
            ;
                DeclResult = ok_no_spec(DeclFileName),
                DefnResult = ok_no_spec(DefnFileName),
                !:TempFileNames = [DeclFileName, DefnFileName],
                io.make_temp_file("/tmp", "difference", "", DiffTempResult,
                    !IO),
                (
                    DiffTempResult = error(DiffTempError),
                    DiffTempSpec =
                        cannot_create_temp_file_spec(ExportedOrNotStr,
                            DiffTempError),
                    !:Specs = [DiffTempSpec | !.Specs]
                ;
                    DiffTempResult = ok(DiffFileName),
                    !:TempFileNames = [DiffFileName | !.TempFileNames],
                    string.format("diff -u %s %s > %s",
                        [s(DeclFileName), s(DefnFileName), s(DiffFileName)],
                        Cmd),
                    io.call_system(Cmd, CmdResult, !IO),
                    (
                        CmdResult = error(CmdError),
                        CmdSpec = cannot_execute_cmd_spec(ExportedOrNotStr,
                            Cmd, CmdError),
                        !:Specs = [CmdSpec | !.Specs]
                    ;
                        CmdResult = ok(_ExitStatus),
                        io.open_input(DiffFileName, DiffOpenResult, !IO),
                        (
                            DiffOpenResult = error(DiffOpenError),
                            DiffOpenSpec =
                                cannot_open_temp_file_spec(ExportedOrNotStr,
                                    DiffFileName, DiffOpenError),
                            !:Specs = [DiffOpenSpec | !.Specs]
                        ;
                            DiffOpenResult = ok(DiffStream),
                            diff_file_to_spec(DiffStream, DiffFileName,
                                ModuleContext, ExportedOrNotStr, DiffReadSpec,
                                !IO),
                            io.close_input(DiffStream, !IO),
                            !:Specs = [DiffReadSpec | !.Specs]
                        )
                    )
                )
            ),
            list.map_foldl(io.remove_file, !.TempFileNames, _Results, !IO)
        )
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

:- pred make_order_temp_file(string::in, string::in,
    list(pred_item_numbers)::in, maybe_error_specs(string)::out,
    io::di, io::uo) is det.

make_order_temp_file(ExportedOrNotStr, DeclOrDefnOrderStr, PredItemNumbers,
        MaybeFileName, !IO) :-
    io.make_temp_file("/tmp", DeclOrDefnOrderStr, "", TempResult, !IO),
    (
        TempResult = error(TempError),
        TempSpec = cannot_create_temp_file_spec(ExportedOrNotStr, TempError),
        MaybeFileName = error_specs(TempSpec, [])
    ;
        TempResult = ok(TempFileName),
        io.open_output(TempFileName, OpenResult, !IO),
        (
            OpenResult = error(OpenError),
            OpenSpec = cannot_open_temp_file_spec(ExportedOrNotStr,
                TempFileName, OpenError),
            MaybeFileName = error_specs(OpenSpec, [])
        ;
            OpenResult = ok(Stream),
            list.foldl(write_pred_desc(Stream), PredItemNumbers, !IO),
            io.close_output(Stream, !IO),
            MaybeFileName = ok_no_spec(TempFileName)
        )
    ).

:- pred write_pred_desc(io.text_output_stream::in, pred_item_numbers::in,
    io::di, io::uo) is det.

write_pred_desc(Stream, PredItemNumbers, !IO) :-
    PredItemNumbers = pred_item_numbers(_, PredInfo, _, _),
    PredPieces =
        describe_one_pred_info_name(should_not_module_qualify, PredInfo),
    PredDesc = error_pieces_to_string(PredPieces),
    io.write_string(Stream, PredDesc, !IO),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%

:- pred diff_file_to_spec(io.text_input_stream::in, string::in,
    prog_context::in, string::in, error_spec::out, io::di, io::uo) is det.

diff_file_to_spec(DiffStream, DiffFileName, ModuleContext, ExportedOrNotStr,
        Spec, !IO) :-
    read_lines_as_rev_strings(DiffStream, [], RevLines, MaybeIOError, !IO),
    (
        MaybeIOError = yes(Error),
        io.error_message(Error, ErrorMsg),
        Pieces = [words("Cannot generate diagnostics"),
            words("about inconsistencies between the order of"),
            words("the declarations and definitions of"),
            words(ExportedOrNotStr), words("predicates,"),
            words("because reading of the temporary file"),
            fixed(DiffFileName), words("failed:"), nl,
            words(ErrorMsg), suffix("."), nl],
        Msg = error_msg(no, do_not_treat_as_first, 0, [always(Pieces)]),
        Spec = error_spec(severity_warning, phase_style, [Msg])
    ;
        MaybeIOError = no,
        list.reverse(RevLines, Lines),
        ( if
            Lines = [Line1, Line2 | DiffLines],
            string.prefix(Line1, "--- "),
            string.prefix(Line2, "+++ ")
        then
            HeadPieces = [words("Warning: the order of"),
                words("the declarations and definitions"),
                words("of the"), words(ExportedOrNotStr), words("predicates"),
                words("is inconsistent, as shown by this diff:"), nl,
                blank_line,
                fixed("--- declaration order"), nl,
                fixed("+++ definition order"), nl],
            list.map(diff_line_to_pieces, DiffLines, DiffPieceLists),
            list.condense(DiffPieceLists, DiffPieces),
            Pieces = HeadPieces ++ DiffPieces,
            Msg = simple_msg(ModuleContext, [always(Pieces)]),
            Spec = error_spec(severity_warning, phase_style, [Msg])
        else
            Pieces = [words("Cannot generate diagnostics"),
                words("about inconsistencies between the order of"),
                words("the declarations and definitions of"),
                words(ExportedOrNotStr), words("predicates,"),
                words("because the output of"), quote("diff -u"),
                words("does not have the expected format."), nl],
            Msg = error_msg(no, do_not_treat_as_first, 0, [always(Pieces)]),
            Spec = error_spec(severity_warning, phase_style, [Msg])
        )
    ).

:- pred read_lines_as_rev_strings(io.text_input_stream::in,
    list(string)::in, list(string)::out, maybe(io.error)::out,
    io::di, io::uo) is det.

read_lines_as_rev_strings(Stream, !RevLines, MaybeIOError, !IO) :-
    io.read_line_as_string(Stream, Result, !IO),
    (
        Result = error(Error),
        MaybeIOError = yes(Error)
    ;
        Result = eof,
        MaybeIOError = no
    ;
        Result = ok(Line),
        !:RevLines = [Line | !.RevLines],
        read_lines_as_rev_strings(Stream, !RevLines, MaybeIOError, !IO)
    ).

:- pred diff_line_to_pieces(string::in, list(format_component)::out) is det.

diff_line_to_pieces(Line0, Pieces) :-
    Line = string.remove_suffix_if_present("\n", Line0),
    Pieces = [fixed(Line), nl].

%---------------------------------------------------------------------------%

:- func cannot_create_temp_file_spec(string, io.error) = error_spec.

cannot_create_temp_file_spec(ExportedOrNotStr, Error) = Spec :-
    io.error_message(Error, ErrorMsg),
    Pieces = [words("Cannot generate diagnostics"),
        words("about inconsistencies between the order of"),
        words("the declarations and definitions of the"),
        words(ExportedOrNotStr), words("predicates,"),
        words("because the creation of a temporary file failed:"), nl,
        words(ErrorMsg), suffix("."), nl],
    Msg = error_msg(no, do_not_treat_as_first, 0, [always(Pieces)]),
    Spec = error_spec(severity_warning, phase_style, [Msg]).

:- func cannot_open_temp_file_spec(string, string, io.error) = error_spec.

cannot_open_temp_file_spec(ExportedOrNotStr, TempFileName, Error) = Spec :-
    io.error_message(Error, ErrorMsg),
    Pieces = [words("Cannot generate diagnostics"),
        words("about inconsistencies between the order of"),
        words("the declarations and definitions of"),
        words(ExportedOrNotStr), words("predicates,"),
        words("because the opening of the temporary file"),
        fixed(TempFileName), words("failed:"), nl,
        words(ErrorMsg), suffix("."), nl],
    Msg = error_msg(no, do_not_treat_as_first, 0, [always(Pieces)]),
    Spec = error_spec(severity_warning, phase_style, [Msg]).

:- func cannot_execute_cmd_spec(string, string, io.error) = error_spec.

cannot_execute_cmd_spec(ExportedOrNotStr, Cmd, Error) = Spec :-
    io.error_message(Error, ErrorMsg),
    Pieces = [words("Cannot generate diagnostics"),
        words("about inconsistencies between the order of"),
        words("the declarations and definitions of the"),
        words(ExportedOrNotStr), words("predicates,"),
        words("because the execution of the following command failed:"), nl,
        fixed(Cmd), nl,
        words("The error message was:"), nl,
        words(ErrorMsg), suffix("."), nl],
    Msg = error_msg(no, do_not_treat_as_first, 0, [always(Pieces)]),
    Spec = error_spec(severity_warning, phase_style, [Msg]).

%---------------------------------------------------------------------------%
:- end_module check_hlds.style_checks.
%---------------------------------------------------------------------------%
