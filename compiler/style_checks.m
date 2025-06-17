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
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------------------------------------------------------------%

:- type warnings_we_want.

:- type maybe_want_style_warnings
    --->    do_not_want_style_warnings
    ;       want_style_warnings(warnings_we_want).

:- pred do_we_want_style_warnings(module_info::in,
    maybe_want_style_warnings::out, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- pred generate_any_style_warnings(module_info::in, warnings_we_want::in,
    list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.write_error_spec.

:- import_module bag.
:- import_module bool.
:- import_module edit_seq.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module uint.

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
    ;       warn_non_contiguous_pred_defns(
                clause_item_number_types,
                allowed_non_contiguity
            ).

:- type allowed_non_contiguity == list(set(pred_id)).

:- type maybe_warn_pred_decl_vs_defn_order
    --->    do_not_warn_pred_decl_vs_defn_order
    ;       warn_pred_decl_vs_defn_order(clause_item_number_types).

do_we_want_style_warnings(ModuleInfo, DoWeWantStyleWarnings, Specs) :-
    % Task 1: generate warnings if the ":- pred/func" and ":- mode"
    % declarations of this predicate or function are not contiguous.
    module_info_get_globals(ModuleInfo, Globals),
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
    get_allowed_non_contiguity(ModuleInfo, AllowedNonContiguity, Specs),
    (
        WarnNonContigForeignProcs = yes,
        NonContigDefns = warn_non_contiguous_pred_defns(
            clauses_and_foreign_procs, AllowedNonContiguity)
    ;
        WarnNonContigForeignProcs = no,
        globals.lookup_bool_option(Globals, warn_non_contiguous_clauses,
            WarnNonContigClauses),
        (
            WarnNonContigClauses = yes,
            NonContigDefns = warn_non_contiguous_pred_defns(only_clauses,
                AllowedNonContiguity)
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

:- pred get_allowed_non_contiguity(module_info::in,
    allowed_non_contiguity::out, list(error_spec)::out) is det.

get_allowed_non_contiguity(ModuleInfo, PredIdSets, !:Specs) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_accumulating_option(Globals, allow_non_contiguity_for,
        AllowNonContiguityForOpts0),
    filter_out_duplicate_options(set.init,
        AllowNonContiguityForOpts0, AllowNonContiguityForOpts),
    module_info_get_predicate_table(ModuleInfo, PredTable),
    module_info_get_name(ModuleInfo, ModuleName),
    !:Specs = [],
    list.foldl4(parse_non_contig_name_group(PredTable, ModuleName),
        AllowNonContiguityForOpts,
        1u, _, [], PredIdSets, bag.init, AllNamesBag, !Specs),
    bag.to_list_only_duplicates(AllNamesBag, DupNames),
    (
        DupNames = []
    ;
        DupNames = [_ | _],
        NameNames = choose_number(DupNames, "name", "names"),
        OccurOccurs = choose_number(DupNames, "occurs", "occur"),
        Pieces =
            [words("Error: the"), words(NameNames)] ++
            quote_list_to_color_pieces(color_subject, "and", [], DupNames) ++
            color_as_incorrect([words(OccurOccurs),
                words("more than once")]) ++
            [words("in"), quote("--allow-non-contiguity-for"),
            words("options."), nl,
            words("(The sets of predicates and/or functions"),
            words("whose clauses may be intermingled"),
            words("must all be disjoint.)"), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

    % XXX This predicate should NOT be needed, but it is (for now).
    %
    % We specify the some --allow-non-contiguity-for options for
    % e.g. the tests/invalid/bad_allow_non_contiguity_for test case
    % in tests/invalid/Mercury.options. When you compile that test case
    % by hand, it works fine. But for some reason, when it is executed
    % by tools/bootcheck, it operates as if every one of those options
    % is given *twice*, which of course triggers the error messages
    % about non-disjoint sets of predicates.
    %
    % Does anyone know why this may be happening? Since there have been
    % no changes to the bootcheck script lately, so if it is happening now,
    % it was probably happening for a long time. It's just that for most
    % options, having them set twice to the same value silently does
    % the right thing.
    %
:- pred filter_out_duplicate_options(set(string)::in,
    list(string)::in, list(string)::out) is det.

filter_out_duplicate_options(_, [], []).
filter_out_duplicate_options(!.SeenOpts,
        [HeadOpt | TailOpts], NonDupOpts) :-
    ( if set.contains(!.SeenOpts, HeadOpt) then
        filter_out_duplicate_options(!.SeenOpts, TailOpts, NonDupOpts)
    else
        set.insert(HeadOpt, !SeenOpts),
        filter_out_duplicate_options(!.SeenOpts, TailOpts, TailNonDupOpts),
        NonDupOpts = [HeadOpt | TailNonDupOpts]
    ).

:- pred parse_non_contig_name_group(predicate_table::in, module_name::in,
    string::in, uint::in, uint::out,
    list(set(pred_id))::in, list(set(pred_id))::out,
    bag(string)::in, bag(string)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_non_contig_name_group(PredTable, ModuleName, GroupStr,
        !OptNum, !GroupPredIdSets, !AllNamesBag, !Specs) :-
    GroupNames = string.split_at_char(',', GroupStr),
    (
        GroupNames = [],
        unexpected($pred, "GroupNames = []")
    ;
        GroupNames = [_],
        Pieces =
            [words("Warning: the"), unth_fixed(!.OptNum),
            quote("--allow-non-contiguity-for option"),
            words("contains just one name:")] ++
            color_as_subject([quote(GroupStr), suffix(".")]) ++
            [words("Such option values")] ++
            color_as_incorrect([words("have not effect.")]) ++ [nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces),
        !:Specs = [Spec | !.Specs]
    ;
        GroupNames = [_, _ | _]
        % This the expected case.
    ),
    bag.insert_list(GroupNames, !AllNamesBag),
    list.foldl2(parse_non_contig_name(PredTable, ModuleName, !.OptNum),
        GroupNames, set.init, GroupPredIdSet, [], GroupSpecs),
    (
        GroupSpecs = [],
        !:GroupPredIdSets = [GroupPredIdSet | !.GroupPredIdSets]
    ;
        GroupSpecs = [_ | _],
        !:Specs = GroupSpecs ++ !.Specs
    ),
    !:OptNum = !.OptNum + 1u.

:- pred parse_non_contig_name(predicate_table::in, module_name::in, uint::in,
    string::in, set(pred_id)::in, set(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_non_contig_name(PredTable, ModuleName, OptNum, Name, !PredIds, !Specs) :-
    SymName = qualified(ModuleName, Name),
    predicate_table_lookup_sym(PredTable, is_fully_qualified, SymName,
        PredIds),
    (
        PredIds = [],
        Pieces =
            [words("Error in the"), unth_fixed(OptNum),
            quote("--allow-non-contiguity-for"), words("option:"),
            words("the name")] ++ color_as_subject([quote(Name)]) ++
            [words("is")] ++ color_as_incorrect([words("unknown.")]) ++ [nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces),
        !:Specs = [Spec | !.Specs]
    ;
        PredIds = [PredId],
        set.insert(PredId, !PredIds)
    ;
        PredIds = [_, _ | _],
        Pieces =
            [words("Error in the"), unth_fixed(OptNum),
            quote("--allow-non-contiguity-for"), words("option:"),
            words("the name")] ++ color_as_subject([quote(Name)]) ++
            [words("is")] ++ color_as_incorrect([words("ambiguous.")]) ++ [nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%

generate_any_style_warnings(ModuleInfo, WarningsWeWant, !:Specs) :-
    WarningsWeWant = warnings_we_want(NonContigDecls, NonContigDefns,
        PredDeclDefnOrder),
    module_info_get_valid_pred_id_set(ModuleInfo, ValidPredIds),
    StyleInfo0 = style_info(NonContigDecls, NonContigDefns, PredDeclDefnOrder,
        ValidPredIds, [], [], [], map.init),
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.foldl(gather_style_info, PredIdTable, StyleInfo0, StyleInfo),
    StyleInfo = style_info(_, _, _, _, ExportedPreds, NonExportedPreds,
        ModeDeclItemNumberSpecs, ClauseGapMap0),
    !:Specs = ModeDeclItemNumberSpecs,
    (
        NonContigDefns = do_not_warn_non_contiguous_pred_defns
    ;
        NonContigDefns = warn_non_contiguous_pred_defns(_,
            AllowedNonContiguity),
        % First, process the sets of predicates and/or functions named by
        % --allow-non-contiguity-for options, effectively treating each set
        % as it were a single predicate.
        list.foldl2(report_non_contiguous_clauses_beyond_group(ModuleInfo),
            AllowedNonContiguity, ClauseGapMap0, ClauseGapMap, !Specs),
        % Then process all the predicates and functions that are not named
        % in any --allow-non-contiguity-for option.
        map.foldl(report_non_contiguous_clauses(ModuleInfo, []),
            ClauseGapMap, !Specs)
    ),
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
                style_clause_gaps           :: map(pred_id, regions_with_gaps)
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
        maybe_gather_clause_gap_info(PredId, ClauseItemNumbers, !StyleInfo),

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

:- pred maybe_gather_clause_gap_info(pred_id::in, clause_item_numbers::in,
    style_info::in, style_info::out) is det.

maybe_gather_clause_gap_info(PredId, ItemNumbers, !StyleInfo) :-
    NonContigDefns = !.StyleInfo ^ style_non_contig_defns,
    (
        NonContigDefns = do_not_warn_non_contiguous_pred_defns
    ;
        NonContigDefns = warn_non_contiguous_pred_defns(NumberTypes, _),
        ( if
            clauses_are_non_contiguous(ItemNumbers, NumberTypes,
                RegionsWithGaps)
            % XXX Currently, despite the existence of a field in ClausesInfo
            % to record whether PredInfo had a clause that is NOT in
            % ClausesInfo because it had a syntax error, we never set
            % that field to "some_clause_syntax_errors". If we ever do,
            % we may want to add a check for no_clause_syntax_errors here,
            % *if and only if* that malformed clause would cause a gap
            % in the item number sequence.
        then
            GapMap0 = !.StyleInfo ^ style_clause_gaps,
            map.det_insert(PredId, RegionsWithGaps, GapMap0, GapMap),
            !StyleInfo ^ style_clause_gaps := GapMap
        else
            true
        )
    ).

%---------------------%

:- pred report_non_contiguous_clauses_beyond_group(module_info::in,
    set(pred_id)::in,
    map(pred_id, regions_with_gaps)::in,
    map(pred_id, regions_with_gaps)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_non_contiguous_clauses_beyond_group(ModuleInfo, GroupPredIdSet,
        !ClauseGapMap, !Specs) :-
    set.foldl2(gather_regions_with_gaps, GroupPredIdSet,
        [], RegionsWithGapsList, !ClauseGapMap),
    MergedRegions = merge_regions_with_gaps(RegionsWithGapsList),
    (
        ( MergedRegions = []
        ; MergedRegions = [_]
        )
        % There are no gaps to report.
    ;
        MergedRegions = [FirstMergedRegion,
            SecondMergedRegion | LaterMergedRegions],
        RegionsWithGaps = regions_with_gaps(FirstMergedRegion,
            SecondMergedRegion, LaterMergedRegions),
        set.to_sorted_list(GroupPredIdSet, GroupPredIds),
        (
            GroupPredIds = []
        ;
            GroupPredIds = [HeadPredId | TailPredIds],
            report_non_contiguous_clauses(ModuleInfo, TailPredIds, HeadPredId,
                RegionsWithGaps, !Specs)
        )
    ).

:- pred gather_regions_with_gaps(pred_id::in,
    list(regions_with_gaps)::in, list(regions_with_gaps)::out,
    map(pred_id, regions_with_gaps)::in,
    map(pred_id, regions_with_gaps)::out) is det.

gather_regions_with_gaps(PredId, !RegionsWithGapsList, !ClauseGapMap) :-
    ( if map.remove(PredId, PredRegionsWithGaps, !ClauseGapMap) then
        !:RegionsWithGapsList = [PredRegionsWithGaps | !.RegionsWithGapsList]
    else
        true
    ).

%---------------------%

:- func merge_regions_with_gaps(list(regions_with_gaps))
    = list(clause_item_number_region).

merge_regions_with_gaps(RegionsWithGapsList) = MergedRegions :-
    RegionLists = list.map(regions_with_gaps_to_just_regions,
        RegionsWithGapsList),
    list.condense(RegionLists, Regions),
    list.sort(Regions, SortedRegions),
    (
        SortedRegions = [],
        MergedRegions = []
    ;
        SortedRegions = [HeadSortedRegion | TailSortedRegions],
        merge_adjacent_regions(HeadSortedRegion, TailSortedRegions,
            MergedRegions)
    ).

:- func regions_with_gaps_to_just_regions(regions_with_gaps)
    = list(clause_item_number_region).

regions_with_gaps_to_just_regions(RegionsWithGaps) = Regions :-
    RegionsWithGaps =
        regions_with_gaps(FirstRegion, SecondRegion, LaterRegions),
    Regions = [FirstRegion, SecondRegion | LaterRegions].

:- pred merge_adjacent_regions(
    clause_item_number_region::in,
    list(clause_item_number_region)::in,
    list(clause_item_number_region)::out) is det.

merge_adjacent_regions(CurRegion, [], [CurRegion]).
merge_adjacent_regions(CurRegion, [NextRegion | LaterRegions],
        MergedRegions) :-
    CurRegion = clause_item_number_region(CurLoItemNum, CurHiItemNum,
        CurLoCtxt, _CurHiCtxt),
    NextRegion = clause_item_number_region(NextLoItemNum, NextHiItemNum,
        _NextLoCtxt, NextHiCtxt),
    ( if CurHiItemNum + 1 = NextLoItemNum then
        CurNextRegion = clause_item_number_region(CurLoItemNum, NextHiItemNum,
            CurLoCtxt, NextHiCtxt),
        merge_adjacent_regions(CurNextRegion, LaterRegions, MergedRegions)
    else
        merge_adjacent_regions(NextRegion, LaterRegions, TailMergedRegions),
        MergedRegions = [CurRegion | TailMergedRegions]
    ).

%---------------------%

:- pred report_non_contiguous_clauses(module_info::in,
    list(pred_id)::in, pred_id::in, regions_with_gaps::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_non_contiguous_clauses(ModuleInfo, OtherPredIds, MainPredId,
        RegionsWithGaps, !Specs) :-
    AllPredIds = [MainPredId | OtherPredIds],
    (
        OtherPredIds = [],
        get_pred_context(ModuleInfo, MainPredId, FrontMsgContext),
        GapPredPieces = describe_one_pred_name(ModuleInfo, no,
            should_not_module_qualify, [], MainPredId)
    ;
        OtherPredIds = [_ | _],
        list.map(get_pred_context(ModuleInfo), AllPredIds, AllPredContexts),
        list.sort(AllPredContexts, SortedAllPredContexts),
        list.det_head(SortedAllPredContexts, FrontMsgContext),
        GapPredPieces = [words("the group")]
    ),
    AllPredsPieces = describe_several_pred_names(ModuleInfo,
        yes(color_subject), should_not_module_qualify, AllPredIds),
    FrontPieces = [words("Warning: the clauses for")] ++
        AllPredsPieces ++
        color_as_incorrect([words("are not contiguous.")]) ++ [nl],
    FrontMsg = msg(FrontMsgContext, FrontPieces),
    RegionsWithGaps =
        regions_with_gaps(FirstRegion, SecondRegion, LaterRegions),
    report_non_contiguous_clause_contexts(GapPredPieces, 1,
        FirstRegion, SecondRegion, LaterRegions, ContextMsgs),
    Msgs = [FrontMsg | ContextMsgs],
    Spec = error_spec($pred, severity_warning, phase_type_check, Msgs),
    !:Specs = [Spec | !.Specs].

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

:- pred get_pred_context(module_info::in, pred_id::in,
        prog_context::out) is det.

get_pred_context(ModuleInfo, PredId, Context) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_context(PredInfo, Context).

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
        EditParams = edit_params(CostDelete, CostInsert, CostReplace),
        construct_diff_for_string_seqs(EditParams, DeclStrs, DefnStrs,
            DiffPieces),
        Pieces = [words("Warning:")] ++
            color_as_subject([words("the order of"),
                words("the declarations and definitions")]) ++
            [words("of the"), words(ExportedOrNotStr), words("predicates"),
                words("is")] ++
            color_as_incorrect([words("inconsistent,")]) ++
            [words("as shown by this diff:"), nl,
            blank_line,
            fixed("--- declaration order"), nl,
            fixed("+++ definition order"), nl] ++
            DiffPieces,
        WarnSpec = spec($pred, severity_warning, phase_style,
            ModuleContext, Pieces),
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

%---------------------------------------------------------------------------%
:- end_module check_hlds.style_checks.
%---------------------------------------------------------------------------%
