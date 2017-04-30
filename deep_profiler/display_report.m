%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: display_report.m.
% Author: pbone.
%
% This module contains code to create a display data structure from a deep
% profiling report.
%
%---------------------------------------------------------------------------%

:- module display_report.
:- interface.

:- import_module display.
:- import_module profile.
:- import_module report.

% XXX: This include should be removed or replaced. Some data structures
% such as preferences are currently defined in query; they should be moved
% into a different module so that this module doesn't need to include
% the whole of query.
:- import_module query.

%---------------------------------------------------------------------------%

:- func report_to_display(deep, preferences, deep_report) = display.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module coverage.
:- import_module exclude.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module measurement_units.
:- import_module program_representation_utils.
:- import_module var_use_analysis.

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

report_to_display(Deep, Prefs, Report) = Display :-
    (
        Report = report_message(message_report(Msg)),
        Display = display(no, [display_heading(Msg)])
    ;
        Report = report_menu(MaybeMenuReport),
        (
            MaybeMenuReport = ok(MenuReport),
            display_report_menu(Deep, Prefs, MenuReport, Display)
        ;
            MaybeMenuReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_clique(MaybeCliqueReport),
        (
            MaybeCliqueReport = ok(CliqueReport),
            display_report_clique(Prefs, CliqueReport, Display)
        ;
            MaybeCliqueReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_clique_recursion_costs(MaybeCliqueRecursionReport),
        (
            MaybeCliqueRecursionReport = ok(CliqueRecursionReport),
            display_report_clique_recursion(Prefs, CliqueRecursionReport,
                Display)
        ;
            MaybeCliqueRecursionReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_recursion_types_frequency(MaybeRecTypesFreqReport),
        (
            MaybeRecTypesFreqReport = ok(RecTypesFreqReport),
            display_report_recursion_types_frequency(Prefs, RecTypesFreqReport,
                Display)
        ;
            MaybeRecTypesFreqReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_program_modules(MaybeProgramModulesReport),
        (
            MaybeProgramModulesReport = ok(ProgramModulesReport),
            display_report_program_modules(Prefs, ProgramModulesReport,
                Display)
        ;
            MaybeProgramModulesReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_module(MaybeModuleReport),
        (
            MaybeModuleReport = ok(ModuleReport),
            display_report_module(Prefs, ModuleReport, Display)
        ;
            MaybeModuleReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_module_getter_setters(MaybeModuleGetterSettersReport),
        (
            MaybeModuleGetterSettersReport = ok(ModuleGetterSettersReport),
            display_report_module_getter_setters(Prefs,
                ModuleGetterSettersReport, Display)
        ;
            MaybeModuleGetterSettersReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_module_rep(MaybeModuleRepReport),
        (
            MaybeModuleRepReport = ok(ModuleRepReport),
            display_report_module_rep(Prefs, ModuleRepReport, Display)
        ;
            MaybeModuleRepReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_top_procs(MaybeTopProcsReport),
        (
            MaybeTopProcsReport = ok(TopProcsReport),
            display_report_top_procs(Prefs, TopProcsReport, Display)
        ;
            MaybeTopProcsReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_proc(MaybeProcReport),
        (
            MaybeProcReport = ok(ProcReport),
            display_report_proc(Deep, Prefs, ProcReport, Display)
        ;
            MaybeProcReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_procrep_coverage(MaybeProcrepCoverageInfo),
        (
            MaybeProcrepCoverageInfo = ok(ProcrepCoverageInfo),
            display_report_procrep_coverage_info(Prefs, ProcrepCoverageInfo,
                Display)
        ;
            MaybeProcrepCoverageInfo = error(Msg),
            Display = display(no, [display_text(Msg)])
        )
    ;
        Report = report_proc_callers(MaybeProcCallersReport),
        (
            MaybeProcCallersReport = ok(ProcCallersReport),
            display_report_proc_callers(Deep, Prefs, ProcCallersReport,
                Display)
        ;
            MaybeProcCallersReport = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_proc_static_dump(MaybeProcStaticDumpInfo),
        (
            MaybeProcStaticDumpInfo = ok(ProcStaticDumpInfo),
            display_report_proc_static_dump(ProcStaticDumpInfo, Display)
        ;
            MaybeProcStaticDumpInfo = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_proc_dynamic_dump(MaybeProcDynamicDumpInfo),
        (
            MaybeProcDynamicDumpInfo = ok(ProcDynamicDumpInfo),
            display_report_proc_dynamic_dump(Deep, Prefs, ProcDynamicDumpInfo,
                Display)
        ;
            MaybeProcDynamicDumpInfo = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_call_site_static_dump(MaybeCallSiteStaticDumpInfo),
        (
            MaybeCallSiteStaticDumpInfo = ok(CallSiteStaticDumpInfo),
            display_report_call_site_static_dump(Prefs, CallSiteStaticDumpInfo,
                Display)
        ;
            MaybeCallSiteStaticDumpInfo = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_call_site_dynamic_dump(MaybeCallSiteDynamicDumpInfo),
        (
            MaybeCallSiteDynamicDumpInfo = ok(CallSiteDynamicDumpInfo),
            display_report_call_site_dynamic_dump(Prefs,
                CallSiteDynamicDumpInfo, Display)
        ;
            MaybeCallSiteDynamicDumpInfo = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_clique_dump(MaybeCliqueDumpInfo),
        (
            MaybeCliqueDumpInfo = ok(CliqueDumpInfo),
            display_report_clique_dump(Prefs, CliqueDumpInfo, Display)
        ;
            MaybeCliqueDumpInfo = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ;
        Report = report_call_site_dynamic_var_use(MaybeVarUseInfo),
        (
            MaybeVarUseInfo = ok(VarUseInfo),
            display_report_call_site_dynamic_var_use(Prefs, VarUseInfo,
                Display)
        ;
            MaybeVarUseInfo = error(Msg),
            Display = display(no, [display_heading(Msg)])
        )
    ).

%---------------------------------------------------------------------------%
%
% Code to display menu report.
%

:- pred display_report_menu(deep::in, preferences::in, menu_report::in,
    display::out) is det.

display_report_menu(Deep, Prefs, MenuReport, Display) :-
    MenuReport = menu_report(ProgramName, QuantaPerSec, UserQuanta, InstQuanta,
        NumCallseqs, NumCSD, NumCSS, NumPD, NumPS, NumClique),
    TotalQuanta = UserQuanta + InstQuanta,
    TotalTime = ticks_to_time(TotalQuanta, QuantaPerSec),

    ShouldDisplayTimes = should_display_times(Deep),

    % Display the links section of the report.
    ActionPrefs0 = Prefs,
    ActionPrefs1 = ActionPrefs0 ^ pref_inactive :=
        inactive_items(inactive_hide, inactive_hide, inactive_hide),
    ActionPrefs = ActionPrefs1 ^ pref_criteria :=
        by_cost(cost_time, self_and_desc, overall),

    LinksExploration = [
        link_base(deep_cmd_root(no), yes(ActionPrefs),
            "Exploring the call graph, starting at the root."),
        link_base(deep_cmd_root(yes(90)), yes(ActionPrefs),
            "Exploring the call graph, starting at the action."),
        link_base(deep_cmd_program_modules, yes(Prefs),
            "Exploring the program module by module.")
    ],

    (
        ShouldDisplayTimes = yes,

        Top100SelfCmd = deep_cmd_top_procs(rank_range(1, 100),
            cost_time, self, overall),
        Top100SelfAndDescCmd = deep_cmd_top_procs(rank_range(1, 100),
            cost_time, self_and_desc, overall),

        LinksTopProcsByLimitTime = [
            link_base(Top100SelfCmd, yes(Prefs),
                "Top 100 most expensive procedures: time, self."),
            link_base(Top100SelfAndDescCmd, yes(Prefs),
                "Top 100 most expensive procedures: time, self+descendants.")
        ]
    ;
        ShouldDisplayTimes = no,
        LinksTopProcsByLimitTime = []
    ),

    TopLimitCallSeqsSelf = deep_cmd_top_procs(rank_range(1, 100),
        cost_callseqs, self, overall),
    TopLimitCallSeqsSelfAndDesc = deep_cmd_top_procs(rank_range(1, 100),
        cost_callseqs, self_and_desc, overall),
    TopLimitWordsSelf = deep_cmd_top_procs(rank_range(1, 100),
        cost_words, self, overall),
    TopLimitWordsSelfAndDesc = deep_cmd_top_procs(rank_range(1, 100),
        cost_words, self_and_desc, overall),

    LinksTopProcsByLimit = [
        link_base(TopLimitCallSeqsSelf, yes(Prefs),
            "Top 100 most expensive procedures: callseqs, self."),
        link_base(TopLimitCallSeqsSelfAndDesc, yes(Prefs),
            "Top 100 most expensive procedures: callseqs, self+descendants."),
        link_base(TopLimitWordsSelf, yes(Prefs),
            "Top 100 most expensive procedures: words, self."),
        link_base(TopLimitWordsSelfAndDesc, yes(Prefs),
            "Top 100 most expensive procedures: words, self+descendants.")
    ],

    (
        ShouldDisplayTimes = yes,
        TimeAbove01PercentCmd = deep_cmd_top_procs(threshold_percent(0.1),
            cost_time, self, overall),
        TimeAbove1PercentCmd = deep_cmd_top_procs(threshold_percent(1.0),
            cost_time, self, overall),
        TimeAbove1SecondCmd = deep_cmd_top_procs(threshold_value(100.0),
            cost_time, self_and_desc, overall),

        LinksTopProcsByPercentTime = [
            link_base(TimeAbove01PercentCmd, yes(Prefs),
                "Procedures above 0.1% threshold: time, self."),
            link_base(TimeAbove1PercentCmd, yes(Prefs),
                "Procedures above 1% threshold: time, self+descendants."),
            link_base(TimeAbove1SecondCmd, yes(Prefs),
                "Procedures above 1 second threshold: time, self+descendants.")
        ]
    ;
        ShouldDisplayTimes = no,
        LinksTopProcsByPercentTime = []
    ),

    CallSeqsAbove01PercentCmd = deep_cmd_top_procs(threshold_percent(0.1),
        cost_callseqs, self, overall),
    CallSeqsAbove1PercentCmd = deep_cmd_top_procs(threshold_percent(1.0),
        cost_callseqs, self_and_desc, overall),
    CallSeqsAboveMillionCmd = deep_cmd_top_procs(threshold_value(1000000.0),
        cost_callseqs, self_and_desc, overall),
    WordsAbove01PercentCmd = deep_cmd_top_procs(threshold_percent(0.1),
        cost_words, self, overall),
    WordsAbove1PercentCmd = deep_cmd_top_procs(threshold_percent(1.0),
        cost_words, self_and_desc, overall),
    % 2M words is 8MB on ia32.
    WordsAbove2Megawords = deep_cmd_top_procs(
        threshold_value(float(1024 * 1024 * 2)),
        cost_words, self_and_desc, overall),

    LinksTopProcsByPercent = [
        link_base(CallSeqsAbove01PercentCmd, yes(Prefs),
            "Procedures above 0.1% threshold: callseqs, self."),
        link_base(CallSeqsAbove1PercentCmd, yes(Prefs),
            "Procedures above 1% threshold: callseqs, self+descendants."),
        link_base(CallSeqsAboveMillionCmd, yes(Prefs),
            ("Procedures above 1,000,000 callseqs threshold: callseqs, " ++
                "self+descendants.")),
        link_base(WordsAbove01PercentCmd, yes(Prefs),
            "Procedures above 0.1% threshold: words, self."),
        link_base(WordsAbove1PercentCmd, yes(Prefs),
            "Procedures above 1% threshold: words, self+descendants."),
        link_base(WordsAbove2Megawords, yes(Prefs),
            "Procedures above 2M words threshold: words, self+descendants.")
    ],

    LinkCmds = LinksExploration ++
        LinksTopProcsByLimitTime ++ LinksTopProcsByLimit ++
        LinksTopProcsByPercentTime ++ LinksTopProcsByPercent,
    list.map(make_link, LinkCmds, LinksList),
    Links = display_list(list_class_vertical_bullets,
        yes("You can start exploring the deep profile at the following" ++
            " points."), LinksList),

    %
    % Produce the developer-only options list.
    %
    RecursionTypeFrequenciesCmd = deep_cmd_recursion_types_frequency,

    LinksDeveloperCmds = [
        link_base(RecursionTypeFrequenciesCmd, yes(Prefs),
            "Frequencies of different types of recursion used in the program.")
    ],
    list.map(make_link, LinksDeveloperCmds, DeveloperLinksList),
    DeveloperLinks = display_developer(
        display_list(list_class_vertical_bullets,
            yes("Options that are only useful to Mercury developers"),
            DeveloperLinksList)),

    % Display the table section of the report.
    ProfilingStatistics =
        [("Profile generated for:"      - td_s(ProgramName)),
        ("Quanta per second:"           - td_i(QuantaPerSec)),
        ("Quanta in user code:"         - td_i(UserQuanta)),
        ("Quanta in instrumentation:"   - td_i(InstQuanta)),
        ("Total quanta:"                - td_i(TotalQuanta)),
        ("Total time:"                  - td_t(TotalTime)),
        ("Call sequence numbers:"       - td_i(NumCallseqs)),
        ("CallSiteDynamic structures:"  - td_i(NumCSD)),
        ("ProcDynamic structures:"      - td_i(NumPD)),
        ("CallSiteStatic structures:"   - td_i(NumCSS)),
        ("ProcStatic structures:"       - td_i(NumPS)),
        ("Cliques:"                     - td_i(NumClique))],

    Rows = list.map(make_labelled_table_row, ProfilingStatistics),
    Table = table(table_class_do_not_box, 2, no, Rows),

    OptionsControls = general_options_controls(deep_cmd_menu, Prefs),

    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),

    % Construct the complete representation of what to display.
    Display = display(yes("Deep profiler menu"),
        [Links, DeveloperLinks, display_table(Table),
        display_paragraph_break, MenuRestartQuitControls,
        display_paragraph_break, OptionsControls]).

%---------------------------------------------------------------------------%
%
% Code to display a clique report.
%

    % Create a display_report structure for a clique report.
    %
:- pred display_report_clique(preferences::in, clique_report::in,
    display::out) is det.

display_report_clique(Prefs, CliqueReport, Display) :-
    CliqueReport = clique_report(CliquePtr, InnerToOuterAncestorCallSites0,
        CliqueProcs0),
    Cmd = deep_cmd_clique(CliquePtr),
    CliquePtr = clique_ptr(CliqueNum),
    Title = string.format("Clique %d:", [i(CliqueNum)]),

    % Build the table of all modules.
    SortByContextPrefs = Prefs ^ pref_criteria := by_context,
    SortByNamePrefs = Prefs ^ pref_criteria := by_name,
    SourceHeaderCell = td_l(deep_link(Cmd, yes(SortByContextPrefs),
        attr_str([], "Source"), link_class_link)),
    ProcHeaderCell = td_l(deep_link(Cmd, yes(SortByNamePrefs),
        attr_str([], "Procedure"), link_class_link)),
    SourceHeaderGroup =
        make_single_table_header_group(SourceHeaderCell,
            table_column_class_source_context, column_do_not_colour),
    ProcHeaderGroup =
        make_single_table_header_group(ProcHeaderCell,
            table_column_class_proc, column_do_not_colour),
    MakeHeaderData = override_order_criteria_header_data(Cmd),
    perf_table_header(total_columns_meaningful, Prefs, MakeHeaderData,
        PerfHeaderGroups),
    AllHeaderGroups = [SourceHeaderGroup, ProcHeaderGroup] ++ PerfHeaderGroups,
    header_groups_to_header(AllHeaderGroups, NumColumns, Header),

    list.length(InnerToOuterAncestorCallSites0, NumAncestors),
    MaybeAncestorLimit = Prefs ^ pref_anc,
    ( if
        MaybeAncestorLimit = yes(AncestorLimit),
        NumAncestors > AncestorLimit
    then
        AncestorTitle = string.format("The %d closest ancestor call sites:",
            [i(AncestorLimit)]),
        list.take_upto(AncestorLimit,
            InnerToOuterAncestorCallSites0, InnerToOuterAncestorCallSites)
    else
        AncestorTitle = "Ancestor call sites:",
        InnerToOuterAncestorCallSites = InnerToOuterAncestorCallSites0
    ),

    list.reverse(InnerToOuterAncestorCallSites, AncestorCallSites),
    ModuleQual = Prefs ^ pref_module_qual,
    CliqueModuleNames = list.map(clique_proc_report_module_name, CliqueProcs0),
    ( if
        CliqueModuleNames = [FirstModuleName | _],
        list.all_same(CliqueModuleNames)
    then
        MaybeCurModuleName = yes(FirstModuleName)
    else
        MaybeCurModuleName = no
    ),
    AncestorDataRows = list.map(
        clique_ancestor_to_row(MaybeCurModuleName, ModuleQual, Prefs),
        AncestorCallSites),
    AncestorSectionHeaderRow = table_section_header(td_s(AncestorTitle)),
    AncestorRows = [AncestorSectionHeaderRow, table_separator_row] ++
        AncestorDataRows,

    list.length(CliqueProcs0, NumCliqueprocs),
    ( if NumCliqueprocs > 1 then
        ProcsTitle = "Procedures of the clique:"
    else
        ProcsTitle = "The clique has one procedure:"
    ),
    CliqueProcsHeaderRow = table_section_header(td_s(ProcsTitle)),

    sort_clique_procs_by_preferences(MaybeCurModuleName, ModuleQual, Prefs,
        CliqueProcs0, CliqueProcs),
    ProcRowLists0 = list.map(
        clique_proc_to_table_rows(MaybeCurModuleName, ModuleQual, Prefs,
            CliquePtr),
        CliqueProcs),
    ProcRowLists = list.map(add_front_separator_row, ProcRowLists0),
    list.condense(ProcRowLists, ProcRows),

    AllRows = AncestorRows ++
        [table_separator_row, CliqueProcsHeaderRow] ++ ProcRows,
    Table = table(table_class_box_if_pref, NumColumns, yes(Header), AllRows),
    DisplayTable = display_table(Table),

    % Build controls at the bottom of the page.
    AncestorControls = ancestor_controls(Prefs, Cmd),
    InactiveCallSiteControls = inactive_call_site_controls(Prefs, Cmd),
    ModuleQualControls = module_qual_controls(Prefs, Cmd),
    FieldControls = field_controls(Prefs, Cmd),
    FormatControls = format_controls(Prefs, Cmd),
    CliqueReportControls = clique_reports_controls(Prefs, CliquePtr, Cmd),
    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),

    Display = display(yes(Title),
        [DisplayTable,
        display_paragraph_break, AncestorControls,
        display_paragraph_break, InactiveCallSiteControls,
        display_paragraph_break, ModuleQualControls,
        display_paragraph_break, FieldControls,
        display_paragraph_break, FormatControls,
        display_paragraph_break, CliqueReportControls,
        display_paragraph_break, MenuRestartQuitControls]).

:- func clique_proc_report_module_name(clique_proc_report) = string.

clique_proc_report_module_name(CliqueProc) =
    CliqueProc ^ cpr_proc_summary ^ perf_row_subject ^ pdesc_module_name.

:- func add_front_separator_row(list(table_row)) = list(table_row).

add_front_separator_row(Rows) = [table_separator_row | Rows].

:- func clique_ancestor_to_row(maybe(string), module_qual, preferences,
    perf_row_data(ancestor_desc)) = table_row.

clique_ancestor_to_row(MaybeCurModuleName, ModuleQual, Prefs, AncestorRowData)
        = Row :-
    AncestorDesc = AncestorRowData ^ perf_row_subject,
    CallSiteDesc = AncestorDesc ^ ad_call_site_desc,
    SourceCell = call_site_desc_source_cell(CallSiteDesc),
    CliqueProcCell = call_site_desc_clique_proc_cell(MaybeCurModuleName,
        ModuleQual, Prefs, AncestorDesc),
    Fields = Prefs ^ pref_fields,
    perf_table_row(total_columns_meaningful, Fields, AncestorRowData,
        PerfCells),
    AllCells = [SourceCell, CliqueProcCell] ++ PerfCells,
    Row = table_row(AllCells).

:- func clique_proc_to_table_rows(maybe(string), module_qual, preferences,
    clique_ptr, clique_proc_report) = list(table_row).

clique_proc_to_table_rows(MaybeCurModuleName, ModuleQual, Prefs, CliquePtr,
        CliqueProcReport) = ProcRows :-
    CliqueProcReport = clique_proc_report(SummaryRowData,
        FirstPDReport, LaterPDReports),
    (
        LaterPDReports = [],
        ProcRows = clique_proc_dynamic_to_table_rows(MaybeCurModuleName,
            ModuleQual, Prefs, CliquePtr, FirstPDReport)
    ;
        LaterPDReports = [_ | _],
        AllPDReports = [FirstPDReport | LaterPDReports],
        sort_clique_proc_dynamics_by_preferences(Prefs, AllPDReports,
            SortedAllPDReports),
        PDProcRowLists =
            list.map(
                clique_proc_dynamic_to_table_rows(MaybeCurModuleName,
                    ModuleQual, Prefs, CliquePtr),
                SortedAllPDReports),
        % Do we want separators between the rows of different proc dynamics?
        list.condense(PDProcRowLists, PDProcRows),
        Fields = Prefs ^ pref_fields,
        perf_table_row(total_columns_meaningful, Fields, SummaryRowData,
            SummaryPerfCells),
        ProcDesc = SummaryRowData ^ perf_row_subject,
        SourceCell = proc_desc_to_source_cell(ProcDesc),
        ProcCell = proc_desc_to_prefix_proc_name_cell(no,
            ModuleQual, Prefs, [attr_bold], ProcDesc, "summary "),
        SummaryRowCells = [SourceCell, ProcCell] ++ SummaryPerfCells,
        SummaryRow = table_row(SummaryRowCells),
        ProcRows = [SummaryRow, table_separator_row] ++ PDProcRows
    ).

:- func clique_proc_dynamic_to_table_rows(maybe(string), module_qual,
    preferences, clique_ptr, clique_proc_dynamic_report) = list(table_row).

clique_proc_dynamic_to_table_rows(MaybeCurModuleName, ModuleQual,
        Prefs, CliquePtr, CliqueProcDynamicReport) = ProcRows :-
    CliqueProcDynamicReport = clique_proc_dynamic_report(SummaryRowData,
        CallSiteReports0),
    ProcDesc = SummaryRowData ^ perf_row_subject,
    ProcCell = proc_desc_to_proc_name_cell_span(no, ModuleQual, Prefs,
        [attr_bold], ProcDesc, 2),
    Fields = Prefs ^ pref_fields,
    perf_table_row(total_columns_meaningful, Fields, SummaryRowData,
        SummaryPerfCells),
    SummaryRowCells = [ProcCell] ++ SummaryPerfCells,
    SummaryRow = table_row(SummaryRowCells),
    sort_clique_call_site_reports_by_preferences(MaybeCurModuleName,
        ModuleQual, Prefs, CallSiteReports0, CallSiteReports),
    CallSiteRowLists =
        list.map(
            clique_call_site_to_rows(MaybeCurModuleName, ModuleQual, Prefs,
                CliquePtr),
        CallSiteReports),
    list.condense(CallSiteRowLists, CallSiteRows),
    ProcRows = [SummaryRow, table_separator_row] ++ CallSiteRows.

:- func clique_call_site_to_rows(maybe(string), module_qual, preferences,
    clique_ptr, clique_call_site_report) = list(table_row).

clique_call_site_to_rows(MaybeCurModuleName, ModuleQual, Prefs,
        CliquePtr, CallSiteReport) = Rows :-
    CallSiteReport = clique_call_site_report(CallSiteRowData, Kind,
        CalleePerfs),
    CallSiteDesc = CallSiteRowData ^ perf_row_subject,
    SourceCell = call_site_desc_source_cell(CallSiteDesc),
    Fields = Prefs ^ pref_fields,
    perf_table_row(total_columns_meaningful, Fields, CallSiteRowData,
        SummaryPerfCells),
    (
        Kind = normal_call_and_callee(CalleeProcDesc, _TypeSubst),
        (
            CalleePerfs = [],
            Inactive = Prefs ^ pref_inactive,
            Inactive = inactive_items(InactiveCallSites, _, _),
            (
                InactiveCallSites = inactive_show,
                CalleeProcCell = proc_desc_to_prefix_proc_name_cell(
                    MaybeCurModuleName, ModuleQual, Prefs,
                    [], CalleeProcDesc, "no calls made to "),
                RowCells = [SourceCell, CalleeProcCell] ++ SummaryPerfCells,
                Row = table_row(RowCells),
                Rows = [Row]
            ;
                InactiveCallSites = inactive_hide,
                Rows = []
            )
        ;
            CalleePerfs = [CalleePerf],
            CalleeCliqueDesc = CalleePerf ^ perf_row_subject,
            CalleeProcCell = clique_desc_to_non_self_link_proc_name_cell(
                MaybeCurModuleName, ModuleQual, Prefs,
                CalleeCliqueDesc, CliquePtr),
            RowCells = [SourceCell, CalleeProcCell] ++ SummaryPerfCells,
            Row = table_row(RowCells),
            Rows = [Row]
        ;
            CalleePerfs = [_, _ | _],
            unexpected($module, $pred, "more than one callee at normal")
        )
    ;
        (
            Kind = special_call_and_no_callee,
            CalleeCellStr0 = "special call"
        ;
            Kind = higher_order_call_and_no_callee,
            CalleeCellStr0 = "higher order call"
        ;
            Kind = method_call_and_no_callee,
            CalleeCellStr0 = "method call"
        ;
            Kind = callback_and_no_callee,
            CalleeCellStr0 = "callback"
        ),
        (
            CalleePerfs = [],
            CalleeCellStr = CalleeCellStr0 ++ " (no calls made)"
        ;
            CalleePerfs = [_ | _],
            CalleeCellStr = CalleeCellStr0 ++ " (summary)"
        ),
        CalleeCell = table_cell(td_s(CalleeCellStr)),
        SummaryRowCells = [SourceCell, CalleeCell] ++ SummaryPerfCells,
        SummaryRow = table_row(SummaryRowCells),
        Summarize = Prefs ^ pref_summarize,
        (
            Summarize = summarize_ho_call_sites,
            Rows = [SummaryRow]
        ;
            Summarize = do_not_summarize_ho_call_sites,
            sort_clique_rows_by_preferences(Prefs, CalleePerfs,
                SortedCalleePerfs),
            CalleeRows =
                list.map(
                    clique_call_site_callee_to_row(MaybeCurModuleName,
                        ModuleQual, Prefs, CliquePtr),
                    SortedCalleePerfs),
            Rows = [SummaryRow] ++ CalleeRows
        )
    ).

:- func clique_call_site_callee_to_row(maybe(string), module_qual,
    preferences, clique_ptr, perf_row_data(clique_desc)) = table_row.

clique_call_site_callee_to_row(MaybeCurModuleName, ModuleQual, Prefs,
        CliquePtr, CalleeRowData) = Row :-
    CalleeCliqueDesc = CalleeRowData ^ perf_row_subject,
    CalleeProcCell = clique_desc_to_non_self_link_proc_name_cell(
        MaybeCurModuleName, ModuleQual, Prefs, CalleeCliqueDesc, CliquePtr),
    Fields = Prefs ^ pref_fields,
    perf_table_row(total_columns_meaningful, Fields, CalleeRowData, PerfCells),
    EmptyCell = table_cell(td_s("")),
    Cells = [EmptyCell, CalleeProcCell] ++ PerfCells,
    Row = table_row(Cells).

%---------------------------------------------------------------------------%
%
% Code to display a clique recursion report.
%

:- pred display_report_clique_recursion(preferences::in,
    clique_recursion_report::in, display::out) is det.

display_report_clique_recursion(Prefs, CliqueRecursionReport, Display) :-
    CliqueRecursionReport = clique_recursion_report(CliquePtr, RecursionType,
        _NumProcs),
    Cmd = deep_cmd_clique_recursive_costs(CliquePtr),
    CliquePtr = clique_ptr(CliqueNum),
    Title = string.format("The recursion information for clique %d:",
        [i(CliqueNum)]),

    display_recursion_type(RecursionType, DisplayRecursionType),

    CliqueReportsControls = clique_reports_controls(Prefs, CliquePtr, Cmd),

    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),
    Display = display(yes(Title), DisplayRecursionType ++
        [display_paragraph_break, CliqueReportsControls,
         display_paragraph_break, MenuRestartQuitControls]).

:- pred display_recursion_type(recursion_type::in, list(display_item)::out)
    is det.

display_recursion_type(RecursionType, Items) :-
    (
        (
            RecursionType = rt_not_recursive,
            Text = "Clique is non-recursive"
        ;
            RecursionType = rt_mutual_recursion(NumProcs),
            Text = format("Mutual recursion between %d procedures",
                [i(NumProcs)])
        ),
        Items = [display_text(Text)]
    ;
        RecursionType = rt_errors(Errors),
        ErrorItems = map((func(Text) = display_text(Text)), Errors),
        Items = [display_list(list_class_vertical_no_bullets,
            yes("Unknown, error(s) occured"), ErrorItems)]
    ;
        (
            RecursionType = rt_single(BaseLevel, RecLevel, AvgDepth,
                AvgRecCost, AnyRecCost),
            RowData = [
                {"Base case", BaseLevel},
                {"Recursive case", RecLevel}],
            Text = "Single-recursion:",

            MaxDepthI = round_to_int(AvgDepth),
            ExtraTableRows0 =
                [{"Average recursion depth:", AvgDepth},
                 {"Average recursive call cost (excluding the call itself):",
                    AvgRecCost}] ++
                map(
                    (func(Level) =
                        {string.format("Cost at depth %d:", [i(Level)]),
                            AnyRecCost(Level)}),
                    [0, 1, 2, round_to_int(AvgDepth / 2.0),
                     MaxDepthI - 2, MaxDepthI - 1, MaxDepthI]),
            ExtraTableRows = map((func({Label, Value}) = table_row(
                    [table_cell(td_s(Label)), table_cell(td_f(Value))])),
                ExtraTableRows0)
        ;
            RecursionType = rt_divide_and_conquer(BaseLevel, RecLevel),
            RowData = [
                {"Base case", BaseLevel},
                {"Doubly-recursive case", RecLevel}],
            Text = "Double-recursion (Probably Divide and Conquer):",

            ExtraTableRows = []
        ;
            RecursionType = rt_other(Levels),
            RowData = map((func(Level) = {Label, Level} :-
                    Label = string.format("Case for %d recursive calls",
                        [i(Level ^ rlr_level)])
                ), Levels),
            Text = "Unknown recursion type:",

            ExtraTableRows = []
        ),
        Rows = map(make_recursion_table_row, RowData),

        ExtraTable = display_table(table(table_class_do_not_box, 2, no,
            ExtraTableRows)),

        Header = table_header(map(
            (func({Name, Class}) =
                table_header_group(table_header_group_single(td_s(Name)),
                    Class, column_do_not_colour)
            ),
            [{"Recursion type", table_column_class_field_name},
             {"Exec count", table_column_class_number},
             {"Non recursive calls per-call cost",
                table_column_class_callseqs},
             {"Recursive calls per-call cost (ex children)",
                table_column_class_callseqs}])),
        Table = display_table(table(table_class_box_if_pref, 4, yes(Header),
            Rows)),
        Description = display_text(Text),
        Items = [Description,
            display_paragraph_break, Table,
            display_paragraph_break, ExtraTable]
    ).

:- func make_recursion_table_row({string, recursion_level_report}) = table_row.

make_recursion_table_row({Label, Report}) =
    table_row([table_cell(td_s(Label)),
        table_cell(td_i(Report ^ rlr_calls)),
        table_cell(td_f(Report ^ rlr_non_rec_calls_cost)),
        table_cell(td_f(Report ^ rlr_rec_calls_ex_chld_cost))]).

:- pred display_report_recursion_types_frequency(preferences::in,
    recursion_types_frequency_report::in, display::out) is det.

display_report_recursion_types_frequency(Prefs, Report, Display) :-
    Cmd = deep_cmd_recursion_types_frequency,
    Report = recursion_types_frequency_report(Histogram0),

    Title = "Frequencies of recognized recursion types",

    % Build the table.
    RecursionTypeLink = deep_link(Cmd,
        yes(Prefs ^ pref_criteria := by_context),
        attr_str([], "Recursion Type"), link_class_link),
    RecursionTypeHeaderGroup = make_single_table_header_group(
        td_l(RecursionTypeLink), table_column_class_no_class,
        column_do_not_colour),
    FreqHeaderGroup = make_single_table_header_group(
        td_s("Frequency"), table_column_class_no_class,
        column_do_not_colour),
    PercentageHeaderGroup = make_single_table_header_group(
        td_s("Percentage"), table_column_class_no_class,
        column_do_not_colour),
    perf_table_header(total_columns_meaningful, Prefs,
        override_order_criteria_header_data(Cmd), PerfHeaderGroups),
    AllHeaderGroups = [RecursionTypeHeaderGroup, FreqHeaderGroup,
        PercentageHeaderGroup] ++ PerfHeaderGroups,
    header_groups_to_header(AllHeaderGroups, NumColumns, Header),

    Histogram1 = map.to_assoc_list(Histogram0),
    sort_recursion_types_by_preferences(Prefs, Histogram1, Histogram),
    list.map(display_report_rec_type_freq_rows(Prefs, NumColumns),
        Histogram, Rowss),
    list.condense(Rowss, Rows),
    RecursionTypesTable = display_table(table(table_class_box_if_pref,
        NumColumns, yes(Header), Rows)),

    ModuleQualControls = module_qual_controls(Prefs, Cmd),
    FieldControls = field_controls(Prefs, Cmd),
    FormatControls = format_controls(Prefs, Cmd),
    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),

    Display = display(yes(Title), [RecursionTypesTable,
        display_paragraph_break, ModuleQualControls,
        display_paragraph_break, FieldControls,
        display_paragraph_break, FormatControls,
        display_paragraph_break, MenuRestartQuitControls]).

:- pred sort_recursion_types_by_preferences(preferences::in,
    assoc_list(recursion_type_simple, recursion_type_freq_data)::in,
    assoc_list(recursion_type_simple, recursion_type_freq_data)::out) is det.

sort_recursion_types_by_preferences(Prefs, !RecursionTypes) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        ( OrderCriteria = by_context
        ; OrderCriteria = by_name
        ),
        % Sort by the type of recursion.
        list.sort(compare_recursion_type_row_by_rec_type, !RecursionTypes)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(
            compare_rec_type_row_datas_by_cost(CostKind, InclDesc, Scope),
            !RecursionTypes),
        % We want the most expensive rows to appear first.
        list.reverse(!RecursionTypes)
    ).

:- pred sort_recursion_type_procs_by_preferences(preferences::in,
    list(recursion_type_proc_freq_data)::in,
    list(recursion_type_proc_freq_data)::out) is det.

sort_recursion_type_procs_by_preferences(Prefs, !RecursionTypeProcs) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        ( OrderCriteria = by_context
        ; OrderCriteria = by_name
        ),
        % Since in this case we sort recursion type rows by their recursion
        % type It makes sense to sort the procs within each type by their
        % frequency.
        sort(compare_recursion_proc_row_by_frequency_rev, !RecursionTypeProcs)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        sort(compare_rec_proc_row_datas_by_cost(CostKind, InclDesc, Scope),
            !RecursionTypeProcs),

        % We want the most expensive rows to appear first.
        reverse(!RecursionTypeProcs)
    ).

:- pred compare_recursion_type_row_by_rec_type(
    pair(recursion_type_simple, T)::in,
    pair(recursion_type_simple, T)::in, comparison_result::out) is det.

compare_recursion_type_row_by_rec_type(RTA - _, RTB - _, Result) :-
    compare(Result, RTA, RTB).

:- pred compare_recursion_proc_row_by_frequency_rev(
    recursion_type_proc_freq_data::in, recursion_type_proc_freq_data::in,
    comparison_result::out) is det.

compare_recursion_proc_row_by_frequency_rev(
        recursion_type_proc_freq_data(FreqA, _, _),
        recursion_type_proc_freq_data(FreqB, _, _), Result) :-
    % The reverse is so that entries with larger frequencies are listed first.
    compare(Result, FreqB, FreqA).

:- pred compare_rec_type_row_datas_by_cost(cost_kind::in,
    include_descendants::in, measurement_scope::in,
    pair(T, recursion_type_freq_data)::in,
    pair(T, recursion_type_freq_data)::in,
    comparison_result::out) is det.

compare_rec_type_row_datas_by_cost(CostKind, IncDesc, Scope, _ - DataA,
        _ - DataB, Result) :-
    MaybePerfA = DataA ^ rtfd_maybe_summary,
    MaybePerfB = DataB ^ rtfd_maybe_summary,
    (
        MaybePerfA = yes(PerfA),
        MaybePerfB = yes(PerfB),
        compare_perf_row_datas_by_cost(CostKind, IncDesc, Scope,
            PerfA, PerfB, Result)
    ;
        MaybePerfA = yes(_),
        MaybePerfB = no,
        Result = (>)
    ;
        MaybePerfA = no,
        MaybePerfB = yes(_),
        Result = (=)
    ;
        MaybePerfA = no,
        MaybePerfB = no,
        Result = (=)
    ).

:- pred compare_rec_proc_row_datas_by_cost(cost_kind::in,
    include_descendants::in, measurement_scope::in,
    recursion_type_proc_freq_data::in, recursion_type_proc_freq_data::in,
    comparison_result::out) is det.

compare_rec_proc_row_datas_by_cost(CostKind, InclDesc, Scope,
        recursion_type_proc_freq_data(_, _, PerfA),
        recursion_type_proc_freq_data(_, _, PerfB), Result) :-
    compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope, PerfA, PerfB,
        Result).

:- pred display_report_rec_type_freq_rows(preferences::in, int::in,
    pair(recursion_type_simple, recursion_type_freq_data)::in,
    list(table_row)::out) is det.

display_report_rec_type_freq_rows(Prefs, NumColumns, Type - FreqData, Rows) :-
    FreqData = recursion_type_freq_data(Frequency, Percent, MaybeSummary,
        ProcsMap),

    % Make the header row.
    display_report_recursion_type_simple(Type, TypeStr),
    HeaderRow = table_section_header(td_s(TypeStr)),

    % Make the row summarising the recursion type.
    (
        MaybeSummary = yes(Summary),
        perf_table_row(total_columns_meaningful, Prefs ^ pref_fields, Summary,
            SummaryCells)
    ;
        MaybeSummary = no,
        duplicate(NumColumns - 3, table_empty_cell, SummaryCells)
    ),
    Row = table_row([table_cell(td_s("Totals")), table_cell(td_i(Frequency)),
        table_cell(td_p(Percent))] ++ SummaryCells),

    % Make rows for the top proc statics.
    Procs0 = values(ProcsMap),
    sort_recursion_type_procs_by_preferences(Prefs, Procs0, Procs1),
    take_upto(Prefs ^ pref_proc_statics_per_rec_type, Procs1, Procs),
    map(display_report_rec_type_proc_rows(Prefs), Procs, ProcRows),

    Rows = [HeaderRow | [Row | ProcRows ++
        [table_separator_row]]].

:- pred display_report_rec_type_proc_rows(preferences::in,
    recursion_type_proc_freq_data::in, table_row::out) is det.

display_report_rec_type_proc_rows(Prefs,
        recursion_type_proc_freq_data(Freq, Percent, Summary), Row) :-
    perf_table_row(total_columns_meaningful, Prefs ^ pref_fields, Summary,
        SummaryCells),
    ProcDesc = Summary ^ perf_row_subject,
    ProcCell = proc_desc_to_proc_name_cell(no, Prefs ^ pref_module_qual, Prefs,
        ProcDesc),
    Row = table_row([ProcCell, table_cell(td_i(Freq)),
        table_cell(td_p(Percent))] ++ SummaryCells).

:- pred display_report_recursion_type_simple(recursion_type_simple::in,
    string::out) is det.

display_report_recursion_type_simple(rts_not_recursive, "Not recursive").
display_report_recursion_type_simple(rts_single, "Single-recursion").
display_report_recursion_type_simple(rts_divide_and_conquer,
        "Divide and conquer").
display_report_recursion_type_simple(rts_mutual_recursion(NumProcs), String) :-
    format("Mutual recursion between %d procs", [i(NumProcs)], String).
display_report_recursion_type_simple(rts_other(Levels), String) :-
    LevelsStr = join_list(", ", map(string, set.to_sorted_list(Levels))),
    format("Other recursion with levels: %s", [s(LevelsStr)], String).
display_report_recursion_type_simple(rts_total_error_instances,
        "Total errors").
display_report_recursion_type_simple(rts_error(Error), "Error: " ++ Error).

%---------------------------------------------------------------------------%
%
% Code to display a program_modules report.
%

    % Create a display_report structure for a program_modules report.
    %
:- pred display_report_program_modules(preferences::in,
    program_modules_report::in, display::out) is det.

display_report_program_modules(Prefs, ProgramModulesReport, Display) :-
    ProgramModulesReport = program_modules_report(ModuleRowDatas0),
    Cmd = deep_cmd_program_modules,
    Title = "The modules of the program:",

    ShowInactiveModules = Prefs ^ pref_inactive ^ inactive_modules,
    (
        ShowInactiveModules = inactive_show,
        ModuleRowDatas1 = ModuleRowDatas0
    ;
        ShowInactiveModules = inactive_hide,
        list.filter(active_module, ModuleRowDatas0, ModuleRowDatas1)
    ),

    SortPrefs = avoid_sort_self_and_desc(Prefs),
    sort_module_active_rows_by_preferences(SortPrefs,
        ModuleRowDatas1, ModuleRowDatas),

    % Build the table of all modules.
    SortByNamePrefs = Prefs ^ pref_criteria := by_name,
    ModuleHeaderCell = td_l(deep_link(Cmd, yes(SortByNamePrefs),
        attr_str([], "Module"), link_class_link)),
    RankHeaderGroup =
        make_single_table_header_group(td_s("Rank"),
            table_column_class_ordinal_rank, column_do_not_colour),
    ModuleHeaderGroup =
        make_single_table_header_group(ModuleHeaderCell,
            table_column_class_module_name, column_do_not_colour),
    MakeHeaderData = override_order_criteria_header_data(Cmd),
    perf_table_header(total_columns_not_meaningful, Prefs, MakeHeaderData,
        PerfHeaderGroups),
    AllHeaderGroups =
        [RankHeaderGroup, ModuleHeaderGroup] ++ PerfHeaderGroups,
    header_groups_to_header(AllHeaderGroups, NumColumns, Header),

    list.map_foldl(
        maybe_ranked_subject_perf_table_row(Prefs, ranked,
            total_columns_not_meaningful, module_active_to_module_name_cell),
        ModuleRowDatas, Rows, 1, _),
    Table = table(table_class_box_if_pref, NumColumns, yes(Header), Rows),
    DisplayTable = display_table(Table),

    % Build controls at the bottom of the page.
    InactiveControls = inactive_module_controls(Prefs, Cmd),
    FieldControls = field_controls(Prefs, Cmd),
    FormatControls = format_controls(Prefs, Cmd),
    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),

    Display = display(yes(Title),
        [DisplayTable,
        display_paragraph_break, InactiveControls,
        display_paragraph_break, FieldControls,
        display_paragraph_break, FormatControls,
        display_paragraph_break, MenuRestartQuitControls]).

:- pred active_module(perf_row_data(module_active)::in) is semidet.

active_module(ModuleRowData) :-
    ModuleActive = ModuleRowData ^ perf_row_subject,
    ModuleActive ^ ma_is_active = module_is_active.

:- func avoid_sort_self_and_desc(preferences) = preferences.

avoid_sort_self_and_desc(Prefs) = SortPrefs :-
    ( if Prefs ^ pref_criteria = by_cost(CostKind, self_and_desc, Scope) then
        SortPrefs = Prefs ^ pref_criteria := by_cost(CostKind, self, Scope)
    else
        SortPrefs = Prefs
    ).

%---------------------------------------------------------------------------%
%
% Code to display a module report.
%

    % Create a display_report structure for a module report.
    %
:- pred display_report_module(preferences::in, module_report::in, display::out)
    is det.

display_report_module(Prefs, ModuleReport, Display) :-
    ModuleReport = module_report(ModuleName, HaveModuleRep, ProcRowDatas0),
    Cmd = deep_cmd_module(ModuleName),
    Title = string.format("The procedures of module %s:", [s(ModuleName)]),

    ShowInactiveProcs = Prefs ^ pref_inactive ^ inactive_procs,
    (
        ShowInactiveProcs = inactive_show,
        ProcRowDatas1 = ProcRowDatas0
    ;
        ShowInactiveProcs = inactive_hide,
        list.filter(active_proc, ProcRowDatas0, ProcRowDatas1)
    ),

    MaybeCurModuleName = yes(ModuleName),
    ModuleQual = Prefs ^ pref_module_qual,
    sort_proc_active_rows_by_preferences(MaybeCurModuleName, ModuleQual,
        Prefs, ProcRowDatas1, ProcRowDatas),

    % Build the table of all modules.
    SortByNamePrefs = Prefs ^ pref_criteria := by_name,
    ProcHeaderCell = td_l(deep_link(Cmd, yes(SortByNamePrefs),
        attr_str([], "Procedure"), link_class_link)),
    RankHeaderGroup =
        make_single_table_header_group(td_s("Rank"),
            table_column_class_ordinal_rank, column_do_not_colour),
    ProcHeaderGroup =
        make_single_table_header_group(ProcHeaderCell,
            table_column_class_module_name, column_do_not_colour),
    MakeHeaderData = override_order_criteria_header_data(Cmd),
    perf_table_header(total_columns_meaningful, Prefs, MakeHeaderData,
        PerfHeaderGroups),
    AllHeaderGroups =
        [RankHeaderGroup, ProcHeaderGroup] ++ PerfHeaderGroups,
    header_groups_to_header(AllHeaderGroups, NumColumns, Header),

    list.map_foldl(
        maybe_ranked_subject_perf_table_row(Prefs, ranked,
            total_columns_meaningful,
            proc_active_to_proc_name_cell(MaybeCurModuleName, ModuleQual)),
        ProcRowDatas, Rows, 1, _),
    Table = table(table_class_box_if_pref, NumColumns, yes(Header), Rows),
    DisplayTable = display_table(Table),

    % Build controls at the bottom of the page.
    GetterSetterCmd = deep_cmd_module_getter_setters(ModuleName),
    GetterSetterControl = display_link(deep_link(GetterSetterCmd, yes(Prefs),
        attr_str([], "Show field getters and setters"), link_class_link)),
    (
        HaveModuleRep = do_not_have_module_rep,
        ModuleControls =
            [display_paragraph_break, GetterSetterControl]
    ;
        HaveModuleRep = have_module_rep,
        RepCmd = deep_cmd_module_rep(ModuleName),
        RepControl = display_link(deep_link(RepCmd, yes(Prefs),
            attr_str([], "Show module representation"), link_class_link)),
        ModuleControls =
            [display_paragraph_break, GetterSetterControl,
            display_paragraph_break, RepControl]
    ),

    InactiveControls = inactive_proc_controls(Prefs, Cmd),
    FieldControls = field_controls(Prefs, Cmd),
    FormatControls = format_controls(Prefs, Cmd),
    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),
    GeneralControls =
        [display_paragraph_break, InactiveControls,
        display_paragraph_break, FieldControls,
        display_paragraph_break, FormatControls,
        display_paragraph_break, MenuRestartQuitControls],

    Display = display(yes(Title),
        [DisplayTable] ++ ModuleControls ++ GeneralControls).

:- pred active_proc(perf_row_data(proc_active)::in) is semidet.

active_proc(ProcRowData) :-
    ProcActive = ProcRowData ^ perf_row_subject,
    ProcActive ^ pa_is_active = proc_is_active.

%---------------------------------------------------------------------------%
%
% Code to display a module_getter_setters report.
%

    % Create a display_report structure for a module_getter_setters report.
    %
:- pred display_report_module_getter_setters(preferences::in,
    module_getter_setters_report::in, display::out) is det.

display_report_module_getter_setters(Prefs, Report, Display) :-
    Report = module_getter_setters_report(ModuleName, GSMap),
    Title = string.format("The getters and setters of module %s:",
        [s(ModuleName)]),
    map.to_assoc_list(GSMap, GSPairs),
    RowLists = list.map(display_data_struct_getter_setters(Prefs, ModuleName),
        GSPairs),
    list.condense(RowLists, Rows),

    SortByNamePrefs = Prefs ^ pref_criteria := by_name,
    FieldNameHeaderCell = td_l(deep_link(Cmd, yes(SortByNamePrefs),
        attr_str([], "FieldName"), link_class_link)),

    RankHeaderGroup = make_single_table_header_group(td_s("Rank"),
        table_column_class_ordinal_rank, column_do_not_colour),
    FieldNameHeaderGroup = make_single_table_header_group(FieldNameHeaderCell,
        table_column_class_field_name, column_do_not_colour),
    Cmd = deep_cmd_module_getter_setters(ModuleName),
    MakeHeaderData = override_order_criteria_header_data(Cmd),
    perf_table_header(total_columns_meaningful, Prefs, MakeHeaderData,
        PerfHeaderGroups),
    header_groups_to_header([RankHeaderGroup, FieldNameHeaderGroup
        | PerfHeaderGroups], NumColumns, Header),

    Table = table(table_class_box_if_pref, NumColumns, yes(Header), Rows),
    TableItem = display_table(Table),

    ModuleCmd = deep_cmd_module(ModuleName),
    ModuleControl = display_link(deep_link(ModuleCmd, yes(Prefs),
        attr_str([], "Show all the procedures of the module"),
        link_class_link)),
    MenuResetQuitControls = cmds_menu_restart_quit(yes(Prefs)),
    Controls =
        [display_paragraph_break, ModuleControl,
        display_paragraph_break, MenuResetQuitControls],

    Display = display(yes(Title), [TableItem] ++ Controls).

:- func display_data_struct_getter_setters(preferences, string,
    pair(data_struct_name, gs_field_map)) = list(table_row).

display_data_struct_getter_setters(Prefs, ModuleName,
        DataStructName - FieldMap) = Rows :-
    DataStructName = data_struct_name(Name),
    Title = string.format("The getters and setters of %s:", [s(Name)]),
    TitleHeader = table_section_header(td_as(attr_str([attr_bold], Title))),
    map.to_assoc_list(FieldMap, FieldPairs0),
    sort_getter_setter_fields(Prefs, FieldPairs0, FieldPairs),
    list.map_foldl(display_field_getter_setters(Prefs, ModuleName),
        FieldPairs, DataRowLists, 1, _),
    list.condense(DataRowLists, DataRows),
    Rows = [table_separator_row, TitleHeader, table_separator_row | DataRows].

:- pred display_field_getter_setters(preferences::in, string::in,
    pair(field_name, gs_field_info)::in, list(table_row)::out,
    int::in, int::out) is det.

display_field_getter_setters(Prefs, ModuleName, FieldName - FieldInfo, Rows,
        !Rank):-
    Fields = Prefs ^ pref_fields,
    FieldName = field_name(Name),
    RankCell = table_cell(td_i(!.Rank)),
    MaybeCurModuleName = yes(ModuleName),
    ModuleQual = Prefs ^ pref_module_qual,
    (
        FieldInfo = gs_field_getter(GetterRowData),
        perf_table_row(total_columns_meaningful, Fields, GetterRowData,
            GetterPerfCells),
        GetterProcDesc = GetterRowData ^ perf_row_subject,
        GetterFieldNameCell = proc_desc_to_proc_name_cell(MaybeCurModuleName,
            ModuleQual, Prefs, GetterProcDesc),
        GetterRow =
            table_row([RankCell, GetterFieldNameCell | GetterPerfCells]),
        Rows = [table_separator_row, GetterRow]
    ;
        FieldInfo = gs_field_setter(SetterRowData),
        perf_table_row(total_columns_meaningful, Fields, SetterRowData,
            SetterPerfCells),
        SetterProcDesc = SetterRowData ^ perf_row_subject,
        SetterFieldNameCell = proc_desc_to_proc_name_cell(MaybeCurModuleName,
            ModuleQual, Prefs, SetterProcDesc),
        SetterRow =
            table_row([RankCell, SetterFieldNameCell | SetterPerfCells]),
        Rows = [table_separator_row, SetterRow]
    ;
        FieldInfo = gs_field_both(GetterRowData, SetterRowData, SumRowData),
        EmptyCell = table_cell(td_s("")),

        perf_table_row(total_columns_meaningful, Fields, SumRowData,
            SumPerfCells),
        SummaryName = string.format("%s summary", [s(Name)]),
        SumFieldNameCell = table_cell(td_s(SummaryName)),
        SumRow = table_row([RankCell, SumFieldNameCell | SumPerfCells]),

        perf_table_row(total_columns_meaningful, Fields, GetterRowData,
            GetterPerfCells),
        GetterProcDesc = GetterRowData ^ perf_row_subject,
        GetterFieldNameCell = proc_desc_to_proc_name_cell(MaybeCurModuleName,
            ModuleQual, Prefs, GetterProcDesc),
        GetterRow =
            table_row([EmptyCell, GetterFieldNameCell | GetterPerfCells]),

        perf_table_row(total_columns_meaningful, Fields, SetterRowData,
            SetterPerfCells),
        SetterProcDesc = SetterRowData ^ perf_row_subject,
        SetterFieldNameCell = proc_desc_to_proc_name_cell(MaybeCurModuleName,
            ModuleQual, Prefs, SetterProcDesc),
        SetterRow =
            table_row([EmptyCell, SetterFieldNameCell | SetterPerfCells]),

        Rows = [table_separator_row, SumRow, GetterRow, SetterRow]
    ),
    !:Rank = !.Rank + 1.

%---------------------------------------------------------------------------%
%
% Code to display a module_rep report.
%

    % Create a display_report structure for a module_rep report.
    %
:- pred display_report_module_rep(preferences::in, module_rep_report::in,
    display::out) is det.

display_report_module_rep(Prefs, Report, Display) :-
    Report = module_rep_report(ModuleName, ModuleRepStr),
    Title = string.format("The representation of module %s:",
        [s(ModuleName)]),

    BodyItem = display_verbatim(ModuleRepStr),

    ModuleCmd = deep_cmd_module(ModuleName),
    ModuleControl = display_link(deep_link(ModuleCmd, yes(Prefs),
        attr_str([], "Show all the procedures of the module"),
        link_class_link)),
    MenuResetQuitControls = cmds_menu_restart_quit(yes(Prefs)),
    Controls =
        [display_paragraph_break, ModuleControl,
        display_paragraph_break, MenuResetQuitControls],

    Display = display(yes(Title), [BodyItem | Controls]).

%---------------------------------------------------------------------------%
%
% Code to display a top procedures report.
%

    % Create a display_report structure for a top_procedures report.
    %
:- pred display_report_top_procs(preferences::in, top_procs_report::in,
    display::out) is det.

display_report_top_procs(Prefs, TopProcsReport, Display) :-
    TopProcsReport = top_procs_report(Ordering, TopProcRowDatas),
    Ordering = report_ordering(DisplayLimit, CostKind, InclDesc, Scope),
    Desc = cost_criteria_to_description(CostKind, InclDesc, Scope),
    Title = "Top procedures " ++ Desc,

    % Build the table of the top procedures.
    Cmd = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope),
    MakeHeaderData = override_order_criteria_header_data(Cmd),
    maybe_ranked_proc_table_header(Prefs, ranked, MakeHeaderData,
        NumColumns, Header),

    MaybeCurModuleName = no,
    ModuleQual = Prefs ^ pref_module_qual,
    sort_proc_desc_rows_by_preferences(MaybeCurModuleName, ModuleQual,
        Prefs, TopProcRowDatas, OrderedTopProcRowDatas),
    list.map_foldl(
        maybe_ranked_subject_perf_table_row(Prefs, ranked,
            total_columns_meaningful,
            proc_desc_to_proc_name_cell(no, ModuleQual)),
        OrderedTopProcRowDatas, Rows, 1, _),
    Table = table(table_class_box_if_pref, NumColumns, yes(Header), Rows),
    DisplayTable = display_table(Table),

    % Build controls at the bottom of the page.
    TopProcsControls = top_procs_controls(Prefs,
        DisplayLimit, CostKind, InclDesc, Scope),
    FieldControls = field_controls(Prefs, Cmd),
    FormatControls = format_controls(Prefs, Cmd),
    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),

    Display = display(yes(Title),
        [DisplayTable,
        display_paragraph_break, TopProcsControls,
        display_paragraph_break, FieldControls,
        display_paragraph_break, FormatControls,
        display_paragraph_break, MenuRestartQuitControls]).

    % Create a phrase describing how the top procedures may be sorted.
    %
:- func cost_criteria_to_description(cost_kind, include_descendants,
    measurement_scope) = string.

cost_criteria_to_description(CostKind, InclDesc, Scope) = Desc :-
    Desc =
        "ordered by " ++
        incl_desc_to_description(InclDesc) ++ " " ++
        cost_kind_to_description(CostKind) ++ " " ++
        scope_to_description(Scope).

    % Give the short name for what profiling data a field may be measuring.
    %
:- func incl_desc_to_description(include_descendants) = string.

incl_desc_to_description(self) = "self".
incl_desc_to_description(self_and_desc) = "total".

    % Describe the a measurement used by the deep profiler.
    %
:- func cost_kind_to_description(cost_kind) = string.

cost_kind_to_description(cost_calls)    = "number of calls".
cost_kind_to_description(cost_redos)    = "number of redos".
cost_kind_to_description(cost_time)     = "time".
cost_kind_to_description(cost_callseqs) = "call sequence numbers".
cost_kind_to_description(cost_allocs)   = "memory allocations".
cost_kind_to_description(cost_words)    = "words allocated".

    % Describe a scope of profiling data.
    %
:- func scope_to_description(measurement_scope) = string.

scope_to_description(per_call) = "per call".
scope_to_description(overall) = "overall".

%---------------------------------------------------------------------------%
%
% Code to display a procedure report.
%

    % Create a display_report structure for a proc report.
    %
:- pred display_report_proc(deep::in, preferences::in, proc_report::in,
    display::out) is det.

display_report_proc(Deep, Prefs, ProcReport, Display) :-
    ProcReport = proc_report(CallersSummaryRowData, ProcSummaryRowData,
        CallSitePerfs0),
    ProcDesc = ProcSummaryRowData ^ perf_row_subject,
    RefinedName = ProcDesc ^ pdesc_q_refined_name,
    Title = "Summary of procedure " ++ RefinedName,

    PSPtr = ProcDesc ^ pdesc_ps_ptr,
    Cmd = deep_cmd_proc(PSPtr),

    SortByContextPrefs = Prefs ^ pref_criteria := by_context,
    SourceHeaderCell = td_l(deep_link(Cmd, yes(SortByContextPrefs),
        attr_str([], "Source"), link_class_link)),

    SortByNamePrefs = Prefs ^ pref_criteria := by_name,
    ProcHeaderCell = td_l(deep_link(Cmd, yes(SortByNamePrefs),
        attr_str([], "Procedure"), link_class_link)),

    SourceHeaderGroup = make_single_table_header_group(SourceHeaderCell,
        table_column_class_source_context, column_do_not_colour),
    ProcHeaderGroup = make_single_table_header_group(ProcHeaderCell,
        table_column_class_proc, column_do_not_colour),
    MakeHeaderData = override_order_criteria_header_data(Cmd),
    perf_table_header(total_columns_meaningful, Prefs, MakeHeaderData,
        PerfHeaderGroups),
    AllHeaderGroups =
        [SourceHeaderGroup, ProcHeaderGroup] ++ PerfHeaderGroups,
    header_groups_to_header(AllHeaderGroups, NumColumns, Header),

    Fields = Prefs ^ pref_fields,
    CallersCounts = CallersSummaryRowData ^ perf_row_subject,
    CallersSummaryText = format("%d static & %d dynamic call sites",
        [i(CallersCounts ^ cc_static), i(CallersCounts ^ cc_dynamic)]),
    CallersSummaryCell = table_multi_cell(td_s(CallersSummaryText), 2),
    perf_table_row(total_columns_meaningful, Fields, CallersSummaryRowData,
        CallersPerfCells),
    CallersCells = [CallersSummaryCell] ++ CallersPerfCells,
    CallersRow = table_row(CallersCells),

    % We could make SummaryProcCell a link, but the only link that would make
    % sense (and the link that pre-display versions of the deep profiler
    % generated) point back to this page itself, which is not useful and
    % could be considered misleading.
    %
    % SummaryProcCell spans two columns: the ones that contain (1) the context
    % and (2) the callee of each call site in the rows below.
    SummaryProcCell = table_multi_cell(td_s(RefinedName), 2),
    perf_table_row(total_columns_meaningful, Fields, ProcSummaryRowData,
        SummaryPerfCells),
    SummaryCells = [SummaryProcCell] ++ SummaryPerfCells,
    SummaryRow = table_row(SummaryCells),

    MaybeCurModuleName = yes(ProcDesc ^ pdesc_module_name),
    ModuleQual = Prefs ^ pref_module_qual,
    sort_call_sites_by_preferences(MaybeCurModuleName, ModuleQual, Prefs,
        CallSitePerfs0, CallSitePerfs),
    CallSiteRowLists = list.map(
        report_proc_call_site(MaybeCurModuleName, ModuleQual, Prefs),
        CallSitePerfs),
    list.condense(CallSiteRowLists, CallSiteRows),
    DeveloperRows = map(func(X) = table_developer_row(X),
        [table_section_header(td_s(
            "Callers excluding directly-recursive calls")),
        CallersRow, table_separator_row]),
    CommonRows = [SummaryRow, table_separator_row] ++ CallSiteRows,
    AllRows = DeveloperRows ++ CommonRows,
    Table = table(table_class_box_if_pref, NumColumns, yes(Header), AllRows),
    DisplayTable = display_table(Table),

    % Build the controls at the bottom of the page.
    ProcCallersControls = proc_callers_group_controls(Deep, Prefs, Cmd,
        PSPtr, group_by_call_site, default_callers_per_bunch,
        Prefs ^ pref_contour),
    SummarizeControls = summarize_controls(Prefs, Cmd),
    InactiveCallSitesControls = inactive_call_site_controls(Prefs, Cmd),
    ModuleQualControls = module_qual_controls(Prefs, Cmd),
    SortControls = sort_controls(Prefs, Cmd),
    FieldControls = field_controls(Prefs, Cmd),
    FormatControls = format_controls(Prefs, Cmd),
    ProcReportControls = proc_reports_controls(Prefs, PSPtr, Cmd),
    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),

    Display = display(yes(Title),
        [DisplayTable,
        display_paragraph_break, ProcCallersControls,
        display_paragraph_break, SummarizeControls,
        display_paragraph_break, InactiveCallSitesControls,
        display_paragraph_break, ModuleQualControls,
        display_paragraph_break, SortControls,
        display_paragraph_break, FieldControls,
        display_paragraph_break, FormatControls,
        display_paragraph_break, ProcReportControls,
        display_paragraph_break, MenuRestartQuitControls]).

:- func default_callers_per_bunch = int.

default_callers_per_bunch = 20.

:- func report_proc_call_site(maybe(string), module_qual, preferences,
    call_site_perf) = list(table_row).

report_proc_call_site(MaybeCurModuleName, ModuleQual, Prefs, CallSitePerf)
        = Rows :-
    CallSitePerf =
        call_site_perf(KindAndCallee, SummaryPerfRowData, SubPerfs0),

    CallSiteDesc = SummaryPerfRowData ^ perf_row_subject,
    ContextCell = call_site_desc_source_cell(CallSiteDesc),

    (
        KindAndCallee = normal_call_and_info(NormalCalleeId),
        NormalCalleeId = normal_callee_id(CalleeDesc, TypeSubstStr),
        CalleeRefinedName = proc_desc_get_refined_id(MaybeCurModuleName,
            ModuleQual, CalleeDesc),
        ( if TypeSubstStr = "" then
            CallSiteStr = CalleeRefinedName
        else
            CallSiteStr = string.format("%s [%s]",
                [s(CalleeRefinedName), s(TypeSubstStr)])
        ),
        CalleePSPtr = CalleeDesc ^ pdesc_ps_ptr,
        CallSiteLinkCmd = deep_cmd_proc(CalleePSPtr),
        CallSiteLink = deep_link(CallSiteLinkCmd, yes(Prefs),
            attr_str([], CallSiteStr), link_class_link),
        CallSiteCell = table_cell(td_l(CallSiteLink)),

        require(unify(SubPerfs0, []),
            "report_proc_call_site: SubPerfs0 != [] for normal call site")
    ;
        (
            KindAndCallee = special_call_and_no_info,
            CallSiteStr = "special call"
        ;
            KindAndCallee = higher_order_call_and_no_info,
            CallSiteStr = "higher order call"
        ;
            KindAndCallee = method_call_and_no_info,
            CallSiteStr = "method call"
        ;
            KindAndCallee = callback_and_no_info,
            CallSiteStr = "callback"
        ),
        CallSiteCell = table_cell(td_s(CallSiteStr))
    ),

    Fields = Prefs ^ pref_fields,
    perf_table_row(total_columns_meaningful, Fields, SummaryPerfRowData,
        SummaryPerfCells),
    SummaryCells = [ContextCell, CallSiteCell] ++ SummaryPerfCells,
    SummaryRow = table_row(SummaryCells),

    Summarize = Prefs ^ pref_summarize,
    (
        Summarize = summarize_ho_call_sites,
        Rows = [SummaryRow]
    ;
        Summarize = do_not_summarize_ho_call_sites,
        sort_proc_desc_rows_by_preferences(MaybeCurModuleName, ModuleQual,
            Prefs, SubPerfs0, SubPerfs),
        SubRows = list.map(
            report_proc_call_site_callee(MaybeCurModuleName, ModuleQual,
                Prefs),
            SubPerfs),
        Rows = [SummaryRow] ++ SubRows
    ).

:- func report_proc_call_site_callee(maybe(string), module_qual,
    preferences, perf_row_data(proc_desc)) = table_row.

report_proc_call_site_callee(MaybeCurModuleName, ModuleQual,
        Prefs, RowData) = Row :-
    Fields = Prefs ^ pref_fields,
    EmptyCell = table_empty_cell,
    ProcDesc = RowData ^ perf_row_subject,
    ProcCell = proc_desc_to_proc_name_cell(MaybeCurModuleName, ModuleQual,
        Prefs, ProcDesc),
    perf_table_row(total_columns_meaningful, Fields, RowData, PerfCells),
    Cells = [EmptyCell, ProcCell] ++ PerfCells,
    Row = table_row(Cells).

%---------------------------------------------------------------------------%
%
% Code to display a procedure callers report.
%

    % Create a display_report structure for a proc_callers report.
    %
:- pred display_report_proc_callers(deep::in, preferences::in,
    proc_callers_report::in, display::out) is det.

display_report_proc_callers(Deep, Prefs0, ProcCallersReport, Display) :-
    ProcCallersReport = proc_callers_report(ProcDesc, CallerRowDatas,
        BunchNum, BunchSize, ContourExcl, MaybeContourWarnMessage),

    (
        MaybeContourWarnMessage = no,
        WarnItems = []
    ;
        MaybeContourWarnMessage = yes(ContourWarnMessage),
        ContourWarnItem = display_text(ContourWarnMessage),
        WarnItems = [display_paragraph_break, ContourWarnItem]
    ),

    RefinedName = ProcDesc ^ pdesc_q_refined_name,

    MaybeCurModuleName = yes(ProcDesc ^ pdesc_module_name),
    ModuleQual = Prefs ^ pref_module_qual,

    % Remember the selected value of contour exclusion.
    Prefs = Prefs0 ^ pref_contour := ContourExcl,

    PSPtr = ProcDesc ^ pdesc_ps_ptr,

    (
        CallerRowDatas = proc_caller_call_sites(CallSiteRowDatas),
        CallerGroups = group_by_call_site,
        Cmd = deep_cmd_proc_callers(PSPtr, CallerGroups, BunchNum, BunchSize,
            ContourExcl),
        Title = "The call sites calling " ++ RefinedName,
        sort_call_site_desc_rows_by_preferences(MaybeCurModuleName, ModuleQual,
            Prefs, CallSiteRowDatas, SortedCallSiteRowDatas),
        select_displayed_rows(SortedCallSiteRowDatas, BunchNum, BunchSize,
            DisplayedCallSiteRowDatas, TotalNumRows, FirstRowNum, LastRowNum,
            DisplayedBunchNum, MaybeFirstAndLastBunchNum),
        list.map_foldl(
            display_caller_call_site(MaybeCurModuleName, ModuleQual, Prefs),
            DisplayedCallSiteRowDatas, Rows, FirstRowNum, AfterLastRowNum),

        SortByContextPrefs = Prefs ^ pref_criteria := by_context,
        SourceHeaderCell = td_l(deep_link(Cmd, yes(SortByContextPrefs),
            attr_str([], "Source"), link_class_link)),
        SortByNamePrefs = Prefs ^ pref_criteria := by_context,
        ProcHeaderCell = td_l(deep_link(Cmd, yes(SortByNamePrefs),
            attr_str([], "In procedure"), link_class_link)),

        SourceHeaderGroup = make_single_table_header_group(SourceHeaderCell,
            table_column_class_source_context, column_do_not_colour),
        ProcHeaderGroup = make_single_table_header_group(ProcHeaderCell,
            table_column_class_proc, column_do_not_colour),
        IdHeaderGroups = [SourceHeaderGroup, ProcHeaderGroup]
    ;
        CallerRowDatas = proc_caller_procedures(ProcRowDatas),
        CallerGroups = group_by_proc,
        Cmd = deep_cmd_proc_callers(PSPtr, CallerGroups, BunchNum, BunchSize,
            ContourExcl),
        Title = "The procedures calling " ++ RefinedName,
        sort_proc_desc_rows_by_preferences(MaybeCurModuleName, ModuleQual,
            Prefs, ProcRowDatas, SortedProcRowDatas),
        select_displayed_rows(SortedProcRowDatas, BunchNum, BunchSize,
            DisplayedProcRowDatas, TotalNumRows, FirstRowNum, LastRowNum,
            DisplayedBunchNum, MaybeFirstAndLastBunchNum),
        list.map_foldl(
            display_caller_proc(MaybeCurModuleName, ModuleQual, Prefs),
            DisplayedProcRowDatas, Rows, FirstRowNum, AfterLastRowNum),

        SortByContextPrefs = Prefs ^ pref_criteria := by_context,
        SourceHeaderCell = td_l(deep_link(Cmd, yes(SortByContextPrefs),
            attr_str([], "Source"), link_class_link)),
        SortByNamePrefs = Prefs ^ pref_criteria := by_context,
        ProcHeaderCell = td_l(deep_link(Cmd, yes(SortByNamePrefs),
            attr_str([], "Procedure"), link_class_link)),

        SourceHeaderGroup = make_single_table_header_group(SourceHeaderCell,
            table_column_class_source_context, column_do_not_colour),
        ProcHeaderGroup = make_single_table_header_group(ProcHeaderCell,
            table_column_class_proc, column_do_not_colour),
        IdHeaderGroups = [SourceHeaderGroup, ProcHeaderGroup]
    ;
        CallerRowDatas = proc_caller_modules(ModuleRowDatas),
        CallerGroups = group_by_module,
        Cmd = deep_cmd_proc_callers(PSPtr, CallerGroups, BunchNum, BunchSize,
            ContourExcl),
        Title = "The modules calling " ++ RefinedName,
        sort_module_name_rows_by_preferences(Prefs, ModuleRowDatas,
            SortedModuleRowDatas),
        select_displayed_rows(SortedModuleRowDatas, BunchNum, BunchSize,
            DisplayedModuleRowDatas, TotalNumRows, FirstRowNum, LastRowNum,
            DisplayedBunchNum, MaybeFirstAndLastBunchNum),
        list.map_foldl(display_caller_module(Prefs),
            DisplayedModuleRowDatas, Rows, FirstRowNum, AfterLastRowNum),

        SortByNamePrefs = Prefs ^ pref_criteria := by_name,
        ModuleHeaderCell = td_l(deep_link(Cmd, yes(SortByNamePrefs),
            attr_str([], "Module"), link_class_link)),

        ModuleHeaderGroup = make_single_table_header_group(ModuleHeaderCell,
            table_column_class_source_context, column_do_not_colour),
        IdHeaderGroups = [ModuleHeaderGroup]
    ;
        CallerRowDatas = proc_caller_cliques(CliqueRowDatas),
        CallerGroups = group_by_clique,
        Cmd = deep_cmd_proc_callers(PSPtr, CallerGroups, BunchNum, BunchSize,
            ContourExcl),
        Title = "The cliques calling " ++ RefinedName,
        sort_clique_rows_by_preferences(Prefs, CliqueRowDatas,
            SortedCliqueRowDatas),
        select_displayed_rows(SortedCliqueRowDatas, BunchNum, BunchSize,
            DisplayedCliqueRowDatas, TotalNumRows, FirstRowNum, LastRowNum,
            DisplayedBunchNum, MaybeFirstAndLastBunchNum),
        list.map_foldl(
            display_caller_clique(MaybeCurModuleName, ModuleQual, Prefs),
            DisplayedCliqueRowDatas, Rows, FirstRowNum, AfterLastRowNum),

        SortByNamePrefs = Prefs ^ pref_criteria := by_name,
        CliqueHeaderCell = td_l(deep_link(Cmd, yes(SortByNamePrefs),
            attr_str([], "Clique"), link_class_link)),

        CliqueHeaderGroup = make_single_table_header_group(CliqueHeaderCell,
            table_column_class_clique, column_do_not_colour),
        MembersHeaderGroup = make_single_table_header_group(td_s("Members"),
            table_column_class_clique, column_do_not_colour),
        IdHeaderGroups = [CliqueHeaderGroup, MembersHeaderGroup]
    ),

    RankHeaderGroup = make_single_table_header_group(td_s("Rank"),
        table_column_class_ordinal_rank, column_do_not_colour),
    MakeHeaderData = override_order_criteria_header_data(Cmd),
    perf_table_header(total_columns_meaningful, Prefs, MakeHeaderData,
        PerfHeaderGroups),
    AllHeaderGroups = [RankHeaderGroup] ++ IdHeaderGroups ++ PerfHeaderGroups,
    header_groups_to_header(AllHeaderGroups, NumColumns, Header),

    (
        MaybeFirstAndLastBunchNum = no,
        Message = "There are none.",
        Display = display(yes(Title), [display_text(Message)])
    ;
        MaybeFirstAndLastBunchNum = yes({FirstBunchNum, LastBunchNum}),
        require(unify(LastRowNum + 1, AfterLastRowNum),
            "display_report_proc_callers: row number mismatch"),
        require((FirstBunchNum =< DisplayedBunchNum),
            "display_report_proc_callers: display bunch number mismatch"),
        require((DisplayedBunchNum =< LastBunchNum),
            "display_report_proc_callers: display bunch number mismatch"),

        ( if FirstBunchNum = LastBunchNum then
            Message = string.format("There are %d:",
                [i(TotalNumRows)]),
            BunchControls = []
        else
            Message = string.format("There are %d, showing %d to %d:",
                [i(TotalNumRows), i(FirstRowNum), i(LastRowNum)]),
            ( if BunchNum > FirstBunchNum then
                BunchControlsFirst = [make_proc_callers_link(Prefs,
                    "First group", PSPtr, CallerGroups, FirstBunchNum,
                    BunchSize, ContourExcl)]
            else
                BunchControlsFirst = []
            ),
            ( if BunchNum - 1 > FirstBunchNum then
                BunchControlsPrev = [make_proc_callers_link(Prefs,
                    "Previous group", PSPtr, CallerGroups, BunchNum - 1,
                    BunchSize, ContourExcl)]
            else
                BunchControlsPrev = []
            ),
            ( if BunchNum + 1 < LastBunchNum then
                BunchControlsNext = [make_proc_callers_link(Prefs,
                    "Next group", PSPtr, CallerGroups, BunchNum + 1,
                    BunchSize, ContourExcl)]
            else
                BunchControlsNext = []
            ),
            ( if BunchNum < LastBunchNum then
                BunchControlsLast = [make_proc_callers_link(Prefs,
                    "Last group", PSPtr, CallerGroups, LastBunchNum,
                    BunchSize, ContourExcl)]
            else
                BunchControlsLast = []
            ),
            BunchControlList = BunchControlsFirst ++ BunchControlsPrev ++
                BunchControlsNext ++ BunchControlsLast,
            BunchControls = [display_paragraph_break,
                display_list(list_class_horizontal_except_title,
                    yes("Show other groups:"), BunchControlList)]
        ),

        ( if
            BunchSize > 5,
            HalfBunchSize = BunchSize / 2,
            HalfBunchSize \= 10,
            HalfBunchSize \= 20,
            HalfBunchSize \= 50,
            HalfBunchSize \= 100
        then
            BunchSizeControlPairsHalf = [(BunchSize / 2) -
                make_proc_callers_link(Prefs,
                    "Halve group size", PSPtr, CallerGroups, BunchNum * 2,
                    BunchSize / 2, ContourExcl)]
        else
            BunchSizeControlPairsHalf = []
        ),
        ( if
            DoubleBunchSize = BunchSize * 2,
            DoubleBunchSize \= 10,
            DoubleBunchSize \= 20,
            DoubleBunchSize \= 50,
            DoubleBunchSize \= 100
        then
            BunchSizeControlPairsDouble = [(BunchSize * 2) -
                make_proc_callers_link(Prefs,
                    "Double group size", PSPtr, CallerGroups, BunchNum / 2,
                    BunchSize * 2, ContourExcl)]
        else
            BunchSizeControlPairsDouble = []
        ),
        ( if
            TotalNumRows > 10,
            BunchSize \= 10
        then
            BunchSizeControlPairs10 = [10 -
                make_proc_callers_link(Prefs,
                    "Set group size to 10", PSPtr, CallerGroups,
                    (BunchNum * BunchSize) / 10, 10, ContourExcl)]
        else
            BunchSizeControlPairs10 = []
        ),
        ( if
            TotalNumRows > 20,
            BunchSize \= 20
        then
            BunchSizeControlPairs20 = [20 -
                make_proc_callers_link(Prefs,
                    "Set group size to 20", PSPtr, CallerGroups,
                    (BunchNum * BunchSize) / 20, 20, ContourExcl)]
        else
            BunchSizeControlPairs20 = []
        ),
        ( if
            TotalNumRows > 50,
            BunchSize \= 50
        then
            BunchSizeControlPairs50 = [50 -
                make_proc_callers_link(Prefs,
                    "Set group size to 50", PSPtr, CallerGroups,
                    (BunchNum * BunchSize) / 50, 50, ContourExcl)]
        else
            BunchSizeControlPairs50 = []
        ),
        ( if
            TotalNumRows > 100,
            BunchSize \= 100
        then
            BunchSizeControlPairs100 = [100 -
                make_proc_callers_link(Prefs,
                    "Set group size to 100", PSPtr, CallerGroups,
                    (BunchNum * BunchSize) / 100, 100, ContourExcl)]
        else
            BunchSizeControlPairs100 = []
        ),
        BunchSizeControlPairs =
            BunchSizeControlPairsHalf ++ BunchSizeControlPairsDouble ++
            BunchSizeControlPairs10 ++ BunchSizeControlPairs20 ++
            BunchSizeControlPairs50 ++ BunchSizeControlPairs100,
        list.sort(BunchSizeControlPairs, SortedBunchSizeControlPairs),
        assoc_list.values(SortedBunchSizeControlPairs, BunchSizeControlList),
        BunchSizeControls = [display_paragraph_break,
            display_list(list_class_horizontal_except_title,
            yes("Change group size:"), BunchSizeControlList)],

        Table = table(table_class_box_if_pref, NumColumns, yes(Header), Rows),
        DisplayTable = display_table(Table),

        % Build the controls at the bottom of the page.
        FirstCmd = deep_cmd_proc_callers(PSPtr, CallerGroups, 1, BunchSize,
            ContourExcl),
        CallerGroupControls = proc_callers_group_controls(Deep, Prefs,
            FirstCmd, PSPtr, CallerGroups, BunchSize, ContourExcl),
        FieldControls = field_controls(Prefs, Cmd),
        FormatControls = format_controls(Prefs, Cmd),
        MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),

        Display = display(yes(Title),
            [display_text(Message)] ++
            WarnItems ++
            [display_paragraph_break, DisplayTable] ++
            BunchControls ++
            BunchSizeControls ++
            [display_paragraph_break, CallerGroupControls,
            display_paragraph_break, FieldControls,
            display_paragraph_break, FormatControls,
            display_paragraph_break, MenuRestartQuitControls])
    ).

:- pred select_displayed_rows(list(perf_row_data(T))::in, int::in, int::in,
    list(perf_row_data(T))::out, int::out, int::out, int::out,
    int::out, maybe({int, int})::out) is det.

select_displayed_rows(RowDatas, BunchNum, BunchSize, DisplayRowDatas,
        TotalNumRows, FirstRowNum, LastRowNum,
        DisplayedBunchNum, MaybeFirstAndLastBunchNum) :-
    list.length(RowDatas, TotalNumRows),
    NumRowsToDelete = (BunchNum - 1) * BunchSize,
    ( if
        list.drop(NumRowsToDelete, RowDatas, RemainingRowDatasPrime),
        RemainingRowDatasPrime = [_ | _]
    then
        DisplayedBunchNum = BunchNum,
        % We start counting rows at 1, not 0.
        FirstRowNum = NumRowsToDelete + 1,
        RemainingRowDatas = RemainingRowDatasPrime,
        NumRemainingRows = TotalNumRows - NumRowsToDelete
    else
        DisplayedBunchNum = 1,
        FirstRowNum = 1,
        RemainingRowDatas = RowDatas,
        NumRemainingRows = TotalNumRows
    ),
    ( if NumRemainingRows > BunchSize then
        list.take_upto(BunchSize, RemainingRowDatas, DisplayRowDatas)
    else
        DisplayRowDatas = RemainingRowDatas
    ),
    LastRowNum = FirstRowNum - 1 + list.length(DisplayRowDatas),
    ( if TotalNumRows > 0 then
        FirstBunchNum = 1,
        LastBunchNum = (TotalNumRows + BunchSize - 1) / BunchSize,
        MaybeFirstAndLastBunchNum = yes({FirstBunchNum, LastBunchNum})
    else
        MaybeFirstAndLastBunchNum = no
    ).

:- pred display_caller_call_site(maybe(string)::in, module_qual::in,
    preferences::in, perf_row_data(call_site_desc)::in, table_row::out,
    int::in, int::out) is det.

display_caller_call_site(MaybeCurModuleName, ModuleQual, Prefs,
        CallSiteRowData, Row, !RowNum) :-
    RankCell = table_cell(td_i(!.RowNum)),
    !:RowNum = !.RowNum + 1,

    CallSiteDesc = CallSiteRowData ^ perf_row_subject,
    SourceCell = call_site_desc_to_source_cell(CallSiteDesc),
    ProcCell = call_site_desc_to_caller_proc_name_cell(MaybeCurModuleName,
        ModuleQual, Prefs, CallSiteDesc),

    Fields = Prefs ^ pref_fields,
    perf_table_row(total_columns_meaningful, Fields, CallSiteRowData,
        PerfCells),
    Cells = [RankCell, SourceCell, ProcCell] ++ PerfCells,
    Row = table_row(Cells).

:- pred display_caller_proc(maybe(string)::in, module_qual::in,
    preferences::in, perf_row_data(proc_desc)::in, table_row::out,
    int::in, int::out) is det.

display_caller_proc(MaybeCurModuleName, ModuleQual, Prefs, ProcRowData, Row,
        !RowNum) :-
    RankCell = table_cell(td_i(!.RowNum)),
    !:RowNum = !.RowNum + 1,

    ProcDesc = ProcRowData ^ perf_row_subject,
    SourceCell = proc_desc_to_source_cell(ProcDesc),
    ProcCell = proc_desc_to_proc_name_cell(MaybeCurModuleName, ModuleQual,
        Prefs, ProcDesc),

    Fields = Prefs ^ pref_fields,
    perf_table_row(total_columns_meaningful, Fields, ProcRowData, PerfCells),
    Cells = [RankCell, SourceCell, ProcCell] ++ PerfCells,
    Row = table_row(Cells).

:- pred display_caller_module(preferences::in, perf_row_data(string)::in,
    table_row::out, int::in, int::out) is det.

display_caller_module(Prefs, ModuleRowData, Row, !RowNum) :-
    RankCell = table_cell(td_i(!.RowNum)),
    !:RowNum = !.RowNum + 1,

    ModuleName = ModuleRowData ^ perf_row_subject,
    ModuleCell = table_cell(td_s(ModuleName)),

    Fields = Prefs ^ pref_fields,
    perf_table_row(total_columns_meaningful, Fields, ModuleRowData, PerfCells),
    Cells = [RankCell, ModuleCell] ++ PerfCells,
    Row = table_row(Cells).

:- pred display_caller_clique(maybe(string)::in, module_qual::in,
    preferences::in, perf_row_data(clique_desc)::in, table_row::out,
    int::in, int::out) is det.

display_caller_clique(MaybeCurModuleName, ModuleQual, Prefs,
        CliqueRowData, Row, !RowNum) :-
    RankCell = table_cell(td_i(!.RowNum)),
    !:RowNum = !.RowNum + 1,

    CliqueDesc = CliqueRowData ^ perf_row_subject,
    CliqueDesc = clique_desc(CliquePtr, EntryProcDesc, OtherProcDescs),
    CliquePtr = clique_ptr(CliqueNum),

    CliqueLinkCmd = deep_cmd_clique(CliquePtr),
    CliqueText = string.format("clique %d", [i(CliqueNum)]),
    CliqueLink = deep_link(CliqueLinkCmd, yes(Prefs), attr_str([], CliqueText),
        link_class_link),
    CliqueCell = table_cell(td_l(CliqueLink)),

    MembersStrs = list.map(
        proc_desc_get_refined_id(MaybeCurModuleName, ModuleQual),
        [EntryProcDesc | OtherProcDescs]),
    MembersStr = string.join_list(", ", MembersStrs),
    MembersCell = table_cell(td_s(MembersStr)),

    Fields = Prefs ^ pref_fields,
    perf_table_row(total_columns_meaningful, Fields, CliqueRowData, PerfCells),
    Cells = [RankCell, CliqueCell, MembersCell] ++ PerfCells,
    Row = table_row(Cells).

:- func make_proc_callers_link(preferences, string, proc_static_ptr,
    caller_groups, int, int, contour_exclusion) = display_item.

make_proc_callers_link(Prefs, Label, PSPtr, CallerGroups, BunchNum, BunchSize,
        ContourExcl) = Item :-
    Cmd = deep_cmd_proc_callers(PSPtr, CallerGroups, BunchNum, BunchSize,
        ContourExcl),
    Link = deep_link(Cmd, yes(Prefs), attr_str([], Label), link_class_control),
    Item = display_link(Link).

%---------------------------------------------------------------------------%
%
% Code to display procrep_coverage information.
%

:- pred display_report_procrep_coverage_info(preferences::in,
    procrep_coverage_info::in, display::out) is det.

display_report_procrep_coverage_info(Prefs, ProcrepCoverageReport, Display) :-
    ProcrepCoverageReport =
        procrep_coverage_info(PSPtr, Procrep, CoverageArray),
    print_proc_to_strings(get_goal_attribute_det(CoverageArray), Procrep,
        ProcRepStrings),
    string.append_list(list(ProcRepStrings), ProcRepString),
    CoverageInfoItem = display_verbatim(ProcRepString),

    Cmd = deep_cmd_static_procrep_coverage(PSPtr),
    ProcReportControls = proc_reports_controls(Prefs, PSPtr, Cmd),
    MenuResetQuitControls = cmds_menu_restart_quit(yes(Prefs)),
    Controls =
        [display_paragraph_break, ProcReportControls,
        display_paragraph_break, MenuResetQuitControls],

    Title = "Procedure coverage:",
    Display = display(yes(Title), [CoverageInfoItem] ++ Controls).

:- instance goal_annotation(coverage_info) where [
        pred(print_goal_annotation_to_strings/3) is coverage_to_cord_string
    ].

    % Print the coverage information for a goal, this is used by
    % print_proc_to_strings.
    %
:- pred coverage_to_cord_string(var_name_table::in, coverage_info::in,
    cord(cord(string))::out) is det.

coverage_to_cord_string(_, Coverage, singleton(singleton(String))) :-
    (
        Coverage = coverage_unknown,
        String0 = "_ - _"
    ;
        Coverage = coverage_known(Before, After),
        String0 = string.format("%d - %d", [i(Before), i(After)])
    ;
        (
            Coverage = coverage_known_same(Count)
        ;
            Coverage = coverage_known_zero,
            Count = 0
        ),
        String0 = string.format("%d - %d", [i(Count), i(Count)])
    ;
        Coverage = coverage_known_before(Before),
        String0 = string.format("%d - _", [i(Before)])
    ;
        Coverage = coverage_known_after(After),
        String0 = string.format("_ - %d", [i(After)])
    ),
    String = "Coverage: " ++ String0.

%---------------------------------------------------------------------------%
%
% Code to display proc_static and proc_dynamic dumps.
%

    % Create a display_report structure for a proc_static_dump report.
    %
:- pred display_report_proc_static_dump(proc_static_dump_info::in,
    display::out) is det.

display_report_proc_static_dump(ProcStaticDumpInfo, Display) :-
    ProcStaticDumpInfo = proc_static_dump_info(PSPtr, RawName,
        UnQualRefinedName, QualRefinedName, FileName, LineNumber,
        NumCallSites, NumCoveragePoints),
    PSPtr = proc_static_ptr(PSI),
    string.format("Dump of proc_static %d", [i(PSI)], Title),

    Values =
        [("Raw name:"                   - td_s(RawName)),
        ("Unqualified refined name:"    - td_s(UnQualRefinedName)),
        ("Qualified refined name:"      - td_s(QualRefinedName)),
        ("File name:"                   - td_s(FileName)),
        ("Line number:"                 - td_i(LineNumber)),
        ("Number of call sites:"        - td_i(NumCallSites)),
        ("Number of coverage points:"   - td_i(NumCoveragePoints))],

    Rows = list.map(make_labelled_table_row, Values),
    Table = table(table_class_do_not_box, 2, no, Rows),
    Display = display(yes(Title), [display_table(Table)]).

    % Create a display_report structure for a proc_dynamic_dump report.
    %
:- pred display_report_proc_dynamic_dump(deep::in, preferences::in,
    proc_dynamic_dump_info::in, display::out) is det.

display_report_proc_dynamic_dump(_Deep, Prefs, ProcDynamicDumpInfo, Display) :-
    ProcDynamicDumpInfo = proc_dynamic_dump_info(PDPtr, PSPtr, RawName,
        ModuleName, UnQualRefinedName, QualRefinedName, CallSites,
        MaybeCoveragePoints),
    PDPtr = proc_dynamic_ptr(PDI),
    PSPtr = proc_static_ptr(PSI),
    string.format("Dump of proc_dynamic %d", [i(PDI)], Title),

    ProcStaticLink = deep_link(deep_cmd_dump_proc_static(PSPtr), yes(Prefs),
        attr_str([], string.int_to_string(PSI)), link_class_link),
    MainValues =
        [("Proc static:"                - td_l(ProcStaticLink)),
        ("Raw name:"                    - td_s(RawName)),
        ("Module name:"                 - td_s(ModuleName)),
        ("Unqualified refined name:"    - td_s(UnQualRefinedName)),
        ("Qualified refined name:"      - td_s(QualRefinedName))],

    MainRows = list.map(make_labelled_table_row, MainValues),
    MainTable = table(table_class_do_not_box, 2, no, MainRows),
    MainTableItem = display_table(MainTable),

    CallSitesTitle = "Call site dynamics:",
    CallSitesTitleItem = display_heading(CallSitesTitle),

    list.map_foldl(dump_psd_call_site(Prefs), CallSites, CallSitesRowsList,
        counter.init(1), _),
    list.condense(CallSitesRowsList, CallSitesRows),
    CallSitesTable = table(table_class_box_if_pref, 2, no, CallSitesRows),
    CallSitesTableItem = display_table(CallSitesTable),

    (
        MaybeCoveragePoints = yes(CoveragePoints),
        CoveragePointsTitle = "Coverage points:",
        CoveragePointsTitleItem = display_heading(CoveragePointsTitle),

        list.map(format_coverage_point_row, CoveragePoints,
            CoveragePointsRows),
        CoveragePointsTableHeader = table_header([
            table_header_group(table_header_group_single(td_s("Goal Path")),
                table_column_class_no_class, column_do_not_colour),
            table_header_group(table_header_group_single(td_s("Type")),
                table_column_class_no_class, column_do_not_colour),
            table_header_group(table_header_group_single(td_s("Count")),
                table_column_class_number, column_do_not_colour)]),
        CoveragePointsTable = table(table_class_box_if_pref, 3,
            yes(CoveragePointsTableHeader), CoveragePointsRows),
        CoveragePointsTableItem = display_table(CoveragePointsTable),

        CoveragePointsItems =
            [CoveragePointsTitleItem, CoveragePointsTableItem]
    ;
        MaybeCoveragePoints = no,
        CoveragePointsItems = []
    ),

    make_link(link_base(deep_cmd_dynamic_procrep_coverage(PDPtr), yes(Prefs),
            "Dynamic coverage annotated procedure representation"),
        CoverageAnnotatedProcrepItem),
    RelatedReportsList = display_list(list_class_horizontal_except_title,
        yes("Related reports:"), [CoverageAnnotatedProcrepItem]),

    Display = display(yes(Title),
        [MainTableItem, CallSitesTitleItem, CallSitesTableItem] ++
        CoveragePointsItems ++
        [display_paragraph_break, RelatedReportsList]).

:- pred dump_psd_call_site(preferences::in,
    call_site_array_slot::in, list(table_row)::out,
    counter::in, counter::out) is det.

dump_psd_call_site(Prefs, CallSite, Rows, !CallSiteCounter) :-
    counter.allocate(CallSiteNum, !CallSiteCounter),
    CallSiteNumCell = table_cell(td_i(CallSiteNum)),
    (
        CallSite = slot_normal(CSDPtr),
        CSDPtr = call_site_dynamic_ptr(CSDI),
        CSDLink = deep_link(deep_cmd_dump_call_site_dynamic(CSDPtr),
            yes(Prefs), attr_str([], string.int_to_string(CSDI)),
            link_class_link),
        CSDCell = table_cell(td_l(CSDLink)),
        FirstRow = table_row([CallSiteNumCell, CSDCell]),
        Rows = [FirstRow]
    ;
        CallSite = slot_multi(IsZeroed, CSDPtrArray),
        (
            IsZeroed = zeroed,
            IsZeroedStr = "zeroed"
        ;
            IsZeroed = not_zeroed,
            IsZeroedStr = "not_zeroed"
        ),
        array.to_list(CSDPtrArray, CSDPtrs),
        NumCSDPtrs = list.length(CSDPtrs),
        string.format("multi, %d csds (%s)", [i(NumCSDPtrs), s(IsZeroedStr)],
            MultiCellStr),
        MultiCell = table_cell(td_s(MultiCellStr)),
        FirstRow = table_row([CallSiteNumCell, MultiCell]),
        list.map(dump_psd_call_site_multi_entry(Prefs), CSDPtrs, LaterRows),
        Rows = [FirstRow | LaterRows]
    ).

:- pred dump_psd_call_site_multi_entry(preferences::in,
    call_site_dynamic_ptr::in, table_row::out) is det.

dump_psd_call_site_multi_entry(Prefs, CSDPtr, Row) :-
    CSDPtr = call_site_dynamic_ptr(CSDI),
    CSDLink = deep_link(deep_cmd_dump_call_site_dynamic(CSDPtr), yes(Prefs),
        attr_str([], string.int_to_string(CSDI)), link_class_link),
    CSDCell = table_cell(td_l(CSDLink)),
    EmptyCell = table_cell(td_s("")),
    Row = table_row([EmptyCell, CSDCell]).

:- pred format_coverage_point_row(coverage_point::in, table_row::out) is det.

format_coverage_point_row(CoveragePoint, Row) :-
    CoveragePoint = coverage_point(Count, RevGoalPath, CPType),
    GoalPathString = rev_goal_path_to_string(RevGoalPath),
    GoalPathCell = table_cell(td_s(GoalPathString)),
    TypeCell = table_cell(td_s(string(CPType))),
    CountCell = table_cell(td_i(Count)),
    Row = table_row([GoalPathCell, TypeCell, CountCell]).

    % Create a display_report structure for a call_site_static_dump report.
    %
:- pred display_report_call_site_static_dump(preferences::in,
    call_site_static_dump_info::in, display::out) is det.

display_report_call_site_static_dump(Prefs, CallSiteStaticDumpInfo, Display) :-
    CallSiteStaticDumpInfo = call_site_static_dump_info(CSSPtr,
        ContainingPSPtr, SlotNumber, LineNumber, GoalPath, CallSiteKind),
    CSSPtr = call_site_static_ptr(CSSI),
    string.format("Dump of call_site_static %d", [i(CSSI)], Title),
    ContainingPSPtr = proc_static_ptr(ContainingPSI),

    ContainingProcStaticLink = deep_link(
        deep_cmd_dump_proc_static(ContainingPSPtr), yes(Prefs),
        attr_str([], string.int_to_string(ContainingPSI)), link_class_link),

    (
        CallSiteKind = normal_call_and_callee(CalleePSPtr, TypeSpecDesc),
        CalleePSPtr = proc_static_ptr(CalleePSI),
        CalleeDesc0 = "normal, callee " ++ string.int_to_string(CalleePSI),
        ( if TypeSpecDesc = "" then
            CalleeDesc = CalleeDesc0
        else
            CalleeDesc = CalleeDesc0 ++ " typespec " ++ TypeSpecDesc
        ),
        CalleeProcStaticLink = deep_link(
            deep_cmd_dump_proc_static(CalleePSPtr), yes(Prefs),
            attr_str([], CalleeDesc), link_class_link),
        CallSiteKindData = td_l(CalleeProcStaticLink)
    ;
        CallSiteKind = special_call_and_no_callee,
        CallSiteKindData = td_s("special_call")
    ;
        CallSiteKind = higher_order_call_and_no_callee,
        CallSiteKindData = td_s("higher_order_call")
    ;
        CallSiteKind = method_call_and_no_callee,
        CallSiteKindData = td_s("method_call")
    ;
        CallSiteKind = callback_and_no_callee,
        CallSiteKindData = td_s("callback")
    ),

    Values =
        [("Containing proc_static:" - td_l(ContainingProcStaticLink)),
        ("Slot number:"             - td_i(SlotNumber)),
        ("Line number:"             - td_i(LineNumber)),
        ("Goal path:"               - td_s(rev_goal_path_to_string(GoalPath))),
        ("Call site kind:"          - CallSiteKindData)],

    Rows = list.map(make_labelled_table_row, Values),
    Table = table(table_class_do_not_box, 2, no, Rows),
    Display = display(yes(Title), [display_table(Table)]).

    % Create a display_report structure for a call_site_dynamic_dump report.
    %
:- pred display_report_call_site_dynamic_dump(preferences::in,
    call_site_dynamic_dump_info::in, display::out) is det.

display_report_call_site_dynamic_dump(Prefs, CallSiteDynamiccDumpInfo,
        Display) :-
    CallSiteDynamiccDumpInfo = call_site_dynamic_dump_info(CSDPtr,
        CallerPDPtr, CalleePDPtr, RowData),
    CSDPtr = call_site_dynamic_ptr(CSDI),
    string.format("Dump of call_site_dynamic %d", [i(CSDI)], Title),

    CallerPDPtr = proc_dynamic_ptr(CallerPDI),
    CallerProcDynamicLink = deep_link(deep_cmd_dump_proc_dynamic(CallerPDPtr),
        yes(Prefs), attr_str([], string.int_to_string(CallerPDI)),
        link_class_link),

    CalleePDPtr = proc_dynamic_ptr(CalleePDI),
    CalleeProcDynamicLink = deep_link(deep_cmd_dump_proc_dynamic(CalleePDPtr),
        yes(Prefs), attr_str([], string.int_to_string(CalleePDI)),
        link_class_link),

    FirstValues =
        [("Caller proc_dynamic:"    - td_l(CallerProcDynamicLink)),
        ("Callee proc_dynamic:"     - td_l(CalleeProcDynamicLink))],

    FirstRows = list.map(make_labelled_table_row, FirstValues),
    FirstTable = table(table_class_do_not_box, 2, no, FirstRows),

    MakeHeaderData = dummy_order_criteria_header_data,
    maybe_ranked_proc_table_header(Prefs, non_ranked, MakeHeaderData,
        NumColumns, Header),
    maybe_ranked_subject_perf_table_row(Prefs, non_ranked,
        total_columns_meaningful,
        call_site_desc_to_name_path_slot_cell(no, module_qual_always),
        RowData, PerfRow, 1, _),
    PerfTable = table(table_class_box, NumColumns, yes(Header), [PerfRow]),

    Display = display(yes(Title),
        [display_table(FirstTable), display_table(PerfTable)]).

    % Create a display_report structure for a clique_dump report.
    %
:- pred display_report_clique_dump(preferences::in, clique_dump_info::in,
    display::out) is det.

display_report_clique_dump(Prefs, CliqueDumpInfo, Display) :-
    CliqueDumpInfo = clique_dump_info(CliqueDesc, EntryCSDPtr, MemberPDPtrs),
    CliquePtr = CliqueDesc ^ cdesc_clique_ptr,
    CliquePtr = clique_ptr(CliqueNum),
    string.format("Dump of clique %d", [i(CliqueNum)], Title),

    EntryCSDPtr = call_site_dynamic_ptr(EntryCSDI),
    EntryLink = deep_link(deep_cmd_dump_call_site_dynamic(EntryCSDPtr),
        yes(Prefs), attr_str([], string.int_to_string(EntryCSDI)),
        link_class_link),

    list.map_foldl(display_report_clique_dump_member(Prefs), MemberPDPtrs,
        MemberPDLinks, "Member proc_dynamics:", _),

    Values =
        [("Caller call_site_dynamic:" - td_l(EntryLink))] ++
        MemberPDLinks,

    Rows = list.map(make_labelled_table_row, Values),
    Table = table(table_class_do_not_box, 2, no, Rows),

    Display = display(yes(Title), [display_table(Table)]).

:- pred display_report_clique_dump_member(preferences::in,
    proc_dynamic_ptr::in, pair(string, table_data)::out,
    string::in, string::out) is det.

display_report_clique_dump_member(Prefs, PDPtr, Pair, !Label) :-
    PDLabel = !.Label,
    !:Label = "",
    PDPtr = proc_dynamic_ptr(PDI),
    Link = deep_link(deep_cmd_dump_proc_dynamic(PDPtr),
        yes(Prefs), attr_str([], string.int_to_string(PDI)),
        link_class_link),
    PDData = td_l(Link),
    Pair = PDLabel - PDData.

:- pred display_report_call_site_dynamic_var_use(preferences::in,
    call_site_dynamic_var_use_info::in, display::out) is det.

display_report_call_site_dynamic_var_use(_Prefs, CSDVarUseInfo, Display) :-
    CSDVarUseInfo = call_site_dynamic_var_use_info(AverageCost, VarUses),
    AverageCostItem = display_text(
        format("Average Cost: %f", [f(AverageCost)])),

    format_var_uses(VarUses, 1, VarUseRows),
    Header = table_header(
        map((func({Text, Class}) = table_header_group(
                table_header_group_single(td_s(Text)),
                Class, column_do_not_colour)
            ), [{"Argument", table_column_class_ordinal_rank},
                {"Name", table_column_class_no_class},
                {"Type", table_column_class_no_class},
                {"Percent into proc", table_column_class_no_class},
                {"Time into proc (Callseqs)", table_column_class_callseqs}])),
    Table = table(table_class_box_if_pref, 5, yes(Header), VarUseRows),

    Title = "Dump of var use info",
    Display = display(yes(Title), [AverageCostItem, display_table(Table)]).

:- pred format_var_uses(list(var_use_and_name)::in, int::in,
    list(table_row)::out) is det.

format_var_uses([], _, []).
format_var_uses([VarUse | VarUses], RowNum, [Row | Rows]) :-
    HeaderCell = table_cell(td_s(format("Argument: %i", [i(RowNum)]))),
    VarUse = var_use_and_name(Name,
        var_use_info(CostUntilUse, ProcCost, UseType)),
    NameCell = table_cell(td_s(Name)),
    (
        UseType = var_use_production,
        TypeText = "production"
    ;
        UseType = var_use_consumption,
        TypeText = "consumption"
    ;
        UseType = var_use_other,
        TypeText = "other/unknown"
    ),
    TypeCell = table_cell(td_s(TypeText)),
    PercentCell = table_cell(td_p(percent(CostUntilUse / ProcCost))),
    TimeCell = table_cell(td_f(CostUntilUse)),
    Row = table_row([HeaderCell, NameCell, TypeCell, PercentCell, TimeCell]),
    format_var_uses(VarUses, RowNum + 1, Rows).

%---------------------------------------------------------------------------%
%
% Common column header strings.
%

:- func percent_label = table_data.

percent_label = td_s("%").

:- func override_order_criteria_header_data(cmd, preferences, order_criteria,
    string) = table_data.

override_order_criteria_header_data(Cmd, Prefs0, Criteria, Label)
        = TableData :-
    Criteria0 = Prefs0 ^ pref_criteria,
    Prefs = Prefs0 ^ pref_criteria := Criteria,
    ( if Criteria = Criteria0 then
        % Should we display a simple string to indicate that this link
        % leads back to the same page, or would that be confusing?
        Link = deep_link(Cmd, yes(Prefs), attr_str([], Label),
            link_class_link),
        TableData = td_l(Link)
    else
        Link = deep_link(Cmd, yes(Prefs), attr_str([], Label),
            link_class_link),
        TableData = td_l(Link)
    ).

:- func dummy_order_criteria_header_data(preferences, order_criteria, string)
    = table_data.

dummy_order_criteria_header_data(_, _, Label) = td_s(Label).

%---------------------------------------------------------------------------%
%
% The predicates in this section build
%
% (a) the headers of table columns that describe each aspect of performance,
%     and
% (b) the entries in those columns.
%
% Each pair of predicates should follow the exact same logic when selecting
% what columns to display, and in what order.
%

:- type total_columns_meaning
    --->    total_columns_meaningful
    ;       total_columns_not_meaningful.

    % Convert the performance information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row(total_columns_meaning::in, fields::in,
    perf_row_data(Subject)::in, list(table_cell)::out) is det.

perf_table_row(TotalsMeaningful, Fields, RowData, PerfCells) :-
    perf_table_row_ports(Fields, RowData, PortCells),
    perf_table_row_time(TotalsMeaningful, Fields, RowData, TimeCells),
    perf_table_row_callseqs(TotalsMeaningful, Fields, RowData, CallSeqsCells),
    perf_table_row_allocs(TotalsMeaningful, Fields, RowData, AllocCells),
    perf_table_row_memory(TotalsMeaningful, Fields, RowData, MemoryCells),
    PerfCells =
        PortCells ++ TimeCells ++ CallSeqsCells ++
        AllocCells ++ MemoryCells.

    % Build the performance group of table headers
    % according to the preferences.
    %
:- pred perf_table_header(total_columns_meaning::in, preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header(TotalsMeaningful, Prefs, MakeHeaderData, HeaderGroups) :-
    perf_table_header_ports(Prefs, MakeHeaderData, PortHeaderGroups),
    perf_table_header_time(TotalsMeaningful, Prefs, MakeHeaderData,
        TimeHeaderGroups),
    perf_table_header_callseqs(TotalsMeaningful, Prefs, MakeHeaderData,
        CallSeqsHeaderGroups),
    perf_table_header_allocs(TotalsMeaningful, Prefs, MakeHeaderData,
        AllocHeaderGroups),
    perf_table_header_memory(TotalsMeaningful, Prefs, MakeHeaderData,
        MemoryHeaderGroups),
    HeaderGroups =
        PortHeaderGroups ++ TimeHeaderGroups ++ CallSeqsHeaderGroups ++
        AllocHeaderGroups ++ MemoryHeaderGroups.

    % Convert the port information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row_ports(fields::in, perf_row_data(Subject)::in,
    list(table_cell)::out) is det.

perf_table_row_ports(Fields, RowData, PortCells) :-
    PortFields = Fields ^ port_fields,
    (
        PortFields = no_port,
        PortCells = []
    ;
        PortFields = port,
        Calls = RowData ^ perf_row_calls,
        Exits = RowData ^ perf_row_exits,
        Fails = RowData ^ perf_row_fails,
        Redos = RowData ^ perf_row_redos,
        Excps = RowData ^ perf_row_excps,

        CallsCell = table_cell(td_i(Calls)),
        ExitsCell = table_cell(td_i(Exits)),
        FailsCell = table_cell(td_i(Fails)),
        RedosCell = table_cell(td_i(Redos)),
        ExcpsCell = table_cell(td_i(Excps)),

        PortCells = [CallsCell, ExitsCell, FailsCell, RedosCell, ExcpsCell]
    ).

    % Build the ports group of table headers according to the preferences.
    %
:- pred perf_table_header_ports(preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header_ports(Prefs, MakeHeaderData, HeaderGroups) :-
    Fields = Prefs ^ pref_fields,
    PortFields = Fields ^ port_fields,
    (
        PortFields = no_port,
        HeaderGroups = []
    ;
        PortFields = port,

        CallsCrit = by_cost(cost_calls, self, overall),
        RedosCrit = by_cost(cost_redos, self, overall),

        CallsData = MakeHeaderData(Prefs, CallsCrit, "Calls"),
        ExitsData = td_s("Exits"),
        FailsData = td_s("Fails"),
        RedosData = MakeHeaderData(Prefs, RedosCrit, "Redos"),
        ExcpsData = td_s("Excps"),

        SubHeaders = [CallsData, ExitsData, FailsData, RedosData, ExcpsData],

        Title = "Port counts",
        HeaderGroup = make_multi_table_header_group(Title, SubHeaders,
            table_column_class_port_counts, column_colour_if_pref),
        HeaderGroups = [HeaderGroup]
    ).

    % Convert the time information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row_time(total_columns_meaning::in, fields::in,
    perf_row_data(Subject)::in, list(table_cell)::out) is det.

perf_table_row_time(TotalsMeaningful, Fields, RowData, TimeCells) :-
    TimeFields = Fields ^ time_fields,
    (
        TimeFields = no_time,
        TimeCells = []
    ;
        ( TimeFields = ticks
        ; TimeFields = time
        ; TimeFields = ticks_and_time
        ; TimeFields = time_and_percall
        ; TimeFields = ticks_and_time_and_percall
        ),

        Self = RowData ^ perf_row_self,
        SelfTicks =             Self ^ perf_row_ticks,
        SelfTime =              Self ^ perf_row_time,
        SelfTimePercent =       Self ^ perf_row_time_percent,
        SelfTimePerCall =       Self ^ perf_row_time_percall,
        SelfTicksCell =         table_cell(td_i(SelfTicks)),
        SelfTimeCell =          table_cell(td_t(SelfTime)),
        SelfTimePercentCell =   table_cell(td_p(SelfTimePercent)),
        SelfTimePerCallCell =   table_cell(td_t(SelfTimePerCall)),
        (
            TotalsMeaningful = total_columns_not_meaningful,
            (
                TimeFields = ticks,
                TimeCells =
                    [SelfTicksCell, SelfTimePercentCell]
            ;
                TimeFields = time,
                TimeCells =
                    [SelfTimeCell, SelfTimePercentCell]
            ;
                TimeFields = ticks_and_time,
                TotalsMeaningful = total_columns_not_meaningful,
                TimeCells =
                    [SelfTicksCell, SelfTimeCell, SelfTimePercentCell]
            ;
                TimeFields = time_and_percall,
                TimeCells =
                    [SelfTimeCell, SelfTimePercentCell, SelfTimePerCallCell]
            ;
                TimeFields = ticks_and_time_and_percall,
                TimeCells =
                    [SelfTicksCell, SelfTimeCell,
                    SelfTimePercentCell, SelfTimePerCallCell]
            )
        ;
            TotalsMeaningful = total_columns_meaningful,
            MaybeTotal = RowData ^ perf_row_maybe_total,
            (
                MaybeTotal = yes(Total)
            ;
                MaybeTotal = no,
                unexpected($module, $pred, "no total")
            ),
            TotalTicks =            Total ^ perf_row_ticks,
            TotalTime =             Total ^ perf_row_time,
            TotalTimePercent =      Total ^ perf_row_time_percent,
            TotalTimePerCall =      Total ^ perf_row_time_percall,
            TotalTicksCell =        table_cell(td_i(TotalTicks)),
            TotalTimeCell =         table_cell(td_t(TotalTime)),
            TotalTimePercentCell =  table_cell(td_p(TotalTimePercent)),
            TotalTimePerCallCell =  table_cell(td_t(TotalTimePerCall)),
            (
                TimeFields = ticks,
                TimeCells =
                    [SelfTicksCell, SelfTimePercentCell,
                    TotalTicksCell, TotalTimePercentCell]
            ;
                TimeFields = time,
                TotalsMeaningful = total_columns_meaningful,
                TimeCells =
                    [SelfTimeCell, SelfTimePercentCell,
                    TotalTimeCell, TotalTimePercentCell]
            ;
                TimeFields = ticks_and_time,
                TimeCells =
                    [SelfTicksCell, SelfTimeCell, SelfTimePercentCell,
                    TotalTicksCell, TotalTimeCell, TotalTimePercentCell]
            ;
                TimeFields = time_and_percall,
                TimeCells =
                    [SelfTimeCell, SelfTimePercentCell, SelfTimePerCallCell,
                    TotalTimeCell, TotalTimePercentCell, TotalTimePerCallCell]
            ;
                TimeFields = ticks_and_time_and_percall,
                TimeCells =
                    [SelfTicksCell, SelfTimeCell,
                    SelfTimePercentCell, SelfTimePerCallCell,
                    TotalTicksCell, TotalTimeCell,
                    TotalTimePercentCell, TotalTimePerCallCell]
            )
        )
    ).

    % Build the time group of table headers according to the preferences.
    %
:- pred perf_table_header_time(total_columns_meaning::in, preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header_time(TotalsMeaningful, Prefs, MakeHeaderData,
        HeaderGroups) :-
    Fields = Prefs ^ pref_fields,
    TimeFields = Fields ^ time_fields,

    (
        TimeFields = no_time,
        HeaderGroups = []
    ;
        ( TimeFields = ticks
        ; TimeFields = time
        ; TimeFields = ticks_and_time
        ; TimeFields = time_and_percall
        ; TimeFields = ticks_and_time_and_percall
        ),

        % Ordering by clock ticks is equivalent to ordering by time.
        SelfTicksCrit =         by_cost(cost_time, self, overall),
        SelfTimeCrit =          by_cost(cost_time, self, overall),
        SelfTimePerCallCrit =   by_cost(cost_time, self, per_call),
        TotalTicksCrit =        by_cost(cost_time, self_and_desc, overall),
        TotalTimeCrit =         by_cost(cost_time, self_and_desc, overall),
        TotalTimePerCallCrit =  by_cost(cost_time, self_and_desc, per_call),

        SelfTicksData =         MakeHeaderData(Prefs, SelfTicksCrit,
                                    "Self"),
        SelfTimeData =          MakeHeaderData(Prefs, SelfTimeCrit,
                                    "Time"),
        SelfTimePercentData =   percent_label,
        SelfTimePerCallData =   MakeHeaderData(Prefs, SelfTimePerCallCrit,
                                    "/call"),
        TotalTicksData =        MakeHeaderData(Prefs,
                                    TotalTicksCrit, "Total"),
        TotalTimeData =         MakeHeaderData(Prefs, TotalTimeCrit,
                                    "Time"),
        TotalTimePercentData =  percent_label,
        TotalTimePerCallData =  MakeHeaderData(Prefs, TotalTimePerCallCrit,
                                    "/call"),

        (
            TotalsMeaningful = total_columns_not_meaningful,
            (
                TimeFields = ticks,
                Title = "Clock ticks",
                SubHeaders =
                    [SelfTicksData, SelfTimePercentData]
            ;
                TimeFields = time,
                Title = "Time",
                SubHeaders =
                    [SelfTimeData, SelfTimePercentData]
            ;
                TimeFields = ticks_and_time,
                Title = "Clock ticks and times",
                SubHeaders =
                    [SelfTicksData, SelfTimeData, SelfTimePercentData]
            ;
                TimeFields = time_and_percall,
                Title = "Time",
                SubHeaders =
                    [SelfTimeData, SelfTimePercentData, SelfTimePerCallData]
            ;
                TimeFields = ticks_and_time_and_percall,
                Title = "Clock ticks and times",
                SubHeaders =
                    [SelfTicksData, SelfTimeData,
                    SelfTimePercentData, SelfTimePerCallData]
            )
        ;
            TotalsMeaningful = total_columns_meaningful,
            (
                TimeFields = ticks,
                Title = "Clock ticks",
                SubHeaders =
                    [SelfTicksData, SelfTimePercentData,
                    TotalTicksData, TotalTimePercentData]
            ;
                TimeFields = time,
                Title = "Time",
                SubHeaders =
                    [SelfTimeData, SelfTimePercentData,
                    TotalTimeData, TotalTimePercentData]
            ;
                TimeFields = ticks_and_time,
                Title = "Clock ticks and times",
                SubHeaders =
                    [SelfTicksData, SelfTimeData, SelfTimePercentData,
                    TotalTicksData, TotalTimeData, TotalTimePercentData]
            ;
                TimeFields = time_and_percall,
                Title = "Time",
                SubHeaders =
                    [SelfTimeData, SelfTimePercentData, SelfTimePerCallData,
                    TotalTimeData, TotalTimePercentData, TotalTimePerCallData]
            ;
                TimeFields = ticks_and_time_and_percall,
                Title = "Clock ticks and times",
                SubHeaders =
                    [SelfTicksData, SelfTimeData,
                    SelfTimePercentData, SelfTimePerCallData,
                    TotalTicksData, TotalTimeData,
                    TotalTimePercentData, TotalTimePerCallData]
            )
        ),

        HeaderGroup = make_multi_table_header_group(Title, SubHeaders,
            table_column_class_ticks_and_times, column_colour_if_pref),
        HeaderGroups = [HeaderGroup]
    ).

    % Convert the callseqs information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row_callseqs(total_columns_meaning::in, fields::in,
    perf_row_data(Subject)::in, list(table_cell)::out) is det.

perf_table_row_callseqs(TotalsMeaningful, Fields, RowData, CallSeqsCells) :-
    CallSeqsFields = Fields ^ callseqs_fields,
    (
        CallSeqsFields = no_callseqs,
        CallSeqsCells = []
    ;
        ( CallSeqsFields = callseqs
        ; CallSeqsFields = callseqs_and_percall
        ),
        Self = RowData ^ perf_row_self,
        SelfCallSeqs =              Self ^ perf_row_callseqs,
        SelfCallSeqsPercent =       Self ^ perf_row_callseqs_percent,
        SelfCallSeqsPerCall =       Self ^ perf_row_callseqs_percall,
        SelfCallSeqsCell =          table_cell(td_i(SelfCallSeqs)),
        SelfCallSeqsPercentCell =   table_cell(td_p(SelfCallSeqsPercent)),
        SelfCallSeqsPerCallCell =   table_cell(td_f(SelfCallSeqsPerCall)),
        (
            TotalsMeaningful = total_columns_not_meaningful,
            (
                CallSeqsFields = callseqs,
                CallSeqsCells =
                    [SelfCallSeqsCell, SelfCallSeqsPercentCell]
            ;
                CallSeqsFields = callseqs_and_percall,
                CallSeqsCells =
                    [SelfCallSeqsCell, SelfCallSeqsPercentCell,
                    SelfCallSeqsPerCallCell]
            )
        ;
            TotalsMeaningful = total_columns_meaningful,
            MaybeTotal = RowData ^ perf_row_maybe_total,
            (
                MaybeTotal = yes(Total)
            ;
                MaybeTotal = no,
                unexpected($module, $pred, "no total")
            ),
            TotalCallSeqs =             Total ^ perf_row_callseqs,
            TotalCallSeqsPercent =      Total ^ perf_row_callseqs_percent,
            TotalCallSeqsPerCall =      Total ^ perf_row_callseqs_percall,
            TotalCallSeqsCell =         table_cell(td_i(TotalCallSeqs)),
            TotalCallSeqsPercentCell =  table_cell(td_p(TotalCallSeqsPercent)),
            TotalCallSeqsPerCallCell =  table_cell(td_f(TotalCallSeqsPerCall)),
            (
                CallSeqsFields = callseqs,
                CallSeqsCells =
                    [SelfCallSeqsCell, SelfCallSeqsPercentCell,
                    TotalCallSeqsCell, TotalCallSeqsPercentCell]
            ;
                CallSeqsFields = callseqs_and_percall,
                CallSeqsCells =
                    [SelfCallSeqsCell, SelfCallSeqsPercentCell,
                    SelfCallSeqsPerCallCell,
                    TotalCallSeqsCell, TotalCallSeqsPercentCell,
                    TotalCallSeqsPerCallCell]
            )
        )
    ).

    % Build the callseqs group of table headers according to the preferences.
    %
:- pred perf_table_header_callseqs(total_columns_meaning::in, preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header_callseqs(TotalsMeaningful, Prefs, MakeHeaderData,
        HeaderGroups) :-
    Fields = Prefs ^ pref_fields,
    CallSeqsFields = Fields ^ callseqs_fields,
    (
        CallSeqsFields = no_callseqs,
        HeaderGroups = []
    ;
        ( CallSeqsFields = callseqs
        ; CallSeqsFields = callseqs_and_percall
        ),

        SelfCrit =          by_cost(cost_callseqs, self, overall),
        SelfPerCallCrit =   by_cost(cost_callseqs, self, per_call),
        TotalCrit =         by_cost(cost_callseqs, self_and_desc, overall),
        TotalPerCallCrit =  by_cost(cost_callseqs, self_and_desc, per_call),

        SelfData =          MakeHeaderData(Prefs, SelfCrit, "Self"),
        SelfPercentData =   percent_label,
        SelfPerCallData =   MakeHeaderData(Prefs, SelfPerCallCrit, "/call"),
        TotalData =         MakeHeaderData(Prefs, TotalCrit, "Total"),
        TotalPercentData =  percent_label,
        TotalPerCallData =  MakeHeaderData(Prefs, TotalPerCallCrit, "/call"),

        (
            TotalsMeaningful = total_columns_not_meaningful,
            (
                CallSeqsFields = callseqs,
                SubHeaders =
                    [SelfData, SelfPercentData]
            ;
                CallSeqsFields = callseqs_and_percall,
                SubHeaders =
                    [SelfData, SelfPercentData, SelfPerCallData]
            )
        ;
            TotalsMeaningful = total_columns_meaningful,
            (
                CallSeqsFields = callseqs,
                SubHeaders =
                    [SelfData, SelfPercentData,
                    TotalData, TotalPercentData]
            ;
                CallSeqsFields = callseqs_and_percall,
                SubHeaders =
                    [SelfData, SelfPercentData, SelfPerCallData,
                    TotalData, TotalPercentData, TotalPerCallData]
            )
        ),
        Title = "Call sequence numbers",
        HeaderGroup = make_multi_table_header_group(Title, SubHeaders,
            table_column_class_callseqs, column_colour_if_pref),
        HeaderGroups = [HeaderGroup]
    ).

    % Convert the allocation information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row_allocs(total_columns_meaning::in, fields::in,
    perf_row_data(Subject)::in, list(table_cell)::out) is det.

perf_table_row_allocs(TotalsMeaningful, Fields, RowData, AllocCells) :-
    AllocFields = Fields ^ alloc_fields,
    (
        AllocFields = no_alloc,
        AllocCells = []
    ;
        ( AllocFields = alloc
        ; AllocFields = alloc_and_percall
        ),

        Self = RowData ^ perf_row_self,
        SelfAllocs =                Self ^ perf_row_allocs,
        SelfAllocsPercent =         Self ^ perf_row_allocs_percent,
        SelfAllocsPerCall =         Self ^ perf_row_allocs_percall,
        SelfAllocsCell =            table_cell(td_i(SelfAllocs)),
        SelfAllocsPercentCell =     table_cell(td_p(SelfAllocsPercent)),
        SelfAllocsPerCallCell =     table_cell(td_f(SelfAllocsPerCall)),
        (
            TotalsMeaningful = total_columns_not_meaningful,
            (
                AllocFields = alloc,
                AllocCells =
                    [SelfAllocsCell, SelfAllocsPercentCell]
            ;
                AllocFields = alloc_and_percall,
                AllocCells =
                    [SelfAllocsCell, SelfAllocsPercentCell,
                    SelfAllocsPerCallCell]
            )
        ;
            TotalsMeaningful = total_columns_meaningful,
            MaybeTotal = RowData ^ perf_row_maybe_total,
            (
                MaybeTotal = yes(Total)
            ;
                MaybeTotal = no,
                unexpected($module, $pred, "no total")
            ),
            TotalAllocs =               Total ^ perf_row_allocs,
            TotalAllocsPercent =        Total ^ perf_row_allocs_percent,
            TotalAllocsPerCall =        Total ^ perf_row_allocs_percall,
            TotalAllocsCell =           table_cell(td_i(TotalAllocs)),
            TotalAllocsPercentCell =    table_cell(td_p(TotalAllocsPercent)),
            TotalAllocsPerCallCell =    table_cell(td_f(TotalAllocsPerCall)),
            (
                AllocFields = alloc,
                AllocCells =
                    [SelfAllocsCell, SelfAllocsPercentCell,
                    TotalAllocsCell, TotalAllocsPercentCell]
            ;
                AllocFields = alloc_and_percall,
                AllocCells =
                    [SelfAllocsCell, SelfAllocsPercentCell,
                    SelfAllocsPerCallCell,
                    TotalAllocsCell, TotalAllocsPercentCell,
                    TotalAllocsPerCallCell]
            )
        )
    ).

    % Build the allocs group of table headers according to the preferences.
    %
:- pred perf_table_header_allocs(total_columns_meaning::in, preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header_allocs(TotalsMeaningful, Prefs, MakeHeaderData,
        HeaderGroups) :-
    Fields = Prefs ^ pref_fields,
    AllocFields = Fields ^ alloc_fields,
    (
        AllocFields = no_alloc,
        HeaderGroups = []
    ;
        ( AllocFields = alloc
        ; AllocFields = alloc_and_percall
        ),

        SelfCrit =          by_cost(cost_allocs, self, overall),
        SelfPerCallCrit =   by_cost(cost_allocs, self, per_call),
        TotalCrit =         by_cost(cost_allocs, self_and_desc, overall),
        TotalPerCallCrit =  by_cost(cost_allocs, self_and_desc, per_call),

        SelfData =          MakeHeaderData(Prefs, SelfCrit, "Self"),
        SelfPercentData =   percent_label,
        SelfPerCallData =   MakeHeaderData(Prefs, SelfPerCallCrit, "/call"),
        TotalData =         MakeHeaderData(Prefs, TotalCrit, "Total"),
        TotalPercentData =  percent_label,
        TotalPerCallData =  MakeHeaderData(Prefs, TotalPerCallCrit, "/call"),

        (
            AllocFields = alloc,
            (
                TotalsMeaningful = total_columns_not_meaningful,
                SubHeaders =
                    [SelfData, SelfPercentData]
            ;
                TotalsMeaningful = total_columns_meaningful,
                SubHeaders =
                    [SelfData, SelfPercentData,
                    TotalData, TotalPercentData]
            )
        ;
            AllocFields = alloc_and_percall,
            (
                TotalsMeaningful = total_columns_not_meaningful,
                SubHeaders =
                    [SelfData, SelfPercentData, SelfPerCallData]
            ;
                TotalsMeaningful = total_columns_meaningful,
                SubHeaders =
                    [SelfData, SelfPercentData, SelfPerCallData,
                    TotalData, TotalPercentData, TotalPerCallData]
            )
        ),

        Title = "Memory allocations",
        HeaderGroup = make_multi_table_header_group(Title, SubHeaders,
            table_column_class_allocations, column_colour_if_pref),
        HeaderGroups = [HeaderGroup]
    ).

    % Convert the memory information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row_memory(total_columns_meaning::in, fields::in,
    perf_row_data(Subject)::in, list(table_cell)::out) is det.

perf_table_row_memory(TotalsMeaningful, Fields, RowData, MemoryCells) :-
    MemoryFields = Fields ^ memory_fields,
    (
        MemoryFields = no_memory,
        MemoryCells = []
    ;
        ( MemoryFields = memory(Units)
        ; MemoryFields = memory_and_percall(Units)
        ),

        Self = RowData ^ perf_row_self,
        SelfMem =               Self ^ perf_row_mem,
        SelfMemPerCall =        Self ^ perf_row_mem_percall,
        SelfMemPercent =        Self ^ perf_row_mem_percent,
        SelfMemCell =           table_cell(td_m(SelfMem, Units, 0)),
        SelfMemPerCallCell =    table_cell(td_m(SelfMemPerCall, Units, 2)),
        SelfMemPercentCell =    table_cell(td_p(SelfMemPercent)),
        (
            TotalsMeaningful = total_columns_not_meaningful,
            (
                MemoryFields = memory(_),
                MemoryCells =
                    [SelfMemCell, SelfMemPercentCell]
            ;
                MemoryFields = memory_and_percall(_),
                MemoryCells =
                    [SelfMemCell, SelfMemPercentCell, SelfMemPerCallCell]
            )
        ;
            TotalsMeaningful = total_columns_meaningful,
            MaybeTotal = RowData ^ perf_row_maybe_total,
            (
                MaybeTotal = yes(Total)
            ;
                MaybeTotal = no,
                unexpected($module, $pred, "no total")
            ),
            TotalMem =            Total ^ perf_row_mem,
            TotalMemPerCall =     Total ^ perf_row_mem_percall,
            TotalMemPercent =     Total ^ perf_row_mem_percent,
            TotalMemCell =        table_cell(td_m(TotalMem, Units, 0)),
            TotalMemPerCallCell = table_cell(td_m(TotalMemPerCall, Units, 2)),
            TotalMemPercentCell = table_cell(td_p(TotalMemPercent)),
            (
                MemoryFields = memory(_),
                MemoryCells =
                    [SelfMemCell, SelfMemPercentCell,
                    TotalMemCell, TotalMemPercentCell]
            ;
                MemoryFields = memory_and_percall(_),
                MemoryCells =
                    [SelfMemCell, SelfMemPercentCell, SelfMemPerCallCell,
                    TotalMemCell, TotalMemPercentCell, TotalMemPerCallCell]
            )
        )
    ).

    % Build the memory group of table headers according to the preferences.
    %
:- pred perf_table_header_memory(total_columns_meaning::in, preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header_memory(TotalsMeaningful, Prefs, MakeHeaderData,
        HeaderGroups) :-
    Fields = Prefs ^ pref_fields,
    MemoryFields = Fields ^ memory_fields,
    (
        MemoryFields = no_memory,
        HeaderGroups = []
    ;
        ( MemoryFields = memory(_)
        ; MemoryFields = memory_and_percall(_)
        ),

        SelfCrit =          by_cost(cost_words, self, overall),
        SelfPerCallCrit =   by_cost(cost_words, self, per_call),
        TotalCrit =         by_cost(cost_words, self_and_desc, overall),
        TotalPerCallCrit =  by_cost(cost_words, self_and_desc, per_call),

        SelfData =          MakeHeaderData(Prefs, SelfCrit, "Self"),
        SelfPercentData =   percent_label,
        SelfPerCallData =   MakeHeaderData(Prefs, SelfPerCallCrit, "/call"),
        TotalData =         MakeHeaderData(Prefs, TotalCrit, "Total"),
        TotalPercentData =  percent_label,
        TotalPerCallData =  MakeHeaderData(Prefs, TotalPerCallCrit, "/call"),

        (
            TotalsMeaningful = total_columns_not_meaningful,
            (
                MemoryFields = memory(Units),
                SubHeaders =
                    [SelfData, SelfPercentData]
            ;
                MemoryFields = memory_and_percall(Units),
                SubHeaders =
                    [SelfData, SelfPercentData, SelfPerCallData]
            )
        ;
            TotalsMeaningful = total_columns_meaningful,
            (
                MemoryFields = memory(Units),
                SubHeaders =
                    [SelfData, SelfPercentData,
                    TotalData, TotalPercentData]
            ;
                MemoryFields = memory_and_percall(Units),
                SubHeaders =
                    [SelfData, SelfPercentData, SelfPerCallData,
                    TotalData, TotalPercentData, TotalPerCallData]
            )
        ),
        (
            Units = units_words,
            Title = "Memory words"
        ;
            Units = units_bytes,
            Title = "Memory bytes"
        ),
        HeaderGroup = make_multi_table_header_group(Title, SubHeaders,
            table_column_class_memory, column_colour_if_pref),
        HeaderGroups = [HeaderGroup]
    ).

%---------------------------------------------------------------------------%

    % Describes whether a table should be ranked or not,  This means that each
    % item has an ordinal number associated with it in an initial column
    % labeled "Rank".
    %
:- type ranked
    --->    ranked
    ;       non_ranked.

    % Build a header for a table of procedures.
    %
:- pred maybe_ranked_proc_table_header(preferences::in, ranked::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    int::out, table_header::out) is det.

maybe_ranked_proc_table_header(Prefs, Ranked, MakeHeaderData, NumColumns,
        Header) :-
    (
        Ranked = non_ranked,
        RankedHeaderGroups = []
    ;
        Ranked = ranked,
        RankedHeaderGroup =
            make_single_table_header_group(td_s("Rank"),
                table_column_class_ordinal_rank, column_do_not_colour),
        RankedHeaderGroups = [RankedHeaderGroup]
    ),

    ProcHeaderGroup =
        make_single_table_header_group(td_s("Procedure"),
            table_column_class_proc, column_do_not_colour),
    ProcHeaderGroups = [ProcHeaderGroup],

    perf_table_header(total_columns_meaningful, Prefs, MakeHeaderData,
        PerfHeaderGroups),

    AllHeaderGroups =
        RankedHeaderGroups ++ ProcHeaderGroups ++ PerfHeaderGroups,
    header_groups_to_header(AllHeaderGroups, NumColumns, Header).

    % Convert row data of procedures from the deep profiler into a table row
    % according to the preferences.
    %
:- pred maybe_ranked_subject_perf_table_row(preferences::in, ranked::in,
    total_columns_meaning::in, (func(preferences, Subject) = table_cell)::in,
    perf_row_data(Subject)::in, table_row::out, int::in, int::out) is det.

maybe_ranked_subject_perf_table_row(Prefs, Ranked, TotalsMeaningful,
        SubjectCellFunc, RowData, Row, Rank, Rank + 1) :-
    (
        Ranked = non_ranked,
        RankCells = []
    ;
        Ranked = ranked,
        RankCells = [table_cell(td_i(Rank))]
    ),

    % The name of the procedure,
    SubjectCells = [SubjectCellFunc(Prefs, RowData ^ perf_row_subject)],

    Fields = Prefs ^ pref_fields,
    perf_table_row(TotalsMeaningful, Fields, RowData, PerfCells),

    Cells = RankCells ++ SubjectCells ++ PerfCells,
    Row = table_row(Cells).

%---------------------------------------------------------------------------%
%
% Some utility procedures.
%

:- func call_site_desc_source_cell(call_site_desc) = table_cell.

call_site_desc_source_cell(CallSiteDesc) = Cell :-
    FileName = CallSiteDesc ^ csdesc_file_name,
    LineNumber = CallSiteDesc ^ csdesc_line_number,
    Context = string.format("%s:%d", [s(FileName), i(LineNumber)]),
    Cell = table_cell(td_s(Context)).

:- func call_site_desc_clique_proc_cell(maybe(string), module_qual,
    preferences, ancestor_desc) = table_cell.

call_site_desc_clique_proc_cell(MaybeCurModuleName, ModuleQual,
        Prefs, AncestorDesc) = Cell :-
    AncestorDesc = ancestor_desc(CallerCliquePtr, _CalleeCliquePtr,
        _CalleeProcDesc, CallSiteDesc),
    ContainingProcName =
        call_site_desc_get_caller_refined_id(MaybeCurModuleName, ModuleQual,
            CallSiteDesc),
    Cmd = deep_cmd_clique(CallerCliquePtr),
    Link = deep_link(Cmd, yes(Prefs), attr_str([], ContainingProcName),
        link_class_link),
    Cell = table_cell(td_l(Link)).

%---------------------------------------------------------------------------%
%
% The basic predicates for creating the controls at the bottoms of pages.
%

:- pred make_top_procs_cmd_items(preferences::in, bool::in, display_limit::in,
    cost_kind::in, include_descendants::in, measurement_scope::in,
    maybe(string)::in,
    assoc_list(string,
        (pred(display_limit, cost_kind, include_descendants,
            measurement_scope, cmd)))::
        in(list_skel(pair(ground, (pred(in, in, in, in, out) is det)))),
    display_item::out) is det.

make_top_procs_cmd_items(Prefs, SetPrefs, DisplayLimit, CostKind, InclDesc,
        Scope, MaybeLabel, LabelsCmdMakers, Item) :-
    list.map(
        make_top_procs_cmd_item(Prefs, SetPrefs, DisplayLimit, CostKind,
            InclDesc, Scope),
        LabelsCmdMakers, ControlItemLists),
    list.condense(ControlItemLists, ControlItems),
    Item = display_list(list_class_horizontal, MaybeLabel, ControlItems).

:- pred make_top_procs_cmd_item(preferences::in, bool::in, display_limit::in,
    cost_kind::in, include_descendants::in, measurement_scope::in,
    pair(string,
        (pred(display_limit, cost_kind, include_descendants, measurement_scope,
            cmd)))::
        in(pair(ground, (pred(in, in, in, in, out) is det))),
    list(display_item)::out) is det.

make_top_procs_cmd_item(Prefs0, SetPrefs, DisplayLimit, CostKind, InclDesc,
        Scope, Label - CmdMaker, Items) :-
    Cmd0 = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope),
    CmdMaker(DisplayLimit, CostKind, InclDesc, Scope, Cmd),
    ( if Cmd = Cmd0 then
        Items = []
        % We could use this code instead.
        % CurLabel = Label ++ " (current setting)",
        % PseudoLink = pseudo_link(CurLabel, link_class_control),
        % Item = display_pseudo_link(PseudoLink),
        % Items = [Item]
    else
        (
            SetPrefs = no,
            Prefs = Prefs0
        ;
            SetPrefs = yes,
            % By default, sort the top procedures by the same criteria
            % by which they were selected. Users can override this choice
            % later if they wish.
            ( if
                Cmd = deep_cmd_top_procs(_, CmdCostKind, CmdInclDesc,
                    CmdScope)
            then
                Prefs = Prefs0 ^ pref_criteria :=
                    by_cost(CmdCostKind, CmdInclDesc, CmdScope)
            else
                Prefs = Prefs0
            )
        ),
        Link = deep_link(Cmd, yes(Prefs), attr_str([], Label),
            link_class_control),
        Item = display_link(Link),
        Items = [Item]
    ).

:- pred make_first_proc_callers_cmds_item(preferences::in, cmd::in,
    proc_static_ptr::in, caller_groups::in, int::in, contour_exclusion::in,
    maybe(string)::in,
    assoc_list(string,
        (pred(proc_static_ptr, caller_groups, int, contour_exclusion, cmd)))::
        in(list_skel(pair(ground, (pred(in, in, in, in, out) is det)))),
    display_item::out) is det.

make_first_proc_callers_cmds_item(Prefs, Cmd, PSPtr, CallerGroups, BunchSize,
        ContourExcl, MaybeLabel, LabelsCmdMakers, Item) :-
    list.map(
        make_first_proc_callers_cmd_item(Prefs, Cmd, PSPtr, CallerGroups,
            BunchSize, ContourExcl),
        LabelsCmdMakers, ControlItemLists),
    list.condense(ControlItemLists, ControlItems),
    Item = display_list(list_class_horizontal, MaybeLabel, ControlItems).

:- pred make_first_proc_callers_cmd_item(preferences::in, cmd::in,
    proc_static_ptr::in, caller_groups::in, int::in, contour_exclusion::in,
    pair(string,
        (pred(proc_static_ptr, caller_groups, int, contour_exclusion, cmd)))::
        in(pair(ground, (pred(in, in, in, in, out) is det))),
    list(display_item)::out) is det.

make_first_proc_callers_cmd_item(Prefs, Cmd0, PSPtr, CallerGroups, BunchSize,
        ContourExcl, Label - CmdMaker, Items) :-
    % When we just to a different kind of grouping or toggle contour exclusion,
    % we always go to the first bunch of the resulting rows.
    CmdMaker(PSPtr, CallerGroups, BunchSize, ContourExcl, Cmd),
    ( if Cmd = Cmd0 then
        Items = []
        % We could use this code instead.
        % CurLabel = Label ++ " (current setting)",
        % PseudoLink = pseudo_link(CurLabel, link_class_control),
        % Item = display_pseudo_link(PseudoLink),
        % Items = [Item]
    else
        Link = deep_link(Cmd, yes(Prefs), attr_str([], Label),
            link_class_control),
        Item = display_link(Link),
        Items = [Item]
    ).

:- pred make_prefs_controls_item(preferences::in, cmd::in, maybe(string)::in,
    assoc_list(string, (pred(preferences, preferences)))::
        in(list_skel(pair(ground, (pred(in, out) is det)))),
    display_item::out) is det.

make_prefs_controls_item(Prefs0, Cmd, MaybeLabel, LabelsPrefMakers, Item) :-
    list.map(make_prefs_control_item(Prefs0, Cmd), LabelsPrefMakers,
        ControlItemLists),
    list.condense(ControlItemLists, ControlItems),
    Item = display_list(list_class_horizontal, MaybeLabel, ControlItems).

:- pred make_prefs_control_item(preferences::in, cmd::in,
    pair(string, (pred(preferences, preferences)))::
        in(pair(ground, (pred(in, out) is det))),
    list(display_item)::out) is det.

make_prefs_control_item(Prefs0, Cmd, Label - PrefMaker, Items) :-
    PrefMaker(Prefs0, Prefs),
    ( if Prefs = Prefs0 then
        Items = []
        % We could use this code instead.
        % CurLabel = Label ++ " (current setting)",
        % PseudoLink = pseudo_link(CurLabel, link_class_control),
        % Item = display_pseudo_link(PseudoLink),
        % Items = [Item]
    else
        Link = deep_link(Cmd, yes(Prefs), attr_str([], Label),
            link_class_control),
        Item = display_link(Link),
        Items = [Item]
    ).

%---------------------------------------------------------------------------%
%
% Control how the top_procs command selects what is "top".
%

:- func top_procs_controls(preferences, display_limit, cost_kind,
    include_descendants, measurement_scope) = display_item.

top_procs_controls(Prefs, DisplayLimit, CostKind, InclDesc, Scope) =
        ControlsItem :-
    make_top_procs_cmd_items(Prefs, no, DisplayLimit, CostKind, InclDesc,
        Scope, no, top_procs_limit_toggles, LimitControls),
    make_top_procs_cmd_items(Prefs, yes, DisplayLimit, CostKind, InclDesc,
        Scope, no, top_procs_sort_toggles, SortControls),
    make_top_procs_cmd_items(Prefs, no, DisplayLimit, CostKind, InclDesc,
        Scope, no, top_procs_incl_desc_toggles, InclDescControls),
    make_top_procs_cmd_items(Prefs, no, DisplayLimit, CostKind, InclDesc,
        Scope, no, top_procs_scope_toggles, ScopeControls),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle sorting criteria:"),
        [LimitControls, SortControls, InclDescControls, ScopeControls]).

:- func top_procs_limit_toggles =
    (assoc_list(string,
        (pred(display_limit, cost_kind, include_descendants, measurement_scope,
            cmd)))::
        out(list_skel(pair(ground, (pred(in, in, in, in, out) is det)))))
    is det.

top_procs_limit_toggles = [
    "Top 10" -
        set_top_procs_display_limit(rank_range(1, 10)),
    "Top 100" -
        set_top_procs_display_limit(rank_range(1, 100)),
    "Top 1000" -
        set_top_procs_display_limit(rank_range(1, 1000)),
    "Top 0.01%" -
        set_top_procs_display_limit(threshold_percent(0.01)),
    "Top 0.1%" -
        set_top_procs_display_limit(threshold_percent(0.1)),
    "Top 1%" -
        set_top_procs_display_limit(threshold_percent(1.0))
].

:- func top_procs_sort_toggles =
    (assoc_list(string,
        (pred(display_limit, cost_kind, include_descendants, measurement_scope,
            cmd)))::
        out(list_skel(pair(ground, (pred(in, in, in, in, out) is det)))))
    is det.

top_procs_sort_toggles = [
    "Sort by calls" -
        set_top_procs_sort_criteria(cost_calls),
    "Sort by redos" -
        set_top_procs_sort_criteria(cost_redos),
    "Sort by time" -
        set_top_procs_sort_criteria(cost_time),
    "Sort by call sequence numbers" -
        set_top_procs_sort_criteria(cost_callseqs),
    "Sort by allocs" -
        set_top_procs_sort_criteria(cost_allocs),
    "Sort by words" -
        set_top_procs_sort_criteria(cost_words)
].

:- func top_procs_incl_desc_toggles =
    (assoc_list(string,
        (pred(display_limit, cost_kind, include_descendants, measurement_scope,
            cmd)))::
        out(list_skel(pair(ground, (pred(in, in, in, in, out) is det)))))
    is det.

top_procs_incl_desc_toggles = [
    "Exclude descendants" -
        set_top_procs_incl_desc(self),
    "Include descendants" -
        set_top_procs_incl_desc(self_and_desc)
].

:- func top_procs_scope_toggles =
    (assoc_list(string,
        (pred(display_limit, cost_kind, include_descendants, measurement_scope,
            cmd)))::
        out(list_skel(pair(ground, (pred(in, in, in, in, out) is det)))))
    is det.

top_procs_scope_toggles = [
    "Overall" -
        set_top_procs_scope(overall),
    "Per call" -
        set_top_procs_scope(per_call)
].

:- pred set_top_procs_display_limit(display_limit::in, display_limit::in,
    cost_kind::in, include_descendants::in, measurement_scope::in, cmd::out)
    is det.

set_top_procs_display_limit(DisplayLimit, _DisplayLimit, CostKind, InclDesc,
        Scope, Cmd) :-
    Cmd = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope).

:- pred set_top_procs_sort_criteria(cost_kind::in, display_limit::in,
    cost_kind::in, include_descendants::in, measurement_scope::in, cmd::out)
    is det.

set_top_procs_sort_criteria(CostKind, DisplayLimit, _CostKind, InclDesc, Scope,
        Cmd) :-
    Cmd = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope).

:- pred set_top_procs_incl_desc(include_descendants::in, display_limit::in,
    cost_kind::in, include_descendants::in, measurement_scope::in, cmd::out)
    is det.

set_top_procs_incl_desc(InclDesc, DisplayLimit, CostKind, _InclDesc, Scope,
        Cmd) :-
    Cmd = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope).

:- pred set_top_procs_scope(measurement_scope::in, display_limit::in,
    cost_kind::in, include_descendants::in, measurement_scope::in, cmd::out)
    is det.

set_top_procs_scope(Scope, DisplayLimit, CostKind, InclDesc, _Scope, Cmd) :-
    Cmd = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope).

%---------------------------------------------------------------------------%
%
% Control how the proc command displays the procedure's callers.
%

:- func proc_callers_group_controls(deep, preferences, cmd, proc_static_ptr,
    caller_groups, int, contour_exclusion) = display_item.

proc_callers_group_controls(Deep, Prefs, Cmd, PSPtr, CallerGroups,
        BunchSize, ContourExcl) = ControlsItem :-
    make_first_proc_callers_cmds_item(Prefs, Cmd, PSPtr, CallerGroups,
        BunchSize, ContourExcl, no, proc_callers_group_toggles, GroupControls),
    ExcludeFile = Deep ^ exclude_contour_file,
    ( if Cmd = deep_cmd_proc_callers(_, _, _, _, _) then
        ExcludeFile = exclude_file(ExcludeFileName, ExcludeContents),
        (
            ExcludeContents = no_exclude_file,
            List = [GroupControls]
        ;
            ExcludeContents = unreadable_exclude_file(ErrorMsg),
            ContourExclMsg = "Cannot apply contour exclusion:\n"
                ++ ExcludeFileName ++ ": " ++ ErrorMsg,
            ContourExclItem = display_text(ContourExclMsg),
            List = [GroupControls, ContourExclItem]
        ;
            ExcludeContents = readable_exclude_file(_, _),
            make_first_proc_callers_cmds_item(Prefs, Cmd, PSPtr, CallerGroups,
                BunchSize, ContourExcl, no, proc_callers_contour_excl_toggles,
                ContourExclControls),
            List = [GroupControls, ContourExclControls]
        )
    else
        List = [GroupControls]
    ),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("The procedure's callers:"), List).

:- func proc_callers_group_toggles =
    (assoc_list(string,
        (pred(proc_static_ptr, caller_groups, int, contour_exclusion, cmd)))::
        out(list_skel(pair(ground, (pred(in, in, in, in, out) is det)))))
    is det.

proc_callers_group_toggles = [
    "Group by call site" -
        set_proc_callers_caller_groups(group_by_call_site),
    "Group by procedure" -
        set_proc_callers_caller_groups(group_by_proc),
    "Group by module" -
        set_proc_callers_caller_groups(group_by_module),
    "Group by clique" -
        set_proc_callers_caller_groups(group_by_clique)
].

:- func proc_callers_contour_excl_toggles =
    (assoc_list(string,
        (pred(proc_static_ptr, caller_groups, int, contour_exclusion, cmd)))::
        out(list_skel(pair(ground, (pred(in, in, in, in, out) is det)))))
    is det.

proc_callers_contour_excl_toggles = [
    "Apply contour exclusion" -
        set_proc_callers_contour_excl(apply_contour_exclusion),
    "Do not apply contour exclusion" -
        set_proc_callers_contour_excl(do_not_apply_contour_exclusion)
].

:- pred set_proc_callers_caller_groups(caller_groups::in, proc_static_ptr::in,
    caller_groups::in, int::in, contour_exclusion::in, cmd::out) is det.

set_proc_callers_caller_groups(CallerGroups, PSPtr, _CallerGroups, BunchSize,
        ContourExcl, Cmd) :-
    Cmd = deep_cmd_proc_callers(PSPtr, CallerGroups, 1, BunchSize,
        ContourExcl).

:- pred set_proc_callers_contour_excl(contour_exclusion::in,
    proc_static_ptr::in, caller_groups::in, int::in, contour_exclusion::in,
    cmd::out) is det.

set_proc_callers_contour_excl(ContourExcl, PSPtr, CallerGroups, BunchSize,
        _ContourExcl, Cmd) :-
    Cmd = deep_cmd_proc_callers(PSPtr, CallerGroups, 1, BunchSize,
        ContourExcl).

%---------------------------------------------------------------------------%
%
% Control how the rows in procedure displays are sorted.
%

:- func sort_controls(preferences, cmd) = display_item.

sort_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls_item(Prefs, Cmd, no,
        sort_main_toggles, SortMainControls),
    make_prefs_controls_item(Prefs, Cmd, no,
        sort_time_toggles, SortTimeControls),
    make_prefs_controls_item(Prefs, Cmd, no,
        sort_callseqs_toggles, SortCallSeqsControls),
    make_prefs_controls_item(Prefs, Cmd, no,
        sort_allocs_toggles, SortAllocsControls),
    make_prefs_controls_item(Prefs, Cmd, no,
        sort_memory_toggles, SortMemoryControls),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle sort options:"),
        [SortMainControls, SortTimeControls, SortCallSeqsControls,
        SortAllocsControls, SortMemoryControls]).

:- func sort_main_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

sort_main_toggles = [
    "Sort by context" -
        set_sort_criteria(by_context),
    "Sort by name" -
        set_sort_criteria(by_name),
    % For cost_calls and cost_redos, the second and third fields do not matter.
    "Sort by calls" -
        set_sort_criteria(by_cost(cost_calls, self, overall)),
    "Sort by redos" -
        set_sort_criteria(by_cost(cost_redos, self, overall))
].

:- func sort_time_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

sort_time_toggles = [
    "Sort by self time" -
        set_sort_criteria(by_cost(cost_time, self, overall)),
    "Sort by total time" -
        set_sort_criteria(by_cost(cost_time, self_and_desc, overall)),
    "Sort by self time per call" -
        set_sort_criteria(by_cost(cost_time, self, per_call)),
    "Sort by total time per call" -
        set_sort_criteria(by_cost(cost_time, self_and_desc, per_call))
].

:- func sort_callseqs_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

sort_callseqs_toggles = [
    "Sort by self callseqs" -
        set_sort_criteria(by_cost(cost_callseqs, self, overall)),
    "Sort by total callseqs" -
        set_sort_criteria(by_cost(cost_callseqs, self_and_desc, overall)),
    "Sort by self callseqs per call" -
        set_sort_criteria(by_cost(cost_callseqs, self, per_call)),
    "Sort by total callseqs per call" -
        set_sort_criteria(by_cost(cost_callseqs, self_and_desc, per_call))
].

:- func sort_allocs_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

sort_allocs_toggles = [
    "Sort by self allocs" -
        set_sort_criteria(by_cost(cost_allocs, self, overall)),
    "Sort by total allocs" -
        set_sort_criteria(by_cost(cost_allocs, self_and_desc, overall)),
    "Sort by self allocs per call" -
        set_sort_criteria(by_cost(cost_allocs, self, per_call)),
    "Sort by total allocs per call" -
        set_sort_criteria(by_cost(cost_allocs, self_and_desc, per_call))
].

:- func sort_memory_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

sort_memory_toggles = [
    "Sort by self memory" -
        set_sort_criteria(by_cost(cost_words, self, overall)),
    "Sort by total memory" -
        set_sort_criteria(by_cost(cost_words, self_and_desc, overall)),
    "Sort by self memory per call" -
        set_sort_criteria(by_cost(cost_words, self, per_call)),
    "Sort by total memory per call" -
        set_sort_criteria(by_cost(cost_words, self_and_desc, per_call))
].

:- pred set_sort_criteria(order_criteria::in,
    preferences::in, preferences::out) is det.

set_sort_criteria(SortCriteria, !Prefs) :-
    !Prefs ^ pref_criteria := SortCriteria.

%---------------------------------------------------------------------------%
%
% Control whether we summarize the info from callees at multi call sites.
%

:- func summarize_controls(preferences, cmd) = display_item.

summarize_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls_item(Prefs, Cmd, no,
        summarize_toggles, SummarizeControls),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle summarize options:"), [SummarizeControls]).

:- func summarize_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

summarize_toggles = [
    "Do not summarize higher order calls" -
        set_summarize(do_not_summarize_ho_call_sites),
    "Summarize higher order calls" -
        set_summarize(summarize_ho_call_sites)
].

:- pred set_summarize(summarize_ho_call_sites::in,
    preferences::in, preferences::out) is det.

set_summarize(Summarize, !Prefs) :-
    !Prefs ^ pref_summarize := Summarize.

%---------------------------------------------------------------------------%
%
% Control whether we display call sites that do not get any calls.
%

:- func module_qual_controls(preferences, cmd) = display_item.

module_qual_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls_item(Prefs, Cmd, no,
        module_qual_toggles, ShowNoCallSitesControls),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle show no calls site options:"), [ShowNoCallSitesControls]).

:- func module_qual_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

module_qual_toggles = [
    "Never module qualify predicate and function names" -
        set_module_qual(module_qual_never),
    "Module qualify predicate and function names only when needed" -
        set_module_qual(module_qual_when_diff),
    "Always module qualify predicate and function names" -
        set_module_qual(module_qual_always)
].

:- pred set_module_qual(module_qual::in,
    preferences::in, preferences::out) is det.

set_module_qual(ModuleQual, !Prefs) :-
    !Prefs ^ pref_module_qual := ModuleQual.

%---------------------------------------------------------------------------%
%
% Control how pages are displayed.
%

:- func format_controls(preferences, cmd) = display_item.

format_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls_item(Prefs, Cmd, no,
        format_toggles, FormatControls),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle format options:"), [FormatControls]).

:- func format_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

format_toggles = [
    "Fade column groups" -
        set_colour_column_groups(do_not_colour_column_groups),
    "Colour column groups" -
        set_colour_column_groups(colour_column_groups),
    "Unbox" -
        set_box_tables(do_not_box_tables),
    "Box" -
        set_box_tables(box_tables)
].

:- pred set_colour_column_groups(colour_column_groups::in,
    preferences::in, preferences::out) is det.

set_colour_column_groups(Colour, !Prefs) :-
    !Prefs ^ pref_colour := Colour.

:- pred set_box_tables(box_tables::in,
    preferences::in, preferences::out) is det.

set_box_tables(Box, !Prefs) :-
    !Prefs ^ pref_box := Box.

%---------------------------------------------------------------------------%
%
% Controls for related procedure and clique reports.
%

:- func proc_reports_controls(preferences, proc_static_ptr, cmd)
    = display_item.

proc_reports_controls(Prefs, Proc, NotCmd) = ControlsItem :-
    Makers = [
        control_maker(deep_cmd_proc(Proc),
            "Procedure", no),
        % XXX: Should procrep_coverage be a developer-only option?
        control_maker(deep_cmd_static_procrep_coverage(Proc),
            "Coverage annotated procedure representation", no),
        control_maker(deep_cmd_dump_proc_static(Proc),
            "Unprocessed proc static data", yes)
    ],
    make_controls(Prefs, NotCmd, Makers, ProcReportControls),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Related procedure reports:"), ProcReportControls).

:- func clique_reports_controls(preferences, clique_ptr, cmd) = display_item.

clique_reports_controls(Prefs, CliquePtr, NotCmd) = ControlsItem :-
    Makers = [
        control_maker(deep_cmd_clique(CliquePtr),
            "Clique", no),
        control_maker(deep_cmd_clique_recursive_costs(CliquePtr),
            "Clique's recursion information", yes),
        control_maker(deep_cmd_dump_clique(CliquePtr),
            "Unprocessed clique data", yes)
    ],
    make_controls(Prefs, NotCmd, Makers, CliqueReportControls),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Related clique reports:"), CliqueReportControls).

:- type control_maker
    --->    control_maker(cmd, string, bool).

:- pred make_controls(preferences::in, cmd::in, list(control_maker)::in,
    list(display_item)::out) is det.

make_controls(_Prefs, _NotCmd, [], []).
make_controls(Prefs, NotCmd, [Maker | Makers], Items) :-
    make_controls(Prefs, NotCmd, Makers, TailItems),
    Maker = control_maker(Cmd, Label, Developer),
    ( if Cmd = NotCmd then
        Items = TailItems
    else
        make_control(yes(Prefs), Cmd, Label, Developer, Control),
        Items = [Control | TailItems]
    ).

%---------------------------------------------------------------------------%
%
% Control whether we display inactive modules.
%

:- func inactive_module_controls(preferences, cmd) = display_item.

inactive_module_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls_item(Prefs, Cmd, no,
        inactive_module_toggles, InactiveModuleControls),
    Controls = [InactiveModuleControls],
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle display of inactive modules:"), Controls).

:- func inactive_module_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

inactive_module_toggles = [
    "Do not display inactive modules" -
        set_inactive_modules(inactive_hide),
    "Display inactive modules" -
        set_inactive_modules(inactive_show)
].

:- pred set_inactive_modules(inactive_status::in,
    preferences::in, preferences::out) is det.

set_inactive_modules(Status, !Prefs) :-
    !Prefs ^ pref_inactive ^ inactive_modules := Status.

%---------------------------------------------------------------------------%
%
% Control whether we display inactive procedures.
%

:- func inactive_proc_controls(preferences, cmd) = display_item.

inactive_proc_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls_item(Prefs, Cmd, no,
        inactive_proc_toggles, InactiveModuleControls),
    Controls = [InactiveModuleControls],
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle display of inactive procedures:"), Controls).

:- func inactive_proc_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

inactive_proc_toggles = [
    "Do not display inactive procedures" -
        set_inactive_procs(inactive_hide),
    "Display inactive procedures" -
        set_inactive_procs(inactive_show)
].

:- pred set_inactive_procs(inactive_status::in,
    preferences::in, preferences::out) is det.

set_inactive_procs(Status, !Prefs) :-
    !Prefs ^ pref_inactive ^ inactive_procs := Status.

%---------------------------------------------------------------------------%
%
% Control whether we display inactive call sites.
%

:- func inactive_call_site_controls(preferences, cmd) = display_item.

inactive_call_site_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls_item(Prefs, Cmd, no,
        inactive_call_site_toggles, InactiveModuleControls),
    Controls = [InactiveModuleControls],
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle display of inactive call_sites:"), Controls).

:- func inactive_call_site_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

inactive_call_site_toggles = [
    "Do not display inactive call sites" -
        set_inactive_call_sites(inactive_hide),
    "Display inactive call sites" -
        set_inactive_call_sites(inactive_show)
].

:- pred set_inactive_call_sites(inactive_status::in,
    preferences::in, preferences::out) is det.

set_inactive_call_sites(Status, !Prefs) :-
    !Prefs ^ pref_inactive ^ inactive_call_sites := Status.

%---------------------------------------------------------------------------%
%
% Control how many ancestors we display.
%

:- func ancestor_controls(preferences, cmd) = display_item.

ancestor_controls(Prefs, Cmd) = ControlsItem :-
    MaybeAncestorLimit = Prefs ^ pref_anc,
    (
        MaybeAncestorLimit = no,
        make_prefs_controls_item(Prefs, Cmd, no,
            ancestor_toggles_no, AncestorControls)
    ;
        MaybeAncestorLimit = yes(AncestorLimit),
        make_prefs_controls_item(Prefs, Cmd, no,
            ancestor_toggles_yes(AncestorLimit), AncestorControls)
    ),
    Controls = [AncestorControls],
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle display of ancestors:"), Controls).

:- func ancestor_toggles_no =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

ancestor_toggles_no = [
    "One ancestor" -
        set_ancestor_limit(yes(1)),
    "Two ancestors" -
        set_ancestor_limit(yes(2)),
    "Three ancestors" -
        set_ancestor_limit(yes(3)),
    "Five ancestors" -
        set_ancestor_limit(yes(5)),
    "Ten ancestors" -
        set_ancestor_limit(yes(10))
].

:- func ancestor_toggles_yes(int::in) =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

ancestor_toggles_yes(CurrentLimit) = [
    "Halve ancestors" -
        set_ancestor_limit(yes(CurrentLimit // 2)),
    "Remove an ancestor" -
        set_ancestor_limit(yes(CurrentLimit - 1)),
    "Add an ancestor" -
        set_ancestor_limit(yes(CurrentLimit + 1)),
    "Double ancestors" -
        set_ancestor_limit(yes(CurrentLimit * 2)),
    "Unlimited ancestors" -
        set_ancestor_limit(no)
].

:- pred set_ancestor_limit(maybe(int)::in,
    preferences::in, preferences::out) is det.

set_ancestor_limit(MaybeAncestorLimit, !Prefs) :-
    !Prefs ^ pref_anc := MaybeAncestorLimit.

%---------------------------------------------------------------------------%
%
% Control the set of displayed fields.
%

:- func field_controls(preferences, cmd) = display_item.

field_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls_item(Prefs, Cmd, no,
        port_field_toggles, PortControls),
    make_prefs_controls_item(Prefs, Cmd, no,
        time_field_toggles, TimeControls),
    make_prefs_controls_item(Prefs, Cmd, no,
        callseqs_field_toggles, CallSeqsControls),
    make_prefs_controls_item(Prefs, Cmd, no,
        alloc_field_toggles, AllocControls),
    make_prefs_controls_item(Prefs, Cmd, no,
        memory_field_toggles, MemoryControls),
    Controls = [PortControls, TimeControls, CallSeqsControls,
        AllocControls, MemoryControls],
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle displayed fields:"), Controls).

:- func port_field_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

port_field_toggles = [
    "No port counts" -
        set_port_fields(no_port),
    "Port counts" -
        set_port_fields(port)
].

:- func time_field_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

time_field_toggles = [
    "No time info" -
        set_time_fields(no_time),
    "Ticks" -
        set_time_fields(ticks),
    "Times" -
        set_time_fields(time),
    "Ticks and times" -
        set_time_fields(ticks_and_time),
    "Times and per-call-times" -
        set_time_fields(time_and_percall),
    "Ticks and times and per-call times" -
        set_time_fields(ticks_and_time_and_percall)
].

:- func callseqs_field_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

callseqs_field_toggles = [
    "No call sequence numbers" -
        set_callseqs_fields(no_callseqs),
    "Call sequence numbers" -
        set_callseqs_fields(callseqs),
    "Call sequence numbers including per-call" -
        set_callseqs_fields(callseqs_and_percall)
].

:- func alloc_field_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

alloc_field_toggles = [
    "No allocation info" -
        set_alloc_fields(no_alloc),
    "Allocations" -
        set_alloc_fields(alloc),
    "Allocations and per-call allocations" -
        set_alloc_fields(alloc_and_percall)
].

:- func memory_field_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

memory_field_toggles = [
    "No memory info" -
        set_memory_fields(no_memory),
    "Words" -
        set_memory_fields(memory(units_words)),
    "Bytes" -
        set_memory_fields(memory(units_bytes)),
    "Words and per-call words" -
        set_memory_fields(memory_and_percall(units_words)),
    "Bytes and per-call bytes" -
        set_memory_fields(memory_and_percall(units_bytes))
].

:- pred set_port_fields(port_fields::in,
    preferences::in, preferences::out) is det.

set_port_fields(PortFields, !Prefs) :-
    Fields0 = !.Prefs ^ pref_fields,
    Fields = Fields0 ^ port_fields := PortFields,
    !Prefs ^ pref_fields := Fields.

:- pred set_time_fields(time_fields::in,
    preferences::in, preferences::out) is det.

set_time_fields(TimeFields, !Prefs) :-
    Fields0 = !.Prefs ^ pref_fields,
    Fields = Fields0 ^ time_fields := TimeFields,
    !Prefs ^ pref_fields := Fields.

:- pred set_callseqs_fields(callseqs_fields::in,
    preferences::in, preferences::out) is det.

set_callseqs_fields(CallSeqsFields, !Prefs) :-
    Fields0 = !.Prefs ^ pref_fields,
    Fields = Fields0 ^ callseqs_fields := CallSeqsFields,
    !Prefs ^ pref_fields := Fields.

:- pred set_alloc_fields(alloc_fields::in,
    preferences::in, preferences::out) is det.

set_alloc_fields(AllocFields, !Prefs) :-
    Fields0 = !.Prefs ^ pref_fields,
    Fields = Fields0 ^ alloc_fields := AllocFields,
    !Prefs ^ pref_fields := Fields.

:- pred set_memory_fields(memory_fields::in,
    preferences::in, preferences::out) is det.

set_memory_fields(MemoryFields, !Prefs) :-
    Fields0 = !.Prefs ^ pref_fields,
    Fields = Fields0 ^ memory_fields := MemoryFields,
    !Prefs ^ pref_fields := Fields.

%---------------------------------------------------------------------------%
%
% Links to the basic commands.
%

    % Give the common list of commands seen at the bottom of all deep-profiler
    % displayed reports.
    %
:- func cmds_menu_restart_quit(maybe(preferences)) = display_item.

cmds_menu_restart_quit(MaybePrefs) = ControlsItem :-
    Menu = display_link(deep_link(deep_cmd_menu, MaybePrefs,
        attr_str([], "Menu"), link_class_control)),
    Restart = display_link(deep_link(deep_cmd_restart, MaybePrefs,
        attr_str([], "Restart"), link_class_control)),
    Quit = display_link(deep_link(deep_cmd_quit, MaybePrefs,
        attr_str([], "Quit"), link_class_control)),
    List = [Menu, Restart, Quit],
    ControlsItem = display_list(list_class_horizontal_except_title,
        yes("General commands:"), List).

:- func general_options_controls(cmd, preferences) = display_item.

general_options_controls(Cmd, Prefs) = ControlsItem :-
    DeveloperMode = Prefs ^ pref_developer_mode,
    (
        DeveloperMode = developer_options_visible,
        DeveloperText = "Disable developer options",
        DeveloperPrefs =
            Prefs ^ pref_developer_mode := developer_options_invisible
    ;
        DeveloperMode = developer_options_invisible,
        DeveloperText = "Enable developer options",
        DeveloperPrefs =
            Prefs ^ pref_developer_mode := developer_options_visible
    ),
    DeveloperControl = display_link(deep_link(Cmd, yes(DeveloperPrefs),
        attr_str([], DeveloperText), link_class_control)),
    List = [DeveloperControl],
    ControlsItem = display_list(list_class_horizontal_except_title,
        yes("General options:"), List).

%---------------------------------------------------------------------------%
%
% Convert procedure, call site and clique descriptions into table cells.
%

:- func module_active_to_module_name_cell(preferences, module_active) =
    table_cell.

module_active_to_module_name_cell(Prefs, ModuleActive) = Cell :-
    ModuleName = ModuleActive ^ ma_module_name,
    Cmd = deep_cmd_module(ModuleName),
    Link = deep_link(Cmd, yes(Prefs), attr_str([], ModuleName),
        link_class_link),
    Cell = table_cell(td_l(Link)).

:- func proc_active_to_proc_name_cell(maybe(string), module_qual,
    preferences, proc_active) = table_cell.

proc_active_to_proc_name_cell(MaybeCurModuleName, ModuleQual,
        Prefs, ProcActive) =
    proc_desc_to_proc_name_cell(MaybeCurModuleName, ModuleQual,
        Prefs, ProcActive ^ pa_proc_desc).

:- func proc_desc_to_source_cell(proc_desc) = table_cell.

proc_desc_to_source_cell(ProcDesc) = Cell :-
    FileName = ProcDesc ^ pdesc_file_name,
    LineNumber = ProcDesc ^ pdesc_line_number,
    Source = string.format("%s:%d", [s(FileName), i(LineNumber)]),
    Cell = table_cell(td_s(Source)).

:- func proc_desc_to_proc_name_cell(maybe(string), module_qual,
    preferences, proc_desc) = table_cell.

proc_desc_to_proc_name_cell(MaybeCurModuleName, ModuleQual, Prefs, ProcDesc)
        = Cell :-
    PSPtr = ProcDesc ^ pdesc_ps_ptr,
    RefinedName = proc_desc_get_refined_id(MaybeCurModuleName, ModuleQual,
        ProcDesc),
    Cmd = deep_cmd_proc(PSPtr),
    Link = deep_link(Cmd, yes(Prefs), attr_str([], RefinedName),
        link_class_link),
    Cell = table_cell(td_l(Link)).

:- func proc_desc_to_proc_name_cell_span(maybe(string), module_qual,
    preferences, list(str_attr), proc_desc, int) = table_cell.

proc_desc_to_proc_name_cell_span(MaybeCurModuleName, ModuleQual,
        Prefs, Attrs, ProcDesc, Span) = Cell :-
    PSPtr = ProcDesc ^ pdesc_ps_ptr,
    RefinedName = proc_desc_get_refined_id(MaybeCurModuleName, ModuleQual,
        ProcDesc),
    Cmd = deep_cmd_proc(PSPtr),
    Link = deep_link(Cmd, yes(Prefs), attr_str(Attrs, RefinedName),
        link_class_link),
    Cell = table_multi_cell(td_l(Link), Span).

:- func proc_desc_to_prefix_proc_name_cell(maybe(string), module_qual,
    preferences, list(str_attr), proc_desc, string) = table_cell.

proc_desc_to_prefix_proc_name_cell(MaybeCurModuleName, ModuleQual,
        Prefs, Attrs, ProcDesc, Prefix) = Cell :-
    PSPtr = ProcDesc ^ pdesc_ps_ptr,
    RefinedName = proc_desc_get_refined_id(MaybeCurModuleName, ModuleQual,
        ProcDesc),
    Cmd = deep_cmd_proc(PSPtr),
    Link = deep_link(Cmd, yes(Prefs), attr_str(Attrs, Prefix ++ RefinedName),
        link_class_link),
    Cell = table_cell(td_l(Link)).

:- func call_site_desc_to_name_path_slot_cell(maybe(string), module_qual,
    preferences, call_site_desc) = table_cell.

call_site_desc_to_name_path_slot_cell(MaybeCurModuleName, ModuleQual,
        Prefs, CallSiteDesc) = Cell :-
    CallSiteDesc = call_site_desc(CSSPtr, _ContainerPSPtr,
        _FileName, _LineNumber, _ModuleName,
        _UnQualRefinedName, _QualRefinedName,
        SlotNumber, RevGoalPath, _MaybeCallee),
    RefinedName = call_site_desc_get_caller_refined_id(MaybeCurModuleName,
        ModuleQual, CallSiteDesc),
    GoalPathStr = rev_goal_path_to_string(RevGoalPath),
    string.format("%s @ %s #%d",
        [s(RefinedName), s(GoalPathStr), i(SlotNumber)], Name),
    Cmd = deep_cmd_dump_call_site_static(CSSPtr),
    Link = deep_link(Cmd, yes(Prefs), attr_str([], Name), link_class_link),
    Cell = table_cell(td_l(Link)).

:- func call_site_desc_to_source_cell(call_site_desc) = table_cell.

call_site_desc_to_source_cell(CallSiteDesc) = Cell :-
    FileName = CallSiteDesc ^ csdesc_file_name,
    LineNumber = CallSiteDesc ^ csdesc_line_number,
    Source = string.format("%s:%d", [s(FileName), i(LineNumber)]),
    Cell = table_cell(td_s(Source)).

:- func call_site_desc_to_caller_proc_name_cell(maybe(string), module_qual,
    preferences, call_site_desc) = table_cell.

call_site_desc_to_caller_proc_name_cell(MaybeCurModuleName, ModuleQual,
        Prefs, CallSiteDesc) = Cell :-
    PSPtr = CallSiteDesc ^ csdesc_container,
    CallerRefinedName =
    call_site_desc_get_caller_refined_id(MaybeCurModuleName, ModuleQual,
        CallSiteDesc),
    Cmd = deep_cmd_proc(PSPtr),
    Link = deep_link(Cmd, yes(Prefs), attr_str([], CallerRefinedName),
        link_class_link),
    Cell = table_cell(td_l(Link)).

:- func clique_desc_to_non_self_link_proc_name_cell(maybe(string), module_qual,
    preferences, clique_desc, clique_ptr) = table_cell.

clique_desc_to_non_self_link_proc_name_cell(MaybeCurModuleName, ModuleQual,
        Prefs, CliqueDesc, SelfCliquePtr) = Cell :-
    CliqueDesc = clique_desc(CliquePtr, EntryProcDesc, _OtherProcDescs),
    EntryProcName = proc_desc_get_refined_id(MaybeCurModuleName, ModuleQual,
        EntryProcDesc),
    ( if CliquePtr = SelfCliquePtr then
        Cell = table_cell(td_s(EntryProcName))
    else
        Cmd = deep_cmd_clique(CliquePtr),
        Link = deep_link(Cmd, yes(Prefs), attr_str([], EntryProcName),
            link_class_link),
        Cell = table_cell(td_l(Link))
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates.
%

    % Make a table row with two columns: a label and a value.
    %
:- func make_labelled_table_row(pair(string, table_data)) = table_row.

make_labelled_table_row(Label - Value) =
    table_row([table_cell(td_s(Label)), table_cell(Value)]).

:- type link_base
    --->    link_base(cmd, maybe(preferences), string).

    % Make a link from a command, preferences and a label.
    %
:- pred make_link(link_base::in, display_item::out) is det.

make_link(link_base(Cmd, MaybePrefs, Label), Item) :-
    Item = display_link(deep_link(Cmd, MaybePrefs, attr_str([], Label),
        link_class_link)).

    % Make a control from a command and label and optional preferences
    % structure.
    %
:- pred make_control(maybe(preferences)::in, cmd::in, string::in, bool::in,
    display_item::out) is det.

make_control(MaybePrefs, Cmd, Label, Developer, Item) :-
    Item0 = display_link(deep_link(Cmd, MaybePrefs, attr_str([], Label),
        link_class_control)),
    (
        Developer = yes,
        Item = display_developer(Item0)
    ;
        Developer = no,
        Item = Item0
    ).

%---------------------------------------------------------------------------%
%
% Sort procedures in a clique by the preferred criteria of performance.
%

:- pred sort_clique_procs_by_preferences(maybe(string)::in, module_qual::in,
    preferences::in,
    list(clique_proc_report)::in, list(clique_proc_report)::out) is det.

sort_clique_procs_by_preferences(MaybeCurModuleName, ModuleQual, Prefs,
        !CliqueProcs) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        OrderCriteria = by_context,
        list.sort(compare_clique_procs_by_context, !CliqueProcs)
    ;
        OrderCriteria = by_name,
        list.sort(
            compare_clique_procs_by_name(MaybeCurModuleName, ModuleQual),
            !CliqueProcs)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_clique_procs_by_cost(CostKind, InclDesc, Scope),
            !CliqueProcs),
        % We want the most expensive procedures to appear first.
        list.reverse(!CliqueProcs)
    ).

:- pred compare_clique_procs_by_context(
    clique_proc_report::in, clique_proc_report::in, comparison_result::out)
    is det.

compare_clique_procs_by_context(CliqueProcReportA, CliqueProcReportB,
        Result) :-
    CliqueProcDescA = CliqueProcReportA ^ cpr_proc_summary ^ perf_row_subject,
    CliqueProcDescB = CliqueProcReportB ^ cpr_proc_summary ^ perf_row_subject,
    compare_proc_descs_by_context(CliqueProcDescA, CliqueProcDescB, Result).

:- pred compare_clique_procs_by_name(maybe(string)::in, module_qual::in,
    clique_proc_report::in, clique_proc_report::in, comparison_result::out)
    is det.

compare_clique_procs_by_name(MaybeCurModuleName, ModuleQual,
        CliqueProcReportA, CliqueProcReportB, Result) :-
    CliqueProcDescA = CliqueProcReportA ^ cpr_proc_summary ^ perf_row_subject,
    CliqueProcDescB = CliqueProcReportB ^ cpr_proc_summary ^ perf_row_subject,
    compare_proc_descs_by_name(MaybeCurModuleName, ModuleQual,
        CliqueProcDescA, CliqueProcDescB, Result).

:- pred compare_clique_procs_by_cost(
    cost_kind::in, include_descendants::in, measurement_scope::in,
    clique_proc_report::in, clique_proc_report::in, comparison_result::out)
    is det.

compare_clique_procs_by_cost(CostKind, InclDesc, Scope,
        CliqueProcReportA, CliqueProcReportB, Result) :-
    ProcRowDataA = CliqueProcReportA ^ cpr_proc_summary,
    ProcRowDataB = CliqueProcReportB ^ cpr_proc_summary,
    compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope,
        ProcRowDataA, ProcRowDataB, Result).

%---------------------------------------------------------------------------%
%
% Sort proc_dynamics in a clique by the preferred criteria of performance.
%

:- pred sort_clique_proc_dynamics_by_preferences(preferences::in,
    list(clique_proc_dynamic_report)::in,
    list(clique_proc_dynamic_report)::out) is det.

sort_clique_proc_dynamics_by_preferences(Prefs, !CliqueProcDynamics) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        ( OrderCriteria = by_context
        ; OrderCriteria = by_name
        ),
        % All the proc_dynamics we want to sort have the same name and context,
        % so it does not make sense to sort on these criteria. Instead, we sort
        % on the default performance criteria.
        CostKind = default_cost_kind,
        InclDesc = default_incl_desc,
        Scope = default_scope
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope)
    ),
    list.sort(compare_clique_proc_dynamics_by_cost(CostKind, InclDesc, Scope),
        !CliqueProcDynamics),
    % We want the most expensive procedures to appear first.
    list.reverse(!CliqueProcDynamics).

:- pred compare_clique_proc_dynamics_by_cost(
    cost_kind::in, include_descendants::in, measurement_scope::in,
    clique_proc_dynamic_report::in, clique_proc_dynamic_report::in,
    comparison_result::out) is det.

compare_clique_proc_dynamics_by_cost(CostKind, InclDesc, Scope,
        CliqueProcDynamicReportA, CliqueProcDynamicReportB, Result) :-
    ProcDynamicRowDataA = CliqueProcDynamicReportA ^ cpdr_proc_summary,
    ProcDynamicRowDataB = CliqueProcDynamicReportB ^ cpdr_proc_summary,
    compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope,
        ProcDynamicRowDataA, ProcDynamicRowDataB, Result).

%---------------------------------------------------------------------------%
%
% Sort clique_call_site_reports by the preferred criteria of performance.
%

:- pred sort_clique_call_site_reports_by_preferences(maybe(string)::in,
    module_qual::in, preferences::in,
    list(clique_call_site_report)::in, list(clique_call_site_report)::out)
    is det.

sort_clique_call_site_reports_by_preferences(MaybeCurModuleName, ModuleQual,
        Prefs, !CallSiteReports) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        OrderCriteria = by_context,
        list.sort(compare_clique_call_site_reports_by_context,
            !CallSiteReports)
    ;
        OrderCriteria = by_name,
        list.sort(
            compare_clique_call_site_reports_by_name(MaybeCurModuleName,
                ModuleQual),
            !CallSiteReports)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_clique_call_site_reports_by_cost(CostKind,
            InclDesc, Scope), !CallSiteReports),
        % We want the most expensive call sites to appear first.
        list.reverse(!CallSiteReports)
    ).

:- pred compare_clique_call_site_reports_by_context(
    clique_call_site_report::in, clique_call_site_report::in,
    comparison_result::out) is det.

compare_clique_call_site_reports_by_context(CallSiteReportA,
        CallSiteReportB, Result) :-
    CallSiteDescA =
        CallSiteReportA ^ ccsr_call_site_summary ^ perf_row_subject,
    CallSiteDescB =
        CallSiteReportB ^ ccsr_call_site_summary ^ perf_row_subject,
    compare_call_site_descs_by_context(CallSiteDescA, CallSiteDescB, Result).

:- pred compare_clique_call_site_reports_by_name(maybe(string)::in,
    module_qual::in, clique_call_site_report::in, clique_call_site_report::in,
    comparison_result::out) is det.

compare_clique_call_site_reports_by_name(MaybeCurModuleName, ModuleQual,
        CallSiteReportA, CallSiteReportB, Result) :-
    CallSiteDescA =
        CallSiteReportA ^ ccsr_call_site_summary ^ perf_row_subject,
    CallSiteDescB =
        CallSiteReportB ^ ccsr_call_site_summary ^ perf_row_subject,
    compare_call_site_descs_by_name(MaybeCurModuleName, ModuleQual,
        CallSiteDescA, CallSiteDescB, Result).

:- pred compare_clique_call_site_reports_by_cost(
    cost_kind::in, include_descendants::in, measurement_scope::in,
    clique_call_site_report::in, clique_call_site_report::in,
    comparison_result::out) is det.

compare_clique_call_site_reports_by_cost(CostKind, InclDesc, Scope,
        CliqueCallSiteReportA, CliqueCallSiteReportB, Result) :-
    PerfA = CliqueCallSiteReportA ^ ccsr_call_site_summary,
    PerfB = CliqueCallSiteReportB ^ ccsr_call_site_summary,
    compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope, PerfA, PerfB,
        Result).

%---------------------------------------------------------------------------%
%
% Sort call_site_perfs by the preferred criteria of performance.
%

:- pred sort_call_sites_by_preferences(maybe(string)::in, module_qual::in,
    preferences::in,
    list(call_site_perf)::in, list(call_site_perf)::out) is det.

sort_call_sites_by_preferences(MaybeCurModuleName, ModuleQual, Prefs,
        !CallSitePerfs) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        OrderCriteria = by_context,
        list.sort(compare_call_site_perfs_by_context, !CallSitePerfs)
    ;
        OrderCriteria = by_name,
        list.sort(
            compare_call_site_perfs_by_callee_name(MaybeCurModuleName,
                ModuleQual),
            !CallSitePerfs)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_call_site_perfs_by_cost(CostKind, InclDesc, Scope),
            !CallSitePerfs),
        % We want the most expensive call sites to appear first.
        list.reverse(!CallSitePerfs)
    ).

:- pred compare_call_site_perfs_by_context(
    call_site_perf::in, call_site_perf::in, comparison_result::out) is det.

compare_call_site_perfs_by_context(CallSitePerfA, CallSitePerfB, Result) :-
    CallSiteDescA = CallSitePerfA ^ csf_summary_perf ^ perf_row_subject,
    CallSiteDescB = CallSitePerfB ^ csf_summary_perf ^ perf_row_subject,
    compare_call_site_descs_by_context(CallSiteDescA, CallSiteDescB, Result).

:- pred compare_call_site_perfs_by_callee_name(maybe(string)::in,
    module_qual::in,
    call_site_perf::in, call_site_perf::in, comparison_result::out) is det.

compare_call_site_perfs_by_callee_name(MaybeCurModuleName, ModuleQual,
        CallSitePerfA, CallSitePerfB, Result) :-
    CallSiteDescA = CallSitePerfA ^ csf_summary_perf ^ perf_row_subject,
    CallSiteDescB = CallSitePerfB ^ csf_summary_perf ^ perf_row_subject,
    compare_call_site_descs_by_callee_name(MaybeCurModuleName, ModuleQual,
        CallSiteDescA, CallSiteDescB, Result).

:- pred compare_call_site_perfs_by_cost(
    cost_kind::in, include_descendants::in, measurement_scope::in,
    call_site_perf::in, call_site_perf::in, comparison_result::out) is det.

compare_call_site_perfs_by_cost(CostKind, InclDesc, Scope,
        CallSitePerfA, CallSitePerfB, Result) :-
    PerfA = CallSitePerfA ^ csf_summary_perf,
    PerfB = CallSitePerfB ^ csf_summary_perf,
    compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope, PerfA, PerfB,
        Result).

%---------------------------------------------------------------------------%
%
% Sort perf_data_rows of call_site_descs by the preferred criteria.
%

:- pred sort_call_site_desc_rows_by_preferences(maybe(string)::in,
    module_qual::in, preferences::in,
    list(perf_row_data(call_site_desc))::in,
    list(perf_row_data(call_site_desc))::out) is det.

sort_call_site_desc_rows_by_preferences(MaybeCurModuleName, ModuleQual, Prefs,
        !CallSiteRowDatas) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        OrderCriteria = by_context,
        list.sort(compare_call_site_desc_rows_by_context, !CallSiteRowDatas)
    ;
        OrderCriteria = by_name,
        list.sort(
            compare_call_site_desc_rows_by_name(MaybeCurModuleName,
                ModuleQual),
            !CallSiteRowDatas)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope),
            !CallSiteRowDatas),
        % We want the most expensive rows to appear first.
        list.reverse(!CallSiteRowDatas)
    ).

:- pred compare_call_site_desc_rows_by_context(
    perf_row_data(call_site_desc)::in, perf_row_data(call_site_desc)::in,
    comparison_result::out) is det.

compare_call_site_desc_rows_by_context(
        CallSiteDescRowDataA, CallSiteDescRowDataB, Result) :-
    CallSiteDescA = CallSiteDescRowDataA ^ perf_row_subject,
    CallSiteDescB = CallSiteDescRowDataB ^ perf_row_subject,
    compare_call_site_descs_by_context(CallSiteDescA, CallSiteDescB, Result).

:- pred compare_call_site_desc_rows_by_name(maybe(string)::in, module_qual::in,
    perf_row_data(call_site_desc)::in, perf_row_data(call_site_desc)::in,
    comparison_result::out) is det.

compare_call_site_desc_rows_by_name(MaybeCurModuleName, ModuleQual,
        CallSiteDescRowDataA, CallSiteDescRowDataB, Result) :-
    CallSiteDescA = CallSiteDescRowDataA ^ perf_row_subject,
    CallSiteDescB = CallSiteDescRowDataB ^ perf_row_subject,
    compare_call_site_descs_by_name(MaybeCurModuleName, ModuleQual,
        CallSiteDescA, CallSiteDescB, Result).

%---------------------------------------------------------------------------%
%
% Sort perf_data_rows of proc_descs by the preferred criteria.
%

:- pred sort_proc_desc_rows_by_preferences(maybe(string)::in, module_qual::in,
    preferences::in,
    list(perf_row_data(proc_desc))::in, list(perf_row_data(proc_desc))::out)
    is det.

sort_proc_desc_rows_by_preferences(MaybeCurModuleName, ModuleQual, Prefs,
        !ProcDescRowDatas) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        OrderCriteria = by_context,
        list.sort(compare_proc_desc_rows_by_context, !ProcDescRowDatas)
    ;
        OrderCriteria = by_name,
        list.sort(
            compare_proc_desc_rows_by_name(MaybeCurModuleName, ModuleQual),
            !ProcDescRowDatas)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope),
            !ProcDescRowDatas),
        % We want the most expensive rows to appear first.
        list.reverse(!ProcDescRowDatas)
    ).

:- pred compare_proc_desc_rows_by_context(
    perf_row_data(proc_desc)::in, perf_row_data(proc_desc)::in,
    comparison_result::out) is det.

compare_proc_desc_rows_by_context(ProcDescRowDataA, ProcDescRowDataB,
            Result) :-
    ProcDescA = ProcDescRowDataA ^ perf_row_subject,
    ProcDescB = ProcDescRowDataB ^ perf_row_subject,
    compare_proc_descs_by_context(ProcDescA, ProcDescB, Result).

:- pred compare_proc_desc_rows_by_name(maybe(string)::in, module_qual::in,
    perf_row_data(proc_desc)::in, perf_row_data(proc_desc)::in,
    comparison_result::out) is det.

compare_proc_desc_rows_by_name(MaybeCurModuleName, ModuleQual,
        ProcDescRowDataA, ProcDescRowDataB, Result) :-
    ProcDescA = ProcDescRowDataA ^ perf_row_subject,
    ProcDescB = ProcDescRowDataB ^ perf_row_subject,
    compare_proc_descs_by_name(MaybeCurModuleName, ModuleQual,
        ProcDescA, ProcDescB, Result).

%---------------------------------------------------------------------------%
%
% Sort perf_data_rows of proc_actives by the preferred criteria.
%

:- pred sort_proc_active_rows_by_preferences(maybe(string)::in,
    module_qual::in, preferences::in,
    list(perf_row_data(proc_active))::in,
    list(perf_row_data(proc_active))::out) is det.

sort_proc_active_rows_by_preferences(MaybeCurModuleName, ModuleQual, Prefs,
        !ProcRowDatas) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        OrderCriteria = by_context,
        list.sort(compare_proc_active_rows_by_context, !ProcRowDatas)
    ;
        OrderCriteria = by_name,
        list.sort(
            compare_proc_active_rows_by_name(MaybeCurModuleName, ModuleQual),
            !ProcRowDatas)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope),
            !ProcRowDatas),
        % We want the most expensive rows to appear first.
        list.reverse(!ProcRowDatas)
    ).

:- pred compare_proc_active_rows_by_context(
    perf_row_data(proc_active)::in, perf_row_data(proc_active)::in,
    comparison_result::out) is det.

compare_proc_active_rows_by_context(ProcRowDataA, ProcRowDataB, Result) :-
    ProcDescA = ProcRowDataA ^ perf_row_subject ^ pa_proc_desc,
    ProcDescB = ProcRowDataB ^ perf_row_subject ^ pa_proc_desc,
    compare_proc_descs_by_context(ProcDescA, ProcDescB, Result).

:- pred compare_proc_active_rows_by_name(maybe(string)::in, module_qual::in,
    perf_row_data(proc_active)::in, perf_row_data(proc_active)::in,
    comparison_result::out) is det.

compare_proc_active_rows_by_name(MaybeCurModuleName, ModuleQual,
        ModuleRowDataA, ModuleRowDataB, Result) :-
    ProcDescA = ModuleRowDataA ^ perf_row_subject ^ pa_proc_desc,
    ProcDescB = ModuleRowDataB ^ perf_row_subject ^ pa_proc_desc,
    compare_proc_descs_by_name(MaybeCurModuleName, ModuleQual,
        ProcDescA, ProcDescB, Result).

%---------------------------------------------------------------------------%
%
% Sort perf_data_rows of module_getter_setters by the preferred criteria.
%

:- pred sort_getter_setter_fields(preferences::in,
    assoc_list(field_name, gs_field_info)::in,
    assoc_list(field_name, gs_field_info)::out) is det.

sort_getter_setter_fields(Prefs, !FieldPairs) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        % In the common case, each FieldPair has two contexts, one each from
        % the getter and the setter. Neither context is all that useful to sort
        % by, so we sort by the field name instead.
        ( OrderCriteria = by_context
        ; OrderCriteria = by_name
        ),
        list.sort(compare_getter_setters_by_name, !FieldPairs)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_getter_setters_by_cost(CostKind, InclDesc, Scope),
            !FieldPairs),
        % We want the most expensive fields to appear first.
        list.reverse(!FieldPairs)
    ).

:- pred compare_getter_setters_by_name(
    pair(field_name, gs_field_info)::in, pair(field_name, gs_field_info)::in,
    comparison_result::out) is det.

compare_getter_setters_by_name(PairA, PairB, Result) :-
    PairA = FieldNameA - _,
    PairB = FieldNameB - _,
    compare(Result, FieldNameA, FieldNameB).

:- pred compare_getter_setters_by_cost(
    cost_kind::in, include_descendants::in, measurement_scope::in,
    pair(field_name, gs_field_info)::in, pair(field_name, gs_field_info)::in,
    comparison_result::out) is det.

compare_getter_setters_by_cost(CostKind, InclDesc, Scope, PairA, PairB,
        Result) :-
    PairA = FieldNameA - FieldInfoA,
    PairB = FieldNameB - FieldInfoB,
    PerfA = representative_field_perf_row(FieldInfoA),
    PerfB = representative_field_perf_row(FieldInfoB),
    compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope, PerfA, PerfB,
        PerfResult),
    (
        ( PerfResult = (<)
        ; PerfResult = (>)
        ),
        Result = PerfResult
    ;
        PerfResult = (=),
        % We switch the field names to rank in reverse alphabetical order.
        % We do this because our caller will reverse the final sorted list,
        % and *this* reversal is needed to undo the effects of *that* reversal.
        compare(Result, FieldNameB, FieldNameA)
    ).

:- func representative_field_perf_row(gs_field_info) = perf_row_data(unit).

representative_field_perf_row(FieldInfo) = Perf :-
    (
        FieldInfo = gs_field_getter(Perf0),
        Perf = Perf0 ^ perf_row_subject := unit
    ;
        FieldInfo = gs_field_setter(Perf0),
        Perf = Perf0 ^ perf_row_subject := unit
    ;
        FieldInfo = gs_field_both(_, _, Perf)
    ).

%---------------------------------------------------------------------------%
%
% Sort perf_data_rows of module_actives by the preferred criteria.
%

:- pred sort_module_active_rows_by_preferences(preferences::in,
    list(perf_row_data(module_active))::in,
    list(perf_row_data(module_active))::out) is det.

sort_module_active_rows_by_preferences(Prefs, !ModuleRowDatas) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        % A context is a filename and line number. The filename is derived
        % from the module name, and for modules, the line number part of the
        % context isn't relevant. Therefore sorting by context is equivalent to
        % sorting by name.
        ( OrderCriteria = by_context
        ; OrderCriteria = by_name
        ),
        list.sort(compare_module_active_rows_by_name, !ModuleRowDatas)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope),
            !ModuleRowDatas),
        % We want the most expensive rows to appear first.
        list.reverse(!ModuleRowDatas)
    ).

:- pred compare_module_active_rows_by_name(
    perf_row_data(module_active)::in, perf_row_data(module_active)::in,
    comparison_result::out) is det.

compare_module_active_rows_by_name(ModuleRowDataA, ModuleRowDataB, Result) :-
    ModuleDescA = ModuleRowDataA ^ perf_row_subject,
    ModuleDescB = ModuleRowDataB ^ perf_row_subject,
    ModuleNameA = ModuleDescA ^ ma_module_name,
    ModuleNameB = ModuleDescB ^ ma_module_name,
    compare(Result, ModuleNameA, ModuleNameB).

%---------------------------------------------------------------------------%
%
% Sort perf_data_rows of module names by the preferred criteria.
%

:- pred sort_module_name_rows_by_preferences(preferences::in,
    list(perf_row_data(string))::in, list(perf_row_data(string))::out) is det.

sort_module_name_rows_by_preferences(Prefs, !ModuleRowDatas) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        % A context is a filename and line number. The filename is derived
        % from the module name, and for modules, the line number part of the
        % context isn't relevant. Therefore sorting by context is equivalent to
        % sorting by name.
        ( OrderCriteria = by_context
        ; OrderCriteria = by_name
        ),
        list.sort(compare_module_name_rows_by_name, !ModuleRowDatas)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope),
            !ModuleRowDatas),
        % We want the most expensive rows to appear first.
        list.reverse(!ModuleRowDatas)
    ).

:- pred compare_module_name_rows_by_name(
    perf_row_data(string)::in, perf_row_data(string)::in,
    comparison_result::out) is det.

compare_module_name_rows_by_name(ModuleRowDataA, ModuleRowDataB, Result) :-
    ModuleNameA = ModuleRowDataA ^ perf_row_subject,
    ModuleNameB = ModuleRowDataB ^ perf_row_subject,
    compare(Result, ModuleNameA, ModuleNameB).

%---------------------------------------------------------------------------%
%
% Sort perf_data_rows of cliques by the preferred criteria.
%

:- pred sort_clique_rows_by_preferences(preferences::in,
    list(perf_row_data(clique_desc))::in,
    list(perf_row_data(clique_desc))::out) is det.

sort_clique_rows_by_preferences(Prefs, !CliqueRowDatas) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        OrderCriteria = by_context,
        % For cliques, we don't have have a single context. We could use the
        % contexts of the procedures in the clique, but using the clique number
        % seems more useful.
        list.sort(compare_clique_rows_by_number, !CliqueRowDatas)
    ;
        OrderCriteria = by_name,
        list.sort(compare_clique_rows_by_first_proc_name, !CliqueRowDatas)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope),
            !CliqueRowDatas),
        % We want the most expensive rows to appear first.
        list.reverse(!CliqueRowDatas)
    ).

:- pred compare_clique_rows_by_number(
    perf_row_data(clique_desc)::in, perf_row_data(clique_desc)::in,
    comparison_result::out) is det.

compare_clique_rows_by_number(CliqueRowDataA, CliqueRowDataB, Result) :-
    CliqueDescA = CliqueRowDataA ^ perf_row_subject,
    CliqueDescB = CliqueRowDataB ^ perf_row_subject,
    CliqueDescA ^ cdesc_clique_ptr = clique_ptr(CliqueNumA),
    CliqueDescB ^ cdesc_clique_ptr = clique_ptr(CliqueNumB),
    compare(Result, CliqueNumA, CliqueNumB).

:- pred compare_clique_rows_by_first_proc_name(
    perf_row_data(clique_desc)::in, perf_row_data(clique_desc)::in,
    comparison_result::out) is det.

compare_clique_rows_by_first_proc_name(CliqueRowDataA, CliqueRowDataB,
        Result) :-
    CliqueDescA = CliqueRowDataA ^ perf_row_subject,
    CliqueDescB = CliqueRowDataB ^ perf_row_subject,
    EntryProcDescA = CliqueDescA ^ cdesc_entry_member,
    EntryProcDescB = CliqueDescB ^ cdesc_entry_member,
    compare_proc_descs_by_context(EntryProcDescA, EntryProcDescB, Result).

%---------------------------------------------------------------------------%
%
% Sort call_site_descs and proc_descs by context and by name.
%

:- pred compare_call_site_descs_by_context(
    call_site_desc::in, call_site_desc::in, comparison_result::out) is det.

compare_call_site_descs_by_context(CallSiteDescA, CallSiteDescB, Result) :-
    FileNameA = CallSiteDescA ^ csdesc_file_name,
    FileNameB = CallSiteDescB ^ csdesc_file_name,
    compare(FileNameResult, FileNameA, FileNameB),
    (
        ( FileNameResult = (<)
        ; FileNameResult = (>)
        ),
        Result = FileNameResult
    ;
        FileNameResult = (=),
        LineNumberA = CallSiteDescA ^ csdesc_line_number,
        LineNumberB = CallSiteDescB ^ csdesc_line_number,
        compare(Result, LineNumberA, LineNumberB)
    ).

:- pred compare_proc_descs_by_context(proc_desc::in, proc_desc::in,
    comparison_result::out) is det.

compare_proc_descs_by_context(ProcDescA, ProcDescB, Result) :-
    FileNameA = ProcDescA ^ pdesc_file_name,
    FileNameB = ProcDescB ^ pdesc_file_name,
    compare(FileNameResult, FileNameA, FileNameB),
    (
        ( FileNameResult = (<)
        ; FileNameResult = (>)
        ),
        Result = FileNameResult
    ;
        FileNameResult = (=),
        LineNumberA = ProcDescA ^ pdesc_line_number,
        LineNumberB = ProcDescB ^ pdesc_line_number,
        compare(Result, LineNumberA, LineNumberB)
    ).

:- pred compare_call_site_descs_by_name(maybe(string)::in, module_qual::in,
    call_site_desc::in, call_site_desc::in, comparison_result::out) is det.

compare_call_site_descs_by_name(MaybeCurModuleName, ModuleQual,
        CallSiteDescA, CallSiteDescB, Result) :-
    NameA = call_site_desc_get_caller_refined_id(MaybeCurModuleName,
        ModuleQual, CallSiteDescA),
    NameB = call_site_desc_get_caller_refined_id(MaybeCurModuleName,
        ModuleQual, CallSiteDescB),
    compare(Result, NameA, NameB).

:- pred compare_call_site_descs_by_callee_name(maybe(string)::in,
    module_qual::in, call_site_desc::in, call_site_desc::in,
    comparison_result::out) is det.

compare_call_site_descs_by_callee_name(MaybeCurModuleName, ModuleQual,
        CallSiteDescA, CallSiteDescB, Result) :-
    MaybeCalleeA = CallSiteDescA ^ csdesc_maybe_callee,
    MaybeCalleeB = CallSiteDescB ^ csdesc_maybe_callee,
    (
        MaybeCalleeA = no,
        MaybeCalleeB = no,
        Result = (=)
    ;
        MaybeCalleeA = no,
        MaybeCalleeB = yes(_),
        Result = (<)
    ;
        MaybeCalleeA = yes(_),
        MaybeCalleeB = no,
        Result = (>)
    ;
        MaybeCalleeA = yes(CalleeNameA),
        MaybeCalleeB = yes(CalleeNameB),
        compare_proc_descs_by_name(MaybeCurModuleName, ModuleQual,
            CalleeNameA, CalleeNameB, Result)
    ).

:- pred compare_proc_descs_by_name(maybe(string)::in, module_qual::in,
    proc_desc::in, proc_desc::in, comparison_result::out) is det.

compare_proc_descs_by_name(MaybeCurModuleName, ModuleQual,
        ProcDescA, ProcDescB, Result) :-
    NameA =
        proc_desc_get_refined_id(MaybeCurModuleName, ModuleQual, ProcDescA),
    NameB =
        proc_desc_get_refined_id(MaybeCurModuleName, ModuleQual, ProcDescB),
    compare(Result, NameA, NameB).

%---------------------------------------------------------------------------%
%
% Sort perf_row_datas by a criterion of performance.
%

:- pred compare_perf_row_datas_by_cost(
    cost_kind::in, include_descendants::in, measurement_scope::in,
    perf_row_data(T)::in, perf_row_data(T)::in, comparison_result::out) is det.

compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope, PerfA, PerfB,
        Result) :-
    (
        CostKind = cost_calls,
        CallsA = PerfA ^ perf_row_calls,
        CallsB = PerfB ^ perf_row_calls,
        compare(Result, CallsA, CallsB)
    ;
        CostKind = cost_redos,
        RedosA = PerfA ^ perf_row_redos,
        RedosB = PerfB ^ perf_row_redos,
        compare(Result, RedosA, RedosB)
    ;
        CostKind = cost_time,
        compare_perf_row_datas_by_time(InclDesc, Scope, PerfA, PerfB,
            TimeResult),
        (
            TimeResult = (=),
            compare_perf_row_datas_by_callseqs(InclDesc, Scope, PerfA, PerfB,
                Result)
        ;
            ( TimeResult = (<)
            ; TimeResult = (>)
            ),
            Result = TimeResult
        )
    ;
        CostKind = cost_callseqs,
        compare_perf_row_datas_by_callseqs(InclDesc, Scope, PerfA, PerfB,
            Result)
    ;
        CostKind = cost_allocs,
        compare_perf_row_datas_by_allocs(InclDesc, Scope, PerfA, PerfB, Result)
    ;
        CostKind = cost_words,
        compare_perf_row_datas_by_words(InclDesc, Scope, PerfA, PerfB, Result)
    ).

:- pred compare_perf_row_datas_by_time(
    include_descendants::in, measurement_scope::in,
    perf_row_data(T)::in, perf_row_data(T)::in, comparison_result::out) is det.

compare_perf_row_datas_by_time(InclDesc, Scope, PerfA, PerfB, Result) :-
    (
        InclDesc = self,
        SelfA = PerfA ^ perf_row_self,
        SelfB = PerfB ^ perf_row_self,
        (
            Scope = overall,
            TimeA = SelfA ^ perf_row_time,
            TimeB = SelfB ^ perf_row_time,
            compare(Result, TimeA, TimeB)
        ;
            Scope = per_call,
            TimeA = SelfA ^ perf_row_time_percall,
            TimeB = SelfB ^ perf_row_time_percall,
            compare(Result, TimeA, TimeB)
        )
    ;
        InclDesc = self_and_desc,
        MaybeTotalA = PerfA ^ perf_row_maybe_total,
        MaybeTotalB = PerfB ^ perf_row_maybe_total,
        ( if
            MaybeTotalA = yes(TotalA),
            MaybeTotalB = yes(TotalB)
        then
            (
                Scope = overall,
                TimeA = TotalA ^ perf_row_time,
                TimeB = TotalB ^ perf_row_time,
                compare(Result, TimeA, TimeB)
            ;
                Scope = per_call,
                TimeA = TotalA ^ perf_row_time_percall,
                TimeB = TotalB ^ perf_row_time_percall,
                compare(Result, TimeA, TimeB)
            )
        else
            unexpected($module, $pred, "self_and_desc")
        )
    ).

:- pred compare_perf_row_datas_by_callseqs(
    include_descendants::in, measurement_scope::in,
    perf_row_data(T)::in, perf_row_data(T)::in, comparison_result::out) is det.

compare_perf_row_datas_by_callseqs(InclDesc, Scope, PerfA, PerfB, Result) :-
    (
        InclDesc = self,
        SelfA = PerfA ^ perf_row_self,
        SelfB = PerfB ^ perf_row_self,
        (
            Scope = overall,
            CallSeqsA = SelfA ^ perf_row_callseqs,
            CallSeqsB = SelfB ^ perf_row_callseqs,
            compare(Result, CallSeqsA, CallSeqsB)
        ;
            Scope = per_call,
            CallSeqsA = SelfA ^ perf_row_callseqs_percall,
            CallSeqsB = SelfB ^ perf_row_callseqs_percall,
            compare(Result, CallSeqsA, CallSeqsB)
        )
    ;
        InclDesc = self_and_desc,
        MaybeTotalA = PerfA ^ perf_row_maybe_total,
        MaybeTotalB = PerfB ^ perf_row_maybe_total,
        ( if
            MaybeTotalA = yes(TotalA),
            MaybeTotalB = yes(TotalB)
        then
            (
                Scope = overall,
                CallSeqsA = TotalA ^ perf_row_callseqs,
                CallSeqsB = TotalB ^ perf_row_callseqs,
                compare(Result, CallSeqsA, CallSeqsB)
            ;
                Scope = per_call,
                CallSeqsA = TotalA ^ perf_row_callseqs_percall,
                CallSeqsB = TotalB ^ perf_row_callseqs_percall,
                compare(Result, CallSeqsA, CallSeqsB)
            )
        else
            unexpected($module, $pred, "self_and_desc")
        )
    ).

:- pred compare_perf_row_datas_by_allocs(
    include_descendants::in, measurement_scope::in,
    perf_row_data(T)::in, perf_row_data(T)::in, comparison_result::out) is det.

compare_perf_row_datas_by_allocs(InclDesc, Scope, PerfA, PerfB, Result) :-
    (
        InclDesc = self,
        SelfA = PerfA ^ perf_row_self,
        SelfB = PerfB ^ perf_row_self,
        (
            Scope = overall,
            AllocsA = SelfA ^ perf_row_allocs,
            AllocsB = SelfB ^ perf_row_allocs,
            compare(Result, AllocsA, AllocsB)
        ;
            Scope = per_call,
            AllocsA = SelfA ^ perf_row_allocs_percall,
            AllocsB = SelfB ^ perf_row_allocs_percall,
            compare(Result, AllocsA, AllocsB)
        )
    ;
        InclDesc = self_and_desc,
        MaybeTotalA = PerfA ^ perf_row_maybe_total,
        MaybeTotalB = PerfB ^ perf_row_maybe_total,
        ( if
            MaybeTotalA = yes(TotalA),
            MaybeTotalB = yes(TotalB)
        then
            (
                Scope = overall,
                AllocsA = TotalA ^ perf_row_allocs,
                AllocsB = TotalB ^ perf_row_allocs,
                compare(Result, AllocsA, AllocsB)
            ;
                Scope = per_call,
                AllocsA = TotalA ^ perf_row_allocs_percall,
                AllocsB = TotalB ^ perf_row_allocs_percall,
                compare(Result, AllocsA, AllocsB)
            )
        else
            unexpected($module, $pred, "missing total")
        )
    ).

:- pred compare_perf_row_datas_by_words(
    include_descendants::in, measurement_scope::in,
    perf_row_data(T)::in, perf_row_data(T)::in, comparison_result::out) is det.

compare_perf_row_datas_by_words(InclDesc, Scope, PerfA, PerfB, Result) :-
    (
        InclDesc = self,
        SelfA = PerfA ^ perf_row_self,
        SelfB = PerfB ^ perf_row_self,
        (
            Scope = overall,
            MemoryA = SelfA ^ perf_row_mem,
            MemoryB = SelfB ^ perf_row_mem,
            compare_memory(MemoryA, MemoryB, Result)
        ;
            Scope = per_call,
            MemoryA = SelfA ^ perf_row_mem_percall,
            MemoryB = SelfB ^ perf_row_mem_percall,
            compare_memory(MemoryA, MemoryB, Result)
        )
    ;
        InclDesc = self_and_desc,
        MaybeTotalA = PerfA ^ perf_row_maybe_total,
        MaybeTotalB = PerfB ^ perf_row_maybe_total,
        ( if
            MaybeTotalA = yes(TotalA),
            MaybeTotalB = yes(TotalB)
        then
            (
                Scope = overall,
                MemoryA = TotalA ^ perf_row_mem,
                MemoryB = TotalB ^ perf_row_mem,
                compare_memory(MemoryA, MemoryB, Result)
            ;
                Scope = per_call,
                MemoryA = TotalA ^ perf_row_mem_percall,
                MemoryB = TotalB ^ perf_row_mem_percall,
                compare_memory(MemoryA, MemoryB, Result)
            )
        else
            unexpected($module, $pred, "missing total")
        )
    ).

%---------------------------------------------------------------------------%

:- func proc_desc_get_refined_id(maybe(string), module_qual, proc_desc)
    = string.

proc_desc_get_refined_id(MaybeCurModuleName, ModuleQual, ProcDesc) = Name :-
    (
        ModuleQual = module_qual_never,
        Name = ProcDesc ^ pdesc_uq_refined_name
    ;
        ModuleQual = module_qual_when_diff,
        (
            MaybeCurModuleName = no,
            Name = ProcDesc ^ pdesc_q_refined_name
        ;
            MaybeCurModuleName = yes(CurModuleName),
            ModuleName = ProcDesc ^ pdesc_module_name,
            ( if ModuleName = CurModuleName then
                Name = ProcDesc ^ pdesc_uq_refined_name
            else
                Name = ProcDesc ^ pdesc_q_refined_name
            )
        )
    ;
        ModuleQual = module_qual_always,
        Name = ProcDesc ^ pdesc_q_refined_name
    ).

:- func call_site_desc_get_caller_refined_id(maybe(string), module_qual,
    call_site_desc) = string.

call_site_desc_get_caller_refined_id(MaybeCurModuleName, ModuleQual,
        CallSiteDesc) = Name :-
    (
        ModuleQual = module_qual_never,
        Name = CallSiteDesc ^ csdesc_caller_uq_refined_name
    ;
        ModuleQual = module_qual_when_diff,
        (
            MaybeCurModuleName = no,
            Name = CallSiteDesc ^ csdesc_caller_q_refined_name
        ;
            MaybeCurModuleName = yes(CurModuleName),
            ModuleName = CallSiteDesc ^ csdesc_caller_module_name,
            ( if ModuleName = CurModuleName then
                Name = CallSiteDesc ^ csdesc_caller_uq_refined_name
            else
                Name = CallSiteDesc ^ csdesc_caller_q_refined_name
            )
        )
    ;
        ModuleQual = module_qual_always,
        Name = CallSiteDesc ^ csdesc_caller_q_refined_name
    ).

%---------------------------------------------------------------------------%
:- end_module display_report.
%---------------------------------------------------------------------------%
