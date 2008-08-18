%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: display_report.m.
% Author: pbone.
%
% This module contains code to create a display data structure from a deep
% profiling report.
%
%-----------------------------------------------------------------------------%

:- module display_report.
:- interface.

:- import_module display.
:- import_module profile.
:- import_module report.

% XXX: This include should be removed or replaced.  Some data structures
% such as preferences are currently defined in query, they should be moved
% into a different module so that this module doesn't need to include
% the whole of query.
:- import_module query.

%-----------------------------------------------------------------------------%

:- func report_to_display(deep, preferences, deep_report) = display.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module measurement_units.

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

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
            display_report_proc(Prefs, ProcReport, Display)
        ;
            MaybeProcReport = error(Msg),
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
    ).

%-----------------------------------------------------------------------------%
%
% Code to display menu report.
%

:- pred display_report_menu(deep::in, preferences::in, menu_report::in,
    display::out) is det.

display_report_menu(Deep, Prefs, MenuReport, Display) :-
    MenuReport = menu_report(ProgramName, QuantaPerSec, UserQuanta, InstQuanta,
        NumCallseqs, NumCSD, NumCSS, NumPD, NumPS, NumClique),

    ShouldDisplayTimes = should_display_times(Deep),

    % Display the links section of the report.
    LinksExploration =
        [(deep_cmd_root(no) -
            "Exploring the call graph, starting at the root."),
         (deep_cmd_root(yes(90)) -
            "Exploring the call graph, starting at the action."),
         (deep_cmd_modules -
            "Exploring the program module by module.")],

    (
        ShouldDisplayTimes = yes,
        LinksTopProcsByLimitTime =
            [(deep_cmd_top_procs(rank_range(1, 100), cost_time, self, overall)
                - "Top 100 most expensive procedures: time, self."),
             (deep_cmd_top_procs(rank_range(1, 100), cost_time, self_and_desc,
                    overall) -
                "Top 100 most expensive procedures: time, self+descendants.")]
    ;
        ShouldDisplayTimes = no,
        LinksTopProcsByLimitTime = []
    ),

    LinksTopProcsByLimit =
        [(deep_cmd_top_procs(rank_range(1, 100), cost_callseqs, self,
                overall) -
            "Top 100 most expensive procedures: callseqs, self."),
         (deep_cmd_top_procs(rank_range(1, 100), cost_callseqs, self_and_desc,
                overall) -
            "Top 100 most expensive procedures: callseqs, self+descendants."),
         (deep_cmd_top_procs(rank_range(1, 100), cost_words, self, overall) -
            "Top 100 most expensive procedures: words, self."),
         (deep_cmd_top_procs(rank_range(1, 100), cost_words, self_and_desc,
                overall) -
            "Top 100 most expensive procedures: words, self+descendants.")],

    (
        ShouldDisplayTimes = yes,
        LinksTopProcsByPercentTime =
            [(deep_cmd_top_procs(threshold_percent(0.1), cost_time, self,
                    overall) -
                "Procedures above 0.1% threshold: time, self."),
             (deep_cmd_top_procs(threshold_percent(1.0), cost_time,
                    self_and_desc, overall) -
                "Procedures above 1% threshold: time, self+descendants."),
             (deep_cmd_top_procs(threshold_value(100.0), cost_time,
                    self_and_desc, overall) -
                "Procedures above 1 second threshold: time, self+descendants."
             )]
    ;
        ShouldDisplayTimes = no,
        LinksTopProcsByPercentTime = []
    ),

    LinksTopProcsByPercent =
        [(deep_cmd_top_procs(threshold_percent(0.1), cost_callseqs, self,
                overall) -
            "Procedures above 0.1% threshold: callseqs, self."),
         (deep_cmd_top_procs(threshold_percent(1.0), cost_callseqs,
                self_and_desc, overall) -
            "Procedures above 1% threshold: callseqs, self+descendants."),
         (deep_cmd_top_procs(threshold_value(1000000.0), cost_callseqs,
                self_and_desc, overall) -
            ("Procedures above 1,000,000 callseqs threshold: callseqs, " ++
                "self+descendants.")),
         (deep_cmd_top_procs(threshold_percent(0.1), cost_words, self,
                overall) -
            "Procedures above 0.1% threshold: words, self."),
         (deep_cmd_top_procs(threshold_percent(1.0), cost_words,
                self_and_desc, overall) -
            "Procedures above 1% threshold: words, self+descendants."),
         % 2M words is chosen because it is 8MB on ia32.
         (deep_cmd_top_procs(threshold_value(float(1024 * 1024 * 2)),
                cost_words, self_and_desc, overall) -
            "Procedures above 2M words threshold: words, self+descendants.")],

    LinkCmds = LinksExploration ++
        LinksTopProcsByLimitTime ++ LinksTopProcsByLimit ++
        LinksTopProcsByPercentTime ++ LinksTopProcsByPercent,
    list.map(make_link, LinkCmds, LinksList),
    Links = display_list(list_class_vertical_bullets,
        yes("You can start exploring the deep profile at the following" ++
            " points."), LinksList),

    % Display the table section of the report.
    ProfilingStatistics =
        [("Profile generated for:"      - td_s(ProgramName)),
        ("Quanta per second:"           - td_i(QuantaPerSec)),
        ("Quanta in user code:"         - td_i(UserQuanta)),
        ("Quanta in instrumentation:"   - td_i(InstQuanta)),
        ("Call sequence numbers:"       - td_i(NumCallseqs)),
        ("CallSiteDyanic structures:"   - td_i(NumCSD)),
        ("ProcDynamic structures:"      - td_i(NumPD)),
        ("CallSiteStatic structures:"   - td_i(NumCSS)),
        ("ProcStatic structures:"       - td_i(NumPS)),
        ("Cliques:"                     - td_i(NumClique))],

    Rows = list.map(make_labelled_table_row, ProfilingStatistics),
    Table = table(table_class_do_not_box, 2, no, Rows),

    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),

    % Construct the complete representation of what to display.
    Display = display(yes("Deep profiler menu"),
        [Links, display_table(Table),
        display_paragraph_break, MenuRestartQuitControls]).

%-----------------------------------------------------------------------------%
%
% Code to display a top procedures report.
%

    % Create a display_report structure for a top_procedures report.
    %
:- pred display_report_top_procs(preferences::in, top_procs_report::in,
    display::out) is det.

display_report_top_procs(Prefs, TopProcsReport, Display) :-
    TopProcsReport = top_procs_report(Ordering, TopProcs),
    Ordering = report_ordering(DisplayLimit, CostKind, InclDesc, Scope),
    Desc = cost_criteria_to_description(CostKind, InclDesc, Scope),
    Title = "Top procedures " ++ Desc,

    % Build the table of the top procedures.
    MakeHeaderData = top_procs_order_criteria_header_data(DisplayLimit,
        CostKind, InclDesc, Scope),
    maybe_ranked_proc_table_header(Prefs, ranked, MakeHeaderData,
        NumColumns, Header),
    list.map_foldl(
        maybe_ranked_subject_perf_table_row(Prefs, ranked, proc_desc_to_cell),
        TopProcs, Rows, 1, _),
    Table = table(table_class_box_if_pref, NumColumns, yes(Header), Rows),
    DisplayTable = display_table(Table),

    % Build controls at the bottom of page.
    Cmd = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope),
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

%-----------------------------------------------------------------------------%
%
% Code to display a procedure report.
%

    % Create a display_report structure for a proc report.
    %
:- pred display_report_proc(preferences::in, proc_report::in,
    display::out) is det.

display_report_proc(Prefs, ProcReport, Display) :-
    ProcReport = proc_report(PSPtr, ProcSummaryRowData, CallSitePerfs0),
    ProcDesc = ProcSummaryRowData ^ perf_row_subject,
    RefinedName = ProcDesc ^ proc_desc_refined_name,
    Title = "Summary of procedure " ++ RefinedName,

    PSPtr = proc_static_ptr(PSI),
    Cmd = deep_cmd_proc(PSI),

    SourceHeaderGroup = make_single_table_header_group(td_s("Source"),
        table_column_class_source_context, column_do_not_colour),
    ProcHeaderGroup = make_single_table_header_group(td_s("Procedure"),
        table_column_class_proc, column_do_not_colour),
    MakeHeaderData = override_order_criteria_header_data(Cmd),
    perf_table_header(Prefs, MakeHeaderData, PerfHeaderGroups),
    AllHeaderGroups =
        [SourceHeaderGroup, ProcHeaderGroup] ++ PerfHeaderGroups,
    header_groups_to_header(AllHeaderGroups, NumColumns, Header),

    % We could make SummaryProcCell a link, but the only link that would make
    % sense (and the link that pre-display versions of the deep profiler
    % generated) point back to this page itself, which is not useful and
    % could be considered misleading.
    %
    % SummaryProcCell spans two columns: the ones that contain (1) the context
    % and (2) the callee of each call site in the rows below.
    SummaryProcCell = table_cell(td_s(RefinedName), 2),
    Fields = Prefs ^ pref_fields,
    perf_table_row(Fields, ProcSummaryRowData, SummaryPerfCells),
    SummaryCells = [SummaryProcCell] ++ SummaryPerfCells,
    SummaryRow = table_row(SummaryCells),

    sort_call_sites_by_preferences(Prefs, CallSitePerfs0, CallSitePerfs),
    CallSiteRowLists = list.map(report_proc_call_site(Prefs), CallSitePerfs),
    list.condense(CallSiteRowLists, CallSiteRows),
    AllRows = [SummaryRow, table_separator_row] ++ CallSiteRows,
    Table = table(table_class_box_if_pref, NumColumns, yes(Header), AllRows),
    DisplayTable = display_table(Table),

    % Build the controls at the bottom of page.
    ParentControls = proc_parent_controls(Prefs, PSI),
    SummarizeControls = summarize_controls(Prefs, Cmd),
    SortControls = sort_controls(Prefs, Cmd),
    FieldControls = field_controls(Prefs, Cmd),
    FormatControls = format_controls(Prefs, Cmd),
    MenuRestartQuitControls = cmds_menu_restart_quit(yes(Prefs)),

    Display = display(yes(Title),
        [DisplayTable,
        display_paragraph_break, ParentControls,
        display_paragraph_break, SummarizeControls,
        display_paragraph_break, SortControls,
        display_paragraph_break, FieldControls,
        display_paragraph_break, FormatControls,
        display_paragraph_break, MenuRestartQuitControls]).

:- func proc_parent_controls(preferences, int) = display_item.

proc_parent_controls(Prefs, PSI) = ControlsItem :-
    OrderedByCallSiteCmd = deep_cmd_proc_callers(PSI, group_by_call_site, 1),
    OrderedByCallSite = display_link(deep_link(OrderedByCallSiteCmd,
        yes(Prefs), "Ordered by call site", link_class_control)),

    OrderedByProcCmd = deep_cmd_proc_callers(PSI, group_by_proc, 1),
    OrderedByProc = display_link(deep_link(OrderedByProcCmd,
        yes(Prefs), "Ordered by procedure", link_class_control)),

    OrderedByModuleCmd = deep_cmd_proc_callers(PSI, group_by_module, 1),
    OrderedByModule = display_link(deep_link(OrderedByModuleCmd,
        yes(Prefs), "Ordered by module", link_class_control)),

    OrderedByCliqueCmd = deep_cmd_proc_callers(PSI, group_by_clique, 1),
    OrderedByClique = display_link(deep_link(OrderedByCliqueCmd,
        yes(Prefs), "Ordered by clique", link_class_control)),

    List = display_list(list_class_horizontal, no,
        [OrderedByCallSite, OrderedByProc, OrderedByModule, OrderedByClique]),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("The procedure's callers:"), [List]).

:- func report_proc_call_site(preferences, call_site_perf) = list(table_row).

report_proc_call_site(Prefs, CallSitePerf) = Rows :-
    CallSitePerf =
        call_site_perf(KindAndCallee, SummaryPerfRowData, SubPerfs0),

    CallSiteDesc = SummaryPerfRowData ^ perf_row_subject,
    FileName = CallSiteDesc ^ call_site_desc_file_name,
    LineNumber = CallSiteDesc ^ call_site_desc_line_number,
    Context = string.format("%s:%d", [s(FileName), i(LineNumber)]),
    ContextCell = table_cell(td_s(Context), 1),

    (
        KindAndCallee = normal_call_and_info(NormalCalleeId),
        NormalCalleeId = normal_callee_id(CalleeDesc, TypeSubstStr),
        CalleeRefinedName = CalleeDesc ^ proc_desc_refined_name,
        ( TypeSubstStr = "" ->
            CallSiteStr = CalleeRefinedName
        ;
            CallSiteStr = string.format("%s [%s]",
                [s(CalleeRefinedName), s(TypeSubstStr)])
        ),
        CalleePSPtr = CalleeDesc ^ proc_desc_static_ptr,
        CalleePSPtr = proc_static_ptr(CalleePSI),
        CallSiteLinkCmd = deep_cmd_proc(CalleePSI),
        CallSiteLink = deep_link(CallSiteLinkCmd, yes(Prefs),
            CallSiteStr, link_class_link),
        CallSiteCell = table_cell(td_l(CallSiteLink), 1),

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
        CallSiteCell = table_cell(td_s(CallSiteStr), 1)
    ),

    Fields = Prefs ^ pref_fields,
    perf_table_row(Fields, SummaryPerfRowData, SummaryPerfCells),
    SummaryCells = [ContextCell, CallSiteCell] ++ SummaryPerfCells,
    SummaryRow = table_row(SummaryCells),

    Summarize = Prefs ^ pref_summarize,
    (
        Summarize = summarize,
        Rows = [SummaryRow]
    ;
        Summarize = do_not_summarize,
        sort_proc_desc_rows_by_preferences(Prefs, SubPerfs0, SubPerfs),
        SubRows = list.map(report_proc_call_site_callee(Prefs), SubPerfs),
        Rows = [SummaryRow] ++ SubRows
    ).

:- func report_proc_call_site_callee(preferences, perf_row_data(proc_desc))
    = table_row.

report_proc_call_site_callee(Prefs, RowData) = Row :-
    Fields = Prefs ^ pref_fields,

    EmptyCell = table_empty_cell,

    ProcDesc = RowData ^ perf_row_subject,
    ProcDesc = proc_desc(PSPtr, _FileName, _LineNumber, RefinedName),
    PSPtr = proc_static_ptr(PSI),
    ProcLinkCmd = deep_cmd_proc(PSI),
    ProcLink = deep_link(ProcLinkCmd, yes(Prefs), RefinedName,
        link_class_link),
    ProcCell = table_cell(td_l(ProcLink), 1),

    perf_table_row(Fields, RowData, PerfCells),

    Cells = [EmptyCell, ProcCell] ++ PerfCells,
    Row = table_row(Cells).

%-----------------------------------------------------------------------------%
%
% Code to display proc_static and proc_dynamic dumps.
%

    % Create a display_report structure for a proc_static_dump report.
    %
:- pred display_report_proc_static_dump(proc_static_dump_info::in,
    display::out) is det.

display_report_proc_static_dump(ProcStaticDumpInfo, Display) :-
    ProcStaticDumpInfo = proc_static_dump_info(PSPtr, RawName, RefinedName,
        FileName, LineNumber, NumCallSites),
    PSPtr = proc_static_ptr(PSI),
    string.format("Dump of proc_static %d", [i(PSI)], Title),

    Values =
        [("Raw name:"               - td_s(RawName)),
        ("Refined name:"            - td_s(RefinedName)),
        ("File name:"               - td_s(FileName)),
        ("Line number:"             - td_i(LineNumber)),
        ("Number of call sites:"    - td_i(NumCallSites))],

    Rows = list.map(make_labelled_table_row, Values),
    Table = table(table_class_do_not_box, 2, no, Rows),
    Display = display(yes(Title), [display_table(Table)]).

    % Create a display_report structure for a proc_dynamic_dump report.
    %
:- pred display_report_proc_dynamic_dump(deep::in, preferences::in,
    proc_dynamic_dump_info::in, display::out) is det.

display_report_proc_dynamic_dump(_Deep, Prefs, ProcDynamicDumpInfo, Display) :-
    ProcDynamicDumpInfo = proc_dynamic_dump_info(PDPtr, PSPtr,
        RawName, RefinedName, CallSites),
    PDPtr = proc_dynamic_ptr(PDI),
    PSPtr = proc_static_ptr(PSI),
    string.format("Dump of proc_dynamic %d", [i(PDI)], Title),

    ProcStaticLink = deep_link(deep_cmd_proc_static(PSI), yes(Prefs),
        string.int_to_string(PSI), link_class_link),
    MainValues =
        [("Proc static:"            - td_l(ProcStaticLink)),
        ("Raw name:"                - td_s(RawName)),
        ("Refined name:"            - td_s(RefinedName))],

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

    Display = display(yes(Title),
        [MainTableItem, CallSitesTitleItem, CallSitesTableItem]).

:- pred dump_psd_call_site(preferences::in,
    call_site_array_slot::in, list(table_row)::out,
    counter::in, counter::out) is det.

dump_psd_call_site(Prefs, CallSite, Rows, !CallSiteCounter) :-
    counter.allocate(CallSiteNum, !CallSiteCounter),
    CallSiteNumCell = table_cell(td_i(CallSiteNum), 1),
    (
        CallSite = slot_normal(CSDPtr),
        CSDPtr = call_site_dynamic_ptr(CSDI),
        CSDLink = deep_link(deep_cmd_call_site_dynamic(CSDI), yes(Prefs),
            string.int_to_string(CSDI), link_class_link),
        CSDCell = table_cell(td_l(CSDLink), 1),
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
        MultiCell = table_cell(td_s(MultiCellStr), 1),
        FirstRow = table_row([CallSiteNumCell, MultiCell]),
        list.map(dump_psd_call_site_multi_entry(Prefs), CSDPtrs, LaterRows),
        Rows = [FirstRow | LaterRows]
    ).

:- pred dump_psd_call_site_multi_entry(preferences::in,
    call_site_dynamic_ptr::in, table_row::out) is det.

dump_psd_call_site_multi_entry(Prefs, CSDPtr, Row) :-
    CSDPtr = call_site_dynamic_ptr(CSDI),
    CSDLink = deep_link(deep_cmd_call_site_dynamic(CSDI), yes(Prefs),
        string.int_to_string(CSDI), link_class_link),
    CSDCell = table_cell(td_l(CSDLink), 1),
    EmptyCell = table_cell(td_s(""), 1),
    Row = table_row([EmptyCell, CSDCell]).

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

    ContainingProcStaticLink = deep_link(deep_cmd_proc_static(ContainingPSI),
        yes(Prefs), string.int_to_string(ContainingPSI), link_class_link),

    (
        CallSiteKind = normal_call_and_callee(CalleePSPtr, TypeSpecDesc),
        CalleePSPtr = proc_static_ptr(CalleePSI),
        CalleeDesc0 = "normal, callee " ++ string.int_to_string(CalleePSI),
        ( TypeSpecDesc = "" ->
            CalleeDesc = CalleeDesc0
        ;
            CalleeDesc = CalleeDesc0 ++ " typespec " ++ TypeSpecDesc
        ),
        CalleeProcStaticLink = deep_link(deep_cmd_proc_static(CalleePSI),
            yes(Prefs), CalleeDesc, link_class_link),
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
        ("Goal path:"               - td_s(GoalPath)),
        ("Call site kind:"          - CallSiteKindData)],

    Rows = list.map(make_labelled_table_row, Values),
    Table = table(table_class_do_not_box, 2, no, Rows),
    Display = display(yes(Title), [display_table(Table)]).

    % Create a display_report structure for a call_site_dynamic_dump report.
    %
:- pred display_report_call_site_dynamic_dump(preferences::in,
    call_site_dynamic_dump_info::in, display::out) is det.

display_report_call_site_dynamic_dump(Prefs, CallSiteStaticDumpInfo,
        Display) :-
    CallSiteStaticDumpInfo = call_site_dynamic_dump_info(CSDPtr,
        CallerPSPtr, CalleePSPtr, RowData),
    CSDPtr = call_site_dynamic_ptr(CSDI),
    string.format("Dump of call_site_dynamic %d", [i(CSDI)], Title),

    CallerPSPtr = proc_dynamic_ptr(CallerPSI),
    CallerProcDynamicLink = deep_link(deep_cmd_proc_dynamic(CallerPSI),
        yes(Prefs), string.int_to_string(CallerPSI), link_class_link),

    CalleePSPtr = proc_dynamic_ptr(CalleePSI),
    CalleeProcDynamicLink = deep_link(deep_cmd_proc_dynamic(CalleePSI),
        yes(Prefs), string.int_to_string(CalleePSI), link_class_link),

    FirstValues =
        [("Caller proc_dynamic:"    - td_l(CallerProcDynamicLink)),
        ("Callee proc_dynamic:"     - td_l(CalleeProcDynamicLink))],

    FirstRows = list.map(make_labelled_table_row, FirstValues),
    FirstTable = table(table_class_do_not_box, 2, no, FirstRows),

    MakeHeaderData = dummy_order_criteria_header_data,
    maybe_ranked_proc_table_header(Prefs, non_ranked, MakeHeaderData,
        NumColumns, Header),
    maybe_ranked_subject_perf_table_row(Prefs, non_ranked,
        call_site_desc_to_cell, RowData, PerfRow, 1, _),
    PerfTable = table(table_class_box, NumColumns, yes(Header), [PerfRow]),

    Display = display(yes(Title),
        [display_table(FirstTable), display_table(PerfTable)]).

%-----------------------------------------------------------------------------%
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
    ( Criteria = Criteria0 ->
        % ZZZ Should we display a simple string to indicate that this link
        % leads back to the same page, or would that be confusing?
        Link = deep_link(Cmd, yes(Prefs), Label, link_class_link),
        TableData = td_l(Link)
    ;
        Link = deep_link(Cmd, yes(Prefs), Label, link_class_link),
        TableData = td_l(Link)
    ).

:- func top_procs_order_criteria_header_data(display_limit, cost_kind,
    include_descendants, measurement_scope, preferences,
    order_criteria, string) = table_data.

top_procs_order_criteria_header_data(DisplayLimit0, CostKind0, InclDesc0,
        Scope0, Prefs0, Criteria, Label) = TableData :-
    Cmd0 = deep_cmd_top_procs(DisplayLimit0, CostKind0, InclDesc0, Scope0),
    (
        ( Criteria = by_context
        ; Criteria = by_name
        ),
        TableData =
            override_order_criteria_header_data(Cmd0, Prefs0, Criteria, Label)
    ;
        Criteria = by_cost(CostKind, InclDesc, Scope),
        Cmd = deep_cmd_top_procs(DisplayLimit0, CostKind, InclDesc, Scope),
        ( Cmd = Cmd0 ->
            % ZZZ Should we display a simple string to indicate that this link
            % leads back to the same page, or would that be confusing?
            Link = deep_link(Cmd, yes(Prefs0), Label, link_class_link),
            TableData = td_l(Link)
        ;
            Link = deep_link(Cmd, yes(Prefs0), Label, link_class_link),
            TableData = td_l(Link)
        )
    ).

:- func dummy_order_criteria_header_data(preferences, order_criteria, string)
    = table_data.

dummy_order_criteria_header_data(_, _, Label) = td_s(Label).

%-----------------------------------------------------------------------------%
%
% The predicates in this section build
%
% (a) the headers of table columns that describe each aspect of performance,
%     and
% (b) the entries in those columns.
%
% Each pair of predicates should follow the exact same logic when selecting
% what columns to display, and in what order.

    % Convert the performance information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row(fields::in, perf_row_data(Subject)::in,
    list(table_cell)::out) is det.

perf_table_row(Fields, RowData, PerfCells) :-
    perf_table_row_ports(Fields, RowData, PortCells),
    perf_table_row_time(Fields, RowData, TimeCells),
    perf_table_row_callseqs(Fields, RowData, CallSeqsCells),
    perf_table_row_allocs(Fields, RowData, AllocCells),
    perf_table_row_memory(Fields, RowData, MemoryCells),
    PerfCells =
        PortCells ++ TimeCells ++ CallSeqsCells ++
        AllocCells ++ MemoryCells.

    % Build the performance group of table headers
    % according to the preferences.
    %
:- pred perf_table_header(preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header(Prefs, MakeHeaderData, HeaderGroups) :-
    perf_table_header_ports(Prefs, MakeHeaderData, PortHeaderGroups),
    perf_table_header_time(Prefs, MakeHeaderData, TimeHeaderGroups),
    perf_table_header_callseqs(Prefs, MakeHeaderData, CallSeqsHeaderGroups),
    perf_table_header_allocs(Prefs, MakeHeaderData, AllocHeaderGroups),
    perf_table_header_memory(Prefs, MakeHeaderData, MemoryHeaderGroups),
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

        CallsCell = table_cell(td_i(Calls), 1),
        ExitsCell = table_cell(td_i(Exits), 1),
        FailsCell = table_cell(td_i(Fails), 1),
        RedosCell = table_cell(td_i(Redos), 1),
        ExcpsCell = table_cell(td_i(Excps), 1),

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
:- pred perf_table_row_time(fields::in, perf_row_data(Subject)::in,
    list(table_cell)::out) is det.

perf_table_row_time(Fields, RowData, TimeCells) :-
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

        SelfTicks =         RowData ^ perf_row_self_ticks,
        SelfTime =          RowData ^ perf_row_self_time,
        SelfTimePercent =   RowData ^ perf_row_self_time_percent,
        SelfTimePerCall =   RowData ^ perf_row_self_time_percall,
        TotalTicks =        RowData ^ perf_row_total_ticks,
        TotalTime =         RowData ^ perf_row_total_time,
        TotalTimePercent =  RowData ^ perf_row_total_time_percent,
        TotalTimePerCall =  RowData ^ perf_row_total_time_percall,

        SelfTicksCell =         table_cell(td_i(SelfTicks), 1),
        SelfTimeCell =          table_cell(td_t(SelfTime), 1),
        SelfTimePercentCell =   table_cell(td_p(SelfTimePercent), 1),
        SelfTimePerCallCell =   table_cell(td_t(SelfTimePerCall), 1),
        TotalTicksCell =        table_cell(td_i(TotalTicks), 1),
        TotalTimeCell =         table_cell(td_t(TotalTime), 1),
        TotalTimePercentCell =  table_cell(td_p(TotalTimePercent), 1),
        TotalTimePerCallCell =  table_cell(td_t(TotalTimePerCall), 1),

        (
            TimeFields = ticks,
            TimeCells =
                [SelfTicksCell, SelfTimePercentCell,
                TotalTicksCell, TotalTimePercentCell]
        ;
            TimeFields = time,
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
    ).

    % Build the time group of table headers according to the preferences.
    %
:- pred perf_table_header_time(preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header_time(Prefs, MakeHeaderData, HeaderGroups) :-
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
        ),

        HeaderGroup = make_multi_table_header_group(Title, SubHeaders,
            table_column_class_ticks_and_times, column_colour_if_pref),
        HeaderGroups = [HeaderGroup]
    ).

    % Convert the callseqs information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row_callseqs(fields::in, perf_row_data(Subject)::in,
    list(table_cell)::out) is det.

perf_table_row_callseqs(Fields, RowData, CallSeqsCells) :-
    CallSeqsFields = Fields ^ callseqs_fields,
    (
        CallSeqsFields = no_callseqs,
        CallSeqsCells = []
    ;
        SelfCallSeqs =              RowData ^ perf_row_self_callseqs,
        SelfCallSeqsPercent =       RowData ^ perf_row_self_callseqs_percent,
        SelfCallSeqsPerCall =       RowData ^ perf_row_self_callseqs_percall,
        TotalCallSeqs =             RowData ^ perf_row_total_callseqs,
        TotalCallSeqsPercent =      RowData ^ perf_row_total_callseqs_percent,
        TotalCallSeqsPerCall =      RowData ^ perf_row_total_callseqs_percall,

        SelfCallSeqsCell =          table_cell(td_i(SelfCallSeqs), 1),
        SelfCallSeqsPercentCell =   table_cell(td_p(SelfCallSeqsPercent), 1),
        SelfCallSeqsPerCallCell =   table_cell(td_f(SelfCallSeqsPerCall), 1),
        TotalCallSeqsCell =         table_cell(td_i(TotalCallSeqs), 1),
        TotalCallSeqsPercentCell =  table_cell(td_p(TotalCallSeqsPercent), 1),
        TotalCallSeqsPerCallCell =  table_cell(td_f(TotalCallSeqsPerCall), 1),

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
    ).

    % Build the callseqs group of table headers according to the preferences.
    %
:- pred perf_table_header_callseqs(preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header_callseqs(Prefs, MakeHeaderData, HeaderGroups) :-
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
            CallSeqsFields = callseqs,
            SubHeaders =
                [SelfData, SelfPercentData, TotalData, TotalPercentData]
        ;
            CallSeqsFields = callseqs_and_percall,
            SubHeaders =
                [SelfData, SelfPercentData, SelfPerCallData,
                TotalData, TotalPercentData, TotalPerCallData]
        ),
        Title = "Call sequence numbers",
        HeaderGroup = make_multi_table_header_group(Title, SubHeaders,
            table_column_class_callseqs, column_colour_if_pref),
        HeaderGroups = [HeaderGroup]
    ).

    % Convert the allocation information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row_allocs(fields::in, perf_row_data(Subject)::in,
    list(table_cell)::out) is det.

perf_table_row_allocs(Fields, RowData, AllocCells) :-
    AllocFields = Fields ^ alloc_fields,
    (
        AllocFields = no_alloc,
        AllocCells = []
    ;
        ( AllocFields = alloc
        ; AllocFields = alloc_and_percall
        ),

        SelfAllocs =                RowData ^ perf_row_self_allocs,
        SelfAllocsPercent =         RowData ^ perf_row_self_allocs_percent,
        SelfAllocsPerCall =         RowData ^ perf_row_self_allocs_percall,
        TotalAllocs =               RowData ^ perf_row_total_allocs,
        TotalAllocsPercent =        RowData ^ perf_row_total_allocs_percent,
        TotalAllocsPerCall =        RowData ^ perf_row_total_allocs_percall,

        SelfAllocsCell =            table_cell(td_i(SelfAllocs), 1),
        SelfAllocsPercentCell =     table_cell(td_p(SelfAllocsPercent), 1),
        SelfAllocsPerCallCell =     table_cell(td_f(SelfAllocsPerCall), 1),
        TotalAllocsCell =           table_cell(td_i(TotalAllocs), 1),
        TotalAllocsPercentCell =    table_cell(td_p(TotalAllocsPercent), 1),
        TotalAllocsPerCallCell =    table_cell(td_f(TotalAllocsPerCall), 1),

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
    ).

    % Build the allocs group of table headers according to the preferences.
    %
:- pred perf_table_header_allocs(preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header_allocs(Prefs, MakeHeaderData, HeaderGroups) :-
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
            SubHeaders =
                [SelfData, SelfPercentData, TotalData, TotalPercentData]
        ;
            AllocFields = alloc_and_percall,
            SubHeaders =
                [SelfData, SelfPercentData, SelfPerCallData,
                TotalData, TotalPercentData, TotalPerCallData]
        ),

        Title = "Memory allocations",
        HeaderGroup = make_multi_table_header_group(Title, SubHeaders,
            table_column_class_allocations, column_colour_if_pref),
        HeaderGroups = [HeaderGroup]
    ).

    % Convert the memory information in a row data into the cells
    % of a table row according to the preferences.
    %
:- pred perf_table_row_memory(fields::in, perf_row_data(Subject)::in,
    list(table_cell)::out) is det.

perf_table_row_memory(Fields, RowData, MemoryCells) :-
    MemoryFields = Fields ^ memory_fields,
    (
        MemoryFields = no_memory,
        MemoryCells = []
    ;
        ( MemoryFields = memory(Units)
        ; MemoryFields = memory_and_percall(Units)
        ),

        SelfMem =               RowData ^ perf_row_self_mem,
        SelfMemPerCall =        RowData ^ perf_row_self_mem_percall,
        SelfMemPercent =        RowData ^ perf_row_self_mem_percent,
        TotalMem =              RowData ^ perf_row_total_mem,
        TotalMemPerCall =       RowData ^ perf_row_total_mem_percall,
        TotalMemPercent =       RowData ^ perf_row_total_mem_percent,

        SelfMemCell =           table_cell(td_m(SelfMem, Units, 0), 1),
        SelfMemPerCallCell =    table_cell(td_m(SelfMemPerCall, Units, 2), 1),
        SelfMemPercentCell =    table_cell(td_p(SelfMemPercent), 1),
        TotalMemCell =          table_cell(td_m(TotalMem, Units, 0), 1),
        TotalMemPerCallCell =   table_cell(td_m(TotalMemPerCall, Units, 2), 1),
        TotalMemPercentCell =   table_cell(td_p(TotalMemPercent), 1),

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
    ).

    % Build the memory group of table headers according to the preferences.
    %
:- pred perf_table_header_memory(preferences::in,
    (func(preferences, order_criteria, string) = table_data)::in,
    list(table_header_group)::out) is det.

perf_table_header_memory(Prefs, MakeHeaderData, HeaderGroups) :-
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
            MemoryFields = memory(Units),
            SubHeaders =
                [SelfData, SelfPercentData, TotalData, TotalPercentData]
        ;
            MemoryFields = memory_and_percall(Units),
            SubHeaders =
                [SelfData, SelfPercentData, SelfPerCallData,
                TotalData, TotalPercentData, TotalPerCallData]
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

%-----------------------------------------------------------------------------%

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

    % ZZZ SubjectHeaderFunc
    ProcHeaderGroup =
        make_single_table_header_group(td_s("Procedure"),
            table_column_class_proc, column_do_not_colour),
    ProcHeaderGroups = [ProcHeaderGroup],

    perf_table_header(Prefs, MakeHeaderData, PerfHeaderGroups),

    AllHeaderGroups =
        RankedHeaderGroups ++ ProcHeaderGroups ++ PerfHeaderGroups,
    header_groups_to_header(AllHeaderGroups, NumColumns, Header).

    % Convert row data of procedures from the deep profiler into a table row
    % according to the preferences.
    %
:- pred maybe_ranked_subject_perf_table_row(preferences::in, ranked::in,
    (func(preferences, Subject) = table_cell)::in,
    perf_row_data(Subject)::in, table_row::out, int::in, int::out) is det.

maybe_ranked_subject_perf_table_row(Prefs, Ranked, SubjectCellFunc,
        RowData, Row, Rank, Rank + 1) :-
    (
        Ranked = non_ranked,
        RankCells = []
    ;
        Ranked = ranked,
        RankCells = [table_cell(td_i(Rank), 1)]
    ),

    % The name of the procedure,
    SubjectCells = [SubjectCellFunc(Prefs, RowData ^ perf_row_subject)],

    Fields = Prefs ^ pref_fields,
    perf_table_row(Fields, RowData, PerfCells),

    Cells = RankCells ++ SubjectCells ++ PerfCells,
    Row = table_row(Cells).

%-----------------------------------------------------------------------------%
%
% The basic predicates for creating the controls at the bottoms of pages.
%

:- pred make_top_procs_cmds(preferences::in, display_limit::in,
    cost_kind::in, include_descendants::in, measurement_scope::in,
    maybe(string)::in,
    assoc_list(string,
        (pred(display_limit, cost_kind, include_descendants,
            measurement_scope, cmd)))::
        in(list_skel(pair(ground, (pred(in, in, in, in, out) is det)))),
    display_item::out) is det.

make_top_procs_cmds(Prefs, DisplayLimit, CostKind, InclDesc, Scope, MaybeLabel,
        LabelsCmdMakers, Item) :-
    list.map(
        make_top_procs_cmd(Prefs, DisplayLimit, CostKind, InclDesc, Scope),
        LabelsCmdMakers, ControlItemLists),
    list.condense(ControlItemLists, ControlItems),
    Item = display_list(list_class_horizontal, MaybeLabel, ControlItems).

:- pred make_top_procs_cmd(preferences::in, display_limit::in,
    cost_kind::in, include_descendants::in, measurement_scope::in,
    pair(string,
        (pred(display_limit, cost_kind, include_descendants, measurement_scope,
            cmd)))::
        in(pair(ground, (pred(in, in, in, in, out) is det))),
    list(display_item)::out) is det.

make_top_procs_cmd(Prefs, DisplayLimit, CostKind, InclDesc, Scope,
        Label - CmdMaker, Items) :-
    Cmd0 = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope),
    CmdMaker(DisplayLimit, CostKind, InclDesc, Scope, Cmd),
    ( Cmd = Cmd0 ->
        Items = []
        % We could use this code instead.
        % CurLabel = Label ++ " (current setting)",
        % PseudoLink = pseudo_link(CurLabel, link_class_control),
        % Item = display_pseudo_link(PseudoLink),
        % Items = [Item]
    ;
        Link = deep_link(Cmd, yes(Prefs), Label, link_class_control),
        Item = display_link(Link),
        Items = [Item]
    ).

:- pred make_prefs_controls(preferences::in, cmd::in, maybe(string)::in,
    assoc_list(string, (pred(preferences, preferences)))::
        in(list_skel(pair(ground, (pred(in, out) is det)))),
    display_item::out) is det.

make_prefs_controls(Prefs0, Cmd, MaybeLabel, LabelsPrefMakers, Item) :-
    list.map(make_prefs_control(Prefs0, Cmd), LabelsPrefMakers,
        ControlItemLists),
    list.condense(ControlItemLists, ControlItems),
    Item = display_list(list_class_horizontal, MaybeLabel, ControlItems).

:- pred make_prefs_control(preferences::in, cmd::in,
    pair(string, (pred(preferences, preferences)))::
        in(pair(ground, (pred(in, out) is det))),
    list(display_item)::out) is det.

make_prefs_control(Prefs0, Cmd, Label - PrefMaker, Items) :-
    PrefMaker(Prefs0, Prefs),
    ( Prefs = Prefs0 ->
        Items = []
        % We could use this code instead.
        % CurLabel = Label ++ " (current setting)",
        % PseudoLink = pseudo_link(CurLabel, link_class_control),
        % Item = display_pseudo_link(PseudoLink),
        % Items = [Item]
    ;
        Link = deep_link(Cmd, yes(Prefs), Label, link_class_control),
        Item = display_link(Link),
        Items = [Item]
    ).

%-----------------------------------------------------------------------------%
%
% Control how the top_procs command selects what is "top".
%

:- func top_procs_controls(preferences, display_limit, cost_kind,
    include_descendants, measurement_scope) = display_item.

top_procs_controls(Prefs, DisplayLimit, CostKind, InclDesc, Scope) =
        ControlsItem :-
    make_top_procs_cmds(Prefs, DisplayLimit, CostKind, InclDesc, Scope, no,
        top_procs_limit_toggles, LimitControls),
    make_top_procs_cmds(Prefs, DisplayLimit, CostKind, InclDesc, Scope, no,
        top_procs_sort_toggles, SortControls),
    make_top_procs_cmds(Prefs, DisplayLimit, CostKind, InclDesc, Scope, no,
        top_procs_incl_desc_toggles, InclDescControls),
    make_top_procs_cmds(Prefs, DisplayLimit, CostKind, InclDesc, Scope, no,
        top_procs_scope_toggles, ScopeControls),
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

%-----------------------------------------------------------------------------%
%
% Control how the rows in procedure displays are sorted.
%

:- func sort_controls(preferences, cmd) = display_item.

sort_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls(Prefs, Cmd, no,
        sort_main_toggles, SortMainControls),
    make_prefs_controls(Prefs, Cmd, no,
        sort_time_toggles, SortTimeControls),
    make_prefs_controls(Prefs, Cmd, no,
        sort_callseqs_toggles, SortCallSeqsControls),
    make_prefs_controls(Prefs, Cmd, no,
        sort_allocs_toggles, SortAllocsControls),
    make_prefs_controls(Prefs, Cmd, no,
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

%-----------------------------------------------------------------------------%
%
% Control whether we summarize the info from callees at multi call sites.
%

:- func summarize_controls(preferences, cmd) = display_item.

summarize_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls(Prefs, Cmd, no,
        summarize_toggles, SummarizeControls),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("Toggle summarize options:"), [SummarizeControls]).

:- func summarize_toggles =
    (assoc_list(string, (pred(preferences, preferences)))::
    out(list_skel(pair(ground, (pred(in, out) is det))))) is det.

summarize_toggles = [
    "Do not summarize higher order calls" -
        set_summarize(do_not_summarize),
    "Summarize higher order calls" -
        set_summarize(summarize)
].

:- pred set_summarize(summarize::in,
    preferences::in, preferences::out) is det.

set_summarize(Summarize, !Prefs) :-
    !Prefs ^ pref_summarize := Summarize.

%-----------------------------------------------------------------------------%
%
% Control how pages are displayed.
%

:- func format_controls(preferences, cmd) = display_item.

format_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls(Prefs, Cmd, no,
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

%-----------------------------------------------------------------------------%
%
% Control the set of displayed fields.
%

:- func field_controls(preferences, cmd) = display_item.

field_controls(Prefs, Cmd) = ControlsItem :-
    make_prefs_controls(Prefs, Cmd, no,
        port_field_toggles, PortControls),
    make_prefs_controls(Prefs, Cmd, no,
        time_field_toggles, TimeControls),
    make_prefs_controls(Prefs, Cmd, no,
        callseqs_field_toggles, CallSeqsControls),
    make_prefs_controls(Prefs, Cmd, no,
        alloc_field_toggles, AllocControls),
    make_prefs_controls(Prefs, Cmd, no,
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

%-----------------------------------------------------------------------------%
%
% Links to the basic commands.
%

    % Give the common list of commands seen at the bottom of all deep-profiler
    % displayed reports.
    %
:- func cmds_menu_restart_quit(maybe(preferences)) = display_item.

cmds_menu_restart_quit(MaybePrefs) = ControlsItem :-
    Menu = display_link(deep_link(deep_cmd_menu, MaybePrefs, "Menu",
        link_class_control)),
    Restart = display_link(deep_link(deep_cmd_restart, MaybePrefs, "Restart",
        link_class_control)),
    Quit = display_link(deep_link(deep_cmd_quit, MaybePrefs, "Quit",
        link_class_control)),
    List = display_list(list_class_horizontal, no,
        [Menu, Restart, Quit]),
    ControlsItem = display_list(list_class_vertical_no_bullets,
        yes("General commands:"), [List]).

%-----------------------------------------------------------------------------%
%
% Convert procedure and call site descriptions into table cells.
%

:- func proc_desc_to_cell(preferences, proc_desc) = table_cell.

proc_desc_to_cell(Prefs, ProcDesc) = table_cell(Data, 1) :-
    ProcDesc = proc_desc(PSPtr, _FileName, _LineNumber, RefinedName),
    PSPtr = proc_static_ptr(PSI),
    Cmd = deep_cmd_proc(PSI),
    Data = td_l(deep_link(Cmd, yes(Prefs), RefinedName, link_class_link)).

:- func call_site_desc_to_cell(preferences, call_site_desc) = table_cell.

call_site_desc_to_cell(Prefs, CallSiteDesc) = table_cell(Data, 1) :-
    CallSiteDesc = call_site_desc(CSSPtr, _ContainerPSPtr,
        _FileName, _LineNumber, RefinedName, SlotNumber, GoalPath),
    string.format("%s @ %s #%d", [s(RefinedName), s(GoalPath), i(SlotNumber)],
        Name),
    CSSPtr = call_site_static_ptr(CSSI),
    Cmd = deep_cmd_call_site_static(CSSI),
    Data = td_l(deep_link(Cmd, yes(Prefs), Name, link_class_link)).

%-----------------------------------------------------------------------------%
%
% Utility predicates.
%

    % Make a table row with two columns: a label and a value.
    %
:- func make_labelled_table_row(pair(string, table_data)) = table_row.

make_labelled_table_row(Label - Value) =
    table_row([table_cell(td_s(Label), 1), table_cell(Value, 1)]).

    % Make a link for use in the menu report.
    %
:- pred make_link(pair(cmd, string)::in, display_item::out) is det.

make_link(Cmd - Label, Item) :-
    Item = display_link(deep_link(Cmd, no, Label, link_class_link)).

%-----------------------------------------------------------------------------%
%
% Sort call_site_perfs by the preferred criteria of performance.
%

:- pred sort_call_sites_by_preferences(preferences::in,
    list(call_site_perf)::in, list(call_site_perf)::out) is det.

sort_call_sites_by_preferences(Prefs, !CallSitePerfs) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        OrderCriteria = by_context,
        list.sort(compare_call_site_perfs_by_context, !CallSitePerfs)
    ;
        OrderCriteria = by_name,
        list.sort(compare_call_site_perfs_by_name, !CallSitePerfs)
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
    FileNameA = CallSiteDescA ^ call_site_desc_file_name,
    FileNameB = CallSiteDescB ^ call_site_desc_file_name,
    compare(FileNameResult, FileNameA, FileNameB),
    (
        ( FileNameResult = (<)
        ; FileNameResult = (>)
        ),
        Result = FileNameResult
    ;
        FileNameResult = (=),
        LineNumberA = CallSiteDescA ^ call_site_desc_line_number,
        LineNumberB = CallSiteDescB ^ call_site_desc_line_number,
        compare(Result, LineNumberA, LineNumberB)
    ).

:- pred compare_call_site_perfs_by_name(
    call_site_perf::in, call_site_perf::in, comparison_result::out) is det.

compare_call_site_perfs_by_name(CallSitePerfA, CallSitePerfB, Result) :-
    CallSiteDescA = CallSitePerfA ^ csf_summary_perf ^ perf_row_subject,
    CallSiteDescB = CallSitePerfB ^ csf_summary_perf ^ perf_row_subject,
    NameA = CallSiteDescA ^ call_site_desc_refined_name,
    NameB = CallSiteDescB ^ call_site_desc_refined_name,
    compare(Result, NameA, NameB).

:- pred compare_call_site_perfs_by_cost(
    cost_kind::in, include_descendants::in, measurement_scope::in,
    call_site_perf::in, call_site_perf::in, comparison_result::out) is det.

compare_call_site_perfs_by_cost(CostKind, InclDesc, Scope,
        CallSitePerfA, CallSitePerfB, Result) :-
    PerfA = CallSitePerfA ^ csf_summary_perf,
    PerfB = CallSitePerfB ^ csf_summary_perf,
    compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope, PerfA, PerfB,
        Result).

%-----------------------------------------------------------------------------%
%
% Sort the procs at a multi call site by the preferred criteria of performance.
%

:- pred sort_proc_desc_rows_by_preferences(preferences::in,
    list(perf_row_data(proc_desc))::in, list(perf_row_data(proc_desc))::out)
    is det.

sort_proc_desc_rows_by_preferences(Prefs, !CalleePerfs) :-
    OrderCriteria = Prefs ^ pref_criteria,
    (
        OrderCriteria = by_context,
        list.sort(compare_proc_desc_rows_by_context, !CalleePerfs)
    ;
        OrderCriteria = by_name,
        list.sort(compare_proc_desc_rows_by_name, !CalleePerfs)
    ;
        OrderCriteria = by_cost(CostKind, InclDesc, Scope),
        list.sort(compare_perf_row_datas_by_cost(CostKind, InclDesc, Scope),
            !CalleePerfs),
        % We want the most expensive rows to appear first.
        list.reverse(!CalleePerfs)
    ).

:- pred compare_proc_desc_rows_by_context(
    perf_row_data(proc_desc)::in, perf_row_data(proc_desc)::in,
    comparison_result::out) is det.

compare_proc_desc_rows_by_context(ProcDescRowDataA, ProcDescRowDataB,
            Result) :-
    ProcDescA = ProcDescRowDataA ^ perf_row_subject,
    ProcDescB = ProcDescRowDataB ^ perf_row_subject,
    FileNameA = ProcDescA ^ proc_desc_file_name,
    FileNameB = ProcDescB ^ proc_desc_file_name,
    compare(FileNameResult, FileNameA, FileNameB),
    (
        ( FileNameResult = (<)
        ; FileNameResult = (>)
        ),
        Result = FileNameResult
    ;
        FileNameResult = (=),
        LineNumberA = ProcDescA ^ proc_desc_line_number,
        LineNumberB = ProcDescB ^ proc_desc_line_number,
        compare(Result, LineNumberA, LineNumberB)
    ).

:- pred compare_proc_desc_rows_by_name(
    perf_row_data(proc_desc)::in, perf_row_data(proc_desc)::in,
    comparison_result::out) is det.

compare_proc_desc_rows_by_name(ProcDescRowDataA, ProcDescRowDataB, Result) :-
    ProcDescA = ProcDescRowDataA ^ perf_row_subject,
    ProcDescB = ProcDescRowDataB ^ perf_row_subject,
    NameA = ProcDescA ^ proc_desc_refined_name,
    NameB = ProcDescB ^ proc_desc_refined_name,
    compare(Result, NameA, NameB).

%-----------------------------------------------------------------------------%
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
        (
            InclDesc = self,
            Scope = overall,
            TimeA = PerfA ^ perf_row_self_time,
            TimeB = PerfB ^ perf_row_self_time,
            compare(Result, TimeA, TimeB)
        ;
            InclDesc = self,
            Scope = per_call,
            TimeA = PerfA ^ perf_row_self_time_percall,
            TimeB = PerfB ^ perf_row_self_time_percall,
            compare(Result, TimeA, TimeB)
        ;
            InclDesc = self_and_desc,
            Scope = overall,
            TimeA = PerfA ^ perf_row_total_time,
            TimeB = PerfB ^ perf_row_total_time,
            compare(Result, TimeA, TimeB)
        ;
            InclDesc = self_and_desc,
            Scope = per_call,
            TimeA = PerfA ^ perf_row_total_time_percall,
            TimeB = PerfB ^ perf_row_total_time_percall,
            compare(Result, TimeA, TimeB)
        )
    ;
        CostKind = cost_callseqs,
        (
            InclDesc = self,
            Scope = overall,
            CallSeqsA = PerfA ^ perf_row_self_callseqs,
            CallSeqsB = PerfB ^ perf_row_self_callseqs,
            compare(Result, CallSeqsA, CallSeqsB)
        ;
            InclDesc = self,
            Scope = per_call,
            CallSeqsA = PerfA ^ perf_row_self_callseqs_percall,
            CallSeqsB = PerfB ^ perf_row_self_callseqs_percall,
            compare(Result, CallSeqsA, CallSeqsB)
        ;
            InclDesc = self_and_desc,
            Scope = overall,
            CallSeqsA = PerfA ^ perf_row_total_callseqs,
            CallSeqsB = PerfB ^ perf_row_total_callseqs,
            compare(Result, CallSeqsA, CallSeqsB)
        ;
            InclDesc = self_and_desc,
            Scope = per_call,
            CallSeqsA = PerfA ^ perf_row_total_callseqs_percall,
            CallSeqsB = PerfB ^ perf_row_total_callseqs_percall,
            compare(Result, CallSeqsA, CallSeqsB)
        )
    ;
        CostKind = cost_allocs,
        (
            InclDesc = self,
            Scope = overall,
            AllocsA = PerfA ^ perf_row_self_allocs,
            AllocsB = PerfB ^ perf_row_self_allocs,
            compare(Result, AllocsA, AllocsB)
        ;
            InclDesc = self,
            Scope = per_call,
            AllocsA = PerfA ^ perf_row_self_allocs_percall,
            AllocsB = PerfB ^ perf_row_self_allocs_percall,
            compare(Result, AllocsA, AllocsB)
        ;
            InclDesc = self_and_desc,
            Scope = overall,
            AllocsA = PerfA ^ perf_row_total_allocs,
            AllocsB = PerfB ^ perf_row_total_allocs,
            compare(Result, AllocsA, AllocsB)
        ;
            InclDesc = self_and_desc,
            Scope = per_call,
            AllocsA = PerfA ^ perf_row_total_allocs_percall,
            AllocsB = PerfB ^ perf_row_total_allocs_percall,
            compare(Result, AllocsA, AllocsB)
        )
    ;
        CostKind = cost_words,
        (
            InclDesc = self,
            Scope = overall,
            MemoryA = PerfA ^ perf_row_self_mem,
            MemoryB = PerfB ^ perf_row_self_mem,
            compare_memory(MemoryA, MemoryB, Result)
        ;
            InclDesc = self,
            Scope = per_call,
            MemoryA = PerfA ^ perf_row_self_mem_percall,
            MemoryB = PerfB ^ perf_row_self_mem_percall,
            compare_memory(MemoryA, MemoryB, Result)
        ;
            InclDesc = self_and_desc,
            Scope = overall,
            MemoryA = PerfA ^ perf_row_total_mem,
            MemoryB = PerfB ^ perf_row_total_mem,
            compare_memory(MemoryA, MemoryB, Result)
        ;
            InclDesc = self_and_desc,
            Scope = per_call,
            MemoryA = PerfA ^ perf_row_total_mem_percall,
            MemoryB = PerfB ^ perf_row_total_mem_percall,
            compare_memory(MemoryA, MemoryB, Result)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module display_report.
%-----------------------------------------------------------------------------%
