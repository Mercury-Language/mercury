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
:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

report_to_display(Deep, Prefs, Report) = Display :-
    (
        Report = report_message(message_info(Msg)),
        Display = display(no, [display_message(Msg)])
    ;
        Report = report_menu(MaybeMenuInfo),
        (
            MaybeMenuInfo = ok(MenuInfo),
            display_report_menu(Deep, MenuInfo, Display)
        ;
            MaybeMenuInfo = error(Msg),
            Display = display(no, [display_message(Msg)])
        )
    ;
        Report = report_top_procs(MaybeTopProcsInfo),
        (
            MaybeTopProcsInfo = ok(TopProcsInfo),
            display_report_top_procs(Prefs, TopProcsInfo, Display)
        ;
            MaybeTopProcsInfo = error(Msg),
            Display = display(no, [display_message(Msg)])
        )
    ;
        Report = report_proc_static_dump(MaybeProcStaticDumpInfo),
        (
            MaybeProcStaticDumpInfo = ok(ProcStaticDumpInfo),
            display_report_proc_static_dump(ProcStaticDumpInfo, Display)
        ;
            MaybeProcStaticDumpInfo = error(Msg),
            Display = display(no, [display_message(Msg)])
        )
    ;
        Report = report_proc_dynamic_dump(MaybeProcDynamicDumpInfo),
        (
            MaybeProcDynamicDumpInfo = ok(ProcDynamicDumpInfo),
            display_report_proc_dynamic_dump(Deep, Prefs, ProcDynamicDumpInfo,
                Display)
        ;
            MaybeProcDynamicDumpInfo = error(Msg),
            Display = display(no, [display_message(Msg)])
        )
    ;
        Report = report_call_site_static_dump(MaybeCallSiteStaticDumpInfo),
        (
            MaybeCallSiteStaticDumpInfo = ok(CallSiteStaticDumpInfo),
            display_report_call_site_static_dump(Prefs, CallSiteStaticDumpInfo,
                Display)
        ;
            MaybeCallSiteStaticDumpInfo = error(Msg),
            Display = display(no, [display_message(Msg)])
        )
    ;
        Report = report_call_site_dynamic_dump(MaybeCallSiteDynamicDumpInfo),
        (
            MaybeCallSiteDynamicDumpInfo = ok(CallSiteDynamicDumpInfo),
            display_report_call_site_dynamic_dump(Prefs,
                CallSiteDynamicDumpInfo, Display)
        ;
            MaybeCallSiteDynamicDumpInfo = error(Msg),
            Display = display(no, [display_message(Msg)])
        )
    ).

%-----------------------------------------------------------------------------%
%
% Code to display menu report.
%

:- pred display_report_menu(deep::in, menu_info::in, display::out) is det.

display_report_menu(Deep, MenuInfo, Display) :-
    MenuInfo = menu_info(QuantaPerSec, UserQuanta, InstQuanta,
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
    list.map(make_command_link, LinkCmds, LinksList),
    Links = display_list(list_class_vertical_bullets,
        yes("You can start exploring the deep profile at the following" ++
            " points."), LinksList),

    % Display the table section of the report.
    ProfilingStatistics =
        [("Quanta per second:"          - td_i(QuantaPerSec)),
        ("Quanta in user code:"         - td_i(UserQuanta)),
        ("Quanta in instrumentation:"   - td_i(InstQuanta)),
        ("Call sequence numbers:"       - td_i(NumCallseqs)),
        ("CallSiteDyanic structures:"   - td_i(NumCSD)),
        ("ProcDynamic structures:"      - td_i(NumPD)),
        ("CallSiteStatic structures:"   - td_i(NumCSS)),
        ("ProcStatic structures:"       - td_i(NumPS)),
        ("Cliques:"                     - td_i(NumClique))],

    Rows = list.map(make_labelled_table_row, ProfilingStatistics),
    Table = table(table_class_plain, 2, no, Rows),

    % Display the Controls section of the report.
    Controls = display_list(list_class_horizontal, no, cmds_menu_restart_quit),

    % Construct the complete representation of what to display.
    Display = display(yes("Deep profiler menu"),
        [Links, display_table(Table), Controls]).

%-----------------------------------------------------------------------------%
%
% Code to display a top procedures report.
%

    % Create a display_report structure for a top_procedures report.
    %
:- pred display_report_top_procs(preferences::in, top_procs_info::in,
    display::out) is det.

display_report_top_procs(Prefs, TopProcsInfo, Display) :-
    TopProcsInfo = top_procs_info(Ordering, TopProcs),
    Ordering = report_ordering(DisplayLimit, CostKind, InclDesc, Scope),
    Desc = cost_criteria_to_description(CostKind, InclDesc, Scope),
    Title = "Top procedures " ++ Desc,

    % Build table.
    top_procs_table(Prefs, Ordering, TopProcs, Table),
    DisplayTable = display_table(Table),

    % Build controls at bottom of page.
    Cmd = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope),
    sort_controls(Prefs, Ordering, SortControls),
    incldesc_and_scope_controls(Prefs, Ordering, InclDescScope),
    Controls1 = display_list(list_class_vertical_no_bullets, no,
        [SortControls, InclDescScope]),

    field_controls(Prefs, Cmd, Controls2),

    display_controls(Prefs, Cmd, Controls3),

    Controls4 =
        display_list(list_class_horizontal, no, cmds_menu_restart_quit),

    Display = display(yes(Title),
        [DisplayTable, Controls1, Controls2, Controls3, Controls4]).

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
    Table = table(table_class_plain, 2, no, Rows),
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
    MainTable = table(table_class_plain, 2, no, MainRows),

    list.map_foldl(dump_psd_call_site(Prefs), CallSites, CallSitesRowsList,
        counter.init(1), _),
    list.condense(CallSitesRowsList, CallSitesRows),
    CallSitesTitle = "Call site dynamics:",
    CallSitesTable =
        table(table_class_plain, 2, no, CallSitesRows),

    Display = display(yes(Title),
        [display_table(MainTable), display_message(CallSitesTitle),
        display_table(CallSitesTable)]).

:- pred dump_psd_call_site(preferences::in,
    call_site_array_slot::in, list(table_row)::out,
    counter::in, counter::out) is det.

dump_psd_call_site(Prefs, CallSite, Rows, !CallSiteCounter) :-
    counter.allocate(CallSiteNum, !CallSiteCounter),
    CallSiteNumCell = table_cell(td_i(CallSiteNum)),
    (
        CallSite = slot_normal(CSDPtr),
        CSDPtr = call_site_dynamic_ptr(CSDI),
        CSDLink = deep_link(deep_cmd_call_site_dynamic(CSDI), yes(Prefs),
            string.int_to_string(CSDI), link_class_link),
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
    CSDLink = deep_link(deep_cmd_call_site_dynamic(CSDI), yes(Prefs),
        string.int_to_string(CSDI), link_class_link),
    CSDCell = table_cell(td_l(CSDLink)),
    EmptyCell = table_cell(td_s("")),
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
    Table = table(table_class_plain, 2, no, Rows),
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
    FirstTable = table(table_class_plain, 2, no, FirstRows),

    % The value of Ordering here shouldn't matter.
    Ordering = report_ordering(rank_range(1, 100), cost_time, self, overall),
    TableInfo = table_info(table_class_boxed, non_ranked, Prefs, Ordering),

    proc_table_header(TableInfo, NumCols, Header),
    perf_table_row(TableInfo, call_site_desc_to_cell,
        RowData, PerfRow, 1, _),
    PerfTable =
        table(TableInfo ^ table_class, NumCols, yes(Header), [PerfRow]),

    Display = display(yes(Title),
        [display_table(FirstTable), display_table(PerfTable)]).

%-----------------------------------------------------------------------------%

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

    % TODO: Generalize this type so it can be used for most tables shown by the
    % deep profiler.
:- type table_info
    --->    table_info(
                table_class     :: table_class,
                table_ranked    :: ranked,
                prefs           :: preferences,
                table_ordering  :: report_ordering
            ).

:- pred top_procs_table(preferences::in, report_ordering::in,
    list(perf_row_data(proc_desc))::in, table::out) is det.

top_procs_table(Prefs, Ordering, TopProcs, Table) :-
    TableInfo = table_info(table_class_boxed, ranked, Prefs, Ordering),
    proc_table(TableInfo, TopProcs, Table).

%-----------------------------------------------------------------------------%
%
% Code for creating procedure tables.
%
% TODO: The code in this section should be generalised as new reports are added
% which may have similar tables.

    % Describes whether a table should be ranked or not,  This means that each
    % item has an ordinal number associated with it in an initial column
    % labeled "Rank".
    %
:- type ranked
    --->    ranked
    ;       non_ranked.

    % Produce a table for all these procedures.
    %
:- pred proc_table(table_info::in, list(perf_row_data(proc_desc))::in,
    table::out) is det.

proc_table(TableInfo, TopProcs, Table) :-
    % Later add support for non-ranked tables.
    proc_table_header(TableInfo, NumCols, Header),
    list.map_foldl(perf_table_row(TableInfo, proc_desc_to_cell),
        TopProcs, Rows, 1, _),
    Table = table(TableInfo ^ table_class, NumCols, yes(Header), Rows).

%-----------------------------------------------------------------------------%
%
% Common column header strings.
%

:- func percall = table_data.
percall = td_s("/call").

:- func percent_label = table_data.
percent_label = td_s("%").

:- func self = table_data.
self = td_s("Self").

:- func time = table_data.
time = td_s("Time").

:- func total = table_data.
total = td_s("Total").

%-----------------------------------------------------------------------------%

:- func top_procs_self_link(table_info, cost_kind) = table_data.

top_procs_self_link(TableInfo, CostKind) =
    top_procs_make_table_link(TableInfo, "Self",
        CostKind, self, overall).

:- func top_procs_self_percall_link(table_info, cost_kind) = table_data.

top_procs_self_percall_link(TableInfo, CostKind) =
    top_procs_make_table_link(TableInfo, "/call",
        CostKind, self, per_call).

:- func top_procs_total_link(table_info, cost_kind) = table_data.

top_procs_total_link(TableInfo, CostKind) =
    top_procs_make_table_link(TableInfo, "Total",
        CostKind, self_and_desc, overall).

:- func top_procs_total_percall_link(table_info, cost_kind) = table_data.

top_procs_total_percall_link(TableInfo, CostKind) =
    top_procs_make_table_link(TableInfo, "/call",
        CostKind, self_and_desc, per_call).

:- func top_procs_time_link(table_info) = table_data.

top_procs_time_link(TableInfo) =
    top_procs_make_table_link(TableInfo, "Time",
        cost_time, self, overall).

:- func top_procs_total_time_link(table_info) = table_data.

top_procs_total_time_link(TableInfo) =
    top_procs_make_table_link(TableInfo, "Time",
        cost_time, self_and_desc, overall).

%-----------------------------------------------------------------------------%

:- func top_procs_make_table_link(table_info, string, cost_kind,
    include_descendants, measurement_scope) = table_data.

top_procs_make_table_link(TableInfo, Label, CostKind, InclDesc, Scope)
        = TableData :-
    Ordering = TableInfo ^ table_ordering,
    Prefs = TableInfo ^ prefs,
    DisplayLimit = Ordering ^ display_limit,
    Cmd = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope),
    Link = deep_link(Cmd, yes(Prefs), Label, link_class_link),
    TableData = td_l(Link).

:- func top_procs_make_link(report_ordering, preferences, string, cost_kind,
    include_descendants, measurement_scope, link_class) = deep_link.

top_procs_make_link(Ordering, Prefs, Label, CostKind, InclDesc, Scope, Class)
        = Link :-
    DisplayLimit = Ordering ^ display_limit,
    Cmd = deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, Scope),
    Link = deep_link(Cmd, yes(Prefs), Label, Class).

%-----------------------------------------------------------------------------%

    % Convert row data of procedures from the deep profiler into a table row
    % according to the preferences.
    %
:- pred perf_table_row(table_info::in,
    (func(table_info, Subject) = table_cell)::in,
    perf_row_data(Subject)::in, table_row::out,
    int::in, int::out) is det.

perf_table_row(TableInfo, SubjectCellFunc, RowData, table_row(Cells),
        Rank, Rank + 1) :-
    Ranked = TableInfo ^ table_ranked,
    Prefs = TableInfo ^ prefs,
    Fields = Prefs ^ pref_fields,

    % An optional rank number.
    (
        Ranked = ranked,
        RankCells = [table_cell(td_i(Rank))]
    ;
        Ranked = non_ranked,
        RankCells = []
    ),

    % The name of the procedure,
    SubjectCells = [SubjectCellFunc(TableInfo, RowData ^ subject)],

    % Build the port counts cells.
    PortFields = Fields ^ port_fields,
    (
        PortFields = port,
        Calls = RowData ^ calls,
        Exits = RowData ^ exits,
        Fails = RowData ^ fails,
        Redos = RowData ^ redos,
        Excps = RowData ^ excps,
        PortCells =
            [table_cell(td_i(Calls)), table_cell(td_i(Exits)),
            table_cell(td_i(Fails)), table_cell(td_i(Redos)),
            table_cell(td_i(Excps))]
    ;
        PortFields = no_port,
        PortCells = []
    ),

    % Build the time and ticks cells.
    TimeFields = Fields ^ time_fields,
    (
        TimeFields = no_time,
        TimeCells = []
    ;
        SelfTicksCell = table_cell(td_i(RowData ^ self_ticks)),
        SelfTimeCell = table_cell(td_t(RowData ^ self_time)),
        SelfTimePercentCell = table_cell(td_p(RowData ^ self_time_percent)),
        SelfTimePercallCell = table_cell(td_t(RowData ^ self_time_percall)),
        TicksCell = table_cell(td_i(RowData ^ ticks)),
        TimeCell = table_cell(td_t(RowData ^ time)),
        TimePercentCell = table_cell(td_p(RowData ^ time_percent)),
        TimePercallCell = table_cell(td_t(RowData ^ time_percall)),
        (
            TimeFields = ticks,
            TimeCells = [SelfTicksCell, SelfTimePercentCell,
                TicksCell, TimePercentCell]
        ;
            TimeFields = time,
            TimeCells = [SelfTimeCell, SelfTimePercentCell,
                TimeCell, TimePercentCell]
        ;
            TimeFields = ticks_and_time,
            TimeCells = [SelfTicksCell, SelfTimeCell, SelfTimePercentCell,
                TicksCell, TimeCell, TimePercentCell]
        ;
            TimeFields = time_and_percall,
            TimeCells = [SelfTimeCell, SelfTimePercentCell,
                    SelfTimePercallCell,
                TimeCell, TimePercentCell, TimePercallCell]
        ;
            TimeFields = ticks_and_time_and_percall,
            TimeCells =
                [SelfTicksCell, SelfTimeCell,
                SelfTimePercentCell, SelfTimePercallCell,
                TicksCell, TimeCell,
                TimePercentCell, TimePercallCell]
        )
    ),

    % Build call sequence numbers cells.
    CallSeqsFields = Fields ^ callseqs_fields,
    (
        CallSeqsFields = no_callseqs,
        CallSeqsCells = []
    ;
        SelfCallseqsCell = table_cell(td_i(RowData ^ self_callseqs)),
        SelfCallseqsPercentCell =
            table_cell(td_p(RowData ^ self_callseqs_percent)),
        CallseqsCell = table_cell(td_i(RowData ^ callseqs)),
        CallseqsPercentCell = table_cell(td_p(RowData ^ callseqs_percent)),
        (
            CallSeqsFields = callseqs,
            CallSeqsCells =
                [SelfCallseqsCell, SelfCallseqsPercentCell,
                CallseqsCell, CallseqsPercentCell]
        ;
            CallSeqsFields = callseqs_and_percall,
            SelfCallseqsPercallCell =
                table_cell(td_f(RowData ^ self_callseqs_percall)),
            CallseqsPercallCell = table_cell(td_f(RowData ^ callseqs_percall)),
            CallSeqsCells =
                [SelfCallseqsCell, SelfCallseqsPercentCell,
                SelfCallseqsPercallCell,
                CallseqsCell, CallseqsPercentCell,
                CallseqsPercallCell]
        )
    ),

    % Build allocation info.
    AllocFields = Fields ^ alloc_fields,
    (
        AllocFields = no_alloc,
        AllocCells = []
    ;
        SelfAllocsCell = table_cell(td_i(RowData ^ self_allocs)),
        SelfAllocsPercentCell =
            table_cell(td_p(RowData ^ self_allocs_percent)),
        AllocsCell = table_cell(td_i(RowData ^ allocs)),
        AllocsPercentCell = table_cell(td_p(RowData ^ allocs_percent)),
        (
            AllocFields = alloc,
            AllocCells =
                [SelfAllocsCell, SelfAllocsPercentCell,
                AllocsCell, AllocsPercentCell]
        ;
            AllocFields = alloc_and_percall,
            SelfAllocsPercallCell =
                table_cell(td_f(RowData ^ self_allocs_percall)),
            AllocsPercallCell = table_cell(td_f(RowData ^ allocs_percall)),
            AllocCells =
                [SelfAllocsCell, SelfAllocsPercentCell, SelfAllocsPercallCell,
                AllocsCell, AllocsPercentCell, AllocsPercallCell]
        )
    ),

    MemoryFields = Fields ^ memory_fields,
    (
        MemoryFields = no_memory,
        MemoryCells = []
    ;
        ( MemoryFields = memory(Units)
        ; MemoryFields = memory_and_percall(Units)
        ),
        SelfMemCell = table_cell(td_m(RowData ^ self_mem, Units, 0)),
        SelfMemPercallCell =
            table_cell(td_m(RowData ^ self_mem_percall, Units, 2)),
        MemCell = table_cell(td_m(RowData ^ mem, Units, 0)),
        MemPercallCell = table_cell(td_m(RowData ^ mem_percall, Units, 2)),
        SelfMemPercentCell = table_cell(td_p(RowData ^ self_mem_percent)),
        MemPercentCell = table_cell(td_p(RowData ^ mem_percent)),
        (
            MemoryFields = memory(_),
            MemoryCells =
                [SelfMemCell, SelfMemPercentCell,
                MemCell, MemPercentCell]
        ;
            MemoryFields = memory_and_percall(_),
            MemoryCells =
                [SelfMemCell, SelfMemPercentCell, SelfMemPercallCell,
                MemCell, MemPercentCell, MemPercallCell]
        )
    ),

    Cells = RankCells ++ SubjectCells ++ PortCells ++ TimeCells ++
        CallSeqsCells ++ AllocCells ++ MemoryCells.

    % Create the table header cell for the timing fields.
    %
:- pred proc_table_time_header(table_info::in, fields::in,
    maybe(table_header_cell)::out) is det.

proc_table_time_header(TableInfo, Fields, MaybeHeaderCell) :-
    TimeFields = Fields ^ time_fields,
    Self = top_procs_self_link(TableInfo, cost_time),
    Time = top_procs_time_link(TableInfo),
    SelfPercall = top_procs_self_percall_link(TableInfo, cost_time),
    Total = top_procs_total_link(TableInfo, cost_time),
    TotalTime = top_procs_total_time_link(TableInfo),
    TotalPercall = top_procs_total_percall_link(TableInfo, cost_time),

    (
        TimeFields = no_time,
        MaybeHeaderCell = no
    ;
        (
            TimeFields = ticks,
            Title = "Clock ticks",
            SubTitles = [Self, percent_label, Total, percent_label]
        ;
            TimeFields = time,
            Title = "Time",
            SubTitles = [Self, percent_label, Total, percent_label]
        ;
            TimeFields = ticks_and_time,
            Title = "Clock ticks and times",
            SubTitles = [Self, Time, percent_label,
                Total, TotalTime, percent_label]
        ;
            TimeFields = time_and_percall,
            Title = "Time",
            SubTitles =
                [Self, percent_label, SelfPercall,
                Total, percent_label, TotalPercall]
        ;
            TimeFields = ticks_and_time_and_percall,
            Title = "Clock ticks and times",
            SubTitles =
                [Self, Time, percent_label, SelfPercall,
                Total, TotalTime, percent_label, TotalPercall]
        ),
        MaybeHeaderCell = yes(table_header_group(Title, SubTitles,
            table_col_class_ticks_and_times))
    ).

    % Build the ports section of the header if required.
    %
:- pred proc_table_ports_header(table_info::in, fields::in,
    maybe(table_header_cell)::out) is det.

proc_table_ports_header(TableInfo, Fields, MaybePortsHeader) :-
    (
        Fields ^ port_fields = port,
        Calls = top_procs_make_table_link(TableInfo, "Calls",
            cost_calls, self, overall),
        Redos = top_procs_make_table_link(TableInfo, "Redos",
            cost_redos, self, overall),
        MaybePortsHeader = yes(table_header_group("Port counts",
            [Calls, td_s("Exits"), td_s("Fails"), Redos, td_s("Excps")],
            table_col_class_port_counts))
    ;
        Fields ^ port_fields = no_port,
        MaybePortsHeader = no
    ).

    % Create the table header cell for the call sequence count fields.
    %
:- pred proc_table_callseqs_header(table_info::in, fields::in,
    maybe(table_header_cell)::out) is det.

proc_table_callseqs_header(TableInfo, Fields, MaybeCallseqsHeader) :-
    Callseqs = Fields ^ callseqs_fields,
    Self = top_procs_self_link(TableInfo, cost_callseqs),
    Total = top_procs_total_link(TableInfo, cost_callseqs),
    (
        Callseqs = no_callseqs,
        MaybeCallseqsHeader = no
    ;
        (
            Callseqs = callseqs,
            SubTitles = [Self, percent_label, Total, percent_label]
        ;
            Callseqs = callseqs_and_percall,
            SelfPercall =
                top_procs_self_percall_link(TableInfo, cost_callseqs),
            TotalPercall =
                top_procs_total_percall_link(TableInfo, cost_callseqs),
            SubTitles =
                [Self, percent_label, SelfPercall,
                Total, percent_label, TotalPercall]
        ),
        MaybeCallseqsHeader = yes(table_header_group("Call sequence numbers",
            SubTitles, table_col_class_callseqs))
    ).

    % Build the header for the allocations column group.
    %
:- pred proc_table_allocations_header(table_info::in, fields::in,
    maybe(table_header_cell)::out) is det.

proc_table_allocations_header(TableInfo, Fields, MaybeHeader) :-
    AllocFields = Fields ^ alloc_fields,
    Self = top_procs_self_link(TableInfo, cost_allocs),
    Total = top_procs_total_link(TableInfo, cost_allocs),
    (
        AllocFields = no_alloc,
        MaybeHeader = no
    ;
        (
            AllocFields = alloc,
            SubTitles = [Self, percent_label, Total, percent_label]
        ;
            AllocFields = alloc_and_percall,
            SelfPercall =
                top_procs_self_percall_link(TableInfo, cost_allocs),
            TotalPercall =
                top_procs_total_percall_link(TableInfo, cost_allocs),
            SubTitles =
                [Self, percent_label, SelfPercall,
                Total, percent_label, TotalPercall]
        ),
        MaybeHeader = yes(table_header_group("Memory allocations", SubTitles,
            table_col_class_allocations))
    ).

    % Build the header for the memory usage column group.
    %
:- pred proc_table_memory_header(table_info::in, fields::in,
    maybe(table_header_cell)::out) is det.

proc_table_memory_header(TableInfo, Fields, MaybeHeader) :-
    Memory = Fields ^ memory_fields,
    Self = top_procs_self_link(TableInfo, cost_words),
    Total = top_procs_total_link(TableInfo, cost_words),
    Percent = percent_label,
    (
        Memory = no_memory,
        MaybeHeader = no
    ;
        (
            Memory = memory(Units),
            SubTitles = [Self, Percent, Total, Percent]
        ;
            Memory = memory_and_percall(Units),
            SelfPercall = top_procs_self_percall_link(TableInfo, cost_words),
            TotalPercall = top_procs_total_percall_link(TableInfo, cost_words),
            SubTitles =
                [Self, Percent, SelfPercall,
                Total, Percent, TotalPercall]
        ),
        (
            Units = units_words,
            Title = "Memory words"
        ;
            Units = units_bytes,
            Title = "Memory bytes"
        ),
        MaybeHeader = yes(table_header_group(Title, SubTitles,
            table_col_class_memory))
    ).

    % Build a header for a table of procedures.
    %
:- pred proc_table_header(table_info::in, int::out, table_header::out) is det.

proc_table_header(TableInfo, NumCols, Header) :-
    Prefs = TableInfo ^ prefs,
    Ranked = TableInfo ^ table_ranked,
    Fields = Prefs ^ pref_fields,
    some [!NumCols, !Cols]
    (
        !:NumCols = 0,
        !:Cols = cord.empty,
        (
            Ranked = ranked,
            RankedHeaderCell =
                table_header_cell(td_s("Rank"), table_col_class_ordinal_rank),
            table_add_header_col(RankedHeaderCell, !Cols, !NumCols)
        ;
            Ranked = non_ranked
        ),
        ProcHeaderCell =
            table_header_cell(td_s("Procedure"), table_col_class_proc),
        table_add_header_col(ProcHeaderCell, !Cols, !NumCols),

        proc_table_ports_header(TableInfo, Fields, MaybePortsHeader),
        table_maybe_add_header_col(MaybePortsHeader, !Cols, !NumCols),

        proc_table_time_header(TableInfo, Fields, MaybeTimeHeader),
        table_maybe_add_header_col(MaybeTimeHeader, !Cols, !NumCols),

        proc_table_callseqs_header(TableInfo, Fields, MaybeCallseqsHeader),
        table_maybe_add_header_col(MaybeCallseqsHeader, !Cols, !NumCols),

        proc_table_allocations_header(TableInfo, Fields,
            MaybeAllocationsHeader),
        table_maybe_add_header_col(MaybeAllocationsHeader, !Cols, !NumCols),

        proc_table_memory_header(TableInfo, Fields, MaybeMemoryHeader),
        table_maybe_add_header_col(MaybeMemoryHeader, !Cols, !NumCols),

        Header = table_header(cord.list(!.Cols)),
        NumCols = !.NumCols
    ).

%-----------------------------------------------------------------------------%
%
% Code to build controls seen at bottom of reports.
%

    % Build the sort controls.
    %
:- pred sort_controls(preferences::in, report_ordering::in, display_item::out)
    is det.

sort_controls(Prefs, Ordering, ControlsList) :-
    CurrentCostKind = Ordering ^ cost_kind,
    Costs0 = [cost_calls, cost_redos, cost_time, cost_callseqs, cost_allocs,
        cost_words],
    list.filter(not_unify(CurrentCostKind), Costs0, Costs1),
    list.map(make_sort_control(Ordering, Prefs), Costs1, Controls),

    ControlsList = display_list(list_class_horizontal, no, Controls).

:- pred cost_kind_label(cost_kind::in, string::out) is det.

cost_kind_label(cost_calls, "Sort by calls").
cost_kind_label(cost_redos, "Sort by redos").
cost_kind_label(cost_time, "Sort by time").
cost_kind_label(cost_callseqs, "Sort by call sequence numbers").
cost_kind_label(cost_allocs, "Sort by allocations").
cost_kind_label(cost_words, "Sort by words").

:- pred make_sort_control(report_ordering::in, preferences::in,
    cost_kind::in, display_item::out) is det.

make_sort_control(Ordering, Prefs, CostKind, display_command_link(Control)) :-
    InclDesc = Ordering ^ incl_desc,
    Scope = Ordering ^ scope,
    cost_kind_label(CostKind, Label),
    Control = top_procs_make_link(Ordering, Prefs, Label, CostKind, InclDesc,
        Scope, link_class_control).

%-----------------------------------------------------------------------------%

    % Create the controls for which measurements to include.
    %
:- pred incldesc_and_scope_controls(preferences::in, report_ordering::in,
    display_item::out) is det.

incldesc_and_scope_controls(Prefs, Ordering, ControlsList) :-
    Ordering =
        report_ordering(DisplayLimit, CostKind, CurrentInclDesc, CurrentScope),

    % Build InclDesc Control.
    (
        CurrentInclDesc = self,
        InclDescLabel = "Include descendants",
        InclDesc = self_and_desc
    ;
        CurrentInclDesc = self_and_desc,
        InclDescLabel = "Exclude descendants",
        InclDesc = self
    ),
    InclDescCmd =
        deep_cmd_top_procs(DisplayLimit, CostKind, InclDesc, CurrentScope),
    InclDescControl = deep_link(InclDescCmd, yes(Prefs), InclDescLabel,
        link_class_control),

    % Build Scope Control.
    (
        CurrentScope = overall,
        ScopeLabel = "Count per-call cost",
        Scope = per_call
    ;
        CurrentScope = per_call,
        ScopeLabel = "Count overall cost",
        Scope = overall
    ),

    ScopeCmd =
        deep_cmd_top_procs(DisplayLimit, CostKind, CurrentInclDesc, Scope),
    ScopeControl = deep_link(ScopeCmd, yes(Prefs), ScopeLabel,
        link_class_control),

    list.map(link_to_display, [InclDescControl, ScopeControl], Controls),
    ControlsList = display_list(list_class_horizontal, no, Controls).

    % Provide a predicate to be used as a higher order value that wraps the
    % display_command_link constructor.
    %
:- pred link_to_display(deep_link::in, display_item::out) is det.

link_to_display(Link, Display) :-
    Display = display_command_link(Link).

%-----------------------------------------------------------------------------%

:- pred display_controls(preferences::in, cmd::in, display_item::out) is det.

display_controls(Prefs, Cmd, ControlsList) :-
    Colour0 = Prefs ^ pref_colour,
    (
        Colour0 = colour_column_groups,
        Colour = colour_none,
        ColourLabel = "Fade column groups"
    ;
        Colour0 = colour_none,
        Colour = colour_column_groups,
        ColourLabel = "Colour column groups"
    ),
    ColourPrefs = Prefs ^ pref_colour := Colour,
    ColourControl = display_command_link(
        deep_link(Cmd, yes(ColourPrefs), ColourLabel, link_class_control)),

    Box0 = Prefs ^ pref_box,
    (
        Box0 = box,
        Box = nobox,
        BoxLabel = "Unbox"
    ;
        Box0 = nobox,
        Box = box,
        BoxLabel = "Box"
    ),
    BoxPrefs = Prefs ^ pref_box := Box,
    BoxControl = display_command_link(
        deep_link(Cmd, yes(BoxPrefs), BoxLabel, link_class_control)),

    ControlsList = display_list(list_class_horizontal, no,
        [ColourControl, BoxControl]).

%-----------------------------------------------------------------------------%

    % Create the field controls section.
    %
:- pred field_controls(preferences::in, cmd::in, display_item::out) is det.

field_controls(Prefs, Cmd, ControlsList) :-
    Fields = Prefs ^ pref_fields,
    Fields = fields(PortFields, TimeFields, CallseqsFields, AllocFields,
        MemoryFields),

    (
        PortFields = no_port,
        Port = port
    ;
        PortFields = port,
        Port = no_port
    ),
    port_label(Port, PortLabel),
    NewPortFields = Fields ^ port_fields := Port,
    PortPrefs = Prefs ^ pref_fields := NewPortFields,
    PortControl = display_list(list_class_horizontal, no, [
        display_command_link(deep_link(Cmd, yes(PortPrefs), PortLabel,
        link_class_control))]),

    AllTimeFields = [no_time, ticks, time, ticks_and_time, time_and_percall,
        ticks_and_time_and_percall],
    list.filter(not_unify(TimeFields), AllTimeFields, NewTimeFields),
    list.map(make_time_control(Cmd, Prefs), NewTimeFields, TimeControls),
    make_horizontal_list(TimeControls, TimeControlsList),

    AllCallseqsFields = [no_callseqs, callseqs, callseqs_and_percall],
    list.filter(not_unify(CallseqsFields), AllCallseqsFields,
        NewCallseqsFields),
    list.map(make_callseqs_control(Cmd, Prefs), NewCallseqsFields,
        CallseqsControls),
    make_horizontal_list(CallseqsControls, CallseqsControlsList),

    AllAllocFields = [no_alloc, alloc, alloc_and_percall],
    list.filter(not_unify(AllocFields), AllAllocFields, NewAllocFields),
    list.map(make_alloc_control(Cmd, Prefs), NewAllocFields, AllocControls),
    make_horizontal_list(AllocControls, AllocControlsList),

    AllMemoryFields = [no_memory, memory(units_words), memory(units_bytes),
        memory_and_percall(units_words), memory_and_percall(units_bytes)],
    list.filter(not_unify(MemoryFields), AllMemoryFields, NewMemoryFields),
    list.map(make_memory_control(Cmd, Prefs), NewMemoryFields, MemoryControls),
    make_horizontal_list(MemoryControls, MemoryControlsList),

    Controls = [PortControl, TimeControlsList, CallseqsControlsList,
        AllocControlsList, MemoryControlsList],

    ControlsList = display_list(list_class_vertical_no_bullets,
        yes("Toggle fields:"), Controls).

%-----------------------------------------------------------------------------%

    % Labels for the port fields controls.
    %
:- pred port_label(port_fields::in, string::out) is det.

port_label(port, "Port counts").
port_label(no_port, "No port counts").

%-----------------------------------------------------------------------------%

    % Make a time fields control using the given command and existing
    % preferences.  Makes a button to control which time fields are visible
    % depending on the third argument.
    %
:- pred make_time_control(cmd::in, preferences::in,
    time_fields::in, display_item::out) is det.

make_time_control(Cmd, Prefs, TimeFields, Control) :-
    make_fields_control(update_time_fields, time_label,
        Cmd, Prefs, TimeFields, Control).

:- pred update_time_fields(time_fields::in, fields::in, fields::out)
    is det.

update_time_fields(TimeFields, !Fields) :-
    !:Fields = !.Fields ^ time_fields := TimeFields.

    % Labels for the time fields controls.
    %
:- pred time_label(time_fields::in, string::out) is det.

time_label(no_time, "No time info").
time_label(ticks, "Ticks").
time_label(time, "Times").
time_label(ticks_and_time, "Ticks and times").
time_label(time_and_percall, "Times and per-call times").
time_label(ticks_and_time_and_percall, "Ticks and times and per-call times").

%-----------------------------------------------------------------------------%

    % Make a callseqs fields control using the given commandv and existing
    % preferences.  Makes a button to control which callseqs fields are visible
    % depending on the third argument.
    %
:- pred make_callseqs_control(cmd::in, preferences::in,
    callseqs_fields::in, display_item::out) is det.

make_callseqs_control(Cmd, Prefs, CallseqsFields, Control) :-
    make_fields_control(update_callseqs_fields, callseqs_label,
        Cmd, Prefs, CallseqsFields, Control).

:- pred update_callseqs_fields(callseqs_fields::in, fields::in, fields::out)
    is det.

update_callseqs_fields(CallseqsFields, !Fields) :-
    !:Fields = !.Fields ^ callseqs_fields := CallseqsFields.

:- pred callseqs_label(callseqs_fields::in, string::out) is det.

callseqs_label(no_callseqs, "No call sequence number info").
callseqs_label(callseqs, "Call sequence numbers").
callseqs_label(callseqs_and_percall,
    "Call sequence numbers including per-call").

%-----------------------------------------------------------------------------%

:- pred make_alloc_control(cmd::in, preferences::in,
    alloc_fields::in, display_item::out) is det.

make_alloc_control(Cmd, Prefs, AllocFields, Control) :-
    make_fields_control(update_alloc_fields, alloc_label,
        Cmd, Prefs, AllocFields, Control).

:- pred update_alloc_fields(alloc_fields::in, fields::in, fields::out)
    is det.

update_alloc_fields(AllocFields, !Fields) :-
    !:Fields = !.Fields ^ alloc_fields := AllocFields.

:- pred alloc_label(alloc_fields::in, string::out) is det.

alloc_label(no_alloc, "No allocations").
alloc_label(alloc, "Allocations").
alloc_label(alloc_and_percall, "Allocations and per-call allocations").

%-----------------------------------------------------------------------------%

:- pred make_memory_control(cmd::in, preferences::in,
    memory_fields::in, display_item::out) is det.

make_memory_control(Cmd, Prefs, MemoryFields, Control) :-
    make_fields_control(update_memory_fields, memory_label, Cmd, Prefs,
        MemoryFields, Control).

:- pred update_memory_fields(memory_fields::in, fields::in, fields::out)
    is det.

update_memory_fields(MemoryFields, !Fields) :-
    !:Fields = !.Fields ^ memory_fields := MemoryFields.

:- pred memory_label(memory_fields::in, string::out) is det.

memory_label(no_memory, "No memory info").
memory_label(memory(units_words), "Words").
memory_label(memory(units_bytes), "Bytes").
memory_label(memory_and_percall(units_words), "Words and per-call words").
memory_label(memory_and_percall(units_bytes), "Bytes and per-call bytes").

%-----------------------------------------------------------------------------%

:- pred make_fields_control(pred(T, fields, fields), pred(T, string),
    cmd, preferences, T, display_item).
:- mode make_fields_control(pred(in, in, out) is det, pred(in, out) is det,
    in, in, in, out) is det.

make_fields_control(UpdateFields, MakeLabel, Cmd, Prefs0, NewFields,
        Control) :-
    Fields0 = Prefs0 ^ pref_fields,
    UpdateFields(NewFields, Fields0, Fields),
    Prefs = Prefs0 ^ pref_fields := Fields,
    MakeLabel(NewFields, Label),
    Control = display_command_link(deep_link(Cmd, yes(Prefs), Label,
        link_class_control)).

%-----------------------------------------------------------------------------%
%
% Code shared within this module.
%

    % Give the common list of commands seen at the bottom of all deep-profiler
    % displayed reports.
    %
:- func cmds_menu_restart_quit = list(display_item).

cmds_menu_restart_quit = [Menu, Restart, Quit] :-
    Menu = display_command_link(deep_link(deep_cmd_menu, no, "Menu",
        link_class_control)),
    Restart = display_command_link(deep_link(deep_cmd_restart, no, "Restart",
        link_class_control)),
    Quit = display_command_link(deep_link(deep_cmd_quit, no, "Quit",
        link_class_control)).

%-----------------------------------------------------------------------------%

    % not_unify(A, B).
    %
    % This predicate is true when A \= B, and is usefull in higher order code.
    %
:- pred not_unify(T::in, T::in) is semidet.

not_unify(A, B) :-
    A \= B.

%-----------------------------------------------------------------------------%

:- func proc_desc_to_cell(table_info, proc_desc) = table_cell.

proc_desc_to_cell(TableInfo, ProcDesc) = table_cell(Data) :-
    Prefs = TableInfo ^ prefs,
    ProcDesc = proc_desc(PSPtr, _FileName, _LineNumber, RefinedName),
    PSPtr = proc_static_ptr(PSI),
    Cmd = deep_cmd_proc(PSI),
    Data = td_l(deep_link(Cmd, yes(Prefs), RefinedName, link_class_link)).

:- func call_site_desc_to_cell(table_info, call_site_desc) = table_cell.

call_site_desc_to_cell(TableInfo, CallSiteDesc) = table_cell(Data) :-
    Prefs = TableInfo ^ prefs,
    CallSiteDesc = call_site_desc(CSSPtr, _ContainerPSPtr,
        _FileName, _LineNumber, RefinedName, SlotNumber, GoalPath),
    string.format("%s @ %s #%d", [s(RefinedName), s(GoalPath), i(SlotNumber)],
        Name),
    CSSPtr = call_site_static_ptr(CSSI),
    Cmd = deep_cmd_call_site_static(CSSI),
    Data = td_l(deep_link(Cmd, yes(Prefs), Name, link_class_link)).

%-----------------------------------------------------------------------------%

    % Make a table row with two columns: a label and a value.
    %
:- func make_labelled_table_row(pair(string, table_data)) = table_row.

make_labelled_table_row(Label - Value) =
    table_row([table_cell(td_s(Label)), table_cell(Value)]).

    % Make a link for use in the menu report.
    %
:- pred make_command_link(pair(cmd, string)::in, display_item::out) is det.

make_command_link(Cmd - Label, Item) :-
    Item = display_command_link(deep_link(Cmd, no, Label, link_class_link)).

    % Make a Mercury list of display items into a display item representing
    % a horizontal list of these items.
    %
:- pred make_horizontal_list(list(display_item)::in, display_item::out) is det.

make_horizontal_list(Items, List) :-
    List = display_list(list_class_horizontal, no, Items).

%-----------------------------------------------------------------------------%
:- end_module display_report.
%-----------------------------------------------------------------------------%
