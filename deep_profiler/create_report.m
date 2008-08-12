%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: create_report.m.
% Author: pbone.
%
% This module creates a report from a deep data structure and a query.
%
%-----------------------------------------------------------------------------%

:- module create_report.
:- interface.

:- import_module report.
:- import_module profile.
:- import_module query.

%-----------------------------------------------------------------------------%

:- pred create_report(cmd::in, deep::in, deep_report::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module measurement_units.
:- import_module measurements.
:- import_module top_procs.

:- import_module array.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

create_report(Cmd, Deep, Report) :-
    (
        Cmd = deep_cmd_quit,
        Msg = string.format("Shutting down deep profile server for %s.",
            [s(Deep ^ data_file_name)]),
        MessageInfo = message_info(Msg),
        Report = report_message(MessageInfo)
    ;
        Cmd = deep_cmd_timeout(Timeout),
        Msg = string.format("Timeout set to %d minutes.", [i(Timeout)]),
        MessageInfo = message_info(Msg),
        Report = report_message(MessageInfo)
    ;
        Cmd = deep_cmd_menu,
        Deep ^ profile_stats = profile_stats(ProgramName, 
            NumCSD, NumCSS, NumPD, NumPS,
            QuantaPerSec, InstrumentationQuanta, UserQuanta, NumCallsequs,
            _, _),
        NumCliques = array.max(Deep ^ clique_members),
        MenuInfo = menu_info(ProgramName, QuantaPerSec, UserQuanta,
            InstrumentationQuanta, NumCallsequs, NumCSD, NumCSS, NumPD, NumPS,
            NumCliques),
        Report = report_menu(ok(MenuInfo))
    ;
        Cmd = deep_cmd_top_procs(Limit, CostKind, InclDesc, Scope),
        create_top_procs_report(Deep, Limit, CostKind, InclDesc, Scope,
            MaybeTopProcsInfo),
        Report = report_top_procs(MaybeTopProcsInfo)
    ;
        Cmd = deep_cmd_proc_static(PSI),
        generate_proc_static_dump_report(Deep, PSI, MaybeProcStaticDumpInfo),
        Report = report_proc_static_dump(MaybeProcStaticDumpInfo)
    ;
        Cmd = deep_cmd_proc_dynamic(PDI),
        generate_proc_dynamic_dump_report(Deep, PDI, MaybeProcDynamicDumpInfo),
        Report = report_proc_dynamic_dump(MaybeProcDynamicDumpInfo)
    ;
        Cmd = deep_cmd_call_site_static(CSSI),
        generate_call_site_static_dump_report(Deep, CSSI,
            MaybeCallSiteStaticDumpInfo),
        Report = report_call_site_static_dump(MaybeCallSiteStaticDumpInfo)
    ;
        Cmd = deep_cmd_call_site_dynamic(CSDI),
        generate_call_site_dynamic_dump_report(Deep, CSDI,
            MaybeCallSiteStaticDumpInfo),
        Report = report_call_site_dynamic_dump(MaybeCallSiteStaticDumpInfo)
    ;
        Cmd = deep_cmd_restart,
        error("create_report/3", "unexpected restart command")
    ;
        ( Cmd = deep_cmd_root(_)
        ; Cmd = deep_cmd_clique(_)
        ; Cmd = deep_cmd_proc(_)
        ; Cmd = deep_cmd_proc_callers(_, _, _)
        ; Cmd = deep_cmd_modules
        ; Cmd = deep_cmd_module(_)
        ; Cmd = deep_cmd_raw_clique(_)
        ),
        error("create_report/3", "Command not supported: " ++ string(Cmd))
    ).

%-----------------------------------------------------------------------------%
%
% Code to build top_procs report.
%

    % Create a top procs report, from the given data with the specified
    % parameters.
    %
:- pred create_top_procs_report(deep::in, display_limit::in, cost_kind::in,
    include_descendants::in, measurement_scope::in,
    maybe_error(top_procs_info)::out) is det.

create_top_procs_report(Deep, Limit, CostKind, InclDesc0, Scope0,
        MaybeTopProcsInfo) :-
    (
        CostKind = cost_calls,
        % Counting calls is incompatible both with self_and_desc
        % and per_call.
        InclDesc = self,
        Scope = overall
    ;
        ( CostKind = cost_redos
        ; CostKind = cost_time
        ; CostKind = cost_callseqs
        ; CostKind = cost_allocs
        ; CostKind = cost_words
        ),
        InclDesc = InclDesc0,
        Scope = Scope0
    ),
    MaybeTopPSIs = find_top_procs(CostKind, InclDesc, Scope, Limit, Deep),
    (
        MaybeTopPSIs = error(ErrorMessage),
        MaybeTopProcsInfo = error("Internal error: " ++ ErrorMessage)
    ;
        MaybeTopPSIs = ok(TopPSIs),
        Ordering = report_ordering(Limit, CostKind, InclDesc, Scope),
        list.map(psi_to_perf_row_data(Deep), TopPSIs, RowData),
        TopProcsInfo = top_procs_info(Ordering, RowData),
        MaybeTopProcsInfo = ok(TopProcsInfo)
    ).

%-----------------------------------------------------------------------------%
%
% Code to build the dump reports.
%

:- pred generate_proc_static_dump_report(deep::in, int::in,
    maybe_error(proc_static_dump_info)::out) is det.

generate_proc_static_dump_report(Deep, PSI, MaybeProcStaticDumpInfo) :-
    PSPtr = proc_static_ptr(PSI),
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        % Should we dump some other fields?
        PS = proc_static(_ProcId, _DeclModule, RefinedName, RawName,
            FileName, LineNumber, _InInterface, CallSites, _CoveragePoints,
            _IsZeroed),
        array.max(CallSites, NumCallSites),
        ProcStaticDumpInfo = proc_static_dump_info(PSPtr, RawName, RefinedName,
            FileName, LineNumber, NumCallSites),
        MaybeProcStaticDumpInfo = ok(ProcStaticDumpInfo)
    ;
        MaybeProcStaticDumpInfo = error("invalid proc_static index")
    ).

:- pred generate_proc_dynamic_dump_report(deep::in, int::in,
    maybe_error(proc_dynamic_dump_info)::out) is det.

generate_proc_dynamic_dump_report(Deep, PDI, MaybeProcDynamicDumpInfo) :-
    PDPtr = proc_dynamic_ptr(PDI),
    ( valid_proc_dynamic_ptr(Deep, PDPtr) ->
        deep_lookup_proc_dynamics(Deep, PDPtr, PD),
        PD = proc_dynamic(PSPtr, CallSiteArray),
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        RawName = PS ^ ps_raw_id,
        RefinedName = PS ^ ps_refined_id,
        array.to_list(CallSiteArray, CallSites),
        ProcDynamicDumpInfo = proc_dynamic_dump_info(PDPtr, PSPtr,
            RawName, RefinedName, CallSites),
        MaybeProcDynamicDumpInfo = ok(ProcDynamicDumpInfo)
    ;
        MaybeProcDynamicDumpInfo = error("invalid proc_dynamic index")
    ).

:- pred generate_call_site_static_dump_report(deep::in, int::in,
    maybe_error(call_site_static_dump_info)::out) is det.

generate_call_site_static_dump_report(Deep, CSSI,
        MaybeCallSiteStaticDumpInfo) :-
    CSSPtr = call_site_static_ptr(CSSI),
    ( valid_call_site_static_ptr(Deep, CSSPtr) ->
        deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
        CSS = call_site_static(ContainingPSPtr, SlotNumber, CallSiteKind,
            LineNumber, GoalPath),
        CallSiteStaticDumpInfo = call_site_static_dump_info(CSSPtr,
            ContainingPSPtr, SlotNumber, LineNumber, GoalPath, CallSiteKind),
        MaybeCallSiteStaticDumpInfo = ok(CallSiteStaticDumpInfo)
    ;
        MaybeCallSiteStaticDumpInfo = error("invalid call_site_static index")
    ).

:- pred generate_call_site_dynamic_dump_report(deep::in, int::in,
    maybe_error(call_site_dynamic_dump_info)::out) is det.

generate_call_site_dynamic_dump_report(Deep, CSDI,
        MaybeCallSiteDynamicDumpInfo) :-
    CSDPtr = call_site_dynamic_ptr(CSDI),
    ( valid_call_site_dynamic_ptr(Deep, CSDPtr) ->
        deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
        CSD = call_site_dynamic(CallerPSPtr, CalleePSDPtr, Own),
        Desc = zero_inherit_prof_info,
        deep_lookup_call_site_static_map(Deep, CSDPtr, CSSPtr),
        CallSiteDesc = describe_call_site(Deep, CSSPtr),
        own_and_inherit_to_perf_row_data(Deep, CallSiteDesc, Own, Desc,
            PerfRowData),
        CallSiteDynamicDumpInfo = call_site_dynamic_dump_info(CSDPtr,
            CallerPSPtr, CalleePSDPtr, PerfRowData),
        MaybeCallSiteDynamicDumpInfo = ok(CallSiteDynamicDumpInfo)
    ;
        MaybeCallSiteDynamicDumpInfo = error("invalid call_site_dynamic index")
    ).

%-----------------------------------------------------------------------------%

    % Lookup the proc_static structure with the given PSI index number
    % and return performance information about it.
    %
:- pred psi_to_perf_row_data(deep::in, int::in, perf_row_data(proc_desc)::out)
    is det.

psi_to_perf_row_data(Deep, PSI, RowData) :-
    PSPtr = proc_static_ptr(PSI),
    ProcDesc = describe_proc(Deep, PSPtr),
    deep_lookup_ps_own(Deep, PSPtr, Own),
    deep_lookup_ps_desc(Deep, PSPtr, Desc),
    own_and_inherit_to_perf_row_data(Deep, ProcDesc, Own, Desc, RowData).

:- pred own_and_inherit_to_perf_row_data(deep::in, T::in,
    own_prof_info::in, inherit_prof_info::in, perf_row_data(T)::out) is det.

own_and_inherit_to_perf_row_data(Deep, Subject, Own, Desc, RowData) :-
    % Look up global parameters and totals.
    ProfileStats = Deep ^ profile_stats,
    TicksPerSec = ProfileStats ^ ticks_per_sec,
    WordSize = ProfileStats ^ word_size,

    Root = root_total_info(Deep),
    TotalQuanta = inherit_quanta(Root),
    TotalCallseqs = inherit_callseqs(Root),
    TotalAllocs = inherit_allocs(Root),
    TotalWords = inherit_words(Root),

    % Port counts.
    Calls = calls(Own),
    Exits = exits(Own),
    Fails = fails(Own),
    Redos = redos(Own),
    Excps = excps(Own),

    % Self times.
    SelfTicks = quanta(Own),
    ticks_to_time(SelfTicks, TicksPerSec, SelfTime),
    SelfTimePercent = percent_from_ints(SelfTicks, TotalQuanta),
    time_percall(SelfTime, Calls, SelfTimePerCall),

    % Self + descendants times.
    Ticks = SelfTicks + inherit_quanta(Desc),
    ticks_to_time(Ticks, TicksPerSec, Time),
    TimePercent = percent_from_ints(Ticks, TotalQuanta),
    time_percall(Time, Calls, TimePerCall),

    % Self call sequence counts.
    SelfCallseqs = callseqs(Own),
    SelfCallseqsPercent = percent_from_ints(SelfCallseqs, TotalCallseqs),
    SelfCallseqsPerCall = divide_ints(SelfCallseqs, Calls),

    % Self + descendants call sequence counts.
    Callseqs = callseqs(Own) + inherit_callseqs(Desc),
    CallseqsPercent = percent_from_ints(Callseqs, TotalCallseqs),
    CallseqsPerCall = divide_ints(Callseqs, Calls),

    % Self memory allocations.
    SelfAllocs = allocs(Own),
    SelfAllocsPercent = percent_from_ints(SelfAllocs, TotalAllocs),
    SelfAllocsPerCall = divide_ints(SelfAllocs, Calls),

    % Self + descendants memory allocations.
    Allocs = SelfAllocs + inherit_allocs(Desc),
    AllocsPercent = percent_from_ints(Allocs, TotalAllocs),
    AllocsPerCall = divide_ints(Allocs, Calls),

    % Self memory words.
    SelfWords = words(Own),
    SelfMemory = memory_words(SelfWords, WordSize),
    SelfMemoryPercent = percent_from_ints(SelfWords, TotalWords),
    SelfMemoryPerCall = SelfMemory / Calls,

    % Self + descendants memory words.
    Words = SelfWords + inherit_words(Desc),
    Memory = memory_words(Words, WordSize),
    MemoryPercent = percent_from_ints(Words, TotalWords),
    MemoryPerCall = Memory / Calls,

    RowData = perf_row_data(Subject,
        Calls, Exits, Fails, Redos, Excps,

        SelfTicks, SelfTime, SelfTimePercent, SelfTimePerCall,
        Ticks, Time, TimePercent, TimePerCall,

        SelfCallseqs, SelfCallseqsPercent, SelfCallseqsPerCall,
        Callseqs, CallseqsPercent, CallseqsPerCall,

        SelfAllocs, SelfAllocsPercent, SelfAllocsPerCall,
        Allocs, AllocsPercent, AllocsPerCall,

        WordSize,
        SelfMemory, SelfMemoryPercent, SelfMemoryPerCall,
        Memory, MemoryPercent, MemoryPerCall
    ).

%-----------------------------------------------------------------------------%

    % divide_ints(Num, Demon) is the quotent of Nom and Denom, after they've
    % both been cast to float.
    %
:- func divide_ints(int, int) = float.

divide_ints(Num, Denom) = float(Num) / float(Denom).

%-----------------------------------------------------------------------------%

    % Give the percentage of two 'counts'.
    %
:- func percent_from_ints(int, int) = percent.

percent_from_ints(Nom, Denom) = percent(divide_ints(Nom, Denom)).

%-----------------------------------------------------------------------------%

    % Create a report_proc structure for a given proc static pointer.
    %
:- func describe_proc(deep, proc_static_ptr) = proc_desc.

describe_proc(Deep, PSPtr) = ProcDesc :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        FileName = PS ^ ps_file_name,
        LineNumber = PS ^ ps_line_number,
        RefinedName = PS ^ ps_refined_id
    ;
        FileName = "",
        LineNumber = 0,
        RefinedName = "mercury_runtime"
    ),
    ProcDesc = proc_desc(PSPtr, FileName, LineNumber, RefinedName).

    % Create a report_call_site structure for a given call site static pointer.
    %
:- func describe_call_site(deep, call_site_static_ptr) = call_site_desc.

describe_call_site(Deep, CSSPtr) = CallSiteDesc :-
    ( valid_call_site_static_ptr(Deep, CSSPtr) ->
        deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
        CSS = call_site_static(ContainingPSPtr, SlotNumber, _Kind, LineNumber,
            GoalPath),
        deep_lookup_proc_statics(Deep, ContainingPSPtr, ContainingPS),
        FileName = ContainingPS ^ ps_file_name,
        RefinedName = ContainingPS ^ ps_refined_id
    ;
        ContainingPSPtr = dummy_proc_static_ptr,
        FileName = "",
        LineNumber = 0,
        RefinedName = "mercury_runtime",
        SlotNumber = -1,
        GoalPath = ""
    ),
    CallSiteDesc = call_site_desc(CSSPtr, ContainingPSPtr,
        FileName, LineNumber, RefinedName, SlotNumber, GoalPath).

%-----------------------------------------------------------------------------%
%
% Code shared across entire module.
%

:- func this_file = string.

this_file = "create_report.m".

%-----------------------------------------------------------------------------%

:- func error_message(string, string) = string.

error_message(Pred, Message) = Error :-
    Error = this_file ++ ": " ++ Pred ++ ": " ++ Message.

%-----------------------------------------------------------------------------%

:- pred error(string::in, string::in) is erroneous.

error(Pred, Message) :-
    throw(software_error(error_message(Pred, Message))).

%-----------------------------------------------------------------------------%
:- end_module create_report.
%-----------------------------------------------------------------------------%
