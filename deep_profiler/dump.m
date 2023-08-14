%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: dump.m.
% Authors: juliensf, zs.
%
% This module contains code for dumping out the deep profiler's data structures
% for use in debugging the deep profiler.
%
%---------------------------------------------------------------------------%

:- module dump.
:- interface.

:- import_module profile.

:- import_module io.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % This structure controls what is printed how by the dump_deep/4 and
    % dump_initial_dump predicates.
    %
:- type dump_options
    --->    dump_options(
                do_restricted               :: show_restricted_dump,
                do_arrays                   :: set(dumpable_array),
                do_stats                    :: show_stats,
                do_dump_cliques             :: dump_cliques,
                do_dump_rev_links           :: dump_rev_links,
                do_dump_prop_measurements   :: dump_prop_measurements
            ).

    % This type is used to describe if a restricted set of "css" and "ps"
    % structures should be shown, (those for code that was executed),
    % or if all "css" and "ps" structures should be shown.
    %
:- type show_restricted_dump
    --->        show_restricted_dump
    ;           show_complete_dump.

    % This type indicates the arrays in the deep profile data that may be
    % printed.
    %
:- type dumpable_array
    --->        csd
    ;           css
    ;           pd
    ;           ps.

    % show_stats describes whether to show some statistics and the root node
    % in the dump.
    %
:- type show_stats
    --->        show_stats
    ;           do_not_show_stats.

    % Types to specify if cliques, rev (proc static to caller) links and
    % propagated measurements should be dumped by dump_deep/3.
    %
:- type dump_cliques
    --->        dump_cliques
    ;           do_not_dump_cliques.

:- type dump_rev_links
    --->        dump_rev_links
    ;           do_not_dump_rev_links.

:- type dump_prop_measurements
    --->        dump_prop_measurements
    ;           do_not_dump_prop_measurements.

%---------------------------------------------------------------------------%

    % Return the default dump options.
    %
:- func default_dump_options = dump_options.

    % dump_array_options will take a list of strings for the accumulating
    % dump options and produce a set if possible.
    %
    % A deterministic version is available that will throw an exception if
    % a string cannot be converted to an option.
    %
:- pred dump_array_options(list(string)::in, set(dumpable_array)::out)
    is semidet.
:- pred det_dump_array_options(list(string)::in, set(dumpable_array)::out)
    is det.

    % dump_array_options_to_dump_options takes a list of strings of the
    % accumulating array dump options and create a dump_options structure
    % based on the default plus these specific array options.
    %
:- pred dump_array_options_to_dump_options(list(string)::in,
    dump_options::out) is det.

    % dump_initial_deep(InitialDeep, DumpOptions, OutputStream, !IO):
    %
    % Print selected parts of InitialDeep to standard output.
    % Which parts to print is controlled by DumpOptions.
    %
:- pred dump_initial_deep(initial_deep::in, dump_options::in,
    io.text_output_stream::in, io::di, io::uo) is det.

    % dump_deep(Deep, DumpOptions, OutputStream, !IO):
    %
    % Dump selected parts of Deep to standard output. Information about cliques
    % is output if DumpOptions contains "clique". The fields of Deep that
    % contain reverse links are dumped if DumpOptions contains "rev".
    % The propagated costs are dumped if DumpOptions contains "prop".
    %
:- pred dump_deep(deep::in, dump_options::in, io.text_output_stream::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module coverage.
:- import_module array_util.
:- import_module measurements.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

default_dump_options = DumpOptions :-
    DumpOptions = dump_options(show_complete_dump, all_array_options,
        show_stats, dump_cliques, dump_rev_links, dump_prop_measurements).

dump_array_options(Strings, DumpArrayOptions) :-
    ( if
        dump_array_options_special(Strings, DumpArrayOptionsSpecial)
    then
        DumpArrayOptions = DumpArrayOptionsSpecial
    else
        string_list_to_sym_set(string_to_dumpable_array, Strings,
            DumpArrayOptions)
    ).

det_dump_array_options(Strings, DumpArrayOptions) :-
    ( if
        dump_array_options_special(Strings, DumpArrayOptionsSpecial)
    then
        DumpArrayOptions = DumpArrayOptionsSpecial
    else
        string_list_to_sym_set(det_string_to_dumpable_array, Strings,
            DumpArrayOptions)
    ).

    % Handle special cases in the list of array options.
    %
:- pred dump_array_options_special(list(string)::in, set(dumpable_array)::out)
    is semidet.

dump_array_options_special([], all_array_options).
dump_array_options_special(["all"], all_array_options).

dump_array_options_to_dump_options(Strings, DumpOptions) :-
    det_dump_array_options(Strings, DumpArrayOptions),
    DumpOptions = default_dump_options ^ do_arrays := DumpArrayOptions.

:- pred string_list_to_sym_set(pred(string, X), list(string), set(X)).
:- mode string_list_to_sym_set(in(pred(in, out) is det), in, out) is det.
:- mode string_list_to_sym_set(in(pred(in, out) is semidet), in, out)
    is semidet.

string_list_to_sym_set(StrToSym, StrList, Set) :-
    list.map(StrToSym, StrList, List),
    list_to_set(List, Set).

:- pred det_string_to_dumpable_array(string::in, dumpable_array::out) is det.

det_string_to_dumpable_array(String, Array) :-
    ( if string_to_dumpable_array(String, ArrayP) then
        Array = ArrayP
    else
        unexpected($pred, "Invalid array name in dump options: " ++ String)
    ).

:- pred string_to_dumpable_array(string::in, dumpable_array::out) is semidet.

string_to_dumpable_array("csd", csd).
string_to_dumpable_array("css", css).
string_to_dumpable_array("pd", pd).
string_to_dumpable_array("ps", ps).

:- func all_array_options = set(dumpable_array).

all_array_options = Set :-
    Set = set.list_to_set([csd, css, pd, ps]).

%---------------------------------------------------------------------------%
%
% Dump initial deep profiling structure.
%

dump_initial_deep(InitialDeep, DumpOptions, OutputStream, !IO) :-
    InitialDeep = initial_deep(Stats, InitRoot, CSDs, PDs, CSSs, PSs),
    Restrict = DumpOptions ^ do_restricted,
    (
        Restrict = show_restricted_dump,
        get_static_ptrs_from_dynamic_procs(PDs, PSs, UsedPSs, UsedCSSs),
        Restriction = these(UsedPSs, UsedCSSs)
    ;
        Restrict = show_complete_dump,
        Restriction = none
    ),
    ShowStats = DumpOptions ^ do_stats,
    (
        ShowStats = show_stats,
        dump_init_profile_stats(OutputStream, Stats, !IO),
        dump_init_root(OutputStream, InitRoot, !IO)
    ;
        ShowStats = do_not_show_stats
    ),
    ( if should_dump(DumpOptions, csd) then
        dump_init_call_site_dynamics(OutputStream, CSDs, !IO)
    else
        true
    ),
    ( if should_dump(DumpOptions, pd) then
        dump_init_proc_dynamics(OutputStream, PDs, PSs, !IO)
    else
        true
    ),
    ( if should_dump(DumpOptions, css) then
        dump_init_call_site_statics(OutputStream, Restriction, CSSs, !IO)
    else
        true
    ),
    ( if should_dump(DumpOptions, ps) then
        dump_init_proc_statics(OutputStream, Restriction, PSs, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%
%
% Restricting static structures to those referenced by dynamic ones
%

:- type restriction
    --->    none
    ;       these(set(proc_static_ptr), set(call_site_static_ptr)).

:- pred get_static_ptrs_from_dynamic_procs(proc_dynamics::in, proc_statics::in,
    set(proc_static_ptr)::out, set(call_site_static_ptr)::out) is det.

get_static_ptrs_from_dynamic_procs(ProcDynamics, ProcStatics, PS_Ptrs,
        CSS_Ptrs) :-
    array_foldl2_from_1(get_static_ptrs_from_dynamic_proc(ProcStatics),
        ProcDynamics, set.init, PS_Ptrs, set.init, CSS_Ptrs).

:- pred get_static_ptrs_from_dynamic_proc(proc_statics::in, int::in,
    proc_dynamic::in, set(proc_static_ptr)::in, set(proc_static_ptr)::out,
    set(call_site_static_ptr)::in, set(call_site_static_ptr)::out) is det.

get_static_ptrs_from_dynamic_proc(ProcStatics, _, ProcDynamic, !PS_Ptrs,
        !CSS_Ptrs) :-
    ProcStaticPtr = ProcDynamic ^ pd_proc_static,
    set.insert(ProcStaticPtr, !PS_Ptrs),
    lookup_proc_statics(ProcStatics, ProcStaticPtr, ProcStatic),
    CSSs = array.to_list(ProcStatic ^ ps_sites),
    set.insert_list(CSSs, !CSS_Ptrs).

%---------------------------------------------------------------------------%
%
% Code for dumping profile_stats
%

:- pred dump_init_profile_stats(io.text_output_stream::in, profile_stats::in,
    io::di, io::uo) is det.

dump_init_profile_stats(OutputStream, Stats, !IO) :-
    Stats = profile_stats(ProgramName, MaxCSD, MaxCSS, MaxPD, MaxPS,
        TicksPerSec, InstrumentQuanta, UserQuanta, NumCallSeqs, DeepFlags),
    io.write_string(OutputStream,
        "SECTION PROFILING STATS:\n\n", !IO),
    io.write_string(OutputStream,
        "\tprogram_name = " ++ ProgramName ++ "\n", !IO),
    io.format(OutputStream, "\tmax_csd = %d\n", [i(MaxCSD)], !IO),
    io.format(OutputStream, "\tmax_css = %d\n", [i(MaxCSS)], !IO),
    io.format(OutputStream, "\tmax_pd  = %d\n", [i(MaxPD)],  !IO),
    io.format(OutputStream, "\tmax_ps  = %d\n", [i(MaxPS)],  !IO),
    io.format(OutputStream, "\tticks_per_sec = %d\n", [i(TicksPerSec)], !IO),
    io.format(OutputStream, "\tinstrument_quanta = %d\n",
        [i(InstrumentQuanta)], !IO),
    io.format(OutputStream, "\tuser_quanta = %d\n", [i(UserQuanta)], !IO),
    io.format(OutputStream, "\tnum_callseqs = %d\n", [i(NumCallSeqs)], !IO),
    DeepFlags = deep_flags(WordSize, Canonical, Compression, CoverageDataType),
    io.format(OutputStream, "\tword_size   = %d\n", [i(WordSize)], !IO),
    io.write_string(OutputStream, "\tcanonical = ", !IO),
    (
        Canonical = is_canonical,
        io.write_string(OutputStream, "yes\n", !IO)
    ;
        Canonical = maybe_not_canonical,
        io.write_string(OutputStream, "no\n", !IO)
    ),
    io.write_string(OutputStream, "\tcompression = ", !IO),
    (
        Compression = no_compression,
        io.write_string(OutputStream, "none\n", !IO)
    ),
    io.format(OutputStream, "\tcoverage_data_type = %s\n\n",
        [s(string(CoverageDataType))], !IO).

%---------------------------------------------------------------------------%

:- pred dump_init_root(io.text_output_stream::in, proc_dynamic_ptr::in,
    io::di, io::uo) is det.

dump_init_root(OutputStream, proc_dynamic_ptr(Root), !IO) :-
    io.write_string(OutputStream, "INITIAL ROOT:\n", !IO),
    io.format(OutputStream, "\tinitial root = %d\n\n", [i(Root)], !IO).

%---------------------------------------------------------------------------%

:- pred dump_init_call_site_dynamics(io.text_output_stream::in,
    call_site_dynamics::in, io::di, io::uo) is det.

dump_init_call_site_dynamics(OutputStream, CallSiteDynamics, !IO) :-
    io.write_string(OutputStream, "SECTION CALL SITE DYNAMICS:\n\n", !IO),
    array_foldl_from_1(dump_call_site_dynamic(OutputStream),
        CallSiteDynamics, !IO).

:- pred dump_call_site_dynamic(io.text_output_stream::in, int::in,
    call_site_dynamic::in, io::di, io::uo) is det.

dump_call_site_dynamic(OutputStream, Index, CallSiteDynamic, !IO) :-
    CallSiteDynamic = call_site_dynamic(CallerPDPtr, CalleePDPtr, Own),
    CallerPDPtr = proc_dynamic_ptr(CallerPDI),
    CalleePDPtr = proc_dynamic_ptr(CalleePDI),
    io.format(OutputStream, "csd%d:\n", [i(Index)], !IO),
    io.format(OutputStream, "\tcsd_caller = pd%d\n", [i(CallerPDI)], !IO),
    io.format(OutputStream, "\tcsd_callee = pd%d\n", [i(CalleePDI)], !IO),
    dump_own_prof_info(OutputStream, Own, !IO),
    io.nl(OutputStream, !IO).

:- pred dump_own_prof_info(io.text_output_stream::in, own_prof_info::in,
    io::di, io::uo) is det.

dump_own_prof_info(OutputStream, Own, !IO) :-
    decompress_profile(Own, Calls, Exits, Fails, Redos, Excps,
        Quanta, CallSeqs, Allocs, Words),
    ( if Calls = 0 then
        true
    else
        io.format(OutputStream, "\tcalls:\t\t%d\n", [i(Calls)], !IO)
    ),
    ( if Exits = 0 then
        true
    else
        io.format(OutputStream, "\texits:\t\t%d\n", [i(Exits)], !IO)
    ),
    ( if Fails = 0 then
        true
    else
        io.format(OutputStream, "\tfails:\t\t%d\n", [i(Fails)], !IO)
    ),
    ( if Redos = 0 then
        true
    else
        io.format(OutputStream, "\tredos:\t\t%d\n", [i(Redos)], !IO)
    ),
    ( if Excps = 0 then
        true
    else
        io.format(OutputStream, "\texcps:\t\t%d\n", [i(Excps)], !IO)
    ),
    ( if Quanta = 0 then
        true
    else
        io.format(OutputStream, "\tquanta:\t\t%d\n", [i(Quanta)], !IO)
    ),
    ( if CallSeqs = 0 then
        true
    else
        io.format(OutputStream, "\tcallseqs:\t%d\n", [i(CallSeqs)], !IO)
    ),
    ( if Allocs = 0 then
        true
    else
        io.format(OutputStream, "\tallocs:\t\t%d\n", [i(Allocs)], !IO)
    ),
    ( if Words = 0 then
        true
    else
        io.format(OutputStream, "\twords:\t\t%d\n", [i(Words)], !IO)
    ).

:- pred dump_inherit_prof_info(io.text_output_stream::in,
    inherit_prof_info::in, io::di, io::uo) is det.

dump_inherit_prof_info(OutputStream, Inherit, !IO) :-
    Quanta = inherit_quanta(Inherit),
    CallSeqs = inherit_callseqs(Inherit),
    Allocs = inherit_allocs(Inherit),
    Words = inherit_words(Inherit),
    ( if Quanta = 0 then
        true
    else
        io.format(OutputStream, "\tquanta:\t\t%d\n", [i(Quanta)], !IO)
    ),
    ( if CallSeqs = 0 then
        true
    else
        io.format(OutputStream, "\tcallseqs:\t%d\n", [i(CallSeqs)], !IO)
    ),
    ( if Allocs = 0 then
        true
    else
        io.format(OutputStream, "\tallocs:\t\t%d\n", [i(Allocs)], !IO)
    ),
    ( if Words = 0 then
        true
    else
        io.format(OutputStream, "\twords:\t\t%d\n", [i(Words)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred dump_init_proc_dynamics(io.text_output_stream::in,
    proc_dynamics::in, proc_statics::in, io::di, io::uo) is det.

dump_init_proc_dynamics(OutputStream, ProcDynamics, ProcStatics, !IO) :-
    io.write_string(OutputStream, "SECTION PROC DYNAMICS:\n\n", !IO),
    array_foldl_from_1(dump_proc_dynamic(OutputStream, ProcStatics),
        ProcDynamics, !IO).

:- pred dump_proc_dynamic(io.text_output_stream::in, proc_statics::in,
    int::in, proc_dynamic::in, io::di, io::uo) is det.

dump_proc_dynamic(OutputStream, ProcStatics, Index, ProcDynamic, !IO) :-
    ProcDynamic = proc_dynamic(PSPtr, Sites, MaybeCPs),
    PSPtr = proc_static_ptr(PSI),
    lookup_proc_statics(ProcStatics, PSPtr, PS),
    ( if PS ^ ps_q_refined_id = "" then
        QualRefinedPSId = "UNKNOWN_PS"
    else
        QualRefinedPSId = PS ^ ps_q_refined_id
    ),
    io.format(OutputStream, "pd%d:\n", [i(Index)], !IO),
    io.format(OutputStream, "\tpd_proc_static = %d (%s)\n",
        [i(PSI), s(QualRefinedPSId)], !IO),
    array_foldl_from_0(dump_call_site_array_slot(OutputStream), Sites, !IO),
    (
        MaybeCPs = yes(CPCounts),
        CPInfos = PS ^ ps_coverage_point_infos,
        coverage_point_arrays_to_list(CPInfos, CPCounts, CPs),
        io.write_string(OutputStream, "Coverage points:\n", !IO),
        list.foldl2(dump_coverage_point(OutputStream), CPs, 0, _, !IO)
    ;
        MaybeCPs = no
    ),
    io.nl(OutputStream, !IO).

:- pred dump_call_site_array_slot(io.text_output_stream::in, int::in,
    call_site_array_slot::in, io::di, io::uo) is det.

dump_call_site_array_slot(OutputStream, Index, CSA_slot, !IO) :-
    io.format(OutputStream, "\tpd_site[%d] = %s\n",
        [i(Index), s(call_site_array_slot_to_string(CSA_slot))], !IO).

:- func call_site_array_slot_to_string(call_site_array_slot) = string.

call_site_array_slot_to_string(slot_normal(call_site_dynamic_ptr(CSDI))) =
    string.format("normal(csd%d)", [i(CSDI)]).
call_site_array_slot_to_string(slot_multi(_, _)) = "multi".

%---------------------------------------------------------------------------%

:- pred dump_init_call_site_statics(io.text_output_stream::in, restriction::in,
    call_site_statics::in, io::di, io::uo) is det.

dump_init_call_site_statics(OutputStream, Restriction, CallStatics, !IO) :-
    io.write_string(OutputStream, "SECTION CALL SITE STATICS:\n\n", !IO),
    array_foldl_from_1(dump_call_site_static(OutputStream, Restriction),
        CallStatics, !IO).

:- pred dump_call_site_static(io.text_output_stream::in, restriction::in,
    int::in, call_site_static::in, io::di, io::uo) is det.

dump_call_site_static(OutputStream, Restriction, Index, CallSiteStatic, !IO) :-
    ( if
        (
            Restriction = none
        ;
            Restriction = these(_, UsedCallSiteStatics),
            set.member(call_site_static_ptr(Index), UsedCallSiteStatics)
        )
    then
        CallSiteStatic = call_site_static(ContainerPSPtr, SlotNum,
            Kind, LineNum, GoalPath),
        ContainerPSPtr = proc_static_ptr(ContainerPSI),
        GoalPathString = rev_goal_path_to_string(GoalPath),
        io.format(OutputStream, "css%d:\n", [i(Index)], !IO),
        io.format(OutputStream, "\tcss_container\t= ps%d\n",
            [i(ContainerPSI)], !IO),
        io.format(OutputStream, "\tcss_slot_num\t= <%d>\n", [i(SlotNum)], !IO),
        io.format(OutputStream, "\tcss_line_num\t= <%d>\n", [i(LineNum)], !IO),
        io.format(OutputStream, "\tcss_goal_path\t= <%s>\n",
            [s(GoalPathString)], !IO),
        io.write_string(OutputStream, "\tcss_kind\t= ", !IO),
        dump_call_site_kind_and_callee(OutputStream, Kind, !IO),
        io.write_string(OutputStream, "\n\n", !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred dump_init_proc_statics(io.text_output_stream::in, restriction::in,
    proc_statics::in, io::di, io::uo) is det.

dump_init_proc_statics(OutputStream, Restriction, ProcStatics, !IO) :-
    io.write_string(OutputStream, "SECTION PROC STATICS:\n\n", !IO),
    array_foldl_from_1(dump_proc_static(OutputStream, Restriction),
        ProcStatics, !IO),
    io.nl(OutputStream, !IO).

:- pred dump_proc_static(io.text_output_stream::in, restriction::in,
    int::in, proc_static::in, io::di, io::uo) is det.

dump_proc_static(OutputStream, Restriction, Index, ProcStatic, !IO) :-
    ( if
        (
            Restriction = none
        ;
            Restriction = these(UsedProcStatics, _),
            set.member(proc_static_ptr(Index), UsedProcStatics)
        )
    then
        ProcStatic = proc_static(Id, DeclModule,
            _UnQualRefinedId, QualRefinedId, RawId, FileName, LineNumber,
            InInterface, Sites, CoveragePointInfos, MaybeCoveragePoints,
            IsZeroed),
        IdStr = dump_proc_id(Id),
        io.format(OutputStream, "ps%d:\n", [i(Index)], !IO),
        io.format(OutputStream, "\tps_id\t\t= %s\n", [s(IdStr)], !IO),
        io.format(OutputStream, "\tps_decl_module\t= %s\n",
            [s(DeclModule)], !IO),
        ( if QualRefinedId = DeclModule ++ "." ++ IdStr then
            % The output is too big already; don't include
            % redundant information.
            true
        else
            io.format(OutputStream, "\tps_q_refined_id\t= %s\n",
                [s(QualRefinedId)], !IO)
        ),
        ( if QualRefinedId \= RawId then
            % The output is too big already; don't include
            % redundant information.
            true
        else
            io.format(OutputStream, "\tps_raw_id\t= %s\n", [s(RawId)], !IO)
        ),
        io.format(OutputStream, "\tlocation\t= %s:%d\n",
            [s(FileName), i(LineNumber)], !IO),
        (
            InInterface = yes,
            io.write_string(OutputStream, "\tin_interface\n", !IO)
        ;
            InInterface = no
        ),
        (
            IsZeroed = zeroed,
            IsZeroStr = "zeroed"
        ;
            IsZeroed = not_zeroed,
            IsZeroStr = "not_zeroed"
        ),
        io.format(OutputStream, "\t%s\n", [s(IsZeroStr)], !IO),
        array_foldl_from_0(dump_proc_static_call_sites(OutputStream),
            Sites, !IO),
        (
            MaybeCoveragePoints = yes(CoveragePointsArray),
            coverage_point_arrays_to_list(CoveragePointInfos,
                CoveragePointsArray, CoveragePoints),
            list.foldl2(dump_coverage_point(OutputStream), CoveragePoints,
                0, _, !IO)
        ;
            MaybeCoveragePoints = no,
            io.write_string(OutputStream,
                "\tCoverage counts not present in proc static\n", !IO),
            array_foldl_from_0(dump_coverage_point_info(OutputStream),
                CoveragePointInfos, !IO)
        ),
        io.nl(OutputStream, !IO)
    else
        true
    ).

:- pred dump_proc_static_call_sites(io.text_output_stream::in, int::in,
    call_site_static_ptr::in, io::di, io::uo) is det.

dump_proc_static_call_sites(OutputStream, Slot, CSSPtr, !IO) :-
    CSSPtr = call_site_static_ptr(CSSI),
    io.format(OutputStream, "\tps_site[%d]: css%d\n", [i(Slot), i(CSSI)], !IO).

:- pred dump_coverage_point(io.text_output_stream::in, coverage_point::in,
    int::in, int::out, io::di, io::uo) is det.

dump_coverage_point(OutputStream, CoveragePoint, !Num, !IO) :-
    CoveragePoint = coverage_point(Count, Path, Type),
    CPInfo = coverage_point_info(Path, Type),
    format_cp_info(!.Num, CPInfo, CPInfoStr),
    io.format(OutputStream, "\t%s: %d\n", [s(CPInfoStr), i(Count)], !IO),
    !:Num = !.Num + 1.

:- pred dump_coverage_point_info(io.text_output_stream::in, int::in,
    coverage_point_info::in, io::di, io::uo) is det.

dump_coverage_point_info(OutputStream, Num, CoveragePointInfo, !IO) :-
    format_cp_info(Num, CoveragePointInfo, CPInfoStr),
    io.format(OutputStream, "\t%s\n", [s(CPInfoStr)], !IO).

:- pred format_cp_info(int::in, coverage_point_info::in, string::out) is det.

format_cp_info(Num, coverage_point_info(RevPath, CPType), String) :-
    rev_goal_path_to_string(RevPath) = PathString,
    string.format("coverage_point[%d]: %s, %s",
        [i(Num), s(string(CPType)), s(PathString)], String).

%---------------------------------------------------------------------------%

:- func dump_proc_id(string_proc_label) = string.

dump_proc_id(Proc) = Str :-
    (
        Proc = str_ordinary_proc_label(PredOrFunc, _DeclModule, _DefnModule,
            Name, Arity, Mode),
        (
            PredOrFunc = pf_predicate,
            Suffix = ""
        ;
            PredOrFunc = pf_function,
            Suffix = "+1"
        ),
        string.format("%s/%d-%d%s",
            [s(Name), i(Arity), i(Mode), s(Suffix)], Str)
    ;
        Proc = str_special_proc_label(Type, _TypeModule, _DefModule,
            Name, _Arity, _Mode),
        string.format("%s predicate for type `%s'", [s(Name), s(Type)], Str)
    ).

%---------------------------------------------------------------------------%

:- pred dump_call_site_kind_and_callee(io.text_output_stream::in,
    call_site_kind_and_callee::in, io::di, io::uo) is det.

dump_call_site_kind_and_callee(OutputStream, KindAndCallee, !IO) :-
    (
        KindAndCallee = normal_call_and_callee(Ptr, String),
        Ptr = proc_static_ptr(Val),
        io.format(OutputStream, "normal_call(%d, \"%s\")",
            [i(Val), s(String)], !IO)
    ;
        (
            KindAndCallee = special_call_and_no_callee,
            KindAndCalleeStr = "special_call"
        ;
            KindAndCallee = higher_order_call_and_no_callee,
            KindAndCalleeStr = "higher_order_call"
        ;
            KindAndCallee = method_call_and_no_callee,
            KindAndCalleeStr = "method_call"
        ;
            KindAndCallee = callback_and_no_callee,
            KindAndCalleeStr = "callback"
        ),
        io.write_string(OutputStream, KindAndCalleeStr, !IO)
    ).

%---------------------------------------------------------------------------%

dump_deep(Deep, DumpOptions, OutputStream, !IO) :-
    DumpCliques = DumpOptions ^ do_dump_cliques,
    DumpRevLinks = DumpOptions ^ do_dump_rev_links,
    DumpPropMeasurements = DumpOptions ^ do_dump_prop_measurements,
    (
        DumpCliques = dump_cliques,
        dump_deep_cliques(OutputStream, Deep, !IO)
    ;
        DumpCliques = do_not_dump_cliques
    ),
    (
        DumpRevLinks = dump_rev_links,
        dump_deep_rev_links(OutputStream, Deep, !IO)
    ;
        DumpRevLinks = do_not_dump_rev_links
    ),
    (
        DumpPropMeasurements = dump_prop_measurements,
        dump_deep_prop_measurements(OutputStream, Deep, !IO)
    ;
        DumpPropMeasurements = do_not_dump_prop_measurements
    ).

%---------------------------------------------------------------------------%

:- pred dump_deep_cliques(io.text_output_stream::in, deep::in,
    io::di, io::uo) is det.

dump_deep_cliques(OutputStream, Deep, !IO) :-
    CliqueIndex = Deep ^ clique_index,
    io.write_string(OutputStream,
        "SECTION MAP FROM PROC DYNAMIC TO CLIQUE:\n\n", !IO),
    array_foldl_from_1(dump_clique_index_entry(OutputStream),
        CliqueIndex, !IO),
    io.nl(OutputStream, !IO),

    CliqueMembers = Deep ^ clique_members,
    io.write_string(OutputStream,
        "SECTION MAP FROM CLIQUE TO PROC DYNAMICS:\n\n", !IO),
    array_foldl_from_1(dump_clique_members(OutputStream), CliqueMembers, !IO),
    io.nl(OutputStream, !IO),

    CliqueParents = Deep ^ clique_parents,
    io.write_string(OutputStream,
        "SECTION MAP FROM CLIQUE TO PARENT CSD:\n\n", !IO),
    array_foldl_from_1(dump_clique_parent(OutputStream), CliqueParents, !IO),
    io.nl(OutputStream, !IO),

    CliqueMaybeChild = Deep ^ clique_maybe_child,
    io.write_string(OutputStream,
        "SECTION MAP FROM CSD TO MAYBE CHILD CLIQUE:\n\n", !IO),
    array_foldl_from_1(dump_clique_maybe_child(OutputStream),
        CliqueMaybeChild, !IO),
    io.nl(OutputStream, !IO).

:- pred dump_clique_index_entry(io.text_output_stream::in, int::in,
    clique_ptr::in, io::di, io::uo) is det.

dump_clique_index_entry(OutputStream, Index, CliquePtr, !IO) :-
    CliquePtr = clique_ptr(CliqueNum),
    io.format(OutputStream, "pd%d is in clique%d\n",
        [i(Index), i(CliqueNum)], !IO).

:- pred dump_clique_members(io.text_output_stream::in, int::in,
    list(proc_dynamic_ptr)::in, io::di, io::uo) is det.

dump_clique_members(OutputStream, Index, CliqueMembers, !IO) :-
    io.format(OutputStream, "clique%d members:", [i(Index)], !IO),
    list.foldl(dump_pd_in_clique(OutputStream), CliqueMembers, !IO),
    io.nl(OutputStream, !IO).

:- pred dump_pd_in_clique(io.text_output_stream::in, proc_dynamic_ptr::in,
    io::di, io::uo) is det.

dump_pd_in_clique(OutputStream, PDPtr, !IO) :-
    PDPtr = proc_dynamic_ptr(PDNum),
    io.format(OutputStream, " pd%d", [i(PDNum)], !IO).

:- pred dump_clique_parent(io.text_output_stream::in, int::in,
    call_site_dynamic_ptr::in, io::di, io::uo) is det.

dump_clique_parent(OutputStream, Index, CSDPtr, !IO) :-
    CSDPtr = call_site_dynamic_ptr(CSDNum),
    io.format(OutputStream, "clique%d parent: csd%d\n",
        [i(Index), i(CSDNum)], !IO).

:- pred dump_clique_maybe_child(io.text_output_stream::in, int::in,
    maybe(clique_ptr)::in, io::di, io::uo) is det.

dump_clique_maybe_child(OutputStream, Index, MaybeCliquePtr, !IO) :-
    (
        MaybeCliquePtr = no
    ;
        MaybeCliquePtr = yes(CliquePtr),
        CliquePtr = clique_ptr(CliqueNum),
        io.format(OutputStream, "csd%d child: clique%d\n",
            [i(Index), i(CliqueNum)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred dump_deep_rev_links(io.text_output_stream::in, deep::in,
    io::di, io::uo) is det.

dump_deep_rev_links(OutputStream, Deep, !IO) :-
    ProcCallers = Deep ^ proc_callers,
    io.write_string(OutputStream,
        "SECTION MAP FROM PROC STATIC TO CALLER CSDs:\n\n", !IO),
    array_foldl_from_1(dump_proc_static_caller_csds(OutputStream),
        ProcCallers, !IO),
    io.nl(OutputStream, !IO),

    CallSiteStaticMap = Deep ^ call_site_static_map,
    io.write_string(OutputStream,
        "SECTION MAP FROM CALL SITE DYNAMICS TO STATICS:\n\n", !IO),
    array_foldl_from_1(dump_call_site_dynamic_to_static(OutputStream),
        CallSiteStaticMap, !IO),
    io.nl(OutputStream, !IO),

    CallSiteCalls = Deep ^ call_site_calls,
    io.write_string(OutputStream,
        "SECTION MAP FROM CALL SITE STATICS TO CALLS:\n\n", !IO),
    array_foldl_from_1(dump_call_site_calls(OutputStream),
        CallSiteCalls, !IO),
    io.nl(OutputStream, !IO).

:- pred dump_proc_static_caller_csds(io.text_output_stream::in, int::in,
    list(call_site_dynamic_ptr)::in, io::di, io::uo) is det.

dump_proc_static_caller_csds(OutputStream, Index, CallerCSDs, !IO) :-
    (
        CallerCSDs = []
    ;
        CallerCSDs = [_ | _],
        io.format(OutputStream, "ps%d callers:", [i(Index)], !IO),
        list.foldl(dump_space_csdptr(OutputStream), CallerCSDs, !IO),
        io.nl(OutputStream, !IO)
    ).

:- pred dump_space_csdptr(io.text_output_stream::in, call_site_dynamic_ptr::in,
    io::di, io::uo) is det.

dump_space_csdptr(OutputStream, CSDPtr, !IO) :-
    CSDPtr = call_site_dynamic_ptr(CSDNum),
    io.format(OutputStream, " csd%d", [i(CSDNum)], !IO).

:- pred dump_call_site_dynamic_to_static(io.text_output_stream::in,
    int::in, call_site_static_ptr::in, io::di, io::uo) is det.

dump_call_site_dynamic_to_static(OutputStream, Index, CSSPtr, !IO) :-
    CSSPtr = call_site_static_ptr(CSSNum),
    io.format(OutputStream, "csd%d is at css%d\n", [i(Index), i(CSSNum)], !IO).

:- pred dump_call_site_calls(io.text_output_stream::in, int::in,
    map(proc_static_ptr, list(call_site_dynamic_ptr))::in,
    io::di, io::uo) is det.

dump_call_site_calls(OutputStream, Index, CalleeMap, !IO) :-
    CalleeList = map.to_assoc_list(CalleeMap),
    (
        CalleeList = []
    ;
        CalleeList = [OneCallee],
        io.format(OutputStream, "css%d calls one procedure: ",
            [i(Index)], !IO),
        dump_call_site_calls_to_proc(OutputStream, "", OneCallee, !IO)
    ;
        CalleeList = [_, _ | _],
        io.format(OutputStream, "css%d calls several procedures:\n",
            [i(Index)], !IO),
        list.foldl(dump_call_site_calls_to_proc(OutputStream, "\t"),
            CalleeList, !IO)
    ).

:- pred dump_call_site_calls_to_proc(io.text_output_stream::in, string::in,
    pair(proc_static_ptr, list(call_site_dynamic_ptr))::in,
    io::di, io::uo) is det.

dump_call_site_calls_to_proc(OutputStream, Prefix, PSPtr - CSDPtrs, !IO) :-
    PSPtr = proc_static_ptr(PSNum),
    io.format(OutputStream, "%sps%d:", [s(Prefix), i(PSNum)], !IO),
    list.foldl(dump_space_csdptr(OutputStream), CSDPtrs, !IO),
    io.nl(OutputStream, !IO).

%---------------------------------------------------------------------------%

:- pred dump_deep_prop_measurements(io.text_output_stream::in, deep::in,
    io::di, io::uo) is det.

dump_deep_prop_measurements(OutputStream, Deep, !IO) :-
    PDOwn = Deep ^ pd_own,
    PDDesc = Deep ^ pd_desc,
    PDOwnMax = array.max(PDOwn),
    PDDescMax = array.max(PDDesc),
    require(unify(PDOwnMax, PDDescMax),
        "dump_deep: PDOwnMax != PDDescMax"),
    io.write_string(OutputStream,
        "SECTION PROC DYNAMIC MEASUREMENTS:\n\n", !IO),
    dump_pd_measurements(OutputStream, 1, PDOwnMax, PDOwn, PDDesc, !IO),

    CSDs = Deep ^ call_site_dynamics,
    CSDDesc = Deep ^ csd_desc,
    CSDMax = array.max(CSDs),
    CSDDescMax = array.max(CSDDesc),
    require(unify(CSDMax, CSDDescMax),
        "dump_deep: CSDMax != CSDDescMax"),
    io.write_string(OutputStream,
        "SECTION CALL SITE DYNAMIC MEASUREMENTS:\n\n", !IO),
    dump_csd_measurements(OutputStream, 1, CSDMax, CSDs, CSDDesc, !IO),

    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    PSOwnMax = array.max(PSOwn),
    PSDescMax = array.max(PSDesc),
    require(unify(PSOwnMax, PSDescMax),
        "dump_deep: PSOwnMax != PSDescMax"),
    io.write_string(OutputStream,
        "SECTION PROC STATIC MEASUREMENTS:\n\n", !IO),
    dump_ps_measurements(OutputStream, 1, PSOwnMax, PSOwn, PSDesc, !IO),

    CSSOwn = Deep ^ css_own,
    CSSDesc = Deep ^ css_desc,
    CSSOwnMax = array.max(CSSOwn),
    CSSDescMax = array.max(CSSDesc),
    require(unify(CSSOwnMax, CSSDescMax),
        "dump_deep: CSSOwnMax != CSSDescMax"),
    io.write_string(OutputStream,
        "SECTION CALL SITE STATIC MEASUREMENTS:\n\n", !IO),
    dump_css_measurements(OutputStream, 1, CSSOwnMax, CSSOwn, CSSDesc, !IO).

:- pred dump_pd_measurements(io.text_output_stream::in, int::in, int::in,
    array(own_prof_info)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_pd_measurements(OutputStream, Cur, Max, PDOwn, PDDesc, !IO) :-
    ( if Cur =< Max then
        array.lookup(PDOwn, Cur, Own),
        array.lookup(PDDesc, Cur, Desc),
        dump_own_and_desc(OutputStream, "pd", Cur, Own, Desc, !IO),
        dump_pd_measurements(OutputStream, Cur + 1, Max, PDOwn, PDDesc, !IO)
    else
        true
    ).

:- pred dump_csd_measurements(io.text_output_stream::in, int::in, int::in,
    array(call_site_dynamic)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_csd_measurements(OutputStream, Cur, Max, CSDs, CSDDesc, !IO) :-
    ( if Cur =< Max then
        array.lookup(CSDs, Cur, CSD),
        Own = CSD ^ csd_own_prof,
        array.lookup(CSDDesc, Cur, Desc),
        dump_own_and_desc(OutputStream, "OutputStream, csd", Cur, Own,
            Desc, !IO),
        dump_csd_measurements(OutputStream, Cur + 1, Max, CSDs, CSDDesc, !IO)
    else
        true
    ).

:- pred dump_ps_measurements(io.text_output_stream::in, int::in, int::in,
    array(own_prof_info)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_ps_measurements(OutputStream, Cur, Max, PSOwn, PSDesc, !IO) :-
    ( if Cur =< Max then
        array.lookup(PSOwn, Cur, Own),
        array.lookup(PSDesc, Cur, Desc),
        dump_own_and_desc(OutputStream, "ps", Cur, Own, Desc, !IO),
        dump_ps_measurements(OutputStream, Cur + 1, Max, PSOwn, PSDesc, !IO)
    else
        true
    ).

:- pred dump_css_measurements(io.text_output_stream::in, int::in, int::in,
    array(own_prof_info)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_css_measurements(OutputStream, Cur, Max, CSSOwn, CSSDesc, !IO) :-
    ( if Cur =< Max then
        array.lookup(CSSOwn, Cur, Own),
        array.lookup(CSSDesc, Cur, Desc),
        dump_own_and_desc(OutputStream, "css", Cur, Own, Desc, !IO),
        dump_css_measurements(OutputStream, Cur + 1, Max, CSSOwn, CSSDesc, !IO)
    else
        true
    ).

:- pred dump_own_and_desc(io.text_output_stream::in, string::in, int::in,
    own_prof_info::in, inherit_prof_info::in, io::di, io::uo) is det.

dump_own_and_desc(OutputStream, Prefix, Cur, Own, Desc, !IO) :-
    ( if is_zero_own_prof_info(Own) then
        PrintedOwn = no
    else
        io.format(OutputStream, "%s%d own:\n", [s(Prefix), i(Cur)], !IO),
        dump_own_prof_info(OutputStream, Own, !IO),
        PrintedOwn = yes
    ),
    ( if is_zero_inherit_prof_info(Desc) then
        PrintedDesc = no
    else
        io.format(OutputStream, "%s%d inherit:\n", [s(Prefix), i(Cur)], !IO),
        dump_inherit_prof_info(OutputStream, Desc, !IO),
        PrintedDesc = yes
    ),
    ( if
        ( PrintedOwn = yes
        ; PrintedDesc = yes
        )
    then
        io.nl(OutputStream, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred should_dump(dump_options::in, dumpable_array::in) is semidet.

should_dump(DumpOptions, What) :-
    Arrays = DumpOptions ^ do_arrays,
    set.member(What, Arrays).

%---------------------------------------------------------------------------%
:- end_module dump.
%---------------------------------------------------------------------------%
