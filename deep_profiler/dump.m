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

    % dump_initial_deep(InitialDeep, DumpOptions, !IO):
    %
    % Print selected parts of InitialDeep to standard output.
    % Which parts to print is controlled by DumpOptions.
    %
:- pred dump_initial_deep(initial_deep::in, dump_options::in, io::di,
    io::uo) is det.

    % dump_deep(Deep, DumpOptions, !IO):
    %
    % Dump selected parts of Deep to standard output. Information about cliques
    % is output if DumpOptions contains "clique". The fields of Deep that
    % contain reverse links are dumped if DumpOptions contains "rev".
    % The propagated costs are dumped if DumpOptions contains "prop".
    %
:- pred dump_deep(deep::in, dump_options::in, io::di, io::uo) is det.

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
:- mode string_list_to_sym_set(pred(in, out) is det, in, out) is det.
:- mode string_list_to_sym_set(pred(in, out) is semidet, in, out) is semidet.

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

dump_initial_deep(InitialDeep, DumpOptions, !IO) :-
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
        dump_init_profile_stats(Stats, !IO),
        dump_init_root(InitRoot, !IO)
    ;
        ShowStats = do_not_show_stats
    ),
    ( if should_dump(DumpOptions, csd) then
        dump_init_call_site_dynamics(CSDs, !IO)
    else
        true
    ),
    ( if should_dump(DumpOptions, pd) then
        dump_init_proc_dynamics(PDs, PSs, !IO)
    else
        true
    ),
    ( if should_dump(DumpOptions, css) then
        dump_init_call_site_statics(Restriction, CSSs, !IO)
    else
        true
    ),
    ( if should_dump(DumpOptions, ps) then
        dump_init_proc_statics(Restriction, PSs, !IO)
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

:- pred dump_init_profile_stats(profile_stats::in, io::di, io::uo) is det.

dump_init_profile_stats(Stats, !IO) :-
    Stats = profile_stats(ProgramName, MaxCSD, MaxCSS, MaxPD, MaxPS,
        TicksPerSec, InstrumentQuanta, UserQuanta, NumCallSeqs, DeepFlags),
    io.write_string("SECTION PROFILING STATS:\n\n", !IO),
    io.write_string("\tprogram_name = " ++ ProgramName ++ "\n", !IO),
    io.format("\tmax_csd = %d\n", [i(MaxCSD)], !IO),
    io.format("\tmax_css = %d\n", [i(MaxCSS)], !IO),
    io.format("\tmax_pd  = %d\n", [i(MaxPD)],  !IO),
    io.format("\tmax_ps  = %d\n", [i(MaxPS)],  !IO),
    io.format("\tticks_per_sec = %d\n", [i(TicksPerSec)], !IO),
    io.format("\tinstrument_quanta = %d\n", [i(InstrumentQuanta)], !IO),
    io.format("\tuser_quanta = %d\n", [i(UserQuanta)], !IO),
    io.format("\tnum_callseqs = %d\n", [i(NumCallSeqs)], !IO),
    DeepFlags = deep_flags(WordSize, Canonical, Compression, CoverageDataType),
    io.format("\tword_size   = %d\n", [i(WordSize)], !IO),
    io.write_string("\tcanonical = ", !IO),
    (
        Canonical = is_canonical,
        io.write_string("yes\n", !IO)
    ;
        Canonical = maybe_not_canonical,
        io.write_string("no\n", !IO)
    ),
    io.write_string("\tcompression = ", !IO),
    (
        Compression = no_compression,
        io.write_string("none\n", !IO)
    ),
    io.format("\tcoverage_data_type = %s\n", [s(string(CoverageDataType))],
        !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred dump_init_root(proc_dynamic_ptr::in, io::di, io::uo) is det.

dump_init_root(proc_dynamic_ptr(Root), !IO) :-
    io.write_string("INITIAL ROOT:\n", !IO),
    io.format("\tinitial root = %d\n", [i(Root)], !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred dump_init_call_site_dynamics(call_site_dynamics::in, io::di, io::uo)
    is det.

dump_init_call_site_dynamics(CallSiteDynamics, !IO) :-
    io.write_string("SECTION CALL SITE DYNAMICS:\n\n", !IO),
    array_foldl_from_1(dump_call_site_dynamic, CallSiteDynamics, !IO).

:- pred dump_call_site_dynamic(int::in, call_site_dynamic::in, io::di, io::uo)
    is det.

dump_call_site_dynamic(Index, CallSiteDynamic, !IO) :-
    CallSiteDynamic = call_site_dynamic(CallerPDPtr, CalleePDPtr, Own),
    CallerPDPtr = proc_dynamic_ptr(CallerPDI),
    CalleePDPtr = proc_dynamic_ptr(CalleePDI),
    io.format("csd%d:\n", [i(Index)], !IO),
    io.format("\tcsd_caller = pd%d\n", [i(CallerPDI)], !IO),
    io.format("\tcsd_callee = pd%d\n", [i(CalleePDI)], !IO),
    dump_own_prof_info(Own, !IO),
    io.nl(!IO).

:- pred dump_own_prof_info(own_prof_info::in, io::di, io::uo) is det.

dump_own_prof_info(Own, !IO) :-
    decompress_profile(Own, Calls, Exits, Fails, Redos, Excps,
        Quanta, CallSeqs, Allocs, Words),
    ( if Calls = 0 then
        true
    else
        io.format("\tcalls:\t\t%d\n", [i(Calls)], !IO)
    ),
    ( if Exits = 0 then
        true
    else
        io.format("\texits:\t\t%d\n", [i(Exits)], !IO)
    ),
    ( if Fails = 0 then
        true
    else
        io.format("\tfails:\t\t%d\n", [i(Fails)], !IO)
    ),
    ( if Redos = 0 then
        true
    else
        io.format("\tredos:\t\t%d\n", [i(Redos)], !IO)
    ),
    ( if Excps = 0 then
        true
    else
        io.format("\texcps:\t\t%d\n", [i(Excps)], !IO)
    ),
    ( if Quanta = 0 then
        true
    else
        io.format("\tquanta:\t\t%d\n", [i(Quanta)], !IO)
    ),
    ( if CallSeqs = 0 then
        true
    else
        io.format("\tcallseqs:\t%d\n", [i(CallSeqs)], !IO)
    ),
    ( if Allocs = 0 then
        true
    else
        io.format("\tallocs:\t\t%d\n", [i(Allocs)], !IO)
    ),
    ( if Words = 0 then
        true
    else
        io.format("\twords:\t\t%d\n", [i(Words)], !IO)
    ).

:- pred dump_inherit_prof_info(inherit_prof_info::in, io::di, io::uo) is det.

dump_inherit_prof_info(Inherit, !IO) :-
    Quanta = inherit_quanta(Inherit),
    CallSeqs = inherit_callseqs(Inherit),
    Allocs = inherit_allocs(Inherit),
    Words = inherit_words(Inherit),
    ( if Quanta = 0 then
        true
    else
        io.format("\tquanta:\t\t%d\n", [i(Quanta)], !IO)
    ),
    ( if CallSeqs = 0 then
        true
    else
        io.format("\tcallseqs:\t%d\n", [i(CallSeqs)], !IO)
    ),
    ( if Allocs = 0 then
        true
    else
        io.format("\tallocs:\t\t%d\n", [i(Allocs)], !IO)
    ),
    ( if Words = 0 then
        true
    else
        io.format("\twords:\t\t%d\n", [i(Words)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred dump_init_proc_dynamics(proc_dynamics::in, proc_statics::in,
    io::di, io::uo) is det.

dump_init_proc_dynamics(ProcDynamics, ProcStatics, !IO) :-
    io.write_string("SECTION PROC DYNAMICS:\n\n", !IO),
    array_foldl_from_1(dump_proc_dynamic(ProcStatics), ProcDynamics, !IO).

:- pred dump_proc_dynamic(proc_statics::in, int::in, proc_dynamic::in,
    io::di, io::uo) is det.

dump_proc_dynamic(ProcStatics, Index, ProcDynamic, !IO) :-
    ProcDynamic = proc_dynamic(PSPtr, Sites, MaybeCPs),
    PSPtr = proc_static_ptr(PSI),
    lookup_proc_statics(ProcStatics, PSPtr, PS),
    ( if PS ^ ps_q_refined_id = "" then
        QualRefinedPSId = "UNKNOWN_PS"
    else
        QualRefinedPSId = PS ^ ps_q_refined_id
    ),
    io.format("pd%d:\n", [i(Index)], !IO),
    io.format("\tpd_proc_static = %d (%s)\n",
        [i(PSI), s(QualRefinedPSId)], !IO),
    array_foldl_from_0(dump_call_site_array_slot, Sites, !IO),
    (
        MaybeCPs = yes(CPCounts),
        CPInfos = PS ^ ps_coverage_point_infos,
        coverage_point_arrays_to_list(CPInfos, CPCounts, CPs),
        io.write_string("Coverage points:\n", !IO),
        foldl2(dump_coverage_point, CPs, 0, _, !IO)
    ;
        MaybeCPs = no
    ),
    io.nl(!IO).

:- pred dump_call_site_array_slot(int::in, call_site_array_slot::in,
    io::di, io::uo) is det.

dump_call_site_array_slot(Index, CSA_slot, !IO) :-
    io.format("\tpd_site[%d] = %s\n",
        [i(Index), s(call_site_array_slot_to_string(CSA_slot))], !IO).

:- func call_site_array_slot_to_string(call_site_array_slot) = string.

call_site_array_slot_to_string(slot_normal(call_site_dynamic_ptr(CSDI))) =
    string.format("normal(csd%d)", [i(CSDI)]).
call_site_array_slot_to_string(slot_multi(_, _)) = "multi".

%---------------------------------------------------------------------------%

:- pred dump_init_call_site_statics(restriction::in, call_site_statics::in,
    io::di, io::uo) is det.

dump_init_call_site_statics(Restriction, CallStatics, !IO) :-
    io.write_string("SECTION CALL SITE STATICS:\n\n", !IO),
    array_foldl_from_1(dump_call_site_static(Restriction), CallStatics, !IO).

:- pred dump_call_site_static(restriction::in, int::in, call_site_static::in,
    io::di, io::uo) is det.

dump_call_site_static(Restriction, Index, CallSiteStatic, !IO) :-
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
        io.format("css%d:\n", [i(Index)], !IO),
        io.format("\tcss_container\t= ps%d\n", [i(ContainerPSI)], !IO),
        io.format("\tcss_slot_num\t= <%d>\n", [i(SlotNum)], !IO),
        io.format("\tcss_line_num\t= <%d>\n", [i(LineNum)], !IO),
        io.format("\tcss_goal_path\t= <%s>\n", [s(GoalPathString)], !IO),
        io.write_string("\tcss_kind\t= ", !IO),
        dump_call_site_kind_and_callee(Kind, !IO),
        io.nl(!IO),
        io.nl(!IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred dump_init_proc_statics(restriction::in, proc_statics::in,
    io::di, io::uo) is det.

dump_init_proc_statics(Restriction, ProcStatics, !IO) :-
    io.write_string("SECTION PROC STATICS:\n\n", !IO),
    array_foldl_from_1(dump_proc_static(Restriction), ProcStatics, !IO),
    io.nl(!IO).

:- pred dump_proc_static(restriction::in, int::in, proc_static::in,
    io::di, io::uo) is det.

dump_proc_static(Restriction, Index, ProcStatic, !IO) :-
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
        io.format("ps%d:\n", [i(Index)], !IO),
        io.format("\tps_id\t\t= %s", [s(IdStr)], !IO),
        io.nl(!IO),
        io.format("\tps_decl_module\t= %s\n", [s(DeclModule)], !IO),
        ( if QualRefinedId = DeclModule ++ "." ++ IdStr then
            % The output is too big already; don't include
            % redundant information.
            true
        else
            io.format("\tps_q_refined_id\t= %s\n", [s(QualRefinedId)], !IO)
        ),
        ( if QualRefinedId \= RawId then
            % The output is too big already; don't include
            % redundant information.
            true
        else
            io.format("\tps_raw_id\t= %s\n", [s(RawId)], !IO)
        ),
        io.format("\tlocation\t= %s:%d\n", [s(FileName), i(LineNumber)], !IO),
        (
            InInterface = yes,
            io.write_string("\tin_interface\n", !IO)
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
        io.format("\t%s\n", [s(IsZeroStr)], !IO),
        array_foldl_from_0(dump_proc_static_call_sites, Sites, !IO),
        (
            MaybeCoveragePoints = yes(CoveragePointsArray),
            coverage_point_arrays_to_list(CoveragePointInfos,
                CoveragePointsArray, CoveragePoints),
            list.foldl2(dump_coverage_point, CoveragePoints, 0, _, !IO)
        ;
            MaybeCoveragePoints = no,
            io.write_string("\tCoverage counts not present in proc static\n",
                !IO),
            array_foldl_from_0(dump_coverage_point_info, CoveragePointInfos,
                !IO)
        ),
        io.nl(!IO)
    else
        true
    ).

:- pred dump_proc_static_call_sites(int::in, call_site_static_ptr::in,
    io::di, io::uo) is det.

dump_proc_static_call_sites(Slot, CSSPtr, !IO) :-
    CSSPtr = call_site_static_ptr(CSSI),
    io.format("\tps_site[%d]: css%d\n", [i(Slot), i(CSSI)], !IO).

:- pred dump_coverage_point(coverage_point::in, int::in, int::out,
    io::di, io::uo) is det.

dump_coverage_point(CoveragePoint, !Num, !IO) :-
    CoveragePoint = coverage_point(Count, Path, Type),
    CPInfo = coverage_point_info(Path, Type),
    format_cp_info(!.Num, CPInfo, CPInfoStr),
    io.format("\t%s: %d\n", [s(CPInfoStr), i(Count)], !IO),
    !:Num = !.Num + 1.

:- pred dump_coverage_point_info(int::in, coverage_point_info::in,
    io::di, io::uo) is det.

dump_coverage_point_info(Num, CoveragePointInfo, !IO) :-
    format_cp_info(Num, CoveragePointInfo, CPInfoStr),
    io.format("\t%s\n", [s(CPInfoStr)], !IO).

:- pred format_cp_info(int::in, coverage_point_info::in, string::out) is det.

format_cp_info(Num, coverage_point_info(RevPath, CPType), String) :-
    rev_goal_path_to_string(RevPath) = PathString,
    format("coverage_point[%d]: %s, %s",
        [i(Num), s(string(CPType)), s(PathString)], String).

%---------------------------------------------------------------------------%

:- func dump_proc_id(string_proc_label) = string.

dump_proc_id(Proc) = Str :-
    Proc = str_ordinary_proc_label(PredOrFunc, _DeclModule, _DefnModule, Name,
        Arity, Mode),
    (
        PredOrFunc = pf_predicate,
        Suffix = ""
    ;
        PredOrFunc = pf_function,
        Suffix = "+1"
    ),
    string.format("%s/%d-%d%s", [s(Name), i(Arity), i(Mode), s(Suffix)],
        Str).
dump_proc_id(Proc) = Str :-
    Proc = str_special_proc_label(Type, _TypeModule, _DefModule, Name,
        _Arity, _Mode),
    string.format("%s predicate for type `%s'", [s(Name), s(Type)], Str).

%---------------------------------------------------------------------------%

:- pred dump_call_site_kind_and_callee(call_site_kind_and_callee::in,
    io::di, io::uo) is det.

dump_call_site_kind_and_callee(normal_call_and_callee(Ptr, String), !IO) :-
    Ptr = proc_static_ptr(Val),
    io.format("normal_call(%d, \"%s\")", [i(Val), s(String)], !IO).
dump_call_site_kind_and_callee(special_call_and_no_callee, !IO) :-
    io.write_string("special_call", !IO).
dump_call_site_kind_and_callee(higher_order_call_and_no_callee, !IO) :-
    io.write_string("higher_order_call", !IO).
dump_call_site_kind_and_callee(method_call_and_no_callee, !IO) :-
    io.write_string("method_call", !IO).
dump_call_site_kind_and_callee(callback_and_no_callee, !IO) :-
    io.write_string("callback", !IO).

%---------------------------------------------------------------------------%

dump_deep(Deep, DumpOptions, !IO) :-
    DumpCliques = DumpOptions ^ do_dump_cliques,
    DumpRevLinks = DumpOptions ^ do_dump_rev_links,
    DumpPropMeasurements = DumpOptions ^ do_dump_prop_measurements,
    (
        DumpCliques = dump_cliques,
        dump_deep_cliques(Deep, !IO)
    ;
        DumpCliques = do_not_dump_cliques
    ),
    (
        DumpRevLinks = dump_rev_links,
        dump_deep_rev_links(Deep, !IO)
    ;
        DumpRevLinks = do_not_dump_rev_links
    ),
    (
        DumpPropMeasurements = dump_prop_measurements,
        dump_deep_prop_measurements(Deep, !IO)
    ;
        DumpPropMeasurements = do_not_dump_prop_measurements
    ).

%---------------------------------------------------------------------------%

:- pred dump_deep_cliques(deep::in, io::di, io::uo) is det.

dump_deep_cliques(Deep, !IO) :-
    CliqueIndex = Deep ^ clique_index,
    io.write_string("SECTION MAP FROM PROC DYNAMIC TO CLIQUE:\n\n", !IO),
    array_foldl_from_1(dump_clique_index_entry, CliqueIndex, !IO),
    io.nl(!IO),

    CliqueMembers = Deep ^ clique_members,
    io.write_string("SECTION MAP FROM CLIQUE TO PROC DYNAMICS:\n\n", !IO),
    array_foldl_from_1(dump_clique_members, CliqueMembers, !IO),
    io.nl(!IO),

    CliqueParents = Deep ^ clique_parents,
    io.write_string("SECTION MAP FROM CLIQUE TO PARENT CSD:\n\n", !IO),
    array_foldl_from_1(dump_clique_parent, CliqueParents, !IO),
    io.nl(!IO),

    CliqueMaybeChild = Deep ^ clique_maybe_child,
    io.write_string("SECTION MAP FROM CSD TO MAYBE CHILD CLIQUE:\n\n",
        !IO),
    array_foldl_from_1(dump_clique_maybe_child, CliqueMaybeChild, !IO),
    io.nl(!IO).

:- pred dump_clique_index_entry(int::in, clique_ptr::in,
    io::di, io::uo) is det.

dump_clique_index_entry(Index, CliquePtr, !IO) :-
    CliquePtr = clique_ptr(CliqueNum),
    io.format("pd%d is in clique%d\n", [i(Index), i(CliqueNum)], !IO).

:- pred dump_clique_members(int::in, list(proc_dynamic_ptr)::in,
    io::di, io::uo) is det.

dump_clique_members(Index, CliqueMembers, !IO) :-
    io.format("clique%d members:", [i(Index)], !IO),
    list.foldl(dump_pd_in_clique, CliqueMembers, !IO),
    io.nl(!IO).

:- pred dump_pd_in_clique(proc_dynamic_ptr::in, io::di, io::uo) is det.

dump_pd_in_clique(PDPtr, !IO) :-
    PDPtr = proc_dynamic_ptr(PDNum),
    io.write_string(" pd", !IO),
    io.write_int(PDNum, !IO).

:- pred dump_clique_parent(int::in, call_site_dynamic_ptr::in,
    io::di, io::uo) is det.

dump_clique_parent(Index, CSDPtr, !IO) :-
    CSDPtr = call_site_dynamic_ptr(CSDNum),
    io.format("clique%d parent: csd%d\n", [i(Index), i(CSDNum)], !IO).

:- pred dump_clique_maybe_child(int::in, maybe(clique_ptr)::in,
    io::di, io::uo) is det.

dump_clique_maybe_child(Index, MaybeCliquePtr, !IO) :-
    (
        MaybeCliquePtr = no
    ;
        MaybeCliquePtr = yes(CliquePtr),
        CliquePtr = clique_ptr(CliqueNum),
        io.format("csd%d child: clique%d\n", [i(Index), i(CliqueNum)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred dump_deep_rev_links(deep::in, io::di, io::uo) is det.

dump_deep_rev_links(Deep, !IO) :-
    ProcCallers = Deep ^ proc_callers,
    io.write_string("SECTION MAP FROM PROC STATIC TO CALLER CSDs:\n\n", !IO),
    array_foldl_from_1(dump_proc_static_caller_csds, ProcCallers, !IO),
    io.nl(!IO),

    CallSiteStaticMap = Deep ^ call_site_static_map,
    io.write_string("SECTION MAP FROM CALL SITE DYNAMICS TO STATICS:\n\n",
        !IO),
    array_foldl_from_1(dump_call_site_dynamic_to_static, CallSiteStaticMap,
        !IO),
    io.nl(!IO),

    CallSiteCalls = Deep ^ call_site_calls,
    io.write_string("SECTION MAP FROM CALL SITE STATICS TO CALLS:\n\n", !IO),
    array_foldl_from_1(dump_call_site_calls, CallSiteCalls, !IO),
    io.nl(!IO).

:- pred dump_proc_static_caller_csds(int::in, list(call_site_dynamic_ptr)::in,
    io::di, io::uo) is det.

dump_proc_static_caller_csds(Index, CallerCSDs, !IO) :-
    (
        CallerCSDs = []
    ;
        CallerCSDs = [_ | _],
        io.format("ps%d callers:", [i(Index)], !IO),
        list.foldl(dump_space_csdptr, CallerCSDs, !IO),
        io.nl(!IO)
    ).

:- pred dump_space_csdptr(call_site_dynamic_ptr::in, io::di, io::uo) is det.

dump_space_csdptr(CSDPtr, !IO) :-
    CSDPtr = call_site_dynamic_ptr(CSDNum),
    io.write_string(" csd", !IO),
    io.write_int(CSDNum, !IO).

:- pred dump_call_site_dynamic_to_static(int::in, call_site_static_ptr::in,
    io::di, io::uo) is det.

dump_call_site_dynamic_to_static(Index, CSSPtr, !IO) :-
    CSSPtr = call_site_static_ptr(CSSNum),
    io.format("csd%d is at css%d\n", [i(Index), i(CSSNum)], !IO).

:- pred dump_call_site_calls(int::in,
    map(proc_static_ptr, list(call_site_dynamic_ptr))::in,
    io::di, io::uo) is det.

dump_call_site_calls(Index, CalleeMap, !IO) :-
    CalleeList = map.to_assoc_list(CalleeMap),
    (
        CalleeList = []
    ;
        CalleeList = [OneCallee],
        io.format("css%d calls one procedure: ", [i(Index)], !IO),
        dump_call_site_calls_to_proc("", OneCallee, !IO)
    ;
        CalleeList = [_, _ | _],
        io.format("css%d calls several procedures:\n", [i(Index)], !IO),
        list.foldl(dump_call_site_calls_to_proc("\t"), CalleeList, !IO)
    ).

:- pred dump_call_site_calls_to_proc(string::in,
    pair(proc_static_ptr, list(call_site_dynamic_ptr))::in,
    io::di, io::uo) is det.

dump_call_site_calls_to_proc(Prefix, PSPtr - CSDPtrs, !IO) :-
    PSPtr = proc_static_ptr(PSNum),
    io.format("%sps%d:", [s(Prefix), i(PSNum)], !IO),
    list.foldl(dump_space_csdptr, CSDPtrs, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred dump_deep_prop_measurements(deep::in, io::di, io::uo) is det.

dump_deep_prop_measurements(Deep, !IO) :-
    PDOwn = Deep ^ pd_own,
    PDDesc = Deep ^ pd_desc,
    PDOwnMax = array.max(PDOwn),
    PDDescMax = array.max(PDDesc),
    require(unify(PDOwnMax, PDDescMax),
        "dump_deep: PDOwnMax != PDDescMax"),
    io.write_string("SECTION PROC DYNAMIC MEASUREMENTS:\n\n", !IO),
    dump_pd_measurements(1, PDOwnMax, PDOwn, PDDesc, !IO),

    CSDs = Deep ^ call_site_dynamics,
    CSDDesc = Deep ^ csd_desc,
    CSDMax = array.max(CSDs),
    CSDDescMax = array.max(CSDDesc),
    require(unify(CSDMax, CSDDescMax),
        "dump_deep: CSDMax != CSDDescMax"),
    io.write_string("SECTION CALL SITE DYNAMIC MEASUREMENTS:\n\n", !IO),
    dump_csd_measurements(1, CSDMax, CSDs, CSDDesc, !IO),

    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    PSOwnMax = array.max(PSOwn),
    PSDescMax = array.max(PSDesc),
    require(unify(PSOwnMax, PSDescMax),
        "dump_deep: PSOwnMax != PSDescMax"),
    io.write_string("SECTION PROC STATIC MEASUREMENTS:\n\n", !IO),
    dump_ps_measurements(1, PSOwnMax, PSOwn, PSDesc, !IO),

    CSSOwn = Deep ^ css_own,
    CSSDesc = Deep ^ css_desc,
    CSSOwnMax = array.max(CSSOwn),
    CSSDescMax = array.max(CSSDesc),
    require(unify(CSSOwnMax, CSSDescMax),
        "dump_deep: CSSOwnMax != CSSDescMax"),
    io.write_string("SECTION CALL SITE STATIC MEASUREMENTS:\n\n", !IO),
    dump_css_measurements(1, CSSOwnMax, CSSOwn, CSSDesc, !IO).

:- pred dump_pd_measurements(int::in, int::in,
    array(own_prof_info)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_pd_measurements(Cur, Max, PDOwn, PDDesc, !IO) :-
    ( if Cur =< Max then
        array.lookup(PDOwn, Cur, Own),
        array.lookup(PDDesc, Cur, Desc),
        dump_own_and_desc("pd", Cur, Own, Desc, !IO),
        dump_pd_measurements(Cur + 1, Max, PDOwn, PDDesc, !IO)
    else
        true
    ).

:- pred dump_csd_measurements(int::in, int::in,
    array(call_site_dynamic)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_csd_measurements(Cur, Max, CSDs, CSDDesc, !IO) :-
    ( if Cur =< Max then
        array.lookup(CSDs, Cur, CSD),
        Own = CSD ^ csd_own_prof,
        array.lookup(CSDDesc, Cur, Desc),
        dump_own_and_desc("csd", Cur, Own, Desc, !IO),
        dump_csd_measurements(Cur + 1, Max, CSDs, CSDDesc, !IO)
    else
        true
    ).

:- pred dump_ps_measurements(int::in, int::in,
    array(own_prof_info)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_ps_measurements(Cur, Max, PSOwn, PSDesc, !IO) :-
    ( if Cur =< Max then
        array.lookup(PSOwn, Cur, Own),
        array.lookup(PSDesc, Cur, Desc),
        dump_own_and_desc("ps", Cur, Own, Desc, !IO),
        dump_ps_measurements(Cur + 1, Max, PSOwn, PSDesc, !IO)
    else
        true
    ).

:- pred dump_css_measurements(int::in, int::in,
    array(own_prof_info)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_css_measurements(Cur, Max, CSSOwn, CSSDesc, !IO) :-
    ( if Cur =< Max then
        array.lookup(CSSOwn, Cur, Own),
        array.lookup(CSSDesc, Cur, Desc),
        dump_own_and_desc("css", Cur, Own, Desc, !IO),
        dump_css_measurements(Cur + 1, Max, CSSOwn, CSSDesc, !IO)
    else
        true
    ).

:- pred dump_own_and_desc(string::in, int::in,
    own_prof_info::in, inherit_prof_info::in, io::di, io::uo) is det.

dump_own_and_desc(Prefix, Cur, Own, Desc, !IO) :-
    ( if is_zero_own_prof_info(Own) then
        PrintedOwn = no
    else
        io.format("%s%d own:\n", [s(Prefix), i(Cur)], !IO),
        dump_own_prof_info(Own, !IO),
        PrintedOwn = yes
    ),
    ( if is_zero_inherit_prof_info(Desc) then
        PrintedDesc = no
    else
        io.format("%s%d inherit:\n", [s(Prefix), i(Cur)], !IO),
        dump_inherit_prof_info(Desc, !IO),
        PrintedDesc = yes
    ),
    ( if
        ( PrintedOwn = yes
        ; PrintedDesc = yes
        )
    then
        io.nl(!IO)
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
