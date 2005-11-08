%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: juliensf, zs.
%
% This module contains code for dumping out the deep profiler's data structures
% for use in debugging the deep profiler.

:- module dump.

:- interface.

:- import_module profile.

:- import_module io.
:- import_module bool.

%-----------------------------------------------------------------------------%

    % dump_initial_deep(ProfStats, Restrict, DumpCSDs, DumpPDs,
    %   DumpCSSs, DumpPSs, InitialDeep, !IO):
    % Dump selected parts of InitialDeep to standard output. The array of call
    % site dynamics, proc dynamics, call site statics and proc statics is
    % dumped if the corresponding Dump bool is "yes". If Restrict is "yes",
    % then the only the elements of the static arrays that will be dumped are
    % the ones that are referred to from the dynamic arrays. The statistics
    % and the root node are dumped if ProfStats is "yes".
    %
:- pred dump_initial_deep(bool::in, bool::in, bool::in, bool::in, bool::in,
    bool::in, initial_deep::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array_util.
:- import_module measurements.

:- import_module array.
:- import_module list.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svset.

%----------------------------------------------------------------------------%

dump_initial_deep(ProfStats, Restrict, DumpCSDs, DumpPDs, DumpCSSs, DumpPSs,
        InitialDeep, !IO) :-
    InitialDeep = initial_deep(Stats, InitRoot, CSDs, PDs, CSSs, PSs),
    (
        Restrict = yes,
        get_static_ptrs_from_dynamic_procs(PDs, PSs, UsedPSs, UsedCSSs),
        Restriction = these(UsedPSs, UsedCSSs)
    ;
        Restrict = no,
        Restriction = none
    ),
    ( 
        ProfStats = yes,
        dump_init_profile_stats(Stats, !IO),
        dump_init_root(InitRoot, !IO)
    ;
        ProfStats = no
    ),
    (
        DumpCSDs= yes,
        dump_init_call_site_dynamics(CSDs, !IO)
    ;
        DumpCSDs = no
    ),
    (
        DumpPDs = yes,
        dump_init_proc_dynamics(PDs, PSs, !IO)
    ;
        DumpPDs = no
    ),
    (
        DumpCSSs = yes,
        dump_init_call_site_statics(Restriction, CSSs, !IO)
    ;
        DumpCSSs = no
    ),
    (
        DumpPSs = yes,
        dump_init_proc_statics(Restriction, PSs, !IO)
    ;
        DumpPSs = no
    ).
    
%----------------------------------------------------------------------------%
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
    ProcDynamic = proc_dynamic(ProcStaticPtr, _PDSites),
    svset.insert(ProcStaticPtr, !PS_Ptrs),
    lookup_proc_statics(ProcStatics, ProcStaticPtr, ProcStatic),
    CSSs = array.to_list(ProcStatic ^ ps_sites),
    svset.insert_list(CSSs, !CSS_Ptrs).     

%----------------------------------------------------------------------------%
%
% Code for dumping profile_stats
%

:- pred dump_init_profile_stats(profile_stats::in, io::di, io::uo) is det.

dump_init_profile_stats(Stats, !IO) :-
    Stats = profile_stats(MaxCSD, MaxCSS, MaxPD, MaxPS, TicksPerSec,
        InstrumentQuanta, UserQuanta, WordSize, Canonical),
    io.write_string("SECTION PROFILING STATS:\n\n", !IO),
    io.format("\tmax_csd = %d\n", [i(MaxCSD)], !IO),
    io.format("\tmax_css = %d\n", [i(MaxCSS)], !IO),
    io.format("\tmax_pd  = %d\n", [i(MaxPD)],  !IO),
    io.format("\tmax_ps  = %d\n", [i(MaxPS)],  !IO),
    io.format("\tticks_per_sec = %d\n", [i(TicksPerSec)], !IO),
    io.format("\tinstrument_quanta = %d\n", [i(InstrumentQuanta)], !IO),
    io.format("\tuser_quanta = %d\n", [i(UserQuanta)], !IO),
    io.format("\tword_size   = %d\n", [i(WordSize)], !IO),
    io.write_string("\tcanonical = ", !IO),
    (
        Canonical = yes,
        io.write_string("yes\n", !IO)
    ;
        Canonical = no,
        io.write_string("no\n", !IO)
    ),
    io.nl(!IO).

%----------------------------------------------------------------------------%

:- pred dump_init_root(proc_dynamic_ptr::in, io::di, io::uo) is det.

dump_init_root(proc_dynamic_ptr(Root), !IO) :-
    io.write_string("INITIAL ROOT:\n", !IO),
    io.format("\tinitial root = %d\n", [i(Root)], !IO),
    io.nl(!IO). 

%----------------------------------------------------------------------------%

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
        Quanta, Allocs, Words),
    ( Calls = 0 ->
        true
    ;
        io.format("\tcalls:\t%d\n", [i(Calls)], !IO)
    ),
    ( Exits = 0 ->
        true
    ;
        io.format("\texits:\t%d\n", [i(Exits)], !IO)
    ),
    ( Fails = 0 ->
        true
    ;
        io.format("\tfails:\t%d\n", [i(Fails)], !IO)
    ),
    ( Redos = 0 ->
        true
    ;
        io.format("\tredos:\t%d\n", [i(Redos)], !IO)
    ),
    ( Excps = 0 ->
        true
    ;
        io.format("\texcps:\t%d\n", [i(Excps)], !IO)
    ),
    ( Quanta = 0 ->
        true
    ;
        io.format("\tquanta:\t%d\n", [i(Quanta)], !IO)
    ),
    ( Allocs = 0 ->
        true
    ;
        io.format("\tallocs:\t%d\n", [i(Allocs)], !IO)
    ),
    ( Words = 0 ->
        true
    ;
        io.format("\twords:\t%d\n", [i(Words)], !IO)
    ).

%----------------------------------------------------------------------------%
    
:- pred dump_init_proc_dynamics(proc_dynamics::in, proc_statics::in,
    io::di, io::uo) is det.

dump_init_proc_dynamics(ProcDynamics, ProcStatics, !IO) :-
    io.write_string("SECTION PROC DYNAMICS:\n\n", !IO),
    array_foldl_from_1(dump_proc_dynamic(ProcStatics), ProcDynamics, !IO).

:- pred dump_proc_dynamic(proc_statics::in, int::in, proc_dynamic::in,
    io::di, io::uo) is det.

dump_proc_dynamic(ProcStatics, Index, ProcDynamic, !IO) :-
    ProcDynamic = proc_dynamic(PSPtr, Sites),
    PSPtr = proc_static_ptr(PSI),   
    lookup_proc_statics(ProcStatics, PSPtr, PS),
    ( PS ^ ps_refined_id = "" ->
        RefinedPSId = "UNKNOWN_PS"
    ;
        RefinedPSId = PS ^ ps_refined_id
    ),
    io.format("pd%d:\n", [i(Index)], !IO),
    io.format("\tpd_proc_static = %d (%s)\n", [i(PSI), s(RefinedPSId)],
        !IO),
    array_foldl_from_0(dump_call_site_array_slot, Sites, !IO),
    io.nl(!IO). 

:- pred dump_call_site_array_slot(int::in, call_site_array_slot::in,
    io::di, io::uo) is det.

dump_call_site_array_slot(Index, CSA_slot, !IO) :-
    io.format("\tpd_site[%d] = %s\n",
        [i(Index), s(call_site_array_slot_to_string(CSA_slot))], !IO).

:- func call_site_array_slot_to_string(call_site_array_slot) = string.

call_site_array_slot_to_string(normal(call_site_dynamic_ptr(CSDI))) = 
    string.format("normal(csd%d)", [i(CSDI)]).
call_site_array_slot_to_string(multi(_, _)) = "multi".
    
%----------------------------------------------------------------------------%

:- pred dump_init_call_site_statics(restriction::in, call_site_statics::in,
    io::di, io::uo) is det.

dump_init_call_site_statics(Restriction, CallStatics, !IO) :-
    io.write_string("SECTION CALL SITE STATICS:\n\n", !IO),
    array_foldl_from_1(dump_call_site_static(Restriction), CallStatics,
        !IO).

:- pred dump_call_site_static(restriction::in, int::in, call_site_static::in,
    io::di, io::uo) is det.

dump_call_site_static(Restriction, Index, CallSiteStatic, !IO) :-
    (
        ( 
            Restriction = none
        ;
            Restriction = these(_, UsedCallSiteStatics),
            set.member(call_site_static_ptr(Index),
                UsedCallSiteStatics)
        )
    ->
        CallSiteStatic = call_site_static(ContainerPSPtr, SlotNum,
            Kind, LineNum, GoalPath),
        ContainerPSPtr = proc_static_ptr(ContainerPSI),
        io.format("css%d:\n", [i(Index)], !IO),
        io.format("\tcss_container\t= ps%d\n", [i(ContainerPSI)], !IO),
        io.format("\tcss_slot_num\t= <%d>\n", [i(SlotNum)], !IO),
        io.format("\tcss_line_num\t= <%d>\n", [i(LineNum)], !IO),
        io.format("\tcss_goal_path\t= <%s>\n", [s(GoalPath)], !IO),
        io.write_string("\tcss_kind\t= ", !IO),
        dump_call_site_kind_and_callee(Kind, !IO),
        io.nl(!IO),
        io.nl(!IO)
    ;
        true
    ).

%----------------------------------------------------------------------------%

:- pred dump_init_proc_statics(restriction::in, proc_statics::in,
    io::di, io::uo) is det.

dump_init_proc_statics(Restriction, ProcStatics, !IO) :-
    io.write_string("SECTION PROC STATICS:\n\n", !IO),
    array_foldl_from_1(dump_proc_static(Restriction), ProcStatics, !IO),
    io.nl(!IO).

:- pred dump_proc_static(restriction::in, int::in, proc_static::in,
    io::di, io::uo) is det.

dump_proc_static(Restriction, Index, ProcStatic, !IO) :-
    ( 
        (
            Restriction = none
        ;
            Restriction = these(UsedProcStatics, _),
            set.member(proc_static_ptr(Index), UsedProcStatics)
        )
    ->   
        ProcStatic = proc_static(Id, DeclModule, RefinedId, RawId,
            FileName, LineNumber, InInterface, Sites, IsZeroed),
        IdStr = dump_proc_id(Id),
        io.format("ps%d:\n", [i(Index)], !IO),
        io.format("\tps_id\t\t= %s", [s(IdStr)], !IO),
        io.nl(!IO),
        io.format("\tps_decl_module\t= %s\n", [s(DeclModule)], !IO),
        ( RefinedId = string.append_list([DeclModule, ":", IdStr]) ->
            % The output is too big already; don't include
            % redundant information.
            true
        ;
            io.format("\tps_refined_id\t= %s\n",
                [s(RefinedId)], !IO)
        ),
        ( RefinedId \= RawId ->
            % The output is too big already; don't include
            % redundant information.
            true
        ;
            io.format("\tps_raw_id\t= %s\n", [s(RawId)], !IO)
        ),
        io.format("\tlocation\t= %s:%d\n",
            [s(FileName), i(LineNumber)], !IO),
        (
            InInterface = yes,
            io.write_string("\tin_interface\n", !IO)
        ;
            InInterface = no
        ),
        IsZeroStr = (IsZeroed = zeroed -> "zeroed" ; "not_zeroed" ),
        io.format("\t%s\n", [s(IsZeroStr)], !IO),
        array_foldl_from_0(dump_proc_static_call_sites, Sites, !IO),
        io.nl(!IO)
    ;
        true
    ).

:- pred dump_proc_static_call_sites(int::in, call_site_static_ptr::in,
    io::di, io::uo) is det.

dump_proc_static_call_sites(Slot, CSSPtr, !IO) :-
    CSSPtr = call_site_static_ptr(CSSI),
    io.format("\tps_site[%d]: css%d\n", [i(Slot), i(CSSI)], !IO).

%----------------------------------------------------------------------------%

:- func dump_proc_id(proc_id) = string.

dump_proc_id(Proc) = Str :-
    Proc = user_defined(PredOrFunc, _DeclModule, _DefnModule, Name,
        Arity, Mode),
    (
        PredOrFunc = predicate,
        Suffix = ""
    ;   
        PredOrFunc = function,
        Suffix = "+1"
    ),
    string.format("%s/%d-%d%s", [s(Name), i(Arity), i(Mode), s(Suffix)],
        Str).
dump_proc_id(Proc) = Str :-
    Proc = uci_pred(Type, _TypeModule, _DefModule, Name, _Arity, _Mode),
    string.format("%s predicate for type `%s'", [s(Name), s(Type)], Str).

%----------------------------------------------------------------------------%

:- pred dump_call_site_kind_and_callee(call_site_kind_and_callee::in,
    io::di, io::uo) is det.

dump_call_site_kind_and_callee(normal_call(Ptr, String), !IO) :-
    Ptr = proc_static_ptr(Val),
    io.format("normal_call(%d, \"%s\")", [i(Val), s(String)], !IO).
dump_call_site_kind_and_callee(special_call, !IO) :-
    io.write_string("special_call", !IO).
dump_call_site_kind_and_callee(higher_order_call, !IO) :-
    io.write_string("higher_order_call", !IO).
dump_call_site_kind_and_callee(method_call, !IO) :-
    io.write_string("method_call", !IO).
dump_call_site_kind_and_callee(callback, !IO) :-
    io.write_string("callback", !IO).

%----------------------------------------------------------------------------%
:- end_module dump.
%----------------------------------------------------------------------------%
