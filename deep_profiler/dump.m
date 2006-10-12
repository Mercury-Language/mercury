%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
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
:- import_module list.

%-----------------------------------------------------------------------------%

    % dump_initial_deep(InitialDeep, DumpOptions, !IO):
    %
    % Dump selected parts of InitialDeep to standard output. The array of call
    % site dynamics, proc dynamics, call site statics and proc statics is
    % dumped if DumpOptions contains "csd", "pd", "css" or "ps" respectively.
    % If it contains "restrict", then the only the elements of the static
    % arrays that will be dumped are the ones that are referred to from the
    % dynamic arrays. The statistics and the root node are dumped if
    % DumpOptions contains "stats".
    %
:- pred dump_initial_deep(initial_deep::in, list(string)::in, io::di, io::uo)
    is det.

    % dump_deep(Deep, DumpOptions, !IO):
    %
    % Dump selected parts of Deep to standard output. Information about cliques
    % is output if DumpOptions contains "clique". The fields of Deep that
    % contain reverse links are dumped if DumpOptions contains "rev".
    % The propagated costs are dumped if DumpOptions contains "prop".
    %
:- pred dump_deep(deep::in, list(string)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array_util.
:- import_module measurements.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svset.

%----------------------------------------------------------------------------%

dump_initial_deep(InitialDeep, DumpOptions, !IO) :-
    InitialDeep = initial_deep(Stats, InitRoot, CSDs, PDs, CSSs, PSs),
    ( should_dump(DumpOptions, "restrict") ->
        get_static_ptrs_from_dynamic_procs(PDs, PSs, UsedPSs, UsedCSSs),
        Restriction = these(UsedPSs, UsedCSSs)
    ;
        Restriction = none
    ),
    ( should_dump(DumpOptions, "stat") ->
        dump_init_profile_stats(Stats, !IO),
        dump_init_root(InitRoot, !IO)
    ;
        true
    ),
    ( should_dump(DumpOptions, "csd") ->
        dump_init_call_site_dynamics(CSDs, !IO)
    ;
        true
    ),
    ( should_dump(DumpOptions, "pd") ->
        dump_init_proc_dynamics(PDs, PSs, !IO)
    ;
        true
    ),
    ( should_dump(DumpOptions, "css") ->
        dump_init_call_site_statics(Restriction, CSSs, !IO)
    ;
        true
    ),
    ( should_dump(DumpOptions, "ps") ->
        dump_init_proc_statics(Restriction, PSs, !IO)
    ;
        true
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
        InstrumentQuanta, UserQuanta, NumCallSeqs, WordSize, Canonical),
    io.write_string("SECTION PROFILING STATS:\n\n", !IO),
    io.format("\tmax_csd = %d\n", [i(MaxCSD)], !IO),
    io.format("\tmax_css = %d\n", [i(MaxCSS)], !IO),
    io.format("\tmax_pd  = %d\n", [i(MaxPD)],  !IO),
    io.format("\tmax_ps  = %d\n", [i(MaxPS)],  !IO),
    io.format("\tticks_per_sec = %d\n", [i(TicksPerSec)], !IO),
    io.format("\tinstrument_quanta = %d\n", [i(InstrumentQuanta)], !IO),
    io.format("\tuser_quanta = %d\n", [i(UserQuanta)], !IO),
    io.format("\tnum_callseqs = %d\n", [i(NumCallSeqs)], !IO),
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
        Quanta, CallSeqs, Allocs, Words),
    ( Calls = 0 ->
        true
    ;
        io.format("\tcalls:\t\t%d\n", [i(Calls)], !IO)
    ),
    ( Exits = 0 ->
        true
    ;
        io.format("\texits:\t\t%d\n", [i(Exits)], !IO)
    ),
    ( Fails = 0 ->
        true
    ;
        io.format("\tfails:\t\t%d\n", [i(Fails)], !IO)
    ),
    ( Redos = 0 ->
        true
    ;
        io.format("\tredos:\t\t%d\n", [i(Redos)], !IO)
    ),
    ( Excps = 0 ->
        true
    ;
        io.format("\texcps:\t\t%d\n", [i(Excps)], !IO)
    ),
    ( Quanta = 0 ->
        true
    ;
        io.format("\tquanta:\t\t%d\n", [i(Quanta)], !IO)
    ),
    ( CallSeqs = 0 ->
        true
    ;
        io.format("\tcallseqs:\t%d\n", [i(CallSeqs)], !IO)
    ),
    ( Allocs = 0 ->
        true
    ;
        io.format("\tallocs:\t\t%d\n", [i(Allocs)], !IO)
    ),
    ( Words = 0 ->
        true
    ;
        io.format("\twords:\t\t%d\n", [i(Words)], !IO)
    ).

:- pred dump_inherit_prof_info(inherit_prof_info::in, io::di, io::uo) is det.

dump_inherit_prof_info(Inherit, !IO) :-
    Quanta = inherit_quanta(Inherit),
    CallSeqs = inherit_callseqs(Inherit),
    Allocs = inherit_allocs(Inherit),
    Words = inherit_words(Inherit),
    ( Quanta = 0 ->
        true
    ;
        io.format("\tquanta:\t\t%d\n", [i(Quanta)], !IO)
    ),
    ( CallSeqs = 0 ->
        true
    ;
        io.format("\tcallseqs:\t%d\n", [i(CallSeqs)], !IO)
    ),
    ( Allocs = 0 ->
        true
    ;
        io.format("\tallocs:\t\t%d\n", [i(Allocs)], !IO)
    ),
    ( Words = 0 ->
        true
    ;
        io.format("\twords:\t\t%d\n", [i(Words)], !IO)
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
    io.format("\tpd_proc_static = %d (%s)\n", [i(PSI), s(RefinedPSId)], !IO),
    array_foldl_from_0(dump_call_site_array_slot, Sites, !IO),
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

%----------------------------------------------------------------------------%

:- pred dump_init_call_site_statics(restriction::in, call_site_statics::in,
    io::di, io::uo) is det.

dump_init_call_site_statics(Restriction, CallStatics, !IO) :-
    io.write_string("SECTION CALL SITE STATICS:\n\n", !IO),
    array_foldl_from_1(dump_call_site_static(Restriction), CallStatics, !IO).

:- pred dump_call_site_static(restriction::in, int::in, call_site_static::in,
    io::di, io::uo) is det.

dump_call_site_static(Restriction, Index, CallSiteStatic, !IO) :-
    (
        (
            Restriction = none
        ;
            Restriction = these(_, UsedCallSiteStatics),
            set.member(call_site_static_ptr(Index), UsedCallSiteStatics)
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
            io.format("\tps_refined_id\t= %s\n", [s(RefinedId)], !IO)
        ),
        ( RefinedId \= RawId ->
            % The output is too big already; don't include
            % redundant information.
            true
        ;
            io.format("\tps_raw_id\t= %s\n", [s(RawId)], !IO)
        ),
        io.format("\tlocation\t= %s:%d\n", [s(FileName), i(LineNumber)], !IO),
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

%----------------------------------------------------------------------------%

dump_deep(Deep, DumpOptions, !IO) :-
    ( should_dump(DumpOptions, "clique") ->
        dump_deep_cliques(Deep, !IO)
    ;
        true
    ),
    ( should_dump(DumpOptions, "rev") ->
        dump_deep_rev_links(Deep, !IO)
    ;
        true
    ),
    ( should_dump(DumpOptions, "prop") ->
        dump_deep_prop_measurements(Deep, !IO)
    ;
        true
    ).

%----------------------------------------------------------------------------%

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

%----------------------------------------------------------------------------%

:- pred dump_deep_rev_links(deep::in, io::di, io::uo) is det.

dump_deep_rev_links(Deep, !IO) :-
    ProcCallers = Deep ^ proc_callers,
    io.write_string("SECTION MAP FROM PROC STATIC TO CALLER CSDs:\n\n", !IO),
    array_foldl_from_1(dump_proc_static_caller_csds, ProcCallers, !IO),
    io.nl(!IO),

    CallSiteStaticMap = Deep ^ call_site_static_map,
    io.write_string("SECTION MAP FROM CALL SITE DYNAMICS TO STATICS:\n\n", !IO),
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

%----------------------------------------------------------------------------%

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
    ( Cur =< Max ->
        array.lookup(PDOwn, Cur, Own),
        array.lookup(PDDesc, Cur, Desc),
        dump_own_and_desc("pd", Cur, Own, Desc, !IO),
        dump_pd_measurements(Cur + 1, Max, PDOwn, PDDesc, !IO)
    ;
        true
    ).

:- pred dump_csd_measurements(int::in, int::in,
    array(call_site_dynamic)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_csd_measurements(Cur, Max, CSDs, CSDDesc, !IO) :-
    ( Cur =< Max ->
        array.lookup(CSDs, Cur, CSD),
        Own = CSD ^ csd_own_prof,
        array.lookup(CSDDesc, Cur, Desc),
        dump_own_and_desc("csd", Cur, Own, Desc, !IO),
        dump_csd_measurements(Cur + 1, Max, CSDs, CSDDesc, !IO)
    ;
        true
    ).

:- pred dump_ps_measurements(int::in, int::in,
    array(own_prof_info)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_ps_measurements(Cur, Max, PSOwn, PSDesc, !IO) :-
    ( Cur =< Max ->
        array.lookup(PSOwn, Cur, Own),
        array.lookup(PSDesc, Cur, Desc),
        dump_own_and_desc("ps", Cur, Own, Desc, !IO),
        dump_ps_measurements(Cur + 1, Max, PSOwn, PSDesc, !IO)
    ;
        true
    ).

:- pred dump_css_measurements(int::in, int::in,
    array(own_prof_info)::in, array(inherit_prof_info)::in,
    io::di, io::uo) is det.

dump_css_measurements(Cur, Max, CSSOwn, CSSDesc, !IO) :-
    ( Cur =< Max ->
        array.lookup(CSSOwn, Cur, Own),
        array.lookup(CSSDesc, Cur, Desc),
        dump_own_and_desc("css", Cur, Own, Desc, !IO),
        dump_css_measurements(Cur + 1, Max, CSSOwn, CSSDesc, !IO)
    ;
        true
    ).

:- pred dump_own_and_desc(string::in, int::in,
    own_prof_info::in, inherit_prof_info::in, io::di, io::uo) is det.

dump_own_and_desc(Prefix, Cur, Own, Desc, !IO) :-
    ( is_zero_own_prof_info(Own) ->
        PrintedOwn = no
    ;
        io.format("%s%d own:\n", [s(Prefix), i(Cur)], !IO),
        dump_own_prof_info(Own, !IO),
        PrintedOwn = yes
    ),
    ( is_zero_inherit_prof_info(Desc) ->
        PrintedDesc = no
    ;
        io.format("%s%d inherit:\n", [s(Prefix), i(Cur)], !IO),
        dump_inherit_prof_info(Desc, !IO),
        PrintedDesc = yes
    ),
    (
        ( PrintedOwn = yes
        ; PrintedDesc = yes
        )
    ->
        io.nl(!IO)
    ;
        true
    ).

%----------------------------------------------------------------------------%

:- pred should_dump(list(string)::in, string::in) is semidet.

should_dump(DumpOptions, What) :-
    ( list.member(What, DumpOptions)
    ; DumpOptions = []
    ).

%----------------------------------------------------------------------------%
:- end_module dump.
%----------------------------------------------------------------------------%
