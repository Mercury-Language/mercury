%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module contains the code for turning the raw list of nodes read in by
% read_profile.m into the data structure that server.m needs to service
% requests for web pages. The algorithm it implements is documented in the
% deep profiling paper.

:- module startup.

:- interface.

:- import_module profile.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module std_util.

:- pred read_and_startup(string::in, list(string)::in, bool::in,
	maybe(io__output_stream)::in, maybe_error(deep)::out,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array_util.
:- import_module callgraph.
:- import_module canonical.
:- import_module measurements.
:- import_module profile.
:- import_module read_profile.

:- import_module array.
:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

% :- import_module unsafe.

read_and_startup(Machine, DataFileNames, Canonical, MaybeOutputStream, Res,
		!IO) :-
	(
		DataFileNames = [],
		% This should have been caught and reported by main.
		error("read_and_startup: no data files")
	;
		DataFileNames = [DataFileName],
		maybe_report_stats(MaybeOutputStream, !IO),
		maybe_report_msg(MaybeOutputStream,
			"  Reading graph data...\n", !IO),
		read_call_graph(DataFileName, Res0, !IO),
		maybe_report_msg(MaybeOutputStream,
			"  Done.\n", !IO),
		maybe_report_stats(MaybeOutputStream, !IO),
		(
			Res0 = ok(InitDeep),
			startup(Machine, DataFileName, Canonical,
				MaybeOutputStream, InitDeep, Deep, !IO),
			Res = ok(Deep)
		;
			Res0 = error(Error),
			Res = error(Error)
		)
	;
		DataFileNames = [_, _ | _],
		error("mdprof_server: merging of data files " ++
			"is not yet implemented")
	).

:- pred startup(string::in, string::in, bool::in, maybe(io__output_stream)::in,
	initial_deep::in, deep::out, io::di, io::uo) is det.

startup(Machine, DataFileName, Canonical, MaybeOutputStream, InitDeep0, Deep,
		!IO) :-
	InitDeep0 = initial_deep(InitStats, Root,
		CallSiteDynamics0, ProcDynamics,
		CallSiteStatics0, ProcStatics0),

	maybe_report_msg(MaybeOutputStream,
		"  Mapping static call sites to containing procedures...\n",
		!IO),
	array_foldl2_from_1(record_css_containers_module_procs, ProcStatics0,
		u(CallSiteStatics0), CallSiteStatics,
		map__init, ModuleProcs),
	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO),

	maybe_report_msg(MaybeOutputStream,
		"  Mapping dynamic call sites to containing procedures...\n",
		!IO),
	array_foldl2_from_1(record_csd_containers_zeroed_pss, ProcDynamics,
		u(CallSiteDynamics0), CallSiteDynamics,
		u(ProcStatics0), ProcStatics),
	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO),

	InitDeep1 = initial_deep(InitStats, Root,
		CallSiteDynamics, ProcDynamics,
		CallSiteStatics, ProcStatics),

	(
		Canonical = no,
		InitDeep = InitDeep1
	;
		Canonical = yes,
		maybe_report_msg(MaybeOutputStream,
			"  Canonicalizing cliques...\n", !IO),
		canonicalize_cliques(InitDeep1, InitDeep),
		maybe_report_msg(MaybeOutputStream,
			"  Done.\n", !IO),
		maybe_report_stats(MaybeOutputStream, !IO)
	),

	array__max(InitDeep ^ init_proc_dynamics, PDMax),
	NPDs = PDMax + 1,
	array__max(InitDeep ^ init_call_site_dynamics, CSDMax),
	NCSDs = CSDMax + 1,
	array__max(InitDeep ^ init_proc_statics, PSMax),
	NPSs = PSMax + 1,
	array__max(InitDeep ^ init_call_site_statics, CSSMax),
	NCSSs = CSSMax + 1,

	maybe_report_msg(MaybeOutputStream,
		"  Finding cliques...\n", !IO),
	find_cliques(InitDeep, CliqueList),
	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO),

	maybe_report_msg(MaybeOutputStream,
		"  Constructing clique indexes...\n", !IO),
	make_clique_indexes(NPDs, CliqueList, Cliques, CliqueIndex),
	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO),

	maybe_report_msg(MaybeOutputStream,
		"  Constructing clique parent map...\n", !IO),

		% For each CallSiteDynamic pointer, if it points to
		% a ProcDynamic which is in a different clique to
		% the one from which the CallSiteDynamic's parent
		% came, then this CallSiteDynamic is the entry to
		% the [lower] clique. We need to compute this information
		% so that we can print clique-based timing summaries in
		% the browser.
	array__max(Cliques, CliqueMax),
	NCliques = CliqueMax + 1,
	array__init(NCliques, call_site_dynamic_ptr(-1), CliqueParents0),
	array__init(NCSDs, no, CliqueMaybeChildren0),
	array_foldl2_from_1(construct_clique_parents(InitDeep, CliqueIndex),
		CliqueIndex,
		CliqueParents0, CliqueParents,
		CliqueMaybeChildren0, CliqueMaybeChildren),

	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO),

	maybe_report_msg(MaybeOutputStream,
		"  Finding procedure callers...\n", !IO),
	array__init(NPSs, [], ProcCallers0),
	array_foldl_from_1(construct_proc_callers(InitDeep),
		CallSiteDynamics, ProcCallers0, ProcCallers),
	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO),

	maybe_report_msg(MaybeOutputStream,
		"  Constructing call site static map...\n", !IO),
	array__init(NCSDs, call_site_static_ptr(-1), CallSiteStaticMap0),
	array_foldl_from_1(construct_call_site_caller(InitDeep),
		ProcDynamics, CallSiteStaticMap0, CallSiteStaticMap),
	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO),

	maybe_report_msg(MaybeOutputStream,
		"  Finding call site calls...\n", !IO),
	array__init(NCSSs, map__init, CallSiteCalls0),
	array_foldl_from_1(construct_call_site_calls(InitDeep),
		ProcDynamics, CallSiteCalls0, CallSiteCalls),
	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO),

	maybe_report_msg(MaybeOutputStream,
		"  Propagating time up call graph...\n", !IO),

	array__init(NCSDs, zero_inherit_prof_info, CSDDesc0),
	array__init(NPDs, zero_own_prof_info, PDOwn0),
	array_foldl_from_1(sum_call_sites_in_proc_dynamic,
		CallSiteDynamics, PDOwn0, PDOwn),
	array__init(NPDs, zero_inherit_prof_info, PDDesc0),
	array__init(NPSs, zero_own_prof_info, PSOwn0),
	array__init(NPSs, zero_inherit_prof_info, PSDesc0),
	array__init(NCSSs, zero_own_prof_info, CSSOwn0),
	array__init(NCSSs, zero_inherit_prof_info, CSSDesc0),
	array__init(NPDs, map__init, PDCompTable0),
	array__init(NCSDs, map__init, CSDCompTable0),

	ModuleData = map__map_values(initialize_module_data, ModuleProcs),
	Deep0 = deep(InitStats, Machine, DataFileName, Root,
		CallSiteDynamics, ProcDynamics, CallSiteStatics, ProcStatics,
		CliqueIndex, Cliques, CliqueParents, CliqueMaybeChildren,
		ProcCallers, CallSiteStaticMap, CallSiteCalls,
		PDOwn, PDDesc0, CSDDesc0,
		PSOwn0, PSDesc0, CSSOwn0, CSSDesc0,
		PDCompTable0, CSDCompTable0, ModuleData),

	array_foldl_from_1(propagate_to_clique, Cliques, Deep0, Deep1),
	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO),

	maybe_report_msg(MaybeOutputStream,
		"  Summarizing information...\n", !IO),
	summarize_proc_dynamics(Deep1, Deep2),
	summarize_call_site_dynamics(Deep2, Deep3),
	summarize_modules(Deep3, Deep),
	maybe_report_msg(MaybeOutputStream,
		"  Done.\n", !IO),
	maybe_report_stats(MaybeOutputStream, !IO).

:- pred count_quanta(int::in, call_site_dynamic::in, int::in, int::out) is det.

count_quanta(_N, CSD, Quanta0, Quanta) :-
	Quanta = Quanta0 + quanta(CSD ^ csd_own_prof).

:- func initialize_module_data(string, list(proc_static_ptr)) = module_data.

initialize_module_data(_ModuleName, PSPtrs) =
	module_data(zero_own_prof_info, zero_inherit_prof_info, PSPtrs).

%-----------------------------------------------------------------------------%

:- pred record_css_containers_module_procs(int::in, proc_static::in,
	array(call_site_static)::array_di,
	array(call_site_static)::array_uo,
	map(string, list(proc_static_ptr))::in,
	map(string, list(proc_static_ptr))::out) is det.

record_css_containers_module_procs(PSI, PS, CallSiteStatics0, CallSiteStatics,
		ModuleProcs0, ModuleProcs) :-
	CSSPtrs = PS ^ ps_sites,
	PSPtr = proc_static_ptr(PSI),
	array__max(CSSPtrs, MaxCS),
	record_css_containers_2(MaxCS, PSPtr, CSSPtrs,
		CallSiteStatics0, CallSiteStatics),
	DeclModule = PS ^ ps_decl_module,
	( map__search(ModuleProcs0, DeclModule, PSPtrs0) ->
		map__det_update(ModuleProcs0, DeclModule, [PSPtr | PSPtrs0],
			ModuleProcs)
	;
		map__det_insert(ModuleProcs0, DeclModule, [PSPtr],
			ModuleProcs)
	).

:- pred record_css_containers_2(int::in, proc_static_ptr::in,
	array(call_site_static_ptr)::in,
	array(call_site_static)::array_di,
	array(call_site_static)::array_uo) is det.

record_css_containers_2(SlotNum, PSPtr, CSSPtrs,
		CallSiteStatics0, CallSiteStatics) :-
	( SlotNum >= 0 ->
		array__lookup(CSSPtrs, SlotNum, CSSPtr),
		lookup_call_site_statics(CallSiteStatics0, CSSPtr, CSS0),
		CSS0 = call_site_static(PSPtr0, SlotNum0,
			Kind, LineNumber, GoalPath),
		require(unify(PSPtr0, proc_static_ptr(-1)),
			"record_css_containers_2: real proc_static_ptr"),
		require(unify(SlotNum0, -1),
			"record_css_containers_2: real slot_num"),
		CSS = call_site_static(PSPtr, SlotNum,
			Kind, LineNumber, GoalPath),
		update_call_site_statics(CallSiteStatics0, CSSPtr, CSS,
			CallSiteStatics1),
		record_css_containers_2(SlotNum - 1,
			PSPtr, CSSPtrs, CallSiteStatics1, CallSiteStatics)
	;
		CallSiteStatics = CallSiteStatics0
	).

%-----------------------------------------------------------------------------%

:- pred record_csd_containers_zeroed_pss(int::in, proc_dynamic::in,
	array(call_site_dynamic)::array_di,
	array(call_site_dynamic)::array_uo,
	array(proc_static)::array_di, array(proc_static)::array_uo) is det.

record_csd_containers_zeroed_pss(PDI, PD, CallSiteDynamics0, CallSiteDynamics,
		ProcStatics0, ProcStatics) :-
	CSDArray = PD ^ pd_sites,
	PDPtr = proc_dynamic_ptr(PDI),
	flatten_call_sites(CSDArray, CSDPtrs, IsZeroed),
	record_csd_containers_2(PDPtr, CSDPtrs,
		CallSiteDynamics0, CallSiteDynamics),
	(
		IsZeroed = zeroed,
		PSPtr = PD ^ pd_proc_static,
		lookup_proc_statics(ProcStatics0, PSPtr, PS0),
		PS = PS0 ^ ps_is_zeroed := zeroed,
		update_proc_statics(ProcStatics0, PSPtr, PS, ProcStatics)
	;
		IsZeroed = not_zeroed,
		ProcStatics = ProcStatics0
	).

:- pred record_csd_containers_2(proc_dynamic_ptr::in,
	list(call_site_dynamic_ptr)::in,
	array(call_site_dynamic)::array_di,
	array(call_site_dynamic)::array_uo) is det.

record_csd_containers_2(_, [], CallSiteDynamics, CallSiteDynamics).
record_csd_containers_2(PDPtr, [CSDPtr | CSDPtrs],
		CallSiteDynamics0, CallSiteDynamics) :-
	lookup_call_site_dynamics(CallSiteDynamics0, CSDPtr, CSD0),
	CSD0 = call_site_dynamic(CallerPDPtr0, CalleePDPtr, Own),
	require(unify(CallerPDPtr0, proc_dynamic_ptr(-1)),
		"record_csd_containers_2: real proc_dynamic_ptr"),
	CSD = call_site_dynamic(PDPtr, CalleePDPtr, Own),
	update_call_site_dynamics(CallSiteDynamics0, CSDPtr, CSD,
		CallSiteDynamics1),
	record_csd_containers_2(PDPtr, CSDPtrs,
		CallSiteDynamics1, CallSiteDynamics).

%-----------------------------------------------------------------------------%

:- pred construct_clique_parents(initial_deep::in, array(clique_ptr)::in,
	int::in, clique_ptr::in,
	array(call_site_dynamic_ptr)::array_di,
	array(call_site_dynamic_ptr)::array_uo,
	array(maybe(clique_ptr))::array_di,
	array(maybe(clique_ptr))::array_uo) is det.

construct_clique_parents(InitDeep, CliqueIndex, PDI, CliquePtr,
		CliqueParents0, CliqueParents,
		CliqueMaybeChildren0, CliqueMaybeChildren) :-
	( PDI > 0 ->
		flat_call_sites(InitDeep ^ init_proc_dynamics,
			proc_dynamic_ptr(PDI), CSDPtrs),
		array_list_foldl2(
			construct_clique_parents_2(InitDeep,
				CliqueIndex, CliquePtr),
			CSDPtrs, CliqueParents0, CliqueParents,
			CliqueMaybeChildren0, CliqueMaybeChildren)
	;
		error("emit nasal daemons")
	).

:- pred construct_clique_parents_2(initial_deep::in, array(clique_ptr)::in,
	clique_ptr::in, call_site_dynamic_ptr::in,
	array(call_site_dynamic_ptr)::array_di,
	array(call_site_dynamic_ptr)::array_uo,
	array(maybe(clique_ptr))::array_di,
	array(maybe(clique_ptr))::array_uo) is det.

% :- pragma promise_pure(construct_clique_parents_2/8).

construct_clique_parents_2(InitDeep, CliqueIndex, ParentCliquePtr, CSDPtr,
		CliqueParents0, CliqueParents,
		CliqueMaybeChildren0, CliqueMaybeChildren) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	( CSDI > 0 ->
		array__lookup(InitDeep ^ init_call_site_dynamics, CSDI, CSD),
		ChildPDPtr = CSD ^ csd_callee,
		ChildPDPtr = proc_dynamic_ptr(ChildPDI),
		( ChildPDI > 0 ->
			array__lookup(CliqueIndex, ChildPDI, ChildCliquePtr),
			( ChildCliquePtr \= ParentCliquePtr ->
				ChildCliquePtr = clique_ptr(ChildCliqueNum),
				% impure unsafe_perform_io(
				% 	write_pdi_cn_csd(ChildPDI,
				% 		ChildCliqueNum, CSDI)),
				array__set(CliqueParents0, ChildCliqueNum,
					CSDPtr, CliqueParents),
				array__set(CliqueMaybeChildren0, CSDI,
					yes(ChildCliquePtr),
					CliqueMaybeChildren)
			;
				CliqueParents = CliqueParents0,
				CliqueMaybeChildren = CliqueMaybeChildren0
			)
		;
			CliqueParents = CliqueParents0,
			CliqueMaybeChildren = CliqueMaybeChildren0
		)
	;
		CliqueParents = CliqueParents0,
		CliqueMaybeChildren = CliqueMaybeChildren0
	).

%-----------------------------------------------------------------------------%

:- pred construct_proc_callers(initial_deep::in, int::in,
	call_site_dynamic::in,
	array(list(call_site_dynamic_ptr))::array_di,
	array(list(call_site_dynamic_ptr))::array_uo) is det.

construct_proc_callers(InitDeep, CSDI, CSD, ProcCallers0, ProcCallers) :-
	PDPtr = CSD ^ csd_callee,
	( valid_proc_dynamic_ptr_raw(InitDeep ^ init_proc_dynamics, PDPtr) ->
		lookup_proc_dynamics(InitDeep ^ init_proc_dynamics, PDPtr, PD),
		PSPtr = PD ^ pd_proc_static,
		lookup_proc_callers(ProcCallers0, PSPtr, Callers0),
		Callers = [call_site_dynamic_ptr(CSDI) | Callers0],
		update_proc_callers(ProcCallers0, PSPtr, Callers, ProcCallers)
	;
		ProcCallers = ProcCallers0
	).

:- pred construct_call_site_caller(initial_deep::in, int::in, proc_dynamic::in,
	array(call_site_static_ptr)::array_di,
	array(call_site_static_ptr)::array_uo) is det.

construct_call_site_caller(InitDeep, _PDI, PD,
		CallSiteStaticMap0, CallSiteStaticMap) :-
	PSPtr = PD ^ pd_proc_static,
	CSDArraySlots = PD ^ pd_sites,
	lookup_proc_statics(InitDeep ^ init_proc_statics, PSPtr, PS),
	CSSPtrs = PS ^ ps_sites,
	array__max(CSDArraySlots, MaxCS),
	construct_call_site_caller_2(MaxCS,
		InitDeep ^ init_call_site_dynamics, CSSPtrs, CSDArraySlots,
		CallSiteStaticMap0, CallSiteStaticMap).

:- pred construct_call_site_caller_2(int::in, call_site_dynamics::in,
	array(call_site_static_ptr)::in,
	array(call_site_array_slot)::in,
	array(call_site_static_ptr)::array_di,
	array(call_site_static_ptr)::array_uo) is det.

construct_call_site_caller_2(SlotNum, Deep, CSSPtrs, CSDArraySlots,
		CallSiteStaticMap0, CallSiteStaticMap) :-
	( SlotNum >= 0 ->
		array__lookup(CSDArraySlots, SlotNum, CSDArraySlot),
		array__lookup(CSSPtrs, SlotNum, CSSPtr),
		(
			CSDArraySlot = normal(CSDPtr),
			construct_call_site_caller_3(Deep, CSSPtr, -1, CSDPtr,
				CallSiteStaticMap0, CallSiteStaticMap1)

		;
			CSDArraySlot = multi(_, CSDPtrs),
			array_foldl_from_0(
				construct_call_site_caller_3(Deep, CSSPtr),
				CSDPtrs,
				CallSiteStaticMap0, CallSiteStaticMap1)
		),
		construct_call_site_caller_2(SlotNum - 1, Deep, CSSPtrs,
			CSDArraySlots, CallSiteStaticMap1, CallSiteStaticMap)
	;
		CallSiteStaticMap = CallSiteStaticMap0
	).

:- pred construct_call_site_caller_3(call_site_dynamics::in,
	call_site_static_ptr::in, int::in, call_site_dynamic_ptr::in,
	array(call_site_static_ptr)::array_di,
	array(call_site_static_ptr)::array_uo) is det.

construct_call_site_caller_3(CallSiteDynamics, CSSPtr, _Dummy, CSDPtr,
		CallSiteStaticMap0, CallSiteStaticMap) :-
	( valid_call_site_dynamic_ptr_raw(CallSiteDynamics, CSDPtr) ->
		update_call_site_static_map(CallSiteStaticMap0,
			CSDPtr, CSSPtr, CallSiteStaticMap)
	;
		CallSiteStaticMap = CallSiteStaticMap0
	).

%-----------------------------------------------------------------------------%

:- pred construct_call_site_calls(initial_deep::in, int::in, proc_dynamic::in,
	array(map(proc_static_ptr, list(call_site_dynamic_ptr)))::array_di,
	array(map(proc_static_ptr, list(call_site_dynamic_ptr)))::array_uo)
	is det.

construct_call_site_calls(InitDeep, _PDI, PD, CallSiteCalls0, CallSiteCalls) :-
	PSPtr = PD ^ pd_proc_static,
	CSDArraySlots = PD ^ pd_sites,
	array__max(CSDArraySlots, MaxCS),
	PSPtr = proc_static_ptr(PSI),
	array__lookup(InitDeep ^ init_proc_statics, PSI, PS),
	CSSPtrs = PS ^ ps_sites,
	CallSiteDynamics = InitDeep ^ init_call_site_dynamics,
	ProcDynamics = InitDeep ^ init_proc_dynamics,
	construct_call_site_calls_2(CallSiteDynamics, ProcDynamics, MaxCS,
		CSSPtrs, CSDArraySlots, CallSiteCalls0, CallSiteCalls).

:- pred construct_call_site_calls_2(call_site_dynamics::in, proc_dynamics::in,
	int::in, array(call_site_static_ptr)::in,
	array(call_site_array_slot)::in,
	array(map(proc_static_ptr, list(call_site_dynamic_ptr)))::array_di,
	array(map(proc_static_ptr, list(call_site_dynamic_ptr)))::array_uo)
	is det.

construct_call_site_calls_2(CallSiteDynamics, ProcDynamics, SlotNum,
		CSSPtrs, CSDArraySlots, CallSiteCalls0, CallSiteCalls) :-
	( SlotNum >= 0 ->
		array__lookup(CSDArraySlots, SlotNum, CSDArraySlot),
		array__lookup(CSSPtrs, SlotNum, CSSPtr),
		(
			CSDArraySlot = normal(CSDPtr),
			construct_call_site_calls_3(CallSiteDynamics,
				ProcDynamics, CSSPtr, -1,
				CSDPtr, CallSiteCalls0, CallSiteCalls1)
		;
			CSDArraySlot = multi(_, CSDPtrs),
			array_foldl_from_0(
				construct_call_site_calls_3(CallSiteDynamics,
					ProcDynamics, CSSPtr),
				CSDPtrs, CallSiteCalls0, CallSiteCalls1)
		),
		construct_call_site_calls_2(CallSiteDynamics, ProcDynamics,
			SlotNum - 1, CSSPtrs, CSDArraySlots,
			CallSiteCalls1, CallSiteCalls)
	;
		CallSiteCalls = CallSiteCalls0
	).

:- pred construct_call_site_calls_3(call_site_dynamics::in, proc_dynamics::in,
	call_site_static_ptr::in, int::in, call_site_dynamic_ptr::in,
	array(map(proc_static_ptr, list(call_site_dynamic_ptr)))::array_di,
	array(map(proc_static_ptr, list(call_site_dynamic_ptr)))::array_uo)
	is det.

construct_call_site_calls_3(CallSiteDynamics, ProcDynamics, CSSPtr,
		_Dummy, CSDPtr, CallSiteCalls0, CallSiteCalls) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	( CSDI > 0 ->
		array__lookup(CallSiteDynamics, CSDI, CSD),
		PDPtr = CSD ^ csd_callee,
		PDPtr = proc_dynamic_ptr(PDI),
		array__lookup(ProcDynamics, PDI, PD),
		PSPtr = PD ^ pd_proc_static,

		CSSPtr = call_site_static_ptr(CSSI),
		array__lookup(CallSiteCalls0, CSSI, CallMap0),
		( map__search(CallMap0, PSPtr, CallList0) ->
			CallList = [CSDPtr | CallList0],
			map__det_update(CallMap0, PSPtr, CallList, CallMap)
		;
			CallList = [CSDPtr],
			map__det_insert(CallMap0, PSPtr, CallList, CallMap)
		),
		array__set(CallSiteCalls0, CSSI, CallMap, CallSiteCalls)
	;
		CallSiteCalls = CallSiteCalls0
	).

%-----------------------------------------------------------------------------%

:- pred sum_call_sites_in_proc_dynamic(int::in, call_site_dynamic::in,
	array(own_prof_info)::array_di, array(own_prof_info)::array_uo) is det.

sum_call_sites_in_proc_dynamic(_, CSD, PDOwnArray0, PDOwnArray) :-
	CalleeOwn = CSD ^ csd_own_prof,
	PDPtr = CSD ^ csd_callee,
	PDPtr = proc_dynamic_ptr(PDI),
	( PDI > 0 ->
		array__lookup(PDOwnArray0, PDI, ProcOwn0),
		ProcOwn = add_own_to_own(CalleeOwn, ProcOwn0),
		array__set(PDOwnArray0, PDI, ProcOwn, PDOwnArray)
	;
		error("sum_call_sites_in_proc_dynamic: invalid pdptr")
	).

%-----------------------------------------------------------------------------%

:- pred summarize_proc_dynamics(deep::in, deep::out) is det.

summarize_proc_dynamics(Deep0, Deep) :-
	PSOwnArray0 = Deep0 ^ ps_own,
	PSDescArray0 = Deep0 ^ ps_desc,
	array_foldl2_from_1(
		summarize_proc_dynamic(Deep0 ^ pd_own, Deep0 ^ pd_desc,
			Deep0 ^ pd_comp_table),
		Deep0 ^ proc_dynamics,
		copy(PSOwnArray0), PSOwnArray,
		copy(PSDescArray0), PSDescArray),
	Deep = ((Deep0
		^ ps_own := PSOwnArray)
		^ ps_desc := PSDescArray).

:- pred summarize_proc_dynamic(array(own_prof_info)::in,
	array(inherit_prof_info)::in, array(compensation_table)::in,
	int::in, proc_dynamic::in,
	array(own_prof_info)::array_di, array(own_prof_info)::array_uo,
	array(inherit_prof_info)::array_di, array(inherit_prof_info)::array_uo)
	is det.

summarize_proc_dynamic(PDOwnArray, PDDescArray, PDCompTableArray, PDI, PD,
		PSOwnArray0, PSOwnArray, PSDescArray0, PSDescArray) :-
	PSPtr = PD ^ pd_proc_static,
	PDPtr = proc_dynamic_ptr(PDI),
	lookup_pd_own(PDOwnArray, PDPtr, PDOwn),
	lookup_pd_desc(PDDescArray, PDPtr, PDDesc0),
	lookup_pd_comp_table(PDCompTableArray, PDPtr, PDCompTable),
	( map__search(PDCompTable, PSPtr, InnerTotal) ->
		PDDesc = subtract_inherit_from_inherit(InnerTotal, PDDesc0)
	;
		PDDesc = PDDesc0
	),
	lookup_ps_own(PSOwnArray0, PSPtr, PSOwn0),
	lookup_ps_desc(PSDescArray0, PSPtr, PSDesc0),
	add_own_to_own(PDOwn, PSOwn0) = PSOwn,
	add_inherit_to_inherit(PDDesc, PSDesc0) = PSDesc,
	update_ps_own(u(PSOwnArray0), PSPtr, PSOwn, PSOwnArray),
	update_ps_desc(u(PSDescArray0), PSPtr, PSDesc, PSDescArray).

%-----------------------------------------------------------------------------%

:- pred summarize_call_site_dynamics(deep::in, deep::out) is det.

summarize_call_site_dynamics(Deep0, Deep) :-
	CSSOwnArray0 = Deep0 ^ css_own,
	CSSDescArray0 = Deep0 ^ css_desc,
	array_foldl2_from_1(
		summarize_call_site_dynamic(
			Deep0 ^ call_site_static_map,
			Deep0 ^ call_site_statics, Deep0 ^ csd_desc,
			Deep0 ^ csd_comp_table),
		Deep0 ^ call_site_dynamics,
		copy(CSSOwnArray0), CSSOwnArray,
		copy(CSSDescArray0), CSSDescArray),
	Deep = ((Deep0
		^ css_own := CSSOwnArray)
		^ css_desc := CSSDescArray).

:- pred summarize_call_site_dynamic(call_site_static_map::in,
	call_site_statics::in, array(inherit_prof_info)::in,
	array(compensation_table)::in, int::in, call_site_dynamic::in,
	array(own_prof_info)::array_di, array(own_prof_info)::array_uo,
	array(inherit_prof_info)::array_di, array(inherit_prof_info)::array_uo)
	is det.

summarize_call_site_dynamic(CallSiteStaticMap, CallSiteStatics,
		CSDDescs, CSDCompTableArray, CSDI, CSD,
		CSSOwnArray0, CSSOwnArray, CSSDescArray0, CSSDescArray) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	lookup_call_site_static_map(CallSiteStaticMap, CSDPtr, CSSPtr),
	CSSPtr = call_site_static_ptr(CSSI),
	( CSSI > 0 ->
		CSDOwn = CSD ^ csd_own_prof,
		lookup_csd_desc(CSDDescs, CSDPtr, CSDDesc0),
		lookup_csd_comp_table(CSDCompTableArray, CSDPtr, CSDCompTable),
		lookup_call_site_statics(CallSiteStatics, CSSPtr, CSS),
		( map__search(CSDCompTable, CSS ^ css_container, InnerTotal) ->
			CSDDesc = subtract_inherit_from_inherit(InnerTotal,
				CSDDesc0)
		;
			CSDDesc = CSDDesc0
		),
		lookup_css_own(CSSOwnArray0, CSSPtr, CSSOwn0),
		lookup_css_desc(CSSDescArray0, CSSPtr, CSSDesc0),
		add_own_to_own(CSDOwn, CSSOwn0) = CSSOwn,
		add_inherit_to_inherit(CSDDesc, CSSDesc0) = CSSDesc,
		update_css_own(u(CSSOwnArray0), CSSPtr, CSSOwn,
			CSSOwnArray),
		update_css_desc(u(CSSDescArray0), CSSPtr, CSSDesc,
			CSSDescArray)
	;
		error("summarize_call_site_dynamic: invalid css ptr")
	).

%-----------------------------------------------------------------------------%

:- pred summarize_modules(deep::in, deep::out) is det.

summarize_modules(Deep0, Deep) :-
	ModuleData0 = Deep0 ^ module_data,
	ModuleData = map__map_values(summarize_module_costs(Deep0),
		ModuleData0),
	Deep = Deep0 ^ module_data := ModuleData.

:- func summarize_module_costs(deep, string, module_data) = module_data.

summarize_module_costs(Deep, _ModuleName, ModuleData0) = ModuleData :-
	ModuleData0 = module_data(Own0, Desc0, PSPtrs),
	list__foldl2(accumulate_ps_costs(Deep), PSPtrs,
		Own0, Own, Desc0, Desc),
	ModuleData = module_data(Own, Desc, PSPtrs).

:- pred accumulate_ps_costs(deep::in, proc_static_ptr::in,
	own_prof_info::in, own_prof_info::out,
	inherit_prof_info::in, inherit_prof_info::out) is det.

accumulate_ps_costs(Deep, PSPtr, Own0, Own, Desc0, Desc) :-
	deep_lookup_ps_own(Deep, PSPtr, PSOwn),
	deep_lookup_ps_desc(Deep, PSPtr, PSDesc),
	Own = add_own_to_own(Own0, PSOwn),
	Desc = add_inherit_to_inherit(Desc0, PSDesc).

%-----------------------------------------------------------------------------%

:- pred propagate_to_clique(int::in, list(proc_dynamic_ptr)::in,
	deep::in, deep::out) is det.

propagate_to_clique(CliqueNumber, Members, Deep0, Deep) :-
	array__lookup(Deep0 ^ clique_parents, CliqueNumber, ParentCSDPtr),
	list__foldl3(propagate_to_proc_dynamic(CliqueNumber, ParentCSDPtr),
		Members, Deep0, Deep1, map__init, SumTable,
		map__init, OverrideMap),
	( valid_call_site_dynamic_ptr(Deep1, ParentCSDPtr) ->
		deep_lookup_call_site_dynamics(Deep1, ParentCSDPtr, ParentCSD),
		ParentOwn = ParentCSD ^ csd_own_prof,
		deep_lookup_csd_desc(Deep1, ParentCSDPtr, ParentDesc0),
		subtract_own_from_inherit(ParentOwn, ParentDesc0) =
			ParentDesc,
		deep_update_csd_desc(Deep1, ParentCSDPtr, ParentDesc, Deep2),
		CSDCompTable = apply_override(OverrideMap, SumTable),
		deep_update_csd_comp_table(Deep2, ParentCSDPtr, CSDCompTable,
			Deep)
	;
		Deep = Deep1
	).

:- pred propagate_to_proc_dynamic(int::in, call_site_dynamic_ptr::in,
	proc_dynamic_ptr::in, deep::in, deep::out,
	compensation_table::in, compensation_table::out,
	compensation_table::in, compensation_table::out) is det.

propagate_to_proc_dynamic(CliqueNumber, ParentCSDPtr, PDPtr, Deep0, Deep,
		SumTable0, SumTable, OverrideTable0, OverrideTable) :-
	flat_call_sites(Deep0 ^ proc_dynamics, PDPtr, CSDPtrs),
	list__foldl2(propagate_to_call_site(CliqueNumber, PDPtr),
		CSDPtrs, Deep0, Deep1, map__init, PDCompTable),
	deep_update_pd_comp_table(Deep1, PDPtr, PDCompTable, Deep2),

	deep_lookup_pd_desc(Deep2, PDPtr, ProcDesc),
	deep_lookup_pd_own(Deep2, PDPtr, ProcOwn),
	ProcTotal = add_own_to_inherit(ProcOwn, ProcDesc),

	SumTable = add_comp_tables(SumTable0, PDCompTable),
	deep_lookup_proc_dynamics(Deep2, PDPtr, PD),
	PSPtr = PD ^ pd_proc_static,
	deep_lookup_proc_statics(Deep2, PSPtr, PS),
	( PS ^ ps_is_zeroed = zeroed ->
		OverrideTable = add_to_override(OverrideTable0, PSPtr,
			ProcTotal)
	;
		OverrideTable = OverrideTable0
	),

	( valid_call_site_dynamic_ptr(Deep1, ParentCSDPtr) ->
		deep_lookup_csd_desc(Deep2, ParentCSDPtr, ParentDesc0),
		ParentDesc = add_inherit_to_inherit(ParentDesc0, ProcTotal),
		deep_update_csd_desc(Deep2, ParentCSDPtr, ParentDesc, Deep)
	;
		Deep = Deep2
	).

:- pred propagate_to_call_site(int::in, proc_dynamic_ptr::in,
	call_site_dynamic_ptr::in, deep::in, deep::out,
	compensation_table::in, compensation_table::out) is det.

propagate_to_call_site(CliqueNumber, PDPtr, CSDPtr, Deep0, Deep,
		PDCompTable0, PDCompTable) :-
	deep_lookup_call_site_dynamics(Deep0, CSDPtr, CSD),
	CalleeOwn = CSD ^ csd_own_prof,
	CalleePDPtr = CSD ^ csd_callee,
	deep_lookup_clique_index(Deep0, CalleePDPtr, ChildCliquePtr),
	ChildCliquePtr = clique_ptr(ChildCliqueNumber),
	( ChildCliqueNumber \= CliqueNumber ->
		deep_lookup_pd_desc(Deep0, PDPtr, ProcDesc0),
		deep_lookup_csd_desc(Deep0, CSDPtr, CalleeDesc),
		CalleeTotal = add_own_to_inherit(CalleeOwn, CalleeDesc),
		ProcDesc = add_inherit_to_inherit(ProcDesc0, CalleeTotal),
		deep_update_pd_desc(Deep0, PDPtr, ProcDesc, Deep),
		deep_lookup_csd_comp_table(Deep, CSDPtr, CSDCompTable),
		PDCompTable = add_comp_tables(PDCompTable0, CSDCompTable)
	;
		% We don't propagate profiling measurements
		% along intra-clique calls.
		Deep = Deep0,
		PDCompTable = PDCompTable0
	).

%-----------------------------------------------------------------------------%

:- func add_comp_tables(compensation_table, compensation_table)
	= compensation_table.

add_comp_tables(CompTable1, CompTable2) = CompTable :-
	( map__is_empty(CompTable1) ->
		CompTable = CompTable2
	; map__is_empty(CompTable2) ->
		CompTable = CompTable1
	;
		CompTable = map__union(add_inherit_to_inherit,
			CompTable1, CompTable2)
	).

:- func apply_override(compensation_table, compensation_table)
	= compensation_table.

apply_override(CompTable1, CompTable2) = CompTable :-
	( map__is_empty(CompTable1) ->
		CompTable = CompTable2
	; map__is_empty(CompTable2) ->
		CompTable = CompTable1
	;
		CompTable = map__union(select_override_comp,
			CompTable1, CompTable2)
	).

:- func select_override_comp(inherit_prof_info, inherit_prof_info)
	= inherit_prof_info.

select_override_comp(OverrideComp, _) = OverrideComp.

:- func add_to_override(compensation_table,
	proc_static_ptr, inherit_prof_info) = compensation_table.

add_to_override(CompTable0, PSPtr, PDTotal) = CompTable :-
	( map__search(CompTable0, PSPtr, Comp0) ->
		Comp = add_inherit_to_inherit(Comp0, PDTotal),
		map__det_update(CompTable0, PSPtr, Comp, CompTable)
	;
		map__det_insert(CompTable0, PSPtr, PDTotal, CompTable)
	).

%-----------------------------------------------------------------------------%

:- pred flat_call_sites(proc_dynamics::in, proc_dynamic_ptr::in,
	list(call_site_dynamic_ptr)::out) is det.

flat_call_sites(ProcDynamics, PDPtr, CSDPtrs) :-
	lookup_proc_dynamics(ProcDynamics, PDPtr, PD),
	CallSiteArray = PD ^ pd_sites,
	flatten_call_sites(CallSiteArray, CSDPtrs, _).

:- pred flatten_call_sites(array(call_site_array_slot)::in,
	list(call_site_dynamic_ptr)::out, is_zeroed::out) is det.

flatten_call_sites(CallSiteArray, CSDPtrs, IsZeroed) :-
	array__to_list(CallSiteArray, CallSites),
	list__foldl2(gather_call_site_csdptrs, CallSites, [], CSDPtrsList0,
		not_zeroed, IsZeroed),
	list__reverse(CSDPtrsList0, CSDPtrsList),
	list__condense(CSDPtrsList, CSDPtrs).

:- pred gather_call_site_csdptrs(call_site_array_slot::in, 
	list(list(call_site_dynamic_ptr))::in,
	list(list(call_site_dynamic_ptr))::out,
	is_zeroed::in, is_zeroed::out) is det.

gather_call_site_csdptrs(Slot, CSDPtrs0, CSDPtrs1, IsZeroed0, IsZeroed) :-
	(
		Slot = normal(CSDPtr),
		CSDPtr = call_site_dynamic_ptr(CSDI),
		( CSDI > 0 ->
			CSDPtrs1 = [[CSDPtr] | CSDPtrs0]
		;
			CSDPtrs1 = CSDPtrs0
		),
		IsZeroed = IsZeroed0
	;
		Slot = multi(IsZeroed1, PtrArray),
		array__to_list(PtrArray, PtrList0),
		list__filter((pred(CSDPtr::in) is semidet :-
			CSDPtr = call_site_dynamic_ptr(CSDI),
			CSDI > 0
		), PtrList0, PtrList1),
		CSDPtrs1 = [PtrList1 | CSDPtrs0],
		( IsZeroed1 = zeroed ->
			IsZeroed = zeroed
		;
			IsZeroed = IsZeroed0
		)
	).

%-----------------------------------------------------------------------------%

:- pred maybe_report_stats(maybe(io__output_stream)::in,
	io::di, io::uo) is det.

% XXX: io__report_stats writes to stderr, which mdprof_cgi has closed.
% We want to write the report to _OutputStream, but the library doesn't
% support that yet.
%
% The stats are needed only when writing the deep profiling paper anyway.

maybe_report_stats(yes(_OutputStream), !IO).
	% io__report_stats("standard", !IO).
maybe_report_stats(no, !IO).

:- pred maybe_report_msg(maybe(io__output_stream)::in, string::in,
	io::di, io::uo) is det.

maybe_report_msg(yes(OutputStream), Msg, !IO) :-
	io__write_string(OutputStream, Msg, !IO),
	flush_output(OutputStream, !IO).
maybe_report_msg(no, _, !IO).

%-----------------------------------------------------------------------------%

% Predicates for use in debugging.

% :- pred print_pdis(initial_deep::in, list(int)::in,
% 	io::di, io::uo) is det.
% 
% print_pdis(InitDeep, PDIs, !IO) :-
% 	io__nl(!IO),
% 	io__write_list(PDIs, "", print_pdi_nl(InitDeep), !IO).
% 
% :- pred print_pdi_nl(initial_deep::in, int::in, io::di, io::uo)
% 	is det.
% 
% print_pdi_nl(InitDeep, PDI, !IO) :-
% 	print_pdi(InitDeep, PDI, !IO),
% 	io__nl(!IO).
% 
% :- pred print_pdi(initial_deep::in, int::in, io::di, io::uo)
% 	is det.
% 
% print_pdi(InitDeep, PDI, !IO) :-
% 	PDIsTmp = InitDeep ^ init_proc_dynamics,
% 	lookup_proc_dynamics(PDIsTmp, proc_dynamic_ptr(PDI), PD),
% 	io__format("pd %d: ", [i(PDI)], !IO),
% 	io__write(PD, !IO),
% 	io__nl(!IO),
% 	proc_static_ptr(PSI) = PD ^ pd_proc_static,
% 	PSIsTmp = InitDeep ^ init_proc_statics,
% 	lookup_proc_statics(PSIsTmp, proc_static_ptr(PSI), PS),
% 	io__format("ps %d: ", [i(PSI)], !IO),
% 	io__write(PS, !IO),
% 	io__nl(!IO).
% 
% :- pred print_csdis(initial_deep::in, list(int)::in,
% 	io::di, io::uo) is det.
% 
% print_csdis(InitDeep, CSDIs, !IO) :-
% 	io__nl(!IO),
% 	io__write_list(CSDIs, "", print_csdi_nl(InitDeep), !IO).
% 
% :- pred print_csdi_nl(initial_deep::in, int::in, io::di, io::uo)
% 	is det.
% 
% print_csdi_nl(InitDeep, CSDI, !IO) :-
% 	print_csdi(InitDeep, CSDI, !IO),
% 	io__nl(!IO).
% 
% :- pred print_csdi(initial_deep::in, int::in, io::di, io::uo)
% 	is det.
% 
% print_csdi(InitDeep, CSDI, !IO) :-
% 	CSDIsTmp = InitDeep ^ init_call_site_dynamics,
% 	lookup_call_site_dynamics(CSDIsTmp, call_site_dynamic_ptr(CSDI), CSD),
% 	io__format("csd %d: ", [i(CSDI)], !IO),
% 	io__write(CSD, !IO),
% 	io__nl(!IO),
% 	io__write_string("caller pd:\n", !IO),
% 	proc_dynamic_ptr(CallerPDI) = CSD ^ csd_caller,
% 	print_pdi(InitDeep, CallerPDI, !IO),
% 	io__write_string("callee pd:\n", !IO),
% 	proc_dynamic_ptr(CalleePDI) = CSD ^ csd_callee,
% 	print_pdi(InitDeep, CalleePDI, !IO).
% 
% :- pred write_pdi_cn_csd(int::in, int::in, int::in,
% 	io::di, io::uo) is det.
% 
% write_pdi_cn_csd(PDI, CN, CSDI, !IO) :-
% 	io__write_string("pdi ", !IO),
% 	io__write_int(PDI, !IO),
% 	io__write_string(", cn ", !IO),
% 	io__write_int(CN, !IO),
% 	io__write_string(", csdi ", !IO),
% 	io__write_int(CSDI, !IO),
% 	io__nl(!IO),
% 	io__flush_output(!IO).
