%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
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
:- import_module io, bool, list, std_util.

:- pred read_and_startup(string::in, list(string)::in, bool::in,
	maybe_error(deep)::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module profile, read_profile, callgraph.
:- import_module measurements, array_util.
:- import_module std_util, int, string, array, assoc_list, set, map, require.

read_and_startup(Machine, DataFileNames, CanonicalClique, Res) -->
	(
		{ DataFileNames = [] },
		% This should have been caught and reported by main.
		{ error("read_and_startup: no data files") }
	;
		{ DataFileNames = [DataFileName] },
		io__stderr_stream(StdErr),
		io__report_stats,
		io__write_string(StdErr, "  Reading graph data...\n"),
		read_call_graph(DataFileName, Res0),
		io__write_string(StdErr, "  Done.\n"),
		io__report_stats,
		(
			{ Res0 = ok(InitDeep) },
			startup(Machine, DataFileName, CanonicalClique,
				InitDeep, Deep),
			{ Res = ok(Deep) }
		;
			{ Res0 = error(Error) },
			{ Res = error(Error) }
		)
	;
		{ DataFileNames = [_, _ | _] },
		{ error("mdprof_server: merging of data files is not yet implemented") }
	).

:- pred startup(string::in, string::in, bool::in, initial_deep::in, deep::out,
	io__state::di, io__state::uo) is det.

startup(Machine, DataFileName, _CanonicalClique, InitDeep0, Deep) -->
	stderr_stream(StdErr),

	{ InitDeep0 = initial_deep(InitStats, Root,
		CallSiteDynamics0, ProcDynamics,
		CallSiteStatics0, ProcStatics) },

	io__format(StdErr,
		"  Mapping static call sites to containing procedures...\n",
		[]),
	{ array_foldl_from_1(record_css_containers, ProcStatics,
		u(CallSiteStatics0), CallSiteStatics) },
	io__format(StdErr, "  Done.\n", []),
	io__report_stats,

	io__format(StdErr,
		"  Mapping dynamic call sites to containing procedures...\n",
		[]),
	{ array_foldl_from_1(record_csd_containers, ProcDynamics,
		u(CallSiteDynamics0), CallSiteDynamics) },
	io__format(StdErr, "  Done.\n", []),
	io__report_stats,

	{ InitDeep1 = initial_deep(InitStats, Root,
		CallSiteDynamics, ProcDynamics,
		CallSiteStatics, ProcStatics) },

	{ InitDeep = InitDeep1 },
%	(
%		{ CanonicalClique = no },
%		{ InitDeep = InitDeep1 }
%	;
%		{ CanonicalClique = yes },
%		io__format(StdErr, "  Canonicalizing cliques...\n", []),
%		{ canonicalize_cliques(InitDeep1, InitDeep) },
%		io__format(StdErr, "  Done.\n", []),
%		io__report_stats
%	),

	{ array__max(InitDeep ^ init_proc_dynamics, PDMax) },
	{ NPDs = PDMax + 1 },
	{ array__max(InitDeep ^ init_call_site_dynamics, CSDMax) },
	{ NCSDs = CSDMax + 1 },
	{ array__max(InitDeep ^ init_proc_statics, PSMax) },
	{ NPSs = PSMax + 1 },
	{ array__max(InitDeep ^ init_call_site_statics, CSSMax) },
	{ NCSSs = CSSMax + 1 },

	io__format(StdErr, "  Finding cliques...\n", []),
	flush_output(StdErr),
	{ find_cliques(InitDeep, CliqueList) },
	io__format(StdErr, "  Done.\n", []),
	io__report_stats,

	io__format(StdErr, "  Constructing clique indexes...\n", []),
	flush_output(StdErr),
	{ make_clique_indexes(NPDs, CliqueList, Cliques, CliqueIndex) },
	io__format(StdErr, "  Done.\n", []),
	io__report_stats,

	io__format(StdErr, "  Constructing clique parent map...\n", []),

		% For each CallSiteDynamic pointer, if it points to
		% a ProcDynamic which is in a different clique to
		% the one from which the CallSiteDynamic's parent
		% came, then this CallSiteDynamic is the entry to
		% the [lower] clique. We need to compute this information
		% so that we can print clique-based timing summaries in
		% the browser.
	{ array__max(Cliques, CliqueMax) },
	{ NCliques = CliqueMax + 1 },
	{ array__init(NCliques, call_site_dynamic_ptr(-1), CliqueParents0) },
	{ array__init(NCSDs, no, CliqueMaybeChildren0) },
	{ array_foldl2_from_1(construct_clique_parents(InitDeep, CliqueIndex),
		CliqueIndex,
		CliqueParents0, CliqueParents,
		CliqueMaybeChildren0, CliqueMaybeChildren) },

	io__format(StdErr, "  Done.\n", []),
	io__report_stats,

	io__format(StdErr, "  Finding procedure callers...\n", []),
	{ array__init(NPSs, [], ProcCallers0) },
	{ array_foldl_from_1(construct_proc_callers(InitDeep),
		CallSiteDynamics, ProcCallers0, ProcCallers) },
	io__format(StdErr, "  Done.\n", []),
	io__report_stats,

	io__format(StdErr, "  Constructing call site static map...\n", []),
	{ array__init(NCSDs, call_site_static_ptr(-1), CallSiteStaticMap0) },
	{ array_foldl_from_1(construct_call_site_caller(InitDeep),
		ProcDynamics, CallSiteStaticMap0, CallSiteStaticMap) },
	io__format(StdErr, "  Done.\n", []),
	io__report_stats,

	io__format(StdErr, "  Finding call site calls...\n", []),
	{ array__init(NCSSs, map__init, CallSiteCalls0) },
	{ array_foldl_from_1(construct_call_site_calls(InitDeep),
		ProcDynamics, CallSiteCalls0, CallSiteCalls) },
	io__format(StdErr, "  Done.\n", []),
	io__report_stats,

	io__format(StdErr, "  Propagating time up call graph...\n", []),

	{ array__init(NCSDs, zero_inherit_prof_info, CSDDesc0) },
	{ array__init(NPDs, zero_own_prof_info, PDOwn0) },
	{ array_foldl_from_1(sum_call_sites_in_proc_dynamic,
		CallSiteDynamics, PDOwn0, PDOwn) },
	{ array__init(NPDs, zero_inherit_prof_info, PDDesc0) },
	{ array__init(NPSs, zero_own_prof_info, PSOwn0) },
	{ array__init(NPSs, zero_inherit_prof_info, PSDesc0) },
	{ array__init(NCSSs, zero_own_prof_info, CSSOwn0) },
	{ array__init(NCSSs, zero_inherit_prof_info, CSSDesc0) },

	{ Deep0 = deep(InitStats, Machine, DataFileName, Root,
		CallSiteDynamics, ProcDynamics, CallSiteStatics, ProcStatics,
		CliqueIndex, Cliques, CliqueParents, CliqueMaybeChildren,
		ProcCallers, CallSiteStaticMap, CallSiteCalls,
		PDOwn, PDDesc0, CSDDesc0,
		PSOwn0, PSDesc0, CSSOwn0, CSSDesc0) },

	{ array_foldl_from_1(propagate_to_clique, Cliques, Deep0, Deep1) },
	io__format(StdErr, "  Done.\n", []),
	io__report_stats,

	io__format(StdErr, "  Summarizing information...\n", []),
	{ summarize_proc_dynamics(Deep1, Deep2) },
	{ summarize_call_site_dynamics(Deep2, Deep) },
	io__format(StdErr, "  Done.\n", []),
	io__report_stats.

:- pred count_quanta(int::in, call_site_dynamic::in, int::in, int::out) is det.

count_quanta(_N, CSD, Quanta0, Quanta) :-
	Quanta = Quanta0 + quanta(CSD ^ csd_own_prof).

%-----------------------------------------------------------------------------%

:- pred record_css_containers(int::in, proc_static::in,
	array(call_site_static)::array_di,
	array(call_site_static)::array_uo) is det.

record_css_containers(PSI, PS, CallSiteStatics0, CallSiteStatics) :-
	CSSPtrs = PS ^ ps_sites,
	PSPtr = proc_static_ptr(PSI),
	array__max(CSSPtrs, MaxCS),
	record_css_containers_2(MaxCS, PSPtr, CSSPtrs,
		CallSiteStatics0, CallSiteStatics).

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

:- pred record_csd_containers(int::in, proc_dynamic::in,
	array(call_site_dynamic)::array_di,
	array(call_site_dynamic)::array_uo) is det.

record_csd_containers(PDI, PD, CallSiteDynamics0, CallSiteDynamics) :-
	CSDArray = PD ^ pd_sites,
	PDPtr = proc_dynamic_ptr(PDI),
	flatten_call_sites(CSDArray, CSDPtrs),
	record_csd_containers_2(PDPtr, CSDPtrs,
		CallSiteDynamics0, CallSiteDynamics).

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

:- pred flat_call_sites(proc_dynamics::in, proc_dynamic_ptr::in,
	list(call_site_dynamic_ptr)::out) is det.

flat_call_sites(ProcDynamics, PDPtr, CSDPtrs) :-
	( PDPtr = proc_dynamic_ptr(PDI), PDI > 0 ->
		array__lookup(ProcDynamics, PDI, PD),
		CallSiteArray = PD ^ pd_sites,
		flatten_call_sites(CallSiteArray, CSDPtrs)
	;
		CSDPtrs = []
	).

:- pred flatten_call_sites(array(call_site_array_slot)::in,
	list(call_site_dynamic_ptr)::out) is det.

flatten_call_sites(CallSiteArray, CSDPtrs) :-
	array__to_list(CallSiteArray, CallSites),
	list__foldl((pred(Slot::in, CSDPtrs0::in, CSDPtrs1::out) is det :-
		(
			Slot = normal(CSDPtr),
			CSDPtr = call_site_dynamic_ptr(CSDI),
			( CSDI > 0 ->
				CSDPtrs1 = [[CSDPtr] | CSDPtrs0]
			;
				CSDPtrs1 = CSDPtrs0
			)
		;
			Slot = multi(PtrArray),
			array__to_list(PtrArray, PtrList0),
			filter((pred(CSDPtr::in) is semidet :-
				CSDPtr = call_site_dynamic_ptr(CSDI),
				CSDI > 0
			), PtrList0, PtrList1),
			CSDPtrs1 = [PtrList1 | CSDPtrs0]
		)
	), CallSites, [], CSDPtrsList0),
	list__reverse(CSDPtrsList0, CSDPtrsList),
	list__condense(CSDPtrsList, CSDPtrs).

:- pred construct_proc_callers(initial_deep::in, int::in,
	call_site_dynamic::in,
	array(list(call_site_dynamic_ptr))::array_di,
	array(list(call_site_dynamic_ptr))::array_uo) is det.

construct_proc_callers(InitDeep, CSDI, CSD, ProcCallers0, ProcCallers) :-
	PDPtr = CSD ^ csd_callee,
	PDPtr = proc_dynamic_ptr(PDI),
	( PDI > 0, array__in_bounds(InitDeep ^ init_proc_dynamics, PDI) ->
		array__lookup(InitDeep ^ init_proc_dynamics, PDI, PD),
		PSPtr = PD ^ pd_proc_static,
		PSPtr = proc_static_ptr(PSI),
		array__lookup(ProcCallers0, PSI, Callers0),
		Callers = [call_site_dynamic_ptr(CSDI) | Callers0],
		array__set(ProcCallers0, PSI, Callers, ProcCallers)
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
	PSPtr = proc_static_ptr(PSI),
	array__lookup(InitDeep ^ init_proc_statics, PSI, PS),
	PS = proc_static(_, _, _, _, CSSPtrs),
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
			CSDArraySlot = multi(CSDPtrs),
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
			CSDArraySlot = multi(CSDPtrs),
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

:- pred sum_call_sites_in_proc_dynamic(int::in, call_site_dynamic::in,
	array(own_prof_info)::array_di, array(own_prof_info)::array_uo) is det.

sum_call_sites_in_proc_dynamic(_, CSD, PDO0, PDO) :-
	PDPtr = CSD ^ csd_callee,
	PI = CSD ^ csd_own_prof,
	PDPtr = proc_dynamic_ptr(PDI),
	( PDI > 0 ->
		array__lookup(PDO0, PDI, OwnPI0),
		OwnPI = add_own_to_own(PI, OwnPI0),
		array__set(PDO0, PDI, OwnPI, PDO)
	;
		PDO = PDO0
	).

:- pred summarize_proc_dynamics(deep::in, deep::out) is det.

summarize_proc_dynamics(Deep0, Deep) :-
	PSOwn0 = Deep0 ^ ps_own,
	PSDesc0 = Deep0 ^ ps_desc,
	array_foldl2_from_1(
		summarize_proc_dynamic(Deep0 ^ pd_own, Deep0 ^ pd_desc),
		Deep0 ^ proc_dynamics,
		copy(PSOwn0), PSOwn, copy(PSDesc0), PSDesc),
	Deep = ((Deep0
		^ ps_own := PSOwn)
		^ ps_desc := PSDesc).

:- pred summarize_proc_dynamic(array(own_prof_info)::in,
	array(inherit_prof_info)::in, int::in, proc_dynamic::in,
	array(own_prof_info)::array_di, array(own_prof_info)::array_uo,
	array(inherit_prof_info)::array_di, array(inherit_prof_info)::array_uo)
	is det.

summarize_proc_dynamic(PDOwn, PDDesc, PDI, PD,
		PSOwn0, PSOwn, PSDesc0, PSDesc) :-
	PSPtr = PD ^ pd_proc_static,
	PSPtr = proc_static_ptr(PSI),
	( PSI > 0 ->
		array__lookup(PDOwn, PDI, PDOwnPI),
		array__lookup(PDDesc, PDI, PDDescPI),

		array__lookup(PSOwn0, PSI, PSOwnPI0),
		array__lookup(PSDesc0, PSI, PSDescPI0),

		add_own_to_own(PDOwnPI, PSOwnPI0) = PSOwnPI,
		add_inherit_to_inherit(PDDescPI, PSDescPI0) = PSDescPI,
		array__set(u(PSOwn0), PSI, PSOwnPI, PSOwn),
		array__set(u(PSDesc0), PSI, PSDescPI, PSDesc)
	;
		error("emit nasal devils")
	).

:- pred summarize_call_site_dynamics(deep::in, deep::out) is det.

summarize_call_site_dynamics(Deep0, Deep) :-
	CSSOwn0 = Deep0 ^ css_own,
	CSSDesc0 = Deep0 ^ css_desc,
	array_foldl2_from_1(
		summarize_call_site_dynamic(Deep0 ^ call_site_static_map,
		Deep0 ^ csd_desc),
		Deep0 ^ call_site_dynamics,
		copy(CSSOwn0), CSSOwn, copy(CSSDesc0), CSSDesc),
	Deep = ((Deep0
		^ css_own := CSSOwn)
		^ css_desc := CSSDesc).

:- pred summarize_call_site_dynamic(call_site_static_map::in,
	array(inherit_prof_info)::in, int::in, call_site_dynamic::in,
	array(own_prof_info)::array_di, array(own_prof_info)::array_uo,
	array(inherit_prof_info)::array_di, array(inherit_prof_info)::array_uo)
	is det.

summarize_call_site_dynamic(CallSiteStaticMap, CSDDescs, CSDI, CSD,
		CSSOwn0, CSSOwn, CSSDesc0, CSSDesc) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	lookup_call_site_static_map(CallSiteStaticMap, CSDPtr, CSSPtr),
	CSSPtr = call_site_static_ptr(CSSI),
	( CSSI > 0 ->
		CSDOwnPI = CSD ^ csd_own_prof,
		array__lookup(CSDDescs, CSDI, CSDDescPI),

		array__lookup(CSSOwn0, CSSI, CSSOwnPI0),
		array__lookup(CSSDesc0, CSSI, CSSDescPI0),

		add_own_to_own(CSDOwnPI, CSSOwnPI0)
			= CSSOwnPI,
		add_inherit_to_inherit(CSDDescPI, CSSDescPI0)
			= CSSDescPI,
		array__set(u(CSSOwn0), CSSI, CSSOwnPI, CSSOwn),
		array__set(u(CSSDesc0), CSSI, CSSDescPI, CSSDesc)
	;
		error("emit nasal gorgons")
	).

:- pred propagate_to_clique(int::in, list(proc_dynamic_ptr)::in,
	deep::in, deep::out) is det.

propagate_to_clique(CliqueNumber, Members, Deep0, Deep) :-
	array__lookup(Deep0 ^ clique_parents, CliqueNumber, ParentCSDPtr),
	list__foldl(propagate_to_proc_dynamic(CliqueNumber, ParentCSDPtr),
		Members, Deep0, Deep1),
	(
		valid_call_site_dynamic_ptr_raw(Deep1 ^ call_site_dynamics,
			ParentCSDPtr)
	->
		lookup_call_site_dynamics(Deep1 ^ call_site_dynamics,
			ParentCSDPtr, ParentCSD),
		ParentOwnPI = ParentCSD ^ csd_own_prof,
		deep_lookup_csd_desc(Deep1, ParentCSDPtr, ParentDesc0),
		subtract_own_from_inherit(ParentOwnPI, ParentDesc0) =
			ParentDesc,
		deep_update_csd_desc(Deep1, ParentCSDPtr, ParentDesc, Deep)
	;
		Deep = Deep1
	).

:- pred propagate_to_proc_dynamic(int::in, call_site_dynamic_ptr::in,
	proc_dynamic_ptr::in, deep::in, deep::out) is det.

propagate_to_proc_dynamic(CliqueNumber, ParentCSDPtr, PDPtr,
		Deep0, Deep) :-
	flat_call_sites(Deep0 ^ proc_dynamics, PDPtr, CSDPtrs),
	list__foldl(propagate_to_call_site(CliqueNumber, PDPtr),
		CSDPtrs, Deep0, Deep1),
	(
		valid_call_site_dynamic_ptr_raw(Deep1 ^ call_site_dynamics,
			ParentCSDPtr)
	->
		deep_lookup_csd_desc(Deep1, ParentCSDPtr, ParentDesc0),
		deep_lookup_pd_desc(Deep1, PDPtr, DescPI),
		deep_lookup_pd_own(Deep1, PDPtr, OwnPI),
		add_own_to_inherit(OwnPI, ParentDesc0) = ParentDesc1,
		add_inherit_to_inherit(DescPI, ParentDesc1) = ParentDesc,
		deep_update_csd_desc(Deep1, ParentCSDPtr, ParentDesc, Deep)
	;
		Deep = Deep1
	).

:- pred propagate_to_call_site(int::in, proc_dynamic_ptr::in,
	call_site_dynamic_ptr::in, deep::in, deep::out) is det.

propagate_to_call_site(CliqueNumber, PDPtr, CSDPtr, Deep0, Deep) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	( CSDI > 0 ->
		array__lookup(Deep0 ^ call_site_dynamics, CSDI, CSD),
		CPDPtr = CSD ^ csd_callee,
		CPI = CSD ^ csd_own_prof,
		CPDPtr = proc_dynamic_ptr(CPDI),
		( CPDI > 0 ->
			array__lookup(Deep0 ^ clique_index, CPDI,
				clique_ptr(ChildCliqueNumber)),
			( ChildCliqueNumber \= CliqueNumber ->
				PDPtr = proc_dynamic_ptr(PDI),
				array__lookup(Deep0 ^ pd_desc, PDI, PDTotal0),
				array__lookup(Deep0 ^ csd_desc, CSDI, CDesc),
				add_own_to_inherit(CPI, PDTotal0) = PDTotal1,
				add_inherit_to_inherit(CDesc, PDTotal1)
					= PDTotal,
				array__set(u(Deep0 ^ pd_desc), PDI, PDTotal,
					PDDesc),
				Deep = Deep0 ^ pd_desc := PDDesc
			;
				Deep = Deep0
			)
		;
			Deep = Deep0
		)
	;
		Deep = Deep0
	).

%-----------------------------------------------------------------------------%
