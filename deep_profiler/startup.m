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

:- import_module unsafe.
:- import_module profile, read_profile, cliques, measurements, array_util.
:- import_module std_util, int, array, assoc_list, set, map, require.

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
			{ Res0 = ok(InitialDeep) },
			startup(Machine, DataFileName, CanonicalClique,
				InitialDeep, Deep),
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

startup(Machine, DataFileName, CanonicalClique, InitialDeep0, Deep) -->
	stderr_stream(StdErr),

	{ InitialDeep0 = initial_deep(InitStats, Root,
		CallSiteDynamics0, ProcDynamics,
		CallSiteStatics0, ProcStatics) },

	format(StdErr,
		"  Mapping static call sites to containing procedures...\n",
		[]),
	{ array_foldl(record_css_containers, ProcStatics,
		u(CallSiteStatics0), CallSiteStatics) },
	format(StdErr, "  Done.\n", []),
	io__report_stats,

	format(StdErr,
		"  Mapping dynamic call sites to containing procedures...\n",
		[]),
	{ array_foldl(record_csd_containers, ProcDynamics,
		u(CallSiteDynamics0), CallSiteDynamics) },
	format(StdErr, "  Done.\n", []),
	io__report_stats,

	{ InitialDeep1 = initial_deep(InitStats, Root,
		CallSiteDynamics, ProcDynamics,
		CallSiteStatics, ProcStatics) },

	find_cliques(InitialDeep1, CliqueList0),
	(
		{ CanonicalClique = no },
		{ InitialDeep = InitialDeep1 },
		{ CliqueList = CliqueList0 }
	;
		{ CanonicalClique = yes },
		format(StdErr, "  Canonicalizing cliques...\n", []),
		{ merge_cliques(CliqueList0, InitialDeep1, InitialDeep) },
		io__report_stats,

		find_cliques(InitialDeep, CliqueList)
	),

	format(StdErr, "  Constructing clique indexes...\n", []),
	flush_output(StdErr),

	{ Cliques = array(CliqueList) },

	{ array__max(ProcDynamics, PDMax) },
	{ NPDs = PDMax + 1 },
	{ array__max(CallSiteDynamics, CSDMax) },
	{ NCSDs = CSDMax + 1 },
	{ array__max(ProcStatics, PSMax) },
	{ NPSs = PSMax + 1 },
	{ array__max(CallSiteStatics, CSSMax) },
	{ NCSSs = CSSMax + 1 },

	{ array__init(NPDs, clique_ptr(-1), CliqueIndex0) },

		% For each clique, add entries in an array
		% that maps from each clique member (ProcDynamic)
		% back to the clique to which it belongs.
	{ array_foldl((pred(CliqueN::in, CliqueMembers::in,
				I0::array_di, I::array_uo) is det :-
		array_list_foldl((pred(X::in, I1::array_di, I2::array_uo)
				is det :-
			X = proc_dynamic_ptr(Y),
			array__set(I1, Y, clique_ptr(CliqueN), I2)
		), CliqueMembers, I0, I)
	), Cliques, CliqueIndex0, CliqueIndex) },
	format(StdErr, "  Done.\n", []),
	io__report_stats,

	format(StdErr, "  Constructing clique parent map...\n", []),

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
	{ array_foldl2(construct_clique_parents(InitialDeep, CliqueIndex),
		CliqueIndex,
		CliqueParents0, CliqueParents,
		CliqueMaybeChildren0, CliqueMaybeChildren) },

	format(StdErr, "  Done.\n", []),
	io__report_stats,

	format(StdErr, "  Finding procedure callers...\n", []),
	{ array__init(NPSs, [], ProcCallers0) },
	{ array_foldl(construct_proc_callers(InitialDeep), CallSiteDynamics,
		ProcCallers0, ProcCallers) },
	format(StdErr, "  Done.\n", []),
	io__report_stats,

	format(StdErr, "  Constructing call site static map...\n", []),
	{ array__init(NCSDs, call_site_static_ptr(-1), CallSiteStaticMap0) },
	{ array_foldl(construct_call_site_caller(InitialDeep), ProcDynamics,
		CallSiteStaticMap0, CallSiteStaticMap) },
	format(StdErr, "  Done.\n", []),
	io__report_stats,

	format(StdErr, "  Finding call site calls...\n", []),
	{ array__init(NCSSs, map__init, CallSiteCalls0) },
	{ array_foldl(construct_call_site_calls(InitialDeep), ProcDynamics,
		CallSiteCalls0, CallSiteCalls) },
	format(StdErr, "  Done.\n", []),
	io__report_stats,

	format(StdErr, "  Propagating time up call graph...\n", []),

	{ array__init(NCSDs, zero_inherit_prof_info, CSDDesc0) },
	{ array__init(NPDs, zero_own_prof_info, PDOwn0) },
	{ array_foldl(sum_call_sites_in_proc_dynamic,
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

	{ array_foldl(propagate_to_clique, Cliques, Deep0, Deep1) },
	format(StdErr, "  Done.\n", []),
	io__report_stats,

	format(StdErr, "  Summarizing information...\n", []),
	{ summarize_proc_dynamics(Deep1, Deep2) },
	{ summarize_call_site_dynamics(Deep2, Deep) },
	format(StdErr, "  Done.\n", []),
	io__report_stats.

:- pred find_cliques(initial_deep::in, list(list(proc_dynamic_ptr))::out,
	io__state::di, io__state::uo) is det.

find_cliques(InitialDeep, CliqueList) -->
	stderr_stream(StdErr),
	format(StdErr, "  Constructing graph...\n", []),
	make_graph(InitialDeep, Graph),
	format(StdErr, "  Done.\n", []),
	io__report_stats,

	format(StdErr, "  Constructing cliques...\n", []),
	{ topological_sort(Graph, CliqueList0) },

		% Turn each of the sets into a list.
		% (We use foldl here because the list may be very
		% long and map runs out of stack space, and we
		% want the final list in reverse order anyway.)
	{ list__foldl((pred(Set::in, L0::in, L::out) is det :-
		set__to_sorted_list(Set, List0),
		map((pred(PDI::in, PDPtr::out) is det :-
			PDPtr = proc_dynamic_ptr(PDI)
		), List0, List),
		L = [List | L0]
	), CliqueList0, [], CliqueList) },
		% It's actually more convenient to have the list in
		% reverse order so that foldl works from the bottom
		% of the tsort to the top, so that we can use it to
		% do the propagation simply.
	format(StdErr, "  Done.\n", []),
	io__report_stats.

%-----------------------------------------------------------------------------%

:- pred make_graph(initial_deep::in, graph::out,
	io__state::di, io__state::uo) is det.

make_graph(InitialDeep, Graph) -->
	{ init(Graph0) },
	array_foldl2((pred(PDI::in, PD::in, G1::in, G2::out, di, uo) is det -->
		{ From = PDI },
		{ CallSiteRefArray = PD ^ pd_sites },
	        { array__to_list(CallSiteRefArray, CallSiteRefList) },
	        list__foldl2((pred(CSR::in, G5::in, G6::out, di, uo) is det -->
		    (
			{ CSR = normal(call_site_dynamic_ptr(CSDI)) },
			( { CSDI > 0 } ->
				{ array__lookup(
					InitialDeep ^ init_call_site_dynamics,
					CSDI, CSD) },
				{ CPDPtr = CSD ^ csd_callee },
				{ CPDPtr = proc_dynamic_ptr(To) },
				{ add_arc(G5, From, To, G6) }
			;
				{ G6 = G5 }
			)
		    ;
			{ CSR = multi(CallSiteArray) },
			{ array__to_list(CallSiteArray, CallSites) },
			list__foldl2((pred(CSDPtr1::in, G7::in, G8::out,
					di, uo) is det -->
			    { CSDPtr1 = call_site_dynamic_ptr(CSDI) },
			    ( { CSDI > 0 } ->
			    	{ array__lookup(
					InitialDeep ^ init_call_site_dynamics,
					CSDI, CSD) },
				{ CPDPtr = CSD ^ csd_callee },
			    	{ CPDPtr = proc_dynamic_ptr(To) },
			    	{ add_arc(G7, From, To, G8) }
			    ;
			    	{ G8 = G7 }
			    )
			), CallSites, G5, G6)
		    )
	        ), CallSiteRefList, G1, G2)
	), InitialDeep ^ init_proc_dynamics, Graph0, Graph).

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
	CSD0 = call_site_dynamic(CallerPDPtr0, CalleePDPtr, Own,
		MaybeRedirect),
	require(unify(CallerPDPtr0, proc_dynamic_ptr(-1)),
		"record_csd_containers_2: real proc_dynamic_ptr"),
	CSD = call_site_dynamic(PDPtr, CalleePDPtr, Own, MaybeRedirect),
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

construct_clique_parents(InitialDeep, CliqueIndex, PDI, CliquePtr,
		CliqueParents0, CliqueParents,
		CliqueMaybeChildren0, CliqueMaybeChildren) :-
	( PDI > 0 ->
		flat_call_sites(InitialDeep ^ init_proc_dynamics,
			proc_dynamic_ptr(PDI), CSDPtrs),
		array_list_foldl2(
			construct_clique_parents_2(InitialDeep,
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

construct_clique_parents_2(InitialDeep, CliqueIndex, ParentCliquePtr, CSDPtr,
		CliqueParents0, CliqueParents,
		CliqueMaybeChildren0, CliqueMaybeChildren) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	( CSDI > 0 ->
		array__lookup(InitialDeep ^ init_call_site_dynamics, CSDI,
			CSD),
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

construct_proc_callers(InitialDeep, CSDI, CSD, ProcCallers0, ProcCallers) :-
	PDPtr = CSD ^ csd_callee,
	PDPtr = proc_dynamic_ptr(PDI),
	( PDI > 0, array__in_bounds(InitialDeep ^ init_proc_dynamics, PDI) ->
		array__lookup(InitialDeep ^ init_proc_dynamics, PDI, PD),
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

construct_call_site_caller(InitialDeep, _PDI, PD,
		CallSiteStaticMap0, CallSiteStaticMap) :-
	PSPtr = PD ^ pd_proc_static,
	CSDArraySlots = PD ^ pd_sites,
	PSPtr = proc_static_ptr(PSI),
	array__lookup(InitialDeep ^ init_proc_statics, PSI, PS),
	PS = proc_static(_, _, _, _, CSSPtrs),
	array__max(CSDArraySlots, MaxCS),
	construct_call_site_caller_2(MaxCS,
		InitialDeep ^ init_call_site_dynamics, CSSPtrs, CSDArraySlots,
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
			array_foldl0(
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

construct_call_site_calls(InitialDeep, _PDI, PD,
		CallSiteCalls0, CallSiteCalls) :-
	PSPtr = PD ^ pd_proc_static,
	CSDArraySlots = PD ^ pd_sites,
	array__max(CSDArraySlots, MaxCS),
	PSPtr = proc_static_ptr(PSI),
	array__lookup(InitialDeep ^ init_proc_statics, PSI, PS),
	CSSPtrs = PS ^ ps_sites,
	CallSiteDynamics = InitialDeep ^ init_call_site_dynamics,
	ProcDynamics = InitialDeep ^ init_proc_dynamics,
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
			array_foldl0(
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
	array_foldl2(summarize_proc_dynamic(Deep0 ^ pd_own, Deep0 ^ pd_desc),
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
	array_foldl2(summarize_call_site_dynamic(Deep0 ^ call_site_static_map,
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

:- pred merge_cliques(list(list(proc_dynamic_ptr))::in,
	initial_deep::in, initial_deep::out) is det.

merge_cliques([], InitDeep, InitDeep).
merge_cliques([Clique | Cliques], InitDeep0, InitDeep) :-
	merge_clique(Clique, InitDeep0, InitDeep1),
	merge_cliques(Cliques, InitDeep1, InitDeep).

:- pred merge_clique(list(proc_dynamic_ptr)::in,
	initial_deep::in, initial_deep::out) is det.

merge_clique(CliquePDs, InitDeep0, InitDeep) :-
	map__init(ProcMap0),
	set__list_to_set(CliquePDs, Clique),
	list__foldl(cluster_pds_by_ps(InitDeep0), CliquePDs,
		ProcMap0, ProcMap),
	map__values(ProcMap, PDsList),
	list__filter(two_or_more, PDsList, ToMergePDsList),
	list__foldl(merge_proc_dynamics(Clique), ToMergePDsList,
		InitDeep0, InitDeep).

:- pred merge_proc_dynamics(set(proc_dynamic_ptr)::in,
	list(proc_dynamic_ptr)::in,
	initial_deep::in, initial_deep::out) is det.

merge_proc_dynamics(Clique, CandidatePDPtrs, InitDeep0, InitDeep) :-
	ProcDynamics0 = InitDeep0 ^ init_proc_dynamics,
	list__filter(valid_proc_dynamic_ptr_raw(ProcDynamics0),
		CandidatePDPtrs, ValidPDPtrs),
	( ValidPDPtrs = [PrimePDPtr | RestPDPtrs] ->
		record_pd_redirect(RestPDPtrs, PrimePDPtr,
			InitDeep0, InitDeep1),
		ProcDynamics1 = InitDeep1 ^ init_proc_dynamics,
		lookup_proc_dynamics(ProcDynamics1, PrimePDPtr, PrimePD0),
		list__map(lookup_proc_dynamics(ProcDynamics1),
			RestPDPtrs, RestPDs),
		list__map(extract_pd_sites, RestPDs, RestSites),
		require(unify(PrimePD0 ^ pd_redirect, no),
			"merge_proc_dynamics: pd already redirected"),
		PrimeSites0 = PrimePD0 ^ pd_sites,
		array__max(PrimeSites0, MaxSiteNum),
		merge_proc_dynamic_slots(MaxSiteNum, Clique, PrimePDPtr,
			u(PrimeSites0), RestSites, PrimeSites,
			InitDeep1, InitDeep2),
		PrimePD = PrimePD0 ^ pd_sites := PrimeSites,
		ProcDynamics2 = InitDeep2 ^ init_proc_dynamics,
		update_proc_dynamics(u(ProcDynamics2), PrimePDPtr, PrimePD,
			ProcDynamics),
		InitDeep = InitDeep2 ^ init_proc_dynamics := ProcDynamics
	;
		% This can happen when merging the callees of CSDs
		% representing special calls.
		InitDeep = InitDeep0
	).

:- pred merge_proc_dynamic_slots(int::in, set(proc_dynamic_ptr)::in,
	proc_dynamic_ptr::in, array(call_site_array_slot)::array_di,
	list(array(call_site_array_slot))::in,
	array(call_site_array_slot)::array_uo,
	initial_deep::in, initial_deep::out) is det.

merge_proc_dynamic_slots(SlotNum, Clique, PrimePDPtr, PrimeSiteArray0,
		RestSiteArrays, PrimeSiteArray, InitDeep0, InitDeep) :-
	( SlotNum >= 0 ->
		array__lookup(PrimeSiteArray0, SlotNum, PrimeSite0),
		(
			PrimeSite0 = normal(PrimeCSDPtr0),
			merge_proc_dynamic_normal_slot(SlotNum, Clique,
				PrimePDPtr, PrimeCSDPtr0, RestSiteArrays,
				PrimeCSDPtr, InitDeep0, InitDeep1),
			array__set(PrimeSiteArray0, SlotNum,
				normal(PrimeCSDPtr), PrimeSiteArray1)
		;
			PrimeSite0 = multi(PrimeCSDPtrArray0),
			array__to_list(PrimeCSDPtrArray0, PrimeCSDPtrList0),
			merge_proc_dynamic_multi_slot(SlotNum, Clique,
				PrimePDPtr, PrimeCSDPtrList0, RestSiteArrays,
				PrimeCSDPtrList, InitDeep0, InitDeep1),
			PrimeCSDPtrArray = array(PrimeCSDPtrList),
			array__set(PrimeSiteArray0, SlotNum,
				multi(PrimeCSDPtrArray), PrimeSiteArray1)
		),
		merge_proc_dynamic_slots(SlotNum - 1, Clique, PrimePDPtr,
			PrimeSiteArray1, RestSiteArrays, PrimeSiteArray,
			InitDeep1, InitDeep)
	;
		PrimeSiteArray = PrimeSiteArray0,
		InitDeep = InitDeep0
	).

:- pred merge_proc_dynamic_normal_slot(int::in, set(proc_dynamic_ptr)::in,
	proc_dynamic_ptr::in, call_site_dynamic_ptr::in,
	list(array(call_site_array_slot))::in, call_site_dynamic_ptr::out,
	initial_deep::in, initial_deep::out) is det.

merge_proc_dynamic_normal_slot(SlotNum, Clique, PrimePDPtr, PrimeCSDPtr0,
		RestSiteArrays, PrimeCSDPtr, InitDeep0, InitDeep) :-
	lookup_normal_sites(RestSiteArrays, SlotNum, RestCSDPtrs),
	merge_call_site_dynamics(Clique, PrimePDPtr,
		[PrimeCSDPtr0 | RestCSDPtrs], PrimeCSDPtr,
		InitDeep0, InitDeep).

:- pred accumulate_csd_owns(call_site_dynamic::in,
	own_prof_info::in, own_prof_info::out) is det.

accumulate_csd_owns(CSD, Own0, Own) :-
	Own = add_own_to_own(Own0, CSD ^ csd_own_prof).

:- pred callee_in_clique(initial_deep::in, set(proc_dynamic_ptr)::in,
	call_site_dynamic_ptr::in) is semidet.

callee_in_clique(InitDeep, Clique, CSDPtr) :-
	lookup_call_site_dynamics(InitDeep ^ init_call_site_dynamics,
		CSDPtr, CSD),
	CalleePDPtr = CSD ^ csd_callee,
	set__member(CalleePDPtr, Clique).

:- pred merge_proc_dynamic_multi_slot(int::in, set(proc_dynamic_ptr)::in,
	proc_dynamic_ptr::in, list(call_site_dynamic_ptr)::in,
	list(array(call_site_array_slot))::in,
	list(call_site_dynamic_ptr)::out, initial_deep::in, initial_deep::out)
	is det.

merge_proc_dynamic_multi_slot(SlotNum, Clique, ParentPDPtr, PrimeCSDPtrs0,
		RestSiteArrays, PrimeCSDPtrs, InitDeep0, InitDeep) :-
	lookup_multi_sites(RestSiteArrays, SlotNum, RestCSDPtrLists),
	list__condense([PrimeCSDPtrs0 | RestCSDPtrLists], AllCSDPtrs),
	map__init(ProcMap0),
	list__foldl(cluster_csds_by_ps(InitDeep0), AllCSDPtrs,
		ProcMap0, ProcMap),
	map__values(ProcMap, CSDPtrsClusters),
	list__foldl2(merge_multi_slot_cluster(ParentPDPtr, Clique),
		CSDPtrsClusters, [], PrimeCSDPtrs, InitDeep0, InitDeep).

:- pred merge_multi_slot_cluster(proc_dynamic_ptr::in,
	set(proc_dynamic_ptr)::in, list(call_site_dynamic_ptr)::in,
	list(call_site_dynamic_ptr)::in, list(call_site_dynamic_ptr)::out,
	initial_deep::in, initial_deep::out) is det.

merge_multi_slot_cluster(ParentPDPtr, Clique, ClusterCSDPtrs,
		PrimeCSDPtrs0, PrimeCSDPtrs, InitDeep0, InitDeep) :-
	merge_call_site_dynamics(Clique, ParentPDPtr, ClusterCSDPtrs,
		PrimeCSDPtr, InitDeep0, InitDeep),
	PrimeCSDPtrs = [PrimeCSDPtr | PrimeCSDPtrs0].

:- pred merge_call_site_dynamics(set(proc_dynamic_ptr)::in,
	proc_dynamic_ptr::in, list(call_site_dynamic_ptr)::in,
	call_site_dynamic_ptr::out,
	initial_deep::in, initial_deep::out) is det.

merge_call_site_dynamics(Clique, ParentPDPtr, CandidateCSDPtrs,
		FirstCSDPtr, InitDeep0, InitDeep) :-
	CallSiteDynamics0 = InitDeep0 ^ init_call_site_dynamics,
	list__filter(valid_call_site_dynamic_ptr_raw(CallSiteDynamics0),
		CandidateCSDPtrs, ValidCSDPtrs),
	(
		ValidCSDPtrs = [],
			% This signifies that there is no call here.
		FirstCSDPtr = call_site_dynamic_ptr(0),
		InitDeep = InitDeep0
	;
		ValidCSDPtrs = [FirstCSDPtr | LaterCSDPtrs],
		lookup_call_site_dynamics(CallSiteDynamics0, FirstCSDPtr,
			FirstCSD0),
		FirstCSD = FirstCSD0 ^ csd_caller := ParentPDPtr,
		update_call_site_dynamics(u(CallSiteDynamics0), FirstCSDPtr,
			FirstCSD, CallSiteDynamics),
		InitDeep1 = InitDeep0 ^ init_call_site_dynamics
			:= CallSiteDynamics,
		(
			LaterCSDPtrs = [],
			InitDeep = InitDeep1
		;
			LaterCSDPtrs = [_ | _],
			merge_call_site_dynamics_2(Clique,
				FirstCSDPtr, LaterCSDPtrs, InitDeep1, InitDeep)
		)
	).

:- pred merge_call_site_dynamics_2(set(proc_dynamic_ptr)::in,
	call_site_dynamic_ptr::in, list(call_site_dynamic_ptr)::in,
	initial_deep::in, initial_deep::out) is det.

merge_call_site_dynamics_2(Clique, PrimeCSDPtr, RestCSDPtrs,
		InitDeep0, InitDeep) :-
	record_csd_redirect(RestCSDPtrs, PrimeCSDPtr, InitDeep0, InitDeep1),
	CallSiteDynamics1 = InitDeep1 ^ init_call_site_dynamics,
	lookup_call_site_dynamics(CallSiteDynamics1, PrimeCSDPtr, PrimeCSD1),
	list__map(lookup_call_site_dynamics(CallSiteDynamics1),
		RestCSDPtrs, RestCSDs),
	PrimeOwn1 = PrimeCSD1 ^ csd_own_prof,
	list__foldl(accumulate_csd_owns, RestCSDs, PrimeOwn1, PrimeOwn2),
	PrimeCSD2 = PrimeCSD1 ^ csd_own_prof := PrimeOwn2,
	update_call_site_dynamics(u(CallSiteDynamics1), PrimeCSDPtr, PrimeCSD2,
		CallSiteDynamics2),
	InitDeep2 = InitDeep1 ^ init_call_site_dynamics := CallSiteDynamics2,
	list__filter(callee_in_clique(InitDeep2, Clique), RestCSDPtrs,
		InClique, NotInClique),
	( callee_in_clique(InitDeep2, Clique, PrimeCSDPtr) ->
		require(unify(NotInClique, []),
			"merge_proc_dynamic_normal_slot: prime in clique, others not in clique"),
		InitDeep = InitDeep2
	;
		require(unify(InClique, []),
			"merge_proc_dynamic_normal_slot: prime not in clique, others in clique"),
		merge_call_site_dynamics_descendants(PrimeCSDPtr, RestCSDPtrs,
			InitDeep2, InitDeep)
		% XXX must ensure that PrimeCSDPtr ^ csd_callee is the chosen 
		% ProcDynamic
	).

:- pred merge_call_site_dynamics_descendants(call_site_dynamic_ptr::in,
	list(call_site_dynamic_ptr)::in, initial_deep::in, initial_deep::out)
	is det.

merge_call_site_dynamics_descendants(PrimeCSDPtr, RestCSDPtrs,
		InitDeep0, InitDeep) :-
	CallSiteDynamics = InitDeep0 ^ init_call_site_dynamics,
	lookup_call_site_dynamics(CallSiteDynamics, PrimeCSDPtr, PrimeCSD),
	extract_csd_callee(PrimeCSD, PrimeCSDCallee),
	list__map(lookup_call_site_dynamics(CallSiteDynamics), 
		RestCSDPtrs, RestCSDs),
	list__map(extract_csd_callee, RestCSDs, RestCSDCallees),
	merge_proc_dynamics(set__init, [PrimeCSDCallee | RestCSDCallees],
		InitDeep0, InitDeep).

:- pred lookup_normal_sites(list(array(call_site_array_slot))::in, int::in,
	list(call_site_dynamic_ptr)::out) is det.

lookup_normal_sites([], _, []).
lookup_normal_sites([RestArray | RestArrays], SlotNum, [CSDPtr | CSDPtrs]) :-
	array__lookup(RestArray, SlotNum, Slot),
	(
		Slot = normal(CSDPtr)
	;
		Slot = multi(_),
		error("lookup_normal_sites: found multi")
	),
	lookup_normal_sites(RestArrays, SlotNum, CSDPtrs).

:- pred lookup_multi_sites(list(array(call_site_array_slot))::in, int::in,
	list(list(call_site_dynamic_ptr))::out) is det.

lookup_multi_sites([], _, []).
lookup_multi_sites([RestArray | RestArrays], SlotNum, [CSDList | CSDLists]) :-
	array__lookup(RestArray, SlotNum, Slot),
	(
		Slot = normal(_),
		error("lookup_multi_sites: found normal")
	;
		Slot = multi(CSDArray),
		array__to_list(CSDArray, CSDList)
	),
	lookup_multi_sites(RestArrays, SlotNum, CSDLists).

:- pragma promise_pure(record_pd_redirect/4).
:- pred record_pd_redirect(list(proc_dynamic_ptr)::in, proc_dynamic_ptr::in,
	initial_deep::in, initial_deep::out) is det.

record_pd_redirect(RestPDPtrs, PrimePDPtr, InitDeep0, InitDeep) :-
	impure unsafe_perform_io(io__write_string("pd redirect: ")),
	impure unsafe_perform_io(io__print(RestPDPtrs)),
	impure unsafe_perform_io(io__write_string(" -> ")),
	impure unsafe_perform_io(io__print(PrimePDPtr)),
	impure unsafe_perform_io(io__nl),
	record_pd_redirect_2(RestPDPtrs, PrimePDPtr, InitDeep0, InitDeep).

:- pred record_pd_redirect_2(list(proc_dynamic_ptr)::in, proc_dynamic_ptr::in,
	initial_deep::in, initial_deep::out) is det.

record_pd_redirect_2([], _, InitDeep, InitDeep).
record_pd_redirect_2([RestPDPtr | RestPDPtrs], PrimePDPtr,
		InitDeep0, InitDeep) :-
	ProcDynamics0 = InitDeep0 ^ init_proc_dynamics,
	lookup_proc_dynamics(ProcDynamics0, RestPDPtr, RestPD0),
	require(unify(RestPD0 ^ pd_redirect, no),
		"record_pd_redirect: already redirected"),
	RestPD = RestPD0 ^ pd_redirect := yes(PrimePDPtr),
	update_proc_dynamics(u(ProcDynamics0), RestPDPtr, RestPD,
		ProcDynamics),
	InitDeep1 = InitDeep0 ^ init_proc_dynamics := ProcDynamics,
	record_pd_redirect_2(RestPDPtrs, PrimePDPtr, InitDeep1, InitDeep).

:- pragma promise_pure(record_csd_redirect/4).
:- pred record_csd_redirect(list(call_site_dynamic_ptr)::in,
	call_site_dynamic_ptr::in, initial_deep::in, initial_deep::out) is det.

record_csd_redirect(RestCSDPtrs, PrimeCSDPtr, InitDeep0, InitDeep) :-
	impure unsafe_perform_io(io__write_string("csd redirect: ")),
	impure unsafe_perform_io(io__print(RestCSDPtrs)),
	impure unsafe_perform_io(io__write_string(" -> ")),
	impure unsafe_perform_io(io__print(PrimeCSDPtr)),
	impure unsafe_perform_io(io__nl),
	record_csd_redirect_2(RestCSDPtrs, PrimeCSDPtr, InitDeep0, InitDeep).

:- pred record_csd_redirect_2(list(call_site_dynamic_ptr)::in,
	call_site_dynamic_ptr::in, initial_deep::in, initial_deep::out) is det.

record_csd_redirect_2([], _, InitDeep, InitDeep).
record_csd_redirect_2([RestCSDPtr | RestCSDPtrs], PrimeCSDPtr,
		InitDeep0, InitDeep) :-
	CallSiteDynamics0 = InitDeep0 ^ init_call_site_dynamics,
	lookup_call_site_dynamics(CallSiteDynamics0, RestCSDPtr, RestCSD0),
	require(unify(RestCSD0 ^ csd_redirect, no),
		"record_csd_redirect: already redirected"),
	RestCSD = RestCSD0 ^ csd_redirect := yes(PrimeCSDPtr),
	update_call_site_dynamics(u(CallSiteDynamics0), RestCSDPtr, RestCSD,
		CallSiteDynamics),
	InitDeep1 = InitDeep0 ^ init_call_site_dynamics := CallSiteDynamics,
	record_csd_redirect_2(RestCSDPtrs, PrimeCSDPtr, InitDeep1, InitDeep).

:- pred extract_pd_sites(proc_dynamic::in, array(call_site_array_slot)::out)
	is det.

extract_pd_sites(PD, PD ^ pd_sites).

:- pred extract_csd_callee(call_site_dynamic::in, proc_dynamic_ptr::out)
	is det.

extract_csd_callee(CSD, CSD ^ csd_callee).

:- pred two_or_more(list(proc_dynamic_ptr)::in) is semidet.

two_or_more([_, _ | _]).

:- pred cluster_pds_by_ps(initial_deep::in, proc_dynamic_ptr::in,
	map(proc_static_ptr, list(proc_dynamic_ptr))::in,
	map(proc_static_ptr, list(proc_dynamic_ptr))::out) is det.

cluster_pds_by_ps(InitDeep, PDPtr, ProcMap0, ProcMap) :-
	ProcDynamics = InitDeep ^ init_proc_dynamics,
	( valid_proc_dynamic_ptr_raw(ProcDynamics, PDPtr) ->
		lookup_proc_dynamics(ProcDynamics, PDPtr, PD),
		PSPtr = PD ^ pd_proc_static,
		( map__search(ProcMap0, PSPtr, PDPtrs0) ->
			map__det_update(ProcMap0, PSPtr, [PDPtr | PDPtrs0],
				ProcMap)
		;
			map__det_insert(ProcMap0, PSPtr, [PDPtr], ProcMap)
		)
	;
		ProcMap = ProcMap0
	).

:- pred cluster_csds_by_ps(initial_deep::in, call_site_dynamic_ptr::in,
	map(proc_static_ptr, list(call_site_dynamic_ptr))::in,
	map(proc_static_ptr, list(call_site_dynamic_ptr))::out) is det.

cluster_csds_by_ps(InitDeep, CSDPtr, ProcMap0, ProcMap) :-
	CallSiteDynamics = InitDeep ^ init_call_site_dynamics,
	( valid_call_site_dynamic_ptr_raw(CallSiteDynamics, CSDPtr) ->
		lookup_call_site_dynamics(CallSiteDynamics, CSDPtr, CSD),
		PDPtr = CSD ^ csd_callee,
		ProcDynamics = InitDeep ^ init_proc_dynamics,
		( valid_proc_dynamic_ptr_raw(ProcDynamics, PDPtr) ->
			lookup_proc_dynamics(ProcDynamics, PDPtr, PD),
			PSPtr = PD ^ pd_proc_static
		;
			PSPtr = proc_static_ptr(0)
		),
		( map__search(ProcMap0, PSPtr, CSDPtrs0) ->
			map__det_update(ProcMap0, PSPtr, [CSDPtr | CSDPtrs0],
				ProcMap)
		;
			map__det_insert(ProcMap0, PSPtr, [CSDPtr], ProcMap)
		)
	;
		ProcMap = ProcMap0
	).
