%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This file defines the main data structures of the Mercury deep profiler,
% and predicates for accessing them. The main concern of the access predicates
% is ensuring the safety of array accesses.
%
% For historical reasons, all the top-level arrays (i.e. those directly
% contained in initial_deep and deep structures) have a dummy element
% at index 0; their first real element is at index 1. The arrays in
% proc_static and proc_dynamic structures, being reflections of arrays created
% in C code, start at index 0.

:- module profile.

:- interface.

:- import_module measurements.
:- import_module std_util, array, list, map.

:- type profile_stats --->
	profile_stats(
		max_csd			:: int,
		max_css			:: int,
		max_pd			:: int,
		max_ps			:: int,
		ticks_per_sec		:: int,
		instrument_quanta	:: int,
		user_quanta		:: int
	).

:- type initial_deep --->
	initial_deep(
		init_profile_stats	:: profile_stats,
		init_root		:: proc_dynamic_ptr,
			% The main arrays, each indexed by own xxx_ptr int
		init_call_site_dynamics	:: call_site_dynamics,
		init_proc_dynamics	:: proc_dynamics,
		init_call_site_statics	:: call_site_statics,
		init_proc_statics	:: proc_statics
	).

:- type deep --->
	deep(
		profile_stats		:: profile_stats,
		server_name		:: string,
		data_file_name		:: string,

		root			:: proc_dynamic_ptr,
			% The main arrays, each indexed by own xxx_ptr int
		call_site_dynamics	:: call_site_dynamics,
		proc_dynamics		:: proc_dynamics,
		call_site_statics	:: call_site_statics,
		proc_statics		:: proc_statics,
			% Clique information
		clique_index		:: array(clique_ptr),
					   % index: proc_dynamic_ptr int
		clique_members		:: array(list(proc_dynamic_ptr)),
					   % index: clique_ptr int
		clique_parents		:: array(call_site_dynamic_ptr),
					   % index: clique_ptr int
		clique_maybe_child	:: array(maybe(clique_ptr)),
					   % index: call_site_dynamic_ptr int
			% Reverse links
		proc_callers		:: array(list(call_site_dynamic_ptr)),
					   % index: proc_static_ptr int
		call_site_static_map	:: call_site_static_map,
					   % index: call_site_dynamic_ptr int
		call_site_calls		:: array(map(proc_static_ptr,
						list(call_site_dynamic_ptr))),
					   % index: call_site_static_ptr int
			% Propagated timing info
		pd_own			:: array(own_prof_info),
		pd_desc			:: array(inherit_prof_info),
		csd_desc		:: array(inherit_prof_info),
		ps_own			:: array(own_prof_info),
		ps_desc			:: array(inherit_prof_info),
		css_own			:: array(own_prof_info),
		css_desc		:: array(inherit_prof_info)
	).

%-----------------------------------------------------------------------------%

:- type proc_dynamics == array(proc_dynamic).
:- type proc_statics == array(proc_static).
:- type call_site_dynamics == array(call_site_dynamic).
:- type call_site_statics == array(call_site_static).
:- type call_site_static_map == array(call_site_static_ptr).

:- type proc_dynamic_ptr
	--->	proc_dynamic_ptr(int).

:- type proc_static_ptr
	--->	proc_static_ptr(int).

:- type call_site_dynamic_ptr
	--->	call_site_dynamic_ptr(int).

:- type call_site_static_ptr
	--->	call_site_static_ptr(int).

:- type clique_ptr
	--->	clique_ptr(int).

%-----------------------------------------------------------------------------%

:- type proc_dynamic
	--->	proc_dynamic(
			pd_proc_static	:: proc_static_ptr,
			pd_sites	:: array(call_site_array_slot)
		).

:- type proc_static
	--->	proc_static(
			ps_id		:: proc_id,	% procedure ID
			ps_refined_id	:: string, 	% refined procedure id
			ps_raw_id	:: string, 	% raw procedure id
			ps_filename	:: string, 	% file name
			ps_sites	:: array(call_site_static_ptr)
		).

:- type call_site_dynamic
	--->	call_site_dynamic(
			csd_caller	:: proc_dynamic_ptr,
			csd_callee	:: proc_dynamic_ptr,
			csd_own_prof	:: own_prof_info
		).

:- type call_site_static
	--->	call_site_static(
			css_container	:: proc_static_ptr,
					   % the containing procedure
			css_slot_num	:: int,
					   % slot number in the
					   % containing procedure
			css_kind	:: call_site_kind_and_callee,
			css_line_num	:: int,
			css_goal_path	:: string
		).

%-----------------------------------------------------------------------------%

:- type pred_or_func
	--->	predicate
	;	function.

:- type proc_id
	--->	user_defined(
			user_pred_or_func	:: pred_or_func,
			user_decl_module	:: string,
			user_def_module		:: string,
			user_name		:: string,
			user_arity		:: int,
			user_mode		:: int
		)
	;	compiler_generated(
			comp_gen_type_name	:: string,
			comp_gen_type_module	:: string,
			comp_gen_def_module	:: string,
			comp_gen_pred_name	:: string,
			comp_gen_arity		:: int,
			comp_gen_mode		:: int
		).

:- type call_site_array_slot
	--->	normal(call_site_dynamic_ptr)
	;	multi(array(call_site_dynamic_ptr)).

:- type call_site_kind
	--->	normal_call
	;	special_call
	;	higher_order_call
	;	method_call
	;	callback.

:- type call_site_kind_and_callee
	--->	normal_call(proc_static_ptr, string)
	;	special_call
	;	higher_order_call
	;	method_call
	;	callback.

:- type call_site_callees
	--->	call_site_callees(
			list(proc_dynamic_ptr)
		).

:- type call_site_caller
	--->	call_site_caller(
			call_site_static_ptr
		).

%-----------------------------------------------------------------------------%

:- func dummy_proc_id = proc_id.
:- func main_parent_proc_id = proc_id.

:- pred valid_clique_ptr(deep::in, clique_ptr::in) is semidet.
:- pred valid_proc_dynamic_ptr(deep::in, proc_dynamic_ptr::in) is semidet.
:- pred valid_proc_static_ptr(deep::in, proc_static_ptr::in) is semidet.
:- pred valid_call_site_dynamic_ptr(deep::in, call_site_dynamic_ptr::in)
	is semidet.
:- pred valid_call_site_static_ptr(deep::in, call_site_static_ptr::in)
	is semidet.

:- pred valid_proc_dynamic_ptr_raw(proc_dynamics::in, proc_dynamic_ptr::in)
	is semidet.
:- pred valid_proc_static_ptr_raw(proc_statics::in, proc_static_ptr::in)
	is semidet.
:- pred valid_call_site_dynamic_ptr_raw(call_site_dynamics::in,
	call_site_dynamic_ptr::in) is semidet.
:- pred valid_call_site_static_ptr_raw(call_site_statics::in,
	call_site_static_ptr::in) is semidet.

:- pred lookup_call_site_dynamics(call_site_dynamics::in,
	call_site_dynamic_ptr::in, call_site_dynamic::out) is det.
:- pred lookup_call_site_statics(call_site_statics::in,
	call_site_static_ptr::in, call_site_static::out) is det.
:- pred lookup_proc_dynamics(proc_dynamics::in,
	proc_dynamic_ptr::in, proc_dynamic::out) is det.
:- pred lookup_proc_statics(proc_statics::in,
	proc_static_ptr::in, proc_static::out) is det.
:- pred lookup_clique_index(array(clique_ptr)::in,
	proc_dynamic_ptr::in, clique_ptr::out) is det.
:- pred lookup_clique_members(array(list(proc_dynamic_ptr))::in,
	clique_ptr::in, list(proc_dynamic_ptr)::out) is det.
:- pred lookup_clique_parents(array(call_site_dynamic_ptr)::in,
	clique_ptr::in, call_site_dynamic_ptr::out) is det.
:- pred lookup_clique_maybe_child(array(maybe(clique_ptr))::in,
	call_site_dynamic_ptr::in, maybe(clique_ptr)::out) is det.
:- pred lookup_call_site_static_map(call_site_static_map::in,
	call_site_dynamic_ptr::in, call_site_static_ptr::out) is det.
:- pred lookup_call_site_calls(array(map(proc_static_ptr,
	list(call_site_dynamic_ptr)))::in, call_site_static_ptr::in,
	map(proc_static_ptr, list(call_site_dynamic_ptr))::out) is det.

:- pred deep_lookup_call_site_dynamics(deep::in, call_site_dynamic_ptr::in,
	call_site_dynamic::out) is det.
:- pred deep_lookup_call_site_statics(deep::in, call_site_static_ptr::in,
	call_site_static::out) is det.
:- pred deep_lookup_proc_dynamics(deep::in, proc_dynamic_ptr::in,
	proc_dynamic::out) is det.
:- pred deep_lookup_proc_statics(deep::in, proc_static_ptr::in,
	proc_static::out) is det.
:- pred deep_lookup_clique_index(deep::in, proc_dynamic_ptr::in,
	clique_ptr::out) is det.
:- pred deep_lookup_clique_members(deep::in, clique_ptr::in,
	list(proc_dynamic_ptr)::out) is det.
:- pred deep_lookup_clique_parents(deep::in, clique_ptr::in,
	call_site_dynamic_ptr::out) is det.
:- pred deep_lookup_clique_maybe_child(deep::in, call_site_dynamic_ptr::in,
	maybe(clique_ptr)::out) is det.
:- pred deep_lookup_call_site_static_map(deep::in, call_site_dynamic_ptr::in,
	call_site_static_ptr::out) is det.
:- pred deep_lookup_call_site_calls(deep::in, call_site_static_ptr::in,
	map(proc_static_ptr, list(call_site_dynamic_ptr))::out) is det.
:- pred deep_lookup_proc_dynamic_sites(deep::in, proc_dynamic_ptr::in,
	array(call_site_array_slot)::out) is det.

:- pred deep_lookup_pd_own(deep::in, proc_dynamic_ptr::in,
	own_prof_info::out) is det.
:- pred deep_lookup_pd_desc(deep::in, proc_dynamic_ptr::in,
	inherit_prof_info::out) is det.
:- pred deep_lookup_csd_own(deep::in, call_site_dynamic_ptr::in,
	own_prof_info::out) is det.
:- pred deep_lookup_csd_desc(deep::in, call_site_dynamic_ptr::in,
	inherit_prof_info::out) is det.
:- pred deep_lookup_ps_own(deep::in, proc_static_ptr::in,
	own_prof_info::out) is det.
:- pred deep_lookup_ps_desc(deep::in, proc_static_ptr::in,
	inherit_prof_info::out) is det.
:- pred deep_lookup_css_own(deep::in, call_site_static_ptr::in,
	own_prof_info::out) is det.
:- pred deep_lookup_css_desc(deep::in, call_site_static_ptr::in,
	inherit_prof_info::out) is det.

:- pred update_call_site_dynamics(call_site_dynamics::array_di,
	call_site_dynamic_ptr::in, call_site_dynamic::in,
	call_site_dynamics::array_uo) is det.
:- pred update_call_site_statics(call_site_statics::array_di,
	call_site_static_ptr::in, call_site_static::in,
	call_site_statics::array_uo) is det.
:- pred update_proc_dynamics(proc_dynamics::array_di,
	proc_dynamic_ptr::in, proc_dynamic::in,
	proc_dynamics::array_uo) is det.
:- pred update_proc_statics(proc_statics::array_di,
	proc_static_ptr::in, proc_static::in, proc_statics::array_uo) is det.
:- pred update_call_site_static_map(call_site_static_map::array_di,
	call_site_dynamic_ptr::in, call_site_static_ptr::in,
	call_site_static_map::array_uo) is det.

:- pred deep_update_csd_desc(deep::in, call_site_dynamic_ptr::in,
	inherit_prof_info::in, deep::out) is det.
:- pred deep_update_pd_desc(deep::in, proc_dynamic_ptr::in,
	inherit_prof_info::in, deep::out) is det.
:- pred deep_update_pd_own(deep::in, proc_dynamic_ptr::in,
	own_prof_info::in, deep::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array_util.
:- import_module int, require.

dummy_proc_id = user_defined(predicate, "unknown", "unknown", "unknown",
	-1, -1).

main_parent_proc_id = user_defined(predicate, "mercury_runtime",
	"mercury_runtime", "main_parent", 0, 0).

%-----------------------------------------------------------------------------%

valid_clique_ptr(Deep, clique_ptr(CliqueNum)) :-
	CliqueNum > 0,
	array__in_bounds(Deep ^ clique_members, CliqueNum).

valid_proc_dynamic_ptr(Deep, proc_dynamic_ptr(PDI)) :-
	PDI > 0,
	array__in_bounds(Deep ^ proc_dynamics, PDI).

valid_proc_static_ptr(Deep, proc_static_ptr(PSI)) :-
	PSI > 0,
	array__in_bounds(Deep ^ proc_statics, PSI).

valid_call_site_dynamic_ptr(Deep, call_site_dynamic_ptr(CSDI)) :-
	CSDI > 0,
	array__in_bounds(Deep ^ call_site_dynamics, CSDI).

valid_call_site_static_ptr(Deep, call_site_static_ptr(CSSI)) :-
	CSSI > 0,
	array__in_bounds(Deep ^ call_site_statics, CSSI).

%-----------------------------------------------------------------------------%

valid_proc_dynamic_ptr_raw(ProcDynamics, proc_dynamic_ptr(PDI)) :-
	PDI > 0,
	array__in_bounds(ProcDynamics, PDI).

valid_proc_static_ptr_raw(ProcStatics, proc_static_ptr(PSI)) :-
	PSI > 0,
	array__in_bounds(ProcStatics, PSI).

valid_call_site_dynamic_ptr_raw(CallSiteDynamics,
		call_site_dynamic_ptr(CSDI)) :-
	CSDI > 0,
	array__in_bounds(CallSiteDynamics, CSDI).

valid_call_site_static_ptr_raw(CallSiteStatics, call_site_static_ptr(CSSI)) :-
	CSSI > 0,
	array__in_bounds(CallSiteStatics, CSSI).

%-----------------------------------------------------------------------------%

lookup_call_site_dynamics(CallSiteDynamics, CSDPtr, CSD) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	( CSDI > 0, array__in_bounds(CallSiteDynamics, CSDI) ->
		array__lookup(CallSiteDynamics, CSDI, CSD)
	;
		error("lookup_call_site_dynamics: bounds error")
	).

lookup_call_site_statics(CallSiteStatics, CSSPtr, CSS) :-
	CSSPtr = call_site_static_ptr(CSSI),
	( CSSI > 0, array__in_bounds(CallSiteStatics, CSSI) ->
		array__lookup(CallSiteStatics, CSSI, CSS)
	;
		error("lookup_call_site_statics: bounds error")
	).

lookup_proc_dynamics(ProcDynamics, PDPtr, PD) :-
	PDPtr = proc_dynamic_ptr(PDI),
	( PDI > 0, array__in_bounds(ProcDynamics, PDI) ->
		array__lookup(ProcDynamics, PDI, PD)
	;
		error("lookup_proc_dynamics: bounds error")
	).

lookup_proc_statics(ProcStatics, PSPtr, PS) :-
	PSPtr = proc_static_ptr(PSI),
	( PSI > 0, array__in_bounds(ProcStatics, PSI) ->
		array__lookup(ProcStatics, PSI, PS)
	;
		error("lookup_proc_statics: bounds error")
	).

lookup_clique_index(CliqueIndex, PDPtr, CliquePtr) :-
	PDPtr = proc_dynamic_ptr(PDI),
	( PDI > 0, array__in_bounds(CliqueIndex, PDI) ->
		array__lookup(CliqueIndex, PDI, CliquePtr)
	;
		error("lookup_clique_index: bounds error")
	).

lookup_clique_members(CliqueMembers, CliquePtr, PDPtrs) :-
	CliquePtr = clique_ptr(CI),
	( array__in_bounds(CliqueMembers, CI) ->
		array__lookup(CliqueMembers, CI, PDPtrs)
	;
		error("lookup_clique_members: bounds error")
	).

lookup_clique_parents(CliqueParents, CliquePtr, CSDPtr) :-
	CliquePtr = clique_ptr(CI),
	( array__in_bounds(CliqueParents, CI) ->
		array__lookup(CliqueParents, CI, CSDPtr)
	;
		error("lookup_clique_parents: bounds error")
	).

lookup_clique_maybe_child(CliqueMaybeChild, CSDPtr, MaybeCliquePtr) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	( CSDI > 0, array__in_bounds(CliqueMaybeChild, CSDI) ->
		array__lookup(CliqueMaybeChild, CSDI, MaybeCliquePtr)
	;
		error("lookup_clique_maybe_child: bounds error")
	).

lookup_call_site_static_map(CallSiteStaticMap, CSDPtr, CSSPtr) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	( CSDI > 0, array__in_bounds(CallSiteStaticMap, CSDI) ->
		array__lookup(CallSiteStaticMap, CSDI, CSSPtr)
	;
		error("lookup_call_site_static_map: bounds error")
	).

lookup_call_site_calls(CallSiteCalls, CSSPtr, Calls) :-
	CSSPtr = call_site_static_ptr(CSSI),
	( CSSI > 0, array__in_bounds(CallSiteCalls, CSSI) ->
		array__lookup(CallSiteCalls, CSSI, Calls)
	;
		error("lookup_call_site_static_map: bounds error")
	).

%-----------------------------------------------------------------------------%

deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD) :-
	lookup_call_site_dynamics(Deep ^ call_site_dynamics, CSDPtr, CSD).

deep_lookup_call_site_statics(Deep, CSSPtr, CSS) :-
	lookup_call_site_statics(Deep ^ call_site_statics, CSSPtr, CSS).

deep_lookup_proc_dynamics(Deep, PDPtr, PD) :-
	lookup_proc_dynamics(Deep ^ proc_dynamics, PDPtr, PD).

deep_lookup_proc_statics(Deep, PSPtr, PS) :-
	lookup_proc_statics(Deep ^ proc_statics, PSPtr, PS).

deep_lookup_clique_index(Deep, PDPtr, CliquePtr) :-
	lookup_clique_index(Deep ^ clique_index, PDPtr, CliquePtr).

deep_lookup_clique_members(Deep, CliquePtr, PDPtrs) :-
	lookup_clique_members(Deep ^ clique_members, CliquePtr, PDPtrs).

deep_lookup_clique_parents(Deep, CliquePtr, CSDPtr) :-
	lookup_clique_parents(Deep ^ clique_parents, CliquePtr, CSDPtr).

deep_lookup_clique_maybe_child(Deep, CSDPtr, MaybeCliquePtr) :-
	lookup_clique_maybe_child(Deep ^ clique_maybe_child, CSDPtr,
		MaybeCliquePtr).

deep_lookup_call_site_static_map(Deep, CSDPtr, CSSPtr) :-
	lookup_call_site_static_map(Deep ^ call_site_static_map, CSDPtr,
		CSSPtr).

deep_lookup_call_site_calls(Deep, CSSPtr, Calls) :-
	lookup_call_site_calls(Deep ^ call_site_calls, CSSPtr, Calls).

deep_lookup_proc_dynamic_sites(Deep, PDPtr, PDSites) :-
	deep_lookup_proc_dynamics(Deep, PDPtr, PD),
	PDSites = PD ^ pd_sites.

%-----------------------------------------------------------------------------%

deep_lookup_pd_own(Deep, PDPtr, Own) :-
	PDPtr = proc_dynamic_ptr(PDI),
	array__lookup(Deep ^ pd_own, PDI, Own).

deep_lookup_pd_desc(Deep, PDPtr, Desc) :-
	PDPtr = proc_dynamic_ptr(PDI),
	array__lookup(Deep ^ pd_desc, PDI, Desc).

deep_lookup_csd_own(Deep, CSDPtr, Own) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	array__lookup(Deep ^ call_site_dynamics, CSDI, CSD),
	Own = CSD ^ csd_own_prof.

deep_lookup_csd_desc(Deep, CSDPtr, Desc) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	array__lookup(Deep ^ csd_desc, CSDI, Desc).

deep_lookup_ps_own(Deep, PSPtr, Own) :-
	PSPtr = proc_static_ptr(PSI),
	array__lookup(Deep ^ ps_own, PSI, Own).

deep_lookup_ps_desc(Deep, PSPtr, Desc) :-
	PSPtr = proc_static_ptr(PSI),
	array__lookup(Deep ^ ps_desc, PSI, Desc).

deep_lookup_css_own(Deep, CSSPtr, Own) :-
	CSSPtr = call_site_static_ptr(CSSI),
	array__lookup(Deep ^ css_own, CSSI, Own).

deep_lookup_css_desc(Deep, CSSPtr, Desc) :-
	CSSPtr = call_site_static_ptr(CSSI),
	array__lookup(Deep ^ css_desc, CSSI, Desc).

%-----------------------------------------------------------------------------%

update_call_site_dynamics(CallSiteDynamics0, CSDPtr, CSD, CallSiteDynamics) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	array__set(CallSiteDynamics0, CSDI, CSD, CallSiteDynamics).

update_call_site_statics(CallSiteStatics0, CSSPtr, CSS, CallSiteStatics) :-
	CSSPtr = call_site_static_ptr(CSSI),
	array__set(CallSiteStatics0, CSSI, CSS, CallSiteStatics).

update_proc_dynamics(ProcDynamics0, PDPtr, PD, ProcDynamics) :-
	PDPtr = proc_dynamic_ptr(PDI),
	array__set(ProcDynamics0, PDI, PD, ProcDynamics).

update_proc_statics(ProcStatics0, PSPtr, PS, ProcStatics) :-
	PSPtr = proc_static_ptr(PSI),
	array__set(ProcStatics0, PSI, PS, ProcStatics).

update_call_site_static_map(CallSiteStaticMap0, CSDPtr, CSSPtr,
		CallSiteStaticMap) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	array__set(CallSiteStaticMap0, CSDI, CSSPtr, CallSiteStaticMap).

%-----------------------------------------------------------------------------%

deep_update_csd_desc(Deep0, CSDPtr, CSDDesc, Deep) :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	array__set(u(Deep0 ^ csd_desc), CSDI, CSDDesc, CSDDescs),
	Deep = Deep0 ^ csd_desc := CSDDescs.

deep_update_pd_desc(Deep0, PDPtr, PDDesc, Deep) :-
	PDPtr = proc_dynamic_ptr(PDI),
	array__set(u(Deep0 ^ pd_desc), PDI, PDDesc, PDDescs),
	Deep = Deep0 ^ pd_desc := PDDescs.

deep_update_pd_own(Deep0, PDPtr, PDOwn, Deep) :-
	PDPtr = proc_dynamic_ptr(PDI),
	array__set(u(Deep0 ^ pd_own), PDI, PDOwn, PDOwns),
	Deep = Deep0 ^ pd_own := PDOwns.

%-----------------------------------------------------------------------------%
