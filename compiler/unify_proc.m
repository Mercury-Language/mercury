%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% unify_proc.nl: 
%
%	This module encapsulates access to the unify_requests table,
%	and handles code generation for out-of-line complicated
%	unification procedures.
%
% Each time we come to generate code for a complicated unification, the
% compiler just generates a call to an out-of-line unification procedure
% (this is done in call_gen.nl), and records in the `unify_requests' table
% that we need to eventually generate code for that out-of-line procedure.
% After we've generated code for all the ordinary predicates, we then
% generate code for the out-of-line unification procedures.  Note that
% unification procedures may call other unification procedures which have
% not yet been enountered, causing new entries to be added to the
% unify_requests table.  We store the entries in a queue and continue the
% process until the queue is empty.
%
% Currently if the same complicated unification procedure is called by
% different modules, each module will end up with a copy of the code for
% that procedure.  In the long run it would be desireable to either delay
% generation of complicated unification procedures until link time (like
% Cfront) or to have a smart linker which could merge duplicate
% definitions (like Borland C++).  However the amount of code duplication
% involved is probably very small, so it's definitely not worth
% worrying about right now.

%-----------------------------------------------------------------------------%

:- module unify_proc.
:- interface.
:- import_module std_util, io, list.
:- import_module prog_io, hlds, llds.

:- type unify_requests.

:- type unify_proc_id == pair(type_id, uni_mode).

:- type unify_proc_num == int.

	% Initialize the unify_requests table.

:- pred unify_proc__init_requests(unify_requests).
:- mode unify_proc__init_requests(out) is det.

	% Add a new request to the unify_requests table.

:- pred unify_proc__request_unify(unify_proc_id, unify_requests,
				unify_requests).
:- mode unify_proc__request_unify(in, in, out) is det.

	% Generate code for the unification procedures which have been
	% requested.

:- pred unify_proc__generate_unification_procs(module_info, unify_requests,
		list(c_procedure), io__state, io__state).
:- mode unify_proc__generate_unification_procs(in, in, out, di, uo) is det.

:- pred unify_proc__lookup_num(unify_requests, type_id, uni_mode,
				unify_proc_num).
:- mode unify_proc__lookup_num(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module tree, map, queue, int.
:- import_module code_util, code_info, type_util, varset.
:- import_module mercury_to_mercury, hlds_out.
:- import_module globals, options.

	% We keep track of all the complicated unification procs we need
	% by storing them in the unify_requests structure.
	% We assign a unique unify_proc_num to each one.

:- type req_map == map(unify_proc_id, unify_proc_num).

:- type req_queue == queue(unify_proc_id).

:- type unify_requests --->
		unify_requests(
			unify_proc_num,		% next unused number
			req_map,		% the assignment of numbers
						% to unify_proc_ids
			req_queue		% queue of procs we still need
						% to generate code for
		).

%-----------------------------------------------------------------------------%

unify_proc__init_requests(Requests) :-
	map__init(ReqMap),
	queue__init(ReqQueue),
	Requests = unify_requests(0, ReqMap, ReqQueue).

%-----------------------------------------------------------------------------%

	% Boring access predicates

:- pred unify_proc__get_num(unify_requests, unify_proc_num).
:- mode unify_proc__get_num(in, out) is det.

:- pred unify_proc__get_req_map(unify_requests, req_map).
:- mode unify_proc__get_req_map(in, out) is det.

:- pred unify_proc__get_req_queue(unify_requests, req_queue).
:- mode unify_proc__get_req_queue(in, out) is det.

:- pred unify_proc__set_num(unify_requests, unify_proc_num, unify_requests).
:- mode unify_proc__set_num(in, in, out) is det.

:- pred unify_proc__set_req_map(unify_requests, req_map, unify_requests).
:- mode unify_proc__set_req_map(in, in, out) is det.

:- pred unify_proc__set_req_queue(unify_requests, req_queue, unify_requests).
:- mode unify_proc__set_req_queue(in, in, out) is det.

unify_proc__get_num(unify_requests(Num, _, _), Num).

unify_proc__get_req_map(unify_requests(_, ReqMap, _), ReqMap).

unify_proc__get_req_queue(unify_requests(_, _, ReqQueue), ReqQueue).

unify_proc__set_num(unify_requests(_, B, C), Num,
			unify_requests(Num, B, C)).

unify_proc__set_req_map(unify_requests(A, _, C), ReqMap,
			unify_requests(A, ReqMap, C)).

unify_proc__set_req_queue(unify_requests(A, B, _), ReqQueue,
			unify_requests(A, B, ReqQueue)).

%-----------------------------------------------------------------------------%

unify_proc__lookup_num(Requests, TypeId, UniMode, Num) :-
	unify_proc__get_req_map(Requests, ReqMap),
	map__lookup(ReqMap, TypeId - UniMode, Num).

%-----------------------------------------------------------------------------%

unify_proc__request_unify(UnifyId, Requests0, Requests) :-
	unify_proc__get_req_map(Requests0, ReqMap0),
	Requests0 = unify_requests(Num0, ReqMap0, ReqQueue0),
	( map__contains(ReqMap0, UnifyId) ->
		Requests = Requests0
	;
		map__set(ReqMap0, UnifyId, Num0, ReqMap),
		unify_proc__set_req_map(Requests0, ReqMap, Requests1),

		unify_proc__get_num(Requests1, Num0),
		Num is Num0 + 1,
		unify_proc__set_num(Requests1, Num, Requests2),

		unify_proc__get_req_queue(Requests1, ReqQueue0),
		queue__put(ReqQueue0, UnifyId, ReqQueue),
		unify_proc__set_req_queue(Requests2, ReqQueue, Requests)
	).

%-----------------------------------------------------------------------------%

unify_proc__generate_unification_procs(ModuleInfo, Requests0, UnifyProcs) -->
	{ unify_proc__get_req_queue(Requests0, RequestQueue0) },
	(
		{ queue__get(RequestQueue0, UnifyProcId, RequestQueue1) }
	->
		{ unify_proc__set_req_queue(Requests0, RequestQueue1,
			Requests1) },
		globals__io_lookup_bool_option(very_verbose, VeryVerbose),
		( { VeryVerbose = yes } ->
			io__write_string("% Generating code for "),
			unify_proc__write_unify_proc_id(UnifyProcId),
			io__write_string("\n")
		;
			[]
		),
		{ unify_proc__generate_unification(UnifyProcId, ModuleInfo,
			Requests1, Requests2, UnifyProc) },
		unify_proc__generate_unification_procs(ModuleInfo, Requests2,
			UnifyProcs0),
		{ UnifyProcs = [UnifyProc | UnifyProcs0] }
	;
		{ UnifyProcs = [] }
	).

:- pred unify_proc__generate_unification(unify_proc_id, module_info,
				unify_requests, unify_requests, c_procedure).
:- mode unify_proc__generate_unification(in, in, in, out, out) is det.

unify_proc__generate_unification(UnifyProcId, ModuleInfo, Requests0,
				Requests, UnifyProc) :-
	unify_proc__get_req_map(Requests0, ReqMap),
	map__lookup(ReqMap, UnifyProcId, UnifyModeNum),
	UnifyProcId = TypeId - _,
	code_util__make_uni_label(ModuleInfo, TypeId, UnifyModeNum, UniLabel),
	type_util__type_id_name(ModuleInfo, TypeId, TypeName),
	type_util__type_id_arity(ModuleInfo, TypeId, TypeArity),
		%
		% XXX this is a stub!
		%
	UnifyProc = c_procedure(TypeName, TypeArity, UnifyModeNum,
	[
		unilabel(UniLabel) -
			"Entry point for out-of-line unification predicate",
		c_code("abort();") -
			"Panic - complicated unifies not yet implemented!"
	]),
	Requests = Requests0.

%-----------------------------------------------------------------------------%

:- pred unify_proc__write_unify_proc_id(unify_proc_id, io__state, io__state).
:- mode unify_proc__write_unify_proc_id(in, di, uo) is det.

unify_proc__write_unify_proc_id(TypeId - UniMode) -->
	io__write_string("unification procedure for type `"),
	hlds_out__write_type_id(TypeId),
	io__write_string("' with initial insts `"),
	{ UniMode = ((InstA - InstB) -> _FinalInst) },
	{ varset__init(InstVarSet) },
	mercury_output_inst(InstA, InstVarSet),
	io__write_string("', `"),
	mercury_output_inst(InstB, InstVarSet),
	io__write_string("'").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
