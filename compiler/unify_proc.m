%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% unify_proc.m: 
%
%	This module encapsulates access to the proc_requests table,
%	and constructs the clauses for out-of-line complicated
%	unification procedures.
%	It also generates the code for other compiler-generated type-specific
%	predicates such as compare/3.
%
% During mode analysis, we notice each different complicated unification
% that occurs.  For each one we add a new mode to the out-of-line
% unification predicate for that type, and we record in the `proc_requests'
% table that we need to eventually modecheck that mode of the unification
% procedure.
%
% After we've done mode analysis for all the ordinary predicates, we then
% do mode analysis for the out-of-line unification procedures.  Note that
% unification procedures may call other unification procedures which have
% not yet been encountered, causing new entries to be added to the
% proc_requests table.  We store the entries in a queue and continue the
% process until the queue is empty.
%
% The same queuing mechanism is also used for procedures created by
% mode inference during mode analysis and unique mode analysis.
%
% Currently if the same complicated unification procedure is called by
% different modules, each module will end up with a copy of the code for
% that procedure.  In the long run it would be desireable to either delay
% generation of complicated unification procedures until link time (like
% Cfront does with C++ templates) or to have a smart linker which could
% merge duplicate definitions (like Borland C++).  However the amount of
% code duplication involved is probably very small, so it's definitely not
% worth worrying about right now.

% XXX What about complicated unification of an abstract type in a partially
% instantiated mode?  Currently we don't implement it correctly. Probably 
% it should be disallowed, but we should issue a proper error message.

%-----------------------------------------------------------------------------%

:- module unify_proc.

:- interface.
:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data.
:- import_module mode_info, prog_data, special_pred.
:- import_module bool, std_util, io, list.

:- type proc_requests.

:- type unify_proc_id == pair(type_id, uni_mode).

	% Initialize the proc_requests table.

:- pred unify_proc__init_requests(proc_requests::out) is det.

	% Add a new request for a unification procedure to the
	% proc_requests table.

:- pred unify_proc__request_unify(unify_proc_id::in, inst_varset::in,
	determinism::in, prog_context::in, module_info::in, module_info::out)
	is det.

% Add a new request for a procedure (not necessarily a unification)
% to the request queue.  Return the procedure's newly allocated
% proc_id.  (This is used by unique_modes.m.)

:- pred unify_proc__request_proc(pred_id::in, list(mode)::in, inst_varset::in,
maybe(list(is_live))::in, maybe(determinism)::in, prog_context::in,
module_info::in, proc_id::out, module_info::out) is det.

% unify_proc__add_lazily_generated_unify_pred(TypeId,
	%	UnifyPredId_for_Type, ModuleInfo0, ModuleInfo).
	%
	% For most imported unification procedures, we delay
	% generating declarations and clauses until we know
	% whether they are actually needed because there
	% is a complicated unification involving the type.
	% This predicate is exported for use by higher_order.m
	% when it is specializing calls to unify/2.
:- pred unify_proc__add_lazily_generated_unify_pred(type_id::in,
	pred_id::out, module_info::in, module_info::out) is det.
	
	% unify_proc__add_lazily_generated_compare_pred_decl(TypeId,
	%	ComparePredId_for_Type, ModuleInfo0, ModuleInfo).
	%
	% Add declarations, but not clauses, for a compare or index predicate.
:- pred unify_proc__add_lazily_generated_compare_pred_decl(type_id::in,
	pred_id::out, module_info::in, module_info::out) is det.

	% Do mode analysis of the queued procedures.
	% If the first argument is `unique_mode_check',
	% then also go on and do full determinism analysis and unique mode
	% analysis on them as well.
	% The pred_table arguments are used to store copies of the
	% procedure bodies before unique mode analysis, so that
	% we can restore them before doing the next analysis pass.

:- pred modecheck_queued_procs(how_to_check_goal::in,
	pred_table::in, module_info::in, pred_table::out, module_info::out,
	bool::out, io__state::di, io__state::uo) is det.

	% Given the type and mode of a unification, look up the
	% mode number for the unification proc.

:- pred unify_proc__lookup_mode_num(module_info::in, type_id::in, uni_mode::in,
	determinism::in, proc_id::out) is det.

	% Generate the clauses for one of the compiler-generated
	% special predicates (compare/3, index/3, unify, etc.)

:- pred unify_proc__generate_clause_info(special_pred_id::in, (type)::in,
	hlds_type_body::in, prog_context::in, module_info::in,
	clauses_info::out) is det.

	% This number gives the maximum number of constructors in a type
	% whose compare procedure can be specialized, and whose compare
	% procedure therefore does need an index procedure on that type.

:- func unify_proc__max_exploited_compare_spec_value = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals, options.
:- import_module code_util, code_info, type_util.
:- import_module mercury_to_mercury, hlds_out.
:- import_module make_hlds, polymorphism, post_typecheck, prog_util, prog_out.
:- import_module quantification, clause_to_proc, term, varset.
:- import_module modes, mode_util, inst_match, instmap, (inst).
:- import_module switch_detection, cse_detection, det_analysis, unique_modes.
:- import_module recompilation.

:- import_module tree, map, set, queue, int, string, require, assoc_list.

	% We keep track of all the complicated unification procs we need
	% by storing them in the proc_requests structure.
	% For each unify_proc_id (i.e. type & mode), we store the proc_id
	% (mode number) of the unification procedure which corresponds to
	% that mode.

:- type unify_req_map == map(unify_proc_id, proc_id).

:- type req_queue == queue(pred_proc_id).

:- type proc_requests --->
		proc_requests(
			unify_req_map,		% the assignment of proc_id
						% numbers to unify_proc_ids
			req_queue		% queue of procs we still need
						% to generate code for
		).

%-----------------------------------------------------------------------------%

unify_proc__init_requests(Requests) :-
	map__init(UnifyReqMap),
	queue__init(ReqQueue),
	Requests = proc_requests(UnifyReqMap, ReqQueue).

%-----------------------------------------------------------------------------%

	% Boring access predicates

:- pred unify_proc__get_unify_req_map(proc_requests, unify_req_map).
:- mode unify_proc__get_unify_req_map(in, out) is det.

:- pred unify_proc__get_req_queue(proc_requests, req_queue).
:- mode unify_proc__get_req_queue(in, out) is det.

:- pred unify_proc__set_unify_req_map(proc_requests, unify_req_map,
					proc_requests).
:- mode unify_proc__set_unify_req_map(in, in, out) is det.

:- pred unify_proc__set_req_queue(proc_requests, req_queue, proc_requests).
:- mode unify_proc__set_req_queue(in, in, out) is det.

unify_proc__get_unify_req_map(proc_requests(UnifyReqMap, _), UnifyReqMap).

unify_proc__get_req_queue(proc_requests(_, ReqQueue), ReqQueue).

unify_proc__set_unify_req_map(proc_requests(_, B), UnifyReqMap,
			proc_requests(UnifyReqMap, B)).

unify_proc__set_req_queue(proc_requests(A, _), ReqQueue,
			proc_requests(A, ReqQueue)).

%-----------------------------------------------------------------------------%

unify_proc__lookup_mode_num(ModuleInfo, TypeId, UniMode, Det, Num) :-
	( unify_proc__search_mode_num(ModuleInfo, TypeId, UniMode, Det, Num1) ->
		Num = Num1
	;
		error("unify_proc.m: unify_proc__search_num failed")
	).

:- pred unify_proc__search_mode_num(module_info, type_id, uni_mode, determinism,
					proc_id).
:- mode unify_proc__search_mode_num(in, in, in, in, out) is semidet.

	% Given the type, mode, and determinism of a unification, look up the
	% mode number for the unification proc.
	% We handle semidet unifications with mode (in, in) specially - they
	% are always mode zero.  Similarly for unifications of `any' insts.
	% (It should be safe to use the `in, in' mode for any insts, since
	% we assume that `ground' and `any' have the same representation.)
	% For unreachable unifications, we also use mode zero.

unify_proc__search_mode_num(ModuleInfo, TypeId, UniMode, Determinism, ProcId) :-
	UniMode = (XInitial - YInitial -> _Final),
	(
		Determinism = semidet,
		inst_is_ground_or_any(ModuleInfo, XInitial),
		inst_is_ground_or_any(ModuleInfo, YInitial)
	->
		hlds_pred__in_in_unification_proc_id(ProcId)
	;
		XInitial = not_reached
	->
		hlds_pred__in_in_unification_proc_id(ProcId)
	;
		YInitial = not_reached
	->
		hlds_pred__in_in_unification_proc_id(ProcId)
	;
		module_info_get_proc_requests(ModuleInfo, Requests),
		unify_proc__get_unify_req_map(Requests, UnifyReqMap),
		map__search(UnifyReqMap, TypeId - UniMode, ProcId)
	).

%-----------------------------------------------------------------------------%

unify_proc__request_unify(UnifyId, InstVarSet, Determinism, Context,
		ModuleInfo0, ModuleInfo) :-
	UnifyId = TypeId - UnifyMode,

	%
	% Generating a unification procedure for a type uses its body.
	%
	module_info_get_maybe_recompilation_info(ModuleInfo0, MaybeRecompInfo0),
	( MaybeRecompInfo0 = yes(RecompInfo0) ->
		recompilation__record_used_item(type_body, 
			TypeId, TypeId, RecompInfo0, RecompInfo),
		module_info_set_maybe_recompilation_info(ModuleInfo0,
			yes(RecompInfo), ModuleInfo1)
	;
		ModuleInfo1 = ModuleInfo0
	),

	%
	% check if this unification has already been requested, or
	% if the proc is hand defined.
	%
	(
		(
			unify_proc__search_mode_num(ModuleInfo1, TypeId,
				UnifyMode, Determinism, _)
		; 
			TypeId = TypeName - _TypeArity,
			TypeName = qualified(TypeModuleName, _),
			module_info_name(ModuleInfo1, ModuleName),
			ModuleName = TypeModuleName,
			module_info_types(ModuleInfo1, TypeTable),
			map__search(TypeTable, TypeId, TypeDefn),
			hlds_data__get_type_defn_body(TypeDefn, TypeBody),
			TypeBody = abstract_type
		; 
			type_id_has_hand_defined_rtti(TypeId)
		)
	->
		ModuleInfo = ModuleInfo1
	;
		%
		% lookup the pred_id for the unification procedure
		% that we are going to generate
		%
		module_info_get_special_pred_map(ModuleInfo1, SpecialPredMap),
		( map__search(SpecialPredMap, unify - TypeId, PredId0) ->
			PredId = PredId0,
			ModuleInfo2 = ModuleInfo1
		;
			% We generate unification predicates for most
			% imported types lazily, so add the declarations
			% and clauses now.
			unify_proc__add_lazily_generated_unify_pred(TypeId,
				PredId, ModuleInfo1, ModuleInfo2)
		),

		% convert from `uni_mode' to `list(mode)'
		UnifyMode = ((X_Initial - Y_Initial) -> (X_Final - Y_Final)),
		ArgModes0 = [(X_Initial -> X_Final), (Y_Initial -> Y_Final)],

		% for polymorphic types, add extra modes for the type_infos
		in_mode(InMode),
		TypeId = _ - TypeArity,
		list__duplicate(TypeArity, InMode, TypeInfoModes),
		list__append(TypeInfoModes, ArgModes0, ArgModes),

		ArgLives = no,  % XXX ArgLives should be part of the UnifyId

		unify_proc__request_proc(PredId, ArgModes, InstVarSet, ArgLives,
			yes(Determinism), Context, ModuleInfo2,
			ProcId, ModuleInfo3),

		%
		% save the proc_id for this unify_proc_id
		%
		module_info_get_proc_requests(ModuleInfo3, Requests0),
		unify_proc__get_unify_req_map(Requests0, UnifyReqMap0),
		map__set(UnifyReqMap0, UnifyId, ProcId, UnifyReqMap),
		unify_proc__set_unify_req_map(Requests0, UnifyReqMap, Requests),
		module_info_set_proc_requests(ModuleInfo3, Requests,
			ModuleInfo)
	).

unify_proc__request_proc(PredId, ArgModes, InstVarSet, ArgLives, MaybeDet,
		Context, ModuleInfo0, ProcId, ModuleInfo) :-
	%
	% create a new proc_info for this procedure
	%
	module_info_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	list__length(ArgModes, Arity),
	DeclaredArgModes = no,
	add_new_proc(PredInfo0, InstVarSet, Arity, ArgModes, DeclaredArgModes,
		ArgLives, MaybeDet, Context, address_is_not_taken,
		PredInfo1, ProcId),

	%
	% copy the clauses for the procedure from the pred_info to the
	% proc_info, and mark the procedure as one that cannot
	% be processed yet
	%
	pred_info_procedures(PredInfo1, Procs1),
	pred_info_clauses_info(PredInfo1, ClausesInfo),
	map__lookup(Procs1, ProcId, ProcInfo0),
	proc_info_set_can_process(ProcInfo0, no, ProcInfo1),

	copy_clauses_to_proc(ProcId, ClausesInfo, ProcInfo1, ProcInfo2),

	proc_info_goal(ProcInfo2, Goal0),
	set_goal_contexts(Context, Goal0, Goal),
	proc_info_set_goal(ProcInfo2, Goal, ProcInfo),
	map__det_update(Procs1, ProcId, ProcInfo, Procs2),
	pred_info_set_procedures(PredInfo1, Procs2, PredInfo2),
	map__det_update(Preds0, PredId, PredInfo2, Preds2),
	module_info_set_preds(ModuleInfo0, Preds2, ModuleInfo2),

	%
	% insert the pred_proc_id into the request queue
	%
	module_info_get_proc_requests(ModuleInfo2, Requests0),
	unify_proc__get_req_queue(Requests0, ReqQueue0),
	queue__put(ReqQueue0, proc(PredId, ProcId), ReqQueue),
	unify_proc__set_req_queue(Requests0, ReqQueue, Requests),
	module_info_set_proc_requests(ModuleInfo2, Requests, ModuleInfo).

%-----------------------------------------------------------------------------%

	% XXX these belong in modes.m

modecheck_queued_procs(HowToCheckGoal, OldPredTable0, ModuleInfo0,
		OldPredTable, ModuleInfo, Changed) -->
	{ module_info_get_proc_requests(ModuleInfo0, Requests0) },
	{ unify_proc__get_req_queue(Requests0, RequestQueue0) },
	(
		{ queue__get(RequestQueue0, PredProcId, RequestQueue1) }
	->
		{ unify_proc__set_req_queue(Requests0, RequestQueue1,
			Requests1) },
		{ module_info_set_proc_requests(ModuleInfo0, Requests1,
			ModuleInfo1) },
		%
		% Check that the procedure is valid (i.e. type-correct),
		% before we attempt to do mode analysis on it.
		% This check is necessary to avoid internal errors
		% caused by doing mode analysis on type-incorrect code.
		% XXX inefficient! This is O(N*M).
		%
		{ PredProcId = proc(PredId, _ProcId) },
		{ module_info_predids(ModuleInfo1, ValidPredIds) },
		( { list__member(PredId, ValidPredIds) } ->
			queued_proc_progress_message(PredProcId,
				HowToCheckGoal, ModuleInfo1),
			modecheck_queued_proc(HowToCheckGoal, PredProcId,
				OldPredTable0, ModuleInfo1, 
				OldPredTable2, ModuleInfo2, Changed1)
		;
			{ OldPredTable2 = OldPredTable0 },
			{ ModuleInfo2 = ModuleInfo1 },
			{ Changed1 = no }
		),
		modecheck_queued_procs(HowToCheckGoal, OldPredTable2,
			ModuleInfo2, OldPredTable, ModuleInfo, Changed2),
		{ bool__or(Changed1, Changed2, Changed) }
	;
		{ OldPredTable = OldPredTable0 },
		{ ModuleInfo = ModuleInfo0 },
		{ Changed = no }
	).

:- pred queued_proc_progress_message(pred_proc_id, how_to_check_goal,
			module_info, io__state, io__state).
:- mode queued_proc_progress_message(in, in, in, di, uo) is det.

queued_proc_progress_message(PredProcId, HowToCheckGoal, ModuleInfo) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		%
		% print progress message
		%
		( { HowToCheckGoal = check_unique_modes } ->
			io__write_string(
		    "% Analyzing modes, determinism, and unique-modes for\n% ")
		;
			io__write_string("% Mode-analyzing ")
		),
		{ PredProcId = proc(PredId, ProcId) },
		hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId),
		io__write_string("\n")
		/*****
		{ mode_list_get_initial_insts(Modes, ModuleInfo1,
			InitialInsts) },
		io__write_string("% Initial insts: `"),
		{ varset__init(InstVarSet) },
		mercury_output_inst_list(InitialInsts, InstVarSet),
		io__write_string("'\n")
		*****/
	;
		[]
	).

:- pred modecheck_queued_proc(how_to_check_goal, pred_proc_id, pred_table,
				module_info, pred_table, module_info, bool,
				io__state, io__state).
:- mode modecheck_queued_proc(in, in, in, in, out, out, out, di, uo) is det.

modecheck_queued_proc(HowToCheckGoal, PredProcId, OldPredTable0, ModuleInfo0,
			OldPredTable, ModuleInfo, Changed) -->
	{
	%
	% mark the procedure as ready to be processed
	%
	PredProcId = proc(PredId, ProcId),
	module_info_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, Procs0),
	map__lookup(Procs0, ProcId, ProcInfo0),
	proc_info_set_can_process(ProcInfo0, yes, ProcInfo1),
	map__det_update(Procs0, ProcId, ProcInfo1, Procs1),
	pred_info_set_procedures(PredInfo0, Procs1, PredInfo1),
	map__det_update(Preds0, PredId, PredInfo1, Preds1),
	module_info_set_preds(ModuleInfo0, Preds1, ModuleInfo1)
	},

	%
	% modecheck the procedure
	%
	modecheck_proc(ProcId, PredId, ModuleInfo1, ModuleInfo2, NumErrors,
		Changed1),
	(
		{ NumErrors \= 0 }
	->
		io__set_exit_status(1),
		{ OldPredTable = OldPredTable0 },
		{ module_info_remove_predid(ModuleInfo2, PredId, ModuleInfo) },
		{ Changed = Changed1 }
	;
		( { HowToCheckGoal = check_unique_modes } ->
			{ detect_switches_in_proc(ProcId, PredId,
						ModuleInfo2, ModuleInfo3) },
			detect_cse_in_proc(ProcId, PredId,
						ModuleInfo3, ModuleInfo4),
			determinism_check_proc(ProcId, PredId,
						ModuleInfo4, ModuleInfo5),
			{ save_proc_info(ProcId, PredId, ModuleInfo5,
				OldPredTable0, OldPredTable) },
			unique_modes__check_proc(ProcId, PredId,
						ModuleInfo5, ModuleInfo,
						Changed2),
			{ bool__or(Changed1, Changed2, Changed) }
		;	
			{ OldPredTable = OldPredTable0 },
			{ ModuleInfo = ModuleInfo2 },
			{ Changed = Changed1 }
		)
	).

%
% save a copy of the proc info for the specified procedure in OldProcTable0,
% giving OldProcTable.
%
:- pred save_proc_info(proc_id, pred_id, module_info, pred_table, pred_table).
:- mode save_proc_info(in, in, in, in, out) is det.

save_proc_info(ProcId, PredId, ModuleInfo, OldPredTable0, OldPredTable) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		_PredInfo, ProcInfo),
	map__lookup(OldPredTable0, PredId, OldPredInfo0),
	pred_info_procedures(OldPredInfo0, OldProcTable0),
	map__set(OldProcTable0, ProcId, ProcInfo, OldProcTable),
	pred_info_set_procedures(OldPredInfo0, OldProcTable, OldPredInfo),
	map__det_update(OldPredTable0, PredId, OldPredInfo, OldPredTable).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

unify_proc__add_lazily_generated_unify_pred(TypeId,
		PredId, ModuleInfo0, ModuleInfo) :-
	(
		type_id_is_tuple(TypeId) 
	->
		TypeId = _ - TupleArity,
		
		%
		% Build a hlds_type_body for the tuple constructor, which will
		% be used by unify_proc__generate_clause_info.
		%

		varset__init(TVarSet0),
		varset__new_vars(TVarSet0, TupleArity, TupleArgTVars, TVarSet),
		term__var_list_to_term_list(TupleArgTVars, TupleArgTypes),

		% Tuple constructors can't be existentially quantified.
		ExistQVars = [], 
		ClassConstraints = [],

		MakeUnamedField = (func(ArgType) = no - ArgType),
		CtorArgs = list__map(MakeUnamedField, TupleArgTypes),

		Ctor = ctor(ExistQVars, ClassConstraints,
				CtorSymName, CtorArgs),

		CtorSymName = unqualified("{}"),
		ConsId = cons(CtorSymName, TupleArity),
		map__from_assoc_list([ConsId - unshared_tag(0)],
			ConsTagValues),
		UnifyPred = no,
		IsEnum = no,
		TypeBody = du_type([Ctor], ConsTagValues, IsEnum, UnifyPred),
		construct_type(TypeId, TupleArgTypes, Type),

		term__context_init(Context)
	;
		unify_proc__collect_type_defn(ModuleInfo0, TypeId,
			Type, TVarSet, TypeBody, Context)
	),

	% Call make_hlds.m to construct the unification predicate.
	( can_generate_special_pred_clauses_for_type(TypeId, TypeBody) ->
		% If the unification predicate has another status it should
		% already have been generated. 
		UnifyPredStatus = pseudo_imported,
		Item = clauses
	;
		UnifyPredStatus = imported(implementation),
		Item = declaration
	),

	unify_proc__add_lazily_generated_special_pred(unify, Item,
		TVarSet, Type, TypeId, TypeBody, Context, UnifyPredStatus,
		PredId, ModuleInfo0, ModuleInfo).

unify_proc__add_lazily_generated_compare_pred_decl(TypeId,
		PredId, ModuleInfo0, ModuleInfo) :-
	unify_proc__collect_type_defn(ModuleInfo0, TypeId, Type,
		TVarSet, TypeBody, Context),
	
	% If the compare predicate has another status it should
	% already have been generated. 
	ImportStatus = imported(implementation),

	unify_proc__add_lazily_generated_special_pred(compare, declaration,
		TVarSet, Type, TypeId, TypeBody, Context, ImportStatus,
		PredId, ModuleInfo0, ModuleInfo).

:- pred unify_proc__add_lazily_generated_special_pred(special_pred_id,
		unify_pred_item, tvarset, type, type_id, hlds_type_body,
		context, import_status, pred_id, module_info, module_info).
:- mode unify_proc__add_lazily_generated_special_pred(in, in, in, in, in, in,
		in, in, out, in, out) is det.

unify_proc__add_lazily_generated_special_pred(SpecialId, Item,
		TVarSet, Type, TypeId, TypeBody, Context, PredStatus,
		PredId, ModuleInfo0, ModuleInfo) :-
	%
	% Add the declaration and maybe clauses.
	%
	(
		Item = clauses,
		make_hlds__add_special_pred_for_real(SpecialId, ModuleInfo0,
			TVarSet, Type, TypeId, TypeBody, Context,
			PredStatus, ModuleInfo1)
	;
		Item = declaration,
		make_hlds__add_special_pred_decl_for_real(SpecialId,
			ModuleInfo0, TVarSet, Type, TypeId,
			Context, PredStatus, ModuleInfo1)
	),

	module_info_get_special_pred_map(ModuleInfo1, SpecialPredMap),
	map__lookup(SpecialPredMap, SpecialId - TypeId, PredId),
	module_info_pred_info(ModuleInfo1, PredId, PredInfo0),

	%
	% The clauses are generated with all type information computed,
	% so just go on to post_typecheck.
	%
	(
		Item = clauses,
		post_typecheck__finish_pred_no_io(ModuleInfo1,
			ErrorProcs, PredInfo0, PredInfo)
	;
		Item = declaration,
		post_typecheck__finish_imported_pred_no_io(ModuleInfo1,
			ErrorProcs,  PredInfo0, PredInfo)
	),
	require(unify(ErrorProcs, []),
"unify_proc__add_lazily_generated_special_pred: error in post_typecheck"),

	%
	% Call polymorphism to introduce type_info arguments
	% for polymorphic types.
	%
	module_info_set_pred_info(ModuleInfo1, PredId, PredInfo, ModuleInfo2),

	%
	% Note that this will not work if the generated clauses call
	% a polymorphic predicate which requires type_infos to be added.
	% Such calls can be generated by unify_proc__generate_clause_info,
	% but unification predicates which contain such calls are never
	% generated lazily.
	%
	polymorphism__process_generated_pred(PredId, ModuleInfo2, ModuleInfo).

:- type unify_pred_item
	--->	declaration
	;	clauses
	.

:- pred unify_proc__collect_type_defn(module_info,
		type_id, type, tvarset, hlds_type_body, prog_context).
:- mode unify_proc__collect_type_defn(in, in, out, out, out, out) is det.

unify_proc__collect_type_defn(ModuleInfo0, TypeId, Type,
		TVarSet, TypeBody, Context) :-
	module_info_types(ModuleInfo0, Types),
	map__lookup(Types, TypeId, TypeDefn),
	hlds_data__get_type_defn_tvarset(TypeDefn, TVarSet),
	hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	hlds_data__get_type_defn_status(TypeDefn, TypeStatus),
	hlds_data__get_type_defn_context(TypeDefn, Context),

	require(special_pred_is_generated_lazily(ModuleInfo0,
		TypeId, TypeBody, TypeStatus),
		"unify_proc__add_lazily_generated_unify_pred"),

	construct_type(TypeId, TypeParams, Type).

%-----------------------------------------------------------------------------%

unify_proc__generate_clause_info(SpecialPredId, Type, TypeBody, Context,
		ModuleInfo, ClauseInfo) :-
	( TypeBody = eqv_type(EqvType) ->
		HeadVarType = EqvType
	;
		HeadVarType = Type
	),
	special_pred_info(SpecialPredId, HeadVarType,
		_PredName, ArgTypes, _Modes, _Det),
	unify_proc__info_init(ModuleInfo, VarTypeInfo0),
	unify_proc__make_fresh_named_vars_from_types(ArgTypes, "HeadVar__", 1,
		Args, VarTypeInfo0, VarTypeInfo1),
	( SpecialPredId = unify, Args = [H1, H2] ->
		unify_proc__generate_unify_clauses(TypeBody, H1, H2,
			Context, Clauses, VarTypeInfo1, VarTypeInfo)
	; SpecialPredId = index, Args = [X, Index] ->
		unify_proc__generate_index_clauses(TypeBody,
			X, Index, Context, Clauses, VarTypeInfo1, VarTypeInfo)
	; SpecialPredId = compare, Args = [Res, X, Y] ->
		unify_proc__generate_compare_clauses(Type, TypeBody,
			Res, X, Y, Context, Clauses, VarTypeInfo1, VarTypeInfo)
	;
		error("unknown special pred")
	),
	unify_proc__info_extract(VarTypeInfo, VarSet, Types),
	map__init(TVarNameMap),
	map__init(TI_VarMap),
	map__init(TCI_VarMap),
	HasForeignClauses = yes,
	ClauseInfo = clauses_info(VarSet, Types, TVarNameMap,
			Types, Args, Clauses, TI_VarMap, TCI_VarMap,
			HasForeignClauses).

:- pred unify_proc__generate_unify_clauses(hlds_type_body, prog_var, prog_var,
		prog_context, list(clause), unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_unify_clauses(in, in, in, in, out, in, out)
	is det.

unify_proc__generate_unify_clauses(TypeBody, H1, H2, Context, Clauses) -->
	(
		{ TypeBody = du_type(Ctors, _, IsEnum, MaybeEqPred) },
		( { MaybeEqPred = yes(PredName) } ->
			%
			% Just generate a call to the specified predicate,
			% which is the user-defined equality pred for this
			% type.
			% (The pred_id and proc_id will be figured
			% out by type checking and mode analysis.)
			%
			{ invalid_pred_id(PredId) },
			{ invalid_proc_id(ModeId) },
			{ Call = call(PredId, ModeId, [H1, H2], not_builtin,
					no, PredName) },
			{ goal_info_init(GoalInfo0) },
			{ goal_info_set_context(GoalInfo0, Context,
				GoalInfo) },
			{ Goal = Call - GoalInfo },
			unify_proc__quantify_clauses_body([H1, H2], Goal,
				Context, Clauses)
		; { IsEnum = yes } ->
			%
			% Enumerations are atomic types, so modecheck_unify.m
			% will treat this unification as a simple_test, not
			% a complicated_unify.
			%
			{ create_atomic_unification(H1, var(H2),
				Context, explicit, [], Goal) },
			unify_proc__quantify_clauses_body([H1, H2], Goal,
				Context, Clauses)
		;
			unify_proc__generate_du_unify_clauses(Ctors, H1, H2, 
				Context, Clauses)
		)
	;
		{ TypeBody = eqv_type(_Type) },
		% We should check whether _Type is a type variable,
		% an abstract type or a concrete type.
		% If it is type variable, then we should generate the same code
		% we generate now. If it is an abstract type, we should call
		% its unification procedure directly; if it is a concrete type,
		% we should generate the body of its unification procedure
		% inline here.
		%
		% XXX Somebody should document here what the later stages
		% of the compiler do to prevent an infinite recursion here.
		{ create_atomic_unification(H1, var(H2), Context, explicit, [],
			Goal) },
		unify_proc__quantify_clauses_body([H1, H2], Goal, Context,
			Clauses)
	;
		{ TypeBody = uu_type(_) },
		{ error("trying to create unify proc for uu type") }
	;
		{ TypeBody = abstract_type },
		{ error("trying to create unify proc for abstract type") }
	).

	% This predicate generates the bodies of index predicates for the
	% types that need index predicates.
	%
	% add_special_preds in make_hlds.m should include index in the list
	% of special preds to define only for the kinds of types which do not
	% lead this predicate to abort.

:- pred unify_proc__generate_index_clauses(hlds_type_body, prog_var, prog_var,
	prog_context, list(clause), unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_index_clauses(in, in, in, in, out, in, out)
	is det.

unify_proc__generate_index_clauses(TypeBody, X, Index, Context, Clauses) -->
	(
		{ TypeBody = du_type(Ctors, _, IsEnum, MaybeEqPred) },
		( { MaybeEqPred = yes(_) } ->
			%
			% For non-canonical types, the generated comparison
			% predicate returns an error, and does not call the
			% type's index predicate, so do not generate an index
			% predicate for such types.
			%
			{ error("trying to create index proc for non-canonical type") }
		; { IsEnum = yes } ->
			%
			% For enum types, the generated comparison predicate
			% performs an integer comparison, and does not call the
			% type's index predicate, so do not generate an index
			% predicate for such types.
			%
			{ error("trying to create index proc for enum type") }
		;
			unify_proc__generate_du_index_clauses(Ctors, X, Index,
				Context, 0, Clauses)
		)
	;
		{ TypeBody = eqv_type(_Type) },
		% The only place that the index predicate for a type can ever
		% be called from is the compare predicate for that type.
		% However, the compare predicate for an equivalence type
		% never calls the index predicate for that type; it calls
		% the compare predicate of the expanded type instead.
		%
		% Therefore the clause body we are generating should never be
		% invoked.
		{ error("trying to create index proc for eqv type") }
	;
		{ TypeBody = uu_type(_) },
		{ error("trying to create index proc for uu type") }
	;
		{ TypeBody = abstract_type },
		{ error("trying to create index proc for abstract type") }
	).

:- pred unify_proc__generate_compare_clauses((type)::in, hlds_type_body::in,
	prog_var::in, prog_var::in, prog_var::in, prog_context::in,
	list(clause)::out, unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__generate_compare_clauses(Type, TypeBody, Res, H1, H2, Context,
		Clauses) -->
	(
		{ TypeBody = du_type(Ctors, _, IsEnum, MaybeEqPred) },
		( { MaybeEqPred = yes(_) } ->
			%
			% just generate code that will call error/1
			%
			{ ArgVars = [Res, H1, H2] },
			unify_proc__build_call(
				"builtin_compare_non_canonical_type",
				ArgVars, Context, Goal),
			unify_proc__quantify_clauses_body(ArgVars, Goal,
				Context, Clauses)
		; { IsEnum = yes } ->
			{ IntType = int_type },
			unify_proc__info_new_var(IntType, TC1),
			unify_proc__info_new_var(IntType, TC2),
			{ TC1ArgVars = [H1, TC1] },
			unify_proc__build_call("unsafe_type_cast",
				TC1ArgVars, Context, TC1Goal),
			{ TC2ArgVars = [H2, TC2] },
			unify_proc__build_call("unsafe_type_cast",
				TC2ArgVars, Context, TC2Goal),
			{ CompareArgVars = [Res, TC1, TC2] },
			unify_proc__build_call("builtin_compare_int",
				CompareArgVars, Context, CompareGoal),
			{ goal_info_init(GoalInfo0) },
			{ goal_info_set_context(GoalInfo0, Context,
				GoalInfo) },
			{ conj_list_to_goal([TC1Goal, TC2Goal, CompareGoal],
				GoalInfo, Goal) },
			{ ArgVars = [Res, H1, H2] },
			unify_proc__quantify_clauses_body(ArgVars, Goal,
				Context, Clauses)
		;
			unify_proc__generate_du_compare_clauses(Type, Ctors,
				Res, H1, H2, Context, Clauses)
		)
	;
		{ TypeBody = eqv_type(_) },
		% We should check whether _Type is a type variable,
		% an abstract type or a concrete type.
		% If it is type variable, then we should generate the same code
		% we generate now. If it is an abstract type, we should call
		% its compare procedure directly; if it is a concrete type,
		% we should generate the body of its compare procedure
		% inline here.
		%
		% XXX Somebody should document here what the later stages
		% of the compiler do to prevent an infinite recursion here.
		{ ArgVars = [Res, H1, H2] },
		unify_proc__build_call("compare", ArgVars, Context, Goal),
		unify_proc__quantify_clauses_body(ArgVars, Goal, Context,
			Clauses)
	;
		{ TypeBody = uu_type(_) },
		{ error("trying to create compare proc for uu type") }
	;
		{ TypeBody = abstract_type },
		{ error("trying to create compare proc for abstract type") }
	).

:- pred unify_proc__quantify_clauses_body(list(prog_var)::in, hlds_goal::in,
	prog_context::in, list(clause)::out,
	unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__quantify_clauses_body(HeadVars, Goal, Context, Clauses) -->
	unify_proc__quantify_clause_body(HeadVars, Goal, Context, Clause),
	{ Clauses = [Clause] }.

:- pred unify_proc__quantify_clause_body(list(prog_var)::in, hlds_goal::in,
	prog_context::in, clause::out,
	unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__quantify_clause_body(HeadVars, Goal, Context, Clause) -->
	unify_proc__info_get_varset(Varset0),
	unify_proc__info_get_types(Types0),
	{ implicitly_quantify_clause_body(HeadVars, Goal, Varset0, Types0,
		Body, Varset, Types, _Warnings) },
	unify_proc__info_set_varset(Varset),
	unify_proc__info_set_types(Types),
	{ Clause = clause([], Body, mercury, Context) }.

%-----------------------------------------------------------------------------%

%	For a type such as
%
%		type t(X) ---> a ; b(int) ; c(X); d(int, X, t)
%
%	we want to generate code
%
%		eq(H1, H2) :-
%			(
%				H1 = a,
%				H2 = a
%			;
%				H1 = b(X1),
%				H2 = b(X2),
%				X1 = X2,
%			;
%				H1 = c(Y1),
%				H2 = c(Y2),
%				Y1 = Y2,
%			;
%				H1 = d(A1, B1, C1),
%				H2 = c(A2, B2, C2),
%				A1 = A2,
%				B1 = B2,
%				C1 = C2
%			).

:- pred unify_proc__generate_du_unify_clauses(list(constructor), prog_var,
		prog_var, prog_context, list(clause),
		unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_du_unify_clauses(in, in, in, in, out, in, out)
	is det.

unify_proc__generate_du_unify_clauses([], _H1, _H2, _Context, []) --> [].
unify_proc__generate_du_unify_clauses([Ctor | Ctors], H1, H2, Context,
		[Clause | Clauses]) -->
	{ Ctor = ctor(ExistQTVars, _Constraints, FunctorName, ArgTypes) },
	{ list__length(ArgTypes, FunctorArity) },
	{ FunctorConsId = cons(FunctorName, FunctorArity) },
	unify_proc__make_fresh_vars(ArgTypes, ExistQTVars, Vars1),
	unify_proc__make_fresh_vars(ArgTypes, ExistQTVars, Vars2),
	{ create_atomic_unification(
		H1, functor(FunctorConsId, Vars1), Context, explicit, [], 
		UnifyH1_Goal) },
	{ create_atomic_unification(
		H2, functor(FunctorConsId, Vars2), Context, explicit, [], 
		UnifyH2_Goal) },
	unify_proc__unify_var_lists(ArgTypes, ExistQTVars, Vars1, Vars2,
		UnifyArgs_Goal),
	{ GoalList = [UnifyH1_Goal, UnifyH2_Goal | UnifyArgs_Goal] },
	{ goal_info_init(GoalInfo0) },
	{ goal_info_set_context(GoalInfo0, Context,
		GoalInfo) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) },
	unify_proc__quantify_clause_body([H1, H2], Goal, Context, Clause),
	unify_proc__generate_du_unify_clauses(Ctors, H1, H2, Context, Clauses).

%-----------------------------------------------------------------------------%

%	For a type such as 
%
%		:- type foo ---> f ; g(a, b, c) ; h(foo).
%
%	we want to generate code
%
%		index(X, Index) :-
%			(
%				X = f,
%				Index = 0
%			;
%				X = g(_, _, _),
%				Index = 1
%			;
%				X = h(_),
%				Index = 2
%			).

:- pred unify_proc__generate_du_index_clauses(list(constructor), prog_var,
		prog_var, prog_context, int, list(clause),
		unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_du_index_clauses(in, in, in, in, in, out, in, out)
	is det.

unify_proc__generate_du_index_clauses([], _X, _Index, _Context, _N, []) --> [].
unify_proc__generate_du_index_clauses([Ctor | Ctors], X, Index, Context, N,
		[Clause | Clauses]) -->
	{ Ctor = ctor(ExistQTVars, _Constraints, FunctorName, ArgTypes) },
	{ list__length(ArgTypes, FunctorArity) },
	{ FunctorConsId = cons(FunctorName, FunctorArity) },
	unify_proc__make_fresh_vars(ArgTypes, ExistQTVars, ArgVars),
	{ create_atomic_unification(
		X, functor(FunctorConsId, ArgVars), Context, explicit, [], 
		UnifyX_Goal) },
	{ create_atomic_unification(
		Index, functor(int_const(N), []), Context, explicit, [], 
		UnifyIndex_Goal) },
	{ GoalList = [UnifyX_Goal, UnifyIndex_Goal] },
	{ goal_info_init(GoalInfo0) },
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) },
	unify_proc__quantify_clause_body([X, Index], Goal, Context, Clause),
	{ N1 is N + 1 },
	unify_proc__generate_du_index_clauses(Ctors, X, Index, Context, N1,
		Clauses).

%-----------------------------------------------------------------------------%

:- pred unify_proc__generate_du_compare_clauses((type)::in,
	list(constructor)::in, prog_var::in, prog_var::in, prog_var::in,
	prog_context::in, list(clause)::out,
	unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__generate_du_compare_clauses(Type, Ctors, Res, H1, H2,
		Context, Clauses) -->
	(
		{ Ctors = [] },
		{ error("compare for type with no functors") }
	;
		{ Ctors = [Ctor] },
		unify_proc__info_get_module_info(ModuleInfo),
		{ module_info_globals(ModuleInfo, Globals) },
		{ globals__lookup_int_option(Globals, compare_specialization,
			CompareSpec) },
		( { CompareSpec >= 1 } ->
			unify_proc__generate_du_one_compare_clause(
				Ctor, Res, H1, H2,
				Context, Clauses)
		;
			unify_proc__generate_du_general_compare_clauses(Type,
				Ctors, Res, H1, H2, Context, Clauses)
		)
	;
		{ Ctors = [Ctor1, Ctor2] },
		unify_proc__info_get_module_info(ModuleInfo),
		{ module_info_globals(ModuleInfo, Globals) },
		{ globals__lookup_int_option(Globals, compare_specialization,
			CompareSpec) },
		( { CompareSpec >= 2 } ->
			unify_proc__generate_du_two_compare_clauses(
				Ctor1, Ctor2, Res, H1, H2,
				Context, Clauses)
		;
			unify_proc__generate_du_general_compare_clauses(Type,
				Ctors, Res, H1, H2, Context, Clauses)
		)
	;
		{ Ctors = [Ctor1, Ctor2, Ctor3] },
		unify_proc__info_get_module_info(ModuleInfo),
		{ module_info_globals(ModuleInfo, Globals) },
		{ globals__lookup_int_option(Globals, compare_specialization,
			CompareSpec) },
		( { CompareSpec >= 3 } ->
			unify_proc__generate_du_three_compare_clauses(
				Ctor1, Ctor2, Ctor3, Res, H1, H2,
				Context, Clauses)
		;
			unify_proc__generate_du_general_compare_clauses(Type,
				Ctors, Res, H1, H2, Context, Clauses)
		)
	;
		{ Ctors = [_, _, _, _ | _] },
		unify_proc__generate_du_general_compare_clauses(Type,
			Ctors, Res, H1, H2, Context, Clauses)
	).

unify_proc__max_exploited_compare_spec_value = 3.

%-----------------------------------------------------------------------------%

%	For a du type with one function symbol, such as 
%
%		:- type foo ---> f(a, b, c)
%
%   	we want to generate code
%
%		compare(Res, X, Y) :-
%			X = f(X1, X2, X3), Y = f(Y1, Y2, Y3),
%			( compare(R1, X1, Y1), R1 \= (=) ->
%				R = R1
%			; compare(R2, X2, Y2), R2 \= (=) ->
%				R = R2
%			; 
%				compare(R, X3, Y3)
%			).

:- pred unify_proc__generate_du_one_compare_clause(constructor::in,
	prog_var::in, prog_var::in, prog_var::in,
	prog_context::in, list(clause)::out,
	unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__generate_du_one_compare_clause(Ctor, R, X, Y, Context, Clauses) -->
	unify_proc__generate_compare_case(Ctor, R, X, Y, Context, Goal),
	{ HeadVars = [R, X, Y] },
	unify_proc__quantify_clauses_body(HeadVars, Goal, Context, Clauses).

%-----------------------------------------------------------------------------%

%	For a du type with two or three function symbols, such as 
%
%		:- type foo ---> f(a) ; g(a, b, c)
%
%   	we want to generate code such as
%
%		compare(Res, X, Y) :-
%			(
%				X = f(X1),
%				Y = f(Y1),
%				compare(R, X1, Y1)
%			;
%				X = f(_),
%				Y = g(_, _, _),
%				R = (<)
%			;
%				X = g(_, _, _),
%				Y = f(_),
%				R = (>)
%			;
%				X = g(X1, X2, X3),
%				Y = g(Y1, Y2, Y3),
%				( compare(R1, X1, Y1), R1 \= (=) ->
%					R = R1
%				; compare(R2, X2, Y2), R2 \= (=) ->
%					R = R2
%				; 
%					compare(R, X3, Y3)
%				)
%			).

:- pred unify_proc__generate_du_two_compare_clauses(
	constructor::in, constructor::in, prog_var::in, prog_var::in,
	prog_var::in, prog_context::in, list(clause)::out,
	unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__generate_du_two_compare_clauses(Ctor1, Ctor2, R, X, Y,
		Context, Clauses) -->
	unify_proc__generate_compare_case(Ctor1, R, X, Y, Context, Case11),
	unify_proc__generate_compare_case(Ctor2, R, X, Y, Context, Case22),
	unify_proc__generate_asymmetric_compare_case(Ctor1, Ctor2, "<",
		R, X, Y, Context, Case12),
	unify_proc__generate_asymmetric_compare_case(Ctor2, Ctor1, ">",
		R, X, Y, Context, Case21),

	{ goal_info_init(GoalInfo0) },
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
	{ map__init(Empty) },
	{ Goal = disj([Case11, Case12, Case21, Case22], Empty) - GoalInfo },
	{ HeadVars = [R, X, Y] },
	unify_proc__quantify_clauses_body(HeadVars, Goal, Context, Clauses).

:- pred unify_proc__generate_du_three_compare_clauses(
	constructor::in, constructor::in, constructor::in,
	prog_var::in, prog_var::in, prog_var::in, prog_context::in,
	list(clause)::out, unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__generate_du_three_compare_clauses(Ctor1, Ctor2, Ctor3, R, X, Y,
		Context, Clauses) -->
	unify_proc__generate_compare_case(Ctor1, R, X, Y, Context, Case11),
	unify_proc__generate_compare_case(Ctor2, R, X, Y, Context, Case22),
	unify_proc__generate_compare_case(Ctor3, R, X, Y, Context, Case33),
	unify_proc__generate_asymmetric_compare_case(Ctor1, Ctor2, "<",
		R, X, Y, Context, Case12),
	unify_proc__generate_asymmetric_compare_case(Ctor1, Ctor3, "<",
		R, X, Y, Context, Case13),
	unify_proc__generate_asymmetric_compare_case(Ctor2, Ctor3, "<",
		R, X, Y, Context, Case23),
	unify_proc__generate_asymmetric_compare_case(Ctor2, Ctor1, ">",
		R, X, Y, Context, Case21),
	unify_proc__generate_asymmetric_compare_case(Ctor3, Ctor1, ">",
		R, X, Y, Context, Case31),
	unify_proc__generate_asymmetric_compare_case(Ctor3, Ctor2, ">",
		R, X, Y, Context, Case32),

	{ goal_info_init(GoalInfo0) },
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
	{ map__init(Empty) },
	{ Goal = disj([Case11, Case12, Case13, Case21, Case22, Case23,
		Case31, Case32, Case33], Empty) - GoalInfo },
	{ HeadVars = [R, X, Y] },
	unify_proc__quantify_clauses_body(HeadVars, Goal, Context, Clauses).

%-----------------------------------------------------------------------------%

%	For a du type with four or more function symbols, such as 
%
%		:- type foo ---> f ; g(a) ; h(b, foo).
%
%   	we want to generate code
%
%		compare(Res, X, Y) :-
%			__Index__(X, X_Index),	% Call_X_Index
%			__Index__(Y, Y_Index),	% Call_Y_Index
%			( X_Index < Y_Index ->	% Call_Less_Than
%				Res = (<)	% Return_Less_Than
%			; X_Index > Y_Index ->	% Call_Greater_Than
%				Res = (>)	% Return_Greater_Than
%			;
%				% This disjunction is generated by
%				% unify_proc__generate_compare_cases, below.
%				(
%					X = f, Y = f,
%					R = (=)
%				;
%					X = g(X1), Y = g(Y1),
%					compare(R, X1, Y1)
%				;
%					X = h(X1, X2), Y = h(Y1, Y2),
%					( compare(R1, X1, Y1), R1 \= (=) ->
%						R = R1
%					; 
%						compare(R, X2, Y2)
%					)
%				)
%			->
%				Res = R		% Return_R
%			;
%				compare_error 	% Abort
%			).

:- pred unify_proc__generate_du_general_compare_clauses((type)::in,
	list(constructor)::in, prog_var::in, prog_var::in, prog_var::in,
	prog_context::in, list(clause)::out,
	unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__generate_du_general_compare_clauses(Type, Ctors, Res, X, Y,
		Context, [Clause]) -->
	unify_proc__generate_du_compare_clauses_2(Type, Ctors, Res,
		X, Y, Context, Goal),
	{ HeadVars = [Res, X, Y] },
	unify_proc__quantify_clause_body(HeadVars, Goal, Context, Clause).

:- pred unify_proc__generate_du_compare_clauses_2((type)::in,
	list(constructor)::in, prog_var::in, prog_var::in, prog_var::in,
	prog_context::in, hlds_goal::out,
	unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__generate_du_compare_clauses_2(Type, Ctors, Res, X, Y, Context,
		Goal) -->
	{ IntType = int_type },
	{ mercury_public_builtin_module(MercuryBuiltin) },
	{ construct_type(qualified(MercuryBuiltin, "comparison_result") - 0,
					[], ResType) },
	unify_proc__info_new_var(IntType, X_Index),
	unify_proc__info_new_var(IntType, Y_Index),
	unify_proc__info_new_var(ResType, R),

	{ goal_info_init(GoalInfo0) },
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },

	{ instmap_delta_from_assoc_list([X_Index - ground(shared, none)],
		X_InstmapDelta) },
	unify_proc__build_specific_call(Type, index, [X, X_Index],
		X_InstmapDelta, det, Context, Call_X_Index),
	{ instmap_delta_from_assoc_list([Y_Index - ground(shared, none)],
		Y_InstmapDelta) },
	unify_proc__build_specific_call(Type, index, [Y, Y_Index],
		Y_InstmapDelta, det, Context, Call_Y_Index),

	unify_proc__build_call("builtin_int_lt", [X_Index, Y_Index], Context,
		Call_Less_Than),
	unify_proc__build_call("builtin_int_gt", [X_Index, Y_Index], Context,
		Call_Greater_Than),

	{ create_atomic_unification(
		Res, functor(cons(unqualified("<"), 0), []),
			Context, explicit, [], 
		Return_Less_Than) },

	{ create_atomic_unification(
		Res, functor(cons(unqualified(">"), 0), []),
			Context, explicit, [], 
		Return_Greater_Than) },

	{ create_atomic_unification(Res, var(R), Context, explicit, [],
		Return_R) },

	unify_proc__generate_compare_cases(Ctors, R, X, Y, Context, Cases),
	{ map__init(Empty) },
	{ CasesGoal = disj(Cases, Empty) - GoalInfo },

	unify_proc__build_call("compare_error", [], Context, Abort),

	{ Goal = conj([
		Call_X_Index,
		Call_Y_Index, 
		if_then_else([], Call_Less_Than, Return_Less_Than,
		    if_then_else([], Call_Greater_Than, Return_Greater_Than,
		        if_then_else([], CasesGoal, Return_R, Abort, Empty
		        ) - GoalInfo, Empty
		    ) - GoalInfo, Empty
		) - GoalInfo
	]) - GoalInfo }.

%	unify_proc__generate_compare_cases: for a type such as 
%
%		:- type foo ---> f ; g(a) ; h(b, foo).
%
%   	we want to generate code
%		(
%			X = f,		% UnifyX_Goal
%			Y = f,		% UnifyY_Goal
%			R = (=)		% CompareArgs_Goal
%		;
%			X = g(X1),	
%			Y = g(Y1),
%			compare(R, X1, Y1)
%		;
%			X = h(X1, X2),
%			Y = h(Y1, Y2),
%			( compare(R1, X1, Y1), R1 \= (=) ->
%				R = R1
%			; 
%				compare(R, X2, Y2)
%			)
%		)

:- pred unify_proc__generate_compare_cases(list(constructor), prog_var,
		prog_var, prog_var, prog_context, list(hlds_goal),
		unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_compare_cases(in, in, in, in, in, out, in, out)
	is det.

unify_proc__generate_compare_cases([], _R, _X, _Y, _Context, []) --> [].
unify_proc__generate_compare_cases([Ctor | Ctors], R, X, Y, Context,
		[Case | Cases]) -->
	unify_proc__generate_compare_case(Ctor, R, X, Y, Context, Case),
	unify_proc__generate_compare_cases(Ctors, R, X, Y, Context, Cases).

:- pred unify_proc__generate_compare_case(constructor, prog_var, prog_var,
		prog_var, prog_context, hlds_goal,
		unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_compare_case(in, in, in, in, in, out, in, out)
	is det.

unify_proc__generate_compare_case(Ctor, R, X, Y, Context, Case) -->
	{ Ctor = ctor(ExistQTVars, _Constraints, FunctorName, ArgTypes) },
	{ list__length(ArgTypes, FunctorArity) },
	{ FunctorConsId = cons(FunctorName, FunctorArity) },
	unify_proc__make_fresh_vars(ArgTypes, ExistQTVars, Vars1),
	unify_proc__make_fresh_vars(ArgTypes, ExistQTVars, Vars2),
	{ create_atomic_unification(
		X, functor(FunctorConsId, Vars1), Context, explicit, [], 
		UnifyX_Goal) },
	{ create_atomic_unification(
		Y, functor(FunctorConsId, Vars2), Context, explicit, [], 
		UnifyY_Goal) },
	unify_proc__compare_args(ArgTypes, ExistQTVars, Vars1, Vars2,
		R, Context, CompareArgs_Goal),
	{ GoalList = [UnifyX_Goal, UnifyY_Goal, CompareArgs_Goal] },
	{ goal_info_init(GoalInfo0) },
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
	{ conj_list_to_goal(GoalList, GoalInfo, Case) }.

:- pred unify_proc__generate_asymmetric_compare_case(constructor::in,
	constructor::in, string::in, prog_var::in, prog_var::in, prog_var::in,
	prog_context::in, hlds_goal::out,
	unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__generate_asymmetric_compare_case(Ctor1, Ctor2, CompareOp, R, X, Y,
		Context, Case) -->
	{ Ctor1 = ctor(ExistQTVars1, _Constraints1, FunctorName1, ArgTypes1) },
	{ Ctor2 = ctor(ExistQTVars2, _Constraints2, FunctorName2, ArgTypes2) },
	{ list__length(ArgTypes1, FunctorArity1) },
	{ list__length(ArgTypes2, FunctorArity2) },
	{ FunctorConsId1 = cons(FunctorName1, FunctorArity1) },
	{ FunctorConsId2 = cons(FunctorName2, FunctorArity2) },
	unify_proc__make_fresh_vars(ArgTypes1, ExistQTVars1, Vars1),
	unify_proc__make_fresh_vars(ArgTypes2, ExistQTVars2, Vars2),
	{ create_atomic_unification(
		X, functor(FunctorConsId1, Vars1), Context, explicit, [], 
		UnifyX_Goal) },
	{ create_atomic_unification(
		Y, functor(FunctorConsId2, Vars2), Context, explicit, [], 
		UnifyY_Goal) },
	{ create_atomic_unification(
		R, functor(cons(unqualified(CompareOp), 0), []),
			Context, explicit, [], 
		ReturnResult) },
	{ GoalList = [UnifyX_Goal, UnifyY_Goal, ReturnResult] },
	{ goal_info_init(GoalInfo0) },
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
	{ conj_list_to_goal(GoalList, GoalInfo, Case) }.

%	unify_proc__compare_args: for a constructor such as
%
%		h(list(int), foo, string)
%
%	we want to generate code
%
%		(
%			compare(R1, X1, Y1),	% Do_Comparison
%			R1 \= (=)		% Check_Not_Equal
%		->
%			R = R1			% Return_R1
%		;
%			compare(R2, X2, Y2),
%			R2 \= (=)
%		->
%			R = R2
%		; 
%			compare(R, X3, Y3)	% Return_Comparison
%		)
%
%	For a constructor with no arguments, we want to generate code
%
%		R = (=)		% Return_Equal

:- pred unify_proc__compare_args(list(constructor_arg), existq_tvars,
		list(prog_var), list(prog_var), prog_var, prog_context,
		hlds_goal, unify_proc_info, unify_proc_info).
:- mode unify_proc__compare_args(in, in, in, in, in, in, out, in, out) is det.

unify_proc__compare_args(ArgTypes, ExistQTVars, Xs, Ys, R, Context, Goal) -->
	(
		unify_proc__compare_args_2(ArgTypes, ExistQTVars, Xs, Ys, R,
			Context, Goal0)
	->
		{ Goal = Goal0 }
	;
		{ error("unify_proc__compare_args: length mismatch") }
	).

:- pred unify_proc__compare_args_2(list(constructor_arg), existq_tvars,
		list(prog_var), list(prog_var), prog_var, prog_context,
		hlds_goal, unify_proc_info, unify_proc_info).
:- mode unify_proc__compare_args_2(in, in, in, in, in, in, out, in, out)
		is semidet.

unify_proc__compare_args_2([], _, [], [], R, Context, Return_Equal) -->
	{ create_atomic_unification(
		R, functor(cons(unqualified("="), 0), []),
		Context, explicit, [], 
		Return_Equal) }.
unify_proc__compare_args_2([_Name - Type|ArgTypes], ExistQTVars, [X|Xs], [Y|Ys],
		R, Context, Goal) -->
	{ goal_info_init(GoalInfo0) },
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
	%
	% When comparing existentially typed arguments, the arguments may
	% have different types; in that case, rather than just comparing them,
	% which would be a type error, we call `typed_compare', which is a
	% builtin that first compares their types and then compares
	% their values.
	%
	{
		list__member(ExistQTVar, ExistQTVars),
		term__contains_var(Type, ExistQTVar)
	->
		ComparePred = "typed_compare"
	;
		ComparePred = "compare"
	},
	( { Xs = [], Ys = [] } ->
		unify_proc__build_call(ComparePred, [R, X, Y], Context, Goal)
	;
		{ mercury_public_builtin_module(MercuryBuiltin) },
		{ construct_type(
			qualified(MercuryBuiltin, "comparison_result") - 0,
			[], ResType) },
		unify_proc__info_new_var(ResType, R1),

		unify_proc__build_call(ComparePred, [R1, X, Y], Context,
			Do_Comparison),

		{ create_atomic_unification(
			R1, functor(cons(unqualified("="), 0), []),
			Context, explicit, [],
			Check_Equal) },
		{ Check_Not_Equal = not(Check_Equal) - GoalInfo },

		{ create_atomic_unification(
			R, var(R1), Context, explicit, [], Return_R1) },
		{ Condition = conj([Do_Comparison, Check_Not_Equal])
					- GoalInfo },
		{ map__init(Empty) },
		{ Goal = if_then_else([], Condition, Return_R1, ElseCase, Empty)
					- GoalInfo},
		unify_proc__compare_args_2(ArgTypes, ExistQTVars, Xs, Ys, R,
			Context, ElseCase)
	).

%-----------------------------------------------------------------------------%

:- pred unify_proc__build_call(string, list(prog_var), prog_context, hlds_goal,
			unify_proc_info, unify_proc_info).
:- mode unify_proc__build_call(in, in, in, out, in, out) is det.

unify_proc__build_call(Name, ArgVars, Context, Goal) -->
	unify_proc__info_get_module_info(ModuleInfo),
	{ module_info_get_predicate_table(ModuleInfo, PredicateTable) },
	{ list__length(ArgVars, Arity) },
	%
	% We assume that the special preds compare/3, index/2, and unify/2
	% are the only public builtins called by code generated
	% by this module.
	%
	{ special_pred_name_arity(_, Name, _, Arity) ->
		mercury_public_builtin_module(MercuryBuiltin)
	;
		mercury_private_builtin_module(MercuryBuiltin)
	},
	{
		predicate_table_search_pred_m_n_a(PredicateTable,
			MercuryBuiltin, Name, Arity, [PredIdPrime])
	->
		PredId = PredIdPrime
	;
		prog_out__sym_name_to_string(qualified(MercuryBuiltin, Name),
			QualName),
		string__int_to_string(Arity, ArityString),
		string__append_list(["unify_proc__build_call: ",
			"invalid/ambiguous pred `",
			QualName, "/", ArityString, "'"],
			ErrorMessage),
		error(ErrorMessage)
	},
	{ hlds_pred__initial_proc_id(ProcId) },
	{ Call = call(PredId, ProcId, ArgVars, not_builtin,
			no, qualified(MercuryBuiltin, Name)) },
	{ goal_info_init(GoalInfo0) },
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
	{ Goal = Call - GoalInfo }.

:- pred unify_proc__build_specific_call((type)::in, special_pred_id::in,
	list(prog_var)::in, instmap_delta::in, determinism::in,
	prog_context::in, hlds_goal::out,
	unify_proc_info::in, unify_proc_info::out) is det.

unify_proc__build_specific_call(Type, SpecialPredId, ArgVars, InstmapDelta,
		Detism, Context, Goal) -->
	unify_proc__info_get_module_info(ModuleInfo),
	{
		polymorphism__get_special_proc(Type, SpecialPredId, ModuleInfo,
			PredName, PredId, ProcId)
	->
		GoalExpr = call(PredId, ProcId, ArgVars, not_builtin, no,
			PredName),
		set__list_to_set(ArgVars, NonLocals),
		goal_info_init(NonLocals, InstmapDelta, Detism, GoalInfo0),
		goal_info_set_context(GoalInfo0, Context, GoalInfo),
		Goal = GoalExpr - GoalInfo
	;
			% unify_proc__build_specific_call is only ever used
			% to build calls to special preds for a type in the
			% bodies of other special preds for that same type.
			% If the special preds for a type are built in the
			% right order (index before compare), the lookup
			% should never fail.
		error("unify_proc__build_specific_call: lookup failed")
	}.

%-----------------------------------------------------------------------------%

:- pred unify_proc__make_fresh_named_vars_from_types(list(type), string, int,
	list(prog_var), unify_proc_info, unify_proc_info).
:- mode unify_proc__make_fresh_named_vars_from_types(in, in, in, out, in, out)
	is det.

unify_proc__make_fresh_named_vars_from_types([], _, _, []) --> [].
unify_proc__make_fresh_named_vars_from_types([Type | Types], BaseName, Num,
		[Var | Vars]) -->
	{ string__int_to_string(Num, NumStr) },
	{ string__append(BaseName, NumStr, Name) },
	unify_proc__info_new_named_var(Type, Name, Var),
	unify_proc__make_fresh_named_vars_from_types(Types, BaseName, Num + 1,
		Vars).

:- pred unify_proc__make_fresh_vars_from_types(list(type), list(prog_var),
			unify_proc_info, unify_proc_info).
:- mode unify_proc__make_fresh_vars_from_types(in, out, in, out) is det.

unify_proc__make_fresh_vars_from_types([], []) --> [].
unify_proc__make_fresh_vars_from_types([Type | Types], [Var | Vars]) -->
	unify_proc__info_new_var(Type, Var),
	unify_proc__make_fresh_vars_from_types(Types, Vars).

:- pred unify_proc__make_fresh_vars(list(constructor_arg), existq_tvars,
			list(prog_var), unify_proc_info, unify_proc_info).
:- mode unify_proc__make_fresh_vars(in, in, out, in, out) is det.

unify_proc__make_fresh_vars(CtorArgs, ExistQTVars, Vars) -->
	( { ExistQTVars = [] } ->
		{ assoc_list__values(CtorArgs, ArgTypes) },
		unify_proc__make_fresh_vars_from_types(ArgTypes, Vars)
	;
		%
		% If there are existential types involved, then it's too
		% hard to get the types right here (it would require
		% allocating new type variables) -- instead, typecheck.m
		% will typecheck the clause to figure out the correct types.
		% So we just allocate the variables and leave it up to
		% typecheck.m to infer their types.
		%
		unify_proc__info_get_varset(VarSet0),
		{ list__length(CtorArgs, NumVars) },
		{ varset__new_vars(VarSet0, NumVars, Vars, VarSet) },
		unify_proc__info_set_varset(VarSet)
	).
		
:- pred unify_proc__unify_var_lists(list(constructor_arg), existq_tvars,
		list(prog_var), list(prog_var), list(hlds_goal),
		unify_proc_info, unify_proc_info).
:- mode unify_proc__unify_var_lists(in, in, in, in, out, in, out) is det.

unify_proc__unify_var_lists(ArgTypes, ExistQVars, Vars1, Vars2, Goal) -->
	(
		unify_proc__unify_var_lists_2(ArgTypes, ExistQVars,
			Vars1, Vars2, Goal0)
	->
		{ Goal = Goal0 }
	;
		{ error("unify_proc__unify_var_lists: length mismatch") }
	).

:- pred unify_proc__unify_var_lists_2(list(constructor_arg), existq_tvars,
		list(prog_var), list(prog_var), list(hlds_goal),
		unify_proc_info, unify_proc_info).
:- mode unify_proc__unify_var_lists_2(in, in, in, in, out, in, out) is semidet.

unify_proc__unify_var_lists_2([], _, [], [], []) --> [].
unify_proc__unify_var_lists_2([_Name - Type | ArgTypes], ExistQTVars,
		[Var1 | Vars1], [Var2 | Vars2], [Goal | Goals]) -->
	{ term__context_init(Context) },
	%
	% When unifying existentially typed arguments, the arguments may
	% have different types; in that case, rather than just unifying them,
	% which would be a type error, we call `typed_unify', which is a
	% builtin that first checks that their types are equal and then
	% unifies the values.
	%
	(
		{ list__member(ExistQTVar, ExistQTVars) },
		{ term__contains_var(Type, ExistQTVar) }
	->
		unify_proc__build_call("typed_unify", [Var1, Var2], Context,
			Goal)
	;
		{ create_atomic_unification(Var1, var(Var2), Context, explicit,
			[], Goal) }
	),
	unify_proc__unify_var_lists_2(ArgTypes, ExistQTVars, Vars1, Vars2,
		Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% It's a pity that we don't have nested modules.

% :- begin_module unify_proc_info.
% :- interface.

:- type unify_proc_info.

:- pred unify_proc__info_init(module_info::in, unify_proc_info::out) is det.
:- pred unify_proc__info_new_var((type)::in, prog_var::out,
	unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc__info_new_named_var((type)::in, string::in, prog_var::out,
	unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc__info_extract(unify_proc_info::in,
	prog_varset::out, vartypes::out) is det.
:- pred unify_proc__info_get_varset(prog_varset::out,
	unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc__info_set_varset(prog_varset::in,
	unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc__info_get_types(vartypes::out,
	unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc__info_set_types(vartypes::in,
	unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc__info_get_type_info_varmap(type_info_varmap::out,
	unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc__info_get_module_info(module_info::out,
	unify_proc_info::in, unify_proc_info::out) is det.

%-----------------------------------------------------------------------------%

% :- implementation

:- type unify_proc_info
	--->	unify_proc_info(
			varset			::	prog_varset,
			vartypes		::	vartypes,
			type_info_varmap	::	type_info_varmap,
			module_info		::	module_info
		).

unify_proc__info_init(ModuleInfo, UPI) :-
	varset__init(VarSet),
	map__init(Types),
	map__init(TVarMap),
	UPI = unify_proc_info(VarSet, Types, TVarMap, ModuleInfo).

unify_proc__info_new_var(Type, Var, UPI,
		(UPI^varset := VarSet) ^vartypes := Types) :-
	varset__new_var(UPI^varset, Var, VarSet),
	map__det_insert(UPI^vartypes, Var, Type, Types).

unify_proc__info_new_named_var(Type, Name, Var, UPI,
		(UPI^varset := VarSet) ^vartypes := Types) :-
	varset__new_named_var(UPI^varset, Name, Var, VarSet),
	map__det_insert(UPI^vartypes, Var, Type, Types).

unify_proc__info_extract(UPI, UPI^varset, UPI^vartypes).

unify_proc__info_get_varset(UPI^varset, UPI, UPI).
unify_proc__info_get_types(UPI^vartypes, UPI, UPI).
unify_proc__info_get_type_info_varmap(UPI^type_info_varmap, UPI, UPI).
unify_proc__info_get_module_info(UPI^module_info, UPI, UPI).

unify_proc__info_set_varset(VarSet, UPI, UPI^varset := VarSet).
unify_proc__info_set_types(Types, UPI, UPI^vartypes := Types).

%-----------------------------------------------------------------------------%
