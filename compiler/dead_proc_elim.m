%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% The job of this module is to delete dead predicates, procedures 
% and base_gen_info structures from the HLDS.
%
% It also computes the usage counts that inlining.m uses for the
% `--inline-single-use' option.
%
% Main author: zs.
%
%-----------------------------------------------------------------------------%

:- module dead_proc_elim.

:- interface.

:- import_module prog_data, hlds_module, hlds_pred.
:- import_module map, std_util, io.

:- pred dead_proc_elim(module_info, module_info, io__state, io__state).
:- mode dead_proc_elim(in, out, di, uo) is det.

:- pred dead_proc_elim__analyze(module_info, needed_map).
:- mode dead_proc_elim__analyze(in, out) is det.

:- pred dead_proc_elim__eliminate(module_info, needed_map, module_info,
	io__state, io__state).
:- mode dead_proc_elim__eliminate(in, in, out, di, uo) is det.

	% This is performed immediately after make_hlds.m to avoid doing
	% semantic checking and optimization on predicates from `.opt' 
	% files which are not used in the current module. This assumes that
	% the clauses_info is still valid, so it cannot be run after mode
	% analysis.
:- pred dead_pred_elim(module_info, module_info).
:- mode dead_pred_elim(in, out) is det.

:- type entity	
	--->	proc(pred_id, proc_id)
	;	base_gen_info(module_name, string, int).

:- type needed_map == map(entity, maybe(int)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, prog_util, llds.
:- import_module passes_aux, globals, options, code_util.

:- import_module int, string, list, set, queue, bool, require.

%-----------------------------------------------------------------------------%

% We deal with two kinds of entities, procedures and base_gen_info structures.
%
% The algorithm has three main data structures:
%
%	- a map of entities known to be needed to either "no" (if they
%	  cannot possibly be eliminated) or to "yes" and the number of their
%	  uses (if they are a candidate for elimination after inlining)
%
%	- a queue of entities to be examined,
%
%	- a set of entities that have been examined.
%
% The needed map and the queue are both initialized with the ids of the
% procedures and base_gen_info structures exported from the module.
% The algorithm then takes the ids of entities from the queue one at a time,
% and if the entity hasn't been examined before, examines the entity
% definition to find all mention of other entities. Their ids are then
% put into both the needed map and the queue.
%
% The final pass of the algorithm deletes from the HLDS any procedure
% or base_gen_info structure whose id is not in the needed map.

:- type entity_queue	==	queue(entity).
:- type examined_set	==	set(entity).

dead_proc_elim(ModuleInfo0, ModuleInfo, State0, State) :-
	dead_proc_elim__analyze(ModuleInfo0, Needed),
	dead_proc_elim__eliminate(ModuleInfo0, Needed, ModuleInfo,
		State0, State).

%-----------------------------------------------------------------------------%

	% Find all needed entities.

dead_proc_elim__analyze(ModuleInfo0, Needed) :-
	set__init(Examined0),
	dead_proc_elim__initialize(ModuleInfo0, Queue0, Needed0),
	dead_proc_elim__examine(Queue0, Examined0, ModuleInfo0,
		Needed0, Needed).

	% Add all exported entities to the queue and map.

:- pred dead_proc_elim__initialize(module_info, entity_queue, needed_map).
:- mode dead_proc_elim__initialize(in, out, out) is det.

dead_proc_elim__initialize(ModuleInfo, Queue, Needed) :-
	queue__init(Queue0),
	map__init(Needed0),
	module_info_predids(ModuleInfo, PredIds),
	module_info_preds(ModuleInfo, PredTable),
	dead_proc_elim__initialize_preds(PredIds, PredTable,
		Queue0, Queue1, Needed0, Needed1),
	module_info_get_pragma_exported_procs(ModuleInfo, PragmaExports),
	dead_proc_elim__initialize_pragma_exports(PragmaExports,
		Queue1, Queue2, Needed1, Needed2),
	module_info_base_gen_infos(ModuleInfo, BaseGenInfos),
	dead_proc_elim__initialize_base_gen_infos(BaseGenInfos,
		Queue2, Queue3, Needed2, Needed3),
	module_info_instances(ModuleInfo, Instances),
	dead_proc_elim__initialize_class_methods(Instances,
		Queue3, Queue, Needed3, Needed).

	% Add all normally exported procedures within the listed predicates
	% to the queue and map.

:- pred dead_proc_elim__initialize_preds(list(pred_id), pred_table,
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__initialize_preds(in, in, in, out, in, out) is det.

dead_proc_elim__initialize_preds([], _PredTable, Queue, Queue, Needed, Needed).
dead_proc_elim__initialize_preds([PredId | PredIds], PredTable,
		Queue0, Queue, Needed0, Needed) :-
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_exported_procids(PredInfo, ProcIds),
	dead_proc_elim__initialize_procs(PredId, ProcIds,
		Queue0, Queue1, Needed0, Needed1),
	dead_proc_elim__initialize_preds(PredIds, PredTable,
		Queue1, Queue, Needed1, Needed).

	% Add the listed procedures to the queue and map.

:- pred dead_proc_elim__initialize_procs(pred_id, list(proc_id),
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__initialize_procs(in, in, in, out, in, out) is det.

dead_proc_elim__initialize_procs(_PredId, [], Queue, Queue, Needed, Needed).
dead_proc_elim__initialize_procs(PredId, [ProcId | ProcIds],
		Queue0, Queue, Needed0, Needed) :-
	queue__put(Queue0, proc(PredId, ProcId), Queue1),
	map__set(Needed0, proc(PredId, ProcId), no, Needed1),
	dead_proc_elim__initialize_procs(PredId, ProcIds,
		Queue1, Queue, Needed1, Needed).

	% Add procedures exported to C by a pragma(export, ...) declaration
	% to the queue and map.

:- pred dead_proc_elim__initialize_pragma_exports(list(pragma_exported_proc),
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__initialize_pragma_exports(in, in, out, in, out) is det.

dead_proc_elim__initialize_pragma_exports([], Queue, Queue, Needed, Needed).
dead_proc_elim__initialize_pragma_exports([PragmaProc | PragmaProcs],
		Queue0, Queue, Needed0, Needed) :-
	PragmaProc = pragma_exported_proc(PredId, ProcId, _CFunction),
	queue__put(Queue0, proc(PredId, ProcId), Queue1),
	map__set(Needed0, proc(PredId, ProcId), no, Needed1),
	dead_proc_elim__initialize_pragma_exports(PragmaProcs,
		Queue1, Queue, Needed1, Needed).

:- pred dead_proc_elim__initialize_base_gen_infos(list(base_gen_info),
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__initialize_base_gen_infos(in, in, out, in, out) is det.

dead_proc_elim__initialize_base_gen_infos([], Queue, Queue, Needed, Needed).
dead_proc_elim__initialize_base_gen_infos([BaseGenInfo | BaseGenInfos],
		Queue0, Queue, Needed0, Needed) :-
	BaseGenInfo = base_gen_info(_TypeId, ModuleName, TypeName,
		Arity, _Status, _Elim, _Procs),
	(
		% XXX: We'd like to do this, but there are problems.
		% ( Status = exported
		% ; Status = abstract_exported
		% )
		% We need to do more thorough analysis of the
		% reachability of the special predicates, in general,
		% because using arg/3 allows us to get at base_type_info
		% via the base_type_layout. The base_type_infos of
		% arguments of functors may have had their special preds
		% eliminated, but they can still be called. In addition,
		% it would be nice for pragma C code to have some
		% support for using compiler generated data structures
		% and preds, so that they aren't just eliminated.
		%
		% So presently, all base_type_infos will be treated
		% as exported, and hence no special preds will be
		% eliminated.
		semidet_succeed
	->
		Entity = base_gen_info(ModuleName, TypeName, Arity),
		queue__put(Queue0, Entity, Queue1),
		map__set(Needed0, Entity, no, Needed1)
	;
		Queue1 = Queue0,
		Needed1 = Needed0
	),
	dead_proc_elim__initialize_base_gen_infos(BaseGenInfos,
		Queue1, Queue, Needed1, Needed).

:- pred dead_proc_elim__initialize_class_methods(instance_table, 
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__initialize_class_methods(in, in, out, in, out) is det.

dead_proc_elim__initialize_class_methods(Instances, Queue0, Queue, 
		Needed0, Needed) :-
	map__values(Instances, InstanceDefns0),
	list__condense(InstanceDefns0, InstanceDefns),
	list__foldl2(get_instance_pred_procs, InstanceDefns, Queue0, Queue,
		Needed0, Needed).

:- pred get_instance_pred_procs(hlds_instance_defn, entity_queue, entity_queue,
	needed_map, needed_map).
:- mode get_instance_pred_procs(in, in, out, in, out) is det.

get_instance_pred_procs(Instance, Queue0, Queue, Needed0, Needed) :-
	Instance = hlds_instance_defn(ImportStatus, _, _, _, PredProcIds, _, _),
	(
			% We only need the instance declarations which were
			% made in this module.
		( ImportStatus = exported
		; ImportStatus = abstract_exported 
		; ImportStatus = pseudo_exported
		; ImportStatus = local
		)
	->
		get_instance_pred_procs2(PredProcIds, Queue0, Queue, 
			Needed0, Needed)
	;
		Queue = Queue0,
		Needed = Needed0
	).

:- pred get_instance_pred_procs2(maybe(list(hlds_class_proc)), 
	entity_queue, entity_queue, needed_map, needed_map).
:- mode get_instance_pred_procs2(in, in, out, in, out) is det.

get_instance_pred_procs2(PredProcIds, Queue0, Queue, Needed0, Needed) :-
	(
			% This should never happen
		PredProcIds = no,
		Queue = Queue0,
		Needed = Needed0
	;
		PredProcIds = yes(Ids),
		AddHldsClassProc = lambda(
			[PredProc::in, Q0::in, Q::out, N0::in, N::out] is det,
			(
				PredProc = hlds_class_proc(PredId, ProcId),
				queue__put(Q0, proc(PredId, ProcId), Q),
				map__set(N0, proc(PredId, ProcId), no, N)
			)),
		list__foldl2(AddHldsClassProc, Ids, Queue0, Queue, 
			Needed0, Needed)
	).

%-----------------------------------------------------------------------------%

:- pred dead_proc_elim__examine(entity_queue, examined_set, module_info,
	needed_map, needed_map).
:- mode dead_proc_elim__examine(in, in, in, in, out) is det.

dead_proc_elim__examine(Queue0, Examined0, ModuleInfo, Needed0, Needed) :-
	% see if the queue is empty
	( queue__get(Queue0, Entity, Queue1) ->
		% see if the next element has been examined before
		( set__member(Entity, Examined0) ->
			dead_proc_elim__examine(Queue1, Examined0, ModuleInfo,
				Needed0, Needed)
		;
			set__insert(Examined0, Entity, Examined1),
			(
				Entity = proc(PredId, ProcId),
				PredProcId = proc(PredId, ProcId),
				dead_proc_elim__examine_proc(
					PredProcId, ModuleInfo,
					Queue1, Queue2, Needed0, Needed1)
			;
				Entity = base_gen_info(Module, Type, Arity),
				dead_proc_elim__examine_base_gen_info(
					Module, Type, Arity, ModuleInfo,
					Queue1, Queue2, Needed0, Needed1)
			),
			dead_proc_elim__examine(Queue2, Examined1, ModuleInfo,
				Needed1, Needed)
		)
	;
		Needed = Needed0
	).

%-----------------------------------------------------------------------------%

:- pred dead_proc_elim__examine_base_gen_info(module_name, string, arity,
	module_info, entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__examine_base_gen_info(in, in, in, in, in, out, in, out)
	is det.

dead_proc_elim__examine_base_gen_info(ModuleName, TypeName, Arity, ModuleInfo,
		Queue0, Queue, Needed0, Needed) :-
	module_info_base_gen_infos(ModuleInfo, BaseGenInfos),
	(
		dead_proc_elim__find_base_gen_info(ModuleName, TypeName,
			Arity, BaseGenInfos, Refs)
	->
		dead_proc_elim__examine_refs(Refs, Queue0, Queue,
			Needed0, Needed)
	;
		Queue = Queue0,
		Needed = Needed0
	).

:- pred dead_proc_elim__find_base_gen_info(module_name, string, arity,
	list(base_gen_info), list(pred_proc_id)).
:- mode dead_proc_elim__find_base_gen_info(in, in, in, in, out) is semidet.

dead_proc_elim__find_base_gen_info(ModuleName, TypeName, TypeArity,
		[BaseGenInfo | BaseGenInfos], Refs) :-
	(
		BaseGenInfo = base_gen_info(_TypeId, ModuleName, TypeName,
			TypeArity, _Status, _Elim, Refs0)
	->
		Refs = Refs0
	;
		dead_proc_elim__find_base_gen_info(ModuleName, TypeName,
			TypeArity, BaseGenInfos, Refs)
	).

:- pred dead_proc_elim__examine_refs(list(pred_proc_id),
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__examine_refs(in, in, out, in, out) is det.

dead_proc_elim__examine_refs([], Queue, Queue, Needed, Needed).
dead_proc_elim__examine_refs([Ref | Refs], Queue0, Queue, Needed0, Needed) :-
	Ref = proc(PredId, ProcId),
	Entity = proc(PredId, ProcId),
	queue__put(Queue0, Entity, Queue1),
	map__set(Needed0, Entity, no, Needed1),
	dead_proc_elim__examine_refs(Refs, Queue1, Queue, Needed1, Needed).

%-----------------------------------------------------------------------------%

:- pred dead_proc_elim__examine_proc(pred_proc_id, module_info,
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__examine_proc(in, in, in, out, in, out) is det.

dead_proc_elim__examine_proc(proc(PredId, ProcId), ModuleInfo, Queue0, Queue,
		Needed0, Needed) :-
	(
		module_info_preds(ModuleInfo, PredTable),
		map__lookup(PredTable, PredId, PredInfo),
		pred_info_non_imported_procids(PredInfo, ProcIds),
		list__member(ProcId, ProcIds),
		pred_info_procedures(PredInfo, ProcTable),
		map__lookup(ProcTable, ProcId, ProcInfo)
	->
		proc_info_goal(ProcInfo, Goal),
		dead_proc_elim__examine_goal(Goal, proc(PredId, ProcId),
			Queue0, Queue, Needed0, Needed)
	;
		Queue = Queue0,
		Needed = Needed0
	).

:- pred dead_proc_elim__examine_goals(list(hlds_goal), pred_proc_id,
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__examine_goals(in, in, in, out, in, out) is det.

dead_proc_elim__examine_goals([], _, Queue, Queue, Needed, Needed).
dead_proc_elim__examine_goals([Goal | Goals], CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goal(Goal, CurrProc, Queue0, Queue1,
		Needed0, Needed1),
	dead_proc_elim__examine_goals(Goals, CurrProc, Queue1, Queue,
		Needed1, Needed).

:- pred dead_proc_elim__examine_cases(list(case), pred_proc_id,
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__examine_cases(in, in, in, out, in, out) is det.

dead_proc_elim__examine_cases([], _CurrProc, Queue, Queue, Needed, Needed).
dead_proc_elim__examine_cases([case(_, _, Goal) | Cases], CurrProc,
		Queue0, Queue, Needed0, Needed) :-
	dead_proc_elim__examine_goal(Goal, CurrProc, Queue0, Queue1,
		Needed0, Needed1),
	dead_proc_elim__examine_cases(Cases, CurrProc, Queue1, Queue,
		Needed1, Needed).

:- pred dead_proc_elim__examine_goal(hlds_goal, pred_proc_id,
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__examine_goal(in, in, in, out, in, out) is det.

dead_proc_elim__examine_goal(GoalExpr - _, CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_expr(GoalExpr, CurrProc, Queue0, Queue,
		Needed0, Needed).

:- pred dead_proc_elim__examine_expr(hlds_goal_expr, pred_proc_id,
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__examine_expr(in, in, in, out, in, out) is det.

dead_proc_elim__examine_expr(disj(Goals, _), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goals(Goals, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(conj(Goals), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goals(Goals, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(not(Goal), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goal(Goal, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(some(_, Goal), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goal(Goal, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(switch(_, _, Cases, _), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_cases(Cases, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(if_then_else(_, Cond, Then, Else, _),
		CurrProc, Queue0, Queue, Needed0, Needed) :-
	dead_proc_elim__examine_goal(Cond, CurrProc, Queue0, Queue1,
		Needed0, Needed1),
	dead_proc_elim__examine_goal(Then, CurrProc, Queue1, Queue2,
		Needed1, Needed2),
	dead_proc_elim__examine_goal(Else, CurrProc, Queue2, Queue,
		Needed2, Needed).
dead_proc_elim__examine_expr(higher_order_call(_,_,_,_,_,_), _,
		Queue, Queue, Needed, Needed).
dead_proc_elim__examine_expr(class_method_call(_,_,_,_,_,_), _,
		Queue, Queue, Needed, Needed).
dead_proc_elim__examine_expr(call(PredId, ProcId, _,_,_,_),
		CurrProc, Queue0, Queue, Needed0, Needed) :-
	queue__put(Queue0, proc(PredId, ProcId), Queue),
	( proc(PredId, ProcId) = CurrProc ->
		% if it's reachable and recursive, then we can't
		% eliminate or inline it
		NewNotation = no,
		map__set(Needed0, proc(PredId, ProcId), NewNotation, Needed)
	; map__search(Needed0, proc(PredId, ProcId), OldNotation) ->
		(
			OldNotation = no,
			NewNotation = no
		;
			OldNotation = yes(Count0),
			Count is Count0 + 1,
			NewNotation = yes(Count)
		),
		map__det_update(Needed0, proc(PredId, ProcId), NewNotation,
			Needed)
	;
		NewNotation = yes(1),
		map__set(Needed0, proc(PredId, ProcId), NewNotation, Needed)
	).
dead_proc_elim__examine_expr(pragma_c_code(_, PredId, ProcId, _, _, _, _),
		_CurrProc, Queue0, Queue, Needed0, Needed) :-
	queue__put(Queue0, proc(PredId, ProcId), Queue),
	map__set(Needed0, proc(PredId, ProcId), no, Needed).
dead_proc_elim__examine_expr(unify(_,_,_, Uni, _), _CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	(
		Uni = construct(_, ConsId, _, _),
		(
			ConsId = pred_const(PredId, ProcId),
			Entity = proc(PredId, ProcId)
		;
			ConsId = code_addr_const(PredId, ProcId),
			Entity = proc(PredId, ProcId)
		;
			ConsId = base_type_info_const(Module, TypeName, Arity),
			Entity = base_gen_info(Module, TypeName, Arity)
		)
	->
		queue__put(Queue0, Entity, Queue),
		map__set(Needed0, Entity, no, Needed)
	;
		Queue = Queue0,
		Needed = Needed0
	).

%-----------------------------------------------------------------------------%


		% information used during the elimination phase.

:- type elim_info
	--->	elimination_info(
			needed_map,	% collected usage counts 
			module_info,	% ye olde module_info
			pred_table	% table of predicates in this module:
					% preds and procs in this table
					% may be eliminated
		).
			
dead_proc_elim__eliminate(ModuleInfo0, Needed0, ModuleInfo, State0, State) :-
	module_info_predids(ModuleInfo0, PredIds),
	module_info_preds(ModuleInfo0, PredTable0),

	ElimInfo0 = elimination_info(Needed0, ModuleInfo0, PredTable0),
	list__foldl2(dead_proc_elim__eliminate_pred, PredIds, ElimInfo0, 
		ElimInfo, State0, State),
	ElimInfo = elimination_info(Needed, ModuleInfo1, PredTable),

	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo2),
	module_info_base_gen_infos(ModuleInfo2, BaseGenInfos0),
	dead_proc_elim__eliminate_base_gen_infos(BaseGenInfos0, Needed,
		BaseGenInfos),
	module_info_set_base_gen_infos(ModuleInfo2, BaseGenInfos, ModuleInfo).


		% eliminate any unused procedures for this pred

:- pred dead_proc_elim__eliminate_pred(pred_id, elim_info, elim_info,
	io__state, io__state).
:- mode dead_proc_elim__eliminate_pred(in, in, out, di, uo) is det.

dead_proc_elim__eliminate_pred(PredId, ElimInfo0, ElimInfo, State0, State) :-
	ElimInfo0 = elimination_info(Needed, ModuleInfo, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_import_status(PredInfo0, Status),
	(
		% Find out if the predicate is defined in this module.
		% If yes, find out also whether any of its procedures
		% must be kept.
		( Status = local,
			Keep = no
		; Status = pseudo_imported,
			Keep = no
		; Status = pseudo_exported,
			hlds_pred__in_in_unification_proc_id(InitProcId),
			Keep = yes(InitProcId)
		)
	->
		pred_info_procids(PredInfo0, ProcIds),
		pred_info_procedures(PredInfo0, ProcTable0),
		list__foldl2(dead_proc_elim__eliminate_proc(PredId, Keep, 
			ElimInfo0),
			ProcIds, ProcTable0, ProcTable, State0, State),
		pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
		map__det_update(PredTable0, PredId, PredInfo, PredTable)
	;
		% Don't generate code in the current module for
		% unoptimized opt_imported preds
		Status = opt_imported
	->
		pred_info_procids(PredInfo0, ProcIds),
		pred_info_procedures(PredInfo0, ProcTable0),
			% Reduce memory usage by replacing the goals with 
			% conj([]).
			% XXX this looks fishy to me - zs
		DestroyGoal =
			lambda([Id::in, PTable0::in, PTable::out] is det, (
				map__lookup(ProcTable0, Id, ProcInfo0),
				goal_info_init(GoalInfo),
				Goal = conj([]) - GoalInfo,
				proc_info_set_goal(ProcInfo0, Goal, ProcInfo),
				map__det_update(PTable0, Id, ProcInfo, PTable)
			)),
		list__foldl(DestroyGoal, ProcIds, ProcTable0, ProcTable),
		pred_info_set_procedures(PredInfo0, ProcTable, PredInfo1),
		pred_info_set_import_status(PredInfo1, imported, PredInfo),
		map__det_update(PredTable0, PredId, PredInfo, PredTable),
		globals__io_lookup_bool_option(very_verbose, VeryVerbose,
			State0, State1),
		( VeryVerbose = yes ->
			write_pred_progress_message(
				"% Eliminated opt_imported predicate ",
				PredId, ModuleInfo, State1, State)
		;
			State = State1
		)
	;
		% This predicate is not defined in this module.
		State = State0,
		PredTable = PredTable0
	),
	ElimInfo = elimination_info(Needed, ModuleInfo, PredTable).


		% eliminate a procedure, if unused

:- pred dead_proc_elim__eliminate_proc(pred_id, maybe(proc_id), elim_info,
	proc_id, proc_table, proc_table, io__state, io__state).
:- mode dead_proc_elim__eliminate_proc(in, in, in, in, in, out, di, uo) is det.

dead_proc_elim__eliminate_proc(PredId, Keep, ElimInfo, ProcId, 
		ProcTable0, ProcTable) -->
	{ ElimInfo = elimination_info(Needed, ModuleInfo, _PredTable) },
	(
		% Keep the procedure if it is in the needed map
		% or if it is to be kept because it is exported.
		( { map__search(Needed, proc(PredId, ProcId), _) }
		; { Keep = yes(ProcId) }
		)
	->
		{ ProcTable = ProcTable0 }
	;
		globals__io_lookup_bool_option(very_verbose, VeryVerbose),
		( { VeryVerbose = yes } ->
			write_proc_progress_message(
				"% Eliminated the dead procedure ",
				PredId, ProcId, ModuleInfo)
		;
			[]
		),
		{ map__delete(ProcTable0, ProcId, ProcTable) }
	).

:- pred dead_proc_elim__eliminate_base_gen_infos(list(base_gen_info),
	needed_map, list(base_gen_info)).
:- mode dead_proc_elim__eliminate_base_gen_infos(in, in, out) is det.

dead_proc_elim__eliminate_base_gen_infos([], _Needed, []).
dead_proc_elim__eliminate_base_gen_infos([BaseGenInfo0 | BaseGenInfos0], Needed,
		BaseGenInfos) :-
	dead_proc_elim__eliminate_base_gen_infos(BaseGenInfos0, Needed,	
		BaseGenInfos1),
	BaseGenInfo0 = base_gen_info(TypeId, ModuleName, TypeName,
		Arity, Status, Elim0, Procs),
	(
		Entity = base_gen_info(ModuleName, TypeName, Arity),
		map__search(Needed, Entity, _)
	->
		BaseGenInfos = [BaseGenInfo0 | BaseGenInfos1]
	;
		list__length(Procs, ProcsLength),

			% Procs may have been eliminated elsewhere, if so
			% we sum the eliminated procs together.
		(
			Elim0 = yes(NumProcs0)
		->
			NumProcs is ProcsLength + NumProcs0
		;
			NumProcs = ProcsLength
		),
		NeuteredBaseGenInfo = base_gen_info(TypeId, ModuleName, 
			TypeName, Arity, Status, yes(NumProcs), []),
		BaseGenInfos = [NeuteredBaseGenInfo | BaseGenInfos1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type dead_pred_info
	--->	dead_pred_info(
			module_info,
			queue(pred_id),	% preds to examine.
			set(pred_id),	% preds examined.
			set(pred_id),	% needed pred_ids.
			set(sym_name)	% pred names needed.
		).

dead_pred_elim(ModuleInfo0, ModuleInfo) :-
	module_info_predids(ModuleInfo0, PredIds),
	queue__init(Queue0),
	set__init(Preds0),
	set__init(Names0),
	DeadInfo0 = dead_pred_info(ModuleInfo0, Queue0, 
		Preds0, Preds0, Names0),
	list__foldl(dead_pred_elim_initialize, PredIds, 
		DeadInfo0, DeadInfo1),
	dead_pred_elim_analyze(DeadInfo1, DeadInfo),
	DeadInfo = dead_pred_info(ModuleInfo1, _, _, NeededPreds, _),
	set__list_to_set(PredIds, PredIdSet),
	set__difference(PredIdSet, NeededPreds, DeadPreds),
	set__to_sorted_list(DeadPreds, DeadPredList),
	list__foldl(module_info_remove_predicate, DeadPredList,
		ModuleInfo1, ModuleInfo).

:- pred dead_pred_elim_initialize(pred_id::in, dead_pred_info::in,
		dead_pred_info::out) is det.

dead_pred_elim_initialize(PredId, DeadInfo0, DeadInfo) :-
	DeadInfo0 = dead_pred_info(ModuleInfo, Q0, Ex, Needed, NeededNames0),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	( 
		pred_info_module(PredInfo, PredModule),
		pred_info_name(PredInfo, PredName),
		pred_info_arity(PredInfo, PredArity),
		(
			% Don't eliminate special preds since they won't
			% be actually called from the HLDS until after 
			% polymorphism.
			code_util__compiler_generated(PredInfo)
		;
			% Don't eliminate preds from mercury_builtin.m since
			% polymorphism.m needs unify/2 and friends.
			mercury_public_builtin_module(PredModule)
		;
			mercury_private_builtin_module(PredModule)
		;
			% Don't attempt to eliminate local preds here, since we
			% want to do semantic checking on those even if they 
			% aren't used.
			\+ pred_info_is_imported(PredInfo), 
			\+ pred_info_import_status(PredInfo, opt_imported)
		;
			% Don't eliminate <foo>_init_any/1 predicates;
			% modes.m may insert calls to them to initialize
			% variables from inst `free' to inst `any'.
			string__remove_suffix(PredName, "_init_any", _),
			PredArity = 1
		)
	->
		set__insert(NeededNames0, qualified(PredModule, PredName), 
			NeededNames),
		queue__put(Q0, PredId, Q)
	;
		NeededNames = NeededNames0,
		Q = Q0
	),
	DeadInfo = dead_pred_info(ModuleInfo, Q, Ex, Needed, NeededNames).

:- pred dead_pred_elim_analyze(dead_pred_info::in, 
		dead_pred_info::out) is det.

dead_pred_elim_analyze(DeadInfo0, DeadInfo) :-
	DeadInfo0 = dead_pred_info(ModuleInfo, Q0, Ex0, Needed0, NeededNames),
	( queue__get(Q0, PredId, Q) ->
		( set__member(PredId, Ex0) ->
			DeadInfo2 = dead_pred_info(ModuleInfo, Q,
				Ex0, Needed0, NeededNames)
		;
			set__insert(Needed0, PredId, Needed),
			set__insert(Ex0, PredId, Ex),
			DeadInfo1 = dead_pred_info(ModuleInfo, Q, Ex, 
				Needed, NeededNames),
			module_info_pred_info(ModuleInfo, PredId, PredInfo),
			pred_info_clauses_info(PredInfo, ClausesInfo),
			ClausesInfo = clauses_info(_,_,_,_, Clauses),
			list__foldl(dead_pred_elim_process_clause, Clauses,
				DeadInfo1, DeadInfo2)
		),
		dead_pred_elim_analyze(DeadInfo2, DeadInfo)
	;
		DeadInfo = DeadInfo0
	).

:- pred dead_pred_elim_process_clause(clause::in, dead_pred_info::in, 
		dead_pred_info::out) is det.

dead_pred_elim_process_clause(clause(_, Goal, _)) -->
	pre_modecheck_examine_goal(Goal).

:- pred pre_modecheck_examine_goal(hlds_goal::in, 
		dead_pred_info::in, dead_pred_info::out) is det.

pre_modecheck_examine_goal(conj(Goals) - _) -->
	list__foldl(pre_modecheck_examine_goal, Goals).
pre_modecheck_examine_goal(disj(Goals, _) - _) -->
	list__foldl(pre_modecheck_examine_goal, Goals).
pre_modecheck_examine_goal(if_then_else(_, If, Then, Else, _) - _) -->
	list__foldl(pre_modecheck_examine_goal, [If, Then, Else]).
pre_modecheck_examine_goal(switch(_, _, Cases, _) - _) -->
	{ ExamineCase = lambda([Case::in, Info0::in, Info::out] is det, (
		Case = case(_, _, Goal),
		pre_modecheck_examine_goal(Goal, Info0, Info)
	)) },
	list__foldl(ExamineCase, Cases).
pre_modecheck_examine_goal(higher_order_call(_,_,_,_,_,_) - _) --> [].
pre_modecheck_examine_goal(class_method_call(_,_,_,_,_,_) - _) --> [].
pre_modecheck_examine_goal(not(Goal) - _) -->
	pre_modecheck_examine_goal(Goal).
pre_modecheck_examine_goal(some(_, Goal) - _) -->
	pre_modecheck_examine_goal(Goal).
pre_modecheck_examine_goal(call(_, _, _, _, _, PredName) - _) -->
	dead_pred_info_add_pred_name(PredName).
pre_modecheck_examine_goal(pragma_c_code(_, _, _, _, _, _, _) - _) --> [].
pre_modecheck_examine_goal(unify(_, Rhs, _, _, _) - _) -->
	pre_modecheck_examine_unify_rhs(Rhs).

:- pred pre_modecheck_examine_unify_rhs(unify_rhs::in, 
		dead_pred_info::in, dead_pred_info::out) is det.

pre_modecheck_examine_unify_rhs(var(_)) --> [].
pre_modecheck_examine_unify_rhs(functor(Functor, _)) -->
	( { Functor = cons(Name, _) } ->
		dead_pred_info_add_pred_name(Name)
	;
		[]
	).
pre_modecheck_examine_unify_rhs(lambda_goal(_, _, _, _, _, _, Goal)) -->
	pre_modecheck_examine_goal(Goal).

:- pred dead_pred_info_add_pred_name(sym_name::in, dead_pred_info::in, 
		dead_pred_info::out) is det.

dead_pred_info_add_pred_name(Name, DeadInfo0, DeadInfo) :-
	DeadInfo0 = dead_pred_info(ModuleInfo, Q0, Ex, Needed, NeededNames0),
	( set__member(Name, NeededNames0) ->	
		DeadInfo = DeadInfo0
	;
		module_info_get_predicate_table(ModuleInfo, PredicateTable),
		set__insert(NeededNames0, Name, NeededNames),
		( predicate_table_search_sym(PredicateTable, Name, PredIds) ->
			queue__put_list(Q0, PredIds, Q)
		;
			Q = Q0
		),
		DeadInfo = dead_pred_info(ModuleInfo, Q, Ex,
			Needed, NeededNames)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
