%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% The job of this module is to delete dead procedures and base_gen_info
% structures from the HLDS.
%
% It also computes the usage counts that inlining.m uses for the
% `--inline-single-use' option.
%
% Main author: zs.
%
%-----------------------------------------------------------------------------%

:- module dead_proc_elim.

:- interface.

:- import_module hlds_module, io.

:- pred dead_proc_elim(module_info, module_info, io__state, io__state).
:- mode dead_proc_elim(in, out, di, uo) is det.

:- pred dead_proc_elim__analyze(module_info, needed_map).
:- mode dead_proc_elim__analyze(in, out) is det.

:- pred dead_proc_elim__eliminate(module_info, needed_map, module_info,
	io__state, io__state).
:- mode dead_proc_elim__eliminate(in, in, out, di, uo) is det.

:- type entity		--->	proc(pred_id, proc_id)
			;	base_gen_info(string, string, int).

:- type needed_map ==	map(entity, maybe(int)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_pred, hlds_goal, hlds_data, prog_data, llds.
:- import_module passes_aux, globals, options.
:- import_module int, list, set, queue, map, bool, std_util, require.

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
		Queue2, Queue, Needed2, Needed).

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

:- pred dead_proc_elim__examine_base_gen_info(string, string, int, module_info,
	entity_queue, entity_queue, needed_map, needed_map).
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

:- pred dead_proc_elim__find_base_gen_info(string, string, int,
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
dead_proc_elim__examine_cases([case(_, Goal) | Cases], CurrProc, Queue0, Queue,
		Needed0, Needed) :-
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
dead_proc_elim__examine_expr(higher_order_call(_,_,_,_,_), _,
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
dead_proc_elim__examine_expr(pragma_c_code(_, _, PredId, ProcId, _, _, _, _),
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
