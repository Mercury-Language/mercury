%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% The job of this module is to delete dead predicates, procedures 
% and type_ctor_gen_info structures from the HLDS.
%
% It also computes the usage counts that inlining.m uses for the
% `--inline-single-use' option.
%
% It also issues warnings about unused procedures.
%
% Main author: zs.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__dead_proc_elim.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module map, std_util, io.

:- type dead_proc_pass
	--->	warning_pass
	;	final_optimization_pass
	.

	% Eliminate dead procedures.
	% If the first argument is `warning_pass',
	% also warn about any user-defined procedures that are dead.
	% If the first argument is `final_optimization_pass',
	% also eliminate any opt_imported procedures.
:- pred dead_proc_elim(dead_proc_pass, module_info, module_info,
		io__state, io__state).
:- mode dead_proc_elim(in, in, out, di, uo) is det.

	% Analyze which entities are needed, and for those entities
	% which are needed, record how many times they are referenced
	% (this information is used by our inlining heuristics).
:- pred dead_proc_elim__analyze(module_info, needed_map).
:- mode dead_proc_elim__analyze(in, out) is det.

	% Optimize away any dead predicates.
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

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__passes_aux.
:- import_module hlds__error_util.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend__llds.
:- import_module parse_tree__prog_util.

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

dead_proc_elim(Pass, ModuleInfo0, ModuleInfo, State0, State) :-
	dead_proc_elim__analyze(ModuleInfo0, Needed),
	dead_proc_elim__eliminate(Pass, ModuleInfo0, Needed, ModuleInfo,
		State0, State).

%-----------------------------------------------------------------------------%

	% Find all needed entities.

dead_proc_elim__analyze(ModuleInfo0, Needed) :-
	set__init(Examined0),
	dead_proc_elim__initialize(ModuleInfo0, Queue0, Needed0),
	dead_proc_elim__examine(Queue0, Examined0, ModuleInfo0,
		Needed0, Needed).

	% Add all exported entities to the queue and map.
	% Note: changes here are likely to require changes to
	% dead_pred_elim as well.

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
	module_info_type_ctor_gen_infos(ModuleInfo, TypeCtorGenInfos),
	dead_proc_elim__initialize_base_gen_infos(TypeCtorGenInfos,
		Queue2, Queue3, Needed2, Needed3),
	module_info_classes(ModuleInfo, Classes),
	module_info_instances(ModuleInfo, Instances),
	dead_proc_elim__initialize_class_methods(Classes, Instances,
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
	PragmaProc = pragma_exported_proc(PredId, ProcId, _CFunction, _Ctxt),
	queue__put(Queue0, proc(PredId, ProcId), Queue1),
	map__set(Needed0, proc(PredId, ProcId), no, Needed1),
	dead_proc_elim__initialize_pragma_exports(PragmaProcs,
		Queue1, Queue, Needed1, Needed).

:- pred dead_proc_elim__initialize_base_gen_infos(list(type_ctor_gen_info),
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__initialize_base_gen_infos(in, in, out, in, out) is det.

dead_proc_elim__initialize_base_gen_infos([], Queue, Queue, Needed, Needed).
dead_proc_elim__initialize_base_gen_infos([TypeCtorGenInfo | TypeCtorGenInfos],
		Queue0, Queue, Needed0, Needed) :-
	TypeCtorGenInfo = type_ctor_gen_info(_TypeCtor, ModuleName, TypeName,
		Arity, _Status, _HldsDefn, _Unify, _Compare),
	(
		% XXX: We'd like to do this, but there are problems.
		% status_is_exported(Status, yes)
		%
		% We need to do more thorough analysis of the
		% reachability of the special predicates, in general,
		% because using arg/3 allows us to get at type_ctor_info
		% via the type_ctor_layout. The type_ctor_infos of
		% arguments of functors may have had their special preds
		% eliminated, but they can still be called. In addition,
		% it would be nice for pragma C code to have some
		% support for using compiler generated data structures
		% and preds, so that they aren't just eliminated.
		%
		% So presently, all type_ctor_infos will be treated
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
	dead_proc_elim__initialize_base_gen_infos(TypeCtorGenInfos,
		Queue1, Queue, Needed1, Needed).

:- pred dead_proc_elim__initialize_class_methods(class_table, instance_table, 
	entity_queue, entity_queue, needed_map, needed_map).
:- mode dead_proc_elim__initialize_class_methods(in, in,
	in, out, in, out) is det.

dead_proc_elim__initialize_class_methods(Classes, Instances, Queue0, Queue, 
		Needed0, Needed) :-
	map__values(Instances, InstanceDefns0),
	list__condense(InstanceDefns0, InstanceDefns),
	list__foldl2(get_instance_pred_procs, InstanceDefns, Queue0, Queue1,
		Needed0, Needed1),
	map__values(Classes, ClassDefns),
	list__foldl2(get_class_pred_procs, ClassDefns, Queue1, Queue,
		Needed1, Needed).

:- pred get_instance_pred_procs(hlds_instance_defn, entity_queue, entity_queue,
	needed_map, needed_map).
:- mode get_instance_pred_procs(in, in, out, in, out) is det.

get_instance_pred_procs(Instance, Queue0, Queue, Needed0, Needed) :-
	Instance = hlds_instance_defn(_, _, _, _, _, _, PredProcIds, _, _),

	%
	% We need to keep the instance methods for all instances
	% for optimization of method lookups.
	%
	(
			% This should never happen
		PredProcIds = no,
		Queue = Queue0,
		Needed = Needed0
	;
		PredProcIds = yes(Ids),
		get_class_interface_pred_procs(Ids, Queue0, Queue,
			Needed0, Needed)
	).

:- pred get_class_pred_procs(hlds_class_defn, entity_queue, entity_queue,
		needed_map, needed_map).
:- mode get_class_pred_procs(in, in, out, in, out) is det.

get_class_pred_procs(Class, Queue0, Queue, Needed0, Needed) :-
	Class = hlds_class_defn(_, _, _, _, Methods, _, _),
	get_class_interface_pred_procs(Methods,
		Queue0, Queue, Needed0, Needed).

:- pred get_class_interface_pred_procs(list(hlds_class_proc), 
	entity_queue, entity_queue, needed_map, needed_map).
:- mode get_class_interface_pred_procs(in, in, out, in, out) is det.

get_class_interface_pred_procs(Ids, Queue0, Queue, Needed0, Needed) :-
	AddHldsClassProc = lambda(
		[PredProc::in, Q0::in, Q::out, N0::in, N::out] is det,
		(
			PredProc = hlds_class_proc(PredId, ProcId),
			queue__put(Q0, proc(PredId, ProcId), Q),
			map__set(N0, proc(PredId, ProcId), no, N)
		)),
	list__foldl2(AddHldsClassProc, Ids, Queue0, Queue, 
		Needed0, Needed).

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
	module_info_type_ctor_gen_infos(ModuleInfo, TypeCtorGenInfos),
	(
		dead_proc_elim__find_base_gen_info(ModuleName, TypeName,
			Arity, TypeCtorGenInfos, Refs)
	->
		dead_proc_elim__examine_refs(Refs, Queue0, Queue,
			Needed0, Needed)
	;
		Queue = Queue0,
		Needed = Needed0
	).

:- pred dead_proc_elim__find_base_gen_info(module_name, string, arity,
	list(type_ctor_gen_info), list(pred_proc_id)).
:- mode dead_proc_elim__find_base_gen_info(in, in, in, in, out) is semidet.

dead_proc_elim__find_base_gen_info(ModuleName, TypeName, TypeArity,
		[TypeCtorGenInfo | TypeCtorGenInfos], Refs) :-
	(
		TypeCtorGenInfo = type_ctor_gen_info(_TypeCtor, ModuleName,
			TypeName, TypeArity, _Status, _HldsDefn,
			Unify, Compare)
	->
		Refs = [Unify, Compare]
		% dead_proc_elim__maybe_add_ref(MaybePretty,  Refs0, Refs)
	;
		dead_proc_elim__find_base_gen_info(ModuleName, TypeName,
			TypeArity, TypeCtorGenInfos, Refs)
	).

:- pred dead_proc_elim__maybe_add_ref(maybe(pred_proc_id),
	list(pred_proc_id), list(pred_proc_id)).
:- mode dead_proc_elim__maybe_add_ref(in, in, out) is det.

dead_proc_elim__maybe_add_ref(no, Refs, Refs).
dead_proc_elim__maybe_add_ref(yes(Ref), Refs, [Ref | Refs]).

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

dead_proc_elim__examine_expr(disj(Goals), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goals(Goals, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(conj(Goals), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goals(Goals, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(par_conj(Goals), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goals(Goals, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(not(Goal), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goal(Goal, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(some(_, _, Goal), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_goal(Goal, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(switch(_, _, Cases), CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	dead_proc_elim__examine_cases(Cases, CurrProc, Queue0, Queue,
		Needed0, Needed).
dead_proc_elim__examine_expr(if_then_else(_, Cond, Then, Else),
		CurrProc, Queue0, Queue, Needed0, Needed) :-
	dead_proc_elim__examine_goal(Cond, CurrProc, Queue0, Queue1,
		Needed0, Needed1),
	dead_proc_elim__examine_goal(Then, CurrProc, Queue1, Queue2,
		Needed1, Needed2),
	dead_proc_elim__examine_goal(Else, CurrProc, Queue2, Queue,
		Needed2, Needed).
dead_proc_elim__examine_expr(generic_call(_,_,_,_), _,
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
			Count = Count0 + 1,
			NewNotation = yes(Count)
		),
		map__det_update(Needed0, proc(PredId, ProcId), NewNotation,
			Needed)
	;
		NewNotation = yes(1),
		map__set(Needed0, proc(PredId, ProcId), NewNotation, Needed)
	).
dead_proc_elim__examine_expr(foreign_proc(_, PredId, ProcId, _, 
		_, _, _), _CurrProc, Queue0, Queue, Needed0, Needed) :-
	queue__put(Queue0, proc(PredId, ProcId), Queue),
	map__set(Needed0, proc(PredId, ProcId), no, Needed).
dead_proc_elim__examine_expr(unify(_,_,_, Uni, _), _CurrProc, Queue0, Queue,
		Needed0, Needed) :-
	(
		Uni = construct(_, ConsId, _, _, _, _, _),
		(
			ConsId = pred_const(PredId, ProcId, _),
			Entity = proc(PredId, ProcId)
		;
			ConsId = type_ctor_info_const(Module, TypeName, Arity),
			Entity = base_gen_info(Module, TypeName, Arity)
		)
	->
		queue__put(Queue0, Entity, Queue),
		map__set(Needed0, Entity, no, Needed)
	;
		Queue = Queue0,
		Needed = Needed0
	).
dead_proc_elim__examine_expr(shorthand(_), _, _, _, _, _) :-
	% these should have been expanded out by now
	error("detect_cse_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%


		% information used during the elimination phase.

:- type elim_info
	--->	elimination_info(
			needed_map,	% collected usage counts 
			module_info,	% ye olde module_info
			pred_table,	% table of predicates in this module:
					% preds and procs in this table
					% may be eliminated
			bool		% has anything changed
		).
			
	% Given the information about which entities are needed,
	% eliminate procedures which are not needed.
:- pred dead_proc_elim__eliminate(dead_proc_pass, module_info, needed_map,
		module_info, io__state, io__state).
:- mode dead_proc_elim__eliminate(in, in, in, out, di, uo) is det.

dead_proc_elim__eliminate(Pass, ModuleInfo0, Needed0, ModuleInfo,
		State0, State) :-
	module_info_predids(ModuleInfo0, PredIds),
	module_info_preds(ModuleInfo0, PredTable0),

	Changed0 = no,
	ElimInfo0 = elimination_info(Needed0, ModuleInfo0,
			PredTable0, Changed0),
	list__foldl2(dead_proc_elim__eliminate_pred(Pass), PredIds, ElimInfo0, 
		ElimInfo, State0, State),
	ElimInfo = elimination_info(Needed, ModuleInfo1, PredTable, Changed),

	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo2),
	module_info_type_ctor_gen_infos(ModuleInfo2, TypeCtorGenInfos0),
	dead_proc_elim__eliminate_base_gen_infos(TypeCtorGenInfos0, Needed,
		TypeCtorGenInfos),
	module_info_set_type_ctor_gen_infos(ModuleInfo2, TypeCtorGenInfos,
		ModuleInfo3),
	(
		Changed = yes,
		% The dependency graph will still contain references to the
		% eliminated procedures, so it must be rebuilt if it will
		% be used later.
		module_info_clobber_dependency_info(ModuleInfo3, ModuleInfo)
	;
		Changed	= no,
		ModuleInfo = ModuleInfo3
	).

		% eliminate any unused procedures for this pred

:- pred dead_proc_elim__eliminate_pred(dead_proc_pass, pred_id,
	elim_info, elim_info, io__state, io__state).
:- mode dead_proc_elim__eliminate_pred(in, in, in, out, di, uo) is det.

dead_proc_elim__eliminate_pred(Pass, PredId, ElimInfo0, ElimInfo,
		State0, State) :-
	ElimInfo0 = elimination_info(Needed, ModuleInfo, PredTable0, Changed0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_import_status(PredInfo0, Status),
	(
		% Find out if the predicate is defined in this module.
		% If yes, find out also whether any of its procedures
		% must be kept.
		( Status = local,
			Keep = no,
			(
				% Don't warn for unify or comparison preds,
				% since they may be automatically generated
				is_unify_or_compare_pred(PredInfo0)
			->
				WarnForThisProc = no
			;
				% Don't warn for procedures introduced from
				% lambda expressions.  The only time those
				% procedures will be unused is if the procedure
				% containing the lambda expression is unused,
				% and in that case, we already warn for that
				% containing procedure if appropriate.
				% Likewise, don't warn for procedures
				% introduced for type specialization.
				pred_info_name(PredInfo0, PredName),
				( string__prefix(PredName, "IntroducedFrom__")
				; string__prefix(PredName, "TypeSpecOf__")
				)
			->
				WarnForThisProc = no
			;
				WarnForThisProc = yes
			)
		; Status = pseudo_imported,
			Keep = no,
			WarnForThisProc = no
		; Status = pseudo_exported,
			hlds_pred__in_in_unification_proc_id(InitProcId),
			Keep = yes(InitProcId),
			WarnForThisProc = no
		)
	->
		pred_info_procids(PredInfo0, ProcIds),
		pred_info_procedures(PredInfo0, ProcTable0),
		list__foldl2(dead_proc_elim__eliminate_proc(Pass, PredId,
			Keep, WarnForThisProc, ElimInfo0),
			ProcIds, Changed0 - ProcTable0, Changed - ProcTable,
			State0, State),
		pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
		map__det_update(PredTable0, PredId, PredInfo, PredTable)
	;
		% Don't generate code in the current module for
		% unoptimized opt_imported preds (that is, for
		% opt_imported preds which we have not by this point
		% managed to inline or specialize; this code should be
		% called with `Pass = final_optimization_pass'
		% only after inlining and specialization is complete).
		Pass = final_optimization_pass,
		Status = opt_imported
	->
		Changed = yes,
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
		pred_info_set_import_status(PredInfo1, imported(interface),
				PredInfo),
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
		PredTable = PredTable0,
		Changed = Changed0
	),
	ElimInfo = elimination_info(Needed, ModuleInfo, PredTable, Changed).


		% eliminate a procedure, if unused

:- pred dead_proc_elim__eliminate_proc(dead_proc_pass, pred_id, maybe(proc_id),
	bool, elim_info, proc_id,
	pair(bool, proc_table), pair(bool, proc_table),
	io__state, io__state).
:- mode dead_proc_elim__eliminate_proc(in, in, in, in, in, in, in, out, di, uo)
	is det.

dead_proc_elim__eliminate_proc(Pass, PredId, Keep, WarnForThisProc, ElimInfo,
		ProcId, Changed0 - ProcTable0, Changed - ProcTable) -->
	{ ElimInfo = elimination_info(Needed, ModuleInfo, _PredTable, _) },
	(
		% Keep the procedure if it is in the needed map
		% or if it is to be kept because it is exported.
		( { map__search(Needed, proc(PredId, ProcId), _) }
		; { Keep = yes(ProcId) }
		)
	->
		{ ProcTable = ProcTable0 },
		{ Changed = Changed0 }
	;
		{ Changed = yes },
		globals__io_lookup_bool_option(very_verbose, VeryVerbose),
		( { VeryVerbose = yes } ->
			write_proc_progress_message(
				"% Eliminated the dead procedure ",
				PredId, ProcId, ModuleInfo)
		;
			[]
		),
		(
			{ Pass = warning_pass },
			{ WarnForThisProc = yes }
			% we don't need to check the warn_dead_procs option
			% since that is already checked by mercury_compile.m
			% when deciding whether to invoke this warning_pass
		->
			{ proc_info_context(ProcTable0 ^ det_elem(ProcId),
				Context) },
			warn_dead_proc(PredId, ProcId, Context, ModuleInfo)
		;
			[]
		),
		{ map__delete(ProcTable0, ProcId, ProcTable) }
	).

:- pred warn_dead_proc(pred_id, proc_id, prog_context, module_info,
		io__state, io__state).
:- mode warn_dead_proc(in, in, in, in, di, uo) is det.

warn_dead_proc(PredId, ProcId, Context, ModuleInfo, !IO) :-
	error_util__describe_one_proc_name(ModuleInfo, proc(PredId, ProcId), 
		ProcName),
	Components = [words("Warning:"), fixed(ProcName),
			words("is never called.")],
	error_util__report_warning(Context, 0, Components, !IO).

:- pred dead_proc_elim__eliminate_base_gen_infos(list(type_ctor_gen_info),
	needed_map, list(type_ctor_gen_info)).
:- mode dead_proc_elim__eliminate_base_gen_infos(in, in, out) is det.

dead_proc_elim__eliminate_base_gen_infos([], _Needed, []).
dead_proc_elim__eliminate_base_gen_infos([TypeCtorGenInfo0 | TypeCtorGenInfos0],
		Needed, TypeCtorGenInfos) :-
	dead_proc_elim__eliminate_base_gen_infos(TypeCtorGenInfos0, Needed,	
		TypeCtorGenInfos1),
	TypeCtorGenInfo0 = type_ctor_gen_info(_TypeCtor, ModuleName,
		TypeName, Arity, _Status, _HldsDefn, _Unify, _Compare),
	(
		Entity = base_gen_info(ModuleName, TypeName, Arity),
		map__search(Needed, Entity, _)
	->
		TypeCtorGenInfos = [TypeCtorGenInfo0 | TypeCtorGenInfos1]
	;
		TypeCtorGenInfos = TypeCtorGenInfos1
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

	queue__init(Queue0),
	map__init(Needed0),
	module_info_get_pragma_exported_procs(ModuleInfo0, PragmaExports),
	dead_proc_elim__initialize_pragma_exports(PragmaExports,
		Queue0, _, Needed0, Needed1),
	%
	% The goals for the class method procs need to be
	% examined because they contain calls to the actual method
	% implementations.
	%
	module_info_instances(ModuleInfo0, Instances),
	module_info_classes(ModuleInfo0, Classes),
	dead_proc_elim__initialize_class_methods(Classes, Instances,
		Queue0, _, Needed1, Needed),
	map__keys(Needed, Entities),
	queue__init(Queue1),
	set__init(NeededPreds0),
	list__foldl2(dead_pred_elim_add_entity, Entities,
		Queue1, Queue, NeededPreds0, NeededPreds1),

	set__init(Preds0),
	set__init(Names0),
	DeadInfo0 = dead_pred_info(ModuleInfo0, Queue, 
		Preds0, NeededPreds1, Names0),

	module_info_predids(ModuleInfo0, PredIds),
	list__foldl(dead_pred_elim_initialize, PredIds, 
		DeadInfo0, DeadInfo1),
	dead_pred_elim_analyze(DeadInfo1, DeadInfo),
	DeadInfo = dead_pred_info(ModuleInfo1, _, _, NeededPreds2, _),

	%
	% If a predicate is not needed, predicates which were added in
	% make_hlds.m to force type specialization are also not needed.
	% Here we add in those which are needed.
	%
	module_info_type_spec_info(ModuleInfo1,
		type_spec_info(TypeSpecProcs0, TypeSpecForcePreds0,
			SpecMap0, PragmaMap0)),
	set__to_sorted_list(NeededPreds2, NeededPredList2),
	list__foldl(
	    lambda([NeededPred::in, AllPreds0::in, AllPreds::out] is det, (
		( map__search(SpecMap0, NeededPred, NewNeededPreds) ->
			set__insert_list(AllPreds0, NewNeededPreds, AllPreds)
		;
			AllPreds = AllPreds0
		)
	)), NeededPredList2, NeededPreds2, NeededPreds),
	set__intersect(TypeSpecForcePreds0, NeededPreds, TypeSpecForcePreds),

	module_info_set_type_spec_info(ModuleInfo1,
		type_spec_info(TypeSpecProcs0, TypeSpecForcePreds, 
			SpecMap0, PragmaMap0),
		ModuleInfo2),

	module_info_get_predicate_table(ModuleInfo2, PredTable0),
	module_info_get_partial_qualifier_info(ModuleInfo2, PartialQualInfo),
	predicate_table_restrict(PartialQualInfo, PredTable0,
			set__to_sorted_list(NeededPreds), PredTable),
	module_info_set_predicate_table(ModuleInfo2, PredTable, ModuleInfo).


:- pred dead_pred_elim_add_entity(entity::in, queue(pred_id)::in,
	queue(pred_id)::out, set(pred_id)::in, set(pred_id)::out) is det.

dead_pred_elim_add_entity(base_gen_info(_, _, _), Q, Q, Preds, Preds).
dead_pred_elim_add_entity(proc(PredId, _), Q0, Q, Preds0, Preds) :-
	queue__put(Q0, PredId, Q),
	set__insert(Preds0, PredId, Preds).

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
			is_unify_or_compare_pred(PredInfo)
		;
			% Don't eliminate preds from builtin modules,
			% since later passes of the compiler may introduce
			% calls to them (e.g. polymorphism.m needs unify/2
			% and friends).
			any_mercury_builtin_module(PredModule)
		;
			% Don't attempt to eliminate local preds here, since we
			% want to do semantic checking on those even if they 
			% aren't used.
			\+ pred_info_is_imported(PredInfo), 
			\+ pred_info_import_status(PredInfo, opt_imported)
		;
			% Don't eliminate predicates declared in this module
			% with a `:- external' or `:- pragma base_relation'
			% declaration.
			% magic.m will change the import_status to
			% `exported' when it generates the interface
			% procedure for a base relation.
			module_info_name(ModuleInfo, PredModule)
		;
			% Don't eliminate <foo>_init_any/1 predicates;
			% modes.m may insert calls to them to initialize
			% variables from inst `free' to inst `any'.
			string__remove_suffix(PredName, "_init_any", _),
			PredArity = 1
		;
			% Don't eliminate the clauses for promises.
			pred_info_get_goal_type(PredInfo, promise(_))
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
			clauses_info_clauses(ClausesInfo, Clauses),
			list__foldl(dead_pred_elim_process_clause, Clauses,
				DeadInfo1, DeadInfo2)
		),
		dead_pred_elim_analyze(DeadInfo2, DeadInfo)
	;
		DeadInfo = DeadInfo0
	).

:- pred dead_pred_elim_process_clause(clause::in, dead_pred_info::in, 
		dead_pred_info::out) is det.

dead_pred_elim_process_clause(clause(_, Goal, _, _)) -->
	pre_modecheck_examine_goal(Goal).

:- pred pre_modecheck_examine_goal(hlds_goal::in, 
		dead_pred_info::in, dead_pred_info::out) is det.

pre_modecheck_examine_goal(conj(Goals) - _) -->
	list__foldl(pre_modecheck_examine_goal, Goals).
pre_modecheck_examine_goal(par_conj(Goals) - _) -->
	list__foldl(pre_modecheck_examine_goal, Goals).
pre_modecheck_examine_goal(disj(Goals) - _) -->
	list__foldl(pre_modecheck_examine_goal, Goals).
pre_modecheck_examine_goal(if_then_else(_, If, Then, Else) - _) -->
	list__foldl(pre_modecheck_examine_goal, [If, Then, Else]).
pre_modecheck_examine_goal(switch(_, _, Cases) - _) -->
	{ ExamineCase = lambda([Case::in, Info0::in, Info::out] is det, (
		Case = case(_, Goal),
		pre_modecheck_examine_goal(Goal, Info0, Info)
	)) },
	list__foldl(ExamineCase, Cases).
pre_modecheck_examine_goal(generic_call(_,_,_,_) - _) --> [].
pre_modecheck_examine_goal(not(Goal) - _) -->
	pre_modecheck_examine_goal(Goal).
pre_modecheck_examine_goal(some(_, _, Goal) - _) -->
	pre_modecheck_examine_goal(Goal).
pre_modecheck_examine_goal(call(_, _, _, _, _, PredName) - _) -->
	dead_pred_info_add_pred_name(PredName).
pre_modecheck_examine_goal(foreign_proc(_, _, _, _, _, _, _) - _) -->
	[].
pre_modecheck_examine_goal(unify(_, Rhs, _, _, _) - _) -->
	pre_modecheck_examine_unify_rhs(Rhs).
pre_modecheck_examine_goal(shorthand(_) - _) -->
	% these should have been expanded out by now
	{ error("pre_modecheck_examine_goal: unexpected shorthand") }.

:- pred pre_modecheck_examine_unify_rhs(unify_rhs::in, 
		dead_pred_info::in, dead_pred_info::out) is det.

pre_modecheck_examine_unify_rhs(var(_)) --> [].
pre_modecheck_examine_unify_rhs(functor(Functor, _, _)) -->
	( { Functor = cons(Name, _) } ->
		dead_pred_info_add_pred_name(Name)
	;
		[]
	).
pre_modecheck_examine_unify_rhs(lambda_goal(_, _, _, _, _, _, _, _, Goal)) -->
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
		(
			predicate_table_search_sym(PredicateTable,
				may_be_partially_qualified, Name, PredIds)
		->
			queue__put_list(Q0, PredIds, Q)
		;
			Q = Q0
		),
		DeadInfo = dead_pred_info(ModuleInfo, Q, Ex,
			Needed, NeededNames)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
