%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: magic.m
% Main author: stayl
%
% This module implements the supplementary magic set transformation,
% sort of as described in 
% 	C. Beeri and R. Ramakrishnan,
% 	On the power of magic,
% 	Journal of Logic Programming,
%	volume 10, 1991, pp. 255-299.
% The main difference is that the input relation is explicitly passed
% as a closure.
%
% The magic set transformation is used to transform queries into a form
% suitable for the Aditi deductive database code generator in rl_gen.m.
% The magic set transformation or the context transformation (defined
% in context.m) must be applied to all Aditi predicates. The magic sets
% and context transformations are mutually exclusive.
%
% It is important that no optimization which could optimize away calls
% to Aditi procedures (e.g. simplify.m) be run between magic.m and rl_gen.m.
% If Aditi calls are removed, the code in dependency_graph.m which merges
% the SCCs containing Aditi predicates could become confused about which
% predicates can be compiled together.
%
%-----------------------------------------------------------------------------%
% Short example:
%
% :- module a.
%
% :- interface.
%
% :- import_module aditi.
%
% :- pred call_anc(aditi__state::in, int::out) is nondet.
% :- pragma aditi(call_anc/3).
%
% :- implementation.
%
% :- pred anc(aditi__state::in, int::in, int::out) is nondet.
% :- pragma aditi(anc/3).
%
% :- pred p(aditi__state::in, int::out, int::out) is nondet.
% :- pragma base_relation(p/3).
%
% anc(DB, X, Y) :- 
%	p(DB, X, Y).
% anc(DB, X, Y) :-
%	p(DB, X, Z),
%	anc(DB, Z, Y).
%
%-----------------------------------------------------------------------------%
% Transformed version:
%
% % The original predicate is converted into a goal which
% % calls do_call_aditi_nondet to do all the work.
% % The type_infos are used for data conversion.
% % The base relation p/3 is given an interface procedure
% % which looks basically the same as this one, except that
% % there are no inputs and two outputs.
% % This procedure is compiled to C, not Aditi-RL.
% anc(HeadVar__1, HeadVar__2, HeadVar__3) :-
%       V_15 = "stayl/a/a__anc__c_interface_2_0/2",	% RL proc name
%       V_16 = 1,					% number of inputs
%       V_17 = "(:I)",					% input relation schema
%       V_18 = 1,					% number of outputs
%       TypeInfo_13 = type_ctor_info("", "int", 0),	% input type_info
%       TypeInfo_14 = type_ctor_info("", "int", 0),	% output type_info
%
%	% The aditi__state is not needed (it contains no information),
%	% so it is not passed.
% 	% aditi_call(PredName, Number of inputs, Input relation schema,
%	% 	Number of Outputs)
%	generic_call(
%		aditi_call("stayl/a/a__anc__c_interface_2_0/2", 1, "(:I)", 1),
%		TypeInfo_13, HeadVar__2,
% 		TypeInfo_14, HeadVar__3).
%
% :- pred anc__c_interface(pred(int)::(pred(out) is nondet),
% 		 int::out) is nondet.
% :- pragma aditi(anc__c_interface/2).
%
% % This predicate calls the Aditi version of anc, joins the result
% % with the input to the calls and then projects the join result onto
% % the output arguments.
% anc__c_interface(InAnc, Y) :-
%	anc__c_interface__supp1(InAnc, X),
% 	V_15 = anc__c_interface__supp1(InAnc),
% 	anc__aditi0(InAnc, V_1, Y),
% 	X == V_1.
%
% 	% The aditi__state arguments are removed and all modes are
%	% converted to output. An input closure is added for each
%	% predicate in the SCC.
% :- pred anc__aditi0(pred(int)::(pred(out) is nondet),
% 		int::out, int::out) is nondet.
% :- pragma aditi(anc__aditi0/3).
%
% anc__aditi0(InAnc, X, Y) :- 
%	anc__magic(InAnc, X),
%	p(V_1, Y),
%	V1 == X.
% anc__aditi0(InAnc, X, Y) :-
%	anc__supp1(InAnc, X),
%	anc__aditi0(InAnc, V1, Y),
%	V1 == X.
%
%	% `anc__magic' collects all tuples which could be input to
%	% a call to `anc' in a top-down execution.
% :- pred anc__magic(pred(int)::(pred(out) is nondet), int::out) is nondet.
% :- pragma aditi(anc__magic/2).
%
% anc__magic(InAnc, X) :-
%	% Collect the input from a higher sub-module.
%	call(InAnc, X).
% anc__magic(InAnc, Z) :-
%	% Collect the input from recursive calls.
% 	anc__supp1(InAnc, _, Z).
%
%	% `anc__supp1' is introduced to do common sub-expression - 
%	% this join would otherwise be done in both `anc__aditi0'
%	% and `anc__magic'. This is also necessary because rl_gen.m
%	% only handles rules with at most two database calls.
% :- pred anc__supp1(pred(int)::(pred(out) is nondet, int::out) is nondet.
% :- pragma aditi(anc__supp1/2).
%
% anc__supp1(InAnc, Z) :-
% 	anc__magic(InAnc, X),
%	p(V_1, Z),
% 	X == V1.
%
%-----------------------------------------------------------------------------%
%
% context.m is called to handle predicates with a `:- pragma context'
% declaration.
%
% Input relations are explicitly passed using closures.
%
% While it processes the module it checks that there are no higher-order,
% partially instantiated, polymorphic or abstract arguments since Aditi
% cannot handle these.
% 
% Any closures occurring in Aditi procedures must not have curried arguments.
% Closures may only be used for aggregates.
%
% XXX This should attempt to reorder within rules so that no supplementary
% predicates are created with partially instantiated arguments, since Aditi
% can only handle ground terms in relations. The problem occurs if there are
% partially instantiated terms live across a database predicate call. At the
% moment an error is reported.
%
% Note that the transformation introduces new mangled predicate names,
% but these should not show up in the generated C code so util/mdemangle does
% not need to handle them.
% 
%-----------------------------------------------------------------------------%
:- module aditi_backend__magic.

:- interface.

:- import_module hlds__hlds_module.
:- import_module io.

:- pred magic__process_module(module_info, module_info, io__state, io__state).
:- mode magic__process_module(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module aditi_backend__magic_util, aditi_backend__context.
:- import_module transform_hlds__dependency_graph, hlds__hlds_pred.
:- import_module hlds__hlds_goal, hlds__hlds_data, parse_tree__prog_data.
:- import_module hlds__passes_aux, check_hlds__mode_util, (parse_tree__inst).
:- import_module hlds__instmap, aditi_backend__rl_gen, aditi_backend__rl.
:- import_module libs__globals, libs__options, hlds__hlds_out.
:- import_module parse_tree__prog_out, hlds__goal_util, check_hlds__type_util.
:- import_module check_hlds__polymorphism, hlds__quantification.
:- import_module ll_backend__saved_vars, transform_hlds__dead_proc_elim.

:- import_module int, list, map, require, set, std_util, string, term, varset.
:- import_module assoc_list, bool, check_hlds__simplify.

magic__process_module(ModuleInfo0, ModuleInfo) -->

	%
	% Run simplification on Aditi procedures, mainly to get rid of 
	% nested explicit quantifications.
	% 
	globals__io_get_globals(Globals),
	{ simplify__find_simplifications(no, Globals, Simplifications) },
	process_matching_nonimported_procs(
		update_module_io(
			magic__ite_to_disj_and_simplify(Simplifications)),
		_, hlds_pred__pred_info_is_aditi_relation,
		ModuleInfo0, ModuleInfo1),

	% We need to run dead_proc_elim before working out the
	% Aditi dependency ordering because any calls from dead
	% procedures could confuse the code to merge SCCs (because
	% procedures called from multiple places are never merged).
	%
	% No optimizations which could optimize away calls to Aditi
	% procedures (e.g. simplify.m) should be run after this is done.
	dead_proc_elim(ModuleInfo1, ModuleInfo2),

	{ module_info_ensure_aditi_dependency_info(ModuleInfo2, ModuleInfo3) },
	{ module_info_aditi_dependency_ordering(ModuleInfo3, Ordering) },
	{ magic_info_init(ModuleInfo3, Info0) },
	{ module_info_predids(ModuleInfo3, PredIds) },

	% 
	% Only preprocess imported Aditi predicates which are used,
	% to avoid performing error checking (e.g. checking for abstract
	% types) on predicates which are not used. The check for abstract
	% types needs to be done in importing modules because an imported
	% predicate's declaration may use types which are indirectly imported
	% from another module. Discriminated union types are written as
	% abstract types to `.int2' files.
	%
	{ set__init(UsedImportedPreds0) },
	{ list__foldl(magic__find_used_imported_aditi_preds(ModuleInfo3),
		Ordering, UsedImportedPreds0, UsedImportedPreds) },
	{ magic__process_imported_procs(PredIds, UsedImportedPreds,
		Info0, Info1) },
	globals__io_lookup_bool_option(very_verbose, Verbose),

		% Add magic procedures, do some transformation on the goals.
	maybe_write_string(Verbose, "% preprocessing module\n"),
	maybe_flush_output(Verbose),
	{ list__foldl(magic__check_scc, Ordering, Info1, Info2) },
	{ list__foldl(magic__preprocess_scc, Ordering, Info2, Info3) },

		% Do the transformation.
	maybe_write_string(Verbose, "% processing module\n"),
	maybe_flush_output(Verbose),
	list__foldl2(magic__process_scc, Ordering, Info3, Info4),
	{ list__foldl(magic__update_pred_status, PredIds, Info4, Info5) },

	{ magic_info_get_module_info(ModuleInfo4, Info5, Info) },
	{ magic_info_get_errors(Errors, Info, _) },
	{ set__to_sorted_list(Errors, ErrorList) },
	( { ErrorList = [] } ->
		{ ModuleInfo5 = ModuleInfo4 }
	;
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		magic_util__report_errors(ErrorList,
			ModuleInfo4, VerboseErrors),
		{ module_info_incr_errors(ModuleInfo4, ModuleInfo5) },
		io__set_exit_status(1)
	),

		% New procedures were created, so the dependency_info
		% is out of date.
	{ module_info_clobber_dependency_info(ModuleInfo5, ModuleInfo) }.

%-----------------------------------------------------------------------------%

	%
	% Convert if-then-elses and switches to disjunctions,
	% then run simplification to flatten goals and remove
	% unnecessary existential quantifications.
	%
:- pred magic__ite_to_disj_and_simplify(list(simplification)::in, pred_id::in,
	proc_id::in, proc_info::in, proc_info::out, module_info::in,
	module_info::out, io__state::di, io__state::uo) is det.

magic__ite_to_disj_and_simplify(Simplifications, PredId, ProcId,
		ProcInfo0, ProcInfo, ModuleInfo0, ModuleInfo) -->
	{ proc_info_goal(ProcInfo0, Goal0) },

	{ Goal0 = if_then_else(_Vars, Cond, Then, Else) - GoalInfo ->
		goal_util__if_then_else_to_disjunction(Cond, Then, Else, 
			GoalInfo, Disj),
		Goal1 = Disj - GoalInfo,
		proc_info_set_goal(ProcInfo0, Goal1, ProcInfo1),

		% Requantify the goal to rename apart the variables
		% in the copies of the condition.
		requantify_proc(ProcInfo1, ProcInfo3),
		ModuleInfo1 = ModuleInfo0
	; Goal0 = switch(Var, _Canfail, Cases) - GoalInfo ->
		proc_info_varset(ProcInfo0, VarSet0),
		proc_info_vartypes(ProcInfo0, VarTypes0),
		proc_info_get_initial_instmap(ProcInfo0, 
			ModuleInfo0, InstMap),
		% XXX check for existentially typed constructors first -
		% they will cause an abort.
		goal_util__switch_to_disjunction(Var, Cases,
			InstMap, Disjuncts, VarSet0, VarSet1, 
			VarTypes0, VarTypes1, ModuleInfo0, ModuleInfo1),
		proc_info_set_varset(ProcInfo0, VarSet1, ProcInfo1),
		proc_info_set_vartypes(ProcInfo1, VarTypes1, ProcInfo2),
		Goal1 = disj(Disjuncts) - GoalInfo,
		proc_info_set_goal(ProcInfo2, Goal1, ProcInfo3)
	;
		ProcInfo3 = ProcInfo0,
		ModuleInfo1 = ModuleInfo0
	},

	simplify__proc(Simplifications, PredId, ProcId,
		ModuleInfo1, ModuleInfo2, ProcInfo3, ProcInfo4),

	%
	% Run saved_vars so that constructions of constants are close
	% to their uses, and constant attributes aren't unnecessarily
	% added to relations. We should be more aggressive about this -
	% constructions of constant compound terms should also be pushed.
	%
	saved_vars_proc(PredId, ProcId, ProcInfo4, ProcInfo,
		ModuleInfo2, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred magic__check_scc(aditi_scc::in,
		magic_info::in, magic_info::out) is det.

magic__check_scc(aditi_scc(SCC0, _)) -->
	list__foldl(magic__check_scc_2, SCC0).

:- pred magic__check_scc_2(list(pred_proc_id)::in,
		magic_info::in, magic_info::out) is det.

magic__check_scc_2(SCC) -->
	magic_info_get_errors(Errors0),
	magic_info_get_module_info(ModuleInfo),
	{ SCC = [_] ->
		Errors1 = Errors0
	;
		% Add errors for context procedures which are mutually
		% recursive with other procedures.
		solutions(
			lambda([ProcAndContext::out] is nondet, (
				list__member(ContextProc, SCC),
				ContextProc = proc(ContextPredId, _),
				module_info_pred_info(ModuleInfo,
					ContextPredId, ContextPredInfo),
				pred_info_get_markers(ContextPredInfo,
					ContextMarkers),
				check_marker(ContextMarkers, context),
				pred_info_context(ContextPredInfo,
					Context),
				ProcAndContext = ContextProc - Context
			)), ContextProcs),
		list__map(
			lambda([BadContextProc::in, Error::out] is det, (
				BadContextProc = TheContextProc - TheContext,
				Error = mutually_recursive_context(
					TheContextProc, SCC) - TheContext
			)), ContextProcs, ContextErrors),
		set__insert_list(Errors0, ContextErrors, Errors1)
	},
	{
		% Add errors if a procedure compiled to C is mutually
		% recursive with an Aditi procedure.
		list__member(proc(PredId, _), SCC),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_get_markers(PredInfo, Markers),
		\+ check_marker(Markers, aditi)
	->
		term__context_init(InitContext),
		set__insert(Errors1, mixed_scc(SCC) - InitContext, Errors)
	;		
		Errors = Errors1
	},
	magic_info_set_errors(Errors).

%-----------------------------------------------------------------------------%
	
	% All the procedures which previously had Aditi markers should
	% have been renamed apart, leaving the old versions to be called
	% from C. These old versions must have their aditi/base relation
	% markers removed.
:- pred magic__update_pred_status(pred_id::in,
		magic_info::in, magic_info::out) is det.

magic__update_pred_status(PredId) -->
	magic_info_get_module_info(ModuleInfo0),
	{ module_info_pred_info(ModuleInfo0, PredId, PredInfo0) },
	{ pred_info_get_markers(PredInfo0, Markers0) },
	( { check_marker(Markers0, aditi) } ->
		{ remove_marker(Markers0, aditi, Markers1) },
		{ remove_marker(Markers1, base_relation, Markers) },
		{ pred_info_set_markers(PredInfo0, Markers, PredInfo) },
		{ module_info_set_pred_info(ModuleInfo0,
			PredId, PredInfo, ModuleInfo) },
		magic_info_set_module_info(ModuleInfo)
	;
		[]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Find all imported procedures which are called within
	% a local Aditi procedure. The magic sets version of their
	% interface must be produced.
:- pred magic__find_used_imported_aditi_preds(module_info::in,
	aditi_scc::in, set(pred_id)::in, set(pred_id)::out) is det.

magic__find_used_imported_aditi_preds(ModuleInfo, SCC, Preds0, Preds) :-
	SCC = aditi_scc(SCCPredProcIds0, _EntryPoints),
	list__condense(SCCPredProcIds0, SCCPredProcIds),
	list__foldl(magic__find_used_imported_aditi_preds_2(ModuleInfo),
		SCCPredProcIds, Preds0, Preds).

:- pred magic__find_used_imported_aditi_preds_2(module_info::in,
	pred_proc_id::in, set(pred_id)::in, set(pred_id)::out) is det.

magic__find_used_imported_aditi_preds_2(ModuleInfo,
		PredProcId, Preds0, Preds) :-
	module_info_pred_proc_info(ModuleInfo, PredProcId, _, ProcInfo),
	proc_info_goal(ProcInfo, Goal),

	% Generate all pred_ids called by a goal.
	Generator = (pred(P::out) is nondet :- goal_calls_pred_id(Goal, P)),

	% Add all used imported Aditi predicates to the accumulator.
	Accumulator = 
	    (pred(CalledPredId::in, UsedPreds0::in, UsedPreds::out) is det :-	
		module_info_pred_info(ModuleInfo,
			CalledPredId, CalledPredInfo),
		(
			pred_info_is_imported(CalledPredInfo),
			pred_info_is_aditi_relation(CalledPredInfo)
		->
			set__insert(UsedPreds0, CalledPredId, UsedPreds)
		;
			UsedPreds = UsedPreds0
		)
	    ),

	Preds = promise_only_solution(
		(pred(Preds1::out) is cc_multi :-
			unsorted_aggregate(Generator, Accumulator,
				Preds0, Preds1)
		)).

	% Convert imported Aditi procedures for the magic sets interface.
:- pred magic__process_imported_procs(list(pred_id)::in, set(pred_id)::in,
		magic_info::in, magic_info::out) is det.

magic__process_imported_procs([], _) --> [].
magic__process_imported_procs([PredId | PredIds], UsedPreds) -->
	magic_info_get_module_info(ModuleInfo),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	(
		{ pred_info_is_imported(PredInfo) },
		{ hlds_pred__is_derived_relation(ModuleInfo, PredId) },
		{ set__member(PredId, UsedPreds) }
	->
		{ pred_info_procids(PredInfo, ProcIds) },
		magic__process_imported_procs_2(PredId, ProcIds)
	;
		{ hlds_pred__pred_info_is_base_relation(PredInfo) },
		{
			% Always preprocess base relations defined in
			% this module.
			module_info_name(ModuleInfo, ModuleName),
			pred_info_module(PredInfo, PredModuleName),
			ModuleName = PredModuleName
		;
			set__member(PredId, UsedPreds)	
		}
	->
		{ pred_info_procids(PredInfo, ProcIds) },
		list__foldl(magic__process_base_relation(PredId), ProcIds)
	;
		[]
	),
	magic__process_imported_procs(PredIds, UsedPreds).

:- pred magic__process_imported_procs_2(pred_id::in, list(proc_id)::in,
		magic_info::in, magic_info::out) is det.

magic__process_imported_procs_2(_, []) --> [].
magic__process_imported_procs_2(PredId, [ProcId | ProcIds]) -->
	{ PredProcId = proc(PredId, ProcId) },
	magic__get_scc_inputs([PredProcId], InputTypes, InputModes),
	magic__adjust_pred_info([PredProcId], InputTypes, 
		InputModes, PredProcId),
	magic__process_imported_procs_2(PredId, ProcIds).

%-----------------------------------------------------------------------------%

	% Create a version without the aditi__states, and with
	% all modes output.
:- pred magic__process_base_relation(pred_id::in, proc_id::in,
		magic_info::in, magic_info::out) is det.

magic__process_base_relation(PredId0, ProcId0) -->
	magic__separate_proc(PredId0, ProcId0),
	magic_info_get_pred_map(PredMap),
	{ CPredProcId = proc(PredId0, ProcId0) },
	{ map__lookup(PredMap, CPredProcId, PredProcId) },

	magic_info_get_module_info(ModuleInfo0),
	{ module_info_pred_proc_info(ModuleInfo0, PredProcId,
		PredInfo0, ProcInfo0) },
	{ pred_info_arg_types(PredInfo0, TVarSet, ExistQVars, ArgTypes0) },
	{ proc_info_argmodes(ProcInfo0, ArgModes0) },
	{ proc_info_headvars(ProcInfo0, HeadVars0) },

	magic_info_set_error_pred_proc_id(CPredProcId),
	{ set__init(ErrorVars) },
	magic_info_set_error_vars(ErrorVars),

	(
		{ pred_info_module(PredInfo0, ModuleName) },
		{ module_info_name(ModuleInfo0, ModuleName) }
	->
		{ pred_info_context(PredInfo0, Context) },
		magic_util__check_args(HeadVars0, ArgModes0, ArgTypes0,
			Context, arg_number)
	;
		[]
	),

	% Remove aditi:states, convert arguments to output.
	{ type_util__remove_aditi_state(ArgTypes0, ArgTypes0, ArgTypes) },
	{ type_util__remove_aditi_state(ArgTypes0, ArgModes0, ArgModes1) },
	{ list__map(magic_util__mode_to_output_mode(ModuleInfo0),
		ArgModes1, ArgModes) },
	{ type_util__remove_aditi_state(ArgTypes0, HeadVars0, HeadVars) },
	{ pred_info_get_indexes(PredInfo0, Indexes0) },
	{ list__map(magic_util__adjust_index(ArgTypes0), Indexes0, Indexes) },
	{ pred_info_set_indexes(PredInfo0, Indexes, PredInfo1) },
	{ pred_info_set_arg_types(PredInfo1, TVarSet, ExistQVars,
		ArgTypes, PredInfo) },
	{ proc_info_set_argmodes(ProcInfo0, ArgModes, ProcInfo1) },
	{ proc_info_set_headvars(ProcInfo1, HeadVars, ProcInfo) },
	{ module_info_set_pred_proc_info(ModuleInfo0,
		PredProcId, PredInfo, ProcInfo, ModuleInfo) },
	magic_info_set_module_info(ModuleInfo),
	magic__interface_from_c([CPredProcId], CPredProcId, PredProcId).

%-----------------------------------------------------------------------------%

	% Go over each sub-module adding in the input arguments for each 
	% procedure, allocating the magic predicates, filling in the 
	% magic_map, pred_map and magic_proc_info fields of the magic_info.
:- pred magic__preprocess_scc(aditi_scc::in,
		magic_info::in, magic_info::out) is det.

magic__preprocess_scc(aditi_scc(SCC0, EntryPoints)) --> 
	{ list__condense(SCC0, SCC) },
	magic__get_scc_inputs(EntryPoints, InputTypes, InputModes),
	list__foldl(magic__adjust_pred_info(EntryPoints,
		InputTypes, InputModes), SCC).

%-----------------------------------------------------------------------------%

	% Work out the types and modes of the input relations that need
	% to be passed around the sub-module.
:- pred magic__get_scc_inputs(list(pred_proc_id)::in, list(type)::out,
		list(mode)::out, magic_info::in, magic_info::out) is det.

magic__get_scc_inputs([], [], []) --> [].
magic__get_scc_inputs([PredProcId | PredProcIds],
		[Type | Types], [Mode | Modes]) -->
	magic_info_get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredProcId,
		PredInfo, ProcInfo) },
	{ proc_info_argmodes(ProcInfo, ArgModes0) },
	{ pred_info_arg_types(PredInfo, ArgTypes0) },
	{ type_util__remove_aditi_state(ArgTypes0, ArgTypes0, ArgTypes) },
	{ type_util__remove_aditi_state(ArgTypes0, ArgModes0, ArgModes) },
	{ partition_args(ModuleInfo, ArgModes, ArgModes, InputModes, _) },
	{ partition_args(ModuleInfo, ArgModes, ArgTypes, InputTypes, _) },
	{ construct_higher_order_type(predicate, (aditi_bottom_up),
		InputTypes, Type) },
	{ GetOutputMode = lambda([ArgMode::in, OutputMode::out] is det, (
			mode_get_insts(ModuleInfo, ArgMode, _, OutputInst),
			OutputMode = (free -> OutputInst)
		)) },
	{ list__map(GetOutputMode, InputModes, InputRelModes) },
 	{ Inst = ground(unique, higher_order(pred_inst_info(predicate, 
				InputRelModes, nondet))) },
	{ Mode = (Inst -> Inst) },
	magic__get_scc_inputs(PredProcIds, Types, Modes).

%-----------------------------------------------------------------------------%

:- pred magic__adjust_pred_info(list(pred_proc_id)::in, list(type)::in,
		list(mode)::in, pred_proc_id::in,
		magic_info::in, magic_info::out) is det.

magic__adjust_pred_info(EntryPoints, MagicTypes, 
		MagicModes, PredProcId0) -->
	{ PredProcId0 = proc(PredId0, ProcId0) },
	magic__separate_proc(PredId0, ProcId0),
	magic_info_get_pred_map(PredMap1),
	{ map__lookup(PredMap1, PredProcId0, PredProcId) },
	magic__adjust_proc_info(EntryPoints, PredProcId0, PredProcId, 
		MagicTypes, MagicModes).

%-----------------------------------------------------------------------------%

	% Separate out the procedures for each predicate so that each
	% pred_info for a derived database predicate contains only one
	% proc_info. This is necessary because the different procedures
	% have different numbers of input arguments and are members of
	% different sub-modules, so the transformed procedures will have 
	% different numbers and types of input relation arguments. We also need
	% to leave the original declarations so that predicates compiled
	% to C can call the procedure.
:- pred magic__separate_proc(pred_id::in, proc_id::in,
		magic_info::in, magic_info::out) is det.

magic__separate_proc(PredId, ProcId) -->
	magic_info_get_module_info(ModuleInfo0),
	{ module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
		PredInfo0, ProcInfo0) },
	magic_info_set_curr_pred_proc_id(proc(PredId, ProcId)),
	magic_info_set_error_pred_proc_id(proc(PredId, ProcId)),
	{ set__init(ErrorVars) },
	magic_info_set_error_vars(ErrorVars),

	% 
	% Create a new pred_info for the procedure.
	%

	% Produce a unique name for the procedure.
	{ pred_info_module(PredInfo0, Module) },
	{ pred_info_name(PredInfo0, Name) },
	{ pred_info_get_markers(PredInfo0, Markers) },

	( { check_marker(Markers, base_relation) } ->
		% Base relations must keep the old name.
		{ NewName = qualified(Module, Name) }
	;
		magic_util__make_pred_name(PredInfo0, ProcId,
			"Aditi_Proc_For", no, NewName)
	),

	{ pred_info_arg_types(PredInfo0, TVarSet, ExistQVars, ArgTypes) },
	{ pred_info_context(PredInfo0, Context) },
	{ pred_info_import_status(PredInfo0, Status) },
	{ pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc) },
	{ pred_info_get_aditi_owner(PredInfo0, Owner) },
	{ pred_info_get_indexes(PredInfo0, Indexes) },
		% type classes aren't supported in Aditi.
	{ ClassConstraints = constraints([], []) },
	{ set__init(Assertions) },
	{ pred_info_create(Module, NewName, TVarSet,
		ExistQVars, ArgTypes, true, Context, Status, Markers, 
		PredOrFunc, ClassConstraints, Owner, Assertions, ProcInfo0, 
		NewProcId, NewPredInfo0) },
	{ pred_info_set_indexes(NewPredInfo0, Indexes, NewPredInfo) },

	magic_info_get_module_info(ModuleInfo1),
	{ module_info_get_predicate_table(ModuleInfo1, PredTable0) },
	{ predicate_table_insert(PredTable0, NewPredInfo, NewPredId, 
		PredTable) },
	{ module_info_set_predicate_table(ModuleInfo0, PredTable,
		ModuleInfo) },
	magic_info_set_module_info(ModuleInfo),

	%
	% Later we need to convert all calls to the old 
	% procedure to calls to the new.
	%
	magic_info_get_pred_map(PredMap0),
	{ map__det_insert(PredMap0, proc(PredId, ProcId),
		proc(NewPredId, NewProcId), PredMap) },
	magic_info_set_pred_map(PredMap).

%-----------------------------------------------------------------------------%

	% Preprocess the procedure
	% - preprocess the goal
	% - convert input arguments to output
	% - add input closure arguments
:- pred magic__adjust_proc_info(list(pred_proc_id)::in, pred_proc_id::in,
		pred_proc_id::in, list(type)::in, list(mode)::in,
		magic_info::in, magic_info::out) is det.

magic__adjust_proc_info(EntryPoints, CPredProcId, AditiPredProcId, 
		MagicTypes, MagicModes) -->
	magic_info_get_module_info(ModuleInfo0),
	{ module_info_pred_proc_info(ModuleInfo0, AditiPredProcId,
		PredInfo0, ProcInfo0) },
	magic_info_set_error_pred_proc_id(CPredProcId),
	{ set__init(ErrorVars) },
	magic_info_set_error_vars(ErrorVars),

	magic__preprocess_proc(CPredProcId, PredInfo0,
		ProcInfo0, ProcInfo1),

	%
	% Find which of the arguments of the SCC carries the
	% input for the current procedure.
	%
	{ list__nth_member_search(EntryPoints, CPredProcId, N) ->
		Index = yes(N),
		( EntryPoints \= [_], pred_info_is_exported(PredInfo0) ->
			InterfaceRequired = yes(N)
		;
			InterfaceRequired = no
		)
	;
		Index = no,
		InterfaceRequired = no
	},

	{ proc_info_inst_varset(ProcInfo1, InstVarSet) },
	magic__adjust_args(CPredProcId, AditiPredProcId, InterfaceRequired,
		Index, MagicTypes, MagicModes, PredInfo0, ProcInfo1,
		InputArgTypes, InputArgModes, LocalAditiPredProcId),

	( { pred_info_is_imported(PredInfo0) } ->
		[]
	;
			% Create a new procedure to collect the input
			% for the current procedure.
		magic__create_magic_pred(CPredProcId, LocalAditiPredProcId,
			MagicTypes, MagicModes, InputArgTypes, InputArgModes, 
			InstVarSet, Index)
	),

	%
	% Replace the goal for the C procedure with a goal to
	% interface with the Aditi procedure from C, unless --aditi-only
	% was specified or the procedure is imported.
	% 	
	magic__interface_from_c(EntryPoints, CPredProcId,
		LocalAditiPredProcId).

%-----------------------------------------------------------------------------%

	% Given a pred_info and a proc_info remove `aditi__state's from the
	% arguments and set up the input closure arguments.
	% Create an interface procedure if the SCC has multiple entry points
	% and is exported.
:- pred magic__adjust_args(pred_proc_id::in, pred_proc_id::in, maybe(int)::in,
		maybe(int)::in, list(type)::in, list(mode)::in,
		pred_info::in, proc_info::in, list(type)::out, list(mode)::out,
		pred_proc_id::out, magic_info::in, magic_info::out) is det.

magic__adjust_args(CPredProcId, AditiPredProcId, InterfaceRequired,
		MaybeIndex, MagicTypes, MagicModes, PredInfo0, ProcInfo0, 
		InputArgTypes, InputArgModes, LocalAditiPredProcId) -->

	%
	% Check that the argument types and modes 
	% are legal for Aditi procedures.
	%
	{ pred_info_arg_types(PredInfo0, TVarSet, ExistQVars, ArgTypes0) },
	{ proc_info_headvars(ProcInfo0, HeadVars0) },
	{ pred_info_context(PredInfo0, Context) },
	{ proc_info_argmodes(ProcInfo0, ArgModes0) },
	magic_util__check_args(HeadVars0, ArgModes0, ArgTypes0, Context,
		arg_number),

	%
	% Strip out the aditi__state argument.
	%
	{ type_util__remove_aditi_state(ArgTypes0, ArgTypes0, ArgTypes1) },
	{ type_util__remove_aditi_state(ArgTypes0, HeadVars0, HeadVars1) },
	{ type_util__remove_aditi_state(ArgTypes0, ArgModes0, ArgModes1) },

	%
	% Convert all of the original modes to output. The input
	% will be carried in with the input closures.
	%
	magic_info_get_module_info(ModuleInfo0),
	{ list__map(magic_util__mode_to_output_mode(ModuleInfo0),
		ArgModes1, ArgModes2) },

	% Create variables for the magic input.
	{ proc_info_create_vars_from_types(ProcInfo0, MagicTypes,
		MagicVars, ProcInfo1) },

	%
	% Add the input relation variables to the arguments.
	%
	{ list__append(MagicVars, HeadVars1, HeadVars) },
	{ list__append(MagicModes, ArgModes2, ArgModes) },
	{ list__append(MagicTypes, ArgTypes1, ArgTypes) },

	%
	% Ensure that the exported interface procedure gets the 
	% correct argmodes.
	%
	{ instmap_delta_from_mode_list(HeadVars, ArgModes, 
		ModuleInfo0, InstMapDelta) },
	{ proc_info_goal(ProcInfo1, Goal0 - GoalInfo0) },
	{ goal_info_set_instmap_delta(GoalInfo0, InstMapDelta, GoalInfo) },
	{ proc_info_set_goal(ProcInfo1, Goal0 - GoalInfo, ProcInfo2) },

	% All Aditi procedures are considered nondet. The C interface
	% procedures retain the old determinism, and abort if the number
	% of answers returned doesn't match the determinism.
	{ proc_info_set_inferred_determinism(ProcInfo2, nondet, ProcInfo3) },

	{ partition_args(ModuleInfo0, ArgModes1,
		ArgModes1, InputArgModes, _) },
	{ partition_args(ModuleInfo0, ArgModes1,
		ArgTypes1, InputArgTypes, _) },

	{ pred_info_set_arg_types(PredInfo0, TVarSet, ExistQVars,
		ArgTypes, PredInfo) },
	
	{ proc_info_set_headvars(ProcInfo3, HeadVars, ProcInfo4) },
	{ proc_info_set_argmodes(ProcInfo4, ArgModes, ProcInfo) },

	( { InterfaceRequired = yes(Index) } ->
		magic__create_interface_proc(Index, CPredProcId,
			AditiPredProcId, PredInfo0, ProcInfo3, ProcInfo,
			HeadVars1, ArgTypes1, ArgModes2, MagicVars,
			MagicTypes, MagicModes, LocalAditiPredProcId)
	;
		magic_info_get_module_info(ModuleInfo5),
		{ module_info_set_pred_proc_info(ModuleInfo5, AditiPredProcId,
			PredInfo, ProcInfo, ModuleInfo) },
		magic_info_set_module_info(ModuleInfo),
		{ LocalAditiPredProcId = AditiPredProcId }
	),

	{ ThisProcInfo = magic_proc_info(ArgModes1, MagicVars, 
		MagicTypes, MagicModes, MaybeIndex) },
	magic_info_get_magic_proc_info(MagicProcInfo0),
	{ map__det_insert(MagicProcInfo0, LocalAditiPredProcId,
		ThisProcInfo, MagicProcInfo) },
	magic_info_set_magic_proc_info(MagicProcInfo).

	%
	% Create an interface procedure to a sub-module for a particular
	% entry-point, used by Mercury compiled to C and Aditi procedures
	% in other modules.
	% A local version is created which takes all the input
	% arguments. The exported version calls this version
	% with empty relations for all except one of the
	% input arguments.
	%
:- pred magic__create_interface_proc(int::in, pred_proc_id::in,
		pred_proc_id::in, pred_info::in, proc_info::in, proc_info::in,
		list(prog_var)::in, list(type)::in, list(mode)::in,
		list(prog_var)::in, list(type)::in, list(mode)::in,
		pred_proc_id::out, magic_info::in, magic_info::out) is det.
	
magic__create_interface_proc(Index, CPredProcId, AditiPredProcId,
		ExportedPredInfo0, ExportedProcInfo0, LocalProcInfo,
		HeadVars1, ArgTypes1, ArgModes1, MagicVars,
		MagicTypes, MagicModes, LocalPredProcId) -->

	%
	% Create the local version.
	%
	{ proc_info_goal(LocalProcInfo, Goal) },
	magic_info_get_module_info(ModuleInfo1),
	{ proc_info_get_initial_instmap(LocalProcInfo, ModuleInfo1, InstMap) },
	{ pred_info_name(ExportedPredInfo0, PredName0) },
	{ string__append(PredName0, "__local", PredName) },
	{ proc_info_headvars(LocalProcInfo, HeadVars) },
	{ proc_info_vartypes(LocalProcInfo, VarTypes) },
	{ proc_info_varset(LocalProcInfo, VarSet) },
	{ proc_info_inst_varset(LocalProcInfo, InstVarSet) },
	{ pred_info_get_markers(ExportedPredInfo0, Markers) },
	{ pred_info_get_aditi_owner(ExportedPredInfo0, Owner) },

	{ ClassContext = constraints([], []) },
	{ map__init(TVarMap) },
	{ map__init(TCVarMap) },
	{ varset__init(TVarSet) },
	{ hlds_pred__define_new_pred(Goal, CallGoal, HeadVars, ExtraArgs,
		InstMap, PredName, TVarSet, VarTypes, ClassContext, TVarMap,
		TCVarMap, VarSet, InstVarSet, Markers, Owner,
		address_is_not_taken, ModuleInfo1, ModuleInfo2,
		LocalPredProcId) },
	{ ExtraArgs = [] ->
		true
	;
		error("magic__create_interface_proc: typeinfo arguments")
	},
	magic_info_set_module_info(ModuleInfo2),

	% Calls in this module should be redirected to point to
	% the local version.
	magic_info_get_pred_map(PredMap0),
	{ map__det_update(PredMap0, CPredProcId, LocalPredProcId, PredMap) },
	magic_info_set_pred_map(PredMap),

	%
	% Add the single magic input relation to the argument list of
	% the exported version.
	%
	{ list__index1_det(MagicVars, Index, MagicInputVar) },
	{ list__index1_det(MagicTypes, Index, MagicInputType) },
	{ list__index1_det(MagicModes, Index, MagicInputMode) },
	{ ExportedArgModes = [MagicInputMode | ArgModes1] },
	{ ExportedArgTypes = [MagicInputType | ArgTypes1] },
	{ ExportedHeadVars = [MagicInputVar | HeadVars1] },
	{ proc_info_set_headvars(ExportedProcInfo0,
		ExportedHeadVars, ExportedProcInfo1) },
	{ proc_info_set_argmodes(ExportedProcInfo1,
		ExportedArgModes, ExportedProcInfo2) },
	{ pred_info_set_arg_types(ExportedPredInfo0, TVarSet, [],
		ExportedArgTypes, ExportedPredInfo1) },

	%
	% Construct the input for the call to the local version.
	%
	magic_info_set_pred_info(ExportedPredInfo1),
	magic_info_set_proc_info(ExportedProcInfo2),
	magic__interface_call_args(MagicVars, MagicTypes, MagicModes,	
		Index, 1, InputGoals),
	magic_info_get_pred_info(ExportedPredInfo2),
	magic_info_get_proc_info(ExportedProcInfo3),
	{ CallGoal = _ - CallGoalInfo },
	{ list__append(InputGoals, [CallGoal], ExportedConj) },
	{ conj_list_to_goal(ExportedConj, CallGoalInfo, ExportedGoal) },
	{ proc_info_set_goal(ExportedProcInfo3,
		ExportedGoal, ExportedProcInfo) },

	{ pred_info_set_import_status(ExportedPredInfo2, exported,
		ExportedPredInfo) },
	magic_info_get_module_info(ModuleInfo5),
	{ module_info_set_pred_proc_info(ModuleInfo5, AditiPredProcId,
		ExportedPredInfo, ExportedProcInfo, ModuleInfo6) },
	magic_info_set_module_info(ModuleInfo6).

%-----------------------------------------------------------------------------%

:- pred magic__interface_call_args(list(prog_var)::in, list(type)::in, 
	list(mode)::in, int::in, int::in, list(hlds_goal)::out,
	magic_info::in, magic_info::out) is det.

magic__interface_call_args([], _, _, _, _, []) --> [].
magic__interface_call_args([MagicInput | MagicInputs], MagicTypes, MagicModes,	
		CalledPredIndex, CurrVar, InputGoals) -->
	{ NextVar is CurrVar + 1 },
	magic__interface_call_args(MagicInputs, MagicTypes, MagicModes,
		CalledPredIndex, NextVar, InputGoals1),
	( { CurrVar = CalledPredIndex } ->
		%
		% Just pass through the closure passed in 
		% from the calling module.
		%
		{ InputGoals = InputGoals1 }
	;
		%
		% Create an empty input closure.
		%
		{ list__index1_det(MagicTypes, CurrVar, MagicType) },
		{ 
			type_is_higher_order(MagicType, predicate,
				(aditi_bottom_up), ArgTypes1)
		->
			ArgTypes = ArgTypes1
		;
			error("magic__interface_call_args")
		},
		magic_info_get_proc_info(ProcInfo0),
		{ proc_info_create_vars_from_types(ProcInfo0, ArgTypes, 
			Args, ProcInfo) },
		magic_info_set_proc_info(ProcInfo),
		{ fail_goal(LambdaGoal) },
		{ list__index1_det(MagicModes, CurrVar, InputMode) },
		magic_util__create_closure(CurrVar, MagicInput, InputMode,
			LambdaGoal, [], Args, InputGoal),
		{ InputGoals = [InputGoal | InputGoals1] }
	).	

%-----------------------------------------------------------------------------%

:- pred magic__interface_from_c(list(pred_proc_id)::in, pred_proc_id::in,
		pred_proc_id::in, magic_info::in, magic_info::out) is det.

magic__interface_from_c(EntryPoints, CPredProcId, AditiPredProcId) -->
	magic_info_get_module_info(ModuleInfo0),
	{ module_info_globals(ModuleInfo0, Globals) },
	{ globals__lookup_bool_option(Globals, aditi_only, AditiOnly) },
	{ module_info_pred_proc_info(ModuleInfo0,
		CPredProcId, PredInfo0, ProcInfo0) },

	{ pred_info_get_markers(PredInfo0, Markers) },
	{ module_info_name(ModuleInfo0, ModuleName) },
	(
		{ AditiOnly = no },
		{ check_marker(Markers, base_relation) },
		{ pred_info_module(PredInfo0, ModuleName) }
	->
		{ pred_info_set_import_status(PredInfo0,
			exported, PredInfo1) },
		{ module_info_set_pred_proc_info(ModuleInfo0, CPredProcId,
			PredInfo1, ProcInfo0, ModuleInfo1) },
		magic_info_set_module_info(ModuleInfo1)
	;
		{ PredInfo1 = PredInfo0 },
		{ ModuleInfo1 = ModuleInfo0 }
	),
	magic_info_get_errors(Errors),
	( { pred_info_is_imported(PredInfo1) } ->
		[]
	; { \+ set__empty(Errors) } ->
		[]
	; { AditiOnly = yes ; \+ list__member(CPredProcId, EntryPoints) } ->
		%
		% If no interface procedure is required we just throw
		% away the goal. The predicate is now an ordinary Mercury
		% predicate.
		%
		{ true_goal(Goal) },
		{ proc_info_set_goal(ProcInfo0, Goal, ProcInfo) },
		{ module_info_set_pred_proc_info(ModuleInfo1, CPredProcId,
			PredInfo1, ProcInfo, ModuleInfo) },
		magic_info_set_module_info(ModuleInfo)
	;
		{ magic__create_input_join_proc(CPredProcId, AditiPredProcId,
			JoinPredProcId, ModuleInfo1, ModuleInfo) },
		magic_info_set_module_info(ModuleInfo),
		
		%
		% Create a procedure which is just a synonym
		% for do_*_aditi_call.
		%
		magic__create_aditi_call_proc(CPredProcId, JoinPredProcId)
	).

	% Make a procedure which calls the Aditi predicate, then joins
	% the result with the input and projects out the input arguments.
:- pred magic__create_input_join_proc(pred_proc_id::in, pred_proc_id::in,
		pred_proc_id::out, module_info::in, module_info::out) is det.

magic__create_input_join_proc(CPredProcId, AditiPredProcId, JoinPredProcId,
		ModuleInfo0, ModuleInfo) :-
	module_info_pred_proc_info(ModuleInfo0, CPredProcId,
		CPredInfo, CProcInfo),
	proc_info_argmodes(CProcInfo, ArgModes0),
	pred_info_arg_types(CPredInfo, ArgTypes),
	type_util__remove_aditi_state(ArgTypes, ArgModes0, ArgModes),
	partition_args(ModuleInfo0, ArgModes, ArgModes,
		InputArgModes, OutputArgModes),

	%
	% The interface procedure on the Aditi side must have
	% only one input closure argument.
	%
	proc_info_vartypes(CProcInfo, VarTypes0),
	proc_info_headvars(CProcInfo, HeadVars0),
	type_util__remove_aditi_state(ArgTypes,
		HeadVars0, HeadVars),

	partition_args(ModuleInfo0, ArgModes, HeadVars,
		InputArgs, OutputArgs),

	map__apply_to_list(InputArgs, VarTypes0, InputVarTypes),

	construct_higher_order_type(predicate, (aditi_bottom_up),
		InputVarTypes, ClosureVarType),
	list__map(magic_util__mode_to_output_mode(ModuleInfo0),
		InputArgModes, MagicArgModes),

	JoinProcInfo0 = CProcInfo,
	proc_info_create_var_from_type(JoinProcInfo0,
		ClosureVarType, no, ClosureVar, JoinProcInfo1),


	%
	% Build a goal to call the input closure.
	%

	set__list_to_set([ClosureVar | InputArgs],
		HOCallNonLocals),
	instmap_delta_from_mode_list(InputArgs, MagicArgModes,
		ModuleInfo0, HOCallDelta),
	goal_info_init(HOCallNonLocals, HOCallDelta, nondet,
		InputGoalInfo),
	list__length(InputArgs, Arity),
	InputGoal = generic_call(
		higher_order(ClosureVar, predicate, Arity),
		InputArgs, MagicArgModes, nondet) - InputGoalInfo,

	ClosureInst = ground(shared,
	    higher_order(pred_inst_info(predicate, MagicArgModes, nondet))),
	ClosureMode = (ClosureInst -> ClosureInst),
	proc_info_set_argmodes(JoinProcInfo1,
		[ClosureMode | OutputArgModes], JoinProcInfo2),
	proc_info_set_headvars(JoinProcInfo2,
		[ClosureVar | OutputArgs], JoinProcInfo3),

	magic__build_join_pred_info(CPredProcId, CPredInfo,
		JoinProcInfo3, [ClosureVar | OutputArgs],
		JoinPredProcId, JoinPredInfo, ModuleInfo0, ModuleInfo1),

	%
	% Build a call to the Aditi procedure.
	%

	AditiPredProcId = proc(AditiPredId, AditiProcId),
	proc_info_goal(CProcInfo, _ - CallGoalInfo0),

	% Convert input arguments to output arguments, producing
	% the tests which will make up the join condition.
	magic_util__create_input_test_unifications(ModuleInfo1, HeadVars,
		InputArgs, ArgModes, CallArgs0, [], Tests,
		CallGoalInfo0, CallGoalInfo, JoinProcInfo3, JoinProcInfo4),

	( hlds_pred__pred_info_is_base_relation(CPredInfo) ->
		CallArgs = CallArgs0
	;
		CallArgs = [ClosureVar | CallArgs0]
	),

	module_info_pred_info(ModuleInfo1, AditiPredId, AditiPredInfo),
	pred_info_module(AditiPredInfo, PredModule),
	pred_info_name(AditiPredInfo, PredName),
	CallGoal = call(AditiPredId, AditiProcId, CallArgs, not_builtin,
		no, qualified(PredModule, PredName)) - CallGoalInfo,

	instmap_delta_from_mode_list(OutputArgs, OutputArgModes,
		ModuleInfo1, GoalDelta),
	set__list_to_set([ClosureVar | OutputArgs],
		GoalNonLocals),
	goal_info_init(GoalNonLocals, GoalDelta, nondet, GoalInfo),
	conj_list_to_goal([InputGoal, CallGoal | Tests], GoalInfo,
		JoinGoal),
	proc_info_set_goal(JoinProcInfo4, JoinGoal, JoinProcInfo),
	module_info_set_pred_proc_info(ModuleInfo1, JoinPredProcId,
		JoinPredInfo, JoinProcInfo, ModuleInfo).

:- pred magic__build_join_pred_info(pred_proc_id::in, pred_info::in,
		proc_info::in, list(prog_var)::in, pred_proc_id::out,
		pred_info::out, module_info::in, module_info::out) is det.

magic__build_join_pred_info(CPredProcId, CPredInfo, JoinProcInfo,
		Args, JoinPredProcId, JoinPredInfo1,
		ModuleInfo0, ModuleInfo) :-
	proc_info_vartypes(JoinProcInfo, JoinVarTypes),
	map__apply_to_list(Args, JoinVarTypes, NewArgTypes),
	pred_info_module(CPredInfo, PredModule),
	rl__get_c_interface_proc_name(ModuleInfo0, CPredProcId, NewPredName),
	init_markers(Markers0),
	add_marker(Markers0, aditi, Markers1),
	add_marker(Markers1, aditi_no_memo, Markers2),
	add_marker(Markers2, naive, Markers),
	ClassContext = constraints([], []),
	pred_info_get_aditi_owner(CPredInfo, User),
	varset__init(TVarSet),	% must be empty.
	term__context_init(DummyContext),
	ExistQVars = [],
	set__init(Assertions),
	pred_info_create(PredModule, qualified(PredModule, NewPredName),
		TVarSet, ExistQVars, NewArgTypes, true, DummyContext,
		exported, Markers, predicate, ClassContext, User, Assertions,
		JoinProcInfo, JoinProcId, JoinPredInfo1),

	module_info_get_predicate_table(ModuleInfo0, Preds0),
	predicate_table_insert(Preds0, JoinPredInfo1, JoinPredId, Preds),
	JoinPredProcId = proc(JoinPredId, JoinProcId),
	module_info_set_predicate_table(ModuleInfo0, Preds, ModuleInfo).

	% The new procedure consists of a `aditi_call' goal,
	% which call_gen.m generates as a call to do_*_aditi_call in
	% extras/aditi/aditi.m.
	% This procedure must use the `compact' argument convention.
	% The arguments are:
	% 	1 -> RL procedure name
	% 	2 -> number of input arguments
	% 	3 -> input schema
	% 	4 -> number of output arguments
	%	type_infos for input arguments
	% 	input arguments
	%	type_infos for output arguments
	%	output arguments
:- pred magic__create_aditi_call_proc(pred_proc_id::in, pred_proc_id::in,
		magic_info::in, magic_info::out) is det.		

magic__create_aditi_call_proc(CPredProcId, AditiPredProcId) -->
	magic_info_get_module_info(ModuleInfo0),
	{ module_info_pred_proc_info(ModuleInfo0, CPredProcId,
		CPredInfo0, CProcInfo0) },
	{ pred_info_arg_types(CPredInfo0, ArgTypes) },
	{ proc_info_argmodes(CProcInfo0, ArgModes) },
	{ proc_info_headvars(CProcInfo0, HeadVars) },

	% Base relations will have an empty vartypes field, so fill it in here.
	{ map__from_corresponding_lists(HeadVars, ArgTypes, VarTypes0) },
	{ proc_info_set_vartypes(CProcInfo0, VarTypes0, CProcInfo1) },

	%
	% Build type-infos for the arguments so do_*_aditi_call
	% can do the required data conversions.
	%

	{ type_util__remove_aditi_state(ArgTypes, ArgTypes, ArgTypes1) },
	{ type_util__remove_aditi_state(ArgTypes, ArgModes, ArgModes1) },
	{ type_util__remove_aditi_state(ArgTypes, HeadVars, HeadVars1) },

	magic__make_type_info_vars(ArgTypes1, TypeInfoVars, TypeInfoGoals,
		CPredInfo0, CPredInfo1, CProcInfo1, CProcInfo2),

	magic_info_get_module_info(ModuleInfo1),

	{ partition_args(ModuleInfo1, ArgModes1, ArgTypes1,
		InputArgTypes, _OutputArgTypes) },
	{ partition_args(ModuleInfo1, ArgModes1, ArgModes1,
		InputArgModes, OutputArgModes) },
	{ partition_args(ModuleInfo1, ArgModes1, TypeInfoVars,
		InputTypeInfoVars, OutputTypeInfoVars) },
	{ partition_args(ModuleInfo1, ArgModes1,
		HeadVars1, InputArgs, OutputArgs) },

	%
	% Build up some other information that do_*_aditi_call needs.
	% 

	% Argument variables.
	{ list__condense([InputTypeInfoVars, InputArgs,
		OutputTypeInfoVars, OutputArgs], DoCallAditiArgs) },

	% Argument modes.
	{ in_mode(InMode) },
	{ list__length(InputArgs, NumInputArgs) },
	{ list__length(OutputArgs, NumOutputArgs) },
	{ list__duplicate(NumInputArgs, InMode, InputTypeInfoModes) },
	{ list__duplicate(NumOutputArgs, InMode, OutputTypeInfoModes) },
	{ list__condense([InputTypeInfoModes, InputArgModes,
		OutputTypeInfoModes, OutputArgModes], DoCallAditiArgModes) },

	%
	% Build the `aditi_call' goal.
	%
	{ set__list_to_set(DoCallAditiArgs, CallNonLocals) },
	{ instmap_delta_from_mode_list(DoCallAditiArgs, DoCallAditiArgModes,
		ModuleInfo1, GoalDelta) },
	{ proc_info_inferred_determinism(CProcInfo2, Detism) },
	{ goal_info_init(CallNonLocals, GoalDelta, Detism, CallGoalInfo) },
	{ pred_info_get_is_pred_or_func(CPredInfo1, CPredOrFunc) },
	{ pred_info_module(CPredInfo1, CPredModule) },
	{ pred_info_name(CPredInfo1, CPredName) },
	{ pred_info_arity(CPredInfo1, Arity) },
	{ DoCallAditiGoal =
		generic_call(aditi_builtin(
			aditi_call(AditiPredProcId, NumInputArgs,
				InputArgTypes, NumOutputArgs),
			CPredOrFunc - qualified(CPredModule, CPredName)/Arity),
		DoCallAditiArgs, DoCallAditiArgModes, Detism) - CallGoalInfo },
	{ list__append(TypeInfoGoals, [DoCallAditiGoal], Goals) },
	{ set__list_to_set(HeadVars, GoalNonLocals) },
	{ goal_list_determinism(Goals, GoalDetism) },
	{ goal_info_init(GoalNonLocals, GoalDelta, GoalDetism, GoalInfo) },
	{ Goal = conj(Goals) - GoalInfo },
	{ proc_info_set_goal(CProcInfo2, Goal, CProcInfo) },

	{ module_info_set_pred_proc_info(ModuleInfo1, CPredProcId,
		CPredInfo1, CProcInfo, ModuleInfo) },
	magic_info_set_module_info(ModuleInfo).

:- pred magic__make_type_info_vars(list(type)::in, list(prog_var)::out,
	list(hlds_goal)::out, pred_info::in, pred_info::out,
	proc_info::in, proc_info::out, magic_info::in, magic_info::out) is det.

magic__make_type_info_vars(Types, TypeInfoVars, TypeInfoGoals,
		PredInfo0, PredInfo, ProcInfo0, ProcInfo) -->
	magic_info_get_module_info(ModuleInfo0),
	{ create_poly_info(ModuleInfo0, PredInfo0, ProcInfo0, PolyInfo0) },
	{ term__context_init(Context) },
	{ polymorphism__make_type_info_vars(Types, Context,
		TypeInfoVars, TypeInfoGoals, PolyInfo0, PolyInfo) },
	{ poly_info_extract(PolyInfo, PredInfo0, PredInfo,
		ProcInfo0, ProcInfo, ModuleInfo) },
	magic_info_set_module_info(ModuleInfo).

:- pred magic__make_const((type)::in, cons_id::in, prog_var::out,
		hlds_goal::out, proc_info::in, proc_info::out) is det.

magic__make_const(Type, ConsId, Var, Goal, ProcInfo0, ProcInfo) :-
	proc_info_create_var_from_type(ProcInfo0, Type, no, Var, ProcInfo),
	set__singleton_set(NonLocals, Var),
	Inst = bound(unique, [functor(ConsId, [])]),
	instmap_delta_init_reachable(Delta0),
	instmap_delta_insert(Delta0, Var, Inst, Delta),
	UnifyMode = (free -> Inst) - (Inst -> Inst),
	RLExprnId = no,
	Uni = construct(Var, ConsId, [], [],
		construct_dynamically, cell_is_unique, RLExprnId),
	Context = unify_context(explicit, []),
	goal_info_init(NonLocals, Delta, det, GoalInfo),
	Goal = unify(Var, functor(ConsId, no, []), UnifyMode, Uni, Context) -
			GoalInfo.
		
%-----------------------------------------------------------------------------%

	% Allocate a predicate to collect the input for the current predicate.
:- pred magic__create_magic_pred(pred_proc_id::in, pred_proc_id::in, 
		list(type)::in, list(mode)::in, list(type)::in, 
		list(mode)::in, inst_varset::in, maybe(int)::in,
		magic_info::in, magic_info::out) is det.

magic__create_magic_pred(CPredProcId, PredProcId, MagicTypes, MagicModes, 
		InputTypes0, InputModes0, InstVarSet, Index) -->

	magic_info_get_module_info(ModuleInfo0),

	{ varset__init(VarSet0) },
	{ map__init(VarTypes0) },

		% Get some new vars to carry the magic input.
	{ list__length(MagicTypes, NumMagicArgs) },
	{ varset__new_vars(VarSet0, NumMagicArgs, MagicArgs, VarSet1) },
	{ map__det_insert_from_corresponding_lists(VarTypes0, MagicArgs, 
		MagicTypes, VarTypes1) },

		% Get some new vars for the outputs.
	{ list__length(InputModes0, NumInputArgs) },
	{ varset__new_vars(VarSet1, NumInputArgs, InputArgs0, VarSet2) },
	{ map__det_insert_from_corresponding_lists(VarTypes1, InputArgs0, 
		InputTypes0, VarTypes2) },

	{ list__map(magic_util__mode_to_output_mode(ModuleInfo0),
		InputModes0, OutputModes0) },

	{ module_info_pred_proc_info(ModuleInfo0, PredProcId,
		PredInfo, _) },
	{ pred_info_get_markers(PredInfo, Markers) },
	{ check_marker(Markers, context) ->
		% For magic context predicates, we get two copies of
		% the outputs. (See the paper cited at the top of context.m)
		varset__new_vars(VarSet2, NumInputArgs, InputArgs1, VarSet),
		map__det_insert_from_corresponding_lists(VarTypes2,
			InputArgs1, InputTypes0, VarTypes),
		list__append(InputArgs0, InputArgs1, InputArgs),
		list__append(InputTypes0, InputTypes0, InputTypes),
		list__append(OutputModes0, OutputModes0, OutputModes),
		assoc_list__from_corresponding_lists(InputArgs0, InputArgs1,
			ArgsAL0),
		IsContext = yes(ArgsAL0)	
	;
		VarSet = VarSet2,
		VarTypes = VarTypes2,
		InputArgs = InputArgs0,
		InputTypes = InputTypes0,
		OutputModes = OutputModes0,
		IsContext = no
	},

	{ list__append(MagicArgs, InputArgs, AllArgs) },
	( { Index = yes(N) } ->
		%
		% If this predicate is an entry point to the sub-module,
		% create a rule in the magic predicate to collect
		% the input relation.
		%
		{ list__index1_det(MagicArgs, N, CurrPredVar) },
		{ set__list_to_set([CurrPredVar | InputArgs0], NonLocals0) },
		{ mode_list_get_final_insts(OutputModes0, ModuleInfo0,
			OutputInsts0) },
		{ assoc_list__from_corresponding_lists(InputArgs0, 
			OutputInsts0, InstAL0) },
		{ instmap_delta_from_assoc_list(InstAL0, InstMapDelta0) },
		{ goal_info_init(NonLocals0, InstMapDelta0,
			nondet, GoalInfo0) },
		{ list__length(InputArgs0, Arity) },
		{ Goal0 = generic_call(
			higher_order(CurrPredVar, predicate, Arity),
			InputArgs0, OutputModes0, nondet) - GoalInfo0 },
		( { IsContext = yes(ArgsAL) } ->
			% Create assignments to assign to the extra arguments.
			{ magic__create_assignments(ModuleInfo0, ArgsAL,
				OutputModes0, Assigns) },
			{ list__append(OutputInsts0,
				OutputInsts0, OutputInsts) },
			{ assoc_list__from_corresponding_lists(InputArgs, 
				OutputInsts, InstAL) },
			{ instmap_delta_from_assoc_list(InstAL, InstMapDelta) },
			{ set__list_to_set([CurrPredVar | InputArgs],
				NonLocals) },
			{ goal_info_init(NonLocals, InstMapDelta, nondet,
				GoalInfo) },
			{ conj_list_to_goal([Goal0 | Assigns],
				GoalInfo, Goal) }
		;
			{ Goal = Goal0 }
		)
	;
		% This predicate is not an entry point, so there's
		% no input to collect.
		{ fail_goal(Goal) }
	),

	{ list__append(MagicModes, OutputModes, AllArgModes) },

	{ term__context_init(Context) },

	% types must all be ground.
	{ map__init(TVarMap) },
	{ map__init(TCVarMap) },

	{ proc_info_create(VarSet, VarTypes, AllArgs, AllArgModes, InstVarSet,
		nondet, Goal, Context, TVarMap, TCVarMap, address_is_not_taken,
		ProcInfo) },
	
	%
	% Fill in the pred_info.
	%

	{ CPredProcId = proc(CPredId, CProcId) },
	{ module_info_pred_info(ModuleInfo0, CPredId, CPredInfo) },
	{ pred_info_module(CPredInfo, ModuleName) },
	magic_util__make_pred_name(CPredInfo, CProcId,
		"Magic_Proc_For", no, SymName),

	{ list__append(MagicTypes, InputTypes, AllArgTypes) },
	{ varset__init(TVarSet) },
	{ pred_info_get_aditi_owner(PredInfo, Owner) },
	{ ClassConstraints = constraints([], []) },
	{ ExistQVars = [] },
	{ set__init(Assertions) },
	{ pred_info_create(ModuleName, SymName, TVarSet, ExistQVars,
		AllArgTypes, true, Context, local, Markers, predicate,
		ClassConstraints, Owner, Assertions, ProcInfo, MagicProcId,
		MagicPredInfo) },

	{ module_info_get_predicate_table(ModuleInfo0, PredTable0) },
	{ predicate_table_insert(PredTable0, 
		MagicPredInfo, MagicPredId, PredTable) },
	{ module_info_set_predicate_table(ModuleInfo0, PredTable,
		ModuleInfo) },
	magic_info_set_module_info(ModuleInfo),

	% Record that the magic predicate in the magic_info.
	{ MagicPredProcId = proc(MagicPredId, MagicProcId) },
	magic_info_get_magic_map(MagicMap0),
	{ map__det_insert(MagicMap0, PredProcId, MagicPredProcId, MagicMap) },
	magic_info_set_magic_map(MagicMap).
	

	% Produce assignments to the duplicate outputs
	% of a context magic predicate.
:- pred magic__create_assignments(module_info::in,
		assoc_list(prog_var, prog_var)::in,
		list(mode)::in, list(hlds_goal)::out) is det.	

magic__create_assignments(_, [], [], []).
magic__create_assignments(_, [], [_|_], _) :-
	error("magic__create_assignments").
magic__create_assignments(_, [_|_], [], _) :-
	error("magic__create_assignments").
magic__create_assignments(ModuleInfo, [Arg0 - Arg | ArgsAL],
		[Mode | Modes], [Goal - GoalInfo | Assigns]) :-
	mode_get_insts(ModuleInfo, Mode, _, Inst),
	Goal = unify(Arg, var(Arg0), (free -> Inst) - (Inst -> Inst),
			assign(Arg, Arg0), unify_context(explicit, [])),
	set__list_to_set([Arg0, Arg], NonLocals),
	instmap_delta_from_assoc_list([Arg - Inst], Delta),
	goal_info_init(NonLocals, Delta, det, GoalInfo),
	magic__create_assignments(ModuleInfo, ArgsAL, Modes, Assigns).	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Put the goal for a procedure in a form suitable for processing.
:- pred magic__preprocess_proc(pred_proc_id::in, pred_info::in,
		proc_info::in, proc_info::out,
		magic_info::in, magic_info::out) is det.

magic__preprocess_proc(PredProcId, PredInfo, ProcInfo0, ProcInfo) -->
	{ proc_info_goal(ProcInfo0, Goal0) },
	magic_info_set_curr_pred_proc_id(PredProcId),
	magic_info_set_pred_info(PredInfo),
	magic_info_set_proc_info(ProcInfo0),
	{ Goal0 = _ - GoalInfo0 },
	{ goal_to_disj_list(Goal0, GoalList0) },
	list__map_foldl(magic__preprocess_disjunct, 
			GoalList0, GoalList),
	{ disj_list_to_goal(GoalList, GoalInfo0, Goal) },
	magic_info_get_proc_info(ProcInfo1),
	{ proc_info_set_goal(ProcInfo1, Goal, ProcInfo) }.

	% Undo common structure elimination of higher-order terms in an
	% attempt to avoid creating procedures with higher-order arguments
	% in the case where one closure is used by multiple aggregate calls.
	% Restore superhomogeneous form for database predicates by introducing
	% new variables for duplicate input arguments.
	% Also remove assignments of `aditi:state's and report errors
	% for goals other than database calls which have an `aditi:state'
	% as a nonlocal.
:- pred magic__preprocess_disjunct(hlds_goal::in, hlds_goal::out,
		magic_info::in, magic_info::out) is det.

magic__preprocess_disjunct(Disjunct0, Disjunct) -->
	{ map__init(HOMap0) },
	{ Disjunct0 = _ - DisjInfo },
	magic__preprocess_goal(Disjunct0, Disjunct1, HOMap0, _),
	{ conj_list_to_goal(Disjunct1, DisjInfo, Disjunct) }.

:- pred magic__preprocess_goal(hlds_goal::in, list(hlds_goal)::out,
		map(prog_var, hlds_goal)::in, map(prog_var, hlds_goal)::out,
		magic_info::in, magic_info::out) is det.

magic__preprocess_goal(Goal, Goals, HOMap0, HOMap) -->
	magic__preprocess_goal_2(Goal, Goals, HOMap0, HOMap),
	list__foldl(magic__check_goal_nonlocals, Goals). 

:- pred magic__preprocess_goal_2(hlds_goal::in, list(hlds_goal)::out,
		map(prog_var, hlds_goal)::in, map(prog_var, hlds_goal)::out,
		magic_info::in, magic_info::out) is det.

	% Switches, if-then-elses and disjunctions involving database calls
	% should have been transformed into separate procedures by dnf.m.
magic__preprocess_goal_2(Goal, [Goal], HOMap, HOMap) -->
	{ Goal = disj(_) - _ }.
magic__preprocess_goal_2(Goal, [Goal], HOMap, HOMap) -->
	{ Goal = switch(_, _, _) - _ }.
magic__preprocess_goal_2(Goal, [Goal], HOMap, HOMap) --> 
	{ Goal = if_then_else(_, _, _, _) - _ }.
magic__preprocess_goal_2(par_conj(_) - _, _, _, _) -->
	{ error("Sorry, not yet implemented: parallel conjunction in Aditi procedures") }.
magic__preprocess_goal_2(generic_call(_, _, _, _) - _, _, _, _) -->
	{ error("Sorry, not yet implemented: higher-order or class-method calls in Aditi procedures") }.
magic__preprocess_goal_2(foreign_proc(_, _, _, _, _, _, _) -
	_, _, _, _) -->
	{ error("Sorry, not yet implemented: foreign_proc calls in Aditi procedures") }.
magic__preprocess_goal_2(conj(Goals0) - GoalInfo, [conj(Goals) - GoalInfo],
		HOMap0, HOMap) -->
	magic__preprocess_conj(Goals0, [], Goals, HOMap0, HOMap).
magic__preprocess_goal_2(Goal0, Goals, HOMap, HOMap) -->
	{ Goal0 = call(PredId, B, Args, C, D, E) - GoalInfo },
	magic_info_get_module_info(ModuleInfo),
	( { hlds_pred__is_aditi_aggregate(ModuleInfo, PredId) } ->
		% Put the closures and the aggregate call in a sub-conjunction
		% of the top-level conjunction.
		magic__rename_and_generate_closures(Args, ExtraGoals,
			Goal0, Goal1, HOMap),
		{ list__append(ExtraGoals, [Goal1], Goals1) },
		{ Goal0 = _ - GoalInfo0 },
		{ conj_list_to_goal(Goals1, GoalInfo0, Goal) },
		{ Goals = [Goal] }
	; { hlds_pred__is_aditi_relation(ModuleInfo, PredId) } ->
		% The predicates in magic_util.m to deal with input arguments
		% expect that there are no duplicates. 
		{ set__init(SeenArgs) },
		magic__preprocess_call_args(Args, NewArgs, SeenArgs,
			[], IntroducedArgs, [], ExtraGoals),
		{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
		{ set__insert_list(NonLocals, IntroducedArgs, NewNonLocals) },
		{ goal_info_set_nonlocals(GoalInfo,
			NewNonLocals, NewGoalInfo) },
		{ NewCall = call(PredId, B, NewArgs, C, D, E) - NewGoalInfo },
		{ list__append(ExtraGoals, [NewCall], Goals) }
	;
		{ Goals = [Goal0] }
	).
magic__preprocess_goal_2(some(Vars, CanRemove, Goal0) - Info,
		[some(Vars, CanRemove, Goal) - Info], HOMap0, HOMap) -->
	{ Goal0 = _ - SomeGoalInfo },
	magic__preprocess_goal(Goal0, SomeGoals, HOMap0, HOMap),
	{ conj_list_to_goal(SomeGoals, SomeGoalInfo, Goal) }.
magic__preprocess_goal_2(not(Goal0) - Info, [not(Goal) - Info],
		HOMap0, HOMap) -->
	{ Goal0 = _ - NegGoalInfo },
	magic__preprocess_goal(Goal0, NegGoals, HOMap0, HOMap),
	{ conj_list_to_goal(NegGoals, NegGoalInfo, Goal) }.
magic__preprocess_goal_2(Goal0, Goals, HOMap0, HOMap) -->
	{ Goal0 = unify(_, _, _, Uni, _) - GoalInfo },
	(
		{ Uni = construct(Var, pred_const(_, _, _), Args, _, _, _, _) }
	->
		% Collect up the closure construction so that it can be
		% placed next to the aggregate goal that uses it.
		% 
		% XXX What about if someone puts a closure inside
		% a structure? At the moment we don't handle it and
		% we don't give an error message.
		magic_info_get_proc_info(ProcInfo),
		(
			{ Args = [] }
		->
			[]
		; 
			{ Args = [Arg] },
			{ proc_info_vartypes(ProcInfo, VarTypes) },
			{ map__lookup(VarTypes, Arg, ArgType) },
			{ type_is_aditi_state(ArgType) }
		->
			[]
		;
			% XXX we don't yet allow curried arguments.
			{ goal_info_get_context(GoalInfo, Context) },
			magic_info_get_curr_pred_proc_id(PredProcId),
			magic_info_get_errors(Errors0),
			{ Error = curried_argument(PredProcId) - Context },
			{ set__insert(Errors0, Error, Errors) },
			magic_info_set_errors(Errors)
		),
		{ map__det_insert(HOMap0, Var, Goal0, HOMap) },
		{ Goals = [] }
	;
		{ Uni = assign(Var1, Var2) }
	->
		magic_info_get_proc_info(ProcInfo),
		{ proc_info_vartypes(ProcInfo, VarTypes) },	
		{ map__lookup(VarTypes, Var1, Var1Type) },
		( { type_is_aditi_state(Var1Type) } ->
			% Remove assignments of `aditi:state's.
			{ HOMap = HOMap0 },
			{ Goals = [] }
		; { map__search(HOMap0, Var2, Entry) } ->
			{ Goals = [] },
			{ map__det_insert(HOMap0, Var1, Entry, HOMap) }
		;
			{ Goals = [Goal0] },
			{ HOMap = HOMap0 }
		)
	;
		{ Goals = [Goal0] },
		{ HOMap = HOMap0 }
	).

magic__preprocess_goal_2(shorthand(_) - _, _, _, _) -->
	% these should have been expanded out by now
	{ error("magic__preprocess_goal_2: unexpected shorthand") }.

	% Introduce new variables and assignments to them for any
	% duplicates in the list.
:- pred magic__preprocess_call_args(list(prog_var)::in, list(prog_var)::out,
	set(prog_var)::in, list(prog_var)::in, list(prog_var)::out,
	list(hlds_goal)::in, list(hlds_goal)::out,
	magic_info::in, magic_info::out) is det.

magic__preprocess_call_args([], [], _, IntroducedArgs,
		IntroducedArgs, ExtraGoals, ExtraGoals) --> [].
magic__preprocess_call_args([Arg | Args], [NewArg | NewArgs], SeenArgs,
		IntroducedArgs0, IntroducedArgs, ExtraGoals0, ExtraGoals) -->
	( { set__member(Arg, SeenArgs) } ->
		{ SeenArgs1 = SeenArgs },
		magic_info_get_proc_info(ProcInfo0),
		{ proc_info_vartypes(ProcInfo0, VarTypes) },
		{ map__lookup(VarTypes, Arg, ArgType) },
		{ proc_info_create_var_from_type(ProcInfo0,
			ArgType, no, NewArg, ProcInfo) },
		magic_info_set_proc_info(ProcInfo),
		{ IntroducedArgs1 = [NewArg | IntroducedArgs0] },
		{ in_mode(InMode) },
		{ out_mode(OutMode) },
		{ Inst = ground(shared, none) },
		{ set__list_to_set([Arg, NewArg], NonLocals) },
		{ instmap_delta_from_assoc_list([NewArg - Inst], Delta) },
		{ goal_info_init(NonLocals, Delta, det, GoalInfo) },
		{ ExtraGoal = unify(NewArg, var(Arg), OutMode - InMode,
			assign(NewArg, Arg), unify_context(explicit, []))
			- GoalInfo },
		{ ExtraGoals1 = [ExtraGoal | ExtraGoals0] }
	;
		{ NewArg = Arg },
		{ set__insert(SeenArgs, Arg, SeenArgs1) },
		{ IntroducedArgs1 = IntroducedArgs0 },
		{ ExtraGoals1 = ExtraGoals0 }
	),
	magic__preprocess_call_args(Args, NewArgs, SeenArgs1, IntroducedArgs1,
		IntroducedArgs, ExtraGoals1, ExtraGoals).

:- pred magic__preprocess_conj(list(hlds_goal)::in, list(hlds_goal)::in,
	list(hlds_goal)::out, map(prog_var, hlds_goal)::in,
	map(prog_var, hlds_goal)::out, magic_info::in, magic_info::out) is det.

magic__preprocess_conj([], RevGoals, Goals, HOMap, HOMap) -->
	{ list__reverse(RevGoals, Goals) }.
magic__preprocess_conj([Goal0 | Goals0], RevGoals0, Goals, HOMap0, HOMap) -->
	magic__preprocess_goal(Goal0, Goals1, HOMap0, HOMap1),
	{ list__reverse(Goals1, RevGoals1) },
	{ list__append(RevGoals1, RevGoals0, RevGoals) },
	magic__preprocess_conj(Goals0, RevGoals, Goals, HOMap1, HOMap).

	% If the goal is not a database call and does not contain
	% a database call, it cannot have an `aditi:state' as a non-local.
:- pred magic__check_goal_nonlocals(hlds_goal::in,
		magic_info::in, magic_info::out) is det.

magic__check_goal_nonlocals(Goal) -->
	magic_info_get_module_info(ModuleInfo),
	magic_info_get_pred_map(PredMap),
	(
		% We check inside not, some and conj goals for calls, so don't
		% report errors at the top-level of those goals.
		{
			Goal = not(_) - _
		;
			Goal = some(_, _, _) - _
		;
			Goal = conj(_) - _
		; 	
			Goal = call(_, _, _, _, _, _) - _,
			magic_util__goal_is_aditi_call(ModuleInfo, PredMap,
				Goal, _, _)
		;
			Goal = unify(_, _, _, Uni, _) - _,
			Uni = construct(_, pred_const(PredId, ProcId, _),
				_, _, _, _, _),
			% XXX once the implementation of aggregates has
			% been updated to use `aditi_bottom_up' closures,
			% this can be done by just checking the eval_method.
			(
				map__contains(PredMap, proc(PredId, ProcId))
			;
				hlds_pred__is_aditi_relation(ModuleInfo,
					PredId)
			)
		}
	->
		[]
	;
		{ Goal = _ - GoalInfo },
		% We don't use the non-locals because in some circumstances
		% involving mode analysis or simplification optimizing
		% away unifications that set can be an overestimate.
		{ quantification__goal_vars(Goal, GoalVars0) },
		{ set__to_sorted_list(GoalVars0, GoalVars) },
		magic_info_get_proc_info(ProcInfo),
		{ proc_info_vartypes(ProcInfo, VarTypes) },
		{ IsAditiVar = lambda([Var::in] is semidet, (
					map__lookup(VarTypes, Var, Type),
					type_is_aditi_state(Type)
				)) },
		{ list__filter(IsAditiVar, GoalVars, IllegalNonLocals) },
		( { IllegalNonLocals = [] } ->
			[]
		;
			magic_info_get_errors(Errors0),
			{ goal_info_get_context(GoalInfo, Context) },
			magic_info_get_curr_pred_proc_id(PredProcId),
			{ proc_info_varset(ProcInfo, VarSet) },
			{ Error = non_removeable_aditi_state(PredProcId,
				VarSet, IllegalNonLocals) - Context },
			{ set__insert(Errors0, Error, Errors) },
			magic_info_set_errors(Errors)			
		)
	).

%-----------------------------------------------------------------------------%

	% Generate goals to create the closures needed by this call.
:- pred magic__rename_and_generate_closures(list(prog_var)::in, 
	list(hlds_goal)::out, hlds_goal::in, hlds_goal::out,
	map(prog_var, hlds_goal)::in, magic_info::in, magic_info::out) is det.

magic__rename_and_generate_closures([], [], Goal, Goal, _) --> [].
magic__rename_and_generate_closures([Arg | Args], ExtraGoals, 
		Goal0, Goal, HOMap) -->
	magic__rename_and_generate_closures(Args, ExtraGoals1, 
		Goal0, Goal1, HOMap), 
	( { map__search(HOMap, Arg, ClosureGoal0) } ->
		magic_info_get_proc_info(ProcInfo0),
		{ proc_info_vartypes(ProcInfo0, VarTypes0) },
		{ map__lookup(VarTypes0, Arg, Type) },
		{ proc_info_create_var_from_type(ProcInfo0, 
			Type, no, NewArg, ProcInfo) },
		magic_info_set_proc_info(ProcInfo),
		{ map__init(Subn0) },
		{ map__det_insert(Subn0, Arg, NewArg, Subn) },
		{ goal_util__rename_vars_in_goal(ClosureGoal0,
			Subn, ClosureGoal) },
		{ goal_util__rename_vars_in_goal(Goal1, Subn, Goal) },
		{ ExtraGoals = [ClosureGoal | ExtraGoals1] }
	;	
		{ ExtraGoals = ExtraGoals1 },
		{ Goal = Goal1 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred magic__process_scc(aditi_scc::in, magic_info::in,
		magic_info::out, io__state::di, io__state::uo) is det.

magic__process_scc(aditi_scc(SCC0, _), Info0, Info) -->
	{ list__condense(SCC0, SCC) },
	{ magic_info_set_scc(SCC, Info0, Info1) },
	{ list__foldl(magic__process_proc, SCC, Info1, Info) }.

%-----------------------------------------------------------------------------%

:- pred magic__process_proc(pred_proc_id::in, 
		magic_info::in, magic_info::out) is det.

magic__process_proc(PredProcId0) -->
	magic_info_get_pred_map(PredMap),
	{ map__search(PredMap, PredProcId0, PredProcId1) ->
		PredProcId = PredProcId1
	;
		PredProcId = PredProcId0
	},

	magic_info_get_module_info(ModuleInfo0),
	{ module_info_pred_proc_info(ModuleInfo0, PredProcId, 
		PredInfo0, ProcInfo0) },
	(
		{ pred_info_is_imported(PredInfo0)
		; pred_info_is_pseudo_imported(PredInfo0)
		}
	->
		[]
	;
		magic_info_set_curr_pred_proc_id(PredProcId),
		magic_info_set_pred_info(PredInfo0),
		magic_info_set_proc_info(ProcInfo0),
		magic_info_get_magic_proc_info(MagicProcInfo),
		{ map__lookup(MagicProcInfo, PredProcId, ThisProcInfo) },
		{ ThisProcInfo = magic_proc_info(OldArgModes, MagicInputs,
					_, _, _) },
		magic_info_set_magic_vars(MagicInputs),
		{ set__init(ErrorVars) },
		magic_info_set_error_vars(ErrorVars),

		{ proc_info_headvars(ProcInfo0, HeadVars) },

		{ list__length(MagicInputs, NumMagicInputs) },
		{ list__drop(NumMagicInputs, HeadVars, OldHeadVars) ->
			partition_args(ModuleInfo0, 
				OldArgModes, OldHeadVars, Inputs, Outputs)
		;
			error("magic__process_proc: list__drop failed")
		},

		{ pred_info_get_markers(PredInfo0, Markers) },
		{ proc_info_goal(ProcInfo0, Goal0) },
		{ Goal0 = _ - GoalInfo0 },
		{ goal_to_disj_list(Goal0, DisjList0) },
		( { check_marker(Markers, context) } ->
			context__process_disjuncts(PredProcId0, Inputs,
				Outputs, DisjList0, DisjList)
		;
			{ set__list_to_set(HeadVars, HeadVarSet) },
			magic__process_disjuncts(HeadVarSet,
				DisjList0, DisjList)
		),

		{ disj_list_to_goal(DisjList, GoalInfo0, Goal) },
		magic_info_get_pred_info(PredInfo),
		magic_info_get_proc_info(ProcInfo1),
		{ proc_info_set_goal(ProcInfo1, Goal, ProcInfo) },

		magic_info_get_module_info(ModuleInfo1),
		{ module_info_set_pred_proc_info(ModuleInfo1, PredProcId,
			PredInfo, ProcInfo, ModuleInfo) },
		magic_info_set_module_info(ModuleInfo)
	).

%-----------------------------------------------------------------------------%

:- pred magic__process_disjuncts(set(prog_var)::in, list(hlds_goal)::in,
		list(hlds_goal)::out, magic_info::in, magic_info::out) is det.	

magic__process_disjuncts(_, [], []) --> [].
magic__process_disjuncts(HeadVars, [Disjunct0 | Disjuncts0],
		[Disjunct | Disjuncts]) -->
	magic__process_disjunct(HeadVars, Disjunct0, Disjunct),
	magic__process_disjuncts(HeadVars, Disjuncts0, Disjuncts).

:- pred magic__process_disjunct(set(prog_var)::in, hlds_goal::in,
		hlds_goal::out, magic_info::in, magic_info::out) is det.	

magic__process_disjunct(HeadVars, Disjunct0, Disjunct) --> 
	{ Disjunct0 = _ - DisjInfo },
	{ goal_to_conj_list(Disjunct0, GoalList0) },
	{ list__reverse(GoalList0, RevGoalList0) },
	magic__get_next_db_pred(RevGoalList0, BeforeGoals, 
		MaybeDBCall, [], AfterGoals),

	( { MaybeDBCall = yes(DBCall1) } ->
		{ magic_util__db_call_nonlocals(DBCall1, NonLocals1) },
		{ goal_list_nonlocals(AfterGoals, AfterNonLocals) },
		{ set__union(HeadVars, AfterNonLocals, SubConjNonLocals0) },
		{ set__union(SubConjNonLocals0, NonLocals1,
			SubConjNonLocals1) },
		magic_util__restrict_nonlocals(SubConjNonLocals1,
			SubConjNonLocals),
		magic__process_disjunct_2(BeforeGoals, DBCall1,
			SubConjNonLocals, GoalList1),
		{ list__append(GoalList1, AfterGoals, GoalList) }
	;
		magic__create_magic_call(MagicCall),
		{ GoalList = [MagicCall | GoalList0] }
	),
	{ conj_list_to_goal(GoalList, DisjInfo, Disjunct) }.

	% Search backwards through the goal list for a disjunct.
	% When a call is found, recursively process the goals before 
	% it. 
:- pred magic__process_disjunct_2(list(hlds_goal)::in, db_call::in,
		set(prog_var)::in, list(hlds_goal)::out,
		magic_info::in, magic_info::out) is det.

magic__process_disjunct_2(RevBeforeGoals1, DBCall1, NonLocals0, Goals) -->

	% Find the next database call.
	magic__get_next_db_pred(RevBeforeGoals1, RevBeforeGoals2, 
		MaybeDBCall2, [], AfterGoals2),

	( { MaybeDBCall2 = yes(DBCall2) } ->

		% Recursively process the goals before the call we just found.
		{ magic_util__db_call_nonlocals(DBCall2, CallNonLocals2) },
		{ goal_list_nonlocals(AfterGoals2, AfterNonLocals) },
		{ set__union(NonLocals0, AfterNonLocals, NonLocals1) },
		{ set__union(NonLocals1, CallNonLocals2, NonLocals2) },
		magic_util__restrict_nonlocals(NonLocals2, NonLocals),
		magic__process_disjunct_2(RevBeforeGoals2, DBCall2,
			NonLocals, Goals2),
		{ list__append(Goals2, AfterGoals2, Goals3) },

		% Turn those goals into a supplementary predicate, and
		% use that to create the input for the first call.
		magic_util__setup_call(Goals3, DBCall1, NonLocals0, Goals)
	;
		% We've run out of calls to process, so get the magic 
		% input for this procedure to feed the other calls.
		magic__create_magic_call(MagicCall),
		{ list__reverse(RevBeforeGoals1, BeforeGoals1) },
		magic_util__setup_call([MagicCall | BeforeGoals1],
			DBCall1, NonLocals0, Goals)
	).

%-----------------------------------------------------------------------------%

	% Skip along the reversed list of goals to the first database call,
	% returning the list of goals before and after the call as well.
:- pred magic__get_next_db_pred(list(hlds_goal)::in, list(hlds_goal)::out, 
		maybe(db_call)::out, list(hlds_goal)::in, 
		list(hlds_goal)::out, magic_info::in, magic_info::out) is det.

magic__get_next_db_pred([], [], no, Goals, Goals) --> [].
magic__get_next_db_pred([Goal | RevGoals], RevBeforeGoals, 
		MaybeCall, AfterGoals0, AfterGoals) -->
	magic_info_get_module_info(ModuleInfo),
	magic_info_get_pred_map(PredMap),
	(
		{ magic_util__goal_is_aditi_call(ModuleInfo, PredMap,
			Goal, Call, AfterGoals1) }
	->
		{ MaybeCall = yes(Call) },
		{ RevBeforeGoals = RevGoals },
		{ list__append(AfterGoals1, AfterGoals0, AfterGoals) }
	;
		magic__get_next_db_pred(RevGoals, RevBeforeGoals,
			MaybeCall, [Goal | AfterGoals0], AfterGoals)
	).

%-----------------------------------------------------------------------------%

	% Create a call to the magic procedure for the current procedure.
:- pred magic__create_magic_call(hlds_goal::out,
		magic_info::in, magic_info::out) is det.

magic__create_magic_call(MagicCall) -->
	magic_util__magic_call_info(MagicPredId, MagicProcId, PredName,
		InputRels, InputArgs, MagicOutputModes),

	{ list__append(InputRels, InputArgs, MagicArgs) },

	{ set__list_to_set(MagicArgs, NonLocals) },
	magic_info_get_module_info(ModuleInfo),
	{ instmap_delta_from_mode_list(InputArgs, MagicOutputModes,
		ModuleInfo, InstMapDelta) },
	{ goal_info_init(NonLocals, InstMapDelta, nondet, GoalInfo) },

	{ MagicCall = call(MagicPredId, MagicProcId, MagicArgs,
			not_builtin, no, PredName) - GoalInfo }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
