%-----------------------------------------------------------------------------%
% Copyright (C) 1998-1999,2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_gen.m
% Main author: stayl
%
% HLDS to RL (see rl.m).
% 
% This assumes that one of the supplementary magic set or context
% transformations has been applied.
%
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_gen.

:- interface.

:- import_module hlds__hlds_module, aditi_backend__rl.
:- import_module io.

:- pred rl_gen__module(module_info, rl_code, io__state, io__state).
:- mode rl_gen__module(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module ll_backend__code_aux, ll_backend__code_util.
:- import_module check_hlds__det_analysis, hlds__hlds_data, hlds__hlds_goal.
:- import_module hlds__hlds_pred, hlds__instmap, check_hlds__mode_util.
:- import_module parse_tree__prog_data, parse_tree__prog_out.
:- import_module aditi_backend__rl_relops, aditi_backend__rl_info.
:- import_module libs__tree, check_hlds__type_util.
:- import_module transform_hlds__dependency_graph.
:- import_module check_hlds__inst_match, (parse_tree__inst), hlds__goal_util.
:- import_module transform_hlds__inlining, libs__globals, libs__options.

:- import_module assoc_list, bool, char, int, list, map, queue.
:- import_module relation, require, set, std_util, string, term, varset.

rl_gen__module(ModuleInfo0, RLProcs) -->
	{ module_info_ensure_aditi_dependency_info(ModuleInfo0, ModuleInfo) },
	{ module_info_aditi_dependency_ordering(ModuleInfo, SubModules) },
	rl_gen__scc_lists(SubModules, ModuleInfo, 0, [], RLProcs).
	
%-----------------------------------------------------------------------------%

:- pred rl_gen__scc_lists(aditi_dependency_ordering::in, module_info::in,
		int::in, list(rl_proc)::in, list(rl_proc)::out, 
		io__state::di, io__state::uo) is det.

rl_gen__scc_lists([], _, _, Procs, Procs, IO, IO).
rl_gen__scc_lists([aditi_scc(SubModule, EntryPoints) | SubModules], 
		ModuleInfo, RLProcId0, Procs0, Procs, IO0, IO) :-
	rl_info_init(ModuleInfo, IO0, RLInfo0),
	rl_gen__scc_list(SubModule, EntryPoints, RLProcId0,
		Procs0, Procs1, RLInfo0, RLInfo),
	rl_info_get_io_state(IO1, RLInfo, _),
	RLProcId is RLProcId0 + 1,
	rl_gen__scc_lists(SubModules, ModuleInfo, RLProcId, 
		Procs1, Procs, IO1, IO).

:- pred rl_gen__scc_list(dependency_ordering::in, list(pred_proc_id)::in,
		int::in, list(rl_proc)::in, list(rl_proc)::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__scc_list(SubModule, EntryPoints, RLProcId, Procs0, Procs) -->
	rl_info_get_module_info(ModuleInfo),
	(
		{
			% Dead code.
			EntryPoints = []
		;
			% Are all the procedures marked generate_inline?
			\+ (
				list__member(Entry, EntryPoints),
				Entry = proc(PredId, _),
				module_info_pred_info(ModuleInfo,
					PredId, PredInfo),
				pred_info_get_markers(PredInfo, Markers),
				\+ pred_info_is_imported(PredInfo),
				\+ check_marker(Markers, generate_inline)
			)
		}
	->
		{ Procs = Procs0 }
	;
		rl_info_write_message("Generating args\n", []),
		rl_gen__scc_list_args(EntryPoints, 
			InputArgs, OutputArgs, InputMap),
				
		rl_gen__proc_name(EntryPoints, RLProcId, ProcName),
		
		( { EntryPoints = [_] } ->
			{ Procs1 = Procs0 }
		;
			% Generate procedures to call each entry-point
			% of this procedure, throwing away outputs
			% that aren't required.
			rl_gen__scc_list_entry_procs(EntryPoints, ProcName,
				InputArgs, OutputArgs, Procs0, Procs1)
		),

		{ list__condense(SubModule, CondensedSubModule) },
		rl_info_set_scc_list(CondensedSubModule),

		rl_gen__sccs(SubModule, InputMap, empty, SubModuleCode),

		% Find out which relations are memoed.
		{ set__init(MemoedRels0) },
		rl_gen__memoed_rels(CondensedSubModule, 
			MemoedRels0, MemoedRels),

		{ tree__flatten(SubModuleCode, SubModuleCodeLists) },
		{ list__condense(SubModuleCodeLists, SubModuleInstrs) },

		rl_info_get_relation_info(RelationInfo),
		{ SubModuleProc = rl_proc(ProcName, InputArgs, OutputArgs,
			MemoedRels, RelationInfo, SubModuleInstrs, 
			EntryPoints) },
		{ Procs = [SubModuleProc | Procs1] }
	).

%-----------------------------------------------------------------------------%

	% Generate a unique procedure name for an SCC.
:- pred rl_gen__proc_name(list(pred_proc_id)::in, int::in, rl_proc_name::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__proc_name(EntryPoints, RLProcId, ProcName) -->
	( { EntryPoints = [EntryPoint] } ->
		% Give a better name for the commonly occurring case 
		% of an RL procedure with a single entry point.
		rl_gen__get_single_entry_proc_name(EntryPoint, ProcName)
	; { EntryPoints = [proc(EntryPredId, _) | _] } ->
		rl_info_get_module_info(ModuleInfo),
		{ module_info_pred_info(ModuleInfo,
			EntryPredId, EntryPredInfo) },
		{ pred_info_get_aditi_owner(EntryPredInfo, Owner) },
		{ module_info_name(ModuleInfo, ModuleName0) },
		{ prog_out__sym_name_to_string(ModuleName0, ModuleName) },
		{ string__int_to_string(RLProcId, ProcStr) },
		{ string__append("rl_proc_", ProcStr, Name) },
		{ list__length(EntryPoints, NumEntries) },
		{ ProcArity is NumEntries * 2 },
		{ ProcName = rl_proc_name(Owner, ModuleName, Name, ProcArity) }
	;
		{ error("rl_gen__proc_name: module with no entry-points") }
	).

	% Get the name for an RL procedure with a single entry point.
:- pred rl_gen__get_single_entry_proc_name(pred_proc_id::in, rl_proc_name::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__get_single_entry_proc_name(PredProcId, ProcName) -->
	rl_info_get_module_info(ModuleInfo),
	{ rl__get_entry_proc_name(ModuleInfo, PredProcId, ProcName) }.

%-----------------------------------------------------------------------------%

	% Get the input and output relations of an RL procedure.
	% Keep this in sync with rl_gen__lower_scc_call.
	% For a given call to a procedure, only one of the input
	% relations should contain any tuples. 
:- pred rl_gen__scc_list_args(list(pred_proc_id)::in, list(relation_id)::out,
		list(relation_id)::out, map(int, relation_id)::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__scc_list_args([], _, _, _) --> 
	{ error("rl_gen__scc_list_args") }.
rl_gen__scc_list_args([EntryPoint | EntryPoints], 
		InputRels, OutputRels, InputMap) -->
	rl_info_get_module_info(ModuleInfo),
	{ EntryPoint = proc(PredId, ProcId) },
	{ module_info_pred_proc_info(ModuleInfo, PredId, 
		ProcId, PredInfo, ProcInfo) },
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	{ pred_info_arg_types(PredInfo, ArgTypes) },
	{ map__init(InputMap0) },

		% The input arguments are the same for each 
		% procedure in the SCC.
	rl_gen__scc_list_input_args(EntryPoint, 1, ArgModes, 
		ArgTypes, [], InputRels, InputMap0, InputMap),
	rl_gen__scc_list_output_args([EntryPoint | EntryPoints], OutputRels),
	{ list__append(InputRels, OutputRels, AllArgs) },
	rl_info_set_scc_list_args(AllArgs).

	% This assumes that magic sets ensures that all the Mercury
	% procedures for this RL procedure have the magic input for
	% each entry point in the same argument position, i.e. that
	% corresponding input arguments for each entry point have the
	% same value. 
:- pred rl_gen__scc_list_input_args(pred_proc_id::in, int::in,
	list(mode)::in, list(type)::in, list(relation_id)::in, 
	list(relation_id)::out, map(int, relation_id)::in,
	map(int, relation_id)::out, rl_info::rl_info_di, 
	rl_info::rl_info_uo) is det.

rl_gen__scc_list_input_args(EntryPoint, ArgNo, ArgModes, ArgTypes,
		RevInputRels0, InputRels, InputMap0, InputMap) -->
	(
		{ ArgModes = [Mode | Modes] },
		{ ArgTypes = [Type | Types] }
	->
		rl_info_get_module_info(ModuleInfo),
		( { mode_is_input(ModuleInfo, Mode) } ->
			(
				{ type_is_higher_order(Type, predicate,
					(aditi_bottom_up), PredArgTypes) } 
			->
				rl_info_get_new_temporary(schema(PredArgTypes),
					InputRel),
				{ map__det_insert(InputMap0, ArgNo,
					InputRel, InputMap1) } 
			;
				% All non-higher order input arguments should
				% have been transformed away by magic.m.
				{
				EntryPoint = proc(PredId, _ProcId),
				module_info_pred_info(ModuleInfo, 
					PredId, PredInfo),
				pred_info_name(PredInfo, PredName),
				string__format("%s%s %s %i", 
					[s("rl_gen__scc_list_input_args - "),
					s("non higher-order input argument "),
					s(PredName), i(ArgNo)], Msg),
				error(Msg)
				}
			),
			{ RevInputRels1 = [InputRel | RevInputRels0] }
		;
			{ RevInputRels1 = RevInputRels0 },
			{ InputMap1 = InputMap0 }
		),
		{ NextArgNo is ArgNo + 1 },
		rl_gen__scc_list_input_args(EntryPoint, NextArgNo, Modes, 
			Types, RevInputRels1, InputRels, InputMap1, InputMap)
	;
		{ ArgModes = [] },
		{ ArgTypes = [] }
	->
		{ list__reverse(RevInputRels0, InputRels) },
		{ InputMap = InputMap0 }
	;
		{ error("rl_gen__scc_list_input_args") }
	).

:- pred rl_gen__scc_list_output_args(list(pred_proc_id)::in, 
		list(relation_id)::out, rl_info::rl_info_di, 
		rl_info::rl_info_uo) is det.

rl_gen__scc_list_output_args([], []) --> [].
rl_gen__scc_list_output_args([EntryPoint | EntryPoints], [Rel | Rels1]) -->
	rl_info_lookup_relation(full - EntryPoint, Rel),
	rl_gen__scc_list_output_args(EntryPoints, Rels1).

%-----------------------------------------------------------------------------%

:- pred rl_gen__sccs(dependency_ordering::in, map(int, relation_id)::in,
		rl_tree::in, rl_tree::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__sccs([], _, Code, Code) --> [].
rl_gen__sccs([SCC | SCCs], InputMap, Code0, Code) -->
	rl_gen__scc(SCC, SCCs, InputMap, SCCCode),
	rl_gen__sccs(SCCs, InputMap, tree(Code0, SCCCode), Code).

	% The generated code for each SCC is:
	%
	% 	non-recursive code;
	%    toplabel:
	% 	if (difference relations all empty) goto bottomlabel;
	% 	recursive code;
	% 	goto toplabel;
	%    bottomlabel:
	%
:- pred rl_gen__scc(list(pred_proc_id)::in, dependency_ordering::in,
		map(int, relation_id)::in, rl_tree::out, 
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__scc(SCC0, SCCs, InputMap, SCCCode) -->
	rl_info_get_module_info(ModuleInfo),

	% Make sure predicates with `generate_inline' markers (used to
	% create input relations for calls) do not have code generated for
	% them, and are not considered to be entry points to the SCC.
	{ list__filter(lambda([PredProcId::in] is semidet, (
		PredProcId = proc(PredId, _),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_get_markers(PredInfo, Markers),
		\+ check_marker(Markers, generate_inline)
	)), SCC0, SCC) },
 
	rl_info_set_scc(SCC),
	{ dependency_graph__get_scc_entry_points(SCC, SCCs, 
		ModuleInfo, EntryPoints) },
	rl_info_set_scc_entry_points(EntryPoints),
	( { SCCs = [] } ->
		rl_info_set_is_highest_scc(yes)
	;
		rl_info_set_is_highest_scc(no)
	),

	{ rl_gen__order_scc(ModuleInfo, SCC, EntryPoints, 
		OrderedSCC, DelayedDiffs) },
	rl_info_set_delayed_diffs(DelayedDiffs),

	rl_info_write_message("Generating SCC code\n", []),
	rl_gen__scc_2(InputMap, OrderedSCC, NonRecRLCode, RecRLCode),

	rl_gen__scc_comment(OrderedSCC, Comment),
	( { tree__tree_of_lists_is_empty(RecRLCode) } ->
		{ RecLoop = empty }
	;
		rl_info_write_message("Generating fixpoint check\n", []),
		rl_info_get_next_label_id(TopLabel),
		rl_info_get_next_label_id(BottomLabel),
		rl_gen__fixpoint_check(SCC, BottomLabel, FixpointCheck),
		{ set__to_sorted_list(DelayedDiffs, DelayedDiffList) },
		rl_gen__update_delayed_diffs(DelayedDiffList, DelayedDiffCode),

		{ RecLoop = 
			tree(node([label(TopLabel) - "recursive loop"]),
			tree(node(FixpointCheck),
			tree(RecRLCode,
			tree(node(DelayedDiffCode),
			node([
				goto(TopLabel) - "",
				label(BottomLabel) - "end of recursive loop"
			])
		)))) }
	),

	{ SCCCode = 
		tree(node([comment - Comment]), 
		tree(NonRecRLCode, 
		RecLoop
	)) }.

%-----------------------------------------------------------------------------%

:- pred rl_gen__scc_2(map(int, relation_id)::in, list(pred_proc_id)::in,
		rl_tree::out, rl_tree::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__scc_2(_InputMap, [], empty, empty) --> [].
rl_gen__scc_2(InputMap, [PredProcId | PredProcIds], 
		NonRecRLCode, RecRLCode) -->
	{ PredProcId = proc(PredId, ProcId) },
	rl_info_get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, _) },
	{ pred_info_get_markers(PredInfo, Markers) },
	( { check_marker(Markers, generate_inline) } ->
		rl_gen__scc_2(InputMap, PredProcIds, NonRecRLCode, RecRLCode)
	;	
		rl_gen__proc(InputMap, PredProcId, NonRecRLCode0, RecRLCode0),
		rl_gen__scc_2(InputMap, PredProcIds, 
			NonRecRLCode1, RecRLCode1),
		{ NonRecRLCode = tree(NonRecRLCode0, NonRecRLCode1) },
		{ RecRLCode = tree(RecRLCode0, RecRLCode1) }
	).

%-----------------------------------------------------------------------------%

	% Identify the elements of an SCC.
:- pred rl_gen__scc_comment(list(pred_proc_id)::in, string::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__scc_comment(SubSCC0, Comment) -->
	{ list__reverse(SubSCC0, SubSCC) },
	add_pred_name_and_arity(SubSCC, "", NameList),
	{ string__append("Code for SCC: ", NameList, Comment) }.

:- pred add_pred_name_and_arity(list(pred_proc_id)::in, string::in, string::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

add_pred_name_and_arity([], S, S) --> [].
add_pred_name_and_arity([proc(PredId, _) | PredProcIds], S0, S) -->
	rl_info_get_module_info(ModuleInfo),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_arity(PredInfo, PredArity) },
	{ string__int_to_string(PredArity, PredArityStr) },
	{ string__append_list([PredName, "/", PredArityStr, " ", S0], S1) },
	add_pred_name_and_arity(PredProcIds, S1, S).

%-----------------------------------------------------------------------------%

	% Work out which of the relations in the procedure are memoed.
:- pred rl_gen__memoed_rels(list(pred_proc_id)::in, set(relation_id)::in,
		set(relation_id)::out, rl_info::rl_info_di, 
		rl_info::rl_info_uo) is det.
		
rl_gen__memoed_rels([], MemoedRels, MemoedRels) --> [].
rl_gen__memoed_rels([PredProcId | PredProcIds], MemoedRels0, MemoedRels) --> 
	rl_info_get_module_info(ModuleInfo),
	{ PredProcId = proc(PredId, _) },
	( { hlds_pred__is_aditi_memoed(ModuleInfo, PredId) } ->
		rl_info_lookup_relation(full - PredProcId, MemoedRel),
		{ set__insert(MemoedRels0, MemoedRel, MemoedRels1) }
	;
		{ MemoedRels1 = MemoedRels0 }
	),
	rl_gen__memoed_rels(PredProcIds, MemoedRels1, MemoedRels).

%-----------------------------------------------------------------------------%

	% Create an RL procedure for each entry point which just calls
	% the RL procedure for the scc-list of which it is a member.
:- pred rl_gen__scc_list_entry_procs(list(pred_proc_id)::in,
		rl_proc_name::in, list(relation_id)::in, list(relation_id)::in,
		list(rl_proc)::in, list(rl_proc)::out, 
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__scc_list_entry_procs([], _, _, _, Procs, Procs) --> [].
rl_gen__scc_list_entry_procs([EntryPoint | EntryPoints], SubModuleProc, 
		InputArgs, OutputArgs, Procs0, Procs) -->
	rl_gen__get_single_entry_proc_name(EntryPoint, ProcLabel),
	rl_info_lookup_relation(full - EntryPoint, Output),

	{ set__init(SavedRels) },
	{ list__map(lambda([Rel::in, OutputRel::out] is det, (
			OutputRel = output_rel(Rel, [])
		)), OutputArgs, OutputRels) },
	{ Instr = call(SubModuleProc, InputArgs, OutputRels, SavedRels) - "" },
	rl_info_get_relation_info(RelInfo),
	{ set__init(MemoedRels) },
	{ Proc = rl_proc(ProcLabel, InputArgs, [Output], MemoedRels,
		RelInfo, [Instr], [EntryPoint]) },
	rl_gen__scc_list_entry_procs(EntryPoints, SubModuleProc, 
		InputArgs, OutputArgs, [Proc | Procs0], Procs).

%-----------------------------------------------------------------------------%

	% Check whether anything changed, do another pass if anything did.
:- pred rl_gen__fixpoint_check(list(pred_proc_id)::in, label_id::in,
		list(rl_instruction)::out, rl_info::rl_info_di,
		rl_info::rl_info_uo) is det.

rl_gen__fixpoint_check(SCC, ExitLabel, Code) -->
	rl_gen__get_diffs(SCC, [], Diffs),
	( { rl_gen__test_relations(Diffs, GotoCond) } ->
		{ Code = [conditional_goto(GotoCond, ExitLabel) - ""] }
	;
		{ Code = [] }
	).

	% Generate a goto condition which evaluates to true if all the 
	% given relations are empty.
:- pred rl_gen__test_relations(list(relation_id)::in,
		goto_cond::out) is semidet.

rl_gen__test_relations([RelId | RelIds], Cond) :-
	(
		RelIds = [_ | _], 
		rl_gen__test_relations(RelIds, Cond0),
		Cond = and(empty(RelId), Cond0)
	;
		RelIds = [],
		Cond = empty(RelId)
	).

%-----------------------------------------------------------------------------%

	% Get the delta relations for each of the given procedures.
:- pred rl_gen__get_diffs(list(pred_proc_id)::in, list(relation_id)::in,
		list(relation_id)::out, rl_info::rl_info_di,
		rl_info::rl_info_uo) is det.

rl_gen__get_diffs([], Diffs, Diffs) --> [].
rl_gen__get_diffs([PredProcId | PredProcIds], Diffs0, Diffs) -->
	rl_info_lookup_relation(diff - PredProcId, Diff),
	rl_gen__get_diffs(PredProcIds, [Diff | Diffs0], Diffs).

%-----------------------------------------------------------------------------%

	% Permute the SCC to some near-optimal ordering. The aim is to minimise 
	% the breaking of cycles within the SCC. With multiple entry points 
	% it may not be obvious what the best ordering is. 
	%
	% When using memoing, care must be taken to order the SCC in such
	% a way that all predicates which use a difference relation created by
	% the exit rules use it before it is clobbered by the new differences
	% for the recursive rules. 
	% There are two cases to consider:
	% - if there is only one predicate in the SCC with exit rules,
	% it can go last, and there is no problem.
	% - if there are multiple predicates in the SCC with exit rules,
	% things get a bit more tricky. In some cases the old and new 
	% differences must both be kept, with the new differences 
	% overwriting the old at the end of the iteration.

:- pred rl_gen__order_scc(module_info::in, list(pred_proc_id)::in, 
		list(pred_proc_id)::in, list(pred_proc_id)::out, 
		set(pred_proc_id)::out) is det.

rl_gen__order_scc(ModuleInfo, SCC, _EntryPoints, OrderedSCC, DelayedDiffs) :-
	( SCC = [_] ->
		% Optimize for a common case.
		OrderedSCC = SCC,
		set__init(DelayedDiffs)
	;
		set__list_to_set(SCC, SCCSet),
		list__filter(rl_gen__proc_has_exit_rule(ModuleInfo, SCCSet), 
			SCC, ExitProcs),
		list__delete_elems(SCC, ExitProcs, NonExitProcs),
		rl_gen__do_order_scc(ModuleInfo, NonExitProcs, 
			ExitProcs, OrderedSCC, DelayedDiffs),
		( 
			list__length(SCC, SCCLength),
			list__length(OrderedSCC, SCCLength),
			set__list_to_set(OrderedSCC, SCCSet)
		->
			true
		;
			error("rl_gen__order_scc: error in ordering")
		)
	).

:- pred rl_gen__proc_has_exit_rule(module_info::in, set(pred_proc_id)::in,
		pred_proc_id::in) is semidet.
		
rl_gen__proc_has_exit_rule(ModuleInfo, SCC, PredProcId) :-
	module_info_pred_proc_info(ModuleInfo, PredProcId, _, ProcInfo),
	proc_info_goal(ProcInfo, Goal),
	goal_to_disj_list(Goal, DisjList),
	list__member(Disjunct, DisjList),
	\+ (
		goal_calls(Disjunct, CalledPredProcId),
		set__member(CalledPredProcId, SCC)
	).

:- pred rl_gen__do_order_scc(module_info::in, list(pred_proc_id)::in,
		list(pred_proc_id)::in, list(pred_proc_id)::out, 
		set(pred_proc_id)::out) is det.

rl_gen__do_order_scc(ModuleInfo, NonExitProcs, ExitProcs,
		Ordering, DelayedDiffs) :-
	module_info_dependency_info(ModuleInfo, DepInfo),
	hlds_dependency_info_get_dependency_graph(DepInfo, DepGraph),
	map__init(ProcDeps0),
	list__append(NonExitProcs, ExitProcs, AllProcs),
	list__map(relation__lookup_element(DepGraph), 
		AllProcs, ProcKeys0),
	set__list_to_set(ProcKeys0, ProcKeys),
	list__foldl(rl_gen__find_proc_dependencies(DepGraph, ProcKeys),
		AllProcs, ProcDeps0, ProcDeps),

	% Order the procedures without exit rules 
	% before all those with exit rules.
	map__init(ExitProcDeps0),
	set__list_to_set(ExitProcs, ExitProcSet),
	rl_gen__find_ordering(NonExitProcs, ExitProcSet, 
		ProcDeps, ExitProcDeps0, [], NonExitOrdering),

	% Order the procedures with exit rules.
	list__map(relation__lookup_element(DepGraph), 
		ExitProcs, ExitProcKeys0),
	set__list_to_set(ExitProcKeys0, ExitProcKeys),
	list__foldl(rl_gen__exit_proc_dependencies(DepGraph, ExitProcKeys),
		ExitProcs, ExitProcDeps0, ExitProcDeps),
	set__insert_list(ExitProcSet, NonExitProcs, EvaluatedProcs2),
	rl_gen__find_ordering(ExitProcs, EvaluatedProcs2, 
		ProcDeps, ExitProcDeps, [], ExitOrdering),

	list__append(NonExitOrdering, ExitOrdering, Ordering),

	set__init(DelayedDiffs0),
	rl_gen__find_delayed_diff_procs(ExitOrdering, ExitProcSet, 
		ExitProcDeps, DelayedDiffs0, DelayedDiffs).

:- pred rl_gen__find_ordering(list(pred_proc_id)::in, set(pred_proc_id)::in,
		map(pred_proc_id, set(pred_proc_id))::in,
		map(pred_proc_id, set(pred_proc_id))::in,
		list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

rl_gen__find_ordering([], _, _, _, RevOrder, Order) :-
	list__reverse(RevOrder, Order).
rl_gen__find_ordering(ProcsToOrder0, EvaluatedProcs0, ProcDeps,
		ExitProcDeps, RevOrder0, RevOrder) :-
	ProcsToOrder0 = [Proc | Rest],
	( 
		Rest = [],
		list__reverse([Proc | RevOrder0], RevOrder)
	;
		Rest = [_ | _],
		rl_gen__select_next_proc(ProcsToOrder0, EvaluatedProcs0, 
			ProcDeps, ExitProcDeps, no, SelectedProc),
		list__delete_all(ProcsToOrder0, SelectedProc, ProcsToOrder),
		set__insert(EvaluatedProcs0, SelectedProc, EvaluatedProcs),
		rl_gen__find_ordering(ProcsToOrder, EvaluatedProcs, ProcDeps, 
			ExitProcDeps, [SelectedProc | RevOrder0], RevOrder)
	).

	% Pick out the procedure with the least number 
	% of unevaluated dependencies.
:- pred rl_gen__select_next_proc(list(pred_proc_id)::in, set(pred_proc_id)::in,
		map(pred_proc_id, set(pred_proc_id))::in,
		map(pred_proc_id, set(pred_proc_id))::in, 
		maybe(pair(pred_proc_id, int))::in,
		pred_proc_id::out) is det.

rl_gen__select_next_proc([], _, _, _, BestSoFar, SelectedProc) :-
	( BestSoFar = yes(SelectedProc0 - _) ->
		SelectedProc = SelectedProc0
	;
		error("rl_gen__select_next_proc - no procs to select from")
	).
rl_gen__select_next_proc([Proc | ProcsToOrder], EvaluatedProcs, ProcDeps, 
		ExitProcDeps, BestSoFar0, SelectedProc) :-
	map__lookup(ProcDeps, Proc, Dependencies),
	set__insert(EvaluatedProcs, Proc, EvaluatedProcs1),
	set__difference(Dependencies, EvaluatedProcs1, Difference),
	set__to_sorted_list(Difference, DifferenceList),
	list__length(DifferenceList, NumUnevaluated),
	( NumUnevaluated = 0 ->
		SelectedProc = Proc
	;
		(
			BestSoFar0 = no,
			BestSoFar = yes(Proc - NumUnevaluated)
		;
			BestSoFar0 = yes(_ - NumUnevaluated0),
			( NumUnevaluated < NumUnevaluated0 ->
				BestSoFar = yes(Proc - NumUnevaluated)
			; NumUnevaluated = NumUnevaluated0 ->
				% Prefer a procedure that isn't depended on 
				% by any other procedures with exit rules which
				% haven't already been scheduled.
				% If a procedure is depended on by other
				% procedures with exit rules, the old 
				% differences must be kept until after the 
				% rules for those procedures are executed.
				(
					map__search(ExitProcDeps, Proc, Deps0),
					set__difference(Deps0, 
						EvaluatedProcs, Deps),
					set__empty(Deps)
				->	
					BestSoFar = yes(Proc - NumUnevaluated)
				;
					BestSoFar = BestSoFar0
				)
			;
				BestSoFar = BestSoFar0
			)
		),
		rl_gen__select_next_proc(ProcsToOrder, EvaluatedProcs,
			ProcDeps, ExitProcDeps, BestSoFar, SelectedProc)
	).
		
	% The recursive rules for procedures with exit rules must come 
	% after the procedures which use the differences created by the
	% exit rules. This finds all the exit procedures which depend on a 
	% given exit procedure.
:- pred rl_gen__exit_proc_dependencies(dependency_graph::in, 
		set(relation_key)::in, pred_proc_id::in, 
		map(pred_proc_id, set(pred_proc_id))::in,
		map(pred_proc_id, set(pred_proc_id))::out) is det.

rl_gen__exit_proc_dependencies(DepGraph, ExitProcKeys, 
		ExitProc, Deps0, Deps) :-
	relation__lookup_element(DepGraph, ExitProc, ExitKey),
	relation__lookup_to(DepGraph, ExitKey, AllDependentKeys),
	set__intersect(AllDependentKeys, ExitProcKeys, DependentKeys0),
	set__to_sorted_list(DependentKeys0, DependentKeys),
	list__map(relation__lookup_key(DepGraph), 
		DependentKeys, DependentProcs0),
	set__list_to_set(DependentProcs0, DependentProcs),
	map__det_insert(Deps0, ExitProc, DependentProcs, Deps).

	% Find the procedures in the set to be ordered which
	% a given procedure depends on.
:- pred rl_gen__find_proc_dependencies(dependency_graph::in, 
		set(relation_key)::in, pred_proc_id::in, 
		map(pred_proc_id, set(pred_proc_id))::in,
		map(pred_proc_id, set(pred_proc_id))::out) is det.

rl_gen__find_proc_dependencies(DepGraph, ProcsToOrderKeys, Proc, 
		Deps0, Deps) :-
	relation__lookup_element(DepGraph, Proc, ProcKey),
	relation__lookup_from(DepGraph, ProcKey, AllDependencyKeys),
	set__intersect(AllDependencyKeys, ProcsToOrderKeys, 
		DependencyKeys0),
	set__to_sorted_list(DependencyKeys0, DependencyKeys),
	list__map(relation__lookup_key(DepGraph), 
		DependencyKeys, DependencyProcs0),
	set__list_to_set(DependencyProcs0, DependencyProcs),
	map__det_insert(Deps0, Proc, DependencyProcs, Deps).

:- pred rl_gen__find_delayed_diff_procs(list(pred_proc_id)::in, 
	set(pred_proc_id)::in, map(pred_proc_id, set(pred_proc_id))::in,
	set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

rl_gen__find_delayed_diff_procs([], _, _, DelayedDiffs, DelayedDiffs).
rl_gen__find_delayed_diff_procs([Proc | Ordering], LaterProcs0, ExitProcDeps, 
		DelayedDiffs0, DelayedDiffs) :-
	map__lookup(ExitProcDeps, Proc, Deps),
	set__delete(LaterProcs0, Proc, LaterProcs),
	set__intersect(Deps, LaterProcs, Intersection),
		% If a later procedure uses the difference 
		% relation, the difference relation must be kept
		% until the end of the iteration.
	( set__empty(Intersection) ->
		DelayedDiffs1 = DelayedDiffs0
	;
		set__insert(DelayedDiffs0, Proc, DelayedDiffs1)
	),
	rl_gen__find_delayed_diff_procs(Ordering, LaterProcs, 
		ExitProcDeps, DelayedDiffs1, DelayedDiffs).

%-----------------------------------------------------------------------------%
	
	% At the end of the iteration, copy any "delayed"
	% differences into the difference relations.
:- pred rl_gen__update_delayed_diffs(list(pred_proc_id)::in, 
		list(rl_instruction)::out, rl_info::rl_info_di, 
		rl_info::rl_info_uo) is det.

rl_gen__update_delayed_diffs([], []) --> [].
rl_gen__update_delayed_diffs([Proc | Procs], [Copy | Copies]) -->
	rl_info_lookup_relation(new_diff - Proc, NewDiff),
	rl_info_lookup_relation(diff - Proc, Diff),
	{ Copy = ref(Diff, NewDiff) - "" },
	rl_gen__update_delayed_diffs(Procs, Copies).
	
%-----------------------------------------------------------------------------%

:- pred rl_gen__proc(map(int, relation_id)::in, pred_proc_id::in, rl_tree::out,
	rl_tree::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__proc(InputMap, PredProcId, NonRecRLCode, RecRLCode) -->
	rl_info_get_module_info(ModuleInfo),
	{ PredProcId = proc(PredId, ProcId) },
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_arity(PredInfo, PredArity) },
	rl_info_write_message("Generating RL for `%s'/%i\n", 
		[s(PredName), i(PredArity)]),
	{ proc_info_headvars(ProcInfo, HeadVars) },
	rl_info_partition_call_args(PredProcId, HeadVars, InputArgs, _),
	rl_gen__proc_input_args(InputArgs, 1, InputMap),
	rl_info_set_pred_proc_id(PredProcId),
	rl_info_set_pred_info(PredInfo),
	rl_info_set_proc_info(ProcInfo),
	{ proc_info_goal(ProcInfo, Goal) },
	{ goal_to_disj_list(Goal, Disjuncts) },
	rl_info_write_message("Generating rules\n", []),
	rl_gen__rules(Disjuncts, 1, NonRecRL, RecRL,
		NonRecRels, RecRels),
	( { RecRels = [] } ->
		{ PredIsRecursive = no }
	;
		{ PredIsRecursive = yes }
	),
	rl_gen__union_rules(PredProcId, PredIsRecursive, no, 
		NonRecRL, NonRecRels, NonRecRLCode),
	rl_gen__union_rules(PredProcId, PredIsRecursive, yes,
		RecRL, RecRels, RecRLCode).

	% Compute the union of all the given rules, then compute the
	% differences and add the new tuples into the relation holding
	% the current procedure,.
:- pred rl_gen__union_rules(pred_proc_id::in, bool::in, bool::in,
		rl_tree::in, list(relation_id)::in, rl_tree::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__union_rules(PredProcId, PredIsRecursive, RuleIsRecursive,
		RuleCode, RelsToUnion, Code) -->
	rl_info_get_current_proc_output_schema(Schema),
	( { RelsToUnion = [] } ->
		{ Code = empty }
	;
		rl_relops__union(yes, Schema, RelsToUnion, no,
			UnionRel, UnionCode),
		rl_gen__union_diff(PredProcId, Schema, PredIsRecursive, 
			RuleIsRecursive, UnionRel, UnionDiffCode),
		{ Code =
			tree(RuleCode, 
			tree(UnionCode,
			UnionDiffCode
		)) }
	).

%-----------------------------------------------------------------------------%

	% Set up the input relations for a procedure.
:- pred rl_gen__proc_input_args(list(prog_var)::in, int::in, 
		map(int, relation_id)::in, rl_info::rl_info_di,
		rl_info::rl_info_uo) is det.

rl_gen__proc_input_args([], _, _) --> [].
rl_gen__proc_input_args([Arg | Args], ArgNo, InputMap) -->
	{ map__lookup(InputMap, ArgNo, InputRel) },
	rl_info_bind_var_to_relation(Arg, InputRel),
	{ NextArgNo is ArgNo + 1 },
	rl_gen__proc_input_args(Args, NextArgNo, InputMap).

%-----------------------------------------------------------------------------%

	% Generate all the disjuncts for a procedure.
:- pred rl_gen__rules(list(hlds_goal)::in, int::in, rl_tree::out, rl_tree::out, 
		list(relation_id)::out, list(relation_id)::out, 
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__rules([], _, empty, empty, [], []) --> [].
rl_gen__rules([Rule | Rules], RuleNo, NonRecRLCode, 
		RecRLCode, NonRecRels, RecRels) -->
	{ goal_to_conj_list(Rule, RuleList) },
	rl_info_write_message("Generating rule\n", []),
	rl_info_set_rule_number(RuleNo),
	rl_info_get_var_rels(VarRels0),
	rl_info_get_var_status_map(VarStat0),
	rl_gen__classify_rule(RuleList, ClassifiedRule),
	rl_info_get_current_proc_output_vars(RuleOutputs),
	rl_info_get_current_proc_output_schema(RuleSchema),
	rl_gen__do_gen_rule(ClassifiedRule, RuleOutputs, RuleSchema,
		RuleCode, RuleType, Rel),
	rl_info_set_var_rels(VarRels0),
	rl_info_set_var_stats(VarStat0),
	{ NextRule is RuleNo + 1 },
	rl_gen__rules(Rules, NextRule, NonRecRLCode1, RecRLCode1,
		NonRecRels1, RecRels1),
	( 
		{
		RuleType = recursive,
		NonRecRLCode = NonRecRLCode1,
		RecRLCode = tree(RuleCode, RecRLCode1),
		RecRels = [Rel | RecRels1],
		NonRecRels = NonRecRels1
		},
		rl_info_write_message("recursive\n", [])
	;
		{
		RuleType = non_recursive,
		NonRecRLCode = tree(RuleCode, NonRecRLCode1),
		RecRLCode = RecRLCode1,
		NonRecRels = [Rel | NonRecRels1],
		RecRels = RecRels1
		},
		rl_info_write_message("non-recursive\n", [])
	).

%-----------------------------------------------------------------------------%

	% Information about database calls.

:- type db_call
	--->	db_call(
			db_call_id,
			maybe(list(hlds_goal)),	% is the call negated, if
					% so, the list(hlds_goal) is the
					% other goals under the negation
					% which will become the subtract
					% condition.
			list(prog_var),	% magic input
			list(prog_var),	% variables corresponding to the
					% output argument positions
			hlds_goal_info
		).

:- type db_call_id
	--->	called_pred(pred_proc_id)
	;	ho_called_var(prog_var)
	.	

:- type classified_rule
	--->	one_call(db_call, list(hlds_goal))
	;	two_calls(db_call, db_call, list(hlds_goal)).

:- type rule_type
	--->	non_recursive
	;	recursive.	

%-----------------------------------------------------------------------------%

	% Work out whether a rule has zero, one or two database calls.
:- pred rl_gen__classify_rule(list(hlds_goal)::in, classified_rule::out, 
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__classify_rule([], _) -->
	{ error("rl_gen__classify_rule: empty rule") }.
rl_gen__classify_rule([Goal | Goals], Rule) -->
	rl_info_get_module_info(ModuleInfo),
	(
		{ rl_gen__goal_is_aditi_call(ModuleInfo, Goal, CallGoal1,
			MaybeNegGoals) } 
	->
		rl_gen__collect_call_info(CallGoal1, MaybeNegGoals, Call1),
		( 
			{ rl_gen__find_aditi_call(ModuleInfo, Goals, 
				[], BetweenGoals, CallGoal2, MaybeNegGoals2,
				JoinCond) }
		->
			rl_gen__collect_call_info(CallGoal2, 
				MaybeNegGoals2, Call2),
			rl_gen__setup_var_rels(BetweenGoals),
			{ Rule = two_calls(Call1, Call2, JoinCond) }
		;
			{ Rule = one_call(Call1, Goals) }
		)
	;
		% Look for a rule which just sets up some input and
		% calls another procedure.
		{ rl_gen__find_aditi_call(ModuleInfo, [Goal | Goals],
			[], BetweenGoals, CallGoal, no, []) }
	->
		rl_gen__setup_var_rels(BetweenGoals),
		rl_gen__collect_call_info(CallGoal, no, Call),
		{ Rule = one_call(Call, []) }
	;
		{ error("rl_gen__classify_rule: invalid rule") }
	).

:- pred rl_gen__goal_is_aditi_call(module_info::in, hlds_goal::in,
		hlds_goal::out, maybe(list(hlds_goal))::out) is semidet.

rl_gen__goal_is_aditi_call(ModuleInfo, Goal, CallGoal, MaybeNegGoals) :-
	(
		Goal = call(PredId, _, _, _, _, _) - _,
		rl_gen__call_is_aditi_call(ModuleInfo, PredId),
		CallGoal = Goal,
		MaybeNegGoals = no
	; 
		% XXX check that the var is an input relation variable.
		Goal = generic_call(higher_order(_, predicate, _),
			_, _, _) - _,
		CallGoal = Goal,
		MaybeNegGoals = no
	;
		Goal = not(NegGoal) - _,
			% magic.m will strip any explicit somes away
			% from the negated goal.
		goal_to_conj_list(NegGoal, [CallGoal | OtherGoals]),
		CallGoal = call(PredId, _, _, _, _, _) - _,
		rl_gen__call_is_aditi_call(ModuleInfo, PredId),
		MaybeNegGoals = yes(OtherGoals)
	).

:- pred rl_gen__call_is_aditi_call(module_info::in, pred_id::in) is semidet.

rl_gen__call_is_aditi_call(ModuleInfo, PredId) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	( hlds_pred__pred_info_is_aditi_relation(PredInfo)
	; hlds_pred__pred_info_is_aditi_aggregate(PredInfo)
	).

:- pred rl_gen__collect_call_info(hlds_goal::in, maybe(list(hlds_goal))::in, 
		db_call::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__collect_call_info(CallGoal, MaybeNegGoals, DBCall) -->
	(
		{ CallGoal = call(PredId, ProcId, Args, _, _, _) - GoalInfo }
	->
		{ PredProcId = proc(PredId, ProcId) },
		rl_info_get_module_info(ModuleInfo),
		( { hlds_pred__is_base_relation(ModuleInfo, PredId) } ->
			{ InputArgs = [] },
			{ OutputArgs = Args }
		;
			rl_info_partition_call_args(PredProcId, Args, 
				InputArgs, OutputArgs)
		),
		{ DBCall = db_call(called_pred(PredProcId), MaybeNegGoals, 
				InputArgs, OutputArgs, GoalInfo) }
	;
		{ CallGoal = generic_call(higher_order(Var, predicate, _),
			Args, ArgModes, _) - GoalInfo }
	->
		{ CallId = ho_called_var(Var) },
		rl_info_get_module_info(ModuleInfo),
		{ partition_args(ModuleInfo, ArgModes, Args,
			InputArgs, OutputArgs) },
		{ DBCall = db_call(CallId, MaybeNegGoals, InputArgs,
			OutputArgs, GoalInfo) }
	;
		{ error("rl_gen__collect_call_info") }
	).

%-----------------------------------------------------------------------------%

	% Find input closure constructions and a database call
	% from a list of goals.
:- pred rl_gen__find_aditi_call(module_info::in, list(hlds_goal)::in,
		list(hlds_goal)::in, list(hlds_goal)::out, hlds_goal::out,
		maybe(list(hlds_goal))::out, list(hlds_goal)::out) is semidet.

rl_gen__find_aditi_call(ModuleInfo, [Goal | Goals], RevBetweenGoals0, 
		BetweenGoals, CallGoal, MaybeNegGoals, JoinCond) :-
	( 
		rl_gen__goal_is_aditi_call(ModuleInfo, Goal, 
			CallGoal0, MaybeNegGoals0) 
	->

		CallGoal = CallGoal0,
		MaybeNegGoals = MaybeNegGoals0,
		JoinCond = Goals,
		list__reverse(RevBetweenGoals0, BetweenGoals)
	;
		% Only closure constructions can come 
		% between two Aditi calls.
		Goal = unify(_, _, _, Uni, _) - _,
		Uni = construct(_, ConsId, _, _, _, _, _),
		ConsId = pred_const(_, _, _)
	->
		rl_gen__find_aditi_call(ModuleInfo, Goals,
			[Goal | RevBetweenGoals0], BetweenGoals, 
			CallGoal, MaybeNegGoals, JoinCond)
	;
		fail
	).

%-----------------------------------------------------------------------------%

	% Tell the rl_info about the input relation arguments to
	% the second database call.
:- pred rl_gen__setup_var_rels(list(hlds_goal)::in, 
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__setup_var_rels([]) --> [].
rl_gen__setup_var_rels([BetweenGoal | BetweenGoals]) -->
	( 
		{ BetweenGoal = unify(_, _, _, Uni, _) - _ },
		{ Uni = construct(Var, ConsId, CurriedArgs, _, _, _, _) },
		{ ConsId = pred_const(PredId, ProcId, _EvalMethod) }
	->
		{ Closure = closure_pred(CurriedArgs, 
			proc(PredId, ProcId)) },
		rl_info_set_var_status(Var, Closure),
		rl_gen__setup_var_rels(BetweenGoals)
	;
		{ error("rl_gen__setup_var_rels") }
	).

%-----------------------------------------------------------------------------%

:- pred rl_gen__do_gen_rule(classified_rule::in, list(prog_var)::in,
		relation_schema::in, rl_tree::out, rule_type::out, 
		relation_id::out, rl_info::rl_info_di, 
		rl_info::rl_info_uo) is det.

rl_gen__do_gen_rule(one_call(CallInfo, Goals), RuleOutputs, 
		_RuleSchema, Code, RuleType, Result) -->
	rl_gen__maybe_generate_lower_scc_call(CallInfo, CallCode, 
		IsRec, FullRel, MaybeDiffRel),
	rl_gen__single_call_rule(CallInfo, FullRel, MaybeDiffRel, Goals, 
		RuleOutputs, RuleCode, Result),
	{ Code = tree(CallCode, RuleCode) },

	{ 
		IsRec = yes,
		RuleType = recursive
	;
		IsRec = no,
		RuleType = non_recursive
	}.

rl_gen__do_gen_rule(two_calls(CallInfo1, CallInfo2, Goals), RuleOutputs,
		RuleSchema, Code, RuleType, Result) -->
	rl_gen__maybe_generate_lower_scc_call(CallInfo1, CallCode1, 
		IsRec1, OutputRel1, MaybeDiffRel1),
	rl_gen__maybe_generate_lower_scc_call(CallInfo2, CallCode2, 
		IsRec2, OutputRel2, MaybeDiffRel2),
	(
		{ MaybeDiffRel1 = yes(DiffRel1) },
		{ MaybeDiffRel2 = yes(DiffRel2) },
		rl_gen__diff_diff_rule(CallInfo1, OutputRel1, DiffRel1,
			CallInfo2, OutputRel2, DiffRel2, Goals, 
			RuleOutputs, RuleSchema, RuleCode, Result)
	;
		{ MaybeDiffRel1 = yes(DiffRel1) },
		{ MaybeDiffRel2 = no },
		rl_gen__diff_non_diff_rule(CallInfo1, OutputRel1, DiffRel1,
			CallInfo2, OutputRel2, Goals, 
			RuleOutputs, RuleSchema, RuleCode, Result)
	;
		{ MaybeDiffRel1 = no },
		{ MaybeDiffRel2 = yes(DiffRel2) },
		rl_gen__non_diff_diff_rule(CallInfo1, OutputRel1,
			CallInfo2, OutputRel2, DiffRel2, Goals, 
			RuleOutputs, RuleSchema, RuleCode, Result)
	;	
		{ MaybeDiffRel1 = no },
		{ MaybeDiffRel2 = no },
		rl_gen__non_diff_non_diff_rule(CallInfo1, OutputRel1,
			CallInfo2, OutputRel2, Goals, 
			RuleOutputs, RuleSchema, RuleCode, Result)
	),
	( { bool__or(IsRec1, IsRec2, yes) } ->
		{ RuleType = recursive }
	;
		{ RuleType = non_recursive }
	),
	{ Code = tree(CallCode1, tree(CallCode2, RuleCode)) }.
	
	% If the called database procedure is in a lower SCC, generate
	% a call to it, otherwise just return the full and difference
	% relations for the procedure.
:- pred rl_gen__maybe_generate_lower_scc_call(db_call::in, rl_tree::out, 
	bool::out, relation_id::out, maybe(relation_id)::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__maybe_generate_lower_scc_call(CallInfo, CallCode, IsRec, 
		FullRel, MaybeDiffRel) -->
	{ CallInfo = db_call(CallId, _, InputArgs, _Outputs, _GoalInfo) },
	rl_info_get_scc_list(SubModule),
	( 
		{ CallId = called_pred(PredProcId) },
		{ list__member(PredProcId, SubModule) }
	->
		rl_info_lookup_relation(full - PredProcId, FullRel),
		rl_gen__get_call_diff_rel(PredProcId, MaybeDiffRel),
		rl_info_get_scc(SCC),
		{ list__member(PredProcId, SCC) ->
			IsRec = yes 
		;
			IsRec = no
		},
		{ CallCode = empty }
	;
		rl_gen__lower_scc_call(CallId, InputArgs, 
			FullRel, CallCode),
		{ MaybeDiffRel = no },
		{ IsRec = no }
	).

	% Find out which relation, if any, stores the difference relation
	% for a called predicate.
:- pred rl_gen__get_call_diff_rel(pred_proc_id::in, maybe(relation_id)::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__get_call_diff_rel(PredProcId, MaybeDiffRel) -->
	rl_info_get_scc_list(SubModule),
	rl_info_get_module_info(ModuleInfo),
	rl_info_get_scc(SCC),
	rl_info_get_pred_proc_id(proc(CurrPredId, _)),
	{ PredProcId = proc(PredId, _) },
	( { hlds_pred__is_differential(ModuleInfo, PredId) } ->
		( { list__member(PredProcId, SCC) } ->
			% The called predicate is in this SCC - 
			% the differences are in the diff relation.
			rl_info_lookup_relation(diff - PredProcId, DiffRel),
			{ MaybeDiffRel = yes(DiffRel) }
		; 
			{ list__member(PredProcId, SubModule) },
			{ hlds_pred__is_aditi_memoed(ModuleInfo, PredId) },
			{ hlds_pred__is_aditi_memoed(ModuleInfo, CurrPredId) }
		->
			% The called predicate is in a lower SCC - 
			% the differences will have been left in 
			% the acc_diff relation, which is only
			% created if the predicate is memoed.
			% If the current predicate is not memoed,
			% accumulated differences for called procedures
			% may not be used, since they would only
			% contain part of the information required%
			% to build up the full relation.
			rl_info_lookup_relation(acc_diff - PredProcId, 
				DiffRel),
			{ MaybeDiffRel = yes(DiffRel) }
		;
			% The called procedure will not be generated within the
			% current RL procedure so differences can't be used.
			{ MaybeDiffRel = no }
		)
	;
		{ MaybeDiffRel = no }
	).

%-----------------------------------------------------------------------------%

:- pred rl_gen__single_call_rule(db_call::in, relation_id::in, 
		maybe(relation_id)::in, list(hlds_goal)::in,
		list(prog_var)::in, rl_tree::out, relation_id::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__single_call_rule(DBCallInfo, FullRel, MaybeDiffRel, Goals, RuleOutputs, 
		Code, RuleResult) -->
	{ DBCallInfo = db_call(_PredProcId, IsNeg,
		_InputArgs, CallOutputs, GoalInfo) },

		% A negated goal must have another call providing 
		% input to subtract from.
	{ require(lambda([] is semidet, IsNeg = no), 
		"rl_gen__single_call_rule: negated supp or magic call") },

	( { MaybeDiffRel = yes(DiffRel) } ->
		{ CallOutput = DiffRel }
	;
		{ CallOutput = FullRel }
	),
	{ instmap__init_reachable(InstMap0) },
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap) },

	rl_relops__select_and_project(CallOutput, RuleResult, CallOutputs,
		RuleOutputs, InstMap, Goals, Code).

%-----------------------------------------------------------------------------%

	% Handle the different cases for rules with two calls.

:- pred rl_gen__diff_diff_rule(db_call::in, relation_id::in, relation_id::in, 
	db_call::in, relation_id::in, relation_id::in, list(hlds_goal)::in,
	list(prog_var)::in, relation_schema::in, rl_tree::out,
	relation_id::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__diff_diff_rule(CallInfo1, Full1, Diff1, CallInfo2, Full2, Diff2, 
		Goals, RuleOutputs, RuleSchema, Code, RuleResult) -->
	{ CallInfo1 = db_call(_PredProcId1, IsNeg1,
		_InputArgs1, OutputArgs1, _GoalInfo1) },
	{ CallInfo2 = db_call(_PredProcId2, IsNeg2,
		_InputArgs2, OutputArgs2, _GoalInfo2) },

	{ require(lambda([] is semidet, (IsNeg1 = no, IsNeg2 = no)),
		"rl_gen__diff_diff_rule: negated differential call") },

	{ rl_gen__get_call_instmap(CallInfo1, CallInfo2, InstMap) },

	rl_relops__diff_diff_join(Diff1, Full1, Diff2, Full2, OutputArgs1,
		OutputArgs2, InstMap, Goals, RuleOutputs,
		RuleSchema, RuleResult, Code).

%-----------------------------------------------------------------------------%

:- pred rl_gen__diff_non_diff_rule(db_call::in, relation_id::in,
	relation_id::in, db_call::in, relation_id::in, list(hlds_goal)::in,
	list(prog_var)::in, relation_schema::in, rl_tree::out, 
	relation_id::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__diff_non_diff_rule(CallInfo1, _Full1, Diff1, CallInfo2, Full2,
		Goals, RuleOutputs, RuleSchema, Code, RuleResult) -->
	{ CallInfo1 = db_call(_PredProcId1, IsNeg1,
		_InputArgs1, OutputArgs1, _GoalInfo1) },
	{ CallInfo2 = db_call(_PredProcId2, IsNeg2,
		_InputArgs2, OutputArgs2, _GoalInfo2) },

	{ require(lambda([] is semidet, (IsNeg1 = no)),
		"rl_gen__rec_non_rec_rule: negated recursive call") },

	{ rl_gen__get_call_instmap(CallInfo1, CallInfo2, InstMap) },

	(
		{ IsNeg2 = no },
		rl_relops__join(Diff1, Full2, OutputArgs1, OutputArgs2,
			InstMap, Goals, RuleOutputs, RuleSchema, RuleResult,
			Code)
	; 
		{ IsNeg2 = yes(NegGoals) },
		rl_relops__subtract(Diff1, Full2, OutputArgs1, OutputArgs2,
			InstMap, NegGoals, Goals, RuleOutputs, RuleSchema,
			RuleResult, Code)
	).

%-----------------------------------------------------------------------------%

:- pred rl_gen__non_diff_diff_rule(db_call::in, relation_id::in, db_call::in,
	relation_id::in, relation_id::in, list(hlds_goal)::in,
	list(prog_var)::in, relation_schema::in, rl_tree::out, 
	relation_id::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__non_diff_diff_rule(CallInfo1, Full1, CallInfo2, _Full2, Diff2,
		Goals, RuleOutputs, RuleSchema, Code, RuleResult) -->
	{ CallInfo1 = db_call(_PredProcId1, IsNeg1,
		_InputArgs1, OutputArgs1, _GoalInfo1) },
	{ CallInfo2 = db_call(_PredProcId2, IsNeg2,
		_InputArgs2, OutputArgs2, _GoalInfo2) },

	{ require(lambda([] is semidet, (IsNeg1 = no, IsNeg2 = no)),
		"rl_gen__non_rec_rec_rule: negated recursive or magic call") },


	{ rl_gen__get_call_instmap(CallInfo1, CallInfo2, InstMap) },

	rl_relops__join(Full1, Diff2, OutputArgs1, OutputArgs2, InstMap,
		Goals, RuleOutputs, RuleSchema, RuleResult, Code).

%-----------------------------------------------------------------------------%

:- pred rl_gen__non_diff_non_diff_rule(db_call::in, relation_id::in,
	db_call::in, relation_id::in, list(hlds_goal)::in,
	list(prog_var)::in, relation_schema::in, rl_tree::out,
	relation_id::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__non_diff_non_diff_rule(CallInfo1, Full1, CallInfo2, Full2, Goals, 
		RuleOutputs, RuleSchema, Code, RuleResult) -->
	{ CallInfo1 = db_call(_PredProcId1, IsNeg1,
		_InputArgs1, OutputArgs1, _GoalInfo1) },
	{ CallInfo2 = db_call(_PredProcId2, IsNeg2,
		_InputArgs2, OutputArgs2, _GoalInfo2) },

	{ require(lambda([] is semidet, (IsNeg1 = no)),
		"rl_gen__non_rec_non_rec_rule: negated magic call") },

	{ rl_gen__get_call_instmap(CallInfo1, CallInfo2, InstMap) },

	(
		{ IsNeg2 = no },
		rl_relops__join(Full1, Full2, OutputArgs1, OutputArgs2,
			InstMap, Goals, RuleOutputs, RuleSchema,
			RuleResult, Code)
	; 
		{ IsNeg2 = yes(NegGoals) },
		rl_relops__subtract(Full1, Full2, OutputArgs1, OutputArgs2,
			InstMap, NegGoals, Goals, RuleOutputs,
			RuleSchema, RuleResult, Code)
	).

%-----------------------------------------------------------------------------%

	% Extract some information from the calls for a two call rule. 
:- pred rl_gen__get_call_instmap(db_call::in, db_call::in,
		instmap::out) is det.

rl_gen__get_call_instmap(db_call(_,_,_,_, GoalInfo1), 
		db_call(_,_,_,_, GoalInfo2), InstMap) :-
	goal_info_get_instmap_delta(GoalInfo1, InstMapDelta1),
	goal_info_get_instmap_delta(GoalInfo2, InstMapDelta2),
	instmap__init_reachable(InstMap0),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta1, InstMap1),
	instmap__apply_instmap_delta(InstMap1, InstMapDelta2, InstMap).

%-----------------------------------------------------------------------------%

:- pred rl_gen__union_diff(pred_proc_id::in, relation_schema::in,
		bool::in, bool::in, relation_id::in, rl_tree::out, 
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__union_diff(PredProcId, Schema, PredIsRecursive, 
		RuleIsRecursive, NewRel, Code) -->
	rl_info_get_is_highest_scc(IsHighest),

	rl_info_lookup_relation(full - PredProcId, FullRel),

	rl_gen__union_diff_rels(PredProcId, PredIsRecursive, RuleIsRecursive,
		IsHighest, Differences, FullIsEmpty),

	( 
		{ Differences = none },
		rl_info_write_message("Differences = none\n", []),
		( { FullIsEmpty = yes } ->
			{ Code = node([ref(FullRel, NewRel) - ""]) }
		;
			% Non-recursive, non-memoed predicate.
			rl_relops__union(no, Schema, [FullRel, NewRel], 
				yes(FullRel), _, Code)
		)
	;
		{ Differences = diff(DiffRel) },
		rl_info_write_message("Differences = diff\n", []),
		( { FullIsEmpty = yes } ->
			{ Code = node([
				ref(DiffRel, NewRel) - "",
				ref(FullRel, NewRel) - ""
			]) }
		;
			rl_relops__difference(FullRel,
				NewRel, DiffRel, DiffCode),
			rl_relops__union(no, Schema, [FullRel, DiffRel], 
				yes(FullRel), _, UnionCode),
			{ Code = tree(DiffCode, UnionCode) }
		)
	;
		% If we're computing acc_diff relations, the predicate,
		% must be memoed, so we can't assume the full relation
		% is empty. 
		{ Differences = acc_diff(AccDiffRel) },
		rl_info_write_message("Differences = acc_diff\n", []),
		rl_relops__difference(FullRel, NewRel,
			AccDiffRel, DiffCode),
		rl_relops__union(no, Schema, [FullRel, AccDiffRel], 
			yes(FullRel), _, UnionCode),
		{ Code = tree(DiffCode, UnionCode) }
	;
		{ Differences = diff_and_acc_diff(DiffRel, AccDiffRel) },
		rl_info_write_message("Differences = diff_and_acc_diff\n", []),
		rl_relops__difference(FullRel, NewRel,
			DiffRel, DiffCode),
		rl_relops__union(no, Schema, [FullRel, AccDiffRel], 
			yes(FullRel), _, UnionCode1),
		rl_relops__union(no, Schema, [AccDiffRel, DiffRel],
			yes(AccDiffRel), _, UnionCode2),
		{ Code = 
			tree(DiffCode, 
			tree(UnionCode1,
			UnionCode2
		)) }
	).

%-----------------------------------------------------------------------------%

	% The difference relations to be returned by a predicate.
	% See rl_info.m for an explanation of the types of difference
	% relations.
:- type differences
	--->	none
	;	diff(relation_id)
	;	acc_diff(relation_id)
	;	diff_and_acc_diff(relation_id, relation_id)
	.

	% Work out whether difference relations should be computed.
	% For recursive procedures, as well as the difference relation
	% to determine when a fixpoint has been reached, we can also
	% maintain an accumulated difference relation for memoed procedures
	% to hold the differences since this procedure was last evaluated.
	% For non-recursive predicates, the differences are always
	% put in the acc_diff relation to simplify calls.
	%
	% FullIsEmpty is true if the full relation for the predicate
	% is guaranteed to be empty before the union_diff.
:- pred rl_gen__union_diff_rels(pred_proc_id::in, bool::in, 
		bool::in, bool::in, differences::out, bool::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__union_diff_rels(PredProcId, PredIsRecursive, RuleIsRecursive, 
		IsHighest, Differences, FullIsEmpty) -->
	rl_info_get_module_info(ModuleInfo),
	{ PredProcId = proc(PredId, _) },
	{ hlds_pred__is_differential(ModuleInfo, PredId) ->
		IsDiff = yes
	;
		IsDiff = no
	},
	{ hlds_pred__is_aditi_memoed(ModuleInfo, PredId) ->
		IsMemoed = yes
	;
		IsMemoed = no
	},
	( 	
		{ PredIsRecursive = no },
			% For non-recursive predicates we only need
			% the acc_diff rel, since there will be no
			% iterations. The accumulated differences 
			% can't be used if the predicate is in the 
			% highest SCC. There's no point computing 
			% accumulated differences if the relation 
			% isn't memoed, since they will be the same 
			% as the full relation.
		( { IsDiff = yes, IsMemoed = yes, IsHighest = no } ->
			rl_info_lookup_relation(acc_diff - PredProcId,
				AccDiffRel),
			{ Differences = acc_diff(AccDiffRel) }
		;
			{ Differences = none }
		),
		{ bool__not(IsMemoed, FullIsEmpty) }
	; 
		{ PredIsRecursive = yes },
		(
			{ IsDiff = yes },
			rl_gen__get_diff_relation(PredProcId,
				RuleIsRecursive, DiffRel),
			( { IsDiff = yes, IsMemoed = yes, IsHighest = no } ->
				rl_info_lookup_relation(acc_diff - PredProcId,
					AccDiffRel),
				{ Differences = diff_and_acc_diff(DiffRel, 
							AccDiffRel) }
			;
				{ Differences = diff(DiffRel) } 
			)
		;
			{ IsDiff = no },
				% Need to compute the differences to
				% find out when to stop iterating.
			rl_info_lookup_relation(diff - PredProcId, DiffRel),
			{ Differences = diff(DiffRel) }
		),
		{
			( RuleIsRecursive = yes
			; IsMemoed = yes
			)
		->
			FullIsEmpty = no
		;
			FullIsEmpty = yes
		}
	).

	% If updating the differences is to be delayed until the end
	% of the iteration, put differences into the new_diff relation,
	% otherwise put them straight into the diff relation.
	% See the comments on rl_gen__order_scc.
:- pred rl_gen__get_diff_relation(pred_proc_id::in, bool::in, relation_id::out, 
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__get_diff_relation(PredProcId, RuleIsRecursive, DiffRel) -->
	rl_info_get_delayed_diffs(Delayed),
	( { RuleIsRecursive = yes, set__member(PredProcId, Delayed) } ->
		rl_info_lookup_relation(new_diff - PredProcId, DiffRel)
	;
		rl_info_lookup_relation(diff - PredProcId, DiffRel)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Setup the input relations and generate a call to a predicate
	% in another RL procedure.
:- pred rl_gen__lower_scc_call(db_call_id::in, list(prog_var)::in, 
		relation_id::out, rl_tree::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__lower_scc_call(called_pred(CalledProc),
		InputArgs, OutputRelation, Code) -->
	{ CalledProc = proc(PredId, _) },
	rl_info_get_module_info(ModuleInfo),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_module(PredInfo, Module0) },
	{ prog_out__sym_name_to_string(Module0, Module) },
	{ pred_info_name(PredInfo, Name) },
	{ pred_info_arity(PredInfo, Arity) },
	rl_info_write_message("Generating call to %s:%s/%i\n",
		[s(Module), s(Name), i(Arity)]),
	( 
		{ hlds_pred__is_aditi_aggregate(ModuleInfo, PredId) },
		{ InputArgs = [InputRelationArg, UpdateAcc, ComputeAcc] }
	->
		rl_gen__aggregate(InputRelationArg, UpdateAcc, 
			ComputeAcc, OutputRelation, Code),
		rl_info_write_message("Finished generating aggregate\n", [])
	;
		rl_gen__get_single_entry_proc_name(CalledProc, ProcName),
		rl_info_lookup_relation(full - CalledProc, OutputRelation),
		rl_info_get_relation_info(RelInfo),
		{ map__lookup(RelInfo, OutputRelation, OutputRelInfo) },
		{ OutputRelInfo = relation_info(OutputRelType, _, _, _) },
		( { OutputRelType = permanent(_) } ->
			{ Code = empty }
		;
			{ OutputRels = [output_rel(OutputRelation, [])] },
			rl_gen__lower_scc_call_inputs(InputArgs, 
				InputRels, InputCode),
			rl_info__comment(Comment),

			{ set__init(SavedRels) },
			{ CallInstr = call(ProcName, InputRels,
					OutputRels, SavedRels) - Comment },
			{ Code = 
				tree(InputCode, 
				node([CallInstr])
			) }
		)
	).
rl_gen__lower_scc_call(ho_called_var(Var),
		InputArgs, OutputRelation, Code) -->
	rl_info_get_var_status(Var, VarStat),
	( { VarStat = input_closure, InputArgs = [] } ->
		rl_info_lookup_var_relation(Var, OutputRelation),
		{ Code = empty }
	;
		{ error(
	"rl_gen__lower_scc_call: ho-called var not an input relation") }
	).

%-----------------------------------------------------------------------------%

:- pred rl_gen__lower_scc_call_inputs(list(prog_var)::in, 
		list(relation_id)::out, rl_tree::out, 
		rl_info::rl_info_di, rl_info::rl_info_uo) is det. 

rl_gen__lower_scc_call_inputs([], [], empty) --> [].
rl_gen__lower_scc_call_inputs([InputArg | InputArgs], 
		[Rel | Rels], Code) -->
	rl_gen__lower_scc_call_input(InputArg, Rel, Code1),
	rl_gen__lower_scc_call_inputs(InputArgs, Rels, Code2),
	{ Code = tree(Code1, Code2) }.

:- pred rl_gen__lower_scc_call_input(prog_var::in, relation_id::out,
		rl_tree::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det. 

rl_gen__lower_scc_call_input(InputArg, InputRel, Code) -->
	rl_info_get_var_status(InputArg, Status),
	( 
		{ Status = closure_pred(CurriedArgs, PredProcId) },
		rl_info_get_module_info(ModuleInfo),
		{ module_info_pred_proc_info(ModuleInfo, PredProcId, 
			CallPredInfo, CallProcInfo) },
		{ pred_info_get_markers(CallPredInfo, Markers) },

		( { check_marker(Markers, generate_inline) } ->
			rl_gen__inline_call(PredProcId, CallPredInfo,
				CallProcInfo, CurriedArgs, InputArg,
				InputRel, Code)
		;
			rl_info_get_scc_list(SubModule),
			( { list__member(PredProcId, SubModule) } ->
				rl_gen__get_call_diff_rel(PredProcId,
					MaybeDiffRel),
				( { MaybeDiffRel = yes(InputRel0) } ->
					{ InputRel = InputRel0 }
				;
					rl_info_lookup_relation(
						full - PredProcId, InputRel)
				),
				{ Code = empty }
			;
				rl_gen__lower_scc_call(
					called_pred(PredProcId),
					CurriedArgs, InputRel, Code)
			)
		)
	;
		{ Status = input_closure },
		rl_info_lookup_var_relation(InputArg, InputRel),
		{ Code = empty }
	;	
		{ Status = normal },
		% All input arguments should have been
		% transformed away by magic sets.
		{ term__var_to_int(InputArg, InputArgNo) },

		{ string__format(
		"rl_gen__lower_scc_call_input: non-closure input argument %i",
			[i(InputArgNo)], Msg) },
		{ error(Msg) }
	).

%-----------------------------------------------------------------------------%

	% Generate input relations inline to ensure that the most
	% up-to-date version of the relation being projected is used.
:- pred rl_gen__inline_call(pred_proc_id::in, pred_info::in, proc_info::in,
	list(prog_var)::in, prog_var::in, relation_id::out, rl_tree::out, 
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__inline_call(_PredProcId, CalledPredInfo, CalledProcInfo, CurriedArgs,
		RelVar, OutputRel, Code) -->

	% Generate some dummy output arguments.
	rl_info_get_proc_info(ProcInfo0),
	{ pred_info_arg_types(CalledPredInfo, Types) },
	{ list__length(CurriedArgs, NumCurriedArgs) },
	{ list__drop(NumCurriedArgs, Types, OutputArgTypes1) ->
		OutputArgTypes = OutputArgTypes1,
		proc_info_create_vars_from_types(ProcInfo0,
			OutputArgTypes, OutputArgs, ProcInfo)
	;
		error("rl_gen__lower_scc_call_input: list__drop failed")
	},
	rl_info_set_proc_info(ProcInfo),
	{ list__append(CurriedArgs, OutputArgs, Args) },
	rl_gen__rename_inline_call(Args, CalledPredInfo, CalledProcInfo, Goal),	
	( { Goal = conj(_) - _ } ->
		{ require(lambda([] is semidet, OutputArgs = []),
		    "rl_gen__inline_call: `true' relation not zero arity") },
		% create a zero arity relation containing a tuple
		{ true_goal(True) },
		{ instmap__init_reachable(InstMap) },
		rl_relops__goal(InstMap, no_inputs,
			yes([]), [True], TupleGoal),
		rl_info_get_new_temporary(schema([]), OutputRel0),
		rl_info_get_new_temporary(schema([]), OutputRel),
		{ Code = node([
			init(output_rel(OutputRel0, [])) - "", 
			insert_tuple(output_rel(OutputRel, []),
				OutputRel0, TupleGoal) - ""
		]) }
	; { Goal = disj(_) - _ } ->
		% create an empty relation 
		rl_info_get_new_temporary(schema(OutputArgTypes), OutputRel),
		{ Code = node([init(output_rel(OutputRel, [])) - ""]) }
	;		
		{ goal_to_conj_list(Goal, GoalList) },
		rl_gen__classify_rule(GoalList, ClassifiedRule),
		( { ClassifiedRule = one_call(_, _) } ->
			rl_gen__do_gen_rule(ClassifiedRule, OutputArgs,
				schema(OutputArgTypes), Code, _, OutputRel)
		;
			{ error(
		"rl_gen__inline_call: closure should contain one call") }
		)
	),
	rl_info_bind_var_to_relation(RelVar, OutputRel).

	% Rename a called goal to use the current procedure's varset.
	% The called goals should be very small (1 call), so this
	% won't be very expensive.
:- pred rl_gen__rename_inline_call(list(prog_var)::in, pred_info::in,
	proc_info::in, hlds_goal::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__rename_inline_call(Args, CalledPredInfo, CalledProcInfo, Goal) -->
	rl_info_get_pred_info(PredInfo0),
	rl_info_get_proc_info(ProcInfo0),
	{ pred_info_get_univ_quant_tvars(PredInfo0, UnivQTVars) },
	{ pred_info_typevarset(PredInfo0, TypeVarSet0) },
	{ proc_info_varset(ProcInfo0, VarSet0) },
	{ proc_info_vartypes(ProcInfo0, VarTypes0) },
	{ proc_info_typeinfo_varmap(ProcInfo0, TVarMap0) },
	{ inlining__do_inline_call(UnivQTVars, Args, CalledPredInfo,
		CalledProcInfo, VarSet0, VarSet, VarTypes0, VarTypes,
		TypeVarSet0, TypeVarSet, TVarMap0, TVarMap, Goal) },
	{ proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1) },
	{ proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2) },
	{ proc_info_set_typeinfo_varmap(ProcInfo2, TVarMap, ProcInfo) },
	{ pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo) },
	rl_info_set_pred_info(PredInfo),
	rl_info_set_proc_info(ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred rl_gen__aggregate(prog_var::in, prog_var::in, prog_var::in,
		relation_id::out, rl_tree::out,
		rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_gen__aggregate(InputRelationArg, UpdateAcc, ComputeInitial,
		OutputRelation, Code) -->
	rl_info_get_var_type(ComputeInitial, ComputeInitialType),
	(
		% XXX The type declaration in extras/aditi/aditi.m
		% should be changed to require that the eval_method
		% for the UpdateAcc and ComputeInitial parameters
		% is `aditi_top_down', and the InputRelationArg
		% is `aditi_bottom_up'.
		{ type_is_higher_order(ComputeInitialType, 
			predicate, _, ComputeInitialArgTypes) },
		{ ComputeInitialArgTypes = [GrpByType, _NGrpByType, AccType] }
	->
		%
		% Compute and sort the query to be aggregated over.
		% The input query must be sorted on group-by then
		% non-group-by attributes.
		%
		rl_info_write_message("Generating aggregate query\n", []),
		rl_gen__lower_scc_call_input(InputRelationArg,
			InputRelation, AggRelCode),
		rl_relops__sort(InputRelation, SortedInput, SortCode),

		%
		% Generate an expression to compute the initial accumulator
		% the update it for each tuple.
		%
		rl_info_write_message("Generating aggregate expression\n", []),
		
		rl_info_get_var_status(ComputeInitial, ComputeStatus),
		{ ComputeStatus = closure_pred([], ComputePredProcId0) ->
			ComputePredProcId = ComputePredProcId0
		;
		    	error(
			"rl_gen__aggregate: compute_initial closure not found")
		},

		rl_info_get_var_status(UpdateAcc, UpdateStatus),
		{ UpdateStatus = closure_pred([], UpdatePredProcId0) ->
			UpdatePredProcId = UpdatePredProcId0
		;
		    	error("rl_gen__aggregate: update closure not found")
		},

		rl_info_get_new_temporary(schema([GrpByType, AccType]),
			OutputRelation),
		rl_info__comment(Comment),

		{ Code = 
			tree(AggRelCode, 
			tree(SortCode, 
			node([aggregate(output_rel(OutputRelation, []),
				SortedInput, ComputePredProcId,
				UpdatePredProcId) - Comment])
		)) }
	;
		{ error("rl_gen__aggregate: invalid aggregate types") }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
