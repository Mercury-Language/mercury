%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_block_opt.
% Main author: stayl
%
% Optimizations local to a basic block.
% This builds a DAG representing the expressions computed by the block,
% applies some rewriting rules to the DAG, then generates the new
% instruction list from the DAG.
%
% Optimizations performed:
% - cse
% - copy elimination
% - optimize R3 = difference(R1, R2), R4 = union(R1, R3) into a union diff.
% - merge multiple projections of the same node into a single instruction.
% - merge projections of projections into a single instruction.
%
% To do -
% push selections through union, sort etc.
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_block_opt.

:- interface.

:- import_module aditi_backend__rl_block.

:- import_module io.
:- import_module list.

:- pred rl_block_opt(list(opt_flag)::in, rl_opt_info::in, rl_opt_info::out,
		io__state::di, io__state::uo) is det.

:- type opt_flag
	--->	add_uniondiff
	;	merge_output_projections
	;	add_pre_projections
	.

:- implementation.

:- import_module aditi_backend__rl.
:- import_module aditi_backend__rl_key.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__prog_data.
:- import_module transform_hlds__inlining.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module relation.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

rl_block_opt(Flags, Info0, Info, IO0, IO) :-
	rl_block_opt_2(Flags, IO0, IO, Info0, Info).

:- pred rl_block_opt_2(list(opt_flag)::in, io__state::di, io__state::uo,
		rl_opt_info::in, rl_opt_info::out) is det.

rl_block_opt_2(Flags, IO, IO) -->
	rl_opt_info_get_rev_block_order(RevOrder),
	{ list__reverse(RevOrder, Order) },

	list__foldl(rl_block_opt__block(Flags), Order).

:- pred rl_block_opt__block(list(opt_flag)::in, block_id::in,
		rl_opt_info::in, rl_opt_info::out) is det.

rl_block_opt__block(Flags, BlockId, Info0, Info) :-
	rl_opt_info_get_block(BlockId, Block0, Info0, Info1),
	Block0 = block(MaybeLabel, Instrs0, MaybeBranch, BlockInfo),

	dag_init(Info1, BlockInfo, Flags, Dag0),
	BlockInfo = block_info(LiveAtStart0, _),
	set__to_sorted_list(LiveAtStart0, LiveAtStart),
	list__foldl(rl_block_opt__setup_input_relation, LiveAtStart,
		Dag0, Dag1),
	list__foldl(rl_block_opt__build_dag, Instrs0, Dag1, Dag2),
	rl_block_opt__rewrite_dag(Dag2, Dag3),
	rl_block_opt__deconstruct_dag(Instrs, Dag3, Dag),
	dag_get_rl_opt_info(Info2, Dag, _),
	Block = block(MaybeLabel, Instrs, MaybeBranch, BlockInfo),
	rl_opt_info_set_block(BlockId, Block, Info2, Info).

:- pred rl_block_opt__setup_input_relation(relation_id::in,
		dag::in, dag::out) is det.

rl_block_opt__setup_input_relation(Relation) -->
	rl_block_opt__add_dag_node([output_rel(Relation, [])], [],
		input(Relation), _).

%-----------------------------------------------------------------------------%

	% Add an instruction to the DAG for the block.
:- pred rl_block_opt__build_dag(rl_instruction::in, dag::in, dag::out) is det.

rl_block_opt__build_dag(join(Output, Input1, Input2, Type, Exprn,
				SemiJoin, TrivialJoin) - _) -->
	rl_block_opt__lookup_relation(Input1, Input1Node),
	rl_block_opt__lookup_relation(Input2, Input2Node),
	rl_block_opt__add_dag_node([Output], [Input1Node, Input2Node],
		join(Input1Node, Input2Node, Type, Exprn,
			SemiJoin, TrivialJoin), _).

rl_block_opt__build_dag(subtract(Output, Input1, Input2, Type,
				Exprn, TrivialSubtract) - _) -->
	rl_block_opt__lookup_relation(Input1, Input1Node),
	rl_block_opt__lookup_relation(Input2, Input2Node),
	rl_block_opt__add_dag_node([Output], [Input1Node, Input2Node],
		subtract(Input1Node, Input2Node, Type, Exprn, TrivialSubtract),
		_).

rl_block_opt__build_dag(difference(Output, Input1, Input2, Type) - _) -->
	rl_block_opt__lookup_relation(Input1, Input1Node),
	rl_block_opt__lookup_relation(Input2, Input2Node),
	rl_block_opt__add_dag_node([Output], [Input1Node, Input2Node],
		difference(Input1Node, Input2Node, Type), _).

rl_block_opt__build_dag(project(Output, Input, Exprn,
		OtherOutputs, Type) - _) -->
	{ assoc_list__keys(OtherOutputs, MaterialisedOutputs) },
	{ assoc_list__values(OtherOutputs, Exprns) },
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__add_dag_node([Output | MaterialisedOutputs], [InputNode],
		project(InputNode, [Exprn | Exprns], Type), _).

rl_block_opt__build_dag(union(Output, Inputs, Type) - _) -->
	rl_block_opt__lookup_relations(Inputs, InputNodes0),

	% Optimize some common cases.
	{ list__sort_and_remove_dups(InputNodes0, InputNodes) },
	( { InputNodes = [] } ->
		rl_block_opt__build_dag(init(Output) - "")
	; { InputNodes = [InputNode] } ->
		rl_block_opt__set_output_rel_id(Output, InputNode)
	;
		rl_block_opt__add_dag_node([Output], InputNodes,
			union(InputNodes, Type), _)
	).

rl_block_opt__build_dag(insert(UoOutput, DiInput, Input, Type, _) - _) -->
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__lookup_relation(DiInput, DiInputNode),
	rl_block_opt__add_dag_node([output_rel(UoOutput, [])],
		[DiInputNode, InputNode],
		insert(DiInputNode, InputNode, Type), _).

rl_block_opt__build_dag(
		union_diff(UoOutput, DiInput, Input, Diff, Index, _) - _) -->
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__lookup_relation(DiInput, DiInputNode),
	rl_block_opt__add_dag_node([output_rel(UoOutput, []), Diff],
		[DiInputNode, InputNode],
		union_diff(DiInputNode, InputNode, Index), _).

rl_block_opt__build_dag(sort(Output, Input, Attrs) - _) -->
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__add_dag_node([Output], [InputNode],
		sort(InputNode, Attrs), _).

rl_block_opt__build_dag(ref(Output, Input) - _) -->
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__set_relation_output_id(Output, InputNode).

	% This may need to be reintroduced later by rl_liveness.m.
rl_block_opt__build_dag(copy(Output, Input) - _) -->
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__set_output_rel_id(Output, InputNode).

	% This may need to be reintroduced later by rl_liveness.m.
rl_block_opt__build_dag(make_unique(Output, Input) - _) -->
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__set_output_rel_id(Output, InputNode).

rl_block_opt__build_dag(init(OutputRel) - _) -->
	{ OutputRel = output_rel(Relation, _) },
	dag_get_relation_info(Relation, OutputInfo),
	{ OutputInfo = relation_info(_, Schema, _, _) },
	rl_block_opt__add_dag_node([OutputRel], [], init(Schema), _).

rl_block_opt__build_dag(insert_tuple(Output, Input, Exprn) - _) -->
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__add_dag_node([Output], [InputNode],
		insert_tuple(InputNode, Exprn), _).

rl_block_opt__build_dag(call(ProcName, Inputs, Outputs, _) - _) -->
	rl_block_opt__lookup_relations(Inputs, InputNodes),
	rl_block_opt__add_dag_node(Outputs, InputNodes,
		call(ProcName, InputNodes), _).

rl_block_opt__build_dag(aggregate(Output, Input, Initial, Update) - _) -->
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__add_dag_node([Output], [InputNode],
		aggregate(InputNode, Initial, Update), _).

rl_block_opt__build_dag(
		add_index(Output, Input) - _) -->
	rl_block_opt__lookup_relation(Input, InputNode),
	rl_block_opt__set_output_rel_id(Output, InputNode).

rl_block_opt__build_dag(clear(_) - _) -->
	{ error("rl_block_opt__build_dag: clear") }.

rl_block_opt__build_dag(unset(Output) - _) -->
	rl_block_opt__unset_relation_node(Output).

rl_block_opt__build_dag(label(_) - _) -->
	{ error("rl_block_opt__build_dag: label") }.
rl_block_opt__build_dag(conditional_goto(_, _) - _) -->
	{ error("rl_block_opt__build_dag: conditional_goto") }.
rl_block_opt__build_dag(goto(_) - _) -->
	{ error("rl_block_opt__build_dag: goto") }.
rl_block_opt__build_dag(comment - _) --> [].

%-----------------------------------------------------------------------------%

:- pred rl_block_opt__lookup_relation(relation_id::in, output_id::out,
		dag::in, dag::out) is det.

rl_block_opt__lookup_relation(RelationId, OutputId) -->
	dag_get_rel_node_map(RelNodeMap0),
	( { map__search(RelNodeMap0, RelationId, OutputId0) } ->
		{ OutputId = OutputId0 }
	;
		(
			dag_get_relation_info(RelationId,
				relation_info(permanent(_), _, Indexes, _))
		->
			rl_block_opt__add_dag_node(
				[output_rel(RelationId, Indexes)], [],
				input(RelationId), OutputId)
		;
			{ string__format(
		"rl_block_opt__lookup_relation: can't find relation %i",
				[i(RelationId)], Msg) },
			{ error(Msg) }
		)
	).

:- pred rl_block_opt__lookup_relations(list(relation_id)::in,
		list(output_id)::out, dag::in, dag::out) is det.

rl_block_opt__lookup_relations([], []) --> [].
rl_block_opt__lookup_relations([Input | Inputs], [Node | Nodes]) -->
	rl_block_opt__lookup_relation(Input, Node),
	rl_block_opt__lookup_relations(Inputs, Nodes).

%-----------------------------------------------------------------------------%

	% Set the location of a relation.
:- pred rl_block_opt__set_relation_output_id(relation_id::in, output_id::in,
		dag::in, dag::out) is det.

rl_block_opt__set_relation_output_id(Relation, OutputId) -->
	rl_block_opt__set_output_rel_id(output_rel(Relation, []), OutputId).

:- pred rl_block_opt__set_output_rel_id(output_rel::in, output_id::in,
		dag::in, dag::out) is det.

rl_block_opt__set_output_rel_id(output_rel(Relation, Indexes),
		OutputId) -->
	rl_block_opt__unset_relation_node(Relation),
	dag_get_node_info_map(NodeInfoMap0),
	dag_get_rel_node_map(RelNodeMap0),
	dag_get_output_loc_map(Locs),
	{ map__lookup(Locs, OutputId, input_node(Node, InputIndex)) },
	{ map__lookup(NodeInfoMap0, Node, NodeInfo0) },
	{ NodeInfo0 = node_info(Instr, Outputs0) },
	{ rl_block_opt__add_output(Outputs0, 0, InputIndex,
		Relation, Indexes, Outputs) },
	{ NodeInfo = node_info(Instr, Outputs) },
	{ map__set(NodeInfoMap0, Node, NodeInfo, NodeInfoMap) },
	{ map__set(RelNodeMap0, Relation, OutputId, RelNodeMap) },
	dag_set_node_info_map(NodeInfoMap),
	dag_set_rel_node_map(RelNodeMap).

:- pred rl_block_opt__add_output(list(output_node)::in, int::in, int::in,
	relation_id::in, list(index_spec)::in, list(output_node)::out) is det.

rl_block_opt__add_output([], _, _, _, _, _) :-
	error("rl_block_opt:rl_block_opt__add_output").
rl_block_opt__add_output([OutputRel0 | Outputs0], Index0, TheIndex,
		Relation, RelIndex, [OutputRel | Outputs]) :-
	( Index0 = TheIndex ->
		OutputRel0 = output_node(Schema, IndexSpec0, Rels0),
		list__append(IndexSpec0, RelIndex, IndexSpec),
		set__insert(Rels0, Relation, Rels),
		OutputRel = output_node(Schema, IndexSpec, Rels),
		Outputs = Outputs0
	;
		OutputRel = OutputRel0,
		Index = Index0 + 1,
		rl_block_opt__add_output(Outputs0, Index, TheIndex,
			Relation, RelIndex, Outputs)
	).

%-----------------------------------------------------------------------------%

	% Remove knowledge about a relation's location from the dag
	% when it is `unset'.
:- pred rl_block_opt__unset_relation_node(relation_id::in,
		dag::in, dag::out) is det.

rl_block_opt__unset_relation_node(RelationId) -->
	dag_get_rel_node_map(RelNodeMap0),
	( { map__search(RelNodeMap0, RelationId, OutputId) } ->
		dag_get_output_loc_map(Locs),
		{ map__lookup(Locs, OutputId, InputRel) },
		{ InputRel = input_node(Node, _) },
		dag_get_node_info_map(NodeInfoMap0),
		{ map__lookup(NodeInfoMap0, Node, NodeInfo0) },
		{ NodeInfo0 = node_info(Instr, Outputs0) },
		{ DeleteOutput = (pred(Output0::in, Output::out) is det :-
				Output0 = output_node(Schema, Index, Set0),
				set__delete(Set0, RelationId, Set),
				Output = output_node(Schema, Index, Set)
			) },
		{ list__map(DeleteOutput, Outputs0, Outputs) },
		{ NodeInfo = node_info(Instr, Outputs) },
		{ map__det_update(NodeInfoMap0, Node, NodeInfo, NodeInfoMap) },
		{ map__delete(RelNodeMap0, RelationId, RelNodeMap) },
		dag_set_rel_node_map(RelNodeMap),
		dag_set_node_info_map(NodeInfoMap)
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% Add an instruction to the dag.
:- pred rl_block_opt__add_dag_node(list(output_rel)::in, list(output_id)::in,
		instr::in, node_id::out, dag::in, dag::out) is det.

rl_block_opt__add_dag_node(OutputRels, InputNodes, Instr, MatchingNode) -->
	{ list__map(rl__output_rel_relation, OutputRels, Outputs) },
	list__foldl(rl_block_opt__unset_relation_node, Outputs),

		% Search for a matching node in the DAG.
	dag_get_exprns(Exprns),
	( { rl_block_opt__find_matching_node(Exprns, Instr, MatchingNode0) } ->
		{ MatchingNode = MatchingNode0 },
		rl_block_opt__add_node_outputs(MatchingNode, OutputRels)
	;
		rl_block_opt__do_add_node(OutputRels, Instr, MatchingNode)
	),

	rl_block_opt__set_relation_nodes(MatchingNode, Outputs),
	rl_block_opt__add_node_dependencies(MatchingNode, InputNodes).

%-----------------------------------------------------------------------------%

	% Do common subexpression elimination.
:- pred rl_block_opt__find_matching_node(exprns::in,
		instr::in, node_id::out) is semidet.

rl_block_opt__find_matching_node(Exprns, Instr, Node) :-
	% Don't do cse on inits.
	% XXX should we? yes - but make init nodes non-shared later.
	Instr \= init(_),
	rl_block_opt__find_matching_node_2(Exprns, Instr, Node).

:- pred rl_block_opt__find_matching_node_2(assoc_list(instr, node_id)::in,
		instr::in, node_id::out) is semidet.

rl_block_opt__find_matching_node_2([InstrToCheck - Node | InstrsToCheck],
		Instr, MatchingNode) :-
	% Could do some more advanced equivalence checking here.
	( InstrToCheck = Instr ->
		MatchingNode = Node
	;
		rl_block_opt__find_matching_node_2(InstrsToCheck, Instr,
			MatchingNode)
	).

%-----------------------------------------------------------------------------%

	% Register the locations of the outputs of an instruction.
:- pred rl_block_opt__add_node_outputs(node_id::in, list(output_rel)::in,
		dag::in, dag::out) is det.

rl_block_opt__add_node_outputs(MatchingNode, OutputRels) -->
	dag_get_node_info_map(NodeInfoMap0),
	{ map__lookup(NodeInfoMap0, MatchingNode, NodeInfo0) },
	{ NodeInfo0 = node_info(Instr0, Outputs0) },
	{ rl_block_opt__do_add_node_outputs(Outputs0, OutputRels, Outputs) },
	{ NodeInfo = node_info(Instr0, Outputs) },
	{ map__det_update(NodeInfoMap0, MatchingNode, NodeInfo, NodeInfoMap) },
	dag_set_node_info_map(NodeInfoMap).

:- pred rl_block_opt__do_add_node_outputs(list(output_node)::in,
		list(output_rel)::in, list(output_node)::out) is det.

rl_block_opt__do_add_node_outputs([], [], []).
rl_block_opt__do_add_node_outputs([], [_|_], _) :-
	error("rl_block_opt__do_add_node_outputs").
rl_block_opt__do_add_node_outputs([_|_], [], _) :-
	error("rl_block_opt__do_add_node_outputs").
rl_block_opt__do_add_node_outputs([Output0 | Outputs0],
		[output_rel(NewOutput, NewIndexes0) | NewOutputs],
		[Output | Outputs]) :-
	Output0 = output_node(Schema, Indexes0, OutputSet0),
	list__delete_elems(NewIndexes0, Indexes0, NewIndexes),
	list__append(Indexes0, NewIndexes, Indexes),
	set__insert(OutputSet0, NewOutput, OutputSet),
	Output = output_node(Schema, Indexes, OutputSet),
	rl_block_opt__do_add_node_outputs(Outputs0, NewOutputs, Outputs).

%-----------------------------------------------------------------------------%

:- pred rl_block_opt__do_add_node(list(output_rel)::in, instr::in,
		node_id::out, dag::in, dag::out) is det.

rl_block_opt__do_add_node(OutputRels, Instr, MatchingNode) -->
	dag_get_next_node_id(MatchingNode),
	list__map_foldl(rl_block_opt__init_output_node,
		OutputRels, Outputs0),
	{ NodeInfo = node_info(Instr, Outputs0) },
	dag_get_node_info_map(NodeInfoMap0),
	{ map__det_insert(NodeInfoMap0, MatchingNode,
		NodeInfo, NodeInfoMap) },
	dag_set_node_info_map(NodeInfoMap),
	dag_get_exprns(Exprns0),
	{ Exprns = [Instr - MatchingNode | Exprns0] },
	dag_set_exprns(Exprns).

:- pred rl_block_opt__init_output_node(output_rel::in, output_node::out,
		dag::in, dag::out) is det.

rl_block_opt__init_output_node(output_rel(OutputRel, Index), OutputRelInfo) -->
	dag_get_relation_info(OutputRel, RelInfo),
	{ RelInfo = relation_info(_, Schema, _, _) },
	{ set__singleton_set(Outputs, OutputRel) },
	{ OutputRelInfo = output_node(Schema, Index, Outputs) }.

	% Set the location of the relations evaluated by the given node.
:- pred rl_block_opt__set_relation_nodes(node_id::in, list(relation_id)::in,
		dag::in, dag::out) is det.

rl_block_opt__set_relation_nodes(MatchingNode, OutputRels) -->
	list__foldl2(rl_block_opt__set_relation_node(MatchingNode),
		OutputRels, 0, _).

:- pred rl_block_opt__set_relation_node(node_id::in, relation_id::in,
		int::in, int::out, dag::in, dag::out) is det.

rl_block_opt__set_relation_node(NodeId, Output, Index0, Index) -->
	dag_get_output_assign_map(Assign0),
	{ InputRel = input_node(NodeId, Index0) },
	( { map__search(Assign0, InputRel, OutputId0) } ->
		{ OutputId = OutputId0 }
	;
		dag_get_next_output_id(OutputId),
		{ map__det_insert(Assign0, InputRel, OutputId, Assign) },
		dag_set_output_assign_map(Assign),
		dag_get_output_loc_map(Locs0),
		{ map__det_insert(Locs0, OutputId, InputRel, Locs) },
		dag_set_output_loc_map(Locs)
	),
	{ Index = Index0 + 1 },
	dag_get_rel_node_map(RelNodeMap0),
	{ map__set(RelNodeMap0, Output, OutputId, RelNodeMap) },
	dag_set_rel_node_map(RelNodeMap).

:- pred rl_block_opt__add_node_dependencies(node_id::in, list(output_id)::in,
		dag::in, dag::out) is det.

rl_block_opt__add_node_dependencies(MatchingNode, InputNodes) -->
	dag_get_output_use_map(Uses0),
	{ AddDepArc = (pred(OutputId::in, UseMap0::in, UseMap::out) is det :-
		multi_map__set(UseMap0, OutputId, MatchingNode, UseMap)
	) },
	{ list__foldl(AddDepArc, InputNodes, Uses0, Uses) },
	dag_set_output_use_map(Uses),

	dag_get_node_dep_map(Dep0),
	{ MMapSet = (pred(Node::in, Depend0::in, Depend::out) is det :-
		multi_map__set(Depend0, MatchingNode, Node, Depend)
	) },
	{ list__foldl(MMapSet, InputNodes, Dep0, Dep) },
	dag_set_node_dep_map(Dep).

%-----------------------------------------------------------------------------%

	% Keep rewriting until nothing changes.
:- pred rl_block_opt__rewrite_dag(dag::in, dag::out) is det.

rl_block_opt__rewrite_dag -->
	dag_get_node_info_map(NodeInfoMap),
	{ map__keys(NodeInfoMap, Nodes) },
	rl_block_opt__rewrite_nodes(Nodes, no, Changed),
	( { Changed = yes } ->
		rl_block_opt__rewrite_dag
	;
		[]
	).

:- pred rl_block_opt__rewrite_nodes(list(node_id)::in, bool::in, bool::out,
		dag::in, dag::out) is det.

rl_block_opt__rewrite_nodes([], Changed, Changed) --> [].
rl_block_opt__rewrite_nodes([Node | Nodes], Changed0, Changed) -->
	dag_get_node_info_map(NodeInfoMap0),

	% This node may have been removed while processing an earlier node.
	( { map__search(NodeInfoMap0, Node, NodeInfo) } ->
		rl_block_opt__rewrite_node(Node, NodeInfo, Changed1),
		{ bool__or(Changed0, Changed1, Changed2) }
	;
		{ Changed2 = Changed0 }
	),
	rl_block_opt__rewrite_nodes(Nodes, Changed2, Changed).

:- pred rl_block_opt__rewrite_node(node_id::in, node_info::in,
		bool::out, dag::in, dag::out) is det.

rl_block_opt__rewrite_node(Node, NodeInfo0, Changed) -->
	dag_get_output_loc_map(Locs),
	dag_get_node_info_map(NodeInfoMap0),
	{ NodeInfo0 = node_info(Instr, OutputRels0) },
	dag_get_flags(Flags),
	dag_get_rl_opt_info(RLInfo),
	{ rl_opt_info_get_module_info(ModuleInfo, RLInfo, _) },
	{ module_info_get_globals(ModuleInfo, Globals) },
	{ globals__lookup_bool_option(Globals,
		optimize_rl_index, OptimizeIndex) },
	(
		{ OptimizeIndex = yes },

		% Look for a union and a difference which can be
		% changed into a union_diff.
		{ Instr = union(UnionInputLocs, _) },

		{ OutputRels0 = [OutputRel0] },
		{ OutputRel0 = output_node(Schema, _, _) },

		% B-tree indexing a zero arity relation would be pretty silly.
		{ Schema \= [] },

		{ list__member(add_uniondiff, Flags) },

		{
			UnionInputLocs = [DiffLoc0, IOLoc0],
			rl_block_opt__get_difference_info(NodeInfoMap0, Locs,
				IOLoc0, DiffLoc0, DiffNode0,
				NewRel0, DiffOutput0)
		->
			DiffLoc = DiffLoc0,
			DiffNode = DiffNode0,
			IOLoc = IOLoc0,
			NewRel = NewRel0,
			DiffOutput = DiffOutput0
		;
			UnionInputLocs = [IOLoc, DiffLoc],
			rl_block_opt__get_difference_info(NodeInfoMap0, Locs,
				IOLoc, DiffLoc, DiffNode, NewRel, DiffOutput)
		}
	->
		rl_block_opt__ensure_index(IOLoc, IndexSpec),
		{ NewInstr = union_diff(IOLoc, NewRel, IndexSpec) },
		rl_block_opt__update_node(Node, NewInstr, [IOLoc, NewRel],
			[OutputRel0, DiffOutput]),

		% Point users of the difference output to its new location.
		% The union output is still in the same place.
		rl_block_opt__rename_node(DiffNode,
			DiffLoc, input_node(Node, 1)),
		rl_block_opt__remove_node(DiffNode),
		{ Changed = yes }
	;
		% If there is a single use of one of the outputs of this node
		% and the use is a project with a single expression (not a
		% merged project produced below), try to merge the project
		% into this operation.
		{ Instr = project(Input, Goals0, ProjType) },
		rl_block_opt__get_single_projects(Node, 0, no, FoundSingle,
			Goals0, Goals, OutputRels0, OutputRels)
	->
		{ Changed = FoundSingle },
		( { FoundSingle = yes } ->
			{ Instr1 = project(Input, Goals, ProjType) },
			{ NodeInfo = node_info(Instr1, OutputRels) },
			dag_get_node_info_map(NodeInfoMap1),
			{ map__det_update(NodeInfoMap1, Node,
				NodeInfo, NodeInfoMap) },
			dag_set_node_info_map(NodeInfoMap)
		;
			[]
		)
	;
		{ list__member(merge_output_projections, Flags) }
	->
		% If there are multiple projections of this node,
		% combine them into one instruction. If the conditions
		% have a key range, we insist that they all have the
		% same one.
		rl_block_opt__merge_output_projections(Node, Changed)
	;
		{ Changed = no }
	).

%-----------------------------------------------------------------------------%

	% Check that the given node holds a difference instruction,
	% and extract some information from the instruction.
:- pred rl_block_opt__get_difference_info(map(node_id, node_info)::in,
		output_loc_map::in, output_id::in, output_id::in,
		node_id::out, output_id::out, output_node::out) is semidet.

rl_block_opt__get_difference_info(NodeInfoMap0, LocMap,
		IOLoc, DiffOutputId, DiffNode, NewRel, DiffOutput) :-
	map__lookup(LocMap, DiffOutputId, DiffLoc),
	DiffLoc = input_node(DiffNode, _),
	map__lookup(NodeInfoMap0, DiffNode, DiffNodeInfo),
	DiffNodeInfo = node_info(DiffInstr, DiffOutputs),
	DiffOutputs = [DiffOutput],
	DiffInstr = difference(NewRel, IOLoc, _).

	% Make sure that the given input to this node has a unique
	% B-tree index on all its attributes.
:- pred rl_block_opt__ensure_index(output_id::in, index_spec::out,
		dag::in, dag::out) is det.

rl_block_opt__ensure_index(OutputId, TheIndex) -->
	rl_block_opt__update_node_index(rl_block_opt__add_full_index,
		OutputId, AddedIndexes),
	{ AddedIndexes = [TheIndex0] ->
		TheIndex = TheIndex0
	;
		error("rl_block_opt__ensure_index")
	}.

:- pred rl_block_opt__add_full_index(list(mer_type)::in, list(index_spec)::in,
		list(index_spec)::out, list(index_spec)::out) is det.

rl_block_opt__add_full_index(Schema, Indexes0, Indexes, [TheIndex]) :-
	rl__attr_list(Schema, Attrs),
	( rl_block_opt__contains_full_index(Attrs, Indexes0, TheIndex0) ->
		TheIndex = TheIndex0,
		Indexes = Indexes0
	;
		TheIndex = index_spec(unique_B_tree, Attrs),
		Indexes = [TheIndex | Indexes0]
	).

:- pred rl_block_opt__contains_full_index(list(int)::in, list(index_spec)::in,
		index_spec::out) is semidet.

rl_block_opt__contains_full_index(Attrs, [Index | Indexes], TheIndex) :-
	Index = index_spec(Type, Attrs0),
	(
		Type = unique_B_tree,
		list__sort_and_remove_dups(Attrs0, Attrs)
	->
		TheIndex = Index
	;
		rl_block_opt__contains_full_index(Attrs, Indexes, TheIndex)
	).

:- pred rl_block_opt__add_index_to_node(output_id::in,
		list(index_spec)::in, dag::in, dag::out) is det.

rl_block_opt__add_index_to_node(OutputId, Index) -->
	rl_block_opt__update_node_index(
		rl_block_opt__add_specific_index(Index), OutputId, _).

:- pred rl_block_opt__add_specific_index(list(index_spec)::in, list(mer_type)::in,
	list(index_spec)::in, list(index_spec)::out,
	list(index_spec)::out) is det.

rl_block_opt__add_specific_index(AddedIndexes0, _,
		Indexes0, Indexes, AddedIndexes) :-
	list__delete_elems(AddedIndexes0, Indexes0, AddedIndexes),
	list__append(Indexes0, AddedIndexes, Indexes).

:- pred rl_block_opt__update_node_index(
	pred(list(mer_type), list(index_spec), list(index_spec), list(index_spec)),
	output_id, list(index_spec), dag, dag).
:- mode rl_block_opt__update_node_index(pred(in, in, out, out) is det,
	in, out, in, out) is det.

rl_block_opt__update_node_index(Update, OutputId, AddedIndexes) -->
	dag_get_output_loc_map(Locs),
	dag_get_node_info_map(NodeInfoMap0),
	{ map__lookup(Locs, OutputId, InputRel) },
	{ InputRel = input_node(InputNode, OutputNo) },
	{ map__lookup(NodeInfoMap0, InputNode, InputInfo0) },
	{ InputInfo0 = node_info(Instr, OutputRels0) },
	( { rl_block_opt__add_index_to_input(Instr, OutputNo, InputLoc) } ->
		rl_block_opt__update_node_index(Update, InputLoc, AddedIndexes)
	;
		{ list__index0_det(OutputRels0, OutputNo, OutputRel0) },
		{ OutputRel0 = output_node(Schema, Index0, RelationIds0) },
		{ call(Update, Schema, Index0, Index, AddedIndexes) },

		{ OutputRel = output_node(Schema, Index, RelationIds0) },
		{ N = OutputNo + 1 },
		{ list__replace_nth(OutputRels0, N, OutputRel, OutputRels1) ->
			OutputRels = OutputRels1
		;
			error(
			"rl_block_opt__ensure_index: list__replace_nth failed")
		},
		{ InputInfo = node_info(Instr, OutputRels) },
		{ map__det_update(NodeInfoMap0, InputNode,
			InputInfo, NodeInfoMap) },
		dag_set_node_info_map(NodeInfoMap)
	).

:- pred rl_block_opt__get_node_indexes(output_id::in, list(index_spec)::out,
		dag::in, dag::out) is det.

rl_block_opt__get_node_indexes(OutputId, Indexes) -->
	dag_get_output_loc_map(Locs),
	{ map__lookup(Locs, OutputId, input_node(InputNode, OutputNo)) },
	dag_get_node_info_map(NodeInfoMap),
	{ map__lookup(NodeInfoMap, InputNode, NodeInfo) },
	{ NodeInfo = node_info(Instr, NodeOutputs) },

	( { rl_block_opt__add_index_to_input(Instr, OutputNo, InputLoc) } ->
		rl_block_opt__get_node_indexes(InputLoc, Indexes)
	;
		{ list__index0_det(NodeOutputs, OutputNo, NodeOutput) },
		{ NodeOutput = output_node(_, Indexes, _) }
	).

	% It doesn't make much sense to add an index after a union_diff
	% or insert has destructively updated a relation - the index
	% should be added to the input relation.
:- pred rl_block_opt__add_index_to_input(instr::in, int::in,
		output_id::out) is semidet.

rl_block_opt__add_index_to_input(Instr, OutputNo, InputLoc) :-
	( Instr = union_diff(_, InputLoc, _)
	; Instr = insert(_, InputLoc, _)
	),
	OutputNo = 0.

%-----------------------------------------------------------------------------%

	% Update the nodes instruction, inputs and outputs.
:- pred rl_block_opt__update_node(node_id::in, instr::in, list(output_id)::in,
		list(output_node)::in, dag::in, dag::out) is det.

rl_block_opt__update_node(Node, Instr, InputLocs, OutputRels) -->
	dag_get_node_info_map(NodeInfoMap0),
	{ map__det_update(NodeInfoMap0, Node, node_info(Instr, OutputRels),
		NodeInfoMap) },
	dag_set_node_info_map(NodeInfoMap),

	% Remove all old dependencies.
	dag_get_output_use_map(Uses0),
	dag_get_node_dep_map(Dep0),
	( { multi_map__search(Dep0, Node, UsedNodes) } ->
		{ multi_map__det_replace(Dep0, Node, [], Dep) },
		{ list__foldl(
			(pred(Use::in, UseMap0::in, UseMap::out) is det :-
				multi_map__delete(UseMap0, Use, Node, UseMap)
			), UsedNodes, Uses0, Uses) },
		dag_set_output_use_map(Uses),
		dag_set_node_dep_map(Dep)
	;
		[]
	),
	rl_block_opt__add_node_dependencies(Node, InputLocs).

:- pred rl_block_opt__rename_node(node_id::in, output_id::in, input_node::in,
		dag::in, dag::out) is det.

rl_block_opt__rename_node(OldNode, OldLoc, InputRel) -->
	dag_get_output_assign_map(Assign0),
	{ map__set(Assign0, InputRel, OldLoc, Assign) },
	dag_set_output_assign_map(Assign),
	dag_get_output_loc_map(Locs0),
	{ map__det_update(Locs0, OldLoc, InputRel, Locs) },
	dag_set_output_loc_map(Locs),

	{ InputRel = input_node(NewNode, _) },

	dag_get_node_dep_map(Deps),
	( { map__search(Deps, OldNode, UsedNodes) } ->
		dag_get_output_use_map(Uses0),
		{ list__foldl(
			(pred(Use::in, UseMap0::in, UseMap::out) is det :-
				multi_map__delete(UseMap0,
					Use, OldNode, UseMap1),
				multi_map__set(UseMap1, Use, NewNode, UseMap)
			), UsedNodes, Uses0, Uses) },
		dag_set_output_use_map(Uses)
	;
		[]
	).

:- pred rl_block_opt__remove_node(node_id::in, dag::in, dag::out) is det.

rl_block_opt__remove_node(Node) -->
	dag_get_node_info_map(NodeInfoMap0),
	{ map__delete(NodeInfoMap0, Node, NodeInfoMap) },
	dag_set_node_info_map(NodeInfoMap).

%-----------------------------------------------------------------------------%

	% If there are multiple projections of the current node, they
	% can all be merged into one.
	% Problems with this
	% - all except one of the outputs must be materialised.
	% - the materialised outputs aren't produced until the stream
	% is evaluated. This may cause problems if a user of a materialised
	% output comes before the user of the non-materialised output. In
	% the worst case, it could cause a circular dependency.
	% The quick hack solution is to just materialise all outputs of a
	% projection if there are more than one.
:- pred rl_block_opt__merge_output_projections(node_id::in, bool::out,
		dag::in, dag::out) is det.

rl_block_opt__merge_output_projections(Node, no) -->
	dag_get_node_info_map(NodeInfoMap),
	{ map__lookup(NodeInfoMap, Node, NodeInfo) },
	{ NodeInfo = node_info(_Instr, OutputNodes) },
	rl_block_opt__find_output_project_nodes(Node, 0, OutputNodes,
		[], OutputProjectNodes),
	{ list__map(rl_block_opt__partition_project_outputs,
		OutputProjectNodes, Partitions0) },
	{ list__condense(Partitions0, Partitions) },
	list__foldl(rl_block_opt__produce_merged_projection, Partitions).

:- type output_projection
	---> output_projection(
		node_id,		% node containing the projection
		instr,
		project_type,
		list(output_node)
	).

	% A partition of the output projections using the same
	% access method (filter / key-range) on the input.
:- type project_partition
	---> project_partition(
		project_type,
		list(output_projection)
	).

	% Work out which of the uses of the outputs of the current node
	% are projections.
:- pred rl_block_opt__find_output_project_nodes(node_id::in, int::in,
	list(output_node)::in, list(list(output_projection))::in,
	list(list(output_projection))::out, dag::in, dag::out) is det.

rl_block_opt__find_output_project_nodes(_, _, [],
		OutputProjns, OutputProjns) --> [].
rl_block_opt__find_output_project_nodes(NodeId, Index,
		[_OutputNode | OutputNodes], OutputProjns0, OutputProjns) -->
	dag_get_output_assign_map(Assign),
	( { map__search(Assign, input_node(NodeId, Index), OutputId) } ->
		dag_get_output_use_map(UseMap),
		( { map__search(UseMap, OutputId, UsingNodes) } ->
			rl_block_opt__find_output_project_nodes_2(UsingNodes,
				[], NodeOutputProjnList),

			% If there are zero or one projections,
			% we have nothing to do.
			( { NodeOutputProjnList = [_, _ | _] } ->
				{ OutputProjns1 =
					[NodeOutputProjnList | OutputProjns0] }
			;
				{ OutputProjns1 = OutputProjns0 }
			)
		;
			{ OutputProjns1 = OutputProjns0 }
		)
	;
		{ OutputProjns1 = OutputProjns0 }
	),
	{ NextIndex = Index + 1 },
	rl_block_opt__find_output_project_nodes(NodeId, NextIndex,
		OutputNodes, OutputProjns1, OutputProjns).

:- pred rl_block_opt__find_output_project_nodes_2(list(node_id)::in,
		list(output_projection)::in, list(output_projection)::out,
		dag::in, dag::out) is det.

rl_block_opt__find_output_project_nodes_2([],
		OutputProjns, OutputProjns) --> [].
rl_block_opt__find_output_project_nodes_2([UsingNode | UsingNodes],
		OutputProjns0, OutputProjns) -->
	dag_get_node_info_map(NodeInfoMap),
	{ map__lookup(NodeInfoMap, UsingNode, NodeInfo) },
	{ NodeInfo = node_info(Instr, Outputs) },
	{
		Instr = project(_, OutputGoals, ProjType),

		% Don't merge in projections where the output is independent
		% of the input - these don't require a traversal of the input
		% at all, and are generated by rl_out.m as an insertion of the
		% generated tuple into the output if the input is non-empty
		\+ (
			OutputGoals = [OutputGoal],
			rl__goal_is_independent_of_input(one, OutputGoal)
		),
		\+ (
			list__member(OtherOutputProjn, OutputProjns0),
			OtherOutputProjn = output_projection(UsingNode,
				_, _, _)
		)
	->
		OutputProjn = output_projection(UsingNode,
				Instr, ProjType, Outputs),
		OutputProjns1 = [OutputProjn | OutputProjns0]
	;
		OutputProjns1 = OutputProjns0
	},
	rl_block_opt__find_output_project_nodes_2(UsingNodes,
		OutputProjns1, OutputProjns).

	% Partition the projections according to the indexes / key ranges
	% used to access the inputs.
:- pred rl_block_opt__partition_project_outputs(list(output_projection)::in,
		list(project_partition)::out) is det.

rl_block_opt__partition_project_outputs(Projects, Partitions) :-
	rl_block_opt__partition_project_outputs_2(Projects, [], Partitions).

:- pred rl_block_opt__partition_project_outputs_2(list(output_projection)::in,
	list(project_partition)::in, list(project_partition)::out) is det.

rl_block_opt__partition_project_outputs_2([], Partitions, Partitions).
rl_block_opt__partition_project_outputs_2([Project | Projects],
		Partitions0, Partitions) :-
	rl_block_opt__add_to_partitions(Project, Partitions0, Partitions1),
	rl_block_opt__partition_project_outputs_2(Projects,
		Partitions1, Partitions).

:- pred rl_block_opt__add_to_partitions(output_projection::in,
	list(project_partition)::in, list(project_partition)::out) is det.

rl_block_opt__add_to_partitions(Project, [],
		[project_partition(ProjType, [Project])]) :-
	Project = output_projection(_, _, ProjType, _).
rl_block_opt__add_to_partitions(Project, [Partition0 | Partitions0],
		[Partition | Partitions]) :-
	Project = output_projection(_, _, ProjType, _),
	Partition0 = project_partition(PartitionType, Projects0),
	( PartitionType = ProjType ->
		Partition = project_partition(PartitionType,
				[Project | Projects0]),
		Partitions = Partitions0
	;
		Partition = Partition0,
		rl_block_opt__add_to_partitions(Project,
			Partitions0, Partitions)
	).

	% Put a node in the dag for the merged output projection.
:- pred rl_block_opt__produce_merged_projection(project_partition::in,
		dag::in, dag::out) is det.

rl_block_opt__produce_merged_projection(project_partition(_, Projects)) -->
	dag_get_next_node_id(NewNode),
	rl_block_opt__collect_project_outputs(NewNode, 0, Projects,
		[], OutputNodes0, [], Goals0),
	{ list__reverse(OutputNodes0, OutputNodes) },
	{ list__reverse(Goals0, Goals) },
	(
		{ Projects = [Project | _] },
		{ Project = output_projection(_,
			project(Input, _, ProjType), _, _) }
	->
                { Instr = project(Input, Goals, ProjType) },
                { NodeInfo = node_info(Instr, OutputNodes) },
                dag_get_node_info_map(NodeInfoMap0),
                { map__det_insert(NodeInfoMap0, NewNode,
			NodeInfo, NodeInfoMap) },
                dag_set_node_info_map(NodeInfoMap),

                dag_get_output_use_map(Uses0),
                { multi_map__lookup(Uses0, Input, InputUses0) },
                { list__filter((pred(NodeId::in) is semidet :-
                        \+ (
                                list__member(Project, Projects),
                                Project = output_projection(NodeId, _, _, _)
                        )
                ), InputUses0, InputUses) },
		{ multi_map__det_replace(Uses0, Input,
			[NewNode | InputUses], Uses) },
		dag_set_output_use_map(Uses),

		dag_get_node_dep_map(Deps0),
		{ multi_map__det_insert(Deps0, NewNode, Input, Deps) },
		dag_set_node_dep_map(Deps)
	;
		{ error("rl_block_opt__produce_merged_projection") }
	).

:- pred rl_block_opt__collect_project_outputs(node_id::in, int::in,
		list(output_projection)::in, list(output_node)::in,
		list(output_node)::out, list(rl_goal)::in, list(rl_goal)::out,
		dag::in, dag::out) is det.

rl_block_opt__collect_project_outputs(_, _, [],
		Outputs, Outputs, Goals, Goals) --> [].
rl_block_opt__collect_project_outputs(NewNode, Index0, [Project | Projects],
		Outputs0, Outputs, Goals0, Goals) -->
	{ Project = output_projection(ProjNode, Instr, _, OutputNodes) },
	( { Instr = project(_, ProjGoals0, _) } ->
		{ list__reverse(ProjGoals0, ProjGoals) },
		{ list__append(ProjGoals, Goals0, Goals1) }
	;
		{ error("rl_block_opt__collect_project_outputs") }
	),

	rl_block_opt__collect_project_outputs_2(ProjNode, 0, OutputNodes,
		NewNode, Index0, Index1, Outputs0, Outputs1),
	rl_block_opt__remove_node(ProjNode),
	rl_block_opt__collect_project_outputs(NewNode, Index1, Projects,
		Outputs1, Outputs, Goals1, Goals).

:- pred rl_block_opt__collect_project_outputs_2(node_id::in, int::in,
		list(output_node)::in, node_id::in, int::in, int::out,
		list(output_node)::in, list(output_node)::out,
		dag::in, dag::out) is det.

rl_block_opt__collect_project_outputs_2(_, _, [],
		_, Index, Index, Outputs, Outputs) --> [].
rl_block_opt__collect_project_outputs_2(ProjNode, OutputIndex0,
		[OutputNode | OutputNodes], NewNode, NewIndex0, NewIndex,
		NewOutputs0, NewOutputs) -->

	% Tell other instructions about the new location of this output.
	dag_get_output_assign_map(Assign),
	{ map__lookup(Assign, input_node(ProjNode, OutputIndex0),
		OldOutputLoc) },
	rl_block_opt__rename_node(ProjNode, OldOutputLoc,
		input_node(NewNode, NewIndex0)),

	{ OutputIndex = OutputIndex0 + 1 },
	{ NewIndex1 = NewIndex0 + 1 },
	rl_block_opt__collect_project_outputs_2(ProjNode, OutputIndex,
		OutputNodes, NewNode, NewIndex1, NewIndex,
		[OutputNode | NewOutputs0], NewOutputs).


%-----------------------------------------------------------------------------%

	% Find projections of the outputs of this node which are
	% used by a single projection.
:- pred rl_block_opt__get_single_projects(node_id::in, int::in,
	bool::in, bool::out, list(rl_goal)::in, list(rl_goal)::out,
	list(output_node)::in, list(output_node)::out,
	dag::in, dag::out) is det.

rl_block_opt__get_single_projects(_, _, FoundSingle, FoundSingle,
		[], [], [], []) --> [].
rl_block_opt__get_single_projects(_, _, _, _,
		[_|_], _, [], _) -->
	{ error("rl_block_opt__get_single_projects") }.
rl_block_opt__get_single_projects(_, _, _, _,
		[], _, [_|_], _) -->
	{ error("rl_block_opt__get_single_projects") }.
rl_block_opt__get_single_projects(Node, Index0, FoundSingle0, FoundSingle,
		[Goal0 | Goals0], [Goal | Goals],
		[OutputRel0 | OutputRels0], [OutputRel | OutputRels]) -->
	dag_get_output_assign_map(Assign),
	( { map__search(Assign, input_node(Node, Index0), OutputId) } ->
		dag_get_output_use_map(Uses0),
		dag_get_node_info_map(NodeInfoMap),
		dag_get_block_info(block_info(_, BlockLiveRels)),
		(
			% If any of the relations live at the end of the
			% block are computed by this projection, it can't
			% be merged.
			{ OutputRel0 = output_node(_, _, Relations) },
			{ set__intersect(Relations, BlockLiveRels,
				NodeBlockLiveRels) },
			{ set__empty(NodeBlockLiveRels) },

			{ multi_map__search(Uses0, OutputId, [SingleNode]) },
			{ map__lookup(NodeInfoMap, SingleNode, NodeInfo) },
			{ NodeInfo = node_info(Instr, [ProjOutputRel]) },
			{ Instr = project(_, [ProjGoal], _) }
		->
			dag_get_rl_opt_info(RLOptInfo),
			{ rl_opt_info_get_module_info(ModuleInfo,
				RLOptInfo, _) },
			{ rl_block_opt__conjoin_goals(ModuleInfo,
				Goal0, ProjGoal, Goal) },
			{ FoundSingle1 = yes },
			{ OutputRel = ProjOutputRel },

			{ map__lookup(Assign, input_node(SingleNode, 0),
				ProjOutputId) },
			{
				multi_map__search(Uses0, ProjOutputId,
					UsingNodes0)
			->
				UsingNodes = UsingNodes0
			;
				UsingNodes = []
			},
			{ multi_map__det_replace(Uses0, OutputId,
				UsingNodes, Uses) },
			dag_set_output_use_map(Uses),
			rl_block_opt__rename_node(SingleNode, ProjOutputId,
				input_node(Node, Index0)),
			rl_block_opt__remove_node(SingleNode)
		;

			{ FoundSingle1 = FoundSingle0 },
			{ Goal = Goal0 },
			{ OutputRel = OutputRel0 }
		)
	;
		{ FoundSingle1 = FoundSingle0 },
		{ Goal = Goal0 },
		{ OutputRel = OutputRel0 }
	),
	{ Index = Index0 + 1 },
	rl_block_opt__get_single_projects(Node, Index,
		FoundSingle1, FoundSingle, Goals0, Goals,
		OutputRels0, OutputRels).

:- pred rl_block_opt__conjoin_goals(module_info::in, rl_goal::in,
		rl_goal::in, rl_goal::out) is det.

rl_block_opt__conjoin_goals(ModuleInfo, RLGoal1, RLGoal2, RLGoal) :-
	RLGoal1 = rl_goal(_, VarSet1, VarTypes1, InstMap0,
			Inputs1, Outputs1, Goals1, _),
	RLGoal2 = rl_goal(_, VarSet2, VarTypes2, _,
			Inputs2, Outputs2, Goals2, _),
	(
		Outputs1 = no,
		( Inputs1 = one_input(InputArgs1) ->
			RenameCallArgs = InputArgs1
		;
			error("rl_block_opt__conjoin_goals")
		)
	;
		Outputs1 = yes(RenameCallArgs)
	),

	( Inputs2 = one_input(InputArgs2) ->
		RenameHeadVars = InputArgs2
	;
		error("rl_block_opt__conjoin_goals")
	),

	goal_info_init(DummyGoalInfo),

	% We don't need to do anything special with the var_types here
	% because the types are guaranteed to be ground.
	inlining__rename_goal(RenameHeadVars, RenameCallArgs,
		VarSet1, VarSet2, VarSet, VarTypes1, VarTypes2, VarTypes,
		Subn, conj(Goals2) - DummyGoalInfo, RenamedGoal),

	(
		Outputs2 = yes(OutputArgs2),
		goal_util__rename_var_list(yes, Subn, OutputArgs2, OutputArgs),
		Outputs = yes(OutputArgs)
	;
		Outputs2 = no,
		Outputs = no
	),

	( RenamedGoal = conj(RenamedGoals2) - _ ->
		% XXX do some simplification, constraint propagation etc.
		list__append(Goals1, RenamedGoals2, Goals)
	;
		error("rl_block_opt__conjoin_goals")
	),

	rl_key__extract_indexing(Inputs1, Goals, ModuleInfo, VarTypes, Bounds),

	RLGoal = rl_goal(no, VarSet, VarTypes, InstMap0, Inputs1,
			Outputs, Goals, Bounds).

%-----------------------------------------------------------------------------%

	% Find a near-optimal ordering for the instructions in the dag.
	%
	% An optimal ordering minimises the total size of the live relations
	% at any point. At the moment we just take a topological sort.
	%
	% After an instruction which has a stream as an output, the
	% materialised relations that went in at the bottom of the stream
	% are still live. For a non-stream instruction, only the output
	% relation is definitely live.

:- pred rl_block_opt__deconstruct_dag(list(rl_instruction)::out,
		dag::in, dag::out) is det.

rl_block_opt__deconstruct_dag(Instrs) -->

	dag_get_block_info(block_info(_, LiveAtEnd0)),
	{ set__to_sorted_list(LiveAtEnd0, LiveAtEnd) },

	% Find out which of the non-locals are evaluated by this block.
	dag_get_rel_node_map(RelNodeMap),
	dag_get_output_loc_map(Locs),
	{ IsProducedNonLocal = (pred(Rel::in, Node::out) is semidet :-
		map__search(RelNodeMap, Rel, OutputId),
		map__lookup(Locs, OutputId, input_node(Node, _))
	) },

	{ list__filter_map(IsProducedNonLocal, LiveAtEnd,
		RequiredNodes0) },

	{ set__init(RequiredNodes1) },
	{ relation__init(NodeRel0) },
	rl_block_opt__get_required_nodes(RequiredNodes0, RequiredNodes1,
		RequiredNodes, NodeRel0, NodeRel),

	{ relation__tsort(NodeRel, NodeSort0) ->
		list__reverse(NodeSort0, NodeSort1)
	;
		error("rl_block_opt__deconstruct_dag: relation__tsort failed")
	},
	{ list__filter(
		(pred(X::in) is semidet :- set__member(X, RequiredNodes)),
		NodeSort1, NodeSort) },
	{ map__init(NodeRels) },
	rl_block_opt__evaluate_nodes(NodeSort, LiveAtEnd0,
		NodeRels, [], [], Instrs).

%-----------------------------------------------------------------------------%

	% Find out which instruction nodes are not dead code.
:- pred rl_block_opt__get_required_nodes(list(node_id)::in, set(node_id)::in,
	set(node_id)::out, relation(node_id)::in, relation(node_id)::out,
	dag::in, dag::out) is det.

rl_block_opt__get_required_nodes([], Required, Required,
		NodeRel, NodeRel) --> [].
rl_block_opt__get_required_nodes([Node | Nodes0], Required0, Required,
		NodeRel0, NodeRel) -->
	( { set__member(Node, Required0) } ->
		{ Required1 = Required0 },
		{ NodeRel1 = NodeRel0 },
		{ Nodes1 = Nodes0 }
	;
		{ set__insert(Required0, Node, Required1) },
		dag_get_node_dep_map(Dep),
		dag_get_output_loc_map(Locs),
		{ map__search(Dep, Node, NeededOutputs) ->
			GetNode =
				(pred(OutputId::in, OutputNode::out) is det :-
					map__lookup(Locs, OutputId,
						input_node(OutputNode, _))
				),
			list__map(GetNode, NeededOutputs, SubNodes),
			list__append(SubNodes, Nodes0, Nodes1),
			AddDep =
				(pred(DepNode::in, Rel0::in, Rel::out) is det :-
					relation__add_values(Rel0,
						Node, DepNode, Rel)
				),
			list__foldl(AddDep, SubNodes, NodeRel0, NodeRel1)
		;
			Nodes1 = Nodes0,
			% Make sure this node is in the topological
			% sort of the required nodes.
			relation__add_element(NodeRel0, Node, _, NodeRel1)
		}
	),
	rl_block_opt__get_required_nodes(Nodes1, Required1, Required,
		NodeRel1, NodeRel).

%-----------------------------------------------------------------------------%

:- type node_rels == map(node_id, list(output_rel)).

:- pred rl_block_opt__evaluate_nodes(list(node_id)::in, set(relation_id)::in,
	node_rels::in, list(rl_instruction)::in, list(rl_instruction)::in,
	list(rl_instruction)::out, dag::in, dag::out) is det.

rl_block_opt__evaluate_nodes([], _, _, AssignInstrs, RevInstrs, Instrs) -->
	{ list__append(AssignInstrs, RevInstrs, Instrs1) },
	{ list__reverse(Instrs1, Instrs) }.
rl_block_opt__evaluate_nodes([Node | Nodes], NonLocalRels, NodeRels0,
		AssignInstrs0, Instrs0, Instrs) -->
	rl_block_opt__evaluate_node(Node, NonLocalRels, NodeRels0, NodeRels1,
		AssignInstrs0, AssignInstrs1, Instrs0, Instrs1),
	rl_block_opt__evaluate_nodes(Nodes, NonLocalRels,
		NodeRels1, AssignInstrs1, Instrs1, Instrs).

:- pred rl_block_opt__evaluate_node(node_id::in, set(relation_id)::in,
	node_rels::in, node_rels::out, list(rl_instruction)::in,
	list(rl_instruction)::out, list(rl_instruction)::in,
	list(rl_instruction)::out, dag::in, dag::out) is det.

rl_block_opt__evaluate_node(Node, NonLocalRels, NodeRels0, NodeRels,
		AssignInstrs0, AssignInstrs, RevInstrs0, RevInstrs) -->
	dag_get_node_info_map(NodeInfoMap),
	{ map__lookup(NodeInfoMap, Node, NodeInfo) },
	{ NodeInfo = node_info(Instr0, OutputRels) },
	rl_block_opt__get_node_outputs(Instr0, NonLocalRels,
		OutputRels, NodeOutputs, [], AssignInstrs1),
	rl_block_opt__generate_instr(OutputRels, NodeOutputs,
		NodeRels0, Instr0, NodeInstrs),
	{ map__det_insert(NodeRels0, Node, NodeOutputs, NodeRels) },
	{ list__reverse(NodeInstrs, RevNodeInstrs) },
	{ list__append(RevNodeInstrs, RevInstrs0, RevInstrs) },
	{ list__append(AssignInstrs1, AssignInstrs0, AssignInstrs) }.

	% Select a relation variable to collect the output for each
	% output argument of the node. Create `ref' instructions
	% for each of the other relations produced by the position.
:- pred rl_block_opt__get_node_outputs(instr::in, set(relation_id)::in,
	list(output_node)::in, list(output_rel)::out, list(rl_instruction)::in,
	list(rl_instruction)::out, dag::in, dag::out) is det.

rl_block_opt__get_node_outputs(_, _, [], [], Instrs, Instrs) --> [].
rl_block_opt__get_node_outputs(Instr, NonLocalRels, [OutputNode | OutputNodes],
		[OutputRel | OutputRels], RefInstrs0, RefInstrs) -->
	{ OutputNode = output_node(Schema, Index, AllOutputs0) },
	{ set__intersect(AllOutputs0, NonLocalRels, NonLocalOutputs0) },
	{ Instr = input(InputRel1) ->
		% Don't assign the input relation to itself.
		set__delete(NonLocalOutputs0, InputRel1, NonLocalOutputs)
	;
		NonLocalOutputs = NonLocalOutputs0
	},
	(
		{
		% If an input relation is passed through without change,
		% we don't need to generate any code for the instruction.
		Instr = input(InputRel2),
		Index = [],

		% If the input relation is not an output of the `input'
		% instruction, another instruction has overwritten its value.
		set__member(InputRel2, NonLocalOutputs0)
		}
	->
		{ RelationId = InputRel2 }
	;
		dag_add_relation(Schema, RelationId)
	),
	{ OutputRel = output_rel(RelationId, Index) },
	{ set__to_sorted_list(NonLocalOutputs, OutputList) },
	{ rl_block_opt__get_ref_instrs(RelationId, OutputList, NewRefInstrs) },
	{ list__append(NewRefInstrs, RefInstrs0, RefInstrs1) },
	rl_block_opt__get_node_outputs(Instr, NonLocalRels, OutputNodes,
		OutputRels, RefInstrs1, RefInstrs).

:- pred rl_block_opt__get_ref_instrs(relation_id::in, list(relation_id)::in,
		list(rl_instruction)::out) is det.

rl_block_opt__get_ref_instrs(RelationId, OutputList, RefInstrs) :-
	GetRefInstr = (pred(OtherOutput::in, Instr::out) is det :-
		Instr = ref(OtherOutput, RelationId) - ""
	),
	list__map(GetRefInstr, OutputList, RefInstrs).

%-----------------------------------------------------------------------------%

:- pred rl_block_opt__generate_instr(list(output_node)::in,
	list(output_rel)::in, node_rels::in, instr::in,
	list(rl_instruction)::out, dag::in, dag::out) is det.

rl_block_opt__generate_instr(_, NodeOutputs, NodeRels,
		join(Input1Loc, Input2Loc, JoinType, Exprn,
			SemiJoin, TrivialJoin),
		[JoinInstr]) -->

	rl_block_opt__get_relation_id(NodeRels, Input1Loc, Input1),
	rl_block_opt__get_relation_id(NodeRels, Input2Loc, Input2),
	{ rl_block_opt__one_output(NodeOutputs, Output) },
	{ JoinInstr = join(Output, Input1, Input2, JoinType, Exprn,
			SemiJoin, TrivialJoin) - "" }.

rl_block_opt__generate_instr(_, NodeOutputs, NodeRels,
		subtract(Input1Loc, Input2Loc, Type, Exprn, TrivialSubtract),
		[SubtractInstr]) -->

	rl_block_opt__get_relation_id(NodeRels, Input1Loc, Input1),
	rl_block_opt__get_relation_id(NodeRels, Input2Loc, Input2),
	{ rl_block_opt__one_output(NodeOutputs, Output) },
	{ SubtractInstr = subtract(Output, Input1, Input2, Type,
				Exprn, TrivialSubtract) - "" }.

rl_block_opt__generate_instr(_, NodeOutputs, NodeRels,
		difference(Input1Loc, Input2Loc, Type),
		[difference(Output, Input1, Input2, Type) - ""]) -->
	rl_block_opt__get_relation_id(NodeRels, Input1Loc, Input1),
	rl_block_opt__get_relation_id(NodeRels, Input2Loc, Input2),
	{ rl_block_opt__one_output(NodeOutputs, Output) }.

rl_block_opt__generate_instr(_, NodeOutputs, NodeRels,
		project(Input1Loc, Exprns, Type),
		[project(Output, Input1, Exprn, OtherOutputs, Type) - ""]) -->
	rl_block_opt__get_relation_id(NodeRels, Input1Loc, Input1),
	{
		% XXX do smarter selection of the stream output, although
		% currently the stream output isn't actually used as a stream
		% if there are other outputs because it is difficult to
		% handle streams with side effects.
		NodeOutputs = [Output0 | Outputs],
		Exprns = [Exprn0 | OtherExprns]
	->
		Output = Output0,
		Exprn = Exprn0,
		assoc_list__from_corresponding_lists(Outputs, OtherExprns,
			OtherOutputs)
	;
		error("rl_block_opt__generate_instr: invalid project")
	}.

rl_block_opt__generate_instr(_OutputRels, NodeOutputs, NodeRels,
		union(InputLocs, Type), Instrs) -->
	{ rl_block_opt__one_output(NodeOutputs, Output) },
	list__map_foldl(rl_block_opt__get_relation_id(NodeRels),
		InputLocs, Inputs),
	{ Instrs = [union(Output, Inputs, Type) - ""] }.

rl_block_opt__generate_instr(_OutputRels, NodeOutputs,
		NodeRels, insert(DiInputLoc, InputLoc, Type), Instrs) -->
	rl_block_opt__get_relation_id(NodeRels, InputLoc, Input),
	rl_block_opt__get_relation_id(NodeRels, DiInputLoc, DiInput),
	{ rl_block_opt__one_output(NodeOutputs, output_rel(UoOutput, _)) },
	rl_block_opt__get_copy_info(DiInputLoc, DiInput, CopyRel),
	{ Instrs = [insert(UoOutput, DiInput, Input, Type,
			yes(CopyRel)) - ""] }.

rl_block_opt__generate_instr(_, NodeOutputs, NodeRels,
		union_diff(DiInputLoc, InputLoc, Index), Instrs) -->
	rl_block_opt__get_relation_id(NodeRels, InputLoc, Input),
	rl_block_opt__get_relation_id(NodeRels, DiInputLoc, DiInput),
	rl_block_opt__get_copy_info(DiInputLoc, DiInput, CopyRel),
	{ NodeOutputs = [output_rel(UoOutput, _), Diff] ->
		Instrs = [union_diff(UoOutput, DiInput, Input,
			 Diff, Index, yes(CopyRel)) - ""]
	;
		error("rl_block_opt__generate_instr: error in union_diff")
	}.

rl_block_opt__generate_instr(_, NodeOutputs, NodeRels,
		sort(Input1Loc, Attrs), [sort(Output, Input1, Attrs) - ""]) -->
	rl_block_opt__get_relation_id(NodeRels, Input1Loc, Input1),
	{ rl_block_opt__one_output(NodeOutputs, Output) }.

rl_block_opt__generate_instr(_, NodeOutputs, NodeRels,
		call(ProcName, InputLocs),
		[call(ProcName, Inputs, NodeOutputs, Saved) - ""]) -->
	% Liveness must be run after this.
	{ set__init(Saved) },
	list__map_foldl(rl_block_opt__get_relation_id(NodeRels),
		InputLocs, Inputs).

rl_block_opt__generate_instr(_, NodeOutputs, NodeRels,
		aggregate(Input1Loc, Initial, Update),
		[aggregate(Output, Input1, Initial, Update) - ""]) -->
	rl_block_opt__get_relation_id(NodeRels, Input1Loc, Input1),
	{ rl_block_opt__one_output(NodeOutputs, Output) }.

rl_block_opt__generate_instr(_, NodeOutputs, NodeRels,
		insert_tuple(Input1Loc, Exprn),
		[insert_tuple(Output, Input1, Exprn) - ""]) -->
	rl_block_opt__get_relation_id(NodeRels, Input1Loc, Input1),
	{ rl_block_opt__one_output(NodeOutputs, Output) }.

rl_block_opt__generate_instr(_, NodeOutputs, _NodeRels,
		init(_), [init(Output) - ""]) -->
	{ rl_block_opt__one_output(NodeOutputs, Output) }.

rl_block_opt__generate_instr(_, NodeOutputs, _, input(Input), Instrs) -->
	{ rl_block_opt__one_output(NodeOutputs, OutputRel) },
	{ OutputRel = output_rel(Output, Indexes) },

	{ Indexes = [] ->
		( Output = Input ->
			Instrs = []
		;
			Instrs = [ref(Output, Input) - ""]
		)
	;
		Instrs = [add_index(OutputRel, Input) - ""]
	}.

%-----------------------------------------------------------------------------%

:- pred rl_block_opt__get_relation_id(node_rels::in,
		output_id::in, relation_id::out, dag::in, dag::out) is det.

rl_block_opt__get_relation_id(NodeRels, OutputId, RelationId) -->
	dag_get_output_loc_map(Locs),
	{ map__lookup(Locs, OutputId, input_node(Node, Index)) },
	{ map__lookup(NodeRels, Node, NodeOutputs) },
	{ list__index0_det(NodeOutputs, Index, output_rel(RelationId, _)) }.

%-----------------------------------------------------------------------------%

	% Produce a new relation to hold a copy of the destructively updated
	% input to a union_diff or insert instruction.
:- pred rl_block_opt__get_copy_info(output_id::in, relation_id::in,
		output_rel::out, dag::in, dag::out) is det.

rl_block_opt__get_copy_info(DiInputLoc, DiInput,
		output_rel(CopyRel, CopyIndexes)) -->
	rl_block_opt__get_node_indexes(DiInputLoc, CopyIndexes),
	dag_get_relation_info(DiInput, relation_info(_, Schema, _, _)),
	dag_add_relation(Schema, CopyRel).

%-----------------------------------------------------------------------------%

:- pred rl_block_opt__one_output(list(T)::in, T::out) is det.

rl_block_opt__one_output(Outputs, Output) :-
	( Outputs = [Output0] ->
		Output = Output0
	;
		error("rl_block_opt__one_output")
	).

%-----------------------------------------------------------------------------%

	% Cut down versions of the relational operations without their outputs.
:- type instr
	--->	join(output_id, output_id, join_type, rl_goal,
			maybe(semi_join_info), maybe(trivial_join_info))
	;	subtract(output_id, output_id, subtract_type, rl_goal,
			maybe(trivial_subtract_info))
	;	difference(output_id, output_id, difference_type)
		% The first output of a project is distinguished -
		% it can be a stream, the others must all be materialised.
	;	project(output_id, list(rl_goal), project_type)
	;	union(list(output_id), union_type)
	;	union_diff(output_id, output_id, index_spec)
	;	insert(output_id, output_id, insert_type)
	;	sort(output_id, assoc_list(int, sort_dir))
	;	call(rl_proc_name, list(output_id))
	;	aggregate(output_id, pred_proc_id, pred_proc_id)
	;	insert_tuple(output_id, rl_goal)
	;	init(list(mer_type))
	;	input(relation_id)	% input to block
	.

:- type dag
	---> dag(
		node_id,			% next node_id
		output_id,			% next output_id
		rel_node_map,
		exprns,
		node_info_map,
		output_assign_map,
		output_loc_map,
		output_use_map,
		node_dep_map,
		rl_opt_info,
		block_info,
		list(opt_flag),
		unit
	).

	% An output_id represents an output of some relational operation.
:- type output_id == int.

:- type exprns == assoc_list(instr, node_id).
:- type node_info_map == map(node_id, node_info).
:- type rel_node_map == map(relation_id, output_id).
:- type output_assign_map == map(input_node, output_id).
:- type output_loc_map == map(output_id, input_node).
:- type output_use_map == multi_map(output_id, node_id).
:- type node_dep_map == multi_map(node_id, output_id).

:- type node_info
	---> node_info(
		instr,
		list(output_node)
	).

:- type output_node
	---> output_node(
		list(mer_type),		% schema.
		list(index_spec),	% indexes which should be added to
					% the outputs.
		set(relation_id)	% relation_ids produced
					% by this output.
	).

:- type input_node
	---> input_node(
		node_id,		% node producing the input
		int			% index in list of outputs
					% of that node, starting at 0.
	).

:- type node_id == int.

:- pred dag_init(rl_opt_info::in, block_info::in,
		list(opt_flag)::in, dag::out) is det.

dag_init(RLOptInfo, BlockInfo, Flags, dag(0, 0, RelMap, [], Outputs, Assign,
		Locs, Uses, Dep, RLOptInfo, BlockInfo, Flags, unit)) :-
	map__init(RelMap),
	map__init(Outputs),
	map__init(Assign),
	map__init(Locs),
	map__init(Uses),
	map__init(Dep).

:- pred dag_get_next_node_id(node_id, dag, dag).
:- mode dag_get_next_node_id(out, in, out) is det.

:- pred dag_get_next_output_id(node_id, dag, dag).
:- mode dag_get_next_output_id(out, in, out) is det.

:- pred dag_get_exprns(exprns, dag, dag).
:- mode dag_get_exprns(out, in, out) is det.
:- pred dag_set_exprns(exprns, dag, dag).
:- mode dag_set_exprns(in, in, out) is det.

:- pred dag_get_node_info_map(node_info_map, dag, dag).
:- mode dag_get_node_info_map(out, in, out) is det.
:- pred dag_set_node_info_map(node_info_map, dag, dag).
:- mode dag_set_node_info_map(in, in, out) is det.

:- pred dag_get_rel_node_map(rel_node_map, dag, dag).
:- mode dag_get_rel_node_map(out, in, out) is det.
:- pred dag_set_rel_node_map(rel_node_map, dag, dag).
:- mode dag_set_rel_node_map(in, in, out) is det.

:- pred dag_get_output_assign_map(output_assign_map, dag, dag).
:- mode dag_get_output_assign_map(out, in, out) is det.
:- pred dag_set_output_assign_map(output_assign_map, dag, dag).
:- mode dag_set_output_assign_map(in, in, out) is det.

:- pred dag_get_output_loc_map(output_loc_map, dag, dag).
:- mode dag_get_output_loc_map(out, in, out) is det.
:- pred dag_set_output_loc_map(output_loc_map, dag, dag).
:- mode dag_set_output_loc_map(in, in, out) is det.

:- pred dag_get_output_use_map(output_use_map, dag, dag).
:- mode dag_get_output_use_map(out, in, out) is det.
:- pred dag_set_output_use_map(output_use_map, dag, dag).
:- mode dag_set_output_use_map(in, in, out) is det.

:- pred dag_get_node_dep_map(node_dep_map, dag, dag).
:- mode dag_get_node_dep_map(out, in, out) is det.
:- pred dag_set_node_dep_map(node_dep_map, dag, dag).
:- mode dag_set_node_dep_map(in, in, out) is det.

:- pred dag_get_rl_opt_info(rl_opt_info, dag, dag).
:- mode dag_get_rl_opt_info(out, in, out) is det.
:- pred dag_set_rl_opt_info(rl_opt_info, dag, dag).
:- mode dag_set_rl_opt_info(in, in, out) is det.

:- pred dag_get_block_info(block_info, dag, dag).
:- mode dag_get_block_info(out, in, out) is det.
:- pred dag_set_block_info(block_info, dag, dag).
:- mode dag_set_block_info(in, in, out) is det.

:- pred dag_get_flags(list(opt_flag), dag, dag).
:- mode dag_get_flags(out, in, out) is det.
:- pred dag_set_flags(list(opt_flag), dag, dag).
:- mode dag_set_flags(in, in, out) is det.

dag_get_rel_node_map(C, Dag, Dag) :-
	Dag = dag(_,_,C,_,_,_,_,_,_,_,_,_,_).
dag_get_exprns(D, Dag, Dag) :-
	Dag = dag(_,_,_,D,_,_,_,_,_,_,_,_,_).
dag_get_node_info_map(E, Dag, Dag) :-
	Dag = dag(_,_,_,_,E,_,_,_,_,_,_,_,_).
dag_get_output_assign_map(F, Dag, Dag) :-
	Dag = dag(_,_,_,_,_,F,_,_,_,_,_,_,_).
dag_get_output_loc_map(G, Dag, Dag) :-
	Dag = dag(_,_,_,_,_,_,G,_,_,_,_,_,_).
dag_get_output_use_map(H, Dag, Dag) :-
	Dag = dag(_,_,_,_,_,_,_,H,_,_,_,_,_).
dag_get_node_dep_map(I, Dag, Dag) :-
	Dag = dag(_,_,_,_,_,_,_,_,I,_,_,_,_).
dag_get_rl_opt_info(J, Dag, Dag) :-
	Dag = dag(_,_,_,_,_,_,_,_,_,J,_,_,_).
dag_get_block_info(K, Dag, Dag) :-
	Dag = dag(_,_,_,_,_,_,_,_,_,_,K,_,_).
dag_get_flags(L, Dag, Dag) :-
	Dag = dag(_,_,_,_,_,_,_,_,_,_,_,L,_).

dag_set_rel_node_map(C, Dag0, Dag) :-
	Dag0 = dag(A,B,_,D,E,F,G,H,I,J,K,L,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).
dag_set_exprns(D, Dag0, Dag) :-
	Dag0 = dag(A,B,C,_,E,F,G,H,I,J,K,L,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).
dag_set_node_info_map(E, Dag0, Dag) :-
	Dag0 = dag(A,B,C,D,_,F,G,H,I,J,K,L,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).
dag_set_output_assign_map(F, Dag0, Dag) :-
	Dag0 = dag(A,B,C,D,E,_,G,H,I,J,K,L,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).
dag_set_output_loc_map(G, Dag0, Dag) :-
	Dag0 = dag(A,B,C,D,E,F,_,H,I,J,K,L,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).
dag_set_output_use_map(H, Dag0, Dag) :-
	Dag0 = dag(A,B,C,D,E,F,G,_,I,J,K,L,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).
dag_set_node_dep_map(I, Dag0, Dag) :-
	Dag0 = dag(A,B,C,D,E,F,G,H,_,J,K,L,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).
dag_set_rl_opt_info(J, Dag0, Dag) :-
	Dag0 = dag(A,B,C,D,E,F,G,H,I,_,K,L,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).
dag_set_block_info(K, Dag0, Dag) :-
	Dag0 = dag(A,B,C,D,E,F,G,H,I,J,_,L,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).
dag_set_flags(L, Dag0, Dag) :-
	Dag0 = dag(A,B,C,D,E,F,G,H,I,J,K,_,M),
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).

dag_get_next_node_id(A0, Dag0, Dag) :-
	Dag0 = dag(A0,B,C,D,E,F,G,H,I,J,K,L,M),
	A = A0 + 1,
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).

dag_get_next_output_id(B0, Dag0, Dag) :-
	Dag0 = dag(A,B0,C,D,E,F,G,H,I,J,K,L,M),
	B = B0 + 1,
	Dag = dag(A,B,C,D,E,F,G,H,I,J,K,L,M).

:- pred dag_get_relation_info(relation_id::in, relation_info::out,
		dag::in, dag::out) is det.

dag_get_relation_info(RelationId, RelationInfo) -->
	dag_get_rl_opt_info(RLOptInfo),
	{ rl_opt_info_get_relation_info(RelationId, RelationInfo,
		RLOptInfo, _) }.

:- pred dag_add_relation(list(mer_type)::in, relation_id::out,
		dag::in, dag::out) is det.

dag_add_relation(Schema, RelationId) -->
	dag_get_rl_opt_info(RLOptInfo0),
	{ rl_opt_info_add_relation(Schema, RelationId,
		RLOptInfo0, RLOptInfo) },
	dag_set_rl_opt_info(RLOptInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
