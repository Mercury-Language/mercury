%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_block.m
% Main author: stayl
%
% Find the basic blocks and loops in a list of RL instructions.
% Produce a flow graph.
% See the Dragon Book for details.
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_block.

:- interface.

:- import_module aditi_backend__rl.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module relation.
:- import_module set.
:- import_module std_util.

:- pred rl_block__create_flow_graph(bool::in, module_info::in,
		rl_proc::in, rl_opt_info::out,
		io__state::di, io__state::uo) is det.

	% Find all relations which are used by more than one block.
:- pred rl_block__get_nonlocal_relations(set(relation_id)::out,
		rl_opt_info::in, rl_opt_info::out) is det.

	% Convert a flow graph back into a list of instructions.
:- pred rl_block__get_proc(rl_opt_info::in, rl_proc_name::in,
		list(pred_proc_id)::in, rl_proc::out) is det.

	% If the boolean is `yes' dump the RL code.
:- pred rl_block__dump_blocks(bool::in, rl_opt_info::in,
		rl_opt_info::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module aditi_backend__rl_dump.
:- import_module hlds__hlds_module.

:- import_module queue.
:- import_module require.
:- import_module string.

rl_block__create_flow_graph(Debug, ModuleInfo, Proc, Info) -->
	{ rl_block__build_maps(ModuleInfo, Proc, Info1) },
	rl_block__dump_blocks(Debug, Info1, Info2),
	rl_block__dump_flow_graph(Debug, Info2, Info3),

	{ rl_block__get_dominator_info(Info3, Info4) },
	{ rl_block__find_loops(Info4, Info5) },
	{ rl_opt_info_get_loops(Loops, Info5, Info) },
	rl_block__dump_loops(Debug, Loops).

	% Separate out the instructions into blocks.
:- pred rl_block__build_maps(module_info::in, rl_proc::in,
		rl_opt_info::out) is det.

rl_block__build_maps(ModuleInfo, Proc, Info) :-
	Proc = rl_proc(_, Inputs, Outputs, Memoed, RelInfo, Instrs, _),
	rl_opt_info_init(ModuleInfo, RelInfo, Inputs, Outputs, Memoed,
		Info0, FirstBlockId),

	% Some analyses require that the first block not be branched
	% to from anywhere, so we add an empty one here.
	block_info_init(BlockInfo),
	rl_opt_info_add_block(FirstBlockId, block(no, [], no, BlockInfo),
		Info0, Info1),
	rl_opt_info_get_next_block_id(BlockId, Info1, Info2),
	rl_opt_info_add_flow_graph_arc(FirstBlockId, BlockId, Info2, Info3),
	rl_block__build_maps_2([], Instrs, BlockId, no, Info3, Info).

:- pred rl_block__build_maps_2(list(rl_instruction)::in,
		list(rl_instruction)::in, block_id::in, maybe(label_id)::in,
		rl_opt_info::in, rl_opt_info::out) is det.

rl_block__build_maps_2(RevBlockInstrs, [], BlockId, MaybeLabel) -->
	% Some analyses, e.g. liveness, require that the last block
	% only falls through to the end of the procedure, so we add an
	% extra one even if there are no instructions.
	{ list__reverse(RevBlockInstrs, BlockInstrs) },
	{ block_info_init(BlockInfo) },
	{ Block = block(MaybeLabel, BlockInstrs, no, BlockInfo) },
	rl_opt_info_add_block(BlockId, Block),
	rl_opt_info_set_last_block_id(BlockId).

rl_block__build_maps_2(RevBlockInstrs0, [Instr | Instrs],
		BlockId, MaybeLabel0) -->
	( { Instr = label(Label) - _ } ->
		% Assign a new block_id for the label.
		rl_opt_info_add_label(Label, LabelBlockId),
		rl_opt_info_add_flow_graph_arc(BlockId, LabelBlockId),
		{ list__reverse(RevBlockInstrs0, BlockInstrs) },
		{ block_info_init(BlockInfo) },
		{ Block = block(MaybeLabel0, BlockInstrs, no, BlockInfo) },
		rl_opt_info_add_block(BlockId, Block),
		rl_block__build_maps_2([], Instrs, LabelBlockId, yes(Label))
	; { Instr = conditional_goto(_, Label) - _ } ->
		rl_opt_info_add_label(Label, LabelBlockId),
		rl_opt_info_add_flow_graph_arc(BlockId, LabelBlockId),

		rl_opt_info_get_next_block_id(NextBlockId),
		rl_opt_info_add_flow_graph_arc(BlockId, NextBlockId),

		{ list__reverse(RevBlockInstrs0, BlockInstrs) },
		{ block_info_init(BlockInfo) },
		{ Block = block(MaybeLabel0, BlockInstrs,
			yes(Instr), BlockInfo) },
		rl_opt_info_add_block(BlockId, Block),

		rl_block__build_maps_2([], Instrs, NextBlockId, no)
	; { Instr = goto(Label) - _ } ->
		rl_opt_info_add_label(Label, LabelBlockId),
		rl_opt_info_add_flow_graph_arc(BlockId, LabelBlockId),

		{ list__reverse(RevBlockInstrs0, BlockInstrs) },
		{ block_info_init(BlockInfo) },
		{ Block = block(MaybeLabel0, BlockInstrs,
			yes(Instr), BlockInfo) },
		rl_opt_info_add_block(BlockId, Block),

		% Avoid creating a dead block in between the
		% goto and the next label.
		( { rl_block__get_next_label(NextLabel, Instrs, Instrs1) } ->
			rl_opt_info_add_label(NextLabel, NextLabelBlockId),
			rl_block__build_maps_2([], Instrs1,
				NextLabelBlockId, yes(NextLabel))
		;
			% There must always be a way to get to the end
			% of the procedure.
			{ error("rl_block__build_maps - goto not followed by a label") }
		)
	; { Instr = comment - _ } ->
		% Strip comments, since they are likely to be fairly
		% meaningless after optimization.
		rl_block__build_maps_2(RevBlockInstrs0,
			Instrs, BlockId, MaybeLabel0)
	;
		rl_block__build_maps_2([Instr | RevBlockInstrs0],
			Instrs, BlockId, MaybeLabel0)
	).

	% Find the next label after an unconditional goto, removing
	% any instructions in between since they are dead code.
:- pred rl_block__get_next_label(label_id::out, list(rl_instruction)::in,
		list(rl_instruction)::out) is semidet.

rl_block__get_next_label(NextLabel, [Instr | Instrs0], Instrs1) :-
	( Instr = label(NextLabel0) - _ ->
		NextLabel = NextLabel0,
		Instrs1 = Instrs0
	;
		rl_block__get_next_label(NextLabel, Instrs0, Instrs1)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% See dragon book p. 670.
	% A node in the flow graph dominates another node if all paths to
	% the second node from the initial node pass through the first.
:- pred rl_block__get_dominator_info(rl_opt_info::in, rl_opt_info::out) is det.

rl_block__get_dominator_info -->
	rl_opt_info_get_flow_graph(FlowGraph),
	{ relation__domain(FlowGraph, Nodes) },
	{ set__delete(Nodes, 0, NonInitialNodes) },
	{ set__to_sorted_list(NonInitialNodes, NonInitialNodeList) },
	{ map__init(Dominators0) },

		% The initial node dominates itself.
	rl_opt_info_get_first_block_id(FirstBlockId),
	{ set__singleton_set(InitialNodeDominators, FirstBlockId) },
	{ set_dominating_nodes(Dominators0, FirstBlockId,
		InitialNodeDominators, Dominators1) },

		% Initially, all non-initial nodes dominate all other
		% non-initial nodes.
	{ Initialise = (pred(Node::in, Dom0::in, Dom::out) is det :-
		 set_dominating_nodes(Dom0, Node, NonInitialNodes, Dom)
	) },
	{ list__foldl(Initialise, NonInitialNodeList,
		Dominators1, Dominators2) },

	{ SinglePass = (pred(Dom0::in, Dom::out, Changed::out) is det :-
		list__foldl(
			rl_block__get_dominator_info_single_pass(FlowGraph),
			NonInitialNodeList, Dom0 - no, Dom - Changed)
	) },
	{ compute_to_fixpoint(SinglePass, Dominators2, Dominators) },
	rl_opt_info_set_dominator_info(Dominators).

:- pred rl_block__get_dominator_info_single_pass(flow_graph::in,
	block_id::in, pair(dominator_info, bool)::in,
	pair(dominator_info, bool)::out) is det.

rl_block__get_dominator_info_single_pass(FlowGraph, Node,
		Dominators0 - Changed0, Dominators - Changed) :-
	relation__lookup_element(FlowGraph, Node, NodeKey),
	relation__lookup_to(FlowGraph, NodeKey, Predecessors0),
	set__to_sorted_list(Predecessors0, Predecessors),
		lookup_dominating_nodes(Dominators0, Node, Value0),
	(
		Predecessors = [PredKey | PredKeys],
		relation__lookup_key(FlowGraph, PredKey, Pred),
		lookup_dominating_nodes(Dominators0, Pred,
			DomByPredecessors0),
		IntersectPredecessors = (pred(PredecessorKey::in, Inter0::in,
				Inter::out) is det :-
			relation__lookup_key(FlowGraph, PredecessorKey,
				Predecessor),
			lookup_dominating_nodes(Dominators0, Predecessor,
				PredecessorDom),
			set__intersect(Inter0, PredecessorDom, Inter)
		),
		list__foldl(IntersectPredecessors, PredKeys,
			DomByPredecessors0, DomByPredecessors),
		set__insert(DomByPredecessors, Node, NewValue)
	;
		Predecessors = [],
		set__singleton_set(NewValue, Node)
	),
	set__difference(Value0, NewValue, Diff),
	( set__empty(Diff) ->
		Changed = Changed0
	;
		Changed = yes
	),
	set_dominating_nodes(Dominators0, Node, NewValue, Dominators).

%-----------------------------------------------------------------------------%

	% Find the back edges in the flow graph, then find the loop for
	% each back edge.
:- pred rl_block__find_loops(rl_opt_info::in, rl_opt_info::out) is det.

rl_block__find_loops -->
	rl_opt_info_get_flow_graph(FlowGraph),
	rl_opt_info_get_dominator_info(DominatorInfo),
	{ relation__to_key_assoc_list(FlowGraph, FlowAssocList) },
		% If the edge is a back edge, find the loop that it closes,
		% otherwise fail.
	{ BackEdgeToLoop = (pred(GraphEdge::in, Loop::out) is semidet :-
		GraphEdge = CallingKey - CalledKey,
		relation__lookup_key(FlowGraph, CallingKey, CallingNode),
		relation__lookup_key(FlowGraph, CalledKey, CalledNode),
		lookup_dominating_nodes(DominatorInfo, CallingNode,
			CallingNodeDominators),
		set__member(CalledNode, CallingNodeDominators),
		( CallingNode = CalledNode ->
			set__singleton_set(LoopNodes, CallingNode)
		;
			set__list_to_set([CallingNode, CalledNode],
				LoopNodes0),
			queue__init(NodesToProcess0),
			queue__put(NodesToProcess0, CallingNode,
				NodesToProcess1),
			rl_block__get_loop(FlowGraph, DominatorInfo,
				NodesToProcess1, LoopNodes0, LoopNodes)
		),
		Loop = loop(CalledNode, LoopNodes)
	) },
	{ list__filter_map(BackEdgeToLoop, FlowAssocList, Loops) },
	rl_opt_info_set_loops(Loops).

:- pred rl_block__get_loop(flow_graph::in, dominator_info::in,
	queue(block_id)::in, set(block_id)::in, set(block_id)::out) is det.

rl_block__get_loop(FlowGraph, DominatorInfo, NodeQueue0,
		LoopNodes0, LoopNodes) :-
	( queue__get(NodeQueue0, Node, NodeQueue1) ->
		relation__lookup_element(FlowGraph, Node, NodeKey),
		relation__lookup_to(FlowGraph, NodeKey, Predecessors),
		set__to_sorted_list(Predecessors, PredecessorList),
		HandlePredecessor = (pred(PredecessorKey::in, Info0::in,
		    		Info::out) is det :-
		    	Info0 = Nodes0 - Queue0,
			relation__lookup_key(FlowGraph, PredecessorKey,
				Predecessor),
			( set__member(Predecessor, Nodes0) ->
				Nodes = Nodes0,
				Queue = Queue0
			;
				set__insert(Nodes0, Predecessor, Nodes),
				queue__put(Queue0, Predecessor, Queue)
			),
			Info = Nodes - Queue
		),
		list__foldl(HandlePredecessor, PredecessorList,
			LoopNodes0 - NodeQueue1, LoopNodes1 - NodeQueue2),
		rl_block__get_loop(FlowGraph, DominatorInfo, NodeQueue2,
			LoopNodes1, LoopNodes)
	;
		LoopNodes = LoopNodes0
	).

%-----------------------------------------------------------------------------%

:- pred compute_to_fixpoint(pred(T, T, bool), T, T).
:- mode compute_to_fixpoint(pred(in, out, out) is det, in, out) is det.

compute_to_fixpoint(Pred, T0, T) :-
	call(Pred, T0, T1, Changed),
	( Changed = yes ->
		compute_to_fixpoint(Pred, T1, T)
	;
		T = T1
	).

%-----------------------------------------------------------------------------%

rl_block__get_nonlocal_relations(NonLocals) -->
	rl_opt_info_get_rev_block_order(Blocks),
	{ set__init(SingleBlockRels0) },
	rl_opt_info_get_input_relations(InputRels),
	rl_opt_info_get_output_relations(OutputRels),
	rl_opt_info_get_memoed_relations(MemoedRels),
	{ set__insert_list(MemoedRels, InputRels, NonLocals0) },
	{ set__insert_list(NonLocals0, OutputRels, NonLocals1) },
	list__foldl2(rl_block__update_nonlocal_relations, Blocks,
		SingleBlockRels0 - NonLocals1, _ - NonLocals).

:- pred rl_block__update_nonlocal_relations(block_id::in,
		pair(set(relation_id))::in, pair(set(relation_id))::out,
		rl_opt_info::in, rl_opt_info::out) is det.

rl_block__update_nonlocal_relations(BlockId, Single0 - Multi0, Single - Multi)
		-->
	rl_opt_info_get_block(BlockId, Block),
	{ Block = block(_, Instrs, MaybeBranch, _) },
	{ AddInstrRels = (pred(Instr::in, Rels0::in, Rels::out) is det :-
			rl__instr_relations(Instr, Inputs, Outputs),
			set__insert_list(Rels0, Inputs, Rels1),
			set__insert_list(Rels1, Outputs, Rels)
		) },
	{ set__init(BlockRels0) },
	{ list__foldl(AddInstrRels, Instrs, BlockRels0, BlockRels1) },
	{ MaybeBranch = yes(Branch) ->
		call(AddInstrRels, Branch, BlockRels1, BlockRels)
	;
		BlockRels = BlockRels1
	},
	{ set__intersect(Single0, BlockRels, NewMulti) },
	{ set__difference(BlockRels, Single0, NewSingle0) },
	{ set__difference(NewSingle0, Multi0, NewSingle) },
	{ set__union(Single0, NewSingle, Single) },
	{ set__union(Multi0, NewMulti, Multi) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- type rl_opt_info.

:- type block_id	==	int.

:- type block
	---> block(
		maybe(label_id),	% label starting the block
		list(rl_instruction),
		maybe(rl_instruction),	% branch instruction ending the block.
		block_info
	).

:- type block_info
	---> block_info(
		set(relation_id),	% relations live on entry
		set(relation_id)	% relations live on exit
	).

:- type block_map	==	map(block_id, block).
:- type label_map	==	map(label_id, block_id).

:- type flow_graph	==	relation(block_id).

	% Nodes which dominate a given node. A node dominates another
	% node if all paths to the second node from the initial node
	% pass through the first.
:- type dominator_info	==	map(block_id, set(block_id)).

:- type loop
	---> loop(
		block_id,	% entry block
		set(block_id)	% all blocks
	).

%-----------------------------------------------------------------------------%

:- pred rl_opt_info_get_next_block_id_no_update(block_id::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_relation_info_map(relation_info_map::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_highest_label_id(label_id::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_first_block_id(block_id::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_block_map(block_map::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_module_info(module_info::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_label_map(label_map::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_flow_graph(flow_graph::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_dominator_info(dominator_info::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_loops(list(loop)::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_rev_block_order(list(block_id)::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_last_block_id(block_id::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_input_relations(list(relation_id)::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_output_relations(list(relation_id)::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_memoed_relations(set(relation_id)::out,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_get_next_relation_id(relation_id::out,
		rl_opt_info::in, rl_opt_info::out) is det.

:- pred rl_opt_info_set_relation_info_map(relation_info_map::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_highest_label_id(label_id::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_first_block_id(block_id::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_block_map(block_map::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_module_info(module_info::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_label_map(label_map::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_flow_graph(flow_graph::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_dominator_info(dominator_info::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_loops(list(loop)::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_rev_block_order(list(block_id)::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_last_block_id(block_id::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_input_relations(list(relation_id)::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_output_relations(list(relation_id)::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_memoed_relations(set(relation_id)::in,
		rl_opt_info::in, rl_opt_info::out) is det.
:- pred rl_opt_info_set_next_relation_id(relation_id::in,
		rl_opt_info::in, rl_opt_info::out) is det.

:- pred rl_opt_info_get_block(block_id::in, block::out,
		rl_opt_info::in, rl_opt_info::out) is det.

:- pred rl_opt_info_set_block(block_id::in, block::in,
		rl_opt_info::in, rl_opt_info::out) is det.

:- pred rl_opt_info_get_new_label(label_id::out, block_id::out,
		rl_opt_info::in, rl_opt_info::out) is det.

:- pred rl_opt_info_add_flow_graph_arc(block_id::in, block_id::in,
		rl_opt_info::in, rl_opt_info::out) is det.

:- pred rl_opt_info_add_block(block_id::in, block::in,
		rl_opt_info::in, rl_opt_info::out) is det.

:- pred rl_opt_info_get_relation_info(relation_id::in, relation_info::out,
		rl_opt_info::in, rl_opt_info::out) is det.

:- pred rl_opt_info_set_relation_info(relation_id::in, relation_info::in,
		rl_opt_info::in, rl_opt_info::out) is det.

:- pred rl_opt_info_add_relation(list(mer_type)::in,
		relation_id::out, rl_opt_info::in, rl_opt_info::out) is det.

:- pred block_info_init(block_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

	% Information about a procedure to be optimized.
:- type rl_opt_info
	--->	rl_opt_info(
			block_id,			% next block id
			relation_info_map,
			label_id,			% next label_id
			block_id,			% first block_id
			block_map,
			module_info,
			label_map,
			flow_graph,
			dominator_info,
			list(loop),
			list(block_id),			% blocks in
							% reverse order
			block_id,			% last block_id
			list(relation_id),		% inputs
			list(relation_id),		% outputs
			set(relation_id),		% memoed relations
			relation_id,			% next relation_id
			unit
		).

rl_opt_info_get_next_block_id_no_update(A, Info, Info) :-
	Info = rl_opt_info(A,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).
rl_opt_info_get_relation_info_map(B, Info, Info) :-
	Info = rl_opt_info(_,B,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).
rl_opt_info_get_highest_label_id(C, Info, Info) :-
	Info = rl_opt_info(_,_,C,_,_,_,_,_,_,_,_,_,_,_,_,_,_).
rl_opt_info_get_first_block_id(D, Info, Info) :-
	Info = rl_opt_info(_,_,_,D,_,_,_,_,_,_,_,_,_,_,_,_,_).
rl_opt_info_get_block_map(E, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,E,_,_,_,_,_,_,_,_,_,_,_,_).
rl_opt_info_get_module_info(F, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,F,_,_,_,_,_,_,_,_,_,_,_).
rl_opt_info_get_label_map(G, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,G,_,_,_,_,_,_,_,_,_,_).
rl_opt_info_get_flow_graph(H, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,_,H,_,_,_,_,_,_,_,_,_).
rl_opt_info_get_dominator_info(I, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,_,_,I,_,_,_,_,_,_,_,_).
rl_opt_info_get_loops(J, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,_,_,_,J,_,_,_,_,_,_,_).
rl_opt_info_get_rev_block_order(K, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,_,_,_,_,K,_,_,_,_,_,_).
rl_opt_info_get_last_block_id(L, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,_,_,_,_,_,L,_,_,_,_,_).
rl_opt_info_get_input_relations(M, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,_,_,_,_,_,_,M,_,_,_,_).
rl_opt_info_get_output_relations(N, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,_,_,_,_,_,_,_,N,_,_,_).
rl_opt_info_get_memoed_relations(O, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,O,_,_).
rl_opt_info_get_next_relation_id(P, Info, Info) :-
	Info = rl_opt_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,P,_).

rl_opt_info_set_relation_info_map(B, Info0, Info) :-
	Info0 = rl_opt_info(A,_,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_highest_label_id(C, Info0, Info) :-
	Info0 = rl_opt_info(A,B,_,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_first_block_id(D, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,_,E,F,G,H,I,J,K,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_block_map(E, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,_,F,G,H,I,J,K,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_module_info(F, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,_,G,H,I,J,K,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_label_map(G, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,_,H,I,J,K,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_flow_graph(H, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,G,_,I,J,K,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_dominator_info(I, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,G,H,_,J,K,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_loops(J, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,G,H,I,_,K,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_rev_block_order(K, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,G,H,I,J,_,L,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_last_block_id(L, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,_,M,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_input_relations(M, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,_,N,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_output_relations(N, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,_,O,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_memoed_relations(O, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,_,P,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).
rl_opt_info_set_next_relation_id(P, Info0, Info) :-
	Info0 = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,_,Q),
	Info = rl_opt_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).

%-----------------------------------------------------------------------------%

:- pred rl_opt_info_init(module_info::in, relation_info_map::in,
		list(relation_id)::in, list(relation_id)::in,
		set(relation_id)::in, rl_opt_info::out, block_id::out) is det.

rl_opt_info_init(ModuleInfo, RelationInfoMap, Inputs, Outputs,
		MemoedRels, Info, FirstBlockId) :-
	map__init(BlockMap0),
	map__init(LabelMap0),
	map__init(DominatorInfo0),
	relation__init(FlowGraph0),
	FirstBlockId = 0,

	map__sorted_keys(RelationInfoMap, RelationIds),
	( list__last(RelationIds, HighestRelationId) ->
		NextRelationId = HighestRelationId + 1
	;
		NextRelationId = 0
	),

	Info = rl_opt_info(FirstBlockId, RelationInfoMap, 0, 0, BlockMap0,
		ModuleInfo, LabelMap0, FlowGraph0, DominatorInfo0, [], [], 0,
		Inputs, Outputs, MemoedRels, NextRelationId, unit).

:- pred rl_opt_info_get_next_block_id(block_id::out,
		rl_opt_info::in, rl_opt_info::out) is det.

rl_opt_info_get_next_block_id(BlockId1, Info0, Info) :-
	Info0 = rl_opt_info(BlockId,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
	BlockId1 = BlockId + 1,
	Info = rl_opt_info(BlockId1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).

%-----------------------------------------------------------------------------%

:- pred lookup_dominating_nodes(dominator_info::in, block_id::in,
		set(block_id)::out) is det.

lookup_dominating_nodes(Dominators, BlockId, Nodes) :-
	( map__search(Dominators, BlockId, Nodes1) ->
		Nodes = Nodes1
	;
		set__init(Nodes)
	).

:- pred set_dominating_nodes(dominator_info::in, block_id::in,
		set(block_id)::in, dominator_info::out) is det.

set_dominating_nodes(Dominators0, BlockId, Nodes, Dominators) :-
	map__set(Dominators0, BlockId, Nodes, Dominators).

%-----------------------------------------------------------------------------%

:- pred rl_opt_info_add_label(label_id::in, block_id::out,
		rl_opt_info::in, rl_opt_info::out) is det.

rl_opt_info_add_label(LabelId, LabelBlockId) -->
	rl_opt_info_get_label_map(LabelMap0),
	( { map__search(LabelMap0, LabelId, LabelBlockId1) } ->
		{ LabelBlockId = LabelBlockId1 }
	;
		rl_opt_info_get_next_block_id(LabelBlockId),
		{ map__det_insert(LabelMap0, LabelId,
			LabelBlockId, LabelMap) },
		rl_opt_info_set_label_map(LabelMap)
	),
	rl_opt_info_get_highest_label_id(TopLabelId0),
	( { LabelId > TopLabelId0 } ->
		rl_opt_info_set_highest_label_id(LabelId)
	;
		[]
	).

%-----------------------------------------------------------------------------%

rl_opt_info_get_block(BlockId, Block) -->
	rl_opt_info_get_block_map(BlockMap),
	{ map__lookup(BlockMap, BlockId, Block) }.

rl_opt_info_set_block(BlockId, Block) -->
	rl_opt_info_get_block_map(BlockMap0),
	{ map__set(BlockMap0, BlockId, Block, BlockMap) },
	rl_opt_info_set_block_map(BlockMap).

%-----------------------------------------------------------------------------%

rl_opt_info_get_new_label(Label, Block) -->
	rl_opt_info_get_next_block_id(Block),
	rl_opt_info_get_highest_label_id(Label0),
	{ Label = Label0 + 1 },
	rl_opt_info_set_highest_label_id(Label).

%-----------------------------------------------------------------------------%

rl_opt_info_get_relation_info(RelationId, RelationInfo) -->
	rl_opt_info_get_relation_info_map(RelMap0),
	{ map__lookup(RelMap0, RelationId, RelationInfo) }.

rl_opt_info_set_relation_info(RelationId, RelationInfo) -->
	rl_opt_info_get_relation_info_map(RelMap0),
	{ map__set(RelMap0, RelationId, RelationInfo, RelMap) },
	rl_opt_info_set_relation_info_map(RelMap).

rl_opt_info_add_relation(Schema, RelationId) -->
	rl_opt_info_get_next_relation_id(RelationId),
	{ NextRelationId = RelationId + 1 },
	rl_opt_info_set_next_relation_id(NextRelationId),
	{ rl__relation_id_to_string(RelationId, RelName) },
	rl_opt_info_get_module_info(ModuleInfo),
	{ rl__default_temporary_state(ModuleInfo, State) },
	{ RelationInfo = relation_info(temporary(State),
		Schema, [], RelName) },
	rl_opt_info_get_relation_info_map(RelMap0),
	{ map__det_insert(RelMap0, RelationId, RelationInfo, RelMap) },
	rl_opt_info_set_relation_info_map(RelMap).

%-----------------------------------------------------------------------------%

rl_opt_info_add_flow_graph_arc(CallingBlock, CalledBlock) -->
	rl_opt_info_get_flow_graph(FlowGraph0),
	{ relation__add_element(FlowGraph0, CallingBlock,
		CallingBlockKey, FlowGraph1) },
	{ relation__add_element(FlowGraph1, CalledBlock,
		CalledBlockKey, FlowGraph2) },
	{ relation__add(FlowGraph2, CallingBlockKey,
		CalledBlockKey, FlowGraph) },
	rl_opt_info_set_flow_graph(FlowGraph).

%-----------------------------------------------------------------------------%

rl_opt_info_add_block(BlockId, Block) -->
	rl_opt_info_get_block_map(BlockMap0),
	{ map__det_insert(BlockMap0, BlockId, Block, BlockMap) },
	rl_opt_info_set_block_map(BlockMap),
	rl_opt_info_get_rev_block_order(BlockOrder0),
	rl_opt_info_set_rev_block_order([BlockId | BlockOrder0]).

%-----------------------------------------------------------------------------%

rl_block__get_proc(Info0, ProcName, MercuryProcs, Proc) :-
	rl_opt_info_get_rev_block_order(RevOrder, Info0, Info1),
	list__reverse(RevOrder, Order),
	rl_opt_info_get_block_map(BlockMap, Info1, Info2),
	GetInstrs = (pred(BlockId::in, Instrs::out) is det :-
		map__lookup(BlockMap, BlockId,
			block(MaybeLabel, Instrs0, MaybeBranch, _)),
		( MaybeBranch = yes(Branch) ->
			list__append(Instrs0, [Branch], Instrs1)
		;
			Instrs1 = Instrs0
		),
	    	( MaybeLabel = yes(Label) ->
			Instrs = [label(Label) - "" | Instrs1]
		;
			Instrs = Instrs1
		)
	),
	list__map(GetInstrs, Order, InstrLists),
	list__condense(InstrLists, Instructions),
	rl_opt_info_get_input_relations(Inputs, Info2, Info3),
	rl_opt_info_get_output_relations(Outputs, Info3, Info4),
	rl_opt_info_get_memoed_relations(Memoed, Info4, Info5),
	rl_opt_info_get_relation_info_map(RelMap, Info5, _),
	Proc = rl_proc(ProcName, Inputs, Outputs, Memoed,
		RelMap, Instructions, MercuryProcs).

%-----------------------------------------------------------------------------%

rl_block__dump_blocks(no, Info, Info) --> [].
rl_block__dump_blocks(yes, Info0, Info) -->
	{ rl_opt_info_get_rev_block_order(RevBlockOrder, Info0, Info1) },
	{ list__reverse(RevBlockOrder, BlockOrder) },
	{ rl_opt_info_get_block_map(BlockMap, Info1, Info2) },
	{ rl_opt_info_get_relation_info_map(RelInfo, Info2, Info3) },
	{ rl_opt_info_get_module_info(ModuleInfo, Info3, Info) },
	list__foldl(rl_block__dump_block(BlockMap, RelInfo, ModuleInfo),
		BlockOrder).

:- pred rl_block__dump_block(block_map::in, relation_info_map::in,
	module_info::in, block_id::in, io__state::di, io__state::uo) is det.

rl_block__dump_block(BlockMap, RelMap, ModuleInfo, BlockId) -->
	io__format("Block %i:\n", [i(BlockId)]),
	{ map__lookup(BlockMap, BlockId, Block) },

	{ Block = block(MaybeLabel, Instrs, MaybeBranch, BlockInfo) },
	( { MaybeLabel = yes(Label) } ->
		rl_dump__write_instruction(ModuleInfo,
			RelMap, label(Label) - "")
	;
		[]
	),
	io__write_list(Instrs, "",
		rl_dump__write_instruction(ModuleInfo, RelMap)),
	( { MaybeBranch = yes(Branch) } ->
		rl_dump__write_instruction(ModuleInfo, RelMap, Branch)
	;
		[]
	),

	{ BlockInfo = block_info(LiveAtStart, LiveAtEnd) },
	io__write_string("Live at start: "),
	io__write(LiveAtStart),
	io__nl,
	io__write_string("Live at end: "),
	io__write(LiveAtEnd),
	io__nl,
	io__nl.

:- pred rl_block__dump_flow_graph(bool::in, rl_opt_info::in,
		rl_opt_info::out, io__state::di, io__state::uo) is det.

rl_block__dump_flow_graph(no, Info, Info) --> [].
rl_block__dump_flow_graph(yes, Info0, Info) -->
	{ rl_opt_info_get_flow_graph(Graph, Info0, Info1) },
	{ rl_opt_info_get_rev_block_order(RevOrder, Info1, Info) },
	{ list__reverse(RevOrder, Order) },
	io__write_string("Flow graph:\n"),
	list__foldl(rl_block__dump_flow_graph_2(Graph), Order).

:- pred rl_block__dump_flow_graph_2(flow_graph::in, block_id::in,
		io__state::di, io__state::uo) is det.

rl_block__dump_flow_graph_2(Graph, Block) -->
	{ relation__lookup_element(Graph, Block, BlockKey) },
	{ relation__lookup_from(Graph, BlockKey, CalledKeys0) },
	{ set__to_sorted_list(CalledKeys0, CalledKeys) },
	{ list__map(relation__lookup_key(Graph), CalledKeys, CalledBlocks) },
	io__write_int(Block),
	io__write_string(": "),
	io__write_list(CalledBlocks, ", ", io__write_int),
	io__nl.

:- pred rl_block__dump_loops(bool::in, list(loop)::in,
		io__state::di, io__state::uo) is det.

rl_block__dump_loops(no, _) --> [].
rl_block__dump_loops(yes, Loops) -->
	io__write_list(Loops, "\n", rl_block__dump_loop),
	io__nl.

:- pred rl_block__dump_loop(loop::in, io__state::di, io__state::uo) is det.

rl_block__dump_loop(loop(Entry, Blocks0)) -->
	io__format("Loop entry: %i. Nodes: [", [i(Entry)]),
	{ set__to_sorted_list(Blocks0, Blocks) },
	io__write_list(Blocks, ", ", io__write_int),
	io__write_string("]\n").

%-----------------------------------------------------------------------------%

block_info_init(block_info(Rels, Rels)) :-
	set__init(Rels).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
