%-----------------------------------------------------------------------------%
% Copyright (C) 1998, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_loop.m
% Main author: stayl
%
% Detect and move loop invariant instructions.
% As described in the dragon book -
% 	Aho, Sethi and Ullman,
% 	Compilers: principles, techniques and tools,
%	Addison-Wesley, 1986.
%-----------------------------------------------------------------------------%

:- module aditi_backend__rl_loop.

:- interface.

:- import_module aditi_backend__rl_block.

	% Given the flow graph for a procedure, return a new flow 
	% graph with loop invariant instructions moved out of loops.
:- pred rl_loop__shift_invariants(rl_opt_info::in, rl_opt_info::out) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module aditi_backend__rl.

:- import_module assoc_list, bimap, bool, int, list, map, queue, relation.
:- import_module require, set, std_util.

rl_loop__shift_invariants -->
	rl_opt_info_get_loops(Loops0),
	% Do the outer loops first, so that we move instructions as far
	% as they can go in one jump. If a variable is an invariant of
	% an outer loop, it must be invariant in any inner loops of that
	% outer loop.
	{ CompareLoops = 
	    lambda([Loop1::in, Loop2::in, Compare::out] is det, (
	    	Loop1 = loop(_, Nodes1),
		Loop2 = loop(_, Nodes2),
		set__to_sorted_list(Nodes1, Nodes1List),
		list__length(Nodes1List, Size1),
		set__to_sorted_list(Nodes2, Nodes2List),
		list__length(Nodes2List, Size2),
		% Sort in descending order on size.
		compare(Compare, Size2, Size1)
	    )) },
	{ list__sort(CompareLoops, Loops0, SortedLoops) },
	rl_loop__shift_invariants_2(SortedLoops, [], Loops),
	rl_opt_info_set_loops(Loops).

:- pred rl_loop__shift_invariants_2(list(loop)::in, list(loop)::in,
		list(loop)::out, rl_opt_info::in, rl_opt_info::out) is det.

rl_loop__shift_invariants_2([], Loops, Loops) --> [].
rl_loop__shift_invariants_2([Loop0 | LoopsToProcess0], 
		ProcessedLoops0, ProcessedLoops) --> 
	{ Loop0 = loop(EntryNode, Nodes) },

		% We can only remove invariant computation if it is performed
		% on every execution of the loop, i.e. the block containing
		% it dominates all the exit points.
	rl_opt_info_get_flow_graph(FlowGraph),
	{ IsExitPoint = 
		lambda([Node::in] is semidet, (
			relation__lookup_element(FlowGraph, Node, NodeKey),
			relation__lookup_from(FlowGraph, NodeKey,
				CalledNodeKeys),
			set__member(CalledNodeKey, CalledNodeKeys),
			relation__lookup_key(FlowGraph,
				CalledNodeKey, CalledNode),
			\+ set__member(CalledNode, Nodes)
		)) },
	{ set__to_sorted_list(Nodes, NodeList) },
	{ list__filter(IsExitPoint, NodeList, ExitNodes) },
	rl_opt_info_get_dominator_info(DominatorInfo), 
	{
		ExitNodes = [ExitNode | ExitNodes1],
		map__lookup(DominatorInfo, ExitNode, ExitDominatingNodes0),
		IntersectDominators =
		    lambda([N::in, Inter0::in, Inter::out] is det, (
		    	map__lookup(DominatorInfo, N, DominatingNodes1),
			set__intersect(Inter0, DominatingNodes1, Inter)
		    )),
		list__foldl(IntersectDominators, ExitNodes1, 
			ExitDominatingNodes0, ExitDominatingNodes)
	;
		ExitNodes = [],
		error("rl_loop__shift_invariants: loop has no exit nodes")
	},

	{ set__init(OneBlockRels0) },
	{ set__init(ManyBlockRels0) },

	% Pick out relations which are only used in one block within the
	% loop. Since rl_gen.m only generates loops with one block,
	% this isn't a problem.
	rl_loop__get_nonlocal_rels(NodeList, OneBlockRels0, 
		_OneBlockRels, ManyBlockRels0, ManyBlockRels),

	rl_loop__shift_invariants_loop(NodeList, NodeList, ExitDominatingNodes,
		ManyBlockRels, _, no, MaybeHeader),
	rl_loop__maybe_add_header(MaybeHeader, Nodes, EntryNode, 
		ProcessedLoops0, ProcessedLoops1, 
		LoopsToProcess0, LoopsToProcess),
	rl_loop__shift_invariants_2(LoopsToProcess, 
		[Loop0 | ProcessedLoops1], ProcessedLoops).

%-----------------------------------------------------------------------------%

	% Given a list of blocks, pick out the relations that occur
	% in more than one of the blocks.
:- pred rl_loop__get_nonlocal_rels(list(block_id)::in,
	set(relation_id)::in, set(relation_id)::out, set(relation_id)::in,
	set(relation_id)::out, rl_opt_info::in, rl_opt_info::out) is det.

rl_loop__get_nonlocal_rels([], OneBlock, OneBlock,
		ManyBlock, ManyBlock) --> [].
rl_loop__get_nonlocal_rels([Block | Blocks], OneBlock0, OneBlock, 
		ManyBlock0, ManyBlock) -->
	rl_opt_info_get_block(Block, block(_, Instrs, _, _)),
	{ GetInstrRelations = 
		lambda([Instr::in, Rels0::in, Rels::out] is det, (
			rl__instr_relations(Instr, Inputs, Outputs),
			set__insert_list(Rels0, Inputs, Rels1),
			set__insert_list(Rels1, Outputs, Rels)
		)) },
	{ set__init(BlockRels0) },
	{ list__foldl(GetInstrRelations, Instrs, BlockRels0, BlockRels1) },

	{ Update =
	    lambda([Rel::in, Changed0::in, Changed::out] is det, (
		Changed0 = One0 - Many0,
		( set__member(Rel, Many0) ->
			Many = Many0,
			One = One0
		; set__member(Rel, One0) ->
			set__delete(One0, Rel, One),
			set__insert(Many0, Rel, Many)
		;
			set__insert(One0, Rel, One),
			Many = Many0
		),
		Changed = One - Many
	    )) },
	{ set__to_sorted_list(BlockRels1, BlockRels) },
	{ list__foldl(Update, BlockRels,
		OneBlock0 - ManyBlock0, OneBlock1 - ManyBlock1) },
	rl_loop__get_nonlocal_rels(Blocks, OneBlock1, OneBlock, 
		ManyBlock1, ManyBlock).

%-----------------------------------------------------------------------------%

:- pred rl_loop__shift_invariants_loop(list(block_id)::in, list(block_id)::in,
	set(block_id)::in, set(relation_id)::in, set(relation_id)::out,
	maybe(block)::in, maybe(block)::out, rl_opt_info::in,
	rl_opt_info::out) is det.

rl_loop__shift_invariants_loop([], _, _, N, N, H, H) --> [].
rl_loop__shift_invariants_loop([LoopNode | LoopNodes], AllLoopNodes,
		ExitDominatingNodes, NonLocalRels0, NonLocalRels,
		MaybeHeader0, MaybeHeader) -->
	rl_opt_info_get_block(LoopNode, Block0),
	{ Block0 = block(Label, Instrs0, Branch, BlockInfo) },
	{ GetChangedRels = 
		lambda([Instr::in, Rels0::in, Rels::out] is det, (
			rl__instr_relations(Instr, _, Outputs),
			set__insert_list(Rels0, Outputs, Rels)
		)) },
	{ set__init(BlockChangedRels0) },
	{ list__foldl(GetChangedRels, Instrs0, 
		BlockChangedRels0, BlockChangedRels) },
	(
		% Can't shift invariants if this block doesn't dominate
		% all the exits to the loop (i.e. isn't executed on every
		% pass through the loop).
		{ set__member(LoopNode, ExitDominatingNodes) },

		% Don't bother processing the block if it doesn't contain
		% any relations which are changed in only one block within
		% the loop.

		{ set__difference(BlockChangedRels, NonLocalRels0, 
			ThisBlockRels) },
		{ \+ set__empty(ThisBlockRels) }
	->
		{ set__init(SeenRels) },
		{ set__init(ShiftedRels0) },
		rl_loop__shift_invariants_block(Instrs0, Instrs, LoopNode,
			AllLoopNodes, ExitDominatingNodes, BlockChangedRels,
			NonLocalRels0, NonLocalRels1, SeenRels, 
			ShiftedRels0, _ShiftedRels, MaybeHeader0, MaybeHeader1),
		rl_opt_info_set_block(LoopNode,
			block(Label, Instrs, Branch, BlockInfo))
	;
		{ MaybeHeader1 = MaybeHeader0 },
		{ NonLocalRels1 = NonLocalRels0 }
	),
	rl_loop__shift_invariants_loop(LoopNodes, AllLoopNodes,
		ExitDominatingNodes, NonLocalRels1, NonLocalRels,
		MaybeHeader1, MaybeHeader).

%-----------------------------------------------------------------------------%

:- pred rl_loop__shift_invariants_block(list(rl_instruction)::in, 
	list(rl_instruction)::out, block_id::in, list(block_id)::in,
	set(block_id)::in, set(relation_id)::in, set(relation_id)::in, 
	set(relation_id)::out, set(relation_id)::in, 
	set(relation_id)::in, set(relation_id)::out, maybe(block)::in,
	maybe(block)::out, rl_opt_info::in, rl_opt_info::out) is det.

rl_loop__shift_invariants_block([], [], _, _, _, _, NL, NL, _, 
		S, S, H, H) --> [].
rl_loop__shift_invariants_block([Instr | Instrs0], Instrs, LoopNode,
		AllLoopNodes, ExitDominatingNodes, BlockChangedRels,
		NonLocalRels0, NonLocalRels, SeenRels0, 
		ShiftedRels0, ShiftedRels, MaybeHeader0, MaybeHeader) -->
	{ rl__instr_relations(Instr, InputRels, OutputRels) },
	(
		%
		% Check if this instruction is invariant.
		%

		{ \+ rl_loop__unmovable(Instr) },

		% Are all the inputs produced outside the loop.
		{ \+ (
			list__member(InputRel, InputRels),
			\+ set__member(InputRel, ShiftedRels0),
			( set__member(InputRel, BlockChangedRels)
			; set__member(InputRel, NonLocalRels0)
			; set__member(InputRel, SeenRels0)
			)
		) },

		% Is there no use of one of the output relations within
		% the block which could be reached by some other definition 
		% of the output.
		{ \+ (
			list__member(OutputRel, OutputRels),
			set__member(OutputRel, SeenRels0)
		) },
	
		% Check that the outputs of this instruction are the only 
		% values of those output relations used by other instructions
		% within this block.
		{ set__list_to_set(OutputRels, OutputRelSet) },
		{ rl_loop__check_later_uses_in_block(OutputRelSet, 
			Instrs0, Instrs1) }
	->
		{ rl_loop__add_instruction_to_pre_header(MaybeHeader0, 
			Instr, MaybeHeader1) },
		{ set__insert_list(SeenRels0, InputRels, SeenRels1) },
		{ set__insert_list(SeenRels1, OutputRels, SeenRels) },
		{ set__insert_list(ShiftedRels0, InputRels, ShiftedRels1) },
		{ set__union(ShiftedRels1, OutputRelSet, ShiftedRels2) }, 
		rl_loop__shift_invariants_block(Instrs1, Instrs, LoopNode, 
			AllLoopNodes, ExitDominatingNodes, BlockChangedRels,
			NonLocalRels0, NonLocalRels, SeenRels, 
			ShiftedRels2, ShiftedRels, MaybeHeader1, MaybeHeader)
	;
		{ set__insert_list(SeenRels0, InputRels, SeenRels1) },
		{ set__insert_list(SeenRels1, OutputRels, SeenRels) },
		rl_loop__shift_invariants_block(Instrs0, Instrs1, LoopNode, 
			AllLoopNodes, ExitDominatingNodes, BlockChangedRels,
			NonLocalRels0, NonLocalRels, SeenRels, 
			ShiftedRels0, ShiftedRels, MaybeHeader0, MaybeHeader),
		{ Instrs = [Instr | Instrs1] }
	).

%-----------------------------------------------------------------------------%

:- pred rl_loop__unmovable(rl_instruction::in) is semidet.

rl_loop__unmovable(label(_) - _).
rl_loop__unmovable(goto(_) - _).
rl_loop__unmovable(conditional_goto(_, _) - _).
rl_loop__unmovable(clear(_) - _).
rl_loop__unmovable(unset(_) - _).

%-----------------------------------------------------------------------------%

	% Check that all instructions in the current block that 
	% use any of the outputs of the instruction we are trying to
	% move only use the version produced by that instruction.
:- pred rl_loop__check_later_uses_in_block(set(relation_id)::in, 
	list(rl_instruction)::in, list(rl_instruction)::out) is semidet.	

rl_loop__check_later_uses_in_block(_, [], []).
rl_loop__check_later_uses_in_block(OutputRels, [Instr | Instrs0], Instrs) :-
	rl__instr_relations(Instr, _, InstrOutputs),
	set__list_to_set(InstrOutputs, InstrOutputSet),
	set__intersect(OutputRels, InstrOutputSet, RedefinedRels),
	set__empty(RedefinedRels),
	rl_loop__check_later_uses_in_block(OutputRels, Instrs0, Instrs1),
	Instrs = [Instr | Instrs1].

%-----------------------------------------------------------------------------%

:- pred rl_loop__add_instruction_to_pre_header(maybe(block)::in, 
		rl_instruction::in, maybe(block)::out) is det.

rl_loop__add_instruction_to_pre_header(no, Instr, yes(Block)) :-
	block_info_init(BlockInfo),
	Block = block(no, [Instr], no, BlockInfo).
rl_loop__add_instruction_to_pre_header(yes(Block0), Instr, yes(Block)) :-
	Block0 = block(Label, RevInstrs0, Branch, BlockInfo),
	RevInstrs = [Instr | RevInstrs0],
	Block = block(Label, RevInstrs, Branch, BlockInfo).
	
%-----------------------------------------------------------------------------%

:- pred rl_loop__maybe_add_header(maybe(block)::in, set(block_id)::in,
	block_id::in, list(loop)::in, list(loop)::out, list(loop)::in,
	list(loop)::out, rl_opt_info::in, rl_opt_info::out) is det.

rl_loop__maybe_add_header(no, _, _, P, P, L, L) --> [].
rl_loop__maybe_add_header(yes(HeaderBlock), LoopNodes, EntryNode, 
		ProcessedLoops0, ProcessedLoops, 
		LoopsToProcess0, LoopsToProcess) -->
	rl_opt_info_get_new_label(HeaderLabel, HeaderBlockId),

	% Add a new block.
	{ HeaderBlock = block(Label, RevInstrs, Branch, BlockInfo) },
	{ list__reverse(RevInstrs, Instrs) },
	{ HeaderBlock1 = block(Label, Instrs, Branch, BlockInfo) },
	rl_opt_info_set_block(HeaderBlockId, HeaderBlock1),
	rl_opt_info_get_block(EntryNode, EntryBlock),

	% Add the new block to the ordering.
	rl_opt_info_get_rev_block_order(RevOrder0),
	{ rl_loop__insert_into_order(RevOrder0, HeaderBlockId, 
		EntryNode, RevOrder) },
	rl_opt_info_set_rev_block_order(RevOrder),

	% Fix up the flow graph.
	rl_opt_info_get_flow_graph(FlowGraph0),
	{ relation__lookup_element(FlowGraph0, EntryNode, EntryKey) },
	{ relation__add_element(FlowGraph0, HeaderBlockId, 
		HeaderKey, FlowGraph1) },
	{ relation__add(FlowGraph1, HeaderKey, EntryKey, FlowGraph) },
	rl_opt_info_set_flow_graph(FlowGraph),

	% Add the new block to all loops containing the old header.
	( { EntryBlock = block(yes(EntryLabel), _, _, _) } ->
		{ UpdateLoop = 
		    lambda([L0::in, L::out] is det, (
			L0 = loop(LEntry0, LNodes0),
			( set__member(EntryNode, LNodes0) ->
				set__insert(LNodes0, HeaderBlockId, LNodes)
			;
				LNodes = LNodes0
			),
			L = loop(LEntry0, LNodes)
		    )) },
		{ list__map(UpdateLoop, ProcessedLoops0, ProcessedLoops) },
		{ list__map(UpdateLoop, LoopsToProcess0, LoopsToProcess) },
		rl_loop__update_gotos(LoopNodes, EntryLabel, HeaderLabel, 
			EntryNode, HeaderBlockId)
	;
		{ error("rl_loop__maybe_add_header: loop entry does not have a label") }
	).

%-----------------------------------------------------------------------------%

:- pred rl_loop__insert_into_order(list(block_id)::in, block_id::in,
		block_id::in, list(block_id)::out) is det.

rl_loop__insert_into_order([], _, _, _) :-
	error("rl_loop__insert_into_order").
rl_loop__insert_into_order([Block | RevOrder0], 
		HeaderBlockId, EntryNode, RevOrder) :-
	( Block = EntryNode ->
		RevOrder = [EntryNode, HeaderBlockId | RevOrder0]
	;
		rl_loop__insert_into_order(RevOrder0, HeaderBlockId, 
			EntryNode, RevOrder1),
		RevOrder = [Block | RevOrder1]
	).

%-----------------------------------------------------------------------------%

:- pred rl_loop__update_gotos(set(block_id)::in, label_id::in, label_id::in, 
	block_id::in, block_id::in, rl_opt_info::in, rl_opt_info::out) is det.

rl_loop__update_gotos(LoopNodes, OldLabel, NewLabel,
		OldEntryBlock, NewEntryBlock) -->

	% 
	% Fix up gotos to the loop entry from outside the loop.
	%
	rl_opt_info_get_block_map(BlockMap0),
	{ map__to_assoc_list(BlockMap0, BlockAL0) },
	{ UpdateBlock =
	   lambda([BlockIdAndBlock0::in, BlockIdAndBlock::out] is det, (
	   	BlockIdAndBlock0 = BlockId - block(A, B, MaybeBranch0, D),
		( set__member(BlockId, LoopNodes) ->
		    MaybeBranch = MaybeBranch0
		;
		    ( 
		    	MaybeBranch0 = yes(goto(OldLabel) - Comment) 
		    ->
			MaybeBranch = yes(goto(NewLabel) - Comment)
		    ; 
		    	MaybeBranch0 = 
				yes(conditional_goto(Cond, OldLabel) - Comment)
		    ->
			MaybeBranch = 
				yes(conditional_goto(Cond, NewLabel) - Comment)
		    ;
		        MaybeBranch = MaybeBranch0
		    )
		),
	   	BlockIdAndBlock = BlockId - block(A, B, MaybeBranch, D)
	    )) },
	{ list__map(UpdateBlock, BlockAL0, BlockAL) },
	{ map__from_assoc_list(BlockAL, BlockMap) },
	rl_opt_info_set_block_map(BlockMap),

	% 
	% Fix up the flow graph.
	%
	rl_opt_info_get_flow_graph(FlowGraph0),
	{ relation__lookup_element(FlowGraph0,
		OldEntryBlock, OldEntryBlockKey) },
	{ relation__lookup_element(FlowGraph0, 
		NewEntryBlock, NewEntryBlockKey) },
	{ relation__lookup_to(FlowGraph0, OldEntryBlockKey, 
		CallingBlockKeys) },
	{ set__to_sorted_list(LoopNodes, NodeList) },
	{ list__map(relation__lookup_element(FlowGraph0), 
		NodeList, NodeKeys) },
	{ set__delete_list(CallingBlockKeys, NodeKeys,
		OutsideLoopCallingBlocks) },
	{ set__to_sorted_list(OutsideLoopCallingBlocks, OutsideLoopBlocks) },
	{ list__length(OutsideLoopBlocks, NumBlocks) },
	{ list__duplicate(NumBlocks, OldEntryBlockKey, OldEntryList) },
	{ assoc_list__from_corresponding_lists(OutsideLoopBlocks, OldEntryList, 
		OldEntryAssocList) },	
	{ relation__remove_assoc_list(FlowGraph0, 
		OldEntryAssocList, FlowGraph1) },
	{ list__duplicate(NumBlocks, NewEntryBlockKey, NewEntryList) },
	{ assoc_list__from_corresponding_lists(OutsideLoopBlocks, NewEntryList, 
		NewEntryAssocList) },	
	{ relation__add_assoc_list(FlowGraph1, 
		NewEntryAssocList, FlowGraph) },
	rl_opt_info_set_flow_graph(FlowGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
