%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_stream.m
% Main author: stayl
%
% The output of a relational operation can be used as a stream if it is 
% used only once after it is produced and if indexing is not required.
% A stream is never stored in its entirety - it is produced one tuple at
% a time as needed.
%
% Some operations have both a stream output and one or more materialised
% outputs (union_diff, insert and project). If any of these materialised
% outputs are used before the stream output is traversed in its entirety,
% the stream output must be materialised.
%
% By default, a temporary relation should have status stream. This module
% finds temporary relations which cannot be used as streams and makes
% sure they are materialised.
%
% The algorithm used follows the chain of blocks in the procedure, dividing
% the procedure up into sections containing no back arcs in the flow graph. 
% A relation which becomes live and then dies within that region only
% being used once can be turned into a stream.
%
% This pass should be run last, after rl_liveness.
%
%-----------------------------------------------------------------------------%

:- module aditi_backend__rl_stream.

:- interface.

:- import_module aditi_backend__rl_block.

:- pred rl_stream__detect_streams(rl_opt_info, rl_opt_info).
:- mode rl_stream__detect_streams(in, out) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module aditi_backend__rl.

:- import_module assoc_list, bag, int, list, map, multi_map.
:- import_module relation, require, set, std_util.

:- type stream_info 
	---> stream_info(
		set(relation_id),		% must materialise rels
		bag(relation_id),		% use counts - must be one
						% if the relation is to be
						% streamed.
		relation(relation_id)
						% aliases introduced by `ref'.
	).

%-----------------------------------------------------------------------------%

rl_stream__detect_streams -->
	rl_opt_info_get_rev_block_order(RevOrder),
	{ list__reverse(RevOrder, Order) },
	rl_opt_info_get_input_relations(Inputs),
	rl_opt_info_get_output_relations(Outputs),
	rl_opt_info_get_memoed_relations(Memoed),
	{ set__insert_list(Memoed, Inputs, Materialise0) },
	{ set__insert_list(Materialise0, Outputs, Materialise1) },
	rl_stream__detect_streams_2(Order, Materialise1, Materialise2),
	{ set__to_sorted_list(Materialise2, Materialise) },
	list__foldl(rl_stream__update_must_materialise, Materialise).

:- pred rl_stream__detect_streams_2(list(block_id)::in, set(relation_id)::in,
	set(relation_id)::out, rl_opt_info::in, rl_opt_info::out) is det.

rl_stream__detect_streams_2([], Materialise, Materialise) --> [].
rl_stream__detect_streams_2([Block | Order0], Materialise0, Materialise) -->
	{ bag__init(Uses) },
	{ relation__init(Aliases) },
	{ Info0 = stream_info(Materialise0, Uses, Aliases) },
	rl_opt_info_get_flow_graph(Graph), 
	{ rl_stream__get_blocks_to_back_arc(Graph, [Block | Order0], Order,
		[], BlockList) },
	{ set__list_to_set(BlockList, BlockSet) },
	rl_opt_info_get_block_map(BlockMap),
	{ list__foldl(rl_stream__detect_must_materialise_rels(Graph,
			BlockMap, BlockSet),
		BlockList, Info0, Info1) },
	{ rl_stream__detect_multiple_use_rels(Graph, BlockMap, BlockList, Info1,
		Block, Info1, Info) },
	{ Info = stream_info(Materialise1, _, _) },
	rl_stream__detect_streams_2(Order, Materialise1, Materialise).

	% Collect all blocks in the list up to the instruction which is
	% the source or target of the first back arc in the graph.
:- pred rl_stream__get_blocks_to_back_arc(flow_graph::in, list(block_id)::in,
	list(block_id)::out, list(block_id)::in, list(block_id)::out) is det.

rl_stream__get_blocks_to_back_arc(_, [], [], Blocks0, Blocks) :-
	list__reverse(Blocks0, Blocks).
rl_stream__get_blocks_to_back_arc(Graph, [Block | Order0], Order,
		Blocks0, Blocks) :-
	relation__lookup_element(Graph, Block, BlockKey),
	relation__lookup_to(Graph, BlockKey, CallingBlockKeys),
	relation__lookup_from(Graph, BlockKey, CalledBlockKeys),
	( 
		% Check that a calling block is not later in the sequence,
		% if this is not the first block in the list to check.
		Blocks0 \= [],
		set__member(CallingBlockKey, CallingBlockKeys),
		relation__lookup_key(Graph, CallingBlockKey, CallingBlock),
		list__member(CallingBlock, Order0)
	->
		list__reverse(Blocks0, Blocks),
		Order = [Block | Order0]
	;
		% Check that all called blocks are later in the sequence.
		set__member(CalledBlockKey, CalledBlockKeys),
		relation__lookup_key(Graph, CalledBlockKey, CalledBlock),
		\+ list__member(CalledBlock, [Block | Order0])
	->
		list__reverse([Block | Blocks0], Blocks),
		Order = Order0
	;
		rl_stream__get_blocks_to_back_arc(Graph, Order0, Order,
			[Block | Blocks0], Blocks)
	).

	% Any relations which are live on entry or exit to
	% this block list must be materialised.
:- pred rl_stream__detect_must_materialise_rels(flow_graph::in, block_map::in,
		set(block_id)::in, block_id::in,
		stream_info::in, stream_info::out) is det.

rl_stream__detect_must_materialise_rels(Graph, BlockMap,
		Blocks, BlockId, Info0, Info) :-
	map__lookup(BlockMap, BlockId, Block),

	%
	% Work out which relations are live on exit from this group of blocks.
	%
	relation__lookup_element(Graph, BlockId, BlockKey),
	relation__lookup_to(Graph, BlockKey, CallingBlockKeys0),
	set__to_sorted_list(CallingBlockKeys0, CallingBlockKeys),
	relation__lookup_from(Graph, BlockKey, CalledBlockKeys0),
	set__to_sorted_list(CalledBlockKeys0, CalledBlockKeys),
	list__map(relation__lookup_key(Graph),
		CallingBlockKeys, CallingBlocks),
	set__list_to_set(CallingBlocks, CallingBlockSet),
	set__difference(CallingBlockSet, Blocks, OutsideCallingBlocks0),	
	set__to_sorted_list(OutsideCallingBlocks0, OutsideCallingBlocks),
	list__map(rl_stream__get_final_live_rels(BlockMap),
		OutsideCallingBlocks, FinalLive0), 
	list__condense(FinalLive0, FinalLive),

	rl_stream__add_must_materialise_rels(FinalLive, Info0, Info1),

	%
	% Work out which relations are live on entry to this group of blocks.
	%
	list__map(relation__lookup_key(Graph), CalledBlockKeys, CalledBlocks),
	set__list_to_set(CalledBlocks, CalledBlockSet),
	set__difference(CalledBlockSet, Blocks, OutsideCalledBlocks0),
	set__to_sorted_list(OutsideCalledBlocks0, OutsideCalledBlocks),
	list__map(rl_stream__get_initial_live_rels(BlockMap),
		OutsideCalledBlocks, InitialLive0), 
	list__condense(InitialLive0, InitialLive),
	rl_stream__add_must_materialise_rels(InitialLive, Info1, Info2),

	%
	% Work out which relations are required to be
	% materialised by some instruction.
	%
	Block = block(_, Instrs, MaybeBranch, _),
	AddMustMaterialiseRels =
	    lambda([Instr::in, StreamInfo0::in, StreamInfo::out] is det, (
		rl_stream__must_materialise_rels(Instr, Rels),
		rl_stream__add_must_materialise_rels(Rels,
			StreamInfo0, StreamInfo)
	    )),
	list__foldl(AddMustMaterialiseRels, Instrs, Info2, Info3),
	( MaybeBranch = yes(Branch) ->
		AddMustMaterialiseRels(Branch, Info3, Info)
	;
		Info = Info3
	).

:- pred rl_stream__get_initial_live_rels(block_map::in,
		block_id::in, list(relation_id)::out) is det.

rl_stream__get_initial_live_rels(BlockMap, BlockId, LiveRels) :-
	map__lookup(BlockMap, BlockId,
		block(_, _, _, block_info(LiveRels0, _))),
	set__to_sorted_list(LiveRels0, LiveRels).

:- pred rl_stream__get_final_live_rels(block_map::in,
		block_id::in, list(relation_id)::out) is det.

rl_stream__get_final_live_rels(BlockMap, BlockId, LiveRels) :-
	map__lookup(BlockMap, BlockId,
		block(_, _, _, block_info(_, LiveRels0))),
	set__to_sorted_list(LiveRels0, LiveRels).

:- pred rl_stream__detect_multiple_use_rels(flow_graph::in, block_map::in,
		list(block_id)::in, stream_info::in,
		block_id::in, stream_info::in, stream_info::out) is det.

rl_stream__detect_multiple_use_rels(Graph, BlockMap, BlockIds, InitialInfo,
		BlockId, Info0, Info) :-
	InitialInfo = stream_info(_, Uses, Aliases),
	Info0 = stream_info(Materialise, _, _),
	Info1 = stream_info(Materialise, Uses, Aliases),
	map__lookup(BlockMap, BlockId, Block),
	Block = block(_, Instrs, MaybeBranch, _),
	list__foldl(rl_stream__detect_streams_instr, Instrs, Info1, Info2),
	( MaybeBranch = yes(Branch) ->
		rl_stream__detect_streams_instr(Branch, Info2, Info3)
	;
		Info3 = Info2
	),
	relation__lookup_element(Graph, BlockId, BlockKey),
	relation__lookup_from(Graph, BlockKey, CalledBlockKeys0),
	set__to_sorted_list(CalledBlockKeys0, CalledBlockKeys),
	list__map(relation__lookup_key(Graph),
		CalledBlockKeys, CalledBlocks),
	rl_stream__inside_and_after(CalledBlocks,
		BlockId, BlockIds, InsideLaterCalledBlocks),
	( InsideLaterCalledBlocks = [] ->
		rl_stream__end_block_list(Info3, Info)
	;
		list__foldl(rl_stream__detect_multiple_use_rels(Graph,
				BlockMap, BlockIds, Info3),
			InsideLaterCalledBlocks, Info0, Info)
	).	

	% Find all called blocks inside the set of interest and
	% after the given one -- we don't want to go back around a loop.
:- pred rl_stream__inside_and_after(list(block_id)::in, block_id::in,
		list(block_id)::in, list(block_id)::out) is det.

rl_stream__inside_and_after(CalledBlocks, BlockId,
		BlockIds, InsideLaterCalledBlocks) :-
	(
		list__nth_member_search(BlockIds, BlockId, N),
		N1 is N - 1,
		list__split_list(N1, BlockIds, _, AfterBlockIds0),
		AfterBlockIds0 = [BlockId | AfterBlockIds]
	->
		list__filter(lambda([CalledBlock::in] is semidet, (
				list__member(CalledBlock, AfterBlockIds)
			)), CalledBlocks, InsideLaterCalledBlocks)
	;
		error("rl_stream__inside_and_after")
	).

%-----------------------------------------------------------------------------%

:- pred rl_stream__detect_streams_instr(rl_instruction::in,
		stream_info::in, stream_info::out) is det.

rl_stream__detect_streams_instr(Instr) -->
	(
		{ Instr = ref(Output, Input) - _ }
	->
		rl_stream__add_alias(Output, Input)
	;
		{ Instr = join(output_rel(Output, _), Input1, Input2,
			_, _, _, Trivial) - "" },
		{ Trivial = yes(
			trivial_join_or_subtract_info(ReturnedTuple, no)) }
	->
		% For a trivial join with no projection,
		% a reference may be taken to the relation
		% on which the join condition depends.
		(
			{ ReturnedTuple = one },
			rl_stream__add_alias(Output, Input1),
			rl_stream__update_counts([Input2])
		;
			{ ReturnedTuple = two },
			rl_stream__add_alias(Output, Input2),
			rl_stream__update_counts([Input1])
		)
	;
		% For a trivial semi-subtract, a reference may be
		% taken to the relation being subtracted from.
		{ Instr = subtract(output_rel(Output, _), Input1, Input2,
			_, _, Trivial) - "" },
		{ Trivial = yes(
			trivial_join_or_subtract_info(UsedTuple, _)) }
	->
		rl_stream__add_alias(Output, Input1),
		(
			{ UsedTuple = one },
			rl_stream__update_counts([Input1, Input2])
		;
			{ UsedTuple = two },
			rl_stream__update_counts([Input2])
		)
	;
		{ rl__instr_relations(Instr, Inputs, _) },
		rl_stream__update_counts(Inputs)
	).

%-----------------------------------------------------------------------------%

:- pred rl_stream__end_block_list(stream_info::in, stream_info::out) is det.

rl_stream__end_block_list(Info0, Info) :-
	Info0 = stream_info(Materialise0, Uses, Aliases0),
	relation__rtc(Aliases0, Aliases),
	relation__domain(Aliases, AliasedRels0),
	bag__to_assoc_list(Uses, UsesAL),
	assoc_list__keys(UsesAL, UsedRels),
	set__insert_list(AliasedRels0, UsedRels, RelsToCheck0),
	set__to_sorted_list(RelsToCheck0, RelsToCheck),
	list__foldl(rl_stream__end_block_check_relation(Uses, Aliases),
		RelsToCheck, Materialise0, Materialise),
	Info = stream_info(Materialise, Uses, Aliases).

	% Work out which relations used in this block need to be materialised.
:- pred rl_stream__end_block_check_relation(bag(relation_id)::in,
		relation(relation_id)::in, relation_id::in,
		set(relation_id)::in, set(relation_id)::out) is det.

rl_stream__end_block_check_relation(Uses, Aliases, Relation,
		Materialise0, Materialise) :-
	( relation__search_element(Aliases, Relation, RelationKey) ->
		relation__lookup_to(Aliases, RelationKey, AliasRelationKeys0),
		set__to_sorted_list(AliasRelationKeys0, AliasRelationKeys),
		list__map(relation__lookup_key(Aliases),
			AliasRelationKeys, Relations0)
	;
		Relations0 = [Relation]
	),
	set__list_to_set(Relations0, Relations),
	set__intersect(Relations, Materialise0, Intersect),
	( set__empty(Intersect) ->
		set__to_sorted_list(Relations, RelationsList),
		list__map(bag__count_value(Uses), RelationsList, Counts),
		list__foldl(lambda([X::in, Y::in, Z::out] is det, Z is X + Y),
			Counts, 0, NumUses),
		( NumUses = 1 ->
			Materialise = Materialise0
		;
			set__union(Materialise0, Relations, Materialise)
		)
	;
		set__union(Materialise0, Relations, Materialise)
	).

%-----------------------------------------------------------------------------%

	% Ensure that the status of a materialised temporary is correct.
:- pred rl_stream__update_must_materialise(relation_id::in,
		rl_opt_info::in, rl_opt_info::out) is det.

rl_stream__update_must_materialise(RelationId) -->
	rl_opt_info_get_relation_info(RelationId, RelationInfo0),
	{ RelationInfo0 = relation_info(Type0, B, C, D) },
	( { Type0 = temporary(stream) } ->
		rl_opt_info_set_relation_info(RelationId,
			relation_info(temporary(materialised), B, C, D))
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% Increment the usage counts for the list of relations.
:- pred rl_stream__update_counts(list(relation_id)::in, stream_info::in,
		stream_info::out) is det.

rl_stream__update_counts(RelationIds, Info0, Info) :-
	Info0 = stream_info(A, Counts0, C),
	bag__insert_list(Counts0, RelationIds, Counts),
	Info = stream_info(A, Counts, C).

%-----------------------------------------------------------------------------%

:- pred rl_stream__add_must_materialise_rels(list(relation_id)::in, 
		stream_info::in, stream_info::out) is det.

rl_stream__add_must_materialise_rels(Rels, Info0, Info) :-
	Info0 = stream_info(Materialise0, B, C),
	set__insert_list(Materialise0, Rels, Materialise),
	Info = stream_info(Materialise, B, C).

%-----------------------------------------------------------------------------%

:- pred rl_stream__add_alias(relation_id::in, relation_id::in,
		stream_info::in, stream_info::out) is det.

rl_stream__add_alias(Rel1, Rel2, Info0, Info) :-
	Info0 = stream_info(A, B, Aliases0),
	relation__add_values(Aliases0, Rel1, Rel2, Aliases1),
	relation__add_values(Aliases1, Rel2, Rel1, Aliases),
	Info = stream_info(A, B, Aliases).

%-----------------------------------------------------------------------------%

	% Return the list of relations which the given instruction requires
	% to be materialised.
:- pred rl_stream__must_materialise_rels(rl_instruction, list(relation_id)).
:- mode rl_stream__must_materialise_rels(in, out) is det.

rl_stream__must_materialise_rels(join(Output, _, _, _, _, _, _) - _,
		Materialise) :-
	rl_stream__output_is_indexed(Output, Materialise).	
rl_stream__must_materialise_rels(subtract(Output, _, _, _, _, _) - _,
		Materialise) :-
	rl_stream__output_is_indexed(Output, Materialise).
rl_stream__must_materialise_rels(difference(Output, _, _, _) - _,
		Materialise) :-
	rl_stream__output_is_indexed(Output, Materialise).
rl_stream__must_materialise_rels(project(Output, _, _, OtherOutputs, _) - _,
		Materialise) :-
	( OtherOutputs = [] ->
		rl_stream__output_is_indexed(Output, Materialise)
	;
		% XXX The first output in this case doesn't actually have
		% to be materialised, but see the comment on union_diff below
		% to see why we do it anyway.
		assoc_list__keys(OtherOutputs, Outputs),
		list__map(rl__output_rel_relation, [Output | Outputs],
			Materialise)
	).
rl_stream__must_materialise_rels(union(Output, _, _) - _, Materialise) :-
	rl_stream__output_is_indexed(Output, Materialise).

	% XXX the difference doesn't actually have to be materialised,
	% but it's a difficult case to handle because we have to be
	% sure that the difference is fully looked at somewhere to
	% force the update of the I/O relation, and also we need to 
	% be sure that this happens before the I/O relation is used.
rl_stream__must_materialise_rels(union_diff(UoOutput, DiInput, _,
		output_rel(Difference, _), _, _) - _, 
		[UoOutput, DiInput, Difference]).
rl_stream__must_materialise_rels(insert(UoOutput, DiInput, _, _, _) - _,
		[UoOutput, DiInput]).
rl_stream__must_materialise_rels(sort(Output, _, _) - _, Materialise) :-
	rl_stream__output_is_indexed(Output, Materialise).
rl_stream__must_materialise_rels(unset(_) - _, []).
rl_stream__must_materialise_rels(init(output_rel(Rel, _)) - _, [Rel]).
rl_stream__must_materialise_rels(insert_tuple(Output, _, _) - _,
		Materialise) :-
	rl_stream__output_is_indexed(Output, Materialise).

	% Indexed relations must always be materialised.
rl_stream__must_materialise_rels(add_index(output_rel(Output, _), Input) - _,
		[Input, Output]).
rl_stream__must_materialise_rels(clear(Rel) - _, [Rel]).
rl_stream__must_materialise_rels(ref(_, _) - _, []).
rl_stream__must_materialise_rels(copy(output_rel(Output, _), Input) - _,
		[Output, Input]).
rl_stream__must_materialise_rels(make_unique(output_rel(Output, _), Input) - _,
		[Output, Input]).
rl_stream__must_materialise_rels(label(_) - _, []).
rl_stream__must_materialise_rels(conditional_goto(Cond, _) - _, Rels) :-
	rl__goto_cond_relations(Cond, Rels).
rl_stream__must_materialise_rels(goto(_) - _, []).
rl_stream__must_materialise_rels(comment - _, []).
rl_stream__must_materialise_rels(aggregate(_, _, _, _) - _, []).
rl_stream__must_materialise_rels(call(_, Inputs, OutputRels, _) - _,
		Materialise) :-
	list__map(rl__output_rel_relation, OutputRels, Outputs),
	list__append(Inputs, Outputs, Materialise).

:- pred rl_stream__outputs_are_indexed(list(output_rel)::in,
		list(relation_id)::out) is det.

rl_stream__outputs_are_indexed(Outputs, Indexed) :-
	list__filter_map(lambda([OutputRel::in, Output::out] is semidet, (
		OutputRel = output_rel(Output, Indexes),
		Indexes = [_|_]
	)), Outputs, Indexed).

:- pred rl_stream__output_is_indexed(output_rel::in,
		list(relation_id)::out) is det.

rl_stream__output_is_indexed(output_rel(Output, Indexes), Indexed) :-
	( Indexes = [] ->
		Indexed = []
	;
		Indexed = [Output]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
