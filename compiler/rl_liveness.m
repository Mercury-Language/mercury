%-----------------------------------------------------------------------------%
% Copyright (C) 1998-1999, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_liveness.m
% Main author: stayl
%
% Use rl_analyse.m to compute the liveness of relations. 
% Remove dead code and insert instructions unset relation variables
% to make sure that temporaries are kept no longer than they are needed.
% Work out which relations need to be saved across calls.
% Insert explicit initialisations for relations used before they are defined.
% Make sure the inputs to uniondiff and insert instructions have at most
% one reference by inserting copy or make_unique instructions.
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_liveness.

:- interface.

:- import_module aditi_backend__rl_block.

:- import_module io.

:- pred rl_liveness(rl_opt_info, rl_opt_info, io__state, io__state).
:- mode rl_liveness(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module aditi_backend__rl.
:- import_module aditi_backend__rl_analyse.

:- import_module bool, int, list, map, relation, require, set, std_util.

rl_liveness(Opt0, Opt, IO0, IO) :-
	rl_liveness_2(IO0, IO, Opt0, Opt).

:- pred rl_liveness_2(io__state, io__state, rl_opt_info, rl_opt_info).
:- mode rl_liveness_2(di, uo, in, out) is det.

rl_liveness_2(IO0, IO) -->
	rl_opt_info_get_input_relations(Inputs),
	rl_opt_info_get_output_relations(Outputs),
	rl_opt_info_get_memoed_relations(MemoedRels),
	{ set__insert_list(MemoedRels, Inputs, LiveAtStart) },
	{ set__insert_list(MemoedRels, Outputs, LiveAtEnd) },

		%
		% Work out which relations are live at the end of each block.
		%
	rl_opt_info_get_rev_block_order(RevOrder),
	{ map__init(LiveMap0) },
	list__foldl2(rl_liveness__init_block_liveness(LiveAtEnd),
		RevOrder, LiveMap0, LiveMap1),

	{ LiveConfluence = rl_liveness__confluence },
	{ LiveBlockUpdate = rl_liveness__update_block_liveness },
	{ LiveEqual = rl_liveness__unify },
	{ LiveWrite = rl_analyse__write_gen_kill_data },
	
	{ LiveAnalysis = 
		rl_analysis(
			backward,
			LiveConfluence,
			LiveBlockUpdate,
			LiveEqual,
			LiveWrite
		) },
	rl_analyse(RevOrder, LiveAnalysis, LiveMap1, LiveMap,
		unit, _, IO0, IO1),

		%
		% Work out which relations have been initialised
		% at the end of each block.
		%
	{ map__init(CreateMap0) },
	list__foldl2(rl_liveness__init_block_creation(LiveAtStart),
		RevOrder, CreateMap0, CreateMap1),

	{ CreateConfluence = rl_liveness__confluence },
	{ CreateBlockUpdate = rl_liveness__update_block_creation },
	{ CreateEqual = rl_liveness__unify },
	{ CreateWrite = rl_analyse__write_gen_kill_data },

	{ CreateAnalysis = 
		rl_analysis(
			forward,
			CreateConfluence,
			CreateBlockUpdate,
			CreateEqual,
			CreateWrite
		) },
	
	{ list__reverse(RevOrder, Order) },
	rl_analyse(Order, CreateAnalysis, CreateMap1, CreateMap,
		unit, _, IO1, IO),

	list__foldl(
		rl_liveness__insert_init_and_unset_instructions(LiveAtStart,
			LiveAtEnd, LiveMap, CreateMap), 
		Order
	).

%-----------------------------------------------------------------------------%

:- type live_data == gen_kill_data.
:- type live_data_map == gen_kill_data_map.

:- pred rl_liveness__init_block_liveness(set(relation_id)::in, block_id::in, 
		live_data_map::in, live_data_map::out,
		rl_opt_info::in, rl_opt_info::out) is det.

rl_liveness__init_block_liveness(LiveAtEndofLast, BlockId,
		LiveData0, LiveData) -->
	rl_opt_info_get_block(BlockId, Block),
	{ Block = block(_, Instrs, MaybeBranch, _) },
	{ set__init(Def0) },
	{ set__init(Use0) },
	{ set__init(DefinedRels0) },
	{ set__init(UsedRels0) },

	{ list__foldl2(rl_liveness__instr_use_before_def, Instrs, 
		Use0, Use1, DefinedRels0, DefinedRels1) },
	{ MaybeBranch = yes(BranchInstr) ->
		rl_liveness__instr_use_before_def(BranchInstr, Use1, Use,
			DefinedRels1, _)
	;
		Use = Use1
	},

	{ list__foldl2(rl_liveness__instr_def_before_use, Instrs, 
		Def0, Def, UsedRels0, _) },
	% The branch instruction at the end of the block
	% can't define any relations.

	rl_opt_info_get_last_block_id(LastBlockId),
	{ BlockId = LastBlockId ->
		EndLiveRels1 = LiveAtEndofLast,
		set__difference(EndLiveRels1, Def, StartLiveRels0),
		set__union(StartLiveRels0, Use, StartLiveRels)
	;
		set__init(EndLiveRels1),
		set__init(StartLiveRels)
	},
	{ MaybeBranch = yes(Branch) ->
		% The relations used in the branch at the end
		% are always live at the end of the block.
		rl__instr_relations(Branch, BranchInputs, _),
		set__insert_list(EndLiveRels1, BranchInputs, EndLiveRels)
	;
		EndLiveRels = EndLiveRels1
	},
	{ BlockData = block_data(EndLiveRels, StartLiveRels, Use - Def) },
	{ map__det_insert(LiveData0, BlockId, BlockData, LiveData) }.

	% Find all relations used in the block before 
	% any definitions in the block. These relations
	% are required to be live on entry to the block.
:- pred rl_liveness__instr_use_before_def(rl_instruction::in, 
		set(relation_id)::in, set(relation_id)::out,
		set(relation_id)::in, set(relation_id)::out) is det.

rl_liveness__instr_use_before_def(Instr, Use0, Use, 
		DefinedRels0, DefinedRels) :-
	rl__instr_relations(Instr, Inputs, Outputs),
	set__list_to_set(Inputs, InputSet),
	set__difference(InputSet, DefinedRels0, UndefinedUsedRels),
	set__union(Use0, UndefinedUsedRels, Use),
	set__insert_list(DefinedRels0, Outputs, DefinedRels).

	% Find all relations defined in the block before any uses
	% in the block. These relations are dead at the start of
	% the block.
:- pred rl_liveness__instr_def_before_use(rl_instruction::in, 
		set(relation_id)::in, set(relation_id)::out,
		set(relation_id)::in, set(relation_id)::out) is det.

rl_liveness__instr_def_before_use(Instr, DefinedRels0, DefinedRels,
		UsedRels0, UsedRels) :-
	rl__instr_relations(Instr, Inputs, Outputs),
	set__insert_list(UsedRels0, Inputs, UsedRels),
	set__list_to_set(Outputs, OutputSet),
	set__difference(OutputSet, UsedRels, UnusedDefinedRels),
	set__union(DefinedRels0, UnusedDefinedRels, DefinedRels).

%-----------------------------------------------------------------------------%

	% Given the required liveness at the end of the block, compute
	% the required liveness at the start of the block.
:- pred rl_liveness__update_block_liveness(block_id::in,
	int_set::in, live_data::in, live_data::out, unit::in, unit::out,
	rl_opt_info::in, rl_opt_info::out) is det.

rl_liveness__update_block_liveness(BlockId, EndLiveRels0,
		BlockData0, BlockData, _, unit) -->
	rl_opt_info_get_block(BlockId, Block),
	rl_opt_info_get_last_block_id(LastBlockId),
	{ BlockId = LastBlockId ->
		% The last block never branches to anywhere,
		% so its liveness information never changes.
		BlockData = BlockData0
	;
		BlockData0 = block_data(_, _, Use - Def),
		Block = block(_, _, MaybeBranch, _),
		( MaybeBranch = yes(Branch) ->
			% The relations used in the branch at the end
			% are always live at the end of the block.
			rl__instr_relations(Branch, BranchInputs, _),
			set__insert_list(EndLiveRels0, 
				BranchInputs, EndLiveRels)
		;
			EndLiveRels = EndLiveRels0
		),
		set__difference(EndLiveRels, Def, StartLiveRels0),
		set__union(StartLiveRels0, Use, StartLiveRels),
		BlockData = block_data(EndLiveRels, StartLiveRels, Use - Def)
	}.	

%-----------------------------------------------------------------------------%

	% Combine the data from multiple callers/callees of a block.
:- pred rl_liveness__confluence(pair(block_id, int_set)::in,
		pair(block_id, maybe(int_set))::in, int_set::out,
		unit::in, unit::out, rl_opt_info::in, rl_opt_info::out) is det.

rl_liveness__confluence(_ - Data, _ - no, Data, _, unit) --> [].
rl_liveness__confluence(_ - Data1, _ - yes(Data2), Data, _, unit) -->
	{ set__union(Data1, Data2, Data) }.

%-----------------------------------------------------------------------------%

:- pred rl_liveness__unify(int_set::in, int_set::in, unit::in) is semidet.

rl_liveness__unify(Data, Data, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type creation_data == gen_kill_data.
:- type creation_data_map == gen_kill_data_map.

:- pred rl_liveness__init_block_creation(set(relation_id)::in, block_id::in,
		creation_data_map::in, creation_data_map::out,
		rl_opt_info::in, rl_opt_info::out) is det.	
	
rl_liveness__init_block_creation(CreatedAtStartOfFirst, BlockId,
		CreateMap0, CreateMap) -->
	rl_opt_info_get_block(BlockId, Block),
	{ Block = block(_, Instrs, _, _) },
	{ set__init(Created0) },
	{ AddCreated = (pred(Instr::in, C0::in, C::out) is det :-
			rl__instr_relations(Instr, _, Outputs),
			set__insert_list(C0, Outputs, C)
		) },
	{ list__foldl(AddCreated, Instrs, Created0, Created) },

	rl_opt_info_get_first_block_id(FirstBlock),
	{ BlockId = FirstBlock ->
		InCreated = CreatedAtStartOfFirst,
		set__union(InCreated, Created, OutCreated)
	;
		set__init(InCreated),
		OutCreated = Created
	},
	{ set__init(Dummy) },
	{ BlockData = block_data(InCreated, OutCreated, Created - Dummy) },
	{ map__det_insert(CreateMap0, BlockId, BlockData, CreateMap) }.

:- pred rl_liveness__update_block_creation(block_id::in, 
		int_set::in, creation_data::in, creation_data::out,
		unit::in, unit::out, rl_opt_info::in, rl_opt_info::out) is det.

rl_liveness__update_block_creation(BlockId, InCreated, 
		BlockData0, BlockData, _, unit) -->
	rl_opt_info_get_first_block_id(FirstBlockId),
	{ BlockId = FirstBlockId ->
		% The first block is never branched to from anywhere,
		% so its initial liveness never changes.
		BlockData = BlockData0
	;
		BlockData0 = block_data(_, _, Created - Dummy),
		set__union(InCreated, Created, OutCreated),
		BlockData = block_data(InCreated, OutCreated, Created - Dummy)
	}.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred rl_liveness__insert_init_and_unset_instructions(set(relation_id)::in,
		set(relation_id)::in, live_data_map::in, creation_data_map::in,
		block_id::in, rl_opt_info::in, rl_opt_info::out) is det.

rl_liveness__insert_init_and_unset_instructions(CreatedAtStart, LiveAtEnd,
		LiveMap, CreateMap, BlockId) -->
	rl_opt_info_get_first_block_id(FirstBlockId),
	rl_opt_info_get_last_block_id(LastBlockId),
	rl_opt_info_get_block(BlockId, Block0),
	{ Block0 = block(Label, Instrs0, Branch, _BlockInfo0) },

		%
		% Go backwards over the instructions removing dead code
		% and inserting instructions to drop temporaries as
		% soon as they become dead.
		%
	{ list__reverse(Instrs0, RevInstrs0) },
	{ map__lookup(LiveMap, BlockId, LiveData) },
	{ LiveData = block_data(BlockLiveAtEnd, _, _) },
	{ list__foldl2(rl_liveness__insert_unset_instructions,
		RevInstrs0, [], Instrs1, BlockLiveAtEnd, BlockLiveAtStart) },

		% 
		% Drop relations which are live at the end of a
		% calling block but are dead at the start of
		% this block.
		%
	rl_opt_info_get_flow_graph(Graph),
	{ relation__lookup_element(Graph, BlockId, BlockKey) },
	{ relation__lookup_to(Graph, BlockKey, CallingBlockKeys0) },
	{ set__to_sorted_list(CallingBlockKeys0, CallingBlockKeys) },
	{ list__map(relation__lookup_key(Graph), CallingBlockKeys, 
		CallingBlocks) },
	{ GetLiveOutputs = 
		(pred(CallingBlock::in, LiveRels::out) is det :-
			map__lookup(LiveMap, CallingBlock, CallingData),
			CallingData = block_data(LiveRels, _, _)
		) },
	{ list__map(GetLiveOutputs, CallingBlocks, CallingBlockLiveRels0) },
	( { BlockId = FirstBlockId } ->
		{ CallingBlockLiveRels1 = CreatedAtStart }
	;
		{ set__init(CallingBlockLiveRels1) }
	),
	{ list__foldl(set__union, CallingBlockLiveRels0,
		CallingBlockLiveRels1, CallingBlockLiveRels) },
	{ set__difference(CallingBlockLiveRels, 
		BlockLiveAtStart, DeadAtStart0) },	
	{ set__to_sorted_list(DeadAtStart0, DeadAtStart) },
	{ list__map(rl_liveness__drop_rel, DeadAtStart, DropDeadRels) },

		%
		% Go forwards over the instructions adding creates for
		% relations which should be live at the start of the
		% block but are not.
		%
	rl_opt_info_get_relation_info_map(RelInfoMap),
	{ map__lookup(CreateMap, BlockId, CreateData) },
	{ CreateData = block_data(BlockCreatedAtStart, _, _) },
	{ list__foldl2(rl_liveness__insert_init_instructions(RelInfoMap),
		Instrs1, [], RevInstrs1, BlockCreatedAtStart,
		BlockCreatedAtEnd) },
	{ list__reverse(RevInstrs1, Instrs2) },

		% 
		% Add creates for relations which are live at the
		% start of a called block but which are not initialised
		% by any caller of this block and are not created 
		% by this block.
		%	
	{ relation__lookup_from(Graph, BlockKey, CalledBlockKeys0) },
	{ set__to_sorted_list(CalledBlockKeys0, CalledBlockKeys) }, 
	{ list__map(relation__lookup_key(Graph), CalledBlockKeys,
		CalledBlocks) },
	{ GetCreatedOutputs =
		(pred(CalledBlock::in, CalledInitAtStart::out) is det :-
			map__lookup(CreateMap, CalledBlock, CalledCreateData),
			map__lookup(LiveMap, CalledBlock, CalledLiveData),
			CalledLiveData = block_data(_, CalledLiveAtStart, _),
			CalledCreateData =
				block_data(_, CalledInitAtStart0, _),
			set__intersect(CalledInitAtStart0, CalledLiveAtStart,
				CalledInitAtStart)
		) },
	{ list__map(GetCreatedOutputs, CalledBlocks, CalledBlockCreated0) },	
	( { BlockId = LastBlockId } ->
		{ CalledBlockCreated1 = LiveAtEnd }
	;
		{ set__init(CalledBlockCreated1) }
	),	
	{ list__foldl(set__union, CalledBlockCreated0,
		CalledBlockCreated1, CalledBlockCreated) },
	{ set__difference(CalledBlockCreated, BlockCreatedAtEnd, 
		NotCreatedAtEnd0) },
	{ set__to_sorted_list(NotCreatedAtEnd0, NotCreatedAtEnd) },

	{ list__filter_map(rl_liveness__init_rel(RelInfoMap), 
		NotCreatedAtEnd, CreateRels) },

	{ list__condense([DropDeadRels, Instrs2, CreateRels], Instrs) },

	{ BlockInfo = block_info(BlockLiveAtStart, BlockLiveAtEnd) },
	{ Block = block(Label, Instrs, Branch, BlockInfo) },
	rl_opt_info_set_block(BlockId, Block).

	% Go backwards over the instructions inserting instructions
	% to unset relation variables that become dead.
	% Also add instructions to make sure inputs to operations
	% such as B-tree union_diff have only one reference and can
	% be updated.
:- pred rl_liveness__insert_unset_instructions(rl_instruction::in, 
		list(rl_instruction)::in, list(rl_instruction)::out,
		set(relation_id)::in, set(relation_id)::out) is det.

rl_liveness__insert_unset_instructions(Instr0, Instrs0, Instrs, 
		LiveRels0, LiveRels) :-
	rl__instr_relations(Instr0, Inputs, Outputs),	
	set__list_to_set(Outputs, OutputSet),
	set__intersect(OutputSet, LiveRels0, LiveOutputs),
	( 
		set__empty(LiveOutputs)
	->
		( rl_liveness__undroppable_instr(Instr0) ->
			Instrs = [Instr0 | Instrs0]
		;
			Instrs = Instrs0
		),
		LiveRels = LiveRels0
	;
		% Order is important here. Relations that are defined
		% by the instruction are made dead, then relations
		% used are made live.
		set__difference(LiveRels0, OutputSet, LiveRels1),

		set__insert_list(LiveRels1, Inputs, LiveRels),

		% Produce instructions to unset all the relation variables
		% made dead by this instruction.
		set__difference(LiveRels, LiveRels0, KilledRels0),
		set__difference(OutputSet, LiveRels0, StillBornRels),
		set__union(KilledRels0, StillBornRels, KilledRels1),
		set__to_sorted_list(KilledRels1, KilledRels),
		list__map(rl_liveness__drop_rel, KilledRels, PostInstrs0),

		(
			% Attach to the call the set of relations which
			% need to be saved across it.
			Instr0 = call(ProcInputs, CallInputs,
				CallOutputs, _) - Comment
		->
			set__list_to_set(Inputs, InputSet),
			set__intersect(LiveRels1, InputSet, LiveInputs),
			PreInstrs = [],
			PostInstrs1 = [],
			Instr = call(ProcInputs, CallInputs, CallOutputs,
					LiveInputs) - Comment
		;
			% Make sure there is only one reference to the
			% destructively updated input to a union_diff.
			Instr0 = union_diff(UoOutput, DiInput, Input, Diff,
				Index, yes(CopyRel)) - Comment
		->
			( set__member(DiInput, KilledRels0) ->
				PreInstrs = [make_unique(CopyRel, DiInput) - ""]
			;
				PreInstrs = [copy(CopyRel, DiInput) - ""]
			),
			CopyRel = output_rel(CopyRelation, _),
			Instr = union_diff(UoOutput, CopyRelation,
				Input, Diff, Index, no) - Comment,
			PostInstrs1 = [unset(CopyRelation) - ""]
		;
			% Make sure there is only one reference to the
			% destructively updated input to an insert.
			Instr0 = insert(UoOutput, DiInput, Input, InsertType,
				yes(CopyRel)) - Comment
		->
			( set__member(DiInput, KilledRels0) ->
				PreInstrs = [make_unique(CopyRel, DiInput) - ""]
			;
				PreInstrs = [copy(CopyRel, DiInput) - ""]
			),
			CopyRel = output_rel(CopyRelation, _),
			Instr = insert(UoOutput, CopyRelation,
				Input, InsertType, no) - Comment,
			PostInstrs1 = [unset(CopyRelation) - ""]
		;
			Instr = Instr0,
			PreInstrs = [],
			PostInstrs1 = []
		),

		% Add the instructions to unset the newly dead 
		% relation variables after the instruction.
		list__condense(
			[
			PreInstrs,
			[Instr],
			PostInstrs0,
			PostInstrs1,
			Instrs0
			],
			Instrs)
	).

:- pred rl_liveness__undroppable_instr(rl_instruction::in) is semidet.

rl_liveness__undroppable_instr(label(_) - _).
rl_liveness__undroppable_instr(goto(_) - _).
rl_liveness__undroppable_instr(conditional_goto(_, _) - _).
rl_liveness__undroppable_instr(clear(_) - _).
rl_liveness__undroppable_instr(unset(_) - _).

:- pred rl_liveness__drop_rel(relation_id::in, rl_instruction::out) is det.

rl_liveness__drop_rel(Rel, unset(Rel) - "").

%-----------------------------------------------------------------------------%

	% Add create instructions to explicitly initialise free relation
	% variables just before they are used.
:- pred rl_liveness__insert_init_instructions(relation_info_map::in,
		rl_instruction::in, list(rl_instruction)::in, 
		list(rl_instruction)::out, set(relation_id)::in, 
		set(relation_id)::out) is det.

rl_liveness__insert_init_instructions(RelInfoMap, Instr, 
		RevInstrs0, RevInstrs, CreatedRels0, CreatedRels) :-
	rl__instr_relations(Instr, Inputs, Outputs),	

	set__list_to_set(Inputs, InputSet),
	set__difference(InputSet, CreatedRels0, UninitialisedInputs0),
	set__to_sorted_list(UninitialisedInputs0, UninitialisedInputs),
	list__filter_map(rl_liveness__init_rel(RelInfoMap), 
		UninitialisedInputs, Initialisations),

	list__append([Instr | Initialisations], RevInstrs0, RevInstrs),
	set__insert_list(CreatedRels0, Outputs, CreatedRels1),
	set__union(CreatedRels1, UninitialisedInputs0, CreatedRels).

:- pred rl_liveness__init_rel(relation_info_map::in, 
		relation_id::in, rl_instruction::out) is semidet.

rl_liveness__init_rel(RelInfoMap, Rel, init(output_rel(Rel, [])) - "") :-
	map__lookup(RelInfoMap, Rel, RelInfo),
	% Only initialise temporary relations.
	RelInfo = relation_info(temporary(_), _, _, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
