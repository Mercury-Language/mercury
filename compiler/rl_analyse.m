%-----------------------------------------------------------------------------%
% Copyright (C) 1998,2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_analyse.m
% Main author: stayl
% 
% Generic flow graph analysis for RL instructions.
% This is mostly as described in the Dragon Book, chapter 10.
%-----------------------------------------------------------------------------%
:- module rl_analyse.

:- interface.

:- import_module rl_block.
:- import_module io, list, map, set, std_util.

	% rl_analyse(Blocks, Direction, Init, Confluence, Update, Results,
	% 	Globals0, Globals, IO0, IO).
:- pred rl_analyse(list(block_id), rl_analysis(T, U, V), block_data_map(T, U),
		block_data_map(T, U), V, V, io__state, io__state,
		rl_opt_info, rl_opt_info) is det.
:- mode rl_analyse(in, rl_analysis, in, out, in, out, di, uo, in, out) is det.

:- type rl_analysis(BlockData, Info, Globals)
	---> rl_analysis(
		direction,
		confluence(BlockData, Globals),
		block_update(BlockData, Info, Globals),
		equal(BlockData, Globals),
		write(BlockData, Info, Globals)
	).

:- inst rl_analysis
	---> rl_analysis(
		ground,
		confluence,
		block_update,
		equal,
		write
	).

	% Input and output data for each block.
:- type block_data_map(T, U) == map(block_id, block_data(T, U)).

:- type block_data(Data, Info)
	---> block_data(
		Data,	% in value
		Data,	% out value
		Info	% data associated with the block (e.g. gen + kill sets)
	).

:- mode rl_analysis :: in(rl_analysis).

:- type direction
	--->	forward
	;	backward.

	% Combine the information for multiple callers/callees of a block.
	% The first block_id - BlockData argument pair corresponds to the
	% feeder block whose information is being added.
	% The second pair is for the block whose entry information is being
	% computed. The maybe(BlockData) should be `no' for data to which
	% nothing has been added.
	% Knowing the block_ids is useful in cases where for some
	% arcs only a subset of the information should be propagated.
:- type confluence(BlockData, Globals) ==
		pred(pair(block_id, BlockData),
			pair(block_id, maybe(BlockData)), BlockData,
			Globals, Globals, rl_opt_info, rl_opt_info).
:- inst confluence = (pred(in, in, out, in, out, in, out) is det).

	% Given the information at entry, compute the information at exit.
:- type block_update(BlockData, Info, Globals) ==
		pred(block_id, BlockData, block_data(BlockData, Info),
			block_data(BlockData, Info), Globals, Globals,
			rl_opt_info, rl_opt_info).
:- inst block_update = (pred(in, in, in, out, in, out, in, out) is det).

	% Do an in-in test unification.
:- type equal(BlockData, Globals) == pred(BlockData, BlockData, Globals).
:- inst equal = (pred(in, in, in) is semidet).

	% Pretty print the block data for debugging.
:- type write(BlockData, Info, Globals) ==
	    (pred(block_data(BlockData, Info), Globals, io__state, io__state)).
:- inst write = (pred(in, in, di, uo) is det).

%-----------------------------------------------------------------------------%

	% This format of data is what is described in the Dragon Book, so
	% it should occur often enough to be special-cased here.
	% The extra data is a pair of int_sets which correspond to the data
	% "generated" and "killed" by the block.
:- type gen_kill_data == block_data(int_set, pair(int_set)).
:- type gen_kill_data_map == block_data_map(int_set, pair(int_set)).
	
	% XXX use bit vectors.
:- type int_set == set(int).

:- pred rl_analyse__write_gen_kill_data(gen_kill_data::in, Globals::in,
		io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals, options.
:- import_module assoc_list, bool, relation, require, string.  

rl_analyse(Blocks, Analysis, BlockDataMap0, BlockDataMap,
		Globals0, Globals, IO0, IO) -->
	rl_opt_info_get_rev_block_order(RevOrder),
	{ list__reverse(RevOrder, Order) },
	{ rl_analyse__message("Initial info\n", [], IO0, IO1) },
	{ rl_analyse__dump_block_data_map(Analysis, Order,
		BlockDataMap0, Globals0, IO1, IO2) },
	rl_analyse__to_fixpoint(Analysis, Blocks,
		BlockDataMap0, BlockDataMap, Globals0, Globals, IO2, IO3),
	{ rl_analyse__dump_block_data_map(Analysis, Order,
		BlockDataMap, Globals, IO3, IO) }.

:- pred rl_analyse__to_fixpoint(rl_analysis(T, U, V)::rl_analysis,
	list(block_id)::in, block_data_map(T, U)::in,
	block_data_map(T, U)::out, V::in, V::out, io__state::di, io__state::uo,
	rl_opt_info::in, rl_opt_info::out) is det.

rl_analyse__to_fixpoint(Analysis, Blocks, BlockDataMap0, BlockDataMap,
		Globals0, Globals, IO0, IO) -->
	{ rl_analyse__do_io(io__write_string("rl_analyse: starting new pass\n"),
		IO0, IO1) },
	rl_analyse__blocks(Analysis, Blocks, BlockDataMap0, BlockDataMap1,
		no, Changed, Globals0, Globals1, IO1, IO2),
	( { Changed = yes } ->
		rl_analyse__to_fixpoint(Analysis, Blocks, BlockDataMap1,
			BlockDataMap, Globals1, Globals, IO2, IO)
	;
		{ rl_analyse__do_io(io__write_string("finished iterating\n"),
			IO2, IO) },
		{ Globals = Globals1 },
		{ BlockDataMap = BlockDataMap1 }
	).

:- pred rl_analyse__blocks(rl_analysis(T, U, V)::rl_analysis,
		list(block_id)::in, block_data_map(T, U)::in,
		block_data_map(T, U)::out, bool::in, bool::out,
		V::in, V::out, io__state::di, io__state::uo,
		rl_opt_info::in, rl_opt_info::out) is det.

rl_analyse__blocks(_, [], Info, Info, Changed, Changed,
		Globals, Globals, IO, IO) --> [].
rl_analyse__blocks(Analysis, [BlockId | BlockIds], BlockDataMap0, BlockDataMap, 
		Changed0, Changed, Globals0, Globals, IO0, IO) -->
	rl_opt_info_get_flow_graph(FlowGraph),
	{ relation__lookup_element(FlowGraph, BlockId, BlockKey) },

	%
	% Work out where the information flowing into the block comes from.
	%
	{ rl_analyse__direction(Analysis, Direction) },
	{
		Direction = forward,
		relation__lookup_to(FlowGraph, BlockKey, PredecessorKeys0)
	;
		Direction = backward,
		relation__lookup_from(FlowGraph, BlockKey, PredecessorKeys0)
	},

	{ set__to_sorted_list(PredecessorKeys0, PredecessorKeys) },
	{ list__map(relation__lookup_key(FlowGraph), PredecessorKeys,
		Predecessors) },
	{ map__lookup(BlockDataMap0, BlockId, BlockData0) },
	
	{ GetOutValues =
	    lambda([Node::in, NodeAndOValue::out] is det, (
		map__lookup(BlockDataMap0, Node, block_data(_, OutValue, _)),
		NodeAndOValue = Node - OutValue
	    )) },
	{ list__map(GetOutValues, Predecessors, OutPredecessors) },

	%
	% Combine the information flowing into the block using
	% the confluence operator.
	%
	rl_analyse__confluence_list(Analysis, OutPredecessors,
		BlockId - no, _ - MaybeInValue, Globals0, Globals1),

	{
		MaybeInValue = yes(InValue)
	;
		MaybeInValue = no,
		BlockData0 = block_data(InValue, _, _)
	},

	%
	% Compute the output value from the current block given the
	% input computed above.
	%
	rl_analyse__block_update(Analysis, BlockId, InValue,
		BlockData0, BlockData, Globals1, Globals2),
	{ map__det_update(BlockDataMap0, BlockId, BlockData, BlockDataMap1) },

	%
	% Check whether anything changed.
	% 
	{ BlockData0 = block_data(_, OldOut, _) },
	{ BlockData = block_data(_, NewOut, _) },
	( { rl_analyse__equal(Analysis, OldOut, NewOut, Globals2) } ->
		{ Changed1 = Changed0 },
		{ IO1 = IO0 }
	;
		{ Changed1 = yes },
		{ rl_analyse__do_io(rl_analyse__dump_changed_data(Analysis,
			BlockId, BlockData0, BlockData, Globals2), IO0, IO1) }
	),
	rl_analyse__blocks(Analysis, BlockIds, BlockDataMap1, BlockDataMap,
		Changed1, Changed, Globals2, Globals, IO1, IO).

%-----------------------------------------------------------------------------%

:- pred rl_analyse__message(string::in, list(string__poly_type)::in,
		io__state::di, io__state::uo) is det.

rl_analyse__message(Msg, Fmt) -->
	rl_analyse__do_io(io__format(Msg, Fmt)).

:- pred rl_analyse__do_io(pred(io__state, io__state)::in(pred(di, uo) is det),
		io__state::di, io__state::uo) is det.

rl_analyse__do_io(Writer) -->
	globals__io_lookup_bool_option(debug_rl_opt, Debug),
	( { Debug = yes } ->
		call(Writer)
	;
		[]
	).

:- pred rl_analyse__dump_block_data_map(rl_analysis(T, U, V)::rl_analysis,
		list(block_id)::in, block_data_map(T, U)::in, V::in, 
		io__state::di, io__state::uo) is det.

rl_analyse__dump_block_data_map(Analysis, Blocks, BlockDataMap, Globals) -->
	rl_analyse__do_io(pred(di, uo) is det -->
		io__write_list(Blocks, "\n",
			rl_analyse__dump_block_data(Analysis,
				BlockDataMap, Globals))
	).

:- pred rl_analyse__dump_block_data(rl_analysis(T, U, V)::rl_analysis,
		block_data_map(T, U)::in, V::in, block_id::in,
		io__state::di, io__state::uo) is det.

rl_analyse__dump_block_data(Analysis, BlockDataMap, Globals, BlockId) -->
	{ map__lookup(BlockDataMap, BlockId, BlockData) },
	io__write_string("Block "),
	io__write_int(BlockId),
	io__write_string(":\n"),
	rl_analyse__write(Analysis, BlockData, Globals),
	io__nl.

:- pred rl_analyse__dump_changed_data(rl_analysis(T, U, V)::rl_analysis,
		block_id::in, block_data(T, U)::in, block_data(T, U)::in, 
		V::in, io__state::di, io__state::uo) is det.

rl_analyse__dump_changed_data(Analysis, BlockId, Data0, Data, Globals) -->
	io__write_string("Changed data for block "),
	io__write_int(BlockId),
	io__write_string(":\n"),
	rl_analyse__write(Analysis, Data0, Globals),
	io__write_string("\t==> "),
	rl_analyse__write(Analysis, Data, Globals),
	io__nl.

rl_analyse__write_gen_kill_data(block_data(In, Out, Gen - Kill), _) -->
	io__write_string("<gen: ["),
	rl_analyse__write_int_set(Gen),
	io__write_string("] kill: ["),
	rl_analyse__write_int_set(Kill),
	io__write_string("] in: ["),
	rl_analyse__write_int_set(In),
	io__write_string("] out: ["),
	rl_analyse__write_int_set(Out),
	io__write_string("]>\n").

:- pred rl_analyse__write_int_set(int_set::in,
		io__state::di, io__state::uo) is det.

rl_analyse__write_int_set(Set0) -->
	{ set__to_sorted_list(Set0, Set) },
	io__write_list(Set, ", ", io__write_int).

%-----------------------------------------------------------------------------%

:- pred rl_analyse__confluence_list(rl_analysis(T, U, V)::in(rl_analysis),
		assoc_list(block_id, T)::in, pair(block_id, maybe(T))::in,
		pair(block_id, maybe(T))::out, V::in, V::out,
		rl_opt_info::in, rl_opt_info::out) is det.
	
rl_analyse__confluence_list(_, [], Value, Value, Globals, Globals) --> [].
rl_analyse__confluence_list(Analysis, [OutPredecessor | OutPredecessors],
		Value0, Value, Globals0, Globals) -->
	rl_analyse__confluence(Analysis, OutPredecessor, Value0, Value1,
		Globals0, Globals1),
	rl_analyse__confluence_list(Analysis, OutPredecessors, Value1, Value,
		Globals1, Globals).

%-----------------------------------------------------------------------------%

:- pred rl_analyse__direction(rl_analysis(_, _, _), direction).
:- mode rl_analyse__direction(rl_analysis, out) is det.

	% Combine the information for multiple entries to a block.
:- pred rl_analyse__confluence(rl_analysis(BlockData, _, Globals),
		pair(block_id, BlockData), pair(block_id, maybe(BlockData)),
		pair(block_id, maybe(BlockData)), Globals, Globals,
		rl_opt_info, rl_opt_info).
:- mode rl_analyse__confluence(rl_analysis, in, in, out,
		in, out, in, out) is det.

	% Given the information at entry, compute the information at exit.
:- pred rl_analyse__block_update(rl_analysis(BlockData, Info, Globals),
		block_id, BlockData, block_data(BlockData, Info),
		block_data(BlockData, Info), Globals, Globals, 
		rl_opt_info, rl_opt_info).
:- mode rl_analyse__block_update(rl_analysis, in, in,
		in, out, in, out, in, out) is det.

:- pred rl_analyse__equal(rl_analysis(BlockData, _, Globals),
		BlockData, BlockData, Globals).
:- mode rl_analyse__equal(rl_analysis, in, in, in) is semidet.

:- pred rl_analyse__write(rl_analysis(BlockData, Info, Globals),
		block_data(BlockData, Info), Globals, io__state, io__state).
:- mode rl_analyse__write(rl_analysis, in, in, di, uo) is det.

%-----------------------------------------------------------------------------%

rl_analyse__direction(rl_analysis(Dir, _, _, _, _), Dir).
rl_analyse__confluence(rl_analysis(_, Confluence, _, _, _), BlockData1,
		Block - MaybeData2, Block - yes(Data), Globals0, Globals) -->
	call(Confluence, BlockData1, Block - MaybeData2, Data,
		Globals0, Globals).
rl_analyse__block_update(rl_analysis(_, _, Update, _, _), BlockId, 
		In, Data0, Data, Globals0, Globals) -->
	call(Update, BlockId, In, Data0, Data, Globals0, Globals).
rl_analyse__equal(rl_analysis(_, _, _, Equal, _), Data1, Data2, Globals) :-
	call(Equal, Data1, Data2, Globals).
rl_analyse__write(rl_analysis(_, _, _, _, Write), Data, Globals) -->
	call(Write, Data, Globals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
