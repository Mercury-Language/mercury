%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_debug.m - debugging messages for vn_order and vn_flush.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_debug.

:- interface.

:- import_module atsort, vn_type, vn_table, livemap.
:- import_module assoc_list, bool, list, io.

:- pred vn_debug__tuple_msg(maybe(bool), list(instruction), vn_ctrl_tuple,
	io__state, io__state).
:- mode vn_debug__tuple_msg(in, in, in, di, uo) is det.

:- pred vn_debug__livemap_msg(livemap, io__state, io__state).
:- mode vn_debug__livemap_msg(in, di, uo) is det.

:- pred vn_debug__fragment_msg(instr, io__state, io__state).
:- mode vn_debug__fragment_msg(in, di, uo) is det.

:- pred vn_debug__failure_msg(instr, string, io__state, io__state).
:- mode vn_debug__failure_msg(in, in, di, uo) is det.

:- pred vn_debug__parallel_msgs(list(parallel), io__state, io__state).
:- mode vn_debug__parallel_msgs(in, di, uo) is det.

:- pred vn_debug__parallel_msg(parallel, io__state, io__state).
:- mode vn_debug__parallel_msg(in, di, uo) is det.

:- pred vn_debug__computed_goto_msg(list(label), list(parallel),
	assoc_list(label, maybe(parallel)), io__state, io__state).
:- mode vn_debug__computed_goto_msg(in, in, in, di, uo) is det.

:- pred vn_debug__order_start_msg(ctrlmap, flushmap, vn_tables,
	io__state, io__state).
:- mode vn_debug__order_start_msg(in, in, in, di, uo) is det.

:- pred vn_debug__order_sink_msg(vn_node, io__state, io__state).
:- mode vn_debug__order_sink_msg(in, di, uo) is det.

:- pred vn_debug__order_link_msg(vn_node, vn_node, bool, io__state, io__state).
:- mode vn_debug__order_link_msg(in, in, in, di, uo) is det.

:- pred vn_debug__order_antidep_msg(vn_node, vn_node, io__state, io__state).
:- mode vn_debug__order_antidep_msg(in, in, di, uo) is det.

:- pred vn_debug__order_map_msg(relmap(vn_node), relmap(vn_node),
	relmap(vn_node), relmap(vn_node), io__state, io__state).
:- mode vn_debug__order_map_msg(in, in, in, in, di, uo) is det.

:- pred vn_debug__order_order_msg(list(vn_node), io__state, io__state).
:- mode vn_debug__order_order_msg(in, di, uo) is det.

:- pred vn_debug__order_equals_msg(string, list(vn_node), io__state, io__state).
:- mode vn_debug__order_equals_msg(in, in, di, uo) is det.

:- pred vn_debug__cost_header_msg(string, io__state, io__state).
:- mode vn_debug__cost_header_msg(in, di, uo) is det.

:- pred vn_debug__cost_msg(bool, int, int, io__state, io__state).
:- mode vn_debug__cost_msg(in, in, in, di, uo) is det.

:- pred vn_debug__cost_detail_msg(instr, int, int, io__state, io__state).
:- mode vn_debug__cost_detail_msg(in, in, in, di, uo) is det.

:- pred vn_debug__restart_msg(instruction, io__state, io__state).
:- mode vn_debug__restart_msg(in, di, uo) is det.

:- pred vn_debug__divide_msg(instruction, io__state, io__state).
:- mode vn_debug__divide_msg(in, di, uo) is det.

:- pred vn_debug__flush_start_msg(vn_node, io__state, io__state).
:- mode vn_debug__flush_start_msg(in, di, uo) is det.

:- pred vn_debug__flush_also_msg(vnlval, io__state, io__state).
:- mode vn_debug__flush_also_msg(in, di, uo) is det.

:- pred vn_debug__flush_end_msg(list(instruction), vn_tables,
	io__state, io__state).
:- mode vn_debug__flush_end_msg(in, in, di, uo) is det.

:- pred vn_debug__dump_instrs(list(instruction), io__state, io__state).
:- mode vn_debug__dump_instrs(in, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, llds_out, globals, options, opt_debug.
:- import_module string, int, std_util, map.

vn_debug__tuple_msg(Intermediate, Instrs, Tuple) -->
	vn_debug__tuple_msg_flag(Flag),
	(
		{ Flag = yes },
		io__write_string("\nTuples from the "),
		(
			{ Intermediate = no },
			io__write_string("final instruction sequence:\n\n")
		;
			{ Intermediate = yes(Stitched) },
			(
				{ Stitched = no },
				io__write_string("intermediate instruction sequence:\n\n")
			;
				{ Stitched = yes },
				io__write_string("stitched-together instruction sequence:\n\n")
			)
		),
		vn_debug__dump_instrs(Instrs),
		io__write_string("\nTuple data:\n\n"),
		{ Tuple = tuple(N, CtrlMap, _, _, ParMap) },
		vn_debug__tuple_entries(0, N, CtrlMap, ParMap),
		io__write_string("\nTuple data ends\n\n")
	;
		{ Flag = no }
	).

:- pred vn_debug__tuple_entries(int, int, ctrlmap, parmap,
	io__state, io__state).
:- mode vn_debug__tuple_entries(in, in, in, in, di, uo) is det.

vn_debug__tuple_entries(N, Max, CtrlMap, ParMap) -->
	( { N < Max } ->
		{ map__lookup(CtrlMap, N, VnInstr) },
		{ map__lookup(ParMap, N, Parallels) },
		{ opt_debug__dump_vninstr(VnInstr, I_str) },
		io__write_string("\n-----------\n"),
		io__write_string(I_str),
		io__write_string(":\n"),
		vn_debug__parallel_msgs(Parallels),
		{ N1 is N + 1 },
		vn_debug__tuple_entries(N1, Max, CtrlMap, ParMap)
	;
		[]
	).

vn_debug__livemap_msg(Livemap) -->
	vn_debug__livemap_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_livemap(Livemap, L_str) },
		io__write_string("\n\nLivemap:\n\n"),
		io__write_string(L_str)
	;
		{ Flag = no }
	).

vn_debug__fragment_msg(Instr) -->
	vn_debug__start_msg_flag(Flag),
	(
		{ Flag = yes },
		io__write_string("\nin value_number__optimize_fragment starting at\n"),
		output_instruction(Instr),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__restart_msg(Instr) -->
	vn_debug__start_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_fullinstr(Instr, I_str) },
		io__write_string("starting again at "),  
		io__write_string(I_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__divide_msg(Instr) -->
	vn_debug__start_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_fullinstr(Instr, I_str) },
		io__write_string("dividing the block at "),  
		io__write_string(I_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__failure_msg(Instr, Cause) -->
	vn_debug__failure_msg_flag(Flag),
	(
		{ Flag = yes },
		io__write_string("FAILURE of VN consistency check "),
		io__write_string("in fragment starting at\n"),
		output_instruction(Instr),
		io__write_string("\n"),
		io__write_string("Cause: "),
		io__write_string(Cause),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__parallel_msgs([]) --> [].
vn_debug__parallel_msgs([Parallel | Parallels]) -->
	vn_debug__parallel_msg(Parallel),
	vn_debug__parallel_msgs(Parallels).

vn_debug__parallel_msg(parallel(OldLabel, NewLabel, ParEntries)) -->
	vn_debug__parallel_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_label(OldLabel, O_str) },
		{ opt_debug__dump_label(NewLabel, N_str) },
		io__write_string("\nparallel from "),
		io__write_string(O_str),
		io__write_string(" to "),
		io__write_string(N_str),
		io__write_string("\n"),
		vn_debug__parentry_msg(ParEntries)
	;
		{ Flag = no }
	).

:- pred vn_debug__parentry_msg(list(parentry), io__state, io__state).
:- mode vn_debug__parentry_msg(in, di, uo) is det.

vn_debug__parentry_msg([]) --> [].
vn_debug__parentry_msg([Lval - Rvals | ParEntries]) -->
	{ opt_debug__dump_lval(Lval, L_str) },
	{ opt_debug__dump_rvals(Rvals, R_str) },
	io__write_string(L_str),
	io__write_string(" -> "),
	io__write_string(R_str),
	io__write_string("\n"),
	vn_debug__parentry_msg(ParEntries).

vn_debug__computed_goto_msg(Labels, Parallels, Pairs) -->
	vn_debug__parallel_msg_flag(Flag),
	(
		{ Flag = yes },
		io__write_string("computed goto labels:\n"),
		vn_debug__dump_labels(Labels),
		io__write_string("computed goto parallels:\n"),
		vn_debug__dump_parallels(Parallels),
		io__write_string("computed goto pairs:\n"),
		vn_debug__dump_label_parallel_pairs(Pairs),
		io__write_string("computed goto message end\n")
	;
		{ Flag = no }
	).

:- pred vn_debug__dump_labels(list(label), io__state, io__state).
:- mode vn_debug__dump_labels(in, di, uo) is det.

vn_debug__dump_labels([]) --> [].
vn_debug__dump_labels([Label | Labels]) -->
	{ opt_debug__dump_label(Label, Lstr) },
	io__write_string(Lstr),
	io__write_string("\n"),
	vn_debug__dump_labels(Labels).

:- pred vn_debug__dump_parallels(list(parallel), io__state, io__state).
:- mode vn_debug__dump_parallels(in, di, uo) is det.

vn_debug__dump_parallels([]) --> [].
vn_debug__dump_parallels([Parallel | Parallels]) -->
	vn_debug__parallel_msg(Parallel),
	vn_debug__dump_parallels(Parallels).

:- pred vn_debug__dump_label_parallel_pairs(assoc_list(label, maybe(parallel)),
	io__state, io__state).
:- mode vn_debug__dump_label_parallel_pairs(in, di, uo) is det.

vn_debug__dump_label_parallel_pairs([]) --> [].
vn_debug__dump_label_parallel_pairs([Label - MaybeParallel | Pairs]) -->
	{ opt_debug__dump_label(Label, Lstr) },
	io__write_string(Lstr),
	io__write_string(": "),
	(
		{ MaybeParallel = yes(Parallel) },
		vn_debug__parallel_msg(Parallel)
	;
		{ MaybeParallel = no },
		io__write_string("no\n")
	),
	vn_debug__dump_label_parallel_pairs(Pairs).

vn_debug__order_start_msg(Ctrlmap, Flushmap, VnTables) -->
	vn_debug__order_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_ctrlmap(Ctrlmap, Ctrl_str) },
		{ opt_debug__dump_flushmap(Flushmap, Flush_str) },
		{ opt_debug__dump_tables(VnTables, Tables_str) },
		io__write_string("\n\n"),
		io__write_string(Ctrl_str),
		io__write_string(Flush_str),
		io__write_string(Tables_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__order_sink_msg(Sink) -->
	vn_debug__order_sink_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_node(Sink, S_str) },
		io__write_string("sink_before_redef for node "),
		io__write_string(S_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__order_link_msg(From, To, User) -->
	vn_debug__order_sink_msg_flag(Flag),
	(
		{ Flag = yes },
		(
			{ User = yes },
			io__write_string("adding user link from ")
		;
			{ User = no },
			io__write_string("adding alias link from ")
		),
		{ opt_debug__dump_node(From, F_str) },
		{ opt_debug__dump_node(To, T_str) },
		io__write_string(F_str),
		io__write_string(" to "),
		io__write_string(T_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__order_antidep_msg(CtrlNode, Node) -->
	vn_debug__order_sink_msg_flag(Flag),
	(
		{ Flag = yes },
		io__write_string("anti dependency from "),
		{ opt_debug__dump_node(CtrlNode, C_str) },
		io__write_string(C_str),
		io__write_string(" to "),
		{ opt_debug__dump_node(Node, N_str) },
		io__write_string(N_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__order_map_msg(MustSuccmap, MustPredmap, Succmap, Predmap) -->
	vn_debug__order_map_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_node_relmap(MustSuccmap, MS_str) },
		{ opt_debug__dump_node_relmap(MustPredmap, MP_str) },
		{ opt_debug__dump_node_relmap(Succmap, S_str) },
		{ opt_debug__dump_node_relmap(Predmap, P_str) },
		io__write_string("\nMustSuccmap:\n"),
		io__write_string(MS_str),
		io__write_string("\nMustPredmap:\n"),
		io__write_string(MP_str),
		io__write_string("\nSuccmap:\n"),
		io__write_string(S_str),
		io__write_string("\nPredmap:\n"),
		io__write_string(P_str)
	;
		{ Flag = no }
	).

vn_debug__order_order_msg(Order) -->
	vn_debug__order_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_longnodelist(Order, O_str) },
		io__write_string("\nOrder:\n"),
		io__write_string(O_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__order_equals_msg(Msg, Order) -->
	vn_debug__order_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_nodelist(Order, O_str) },
		io__write_string(Msg),
		io__write_string(O_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__cost_header_msg(Header) -->
	vn_debug__cost_msg_flag(Flag),
	opt_debug__msg(Flag, Header).

vn_debug__cost_msg(Used, OrigCost, VnCost) -->
	vn_debug__cost_msg_flag(Flag),
	(
		{ Flag = yes },
		{ string__int_to_string(OrigCost, OC_str) },
		{ string__int_to_string(VnCost, VC_str) },
		io__write_string("\n"),
		io__write_string("Old cost: "),
		io__write_string(OC_str),
		io__write_string("\n"),
		io__write_string("New cost: "),
		io__write_string(VC_str),
		io__write_string("\n"),
		( { VnCost < OrigCost } ->
			io__write_string("Result: cost improvement\n")
		;
			io__write_string("Result: no cost improvement\n")
		),
		( { Used = yes } ->
			io__write_string("Used: yes\n")
		;
			io__write_string("Used: no\n")
		)
	;
		{ Flag = no }
	).

vn_debug__cost_detail_msg(Uinstr, InstrCost, CostNow) -->
	vn_debug__cost_msg_flag(Flag),
	(
		{ Flag = yes },
		{ string__int_to_string(InstrCost, InstrCostStr) },
		{ string__int_to_string(CostNow, CostNowStr) },
		io__write_string(InstrCostStr),
		io__write_string("\t"),
		io__write_string(CostNowStr),
		io__write_string("\t"),
		output_instruction(Uinstr)
	;
		{ Flag = no }
	).

vn_debug__flush_start_msg(Node) -->
	vn_debug__flush_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_node(Node, N_str) },
		io__write_string("\nat node "),
		io__write_string(N_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__flush_also_msg(Vnlval) -->
	vn_debug__flush_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_vnlval(Vnlval, Vn_str) },
		io__write_string("took care of node_lval for "),
		io__write_string(Vn_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn_debug__flush_end_msg(Instrs, VnTables) -->
	vn_debug__flush_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_fullinstrs(Instrs, I_str) },
		{ opt_debug__dump_useful_vns(VnTables, U_str) },
		% { opt_debug__dump_useful_locs(VnTables, L_str) },
		% { opt_debug__dump_vn_locs(VnTables, V_str) },
		io__write_string("generated instrs:\n"),
		io__write_string(I_str),
		io__write_string("new use info\n"),
		io__write_string(U_str)
		% io__write_string("new location content info\n"),
		% io__write_string(L_str),
		% io__write_string("new vn location info\n"),
		% io__write_string(V_str)
	;
		{ Flag = no }
	).

vn_debug__dump_instrs(Instrs) -->
	vn_debug__parallel_msg_flag(Flag1),
	vn_debug__cost_msg_flag(Flag2),
	{ bool__or(Flag1, Flag2, Flag) },
	opt_debug__dump_instrs(Flag, Instrs).

%-----------------------------------------------------------------------------%

:- pred vn_debug__tuple_msg_flag(bool, io__state, io__state).
:- mode vn_debug__tuple_msg_flag(out, di, uo) is det.

:- pred vn_debug__livemap_msg_flag(bool, io__state, io__state).
:- mode vn_debug__livemap_msg_flag(out, di, uo) is det.

:- pred vn_debug__parallel_msg_flag(bool, io__state, io__state).
:- mode vn_debug__parallel_msg_flag(out, di, uo) is det.

:- pred vn_debug__order_sink_msg_flag(bool, io__state, io__state).
:- mode vn_debug__order_sink_msg_flag(out, di, uo) is det.

:- pred vn_debug__order_msg_flag(bool, io__state, io__state).
:- mode vn_debug__order_msg_flag(out, di, uo) is det.

:- pred vn_debug__order_map_msg_flag(bool, io__state, io__state).
:- mode vn_debug__order_map_msg_flag(out, di, uo) is det.

:- pred vn_debug__cost_msg_flag(bool, io__state, io__state).
:- mode vn_debug__cost_msg_flag(out, di, uo) is det.

:- pred vn_debug__flush_msg_flag(bool, io__state, io__state).
:- mode vn_debug__flush_msg_flag(out, di, uo) is det.

:- pred vn_debug__start_msg_flag(bool, io__state, io__state).
:- mode vn_debug__start_msg_flag(out, di, uo) is det.

:- pred vn_debug__failure_msg_flag(bool, io__state, io__state).
:- mode vn_debug__failure_msg_flag(out, di, uo) is det.

%-----------------------------------------------------------------------------%

vn_debug__tuple_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 512 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn_debug__livemap_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 256 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn_debug__parallel_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 128 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn_debug__order_sink_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 64 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn_debug__order_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 32 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn_debug__order_map_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 16 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn_debug__cost_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 8 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn_debug__flush_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 4 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn_debug__start_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 2 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn_debug__failure_msg_flag(Flag) -->
	globals__io_lookup_int_option(debug_vn, DebugVn),
	{ Bit is DebugVn /\ 1 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

%-----------------------------------------------------------------------------%
