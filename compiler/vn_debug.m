%-----------------------------------------------------------------------------%

% Vn_debug.nl - debugging messages for vn_order and vn_flush.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_debug.

:- interface.

:- import_module vn_type, vn_table.
:- import_module atsort, map, list, io.

:- pred vn__livemap_msg(livemap, io__state, io__state).
:- mode vn__livemap_msg(in, di, uo) is det.

:- pred vn__order_start_msg(ctrlmap, flushmap, vn_tables, io__state, io__state).
:- mode vn__order_start_msg(in, in, in, di, uo) is det.

:- pred vn__order_sink_msg(vn_node, io__state, io__state).
:- mode vn__order_sink_msg(in, di, uo) is det.

:- pred vn__order_link_msg(vn_node, vn_node, bool, io__state, io__state).
:- mode vn__order_link_msg(in, in, in, di, uo) is det.

:- pred vn__order_map_msg(relmap(vn_node), relmap(vn_node),
	relmap(vn_node), relmap(vn_node), io__state, io__state).
:- mode vn__order_map_msg(in, in, in, in, di, uo) is det.

:- pred vn__order_order_msg(list(vn_node), io__state, io__state).
:- mode vn__order_order_msg(in, di, uo) is det.

:- pred vn__cost_header_msg(string, io__state, io__state).
:- mode vn__cost_header_msg(in, di, uo) is det.

:- pred vn__cost_msg(int, int, io__state, io__state).
:- mode vn__cost_msg(in, in, di, uo) is det.

:- pred vn__cost_detail_msg(instr, int, int, io__state, io__state).
:- mode vn__cost_detail_msg(in, in, in, di, uo) is det.

:- pred vn__order_restart_msg(instruction, io__state, io__state).
:- mode vn__order_restart_msg(in, di, uo) is det.

:- pred vn__flush_start_msg(vn_node, io__state, io__state).
:- mode vn__flush_start_msg(in, di, uo) is det.

:- pred vn__flush_also_msg(vnlval, io__state, io__state).
:- mode vn__flush_also_msg(in, di, uo) is det.

:- pred vn__flush_end_msg(list(instruction), vn_tables, io__state, io__state).
:- mode vn__flush_end_msg(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, globals, options, opt_debug, string, int, std_util.

vn__livemap_msg(Livemap) -->
	vn__livemap_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_livemap(Livemap, L_str) },
		io__write_string("\n\nLivemap:\n\n"),
		io__write_string(L_str)
	;
		{ Flag = no }
	).

vn__order_start_msg(Ctrlmap, Flushmap, Vn_tables) -->
	vn__order_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_ctrlmap(Ctrlmap, Ctrl_str) },
		{ opt_debug__dump_flushmap(Flushmap, Flush_str) },
		{ opt_debug__dump_tables(Vn_tables, Tables_str) },
		io__write_string("\n\n"),
		io__write_string(Ctrl_str),
		io__write_string(Flush_str),
		io__write_string(Tables_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn__order_sink_msg(Sink) -->
	vn__order_sink_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_node(Sink, S_str) },
		io__write_string("sink_before_redef for node "),
		io__write_string(S_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn__order_link_msg(From, To, User) -->
	vn__order_sink_msg_flag(Flag),
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

vn__order_map_msg(MustSuccmap, MustPredmap, Succmap, Predmap) -->
	vn__order_map_msg_flag(Flag),
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

vn__order_order_msg(Order) -->
	vn__order_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_longnodelist(Order, O_str) },
		io__write_string("\nOrder:\n"),
		io__write_string(O_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn__cost_header_msg(Header) -->
	vn__cost_msg_flag(Flag),
	(
		{ Flag = yes },
		io__write_string("\n"),
		io__write_string(Header),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn__cost_msg(OrigCost, VnCost) -->
	vn__cost_msg_flag(Flag),
	(
		{ Flag = yes },
		{ string__int_to_string(OrigCost, OC_str) },
		{ string__int_to_string(VnCost, VC_str) },
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
		)
	;
		{ Flag = no }
	).

vn__cost_detail_msg(Uinstr, InstrCost, CostNow) -->
	vn__cost_msg_flag(Flag),
	(
		{ Flag = yes },
		{ string__int_to_string(InstrCost, InstrCostStr) },
		{ string__int_to_string(CostNow, CostNowStr) },
		io__write_string(InstrCostStr),
		io__write_string("\t"),
		io__write_string(CostNowStr),
		io__write_string("\t"),
		output_instruction(Uinstr),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn__order_restart_msg(Instr) -->
	vn__order_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_fullinstr(Instr, I_str) },
		io__write_string("starting again at "),  
		io__write_string(I_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn__flush_start_msg(Node) -->
	vn__flush_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_node(Node, N_str) },
		io__write_string("\nat node "),
		io__write_string(N_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn__flush_also_msg(Vnlval) -->
	vn__flush_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_vnlval(Vnlval, Vn_str) },
		io__write_string("took care of node_lval for "),
		io__write_string(Vn_str),
		io__write_string("\n")
	;
		{ Flag = no }
	).

vn__flush_end_msg(Instrs, Vn_tables) -->
	vn__flush_msg_flag(Flag),
	(
		{ Flag = yes },
		{ opt_debug__dump_fullinstrs(Instrs, I_str) },
		{ opt_debug__dump_useful_vns(Vn_tables, U_str) },
		io__write_string("generated instrs:\n"),
		io__write_string(I_str),
		io__write_string("new use info\n"),
		io__write_string(U_str)
	;
		{ Flag = no }
	).

%-----------------------------------------------------------------------------%

:- pred vn__livemap_msg_flag(bool, io__state, io__state).
:- mode vn__livemap_msg_flag(out, di, uo) is det.

:- pred vn__order_sink_msg_flag(bool, io__state, io__state).
:- mode vn__order_sink_msg_flag(out, di, uo) is det.

:- pred vn__order_msg_flag(bool, io__state, io__state).
:- mode vn__order_msg_flag(out, di, uo) is det.

:- pred vn__order_map_msg_flag(bool, io__state, io__state).
:- mode vn__order_map_msg_flag(out, di, uo) is det.

:- pred vn__cost_msg_flag(bool, io__state, io__state).
:- mode vn__cost_msg_flag(out, di, uo) is det.

:- pred vn__flush_msg_flag(bool, io__state, io__state).
:- mode vn__flush_msg_flag(out, di, uo) is det.

%-----------------------------------------------------------------------------%

vn__livemap_msg_flag(Flag) -->
	globals__io_lookup_int_option(vndebug, Vndebug),
	{ Bit is Vndebug /\ 32 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn__order_sink_msg_flag(Flag) -->
	globals__io_lookup_int_option(vndebug, Vndebug),
	{ Bit is Vndebug /\ 16 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn__order_msg_flag(Flag) -->
	globals__io_lookup_int_option(vndebug, Vndebug),
	{ Bit is Vndebug /\ 8 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn__order_map_msg_flag(Flag) -->
	globals__io_lookup_int_option(vndebug, Vndebug),
	{ Bit is Vndebug /\ 4 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn__cost_msg_flag(Flag) -->
	globals__io_lookup_int_option(vndebug, Vndebug),
	{ Bit is Vndebug /\ 2 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

vn__flush_msg_flag(Flag) -->
	globals__io_lookup_int_option(vndebug, Vndebug),
	{ Bit is Vndebug /\ 1 },
	{ Bit = 0 -> Flag = no ; Flag = yes }.

%-----------------------------------------------------------------------------%
