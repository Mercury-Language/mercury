%-----------------------------------------------------------------------------%

% Vn_debug.nl - debugging messages for vn_order and vn_flush.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_debug.

:- interface.

:- import_module vn_type, vn_table.
:- import_module atsort, map, list.

:- pred vn__livemap_msg(livemap).
:- mode vn__livemap_msg(in) is det.

:- pred vn__order_start_msg(ctrlmap, flushmap, vn_tables).
:- mode vn__order_start_msg(in, in, in) is det.

:- pred vn__order_sink_msg(vn_node).
:- mode vn__order_sink_msg(in) is det.

:- pred vn__order_link_msg(vn_node, vn_node, bool).
:- mode vn__order_link_msg(in, in, in) is det.

:- pred vn__order_map_msg(relmap(vn_node), relmap(vn_node),
	relmap(vn_node), relmap(vn_node)).
:- mode vn__order_map_msg(in, in, in, in) is det.

:- pred vn__order_order_msg(list(vn_node)).
:- mode vn__order_order_msg(in) is det.

:- pred vn__order_cost_msg(int, int).
:- mode vn__order_cost_msg(in, in) is det.

:- pred vn__flush_start_msg(vn_node).
:- mode vn__flush_start_msg(in) is det.

:- pred vn__flush_also_msg(vnlval).
:- mode vn__flush_also_msg(in) is det.

:- pred vn__flush_end_msg(list(instruction), vn_tables).
:- mode vn__flush_end_msg(in, in) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module opt_debug, string, int, std_util.

vn__livemap_msg(Livemap) :-
	vn__livemap_msg_flag(Flag),
	(
		Flag = yes,
		opt_debug__dump_livemap(Livemap, L_str),
		opt_debug__write("\n\nLivemap:\n\n"),
		opt_debug__write(L_str)
	;
		Flag = no
	).

vn__order_start_msg(Ctrlmap, Flushmap, Vn_tables) :-
	vn__order_msg_flag(Flag),
	(
		Flag = yes,
		opt_debug__write("\n\n"),
		opt_debug__dump_ctrlmap(Ctrlmap, Ctrl_str),
		opt_debug__write(Ctrl_str),
		opt_debug__dump_flushmap(Flushmap, Flush_str),
		opt_debug__write(Flush_str),
		opt_debug__dump_tables(Vn_tables, Tables_str),
		opt_debug__write(Tables_str),
		opt_debug__write("\n")
	;
		Flag = no
	).

vn__order_sink_msg(Sink) :-
	vn__order_sink_msg_flag(Flag),
	(
		Flag = yes,
		opt_debug__write("sink_before_redef for node "),
		opt_debug__dump_node(Sink, S_str),
		opt_debug__write(S_str),
		opt_debug__write("\n")
	;
		Flag = no
	).

vn__order_link_msg(From, To, User) :-
	vn__order_sink_msg_flag(Flag),
	(
		Flag = yes,
		(
			User = yes,
			opt_debug__write("adding user link from ")
		;
			User = no,
			opt_debug__write("adding alias link from ")
		),
		opt_debug__dump_node(From, F_str),
		opt_debug__write(F_str),
		opt_debug__write(" to "),
		opt_debug__dump_node(To, T_str),
		opt_debug__write(T_str),
		opt_debug__write("\n")
	;
		Flag = no
	).

vn__order_map_msg(MustSuccmap, MustPredmap, Succmap, Predmap) :-
	vn__order_map_msg_flag(Flag),
	(
		Flag = yes,
		opt_debug__write("\nMustSuccmap:\n"),
		opt_debug__dump_node_relmap(MustSuccmap, MS_str),
		opt_debug__write(MS_str),
		opt_debug__write("\nMustPredmap:\n"),
		opt_debug__dump_node_relmap(MustPredmap, MP_str),
		opt_debug__write(MP_str),
		opt_debug__write("\nSuccmap:\n"),
		opt_debug__dump_node_relmap(Succmap, S_str),
		opt_debug__write(S_str),
		opt_debug__write("\nPredmap:\n"),
		opt_debug__dump_node_relmap(Predmap, P_str),
		opt_debug__write(P_str)
	;
		Flag = no
	).

vn__order_order_msg(Order) :-
	vn__order_msg_flag(Flag),
	(
		Flag = yes,
		opt_debug__dump_longnodelist(Order, O_str),
		opt_debug__write("\nOrder:\n"),
		opt_debug__write(O_str),
		opt_debug__write("\n")
	;
		Flag = no
	).

vn__order_cost_msg(OrigCost, VnCost) :-
	vn__order_cost_msg_flag(Flag),
	(
		Flag = yes,
		string__int_to_string(OrigCost, OC_str),
		string__int_to_string(VnCost, VC_str),
		opt_debug__write("Old cost: "),
		opt_debug__write(OC_str),
		opt_debug__write("\n"),
		opt_debug__write("New cost: "),
		opt_debug__write(VC_str),
		opt_debug__write("\n"),
		( VnCost < OrigCost ->
			opt_debug__write("Result: cost improvement\n")
		;
			opt_debug__write("Result: no cost improvement\n")
		)
	;
		Flag = no
	).

vn__flush_start_msg(Node) :-
	vn__flush_msg_flag(Flag),
	(
		Flag = yes,
		opt_debug__write("\nat node "),
		opt_debug__dump_node(Node, N_str),
		opt_debug__write(N_str),
		opt_debug__write("\n")
	;
		Flag = no
	).

vn__flush_also_msg(Vnlval) :-
	vn__flush_msg_flag(Flag),
	(
		Flag = yes,
		opt_debug__write("took care of node_lval for "),
		opt_debug__dump_vnlval(Vnlval, Vn_str),
		opt_debug__write(Vn_str),
		opt_debug__write("\n")
	;
		Flag = no
	).

vn__flush_end_msg(Instrs, Vn_tables) :-
	vn__flush_msg_flag(Flag),
	(
		Flag = yes,
		opt_debug__write("generated instrs:\n"),
		opt_debug__dump_fullinstrs(Instrs, I_str),
		opt_debug__write(I_str),
		opt_debug__write("new use info\n"),
		opt_debug__dump_useful_vns(Vn_tables, U_str),
		opt_debug__write(U_str)
	;
		Flag = no
	).

%-----------------------------------------------------------------------------%

:- pred vn__livemap_msg_flag(bool).
:- mode vn__livemap_msg_flag(out) is det.

:- pred vn__order_sink_msg_flag(bool).
:- mode vn__order_sink_msg_flag(out) is det.

:- pred vn__order_msg_flag(bool).
:- mode vn__order_msg_flag(out) is det.

:- pred vn__order_map_msg_flag(bool).
:- mode vn__order_map_msg_flag(out) is det.

:- pred vn__order_cost_msg_flag(bool).
:- mode vn__order_cost_msg_flag(out) is det.

:- pred vn__flush_msg_flag(bool).
:- mode vn__flush_msg_flag(out) is det.

%-----------------------------------------------------------------------------%

vn__livemap_msg_flag(no).
vn__order_sink_msg_flag(no).
vn__order_msg_flag(no).
vn__order_map_msg_flag(no).
vn__order_cost_msg_flag(yes).
vn__flush_msg_flag(no).

%-----------------------------------------------------------------------------%
