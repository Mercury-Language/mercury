%-----------------------------------------------------------------------------%

% Debugging support for LLDS to LLDS peephole optimization.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module opt_debug.

:- interface.
:- import_module llds, value_number, peephole, list, std_util.

:- pred opt_debug__dump_tables(vn_tables, string).
:- mode opt_debug__dump_tables(in, out) is det.

:- pred opt_debug__dump_lval_to_vn(assoc_list(vn_lval, vn), string).
:- mode opt_debug__dump_lval_to_vn(in, out) is det.

:- pred opt_debug__dump_rval_to_vn(assoc_list(vn_rval, vn), string).
:- mode opt_debug__dump_rval_to_vn(in, out) is det.

:- pred opt_debug__dump_vn_to_rval(assoc_list(vn, vn_rval), string).
:- mode opt_debug__dump_vn_to_rval(in, out) is det.

:- pred opt_debug__dump_vn_to_uses(assoc_list(vn, int), string).
:- mode opt_debug__dump_vn_to_uses(in, out) is det.

:- pred opt_debug__dump_vn_to_locs(assoc_list(vn, list(vn_lval)), string).
:- mode opt_debug__dump_vn_to_locs(in, out) is det.

:- pred opt_debug__dump_vn(vn, string).
:- mode opt_debug__dump_vn(in, out) is det.

:- pred opt_debug__dump_vn_locs(list(vn_lval), string).
:- mode opt_debug__dump_vn_locs(in, out) is det.

:- pred opt_debug__dump_reg(reg, string).
:- mode opt_debug__dump_reg(in, out) is det.

:- pred opt_debug__dump_vn_lval(vn_lval, string).
:- mode opt_debug__dump_vn_lval(in, out) is det.

:- pred opt_debug__dump_vn_rval(vn_rval, string).
:- mode opt_debug__dump_vn_rval(in, out) is det.

:- pred opt_debug__dump_const(rval_const, string).
:- mode opt_debug__dump_const(in, out) is det.

:- pred opt_debug__dump_unop(unary_op, string).
:- mode opt_debug__dump_unop(in, out) is det.

:- pred opt_debug__dump_binop(operator, string).
:- mode opt_debug__dump_binop(in, out) is det.

:- pred opt_debug__dump_label(label, string).
:- mode opt_debug__dump_label(in, out) is det.

:- pred opt_debug__print_tailmap(tailmap).
:- mode opt_debug__print_tailmap(in) is det.

:- pred opt_debug__print_instmap(tailmap).
:- mode opt_debug__print_instmap(in) is det.

:- pred opt_debug__print_proclist(list(pair(label, list(instruction)))).
:- mode opt_debug__print_proclist(in) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, string.

opt_debug__dump_lval_to_vn([], "").
opt_debug__dump_lval_to_vn([Vn_lval - Vn | Lval_to_vn_list], Str) :-
	opt_debug__dump_lval_to_vn(Lval_to_vn_list, Tail_str),
	opt_debug__dump_vn_lval(Vn_lval, Vn_lval_str),
	opt_debug__dump_vn(Vn, Vn_str),
	string__append_list([Vn_lval_str, " -> ", Vn_str, "\n", Tail_str], Str).

opt_debug__dump_tables(Vn_tables, Comment) :-
	Vn_tables = vn_tables(Next_vn,
		Lval_to_vn_table, Rval_to_vn_table,
		Vn_to_rval_table, Vn_to_uses_table,
		Vn_to_locs_table, Loc_to_vn_table),
	string__int_to_string(Next_vn, Next_vn_str),
	map__to_assoc_list(Lval_to_vn_table, Lval_to_vn_list),
	map__to_assoc_list(Rval_to_vn_table, Rval_to_vn_list),
	map__to_assoc_list(Vn_to_rval_table, Vn_to_rval_list),
	map__to_assoc_list(Vn_to_uses_table, Vn_to_uses_list),
	map__to_assoc_list(Vn_to_locs_table, Vn_to_locs_list),
	map__to_assoc_list(Loc_to_vn_table,  Loc_to_vn_list),
	opt_debug__dump_lval_to_vn(Lval_to_vn_list, Lval_to_vn_str),
	opt_debug__dump_rval_to_vn(Rval_to_vn_list, Rval_to_vn_str),
	opt_debug__dump_vn_to_rval(Vn_to_rval_list, Vn_to_rval_str),
	opt_debug__dump_vn_to_uses(Vn_to_uses_list, Vn_to_uses_str),
	opt_debug__dump_vn_to_locs(Vn_to_locs_list, Vn_to_locs_str),
	opt_debug__dump_lval_to_vn( Loc_to_vn_list,  Loc_to_vn_str),
	string__append_list([
		"Next vn\n",      Next_vn_str,
		"\nLval to vn\n", Lval_to_vn_str,
		"\nRval to vn\n", Rval_to_vn_str,
		"\nVn to rval\n", Vn_to_rval_str,
		"\nVn to uses\n", Vn_to_uses_str,
		"\nVn to locs\n", Vn_to_locs_str,
		"\nLoc to vn\n",  Loc_to_vn_str], Comment).

opt_debug__dump_rval_to_vn([], "").
opt_debug__dump_rval_to_vn([Vn_rval - Vn | Rval_to_vn_list], Str) :-
	opt_debug__dump_rval_to_vn(Rval_to_vn_list, Tail_str),
	opt_debug__dump_vn_rval(Vn_rval, Vn_rval_str),
	opt_debug__dump_vn(Vn, Vn_str),
	string__append_list([Vn_rval_str, " -> ", Vn_str, "\n", Tail_str], Str).

opt_debug__dump_vn_to_rval([], "").
opt_debug__dump_vn_to_rval([Vn - Vn_rval | Vn_to_rval_list], Str) :-
	opt_debug__dump_vn_to_rval(Vn_to_rval_list, Tail_str),
	opt_debug__dump_vn(Vn, Vn_str),
	opt_debug__dump_vn_rval(Vn_rval, Vn_rval_str),
	string__append_list([Vn_str, " -> ", Vn_rval_str, "\n", Tail_str], Str).

opt_debug__dump_vn_to_uses([], "").
opt_debug__dump_vn_to_uses([Vn - Uses | Vn_to_uses_list], Str) :-
	opt_debug__dump_vn_to_uses(Vn_to_uses_list, Tail_str),
	opt_debug__dump_vn(Vn, Vn_str),
	string__int_to_string(Uses, Uses_str),
	string__append_list([Vn_str, " -> ", Uses_str, "\n", Tail_str], Str).

opt_debug__dump_vn_to_locs([], "").
opt_debug__dump_vn_to_locs([Vn - Vn_locs | Vn_to_locs_list], Str) :-
	opt_debug__dump_vn_to_locs(Vn_to_locs_list, Tail_str),
	opt_debug__dump_vn(Vn, Vn_str),
	opt_debug__dump_vn_locs(Vn_locs, Vn_locs_str),
	string__append_list([Vn_str, " -> ", Vn_locs_str, "\n", Tail_str], Str).

opt_debug__dump_vn(Vn, Str) :-
	string__int_to_string(Vn, Str).

opt_debug__dump_vn_locs([], "").
opt_debug__dump_vn_locs([Lval | Lvals], Str) :-
	opt_debug__dump_vn_lval(Lval, Lval_str),
	opt_debug__dump_vn_locs(Lvals, Tail_str),
	string__append_list([" @", Lval_str, Tail_str], Str).

opt_debug__dump_reg(r(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["r(", N_str, ")"], Str).
opt_debug__dump_reg(f(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["f(", N_str, ")"], Str).

opt_debug__dump_vn_lval(vn_reg(R), Str) :-
	opt_debug__dump_reg(R, R_str),
	string__append_list(["vn_reg(", R_str, ")"], Str).
opt_debug__dump_vn_lval(vn_stackvar(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["vn_stackvar(", N_str, ")"], Str).
opt_debug__dump_vn_lval(vn_framevar(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["vn_framevar(", N_str, ")"], Str).
opt_debug__dump_vn_lval(vn_succip, Str) :-
	string__append_list(["vn_succip"], Str).
opt_debug__dump_vn_lval(vn_maxfr, Str) :-
	string__append_list(["vn_maxfr"], Str).
opt_debug__dump_vn_lval(vn_curredoip, Str) :-
	string__append_list(["vn_curredoip"], Str).
opt_debug__dump_vn_lval(vn_hp, Str) :-
	string__append_list(["vn_hp"], Str).
opt_debug__dump_vn_lval(vn_sp, Str) :-
	string__append_list(["vn_sp"], Str).
opt_debug__dump_vn_lval(vn_field(T, N, F), Str) :-
	string__int_to_string(T, T_str),
	string__int_to_string(N, N_str),
	string__int_to_string(F, F_str),
	string__append_list(["vn_field(", T_str, ", ", N_str, ", ",
		F_str, ")"], Str).
opt_debug__dump_vn_lval(vn_temp(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["vn_temp(", N_str, ")"], Str).

opt_debug__dump_vn_rval(vn_origlval(Vn_lval), Str) :-
	opt_debug__dump_vn_lval(Vn_lval, Lval_str),
	string__append_list(["vn_origlval(", Lval_str, ")"], Str).
opt_debug__dump_vn_rval(vn_mkword(T, N), Str) :-
	string__int_to_string(T, T_str),
	string__int_to_string(N, N_str),
	string__append_list(["vn_mkword(", T_str, ", ", N_str, ")"], Str).
opt_debug__dump_vn_rval(vn_const(C), Str) :-
	opt_debug__dump_const(C, C_str),
	string__append_list(["vn_const(", C_str, ")"], Str).
opt_debug__dump_vn_rval(vn_field(T, N, F), Str) :-
	string__int_to_string(T, T_str),
	string__int_to_string(N, N_str),
	string__int_to_string(F, F_str),
	string__append_list(["vn_field(", T_str, ", ", N_str, ", ",
		F_str, ")"], Str).
opt_debug__dump_vn_rval(vn_unop(O, N), Str) :-
	opt_debug__dump_unop(O, O_str),
	string__int_to_string(N, N_str),
	string__append_list(["vn_unop(", O_str, ", ", N_str, ")"], Str).
opt_debug__dump_vn_rval(vn_binop(O, N1, N2), Str) :-
	opt_debug__dump_binop(O, O_str),
	string__int_to_string(N1, N1_str),
	string__int_to_string(N2, N2_str),
	string__append_list(["vn_binop(", O_str, ", ", N1_str, ", ",
		N2_str, ")"], Str).

opt_debug__dump_const(true, "true").
opt_debug__dump_const(false, "false").
opt_debug__dump_const(int_const(I), Str) :-
	string__int_to_string(I, Str).
opt_debug__dump_const(string_const(I), Str) :-
	string__append_list(["\"", I, "\""], Str).

opt_debug__dump_unop(mktag, "mktag").
opt_debug__dump_unop(tag, "tag").
opt_debug__dump_unop(mkbody, "mkbody").
opt_debug__dump_unop(body, "body").
opt_debug__dump_unop(not, "not").
opt_debug__dump_unop(hash_string, "hash_string").
opt_debug__dump_unop(bitwise_complement, "bitwise_complement").
opt_debug__dump_unop(cast_to_unsigned, "cast_to_unsigned").

opt_debug__dump_binop(Op, String) :-
	llds__operator_to_string(Op, String).

