%-----------------------------------------------------------------------------%

% Debugging support for LLDS to LLDS peephole optimization.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module opt_debug.

:- interface.
:- import_module llds, value_number, opt_util, list, std_util, int.

:- pred opt_debug__dump_tables(vn_tables, string).
:- mode opt_debug__dump_tables(in, out) is det.

:- pred opt_debug__dump_lval_to_vn(assoc_list(vnlval, vn), string).
:- mode opt_debug__dump_lval_to_vn(in, out) is det.

:- pred opt_debug__dump_rval_to_vn(assoc_list(vnrval, vn), string).
:- mode opt_debug__dump_rval_to_vn(in, out) is det.

:- pred opt_debug__dump_vn_to_rval(assoc_list(vn, vnrval), string).
:- mode opt_debug__dump_vn_to_rval(in, out) is det.

:- pred opt_debug__dump_vn_to_uses(assoc_list(vn, int), string).
:- mode opt_debug__dump_vn_to_uses(in, out) is det.

:- pred opt_debug__dump_vn_to_locs(assoc_list(vn, list(vnlval)), string).
:- mode opt_debug__dump_vn_to_locs(in, out) is det.

:- pred opt_debug__dump_vn(vn, string).
:- mode opt_debug__dump_vn(in, out) is det.

:- pred opt_debug__dump_vn_locs(list(vnlval), string).
:- mode opt_debug__dump_vn_locs(in, out) is det.

:- pred opt_debug__dump_reg(reg, string).
:- mode opt_debug__dump_reg(in, out) is det.

:- pred opt_debug__dump_vnlval(vnlval, string).
:- mode opt_debug__dump_vnlval(in, out) is det.

:- pred opt_debug__dump_vnrval(vnrval, string).
:- mode opt_debug__dump_vnrval(in, out) is det.

:- pred opt_debug__dump_const(rval_const, string).
:- mode opt_debug__dump_const(in, out) is det.

:- pred opt_debug__dump_unop(unary_op, string).
:- mode opt_debug__dump_unop(in, out) is det.

:- pred opt_debug__dump_binop(binary_op, string).
:- mode opt_debug__dump_binop(in, out) is det.

:- pred opt_debug__dump_label(label, string).
:- mode opt_debug__dump_label(in, out) is det.

:- pred opt_debug__dump_maybe_rvals(list(maybe(rval)), int, string).
:- mode opt_debug__dump_maybe_rvals(in, in, out) is det.

:- pred opt_debug__dump_rval(rval, string).
:- mode opt_debug__dump_rval(in, out) is det.

:- pred opt_debug__print_tailmap(tailmap).
:- mode opt_debug__print_tailmap(in) is det.

:- pred opt_debug__print_instmap(tailmap).
:- mode opt_debug__print_instmap(in) is det.

:- pred opt_debug__print_proclist(list(pair(label, list(instruction)))).
:- mode opt_debug__print_proclist(in) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, string.

:- external(opt_debug__dump_label/2).
:- external(opt_debug__dump_rval/2).
:- external(opt_debug__print_tailmap/1).
:- external(opt_debug__print_instmap/1).
:- external(opt_debug__print_proclist/1).

opt_debug__dump_lval_to_vn([], "").
opt_debug__dump_lval_to_vn([Vnlval - Vn | Lval_to_vn_list], Str) :-
	opt_debug__dump_lval_to_vn(Lval_to_vn_list, Tail_str),
	opt_debug__dump_vnlval(Vnlval, Vnlval_str),
	opt_debug__dump_vn(Vn, Vn_str),
	string__append_list([Vnlval_str, " -> ", Vn_str, "\n", Tail_str], Str).

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
opt_debug__dump_rval_to_vn([Vnrval - Vn | Rval_to_vn_list], Str) :-
	opt_debug__dump_rval_to_vn(Rval_to_vn_list, Tail_str),
	opt_debug__dump_vnrval(Vnrval, Vnrval_str),
	opt_debug__dump_vn(Vn, Vn_str),
	string__append_list([Vnrval_str, " -> ", Vn_str, "\n", Tail_str], Str).

opt_debug__dump_vn_to_rval([], "").
opt_debug__dump_vn_to_rval([Vn - Vnrval | Vn_to_rval_list], Str) :-
	opt_debug__dump_vn_to_rval(Vn_to_rval_list, Tail_str),
	opt_debug__dump_vn(Vn, Vn_str),
	opt_debug__dump_vnrval(Vnrval, Vnrval_str),
	string__append_list([Vn_str, " -> ", Vnrval_str, "\n", Tail_str], Str).

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
	opt_debug__dump_vnlval(Lval, Lval_str),
	opt_debug__dump_vn_locs(Lvals, Tail_str),
	string__append_list([" @", Lval_str, Tail_str], Str).

opt_debug__dump_reg(r(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["r(", N_str, ")"], Str).
opt_debug__dump_reg(f(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["f(", N_str, ")"], Str).

opt_debug__dump_vnlval(vn_reg(R), Str) :-
	opt_debug__dump_reg(R, R_str),
	string__append_list(["vn_reg(", R_str, ")"], Str).
opt_debug__dump_vnlval(vn_stackvar(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["vn_stackvar(", N_str, ")"], Str).
opt_debug__dump_vnlval(vn_framevar(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["vn_framevar(", N_str, ")"], Str).
opt_debug__dump_vnlval(vn_succip, Str) :-
	string__append_list(["vn_succip"], Str).
opt_debug__dump_vnlval(vn_maxfr, Str) :-
	string__append_list(["vn_maxfr"], Str).
opt_debug__dump_vnlval(vn_curredoip, Str) :-
	string__append_list(["vn_curredoip"], Str).
opt_debug__dump_vnlval(vn_hp, Str) :-
	string__append_list(["vn_hp"], Str).
opt_debug__dump_vnlval(vn_sp, Str) :-
	string__append_list(["vn_sp"], Str).
opt_debug__dump_vnlval(vn_field(T, N, F), Str) :-
	string__int_to_string(T, T_str),
	string__int_to_string(N, N_str),
	string__int_to_string(F, F_str),
	string__append_list(["vn_field(", T_str, ", ", N_str, ", ",
		F_str, ")"], Str).
opt_debug__dump_vnlval(vn_temp(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["vn_temp(", N_str, ")"], Str).

opt_debug__dump_vnrval(vn_origlval(Vnlval), Str) :-
	opt_debug__dump_vnlval(Vnlval, Lval_str),
	string__append_list(["vn_origlval(", Lval_str, ")"], Str).
opt_debug__dump_vnrval(vn_mkword(T, N), Str) :-
	string__int_to_string(T, T_str),
	string__int_to_string(N, N_str),
	string__append_list(["vn_mkword(", T_str, ", ", N_str, ")"], Str).
opt_debug__dump_vnrval(vn_const(C), Str) :-
	opt_debug__dump_const(C, C_str),
	string__append_list(["vn_const(", C_str, ")"], Str).
opt_debug__dump_vnrval(vn_create(T, MA, L), Str) :-
	string__int_to_string(T, T_str),
	opt_debug__dump_maybe_rvals(MA, 3, MA_str),
	string__int_to_string(L, L_str),
	string__append_list(["vn_create(", T_str, ", ", MA_str, ", ",
		L_str, ")"], Str).
opt_debug__dump_vnrval(vn_unop(O, N), Str) :-
	opt_debug__dump_unop(O, O_str),
	string__int_to_string(N, N_str),
	string__append_list(["vn_unop(", O_str, ", ", N_str, ")"], Str).
opt_debug__dump_vnrval(vn_binop(O, N1, N2), Str) :-
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
opt_debug__dump_const(pred_const(_CodeAddress), "pred_const(<code address>)").

opt_debug__dump_unop(mktag, "mktag").
opt_debug__dump_unop(tag, "tag").
opt_debug__dump_unop(unmktag, "unmktag").
opt_debug__dump_unop(mkbody, "mkbody").
opt_debug__dump_unop(body, "body").
opt_debug__dump_unop(unmkbody, "unmkbody").
opt_debug__dump_unop(not, "not").
opt_debug__dump_unop(hash_string, "hash_string").
opt_debug__dump_unop(bitwise_complement, "bitwise_complement").
opt_debug__dump_unop(cast_to_unsigned, "cast_to_unsigned").

opt_debug__dump_binop(Op, String) :-
	llds__binary_op_to_string(Op, String).

opt_debug__dump_maybe_rvals([], _, "").
opt_debug__dump_maybe_rvals([MR | MRs], N, Str) :-
	( N > 0 ->
		( MR = yes(R) ->
			opt_debug__dump_rval(R, MR_str)
		;
			MR_str = "no"
		),
		N1 is N - 1,
		opt_debug__dump_maybe_rvals(MRs, N1, MRs_str),
		string__append_list([MR_str, ", ", MRs_str], Str)
	;
		Str = "truncated"
	).
