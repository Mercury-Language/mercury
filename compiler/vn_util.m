%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_util.m - utility predicates for value numbering.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module vn_util.

:- interface.

:- import_module vn_type, vn_table.
:- import_module llds, list, set, int.

:- implementation.

:- import_module opt_util, string, require, std_util, map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule for the forward phase of vn__handle_instr

:- interface.

:- pred vn__find_specials(vnlval, list(vnlval)).
:- mode vn__find_specials(in, out) is det.

:- pred vn__convert_to_vnlval_and_insert(list(lval), vnlvalset, vnlvalset).
% :- mode vn__convert_to_vnlval_and_insert(in, di, uo) is det.
:- mode vn__convert_to_vnlval_and_insert(in, in, out) is det.

:- implementation.

vn__find_specials(vn_reg(_), []).
vn__find_specials(vn_stackvar(_), []).
vn__find_specials(vn_framevar(_), []).
vn__find_specials(vn_succip, [vn_succip]).
vn__find_specials(vn_maxfr, [vn_maxfr]).
vn__find_specials(vn_curfr, [vn_curfr]).
vn__find_specials(vn_redoip(Vn), [vn_redoip(Vn)]).
vn__find_specials(vn_succfr(Vn), [vn_succfr(Vn)]).
vn__find_specials(vn_prevfr(Vn), [vn_prevfr(Vn)]).
vn__find_specials(vn_hp, [vn_hp]).
vn__find_specials(vn_sp, [vn_sp]).
vn__find_specials(vn_field(_, _, _), []).
vn__find_specials(vn_temp(_), []).

vn__convert_to_vnlval_and_insert([], Liveset, Liveset).
vn__convert_to_vnlval_and_insert([Lval | Lvals], Liveset0, Liveset) :-
	vn__no_access_lval_to_vnlval(Lval, MaybeVnlval),
	(
		MaybeVnlval = yes(Vnlval),
		set__insert(Liveset0, Vnlval, Liveset1)
	;
		MaybeVnlval = no,
		Liveset1 = Liveset0
	),
	vn__convert_to_vnlval_and_insert(Lvals, Liveset1, Liveset).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Convert an rval into a vnrval and hence into a vn.

:- pred vn__rval_to_vn(rval, vn, vn_tables, vn_tables).
% :- mode vn__rval_to_vn(in, out, di, uo) is det.
:- mode vn__rval_to_vn(in, out, in, out) is det.

:- pred vn__lval_to_vn(lval, vn, vn_tables, vn_tables).
% :- mode vn__lval_to_vn(in, out, di, uo) is det.
:- mode vn__lval_to_vn(in, out, in, out) is det.

:- pred vn__lval_to_vnlval(lval, vnlval, vn_tables, vn_tables).
% :- mode vn__lval_to_vnlval(in, out, di, uo) is det.
:- mode vn__lval_to_vnlval(in, out, in, out) is det.

:- pred vn__is_const_expr(vn, bool, vn_tables).
:- mode vn__is_const_expr(in, out, in) is det.

	% Find out what vns, if any, are needed to access a vnlval.

:- pred vn__vnlval_access_vns(vnlval, list(vn)).
:- mode vn__vnlval_access_vns(in, out) is det.

	% Turn an vnlval into an lval if possible.

:- pred vn__no_access_vnlval_to_lval(vnlval, maybe(lval)).
:- mode vn__no_access_vnlval_to_lval(in, out) is det.

	% Do the reverse.

:- pred vn__no_access_lval_to_vnlval(lval, maybe(vnlval)).
:- mode vn__no_access_lval_to_vnlval(in, out) is det.

:- pred vn__find_sub_vns(vnrval, list(vn)).
:- mode vn__find_sub_vns(in, out) is det.

:- pred vn__find_sub_vns_vnlval(vnlval, list(vn)).
:- mode vn__find_sub_vns_vnlval(in, out) is det.

	% Find all lvals inside a given rval.

:- pred vn__find_lvals_in_rval(rval, list(lval)).
:- mode vn__find_lvals_in_rval(in, out) is det.

:- pred vn__find_lvals_in_rvals(list(rval), list(lval)).
:- mode vn__find_lvals_in_rvals(in, out) is det.

	% Find out whether a vn is needed in two or more assignments.

:- pred vn__is_vn_shared(vn, vnrval, list(vn_src), vn_tables).
:- mode vn__is_vn_shared(in, in, in, in) is semidet.

	% Find out which uses of a vn actually require an assignment.

:- pred vn__real_uses(list(vn_src), list(vn_src), vn_tables).
% :- mode vn__real_uses(di, uo, in) is semidet.
:- mode vn__real_uses(in, out, in) is semidet.

:- implementation.

%-----------------------------------------------------------------------------%

vn__rval_to_vn(Rval, Vn, VnTables0, VnTables) :-
	(
		Rval = lval(Lval),
		vn__lval_to_vn(Lval, Vn, VnTables0, VnTables)
	;
		Rval = var(_),
		error("value_number should never get rval: var")
	;
		Rval = create(Tag, Args, Label),
		vn__vnrval_to_vn(vn_create(Tag, Args, Label), Vn,
			VnTables0, VnTables)
	;
		Rval = mkword(Tag, Rval1),
		vn__rval_to_vn(Rval1, SubVn, VnTables0, VnTables1),
		vn__vnrval_to_vn(vn_mkword(Tag, SubVn), Vn,
			VnTables1, VnTables)
	;
		Rval = const(Const),
		vn__vnrval_to_vn(vn_const(Const), Vn,
			VnTables0, VnTables)
	;
		Rval = unop(Unop, Rval1),
		vn__rval_to_vn(Rval1, SubVn, VnTables0, VnTables1),
		vn__vnrval_to_vn(vn_unop(Unop, SubVn), Vn,
			VnTables1, VnTables)
	;
		Rval = binop(Binop, Rval1, Rval2),
		vn__rval_to_vn(Rval1, SubVn1, VnTables0, VnTables1),
		vn__rval_to_vn(Rval2, SubVn2, VnTables1, VnTables2),
		vn__vnrval_to_vn(vn_binop(Binop, SubVn1, SubVn2), Vn,
			VnTables2, VnTables)
	).

:- pred vn__vnrval_to_vn(vnrval, vn, vn_tables, vn_tables).
% :- mode vn__vnrval_to_vn(in, out, di, uo) is det.
:- mode vn__vnrval_to_vn(in, out, in, out) is det.

vn__vnrval_to_vn(Vnrval, Vn, VnTables0, VnTables) :-
	vn__simplify_vnrval(Vnrval, Vnrval1, VnTables0, VnTables1),
	( vn__search_assigned_vn(Vnrval1, Vn_prime, VnTables1) ->
		Vn = Vn_prime,
		VnTables = VnTables1
	;
		vn__record_first_vnrval(Vnrval1, Vn, VnTables1, VnTables)
	).

	% Simplify the vnrval by partially evaluating expressions involving
	% integer constants. To make this simpler, swap the arguments of
	% commutative expressions around to put the constants on the right
	% side.

	% The simplification has to be done on vnrvals and not on rvals
	% even though this complicates the code. The reason is that an
	% expression such as r1 + 4 can be simplified if we know that
	% r1 was defined as r2 + 8.

	% XXX more simplification opportunities exist

:- pred vn__simplify_vnrval(vnrval, vnrval, vn_tables, vn_tables).
% :- mode vn__simplify_vnrval(in, out, di, uo) is det.
:- mode vn__simplify_vnrval(in, out, in, out) is det.

vn__simplify_vnrval(Vnrval0, Vnrval, VnTables0, VnTables) :-
	(
		Vnrval0 = vn_binop(Binop, Vn1, Vn2),
		vn__simplify_vnrval_binop(Binop, Vn1, Vn2, VnrvalPrime,
			VnTables0, VnTablesPrime)
	->
		Vnrval = VnrvalPrime,
		VnTables = VnTablesPrime
	;
		Vnrval = Vnrval0,
		VnTables = VnTables0
	).

:- pred vn__simplify_vnrval_binop(binary_op, vn, vn, vnrval,
	vn_tables, vn_tables).
% :- mode vn__simplify_vnrval_binop(in, in, in, out, di, uo) is semidet.
:- mode vn__simplify_vnrval_binop(in, in, in, out, in, out) is semidet.

vn__simplify_vnrval_binop(Binop, Vn1, Vn2, Vnrval, VnTables0, VnTables) :-
	vn__lookup_defn(Vn1, Vnrval1, "vn__simplify_vnrval_binop", VnTables0),
	vn__lookup_defn(Vn2, Vnrval2, "vn__simplify_vnrval_binop", VnTables0),
	(
		Binop = (+),
		(
						% c1+c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 + I2,
			Vnrval = vn_const(int_const(I)),
			VnTables = VnTables0
		;
						% c1+e21+c22 => e21+c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_binop((+), Vn21, Vn22),
			vn__lookup_defn(Vn22, Vnrval22, "vn__simplify_vnrval",
				VnTables0),
			Vnrval22 = vn_const(int_const(I22))
		->
			I is I1 + I22,
			vn__vnrval_to_vn(vn_const(int_const(I)), VnConst,
				VnTables0, VnTables),
			Vnrval = vn_binop((+), Vn21, VnConst)
		;
						% c1+e2 => e2+c1
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_binop((+), Vn21, Vn22)
		->
			Vnrval = vn_binop((+), Vn2, Vn1),
			VnTables = VnTables0
		;
						% e11+c12+c2 => e11+c
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn__lookup_defn(Vn12, Vnrval12, "vn__simplify_vnrval",
				VnTables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I12 + I2,
			vn__vnrval_to_vn(vn_const(int_const(I)),
				VnConst, VnTables0, VnTables),
			Vnrval = vn_binop((+), Vn11, VnConst)
		;
						% e11+c12+e21+c22 => e11+e21+c
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn__lookup_defn(Vn12, Vnrval12, "vn__simplify_vnrval",
				VnTables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_binop((+), Vn21, Vn22),
			vn__lookup_defn(Vn22, Vnrval22, "vn__simplify_vnrval",
				VnTables0),
			Vnrval22 = vn_const(int_const(I22))
		->
			I is I12 + I22,
			vn__vnrval_to_vn(vn_binop((+), Vn11, Vn21), VnExpr,
				VnTables0, VnTables1),
			vn__vnrval_to_vn(vn_const(int_const(I)),
				VnConst, VnTables1, VnTables),
			Vnrval = vn_binop((+), VnExpr, VnConst)
		;
						% e11+c12+e2 => e11+e2+c12
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn__lookup_defn(Vn12, Vnrval12, "vn__simplify_vnrval",
				VnTables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_binop((+), Vn21, Vn22)
		->
			vn__vnrval_to_vn(vn_binop((+), Vn11, Vn2), VnExpr,
				VnTables0, VnTables),
			Vnrval = vn_binop((+), VnExpr, Vn12)
		;
			fail
		)
	;
		Binop = (-),
		(
						% e1-c2 => e1+c
			Vnrval2 = vn_const(int_const(I2))
		->
			NI2 is 0 - I2,
			vn__vnrval_to_vn(vn_const(int_const(NI2)), VnConst,
				VnTables0, VnTables1),
			vn__simplify_vnrval(vn_binop((+), Vn1, VnConst), Vnrval,
				VnTables1, VnTables)
		;
						% c1-e2 => 0-e2+c1
			Vnrval1 = vn_const(int_const(I1)),
			I1 \= 0
		->
			vn__vnrval_to_vn(vn_const(int_const(0)), VnConst0,
				VnTables0, VnTables1),
			vn__vnrval_to_vn(vn_binop((-), VnConst0, Vn2), VnExpr,
				VnTables1, VnTables2),
			vn__simplify_vnrval(vn_binop((+), VnExpr, Vn1), Vnrval,
				VnTables2, VnTables)
		;
			fail
		)
	;
		Binop = (*),
		(
						% c1*c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 * I2,
			Vnrval = vn_const(int_const(I)),
			VnTables = VnTables0
		;
			fail
		)
	;
		Binop = (/),
		(
						% c1/c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 // I2,
			Vnrval = vn_const(int_const(I)),
			VnTables = VnTables0
		;
			fail
		)
	;	
		Binop = eq,
		(
			Vn1 = Vn2
		->
			Vnrval = vn_const(true),
			VnTables = VnTables0
		;
			Vnrval1 = vn_unop(tag, WordVn),
			Vnrval2 = vn_unop(mktag, Tag),
			vn__lookup_defn(WordVn, WordVnrval,
				"vn__simplify_vnrval_binop", VnTables0),
			WordVnrval = vn_mkword(Tag, _)
		->
			Vnrval = vn_const(true),
			VnTables = VnTables0
		;
			fail
		)
	;	
		Binop = ne,
		(
			Vn1 = Vn2
		->
			Vnrval = vn_const(false),
			VnTables = VnTables0
		;
			Vnrval1 = vn_unop(tag, WordVn),
			Vnrval2 = vn_unop(mktag, Tag),
			vn__lookup_defn(WordVn, WordVnrval,
				"vn__simplify_vnrval_binop", VnTables0),
			WordVnrval = vn_mkword(Tag, _)
		->
			Vnrval = vn_const(false),
			VnTables = VnTables0
		;
			fail
		)
	;
		Binop = (and),
		( Vnrval1 = vn_const(true) ->
			Vnrval = Vnrval2,
			VnTables = VnTables0
		; Vnrval2 = vn_const(true) ->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		; Vnrval1 = vn_const(false) ->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		; Vnrval2 = vn_const(false) ->
			Vnrval = Vnrval2,
			VnTables = VnTables0
		;
			fail
		)
	;
		Binop = (or),
		( Vnrval1 = vn_const(false) ->
			Vnrval = Vnrval2,
			VnTables = VnTables0
		; Vnrval2 = vn_const(false) ->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		; Vnrval1 = vn_const(true) ->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		; Vnrval2 = vn_const(true) ->
			Vnrval = Vnrval2,
			VnTables = VnTables0
		;
			fail
		)
	).

vn__lval_to_vn(Lval, Vn, VnTables0, VnTables) :-
	vn__lval_to_vnlval(Lval, Vnlval, VnTables0, VnTables1),
	( vn__search_desired_value(Vnlval, Vn_prime, VnTables1) ->
		Vn = Vn_prime,
		VnTables = VnTables1
	;
		vn__record_first_vnlval(Vnlval, Vn, VnTables1, VnTables)
	).

vn__lval_to_vnlval(Lval, Vnlval, VnTables0, VnTables) :-
	vn__no_access_lval_to_vnlval(Lval, MaybeVnlval),
	( MaybeVnlval = yes(VnlvalPrime) ->
		Vnlval = VnlvalPrime,
		VnTables = VnTables0
	; Lval = field(Tag, Rval1, Rval2) ->
		vn__rval_to_vn(Rval1, Vn1, VnTables0, VnTables1),
		vn__rval_to_vn(Rval2, Vn2, VnTables1, VnTables),
		Vnlval = vn_field(Tag, Vn1, Vn2)
	; Lval = succfr(Rval1) ->
		vn__rval_to_vn(Rval1, Vn1, VnTables0, VnTables),
		Vnlval = vn_succfr(Vn1)
	; Lval = prevfr(Rval1) ->
		vn__rval_to_vn(Rval1, Vn1, VnTables0, VnTables),
		Vnlval = vn_prevfr(Vn1)
	; Lval = redoip(Rval1) ->
		vn__rval_to_vn(Rval1, Vn1, VnTables0, VnTables),
		Vnlval = vn_redoip(Vn1)
	;
		error("unexpected lval in vn__lval_to_vnlval")
	).

% If you to add to this list to fix a determinism error,
% check vn__lval_to_vnlval above as well.

vn__no_access_lval_to_vnlval(reg(Reg),		yes(vn_reg(Reg))).
vn__no_access_lval_to_vnlval(stackvar(N),	yes(vn_stackvar(N))).
vn__no_access_lval_to_vnlval(framevar(N),	yes(vn_framevar(N))).
vn__no_access_lval_to_vnlval(succip,		yes(vn_succip)).
vn__no_access_lval_to_vnlval(maxfr,		yes(vn_maxfr)).
vn__no_access_lval_to_vnlval(curfr,		yes(vn_curfr)).
vn__no_access_lval_to_vnlval(redoip(_),		no).
vn__no_access_lval_to_vnlval(prevfr(_),		no).
vn__no_access_lval_to_vnlval(succfr(_),		no).
vn__no_access_lval_to_vnlval(hp,		yes(vn_hp)).
vn__no_access_lval_to_vnlval(sp,		yes(vn_sp)).
vn__no_access_lval_to_vnlval(field(_, _, _),	no).
vn__no_access_lval_to_vnlval(temp(N),		yes(vn_temp(N))).
vn__no_access_lval_to_vnlval(lvar(_Var), _) :-
	error("lvar detected in value_number").

vn__no_access_vnlval_to_lval(vn_reg(Reg),	yes(reg(Reg))).
vn__no_access_vnlval_to_lval(vn_stackvar(N),	yes(stackvar(N))).
vn__no_access_vnlval_to_lval(vn_framevar(N),	yes(framevar(N))).
vn__no_access_vnlval_to_lval(vn_succip,		yes(succip)).
vn__no_access_vnlval_to_lval(vn_maxfr,		yes(maxfr)).
vn__no_access_vnlval_to_lval(vn_curfr,		yes(curfr)).
vn__no_access_vnlval_to_lval(vn_succfr(_),	no).
vn__no_access_vnlval_to_lval(vn_prevfr(_),	no).
vn__no_access_vnlval_to_lval(vn_redoip(_),	no).
vn__no_access_vnlval_to_lval(vn_hp,		yes(hp)).
vn__no_access_vnlval_to_lval(vn_sp,		yes(sp)).
vn__no_access_vnlval_to_lval(vn_field(_, _, _), no).
vn__no_access_vnlval_to_lval(vn_temp(N),	yes(temp(N))).

/* one of these preds should be eliminated XXX */
vn__vnlval_access_vns(vn_reg(_), []).
vn__vnlval_access_vns(vn_stackvar(_), []).
vn__vnlval_access_vns(vn_framevar(_), []).
vn__vnlval_access_vns(vn_succip, []).
vn__vnlval_access_vns(vn_maxfr, []).
vn__vnlval_access_vns(vn_curfr, []).
vn__vnlval_access_vns(vn_succfr(Vn), [Vn]).
vn__vnlval_access_vns(vn_prevfr(Vn), [Vn]).
vn__vnlval_access_vns(vn_redoip(Vn), [Vn]).
vn__vnlval_access_vns(vn_hp, []).
vn__vnlval_access_vns(vn_sp, []).
vn__vnlval_access_vns(vn_field(_, Vn1, Vn2), [Vn1, Vn2]).
vn__vnlval_access_vns(vn_temp(_), []).

vn__find_sub_vns(vn_origlval(Vnlval), SubVns) :-
	vn__find_sub_vns_vnlval(Vnlval, SubVns).
vn__find_sub_vns(vn_mkword(_, SubVn), [SubVn]).
vn__find_sub_vns(vn_const(_), []).
vn__find_sub_vns(vn_create(_, _, _), []).
vn__find_sub_vns(vn_unop(_, SubVn), [SubVn]).
vn__find_sub_vns(vn_binop(_, SubVn1, SubVn2), [SubVn1, SubVn2]).

vn__find_sub_vns_vnlval(vn_reg(_), []).
vn__find_sub_vns_vnlval(vn_stackvar(_), []).
vn__find_sub_vns_vnlval(vn_framevar(_), []).
vn__find_sub_vns_vnlval(vn_succip, []).
vn__find_sub_vns_vnlval(vn_maxfr, []).
vn__find_sub_vns_vnlval(vn_curfr, []).
vn__find_sub_vns_vnlval(vn_succfr(Vn), [Vn]).
vn__find_sub_vns_vnlval(vn_prevfr(Vn), [Vn]).
vn__find_sub_vns_vnlval(vn_redoip(Vn), [Vn]).
vn__find_sub_vns_vnlval(vn_hp, []).
vn__find_sub_vns_vnlval(vn_sp, []).
vn__find_sub_vns_vnlval(vn_field(_, Vn1, Vn2), [Vn1, Vn2]).
vn__find_sub_vns_vnlval(vn_temp(_), []).

vn__is_const_expr(Vn, IsConst, VnTables) :-
	vn__lookup_defn(Vn, Vnrval, "vn__is_const_expr", VnTables),
	(
		Vnrval = vn_origlval(_),
		IsConst = no
	;
		Vnrval = vn_mkword(_, Vn1),
		vn__is_const_expr(Vn1, IsConst, VnTables)
	;
		Vnrval = vn_const(_),
		IsConst = yes
	;
		Vnrval = vn_create(_, _, _),
		IsConst = yes
	;
		Vnrval = vn_unop(_, Vn1),
		vn__is_const_expr(Vn1, IsConst, VnTables)
	;	
		Vnrval = vn_binop(_, Vn1, Vn2),
		vn__is_const_expr(Vn1, IsConst1, VnTables),
		vn__is_const_expr(Vn2, IsConst2, VnTables),
		bool__and(IsConst1, IsConst2, IsConst)
	).

vn__find_lvals_in_rval(Rval, Lvals) :-
	(
		Rval = lval(Lval),
		opt_util__lval_access_rvals(Lval, Rvals),
		vn__find_lvals_in_rvals(Rvals, Lvals1),
		Lvals = [Lval | Lvals1]
	;
		Rval = var(_),
		error("var found in vn__find_lvals_in_rval")
	;
		Rval = create(_, _, _),
		Lvals = []
	;
		Rval = mkword(_, Rval1),
		vn__find_lvals_in_rval(Rval1, Lvals)
	;
		Rval = const(_),
		Lvals = []
	;
		Rval = unop(_, Rval1),
		vn__find_lvals_in_rval(Rval1, Lvals)
	;
		Rval = binop(_, Rval1, Rval2),
		vn__find_lvals_in_rval(Rval1, Lvals1),
		vn__find_lvals_in_rval(Rval2, Lvals2),
		list__append(Lvals1, Lvals2, Lvals)
	).

vn__find_lvals_in_rvals([], []).
vn__find_lvals_in_rvals([Rval | Rvals], Lvals) :-
	vn__find_lvals_in_rval(Rval, Lvals1),
	vn__find_lvals_in_rvals(Rvals, Lvals2),
	list__append(Lvals1, Lvals2, Lvals).

vn__is_vn_shared(Vn, Vnrval, Uses0, VnTables) :-
	vn__is_const_expr(Vn, no, VnTables),
	\+ Vnrval = vn_origlval(vn_hp),
	vn__real_uses(Uses0, Uses1, VnTables),
	Uses1 = [_,_|_].

vn__real_uses([], [], _VnTables).
vn__real_uses([Use0 | Uses0], Uses, VnTables) :-
	vn__real_uses(Uses0, Uses1, VnTables),
	( Use0 = src_liveval(Vnlval) ->
		(
			vn__search_desired_value(Vnlval, Vn, VnTables),
			vn__search_current_value(Vnlval, Vn, VnTables)
		->
			Uses = Uses1
		;
			Uses = [Use0 | Uses1]
		)
	; Use0 = src_access(Vnlval) ->
		(
			vn__search_desired_value(Vnlval, Vn, VnTables),
			vn__search_current_value(Vnlval, Vn, VnTables),
			vn__search_uses(Vn, AccessUses, VnTables),
			vn__real_uses(AccessUses, [], VnTables)
		->
			Uses = Uses1
		;
			Uses = [Use0 | Uses1]
		)
	;
		Uses = [Use0 | Uses1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Build up a list of the uses of each vn.

:- pred vn__build_uses(vnlvalset, ctrlmap, vn_tables, vn_tables).
% :- mode vn__build_uses(in, in, di, uo) is det.
:- mode vn__build_uses(in, in, in, out) is det.

:- implementation.

vn__build_uses(Livevals, Ctrlmap, VnTables0, VnTables) :-
	vn__build_uses_from_ctrl(0, Ctrlmap, VnTables0, VnTables1),
	set__to_sorted_list(Livevals, Livelist),
	vn__build_uses_from_livevals(Livelist, VnTables1, VnTables).

:- pred vn__build_uses_from_ctrl(int, ctrlmap, vn_tables, vn_tables).
% :- mode vn__build_uses_from_ctrl(in, in, di, uo) is det.
:- mode vn__build_uses_from_ctrl(in, in, in, out) is det.

vn__build_uses_from_ctrl(Ctrl, Ctrlmap, VnTables0, VnTables) :-
	( map__search(Ctrlmap, Ctrl, VnInstr) ->
		(
			VnInstr = vn_livevals(_),
			VnTables1 = VnTables0
		;
			VnInstr = vn_call(_, _, _, _),
			VnTables1 = VnTables0
		;
			VnInstr = vn_call_closure(_, _, _),
			VnTables1 = VnTables0
		;
			VnInstr = vn_mkframe(_, _, _),
			VnTables1 = VnTables0
		;
			VnInstr = vn_label(_),
			VnTables1 = VnTables0
		;
			VnInstr = vn_goto(_, _),
			VnTables1 = VnTables0
		;
			VnInstr = vn_computed_goto(Vn, _),
			vn__record_use(Vn, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_if_val(Vn, _),
			vn__record_use(Vn, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_mark_hp(Vnlval),
			vn__vnlval_access_vns(Vnlval, Vns),
			vn__record_use_list(Vns, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_restore_hp(Vn),
			vn__record_use(Vn, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_incr_sp(_),
			VnTables1 = VnTables0
		;
			VnInstr = vn_decr_sp(_),
			VnTables1 = VnTables0
		),
		NextCtrl is Ctrl + 1,
		vn__build_uses_from_ctrl(NextCtrl, Ctrlmap,
			VnTables1, VnTables)
	;
		VnTables = VnTables0
	).

	% We have to record two kinds of uses. The first is the use of the
	% value number we want to assign to the vnlval. The second is the
	% value numbers needed to access the vnlval at all.

:- pred vn__build_uses_from_livevals(list(vnlval), vn_tables, vn_tables).
% :- mode vn__build_uses_from_livevals(in, di, uo) is det.
:- mode vn__build_uses_from_livevals(in, in, out) is det.

vn__build_uses_from_livevals([], VnTables, VnTables).
vn__build_uses_from_livevals([Live | Liveslist], VnTables0, VnTables) :-
	( vn__search_desired_value(Live, VnPrime, VnTables0) ->
		Vn = VnPrime,
		VnTables1 = VnTables0
	;
		vn__record_first_vnlval(Live, Vn, VnTables0, VnTables1)
	),
	vn__record_use(Vn, src_liveval(Live), VnTables1, VnTables2),
	vn__record_access([Live], VnTables2, VnTables3),
	vn__build_uses_from_livevals(Liveslist, VnTables3, VnTables).

:- pred vn__record_access(list(vnlval), vn_tables, vn_tables).
% :- mode vn__record_access(in, di, uo) is det.
:- mode vn__record_access(in, in, out) is det.

vn__record_access([], VnTables, VnTables).
vn__record_access([Vnlval | Vnlvals], VnTables0, VnTables) :-
	vn__vnlval_access_vns(Vnlval, SubVns),
	vn__record_use_list(SubVns, src_access(Vnlval), VnTables0, VnTables1),
	vn__record_access_vns(SubVns, VnTables1, VnTables2),
	vn__record_access(Vnlvals, VnTables2, VnTables).

:- pred vn__record_access_vns(list(vn), vn_tables, vn_tables).
% :- mode vn__record_access_vns(in, di, uo) is det.
:- mode vn__record_access_vns(in, in, out) is det.

vn__record_access_vns(_, VnTables, VnTables).
% vn__record_access_vns([], VnTables, VnTables).
% vn__record_access_vns([Vn | Vns], VnTables0, VnTables) :-
% 	vn__vnlval_access_vns(Vnlval, SubVns),
% 	vn__record_use_list(SubVns, src_access(Vnlval), VnTables0, VnTables1),
% 	vn__record_access_vns(Vnlvals, VnTables1, VnTables).

:- pred vn__record_use(vn, vn_src, vn_tables, vn_tables).
% :- mode vn__record_use(in, in, di, uo) is det.
:- mode vn__record_use(in, in, in, out) is det.

vn__record_use(Vn, Src, VnTables0, VnTables) :-
	vn__lookup_uses(Vn, OldUses, "vn__record_use", VnTables0),
	vn__add_new_use(Vn, Src, VnTables0, VnTables1),
	( OldUses = [] ->
		vn__lookup_defn(Vn, Vnrval, "vn__record_use", VnTables1),
		(
			Vnrval = vn_origlval(Vnlval),
			vn__record_access([Vnlval], VnTables1, VnTables)
		;
			Vnrval = vn_mkword(_, SubVn),
			vn__record_use(SubVn, src_vn(Vn),
				VnTables1, VnTables)
		;
			Vnrval = vn_const(_),
			VnTables = VnTables1
		;
			Vnrval = vn_create(_, _, _),
			VnTables = VnTables1
		;
			Vnrval = vn_unop(_, SubVn),
			vn__record_use(SubVn, src_vn(Vn),
				VnTables1, VnTables)
		;
			Vnrval = vn_binop(_, SubVn1, SubVn2),
			vn__record_use(SubVn1, src_vn(Vn),
				VnTables1, VnTables2),
			vn__record_use(SubVn2, src_vn(Vn),
				VnTables2, VnTables)
		)
	;
		VnTables = VnTables1
	).

:- pred vn__record_use_list(list(vn), vn_src, vn_tables, vn_tables).
% :- mode vn__record_use_list(in, in, di, uo) is det.
:- mode vn__record_use_list(in, in, in, out) is det.

vn__record_use_list([], _Src, VnTables, VnTables).
vn__record_use_list([Vn | Vns], Src, VnTables0, VnTables) :-
	vn__record_use(Vn, Src, VnTables0, VnTables1),
	vn__record_use_list(Vns, Src, VnTables1, VnTables).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
