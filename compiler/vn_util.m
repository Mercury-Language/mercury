%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_util.m - utility predicates for value numbering.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module vn_util.

:- interface.

:- import_module list, bool, std_util.
:- import_module llds, vn_type, vn_table.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule for the forward phase of vn_block__handle_instr

:- interface.

:- pred vn_util__find_specials(vnlval, list(vnlval)).
:- mode vn_util__find_specials(in, out) is det.

:- pred vn_util__convert_to_vnlval_and_insert(list(lval), vnlvalset, vnlvalset).
% :- mode vn_util__convert_to_vnlval_and_insert(in, di, uo) is det.
:- mode vn_util__convert_to_vnlval_and_insert(in, in, out) is det.

:- implementation.

:- import_module int, float, string, set, map, require.

:- import_module opt_util.

vn_util__find_specials(vn_reg(_, _), []).
vn_util__find_specials(vn_temp(_, _), []).
vn_util__find_specials(vn_stackvar(_), []).
vn_util__find_specials(vn_framevar(_), []).
vn_util__find_specials(vn_succip, [vn_succip]).
vn_util__find_specials(vn_maxfr, [vn_maxfr]).
vn_util__find_specials(vn_curfr, [vn_curfr]).
vn_util__find_specials(vn_redoip(Vn), [vn_redoip(Vn)]).
vn_util__find_specials(vn_succip(Vn), [vn_succip(Vn)]).
vn_util__find_specials(vn_succfr(Vn), [vn_succfr(Vn)]).
vn_util__find_specials(vn_prevfr(Vn), [vn_prevfr(Vn)]).
vn_util__find_specials(vn_hp, [vn_hp]).
vn_util__find_specials(vn_sp, [vn_sp]).
vn_util__find_specials(vn_field(_, _, _), []).
vn_util__find_specials(vn_mem_ref(_), []).

vn_util__convert_to_vnlval_and_insert([], Liveset, Liveset).
vn_util__convert_to_vnlval_and_insert([Lval | Lvals], Liveset0, Liveset) :-
	vn_util__no_access_lval_to_vnlval(Lval, MaybeVnlval),
	(
		MaybeVnlval = yes(Vnlval),
		set__insert(Liveset0, Vnlval, Liveset1)
	;
		MaybeVnlval = no,
		Liveset1 = Liveset0
	),
	vn_util__convert_to_vnlval_and_insert(Lvals, Liveset1, Liveset).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Convert an rval into a vnrval and hence into a vn.

:- pred vn_util__rval_to_vn(rval, vn, vn_tables, vn_tables).
% :- mode vn_util__rval_to_vn(in, out, di, uo) is det.
:- mode vn_util__rval_to_vn(in, out, in, out) is det.

:- pred vn_util__lval_to_vn(lval, vn, vn_tables, vn_tables).
% :- mode vn_util__lval_to_vn(in, out, di, uo) is det.
:- mode vn_util__lval_to_vn(in, out, in, out) is det.

:- pred vn_util__lval_to_vnlval(lval, vnlval, vn_tables, vn_tables).
% :- mode vn_util__lval_to_vnlval(in, out, di, uo) is det.
:- mode vn_util__lval_to_vnlval(in, out, in, out) is det.

:- pred vn_util__is_const_expr(vn, bool, vn_tables).
:- mode vn_util__is_const_expr(in, out, in) is det.

	% Find out what vns, if any, are needed to access a vnlval.

:- pred vn_util__vnlval_access_vns(vnlval, list(vn)).
:- mode vn_util__vnlval_access_vns(in, out) is det.

	% Turn an vnlval into an lval if possible.

:- pred vn_util__no_access_vnlval_to_lval(vnlval, maybe(lval)).
:- mode vn_util__no_access_vnlval_to_lval(in, out) is det.

	% Do the reverse.

:- pred vn_util__no_access_lval_to_vnlval(lval, maybe(vnlval)).
:- mode vn_util__no_access_lval_to_vnlval(in, out) is det.

:- pred vn_util__find_sub_vns(vnrval, list(vn)).
:- mode vn_util__find_sub_vns(in, out) is det.

	% Find all lvals inside a given rval.

:- pred vn_util__find_lvals_in_rval(rval, list(lval)).
:- mode vn_util__find_lvals_in_rval(in, out) is det.

:- pred vn_util__find_lvals_in_rvals(list(rval), list(lval)).
:- mode vn_util__find_lvals_in_rvals(in, out) is det.

	% Find out whether a vn is needed in two or more assignments.

:- pred vn_util__is_vn_shared(vn, vnrval, list(vn_src), vn_tables).
:- mode vn_util__is_vn_shared(in, in, in, in) is semidet.

	% Find out which uses of a vn actually require an assignment.

:- pred vn_util__real_uses(list(vn_src), list(vn_src), vn_tables).
% :- mode vn_util__real_uses(di, uo, in) is semidet.
:- mode vn_util__real_uses(in, out, in) is semidet.

	% Choose the cheapest location from a given list of locations.
	% Access time is the only consideration. There are three cost levels:
	% registers and temporaries; stackvars and framevars; fields.
	% Fail only if the input list is empty.

:- pred vn_util__choose_cheapest_loc(list(vnlval), vnlval).
:- mode vn_util__choose_cheapest_loc(in, out) is semidet.

:- pred vn_util__classify_loc_cost(vnlval, int).
:- mode vn_util__classify_loc_cost(in, out) is det.

:- implementation.

%-----------------------------------------------------------------------------%

vn_util__rval_to_vn(Rval, Vn, VnTables0, VnTables) :-
	(
		Rval = lval(Lval),
		vn_util__lval_to_vn(Lval, Vn, VnTables0, VnTables)
	;
		Rval = var(_),
		error("value_number should never get rval: var")
	;
		Rval = create(Tag, Args, Unique, Label, Msg),
		vn_util__vnrval_to_vn(vn_create(Tag, Args, Unique, Label, Msg),
			Vn, VnTables0, VnTables)
	;
		Rval = mkword(Tag, Rval1),
		vn_util__rval_to_vn(Rval1, SubVn, VnTables0, VnTables1),
		vn_util__vnrval_to_vn(vn_mkword(Tag, SubVn), Vn,
			VnTables1, VnTables)
	;
		Rval = const(Const),
		vn_util__vnrval_to_vn(vn_const(Const), Vn,
			VnTables0, VnTables)
	;
		Rval = unop(Unop, Rval1),
		vn_util__rval_to_vn(Rval1, SubVn, VnTables0, VnTables1),
		vn_util__vnrval_to_vn(vn_unop(Unop, SubVn), Vn,
			VnTables1, VnTables)
	;
		Rval = binop(Binop, Rval1, Rval2),
		vn_util__rval_to_vn(Rval1, SubVn1, VnTables0, VnTables1),
		vn_util__rval_to_vn(Rval2, SubVn2, VnTables1, VnTables2),
		vn_util__vnrval_to_vn(vn_binop(Binop, SubVn1, SubVn2), Vn,
			VnTables2, VnTables)
	;
		Rval = mem_addr(MemRef),
		vn_util__mem_ref_to_vn(MemRef, Vn, VnTables0, VnTables)
	).

:- pred vn_util__mem_ref_to_vn(mem_ref, vn, vn_tables, vn_tables).
% :- mode vn_util__mem_ref_to_vn(in, out, di, uo) is det.
:- mode vn_util__mem_ref_to_vn(in, out, in, out) is det.

vn_util__mem_ref_to_vn(MemRef, Vn, VnTables0, VnTables) :-
	(
		MemRef = stackvar_ref(Slot), 
		vn_util__vnrval_to_vn(vn_stackvar_addr(Slot), Vn,
			VnTables0, VnTables)
	;
		MemRef = framevar_ref(Slot), 
		vn_util__vnrval_to_vn(vn_framevar_addr(Slot), Vn,
			VnTables0, VnTables)
	;
		MemRef = heap_ref(Rval, Tag, Field), 
		vn_util__rval_to_vn(Rval, SubVn, VnTables0, VnTables1),
		vn_util__vnrval_to_vn(vn_heap_addr(SubVn, Tag, Field), Vn,
			VnTables1, VnTables)
	).

:- pred vn_util__vnrval_to_vn(vnrval, vn, vn_tables, vn_tables).
% :- mode vn_util__vnrval_to_vn(in, out, di, uo) is det.
:- mode vn_util__vnrval_to_vn(in, out, in, out) is det.

vn_util__vnrval_to_vn(Vnrval, Vn, VnTables0, VnTables) :-
	vn_util__simplify_vnrval(Vnrval, Vnrval1, VnTables0, VnTables1),
	( vn_table__search_assigned_vn(Vnrval1, Vn_prime, VnTables1) ->
		( vn_util__vnrval_may_share_vn(Vnrval1) ->
			Vn = Vn_prime,
			VnTables = VnTables1
		;
			vn_table__record_new_vnrval(Vnrval1, Vn,
				VnTables1, VnTables)
		)
	;
		vn_table__record_first_vnrval(Vnrval1, Vn, VnTables1, VnTables)
	).

:- pred vn_util__vnrval_may_share_vn(vnrval).
:- mode vn_util__vnrval_may_share_vn(in) is semidet.

vn_util__vnrval_may_share_vn(Vnrval) :-
	(
		Vnrval = vn_unop(cast_to_unsigned, _)
	->
		fail
	;
		true
	).

	% Simplify the vnrval by partially evaluating expressions involving
	% constants. To make this simpler, swap the arguments of commutative
	% expressions around to put the constants on the right side.

	% For the time being we assume ideal integers and reals.
	% Eventually, we will have to introduce a mechanism to allow
	% programmers to prescribe orders of evaluation.

	% We also assume that the compiler's arithmetic is at least as
	% accurate as the target machine's arithmetic.

	% The simplification has to be done on vnrvals and not on rvals
	% even though this complicates the code. The reason is that an
	% expression such as r1 + 4 can be simplified if we know that
	% r1 was defined as r2 + 8.

	% XXX more simplification opportunities exist

:- pred vn_util__simplify_vnrval(vnrval, vnrval, vn_tables, vn_tables).
% :- mode vn_util__simplify_vnrval(in, out, di, uo) is det.
:- mode vn_util__simplify_vnrval(in, out, in, out) is det.

vn_util__simplify_vnrval(Vnrval0, Vnrval, VnTables0, VnTables) :-
	(
		Vnrval0 = vn_binop(Binop, Vn1, Vn2),
		vn_util__simplify_vnrval_binop(Binop, Vn1, Vn2, VnrvalPrime,
			VnTables0, VnTablesPrime)
	->
		Vnrval = VnrvalPrime,
		VnTables = VnTablesPrime
	;
		Vnrval = Vnrval0,
		VnTables = VnTables0
	).

:- pred vn_util__simplify_vnrval_binop(binary_op, vn, vn, vnrval,
	vn_tables, vn_tables).
% :- mode vn_util__simplify_vnrval_binop(in, in, in, out, di, uo) is semidet.
:- mode vn_util__simplify_vnrval_binop(in, in, in, out, in, out) is semidet.

vn_util__simplify_vnrval_binop(Binop, Vn1, Vn2, Vnrval, VnTables0, VnTables) :-
	vn_table__lookup_defn(Vn1, Vnrval1,
		"vn_util__simplify_vnrval_binop", VnTables0),
	vn_table__lookup_defn(Vn2, Vnrval2,
		"vn_util__simplify_vnrval_binop", VnTables0),
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
			vn_table__lookup_defn(Vn22, Vnrval22,
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval22 = vn_const(int_const(I22))
		->
			I is I1 + I22,
			vn_util__vnrval_to_vn(vn_const(int_const(I)), VnConst,
				VnTables0, VnTables),
			Vnrval = vn_binop((+), Vn21, VnConst)
		;
						% c1+e2 => e2+c1
			Vnrval1 = vn_const(int_const(_I1)),
			Vnrval2 = vn_binop((+), _Vn21, _Vn22)
		->
			Vnrval = vn_binop((+), Vn2, Vn1),
			VnTables = VnTables0
		;
						% e11+c12+c2 => e11+c
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn_table__lookup_defn(Vn12, Vnrval12,
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I12 + I2,
			vn_util__vnrval_to_vn(vn_const(int_const(I)),
				VnConst, VnTables0, VnTables),
			Vnrval = vn_binop((+), Vn11, VnConst)
		;
						% e11+c12+e21+c22 => e11+e21+c
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn_table__lookup_defn(Vn12, Vnrval12,	
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_binop((+), Vn21, Vn22),
			vn_table__lookup_defn(Vn22, Vnrval22,
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval22 = vn_const(int_const(I22))
		->
			I is I12 + I22,
			vn_util__vnrval_to_vn(vn_binop((+), Vn11, Vn21), VnExpr,
				VnTables0, VnTables1),
			vn_util__vnrval_to_vn(vn_const(int_const(I)),
				VnConst, VnTables1, VnTables),
			Vnrval = vn_binop((+), VnExpr, VnConst)
		;
						% e11+c12+e2 => e11+e2+c12
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn_table__lookup_defn(Vn12, Vnrval12,
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval12 = vn_const(int_const(_I12)),
			Vnrval2 = vn_binop((+), _Vn21, _Vn22)
		->
			vn_util__vnrval_to_vn(vn_binop((+), Vn11, Vn2), VnExpr,
				VnTables0, VnTables),
			Vnrval = vn_binop((+), VnExpr, Vn12)
		;
						% e+0 => e
			Vnrval2 = vn_const(int_const(0))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
			fail
		)
	;
		Binop = (float_plus),
		(
						% c1+c2 => c
			Vnrval1 = vn_const(float_const(F1)),
			Vnrval2 = vn_const(float_const(F2))
		->
			F is F1 + F2,
			Vnrval = vn_const(float_const(F)),
			VnTables = VnTables0
		;
						% c1+e21+c22 => e21+c
			Vnrval1 = vn_const(float_const(F1)),
			Vnrval2 = vn_binop((float_plus), Vn21, Vn22),
			vn_table__lookup_defn(Vn22, Vnrval22,
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval22 = vn_const(float_const(F22))
		->
			F is F1 + F22,
			vn_util__vnrval_to_vn(vn_const(float_const(F)), VnConst,
				VnTables0, VnTables),
			Vnrval = vn_binop((float_plus), Vn21, VnConst)
		;
						% c1+e2 => e2+c1
			Vnrval1 = vn_const(float_const(_F1)),
			Vnrval2 = vn_binop((float_plus), _Vn21, _Vn22)
		->
			Vnrval = vn_binop((float_plus), Vn2, Vn1),
			VnTables = VnTables0
		;
						% e11+c12+c2 => e11+c
			Vnrval1 = vn_binop((float_plus), Vn11, Vn12),
			vn_table__lookup_defn(Vn12, Vnrval12,
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval12 = vn_const(float_const(F12)),
			Vnrval2 = vn_const(float_const(F2))
		->
			F is F12 + F2,
			vn_util__vnrval_to_vn(vn_const(float_const(F)),
				VnConst, VnTables0, VnTables),
			Vnrval = vn_binop((float_plus), Vn11, VnConst)
		;
						% e11+c12+e21+c22 => e11+e21+c
			Vnrval1 = vn_binop((float_plus), Vn11, Vn12),
			vn_table__lookup_defn(Vn12, Vnrval12,	
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval12 = vn_const(float_const(F12)),
			Vnrval2 = vn_binop((float_plus), Vn21, Vn22),
			vn_table__lookup_defn(Vn22, Vnrval22,
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval22 = vn_const(float_const(F22))
		->
			F is F12 + F22,
			vn_util__vnrval_to_vn(vn_binop((float_plus), Vn11,
				Vn21), VnExpr, VnTables0, VnTables1),
			vn_util__vnrval_to_vn(vn_const(float_const(F)),
				VnConst, VnTables1, VnTables),
			Vnrval = vn_binop((float_plus), VnExpr, VnConst)
		;
						% e11+c12+e2 => e11+e2+c12
			Vnrval1 = vn_binop((float_plus), Vn11, Vn12),
			vn_table__lookup_defn(Vn12, Vnrval12,
				"vn_util__simplify_vnrval", VnTables0),
			Vnrval12 = vn_const(float_const(_F12)),
			Vnrval2 = vn_binop((float_plus), _Vn21, _Vn22)
		->
			vn_util__vnrval_to_vn(vn_binop((float_plus), Vn11,
				Vn2), VnExpr, VnTables0, VnTables),
			Vnrval = vn_binop((float_plus), VnExpr, Vn12)
		;
						% e+0 => e
			Vnrval2 = vn_const(float_const(0.0))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
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
			vn_util__vnrval_to_vn(vn_const(int_const(NI2)), VnConst,
				VnTables0, VnTables1),
			vn_util__simplify_vnrval(vn_binop((+), Vn1, VnConst),
				Vnrval, VnTables1, VnTables)
		;
						% c1-e2 => 0-e2+c1
			Vnrval1 = vn_const(int_const(I1)),
			I1 \= 0
		->
			vn_util__vnrval_to_vn(vn_const(int_const(0)), VnConst0,
				VnTables0, VnTables1),
			vn_util__vnrval_to_vn(vn_binop((-), VnConst0, Vn2),
				VnExpr, VnTables1, VnTables2),
			vn_util__simplify_vnrval(vn_binop((+), VnExpr, Vn1),
				Vnrval, VnTables2, VnTables)
		;
						% e1-e1 => 0
			Vn1 = Vn2
		->
			Vnrval = vn_const(int_const(0)),
			VnTables = VnTables0
		;
						% e-0 => e
			Vnrval2 = vn_const(int_const(0))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
			fail
		)
	;
		Binop = (float_minus),
		(
						% e1-c2 => e1+c
			Vnrval2 = vn_const(float_const(F2))
		->
			NF2 is 0.0 - F2,
			vn_util__vnrval_to_vn(vn_const(float_const(NF2)),
				VnConst, VnTables0, VnTables1),
			vn_util__simplify_vnrval(vn_binop((float_plus), Vn1,
				VnConst), Vnrval, VnTables1, VnTables)
		;
						% c1-e2 => 0-e2+c1
			Vnrval1 = vn_const(float_const(F1)),
			F1 \= 0.0
		->
			vn_util__vnrval_to_vn(vn_const(float_const(0.0)),
				VnConst0, VnTables0, VnTables1),
			vn_util__vnrval_to_vn(vn_binop((float_minus), VnConst0,
				Vn2), VnExpr, VnTables1, VnTables2),
			vn_util__simplify_vnrval(vn_binop((float_plus), VnExpr,
				Vn1), Vnrval, VnTables2, VnTables)
		;
						% e1-e1 => 0
			Vn1 = Vn2
		->
			Vnrval = vn_const(int_const(0)),
			VnTables = VnTables0
		;
						% e-0 => e
			Vnrval2 = vn_const(float_const(0.0))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
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
						% e*1 => e
			Vnrval2 = vn_const(int_const(1))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
						% 1*e => e
			Vnrval1 = vn_const(int_const(1))
		->
			Vnrval = Vnrval2,
			VnTables = VnTables0
		;
			fail
		)
	;
		Binop = (float_times),
		(
						% c1*c2 => c
			Vnrval1 = vn_const(float_const(F1)),
			Vnrval2 = vn_const(float_const(F2))
		->
			F is F1 * F2,
			Vnrval = vn_const(float_const(F)),
			VnTables = VnTables0
		;
						% e*1 => e
			Vnrval2 = vn_const(float_const(1.0))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
						% 1*e => e
			Vnrval1 = vn_const(float_const(1.0))
		->
			Vnrval = Vnrval2,
			VnTables = VnTables0
		;
			fail
		)
	;
		Binop = (/),
		(
						% c1/c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2)),
			I2 \= 0
		->
			I is I1 // I2,
			Vnrval = vn_const(int_const(I)),
			VnTables = VnTables0
		;
						% e/1 => e
			Vnrval2 = vn_const(int_const(1))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
			fail
		)
	;
		Binop = (float_divide),
		(
						% c1/c2 => c
			Vnrval1 = vn_const(float_const(F1)),
			Vnrval2 = vn_const(float_const(F2)),
			F2 \= 0.0
		->
			F is F1 / F2,
			Vnrval = vn_const(float_const(F)),
			VnTables = VnTables0
		;
						% e/1 => e
			Vnrval2 = vn_const(float_const(1.0))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
			fail
		)
	;	
		Binop = ('|'),
		(
						% c1\/c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 \/ I2,
			Vnrval = vn_const(int_const(I)),
			VnTables = VnTables0
		;
						% e\/0 => e
			Vnrval2 = vn_const(int_const(0))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
						% 0\/e => e
			Vnrval1 = vn_const(int_const(0))
		->
			Vnrval = Vnrval2,
			VnTables = VnTables0
		;
						% e\/e => e
			Vn1 = Vn2
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
			fail
		)
	;	
		Binop = (&),
		(
						% c1/\c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 /\ I2,
			Vnrval = vn_const(int_const(I)),
			VnTables = VnTables0
		;
						% e/\0 => 0
			Vnrval2 = vn_const(int_const(0))
		->
			Vnrval = vn_const(int_const(0)),
			VnTables = VnTables0
		;
						% 0/\e => 0
			Vnrval1 = vn_const(int_const(0))
		->
			Vnrval = vn_const(int_const(0)),
			VnTables = VnTables0
		;
						% e/\e => e
			Vn1 = Vn2
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
			fail
		)
	;	
		Binop = (>>),
		(
						% c1>>c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 >> I2,
			Vnrval = vn_const(int_const(I)),
			VnTables = VnTables0
		;
						% e>>0 => e
			Vnrval2 = vn_const(int_const(0))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
			fail
		)
	;	
		Binop = (<<),
		(
						% c1<<c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 << I2,
			Vnrval = vn_const(int_const(I)),
			VnTables = VnTables0
		;
						% e<<0 => e
			Vnrval2 = vn_const(int_const(0))
		->
			Vnrval = Vnrval1,
			VnTables = VnTables0
		;
			fail
		)
	;	
		Binop = eq,
		(
						% e==e => true
			Vn1 = Vn2
		->
			Vnrval = vn_const(true),
			VnTables = VnTables0
		;
						% otherwise, c1==c2 => false
						% (true case handled above)
			Vnrval1 = vn_const(_C1),
			Vnrval2 = vn_const(_C2)
		->
			Vnrval = vn_const(false),
			VnTables = VnTables0
		;
						% tag(mktag(e))==e => true
			Vnrval1 = vn_unop(tag, WordVn),
			Vnrval2 = vn_unop(mktag, Tag),
			vn_table__lookup_defn(WordVn, WordVnrval,
				"vn_util__simplify_vnrval_binop", VnTables0),
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
						% e!=e => false
			Vn1 = Vn2
		->
			Vnrval = vn_const(false),
			VnTables = VnTables0
		;
						% otherwise, c1!=c2 => true
						% (false case handled above)
			Vnrval1 = vn_const(_C1),
			Vnrval2 = vn_const(_C2)
		->
			Vnrval = vn_const(true),
			VnTables = VnTables0
		;
						% tag(mktag(e))!=e => false
			Vnrval1 = vn_unop(tag, WordVn),
			Vnrval2 = vn_unop(mktag, Tag),
			vn_table__lookup_defn(WordVn, WordVnrval,
				"vn_util__simplify_vnrval_binop", VnTables0),
			WordVnrval = vn_mkword(Tag, _)
		->
			Vnrval = vn_const(false),
			VnTables = VnTables0
		;
			fail
		)
	;
		Binop = (>=),
		vn_util__simplify_int_compare_op(>=, Vn1, Vnrval1, Vn2, Vnrval2,
			true, Vnrval),
		VnTables = VnTables0
	;
		Binop = (<=),
		vn_util__simplify_int_compare_op(=<, Vn1, Vnrval1, Vn2, Vnrval2,
			true, Vnrval),
		VnTables = VnTables0
	;
		Binop = (>),
		vn_util__simplify_int_compare_op(>, Vn1, Vnrval1, Vn2, Vnrval2,
			false, Vnrval),
		VnTables = VnTables0
	;
		Binop = (<),
		vn_util__simplify_int_compare_op(<, Vn1, Vnrval1, Vn2, Vnrval2,
			false, Vnrval),
		VnTables = VnTables0
	;
		Binop = (float_eq),
		vn_util__simplify_float_compare_op(float_eq, Vn1, Vnrval1,
			Vn2, Vnrval2, true, Vnrval),
		VnTables = VnTables0
	;
		Binop = (float_ge),
		vn_util__simplify_float_compare_op(>=, Vn1, Vnrval1,
			Vn2, Vnrval2, true, Vnrval),
		VnTables = VnTables0
	;
		Binop = (float_le),
		vn_util__simplify_float_compare_op(=<, Vn1, Vnrval1,
			Vn2, Vnrval2, true, Vnrval),
		VnTables = VnTables0
	;
		Binop = (float_ne),
		vn_util__simplify_float_compare_op(float_ne, Vn1, Vnrval1,
			Vn2, Vnrval2, false, Vnrval),
		VnTables = VnTables0
	;
		Binop = (float_gt),
		vn_util__simplify_float_compare_op(>=, Vn1, Vnrval1,
			Vn2, Vnrval2, false, Vnrval),
		VnTables = VnTables0
	;
		Binop = (float_lt),
		vn_util__simplify_float_compare_op(=<, Vn1, Vnrval1,
			Vn2, Vnrval2, false, Vnrval),
		VnTables = VnTables0
	;
		Binop = (str_eq),
		vn_util__const_if_equal_vns(Vn1, Vn2, true, Vnrval),
		VnTables = VnTables0
	;
		Binop = (str_ge),
		vn_util__const_if_equal_vns(Vn1, Vn2, true, Vnrval),
		VnTables = VnTables0
	;
		Binop = (str_le),
		vn_util__const_if_equal_vns(Vn1, Vn2, true, Vnrval),
		VnTables = VnTables0
	;
		Binop = (str_ne),
		vn_util__const_if_equal_vns(Vn1, Vn2, false, Vnrval),
		VnTables = VnTables0
	;
		Binop = (str_gt),
		vn_util__const_if_equal_vns(Vn1, Vn2, false, Vnrval),
		VnTables = VnTables0
	;
		Binop = (str_lt),
		vn_util__const_if_equal_vns(Vn1, Vn2, false, Vnrval),
		VnTables = VnTables0
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

:- pred vn_util__simplify_int_compare_op(pred(int, int),
		vn, vnrval, vn, vnrval, rval_const, vnrval).
:- mode vn_util__simplify_int_compare_op(pred(in, in) is semidet,
		in, in, in, in, in, out) is semidet.

vn_util__simplify_int_compare_op(ComparePred, Vn1, Vnrval1, Vn2, Vnrval2,
		ResultIfEqual, Vnrval) :-
	(
		Vnrval1 = vn_const(int_const(C1)),
		Vnrval2 = vn_const(int_const(C2))
	->
		(
			call(ComparePred, C1, C2)
		->
			Vnrval = vn_const(true)
		;
			Vnrval = vn_const(false)
		)
	;
		vn_util__const_if_equal_vns(Vn1, Vn2, ResultIfEqual, Vnrval)
	).

:- pred vn_util__simplify_float_compare_op(pred(float, float),
		vn, vnrval, vn, vnrval, rval_const, vnrval).
:- mode vn_util__simplify_float_compare_op(pred(in, in) is semidet,
		in, in, in, in, in, out) is semidet.

vn_util__simplify_float_compare_op(ComparePred, Vn1, Vnrval1, Vn2, Vnrval2,
		ResultIfEqual, Vnrval) :-
	(
		Vnrval1 = vn_const(float_const(C1)),
		Vnrval2 = vn_const(float_const(C2))
	->
		(
			call(ComparePred, C1, C2)
		->
			Vnrval = vn_const(true)
		;
			Vnrval = vn_const(false)
		)
	;
		vn_util__const_if_equal_vns(Vn1, Vn2, ResultIfEqual, Vnrval)
	).

:- pred float_eq(float::in, float::in) is semidet.
float_eq(X, X).

:- pred float_ne(float::in, float::in) is semidet.
float_ne(X, Y) :- X \= Y.

:- pred vn_util__const_if_equal_vns(vn, vn, rval_const, vnrval).
:- mode vn_util__const_if_equal_vns(in, in, in, out) is semidet.

vn_util__const_if_equal_vns(Vn1, Vn2, Const, Vnrval) :-
	( Vn1 = Vn2 ->
		Vnrval = vn_const(Const)
	;
		fail
	).

vn_util__lval_to_vn(Lval, Vn, VnTables0, VnTables) :-
	vn_util__lval_to_vnlval(Lval, Vnlval, VnTables0, VnTables1),
	( vn_table__search_desired_value(Vnlval, Vn_prime, VnTables1) ->
		Vn = Vn_prime,
		VnTables = VnTables1
	;
		vn_table__record_first_vnlval(Vnlval, Vn, VnTables1, VnTables)
	).

vn_util__lval_to_vnlval(Lval, Vnlval, VnTables0, VnTables) :-
	vn_util__no_access_lval_to_vnlval(Lval, MaybeVnlval),
	( MaybeVnlval = yes(VnlvalPrime) ->
		Vnlval = VnlvalPrime,
		VnTables = VnTables0
	; Lval = field(Tag, Rval1, Rval2) ->
		vn_util__rval_to_vn(Rval1, Vn1, VnTables0, VnTables1),
		vn_util__rval_to_vn(Rval2, Vn2, VnTables1, VnTables),
		Vnlval = vn_field(Tag, Vn1, Vn2)
	; Lval = mem_ref(Rval1) ->
		vn_util__rval_to_vn(Rval1, Vn1, VnTables0, VnTables),
		Vnlval = vn_mem_ref(Vn1)
	; Lval = succfr(Rval1) ->
		vn_util__rval_to_vn(Rval1, Vn1, VnTables0, VnTables),
		Vnlval = vn_succfr(Vn1)
	; Lval = prevfr(Rval1) ->
		vn_util__rval_to_vn(Rval1, Vn1, VnTables0, VnTables),
		Vnlval = vn_prevfr(Vn1)
	; Lval = redoip(Rval1) ->
		vn_util__rval_to_vn(Rval1, Vn1, VnTables0, VnTables),
		Vnlval = vn_redoip(Vn1)
	; Lval = succip(Rval1) ->
		vn_util__rval_to_vn(Rval1, Vn1, VnTables0, VnTables),
		Vnlval = vn_succip(Vn1)
	;
		error("unexpected lval in vn_util__lval_to_vnlval")
	).

% If you to add to this list to fix a determinism error,
% check vn_util__lval_to_vnlval above as well.
% vn_util__lval_to_vnlval should have code to handle every lval
% that is mapped to "no" by vn_util__no_access_lval_to_vnlval.

vn_util__no_access_lval_to_vnlval(reg(T, N),		yes(vn_reg(T, N))).
vn_util__no_access_lval_to_vnlval(temp(T, N),		yes(vn_temp(T, N))).
vn_util__no_access_lval_to_vnlval(stackvar(N),		yes(vn_stackvar(N))).
vn_util__no_access_lval_to_vnlval(framevar(N),		yes(vn_framevar(N))).
vn_util__no_access_lval_to_vnlval(succip,		yes(vn_succip)).
vn_util__no_access_lval_to_vnlval(maxfr,		yes(vn_maxfr)).
vn_util__no_access_lval_to_vnlval(curfr,		yes(vn_curfr)).
vn_util__no_access_lval_to_vnlval(redoip(_),		no).
vn_util__no_access_lval_to_vnlval(succip(_),		no).
vn_util__no_access_lval_to_vnlval(prevfr(_),		no).
vn_util__no_access_lval_to_vnlval(succfr(_),		no).
vn_util__no_access_lval_to_vnlval(hp,			yes(vn_hp)).
vn_util__no_access_lval_to_vnlval(sp,			yes(vn_sp)).
vn_util__no_access_lval_to_vnlval(mem_ref(_),		no).
vn_util__no_access_lval_to_vnlval(field(_, _, _),	no).
vn_util__no_access_lval_to_vnlval(lvar(_Var), _) :-
	error("lvar detected in value_number").

vn_util__no_access_vnlval_to_lval(vn_reg(T, N),		yes(reg(T, N))).
vn_util__no_access_vnlval_to_lval(vn_temp(T, N),	yes(temp(T, N))).
vn_util__no_access_vnlval_to_lval(vn_stackvar(N),	yes(stackvar(N))).
vn_util__no_access_vnlval_to_lval(vn_framevar(N),	yes(framevar(N))).
vn_util__no_access_vnlval_to_lval(vn_succip,		yes(succip)).
vn_util__no_access_vnlval_to_lval(vn_maxfr,		yes(maxfr)).
vn_util__no_access_vnlval_to_lval(vn_curfr,		yes(curfr)).
vn_util__no_access_vnlval_to_lval(vn_succfr(_),		no).
vn_util__no_access_vnlval_to_lval(vn_prevfr(_),		no).
vn_util__no_access_vnlval_to_lval(vn_redoip(_),		no).
vn_util__no_access_vnlval_to_lval(vn_succip(_),		no).
vn_util__no_access_vnlval_to_lval(vn_hp,		yes(hp)).
vn_util__no_access_vnlval_to_lval(vn_sp,		yes(sp)).
vn_util__no_access_vnlval_to_lval(vn_field(_, _, _),	no).
vn_util__no_access_vnlval_to_lval(vn_mem_ref(_),	no).

/* one of these preds should be eliminated XXX */
vn_util__vnlval_access_vns(vn_reg(_, _), []).
vn_util__vnlval_access_vns(vn_temp(_, _), []).
vn_util__vnlval_access_vns(vn_stackvar(_), []).
vn_util__vnlval_access_vns(vn_framevar(_), []).
vn_util__vnlval_access_vns(vn_succip, []).
vn_util__vnlval_access_vns(vn_maxfr, []).
vn_util__vnlval_access_vns(vn_curfr, []).
vn_util__vnlval_access_vns(vn_succfr(Vn), [Vn]).
vn_util__vnlval_access_vns(vn_prevfr(Vn), [Vn]).
vn_util__vnlval_access_vns(vn_redoip(Vn), [Vn]).
vn_util__vnlval_access_vns(vn_succip(Vn), [Vn]).
vn_util__vnlval_access_vns(vn_hp, []).
vn_util__vnlval_access_vns(vn_sp, []).
vn_util__vnlval_access_vns(vn_field(_, Vn1, Vn2), [Vn1, Vn2]).
vn_util__vnlval_access_vns(vn_mem_ref(Vn), [Vn]).

vn_util__find_sub_vns(vn_origlval(Vnlval), SubVns) :-
	vn_util__vnlval_access_vns(Vnlval, SubVns).
vn_util__find_sub_vns(vn_mkword(_, SubVn), [SubVn]).
vn_util__find_sub_vns(vn_const(_), []).
vn_util__find_sub_vns(vn_create(_, _, _, _, _), []).
vn_util__find_sub_vns(vn_unop(_, SubVn), [SubVn]).
vn_util__find_sub_vns(vn_binop(_, SubVn1, SubVn2), [SubVn1, SubVn2]).
vn_util__find_sub_vns(vn_stackvar_addr(_), []).
vn_util__find_sub_vns(vn_framevar_addr(_), []).
vn_util__find_sub_vns(vn_heap_addr(SubVn, _, _), [SubVn]).

vn_util__is_const_expr(Vn, IsConst, VnTables) :-
	vn_table__lookup_defn(Vn, Vnrval, "vn_util__is_const_expr", VnTables),
	(
		Vnrval = vn_origlval(_),
		IsConst = no
	;
		Vnrval = vn_mkword(_, Vn1),
		vn_util__is_const_expr(Vn1, IsConst, VnTables)
	;
		Vnrval = vn_const(_),
		IsConst = yes
	;
		Vnrval = vn_create(_, _, _, _, _),
		IsConst = yes
	;
		Vnrval = vn_unop(_, Vn1),
		vn_util__is_const_expr(Vn1, IsConst, VnTables)
	;	
		Vnrval = vn_binop(_, Vn1, Vn2),
		vn_util__is_const_expr(Vn1, IsConst1, VnTables),
		vn_util__is_const_expr(Vn2, IsConst2, VnTables),
		bool__and(IsConst1, IsConst2, IsConst)
	;
		Vnrval = vn_stackvar_addr(_),
		IsConst = no
	;
		Vnrval = vn_framevar_addr(_),
		IsConst = no
	;
		Vnrval = vn_heap_addr(_, _, _),
		IsConst = no
	).

vn_util__find_lvals_in_rval(Rval, Lvals) :-
	(
		Rval = lval(Lval),
		opt_util__lval_access_rvals(Lval, Rvals),
		vn_util__find_lvals_in_rvals(Rvals, Lvals1),
		Lvals = [Lval | Lvals1]
	;
		Rval = var(_),
		error("var found in vn_util__find_lvals_in_rval")
	;
		Rval = create(_, _, _, _, _),
		Lvals = []
	;
		Rval = mkword(_, Rval1),
		vn_util__find_lvals_in_rval(Rval1, Lvals)
	;
		Rval = const(_),
		Lvals = []
	;
		Rval = unop(_, Rval1),
		vn_util__find_lvals_in_rval(Rval1, Lvals)
	;
		Rval = binop(_, Rval1, Rval2),
		vn_util__find_lvals_in_rval(Rval1, Lvals1),
		vn_util__find_lvals_in_rval(Rval2, Lvals2),
		list__append(Lvals1, Lvals2, Lvals)
	;
		Rval = mem_addr(MemRef),
		vn_util__find_lvals_in_mem_ref(MemRef, Lvals)
	).

vn_util__find_lvals_in_rvals([], []).
vn_util__find_lvals_in_rvals([Rval | Rvals], Lvals) :-
	vn_util__find_lvals_in_rval(Rval, Lvals1),
	vn_util__find_lvals_in_rvals(Rvals, Lvals2),
	list__append(Lvals1, Lvals2, Lvals).

:- pred vn_util__find_lvals_in_mem_ref(mem_ref, list(lval)).
:- mode vn_util__find_lvals_in_mem_ref(in, out) is det.

	% XXX
vn_util__find_lvals_in_mem_ref(stackvar_ref(_), []).
vn_util__find_lvals_in_mem_ref(framevar_ref(_), []).
vn_util__find_lvals_in_mem_ref(heap_ref(Rval, _, _), Lvals) :-
	vn_util__find_lvals_in_rval(Rval, Lvals).

vn_util__is_vn_shared(Vn, Vnrval, Uses0, VnTables) :-
	vn_util__is_const_expr(Vn, no, VnTables),
	\+ Vnrval = vn_origlval(vn_hp),
	vn_util__real_uses(Uses0, Uses1, VnTables),
	Uses1 = [_,_|_].

vn_util__real_uses([], [], _VnTables).
vn_util__real_uses([Use0 | Uses0], Uses, VnTables) :-
	vn_util__real_uses(Uses0, Uses1, VnTables),
	( list__member(Use0, Uses1) ->
		Uses = Uses1
	; Use0 = src_liveval(Vnlval) ->
		(
			vn_table__search_desired_value(Vnlval, Vn, VnTables),
			vn_table__search_current_value(Vnlval, Vn, VnTables)
		->
			Uses = Uses1
		;
			Uses = [Use0 | Uses1]
		)
	; Use0 = src_access(Vnlval) ->
		(
			vn_table__search_desired_value(Vnlval, Vn, VnTables),
			vn_table__search_current_value(Vnlval, Vn, VnTables),
			( vn_table__search_uses(Vn, AccessUses, VnTables) ->
				(
					vn_util__real_uses(AccessUses, [],
						VnTables)
				;
					vn_table__search_current_locs(Vn, Locs,
						VnTables),
					vn_util__choose_cheapest_loc(Locs, Loc),
					vn_util__classify_loc_cost(Loc, 0)
				)
			;
				true
			)
		->
			Uses = Uses1
		;
			Uses = [Use0 | Uses1]
		)
	;
		Uses = [Use0 | Uses1]
	).

vn_util__choose_cheapest_loc(Locs, BestLoc) :-
	vn_util__choose_cheapest_loc_2(Locs, no, no, BestLoc).

:- pred vn_util__choose_cheapest_loc_2(list(vnlval), maybe(vnlval), maybe(vnlval),
	vnlval).
:- mode vn_util__choose_cheapest_loc_2(in, in, in, out) is semidet.

vn_util__choose_cheapest_loc_2([Loc | Locs], Stack0, Heap0, BestLoc) :-
	vn_util__classify_loc_cost(Loc, Cost),
	( Cost = 0 ->
		BestLoc = Loc
	; Cost = 1 ->
		vn_util__choose_cheapest_loc_2(Locs, yes(Loc), Heap0, BestLoc)
	;
		vn_util__choose_cheapest_loc_2(Locs, Stack0, yes(Loc), BestLoc)
	).
vn_util__choose_cheapest_loc_2([], Stack0, Heap0, BestLoc) :-
	( Stack0 = yes(Stack) ->
		BestLoc = Stack
	; Heap0 = yes(Heap) ->
		BestLoc = Heap
	;
		fail
	).

vn_util__classify_loc_cost(vn_reg(_, _), 0).
vn_util__classify_loc_cost(vn_temp(_, _), 0).
vn_util__classify_loc_cost(vn_stackvar(_), 1).
vn_util__classify_loc_cost(vn_framevar(_), 1).
vn_util__classify_loc_cost(vn_succip, 0).
vn_util__classify_loc_cost(vn_maxfr, 0).
vn_util__classify_loc_cost(vn_curfr, 0).
vn_util__classify_loc_cost(vn_succfr(_), 1).
vn_util__classify_loc_cost(vn_prevfr(_), 1).
vn_util__classify_loc_cost(vn_redoip(_), 1).
vn_util__classify_loc_cost(vn_succip(_), 1).
vn_util__classify_loc_cost(vn_hp, 0).
vn_util__classify_loc_cost(vn_sp, 0).
vn_util__classify_loc_cost(vn_field(_, _, _), 2).
vn_util__classify_loc_cost(vn_mem_ref(_), 2).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Build up a list of the uses of each vn.

:- pred vn_util__build_uses(vnlvalset, ctrlmap, vn_tables, vn_tables).
% :- mode vn_util__build_uses(in, in, di, uo) is det.
:- mode vn_util__build_uses(in, in, in, out) is det.

:- implementation.

vn_util__build_uses(Livevals, Ctrlmap, VnTables0, VnTables) :-
	vn_util__build_uses_from_ctrl(0, Ctrlmap, VnTables0, VnTables1),
	set__to_sorted_list(Livevals, Livelist),
	vn_util__build_uses_from_livevals(Livelist, VnTables1, VnTables).

:- pred vn_util__build_uses_from_ctrl(int, ctrlmap, vn_tables, vn_tables).
% :- mode vn_util__build_uses_from_ctrl(in, in, di, uo) is det.
:- mode vn_util__build_uses_from_ctrl(in, in, in, out) is det.

vn_util__build_uses_from_ctrl(Ctrl, Ctrlmap, VnTables0, VnTables) :-
	( map__search(Ctrlmap, Ctrl, VnInstr) ->
		(
			VnInstr = vn_livevals(_),
			VnTables1 = VnTables0
		;
			VnInstr = vn_call(_, _, _, _),
			VnTables1 = VnTables0
		;
			VnInstr = vn_mkframe(_, _, _, _),
			VnTables1 = VnTables0
		;
			VnInstr = vn_label(_),
			VnTables1 = VnTables0
		;
			VnInstr = vn_goto(_),
			VnTables1 = VnTables0
		;
			VnInstr = vn_computed_goto(Vn, _),
			vn_util__record_use(Vn, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_if_val(Vn, _),
			vn_util__record_use(Vn, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_mark_hp(Vnlval),
			vn_util__vnlval_access_vns(Vnlval, Vns),
			vn_util__record_use_list(Vns, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_restore_hp(Vn),
			vn_util__record_use(Vn, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_store_ticket(Vnlval),
			vn_util__vnlval_access_vns(Vnlval, Vns),
			vn_util__record_use_list(Vns, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_reset_ticket(Vn, _Reason),
			vn_util__record_use(Vn, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_discard_ticket,
			VnTables1 = VnTables0
		;
			VnInstr = vn_mark_ticket_stack(Vnlval),
			vn_util__vnlval_access_vns(Vnlval, Vns),
			vn_util__record_use_list(Vns, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_discard_tickets_to(Vn),
			vn_util__record_use(Vn, src_ctrl(Ctrl),
				VnTables0, VnTables1)
		;
			VnInstr = vn_incr_sp(_, _),
			VnTables1 = VnTables0
		;
			VnInstr = vn_decr_sp(_),
			VnTables1 = VnTables0
		),
		NextCtrl is Ctrl + 1,
		vn_util__build_uses_from_ctrl(NextCtrl, Ctrlmap,
			VnTables1, VnTables)
	;
		VnTables = VnTables0
	).

	% We have to record two kinds of uses. The first is the use of the
	% value number we want to assign to the vnlval. The second is the
	% value numbers needed to access the vnlval at all.

:- pred vn_util__build_uses_from_livevals(list(vnlval), vn_tables, vn_tables).
% :- mode vn_util__build_uses_from_livevals(in, di, uo) is det.
:- mode vn_util__build_uses_from_livevals(in, in, out) is det.

vn_util__build_uses_from_livevals([], VnTables, VnTables).
vn_util__build_uses_from_livevals([Live | Liveslist], VnTables0, VnTables) :-
	( vn_table__search_desired_value(Live, VnPrime, VnTables0) ->
		Vn = VnPrime,
		VnTables1 = VnTables0
	;
		vn_table__record_first_vnlval(Live, Vn, VnTables0, VnTables1)
	),
	vn_util__record_use(Vn, src_liveval(Live), VnTables1, VnTables2),
	vn_util__record_access([Live], VnTables2, VnTables3),
	vn_util__build_uses_from_livevals(Liveslist, VnTables3, VnTables).

:- pred vn_util__record_access(list(vnlval), vn_tables, vn_tables).
% :- mode vn_util__record_access(in, di, uo) is det.
:- mode vn_util__record_access(in, in, out) is det.

vn_util__record_access([], VnTables, VnTables).
vn_util__record_access([Vnlval | Vnlvals], VnTables0, VnTables) :-
	vn_util__vnlval_access_vns(Vnlval, SubVns),
	vn_util__record_use_list(SubVns, src_access(Vnlval),
		VnTables0, VnTables1),
	vn_util__record_access(Vnlvals, VnTables1, VnTables).

:- pred vn_util__record_use(vn, vn_src, vn_tables, vn_tables).
% :- mode vn_util__record_use(in, in, di, uo) is det.
:- mode vn_util__record_use(in, in, in, out) is det.

vn_util__record_use(Vn, Src, VnTables0, VnTables) :-
	vn_table__lookup_uses(Vn, OldUses, "vn_util__record_use", VnTables0),
	vn_table__add_new_use(Vn, Src, VnTables0, VnTables1),
	( OldUses = [] ->
		vn_table__lookup_defn(Vn, Vnrval, "vn_util__record_use",
			VnTables1),
		(
			Vnrval = vn_origlval(Vnlval),
			vn_util__record_access([Vnlval], VnTables1, VnTables)
		;
			Vnrval = vn_mkword(_, SubVn),
			vn_util__record_use(SubVn, src_vn(Vn),
				VnTables1, VnTables)
		;
			Vnrval = vn_const(_),
			VnTables = VnTables1
		;
			Vnrval = vn_create(_, _, _, _, _),
			VnTables = VnTables1
		;
			Vnrval = vn_unop(_, SubVn),
			vn_util__record_use(SubVn, src_vn(Vn),
				VnTables1, VnTables)
		;
			Vnrval = vn_binop(_, SubVn1, SubVn2),
			vn_util__record_use(SubVn1, src_vn(Vn),
				VnTables1, VnTables2),
			vn_util__record_use(SubVn2, src_vn(Vn),
				VnTables2, VnTables)
		;
			Vnrval = vn_stackvar_addr(_),
			VnTables = VnTables1
		;
			Vnrval = vn_framevar_addr(_),
			VnTables = VnTables1
		;
			Vnrval = vn_heap_addr(SubVn, _, _),
			vn_util__record_use(SubVn, src_vn(Vn),
				VnTables1, VnTables)
		)
	;
		VnTables = VnTables1
	).

:- pred vn_util__record_use_list(list(vn), vn_src, vn_tables, vn_tables).
% :- mode vn_util__record_use_list(in, in, di, uo) is det.
:- mode vn_util__record_use_list(in, in, in, out) is det.

vn_util__record_use_list([], _Src, VnTables, VnTables).
vn_util__record_use_list([Vn | Vns], Src, VnTables0, VnTables) :-
	vn_util__record_use(Vn, Src, VnTables0, VnTables1),
	vn_util__record_use_list(Vns, Src, VnTables1, VnTables).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
