%-----------------------------------------------------------------------------%

% Vn_util.nl - utility predicates for value numbering.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module vn_util.

:- interface.

:- import_module vn_type.
:- import_module llds, list, bintree_set, int.

:- implementation.

:- import_module vn_table.
:- import_module string, require, std_util, map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule for the forward phase of vn__handle_instr

:- interface.

:- pred vn__find_specials(vnlval, list(vnlval)).
:- mode vn__find_specials(in, out) is det.

:- pred vn__convert_to_vnlval_and_insert(list(lval), vnlvalset, vnlvalset).
:- mode vn__convert_to_vnlval_and_insert(in, di, uo) is det.

:- implementation.

vn__find_specials(vn_reg(_), []).
vn__find_specials(vn_stackvar(_), []).
vn__find_specials(vn_framevar(_), []).
vn__find_specials(vn_succip, [vn_succip]).
vn__find_specials(vn_maxfr, [vn_maxfr]).
vn__find_specials(vn_curredoip, [vn_curredoip]).
vn__find_specials(vn_hp, [vn_hp]).
vn__find_specials(vn_sp, [vn_sp]).
vn__find_specials(vn_field(_, _, _), []).
vn__find_specials(vn_temp(_), []).

vn__convert_to_vnlval_and_insert([], Liveset, Liveset).
vn__convert_to_vnlval_and_insert([Lval | Lvals], Liveset0, Liveset) :-
	vn__no_heap_lval_to_vnlval(Lval, MaybeVnlval),
	(
		MaybeVnlval = yes(Vnlval),
		bintree_set__insert(Liveset0, Vnlval, Liveset1)
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
:- mode vn__rval_to_vn(in, out, di, uo) is det.

:- pred vn__lval_to_vnlval(lval, vnlval, vn_tables, vn_tables).
:- mode vn__lval_to_vnlval(in, out, di, uo) is det.

:- pred vn__is_const_expr(vn, bool, vn_tables).
:- mode vn__is_const_expr(in, out, in) is det.

	% Find out what rvals, if any, are needed to access an lval.

:- pred vn__lval_access_rval(lval, list(rval)).
:- mode vn__lval_access_rval(in, out) is det.

	% Find out what vns, if any, are needed to access a vnlval.

:- pred vn__vnlval_access_vns(vnlval, list(vn)).
:- mode vn__vnlval_access_vns(in, out) is det.

	% Turn an vnlval into an lval if possible.

:- pred vn__no_heap_vnlval_to_lval(vnlval, maybe(lval)).
:- mode vn__no_heap_vnlval_to_lval(in, out) is det.

	% Do the reverse.

:- pred vn__no_heap_lval_to_vnlval(lval, maybe(vnlval)).
:- mode vn__no_heap_lval_to_vnlval(in, out) is det.

:- pred vn__find_sub_vns(vnrval, list(vn)).
:- mode vn__find_sub_vns(in, out) is det.

:- pred vn__find_sub_vns_vnlval(vnlval, list(vn)).
:- mode vn__find_sub_vns_vnlval(in, out) is det.

	% Find all lvals inside a given rval.

:- pred vn__find_lvals_in_rval(rval, list(lval)).
:- mode vn__find_lvals_in_rval(in, out) is det.

:- pred vn__find_lvals_in_rvals(list(rval), list(lval)).
:- mode vn__find_lvals_in_rvals(in, out) is det.

:- implementation.

%-----------------------------------------------------------------------------%

vn__rval_to_vn(Rval, Vn, Vn_tables0, Vn_tables) :-
	(
		Rval = lval(Lval),
		vn__lval_to_vn(Lval, Vn, Vn_tables0, Vn_tables)
	;
		Rval = var(_),
		error("value_number should never get rval: var")
	;
		Rval = create(Tag, Args, Label),
		vn__vnrval_to_vn(vn_create(Tag, Args, Label), Vn,
			Vn_tables0, Vn_tables)
	;
		Rval = mkword(Tag, Rval1),
		vn__rval_to_vn(Rval1, SubVn, Vn_tables0, Vn_tables1),
		vn__vnrval_to_vn(vn_mkword(Tag, SubVn), Vn,
			Vn_tables1, Vn_tables)
	;
		Rval = const(Const),
		vn__vnrval_to_vn(vn_const(Const), Vn,
			Vn_tables0, Vn_tables)
	;
		Rval = unop(Unop, Rval1),
		vn__rval_to_vn(Rval1, SubVn, Vn_tables0, Vn_tables1),
		vn__vnrval_to_vn(vn_unop(Unop, SubVn), Vn,
			Vn_tables1, Vn_tables)
	;
		Rval = binop(Binop, Rval1, Rval2),
		vn__rval_to_vn(Rval1, SubVn1, Vn_tables0, Vn_tables1),
		vn__rval_to_vn(Rval2, SubVn2, Vn_tables1, Vn_tables2),
		vn__vnrval_to_vn(vn_binop(Binop, SubVn1, SubVn2), Vn,
			Vn_tables2, Vn_tables)
	).

:- pred vn__vnrval_to_vn(vnrval, vn, vn_tables, vn_tables).
:- mode vn__vnrval_to_vn(in, out, di, uo) is det.

vn__vnrval_to_vn(Vnrval, Vn, Vn_tables0, Vn_tables) :-
	vn__simplify_vnrval(Vnrval, Vnrval1, Vn_tables0, Vn_tables1),
	( vn__search_assigned_vn(Vnrval1, Vn_prime, Vn_tables1) ->
		Vn = Vn_prime,
		Vn_tables = Vn_tables1
	;
		vn__record_first_vnrval(Vnrval1, Vn, Vn_tables1, Vn_tables)
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
:- mode vn__simplify_vnrval(in, out, di, uo) is det.

vn__simplify_vnrval(Vnrval0, Vnrval, Vn_tables0, Vn_tables) :-
	( Vnrval0 = vn_binop((+), Vn1, Vn2) ->
		vn__lookup_defn(Vn1, Vnrval1, Vn_tables0),
		vn__lookup_defn(Vn2, Vnrval2, Vn_tables0),
		(
							% c1+c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 + I2,
			Vnrval = vn_const(int_const(I)),
			Vn_tables = Vn_tables0
		;
							% c1+e21+c22 => e21+c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_binop((+), Vn21, Vn22),
			vn__lookup_defn(Vn22, Vnrval22, Vn_tables0),
			Vnrval22 = vn_const(int_const(I22))
		->
			I is I1 + I22,
			vn__vnrval_to_vn(vn_const(int_const(I)), VnConst,
				Vn_tables0, Vn_tables),
			Vnrval = vn_binop((+), Vn21, VnConst)
		;
							% c1+e2 => e2+c1
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_binop((+), Vn21, Vn22)
		->
			Vnrval = vn_binop((+), Vn2, Vn1),
			Vn_tables = Vn_tables0
		;
							% e11+c12+c2 => e11+c
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn__lookup_defn(Vn12, Vnrval12, Vn_tables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I12 + I2,
			vn__vnrval_to_vn(vn_const(int_const(I)),
				VnConst, Vn_tables0, Vn_tables),
			Vnrval = vn_binop((+), Vn11, VnConst)
		;
							% e11+c12+e21+c22 =>
							%	e11+e21+c
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn__lookup_defn(Vn12, Vnrval12, Vn_tables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_binop((+), Vn21, Vn22),
			vn__lookup_defn(Vn22, Vnrval22, Vn_tables0),
			Vnrval22 = vn_const(int_const(I22))
		->
			I is I12 + I22,
			vn__vnrval_to_vn(vn_binop((+), Vn11, Vn21), VnExpr,
				Vn_tables0, Vn_tables1),
			vn__vnrval_to_vn(vn_const(int_const(I)),
				VnConst, Vn_tables1, Vn_tables),
			Vnrval = vn_binop((+), VnExpr, VnConst)
		;
							% e11+c12+e2 =>
							%	e11+e2+c12
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn__lookup_defn(Vn12, Vnrval12, Vn_tables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_binop((+), Vn21, Vn22)
		->
			vn__vnrval_to_vn(vn_binop((+), Vn11, Vn2), VnExpr,
				Vn_tables0, Vn_tables),
			Vnrval = vn_binop((+), VnExpr, Vn12)
		;
			Vnrval = Vnrval0,
			Vn_tables = Vn_tables0
		)
	; Vnrval0 = vn_binop((-), Vn1, Vn2) ->
		vn__lookup_defn(Vn1, Vnrval1, Vn_tables0),
		vn__lookup_defn(Vn2, Vnrval2, Vn_tables0),
		(
							% e1-c2 => e1+c
			Vnrval2 = vn_const(int_const(I2))
		->
			NI2 is 0 - I2,
			vn__vnrval_to_vn(vn_const(int_const(NI2)), VnConst,
				Vn_tables0, Vn_tables1),
			vn__simplify_vnrval(vn_binop((+), Vn1, VnConst), Vnrval,
				Vn_tables1, Vn_tables)
		;
							% c1-e2 => 0-e2+c1
			Vnrval1 = vn_const(int_const(I1))
		->
			vn__vnrval_to_vn(vn_const(int_const(0)), VnConst0,
				Vn_tables0, Vn_tables1),
			vn__vnrval_to_vn(vn_binop((-), VnConst0, Vn2), VnExpr,
				Vn_tables1, Vn_tables2),
			vn__simplify_vnrval(vn_binop((+), VnExpr, Vn1), Vnrval,
				Vn_tables2, Vn_tables)
		;
			Vnrval = Vnrval0,
			Vn_tables = Vn_tables0
		)
	; Vnrval0 = vn_binop((*), Vn1, Vn2) ->
		vn__lookup_defn(Vn1, Vnrval1, Vn_tables0),
		vn__lookup_defn(Vn2, Vnrval2, Vn_tables0),
		(
							% c1*c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 * I2,
			Vnrval = vn_const(int_const(I)),
			Vn_tables = Vn_tables0
		;
			Vnrval = Vnrval0,
			Vn_tables = Vn_tables0
		)
	; Vnrval0 = vn_binop((/), Vn1, Vn2) ->
		vn__lookup_defn(Vn1, Vnrval1, Vn_tables0),
		vn__lookup_defn(Vn2, Vnrval2, Vn_tables0),
		(
							% c1/c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 // I2,
			Vnrval = vn_const(int_const(I)),
			Vn_tables = Vn_tables0
		;
			Vnrval = Vnrval0,
			Vn_tables = Vn_tables0
		)
	;
		Vnrval = Vnrval0,
		Vn_tables = Vn_tables0
	).

:- pred vn__lval_to_vn(lval, vn, vn_tables, vn_tables).
:- mode vn__lval_to_vn(in, out, di, uo) is det.

vn__lval_to_vn(Lval, Vn, Vn_tables0, Vn_tables) :-
	vn__lval_to_vnlval(Lval, Vnlval, Vn_tables0, Vn_tables1),
	( vn__search_desired_value(Vnlval, Vn_prime, Vn_tables1) ->
		Vn = Vn_prime,
		Vn_tables = Vn_tables1
	;
		vn__record_first_vnlval(Vnlval, Vn, Vn_tables1, Vn_tables)
	).

vn__lval_to_vnlval(Lval, Vnlval, Vn_tables0, Vn_tables) :-
	vn__no_heap_lval_to_vnlval(Lval, MaybeVnlval),
	( MaybeVnlval = yes(VnlvalPrime) ->
		Vnlval = VnlvalPrime,
		Vn_tables = Vn_tables0
	; Lval = field(Tag, Rval1, Rval2) ->
		vn__rval_to_vn(Rval1, Vn1, Vn_tables0, Vn_tables1),
		vn__rval_to_vn(Rval2, Vn2, Vn_tables1, Vn_tables),
		Vnlval = vn_field(Tag, Vn1, Vn2)
	;
		error("unexpected lval in vn__lval_to_vnlval")
	).

% If you to add to this list to fix a determinism error,
% check vn__lval_to_vnlval above as well.

vn__no_heap_lval_to_vnlval(reg(Reg),		yes(vn_reg(Reg))).
vn__no_heap_lval_to_vnlval(stackvar(N),		yes(vn_stackvar(N))).
vn__no_heap_lval_to_vnlval(framevar(N),		yes(vn_framevar(N))).
vn__no_heap_lval_to_vnlval(succip,		yes(vn_succip)).
vn__no_heap_lval_to_vnlval(maxfr,		yes(vn_maxfr)).
vn__no_heap_lval_to_vnlval(curredoip,		yes(vn_curredoip)).
vn__no_heap_lval_to_vnlval(hp,			yes(vn_hp)).
vn__no_heap_lval_to_vnlval(sp,			yes(vn_sp)).
vn__no_heap_lval_to_vnlval(field(_, _, _),	no).
vn__no_heap_lval_to_vnlval(temp(N),		yes(vn_temp(N))).
vn__no_heap_lval_to_vnlval(lvar(_Var), _) :-
	error("lvar detected in value_number").

vn__no_heap_vnlval_to_lval(vn_reg(Reg),		yes(reg(Reg))).
vn__no_heap_vnlval_to_lval(vn_stackvar(N),	yes(stackvar(N))).
vn__no_heap_vnlval_to_lval(vn_framevar(N),	yes(framevar(N))).
vn__no_heap_vnlval_to_lval(vn_succip,		yes(succip)).
vn__no_heap_vnlval_to_lval(vn_maxfr,		yes(maxfr)).
vn__no_heap_vnlval_to_lval(vn_curredoip,	yes(curredoip)).
vn__no_heap_vnlval_to_lval(vn_hp,		yes(hp)).
vn__no_heap_vnlval_to_lval(vn_sp,		yes(sp)).
vn__no_heap_vnlval_to_lval(vn_field(_, _, _), 	no).
vn__no_heap_vnlval_to_lval(vn_temp(N),		yes(temp(N))).

vn__lval_access_rval(reg(_), []).
vn__lval_access_rval(stackvar(_), []).
vn__lval_access_rval(framevar(_), []).
vn__lval_access_rval(succip, []).
vn__lval_access_rval(maxfr, []).
vn__lval_access_rval(curredoip, []).
vn__lval_access_rval(hp, []).
vn__lval_access_rval(sp, []).
vn__lval_access_rval(field(_, Rval1, Rval2), [Rval1, Rval2]).
vn__lval_access_rval(temp(_), []).
vn__lval_access_rval(lvar(_), _) :-
	error("lvar detected in value_number").

vn__vnlval_access_vns(vn_reg(_), []).
vn__vnlval_access_vns(vn_stackvar(_), []).
vn__vnlval_access_vns(vn_framevar(_), []).
vn__vnlval_access_vns(vn_succip, []).
vn__vnlval_access_vns(vn_maxfr, []).
vn__vnlval_access_vns(vn_curredoip, []).
vn__vnlval_access_vns(vn_hp, []).
vn__vnlval_access_vns(vn_sp, []).
vn__vnlval_access_vns(vn_field(_, Vn1, Vn2), [Vn1, Vn2]).
vn__vnlval_access_vns(vn_temp(_), []).

vn__is_const_expr(Vn, IsConst, Vn_tables) :-
	vn__lookup_defn(Vn, Vnrval, Vn_tables),
	(
		Vnrval = vn_origlval(_),
		IsConst = no
	;
		Vnrval = vn_mkword(_, Vn1),
		vn__is_const_expr(Vn1, IsConst, Vn_tables)
	;
		Vnrval = vn_const(_),
		IsConst = yes
	;
		Vnrval = vn_create(_, _, _),
		IsConst = yes
	;
		Vnrval = vn_unop(_, Vn1),
		vn__is_const_expr(Vn1, IsConst, Vn_tables)
	;	
		Vnrval = vn_binop(_, Vn1, Vn2),
		vn__is_const_expr(Vn1, IsConst1, Vn_tables),
		vn__is_const_expr(Vn2, IsConst2, Vn_tables),
		bool__and(IsConst1, IsConst2, IsConst)
	).

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
vn__find_sub_vns_vnlval(vn_curredoip, []).
vn__find_sub_vns_vnlval(vn_hp, []).
vn__find_sub_vns_vnlval(vn_sp, []).
vn__find_sub_vns_vnlval(vn_field(_, Vn1, Vn2), [Vn1, Vn2]).
vn__find_sub_vns_vnlval(vn_temp(_), []).

vn__find_lvals_in_rval(Rval, Lvals) :-
	(
		Rval = lval(Lval),
		vn__lval_access_rval(Lval, Rvals),
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Build up a list of the uses of each vn.

:- pred vn__build_uses(vnlvalset, ctrlmap, vn_tables, vn_tables).
:- mode vn__build_uses(in, in, di, uo) is det.

:- implementation.

vn__build_uses(Livevals, Ctrlmap, Vn_tables0, Vn_tables) :-
	vn__build_uses_from_ctrl(0, Ctrlmap, Vn_tables0, Vn_tables1),
	bintree_set__to_sorted_list(Livevals, Livelist),
	vn__build_uses_from_livevals(Livelist, Vn_tables1, Vn_tables).

:- pred vn__build_uses_from_ctrl(int, ctrlmap, vn_tables, vn_tables).
:- mode vn__build_uses_from_ctrl(in, in, di, uo) is det.

vn__build_uses_from_ctrl(Ctrl, Ctrlmap, Vn_tables0, Vn_tables) :-
	( map__search(Ctrlmap, Ctrl, VnInstr) ->
		(
			VnInstr = vn_call(_, _, _, _),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_call_closure(_, _, _),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_mkframe(_, _, _),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_modframe(_),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_label(_),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_goto(_),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_computed_goto(Vn, _),
			vn__record_use(Vn, src_ctrl(Ctrl),
				Vn_tables0, Vn_tables1)
		;
			VnInstr = vn_if_val(Vn, _),
			vn__record_use(Vn, src_ctrl(Ctrl),
				Vn_tables0, Vn_tables1)
		;
			VnInstr = vn_mark_hp(Vnlval),
			vn__vnlval_access_vns(Vnlval, Vns),
			vn__record_use_list(Vns, src_ctrl(Ctrl),
				Vn_tables0, Vn_tables1)
		;
			VnInstr = vn_restore_hp(Vn),
			vn__record_use(Vn, src_ctrl(Ctrl),
				Vn_tables0, Vn_tables1)
		;
			VnInstr = vn_incr_sp(_),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_decr_sp(_),
			Vn_tables1 = Vn_tables0
		),
		NextCtrl is Ctrl + 1,
		vn__build_uses_from_ctrl(NextCtrl, Ctrlmap,
			Vn_tables1, Vn_tables)
	;
		Vn_tables = Vn_tables0
	).

	% We have to record two kinds of uses. The first is the use of the
	% value number we want to assign to the vnlval. The second is the
	% value numbers needed to access the vnlval at all.

:- pred vn__build_uses_from_livevals(list(vnlval), vn_tables, vn_tables).
:- mode vn__build_uses_from_livevals(in, di, uo) is det.

vn__build_uses_from_livevals([], Vn_tables, Vn_tables).
vn__build_uses_from_livevals([Live | Liveslist], Vn_tables0, Vn_tables) :-
	vn__lookup_desired_value(Live, Vn, Vn_tables0),
	vn__record_use(Vn, src_liveval(Live), Vn_tables0, Vn_tables1),
	vn__vnlval_access_vns(Live, SubVns),
	vn__record_use_list(SubVns, src_access(Live),
		Vn_tables1, Vn_tables2),
	vn__build_uses_from_livevals(Liveslist, Vn_tables2, Vn_tables).

:- pred vn__record_use(vn, vn_src, vn_tables, vn_tables).
:- mode vn__record_use(in, in, di, uo) is det.

vn__record_use(Vn, Src, Vn_tables0, Vn_tables) :-
	vn__lookup_uses(Vn, OldUses, Vn_tables0),
	vn__add_new_use(Vn, Src, Vn_tables0, Vn_tables1),
	( OldUses = [] ->
		vn__lookup_defn(Vn, Vnrval, Vn_tables1),
		(
			Vnrval = vn_origlval(Vnlval),
			vn__vnlval_access_vns(Vnlval, SubVns),
			vn__record_use_list(SubVns, src_access(Vnlval),
				Vn_tables1, Vn_tables)
		;
			Vnrval = vn_mkword(_, SubVn),
			vn__record_use(SubVn, src_vn(Vn),
				Vn_tables1, Vn_tables)
		;
			Vnrval = vn_const(_),
			Vn_tables = Vn_tables1
		;
			Vnrval = vn_create(_, _, _),
			Vn_tables = Vn_tables1
		;
			Vnrval = vn_unop(_, SubVn),
			vn__record_use(SubVn, src_vn(Vn),
				Vn_tables1, Vn_tables)
		;
			Vnrval = vn_binop(_, SubVn1, SubVn2),
			vn__record_use(SubVn1, src_vn(Vn),
				Vn_tables1, Vn_tables2),
			vn__record_use(SubVn2, src_vn(Vn),
				Vn_tables2, Vn_tables)
		)
	;
		Vn_tables = Vn_tables1
	).

:- pred vn__record_use_list(list(vn), vn_src, vn_tables, vn_tables).
:- mode vn__record_use_list(in, in, di, uo) is det.

vn__record_use_list([], _Src, Vn_tables, Vn_tables).
vn__record_use_list([Vn | Vns], Src, Vn_tables0, Vn_tables) :-
	vn__record_use(Vn, Src, Vn_tables0, Vn_tables1),
	vn__record_use_list(Vns, Src, Vn_tables1, Vn_tables).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- pred vn__block_cost(list(instruction), int).
:- mode vn__block_cost(in, out) is det.

:- implementation.

vn__block_cost(Instrs, Cost) :-
	list__reverse(Instrs, Rev),
	vn__block_cost_2(Rev, 0, Assigns, 0, Ops, 0, StackRefs, 0, HeapRefs),
	AssignCost is Assigns * 1,
	OpsCost is Ops * 1,
	StackCost is StackRefs * 2,
	HeapCost is HeapRefs * 2,
	Cost1 is AssignCost + OpsCost,
	Cost2 is StackCost + HeapCost,
	Cost is Cost1 + Cost2.

:- pred vn__block_cost_2(list(instruction), int, int, int, int, int, int,
	int, int).
:- mode vn__block_cost_2(in, in, out, in, out, in, out, in, out) is det.

vn__block_cost_2([], A, A, O, O, S, S, H, H).
vn__block_cost_2([Uinstr - _Comment | Instrs],  A0, A, O0, O, S0, S, H0, H) :-
	(
		Uinstr = comment(_),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	;
		Uinstr = livevals(_),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	;
		Uinstr = block(_, _),
		error("block found in vn_block_cost")
	;
		Uinstr = assign(Lval, Rval),
		A1 is A0 + 1,
		vn__lval_cost(Lval, O0, O1, S0, S1, H0, H1),
		vn__rval_cost(Rval, O1, O2, S1, S2, H1, H2)
	;
		Uinstr = call(_, _, _, _),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	;
		Uinstr = call_closure(_, _, _),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	;
		Uinstr = mkframe(_, _, _),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	;
		Uinstr = modframe(_),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	;
		Uinstr = label(_),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	;
		Uinstr = goto(_),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	;
		Uinstr = computed_goto(Rval, _),
		A1 = A0,
		vn__rval_cost(Rval, O0, O2, S0, S2, H0, H2)
	;
		Uinstr = c_code(_),
		error("c_code found in vn_block_cost")
	;
		Uinstr = if_val(Rval, _),
		vn__rval_cost(Rval, O0, O1, S0, S1, H0, H1),
		% count the earlier instructions twice,
		% to favor code sequences that move code after ifs
		vn__block_cost_2(Instrs, A0, A1, O1, O2, S1, S2, H1, H2)
	;
		Uinstr = incr_hp(Lval, _, Rval),
		A1 is A0 + 2,
		vn__lval_cost(Lval, O0, O1, S0, S1, H0, H1),
		vn__lval_cost(Lval, O1, O2, S1, S2, H1, H2)
	;
		Uinstr = mark_hp(Lval),
		A1 = A0,
		vn__lval_cost(Lval, O0, O2, S0, S2, H0, H2)
	;
		Uinstr = restore_hp(Rval),
		A1 = A0,
		vn__rval_cost(Rval, O0, O2, S0, S2, H0, H2)
	;
		Uinstr = incr_sp(_),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	;
		Uinstr = decr_sp(_),
		A1 = A0,
		O2 = O0,
		S2 = S0,
		H2 = H0
	),
	vn__block_cost_2(Instrs, A1, A, O2, O, S2, S, H2, H).

:- pred vn__lval_cost(lval, int, int, int, int, int, int).
:- mode vn__lval_cost(in, in, out, in, out, in, out) is det.

vn__lval_cost(Lval, O0, O, S0, S, H0, H) :-
	(
		Lval = reg(_),
		O = O0,
		S = S0,
		H = H0
	;
		Lval = stackvar(_),
		O = O0,
		S is S0 + 1,
		H = H0
	;
		Lval = framevar(_),
		O = O0,
		S is S0 + 1,
		H = H0
	;
		Lval = succip,
		O = O0,
		S = S0,
		H = H0
	;
		Lval = maxfr,
		O = O0,
		S = S0,
		H = H0
	;
		Lval = curredoip,
		O = O0,
		S is S0 + 1,
		H = H0
	;
		Lval = hp,
		O = O0,
		S = S0,
		H = H0
	;
		Lval = sp,
		O = O0,
		S = S0,
		H = H0
	;
		Lval = field(_, Rval1, Rval2),
		H1 is H0 + 1,
		vn__rval_cost(Rval1, O0, O1, S0, S1, H1, H2),
		vn__rval_cost(Rval2, O1, O, S1, S, H2, H)
	;
		Lval = lvar(_),
		error("lvar found in lval_cost")
	;
		Lval = temp(_),
		O = O0,
		S = S0,
		H = H0
	).

:- pred vn__rval_cost(rval, int, int, int, int, int, int).
:- mode vn__rval_cost(in, in, out, in, out, in, out) is det.

vn__rval_cost(Rval, O0, O, S0, S, H0, H) :-
	(
		Rval = lval(Lval),
		vn__lval_cost(Lval, O0, O, S0, S, H0, H)
	;
		Rval = var(_),
		error("var found in rval_cost")
	;
		Rval = create(_, _, _),
		O = O0,
		S = S0,
		H = H0
	;
		Rval = mkword(_, Rval1),
		O1 is O0 + 1,
		vn__rval_cost(Rval1, O1, O, S0, S, H0, H)
	;
		Rval = const(_),
		O = O0,
		S = S0,
		H = H0
	;
		Rval = unop(_, Rval1),
		O1 is O0 + 1,
		vn__rval_cost(Rval1, O1, O, S0, S, H0, H)
	;
		Rval = binop(_, Rval1, Rval2),
		O1 is O0 + 1,
		vn__rval_cost(Rval1, O1, O2, S0, S1, H0, H1),
		vn__rval_cost(Rval2, O2, O, S1, S, H1, H)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
