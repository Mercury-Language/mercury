%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module exprn_aux.

:- interface.

:- import_module llds.
:- import_module list, std_util.

:- pred exprn_aux__rval_contains_lval(rval, lval).
:- mode exprn_aux__rval_contains_lval(in, in) is semidet.

:- pred exprn_aux__rval_contains_rval(rval, rval).
:- mode exprn_aux__rval_contains_rval(in, in) is semidet.

:- pred exprn_aux__substitute_lval_in_rval(lval, lval, rval, rval).
:- mode exprn_aux__substitute_lval_in_rval(in, in, in, out) is det.

:- pred exprn_aux__substitute_rval_in_rval(rval, rval, rval, rval).
:- mode exprn_aux__substitute_rval_in_rval(in, in, in, out) is det.

:- pred exprn_aux__vars_in_rval(rval, list(var)).
:- mode exprn_aux__vars_in_rval(in, out) is det.

:- pred exprn_aux__substitute_vars_in_rval(assoc_list(var, rval), rval, rval).
:- mode exprn_aux__substitute_vars_in_rval(in, in, out) is det.

:- pred exprn_aux__simplify_rval(rval, rval).
:- mode exprn_aux__simplify_rval(in, out) is det.

:- pred exprn_aux__rval_list_code_addrs(list(rval), list(code_addr)).
:- mode exprn_aux__rval_list_code_addrs(in, out) is det.

:- pred exprn_aux__lval_list_code_addrs(list(lval), list(code_addr)).
:- mode exprn_aux__lval_list_code_addrs(in, out) is det.

:- pred exprn_aux__rval_code_addrs(rval, list(code_addr)).
:- mode exprn_aux__rval_code_addrs(in, out) is det.

:- pred exprn_aux__lval_code_addrs(lval, list(code_addr)).
:- mode exprn_aux__lval_code_addrs(in, out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.
:- import_module require.

exprn_aux__rval_contains_lval(lval(Lval0), Lval) :-
	exprn_aux__lval_contains_lval(Lval0, Lval).
exprn_aux__rval_contains_lval(create(_, Rvals, _), Lval) :-
	exprn_aux__args_contain_lval(Rvals, Lval).
exprn_aux__rval_contains_lval(mkword(_, Rval), Lval) :-
	exprn_aux__rval_contains_lval(Rval, Lval).
exprn_aux__rval_contains_lval(unop(_, Rval), Lval) :-
	exprn_aux__rval_contains_lval(Rval, Lval).
exprn_aux__rval_contains_lval(binop(_, Rval0, Rval1), Lval) :-
	(
		exprn_aux__rval_contains_lval(Rval0, Lval)
	;
		exprn_aux__rval_contains_lval(Rval1, Lval)
	).

:- pred exprn_aux__lval_contains_lval(lval, lval).
:- mode exprn_aux__lval_contains_lval(in, in) is semidet.

exprn_aux__lval_contains_lval(Lval0, Lval) :-
	(
		Lval0 = Lval
	->
		true
	;
		Lval0 = field(_, Rval0, Rval1)
	->
		(
			exprn_aux__rval_contains_lval(Rval0, Lval)
		;
			exprn_aux__rval_contains_lval(Rval1, Lval)
		)
	;
		Lval0 = lvar(_Var)
	->
		error("exprn_aux__lval_contains_lval: var! I can't tell")
	;
		fail
	).

:- pred exprn_aux__args_contain_lval(list(maybe(rval)), lval).
:- mode exprn_aux__args_contain_lval(in, in) is semidet.

exprn_aux__args_contain_lval([M|Ms], Lval) :-
	(
		M = yes(Rval),
		exprn_aux__rval_contains_lval(Rval, Lval)
	->
		true
	;
		exprn_aux__args_contain_lval(Ms, Lval)
	).

%------------------------------------------------------------------------------%

exprn_aux__rval_contains_rval(Rval0, Rval) :-
	(
		Rval0 = Rval
	->
		true
	;
		(
			Rval0 = lval(Lval),
			exprn_aux__lval_contains_rval(Lval, Rval)
		;
			Rval0 = create(_, Rvals, _),
			exprn_aux__args_contain_rval(Rvals, Rval)
		;
			Rval0 = mkword(_, Rval1),
			exprn_aux__rval_contains_rval(Rval1, Rval)
		;
			Rval0 = unop(_, Rval1),
			exprn_aux__rval_contains_rval(Rval1, Rval)
		;
			Rval0 = binop(_, Rval1, Rval2),
			(
				exprn_aux__rval_contains_rval(Rval1, Rval)
			;
				exprn_aux__rval_contains_rval(Rval2, Rval)
			)
		)
	).

:- pred exprn_aux__lval_contains_rval(lval, rval).
:- mode exprn_aux__lval_contains_rval(in, in) is semidet.

exprn_aux__lval_contains_rval(field(_, Rval0, Rval1), Rval) :-
	(
		exprn_aux__rval_contains_rval(Rval0, Rval)
	;
		exprn_aux__rval_contains_rval(Rval1, Rval)
	).

:- pred exprn_aux__args_contain_rval(list(maybe(rval)), rval).
:- mode exprn_aux__args_contain_rval(in, in) is semidet.

exprn_aux__args_contain_rval([M|Ms], Rval) :-
	(
		M = yes(Rval0),
		exprn_aux__rval_contains_rval(Rval0, Rval)
	->
		true
	;
		exprn_aux__args_contain_rval(Ms, Rval)
	).

%------------------------------------------------------------------------------%

exprn_aux__vars_in_rval(lval(Lval), Vars) :-
	exprn_aux__vars_in_lval(Lval, Vars).
exprn_aux__vars_in_rval(var(Var), [Var]).
exprn_aux__vars_in_rval(create(_, Rvals, _), Vars) :-
	exprn_aux__vars_in_args(Rvals, Vars).
exprn_aux__vars_in_rval(mkword(_, Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_rval(const(_Conts), []).
exprn_aux__vars_in_rval(unop(_, Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_rval(binop(_, Rval0, Rval1), Vars) :-
	exprn_aux__vars_in_rval(Rval0, Vars0),
	exprn_aux__vars_in_rval(Rval1, Vars1),
	list__append(Vars0, Vars1, Vars).

:- pred exprn_aux__vars_in_lval(lval, list(var)).
:- mode exprn_aux__vars_in_lval(in, out) is det.

exprn_aux__vars_in_lval(Lval, Vars) :-
	(
		Lval = lvar(Var)
	->
		Vars = [Var]
	;
		Lval = field(_, Rval0, Rval1)
	->
		exprn_aux__vars_in_rval(Rval0, Vars0),
		exprn_aux__vars_in_rval(Rval1, Vars1),
		list__append(Vars0, Vars1, Vars)
	;
		Vars = []
	).

:- pred exprn_aux__vars_in_args(list(maybe(rval)), list(var)).
:- mode exprn_aux__vars_in_args(in, out) is det.

exprn_aux__vars_in_args([], []).
exprn_aux__vars_in_args([M|Ms], Vars) :-
	exprn_aux__vars_in_args(Ms, Vars0),
	(
		M = yes(Rval)
	->
		exprn_aux__vars_in_rval(Rval, Vars1),
		list__append(Vars1, Vars0, Vars)
	;
		Vars = Vars0
	).

%------------------------------------------------------------------------------%

exprn_aux__substitute_lval_in_rval(OldLval, NewLval, Rval0, Rval) :-
	(
		Rval0 = lval(Lval0),
		exprn_aux__substitute_lval_in_lval(OldLval, NewLval,
								Lval0, Lval),
		Rval = lval(Lval)
	;
		Rval0 = var(_Var),
		Rval = Rval0
	;
		Rval0 = create(Tag, Rvals0, Num),
		exprn_aux__substitute_lval_in_args(OldLval, NewLval,
						Rvals0, Rvals),
		Rval = create(Tag, Rvals, Num)
	;
		Rval0 = mkword(Tag, Rval1),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval, Rval1,
			Rval2),
		Rval = mkword(Tag, Rval2)
	;
		Rval0 = const(_Const),
		Rval = Rval0
	;
		Rval0 = unop(Unop, Rval1),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval, Rval1,
			Rval2),
		Rval = unop(Unop, Rval2)
	;
		Rval0 = binop(Binop, Rval1, Rval2),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval, Rval1,
			Rval3),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval, Rval2,
			Rval4),
		Rval = binop(Binop, Rval3, Rval4)
	).

:- pred exprn_aux__substitute_lval_in_lval(lval, lval, lval, lval).
:- mode exprn_aux__substitute_lval_in_lval(in, in, in, out) is det.

exprn_aux__substitute_lval_in_lval(OldLval, NewLval, Lval0, Lval) :-
	(
		Lval0 = OldLval
	->
		Lval = NewLval
	;
		Lval0 = field(Tag, Rval0, Rval1)
	->
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval, Rval0,
			Rval2),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval, Rval1,
			Rval3),
		Lval = field(Tag, Rval2, Rval3)
	;
		Lval = Lval0
	).

:- pred exprn_aux__substitute_lval_in_args(lval, lval,
				list(maybe(rval)), list(maybe(rval))).
:- mode exprn_aux__substitute_lval_in_args(in, in, in, out) is det.

exprn_aux__substitute_lval_in_args(_OldLval, _NewLval, [], []).
exprn_aux__substitute_lval_in_args(OldLval, NewLval, [M0|Ms0], [M|Ms]) :-
	(
		M0 = yes(Rval0)
	->
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval, Rval0,
			Rval),
		M = yes(Rval)
	;
		M = M0
	),
	exprn_aux__substitute_lval_in_args(OldLval, NewLval, Ms0, Ms).

exprn_aux__substitute_rval_in_rval(OldRval, NewRval, Rval0, Rval) :-
	(
		Rval0 = OldRval
	->
		Rval = NewRval
	;
		(
			Rval0 = lval(Lval0),
			exprn_aux__substitute_rval_in_lval(OldRval, NewRval,
						Lval0, Lval),
			Rval = lval(Lval)
		;
			Rval0 = var(_),
			Rval = Rval0
		;
			Rval0 = create(Tag, Rvals0, Num),
			exprn_aux__substitute_rval_in_args(OldRval, NewRval,
							Rvals0, Rvals),
			Rval = create(Tag, Rvals, Num)
		;
			Rval0 = mkword(Tag, Rval1),
			exprn_aux__substitute_rval_in_rval(OldRval, NewRval, Rval1,
				Rval2),
			Rval = mkword(Tag, Rval2)
		;
			Rval0 = const(_Const),
			Rval = Rval0
		;
			Rval0 = unop(Unop, Rval1),
			exprn_aux__substitute_rval_in_rval(OldRval, NewRval, Rval1,
				Rval2),
			Rval = unop(Unop, Rval2)
		;
			Rval0 = binop(Binop, Rval1, Rval2),
			exprn_aux__substitute_rval_in_rval(OldRval, NewRval, Rval1,
				Rval3),
			exprn_aux__substitute_rval_in_rval(OldRval, NewRval, Rval2,
				Rval4),
			Rval = binop(Binop, Rval3, Rval4)
		)
	).

:- pred exprn_aux__substitute_rval_in_lval(rval, rval, lval, lval).
:- mode exprn_aux__substitute_rval_in_lval(in, in, in, out) is det.

exprn_aux__substitute_rval_in_lval(OldRval, NewRval, Lval0, Lval) :-
	(
		Lval0 = field(Tag, Rval0, Rval1)
	->
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval, Rval0,
			Rval2),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval, Rval1,
			Rval3),
		Lval = field(Tag, Rval2, Rval3)
	;
		Lval = Lval0
	).

:- pred exprn_aux__substitute_rval_in_args(rval, rval,
				list(maybe(rval)), list(maybe(rval))).
:- mode exprn_aux__substitute_rval_in_args(in, in, in, out) is det.

exprn_aux__substitute_rval_in_args(_OldRval, _NewRval, [], []).
exprn_aux__substitute_rval_in_args(OldRval, NewRval, [M0|Ms0], [M|Ms]) :-
	(
		M0 = yes(Rval0)
	->
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval, Rval0,
			Rval),
		M = yes(Rval)
	;
		M = M0
	),
	exprn_aux__substitute_rval_in_args(OldRval, NewRval, Ms0, Ms).

%------------------------------------------------------------------------------%

exprn_aux__substitute_vars_in_rval([], Rval, Rval).
exprn_aux__substitute_vars_in_rval([Var - Sub|Rest], Rval0, Rval) :-
	exprn_aux__substitute_rval_in_rval(var(Var), Sub, Rval0, Rval1),
	exprn_aux__substitute_vars_in_rval(Rest, Rval1, Rval).

%---------------------------------------------------------------------------%

exprn_aux__simplify_rval(Rval0, Rval) :-
	(
		exprn_aux__simplify_rval_2(Rval0, Rval1)
	->
		exprn_aux__simplify_rval(Rval1, Rval)
	;
		Rval = Rval0
	).

:- pred exprn_aux__simplify_rval_2(rval, rval).
:- mode exprn_aux__simplify_rval_2(in, out) is semidet.

exprn_aux__simplify_rval_2(Rval0, Rval) :-
	(
		Rval0 = lval(field(Tag, create(Tag, Args, _), Field)),
		Field = const(int_const(FieldNum))
	->
		list__index0_det(Args, FieldNum, yes(Rval))
	;
		Rval0 = lval(field(Tag, Rval1, Num)),
		exprn_aux__simplify_rval_2(Rval1, Rval2)
	->
		Rval = lval(field(Tag, Rval2, Num))
	;
		Rval0 = create(Tag, Args0, CNum),
		exprn_aux__simplify_args(Args0, Args),
		Args \= Args0
	->
		Rval = create(Tag, Args, CNum)
	;
		Rval0 = unop(UOp, Rval1),
		exprn_aux__simplify_rval_2(Rval1, Rval2)
	->
		Rval = unop(UOp, Rval2)
	;
		Rval0 = binop(BOp, Rval1, Rval2),
		exprn_aux__simplify_rval_2(Rval1, Rval3)
	->
		Rval = binop(BOp, Rval3, Rval2)
	;
		Rval0 = binop(BOp, Rval1, Rval2),
		exprn_aux__simplify_rval_2(Rval2, Rval3)
	->
		Rval = binop(BOp, Rval1, Rval3)
	;
		fail
	).

:- pred exprn_aux__simplify_args(list(maybe(rval)), list(maybe(rval))).
:- mode exprn_aux__simplify_args(in, out) is det.

exprn_aux__simplify_args([], []).
exprn_aux__simplify_args([MR0|Ms0], [MR|Ms]) :-
	exprn_aux__simplify_args(Ms0, Ms),
	(
		MR0 = yes(Rval0),
		exprn_aux__simplify_rval_2(Rval0, Rval)
	->
		MR = yes(Rval)
	;
		MR = MR0
	).

%-----------------------------------------------------------------------------%

	% give an lval, return a list of the code_addrs
	% that are reference by that lval

exprn_aux__rval_code_addrs(lval(Lval), CodeAddrs) :-
	exprn_aux__lval_code_addrs(Lval, CodeAddrs).
exprn_aux__rval_code_addrs(var(_), []).
exprn_aux__rval_code_addrs(create(_, MaybeRvals, _), CodeAddrs) :-
	exprn_aux__maybe_rval_list_code_addrs(MaybeRvals, CodeAddrs).
exprn_aux__rval_code_addrs(mkword(_Tag, Rval), CodeAddrs) :-
	exprn_aux__rval_code_addrs(Rval, CodeAddrs).
exprn_aux__rval_code_addrs(const(Const), CodeAddrs) :-
	( Const = address_const(CodeAddress) ->
		CodeAddrs = [CodeAddress]
	;
		CodeAddrs = []
	).
exprn_aux__rval_code_addrs(unop(_Op, Rval), CodeAddrs) :-
	exprn_aux__rval_code_addrs(Rval, CodeAddrs).
exprn_aux__rval_code_addrs(binop(_BinOp, Rval1, Rval2), CodeAddrs) :-
	exprn_aux__rval_code_addrs(Rval1, CodeAddrs1),
	exprn_aux__rval_code_addrs(Rval2, CodeAddrs2),
	list__append(CodeAddrs1, CodeAddrs2, CodeAddrs).

	% give an lval, return a list of the code_addrs
	% that are reference by that lval

exprn_aux__lval_code_addrs(reg(_Int), []).
exprn_aux__lval_code_addrs(stackvar(_Int), []).
exprn_aux__lval_code_addrs(framevar(_Int), []).
exprn_aux__lval_code_addrs(succip, []).
exprn_aux__lval_code_addrs(maxfr, []).
exprn_aux__lval_code_addrs(curfr, []).
exprn_aux__lval_code_addrs(redoip(Rval), CodeAddrs) :-
	exprn_aux__rval_code_addrs(Rval, CodeAddrs).
exprn_aux__lval_code_addrs(hp, []).
exprn_aux__lval_code_addrs(sp, []).
exprn_aux__lval_code_addrs(field(_Tag, Rval1, Rval2), CodeAddrs) :-
	exprn_aux__rval_code_addrs(Rval1, CodeAddrs1),
	exprn_aux__rval_code_addrs(Rval2, CodeAddrs2),
	list__append(CodeAddrs1, CodeAddrs2, CodeAddrs).
exprn_aux__lval_code_addrs(lvar(_Var), []).
exprn_aux__lval_code_addrs(temp(_Int), []).

	% give a list of rval, return a list of the code_addrs
	% that are reference by that list

exprn_aux__rval_list_code_addrs([], []).
exprn_aux__rval_list_code_addrs([Rval | Rvals], CodeAddrs) :-
	exprn_aux__rval_code_addrs(Rval, CodeAddrs0),
	list__append(CodeAddrs0, CodeAddrs1, CodeAddrs),
	exprn_aux__rval_list_code_addrs(Rvals, CodeAddrs1).

exprn_aux__lval_list_code_addrs([], []).
exprn_aux__lval_list_code_addrs([Lval | Lvals], CodeAddrs) :-
	exprn_aux__lval_code_addrs(Lval, CodeAddrs0),
	list__append(CodeAddrs0, CodeAddrs1, CodeAddrs),
	exprn_aux__lval_list_code_addrs(Lvals, CodeAddrs1).

	% give a list of maybe(rval), return a list of the code_addrs
	% that are reference by that list

:- pred exprn_aux__maybe_rval_list_code_addrs(list(maybe(rval)),
						list(code_addr)).
:- mode exprn_aux__maybe_rval_list_code_addrs(in, out) is det.

exprn_aux__maybe_rval_list_code_addrs([], []).
exprn_aux__maybe_rval_list_code_addrs([MaybeRval | MaybeRvals], CodeAddrs) :-
	( MaybeRval = yes(Rval) ->
		exprn_aux__rval_code_addrs(Rval, CodeAddrs0),
		list__append(CodeAddrs0, CodeAddrs1, CodeAddrs),
		exprn_aux__maybe_rval_list_code_addrs(MaybeRvals, CodeAddrs1)
	;
		exprn_aux__maybe_rval_list_code_addrs(MaybeRvals, CodeAddrs)
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
