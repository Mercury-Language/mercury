%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%------------------------------------------------------------------------------%

:- module exprn_aux.

:- interface.

:- import_module llds.
:- import_module list, std_util, bool, assoc_list.

:- type exprn_opts	--->	nlg_asm_sgt(bool, bool, bool).

:- pred exprn_aux__init_exprn_opts(option_table, exprn_opts).
:- mode exprn_aux__init_exprn_opts(in, out) is det.

:- pred exprn_aux__const_is_constant(rval_const, exprn_opts, bool).
:- mode exprn_aux__const_is_constant(in, in, out) is det.

:- pred exprn_aux__rval_contains_lval(rval, lval).
:- mode exprn_aux__rval_contains_lval(in, in) is semidet.

:- pred exprn_aux__rval_contains_rval(rval, rval).
:- mode exprn_aux__rval_contains_rval(in, in) is semidet.

:- pred exprn_aux__substitute_lval_in_rval(lval, lval, rval, rval).
:- mode exprn_aux__substitute_lval_in_rval(in, in, in, out) is det.

:- pred exprn_aux__substitute_rval_in_rval(rval, rval, rval, rval).
:- mode exprn_aux__substitute_rval_in_rval(in, in, in, out) is det.

:- pred exprn_aux__substitute_vars_in_rval(assoc_list(var, rval), rval, rval).
:- mode exprn_aux__substitute_vars_in_rval(in, in, out) is det.

:- pred exprn_aux__substitute_rvals_in_rval(assoc_list(rval, rval), rval, rval).
:- mode exprn_aux__substitute_rvals_in_rval(in, in, out) is det.

:- pred exprn_aux__vars_in_rval(rval, list(var)).
:- mode exprn_aux__vars_in_rval(in, out) is det.

:- pred exprn_aux__simplify_rval(rval, rval).
:- mode exprn_aux__simplify_rval(in, out) is det.

	% the following predicates take an lval/rval (list)
	% and return a list of the code_addrs that it references.

:- pred exprn_aux__rval_list_code_addrs(list(rval), list(code_addr)).
:- mode exprn_aux__rval_list_code_addrs(in, out) is det.

:- pred exprn_aux__lval_list_code_addrs(list(lval), list(code_addr)).
:- mode exprn_aux__lval_list_code_addrs(in, out) is det.

:- pred exprn_aux__rval_code_addrs(rval, list(code_addr)).
:- mode exprn_aux__rval_code_addrs(in, out) is det.

:- pred exprn_aux__lval_code_addrs(lval, list(code_addr)).
:- mode exprn_aux__lval_code_addrs(in, out) is det.

:- pred exprn_aux__maybe_rval_list_code_addrs(list(maybe(rval)),
						list(code_addr)).
:- mode exprn_aux__maybe_rval_list_code_addrs(in, out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.
:- import_module int, require, getopt, options.

exprn_aux__init_exprn_opts(Options, ExprnOpts) :-
	getopt__lookup_bool_option(Options, gcc_non_local_gotos, NLG),
	getopt__lookup_bool_option(Options, asm_labels, ASM),
	getopt__lookup_bool_option(Options, static_ground_terms, SGT),
	ExprnOpts = nlg_asm_sgt(NLG, ASM, SGT).

	% Floating point values cannot be considered constants because
	% they must be stored on the heap.

exprn_aux__const_is_constant(true, _, yes).
exprn_aux__const_is_constant(false, _, yes).
exprn_aux__const_is_constant(int_const(_), _, yes).
exprn_aux__const_is_constant(float_const(_), _, no).
exprn_aux__const_is_constant(string_const(_), _, yes).
exprn_aux__const_is_constant(address_const(CodeAddress), ExprnOpts, IsConst) :-
	exprn_aux__addr_is_constant(CodeAddress, ExprnOpts, IsConst).

:- pred exprn_aux__addr_is_constant(code_addr, exprn_opts, bool).
:- mode exprn_aux__addr_is_constant(in, in, out) is det.

exprn_aux__addr_is_constant(succip, _, no).
exprn_aux__addr_is_constant(do_redo, _, no).
exprn_aux__addr_is_constant(do_fail, _, no).
exprn_aux__addr_is_constant(do_succeed(_), _, no).
exprn_aux__addr_is_constant(label(_), _, yes).
exprn_aux__addr_is_constant(imported(_), ExprnOpts, IsConst) :-
	ExprnOpts = nlg_asm_sgt(NonLocalGotos, AsmLabels, _SGT),
	(
		(
			NonLocalGotos = no
		;
			AsmLabels = yes
		)
	->
		IsConst = yes
	;
		IsConst = no
	).

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

exprn_aux__args_contain_lval([M | Ms], Lval) :-
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

exprn_aux__args_contain_rval([M | Ms], Rval) :-
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
exprn_aux__vars_in_args([M | Ms], Vars) :-
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
exprn_aux__substitute_lval_in_args(OldLval, NewLval, [M0 | Ms0], [M | Ms]) :-
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
exprn_aux__substitute_rval_in_args(OldRval, NewRval, [M0 | Ms0], [M | Ms]) :-
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
exprn_aux__substitute_vars_in_rval([Var - Sub | Rest], Rval0, Rval) :-
	exprn_aux__substitute_rval_in_rval(var(Var), Sub, Rval0, Rval1),
	exprn_aux__substitute_vars_in_rval(Rest, Rval1, Rval).

% When we substitute a one set of rvals for another, we face the problem
% that the substitution may not be idempotent. We finesse this problem by
% substituting unique new rvals for the original rvals, and then substituting
% the replacement rvals for these unique rvals. We guarantee the uniqueness
% of these rvals by using framevars with negative numbers for them.

exprn_aux__substitute_rvals_in_rval(RvalPairs, Rval0, Rval) :-
	exprn_aux__substitute_rvals_in_rval_1(RvalPairs, 0,
		RvalUniqPairs, UniqRvalPairs),
	exprn_aux__substitute_rvals_in_rval_2(RvalUniqPairs, Rval0, Rval1),
	exprn_aux__substitute_rvals_in_rval_2(UniqRvalPairs, Rval1, Rval).

:- pred exprn_aux__substitute_rvals_in_rval_1(assoc_list(rval, rval), int,
	assoc_list(rval, rval), assoc_list(rval, rval)).
:- mode exprn_aux__substitute_rvals_in_rval_1(in, in, out, out) is det.

exprn_aux__substitute_rvals_in_rval_1([], _, [], []).
exprn_aux__substitute_rvals_in_rval_1([Rval1 - Rval2 | RvalPairList], N0,
		[Rval1 - Uniq | RvalUniqList], [Uniq - Rval2 | UniqRvalList]) :-
	N1 is N0 - 1,
	Uniq = lval(framevar(N1)),
	exprn_aux__substitute_rvals_in_rval_1(RvalPairList, N1,
		RvalUniqList, UniqRvalList).

:- pred exprn_aux__substitute_rvals_in_rval_2(assoc_list(rval, rval),
	rval, rval).
:- mode exprn_aux__substitute_rvals_in_rval_2(in, in, out) is det.

exprn_aux__substitute_rvals_in_rval_2([], Rval, Rval).
exprn_aux__substitute_rvals_in_rval_2([Left - Right | Rest], Rval0, Rval2) :-
	exprn_aux__substitute_rval_in_rval(Left, Right, Rval0, Rval1),
	exprn_aux__substitute_rvals_in_rval_2(Rest, Rval1, Rval2).

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
exprn_aux__simplify_args([MR0 | Ms0], [MR | Ms]) :-
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
exprn_aux__lval_code_addrs(prevfr(Rval), CodeAddrs) :-
	exprn_aux__rval_code_addrs(Rval, CodeAddrs).
exprn_aux__lval_code_addrs(succfr(Rval), CodeAddrs) :-
	exprn_aux__rval_code_addrs(Rval, CodeAddrs).
exprn_aux__lval_code_addrs(redoip(Rval), CodeAddrs) :-
	exprn_aux__rval_code_addrs(Rval, CodeAddrs).
exprn_aux__lval_code_addrs(succip(Rval), CodeAddrs) :-
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
