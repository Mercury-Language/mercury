%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%------------------------------------------------------------------------------%

:- module exprn_aux.

:- interface.

:- import_module llds, options.
:- import_module list, std_util, bool, assoc_list, term.

:- type exprn_opts
	--->	nlg_asm_sgt_ubf(
			bool,	% --use-non-local-gotos
			bool,	% --use-asm-labels
			bool,	% --static-ground-terms
			bool	% --unboxed-float
		).

:- pred exprn_aux__init_exprn_opts(option_table, exprn_opts).
:- mode exprn_aux__init_exprn_opts(in, out) is det.

	% Determine whether an rval_const can be used as the initializer
	% of a C static constant.
:- pred exprn_aux__const_is_constant(rval_const, exprn_opts, bool).
:- mode exprn_aux__const_is_constant(in, in, out) is det.

	% exprn_aux__imported_is_constant(NonLocalGotos, AsmLabels, IsConst)
	% figures out whether an imported label address is a constant.
	% This depends on how we treat labels.

:- pred exprn_aux__imported_is_constant(bool, bool, bool).
:- mode exprn_aux__imported_is_constant(in, in, out) is det.

:- pred exprn_aux__rval_contains_lval(rval, lval).
:- mode exprn_aux__rval_contains_lval(in, in) is semidet.

:- pred exprn_aux__rval_contains_rval(rval, rval).
:- mode exprn_aux__rval_contains_rval(in, in) is semidet.
:- mode exprn_aux__rval_contains_rval(in, out) is multidet.

:- pred exprn_aux__args_contain_rval(list(maybe(rval)), rval).
:- mode exprn_aux__args_contain_rval(in, in) is semidet.
:- mode exprn_aux__args_contain_rval(in, out) is nondet.

:- pred exprn_aux__substitute_lval_in_rval(lval, lval, rval, rval).
:- mode exprn_aux__substitute_lval_in_rval(in, in, in, out) is det.

:- pred exprn_aux__substitute_rval_in_rval(rval, rval, rval, rval).
:- mode exprn_aux__substitute_rval_in_rval(in, in, in, out) is det.

:- pred exprn_aux__substitute_vars_in_rval(assoc_list(var, rval), rval, rval).
:- mode exprn_aux__substitute_vars_in_rval(in, in, out) is det.

:- pred exprn_aux__substitute_rvals_in_rval(assoc_list(rval, rval), rval, rval).
:- mode exprn_aux__substitute_rvals_in_rval(in, in, out) is det.

:- pred exprn_aux__vars_in_lval(lval, list(var)).
:- mode exprn_aux__vars_in_lval(in, out) is det.

:- pred exprn_aux__vars_in_rval(rval, list(var)).
:- mode exprn_aux__vars_in_rval(in, out) is det.

:- pred exprn_aux__simplify_rval(rval, rval).
:- mode exprn_aux__simplify_rval(in, out) is det.

	% the following predicates take an lval/rval (list)
	% and return a list of the code and data addresses that it references.

:- pred exprn_aux__rval_list_addrs(list(rval),
	list(code_addr), list(data_addr)).
:- mode exprn_aux__rval_list_addrs(in, out, out) is det.

:- pred exprn_aux__lval_list_addrs(list(lval),
	list(code_addr), list(data_addr)).
:- mode exprn_aux__lval_list_addrs(in, out, out) is det.

:- pred exprn_aux__rval_addrs(rval, list(code_addr), list(data_addr)).
:- mode exprn_aux__rval_addrs(in, out, out) is det.

:- pred exprn_aux__lval_addrs(lval, list(code_addr), list(data_addr)).
:- mode exprn_aux__lval_addrs(in, out, out) is det.

:- pred exprn_aux__maybe_rval_list_addrs(list(maybe(rval)),
	list(code_addr), list(data_addr)).
:- mode exprn_aux__maybe_rval_list_addrs(in, out, out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.
:- import_module int, require, getopt, options.

exprn_aux__init_exprn_opts(Options, ExprnOpts) :-
	getopt__lookup_bool_option(Options, gcc_non_local_gotos, NLG),
	getopt__lookup_bool_option(Options, asm_labels, ASM),
	getopt__lookup_bool_option(Options, static_ground_terms, SGT),
	getopt__lookup_bool_option(Options, unboxed_float, UBF),
	ExprnOpts = nlg_asm_sgt_ubf(NLG, ASM, SGT, UBF).

% Determine whether a const (well, what _we_ consider to be a const)
% is constant as far as the C compiler is concerned -- specifically,
% determine whether it can be used as the initializer of a C static
% constant.

exprn_aux__const_is_constant(true, _, yes).
exprn_aux__const_is_constant(false, _, yes).
exprn_aux__const_is_constant(int_const(_), _, yes).
exprn_aux__const_is_constant(float_const(_), ExprnOpts, IsConst) :-
	ExprnOpts = nlg_asm_sgt_ubf(_NLG, _ASM, StaticGroundTerms,
					UnboxedFloat),
	( UnboxedFloat = yes ->
		%
		% If we're using unboxed (single-precision) floats,
		% floating point values are always constants
		%
		IsConst = yes
	;
		%
		% If we're using boxed floats, then we can generate a static
		% constant variable to hold a float constant, and gcc
		% doesn't mind us converting from its address to word
		% in a static initializer.  However, we only do this if
		% --static-ground-terms is enabled.
		%
		IsConst = StaticGroundTerms
	).
exprn_aux__const_is_constant(string_const(_), _, yes).
exprn_aux__const_is_constant(code_addr_const(CodeAddr), ExprnOpts, IsConst) :-
	exprn_aux__addr_is_constant(CodeAddr, ExprnOpts, IsConst).
exprn_aux__const_is_constant(data_addr_const(_), _, yes).

:- pred exprn_aux__addr_is_constant(code_addr, exprn_opts, bool).
:- mode exprn_aux__addr_is_constant(in, in, out) is det.

exprn_aux__addr_is_constant(label(Label), ExprnOpts, IsConst) :-
	ExprnOpts = nlg_asm_sgt_ubf(NonLocalGotos, AsmLabels, _SGT, _UBF),
	exprn_aux__label_is_constant(Label, NonLocalGotos, AsmLabels, IsConst).
exprn_aux__addr_is_constant(imported(_), ExprnOpts, IsConst) :-
	ExprnOpts = nlg_asm_sgt_ubf(NonLocalGotos, AsmLabels, _SGT, _UBF),
	exprn_aux__imported_is_constant(NonLocalGotos, AsmLabels, IsConst).
exprn_aux__addr_is_constant(succip, _, no).
exprn_aux__addr_is_constant(do_succeed(_), _, no).
exprn_aux__addr_is_constant(do_redo, _, no).
exprn_aux__addr_is_constant(do_fail, _, no).
exprn_aux__addr_is_constant(do_det_closure, _, no).
exprn_aux__addr_is_constant(do_semidet_closure, _, no).
exprn_aux__addr_is_constant(do_nondet_closure, _, no).
exprn_aux__addr_is_constant(do_not_reached, _, no).

:- pred exprn_aux__label_is_constant(label, bool, bool, bool).
:- mode exprn_aux__label_is_constant(in, in, in, out) is det.

exprn_aux__label_is_constant(exported(_), NonLocalGotos, AsmLabels, IsConst) :-
	exprn_aux__imported_is_constant(NonLocalGotos, AsmLabels, IsConst).
exprn_aux__label_is_constant(local(_), NonLocalGotos, AsmLabels, IsConst) :-
	exprn_aux__imported_is_constant(NonLocalGotos, AsmLabels, IsConst).
exprn_aux__label_is_constant(c_local(_), _NonLocalGotos, _AsmLabels, yes).
exprn_aux__label_is_constant(local(_, _), _NonLocalGotos, _AsmLabels, yes).

	% The logic of this function and how it is used in globals.m to
	% select the default type_info method must agree with the code in
	% runtime/typeinfo.h.

exprn_aux__imported_is_constant(NonLocalGotos, AsmLabels, IsConst) :-
	(
		NonLocalGotos = yes,
		AsmLabels = no
	->
		%
		% with non-local gotos but no asm labels, jumps to code
		% addresses in different c_modules must be done via global
		% variables; the value of these global variables is not
		% constant (i.e. not computable at load time), since they
		% can't be initialized until we call init_modules().
		%
		IsConst = no
	;
		IsConst = yes
	).

%------------------------------------------------------------------------------%

exprn_aux__rval_contains_lval(lval(Lval0), Lval) :-
	exprn_aux__lval_contains_lval(Lval0, Lval).
exprn_aux__rval_contains_lval(create(_, Rvals, _, _), Lval) :-
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
	;
		(
			Rval0 = lval(Lval),
			exprn_aux__lval_contains_rval(Lval, Rval)
		;
			Rval0 = create(_, Rvals, _, _),
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
:- mode exprn_aux__lval_contains_rval(in, out) is nondet.

exprn_aux__lval_contains_rval(field(_, Rval0, Rval1), Rval) :-
	(
		exprn_aux__rval_contains_rval(Rval0, Rval)
	;
		exprn_aux__rval_contains_rval(Rval1, Rval)
	).

exprn_aux__args_contain_rval([M | Ms], Rval) :-
	(
		M = yes(Rval0),
		exprn_aux__rval_contains_rval(Rval0, Rval)
	;
		exprn_aux__args_contain_rval(Ms, Rval)
	).

%------------------------------------------------------------------------------%

exprn_aux__vars_in_rval(lval(Lval), Vars) :-
	exprn_aux__vars_in_lval(Lval, Vars).
exprn_aux__vars_in_rval(var(Var), [Var]).
exprn_aux__vars_in_rval(create(_, Rvals, _, _), Vars) :-
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
exprn_aux__vars_in_rval(mem_addr(MemRef), Vars) :-
	exprn_aux__vars_in_mem_ref(MemRef, Vars).

exprn_aux__vars_in_lval(reg(_, _), []).
exprn_aux__vars_in_lval(temp(_, _), []).
exprn_aux__vars_in_lval(succip, []).
exprn_aux__vars_in_lval(maxfr, []).
exprn_aux__vars_in_lval(curfr, []).
exprn_aux__vars_in_lval(hp, []).
exprn_aux__vars_in_lval(sp, []).
exprn_aux__vars_in_lval(stackvar(_), []).
exprn_aux__vars_in_lval(framevar(_), []).
exprn_aux__vars_in_lval(succip(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(redoip(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(succfr(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(prevfr(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(field(_, Rval0, Rval1), Vars) :-
	exprn_aux__vars_in_rval(Rval0, Vars0),
	exprn_aux__vars_in_rval(Rval1, Vars1),
	list__append(Vars0, Vars1, Vars).
exprn_aux__vars_in_lval(mem_ref(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(lvar(Var), [Var]).

:- pred exprn_aux__vars_in_mem_ref(mem_ref, list(var)).
:- mode exprn_aux__vars_in_mem_ref(in, out) is det.

exprn_aux__vars_in_mem_ref(stackvar_ref(_), []).
exprn_aux__vars_in_mem_ref(framevar_ref(_), []).
exprn_aux__vars_in_mem_ref(heap_ref(Rval, _, _), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).

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
		Rval0 = create(Tag, Rvals0, Unique, Num),
		exprn_aux__substitute_lval_in_args(OldLval, NewLval,
			Rvals0, Rvals),
		Rval = create(Tag, Rvals, Unique, Num)
	;
		Rval0 = mkword(Tag, Rval1),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval1, Rval2),
		Rval = mkword(Tag, Rval2)
	;
		Rval0 = const(_Const),
		Rval = Rval0
	;
		Rval0 = unop(Unop, Rval1),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval1, Rval2),
		Rval = unop(Unop, Rval2)
	;
		Rval0 = binop(Binop, Rval1, Rval2),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval1, Rval3),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval2, Rval4),
		Rval = binop(Binop, Rval3, Rval4)
	;
		Rval0 = mem_addr(MemRef0),
		exprn_aux__substitute_lval_in_mem_ref(OldLval, NewLval,
			MemRef0, MemRef),
		Rval = mem_addr(MemRef)
	).

:- pred exprn_aux__substitute_lval_in_mem_ref(lval, lval, mem_ref, mem_ref).
:- mode exprn_aux__substitute_lval_in_mem_ref(in, in, in, out) is det.

exprn_aux__substitute_lval_in_mem_ref(OldLval, NewLval, MemRef0, MemRef) :-
	(
		MemRef0 = stackvar_ref(N),
		MemRef = stackvar_ref(N)
	;
		MemRef0 = framevar_ref(N),
		MemRef = framevar_ref(N)
	;
		MemRef0 = heap_ref(Rval0, Tag, N),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval0, Rval),
		MemRef = heap_ref(Rval, Tag, N)
	).

:- pred exprn_aux__substitute_lval_in_lval(lval, lval, lval, lval).
:- mode exprn_aux__substitute_lval_in_lval(in, in, in, out) is det.

exprn_aux__substitute_lval_in_lval(OldLval, NewLval, Lval0, Lval) :-
	(
		Lval0 = OldLval
	->
		Lval = NewLval
	;
		exprn_aux__substitute_lval_in_lval_2(OldLval, NewLval,
			Lval0, Lval)
	).

:- pred exprn_aux__substitute_lval_in_lval_2(lval, lval, lval, lval).
:- mode exprn_aux__substitute_lval_in_lval_2(in, in, in, out) is det.

exprn_aux__substitute_lval_in_lval_2(OldLval, NewLval, Lval0, Lval) :-
	(
		Lval0 = reg(T, N),
		Lval = reg(T, N)
	;
		Lval0 = succip,
		Lval = succip
	;
		Lval0 = maxfr,
		Lval = maxfr
	;
		Lval0 = curfr,
		Lval = curfr
	;
		Lval0 = hp,
		Lval = hp
	;
		Lval0 = sp,
		Lval = sp
	;
		Lval0 = temp(T, N),
		Lval = temp(T, N)
	;
		Lval0 = stackvar(N),
		Lval = stackvar(N)
	;
		Lval0 = framevar(N),
		Lval = framevar(N)
	;
		Lval0 = succip(Rval0),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval0, Rval),
		Lval = succip(Rval)
	;
		Lval0 = redoip(Rval0),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval0, Rval),
		Lval = redoip(Rval)
	;
		Lval0 = succfr(Rval0),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval0, Rval),
		Lval = succfr(Rval)
	;
		Lval0 = prevfr(Rval0),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval0, Rval),
		Lval = prevfr(Rval)
	;
		Lval0 = field(Tag, Rval1, Rval2),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval1, Rval3),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval2, Rval4),
		Lval = field(Tag, Rval3, Rval4)
	;
		Lval0 = mem_ref(Rval0),
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval0, Rval),
		Lval = mem_ref(Rval)
	;
		Lval0 = lvar(N),
		Lval = lvar(N)
	).

:- pred exprn_aux__substitute_lval_in_args(lval, lval,
				list(maybe(rval)), list(maybe(rval))).
:- mode exprn_aux__substitute_lval_in_args(in, in, in, out) is det.

exprn_aux__substitute_lval_in_args(_OldLval, _NewLval, [], []).
exprn_aux__substitute_lval_in_args(OldLval, NewLval, [M0 | Ms0], [M | Ms]) :-
	(
		M0 = yes(Rval0)
	->
		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
			Rval0, Rval),
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
			Rval0 = create(Tag, Rvals0, Unique, Num),
			exprn_aux__substitute_rval_in_args(OldRval, NewRval,
				Rvals0, Rvals),
			Rval = create(Tag, Rvals, Unique, Num)
		;
			Rval0 = mkword(Tag, Rval1),
			exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
				Rval1, Rval2),
			Rval = mkword(Tag, Rval2)
		;
			Rval0 = const(_Const),
			Rval = Rval0
		;
			Rval0 = unop(Unop, Rval1),
			exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
				Rval1, Rval2),
			Rval = unop(Unop, Rval2)
		;
			Rval0 = binop(Binop, Rval1, Rval2),
			exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
				Rval1, Rval3),
			exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
				Rval2, Rval4),
			Rval = binop(Binop, Rval3, Rval4)
		;
			Rval0 = mem_addr(MemRef1),
			exprn_aux__substitute_rval_in_mem_ref(OldRval, NewRval,
				MemRef1, MemRef2),
			Rval = mem_addr(MemRef2)
		)
	).

:- pred exprn_aux__substitute_rval_in_mem_ref(rval, rval, mem_ref, mem_ref).
:- mode exprn_aux__substitute_rval_in_mem_ref(in, in, in, out) is det.

exprn_aux__substitute_rval_in_mem_ref(OldRval, NewRval, MemRef0, MemRef) :-
	(
		MemRef0 = stackvar_ref(N),
		MemRef = stackvar_ref(N)
	;
		MemRef0 = framevar_ref(N),
		MemRef = framevar_ref(N)
	;
		MemRef0 = heap_ref(Rval0, Tag, N),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval0, Rval),
		MemRef = heap_ref(Rval, Tag, N)
	).

:- pred exprn_aux__substitute_rval_in_lval(rval, rval, lval, lval).
:- mode exprn_aux__substitute_rval_in_lval(in, in, in, out) is det.

exprn_aux__substitute_rval_in_lval(OldRval, NewRval, Lval0, Lval) :-
	(
		Lval0 = reg(T, N),
		Lval = reg(T, N)
	;
		Lval0 = succip,
		Lval = succip
	;
		Lval0 = maxfr,
		Lval = maxfr
	;
		Lval0 = curfr,
		Lval = curfr
	;
		Lval0 = hp,
		Lval = hp
	;
		Lval0 = sp,
		Lval = sp
	;
		Lval0 = temp(T, N),
		Lval = temp(T, N)
	;
		Lval0 = stackvar(N),
		Lval = stackvar(N)
	;
		Lval0 = framevar(N),
		Lval = framevar(N)
	;
		Lval0 = succip(Rval0),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval0, Rval),
		Lval = succip(Rval)
	;
		Lval0 = redoip(Rval0),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval0, Rval),
		Lval = redoip(Rval)
	;
		Lval0 = succfr(Rval0),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval0, Rval),
		Lval = succfr(Rval)
	;
		Lval0 = prevfr(Rval0),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval0, Rval),
		Lval = prevfr(Rval)
	;
		Lval0 = field(Tag, Rval1, Rval2),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval1, Rval3),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval2, Rval4),
		Lval = field(Tag, Rval3, Rval4)
	;
		Lval0 = mem_ref(Rval0),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval0, Rval),
		Lval = mem_ref(Rval)
	;
		Lval0 = lvar(N),
		Lval = lvar(N)
	).

:- pred exprn_aux__substitute_rval_in_args(rval, rval,
				list(maybe(rval)), list(maybe(rval))).
:- mode exprn_aux__substitute_rval_in_args(in, in, in, out) is det.

exprn_aux__substitute_rval_in_args(_OldRval, _NewRval, [], []).
exprn_aux__substitute_rval_in_args(OldRval, NewRval, [M0 | Ms0], [M | Ms]) :-
	(
		M0 = yes(Rval0)
	->
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval0, Rval),
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
		Rval0 = lval(field(Tag, create(Tag, Args, _, _), Field)),
		Field = const(int_const(FieldNum))
	->
		list__index0_det(Args, FieldNum, yes(Rval))
	;
		Rval0 = lval(field(Tag, Rval1, Num)),
		exprn_aux__simplify_rval_2(Rval1, Rval2)
	->
		Rval = lval(field(Tag, Rval2, Num))
	;
		Rval0 = create(Tag, Args0, Unique, CNum),
		exprn_aux__simplify_args(Args0, Args),
		Args \= Args0
	->
		Rval = create(Tag, Args, Unique, CNum)
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

	% give an rval, return a list of the code and data addresses
	% that are referenced by that rval

exprn_aux__rval_addrs(lval(Lval), CodeAddrs, DataAddrs) :-
	exprn_aux__lval_addrs(Lval, CodeAddrs, DataAddrs).
exprn_aux__rval_addrs(var(_), [], []).
exprn_aux__rval_addrs(create(_, MaybeRvals, _, _), CodeAddrs, DataAddrs) :-
	exprn_aux__maybe_rval_list_addrs(MaybeRvals, CodeAddrs, DataAddrs).
exprn_aux__rval_addrs(mkword(_Tag, Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).
exprn_aux__rval_addrs(const(Const), CodeAddrs, DataAddrs) :-
	( Const = code_addr_const(CodeAddress) ->
		CodeAddrs = [CodeAddress],
		DataAddrs = []
	; Const = data_addr_const(DataAddress) ->
		CodeAddrs = [],
		DataAddrs = [DataAddress]
	;
		CodeAddrs = [],
		DataAddrs = []
	).
exprn_aux__rval_addrs(unop(_, Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).
exprn_aux__rval_addrs(binop(_, Rval1, Rval2), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval1, CodeAddrs1, DataAddrs1),
	exprn_aux__rval_addrs(Rval2, CodeAddrs2, DataAddrs2),
	list__append(CodeAddrs1, CodeAddrs2, CodeAddrs),
	list__append(DataAddrs1, DataAddrs2, DataAddrs).
exprn_aux__rval_addrs(mem_addr(Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__mem_ref_addrs(Rval, CodeAddrs, DataAddrs).

	% give an lval, return a list of the code and data addresses
	% that are referenced by that lval

exprn_aux__lval_addrs(reg(_, _), [], []).
exprn_aux__lval_addrs(stackvar(_Int), [], []).
exprn_aux__lval_addrs(framevar(_Int), [], []).
exprn_aux__lval_addrs(succip, [], []).
exprn_aux__lval_addrs(maxfr, [], []).
exprn_aux__lval_addrs(curfr, [], []).
exprn_aux__lval_addrs(prevfr(Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).
exprn_aux__lval_addrs(succfr(Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).
exprn_aux__lval_addrs(redoip(Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).
exprn_aux__lval_addrs(succip(Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).
exprn_aux__lval_addrs(hp, [], []).
exprn_aux__lval_addrs(sp, [], []).
exprn_aux__lval_addrs(field(_Tag, Rval1, Rval2), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval1, CodeAddrs1, DataAddrs1),
	exprn_aux__rval_addrs(Rval2, CodeAddrs2, DataAddrs2),
	list__append(CodeAddrs1, CodeAddrs2, CodeAddrs),
	list__append(DataAddrs1, DataAddrs2, DataAddrs).
exprn_aux__lval_addrs(lvar(_Var), [], []).
exprn_aux__lval_addrs(temp(_, _), [], []).
exprn_aux__lval_addrs(mem_ref(Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).

	% give a list of rvals, return a list of the code and data addresses
	% that are referenced by those rvals

exprn_aux__rval_list_addrs([], [], []).
exprn_aux__rval_list_addrs([Rval | Rvals], CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs0, DataAddrs0),
	exprn_aux__rval_list_addrs(Rvals, CodeAddrs1, DataAddrs1),
	list__append(CodeAddrs0, CodeAddrs1, CodeAddrs),
	list__append(DataAddrs0, DataAddrs1, DataAddrs).

	% give a list of lvals, return a list of the code and data addresses
	% that are referenced by those lvals

exprn_aux__lval_list_addrs([], [], []).
exprn_aux__lval_list_addrs([Lval | Lvals], CodeAddrs, DataAddrs) :-
	exprn_aux__lval_addrs(Lval, CodeAddrs0, DataAddrs0),
	exprn_aux__lval_list_addrs(Lvals, CodeAddrs1, DataAddrs1),
	list__append(CodeAddrs0, CodeAddrs1, CodeAddrs),
	list__append(DataAddrs0, DataAddrs1, DataAddrs).

:- pred exprn_aux__mem_ref_addrs(mem_ref, list(code_addr), list(data_addr)).
:- mode exprn_aux__mem_ref_addrs(in, out, out) is det.

exprn_aux__mem_ref_addrs(stackvar_ref(_), [], []).
exprn_aux__mem_ref_addrs(framevar_ref(_), [], []).
exprn_aux__mem_ref_addrs(heap_ref(Rval, _, _), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).

	% give a list of maybe(rval), return a list of the code and data
	% addresses that are reference by that list

exprn_aux__maybe_rval_list_addrs([], [], []).
exprn_aux__maybe_rval_list_addrs([MaybeRval | MaybeRvals],
		CodeAddrs, DataAddrs) :-
	( MaybeRval = yes(Rval) ->
		exprn_aux__rval_addrs(Rval, CodeAddrs0, DataAddrs0),
		exprn_aux__maybe_rval_list_addrs(MaybeRvals,
			CodeAddrs1, DataAddrs1),
		list__append(CodeAddrs0, CodeAddrs1, CodeAddrs),
		list__append(DataAddrs0, DataAddrs1, DataAddrs)
	;
		exprn_aux__maybe_rval_list_addrs(MaybeRvals,
			CodeAddrs, DataAddrs)
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
