%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%------------------------------------------------------------------------------%

:- module ll_backend__exprn_aux.

:- interface.

:- import_module ll_backend__llds, libs__options, parse_tree__prog_data.
:- import_module list, std_util, bool, assoc_list.

:- type exprn_opts
	--->	nlg_asm_sgt_ubf(
			bool,	% --use-non-local-gotos
			bool,	% --use-asm-labels
			bool,	% --static-ground-terms
			bool	% --unboxed-float
		).

:- pred exprn_aux__init_exprn_opts(option_table::in, exprn_opts::out) is det.

	% Determine whether an rval_const can be used as the initializer
	% of a C static constant.
:- pred exprn_aux__const_is_constant(rval_const::in, exprn_opts::in, bool::out)
	is det.

	% exprn_aux__imported_is_constant(NonLocalGotos, AsmLabels, IsConst)
	% figures out whether an imported label address is a constant.
	% This depends on how we treat labels.

:- pred exprn_aux__imported_is_constant(bool::in, bool::in, bool::out) is det.

:- pred exprn_aux__rval_contains_lval(rval::in, lval::in) is semidet.

:- pred exprn_aux__rval_contains_rval(rval, rval).
:- mode exprn_aux__rval_contains_rval(in, in) is semidet.
:- mode exprn_aux__rval_contains_rval(in, out) is multidet.

:- pred exprn_aux__args_contain_rval(list(maybe(rval)), rval).
:- mode exprn_aux__args_contain_rval(in, in) is semidet.
:- mode exprn_aux__args_contain_rval(in, out) is nondet.

	% exprn_aux__substitute_lval_in_instr(OldLval, NewLval, Instr0, Instr,
	% SubstCount0, SubstCount): substitute all occurrences of OldLval in
	% Instr0 with NewLval, yielding Instr. Return the number of
	% substitutions performed as the difference between SubstCount0 and
	% SubstCount.
	%
	% The other exprn_aux__substitute_lval_in_* predicates are similar,
	% although many do not return substitution counts.

:- pred exprn_aux__substitute_lval_in_instr(lval::in, lval::in,
	instruction::in, instruction::out, int::in, int::out) is det.

:- pred exprn_aux__substitute_lval_in_lval(lval::in, lval::in,
	lval::in, lval::out) is det.

:- pred exprn_aux__substitute_lval_in_rval(lval::in, lval::in,
	rval::in, rval::out) is det.

:- pred exprn_aux__substitute_rval_in_rval(rval::in, rval::in,
	rval::in, rval::out) is det.

:- pred exprn_aux__substitute_vars_in_rval(assoc_list(prog_var, rval)::in,
	rval::in, rval::out) is det.

:- pred exprn_aux__substitute_rvals_in_rval(assoc_list(rval, rval)::in,
	rval::in, rval::out) is det.

:- pred exprn_aux__vars_in_lval(lval::in, list(prog_var)::out) is det.

:- pred exprn_aux__vars_in_rval(rval::in, list(prog_var)::out) is det.

:- pred exprn_aux__simplify_rval(rval::in, rval::out) is det.

	% the following predicates take an lval/rval (list)
	% and return a list of the code and data addresses that it references.

:- pred exprn_aux__rval_list_addrs(list(rval)::in,
	list(code_addr)::out, list(data_addr)::out) is det.

:- pred exprn_aux__lval_list_addrs(list(lval)::in,
	list(code_addr)::out, list(data_addr)::out) is det.

:- pred exprn_aux__rval_addrs(rval::in,
	list(code_addr)::out, list(data_addr)::out) is det.

:- pred exprn_aux__lval_addrs(lval::in,
	list(code_addr)::out, list(data_addr)::out) is det.

:- pred exprn_aux__maybe_rval_list_addrs(list(maybe(rval))::in,
	list(code_addr)::out, list(data_addr)::out) is det.

:- func exprn_aux__var_lval_to_rval(prog_var, lval) = rval.

:- func exprn_aux__lval_to_rval(lval) = rval.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module libs__options.
:- import_module int, set, require, getopt.

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
exprn_aux__const_is_constant(multi_string_const(_, _), _, yes).
exprn_aux__const_is_constant(code_addr_const(CodeAddr), ExprnOpts, IsConst) :-
	exprn_aux__addr_is_constant(CodeAddr, ExprnOpts, IsConst).
exprn_aux__const_is_constant(data_addr_const(_), _, yes).
exprn_aux__const_is_constant(label_entry(Label), ExprnOpts, IsConst) :-
	exprn_aux__addr_is_constant(label(Label), ExprnOpts, IsConst).

:- pred exprn_aux__addr_is_constant(code_addr::in, exprn_opts::in, bool::out)
	is det.

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
exprn_aux__addr_is_constant(do_trace_redo_fail_shallow, _, no).
exprn_aux__addr_is_constant(do_trace_redo_fail_deep, _, no).
exprn_aux__addr_is_constant(do_call_closure, _, no).
exprn_aux__addr_is_constant(do_call_class_method, _, no).
exprn_aux__addr_is_constant(do_det_aditi_call, _, no).
exprn_aux__addr_is_constant(do_semidet_aditi_call, _, no).
exprn_aux__addr_is_constant(do_nondet_aditi_call, _, no).
exprn_aux__addr_is_constant(do_aditi_insert, _, no).
exprn_aux__addr_is_constant(do_aditi_delete, _, no).
exprn_aux__addr_is_constant(do_aditi_bulk_insert, _, no).
exprn_aux__addr_is_constant(do_aditi_bulk_delete, _, no).
exprn_aux__addr_is_constant(do_aditi_bulk_modify, _, no).
exprn_aux__addr_is_constant(do_not_reached, _, no).

:- pred exprn_aux__label_is_constant(label::in, bool::in, bool::in, bool::out)
	is det.

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
exprn_aux__rval_contains_lval(create(_, Rvals, _, _, _, _, Reuse), Lval) :-
	exprn_aux__args_contain_lval([Reuse | Rvals], Lval).
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

:- pred exprn_aux__lval_contains_lval(lval::in, lval::in) is semidet.

exprn_aux__lval_contains_lval(Lval0, Lval) :-
	(
		Lval0 = Lval
	->
		true
	;
		Lval0 = field(_MaybeTag, Rval0, Rval1)
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

:- pred exprn_aux__args_contain_lval(list(maybe(rval))::in, lval::in)
	is semidet.

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
			Rval0 = create(_, Rvals, _, _, _, _, Reuse),
			exprn_aux__args_contain_rval([Reuse | Rvals], Rval)
		;
			Rval0 = mkword(_, Rval1),
			exprn_aux__rval_contains_rval(Rval1, Rval)
		;
			Rval0 = unop(_Unop, Rval1),
			exprn_aux__rval_contains_rval(Rval1, Rval)
		;
			Rval0 = binop(_Binop, Rval1, Rval2),
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

exprn_aux__lval_contains_rval(field(_MaybeTag, Rval0, Rval1), Rval) :-
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
exprn_aux__vars_in_rval(create(_, Rvals, _, _, _, _, Reuse), Vars) :-
	exprn_aux__vars_in_args([Reuse | Rvals], Vars).
exprn_aux__vars_in_rval(mkword(_, Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_rval(const(_Conts), []).
exprn_aux__vars_in_rval(unop(_Unop, Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_rval(binop(_Binop, Rval0, Rval1), Vars) :-
	exprn_aux__vars_in_rval(Rval0, Vars0),
	exprn_aux__vars_in_rval(Rval1, Vars1),
	list__append(Vars0, Vars1, Vars).
exprn_aux__vars_in_rval(mem_addr(MemRef), Vars) :-
	exprn_aux__vars_in_mem_ref(MemRef, Vars).

exprn_aux__vars_in_lval(reg(_Type, _RegNum), []).
exprn_aux__vars_in_lval(temp(_Type, _TmpNum), []).
exprn_aux__vars_in_lval(succip, []).
exprn_aux__vars_in_lval(maxfr, []).
exprn_aux__vars_in_lval(curfr, []).
exprn_aux__vars_in_lval(hp, []).
exprn_aux__vars_in_lval(sp, []).
exprn_aux__vars_in_lval(stackvar(_SlotNum), []).
exprn_aux__vars_in_lval(framevar(_SlotNum), []).
exprn_aux__vars_in_lval(succip(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(redoip(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(redofr(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(succfr(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(prevfr(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(field(_MaybeTag, Rval0, Rval1), Vars) :-
	exprn_aux__vars_in_rval(Rval0, Vars0),
	exprn_aux__vars_in_rval(Rval1, Vars1),
	list__append(Vars0, Vars1, Vars).
exprn_aux__vars_in_lval(mem_ref(Rval), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).
exprn_aux__vars_in_lval(lvar(Var), [Var]).

:- pred exprn_aux__vars_in_mem_ref(mem_ref::in, list(prog_var)::out) is det.

exprn_aux__vars_in_mem_ref(stackvar_ref(_SlotNum), []).
exprn_aux__vars_in_mem_ref(framevar_ref(_SlotNum), []).
exprn_aux__vars_in_mem_ref(heap_ref(Rval, _Tag, _FieldNum), Vars) :-
	exprn_aux__vars_in_rval(Rval, Vars).

:- pred exprn_aux__vars_in_args(list(maybe(rval))::in, list(prog_var)::out)
	is det.

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

exprn_aux__substitute_lval_in_lval(OldLval, NewLval, Lval0, Lval) :-
	exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval,
		0, _SubstCount).

exprn_aux__substitute_lval_in_rval(OldLval, NewLval, Rval0, Rval) :-
	exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval,
		0, _SubstCount).

exprn_aux__substitute_lval_in_instr(OldLval, NewLval, Instr0, Instr, N0, N) :-
	Instr0 = Uinstr0 - Comment,
	exprn_aux__substitute_lval_in_uinstr(OldLval, NewLval,
		Uinstr0, Uinstr, N0, N),
	Instr = Uinstr - Comment.

:- pred exprn_aux__substitute_lval_in_uinstr(lval::in, lval::in,
	instr::in, instr::out, int::in, int::out) is det.

exprn_aux__substitute_lval_in_uinstr(OldLval, NewLval, Uinstr0, Uinstr, N0, N)
		:-
	(
		Uinstr0 = comment(_Comment),
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = livevals(LvalSet0),
		set__to_sorted_list(LvalSet0, Lvals0),
		list__map_foldl(
			exprn_aux__substitute_lval_in_lval_count(OldLval,
				NewLval),
			Lvals0, Lvals, N0, N),
		set__list_to_set(Lvals, LvalSet),
		Uinstr = livevals(LvalSet)
	;
		Uinstr0 = block(TempR, TempF, Instrs0),
		list__map_foldl(
			exprn_aux__substitute_lval_in_instr(OldLval, NewLval),
			Instrs0, Instrs, N0, N),
		Uinstr = block(TempR, TempF, Instrs)
	;
		Uinstr0 = assign(Lval0, Rval0),
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval,
			Lval0, Lval, N0, N1),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N1, N),
		Uinstr = assign(Lval, Rval)
	;
		Uinstr0 = call(_, _, _, _, _, _),
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = mkframe(_, _),
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = label(_),
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = goto(_),
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = computed_goto(Rval0, Labels),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Uinstr = computed_goto(Rval, Labels)
	;
		Uinstr0 = c_code(Code, LiveLvals0),
		exprn_aux__substitute_lval_in_live_lval_info(OldLval, NewLval,
			LiveLvals0, LiveLvals, N0, N),
		Uinstr = c_code(Code, LiveLvals)
	;
		Uinstr0 = if_val(Rval0, CodeAddr),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Uinstr = if_val(Rval, CodeAddr)
	;
		Uinstr0 = incr_hp(Lval0, MaybeTag, Rval0, TypeCtor),
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval,
			Lval0, Lval, N0, N1),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N1, N),
		Uinstr = incr_hp(Lval, MaybeTag, Rval, TypeCtor)
	;
		Uinstr0 = mark_hp(Lval0),
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval,
			Lval0, Lval, N0, N),
		Uinstr = mark_hp(Lval)
	;
		Uinstr0 = restore_hp(Rval0),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Uinstr = restore_hp(Rval)
	;
		Uinstr0 = free_heap(Rval0),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Uinstr = free_heap(Rval)
	;
		Uinstr0 = store_ticket(Lval0),
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval,
			Lval0, Lval, N0, N),
		Uinstr = store_ticket(Lval)
	;
		Uinstr0 = reset_ticket(Rval0, Reason),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Uinstr = reset_ticket(Rval, Reason)
	;
		Uinstr0 = prune_ticket,
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = discard_ticket,
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = mark_ticket_stack(Lval0),
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval,
			Lval0, Lval, N0, N),
		Uinstr = mark_ticket_stack(Lval)
	;
		Uinstr0 = prune_tickets_to(Rval0),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Uinstr = prune_tickets_to(Rval)
%	;
%		% discard_tickets_to(_) is used only in hand-written code
%		Uinstr0 = discard_tickets_to(Rval0),
%		exprn_aux__substitute_lval_in_rval(OldLval, NewLval,
%			Rval0, Rval, N0, N),
%		Uinstr = discard_tickets_to(Rval)
	;
		Uinstr0 = incr_sp(_, _),
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = decr_sp(_),
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = pragma_c(Decls, Components0, MayCallMercury,
			MaybeLabel1, MaybeLabel2, MaybeLabel3, MaybeLabel4,
			ReferStackSlot),
		list__map_foldl(exprn_aux__substitute_lval_in_component(
			OldLval, NewLval), Components0, Components, N0, N),
		Uinstr = pragma_c(Decls, Components, MayCallMercury,
			MaybeLabel1, MaybeLabel2, MaybeLabel3, MaybeLabel4,
			ReferStackSlot)
	;
		Uinstr0 = init_sync_term(Lval0, BranchCount),
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval,
			Lval0, Lval, N0, N),
		Uinstr = init_sync_term(Lval, BranchCount)
	;
		Uinstr0 = fork(_, _, _),
		Uinstr = Uinstr0,
		N = N0
	;
		Uinstr0 = join_and_terminate(Lval0),
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval,
			Lval0, Lval, N0, N),
		Uinstr = join_and_terminate(Lval)
	;
		Uinstr0 = join_and_continue(Lval0, Label),
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval,
			Lval0, Lval, N0, N),
		Uinstr = join_and_continue(Lval, Label)
	).

:- pred exprn_aux__substitute_lval_in_component(lval::in, lval::in,
	pragma_c_component::in, pragma_c_component::out, int::in, int::out)
	is det.

exprn_aux__substitute_lval_in_component(OldLval, NewLval,
		Component0, Component, N0, N) :-
	(
		Component0 = pragma_c_inputs(Inputs0),
		list__map_foldl(exprn_aux__substitute_lval_in_pragma_c_input(
			OldLval, NewLval), Inputs0, Inputs, N0, N),
		Component = pragma_c_inputs(Inputs)
	;
		Component0 = pragma_c_outputs(Outputs0),
		list__map_foldl(exprn_aux__substitute_lval_in_pragma_c_output(
			OldLval, NewLval), Outputs0, Outputs, N0, N),
		Component = pragma_c_outputs(Outputs)
	;
		Component0 = pragma_c_user_code(_, _),
		Component = Component0,
		N = N0
	;
		Component0 = pragma_c_raw_code(Code, LvalSet0),
		exprn_aux__substitute_lval_in_live_lval_info(OldLval, NewLval,
			LvalSet0, LvalSet, N0, N),
		Component = pragma_c_raw_code(Code, LvalSet)
	;
		Component0 = pragma_c_fail_to(_),
		Component = Component0,
		N = N0
	;
		Component0 = pragma_c_noop,
		Component = Component0,
		N = N0
	).

:- pred exprn_aux__substitute_lval_in_live_lval_info(lval::in, lval::in,
	c_code_live_lvals::in, c_code_live_lvals::out, int::in, int::out)
	is det.

exprn_aux__substitute_lval_in_live_lval_info(_OldLval, _NewLval,
		no_live_lvals_info, no_live_lvals_info, N, N).
exprn_aux__substitute_lval_in_live_lval_info(OldLval, NewLval,
		live_lvals_info(LvalSet0), live_lvals_info(LvalSet), N0, N) :-
	Lvals0 = set__to_sorted_list(LvalSet0),
	list__map_foldl(
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval),
		Lvals0, Lvals, N0, N),
	set__list_to_set(Lvals, LvalSet).

:- pred exprn_aux__substitute_lval_in_pragma_c_input(lval::in, lval::in,
	pragma_c_input::in, pragma_c_input::out, int::in, int::out) is det.

exprn_aux__substitute_lval_in_pragma_c_input(OldLval, NewLval, Out0, Out,
		N0, N) :-
	Out0 = pragma_c_input(Name, Type, Rval0, MaybeForeign),
	exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval,
		N0, N),
	Out = pragma_c_input(Name, Type, Rval, MaybeForeign).

:- pred exprn_aux__substitute_lval_in_pragma_c_output(lval::in, lval::in,
	pragma_c_output::in, pragma_c_output::out, int::in, int::out) is det.

exprn_aux__substitute_lval_in_pragma_c_output(OldLval, NewLval, Out0, Out,
		N0, N) :-
	Out0 = pragma_c_output(Lval0, Type, Name, MaybeForeign),
	exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval,
		N0, N),
	Out = pragma_c_output(Lval, Type, Name, MaybeForeign).

:- pred exprn_aux__substitute_lval_in_rval_count(lval::in, lval::in,
	rval::in, rval::out, int::in, int::out) is det.

exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval, Rval0, Rval,
		N0, N) :-
	(
		Rval0 = lval(Lval0),
		exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval,
			Lval0, Lval, N0, N),
		Rval = lval(Lval)
	;
		Rval0 = var(_Var),
		Rval = Rval0,
		N = N0
	;
		Rval0 = create(Tag, Rvals0, ArgTypes, StatDyn,
				Num, Msg, Reuse0),
		exprn_aux__substitute_lval_in_args(OldLval, NewLval,
			Rvals0, Rvals, N0, N1),
		exprn_aux__substitute_lval_in_arg(OldLval, NewLval,
			Reuse0, Reuse, N1, N),
		Rval = create(Tag, Rvals, ArgTypes, StatDyn, Num, Msg, Reuse)
	;
		Rval0 = mkword(Tag, Rval1),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval1, Rval2, N0, N),
		Rval = mkword(Tag, Rval2)
	;
		Rval0 = const(_Const),
		Rval = Rval0,
		N = N0
	;
		Rval0 = unop(Unop, Rval1),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval1, Rval2, N0, N),
		Rval = unop(Unop, Rval2)
	;
		Rval0 = binop(Binop, Rval1, Rval2),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval1, Rval3, N0, N1),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval2, Rval4, N1, N),
		Rval = binop(Binop, Rval3, Rval4)
	;
		Rval0 = mem_addr(MemRef0),
		exprn_aux__substitute_lval_in_mem_ref(OldLval, NewLval,
			MemRef0, MemRef, N0, N),
		Rval = mem_addr(MemRef)
	).

:- pred exprn_aux__substitute_lval_in_mem_ref(lval::in, lval::in,
	mem_ref::in, mem_ref::out, int::in, int::out) is det.

exprn_aux__substitute_lval_in_mem_ref(OldLval, NewLval, MemRef0, MemRef,
		N0, N) :-
	(
		MemRef0 = stackvar_ref(_SlotNum),
		MemRef = MemRef0,
		N = N0
	;
		MemRef0 = framevar_ref(_SlotNum),
		MemRef = MemRef0,
		N = N0
	;
		MemRef0 = heap_ref(Rval0, Tag, FieldNum),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		MemRef = heap_ref(Rval, Tag, FieldNum)
	).

:- pred exprn_aux__substitute_lval_in_lval_count(lval::in, lval::in,
	lval::in, lval::out, int::in, int::out) is det.

exprn_aux__substitute_lval_in_lval_count(OldLval, NewLval, Lval0, Lval,
		N0, N) :-
	( Lval0 = OldLval ->
		Lval = NewLval,
		N = N0 + 1
	;
		exprn_aux__substitute_lval_in_lval_count_2(OldLval, NewLval,
			Lval0, Lval, N0, N)
	).

:- pred exprn_aux__substitute_lval_in_lval_count_2(lval::in, lval::in,
	lval::in, lval::out, int::in, int::out) is det.

exprn_aux__substitute_lval_in_lval_count_2(OldLval, NewLval, Lval0, Lval,
		N0, N) :-
	(
		Lval0 = reg(_Type, _RegNum),
		Lval = Lval0,
		N = N0
	;
		Lval0 = succip,
		Lval = succip,
		N = N0
	;
		Lval0 = maxfr,
		Lval = maxfr,
		N = N0
	;
		Lval0 = curfr,
		Lval = curfr,
		N = N0
	;
		Lval0 = hp,
		Lval = hp,
		N = N0
	;
		Lval0 = sp,
		Lval = sp,
		N = N0
	;
		Lval0 = temp(_Type, _TmpNum),
		Lval = Lval0,
		N = N0
	;
		Lval0 = stackvar(_SlotNum),
		Lval = Lval0,
		N = N0
	;
		Lval0 = framevar(_SlotNum),
		Lval = Lval0,
		N = N0
	;
		Lval0 = succip(Rval0),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Lval = succip(Rval)
	;
		Lval0 = redoip(Rval0),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Lval = redoip(Rval)
	;
		Lval0 = redofr(Rval0),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Lval = redofr(Rval)
	;
		Lval0 = succfr(Rval0),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Lval = succfr(Rval)
	;
		Lval0 = prevfr(Rval0),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Lval = prevfr(Rval)
	;
		Lval0 = field(Tag, Rval1, Rval2),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval1, Rval3, N0, N1),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval2, Rval4, N1, N),
		Lval = field(Tag, Rval3, Rval4)
	;
		Lval0 = mem_ref(Rval0),
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		Lval = mem_ref(Rval)
	;
		Lval0 = lvar(_Var),
		Lval = Lval0,
		N = N0
	).

:- pred exprn_aux__substitute_lval_in_args(lval::in, lval::in,
	list(maybe(rval))::in, list(maybe(rval))::out, int::in, int::out)
	is det.

exprn_aux__substitute_lval_in_args(_OldLval, _NewLval, [], [], N, N).
exprn_aux__substitute_lval_in_args(OldLval, NewLval, [M0 | Ms0], [M | Ms],
		N0, N) :-
	exprn_aux__substitute_lval_in_arg(OldLval, NewLval, M0, M, N0, N1),
	exprn_aux__substitute_lval_in_args(OldLval, NewLval, Ms0, Ms, N1, N).

:- pred exprn_aux__substitute_lval_in_arg(lval::in, lval::in,
	maybe(rval)::in, maybe(rval)::out, int::in, int::out) is det.

exprn_aux__substitute_lval_in_arg(OldLval, NewLval, M0, M, N0, N) :-
	( M0 = yes(Rval0) ->
		exprn_aux__substitute_lval_in_rval_count(OldLval, NewLval,
			Rval0, Rval, N0, N),
		M = yes(Rval)
	;
		M = M0,
		N = N0
	).

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
			Rval0 = var(_Var),
			Rval = Rval0
		;
			Rval0 = create(Tag, Rvals0, ATs, StatDyn,
					Num, Msg, Reuse0),
			exprn_aux__substitute_rval_in_args(OldRval, NewRval,
				Rvals0, Rvals),
			exprn_aux__substitute_rval_in_arg(OldRval, NewRval,
				Reuse0, Reuse),
			Rval = create(Tag, Rvals, ATs, StatDyn,
					Num, Msg, Reuse)
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

:- pred exprn_aux__substitute_rval_in_mem_ref(rval::in, rval::in,
	mem_ref::in, mem_ref::out) is det.

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

:- pred exprn_aux__substitute_rval_in_lval(rval::in, rval::in,
	lval::in, lval::out) is det.

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
		Lval0 = redofr(Rval0),
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval0, Rval),
		Lval = redofr(Rval)
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

:- pred exprn_aux__substitute_rval_in_args(rval::in, rval::in,
	list(maybe(rval))::in, list(maybe(rval))::out) is det.

exprn_aux__substitute_rval_in_args(_OldRval, _NewRval, [], []).
exprn_aux__substitute_rval_in_args(OldRval, NewRval, [M0 | Ms0], [M | Ms]) :-
	exprn_aux__substitute_rval_in_arg(OldRval, NewRval, M0, M),
	exprn_aux__substitute_rval_in_args(OldRval, NewRval, Ms0, Ms).

:- pred exprn_aux__substitute_rval_in_arg(rval::in, rval::in,
	maybe(rval)::in, maybe(rval)::out) is det.

exprn_aux__substitute_rval_in_arg(OldRval, NewRval, M0, M) :-
	(
		M0 = yes(Rval0)
	->
		exprn_aux__substitute_rval_in_rval(OldRval, NewRval,
			Rval0, Rval),
		M = yes(Rval)
	;
		M = M0
	).

%------------------------------------------------------------------------------%

exprn_aux__substitute_vars_in_rval([], Rval, Rval).
exprn_aux__substitute_vars_in_rval([Var - Sub | Rest], Rval0, Rval) :-
	exprn_aux__substitute_rval_in_rval(var(Var), Sub, Rval0, Rval1),
	exprn_aux__substitute_vars_in_rval(Rest, Rval1, Rval).

% When we substitute one set of rvals for another, we face the problem
% that the substitution may not be idempotent. We finesse this problem by
% substituting unique new rvals for the original rvals, and then substituting
% the replacement rvals for these unique rvals. We guarantee the uniqueness
% of these rvals by using framevars with negative numbers for them.

exprn_aux__substitute_rvals_in_rval(RvalPairs, Rval0, Rval) :-
	exprn_aux__substitute_rvals_in_rval_1(RvalPairs, 0,
		RvalUniqPairs, UniqRvalPairs),
	exprn_aux__substitute_rvals_in_rval_2(RvalUniqPairs, Rval0, Rval1),
	exprn_aux__substitute_rvals_in_rval_2(UniqRvalPairs, Rval1, Rval).

:- pred exprn_aux__substitute_rvals_in_rval_1(assoc_list(rval, rval)::in,
	int::in, assoc_list(rval, rval)::out, assoc_list(rval, rval)::out)
	is det.

exprn_aux__substitute_rvals_in_rval_1([], _, [], []).
exprn_aux__substitute_rvals_in_rval_1([Rval1 - Rval2 | RvalPairList], N0,
		[Rval1 - Uniq | RvalUniqList], [Uniq - Rval2 | UniqRvalList]) :-
	N1 is N0 - 1,
	Uniq = lval(framevar(N1)),
	exprn_aux__substitute_rvals_in_rval_1(RvalPairList, N1,
		RvalUniqList, UniqRvalList).

:- pred exprn_aux__substitute_rvals_in_rval_2(assoc_list(rval, rval)::in,
	rval::in, rval::out) is det.

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

:- pred exprn_aux__simplify_rval_2(rval::in, rval::out) is semidet.

exprn_aux__simplify_rval_2(Rval0, Rval) :-
	(
		Rval0 = lval(field(MaybeTag, Base, Field)),
		Base = create(Tag, Args, _, _, _, _, _),
		(
			MaybeTag = yes(Tag)
		;
			MaybeTag = no
		),
		Field = const(int_const(FieldNum))
	->
		list__index0_det(Args, FieldNum, yes(Rval))
	;
		Rval0 = lval(field(MaybeTag, Rval1, Num)),
		exprn_aux__simplify_rval_2(Rval1, Rval2)
	->
		Rval = lval(field(MaybeTag, Rval2, Num))
	;
		Rval0 = create(Tag, Args0, ArgTypes, StatDyn,
				CNum, Msg, Reuse0),
		exprn_aux__simplify_args(Args0, Args),
		exprn_aux__simplify_arg(Reuse0, Reuse),
		( Args \= Args0
		; Reuse \= Reuse0
		)
	->
		Rval = create(Tag, Args, ArgTypes, StatDyn, CNum, Msg, Reuse)
	;
		Rval0 = unop(UnOp, Rval1),
		exprn_aux__simplify_rval_2(Rval1, Rval2)
	->
		Rval = unop(UnOp, Rval2)
	;
		Rval0 = binop(BinOp, Rval1, Rval2),
		exprn_aux__simplify_rval_2(Rval1, Rval3)
	->
		Rval = binop(BinOp, Rval3, Rval2)
	;
		Rval0 = binop(BinOp, Rval1, Rval2),
		exprn_aux__simplify_rval_2(Rval2, Rval3)
	->
		Rval = binop(BinOp, Rval1, Rval3)
	;
		fail
	).

:- pred exprn_aux__simplify_args(list(maybe(rval))::in, list(maybe(rval))::out)
	is det.

exprn_aux__simplify_args([], []).
exprn_aux__simplify_args([MR0 | Ms0], [MR | Ms]) :-
	exprn_aux__simplify_args(Ms0, Ms),
	exprn_aux__simplify_arg(MR0, MR).

:- pred exprn_aux__simplify_arg(maybe(rval)::in, maybe(rval)::out) is det.

exprn_aux__simplify_arg(MR0, MR) :-
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
exprn_aux__rval_addrs(var(_Var), [], []).
exprn_aux__rval_addrs(create(_, MaybeRvals, _, _, _, _, Reuse),
		CodeAddrs, DataAddrs) :-
	exprn_aux__maybe_rval_list_addrs([Reuse | MaybeRvals],
		CodeAddrs, DataAddrs).
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
exprn_aux__rval_addrs(unop(_Unop, Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).
exprn_aux__rval_addrs(binop(_Binop, Rval1, Rval2), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval1, CodeAddrs1, DataAddrs1),
	exprn_aux__rval_addrs(Rval2, CodeAddrs2, DataAddrs2),
	list__append(CodeAddrs1, CodeAddrs2, CodeAddrs),
	list__append(DataAddrs1, DataAddrs2, DataAddrs).
exprn_aux__rval_addrs(mem_addr(Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__mem_ref_addrs(Rval, CodeAddrs, DataAddrs).

	% give an lval, return a list of the code and data addresses
	% that are referenced by that lval

exprn_aux__lval_addrs(reg(_Type, _RegNum), [], []).
exprn_aux__lval_addrs(stackvar(_SlotNum), [], []).
exprn_aux__lval_addrs(framevar(_SlotNum), [], []).
exprn_aux__lval_addrs(succip, [], []).
exprn_aux__lval_addrs(maxfr, [], []).
exprn_aux__lval_addrs(curfr, [], []).
exprn_aux__lval_addrs(prevfr(Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).
exprn_aux__lval_addrs(succfr(Rval), CodeAddrs, DataAddrs) :-
	exprn_aux__rval_addrs(Rval, CodeAddrs, DataAddrs).
exprn_aux__lval_addrs(redofr(Rval), CodeAddrs, DataAddrs) :-
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
exprn_aux__lval_addrs(temp(_Type, _TmpNum), [], []).
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

:- pred exprn_aux__mem_ref_addrs(mem_ref::in,
	list(code_addr)::out, list(data_addr)::out) is det.

exprn_aux__mem_ref_addrs(stackvar_ref(_SlotNum), [], []).
exprn_aux__mem_ref_addrs(framevar_ref(_SlotNum), [], []).
exprn_aux__mem_ref_addrs(heap_ref(Rval, _Tag, _FieldNum),
		CodeAddrs, DataAddrs) :-
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

exprn_aux__var_lval_to_rval(_Var, Lval) = lval(Lval).

exprn_aux__lval_to_rval(Lval) = lval(Lval).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
