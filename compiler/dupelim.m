%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% dupelim.m - eliminate some duplicate code sequences.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module dupelim.

:- interface.

:- import_module list, llds.

:- pred dupelim_main(list(instruction), list(instruction)).
% :- mode dupelim_main(di, uo) is det.
:- mode dupelim_main(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool, std_util, map, require.

:- import_module opt_util.

:- type block	--->	 block(label, string, list(instruction)).

dupelim_main(Instrs0, Instrs) :-
	map__init(Seqmap0),
	map__init(Replmap0),
	opt_util__skip_to_next_label(Instrs0, Initial, Instrs1),
	dupelim__make_blocks(Instrs1, Blocks0),
	dupelim__build_maps(Blocks0, yes, Seqmap0, Replmap0, Replmap),
	dupelim__replace_labels(Blocks0, Replmap, Blocks1),
	dupelim__condense(Blocks1, Instrs2),
	list__append(Initial, Instrs2, Instrs).

%-----------------------------------------------------------------------------%

:- pred dupelim__make_blocks(list(instruction), list(block)).
:- mode dupelim__make_blocks(in, out) is det.

dupelim__make_blocks(Instrs0, Blocks) :-
	( Instrs0 = [] ->
		Blocks = []
	; Instrs0 = [label(BlockLabel) - Comment | Instrs1] ->
		opt_util__skip_to_next_label(Instrs1, BlockCode, Instrs2),
		dupelim__make_blocks(Instrs2, Blocks1),
		Blocks = [block(BlockLabel, Comment, BlockCode) | Blocks1]
	;
		error("instruction other than label in dupelim__make_blocks")
	).

%-----------------------------------------------------------------------------%

:- pred dupelim__build_maps(list(block), bool, map(list(instruction), label),
	map(label, label), map(label, label)).
% :- mode dupelim__build_maps(ui, in, in, di, uo) is det.
:- mode dupelim__build_maps(in, in, in, in, out) is det.

% If a block can fall through, it should not be put into the sequence map.
% Two identical blocks that both fall through will fall through into
% different blocks.

% If a block can be fallen into, its label may not be replaced, since that
% would require the introduction of a goto to the new label at the end of
% the previous block.

dupelim__build_maps([], _FallInto, _Seqmap, Replmap, Replmap).
dupelim__build_maps([block(Label, _, Code) | Blocks1], FallInto, Seqmap0,
		Replmap0, Replmap) :-
	(
		list__reverse(Code, RevCode0),
		opt_util__skip_comments_livevals(RevCode0, RevCode1),
		RevCode1 = [Uinstr - _ | _],
		opt_util__can_instr_fall_through(Uinstr, no)
	->
		FallThrough = no
	;
		FallThrough = yes
	),
	( map__search(Seqmap0, Code, OldLabel) ->
		Seqmap1 = Seqmap0,
		(
			FallInto = yes,
			Replmap1 = Replmap0
		;
			FallInto = no,
			map__det_insert(Replmap0, Label, OldLabel, Replmap1)
		)
	;
		Replmap1 = Replmap0,
		(
			FallThrough = yes,
			Seqmap1 = Seqmap0
		;
			FallThrough = no,
			map__det_insert(Seqmap0, Code, Label, Seqmap1)
		)
	),
	dupelim__build_maps(Blocks1, FallThrough, Seqmap1, Replmap1, Replmap).

%-----------------------------------------------------------------------------%

:- pred dupelim__replace_labels(list(block), map(label, label), list(block)).
% :- mode dupelim__replace_labels(di, in, uo) is det.
:- mode dupelim__replace_labels(in, in, out) is det.

dupelim__replace_labels([], _Replmap, []).
dupelim__replace_labels([block(Label, Comment, Code) | Blocks1],
		Replmap, Blocks) :-
	( map__search(Replmap, Label, _) ->
		dupelim__replace_labels(Blocks1, Replmap, Blocks)
	;
		dupelim__replace_labels_instr_list(Code, Replmap, Code1),
		dupelim__replace_labels(Blocks1, Replmap, Blocks2),
		Blocks = [block(Label, Comment, Code1) | Blocks2]
	).

:- pred dupelim__replace_labels_instr_list(list(instruction), map(label, label),
	list(instruction)).
% :- mode dupelim__replace_labels_instr_list(di, in, uo) is det.
:- mode dupelim__replace_labels_instr_list(in, in, out) is det.

dupelim__replace_labels_instr_list([], _Replmap, []).
dupelim__replace_labels_instr_list([Instr0 - Comment | Instrs0],
		Replmap, [Instr - Comment | Instrs]) :-
	dupelim__replace_labels_instr(Instr0, Replmap, Instr),
	dupelim__replace_labels_instr_list(Instrs0, Replmap, Instrs).

:- pred dupelim__replace_labels_instr(instr, map(label, label), instr).
% :- mode dupelim__replace_labels_instr(di, in, uo) is det.
:- mode dupelim__replace_labels_instr(in, in, out) is det.

dupelim__replace_labels_instr(comment(Comment), _, comment(Comment)).
dupelim__replace_labels_instr(livevals(Livevals), _, livevals(Livevals)).
dupelim__replace_labels_instr(block(R, F, Instrs0), Replmap,
		block(R, F, Instrs)) :-
	dupelim__replace_labels_instr_list(Instrs0, Replmap, Instrs).
dupelim__replace_labels_instr(assign(Lval0, Rval0), Replmap,
		assign(Lval, Rval)) :-
	dupelim__replace_labels_lval(Lval0, Replmap, Lval),
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_instr(call(Target, Return0, LiveInfo, CM),
		Replmap, call(Target, Return, LiveInfo, CM)) :-
	dupelim__replace_labels_code_addr(Return0, Replmap, Return).
dupelim__replace_labels_instr(mkframe(Name, Size, Redoip0), Replmap,
		mkframe(Name, Size, Redoip)) :-
	dupelim__replace_labels_code_addr(Redoip0, Replmap, Redoip).
dupelim__replace_labels_instr(modframe(Redoip0), Replmap, modframe(Redoip)) :-
	dupelim__replace_labels_code_addr(Redoip0, Replmap, Redoip).
dupelim__replace_labels_instr(label(_), _, _) :-
	error("found label in dupelim__replace_labels_instr").
dupelim__replace_labels_instr(goto(Target0), Replmap, goto(Target)) :-
	dupelim__replace_labels_code_addr(Target0, Replmap, Target).
dupelim__replace_labels_instr(computed_goto(Rval0, Labels0), Replmap,
		computed_goto(Rval, Labels)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval),
	dupelim__replace_labels_label_list(Labels0, Replmap, Labels).
dupelim__replace_labels_instr(c_code(Code), _, c_code(Code)).
dupelim__replace_labels_instr(if_val(Rval0, Target0), Replmap,
		if_val(Rval, Target)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval),
	dupelim__replace_labels_code_addr(Target0, Replmap, Target).
dupelim__replace_labels_instr(incr_hp(Lval0, MaybeTag, Rval0, Msg), Replmap,
		incr_hp(Lval, MaybeTag, Rval, Msg)) :-
	dupelim__replace_labels_lval(Lval0, Replmap, Lval),
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_instr(mark_hp(Lval0), Replmap, mark_hp(Lval)) :-
	dupelim__replace_labels_lval(Lval0, Replmap, Lval).
dupelim__replace_labels_instr(restore_hp(Rval0), Replmap, restore_hp(Rval)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_instr(store_ticket(Lval0), Replmap, 
		store_ticket(Lval)) :-
	dupelim__replace_labels_lval(Lval0, Replmap, Lval).
dupelim__replace_labels_instr(reset_ticket(Rval0, Reason), Replmap, 
		reset_ticket(Rval, Reason)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_instr(discard_ticket, _, discard_ticket).
dupelim__replace_labels_instr(mark_ticket_stack(Lval0), Replmap, 
		mark_ticket_stack(Lval)) :-
	dupelim__replace_labels_lval(Lval0, Replmap, Lval).
dupelim__replace_labels_instr(discard_tickets_to(Rval0), Replmap, 
		discard_tickets_to(Rval)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_instr(incr_sp(Size, Msg), _, incr_sp(Size, Msg)).
dupelim__replace_labels_instr(decr_sp(Size), _, decr_sp(Size)).
dupelim__replace_labels_instr(pragma_c(A,B,C,D,E), _, pragma_c(A,B,C,D,E)).

:- pred dupelim__replace_labels_lval(lval, map(label, label), lval).
% :- mode dupelim__replace_labels_lval(di, in, uo) is det.
:- mode dupelim__replace_labels_lval(in, in, out) is det.

dupelim__replace_labels_lval(reg(RegType, RegNum), _, reg(RegType, RegNum)).
dupelim__replace_labels_lval(stackvar(N), _, stackvar(N)).
dupelim__replace_labels_lval(framevar(N), _, framevar(N)).
dupelim__replace_labels_lval(succip, _, succip).
dupelim__replace_labels_lval(maxfr, _, maxfr).
dupelim__replace_labels_lval(curfr, _, curfr).
dupelim__replace_labels_lval(succip(Rval0), Replmap, succip(Rval)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_lval(redoip(Rval0), Replmap, redoip(Rval)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_lval(succfr(Rval0), Replmap, succfr(Rval)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_lval(prevfr(Rval0), Replmap, prevfr(Rval)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_lval(hp, _, hp).
dupelim__replace_labels_lval(sp, _, sp).
dupelim__replace_labels_lval(field(Tag, Base0, Offset0), Replmap,
		field(Tag, Base, Offset)) :-
	dupelim__replace_labels_rval(Base0, Replmap, Base),
	dupelim__replace_labels_rval(Offset0, Replmap, Offset).
dupelim__replace_labels_lval(lvar(Var), _, lvar(Var)).
dupelim__replace_labels_lval(temp(Type, Num), _, temp(Type, Num)).
dupelim__replace_labels_lval(mem_ref(Rval0), Replmap, mem_ref(Rval)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).

:- pred dupelim__replace_labels_rval(rval, map(label, label), rval).
% :- mode dupelim__replace_labels_rval(di, in, uo) is det.
:- mode dupelim__replace_labels_rval(in, in, out) is det.

dupelim__replace_labels_rval(lval(Lval0), Replmap, lval(Lval)) :-
	dupelim__replace_labels_lval(Lval0, Replmap, Lval).
dupelim__replace_labels_rval(var(Var), _, var(Var)).
dupelim__replace_labels_rval(create(Tag, Rvals, Unique, N, Msg), _,
		create(Tag, Rvals, Unique, N, Msg)).
dupelim__replace_labels_rval(mkword(Tag, Rval0), Replmap, mkword(Tag, Rval)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_rval(const(Const0), Replmap, const(Const)) :-
	dupelim__replace_labels_rval_const(Const0, Replmap, Const).
dupelim__replace_labels_rval(unop(Op, Rval0), Replmap, unop(Op, Rval)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).
dupelim__replace_labels_rval(binop(Op, LRval0, RRval0), Replmap,
		binop(Op, LRval, RRval)) :-
	dupelim__replace_labels_rval(LRval0, Replmap, LRval),
	dupelim__replace_labels_rval(RRval0, Replmap, RRval).
dupelim__replace_labels_rval(mem_addr(MemRef0), Replmap, mem_addr(MemRef)) :-
	dupelim__replace_labels_mem_ref(MemRef0, Replmap, MemRef).

:- pred dupelim__replace_labels_mem_ref(mem_ref, map(label, label), mem_ref).
% :- mode dupelim__replace_labels_rval(di, in, uo) is det.
:- mode dupelim__replace_labels_mem_ref(in, in, out) is det.

dupelim__replace_labels_mem_ref(stackvar_ref(N), _, stackvar_ref(N)).
dupelim__replace_labels_mem_ref(framevar_ref(N), _, framevar_ref(N)).
dupelim__replace_labels_mem_ref(heap_ref(Rval0, Tag, N), Replmap,
		heap_ref(Rval, Tag, N)) :-
	dupelim__replace_labels_rval(Rval0, Replmap, Rval).

:- pred dupelim__replace_labels_rval_const(rval_const, map(label, label),
	rval_const).
% :- mode dupelim__replace_labels_rval_const(di, in, uo) is det.
:- mode dupelim__replace_labels_rval_const(in, in, out) is det.

dupelim__replace_labels_rval_const(true, _, true).
dupelim__replace_labels_rval_const(false, _, false).
dupelim__replace_labels_rval_const(int_const(N), _, int_const(N)).
dupelim__replace_labels_rval_const(float_const(N), _, float_const(N)).
dupelim__replace_labels_rval_const(string_const(S), _, string_const(S)).
dupelim__replace_labels_rval_const(code_addr_const(Addr0), Replmap,
		code_addr_const(Addr)) :-
	dupelim__replace_labels_code_addr(Addr0, Replmap, Addr).
dupelim__replace_labels_rval_const(data_addr_const(DataAddr), _,
		data_addr_const(DataAddr)).
dupelim__replace_labels_rval_const(label_entry(Label), _, label_entry(Label)).

:- pred dupelim__replace_labels_code_addr(code_addr, map(label, label),
	code_addr).
% :- mode dupelim__replace_labels_code_addr(di, in, uo) is det.
:- mode dupelim__replace_labels_code_addr(in, in, out) is det.

dupelim__replace_labels_code_addr(label(Label0), Replmap, label(Label)) :-
	dupelim__replace_labels_label(Label0, Replmap, Label).
dupelim__replace_labels_code_addr(imported(Proc), _, imported(Proc)).
dupelim__replace_labels_code_addr(succip, _, succip).
dupelim__replace_labels_code_addr(do_succeed(Last), _, do_succeed(Last)).
dupelim__replace_labels_code_addr(do_redo, _, do_redo).
dupelim__replace_labels_code_addr(do_fail, _, do_fail).
dupelim__replace_labels_code_addr(do_det_closure, _, do_det_closure).
dupelim__replace_labels_code_addr(do_semidet_closure, _, do_semidet_closure).
dupelim__replace_labels_code_addr(do_nondet_closure, _, do_nondet_closure).
dupelim__replace_labels_code_addr(do_not_reached, _, do_not_reached).

:- pred dupelim__replace_labels_label_list(list(label), map(label, label),
	list(label)).
% :- mode dupelim__replace_labels_label_list(di, in, uo) is det.
:- mode dupelim__replace_labels_label_list(in, in, out) is det.

dupelim__replace_labels_label_list([], _Replmap, []).
dupelim__replace_labels_label_list([Label0 | Labels0], Replmap,
		[Label | Labels]) :-
	dupelim__replace_labels_label(Label0, Replmap, Label),
	dupelim__replace_labels_label_list(Labels0, Replmap, Labels).

:- pred dupelim__replace_labels_label(label, map(label, label), label).
% :- mode dupelim__replace_labels_label(di, in, uo) is det.
:- mode dupelim__replace_labels_label(in, in, out) is det.

dupelim__replace_labels_label(Label0, Replmap, Label) :-
	( map__search(Replmap, Label0, NewLabel) ->
		Label = NewLabel
	;
		Label = Label0
	).

:- pred dupelim__condense(list(block), list(instruction)).
% :- mode dupelim__condense(di, uo) is det.
:- mode dupelim__condense(in, out) is det.

dupelim__condense([], []).
dupelim__condense([block(Label, Comment, Code) | Blocks1], Instrs) :-
	dupelim__condense(Blocks1, Instrs1),
	list__append([label(Label) - Comment | Code], Instrs1, Instrs).
