%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: livemap.m
%
% Main author: zs.
%
% This module builds up a map that gives the set of live lvals at each label.

%-----------------------------------------------------------------------------%

:- module ll_backend__livemap.

:- interface.

:- import_module list, set, map, std_util.
:- import_module ll_backend__llds.

:- type livemap		==	map(label, lvalset).
:- type lvalset		==	set(lval).

	% Given a list of instructions defining a procedure, return a map
	% giving the set of live non-field lvals at each label.
	%
	% We can compute this set only if the procedure contains no C code.

:- pred livemap__build(list(instruction)::in, maybe(livemap)::out) is det.

:- implementation.

:- import_module ll_backend__opt_util.
:- import_module require, string, bool.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The method we follow is a backward scan of the instruction list,
	% keeping track of the set of live lvals as we go. We update this set
	% at each instruction. When we get to a label, we know that this set
	% of lvals is live at that label.
	%
	% At instructions that can branch away, every lval that is live at
	% any possible target is live before that instruction. Since some
	% branches may be backward branches, we may not have seen the branch
	% target when we process the branch. Therefore we have to repeat the
	% scan, this time with more knowledge about more labels, until we
	% get to a fixpoint.

livemap__build(Instrs, MaybeLivemap) :-
	map__init(Livemap0),
	list__reverse(Instrs, BackInstrs),
	livemap__build_2(BackInstrs, Livemap0, MaybeLivemap).

:- pred livemap__build_2(list(instruction)::in, livemap::in,
	maybe(livemap)::out) is det.

livemap__build_2(Backinstrs, Livemap0, MaybeLivemap) :-
	set__init(Livevals0),
	livemap__build_livemap(Backinstrs, Livevals0, no, DontValueNumber,
		Livemap0, Livemap1),
	( DontValueNumber = yes ->
		MaybeLivemap = no
	; livemap__equal_livemaps(Livemap0, Livemap1) ->
		MaybeLivemap = yes(Livemap1)
	;
		livemap__build_2(Backinstrs, Livemap1, MaybeLivemap)
	).

	% Check whether the two livemaps agree on the set of live lvals
	% at every label. They must agree on the set of labels as well.
	% This is important. Livemap1 will be empty in the first call,
	% so agreement only on the set of labels in Livemap1 is useless.
	% The domain of Livemap2 should always be every label in the procedure.
	% as should the domain of Livemap1 in every call after the first.

:- pred livemap__equal_livemaps(livemap::in, livemap::in) is semidet.

livemap__equal_livemaps(Livemap1, Livemap2) :-
	map__keys(Livemap1, Labels),
	map__keys(Livemap2, Labels),
	livemap__equal_livemaps_keys(Labels, Livemap1, Livemap2).

:- pred livemap__equal_livemaps_keys(list(label)::in, livemap::in, livemap::in)
	is semidet.

livemap__equal_livemaps_keys([], _Livemap1, _Livemap2).
livemap__equal_livemaps_keys([Label | Labels], Livemap1, Livemap2) :-
	map__lookup(Livemap1, Label, Liveset1),
	map__lookup(Livemap2, Label, Liveset2),
	set__equal(Liveset1, Liveset2),
	livemap__equal_livemaps_keys(Labels, Livemap1, Livemap2).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Build up a map of what lvals are live at each label.
	% The input instruction sequence is reversed.

:- pred livemap__build_livemap(list(instruction)::in, lvalset::in,
	bool::in, bool::out, livemap::in, livemap::out) is det.

livemap__build_livemap([], _, DontValueNumber, DontValueNumber,
		Livemap, Livemap).
livemap__build_livemap([Instr0 | Instrs0], Livevals0,
		DontValueNumber0, DontValueNumber, Livemap0, Livemap) :-
	livemap__build_livemap_instr(Instr0, Instrs0, Instrs1,
		Livevals0, Livevals1, DontValueNumber0, DontValueNumber1,
		Livemap0, Livemap1),
	livemap__build_livemap(Instrs1, Livevals1,
		DontValueNumber1, DontValueNumber, Livemap1, Livemap).

:- pred livemap__build_livemap_instr(instruction::in, list(instruction)::in,
	list(instruction)::out, lvalset::in, lvalset::out,
	bool::in, bool::out, livemap::in, livemap::out) is det.

livemap__build_livemap_instr(Instr0, Instrs0, Instrs,
		Livevals0, Livevals, DontValueNumber0, DontValueNumber,
		Livemap0, Livemap) :-
	Instr0 = Uinstr0 - _,
	(
		Uinstr0 = comment(_),
		Livemap = Livemap0,
		Livevals = Livevals0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = livevals(_),
		error("livevals found in backward scan in build_livemap")
	;
		Uinstr0 = block(_, _, _),
		error("block found in backward scan in build_livemap")
	;
		Uinstr0 = assign(Lval, Rval),

		% Make dead the variable assigned, but make any variables
		% needed to access it live. Make the variables in the assigned
		% expression live as well.
		% The deletion has to be done first. If the assigned-to lval
		% appears on the right hand side as well as the left, then we
		% want make_live to put it back into the liveval set.

		set__delete(Livevals0, Lval, Livevals1),
		opt_util__lval_access_rvals(Lval, Rvals),
		livemap__make_live_in_rvals([Rval | Rvals], Livevals1,
			Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = call(_, _, _, _, _, _),
		livemap__look_for_livevals(Instrs0, Instrs,
			Livevals0, Livevals, "call", yes, _),
		Livemap = Livemap0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = mkframe(_, _),
		Livemap = Livemap0,
		Livevals = Livevals0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = label(Label),
		map__set(Livemap0, Label, Livevals0, Livemap),
		Livevals = Livevals0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = goto(CodeAddr),
		opt_util__livevals_addr(CodeAddr, LivevalsNeeded),
		livemap__look_for_livevals(Instrs0, Instrs,
			Livevals0, Livevals1, "goto", LivevalsNeeded, Found),
		( Found = yes ->
			Livevals3 = Livevals1
		; CodeAddr = label(Label) ->
			set__init(Livevals2),
			livemap__insert_label_livevals([Label],
				Livemap0, Livevals2, Livevals3)
		;
			( CodeAddr = do_redo
			; CodeAddr = do_fail
			; CodeAddr = do_not_reached
			)
		->
			Livevals3 = Livevals1
		;
			error("unknown label type in build_livemap")
		),
		livemap__special_code_addr(CodeAddr, MaybeSpecial),
		( MaybeSpecial = yes(Special) ->
			set__insert(Livevals3, Special, Livevals)
		;
			Livevals = Livevals3
		),
		Livemap = Livemap0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = computed_goto(Rval, Labels),
		set__init(Livevals1),
		livemap__make_live_in_rvals([Rval], Livevals1, Livevals2),
		livemap__insert_label_livevals(Labels, Livemap0,
			Livevals2, Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = if_val(Rval, CodeAddr),
		livemap__look_for_livevals(Instrs0, Instrs,
			Livevals0, Livevals1, "if_val", no, Found),
		(
			Found = yes,
			% This if_val was put here by middle_rec.
			% We must make sure that the locations mentioned
			% in the livevals annotation become live,
			% since they will be needed at CodeAddr.
			% The locations in Livevals0 may be needed
			% in the fall-through continuation.
			set__union(Livevals0, Livevals1, Livevals3)
		;
			Found = no,
			livemap__make_live_in_rvals([Rval],
				Livevals1, Livevals2),
			( CodeAddr = label(Label) ->
				livemap__insert_label_livevals([Label],
					Livemap0, Livevals2, Livevals3)
			;	
				Livevals3 = Livevals2
			)
		),
		livemap__special_code_addr(CodeAddr, MaybeSpecial),
		( MaybeSpecial = yes(Special) ->
			set__insert(Livevals3, Special, Livevals)
		;
			Livevals = Livevals3
		),
		Livemap = Livemap0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = incr_hp(Lval, _, Rval, _),

		% Make dead the variable assigned, but make any variables
		% needed to access it live. Make the variables in the size
		% expression live as well.
		% The use of the size rval occurs after the assignment
		% to lval, but the two should never have any variables in
		% common. This is why doing the deletion first works.

		set__delete(Livevals0, Lval, Livevals1),
		opt_util__lval_access_rvals(Lval, Rvals),
		livemap__make_live_in_rvals([Rval | Rvals],
			Livevals1, Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = mark_hp(Lval),
		set__delete(Livevals0, Lval, Livevals1),
		opt_util__lval_access_rvals(Lval, Rvals),
		livemap__make_live_in_rvals(Rvals, Livevals1, Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = restore_hp(Rval),
		livemap__make_live_in_rvals([Rval], Livevals0, Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = free_heap(Rval),
		livemap__make_live_in_rvals([Rval], Livevals0, Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = store_ticket(Lval),
		set__delete(Livevals0, Lval, Livevals1),
		opt_util__lval_access_rvals(Lval, Rvals),
		livemap__make_live_in_rvals(Rvals, Livevals1, Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = reset_ticket(Rval, _Reason),
		livemap__make_live_in_rval(Rval, Livevals0, Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = discard_ticket,
		Livevals = Livevals0,
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = prune_ticket,
		Livevals = Livevals0,
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = mark_ticket_stack(Lval),
		set__delete(Livevals0, Lval, Livevals1),
		opt_util__lval_access_rvals(Lval, Rvals),
		livemap__make_live_in_rvals(Rvals, Livevals1, Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = prune_tickets_to(Rval),
		livemap__make_live_in_rval(Rval, Livevals0, Livevals),
		Livemap = Livemap0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = incr_sp(_, _),
		Livemap = Livemap0,
		Livevals = Livevals0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = decr_sp(_),
		Livemap = Livemap0,
		Livevals = Livevals0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		Uinstr0 = init_sync_term(_, _),
		Livemap = Livemap0,
		Livevals = Livevals0,
		Instrs = Instrs0,
		DontValueNumber = DontValueNumber0
	;
		% XXX Value numbering doesn't handle fork [yet] so
		% set DontValueNumber to yes.
		Uinstr0 = fork(_, _, _),
		Livemap = Livemap0,
		Livevals = Livevals0,
		Instrs = Instrs0,
		DontValueNumber = yes
	;
		% XXX Value numbering doesn't handle join_and_terminate [yet] so
		% set DontValueNumber to yes.
		Uinstr0 = join_and_terminate(_),
		Livemap = Livemap0,
		Livevals = Livevals0,
		Instrs = Instrs0,
		DontValueNumber = yes
	;
		% XXX Value numbering doesn't handle join_and_continue [yet] so
		% set DontValueNumber to yes.
		Uinstr0 = join_and_continue(_, _),
		Livemap = Livemap0,
		Livevals = Livevals0,
		Instrs = Instrs0,
		DontValueNumber = yes
	;
		Uinstr0 = c_code(_, LiveLvalInfo),
		livemap__build_live_lval_info(LiveLvalInfo,
			Livevals0, Livevals,
			DontValueNumber0, DontValueNumber),
		Livemap = Livemap0,
		Instrs = Instrs0
	;
		Uinstr0 = pragma_c(_, Components, _, _, _, _, _, _),
		livemap__build_livemap_pragma_components(Components,
			Livevals0, Livevals,
			DontValueNumber0, DontValueNumber),
		Livemap = Livemap0,
		Instrs = Instrs0
	).

:- pred livemap__build_livemap_pragma_components(list(pragma_c_component)::in,
	lvalset::in, lvalset::out, bool::in, bool::out) is det.

livemap__build_livemap_pragma_components([], Livevals, Livevals,
		DontValueNumber, DontValueNumber).
livemap__build_livemap_pragma_components([Component | Components],
		Livevals0, Livevals, DontValueNumber0, DontValueNumber) :-
	(
		Component = pragma_c_inputs(Inputs),
		livemap__build_livemap_pragma_inputs(Inputs,
			Livevals0, Livevals1),
		DontValueNumber1 = DontValueNumber0
	;
		Component = pragma_c_outputs(_),
		Livevals1 = Livevals0,
		DontValueNumber1 = DontValueNumber0
	;
		Component = pragma_c_user_code(_, _),
		Livevals1 = Livevals0,
		DontValueNumber1 = yes
	;
		Component = pragma_c_raw_code(_, LiveLvalInfo),
		livemap__build_live_lval_info(LiveLvalInfo,
			Livevals0, Livevals1,
			DontValueNumber0, DontValueNumber1)
	;
		Component = pragma_c_fail_to(_),
		Livevals1 = Livevals0,
		DontValueNumber1 = DontValueNumber0
	;
		Component = pragma_c_noop,
		Livevals1 = Livevals0,
		DontValueNumber1 = DontValueNumber0
	),
	livemap__build_livemap_pragma_components(Components,
		Livevals1, Livevals, DontValueNumber1, DontValueNumber).

:- pred livemap__build_live_lval_info(c_code_live_lvals::in,
	lvalset::in, lvalset::out, bool::in, bool::out) is det.

livemap__build_live_lval_info(no_live_lvals_info,
		Livevals, Livevals, _, yes).
livemap__build_live_lval_info(live_lvals_info(LiveLvalSet),
		Livevals0, Livevals, DontValueNumber, DontValueNumber) :-
	set__to_sorted_list(LiveLvalSet, LiveLvals),
	livemap__insert_proper_livevals(LiveLvals, Livevals0, Livevals).

:- pred livemap__build_livemap_pragma_inputs(list(pragma_c_input)::in,
	lvalset::in, lvalset::out) is det.

livemap__build_livemap_pragma_inputs([], Livevals, Livevals).
livemap__build_livemap_pragma_inputs([Input | Inputs], Livevals0, Livevals) :-
	Input = pragma_c_input(_, _, Rval, _),
	( Rval = lval(Lval) ->
		livemap__insert_proper_liveval(Lval, Livevals0, Livevals1)
	;
		Livevals1 = Livevals0
	),
	livemap__build_livemap_pragma_inputs(Inputs, Livevals1, Livevals).

:- pred livemap__look_for_livevals(list(instruction)::in,
	list(instruction)::out, lvalset::in, lvalset::out, string::in,
	bool::in, bool::out) is det.

livemap__look_for_livevals(Instrs0, Instrs, Livevals0, Livevals,
		Site, Compulsory, Found) :-
	opt_util__skip_comments(Instrs0, Instrs1),
	( Instrs1 = [livevals(Livevals1) - _ | Instrs2] ->
		livemap__filter_livevals(Livevals1, Livevals),
		Instrs = Instrs2,
		Found = yes
	; Compulsory = yes ->
		string__append(Site, " not preceded by livevals", Msg),
		error(Msg)
	;
		Instrs = Instrs1,
		Livevals = Livevals0,
		Found = no
	).

	% What lval (if any) is consulted when we branch to a code address?

:- pred livemap__special_code_addr(code_addr::in, maybe(lval)::out) is det.

livemap__special_code_addr(label(_), no).
livemap__special_code_addr(imported(_), no).
livemap__special_code_addr(succip, yes(succip)).
livemap__special_code_addr(do_succeed(_), yes(succip(lval(curfr)))).
livemap__special_code_addr(do_redo, yes(redoip(lval(maxfr)))).
livemap__special_code_addr(do_trace_redo_fail_shallow, no).
livemap__special_code_addr(do_trace_redo_fail_deep, no).
livemap__special_code_addr(do_fail, no).
livemap__special_code_addr(do_call_closure, no).
livemap__special_code_addr(do_call_class_method, no).
livemap__special_code_addr(do_det_aditi_call, no).
livemap__special_code_addr(do_semidet_aditi_call, no).
livemap__special_code_addr(do_nondet_aditi_call, no).
livemap__special_code_addr(do_aditi_insert, no).
livemap__special_code_addr(do_aditi_delete, no).
livemap__special_code_addr(do_aditi_bulk_insert, no).
livemap__special_code_addr(do_aditi_bulk_delete, no).
livemap__special_code_addr(do_aditi_bulk_modify, no).
livemap__special_code_addr(do_not_reached, no).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred livemap__make_live_in_rvals(list(rval)::in, lvalset::in, lvalset::out)
	is det.

livemap__make_live_in_rvals([], Live, Live).
livemap__make_live_in_rvals([Rval | Rvals], Live0, Live) :-
	livemap__make_live_in_rval(Rval, Live0, Live1),
	livemap__make_live_in_rvals(Rvals, Live1, Live).

	% Set all lvals found in this rval to live, with the exception of
	% fields, since they are treated specially (the later stages consider
	% them to be live even if they are not explicitly in the live set).

:- pred livemap__make_live_in_rval(rval::in, lvalset::in, lvalset::out) is det.

livemap__make_live_in_rval(lval(Lval), Live0, Live) :-
	% XXX maybe we should treat mem_refs the same way as field refs
	( Lval = field(_, _, _) ->
		Live1 = Live0
	;
		set__insert(Live0, Lval, Live1)
	),
	opt_util__lval_access_rvals(Lval, AccessRvals),
	livemap__make_live_in_rvals(AccessRvals, Live1, Live).
livemap__make_live_in_rval(create(_, _, _, _, _, _, _), Live, Live).
	% All terms inside creates in the optimizer must be static.
livemap__make_live_in_rval(mkword(_, Rval), Live0, Live) :-
	livemap__make_live_in_rval(Rval, Live0, Live).
livemap__make_live_in_rval(const(_), Live, Live).
livemap__make_live_in_rval(unop(_, Rval), Live0, Live) :-
	livemap__make_live_in_rval(Rval, Live0, Live).
livemap__make_live_in_rval(binop(_, Rval1, Rval2), Live0, Live) :-
	livemap__make_live_in_rval(Rval1, Live0, Live1),
	livemap__make_live_in_rval(Rval2, Live1, Live).
livemap__make_live_in_rval(var(_), _, _) :-
	error("var rval should not propagate to the optimizer").
livemap__make_live_in_rval(mem_addr(MemRef), Live0, Live) :-
	livemap__make_live_in_mem_ref(MemRef, Live0, Live).

:- pred livemap__make_live_in_mem_ref(mem_ref::in, lvalset::in, lvalset::out)
	is det.

livemap__make_live_in_mem_ref(stackvar_ref(_), Live, Live).
livemap__make_live_in_mem_ref(framevar_ref(_), Live, Live).
livemap__make_live_in_mem_ref(heap_ref(Rval, _, _), Live0, Live) :-
	livemap__make_live_in_rval(Rval, Live0, Live).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred livemap__filter_livevals(lvalset::in, lvalset::out) is det.

livemap__filter_livevals(Livevals0, Livevals) :-
	set__to_sorted_list(Livevals0, Livelist),
	set__init(Livevals1),
	livemap__insert_proper_livevals(Livelist, Livevals1, Livevals).

:- pred livemap__insert_label_livevals(list(label)::in, livemap::in,
	lvalset::in, lvalset::out) is det.

livemap__insert_label_livevals([], _, Livevals, Livevals).
livemap__insert_label_livevals([Label | Labels], Livemap, Livevals0, Livevals)
		:-
	( map__search(Livemap, Label, LabelLivevals) ->
		set__to_sorted_list(LabelLivevals, Livelist),
		livemap__insert_proper_livevals(Livelist, Livevals0, Livevals1)
	;
		Livevals1 = Livevals0
	),
	livemap__insert_label_livevals(Labels, Livemap, Livevals1, Livevals).

:- pred livemap__insert_proper_livevals(list(lval)::in, lvalset::in,
	lvalset::out) is det.

livemap__insert_proper_livevals([], Livevals, Livevals).
livemap__insert_proper_livevals([Live | Livelist], Livevals0, Livevals) :-
	livemap__insert_proper_liveval(Live, Livevals0, Livevals1),
	livemap__insert_proper_livevals(Livelist, Livevals1, Livevals).

	% Don't insert references to locations on the heap.

:- pred livemap__insert_proper_liveval(lval::in, lvalset::in, lvalset::out)
	is det.

livemap__insert_proper_liveval(Live, Livevals0, Livevals) :-
	( Live = field(_, _, _) ->
		Livevals = Livevals0
	;
		set__insert(Livevals0, Live, Livevals)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
