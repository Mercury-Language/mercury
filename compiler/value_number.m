%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% value_number.m - optimization of straight-line LLDS code.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module value_number.

:- interface.

:- import_module vn_type, llds, list, io.

	% Find straight-line code sequences and optimize them using
	% value numbering.

:- pred value_number__main(list(instruction), list(instruction),
	io__state, io__state).
:- mode value_number__main(in, out, di, uo) is det.

	% The main value numbering pass introduces references to temporary
	% variables whose values need be preserved only within an extended
	% basic block. The post_main pass looks for references to temporaries
	% and introduces block instructions whenever it sees them. These
	% block instructions go from the first reference to a temporary
	% to the end of its extended basic block.

:- pred value_number__post_main(list(instruction), list(instruction)).
:- mode value_number__post_main(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module vn_table, vn_block, vn_order, vn_flush, vn_temploc. 
:- import_module vn_cost, vn_debug, vn_util, opt_debug, labelopt.
:- import_module globals, options, peephole, livemap, code_util, opt_util.
:- import_module set, map, bimap, require, int, string, std_util, assoc_list.

	% We can't find out what variables are used by C code sequences,
	% so we don't optimize any predicates containing them.

value_number__main(Instrs0, Instrs) -->
	{ opt_util__get_prologue(Instrs0, ProcLabel, Comments, Instrs1) },
	{ opt_util__new_label_no(Instrs1, 1000, N0) },
	{ value_number__prepare_for_vn(Instrs1, ProcLabel, N0, Instrs2) },
	{ labelopt__build_usemap(Instrs2, Usemap) },
	{ livemap__build(Instrs2, Ccode, Livemap) },
	vn_debug__livemap_msg(Livemap),
	(
		{ Ccode = no },
		value_number__procedure(Instrs2, Livemap, Usemap, Instrs3),
		{ list__append(Comments, Instrs3, Instrs) }
	;
		% Don't perform value numbering if there is a c_code or a 
		% pragma_c in the instructions.
		{ Ccode = yes },
		{ Instrs = Instrs0 }
	).

%-----------------------------------------------------------------------------%

	% Instructions such as if_val(tag(r1) == 1 && field(1, r1, N) = X)
	% pose a problem for value numbering. The field reference will be
	% extracted into a register before the if, and this extraction will
	% cause an unaligned access if done before the tag test. Similar
	% problems can arise even if the code before the && does not contain
	% a tag operator, since this may have been applied earlier.
	%
	% By converting all boolean operations in if_vals into multiple
	% if_vals, we are preventing this from happening. 

:- pred value_number__prepare_for_vn(list(instruction), proc_label, int,
	list(instruction)).
:- mode value_number__prepare_for_vn(in, in, in, out) is det.

value_number__prepare_for_vn([], _, _, []).
value_number__prepare_for_vn([Instr0 | Instrs0], ProcLabel, N0, Instrs) :-
	Instr0 = Uinstr0 - _Comment,
	(
		Uinstr0 = if_val(Test, TrueAddr)
	->
		( Instrs0 = [label(FalseLabelPrime) - _ | _] ->
			FalseLabel = FalseLabelPrime,
			FalseAddr = label(FalseLabel),
			N1 = N0
		;
			FalseLabel = local(ProcLabel, N0),
			FalseAddr = label(FalseLabel),
			N1 is N0 + 1
		),
		value_number__breakup_complex_if(Test, TrueAddr, FalseAddr, FalseAddr,
			ProcLabel, N1, N2, IfInstrs),
		value_number__prepare_for_vn(Instrs0, ProcLabel, N2, Instrs1),
		( N1 = N0 ->
			list__append(IfInstrs, Instrs1, Instrs)
		;
			LabelInstr = label(FalseLabel) - "vn false label",
			list__append(IfInstrs, [LabelInstr | Instrs1], Instrs)
		)
	;
		value_number__prepare_for_vn(Instrs0, ProcLabel, N0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__breakup_complex_if(rval, code_addr, code_addr, code_addr,
	proc_label, int, int, list(instruction)).
:- mode value_number__breakup_complex_if(in, in, in, in, in, in, out, out) is det.

value_number__breakup_complex_if(Test, TrueAddr, FalseAddr, NextAddr,
		ProcLabel, N0, N, Instrs) :-
	( Test = binop(and, Test1, Test2) ->
		NewLabel = local(ProcLabel, N0),
		NewAddr = label(NewLabel),
		N1 is N0 + 1,
		value_number__breakup_complex_if(Test1, NewAddr, FalseAddr,
			NewAddr, ProcLabel, N1, N2, Instrs1),
		value_number__breakup_complex_if(Test2, TrueAddr, FalseAddr,
			NextAddr, ProcLabel, N2, N, Instrs2),
		list__append(Instrs1, [label(NewLabel) - "" | Instrs2], Instrs)
	; Test = binop(or, Test1, Test2) ->
		NewLabel = local(ProcLabel, N0),
		NewAddr = label(NewLabel),
		N1 is N0 + 1,
		value_number__breakup_complex_if(Test1, TrueAddr, NewAddr,
			NewAddr, ProcLabel, N1, N2, Instrs1),
		value_number__breakup_complex_if(Test2, TrueAddr, FalseAddr,
			NextAddr, ProcLabel, N2, N, Instrs2),
		list__append(Instrs1, [label(NewLabel) - "" | Instrs2], Instrs)
	; Test = unop(not, Test1) ->
		value_number__breakup_complex_if(Test1, FalseAddr, TrueAddr, NextAddr,
			ProcLabel, N0, N, Instrs)
	;
		N = N0,
		( NextAddr = FalseAddr ->
			Instrs = [if_val(Test, TrueAddr) - ""]
		; NextAddr = TrueAddr ->
			code_util__neg_rval(Test, NegTest),
			Instrs = [if_val(NegTest, FalseAddr) - ""]
		;
			Instrs = [if_val(Test, TrueAddr) - "",
				goto(FalseAddr) - ""]
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Optimize the code of a procedure.

:- pred value_number__procedure(list(instruction), livemap, set(label), list(instruction),
	io__state, io__state).
:- mode value_number__procedure(in, in, in, out, di, uo) is det.

value_number__procedure(Instrs0, Livemap, Usemap, OptInstrs) -->
	{ opt_util__new_label_no(Instrs0, 1000, LabelNo0) },
	{ opt_util__gather_comments(Instrs0, Comments, Instrs1) },
	{ vn_block__divide_into_blocks(Instrs1, Usemap, Blocks) },
	value_number__optimize_blocks(Blocks, Livemap, LabelNo0, OptBlocks0,
		[], RevTuples),
	globals__io_lookup_bool_option(pred_value_number, PredVn),
	( { PredVn = yes } ->
		{ list__reverse(RevTuples, Tuples) },
		value_number__process_parallel_tuples(Tuples, OptBlocks0, Livemap, OptBlocks1)
	;
		{ OptBlocks1 = OptBlocks0 }
	),
	{ list__condense([Comments | OptBlocks1], OptInstrs) }.

:- pred value_number__optimize_blocks(list(list(instruction)), livemap, int,
	list(list(instruction)), list(maybe(vn_ctrl_tuple)),
	list(maybe(vn_ctrl_tuple)), io__state, io__state).
% :- mode value_number__optimize_blocks(in, in, in, out, di, uo, di, uo) is det.
:- mode value_number__optimize_blocks(in, in, in, out, in, out, di, uo) is det.

value_number__optimize_blocks([], _, _, [], Tuples, Tuples) --> [].
value_number__optimize_blocks([Block0 | Blocks0], Livemap, LabelNo0, [Block | Blocks],
		RevTuples0, RevTuples) -->
	value_number__optimize_block(Block0, Livemap, [], LabelNo0, LabelNo1, Block,
		RevTuples0, RevTuples1),
	value_number__optimize_blocks(Blocks0, Livemap, LabelNo1, Blocks,
		RevTuples1, RevTuples).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred value_number__optimize_block(list(instruction), livemap, list(parentry),
	int, int, list(instruction), list(maybe(vn_ctrl_tuple)),
	list(maybe(vn_ctrl_tuple)), io__state, io__state).
:- mode value_number__optimize_block(in, in, in, in, out, out, in, out, di, uo) is det.

value_number__optimize_block(Instrs0, Livemap, ParEntries, LabelNo0, LabelNo, Instrs,
		RevTuples0, RevTuples) -->
	(
		{ list__reverse(Instrs0, RevInstrs) },
		{ RevInstrs = [LastInstr - _ | _] },
		{ opt_util__can_instr_fall_through(LastInstr, yes) }
	->
		% The blocks ends with a call to an erroneous procedure
		% and its never to be used return label
		{ Instrs = Instrs0 },
		{ LabelNo = LabelNo0 },
		{ RevTuples = [no | RevTuples0] }
	;
		value_number__optimize_fragment(Instrs0, Livemap, ParEntries, LabelNo0,
			Tuple, Instrs),
		{ Tuple = tuple(_, _, _, LabelNo, _) },
		{ RevTuples = [yes(Tuple) | RevTuples0] }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Optimize a tail fragment of a block. This may be the entire block,
	% or it may be a part of the block; we optimize parts of blocks if
	% a conflict prevents us from optimizing the whole block together.

:- pred value_number__optimize_fragment(list(instruction), livemap, list(parentry),
	int, vn_ctrl_tuple, list(instruction), io__state, io__state).
:- mode value_number__optimize_fragment(in, in, in, in, out, out, di, uo) is det.

value_number__optimize_fragment(Instrs0, Livemap, ParEntries, LabelNo0,
		Tuple, Instrs) -->
	globals__io_get_gc_method(GC),
	(
		% Value numbering currently combines multiple heap pointer
		% increments into a single heap pointer increment.  If we're
		% using conservative garbage collection, this would create
		% invalid code (unless the collector was compiled with
		% -DALL_INTERIOR_POINTERS, which would be very bad for
		% performance).  Hence, if GC=conservative we must not
		% perform value numbering on a block that contains more
		% than one heap pointer increment.

		{ GC = conservative ->
			opt_util__count_incr_hp(Instrs0, NumIncrs),
			NumIncrs < 2
		;
			true
		}
	->
		value_number__optimize_fragment_2(Instrs0, Livemap, ParEntries,
			LabelNo0, Tuple, Instrs)
	;
		{ Instrs = Instrs0 },
		{ vn_block__build_block_info(Instrs0, Livemap, ParEntries,	
			LabelNo0, _, _, _, Tuple) }
	).

:- pred value_number__optimize_fragment_2(list(instruction), livemap, list(parentry),
	int, vn_ctrl_tuple, list(instruction), io__state, io__state).
:- mode value_number__optimize_fragment_2(in, in, in, in, out, out, di, uo) is det.

value_number__optimize_fragment_2(Instrs0, Livemap, ParEntries, LabelNo0, Tuple, Instrs) -->
	( { Instrs0 = [Uinstr0Prime - _ | _] } ->
		{ Uinstr0 = Uinstr0Prime },
		vn_debug__fragment_msg(Uinstr0)
	;
		{ error("empty instruction sequence in value_number__optimize_fragment") }
	),
	{ Instrs1 = Instrs0 },	% historical relic
	{ vn_block__build_block_info(Instrs1, Livemap, ParEntries, LabelNo0,
		VnTables0, Liveset, SeenIncr, Tuple0) },
	{ Tuple0 = tuple(Ctrl, Ctrlmap, Flushmap, LabelNo, _Parmap) },

	{ vn_util__build_uses(Liveset, Ctrlmap, VnTables0, VnTables1) },

	vn_order__order(Liveset, VnTables1, SeenIncr, Ctrl, Ctrlmap, Flushmap, Res),
	(
		{ Res = success(VnTables2, Order) },
		{ value_number__max_real_regs(MaxRealRegs) },
		{ value_number__max_real_temps(MaxRealTemps) },
		{ vn_temploc__init_templocs(MaxRealRegs, MaxRealTemps,
			Liveset, VnTables2, Templocs0) },
		vn_flush__nodelist(Order, Ctrlmap, VnTables2, Templocs0,
			Instrs2),

		{ value_number__push_decr_sp_back(Instrs2, Instrs3) },
		{ value_number__push_incr_sp_forw(Instrs3, Instrs4) },
		{ value_number__push_livevals_back(Instrs4, Instrs5) },
		{ value_number__convert_back_modframe(Instrs5, Instrs6) },
		{ bimap__init(TeardownMap) },
		{ peephole__optimize(Instrs6, Instrs7, TeardownMap, no, _) },

		vn_debug__cost_header_msg("original code sequence"),
		vn_cost__block_cost(Instrs0, yes, OrigCost),
		vn_debug__cost_header_msg("new code sequence"),
		vn_cost__block_cost(Instrs7, yes, VnCost),
		vn_debug__cost_msg(OrigCost, VnCost),

		( { VnCost < OrigCost } ->
			{ vn_block__build_block_info(Instrs7, Livemap, ParEntries,
				LabelNo0, VnTables7, Liveset7, SeenIncr7,
				Tuple7) },
			{ value_number__verify_equivalence(Liveset, Liveset7,
				VnTables0, VnTables7, Problem) },
			( { SeenIncr \= SeenIncr7 } ->
				vn_debug__failure_msg(Uinstr0, "disagreement on SeenIncr"),
				{ Instrs = Instrs0 },
				{ Tuple = Tuple0 }
			; { Problem = yes(Msg) } ->
				vn_debug__failure_msg(Uinstr0, Msg),
				{ Instrs = Instrs0 },
				{ Tuple = Tuple0 }
			; { value_number__verify_tags(Instrs7) } ->
				{ Instrs = Instrs7 },
				{ Tuple = Tuple7 }
			;
				vn_debug__failure_msg(Uinstr0, "failure of tag check"),
				{ Instrs = Instrs0 },
				{ Tuple = Tuple0 }
			)
		;
			{ Instrs = Instrs0 },
			{ Tuple = Tuple0 }
		)
	;
		{ Res = failure(_LastLabel) },
		value_number__try_again(Instrs0, Livemap, LabelNo, Instrs),
		{ vn_block__build_block_info(Instrs0, Livemap, ParEntries,	
			LabelNo0, _, _, _, Tuple) }
	).

%-----------------------------------------------------------------------------%

:- pred value_number__try_again(list(instruction), livemap, int, list(instruction),
	io__state, io__state).
:- mode value_number__try_again(in, in, in, out, di, uo) is det.

value_number__try_again([], _, _, []) --> [].
value_number__try_again([Instr0 | Instrs0], Livemap, LabelNo0, Instrs) -->
	(
		{ Instr0 = Uinstr0 - _ },
		{
			Uinstr0 = if_val(_, _)
		;
			Uinstr0 = restore_hp(_)
		;
			Uinstr0 = mark_hp(_)
		}
	->
		vn_debug__restart_msg(Instr0),
		value_number__optimize_fragment(Instrs0, Livemap, [], LabelNo0,
			_, Instrs1),
		{ Instrs = [Instr0 | Instrs1] }
	;
		value_number__try_again(Instrs0, Livemap, LabelNo0, Instrs1),
		{ Instrs = [Instr0 | Instrs1] }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred value_number__verify_equivalence(vnlvalset, vnlvalset, vn_tables, vn_tables,
	maybe(string)).
:- mode value_number__verify_equivalence(in, in, in, in, out) is det.

value_number__verify_equivalence(Liveset0, Liveset7, VnTables0, VnTables7, Problem) :-
	set__to_sorted_list(Liveset0, Livevals0),
	set__to_sorted_list(Liveset7, Livevals7),
	map__init(InitVerifyMap0),
	value_number__make_verify_map_map(Livevals0, VnTables0, InitVerifyMap0, VerifyMap0,
		Problem0),
	map__init(InitVerifyMap7),
	value_number__make_verify_map_map(Livevals7, VnTables7, InitVerifyMap7, VerifyMap7,
		Problem7),
	( Problem0 = yes(_) ->
		Problem = Problem0
	; Problem7 = yes(_) ->
		Problem = Problem7
	;
		map__keys(VerifyMap0, Keys0),
		map__keys(VerifyMap7, Keys7),
		list__append(Keys0, Keys7, KeysDups),
		list__remove_dups(KeysDups, Keys),
		value_number__verify_correspondence(Keys, VerifyMap0, VerifyMap7, Problem)
	).

:- pred value_number__verify_correspondence(list(lval), map(lval, rval), map(lval, rval),
	maybe(string)).
:- mode value_number__verify_correspondence(in, in, in, out) is det.

value_number__verify_correspondence([], _, _, no).
value_number__verify_correspondence([Lval | Lvals], VerifyMap0, VerifyMap7, Problem) :-
	(
		map__search(VerifyMap0, Lval, Rval0),
		map__search(VerifyMap7, Lval, Rval7)
	->
		( Rval0 = Rval7 ->
			value_number__verify_correspondence(Lvals, VerifyMap0, VerifyMap7,
				Problem)
		;
			opt_debug__dump_lval(Lval, Lstr),
			string__append("disagreement on value of ", Lstr, Msg),
			Problem = yes(Msg)
		)
	;
		% We want to allow the optimization of blocks in which
		% the new version of the code does not have any mention at all
		% of some lval that appeared in dead code.
		\+ map__search(VerifyMap7, Lval, _)
	->
		value_number__verify_correspondence(Lvals, VerifyMap0, VerifyMap7,
			Problem)
	;
		opt_debug__dump_lval(Lval, Lstr),
		string__append("cannot find value of ", Lstr, Msg),
		Problem = yes(Msg)
	).

:- pred value_number__make_verify_map_map(list(vnlval), vn_tables,
	map(lval, rval), map(lval, rval), maybe(string)).
% :- mode value_number__make_verify_map_map(in, in, di, uo, out) is det.
:- mode value_number__make_verify_map_map(in, in, in, out, out) is det.

value_number__make_verify_map_map(LiveVnlvals, VnTables, VerifyMap0, VerifyMap, Problem) :-
	vn_table__get_all_vnrvals(Vnrvals, VnTables),
	value_number__make_verify_map_map_specials(Vnrvals, LiveVnlvals, Vnlvals),
	value_number__make_verify_map_map_2(Vnlvals, VnTables,
		VerifyMap0, VerifyMap, Problem).

:- pred value_number__make_verify_map_map_2(list(vnlval), vn_tables,
	map(lval, rval), map(lval, rval), maybe(string)).
% :- mode value_number__make_verify_map_map_2(in, in, di, uo, out) is det.
:- mode value_number__make_verify_map_map_2(in, in, in, out, out) is det.

value_number__make_verify_map_map_2([], _, VerifyMap, VerifyMap, no).
value_number__make_verify_map_map_2([Vnlval | Vnlvals], VnTables, VerifyMap0, VerifyMap,
		Problem) :-
	value_number__verify_lval(Vnlval, VnTables, Lval),
	( vn_table__search_desired_value(Vnlval, DesVn, VnTables) ->
		value_number__verify_value(DesVn, VnTables, Rval),
		map__set(VerifyMap0, Lval, Rval, VerifyMap1),
		value_number__make_verify_map_map_2(Vnlvals, VnTables,
			VerifyMap1, VerifyMap, Problem)
	;
		% opt_debug__dump_vnlval(Vnlval, Lstr),
		% string__append("cannot find desired value of ", Lstr, Msg),
		% Problem = yes(Msg),
		% VerifyMap = VerifyMap0	% should be ignored
		value_number__make_verify_map_map_2(Vnlvals, VnTables,
			VerifyMap0, VerifyMap, Problem)
	).

:- pred value_number__make_verify_map_map_specials(list(vnrval), list(vnlval), list(vnlval)).
% :- mode value_number__make_verify_map_map_specials(in, di, uo) is det.
:- mode value_number__make_verify_map_map_specials(in, in, out) is det.

value_number__make_verify_map_map_specials([], Vnlvals, Vnlvals).
value_number__make_verify_map_map_specials([Vnrval | Vnrvals], Vnlvals0, Vnlvals) :-
	(
		Vnrval = vn_origlval(Vnlval),
		vn_util__find_specials(Vnlval, Specials)
	->
		list__append(Vnlvals0, Specials, Vnlvals1)
	;
		Vnlvals1 = Vnlvals0
	),
	value_number__make_verify_map_map_specials(Vnrvals, Vnlvals1, Vnlvals).

:- pred value_number__verify_lval(vnlval, vn_tables, lval).
:- mode value_number__verify_lval(in, in, out) is det.

value_number__verify_lval(Vnlval, VnTables, Lval) :-
	vn_util__vnlval_access_vns(Vnlval, AccessVns),
	value_number__verify_values(AccessVns, VnTables, AccessRvals),
	( value_number__substitute_access_vns(Vnlval, AccessRvals, LvalPrime) ->
		Lval = LvalPrime
	;
		error("cannot substitute access vns in value_number__verify_lval")
	).

:- pred value_number__verify_values(list(vn), vn_tables, list(rval)).
:- mode value_number__verify_values(in, in, out) is det.

value_number__verify_values([], _VnTables, []).
value_number__verify_values([Vn | Vns], VnTables, [Rval | Rvals]) :-
	value_number__verify_value(Vn, VnTables, Rval),
	value_number__verify_values(Vns, VnTables, Rvals).

:- pred value_number__verify_value(vn, vn_tables, rval).
:- mode value_number__verify_value(in, in, out) is det.

value_number__verify_value(Vn, VnTables, Rval) :-
	vn_table__lookup_defn(Vn, Vnrval, "value_number__verify_value", VnTables),
	vn_util__find_sub_vns(Vnrval, SubVns),
	value_number__verify_values(SubVns, VnTables, SubRvals),
	( value_number__substitute_sub_vns(Vnrval, SubRvals, VnTables, RvalPrime) ->
		Rval = RvalPrime
	;
		error("cannot substitute sub vns in value_number__verify_value")
	).

:- pred value_number__substitute_access_vns(vnlval, list(rval), lval).
:- mode value_number__substitute_access_vns(in, in, out) is semidet.

value_number__substitute_access_vns(vn_reg(R), [], reg(R)).
value_number__substitute_access_vns(vn_stackvar(N), [], stackvar(N)).
value_number__substitute_access_vns(vn_framevar(N), [], framevar(N)).
value_number__substitute_access_vns(vn_succip, [], succip).
value_number__substitute_access_vns(vn_maxfr, [], maxfr).
value_number__substitute_access_vns(vn_curfr, [], curfr).
value_number__substitute_access_vns(vn_succfr(_), [R], succfr(R)).
value_number__substitute_access_vns(vn_prevfr(_), [R], prevfr(R)).
value_number__substitute_access_vns(vn_redoip(_), [R], redoip(R)).
value_number__substitute_access_vns(vn_succip(_), [R], succip(R)).
value_number__substitute_access_vns(vn_hp, [], hp).
value_number__substitute_access_vns(vn_sp, [], sp).
value_number__substitute_access_vns(vn_field(T, _, _), [R1, R2], field(T, R1, R2)).
value_number__substitute_access_vns(vn_temp(N), [], temp(N)).

:- pred value_number__substitute_sub_vns(vnrval, list(rval), vn_tables, rval).
:- mode value_number__substitute_sub_vns(in, in, in, out) is semidet.

value_number__substitute_sub_vns(vn_origlval(Vnlval), _, VnTables, lval(Lval)) :-
	value_number__verify_lval(Vnlval, VnTables, Lval).
value_number__substitute_sub_vns(vn_mkword(Tag, _), [R], _, mkword(Tag, R)).
value_number__substitute_sub_vns(vn_const(Const), [], _, const(Const)).
value_number__substitute_sub_vns(vn_create(T, A, L), [], _, create(T, A, L)).
value_number__substitute_sub_vns(vn_unop(Op, _), [R], _, unop(Op, R)).
value_number__substitute_sub_vns(vn_binop(Op, _, _), [R1, R2], _, binop(Op, R1, R2)).

%-----------------------------------------------------------------------------%

:- pred value_number__verify_tags(list(instruction)).
:- mode value_number__verify_tags(in) is semidet.

value_number__verify_tags(Instrs) :-
	list__reverse(Instrs, RevInstrs),
	set__init(NoDeref),
	set__init(Tested),
	value_number__verify_tags_2(RevInstrs, NoDeref, Tested).

:- pred value_number__verify_tags_2(list(instruction), set(rval), set(rval)).
:- mode value_number__verify_tags_2(in, in, in) is semidet.

value_number__verify_tags_2([], _, _).
value_number__verify_tags_2([Instr0 - _| RevInstrs], NoDeref0, Tested0) :-
	value_number__verify_tags_instr(Instr0, NoDeref0, NoDeref1, Tested0, Tested1),
	value_number__verify_tags_2(RevInstrs, NoDeref1, Tested1).

:- pred value_number__verify_tags_instr(instr, set(rval), set(rval),
	set(rval), set(rval)).
% :- mode value_number__verify_tags_instr(in, di, uo, di, uo) is semidet.
:- mode value_number__verify_tags_instr(in, in, out, in, out) is semidet.

value_number__verify_tags_instr(Instr, NoDeref0, NoDeref, Tested0, Tested) :-
	(
		Instr = comment(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = livevals(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = block(_, _),
		error("found block in value_number__verify_tags_instr")
	;
		Instr = assign(Lval, Rval),
		value_number__verify_tags_lval(Lval, NoDeref0),
		(
			set__member(lval(Lval), NoDeref0),
			Rval = lval(_)
		->
			set__insert(NoDeref0, Rval, NoDeref1)
		;
			NoDeref1 = NoDeref0
		),
		(
			set__member(lval(Lval), Tested0)
		->
			value_number__verify_tags_cond(Rval, NoDeref1, NoDeref2,
				Tested0, Tested)
		;
			NoDeref2 = NoDeref1,
			Tested = Tested0
		),
		% Lval on the right hand side refers to the old value;
		% the new value is the one that should not be tested.
		set__delete(NoDeref2, lval(Lval), NoDeref),
		value_number__verify_tags_rval(Rval, NoDeref)
	;
		Instr = call(_, _, _, _),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = call_closure(_, _, _),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = mkframe(_, _, _),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = modframe(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = label(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = goto(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = computed_goto(_, _),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = c_code(_),
		error("found c_code in value_number__verify_tags_instr")
	;
		Instr = if_val(Rval, _),
		value_number__verify_tags_cond(Rval, NoDeref0, NoDeref, Tested0, Tested)
	;
		Instr = incr_hp(Lval, _, Rval),
		value_number__verify_tags_lval(Lval, NoDeref0),
		value_number__verify_tags_rval(Rval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = mark_hp(Lval),
		value_number__verify_tags_lval(Lval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = restore_hp(Rval),
		value_number__verify_tags_rval(Rval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = incr_sp(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = decr_sp(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	).

:- pred value_number__verify_tags_lval(lval, set(rval)).
:- mode value_number__verify_tags_lval(in, in) is semidet.

value_number__verify_tags_lval(reg(_), _).
value_number__verify_tags_lval(stackvar(_), _).
value_number__verify_tags_lval(framevar(_), _).
value_number__verify_tags_lval(succip, _).
value_number__verify_tags_lval(maxfr, _).
value_number__verify_tags_lval(curfr, _).
value_number__verify_tags_lval(redoip(Rval), NoDeref) :-
	value_number__verify_tags_rval(Rval, NoDeref).
value_number__verify_tags_lval(succfr(Rval), NoDeref) :-
	value_number__verify_tags_rval(Rval, NoDeref).
value_number__verify_tags_lval(prevfr(Rval), NoDeref) :-
	value_number__verify_tags_rval(Rval, NoDeref).
value_number__verify_tags_lval(hp, _).
value_number__verify_tags_lval(sp, _).
value_number__verify_tags_lval(field(_, Rval1, Rval2), NoDeref) :-
	\+ set__member(Rval1, NoDeref),
	value_number__verify_tags_rval(Rval1, NoDeref),
	value_number__verify_tags_rval(Rval2, NoDeref).
value_number__verify_tags_lval(lvar(_), _) :-
	error("found lvar in value_number__verify_tags_lval").
value_number__verify_tags_lval(temp(_), _).

:- pred value_number__verify_tags_rval(rval, set(rval)).
:- mode value_number__verify_tags_rval(in, in) is semidet.

value_number__verify_tags_rval(lval(Lval), NoDeref) :-
	value_number__verify_tags_lval(Lval, NoDeref).
value_number__verify_tags_rval(var(_), _) :-
	error("found var in value_number__verify_tags_rval").
value_number__verify_tags_rval(create(_, _, _), _).
value_number__verify_tags_rval(mkword(_, Rval), NoDeref) :-
	value_number__verify_tags_rval(Rval, NoDeref).
value_number__verify_tags_rval(const(_), _).
value_number__verify_tags_rval(unop(_, Rval), NoDeref) :-
	value_number__verify_tags_rval(Rval, NoDeref).
value_number__verify_tags_rval(binop(_, Rval1, Rval2), NoDeref) :-
	value_number__verify_tags_rval(Rval1, NoDeref),
	value_number__verify_tags_rval(Rval2, NoDeref).

:- pred value_number__verify_tags_cond(rval, set(rval), set(rval), set(rval), set(rval)).
% :- mode value_number__verify_tags_cond(in, di, uo, di, uo) is semidet.
:- mode value_number__verify_tags_cond(in, in, out, in, out) is semidet.

value_number__verify_tags_cond(Cond, NoDeref0, NoDeref, Tested0, Tested) :-
	( Cond = binop(Binop, Rval1, Rval2) ->
		( ( Binop = (and) ; Binop = (or) ) ->
			value_number__verify_tags_cond(Rval2, NoDeref0, NoDeref1,
				Tested0, Tested1),
			value_number__verify_tags_cond(Rval1, NoDeref1, NoDeref,
				Tested1, Tested)
		;
			( Binop = eq ; Binop = ne ),
			% at most one of the next two ifs should be taken
			( value_number__verify_tags_is_base(Rval1, Base1) ->
				set__insert(NoDeref0, Base1, NoDeref1)
			;
				value_number__verify_tags_rval(Rval1, NoDeref0),
				NoDeref1 = NoDeref0
			),
			( value_number__verify_tags_is_base(Rval2, Base2) ->
				set__insert(NoDeref1, Base2, NoDeref)
			;
				value_number__verify_tags_rval(Rval2, NoDeref1),
				NoDeref = NoDeref1
			),
			Tested = Tested0
		)
	; Cond = unop(Unop, Rval1) ->
		( Unop = (not) ->
			value_number__verify_tags_cond(Rval1, NoDeref0, NoDeref,
				Tested0, Tested)
		;
			value_number__verify_tags_rval(Cond, NoDeref0),
			NoDeref = NoDeref0,
			set__insert(Tested0, Rval1, Tested)
		)
	;
		value_number__verify_tags_rval(Cond, NoDeref0),
		NoDeref = NoDeref0,
		set__insert(Tested0, Cond, Tested)
	).

:- pred value_number__verify_tags_is_base(rval, rval).
:- mode value_number__verify_tags_is_base(in, out) is semidet.

value_number__verify_tags_is_base(Rval, Base) :-
	(
		Rval = unop(tag, Base)
	;
		Rval = lval(_),
		Base = Rval
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred value_number__process_parallel_tuples(list(maybe(vn_ctrl_tuple)),
	list(list(instruction)), livemap, list(list(instruction)),
	io__state, io__state).
:- mode value_number__process_parallel_tuples(in, in, in, out, di, uo) is det.

value_number__process_parallel_tuples(Tuples0, Blocks0, Livemap, Blocks) -->
	{ list__length(Tuples0, TupleLength) },
	{ list__length(Blocks0, BlockLength) },
	{ TupleLength = BlockLength ->
		true
	;
		error("number of tuples and blocks differ")
	},
	value_number__process_parallel_tuples_2(Blocks0, Tuples0, Livemap, Blocks0,
		Blocks1, Extras),
	{ value_number__insert_new_blocks(Extras, Blocks1, Blocks) }.

:- pred value_number__insert_new_blocks(assoc_list(label, list(instruction)),
	list(list(instruction)), list(list(instruction))).
% :- mode value_number__insert_new_blocks(di, di, uo) is det.
:- mode value_number__insert_new_blocks(in, in, out) is det.

value_number__insert_new_blocks([], Blocks, Blocks).
value_number__insert_new_blocks([Label - Extra | Extras], Blocks0, Blocks) :-
	value_number__find_block_by_label(Blocks0, Label, Before, LabelBlock, After),
	list__condense([Before, [Extra, LabelBlock], After], Blocks1),
	value_number__insert_new_blocks(Extras, Blocks1, Blocks).

:- pred value_number__process_parallel_tuples_2(list(list(instruction)),
	list(maybe(vn_ctrl_tuple)), livemap, list(list(instruction)),
	list(list(instruction)), assoc_list(label, list(instruction)),
	io__state, io__state).
% :- mode value_number__process_parallel_tuples_2(di, in, in, in, uo, out, di, uo) is det.
:- mode value_number__process_parallel_tuples_2(in, in, in, in, out, out, di, uo) is det.

value_number__process_parallel_tuples_2([], _, _, _, [], []) --> [].
value_number__process_parallel_tuples_2([Block0 | Blocks0], MaybeTuples0, Livemap,
		AllBlocks, [Block | Blocks], Extras) -->
	{ MaybeTuples0 = [MaybeTuple0Prime | MaybeTuples1Prime] ->
		MaybeTuple0 = MaybeTuple0Prime,
		MaybeTuples1 = MaybeTuples1Prime
	;
		error("tuples and blocks not in sync")
	},
	(
		{ MaybeTuple0 = yes(Tuple) },
		value_number__process_parallel_tuple(Block0, Tuple, Livemap,
			AllBlocks, Block, Extras1)
	;
		{ MaybeTuple0 = no },
		{ Block = Block0 },
		{ Extras1 = [] }
	),
	value_number__process_parallel_tuples_2(Blocks0, MaybeTuples1, Livemap,
		AllBlocks, Blocks, Extras2),
	{ list__append(Extras1, Extras2, Extras) }.

:- pred value_number__process_parallel_tuple(list(instruction), vn_ctrl_tuple,
	livemap, list(list(instruction)), list(instruction),
	assoc_list(label, list(instruction)), io__state, io__state).
% :- mode value_number__process_parallel_tuple(di, in, in, in, uo, out, di, uo) is det.
:- mode value_number__process_parallel_tuple(in, in, in, in, out, out, di, uo) is det.

value_number__process_parallel_tuple(Block0, tuple(_, _, _, _, Parmap), Livemap,
		AllBlocks, Block, Extras) -->
	{ map__values(Parmap, ParList) },
	( { value_number__all_empty_lists(ParList) } ->
		{ Block = Block0 },
		{ Extras = [] }
	;
		value_number__process_parallel_nodes(ParList, Livemap,
			Block0, AllBlocks, Block, Extras)
	).

:- pred value_number__all_empty_lists(list(list(T))).
:- mode value_number__all_empty_lists(in) is semidet.

value_number__all_empty_lists([]).
value_number__all_empty_lists([[] | Lists]) :-
	value_number__all_empty_lists(Lists).

:- pred value_number__process_parallel_nodes(list(list(parallel)), livemap,
	list(instruction), list(list(instruction)), list(instruction),
	assoc_list(label, list(instruction)), io__state, io__state).
% :- mode value_number__process_parallel_nodes(in, in, di, in, uo, out, di, uo) is det.
:- mode value_number__process_parallel_nodes(in, in, in, in, out, out, di, uo) is det.

value_number__process_parallel_nodes([], _, Block, _, Block, []) --> [].
value_number__process_parallel_nodes([Par0 | Pars1], Livemap,
		Block0, AllBlocks, Block, Extras) -->
	{ vn_block__split_at_next_ctrl_instr(Block0, Start, NodeInstr, Block1) },
	value_number__process_parallels(Par0, Livemap, NodeInstr,
		NewNodeInstr, AllBlocks, Extras1),
	value_number__process_parallel_nodes(Pars1, Livemap,
		Block1, AllBlocks, Block2, Extras2),
	{ list__condense([Start, [NewNodeInstr], Block2], Block) },
	{ list__append(Extras1, Extras2, Extras) }.

:- pred value_number__process_parallels(list(parallel), livemap, instruction, instruction,
	list(list(instruction)), assoc_list(label, list(instruction)),
	io__state, io__state).
:- mode value_number__process_parallels(in, in, in, out, in, out, di, uo) is det.

value_number__process_parallels(Pars, Livemap, Instr0, Instr, AllBlocks, Extras) -->
	{ Instr0 = Uinstr0 - Comment },
	(
		{ Pars = [] }
	->
		{ Instr = Instr0 },
		{ Extras = []}
	;
		{ Uinstr0 = if_val(Rval, label(Label)) }
	->
		( { Pars = [Par] } ->
			( { Par = parallel(Label, _NewLabel, _ParEntries) } ->
				value_number__process_parallel(Par, Livemap, AllBlocks,
					FinalLabel, Extras),
				{ Instr = if_val(Rval, label(FinalLabel))
					- Comment }
			;
				{ error("wrong label in parallel for if_val") }
			)
		;
			{ error("more than one parallel for if_val") }
		)
	;
		{ Uinstr0 = goto(label(Label)) }
	->
		( { Pars = [Par] } ->
			( { Par = parallel(Label, _NewLabel, _ParEntries) } ->
				value_number__process_parallel(Par, Livemap, AllBlocks,
					FinalLabel, Extras),
				{ Instr = goto(label(FinalLabel)) - Comment }
			;
				{ error("wrong label in parallel for goto") }
			)
		;
			{ error("more than one parallel for goto") }
		)
	;
		{ Uinstr0 = computed_goto(Rval, Labels) }
	->
		value_number__process_parallel_list(Pars, Labels, Livemap, AllBlocks,
			FinalLabels, Extras),
		{ Instr = computed_goto(Rval, FinalLabels) - Comment }
	;
		{ Instr = Instr0 },
		{ Extras = [] }
	).

:- pred value_number__process_parallel_list(list(parallel), list(label),
	livemap, list(list(instruction)), list(label),
	assoc_list(label, list(instruction)), io__state, io__state).
:- mode value_number__process_parallel_list(in, in, in, in, out, out, di, uo) is det.

value_number__process_parallel_list([], _, _, _, [], []) --> [].
value_number__process_parallel_list([Par | Pars], OldLabels, Livemap, AllBlocks,
		[Label | Labels], Extras) -->
	{ Par = parallel(OldLabel, _, _) },
	{ OldLabels = [OldLabel | OldLabels1Prime] ->
		OldLabels1 = OldLabels1Prime
	;
		error("wrong label sequence in parallel for computed_goto")
	},
	value_number__process_parallel(Par, Livemap, AllBlocks, Label, Extras1),
	value_number__process_parallel_list(Pars, OldLabels1, Livemap,
		AllBlocks, Labels, Extras2),
	{ list__append(Extras1, Extras2, Extras) }.

:- pred value_number__process_parallel(parallel, livemap, list(list(instruction)),
	label, assoc_list(label, list(instruction)), io__state, io__state).
:- mode value_number__process_parallel(in, in, in, out, out, di, uo) is det.

value_number__process_parallel(Par, Livemap, AllBlocks, FinalLabel, Extras) -->
	vn_debug__parallel_msg(Par),
	{ Par = parallel(OldLabel, NewLabel, ParEntries) },
	{ value_number__find_block_by_label(AllBlocks, OldLabel, _, Block, _) },
	value_number__optimize_block(Block, Livemap, ParEntries, 2000, _,
		NewBlock0, [], _),
	vn_cost__block_cost(Block, no, OrigCost),
	vn_cost__block_cost(NewBlock0, no, ParCost),
	{
		ParCost < OrigCost
	->
		FinalLabel = NewLabel,
		( NewBlock0 = [label(OldLabel) - Comment | Rest] ->
			NewBlock = [label(NewLabel) - Comment | Rest],
			Extras = [OldLabel - NewBlock]
		;
			error("block starts with wrong label")
		)
	;
		FinalLabel = OldLabel,
		Extras = []
	}.

	% Given a list of blocks and a label, return the blocks before the
	% labelled block, the labelled block itself, and the following blocks.

:- pred value_number__find_block_by_label(list(list(instruction)), label,
	list(list(instruction)), list(instruction), list(list(instruction))).
% :- mode value_number__find_block_by_label(di, in, uo, uo, uo) is det.
:- mode value_number__find_block_by_label(in, in, out, out, out) is det.

value_number__find_block_by_label([], Label, _, _, _) :-
	opt_debug__dump_label(Label, L_str),
	string__append("Cannot find block with label ", L_str, Str),
	error(Str).
value_number__find_block_by_label([Block | Blocks], Label, Before, LabelBlock, After) :-
	( Block = [FirstInstr | _] ->
		( FirstInstr = label(Label) - _ ->
			Before = [],
			LabelBlock = Block,
			After = Blocks
		;
			value_number__find_block_by_label(Blocks, Label,
				Before0, LabelBlock, After),
			Before = [Block | Before0]
		)
	;
		error("found empty block")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred value_number__convert_back_modframe(list(instruction), list(instruction)).
:- mode value_number__convert_back_modframe(in, out) is det.

value_number__convert_back_modframe([], []).
value_number__convert_back_modframe([Instr0 | Instrs0], [Instr | Instrs]) :-
	value_number__convert_back_modframe(Instrs0, Instrs),
	(
		Instr0 = assign(redoip(lval(curfr)),
			const(address_const(Redoip))) - _
	->
		Instr = modframe(Redoip) - "recovered modframe"
	;
		Instr = Instr0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred value_number__push_decr_sp_back(list(instruction), list(instruction)).
% :- mode value_number__push_decr_sp_back(di, uo) is det.
:- mode value_number__push_decr_sp_back(in, out) is det.

value_number__push_decr_sp_back([], []).
value_number__push_decr_sp_back([Instr0 | Instrs0], Instrs) :-
	( Instr0 = decr_sp(N) - _ ->
		value_number__push_decr_sp_back_2(Instrs0, N, Instrs)
	;
		value_number__push_decr_sp_back(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_decr_sp_back_2(list(instruction), int, list(instruction)).
% :- mode value_number__push_decr_sp_back_2(di, in, uo) is det.
:- mode value_number__push_decr_sp_back_2(in, in, out) is det.

value_number__push_decr_sp_back_2([], N, [decr_sp(N) - ""]).
value_number__push_decr_sp_back_2([Instr0 | Instrs0], N, Instrs) :-
	Instr0 = Uinstr0 - _,
	value_number__boundary_instr(Uinstr0, Boundary),
	(
		Boundary = yes,
		Instrs = [decr_sp(N) - "", Instr0 | Instrs0],
		opt_util__block_refers_stackvars([Instr0 | Instrs], Ref),
		(
			Ref = yes,
			error("cannot push decr_sp back enough")
		;
			Ref = no
		)
	;
		Boundary = no,
		value_number__push_decr_sp_back_2(Instrs0, N, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_incr_sp_forw(list(instruction), list(instruction)).
% :- mode value_number__push_incr_sp_forw(di, uo) is det.
:- mode value_number__push_incr_sp_forw(in, out) is det.

value_number__push_incr_sp_forw(Instrs0, Instrs) :-
	list__reverse(Instrs0, Instrs1),
	value_number__push_incr_sp_forw_rev(Instrs1, MaybeFrameSize, Instrs2),
	(
		MaybeFrameSize = yes(N),
		value_number__push_save_succip_forw_rev(Instrs2, N, Instrs3)
	;
		MaybeFrameSize = no,
		Instrs3 = Instrs2
	),
	list__reverse(Instrs3, Instrs).

%-----------------------------------------------------------------------------%

:- pred value_number__push_incr_sp_forw_rev(list(instruction), maybe(int),
	list(instruction)).
% :- mode value_number__push_incr_sp_forw_rev(di, out, uo) is det.
:- mode value_number__push_incr_sp_forw_rev(in, out, out) is det.

value_number__push_incr_sp_forw_rev([], no, []).
value_number__push_incr_sp_forw_rev([Instr0 | Instrs0], MaybeFrameSize, Instrs) :-
	( Instr0 = incr_sp(N) - _ ->
		value_number__push_incr_sp_forw_rev_2(Instrs0, N, Instrs),
		MaybeFrameSize = yes(N)
	;
		value_number__push_incr_sp_forw_rev(Instrs0, MaybeFrameSize, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_incr_sp_forw_rev_2(list(instruction), int, list(instruction)).
% :- mode value_number__push_incr_sp_forw_rev_2(di, in, uo) is det.
:- mode value_number__push_incr_sp_forw_rev_2(in, in, out) is det.

value_number__push_incr_sp_forw_rev_2([], N, [incr_sp(N) - ""]).
value_number__push_incr_sp_forw_rev_2([Instr0 | Instrs0], N, Instrs) :-
	Instr0 = Uinstr0 - _,
	value_number__boundary_instr(Uinstr0, Boundary),
	(
		Boundary = yes,
		Instrs = [incr_sp(N) - "", Instr0 | Instrs0],
		opt_util__block_refers_stackvars([Instr0 | Instrs], Ref),
		(
			Ref = yes,
			error("cannot push incr_sp forward enough")
		;
			Ref = no
		)
	;
		Boundary = no,
		value_number__push_incr_sp_forw_rev_2(Instrs0, N, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred value_number__push_save_succip_forw_rev(list(instruction), int,
	list(instruction)).
% :- mode value_number__push_save_succip_forw_rev(di, in, uo) is det.
:- mode value_number__push_save_succip_forw_rev(in, in, out) is det.

value_number__push_save_succip_forw_rev([], _, []).
value_number__push_save_succip_forw_rev([Instr0 | Instrs0], FrameSize, Instrs) :-
	( Instr0 = assign(stackvar(FrameSize), lval(succip)) - _ ->
		value_number__push_save_succip_forw_rev_2(Instrs0, FrameSize, Instrs)
	;
		value_number__push_save_succip_forw_rev(Instrs0, FrameSize, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_save_succip_forw_rev_2(list(instruction), int,
	list(instruction)).
% :- mode value_number__push_save_succip_forw_rev_2(di, in, uo) is det.
:- mode value_number__push_save_succip_forw_rev_2(in, in, out) is det.

value_number__push_save_succip_forw_rev_2([], _FrameSize, _) :-
	error("succip save without incr_sp").
value_number__push_save_succip_forw_rev_2([Instr0 | Instrs0], FrameSize, Instrs) :-
	Instr0 = Uinstr0 - _,
	( Uinstr0 = incr_sp(FrameSize) ->
		Instrs = [assign(stackvar(FrameSize), lval(succip)) - "",
			Instr0 | Instrs0]
	;
		value_number__push_save_succip_forw_rev_2(Instrs0, FrameSize, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred value_number__push_livevals_back(list(instruction), list(instruction)).
% :- mode value_number__push_livevals_back(di, uo) is det.
:- mode value_number__push_livevals_back(in, out) is det.

value_number__push_livevals_back([], []).
value_number__push_livevals_back([Instr0 | Instrs0], Instrs) :-
	( Instr0 = livevals(Livevals) - _ ->
		value_number__push_livevals_back_2(Instrs0, Livevals, Instrs)
	;
		value_number__push_livevals_back(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_livevals_back_2(list(instruction), lvalset, list(instruction)).
% :- mode value_number__push_livevals_back_2(di, in, uo) is det.
:- mode value_number__push_livevals_back_2(in, in, out) is det.

value_number__push_livevals_back_2([], Livevals, [livevals(Livevals) - ""]).
value_number__push_livevals_back_2([Instr0 | Instrs0], Livevals, Instrs) :-
	Instr0 = Uinstr0 - _,
	value_number__boundary_instr(Uinstr0, Boundary),
	opt_util__can_instr_branch_away(Uinstr0, CanBranch),
	( Boundary = yes, CanBranch = yes ->
		Instrs = [livevals(Livevals) - "", Instr0 | Instrs0]
	;
		value_number__push_livevals_back_2(Instrs0, Livevals, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred value_number__boundary_instr(instr, bool).
:- mode value_number__boundary_instr(in, out) is det.

value_number__boundary_instr(comment(_), no).
value_number__boundary_instr(livevals(_), no).
value_number__boundary_instr(block(_, _), no).
value_number__boundary_instr(assign(_,_), no).
value_number__boundary_instr(call(_, _, _, _), yes).
value_number__boundary_instr(call_closure(_, _, _), yes).
value_number__boundary_instr(mkframe(_, _, _), yes).
value_number__boundary_instr(modframe(_), yes).
value_number__boundary_instr(label(_), yes).
value_number__boundary_instr(goto(_), yes).
value_number__boundary_instr(computed_goto(_, _), yes).
value_number__boundary_instr(c_code(_), yes).
value_number__boundary_instr(if_val(_, _), yes).
value_number__boundary_instr(incr_hp(_, _, _), no).
value_number__boundary_instr(mark_hp(_), no).
value_number__boundary_instr(restore_hp(_), no).
value_number__boundary_instr(incr_sp(_), yes).
value_number__boundary_instr(decr_sp(_), yes).
value_number__boundary_instr(pragma_c(_, _, _, _), yes).

%-----------------------------------------------------------------------------%

	% The best values of these two parameters are platform dependent.

:- pred value_number__max_real_regs(int).
:- mode value_number__max_real_regs(out) is det.

value_number__max_real_regs(5).

:- pred value_number__max_real_temps(int).
:- mode value_number__max_real_temps(out) is det.

value_number__max_real_temps(5).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

value_number__post_main(Instrs0, Instrs) :-
	value_number__post_main_2(Instrs0, 0, [], Instrs).

	% N is the number of the highest numbered temp variable seen so far;
	% N = 0 means we haven't seen any temp variables. RevSofar is a
	% reversed list of instructions starting with the first instruction
	% in this block that accesses a temp variable. Invariant: RevSofar
	% is always empty if N = 0.

:- pred value_number__post_main_2(list(instruction), int, list(instruction),
	list(instruction)).
:- mode value_number__post_main_2(in, in, in, out) is det.

value_number__post_main_2([], N, RevSofar, []) :-
	( RevSofar = [_|_] ->
		error("procedure ends with fallthrough")
	; N > 0 ->
		error("procedure ends without closing block")
	;
		true
	).
value_number__post_main_2([Instr0 | Instrs0], N0, RevSofar, Instrs) :-
	Instr0 = Uinstr0 - _Comment0,
	opt_util__count_temps_instr(Uinstr0, N0, N1),
	(
		N1 > 0
	->
		(
			opt_util__can_instr_fall_through(Uinstr0, no)
		->
			list__reverse([Instr0 | RevSofar], BlockInstrs),
			value_number__post_main_2(Instrs0, 0, [], Instrs1),
			Instrs = [block(N1, BlockInstrs) - "" | Instrs1]
		;
			Uinstr0 = label(_)
		->
			list__reverse(RevSofar, BlockInstrs),
			value_number__post_main_2(Instrs0, 0, [], Instrs1),
			Instrs = [block(N1, BlockInstrs) - "", Instr0 | Instrs1]
		;
			value_number__post_main_2(Instrs0,
				N1, [Instr0 | RevSofar], Instrs)
		)
	;
		value_number__post_main_2(Instrs0, 0, [], Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
