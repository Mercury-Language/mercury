%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: use_local_vars.m
%
% Author: zs.
%
% This module implements an LLDS->LLDS transformation that optimizes the
% sequence of instructions in a procedure body by replacing references to
% relatively expensive locations: fake registers (Mercury abstract machine
% registers that are not mapped to machine registers) or stack slots with
% references to cheaper locations: local variables in C blocks, which should
% be mapped to machine registers by the C compiler. The C blocks should be
% introduced later by wrap_blocks.m, possibly after the LLDS code has been
% transformed further. Wrap_blocks will know what local variables to declare
% in each block by looking for the temp(_, _) lvals that represent those local
% variables.
%
% This module looks for three patterns. The first is
%
%	<instruction that defines a fake register>
%	<instructions that use and possibly define the fake register>
%	<end of basic block, at which the fake register is not live>
%
% When it finds an occurrence of that pattern, it replaces all references to
% the fake register with a local variable.
%
% If the basic block jumps to a code address which is not a label (e.g.
% do_redo, do_fail), we consider all registers to be live at the end of the
% basic block. This is because livemap.m, which computes liveness information
% for us, does not know about liveness requirements introduced by backtracking.
% This is a conservative approximation. The union of the livenesses of all the
% labels that represent resume points is a better approximation, but it would
% be tedious to compute and is unlikely to yield significantly better code.
%
% The second pattern we look for is simply an instruction that defines a fake
% register or stack slot, followed by some uses of that register or stack slot
% before code that redefines the register or stack slot. When we find this
% pattern, we again replace all references to the fake register or stack slot
% with a local variable, but since this time we cannot be sure that the
% original lval will not be referred to, we assign the local variable to the
% lval as well. This is a win because the cost of the assignment is less than
% the savings from replacing the fake register or stack slot references with
% local variable references.
%
% The third pattern we look for consists of a sequence of instructions in which
% a false register or stack slot is used several times, including at least once
% in the first instruction as a part of a path to a memory location, before
% being redefined or maybe aliased. This typically occurs when the code
% generator fills in the fields of a structure or extracts the fields of a
% structure. Again, we replace the false register or stack slot with a
% temporary after assigning the value in the false register or stack slot to
% the temporary.

%-----------------------------------------------------------------------------%

:- module ll_backend__use_local_vars.

:- interface.

:- import_module ll_backend__llds.
:- import_module list, counter.

:- pred use_local_vars__main(list(instruction)::in, list(instruction)::out,
	proc_label::in, int::in, int::in, counter::in, counter::out) is det.

:- implementation.

:- import_module ll_backend__basic_block, ll_backend__livemap.
:- import_module ll_backend__exprn_aux, ll_backend__code_util.
:- import_module ll_backend__opt_util.

:- import_module int, set, map, std_util, require.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

use_local_vars__main(Instrs0, Instrs, ProcLabel, NumRealRRegs, AccessThreshold,
		C0, C) :-
	create_basic_blocks(Instrs0, Comments, ProcLabel, C0, C1,
		LabelSeq, BlockMap0),
	flatten_basic_blocks(LabelSeq, BlockMap0, TentativeInstrs),
	livemap__build(TentativeInstrs, MaybeLiveMap),
	(
		% Instrs0 must have contained C code which cannot be analyzed
		MaybeLiveMap = no,
		Instrs = Instrs0,
		C = C0
	;
		MaybeLiveMap = yes(LiveMap),
		list__foldl(use_local_vars_block(LiveMap, NumRealRRegs,
			AccessThreshold), LabelSeq, BlockMap0, BlockMap),
		flatten_basic_blocks(LabelSeq, BlockMap, Instrs1),
		list__append(Comments, Instrs1, Instrs),
		C = C1
	).

:- pred use_local_vars_block(livemap::in, int::in, int::in, label::in,
	block_map::in, block_map::out) is det.

use_local_vars_block(LiveMap, NumRealRRegs, AccessThreshold, Label,
		BlockMap0, BlockMap) :-
	map__lookup(BlockMap0, Label, BlockInfo0),
	BlockInfo0 = block_info(BlockLabel, LabelInstr, RestInstrs0,
		JumpLabels, MaybeFallThrough),
	( can_branch_to_unknown_label(RestInstrs0) ->
		MaybeEndLiveLvals = no
	;
		(
			MaybeFallThrough = yes(FallThrough),
			EndLabels = [FallThrough | JumpLabels]
		;
			MaybeFallThrough = no,
			EndLabels = JumpLabels
		),
		list__foldl(find_live_lvals_at_end_labels(LiveMap), EndLabels,
			set__init, EndLiveLvals0),
		list__foldl(find_live_lvals_in_annotations, RestInstrs0,
			EndLiveLvals0, EndLiveLvals),
		MaybeEndLiveLvals = yes(EndLiveLvals)
	),
	counter__init(1, TempCounter0),
	use_local_vars_instrs(RestInstrs0, RestInstrs,
		TempCounter0, TempCounter, NumRealRRegs, AccessThreshold,
		MaybeEndLiveLvals),
	( TempCounter = TempCounter0 ->
		BlockMap = BlockMap0
	;
		BlockInfo = block_info(BlockLabel, LabelInstr,
			RestInstrs, JumpLabels, MaybeFallThrough),
		map__det_update(BlockMap0, Label, BlockInfo, BlockMap)
	).

:- pred can_branch_to_unknown_label(list(instruction)::in) is semidet.

can_branch_to_unknown_label([Uinstr - _ | Instrs]) :-
	(
		opt_util__instr_labels(Uinstr, _, CodeAddrs),
		some_code_addr_is_not_label(CodeAddrs)
	;
		can_branch_to_unknown_label(Instrs)
	).

:- pred some_code_addr_is_not_label(list(code_addr)::in) is semidet.

some_code_addr_is_not_label([CodeAddr | CodeAddrs]) :-
	(
		CodeAddr \= label(_Label)
	;
		some_code_addr_is_not_label(CodeAddrs)
	).

:- pred find_live_lvals_at_end_labels(livemap::in, label::in,
	lvalset::in, lvalset::out) is det.

find_live_lvals_at_end_labels(LiveMap, Label, LiveLvals0, LiveLvals) :-
	( map__search(LiveMap, Label, LabelLiveLvals) ->
		set__union(LiveLvals0, LabelLiveLvals, LiveLvals)
	; Label = local(_, _) ->
		error("find_live_lvals_at_end_labels: local label not found")
	;
		% Non-local labels can be found only through call instructions,
		% which must be preceded by livevals instructions. The
		% variables live at the label will be included when we process
		% the livevals instruction.
		LiveLvals = LiveLvals0
	).

:- pred find_live_lvals_in_annotations(instruction::in,
	lvalset::in, lvalset::out) is det.

find_live_lvals_in_annotations(Uinstr - _, LiveLvals0, LiveLvals) :-
	( Uinstr = livevals(InstrLiveLvals) ->
		set__union(LiveLvals0, InstrLiveLvals, LiveLvals)
	;
		LiveLvals = LiveLvals0
	).

%-----------------------------------------------------------------------------%

:- pred use_local_vars_instrs(list(instruction)::in, list(instruction)::out,
	counter::in, counter::out, int::in, int::in, maybe(lvalset)::in)
	is det.

use_local_vars_instrs(RestInstrs0, RestInstrs, TempCounter0, TempCounter,
		NumRealRRegs, AccessThreshold, MaybeEndLiveLvals) :-
	opt_assign(RestInstrs0, RestInstrs1,
		TempCounter0, TempCounter1, NumRealRRegs, MaybeEndLiveLvals),
	( AccessThreshold >= 1 ->
		opt_access(RestInstrs1, RestInstrs,
			TempCounter1, TempCounter, NumRealRRegs, set__init,
			AccessThreshold)
	;
		RestInstrs = RestInstrs1,
		TempCounter = TempCounter1
	).

%-----------------------------------------------------------------------------%

:- pred opt_assign(list(instruction)::in, list(instruction)::out,
	counter::in, counter::out, int::in, maybe(lvalset)::in) is det.

opt_assign([], [], TempCounter, TempCounter, _, _).
opt_assign([Instr0 | TailInstrs0], Instrs,
		TempCounter0, TempCounter, NumRealRRegs, MaybeEndLiveLvals) :-
	Instr0 = Uinstr0 - _Comment0,
	(
		( Uinstr0 = assign(ToLval, _FromRval)
		; Uinstr0 = incr_hp(ToLval, _MaybeTag, _SizeRval, _Type)
		),
		base_lval_worth_replacing(NumRealRRegs, ToLval)
	->
		counter__allocate(TempNum, TempCounter0, TempCounter1),
		NewLval = temp(r, TempNum),
		(
			ToLval = reg(_, _),
			MaybeEndLiveLvals = yes(EndLiveLvals),
			not set__member(ToLval, EndLiveLvals)
		->
			substitute_lval_in_defn(ToLval, NewLval,
				Instr0, Instr),
			list__map_foldl(exprn_aux__substitute_lval_in_instr(
				ToLval, NewLval),
				TailInstrs0, TailInstrs1, 0, _),
			opt_assign(TailInstrs1, TailInstrs,
				TempCounter1, TempCounter,
				NumRealRRegs, MaybeEndLiveLvals),
			Instrs = [Instr | TailInstrs]
		;
			substitute_lval_in_instr_until_defn(ToLval, NewLval,
				TailInstrs0, TailInstrs1, 0, NumSubst),
			NumSubst > 1
		->
			substitute_lval_in_defn(ToLval, NewLval,
				Instr0, Instr),
			CopyInstr = assign(ToLval, lval(NewLval)) - "",
			opt_assign(TailInstrs1, TailInstrs,
				TempCounter1, TempCounter,
				NumRealRRegs, MaybeEndLiveLvals),
			Instrs = [Instr, CopyInstr | TailInstrs]
		;
			opt_assign(TailInstrs0, TailInstrs,
				TempCounter0, TempCounter,
				NumRealRRegs, MaybeEndLiveLvals),
			Instrs = [Instr0 | TailInstrs]
		)
	;
		opt_assign(TailInstrs0, TailInstrs,
			TempCounter0, TempCounter,
			NumRealRRegs, MaybeEndLiveLvals),
		Instrs = [Instr0 | TailInstrs]
	).

%-----------------------------------------------------------------------------%

:- pred opt_access(list(instruction)::in, list(instruction)::out,
	counter::in, counter::out, int::in, lvalset::in, int::in) is det.

opt_access([], [], TempCounter, TempCounter, _, _, _).
opt_access([Instr0 | TailInstrs0], Instrs,
		TempCounter0, TempCounter, NumRealRRegs, AlreadyTried0,
		AccessThreshold) :-
	Instr0 = Uinstr0 - _Comment0,
	(
		Uinstr0 = assign(ToLval, FromRval),
		lvals_in_lval(ToLval, ToSubLvals),
		lvals_in_rval(FromRval, FromSubLvals),
		list__append(ToSubLvals, FromSubLvals, SubLvals),
		list__filter(
			base_lval_worth_replacing_not_tried(
				AlreadyTried0, NumRealRRegs),
			SubLvals, ReplaceableSubLvals),
		ReplaceableSubLvals = [ChosenLval | ChooseableRvals]
	->
		counter__allocate(TempNum, TempCounter0, TempCounter1),
		TempLval = temp(r, TempNum),
		lvals_in_lval(ChosenLval, SubChosenLvals),
		require(unify(SubChosenLvals, []),
			"opt_access: nonempty SubChosenLvals"),
		substitute_lval_in_instr_until_defn(ChosenLval, TempLval,
			[Instr0 | TailInstrs0], Instrs1, 0, NumReplacements),
		set__insert(AlreadyTried0, ChosenLval, AlreadyTried1),
		( NumReplacements >= AccessThreshold ->
			TempAssign = assign(TempLval, lval(ChosenLval))
				- "factor out common sub lval",
			Instrs2 = [TempAssign | Instrs1],
			opt_access(Instrs2, Instrs, TempCounter1, TempCounter,
				NumRealRRegs, AlreadyTried1, AccessThreshold)
		; ChooseableRvals = [_ | _] ->
			opt_access([Instr0 | TailInstrs0], Instrs,
				TempCounter0, TempCounter,
				NumRealRRegs, AlreadyTried1, AccessThreshold)
		;
			opt_access(TailInstrs0, TailInstrs,
				TempCounter0, TempCounter,
				NumRealRRegs, set__init, AccessThreshold),
			Instrs = [Instr0 | TailInstrs]
		)
	;
		opt_access(TailInstrs0, TailInstrs,
			TempCounter0, TempCounter,
			NumRealRRegs, set__init, AccessThreshold),
		Instrs = [Instr0 | TailInstrs]
	).

%-----------------------------------------------------------------------------%

:- pred base_lval_worth_replacing(int::in, lval::in) is semidet.

base_lval_worth_replacing(NumRealRRegs, Lval) :-
	(
		Lval = reg(r, RegNum),
		RegNum > NumRealRRegs
	;
		Lval = stackvar(_)
	;
		Lval = framevar(_)
	).

:- pred base_lval_worth_replacing_not_tried(lvalset::in, int::in, lval::in)
	is semidet.

base_lval_worth_replacing_not_tried(AlreadyTried, NumRealRRegs, Lval) :-
	\+ set__member(Lval, AlreadyTried),
	base_lval_worth_replacing(NumRealRRegs, Lval).

%-----------------------------------------------------------------------------%

	% When processing substituting e.g. tempr1 for e.g. r2
	% in the instruction that defines r2, we must be careful
	% to leave intact the value being assigned. Given the instruction
	%
	%	r2 = field(0, r2, 5)
	%
	% we must generate
	%
	%	tempr1 = field(0, r2, 5)
	%
	% Generating
	%
	%	tempr1 = field(0, tempr1, 5)
	%
	% would introduce a bug, since the right hand side now refers to
	% an as yet undefined variable.

:- pred substitute_lval_in_defn(lval::in, lval::in,
	instruction::in, instruction::out) is det.

substitute_lval_in_defn(OldLval, NewLval, Instr0, Instr) :-
	Instr0 = Uinstr0 - Comment,
	( Uinstr0 = assign(ToLval, FromRval) ->
		require(unify(ToLval, OldLval),
			"substitute_lval_in_defn: mismatch in assign"),
		Uinstr = assign(NewLval, FromRval)
	; Uinstr0 = incr_hp(ToLval, MaybeTag, SizeRval, Type) ->
		require(unify(ToLval, OldLval),
			"substitute_lval_in_defn: mismatch in incr_hp"),
		Uinstr = incr_hp(NewLval, MaybeTag, SizeRval, Type)
	;
		error("substitute_lval_in_defn: unexpected instruction")
	),
	Instr = Uinstr - Comment.

	% Substitute NewLval for OldLval in an instruction sequence
	% until we come an instruction that may define OldLval.
	% We don't worry about instructions that define a variable that
	% occurs in the access path to OldLval (and which therefore indirectly
	% modifies the value that OldLval refers to), because our caller will
	% call us only with OldLvals (and NewLvals for that matter) that have
	% no lvals in their access path. The NewLvals will be temporaries,
	% representing local variables in C blocks.
	%
	% When control leaves this instruction sequence via a if_val, goto or
	% call, the local variables of the block in which this instruction
	% sequence will go out of scope, so we must stop using them. At points
	% at which control can enter this instruction sequence, i.e. at labels,
	% the C block ends, so again we must stop using its local variables.
	% (Livevals pseudo-instructions occur only immediately before
	% instructions that cause control transfer, so we stop at them too.)
	% 
	% Our caller ensures that we can also so stop at any point. By doing so
	% we may fail to exploit an optimization opportunity, but the code we
	% generate will still be correct. At the moment we stop at instructions
	% whose correct handling would be non-trivial and which rarely if ever
	% appear between the definition and a use of a location we want to
	% substitute. These include instructions that manipulate stack frames,
	% the heap, the trail and synchronization data.

:- pred substitute_lval_in_instr_until_defn(lval::in, lval::in,
	list(instruction)::in, list(instruction)::out, int::in, int::out)
	is det.

substitute_lval_in_instr_until_defn(_, _, [], [], N, N).
substitute_lval_in_instr_until_defn(OldLval, NewLval,
		[Instr0 | Instrs0], [Instr | Instrs], N0, N) :-
	Instr0 = Uinstr0 - _,
	(
		Uinstr0 = comment(_),
		Instr = Instr0,
		substitute_lval_in_instr_until_defn(OldLval, NewLval,
			Instrs0, Instrs, N0, N)
	;
		Uinstr0 = livevals(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = block(_, _, _),
		error("substitute_lval_in_instr_until_defn: found block")
	;
		Uinstr0 = assign(Lval, _),
		( Lval = OldLval ->
				% If we alter any lval that occurs in OldLval,
				% we must stop the substitutions. At the
				% moment, the only lval OldLval contains is
				% itself.
			Instr = Instr0,
			Instrs = Instrs0,
			N = N0
		;
			exprn_aux__substitute_lval_in_instr(OldLval, NewLval,
				Instr0, Instr, N0, N1),
			substitute_lval_in_instr_until_defn(OldLval, NewLval,
				Instrs0, Instrs, N1, N)
		)
	;
		Uinstr0 = call(_, _, _, _, _, _),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = mkframe(_, _),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = label(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = goto(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = computed_goto(_, _),
		exprn_aux__substitute_lval_in_instr(OldLval, NewLval,
			Instr0, Instr, N0, N),
		Instrs = Instrs0
	;
		Uinstr0 = if_val(_, _),
		exprn_aux__substitute_lval_in_instr(OldLval, NewLval,
			Instr0, Instr, N0, N),
		Instrs = Instrs0
	;
		Uinstr0 = incr_hp(Lval, _, _, _),
		( Lval = OldLval ->
				% If we alter any lval that occurs in OldLval,
				% we must stop the substitutions. At the
				% moment, the only lval OldLval contains is
				% itself.
			Instr = Instr0,
			Instrs = Instrs0,
			N = N0
		;
			exprn_aux__substitute_lval_in_instr(OldLval, NewLval,
				Instr0, Instr, N0, N1),
			substitute_lval_in_instr_until_defn(OldLval, NewLval,
				Instrs0, Instrs, N1, N)
		)
	;
		Uinstr0 = mark_hp(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = restore_hp(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = free_heap(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = store_ticket(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = reset_ticket(_, _),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = discard_ticket,
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = prune_ticket,
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = mark_ticket_stack(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = prune_tickets_to(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = incr_sp(_, _),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = decr_sp(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = init_sync_term(_, _),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = fork(_, _, _),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = join_and_terminate(_),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = join_and_continue(_, _),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = c_code(_, _),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	;
		Uinstr0 = pragma_c(_, _, _, _, _, _, _, _),
		Instr = Instr0,
		Instrs = Instrs0,
		N = N0
	).
