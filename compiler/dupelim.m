%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% dupelim.m - eliminate some duplicate code sequences.
%
% Author: zs.
%
% Our algorithm has the following stages.
%
% 1.	Divide the code of the procedure into basic blocks.
%
% 2.	For each block, compute a standard form, which is its most general
%	generalization.
%
% 3.	Find out which sets of blocks have the same standard form.
%
% 4.	For each set of blocks with the same standard form, find out
%	which blocks are not fallen into and can thus be eliminated,
%	and choose which blocks will be eliminated.
%
% 5.	For each set of blocks with the same standard form, compute
%	their most specific common generalization (which must exist),
%	and substitute this code for the code of the copy of the block
%	that step 4 has decided to keep.
%
% 6.	Convert the (possibly reduced) list of basic blocks back to a
%	list of instructions and substitute all references to the labels
%	starting eliminated blocks to refer to their noneliminated version.
%
% Generalizing an rval, lval or instruction involves replacing field references
% with known tags with field references with unknown tags. Generalizing a block
% involves generalizing its constituent instructions, removing comments, and
% possibly adding a goto at the end to represent falling through to the next
% label. In all other ways the original and the generalized version will be
% identical.

%-----------------------------------------------------------------------------%

:- module dupelim.

:- interface.

:- import_module llds.
:- import_module list, counter.

:- pred dupelim_main(list(instruction)::in, proc_label::in,
	counter::in, counter::out, list(instruction)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module basic_block, opt_util.
:- import_module bool, std_util, assoc_list, set, map, require.

	% A std_map maps a list of standardized instructions to the list
	% of labels whose basic blocks have that standardized form.
:- type std_map		==	map(list(instr), list(label)).

	% cluster(Exemplar, OtherLabels) means that references to labels
	% in OtherLabels can be replaced with references to Exemplar
	% once its block has been replaced with the most specific
	% generalization of the blocks started by Exemplar and OtherLabels.
	% OtherLabels must be nonempty.
:- type cluster		--->	cluster(label, list(label)).

dupelim_main(Instrs0, ProcLabel, C0, C, Instrs) :-
	create_basic_blocks(Instrs0, Comments, ProcLabel, C0, C,
		LabelSeq0, BlockMap0),
	map__init(StdMap0),
	set__init(Fixed0),
	dupelim__build_maps(LabelSeq0, BlockMap0, StdMap0, StdMap,
		Fixed0, Fixed),
	map__values(StdMap, StdList),
	find_clusters(StdList, Fixed, [], Clusters),
	( Clusters = [] ->
			% We don't want to introduce any incidental changes
			% if we cannot eliminate any blocks.
		Instrs = Instrs0
	;
		map__init(ReplMap0),
		process_clusters(Clusters, LabelSeq0, LabelSeq,
			BlockMap0, BlockMap, ReplMap0, ReplMap),
		flatten_basic_blocks(LabelSeq, BlockMap, Instrs1),
		opt_util__replace_labels_instruction_list(Instrs1,
			ReplMap, yes, Instrs2),
		list__append(Comments, Instrs2, Instrs)
	).

%-----------------------------------------------------------------------------%

% dupelim__build_maps builds up a map mapping standardized instruction
% sequences to the label(s) that start basic blocks with that standardized
% form, and a set showing which labels are fallen into.

:- pred dupelim__build_maps(list(label)::in, block_map::in,
	std_map::in, std_map::out, set(label)::in, set(label)::out) is det.

dupelim__build_maps([], _, StdMap, StdMap, Fixed, Fixed).
dupelim__build_maps([Label | Labels], BlockMap, StdMap0, StdMap,
		Fixed0, Fixed) :-
	map__lookup(BlockMap, Label, BlockInfo),
	BlockInfo = block_info(_, _, Instrs, _, MaybeFallThrough),
	standardize_instr_block(Instrs, MaybeFallThrough, StdInstrs),
	( map__search(StdMap0, StdInstrs, Cluster) ->
		map__det_update(StdMap0, StdInstrs, [Label | Cluster], StdMap1)
	;
		map__det_insert(StdMap0, StdInstrs, [Label], StdMap1)
	),
	( MaybeFallThrough = yes(FallIntoLabel) ->
		set__insert(Fixed0, FallIntoLabel, Fixed1)
	;
		Fixed1 = Fixed0
	),
	AddPragmaReferredLabels = lambda(
		[Instr::in, FoldFixed0::in, FoldFixed::out] is det, (
		(
			Instr = pragma_c(_, _, _,
				MaybeFixedLabel, MaybeLayoutLabel,
				MaybeOnlyLayoutLabel, _, _) - _
		->
			( MaybeFixedLabel = yes(FixedLabel) ->
				set__insert(FoldFixed0, FixedLabel, FoldFixed1)
			;
				FoldFixed1 = FoldFixed0
			),
			( MaybeLayoutLabel = yes(LayoutLabel) ->
				set__insert(FoldFixed1, LayoutLabel,
					FoldFixed2)
			;
				FoldFixed2 = FoldFixed1
			),
			( MaybeOnlyLayoutLabel = yes(OnlyLayoutLabel) ->
				set__insert(FoldFixed2, OnlyLayoutLabel,
					FoldFixed)
			;
				FoldFixed = FoldFixed2
			)
		;
			FoldFixed = FoldFixed0
		)
	)),
	list__foldl(AddPragmaReferredLabels, Instrs,
		Fixed1, Fixed2),
	dupelim__build_maps(Labels, BlockMap, StdMap1, StdMap,
		Fixed2, Fixed).

% For each set of labels that start basic blocks with identical standard forms,
% find_clusters finds out whether we can eliminate some of those blocks;
% if yes, it decides which blocks can be eliminated and which other block
% should stand in their place.

% If two or more blocks have the same standardized form, it may be possible
% to eliminate all but one of the blocks. However, blocks that can be fallen
% into cannot be eliminated. (Actually, they could, but only by inserting
% a goto, and full jumpopt would then undo the elimination of the block.)
% Similarly, blocks whose starting label is referred to by C code cannot
% be eliminated. (Actually, they could, but only by doing surgery on C code
% strings, which is not a good idea.)

:- pred find_clusters(list(list(label))::in, set(label)::in,
	list(cluster)::in, list(cluster)::out) is det.

find_clusters([], _, Clusters, Clusters).
find_clusters([Labels | LabelsList], Fixed, Clusters0, Clusters) :-
	(
		Labels = [_, _ | _],
			% The rest of the condition is relatively expensive,
			% so don't do it if there aren't at least two labels
			% whose blocks have the same standardized form.
		IsFallenInto = lambda([Label::in] is semidet, (
			set__member(Label, Fixed)
		)),
		list__filter(IsFallenInto, Labels,
			FixedLabels, NonFixedLabels),
		NonFixedLabels = [FirstNonFixed | OtherNonFixed]
	->
		( FixedLabels = [ChosenLabel | _] ->
			Cluster = cluster(ChosenLabel, NonFixedLabels)
		;
			Cluster = cluster(FirstNonFixed, OtherNonFixed)
		),
		Clusters1 = [Cluster | Clusters0]
	;
		Clusters1 = Clusters0
	),
	find_clusters(LabelsList, Fixed, Clusters1, Clusters).

%-----------------------------------------------------------------------------%

% For each cluster, a set of blocks in which all but one are to be eliminated
% favor of the remaining one, find their most specific common generalization
% (which must exist), and substitute this code for the code of the copy of
% the block that is to be kept. Remove the eliminated labels from the
% label sequence and map them to their replacements.

:- pred process_clusters(list(cluster)::in, list(label)::in, list(label)::out,
	block_map::in, block_map::out,
	map(label, label)::in, map(label, label)::out) is det.

process_clusters([], LabelSeq, LabelSeq, BlockMap, BlockMap,
		ReplMap, ReplMap).
process_clusters([Cluster | Clusters], LabelSeq0, LabelSeq,
		BlockMap0, BlockMap, ReplMap0, ReplMap) :-
	Cluster = cluster(Exemplar, ElimLabels),
	map__lookup(BlockMap0, Exemplar, ExemplarInfo0),
	ExemplarInfo0 = block_info(ExLabel, ExLabelInstr, ExInstrs0,
		ExSideLabels, ExMaybeFallThrough),
	require(unify(Exemplar, ExLabel), "exemplar label mismatch"),
	process_elim_labels(ElimLabels, ExInstrs0, ExMaybeFallThrough,
		LabelSeq0, LabelSeq1, BlockMap0, Exemplar, ReplMap0, ReplMap1,
		UnifiedInstrs, UnifiedMaybeFallThrough),
	ExemplarInfo = block_info(ExLabel, ExLabelInstr, UnifiedInstrs,
		ExSideLabels, UnifiedMaybeFallThrough),
	map__det_update(BlockMap0, Exemplar, ExemplarInfo, BlockMap1),
	process_clusters(Clusters, LabelSeq1, LabelSeq, BlockMap1, BlockMap,
		ReplMap1, ReplMap).

% Given the current form of a basic block (instructions and fallthrough),
% compute its most specific generalization with the basic blocks headed
% by the given labels, whose basic blocks are to be eliminated.
%
% On the same traversal of the list of to-be-eliminated labels, remove each
% such label from the sequence of labels whose basic blocks will make up
% the final code of the procedure, and add the mapping of the eliminated
% label to the replacement (exemplar) label to the set of substitutions
% that will need to be done.

:- pred process_elim_labels(list(label)::in, list(instruction)::in,
	maybe(label)::in, list(label)::in, list(label)::out, block_map::in,
	label::in, map(label, label)::in, map(label, label)::out,
	list(instruction)::out, maybe(label)::out) is det.

process_elim_labels([], Instrs, MaybeFT, LabelSeq, LabelSeq, _,
		_, ReplMap, ReplMap, Instrs, MaybeFT).
process_elim_labels([ElimLabel | ElimLabels], Instrs0, MaybeFallThrough0,
		LabelSeq0, LabelSeq, BlockMap, Exemplar, ReplMap0, ReplMap,
		Instrs, MaybeFallThrough) :-
	map__lookup(BlockMap, ElimLabel, ElimLabelInfo),
	ElimLabelInfo = block_info(ElimLabel2, _, ElimInstrs,
		_, ElimMaybeFallThrough),
	require(unify(ElimLabel, ElimLabel2), "elim label mismatch"),
	(
		most_specific_block(Instrs0, MaybeFallThrough0,
			ElimInstrs, ElimMaybeFallThrough,
			Instrs1, MaybeFallThrough1)
	->
		list__delete_all(LabelSeq0, ElimLabel, LabelSeq1),
		map__det_insert(ReplMap0, ElimLabel, Exemplar, ReplMap1),
		process_elim_labels(ElimLabels, Instrs1, MaybeFallThrough1,
			LabelSeq1, LabelSeq, BlockMap,
			Exemplar, ReplMap1, ReplMap, Instrs, MaybeFallThrough)
	;
		error("blocks with same standard form don't antiunify")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The code of this section is concerned with computing the standard
	% form (most general generalization) of a sequence of instructions.

	% If a block can fall through, we add a goto to the following label
	% at the end. This way, it will match with other blocks that have
	% identical (standardized) content except for an explicit goto to our
	% fallthrough label.

:- pred standardize_instr_block(list(instruction)::in, maybe(label)::in,
	list(instr)::out) is det.

standardize_instr_block(Instrs0, MaybeFallThrough, Uinstrs) :-
	standardize_instrs(Instrs0, Uinstrs1),
	(
		MaybeFallThrough = yes(Label),
		Goto = goto(label(Label)),
		list__append(Uinstrs1, [Goto], Uinstrs)
	;
		MaybeFallThrough = no,
		Uinstrs = Uinstrs1
	).

	% Compute the standard form of a sequence of instructions.

:- pred standardize_instrs(list(instruction)::in, list(instr)::out) is det.

standardize_instrs([], []).
standardize_instrs([Instr - _ | Instrs], StdInstrs) :-
	standardize_instrs(Instrs, StdInstrs1),
	standardize_instr(Instr, StdInstr),
	( StdInstr = comment(_) ->
		StdInstrs = StdInstrs1
	;
		StdInstrs = [StdInstr | StdInstrs1]
	).

	% Compute the standard form of an instruction.

:- pred standardize_instr(instr::in, instr::out) is det.

standardize_instr(Instr1, Instr) :-
	(
		Instr1 = comment(_),
		Instr = Instr1
	;
		Instr1 = livevals(_),
		Instr = Instr1
	;
		Instr1 = block(_, _, _),
		Instr = Instr1
	;
		Instr1 = assign(Lval1, Rval1),
		standardize_lval(Lval1, Lval),
		standardize_rval(Rval1, Rval),
		Instr = assign(Lval, Rval)
	;
		Instr1 = call(_, _, _, _, _, _),
		Instr = Instr1
	;
		Instr1 = mkframe(_, _),
		Instr = Instr1
	;
		Instr1 = label(_),
		Instr = Instr1
	;
		Instr1 = goto(_),
		Instr = Instr1
	;
		Instr1 = computed_goto(_, _),
		Instr = Instr1
	;
		Instr1 = c_code(_, _),
		Instr = Instr1
	;
		Instr1 = if_val(Rval1, CodeAddr),
		standardize_rval(Rval1, Rval),
		Instr = if_val(Rval, CodeAddr)
	;
		Instr1 = incr_hp(Lval1, MaybeTag, Rval1, Msg),
		standardize_lval(Lval1, Lval),
		standardize_rval(Rval1, Rval),
		Instr = incr_hp(Lval, MaybeTag, Rval, Msg)
	;
		Instr1 = mark_hp(Lval1),
		standardize_lval(Lval1, Lval),
		Instr = mark_hp(Lval)
	;
		Instr1 = restore_hp(Rval1),
		standardize_rval(Rval1, Rval),
		Instr = restore_hp(Rval)
	;
		Instr1 = free_heap(Rval1),
		standardize_rval(Rval1, Rval),
		Instr = free_heap(Rval)
	;
		Instr1 = store_ticket(Lval1),
		standardize_lval(Lval1, Lval),
		Instr = store_ticket(Lval)
	;
		Instr1 = reset_ticket(Rval1, Reason),
		standardize_rval(Rval1, Rval),
		Instr = reset_ticket(Rval, Reason)
	;
		Instr1 = discard_ticket,
		Instr = Instr1
	;
		Instr1 = prune_ticket,
		Instr = Instr1
	;
		Instr1 = mark_ticket_stack(Lval1),
		standardize_lval(Lval1, Lval),
		Instr = mark_ticket_stack(Lval)
	;
		Instr1 = prune_tickets_to(Rval1),
		standardize_rval(Rval1, Rval),
		Instr = prune_tickets_to(Rval)
	;
		Instr1 = incr_sp(_, _),
		Instr = Instr1
	;
		Instr1 = decr_sp(_),
		Instr = Instr1
	;
		Instr1 = fork(_, _, _),
		Instr = Instr1
	;
		Instr1 = init_sync_term(Lval1, N),
		standardize_lval(Lval1, Lval),
		Instr = init_sync_term(Lval, N)
	;
		Instr1 = join_and_terminate(Lval1),
		standardize_lval(Lval1, Lval),
		Instr = join_and_terminate(Lval)
	;
		Instr1 = join_and_continue(Lval1, N),
		standardize_lval(Lval1, Lval),
		Instr = join_and_continue(Lval, N)
	;
		Instr1 = pragma_c(_, _, _, _, _, _, _, _),
		Instr = Instr1
	).

	% Compute the standard form of an lval.

:- pred standardize_lval(lval::in, lval::out) is det.

standardize_lval(Lval1, Lval) :-
	(
		Lval1 = reg(_, _),
		Lval = Lval1
	;
		Lval1 = succip,
		Lval = Lval1
	;
		Lval1 = maxfr,
		Lval = Lval1
	;
		Lval1 = curfr,
		Lval = Lval1
	;
		Lval1 = hp,
		Lval = Lval1
	;
		Lval1 = sp,
		Lval = Lval1
	;
		Lval1 = temp(_, _),
		Lval = Lval1
	;
		Lval1 = stackvar(_),
		Lval = Lval1
	;
		Lval1 = framevar(_),
		Lval = Lval1
	;
		Lval1 = succip(_),
		Lval = Lval1
	;
		Lval1 = redoip(_),
		Lval = Lval1
	;
		Lval1 = succfr(_),
		Lval = Lval1
	;
		Lval1 = redofr(_),
		Lval = Lval1
	;
		Lval1 = prevfr(_),
		Lval = Lval1
	;
		Lval1 = field(_, Addr, FieldNum),
		Lval = field(no, Addr, FieldNum)
	;
		Lval1 = mem_ref(_),
		Lval = Lval1
	;
		Lval1 = lvar(_),
		error("lvar in standardize_lval")
	).

	% Compute the standard form of an rval.

:- pred standardize_rval(rval::in, rval::out) is det.

standardize_rval(Rval1, Rval) :-
	(
		Rval1 = lval(Lval1),
		standardize_lval(Lval1, Lval),
		Rval = lval(Lval)
	;
		Rval1 = var(_),
		error("var in standardize_rval")
	;
		Rval1 = create(_, _, _, _, _, _, _),
		Rval = Rval1
	;
		Rval1 = mkword(_, _),
		Rval = Rval1
	;
		Rval1 = const(_),
		Rval = Rval1
	;
		Rval1 = unop(Unop, Rval1L),
		standardize_rval(Rval1L, RvalL),
		Rval = unop(Unop, RvalL)
	;
		Rval1 = binop(Binnop, Rval1L, Rval1R),
		standardize_rval(Rval1L, RvalL),
		standardize_rval(Rval1R, RvalR),
		Rval = binop(Binnop, RvalL, RvalR)
	;
		Rval1 = mem_addr(_),
		Rval = Rval1
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% This predicate computes the most specific code sequence that
	% generalizes both input sequences.

	% If a block can fall through, we add a goto to the following label
	% at the end. This way, it will match with other blocks that have
	% identical (standardized) content except for an explicit goto to our
	% fallthrough label.

:- pred standardize_block(list(instruction)::in, maybe(label)::in,
	list(instruction)::out) is det.

standardize_block(Instrs, MaybeFallThrough, StdInstrs) :-
	(
		MaybeFallThrough = yes(Label),
		(
			list__last(Instrs, LastInstr),
			LastInstr = goto(label(Label)) - _
		->
			StdInstrs = Instrs
		;
			Goto = goto(label(Label)) - "",
			list__append(Instrs, [Goto], StdInstrs)
		)
	;
		MaybeFallThrough = no,
		StdInstrs = Instrs
	).

:- pred most_specific_block(list(instruction)::in, maybe(label)::in,
	list(instruction)::in, maybe(label)::in,
	list(instruction)::out, maybe(label)::out) is semidet.

most_specific_block(Instrs1, MaybeFallThrough1,
		Instrs2, MaybeFallThrough2, Instrs, MaybeFallThrough) :-
	standardize_block(Instrs1, MaybeFallThrough1, StdInstrs1),
	standardize_block(Instrs2, MaybeFallThrough2, StdInstrs2),
	most_specific_instrs(StdInstrs1, StdInstrs2, Instrs),
		% A basic block cannot be empty after standardization, since
		% standardization adds a goto to basic blocks that previously
		% had no executable instructions. While most_specific_instrs
		% can delete comments from its input instruction sequences,
		% it cannot delete executable instructions.
	list__last_det(Instrs, LastInstr),
	( LastInstr = goto(label(Label)) - _ ->
		MaybeFallThrough = yes(Label)
	;
		MaybeFallThrough = no
	).

:- pred most_specific_instrs(list(instruction)::in, list(instruction)::in,
	list(instruction)::out) is semidet.

most_specific_instrs(Instrs1, Instrs2, Instrs) :-
	(
		Instrs1 = [Instr1 | Tail1],
		Instrs2 = [Instr2 | Tail2]
	->
		Instr1 = Uinstr1 - Comment1,
		Instr2 = Uinstr2 - Comment2,
		(
			most_specific_instr(Uinstr1, Uinstr2, Uinstr)
		->
			( Comment1 = Comment2 ->
				Comment = Comment1
			;
				Comment = "unified intruction"
			),
			Instr = Uinstr - Comment,
			most_specific_instrs(Tail1, Tail2, Tail),
			Instrs = [Instr | Tail]
		;
			Uinstr1 = comment(_)
		->
			most_specific_instrs(Tail1, Instrs2, Instrs)
		;
			Uinstr2 = comment(_)
		->
			most_specific_instrs(Instrs1, Tail2, Instrs)
		;
			fail
		)
	;
		Instrs1 = [],
		Instrs2 = []
	->
		Instrs = []
	;
		Instrs1 = [Instr1 | Tail1],
		Instr1 = comment(_) - _
	->
		most_specific_instrs(Tail1, Instrs2, Instrs)
	;
		Instrs2 = [Instr2 | Tail2],
		Instr2 = comment(_) - _
	->
		most_specific_instrs(Instrs1, Tail2, Instrs)
	;
		fail
	).

	% This predicate computes the most specific instruction that
	% generalizes both input instructions.

:- pred most_specific_instr(instr::in, instr::in, instr::out) is semidet.

most_specific_instr(Instr1, Instr2, Instr) :-
	(
		Instr1 = livevals(_),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = block(_, _, _),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = assign(Lval1, Rval1),
		Instr2 = assign(Lval2, Rval2),
		most_specific_lval(Lval1, Lval2, Lval),
		most_specific_rval(Rval1, Rval2, Rval),
		Instr = assign(Lval, Rval)
	;
		Instr1 = call(_, _, _, _, _, _),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = mkframe(_, _),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = label(_),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = goto(_),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = computed_goto(_, _),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = c_code(_, _),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = if_val(Rval1, CodeAddr),
		Instr2 = if_val(Rval2, CodeAddr),
		most_specific_rval(Rval1, Rval2, Rval),
		Instr = if_val(Rval, CodeAddr)
	;
		Instr1 = incr_hp(Lval1, MaybeTag, Rval1, Msg),
		Instr2 = incr_hp(Lval2, MaybeTag, Rval2, Msg),
		most_specific_lval(Lval1, Lval2, Lval),
		most_specific_rval(Rval1, Rval2, Rval),
		Instr = incr_hp(Lval, MaybeTag, Rval, Msg)
	;
		Instr1 = mark_hp(Lval1),
		Instr2 = mark_hp(Lval2),
		most_specific_lval(Lval1, Lval2, Lval),
		Instr = mark_hp(Lval)
	;
		Instr1 = restore_hp(Rval1),
		Instr2 = restore_hp(Rval2),
		most_specific_rval(Rval1, Rval2, Rval),
		Instr = restore_hp(Rval)
	;
		Instr1 = free_heap(Rval1),
		Instr2 = free_heap(Rval2),
		most_specific_rval(Rval1, Rval2, Rval),
		Instr = free_heap(Rval)
	;
		Instr1 = store_ticket(Lval1),
		Instr2 = store_ticket(Lval2),
		most_specific_lval(Lval1, Lval2, Lval),
		Instr = store_ticket(Lval)
	;
		Instr1 = reset_ticket(Rval1, Reason),
		Instr2 = reset_ticket(Rval2, Reason),
		most_specific_rval(Rval1, Rval2, Rval),
		Instr = reset_ticket(Rval, Reason)
	;
		Instr1 = discard_ticket,
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = prune_ticket,
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = mark_ticket_stack(Lval1),
		Instr2 = mark_ticket_stack(Lval2),
		most_specific_lval(Lval1, Lval2, Lval),
		Instr = mark_ticket_stack(Lval)
	;
		Instr1 = prune_tickets_to(Rval1),
		Instr2 = prune_tickets_to(Rval2),
		most_specific_rval(Rval1, Rval2, Rval),
		Instr = prune_tickets_to(Rval)
	;
		Instr1 = incr_sp(_, _),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = decr_sp(_),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = pragma_c(_, _, _, _, _, _, _, _),
		Instr2 = Instr1,
		Instr = Instr1
	).

	% This predicate computes the most specific lval that
	% generalizes both input lvals.

:- pred most_specific_lval(lval::in, lval::in, lval::out) is semidet.

most_specific_lval(Lval1, Lval2, Lval) :-
	(
		Lval1 = reg(_, _),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = succip,
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = maxfr,
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = curfr,
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = hp,
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = sp,
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = temp(_, _),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = stackvar(_),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = framevar(_),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = succip(_),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = redoip(_),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = redofr(_),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = succfr(_),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = prevfr(_),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = field(MaybeTag1, Addr, FieldNum),
		Lval2 = field(MaybeTag2, Addr, FieldNum),
		( MaybeTag1 = MaybeTag2 ->
			MaybeTag = MaybeTag1
		;
			MaybeTag = no
		),
		Lval = field(MaybeTag, Addr, FieldNum)
	;
		Lval1 = mem_ref(_),
		Lval2 = Lval1,
		Lval = Lval1
	;
		Lval1 = lvar(_),
		error("lvar in most_specific_lval")
	).

	% This predicate computes the most specific rval that
	% generalizes both input rvals.

:- pred most_specific_rval(rval::in, rval::in, rval::out) is semidet.

most_specific_rval(Rval1, Rval2, Rval) :-
	(
		Rval1 = lval(Lval1),
		Rval2 = lval(Lval2),
		most_specific_lval(Lval1, Lval2, Lval),
		Rval = lval(Lval)
	;
		Rval1 = var(_),
		error("var in most_specific_rval")
	;
		Rval1 = create(_, _, _, _, _, _, _),
		Rval2 = Rval1,
		Rval = Rval1
	;
		Rval1 = mkword(_, _),
		Rval2 = Rval1,
		Rval = Rval1
	;
		Rval1 = const(_),
		Rval2 = Rval1,
		Rval = Rval1
	;
		Rval1 = unop(Unop, Rval1L),
		Rval2 = unop(Unop, Rval2L),
		most_specific_rval(Rval1L, Rval2L, RvalL),
		Rval = unop(Unop, RvalL)
	;
		Rval1 = binop(Binnop, Rval1L, Rval1R),
		Rval2 = binop(Binnop, Rval2L, Rval2R),
		most_specific_rval(Rval1L, Rval2L, RvalL),
		most_specific_rval(Rval1R, Rval2R, RvalR),
		Rval = binop(Binnop, RvalL, RvalR)
	;
		Rval1 = mem_addr(_),
		Rval2 = Rval1,
		Rval = Rval1
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
