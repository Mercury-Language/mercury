%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
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

:- import_module list, llds.

:- pred dupelim_main(list(instruction)::in, list(instruction)::out) is det.

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

dupelim_main(Instrs0, Instrs) :-
	create_basic_blocks(Instrs0, Comments, _ProcLabel, _N,
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
		dupelim__replace_labels_instr_list(Instrs1, ReplMap, Instrs2),
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
	standardize_block(Instrs, MaybeFallThrough, StdInstrs),
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
		( Instr = pragma_c(_, _, _, yes(PragmaLabel), _) - _ ->
			set__insert(FoldFixed0, PragmaLabel, FoldFixed)
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
process_elim_labels([Label | Labels], Instrs0, MaybeFallThrough0,
		LabelSeq0, LabelSeq, BlockMap, Exemplar, ReplMap0, ReplMap,
		Instrs, MaybeFallThrough) :-
	map__lookup(BlockMap, Label, LabelInfo),
	LabelInfo = block_info(ElimLabel, _, ElimInstrs,
		_, ElimMaybeFallThrough),
	require(unify(Label, ElimLabel), "elim label mismatch"),
	(
		most_specific_instrs(Instrs0, MaybeFallThrough0,
			ElimInstrs, ElimMaybeFallThrough,
			Instrs1, MaybeFallThrough1)
	->
		list__delete_all(LabelSeq0, Label, LabelSeq1),
		map__det_insert(ReplMap0, Label, Exemplar, ReplMap1),
		process_elim_labels(Labels, Instrs1, MaybeFallThrough1,
			LabelSeq1, LabelSeq, BlockMap,
			Exemplar, ReplMap1, ReplMap, Instrs, MaybeFallThrough)
	;
		error("blocks with same standard form don't antiunify")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The code of this section is concerned with computing the standard
	% form (most general generalization) of a sequence of instructions.

:- pred standardize_block(list(instruction)::in, maybe(label)::in,
	list(instr)::out) is det.

	% If a block can fall through, we add a goto to the following label
	% at the end. This way, it will match with other blocks that have
	% identical (standardized) content except for an explicit goto to our
	% fallthrough label.

standardize_block(Instrs0, MaybeFallThrough, Uinstrs) :-
	standardize_instrs(Instrs0, Uinstrs1),
	( MaybeFallThrough = yes(Label) ->
		Goto = goto(label(Label)),
		list__append(Uinstrs1, [Goto], Uinstrs)
	;
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
		Instr1 = call(_, _, _, _),
		Instr = Instr1
	;
		Instr1 = mkframe(_, _),
		Instr = Instr1
	;
		Instr1 = modframe(_),
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
		Instr1 = c_code(_),
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
		Instr1 = mark_ticket_stack(Lval1),
		standardize_lval(Lval1, Lval),
		Instr = mark_ticket_stack(Lval)
	;
		Instr1 = discard_tickets_to(Rval1),
		standardize_rval(Rval1, Rval),
		Instr = discard_tickets_to(Rval)
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
		Instr1 = pragma_c(_, _, _, _, _),
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
		Rval1 = create(_, _, _, _, _),
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

:- pred most_specific_instrs(list(instruction)::in, maybe(label)::in,
	list(instruction)::in, maybe(label)::in,
	list(instruction)::out, maybe(label)::out) is semidet.

most_specific_instrs(Instrs1, MaybeFallThrough1,
		Instrs2, MaybeFallThrough2, Instrs, MaybeFallThrough) :-
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
			most_specific_instrs(Tail1, MaybeFallThrough1,
				Tail2, MaybeFallThrough2,
				Tail, MaybeFallThrough),
			Instrs = [Instr | Tail]
		;
			Uinstr1 = comment(_)
		->
			most_specific_instrs(Tail1, MaybeFallThrough1,
				Instrs2, MaybeFallThrough2,
				Instrs, MaybeFallThrough)
		;
			Uinstr2 = comment(_)
		->
			most_specific_instrs(Instrs1, MaybeFallThrough1,
				Tail2, MaybeFallThrough2,
				Instrs, MaybeFallThrough)
		;
			fail
		)
	;
		Instrs1 = [],
		Instrs2 = []
	->
		require(unify(MaybeFallThrough1, no), "two empty lists with fallthrough"),
		require(unify(MaybeFallThrough2, no), "two empty lists with fallthrough"),
		Instrs = [],
		MaybeFallThrough = no
	;
		Instrs1 = [Instr1],
		Instrs2 = [],
		Instr1 = goto(label(Target)) - _,
		MaybeFallThrough2 = yes(Target)
	->
		Instrs = [Instr1],
		MaybeFallThrough = no
	;
		Instrs1 = [],
		Instrs2 = [Instr2],
		Instr2 = goto(label(Target)) - _,
		MaybeFallThrough1 = yes(Target)
	->
		Instrs = [Instr2],
		MaybeFallThrough = no
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
		Instr1 = call(_, _, _, _),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = mkframe(_, _),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = modframe(_),
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
		Instr1 = c_code(_),
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
		Instr1 = mark_ticket_stack(Lval1),
		Instr2 = mark_ticket_stack(Lval2),
		most_specific_lval(Lval1, Lval2, Lval),
		Instr = mark_ticket_stack(Lval)
	;
		Instr1 = discard_tickets_to(Rval1),
		Instr2 = discard_tickets_to(Rval2),
		most_specific_rval(Rval1, Rval2, Rval),
		Instr = discard_tickets_to(Rval)
	;
		Instr1 = incr_sp(_, _),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = decr_sp(_),
		Instr2 = Instr1,
		Instr = Instr1
	;
		Instr1 = pragma_c(_, _, _, _, _),
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
		Rval1 = create(_, _, _, _, _),
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

	% The code in this section is concerned with replacing all references
	% to one given label with a reference to another given label.

:- pred dupelim__replace_labels_instr_list(list(instruction)::in,
	map(label, label)::in, list(instruction)::out) is det.

dupelim__replace_labels_instr_list([], _ReplMap, []).
dupelim__replace_labels_instr_list([Instr0 - Comment | Instrs0],
		ReplMap, [Instr - Comment | Instrs]) :-
	dupelim__replace_labels_instr(Instr0, ReplMap, Instr),
	dupelim__replace_labels_instr_list(Instrs0, ReplMap, Instrs).

:- pred dupelim__replace_labels_instr(instr::in, map(label, label)::in,
	instr::out) is det.

dupelim__replace_labels_instr(comment(Comment), _, comment(Comment)).
dupelim__replace_labels_instr(livevals(Livevals), _, livevals(Livevals)).
dupelim__replace_labels_instr(block(R, F, Instrs0), ReplMap,
		block(R, F, Instrs)) :-
	dupelim__replace_labels_instr_list(Instrs0, ReplMap, Instrs).
dupelim__replace_labels_instr(assign(Lval0, Rval0), ReplMap,
		assign(Lval, Rval)) :-
	dupelim__replace_labels_lval(Lval0, ReplMap, Lval),
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_instr(call(Target, Return0, LiveInfo, CM),
		ReplMap, call(Target, Return, LiveInfo, CM)) :-
	dupelim__replace_labels_code_addr(Return0, ReplMap, Return).
dupelim__replace_labels_instr(mkframe(NondetFrameInfo, Redoip0), ReplMap,
		mkframe(NondetFrameInfo, Redoip)) :-
	dupelim__replace_labels_code_addr(Redoip0, ReplMap, Redoip).
dupelim__replace_labels_instr(modframe(Redoip0), ReplMap, modframe(Redoip)) :-
	dupelim__replace_labels_code_addr(Redoip0, ReplMap, Redoip).
dupelim__replace_labels_instr(label(Label), ReplMap, label(Label)) :-
	( map__search(ReplMap, Label, _) ->
		error("found eliminated label in dupelim__replace_labels_instr")
	;
		true
	).
dupelim__replace_labels_instr(goto(Target0), ReplMap, goto(Target)) :-
	dupelim__replace_labels_code_addr(Target0, ReplMap, Target).
dupelim__replace_labels_instr(computed_goto(Rval0, Labels0), ReplMap,
		computed_goto(Rval, Labels)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval),
	dupelim__replace_labels_label_list(Labels0, ReplMap, Labels).
dupelim__replace_labels_instr(c_code(Code), _, c_code(Code)).
dupelim__replace_labels_instr(if_val(Rval0, Target0), ReplMap,
		if_val(Rval, Target)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval),
	dupelim__replace_labels_code_addr(Target0, ReplMap, Target).
dupelim__replace_labels_instr(incr_hp(Lval0, MaybeTag, Rval0, Msg), ReplMap,
		incr_hp(Lval, MaybeTag, Rval, Msg)) :-
	dupelim__replace_labels_lval(Lval0, ReplMap, Lval),
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_instr(mark_hp(Lval0), ReplMap, mark_hp(Lval)) :-
	dupelim__replace_labels_lval(Lval0, ReplMap, Lval).
dupelim__replace_labels_instr(restore_hp(Rval0), ReplMap, restore_hp(Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_instr(store_ticket(Lval0), ReplMap, 
		store_ticket(Lval)) :-
	dupelim__replace_labels_lval(Lval0, ReplMap, Lval).
dupelim__replace_labels_instr(reset_ticket(Rval0, Reason), ReplMap, 
		reset_ticket(Rval, Reason)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_instr(discard_ticket, _, discard_ticket).
dupelim__replace_labels_instr(mark_ticket_stack(Lval0), ReplMap, 
		mark_ticket_stack(Lval)) :-
	dupelim__replace_labels_lval(Lval0, ReplMap, Lval).
dupelim__replace_labels_instr(discard_tickets_to(Rval0), ReplMap, 
		discard_tickets_to(Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_instr(incr_sp(Size, Msg), _, incr_sp(Size, Msg)).
dupelim__replace_labels_instr(decr_sp(Size), _, decr_sp(Size)).
dupelim__replace_labels_instr(init_sync_term(T, N), _, init_sync_term(T, N)).
dupelim__replace_labels_instr(fork(Child0, Parent0, SlotCount), Replmap,
		fork(Child, Parent, SlotCount)) :-
	dupelim__replace_labels_label(Child0, Replmap, Child),
	dupelim__replace_labels_label(Parent0, Replmap, Parent).
dupelim__replace_labels_instr(join_and_terminate(Lval0), Replmap, join_and_terminate(Lval)) :-
	dupelim__replace_labels_lval(Lval0, Replmap, Lval).
dupelim__replace_labels_instr(join_and_continue(Lval0, Label0),
		Replmap, join_and_continue(Lval, Label)) :-
	dupelim__replace_labels_label(Label0, Replmap, Label),
	dupelim__replace_labels_lval(Lval0, Replmap, Lval).

:- pred dupelim__replace_labels_lval(lval, map(label, label), lval).
:- mode dupelim__replace_labels_lval(in, in, out) is det.

dupelim__replace_labels_instr(pragma_c(A,B,C,D,E), ReplMap,
		pragma_c(A,B,C,D,E)) :-
	(
		D = no
	;
		D = yes(Label0),
		dupelim__replace_labels_label(Label0, ReplMap, Label),
			% We cannot replace the label in the C code string
			% itself.
		require(unify(Label0, Label), "trying to replace Mercury label in C code")
	).

dupelim__replace_labels_lval(reg(RegType, RegNum), _, reg(RegType, RegNum)).
dupelim__replace_labels_lval(stackvar(N), _, stackvar(N)).
dupelim__replace_labels_lval(framevar(N), _, framevar(N)).
dupelim__replace_labels_lval(succip, _, succip).
dupelim__replace_labels_lval(maxfr, _, maxfr).
dupelim__replace_labels_lval(curfr, _, curfr).
dupelim__replace_labels_lval(succip(Rval0), ReplMap, succip(Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_lval(redoip(Rval0), ReplMap, redoip(Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_lval(redofr(Rval0), ReplMap, redofr(Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_lval(succfr(Rval0), ReplMap, succfr(Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_lval(prevfr(Rval0), ReplMap, prevfr(Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_lval(hp, _, hp).
dupelim__replace_labels_lval(sp, _, sp).
dupelim__replace_labels_lval(field(Tag, Base0, Offset0), ReplMap,
		field(Tag, Base, Offset)) :-
	dupelim__replace_labels_rval(Base0, ReplMap, Base),
	dupelim__replace_labels_rval(Offset0, ReplMap, Offset).
dupelim__replace_labels_lval(lvar(Var), _, lvar(Var)).
dupelim__replace_labels_lval(temp(Type, Num), _, temp(Type, Num)).
dupelim__replace_labels_lval(mem_ref(Rval0), ReplMap, mem_ref(Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).

:- pred dupelim__replace_labels_rval(rval::in, map(label, label)::in,
	rval::out) is det.

dupelim__replace_labels_rval(lval(Lval0), ReplMap, lval(Lval)) :-
	dupelim__replace_labels_lval(Lval0, ReplMap, Lval).
dupelim__replace_labels_rval(var(Var), _, var(Var)).
dupelim__replace_labels_rval(create(Tag, Rvals, Unique, N, Msg), _,
		create(Tag, Rvals, Unique, N, Msg)).
dupelim__replace_labels_rval(mkword(Tag, Rval0), ReplMap, mkword(Tag, Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_rval(const(Const0), ReplMap, const(Const)) :-
	dupelim__replace_labels_rval_const(Const0, ReplMap, Const).
dupelim__replace_labels_rval(unop(Op, Rval0), ReplMap, unop(Op, Rval)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).
dupelim__replace_labels_rval(binop(Op, LRval0, RRval0), ReplMap,
		binop(Op, LRval, RRval)) :-
	dupelim__replace_labels_rval(LRval0, ReplMap, LRval),
	dupelim__replace_labels_rval(RRval0, ReplMap, RRval).
dupelim__replace_labels_rval(mem_addr(MemRef0), ReplMap, mem_addr(MemRef)) :-
	dupelim__replace_labels_mem_ref(MemRef0, ReplMap, MemRef).

:- pred dupelim__replace_labels_mem_ref(mem_ref::in, map(label, label)::in,
	mem_ref::out) is det.

dupelim__replace_labels_mem_ref(stackvar_ref(N), _, stackvar_ref(N)).
dupelim__replace_labels_mem_ref(framevar_ref(N), _, framevar_ref(N)).
dupelim__replace_labels_mem_ref(heap_ref(Rval0, Tag, N), ReplMap,
		heap_ref(Rval, Tag, N)) :-
	dupelim__replace_labels_rval(Rval0, ReplMap, Rval).

:- pred dupelim__replace_labels_rval_const(rval_const::in,
	map(label, label)::in, rval_const::out) is det.

dupelim__replace_labels_rval_const(true, _, true).
dupelim__replace_labels_rval_const(false, _, false).
dupelim__replace_labels_rval_const(int_const(N), _, int_const(N)).
dupelim__replace_labels_rval_const(float_const(N), _, float_const(N)).
dupelim__replace_labels_rval_const(string_const(S), _, string_const(S)).
dupelim__replace_labels_rval_const(code_addr_const(Addr0), ReplMap,
		code_addr_const(Addr)) :-
	dupelim__replace_labels_code_addr(Addr0, ReplMap, Addr).
dupelim__replace_labels_rval_const(data_addr_const(DataAddr), _,
		data_addr_const(DataAddr)).
dupelim__replace_labels_rval_const(label_entry(Label), _, label_entry(Label)).

:- pred dupelim__replace_labels_code_addr(code_addr::in, map(label, label)::in,
	code_addr::out) is det.

dupelim__replace_labels_code_addr(label(Label0), ReplMap, label(Label)) :-
	dupelim__replace_labels_label(Label0, ReplMap, Label).
dupelim__replace_labels_code_addr(imported(Proc), _, imported(Proc)).
dupelim__replace_labels_code_addr(succip, _, succip).
dupelim__replace_labels_code_addr(do_succeed(Last), _, do_succeed(Last)).
dupelim__replace_labels_code_addr(do_redo, _, do_redo).
dupelim__replace_labels_code_addr(do_fail, _, do_fail).
dupelim__replace_labels_code_addr(do_det_closure, _, do_det_closure).
dupelim__replace_labels_code_addr(do_semidet_closure, _, do_semidet_closure).
dupelim__replace_labels_code_addr(do_nondet_closure, _, do_nondet_closure).
dupelim__replace_labels_code_addr(do_det_class_method, _, do_det_class_method).
dupelim__replace_labels_code_addr(do_semidet_class_method, _,
	do_semidet_class_method).
dupelim__replace_labels_code_addr(do_nondet_class_method, _,
	do_nondet_class_method).
dupelim__replace_labels_code_addr(do_not_reached, _, do_not_reached).

:- pred dupelim__replace_labels_label_list(list(label)::in,
	map(label, label)::in, list(label)::out) is det.

dupelim__replace_labels_label_list([], _ReplMap, []).
dupelim__replace_labels_label_list([Label0 | Labels0], ReplMap,
		[Label | Labels]) :-
	dupelim__replace_labels_label(Label0, ReplMap, Label),
	dupelim__replace_labels_label_list(Labels0, ReplMap, Labels).

:- pred dupelim__replace_labels_label(label::in, map(label, label)::in,
	label::out) is det.

dupelim__replace_labels_label(Label0, ReplMap, Label) :-
	( map__search(ReplMap, Label0, NewLabel) ->
		Label = NewLabel
	;
		Label = Label0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
