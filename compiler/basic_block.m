%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: zs.
%
% This module defines a representation for basic blocks, sequences of
% instructions with one entry and one exit, and provides predicates
% that convert a list of instructions into a list of basic blocks
% and vice versa.

%-----------------------------------------------------------------------------%

:- module basic_block.

:- interface.

:- import_module llds.
:- import_module list, map, std_util.

:- type block_map	==	map(label, block_info).

:- type block_info
	--->	block_info(
			label,
				% The label starting the block.
			instruction,
				% The instruction containing the label.
			list(instruction),
				% The code of the block without the initial
				% label.
			list(label),
				% The labels we can jump to
				% (not falling through).
			maybe(label)
				% The label we fall through to
				% (if there is one).
		).

:- pred create_basic_blocks(list(instruction)::in, list(instruction)::out,
	proc_label::out, int::out, list(label)::out, block_map::out) is det.

:- pred flatten_basic_blocks(list(label)::in, block_map::in,
        list(instruction)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module opt_util.
:- import_module bool, int, require.

create_basic_blocks(Instrs0, Comments, ProcLabel, N,
		LabelSeq, BlockMap) :-
	opt_util__get_prologue(Instrs0, ProcLabel, LabelInstr,
		Comments, AfterLabelInstrs),
	Instrs1 = [LabelInstr | AfterLabelInstrs],
	opt_util__new_label_no(Instrs0, 1000, N0),
	map__init(BlockMap0),
	build_block_map(Instrs1, LabelSeq, BlockMap0, BlockMap,
		ProcLabel, N0, N).

	% Add labels to the given instruction sequence so that
	% every basic block has labels around it.

%-----------------------------------------------------------------------------%

:- pred build_block_map(list(instruction)::in, list(label)::out,
	block_map::in, block_map::out, proc_label::in, int::in, int::out)
	is det.

build_block_map([], [], BlockMap, BlockMap, _, N, N).
build_block_map([OrigInstr0 | OrigInstrs0], LabelSeq, BlockMap0, BlockMap,
		ProcLabel, N0, N) :-
	( OrigInstr0 = label(OrigLabel) - _ ->
		Label = OrigLabel,
		LabelInstr = OrigInstr0,
		RestInstrs = OrigInstrs0,
		N1 = N0
	;
		N1 is N0 + 1,
		Label = local(ProcLabel, N0),
		LabelInstr = label(Label) - "",
		RestInstrs = [OrigInstr0 | OrigInstrs0]
	),
	( 
		take_until_end_of_block(RestInstrs, BlockInstrs, Instrs1),
		build_block_map(Instrs1, LabelSeq0,
			BlockMap0, BlockMap1, ProcLabel, N1, N),
		( list__last(BlockInstrs, LastInstr) ->
			LastInstr = LastUinstr - _,
			possible_targets(LastUinstr, SideLabels),
			opt_util__can_instr_fall_through(LastUinstr,
				CanFallThrough),
			( CanFallThrough = yes ->
				get_fallthrough_from_seq(LabelSeq0,
					MaybeFallThrough)
			;
				MaybeFallThrough = no
			)
		;
			SideLabels = [],
			get_fallthrough_from_seq(LabelSeq0,
				MaybeFallThrough)
		),
		BlockInfo = block_info(Label, LabelInstr, BlockInstrs,
			SideLabels, MaybeFallThrough),
		map__det_insert(BlockMap1, Label, BlockInfo, BlockMap),
		LabelSeq = [Label | LabelSeq0]
	).

%-----------------------------------------------------------------------------%

:- pred take_until_end_of_block(list(instruction)::in,
	list(instruction)::out, list(instruction)::out) is det.

take_until_end_of_block([], [], []).
take_until_end_of_block([Instr0 | Instrs0], BlockInstrs, Rest) :-
	Instr0 = Uinstr0 - _Comment,
	( Uinstr0 = label(_) ->
		BlockInstrs = [],
		Rest = [Instr0 | Instrs0]
	; opt_util__can_instr_branch_away(Uinstr0, yes) ->
		BlockInstrs = [Instr0],
		Rest = Instrs0
	;
		take_until_end_of_block(Instrs0, BlockInstrs1, Rest),
		BlockInstrs = [Instr0 | BlockInstrs1]
	).

%-----------------------------------------------------------------------------%

:- pred get_fallthrough_from_seq(list(label)::in, maybe(label)::out) is det.

get_fallthrough_from_seq(LabelSeq, MaybeFallThrough) :-
	( LabelSeq = [NextLabel | _] ->
		MaybeFallThrough = yes(NextLabel)
	;
		MaybeFallThrough = no
	).

%-----------------------------------------------------------------------------%

	% Given an instruction, find the set of labels to which it can cause
	% control to transfer. In the case of calls, this includes transfer
	% via return from the called procedure.

:- pred possible_targets(instr::in, list(label)::out) is det.

possible_targets(comment(_), []).
possible_targets(livevals(_), []).
possible_targets(block(_, _, _), _) :-
	error("block in possible_targets").
possible_targets(assign(_, _), []).
possible_targets(call(_, ReturnAddr, _, _), Labels) :-
	( ReturnAddr = label(Label) ->
		Labels = [Label]
	;
		Labels = []
	).
possible_targets(mkframe(_, _, _, _), []).
possible_targets(modframe(_), []).
possible_targets(label(_), []).
possible_targets(goto(CodeAddr), Targets) :-
	( CodeAddr = label(Label) ->
		Targets = [Label]
	;
		Targets = []
	).
possible_targets(computed_goto(_, Targets), Targets).
possible_targets(c_code(_), []).
possible_targets(if_val(_, CodeAddr), Targets) :-
	( CodeAddr = label(Label) ->
		Targets = [Label]
	;
		Targets = []
	).
possible_targets(incr_hp(_, _, _, _), []).
possible_targets(mark_hp(_), []).
possible_targets(restore_hp(_), []).
possible_targets(store_ticket(_), []).
possible_targets(reset_ticket(_, _), []).
possible_targets(discard_ticket, []).
possible_targets(mark_ticket_stack(_), []).
possible_targets(discard_tickets_to(_), []).
possible_targets(incr_sp(_, _), []).
possible_targets(decr_sp(_), []).
possible_targets(init_sync_term(_, _), []).
possible_targets(fork(P, C, _), [P, C]).
possible_targets(join_and_terminate(_), []).
possible_targets(join_and_continue(_, L), [L]).
possible_targets(pragma_c(_, _, _, _, _), []).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

flatten_basic_blocks([], _, []).
flatten_basic_blocks([Label | Labels], BlockMap, Instrs) :-
	flatten_basic_blocks(Labels, BlockMap, RestInstrs),
	map__lookup(BlockMap, Label, BlockInfo),
	BlockInfo = block_info(_, BlockLabelInstr, BlockInstrs, _, _),
	list__append([BlockLabelInstr | BlockInstrs], RestInstrs, Instrs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
