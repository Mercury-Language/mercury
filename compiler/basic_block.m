%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2001,2003-2005 The University of Melbourne.
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

:- module ll_backend__basic_block.

:- interface.

:- import_module ll_backend__llds.
:- import_module mdbcomp__prim_data.

:- import_module bool.
:- import_module counter.
:- import_module list.
:- import_module map.
:- import_module std_util.

:- type block_map   ==  map(label, block_info).

:- type block_info
    --->    block_info(
                starting_label      :: label,
                                    % The label starting the block.

                label_instr         :: instruction,
                                    % The instruction containing the label.

                later_instrs        :: list(instruction),
                                    % The code of the block without the initial
                                    % label.

                fallen_into         :: bool,
                                    % Does the previous block (if any)
                                    % fall through to this block?

                jump_dests          :: list(label),
                                    % The labels we can jump to
                                    % (not falling through).

                fall_dest           :: maybe(label)
                                    % The label we fall through to
                                    % (if there is one).
            ).

:- pred create_basic_blocks(list(instruction)::in, list(instruction)::out,
    proc_label::in, counter::in, counter::out,
    list(label)::out, block_map::out) is det.

:- pred flatten_basic_blocks(list(label)::in, block_map::in,
    list(instruction)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend__opt_util.

:- import_module int.
:- import_module require.

create_basic_blocks(Instrs0, Comments, ProcLabel, !C, LabelSeq, BlockMap) :-
    opt_util__get_prologue(Instrs0, LabelInstr, Comments, AfterLabelInstrs),
    Instrs1 = [LabelInstr | AfterLabelInstrs],
    build_block_map(Instrs1, LabelSeq, ProcLabel, no, map__init, BlockMap, !C).

%-----------------------------------------------------------------------------%

    % Build up the block map. As we go along, we add labels to the given
    % instruction sequence so that every basic block has labels around it.
    %
:- pred build_block_map(list(instruction)::in, list(label)::out,
    proc_label::in, bool::in, block_map::in, block_map::out,
    counter::in, counter::out) is det.

build_block_map([], [], _, _, !BlockMap, !C).
build_block_map([OrigInstr0 | OrigInstrs0], LabelSeq, ProcLabel, FallInto,
        !BlockMap, !C) :-
    ( OrigInstr0 = label(OrigLabel) - _ ->
        Label = OrigLabel,
        LabelInstr = OrigInstr0,
        RestInstrs = OrigInstrs0
    ;
        counter__allocate(N, !C),
        Label = internal(N, ProcLabel),
        LabelInstr = label(Label) - "",
        RestInstrs = [OrigInstr0 | OrigInstrs0]
    ),
    (
        take_until_end_of_block(RestInstrs, BlockInstrs, Instrs1),
        build_block_map(Instrs1, LabelSeq1, ProcLabel, NextFallInto, !BlockMap,
            !C),
        ( list__last(BlockInstrs, LastInstr) ->
            LastInstr = LastUinstr - _,
            opt_util__possible_targets(LastUinstr, SideLabels),
            opt_util__can_instr_fall_through(LastUinstr, NextFallInto)
        ;
            SideLabels = [],
            NextFallInto = yes
        ),
        (
            NextFallInto = yes,
            get_fallthrough_from_seq(LabelSeq1, MaybeFallThrough)
        ;
            NextFallInto = no,
            MaybeFallThrough = no
        ),
        BlockInfo = block_info(Label, LabelInstr, BlockInstrs, FallInto,
            SideLabels, MaybeFallThrough),
        map__det_insert(!.BlockMap, Label, BlockInfo, !:BlockMap),
        LabelSeq = [Label | LabelSeq1]
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

flatten_basic_blocks([], _, []).
flatten_basic_blocks([Label | Labels], BlockMap, Instrs) :-
    flatten_basic_blocks(Labels, BlockMap, RestInstrs),
    map__lookup(BlockMap, Label, BlockInfo),
    BlockInfo = block_info(_, BlockLabelInstr, BlockInstrs, _, _, _),
    list__append([BlockLabelInstr | BlockInstrs], RestInstrs, Instrs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
