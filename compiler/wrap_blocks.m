%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2003, 2005-2007, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: wrap_blocks.m
% Author: zs.
%
% The optimizations in use_local_vars.m insert into instruction sequences
% references to temporary variables whose values need be preserved only within
% an extended basic block. The wrap_blocks pass looks for references to
% temporaries and introduces block instructions whenever it sees them. These
% block instructions go from the first reference to a temporary to the end of
% its extended basic block.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.wrap_blocks.
:- interface.

:- import_module ll_backend.llds.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred wrap_blocks(list(instruction)::in, list(instruction)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend.opt_util.

:- import_module bool.
:- import_module int.
:- import_module require.

%-----------------------------------------------------------------------------%

wrap_blocks(Instrs0, Instrs) :-
    wrap_instrs(Instrs0, 0, 0, [], Instrs).

    % R is the number of the highest numbered tempr variable seen so far;
    % R = 0 means we haven't seen any temp variables. Similarly, F is the
    % highest numbered tempf variable seen so far. RevSofar is a
    % reversed list of instructions starting with the first instruction
    % in this block that accesses a temp variable. Invariant: RevSofar
    % is always empty if R = 0 and F = 0.
    %
:- pred wrap_instrs(list(instruction)::in, int::in, int::in,
    list(instruction)::in, list(instruction)::out) is det.

wrap_instrs([], R, F, RevSofar, []) :-
    (
        RevSofar = [_ | _],
        unexpected($pred, "procedure ends with fallthrough")
    ;
        RevSofar = [],
        ( if ( R > 0 ; F > 0 ) then
            unexpected($pred, "procedure ends without closing block")
        else
            true
        )
    ).
wrap_instrs([Instr0 | Instrs0], R0, F0, RevSofar, Instrs) :-
    Instr0 = llds_instr(Uinstr0, _Comment0),
    opt_util.count_temps_instr(Uinstr0, R0, R1, F0, F1),
    ( if ( R1 > 0 ; F1 > 0) then
        % We must close the block before a label, since you can jump
        % to a label from other blocks.
        %
        % Call instructions cannot fall through, but they cannot refer
        % to the temp variables declared by the block either, so we
        % close the block either just before or just after the call
        % instruction. We close the block before the call instruction,
        % because including it in the block causes the test case
        % debugger/all_solutions to fail.

        ( if ( Uinstr0 = label(_) ; Uinstr0 = llcall(_, _, _, _, _, _) ) then
            list.reverse(RevSofar, BlockInstrs),
            wrap_instrs(Instrs0, 0, 0, [], Instrs1),
            BlockInstr = llds_instr(block(R1, F1, BlockInstrs), ""),
            Instrs = [BlockInstr, Instr0 | Instrs1]
        else if opt_util.can_instr_fall_through(Uinstr0) = no then
            list.reverse([Instr0 | RevSofar], BlockInstrs),
            wrap_instrs(Instrs0, 0, 0, [], Instrs1),
            BlockInstr = llds_instr(block(R1, F1, BlockInstrs), ""),
            Instrs = [BlockInstr | Instrs1]
        else
            wrap_instrs(Instrs0, R1, F1, [Instr0 | RevSofar], Instrs)
        )
    else
        wrap_instrs(Instrs0, 0, 0, [], Instrs1),
        Instrs = [Instr0 | Instrs1]
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.wrap_blocks.
%-----------------------------------------------------------------------------%
