%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003-2007, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: delay_slot.m.
% Main author: zs.
%
% This module attempts to fill the delay slots of branch instructions.
% Since it assumes the existence of one delay slot after every branch,
% this module should not be invoked if the architecture we are targeting
% has no delay slots on its branch instructions.
%
% Since we are generating C code, not assembler, we cannot fill slots
% directly. However, we can let the C compiler know that the instruction
% immediately after a condition branch instruction can be safely executed even
% if the branch is not taken. We can do so by moving the instruction before
% the conditional branch. This actually requires the instruction to be
% executed in both continuations, therefore we want to do this only if there
% is no chance that the C compiler would be able to fill the delay slot any
% other way.
%
% The only code pattern that we recognize as being both safe and advantageous
% to transform the code in order to fill a delay slot is this pattern,
% produced by frameopt:
%
%   L1:
%       if (cond) goto L2
%       incr_sp(N)
%       stackvar(N) = succip
%       ...
%
% We transform this code into:
%
%   L1:
%       stackvar(0) = succip
%       if (cond) goto L2
%       incr_sp(N)
%       ...
%
% The initial store into stackvar(0) is into the first word above the
% current detstack top. After the incr_sp is executed (if it ever is), this
% word will become the bottom stack slot of the new frame, i.e. stackvar(N).
%
%-----------------------------------------------------------------------------%

:- module ll_backend.delay_slot.
:- interface.

:- import_module ll_backend.llds.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred fill_branch_delay_slot(list(instruction)::in, list(instruction)::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%-----------------------------------------------------------------------------%

fill_branch_delay_slot([], []).
fill_branch_delay_slot([Instr0 | Instrs0], Instrs) :-
    (
        Instr0 = llds_instr(label(_), _),
        Instrs0 = [Instr1, Instr2, Instr3 | Tail0],
        Instr1 = llds_instr(if_val(_, _), _),
        Instr2 = llds_instr(incr_sp(Size, _, _), _),
        Instr3 = llds_instr(assign(stackvar(Size), lval(succip)), C2)
    ->
        fill_branch_delay_slot(Tail0, Tail1),
        string.append(C2, " (early save in delay slot)", NewC2),
        EarlySave = llds_instr(assign(stackvar(0), lval(succip)), NewC2),
        Instrs = [Instr0, EarlySave, Instr1, Instr2 | Tail1]
    ;
        fill_branch_delay_slot(Instrs0, Instrs1),
        Instrs = [Instr0 | Instrs1]
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.delay_slot.
%-----------------------------------------------------------------------------%
