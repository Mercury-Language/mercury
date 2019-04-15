%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2007, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: stdlabel.m.
% Author: zs.
%
% Module to rename (actually renumber) all the labels in a procedure so that
% the label numbers are in a dense ascending sequence.
%
% This transformation leaves the performance of the program unaffected.
% However, one can use it make the effect of an optimization much more readily
% apparent. The idea is that enabling --standardize-labels when compiling
% a program both with and without a new or modified optimization cleans up
% the output of the diff between the two sets of resulting .c files, by
% eliminating incidental differences in labels that could otherwise overwhelm
% the real differences made by the optimization.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.stdlabel.
:- interface.

:- import_module ll_backend.llds.

:- import_module counter.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred standardize_labels(list(instruction)::in, list(instruction)::out,
    counter::in, counter::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend.opt_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%

standardize_labels(Instrs0, Instrs, _, !:ProcCounter) :-
    opt_util.get_prologue(Instrs0, LabelInstr, Comments, Instrs1),
    ( if
        LabelInstr = llds_instr(label(FirstLabel), _),
        FirstLabel = entry_label(_, ProcLabel)
    then
        build_std_map(Instrs1, ProcLabel, counter.init(1), !:ProcCounter,
            map.init, Map),
        replace_labels_instruction_list(Instrs1, Instrs2, Map, yes, yes),
        Instrs = [LabelInstr | Comments] ++ Instrs2
    else
        unexpected($pred, "no proc_label")
    ).

%-----------------------------------------------------------------------------%

:- pred build_std_map(list(instruction)::in, proc_label::in,
    counter::in, counter::out,
    map(label, label)::in, map(label, label)::out) is det.

build_std_map([], _, !Counter, !Map).
build_std_map([Instr | Instrs], ProcLabel, !Counter, !Map) :-
    ( if Instr = llds_instr(label(Label), _) then
        counter.allocate(LabelNum, !Counter),
        StdLabel = internal_label(LabelNum, ProcLabel),
        map.det_insert(Label, StdLabel, !Map)
    else
        true
    ),
    build_std_map(Instrs, ProcLabel, !Counter, !Map).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.stdlabel.
%-----------------------------------------------------------------------------%
