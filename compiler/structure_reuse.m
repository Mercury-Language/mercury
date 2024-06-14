%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006, 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.m.
% Main author: nancy.
%
% Package grouping all the modules related to the structure reuse analysis
% part of compile-time garbage collection.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.

:- interface.

:- include_module analysis.
    :- include_module lfu.
    :- include_module lbu.
    :- include_module direct.
        :- include_module dead_cell_table.
        :- include_module direct_choose_reuse.
        :- include_module direct_detect_garbage.
    :- include_module indirect.
    :- include_module versions.

:- include_module domain.

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.
%-----------------------------------------------------------------------------%
