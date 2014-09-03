%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ctgc.m
% Main author: nancy
%
% Package grouping all the modules that are used for compile-time garbage
% collection.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.
:- interface.

:- import_module parse_tree.

:- include_module datastruct.
:- include_module fixpoint_table.
:- include_module livedata.
:- include_module selector.
:- include_module structure_reuse.
:- include_module structure_sharing.
:- include_module util.

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.
%-----------------------------------------------------------------------------%
