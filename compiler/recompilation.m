%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: recompilation.m.
% Main author: stayl.
%
% Type declarations for smart recompilation.
% Predicates to record program items used by a compilation.
%
% A module must be recompiled if
% - The file itself has changed.
% - An imported item used in compiling the module has changed or been removed.
% - An item has been added to an imported module which could cause an
%   ambiguity with an item used in compiling the module.
%
% Currently smart recompilation does not work properly with
% inter-module optimization. If a `.opt' file changes, all modules
% importing it need to be recompiled.
%
%-----------------------------------------------------------------------------%

:- module recompilation.
:- interface.

:- include_module recompilation.check.
:- include_module recompilation.item_types.
:- include_module recompilation.record_uses.
:- include_module recompilation.usage.
:- include_module recompilation.used_file.
:- include_module recompilation.version.

%-----------------------------------------------------------------------------%
:- end_module recompilation.
%-----------------------------------------------------------------------------%
