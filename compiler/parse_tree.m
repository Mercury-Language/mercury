%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains the parse tree data structure,
% and modules for parsing and for manipulating parse trees.
%
% It corresponds to the parts of "Phase 1: Parsing"
% in notes/compiler_design.html up to (but not including) make_hlds.m.
%

:- module parse_tree.
:- interface.

:- import_module libs.
:- import_module mdbcomp.
:- import_module recompilation.

% The parse tree data type itself.
% The parse tree is split in two.  The parts defined in prog_item is
% only needed in the frontend of the compiler, the parts in prog_data
% are needed throughout.
:- include_module prog_item.
:- include_module prog_data.

% The parser.
:- include_module prog_io. 
   :- include_module prog_io_dcg.
   :- include_module prog_io_goal.
   :- include_module prog_io_pragma.
   :- include_module prog_io_typeclass.
   :- include_module prog_io_util.

% Parser/pretty-printer/utility routines for the ctgc related types.
:- include_module prog_ctgc.

% Pretty-printers.
:- include_module mercury_to_mercury.
:- include_module prog_out.

% Utility routines.
:- include_module error_util.
:- include_module prog_event.
:- include_module prog_foreign.
:- include_module prog_mode.
:- include_module prog_mutable.
:- include_module prog_type.
:- include_module prog_type_subst.
:- include_module prog_util.

% Transformations that act on the parse tree,
% and stuff relating to the module system.
:- include_module equiv_type.
:- include_module modules.
:- include_module module_qual.
:- include_module source_file_map.

% (Note that intermod and trans_opt also contain routines that
% act on the parse tree, but those modules are considered part
% of the HLDS transformations package.)
% :- include_module intermod.
% :- include_module trans_opt.

%-----------------------------------------------------------------------------%
:- end_module parse_tree.
%-----------------------------------------------------------------------------%
