%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
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
:- import_module backend_libs. % XXX for `foreign'
:- import_module mdbcomp.
:- import_module recompilation.

% The parse tree data type itself.
:- include_module prog_data.

% The parser.
:- include_module prog_io.
   :- include_module prog_io_dcg.
   :- include_module prog_io_goal.
   :- include_module prog_io_pragma.
   :- include_module prog_io_typeclass.
   :- include_module prog_io_util.

% Pretty-printers.
:- include_module mercury_to_mercury.
:- include_module prog_out.

% Utility routines.
:- include_module prog_mode.
:- include_module prog_util.
:- include_module error_util.

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

% XXX lots of stuff uses hlds_data__type_id and type_util.m.
% XXX modules.m uses llds_out for the init names.

:- implementation.

:- import_module check_hlds.     % XXX for type_util.m
:- end_module parse_tree.

%-----------------------------------------------------------------------------%
