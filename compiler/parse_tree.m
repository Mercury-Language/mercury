%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
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
:- import_module hlds. % XXX for hlds_data__cons_id
:- import_module backend_libs. % XXX for `foreign'

% The parse tree data type itself.
:- include_module prog_data, (inst).
	% XXX inst uses hlds_data__cons_id

% The parser.
:- include_module prog_io.
   :- include_module prog_io_goal, prog_io_dcg, prog_io_pragma.
   :- include_module prog_io_typeclass, prog_io_util.

% Pretty-printers.
:- include_module prog_out, mercury_to_mercury.

% Utility routines.
:- include_module prog_util.

% Transformations that act on the parse tree,
% and stuff relating to the module system.
:- include_module equiv_type.
:- include_module modules, module_qual.

% (Note that intermod and trans_opt also contain routines that
% act on the parse tree, but those modules are considered part
% of the HLDS transformations package.)
% :- include_module intermod, trans_opt.

% :- implementation.

% XXX lots of stuff uses hlds_data__type_id and type_util.m.
% XXX modules.m uses llds_out for the init names.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.     % XXX for type_util.m
:- import_module transform_hlds. % XXX for write_pragma_termination_info
				     % in termination.m, which is used by
				     % mercury_to_mercury.m
:- import_module ll_backend.     % XXX for llds_out.m, which is used
				     % by modules__append_to_init_list,
				     % which creates the LLDS and RL
				     % initialization code.
:- end_module parse_tree.

%-----------------------------------------------------------------------------%
