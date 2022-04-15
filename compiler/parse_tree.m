%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
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

% The parse tree data type itself.
% The parse tree is split in two. The parts defined in prog_item are needed
% only by the frontend of the compiler, the parts in prog_data* are needed
% throughout.
:- include_module prog_item.
:- include_module prog_data.
:- include_module prog_data_event.
:- include_module prog_data_foreign.
:- include_module prog_data_pragma.
:- include_module prog_data_used_modules.
:- include_module file_kind.

% The parser.
:- include_module parse_module.
   :- include_module parse_dcg_goal.
   :- include_module parse_error.
   :- include_module parse_goal.
   :- include_module parse_inst_mode_defn.
   :- include_module parse_inst_mode_name.
   :- include_module parse_item.
   :- include_module parse_mutable.
   :- include_module parse_pragma.
   :- include_module parse_pragma_analysis.
   :- include_module parse_pragma_foreign.
   :- include_module parse_pragma_tabling.
   :- include_module parse_sym_name.
   :- include_module parse_type_defn.
   :- include_module parse_type_name.
   :- include_module parse_type_repn.
   :- include_module parse_class.
   :- include_module parse_vars.

   :- include_module find_module.
   :- include_module parse_types.
   :- include_module parse_util.

% Parser/pretty-printer/utility routines for the ctgc related types.
:- include_module prog_ctgc.

% Pretty-printers.
:- include_module mercury_to_mercury.
:- include_module parse_tree_out.
:- include_module parse_tree_out_clause.
:- include_module parse_tree_out_inst.
:- include_module parse_tree_out_pragma.
:- include_module parse_tree_out_pred_decl.
:- include_module parse_tree_out_term.
:- include_module parse_tree_out_type_repn.
:- include_module parse_tree_out_info.
:- include_module prog_out.
:- include_module parse_tree_to_term.

% Utility data structures.
:- include_module set_of_var.

% Utility routines.
:- include_module builtin_lib_types.
:- include_module check_type_inst_mode_defns.
:- include_module error_util.
:- include_module item_util.
:- include_module maybe_error.
:- include_module pred_name.
:- include_module prog_detism.
:- include_module prog_event.
:- include_module prog_foreign.
:- include_module prog_foreign_enum.
:- include_module prog_item_stats.
:- include_module prog_mode.
:- include_module prog_mutable.
:- include_module prog_rename.
:- include_module prog_type.
:- include_module prog_type_subst.
:- include_module prog_util.

% Type representation.
:- include_module decide_type_repn.

% Transformations that act on the parse tree,
% and stuff relating to the module system.
:- include_module canonicalize_interface.
:- include_module check_module_interface.
:- include_module comp_unit_interface.
:- include_module convert_parse_tree.
:- include_module deps_map.
:- include_module equiv_type.
:- include_module file_names.
:- include_module generate_dep_d_files.
:- include_module get_dependencies.
:- include_module grab_modules.
:- include_module module_baggage.
:- include_module module_cmds.
:- include_module module_dep_info.
:- include_module module_deps_graph.
:- include_module module_qual.
:- include_module read_modules.
:- include_module source_file_map.
:- include_module split_parse_tree_src.
:- include_module write_deps_file.
:- include_module write_module_interface_files.

% Java and C# related utilities.
:- include_module java_names.

% (Note that intermod and trans_opt also contain routines that
% act on the parse tree, but those modules are considered part
% of the HLDS transformations package.)
% :- include_module intermod.
% :- include_module trans_opt.

%-----------------------------------------------------------------------------%
:- end_module parse_tree.
%-----------------------------------------------------------------------------%
