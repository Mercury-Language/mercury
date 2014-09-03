%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% The MLDS back-end.
%
% This package includes
% - the MLDS data structure, which is an abstract
%   representation of a generic imperative language;
% - the MLDS code generator, which converts HLDS to MLDS;
% - the high-level C back-end, the Java back-end, the .NET back-end,
%   and a wrapper for the assembler back-end,
%   each of which convert MLDS to their respective target language.
%
% The main part of the assembler back-end, which converts MLDS to GCC's
% internal abstract syntax trees and then invokes the GCC back-end to convert
% this to assembler, is in a package of its own, so that this package doesn't
% depend on the GCC back-end.
%
:- module ml_backend.
:- interface.

:- import_module hlds.
:- import_module parse_tree.

%-----------------------------------------------------------------------------%

:- include_module mlds.
:- include_module ml_target_util.
:- include_module ml_util.

% Phase 4-ml: MLDS-specific HLDS to HLDS transformations and annotations.
:- include_module add_heap_ops.         % transformation
:- include_module add_trail_ops.        % transformation

% Phase 5-ml: compile HLDS to MLDS
:- include_module ml_proc_gen.
:- include_module ml_code_gen.
   :- include_module ml_call_gen.
   :- include_module ml_foreign_proc_gen.
   :- include_module ml_closure_gen.
   :- include_module ml_commit_gen.
   :- include_module ml_disj_gen.
   :- include_module ml_switch_gen.
      :- include_module ml_simplify_switch.
      :- include_module ml_string_switch.
      :- include_module ml_tag_switch.
      :- include_module ml_lookup_switch.
   :- include_module ml_type_gen.
   :- include_module ml_unify_gen.
:- include_module ml_gen_info.
:- include_module ml_code_util.
:- include_module ml_accurate_gc.
:- include_module ml_global_data.
:- include_module rtti_to_mlds.

% Phase 6-ml: MLDS -> MLDS transformations
:- include_module ml_elim_nested.
:- include_module ml_optimize.
:- include_module ml_tailcall.

% Phase 7-ml: compile MLDS to target code

% MLDS->C back-end
:- include_module mlds_to_c.

% MLDS->Java back-end
:- include_module mlds_to_java.
:- include_module java_util.

% MLDS->C# back-end.
:- include_module mlds_to_cs.

% MLDS->.NET CLR back-end
:- include_module il_peephole.
:- include_module ilasm.
:- include_module ilds.
:- include_module mlds_to_il.
:- include_module mlds_to_ilasm.
:- include_module mlds_to_managed.

:- implementation.

:- import_module backend_libs.
:- import_module libs.
:- import_module check_hlds.    % needed for type_util, mode_util,
                                % and polymorphism.
:- import_module mdbcomp.

%-----------------------------------------------------------------------------%
:- end_module ml_backend.
%-----------------------------------------------------------------------------%
