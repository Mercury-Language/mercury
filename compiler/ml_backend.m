%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
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
% The main part of the assembler back-end, which converts MLDS
% to GCC's internal abstract syntax trees and then invokes the
% GCC back-end to convert this to assembler, is in a package of
% its own, so that this package doesn't depend on the GCC back-end.
%
:- module ml_backend.
:- interface.
:- import_module transform_hlds, check_hlds. % are these needed?
:- import_module hlds, parse_tree, libs, backend_libs.
:- import_module ll_backend. % XXX needed for llds_out__name_mangle, etc.

%-----------------------------------------------------------------------------%

:- include_module mlds.
:- include_module ml_util.

% Phase 4-ml: MLDS-specific HLDS to HLDS transformations and annotations.
:- include_module add_heap_ops, add_trail_ops. % transformations
:- include_module mark_static_terms. % annotation

% Phase 5-ml: compile HLDS to MLDS
:- include_module ml_code_gen.
   :- include_module ml_type_gen.
   :- include_module ml_call_gen.
   :- include_module ml_unify_gen, ml_closure_gen.
   :- include_module ml_switch_gen.
      :- include_module ml_string_switch, ml_tag_switch, ml_simplify_switch.
:- include_module ml_code_util.
:- include_module rtti_to_mlds.

% Phase 6-ml: MLDS -> MLDS transformations
:- include_module ml_elim_nested.
:- include_module ml_tailcall.
:- include_module ml_optimize.

% Phase 7-ml: compile MLDS to target code

% MLDS->C back-end
:- include_module mlds_to_c.

% MLDS->Assembler back-end
:- include_module maybe_mlds_to_gcc.
% :- include_module mlds_to_gcc, gcc.

% MLDS->Java back-end
:- include_module mlds_to_java, java_util.

% MLDS->.NET CLR back-end
:- include_module mlds_to_il.
:- include_module mlds_to_ilasm.
:- include_module mlds_to_managed.
:- include_module ilds.
:- include_module ilasm.
:- include_module il_peephole.

:- end_module ml_backend.

%-----------------------------------------------------------------------------%
