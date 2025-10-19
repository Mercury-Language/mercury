%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2006, 2009-2011 The University of Melbourne.
% Copyright (C) 2013-2018, 2023, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% The MLDS back-end.
%
% This package includes
% - the MLDS data structure, which is an abstract representation
%   of a generic imperative language;
%
% - the MLDS code generator, which converts HLDS to MLDS;
%
% - the code generators that convert MLDS to one of our three target languages:
%
%   - the high-level C back-end,
%   - the Java back-end, and
%   - the C# back-end.
%
%---------------------------------------------------------------------------%

:- module ml_backend.
:- interface.

%---------------------------------------------------------------------------%

% Phase 4-ml: MLDS-specific pre-passes to transform or annotate the HLDS.
:- include_module add_heap_ops.         % transform
:- include_module add_trail_ops.        % transform

% Phase 5-ml: compile HLDS to MLDS.

% The MLDS data structure itself.
:- include_module mlds.

% The HLDS->MLDS code generator.
:- include_module ml_top_gen.
:- include_module rtti_to_mlds.

% Phase 6-ml: MLDS -> MLDS transformations.
:- include_module ml_elim_nested.
:- include_module ml_optimize.

% Phase 7-ml: compile MLDS to target code.

% MLDS->C back-end.
:- include_module mlds_to_c_file.
:- include_module mlds_to_c_name.
:- include_module mlds_to_c_util.

% MLDS->Java back-end.
:- include_module mlds_to_java_file.

% MLDS->C# back-end.
:- include_module mlds_to_cs_file.

% For debugging the modules in this package.
:- include_module mlds_dump.

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

% Phase 5-ml: compile HLDS to MLDS.

% Utility predicates on the MLDS data structure.
:- include_module ml_target_util.
:- include_module ml_util.

% The HLDS->MLDS code generator.
:- include_module ml_proc_gen.
    :- include_module ml_code_gen.
    :- include_module ml_unify_gen.
        :- include_module ml_unify_gen_construct.
        :- include_module ml_unify_gen_deconstruct.
        :- include_module ml_unify_gen_test.
        :- include_module ml_unify_gen_util.
        :- include_module ml_closure_gen.
    :- include_module ml_call_gen.
    :- include_module ml_foreign_proc_gen.
    :- include_module ml_disj_gen.
    :- include_module ml_switch_gen.
        :- include_module ml_simplify_switch.
        :- include_module ml_string_switch.
        :- include_module ml_tag_switch.
        :- include_module ml_lookup_switch.
    :- include_module ml_commit_gen.

    :- include_module ml_gen_info.
    :- include_module ml_args_util.
    :- include_module ml_code_util.
:- include_module ml_accurate_gc.
:- include_module ml_type_gen.
:- include_module ml_global_data.

% Phase 6-ml: MLDS -> MLDS transformations.
:- include_module ml_rename_classes.
:- include_module ml_unused_assign.

% Phase 7-ml: compile MLDS to target code.

% MLDS->C back-end.
:- include_module mlds_to_c_class.
:- include_module mlds_to_c_data.
:- include_module mlds_to_c_export.
:- include_module mlds_to_c_func.
:- include_module mlds_to_c_global.
:- include_module mlds_to_c_stmt.
:- include_module mlds_to_c_type.

% MLDS->Java back-end.
:- include_module mlds_to_java_class.
:- include_module mlds_to_java_data.
:- include_module mlds_to_java_export.
:- include_module mlds_to_java_func.
:- include_module mlds_to_java_global.
:- include_module mlds_to_java_name.
:- include_module mlds_to_java_stmt.
:- include_module mlds_to_java_type.
:- include_module mlds_to_java_util.
:- include_module mlds_to_java_wrap.

% MLDS->C# back-end.
:- include_module mlds_to_cs_class.
:- include_module mlds_to_cs_data.
:- include_module mlds_to_cs_export.
:- include_module mlds_to_cs_func.
:- include_module mlds_to_cs_global.
:- include_module mlds_to_cs_stmt.
:- include_module mlds_to_cs_type.
:- include_module mlds_to_cs_name.
:- include_module mlds_to_cs_util.

% Utility predicates that are useful for more than one MLDS->? back-end.
:- include_module mlds_to_target_util.

%---------------------------------------------------------------------------%
:- end_module ml_backend.
%---------------------------------------------------------------------------%
