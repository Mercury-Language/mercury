%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2003-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: code_model.m.
%
% This module defines the `code_model' data type, and associated procedures.
% The `code_model' type is a simplified version of the `determinism' type
% that is defined in prog_data.m.  It ignores most of the distinctions in
% the determinism type and keeps only the distinctions that are important
% for code generation.
%
% We define this in a different module than the `determinism' type because
% it is only used by some of the different back-ends, not all of them.
% It is used by the MLDS, LLDS, and bytecode back-ends.
%
%-----------------------------------------------------------------------------%

:- module hlds.code_model.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- type code_model
    --->    model_det       % functional & total
    ;       model_semi      % just functional
    ;       model_non.      % not functional

:- pred determinism_to_code_model(determinism, code_model).
:- mode determinism_to_code_model(in, out) is det.
:- mode determinism_to_code_model(out, in) is multi.

:- func proc_info_interface_code_model(proc_info) = code_model.

:- func goal_info_get_code_model(hlds_goal_info) = code_model.

    % Construct a representation of the interface determinism of a procedure.
    % The code we have chosen is not sequential; instead it encodes the various
    % properties of each determinism. This must match the encoding of
    % MR_Determinism in mercury_stack_layout.h.
    %
    % The 8 bit is set iff the context is first_solution.
    % The 4 bit is set iff the min number of solutions is more than zero.
    % The 2 bit is set iff the max number of solutions is more than zero.
    % The 1 bit is set iff the max number of solutions is more than one.
    %
:- func represent_determinism(determinism) = int.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.

determinism_to_code_model(detism_det,       model_det).
determinism_to_code_model(detism_semi,      model_semi).
determinism_to_code_model(detism_non,       model_non).
determinism_to_code_model(detism_multi,     model_non).
determinism_to_code_model(detism_cc_non,    model_semi).
determinism_to_code_model(detism_cc_multi,  model_det).
determinism_to_code_model(detism_erroneous, model_det).
determinism_to_code_model(detism_failure,   model_semi).

proc_info_interface_code_model(ProcInfo) = CodeModel :-
    proc_info_interface_determinism(ProcInfo, Determinism),
    determinism_to_code_model(Determinism, CodeModel).

goal_info_get_code_model(GoalInfo) = CodeModel :-
    Determinism = goal_info_get_determinism(GoalInfo),
    determinism_to_code_model(Determinism, CodeModel).

represent_determinism(detism_det) = detism_rep(det_rep).
represent_determinism(detism_semi) = detism_rep(semidet_rep).
represent_determinism(detism_non) = detism_rep(nondet_rep).
represent_determinism(detism_multi) = detism_rep(multidet_rep).
represent_determinism(detism_erroneous) = detism_rep(erroneous_rep).
represent_determinism(detism_failure) = detism_rep(failure_rep).
represent_determinism(detism_cc_non) = detism_rep(cc_nondet_rep).
represent_determinism(detism_cc_multi) = detism_rep(cc_multidet_rep).

%-----------------------------------------------------------------------------%
:- end_module hlds.code_model.
%-----------------------------------------------------------------------------%
