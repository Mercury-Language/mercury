%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module backend_libs__code_model.

% This module defines the `code_model' data type, and associated procedures.
% The `code_model' type is a simplified version of the `determinism' type
% that is defined in prog_data.m.  It ignores most of the distinctions in
% the determinism type and keeps only the distinctions that are important
% for code generation.

% We define this in a different module than the `determinism' type because
% it is only used by some of the different back-ends, not all of them.
% It is used by the MLDS, LLDS, and bytecode back-ends, but not by the
% Aditi-RL back-end.

%-----------------------------------------------------------------------------%

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- type code_model
	--->	model_det		% functional & total
	;	model_semi		% just functional
	;	model_non.		% not functional

:- pred determinism_to_code_model(determinism, code_model).
:- mode determinism_to_code_model(in, out) is det.
:- mode determinism_to_code_model(out, in) is multi.

:- pred proc_info_interface_code_model(proc_info, code_model).
:- mode proc_info_interface_code_model(in, out) is det.

:- pred goal_info_get_code_model(hlds_goal_info, code_model).
:- mode goal_info_get_code_model(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

determinism_to_code_model(det,         model_det).
determinism_to_code_model(semidet,     model_semi).
determinism_to_code_model(nondet,      model_non).
determinism_to_code_model(multidet,    model_non).
determinism_to_code_model(cc_nondet,   model_semi).
determinism_to_code_model(cc_multidet, model_det).
determinism_to_code_model(erroneous,   model_det).
determinism_to_code_model(failure,     model_semi).

proc_info_interface_code_model(ProcInfo, CodeModel) :-
	proc_info_interface_determinism(ProcInfo, Determinism),
	determinism_to_code_model(Determinism, CodeModel).

goal_info_get_code_model(GoalInfo, CodeModel) :-
	goal_info_get_determinism(GoalInfo, Determinism),
	determinism_to_code_model(Determinism, CodeModel).

%-----------------------------------------------------------------------------%
