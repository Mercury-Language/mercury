%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: assertion
%
% Main authors: petdr
%
% This module is an abstract interface to the assertion table.
% Note that this is a first design and will probably change
% substantially in the future.
%
%-----------------------------------------------------------------------------%

:- module (assertion).

:- interface.

:- import_module hlds_data, hlds_goal, hlds_module.

	%
	% assertion__goal
	%
	% Get the hlds_goal which represents the assertion.
	%
:- pred assertion__goal(assert_id::in, module_info::in, hlds_goal::out) is det.

	%
	% assertion__record_preds_used_in
	%
	% Record into the pred_info of each pred used in the assertion
	% the assert_id.
	%
:- pred assertion__record_preds_used_in(hlds_goal::in, assert_id::in,
		module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred.
:- import_module list, require, set, std_util.

%-----------------------------------------------------------------------------%

assertion__goal(AssertId, Module, Goal) :-
	module_info_assertion_table(Module, AssertTable),
	assertion_table_lookup(AssertTable, AssertId, PredId),
	module_info_pred_info(Module, PredId, PredInfo),
	pred_info_clauses_info(PredInfo, ClausesInfo),
	clauses_info_clauses(ClausesInfo, Clauses),
	(
		Clauses = [clause(_ProcIds, Goal0, _Context)]
	->
		Goal = Goal0
	;
		error("assertion__goal: not an assertion")
	).

%-----------------------------------------------------------------------------%

assertion__record_preds_used_in(call(PredId, _, _, _, _, _) - _, AssertId,
		Module0, Module) :-
	update_pred_info(PredId, AssertId, Module0, Module).
assertion__record_preds_used_in(generic_call(_, _, _, _) - _, _AssertId,
		Module, Module).
assertion__record_preds_used_in(conj(Goals) - _, AssertId, Module0, Module) :-
	assertion__record_preds_used_in_goal_list(Goals, AssertId,
			Module0, Module).
assertion__record_preds_used_in(switch(_, _, Cases, _) - _, AssertId,
		Module0, Module) :-
	assertion__record_preds_used_in_cases(Cases, AssertId, Module0, Module).
assertion__record_preds_used_in(unify(_, _, _, _, _) - _,
		_AssertId, Module, Module).
assertion__record_preds_used_in(disj(Goals, _) - _, AssertId,
		Module0, Module) :-
	assertion__record_preds_used_in_goal_list(Goals, AssertId,
			Module0, Module).
assertion__record_preds_used_in(not(Goal) - _, AssertId, Module0, Module) :-
	assertion__record_preds_used_in(Goal, AssertId, Module0, Module).
assertion__record_preds_used_in(some(_, _, Goal) - _, AssertId,
		Module0, Module) :-
	assertion__record_preds_used_in(Goal, AssertId, Module0, Module).
assertion__record_preds_used_in(if_then_else(_, If, Then, Else, _) - _,
		AssertId, Module0, Module) :-
	assertion__record_preds_used_in(If, AssertId, Module0, Module1),
	assertion__record_preds_used_in(Then, AssertId, Module1, Module2),
	assertion__record_preds_used_in(Else, AssertId, Module2, Module).
assertion__record_preds_used_in(pragma_c_code(_, _, _, _, _, _, _) - _,
		_AssertId, Module, Module).
assertion__record_preds_used_in(par_conj(Goals, _) - _, AssertId,
		Module0, Module) :-
	assertion__record_preds_used_in_goal_list(Goals, AssertId,
			Module0, Module).
	
%-----------------------------------------------------------------------------%

:- pred assertion__record_preds_used_in_goal_list(list(hlds_goal)::in,
		assert_id::in, module_info::in, module_info::out) is det.

assertion__record_preds_used_in_goal_list([], _, Module, Module).
assertion__record_preds_used_in_goal_list([Goal | Goals], AssertId,
		Module0, Module) :-
	assertion__record_preds_used_in(Goal, AssertId, Module0, Module1),
	assertion__record_preds_used_in_goal_list(Goals, AssertId,
			Module1, Module).

%-----------------------------------------------------------------------------%

:- pred assertion__record_preds_used_in_cases(list(case)::in, assert_id::in,
		module_info::in, module_info::out) is det.

assertion__record_preds_used_in_cases([], _, Module, Module).
assertion__record_preds_used_in_cases([Case | Cases], AssertId,
		Module0, Module) :-
	Case = case(_, Goal),
	assertion__record_preds_used_in(Goal, AssertId, Module0, Module1),
	assertion__record_preds_used_in_cases(Cases, AssertId, Module1, Module).

%-----------------------------------------------------------------------------%

	%
	% update_pred_info(Id, A, M0, M)
	%
	% Record in the pred_info pointed to by Id that that predicate
	% is used in the assertion pointed to by A.
	%
:- pred update_pred_info(pred_id::in, assert_id::in, module_info::in,
		module_info::out) is det.

update_pred_info(PredId, AssertId, Module0, Module) :-
	module_info_pred_info(Module0, PredId, PredInfo0),
	pred_info_get_assertions(PredInfo0, Assertions0),
	set__insert(Assertions0, AssertId, Assertions),
	pred_info_set_assertions(PredInfo0, Assertions, PredInfo),
	module_info_set_pred_info(Module0, PredId, PredInfo, Module).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
