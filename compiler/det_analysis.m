%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% det_analysis.nl - the determinism analysis pass.

% Main author: conway.

% This pass has three components:
%	o Segregate the procedures into those that have determinism
%		declarations, and those that don't
%	o A step of performing a local analysis pass on each procedure
%		without a determinism declaration is iterated untill
%		a fixpoint is reached
%	o A checking step is performed on all the procedures that have
%		determinism declarations to ensure that they are at
%		least as deterministic as their declaration. This uses
%		a form of the local analysis pass.

%-----------------------------------------------------------------------------%

:- module det_analysis.
:- interface.
:- import_module hlds.

:- pred determinism_pass(moduleinfo, moduleinfo).
:- mode determinism_pass(input, output).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, prog_io, std_util.

%-----------------------------------------------------------------------------%

determinism_pass(ModuleInfo0, ModuleInfo) :-
	determinism_declarations(ModuleInfo0, DeclaredProcs, UndeclaredProcs),
	global_analysis_pass(ModuleInfo0, UndeclaredProcs, ModuleInfo1),
	global_checking_pass(ModuleInfo1, DeclaredProcs, ModuleInfo).

%-----------------------------------------------------------------------------%

:- type predproclist	==	list(pair(pred_id, pred_mode_id)).
:- type miscinfo	--->	miscinfo(module_info, pred_id, pred_mode_id).

:- pred determinism_declarations(module_info, predproclist, predproclist).
:- mode determinism_declarations(input, output, output).

determinism_declarations(ModuleInfo, DeclaredProcs, UndeclaredProcs) :-
	get_all_pred_procs(ModuleInfo, PredProcs),
	segregate_procs(ModuleInfo, PredProcs, [], DeclaredProcs,
			[], UndeclaredProcs).

:- pred get_all_pred_procs(moduleinfo, predproclist).
:- mode get_all_pred_procs(input, output).

get_all_pred_procs(ModuleInfo, PredProcs) :-
	moduleinfo_predids(ModuleInfo, PredIds),
	moduleinfo_preds(ModuleInfo, Preds),
	get_all_pred_procs_2(Preds, PredIds, [], PredProcs).

:- pred get_all_pred_procs_2(pred_table, list(pred_id),
				predproclist, predproclist).
:- mode get_all_pred_procs_2(input, input, input, output).

get_all_pred_procs_2(_Preds, [], PredProcs, PredProcs).
get_all_pred_procs_2(Preds, [PredId|PredIds], PredProcs0, PredProcs) :-
	map__search(Preds, PredId, Pred),
	predinfo_modes(Pred, PredModes),
	fold_pred_modes(PredId, PredModes, PredProcs0, PredProcs1),
	get_all_pred_procs_2(Preds, PredIds, PredProcs1, PredProcs).

:- pred fold_pred_modes(pred_id, list(pred_mode), predproclist, predproclist).
:- mode fold_pred_modes(input, input, input, output).

fold_pred_modes(_PredId, [], PredProcs, PredProcs).
fold_pred_modes(PredId, [PredMode|PredModes], PredProcs0, PredProcs) :-
	fold_pred_modes(PredId, PredModes, [PredId - PredMode|PredProcs0],
		PredProcs).

:- pred segregate_procs(module_info, predproclist, predproclist,
			predproclist, predproclist).
:- mode segregate_procs(input, input, input, output, input, output).

segregate_procs(_ModuleInfo, [], DeclaredProcs, DeclaredProcs,
				UndeclaredProcs, UndeclaredProcs).
segregate_procs(ModuleInfo, [PredId - PredMode|PredProcs],
			DeclaredProcs0, DeclaredProcs,
				UndeclaredProcs0, UndeclaredProcs) :-
	moduleinfo_preds(ModuleInfo, Preds),
	map__search(Preds, PredId, Pred),
	predinfo_procedures(Pred, PredMode, Procs),
	map__search(Procs, PredMode, Proc),
	procinfo_category(Proc, Category),
	(if
		Category = unspecified
	then
		UndeclaredProcs1 = [PredId - PredMode|UndeclaredProcs0],
		DeclaredProcs1 = DeclaredProcs0
	else
		DeclaredProcs1 = [PredId - PredMode|DeclaredProcs0],
		UndeclaredProcs1 = UndeclaredProcs0
	),
	segregate_procs(ModuleInfo, PredProcs, DeclaredProcs1, DeclaredProcs,
				UndeclaredProcs1, UndeclaredProcs).

%-----------------------------------------------------------------------------%

:- pred global_analysis_pass(module_info, predproclist, module_info).
:- mode global_analysis_pass(input, input, output).

global_analysis_pass(ModuleInfo0, UndeclaredProcs, ModuleInfo) :-
	global_analysis_pass_2(ModuleInfo0, UndeclaredProcs, UndeclaredProcs,
		unchanged, ModuleInfo).

:- type maybe_changed ---> changed ; unchanged.

:- pred global_analysis_pass(module_info, predproclist, predproclist,
				maybe_changed, module_info).
:- mode global_analysis_pass(input, input, input, input, output).

:- global_analysis_pass_2(_, _, L, _, _) when L.	% NU-Prolog indexing.

global_analysis_pass_2(ModuleInfo, _UndeclaredProcs, [], unchanged, ModuleInfo).
global_analysis_pass_2(ModuleInfo0, UndeclaredProcs, [], changed, ModuleInfo) :-
	global_analysis_pass_2(ModuleInfo0, UndeclaredProcs, UndeclaredProcs,
				unchanged, ModuleInfo).
global_analysis_pass_2(ModuleInfo0, UndeclaredProcs,
		[PredId - PredMode|PredProcs], State0, ModuleInfo) :-
	moduleinfo_preds(ModuleInfo, Preds0),
	map__search(Preds0, PredId, Pred0),
	predinfo_procedures(Pred0, Procs0),
	map__search(Procs0, PredMode, Pred0),
	procinfo_category(Proc0, Category0),
	procinfo_goal(Proc0, Goal0),
	MiscInfo = miscinfo(ModuleInfo0, PredId, PredMode),
	make_inst_map(Proc, InstMap0),
	Goal0 = HldsGoal0 - GoalInfo0,
	goalinfo_category(GoalInfo0, Detism0),
		% XXX MiscInfo is uninitialized
	local_det_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap,
			deterministic(infered), Detism1),
	infered_category(Detism1, Detism),
	procinfo_set_goal(Proc0, Goal, Proc1),
	procinfo_set_category(Proc1, Detism, Proc),
	map__set(Procs0, PredMode, Proc, Procs),
	predinfo_set_procs(Pred0, Procs, Pred),
	map__set(Preds0, PredId, Pred, Preds),
	moduleinfo_set_preds(ModuleInfo0, Preds, ModuleInfo1),
	(if
		Detism = Detism0
	then
		State = State0
	else
		State = changed
	),
	global_analysis_pass_2(ModuleInfo1, UndeclaredProcs, PredProcs,
			State, ModuleInfo).

%-----------------------------------------------------------------------------%

/****
global_checking_pass(ModuleInfo, Result, Result, [], ModuleInfo).
global_checking_pass(ModuleInfo0, Result0, Result,
			[PredId - PredMode|DeclaredProcs], ModuleInfo) :-
	XXX
****/
	
%-----------------------------------------------------------------------------%

	% the category of a conjunction is the worst case of the elements
	% of that conjuction.
local_det_goal(conj(Goals0) - GoalInfo0, MiscInfo, conj(Goals) - GoalInfo,
		InstMap0, InstMap, D0, D) :-
	local_det_conj(Goals0, MiscInfo, InstMap0,
				deterministic(infered), Goals, D1),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D1, GoalInfo),
	max_category(D0, D1, D).

	% a disjunction is always nondeterministic, however, we do need to
	% recursively decend the subgoals to annotate them.
local_det_goal(disj(Goals0) - GoalInfo0, MiscInfo, disj(Goals) - GoalInfo,
		InstMap0, InstMap, D0, D) :-
	local_det_disj(Goals0, MiscInfo, InstMap0, Goals),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, nondeterministic(infered), GoalInfo),
	max_category(D0, nondeterministic(infered), D).

	% the category of a switch is the worst of the category of each of
	% the cases, and (if only a subset of the constructors are handled)
	% semideterministic
local_det_goal(switch(Var, Cases0, Follow) - GoalInfo0, MiscInfo,
		switch(Var, Cases, Follow) - GoalInfo, InstMap0,
		InstMap, D0, D) :-
	local_det_switch(Cases0, MiscInfo, Cases,
			[], Cons, InstMap0, deterministic(infered), D1),
	test_to_see_that_all_constructors_are_tested(Cons, MiscInfo, D2),
	max_category(D1, D2, D3),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D2, GoalInfo),
	max_category(D0, D3, D).
	
	% look up the category entry associated with the call.
	% This is the point at which annotations start changing
	% when we iterate to fixpoint for global determinism analysis.
local_det_goal(call(PredId, ModeId, Args, BuiltIn) - GoalInfo, MiscInfo,
		call(PredId, ModeId, Args, BuiltIn) - GoalInfo, InstMap0,
		InstMap, D, D) :-
	goalinfo_instmap(GoalInfo, InstMap),
	detism_lookup(MiscInfo, PredId, ModeId, Category),
	goalinfo_set_category(GoalInfo0, Category, GoalInfo),
	max_category(D0, Category, D).

	% unifications are either deterministic or semideterministic.
	% (see local_det_unify).
local_det_goal(unify(LT, RT, M, U) - GoalInfo0, MiscInfo,
		unify(LT, RT, M, U) - GoalInfo, InstMap0, InstMap, D0, D) :-
	local_det_unify(U, MiscInfo, D1),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D1, GoalInfo),
	max_category(D0, D1, D).

	% if_then_elses have a category determined by the worst of the
	% condition and the two alternitive cases (then, else). The
	% condition is evaluated the same as a "some" construct, and if
	% the goal is nondeterministic and it further instansiates non-
	% local variables, the a commit must be performed. XXX at the 
	% moment, this is not dealt with - if_then_else/4 should be
	% extended to contain an annotation saying whether or not to
	% generate code for a commit.
local_det_goal(if_then_else(Vars, Cond0, Then0, Else0) - GoalInfo0, MiscInfo,
		if_then_else(Vars, Cond, Then, Else) - GoalInfo,
		InstMap0, InstMap, D0, D) :-
	local_det_goal(Cond0, MiscInfo, Cond, InstMap0, InstMap1,
			deterministic(infered), D1),
	local_det_goal(Then0, MiscInfo, Then, InstMap1, _InstMap2,
			deterministic(infered), D2),
	local_det_goal(Else0, MiscInfo, Else, InstMap1, _InstMap3,
			deterministic(infered), D3),
	goalinfo_instmap(GoalInfo0, InstMap),
	make_var_modes(InstMap0, InstMap1, ModeMap),
	(if
		no_output_vars(Vars, ModeMap)
	then
		max_category(D2,D3,D4)
	else
		D4 = nondeterministic(infered)
	),
	goalinfo_set_category(GoalInfo0, D4, GoalInfo),
	max_category(D0, D4, D).

	% nots are always semideterministic. it is an error for
	% a not to further instansiate any non-local variable. Such
	% errors should be reported by the mode analysis
	% XXX should they be reported here?
local_det_goal(not(Vars, Goal0) - GoalInfo0, MiscInfo,
		not(Vars, Goal) - GoalInfo, InstMap0, InstMap, D0, D) :-
	local_det_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap1,
			deterministic(infered), _D1),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, semideterministic(infered), GoalInfo),
	max_category(D0, semideterministic(infered), D).
local_det_goal(some(Vars, Goal0) - GoalInfo0, MiscInfo,
		some(Vars, Goal) - GoalInfo, InstMap0, InstMap, D0, D) :-
	local_det_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap1,
			deterministic(infered), D1),
	make_var_modes(InstMap0, InstMap1, ModeMap),
	(if
		no_output_vars(Vars, ModeMap)
	then
		(if
			some [Inference] (
				D1 = deterministic(Inference)
			)
		then
			D2 = D1
		else
			D2 = semideterministic(infered)
		)
	else
		D2 = D1
	),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D2, GoalInfo),
	max_category(D0, D2, D).
local_det_goal(all(Vars, Goal0) - GoalInfo0, MiscInfo,
		all(Vars, Goal) - GoalInfo, InstMap0, InstMap, D0, D) :-
	local_det_goal(Goal0, MiscInfo, Goal, deterministic(infered), _D1),
	goalinfo_set_category(GoalInfo0, semideterministic(infered), GoalInfo),
	max_category(D0, semideterministic(infered), D).

local_det_conj([], MiscInfo, InstMap0, D, [], D).
local_det_conj([Goal0|Goals0], MiscInfo, InstMap0, D0, [Goal|Goals], D) :-
	local_det_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap1, D0, D1),
	local_det_conj(Goals0, MiscInfo, InstMap1, D1, Goals, D).

local_det_disj([], MiscInfo, InstMap0, []).
local_det_disj([Goal0|Goals0], MiscInfo, InstMap0, [Goal|Goals]) :-
	local_det_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap, deterministic(infered), _D),
	local_det_disj(Goals0, MiscInfo, InstMap, Goals).

local_det_unify(construct(_, _, _, _), MiscInfo,
		deterministic(infered)).
local_det_unify(deconstruct(_, _, _, _), MiscInfo,
		semideterministic(infered)).		% XXX
local_det_unify(simple_test(_, _), MiscInfo,
		semideterministic(infered)).
local_det_unify(complicated_unify(_, _, _), MiscInfo,
		semideterministic(infered)).


local_det_switch([], MiscInfo, [], Cons, Cons, InstMap0, D, D).
local_det_switch([Case0|Cases0], MiscInfo, [Case|Cases], Cons0, Cons, InstMap0, D0, D) :-
	Case0 = case(ConsId, Vars, Goal0),
	local_det_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap1, D0, D1),
	Case = case(ConsId, Vars, Goal),
	local_det_switch(Cases0, MiscInfo, Cases, [ConsId|Cons0], InstMap0, D1, D).

max_category(deterministic(I0), deterministic(I1), deterministic(I)) :-
	max_category_2(I0, I1, I).
max_category(deterministic(_), semideterministic(I), semideterministic(I)).
max_category(deterministic(_), nondeterministic(I), nondeterministic(I)).

max_category(semideterministic(I), deterministic(_), semideterministic(I)).
max_category(semideterministic(I0), semideterministic(I1),
		semideterministic(I)) :-
	max_category_2(I0, I1, I).
max_category(semideterministic(_), nondeterministic(I), nondeterministic(I)).

max_category(nondeterministic(I), deterministic(_), nondeterministic(I)).
max_category(nondeterministic(I), semideterministic(_), nondeterministic(I)).
max_category(nondeterministic(I0), nondeterministic(I1), nondeterministic(I)) :-
	max_category_2(I0, I1, I).

	% We consider inference to be stronger than declarations,
	% since we may be able to prove by inference that a declaration
	% is wrong. 
max_category_2(declared, I, I).
max_category_2(infered, _I, infered).

infered_category(deterministic(_), deterministic(infered)).
infered_category(semideterministic(_), semideterministic(infered)).
infered_category(nondeterministic(_), nondeterministic(infered)).

make_var_modes(InstMap0, InstMap1, ModeMap) :-
	map__keys(InstMap1, Keys),
	make_var_modes_2(InstMap0, InstMap1, Keys, ModeMap).

make_var_modes_2(_InstMap0, _InstMap1, [], ModeMap) :-
	map__init(ModeMap).
make_var_modes_2(InstMap0, InstMap1, [Key|Keys], ModeMap) :-
	make_var_modes_2(InstMap0, InstMap1, Keys, ModeMap0),
	map__search(InstMap1, Key, Inst1),
	(if
		some [Inst0] (
			map__search(InstMap0, Key, Inst0)
		)
	then
		map__set(ModeMap0, Key,Inst0 -> Inst1, ModeMap)
	else
		map__set(ModeMap0, Key,free -> Inst1, ModeMap)
	).

detism_lookup(MiscInfo, PredId, ModeId, Category) :-
	MiscInfo = miscinfo(ModuleInfo, _PredId, _PredModeId),
	moduleinfo_predinfo(ModuleInfo, PredId, PredInfo),
	predinfo_procinfo(PredInfo, PredMode, ProcInfo),
	procinfo_category(ProcInfo, Category).


%-----------------------------------------------------------------------------%

	% the category of a conjunction is the worst case of the elements
	% of that conjuction.
	% We don't need to report any errors at this point because a conj
	% of itself does not introduce any nondeterminism, so any conflict
	% will have been incurred in a member of the conjuction and will 
	% have been reported there.
local_check_goal(conj(Goals0) - GoalInfo0, MiscInfo, conj(Goals) - GoalInfo,
		InstMap0, InstMap, Category, D0, D) :-
	local_check_conj(Goals0, MiscInfo, InstMap0, Category,
				deterministic(infered), Goals, D1),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D1, GoalInfo),
	max_category(D0, D1, D).

	% a disjunction is always nondeterministic, however, we do need to
	% recursively decend the subgoals to annotate them.
	% Since a disjunction is always nondeterministic, if the declared
	% category is anything other, then there is an error, irrespective
	% of the determinacy of the members of the disjunction.
	% since a disjucntion is maximally nondeterministic, we don't need
	% to check the subgoals, but we still need to annotate them; so
	% we can use the local_det_disj predicate.
local_check_goal(disj(Goals0) - GoalInfo0, MiscInfo, disj(Goals) - GoalInfo,
		InstMap0, InstMap, Category, D0, D) :-
	local_det_disj(Goals0, MiscInfo, InstMap0, Goals),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, nondeterministic(infered), GoalInfo),
	max_category(D0, nondeterministic(infered), D),
	(if
		Category = nondeterministic(_)
	then
		true
	else
		warn("A procedure containing a disjunction must be declared"),
		warn("as nondet.")
	).

	% the category of a switch is the worst of the category of each of
	% the cases, and (if only a subset of the constructors are handled)
	% semideterministic
	% if the switch is semideterministic (ie there are constructors not
	% handled by the switch) then the bodies of the switch must be at
	% most semideterministic. If the switch covers all the constructors
	% then the cases must all be deterministic for the switch to be
	% deterministic, and since my brain is currently semifunctional,
	% working out how to do all this properly will have to wait.
local_check_goal(switch(Var, Cases0, Follow) - GoalInfo0, MiscInfo,
		switch(Var, Cases, Follow) - GoalInfo, InstMap0,
		InstMap, Category, D0, D) :-
	local_check_switch(Cases0, MiscInfo, Cases,
			[], Cons, InstMap0, Category,
					deterministic(infered), D1),
	test_to_see_that_all_constructors_are_tested(Cons, MiscInfo, D2),
	max_category(D1, D2, D3),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D2, GoalInfo),
	max_category(D0, D3, D).
	
	% look up the category entry associated with the call.
	% This is the point at which annotations start changing
	% when we iterate to fixpoint for global determinism analysis.
	% the called function must be at most the same category as this
	% procedure - if it is more nondeterministic than this procedure
	% then that is an error.
local_check_goal(call(PredId, ModeId, Args, BuiltIn) - GoalInfo, MiscInfo,
		call(PredId, ModeId, Args, BuiltIn) - GoalInfo, InstMap0,
		InstMap, Category, D, D) :-
	goalinfo_instmap(GoalInfo, InstMap),
	detism_lookup(MiscInfo, PredId, ModeId, Category0),
	goalinfo_set_category(GoalInfo0, Category0, GoalInfo),
	max_category(D0, Category0, D),
	(if
		max_category(Category, D, Category)
	then
		true
	else
		warn("predicate call is more nondeterministic than the"),
		warn("declared determinism for this mode of this predicate")
	).

	% unifications are either deterministic or semideterministic.
	% (see local_check_unify).
	% like calls, if the unification is more nondeterministic than
	% the declared determinism of this procedure, then raise an error.
local_check_goal(unify(LT, RT, M, U) - GoalInfo0, MiscInfo,
		unify(LT, RT, M, U) - GoalInfo, InstMap0,
					InstMap, Category, D0, D) :-
	local_check_unify(U, MiscInfo, D1),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D1, GoalInfo),
	max_category(D0, D1, D),
	(if
		max_category(Category, D, Category)
	then
		true
	else
		warn("unification is more nondeterministic than the declared"),
		warn("determinism for this mode of this predicate.")
	).

	% if_then_elses have a category determined by the worst of the
	% condition and the two alternitive cases (then, else). The
	% condition is evaluated the same as a "some" construct, and if
	% the goal is nondeterministic and it further instansiates non-
	% local variables, the a commit must be performed. XXX at the 
	% moment, this is not dealt with - if_then_else/4 should be
	% extended to contain an annotation saying whether or not to
	% generate code for a commit.
local_check_goal(if_then_else(Vars, Cond0, Then0, Else0) - GoalInfo0, MiscInfo,
		if_then_else(Vars, Cond, Then, Else) - GoalInfo,
		InstMap0, InstMap, Category, D0, D) :-
	local_check_goal(Cond0, MiscInfo, Cond, InstMap0, InstMap1, Category,
			deterministic(infered), D1),
	local_check_goal(Then0, MiscInfo, Then, InstMap1, _InstMap2, Category,
			deterministic(infered), D2),
	local_check_goal(Else0, MiscInfo, Else, InstMap1, _InstMap3, Category,
			deterministic(infered), D3),
	goalinfo_instmap(GoalInfo0, InstMap),
	make_var_modes(InstMap0, InstMap1, ModeMap),
	(if
		no_output_vars(Vars, ModeMap)
	then
		max_category(D2,D3,D4)
	else
		D4 = nondeterministic(infered)
	),
	goalinfo_set_category(GoalInfo0, D4, GoalInfo),
	max_category(D0, D4, D).

	% nots are always semideterministic. it is an error for
	% a not to further instansiate any non-local variable. Such
	% errors should be reported by the mode analysis
	% XXX should they be reported here?
local_check_goal(not(Vars, Goal0) - GoalInfo0, MiscInfo,
		not(Vars, Goal) - GoalInfo, InstMap0,
					InstMap, Category, D0, D) :-
	local_check_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap1, Category,
			deterministic(infered), _D1),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, semideterministic(infered), GoalInfo),
	max_category(D0, semideterministic(infered), D).
local_check_goal(some(Vars, Goal0) - GoalInfo0, MiscInfo,
		some(Vars, Goal) - GoalInfo, InstMap0,
					InstMap, Category, D0, D) :-
	local_check_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap1, Category,
			deterministic(infered), D1),
	make_var_modes(InstMap0, InstMap1, ModeMap),
	(if
		no_output_vars(Vars, ModeMap)
	then
		(if
			some [Inference] (
				D1 = deterministic(Inference)
			)
		then
			D2 = D1
		else
			D2 = semideterministic(infered)
		)
	else
		D2 = D1
	),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D2, GoalInfo),
	max_category(D0, D2, D).
local_check_goal(all(Vars, Goal0) - GoalInfo0, MiscInfo,
		all(Vars, Goal) - GoalInfo, InstMap0,
					InstMap, Category, D0, D) :-
	local_check_goal(Goal0, MiscInfo, Goal,
				Category, deterministic(infered), _D1),
	goalinfo_set_category(GoalInfo0, semideterministic(infered), GoalInfo),
	max_category(D0, semideterministic(infered), D).

local_check_conj([], MiscInfo, InstMap0, Category, D, [], D).
local_check_conj([Goal0|Goals0], MiscInfo, InstMap0,
					Category, D0, [Goal|Goals], D) :-
	local_check_goal(Goal0, MiscInfo, Goal, InstMap0,
					InstMap1, Category, D0, D1),
	local_check_conj(Goals0, MiscInfo, InstMap1, Category, D1, Goals, D).

local_check_disj([], MiscInfo, InstMap0, Category, []).
local_check_disj([Goal0|Goals0], MiscInfo, InstMap0, Category, [Goal|Goals]) :-
	local_check_goal(Goal0, MiscInfo, Goal, InstMap0,
			InstMap, Category, deterministic(infered), _D),
	local_check_disj(Goals0, MiscInfo, InstMap, Category, Goals).

%-----------------------------------------------------------------------------%
