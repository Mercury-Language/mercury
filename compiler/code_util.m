%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_util.

:- interface.

:- import_module hlds, llds.

:- pred code_util__make_entry_label(module_info, pred_id, proc_id, code_addr).
:- mode code_util__make_entry_label(in, in, in, out) is det.

:- pred code_util__make_local_entry_label(module_info, pred_id, proc_id, label).
:- mode code_util__make_local_entry_label(in, in, in, out) is det.

:- pred code_util__make_nonlocal_entry_label(module_info,
						pred_id, proc_id, code_addr).
:- mode code_util__make_nonlocal_entry_label(in, in, in, out) is det.

:- pred code_util__make_local_label(module_info, pred_id, proc_id, int, label).
:- mode code_util__make_local_label(in, in, in, in, out) is det.

:- pred code_util__uni_mode_to_unilabel(uni_mode, unilabel).
:- mode code_util__uni_mode_to_unilabel(in, out) is det.

:- pred code_util__arg_loc_to_register(arg_loc, reg).
:- mode code_util__arg_loc_to_register(in, out) is det.

:- pred code_util__goal_may_allocate_heap(hlds__goal).
:- mode code_util__goal_may_allocate_heap(in) is semidet.

:- pred code_util__goal_list_may_allocate_heap(list(hlds__goal)).
:- mode code_util__goal_list_may_allocate_heap(in) is semidet.

:- pred atom_to_operator(string, operator).
:- mode atom_to_operator(in, out) is semidet.
:- mode atom_to_operator(out, in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module list, hlds, map, std_util.

%---------------------------------------------------------------------------%

code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, Label) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = entrylabel(ModuleName, PredName, Arity, ProcId).

code_util__make_nonlocal_entry_label(ModuleInfo, PredId, ProcId, Label) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = nonlocal(ModuleName, PredName, Arity, ProcId).

code_util__make_local_label(ModuleInfo, PredId, ProcId, LabelNum, Label) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = label(ModuleName, PredName, Arity, ProcId, LabelNum).

code_util__make_entry_label(ModuleInfo, PredId, ProcId, PredAddress) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	(
		pred_info_is_imported(PredInfo)
	->
		code_util__make_nonlocal_entry_label(ModuleInfo,
						PredId, ProcId, PredAddress)
	;
		code_util__make_local_entry_label(ModuleInfo,
							PredId, ProcId, Label),
		PredAddress = local(Label)
	).

%-----------------------------------------------------------------------------%

code_util__uni_mode_to_unilabel(_UniMode,
				unilabel("xxx","outofline","unification")).

%-----------------------------------------------------------------------------%

code_util__arg_loc_to_register(ArgLoc, r(ArgLoc)).

%-----------------------------------------------------------------------------%

atom_to_operator("builtin_plus", (+)).
atom_to_operator("builtin_minus", (-)).
atom_to_operator("builtin_times", (*)).
atom_to_operator("builtin_div", (/)).
atom_to_operator("builtin_mod", (mod)).
atom_to_operator(">", (>)).
atom_to_operator("<", (<)).
atom_to_operator(">=", (>=)).
atom_to_operator("=<", (<=)).

%-----------------------------------------------------------------------------%

	% This code may _look_ nondeterministic, but it's really semidet,
	% and Mercury is smart enough to know this.

code_util__goal_may_allocate_heap(Goal - _GoalInfo) :-
	code_util__goal_may_allocate_heap_2(Goal).

:- pred code_util__goal_may_allocate_heap_2(hlds__goal_expr).
:- mode code_util__goal_may_allocate_heap_2(in) is semidet.

code_util__goal_may_allocate_heap_2(call(_, _, _, not_builtin, _, _)).
code_util__goal_may_allocate_heap_2(unify(_, _, _, construct(_,_,Args,_), _)) :-
	Args = [_|_].
code_util__goal_may_allocate_heap_2(some(_Vars, Goal)) :-
	code_util__goal_may_allocate_heap(Goal).
code_util__goal_may_allocate_heap_2(not(_Vars, Goal)) :-
	code_util__goal_may_allocate_heap(Goal).
code_util__goal_may_allocate_heap_2(conj(Goals)) :-
	code_util__goal_list_may_allocate_heap(Goals).
code_util__goal_may_allocate_heap_2(disj(Goals)) :-
	code_util__goal_list_may_allocate_heap(Goals).
code_util__goal_may_allocate_heap_2(switch(_Var, _Det, Cases)) :-
	code_util__cases_may_allocate_heap(Cases).
code_util__goal_may_allocate_heap_2(if_then_else(_Vars, A, B, C)) :-
	(
		code_util__goal_may_allocate_heap(A)
	;	
		code_util__goal_may_allocate_heap(B)
	;
		code_util__goal_may_allocate_heap(C)
	).

:- pred code_util__cases_may_allocate_heap(list(case)).
:- mode code_util__cases_may_allocate_heap(in) is semidet.

code_util__cases_may_allocate_heap([case(_, Goal) | _]) :-
	code_util__goal_may_allocate_heap(Goal).
code_util__cases_may_allocate_heap([_ | Cases]) :-
	code_util__cases_may_allocate_heap(Cases).

code_util__goal_list_may_allocate_heap([Goal | _]) :-
	code_util__goal_may_allocate_heap(Goal).
code_util__goal_list_may_allocate_heap([_ | Goals]) :-
	code_util__goal_list_may_allocate_heap(Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
