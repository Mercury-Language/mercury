%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_util.

:- interface.

:- import_module hlds, llds, code_info.

:- pred code_util__make_entry_label(module_info, pred_id, proc_id, code_addr).
:- mode code_util__make_entry_label(in, in, in, out) is det.

:- pred code_util__make_local_entry_label(module_info, pred_id, proc_id, label).
:- mode code_util__make_local_entry_label(in, in, in, out) is det.

:- pred code_util__make_local_label(module_info, pred_id, proc_id, int, label).
:- mode code_util__make_local_label(in, in, in, in, out) is det.

:- pred code_util__uni_mode_to_unilabel(uni_mode, unilabel).
:- mode code_util__uni_mode_to_unilabel(in, out) is det.

:- pred code_util__arg_loc_to_register(arg_loc, reg).
:- mode code_util__arg_loc_to_register(in, out) is det.

:- pred atom_to_operator(string, operator).
:- mode atom_to_operator(in, out) is semidet.
:- mode atom_to_operator(out, in) is det.

:- pred is_to_op_and_vars(term, operator, var, var).
:- mode is_to_op_and_vars(in, out, out, out) is semidet.

%---------------------------------------------------------------------------%
:- implementation.

code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, Label) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = entrylabel(ModuleName, PredName, Arity, ProcId).

code_util__make_local_label(ModuleInfo, PredId, ProcId, LabelNum, Label) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = label(ModuleName, PredName, Arity, ProcId, LabelNum).

code_util__make_entry_label(ModuleInfo, PredId, ProcId, PredAddress) :-
	code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, Label),
	PredAddress = local(Label).

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
atom_to_operator(">", (>)).
atom_to_operator("<", (<)).
atom_to_operator(">=", (>=)).
atom_to_operator("=<", (<=)).

%-----------------------------------------------------------------------------%

is_to_op_and_vars(Is, Op, X, Y) :-
	Is = term_functor(term_atom(OpStr), [term_variable(X),
							term_variable(Y)], _),
	atom_to_operator(OpStr, Op).

%-----------------------------------------------------------------------------%
