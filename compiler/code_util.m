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

:- pred atom_to_operator(string, operator).
:- mode atom_to_operator(in, out) is semidet.
:- mode atom_to_operator(out, in) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module list, hlds, map.

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
%-----------------------------------------------------------------------------%
