%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_util.

:- interface.

:- import_module hlds, llds, code_info.

:- pred code_util__make_entry_label(pred_id, proc_id, code_addr).
:- mode code_util__make_entry_label(in, in, out) is det.

:- pred code_util__make_local_entry_label(pred_id, proc_id, label).
:- mode code_util__make_local_entry_label(in, in, out) is det.

:- pred code_util__make_local_label(pred_id, proc_id, int, label).
:- mode code_util__make_local_label(in, in, in, out) is det.

:- pred code_util__uni_mode_to_unilabel(uni_mode, unilabel).
:- mode code_util__uni_mode_to_unilabel(in, out) is det.

:- pred code_util__arg_loc_to_register(arg_loc, reg).
:- mode code_util__arg_loc_to_register(in, out) is det.

:- pred atom_to_operator(string, operator).
:- mode atom_to_operator(in, out) is semidet.
:- mode atom_to_operator(out, in) is det.

%---------------------------------------------------------------------------%
:- implementation.

code_util__make_local_entry_label(PredId, ProcId, Label) :-
	PredId = pred(ModuleName, PredName, Arity),
	Label = entrylabel(ModuleName, PredName, Arity, ProcId).

code_util__make_local_label(PredId, ProcId, LabelNum, Label) :-
	PredId = pred(ModuleName, PredName, Arity),
	Label = label(ModuleName, PredName, Arity, ProcId, LabelNum).

code_util__make_entry_label(PredId, ProcId, PredAddress) :-
	code_util__make_local_entry_label(PredId, ProcId, Label),
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
