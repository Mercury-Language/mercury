%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rtti_access.m.
% Main authors: zs, maclarty
%
% This module contains an interface to the label_layout and proc_layout
% types which are used in the C backend of the debugger.

:- module mdbcomp.rtti_access.

:- interface.

:- import_module list.
:- import_module std_util.

:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.trace_counts.

:- type label_layout.

:- func get_proc_layout_from_label_layout(label_layout) = proc_layout.

:- func get_goal_path_from_label_layout(label_layout) = goal_path_string.

:- func get_goal_path_from_maybe_label(maybe(label_layout)) = goal_path_string.

:- func get_port_from_label_layout(label_layout) = trace_port.

:- func get_path_port_from_label_layout(label_layout) = path_port.

:- pred get_context_from_label_layout(label_layout::in, string::out, int::out)
	is semidet.

:- type proc_layout. 

:- func get_proc_label_from_layout(proc_layout) = proc_label.

:- func get_proc_name(proc_label) = string.

:- func get_all_modes_for_layout(proc_layout) = list(proc_layout).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

:- pragma foreign_type("C", label_layout, "const MR_Label_Layout *",
	[can_pass_as_mercury_type, stable]).

	% stub only
:- pragma foreign_type("Java", label_layout, "java.lang.Object", []). 

:- pragma foreign_proc("C",
	get_proc_layout_from_label_layout(Label::in) = (ProcLayout::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	ProcLayout = Label->MR_sll_entry;
").

:- pragma foreign_proc("C",
	get_goal_path_from_label_layout(Label::in) = (GoalPath::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	GoalPath = (MR_String)MR_label_goal_path(Label);
").

get_goal_path_from_maybe_label(yes(Label)) 
	= get_goal_path_from_label_layout(Label).
get_goal_path_from_maybe_label(no) = "".

:- pragma foreign_proc("C",
	get_context_from_label_layout(Label::in, FileName::out, LineNo::out), 
	[will_not_call_mercury, thread_safe, promise_pure],
"
	const char	*filename;
	int		line_no;
	
	SUCCESS_INDICATOR = MR_find_context(Label, &filename, &line_no);
	LineNo = (MR_Integer) line_no;
	MR_TRACE_USE_HP(
		MR_make_aligned_string(FileName, (MR_String) filename);
	);
").

:- pragma foreign_proc("C",
	get_port_from_label_layout(Label::in) = (Port::out), 
	[will_not_call_mercury, thread_safe, promise_pure],
"
	Port = Label->MR_sll_port;
").

get_path_port_from_label_layout(Label) = PathPort :-
	Port = get_port_from_label_layout(Label),
	GoalPathStr = get_goal_path_from_label_layout(Label),
	( if path_from_string(GoalPathStr, ValidGoalPath) then
		GoalPath = ValidGoalPath
	else
		error("get_path_port_from_label_layout: invalid goal path")
	),
	PathPort = make_path_port(GoalPath, Port).

:- pragma foreign_type("C", proc_layout, "const MR_Proc_Layout *",
	[can_pass_as_mercury_type, stable]).
:- pragma foreign_type("Java", proc_layout, "java.lang.Object", []). %stub only

get_proc_label_from_layout(Layout) = ProcLabel :-
	( proc_layout_is_uci(Layout) ->
		proc_layout_get_uci_fields(Layout, TypeName, TypeModule,
			DefModule, PredName, TypeArity, ModeNum),
		( PredName = "__Unify__" ->
			SpecialId = unify
		; PredName = "__Index__" ->
			SpecialId = index
		; PredName = "__Compare__" ->
			SpecialId = compare
		;
			error("get_proc_label_from_layout: " ++ 
				"bad special_pred_id")
		),
		string_to_sym_name(DefModule, ".", SymDefModule),
		string_to_sym_name(TypeModule, ".", SymTypeModule),
		ProcLabel = special_proc(SymDefModule, SpecialId, 
			SymTypeModule, TypeName, TypeArity, ModeNum)
	;
		proc_layout_get_non_uci_fields(Layout, PredOrFunc,
			DeclModule, DefModule, PredName, Arity, ModeNum),
		string_to_sym_name(DefModule, ".", SymDefModule),
		string_to_sym_name(DeclModule, ".", SymDeclModule),
		ProcLabel = proc(SymDefModule, PredOrFunc, SymDeclModule, 
			PredName, Arity, ModeNum)
	).

get_proc_name(proc(_, _, _, ProcName, _, _)) = ProcName.
get_proc_name(special_proc(_, _, _, ProcName , _, _)) = ProcName. 

:- pred proc_layout_is_uci(proc_layout::in) is semidet.

:- pragma foreign_proc("C",
	proc_layout_is_uci(Layout::in),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	if (MR_PROC_ID_IS_UCI(Layout->MR_sle_proc_id)) {
		SUCCESS_INDICATOR = MR_TRUE;
	} else {
		SUCCESS_INDICATOR = MR_FALSE;
	}
").

:- pred proc_layout_get_uci_fields(proc_layout::in, string::out,
	string::out, string::out, string::out, int::out, int::out) is det.

:- pragma foreign_proc("C",
	proc_layout_get_uci_fields(Layout::in, TypeName::out, TypeModule::out,
		DefModule::out, PredName::out, TypeArity::out, ModeNum::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	const MR_UCI_Proc_Id	*proc_id;

	proc_id = &Layout->MR_sle_uci;

	/* The casts are there to cast away const without warnings */
	TypeName   = (MR_String) (MR_Integer) proc_id->MR_uci_type_name;
	TypeModule = (MR_String) (MR_Integer) proc_id->MR_uci_type_module;
	DefModule  = (MR_String) (MR_Integer) proc_id->MR_uci_def_module;
	PredName   = (MR_String) (MR_Integer) proc_id->MR_uci_pred_name;
	TypeArity  = proc_id->MR_uci_type_arity;
	ModeNum    = proc_id->MR_uci_mode;
").

:- pred proc_layout_get_non_uci_fields(proc_layout::in, pred_or_func::out,
	string::out, string::out, string::out, int::out, int::out) is det.

:- pragma foreign_proc("C",
	proc_layout_get_non_uci_fields(Layout::in, PredOrFunc::out,
		DeclModule::out, DefModule::out, PredName::out,
		Arity::out, ModeNum::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	const MR_User_Proc_Id	*proc_id;

	proc_id = &Layout->MR_sle_user;

	/* The casts are there to cast away const without warnings */
	PredOrFunc = proc_id->MR_user_pred_or_func;
	DeclModule = (MR_String) (MR_Integer) proc_id->MR_user_decl_module;
	DefModule  = (MR_String) (MR_Integer) proc_id->MR_user_def_module;
	PredName   = (MR_String) (MR_Integer) proc_id->MR_user_name;
	Arity      = proc_id->MR_user_arity;
	ModeNum    = proc_id->MR_user_mode;
").

:- pragma foreign_proc("C",
	get_all_modes_for_layout(Layout::in) = (Layouts::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	const MR_Module_Layout	*module;
	const MR_Proc_Layout	*proc;
	int			i;
	MR_Word			list;
	MR_bool			match;
	const MR_Proc_Layout	*selected_proc;

	selected_proc = Layout;

	if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(selected_proc)) {
		MR_fatal_error(
			""get_all_modes_for_layout: selected_proc"");
	}

	module = selected_proc->MR_sle_module_layout;
	list = MR_list_empty();
	for (i = 0; i < module->MR_ml_proc_count; i++) {
		proc = module->MR_ml_procs[i];
		if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(selected_proc)) {
			MR_fatal_error(
				""get_all_modes_for_layout: proc"");
		}

		if (MR_PROC_LAYOUT_IS_UCI(selected_proc)
			&& MR_PROC_LAYOUT_IS_UCI(proc))
		{
			const MR_UCI_Proc_Id	*proc_id;
			const MR_UCI_Proc_Id	*selected_proc_id;

			proc_id = &proc->MR_sle_uci;
			selected_proc_id = &selected_proc->MR_sle_uci;

			if (MR_streq(proc_id->MR_uci_type_name,
				selected_proc_id->MR_uci_type_name)
			&& MR_streq(proc_id->MR_uci_type_module,
				selected_proc_id->MR_uci_type_module)
			&& MR_streq(proc_id->MR_uci_pred_name,
				selected_proc_id->MR_uci_pred_name)
			&& (proc_id->MR_uci_type_arity ==
				selected_proc_id->MR_uci_type_arity))
			{
				match = MR_TRUE;
			} else {
				match = MR_FALSE;
			}
		} else if (!MR_PROC_LAYOUT_IS_UCI(selected_proc)
			&& !MR_PROC_LAYOUT_IS_UCI(proc))
		{
			const MR_User_Proc_Id	*proc_id;
			const MR_User_Proc_Id	*selected_proc_id;

			proc_id = &proc->MR_sle_user;
			selected_proc_id = &selected_proc->MR_sle_user;

			if ((proc_id->MR_user_pred_or_func ==
				selected_proc_id->MR_user_pred_or_func)
			&& MR_streq(proc_id->MR_user_decl_module,
				selected_proc_id->MR_user_decl_module)
			&& MR_streq(proc_id->MR_user_name,
				selected_proc_id->MR_user_name)
			&& (proc_id->MR_user_arity ==
				selected_proc_id->MR_user_arity))
			{
				match = MR_TRUE;
			} else {
				match = MR_FALSE;
			}
		} else {
			match = MR_FALSE;
		}

		if (match) {
			list = MR_int_list_cons((MR_Integer) proc, list);
		}
	}

	Layouts = list;
	").

%-----------------------------------------------------------------------------%
:- end_module mdbcomp.rtti_access.
%-----------------------------------------------------------------------------%
