%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_execution.m
% Author: Mark Brown
%
% This module defines a Mercury representation of Mercury program
% execution, the annotated trace.  This structure is described in
% papers/decl_debug.  The declarative debugging infrastructure in the
% trace directory builds an annotated trace, using predicates exported
% from this module.  Once built, the structure is passed to the front
% end (in browser/declarative_debugger.m) where it is analysed
% to produce a bug diagnosis.

:- module mdb.declarative_execution.

:- interface.

:- import_module mdb.util.
:- import_module mdb.term_rep.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.

:- import_module list, std_util, io, bool.

	% This type represents a port in the annotated trace.
	% The type R is the type of references to other nodes
	% in the store.
	%
	% If this type is modified, the procedures below which
	% do destructive update on values of this type may also
	% need to be modified.
	%
:- type trace_node(R)
	--->	call(
			call_preceding		:: R,
						% Preceding event.
			call_last_interface	:: R,
						% Last EXIT, REDO, FAIL or
						% EXCP event.
			call_atom_args		:: list(trace_atom_arg),
						% Atom that was called.
			call_seq		:: sequence_number,
						% Call sequence number.
			call_event		:: event_number,
						% Trace event number.
			call_at_max_depth	:: bool,
						% At the maximum depth?
			call_proc_rep		:: maybe(proc_rep),
						% Body of the called procedure.
			call_return_label	:: maybe(label_layout),
						% The return label, if there
						% is one.
			call_label		:: label_layout,
			call_io_seq_num		:: int
						% The I/O action sequence
						% number at the time of the
						% call.
		)
	;	exit(
			exit_preceding		:: R,
						% Preceding event.
			exit_call		:: R,
						% CALL event.
			exit_prev_redo		:: R,
						% Previous REDO event, if any.
			exit_atom_args		:: list(trace_atom_arg),
						% Atom in its final state.
			exit_event		:: event_number,
						% Trace event number.
			exit_label		:: label_layout,
			exit_io_seq_num		:: int
						% The I/O action sequence
						% number at the time of the
						% exit.
		)
	;	redo(
			redo_preceding		:: R,
						% Preceding event.
			redo_exit		:: R,
						% EXIT event.
			redo_event		:: event_number,
						% REDO event number.
			redo_label		:: label_layout
		)
	;	fail(
			fail_preceding		:: R,
						% Preceding event.
			fail_call		:: R,
						% CALL event.
			fail_redo		:: R,
						% Previous REDO event, if any.
			fail_event		:: event_number,
						% Trace event number.
			fail_label		:: label_layout
		)
	;	excp(
			excp_preceding		:: R,
						% Preceding event.
			excp_call		:: R,
						% Call event.
			excp_redo		:: R,
						% Previous redo, if any.
			excp_value		:: term_rep,
						% Exception thrown.
			excp_event		:: event_number,
						% Trace event number.
			excp_label		:: label_layout
		)
	;	switch(
			switch_preceding	:: R,
						% Preceding event.
			switch_label		:: label_layout
		)
	;	first_disj(
			first_disj_preceding	:: R,
						% Preceding event.
			first_disj_label	:: label_layout
		)
	;	later_disj(
			later_disj_preceding	:: R,
						% Preceding event.
			later_disj_label	:: label_layout,
			later_disj_first	:: R
						% Event of the first DISJ.
		)
	;	cond(
			cond_preceding		:: R,
						% Preceding event.
			cond_label		:: label_layout,
			cond_status		:: goal_status
						% Whether we have reached
						% a THEN or ELSE event.
		)
	;	then(
			then_preceding		:: R,
						% Preceding event.
			then_cond		:: R,
						% COND event.
			then_label		:: label_layout
		)
	;	else(
			else_preceding		:: R,
						% Preceding event.
			else_cond		:: R,
						% COND event.
			else_label		:: label_layout
		)
	;	neg(
			neg_preceding		:: R,
						% Preceding event.
			neg_label		:: label_layout,
						% Path for this event.
			neg_status		:: goal_status
						% Whether we have reached
						% a NEGS or NEGF event.
		)
	;	neg_succ(
			neg_succ_preceding	:: R,
						% Preceding event.
			neg_succ_enter		:: R,
						% NEGE event.
			neg_succ_label		:: label_layout
		)
	;	neg_fail(
			neg_fail_preceding	:: R,
						% Preceding event.
			neg_fail_enter		:: R,
						% NEGE event.
			neg_fail_label		:: label_layout
		).

:- type trace_atom_arg
	--->	arg_info(
			prog_visible		:: bool,
			prog_vis_headvar_num	:: int,
						% N, if this is the Nth
						% programmer visible headvar
						% (as opposed to a variable
						% created by the compiler).
			arg_value		:: maybe(term_rep)
		).

:- type trace_atom
	--->	atom(
			proc_layout		:: proc_layout,
						% Info about the
						% procedure like its name
						% and module and whether it is
						% a function or a predicate.
						
			atom_args		:: list(trace_atom_arg)
						% The arguments, including the
						% compiler-generated ones.
						% XXX This representation can't
						% handle partially instantiated
						% data structures.
		).

:- func get_trace_exit_atom(trace_node(R)) = trace_atom.
:- mode get_trace_exit_atom(in(trace_node_exit)) = out is det.
:- mode get_trace_exit_atom(in) = out is semidet.

:- func get_trace_call_atom(trace_node(R)) = trace_atom.
:- mode get_trace_call_atom(in(trace_node_call)) = out is det.
:- mode get_trace_call_atom(in) = out is semidet.

:- type proc_layout. 

:- func get_proc_label_from_layout(proc_layout) = proc_label.

:- func get_proc_name(proc_label) = string.

:- func get_all_modes_for_layout(proc_layout) = list(proc_layout).

	% get_pred_attributes(ProcLabel, Module, Name, Arity, PredOrFunc).
	% Return the predicate/function attributes common to both UCI and
	% regular predicates/functions.  
	%
:- pred get_pred_attributes(proc_label::in, module_name::out, string::out, 
	int::out, pred_or_func::out) is det.

:- type label_layout.

:- func get_proc_layout_from_label_layout(label_layout) = proc_layout.

:- func get_goal_path_from_label_layout(label_layout) = goal_path_string.

:- func get_goal_path_from_maybe_label(maybe(label_layout)) = goal_path_string.

:- pred get_context_from_label_layout(label_layout::in, string::out, int::out)
	is semidet.

%-----------------------------------------------------------------------------%

	% If the following type is modified, some of the macros in
	% trace/mercury_trace_declarative.h may need to be updated.
	%
:- type goal_status
	--->	succeeded
	;	failed
	;	undecided.

:- type sequence_number == int.
:- type event_number == int.

	% Members of this typeclass represent an entire annotated
	% trace.  The second parameter is the type of references
	% to trace nodes, and the first parameter is the type of
	% a "store": an abstract mapping from references to the
	% nodes they refer to.
	%
	% By convention, we use the names S and R for type variables
	% which are constrained by annotated_trace.  We also use
	% these names in type declarations where it is *intended* that
	% the type variables be constrained by annotated_trace.
	%
	% (Compare with the similar conventions for mercury_edt/2.)
	%
:- typeclass annotated_trace(S, R) where [

		% Dereference the identifier.  This fails if the
		% identifier does not refer to any trace_node (ie.
		% it is a NULL pointer).
		%
	pred trace_node_from_id(S::in, R::in, trace_node(R)::out) is semidet
].

	% Given any node in an annotated trace, find the most recent
	% node in the same contour (ie. the last node which has not been
	% backtracked over, skipping negations, failed conditions, the
	% bodies of calls, and alternative disjuncts).  Throw an exception
	% if there is no such node (ie. if we are at the start of a
	% negation, call, or failed condition).
	%
	% In some cases the contour may reach a dead end.  This can
	% happen if, for example, a DISJ node is not present because
	% it is beyond the depth bound or in a module that is not traced;
	% "stepping left" will arrive at a FAIL, REDO or NEGF node.  Since
	% it is not possible to follow the original contour in these
	% circumstances, we follow the previous contour instead.
	%
:- func step_left_in_contour(S, trace_node(R)) = R <= annotated_trace(S, R).

	% Given any node in an annotated trace, find the most recent
	% node in the same stratum (ie. the most recent node, skipping
	% negations, failed conditions, and the bodies of calls).
	% Throw an exception if there is no such node (ie. if we are at
	% the start of a negation, call, or failed negation).
	%
:- func step_in_stratum(S, trace_node(R)) = R <= annotated_trace(S, R).

	% The following procedures also dereference the identifiers,
	% but they give an error if the node is not of the expected type.
	%
:- pred det_trace_node_from_id(S::in, R::in, trace_node(R)::out) is det
	<= annotated_trace(S, R).

:- inst trace_node_call ---> call(ground, ground, ground, ground, ground,
	ground, ground, ground, ground, ground).

:- pred call_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_call)) 
	is det <= annotated_trace(S, R).

:- inst trace_node_redo ---> redo(ground, ground, ground, ground).

	% maybe_redo_node_from_id/3 fails if the argument is a
	% NULL reference.
	% 
:- pred maybe_redo_node_from_id(S::in, R::in, 
	trace_node(R)::out(trace_node_redo)) is semidet 
	<= annotated_trace(S, R).

:- inst trace_node_exit ---> exit(ground, ground, ground, ground,
	ground, ground, ground).

:- pred exit_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_exit)) 
	is det <= annotated_trace(S, R).

:- inst trace_node_cond ---> cond(ground, ground, ground).

:- pred cond_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_cond)) 
	is det <= annotated_trace(S, R).

:- inst trace_node_neg ---> neg(ground, ground, ground).

:- pred neg_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_neg)) 
	is det <= annotated_trace(S, R).

:- inst trace_node_first_disj ---> first_disj(ground, ground).

:- pred first_disj_node_from_id(S::in, R::in, 
	trace_node(R)::out(trace_node_first_disj)) is det 
	<= annotated_trace(S, R).

:- inst trace_node_disj
	--->	first_disj(ground, ground)
	;	later_disj(ground, ground, ground).

:- pred disj_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_disj)) 
	is det <= annotated_trace(S, R).

	% Load an execution tree which was previously saved by
	% the back end.
	%
:- pred load_trace_node_map(io.input_stream::in, trace_node_map::out,
	trace_node_key::out, io::di, io::uo) is det.

	% Save an execution tree generated by the back end.  It is
	% first converted into a trace_node_map/trace_node_key pair.
	%
:- pred save_trace_node_store(io.output_stream::in, trace_node_store::in,
	trace_node_id::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

	% This instance is used when the declarative debugger is in
	% normal mode.  Values of this instance are produced by the
	% back end and passed directly to the front end.
	%
:- type trace_node_store.
:- type trace_node_id.
:- instance annotated_trace(trace_node_store, trace_node_id).

	% This instance is used when the declarative debugger is in
	% test mode.  Values of this instance are produced by copying
	% values of the previous instance.  Unlike the previous
	% instance, values of this one can be fed through a stream.
	% 
:- type trace_node_map.
:- type trace_node_key.
:- instance annotated_trace(trace_node_map, trace_node_key).

%-----------------------------------------------------------------------------%

:- type which_headvars
	--->	all_headvars
	;	only_user_headvars.

:- pred maybe_filter_headvars(which_headvars::in, list(trace_atom_arg)::in,
	list(trace_atom_arg)::out) is det.

:- func chosen_head_vars_presentation = which_headvars.

:- pred is_user_visible_arg(trace_atom_arg::in) is semidet.

:- pred select_arg_at_pos(arg_pos::in, list(trace_atom_arg)::in,
	trace_atom_arg::out) is det.

:- pred absolute_arg_num(arg_pos::in, trace_atom::in, int::out)
	is det.

%-----------------------------------------------------------------------------%
	
:- implementation.

:- import_module mdb.declarative_debugger.
:- import_module int, map, exception, store.
:- import_module require.
:- import_module mdb.declarative_edt.
:- import_module string.

%-----------------------------------------------------------------------------%

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

:- func get_special_pred_id_name(special_pred_id) = string.

get_special_pred_id_name(unify) = "__Unify__".
get_special_pred_id_name(index) = "__Index__".
get_special_pred_id_name(compare) = "__Compare__".
get_special_pred_id_name(initialise) = "__Initialise__".

:- func get_special_pred_id_arity(special_pred_id) = int.

get_special_pred_id_arity(unify) = 2.
get_special_pred_id_arity(index) = 2.
get_special_pred_id_arity(compare) = 3.
get_special_pred_id_arity(initialise) = 1.

get_pred_attributes(ProcId, Module, Name, Arity, PredOrFunc) :-
	(
		ProcId = proc(Module, PredOrFunc, _, Name, Arity, _)
	;
		ProcId = special_proc(Module, SpecialId, _, _, _, _), 
		PredOrFunc = predicate,
		Arity = get_special_pred_id_arity(SpecialId),
		Name = get_special_pred_id_name(SpecialId)
	).

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", label_layout, "const MR_Label_Layout *",
	[can_pass_as_mercury_type, stable]).

	% stub only
:- pragma foreign_type("Java", label_layout, "java.lang.Object", []). 

:- pragma foreign_proc("C", get_proc_layout_from_label_layout(Label::in)
	= (ProcLayout::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	ProcLayout = Label->MR_sll_entry;
").

:- pragma foreign_proc("C", get_goal_path_from_label_layout(Label::in)
	= (GoalPath::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	GoalPath = (MR_String)MR_label_goal_path(Label);
").

get_goal_path_from_maybe_label(yes(Label)) 
	= get_goal_path_from_label_layout(Label).
get_goal_path_from_maybe_label(no) = "".

:- pragma foreign_proc("C", get_context_from_label_layout(Label::in, 
	FileName::out, LineNo::out), 
	[will_not_call_mercury, thread_safe, promise_pure],
"
	const char	*filename;
	
	SUCCESS_INDICATOR = MR_find_context(Label, &filename, &LineNo);
	MR_TRACE_USE_HP(
		MR_make_aligned_string(FileName, (MR_String) filename);
	);
").

%-----------------------------------------------------------------------------%

get_trace_exit_atom(exit(_, _, _, AtomArgs, _, Label, _)) = Atom :-
	ProcLayout = get_proc_layout_from_label_layout(Label),
	Atom = atom(ProcLayout, AtomArgs).

get_trace_call_atom(call(_, _, AtomArgs, _, _, _, _, _, Label, _)) = Atom :-
	ProcLayout = get_proc_layout_from_label_layout(Label),
	Atom = atom(ProcLayout, AtomArgs).

%-----------------------------------------------------------------------------%

step_left_in_contour(Store, exit(_, Call, _, _, _, _, _)) = Prec :-
	call_node_from_id(Store, Call, CallNode),
	Prec = CallNode ^ call_preceding.
step_left_in_contour(Store, excp(_, Call, _, _, _, _)) = Prec :-
	call_node_from_id(Store, Call, CallNode),
	Prec = CallNode ^ call_preceding.
step_left_in_contour(_, switch(Prec, _)) = Prec.
step_left_in_contour(_, first_disj(Prec, _)) = Prec.
step_left_in_contour(Store, later_disj(_, _, FirstDisj)) = Prec :-
	first_disj_node_from_id(Store, FirstDisj, first_disj(Prec, _)).
step_left_in_contour(_, cond(Prec, _, Status)) = Node :-
	(
		Status = failed
	->
		throw(internal_error("step_left_in_contour",
			"failed COND node"))
	;
		Node = Prec
	).
step_left_in_contour(_, then(Prec, _, _)) = Prec.
step_left_in_contour(Store, else(_, Cond, _)) = Prec :-
	cond_node_from_id(Store, Cond, cond(Prec, _, _)).
step_left_in_contour(Store, neg_succ(_, Neg, _)) = Prec :-
	neg_node_from_id(Store, Neg, neg(Prec, _, _)).
	%
	% The following cases are possibly at the left end of a contour,
	% where we cannot step any further.
	%
step_left_in_contour(_, call(_, _, _, _, _, _, _, _, _, _)) = _ :-
	throw(internal_error("step_left_in_contour", "unexpected CALL node")).
step_left_in_contour(_, neg(Prec, _, Status)) = Next :-
	(
		Status = undecided
	->
			%
			% An exception must have been thrown inside the
			% negation, so we don't consider it a separate
			% context.
			%
		Next = Prec
	;
		throw(internal_error("step_left_in_contour",
			"unexpected NEGE node"))
	).
	%
	% In the remaining cases we have reached a dead end, so we
	% step to the previous contour instead.
	%
step_left_in_contour(Store, Node) = Prec :-
	Node = fail(_, _, _, _, _),
	find_prev_contour(Store, Node, Prec).
step_left_in_contour(Store, Node) = Prec :-
	Node = redo(_, _, _, _),
	find_prev_contour(Store, Node, Prec).
step_left_in_contour(Store, Node) = Prec :-
	Node = neg_fail(_, _, _),
	find_prev_contour(Store, Node, Prec).

	% Given any node which is not on a contour, find a node on
	% the previous contour in the same stratum.
	%
:- pred find_prev_contour(S, trace_node(R), R) <= annotated_trace(S, R).
:- mode find_prev_contour(in, in, out) is semidet.
:- mode find_prev_contour(in, in(trace_node_reverse), out) is det.

:- inst trace_node_reverse
	---> 	fail(ground, ground, ground, ground, ground)
	;	redo(ground, ground, ground, ground)
	;	neg_fail(ground, ground, ground).

find_prev_contour(Store, fail(_, Call, _, _, _), OnContour) :-
	call_node_from_id(Store, Call, CallNode),
	OnContour = CallNode ^ call_preceding.
find_prev_contour(Store, redo(_, Exit, _, _), OnContour) :-
	exit_node_from_id(Store, Exit, ExitNode),
	OnContour = ExitNode ^ exit_preceding.
find_prev_contour(Store, neg_fail(_, Neg, _), OnContour) :-
	neg_node_from_id(Store, Neg, neg(OnContour, _, _)).
	%
	% The following cases are at the left end of a contour,
	% so there are no previous contours in the same stratum.
	%
find_prev_contour(_, call(_, _, _, _, _, _, _, _, _, _), _) :-
	throw(internal_error("find_prev_contour", "reached CALL node")).
find_prev_contour(_, cond(_, _, _), _) :-
	throw(internal_error("find_prev_contour", "reached COND node")).
find_prev_contour(_, neg(_, _, _), _) :-
	throw(internal_error("find_prev_contour", "reached NEGE node")).

step_in_stratum(Store, exit(_, Call, MaybeRedo, _, _, _, _)) =
	step_over_redo_or_call(Store, Call, MaybeRedo).
step_in_stratum(Store, fail(_, Call, MaybeRedo, _, _)) =
	step_over_redo_or_call(Store, Call, MaybeRedo).
step_in_stratum(Store, excp(_, Call, MaybeRedo, _, _, _)) =
	step_over_redo_or_call(Store, Call, MaybeRedo).
step_in_stratum(Store, redo(_, Exit, _, _)) = Next :-
	exit_node_from_id(Store, Exit, ExitNode),
	Next = ExitNode ^ exit_preceding.
step_in_stratum(_, switch(Next, _)) = Next.
step_in_stratum(_, first_disj(Next, _)) = Next.
step_in_stratum(_, later_disj(Next, _, _)) = Next.
step_in_stratum(_, cond(Prec, _, Status)) = Next :-
	(
		Status = failed
	->
		throw(internal_error("step_in_stratum", "failed COND node"))
	;
		Next = Prec
	).
step_in_stratum(_, then(Next, _, _)) = Next.
step_in_stratum(Store, else(_, Cond, _)) = Next :-
	cond_node_from_id(Store, Cond, cond(Next, _, _)).
step_in_stratum(Store, neg_succ(_, Neg, _)) = Next :-
	neg_node_from_id(Store, Neg, neg(Next, _, _)).
step_in_stratum(Store, neg_fail(_, Neg, _)) = Next :-
	neg_node_from_id(Store, Neg, neg(Next, _, _)).
	%
	% The following cases mark the boundary of the stratum,
	% so we cannot step any further.
	%
step_in_stratum(_, call(_, _, _, _, _, _, _, _, _, _)) = _ :-
	throw(internal_error("step_in_stratum", "unexpected CALL node")).
step_in_stratum(_, neg(_, _, _)) = _ :-
	throw(internal_error("step_in_stratum", "unexpected NEGE node")).

:- func step_over_redo_or_call(S, R, R) = R <= annotated_trace(S, R).

step_over_redo_or_call(Store, Call, MaybeRedo) = Next :-
	(
		maybe_redo_node_from_id(Store, MaybeRedo, Redo)
	->
		Redo = redo(Next, _, _, _)
	;
		call_node_from_id(Store, Call, CallNode),
		Next = CallNode ^ call_preceding
	).

det_trace_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0)
	->
		Node = Node0
	;
		throw(internal_error("det_trace_node_from_id", "NULL node id"))
	).

call_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = call(_, _, _, _, _, _, _, _, _, _)
	->
		Node = Node0
	;
		throw(internal_error("call_node_from_id", "not a CALL node"))
	).

maybe_redo_node_from_id(Store, NodeId, Node) :-
	trace_node_from_id(Store, NodeId, Node0),
	(
		Node0 = redo(_, _, _, _)
	->
		Node = Node0
	;
		throw(internal_error("maybe_redo_node_from_id",
			"not a REDO node or NULL"))
	).

exit_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = exit(_, _, _, _, _, _, _)
	->
		Node = Node0
	;
		throw(internal_error("exit_node_from_id", "not an EXIT node"))
	).

cond_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = cond(_, _, _)
	->
		Node = Node0
	;
		throw(internal_error("cond_node_from_id", "not a COND node"))
	).

neg_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = neg(_, _, _)
	->
		Node = Node0
	;
		throw(internal_error("neg_node_from_id", "not a NEG node"))
	).

first_disj_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = first_disj(_, _)
	->
		Node = Node0
	;
		throw(internal_error("first_disj_node_from_id",
			"not a first DISJ node"))
	).

disj_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		( Node0 = first_disj(_, _)
		; Node0 = later_disj(_, _, _)
		)
	->
		Node = Node0
	;
		throw(internal_error("disj_node_from_id",
			"not a DISJ node"))
	).

%-----------------------------------------------------------------------------%

:- instance annotated_trace(trace_node_store, trace_node_id) where [
	pred(trace_node_from_id/3) is search_trace_node_store
].

	% The "map" is actually just an integer representing the version
	% of the map.  The empty map should be given the value 0, and
	% each time the map is destructively modified (by C code), the
	% value should be incremented.
	%
:- type trace_node_store ---> store(int).

	% The implementation of the identifiers is the same as what
	% is identified.  This fact is hidden, however, to force the
	% abstract map to be explicitly used whenever a new node is
	% accessed.
	%
:- type trace_node_id ---> id(c_pointer).

:- pred search_trace_node_store(trace_node_store::in, trace_node_id::in,
	trace_node(trace_node_id)::out) is semidet.

:- pragma foreign_proc("C",
	search_trace_node_store(_Store::in, Id::in, Node::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Node = Id;
	SUCCESS_INDICATOR = (Id != (MR_Word) NULL);
"
).
search_trace_node_store(_, _, _) :-
	private_builtin.sorry("search_trace_node_store").

	%
	% Following are some predicates that are useful for
	% manipulating the above instance in C code.
	%

:- func call_node_get_last_interface(trace_node(trace_node_id))
		= trace_node_id.
:- pragma export(call_node_get_last_interface(in) = out,
		"MR_DD_call_node_get_last_interface").

call_node_get_last_interface(Call) = Last :-
	(
		Call = call(_, Last0, _, _, _, _, _, _, _, _)
	->
		Last = Last0
	;
		throw(internal_error("call_node_get_last_interface",
			"not a CALL node"))
	).

:- func call_node_set_last_interface(trace_node(trace_node_id)::di, 
	trace_node_id::di) = (trace_node(trace_node_id)::out) is det.

:- pragma export(call_node_set_last_interface(di, di) = out,
		"MR_DD_call_node_set_last_interface").

call_node_set_last_interface(Call0, Last) = Call :-
	(
		Call0 = call(_, _, _, _, _, _, _, _, _, _)
	->
		Call1 = Call0
	;
		throw(internal_error("call_node_set_last_interface",
			"not a CALL node"))
	),
		% The last interface is the second field, so we pass 1
		% (since argument numbers start from 0).
		%
	set_trace_node_arg(Call1, 1, Last, Call).

:- func cond_node_set_status(trace_node(trace_node_id)::di, goal_status::di)
		= (trace_node(trace_node_id)::out) is det.

:- pragma export(cond_node_set_status(di, di) = out,
		"MR_DD_cond_node_set_status").

cond_node_set_status(Cond0, Status) = Cond :-
	(
		Cond0 = cond(_, _, _)
	->
		Cond1 = Cond0
	;
		throw(internal_error("cond_node_set_status", "not a COND node"))
	),
		% The goal status is the third field, so we pass 2
		% (since argument numbers start from 0).
		%
	set_trace_node_arg(Cond1, 2, Status, Cond).

:- func neg_node_set_status(trace_node(trace_node_id)::di, goal_status::di)
		= (trace_node(trace_node_id)::out) is det.

:- pragma export(neg_node_set_status(di, di) = out,
		"MR_DD_neg_node_set_status").

neg_node_set_status(Neg0, Status) = Neg :-
	(
		Neg0 = neg(_, _, _)
	->
		Neg1 = Neg0
	;
		throw(internal_error("neg_node_set_status", "not a NEGE node"))
	),
		% The goal status is the third field, so we pass 2
		% (since argument numbers start from 0).
		%
	set_trace_node_arg(Neg1, 2, Status, Neg).

:- pred set_trace_node_arg(trace_node(trace_node_id)::di, int::in, T::di,
		trace_node(trace_node_id)::out) is det.

set_trace_node_arg(Node0, FieldNum, Val, Node) :-
	store.new(S0),
	store.new_ref(Node0, Ref, S0, S1),
	store.arg_ref(Ref, FieldNum, ArgRef, S1, S2),
	store.set_ref_value(ArgRef, Val, S2, S),
	store.extract_ref_value(S, Ref, Node).

:- func trace_node_port(trace_node(trace_node_id)) = trace_port.
:- pragma export(trace_node_port(in) = out,
		"MR_DD_trace_node_port").

trace_node_port(call(_, _, _, _, _, _, _, _, _, _))	= call.
trace_node_port(exit(_, _, _, _, _, _, _))		= exit.
trace_node_port(redo(_, _, _, _))			= redo.
trace_node_port(fail(_, _, _, _, _))			= fail.
trace_node_port(excp(_, _, _, _, _, _))			= exception.
trace_node_port(switch(_, _))				= switch.
trace_node_port(first_disj(_, _))			= disj.
trace_node_port(later_disj(_, _, _))			= disj.
trace_node_port(cond(_, _, _))				= ite_cond.
trace_node_port(then(_, _, _))				= ite_then.
trace_node_port(else(_, _, _))				= ite_else.
trace_node_port(neg(_, _, _))				= neg_enter.
trace_node_port(neg_succ(_, _, _))			= neg_success.
trace_node_port(neg_fail(_, _, _))			= neg_failure.

:- func trace_node_path(trace_node(trace_node_id)) = goal_path_string.
:- pragma export(trace_node_path(in) = out, "MR_DD_trace_node_path").

trace_node_path(Node) = Path :-
	Label = get_trace_node_label(Node),
	Path = get_goal_path_from_label_layout(Label).

:- func get_trace_node_label(trace_node(R)) = label_layout.

get_trace_node_label(call(_, _, _, _, _, _, _, _, Label, _)) = Label.
get_trace_node_label(exit(_, _, _, _, _, Label, _)) = Label.
get_trace_node_label(redo(_, _, _, Label)) = Label.
get_trace_node_label(fail(_, _, _, _, Label)) = Label.
get_trace_node_label(excp(_, _, _, _, _, Label)) = Label.
get_trace_node_label(switch(_, Label)) = Label.
get_trace_node_label(first_disj(_, Label)) = Label.
get_trace_node_label(later_disj(_, Label, _)) = Label.
get_trace_node_label(cond(_, Label, _)) = Label.
get_trace_node_label(then(_, _, Label)) = Label.
get_trace_node_label(else(_, _, Label)) = Label.
get_trace_node_label(neg(_, Label, _)) = Label.
get_trace_node_label(neg_succ(_, _, Label)) = Label.
get_trace_node_label(neg_fail(_, _, Label)) = Label.

:- pred trace_node_seqno(trace_node_store::in, trace_node(trace_node_id)::in,
		sequence_number::out) is semidet.

:- pragma export(trace_node_seqno(in, in, out), "MR_DD_trace_node_seqno").

trace_node_seqno(S, Node, SeqNo) :-
	(
		SeqNo0 = Node ^ call_seq
	->
		SeqNo = SeqNo0
	;
		trace_node_call(S, Node, Call),
		call_node_from_id(S, Call, CallNode),
		SeqNo = CallNode ^ call_seq
	).

:- pred trace_node_call(trace_node_store::in, trace_node(trace_node_id)::in,
		trace_node_id::out) is semidet.

:- pragma export(trace_node_call(in, in, out), "MR_DD_trace_node_call").

trace_node_call(_, exit(_, Call, _, _, _, _, _), Call).
trace_node_call(S, redo(_, Exit, _, _), Call) :-
	exit_node_from_id(S, Exit, ExitNode),
	Call = ExitNode ^ exit_call.
trace_node_call(_, fail(_, Call, _, _, _), Call).
trace_node_call(_, excp(_, Call, _, _, _, _), Call).

:- pred trace_node_first_disj(trace_node(trace_node_id)::in, 
	trace_node_id::out) is semidet.

:- pragma export(trace_node_first_disj(in, out),
		"MR_DD_trace_node_first_disj").

trace_node_first_disj(first_disj(_, _), NULL) :-
	null_trace_node_id(NULL).
trace_node_first_disj(later_disj(_, _, FirstDisj), FirstDisj).	

	% Export a version of this function to be called by C code
	% in trace/mercury_trace_declarative.c.
	%
:- func step_left_in_contour_store(trace_node_store, trace_node(trace_node_id))
		= trace_node_id.
:- pragma export(step_left_in_contour_store(in, in) = out,
		"MR_DD_step_left_in_contour").

step_left_in_contour_store(Store, Node) = step_left_in_contour(Store, Node).

	% Export a version of this function to be called by C code
	% in trace/declarative_debugger.c.  If called with a node
	% that is already on a contour, this function returns the
	% same node.  This saves the C code from having to perform
	% that check itself.
	%
:- func find_prev_contour_store(trace_node_store, trace_node_id)
		= trace_node_id.
:- pragma export(find_prev_contour_store(in, in) = out,
		"MR_DD_find_prev_contour").

find_prev_contour_store(Store, Id) = Prev :-
	det_trace_node_from_id(Store, Id, Node),
	(
		find_prev_contour(Store, Node, Prev0)
	->
		Prev = Prev0
	;
		Prev = Id
	).

	% Print a text representation of a trace node, useful
	% for debugging purposes.
	%
:- pred print_trace_node(io.output_stream::in, trace_node(trace_node_id)::in,
		io::di, io::uo) is det.

:- pragma export(print_trace_node(in, in, di, uo), "MR_DD_print_trace_node").

print_trace_node(OutStr, Node, !IO) :-
	convert_node(Node, CNode),
	io.write(OutStr, CNode, !IO).

%-----------------------------------------------------------------------------%

	%
	% Each node type has a Mercury function which constructs
	% a node of that type.  The functions are exported to C so
	% that the back end can build an execution tree.
	%

:- func construct_call_node(trace_node_id, list(trace_atom_arg), 
	sequence_number, event_number, bool, maybe(label_layout), 
	label_layout, int) = trace_node(trace_node_id).
:- pragma export(construct_call_node(in, in, in, in, in, in, in, in) = out,
	"MR_DD_construct_call_node").

construct_call_node(Preceding, AtomArgs, SeqNo, EventNo, MaxDepth, 
		MaybeReturnLabel, Label, IoSeqNum) = Call :-
	Call = call(Preceding, Answer, AtomArgs, SeqNo, EventNo, MaxDepth,
		no, MaybeReturnLabel, Label, IoSeqNum),
	null_trace_node_id(Answer).

:- func construct_call_node_with_goal(trace_node_id, list(trace_atom_arg),
	sequence_number, event_number, bool, proc_rep, maybe(label_layout), 
	label_layout, int) = trace_node(trace_node_id).
:- pragma export(construct_call_node_with_goal(in, in, in, in, in, in, in, in,
	in) = out, "MR_DD_construct_call_node_with_goal").

construct_call_node_with_goal(Preceding, AtomArgs, SeqNo, EventNo, MaxDepth,
		ProcRep, MaybeReturnLabel, Label, IoSeqNum) = Call :-
	Call = call(Preceding, Answer, AtomArgs, SeqNo, EventNo, MaxDepth,
		yes(ProcRep), MaybeReturnLabel, Label, IoSeqNum),
	null_trace_node_id(Answer).

:- func make_yes_maybe_label(label_layout) = maybe(label_layout).
:- pragma export(make_yes_maybe_label(in) = out, "MR_DD_make_yes_maybe_label").

make_yes_maybe_label(Label) = yes(Label).

:- func make_no_maybe_label = maybe(label_layout).
:- pragma export(make_no_maybe_label = out, "MR_DD_make_no_maybe_label").

make_no_maybe_label = no.

:- func construct_exit_node(trace_node_id, trace_node_id, trace_node_id,
	list(trace_atom_arg), event_number, label_layout, int) 
	= trace_node(trace_node_id).
:- pragma export(construct_exit_node(in, in, in, in, in, in, in) = out,
	"MR_DD_construct_exit_node").

construct_exit_node(Preceding, Call, MaybeRedo, AtomArgs, EventNo, Label, 
		IoSeqNum) = 
	exit(Preceding, Call, MaybeRedo, AtomArgs, EventNo, Label, IoSeqNum).

:- func construct_redo_node(trace_node_id, trace_node_id, event_number,
	label_layout) = trace_node(trace_node_id).
:- pragma export(construct_redo_node(in, in, in, in) = out,
	"MR_DD_construct_redo_node").

construct_redo_node(Preceding, Exit, Event, Label) 
	= redo(Preceding, Exit, Event, Label).

:- func construct_fail_node(trace_node_id, trace_node_id, trace_node_id,
	event_number, label_layout) = trace_node(trace_node_id).
:- pragma export(construct_fail_node(in, in, in, in, in) = out,
	"MR_DD_construct_fail_node").

construct_fail_node(Preceding, Call, Redo, EventNo, Label) =
		fail(Preceding, Call, Redo, EventNo, Label).

:- pred construct_excp_node(trace_node_id::in, trace_node_id::in, 
	trace_node_id::in, univ::in, event_number::in, label_layout::in,
	trace_node(trace_node_id)::out) is cc_multi.
:- pragma export(construct_excp_node(in, in, in, in, in, in, out),
	"MR_DD_construct_excp_node").

construct_excp_node(Preceding, Call, MaybeRedo, Exception, EventNo, Label, 
		Excp) :-
	term_rep.univ_to_rep(Exception, ExceptionRep),
	Excp = excp(Preceding, Call, MaybeRedo, ExceptionRep, EventNo, Label).

:- func construct_switch_node(trace_node_id, label_layout)
		= trace_node(trace_node_id).
:- pragma export(construct_switch_node(in, in) = out,
		"MR_DD_construct_switch_node").

construct_switch_node(Preceding, Label) = switch(Preceding, Label).

:- func construct_first_disj_node(trace_node_id, label_layout)
		= trace_node(trace_node_id).
:- pragma export(construct_first_disj_node(in, in) = out,
		"MR_DD_construct_first_disj_node").

construct_first_disj_node(Preceding, Label) = first_disj(Preceding, Label).

:- func construct_later_disj_node(trace_node_store, trace_node_id,
		label_layout, trace_node_id) = trace_node(trace_node_id).
:- pragma export(construct_later_disj_node(in, in, in, in) = out,
		"MR_DD_construct_later_disj_node").

construct_later_disj_node(Store, Preceding, Label, PrevDisj)
		= later_disj(Preceding, Label, FirstDisj) :-
	disj_node_from_id(Store, PrevDisj, PrevDisjNode),
	(
		PrevDisjNode = first_disj(_, _),
		FirstDisj = PrevDisj
	;
		PrevDisjNode = later_disj(_, _, FirstDisj)
	).

:- func construct_cond_node(trace_node_id, label_layout)
		= trace_node(trace_node_id).
:- pragma export(construct_cond_node(in, in) = out,
		"MR_DD_construct_cond_node").

construct_cond_node(Preceding, Label) = cond(Preceding, Label, undecided).

:- func construct_then_node(trace_node_id, trace_node_id, label_layout)
		= trace_node(trace_node_id).
:- pragma export(construct_then_node(in, in, in) = out,
		"MR_DD_construct_then_node").

construct_then_node(Preceding, Cond, Label) = then(Preceding, Cond, Label).

:- func construct_else_node(trace_node_id, trace_node_id, label_layout)
		= trace_node(trace_node_id).
:- pragma export(construct_else_node(in, in, in) = out,
		"MR_DD_construct_else_node").

construct_else_node(Preceding, Cond, Label) = else(Preceding, Cond, Label).

:- func construct_neg_node(trace_node_id, label_layout)
		= trace_node(trace_node_id).
:- pragma export(construct_neg_node(in, in) = out,
		"MR_DD_construct_neg_node").

construct_neg_node(Preceding, Label) = neg(Preceding, Label, undecided).

:- func construct_neg_succ_node(trace_node_id, trace_node_id, label_layout)
		= trace_node(trace_node_id).
:- pragma export(construct_neg_succ_node(in, in, in) = out,
		"MR_DD_construct_neg_succ_node").

construct_neg_succ_node(Preceding, Neg, Label) 
	= neg_succ(Preceding, Neg, Label).

:- func construct_neg_fail_node(trace_node_id, trace_node_id, label_layout)
		= trace_node(trace_node_id).
:- pragma export(construct_neg_fail_node(in, in, in) = out,
		"MR_DD_construct_neg_fail_node").

construct_neg_fail_node(Preceding, Neg, Label) 
	= neg_fail(Preceding, Neg, Label).

:- pred null_trace_node_id(trace_node_id::out) is det.

:- pragma foreign_proc("C",
	null_trace_node_id(Id::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"Id = (MR_Word) NULL;"
).

null_trace_node_id(_) :-
	private_builtin.sorry("null_trace_node_id").

:- func init_trace_atom_args = list(trace_atom_arg).

:- pragma export(init_trace_atom_args = out, "MR_DD_init_trace_atom_args").

init_trace_atom_args = [].

	% add_trace_atom_arg_value(HldsNum, ProgVis, Val, !AtomArgs):
	% Add the argument with value Val and HLDS number HldsNum to the
	% beginning of a list of arguments for an atom.  ProgVis is a C
	% boolean, which is true iff variable HldsNum is a user visible
	% variable.
	%
:- pred add_trace_atom_arg_value(int::in, int::in, univ::in, 
	list(trace_atom_arg)::in, list(trace_atom_arg)::out) is cc_multi.
:- pragma export(add_trace_atom_arg_value(in, in, in, in, out),
	"MR_DD_add_trace_atom_arg_value").

add_trace_atom_arg_value(HldsNum, ProgVis, Val, Args, [Arg | Args]) :-
	term_rep.univ_to_rep(Val, Rep),
	Arg = arg_info(c_bool_to_merc_bool(ProgVis), HldsNum, yes(Rep)).

	% Like add_trace_atom_arg_value, except that the specified variable
	% has no value (i.e. it is not bound).
:- pred add_trace_atom_arg_no_value(int::in, int::in, 
	list(trace_atom_arg)::in, list(trace_atom_arg)::out) is det.
:- pragma export(add_trace_atom_arg_no_value(in, in, in, out),
		"MR_DD_add_trace_atom_arg_no_value").

add_trace_atom_arg_no_value(HldsNum, ProgVis, Args, [Arg | Args]) :-
	Arg = arg_info(c_bool_to_merc_bool(ProgVis), HldsNum, no).

	% This code converts a C bool (represented as int) to a Mercury bool.
:- func c_bool_to_merc_bool(int) = bool.

c_bool_to_merc_bool(ProgVis) =
	( ProgVis = 0 ->
		no
	;
		yes
	).

	% Create a temporary placeholder until the code MR_decl_make_atom
	% can fill in all the argument slots.
:- func dummy_arg_info = trace_atom_arg.

dummy_arg_info = arg_info(no, -1, no).

%-----------------------------------------------------------------------------%

	% The most important property of this instance is that it
	% can be written to or read in from a stream easily.  It
	% is not as efficient to use as the earlier instance, though.
	%
:- instance annotated_trace(trace_node_map, trace_node_key) where [
	pred(trace_node_from_id/3) is search_trace_node_map
].

:- type trace_node_map
	--->	map(map(trace_node_key, trace_node(trace_node_key))).

	% Values of this type are represented in the same way (in the
	% underlying C code) as corresponding values of the other
	% instance.
	%
:- type trace_node_key
	--->	key(int).

:- pred search_trace_node_map(trace_node_map::in, trace_node_key::in,
		trace_node(trace_node_key)::out) is semidet.

search_trace_node_map(map(Map), Key, Node) :-
	map.search(Map, Key, Node).

load_trace_node_map(Stream, Map, Key, !IO) :-
	io.read(Stream, ResKey, !IO),
	(
		ResKey = ok(Key)
	;
		ResKey = eof,
		throw(io_error("load_trace_node_map", "unexpected EOF"))
	;
		ResKey = error(Msg, _),
		throw(io_error("load_trace_node_map", Msg))
	),
	io.read(Stream, ResMap, !IO),
	(
		ResMap = ok(Map)
	;
		ResMap = eof,
		throw(io_error("load_trace_node_map", "unexpected EOF"))
	;
		ResMap = error(Msg, _),
		throw(io_error("load_trace_node_map", Msg))
	).

:- pragma export(save_trace_node_store(in, in, in, di, uo),
		"MR_DD_save_trace").

save_trace_node_store(Stream, Store, NodeId, !IO) :-
	map.init(Map0),
	node_id_to_key(NodeId, Key),
	node_map(Store, NodeId, map(Map0), Map),
	io.write(Stream, Key, !IO),
	io.write_string(Stream, ".\n", !IO),
	io.write(Stream, Map, !IO),
	io.write_string(Stream, ".\n", !IO).

:- pred node_map(trace_node_store::in, trace_node_id::in, trace_node_map::in,
		trace_node_map::out) is det.

node_map(Store, NodeId, map(Map0), Map) :-
	(
		search_trace_node_store(Store, NodeId, Node1)
	->
		node_id_to_key(NodeId, Key),
		convert_node(Node1, Node2),
		map.det_insert(Map0, Key, Node2, Map1),
		Next = preceding_node(Node1),
		node_map(Store, Next, map(Map1), Map)
	;
		Map = map(Map0)
	).

:- pred node_id_to_key(trace_node_id::in, trace_node_key::out) is det.

:- pragma foreign_proc("C", node_id_to_key(Id::in, Key::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"Key = (MR_Integer) Id;").

node_id_to_key(_, _) :-
	private_builtin.sorry("node_id_to_key").

:- pred convert_node(trace_node(trace_node_id)::in, 
	trace_node(trace_node_key)::out) is det.

:- pragma foreign_proc("C", convert_node(N1::in, N2::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"N2 = N1;").

convert_node(_, _) :-
	private_builtin.sorry("convert_node").

	% Given a node in an annotated trace, return a reference to
	% the preceding node in the trace, or a NULL reference if
	% it is the first.
	%
:- func preceding_node(trace_node(T)) = T.

preceding_node(call(P, _, _, _, _, _, _, _, _, _)) = P.
preceding_node(exit(P, _, _, _, _, _, _))	= P.
preceding_node(redo(P, _, _, _))		= P.
preceding_node(fail(P, _, _, _, _))		= P.
preceding_node(excp(P, _, _, _, _, _))		= P.
preceding_node(switch(P, _))			= P.
preceding_node(first_disj(P, _))		= P.
preceding_node(later_disj(P, _, _))		= P.
preceding_node(cond(P, _, _))			= P.
preceding_node(then(P, _, _))			= P.
preceding_node(else(P, _, _))			= P.
preceding_node(neg(P, _, _))			= P.
preceding_node(neg_succ(P, _, _))		= P.
preceding_node(neg_fail(P, _, _))		= P.

%-----------------------------------------------------------------------------%

maybe_filter_headvars(Which, Args0, Args) :-
	(
		Which = all_headvars,
		Args = Args0
	;
		Which = only_user_headvars,
		Args = list.filter(is_user_visible_arg, Args0)
	).

chosen_head_vars_presentation = only_user_headvars.

is_user_visible_arg(arg_info(yes, _, _)).

select_arg_at_pos(ArgPos, Args0, Arg) :-
	(
		ArgPos = user_head_var(N),
		Which = only_user_headvars
	;
		ArgPos = any_head_var(N),
		Which = all_headvars
	;
		ArgPos = any_head_var_from_back(M),
		N = length(Args0) - M + 1,
		Which = all_headvars
	),
	maybe_filter_headvars(Which, Args0, Args),
	list.index1_det(Args, N, Arg).

absolute_arg_num(any_head_var(ArgNum), _, ArgNum).
absolute_arg_num(user_head_var(N), atom(_, Args), ArgNum) :-
	head_var_num_to_arg_num(Args, N, 1, ArgNum).
absolute_arg_num(any_head_var_from_back(M), atom(_, Args), length(Args)-M+1).

:- pred head_var_num_to_arg_num(list(trace_atom_arg)::in, int::in, int::in,
	int::out) is det.

head_var_num_to_arg_num([], _, _, _) :-
	throw(internal_error("head_var_num_to_arg_num",
		"nonexistent head_var_num")).
head_var_num_to_arg_num([Arg | Args], SearchUserHeadVarNum, CurArgNum,
		ArgNum) :-
	Arg = arg_info(UserVis, _, _),
	(
		UserVis = no,
		head_var_num_to_arg_num(Args, SearchUserHeadVarNum,
			CurArgNum + 1, ArgNum)
	;
		UserVis = yes,
		( SearchUserHeadVarNum = 1 ->
			ArgNum = CurArgNum
		;
			head_var_num_to_arg_num(Args, SearchUserHeadVarNum - 1,
				CurArgNum + 1, ArgNum)
		)
	).
