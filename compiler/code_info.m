%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: code_info.m
%
% Main authors: conway, zs.
%
% This file defines the code_info type and various operations on it.
%
% The code_info structure is a 'state' used by the code generator.
%
% The following assumptions are made about the state of the high-level
% data structure:
%
%	o  Variables can be stored in any number of distinct places.
%	o  Registers may contain a value corresponding to more than
%		one variable.
%	o  Procedures are in superhomogeneous form. This means that
%		construction unifications and builtins are not nested.
%	o  Evaluation of arguments in construction and deconstruction
%		unifications is lazy. This means that arguments in a
%		`don't care' mode are ignored, and that assignments
%		are cached.
%
% Note: Any new "state" arguments (eg counters) that should be strictly
% threaded (for example the counter used for allocating new labels) will
% require changes to code_info__slap_code_info/3.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_info.

:- interface.

:- import_module hlds_pred, hlds_goal, llds, instmap.
:- import_module code_util, tree, globals, unify_proc.
:- import_module bool, set, std_util, assoc_list.

:- implementation.

:- import_module hlds_module, hlds_data.
:- import_module code_exprn, set, varset, term, stack, prog_data.
:- import_module type_util, mode_util, options, shapes.
:- import_module string, require, char, list, map, bimap, tree, int.

%---------------------------------------------------------------------------%

	% Submodule for the code_info type and its access predicates.
	%
	% This submodule has the following components:
	%
	%	declarations for exported access predicates
	%	declarations for non-exported access predicates
	%	the definition of the type and the init predicate
	%	the definition of the read access predicates
	%	the definition of the write access predicates
	%
	% Please keep the order of mention of the various field consistent
	% in each of these five components.

:- interface.

:- type code_info.

		% Create a new code_info structure.
:- pred code_info__init(varset, liveness_info, stack_slots, bool, globals,
	pred_id, proc_id, proc_info, instmap, follow_vars, module_info,
	shape_table, code_info).
:- mode code_info__init(in, in, in, in, in, in, in, in, in, in, in, in, out)
	is det.

		% Get the variables for the current procedure.
:- pred code_info__get_varset(varset, code_info, code_info).
:- mode code_info__get_varset(out, in, out) is det.

		% Get the hlds mapping from variables to stack slots
:- pred code_info__get_stack_slots(stack_slots, code_info, code_info).
:- mode code_info__get_stack_slots(out, in, out) is det.

		% Get the id of the predicate we are generating code for.
:- pred code_info__get_pred_id(pred_id, code_info, code_info).
:- mode code_info__get_pred_id(out, in, out) is det.

		% Get the id of the procedure we are generating code for.
:- pred code_info__get_proc_id(proc_id, code_info, code_info).
:- mode code_info__get_proc_id(out, in, out) is det.

		% Get the HLDS of the procedure we are generating code for.
:- pred code_info__get_proc_info(proc_info, code_info, code_info).
:- mode code_info__get_proc_info(out, in, out) is det.

		% Get the flag that indicates whether succip is used or not.
:- pred code_info__get_succip_used(bool, code_info, code_info).
:- mode code_info__get_succip_used(out, in, out) is det.

		% Get the HLDS of the entire module.
:- pred code_info__get_module_info(module_info, code_info, code_info).
:- mode code_info__get_module_info(out, in, out) is det.

		% Get the set of currently forward-live variables.
:- pred code_info__get_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__get_liveness_info(out, in, out) is det.

		% Set the set of currently forward-live variables.
:- pred code_info__set_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__set_liveness_info(in, in, out) is det.

		% Get the table mapping variables to the current
		% instantiation states.
:- pred code_info__get_instmap(instmap, code_info, code_info).
:- mode code_info__get_instmap(out, in, out) is det.

		% Set the table mapping variables to the current
		% instantiation states.
:- pred code_info__set_instmap(instmap, code_info, code_info).
:- mode code_info__set_instmap(in, in, out) is det.

		% Get the globals table.
:- pred code_info__get_globals(globals, code_info, code_info).
:- mode code_info__get_globals(out, in, out) is det.

:- pred code_info__get_shapes(shape_table, code_info, code_info).
:- mode code_info__get_shapes(out, in, out) is det.

:- pred code_info__set_shapes(shape_table, code_info, code_info).
:- mode code_info__set_shapes(in, in, out) is det.

		% Get the table that contains advice about where
		% variables should be put.
:- pred code_info__get_follow_vars(follow_vars, code_info, code_info).
:- mode code_info__get_follow_vars(out, in, out) is det.

		% Set the table that contains advice about where
		% variables should be put.
:- pred code_info__set_follow_vars(follow_vars, code_info, code_info).
:- mode code_info__set_follow_vars(in, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred code_info__get_stackslot_count(int, code_info, code_info).
:- mode code_info__get_stackslot_count(out, in, out) is det.

:- pred code_info__get_label_count(int, code_info, code_info).
:- mode code_info__get_label_count(out, in, out) is det.

:- pred code_info__set_label_count(int, code_info, code_info).
:- mode code_info__set_label_count(in, in, out) is det.

:- pred code_info__get_cell_count(int, code_info, code_info).
:- mode code_info__get_cell_count(out, in, out) is det.

:- pred code_info__set_cell_count(int, code_info, code_info).
:- mode code_info__set_cell_count(in, in, out) is det.

:- pred code_info__get_exprn_info(exprn_info, code_info, code_info).
:- mode code_info__get_exprn_info(out, in, out) is det.

:- pred code_info__set_exprn_info(exprn_info, code_info, code_info).
:- mode code_info__set_exprn_info(in, in, out) is det.

:- pred code_info__set_succip_used(bool, code_info, code_info).
:- mode code_info__set_succip_used(in, in, out) is det.

:- pred code_info__get_fail_stack(fail_stack, code_info, code_info).
:- mode code_info__get_fail_stack(out, in, out) is det.

:- pred code_info__set_fail_stack(fail_stack, code_info, code_info).
:- mode code_info__set_fail_stack(in, in, out) is det.

:- pred code_info__get_push_count(int, code_info, code_info).
:- mode code_info__get_push_count(out, in, out) is det.

:- pred code_info__set_push_count(int, code_info, code_info).
:- mode code_info__set_push_count(in, in, out) is det.

:- pred code_info__get_max_push_count(int, code_info, code_info).
:- mode code_info__get_max_push_count(out, in, out) is det.

:- pred code_info__set_max_push_count(int, code_info, code_info).
:- mode code_info__set_max_push_count(in, in, out) is det.

:- pred code_info__get_pushed_values(stack(pair(lval, lval_or_ticket)),
	code_info, code_info).
:- mode code_info__get_pushed_values(out, in, out) is det.

:- pred code_info__set_pushed_values(stack(pair(lval, lval_or_ticket)),
	code_info, code_info).
:- mode code_info__set_pushed_values(in, in, out) is det.

:- pred code_info__get_zombies(set(var), code_info, code_info).
:- mode code_info__get_zombies(out, in, out) is det.

:- pred code_info__set_zombies(set(var), code_info, code_info).
:- mode code_info__set_zombies(in, in, out) is det.

:- pred code_info__get_resume_point_stack(stack(set(var)),
	code_info, code_info).
:- mode code_info__get_resume_point_stack(out, in, out) is det.

:- pred code_info__set_resume_point_stack(stack(set(var)),
	code_info, code_info).
:- mode code_info__set_resume_point_stack(in, in, out) is det.

:- pred code_info__get_commit_vals(list(pair(lval, lval_or_ticket)),
	code_info, code_info).
:- mode code_info__get_commit_vals(out, in, out) is det.

:- pred code_info__set_commit_vals(list(pair(lval, lval_or_ticket)),
	code_info, code_info).
:- mode code_info__set_commit_vals(in, in, out) is det.

%---------------------------------------------------------------------------%

:- type code_info	--->
		code_info(
			int,		% The number of stack slots allocated.
					% for storing variables.
					% (Some extra stack slots are used
					% for saving and restoring registers.)
			int,		% Counter for the local labels used
					% by this procedure.
			varset,		% The variables in this procedure.
			stack_slots,	% The map giving the stack slot
					% to be used for each variable
					% that will ever need to be stored
					% on the stack.
			pred_id,	% The label of the current predicate.
			proc_id,	% The label of the current procedure.
			int,		% Counter for cells in this proc.
			exprn_info,	% A map storing the information about
					% the status of each variable.
			proc_info,	% The proc_info for the this procedure.
			bool,		% do we need to store succip?
			fail_stack,	% The failure continuation stack
			module_info,	% The module_info structure - you just
					% never know when you might need it.
					% It should be read-only.
			liveness_info,	% Variables that are live
					% after this goal
			instmap,	% insts of variables
			int,		% The current number of extra
					% temporary stackslots that have been
					% pushed during the procedure
			int,		% The maximum number of extra
					% temporary stackslots that have been
					% pushed during the procedure
			globals,	% code generation options
			stack(pair(lval, lval_or_ticket)),
					% the locations in use on the stack
			shape_table,	% Table of shapes.
			follow_vars,	% Advisory information about where
					% each variable will be needed next.
			set(var),	% Zombie variables; variables that have
					% been killed but are protected by a
					% resume point.
			stack(set(var)),
					% Each resumption point has an
					% associated set of variables
					% whose values may be needed on
					% resumption at that point.
					% This field gives those variables
					% for a nested set of resumption
					% points. Each element must be
					% a superset of the ones below.
					% When a variable included in the top
					% set becomes no longer forward live,
					% we must save its value to the stack.
			list(pair(lval, lval_or_ticket))
					% A list of lvalues (ie curfr, maxfr
					% and redoip) that get saved onto the
					% det stack even though the current
					% context is nondet. We need to store
					% these for GC purposes.
	).

:- type lval_or_ticket  --->	ticket ; lval(lval).

%---------------------------------------------------------------------------%

code_info__init(Varset, Liveness, StackSlots, SaveSuccip, Globals,
		PredId, ProcId, ProcInfo, Requests, FollowVars,
		ModuleInfo, Shapes, C) :-
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_arg_info(ProcInfo, ArgInfos),
	assoc_list__from_corresponding_lists(HeadVars, ArgInfos, Args),
	code_info__build_input_arg_list(Args, ArgList),
	globals__get_options(Globals, Options),
	code_exprn__init_state(ArgList, Varset, Options, ExprnInfo),
	stack__init(Continue),
	stack__init(PushedVals0),
	stack__init(ResumeSetStack0),
	set__init(Zombies),
	code_info__max_slot(StackSlots, SlotCount0),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	(
		CodeModel = model_non
	->
		SlotCount is SlotCount0 + 1
	;
		SlotCount = SlotCount0
	),
	C = code_info(
		SlotCount,
		0,
		Varset,
		StackSlots,
		PredId,
		ProcId,
		0,
		ExprnInfo,
		ProcInfo,
		SaveSuccip,
		Continue,
		ModuleInfo,
		Liveness,
		Requests,
		0,
		0,
		Globals,
		PushedVals0,
		Shapes,
		FollowVars,
		Zombies,
		ResumeSetStack0,
		[]
	).

	% XXX This should be in arg_info.m.
:- pred code_info__build_input_arg_list(assoc_list(var, arg_info),
	assoc_list(var, rval)).
:- mode code_info__build_input_arg_list(in, out) is det.

code_info__build_input_arg_list([], []).
code_info__build_input_arg_list([V - Arg | Rest0], VarArgs) :-
	Arg = arg_info(Loc, Mode),
	(
		Mode = top_in
	->
		code_util__arg_loc_to_register(Loc, Reg),
		VarArgs = [V - lval(reg(Reg)) | VarArgs0]
	;
		VarArgs = VarArgs0
	),
	code_info__build_input_arg_list(Rest0, VarArgs0).

%---------------------------------------------------------------------------%

code_info__get_stackslot_count(A, CI, CI) :-
	CI = code_info(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_label_count(B, CI, CI) :-
	CI = code_info(_, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_varset(C, CI, CI) :-
	CI = code_info(_, _, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_stack_slots(D, CI, CI) :-
	CI = code_info(_, _, _, D, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_pred_id(E, CI, CI) :-
	CI = code_info(_, _, _, _, E, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_proc_id(F, CI, CI) :-
	CI = code_info(_, _, _, _, _, F, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_cell_count(G, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, G, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_exprn_info(H, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, H, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_proc_info(I, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, I, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_succip_used(J, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, J, _, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_fail_stack(K, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, K, _, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_module_info(L, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, L, _, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_liveness_info(M, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, M, _, _, _, _, _, _,
		_, _, _, _).

code_info__get_instmap(N, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, N, _, _, _, _, _,
		_, _, _, _).

code_info__get_push_count(O, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, O, _, _, _, _,
		_, _, _, _).

code_info__get_max_push_count(P, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, P, _, _, _,
		_, _, _, _).

code_info__get_globals(Q, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Q, _, _,
		_, _, _, _).

code_info__get_pushed_values(R, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, R, _,
		_, _, _, _).

code_info__get_shapes(S, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, S,
		_, _, _, _).

code_info__get_follow_vars(T, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		T, _, _, _).

code_info__get_zombies(U, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, U, _, _).

code_info__get_resume_point_stack(V, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, V, _).

code_info__get_commit_vals(W, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, W).

% :- type code_info	--->
% 		code_info(
% 	A		int,		% The number of stack slots allocated.
% 					% for storing variables.
% 					% (Some extra stack slots are used
% 					% for saving and restoring registers.)
% 	B		int,		% Counter for the local labels used
% 					% by this procedure.
% 	C		varset,		% The variables in this procedure.
% 	D		stack_slots,	% The map giving the stack slot
% 					% to be used for each variable
% 					% that will ever need to be stored
% 					% on the stack.
% 	E		pred_id,	% The label of the current predicate.
% 	F		proc_id,	% The label of the current procedure.
% 	G		int,		% Counter for cells in this proc.
% 	H		exprn_info,	% A map storing the information about
% 					% the status of each variable.
% 	I		proc_info,	% The proc_info for the this procedure.
% 	J		bool,		% do we need to store succip?
% 	K		fail_stack,	% The failure continuation stack
% 	L		module_info,	% The module_info structure - you just
% 					% never know when you might need it.
% 					% It should be read-only.
% 	M		liveness_info,	% Variables that are live
% 					% after this goal
% 	N		instmap,	% insts of variables
% 	O		int,		% The current number of extra
% 					% temporary stackslots that have been
% 					% pushed during the procedure
% 	P		int,		% The maximum number of extra
% 					% temporary stackslots that have been
% 					% pushed during the procedure
% 	Q		globals,	% code generation options
% 	R		stack(pair(lval, lval_or_ticket)),
% 					% the locations in use on the stack
% 	S		shape_table,	% Table of shapes.
% 	T		follow_vars,	% Advisory information about where
% 					% each variable will be needed next.
% 	U		set(var),	% Zombie variables; variables that have
% 					% been killed but are protected by a
% 					% resume point.
% 	V		stack(set(var)),
% 					% Each resumption point has an
% 					% associated set of variables
% 					% whose values may be needed on
% 					% resumption at that point.
% 					% This field gives those variables
% 					% for a nested set of resumption
% 					% points. Each element must be
% 					% a superset of the ones below.
% 					% When a variable included in the top
% 					% set becomes no longer forward live,
% 					% we must save its value to the stack.
% 	W		list(pair(lval, lval_or_ticket))
% 					% A list of lvalues (ie curfr, maxfr
% 					% and redoip) that get saved onto the
% 					% det stack even though the current
% 					% context is nondet. We need to store
% 					% these for GC purposes.
% 	).

% we don't need
% code_info__set_stackslot_count
% code_info__set_varset
% code_info__set_pred_id
% code_info__set_proc_id
% code_info__set_stack_slots
% code_info__set_module_info
% code_info__set_globals

code_info__set_label_count(B, CI0, CI) :-
	CI0 = code_info(A, _, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_cell_count(G, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, _, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_exprn_info(H, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, _, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_succip_used(J, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, P, Q,
		R, S, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_fail_stack(K, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, P, Q,
		R, S, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_liveness_info(M, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, P, Q,
		R, S, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_instmap(N, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, _, O, P, Q,
		R, S, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_push_count(O, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, _, P0, Q,
		R, S, T, U, V, W),
	int__max(P0, O, P),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_max_push_count(P, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, _, Q,
		R, S, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_pushed_values(R, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		_, S, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_shapes(S, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, _, T, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_follow_vars(T, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, _, U, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_zombies(U, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, _, V, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_resume_point_stack(V, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, _, W),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

code_info__set_commit_vals(W, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, _),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V, W).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for simple wrappers around access predicates.

:- interface.

	% code_info__pre_goal_update(GoalInfo, Atomic, OldCodeInfo, NewCodeInfo)
	% updates OldCodeInfo to produce NewCodeInfo with the changes
	% specified by GoalInfo.
:- pred code_info__pre_goal_update(hlds__goal_info, bool, code_info, code_info).
:- mode code_info__pre_goal_update(in, in, in, out) is det.

	% code_info__post_goal_update(GoalInfo, OldCodeInfo, NewCodeInfo)
	% updates OldCodeInfo to produce NewCodeInfo with the changes described
	% by GoalInfo.
:- pred code_info__post_goal_update(hlds__goal_info, code_info, code_info).
:- mode code_info__post_goal_update(in, in, out) is det.

	% Find out the type of the given variable.

:- pred code_info__variable_type(var, type, code_info, code_info).
:- mode code_info__variable_type(in, out, in, out) is det.

:- pred code_info__lookup_type_defn(type, hlds__type_defn,
	code_info, code_info).
:- mode code_info__lookup_type_defn(in, out, in, out) is det.

	% Given a list of type variables, find the lvals where the
	% corresponding type_infos are being stored.

:- pred code_info__find_type_infos(list(var), list(lval), code_info, code_info).
:- mode code_info__find_type_infos(in, out, in, out) is det.

	% Given a constructor id, and a variable (so that we can work out the
	% type of the constructor), determine correct tag (representation)
	% of that constructor.

:- pred code_info__cons_id_to_tag(var, cons_id, cons_tag, code_info, code_info).
:- mode code_info__cons_id_to_tag(in, in, out, in, out) is det.

:- pred code_info__get_headvars(list(var), code_info, code_info).
:- mode code_info__get_headvars(out, in, out) is det.

	% Get the call argument information for the current procedure
:- pred code_info__get_arginfo(list(arg_info), code_info, code_info).
:- mode code_info__get_arginfo(out, in, out) is det.

	% Get the call argument info for a given mode of a given predicate
:- pred code_info__get_pred_proc_arginfo(pred_id, proc_id, list(arg_info),
	code_info, code_info).
:- mode code_info__get_pred_proc_arginfo(in, in, out, in, out) is det.

	% Pop the failure continuation stack.

:- pred code_info__pop_failure_cont(code_info, code_info).
:- mode code_info__pop_failure_cont(in, out) is det.

:- pred code_info__push_resume_point_vars(set(var), code_info, code_info).
:- mode code_info__push_resume_point_vars(in, in, out) is det.

:- pred code_info__pop_resume_point_vars(code_info, code_info).
:- mode code_info__pop_resume_point_vars(in, out) is det.

:- pred code_info__variable_to_string(var, string, code_info, code_info).
:- mode code_info__variable_to_string(in, out, in, out) is det.

:- pred code_info__grab_code_info(code_info, code_info, code_info).
:- mode code_info__grab_code_info(out, in, out) is det.

:- pred code_info__slap_code_info(code_info, code_info, code_info).
:- mode code_info__slap_code_info(in, in, out) is det.

:- pred code_info__apply_instmap_delta(instmap_delta, code_info, code_info).
:- mode code_info__apply_instmap_delta(in, in, out) is det.

	% Create a code address which holds the address of the specified
	% procedure.
	% The fourth argument should be `no' if the the caller wants the
	% returned address to be valid from everywhere in the program.
	% If being valid from within the current procedure is enough,
	% this argument should be `yes' wrapped around the value of the
	% --procs-per-c-function option and the current procedure id.
	% Using an address that is only valid from within the current
	% procedure may make jumps more efficient.
	% XXX
:- pred code_info__make_entry_label(module_info, pred_id, proc_id, bool,
	code_addr, code_info, code_info).
:- mode code_info__make_entry_label(in, in, in, in, out, in, out) is det.

	% Generate the next local label in sequence.
:- pred code_info__get_next_label(label, code_info, code_info).
:- mode code_info__get_next_label(out, in, out) is det.

	% Generate the next local cell number in sequence.
:- pred code_info__get_next_cell_number(int, code_info, code_info).
:- mode code_info__get_next_cell_number(out, in, out) is det.

:- pred code_info__succip_is_used(code_info, code_info).
:- mode code_info__succip_is_used(in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred code_info__get_proc_model(code_model, code_info, code_info).
:- mode code_info__get_proc_model(out, in, out) is det.

:- pred code_info__push_failure_cont(failure_cont, code_info, code_info).
:- mode code_info__push_failure_cont(in, in, out) is det.

	% Lookup the value on the top of the failure continuation stack

:- pred code_info__top_failure_cont(failure_cont, code_info, code_info).
:- mode code_info__top_failure_cont(out, in, out) is det.

:- pred code_info__current_resume_point_vars(set(var), code_info, code_info).
:- mode code_info__current_resume_point_vars(out, in, out) is det.

:- pred code_info__add_commit_val(lval, lval, code_info, code_info).
:- mode code_info__add_commit_val(in, in, in, out) is det.

:- pred code_info__rem_commit_val(code_info, code_info).
:- mode code_info__rem_commit_val(in, out) is det.

%-----------------------------------------------------------------------------%

	% Update the code info structure to be consistent
	% immediately prior to generating a goal
code_info__pre_goal_update(GoalInfo, Atomic) -->
	% The liveness pass puts resume_point annotations on some kinds
	% of goals. The parts of the code generator that handle those kinds
	% of goals should handle the resume point annotation as well;
	% when they do, they remove the annotation. The following code
	% is a sanity check to make sure that this has in fact been done.
	{ goal_info_get_resume_point(GoalInfo, ResumePoint) },
	(
		{ ResumePoint = no_resume_point }
	;
		{ ResumePoint = resume_point(_, _) },
		{ error("pre_goal_update with resume point") }
	),
	{ goal_info_get_follow_vars(GoalInfo, MaybeFollowVars) },
	(
		{ MaybeFollowVars = yes(FollowVars) },
		code_info__set_follow_vars(FollowVars)
	;
		{ MaybeFollowVars = no }
	),
	{ goal_info_get_pre_births(GoalInfo, PreBirths) },
	{ goal_info_get_pre_deaths(GoalInfo, PreDeaths) },
	code_info__update_liveness_info(PreBirths),
	code_info__update_deadness_info(PreDeaths),
	code_info__make_vars_dead(PreDeaths),
	( { Atomic = yes } ->
		{ goal_info_get_post_deaths(GoalInfo, PostDeaths) },
		code_info__update_deadness_info(PostDeaths)
	;
		[]
	).

	% Update the code info structure to be consistent
	% immediately after generating a goal
code_info__post_goal_update(GoalInfo) -->
	{ goal_info_get_post_births(GoalInfo, PostBirths) },
	{ goal_info_get_post_deaths(GoalInfo, PostDeaths) },
	code_info__update_liveness_info(PostBirths),
	code_info__update_deadness_info(PostDeaths),
	code_info__make_vars_dead(PostDeaths),
	code_info__make_vars_live(PostBirths),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	code_info__apply_instmap_delta(InstMapDelta).

%---------------------------------------------------------------------------%

code_info__variable_type(Var, Type) -->
	code_info__get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) }.

code_info__lookup_type_defn(Type, TypeDefn) -->
	code_info__get_module_info(ModuleInfo),
	{ type_to_type_id(Type, TypeIdPrime, _) ->
		TypeId = TypeIdPrime
	;
		error("unknown type in code_aux__lookup_type_defn")
	},
	{ module_info_types(ModuleInfo, TypeTable) },
	{ map__lookup(TypeTable, TypeId, TypeDefn) }.

code_info__find_type_infos([], []) --> [].
code_info__find_type_infos([TVar | TVars], [Lval | Lvals]) -->
	code_info__get_proc_info(ProcInfo),
	{ proc_info_typeinfo_varmap(ProcInfo, TypeInfoMap) },
	(
		{ map__search(TypeInfoMap, TVar, Var0) }
	->
		{ Var = Var0 }
	;
		{ error("cannot find var for type variable") }
	),
	{ proc_info_stack_slots(ProcInfo, StackSlots) },
	(
		{ map__search(StackSlots, Var, Lval0) }
	->
		{ Lval = Lval0 }
	;
		code_info__variable_to_string(Var, VarString),
		{ string__format("code_info__find_type_infos: can't find lval for type_info var %s",
			[s(VarString)], ErrStr) },
		{ error(ErrStr) }
	),
	code_info__find_type_infos(TVars, Lvals).

code_info__cons_id_to_tag(Var, ConsId, ConsTag) -->
	code_info__variable_type(Var, Type),
	code_info__get_module_info(ModuleInfo),
	{ code_util__cons_id_to_tag(ConsId, Type, ModuleInfo, ConsTag) }.

%---------------------------------------------------------------------------%

code_info__get_proc_model(CodeModel) -->
	code_info__get_proc_info(ProcInfo),
	{ proc_info_interface_code_model(ProcInfo, CodeModel) }.

code_info__get_headvars(HeadVars) -->
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_headvars(ProcInfo, HeadVars) }.

code_info__get_arginfo(ArgInfo) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo).

code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo) -->
	code_info__get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo) },
	{ proc_info_arg_info(ProcInfo, ArgInfo) }.

%---------------------------------------------------------------------------%

code_info__push_failure_cont(Cont) -->
	code_info__get_fail_stack(Fall0),
	{ stack__push(Fall0, Cont, Fall) },
	code_info__set_fail_stack(Fall).

code_info__pop_failure_cont -->
	code_info__get_fail_stack(Fall0),
	( { stack__pop(Fall0, _, Fall) } ->
		code_info__set_fail_stack(Fall)
	;
		{ error("code_info__pop_failure_cont: empty stack") }
	).

code_info__top_failure_cont(Cont) -->
	code_info__get_fail_stack(Fall),
	( { stack__top(Fall, Cont0) } ->
		{ Cont = Cont0 }
	;
		{ error("code_info__failure_cont: no failure continuation") }
	).

%---------------------------------------------------------------------------%

code_info__current_resume_point_vars(ResumeVars) -->
	code_info__get_resume_point_stack(ResumeStack),
	{ stack__top(ResumeStack, TopVars) ->
		ResumeVars = TopVars
	;
		set__init(ResumeVars)
	}.

code_info__push_resume_point_vars(ResumeVars) -->
	code_info__get_resume_point_stack(ResumeStack0),
	{ stack__top(ResumeStack0, OldTopVars) ->
		require(set__subset(OldTopVars, ResumeVars),
		"new resume point variable set does not include old one")
	;
		true
	},
	{ stack__push(ResumeStack0, ResumeVars, ResumeStack) },
	code_info__set_resume_point_stack(ResumeStack).

code_info__pop_resume_point_vars -->
	code_info__get_resume_point_stack(ResumeStack0),
	{ stack__pop_det(ResumeStack0, _, ResumeStack) },
	code_info__set_resume_point_stack(ResumeStack).

%---------------------------------------------------------------------------%

code_info__add_commit_val(Item, StackVar) -->
	code_info__get_commit_vals(Stack0),
	code_info__set_commit_vals([StackVar - lval(Item) | Stack0]).

code_info__rem_commit_val -->
	code_info__get_commit_vals(Stack0),
	(
		{ Stack0 = [_ | Stack] },
		code_info__set_commit_vals(Stack)
	;
		{ Stack0 = [] },
		{ error("code_info__rem_commit_val: Empty list") }
	).

%---------------------------------------------------------------------------%

code_info__variable_to_string(Var, Name) -->
	code_info__get_varset(Varset),
	{ varset__lookup_name(Varset, Var, Name) }.

code_info__grab_code_info(C, C, C).

code_info__slap_code_info(C0, C1, C) :-
	code_info__get_label_count(L, C1, _),
	code_info__set_label_count(L, C0, C2),
	code_info__get_succip_used(S, C1, _),
	code_info__set_succip_used(S, C2, C3),
	code_info__get_fail_stack(J, C0, _),
	code_info__set_fail_stack(J, C3, C4),
	code_info__get_max_push_count(PC, C1, _),
	code_info__set_max_push_count(PC, C4, C5),
	code_info__get_shapes(Shapes, C1, _),
	code_info__set_shapes(Shapes, C5, C).

code_info__apply_instmap_delta(Delta) -->
	code_info__get_instmap(InstMap0),
	{ instmap__apply_instmap_delta(InstMap0, Delta, InstMap) },
	code_info__set_instmap(InstMap).

%---------------------------------------------------------------------------%

code_info__make_entry_label(ModuleInfo, PredId, ProcId, Immed0, PredAddress) -->
	(
		{ Immed0 = no },
		{ Immed = no }
	;
		{ Immed0 = yes },
		code_info__get_globals(Globals),
		{ globals__lookup_int_option(Globals, procs_per_c_function,
			ProcsPerFunc) },
		code_info__get_pred_id(CurPredId),
		code_info__get_proc_id(CurProcId),
		{ Immed = yes(ProcsPerFunc - proc(CurPredId, CurProcId)) }
	),
	{ code_util__make_entry_label(ModuleInfo, PredId, ProcId, Immed,
		PredAddress) }.

code_info__get_next_label(Label) -->
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_label_count(N0),
	{ N is N0 + 1 },
	code_info__set_label_count(N),
	{ code_util__make_internal_label(ModuleInfo, PredId, ProcId, N,
		Label) }.

code_info__get_next_cell_number(N) -->
	code_info__get_cell_count(N0),
	{ N is N0 + 1 },
	code_info__set_cell_count(N).

code_info__succip_is_used -->
	code_info__set_succip_used(yes).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for the handling of failure continuations.

:- interface.

	% We manufacture a failure cont when we start generating
	% code for a proc because on the failure of a procedure
	% we don't need to prepare for execution to resume in this
	% procedure.

:- pred code_info__manufacture_failure_cont(bool, code_info, code_info).
:- mode code_info__manufacture_failure_cont(in, in, out) is det.

	% Push a new failure continuation onto the stack.

:- pred code_info__make_known_failure_cont(set(var), resume_locs, bool,
	bool, bool, code_tree, code_info, code_info).
:- mode code_info__make_known_failure_cont(in, in, in, in, out, out, in, out)
	is det.

	% Generate some code to restore the current redoip, by looking
	% at the top of the failure continuation stack.

:- pred code_info__restore_failure_cont(code_tree, code_info, code_info).
:- mode code_info__restore_failure_cont(out, in, out) is det.

	% XXX

:- pred code_info__failure_is_direct_branch(code_addr, code_info, code_info).
:- mode code_info__failure_is_direct_branch(out, in, out) is semidet.

	% XXX

:- pred code_info__generate_failure(code_tree, code_info, code_info).
:- mode code_info__generate_failure(out, in, out) is det.

	% XXX

:- pred code_info__fail_if_rval_is_false(rval, code_tree,
	code_info, code_info).
:- mode code_info__fail_if_rval_is_false(in, out, in, out) is det.

	% Set the topmost failure cont to `unknown' (e.g. after
	% a nondet call or after a disjunction).

:- pred code_info__unset_failure_cont(code_tree, code_info, code_info).
:- mode code_info__unset_failure_cont(out, in, out) is det.

	% Flush the variables needed for any current resumption point
	% to their stack slots.

:- pred code_info__flush_resume_vars_to_stack(code_tree, code_info, code_info).
:- mode code_info__flush_resume_vars_to_stack(out, in, out) is det.

	% XXX

:- pred code_info__may_use_nondet_tailcall(bool, code_info, code_info).
:- mode code_info__may_use_nondet_tailcall(out, in, out) is det.

	% do_soft_cut takes the lval where the address of the
	% failure frame is stored, and it returns code to
	% set the redoip of that frame to do_fail.

:- pred code_info__do_soft_cut(lval, code_tree, code_info, code_info).
:- mode code_info__do_soft_cut(in, out, in, out) is det.

	% `semi_pre_commit' and `semi_commit' should be generated as a pair
	% surrounding a nondet goal.
	%
	% `generate_semi_pre_commit' returns a label (a failure cont label)
	% which should be passed to `generate_semi_commit'.
	% If the goal succeeds, the `commit' will cut any choice points
	% generated in the goal.

:- pred code_info__generate_semi_pre_commit(label, code_tree,
	code_info, code_info).
:- mode code_info__generate_semi_pre_commit(out, out, in, out) is det.

:- pred code_info__generate_semi_commit(label, code_tree, code_info, code_info).
:- mode code_info__generate_semi_commit(in, out, in, out) is det.

	% `det_pre_commit' and `det_commit' should be generated as a pair
	% surrounding a multidet goal.
	% After the goal succeeds, the `commit' will cut any choice points
	% generated in the goal.

:- pred code_info__generate_det_pre_commit(code_tree, code_info, code_info).
:- mode code_info__generate_det_pre_commit(out, in, out) is det.

:- pred code_info__generate_det_commit(code_tree, code_info, code_info).
:- mode code_info__generate_det_commit(out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- type fail_stack	==	stack(failure_cont).

:- type failure_cont
	--->	failure_cont(
			failure_cont_info,
			resume_maps
		).

:- type failure_cont_info
	--->	semidet
	;	nondet(
			is_known,	% Says whether on failure we can branch
					% directly to one of the labels in
					% the resume_maps, or if we must
					% instead execute a redo.

			maybe(label)	% The maybe(label) is yes if we have
					% created a temporary frame and we
					% must restore curfr after a redo().
		).

:- type is_known	--->	known ; unknown.

:- type resume_map	==	map(var, set(rval)).

:- type resume_maps
	--->	orig_only(resume_map, code_addr)
	;	stack_only(resume_map, code_addr)
	;	orig_and_stack(resume_map, code_addr, resume_map, code_addr)
	;	stack_and_orig(resume_map, code_addr, resume_map, code_addr).

:- pred code_info__fail_cont_is_known(failure_cont_info).
:- mode code_info__fail_cont_is_known(in) is semidet.

code_info__fail_cont_is_known(FailContInfo) :-
	FailContInfo \= nondet(unknown, _).

:- pred code_info__fail_cont_is_unknown(failure_cont_info).
:- mode code_info__fail_cont_is_unknown(in) is semidet.

code_info__fail_cont_is_unknown(FailContInfo) :-
	FailContInfo = nondet(unknown, _).

%---------------------------------------------------------------------------%

code_info__manufacture_failure_cont(IsNondet) -->
	{ map__init(Empty) },
	(
		{ IsNondet = no },
		code_info__get_next_label(ContLab1),
		{ Address1 = label(ContLab1) },
		{ ResumeMap = stack_only(Empty, Address1) },
		{ ContInfo = semidet }
	;
		{ IsNondet = yes },
		{ Address1 = do_fail },
		{ ResumeMap = stack_only(Empty, Address1) },
		{ ContInfo = nondet(known, no) }
	),
	code_info__push_failure_cont(failure_cont(ContInfo, ResumeMap)).

%---------------------------------------------------------------------------%

code_info__make_known_failure_cont(ResumeVars, ResumeLocs, IsNondet,
		HaveTempFrame0, HaveTempFrame, ModContCode) -->
	code_info__get_next_label(OrigLabel),
	code_info__get_next_label(StackLabel),
	{ OrigAddr = label(OrigLabel) },
	{ StackAddr = label(StackLabel) },
	(
		% In semidet continuations we don't use the redoip
		% of the top stack frame.

		{ IsNondet = no },
		{ TempFrameCode = empty },
		{ HaveTempFrame = HaveTempFrame0 },
		{ FailContInfo = semidet }
	;
		% In nondet continuations we may use the redoip
		% of the top stack frame. Therefore we must ensure
		% that this redoip is free for use, creating our own
		% frame if necessary.

		{ IsNondet = yes },
		code_info__top_failure_cont(FailureCont),
		{ FailureCont = failure_cont(OrigInfo, _) },
		(
			{ code_info__fail_cont_is_unknown(OrigInfo) }
		->
			code_info__get_next_label(RedoLabel),
			{ MaybeRedoLabel = yes(RedoLabel) },
			{ RedoAddr = label(RedoLabel) },
			(
				{ HaveTempFrame0 = no }
			->
					% this code could be better
					% (mkframe is a bit of a sledge hammer)
				{ TempFrameCode = node([
					mkframe("temp frame", 1, RedoAddr)
						- "create a temporary frame",
					assign(curfr, lval(succfr(lval(maxfr))))
						- "restore curfr after mkframe"
				]) }
			;
				{ TempFrameCode = node([
					assign(redoip(lval(maxfr)),
						const(code_addr_const(RedoAddr)))
						- "Set failure continuation"
				]) }
			),
			{ HaveTempFrame = yes }
		;
			{ MaybeRedoLabel = no },
			{ TempFrameCode = node([
				assign(redoip(lval(maxfr)),
					const(code_addr_const(StackAddr))) -
					"Set failure continuation"
			]) },
			{ HaveTempFrame = HaveTempFrame0 }
		),
		{ FailContInfo = nondet(known, MaybeRedoLabel) }
	),
	{ set__to_sorted_list(ResumeVars, VarList) },
	(
		{ ResumeLocs = orig_only },
		code_info__produce_resume_vars(VarList, OrigMap, OrigCode),
		{ ResumeMaps = orig_only(OrigMap, OrigAddr) }
	;
		{ ResumeLocs = stack_only },
		code_info__produce_resume_vars(VarList, _OrigMap, OrigCode),
		code_info__get_stack_slots(StackSlots),
		{ map__select(StackSlots, ResumeVars, StackMap0) },
		{ map__to_assoc_list(StackMap0, StackList0) },
		{ code_info__tweak_stacklist(StackList0, StackList) },
		{ map__from_assoc_list(StackList, StackMap) },
		{ ResumeMaps = stack_only(StackMap, StackAddr) }
	;
		{ ResumeLocs = orig_and_stack },
		code_info__produce_resume_vars(VarList, OrigMap, OrigCode),
		code_info__get_stack_slots(StackSlots),
		{ map__select(StackSlots, ResumeVars, StackMap0) },
		{ map__to_assoc_list(StackMap0, StackList0) },
		{ code_info__tweak_stacklist(StackList0, StackList) },
		{ map__from_assoc_list(StackList, StackMap) },
		{ ResumeMaps = orig_and_stack(OrigMap, OrigAddr,
			StackMap, StackAddr) }
	;
		{ ResumeLocs = stack_and_orig },
		code_info__produce_resume_vars(VarList, OrigMap, OrigCode),
		code_info__get_stack_slots(StackSlots),
		{ map__select(StackSlots, ResumeVars, StackMap0) },
		{ map__to_assoc_list(StackMap0, StackList0) },
		{ code_info__tweak_stacklist(StackList0, StackList) },
		{ map__from_assoc_list(StackList, StackMap) },
		{ ResumeMaps = stack_and_orig(StackMap, StackAddr,
			OrigMap, OrigAddr) }
	),
	code_info__push_failure_cont(failure_cont(FailContInfo, ResumeMaps)),
	{ ModContCode = tree(OrigCode, TempFrameCode) }.

:- pred code_info__produce_resume_vars(list(var), map(var, set(rval)),
	code_tree, code_info, code_info).
:- mode code_info__produce_resume_vars(in, out, out, in, out) is det.

code_info__produce_resume_vars([], Map, empty) -->
	{ map__init(Map) }.
code_info__produce_resume_vars([V | Vs], Map, Code) -->
	code_info__produce_resume_vars(Vs, Map0, Code0),
		% XXX should use follow_vars to decide whether to put into reg
	code_info__produce_variable_in_reg_or_stack(V, Code1, Rval),
	{ set__singleton_set(Rvals, Rval) },
	{ map__set(Map0, V, Rvals, Map) },
	{ Code = tree(Code0, Code1) }.

:- pred code_info__tweak_stacklist(assoc_list(var, lval),
	assoc_list(var, set(rval))).
:- mode code_info__tweak_stacklist(in, out) is det.

code_info__tweak_stacklist([], []).
code_info__tweak_stacklist([V - L | Rest0], [V - Rs | Rest]) :-
	set__singleton_set(Rs, lval(L)),
	code_info__tweak_stacklist(Rest0, Rest).

%---------------------------------------------------------------------------%

% :- pred code_info__modify_failure_cont(code_tree, code_info, code_info).
% :- mode code_info__modify_failure_cont(out, in, out) is det.
% 
% code_info__modify_failure_cont(ModifyCode) -->
% 	code_info__top_failure_cont(FailureCont),
% 	{ FailureCont = failure_cont(OldCont, MaybeRedo0, FailureMap) },
% 	code_info__generate_failure_cont(FailureCont, FailureCode),
% 	code_info__pop_failure_cont,
% 	code_info__get_next_label(NewRegCont),
% 	code_info__get_next_label(NewStackCont),
% 	(
% 		{ OldCont = unknown ; OldCont = known(yes) }
% 	->
% 		{ NewCont = known(yes) },
% 		( { MaybeRedo0 = yes(_OldRedo) } ->
% 			code_info__get_next_label(NewRedoCont),
% 			{ MaybeRedo = yes(NewRedoCont) }
% 		;
% 			{ NewRedoCont = NewStackCont },
% 			{ MaybeRedo = no }
% 		),
% 		{ ResetCode = node([
% 			assign(redoip(lval(maxfr)),
% 				const(code_addr_const(label(NewRedoCont)))) -
% 				"modify failure cont"
% 		]) }
% 	;
% 		{ error("code_info__modify_failure_cont: semidet context") }
% 		% { NewCont = known(no) },
% 		% { ResetCode = empty },
% 		% { MaybeRedo = no }
% 	),
% 	(
% 		{ FailureMap = [RegMap - _RegCont, StackMap - _StackCont] }
% 	->
% 		code_info__push_failure_cont(failure_cont(NewCont, MaybeRedo,
% 			[RegMap - label(NewRegCont),
% 			StackMap - label(NewStackCont)]))
% 	;
% 		{ error("code_info__modify_failure_cont: bad failure map.") }
% 	),
% 	{ ModifyCode = tree(FailureCode, ResetCode) }.

%---------------------------------------------------------------------------%

code_info__restore_failure_cont(Code) -->
	code_info__top_failure_cont(CurFailureCont),
	{ CurFailureCont = failure_cont(CurContInfo, _FailureMap) },
	code_info__generate_failure_cont(CurFailureCont, FailureCode),
	code_info__pop_failure_cont,
		% Fixup the redoip of the top frame if necessary
	(
		{ CurContInfo = semidet },
		{ ResetCode = empty }
	;
		{ CurContInfo = nondet(_, _) },
		code_info__top_failure_cont(EnclosingFailureCont),
		{ EnclosingFailureCont = failure_cont(EnclosingContInfo, _) },
		{
			EnclosingContInfo = semidet,
			ResetCode = empty
		;
			EnclosingContInfo = nondet(unknown, _),
			ResetCode = node([
				assign(redoip(lval(maxfr)),
					const(code_addr_const(do_fail))) -
					"restore failure cont"
			])
		;
			EnclosingContInfo = nondet(known, _),
			code_info__find_first_resume_label(
				EnclosingFailureCont, RedoAddress),
			ResetCode = node([
				assign(redoip(lval(maxfr)),
					const(code_addr_const(RedoAddress)))
					- "restore failure cont"
			])
		}
	),
	{ Code = tree(FailureCode, ResetCode) }.

%---------------------------------------------------------------------------%

	% In establishing this resumption point, we may have pushed
	% a temporary frame onto the nondet stack. If we have done so,
	% we will have recorded this fact by setting the second argument
	% of the top failure continuation to yes, with the argument of
	% the yes giving the name of the label whose address was put
	% into the redoip slot of that frame.
	%
	% When control arrives at that label, curfr will point to the
	% temporary frame. However, the variables needed by the code
	% at the resumption point are in the main nondet frame of the
	% procedure. Since this was the current frame when we created
	% the temporary frame, we can find it by following the succfr
	% link in the temporary frame.
	%
	% The code we generate in general is
	%
	% label(RedoLabel)
	% <reset curfr>
	% label(StackLabel)
	% <assume variables are where StackMap says they are>
	% <copy variables to their locations according to OrigMap>
	% label(OrigLabel)
	% <assume variables are where OrigMap says they are>
	%
	% If, in establishing this resumption point, we did not create
	% a temporary frame, then curfr will be OK when code to the right
	% does a fail(), and hence the first label and the resetting of
	% the curfr register can be omitted.
	%
	% Failures at different points may cause control to arrive at
	% the resumption point via any one of these each labels.
	% The last line above is necessary since it may arrive at OrigLabel
	% without going through StackLabel first.
	%
	% The first two lines above are generated in this predicate;
	% the others are generated in code_info__generate_resume_setup.
	% It may not generate some of these other lines if it knows that
	% they won't be needed.

:- pred code_info__generate_failure_cont(failure_cont, code_tree,
	code_info, code_info).
:- mode code_info__generate_failure_cont(in, out, in, out) is det.

code_info__generate_failure_cont(FailureCont, Code) -->
	{ FailureCont = failure_cont(ContInfo, ResumeMap) },

		% Did we create a temp nondet frame for this continuation?
		% If not, then curfr will be right when we arrive here.
		% If yes, it will be wrong, pointing to the temp frame,
		% so we must restore curfr before continuing.
	{
		ContInfo = nondet(_, MaybeRedoLabel),
		MaybeRedoLabel = yes(RedoLabel)
	->
		FixCurFrCode = node([
			label(RedoLabel) -
				"redo entry point",
			assign(curfr, lval(succfr(lval(maxfr)))) -
				"restore curfr"
		]),
		(
			( ResumeMap = orig_only(_, _)
			; ResumeMap = orig_and_stack(_, _, _, _)
			)
		->
			error("redo entry before an orig resume point")
		;
			true
		)
	;
		FixCurFrCode = empty
	},
	code_info__generate_resume_setup(ResumeMap, ResumeCode),
	{ Code = tree(FixCurFrCode, ResumeCode) }.

:- pred code_info__generate_resume_setup(resume_maps, code_tree,
	code_info, code_info).
:- mode code_info__generate_resume_setup(in, out, in, out) is det.

code_info__generate_resume_setup(ResumeMaps, Code) -->
	(
		{ ResumeMaps = orig_only(Map1, Addr1) },
		{ extract_label_from_code_addr(Addr1, Label1) },
		{ Code = node([
			label(Label1) -
				"orig only failure continuation"
		]) },
		code_info__set_var_locations(Map1)
	;
		{ ResumeMaps = stack_only(Map1, Addr1) },
		{ extract_label_from_code_addr(Addr1, Label1) },
		{ Code = node([
			label(Label1) -
				"stack only failure continuation"
		]) },
		code_info__set_var_locations(Map1)
	;
		{ ResumeMaps = stack_and_orig(Map1, Addr1, Map2, Addr2) },
		{ extract_label_from_code_addr(Addr1, Label1) },
		{ extract_label_from_code_addr(Addr2, Label2) },
		{ Label1Code = node([
			label(Label1) -
				"stack failure continuation before orig"
		]) },
		code_info__set_var_locations(Map1),
		{ map__to_assoc_list(Map2, AssocList2) },
		code_info__place_resume_vars(AssocList2, PlaceCode),
		{ Label2Code = node([
			label(Label2) -
				"orig failure continuation after stack"
		]) },
		code_info__set_var_locations(Map2),
		{ Code = tree(Label1Code, tree(PlaceCode, Label2Code)) }
	;
		{ ResumeMaps = orig_and_stack(Map1, Addr1, Map2, Addr2) },
		{ extract_label_from_code_addr(Addr1, Label1) },
		{ extract_label_from_code_addr(Addr2, Label2) },
		{ Label1Code = node([
			label(Label1) -
				"orig failure continuation before stack"
		]) },
		code_info__set_var_locations(Map1),
		{ map__to_assoc_list(Map2, AssocList2) },
		code_info__place_resume_vars(AssocList2, PlaceCode),
		{ Label2Code = node([
			label(Label2) -
				"stack failure continuation after orig"
		]) },
		code_info__set_var_locations(Map2),
		{ Code = tree(Label1Code, tree(PlaceCode, Label2Code)) }
	).

:- pred extract_label_from_code_addr(code_addr, label).
:- mode extract_label_from_code_addr(in, out) is det.

extract_label_from_code_addr(CodeAddr, Label) :-
	( CodeAddr = label(Label0) ->
		Label = Label0
	;
		error("code_info__generate_resume_setup: non-label!")
	).

:- pred code_info__place_resume_vars(assoc_list(var, set(rval)), code_tree,
	code_info, code_info).
:- mode code_info__place_resume_vars(in, out, in, out) is det.

code_info__place_resume_vars([], empty) --> [].
code_info__place_resume_vars([Var - TargetSet | Rest], Code) -->
	{ set__to_sorted_list(TargetSet, Targets) },
	code_info__place_resume_var(Var, Targets, FirstCode),
	{ Code = tree(FirstCode, RestCode) },
	code_info__place_resume_vars(Rest, RestCode).

:- pred code_info__place_resume_var(var, list(rval), code_tree,
	code_info, code_info).
:- mode code_info__place_resume_var(in, in, out, in, out) is det.

code_info__place_resume_var(_Var, [], empty) --> [].
code_info__place_resume_var(Var, [Target | Targets], Code) -->
	( { Target = lval(TargetLval) } ->
		code_info__place_var(Var, TargetLval, FirstCode)
	;
		{ error("code_info__place_resume_var: not lval") }
	),
	{ Code = tree(FirstCode, RestCode) },
	code_info__place_resume_var(Var, Targets, RestCode).

	% Reset the code generator's database of what is where.
	% Remember that the variables in the map are available in their
	% associated rvals; forget about all other variables.

:- pred code_info__set_var_locations(map(var, set(rval)), code_info, code_info).
:- mode code_info__set_var_locations(in, in, out) is det.

code_info__set_var_locations(Map) -->
	{ map__to_assoc_list(Map, List0) },
	{ code_info__flatten_varlval_list(List0, List) },
	code_info__get_varset(Varset),
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ code_exprn__init_state(List, Varset, Options, Exprn) },
	code_info__set_exprn_info(Exprn).

:- pred code_info__flatten_varlval_list(assoc_list(var, set(rval)),
						assoc_list(var, rval)).
:- mode code_info__flatten_varlval_list(in, out) is det.

code_info__flatten_varlval_list([], []).
code_info__flatten_varlval_list([V - Rvals | Rest0], All) :-
	code_info__flatten_varlval_list(Rest0, Rest),
	set__to_sorted_list(Rvals, RvalList),
	code_info__flatten_varlval_list_2(RvalList, V, Rest1),
	list__append(Rest1, Rest, All).

:- pred code_info__flatten_varlval_list_2(list(rval), var,
	assoc_list(var, rval)).
:- mode code_info__flatten_varlval_list_2(in, in, out) is det.

code_info__flatten_varlval_list_2([], _V, []).
code_info__flatten_varlval_list_2([R | Rs], V, [V - R | Rest]) :-
	code_info__flatten_varlval_list_2(Rs, V, Rest).

%---------------------------------------------------------------------------%

code_info__failure_is_direct_branch(CodeAddr) -->
	code_info__top_failure_cont(FailureCont),
	{ FailureCont = failure_cont(ContInfo, FailureMap) },
	{ code_info__fail_cont_is_known(ContInfo) },
	code_info__pick_matching_resume_addr(FailureMap, CodeAddr).

code_info__generate_failure(Code) -->
	code_info__top_failure_cont(FailureCont),
	{ FailureCont = failure_cont(ContInfo, FailureMap) },
	(
		{ code_info__fail_cont_is_known(ContInfo) }
	->
		(
			code_info__pick_matching_resume_addr(FailureMap,
				FailureAddress0)
		->
			{ FailureAddress = FailureAddress0 },
			{ PlaceCode = empty }
		;
			{ code_info__pick_first_resume_point(FailureMap,
				Map, FailureAddress) },
			{ map__to_assoc_list(Map, AssocList) },
			code_info__grab_code_info(CodeInfo),
			code_info__place_vars(AssocList, PlaceCode),
			code_info__slap_code_info(CodeInfo)
		),
		{ BranchCode = node([goto(FailureAddress) - "fail"]) },
		{ Code = tree(PlaceCode, BranchCode) }
	;
		{ Code = node([goto(do_redo) - "fail"]) }
	).

code_info__fail_if_rval_is_false(Rval0, Code) -->
	code_info__top_failure_cont(FailureCont),
	{ FailureCont = failure_cont(ContInfo, FailureMap) },
	(
		{ code_info__fail_cont_is_known(ContInfo) }
	->
		(
			code_info__pick_matching_resume_addr(FailureMap,
				FailureAddress0)
		->
				% We branch away if the test *fails*
			{ code_util__neg_rval(Rval0, Rval) },
			{ Code = node([
				if_val(Rval, FailureAddress0) -
					"Test for failure"
			]) }
		;
			{ code_info__pick_first_resume_point(FailureMap,
				Map, FailureAddress) },
			{ map__to_assoc_list(Map, AssocList) },
			code_info__get_next_label(SuccessLabel),
			code_info__grab_code_info(CodeInfo),
			code_info__place_vars(AssocList, PlaceCode),
			code_info__slap_code_info(CodeInfo),
			{ SuccessAddress = label(SuccessLabel) },
				% We branch away if the test *fails*,
				% therefore we branch around the code
				% that moves variables to their failure
				% locations and branches away
				% if the test succeeds
			{ TestCode = node([
				if_val(Rval0, SuccessAddress) -
					"Test for failure"
			]) },
			{ TailCode = node([
				goto(FailureAddress) -
					"Goto failure",
				label(SuccessLabel) -
					"Success continuation"
			]) },
			{ Code = tree(TestCode, tree(PlaceCode, TailCode)) }
		)
	;
		{ FailureAddress = do_redo },
			% We branch away if the test *fails*
		{ code_util__neg_rval(Rval0, Rval) },
		{ Code = node([
			if_val(Rval, FailureAddress) -
				"Test for failure"
		]) }
	).

%---------------------------------------------------------------------------%

	% See whether the current locations of variables match the locations
	% associated with any of the options in the given failure map.
	% If yes, return the code_addr of that option.

:- pred code_info__pick_matching_resume_addr(resume_maps, code_addr,
	code_info, code_info).
:- mode code_info__pick_matching_resume_addr(in, out, in, out) is semidet.

code_info__pick_matching_resume_addr(ResumeMaps, Addr) -->
	code_info__variable_locations(Locations),
	{
		ResumeMaps = orig_only(Map1, Addr1),
		( code_info__match_resume_loc(Map1, Locations) ->
			Addr = Addr1
		;
			fail
		)
	;
		ResumeMaps = stack_only(Map1, Addr1),
		( code_info__match_resume_loc(Map1, Locations) ->
			Addr = Addr1
		;
			fail
		)
	;
		ResumeMaps = orig_and_stack(Map1, Addr1, Map2, Addr2),
		( code_info__match_resume_loc(Map1, Locations) ->
			Addr = Addr1
		; code_info__match_resume_loc(Map2, Locations) ->
			Addr = Addr2
		;
			fail
		)
	;
		ResumeMaps = stack_and_orig(Map1, Addr1, Map2, Addr2),
		( code_info__match_resume_loc(Map1, Locations) ->
			Addr = Addr1
		; code_info__match_resume_loc(Map2, Locations) ->
			Addr = Addr2
		;
			fail
		)
	}.

:- pred code_info__match_resume_loc(resume_map, resume_map).
:- mode code_info__match_resume_loc(in, in) is semidet.

code_info__match_resume_loc(Map, Locations0) :-
	map__keys(Map, KeyList),
	set__list_to_set(KeyList, Keys),
	map__select(Locations0, Keys, Locations),
	map__to_assoc_list(Locations, List),
	\+ (
		list__member(Thingy, List),
		\+ (
			Thingy = Var - Actual,
			map__search(Map, Var, Rvals),
			set__subset(Rvals, Actual)
		)
	).

	% Find the first label that will be generated for the
	% given failure continuation based on the scheme used by
	% code_info__generate_failure_cont.

:- pred code_info__find_first_resume_label(failure_cont, code_addr).
:- mode code_info__find_first_resume_label(in, out) is det.

code_info__find_first_resume_label(FailureCont, Address) :-
	FailureCont = failure_cont(ContInfo, FailMap),
	(
		ContInfo = nondet(_, MaybeRedoLabel),
		MaybeRedoLabel = yes(RedoLabel)
	->
		Address = label(RedoLabel)
	;
		code_info__pick_first_resume_point(FailMap, _, Address)
	).

:- pred code_info__pick_first_resume_point(resume_maps, resume_map, code_addr).
:- mode code_info__pick_first_resume_point(in, out, out) is det.

code_info__pick_first_resume_point(orig_only(Map, Addr), Map, Addr).
code_info__pick_first_resume_point(stack_only(Map, Addr), Map, Addr).
code_info__pick_first_resume_point(orig_and_stack(Map, Addr, _, _), Map, Addr).
code_info__pick_first_resume_point(stack_and_orig(Map, Addr, _, _), Map, Addr).

:- pred code_info__pick_last_resume_point(resume_maps, resume_map, code_addr).
:- mode code_info__pick_last_resume_point(in, out, out) is det.

code_info__pick_last_resume_point(orig_only(Map, Addr), Map, Addr).
code_info__pick_last_resume_point(stack_only(Map, Addr), Map, Addr).
code_info__pick_last_resume_point(orig_and_stack( _, _, Map, Addr), Map, Addr).
code_info__pick_last_resume_point(stack_and_orig( _, _, Map, Addr), Map, Addr).

:- pred code_info__pick_stack_resume_point(resume_maps, resume_map, code_addr).
:- mode code_info__pick_stack_resume_point(in, out, out) is det.

code_info__pick_stack_resume_point(orig_only(_, _), _, _) :-
	error("no stack resume point").
code_info__pick_stack_resume_point(stack_only(Map, Addr), Map, Addr).
code_info__pick_stack_resume_point(orig_and_stack(_, _, Map, Addr), Map, Addr).
code_info__pick_stack_resume_point(stack_and_orig(Map, Addr, _, _), Map, Addr).

%---------------------------------------------------------------------------%

code_info__unset_failure_cont(Code) -->
	code_info__flush_resume_vars_to_stack(Code),
	code_info__top_failure_cont(FailureCont0),
	{ FailureCont0 = failure_cont(ContInfo0, FailureMap) },
	code_info__pop_failure_cont,
	{
		ContInfo0 = semidet,
		error("unset of semidet failure cont")
	;
		ContInfo0 = nondet(_, MaybeRedoLabel),
		ContInfo = nondet(unknown, MaybeRedoLabel)
	},
	{ FailureCont = failure_cont(ContInfo, FailureMap) },
	code_info__push_failure_cont(FailureCont).

code_info__flush_resume_vars_to_stack(Code) -->
	code_info__top_failure_cont(FailureCont0),
	{ FailureCont0 = failure_cont(_, FailureMap) },
	{ code_info__pick_stack_resume_point(FailureMap, StackMap, _) },
	{ map__to_assoc_list(StackMap, StackLocs) },
	code_info__place_resume_vars(StackLocs, Code).

code_info__may_use_nondet_tailcall(MayTailCall) -->
	code_info__top_failure_cont(FailureCont),
	{ FailureCont = failure_cont(ContInfo, FailureMap) },
	(
		{ code_info__fail_cont_is_known(ContInfo) },
		{ FailureMap = stack_only(_, do_fail) }
	->
		{ MayTailCall = yes }
	;
		{ MayTailCall = no }
	).

%---------------------------------------------------------------------------%

code_info__do_soft_cut(TheFrame, Code) -->
	code_info__top_failure_cont(FailureCont),
	{ FailureCont = failure_cont(ContInfo, _) },
	(
		{ code_info__fail_cont_is_known(ContInfo) }
	->
		{ code_info__find_first_resume_label(FailureCont, Address) }
	;
			% If the newly uncovered cont is unknown then
			% we must have created a new frame before the
			% condition which we no longer need, so we
			% set its redoip to do_fail.
		{ Address = do_fail }
	),
	{ Code = node([
		assign(redoip(lval(TheFrame)), const(code_addr_const(Address)))
			- "prune away the `else' case of the if-then-else"
	]) }.

%---------------------------------------------------------------------------%

code_info__generate_semi_pre_commit(RedoLabel, PreCommit) -->
	code_info__generate_pre_commit_saves(SaveCode),
	code_info__top_failure_cont(FailureCont0),
	{ FailureCont0 = failure_cont(_, FailureMap0) },
	code_info__clone_resume_maps(FailureMap0, FailureMap),
	{ FailureCont = failure_cont(nondet(known, no), FailureMap) },
	code_info__push_failure_cont(FailureCont),
	code_info__get_next_label(RedoLabel),
	{ HijackCode = node([
		assign(redoip(lval(maxfr)),
			const(code_addr_const(label(RedoLabel)))) -
			"Hijack the failure cont"
	]) },
	{ PreCommit = tree(SaveCode, HijackCode) }.

code_info__generate_semi_commit(RedoLabel, Commit) -->
	code_info__get_next_label(SuccLabel),
	{ GotoSuccLabel = node([
		goto(label(SuccLabel)) - "Jump to success continuation"
	]) },
	{ SuccLabelCode = node([
		label(SuccLabel) - "Success continuation"
	]) },
	{ RedoLabelCode = node([
		label(RedoLabel) - "Failure (redo) continuation"
	]) },

	code_info__grab_code_info(CodeInfo0),
	code_info__top_failure_cont(FailureCont),
	{ FailureCont = failure_cont(_, FailureMaps) },
	code_info__generate_resume_setup(FailureMaps, FailureContCode),
	code_info__pop_failure_cont,
	code_info__generate_failure(Fail),
	code_info__slap_code_info(CodeInfo0),
	code_info__pop_failure_cont,

	code_info__undo_pre_commit_saves(RestoreMaxfr, RestoreRedoip,
		RestoreCurfr, PopCode),

	{ SuccessCode =
		tree(RestoreMaxfr,
		tree(RestoreRedoip,
		tree(RestoreCurfr,
		     PopCode)))
	},
	{ FailCode =
		tree(RedoLabelCode,
		tree(RestoreCurfr,
		tree(FailureContCode,
		tree(RestoreRedoip,
		tree(PopCode,
		     Fail)))))
	},
	{ Commit =
		tree(SuccessCode,
		tree(GotoSuccLabel,
		tree(FailCode,
		     SuccLabelCode)))
	}.

%---------------------------------------------------------------------------%

code_info__generate_det_pre_commit(PreCommit) -->
	code_info__generate_pre_commit_saves(PreCommit),
	% Since the code we are cutting is model_non, it will call
	% unset_failure_cont. If the current top entry on the failure stack
	% at the time is semidet, this would cause unset_failure_cont to abort.
	% We therefore push a dummy nondet failure continuation onto the
	% failure stack. Since the code we are cutting across is multi,
	% the failure continuation will never actually be used.
	{ map__init(Empty) },
	{ FailureCont = failure_cont(nondet(known, no),
		stack_only(Empty, do_fail)) },
	code_info__push_failure_cont(FailureCont).

code_info__generate_det_commit(Commit) -->
	% Remove the dummy failure continuation pushed by det_pre_commit.
	code_info__pop_failure_cont,
	code_info__undo_pre_commit_saves(RestoreMaxfr, RestoreRedoip,
		RestoreCurfr, PopCode),
	{ Commit = tree(RestoreMaxfr,
		   tree(RestoreRedoip,
		   tree(RestoreCurfr,
		        PopCode)))
	}.

%---------------------------------------------------------------------------%

:- pred code_info__generate_pre_commit_saves(code_tree, code_info, code_info).
:- mode code_info__generate_pre_commit_saves(out, in, out) is det.

code_info__generate_pre_commit_saves(Code) -->
	code_info__get_proc_model(CodeModel),
	( { CodeModel = model_non } ->
		% the pushes and pops on the det stack below will cause
		% problems for accurate garbage collection. Hence we
		% make sure the commit vals are made live, so gc
		% can figure out what is going on later.
		code_info__get_module_info(ModuleInfo),
		code_info__get_pred_id(PredId),
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ string__append("commit in ", PredName, Message) },
		{ PushCode = node([
			incr_sp(3, Message) -
				"push space for curfr, maxfr, and redoip"
		]) },
		{ CurfrSlot = stackvar(1) },
		{ MaxfrSlot = stackvar(2) },
		{ RedoipSlot = stackvar(3) },
		code_info__add_commit_val(curfr, CurfrSlot),
		code_info__add_commit_val(maxfr, MaxfrSlot),
		code_info__add_commit_val(redoip(lval(maxfr)), RedoipSlot)
	;
		{ PushCode = empty },
		code_info__push_temp(lval(curfr), CurfrSlot),
		code_info__push_temp(lval(maxfr), MaxfrSlot),
		code_info__push_temp(lval(redoip(lval(maxfr))), RedoipSlot)
	),
	{ SaveCode = node([
		assign(CurfrSlot, lval(curfr)) -
				"Save current nondet frame pointer",
		assign(MaxfrSlot, lval(maxfr)) -
				"Save top of nondet stack",
		assign(RedoipSlot, lval(redoip(lval(maxfr)))) -
				"Save the top redoip"
	]) },
	{ Code = tree(PushCode, SaveCode) }.

:- pred code_info__undo_pre_commit_saves(code_tree, code_tree,
	code_tree, code_tree, code_info, code_info).
:- mode code_info__undo_pre_commit_saves(out, out, out, out, in, out) is det.

code_info__undo_pre_commit_saves(RestoreMaxfr, RestoreRedoip,
		RestoreCurfr, PopCode) -->
	code_info__get_proc_model(CodeModel),
	( { CodeModel = model_non } ->
		{ PopCode = node([decr_sp(3) -
			"pop redoip, maxfr & curfr"]) },
		{ RedoipSlot = stackvar(3) },
		{ MaxfrSlot = stackvar(2) },
		{ CurfrSlot = stackvar(1) },
		code_info__rem_commit_val,
		code_info__rem_commit_val,
		code_info__rem_commit_val
	;
		{ PopCode = empty },
		code_info__pop_temp(RedoipSlot),
		code_info__pop_temp(MaxfrSlot),
		code_info__pop_temp(CurfrSlot)
	),
	{ RestoreMaxfr = node([
		assign(maxfr, lval(MaxfrSlot)) -
			"Prune away unwanted choice-points"
	]) },
	{ RestoreRedoip = node([
		assign(redoip(lval(maxfr)), lval(RedoipSlot)) -
			"Restore the top redoip"
	]) },
	{ RestoreCurfr = node([
		assign(curfr, lval(CurfrSlot)) -
			"Restore nondet frame pointer"
	]) }.

:- pred code_info__clone_resume_maps(resume_maps, resume_maps,
	code_info, code_info).
:- mode code_info__clone_resume_maps(in, out, in, out) is det.

code_info__clone_resume_maps(ResumeMaps0, ResumeMaps) -->
	(
		{ ResumeMaps0 = orig_only(Map1, _) },
		code_info__get_next_label(Label1),
		{ Addr1 = label(Label1) },
		{ ResumeMaps = orig_only(Map1, Addr1) }
	;
		{ ResumeMaps0 = stack_only(Map1, _) },
		code_info__get_next_label(Label1),
		{ Addr1 = label(Label1) },
		{ ResumeMaps = stack_only(Map1, Addr1) }
	;
		{ ResumeMaps0 = stack_and_orig(Map1, _, Map2, _) },
		code_info__get_next_label(Label1),
		{ Addr1 = label(Label1) },
		code_info__get_next_label(Label2),
		{ Addr2 = label(Label2) },
		{ ResumeMaps = stack_and_orig(Map1, Addr1, Map2, Addr2) }
	;
		{ ResumeMaps0 = orig_and_stack(Map1, _, Map2, _) },
		code_info__get_next_label(Label1),
		{ Addr1 = label(Label1) },
		code_info__get_next_label(Label2),
		{ Addr2 = label(Label2) },
		{ ResumeMaps = orig_and_stack(Map1, Addr1, Map2, Addr2) }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule to deal with liveness issues.

:- interface.

:- pred code_info__variable_is_live(var, code_info, code_info).
:- mode code_info__variable_is_live(in, in, out) is semidet.

:- pred code_info__get_live_variables(list(var), code_info, code_info).
:- mode code_info__get_live_variables(out, in, out) is det.

:- pred code_info__update_liveness_info(set(var), code_info, code_info).
:- mode code_info__update_liveness_info(in, in, out) is det.

:- pred code_info__update_deadness_info(set(var), code_info, code_info).
:- mode code_info__update_deadness_info(in, in, out) is det.

	% Make these variables appear magically live.
	% We don't care where they are put.

:- pred code_info__make_vars_live(set(var), code_info, code_info).
:- mode code_info__make_vars_live(in, in, out) is det.

:- pred code_info__make_vars_dead(set(var), code_info, code_info).
:- mode code_info__make_vars_dead(in, in, out) is det.

:- pred code_info__pickup_zombies(set(var), code_info, code_info).
:- mode code_info__pickup_zombies(out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

code_info__get_live_variables(VarList) -->
	code_info__get_liveness_info(ForwardLiveVars),
	code_info__current_resume_point_vars(ResumeVars),
	{ set__union(ForwardLiveVars, ResumeVars, Vars) },
	{ set__to_sorted_list(Vars, VarList) }.

code_info__variable_is_live(Var) -->
	code_info__get_liveness_info(Liveness),
	code_info__current_resume_point_vars(ResumeVars),
	(
		{ set__member(Var, Liveness) }
	;
		{ set__member(Var, ResumeVars) }
	).

code_info__update_liveness_info(Births) -->
	code_info__get_liveness_info(Liveness0),
	{ set__union(Liveness0, Births, Liveness) },
	code_info__set_liveness_info(Liveness).

code_info__update_deadness_info(Deaths) -->
	code_info__get_liveness_info(Liveness0),
	{ set__difference(Liveness0, Deaths, Liveness) },
	code_info__set_liveness_info(Liveness).

code_info__make_vars_live(Vars) -->
	code_info__get_stack_slots(StackSlots),
	code_info__get_exprn_info(Exprn0),
	{ set__to_sorted_list(Vars, VarList) },
	{ code_info__make_vars_live_2(VarList, StackSlots, 1, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

:- pred code_info__make_vars_live_2(list(var), stack_slots, int,
	exprn_info, exprn_info).
:- mode code_info__make_vars_live_2(in, in, in, in, out) is det.

code_info__make_vars_live_2([], _, _, Exprn, Exprn).
code_info__make_vars_live_2([V | Vs], StackSlots, N0, Exprn0, Exprn) :-
	( map__search(StackSlots, V, Lval0) ->
		Lval = Lval0,
		N1 = N0
	;
		code_info__find_unused_reg(N0, Exprn0, N1),
		Lval = reg(r(N1))
	),
	code_exprn__maybe_set_var_location(V, Lval, Exprn0, Exprn1),
	code_info__make_vars_live_2(Vs, StackSlots, N1, Exprn1, Exprn).

:- pred code_info__find_unused_reg(int, exprn_info, int).
:- mode code_info__find_unused_reg(in, in, out) is det.

code_info__find_unused_reg(N0, Exprn0, N) :-
	( code_exprn__lval_in_use(reg(r(N0)), Exprn0, _) ->
		N1 is N0 + 1,
		code_info__find_unused_reg(N1, Exprn0, N)
	;
		N = N0
	).

code_info__make_vars_dead(Vars0) -->
	code_info__current_resume_point_vars(ResumeVars),
	{ set__intersect(Vars0, ResumeVars, FlushVars) },
	code_info__get_zombies(Zombies0),
	{ set__union(Zombies0, FlushVars, Zombies) },
	code_info__set_zombies(Zombies),
	{ set__difference(Vars0, Zombies, Vars) },
	{ set__to_sorted_list(Vars, VarList) },
	code_info__make_vars_dead_2(VarList).

:- pred code_info__make_vars_dead_2(list(var), code_info, code_info).
:- mode code_info__make_vars_dead_2(in, in, out) is det.

code_info__make_vars_dead_2([]) --> [].
code_info__make_vars_dead_2([V | Vs]) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__var_becomes_dead(V, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn),
	code_info__make_vars_dead_2(Vs).

code_info__pickup_zombies(Zombies) -->
	code_info__get_zombies(Zombies),
	{ set__init(Empty) },
	code_info__set_zombies(Empty).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for handling the saving and restoration
	% of tickets, heap pointers, stack pointers etc.

:- interface.

:- pred code_info__save_hp(code_tree, lval, code_info, code_info).
:- mode code_info__save_hp(out, out, in, out) is det.

:- pred code_info__restore_hp(lval, code_tree, code_info, code_info).
:- mode code_info__restore_hp(in, out, in, out) is det.

:- pred code_info__restore_and_discard_hp(lval, code_tree,
	code_info, code_info).
:- mode code_info__restore_and_discard_hp(in, out, in, out) is det.

:- pred code_info__discard_hp(lval, code_info, code_info).
:- mode code_info__discard_hp(in, in, out) is det.

:- pred code_info__maybe_save_hp(bool, code_tree, maybe(lval),
	code_info, code_info).
:- mode code_info__maybe_save_hp(in, out, out, in, out) is det.

:- pred code_info__maybe_restore_hp(maybe(lval), code_tree,
	code_info, code_info).
:- mode code_info__maybe_restore_hp(in, out, in, out) is det.

:- pred code_info__maybe_restore_and_discard_hp(maybe(lval), code_tree,
	code_info, code_info).
:- mode code_info__maybe_restore_and_discard_hp(in, out, in, out) is det.

:- pred code_info__maybe_discard_hp(maybe(lval), code_info, code_info).
:- mode code_info__maybe_discard_hp(in, in, out) is det.

:- pred code_info__save_ticket(code_tree, lval, code_info, code_info).
:- mode code_info__save_ticket(out, out, in, out) is det.

:- pred code_info__restore_ticket(lval, code_tree, code_info, code_info).
:- mode code_info__restore_ticket(in, out, in, out) is det.

:- pred code_info__restore_and_discard_ticket(lval, code_tree,
	code_info, code_info).
:- mode code_info__restore_and_discard_ticket(in, out, in, out) is det.

:- pred code_info__discard_ticket(lval, code_tree, code_info, code_info).
:- mode code_info__discard_ticket(in, out, in, out) is det.

:- pred code_info__maybe_save_ticket(bool, code_tree, maybe(lval),
	code_info, code_info).
:- mode code_info__maybe_save_ticket(in, out, out, in, out) is det.

:- pred code_info__maybe_restore_ticket(maybe(lval), code_tree,
	code_info, code_info).
:- mode code_info__maybe_restore_ticket(in, out, in, out) is det.

:- pred code_info__maybe_restore_and_discard_ticket(maybe(lval), code_tree,
	code_info, code_info).
:- mode code_info__maybe_restore_and_discard_ticket(in, out, in, out) is det.

:- pred code_info__maybe_discard_ticket(maybe(lval), code_tree,
	code_info, code_info).
:- mode code_info__maybe_discard_ticket(in, out, in, out) is det.

:- pred code_info__save_redoip(code_tree, code_info, code_info).
:- mode code_info__save_redoip(out, in, out) is det.

:- pred code_info__restore_redoip(code_tree, code_info, code_info).
:- mode code_info__restore_redoip(out, in, out) is det.

:- pred code_info__save_maxfr(lval, code_tree, code_info, code_info).
:- mode code_info__save_maxfr(out, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

code_info__save_hp(Code, HpSlot) -->
	code_info__push_temp(lval(hp), HpSlot),
	{ Code = node([mark_hp(HpSlot) - "Save heap pointer"]) }.

code_info__restore_hp(HpSlot, Code) -->
	{ Code = node([restore_hp(lval(HpSlot)) - "Restore heap pointer"]) }.

code_info__discard_hp(HpSlot) -->
	code_info__get_stack_top(Lval),
	( { Lval = HpSlot } ->
		code_info__pop_temp(_)
	;
		{ error("improperly nested temp, used for hp") }
	).

code_info__restore_and_discard_hp(HpSlot, Code) -->
	{ Code = node([restore_hp(lval(HpSlot)) - "Restore heap pointer"]) },
	code_info__discard_hp(HpSlot).

code_info__maybe_save_hp(Maybe, Code, MaybeHpSlot) -->
	( { Maybe = yes } ->
		code_info__save_hp(Code, HpSlot),
		{ MaybeHpSlot = yes(HpSlot) }
	;
		{ Code = empty },
		{ MaybeHpSlot = no }
	).

code_info__maybe_restore_hp(MaybeHpSlot, Code) -->
	( { MaybeHpSlot = yes(HpSlot) } ->
		code_info__restore_hp(HpSlot, Code)
	;
		{ Code = empty }
	).

code_info__maybe_restore_and_discard_hp(MaybeHpSlot, Code) -->
	( { MaybeHpSlot = yes(HpSlot) } ->
		code_info__restore_and_discard_hp(HpSlot, Code)
	;
		{ Code = empty }
	).

code_info__maybe_discard_hp(MaybeHpSlot) -->
	( { MaybeHpSlot = yes(HpSlot) } ->
		code_info__discard_hp(HpSlot)
	;
		[]
	).

% ZZZ

code_info__save_ticket(Code, TicketSlot) -->
	code_info__push_temp(ticket, TicketSlot),
	{ Code = node([store_ticket(TicketSlot) - "Save solver state"]) }.

code_info__restore_ticket(TicketSlot, Code) -->
	{ Code = node([restore_ticket(lval(TicketSlot)) - "Restore solver state"]) }.

code_info__restore_and_discard_ticket(TicketSlot, Code) -->
	code_info__get_stack_top(Lval),
	( { Lval = TicketSlot } ->
		code_info__pop_temp(_)
	;
		{ error("improperly nested temp, used for ticket") }
	),
	{ Code = tree(
		node([restore_ticket(lval(TicketSlot)) - "Restore solver state"]),
		node([discard_ticket - "Pop ticket stack"]) )
	}.

code_info__discard_ticket(TicketSlot, Code) -->
	code_info__get_stack_top(Lval),
	( { Lval = TicketSlot } ->
		code_info__pop_temp(_)
	;
		{ error("improperly nested temp, used for ticket") }
	),
	{ Code = node([discard_ticket - "Pop ticket stack"]) }.

code_info__maybe_save_ticket(Maybe, Code, MaybeTicketSlot) -->
	( { Maybe = yes } ->
		code_info__save_ticket(Code, TicketSlot),
		{ MaybeTicketSlot = yes(TicketSlot) }
	;
		{ Code = empty },
		{ MaybeTicketSlot = no }
	).

code_info__maybe_restore_ticket(MaybeTicketSlot, Code) -->
	( { MaybeTicketSlot = yes(TicketSlot) } ->
		code_info__restore_ticket(TicketSlot, Code)
	;
		{ Code = empty }
	).

code_info__maybe_restore_and_discard_ticket(MaybeTicketSlot, Code) -->
	( { MaybeTicketSlot = yes(TicketSlot) } ->
		code_info__restore_and_discard_ticket(TicketSlot, Code)
	;
		{ Code = empty }
	).

code_info__maybe_discard_ticket(MaybeTicketSlot, Code) -->
	( { MaybeTicketSlot = yes(TicketSlot) } ->
		code_info__discard_ticket(TicketSlot, Code)
	;
		{ Code = empty }
	).

code_info__save_redoip(Code) -->
	code_info__push_temp(lval(redoip(lval(maxfr))), RedoIpSlot),
	{ Code = node([assign(RedoIpSlot, lval(redoip(lval(maxfr))))
		- "Save the redoip"]) }.

code_info__restore_redoip(Code) -->
	code_info__pop_temp(Lval),
	{ Code = node([assign(redoip(lval(maxfr)), lval(Lval))
		- "Restore the redoip"]) }.

code_info__save_maxfr(MaxfrSlot, Code) -->
	code_info__push_temp(lval(maxfr), MaxfrSlot),
	{ Code = node([assign(MaxfrSlot, lval(maxfr)) - "Save maxfr"]) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule to deal with code_exprn.

:- interface.

:- pred code_info__variable_locations(map(var, set(rval)),
	code_info, code_info).
:- mode code_info__variable_locations(out, in, out) is det.

:- pred code_info__set_var_location(var, lval, code_info, code_info).
:- mode code_info__set_var_location(in, in, in, out) is det.

:- pred code_info__cache_expression(var, rval, code_info, code_info).
:- mode code_info__cache_expression(in, in, in, out) is det.

:- pred code_info__place_var(var, lval, code_tree, code_info, code_info).
:- mode code_info__place_var(in, in, out, in, out) is det.

:- pred code_info__produce_variable(var, code_tree, rval, code_info, code_info).
:- mode code_info__produce_variable(in, out, out, in, out) is det.

:- pred code_info__produce_variable_in_reg(var, code_tree, rval,
	code_info, code_info).
:- mode code_info__produce_variable_in_reg(in, out, out, in, out) is det.

:- pred code_info__materialize_vars_in_rval(rval, rval, code_tree, code_info,
	code_info).
:- mode code_info__materialize_vars_in_rval(in, out, out, in, out) is det.

:- pred code_info__lock_reg(reg, code_info, code_info).
:- mode code_info__lock_reg(in, in, out) is det.

:- pred code_info__unlock_reg(reg, code_info, code_info).
:- mode code_info__unlock_reg(in, in, out) is det.

:- pred code_info__acquire_reg(reg, code_info, code_info).
:- mode code_info__acquire_reg(out, in, out) is det.

:- pred code_info__release_reg(reg, code_info, code_info).
:- mode code_info__release_reg(in, in, out) is det.

:- pred code_info__clear_r1(code_tree, code_info, code_info).
:- mode code_info__clear_r1(out, in, out) is det.

:- pred code_info__generate_branch_end(code_model, store_map, code_tree,
	code_info, code_info).
:- mode code_info__generate_branch_end(in, in, out, in, out) is det.

	% code_info__remake_with_store_map throws away the exprn_info data
	% structure, forgetting the current locations of all variables,
	% and rebuilds it from scratch based on the given store map.
	% The new exprn_info will know about only the variables present
	% in the store map, and will believe they are where the store map
	% says they are.

:- pred code_info__remake_with_store_map(store_map, code_info, code_info).
:- mode code_info__remake_with_store_map(in, in, out) is det.

:- type call_direction ---> caller ; callee.

	% Generate code to either setup the input arguments for a call
	% (i.e. in the caller), or to setup the output arguments in the
	% predicate epilog (i.e. in the callee).

:- pred code_info__setup_call(assoc_list(var, arg_info),
	call_direction, code_tree, code_info, code_info).
:- mode code_info__setup_call(in, in, out, in, out) is det.

:- pred code_info__clear_all_registers(code_info, code_info).
:- mode code_info__clear_all_registers(in, out) is det.

:- pred code_info__save_variable_on_stack(var, code_tree,
	code_info, code_info).
:- mode code_info__save_variable_on_stack(in, out, in, out) is det.

:- pred code_info__save_variables_on_stack(list(var), code_tree,
	code_info, code_info).
:- mode code_info__save_variables_on_stack(in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred code_info__place_vars(assoc_list(var, set(rval)), code_tree,
	code_info, code_info).
:- mode code_info__place_vars(in, out, in, out) is det.

:- pred code_info__produce_variable_in_reg_or_stack(var, code_tree, rval,
	code_info, code_info).
:- mode code_info__produce_variable_in_reg_or_stack(in, out, out, in, out)
	is det.

code_info__variable_locations(Locations) -->
	code_info__get_exprn_info(Exprn),
	{ code_exprn__get_varlocs(Exprn, Locations) }.

code_info__set_var_location(Var, Lval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__set_var_location(Var, Lval, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__cache_expression(Var, Rval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__cache_exprn(Var, Rval, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__place_var(Var, Lval, Code) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__place_var(Var, Lval, Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__place_vars([], empty) --> [].
code_info__place_vars([V - Rs | RestList], Code) -->
	(
		{ set__to_sorted_list(Rs, RList) },
		{ code_info__lval_in_rval_list(L, RList) }
	->
		code_info__place_var(V, L, ThisCode)
	;
		{ ThisCode = empty }
	),
	code_info__place_vars(RestList, RestCode),
	{ Code = tree(ThisCode, RestCode) }.

:- pred code_info__lval_in_rval_list(lval, list(rval)).
:- mode code_info__lval_in_rval_list(out, in) is semidet.

code_info__lval_in_rval_list(Lval, [Rval | Rvals]) :-
	( Rval = lval(Lval0) ->
		Lval = Lval0
	;
		code_info__lval_in_rval_list(Lval, Rvals)
	).

code_info__produce_variable(Var, Code, Rval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__produce_var(Var, Rval, Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__produce_variable_in_reg(Var, Code, Rval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__produce_var_in_reg(Var, Rval, Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__produce_variable_in_reg_or_stack(Var, Code, Rval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__produce_var_in_reg_or_stack(Var, Rval, Code,
		Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__materialize_vars_in_rval(Rval0, Rval, Code) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__materialize_vars_in_rval(Rval0, Rval, Code,
		Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__lock_reg(Reg) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__lock_reg(Reg, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__unlock_reg(Reg) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__unlock_reg(Reg, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__acquire_reg(Reg) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__acquire_reg(Reg, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__release_reg(Reg) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__release_reg(Reg, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__clear_r1(Code) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__clear_r1(Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__generate_branch_end(CodeModel, StoreMap, Code) -->
	code_info__get_exprn_info(Exprn0),
	{ map__to_assoc_list(StoreMap, VarLocs) },
	{ code_exprn__place_vars(VarLocs, PlaceCode, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn),
	( { CodeModel = model_non } ->
		code_info__unset_failure_cont(FlushCode),
		{ Code = tree(PlaceCode, FlushCode) }
	;
		{ Code = PlaceCode }
	).

code_info__remake_with_store_map(StoreMap) -->
	{ map__to_assoc_list(StoreMap, VarLvals) },
	{ code_info__fixup_lvallist(VarLvals, VarRvals) },
	code_info__get_varset(Varset),
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ code_exprn__init_state(VarRvals, Varset, Options, Exprn) },
	code_info__set_exprn_info(Exprn).

:- pred code_info__fixup_lvallist(assoc_list(var, lval), assoc_list(var, rval)).
:- mode code_info__fixup_lvallist(in, out) is det.

code_info__fixup_lvallist([], []).
code_info__fixup_lvallist([V - L | Ls], [V - lval(L) | Rs]) :-
	code_info__fixup_lvallist(Ls, Rs).

%---------------------------------------------------------------------------%

code_info__setup_call([], _Direction, empty) --> [].
code_info__setup_call([V - arg_info(Loc,Mode) | Rest], Direction, Code) -->
	(
		{
			Mode = top_in,
			Direction = caller
		;
			Mode = top_out,
			Direction = callee
		}
	->
		{ code_util__arg_loc_to_register(Loc, Reg) },
		code_info__get_exprn_info(Exprn0),
		{ code_exprn__place_var(V, reg(Reg), Code0, Exprn0, Exprn1) },
			% We need to test that either the variable
			% is live OR it occurs in the remaining arguments
			% because of a bug in polymorphism.m which
			% causes some compiler generated code to violate
			% superhomogeneous form
		(
			code_info__variable_is_live(V)
		->
			{ IsLive = yes }
		;
			{ IsLive = no }
		),
		{
			list__member(Vtmp - _, Rest),
			V = Vtmp
		->
			Occurs = yes
		;
			Occurs = no
		},
		(
				% We can't simply use a disj here
				% because of bugs in modes/det_analysis
			{ bool__or(Occurs, IsLive, yes) }
		->
			{ code_exprn__lock_reg(Reg, Exprn1, Exprn2) },
			code_info__set_exprn_info(Exprn2),
			code_info__setup_call(Rest, Direction, Code1),
			code_info__get_exprn_info(Exprn3),
			{ code_exprn__unlock_reg(Reg, Exprn3, Exprn) },
			code_info__set_exprn_info(Exprn),
			{ Code = tree(Code0, Code1) }
		;
			{ code_exprn__lock_reg(Reg, Exprn1, Exprn2) },
			code_info__set_exprn_info(Exprn2),
			{ set__singleton_set(Vset, V) },
			code_info__make_vars_dead(Vset),
			code_info__setup_call(Rest, Direction, Code1),
			code_info__get_exprn_info(Exprn4),
			{ code_exprn__unlock_reg(Reg, Exprn4, Exprn) },
			code_info__set_exprn_info(Exprn),
			{ Code = tree(Code0, Code1) }
		)
	;
		code_info__setup_call(Rest, Direction, Code)
	).

	% XXX We could use the sanity checking mechanism...
code_info__clear_all_registers -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__clobber_regs([], Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__save_variable_on_stack(Var, Code) -->
	code_info__get_variable_slot(Var, Slot),
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__place_var(Var, Slot, Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

code_info__save_variables_on_stack([], empty) --> [].
code_info__save_variables_on_stack([Var | Vars], Code) -->
	code_info__save_variable_on_stack(Var, FirstCode),
	code_info__save_variables_on_stack(Vars, RestCode),
	{ Code = tree(FirstCode, RestCode) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for dealing with information for garbage collection
	% and value numbering.

:- interface.

:- pred code_info__generate_stack_livevals(set(var), set(lval),
	code_info, code_info).
:- mode code_info__generate_stack_livevals(in, out, in, out) is det.

:- pred code_info__generate_stack_livelvals(set(var), list(liveinfo),
	code_info, code_info).
:- mode code_info__generate_stack_livelvals(in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

code_info__generate_stack_livevals(Args, LiveVals) -->
	code_info__get_live_variables(LiveVars),
	{ set__list_to_set(LiveVars, Vars0) },
	{ set__difference(Vars0, Args, Vars) },
	{ set__to_sorted_list(Vars, VarList) },
	{ set__init(LiveVals0) },
	code_info__generate_stack_livevals_2(VarList, LiveVals0, LiveVals1),
	code_info__get_pushed_values(Pushed0),
	code_info__get_commit_vals(CommitVals),
	{ stack__push_list(Pushed0, CommitVals, Pushed) },
	{ code_info__generate_stack_livevals_3(Pushed, LiveVals1, LiveVals) }.

:- pred code_info__generate_stack_livevals_2(list(var), set(lval), set(lval),
	code_info, code_info).
:- mode code_info__generate_stack_livevals_2(in, in, out, in, out) is det.

code_info__generate_stack_livevals_2([], Vals, Vals) --> [].
code_info__generate_stack_livevals_2([V | Vs], Vals0, Vals) -->
	code_info__get_variable_slot(V, Slot),
	{ set__insert(Vals0, Slot, Vals1) },
	code_info__generate_stack_livevals_2(Vs, Vals1, Vals).

:- pred code_info__generate_stack_livevals_3(stack(pair(lval, lval_or_ticket)),
	set(lval), set(lval)).
:- mode code_info__generate_stack_livevals_3(in, in, out) is det.

code_info__generate_stack_livevals_3(Stack0, Vals0, Vals) :-
	(
		stack__pop(Stack0, Top - _, Stack1)
	->
		set__insert(Vals0, Top, Vals1),
		code_info__generate_stack_livevals_3(Stack1, Vals1, Vals)
	;
		Vals = Vals0
	).

%---------------------------------------------------------------------------%

code_info__generate_stack_livelvals(Args, LiveVals) -->
	code_info__get_live_variables(LiveVars),
	{ set__list_to_set(LiveVars, Vars0) },
	{ set__difference(Vars0, Args, Vars) },
	{ set__to_sorted_list(Vars, VarList) },
	{ set__init(LiveVals0) },
	code_info__generate_stack_livelvals_2(VarList, LiveVals0, LiveVals1),
	{ set__to_sorted_list(LiveVals1, LiveVals2) },
	code_info__get_globals(Globals),
	{ globals__get_gc_method(Globals, GC_Method) },
	code_info__livevals_to_livelvals(LiveVals2, LiveVals3, GC_Method),
	code_info__get_pushed_values(Pushed0),
	code_info__get_commit_vals(CommitVals),
	{ stack__push_list(Pushed0, CommitVals, Pushed) },
	{ code_info__generate_stack_livelvals_3(Pushed, LiveVals3, LiveVals) }.

:- pred code_info__generate_stack_livelvals_2(list(var),
	set(pair(lval, var)), set(pair(lval, var)), code_info, code_info).
:- mode code_info__generate_stack_livelvals_2(in, in, out, in, out) is det.

code_info__generate_stack_livelvals_2([], Vals, Vals) --> [].
code_info__generate_stack_livelvals_2([V | Vs], Vals0, Vals) -->
	code_info__get_variable_slot(V, Slot),
	{ set__insert(Vals0, Slot - V, Vals1) },
	code_info__generate_stack_livelvals_2(Vs, Vals1, Vals).

:- pred code_info__generate_stack_livelvals_3(stack(pair(lval,
	lval_or_ticket)), list(liveinfo), list(liveinfo)).
:- mode code_info__generate_stack_livelvals_3(in, in, out) is det.

code_info__generate_stack_livelvals_3(Stack0, LiveInfo0, LiveInfo) :-
	(
		stack__pop(Stack0, Top - StoredLval , Stack1)
	->
		code_info__get_shape_num(StoredLval, S_Num),
		LiveInfo = [live_lvalue(Top, S_Num, no) | Lives],
		code_info__generate_stack_livelvals_3(Stack1, LiveInfo0, Lives)
	;
		LiveInfo = LiveInfo0
	).

:- pred code_info__livevals_to_livelvals(list(pair(lval, var)),
	list(liveinfo), gc_method, code_info, code_info).
:- mode code_info__livevals_to_livelvals(in, out, in, in, out) is det.

code_info__livevals_to_livelvals([], [], _GC_Method, C, C).
code_info__livevals_to_livelvals([L - V | Ls],
		[live_lvalue(L, num(S_Num), TypeParams) | Lives], GC_Method) -->
	(
		{ GC_Method = accurate }
	->
		code_info__get_module_info(ModuleInfo),
		code_info__get_shapes(S_Tab0),
		{ module_info_types(ModuleInfo, Type_Table) },
		code_info__variable_type(V, Type),

		% XXX We don't yet support partial insts when allocating
		% XXX shapes, so pass ground(shared, no) as a placeholder.
		{ shapes__request_shape_number(Type - ground(shared, no),
			Type_Table, S_Tab0, S_Tab1, S_Num) },
		{ type_util__vars(Type, TypeVars) },
		(
			% if not polymorphic
			{ TypeVars = [] }
		->
			{ TypeParams = no }
		;
			code_info__find_type_infos(TypeVars, Lvals),
			{ TypeParams = yes(Lvals) }
		),
		code_info__set_shapes(S_Tab1)
	;
		% Dummy values
		{ TypeParams = no },
		{ S_Num = 0 }
	),
	code_info__livevals_to_livelvals(Ls, Lives, GC_Method).

:- pred code_info__get_shape_num(lval_or_ticket, shape_num).
:- mode code_info__get_shape_num(in, out) is det.

code_info__get_shape_num(lval(succip), succip).
code_info__get_shape_num(lval(hp), hp).
code_info__get_shape_num(lval(maxfr), maxfr).
code_info__get_shape_num(lval(curfr), curfr).
code_info__get_shape_num(lval(succfr(_)), succfr).
code_info__get_shape_num(lval(prevfr(_)), prevfr).
code_info__get_shape_num(lval(redoip(_)), redoip).
code_info__get_shape_num(lval(succip(_)), succip).
code_info__get_shape_num(lval(sp), sp).
code_info__get_shape_num(lval(lvar(_)), unwanted).
code_info__get_shape_num(lval(field(_, _, _)), unwanted).
code_info__get_shape_num(lval(temp(_)), unwanted).
code_info__get_shape_num(lval(reg(_)), unwanted).
code_info__get_shape_num(lval(stackvar(_)), unwanted).
code_info__get_shape_num(lval(framevar(_)), unwanted).
code_info__get_shape_num(ticket, ticket).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for managing stack slots.

	% The det stack frame is organized as follows.
	%
	%		... unused ...
	%	sp --->	<first unused slot>
	%		<space for local var 1>
	%		... local vars ...
	%		<space for local var n>
	%		<space for temporary reg save 1>
	%		... temporary reg saves ...
	%		<space for temporary reg save n>
	%		<space for succip>
	%
	% The stack pointer points to the first free location at the
	% top of the stack.
	%
	% `code_info__num_stackslots' counts the number of slots reserved
	% for saving local variables. XXX
	%
	% `code_info__max_push_count' counts the number of slots reserved
	% for saving and restoring registers (hp, redoip, etc.)
	%
	% `code_info__succip_used' determines whether we need a slot to
	% hold the succip.
	%
	% The variable part of the nondet stack is organized in the same way
	% as the det stack (but the nondet stack also contains several other
	% fixed fields.)

:- interface.

	% Returns the total stackslot count, but not including space for
	% succip.
:- pred code_info__get_total_stackslot_count(int, code_info, code_info).
:- mode code_info__get_total_stackslot_count(out, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

	% `push_temp' doesn't actually increment the stack pointer, it just
	% increments the push count.  The space will be allocated in the
	% procedure prologue.

:- pred code_info__push_temp(lval_or_ticket, lval, code_info, code_info).
:- mode code_info__push_temp(in, out, in, out) is det.

:- pred code_info__pop_temp(lval, code_info, code_info).
:- mode code_info__pop_temp(out, in, out) is det.

:- pred code_info__get_stack_top(lval, code_info, code_info).
:- mode code_info__get_stack_top(out, in, out) is det.

:- pred code_info__pop_stack(code_tree, code_info, code_info).
:- mode code_info__pop_stack(out, in, out) is det.

:- pred code_info__get_variable_slot(var, lval, code_info, code_info).
:- mode code_info__get_variable_slot(in, out, in, out) is det.

:- pred code_info__max_slot(stack_slots, int).
:- mode code_info__max_slot(in, out) is det.

:- pred code_info__stack_variable(int, lval, code_info, code_info).
:- mode code_info__stack_variable(in, out, in, out) is det.

code_info__push_temp(Item, StackVar) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 + 1 },
	code_info__set_push_count(Count),
	code_info__get_stackslot_count(NumSlots),
	{ Slot is Count + NumSlots },
	code_info__stack_variable(Slot, StackVar),
	code_info__get_pushed_values(VStack0),
	{ stack__push(VStack0, StackVar - Item, VStack) },
	code_info__set_pushed_values(VStack).

	% `pop_stack' and `pop_temp' don't actually decrement the stack
	% pointer, they just decrement the push count.  The space will
	% be deallocated in the procedure epilogue.

code_info__pop_stack(empty) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 - 1 },
	code_info__set_push_count(Count),
	code_info__get_pushed_values(VStack0),
	{ stack__pop_det(VStack0, _, VStack) },
	code_info__set_pushed_values(VStack).

code_info__pop_temp(StackVar) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 - 1 },
	code_info__set_push_count(Count),
	code_info__get_pushed_values(VStack0),
	{ stack__pop_det(VStack0, _, VStack) },
	code_info__set_pushed_values(VStack),
	code_info__get_stackslot_count(NumSlots),
	{ Slot is Count0 + NumSlots },
	code_info__stack_variable(Slot, StackVar).

code_info__get_stack_top(StackVar) -->
	code_info__get_push_count(Count),
	code_info__get_stackslot_count(NumSlots),
	{ Slot is Count + NumSlots },
	code_info__stack_variable(Slot, StackVar).

%---------------------------------------------------------------------------%

code_info__get_variable_slot(Var, Slot) -->
	code_info__get_stack_slots(StackSlots),
	( { map__search(StackSlots, Var, SlotPrime) } ->
		{ Slot = SlotPrime }
	;
		code_info__variable_to_string(Var, Name),
		{ term__var_to_int(Var, Num) },
		{ string__int_to_string(Num, NumStr) },
		{ string__append_list([
			"code_info__get_variable_slot: variable `",
			Name, "' (", NumStr, ") not found"], Str) },
		{ error(Str) }
	).

code_info__max_slot(StackSlots, SlotCount) :-
	map__values(StackSlots, StackSlotList),
	code_info__max_slot_2(StackSlotList, 0, SlotCount).

:- pred code_info__max_slot_2(list(lval), int, int).
:- mode code_info__max_slot_2(in, in, out) is det.

code_info__max_slot_2([], Max, Max).
code_info__max_slot_2([L | Ls], Max0, Max) :-
	( L = stackvar(N) ->
		int__max(N, Max0, Max1)
	; L = framevar(N) ->
		int__max(N, Max0, Max1)
	;
		Max1 = Max0
	),
	code_info__max_slot_2(Ls, Max1, Max).

code_info__get_total_stackslot_count(NumSlots) -->
	code_info__get_stackslot_count(SlotsForVars),
	code_info__get_max_push_count(SlotsForTemps),
	{ NumSlots is SlotsForVars + SlotsForTemps }.

code_info__stack_variable(Num, Lval) -->
	code_info__get_proc_model(CodeModel),
	( { CodeModel = model_non } ->
		{ Num1 is Num - 1 },		% framevars start at zero
		{ Lval = framevar(Num1) }
	;
		{ Lval = stackvar(Num) }	% stackvars start at one
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
