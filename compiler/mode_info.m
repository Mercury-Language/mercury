%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mode_info.m.
% Main author: fjh.

% This file defines the mode_info data structure, which is used to hold
% the information we need during mode analysis.

%-----------------------------------------------------------------------------%

:- module mode_info.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data, instmap.
:- import_module prog_data, mode_errors, delay_info, mode_debug, (inst).
:- import_module inst_table.
:- import_module map, list, set, bool, assoc_list, std_util.

:- interface.

	% The mode_info data structure and access predicates.

	% XXX `side' is not used
:- type mode_context
	--->	call(	
			call_id,
			int		% argument number (offset so that
					% the real arguments start at number 1
					% whereas the type_info arguments
					% have numbers <= 0).
		)
	;	unify(
			unify_context,	% original source of the unification
			side		% LHS or RHS
		)
/**** Not yet used
	;	unify_arg(
			unify_context,
			side,
			cons_id,
			int
		)
****/
	;	uninitialized.

:- type side ---> left ; right.

:- type call_context
	--->	unify(unify_context)
	;	call(call_id).

:- type var_lock_reason
	--->	negation
	;	if_then_else
	;	lambda(pred_or_func)
	;	par_conj
	.

	% Specify how to process goals - using either
	% modes.m or unique_modes.m.
:- type how_to_check_goal
	--->    check_modes
	;       check_unique_modes
	.
	
	% Is mode analysis allowed to change which procedure of a predicate
	% is called. It may not change the called procedure after deforestation
	% has performed a generalisation step, since that could result
	% in selecting a less efficient mode, or one which doesn't even
	% have the same termination behaviour.
	% Also, when rechecking a goal after adding extra goals, it is
	% not necessary to choose again which procedure is to be called.
:- type may_change_called_proc
	--->	may_change_called_proc
	;	may_not_change_called_proc.

:- type locked_vars == assoc_list(var_lock_reason, set(prog_var)).

:- type mode_info.

:- pred mode_info_init(io__state, module_info, inst_table, pred_id, proc_id,
		prog_context, set(prog_var), instmap, how_to_check_goal,
		may_change_called_proc, mode_info).
:- mode mode_info_init(di, in, in, in, in, in, in, in, in, in,
		mode_info_uo) is det.

:- pred mode_info_get_io_state(mode_info, io__state).
:- mode mode_info_get_io_state(mode_info_get_io_state, uo) is det.

:- pred mode_info_set_io_state(mode_info, io__state, mode_info).
:- mode mode_info_set_io_state(mode_info_set_io_state, di, mode_info_uo) is det.

:- pred mode_info_get_module_info(mode_info, module_info).
:- mode mode_info_get_module_info(mode_info_ui, out) is det.

:- pred mode_info_set_inst_table(inst_table, mode_info, mode_info).
:- mode mode_info_set_inst_table(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_inst_table(mode_info, inst_table).
:- mode mode_info_get_inst_table(mode_info_ui, out) is det.

:- pred mode_info_dcg_get_inst_table(inst_table, mode_info, mode_info).
:- mode mode_info_dcg_get_inst_table(out, mode_info_di, mode_info_uo)
		is det.

:- pred mode_info_set_module_info(mode_info, module_info, mode_info).
:- mode mode_info_set_module_info(mode_info_di, in, mode_info_uo) is det.

:- pred mode_info_get_preds(mode_info, pred_table).
:- mode mode_info_get_preds(mode_info_ui, out) is det.

:- pred mode_info_get_modes(mode_info, mode_table).
:- mode mode_info_get_modes(mode_info_ui, out) is det.

:- pred mode_info_get_user_insts(mode_info, user_inst_table).
:- mode mode_info_get_user_insts(mode_info_ui, out) is det.

:- pred mode_info_get_predid(mode_info, pred_id).
:- mode mode_info_get_predid(mode_info_ui, out) is det.

:- pred mode_info_get_procid(mode_info, proc_id).
:- mode mode_info_get_procid(mode_info_ui, out) is det.

:- pred mode_info_get_context(mode_info, prog_context).
:- mode mode_info_get_context(mode_info_ui, out) is det.

:- pred mode_info_set_context(prog_context, mode_info, mode_info).
:- mode mode_info_set_context(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_mode_context(mode_info, mode_context).
:- mode mode_info_get_mode_context(mode_info_ui, out) is det.

:- pred mode_info_set_mode_context(mode_context, mode_info, mode_info).
:- mode mode_info_set_mode_context(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_call_context(call_context, mode_info, mode_info).
:- mode mode_info_set_call_context(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_call_arg_context(int, mode_info, mode_info).
:- mode mode_info_set_call_arg_context(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_unset_call_context(mode_info, mode_info).
:- mode mode_info_unset_call_context(mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_instmap(mode_info, instmap).
:- mode mode_info_get_instmap(mode_info_ui, out) is det.

:- pred mode_info_dcg_get_instmap(instmap, mode_info, mode_info).
:- mode mode_info_dcg_get_instmap(out, mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_instmap(instmap, mode_info, mode_info).
:- mode mode_info_set_instmap(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_locked_vars(mode_info, locked_vars).
:- mode mode_info_get_locked_vars(mode_info_ui, out) is det.

:- pred mode_info_set_locked_vars(mode_info, locked_vars, mode_info).
:- mode mode_info_set_locked_vars(mode_info_di, in, mode_info_uo) is det.

:- pred mode_info_get_errors(mode_info, list(mode_error_info)).
:- mode mode_info_get_errors(mode_info_ui, out) is det.

:- pred mode_info_get_num_errors(mode_info, int).
:- mode mode_info_get_num_errors(mode_info_ui, out) is det.

:- pred mode_info_set_errors(list(mode_error_info), mode_info, mode_info).
:- mode mode_info_set_errors(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_add_live_vars(set(prog_var), mode_info, mode_info).
:- mode mode_info_add_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_remove_live_vars(set(prog_var), mode_info, mode_info).
:- mode mode_info_remove_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_var_list_is_live(list(prog_var), mode_info, list(is_live)).
:- mode mode_info_var_list_is_live(in, mode_info_ui, out) is det.

:- pred mode_info_var_is_live(mode_info, prog_var, is_live).
:- mode mode_info_var_is_live(mode_info_ui, in, out) is det.

:- pred mode_info_var_is_nondet_live(mode_info, prog_var, is_live).
:- mode mode_info_var_is_nondet_live(mode_info_ui, in, out) is det.

:- pred mode_info_get_liveness(mode_info, set(prog_var)).
:- mode mode_info_get_liveness(mode_info_ui, out) is det.

:- pred mode_info_get_liveness_2(list(set(prog_var)), set(prog_var),
		set(prog_var)).
:- mode mode_info_get_liveness_2(in, in, out) is det.

:- pred mode_info_get_varset(mode_info, prog_varset).
:- mode mode_info_get_varset(mode_info_ui, out) is det.

:- pred mode_info_set_varset(prog_varset, mode_info, mode_info).
:- mode mode_info_set_varset(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_instvarset(mode_info, inst_varset).
:- mode mode_info_get_instvarset(mode_info_ui, out) is det.

:- pred mode_info_get_var_types(mode_info, map(prog_var, type)).
:- mode mode_info_get_var_types(mode_info_ui, out) is det.

:- pred mode_info_set_var_types(map(prog_var, type), mode_info, mode_info).
:- mode mode_info_set_var_types(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_types_of_vars(mode_info, list(prog_var), list(type)).
:- mode mode_info_get_types_of_vars(mode_info_ui, in, out) is det.

:- pred mode_info_lock_vars(var_lock_reason, set(prog_var),
		mode_info, mode_info).
:- mode mode_info_lock_vars(in, in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_unlock_vars(var_lock_reason, set(prog_var),
		mode_info, mode_info).
:- mode mode_info_unlock_vars(in, in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_var_is_locked(mode_info, prog_var, var_lock_reason).
:- mode mode_info_var_is_locked(mode_info_ui, in, out) is semidet.

:- pred mode_info_var_is_locked_2(locked_vars, prog_var, var_lock_reason).
:- mode mode_info_var_is_locked_2(in, in, out) is semidet.

:- pred mode_info_get_delay_info(mode_info, delay_info).
:- mode mode_info_get_delay_info(mode_info_no_io, out) is det.

:- pred mode_info_set_delay_info(delay_info, mode_info, mode_info).
:- mode mode_info_set_delay_info(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_live_vars(mode_info, list(set(prog_var))).
:- mode mode_info_get_live_vars(mode_info_ui, out) is det.

:- pred mode_info_set_live_vars(list(set(prog_var)), mode_info, mode_info).
:- mode mode_info_set_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_nondet_live_vars(mode_info, list(set(prog_var))).
:- mode mode_info_get_nondet_live_vars(mode_info_no_io, out) is det.

:- pred mode_info_set_nondet_live_vars(list(set(prog_var)),
		mode_info, mode_info).
:- mode mode_info_set_nondet_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_mode_debug_info(mode_info, mode_debug_info).
:- mode mode_info_get_mode_debug_info(mode_info_no_io, out) is det.

:- pred mode_info_set_mode_debug_info(mode_debug_info, mode_info, mode_info).
:- mode mode_info_set_mode_debug_info(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_last_checkpoint_insts(mode_info,
		assoc_list(prog_var, inst)).
:- mode mode_info_get_last_checkpoint_insts(mode_info_no_io, out) is det.

:- pred mode_info_set_last_checkpoint_insts(assoc_list(prog_var, inst),
		mode_info, mode_info).
:- mode mode_info_set_last_checkpoint_insts(in,
		mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_parallel_vars(list(pair(set(prog_var))), mode_info,
		mode_info).
:- mode mode_info_get_parallel_vars(out, mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_parallel_vars(list(pair(set(prog_var))), mode_info,
		mode_info).
:- mode mode_info_set_parallel_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_changed_flag(mode_info, bool).
:- mode mode_info_get_changed_flag(mode_info_no_io, out) is det.

:- pred mode_info_set_changed_flag(bool, mode_info, mode_info).
:- mode mode_info_set_changed_flag(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_bind_var_to_functor(prog_var, cons_id, mode_info, mode_info).
:- mode mode_info_bind_var_to_functor(in, in, mode_info_di, mode_info_uo)
	is det.

:- pred mode_info_get_how_to_check(mode_info, how_to_check_goal).
:- mode mode_info_get_how_to_check(mode_info_ui, out) is det.

:- pred mode_info_set_how_to_check(how_to_check_goal, mode_info, mode_info).
:- mode mode_info_set_how_to_check(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_may_change_called_proc(mode_info,
		may_change_called_proc).
:- mode mode_info_get_may_change_called_proc(mode_info_ui, out) is det.

:- pred mode_info_set_may_change_called_proc(may_change_called_proc,
		mode_info, mode_info).
:- mode mode_info_set_may_change_called_proc(in,
		mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_checking_extra_goals(bool, mode_info, mode_info).
:- mode mode_info_set_checking_extra_goals(in,
		mode_info_di, mode_info_uo) is det.

	% Find the simple_call_id to use in error messages
	% for the given pred_id.
:- pred mode_info_get_call_id(mode_info, pred_id, simple_call_id).
:- mode mode_info_get_call_id(mode_info_ui, in, out) is det.

/*
:- inst uniq_mode_info	=	bound_unique(
					mode_info(
						unique,
						ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).
*/
:- inst uniq_mode_info	=	ground.
:- inst dead_mode_info  =	ground.

:- mode mode_info_uo :: free -> uniq_mode_info.
:- mode mode_info_ui :: uniq_mode_info -> uniq_mode_info.
:- mode mode_info_di :: uniq_mode_info -> dead_mode_info.

	% Some fiddly modes used when we want to extract
	% the io_state from a mode_info struct and then put it back again.

/*
:- inst mode_info_no_io	=	bound_unique(
					mode_info(
						dead, ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).
*/
:- inst mode_info_no_io	=	ground.

:- mode mode_info_get_io_state	:: uniq_mode_info -> mode_info_no_io.
:- mode mode_info_no_io		:: mode_info_no_io -> mode_info_no_io.
:- mode mode_info_set_io_state	:: mode_info_no_io -> dead_mode_info.

%-----------------------------------------------------------------------------%

        % record a mode error (and associated context info) in the mode_info.

:- pred mode_info_error(set(prog_var), mode_error, mode_info, mode_info).
:- mode mode_info_error(in, in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_add_error(mode_error_info, mode_info, mode_info).
:- mode mode_info_add_error(in, mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%

:- pred instmap_sanity_check(instmap :: in, inst_table :: in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module delay_info, mode_errors, mode_util, (inst).
:- import_module term, varset.
:- import_module require, std_util, queue.

:- type mode_info 
	--->	mode_info(
			io__state,
			module_info,
			inst_table,
			pred_id,	% The pred we are checking
			proc_id,	% The mode which we are checking
			prog_varset,	% The variables in the current proc
			map(prog_var, type),
					% The types of the variables
			prog_context,	% The line number of the subgoal we
					% are currently checking
			mode_context,	% A description of where in the
					% goal the error occurred
			instmap,	% The current instantiatedness
					% of the variables
			locked_vars,	% The "locked" variables,
					% i.e. variables which cannot be
					% further instantiated inside a
					% negated context
			delay_info,	% info about delayed goals
			list(mode_error_info),
					% The mode errors found

			list(set(prog_var)),	% The live variables,
	% i.e. those variables which may be referenced again on forward
	% execution or after shallow backtracking.  (By shallow
	% backtracking, I mean semidet backtracking in a negation,
	% if-then-else, or semidet disjunction within the current
	% predicate.)

			list(set(prog_var)),
					% The nondet-live variables,
	% i.e. those variables which may be referenced again after deep
	% backtracking TO THE CURRENT EXECUTION POINT.  These are the
	% variables which need to be made mostly_unique rather than
	% unique when we get to a nondet disjunction or a nondet call.
	% We do not include variables which may be referenced again
	% after backtracking to a point EARLIER THAN the current
	% execution point, since those variables will *already* have
	% been marked as mostly_unique rather than unique.)

			mode_debug_info,
			assoc_list(prog_var, inst),
	% This field is used by the checkpoint code when debug_modes is on.
	% It contains any "context" which mode_info_checkpoint wishes to
	% preserve between checkpoints.
	% This field will always be small if debug_modes is off,
	% since its information is not needed then.

			list(pair(set(prog_var), set(prog_var))),
	% A stack of pairs of sets of variables used to mode-check
	% parallel conjunctions. The first set is the nonlocals of
	% the parallel conjunction. The second set is a subset of the
	% first, and is the set of variables that have been [further]
	% bound inside the current parallel conjunct - the stack is for
	% the correct handling of nested parallel conjunctions.

			bool,		% Changed flag
					% If `yes', then we may need
					% to repeat mode inference.

			how_to_check_goal,

			may_change_called_proc,
					% Is mode analysis allowed
					% to change which procedure
					% is called?

			bool		% Are we rechecking a goal after
					% introducing unifications for
					% complicated sub-unifications
					% or an implied mode?
					% If so, redoing the mode check
					% should not introduce more
					% extra unifications.
		).

	% The normal inst of a mode_info struct: ground, with
	% the io_state and the struct itself unique, but with
	% multiple references allowed for the other parts.

%-----------------------------------------------------------------------------%

	% Initialize the mode_info

mode_info_init(IOState, ModuleInfo, IKT, PredId, ProcId, Context,
		LiveVars, InstMapping0, HowToCheck, MayChangeProc, ModeInfo) :-
	mode_context_init(ModeContext),
	LockedVars = [],
	delay_info__init(DelayInfo),
	ErrorList = [],
		% look up the varset and var types
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),

	LiveVarsList = [LiveVars],
	NondetLiveVarsList = [LiveVars],
	mode_debug_info_init(ModeDebugInfo),

	Changed = no,

	CheckingExtraGoals = no,

	ModeInfo = mode_info(
		IOState, ModuleInfo, IKT, PredId, ProcId, VarSet, VarTypes,
		Context, ModeContext, InstMapping0, LockedVars, DelayInfo,
		ErrorList, LiveVarsList, NondetLiveVarsList,
		ModeDebugInfo, [], [], Changed, HowToCheck, MayChangeProc,
		CheckingExtraGoals
	).

%-----------------------------------------------------------------------------%

	% Lots of very boring access predicates.

mode_info_get_io_state(
		mode_info(IOState0, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		IOState) :-
	% XXX
	unsafe_promise_unique(IOState0, IOState).

%-----------------------------------------------------------------------------%

mode_info_set_io_state(mode_info(_,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V),
		IOState0, 
		mode_info(IOState,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)) :-
	% XXX
	unsafe_promise_unique(IOState0, IOState).

%-----------------------------------------------------------------------------%

mode_info_get_module_info(
	mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
	ModuleInfo).

%-----------------------------------------------------------------------------%

mode_info_set_module_info(
	mode_info(A,_,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V),
	ModuleInfo,
	mode_info(A,ModuleInfo,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)).

%-----------------------------------------------------------------------------%

mode_info_get_preds(
		mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		Preds) :-
	module_info_preds(ModuleInfo, Preds).

%-----------------------------------------------------------------------------%

mode_info_get_modes(
		mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		Modes) :-
	module_info_modes(ModuleInfo, Modes).

%-----------------------------------------------------------------------------%

mode_info_get_user_insts(
		mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		UserInsts) :-
	module_info_user_insts(ModuleInfo, UserInsts).

%-----------------------------------------------------------------------------%

mode_info_get_inst_table(
	mode_info(_,_,InstTable,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		InstTable).

mode_info_dcg_get_inst_table(InstTable, ModeInfo, ModeInfo) :-
	mode_info_get_inst_table(ModeInfo, InstTable).

%-----------------------------------------------------------------------------%

mode_info_set_inst_table(InstTable,
		mode_info(A,B,_,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V),
		mode_info(A,B,InstTable,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)).

%-----------------------------------------------------------------------------%

mode_info_get_predid(
		mode_info(_,_,_,PredId,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		PredId).

%-----------------------------------------------------------------------------%

mode_info_get_procid(
		mode_info(_,_,_,_,ProcId,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		ProcId).

%-----------------------------------------------------------------------------%

mode_info_get_varset(
		mode_info(_,_,_,_,_,VarSet,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		VarSet).

%-----------------------------------------------------------------------------%

mode_info_set_varset(VarSet,
		mode_info(A,B,C,D,E,_,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V),
		mode_info(A,B,C,D,E,VarSet,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)).

%-----------------------------------------------------------------------------%

mode_info_get_var_types(
		mode_info(_,_,_,_,_,_,VarTypes,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		VarTypes).

%-----------------------------------------------------------------------------%

mode_info_set_var_types(VarTypes,
		mode_info(A,B,C,D,E,F,_,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V),
		mode_info(A,B,C,D,E,F,VarTypes,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)).

%-----------------------------------------------------------------------------%

mode_info_get_context(
		mode_info(_,_,_,_,_,_,_,Context,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
		Context).

%-----------------------------------------------------------------------------%

mode_info_set_context(Context, 
		mode_info(A,B,C,D,E,F,G,_,I,J,K,L,M,N,O,P,Q,R,S,T,U,V),
		mode_info(A,B,C,D,E,F,G,Context,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)).

%-----------------------------------------------------------------------------%

mode_info_get_mode_context(
	mode_info(_,_,_,_,_,_,_,_,ModeContext,_,_,_,_,_,_,_,_,_,_,_,_,_),
	ModeContext).

%-----------------------------------------------------------------------------%

mode_info_set_mode_context(ModeContext,
	mode_info(A,B,C,D,E,F,G,H,_,J,K,L,M,N,O,P,Q,R,S,T,U,V),
	mode_info(A,B,C,D,E,F,G,H,ModeContext,J,K,L,M,N,O,P,Q,R,S,T,U,V)).

%-----------------------------------------------------------------------------%

mode_info_set_call_context(unify(UnifyContext)) -->
	mode_info_set_mode_context(unify(UnifyContext, left)).
mode_info_set_call_context(call(CallId)) -->
	mode_info_set_mode_context(call(CallId, 0)).

mode_info_set_call_arg_context(ArgNum, ModeInfo0, ModeInfo) :-
	mode_info_get_mode_context(ModeInfo0, ModeContext0),
	( ModeContext0 = call(CallId, _) ->
		mode_info_set_mode_context(call(CallId, ArgNum),
			ModeInfo0, ModeInfo)
	; ModeContext0 = unify(_UnifyContext, _Side) ->
		% This only happens when checking that the typeinfo variables
		% for polymorphic complicated unifications are ground.
		% For that case, we don't care about the ArgNum.
		ModeInfo = ModeInfo0
	;
		error("mode_info_set_call_arg_context")
	).

mode_info_unset_call_context -->
	mode_info_set_mode_context(uninitialized).

%-----------------------------------------------------------------------------%

mode_info_get_instmap(
		mode_info(_,_,_,_,_,_,_,_,_,InstMap,_,_,_,_,_,_,_,_,_,_,_,_),
		InstMap).

	% mode_info_dcg_get_instmap/3 is the same as mode_info_get_instmap/2
	% except that it's easier to use inside a DCG.

mode_info_dcg_get_instmap(InstMap, ModeInfo, ModeInfo) :-
	mode_info_get_instmap(ModeInfo, InstMap).

%-----------------------------------------------------------------------------%

mode_info_set_instmap( InstMap,
	mode_info(A,B,C,D,E,F,G,H,I,InstMap0,K,DelayInfo0,M,N,O,P,Q,R,S,T,U,V),
	mode_info(A,B,C,D,E,F,G,H,I,InstMap,K,DelayInfo,M,N,O,P,Q,R,S,T,U,V)) :-
	( instmap__is_unreachable(InstMap), instmap__is_reachable(InstMap0) ->
		delay_info__bind_all_vars(DelayInfo0, DelayInfo)
	;
		DelayInfo = DelayInfo0
	).

%-----------------------------------------------------------------------------%

mode_info_get_locked_vars(
	mode_info(_,_,_,_,_,_,_,_,_,_,LockedVars,_,_,_,_,_,_,_,_,_,_,_),
	LockedVars).

%-----------------------------------------------------------------------------%

mode_info_set_locked_vars(
	mode_info(A,B,C,D,E,F,G,H,I,J,_,L,M,N,O,P,Q,R,S,T,U,V),
	LockedVars,
	mode_info(A,B,C,D,E,F,G,H,I,J,LockedVars,L,M,N,O,P,Q,R,S,T,U,V)).

%-----------------------------------------------------------------------------%

mode_info_get_errors(
		mode_info(_,_,_,_,_,_,_,_,_,_,_,_,Errors,_,_,_,_,_,_,_,_,_),
		Errors).

%-----------------------------------------------------------------------------%

mode_info_get_num_errors(
		mode_info(_,_,_,_,_,_,_,_,_,_,_,_,Errors,_,_,_,_,_,_,_,_,_),
		NumErrors) :-
	list__length(Errors, NumErrors).

%-----------------------------------------------------------------------------%

mode_info_set_errors(Errors,
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,_,N,O,P,Q,R,S,T,U,V),
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,Errors,N,O,P,Q,R,S,T,U,V)).

%-----------------------------------------------------------------------------%

	% We keep track of the live variables and the nondet-live variables
	% a bag, represented as a list of sets of vars.
	% This allows us to easily add and remove sets of variables.
	% It's probably not maximally efficient.

	% Add a set of vars to the bag of live vars and
	% the bag of nondet-live vars.

mode_info_add_live_vars(NewLiveVars,
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,
			M,LiveVars0,NondetLiveVars0,P,Q,R,S,T,U,V),
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,
			M,LiveVars,NondetLiveVars,P,Q,R,S,T,U,V)) :-

	LiveVars = [NewLiveVars | LiveVars0],
	NondetLiveVars = [NewLiveVars | NondetLiveVars0].

	% Remove a set of vars from the bag of live vars and
	% the bag of nondet-live vars.

mode_info_remove_live_vars(OldLiveVars, ModeInfo0, ModeInfo) :-
	ModeInfo0 = mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,
				LiveVars0, NondetLiveVars0,P,Q,R,S,T,U,V),
	ModeInfo1 = mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,
				LiveVars, NondetLiveVars,P,Q,R,S,T,U,V),
	(
		list__delete_first(LiveVars0, OldLiveVars, LiveVars1),
		list__delete_first(NondetLiveVars0, OldLiveVars,
			NondetLiveVars1)
	->
		LiveVars = LiveVars1,
		NondetLiveVars = NondetLiveVars1
	;
		error("mode_info_remove_live_vars: failed")
	),
		% when a variable becomes dead, we may be able to wake
		% up a goal which is waiting on that variable
	set__to_sorted_list(OldLiveVars, VarList),
	mode_info_get_delay_info(ModeInfo1, DelayInfo0),
	delay_info__bind_var_list(VarList, DelayInfo0, DelayInfo),
	mode_info_set_delay_info(DelayInfo, ModeInfo1, ModeInfo).

	% Check whether a list of variables are live or not

mode_info_var_list_is_live([], _, []).
mode_info_var_list_is_live([Var | Vars], ModeInfo, [Live | Lives]) :-
	mode_info_var_is_live(ModeInfo, Var, Live),
	mode_info_var_list_is_live(Vars, ModeInfo, Lives).

	% Check whether a variable is live or not

mode_info_var_is_live(
	mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,LiveVarsList,_,_,_,_,_,_,_,_),
		Var, Result) :-
	(
		% some [LiveVars] 
		list__member(LiveVars, LiveVarsList),
		set__member(Var, LiveVars)
	->
		Result = live
	;
		Result = dead
	).

	% Check whether a variable is nondet_live or not.

mode_info_var_is_nondet_live(mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,
		NondetLiveVarsList,_,_,_,_,_,_,_), Var, Result) :-
	(
		% some [LiveVars] 
		list__member(LiveVars, NondetLiveVarsList),
		set__member(Var, LiveVars)
	->
		Result = live
	;
		Result = dead
	).

mode_info_get_liveness(
	mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,LiveVarsList,_,_,_,_,_,_,_,_),
	LiveVars) :-
	set__init(LiveVars0),
	mode_info_get_liveness_2(LiveVarsList, LiveVars0, LiveVars).

mode_info_get_liveness_2([], LiveVars, LiveVars).
mode_info_get_liveness_2([LiveVarsSet | LiveVarsList], LiveVars0, LiveVars) :-
	set__union(LiveVars0, LiveVarsSet, LiveVars1),
	mode_info_get_liveness_2(LiveVarsList, LiveVars1, LiveVars).

mode_info_get_live_vars(
	mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,LiveVarsList,_,_,_,_,_,_,_,_),
	LiveVarsList).

mode_info_set_live_vars(LiveVarsList,
	mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,_,O,P,Q,R,S,T,U,V),
	mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,LiveVarsList,O,P,Q,R,S,T,U,V)).

%-----------------------------------------------------------------------------%

	% Since we don't yet handle polymorphic modes, the inst varset
	% is always empty.

mode_info_get_instvarset(_ModeInfo, InstVarSet) :-
	varset__init(InstVarSet).

mode_info_get_types_of_vars(ModeInfo, Vars, TypesOfVars) :-
	mode_info_get_var_types(ModeInfo, VarTypes),
	map__apply_to_list(Vars, VarTypes, TypesOfVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The locked variables are stored as a stack 
	% of sets of variables.  A variable is locked if it is
	% a member of any of the sets.  To lock a set of vars, we just
	% push them on the stack, and to unlock a set of vars, we just
	% pop them off the stack.  The stack is implemented as a list.

mode_info_lock_vars(Reason, Vars, ModeInfo0, ModeInfo) :-
	mode_info_get_locked_vars(ModeInfo0, LockedVars),
	mode_info_set_locked_vars(ModeInfo0, [Reason - Vars | LockedVars],
			ModeInfo).

mode_info_unlock_vars(Reason, Vars, ModeInfo0, ModeInfo) :-
	mode_info_get_locked_vars(ModeInfo0, LockedVars0),
	(
		LockedVars0 = [Reason - TheseVars | LockedVars1],
		set__equal(TheseVars, Vars)
	->
		LockedVars = LockedVars1
	;
		error("mode_info_unlock_vars: some kind of nesting error")
	),
	mode_info_set_locked_vars(ModeInfo0, LockedVars, ModeInfo).

mode_info_var_is_locked(ModeInfo, Var, Reason) :-
	mode_info_get_locked_vars(ModeInfo, LockedVarsList),
	mode_info_var_is_locked_2(LockedVarsList, Var, Reason).

mode_info_var_is_locked_2([ThisReason - Set | Sets], Var, Reason) :-
	(
		set__member(Var, Set)
	->
		Reason = ThisReason
	;
		mode_info_var_is_locked_2(Sets, Var, Reason)
	).

mode_info_get_delay_info(
	mode_info(_,_,_,_,_,_,_,_,_,_,_,DelayInfo,_,_,_,_,_,_,_,_,_,_),
	DelayInfo).

mode_info_set_delay_info(DelayInfo,
	mode_info(A,B,C,D,E,F,G,H,I,J,K,_,M,N,O,P,Q,R,S,T,U,V),
	mode_info(A,B,C,D,E,F,G,H,I,J,K,DelayInfo,M,N,O,P,Q,R,S,T,U,V)).

mode_info_get_nondet_live_vars(mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,
			NondetLiveVars,_,_,_,_,_,_,_), NondetLiveVars).

mode_info_set_nondet_live_vars(NondetLiveVars,
	mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,_,P,Q,R,S,T,U,V),
	mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,NondetLiveVars,P,Q,R,S,T,U,V)).

mode_info_get_mode_debug_info(mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
		ModeDebugInfo,_,_,_,_,_,_), ModeDebugInfo).

mode_info_set_mode_debug_info(ModeDebugInfo,
			mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,_,Q,R,S,T,U,V),
			mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,
				ModeDebugInfo,Q,R,S,T,U,V)).

mode_info_get_last_checkpoint_insts(mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
		LastCheckpointInsts,_,_,_,_,_), LastCheckpointInsts).

mode_info_set_last_checkpoint_insts(LastCheckpointInsts,
			mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,_,R,S,T,U,V),
			mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,
				LastCheckpointInsts,R,S,T,U,V)).

mode_info_get_parallel_vars(PVars, ModeInfo, ModeInfo) :-
	ModeInfo = mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,PVars,_,_,_,_).

mode_info_set_parallel_vars(PVars,
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,_,S,T,U,V),
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,PVars,S,T,U,V)).

mode_info_get_changed_flag(
	mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,Changed,_,_,_),
	Changed).

mode_info_set_changed_flag(Changed,
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,_,T,U,V),
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Changed,T,U,V)).

mode_info_get_how_to_check(
		mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,How,_,_), How).

mode_info_set_how_to_check(How,
	mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,_,U,V),
	mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,How,U,V)).

mode_info_get_may_change_called_proc(
		mode_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,MayChange,_),
		MayChange).

mode_info_set_may_change_called_proc(MayChange,
	mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,_,V),
	mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,MayChange,V)).

mode_info_set_checking_extra_goals(Checking,
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Checking0,T,U,V),
		mode_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Checking,T,U,V))
		:-
	( Checking0 = yes, Checking = yes ->
		% This should never happen - once the extra goals are
		% introduced, rechecking the goal should not introduce
		% more extra goals.
		error(
		"mode analysis: rechecking extra goals adds more extra goals")
	;
		true
	).

%-----------------------------------------------------------------------------%

mode_info_get_call_id(ModeInfo, PredId, CallId) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_call_id(PredInfo, CallId).

%-----------------------------------------------------------------------------%

mode_info_error(Vars, ModeError, ModeInfo0, ModeInfo) :-
        mode_info_get_context(ModeInfo0, Context),
        mode_info_get_mode_context(ModeInfo0, ModeContext),
        mode_info_get_inst_table(ModeInfo0, InstTable),
        ModeErrorInfo = mode_error_info(Vars, ModeError, InstTable,
		Context, ModeContext),
        mode_info_add_error(ModeErrorInfo, ModeInfo0, ModeInfo).

mode_info_add_error(ModeErrorInfo, ModeInfo0, ModeInfo) :-
        mode_info_get_errors(ModeInfo0, Errors0),
        list__append(Errors0, [ModeErrorInfo], Errors),
        mode_info_set_errors(Errors, ModeInfo0, ModeInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

mode_info_bind_var_to_functor(Var, ConsId, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_inst_table(ModeInfo0, InstTable0),

	instmap__bind_var_to_functor(Var, ConsId, InstMap0, InstMap,
		InstTable0, InstTable, ModuleInfo0, ModuleInfo),

        mode_info_set_instmap(InstMap, ModeInfo0, ModeInfo1),
        mode_info_set_inst_table(InstTable, ModeInfo1, ModeInfo2),
	mode_info_set_module_info(ModeInfo2, ModuleInfo, ModeInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap_sanity_check(InstMap, InstTable) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	instmap__vars(InstMap, VarsSet),
	set__to_sorted_list(VarsSet, Vars),
	instmap__lookup_vars(Vars, InstMap, Insts),
	list__foldl(inst_keys_in_inst, Insts, [], IKs),
	list__map(inst_key_table_lookup(IKT), IKs, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
