%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
:- import_module hlds, map, list, varset, set.

:- interface.

:- type delay_info.		% defined in delay_info.m
:- type mode_error_info.	% defined in mode_errors.m

	% The mode_info data structure and access predicates.

	% XXX `side' is not used
:- type mode_context
	--->	call(	
			pred_call_id,	% pred name / arity
			int		% argument number
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
	;	call(pred_call_id).

:- type mode_info.

:- pred mode_info_init(io__state, module_info, pred_id, proc_id,
			term__context, set(var), instmap, mode_info).
:- mode mode_info_init(di, in, in, in, in, in, in, mode_info_uo) is det.

:- pred mode_info_get_io_state(mode_info, io__state).
:- mode mode_info_get_io_state(mode_info_get_io_state, uo) is det.

:- pred mode_info_set_io_state(mode_info, io__state, mode_info).
:- mode mode_info_set_io_state(mode_info_set_io_state, di, mode_info_uo) is det.

:- pred mode_info_get_module_info(mode_info, module_info).
:- mode mode_info_get_module_info(mode_info_ui, out) is det.

:- pred mode_info_set_module_info(mode_info, module_info, mode_info).
:- mode mode_info_set_module_info(mode_info_di, in, mode_info_uo) is det.

:- pred mode_info_get_preds(mode_info, pred_table).
:- mode mode_info_get_preds(mode_info_ui, out) is det.

:- pred mode_info_get_modes(mode_info, mode_table).
:- mode mode_info_get_modes(mode_info_ui, out) is det.

:- pred mode_info_get_insts(mode_info, inst_table).
:- mode mode_info_get_insts(mode_info_ui, out) is det.

:- pred mode_info_get_predid(mode_info, pred_id).
:- mode mode_info_get_predid(mode_info_ui, out) is det.

:- pred mode_info_get_procid(mode_info, proc_id).
:- mode mode_info_get_procid(mode_info_ui, out) is det.

:- pred mode_info_get_context(mode_info, term__context).
:- mode mode_info_get_context(mode_info_ui, out) is det.

:- pred mode_info_set_context(term__context, mode_info, mode_info).
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

:- pred mode_info_get_vars_instmap(mode_info, set(var), instmap).
:- mode mode_info_get_vars_instmap(mode_info_ui, in, out) is det.

:- pred mode_info_set_instmap(instmap, mode_info, mode_info).
:- mode mode_info_set_instmap(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_locked_vars(mode_info, list(set(var))).
:- mode mode_info_get_locked_vars(mode_info_ui, out) is det.

:- pred mode_info_set_locked_vars(mode_info, list(set(var)), mode_info).
:- mode mode_info_set_locked_vars(mode_info_di, in, mode_info_uo) is det.

:- pred mode_info_get_errors(mode_info, list(mode_error_info)).
:- mode mode_info_get_errors(mode_info_ui, out) is det.

:- pred mode_info_get_num_errors(mode_info, int).
:- mode mode_info_get_num_errors(mode_info_ui, out) is det.

:- pred mode_info_set_errors(list(mode_error_info), mode_info, mode_info).
:- mode mode_info_set_errors(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_add_live_vars(set(var), mode_info, mode_info).
:- mode mode_info_add_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_remove_live_vars(set(var), mode_info, mode_info).
:- mode mode_info_remove_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_var_list_is_live(list(var), mode_info, list(is_live)).
:- mode mode_info_var_list_is_live(in, mode_info_ui, out) is det.

:- pred mode_info_var_is_live(mode_info, var, is_live).
:- mode mode_info_var_is_live(mode_info_ui, in, out) is det.

:- pred mode_info_get_liveness(mode_info, set(var)).
:- mode mode_info_get_liveness(mode_info_ui, out) is det.

:- pred mode_info_get_liveness_2(list(set(var)), set(var), set(var)).
:- mode mode_info_get_liveness_2(in, in, out) is det.

:- pred mode_info_get_varset(mode_info, varset).
:- mode mode_info_get_varset(mode_info_ui, out) is det.

:- pred mode_info_set_varset(varset, mode_info, mode_info).
:- mode mode_info_set_varset(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_instvarset(mode_info, varset).
:- mode mode_info_get_instvarset(mode_info_ui, out) is det.

:- pred mode_info_get_var_types(mode_info, map(var,type)).
:- mode mode_info_get_var_types(mode_info_ui, out) is det.

:- pred mode_info_set_var_types(map(var, type), mode_info, mode_info).
:- mode mode_info_set_var_types(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_types_of_vars(mode_info, list(var), list(type)).
:- mode mode_info_get_types_of_vars(mode_info_ui, in, out) is det.

:- pred mode_info_lock_vars(set(var), mode_info, mode_info).
:- mode mode_info_lock_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_unlock_vars(set(var), mode_info, mode_info).
:- mode mode_info_unlock_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_var_is_locked(mode_info, var).
:- mode mode_info_var_is_locked(mode_info_ui, in) is semidet.

:- pred mode_info_var_is_locked_2(list(set(var)), var).
:- mode mode_info_var_is_locked_2(in, in) is semidet.

:- pred mode_info_get_delay_info(mode_info, delay_info).
:- mode mode_info_get_delay_info(mode_info_no_io, out) is det.

:- pred mode_info_set_delay_info(delay_info, mode_info, mode_info).
:- mode mode_info_set_delay_info(in, mode_info_di, mode_info_uo) is det.

/*
:- inst uniq_mode_info	=	bound_unique(
					mode_info(
						unique,
						ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).
*/
:- inst uniq_mode_info	=	ground.

:- mode mode_info_uo :: free -> uniq_mode_info.
:- mode mode_info_ui :: uniq_mode_info -> uniq_mode_info.
:- mode mode_info_di :: uniq_mode_info -> dead.

	% Some fiddly modes used when we want to extract
	% the io_state from a mode_info struct and then put it back again.

/*
:- inst mode_info_no_io	=	bound_unique(
					mode_info(
						dead, ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).
*/
:- inst mode_info_no_io	=	ground.

:- mode mode_info_get_io_state	:: uniq_mode_info -> mode_info_no_io.
:- mode mode_info_no_io		:: mode_info_no_io -> mode_info_no_io.
:- mode mode_info_set_io_state	:: mode_info_no_io -> dead.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, delay_info, mode_errors, prog_io.

:- type mode_info 
	--->	mode_info(
			io__state,
			module_info,
			pred_id,	% The pred we are checking
			proc_id,	% The mode which we are checking
			varset,		% The variables in the current proc
			map(var, type),	% The types of the variables
			term__context,	% The line number of the subgoal we
					% are currently checking
			mode_context,	% A description of where in the
					% goal the error occurred
			instmap,	% The current instantiatedness
					% of the variables
			list(set(var)),	% The "locked" variables,
					% i.e. variables which cannot be
					% further instantiated inside a
					% negated context
			delay_info,	% info about delayed goals
			list(mode_error_info),
					% The mode errors found
			list(set(var))	% The live variables
		).

	% The normal inst of a mode_info struct: ground, with
	% the io_state and the struct itself unique, but with
	% multiple references allowed for the other parts.

%-----------------------------------------------------------------------------%

	% Initialize the mode_info

mode_info_init(IOState, ModuleInfo, PredId, ProcId, Context, LiveVars,
		InstMapping0, ModeInfo) :-
	mode_context_init(ModeContext),
	LockedVars = [],
	delay_info__init(DelayInfo),
	ErrorList = [],
		% look up the varset and var types
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_variables(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),

	ModeInfo = mode_info(
		IOState, ModuleInfo, PredId, ProcId, VarSet, VarTypes,
		Context, ModeContext,
		InstMapping0, LockedVars, DelayInfo, ErrorList, [LiveVars]
	).

%-----------------------------------------------------------------------------%

	% Lots of very boring access predicates.

mode_info_get_io_state(mode_info(IOState0,_,_,_,_,_,_,_,_,_,_,_,_), IOState) :-
	% XXX
	copy(IOState0, IOState).

%-----------------------------------------------------------------------------%

mode_info_set_io_state( mode_info(_,B,C,D,E,F,G,H,I,J,K,L,M), IOState0,
			mode_info(IOState,B,C,D,E,F,G,H,I,J,K,L,M)) :-
	% XXX
	copy(IOState0, IOState).

%-----------------------------------------------------------------------------%

mode_info_get_module_info(mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_),
				ModuleInfo).

%-----------------------------------------------------------------------------%

mode_info_set_module_info(mode_info(A,_,C,D,E,F,G,H,I,J,K,L,M), ModuleInfo,
				mode_info(A,ModuleInfo,C,D,E,F,G,H,I,J,K,L,M)).

%-----------------------------------------------------------------------------%

mode_info_get_preds(mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_), Preds) :-
	module_info_preds(ModuleInfo, Preds).

%-----------------------------------------------------------------------------%

mode_info_get_modes(mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_), Modes) :-
	module_info_modes(ModuleInfo, Modes).

%-----------------------------------------------------------------------------%

mode_info_get_insts(mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_), Insts) :-
	module_info_insts(ModuleInfo, Insts).

%-----------------------------------------------------------------------------%

mode_info_get_predid(mode_info(_,_,PredId,_,_,_,_,_,_,_,_,_,_), PredId).

%-----------------------------------------------------------------------------%

mode_info_get_procid(mode_info(_,_,_,ProcId,_,_,_,_,_,_,_,_,_), ProcId).

%-----------------------------------------------------------------------------%

mode_info_get_varset(mode_info(_,_,_,_,VarSet,_,_,_,_,_,_,_,_), VarSet).

%-----------------------------------------------------------------------------%

mode_info_set_varset(VarSet, mode_info(A,B,C,D,_,F,G,H,I,J,K,L,M),
				mode_info(A,B,C,D,VarSet,F,G,H,I,J,K,L,M)).

%-----------------------------------------------------------------------------%

mode_info_get_var_types(mode_info(_,_,_,_,_,VarTypes,_,_,_,_,_,_,_), VarTypes).

%-----------------------------------------------------------------------------%

mode_info_set_var_types(VarTypes, mode_info(A,B,C,D,E,_,G,H,I,J,K,L,M),
				mode_info(A,B,C,D,E,VarTypes,G,H,I,J,K,L,M)).

%-----------------------------------------------------------------------------%

mode_info_get_context(mode_info(_,_,_,_,_,_,Context,_,_,_,_,_,_), Context).

%-----------------------------------------------------------------------------%

mode_info_set_context(Context, mode_info(A,B,C,D,E,F,_,H,I,J,K,L,M),
				mode_info(A,B,C,D,E,F,Context,H,I,J,K,L,M)).

%-----------------------------------------------------------------------------%

mode_info_get_mode_context(mode_info(_,_,_,_,_,_,_,ModeContext,_,_,_,_,_),
				ModeContext).

%-----------------------------------------------------------------------------%

mode_info_set_mode_context(ModeContext, mode_info(A,B,C,D,E,F,G,_,I,J,K,L,M),
				mode_info(A,B,C,D,E,F,G,ModeContext,I,J,K,L,M)).

%-----------------------------------------------------------------------------%

mode_info_set_call_context(unify(UnifyContext)) -->
	mode_info_set_mode_context(unify(UnifyContext, left)).
mode_info_set_call_context(call(PredId)) -->
	mode_info_set_mode_context(call(PredId, 0)).

mode_info_set_call_arg_context(ArgNum, ModeInfo0, ModeInfo) :-
	mode_info_get_mode_context(ModeInfo0, ModeContext0),
	( ModeContext0 = call(PredId, _) ->
		mode_info_set_mode_context(call(PredId, ArgNum),
			ModeInfo0, ModeInfo)
	;
		error("mode_info_set_call_arg_context")
	).

mode_info_unset_call_context -->
	mode_info_set_mode_context(uninitialized).

%-----------------------------------------------------------------------------%

mode_info_get_instmap(mode_info(_,_,_,_,_,_,_,_,InstMap,_,_,_,_), InstMap).

	% mode_info_dcg_get_instmap/3 is the same as mode_info_get_instmap/2
	% except that it's easier to use inside a DCG.

mode_info_dcg_get_instmap(InstMap, ModeInfo, ModeInfo) :-
	mode_info_get_instmap(ModeInfo, InstMap).

	% mode_info_get_vars_instmap/3 is the same as mode_info_get_instmap/2
	% except that the map it returns might only contain the specified
	% variables if that would be more efficient; currently it's not,
	% so the two are just the same, but if we were to change the
	% data structures...

mode_info_get_vars_instmap(ModeInfo, _Vars, InstMap) :-
	mode_info_get_instmap(ModeInfo, InstMap).

%-----------------------------------------------------------------------------%

mode_info_set_instmap( InstMap, mode_info(A,B,C,D,E,F,G,H,_,J,K,L,M),
			mode_info(A,B,C,D,E,F,G,H,InstMap,J,K,L,M)).

%-----------------------------------------------------------------------------%

mode_info_get_locked_vars(mode_info(_,_,_,_,_,_,_,_,_,LockedVars,_,_,_),
		LockedVars).

%-----------------------------------------------------------------------------%

mode_info_set_locked_vars( mode_info(A,B,C,D,E,F,G,H,I,_,K,L,M), LockedVars,
			mode_info(A,B,C,D,E,F,G,H,I,LockedVars,K,L,M)).

%-----------------------------------------------------------------------------%

mode_info_get_errors(mode_info(_,_,_,_,_,_,_,_,_,_,_,Errors,_), Errors).

%-----------------------------------------------------------------------------%

mode_info_get_num_errors(mode_info(_,_,_,_,_,_,_,_,_,_,_,Errors,_), NumErrors)
		:-
	list__length(Errors, NumErrors).

%-----------------------------------------------------------------------------%

mode_info_set_errors( Errors, mode_info(A,B,C,D,E,F,G,H,I,J,K,_,M), 
			mode_info(A,B,C,D,E,F,G,H,I,J,K,Errors,M)).

%-----------------------------------------------------------------------------%

	% We keep track of the live variables as a bag, represented
	% as a list of sets of vars.
	% This allows us to easily add and remove sets of variables.
	% It's probably not maximally efficient.

	% Add a set of vars to the bag of live vars.

mode_info_add_live_vars(NewLiveVars,
			mode_info(A,B,C,D,E,F,G,H,I,J,K,L,LiveVars0),
			mode_info(A,B,C,D,E,F,G,H,I,J,K,L,LiveVars)) :-
	LiveVars = [NewLiveVars | LiveVars0].

	% Remove a set of vars from the bag of live vars.

mode_info_remove_live_vars(OldLiveVars, ModeInfo0, ModeInfo) :-
	ModeInfo0 = mode_info(A,B,C,D,E,F,G,H,I,J,K,L,LiveVars0),
	ModeInfo1 = mode_info(A,B,C,D,E,F,G,H,I,J,K,L,LiveVars),
	( list__delete_first(LiveVars0, OldLiveVars, LiveVars1) ->
		LiveVars = LiveVars1
	;
		error("mode_info_remove_live_vars: delete_first failed")
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

mode_info_var_is_live(mode_info(_,_,_,_,_,_,_,_,_,_,_,_,LiveVarsList), Var,
		Result) :-
	(
		% some [LiveVars] 
		list__member(LiveVars, LiveVarsList),
		set__member(Var, LiveVars)
	->
		Result = live
	;
		Result = dead
	).

mode_info_get_liveness(mode_info(_,_,_,_,_,_,_,_,_,_,_,_,LiveVarsList),
		LiveVars) :-
	set__init(LiveVars0),
	mode_info_get_liveness_2(LiveVarsList, LiveVars0, LiveVars).

mode_info_get_liveness_2([], LiveVars, LiveVars).
mode_info_get_liveness_2([LiveVarsSet | LiveVarsList], LiveVars0, LiveVars) :-
	set__union(LiveVars0, LiveVarsSet, LiveVars1),
	mode_info_get_liveness_2(LiveVarsList, LiveVars1, LiveVars).

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

mode_info_lock_vars(Vars, ModeInfo0, ModeInfo) :-
	mode_info_get_locked_vars(ModeInfo0, LockedVars),
	mode_info_set_locked_vars(ModeInfo0, [Vars | LockedVars], ModeInfo).

mode_info_unlock_vars(_, ModeInfo0, ModeInfo) :-
	mode_info_get_locked_vars(ModeInfo0, LockedVars0),
	( LockedVars0 = [_ | LockedVars1] ->
		LockedVars = LockedVars1
	;
		error("mode_info_unlock_vars: stack is empty")
	),
	mode_info_set_locked_vars(ModeInfo0, LockedVars, ModeInfo).

mode_info_var_is_locked(ModeInfo, Var) :-
	mode_info_get_locked_vars(ModeInfo, LockedVarsList),
	mode_info_var_is_locked_2(LockedVarsList, Var).

mode_info_var_is_locked_2([Set | Sets], Var) :-
	(
		set__member(Var, Set)
	->
		true
	;
		mode_info_var_is_locked_2(Sets, Var)
	).

mode_info_get_delay_info(mode_info(_,_,_,_,_,_,_,_,_,_,DelayInfo,_,_),
	DelayInfo).

mode_info_set_delay_info(DelayInfo, mode_info(A,B,C,D,E,F,G,H,I,J,_,L,M),
			mode_info(A,B,C,D,E,F,G,H,I,J,DelayInfo,L,M)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
