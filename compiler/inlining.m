%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.

:- module inlining.

	% This module inlines
	%
	%	* (--inline-simple and --inline-simple-threshold N)
	%	  procedures whose size is below the given threshold,
	%	  PLUS	  
	%	  procedures that are flat (ie contain no branched structures)
	%	  and are composed of inline builtins (eg arithmetic),
	%	  and whose size is less than three times the given threshold
	%	  (XXX shouldn't hard-code 3)
	%
	%	* (--inline-compound-threshold N)
	%	  procedures where the product of the number of calls to them
	%	  and their size is below a given threshold.
	%
	%	* (--inline-single-use)
	%	  procedures which are called only once
	%
	% 	* procedures which have a `:- pragma inline(name/arity).'
	%
	% It will not inline procedures which have a 
	% 	`:- pragma no_inline(name/arity).'
	%
	% If inlining a procedure takes the total number of variables over
	% a given threshold (from a command-line option), then the procedure
	% is not inlined - note that this means that some calls to a
	% procedure may inlined while others are not.
	%
	% It builds the call-graph (if necessary) works from the bottom of
	% the call-graph towards the top, first perfoming inlining on a
	% procedure then deciding if calls to it (higher in the call-graph)
	% should be inlined. SCCs get flattend and processed in the order
	% returned by hlds_dependency_info_get_dependency_ordering.
	%
	% There are a couple of classes of procedure that we clearly want
	% to inline because doing so *reduces* the size of the generated
	% code:
	%
	%	- access predicates that get or set one or more fields
	%	  of a structure (Inlining these is almost always a win
	%	  because the infrastructure for the call to the procedure
	%	  is almost always larger than the code to do the access.
	%	  In the case of `get' accessors, the call usually becomes
	%	  a single `field' expression to get the relevant field of
	%	  the structure. In the case of `set' accessors, it is a bit
	%	  more complicated since the code to copy the fields can be
	%	  quite big if there are lots of fields, however in the case
	%	  where several `set' accessors get called one after the other,
	%	  inlining them enables the code generator to avoid creating
	%	  the intermediate structures which is often a win).
	%
	%	- arithmetic predicates where the as above, the cost of the
	%	  call will often outweigh the cost of the arithmetic.
	%
	%	- det or semi pragma C code, where often the C operation is
	%	  very small, inlining avoids a call and allows the C compiler
	%	  to do a better job of optimizing it.
	%
	% The threshold on the size of simple goals (which covers both of the
	% first two cases above), is to prevent the inlining of large goals
	% such as those that construct big terms where the duplication is
	% usually inappropriate (for example in nrev).
	%
	% The threshold on the number of variables in a procedure is to prevent
	% the problem of inlining lots of calls and having a resulting
	% procedure with so many variables that the back end of the compiler
	% gets bogged down (for example in the pseudoknot benchmark).
	%
	% Due to the way in which we generate code for model_non pragma_c_code,
	% procedures whose body is such a pragma_c_code must NOT be inlined.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module hlds_module.
:- import_module io.

:- pred inlining(module_info, module_info, io__state, io__state).
:- mode inlining(in, out, di, uo) is det.

:- pred inlining__is_simple_goal(hlds_goal, int).
:- mode inlining__is_simple_goal(in, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, hlds_goal, globals, options, llds.
:- import_module dead_proc_elim, type_util, mode_util, goal_util.
:- import_module passes_aux, code_aux, quantification, det_analysis, prog_data.

:- import_module bool, int, list, assoc_list, map, set, std_util.
:- import_module term, varset, require, hlds_data, dependency_graph.

%-----------------------------------------------------------------------------%

:- type inline_params	--->	params(bool, bool, int, int, int).
				% simple, single_use,
				%	size-threshold, simple-goal-threshold
				%		var-threshold

inlining(ModuleInfo0, ModuleInfo) -->
		%
		% Package up all the inlining options
		% - whether to inline simple conj's of builtins
		% - whether to inline predicates that are
		%   only called once
		% - the threshold for determining whether to
		%   inline more complicated goals
		% - the threshold for determining whether to
		%   inline the simple conj's
		% - the upper limit on the number of variables
		%   we want in procedures - if inlining a procedure
		%   would cause the number of variables to exceed
		%   this threshold then we don't inline it.
		%
	globals__io_lookup_bool_option(inline_simple, Simple),
	globals__io_lookup_bool_option(inline_single_use, SingleUse),
	globals__io_lookup_int_option(inline_compound_threshold,
							CompoundThreshold),
	globals__io_lookup_int_option(inline_simple_threshold, SimpleThreshold),
	globals__io_lookup_int_option(inline_vars_threshold, VarThreshold),
	{ Params = params(Simple, SingleUse, CompoundThreshold,
			SimpleThreshold, VarThreshold) },

		%
		% Get the usage counts for predicates
		% (but only if needed, i.e. only if --inline-single-use
		% or --inline-compound-threshold has been specified)
		%
	(
		( { SingleUse = yes }
		; { CompoundThreshold > 0 }
		)
	->
		{ dead_proc_elim__analyze(ModuleInfo0, NeededMap) }
	;
		{ map__init(NeededMap) }
	),

		% build the call graph and extract the topological sort
		% Note: the topological sort returns a list of SCCs.
		% Clearly, we want to process the SCCs bottom to top
		% (which is the order that they are returned), but it
		% is not easy to guess the best way to flatten each SCC
		% to achieve the best result. The current implementation
		% just uses the ordering of the list returned by the
		% topological sort. A more sophisticated approach would be
		% to break the cycle so that the procedure(s) that are called
		% by higher SCCs are processed last, but we do not implement
		% that yet.
	{ module_info_ensure_dependency_info(ModuleInfo0, ModuleInfo1) },
	{ module_info_dependency_info(ModuleInfo1, DepInfo) },
	{ hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs) },
	{ list__condense(SCCs, PredProcs) },
	{ set__init(InlinedProcs0) },
	inlining__do_inlining(PredProcs, NeededMap, Params, InlinedProcs0,
		ModuleInfo1, ModuleInfo).

:- pred inlining__do_inlining(list(pred_proc_id), needed_map, inline_params,
		set(pred_proc_id), module_info, module_info,
		io__state, io__state).
:- mode inlining__do_inlining(in, in, in, in, in, out, di, uo) is det.

inlining__do_inlining([], _Needed, _Params, _Inlined, Module, Module) --> [].
inlining__do_inlining([PPId|PPIds], Needed, Params, Inlined0,
		Module0, Module) -->
	inlining__in_predproc(PPId, Inlined0, Params, Module0, Module1),
	inlining__mark_predproc(PPId, Needed, Params, Module1,
		Inlined0, Inlined1),
	inlining__do_inlining(PPIds, Needed, Params, Inlined1, Module1, Module).

:- pred inlining__mark_predproc(pred_proc_id, needed_map, inline_params,
		module_info, set(pred_proc_id), set(pred_proc_id),
		io__state, io__state).
:- mode inlining__mark_predproc(in, in, in, in, in, out, di, uo) is det.

inlining__mark_predproc(PredProcId, NeededMap, Params, ModuleInfo, 
		InlinedProcs0, InlinedProcs) -->
	(
		{ Params = params(Simple, SingleUse, CompoundThreshold,
				SimpleThreshold, _VarThreshold) },
		{ PredProcId = proc(PredId, ProcId) },
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		{ pred_info_procedures(PredInfo, Procs) },
		{ map__lookup(Procs, ProcId, ProcInfo) },
		{ proc_info_goal(ProcInfo, CalledGoal) },
		{ Entity = proc(PredId, ProcId) },
	%
	% the heuristic represented by the following code
	% could be improved
	%
		(
			{ Simple = yes },
			{ inlining__is_simple_goal(CalledGoal,
				SimpleThreshold) }
		;
			{ CompoundThreshold > 0 },
			{ map__search(NeededMap, Entity, Needed) },
			{ Needed = yes(NumUses) },
			{ goal_size(CalledGoal, Size) },
			{ Size * NumUses =< CompoundThreshold }
		;
			{ SingleUse = yes },
			{ map__search(NeededMap, Entity, Needed) },
			{ Needed = yes(NumUses) },
			{ NumUses = 1 }
		),
		% Don't inline recursive predicates
		{ \+ goal_calls(CalledGoal, PredProcId) },

		% Don't inline model_non pragma c that doesn't have an
		% `extra_pragma_info'.  
		%
		% XXX  model_non pragma c without `extra_pragma_info' should
		% not be accepted by the compiler, but at the moment it's
		% the only way to get model_non pragma c (the ``correct''
		% way of doing it hasn't been implemented yet).  We just
		% have to make sure it doesn't get inlined because that stops
		% it from working.
		\+ {
			CalledGoal = pragma_c_code(_,_,_,_,_,_,_,none) - _,
			proc_info_interface_code_model(ProcInfo, model_non)
		}
	->
		inlining__mark_proc_as_inlined(PredProcId, ModuleInfo,
			InlinedProcs0, InlinedProcs)
	;
		{ InlinedProcs = InlinedProcs0 }
	).

	% this heuristic is used for both local and intermodule inlining

inlining__is_simple_goal(CalledGoal, SimpleThreshold) :-
	goal_size(CalledGoal, Size),
	(
		Size < SimpleThreshold
	;
		%
		% For flat goals, we are more likely to be able to
		% optimize stuff away, so we use a higher threshold.
		% XXX this should be a separate option, we shouldn't
		% hardcode the number `3' (which is just a guess).
		%
		Size < SimpleThreshold * 3,
		inlining__is_flat_simple_goal(CalledGoal)
	).

:- pred inlining__is_flat_simple_goal(hlds_goal::in) is semidet.

inlining__is_flat_simple_goal(conj(Goals) - _) :-
	inlining__is_flat_simple_goal_list(Goals).
inlining__is_flat_simple_goal(not(Goal) - _) :-
	inlining__is_flat_simple_goal(Goal).
inlining__is_flat_simple_goal(some(_, Goal) - _) :-
	inlining__is_flat_simple_goal(Goal).
inlining__is_flat_simple_goal(call(_, _, _, BuiltinState, _, _) - _) :-
	BuiltinState = inline_builtin.
inlining__is_flat_simple_goal(unify(_, _, _, _, _) - _).

:- pred inlining__is_flat_simple_goal_list(hlds_goals::in) is semidet.

inlining__is_flat_simple_goal_list([]).
inlining__is_flat_simple_goal_list([Goal | Goals]) :-
	inlining__is_flat_simple_goal(Goal),
	inlining__is_flat_simple_goal_list(Goals).

:- pred inlining__mark_proc_as_inlined(pred_proc_id, module_info,
	set(pred_proc_id), set(pred_proc_id), io__state, io__state).
:- mode inlining__mark_proc_as_inlined(in, in, in, out, di, uo) is det.

inlining__mark_proc_as_inlined(proc(PredId, ProcId), ModuleInfo,
		InlinedProcs0, InlinedProcs) -->
	{ set__insert(InlinedProcs0, proc(PredId, ProcId), InlinedProcs) },
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	( { pred_info_requested_inlining(PredInfo) } ->
		[]
	;
		write_proc_progress_message("% Inlining ", PredId, ProcId,
			ModuleInfo)
	).

%-----------------------------------------------------------------------------%

		% inline_info contains the information that is changed
		% as a result of inlining. It is threaded through the
		% inlining process, and when finished, contains the
		% updated information associated with the new goal.
		%
		% It also stores some necessary information that is not
		% updated.

:- type inline_info	--->

	inline_info(
		int,			% variable threshold for inlining
		set(pred_proc_id),	% inlined procs
		module_info,		% module_info

			% the following fields are updated as a result
			% of inlining
		varset,			% varset
		map(var, type),		% variable types
		tvarset,		% type variables
		map(tvar, var),		% type_info varset, a mapping from 
					% type variables to variables
					% where their type_info is
					% stored.
		bool			% Did we change the determinism
					% of any subgoal?
		).

:- pred inlining__in_predproc(pred_proc_id, set(pred_proc_id), inline_params,
		module_info, module_info, io__state, io__state).
:- mode inlining__in_predproc(in, in, in, in, out, di, uo) is det.

inlining__in_predproc(PredProcId, InlinedProcs, Params,
		ModuleInfo0, ModuleInfo, IoState0, IoState) :-
	Params = params(_Simple, _SingleUse, _CompoundThreshold,
			_SimpleThreshold, VarThresh),
	PredProcId = proc(PredId, ProcId),

	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	pred_info_typevarset(PredInfo0, TypeVarSet0),

	proc_info_goal(ProcInfo0, Goal0),
	proc_info_variables(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_typeinfo_varmap(ProcInfo0, TypeInfoVarMap0),

	DetChanged0 = no,

	InlineInfo0 = inline_info(VarThresh, InlinedProcs, ModuleInfo0,
		VarSet0, VarTypes0, TypeVarSet0, TypeInfoVarMap0, DetChanged0),

	inlining__inlining_in_goal(Goal0, Goal, InlineInfo0, InlineInfo),

	InlineInfo = inline_info(_, _, _, VarSet, VarTypes, TypeVarSet, 
		TypeInfoVarMap, DetChanged),

	pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo1),

	proc_info_set_variables(ProcInfo0, VarSet, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_typeinfo_varmap(ProcInfo2, TypeInfoVarMap, ProcInfo3),
	proc_info_set_goal(ProcInfo3, Goal, ProcInfo),

	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo1, ProcTable, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),

		% Re-run determinism analysis if we have to.
	( DetChanged = yes,	
		globals__io_get_globals(Globals, IoState0, IoState),
		det_infer_proc(PredId, ProcId, ModuleInfo1, ModuleInfo,
			Globals, _, _, _)
	; DetChanged = no,
		ModuleInfo = ModuleInfo1,
		IoState = IoState0
	).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_goal(hlds_goal, hlds_goal, inline_info,
		inline_info).
:- mode inlining__inlining_in_goal(in, out, in, out) is det.

inlining__inlining_in_goal(Goal0 - GoalInfo, Goal - GoalInfo) -->
	inlining__inlining_in_goal_2(Goal0, GoalInfo, Goal).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_goal_2(hlds_goal_expr, hlds_goal_info,
		hlds_goal_expr, inline_info, inline_info).
:- mode inlining__inlining_in_goal_2(in, in, out, in, out) is det.

inlining__inlining_in_goal_2(conj(Goals0), _GoalInfo, conj(Goals)) -->
	inlining__inlining_in_conj(Goals0, Goals).

inlining__inlining_in_goal_2(disj(Goals0, SM), _GoalInfo, disj(Goals, SM)) -->
	inlining__inlining_in_disj(Goals0, Goals).

inlining__inlining_in_goal_2(switch(Var, Det, Cases0, SM), _GoalInfo,
		switch(Var, Det, Cases, SM)) -->
	inlining__inlining_in_cases(Cases0, Cases).

inlining__inlining_in_goal_2(
		if_then_else(Vars, Cond0, Then0, Else0, SM), _GoalInfo,
		if_then_else(Vars, Cond, Then, Else, SM)) -->
	inlining__inlining_in_goal(Cond0, Cond),
	inlining__inlining_in_goal(Then0, Then),
	inlining__inlining_in_goal(Else0, Else).

inlining__inlining_in_goal_2(not(Goal0), _GoalInfo, not(Goal)) -->
	inlining__inlining_in_goal(Goal0, Goal).

inlining__inlining_in_goal_2(some(Vars, Goal0), _GoalInfo, some(Vars, Goal)) -->
	inlining__inlining_in_goal(Goal0, Goal).

inlining__inlining_in_goal_2(
		call(PredId, ProcId, ArgVars, Builtin, Context, Sym),
		GoalInfo0, Goal, InlineInfo0, InlineInfo) :-

	InlineInfo0 = inline_info(VarThresh, InlinedProcs, ModuleInfo,
		VarSet0, VarTypes0, TypeVarSet0, TypeInfoVarMap0,
		DetChanged0),

	% should we inline this call?
	(
		inlining__should_inline_proc(PredId, ProcId, Builtin,
				InlinedProcs, ModuleInfo),
			% okay, but will we exceed the number-of-variables
			% threshold?
		varset__vars(VarSet0, ListOfVars),
		list__length(ListOfVars, ThisMany),
			% We need to find out how many variables the
			% Callee has
		module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			PredInfo, ProcInfo),
        	proc_info_variables(ProcInfo, CalleeVarset),
		varset__vars(CalleeVarset, CalleeListOfVars),
		list__length(CalleeListOfVars, CalleeThisMany),
		TotalVars is ThisMany + CalleeThisMany,
		TotalVars =< VarThresh
	->
		% Yes.  So look up the rest of the info for the
		% called procedure.

		pred_info_typevarset(PredInfo, CalleeTypeVarSet),
		proc_info_headvars(ProcInfo, HeadVars),
		proc_info_goal(ProcInfo, CalledGoal),
		proc_info_vartypes(ProcInfo, CalleeVarTypes0),
		proc_info_typeinfo_varmap(ProcInfo, CalledTypeInfoVarMap0),

		% Substitute the appropriate types into the type
		% mapping of the called procedure.  For example, if we
		% call `:- pred foo(T)' with an argument of type
		% `int', then we need to replace all occurrences of
		% type `T' with type `int' when we inline it.

		% first, rename apart the type variables in the callee.
		% (we can almost throw away the new typevarset, since we
		% are about to substitute away any new type variables,
		% but any unbound type variables in the callee will not
		% be substituted away)

		varset__merge_subst(TypeVarSet0, CalleeTypeVarSet,
			TypeVarSet, TypeRenaming),
		apply_substitution_to_type_map(CalleeVarTypes0, TypeRenaming,
			CalleeVarTypes1),

		% next, compute the type substitution and then apply it

		map__apply_to_list(HeadVars, CalleeVarTypes1, HeadTypes),
		map__apply_to_list(ArgVars, VarTypes0, ArgTypes),
		(
			type_list_subsumes(HeadTypes, ArgTypes, TypeSubn)
		->
			apply_rec_substitution_to_type_map(CalleeVarTypes1,
				TypeSubn, CalleeVarTypes)
		;
			% The head types should always subsume the
			% actual argument types, otherwise it is a type error
			% that should have been detected by typechecking
			% But polymorphism.m introduces type-incorrect code --
			% e.g. compare(Res, EnumA, EnumB) gets converted
			% into builtin_compare_int(Res, EnumA, EnumB), which
			% is a type error since it assumes that an enumeration
			% is an int.  In those cases, we don't need to
			% worry about the type substitution.
			CalleeVarTypes = CalleeVarTypes1
		),

		% Now rename apart the variables in the called goal.

		map__from_corresponding_lists(HeadVars, ArgVars, Subn0),
		goal_util__create_variables(CalleeListOfVars, VarSet0,
			VarTypes0, Subn0, CalleeVarTypes, CalleeVarset,
				VarSet, VarTypes, Subn),
		goal_util__must_rename_vars_in_goal(CalledGoal, Subn,
			Goal - GoalInfo1),

			% If the inferred determinism of the called
			% goal differs from the declared determinism,
			% flag that we must re-run determinism checking
			% on this proc.
		goal_info_get_determinism(GoalInfo0, Determinism0),
		goal_info_get_determinism(GoalInfo1, Determinism1),
		( Determinism0 = Determinism1 ->
			DetChanged = DetChanged0
		;
			DetChanged = yes
		),

		apply_substitutions_to_var_map(CalledTypeInfoVarMap0, 
			TypeRenaming, Subn, CalledTypeInfoVarMap1),
		map__merge(TypeInfoVarMap0, CalledTypeInfoVarMap1,
			TypeInfoVarMap)
	;
		Goal = call(PredId, ProcId, ArgVars, Builtin, Context, Sym),
		VarSet = VarSet0,
		VarTypes = VarTypes0,
		TypeVarSet = TypeVarSet0,
		TypeInfoVarMap = TypeInfoVarMap0,
		DetChanged = DetChanged0
	),
	InlineInfo = inline_info(VarThresh, InlinedProcs, ModuleInfo,
		VarSet, VarTypes, TypeVarSet, TypeInfoVarMap, DetChanged).

inlining__inlining_in_goal_2(higher_order_call(A, B, C, D, E), _GoalInfo,
		higher_order_call(A, B, C, D, E)) --> [].

inlining__inlining_in_goal_2(unify(A, B, C, D, E), _GoalInfo,
		unify(A, B, C, D, E)) --> [].

inlining__inlining_in_goal_2(pragma_c_code(A, B, C, D, E, F, G, H), _GoalInfo,
		pragma_c_code(A, B, C, D, E, F, G, H)) --> [].

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_disj(list(hlds_goal), list(hlds_goal), 
		inline_info, inline_info).
:- mode inlining__inlining_in_disj(in, out, in, out) is det.

inlining__inlining_in_disj([], []) --> [].
inlining__inlining_in_disj([Goal0 | Goals0], [Goal | Goals]) -->
	inlining__inlining_in_goal(Goal0, Goal),
	inlining__inlining_in_disj(Goals0, Goals).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_cases(list(case), list(case), inline_info, 
		inline_info).
:- mode inlining__inlining_in_cases(in, out, in, out) is det.

inlining__inlining_in_cases([], []) --> [].
inlining__inlining_in_cases([case(Cons, Goal0) | Goals0],
		[case(Cons, Goal) | Goals]) -->
	inlining__inlining_in_goal(Goal0, Goal),
	inlining__inlining_in_cases(Goals0, Goals).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_conj(list(hlds_goal), list(hlds_goal),
		inline_info, inline_info).
:- mode inlining__inlining_in_conj(in, out, in, out) is det.

	% Since a single goal may become a conjunction,
	% we flatten the conjunction as we go.

inlining__inlining_in_conj([], []) --> [].
inlining__inlining_in_conj([Goal0 | Goals0], Goals) -->
	inlining__inlining_in_goal(Goal0, Goal1),
	{ goal_to_conj_list(Goal1, Goal1List) },
	inlining__inlining_in_conj(Goals0, Goals1),
	{ list__append(Goal1List, Goals1, Goals) }.

%-----------------------------------------------------------------------------%

	% Check to see if we should inline a call.
	%
	% Fails if the called predicate is a builtin or is imported.
	%
	% Succeeds if the called predicate has an annotation
	% indicating that it should be inlined, or if the goal
	% is a conjunction of builtins.

:- pred inlining__should_inline_proc(pred_id, proc_id, builtin_state,
	set(pred_proc_id), module_info).
:- mode inlining__should_inline_proc(in, in, in, in, in) is semidet.

inlining__should_inline_proc(PredId, ProcId, BuiltinState, InlinedProcs,
		ModuleInfo) :-

	% don't inline builtins, the code generator will handle them

	BuiltinState = not_builtin,

	% don't try to inline imported predicates, since we don't
	% have the code for them.

	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	\+ pred_info_is_imported(PredInfo),
		% this next line catches the case of locally defined
		% unification predicates for imported types.
	\+ (
		pred_info_is_pseudo_imported(PredInfo),
		hlds_pred__in_in_unification_proc_id(ProcId)
	),

	% don't inlining anything we have been specifically requested
	% not to inline.

	\+ pred_info_requested_no_inlining(PredInfo),

	% OK, we could inline it - but should we?  Apply our heuristic.

	(
		pred_info_requested_inlining(PredInfo)
	;
		set__member(proc(PredId, ProcId), InlinedProcs)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
