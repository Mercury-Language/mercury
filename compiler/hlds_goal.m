%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% The module defines the part of the HLDS that deals with goals.

% Main authors: fjh, conway.

:- module hlds_goal.

:- interface.

:- import_module bool, list, set, map, std_util.
:- import_module hlds_data, prog_data, instmap.

	% Here is how goals are represented

:- type hlds__goal	== pair(hlds__goal_expr, hlds__goal_info).

:- type hlds__goal_expr

		% A conjunction.
		% Note: conjunctions must be fully flattened before
		% mode analysis.  As a general rule, it is a good idea
		% to keep them flattened.

	--->	conj(hlds__goals)

		% A predicate call.
		% Initially only the sym_name and arguments
		% are filled in. Type analysis fills in the
		% pred_id. Mode analysis fills in the
		% proc_id and the is_builtin field.
		% `follow_vars.m' fills in
		% the follow_vars field.

	;	call(
			pred_id,	% which predicate are we calling?
			proc_id,	% which mode of the predicate?
			list(var),	% the list of argument variables
			is_builtin,	% is the predicate builtin, and
					% do we generate inline code for it?
			maybe(call_unify_context),
					% was this predicate call originally
					% a unification?  If so, we store the
					% context of the unification.
			sym_name,	% the name of the predicate
			follow_vars	% advisory storage locations for
					% placing variables when generating
					% the code that follows this call
					% (used so we can generate more
					% efficient code by putting variables
					% in the place they will be needed)
		)

	;	higher_order_call(
			var,		% the predicate to call
			list(var),	% the list of argument variables
			list(type),	% the types of the argument variables
			list(mode),	% the modes of the argument variables
			determinism,	% the determinism of the called pred
			follow_vars	% advisory storage locations for
					% placing variables when generating
					% the code that follows this call
					% (used so we can generate more
					% efficient code by putting variables
					% in the place they will be needed)
		)

		% Deterministic disjunctions are converted
		% into case statements by the switch detection pass.

	;	switch(
			var,		% the variable we are switching on
			can_fail,	% whether or not the switch test itself
					% can fail (i.e. whether or not it
					% covers all the possible cases)
			list(case),
			follow_vars	% advisory storage locations for
					% placing variables at the end of
					% each arm of the switch
		)

		% A unification.
		% Initially only the terms and the context
		% are known. Mode analysis fills in the missing information.

	;	unify(
			var,		% the variable on the left hand side
					% of the unification
			unify_rhs,	% whatever is on the right hand side
					% of the unification
			unify_mode,	% the mode of the unification
			unification,	% this field says what category of
					% unification it is, and contains
					% information specific to each category
			unify_context	% the location of the unification
					% in the original source code
					% (for use in error messages)
		)

		% A disjunction.
		% Note: disjunctions should be fully flattened.

	;	disj(
			hlds__goals,
			follow_vars	% advisory storage locations for
					% placing variables at the end of
					% each arm of the disjunction
		)

		% A negation
	;	not(hlds__goal)

		% An explicit quantification.
		% Quantification information is stored in the `non_locals'
		% field of the goal_info, so these get ignored
		% (except to recompute the goal_info quantification).
		% `all Vs' gets converted to `not some Vs not'.

	;	some(list(var), hlds__goal)

		% An if-then-else,
		% `if some <Vars> <Condition> then <Then> else <Else>'.
		% The scope of the locally existentially quantified variables
		% <Vars> is over the <Condition> and the <Then> part, 
		% but not the <Else> part.

	;	if_then_else(
			list(var),	% The locally existentially quantified
					% variables <Vars>.
			hlds__goal,	% The <Condition>
			hlds__goal,	% The <Then> part
			hlds__goal,	% The <Else> part
			follow_vars	% advisory storage locations for
					% placing variables at the end of
					% each arm of the ite
		)
	
		% C code from a pragma(c_code, ...) decl.

	;	pragma_c_code(
			string,		% The C code to do the work
			c_is_recursive, % Does the C code recursively
					% invoke Mercury code?
			pred_id,	% The called predicate
			proc_id, 	% The mode of the predicate
			list(var),	% The (Mercury) argument variables
			list(maybe(string))
					% C variable names for each of the
					% arguments. A no for a particular 
					% argument means that it is not used
					% by the C code.  (In particular, the
					% type_info variables introduced by
					% polymorphism.m might be represented
					% in this way).
		).


	% Given the variable name field from a pragma c_code, get all the
	% variable names.
:- pred get_pragma_c_var_names(list(maybe(string)), list(string)).
:- mode get_pragma_c_var_names(in, out) is det.


	% Record whether a call should be inlined or not,
	% and whether it is a builtin or not.

:- type is_builtin.

:- type call_info	==	map(var, lval).

:- type case		--->	case(cons_id, hlds__goal).
			%	functor to match with,
			%	goal to execute if match succeeds.

:- type follow_vars	==	map(var, lval).

	% Initially all unifications are represented as
	% unify(var, unify_rhs, _, _, _), but mode analysis replaces
	% these with various special cases (construct/deconstruct/assign/
	% simple_test/complicated_unify).
	% The cons_id for functor/2 cannot be a pred_const, code_addr_const,
	% or base_type_info_const, since none of these can be created when
	% the unify_rhs field is used.
:- type unify_rhs
	--->	var(var)
	;	functor(cons_id, list(var))
	;	lambda_goal(pred_or_func, list(var), list(mode), determinism,
				hlds__goal).

:- type unification
		% A construction unification is a unification with a functor
		% or lambda expression which binds the LHS variable,
		% e.g. Y = f(X) where the top node of Y is output,
		% Constructions are written using `:=', e.g. Y := f(X).

	--->	construct(
			var,		% the variable being constructed
					% e.g. Y in above example
			cons_id,	% the cons_id of the functor
					% f/1 in the above example
			list(var),	% the list of argument variables
					% [X] in the above example
					% For a unification with a lambda
					% expression, this is the list of
					% the non-local variables of the
					% lambda expression.
			list(uni_mode)	% The list of modes of the arguments
					% sub-unifications.
					% For a unification with a lambda
					% expression, this is the list of
					% modes of the non-local variables
					% of the lambda expression.
		)

		% A deconstruction unification is a unification with a functor
		% for which the LHS variable was already bound,
		% e.g. Y = f(X) where the top node of Y is input.
		% Deconstructions are written using `==', e.g. Y == f(X).
		% Note that deconstruction of lambda expressions is
		% a mode error.

	;	deconstruct(
			var,		% The variable being deconstructed
					% e.g. Y in the above example.
			cons_id,	% The cons_id of the functor,
					% e.g. f/1 in the above example
			list(var),	% The list of argument variables,
					% e.g. [X] in the above example.
			list(uni_mode), % The lists of modes of the argument
					% sub-unifications.
			can_fail	% Whether or not the unification
					% could possibly fail.
		)

		% Y = X where the top node of Y is output,
		% written Y := X.

	;	assign(
			var,	% variable being assigned to
			var	% variable whose value is being assigned
		)

		% Y = X where the type of X and Y is an atomic
		% type and they are both input, written Y == X.

	;	simple_test(var, var)

		% Y = X where the type of Y and X is not an
		% atomic type, and where the top-level node
		% of both Y and X is input. May involve
		% bi-directional data flow. Implemented
		% using out-of-line call to a compiler
		% generated unification predicate for that
		% type & mode.

	;	complicated_unify(
			uni_mode,	% The mode of the unification.
			can_fail,	% Whether or not it could possibly fail
			follow_vars	% The suggested locations where
					% variables should be placed when
					% generating code that follows
					% the call to the out-of-line
					% unification predicate (e.g. as
					% determined by whichever call follows
					% this one).
					% These are converted into predicate
					% calls by polymorphism.m.
		).

	% A unify_context describes the location in the original source
	% code of a unification, for use in error messages.

:- type unify_context
	--->	unify_context(
			unify_main_context,
			unify_sub_contexts
		).

	% A unify_main_context describes overall location of the
	% unification within a clause

:- type unify_main_context
		% an explicit call to =/2
	--->	explicit
			
		% a unification in an argument of a clause head
	;	head(	
			int		% the argument number
					% (first argument == no. 1)
		)

		% a unification in an argument of a predicate call
	;	call(	
			pred_call_id,	% the name and arity of the predicate
			int		% the argument number (first arg == 1)
		).

	% A unify_sub_context describes the location of sub-unification
	% (which is unifying one argument of a term)
	% within a particular unification.

:- type unify_sub_context
	==	pair(
			cons_id,	% the functor
			int		% the argument number (first arg == 1)
		).

:- type unify_sub_contexts == list(unify_sub_context).

	% A call_unify_context is used for unifications that get
	% turned into calls to out-of-line unification predicates.
	% It records which part of the original source code
	% the unification occurred in.

:- type call_unify_context
	--->	call_unify_context(
			var,		% the LHS of the unification
			unify_rhs,	% the RHS of the unification
			unify_context	% the context of the unification
		).

:- type hlds__goals == list(hlds__goal).

:- type hlds__goal_info.

:- type goal_feature
	--->	constraint.	% This is included if the goal is
				% a constraint.  See constraint.m
				% for the definition of this.

:- implementation.

:- type hlds__goal_info
	---> goal_info(
		delta_liveness,	% the changes in liveness after goal
				% (computed by liveness.m)
		unit,		% junk
		determinism, 	% the overall determinism of the goal
				% (computed during determinism analysis)
		instmap_delta,	% the change in insts over this goal
				% (computed during mode analysis)
		term__context,
		set(var),	% the non-local vars in the goal
				% (computed by quantification.m)
		delta_liveness,	% the changes in liveness before goal
				% (computed by liveness.m)
		maybe(set(var)),
				% The `cont lives' -
				% maybe the set of variables that are
				% live when forward execution resumes
				% on the failure of some subgoal of this
				% goal. For goals inside
				% negations, it is just the set of
				% variables live after the negation.
				% For conditions of ite's it is the
				% set of variables
				% live after the condition.
				% (XXX For disjunctions in det or semidet
				% disjunctions, it should perhaps be the set of
				% variables live at the start of the next
				% disjunction.  But we don't handle them
				% yet.)
				% These are the only kinds of goal that
				% use this field.
				% (Computed by store_alloc.m.)
		set(goal_feature),
				% The set of used-defined "features" of
				% this goal, which optimisers may wish
				% to know about.
		set(var)
				% The "nondet lives" -
				% Nondet live variables that may be 'dead' but
				% still nondet live.
				% (Computed by liveness.m.)
	).


get_pragma_c_var_names(MaybeVarNames, VarNames) :-
	get_pragma_c_var_names_2(MaybeVarNames, [], VarNames0),
	list__reverse(VarNames0, VarNames).

:- pred get_pragma_c_var_names_2(list(maybe(string))::in, list(string)::in,
					list(string)::out) is det.

get_pragma_c_var_names_2([], Names, Names).
get_pragma_c_var_names_2([MaybeName | MaybeNames], Names0, Names) :-
	(
		MaybeName = yes(Name),
		Names1 = [Name | Names0]
	;
		MaybeName = no,
		Names1 = Names0
	),
	get_pragma_c_var_names_2(MaybeNames, Names1, Names).
		

:- interface.

:- type unify_mode	==	pair(mode, mode).

:- type uni_mode	--->	pair(inst) -> pair(inst).
					% Each uni_mode maps a pair
					% of insts to a pair of new insts
					% Each pair represents the insts
					% of the LHS and the RHS respectively

%-----------------------------------------------------------------------------%

	% Access predicates for the hlds__goal_info data structure.

:- interface.

:- pred goal_info_init(hlds__goal_info).
:- mode goal_info_init(out) is det.

% Instead of recording the liveness of every variable at every
% part of the goal, we just keep track of the initial liveness
% and the changes in liveness.

:- type delta_liveness == pair(set(var)).
			% Births - Deaths.

:- pred goal_info_pre_delta_liveness(hlds__goal_info, delta_liveness).
:- mode goal_info_pre_delta_liveness(in, out) is det.

:- pred goal_info_set_pre_delta_liveness(hlds__goal_info, delta_liveness,
					hlds__goal_info).
:- mode goal_info_set_pre_delta_liveness(in, in, out) is det.

:- pred goal_info_post_delta_liveness(hlds__goal_info, delta_liveness).
:- mode goal_info_post_delta_liveness(in, out) is det.

:- pred goal_info_set_post_delta_liveness(hlds__goal_info, delta_liveness,
					hlds__goal_info).
:- mode goal_info_set_post_delta_liveness(in, in, out) is det.

:- pred goal_info_get_code_model(hlds__goal_info, code_model).
:- mode goal_info_get_code_model(in, out) is det.

:- pred goal_info_get_determinism(hlds__goal_info, determinism).
:- mode goal_info_get_determinism(in, out) is det.

:- pred goal_info_set_determinism(hlds__goal_info, determinism,
	hlds__goal_info).
:- mode goal_info_set_determinism(in, in, out) is det.

:- pred goal_info_get_nonlocals(hlds__goal_info, set(var)).
:- mode goal_info_get_nonlocals(in, out) is det.

:- pred goal_info_set_nonlocals(hlds__goal_info, set(var), hlds__goal_info).
:- mode goal_info_set_nonlocals(in, in, out) is det.

:- pred goal_info_get_features(hlds__goal_info, set(goal_feature)).
:- mode goal_info_get_features(in, out) is det.

:- pred goal_info_set_features(hlds__goal_info, set(goal_feature),
					hlds__goal_info).
:- mode goal_info_set_features(in, in, out) is det.

:- pred goal_info_add_feature(hlds__goal_info, goal_feature, hlds__goal_info).
:- mode goal_info_add_feature(in, in, out) is det.

:- pred goal_info_remove_feature(hlds__goal_info, goal_feature, 
					hlds__goal_info).
:- mode goal_info_remove_feature(in, in, out) is det.

:- pred goal_info_has_feature(hlds__goal_info, goal_feature).
:- mode goal_info_has_feature(in, in) is semidet.

:- pred goal_info_get_instmap_delta(hlds__goal_info, instmap_delta).
:- mode goal_info_get_instmap_delta(in, out) is det.

:- pred goal_info_set_instmap_delta(hlds__goal_info, instmap_delta,
				hlds__goal_info).
:- mode goal_info_set_instmap_delta(in, in, out) is det.

:- pred goal_info_context(hlds__goal_info, term__context).
:- mode goal_info_context(in, out) is det.

:- pred goal_info_set_context(hlds__goal_info, term__context, hlds__goal_info).
:- mode goal_info_set_context(in, in, out) is det.

:- pred goal_info_cont_lives(hlds__goal_info, maybe(set(var))).
:- mode goal_info_cont_lives(in, out) is det.

:- pred goal_info_set_cont_lives(hlds__goal_info,
				maybe(set(var)), hlds__goal_info).
:- mode goal_info_set_cont_lives(in, in, out) is det.

:- pred goal_info_nondet_lives(hlds__goal_info, set(var)).
:- mode goal_info_nondet_lives(in, out) is det.

:- pred goal_info_set_nondet_lives(hlds__goal_info,
				set(var), hlds__goal_info).
:- mode goal_info_set_nondet_lives(in, in, out) is det.

	% Convert a goal to a list of conjuncts.
	% If the goal is a conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.

:- pred goal_to_conj_list(hlds__goal, list(hlds__goal)).
:- mode goal_to_conj_list(in, out) is det.

	% Convert a goal to a list of disjuncts.
	% If the goal is a disjunction, then return its disjuncts,
	% otherwise return the goal as a singleton list.

:- pred goal_to_disj_list(hlds__goal, list(hlds__goal)).
:- mode goal_to_disj_list(in, out) is det.

	% Convert a list of conjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the conjunction of the conjuncts,
	% with the specified goal_info.

:- pred conj_list_to_goal(list(hlds__goal), hlds__goal_info, hlds__goal).
:- mode conj_list_to_goal(in, in, out) is det.

	% Convert a list of disjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the disjunction of the disjuncts,
	% with the specified goal_info.

:- pred disj_list_to_goal(list(hlds__goal), hlds__goal_info, hlds__goal).
:- mode disj_list_to_goal(in, in, out) is det.

	% A goal is atomic iff it doesn't contain any sub-goals
	% (except possibly goals inside lambda expressions --
	% but lambda expressions will get transformed into separate
	% predicates by the polymorphism.m pass).

:- pred goal_is_atomic(hlds__goal_expr).
:- mode goal_is_atomic(in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

goal_info_init(GoalInfo) :-
	ExternalDetism = erroneous,
	set__init(Births),
	set__init(Deaths),
	set__init(NondetLives),
	DeltaLiveness = Births - Deaths,
	InstMapDelta = unreachable,
	set__init(NonLocals),
	term__context_init(Context),
	set__init(Features),
	GoalInfo = goal_info(DeltaLiveness, unit, ExternalDetism,
		InstMapDelta, Context, NonLocals, DeltaLiveness, no,
		Features, NondetLives).

goal_info_pre_delta_liveness(GoalInfo, DeltaLiveness) :-
	GoalInfo = goal_info(DeltaLiveness, _, _, _, _, _, _, _, _, _).

goal_info_set_pre_delta_liveness(GoalInfo0, DeltaLiveness, GoalInfo) :-
	GoalInfo0 = goal_info(_, B, C, D, E, F, G, H, I, J),
	GoalInfo = goal_info(DeltaLiveness, B, C, D, E, F, G, H, I, J).

goal_info_post_delta_liveness(GoalInfo, DeltaLiveness) :-
	GoalInfo = goal_info(_, _, _, _, _, _, DeltaLiveness, _, _, _).

goal_info_set_post_delta_liveness(GoalInfo0, DeltaLiveness, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, _, H, I, J),
	GoalInfo = goal_info(A, B, C, D, E, F, DeltaLiveness, H, I, J).

goal_info_get_code_model(GoalInfo, CodeModel) :-
	goal_info_get_determinism(GoalInfo, Determinism),
	determinism_to_code_model(Determinism, CodeModel).

goal_info_get_determinism(GoalInfo, Determinism) :-
	GoalInfo = goal_info(_, _, Determinism, _, _, _, _, _, _, _).

goal_info_set_determinism(GoalInfo0, Determinism, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, _, D, E, F, G, H, I, J),
	GoalInfo = goal_info(A, B, Determinism, D, E, F, G, H, I, J).

goal_info_get_instmap_delta(GoalInfo, InstMapDelta) :-
	GoalInfo = goal_info(_, _, _, InstMapDelta, _, _, _, _, _, _).

goal_info_set_instmap_delta(GoalInfo0, InstMapDelta, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, _, E, F, G, H, I, J),
	GoalInfo = goal_info(A, B, C, InstMapDelta, E, F, G, H, I, J).

goal_info_context(GoalInfo, Context) :-
	GoalInfo = goal_info(_, _, _, _, Context, _, _, _, _, _).

goal_info_set_context(GoalInfo0, Context, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, _, F, G, H, I, J),
	GoalInfo = goal_info(A, B, C, D, Context, F, G, H, I, J).

goal_info_get_nonlocals(GoalInfo, NonLocals) :-
	GoalInfo = goal_info(_, _, _, _, _, NonLocals, _, _, _, _).

goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, _, G, H, I, J),
	GoalInfo  = goal_info(A, B, C, D, E, NonLocals, G, H, I, J).

goal_info_cont_lives(GoalInfo, I) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, I, _, _).

goal_info_set_cont_lives(GoalInfo0, H, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, _, I, J),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, J).

goal_info_get_features(GoalInfo, I) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, _, I, _).

goal_info_set_features(GoalInfo0, I, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, H, _, J),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, J).

goal_info_nondet_lives(GoalInfo, J) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, _, _, J).

goal_info_set_nondet_lives(GoalInfo0, J, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, H, I, _),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, J).

goal_info_add_feature(GoalInfo0, Feature, GoalInfo) :-
	goal_info_get_features(GoalInfo0, Features0),
	set__insert(Features0, Feature, Features),
	goal_info_set_features(GoalInfo0, Features, GoalInfo).

goal_info_remove_feature(GoalInfo0, Feature, GoalInfo) :-
	goal_info_get_features(GoalInfo0, Features0),
	set__delete(Features0, Feature, Features),
	goal_info_set_features(GoalInfo0, Features, GoalInfo).

goal_info_has_feature(GoalInfo, Feature) :-
	goal_info_get_features(GoalInfo, Features),
	set__member(Feature, Features).

%-----------------------------------------------------------------------------%

	% Convert a goal to a list of conjuncts.
	% If the goal is a conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.

goal_to_conj_list(Goal, ConjList) :-
	( Goal = (conj(List) - _) ->
		ConjList = List
	;
		ConjList = [Goal]
	).

	% Convert a goal to a list of disjuncts.
	% If the goal is a disjunction, then return its disjuncts
	% otherwise return the goal as a singleton list.

goal_to_disj_list(Goal, DisjList) :-
	( Goal = (disj(List, _) - _) ->
		DisjList = List
	;
		DisjList = [Goal]
	).

	% Convert a list of conjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the conjunction of the conjuncts,
	% with the specified goal_info.

conj_list_to_goal(ConjList, GoalInfo, Goal) :-
	( ConjList = [Goal0] ->
		Goal = Goal0
	;
		Goal = conj(ConjList) - GoalInfo
	).

	% Convert a list of disjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the disjunction of the conjuncts,
	% with the specified goal_info.

disj_list_to_goal(DisjList, GoalInfo, Goal) :-
	( DisjList = [Goal0] ->
		Goal = Goal0
	;
		map__init(Empty),
		Goal = disj(DisjList, Empty) - GoalInfo
	).

%-----------------------------------------------------------------------------%

goal_is_atomic(conj([])).
goal_is_atomic(disj([], _)).
goal_is_atomic(higher_order_call(_,_,_,_,_,_)).
goal_is_atomic(call(_,_,_,_,_,_,_)).
goal_is_atomic(unify(_,_,_,_,_)).
goal_is_atomic(pragma_c_code(_,_,_,_,_,_)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Originally we classified predicates according to whether they
	% were "builtin" or not.  But in fact there are two sorts of
	% "builtin" predicates - those that we open-code using inline
	% instructions (e.g. arithmetic predicates), and those which
	% are still "internal", but for which we generate a call to an
	% out-of-line procedure (e.g. call/N).

:- pred hlds__is_builtin_is_internal(is_builtin).
:- mode hlds__is_builtin_is_internal(in) is semidet.

:- pred hlds__is_builtin_is_inline(is_builtin).
:- mode hlds__is_builtin_is_inline(in) is semidet.

:- pred hlds__is_builtin_make_builtin(bool, bool, is_builtin).
:- mode hlds__is_builtin_make_builtin(in, in, out) is det.

:- implementation.

:- type is_builtin	== pair(bool).

hlds__is_builtin_is_internal(yes - _).

hlds__is_builtin_is_inline(_ - yes).

hlds__is_builtin_make_builtin(IsInternal, IsInline, IsInternal - IsInline).

