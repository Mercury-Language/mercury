%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% The module defines the part of the HLDS that deals with goals.

% Main authors: fjh, conway.

:- module hlds_goal.

:- interface.

:- import_module hlds_data, hlds_pred, llds, prog_data, (inst), instmap.
:- import_module list, assoc_list, set, map, std_util.

	% Here is how goals are represented

:- type hlds_goal	== pair(hlds_goal_expr, hlds_goal_info).

:- type hlds_goal_expr

		% A conjunction.
		% Note: conjunctions must be fully flattened before
		% mode analysis.  As a general rule, it is a good idea
		% to keep them flattened.

	--->	conj(hlds_goals)

		% A predicate call.
		% Initially only the sym_name and arguments
		% are filled in. Type analysis fills in the
		% pred_id. Mode analysis fills in the
		% proc_id and the builtin_state field.

	;	call(
			pred_id,	% which predicate are we calling?
			proc_id,	% which mode of the predicate?
			list(var),	% the list of argument variables
			builtin_state,	% is the predicate builtin, and if yes,
					% do we generate inline code for it?
			maybe(call_unify_context),
					% was this predicate call originally
					% a unification?  If so, we store the
					% context of the unification.
			sym_name	% the name of the predicate
		)

	;	higher_order_call(
			var,		% the predicate to call
			list(var),	% the list of argument variables
			list(type),	% the types of the argument variables
			list(mode),	% the modes of the argument variables
			determinism,	% the determinism of the called pred
			pred_or_func	% call/N (pred) or apply/N (func)
		)

		% Deterministic disjunctions are converted
		% into switches by the switch detection pass.

	;	switch(
			var,		% the variable we are switching on
			can_fail,	% whether or not the switch test itself
					% can fail (i.e. whether or not it
					% covers all the possible cases)
			list(case),
			store_map	% a map saying where each live variable
					% should be at the end of each arm of
					% the switch. This field is filled in
					% with advisory information by the
					% follow_vars pass, while store_alloc
					% fills it with authoritative
					% information.
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
					% (this field might not make a lot
					% of sense for higher-order
					% unifications, because polymorphism.m
					% does not update it properly)
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
			hlds_goals,
			store_map	% a map saying where each live variable
					% should be at the end of each arm of
					% the disj. This field is filled in
					% with advisory information by the
					% follow_vars pass, while store_alloc
					% fills it with authoritative
					% information.
		)

		% A negation
	;	not(hlds_goal)

		% An explicit quantification.
		% Quantification information is stored in the `non_locals'
		% field of the goal_info, so these get ignored
		% (except to recompute the goal_info quantification).
		% `all Vs' gets converted to `not some Vs not'.

	;	some(list(var), hlds_goal)

		% An if-then-else,
		% `if some <Vars> <Condition> then <Then> else <Else>'.
		% The scope of the locally existentially quantified variables
		% <Vars> is over the <Condition> and the <Then> part, 
		% but not the <Else> part.

	;	if_then_else(
			list(var),	% The locally existentially quantified
					% variables <Vars>.
			hlds_goal,	% The <Condition>
			hlds_goal,	% The <Then> part
			hlds_goal,	% The <Else> part
			store_map	% a map saying where each live variable
					% should be at the end of each arm of
					% the disj. This field is filled in
					% with advisory information by the
					% follow_vars pass, while store_alloc
					% fills it with authoritative
					% information.
		)

		% C code from a pragma(c_code, ...) decl.

	;	pragma_c_code(
			string,		% The C code to do the work
			may_call_mercury,
					% Can the C code recursively
					% invoke Mercury code?
			pred_id,	% The called predicate
			proc_id, 	% The mode of the predicate
			list(var),	% The (Mercury) argument variables
			list(maybe(string)),
					% C variable names for each of the
					% arguments. A no for a particular 
					% argument means that it is not used
					% by the C code.  (In particular, the
					% type_info variables introduced by
					% polymorphism.m might be represented
					% in this way).
			list(type),	% The original types of the arguments.
					% (With inlining, the actual types may
					% be instances of the original types.)
			extra_pragma_info
					% Extra information for model_non
					% pragma_c_codes; none for others.
		).

:- type extra_pragma_info
	--->	none
	;	extra_pragma_info(
			assoc_list(var, string),
					% the vars/names of the framevars used
					% by the hand-written C code (we may
					% need some more for saving the heap
					% pointer and/or tickets)
			list(string)	% the names of the labels needed
		).

	% Given the variable name field from a pragma c_code, get all the
	% variable names.
:- pred get_pragma_c_var_names(list(maybe(string)), list(string)).
:- mode get_pragma_c_var_names(in, out) is det.

	% There may be two sorts of "builtin" predicates - those that we
	% open-code using inline instructions (e.g. arithmetic predicates),
	% and those which are still "internal", but for which we generate
	% a call to an out-of-line procedure. At the moment there are no
	% builtins of the second sort, although we used to handle call/N
	% that wayay.

:- type builtin_state	--->	inline_builtin
			;	out_of_line_builtin
			;	not_builtin.

:- type case		--->	case(cons_id, hlds_goal).
			%	functor to match with,
			%	goal to execute if match succeeds.

:- type stack_slots	==	map(var, lval).
				% Maps variables to their stack slots.
				% The only legal lvals in the range are
				% stackvars and framevars.

:- type follow_vars	==	map(var, lval).
				% Advisory information about where variables
				% ought to be put next. The legal range
				% includes the nonexistent register r(-1),
				% which indicates any available register.

:- type store_map	==	map(var, lval).
				% Authoritative information about where
				% variables must be put at the ends of
				% branches of branched control structures.
				% However, between the follow_vars and
				% and store_alloc passes, these fields
				% temporarily hold follow_vars information.
				% Apart from this, the legal range is
				% the set of legal lvals.

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
				hlds_goal).

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
					% (this field might not make a lot
					% of sense for higher-order
					% unifications, because polymorphism.m
					% does not update it properly)
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
			can_fail	% Whether or not it could possibly fail
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

:- type hlds_goals == list(hlds_goal).

:- type hlds_goal_info.

:- type goal_feature
	--->	constraint	% This is included if the goal is
				% a constraint.  See constraint.m
				% for the definition of this.
	    ;	(impure)	% This goal is impure.  See hlds_pred.m.
	    ;	(semipure).	% This goal is semipure.  See hlds_pred.m.

	% see notes/ALLOCATION for what these alternatives mean
:- type resume_point	--->	resume_point(set(var), resume_locs)
			;	no_resume_point.

:- type resume_locs	--->	orig_only
			;	stack_only
			;	orig_and_stack
			;	stack_and_orig.

	% We can think of the goal that defines a procedure to be a tree,
	% whose leaves are primitive goals and whose interior nodes are
	% compound goals. These two types describe the position of a goal
	% in this tree. The first says which branch to take at an interior
	% node (the integer counts start at one). The second gives the
	% sequence of steps from the root to the given goal *in reverse order*,
	% so that the step closest to the root is last.

:- type goal_path_step	--->	conj(int)
			;	disj(int)
			;	switch(int)
			;	ite_cond
			;	ite_then
			;	ite_else
			;	neg
			;	exist.

:- type goal_path == list(goal_path_step).

:- implementation.

	% NB. Don't forget to check goal_util__name_apart_goalinfo
	% if this structure is modified.
:- type hlds_goal_info
	---> goal_info(
		set(var),	% the pre-birth set
		set(var),	% the post-birth set
		set(var),	% the pre-death set
		set(var),	% the post-death set
				% (all four are computed by liveness.m)
				% NB for atomic goals, the post-deadness
				% should be applied _before_ the goal

		determinism, 	% the overall determinism of the goal
				% (computed during determinism analysis)
				% [because true determinism is undecidable,
				% this may be a conservative approximation]

		instmap_delta,	% the change in insts over this goal
				% (computed during mode analysis)
				% [because true unreachability is undecidable,
				% the instmap_delta may be reachable even
				% when the goal really never succeeds]
				%
				% The following invariant is required
				% by the code generator and is enforced
				% by the final simplification pass:
				% the determinism specifies at_most_zero solns
				% iff the instmap_delta is unreachable.
				%
				% Before the final simplification pass,
				% the determinism and instmap_delta
				% might not be consistent with regard to
				% unreachability, but both will be
				% conservative approximations, so if either
				% says a goal is unreachable then it is.

		term__context,

		set(var),	% the non-local vars in the goal,
				% i.e. the variables that occur both inside
				% and outside of the goal.
				% (computed by quantification.m)
				% [in some circumstances, this may be a
				% conservative approximation: it may be
				% a superset of the real non-locals]

		maybe(follow_vars),
				% advisory information about where variables
				% ought to be put next. The legal range
				% includes the nonexistent register r(-1),
				% which indicates any available register.

		set(goal_feature),
				% The set of used-defined "features" of
				% this goal, which optimisers may wish
				% to know about.

		resume_point,
				% If this goal establishes a resumption point,
				% state what variables need to be saved for
				% that resumption point, and which entry
				% labels of the resumption point will be
				% needed. (See compiler/notes/allocation.html)

		goal_path
				% The path to this goal from the root in
				% reverse order.
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

	% Access predicates for the hlds_goal_info data structure.

:- interface.

:- pred goal_info_init(hlds_goal_info).
:- mode goal_info_init(out) is det.

:- pred goal_info_init(set(var), instmap_delta, determinism, hlds_goal_info).
:- mode goal_info_init(in, in, in, out) is det.

% Instead of recording the liveness of every variable at every
% part of the goal, we just keep track of the initial liveness
% and the changes in liveness.  Note that when traversing forwards
% through a goal, deaths must be applied before births;
% this is necessary to handle certain circumstances where a
% variable can occur in both the post-death and post-birth sets,
% or in both the pre-death and pre-birth sets.

:- pred goal_info_get_pre_births(hlds_goal_info, set(var)).
:- mode goal_info_get_pre_births(in, out) is det.

:- pred goal_info_set_pre_births(hlds_goal_info, set(var), hlds_goal_info).
:- mode goal_info_set_pre_births(in, in, out) is det.

:- pred goal_info_get_post_births(hlds_goal_info, set(var)).
:- mode goal_info_get_post_births(in, out) is det.

:- pred goal_info_set_post_births(hlds_goal_info, set(var), hlds_goal_info).
:- mode goal_info_set_post_births(in, in, out) is det.

:- pred goal_info_get_pre_deaths(hlds_goal_info, set(var)).
:- mode goal_info_get_pre_deaths(in, out) is det.

:- pred goal_info_set_pre_deaths(hlds_goal_info, set(var), hlds_goal_info).
:- mode goal_info_set_pre_deaths(in, in, out) is det.

:- pred goal_info_get_post_deaths(hlds_goal_info, set(var)).
:- mode goal_info_get_post_deaths(in, out) is det.

:- pred goal_info_set_post_deaths(hlds_goal_info, set(var), hlds_goal_info).
:- mode goal_info_set_post_deaths(in, in, out) is det.

:- pred goal_info_get_code_model(hlds_goal_info, code_model).
:- mode goal_info_get_code_model(in, out) is det.

:- pred goal_info_get_determinism(hlds_goal_info, determinism).
:- mode goal_info_get_determinism(in, out) is det.

:- pred goal_info_set_determinism(hlds_goal_info, determinism,
	hlds_goal_info).
:- mode goal_info_set_determinism(in, in, out) is det.

:- pred goal_info_get_nonlocals(hlds_goal_info, set(var)).
:- mode goal_info_get_nonlocals(in, out) is det.

:- pred goal_info_set_nonlocals(hlds_goal_info, set(var), hlds_goal_info).
:- mode goal_info_set_nonlocals(in, in, out) is det.

:- pred goal_info_get_features(hlds_goal_info, set(goal_feature)).
:- mode goal_info_get_features(in, out) is det.

:- pred goal_info_set_features(hlds_goal_info, set(goal_feature),
					hlds_goal_info).
:- mode goal_info_set_features(in, in, out) is det.

:- pred goal_info_add_feature(hlds_goal_info, goal_feature, hlds_goal_info).
:- mode goal_info_add_feature(in, in, out) is det.

:- pred goal_info_remove_feature(hlds_goal_info, goal_feature, 
					hlds_goal_info).
:- mode goal_info_remove_feature(in, in, out) is det.

:- pred goal_info_has_feature(hlds_goal_info, goal_feature).
:- mode goal_info_has_feature(in, in) is semidet.

:- pred goal_info_get_instmap_delta(hlds_goal_info, instmap_delta).
:- mode goal_info_get_instmap_delta(in, out) is det.

:- pred goal_info_set_instmap_delta(hlds_goal_info, instmap_delta,
				hlds_goal_info).
:- mode goal_info_set_instmap_delta(in, in, out) is det.

:- pred goal_info_get_context(hlds_goal_info, term__context).
:- mode goal_info_get_context(in, out) is det.

:- pred goal_info_set_context(hlds_goal_info, term__context, hlds_goal_info).
:- mode goal_info_set_context(in, in, out) is det.

:- pred goal_info_get_follow_vars(hlds_goal_info, maybe(follow_vars)).
:- mode goal_info_get_follow_vars(in, out) is det.

:- pred goal_info_set_follow_vars(hlds_goal_info, maybe(follow_vars),
	hlds_goal_info).
:- mode goal_info_set_follow_vars(in, in, out) is det.

:- pred goal_info_get_resume_point(hlds_goal_info, resume_point).
:- mode goal_info_get_resume_point(in, out) is det.

:- pred goal_info_set_resume_point(hlds_goal_info, resume_point,
	hlds_goal_info).
:- mode goal_info_set_resume_point(in, in, out) is det.

:- pred goal_info_get_goal_path(hlds_goal_info, goal_path).
:- mode goal_info_get_goal_path(in, out) is det.

:- pred goal_info_set_goal_path(hlds_goal_info, goal_path, hlds_goal_info).
:- mode goal_info_set_goal_path(in, in, out) is det.

:- pred goal_set_follow_vars(hlds_goal, maybe(follow_vars), hlds_goal).
:- mode goal_set_follow_vars(in, in, out) is det.

:- pred goal_set_resume_point(hlds_goal, resume_point, hlds_goal).
:- mode goal_set_resume_point(in, in, out) is det.

:- pred goal_info_resume_vars_and_loc(resume_point, set(var), resume_locs).
:- mode goal_info_resume_vars_and_loc(in, out, out) is det.

	% Convert a goal to a list of conjuncts.
	% If the goal is a conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.

:- pred goal_to_conj_list(hlds_goal, list(hlds_goal)).
:- mode goal_to_conj_list(in, out) is det.

	% Convert a goal to a list of disjuncts.
	% If the goal is a disjunction, then return its disjuncts,
	% otherwise return the goal as a singleton list.

:- pred goal_to_disj_list(hlds_goal, list(hlds_goal)).
:- mode goal_to_disj_list(in, out) is det.

	% Convert a list of conjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the conjunction of the conjuncts,
	% with the specified goal_info.

:- pred conj_list_to_goal(list(hlds_goal), hlds_goal_info, hlds_goal).
:- mode conj_list_to_goal(in, in, out) is det.

	% Convert a list of disjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the disjunction of the disjuncts,
	% with the specified goal_info.

:- pred disj_list_to_goal(list(hlds_goal), hlds_goal_info, hlds_goal).
:- mode disj_list_to_goal(in, in, out) is det.

	% Takes a goal and a list of goals, and conjoins them
	% (with a potentially blank goal_info).

:- pred conjoin_goal_and_goal_list(hlds_goal, list(hlds_goal),
	hlds_goal).
:- mode conjoin_goal_and_goal_list(in, in, out) is det.

	% Conjoin two goals (with a potentially blank goal_info).
	
:- pred conjoin_goals(hlds_goal, hlds_goal, hlds_goal).
:- mode conjoin_goals(in, in, out) is det.

	% A goal is atomic iff it doesn't contain any sub-goals
	% (except possibly goals inside lambda expressions --
	% but lambda expressions will get transformed into separate
	% predicates by the polymorphism.m pass).

:- pred goal_is_atomic(hlds_goal_expr).
:- mode goal_is_atomic(in) is semidet.

	% Return the HLDS equivalent of `true'.
:- pred true_goal(hlds_goal).
:- mode true_goal(out) is det.

	% Return the HLDS equivalent of `fail'.
:- pred fail_goal(hlds_goal).
:- mode fail_goal(out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, term.

goal_info_init(GoalInfo) :-
	Detism = erroneous,
	set__init(PreBirths),
	set__init(PostBirths),
	set__init(PreDeaths),
	set__init(PostDeaths),
	instmap_delta_init_unreachable(InstMapDelta),
	set__init(NonLocals),
	term__context_init(Context),
	set__init(Features),
	GoalInfo = goal_info(PreBirths, PostBirths, PreDeaths, PostDeaths,
		Detism, InstMapDelta, Context, NonLocals, no, Features,
		no_resume_point, []).

goal_info_init(NonLocals, InstMapDelta, Detism, GoalInfo) :-
	goal_info_init(GoalInfo0),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
	goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo2),
	goal_info_set_determinism(GoalInfo2, Detism, GoalInfo).

goal_info_get_pre_births(GoalInfo, PreBirths) :-
	GoalInfo = goal_info(PreBirths, _, _, _, _, _, _, _, _, _, _, _).

goal_info_get_post_births(GoalInfo, PostBirths) :-
	GoalInfo = goal_info(_, PostBirths, _, _, _, _, _, _, _, _, _, _).

goal_info_get_pre_deaths(GoalInfo, PreDeaths) :-
	GoalInfo = goal_info(_, _, PreDeaths, _, _, _, _, _, _, _, _, _).

goal_info_get_post_deaths(GoalInfo, PostDeaths) :-
	GoalInfo = goal_info(_, _, _, PostDeaths, _, _, _, _, _, _, _, _).

goal_info_get_determinism(GoalInfo, Determinism) :-
	GoalInfo = goal_info(_, _, _, _, Determinism, _, _, _, _, _, _, _).

goal_info_get_instmap_delta(GoalInfo, InstMapDelta) :-
	GoalInfo = goal_info(_, _, _, _, _, InstMapDelta, _, _, _, _, _, _).

goal_info_get_context(GoalInfo, Context) :-
	GoalInfo = goal_info(_, _, _, _, _, _, Context, _, _, _, _, _).

goal_info_get_nonlocals(GoalInfo, NonLocals) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, NonLocals, _, _, _, _).

goal_info_get_follow_vars(GoalInfo, MaybeFollowVars) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, _, MaybeFollowVars, _, _, _).

goal_info_get_features(GoalInfo, Features) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, _, _, Features, _, _).

goal_info_get_resume_point(GoalInfo, ResumePoint) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, _, _, _, ResumePoint, _).

goal_info_get_goal_path(GoalInfo, GoalPath) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, _, _, _, _, GoalPath).

% :- type hlds_goal_info
% 	--->	goal_info(
% 		A	set(var),	% the pre-birth set
% 		B	set(var),	% the post-birth set
% 		C	set(var),	% the pre-death set
% 		D	set(var),	% the post-death set
% 		E	determinism, 	% the overall determinism of the goal
% 		F	instmap_delta,	% the change in insts over this goal
% 		G	term__context,
% 		H	set(var),	% the non-local vars in the goal
% 		I	maybe(follow_vars),
% 		J	set(goal_feature),
%		K	resume_point,
%		L	goal_path
% 	).

goal_info_set_pre_births(GoalInfo0, PreBirths, GoalInfo) :-
	GoalInfo0 = goal_info(_, B, C, D, E, F, G, H, I, J, K, L),
	GoalInfo = goal_info(PreBirths, B, C, D, E, F, G, H, I, J, K, L).

goal_info_set_post_births(GoalInfo0, PostBirths, GoalInfo) :-
	GoalInfo0 = goal_info(A, _, C, D, E, F, G, H, I, J, K, L),
	GoalInfo = goal_info(A, PostBirths, C, D, E, F, G, H, I, J, K, L).

goal_info_set_pre_deaths(GoalInfo0, PreDeaths, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, _, D, E, F, G, H, I, J, K, L),
	GoalInfo = goal_info(A, B, PreDeaths, D, E, F, G, H, I, J, K, L).

goal_info_set_post_deaths(GoalInfo0, PostDeaths, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, _, E, F, G, H, I, J, K, L),
	GoalInfo = goal_info(A, B, C, PostDeaths, E, F, G, H, I, J, K, L).

goal_info_set_determinism(GoalInfo0, Determinism, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, _, F, G, H, I, J, K, L),
	GoalInfo = goal_info(A, B, C, D, Determinism, F, G, H, I, J, K, L).

goal_info_set_instmap_delta(GoalInfo0, InstMapDelta, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, _, G, H, I, J, K, L),
	GoalInfo = goal_info(A, B, C, D, E, InstMapDelta, G, H, I, J, K, L).

goal_info_set_context(GoalInfo0, Context, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, _, H, I, J, K, L),
	GoalInfo = goal_info(A, B, C, D, E, F, Context, H, I, J, K, L).

goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, _, I, J, K, L),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, NonLocals, I, J, K, L).

goal_info_set_follow_vars(GoalInfo0, FollowVars, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, H, _, J, K, L),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, FollowVars, J, K, L).

goal_info_set_features(GoalInfo0, Features, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, H, I, _, K, L),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, Features, K, L).

goal_info_set_resume_point(GoalInfo0, ResumePoint, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, H, I, J, _, L),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, J, ResumePoint, L).

goal_info_set_goal_path(GoalInfo0, GoalPath, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, H, I, J, K, _),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, J, K, GoalPath).

goal_info_get_code_model(GoalInfo, CodeModel) :-
	goal_info_get_determinism(GoalInfo, Determinism),
	determinism_to_code_model(Determinism, CodeModel).

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

goal_set_follow_vars(Goal - GoalInfo0, FollowVars, Goal - GoalInfo) :-
	goal_info_set_follow_vars(GoalInfo0, FollowVars, GoalInfo).

goal_set_resume_point(Goal - GoalInfo0, ResumePoint, Goal - GoalInfo) :-
	goal_info_set_resume_point(GoalInfo0, ResumePoint, GoalInfo).

%-----------------------------------------------------------------------------%

goal_info_resume_vars_and_loc(Resume, Vars, Locs) :-
	(
		Resume = resume_point(Vars, Locs)
	;
		Resume = no_resume_point,
		error("goal_info__get_resume_vars_and_loc: no resume point")
	).

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

conjoin_goal_and_goal_list(Goal0, Goals, Goal) :-
	Goal0 = GoalExpr0 - GoalInfo0,
	( GoalExpr0 = conj(GoalList0) ->
		list__append(GoalList0, Goals, GoalList),
		GoalExpr = conj(GoalList)
	;
		GoalExpr = conj([Goal0 | Goals])
	),
	Goal = GoalExpr - GoalInfo0.

conjoin_goals(Goal1, Goal2, Goal) :-
	( Goal2 = conj(Goals2) - _ ->
		GoalList = Goals2
	;
		GoalList = [Goal2]
	),
	conjoin_goal_and_goal_list(Goal1, GoalList, Goal).
	
%-----------------------------------------------------------------------------%

goal_is_atomic(conj([])).
goal_is_atomic(disj([], _)).
goal_is_atomic(higher_order_call(_,_,_,_,_,_)).
goal_is_atomic(call(_,_,_,_,_,_)).
goal_is_atomic(unify(_,_,_,_,_)).
goal_is_atomic(pragma_c_code(_,_,_,_,_,_,_,_)).

%-----------------------------------------------------------------------------%

true_goal(conj([]) - GoalInfo) :-
	goal_info_init(GoalInfo0),
	goal_info_set_determinism(GoalInfo0, det, GoalInfo1), 
	instmap_delta_init_reachable(InstMapDelta),
	goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo).

fail_goal(disj([], SM) - GoalInfo) :-
	map__init(SM),
	goal_info_init(GoalInfo0),
	goal_info_set_determinism(GoalInfo0, failure, GoalInfo1), 
	instmap_delta_init_unreachable(InstMapDelta),
	goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
