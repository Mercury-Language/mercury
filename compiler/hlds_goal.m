%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% The module defines the part of the HLDS that deals with goals.

% Main authors: fjh, conway.

:- module hlds__hlds_goal.

:- interface.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_llds.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module parse_tree__inst.
:- import_module parse_tree__prog_data.

:- import_module bool, char, list, set, map, std_util.

%-----------------------------------------------------------------------------%

	% Here is how goals are represented

:- type hlds_goals	== list(hlds_goal).

:- type hlds_goal	== pair(hlds_goal_expr, hlds_goal_info).

:- type hlds_goal_expr

		% A conjunction.
		% Note: conjunctions must be fully flattened before
		% mode analysis.  As a general rule, it is a good idea
		% to keep them flattened.

	--->	conj(hlds_goals)

		% A predicate call.
		% Initially only the sym_name, arguments, and context
		% are filled in. Type analysis fills in the
		% pred_id. Mode analysis fills in the
		% proc_id and the builtin_state field.

	;	call(
			call_pred_id	:: pred_id,
					% which predicate are we calling?
			call_proc_id	:: proc_id,
					% which mode of the predicate?
			call_args	:: list(prog_var),
					% the list of argument variables
			call_builtin	:: builtin_state,
					% is the predicate builtin, and if yes,
					% do we generate inline code for it?
			call_unify_context :: maybe(call_unify_context),
					% was this predicate call originally
					% a unification?  If so, we store the
					% context of the unification.
			call_sym_name	:: sym_name
					% the name of the predicate
		)

		% A generic call implements operations which are too
		% polymorphic to be written as ordinary predicates in Mercury
		% and require special casing, either because their arity
		% is variable, or they take higher-order arguments of
		% variable arity.
		% This currently includes higher-order calls, class-method
		% calls, Aditi calls and the Aditi update goals.

	;	generic_call(
			gcall_details	:: generic_call,
			gcall_args	:: list(prog_var),
					% the list of argument variables
			gcall_modes	:: list(mode),
					% The modes of the argument variables.
					% For higher_order calls, this field
					% is junk until after mode analysis.
					% For aditi_builtins, this field
					% is junk until after purity checking.
			gcall_detism	:: determinism
					% the determinism of the call
		)

		% Deterministic disjunctions are converted
		% into switches by the switch detection pass.

	;	switch(
			switch_var	:: prog_var,
					% the variable we are switching on
			switch_canfail	:: can_fail,
					% whether or not the switch test itself
					% can fail (i.e. whether or not it
					% covers all the possible cases)
			switch_cases	:: list(case)
		)

		% A unification.
		% Initially only the terms and the context
		% are known. Mode analysis fills in the missing information.

	;	unify(
			unify_lhs	:: prog_var,
					% the variable on the left hand side
					% of the unification
			unify_rhs	:: unify_rhs,
					% whatever is on the right hand side
					% of the unification
			unify_mode	:: unify_mode,
					% the mode of the unification
			unify_kind	:: unification,
					% this field says what category of
					% unification it is, and contains
					% information specific to each category
			unify_context	:: unify_context
					% the location of the unification
					% in the original source code
					% (for use in error messages)
		)

		% A disjunction.
		% Note: disjunctions should be fully flattened.

	;	disj(hlds_goals)

		% A negation
	;	not(hlds_goal)

		% An explicit quantification.
		% Quantification information is stored in the `non_locals'
		% field of the goal_info, so these get ignored
		% (except to recompute the goal_info quantification).
		% `all Vs' gets converted to `not some Vs not'.
		% The second argument is `can_remove' if the quantification
		% is allowed to be removed; see the docs for the can_remove
		% type.
	;	{ some(
			some_exist_vars	:: list(prog_var),
			some_can_remove	:: can_remove,
			some_goal	:: hlds_goal
		) }

		% An if-then-else,
		% `if some <Vars> <Condition> then <Then> else <Else>'.
		% The scope of the locally existentially quantified variables
		% <Vars> is over the <Condition> and the <Then> part,
		% but not the <Else> part.

	;	if_then_else(
			ite_exist_vars	:: list(prog_var),
					% The locally existentially quantified
					% variables <Vars>.
			ite_cond	:: hlds_goal,	% The <Condition>
			ite_then	:: hlds_goal,	% The <Then> part
			ite_else	:: hlds_goal	% The <Else> part
		)

		% Foreign code from a pragma foreign_proc(...) decl.

	;	foreign_proc(
			foreign_attr	:: pragma_foreign_proc_attributes,
			foreign_pred_id	:: pred_id,
					% The called predicate
			foreign_proc_id	:: proc_id,
					% The mode of the predicate
			foreign_args	:: list(prog_var),
					% The (Mercury) argument variables
			foreign_names	:: list(maybe(pair(string, mode))),
					% Foreign variable names and the
					% original mode declaration for each
					% of the arguments. A no for a
					% particular argument means that it is
					% not used by the foreign code.  (In
					% particular, the type_info variables
					% introduced by polymorphism.m might
					% be represented in this way).
			foreign_types	:: list(type),
					% The original types of the arguments.
					% (With inlining, the actual types may
					% be instances of the original types.)
			foreign_impl	:: pragma_foreign_code_impl
					% Extra information for model_non
					% pragma_foreign_codes; none for others.
		)

		% parallel conjunction
	;	par_conj(hlds_goals)

		% shorthand goals
		%
		% All shorthand goals are eliminated during or shortly after
		% the construction of the hlds, so most passes of the compiler
		% will just call error/1 if they occur.
	;	shorthand(shorthand_goal_expr).

	% Instances of these `shorthand' goals are implemented by a
	% hlds --> hlds transformation that replaces them with
	% equivalent non-shorthand goals.
:- type shorthand_goal_expr
		% bi-implication (A <=> B)
		%
		% Note that ordinary implications (A => B)
		% and reverse implications (A <= B) are expanded
		% out before we construct the HLDS.  But we can't
		% do that for bi-implications, because if expansion
		% of bi-implications is done before implicit quantification,
		% then the quantification would be wrong
	--->	bi_implication(hlds_goal, hlds_goal).

%-----------------------------------------------------------------------------%
%
% Information for generic_calls
%

:- type generic_call
	--->	higher_order(
			prog_var,
			purity,
			pred_or_func,	% call/N (pred) or apply/N (func)
			arity		% number of arguments (including the
					% higher-order term)
		)

	;	class_method(
			prog_var,	% typeclass_info for the instance
			int,		% number of the called method
			class_id,	% name and arity of the class
			simple_call_id	% name of the called method
		)

	;	aditi_builtin(
			aditi_builtin,
			simple_call_id
		)
	.

	% Get a description of a generic_call goal.
:- pred hlds_goal__generic_call_id(generic_call, call_id).
:- mode hlds_goal__generic_call_id(in, out) is det.

	% Determine whether a generic_call is calling
	% a predicate or a function
:- func generic_call_pred_or_func(generic_call) = pred_or_func.

%-----------------------------------------------------------------------------%
%
% Information for quantifications
%

	% The second argument of explicit quantification goals
	% is `can_remove' if the quantification is allowed to
	% be removed.  A non-removable explicit
	% quantification may be introduced to keep related goals
	% together where optimizations that separate the goals
	% can only result in worse behaviour. An example is the
	% closures for the builtin aditi update predicates -
	% they should be kept close to the update call where
	% possible to make it easier to use indexes for the update.
	%
	% See also the closely related `keep_this_commit' goal_feature.
	% XXX Why do we have both cannot_remove and keep_this_commit?
	%     Do we really need both?
:- type can_remove
	--->	can_remove
	;	cannot_remove.

%-----------------------------------------------------------------------------%
%
% Information for calls
%

	% There may be two sorts of "builtin" predicates - those that we
	% open-code using inline instructions (e.g. arithmetic predicates),
	% and those which are still "internal", but for which we generate
	% a call to an out-of-line procedure. At the moment there are no
	% builtins of the second sort, although we used to handle call/N
	% that way.

:- type builtin_state	--->	inline_builtin
			;	out_of_line_builtin
			;	not_builtin.

%-----------------------------------------------------------------------------%
%
% Information for switches
%

:- type case		--->	case(cons_id, hlds_goal).
			%	functor to match with,
			%	goal to execute if match succeeds.

%-----------------------------------------------------------------------------%
%
% Information for unifications
%

	% Initially all unifications are represented as
	% unify(prog_var, unify_rhs, _, _, _), but mode analysis replaces
	% these with various special cases (construct/deconstruct/assign/
	% simple_test/complicated_unify).
:- type unify_rhs
	--->	var(prog_var)
	;	functor(
			cons_id,
			is_existential_construction,
					% The `is_existential_construction'
					% field is only used after
					% polymorphism.m strips off
					% the `new ' prefix from
					% existentially typed constructions.
					
			list(prog_var)
		)
	;	lambda_goal(
			purity,
			pred_or_func,
			lambda_eval_method,
					% should be `normal' except for
					% closures executed by Aditi.
			fix_aditi_state_modes,
			list(prog_var),	% non-locals of the goal excluding
					% the lambda quantified variables
			list(prog_var),	% lambda quantified variables
			list(mode),	% modes of the lambda
					% quantified variables
			determinism,
			hlds_goal
		).

	% Was the constructor originally of the form 'new ctor'(...).
:- type is_existential_construction == bool.

:- type unification
		% A construction unification is a unification with a functor
		% or lambda expression which binds the LHS variable,
		% e.g. Y = f(X) where the top node of Y is output,
		% Constructions are written using `:=', e.g. Y := f(X).

	--->	construct(
			construct_cell_var	:: prog_var,
					% the variable being constructed
					% e.g. Y in above example
			construct_cons_id	:: cons_id,
					% the cons_id of the functor
					% f/1 in the above example
			construct_args		:: list(prog_var),
					% the list of argument variables
					% [X] in the above example
					% For a unification with a lambda
					% expression, this is the list of
					% the non-local variables of the
					% lambda expression.
			construct_arg_modes	:: list(uni_mode),
					% The list of modes of the arguments
					% sub-unifications.
					% For a unification with a lambda
					% expression, this is the list of
					% modes of the non-local variables
					% of the lambda expression.
			construct_how		:: how_to_construct,
					% Specify whether to allocate
					% statically, to allocate dynamically,
					% or to reuse an existing cell
					% (and if so, which cell).
					% Constructions for which this
					% field is `reuse_cell(_)' are
					% described as "reconstructions".
			construct_is_unique	:: cell_is_unique,
					% Can the cell be allocated
					% in shared data.
			construct_exprn_id	:: maybe(rl_exprn_id)
					% Used for `aditi_top_down' closures
					% passed to `aditi_delete' and
					% `aditi_modify' calls where the
					% relation being modified has a
					% B-tree index.
					% The Aditi-RL expression referred
					% to by this field constructs a key
					% range which restricts the deletion
					% or modification of the relation using
					% the index so that the deletion or
					% modification closure is only applied
					% to tuples for which the closure could
					% succeed, reducing the number of
					% tuples read from disk.
		)

		% A deconstruction unification is a unification with a functor
		% for which the LHS variable was already bound,
		% e.g. Y = f(X) where the top node of Y is input.
		% Deconstructions are written using `==', e.g. Y == f(X).
		% Note that deconstruction of lambda expressions is
		% a mode error.

	;	deconstruct(
			deconstruct_cell_var	:: prog_var,
					% The variable being deconstructed
					% e.g. Y in the above example.
			deconstruct_cons_id	:: cons_id,
					% The cons_id of the functor,
					% e.g. f/1 in the above example
			deconstruct_args	:: list(prog_var),
					% The list of argument variables,
					% e.g. [X] in the above example.
			deconstruct_arg_modes	:: list(uni_mode),
					% The lists of modes of the argument
					% sub-unifications.
			deconstruct_can_fail	:: can_fail,
					% Whether or not the unification
					% could possibly fail.
			deconstruct_can_cgc	:: can_cgc
					% Can compile time GC this cell,
					% ie explicitly deallocate it
					% after the deconstruction.
		)

		% Y = X where the top node of Y is output,
		% written Y := X.

	;	assign(
			assign_to_var		:: prog_var,
			assign_from_var		:: prog_var
		)

		% Y = X where the type of X and Y is an atomic
		% type and they are both input, written Y == X.

	;	simple_test(
			test_var1		:: prog_var,
			test_var2		:: prog_var
		)

		% Y = X where the type of Y and X is not an
		% atomic type, and where the top-level node
		% of both Y and X is input. May involve
		% bi-directional data flow. Implemented
		% using out-of-line call to a compiler
		% generated unification predicate for that
		% type & mode.

	;	complicated_unify(
			compl_unify_mode	:: uni_mode,
					% The mode of the unification.
			compl_unify_can_fail	:: can_fail,
					% Whether or not it could possibly fail

			% When unifying polymorphic types such as
			% map/2, we need to pass type_info variables
			% to the unification procedure for map/2
			% so that it knows how to unify the
			% polymorphically typed components of the
			% data structure.  Likewise for comparison
			% predicates.
			% This field records which type_info variables
			% we will need.
			% This field is set by polymorphism.m.
			% It is used by quantification.m
			% when recomputing the nonlocals.
			% It is also used by modecheck_unify.m,
			% which checks that the type_info
			% variables needed are all ground.
			% It is also checked by simplify.m when
			% it converts complicated unifications
			% into procedure calls.
			compl_unify_typeinfos	:: list(prog_var)
					% The type_info variables needed
					% by this unification, if it ends up
					% being a complicated unify.
		).

	% `yes' iff the cell is available for compile time garbage collection.
	% Compile time garbage collection is when the compiler
	% recognises that a memory cell is no longer needed and can be
	% safely deallocated (ie by inserting an explicit call to free).
:- type can_cgc == bool.

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

		% a unification in the function result term of a clause head
	;	head_result

		% a unification in an argument of a predicate call
	;	call(
			call_id,	% the name and arity of the predicate
			int		% the argument number (first arg == 1)
		)

		% a unification added by some syntactic transformation
		% (e.g. for handling state variables.)
	;	implicit(
			string		% used to explain the source of the
					% unification
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
	% turned into calls to out-of-line unification predicates,
	% and functions.  It records which part of the original source
	% code the unification (which may be a function application)
	% occurred in.

:- type call_unify_context
	--->	call_unify_context(
			prog_var,	% the LHS of the unification
			unify_rhs,	% the RHS of the unification
			unify_context	% the context of the unification
		).

	% Information on how to construct the cell for a
	% construction unification.  The `construct_statically'
	% alternative is set by the `mark_static_terms.m' pass,
	% and is currently only used for the MLDS back-end
	% (for the LLDS back-end, the same optimization is
	% handled by code_exprn.m).
	% The `reuse_cell' alternative is not yet used.
:- type how_to_construct
	--->	construct_statically(		% Use a statically initialized
						% constant
			args :: list(static_cons)
		)
	;	construct_dynamically		% Allocate a new term on the
						% heap
	;	reuse_cell(cell_to_reuse)	% Reuse an existing heap cell
	.

	% Information on how to construct an argument for
	% a static construction unification.  Each such
	% argument must itself have been constructed
	% statically; we store here a subset of the fields
	% of the original `construct' unification for the arg.
	% This is used by the MLDS back-end.
:- type static_cons
	--->	static_cons(
			cons_id,		% the cons_id of the functor
			list(prog_var),		% the list of arg variables
			list(static_cons)	% how to construct the args
		).

	% Information used to perform structure reuse on a cell.
:- type cell_to_reuse
	---> cell_to_reuse(
		prog_var,
		list(cons_id),	% The cell to be reused may be tagged
				% with one of these cons_ids.
		list(bool)      % A `no' entry means that the corresponding
				% argument already has the correct value
				% and does not need to be filled in.
	).

	% Cells marked `cell_is_shared' can be allocated in read-only memory,
	% and can be shared.
	% Cells marked `cell_is_unique' must be writeable, and therefore
	% cannot be shared.
	% `cell_is_unique' is always a safe approximation.
:- type cell_is_unique
	--->	cell_is_unique
	;	cell_is_shared
	.

:- type unify_mode	==	pair(mode, mode).

:- type uni_mode	--->	pair(inst) -> pair(inst).
					% Each uni_mode maps a pair
					% of insts to a pair of new insts
					% Each pair represents the insts
					% of the LHS and the RHS respectively

%-----------------------------------------------------------------------------%
%
% Information for all kinds of goals
%

%
% Access predicates for the hlds_goal_info data structure.
% For documentation on the meaning of the fields that these
% procedures access, see the definition of the hlds_goal_info type.
%

:- type hlds_goal_info.
:- type hlds_goal_code_gen_info.

:- pred goal_info_init(hlds_goal_info).
:- mode goal_info_init(out) is det.

:- pred goal_info_init(prog_context, hlds_goal_info).
:- mode goal_info_init(in, out) is det.

:- pred goal_info_init(set(prog_var), instmap_delta, determinism, purity,
		hlds_goal_info).
:- mode goal_info_init(in, in, in, in, out) is det.

:- pred goal_info_init(set(prog_var)::in, instmap_delta::in, determinism::in,
	purity::in, prog_context::in, hlds_goal_info::out) is det.

% Instead of recording the liveness of every variable at every
% part of the goal, we just keep track of the initial liveness
% and the changes in liveness.  Note that when traversing forwards
% through a goal, deaths must be applied before births;
% this is necessary to handle certain circumstances where a
% variable can occur in both the post-death and post-birth sets,
% or in both the pre-death and pre-birth sets.

	% see also goal_info_get_code_model in code_model.m
:- pred goal_info_get_determinism(hlds_goal_info, determinism).
:- mode goal_info_get_determinism(in, out) is det.

:- pred goal_info_set_determinism(hlds_goal_info, determinism,
	hlds_goal_info).
:- mode goal_info_set_determinism(in, in, out) is det.

:- pred goal_info_get_nonlocals(hlds_goal_info, set(prog_var)).
:- mode goal_info_get_nonlocals(in, out) is det.

:- pred goal_info_get_code_gen_nonlocals(hlds_goal_info, set(prog_var)).
:- mode goal_info_get_code_gen_nonlocals(in, out) is det.

:- pred goal_info_set_nonlocals(hlds_goal_info, set(prog_var), hlds_goal_info).
:- mode goal_info_set_nonlocals(in, in, out) is det.

:- pred goal_info_set_code_gen_nonlocals(hlds_goal_info,
		set(prog_var), hlds_goal_info).
:- mode goal_info_set_code_gen_nonlocals(in, in, out) is det.

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

:- pred goal_info_get_context(hlds_goal_info, prog_context).
:- mode goal_info_get_context(in, out) is det.

:- pred goal_info_set_context(hlds_goal_info, prog_context, hlds_goal_info).
:- mode goal_info_set_context(in, in, out) is det.

:- pred goal_info_get_goal_path(hlds_goal_info, goal_path).
:- mode goal_info_get_goal_path(in, out) is det.

:- pred goal_info_set_goal_path(hlds_goal_info, goal_path, hlds_goal_info).
:- mode goal_info_set_goal_path(in, in, out) is det.

:- pred goal_info_get_code_gen_info(hlds_goal_info, hlds_goal_code_gen_info).
:- mode goal_info_get_code_gen_info(in, out) is det.

:- pred goal_info_set_code_gen_info(hlds_goal_info, hlds_goal_code_gen_info,
	hlds_goal_info).
:- mode goal_info_set_code_gen_info(in, in, out) is det.

:- pred goal_get_nonlocals(hlds_goal, set(prog_var)).
:- mode goal_get_nonlocals(in, out) is det.

:- pred goal_add_feature(hlds_goal, goal_feature, hlds_goal).
:- mode goal_add_feature(in, in, out) is det.

:- pred goal_remove_feature(hlds_goal, goal_feature, hlds_goal).
:- mode goal_remove_feature(in, in, out) is det.

:- pred goal_has_feature(hlds_goal, goal_feature).
:- mode goal_has_feature(in, in) is semidet.

%  Update a goal info to reflect the specified purity
:- pred add_goal_info_purity_feature(hlds_goal_info, purity, hlds_goal_info).
:- mode add_goal_info_purity_feature(in, in, out) is det.

%  Determine the purity of a goal from its hlds_goal_info.
:- pred infer_goal_info_purity(hlds_goal_info, purity).
:- mode infer_goal_info_purity(in, out) is det.

%  Check if a hlds_goal_info is for a pure goal
:- pred goal_info_is_pure(hlds_goal_info).
:- mode goal_info_is_pure(in) is semidet.

%  Check if a hlds_goal_info is for an impure goal.  Fails if the goal is
%  semipure, so this isn't the same as \+ goal_info_is_pure.
:- pred goal_info_is_impure(hlds_goal_info).
:- mode goal_info_is_impure(in) is semidet.

:- type goal_feature
	--->	constraint	% This is included if the goal is
				% a constraint.  See constraint.m
				% for the definition of this.
	;	(impure)	% This goal is impure.  See hlds_pred.m.
	;	(semipure)	% This goal is semipure.  See hlds_pred.m.
	;	stack_opt	% This goal was created by stack slot
				% optimization. Other optimizations should
				% assume that it is there for a reason, and
				% therefore should refrain from "optimizing"
				% it away, even though it is a copy of another,
				% previous goal.
	;	call_table_gen	% This goal generates the variable that
				% represents the call table tip. If debugging
				% is enabled, the code generator needs to save
				% the value of this variable in its stack slot
				% as soon as it is generated; this marker
				% tells the code generator when this happens.
	;	keep_this_commit
				% This feature should be attached only to goals
				% that represent commits (i.e. some() goals in
				% which the inner and outer determinisms
				% differ). It tells determinism analysis that
				% some other part of the compiler wants the
				% commit to stay, even if the usual rules of
				% determinism analysis say that the
				% nondeterminism inside the some() should be
				% exposed to the environment outside.
	;	preserve_backtrack_into
				% Determinism analysis should preserve
				% backtracking into goals marked with this
				% feature, even if their determinism puts an
				% at_most_zero upper bound on the number of
				% solutions they have.
	;	hide_debug_event
				% The events associated with this goal should
				% be hidden. This is used e.g. by the tabling
				% transformation to preserve the set of events
				% generated by a tabled procedure.
	;	tailcall.	% This goal represents a tail call. This marker
				% is used by deep profiling.

	% We can think of the goal that defines a procedure to be a tree,
	% whose leaves are primitive goals and whose interior nodes are
	% compound goals. These two types describe the position of a goal
	% in this tree. A goal_path_step type says which branch to take at an
	% interior node; the integer counts start at one. (For switches,
	% the second int gives the total number of function symbols in the type
	% of the switched-on var; for builtin types such as integer and string,
	% for which this number is effectively infinite, we store a negative
	% number.) The goal_path type gives the sequence of steps from the root
	% to the given goal *in reverse order*, so that the step closest to
	% the root is last. (Keeping the list in reverse order makes the
	% common operations constant-time instead of linear in the length
	% of the list.)
	%
	% If any of the following three types is changed, then the
	% corresponding types in browser/program_representation.m must be
	% updated.

:- type goal_path == list(goal_path_step).

:- type goal_path_step	--->	conj(int)
			;	disj(int)
			;	switch(int, int)
			;	ite_cond
			;	ite_then
			;	ite_else
			;	neg
			;	exist(maybe_cut)
			;	first
			;	later.

:- type maybe_cut	--->	cut ; no_cut.

%-----------------------------------------------------------------------------%
%
% Miscellaneous utility procedures for dealing with HLDS goals
%

	% Convert a goal to a list of conjuncts.
	% If the goal is a conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.

:- pred goal_to_conj_list(hlds_goal, list(hlds_goal)).
:- mode goal_to_conj_list(in, out) is det.

	% Convert a goal to a list of parallel conjuncts.
	% If the goal is a parallel conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.

:- pred goal_to_par_conj_list(hlds_goal, list(hlds_goal)).
:- mode goal_to_par_conj_list(in, out) is det.

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

	% Convert a list of parallel conjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the parallel conjunction of the conjuncts,
	% with the specified goal_info.

:- pred par_conj_list_to_goal(list(hlds_goal), hlds_goal_info, hlds_goal).
:- mode par_conj_list_to_goal(in, in, out) is det.

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

	% Negate a goal, eliminating double negations as we go.
	%
:- pred negate_goal(hlds_goal, hlds_goal_info, hlds_goal).
:- mode negate_goal(in, in, out) is det.

	% Return yes if goal(s) contain any foreign code
:- func goal_has_foreign(hlds_goal) = bool.
:- mode goal_has_foreign(in) = out is det.
:- func goal_list_has_foreign(list(hlds_goal)) = bool.
:- mode goal_list_has_foreign(in) = out is det.

	% A goal is atomic iff it doesn't contain any sub-goals
	% (except possibly goals inside lambda expressions --
	% but lambda expressions will get transformed into separate
	% predicates by the polymorphism.m pass).

:- pred goal_is_atomic(hlds_goal_expr).
:- mode goal_is_atomic(in) is semidet.

	% Return the HLDS equivalent of `true'.
:- pred true_goal(hlds_goal).
:- mode true_goal(out) is det.

:- pred true_goal(prog_context, hlds_goal).
:- mode true_goal(in, out) is det.

	% Return the HLDS equivalent of `fail'.
:- pred fail_goal(hlds_goal).
:- mode fail_goal(out) is det.

:- pred fail_goal(prog_context, hlds_goal).
:- mode fail_goal(in, out) is det.

       % Return the union of all the nonlocals of a list of goals.
:- pred goal_list_nonlocals(list(hlds_goal), set(prog_var)).
:- mode goal_list_nonlocals(in, out) is det.

       % Compute the instmap_delta resulting from applying
       % all the instmap_deltas of the given goals.
:- pred goal_list_instmap_delta(list(hlds_goal), instmap_delta).
:- mode goal_list_instmap_delta(in, out) is det.

       % Compute the determinism of a list of goals.
:- pred goal_list_determinism(list(hlds_goal), determinism).
:- mode goal_list_determinism(in, out) is det.

	% Compute the purity of a list of goals. 
:- pred goal_list_purity(list(hlds_goal), purity).
:- mode goal_list_purity(in, out) is det.

	% Change the contexts of the goal_infos of all the sub-goals
	% of the given goal. This is used to ensure that error messages
	% for automatically generated unification procedures have a useful
	% context.
:- pred set_goal_contexts(prog_context, hlds_goal, hlds_goal).
:- mode set_goal_contexts(in, in, out) is det.

	% Create the hlds_goal for a unification, filling in all the as yet
	% unknown slots with dummy values.
:- pred create_atomic_unification(prog_var, unify_rhs, prog_context,
			unify_main_context, unify_sub_contexts, hlds_goal).
:- mode create_atomic_unification(in, in, in, in, in, out) is det.

	%
	% Produce a goal to construct a given constant.
	% These predicates all fill in the non-locals, instmap_delta
	% and determinism fields of the goal_info of the returned goal.
	% With alias tracking, the instmap_delta will be correct
	% only if the variable being assigned to has no aliases.
	%

:- pred make_int_const_construction(prog_var, int, hlds_goal).
:- mode make_int_const_construction(in, in, out) is det.

:- pred make_string_const_construction(prog_var, string, hlds_goal).
:- mode make_string_const_construction(in, in, out) is det.

:- pred make_float_const_construction(prog_var, float, hlds_goal).
:- mode make_float_const_construction(in, in, out) is det.

:- pred make_char_const_construction(prog_var, char, hlds_goal).
:- mode make_char_const_construction(in, in, out) is det.

:- pred make_const_construction(prog_var, cons_id, hlds_goal).
:- mode make_const_construction(in, in, out) is det.

:- pred make_int_const_construction(int, maybe(string), hlds_goal, prog_var,
	map(prog_var, type), map(prog_var, type), prog_varset, prog_varset).
:- mode make_int_const_construction(in, in, out, out, in, out, in, out) is det.

:- pred make_string_const_construction(string, maybe(string),
	hlds_goal, prog_var, map(prog_var, type), map(prog_var, type),
	prog_varset, prog_varset).
:- mode make_string_const_construction(in, in, out, out, in, out, in, out)
	is det.

:- pred make_float_const_construction(float, maybe(string),
	hlds_goal, prog_var, map(prog_var, type), map(prog_var, type),
	prog_varset, prog_varset).
:- mode make_float_const_construction(in, in, out, out, in, out, in, out)
	is det.

:- pred make_char_const_construction(char, maybe(string), hlds_goal, prog_var,
	map(prog_var, type), map(prog_var, type), prog_varset, prog_varset).
:- mode make_char_const_construction(in, in, out, out, in, out, in, out)
	is det.

:- pred make_const_construction(cons_id, (type), maybe(string),
	hlds_goal, prog_var, map(prog_var, type), map(prog_var, type),
	prog_varset, prog_varset).
:- mode make_const_construction(in, in, in, out, out, in, out, in, out) is det.

:- pred make_int_const_construction(int, maybe(string), hlds_goal, prog_var,
	proc_info, proc_info).
:- mode make_int_const_construction(in, in, out, out, in, out) is det.

:- pred make_string_const_construction(string, maybe(string),
	hlds_goal, prog_var, proc_info, proc_info).
:- mode make_string_const_construction(in, in, out, out, in, out) is det.

:- pred make_float_const_construction(float, maybe(string),
	hlds_goal, prog_var, proc_info, proc_info).
:- mode make_float_const_construction(in, in, out, out, in, out) is det.

:- pred make_char_const_construction(char, maybe(string), hlds_goal, prog_var,
	proc_info, proc_info).
:- mode make_char_const_construction(in, in, out, out, in, out) is det.

:- pred make_const_construction(cons_id, (type), maybe(string), hlds_goal,
	prog_var, proc_info, proc_info).
:- mode make_const_construction(in, in, in, out, out, in, out) is det.

	% Given the variable info field from a pragma foreign_code, get all the
	% variable names.
:- pred get_pragma_foreign_var_names(list(maybe(pair(string, mode))),
		list(string)).
:- mode get_pragma_foreign_var_names(in, out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff specific to Aditi.
%

	% Builtin Aditi operations.
:- type aditi_builtin
	--->
		% Call an Aditi predicate from Mercury compiled to C.
		% This is introduced by magic.m.
		% Arguments:
		%   type-infos for the input arguments
		%   the input arguments
		%   type-infos for the output arguments
		%   the output arguments
		aditi_call(
			pred_proc_id,	% procedure to call
			int,		% number of inputs
			list(type),	% types of input arguments
			int		% number of outputs
		)

		% Insert or delete a single tuple into/from a base relation.
		% Arguments:
		%   type-infos for the arguments of the tuple to insert
		%   the arguments of tuple to insert
		% aditi__state::di, aditi__state::uo
	;	aditi_tuple_insert_delete(
			aditi_insert_delete,
			pred_id		% base relation to insert into
		)

		% Insert/delete/modify operations which take
		% an input closure.
		% These operations all have two variants.
		%
		% A pretty syntax:
		%
		% aditi_bulk_insert(p(DB, X, Y) :- q(DB, X, Y)).
		% aditi_bulk_delete(p(DB, X, Y) :- q(DB, X, Y)).
		% aditi_bulk_modify(
		%	(p(DB, X0, Y0) ==> p(_, X, Y) :-
		%		X = X0 + 1,
		%		Y = Y0 + 3
		%	)).
		%
		% An ugly syntax:
		%
		% InsertPred = (aditi_bottom_up
		%	pred(DB::aditi_mui, X::out, Y::out) :-
		%		q(DB, X, Y)
		% ),
		% aditi_bulk_insert(pred p/3, InsertPred).
		%
		% DeletePred = (aditi_bottom_up
		%	pred(DB::aditi_mui, X::out, Y::out) :-
		%		p(DB, X, Y),
		%		q(DB, X, Y)
		% ),
		% aditi_bulk_delete(pred p/3, DeletePred).
	;	aditi_insert_delete_modify(
			aditi_insert_delete_modify,
			pred_id,
			aditi_builtin_syntax
		)
	.

:- type aditi_insert_delete
	--->	delete			% `aditi_delete'
	;	insert			% `aditi_insert'
	.

:- type aditi_insert_delete_modify
	--->	delete(bulk_or_filter)	% `aditi_bulk_delete' or `aditi_filter'
	;	bulk_insert		% `aditi_bulk_insert'
	;	modify(bulk_or_filter)	% `aditi_bulk_modify' or `aditi_modify'
	.

	% Deletions and modifications can either be done by computing
	% all tuples for which the update applies, then applying the
	% update for all tuples in one go (`bulk'), or by applying
	% the update to each tuple during a pass over the relation
	% being modified (`filter').
	%
	% The `filter' updates are not yet implemented in Aditi, and
	% it may be difficult to ever implement them.
:- type bulk_or_filter
	--->	bulk
	;	filter
	.

	% Which syntax was used for an `aditi_delete' or `aditi_modify'
	% call. The first syntax is prettier, the second is used
	% where the closure to be passed in is not known at the call site.
	% (See the "Aditi update syntax" section of the Mercury Language
	% Reference Manual).
:- type aditi_builtin_syntax
	--->	pred_term		% e.g.
					% aditi_bulk_insert(p(_, X) :- X = 1).
	;	sym_name_and_closure	% e.g.
					% aditi_insert(p/2,
					%    (pred(_::in, X::out) is nondet:-
					%	X = 1)
					%    )
	.

	% For lambda expressions built automatically for Aditi updates
	% the modes of `aditi__state' arguments may need to be fixed
	% by purity.m because make_hlds.m does not know which relation
	% is being updated, so it doesn't know which are the `aditi__state'
	% arguments.
:- type fix_aditi_state_modes
	--->	modes_need_fixing
	;	modes_are_ok
	.

%-----------------------------------------------------------------------------%
%
% Stuff specific to a back-end. At the moment, only the LLDS back-end
% annotates the HLDS.
%

:- type hlds_goal_code_gen_info
	--->	no_code_gen_info
	;	llds_code_gen_info(llds_code_gen :: llds_code_gen_details).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__det_analysis.
:- import_module check_hlds__purity.
:- import_module check_hlds__type_util.
:- import_module parse_tree__prog_util.

:- import_module require, string, term, varset.

%-----------------------------------------------------------------------------%
%
% Predicates dealing with generic_calls
%

hlds_goal__generic_call_id(higher_order(_, Purity, PorF, Arity),
		generic_call(higher_order(Purity, PorF, Arity))).
hlds_goal__generic_call_id(
		class_method(_, _, ClassId, MethodId),
		generic_call(class_method(ClassId, MethodId))).
hlds_goal__generic_call_id(aditi_builtin(Builtin, Name),
		generic_call(aditi_builtin(Builtin, Name))).

generic_call_pred_or_func(higher_order(_, _, PredOrFunc, _)) = PredOrFunc.
generic_call_pred_or_func(class_method(_, _, _, CallId)) =
	simple_call_id_pred_or_func(CallId).
generic_call_pred_or_func(aditi_builtin(_, CallId)) =
	simple_call_id_pred_or_func(CallId).

:- func simple_call_id_pred_or_func(simple_call_id) = pred_or_func.

simple_call_id_pred_or_func(PredOrFunc - _) = PredOrFunc.

%-----------------------------------------------------------------------------%
%
% Information stored with all kinds of goals
%

	% NB. Don't forget to check goal_util__name_apart_goalinfo
	% if this structure is modified.
:- type hlds_goal_info
	---> goal_info(
		determinism :: determinism,
				% the overall determinism of the goal
				% (computed during determinism analysis)
				% [because true determinism is undecidable,
				% this may be a conservative approximation]

		instmap_delta :: instmap_delta,
				% the change in insts over this goal
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
				%
				% Normally the instmap_delta will list only
				% the nonlocal variables of the goal.

		context :: prog_context,

		nonlocals :: set(prog_var),
				% the non-local vars in the goal,
				% i.e. the variables that occur both inside
				% and outside of the goal.
				% (computed by quantification.m)
				% [in some circumstances, this may be a
				% conservative approximation: it may be
				% a superset of the real non-locals]

		/*
		code_gen_nonlocals :: maybe(set(prog_var)),
				% the non-local vars in the goal,
				% modified slightly for code generation.
				% The difference between the code-gen nonlocals
				% and the ordinary nonlocals is that arguments
				% of a reconstruction which are taken from the
				% reused cell are not considered to be
				% `code_gen_nonlocals' of the goal.
				% This avoids allocating stack slots and
				% generating unnecessary field extraction
				% instructions for those arguments.
				% Mode information is still computed using
				% the ordinary non-locals.
				%
				% If the field has value `no', the ordinary
				% nonlocals are used instead. This will
				% be the case if the procedure body does not
				% contain any reconstructions.
		*/

		features :: set(goal_feature),
				% The set of used-defined "features" of
				% this goal, which optimisers may wish
				% to know about.

		goal_path :: goal_path,
				% The path to this goal from the root in
				% reverse order.

		code_gen_info :: hlds_goal_code_gen_info
	).

:- pragma inline(goal_info_init/1).
goal_info_init(GoalInfo) :-
	Detism = erroneous,
	instmap_delta_init_unreachable(InstMapDelta),
	set__init(NonLocals),
	term__context_init(Context),
	set__init(Features),
	GoalInfo = goal_info(Detism, InstMapDelta, Context, NonLocals,
		Features, [], no_code_gen_info).

:- pragma inline(goal_info_init/2).
goal_info_init(Context, GoalInfo) :-
	Detism = erroneous,
	instmap_delta_init_unreachable(InstMapDelta),
	set__init(NonLocals),
	set__init(Features),
	GoalInfo = goal_info(Detism, InstMapDelta, Context, NonLocals,
		Features, [], no_code_gen_info).

goal_info_init(NonLocals, InstMapDelta, Detism, Purity, GoalInfo) :-
	term__context_init(Context),
	purity_features(Purity, _, Features),
	GoalInfo = goal_info(Detism, InstMapDelta, Context, NonLocals,
		list_to_set(Features), [], no_code_gen_info).

goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context, GoalInfo) :-
	purity_features(Purity, _, Features),
	GoalInfo = goal_info(Detism, InstMapDelta, Context, NonLocals,
		list_to_set(Features), [], no_code_gen_info).

goal_info_get_determinism(GoalInfo, GoalInfo ^ determinism).

goal_info_get_instmap_delta(GoalInfo, GoalInfo ^ instmap_delta).

goal_info_get_context(GoalInfo, GoalInfo ^ context).

goal_info_get_nonlocals(GoalInfo, GoalInfo ^ nonlocals).

	% The code-gen non-locals are always the same as the
	% non-locals when structure reuse is not being performed.
goal_info_get_code_gen_nonlocals(GoalInfo, NonLocals) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals).

goal_info_get_features(GoalInfo, GoalInfo ^ features).

goal_info_get_goal_path(GoalInfo, GoalInfo ^ goal_path).

goal_info_get_code_gen_info(GoalInfo, GoalInfo ^ code_gen_info).

goal_info_set_determinism(GoalInfo0, Determinism,
		GoalInfo0 ^ determinism := Determinism).

goal_info_set_instmap_delta(GoalInfo0, InstMapDelta,
		GoalInfo0 ^ instmap_delta := InstMapDelta).

goal_info_set_context(GoalInfo0, Context, GoalInfo0 ^ context := Context).

goal_info_set_nonlocals(GoalInfo0, NonLocals,
		GoalInfo0 ^ nonlocals := NonLocals).

	% The code-gen non-locals are always the same as the
	% non-locals when structure reuse is not being performed.
goal_info_set_code_gen_nonlocals(GoalInfo0, NonLocals, GoalInfo) :-
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo).

goal_info_set_features(GoalInfo0, Features, GoalInfo0 ^ features := Features).

goal_info_set_goal_path(GoalInfo0, GoalPath,
		GoalInfo0 ^ goal_path := GoalPath).

goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo,
		GoalInfo0 ^ code_gen_info := CodeGenInfo).

%-----------------------------------------------------------------------------%

add_goal_info_purity_feature(GoalInfo0, Purity, GoalInfo) :-
	infer_goal_info_purity(GoalInfo0, Purity0),
	( Purity = Purity0 ->
		GoalInfo = GoalInfo0
	;
		purity_features(Purity, FeaturesToRemove, FeaturesToAdd),
		goal_info_get_features(GoalInfo0, Features0),
		Features = set__union(list_to_set(FeaturesToAdd),
				set__difference(Features0,
					list_to_set(FeaturesToRemove))),
		goal_info_set_features(GoalInfo0, Features, GoalInfo)
	).

:- pred purity_features(purity::in, list(goal_feature)::out,
		list(goal_feature)::out) is det.

purity_features(pure, [(impure), (semipure)], []).
purity_features((semipure), [(impure)], [(semipure)]).
purity_features((impure), [(semipure)], [(impure)]).

infer_goal_info_purity(GoalInfo, Purity) :-
	( goal_info_has_feature(GoalInfo, (impure)) ->
		Purity = (impure)
	; goal_info_has_feature(GoalInfo, (semipure)) ->
		Purity = (semipure)
	;
		Purity = pure
	).
			
goal_info_is_pure(GoalInfo) :-
	\+ goal_info_has_feature(GoalInfo, (impure)),
	\+ goal_info_has_feature(GoalInfo, (semipure)).
	
goal_info_is_impure(GoalInfo) :-
	goal_info_has_feature(GoalInfo, (impure)).
	
%-----------------------------------------------------------------------------%

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

goal_get_nonlocals(_Goal - GoalInfo, NonLocals) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals).

goal_add_feature(Goal - GoalInfo0, Feature, Goal - GoalInfo) :-
	goal_info_add_feature(GoalInfo0, Feature, GoalInfo).

goal_remove_feature(Goal - GoalInfo0, Feature, Goal - GoalInfo) :-
	goal_info_remove_feature(GoalInfo0, Feature, GoalInfo).

goal_has_feature(_Goal - GoalInfo, Feature) :-
	goal_info_has_feature(GoalInfo, Feature).

%-----------------------------------------------------------------------------%
%
% Miscellaneous utility procedures for dealing with HLDS goals
%

	% Convert a goal to a list of conjuncts.
	% If the goal is a conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.

goal_to_conj_list(Goal, ConjList) :-
	( Goal = (conj(List) - _) ->
		ConjList = List
	;
		ConjList = [Goal]
	).

	% Convert a goal to a list of parallel conjuncts.
	% If the goal is a conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.

goal_to_par_conj_list(Goal, ConjList) :-
	( Goal = par_conj(List) - _ ->
		ConjList = List
	;
		ConjList = [Goal]
	).

	% Convert a goal to a list of disjuncts.
	% If the goal is a disjunction, then return its disjuncts
	% otherwise return the goal as a singleton list.

goal_to_disj_list(Goal, DisjList) :-
	( Goal = disj(List) - _ ->
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

	% Convert a list of parallel conjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the parallel conjunction of the conjuncts,
	% with the specified goal_info.

par_conj_list_to_goal(ConjList, GoalInfo, Goal) :-
	( ConjList = [Goal0] ->
		Goal = Goal0
	;
		Goal = par_conj(ConjList) - GoalInfo
	).

	% Convert a list of disjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the disjunction of the conjuncts,
	% with the specified goal_info.

disj_list_to_goal(DisjList, GoalInfo, Goal) :-
	( DisjList = [Goal0] ->
		Goal = Goal0
	;
		Goal = disj(DisjList) - GoalInfo
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

	% Negate a goal, eliminating double negations as we go.

negate_goal(Goal, GoalInfo, NegatedGoal) :-
	(
		% eliminate double negations
		Goal = not(Goal1) - _
	->
		NegatedGoal = Goal1
	;
		% convert negated conjunctions of negations
		% into disjunctions
		Goal = conj(NegatedGoals) - _,
		all_negated(NegatedGoals, UnnegatedGoals)
	->
		NegatedGoal = disj(UnnegatedGoals) - GoalInfo
	;
		NegatedGoal = not(Goal) - GoalInfo
	).

:- pred all_negated(list(hlds_goal), list(hlds_goal)).
:- mode all_negated(in, out) is semidet.

all_negated([], []).
all_negated([not(Goal) - _ | NegatedGoals], [Goal | Goals]) :-
	all_negated(NegatedGoals, Goals).
all_negated([conj(NegatedConj) - _GoalInfo | NegatedGoals], Goals) :-
	all_negated(NegatedConj, Goals1),
	all_negated(NegatedGoals, Goals2),
	list__append(Goals1, Goals2, Goals).

%-----------------------------------------------------------------------------%
% Returns yes if a goal (or subgoal contained within) contains any foreign
% code
goal_has_foreign(Goal) = HasForeign :-
	Goal = GoalExpr - _,
	(
		GoalExpr = conj(Goals),
		HasForeign = goal_list_has_foreign(Goals)
	;
		GoalExpr = call(_, _, _, _, _, _),
		HasForeign = no
	;
		GoalExpr = generic_call(_, _, _, _),
		HasForeign = no
	;
		GoalExpr = switch(_, _, _),
		HasForeign = no
	;
		GoalExpr = unify(_, _, _, _, _),
		HasForeign = no
	;
		GoalExpr = disj(Goals),
		HasForeign = goal_list_has_foreign(Goals)
	;
		GoalExpr = not(Goal2),
		HasForeign = goal_has_foreign(Goal2)
	;
		GoalExpr = some(_, _, Goal2),
		HasForeign = goal_has_foreign(Goal2)
	;
		GoalExpr = if_then_else(_, Goal2, Goal3, Goal4),
		HasForeign =
		(	goal_has_foreign(Goal2) = yes
		->	yes
		;	goal_has_foreign(Goal3) = yes
		->	yes
		;	goal_has_foreign(Goal4) = yes
		->	yes
		;	no
		)
	;
		GoalExpr = foreign_proc(_, _, _, _, _, _, _),
		HasForeign = yes
	;
		GoalExpr = par_conj(Goals),
		HasForeign = goal_list_has_foreign(Goals)
	;
		GoalExpr = shorthand(ShorthandGoal),
		HasForeign = goal_has_foreign_shorthand(ShorthandGoal)
	).

	% Return yes if the shorthand goal contains any foreign code
:- func goal_has_foreign_shorthand(shorthand_goal_expr) = bool.
:- mode goal_has_foreign_shorthand(in) = out is det.

goal_has_foreign_shorthand(bi_implication(Goal2, Goal3)) = HasForeign :-
	HasForeign =
	(	goal_has_foreign(Goal2) = yes
	->	yes
	;	goal_has_foreign(Goal3) = yes
	->	yes
	;	no
	).

goal_list_has_foreign([]) = no.
goal_list_has_foreign([X | Xs]) =
	(	goal_has_foreign(X) = yes
	->	yes
	;	goal_list_has_foreign(Xs)
	).

%-----------------------------------------------------------------------------%

goal_is_atomic(conj([])).
goal_is_atomic(disj([])).
goal_is_atomic(generic_call(_,_,_,_)).
goal_is_atomic(call(_,_,_,_,_,_)).
goal_is_atomic(unify(_,_,_,_,_)).
goal_is_atomic(foreign_proc(_,_,_,_,_,_,_)).

%-----------------------------------------------------------------------------%

true_goal(conj([]) - GoalInfo) :-
	instmap_delta_init_reachable(InstMapDelta),
	goal_info_init(set__init, InstMapDelta, det, pure, GoalInfo).

true_goal(Context, Goal - GoalInfo) :-
	true_goal(Goal - GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo).

fail_goal(disj([]) - GoalInfo) :-
	instmap_delta_init_unreachable(InstMapDelta),
	goal_info_init(set__init, InstMapDelta, failure, pure, GoalInfo).

fail_goal(Context, Goal - GoalInfo) :-
	fail_goal(Goal - GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo).

%-----------------------------------------------------------------------------%

goal_list_nonlocals(Goals, NonLocals) :-
       UnionNonLocals =
               lambda([Goal::in, Vars0::in, Vars::out] is det, (
                       Goal = _ - GoalInfo,
                       goal_info_get_nonlocals(GoalInfo, Vars1),
                       set__union(Vars0, Vars1, Vars)
               )),
       set__init(NonLocals0),
       list__foldl(UnionNonLocals, Goals, NonLocals0, NonLocals).

goal_list_instmap_delta(Goals, InstMapDelta) :-
       ApplyDelta =
               lambda([Goal::in, Delta0::in, Delta::out] is det, (
                       Goal = _ - GoalInfo,
                       goal_info_get_instmap_delta(GoalInfo, Delta1),
                       instmap_delta_apply_instmap_delta(Delta0,
                               Delta1, Delta)
               )),
       instmap_delta_init_reachable(InstMapDelta0),
       list__foldl(ApplyDelta, Goals, InstMapDelta0, InstMapDelta).

goal_list_determinism(Goals, Determinism) :-
       ComputeDeterminism =
               lambda([Goal::in, Det0::in, Det::out] is det, (
                       Goal = _ - GoalInfo,
                       goal_info_get_determinism(GoalInfo, Det1),
                       det_conjunction_detism(Det0, Det1, Det)
               )),
       list__foldl(ComputeDeterminism, Goals, det, Determinism).

goal_list_purity(Goals, Purity) :-
	Purity = list__foldl(
			(func(_ - GoalInfo, Purity0) = Purity1 :-
				infer_goal_info_purity(GoalInfo, GoalPurity),
		    		worst_purity(GoalPurity, Purity0, Purity1)
			), Goals, pure).

%-----------------------------------------------------------------------------%

set_goal_contexts(Context, Goal0 - GoalInfo0, Goal - GoalInfo) :-
	goal_info_set_context(GoalInfo0, Context, GoalInfo),
	set_goal_contexts_2(Context, Goal0, Goal).

:- pred set_goal_contexts_2(prog_context, hlds_goal_expr, hlds_goal_expr).
:- mode set_goal_contexts_2(in, in, out) is det.

set_goal_contexts_2(Context, conj(Goals0), conj(Goals)) :-
	list__map(set_goal_contexts(Context), Goals0, Goals).
set_goal_contexts_2(Context, disj(Goals0), disj(Goals)) :-
	list__map(set_goal_contexts(Context), Goals0, Goals).
set_goal_contexts_2(Context, par_conj(Goals0), par_conj(Goals)) :-
	list__map(set_goal_contexts(Context), Goals0, Goals).
set_goal_contexts_2(Context, if_then_else(Vars, Cond0, Then0, Else0),
		if_then_else(Vars, Cond, Then, Else)) :-
	set_goal_contexts(Context, Cond0, Cond),
	set_goal_contexts(Context, Then0, Then),
	set_goal_contexts(Context, Else0, Else).
set_goal_contexts_2(Context, switch(Var, CanFail, Cases0),
		switch(Var, CanFail, Cases)) :-
	list__map(
	    (pred(case(ConsId, Goal0)::in, case(ConsId, Goal)::out) is det :-
		set_goal_contexts(Context, Goal0, Goal)
	    ), Cases0, Cases).
set_goal_contexts_2(Context, some(Vars, CanRemove, Goal0),
		some(Vars, CanRemove, Goal)) :-
	set_goal_contexts(Context, Goal0, Goal).
set_goal_contexts_2(Context, not(Goal0), not(Goal)) :-
	set_goal_contexts(Context, Goal0, Goal).
set_goal_contexts_2(_, Goal, Goal) :-
	Goal = call(_, _, _, _, _, _).
set_goal_contexts_2(_, Goal, Goal) :-
	Goal = generic_call(_, _, _, _).
set_goal_contexts_2(_, Goal, Goal) :-
	Goal = unify(_, _, _, _, _).
set_goal_contexts_2(_, Goal, Goal) :-
	Goal = foreign_proc(_, _, _, _, _, _, _).
set_goal_contexts_2(Context, shorthand(ShorthandGoal0),
		shorthand(ShorthandGoal)) :-
	set_goal_contexts_2_shorthand(Context, ShorthandGoal0,
		ShorthandGoal).

:- pred set_goal_contexts_2_shorthand(prog_context, shorthand_goal_expr,
		shorthand_goal_expr).
:- mode set_goal_contexts_2_shorthand(in, in, out) is det.

set_goal_contexts_2_shorthand(Context, bi_implication(LHS0, RHS0),
		bi_implication(LHS, RHS)) :-
	set_goal_contexts(Context, LHS0, LHS),
	set_goal_contexts(Context, RHS0, RHS).

%-----------------------------------------------------------------------------%

create_atomic_unification(A, B, Context, UnifyMainContext, UnifySubContext,
		Goal) :-
	UMode = ((free - free) -> (free - free)),
	Mode = ((free -> free) - (free -> free)),
	UnifyInfo = complicated_unify(UMode, can_fail, []),
	UnifyC = unify_context(UnifyMainContext, UnifySubContext),
	goal_info_init(Context, GoalInfo),
	Goal = unify(A, B, Mode, UnifyInfo, UnifyC) - GoalInfo.

%-----------------------------------------------------------------------------%

make_int_const_construction(Int, MaybeName, Goal, Var, ProcInfo0, ProcInfo) :-
	proc_info_create_var_from_type(ProcInfo0, int_type, MaybeName,
		Var, ProcInfo),
	make_int_const_construction(Var, Int, Goal).

make_string_const_construction(String, MaybeName, Goal, Var,
		ProcInfo0, ProcInfo) :-
	proc_info_create_var_from_type(ProcInfo0, string_type, MaybeName,
		Var, ProcInfo),
	make_string_const_construction(Var, String, Goal).

make_float_const_construction(Float, MaybeName, Goal, Var,
		ProcInfo0, ProcInfo) :-
	proc_info_create_var_from_type(ProcInfo0, float_type, MaybeName,
		Var, ProcInfo),
	make_float_const_construction(Var, Float, Goal).

make_char_const_construction(Char, MaybeName, Goal, Var,
		ProcInfo0, ProcInfo) :-
	proc_info_create_var_from_type(ProcInfo0, char_type, MaybeName,
		Var, ProcInfo),
	make_char_const_construction(Var, Char, Goal).

make_const_construction(ConsId, Type, MaybeName, Goal, Var,
		ProcInfo0, ProcInfo) :-
	proc_info_create_var_from_type(ProcInfo0, Type, MaybeName,
		Var, ProcInfo),
	make_const_construction(Var, ConsId, Goal).

make_int_const_construction(Int, MaybeName, Goal, Var, VarTypes0, VarTypes,
		VarSet0, VarSet) :-
	varset__new_maybe_named_var(VarSet0, MaybeName, Var, VarSet),
	map__det_insert(VarTypes0, Var, int_type, VarTypes),
	make_int_const_construction(Var, Int, Goal).

make_string_const_construction(String, MaybeName, Goal, Var,
		VarTypes0, VarTypes, VarSet0, VarSet) :-
	varset__new_maybe_named_var(VarSet0, MaybeName, Var, VarSet),
	map__det_insert(VarTypes0, Var, string_type, VarTypes),
	make_string_const_construction(Var, String, Goal).

make_float_const_construction(Float, MaybeName, Goal, Var,
		VarTypes0, VarTypes, VarSet0, VarSet) :-
	varset__new_maybe_named_var(VarSet0, MaybeName, Var, VarSet),
	map__det_insert(VarTypes0, Var, float_type, VarTypes),
	make_float_const_construction(Var, Float, Goal).

make_char_const_construction(Char, MaybeName, Goal, Var,
		VarTypes0, VarTypes, VarSet0, VarSet) :-
	varset__new_maybe_named_var(VarSet0, MaybeName, Var, VarSet),
	map__det_insert(VarTypes0, Var, char_type, VarTypes),
	make_char_const_construction(Var, Char, Goal).

make_const_construction(ConsId, Type, MaybeName, Goal, Var,
		VarTypes0, VarTypes, VarSet0, VarSet) :-
	varset__new_maybe_named_var(VarSet0, MaybeName, Var, VarSet),
	map__det_insert(VarTypes0, Var, Type, VarTypes),
	make_const_construction(Var, ConsId, Goal).

make_int_const_construction(Var, Int, Goal) :-
	make_const_construction(Var, int_const(Int), Goal).

make_string_const_construction(Var, String, Goal) :-
	make_const_construction(Var, string_const(String), Goal).

make_float_const_construction(Var, Float, Goal) :-
	make_const_construction(Var, float_const(Float), Goal).

make_char_const_construction(Var, Char, Goal) :-
	string__char_to_string(Char, String),
	make_const_construction(Var, cons(unqualified(String), 0), Goal).

make_const_construction(Var, ConsId, Goal - GoalInfo) :-
	RHS = functor(ConsId, no, []),
	Inst = bound(unique, [functor(ConsId, [])]),
	Mode = (free -> Inst) - (Inst -> Inst),
	RLExprnId = no,
	Unification = construct(Var, ConsId, [], [],
		construct_dynamically, cell_is_unique, RLExprnId),
	Context = unify_context(explicit, []),
	Goal = unify(Var, RHS, Mode, Unification, Context),
	set__singleton_set(NonLocals, Var),
	instmap_delta_init_reachable(InstMapDelta0),
	instmap_delta_insert(InstMapDelta0, Var, Inst, InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, pure, GoalInfo).

%-----------------------------------------------------------------------------%

get_pragma_foreign_var_names(MaybeVarNames, VarNames) :-
	get_pragma_foreign_var_names_2(MaybeVarNames, [], VarNames0),
	list__reverse(VarNames0, VarNames).

:- pred get_pragma_foreign_var_names_2(list(maybe(pair(string, mode)))::in,
	list(string)::in, list(string)::out) is det.

get_pragma_foreign_var_names_2([], Names, Names).
get_pragma_foreign_var_names_2([MaybeName | MaybeNames], Names0, Names) :-
	(
		MaybeName = yes(Name - _),
		Names1 = [Name | Names0]
	;
		MaybeName = no,
		Names1 = Names0
	),
	get_pragma_foreign_var_names_2(MaybeNames, Names1, Names).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
