%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% HLDS - The High-Level Data Structure.

% Main authors: fjh, conway.

% This file contains the data types for the high-level data structure.
% The file is arranged as follows: first all the data structures are
% listed. Most of these are private to this module, with access
% predicates provided.
% Then for each data structure, we give the interface and then
% the implementation for the access predicates for that data structure.
%
% Although most of the data structures are private, it is a quite thin
% layer of abstraction.
%
% WARNING: changes here will probably require changes in make_hlds.m
% and elsewhere.

:- module hlds.
:- interface.
:- import_module bool, float, int, string, list, set, map, std_util, relation.
:- import_module varset, term.
:- import_module prog_io, llds, special_pred.

:- implementation.
:- import_module prog_util, mode_util, unify_proc, shapes, require.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- type module_info.

:- type predicate_table.
:- type pred_table	==	map(pred_id, pred_info).
:- type pred_id.
:- type pred_info.

:- type pred_call_id	--->	sym_name / arity.

:- type proc_table	==	map(proc_id, proc_info).
	% a proc_id is a mode number within a particular predicate -
	% not to be confused with a mode_id, which is the name of a
	% user-defined mode.
:- type proc_id		==	int.
:- type proc_info.

:- type pred_proc_id	--->	proc(pred_id, proc_id).
:- type pred_proc_list	==	list(pred_proc_id).

	% the type of goals that have been given for a pred.
:- type goal_type 	--->	pragmas		% pragma(c_code, ...)
			;	clauses		
			;	none.

%-----------------------------------------------------------------------------%

:- implementation.

:- type module_info	--->	module(
					string,		% module name
					c_code_info,	
					predicate_table,
					unify_requests,
					special_pred_map,
					shape_info,
					type_table,
					inst_table,
					mode_table,
					cons_table,
					maybe(dependency_info),
					int,		% number of errors
					%%% num_warnings not used:
					%%% int,	% number of warnings
					int	% lambda predicate counter
				).

:- interface.

:- type unify_requests.

:- type clauses_info	--->	clauses_info(
					varset,		% variable names
					map(var, type), % variable types
					list(var),	% head vars
					list(clause)
				).

:- type clause		--->	clause(
					list(proc_id),	% modes for which
							% this clause applies
					hlds__goal,	% Body
					term__context
				).

:- type c_code_info 	--->	c_code_info(
					c_header_info,
					c_body_info
				).
:- implementation.

:- type proc_info	--->	procedure(
					maybe(determinism),% _declared_ det'ism
					varset,		% variable names
					map(var, type),	% variable types
					list(var),	% head vars
					list(mode), 	% modes of args
					hlds__goal,	% Body
					term__context,	% The context of
							% the :- mode decl,
							% not the clause.
					call_info,	% stack allocations
					determinism,	% _inferred_ det'ism
					bool,		% no if we must not
						% process this procedure yet
						% (used for complicated modes
						% of unification procs)
					list(arg_info),	% information about
							% the arguments
							% derived from the
							% modes etc
					liveness_info,	% the initial liveness
					follow_vars	% initial followvars
				).

:- interface.

:- type can_fail	--->	can_fail ; cannot_fail.

:- type soln_count
	--->	at_most_zero
	;	at_most_one
	;	at_most_many_cc
			% "_cc" means "committed-choice": there is
			% more than one logical solution, but
			% the pred or goal is being used in a context
			% where we are only looking for the first
			% solution.
	;	at_most_many.

:- type soln_context	--->	all_solns ; first_soln.

:- type liveness_info	==	set(var).	% The live variables

:- type liveness	--->	live
			;	dead.

%-----------------------------------------------------------------------------%

:- pred determinism_components(determinism, can_fail, soln_count).
:- mode determinism_components(in, out, out) is det.
:- mode determinism_components(out, in, in) is det.

:- pred determinism_to_code_model(determinism, code_model).
:- mode determinism_to_code_model(in, out) is det.

:- implementation.

determinism_components(det,         cannot_fail, at_most_one).
determinism_components(semidet,     can_fail,    at_most_one).
determinism_components(multidet,    cannot_fail, at_most_many).
determinism_components(nondet,      can_fail,    at_most_many).
determinism_components(cc_multidet, cannot_fail, at_most_many_cc).
determinism_components(cc_nondet,   can_fail,    at_most_many_cc).
determinism_components(erroneous,   cannot_fail, at_most_zero).
determinism_components(failure,     can_fail,    at_most_zero).

determinism_to_code_model(det,         model_det).
determinism_to_code_model(semidet,     model_semi).
determinism_to_code_model(nondet,      model_non).
determinism_to_code_model(multidet,    model_non).
determinism_to_code_model(cc_nondet,   model_semi).
determinism_to_code_model(cc_multidet, model_det).
determinism_to_code_model(erroneous,   model_det).
determinism_to_code_model(failure,     model_semi).

%-----------------------------------------------------------------------------%

:- interface.

:- type shape_id	==	pair(type, inst).

:- type shape_info	--->	shape_info(shape_table, abs_exports).

:- type abs_exports	==	map(type_id, maybe_shape_num).

:- type maybe_shape_num --->	yes(shape_num)
			;	no(type).

:- type shape_table	==	pair(map(shape_id, pair(shape_num, shape)),int).

:- type shape		--->	quad(shape_tag, shape_tag, shape_tag,
					 shape_tag)
			;	abstract(type, list(shape_num))
			;	equivalent(shape_num)
			;	polymorphic(type, int)
			;	closure(type).

:- type shape_tag	--->	constant
			;	simple(list(pair(shape_num, shape_id)))
			;	complicated(list(list(pair(shape_num, shape_id)))).

%-----------------------------------------------------------------------------%

	% The symbol table for types.

:- type type_id		== 	pair(sym_name, arity).
				% name, arity

:- type type_table	==	map(type_id, hlds__type_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for modes.

:- type mode_id		==	pair(sym_name, arity).
				% name, arity

:- type mode_table	==	map(mode_id, hlds__mode_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for insts.

:- type inst_id		==	pair(sym_name, arity).
				% name, arity.

:- type inst_table.

:- type user_inst_table	==	map(inst_id, hlds__inst_defn).

:- type unify_inst_table ==	map(inst_name, maybe_inst_det).

:- type unify_inst_pair	--->	unify_inst_pair(is_live, inst, inst,
					unify_is_real).

:- type merge_inst_table ==	map(pair(inst), maybe_inst).

:- type ground_inst_table == 	map(inst_name, maybe_inst).

:- type shared_inst_table == 	map(inst_name, maybe_inst).

:- type mostly_uniq_inst_table == map(inst_name, maybe_inst).

:- type maybe_inst	--->	unknown
			;	known(inst).

:- type maybe_inst_det	--->	unknown
			;	known(inst, determinism).

:- pred inst_table_init(inst_table).
:- mode inst_table_init(out) is det.

:- pred inst_table_get_user_insts(inst_table, user_inst_table).
:- mode inst_table_get_user_insts(in, out) is det.

:- pred inst_table_get_unify_insts(inst_table, unify_inst_table).
:- mode inst_table_get_unify_insts(in, out) is det.

:- pred inst_table_get_merge_insts(inst_table, merge_inst_table).
:- mode inst_table_get_merge_insts(in, out) is det.

:- pred inst_table_get_ground_insts(inst_table, ground_inst_table).
:- mode inst_table_get_ground_insts(in, out) is det.

:- pred inst_table_get_shared_insts(inst_table, shared_inst_table).
:- mode inst_table_get_shared_insts(in, out) is det.

:- pred inst_table_get_mostly_uniq_insts(inst_table, mostly_uniq_inst_table).
:- mode inst_table_get_mostly_uniq_insts(in, out) is det.

:- pred inst_table_set_user_insts(inst_table, user_inst_table, inst_table).
:- mode inst_table_set_user_insts(in, in, out) is det.

:- pred inst_table_set_unify_insts(inst_table, unify_inst_table, inst_table).
:- mode inst_table_set_unify_insts(in, in, out) is det.

:- pred inst_table_set_merge_insts(inst_table, merge_inst_table, inst_table).
:- mode inst_table_set_merge_insts(in, in, out) is det.

:- pred inst_table_set_ground_insts(inst_table, ground_inst_table, inst_table).
:- mode inst_table_set_ground_insts(in, in, out) is det.

:- pred inst_table_set_shared_insts(inst_table, shared_inst_table, inst_table).
:- mode inst_table_set_shared_insts(in, in, out) is det.

:- pred inst_table_set_mostly_uniq_insts(inst_table, mostly_uniq_inst_table,
					inst_table).
:- mode inst_table_set_mostly_uniq_insts(in, in, out) is det.

:- implementation.

:- type inst_table
	--->	inst_table(
			user_inst_table,
			unify_inst_table,
			merge_inst_table,
			ground_inst_table,
			shared_inst_table,
			mostly_uniq_inst_table
		).

inst_table_init(inst_table(UserInsts, UnifyInsts, MergeInsts, GroundInsts,
				SharedInsts, NondetLiveInsts)) :-
	map__init(UserInsts),
	map__init(UnifyInsts),
	map__init(MergeInsts),
	map__init(GroundInsts),
	map__init(SharedInsts),
	map__init(NondetLiveInsts).

inst_table_get_user_insts(inst_table(UserInsts, _, _, _, _, _), UserInsts).

inst_table_get_unify_insts(inst_table(_, UnifyInsts, _, _, _, _), UnifyInsts).

inst_table_get_merge_insts(inst_table(_, _, MergeInsts, _, _, _), MergeInsts).

inst_table_get_ground_insts(inst_table(_, _, _, GroundInsts, _, _),
			GroundInsts).

inst_table_get_shared_insts(inst_table(_, _, _, _, SharedInsts, _),
			SharedInsts).

inst_table_get_mostly_uniq_insts(inst_table(_, _, _, _, _, NondetLiveInsts),
			NondetLiveInsts).

inst_table_set_user_insts(inst_table(_, B, C, D, E, F), UserInsts,
			inst_table(UserInsts, B, C, D, E, F)).

inst_table_set_unify_insts(inst_table(A, _, C, D, E, F), UnifyInsts,
			inst_table(A, UnifyInsts, C, D, E, F)).

inst_table_set_merge_insts(inst_table(A, B, _, D, E, F), MergeInsts,
			inst_table(A, B, MergeInsts, D, E, F)).

inst_table_set_ground_insts(inst_table(A, B, C, _, E, F), GroundInsts,
			inst_table(A, B, C, GroundInsts, E, F)).

inst_table_set_shared_insts(inst_table(A, B, C, D, _, F), SharedInsts,
			inst_table(A, B, C, D, SharedInsts, F)).

inst_table_set_mostly_uniq_insts(inst_table(A, B, C, D, E, _), NondetLiveInsts,
			inst_table(A, B, C, D, E, NondetLiveInsts)).

%-----------------------------------------------------------------------------%

:- interface.

	% The symbol table for constructors.
	% This table is used by the type-checker to look
	% up the type of functors/constants.

:- type cons_table	==	map(cons_id, list(hlds__cons_defn)).

:- type cons_id		--->	cons(string, arity)	% name, arity
			;	int_const(int)
			;	string_const(string)
			;	float_const(float)

			;	pred_const(pred_id, proc_id)
				% Currently not used - preds are
				% currently just stored as cons(Name, Arity).
				% But that causes problems for preds
				% that are overloaded or have multiple
				% modes, so eventually we will need to
				% use `pred_const(PredId, ProcId)'.

			;	address_const(pred_id, proc_id).
				% used for constructing type_infos
				% Note that a pred_const is for a closure
				% whereas an address_const is just an address.

%-----------------------------------------------------------------------------%

:- type arg_info	--->	arg_info(
					arg_loc,	% stored location
					arg_mode	% mode of top functor
				).

:- type arg_mode	--->	top_in
			;	top_out
			;	top_unused.

:- type arg_loc == int.

%-----------------------------------------------------------------------------%

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
			sym_name,	% the name/arity of the predicate
			follow_vars	% advisory storage locations for
					% placing variables when generating
					% the code that follows this call
					% (used so we can generate more
					% efficient code by putting variables
					% in the place they will be needed)
		)

		% Deterministic disjunctions are converted
		% into case statements by the switch
		% detection pass.
	;	switch(
			var,		% the variable we are switching on
			can_fail,	% whether or not the switch test itself
					% can fail (i.e. whether or not it
					% covers all the possible cases)
			list(case)
		)

		% A unification.
		% Initially only the terms and the context
		% are known. Mode analysis fills in the
		% missing information.
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
	;	disj(hlds__goals)

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
			hlds__goal	% The <Else> part
		)
	
		% C code from a pragma(c_code, ...) decl.
	;	pragma_c_code(
			string,		% The C code to do the work
			pred_id,	% The called predicate
			proc_id, 	% The mode of the predicate
			list(var),	% The (Mercury) argument variables
			map(var, string)
				%  A map from the (Mercury) argument
				%  variables to the C variable names used
				%  in the C code.  If a variable in the
				%  argument list does not occur in this
				%  map, it means that it is not used by
				%  the C code.  (In particular, the
				%  type_info variables introduced by
				%  polymorphism.m might not occur in this
				%  map.)
		).

	% Record whether a call is a builtin or not, and if so, which one.
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

:- type unify_rhs
	--->	var(var)
	;	functor(const, list(var))
	;	lambda_goal(list(var), list(mode), determinism, hlds__goal).

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
		maybe(map(var, lval)),
				% the new store_map, if any - this records
				% where to store variables at the end of
				% branched structures.
				% (Computed by store_alloc.m)
		maybe(set(var)),
				% The `cont lives' -
				% maybe the set of variables that are
				% live when forward execution resumes
				% on the failure of some subgoal of this
				% goal. For
				% negations, it is just the set of
				% variables live after the negation.
				% For ite's it is the set of variables
				% live after the condition.
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

:- interface.

:- type unify_mode	==	pair(mode, mode).

:- type uni_mode	--->	pair(inst) -> pair(inst).
					% Each uni_mode maps a pair
					% of insts to a pair of new insts
					% Each pair represents the insts
					% of the LHS and the RHS respectively

%-----------------------------------------------------------------------------%

	% This is how type, modes and constructors are represented.
	% The parts that are not defined here (i.e. type_param, constructor,
	% type, inst, mode, condition) are represented in the same way as
	% in prog_io.m, and are defined there.

	% An hlds__type_defn holds the information about a type definition.
	%
:- type hlds__type_defn
	--->	hlds__type_defn(
			tvarset,		% Names of type vars (empty
						% except for polymorphic types)
			list(type_param),	% Formal type parameters
			hlds__type_body,	% the definition of the type

			condition,		% UNUSED
				% Reserved for holding a user-defined invariant
				% for the type, as in the NU-Prolog's type
				% checker, which allows `where' conditions on
				% type definitions.  For example:
				% :- type sorted_list(T) == list(T)
				%	where sorted.

			term__context		% the location of this type
						% definition in the original
						% source code
		).

	% An `hlds__type_body' holds the body of a type definition:
	% du = discriminated union, uu = undiscriminated union,
	% eqv_type = equivalence type (a type defined to be equivalen
	% to some other type)
	%
:- type hlds__type_body
	--->	du_type(
			list(constructor), 	% the ctors for this type
			cons_tag_values,	% their tag values
			bool		% is this type an enumeration?
		)
	;	uu_type(list(type))	% not yet implemented!
	;	eqv_type(type)
	;	abstract_type.

	% The `cons_tag_values' type stores the information on how
	% a discriminated union type is represented.
	% For each functor in the d.u. type, it gives a cons_tag
	% which specifies how that functor and its arguments are represented.
:- type cons_tag_values
	== map(cons_id, cons_tag).

	% A `cons_tag' specifies how a functor and its arguments (if any)
	% are represented.  Currently all values are represented as
	% a single word; values which do not fit into a word are represented
	% by a (possibly tagged) pointer to memory on the heap.
:- type cons_tag
	--->	string_constant(string)
			% Strings are represented using the string_const()
			% macro; in the current implementation, Mercury
			% strings are represented just as C null-terminated
			% strings.
	;	float_constant(float)
			% Floats are represented using the float_to_word(),
			% word_to_float(), and float_const() macros.
			% The default implementation of these is to
			% use boxed double-precision floats.
	;	int_constant(int)
			% This means the constant is represented just as
			% a word containing the specified integer value.
			% This is used for enumerations and character
			% constants as well as for int constants.
	;	pred_closure_tag(pred_id, proc_id)
			% Higher-order pred closures tags.
			% These are represented as a pointer to
			% an argument vector.
			% The first two words of the argument vector
			% hold the number of args and the address of
			% the procedure respectively.
			% The remaining words hold the arguments.
	;	address_constant(pred_id, proc_id)
			% Procedure address constants
			% (used for constructing type_infos).
			% The word just contains the address of the
			% specified procedure.
	;	simple_tag(tag_bits)
			% This is for constants or functors which only
			% require a simple tag.  (A "simple" tag is one
			% which fits on the bottom of a pointer - i.e.
			% two bits for 32-bit architectures, or three bits
			% for 64-bit architectures).
			% For constants we store a tagged zero, for functors
			% we store a tagged pointer to the argument vector.
	;	complicated_tag(tag_bits, int)
			% This is for functors or constants which
			% require more than just a two-bit tag. In this case,
			% we use both a primary and a secondary tag.
			% The secondary tag is stored as the first word of
			% the argument vector. (If it is a constant, then
			% in this case there is an argument vector of size 1
			% which just holds the secondary tag.)
	;	complicated_constant_tag(tag_bits, int).
			% This is for constants which require more than a
			% two-bit tag. In this case, we use both a primary
			% and a secondary tag, but this time the secondary
			% tag is stored in the rest of the main word rather
			% than in the first word of the argument vector.

	% The type `tag_bits' holds a simple tag value.
:- type tag_bits	==	int.	% actually only 2 (or maybe 3) bits

	% An `hlds__inst_defn' holds the information we need to store
	% about inst definitions such as
	%	:- inst list_skel(I) = bound([] ; [I | list_skel(I)].
	%
:- type hlds__inst_defn
	--->	hlds__inst_defn(
			varset,			% The names of the inst
						% parameters (if any).
			list(inst_param),	% The inst parameters (if any).
						% ([I] in the above example.)
			hlds__inst_body,	% The definition of this inst.
			condition,		% Unused (reserved for
						% holding a user-defined 
						% invariant).
			term__context		% The location in the source
						% code of this inst definition.
		).

:- type hlds__inst_body
	--->	eqv_inst(inst)			% This inst is equivalent to
						% some other inst.
	;	abstract_inst.			% This inst is just a forward
						% declaration; the real
						% definition will be filled in
						% later.  (XXX Abstract insts
						% are not really supported.)

	% A hlds__mode_defn stores the information about a mode
	% definition such as
	%	:- mode out :: free -> ground.
	% or
	%	:- mode in(I) :: I -> I.
	% or
	%	:- mode in_list_skel :: in(list_skel).
	%
:- type hlds__mode_defn
	--->	hlds__mode_defn(
			varset,			% The names of the inst
						% parameters (if any).
			list(inst_param),	% The list of the inst
						% parameters (if any).
						% (e.g. [I] for the second
						% example above.)
			hlds__mode_body,	% The definition of this mode.
			condition,		% Unused (reserved for
						% holding a user-defined
						% invariant).
			term__context		% The location of this mode
						% definition in the original
						% source code.
		).

	% The only sort of mode definitions allowed are equivalence modes.
	% 
:- type hlds__mode_body
	--->	eqv_mode(mode).		% This mode is equivalent to some
					% other mode.

	% A cons_defn is the definition of a constructor (i.e. a constant
	% or a functor) for a particular type.
:- type hlds__cons_defn
	--->	hlds__cons_defn(
			% maybe add tvarset?
			list(type),		% The types of the arguments
						% of this functor (if any)
			type_id,		% The result type, i.e. the
						% type to which this
						% cons_defn belongs.
			term__context		% The location of this
						% ctor definition in the
						% original source code
		).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for manipulating the module_info data structure

:- interface.

	% Create an empty module_info for a given module name.
:- pred module_info_init(string, module_info).
:- mode module_info_init(in, out) is det.

:- pred module_info_name(module_info, string).
:- mode module_info_name(in, out) is det.

:- pred module_info_get_c_header(module_info, c_header_info).
:- mode module_info_get_c_header(in, out) is det.

:- pred module_info_set_c_header(module_info, c_header_info, module_info).
:- mode module_info_set_c_header(in, in, out) is det.

:- pred module_info_get_c_body_code(module_info, c_body_info).
:- mode module_info_get_c_body_code(in, out) is det.

:- pred module_info_set_c_body_code(module_info, c_body_info, module_info).
:- mode module_info_set_c_body_code(in, in, out) is det.

:- pred module_info_get_predicate_table(module_info, predicate_table).
:- mode module_info_get_predicate_table(in, out) is det.

:- pred module_info_preds(module_info, pred_table).
:- mode module_info_preds(in, out) is det.

:- pred module_info_pred_info(module_info, pred_id, pred_info).
:- mode module_info_pred_info(in, in, out) is det.

	% Given a pred_id and a proc_id, get the
	% pred_info that predicate and the proc_info for that
	% mode of that predicate.
:- pred module_info_pred_proc_info(module_info, pred_id, proc_id,
					pred_info, proc_info).
:- mode module_info_pred_proc_info(in, in, in, out, out) is det.

	% Return a list of the pred_ids of all the "valid" predicates.
	% (Predicates whose definition contains a type error, etc.
	% get removed from this list, so that later passes can rely
	% on the predicates in this list being type-correct, etc.)
:- pred module_info_predids(module_info, list(pred_id)).
:- mode module_info_predids(in, out) is det.

	% Reverse the list of pred_ids.
	% (The list is built up by inserting values at the front,
	% for efficiency; once we've done so, we reverse the list
	% so that progress messages and error messages come out
	% in the expected order.)
:- pred module_info_reverse_predids(module_info, module_info).
:- mode module_info_reverse_predids(in, out) is det.

	% For an explanation of the unify_requests structure,
	% see unify_info.m.
:- pred module_info_get_unify_requests(module_info, unify_requests).
:- mode module_info_get_unify_requests(in, out) is det.

:- pred module_info_get_special_pred_map(module_info, special_pred_map).
:- mode module_info_get_special_pred_map(in, out) is det.

:- pred module_info_get_shapes(module_info, shape_table).
:- mode module_info_get_shapes(in, out) is det.

:- pred module_info_shape_info(module_info, shape_info).
:- mode module_info_shape_info(in, out) is det.

:- pred module_info_types(module_info, type_table).
:- mode module_info_types(in, out) is det.

:- pred module_info_typeids(module_info, list(type_id)).
:- mode module_info_typeids(in, out) is det.

:- pred module_info_insts(module_info, inst_table).
:- mode module_info_insts(in, out) is det.

:- pred module_info_instids(module_info, list(inst_id)).
:- mode module_info_instids(in, out) is det.

:- pred module_info_modes(module_info, mode_table).
:- mode module_info_modes(in, out) is det.

:- pred module_info_modeids(module_info, list(mode_id)).
:- mode module_info_modeids(in, out) is det.

:- pred module_info_ctors(module_info, cons_table).
:- mode module_info_ctors(in, out) is det.

:- pred module_info_num_errors(module_info, int).
:- mode module_info_num_errors(in, out) is det.

% not used
% :- pred module_info_num_warnings(module_info, int).
% :- mode module_info_num_warnings(in, out) is det.

:- pred module_info_consids(module_info, list(cons_id)).
:- mode module_info_consids(in, out) is det.

	% The dependency information must have been build before
	% calling this predicate.
:- pred module_info_dependency_info(module_info, dependency_info).
:- mode module_info_dependency_info(in, out) is det.

	% Succeeds iff the dependency information has already been built
:- pred module_info_dependency_info_built(module_info).
:- mode module_info_dependency_info_built(in) is semidet.

:- pred module_info_set_name(module_info, string, module_info).
:- mode module_info_set_name(in, in, out) is det.

:- pred module_info_set_predicate_table(module_info, predicate_table,
					module_info).
:- mode module_info_set_predicate_table(in, in, out) is det.

:- pred module_info_set_preds(module_info, pred_table, module_info).
:- mode module_info_set_preds(in, in, out) is det.

:- pred module_info_set_unify_requests(module_info, unify_requests,
					module_info).
:- mode module_info_set_unify_requests(in, in, out) is det.

:- pred module_info_set_special_pred_map(module_info, special_pred_map,
					module_info).
:- mode module_info_set_special_pred_map(in, in, out) is det.

:- pred module_info_set_shapes(module_info, shape_table, module_info).
:- mode module_info_set_shapes(in, in, out) is det.

:- pred module_info_set_shape_info(module_info, shape_info, module_info).
:- mode module_info_set_shape_info(in, in, out) is det.

:- pred module_info_set_types(module_info, type_table, module_info).
:- mode module_info_set_types(in, in, out) is det.

:- pred module_info_set_insts(module_info, inst_table, module_info).
:- mode module_info_set_insts(in, in, out) is det.

:- pred module_info_set_modes(module_info, mode_table, module_info).
:- mode module_info_set_modes(in, in, out) is det.

:- pred module_info_set_ctors(module_info, cons_table, module_info).
:- mode module_info_set_ctors(in, in, out) is det.

:- pred module_info_set_dependency_info(module_info, dependency_info, module_info).
:- mode module_info_set_dependency_info(in, in, out) is det.

:- pred module_info_set_num_errors(module_info, int, module_info).
:- mode module_info_set_num_errors(in, in, out) is det.

:- pred module_info_incr_errors(module_info, module_info).
:- mode module_info_incr_errors(in, out) is det.

/* not used
:- pred module_info_incr_warnings(module_info, module_info).
:- mode module_info_incr_warnings(in, out) is det.
*/

	% The module_info stores a counter which is used to number
	% introduced lambda predicates as __LambdaGoal__1, __LambdaGoal__2,
	% etc.; this predicate returns the next number and increments
	% the counter.
:- pred module_info_next_lambda_count(module_info, int, module_info).
:- mode module_info_next_lambda_count(in, out, out) is det.

	% Remove a predicate from the list of pred_ids, to prevent
	% further processing of this predicate after an error is
	% encountered.
:- pred module_info_remove_predid(module_info, pred_id, module_info).
:- mode module_info_remove_predid(in, in, out) is det.

	% Once the module_info has been built, we call module_info_optimize
	% to attempt to optimize the data structures for lots of accesses
	% and relatively few insertion/deletions.  (This was useful when
	% we were using unbalanced binary trees, but now that we are using
	% 234-trees, it is a no-op.)
:- pred module_info_optimize(module_info, module_info).
:- mode module_info_optimize(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

	% A predicate which creates an empty module

module_info_init(Name, module(Name, C_Code_Info, PredicateTable, Requests, 
				UnifyPredMap, Shapes, Types, Insts, Modes, 
				Ctors, DepInfo, 0, 0)) :-
	C_Code_Info = c_code_info([], []),
	predicate_table_init(PredicateTable),
	unify_proc__init_requests(Requests),
	map__init(UnifyPredMap),
	map__init(Types),
	inst_table_init(Insts),
	map__init(Modes),
	shapes__init_shape_table(ShapeTable),
	map__init(AbsExports),
	Shapes = shape_info(ShapeTable, AbsExports),
	map__init(Ctors),
	DepInfo = no.

	% Various access predicates which extract different pieces
	% of info from the module_info data structure.

module_info_name(ModuleInfo, Name) :-
	ModuleInfo = module(Name, _, _, _, _, _, _, _, _, _, _, _, _).

module_info_get_c_header(ModuleInfo, C_Header) :-
	ModuleInfo = module(_, C_Code_Info, _, _, _, _, _, _, _, _, _, _, _),
	C_Code_Info = c_code_info(C_Header, _).

module_info_set_c_header(ModuleInfo1, C_Header, ModuleInfo2) :-
	ModuleInfo1 = module(A, C_Code_Info0, C, D, E, F, G, H, I, J, K, L, M),
	C_Code_Info0 = c_code_info(_C_Header0, C_Body),
	C_Code_Info = c_code_info(C_Header, C_Body),
	ModuleInfo2 = module(A, C_Code_Info, C, D, E, F, G, H, I, J, K, L, M).

module_info_get_c_body_code(ModuleInfo, C_Body) :-
	ModuleInfo = module(_, C_Code_Info, _, _, _, _, _, _, _, _, _, _, _),
	C_Code_Info = c_code_info(_, C_Body).

module_info_set_c_body_code(ModuleInfo1, C_Body, ModuleInfo2) :-
	ModuleInfo1 = module(A, C_Code_Info0, C, D, E, F, G, H, I, J, K, L, M),
	C_Code_Info0 = c_code_info(C_Header, _C_Body0),
	C_Code_Info = c_code_info(C_Header, C_Body),
	ModuleInfo2 = module(A, C_Code_Info, C, D, E, F, G, H, I, J, K, L, M).

module_info_get_predicate_table(ModuleInfo, PredicateTable) :-
	ModuleInfo = module(_, _, PredicateTable, _, _, _, _, _, _, _, _, _, _).

module_info_preds(ModuleInfo, Preds) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds).

module_info_pred_info(ModuleInfo, PredId, PredInfo) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo).

module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo).

module_info_predids(ModuleInfo, PredIds) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_predids(PredicateTable, PredIds).

module_info_reverse_predids(ModuleInfo0, ModuleInfo) :-
	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_reverse_predids(PredicateTable0, PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo).

module_info_get_unify_requests(ModuleInfo, Requests) :-
	ModuleInfo = module(_, _, _, Requests, _, _, _, _, _, _, _, _, _).

module_info_get_shapes(ModuleInfo, Shapes) :-
	module_info_shape_info(ModuleInfo, Shape_Info),
	Shape_Info = shape_info(Shapes, _AbsExports).

module_info_get_special_pred_map(ModuleInfo, SpecialPredMap) :-
	ModuleInfo = module(_, _, _, _, SpecialPredMap, _, _, _, _, _, _, _, _).

module_info_shape_info(ModuleInfo, ShapeInfo) :-
	ModuleInfo = module(_, _, _, _, _, ShapeInfo, _, _, _, _, _, _, _).

module_info_types(ModuleInfo, Types) :-
	ModuleInfo = module(_, _, _, _, _, _, Types, _, _, _, _, _, _).

module_info_typeids(ModuleInfo, TypeIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, Types, _, _, _, _, _, _),
	map__keys(Types, TypeIDs).

module_info_insts(ModuleInfo, Insts) :-
	ModuleInfo = module(_, _, _, _, _, _, _, Insts, _, _, _, _, _).

module_info_instids(ModuleInfo, InstIDs) :-
	module_info_insts(ModuleInfo, InstTable),
	inst_table_get_user_insts(InstTable, UserInstTable),
	map__keys(UserInstTable, InstIDs).

module_info_modes(ModuleInfo, Modes) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, Modes, _, _, _, _).

module_info_modeids(ModuleInfo, ModeIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, Modes, _, _, _, _),
	map__keys(Modes, ModeIDs).

module_info_ctors(ModuleInfo, Ctors) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, Ctors, _, _, _).

module_info_consids(ModuleInfo, ConsIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, Ctors, _, _, _),
	map__keys(Ctors, ConsIDs).

module_info_dependency_info(ModuleInfo, DepInfo) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, DepInfo0, _, _),
	( DepInfo0 = yes(DepInfo1) ->
	    DepInfo = DepInfo1
	;
	    error("Attempted to access uninitialised dependency_info")
	).

module_info_dependency_info_built(ModuleInfo) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, yes(_), _, _).

module_info_num_errors(ModuleInfo, NumErrors) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, NumErrors, _).

% not used:
% module_info_num_warnings(ModuleInfo, NumWarnings) :-
% 	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, NumWarnings).

% module_info_lambda_count(ModuleInfo, LambdaCount) :-
%  	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, LambdaCount).

	% Various predicates which modify the module_info data structure.

module_info_set_name(ModuleInfo0, Name, ModuleInfo) :-
	ModuleInfo0 = module(_, B, C, D, E, F, G, H, I, J, K, L, M),
	ModuleInfo = module(Name, B, C, D, E, F, G, H, I, J, K, L, M).

module_info_set_predicate_table(ModuleInfo0, PredicateTable, ModuleInfo) :-
	ModuleInfo0 = module(A, B, _, D, E, F, G, H, I, J, K, L, M),
	ModuleInfo = module(A, B, PredicateTable, D, E, F, G, H, I, J, K, L, M).

module_info_set_preds(ModuleInfo0, Preds, ModuleInfo) :-
	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_set_preds(PredicateTable0, Preds, PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo).

module_info_set_unify_requests(ModuleInfo0, Requests, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, _, E, F, G, H, I, J, K, L, M), 
	ModuleInfo = module(A, B, C, Requests, E, F, G, H, I, J, K, L, M).

module_info_set_special_pred_map(ModuleInfo0, SpecialPredMap, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, _, F, G, H, I, J, K, L, M),
	ModuleInfo = module(A, B, C, D, SpecialPredMap, F, G, H, I, J, K, L, M).

module_info_set_shapes(ModuleInfo0, Shapes, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, M),
	F = shape_info(_, AbsExports),
	ModuleInfo = module(A, B, C, D, E, shape_info(Shapes, AbsExports),
		G, H, I, J, K, L, M).

module_info_set_shape_info(ModuleInfo0, Shape_Info, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, _, G, H, I, J, K, L, M),
	ModuleInfo = module(A, B, C, D, E, Shape_Info, G, H, I, J, K, L, M).

module_info_set_types(ModuleInfo0, Types, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, _, H, I, J, K, L, M),
	ModuleInfo = module(A, B, C, D, E, F, Types, H, I, J, K, L, M).

module_info_set_insts(ModuleInfo0, Insts, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, _, I, J, K, L, M),
	ModuleInfo = module(A, B, C, D, E, F, G, Insts, I, J, K, L, M).

module_info_set_modes(ModuleInfo0, Modes, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, _, J, K, L, M),
	ModuleInfo = module(A, B, C, D, E, F, G, H, Modes, J, K, L, M).

module_info_set_ctors(ModuleInfo0, Ctors, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, _, K, L, M),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, Ctors, K, L, M).

module_info_set_dependency_info(ModuleInfo0, DepInfo, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, _, L, M),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, yes(DepInfo), L, M).

module_info_set_num_errors(ModuleInfo0, Errs, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, _, M),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, Errs, M).

module_info_incr_errors(ModuleInfo0, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, Errs0, M),
	Errs is Errs0 + 1,
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, Errs, M).

/* not used
module_info_incr_warnings(ModuleInfo0, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, Warns0),
	Warns is Warns0 + 1,
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, L, Warns).
*/
module_info_next_lambda_count(ModuleInfo0, Count, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, Count0),
	Count is Count0 + 1,
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, L, Count).

module_info_remove_predid(ModuleInfo0, PredId, ModuleInfo) :-
	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_remove_predid(PredicateTable0, PredId,
				PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
				ModuleInfo).

	% After we have finished constructing the symbol tables,
	% we balance all the binary trees, to improve performance
	% in later stages of the compiler.

module_info_optimize(ModuleInfo0, ModuleInfo) :-

	module_info_get_predicate_table(ModuleInfo0, Preds0),
	predicate_table_optimize(Preds0, Preds),
	module_info_set_predicate_table(ModuleInfo0, Preds, ModuleInfo2),

	module_info_get_shapes(ModuleInfo2, (Shapes0 - N)),
	map__optimize(Shapes0, Shapes),
	module_info_set_shapes(ModuleInfo2, (Shapes - N), ModuleInfo3),

	module_info_types(ModuleInfo3, Types0),
	map__optimize(Types0, Types),
	module_info_set_types(ModuleInfo3, Types, ModuleInfo4),

	module_info_insts(ModuleInfo4, InstTable0),
	inst_table_get_user_insts(InstTable0, Insts0),
	map__optimize(Insts0, Insts),
	inst_table_set_user_insts(InstTable0, Insts, InstTable),
	module_info_set_insts(ModuleInfo4, InstTable, ModuleInfo5),

	module_info_modes(ModuleInfo5, Modes0),
	map__optimize(Modes0, Modes),
	module_info_set_modes(ModuleInfo4, Modes, ModuleInfo6),

	module_info_ctors(ModuleInfo6, Ctors0),
	map__optimize(Ctors0, Ctors),
	module_info_set_ctors(ModuleInfo6, Ctors, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- type pred_id == int.

	% Various predicates for accessing the predicate_table type.
	% The predicate_table holds information about the predicates
	% defined in this module or imported from other modules.
	% The primary key for this table is the `pred_id', but there
	% are also secondary indexes on name, name+arity, and module+name+arity.

	% Initialize the predicate table
	%
:- pred predicate_table_init(predicate_table).
:- mode predicate_table_init(out) is det.

	% Balance all the binary trees in the predicate table
	%
:- pred predicate_table_optimize(predicate_table, predicate_table).
:- mode predicate_table_optimize(in, out) is det.

	% Get the pred_id->pred_info map.
	%
:- pred predicate_table_get_preds(predicate_table, pred_table).
:- mode predicate_table_get_preds(in, out) is det.

	% Set the pred_id->pred_info map.
	% NB You shouldn't modify the keys in this table, only
	% use predicate_table_insert and predicate_table_remove_predid.
	%
:- pred predicate_table_set_preds(predicate_table, pred_table, predicate_table).
:- mode predicate_table_set_preds(in, in, out) is det.

	% Get a list of all the valid predids in the predicate_table.
	%
:- pred predicate_table_get_predids(predicate_table, list(pred_id)).
:- mode predicate_table_get_predids(in, out) is det.

	% Remove a pred_id from the valid list.
	%
:- pred predicate_table_remove_predid(predicate_table, pred_id,
					predicate_table).
:- mode predicate_table_remove_predid(in, in, out) is det.

	% Search the table for predicates matching this
	% (possibly module-qualified) sym_name & arity.
	%
:- pred predicate_table_search_sym_arity(predicate_table, sym_name, arity,
					list(pred_id)).
:- mode predicate_table_search_sym_arity(in, in, in, out) is semidet.

	% Search the table for predicates matching this name.
	%
:- pred predicate_table_search_name(predicate_table, string, list(pred_id)).
:- mode predicate_table_search_name(in, in, out) is semidet.

	% Search the table for predicates matching this name & arity.
	%
:- pred predicate_table_search_name_arity(predicate_table, string, arity,
						list(pred_id)).
:- mode predicate_table_search_name_arity(in, in, in, out) is semidet.

	% Search the table for THE predicate matching this module,
	% name, and arity. (We currently don't allow overloading
	% of predicates with the same name/arity in the same module.)
	% m_n_a is short for module, name, arity.
	%
:- pred predicate_table_search_m_n_a(predicate_table, module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_m_n_a(in, in, in, in, out) is semidet.

	% Insert a new pred_info structure into the predicate_table
	% and assign it a new pred_id. You should check beforehand
	% that the pred doesn't already occur in the table.
	%
:- pred predicate_table_insert(predicate_table, pred_info, pred_id,
				predicate_table).
:- mode predicate_table_insert(in, in, out, out) is det.

	% Return an invalid pred_id. Used to initialize the pred_id
	% in call(...) goals before we do typechecking or when type-checking
	% finds that there was no predicate which matched the call.
	%
:- pred invalid_pred_id(pred_id).
:- mode invalid_pred_id(out) is det.
:- mode invalid_pred_id(in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- type predicate_table --->
	predicate_table(
		pred_table,		% map from pred_id to pred_info
		pred_id,		% next available pred_id
		list(pred_id),		% the keys of the pred_table -
					% cached here for efficiency
		pred_name_index,	% map from pred name to pred_id
		pred_name_arity_index,	% map from pred name & arity to pred_id
		pred_module_name_arity_index
					% map from pred module, name & arity
					% to pred_id
	).

:- type pred_name_index	== map(string, list(pred_id)).

:- type pred_name_arity_index == map(name_arity, list(pred_id)).
:- type name_arity ---> string / arity.

:- type pred_module_name_arity_index == map(module_name_arity, list(pred_id)).
:- type module_name_arity ---> module_name_arity(module_name, string, arity).

predicate_table_init(PredicateTable) :-
	PredicateTable = predicate_table(Preds, NextPredId, PredIds,
				N_Index, NA_Index, MNA_Index),
	map__init(Preds),
	NextPredId = 0,
	PredIds = [],
	map__init(N_Index),
	map__init(NA_Index),
	map__init(MNA_Index).

predicate_table_optimize(PredicateTable0, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, C,
				N_Index0, NA_Index0, MNA_Index0),
	map__optimize(N_Index0, N_Index),
	map__optimize(NA_Index0, NA_Index),
	map__optimize(MNA_Index0, MNA_Index),
	PredicateTable = predicate_table(A, B, C,
				N_Index, NA_Index, MNA_Index).

predicate_table_get_preds(PredicateTable, Preds) :-
	PredicateTable = predicate_table(Preds, _, _, _, _, _).

predicate_table_set_preds(PredicateTable0, Preds, PredicateTable) :-
	PredicateTable0 = predicate_table(_, B, C, D, E, F),
	PredicateTable = predicate_table(Preds, B, C, D, E, F).

predicate_table_get_predids(PredicateTable, PredIds) :-
	PredicateTable = predicate_table(_, _, PredIds, _, _, _).

predicate_table_remove_predid(PredicateTable0, PredId, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, PredIds0, D, E, F),
	list__delete_all(PredIds0, PredId, PredIds),
	PredicateTable = predicate_table(A, B, PredIds, D, E, F).

:- pred predicate_table_reverse_predids(predicate_table, predicate_table).
:- mode predicate_table_reverse_predids(in, out) is det.

predicate_table_reverse_predids(PredicateTable0, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, PredIds0, D, E, F),
	list__reverse(PredIds0, PredIds),
	PredicateTable = predicate_table(A, B, PredIds, D, E, F).

:- predicate_table_search_sym_arity(_, X, _, _) when X. % NU-Prolog indexing.

predicate_table_search_sym_arity(PredicateTable, qualified(Module, Name),
		Arity, PredIdList) :-
	predicate_table_search_m_n_a(PredicateTable, Module, Name, Arity,
		PredIdList).
predicate_table_search_sym_arity(PredicateTable, unqualified(Name),
		Arity, PredIdList) :-
	predicate_table_search_name_arity(PredicateTable, Name, Arity,
		PredIdList).

predicate_table_search_name(PredicateTable, PredName, PredId) :-
	PredicateTable = predicate_table(_, _, _, PredNameIndex, _, _),
	map__search(PredNameIndex, PredName, PredId).

predicate_table_search_name_arity(PredicateTable, PredName, Arity, PredId) :-
	PredicateTable = predicate_table(_, _, _, _, PredNameArityIndex, _),
	map__search(PredNameArityIndex, PredName / Arity, PredId).

predicate_table_search_m_n_a(PredicateTable, Module, PredName, Arity, PredIds)
		:-
	PredicateTable = predicate_table(_, _, _, _, _, MNA_Index),
	MNA = module_name_arity(Module, PredName, Arity),
	map__search(MNA_Index, MNA, PredIds).

predicate_table_insert(PredicateTable0, PredInfo, PredId, PredicateTable) :-
	PredicateTable0 = predicate_table(Preds0, NextPredId0, PredIds0,
				N_Index0, NA_Index0, MNA_Index0),
	pred_info_module(PredInfo, Module),
	pred_info_name(PredInfo, Name),
	pred_info_arity(PredInfo, Arity),

		% allocate a new pred_id
	PredId = NextPredId0,
	NextPredId is PredId + 1,

		% insert the pred_id into the name index
	( map__search(N_Index0, Name, N_PredIdList0) ->
		N_PredIdList = [PredId | N_PredIdList0]
	;
		N_PredIdList = [PredId]
	),
	map__set(N_Index0, Name, N_PredIdList, N_Index),

		% insert it into the name/arity index
	NA = Name / Arity,
	( map__search(NA_Index0, NA, NA_PredIdList0) ->
		NA_PredIdList = [PredId | NA_PredIdList0]
	;
		NA_PredIdList = [PredId]
	),
	map__set(NA_Index0, NA, NA_PredIdList, NA_Index),

		% insert it into the module:name/arity index
	MNA = module_name_arity(Module, Name, Arity),
	( map__search(MNA_Index0, MNA, MNA_PredIdList0) ->
		MNA_PredIdList = [PredId | MNA_PredIdList0]
	;
		MNA_PredIdList = [PredId]
	),
	map__set(MNA_Index0, MNA, MNA_PredIdList, MNA_Index),

		% insert it into the pred_id list
	PredIds = [PredId | PredIds0],

		% save the pred_info for this pred_id
	map__set(Preds0, PredId, PredInfo, Preds),

	PredicateTable = predicate_table(Preds, NextPredId, PredIds,
				N_Index, NA_Index, MNA_Index).

invalid_pred_id(-1).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the cons_id type.

:- interface.

	% Given a cons_id, convert it into a const (from term.m) and
	% an integer arity.  Fails if the cons_id is not representable
	% as a const (for example, if it is a higher-order pred constant
	% or an address constant).
:- pred cons_id_to_const(cons_id, const, arity).
:- mode cons_id_to_const(in, out, out) is semidet.

	% The reverse conversion - make a cons_id for a functor.
	% Given a const and an arity for the functor, create a cons_id.
:- pred make_functor_cons_id(const, arity, cons_id).
:- mode make_functor_cons_id(in, in, out) is det.

	% Another way of making a cons_id from a functor.
	% Given the name, argument types, and type_id of a functor,
	% create a cons_id for that functor.
:- pred make_cons_id(sym_name, list(type), type_id, cons_id).
:- mode make_cons_id(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

cons_id_to_const(cons(Name, Arity), term__atom(Name), Arity).
cons_id_to_const(int_const(Int), term__integer(Int), 0).
cons_id_to_const(string_const(String), term__string(String), 0).
cons_id_to_const(float_const(Float), term__float(Float), 0).

make_functor_cons_id(term__atom(Name), Arity, cons(Name, Arity)).
make_functor_cons_id(term__integer(Int), _, int_const(Int)).
make_functor_cons_id(term__string(String), _, string_const(String)).
make_functor_cons_id(term__float(Float), _, float_const(Float)).

make_cons_id(qualified(_Module, Name), Args, _TypeId, cons(Name, Arity)) :-
	list__length(Args, Arity).
make_cons_id(unqualified(Name), Args, _TypeId, cons(Name, Arity)) :-
	list__length(Args, Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the information stored in the
	% pred_id and pred_info data structures.

:- interface.

	% The type `import_status' describes whether an entity (a predicate,
	% type, inst, or mode) is local to the current module, exported from
	% the current module, or imported from some other module.
:- type import_status
	--->	imported	% defined in the interface of some other module
				% or `external' (in some other language)
	;	pseudo_imported % this is used for entities that are defined
				% in the interface of some other module but
				% for which we may generate some code in
				% this module - in particular, this is used
				% for unification predicates (see comments in
				% unify_proc.m)
	;	exported	% defined in the interface of this module
	;	pseudo_exported % the converse of pseudo_imported
				% this means that only the (in, in) mode
				% of a unification is exported
	;	local.		% defined in the implementation of this module

:- type marker		--->	inline ; dnf ; magic ; memo.
:- type marker_status	--->	request(marker) ; done(marker).

:- pred predicate_module(module_info, pred_id, module_name).
:- mode predicate_module(in, in, out) is det.

:- pred predicate_name(module_info, pred_id, string).
:- mode predicate_name(in, in, out) is det.

:- pred predicate_arity(module_info, pred_id, arity).
:- mode predicate_arity(in, in, out) is det.

:- pred pred_info_init(module_name, sym_name, arity, tvarset, list(type),
			condition, term__context, clauses_info, import_status,
			bool, goal_type, pred_info).
:- mode pred_info_init(in, in, in, in, in, in, in, in, in, in, in, out) is det.

:- pred pred_info_module(pred_info, module_name).
:- mode pred_info_module(in, out) is det.

:- pred pred_info_name(pred_info, string).
:- mode pred_info_name(in, out) is det.

:- pred pred_info_arity(pred_info, arity).
:- mode pred_info_arity(in, out) is det.

	% Return a list of all the proc_ids for the different modes
	% of this predicate.
:- pred pred_info_procids(pred_info, list(proc_id)).
:- mode pred_info_procids(in, out) is det.

	% Return a list of the proc_ids for all the modes
	% of this predicate that are not imported.
:- pred pred_info_non_imported_procids(pred_info, list(proc_id)).
:- mode pred_info_non_imported_procids(in, out) is det.

	% Return a list of the proc_ids for all the modes
	% of this predicate that are exported.
:- pred pred_info_exported_procids(pred_info, list(proc_id)).
:- mode pred_info_exported_procids(in, out) is det.

:- pred pred_info_arg_types(pred_info, tvarset, list(type)).
:- mode pred_info_arg_types(in, out, out) is det.

:- pred pred_info_set_arg_types(pred_info, tvarset, list(type), pred_info).
:- mode pred_info_set_arg_types(in, in, in, out) is det.

:- pred pred_info_clauses_info(pred_info, clauses_info).
:- mode pred_info_clauses_info(in, out) is det.

:- pred pred_info_set_clauses_info(pred_info, clauses_info, pred_info).
:- mode pred_info_set_clauses_info(in, in, out) is det.

:- pred pred_info_procedures(pred_info, proc_table).
:- mode pred_info_procedures(in, out) is det.

:- pred pred_info_set_procedures(pred_info, proc_table, pred_info).
:- mode pred_info_set_procedures(in, in, out) is det.

:- pred pred_info_context(pred_info, term__context).
:- mode pred_info_context(in, out) is det.

:- pred pred_info_import_status(pred_info::in, import_status::out) is det.

:- pred pred_info_is_imported(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_imported(pred_info::in) is semidet.

:- pred pred_info_is_exported(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_exported(pred_info::in) is semidet.

	% Set the import_status of the predicate to `imported'.
	% This is used for `:- external(foo/2).' declarations.
:- pred pred_info_mark_as_external(pred_info::in, pred_info::out) is det.

:- pred pred_info_set_status(pred_info::in, import_status::in,
				pred_info::out) is det.

:- pred pred_info_typevarset(pred_info, tvarset).
:- mode pred_info_typevarset(in, out) is det.

:- pred pred_info_set_typevarset(pred_info, tvarset, pred_info).
:- mode pred_info_set_typevarset(in, in, out) is det.

:- pred pred_info_get_goal_type(pred_info, goal_type).
:- mode pred_info_get_goal_type(in, out) is det.

:- pred pred_info_set_goal_type(pred_info, goal_type, pred_info).
:- mode pred_info_set_goal_type(in, in, out) is det.

	% Succeeds if there was a `:- pragma(inline, ...)' declaration
	% for this predicate.  Note that the compiler may decide
	% to inline a predicate even if there was no pragma(inline, ...)
	% declaration for that predicate.
:- pred pred_info_is_inlined(pred_info).
:- mode pred_info_is_inlined(in) is semidet.

:- pred pred_info_get_marker_list(pred_info, list(marker_status)).
:- mode pred_info_get_marker_list(in, out) is det.

:- pred pred_info_set_marker_list(pred_info, list(marker_status), pred_info).
:- mode pred_info_set_marker_list(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

predicate_module(ModuleInfo, PredId, Module) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_module(PredInfo, Module).

predicate_name(ModuleInfo, PredId, PredName) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_name(PredInfo, PredName).

predicate_arity(ModuleInfo, PredId, Arity) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_arity(PredInfo, Arity).

	% The symbol table for predicates.

:- type pred_info
	--->	predicate(
			tvarset,	% names of type vars
					% in the predicate's type decl
			list(type),	% argument types
			condition,	% formal specification
					% (not used)

			clauses_info,

			proc_table,

			term__context,	% the location (line #)
					% of the :- pred decl.

			module_name,	% module in which pred occurs
			string,		% predicate name
			arity,		% the arity of the pred
			import_status,
			tvarset,	% names of type vars
					% in the predicate's type decl
					% or in the variable type assignments
			goal_type,	% whether the goals seen so far for
					% this pred are clauses, 
					% pragma(c_code, ...) decs, or none
			list(marker_status)
					% records which transformations
					% have been done or are to be done
		).

pred_info_init(ModuleName, SymName, Arity, TypeVarSet, Types, Cond, Context,
		ClausesInfo, Status, Inline, GoalType, PredInfo) :-
	map__init(Procs),
	unqualify_name(SymName, PredName),
	sym_name_get_module_name(SymName, ModuleName, PredModuleName),
	( Inline = yes ->
		Markers = [request(inline)]
	;
		Markers = []
	),
	PredInfo = predicate(TypeVarSet, Types, Cond, ClausesInfo, Procs,
		Context, PredModuleName, PredName, Arity, Status, TypeVarSet, 
		GoalType, Markers).

pred_info_procids(PredInfo, ProcIds) :-
	PredInfo = predicate(_, _, _, _, Procs, _, _, _, _, _, _, _, _),
	map__keys(Procs, ProcIds).

pred_info_non_imported_procids(PredInfo, ProcIds) :-
	pred_info_import_status(PredInfo, ImportStatus),
	( ImportStatus = imported ->
		ProcIds = []
	; ImportStatus = pseudo_imported ->
		pred_info_procids(PredInfo, ProcIds0),
		% for pseduo_imported preds, procid 0 is imported
		list__delete_all(ProcIds0, 0, ProcIds)
	;
		pred_info_procids(PredInfo, ProcIds)
	).

pred_info_exported_procids(PredInfo, ProcIds) :-
	pred_info_import_status(PredInfo, ImportStatus),
	( ImportStatus = exported ->
		pred_info_procids(PredInfo, ProcIds)
	; ImportStatus = pseudo_exported ->
		ProcIds = [0]
	;
		ProcIds = []
	).

pred_info_clauses_info(PredInfo, Clauses) :-
	PredInfo = predicate(_, _, _, Clauses, _, _, _, _, _, _, _, _, _).

pred_info_set_clauses_info(PredInfo0, Clauses, PredInfo) :-
	PredInfo0 = predicate(A, B, C, _, E, F, G, H, I, J, K, L, M),
	PredInfo = predicate(A, B, C, Clauses, E, F, G, H, I, J, K, L, M).

pred_info_arg_types(PredInfo, TypeVars, ArgTypes) :-
	PredInfo = predicate(TypeVars, ArgTypes, 
		_, _, _, _, _, _, _, _, _, _, _).

pred_info_set_arg_types(PredInfo0, TypeVarSet, ArgTypes, PredInfo) :-
	PredInfo0 = predicate(_, _, C, D, E, F, G, H, I, J, K, L, M),
	PredInfo = predicate(TypeVarSet, ArgTypes, 
			C, D, E, F, G, H, I, J, K, L, M).

pred_info_procedures(PredInfo, Procs) :-
	PredInfo = predicate(_, _, _, _, Procs, _, _, _, _, _, _, _, _).

pred_info_set_procedures(PredInfo0, Procedures, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, _, F, G, H, I, J, K, L, M),
	PredInfo = predicate(A, B, C, D, Procedures, F, G, H, I, J, K, L, M).

pred_info_context(PredInfo, Context) :-
	PredInfo = predicate(_, _, _, _, _, Context, _, _, _, _, _, _, _).

pred_info_module(PredInfo, Module) :-
	PredInfo = predicate(_, _, _, _, _, _, Module, _, _, _, _, _, _).

pred_info_name(PredInfo, PredName) :-
	PredInfo = predicate(_, _, _, _, _, _, _, PredName, _, _, _, _, _).

pred_info_arity(PredInfo, Arity) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, Arity, _, _, _, _).

pred_info_import_status(PredInfo, ImportStatus) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, ImportStatus, _, _, _).

pred_info_is_imported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = imported.

pred_info_is_pseudo_imported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = pseudo_imported.

pred_info_is_exported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = exported.

pred_info_is_pseudo_exported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = pseudo_exported.

pred_info_mark_as_external(PredInfo0, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, _, K, L, M),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, imported, K, L, M).

pred_info_set_status(PredInfo0, Status, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, _, K, L, M),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, Status, K, L, M).

pred_info_typevarset(PredInfo, TypeVarSet) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, TypeVarSet, _, _).

pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, _, L, M),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, TypeVarSet, L, M).

pred_info_get_goal_type(PredInfo, GoalType) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, GoalType, _).

pred_info_set_goal_type(PredInfo0, GoalType, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, _, M),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, GoalType, M).

pred_info_is_inlined(PredInfo0) :-
	pred_info_get_marker_list(PredInfo0, Markers),
	list__member(request(inline), Markers).

pred_info_get_marker_list(PredInfo, Markers) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, Markers).

pred_info_set_marker_list(PredInfo0, Markers, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, _),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, Markers).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the proc_info data structure.

:- interface.

:- pred proc_info_init(int, list(mode), maybe(determinism), term__context,
	proc_info).
:- mode proc_info_init(in, in, in, in, out) is det.

:- pred proc_info_declared_determinism(proc_info, maybe(determinism)).
:- mode proc_info_declared_determinism(in, out) is det.

:- pred proc_info_inferred_determinism(proc_info, determinism).
:- mode proc_info_inferred_determinism(in, out) is det.

:- pred proc_info_interface_determinism(proc_info, determinism).
:- mode proc_info_interface_determinism(in, out) is det.

:- pred proc_info_interface_code_model(proc_info, code_model).
:- mode proc_info_interface_code_model(in, out) is det.

:- pred proc_info_variables(proc_info, varset).
:- mode proc_info_variables(in, out) is det.

:- pred proc_info_set_varset(proc_info, varset, proc_info).
:- mode proc_info_set_varset(in, in, out) is det.

:- pred proc_info_set_variables(proc_info, varset, proc_info).
:- mode proc_info_set_variables(in, in, out) is det.

:- pred proc_info_vartypes(proc_info, map(var, type)).
:- mode proc_info_vartypes(in, out) is det.

:- pred proc_info_set_vartypes(proc_info, map(var, type), proc_info).
:- mode proc_info_set_vartypes(in, in, out) is det.

:- pred proc_info_headvars(proc_info, list(var)).
:- mode proc_info_headvars(in, out) is det.

:- pred proc_info_set_headvars(proc_info, list(var), proc_info).
:- mode proc_info_set_headvars(in, in, out) is det.

:- pred proc_info_argmodes(proc_info, list(mode)).
:- mode proc_info_argmodes(in, out) is det.

:- pred proc_info_set_argmodes(proc_info, list(mode), proc_info).
:- mode proc_info_set_argmodes(in, in, out) is det.

:- pred proc_info_goal(proc_info, hlds__goal).
:- mode proc_info_goal(in, out) is det.

:- pred proc_info_context(proc_info, term__context).
:- mode proc_info_context(in, out) is det.

:- pred proc_info_call_info(proc_info, call_info).
:- mode proc_info_call_info(in, out) is det.

:- pred proc_info_liveness_info(proc_info, liveness_info).
:- mode proc_info_liveness_info(in, out) is det.

:- pred proc_info_follow_vars(proc_info, follow_vars).
:- mode proc_info_follow_vars(in, out) is det.

:- pred proc_info_can_process(proc_info, bool).
:- mode proc_info_can_process(in, out) is det.

:- pred proc_info_set_body(proc_info, varset, map(var, type), list(var),
				hlds__goal, proc_info).
:- mode proc_info_set_body(in, in, in, in, in, out) is det.

:- pred proc_info_set_inferred_determinism(proc_info, determinism, proc_info).
:- mode proc_info_set_inferred_determinism(in, in, out) is det.

:- pred proc_info_set_goal(proc_info, hlds__goal, proc_info).
:- mode proc_info_set_goal(in, in, out) is det.

:- pred proc_info_arg_info(proc_info, list(arg_info)).
:- mode proc_info_arg_info(in, out) is det.

:- pred proc_info_set_arg_info(proc_info, list(arg_info), proc_info).
:- mode proc_info_set_arg_info(in, in, out) is det.

:- pred proc_info_get_initial_instmap(proc_info, module_info, instmap).
:- mode proc_info_get_initial_instmap(in, in, out) is det.

:- pred proc_info_set_liveness_info(proc_info, liveness_info, proc_info).
:- mode proc_info_set_liveness_info(in, in, out) is det.

:- pred proc_info_set_follow_vars(proc_info, follow_vars, proc_info).
:- mode proc_info_set_follow_vars(in, in, out) is det.

:- pred proc_info_set_call_info(proc_info, call_info, proc_info).
:- mode proc_info_set_call_info(in, in, out) is det.

:- pred proc_info_set_can_process(proc_info, bool, proc_info).
:- mode proc_info_set_can_process(in, in, out) is det.

:- implementation.

	% Some parts of the procedure aren't known yet. We initialize
	% them to any old garbage which we will later throw away.

	% If the source code doesn't specify any determinism annotation,
	% inferred determinism gets initialized to `erroneous'.
	% This is what `det_analysis.m' wants. det_analysis.m
	% will later provide the correct inferred determinism for it.

proc_info_init(Arity, Modes, MaybeDet, MContext, NewProc) :-
	map__init(BodyTypes),
	goal_info_init(GoalInfo),
	varset__init(BodyVarSet0),
	make_n_fresh_vars(Arity, BodyVarSet0, HeadVars, BodyVarSet),
	InferredDet = erroneous,
	map__init(CallInfo),
	set__init(Liveness),
	ArgInfo = [],
	ClauseBody = conj([]) - GoalInfo,
	map__init(FollowVars),
	CanProcess = yes,
	NewProc = procedure(
		MaybeDet, BodyVarSet, BodyTypes, HeadVars, Modes,
		ClauseBody, MContext, CallInfo, InferredDet, CanProcess,
		ArgInfo, Liveness, FollowVars
	).

proc_info_interface_determinism(ProcInfo, Determinism) :-
	proc_info_declared_determinism(ProcInfo, MaybeDeterminism),
	(
		MaybeDeterminism = no,
		proc_info_inferred_determinism(ProcInfo, Determinism)
	;
		MaybeDeterminism = yes(Determinism)
	).

proc_info_interface_code_model(ProcInfo, CodeModel) :-
	proc_info_interface_determinism(ProcInfo, Determinism),
	determinism_to_code_model(Determinism, CodeModel).

proc_info_declared_determinism(ProcInfo, Determinism) :-
	ProcInfo = procedure(Determinism, _, _, _, _, _, _, _, _, _, _, _, _).
proc_info_variables(ProcInfo, VarSet) :-
	ProcInfo = procedure(_, VarSet, _, _, _, _, _, _, _, _, _, _, _).
proc_info_vartypes(ProcInfo, VarTypes) :-
	ProcInfo = procedure(_, _, VarTypes, _, _, _, _, _, _, _, _, _, _).
proc_info_headvars(ProcInfo, HeadVars) :-
	ProcInfo = procedure(_, _, _, HeadVars, _, _, _, _, _, _, _, _, _).
proc_info_argmodes(ProcInfo, ModeInfo) :-
	ProcInfo = procedure(_, _, _, _, ModeInfo, _, _, _, _, _, _, _, _).
proc_info_goal(ProcInfo, Goal) :-
	ProcInfo = procedure(_, _, _, _, _, Goal, _, _, _, _, _, _, _).
proc_info_context(ProcInfo, Context) :-
	ProcInfo = procedure(_, _, _, _, _, _, Context, _, _, _, _, _, _).
proc_info_call_info(ProcInfo, CallInfo) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, CallInfo, _, _, _, _, _).
proc_info_inferred_determinism(ProcInfo, Determinism) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, Determinism, _, _, _, _).
proc_info_can_process(ProcInfo, CanProcess) :-
 	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, CanProcess, _, _, _).
proc_info_arg_info(ProcInfo, ArgInfo) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, ArgInfo, _, _).
proc_info_liveness_info(ProcInfo, Liveness) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, Liveness, _).
proc_info_follow_vars(ProcInfo, Follow) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, Follow).

% :- type proc_info	--->	procedure(
% 				A	maybe(determinism),% _declared_ detism
% 				B	varset,		% variable names
% 				C	map(var, type),	% variable types
% 				D	list(var),	% head vars
% 				E	list(mode), 	% modes of args
% 				F	hlds__goal,	% Body
% 				G	term__context,	% The context of
% 							% the :- mode decl,
% 							% not the clause.
% 				H	call_info,	% stack allocations
% 				I	determinism,	% _inferred_ detism
% 				J	bool,		% can_process
% 				K	list(arg_info),	% information about
% 							% the arguments
% 							% derived from the
% 							% modes etc
% 				L	liveness_info,	% the initial liveness
% 				M	follow_vars	% initial followvars
% 				).

proc_info_set_body(ProcInfo0, VarSet, VarTypes, HeadVars, Goal, ProcInfo) :-
	ProcInfo0 = procedure(A, _, _, _, E, _, G,
		H, I, J, K, L, M),
	ProcInfo = procedure(A, VarSet, VarTypes, HeadVars, E, Goal, G,
		H, I, J, K, L, M).

proc_info_set_varset(ProcInfo0, VarSet, ProcInfo) :-
	ProcInfo0 = procedure(A, _, C, D, E, F, G, H, I, J, K, L, M),
	ProcInfo = procedure(A, VarSet, C, D, E, F, G, H, I, J, K, L, M).

proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, _, E, F, G, H, I, J, K, L, M),
	ProcInfo = procedure(A, B, C, HeadVars, E, F, G, H, I, J, K, L, M).

proc_info_set_argmodes(ProcInfo0, ArgModes, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, _, F, G, H, I, J, K, L, M),
	ProcInfo = procedure(A, B, C, D, ArgModes, F, G, H, I, J, K, L, M).

proc_info_set_inferred_determinism(ProcInfo0, Determinism, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, _, J, K, L, M),
	ProcInfo = procedure(A, B, C, D, E, F, G, H, Determinism, J, K, L, M).

proc_info_set_can_process(ProcInfo0, CanProcess, ProcInfo) :-
 	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, _, K, L, M),
 	ProcInfo = procedure(A, B, C, D, E, F, G, H, I, CanProcess, K, L, M).

proc_info_set_goal(ProcInfo0, Goal, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, _, G, H, I, J, K, L, M),
	ProcInfo = procedure(A, B, C, D, E, Goal, G, H, I, J, K, L, M).

proc_info_set_call_info(ProcInfo0, CallInfo, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, _, I, J, K, L, M),
	ProcInfo = procedure(A, B, C, D, E, F, G, CallInfo, I, J, K, L, M).

proc_info_set_arg_info(ProcInfo0, ArgInfo, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, _, L, M),
	ProcInfo = procedure(A, B, C, D, E, F, G, H, I, J, ArgInfo, L, M).

proc_info_set_liveness_info(ProcInfo0, Liveness, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, _, M),
	ProcInfo = procedure(A, B, C, D, E, F, G, H, I, J, K, Liveness, M).

proc_info_set_follow_vars(ProcInfo0, Follow, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, _),
	ProcInfo = procedure(A, B, C, D, E, F, G, H, I, J, K, L, Follow).

proc_info_get_initial_instmap(ProcInfo, ModuleInfo, reachable(InstMapping)) :-
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_argmodes(ProcInfo, ArgModes),
	mode_list_get_initial_insts(ArgModes, ModuleInfo, InitialInsts),
/***********
		% propagate type information into the modes
	proc_info_vartypes(ProcInfo, VarTypes),
	propagate_type_info_inst_list(VarTypes, ModuleInfo, InitialInsts0,
					InitialInsts),
***********/
	map__from_corresponding_lists(HeadVars, InitialInsts, InstMapping).

proc_info_set_variables(ProcInfo0, Vars, ProcInfo) :-
	ProcInfo0 = procedure(A, _, C, D, E, F, G, H, I, J, K, L, M),
	ProcInfo = procedure(A, Vars, C, D, E, F, G, H, I, J, K, L, M).

proc_info_set_vartypes(ProcInfo0, Vars, ProcInfo) :-
	ProcInfo0 = procedure(A, B, _, D, E, F, G, H, I, J, K, L, M),
	ProcInfo = procedure(A, B, Vars, D, E, F, G, H, I, J, K, L, M).

%-----------------------------------------------------------------------------%
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

	% The instmap delta stores the final instantiatedness
	% of the non-local variables whose instantiatedness
	% changed.

:- type instmap_delta	==	instmap.

:- type instmap		--->	reachable(instmapping)
			;	unreachable.

:- type instmapping	==	map(var, inst).

:- pred goal_info_get_instmap_delta(hlds__goal_info, instmap_delta).
:- mode goal_info_get_instmap_delta(in, out) is det.

:- pred goal_info_set_instmap_delta(hlds__goal_info, instmap_delta,
				hlds__goal_info).
:- mode goal_info_set_instmap_delta(in, in, out) is det.

:- pred goal_info_context(hlds__goal_info, term__context).
:- mode goal_info_context(in, out) is det.

:- pred goal_info_set_context(hlds__goal_info, term__context, hlds__goal_info).
:- mode goal_info_set_context(in, in, out) is det.

:- pred goal_info_store_map(hlds__goal_info, maybe(map(var, lval))).
:- mode goal_info_store_map(in, out) is det.

:- pred goal_info_set_store_map(hlds__goal_info,
				maybe(map(var, lval)), hlds__goal_info).
:- mode goal_info_set_store_map(in, in, out) is det.

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
	%
:- pred goal_to_conj_list(hlds__goal, list(hlds__goal)).
:- mode goal_to_conj_list(in, out) is det.

	% Convert a goal to a list of disjuncts.
	% If the goal is a disjunction, then return its disjuncts,
	% otherwise return the goal as a singleton list.
	%
:- pred goal_to_disj_list(hlds__goal, list(hlds__goal)).
:- mode goal_to_disj_list(in, out) is det.

	% Convert a list of conjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the conjunction of the conjuncts,
	% with the specified goal_info.
	%
:- pred conj_list_to_goal(list(hlds__goal), hlds__goal_info, hlds__goal).
:- mode conj_list_to_goal(in, in, out) is det.

	% Convert a list of disjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the disjunction of the disjuncts,
	% with the specified goal_info.
	%
:- pred disj_list_to_goal(list(hlds__goal), hlds__goal_info, hlds__goal).
:- mode disj_list_to_goal(in, in, out) is det.

	% A goal is atomic iff it doesn't contain any sub-goals
	% (except possibly goals inside lambda expressions --
	% but lambda expressions will get transformed into separate
	% predicates by the polymorphism.m pass).
	%
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
		InstMapDelta, Context, NonLocals, DeltaLiveness, no, no,
		Features, NondetLives).

goal_info_pre_delta_liveness(GoalInfo, DeltaLiveness) :-
	GoalInfo = goal_info(DeltaLiveness, _, _, _, _, _, _, _, _, _, _).

goal_info_set_pre_delta_liveness(GoalInfo0, DeltaLiveness, GoalInfo) :-
	GoalInfo0 = goal_info(_, B, C, D, E, F, G, H, I, J, K),
	GoalInfo = goal_info(DeltaLiveness, B, C, D, E, F, G, H, I, J, K).

goal_info_post_delta_liveness(GoalInfo, DeltaLiveness) :-
	GoalInfo = goal_info(_, _, _, _, _, _, DeltaLiveness, _, _, _, _).

goal_info_set_post_delta_liveness(GoalInfo0, DeltaLiveness, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, _, H, I, J, K),
	GoalInfo = goal_info(A, B, C, D, E, F, DeltaLiveness, H, I, J, K).

goal_info_get_code_model(GoalInfo, CodeModel) :-
	goal_info_get_determinism(GoalInfo, Determinism),
	determinism_to_code_model(Determinism, CodeModel).

goal_info_get_determinism(GoalInfo, Determinism) :-
	GoalInfo = goal_info(_, _, Determinism, _, _, _, _, _, _, _, _).

goal_info_set_determinism(GoalInfo0, Determinism, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, _, D, E, F, G, H, I, J, K),
	GoalInfo = goal_info(A, B, Determinism, D, E, F, G, H, I, J, K).

goal_info_get_instmap_delta(GoalInfo, InstMapDelta) :-
	GoalInfo = goal_info(_, _, _, InstMapDelta, _, _, _, _, _, _, _).

goal_info_set_instmap_delta(GoalInfo0, InstMapDelta, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, _, E, F, G, H, I, J, K),
	GoalInfo = goal_info(A, B, C, InstMapDelta, E, F, G, H, I, J, K).

goal_info_context(GoalInfo, Context) :-
	GoalInfo = goal_info(_, _, _, _, Context, _, _, _, _, _, _).

goal_info_set_context(GoalInfo0, Context, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, _, F, G, H, I, J, K),
	GoalInfo = goal_info(A, B, C, D, Context, F, G, H, I, J, K).

goal_info_get_nonlocals(GoalInfo, NonLocals) :-
	GoalInfo = goal_info(_, _, _, _, _, NonLocals, _, _, _, _, _).

goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, _, G, H, I, J, K),
	GoalInfo  = goal_info(A, B, C, D, E, NonLocals, G, H, I, J, K).

goal_info_store_map(GoalInfo, H) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, H, _, _, _).

goal_info_set_store_map(GoalInfo0, H, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, _, I, J, K),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, J, K).

goal_info_cont_lives(GoalInfo, I) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, _, I, _, _).

goal_info_set_cont_lives(GoalInfo0, I, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, H, _, J, K),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, J, K).

goal_info_get_features(GoalInfo, J) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, _, _, J, _).

goal_info_set_features(GoalInfo0, J, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, H, I, _, K),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, J, K).

goal_info_nondet_lives(GoalInfo, K) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, _, _, _, K).

goal_info_set_nondet_lives(GoalInfo0, K, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, H, I, J, _),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H, I, J, K).

goal_info_add_feature(GoalInfo0, Feature, GoalInfo) :-
	goal_info_get_features(GoalInfo0, J0),
	set__insert(J0, Feature, J),
	goal_info_set_features(GoalInfo0, J, GoalInfo).

goal_info_remove_feature(GoalInfo0, Feature, GoalInfo) :-
	goal_info_get_features(GoalInfo0, J0),
	set__delete(J0, Feature, J),
	goal_info_set_features(GoalInfo0, J, GoalInfo).

goal_info_has_feature(GoalInfo, Feature) :-
	goal_info_get_features(GoalInfo, J),
	set__member(Feature, J).

%-----------------------------------------------------------------------------%

	% Convert a goal to a list of conjuncts.
	% If the goal is a conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.
	%
goal_to_conj_list(Goal, ConjList) :-
	( Goal = (conj(List) - _) ->
		ConjList = List
	;
		ConjList = [Goal]
	).

	% Convert a goal to a list of disjuncts.
	% If the goal is a disjunction, then return its disjuncts
	% otherwise return the goal as a singleton list.
	%
goal_to_disj_list(Goal, DisjList) :-
	( Goal = (disj(List) - _) ->
		DisjList = List
	;
		DisjList = [Goal]
	).

	% Convert a list of conjuncts to a goal.
	% If the list contains only one goal, then return that goal,
	% otherwise return the conjunction of the conjuncts,
	% with the specified goal_info.
	%
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
	%
disj_list_to_goal(ConjList, GoalInfo, Goal) :-
	( ConjList = [Goal0] ->
		Goal = Goal0
	;
		Goal = disj(ConjList) - GoalInfo
	).

%-----------------------------------------------------------------------------%

goal_is_atomic(conj([])).
goal_is_atomic(disj([])).
goal_is_atomic(call(_,_,_,_,_,_,_)).
goal_is_atomic(unify(_,_,_,_,_)).
goal_is_atomic(pragma_c_code(_,_,_,_,_)).

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

%-----------------------------------------------------------------------------%

:- interface.

	% make_n_fresh_vars(N, VarSet0, Vars, VarSet):
	%	`Vars' is a list of `N' fresh variables allocated from
	%	`VarSet0'. `VarSet' is the resulting varset.

:- pred make_n_fresh_vars(int, varset, list(var), varset).
:- mode make_n_fresh_vars(in, in, out, out) is det.

:- implementation.

make_n_fresh_vars(N, VarSet0, Vars, VarSet) :-
	make_n_fresh_vars_2(0, N, VarSet0, Vars, VarSet).

:- pred make_n_fresh_vars_2(int, int, varset, list(var), varset).
:- mode make_n_fresh_vars_2(in, in, in, out, out) is det.

make_n_fresh_vars_2(N, Max, VarSet0, Vars, VarSet) :-
	(N = Max ->
		VarSet = VarSet0,
		Vars = []
	;
		N1 is N + 1,
		varset__new_var(VarSet0, Var, VarSet1),
		string__int_to_string(N1, Num),
		string__append("HeadVar__", Num, VarName),
		varset__name_var(VarSet1, Var, VarName, VarSet2),
		Vars = [Var | Vars1],
		make_n_fresh_vars_2(N1, Max, VarSet2, Vars1, VarSet)
	).

%-----------------------------------------------------------------------------%

:- interface.

:- type dependency_ordering	== list(list(pred_proc_id)).
:- type dependency_graph	== relation(pred_proc_id).
:- type dependency_info.

:- pred hlds__dependency_info_init(dependency_info).
:- mode hlds__dependency_info_init(out) is det.

:- pred hlds__dependency_info_get_dependency_graph(dependency_info, 
				dependency_graph).
:- mode hlds__dependency_info_get_dependency_graph(in, out) is det.

:- pred hlds__dependency_info_get_dependency_ordering(dependency_info, 
				dependency_ordering).
:- mode hlds__dependency_info_get_dependency_ordering(in, out) is det.

:- pred hlds__dependency_info_set_dependency_graph(dependency_info,
			dependency_graph, dependency_info).
:- mode hlds__dependency_info_set_dependency_graph(in, in, out) is det.

:- pred hlds__dependency_info_set_dependency_ordering(dependency_info,
			dependency_ordering, dependency_info).
:- mode hlds__dependency_info_set_dependency_ordering(in, in, out) is det.

:- implementation.

:- type dependency_info --->
		dependency_info(
			dependency_graph,	% Dependency graph
			dependency_ordering,	% Dependency ordering
			set(pred_proc_id),	% Unused procs
			unit,			% Junk slots
			unit,
			unit
		).

hlds__dependency_info_init(DepInfo) :-
	DepInfo = dependency_info(DepRel, DepOrd, Unused, unit, unit, unit),
	relation__init(DepRel),
	DepOrd = [],
	set__init(Unused).

hlds__dependency_info_get_dependency_graph(DepInfo, DepRel) :-
	DepInfo = dependency_info(DepRel, _, _, _, _, _).

hlds__dependency_info_get_dependency_ordering(DepInfo, DepOrd) :-
	DepInfo = dependency_info(_, DepOrd, _, _, _, _).

hlds__dependency_info_set_dependency_graph(DepInfo0, DepRel, DepInfo) :-
	DepInfo0 = dependency_info(_, B, C, D, E, F),
	DepInfo = dependency_info(DepRel, B, C, D, E, F).

hlds__dependency_info_set_dependency_ordering(DepInfo0, DepRel, DepInfo) :-
	DepInfo0 = dependency_info(A, _, C, D, E, F),
	DepInfo = dependency_info(A, DepRel, C, D, E, F).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
