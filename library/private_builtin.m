%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: private_builtin.m.
% Main authors: fjh, ohutch, zs.
% Stability: low.

% This file is automatically imported, as if via `use_module', into every
% module.  It is intended for builtins that are just implementation details,
% such as procedures that the compiler generates implicit calls to when
% implementing polymorphism, unification, compare/3, tabling, etc.

% This module is a private part of the Mercury implementation;
% user modules should never explicitly import this module.
% The interface for this module does not get included in the
% Mercury library reference manual.

% Many of the predicates defined in this module are builtin -
% they do not have definitions because the compiler generates code
% for them inline. Some others are implemented in the runtime.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module private_builtin.

%-----------------------------------------------------------------------------%

:- interface.


	% This section of the module contains predicates that are used
	% by the compiler, to implement polymorphism. These predicates
	% should not be used by user programs directly.

	% Changes here may also require changes in compiler/polymorphism.m,
	% compiler/higher_order.m and runtime/mercury_type_info.{c,h}.

:- pred builtin_unify_int(int::in, int::in) is semidet.
:- pred builtin_index_int(int::in, int::out) is det.
:- pred builtin_compare_int(comparison_result::uo, int::in, int::in) is det.
:- pred builtin_solve_equal_int(int::in, int::in) is semidet.

:- pred builtin_unify_character(character::in, character::in) is semidet.
:- pred builtin_index_character(character::in, int::out) is det.
:- pred builtin_compare_character(comparison_result::uo, character::in,
	character::in) is det.
:- pred builtin_solve_equal_character(character::in, character::in) is semidet.

:- pred builtin_unify_string(string::in, string::in) is semidet.
:- pred builtin_index_string(string::in, int::out) is det.
:- pred builtin_compare_string(comparison_result::uo, string::in, string::in)
	is det.
:- pred builtin_solve_equal_string(string::in, string::in) is semidet.

:- pred builtin_unify_float(float::in, float::in) is semidet.
:- pred builtin_index_float(float::in, int::out) is det.
:- pred builtin_compare_float(comparison_result::uo, float::in, float::in)
	is det.
:- pred builtin_solve_equal_float(float::in, float::in) is semidet.

:- pred builtin_unify_pred((pred)::in, (pred)::in) is semidet.
:- pred builtin_index_pred((pred)::in, int::out) is det.
:- pred builtin_compare_pred(comparison_result::uo, (pred)::in, (pred)::in)
	is det.
:- pred builtin_solve_equal_pred((pred)::in, (pred)::in) is semidet.

	% The following two preds are used for index/1 or compare/3
	% on non-canonical types (types for which there is a
	% `where equality is ...' declaration).
:- pred builtin_index_non_canonical_type(T::in, int::out) is det.
:- pred builtin_compare_non_canonical_type(comparison_result::uo,
		T::in, T::in) is det.

	% Compare_error is used in the code generated for compare/3 preds.
:- pred compare_error is erroneous.

	% The builtin < operator on ints, used in the code generated
	% for compare/3 preds.
:- pred builtin_int_lt(int, int).
:- mode builtin_int_lt(in, in) is semidet.
:- external(builtin_int_lt/2).

	% The builtin > operator on ints, used in the code generated
	% for compare/3 preds.
:- pred builtin_int_gt(int, int).
:- mode builtin_int_gt(in, in) is semidet.
:- external(builtin_int_gt/2).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, string, std_util, int, float, char, string, list.

:- pragma inline(builtin_compare_int/3).
:- pragma inline(builtin_compare_character/3).
:- pragma inline(builtin_compare_string/3).
:- pragma inline(builtin_compare_float/3).

builtin_unify_int(X, X).

builtin_index_int(X, X).

builtin_compare_int(R, X, Y) :-
	( X < Y ->
		R = (<)
	; X = Y ->
		R = (=)
	;
		R = (>)
	).

builtin_solve_equal_int(X, X).

builtin_unify_character(C, C).

builtin_index_character(C, N) :-
	char__to_int(C, N).

builtin_compare_character(R, X, Y) :-
	char__to_int(X, XI),
	char__to_int(Y, YI),
	( XI < YI ->
		R = (<)
	; XI = YI ->
		R = (=)
	;
		R = (>)
	).

builtin_solve_equal_character(C, C).

builtin_unify_string(S, S).

builtin_index_string(_, -1).

builtin_compare_string(R, S1, S2) :-
	builtin_strcmp(Res, S1, S2),
	( Res < 0 ->
		R = (<)
	; Res = 0 ->
		R = (=)
	;
		R = (>)
	).

builtin_solve_equal_string(S, S).

builtin_unify_float(F, F).

builtin_index_float(_, -1).

builtin_compare_float(R, F1, F2) :-
	( F1 < F2 ->
		R = (<)
	; F1 > F2 ->
		R = (>)
	;
		R = (=)
	).

builtin_solve_equal_float(F, F).

:- pred builtin_strcmp(int, string, string).
:- mode builtin_strcmp(out, in, in) is det.

:- pragma c_code(builtin_strcmp(Res::out, S1::in, S2::in),
	[will_not_call_mercury, thread_safe],
	"Res = strcmp(S1, S2);").

:- external(builtin_unify_pred/2).
:- external(builtin_index_pred/2).
:- external(builtin_compare_pred/3).
:- external(builtin_solve_equal_pred/2).

builtin_index_non_canonical_type(_, -1).

builtin_compare_non_canonical_type(Res, X, _Y) :-
	% suppress determinism warning
	( semidet_succeed ->
		string__append_list([
			"call to compare/3 for non-canonical type `",
			type_name(type_of(X)),
			"'"],
			Message),
		error(Message)
	;
		% the following is never executed
		Res = (<)
	).

	% This is used by the code that the compiler generates for compare/3.
compare_error :-
	error("internal error in compare/3").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% This section of the module handles the runtime representation of
	% type information.

	% The code generated by polymorphism.m always requires
	% the existence of a type_info functor, and requires
	% the existence of a type_ctor_info functor as well
	% when using --type-info {shared-,}one-or-two-cell.
	%
	% The actual arities of these two function symbols are variable;
	% they depend on the number of type parameters of the type represented
	% by the type_info, and how many predicates we associate with each
	% type.
	%
	% Note that, since these types look to the compiler as though they
	% are candidates to become no_tag types, special code is required
	% to handle them in type_util:type_is_no_tag_type/3.

:- type type_info(T) ---> type_info(type_ctor_info(T) /*, ... */).
:- type type_ctor_info(T) ---> type_ctor_info(int /*, ... */).

	% The type variable in these types isn't really a type variable,
	% it is a place for polymorphism.m to put a representation of the
	% class constraint about which the typeclass_info carries information.
	%
	% Note that, since these types look to the compiler as though they
	% are candidates to become no_tag types, special code is required
	% to handle them in type_util:type_is_no_tag_type/3.

:- type typeclass_info(T) ---> typeclass_info(base_typeclass_info(T)
						/*, ... */).
:- type base_typeclass_info(_) ---> typeclass_info(int /*, ... */).

	% type_info_from_typeclass_info(TypeClassInfo, Index, TypeInfo)
	% extracts TypeInfo from TypeClassInfo, where TypeInfo is the Indexth
	% type_info in the typeclass_info.
	%
	% Note: Index must be equal to the number of the desired type_info
	% plus the number of superclasses for this class.
:- pred type_info_from_typeclass_info(typeclass_info(_), int, type_info(T)).
:- mode type_info_from_typeclass_info(in, in, out) is det.

	% superclass_from_typeclass_info(TypeClassInfo, Index, SuperClass)
	% extracts SuperClass from TypeClassInfo where SuperClass is the
	% Indexth superclass of the class.
:- pred superclass_from_typeclass_info(typeclass_info(_),
		int, typeclass_info(_)).
:- mode superclass_from_typeclass_info(in, in, out) is det.

	% instance_constraint_from_typeclass_info(TypeClassInfo, Index,
	%       InstanceConstraintTypeClassInfo)
	% extracts the typeclass_info for the Indexth typeclass constraint
	% of the instance described by TypeClassInfo.
:- pred instance_constraint_from_typeclass_info(
		typeclass_info(_), int, typeclass_info(_)).
:- mode instance_constraint_from_typeclass_info(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

	% The definitions for type_ctor_info/1 and type_info/1.

:- pragma c_header_code("

extern MR_STATIC_CODE_CONST struct
	mercury_data___type_ctor_info_int_0_struct
	mercury_data___type_ctor_info_int_0;
extern MR_STATIC_CODE_CONST struct
	mercury_data___type_ctor_info_string_0_struct
	mercury_data___type_ctor_info_string_0;
extern MR_STATIC_CODE_CONST struct
	mercury_data___type_ctor_info_float_0_struct
	mercury_data___type_ctor_info_float_0;
extern MR_STATIC_CODE_CONST struct
	mercury_data___type_ctor_info_character_0_struct
	mercury_data___type_ctor_info_character_0;

").

:- pragma c_code("

Define_extern_entry(mercury____Unify___private_builtin__type_info_1_0);
Define_extern_entry(mercury____Index___private_builtin__type_info_1_0);
Define_extern_entry(mercury____Compare___private_builtin__type_info_1_0);
#ifdef MR_USE_SOLVE_EQUAL
Define_extern_entry(mercury____SolveEqual___private_builtin__type_info_1_0);
#endif

extern const struct
	mercury_data_private_builtin__type_ctor_layout_type_info_1_struct
	mercury_data_private_builtin__type_ctor_layout_type_info_1;
extern const struct
	mercury_data_private_builtin__type_ctor_functors_type_info_1_struct
	mercury_data_private_builtin__type_ctor_functors_type_info_1;

	/*
	** For most purposes, type_ctor_info can be treated just like
	** type_info.  The code that handles type_infos can also handle
	** type_ctor_infos.
	*/

MR_STATIC_CODE_CONST struct
mercury_data_private_builtin__type_ctor_info_type_ctor_info_1_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef MR_USE_SOLVE_EQUAL
	Code *f5;
#endif
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
	const Word *f10;
} mercury_data_private_builtin__type_ctor_info_type_ctor_info_1 = {
	((Integer) 1),
	MR_MAYBE_STATIC_CODE(ENTRY(
		mercury____Unify___private_builtin__type_info_1_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(
		mercury____Index___private_builtin__type_info_1_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(
		mercury____Compare___private_builtin__type_info_1_0)),
#ifdef MR_USE_SOLVE_EQUAL
	MR_MAYBE_STATIC_CODE(ENTRY(
		mercury____SolveEqual___private_builtin__type_info_1_0)),
#endif
	(const Word *) &
		mercury_data_private_builtin__type_ctor_layout_type_info_1,
	(const Word *) &
		mercury_data_private_builtin__type_ctor_functors_type_info_1,
	(const Word *) &
		mercury_data_private_builtin__type_ctor_layout_type_info_1,
	(const Word *) string_const(""private_builtin"", 15),
	(const Word *) string_const(""type_ctor_info"", 14)
};

MR_STATIC_CODE_CONST struct
mercury_data_private_builtin__type_ctor_info_type_info_1_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef MR_USE_SOLVE_EQUAL
	Code *f5;
#endif
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
	const Word *f10;
} mercury_data_private_builtin__type_ctor_info_type_info_1 = {
	((Integer) 1),
	MR_MAYBE_STATIC_CODE(ENTRY(
		mercury____Unify___private_builtin__type_info_1_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(
		mercury____Index___private_builtin__type_info_1_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(
		mercury____Compare___private_builtin__type_info_1_0)),
#ifdef MR_USE_SOLVE_EQUAL
	MR_MAYBE_STATIC_CODE(ENTRY(
		mercury____SolveEqual___private_builtin__type_info_1_0)),
#endif
	(const Word *) &
		mercury_data_private_builtin__type_ctor_layout_type_info_1,
	(const Word *) &
		mercury_data_private_builtin__type_ctor_functors_type_info_1,
	(const Word *) string_const(""private_builtin"", 15),
	(const Word *) string_const(""type_info"", 9)
};


const struct mercury_data_private_builtin__type_ctor_layout_type_info_1_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data_private_builtin__type_ctor_layout_type_info_1 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG,
		mkbody(MR_TYPE_CTOR_LAYOUT_TYPEINFO_VALUE))
};

const struct mercury_data_private_builtin__type_ctor_functors_type_info_1_struct {
	Integer f1;
} mercury_data_private_builtin__type_ctor_functors_type_info_1 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

BEGIN_MODULE(type_info_module)
	init_entry(mercury____Unify___private_builtin__type_info_1_0);
	init_entry(mercury____Index___private_builtin__type_info_1_0);
	init_entry(mercury____Compare___private_builtin__type_info_1_0);
#ifdef MR_USE_SOLVE_EQUAL
	init_entry(mercury____SolveEqual___private_builtin__type_info_1_0);
#endif
BEGIN_CODE
Define_entry(mercury____Unify___private_builtin__type_info_1_0);
{
	/*
	** Unification for type_info.
	**
	** The two inputs are in the registers named by unify_input[12].
	** The success/failure indication should go in unify_output.
	*/
	int comp;
	save_transient_registers();
	comp = MR_compare_type_info(unify_input1, unify_input2);
	restore_transient_registers();
	unify_output = (comp == COMPARE_EQUAL);
	proceed();
}

Define_entry(mercury____Index___private_builtin__type_info_1_0);
	index_output = -1;
	proceed();

Define_entry(mercury____Compare___private_builtin__type_info_1_0);
{
	/*
	** Comparison for type_info:
	**
	** The two inputs are in the registers named by compare_input[12].
	** The result should go in compare_output.
	*/
	int comp;
	save_transient_registers();
	comp = MR_compare_type_info(compare_input1, compare_input2);
	restore_transient_registers();
	compare_output = comp;
	proceed();
}

#ifdef MR_USE_SOLVE_EQUAL
Define_entry(mercury____SolveEqual___private_builtin__type_info_1_0);
{
	/*
	** Solve equal for type_info.
	** (this is assumed to be the same as unification)
	**
	** The two inputs are in the registers named by solve_equal_input[12].
	** The success/failure indication should go in solve_equal_output.
	*/
	int comp;
	save_transient_registers();
	comp = MR_compare_type_info(solve_equal_input1, solve_equal_input2);
	restore_transient_registers();
	solve_equal_output = (comp == COMPARE_EQUAL);
	proceed();
}
#endif
END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_type_info_module
*/
extern ModuleFunc type_info_module;
void sys_init_type_info_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_type_info_module(void) {
	type_info_module();
}

").

:- pragma c_code(type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
	TypeInfo::out), [will_not_call_mercury, thread_safe],
"
	TypeInfo = MR_typeclass_info_type_info(TypeClassInfo, Index);
").

:- pragma c_code(superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
	TypeClassInfo::out), [will_not_call_mercury, thread_safe],
"
	TypeClassInfo =
		MR_typeclass_info_superclass_info(TypeClassInfo0, Index);
").

:- pragma c_code(instance_constraint_from_typeclass_info(TypeClassInfo0::in,
        Index::in, TypeClassInfo::out), [will_not_call_mercury, thread_safe],
"
	TypeClassInfo =
		MR_typeclass_info_arg_typeclass_info(TypeClassInfo0, Index);
").

%-----------------------------------------------------------------------------%

:- interface.

	% This section of the module is for miscellaneous predicates
	% that sometimes have calls to them emitted by the compiler.

	% unsafe_type_cast/2 is used internally by the compiler. Bad things
	% will happen if this is used in programs. It has no definition,
	% since for efficiency the code generator treats it as a builtin.

:- pred unsafe_type_cast(T1, T2).
:- mode unsafe_type_cast(in, out) is det.

:- pred unused is det.

:- implementation.

unused :-
	( semidet_succeed ->
		error("attempted use of dead predicate")
	;
		% the following is never executed
		true
	).

%-----------------------------------------------------------------------------%

:- interface.

% This section of the module contains the predicates that are
% automatically inserted by the table_gen pass of the compiler
% into predicates that use tabling, and the types they use.
%
% The predicates fall into three categories:
%
% (1)	Predicates that manage the tabling of simple subgoals.
%	A subgoal is simple if its predicate is model_det or model_semi,
%	which means that its evaluation method must be something
%	other than minimal model.
%
% (2)	Predicates that manage the tabling of model_non subgoals,
%	which usually means that its evaluation method is minimal model.
%
% (3)	Utility predicates that are needed in the tabling of both
%	simple and nondet subgoals.
%
% The utility predicates that handle tries are combined lookup/insert
% operations; if the item being searched for is not already in the trie,
% they insert it. These predicates are used to implement both subgoal tables,
% in which case the items inserted are input arguments of a tabled predicate,
% and answer tables, in which case the items inserted are output arguments
% of a tabled predicate.
%
% The subgoal table trie is used for detecting duplicate calls,
% while the answer table trie is used for detecting duplicate answers.
% However, storing answers only in the answer table trie is not sufficient,
% for two reasons. First, while the trie encodes the values of the output
% arguments, this encoding is not in the form of the native Mercury
% representations of those arguments. Second, for model_non subgoals we
% want a chronological list of answers, to allow us to separate out
% answers we have returned already from answers we have not yet returned.
% To handle the first problem, we save each answer not only in the
% answer table trie but also in an answer block, which is a vector of N
% elements, where N is the number of output arguments of the procedure
% concerned. To handle the second problem, for model_non procedures
% we chain these answer blocks together in a chronological list.
%
% For simple goals, the word at the end of the subgoal table trie is used
% first as a status indication (of type MR_SimpletableStatus), and later on
% as a pointer to an answer block (if the goal succeeded). This is OK, because
% we can distinguish the two, and because an answer block pointer can be
% associated with only one status value.
%
% For nondet goals, the word at the end of the subgoal table trie always
% points to a subgoal structure, with several fields. The status of the
% subgoal and the list of answers are two of these fields. Other fields,
% described in runtime/mercury_tabling.h, are used in the implementation
% of the minimal model.
%
% All of the predicates here with the impure declaration modify the tabling
% structures. Because the structures are persistent through backtracking,
% this causes the predicates to become impure. The predicates with the semipure
% directive only examine the tabling structures, but do not modify them.

	% This type is used as a generic table: it can in fact represent two
	% types, either a subgoal_table or an answer_table. The subgoal_table
	% and answer_table types are differentiated by what they have at the
	% table nodes but not by the actual underlying trie structure.
:- type ml_table.

	% This type is used in contexts where a node of a subgoal table is
	% expected.
:- type ml_subgoal_table_node.

	% This type is used in contexts where a node of an answer table is
	% expected.
:- type ml_answer_table_node.

	% This type is used in contexts where an answer slot is expected.
:- type ml_answer_slot.

	% This type is used in contexts where an answer block is expected.
:- type ml_answer_block.

	% These equivalences should be local to private_builtin. However,
	% at the moment table_gen.m assumes that it can use a single variable
	% sometimes as an ml_table and other times as an ml_subgoal_table_node
	% (e.g. by giving the output of table_lookup_insert_int as input to
	% table_have_all_ans). The proper fix would be for table_gen.m to
	% use additional variables and insert unsafe casts. However, this
	% would require significant work for no real gain, so for now
	% we fix the problem by exposing the equivalences to code generated
	% by table_gen.m.
:- type ml_subgoal_table_node == ml_table.
:- type ml_answer_table_node == ml_table.
:- type ml_answer_slot == ml_table.
:- type ml_answer_block == ml_table.
:- type ml_table == c_pointer.

:- implementation.

% This equivalence should be private. However, polymorphism gets an
% internal error when compiling tests/tabling/boyer.m if it is.
% :- type ml_table == c_pointer.

%-----------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage the tabling of simple subgoals.
%

	% Return true if the subgoal represented by the given table has an
	% answer.
:- semipure pred table_simple_is_complete(ml_subgoal_table_node::in)
	is semidet.

	% Return true if the subgoal represented by the given table has a
	% true answer.
:- semipure pred table_simple_has_succeeded(ml_subgoal_table_node::in)
	is semidet.

	% Return true if the subgoal represented by the given table has
	% failed.
:- semipure pred table_simple_has_failed(ml_subgoal_table_node::in) is semidet.

	% Return true if the subgoal represented by the given table is
	% currently being evaluated (working on an answer).
:- semipure pred table_simple_is_active(ml_subgoal_table_node::in) is semidet.

	% Return false if the subgoal represented by the given table is
	% currently being evaluated (working on an answer).
:- semipure pred table_simple_is_inactive(ml_subgoal_table_node::in)
	is semidet.

	% Save the fact the the subgoal has succeeded in the given table.
:- impure pred table_simple_mark_as_succeeded(ml_subgoal_table_node::in)
	is det.

	% Save the fact the the subgoal has failed in the given table.
:- impure pred table_simple_mark_as_failed(ml_subgoal_table_node::in) is det.

	% Mark the subgoal represented by the given table as currently
	% being evaluated (working on an answer).
:- impure pred table_simple_mark_as_active(ml_subgoal_table_node::in) is det.

	% Mark the subgoal represented by the given table as currently
	% not being evaluated (working on an answer).
:- impure pred table_simple_mark_as_inactive(ml_subgoal_table_node::in) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma c_code(table_simple_is_complete(T::in), will_not_call_mercury, "
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if %p is succeeded or failed: %lu\\n"",
			(Unsigned *) T, (unsigned long) (*((Unsigned *) T)));
	}
#endif
	SUCCESS_INDICATOR = 
		((*((Unsigned *) T) == MR_SIMPLETABLE_FAILED)
		|| (*((Unsigned *) T) >= MR_SIMPLETABLE_SUCCEEDED));
").

:- pragma c_code(table_simple_has_succeeded(T::in), will_not_call_mercury, "
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if %p is succeeded: %lu\\n"",
			(Unsigned *) T, (unsigned long) (*((Unsigned *) T)));
	}
#endif
	SUCCESS_INDICATOR = (*((Unsigned *) T) >= MR_SIMPLETABLE_SUCCEEDED)
").

:- pragma c_code(table_simple_has_failed(T::in), will_not_call_mercury, "
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if %p is failed: %lu\\n"",
			(Unsigned *) T, (unsigned long) (*((Unsigned *) T)));
	}
#endif
	SUCCESS_INDICATOR = (*((Unsigned *) T) == MR_SIMPLETABLE_FAILED);
").

:- pragma c_code(table_simple_is_active(T::in), will_not_call_mercury, "
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if %p is active: %lu\\n"",
			(Unsigned *) T, (unsigned long) (*((Unsigned *) T)));
	}
#endif
	SUCCESS_INDICATOR = (*((Unsigned *) T) == MR_SIMPLETABLE_WORKING);
").

:- pragma c_code(table_simple_is_inactive(T::in), will_not_call_mercury, "
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if %p is not inactive: %lu\\n"",
			(Unsigned *) T, (unsigned long) (*((Unsigned *) T)));
	}
#endif
	SUCCESS_INDICATOR = (*((Unsigned *) T) != MR_SIMPLETABLE_WORKING);
").

:- pragma c_code(table_simple_mark_as_succeeded(T::in), will_not_call_mercury, "
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""marking %p as succeeded\\n"", (Unsigned *) T);
	}
#endif
	*((Unsigned *) T) = MR_SIMPLETABLE_SUCCEEDED;
").

:- pragma c_code(table_simple_mark_as_failed(T::in), will_not_call_mercury, "
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""marking %p as failed\\n"", (Unsigned *) T);
	}
#endif
	*((Unsigned *) T) = MR_SIMPLETABLE_FAILED;
").

:- pragma c_code(table_simple_mark_as_active(T::in), will_not_call_mercury, "
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""marking %p as working\\n"", (Unsigned *) T);
	}
#endif
	*((Unsigned *) T) = MR_SIMPLETABLE_WORKING;
").

:- pragma c_code(table_simple_mark_as_inactive(T::in), will_not_call_mercury, "
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""marking %p as uninitialized\\n"", (Unsigned *) T);
	}
#endif
	*((Unsigned *) T) = MR_SIMPLETABLE_UNINITIALIZED;
").

%-----------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage the tabling of model_non subgoals.
%

	% Save the information that will be needed later about this
	% nondet subgoal in a data structure. If we have already seen
	% this subgoal before, do nothing.
:- impure pred table_nondet_setup(ml_subgoal_table_node::in,
	ml_subgoal_table_node::out) is det.

	% Save the state of the current subgoal and fail. Sometime later,
	% when the subgoal has some solutions, table_nondet_resume will
	% restore the saved state. At the time, table_nondet_suspend will
	% succeed, and return an answer block as its second argument.
:- impure pred table_nondet_suspend(ml_subgoal_table_node::in,
	ml_answer_block::out) is nondet.

	% Resume all suspended subgoal calls. This predicate will resume each
	% of the suspended subgoals that depend on it in turn until it reaches
	% a fixed point, at which all depended suspended subgoals have had
	% all available answers returned to them.
:- impure pred table_nondet_resume(ml_subgoal_table_node::in) is det.

	% Succeed if we have finished generating all answers for
	% the given nondet subgoal.
:- semipure pred table_nondet_is_complete(ml_subgoal_table_node::in)
	is semidet.

	% Succeed if the given nondet subgoal is active,
	% i.e. the process of computing all its answers is not yet complete.
:- semipure pred table_nondet_is_active(ml_subgoal_table_node::in) is semidet.

	% Mark a table as being active.
:- impure pred table_nondet_mark_as_active(ml_subgoal_table_node::in) is det.

	% Return the table of answers already return to the given nondet
	% table.
:- impure pred table_nondet_get_ans_table(ml_subgoal_table_node::in,
	ml_table::out) is det.

	% If the answer represented by the given answer table
	% has not been generated before by this subgoal,
	% succeed and remember the answer as having been generated.
	% If the answer has been generated before, fail.
:- impure pred table_nondet_answer_is_not_duplicate(ml_answer_table_node::in)
	is semidet.

	% Create a new slot in the answer list.
:- impure pred table_nondet_new_ans_slot(ml_subgoal_table_node::in,
	ml_answer_slot::out) is det.

	% Return all of the answer blocks stored in the given table.
:- semipure pred table_nondet_return_all_ans(ml_subgoal_table_node::in,
	ml_answer_block::out) is nondet.

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma c_code(table_nondet_setup(T0::in, T::out), will_not_call_mercury, "
#ifdef	MR_USE_MINIMAL_MODEL
#ifdef	MR_THREAD_SAFE
#error ""Sorry, not yet implemented: mixing minimal model tabling and threads""
#endif
	/*
	** Initialize the subgoal if this is the first time we see it.
	** If the subgoal structure already exists but is marked inactive,
	** then it was left by a previous generator that couldn't
	** complete the evaluation of the subgoal due to a commit.
	** In that case, we want to forget all about the old generator.
	*/

	if (MR_SUBGOAL(T0) == NULL) {
		MR_Subgoal	*subgoal;

		subgoal = (MR_Subgoal *)
			table_allocate_bytes(sizeof(MR_Subgoal));
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf(""setting up table %p -> %p\n"",
				(MR_Subgoal **) T0, subgoal);
		}
#endif
		subgoal->status = MR_SUBGOAL_INACTIVE;
		subgoal->leader = NULL;
		subgoal->followers = make(struct MR_SubgoalListNode);
		subgoal->followers->item = subgoal;
		subgoal->followers->next = NULL;
		subgoal->followers_tail = &(subgoal->followers->next);
		subgoal->answer_table = (Word) NULL;
		subgoal->num_ans = 0;
		subgoal->answer_list = NULL;
		subgoal->answer_list_tail = &subgoal->answer_list;
		subgoal->consumer_list = NULL;
		subgoal->consumer_list_tail = &subgoal->consumer_list;
#ifdef	MR_TABLE_DEBUG
		if (MR_maxfr != MR_curfr) {
			fatal_error(""MR_maxfr != MR_curfr at table setup\n"");
		}
#endif
		subgoal->generator_maxfr = MR_prevfr_slot(MR_maxfr);
		subgoal->generator_sp = MR_sp;
		MR_SUBGOAL(T0) = subgoal;
	}
	T = T0;
#else
	fatal_error(""minimal model code entered when not enabled"");
#endif
").

	% The definitions of these two predicates are in the runtime system,
	% in runtime/mercury_tabling.c.
:- external(table_nondet_suspend/2).
:- external(table_nondet_resume/1).

:- pragma c_code(table_nondet_is_complete(T::in),"
#ifdef	MR_USE_MINIMAL_MODEL
	SUCCESS_INDICATOR = (MR_SUBGOAL(T)->status == MR_SUBGOAL_COMPLETE);
#else
	fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma c_code(table_nondet_is_active(T::in), will_not_call_mercury, "
#ifdef	MR_USE_MINIMAL_MODEL
	SUCCESS_INDICATOR = (MR_SUBGOAL(T)->status == MR_SUBGOAL_ACTIVE);
#else
	fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma c_code(table_nondet_mark_as_active(T::in), will_not_call_mercury, "
#ifdef	MR_USE_MINIMAL_MODEL
	MR_push_generator(MR_curfr, MR_SUBGOAL(T));
	MR_register_generator_ptr((MR_Subgoal **) T);
	MR_SUBGOAL(T)->status = MR_SUBGOAL_ACTIVE;
#else
	fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma c_code(table_nondet_get_ans_table(T::in, AT::out),
		will_not_call_mercury, "
#ifdef	MR_USE_MINIMAL_MODEL
	AT = (Word) &(MR_SUBGOAL(T)->answer_table);
#else
	fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma c_code(table_nondet_answer_is_not_duplicate(T::in),
		will_not_call_mercury, "
#ifdef	MR_USE_MINIMAL_MODEL
	bool	is_new_answer;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if %p is a duplicate answer: %d\\n"",
			(Word *) T, *((Word *) T));
	}
#endif
	is_new_answer = (*((Word *) T) == MR_ANS_NOT_GENERATED);
	*((Word *) T) = MR_ANS_GENERATED;
	SUCCESS_INDICATOR = is_new_answer;
#else
	fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma c_code(table_nondet_new_ans_slot(T::in, Slot::out),
		will_not_call_mercury, "
#ifdef	MR_USE_MINIMAL_MODEL
	MR_Subgoal		*table;
	MR_AnswerListNode	*answer_node;

	table = MR_SUBGOAL(T);
	table->num_ans += 1;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""new answer slot %d, storing into addr %p\\n"",
			table->num_ans, table->answer_list_tail);
	}
#endif
	/*
	**
	** We fill in the answer_data slot with a dummy value.
	** This slot will be filled in by the next piece of code
	** to be executed after we return, which is why we return its address.
	*/

	answer_node = table_allocate_bytes(sizeof(MR_AnswerListNode));
	answer_node->answer_num = table->num_ans;
	answer_node->answer_data = 0;
	answer_node->next_answer = NULL;

	*(table->answer_list_tail) = answer_node;
	table->answer_list_tail = &(answer_node->next_answer);

	Slot = (Word) &(answer_node->answer_data);
#else
	fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma c_code(table_nondet_return_all_ans(T::in, A::out),
	will_not_call_mercury,
	local_vars("
#ifdef MR_USE_MINIMAL_MODEL
		MR_AnswerList	cur_node;
#endif
	"),
	first_code("
#ifdef MR_USE_MINIMAL_MODEL
		LOCALS->cur_node = MR_SUBGOAL(T)->answer_list;
#endif
	"),
	retry_code("
	"),
	shared_code("
#ifdef MR_USE_MINIMAL_MODEL
		if (LOCALS->cur_node == NULL) {
			FAIL;
		} else {
			A = LOCALS->cur_node->answer_data;
			LOCALS->cur_node = LOCALS->cur_node->next_answer;
			SUCCEED;
		}
#else
		fatal_error(""minimal model code entered when not enabled"");
#endif
	")
).
%-----------------------------------------------------------------------------%

:- interface.

%
% Utility predicates that are needed in the tabling of both
% simple and nondet subgoals.
%

%
% The following table_lookup_insert... predicates lookup or insert the second
% argument into the trie pointed to by the first argument. The value returned
% is a pointer to the leaf of the trie reached by the lookup. From the
% returned leaf another trie may be connected.
%
	% Lookup or insert an integer in the given table.
:- impure pred table_lookup_insert_int(ml_table::in, int::in, ml_table::out)
	is det.

	% Lookup or insert a character in the given trie.
:- impure pred table_lookup_insert_char(ml_table::in, character::in,
	ml_table::out) is det.

	% Lookup or insert a string in the given trie.
:- impure pred table_lookup_insert_string(ml_table::in, string::in,
	ml_table::out) is det.

	% Lookup or insert a float in the current trie.
:- impure pred table_lookup_insert_float(ml_table::in, float::in,
	ml_table::out) is det.

	% Lookup or inert an enumeration type in the given trie.
:- impure pred table_lookup_insert_enum(ml_table::in, int::in, T::in,
	ml_table::out) is det.

	% Lookup or insert a monomorphic user defined type in the given trie.
:- impure pred table_lookup_insert_user(ml_table::in, T::in, ml_table::out)
	is det.

	% Lookup or insert a polymorphic user defined type in the given trie.
:- impure pred table_lookup_insert_poly(ml_table::in, T::in, ml_table::out)
	is det.

	% Save an integer answer in the given answer block at the given
	% offset.
:- impure pred table_save_int_ans(ml_answer_block::in, int::in, int::in)
	is det.

	% Save a character answer in the given answer block at the given
	% offset.
:- impure pred table_save_char_ans(ml_answer_block::in, int::in, character::in)
	is det.

	% Save a string answer in the given answer block at the given
	% offset.
:- impure pred table_save_string_ans(ml_answer_block::in, int::in, string::in)
	is det.

	% Save a float answer in the given answer block at the given
	% offset.
:- impure pred table_save_float_ans(ml_answer_block::in, int::in, float::in)
	is det.

	% Save any type of answer in the given answer block at the given
	% offset.
:- impure pred table_save_any_ans(ml_answer_block::in, int::in, T::in) is det.

	% Restore an integer answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_int_ans(ml_answer_block::in, int::in, int::out)
	is det.

	% Restore a character answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_char_ans(ml_answer_block::in, int::in,
	character::out) is det.

	% Restore a string answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_string_ans(ml_answer_block::in, int::in,
	string::out) is det.

	% Restore a float answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_float_ans(ml_answer_block::in, int::in,
	float::out) is det.

	% Restore any type of answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_any_ans(ml_answer_block::in, int::in, T::out)
	is det.

	% Report an error message about the current subgoal looping.
:- pred table_loopcheck_error(string::in) is erroneous.

	% Create an answer block with the given number of slots and add it
	% to the given table.
:- impure pred table_create_ans_block(ml_subgoal_table_node::in, int::in,
	ml_answer_block::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma c_code(table_lookup_insert_int(T0::in, I::in, T::out),
		will_not_call_mercury, "
	MR_DEBUG_NEW_TABLE_INT(T, T0, I);
").

:- pragma c_code(table_lookup_insert_char(T0::in, C::in, T::out),
		will_not_call_mercury, "
	MR_DEBUG_NEW_TABLE_CHAR(T, T0, C);
").

:- pragma c_code(table_lookup_insert_string(T0::in, S::in, T::out),
		will_not_call_mercury, "
	MR_DEBUG_NEW_TABLE_STRING(T, T0, S);
").

:- pragma c_code(table_lookup_insert_float(T0::in, F::in, T::out),
		will_not_call_mercury, "
	MR_DEBUG_NEW_TABLE_FLOAT(T, T0, F);
").

:- pragma c_code(table_lookup_insert_enum(T0::in, R::in, V::in, T::out),
		will_not_call_mercury, "
	MR_DEBUG_NEW_TABLE_ENUM(T, T0, R, V);
").

:- pragma c_code(table_lookup_insert_user(T0::in, V::in, T::out),
		will_not_call_mercury, "
	MR_DEBUG_NEW_TABLE_ANY(T, T0, TypeInfo_for_T, V);
").

:- pragma c_code(table_lookup_insert_poly(T0::in, V::in, T::out),
		will_not_call_mercury, "
	Word T1;
	MR_DEBUG_NEW_TABLE_TYPEINFO(T1, T0, TypeInfo_for_T);
	MR_DEBUG_NEW_TABLE_ANY(T, T1, TypeInfo_for_T, V);
").

:- pragma c_code(table_save_int_ans(T::in, Offset::in, I::in),
		will_not_call_mercury, "
	MR_TABLE_SAVE_ANSWER(Offset, T, I,
		mercury_data___type_ctor_info_int_0);
").

:- pragma c_code(table_save_char_ans(T::in, Offset::in, C::in),
		will_not_call_mercury, "
	MR_TABLE_SAVE_ANSWER(Offset, T, C,
		mercury_data___type_ctor_info_character_0);
").

:- pragma c_code(table_save_string_ans(T::in, Offset::in, S::in),
		will_not_call_mercury, "
	MR_TABLE_SAVE_ANSWER(Offset, T, (Word) S,
		mercury_data___type_ctor_info_string_0);
").

:- pragma c_code(table_save_float_ans(T::in, Offset::in, F::in),
		will_not_call_mercury, "
	MR_TABLE_SAVE_ANSWER(Offset, T, float_to_word(F),
		mercury_data___type_ctor_info_float_0);
").

:- pragma c_code(table_save_any_ans(T::in, Offset::in, V::in),
		will_not_call_mercury, "
	MR_TABLE_SAVE_ANSWER(Offset, T, V, TypeInfo_for_T);
").

:- pragma c_code(table_restore_int_ans(T::in, Offset::in, I::out),
		will_not_call_mercury, "
	I = (Integer) MR_TABLE_GET_ANSWER(Offset, T);
").

:- pragma c_code(table_restore_char_ans(T::in, Offset::in, C::out),
		will_not_call_mercury, "
	C = (Char) MR_TABLE_GET_ANSWER(Offset, T);
").

:- pragma c_code(table_restore_string_ans(T::in, Offset::in, S::out),
		will_not_call_mercury, "
	S = (String) MR_TABLE_GET_ANSWER(Offset, T);
").

:- pragma c_code(table_restore_float_ans(T::in, Offset::in, F::out),
		will_not_call_mercury, "
	F = word_to_float(MR_TABLE_GET_ANSWER(Offset, T));
").

:- pragma c_code(table_restore_any_ans(T::in, Offset::in, V::out),
		will_not_call_mercury, "
	V = (Word) MR_TABLE_GET_ANSWER(Offset, T);
").

:- pragma c_code(table_create_ans_block(T0::in, Size::in, T::out),
		will_not_call_mercury, "
	MR_TABLE_CREATE_ANSWER_BLOCK(T0, Size);
	T = T0;
").

table_loopcheck_error(Message) :-
	error(Message).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
