%---------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: private_builtin.m.
% Main authors: fjh, zs.
% Stability: low.

% This file is automatically imported, as if via `use_module', into every
% module.  It is intended for builtins that are just implementation details,
% such as procedures that the compiler generates implicit calls to when
% implementing polymorphism, unification, compare/3, etc.
% Note that the builtins used for tabling are in a separate module
% (table_builtin.m).

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

	% free_heap/1 is used internally by the compiler to implement
	% compile-time garbage collection. Don't use it in programs.
	% The `di' mode on the argument is overly conservative -- only
	% the top-level cell is clobbered. This is handled correctly by
	% mode_util__recompute_instmap_delta.
:- pred free_heap(_T).
:- mode free_heap(di) is det.
:- external(free_heap/1).

%-----------------------------------------------------------------------------%

	% This section of the module contains predicates that are used
	% by the compiler, to implement polymorphism. These predicates
	% should not be used by user programs directly.

	% Changes here may also require changes in compiler/polymorphism.m,
	% compiler/unify_proc.m, compiler/higher_order.m and
	% runtime/mercury_type_info.{c,h}.

:- pred builtin_unify_int(int::in, int::in) is semidet.
:- pred builtin_compare_int(comparison_result::uo, int::in, int::in) is det.
:- pred builtin_solve_equal_int(int::in, int::in) is semidet.
:- pred builtin_init_int(int::out(any)) is det.

:- pred builtin_unify_character(character::in, character::in) is semidet.
:- pred builtin_compare_character(comparison_result::uo, character::in,
	character::in) is det.
:- pred builtin_solve_equal_character(character::in, character::in) is semidet.
:- pred builtin_init_character(character::out(any)) is det.

:- pred builtin_unify_string(string::in, string::in) is semidet.
:- pred builtin_compare_string(comparison_result::uo, string::in, string::in)
	is det.
:- pred builtin_solve_equal_string(string::in, string::in) is semidet.
:- pred builtin_init_string(string::out(any)) is det.

:- pred builtin_unify_float(float::in, float::in) is semidet.
:- pred builtin_compare_float(comparison_result::uo, float::in, float::in)
	is det.
:- pred builtin_solve_equal_float(float::in, float::in) is semidet.
:- pred builtin_init_float(float::out(any)) is det.

:- pred builtin_unify_pred((pred)::in, (pred)::in) is semidet.
:- pred builtin_compare_pred(comparison_result::uo, (pred)::in, (pred)::in)
	is det.
:- pred builtin_solve_equal_pred((pred)::in, (pred)::in) is semidet.
:- pred builtin_init_pred((pred)::out(any)) is det.

	% These should never be called -- the compiler never
	% specializes them because the generic compare is just
	% as good as anything we could put here.
:- pred builtin_unify_tuple(T::in, T::in) is semidet.
:- pred builtin_compare_tuple(comparison_result::uo, T::in, T::in) is det.
:- pred builtin_solve_equal_tuple(T::in, T::in) is semidet.
:- pred builtin_init_tuple(T::out(any)) is det.

	% The following pred is used for compare/3
	% on non-canonical types (types for which there is a
	% `where equality is ...' declaration).
:- pred builtin_compare_non_canonical_type(comparison_result::uo,
		T::in, T::in) is det.

	% Compare_error is used in the code generated for compare/3 preds.
:- pred compare_error is erroneous.

% The following pred is used for calls to solve_equal/2 for non-solver types
% (i.e. types for which no solve predicate has been specified).
:- pred builtin_solve_equal_non_solver_type(T::(any->any), T::(any->any))
		is semidet.

% The following pred is used for calls to init/1 for non-solver types
% (i.e. types for which no init predicate has been specified).
:- pred builtin_init_non_solver_type(T::(free->any)) is det.

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

	% A "typed" version of unify/2 -- i.e. one that can handle arguments
	% of different types.  It first unifies their types, and then if
	% the types are equal it unifies the values.
:- pred typed_unify(T1, T2).
:- mode typed_unify(in, in) is semidet.

	% A "typed" version of compare/3 -- i.e. one that can handle arguments
	% of different types.  It first compares the types, and then if the
	% types are equal it compares the values.
:- pred typed_compare(comparison_result, T1, T2).
:- mode typed_compare(uo, in, in) is det.

	% A "typed" version of solve_equal/2 -- i.e. one that can handle
	% arguments of different types.  It first unifies their types, and
	% then if the types are equal it solves the values.
:- pred typed_solve_equal(T1, T2).
:- mode typed_solve_equal((any->any), (any->any)) is semidet.

	% N.B. interface continued below.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, string, std_util, int, float, char, string, list.

:- pragma inline(builtin_compare_int/3).
:- pragma inline(builtin_compare_character/3).
:- pragma inline(builtin_compare_string/3).
:- pragma inline(builtin_compare_float/3).

builtin_unify_int(X, X).

builtin_compare_int(R, X, Y) :-
	( X < Y ->
		R = (<)
	; X = Y ->
		R = (=)
	;
		R = (>)
	).

builtin_solve_equal_int(X, X).

builtin_init_int(X) :-
	builtin_init_non_solver_type(X).

builtin_unify_character(C, C).

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

builtin_init_character(C) :-
	builtin_init_non_solver_type(C).

builtin_unify_string(S, S).

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

builtin_init_string(S) :-
	builtin_init_non_solver_type(S).

:- pred builtin_strcmp(int, string, string).
:- mode builtin_strcmp(out, in, in) is det.

:- pragma c_code(builtin_strcmp(Res::out, S1::in, S2::in),
	[will_not_call_mercury, thread_safe],
	"Res = strcmp(S1, S2);").

builtin_unify_float(F, F).

builtin_compare_float(R, F1, F2) :-
	( F1 < F2 ->
		R = (<)
	; F1 > F2 ->
		R = (>)
	;
		R = (=)
	).

builtin_solve_equal_float(F, F).

builtin_init_float(F) :-
	builtin_init_non_solver_type(F).

:- pragma no_inline(builtin_unify_pred/2).
builtin_unify_pred(_X, _Y) :-
	( semidet_succeed ->
		error("attempted higher-order unification")
	;
		% the following is never executed
		semidet_succeed
	).

:- pragma no_inline(builtin_compare_pred/3).
builtin_compare_pred(Result, _X, _Y) :-
	( semidet_succeed ->
		error("attempted higher-order comparison")
	;
		% the following is never executed
		Result = (<)
	).

:- pragma no_inline(builtin_solve_equal_pred/2).
builtin_solve_equal_pred(X, Y) :-
	builtin_solve_equal_non_solver_type(X, Y).

:- pragma no_inline(builtin_init_pred/1).
builtin_init_pred(X) :-
	builtin_init_non_solver_type(X).

builtin_unify_tuple(_, _) :-
	( semidet_succeed ->
		% The generic unification function in the runtime
		% should handle this itself.
		error("builtin_unify_tuple called")
	;
		% the following is never executed
		semidet_succeed
	).

builtin_compare_tuple(Res, _, _) :-
	( semidet_succeed ->
		% The generic comparison function in the runtime
		% should handle this itself.
		error("builtin_compare_tuple called")
	;
		% the following is never executed
		Res = (<)
	).

:- pragma no_inline(builtin_solve_equal_tuple/2).
builtin_solve_equal_tuple(X, Y) :-
	builtin_solve_equal_non_solver_type(X, Y).

:- pragma no_inline(builtin_init_tuple/1).
builtin_init_tuple(X) :-
	builtin_init_non_solver_type(X).

builtin_solve_equal_non_solver_type(X, _Y) :-
	% suppress determinism warning
	( semidet_succeed ->
		string__append_list([
			"call to solve_equal/2 for non-solver type `",
			type_name(type_of(X)),
			"'"],
			Message),
		error(Message)
	;
		% the following is never executed
		semidet_succeed
	).

builtin_init_non_solver_type(X) :-
	% suppress determinism warning
	( semidet_succeed ->
		string__append_list([
			"call to init/1 for non-solver type `",
			type_name(type_of(X)),
			"'"],
			Message),
		error(Message)
	;
		% the following is never executed
		% We use a recursive call to init/1 to get the mode of X
		% right.
		init(X)
	).

:- pragma no_inline(builtin_compare_non_canonical_type/3).
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
:- pragma no_inline(compare_error/0).
compare_error :-
	error("internal error in compare/3").

	% XXX These could be implemented more efficiently using
	%     `pragma c_code' -- the implementation below does some
	%     unnecessary memory allocatation.
typed_unify(X, Y) :- univ(X) = univ(Y).
typed_compare(R, X, Y) :- compare(R, univ(X), univ(Y)).

	% typed_solve_equal/2 should not exist unless the `se' grade
	% component has been specified.  However, having a predicate exist
	% only in some grades is not possible.  Unfortunately, if we
	% implement it in Mercury as:
	% 	typed_solve_equal(X, Y) :- solve_equal(univ(X), univ(Y)).
	% then the compiler aborts with an error in non-`se' grades, while
	% trying to specialise the solve_equal/2 call based on the type.
	% So we use the following hack to get around it.
typed_solve_equal(X, Y) :- my_solve_equal(univ(X), univ(Y)).

:- pred my_solve_equal(T, T).
:- mode my_solve_equal((any->any), (any->any)) is semidet.

my_solve_equal(X, Y) :-
	solve_equal(X, Y).

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

	% unconstrained_type_info_from_typeclass_info(TypeClassInfo, 
	%               Index, TypeInfo)
	% extracts the TypeInfo for the Indexth unconstrained type variable
	% from the instance represented by TypeClassInfo.
:- pred unconstrained_type_info_from_typeclass_info(typeclass_info(_),
		int, type_info(_)).
:- mode unconstrained_type_info_from_typeclass_info(in, in, out) is det.

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
	%
	% Note: Index must be equal to the number of the desired constraint
	% plus the number of unconstrained type variables for this instance.
:- pred instance_constraint_from_typeclass_info(
		typeclass_info(_), int, typeclass_info(_)).
:- mode instance_constraint_from_typeclass_info(in, in, out) is det.

	% N.B. interface continued below.

%-----------------------------------------------------------------------------%

:- implementation.

	% The definitions for type_ctor_info/1 and type_info/1.

:- pragma c_code("

#ifdef MR_HIGHLEVEL_CODE
void sys_init_type_info_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_type_info_module(void) { return; }
#else

	/*
	** For most purposes, type_ctor_info can be treated just like
	** type_info.  The code that handles type_infos can also handle
	** type_ctor_infos.
	*/

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_PRED(private_builtin, type_ctor_info, 1,
	MR_TYPECTOR_REP_TYPEINFO,
	mercury____Unify___private_builtin__type_info_1_0,
	mercury____Compare___private_builtin__type_info_1_0,
	mercury____SolveEqual___private_builtin__type_info_1_0,
	mercury____Init___private_builtin__type_info_1_0);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(private_builtin, type_info, 1,
	MR_TYPECTOR_REP_TYPEINFO);

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_PRED(private_builtin, base_typeclass_info, 1,
	MR_TYPECTOR_REP_TYPECLASSINFO,
	mercury____Unify___private_builtin__typeclass_info_1_0,
	mercury____Compare___private_builtin__typeclass_info_1_0,
	mercury____SolveEqual___private_builtin__typeclass_info_1_0,
	mercury____Init___private_builtin__typeclass_info_1_0);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(private_builtin, typeclass_info, 1,
	MR_TYPECTOR_REP_TYPECLASSINFO);

BEGIN_MODULE(type_info_module)
	init_entry(mercury____Unify___private_builtin__type_info_1_0);
	init_entry(mercury____Compare___private_builtin__type_info_1_0);
#ifdef MR_USE_SOLVE_EQUAL
	init_entry(mercury____SolveEqual___private_builtin__type_info_1_0);
#endif
#ifdef MR_USE_INIT
	init_entry(mercury____Init___private_builtin__type_info_1_0);
#endif
	init_entry(mercury____Unify___private_builtin__typeclass_info_1_0);
	init_entry(mercury____Compare___private_builtin__typeclass_info_1_0);
#ifdef MR_USE_SOLVE_EQUAL
	init_entry(mercury____SolveEqual___private_builtin__typeclass_info_1_0);
#endif
#ifdef MR_USE_INIT
	init_entry(mercury____Init___private_builtin__typeclass_info_1_0);
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
	int	comp;

	save_transient_registers();
	comp = MR_compare_type_info((MR_TypeInfo) r1, (MR_TypeInfo) r2);
	restore_transient_registers();
	r1 = (comp == MR_COMPARE_EQUAL);
	proceed();
}

Define_entry(mercury____Compare___private_builtin__type_info_1_0);
{
	/*
	** Comparison for type_info:
	**
	** The two inputs are in the registers named by compare_input[12].
	** The result should go in compare_output.
	*/
	int	comp;

	save_transient_registers();
	comp = MR_compare_type_info((MR_TypeInfo) r1, (MR_TypeInfo) r2);
	restore_transient_registers();
	r1 = comp;
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
	comp = MR_compare_type_info(r1, r2);
	restore_transient_registers();
	r1 = (comp == COMPARE_EQUAL);
	proceed();
}
#endif

#ifdef MR_USE_INIT
Define_entry(mercury____Init___private_builtin__type_info_1_0);
{
	MR_fatal_error(""Cannot init a private_builtin:type_info/1"");
}
#endif


Define_entry(mercury____Unify___private_builtin__typeclass_info_1_0);
{
	MR_fatal_error(""attempt to unify typeclass_info"");
}

Define_entry(mercury____Compare___private_builtin__typeclass_info_1_0);
{
	MR_fatal_error(""attempt to compare typeclass_info"");
}

#ifdef MR_USE_SOLVE_EQUAL
Define_entry(mercury____SolveEqual___private_builtin__typeclass_info_1_0);
{
	MR_fatal_error(""attempt to compare typeclass_info"");
}
#endif

#ifdef MR_USE_INIT
Define_entry(mercury____Init___private_builtin__typeclass_info_1_0);
{
	MR_fatal_error(""attempt to init typeclass_info"");
}
#endif
END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_type_info_module
*/
MR_MODULE_STATIC_OR_EXTERN ModuleFunc type_info_module;
void sys_init_type_info_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_type_info_module(void) {
	type_info_module();

	MR_INIT_TYPE_CTOR_INFO(
	  mercury_data_private_builtin__type_ctor_info_type_ctor_info_1,
	  private_builtin__type_info_1_0);
	MR_INIT_TYPE_CTOR_INFO(
	  mercury_data_private_builtin__type_ctor_info_type_info_1,
	  private_builtin__type_info_1_0);
	MR_INIT_TYPE_CTOR_INFO(
	  mercury_data_private_builtin__type_ctor_info_base_typeclass_info_1,
	  private_builtin__typeclass_info_1_0);
	MR_INIT_TYPE_CTOR_INFO(
	  mercury_data_private_builtin__type_ctor_info_typeclass_info_1,
	  private_builtin__typeclass_info_1_0);

	MR_register_type_ctor_info(
	  &mercury_data_private_builtin__type_ctor_info_type_ctor_info_1);
	MR_register_type_ctor_info(
	  &mercury_data_private_builtin__type_ctor_info_type_info_1);
	MR_register_type_ctor_info(
	  &mercury_data_private_builtin__type_ctor_info_base_typeclass_info_1);
	MR_register_type_ctor_info(
	  &mercury_data_private_builtin__type_ctor_info_typeclass_info_1);
}

#endif /* ! MR_HIGHLEVEL_CODE */

").

:- pragma c_code(type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
	TypeInfo::out), [will_not_call_mercury, thread_safe],
"
	TypeInfo = MR_typeclass_info_type_info(TypeClassInfo, Index);
").

:- pragma c_code(unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
	Index::in, TypeInfo::out), [will_not_call_mercury, thread_safe],
"
	TypeInfo = MR_typeclass_info_unconstrained_type_info(TypeClassInfo,
			Index);
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
	% will happen if this is used in programs.
	% With the LLDS back-end, it has no definition,
	% since for efficiency the code generator treats it as a builtin.
	% With the MLDS back-end, it is defined in runtime/mercury.h.

:- pred unsafe_type_cast(T1, T2).
:- mode unsafe_type_cast(in, out) is det.

:- pred unused is det.

:- implementation.

:- external(unsafe_type_cast/2).

unused :-
	( semidet_succeed ->
		error("attempted use of dead predicate")
	;
		% the following is never executed
		true
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
