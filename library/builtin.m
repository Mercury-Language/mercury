%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: builtin.m.
% Main author: fjh.
% Stability: low.

% This file is automatically imported into every module.
% It is intended for things that are part of the language,
% but which are implemented just as normal user-level code
% rather than with special coding in the compiler.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module builtin.
:- interface.

%-----------------------------------------------------------------------------%

% TYPES.

% The types `character', `int', `float', and `string',
% and the types `pred', `pred(T)', `pred(T1, T2)', `pred(T1, T2, T3)', ...
% and `func(T1) = T2', `func(T1, T2) = T3', `func(T1, T2, T3) = T4', ...
% are builtin and are implemented using special code in the
% type-checker.  (XXX TODO: report an error for attempts to redefine
% these types.)

% The type c_pointer can be used by predicates which use the C interface.
:- type c_pointer.

%-----------------------------------------------------------------------------%

% INSTS.

% The standard insts `free', `ground', and `bound(...)' are builtin
% and are implemented using special code in the parser and mode-checker.

% So are the standard unique insts `unique', `unique(...)',
% `mostly_unique', `mostly_unique(...)', and `clobbered'.
% The name `dead' is allowed as a synonym for `clobbered'.
% Similarly `mostly_dead' is a synonym for `mostly_clobbered'.

:- inst dead = clobbered.
:- inst mostly_dead = mostly_clobbered.

% The `any' inst used for the constraint solver interface is also builtin.

% Higher-order predicate insts `pred(<modes>) is <detism>'
% and higher-order functions insts `func(<modes>) = <mode> is det'
% are also builtin.

%-----------------------------------------------------------------------------%

% MODES.

% The standard modes.

:- mode unused :: (free -> free).
:- mode output :: (free -> ground).
:- mode input :: (ground -> ground).

:- mode in :: (ground -> ground).
:- mode out :: (free -> ground).

:- mode in(Inst) :: (Inst -> Inst).
:- mode out(Inst) :: (free -> Inst).
:- mode di(Inst) :: (Inst -> clobbered).
:- mode mdi(Inst) :: (Inst -> mostly_clobbered).

% Unique modes.  These are still not fully implemented.

% unique output
:- mode uo :: free -> unique.

% unique input
:- mode ui :: unique -> unique.

% destructive input
:- mode di :: unique -> clobbered.

% "Mostly" unique modes (unique except that that may be referenced
% again on backtracking).

% mostly unique output
:- mode muo :: free -> mostly_unique.

% mostly unique input
:- mode mui :: mostly_unique -> mostly_unique.

% mostly destructive input
:- mode mdi :: mostly_unique -> mostly_clobbered.

% Higher-order predicate modes are builtin.

%-----------------------------------------------------------------------------%

% PREDICATES.

% Most of these probably ought to be moved to another
% module in the standard library such as std_util.m.

% copy/2 makes a deep copy of a data structure.  The resulting copy is a
% `unique' value, so you can use destructive update on it.

:- pred copy(T, T).
:- mode copy(ui, uo) is det.
:- mode copy(in, uo) is det.

% unsafe_promise_unique/2 is used to promise the compiler that you have a
% `unique' copy of a data structure, so that you can use destructive update.
% It is used to work around limitations in the current support for unique
% modes.  `unsafe_promise_unique(X, Y)' is the same as `Y = X' except that
% the compiler will assume that `Y' is unique.

:- pred unsafe_promise_unique(T, T).
:- mode unsafe_promise_unique(in, uo) is det.

% We define !/0 (and !/2 for dcgs) to be equivalent to `true'.  This is for
% backwards compatibility with Prolog systems.  But of course it only works
% if all your cuts are green cuts.

:- pred ! is det.

:- pred !(T, T).
:- mode !(di, uo) is det.
:- mode !(in, out) is det.

%-----------------------------------------------------------------------------%

	% unify(X, Y) is true iff X = Y.
:- pred unify(T::in, T::in) is semidet.

:- type comparison_result ---> (=) ; (<) ; (>).

	% compare(Res, X, Y) binds Res to =, <, or >
	% depending on wheither X is =, <, or > Y in the
	% standard ordering.
:- pred compare(comparison_result, T, T).
:- mode compare(uo, ui, ui) is det.
:- mode compare(uo, ui, in) is det.
:- mode compare(uo, in, ui) is det.
:- mode compare(uo, in, in) is det.

	% index(X, N): if X is a discriminated union type, this is
	% true iff the top-level functor of X is the (N-1)th functor in its
	% type.  If X is of type int, then it is true iff N = X.
	% Otherwise, it is true iff N = -1.
:- pred index(T::in, int::out) is det.

% In addition, the following predicate-like constructs are builtin:
%
%	:- pred (T = T).
%	:- pred (T \= T).
%	:- pred (pred , pred).
%	:- pred (pred ; pred).
%	:- pred (\+ pred).
%	:- pred (not pred).
%	:- pred (pred -> pred).
%	:- pred (if pred then pred).
%	:- pred (if pred then pred else pred).
%	:- pred (pred => pred).
%	:- pred (pred <= pred).
%	:- pred (pred <=> pred).
%
%	(pred -> pred ; pred).
%	some Vars pred
%	all Vars pred
%	call/N

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, string, std_util, int, float, char, string, list.

%-----------------------------------------------------------------------------%

!.
!(X, X).

%-----------------------------------------------------------------------------%

:- external(unify/2).
:- external(index/2).
:- external(compare/3).

%-----------------------------------------------------------------------------%

:- pragma c_header_code("#include ""mercury_type_info.h""").

:- pragma c_code("


#ifdef  USE_TYPE_LAYOUT

	/* base_type_layout definitions */ 

	/* base_type_layout for `int' */

const struct mercury_data___base_type_layout_int_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___base_type_layout_int_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_INT_VALUE))
};

	/* base_type_layout for `character' */

const struct mercury_data___base_type_layout_character_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___base_type_layout_character_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_CHARACTER_VALUE))
};

	/* base_type_layout for `string' */

const struct mercury_data___base_type_layout_string_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___base_type_layout_string_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_STRING_VALUE))
};

	/* base_type_layout for `float' */

const struct mercury_data___base_type_layout_float_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___base_type_layout_float_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_FLOAT_VALUE))
};

	/* base_type_layout for `void' */

const struct mercury_data___base_type_layout_void_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___base_type_layout_void_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_VOID_VALUE))
};

	/* base_type_functors definitions */

	/* base_type_functors for `int' */

const struct mercury_data___base_type_functors_int_0_struct {
	Integer f1;
} mercury_data___base_type_functors_int_0 = {
	MR_TYPEFUNCTORS_SPECIAL
};

	/* base_type_functors for `character' */

const struct mercury_data___base_type_functors_character_0_struct {
	Integer f1;
} mercury_data___base_type_functors_character_0 = {
	MR_TYPEFUNCTORS_SPECIAL
};

	/* base_type_functors for `string' */

const struct mercury_data___base_type_functors_string_0_struct {
	Integer f1;
} mercury_data___base_type_functors_string_0 = {
	MR_TYPEFUNCTORS_SPECIAL
};

	/* base_type_functors for `float' */

const struct mercury_data___base_type_functors_float_0_struct {
	Integer f1;
} mercury_data___base_type_functors_float_0 = {
	MR_TYPEFUNCTORS_SPECIAL
};

	/* base_type_functors for `void' */

const struct mercury_data___base_type_functors_void_0_struct {
	Integer f1;
} mercury_data___base_type_functors_void_0 = {
	MR_TYPEFUNCTORS_SPECIAL
};

#endif /* USE_TYPE_LAYOUT */

	/* base_type_infos definitions */

	/* base_type_info for `int' */

Declare_entry(mercury__builtin_unify_int_2_0);
Declare_entry(mercury__builtin_index_int_2_0);
Declare_entry(mercury__builtin_compare_int_3_0);
MR_STATIC_CODE_CONST struct mercury_data___base_type_info_int_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
#endif
} mercury_data___base_type_info_int_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_int_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_int_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_int_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_int_0,
	(const Word *) & mercury_data___base_type_functors_int_0,
	(const Word *) string_const(""builtin"", 15),
	(const Word *) string_const(""int"", 3)
#endif
};

	/* base_type_info for `character' */

Declare_entry(mercury__builtin_unify_character_2_0);
Declare_entry(mercury__builtin_index_character_2_0);
Declare_entry(mercury__builtin_compare_character_3_0);
MR_STATIC_CODE_CONST struct 
mercury_data___base_type_info_character_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
#endif
} mercury_data___base_type_info_character_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_character_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_character_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_character_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_character_0,
	(const Word *) & mercury_data___base_type_functors_character_0,
	(const Word *) string_const(""builtin"", 15),
	(const Word *) string_const(""character"", 9)
#endif
};

	/* base_type_info for `string' */

Declare_entry(mercury__builtin_unify_string_2_0);
Declare_entry(mercury__builtin_index_string_2_0);
Declare_entry(mercury__builtin_compare_string_3_0);
MR_STATIC_CODE_CONST struct mercury_data___base_type_info_string_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
#endif
} mercury_data___base_type_info_string_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_string_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_string_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_string_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_string_0,
	(const Word *) & mercury_data___base_type_functors_string_0,
	(const Word *) string_const(""builtin"", 15),
	(const Word *) string_const(""string"", 6)
#endif
};

	/* base_type_info for `float' */

Declare_entry(mercury__builtin_unify_float_2_0);
Declare_entry(mercury__builtin_index_float_2_0);
Declare_entry(mercury__builtin_compare_float_3_0);
MR_STATIC_CODE_CONST struct mercury_data___base_type_info_float_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
#endif
} mercury_data___base_type_info_float_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_float_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_float_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_float_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_float_0,
	(const Word *) & mercury_data___base_type_functors_float_0,
	(const Word *) string_const(""builtin"", 15),
	(const Word *) string_const(""float"", 5)
#endif
};

	/* base_type_info for `void' */

Declare_entry(mercury__unused_0_0);
MR_STATIC_CODE_CONST struct mercury_data___base_type_info_void_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
#endif
} mercury_data___base_type_info_void_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_void_0,
	(const Word *) & mercury_data___base_type_functors_void_0,
	(const Word *) string_const(""builtin"", 15),
	(const Word *) string_const(""void"", 4)
#endif
};

BEGIN_MODULE(builtin_types_module)

BEGIN_CODE

END_MODULE

/*
INIT sys_init_builtin_types_module
*/
extern ModuleFunc builtin_types_module;
extern void mercury__private_builtin__init(void);
void sys_init_builtin_types_module(void);
void sys_init_builtin_types_module(void) {

	builtin_types_module();

	/* 
	** We had better call this init() because we use the
	** labels for the special preds of int, float, pred, 
	** character and string. If they aren't initialized,
	** we might initialize the base_type_info with
	** garbage
	*/
	mercury__private_builtin__init();

	MR_INIT_BUILTIN_BASE_TYPE_INFO(
		mercury_data___base_type_info_int_0, _int_);
	MR_INIT_BUILTIN_BASE_TYPE_INFO(
		mercury_data___base_type_info_float_0, _float_);
	MR_INIT_BUILTIN_BASE_TYPE_INFO(
		mercury_data___base_type_info_character_0, _character_);
	MR_INIT_BUILTIN_BASE_TYPE_INFO(
		mercury_data___base_type_info_string_0, _string_);
	MR_INIT_BASE_TYPE_INFO_WITH_PRED(
		mercury_data___base_type_info_void_0, mercury__unused_0_0);
}

").

%-----------------------------------------------------------------------------%

% unsafe_promise_unique/2 is a compiler builtin.

%-----------------------------------------------------------------------------%

/* copy/2
	:- pred copy(T, T).
	:- mode copy(ui, uo) is det.
	:- mode copy(in, uo) is det.
*/

/*************
Using `pragma c_code' doesn't work, due to the lack of support for
aliasing, and in particular the lack of support for `ui' modes.
:- pragma c_code(copy(Value::ui, Copy::uo), "
	save_transient_registers();
	Copy = deep_copy(&Value, TypeInfo_for_T, NULL, NULL);
	restore_transient_registers();
").
:- pragma c_code(copy(Value::in, Copy::uo), "
	save_transient_registers();
	Copy = deep_copy(&Value, TypeInfo_for_T, NULL, NULL);
	restore_transient_registers();
").
*************/

:- external(copy/2).

:- pragma c_header_code("#include ""mercury_deep_copy.h""").

:- pragma c_code("
Define_extern_entry(mercury__copy_2_0);
Define_extern_entry(mercury__copy_2_1);
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__copy_2_0);
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__copy_2_1);

BEGIN_MODULE(copy_module)
	init_entry(mercury__copy_2_0);
	init_entry(mercury__copy_2_1);
BEGIN_CODE

#ifdef PROFILE_CALLS
  #define fallthru(target, caller) { tailcall((target), (caller)); }
#else
  #define fallthru(target, caller)
#endif

Define_entry(mercury__copy_2_0);
fallthru(ENTRY(mercury__copy_2_1), ENTRY(mercury__copy_2_0))
Define_entry(mercury__copy_2_1);
{
	Word value, copy, type_info;

	type_info = r1;
	value = r2;

	save_transient_registers();
	copy = deep_copy(&value, (Word *) type_info, NULL, NULL);
	restore_transient_registers();

#ifdef	COMPACT_ARGS
	r1 = copy;
#else
	r3 = copy;
#endif

	proceed();
}
END_MODULE

/* Ensure that the initialization code for the above module gets run. */

/*
INIT sys_init_copy_module
*/
extern ModuleFunc copy_module;
void sys_init_copy_module(void);
	/* extra declaration to suppress gcc -Wmissing-decl warning */
void sys_init_copy_module(void) {
	copy_module();
}

").

%-----------------------------------------------------------------------------%

% The type c_pointer can be used by predicates which use the C interface.

:- pragma c_code("

/*
 * c_pointer has a special value reserved for its layout, since it needs to
 * be handled as a special case.
 */

#ifdef  USE_TYPE_LAYOUT

const struct mercury_data_builtin__base_type_layout_c_pointer_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data_builtin__base_type_layout_c_pointer_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_C_POINTER_VALUE))
};

const struct
mercury_data_builtin__base_type_functors_c_pointer_0_struct {
	Integer f1;
} mercury_data_builtin__base_type_functors_c_pointer_0 = {
	MR_TYPEFUNCTORS_SPECIAL
};

#endif

Define_extern_entry(mercury____Unify___builtin__c_pointer_0_0);
Define_extern_entry(mercury____Index___builtin__c_pointer_0_0);
Define_extern_entry(mercury____Compare___builtin__c_pointer_0_0);
MR_MAKE_STACK_LAYOUT_ENTRY(mercury____Unify___builtin__c_pointer_0_0);
MR_MAKE_STACK_LAYOUT_ENTRY(mercury____Index___builtin__c_pointer_0_0);
MR_MAKE_STACK_LAYOUT_ENTRY(mercury____Compare___builtin__c_pointer_0_0);

BEGIN_MODULE(unify_c_pointer_module)
	init_entry(mercury____Unify___builtin__c_pointer_0_0);
	init_entry(mercury____Index___builtin__c_pointer_0_0);
	init_entry(mercury____Compare___builtin__c_pointer_0_0);

BEGIN_CODE
Define_entry(mercury____Unify___builtin__c_pointer_0_0);
	/*
	** For c_pointer, we assume that equality and comparison
	** can be based on object identity (i.e. using address comparisons).
	** This is correct for types like io__stream, and necessary since
	** the io__state contains a map(io__stream, filename).
	** However, it might not be correct in general...
	*/
	unify_output = (unify_input1 == unify_input2);
	proceed();

Define_entry(mercury____Index___builtin__c_pointer_0_0);
	index_output = -1;
	proceed();

Define_entry(mercury____Compare___builtin__c_pointer_0_0);
	compare_output = (compare_input1 == compare_input2 ? COMPARE_EQUAL :
			  compare_input1 < compare_input2 ? COMPARE_LESS :
			  COMPARE_GREATER);
	proceed();

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_unify_c_pointer_module
*/
extern ModuleFunc unify_c_pointer_module;
void sys_init_unify_c_pointer_module(void);
	/* duplicate declaration to suppress gcc -Wmissing-decl warning */
void sys_init_unify_c_pointer_module(void) {
	unify_c_pointer_module();
}

").

:- end_module builtin.

%-----------------------------------------------------------------------------%
