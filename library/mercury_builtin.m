%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: mercury_builtin.m.
% Main author: fjh.
% Stability: low.

% This file is automatically imported into every module.
% It is intended for things that are part of the language,
% but which are implemented just as normal user-level code
% rather than with special coding in the compiler.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mercury_builtin.
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

:- inst dead = clobbered.

% The not yet properly supported `any' inst used for the
% constraint solver interface is also builtin.

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

% copy/2 is used to make a `unique' copy of a data structure,
% so that you can use destructive update.
% At the moment it doesn't actually do any copying, since we
% haven't implemented destructive update yet and so there is no need.

:- pred copy(T, T).
:- mode copy(ui, uo) is det.
:- mode copy(in, uo) is det.

% We define !/0 (and !/2 for dcgs) to be equivalent to `true'.  This is for
% backwards compatibility with Prolog systems.  But of course it only works
% if all your cuts are green cuts.

:- pred ! is det.

:- pred !(T, T).
:- mode !(di, uo) is det.
:- mode !(in, out) is det.

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

	% unify(X, Y) is true iff X = Y.
:- pred unify(T::in, T::in) is semidet.

:- type comparison_result ---> (=) ; (<) ; (>).

	% compare(Res, X, Y) binds Res to =, <, or >
	% depending on whether X is =, <, or > Y in the
	% standard ordering.
:- pred compare(comparison_result, T, T).
:- mode compare(uo, ui, ui) is det.
:- mode compare(uo, ui, in) is det.
:- mode compare(uo, in, ui) is det.
:- mode compare(uo, in, in) is det.

%-----------------------------------------------------------------------------%

:- implementation.

% The things beyond this point are implementation details; they do
% not get included in the Mercury library library reference manual.

%-----------------------------------------------------------------------------%

:- interface.

% The following are used by the compiler, to implement polymorphism.
% They should not be used in programs.

	% index(X, N): if X is a discriminated union type, this is
	% true iff the top-level functor of X is the (N-1)th functor in its
	% type.  Otherwise, if X is a builtin type, N = -1, unless X is
	% of type int, in which case N = X.
:- pred index(T::in, int::out) is det.

:- pred builtin_unify_int(int::in, int::in) is semidet.
:- pred builtin_index_int(int::in, int::out) is det.
:- pred builtin_compare_int(comparison_result::out, int::in, int::in) is det.

:- pred builtin_unify_character(character::in, character::in) is semidet.
:- pred builtin_index_character(character::in, int::out) is det.
:- pred builtin_compare_character(comparison_result::out, character::in,
	character::in) is det.

:- pred builtin_unify_string(string::in, string::in) is semidet.
:- pred builtin_index_string(string::in, int::out) is det.
:- pred builtin_compare_string(comparison_result::out, string::in, string::in)
	is det.

:- pred builtin_unify_float(float::in, float::in) is semidet.
:- pred builtin_index_float(float::in, int::out) is det.
:- pred builtin_compare_float(comparison_result::out, float::in, float::in)
	is det.

:- pred builtin_unify_pred((pred)::in, (pred)::in) is semidet.
:- pred builtin_index_pred((pred)::in, int::out) is det.
:- pred builtin_compare_pred(comparison_result::out, (pred)::in, (pred)::in)
	is det.

:- pred unused is det.

	% compare_error is used in the code generated for compare/3 preds
:- pred compare_error is erroneous.

	% The code generated by polymorphism.m always requires
	% the existence of a type_info functor, and requires
	% the existence of a base_type_info functor as well
	% when using --type-info {shared-,}one-or-two-cell.
	%
	% The actual arities of these two function symbols are variable;
	% they depend on the number of type parameters of the type represented
	% by the type_info, and how many predicates we associate with each
	% type.

:- type type_info(T) ---> type_info(base_type_info(T) /*, ... */).
:- type base_type_info(T) ---> base_type_info(int /*, ... */).

	% the builtin < operator on ints, used in the code generated
	% for compare/3 preds
:- pred builtin_int_lt(int, int).
:- mode builtin_int_lt(in, in) is semidet.
:- external(builtin_int_lt/2).

	% the builtin > operator on ints, used in the code generated
	% for compare/3 preds
:- pred builtin_int_gt(int, int).
:- mode builtin_int_gt(in, in) is semidet.
:- external(builtin_int_gt/2).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, std_util, int, float, char, string, list.

% Many of the predicates defined in this module are builtin -
% the compiler generates code for them inline.

%-----------------------------------------------------------------------------%

!.
!(X, X).

%-----------------------------------------------------------------------------%

:- external(unify/2).
:- external(index/2).
:- external(compare/3).

%-----------------------------------------------------------------------------%

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

builtin_unify_float(F, F).

builtin_index_float(_, -1).

builtin_compare_float(R, F1, F2) :-
	( builtin_float_lt(F1, F2) ->
		R = (<)
	; builtin_float_gt(F1, F2) ->
		R = (>)
	;
		R = (=)
	).

:- pred builtin_strcmp(int, string, string).
:- mode builtin_strcmp(out, in, in) is det.

:- pragma(c_code, builtin_strcmp(Res::out, S1::in, S2::in),
	"Res = strcmp(S1, S2);").

builtin_unify_pred(_Pred1, _Pred2) :-
	% suppress determinism warning
	( semidet_succeed ->
		error("attempted unification of higher-order predicate terms")
	;
		semidet_fail
	).

builtin_index_pred(_, -1).

builtin_compare_pred(Res, _Pred1, _Pred2) :-
	% suppress determinism warning
	( semidet_succeed ->
		error("attempted comparison of higher-order predicate terms")
	;
		% the following is never executed
		Res = (<)
	).

unused :-
	( semidet_succeed ->
		error("attempted use of dead predicate")
	;
		% the following is never executed 
		true
	).

:- pragma(c_header_code, "#include ""type_info.h""").

:- pragma(c_code, "


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

	/* base_type_layout for `pred' */
	/* (this is used for all higher-order types) */

const struct mercury_data___base_type_layout_pred_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___base_type_layout_pred_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_PREDICATE_VALUE))
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

	/* base_type_functors for `pred' */
	/* (this is used for all higher-order types) */

const struct mercury_data___base_type_functors_pred_0_struct {
	Integer f1;
} mercury_data___base_type_functors_pred_0 = {
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
#ifdef USE_TYPE_TO_TERM
	Code *f5;
	Code *f6;
#endif
#ifdef USE_TYPE_LAYOUT
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___base_type_info_int_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_int_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_int_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_int_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_int_0,
	(const Word *) & mercury_data___base_type_functors_int_0,
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
#ifdef USE_TYPE_TO_TERM
	Code *f5;
	Code *f6;
#endif
#ifdef USE_TYPE_LAYOUT
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___base_type_info_character_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_character_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_character_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_character_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_character_0,
	(const Word *) & mercury_data___base_type_functors_character_0,
	(const Word *) string_const(""char"", 4)
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
#ifdef USE_TYPE_TO_TERM
	Code *f5;
	Code *f6;
#endif
#ifdef USE_TYPE_LAYOUT
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___base_type_info_string_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_string_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_string_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_string_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_string_0,
	(const Word *) & mercury_data___base_type_functors_string_0,
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
#ifdef USE_TYPE_TO_TERM
	Code *f5;
	Code *f6;
#endif
#ifdef USE_TYPE_LAYOUT
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___base_type_info_float_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_float_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_float_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_float_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_float_0,
	(const Word *) & mercury_data___base_type_functors_float_0,
	(const Word *) string_const(""float"", 5)
#endif
};

	/* base_type_info for `pred' */
	/* (this is used for all higher-order types) */

Declare_entry(mercury__builtin_unify_pred_2_0);
Declare_entry(mercury__builtin_index_pred_2_0);
Declare_entry(mercury__builtin_compare_pred_3_0);
MR_STATIC_CODE_CONST struct mercury_data___base_type_info_pred_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_TO_TERM
	Code *f5;
	Code *f6;
#endif
#ifdef USE_TYPE_LAYOUT
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___base_type_info_pred_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_pred_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_pred_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_pred_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_pred_0,
	(const Word *) & mercury_data___base_type_functors_pred_0,
	(const Word *) string_const(""pred"", 4)
#endif
};

	/* base_type_info for `void' */

Declare_entry(mercury__unused_0_0);
MR_STATIC_CODE_CONST struct mercury_data___base_type_info_void_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_TO_TERM
	Code *f5;
	Code *f6;
#endif
#ifdef USE_TYPE_LAYOUT
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___base_type_info_void_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
#ifdef USE_TYPE_TO_TERM
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0))
#endif
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_void_0,
	(const Word *) & mercury_data___base_type_functors_void_0,
	(const Word *) string_const(""void"", 4)
#endif
};

BEGIN_MODULE(builtin_types_module)

BEGIN_CODE

END_MODULE

/*
INIT sys_init_builtin_types_module
*/
void sys_init_builtin_types_module(void);
void sys_init_builtin_types_module(void) {
	extern ModuleFunc builtin_types_module;
	extern ModuleFunc mercury__mercury_builtin__init;

	builtin_types_module();

	/* 
	** We had better call this init() because we use the
	** labels for the special preds of int, float, pred, 
	** character and string. If they aren't initialized,
	** we might initialize the base_type_info with
	** garbage
	*/
	mercury__mercury_builtin__init();

	MR_INIT_BUILTIN_BASE_TYPE_INFO(
		mercury_data___base_type_info_int_0, _int_);
	MR_INIT_BUILTIN_BASE_TYPE_INFO(
		mercury_data___base_type_info_float_0, _float_);
	MR_INIT_BUILTIN_BASE_TYPE_INFO(
		mercury_data___base_type_info_pred_0, _pred_);
	MR_INIT_BUILTIN_BASE_TYPE_INFO(
		mercury_data___base_type_info_character_0, _character_);
	MR_INIT_BUILTIN_BASE_TYPE_INFO(
		mercury_data___base_type_info_string_0, _string_);
	MR_INIT_BASE_TYPE_INFO_WITH_PRED(
		mercury_data___base_type_info_void_0, mercury__unused_0_0);
}

").

	% This is used by the code that the compiler generates for compare/3.
compare_error :-
	error("internal error in compare/3").

%-----------------------------------------------------------------------------%

/* copy/2
	:- pred copy(T, T).
	:- mode copy(ui, uo) is det.
	:- mode copy(in, uo) is det.
*/

	% XXX note that this is *not* deep copy, and so it is unsafe!

/* This doesn't work, due to the lack of support for aliasing.
:- pragma(c_code, copy(X::ui, Y::uo), "Y = X;").
:- pragma(c_code, copy(X::in, Y::uo), "Y = X;").
*/

:- external(copy/2).
:- pragma(c_code, "
Define_extern_entry(mercury__copy_2_0);
Define_extern_entry(mercury__copy_2_1);

BEGIN_MODULE(copy_module)
	init_entry(mercury__copy_2_0);
	init_entry(mercury__copy_2_1);
BEGIN_CODE

Define_entry(mercury__copy_2_0);
Define_entry(mercury__copy_2_1);
#ifdef	COMPACT_ARGS
	r1 = r2;
#else
	r3 = r2;
#endif
	proceed();

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_copy_module
*/
void sys_init_copy_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_copy_module(void) {
	extern ModuleFunc copy_module;
	copy_module();
}

").

%-----------------------------------------------------------------------------%

% The type c_pointer can be used by predicates which use the C interface.

:- pragma(c_code, "

/*
 * c_pointer has a special value reserved for its layout, since it needs to
 * be handled as a special case.
 */

#ifdef  USE_TYPE_LAYOUT

const struct mercury_data_mercury_builtin__base_type_layout_c_pointer_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data_mercury_builtin__base_type_layout_c_pointer_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_C_POINTER_VALUE))
};

const struct
mercury_data_mercury_builtin__base_type_functors_c_pointer_0_struct {
	Integer f1;
} mercury_data_mercury_builtin__base_type_functors_c_pointer_0 = {
	MR_TYPEFUNCTORS_SPECIAL
};

#endif

Define_extern_entry(mercury____Unify___mercury_builtin__c_pointer_0_0);
Define_extern_entry(mercury____Index___mercury_builtin__c_pointer_0_0);
Define_extern_entry(mercury____Compare___mercury_builtin__c_pointer_0_0);

BEGIN_MODULE(unify_c_pointer_module)
	init_entry(mercury____Unify___mercury_builtin__c_pointer_0_0);
	init_entry(mercury____Index___mercury_builtin__c_pointer_0_0);
	init_entry(mercury____Compare___mercury_builtin__c_pointer_0_0);

BEGIN_CODE
Define_entry(mercury____Unify___mercury_builtin__c_pointer_0_0);
	/*
	** For c_pointer, we assume that equality and comparison
	** can be based on object identity (i.e. using address comparisons).
	** This is correct for types like io__stream, and necessary since
	** the io__state contains a map(io__stream, filename).
	** However, it might not be correct in general...
	*/
	unify_output = (unify_input1 == unify_input2);
	proceed();

Define_entry(mercury____Index___mercury_builtin__c_pointer_0_0);
	index_output = -1;
	proceed();

Define_entry(mercury____Compare___mercury_builtin__c_pointer_0_0);
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



:- end_module mercury_builtin.

%-----------------------------------------------------------------------------%
