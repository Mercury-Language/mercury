%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999 The University of Melbourne.
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
%
% Note that misuse of this predicate may lead to unsound results:
% if there is more than one reference to the data in question,
% i.e. it is not `unique', then the behaviour is undefined.
% (If you lie to the compiler, the compiler will get its revenge!)

:- pred unsafe_promise_unique(T, T).
:- mode unsafe_promise_unique(in, uo) is det.

%-----------------------------------------------------------------------------%

% A call to the function `promise_only_solution(Pred)' constitutes a
% promise on the part of the caller that `Pred' has at most one solution,
% i.e. that `not some [X1, X2] (Pred(X1), Pred(X2), X1 \= X2)'.
% `promise_only_solution(Pred)' presumes that this assumption is
% satisfied, and returns the X for which Pred(X) is true, if
% there is one.
%
% You can use `promise_only_solution' as a way of 
% introducing `cc_multi' or `cc_nondet' code inside a
% `det' or `semidet' procedure.
%
% Note that misuse of this function may lead to unsound results:
% if the assumption is not satisfied, the behaviour is undefined.
% (If you lie to the compiler, the compiler will get its revenge!)

:- func promise_only_solution(pred(T)) = T.
:- mode promise_only_solution(pred(out) is cc_multi) = out is det.
:- mode promise_only_solution(pred(out) is cc_nondet) = out is semidet.

%-----------------------------------------------------------------------------%


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

promise_only_solution(Pred) = OutVal :-
        call(cc_cast(Pred), OutVal).

:- func cc_cast(pred(T)) = pred(T).
:- mode cc_cast(pred(out) is cc_nondet) = out(pred(out) is semidet) is det.
:- mode cc_cast(pred(out) is cc_multi) = out(pred(out) is det) is det.

:- pragma c_code(cc_cast(X :: (pred(out) is cc_multi)) =
                        (Y :: out(pred(out) is det)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").
:- pragma c_code(cc_cast(X :: (pred(out) is cc_nondet)) =
                        (Y :: out(pred(out) is semidet)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").

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

	/* type_ctor_layout definitions */ 

	/* type_ctor_layout for `int' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_int_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_int_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_INT_VALUE))
};

	/* type_ctor_layout for `character' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_character_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_character_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_CHARACTER_VALUE))
};

	/* type_ctor_layout for `string' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_string_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_string_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_STRING_VALUE))
};

	/* type_ctor_layout for `float' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_float_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_float_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_FLOAT_VALUE))
};

	/* type_ctor_layout for `void' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_void_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_void_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_VOID_VALUE))
};

#ifdef	NATIVE_GC

	/* type_ctor_layout for `succip' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_succip_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_succip_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_SUCCIP_VALUE))
};

	/* type_ctor_layout for `hp' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_hp_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_hp_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_HP_VALUE))
};

	/* type_ctor_layout for `curfr' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_curfr_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_curfr_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_CURFR_VALUE))
};

	/* type_ctor_layout for `maxfr' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_maxfr_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_maxfr_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_MAXFR_VALUE))
};

	/* type_ctor_layout for `redofr' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_redofr_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_redofr_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_REDOFR_VALUE))
};

	/* type_ctor_layout for `redoip' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_layout_redoip_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___type_ctor_layout_redoip_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_REDOIP_VALUE))
};

#endif /* NATIVE_GC */

	/* type_ctor_functors definitions */

	/* type_ctor_functors for `int' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_int_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_int_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

	/* type_ctor_functors for `character' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_character_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_character_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

	/* type_ctor_functors for `string' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_string_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_string_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

	/* type_ctor_functors for `float' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_float_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_float_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

	/* type_ctor_functors for `void' */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_void_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_void_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

#ifdef	NATIVE_GC

	/* type_ctor_functors for `succip' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_succip_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_succip_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

	/* type_ctor_functors for `hp' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_hp_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_hp_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

	/* type_ctor_functors for `curfr' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_curfr_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_curfr_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

	/* type_ctor_functors for `maxfr' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_maxfr_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_maxfr_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

	/* type_ctor_functors for `redofr' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_redofr_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_redofr_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

	/* type_ctor_functors for `redoip' (only used by accurate gc) */

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data___type_ctor_functors_redoip_0_struct {
	Integer f1;
} mercury_data___type_ctor_functors_redoip_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

#endif /* NATIVE_GC */

#endif /* USE_TYPE_LAYOUT */

	/* type_ctor_infos definitions */

	/* type_ctor_info for `int' */

Declare_entry(mercury__builtin_unify_int_2_0);
Declare_entry(mercury__builtin_index_int_2_0);
Declare_entry(mercury__builtin_compare_int_3_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_int_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	Word f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_int_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_int_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_int_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_int_3_0)),
#ifdef  USE_TYPE_LAYOUT
	MR_TYPECTOR_REP_INT,
	(const Word *) & mercury_data___type_ctor_functors_int_0,
	(const Word *) & mercury_data___type_ctor_layout_int_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""int"", 3)
#endif
};

	/* type_ctor_info for `character' */

Declare_entry(mercury__builtin_unify_character_2_0);
Declare_entry(mercury__builtin_index_character_2_0);
Declare_entry(mercury__builtin_compare_character_3_0);
MR_STATIC_CODE_CONST struct 
mercury_data___type_ctor_info_character_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	Word f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_character_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_character_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_character_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_character_3_0)),
#ifdef  USE_TYPE_LAYOUT
	MR_TYPECTOR_REP_CHAR,
	(const Word *) & mercury_data___type_ctor_functors_character_0,
	(const Word *) & mercury_data___type_ctor_layout_character_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""character"", 9)
#endif
};

	/* type_ctor_info for `string' */

Declare_entry(mercury__builtin_unify_string_2_0);
Declare_entry(mercury__builtin_index_string_2_0);
Declare_entry(mercury__builtin_compare_string_3_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_string_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	Word f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_string_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_string_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_string_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_string_3_0)),
#ifdef  USE_TYPE_LAYOUT
	MR_TYPECTOR_REP_STRING,
	(const Word *) & mercury_data___type_ctor_functors_string_0,
	(const Word *) & mercury_data___type_ctor_layout_string_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""string"", 6)
#endif
};

	/* type_ctor_info for `float' */

Declare_entry(mercury__builtin_unify_float_2_0);
Declare_entry(mercury__builtin_index_float_2_0);
Declare_entry(mercury__builtin_compare_float_3_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_float_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	Word f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_float_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_float_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_float_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_float_3_0)),
#ifdef  USE_TYPE_LAYOUT
	MR_TYPECTOR_REP_FLOAT,
	(const Word *) & mercury_data___type_ctor_functors_float_0,
	(const Word *) & mercury_data___type_ctor_layout_float_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""float"", 5)
#endif
};

	/* type_ctor_info for `void' */

Declare_entry(mercury__unused_0_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_void_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	Word f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_void_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
#ifdef  USE_TYPE_LAYOUT
	MR_TYPECTOR_REP_VOID,
	(const Word *) & mercury_data___type_ctor_functors_void_0,
	(const Word *) & mercury_data___type_ctor_layout_void_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""void"", 4)
#endif
};

#ifdef	NATIVE_GC

	/* type_ctor_info for `succip' (only used by accurate gc) */

Declare_entry(mercury__unused_0_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_succip_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_succip_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___type_ctor_layout_succip_0,
	(const Word *) & mercury_data___type_ctor_functors_succip_0,
	(const Word *) & mercury_data___type_ctor_layout_succip_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""succip"", 6)
#endif
};

	/* type_ctor_info for `hp' (only used by accurate gc) */

Declare_entry(mercury__unused_0_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_hp_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_hp_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___type_ctor_layout_hp_0,
	(const Word *) & mercury_data___type_ctor_functors_hp_0,
	(const Word *) & mercury_data___type_ctor_layout_hp_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""hp"", 2)
#endif
};

	/* type_ctor_info for `curfr' (only used by accurate gc) */

Declare_entry(mercury__unused_0_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_curfr_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_curfr_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___type_ctor_layout_curfr_0,
	(const Word *) & mercury_data___type_ctor_functors_curfr_0,
	(const Word *) & mercury_data___type_ctor_layout_curfr_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""curfr"", 5)
#endif
};

	/* type_ctor_info for `maxfr' (only used by accurate gc) */

Declare_entry(mercury__unused_0_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_maxfr_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_maxfr_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___type_ctor_layout_maxfr_0,
	(const Word *) & mercury_data___type_ctor_functors_maxfr_0,
	(const Word *) & mercury_data___type_ctor_layout_maxfr_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""maxfr"", 5)
#endif
};

	/* type_ctor_info for `redoip' (only used by accurate gc) */

Declare_entry(mercury__unused_0_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_redoip_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_redoip_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___type_ctor_layout_redoip_0,
	(const Word *) & mercury_data___type_ctor_functors_redoip_0,
	(const Word *) & mercury_data___type_ctor_layout_redoip_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""redoip"", 6)
#endif
};

	/* type_ctor_info for `redofr' (only used by accurate gc) */

Declare_entry(mercury__unused_0_0);
MR_STATIC_CODE_CONST struct mercury_data___type_ctor_info_redofr_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_LAYOUT
	const Word *f5;
	const Word *f6;
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___type_ctor_info_redofr_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__unused_0_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___type_ctor_layout_redofr_0,
	(const Word *) & mercury_data___type_ctor_functors_redofr_0,
	(const Word *) & mercury_data___type_ctor_layout_redofr_0,
	(const Word *) string_const(""builtin"", 7),
	(const Word *) string_const(""redofr"", 6)
#endif
};

#endif /* NATIVE_GC */

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
	** we might initialize the type_ctor_info with
	** garbage
	*/
	mercury__private_builtin__init();

	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_int_0, _int_);
	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_float_0, _float_);
	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_character_0, _character_);
	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_string_0, _string_);
	MR_INIT_TYPE_CTOR_INFO_WITH_PRED(
		mercury_data___type_ctor_info_void_0, mercury__unused_0_0);
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

	r1 = copy;
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

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data_builtin__type_ctor_layout_c_pointer_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data_builtin__type_ctor_layout_c_pointer_0 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		mkbody(MR_TYPE_CTOR_LAYOUT_C_POINTER_VALUE))
};

MR_MODULE_STATIC_OR_EXTERN
const struct
mercury_data_builtin__type_ctor_functors_c_pointer_0_struct {
	Integer f1;
} mercury_data_builtin__type_ctor_functors_c_pointer_0 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

Define_extern_entry(mercury____Unify___builtin__c_pointer_0_0);
Define_extern_entry(mercury____Index___builtin__c_pointer_0_0);
Define_extern_entry(mercury____Compare___builtin__c_pointer_0_0);

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
	r1 = (r1 == r2);
	proceed();

Define_entry(mercury____Index___builtin__c_pointer_0_0);
	r1 = -1;
	proceed();

Define_entry(mercury____Compare___builtin__c_pointer_0_0);
	r1 = (r1 == r2 ? COMPARE_EQUAL :
			  r1 < r2 ? COMPARE_LESS :
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
