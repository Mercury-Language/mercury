%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: std_util.m.
% Main author: fjh.
% Stability: medium to high.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.
%
% It contains the predicates solutions/2, semidet_succeed/0, semidet_fail/0,
% and report_stats/0; the types univ, unit, maybe(T), pair(T1, T2);
% and some predicates which operate on those types.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.

:- interface.

:- import_module list, set.

%-----------------------------------------------------------------------------%

% The universal type.
% Note that the current NU-Prolog implementation of univ_to_type
% is buggy in that it always succeeds, even if the types didn't
% match, so until this gets implemented correctly, don't use
% univ_to_type unless you are sure that the types will definely match.

:- type univ.

:- pred type_to_univ(T, univ).
:- mode type_to_univ(di, uo) is det.
:- mode type_to_univ(in, out) is det.
:- mode type_to_univ(out, in) is semidet.

:- pred univ_to_type(univ, T).
:- mode univ_to_type(in, out) is semidet.
:- mode univ_to_type(out, in) is det.

%-----------------------------------------------------------------------------%

% The "maybe" type.

:- type maybe(T) ---> yes(T) ; no.

%-----------------------------------------------------------------------------%

% The "unit" type - stores no information at all.

:- type unit		--->	unit.

%-----------------------------------------------------------------------------%

% The "pair" type.  Useful for many purposes.

:- type pair(T1, T2)	--->	(T1 - T2).
:- type pair(T)		==	pair(T,T).

%-----------------------------------------------------------------------------%

% solutions/2 collects all the solutions to a predicate and
% returns them as a list in sorted order, with duplicates removed.
% solutions_set/2 returns them as a set.
% unsorted_solutions/2 returns them as an unsorted list with possible
% duplicates; since there are an infinite number of such lists,
% this must be called from a context in which only a single solution
% is required.

:- pred solutions(pred(T), list(T)).
:- mode solutions(pred(out) is multi, out) is det.
:- mode solutions(pred(out) is nondet, out) is det.

:- pred solutions_set(pred(T), set(T)).
:- mode solutions_set(pred(out) is multi, out) is det.
:- mode solutions_set(pred(out) is nondet, out) is det.

:- pred unsorted_solutions(pred(T), list(T)).
:- mode unsorted_solutions(pred(out) is multi, out) is cc_multi.
:- mode unsorted_solutions(pred(out) is nondet, out) is cc_multi.

%-----------------------------------------------------------------------------%

	% maybe_pred(Pred, X, Y) takes a closure Pred which transfroms an
	% input semideterministically. If calling the closure with the input
	% X succeeds, Y is bound to `yes(Z)' where Z is the output of the
	% call, or to `no' if the call fails.
:- pred maybe_pred(pred(T1, T2), T1, maybe(T2)).
:- mode maybe_pred(pred(in, out) is semidet, in, out) is det.

%-----------------------------------------------------------------------------%

% Declaratively, `report_stats' is the same as `true'.
% It has the side-effect of reporting some memory and time usage statistics
% to stdout.  (Technically, every Mercury implementation must offer
% a mode of invocation which disables this side-effect.)

:- pred report_stats is det.

%-----------------------------------------------------------------------------%

	% `semidet_succeed' is exactly the same as `true', except that
	% the compiler thinks that it is semi-deterministic.  You can
	% use calls to `semidet_succeed' to suppress warnings about
	% determinism declarations which could be stricter.
	% Similarly, `semidet_fail' is like `fail' except that its
	% determinism is semidet rather than failure, and
	% `cc_multi_equal(X,Y)' is the same as `X=Y' except that it
	% is cc_multi rather than det.

:- pred semidet_succeed is semidet.

:- pred semidet_fail is semidet.

:- pred cc_multi_equal(T::in, T::out) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, set.

/****
	Is this really useful?
% for use in lambda expressions where the type of functor '-' is ambiguous
:- pred pair(X, Y, pair(X, Y)).
:- mode pair(in, in, out) is det.
:- mode pair(out, out, in) is det.

pair(X, Y, X-Y).
****/

maybe_pred(Pred, X, Y) :-
	(
		call(Pred, X, Z)
	->
		Y = yes(Z)
	;
		Y = no
	).

:- pred builtin_solutions(pred(T), list(T)).
:- mode builtin_solutions(pred(out) is multi, out) is det.
:- mode builtin_solutions(pred(out) is nondet, out) is det.
:- external(builtin_solutions/2).
	% builtin_solutions is implemented in runtime/solutions.mod.

solutions(Pred, List) :-
	builtin_solutions(Pred, UnsortedList),
	list__sort_and_remove_dups(UnsortedList, List).

solutions_set(Pred, Set) :-
	builtin_solutions(Pred, List),
	set__list_to_set(List, Set).

unsorted_solutions(Pred, List) :-
	builtin_solutions(Pred, UnsortedList),
	cc_multi_equal(UnsortedList, List).

univ_to_type(Univ, X) :- type_to_univ(X, Univ).

%-----------------------------------------------------------------------------%

% semidet_succeed and semidet_fail, implemented using the C interface
% to make sure that the compiler doesn't issue any determinism warnings
% for them.

:- pragma(c_code, semidet_succeed, "SUCCESS_INDICATOR = TRUE;").
:- pragma(c_code, semidet_fail,    "SUCCESS_INDICATOR = FALSE;").
:- pragma(c_code, cc_multi_equal(X::in, Y::out), "Y = X;").

/*---------------------------------------------------------------------------*/

:- pragma(c_header_code, "

#include ""timing.h""

").

:- pragma(c_code, report_stats, "
	int	time_at_prev_stat;

	time_at_prev_stat = time_at_last_stat;
	time_at_last_stat = get_run_time();

	fprintf(stderr, 
		""[Time: +%.3fs, %.3fs, D Stack: %.3fk, ND Stack: %.3fk, "",
		(time_at_last_stat - time_at_prev_stat) / 1000.0,
		(time_at_last_stat - time_at_start) / 1000.0,
		((char *) sp - (char *) detstackmin) / 1024.0,
		((char *) maxfr - (char *) nondstackmin) / 1024.0
	);

#ifdef CONSERVATIVE_GC
	fprintf(stderr, 
		""#GCs: %lu,\\n""
		""Heap used since last GC: %.3fk, Total used: %.3fk]\\n"",
		(unsigned long) GC_gc_no,
		GC_get_bytes_since_gc() / 1024.0,
		GC_get_heap_size() / 1024.0
	);
#else
	fprintf(stderr, 
		""Heap: %.3fk]\\n"",
		((char *) hp - (char *) heapmin) / 1024.0
	);
#endif
").

/*---------------------------------------------------------------------------*/

:- pragma(c_header_code, "

#include ""type_info.h""

int	mercury_compare_type_info(Word type_info_1, Word type_info_2);

").

:- pragma(c_code, "

/*
** Compare two type_info structures, using an arbitrary ordering
** (based on the addresses of the unification predicates).
*/

int
mercury_compare_type_info(Word type_info_1, Word type_info_2)
{
	int	i, num_arg_types, comp;
	Word	unify_pred_1, unify_pred_2;

	/* First compare the addresses of the unify preds in the type_infos */
	unify_pred_1 = field(mktag(0), type_info_1, OFFSET_FOR_UNIFY_PRED);
	unify_pred_2 = field(mktag(0), type_info_2, OFFSET_FOR_UNIFY_PRED);
	if (unify_pred_1 < unify_pred_2) {
		return COMPARE_LESS;
	}
	if (unify_pred_1 > unify_pred_2) {
		return COMPARE_GREATER;
	}
	/*
	** If the addresses of the unify preds are equal, we don't need to
	** compare the arity of the types - they must be the same.
	** But we need to recursively compare the argument types, if any.
	*/
	num_arg_types = field(mktag(0), type_info_1, OFFSET_FOR_COUNT);
	for (i = 0; i < num_arg_types; i++) {
		Word arg_type_info_1 = field(mktag(0), type_info_1,
					OFFSET_FOR_ARG_TYPE_INFOS + i);
		Word arg_type_info_2 = field(mktag(0), type_info_2,
					OFFSET_FOR_ARG_TYPE_INFOS + i);
		comp = mercury_compare_type_info(
				arg_type_info_1, arg_type_info_2);
		if (comp != COMPARE_EQUAL) return comp;
	}
	return COMPARE_EQUAL;
}

").

:- pragma(c_header_code, "
/*
**	`univ' is represented as a two word structure.
**	The first word contains the address of a type_info for the type.
**	The second word contains the data.
*/

#define UNIV_OFFSET_FOR_TYPEINFO 0
#define UNIV_OFFSET_FOR_DATA 1

").

% :- pred type_to_univ(T, univ).
% :- mode type_to_univ(di, uo) is det.
% :- mode type_to_univ(in, out) is det.
% :- mode type_to_univ(out, in) is semidet.

	% Forward mode - convert from type to univ.
	% Allocate heap space, set the first field to contain the address
	% of the type_info for this type, and then store the input argument
	% in the second field.
:- pragma(c_code, type_to_univ(Type::di, Univ::uo), "
	incr_hp(Univ, 2);
	field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO) = TypeInfo_for_T;
	field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA) = Type;
").
:- pragma(c_code, type_to_univ(Type::in, Univ::out), "
	incr_hp(Univ, 2);
	field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO) = TypeInfo_for_T;
	field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA) = Type;
").

	% Backward mode - convert from univ to type.
	% We check that type_infos compare equal.
	% The variable `TypeInfo_for_T' used in the C code
	% is the compiler-introduced type-info variable.
:- pragma(c_code, type_to_univ(Type::out, Univ::in), "{
	Word univ_type_info = field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO);
	if (mercury_compare_type_info(univ_type_info, TypeInfo_for_T)
		== COMPARE_EQUAL)
	{
		Type = field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA);
		SUCCESS_INDICATOR = TRUE;
	} else {
		SUCCESS_INDICATOR = FALSE;
	}
}").

:- pragma(c_code, "

Define_extern_entry(mercury____Unify___std_util__univ_0_0);
Define_extern_entry(mercury____Index___std_util__univ_0_0);
Define_extern_entry(mercury____Compare___std_util__univ_0_0);
Declare_label(mercury____Compare___std_util__univ_0_0_i1);
Define_extern_entry(mercury____Term_To_Type___std_util__univ_0_0);
Define_extern_entry(mercury____Type_To_Term___std_util__univ_0_0);

BEGIN_MODULE(unify_univ_module)
	init_entry(mercury____Unify___std_util__univ_0_0);
	init_entry(mercury____Index___std_util__univ_0_0);
	init_entry(mercury____Compare___std_util__univ_0_0);
	init_label(mercury____Compare___std_util__univ_0_0_i1);
	init_entry(mercury____Term_To_Type___std_util__univ_0_0);
	init_entry(mercury____Type_To_Term___std_util__univ_0_0);
BEGIN_CODE
Define_entry(mercury____Unify___std_util__univ_0_0);
{
	/*
	** Unification for univ.
	**
	** The two inputs are in the registers named by unify_input[12].
	** The success/failure indication should go in unify_output.
	*/

	Word univ1, univ2;
	Word typeinfo1, typeinfo2;

	univ1 = unify_input1;
	univ2 = unify_input2;

	/* First check the type_infos compare equal */
	typeinfo1 = field(mktag(0), univ1, UNIV_OFFSET_FOR_TYPEINFO);
	typeinfo2 = field(mktag(0), univ2, UNIV_OFFSET_FOR_TYPEINFO);
	if (mercury_compare_type_info(typeinfo1, typeinfo2) != COMPARE_EQUAL)
	{
		unify_output = FALSE;
		proceed();
	}

	/*
	** Then invoke the generic unification predicate on the
	** unwrapped args
	*/
	mercury__unify__x = field(mktag(0), univ1, UNIV_OFFSET_FOR_DATA);
	mercury__unify__y = field(mktag(0), univ2, UNIV_OFFSET_FOR_DATA);
	mercury__unify__typeinfo = typeinfo1;
	{
		Declare_entry(mercury__unify_2_0);
		tailcall(ENTRY(mercury__unify_2_0),
			LABEL(mercury____Unify___std_util__univ_0_0));
	}
}

Define_entry(mercury____Index___std_util__univ_0_0);
	r2 = -1;
	proceed();

Define_entry(mercury____Compare___std_util__univ_0_0);
{
	/*
	** Comparison for univ:
	**
	** The two inputs are in the registers named by compare_input[12].
	** The result should go in compare_output.
	*/

	Word univ1, univ2;
	Word typeinfo1, typeinfo2;

	univ1 = compare_input1;
	univ2 = compare_input2;

	/* First compare the type_infos */
	typeinfo1 = field(mktag(0), univ1, UNIV_OFFSET_FOR_TYPEINFO);
	typeinfo2 = field(mktag(0), univ2, UNIV_OFFSET_FOR_TYPEINFO);
	compare_output = mercury_compare_type_info(typeinfo1, typeinfo2);
	if (compare_output != COMPARE_EQUAL) {
		proceed();
	}

	/*
	** If the types are the same, then invoke the generic compare/3
	** predicate on the unwrapped args.
	*/
#ifdef	COMPACT_ARGS
	r1 = typeinfo1;
	r3 = field(mktag(0), univ2, UNIV_OFFSET_FOR_DATA);
	r2 = field(mktag(0), univ1, UNIV_OFFSET_FOR_DATA);
	{
		Declare_entry(mercury__compare_3_0);
		tailcall(ENTRY(mercury__compare_3_0),
			LABEL(mercury____Compare___std_util__univ_0_0));
	}
#else
	r1 = typeinfo1;
	r4 = field(mktag(0), univ2, UNIV_OFFSET_FOR_DATA);
	r3 = field(mktag(0), univ1, UNIV_OFFSET_FOR_DATA);
	{
		Declare_entry(mercury__compare_3_0);
		call(ENTRY(mercury__compare_3_0),
			LABEL(mercury____Compare___std_util__univ_0_0_i1),
			LABEL(mercury____Compare___std_util__univ_0_0));
	}
#endif
}
Define_label(mercury____Compare___std_util__univ_0_0_i1);
#ifdef	COMPACT_ARGS
	fatal_error(""mercury____Compare___std_util__univ_0_0_i1 reached in COMPACT_ARGS mode"");
#else
	/* shuffle the return value into the right register */
	r1 = r2;
	proceed();
#endif

Define_entry(mercury____Term_To_Type___std_util__univ_0_0);
	/* don't know what to put here. */
	fatal_error(""cannot convert univ type to term"");

Define_entry(mercury____Type_To_Term___std_util__univ_0_0);
	/* don't know what to put here. */
	fatal_error(""cannot convert type univ to term"");

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_unify_univ_module
*/
void sys_init_unify_univ_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_unify_univ_module(void) {
	extern ModuleFunc unify_univ_module;
	unify_univ_module();
}

").

%-----------------------------------------------------------------------------%
