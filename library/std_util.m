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
% It contains the predicates solutions/2, semidet_succeed/0,
% semidet_fail/0, functor/3, arg/3, det_arg/3, expand/4; the types univ,
% unit, maybe(T), pair(T1, T2); and some predicates which operate on
% those types.

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

	% maybe_pred(Pred, X, Y) takes a closure Pred which transforms an
	% input semideterministically. If calling the closure with the input
	% X succeeds, Y is bound to `yes(Z)' where Z is the output of the
	% call, or to `no' if the call fails.
:- pred maybe_pred(pred(T1, T2), T1, maybe(T2)).
:- mode maybe_pred(pred(in, out) is semidet, in, out) is det.

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

       % The `type_info' type - allows access to type information.
       %
       % A type_info represents the type of a variable.
       % 
       % It is possible for the type of a variable to be an unbound type
       % variable, this is represented as the type 'void'/0. 'void' is
       % considered a special (builtin) type - it is not a discriminated
       % union, so get_functor/5 and the function construct/3 will
       % fail if used upon this type.

:- type type_info.

	% type_info(Data) returns the type_info of the type of Data.

:- func type_of(T) = type_info.
:- mode type_of(unused) = out is det.

	% num_functors(TypeInfo) 
	% 
	% Returns the number of different functors for the type
	% specified by TypeInfo, or -1 if the type is not a
	% discriminated union type.

:- func num_functors(type_info) = int.
:- mode num_functors(in) = out is det.

	% get_functor(Var, N, Functor, Arity, ArgTypes)
	%
	% Binds Functor and Arity to the name and arity of the Nth
	% functor for the specified type (starting at zero), and binds
	% ArgTypes to the type_infos for the types of the arguments of
	% that functor.  Fails if the type is not a discriminated union
	% type, or if N is out of range.

:- pred get_functor(type_info::in, int::in, string::out, int::out,
		list(type_info)::out) is semidet.

	% construct(TypeInfo, N, Args) = Term
	%
	% Returns a term of the type specified by TypeInfo whose functor
	% is the Nth functor of TypeInfo (starting at zero), and whose
	% arguments are given by Args.  Fails if the type is not a
	% discriminated union type, or if N is out of range, or if the
	% number of arguments doesn't match the arity of the Nth functor
	% of the type, or if the types of the arguments doesn't match
	% the expected argument types for that functor.

:- func construct(type_info::in, int::in, list(univ)::in) = (univ::out)
		is semidet.

%-----------------------------------------------------------------------------%

	% functor, argument and expand take any type (including univ),
	% and return representation information for that type.
	%
	% The string representation of the functor that `functor' and 
	% `expand' return is:
	% 	- for user defined types, the functor that is given
	% 	  in the type defintion. For lists, this
	% 	  means the functors ./2 and []/0 are used, even if
	% 	  the list uses the [....] shorthand.
	%	- for integers, the string is a base 10 number,
	%	  positive integers have no sign.
	%	- for floats, the string is a floating point,
	%	  base 10 number, positive floating point numbers have
	%	  no sign. 
	%	- for strings, the string, inside double quotation marks
	%	- for characters, the character inside single 
	%	  quotation marks
	%	- for predicates and functions, the string
	%	  <<predicate>>

	% functor(Data, Functor, Arity)
	% 
	% Given a data item (Data), binds Functor to a string
	% representation of the functor and Arity to the arity of this
	% data item.

:- pred functor(T::in, string::out, int::out) is det.

	% argument(Data, ArgumentIndex, Argument)
	% 
	% Given a data item (Data) and an argument index
	% (ArgumentIndex), starting at 0 for the first argument, binds
	% Argument to that argument of the functor of the data item. If
	% the argument index is out of range -- that is, greater than or
	% equal to the arity of the functor or lower than 0 -- argument/3
	% fails.  The argument has the type univ. 

:- pred argument(T::in, int::in, univ::out) is semidet.

	% det_argument(ArgumentIndex, Data, Argument)
	% 
	% Given a data item (Data) and an argument index
	% (ArgumentIndex), starting at 0 for the first argument, binds
	% Argument to that argument of the functor of the data item. If
	% the argument index is out of range -- that is, greater than or
	% equal to the arity of the functor or lower than 0 --
	% det_argument/3 aborts. 

:- pred det_argument(T::in, int::in, univ::out) is det.

	% arg(ArgumentIndex, Data, Argument) 
	% 
	% Given a data item (Data) and an argument index
	% (ArgumentIndex), starting at 1 for the first argument, binds
	% Argument to that argument of the functor of the data item. If
	% the argument index is out of range -- that is, higher than the
	% arity of the functor or lower than 1 -- arg/3 fails.  The
	% argument has the type univ. 
	%
	% NOTE: `arg' is provided for Prolog compatability - the order
	% of parameters, and first argument number in `arg' are
	% different from `argument'.

:- pred arg(int::in, T::in, univ::out) is semidet.

	% det_arg(ArgumentIndex, Data, Argument) 
	% 
	% Given a data item (Data) and an argument index
	% (ArgumentIndex), starting at 1 for the first argument, binds
	% Argument to that argument of the functor of the data item. If
	% the argument index is out of range -- that is, higher than the
	% arity of the functor or lower than 1 -- det_arg/3 aborts. 

:- pred det_arg(int::in, T::in, univ::out) is det.

	% expand(Data, Functor, Arity, Arguments) 
	% 
	% Given a data item (Data), binds Functor to a string
	% representation of the functor, Arity to the arity of this data
	% item, and Arguments to a list of arguments of the functor.
	% The arguments in the list are each of type univ.

:- pred expand(T::in, string::out, int::out, list(univ)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, set, int.

:- type type_info == c_pointer.

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
	% builtin_solutions is implemented in c_code.

:- pragma c_code("

/*
** file: solutions.mod
** authors: conway, fjh.
**
** this module defines solutions/2 which takes a closure of type
** pred(T) in which the remaining argument is output.
*/

#include ""imp.h""
#include ""deep_copy.h""

MR_DECLARE_STRUCT(
	mercury_data_mercury_builtin__base_type_info_list_1);

Declare_entry(do_call_nondet_closure);

Define_extern_entry(mercury__std_util__builtin_solutions_2_0);
Define_extern_entry(mercury__std_util__builtin_solutions_2_1);
Declare_label(mercury__std_util__builtin_solutions_2_0_i1);
Declare_label(mercury__std_util__builtin_solutions_2_0_i2);

BEGIN_MODULE(solutions_module)
	init_entry(mercury__std_util__builtin_solutions_2_0);
	init_entry(mercury__std_util__builtin_solutions_2_1);
	init_label(mercury__std_util__builtin_solutions_2_0_i1);
	init_label(mercury__std_util__builtin_solutions_2_0_i2);
BEGIN_CODE

/*
** :- pred builtin_solutions(pred(T), list(T)).
** :- mode builtin_solutions(pred(out) is multi/nondet, out) is det.
**
** Polymorphism will add an extra input parameter, a type_info for T,
** which we don't use at the moment (later it could be used to find
** the address of the deep copy routine).
**
** The type_info structure will be in r1 and the closure will be in r2
** with both caling conventions. The output should go either in r3
** (for the normal parameter convention) or r1 (for the compact parameter
** convention).
*/

#ifdef	COMPACT_ARGS
  #define solutions_output_reg	r1
#else
  #define solutions_output_reg	r3
#endif

Define_entry(mercury__std_util__builtin_solutions_2_0);
#ifdef PROFILE_CALLS
{
	tailcall(ENTRY(mercury__std_util__builtin_solutions_2_1),
			LABEL(mercury__std_util__builtin_solutions_2_0));
}
#endif
Define_entry(mercury__std_util__builtin_solutions_2_1);

#ifndef CONSERVATIVE_GC

#ifndef USE_TYPE_LAYOUT
	fatal_error(""`solutions' not supported with this grade on this ""
		    ""system.\n""
		""Try using a `.gc' (conservative gc) grade.\n"");
#endif

/*
** The following algorithm uses a `solutions heap', and will work with
** non-conservative gc. We create a solution, on the normal heap, then
** copy it to the solutions heap, a part of a solutions list. This list
** list then copied back to the mercury heap.
**
** An improvement to this is that we can copy each solution to the
** solutions heap, but have deep_copy add an offset to the pointers
** (at least, those that would otherwise point to the solutions heap),
** so that, when finished, a block move of the solutions heap back to the
** real heap will leave all the pointers in the correct place.
*/


/*
** Define some framevars we will be using - we need to keep the
** value of hp and the solutions hp (solhp) before we entered 
** solutions, so we can reset the hp after each solution, and
** reset the solhp after all solutions have been found.
** To do a deep copy, we need the type_info of the type of a solution,
** so we save the type_info in type_info_fv.
** Finally, we store the list of solutions so far in list_fv.
*/

#define saved_hp_fv	(framevar(0))
#define saved_solhp_fv	(framevar(1))
#define type_info_fv	(framevar(2))
#define list_fv		(framevar(3))

	/* create a nondet stack frame with four slots,
	   and set the failure continuation */
	
	mkframe(""builtin_solutions"", 4,
		LABEL(mercury__std_util__builtin_solutions_2_0_i2));

	/* setup the framevars */
	saved_solhp_fv = (Word) solutions_heap_pointer; 
	mark_hp(saved_hp_fv);
	type_info_fv = r1;		
	list_fv = list_empty();

	/* setup for calling the closure */
	r1 = r2;
	r2 = (Word) 0;	/* the higher-order call has 0 extra input arguments */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */

	call(ENTRY(do_call_nondet_closure),
		LABEL(mercury__std_util__builtin_solutions_2_0_i1),
		LABEL(mercury__std_util__builtin_solutions_2_1));

Define_label(mercury__std_util__builtin_solutions_2_0_i1);
{
	/* we found a solution (in r1) */

	Word solution_copy;

	/* save the current heap pointer */
	Word *temp_hp = hp;

	/* set heap to solutions heap */
	hp = (Word) solutions_heap_pointer;

	/*
	** deep_copy() it to the solutions heap, up to the saved_hp.
	** Note that we need to save/restore the hp register, if it
	** is transient, before/after calling deep_copy().
	*/
	save_transient_registers();
	solution_copy = deep_copy(r1, (Word *) type_info_fv,
				(Word *) saved_hp_fv, heap_zone->top);
	restore_transient_registers();

	/* create a cons cell on the solutions heap */
	list_fv = list_cons(solution_copy, list_fv);

	/* save solutions heap pointer */
	solutions_heap_pointer = (Word *) hp;

	/* reset the heap pointer - use the normal mercury heap */
	hp = temp_hp;

	redo();
}
	
Define_label(mercury__std_util__builtin_solutions_2_0_i2);
	/* no more solutions */

	/* reset heap */
	restore_hp(saved_hp_fv);

	/* copy all solutions to mercury heap */ 

	{  /* create a type_info for list(T), where T is the type
	      of the solutions */

	  Word* new_type_info[2];
	  Word solutions_copy;
	 
	  new_type_info[0] = (Word *) (Word)
	  	&mercury_data_mercury_builtin__base_type_info_list_1;
	  new_type_info[1] = (Word *) type_info_fv;

	  /*
	  ** deep_copy() the list to the mercury heap, copying
	  ** everything between where we started on the solutions
	  ** heap, and the top of the solutions heap.
	  ** Note that we need to save/restore the hp register, if it
	  ** is transient, before/after calling deep_copy().
	  */
	  save_transient_registers();
	  solutions_copy = deep_copy(list_fv, (Word *) new_type_info,
		(Word *) saved_solhp_fv, solutions_heap_zone->top);
	  restore_transient_registers();

	  solutions_output_reg = solutions_copy;
	}

	/* reset solutions heap to where it was before call to solutions  */
	solutions_heap_pointer = (Word *) saved_solhp_fv;
	
	/* discard the frame we made */
	succeed_discard();

#undef saved_hp_fv
#undef saved_solhp_fv
#undef type_info_fv
#undef list_fv

#else

/*
** The following algorithm is very straight-forward implementation
** but only works with `--gc conservative'.
** Since with conservative gc, we don't reclaim any memory on failure,
** but instead leave it to the garbage collector, there is no need to
** make deep copies of the solutions.  This is a `copy-zero' implementation ;-)
*/

	/* create a nondet stack frame with one slot, to hold the list
	   of solutions, and set the failure continuation */
	mkframe(""builtin_solutions"", 1,
		LABEL(mercury__std_util__builtin_solutions_2_0_i2));
	framevar(0) = list_empty();

	/* we do not (yet) need the type_info we are passed in r1 */
	/* call the higher-order pred closure that we were passed in r2 */
	r1 = r2;
	r2 = (Word) 0;	/* the higher-order call has 0 extra input arguments */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */
	call(ENTRY(do_call_nondet_closure),
		LABEL(mercury__std_util__builtin_solutions_2_0_i1),
		LABEL(mercury__std_util__builtin_solutions_2_1));

Define_label(mercury__std_util__builtin_solutions_2_0_i1);
	/* we found a solution */
	/* insert it into the list, and then look for the next one */
	framevar(0) = list_cons(r1, framevar(0));
	redo();

Define_label(mercury__std_util__builtin_solutions_2_0_i2);
	/* no more solutions */
	/* return the solutions list and discard the frame we made */
	solutions_output_reg = framevar(0);
	succeed_discard();

#endif

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_solutions_module
*/
extern ModuleFunc solutions_module;
void sys_init_solutions_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_solutions_module(void) {
	solutions_module();
}



").

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

#include ""type_info.h""

int	mercury_compare_type_info(Word type_info_1, Word type_info_2);

").

:- pragma(c_code, "

/*
** Compare two type_info structures, using an arbitrary ordering
** (based on the addresses of the unification predicates, or in
** the case of higher order types, the arity).
*/

MR_DECLARE_STRUCT(mercury_data___base_type_info_pred_0);

int
mercury_compare_type_info(Word type_info_1, Word type_info_2)
{
	int	i, num_arg_types, comp;
	Word	unify_pred_1, unify_pred_2;
#ifdef	ONE_OR_TWO_CELL_TYPE_INFO
	Word	base_type_info_1, base_type_info_2;
#endif

	/* Test to see if either of the type_infos are null
	 * pointers, which represent a free type variable
	 * (see compiler/polymorphism.m for
	 * an explanation of when that can happen).
	 * We define the ordering so that free type variables precede
	 * ground types (this choice is arbitrary).
	 * Free type variables are considered to have been instantiated
	 * to a single type, so two free type variables compare equal.
	 */
	 if (type_info_1 == type_info_2)
		return COMPARE_EQUAL;
	 if (type_info_1 == (Word) NULL)
		return COMPARE_LESS;
	 if (type_info_2 == (Word) NULL)
		return COMPARE_GREATER;

	/* Next find the addresses of the unify preds in the type_infos */

#ifdef	ONE_OR_TWO_CELL_TYPE_INFO
	base_type_info_1 = field(mktag(0), type_info_1, 0);
	base_type_info_2 = field(mktag(0), type_info_2, 0);

	if (base_type_info_1 == 0)
		unify_pred_1 = field(mktag(0), type_info_1,
				OFFSET_FOR_UNIFY_PRED);
	else
		unify_pred_1 = field(mktag(0), base_type_info_1,
				OFFSET_FOR_UNIFY_PRED);

	if (base_type_info_2 == 0)
		unify_pred_2 = field(mktag(0), type_info_2,
				OFFSET_FOR_UNIFY_PRED);
	else
		unify_pred_2 = field(mktag(0), base_type_info_2,
				OFFSET_FOR_UNIFY_PRED);

#else
	unify_pred_1 = field(mktag(0), type_info_1, OFFSET_FOR_UNIFY_PRED);
	unify_pred_2 = field(mktag(0), type_info_2, OFFSET_FOR_UNIFY_PRED);
#endif

	/* Then compare the addresses of the unify preds in the type_infos */
	if (unify_pred_1 < unify_pred_2) {
		return COMPARE_LESS;
	}
	if (unify_pred_1 > unify_pred_2) {
		return COMPARE_GREATER;
	}

	/*
	** If the addresses of the unify preds are equal, we don't need to
	** compare the arity of the types - they must be the same -
	** unless they are higher-order (which are all mapped to
	** pred/0 when using ONE_OR_TWO_CELL_TYPE_INFO).
	** But we need to recursively compare the argument types, if any.
	*/

#ifdef	ONE_OR_TWO_CELL_TYPE_INFO

	/*
	** Higher order preds can't be optimised into the
	** type_info == base_type_info, so we don't need 
	** to check for them in this case.
	*/
	if (base_type_info_1 == 0)
		return COMPARE_EQUAL;
	else
	{
				/* Check for higher order */
		if (base_type_info_1 ==
				(Word) &mercury_data___base_type_info_pred_0) {
			int num_arg_types_2;

				/* Get number of arguments from type_info */
			num_arg_types = field(mktag(0), type_info_1, 
				TYPEINFO_OFFSET_FOR_PRED_ARITY);

			num_arg_types_2 = field(mktag(0), type_info_2, 
				TYPEINFO_OFFSET_FOR_PRED_ARITY);

				/* Check arity */
			if (num_arg_types < num_arg_types_2) {
				return COMPARE_LESS;
			} else if(num_arg_types > num_arg_types_2) {
				return COMPARE_GREATER;
			}

				/*
				** Increment, so arguments are at the
				** expected offset.
				*/
			type_info_1 += sizeof(Word);
			type_info_2 += sizeof(Word);
		} else {
			num_arg_types = field(mktag(0), base_type_info_1,
					OFFSET_FOR_COUNT);
		}
		for (i = 1; i <= num_arg_types; i++) {
			Word arg_type_info_1 = field(mktag(0),
						type_info_1, i);
			Word arg_type_info_2 = field(mktag(0),
						type_info_2, i);
			comp = mercury_compare_type_info(
					arg_type_info_1, arg_type_info_2);
			if (comp != COMPARE_EQUAL)
				return comp;
		}
		return COMPARE_EQUAL;
	}
#else
	num_arg_types = field(mktag(0), type_info_1, OFFSET_FOR_COUNT);
	for (i = 0; i < num_arg_types; i++) {
		Word arg_type_info_1 = field(mktag(0), type_info_1,
					OFFSET_FOR_ARG_TYPE_INFOS + i);
		Word arg_type_info_2 = field(mktag(0), type_info_2,
					OFFSET_FOR_ARG_TYPE_INFOS + i);
		comp = mercury_compare_type_info(
				arg_type_info_1, arg_type_info_2);
		if (comp != COMPARE_EQUAL)
			return comp;
	}
	return COMPARE_EQUAL;
#endif
}

").

:- pragma(c_header_code, "
/*
**	`univ' is represented as a two word structure.
**	One word contains the address of a type_info for the type.
**	The other word contains the data.
**	The offsets UNIV_OFFSET_FOR_TYPEINFO and UNIV_OFFSET_FOR_DATA 
**	are defined in runtime/type_info.h.
*/

#include ""type_info.h""


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
	field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO) = (Word) TypeInfo_for_T;
	field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA) = (Word) Type;
").
:- pragma(c_code, type_to_univ(Type::in, Univ::out), "
	incr_hp(Univ, 2);
	field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO) = (Word) TypeInfo_for_T;
	field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA) = (Word) Type;
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

/*
 * Univ has a special value reserved for its layout, since it needs to
 * be handled as a special case. See above for information on 
 * the representation of data of type `univ'.
 */

#ifdef  USE_TYPE_LAYOUT

const struct mercury_data_std_util__base_type_layout_univ_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data_std_util__base_type_layout_univ_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_UNIV_VALUE))
};

const struct mercury_data_std_util__base_type_functors_univ_0_struct {
	Integer f1;
} mercury_data_std_util__base_type_functors_univ_0 = {
	MR_TYPEFUNCTORS_UNIV
};

#endif

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
extern ModuleFunc unify_univ_module;
void sys_init_unify_univ_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_unify_univ_module(void) {
	unify_univ_module();
}

").

%-----------------------------------------------------------------------------%

	% Code for type manipulation.


	% Prototypes and type definitions.

:- pragma(c_header_code, "

typedef struct ML_Construct_Info_Struct {
	int vector_type;
	int arity;
	Word *functors_vector;
	Word *argument_vector;
	Word primary_tag;
	Word secondary_tag;
	ConstString functor_name;
} ML_Construct_Info;

int	ML_get_num_functors(Word type_info); 
Word 	ML_copy_argument_typeinfos(int arity, Word type_info,
				Word *arg_vector);
bool 	ML_get_functors_check_range(int functor_number, Word type_info, 
				ML_Construct_Info *info);
void	ML_copy_arguments_from_list_to_vector(int arity, Word arg_list, 
				Word term_vector);
int	ML_typecheck_arguments(Word type_info, int arity, 
				Word arg_list, Word* arg_vector);
Word 	ML_collapse_equivalences(Word maybe_equiv_type_info);

").


:- pragma c_code(type_of(Value::unused) = (TypeInfo::out),
	will_not_call_mercury, " 
{
	/* 
	** `Value' isn't used in this c_code, but the compiler
	** gives a warning if you don't mention it.
	*/ 

	/*
	** We collapse equivalences for efficiency. Any use of
	** a type_info will collapse equivalences anyway, so we
	** try to avoid doing it multiple times.
	*/
	save_transient_registers();
	TypeInfo = ML_collapse_equivalences(TypeInfo_for_T);
	restore_transient_registers();

}
").


:- pragma c_code(num_functors(TypeInfo::in) = (Functors::out), 
	will_not_call_mercury, "
{
	save_transient_registers();
	Functors = ML_get_num_functors(TypeInfo); 
	restore_transient_registers(); 
}
").


:- pragma c_code(get_functor(TypeInfo::in, FunctorNumber::in,
		FunctorName::out, Arity::out, TypeInfoList::out), 
	will_not_call_mercury, "
{
	int i;
	ML_Construct_Info info;
	bool success;

		/* 
		** Get information for this functor number and
		** store in info. If this is a discriminated union
		** type and if the functor number is in range, we
	 	** succeed.
		*/
	save_transient_registers();
	success = ML_get_functors_check_range(FunctorNumber,
				TypeInfo, &info);
	restore_transient_registers();

		/* 
		** Get the functor name and arity, construct the list
		** of type_infos for arguments.
		*/

	if (success) {
		make_aligned_string(FunctorName, (String) (Word) 
				info.functor_name);
		Arity = info.arity;
		save_transient_registers();
		TypeInfoList = ML_copy_argument_typeinfos((int) Arity,
				TypeInfo, info.argument_vector);
		restore_transient_registers();
	}
	SUCCESS_INDICATOR = success;
}
").

:- pragma c_code(construct(TypeInfo::in, FunctorNumber::in, ArgList::in) =
	(Term::out), will_not_call_mercury, "
{
	Word 	arg_list, arg_type_info, layout_entry, new_data, 
		term_vector, list_arg_type_info;
	int i;
	ML_Construct_Info info;
	bool success;

		/* 
		** Check range of FunctorNum, get info for this
		** functor.
		*/
	save_transient_registers();
	success = 
		ML_get_functors_check_range(FunctorNumber, TypeInfo, &info) &&
		ML_typecheck_arguments(TypeInfo, info.arity, ArgList, 
				info.argument_vector);
	restore_transient_registers();

		/*
		** Build the new term. 
		** 
		** It will be stored in `new_data', and `term_vector' is a
		** the argument vector.
		** 
		*/
	if (success) {

		layout_entry = MR_BASE_TYPEINFO_GET_TYPELAYOUT_ENTRY(
			MR_TYPEINFO_GET_BASE_TYPEINFO((Word *) TypeInfo), 
				info.primary_tag);

		if (info.vector_type == MR_TYPEFUNCTORS_ENUM) {
			/*
			** Enumeratiors don't have tags or arguments,
			** just the enumeration value.
			*/
			new_data = (Word) info.secondary_tag;
		} else {
			/* 
			** It must be some sort of tagged functor.
			*/

			if (info.vector_type == MR_TYPEFUNCTORS_NO_TAG) {

				/*
				** We set term_vector to point to
				** new_data so that the argument filling
				** loop will fill the argument in.
				*/

				term_vector = (Word) &new_data;

			} else if (tag(layout_entry) == 
					TYPELAYOUT_COMPLICATED_TAG) {

				/*
				** Create arity + 1 words, fill in the
				** secondary tag, and the term_vector will
				** be the rest of the words.
				*/
				incr_hp(new_data, info.arity + 1);
				field(0, new_data, 0) = info.secondary_tag;
				term_vector = (Word) (new_data + sizeof(Word));

			} else if (tag(layout_entry) == TYPELAYOUT_CONST_TAG) {

				/* 
				** If it's a du, and this tag is
				** constant, it must be a complicated
				** constant tag. 
				*/

				new_data = mkbody(info.secondary_tag);
				term_vector = (Word) NULL;

			} else {

				/*
				** A simple tagged word, just need to
				** create arguments.
				*/

				incr_hp(new_data, info.arity);
				term_vector = (Word) new_data; 
			}

				/* 
				** Copy arguments.
				*/

			ML_copy_arguments_from_list_to_vector(info.arity,
					ArgList, term_vector);

				/* 
				** Add tag to new_data.
				*/
			new_data = (Word) mkword(mktag(info.primary_tag), 
				new_data);
		}

		/* 
		** Create a univ.
		*/

		incr_hp(Term, 2);
	        field(mktag(0), Term, UNIV_OFFSET_FOR_TYPEINFO) = 
			(Word) TypeInfo;
	        field(mktag(0), Term, UNIV_OFFSET_FOR_DATA) = (Word) new_data;
	}

	SUCCESS_INDICATOR = success;
}
"). 

:- pragma(c_code, "

	/* 
	** Prototypes
	*/

static int 	get_functor_info(Word type_info, int functor_number, 
				ML_Construct_Info *info);

	/*
	** get_functor_info:
	**
	** Extract the information for functor number `functor_number',
	** for the type represented by type_info.
	** We succeed if the type is some sort of discriminated union.
	**
	** You need to save and restore transient registers around
	** calls to this function.
	*/

int 
get_functor_info(Word type_info, int functor_number, ML_Construct_Info *info)
{
	Word *base_type_functors;

	base_type_functors = MR_BASE_TYPEINFO_GET_TYPEFUNCTORS(
		MR_TYPEINFO_GET_BASE_TYPEINFO((Word *) type_info));

	info->vector_type = MR_TYPEFUNCTORS_INDICATOR(base_type_functors);

	switch	(info->vector_type) {

	case MR_TYPEFUNCTORS_ENUM:
		info->functors_vector = MR_TYPEFUNCTORS_ENUM_FUNCTORS(
				base_type_functors);
		info->arity = 0;
		info->argument_vector = NULL;
		info->primary_tag = 0;
		info->secondary_tag = functor_number;
		info->functor_name = MR_TYPELAYOUT_ENUM_VECTOR_FUNCTOR_NAME(
				info->functors_vector, functor_number);
		break; 

	case MR_TYPEFUNCTORS_DU:
		info->functors_vector = MR_TYPEFUNCTORS_DU_FUNCTOR_N(
				base_type_functors, functor_number);
		info->arity = MR_TYPELAYOUT_SIMPLE_VECTOR_ARITY(
			info->functors_vector);
		info->argument_vector = MR_TYPELAYOUT_SIMPLE_VECTOR_ARGS(
				info->functors_vector);
		info->primary_tag = tag(MR_TYPELAYOUT_SIMPLE_VECTOR_TAG(
			info->functors_vector));
		info->secondary_tag = unmkbody(
			body(MR_TYPELAYOUT_SIMPLE_VECTOR_TAG(
				info->functors_vector), info->primary_tag));
		info->functor_name = MR_TYPELAYOUT_SIMPLE_VECTOR_FUNCTOR_NAME(
				info->functors_vector);
		break; 

	case MR_TYPEFUNCTORS_NO_TAG:
		info->functors_vector = MR_TYPEFUNCTORS_NO_TAG_FUNCTOR(
				base_type_functors);
		info->arity = 1;
		info->argument_vector = MR_TYPELAYOUT_NO_TAG_VECTOR_ARGS(
				info->functors_vector);
		info->primary_tag = 0;
		info->secondary_tag = 0;
		info->functor_name = MR_TYPELAYOUT_NO_TAG_VECTOR_FUNCTOR_NAME(
				info->functors_vector);
		break; 

	case MR_TYPEFUNCTORS_EQUIV: {
			Word *equiv_type;
			equiv_type = (Word *) MR_TYPEFUNCTORS_EQUIV_TYPE(
					base_type_functors);
			return get_functor_info((Word)
					create_type_info((Word *) type_info, 
							equiv_type),
					functor_number, info);
	}
	case MR_TYPEFUNCTORS_SPECIAL:
		return FALSE;
	case MR_TYPEFUNCTORS_UNIV:
		return FALSE;
	default:
		fatal_error(""std_util:construct - unexpected type."");
	}

	return TRUE;
}

	/*
	** ML_typecheck_arguments:
	**
	** Given a list of univs (`arg_list'), and an vector of
	** type_infos (`arg_vector'), checks that they are all of the
	** same type. `arg_vector' may contain type variables, these
	** will be filled in by the type arguments of `type_info'.
	**
	** If the type arguments of `type_info' are still type variables 
	** they will be replaced by the void type (see the
	** documentation of `create_type_info').
	**
	** Assumes the length of the list has already been checked.
	**
	** You need to save and restore transient registers around
	** calls to this function.
	*/

int
ML_typecheck_arguments(Word type_info, int arity, Word arg_list,
		Word* arg_vector) 
{
	int i;
	Word arg_type_info, list_arg_type_info;

		/* Type check list of arguments */

	for (i = 0; i < arity; i++) {
		if (list_is_empty(arg_list)) {
			return FALSE;
		}
		list_arg_type_info = field(0, (list_head(arg_list)), 
			UNIV_OFFSET_FOR_TYPEINFO);

		arg_type_info = (Word) create_type_info(
			(Word *) type_info, (Word *) arg_vector[i]);

		if (mercury_compare_type_info(list_arg_type_info, 
				arg_type_info) != COMPARE_EQUAL) {
			return FALSE;
		}
		arg_list = list_tail(arg_list);
	}

		/* List should now be empty */
	return list_is_empty(arg_list);
}

	/*
	** ML_copy_arguments_from_list_to_vector:
	**
	** Copy the arguments from a list of univs (`arg_list'), 
	** into the vector (`term_vector').
	**
	** Assumes the length of the list has already been checked.
	*/

void
ML_copy_arguments_from_list_to_vector(int arity, Word arg_list,
		Word term_vector) 
{
	int i;

	for (i = 0; i < arity; i++) {
		field(mktag(0), term_vector, i) = 
			field(mktag(0), list_head(arg_list), 
				UNIV_OFFSET_FOR_DATA);
		arg_list = list_tail(arg_list);
	}
}


	/*
	** ML_get_functors_check_range:
	**
	** Check that functor_number is in range, and get the functor
	** info if it is. Return FALSE if it is out of range, or
	** if get_functor_info returns FALSE, otherwise return TRUE.
	**
	** You need to save and restore transient registers around
	** calls to this function.
	*/

bool
ML_get_functors_check_range(int functor_number, Word type_info, 
	ML_Construct_Info *info)
{
		/* 
		** Check range of functor_number, get functors
		** vector
		*/
	return  functor_number < ML_get_num_functors(type_info) &&
		functor_number >= 0 &&
		get_functor_info(type_info, functor_number, info);
}


	/* 
	** ML_copy_argument_typeinfos:
	**
	** Copy `arity' type_infos from `arg_vector' onto the heap
	** in a list. 
	** 
	** You need to save and restore transient registers around
	** calls to this function.
	*/

Word 
ML_copy_argument_typeinfos(int arity, Word type_info, Word *arg_vector)
{
	Word type_info_list, *functors;

	restore_transient_registers();
	type_info_list = list_empty(); 

	while (--arity >= 0) {
		Word argument;

			/* Get the argument type_info */
		argument = arg_vector[arity];

			/* Fill in any polymorphic type_infos */
		save_transient_registers();
		argument = (Word) create_type_info(
			(Word *) type_info, (Word *) argument);
		restore_transient_registers();

			/* Look past any equivalences */
		save_transient_registers();
		argument = ML_collapse_equivalences(argument);
		restore_transient_registers();

			/* Join the argument to the front of the list */
		type_info_list = list_cons(argument, type_info_list);
	}
	save_transient_registers();

	return type_info_list;
}

	/*
	** ML_collapse_equivalences:
	**
	** Keep looking past equivalences until the there are no more.
	** This only looks past equivalences of the top level type, not
	** the argument typeinfos.
	** 
	** You need to save and restore transient registers around
	** calls to this function.
	*/

Word
ML_collapse_equivalences(Word maybe_equiv_type_info) 
{
	Word *functors, equiv_type_info;
	
	functors = MR_BASE_TYPEINFO_GET_TYPEFUNCTORS(
			MR_TYPEINFO_GET_BASE_TYPEINFO((Word *) 
					maybe_equiv_type_info));

		/* Look past equivalences */
	while (MR_TYPEFUNCTORS_INDICATOR(functors) == MR_TYPEFUNCTORS_EQUIV) {
		equiv_type_info = (Word) MR_TYPEFUNCTORS_EQUIV_TYPE(functors);
		equiv_type_info = (Word) create_type_info(
				(Word *) maybe_equiv_type_info, 
				(Word *) equiv_type_info);
		functors = MR_BASE_TYPEINFO_GET_TYPEFUNCTORS(
			MR_TYPEINFO_GET_BASE_TYPEINFO((Word *) 
				equiv_type_info));
		maybe_equiv_type_info = equiv_type_info;
	}

	return maybe_equiv_type_info;
}

	/* 
	** ML_get_num_functors:
	**
	** Get the number of functors for a type. If it isn't a
	** discriminated union, return -1.
	**
	** You need to save and restore transient registers around
	** calls to this function.
	*/

int 
ML_get_num_functors(Word type_info)
{
	Word *base_type_functors;
	int Functors;

	base_type_functors = MR_BASE_TYPEINFO_GET_TYPEFUNCTORS(
		MR_TYPEINFO_GET_BASE_TYPEINFO((Word *) type_info));

	switch ((int) MR_TYPEFUNCTORS_INDICATOR(base_type_functors)) {

		case MR_TYPEFUNCTORS_DU:
			Functors = MR_TYPEFUNCTORS_DU_NUM_FUNCTORS(
					base_type_functors);
			break;

		case MR_TYPEFUNCTORS_ENUM:
			Functors = MR_TYPEFUNCTORS_ENUM_NUM_FUNCTORS(
					base_type_functors);
			break;

		case MR_TYPEFUNCTORS_EQUIV: {
			Word *equiv_type;
			equiv_type = (Word *) 
				MR_TYPEFUNCTORS_EQUIV_TYPE(
					base_type_functors);
			Functors = ML_get_num_functors((Word)
					create_type_info((Word *) 
						type_info, equiv_type));
			break;
		}

		case MR_TYPEFUNCTORS_SPECIAL:
			Functors = -1;
			break;

		case MR_TYPEFUNCTORS_NO_TAG:
			Functors = 1;
			break;

		case MR_TYPEFUNCTORS_UNIV:
			Functors = -1;
			break;

		default:
			fatal_error(""std_util:ML_get_num_functors :""
				"" unknown indicator"");
	}
	return Functors;
}

").

%-----------------------------------------------------------------------------%


:- pragma(c_header_code, "

	/* 
	 * Code for functor, arg and expand
	 * 
	 * This relies on some C primitives that take a type_info
	 * and a data_word, and get a functor, arity, argument vector,
	 * and argument type_info vector.
	 */

	/* Type definitions */

	/* 
	 * The last two fields, need_functor, and need_args, must
	 * be set by the caller, to indicate whether mercury_expand
	 * should copy the functor (if need_functor is non-zero) or
	 * the argument vector and type_info_vector (if need_args is
	 * non-zero). The arity will always be set.
	 *
	 * mercury_expand will fill in the other fields (functor, arity,
	 * argument_vector and type_info_vector) accordingly, but
	 * the values of fields not asked for should be assumed to
	 * contain random data when mercury_expand returns.
	 * (that is, they should not be relied on to remain unchanged).
	 */


typedef struct ML_Expand_Info_Struct {
	ConstString functor;
	int arity;
	Word *argument_vector;
	Word *type_info_vector;
	bool need_functor;
	bool need_args;
} ML_Expand_Info;


	/* Prototypes */

void mercury_expand(Word* type_info, Word data_word, ML_Expand_Info *info);

Word * create_type_info(Word *term_type_info, 
	Word *arg_pseudo_type_info);

").

:- pragma(c_code, "

static void mercury_expand_const(Word data_value, Word entry_value,
	ML_Expand_Info *info);
static void mercury_expand_enum(Word data_value, Word entry_value, 
	ML_Expand_Info *info);
static void mercury_expand_simple(Word data_value, Word* arg_type_infos, 
	Word * type_info, ML_Expand_Info *info);
static void mercury_expand_builtin(Word data_value, Word entry_value,
	ML_Expand_Info *info);
static void mercury_expand_complicated(Word data_value, Word entry_value, 
	Word * type_info, ML_Expand_Info *info);

/*
** Expand the given data using its type_info, find its
** functor, arity, argument vector and type_info vector.
** 
** The info.type_info_vector is allocated using malloc 
** It is the responsibility of the  caller to free this
** memory, and to copy any fields of this vector to
** the Mercury heap. The type_infos that the elements of
** this vector point to are either
** 	- already allocated on the heap.
** 	- constants (eg base_type_infos)
**
** Please note: 
**	mercury_expand increments the heap pointer, however, on
**	some platforms the register windows mean that transient
**	Mercury registers may be lost. Before calling mercury_expand,
**	call save_transient_registers(), and afterwards, call
**	restore_transient_registers().
**
** 	If writing a C function that calls deep_copy, make sure you
** 	document that around your function, save_transient_registers()
** 	restore_transient_registers() need to be used.
*/

void 
mercury_expand(Word* type_info, Word data_word, ML_Expand_Info *info)
{
	Word *base_type_info, *arg_type_info, *base_type_layout;
	Word data_value, entry_value, base_type_layout_entry;
	int entry_tag, data_tag; 

	base_type_info = (Word *) type_info[0];

		/* 
		 * Find the base_type_info - type_infos for types with no args 
		 * are themselves base_type_infos
		 */

	if(base_type_info == 0) {
		base_type_info = type_info;
	}

		/* Retrieve base_type_layout */
	base_type_layout = (Word *) base_type_info[OFFSET_FOR_BASE_TYPE_LAYOUT];

	data_tag = tag(data_word);
	data_value = body(data_word, data_tag);
	
	base_type_layout_entry = base_type_layout[data_tag];

	entry_tag = tag(base_type_layout_entry);
	entry_value = body(base_type_layout_entry, entry_tag);
	
	switch(entry_tag) {

	case TYPELAYOUT_CONST_TAG: /* case TYPELAYOUT_COMP_CONST_TAG: */

		/* Is it a builtin or a constant/enum? */ 

		if (entry_value > TYPELAYOUT_MAX_VARINT) {

			/* Check enum indicator */

			if (((Word *) entry_value)[0]) {
				mercury_expand_enum(data_word, entry_value, 
					info);
			} else {
				data_value = unmkbody(data_value);
				mercury_expand_const(data_value, entry_value, 
					info);
			}
		} else {
			entry_value = unmkbody(entry_value);
			mercury_expand_builtin(data_word, entry_value,
				info);
		}
		break;

	case TYPELAYOUT_SIMPLE_TAG:
		mercury_expand_simple(data_value, (Word *) entry_value, 
			type_info, info);
		break;

	case TYPELAYOUT_COMPLICATED_TAG:
		mercury_expand_complicated(data_value, entry_value, type_info,
			info);
		break;

	case TYPELAYOUT_EQUIV_TAG: /* case TYPELAYOUT_NO_TAG: */
	{
		int allocated = 0; 

#ifdef DEBUG_STD_UTIL__EXPAND
		fprintf(stderr, ""Equivalent to:\n""); 
#endif

		/* is it equivalent to a type variable? */

		if (entry_value < TYPELAYOUT_MAX_VARINT) {
			arg_type_info = create_type_info(type_info, 
				(Word *) entry_value);
			mercury_expand(arg_type_info, data_word, info);
		}
			/* is it a no_tag type? */
		else if (((Word *) entry_value)[0]) {
			Word new_arg_vector; 
			incr_saved_hp(new_arg_vector, 1);
			field(0, new_arg_vector, 0) = data_word;
			mercury_expand_simple(new_arg_vector, 
				(Word *) entry_value, type_info, info);
		}
			/* is it an equivalent type */
		else {
			arg_type_info = create_type_info(type_info, 
				(Word *) ((Word *) entry_value)[1]);
			mercury_expand(arg_type_info, data_word, info);
		}

	}
	break;

	default:
		/* If this happens, the layout data is corrupt */

		fatal_error(""Found unused tag value in expand"");
	}
}

/*
 * Expand a constant value.
 */

void
mercury_expand_const(Word data_value, Word entry_value, ML_Expand_Info *info) 
{

#ifdef DEBUG_STD_UTIL__EXPAND
	fprintf(stderr, 
		""This is a constant functor, %ld of %ld with this tag\n"",
            data_value + 1, ((Word *) entry_value)[1]); 
#endif

	/* the functors are stored after the enum_indicator and
	 * the number of functors
	 */
	info->functor = (ConstString) ((Word *) entry_value)[data_value +
		TYPELAYOUT_CONST_FUNCTOR_OFFSET];	
	info->arity = 0;
	info->argument_vector = NULL;
	info->type_info_vector = NULL;
}


/*
 * Expand an enum.
 */

void
mercury_expand_enum(Word data_value, Word entry_value, ML_Expand_Info *info) 
{

#ifdef DEBUG_STD_UTIL__EXPAND
	fprintf(stderr, 
		""This is a constant functor, %ld of %ld in this enum\n"",
            data_value + 1, ((Word *) entry_value)[1]); 
#endif

	/* the functors are stored after the enum_indicator and
	 * the number of functors
	 */

	info->functor = (ConstString) ((Word *) entry_value)[data_value + 
		TYPELAYOUT_ENUM_FUNCTOR_OFFSET];	
	info->arity = 0;
	info->argument_vector = NULL;
	info->type_info_vector = NULL;
}


/*
 * Expand a functor with arguments, which has a simple tag.
 *
 * Simple tags - type_layout points to an array containing
 * the arity, then a pseudo-typeinfo for each argument, and type_info is
 * the current type_info (the type of this data item).
 *
 * Data word points to an array of argument data.
 *
 */
void 
mercury_expand_simple(Word data_value, Word* arg_type_infos, Word * type_info,
	ML_Expand_Info *info)
{
	int i;

	info->arity = arg_type_infos[TYPELAYOUT_SIMPLE_ARITY_OFFSET];

	if (info->need_functor) {
		make_aligned_string(info->functor, 
			(String) arg_type_infos[info->arity + 
			TYPELAYOUT_SIMPLE_FUNCTOR_OFFSET]);
	}

	if (info->need_args) {
		info->argument_vector = (Word *) data_value;

		info->type_info_vector = 
			checked_malloc(info->arity * sizeof(Word));

		for (i = 0; i < info->arity ; i++) {
			Word * arg_pseudo_type_info;

			arg_pseudo_type_info = (Word *) arg_type_infos[i + 1];
			info->type_info_vector[i] = (Word) 
				create_type_info(type_info, 
					(Word *) arg_pseudo_type_info);
		}
	}
}

/*
 * Complicated tags - entry_value points to a vector containing: 
 *	The number of sharers of this tag
 *	A pointer to a simple tag structure (see mercury_print_simple)
 *	for each sharer.
 *
 *	The data_value points to the actual sharer of this tag, 
 *	which should be used as an index into the vector of pointers
 *	into simple tag structures. The next n words the data_value
 *	points to are the arguments of the functor.
 */

void
mercury_expand_complicated(Word data_value, Word entry_value, Word * type_info,
	ML_Expand_Info *info)
{
	Word new_data_value, new_entry_value, new_entry_body,
		new_entry_tag, secondary_tag;

	secondary_tag = ((Word *) data_value)[0];

#ifdef DEBUG_STD_UTIL__EXPAND
	fprintf(stderr, ""This is %ld of %ld functors sharing this tag\n"",
		secondary_tag + 1, ((Word *) entry_value)[0]); 
#endif

	new_entry_value = ((Word *) entry_value)[secondary_tag + 1];
	new_entry_tag = tag(new_entry_value);
	new_entry_body = body(new_entry_value, new_entry_tag);
	new_data_value = (Word) ((Word *) data_value + 1);

	mercury_expand_simple(new_data_value, (Word *) new_entry_body, 
		type_info, info);
}

void
mercury_expand_builtin(Word data_value, Word entry_value, ML_Expand_Info *info)
{
	switch ((int) entry_value) {
	
	case TYPELAYOUT_UNASSIGNED_VALUE:
		fatal_error(""Attempt to use an UNASSIGNED tag in expand."");
		break;

	case TYPELAYOUT_UNUSED_VALUE:
		fatal_error(""Attempt to use an UNUSED tag in expand."");
		break;

	case TYPELAYOUT_STRING_VALUE:
		/* XXX should escape characters correctly */

		if (info->need_functor) {
			char *str;

			incr_saved_hp_atomic(LVALUE_CAST(Word, str),
				(strlen((String) data_value) + 2 + 
					sizeof(Word)) / sizeof(Word));
			sprintf(str, ""%c%s%c"", '""', 
				(String) data_value, '""');
			info->functor = str;
		}
		info->argument_vector = NULL;
		info->type_info_vector = NULL;
		info->arity = 0;
		break;

	case TYPELAYOUT_FLOAT_VALUE:
		if (info->need_functor) {
			char buf[500];
			Float f;
			char *str;

			f = word_to_float(data_value);
			sprintf(buf, ""%#.15g"", f);
			incr_saved_hp_atomic(LVALUE_CAST(Word, str), 
				(strlen(buf) + sizeof(Word)) / sizeof(Word));
			strcpy(str, buf);
			info->functor = str;
		}
		info->argument_vector = NULL;
		info->type_info_vector = NULL;
		info->arity = 0;
	break;

	case TYPELAYOUT_INT_VALUE:
		if (info->need_functor) {
			char buf[500];
			char *str;

			sprintf(buf, ""%ld"", (long) data_value);
			incr_saved_hp_atomic(LVALUE_CAST(Word, str), 
				(strlen(buf) + sizeof(Word)) / sizeof(Word));
			strcpy(str, buf);
			info->functor = str;
		}

		info->argument_vector = NULL;
		info->type_info_vector = NULL;
		info->arity = 0;
	break;

	case TYPELAYOUT_CHARACTER_VALUE:
		/* XXX should escape characters correctly */

		if (info->need_functor) {
			char *str;

			incr_saved_hp_atomic(LVALUE_CAST(Word, str), 
				(3 + sizeof(Word)) / sizeof(Word));
			sprintf(str, ""\'%c\'"", (char) data_value);
			info->functor = str;
		}
		info->argument_vector = NULL;
		info->type_info_vector = NULL;
		info->arity = 0;
	break;

	case TYPELAYOUT_UNIV_VALUE:

		/* Univ is a two word structure, containing
		 * type_info and data.
		 */

		mercury_expand((Word *) 
			((Word *) data_value)[UNIV_OFFSET_FOR_TYPEINFO], 
			((Word *) data_value)[UNIV_OFFSET_FOR_DATA], info);
		break;

	case TYPELAYOUT_PREDICATE_VALUE:
		if (info->need_functor) {
			make_aligned_string(info->functor, ""<<predicate>>"");
		}
		info->argument_vector = NULL;
		info->type_info_vector = NULL;
		info->arity = 0;
	break;

	default:
		fatal_error(""Invalid tag value in expand"");
		break;
	}
}


	/* 
	** Given a type_info (term_type_info) which contains a
	** base_type_info pointer and possibly other type_infos
	** giving the values of the type parameters of this type,
	** and a pseudo-type_info (arg_pseudo_type_info), which contains a
	** base_type_info pointer and possibly other type_infos
	** giving EITHER
	** 	- the values of the type parameters of this type,
	** or	- an indication of the type parameter of the
	** 	  term_type_info that should be substituted here
	**
	** This returns a fully instantiated type_info, a version of the
	** arg_pseudo_type_info with all the type variables filled in.
	**
	** If the substituted type parameters from the term_type_info
	** were type variables, they will be replaced with references
	** to the void type ('void'/0).
	** XXX: This is a temporary measure. It would be best if the
	** code in polymorphism.m and typecheck.m was changed to output
	** references to 'void' for unbound type variables, rather than
	** outputting NULL pointers, which we convert to references to
	** void here. Note that this would also involve changing any
	** code that relied upon the NULL definition (for example,
	** mercury_compare_type_info).
	**
	** We allocate memory for a new type_info on the Mercury heap,
	** copy the necessary information, and return a pointer to the
	** new type_info. 
	**
	** In the case where the argument's pseudo_type_info is a
	** base_type_info with no arguments, we don't copy the
	** base_type_info - we just return a pointer to it - no memory
	** is allocated. The caller can check this by looking at the
	** first cell of the returned pointer - if it is zero, this is a
	** base_type_info. Otherwise, it is an allocated copy of a
	** type_info.
	**
	** NOTE: If you are changing this code, you might also need
	** to change the code in create_type_info in runtime/deep_copy.c,
	** which does much the same thing, only allocating using malloc
	** instead of on the heap.
	*/

MR_DECLARE_STRUCT(mercury_data___base_type_info_void_0);

Word * 
create_type_info(Word *term_type_info, Word *arg_pseudo_type_info)
{
	int i, arity;
	Word base_type_info;
	Word *type_info;

		/* 
		** The arg_pseudo_type_info might be a polymorphic variable,
		** if so - substitute.
		*/

	if ((Word) arg_pseudo_type_info < TYPELAYOUT_MAX_VARINT) {
		arg_pseudo_type_info = (Word *) 
			term_type_info[(Word) arg_pseudo_type_info];
	}

		/* 
		** If it's still a variable, make it a reference to 'void'.
		*/
	if (arg_pseudo_type_info == NULL) {
		return (Word *) (Word) &mercury_data___base_type_info_void_0;
	}
	else if ((Word) arg_pseudo_type_info < TYPELAYOUT_MAX_VARINT) {
		fatal_error(""unbound type variable"");
	}

	base_type_info = arg_pseudo_type_info[0];

		/* no arguments - optimise common case */
	if (base_type_info == 0) {
		return arg_pseudo_type_info;
	}

	arity = MR_BASE_TYPEINFO_GET_TYPE_ARITY(base_type_info);

	incr_saved_hp(LVALUE_CAST(Word, type_info), arity + 1);

	for (i = 0; i <= arity; i++) {
		if (arg_pseudo_type_info[i] < TYPELAYOUT_MAX_VARINT) {
			type_info[i] = term_type_info[arg_pseudo_type_info[i]];

			/* 
			** If it's still a variable, make it a reference 
			** to `void'.
			*/
			if ((Word *) type_info[i] == NULL) {
				type_info[i] = (Word) 
					&mercury_data___base_type_info_void_0;
			}
			else if (type_info[i] < TYPELAYOUT_MAX_VARINT) {
				fatal_error(""unbound type variable"");
			}

		} else {
			type_info[i] = arg_pseudo_type_info[i];
		}
	}
	return type_info;
}

").

%-----------------------------------------------------------------------------%

	% Code for functor, arg and expand.

:- pragma(c_code, functor(Type::in, Functor::out, Arity::out), " 
{
	ML_Expand_Info info;

	info.need_functor = TRUE;
	info.need_args = FALSE;

	save_transient_registers();

	mercury_expand((Word *) TypeInfo_for_T, Type, &info);

	restore_transient_registers();

		/* Copy functor onto the heap */
	make_aligned_string(LVALUE_CAST(ConstString, Functor), info.functor);

	Arity = info.arity;

}").

:- pragma(c_code, argument(Type::in, ArgumentIndex::in, Argument::out), " 
{
	ML_Expand_Info info;
	Word arg_pseudo_type_info;
	bool success;

	info.need_functor = FALSE;
	info.need_args = TRUE;

	save_transient_registers();

	mercury_expand((Word *) TypeInfo_for_T, Type, &info);

	restore_transient_registers();

		/* Check range */
	success = (ArgumentIndex >= 0 && ArgumentIndex < info.arity);
	if (success) {

			/* Allocate enough room for a univ */
		incr_hp(Argument, 2);
		arg_pseudo_type_info = info.type_info_vector[ArgumentIndex];
		if (arg_pseudo_type_info < TYPELAYOUT_MAX_VARINT) {
			field(0, Argument, UNIV_OFFSET_FOR_TYPEINFO) = 
				((Word *) TypeInfo_for_T)[arg_pseudo_type_info];
		}
		else {
			field(0, Argument, UNIV_OFFSET_FOR_TYPEINFO) = 
				arg_pseudo_type_info;
		}
		field(0, Argument, UNIV_OFFSET_FOR_DATA) = 
			info.argument_vector[ArgumentIndex];
	}

	/* Free the allocated type_info_vector, since we just copied
	 * the argument we want onto the heap. 
	 */

	free(info.type_info_vector);

	SUCCESS_INDICATOR = success;

}").

arg(ArgumentIndex, Type, Argument) :-
	ArgumentIndex1 is ArgumentIndex - 1,
	argument(Type, ArgumentIndex1, Argument).

det_arg(ArgumentIndex, Type, Argument) :-
	ArgumentIndex1 is ArgumentIndex - 1,
	det_argument(Type, ArgumentIndex1, Argument).

det_argument(Type, ArgumentIndex, Argument) :-
	(
		argument(Type, ArgumentIndex, Argument0)
	->
		Argument = Argument0
	;
		error("det_argument : argument out of range")
	).

:- pragma(c_code, expand(Type::in, Functor::out, Arity::out, Arguments::out), " 
{
	ML_Expand_Info info;
	Word arg_pseudo_type_info;
	Word Argument, tmp;
	int i;

	info.need_functor = TRUE;
	info.need_args = TRUE;

	save_transient_registers();

	mercury_expand((Word *) TypeInfo_for_T, Type, &info);
	
	restore_transient_registers();

		/* Get functor */
	make_aligned_string(LVALUE_CAST(ConstString, Functor), info.functor);

		/* Get arity */
	Arity = info.arity;

		/* Build argument list */
	Arguments = list_empty();
	i = info.arity;

	while (--i >= 0) {

			/* Create an argument on the heap */
		incr_hp(Argument, 2);

			/* Join the argument to the front of the list */
		Arguments = list_cons(Argument, Arguments);

			/* Fill in the arguments */
		arg_pseudo_type_info = info.type_info_vector[i];

		if (arg_pseudo_type_info < TYPELAYOUT_MAX_VARINT) {

				/* It's a type variable, get its value */
			field(0, Argument, UNIV_OFFSET_FOR_TYPEINFO) = 
				((Word *) TypeInfo_for_T)[arg_pseudo_type_info];
		}
		else {
				/* It's already a type_info */
			field(0, Argument, UNIV_OFFSET_FOR_TYPEINFO) = 
				arg_pseudo_type_info;
		}
			/* Fill in the data */
		field(0, Argument, UNIV_OFFSET_FOR_DATA) = 
			info.argument_vector[i];
	}

	/* Free the allocated type_info_vector, since we just copied
	 * all its arguments onto the heap. 
	 */

	free(info.type_info_vector);

}").

%-----------------------------------------------------------------------------%
