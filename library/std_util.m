%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: std_util.m.
% Main author: fjh.
% Stability: medium to high.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.

:- interface.

:- import_module list, set.

%-----------------------------------------------------------------------------%

% The universal type `univ'.
% An object of type `univ' can hold the type and value of an object of any
% other type.
%
% Note that the current NU-Prolog/SICStus Prolog implementation of
% univ_to_type is buggy in that it always succeeds, even if the types didn't
% match, so until this gets implemented correctly, don't use
% univ_to_type unless you are sure that the types will definitely match,
% or you don't care about debugging with Prolog.

:- type univ.

	% type_to_univ(Object, Univ):
	% 	true iff the type stored in `Univ' is the same as the type
	%	of `Object', and the value stored in `Univ' is equal to the
	%	value of `Object'.
	%
	% Operational, the forwards mode converts an object to type `univ',
	% while the reverse mode converts the value stored in `Univ'
	% to the type of `Object', but fails if the type stored in `Univ'
	% does not match the type of `Object'.
	% 
:- pred type_to_univ(T, univ).
:- mode type_to_univ(di, uo) is det.
:- mode type_to_univ(in, out) is det.
:- mode type_to_univ(out, in) is semidet.

	% univ_to_type(Univ, Object) :- type_to_univ(Object, Univ).
	%
:- pred univ_to_type(univ, T).
:- mode univ_to_type(in, out) is semidet.
:- mode univ_to_type(out, in) is det.
:- mode univ_to_type(uo, di) is det.

	% The function univ/1 provides the same
	% functionality as type_to_univ/2.

	% univ(Object) = Univ :- type_to_univ(Object, Univ).
	%
:- func univ(T) = univ.
:- mode univ(in) = out is det.
:- mode univ(di) = uo is det.
:- mode univ(out) = in is semidet.

	% univ_type(Univ):
	%	returns the type_info for the type stored in `Univ'.
	%
:- func univ_type(univ) = type_info.

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

	% aggregate/4 generates all the solutions to a predicate,
	% sorts them and removes duplicates, then applies an accumulator
	% predicate to each solution in turn:
	%
	% aggregate(Generator, Accumulator, Acc0, Acc) <=>
	%	solutions(Generator, Solutions),
	%	list__foldl(Accumulator, Solutions, Acc0, Acc).
	%

:- pred aggregate(pred(T), pred(T, U, U), U, U).
:- mode aggregate(pred(out) is multi, pred(in, in, out) is det,
		in, out) is det.
:- mode aggregate(pred(out) is multi, pred(in, di, uo) is det,
		di, uo) is det.
:- mode aggregate(pred(out) is nondet, pred(in, di, uo) is det,
		di, uo) is det.
:- mode aggregate(pred(out) is nondet, pred(in, in, out) is det,
		in, out) is det.

	% unsorted_aggregate/4 generates all the solutions to a predicate
	% and applies an accumulator predicate to each solution in turn:
	%
	% unsorted_aggregate(Generator, Accumulator, Acc0, Acc) <=>
	%	unsorted_solutions(Generator, Solutions),
	%	list__foldl(Accumulator, Solutions, Acc0, Acc).
	%
	% The current implementation is in terms of [unsorted_]solutions and
	% list__foldl, which, for a predicate with N solutions requires O(N)
	% memory, whereas it is possible to implement unsorted_aggregate
	% to use O(1) memory.

:- pred unsorted_aggregate(pred(T), pred(T, U, U), U, U).
:- mode unsorted_aggregate(pred(out) is multi, pred(in, in, out) is det,
		in, out) is cc_multi.
:- mode unsorted_aggregate(pred(out) is multi, pred(in, di, uo) is det,
		di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, di, uo) is det,
		di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, in, out) is det,
		in, out) is cc_multi.

%-----------------------------------------------------------------------------%

	% maybe_pred(Pred, X, Y) takes a closure Pred which transforms an
	% input semideterministically. If calling the closure with the input
	% X succeeds, Y is bound to `yes(Z)' where Z is the output of the
	% call, or to `no' if the call fails.
	%
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

	% The `type_info' and `type_ctor_info' types: these
	% provide access to type information.
	% A type_info represents a type, e.g. `list(int)'.
	% A type_ctor_info represents a type constructor, e.g. `list/1'.

	% XXX The facilities here don't yet work for higher-order types.
	%     If you try, there is a good chance that you will get a core
	%     dump or worse.
 
:- type type_info.
:- type type_ctor_info.

	% (Note: it is not possible for the type of a variable to be an
	% unbound type variable; if there are no constraints on a type
	% variable, then the typechecker will use the type `void'.
	% `void' is a special (builtin) type that has no constructors.
	% There is no way of creating an object of type `void'.
	% `void' is not considered to be a discriminated union, so
	% get_functor/5 and construct/3 will fail if used upon a value
	% of this type.)

	% type_of(Data) returns a representation of the type of Data.
	%
:- func type_of(T) = type_info.
:- mode type_of(unused) = out is det.

	% type_name(Type) returns the name of the specified type
	% (e.g. type_name(type_of([2,3])) = "list(int)").
	% Any equivalence types will be fully expanded.
	% XXX we should think about what happens with module qualifiers...
	%
:- func type_name(type_info) = string.

	% type_ctor_and_args(Type, TypeCtor, TypeArgs):
	%	True iff `TypeCtor' is a representation of the top-level
	%	type constructor for `Type', and `TypeArgs' is a list
	%	of the corresponding type arguments to `TypeCtor',
	%	and `TypeCtor' is not an equivalence type.
	%
	% For example, type_ctor_and_args(type_of([2,3]), TypeCtor,
	% TypeArgs) will bind `TypeCtor' to a representation of the
	% type constructor list/1, and will bind `TypeArgs' to the list
	% `[Int]', where `Int' is a representation of the type `int'.
	%
	% Note that the requirement that `TypeCtor' not be an
	% equivalence type is fulfilled by fully expanding any
	% equivalence types.  For example, if you have a declaration
	% `:- type foo == bar.', then type_ctor_and_args/3 will always
	% return a representation of type constructor `bar/0', not `foo/0'. 
	% (If you don't want them expanded, you can use the reverse mode
	% of make_type/2 instead.)
	%
:- pred type_ctor_and_args(type_info, type_ctor_info, list(type_info)).
:- mode type_ctor_and_args(in, out, out) is det.

	% type_ctor(Type) = TypeCtor :-
	%	type_ctor_and_args(Type, TypeCtor, _).
	%
:- func type_ctor(type_info) = type_ctor_info.

	% type_args(Type) = TypeArgs :-
	%	type_ctor_and_args(Type, _, TypeArgs).
	%
:- func type_args(type_info) = list(type_info).

	% type_ctor_name(TypeCtor) returns the name of specified
	% type constructor.
	% (e.g. type_ctor_name(type_ctor(type_of([2,3]))) = "list").
	% XXX we should think about what happens with module qualifiers...
	%
:- func type_ctor_name(type_ctor_info) = string.

	% type_ctor_arity(TypeCtor) returns the arity of specified
	% type constructor.
	% (e.g. type_ctor_arity(type_ctor(type_of([2,3]))) = 1).
	%
:- func type_ctor_arity(type_ctor_info) = int.

	% type_ctor_name_and_arity(TypeCtor, Name, Arity) :-
	%	Name = type_ctor_name(TypeCtor),
	%	Arity = type_ctor_arity(TypeCtor).
	% XXX we should think about what happens with module qualifiers...
	%
:- pred type_ctor_name_and_arity(type_ctor_info, string, int).
:- mode type_ctor_name_and_arity(in, out, out) is det.

	% make_type(TypeCtor, TypeArgs) = Type:
	%	True iff `Type' is a type constructed by applying
	%	the type constructor `TypeCtor' to the type arguments
	%	`TypeArgs'.
	%
	% Operationally, the forwards mode returns the type formed by
	% applying the specified type constructor to the specified
	% argument types, or fails if the length of TypeArgs is not the
	% same as the arity of TypeCtor.  The reverse mode returns a
	% type constructor and its argument types, given a type_info;
	% the type constructor returned may be an equivalence type
	% (and hence this reverse mode of make_type/2 may be more useful
	% for some purposes than the type_ctor/1 function).
	% 
:- func make_type(type_ctor_info, list(type_info)) = type_info.
:- mode make_type(in, in) = out is semidet.
:- mode make_type(out, out) = in is cc_multi.

	% det_make_type(TypeCtor, TypeArgs):
	%
	% Returns the type formed by applying the specified type
	% constructor to the specified argument types.  Aborts if the
	% length of `TypeArgs' is not the same as the arity of `TypeCtor'.
	%
:- func det_make_type(type_ctor_info, list(type_info)) = type_info.
:- mode det_make_type(in, in) = out is det.

%-----------------------------------------------------------------------------%

	% num_functors(TypeInfo) 
	% 
	% Returns the number of different functors for the top-level
	% type constructor of the type specified by TypeInfo, or -1
	% if the type is not a discriminated union type.
	%
:- func num_functors(type_info) = int.

	% get_functor(Type, N, Functor, Arity, ArgTypes)
	%
	% Binds Functor and Arity to the name and arity of the Nth
	% functor for the specified type (starting at zero), and binds
	% ArgTypes to the type_infos for the types of the arguments of
	% that functor.  Fails if the type is not a discriminated union
	% type, or if N is out of range.
	%
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
	%
:- func construct(type_info, int, list(univ)) = univ.
:- mode construct(in, in, in) = out is semidet.

%-----------------------------------------------------------------------------%

	% functor, argument and deconstruct take any type (including univ),
	% and return representation information for that type.
	%
	% The string representation of the functor that `functor' and 
	% `deconstruct' return is:
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
	%
:- pred functor(T::in, string::out, int::out) is det.

	% argument(Data, ArgumentIndex) = Argument
	% 
	% Given a data item (Data) and an argument index
	% (ArgumentIndex), starting at 0 for the first argument, binds
	% Argument to that argument of the functor of the data item. If
	% the argument index is out of range -- that is, greater than or
	% equal to the arity of the functor or lower than 0 -- argument/3
	% fails.  The argument returned has the type univ. 
	%
:- func argument(T::in, int::in) = (univ::out) is semidet.

	% det_argument(ArgumentIndex, Data, Argument)
	% 
	% Given a data item (Data) and an argument index
	% (ArgumentIndex), starting at 0 for the first argument, binds
	% Argument to that argument of the functor of the data item. If
	% the argument index is out of range -- that is, greater than or
	% equal to the arity of the functor or lower than 0 --
	% det_argument/3 aborts. 
	%
:- func det_argument(T::in, int::in) = (univ::out) is det.

	% deconstruct(Data, Functor, Arity, Arguments) 
	% 
	% Given a data item (Data), binds Functor to a string
	% representation of the functor, Arity to the arity of this data
	% item, and Arguments to a list of arguments of the functor.
	% The arguments in the list are each of type univ.
	%
:- pred deconstruct(T::in, string::out, int::out, list(univ)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, set, int, string.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

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
	mercury_data_list__base_type_info_list_1);

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
** with both calling conventions. The output should go either in r3
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
	  	&mercury_data_list__base_type_info_list_1;
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

%-----------------------------------------------------------------------------%

aggregate(Generator, Accumulator, Acc0, Acc) :-
	solutions(Generator, Solutions),
	list__foldl(Accumulator, Solutions, Acc0, Acc).

unsorted_aggregate(Generator, Accumulator, Acc0, Acc) :-
	unsorted_solutions(Generator, Solutions),
	list__foldl(Accumulator, Solutions, Acc0, Acc).

%-----------------------------------------------------------------------------%

% semidet_succeed and semidet_fail, implemented using the C interface
% to make sure that the compiler doesn't issue any determinism warnings
% for them.

:- pragma c_code(semidet_succeed, will_not_call_mercury,
		"SUCCESS_INDICATOR = TRUE;").
:- pragma c_code(semidet_fail, will_not_call_mercury,
		"SUCCESS_INDICATOR = FALSE;").
:- pragma c_code(cc_multi_equal(X::in, Y::out), will_not_call_mercury,
		"Y = X;").
%-----------------------------------------------------------------------------%

univ_to_type(Univ, X) :- type_to_univ(X, Univ).

univ(X) = Univ :- type_to_univ(X, Univ).

/****

% univ_value/1 can't be implemented yet, due to the lack of support for
% existential types in Mercury.

	% univ_value(Univ):
	%	returns the value of the object stored in Univ.
:- some [T] (
   func univ_value(univ) = T
).

:- pragma c_code(univ_value(Univ::uo) = (Value), will_not_call_mercury, "
	TypeInfo_for_T = field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO);
	Value = field(mktag(0), Univ, UNIV_OFFSET_FOR_Data);
").

****/

:- pragma c_header_code("

#include ""type_info.h""

int	ML_compare_type_info(Word type_info_1, Word type_info_2);

").

:- pragma c_code("

/*
** ML_compare_type_info(type_info_1, type_info_2):
**
** Compare two type_info structures, using an arbitrary ordering
** (based on the addresses of the base_type_infos, or in
** the case of higher order types, the arity).
**
** You need to save and restore transient registers around
** calls to this function.
*/

MR_DECLARE_STRUCT(mercury_data___base_type_info_pred_0);

int
ML_compare_type_info(Word t1, Word t2)
{
	Word	*type_info_1, *type_info_2;
	Word	*base_type_info_1, *base_type_info_2;
	int	num_arg_types;
	int	i;

	/* 
	** Try to optimize a common case:
	** If type_info addresses are equal, they must represent the
	** same type.
	*/
	if (t1 == t2) {
		return COMPARE_EQUAL;
	}

	/* 
	** Otherwise, we need to expand equivalence types, if any.
	*/
	type_info_1 = (Word *) ML_collapse_equivalences(t1);
	type_info_2 = (Word *) ML_collapse_equivalences(t2);

	/* 
	** Perhaps they are equal now...
	*/
	if (type_info_1 == type_info_2) {
		return COMPARE_EQUAL;
	}

	/*
	** Otherwise find the addresses of the base_type_infos,
	** and compare those.
	**
	** Note: this is an arbitrary ordering. It doesn't matter
	** what the ordering is, just so long as it is consistent.
	** ANSI C doesn't guarantee much about pointer comparisons,
	** so it is possible that this might not do the right thing
	** on some obscure systems.
	** The casts to (Word) here are in the hope of increasing
	** the chance that this will work on a segmented architecture.
	*/
	base_type_info_1 = MR_TYPEINFO_GET_BASE_TYPEINFO(type_info_1);
	base_type_info_2 = MR_TYPEINFO_GET_BASE_TYPEINFO(type_info_2);
	if ((Word) base_type_info_1 < (Word) base_type_info_2) {
		return COMPARE_LESS;
	}
	if ((Word) base_type_info_1 > (Word) base_type_info_2) {
		return COMPARE_GREATER;
	}

	/*
	** If the base_type_info addresses are equal, we don't need to
	** compare the arity of the types - they must be the same -
	** unless they are higher-order (which are all mapped to
	** pred/0). 
	** But we need to recursively compare the argument types, if any.
	*/
		/* Check for higher order */
	if (base_type_info_1 ==
		(const Word *) &mercury_data___base_type_info_pred_0)
	{
		int num_arg_types_2;

			/* Get number of arguments from type_info */
		num_arg_types = field(mktag(0), type_info_1, 
			TYPEINFO_OFFSET_FOR_PRED_ARITY);

		num_arg_types_2 = field(mktag(0), type_info_2, 
			TYPEINFO_OFFSET_FOR_PRED_ARITY);

			/* Check arity */
		if (num_arg_types < num_arg_types_2) {
			return COMPARE_LESS;
		}
		if (num_arg_types > num_arg_types_2) {
			return COMPARE_GREATER;
		}

			/*
			** Increment, so arguments are at the
			** expected offset.
			*/
		type_info_1++;
		type_info_2++;
	} else {
		num_arg_types = field(mktag(0), base_type_info_1,
				OFFSET_FOR_COUNT);
	}
		/* compare the argument types */
	for (i = 0; i < num_arg_types; i++) {
		Word arg_type_info_1 = field(mktag(0), type_info_1,
			OFFSET_FOR_ARG_TYPE_INFOS + i);
		Word arg_type_info_2 = field(mktag(0), type_info_2,
			OFFSET_FOR_ARG_TYPE_INFOS + i);
		int comp = ML_compare_type_info(
				arg_type_info_1, arg_type_info_2);
		if (comp != COMPARE_EQUAL)
			return comp;
	}
	return COMPARE_EQUAL;
}

").

:- pragma c_header_code("
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
:- pragma c_code(type_to_univ(Type::di, Univ::uo), will_not_call_mercury, "
	incr_hp(Univ, 2);
	field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO) = (Word) TypeInfo_for_T;
	field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA) = (Word) Type;
").
:- pragma c_code(type_to_univ(Type::in, Univ::out), will_not_call_mercury, "
	incr_hp(Univ, 2);
	field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO) = (Word) TypeInfo_for_T;
	field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA) = (Word) Type;
").

	% Backward mode - convert from univ to type.
	% We check that type_infos compare equal.
	% The variable `TypeInfo_for_T' used in the C code
	% is the compiler-introduced type-info variable.
:- pragma c_code(type_to_univ(Type::out, Univ::in), will_not_call_mercury, "{
	Word univ_type_info = field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO);
	int comp;
	save_transient_registers();
	comp = ML_compare_type_info(univ_type_info, TypeInfo_for_T);
	restore_transient_registers();
	if (comp == COMPARE_EQUAL) {
		Type = field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA);
		SUCCESS_INDICATOR = TRUE;
	} else {
		SUCCESS_INDICATOR = FALSE;
	}
}").

:- pragma c_code(univ_type(Univ::in) = (TypeInfo::out), will_not_call_mercury, "
	TypeInfo = field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO);
").

:- pragma c_code("

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

const struct mercury_data_std_util__base_type_layout_type_info_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data_std_util__base_type_layout_type_info_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_TYPEINFO_VALUE))
};

const struct mercury_data_std_util__base_type_functors_type_info_0_struct {
	Integer f1;
} mercury_data_std_util__base_type_functors_type_info_0 = {
	MR_TYPEFUNCTORS_SPECIAL
};

#endif

Define_extern_entry(mercury____Unify___std_util__univ_0_0);
Define_extern_entry(mercury____Index___std_util__univ_0_0);
Define_extern_entry(mercury____Compare___std_util__univ_0_0);
Declare_label(mercury____Compare___std_util__univ_0_0_i1);

Define_extern_entry(mercury____Unify___std_util__type_info_0_0);
Define_extern_entry(mercury____Index___std_util__type_info_0_0);
Define_extern_entry(mercury____Compare___std_util__type_info_0_0);

BEGIN_MODULE(unify_univ_module)
	init_entry(mercury____Unify___std_util__univ_0_0);
	init_entry(mercury____Index___std_util__univ_0_0);
	init_entry(mercury____Compare___std_util__univ_0_0);
	init_label(mercury____Compare___std_util__univ_0_0_i1);

	init_entry(mercury____Unify___std_util__type_info_0_0);
	init_entry(mercury____Index___std_util__type_info_0_0);
	init_entry(mercury____Compare___std_util__type_info_0_0);
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
	int comp;

	univ1 = unify_input1;
	univ2 = unify_input2;

	/* First check the type_infos compare equal */
	typeinfo1 = field(mktag(0), univ1, UNIV_OFFSET_FOR_TYPEINFO);
	typeinfo2 = field(mktag(0), univ2, UNIV_OFFSET_FOR_TYPEINFO);
	save_transient_registers();
	comp = ML_compare_type_info(typeinfo1, typeinfo2);
	restore_transient_registers();
	if (comp != COMPARE_EQUAL) {
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
	index_output = -1;
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
	int comp;

	univ1 = compare_input1;
	univ2 = compare_input2;

	/* First compare the type_infos */
	typeinfo1 = field(mktag(0), univ1, UNIV_OFFSET_FOR_TYPEINFO);
	typeinfo2 = field(mktag(0), univ2, UNIV_OFFSET_FOR_TYPEINFO);
	save_transient_registers();
	comp = ML_compare_type_info(typeinfo1, typeinfo2);
	restore_transient_registers();
	if (comp != COMPARE_EQUAL) {
		compare_output = comp;
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

Define_entry(mercury____Unify___std_util__type_info_0_0);
{
	/*
	** Unification for type_info.
	**
	** The two inputs are in the registers named by unify_input[12].
	** The success/failure indication should go in unify_output.
	*/
	int comp;
	save_transient_registers();
	comp = ML_compare_type_info(unify_input1, unify_input2);
	restore_transient_registers();
	unify_output = (comp == COMPARE_EQUAL);
	proceed();
}

Define_entry(mercury____Index___std_util__type_info_0_0);
	index_output = -1;
	proceed();

Define_entry(mercury____Compare___std_util__type_info_0_0);
{
	/*
	** Comparison for type_info:
	**
	** The two inputs are in the registers named by compare_input[12].
	** The result should go in compare_output.
	*/
	int comp;
	save_transient_registers();
	comp = ML_compare_type_info(unify_input1, unify_input2);
	restore_transient_registers();
	compare_output = comp;
	proceed();
}

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

:- pragma c_header_code("

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
bool	ML_typecheck_arguments(Word type_info, int arity, 
				Word arg_list, Word* arg_vector);
Word 	ML_collapse_equivalences(Word maybe_equiv_type_info);
Word 	ML_make_type(int arity, Word *base_type_info, Word arg_type_list);

").


	% A type_ctor_info is represented as a pointer to a base_type_info.
	% XXX what about higher-order types?
	%     For them the type_ctor_info should include the arity, but
	%     the arity is stored in the type_info, not the base_type_info.
:- type type_ctor_info == c_pointer.  

:- pragma c_code(type_of(Value::unused) = (TypeInfo::out),
	will_not_call_mercury, " 
{
	/* 
	** `Value' isn't used in this c_code, but the compiler
	** gives a warning if you don't mention it.
	*/ 

	TypeInfo = TypeInfo_for_T;

	/*
	** We used to collapse equivalences for efficiency here,
	** but that's not always desirable, due to the reverse
	** mode of make_type/2, and efficiency of type_infos
	** probably isn't very important anyway.
	*/
#if 0
	save_transient_registers();
	TypeInfo = ML_collapse_equivalences(TypeInfo_for_T);
	restore_transient_registers();
#endif

}
").

type_name(Type) = TypeName :-
	type_ctor_and_args(Type, TypeCtor, ArgTypes),
	type_ctor_name_and_arity(TypeCtor, Name, Arity),
	( Arity = 0 ->
		TypeName = Name
	;
		type_arg_names(ArgTypes, ArgTypeNames),
		string__append_list([Name, "(" | ArgTypeNames], TypeName)
	).

:- pred type_arg_names(list(type_info), list(string)).
:- mode type_arg_names(in, out) is det.

type_arg_names([], []).
type_arg_names([Type|Types], ArgNames) :-
	Name = type_name(Type),
	( Types = [] ->
		ArgNames = [Name, ")"]
	;
		type_arg_names(Types, Names),
		ArgNames = [Name, ", " | Names]
	).

type_args(Type) = ArgTypes :-
	type_ctor_and_args(Type, _TypeCtor, ArgTypes).

type_ctor_name(TypeCtor) = Name :-
	type_ctor_name_and_arity(TypeCtor, Name, _Arity).

type_ctor_arity(TypeCtor) = Arity :-
	type_ctor_name_and_arity(TypeCtor, _Name, Arity).

det_make_type(TypeCtor, ArgTypes) = Type :-
	( make_type(TypeCtor, ArgTypes) = NewType ->
		Type = NewType
	;
		error("det_make_type/2: make_type/2 failed (wrong arity)")
	).

:- pragma c_code(type_ctor(TypeInfo::in) = (TypeCtor::out), 
	will_not_call_mercury, "
{
	Word *type_info;

	save_transient_registers();
	type_info = (Word *) ML_collapse_equivalences(TypeInfo);
	restore_transient_registers();

	TypeCtor = (Word) MR_TYPEINFO_GET_BASE_TYPEINFO(type_info);
}
").

:- pragma c_code(type_ctor_and_args(TypeInfo::in,
		TypeCtor::out, TypeArgs::out), will_not_call_mercury, "
{
	Word *type_info;
	Word *base_type_info;
	Integer arity;

	save_transient_registers();
	type_info = (Word *) ML_collapse_equivalences(TypeInfo);
	base_type_info = MR_TYPEINFO_GET_BASE_TYPEINFO(type_info);
	TypeCtor = (Word) base_type_info;
	arity = MR_BASE_TYPEINFO_GET_TYPE_ARITY(base_type_info);
	TypeArgs = ML_copy_argument_typeinfos(arity, 0,
			type_info + OFFSET_FOR_ARG_TYPE_INFOS);
	restore_transient_registers();
}
").

	/*
	** This is the forwards mode of make_type/2:
	** given a type constructor and a list of argument
	** types, check that the length of the argument
	** types matches the arity of the type constructor,
	** and if so, use the type constructor to construct
	** a new type with the specified arguments.
	*/

:- pragma c_code(make_type(TypeCtor::in, ArgTypes::in) = (Type::out),
		will_not_call_mercury, "
{
	int list_length, arity;
	Word arg_type;
	Word *base_type_info;
	
	base_type_info = (Word *) TypeCtor;

	arity = MR_BASE_TYPEINFO_GET_TYPE_ARITY(base_type_info);

	arg_type = ArgTypes; 
	for (list_length = 0; !list_is_empty(arg_type); list_length++) {
		arg_type = list_tail(arg_type);
	}

	if (list_length != arity) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		save_transient_registers();
		Type = ML_make_type(arity, base_type_info, ArgTypes);
		restore_transient_registers();
		SUCCESS_INDICATOR = TRUE;
	}
}
").

	/*
	** This is the reverse mode of make_type: given a type,
	** split it up into a type constructor and a list of
	** arguments.
	*/

:- pragma c_code(make_type(TypeCtor::out, ArgTypes::out) = (TypeInfo::in),
		will_not_call_mercury, "
{
	Word *type_info = (Word *) TypeInfo;
	Word *base_type_info = MR_TYPEINFO_GET_BASE_TYPEINFO(type_info);
	Integer arity = MR_BASE_TYPEINFO_GET_TYPE_ARITY(base_type_info);
	TypeCtor = (Word) base_type_info;
	save_transient_registers();
	ArgTypes = ML_copy_argument_typeinfos(arity, 0,
			type_info + OFFSET_FOR_ARG_TYPE_INFOS);
	restore_transient_registers();
}
").

:- pragma c_code(type_ctor_name_and_arity(TypeCtor::in,
	TypeCtorName::out, TypeCtorArity::out), will_not_call_mercury, "
{
	Word *base_type_info = (Word *) TypeCtor;
	TypeCtorName = MR_BASE_TYPEINFO_GET_TYPE_NAME(base_type_info);
	TypeCtorArity = MR_BASE_TYPEINFO_GET_TYPE_ARITY(base_type_info);
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
	Word 	layout_entry, new_data, term_vector;
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

:- pragma c_code("

	/* 
	** Prototypes
	*/

static int 	ML_get_functor_info(Word type_info, int functor_number, 
				ML_Construct_Info *info);

	/*
	** ML_get_functor_info:
	**
	** Extract the information for functor number `functor_number',
	** for the type represented by type_info.
	** We succeed if the type is some sort of discriminated union.
	**
	** You need to save and restore transient registers around
	** calls to this function.
	*/

int 
ML_get_functor_info(Word type_info, int functor_number, ML_Construct_Info *info)
{
	Word *base_type_functors;

	base_type_functors = MR_BASE_TYPEINFO_GET_TYPEFUNCTORS(
		MR_TYPEINFO_GET_BASE_TYPEINFO((Word *) type_info));

	info->vector_type = MR_TYPEFUNCTORS_INDICATOR(base_type_functors);

	switch (info->vector_type) {

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
		return ML_get_functor_info((Word)
				ML_create_type_info((Word *) type_info, 
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
	** same type; if so, returns TRUE, otherwise returns FALSE;
	** `arg_vector' may contain type variables, these
	** will be filled in by the type arguments of `type_info'.
	**
	** Assumes the length of the list has already been checked.
	**
	** You need to save and restore transient registers around
	** calls to this function.
	*/

bool
ML_typecheck_arguments(Word type_info, int arity, Word arg_list,
		Word* arg_vector) 
{
	int i, comp;
	Word arg_type_info, list_arg_type_info;

		/* Type check list of arguments */

	for (i = 0; i < arity; i++) {
		if (list_is_empty(arg_list)) {
			return FALSE;
		}
		list_arg_type_info = field(0, list_head(arg_list), 
			UNIV_OFFSET_FOR_TYPEINFO);

		arg_type_info = (Word) ML_create_type_info(
			(Word *) type_info, (Word *) arg_vector[i]);

		comp = ML_compare_type_info(list_arg_type_info, arg_type_info);
		if (comp != COMPARE_EQUAL) {
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
	** ML_make_type(arity, base_type_info, arg_types_list):
	**
	** Construct and return a type_info for a type using the
	** specified base_type_info for the type constructor,
	** and using the arguments specified in arg_types_list
	** for the type arguments (if any).
	**
	** Assumes that the arity of the type constructor represented
	** by base_type_info and the length of the arg_types_list 
	** are both equal to `arity'.
	**
	** You need to save and restore transient registers around
	** calls to this function.
	*/

Word
ML_make_type(int arity, Word *base_type_info, Word arg_types_list) 
{
	int i;

	/*
	** XXX: do we need to treat higher-order predicates as
	**      a special case here?
	*/


	if (arity == 0) {
		return (Word) base_type_info;
	} else {
		Word *type_info;

		restore_transient_registers();
		incr_hp(LVALUE_CAST(Word, type_info), arity + 1);
		save_transient_registers();
		
		field(mktag(0), type_info, 0) = (Word) base_type_info;
		for (i = 0; i < arity; i++) {
			field(mktag(0), type_info, i + 1) = 
				list_head(arg_types_list);
			arg_types_list = list_tail(arg_types_list);
		}

		return (Word) type_info;
	}
}


	/*
	** ML_get_functors_check_range:
	**
	** Check that functor_number is in range, and get the functor
	** info if it is. Return FALSE if it is out of range, or
	** if ML_get_functor_info returns FALSE, otherwise return TRUE.
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
		ML_get_functor_info(type_info, functor_number, info);
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
		argument = (Word) ML_create_type_info(
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
		equiv_type_info = (Word) ML_create_type_info(
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
					ML_create_type_info((Word *) 
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


:- pragma c_header_code("

	#include <stdio.h>

	/* 
	 * Code for functor, arg and deconstruct
	 * 
	 * This relies on some C primitives that take a type_info
	 * and a data_word, and get a functor, arity, argument vector,
	 * and argument type_info vector.
	 */

	/* Type definitions */

	/* 
	 * The last two fields, need_functor, and need_args, must
	 * be set by the caller, to indicate whether ML_expand
	 * should copy the functor (if need_functor is non-zero) or
	 * the argument vector and type_info_vector (if need_args is
	 * non-zero). The arity will always be set.
	 *
	 * ML_expand will fill in the other fields (functor, arity,
	 * argument_vector and type_info_vector) accordingly, but
	 * the values of fields not asked for should be assumed to
	 * contain random data when ML_expand returns.
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

void ML_expand(Word* type_info, Word data_word, ML_Expand_Info *info);

Word * ML_create_type_info(Word *term_type_info, Word *arg_pseudo_type_info);

").

:- pragma c_code("

static void ML_expand_const(Word data_value, Word entry_value,
	ML_Expand_Info *info);
static void ML_expand_enum(Word data_value, Word entry_value, 
	ML_Expand_Info *info);
static void ML_expand_simple(Word data_value, Word* arg_type_infos, 
	Word * type_info, ML_Expand_Info *info);
static void ML_expand_builtin(Word data_value, Word entry_value,
	ML_Expand_Info *info);
static void ML_expand_complicated(Word data_value, Word entry_value, 
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
**	ML_expand increments the heap pointer, however, on
**	some platforms the register windows mean that transient
**	Mercury registers may be lost. Before calling ML_expand,
**	call save_transient_registers(), and afterwards, call
**	restore_transient_registers().
**
** 	If writing a C function that calls deep_copy, make sure you
** 	document that around your function, save_transient_registers()
** 	restore_transient_registers() need to be used.
*/

void 
ML_expand(Word* type_info, Word data_word, ML_Expand_Info *info)
{
	Word *base_type_info, *arg_type_info;
	Word data_value, entry_value, base_type_layout_entry;
	int entry_tag, data_tag; 

	base_type_info = MR_TYPEINFO_GET_BASE_TYPEINFO(type_info);

	data_tag = tag(data_word);
	data_value = body(data_word, data_tag);
	
	base_type_layout_entry = MR_BASE_TYPEINFO_GET_TYPELAYOUT_ENTRY(
		base_type_info, data_tag);

	entry_tag = tag(base_type_layout_entry);
	entry_value = body(base_type_layout_entry, entry_tag);
	
	switch(entry_tag) {

	case TYPELAYOUT_CONST_TAG: /* case TYPELAYOUT_COMP_CONST_TAG: */

		/* 
		** This tag represents builtins, enums or complicated
		** constants.
		*/ 

		if (TYPEINFO_IS_VARIABLE(entry_value)) {

			/* 
			** It's a builtin, the rest of the layout 
			** entry value represents the type of builtin.
			*/
			entry_value = unmkbody(entry_value);
			ML_expand_builtin(data_word, entry_value,
				info);
		} else {
			/* It's a complicated constant or enum */
			if (MR_TYPELAYOUT_ENUM_VECTOR_IS_ENUM(entry_value)) {
				ML_expand_enum(data_word, entry_value, 
					info);
			} else {
				data_value = unmkbody(data_value);
				ML_expand_const(data_value, entry_value, 
					info);
			}
		}
		break;

	case TYPELAYOUT_SIMPLE_TAG:
		ML_expand_simple(data_value, (Word *) entry_value, 
			type_info, info);
		break;

	case TYPELAYOUT_COMPLICATED_TAG:
		ML_expand_complicated(data_value, entry_value, type_info,
			info);
		break;

	case TYPELAYOUT_EQUIV_TAG: /* case TYPELAYOUT_NO_TAG: */

			/* 
			** Is it a type variable? 
			*/
		if (TYPEINFO_IS_VARIABLE(entry_value)) {
			arg_type_info = ML_create_type_info(type_info, 
				(Word *) entry_value);
			ML_expand(arg_type_info, data_word, info);
		}
			/* 
			** is it a no_tag type?
			*/
		else if (MR_TYPELAYOUT_NO_TAG_VECTOR_IS_NO_TAG(entry_value)) {
			Word new_arg_vector; 
			incr_saved_hp(new_arg_vector, 1);
			field(0, new_arg_vector, 0) = data_word;
			ML_expand_simple(new_arg_vector, 
				(Word *) entry_value, type_info, info);
		}
			/* 
			** It must be an equivalent type.
			*/
		else {
			arg_type_info = ML_create_type_info(type_info, 
				(Word *) MR_TYPELAYOUT_EQUIV_TYPE(
					entry_value));
			ML_expand(arg_type_info, data_word, info);
		}

		break;

	default:
		/* If this happens, the layout data is corrupt */

		fatal_error(""ML_expand: found unused tag value"");
	}
}

/*
 * Expand a constant value.
 */

void
ML_expand_const(Word data_value, Word entry_value, ML_Expand_Info *info) 
{

	/* the functors are stored after the enum_indicator and
	 * the number of functors
	 */
	info->functor = MR_TYPELAYOUT_ENUM_VECTOR_FUNCTOR_NAME(entry_value,
		data_value);
	info->arity = 0;
	info->argument_vector = NULL;
	info->type_info_vector = NULL;
}


/*
 * Expand an enum.
 */

void
ML_expand_enum(Word data_value, Word enum_vector, ML_Expand_Info *info) 
{
	info->functor = MR_TYPELAYOUT_ENUM_VECTOR_FUNCTOR_NAME(enum_vector,
		data_value);
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
ML_expand_simple(Word data_value, Word* simple_vector, Word * type_info,
	ML_Expand_Info *info)
{
	int i;

	info->arity = MR_TYPELAYOUT_SIMPLE_VECTOR_ARITY(simple_vector);
	
	if (info->need_functor) {
		make_aligned_string(info->functor, 
			MR_TYPELAYOUT_SIMPLE_VECTOR_FUNCTOR_NAME(
				simple_vector));
	}

	if (info->need_args) {
		info->argument_vector = (Word *) data_value;

		info->type_info_vector = 
			checked_malloc(info->arity * sizeof(Word));

		for (i = 0; i < info->arity ; i++) {
			Word *arg_pseudo_type_info;

			arg_pseudo_type_info = (Word *)
				MR_TYPELAYOUT_SIMPLE_VECTOR_ARGS(
					simple_vector)[i];
			info->type_info_vector[i] = (Word) 
				ML_create_type_info(type_info, 
					arg_pseudo_type_info);
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
ML_expand_complicated(Word data_value, Word entry_value, Word * type_info,
	ML_Expand_Info *info)
{
	Word new_data_value, simple_vector, simple_vector_tag, secondary_tag;

	secondary_tag = ((Word *) data_value)[0];
	new_data_value = (Word) ((Word *) data_value + 1);

	simple_vector = MR_TYPELAYOUT_COMPLICATED_VECTOR_GET_SIMPLE_VECTOR(
		entry_value, secondary_tag);
	simple_vector_tag = tag(simple_vector);
	simple_vector = body(simple_vector, simple_vector_tag);

	ML_expand_simple(new_data_value, (Word *) simple_vector, 
		type_info, info);
}

void
ML_expand_builtin(Word data_value, Word entry_value, ML_Expand_Info *info)
{
	switch ((int) entry_value) {
	
	case TYPELAYOUT_UNASSIGNED_VALUE:
		fatal_error(""ML_expand: attempt to use an UNASSIGNED tag"");
		break;

	case TYPELAYOUT_UNUSED_VALUE:
		fatal_error(""ML_expand: attempt to use an UNUSED tag"");
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

		ML_expand((Word *) 
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

	case TYPELAYOUT_VOID_VALUE:
		fatal_error(""ML_expand: found void"");
		break;

	case TYPELAYOUT_UNIQ_ARRAY_VALUE:
		fatal_error(""ML_expand: found uniq_array"");
		break;

	case TYPELAYOUT_TYPEINFO_VALUE:
		fatal_error(""ML_expand: found type_info"");
		break;

	case TYPELAYOUT_C_POINTER_VALUE:
		fatal_error(""ML_expand: found c_pointer"");
		break;
		
		
	default:
		fatal_error(""ML_expand: invalid tag value"");
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
	** to change the code in ML_create_type_info in runtime/deep_copy.c,
	** which does much the same thing, only allocating using malloc
	** instead of on the heap.
	*/

Word * 
ML_create_type_info(Word *term_type_info, Word *arg_pseudo_type_info)
{
	int i, arity;
	Word base_type_info;
	Word *type_info;

		/* 
		** The arg_pseudo_type_info might be a polymorphic variable,
		** if so - substitute.
		*/

	if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info)) {
		arg_pseudo_type_info = (Word *) 
			term_type_info[(Word) arg_pseudo_type_info];
	}

	if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info)) {
		fatal_error(""ML_create_type_info: unbound type variable"");
	}

	base_type_info = arg_pseudo_type_info[0];

		/* no arguments - optimise common case */
	if (base_type_info == 0) {
		return arg_pseudo_type_info;
	}

	arity = MR_BASE_TYPEINFO_GET_TYPE_ARITY(base_type_info);

	incr_saved_hp(LVALUE_CAST(Word, type_info), arity + 1);

	for (i = 0; i <= arity; i++) {
		if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info[i])) {
			type_info[i] = term_type_info[arg_pseudo_type_info[i]];
			if (TYPEINFO_IS_VARIABLE(type_info[i])) {
				fatal_error(""ML_create_type_info: ""
					""unbound type variable"");
			}

		} else {
			type_info[i] = arg_pseudo_type_info[i];
		}
	}
	return type_info;
}

").

%-----------------------------------------------------------------------------%

	% Code for functor, arg and deconstruct.

:- pragma c_code(functor(Type::in, Functor::out, Arity::out),
		will_not_call_mercury, " 
{
	ML_Expand_Info info;

	info.need_functor = TRUE;
	info.need_args = FALSE;

	save_transient_registers();

	ML_expand((Word *) TypeInfo_for_T, Type, &info);

	restore_transient_registers();

		/* Copy functor onto the heap */
	make_aligned_string(LVALUE_CAST(ConstString, Functor), info.functor);

	Arity = info.arity;

}").

:- pragma c_code(argument(Type::in, ArgumentIndex::in) = (Argument::out),
		will_not_call_mercury, " 
{
	ML_Expand_Info info;
	Word arg_pseudo_type_info;
	bool success;

	info.need_functor = FALSE;
	info.need_args = TRUE;

	save_transient_registers();

	ML_expand((Word *) TypeInfo_for_T, Type, &info);

	restore_transient_registers();

		/* Check range */
	success = (ArgumentIndex >= 0 && ArgumentIndex < info.arity);
	if (success) {

			/* Allocate enough room for a univ */
		incr_hp(Argument, 2);
		arg_pseudo_type_info = info.type_info_vector[ArgumentIndex];
		if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info)) {
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

det_argument(Type, ArgumentIndex) = Argument :-
	(
		argument(Type, ArgumentIndex) = Argument0
	->
		Argument = Argument0
	;
		error("det_argument : argument out of range")
	).

:- pragma c_code(deconstruct(Type::in, Functor::out, Arity::out, 
		Arguments::out), will_not_call_mercury, " 
{
	ML_Expand_Info info;
	Word arg_pseudo_type_info;
	Word Argument, tmp;
	int i;

	info.need_functor = TRUE;
	info.need_args = TRUE;

	save_transient_registers();

	ML_expand((Word *) TypeInfo_for_T, Type, &info);
	
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

		if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info)) {

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
