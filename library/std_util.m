%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: std_util.m.
% Main author: fjh.
% Stability: medium.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.

:- interface.

:- import_module list, set, bool.
:- import_module type_desc.

%-----------------------------------------------------------------------------%

% The universal type `univ'.
% An object of type `univ' can hold the type and value of an object of any
% other type.

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

	% det_univ_to_type(Univ, Object):
	% 	the same as the forwards mode of univ_to_type, but
	% 	abort if univ_to_type fails.
	%
:- pred det_univ_to_type(univ, T).
:- mode det_univ_to_type(in, out) is det.

	% univ_type(Univ):
	%	returns the type_desc for the type stored in `Univ'.
	%
:- func univ_type(univ) = type_desc__type_desc.

	% univ_value(Univ):
	%	returns the value of the object stored in Univ.
:- some [T] func univ_value(univ) = T.

%-----------------------------------------------------------------------------%

% The "maybe" type.

:- type maybe(T) ---> no ; yes(T).
:- inst maybe(I) ---> no ; yes(I).

:- type maybe_error ---> ok ; error(string).
:- type maybe_error(T) ---> ok(T) ; error(string).
:- inst maybe_error(I) ---> ok(I) ; error(ground).

	% map_maybe(P, yes(Value0), yes(Value)) :- P(Value, Value).
	% map_maybe(_, no, no).
	%
:- pred map_maybe(pred(T, U), maybe(T), maybe(U)).
:- mode map_maybe(pred(in, out) is det, in, out) is det.
:- mode map_maybe(pred(in, out) is semidet, in, out) is semidet.
:- mode map_maybe(pred(in, out) is multi, in, out) is multi.
:- mode map_maybe(pred(in, out) is nondet, in, out) is nondet.

	% map_maybe(F, yes(Value)) = yes(F(Value)).
	% map_maybe(_, no) = no.
	%
:- func map_maybe(func(T) = U, maybe(T)) = maybe(U).

%-----------------------------------------------------------------------------%

% The "unit" type - stores no information at all.

:- type unit		--->	unit.

%-----------------------------------------------------------------------------%

% The "pair" type.  Useful for many purposes.

:- type pair(T1, T2)	--->	(T1 - T2).
:- type pair(T)		==	pair(T,T).
:- inst pair(I1, I2)	--->	(I1 - I2).
:- inst pair(I)		==	pair(I,I).

	% Return the first element of the pair.
:- pred fst(pair(X,Y)::in, X::out) is det.
:- func fst(pair(X,Y)) = X.

	% Return the second element of the pair.
:- pred snd(pair(X,Y)::in, Y::out) is det.
:- func snd(pair(X,Y)) = Y.

:- func pair(T1, T2) = pair(T1, T2).

%-----------------------------------------------------------------------------%

% solutions/2 collects all the solutions to a predicate and
% returns them as a list in sorted order, with duplicates removed.
% solutions_set/2 returns them as a set.
% unsorted_solutions/2 returns them as an unsorted list with possible
% duplicates; since there are an infinite number of such lists,
% this must be called from a context in which only a single solution
% is required.

:- pred solutions(pred(T), list(T)).
:- mode solutions(pred(out) is multi, out(non_empty_list)) is det.
:- mode solutions(pred(out) is nondet, out) is det.

:- func solutions(pred(T)) = list(T).
:- mode solutions(pred(out) is multi) = out(non_empty_list) is det.
:- mode solutions(pred(out) is nondet) = out is det.

:- pred solutions_set(pred(T), set(T)).
:- mode solutions_set(pred(out) is multi, out) is det.
:- mode solutions_set(pred(out) is nondet, out) is det.

:- func solutions_set(pred(T)) = set(T).
:- mode solutions_set(pred(out) is multi) = out is det.
:- mode solutions_set(pred(out) is nondet) = out is det.

:- pred unsorted_solutions(pred(T), list(T)).
:- mode unsorted_solutions(pred(out) is multi, out(non_empty_list)) 
	is cc_multi.
:- mode unsorted_solutions(pred(out) is nondet, out) is cc_multi.

:- func aggregate(pred(T), func(T, U) = U, U) = U.
:- mode aggregate(pred(out) is multi, func(in, in) = out is det,
		in) = out is det.
:- mode aggregate(pred(out) is nondet, func(in, in) = out is det,
		in) = out is det.

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

	% aggregate2/6 generates all the solutions to a predicate,
	% sorts them and removes duplicates, then applies an accumulator
	% predicate to each solution in turn:
	%
	% aggregate2(Generator, Accumulator, AccA0, AccA, AccB0, AccB) <=>
	%	solutions(Generator, Solutions),
	%	list__foldl2(Accumulator, Solutions, AccA0, AccA, AccB0, AccB).
	%

:- pred aggregate2(pred(T), pred(T, U, U, V, V), U, U, V, V).
:- mode aggregate2(pred(out) is multi, pred(in, in, out, in, out) is det,
		in, out, in, out) is det.
:- mode aggregate2(pred(out) is multi, pred(in, in, out, di, uo) is det,
		in, out, di, uo) is det.
:- mode aggregate2(pred(out) is nondet, pred(in, in, out, di, uo) is det,
		in, out, di, uo) is det.
:- mode aggregate2(pred(out) is nondet, pred(in, in, out, in, out) is det,
		in, out, in, out) is det.

	% unsorted_aggregate/4 generates all the solutions to a predicate
	% and applies an accumulator predicate to each solution in turn.
	% Declaratively, the specification is as follows:
	%
	% unsorted_aggregate(Generator, Accumulator, Acc0, Acc) <=>
	%	unsorted_solutions(Generator, Solutions),
	%	list__foldl(Accumulator, Solutions, Acc0, Acc).
	%
	% Operationally, however, unsorted_aggregate/4 will call the
	% Accumulator for each solution as it is obtained, rather than
	% first building a list of all the solutions.

:- pred unsorted_aggregate(pred(T), pred(T, U, U), U, U).
:- mode unsorted_aggregate(pred(out) is multi, pred(in, in, out) is det,
		in, out) is cc_multi.
:- mode unsorted_aggregate(pred(out) is multi, pred(in, di, uo) is det,
		di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(muo) is multi, pred(mdi, di, uo) is det,
		di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, di, uo) is det,
		di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, in, out) is det,
		in, out) is cc_multi.
:- mode unsorted_aggregate(pred(muo) is nondet, pred(mdi, di, uo) is det,
		di, uo) is cc_multi.

	% This is a generalization of unsorted_aggregate which allows the
	% iteration to stop before all solutions have been found.
	% Declaratively, the specification is as follows:
	%
	%	do_while(Generator, Filter) -->
	%		{ unsorted_solutions(Generator, Solutions) },
	%		do_while_2(Solutions, Filter).
	%
	%	do_while_2([], _) --> [].
	%	do_while_2([X|Xs], Filter) -->
	%		Filter(X, More),
	%		(if { More = yes } then
	%			do_while_2(Xs, Filter)
	%		else
	%			{ true }
	%		).
	%
	% Operationally, however, do_while/4 will call the Filter
	% predicate for each solution as it is obtained, rather than
	% first building a list of all the solutions.
	%
:- pred do_while(pred(T), pred(T, bool, T2, T2), T2, T2).
:- mode do_while(pred(out) is multi, pred(in, out, in, out) is det, in, out)
	is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, in, out) is det, in, out)
	is cc_multi.
:- mode do_while(pred(out) is multi, pred(in, out, di, uo) is det, di, uo)
	is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, di, uo) is det, di, uo)
	is cc_multi.

%-----------------------------------------------------------------------------%

	% General purpose higher-order programming constructs.

	% compose(F, G, X) = F(G(X))
	%
	% Function composition.
	% XXX It would be nice to have infix `o' or somesuch for this.
:- func compose(func(T2) = T3, func(T1) = T2, T1) = T3.

	% converse(F, X, Y) = F(Y, X)
:- func converse(func(T1, T2) = T3, T2, T1) = T3.

	% pow(F, N, X) = F^N(X)
	%
	% Function exponentiation.
:- func pow(func(T) = T, int, T) = T.

	% The identity function.
	%
:- func id(T) = T.

%-----------------------------------------------------------------------------%

	% maybe_pred(Pred, X, Y) takes a closure Pred which transforms an
	% input semideterministically. If calling the closure with the input
	% X succeeds, Y is bound to `yes(Z)' where Z is the output of the
	% call, or to `no' if the call fails.
	%
:- pred maybe_pred(pred(T1, T2), T1, maybe(T2)).
:- mode maybe_pred(pred(in, out) is semidet, in, out) is det.

:- func maybe_func(func(T1) = T2, T1) = maybe(T2).
:- mode maybe_func(func(in) = out is semidet, in) = out is det.

%-----------------------------------------------------------------------------%

	% isnt(Pred, X) <=> not Pred(X)
	%
	% This is useful in higher order programming, e.g.
	% 	Odds  = list__filter(odd, Xs)
	% 	Evens = list__filter(isnt(odd), Xs)
	%
:- pred isnt(pred(T), T).
:- mode isnt(pred(in) is semidet, in) is semidet.

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

:- pred cc_multi_equal(T, T).
:- mode cc_multi_equal(di, uo) is cc_multi.
:- mode cc_multi_equal(in, out) is cc_multi.

%-----------------------------------------------------------------------------%

	% The `type_desc' and `type_ctor_desc' types: these
	% provide access to type information.
	% A type_desc represents a type, e.g. `list(int)'.
	% A type_ctor_desc represents a type constructor, e.g. `list/1'.

:- type type_desc == type_desc__type_desc.
:- type type_ctor_desc == type_desc__type_ctor_desc.

	% Type_info and type_ctor_info are the old names for type_desc and
	% type_ctor_desc. They should not be used by new software.

:- type type_info == type_desc__type_desc.
:- type type_ctor_info == type_desc__type_ctor_desc.

	% (Note: it is not possible for the type of a variable to be an
	% unbound type variable; if there are no constraints on a type
	% variable, then the typechecker will use the type `void'.
	% `void' is a special (builtin) type that has no constructors.
	% There is no way of creating an object of type `void'.
	% `void' is not considered to be a discriminated union, so
	% get_functor/5 and construct/3 will fail if used upon a value
	% of this type.)

	% The function type_of/1 returns a representation of the type
	% of its argument.
	%
:- func type_of(T) = type_desc__type_desc.
:- mode type_of(unused) = out is det.

	% The predicate has_type/2 is basically an existentially typed
	% inverse to the function type_of/1.  It constrains the type
	% of the first argument to be the type represented by the
	% second argument.
:- some [T] pred has_type(T::unused, type_desc__type_desc::in) is det.

	% type_name(Type) returns the name of the specified type
	% (e.g. type_name(type_of([2,3])) = "list:list(int)").
	% Any equivalence types will be fully expanded.
	% Builtin types (those defined in builtin.m) will
	% not have a module qualifier.
	%
:- func type_name(type_desc__type_desc) = string.

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
:- pred type_ctor_and_args(type_desc__type_desc, type_desc__type_ctor_desc,
	list(type_desc__type_desc)).
:- mode type_ctor_and_args(in, out, out) is det.

	% type_ctor(Type) = TypeCtor :-
	%	type_ctor_and_args(Type, TypeCtor, _).
	%
:- func type_ctor(type_desc__type_desc) = type_desc__type_ctor_desc.

	% type_args(Type) = TypeArgs :-
	%	type_ctor_and_args(Type, _, TypeArgs).
	%
:- func type_args(type_desc__type_desc) = list(type_desc__type_desc).

	% type_ctor_name(TypeCtor) returns the name of specified
	% type constructor.
	% (e.g. type_ctor_name(type_ctor(type_of([2,3]))) = "list").
	%
:- func type_ctor_name(type_desc__type_ctor_desc) = string.

	% type_ctor_module_name(TypeCtor) returns the module name of specified
	% type constructor.
	% (e.g. type_ctor_module_name(type_ctor(type_of(2))) = "builtin").
	%
:- func type_ctor_module_name(type_desc__type_ctor_desc) = string.

	% type_ctor_arity(TypeCtor) returns the arity of specified
	% type constructor.
	% (e.g. type_ctor_arity(type_ctor(type_of([2,3]))) = 1).
	%
:- func type_ctor_arity(type_desc__type_ctor_desc) = int.

	% type_ctor_name_and_arity(TypeCtor, ModuleName, TypeName, Arity) :-
	%	Name = type_ctor_name(TypeCtor),
	%	ModuleName = type_ctor_module_name(TypeCtor),
	%	Arity = type_ctor_arity(TypeCtor).
	%
:- pred type_ctor_name_and_arity(type_desc__type_ctor_desc::in, string::out,
	string::out, int::out) is det.

	% make_type(TypeCtor, TypeArgs) = Type:
	%	True iff `Type' is a type constructed by applying
	%	the type constructor `TypeCtor' to the type arguments
	%	`TypeArgs'.
	%
	% Operationally, the forwards mode returns the type formed by
	% applying the specified type constructor to the specified
	% argument types, or fails if the length of TypeArgs is not the
	% same as the arity of TypeCtor.  The reverse mode returns a
	% type constructor and its argument types, given a type_desc;
	% the type constructor returned may be an equivalence type
	% (and hence this reverse mode of make_type/2 may be more useful
	% for some purposes than the type_ctor/1 function).
	%
:- func make_type(type_desc__type_ctor_desc, list(type_desc__type_desc)) =
	type_desc__type_desc.
:- mode make_type(in, in) = out is semidet.
:- mode make_type(out, out) = in is cc_multi.

	% det_make_type(TypeCtor, TypeArgs):
	%
	% Returns the type formed by applying the specified type
	% constructor to the specified argument types.  Aborts if the
	% length of `TypeArgs' is not the same as the arity of `TypeCtor'.
	%
:- func det_make_type(type_desc__type_ctor_desc, list(type_desc__type_desc)) =
	type_desc__type_desc.
:- mode det_make_type(in, in) = out is det.

%-----------------------------------------------------------------------------%

	% num_functors(TypeInfo)
	%
	% Returns the number of different functors for the top-level
	% type constructor of the type specified by TypeInfo, or -1
	% if the type is not a discriminated union type.
	%
	% The functors of a discriminated union type are numbered from
	% zero to N-1, where N is the value returned by num_functors.
	% The functors are numbered in lexicographic order. If two
	% functors have the same name, the one with the lower arity
	% will have the lower number.
	%
:- func num_functors(type_desc__type_desc) = int.

	% get_functor(Type, FunctorNumber, FunctorName, Arity, ArgTypes)
	%
	% Binds FunctorName and Arity to the name and arity of functor number
	% FunctorNumber for the specified type, and binds ArgTypes to the
	% type_descs for the types of the arguments of that functor.
	% Fails if the type is not a discriminated union type, or if
	% FunctorNumber is out of range.
	%
:- pred get_functor(type_desc__type_desc::in, int::in, string::out, int::out,
		list(type_desc__type_desc)::out) is semidet.

	% get_functor(Type, FunctorNumber, FunctorName, Arity, ArgTypes,
	%	ArgNames)
	%
	% Binds FunctorName and Arity to the name and arity of functor number
	% FunctorNumber for the specified type, ArgTypes to the type_descs
	% for the types of the arguments of that functor, and ArgNames to the
	% field name of each functor argument, if any.  Fails if the type is
	% not a discriminated union type, or if FunctorNumber is out of range.
	%
:- pred get_functor(type_desc__type_desc::in, int::in, string::out, int::out,
		list(type_desc__type_desc)::out, list(maybe(string))::out)
		is semidet.

	% get_functor_ordinal(Type, I, Ordinal)
	%
	% Returns Ordinal, where Ordinal is the position in declaration order
	% for the specified type of the function symbol that is in position I
	% in lexicographic order. Fails if the type is not a discriminated
	% union type, or if I is out of range.
:- pred get_functor_ordinal(type_desc__type_desc::in, int::in, int::out)
	is semidet.

	% construct(TypeInfo, I, Args) = Term
	%
	% Returns a term of the type specified by TypeInfo whose functor
	% is functor number I of the type given by TypeInfo, and whose
	% arguments are given by Args.  Fails if the type is not a
	% discriminated union type, or if I is out of range, or if the
	% number of arguments supplied doesn't match the arity of the selected
	% functor, or if the types of the arguments do not match
	% the expected argument types of that functor.
	%
:- func construct(type_desc__type_desc, int, list(univ)) = univ.
:- mode construct(in, in, in) = out is semidet.

	% construct_tuple(Args) = Term
	%
	% Returns a tuple whose arguments are given by Args.
:- func construct_tuple(list(univ)) = univ.

%-----------------------------------------------------------------------------%

	% functor, argument and deconstruct and their variants take any type
	% (including univ), and return representation information for that type.
	%
	% The string representation of the functor that these predicates
	% return is:
	%
	% 	- for user defined types, the functor that is given
	% 	  in the type definition. For lists, this
	% 	  means the functors [|]/2 and []/0 are used, even if
	% 	  the list uses the [....] shorthand.
	%	- for integers, the string is a base 10 number,
	%	  positive integers have no sign.
	%	- for floats, the string is a floating point,
	%	  base 10 number, positive floating point numbers have
	%	  no sign.
	%	- for strings, the string, inside double quotation marks
	%	- for characters, the character inside single quotation marks
	%	- for predicates, the string <<predicate>>
	%	- for functions, the string <<function>>
	%	- for tuples, the string {}
	%	- for arrays, the string <<array>>
	%
	% The arity that these predicates return is:
	%
	% 	- for user defined types, the arity of the functor.
	%	- for integers, zero.
	%	- for floats, zero.
	%	- for strings, zero.
	%	- for characters, zero.
	%	- for predicates and functions, zero; we do not return the
	%	  number of arguments expected by the predicate or function.
	%	- for tuples, the number of elements in the tuple.
	%	- for arrays, the number of elements in the array.

	% functor(Data, Functor, Arity)
	%
	% Given a data item (Data), binds Functor to a string
	% representation of the functor and Arity to the arity of this
	% data item.  (Aborts if the type of Data is a type with a
	% non-canonical representation, i.e. one for which there is a
	% user-defined equality predicate.)
	%
	% Functor_cc succeeds even if the first argument is of a
	% non-canonical type.
	%
:- pred functor(T::in, string::out, int::out) is det.
:- pred functor_cc(T::in, string::out, int::out) is cc_multi.

	% arg(Data, ArgumentIndex) = Argument
	% argument(Data, ArgumentIndex) = ArgumentUniv
	%
	% Given a data item (Data) and an argument index
	% (ArgumentIndex), starting at 0 for the first argument, binds
	% Argument to that argument of the functor of the data item. If
	% the argument index is out of range -- that is, greater than or
	% equal to the arity of the functor or lower than 0 -- then
	% the call fails.  For argument/2 the argument returned has the
	% type univ, which can store any type.  For arg/2, if the
	% argument has the wrong type, then the call fails.
	% (Both abort if the type of Data is a type with a non-canonical
	% representation, i.e. one for which there is a user-defined
	% equality predicate.)
	%
	% arg_cc and argument_cc succeed even if the first argument is
	% of a non-canonical type.
	%
:- func arg(T::in, int::in) = (ArgT::out) is semidet.
:- pred arg_cc(T::in, int::in, ArgT::out) is cc_nondet.
:- func argument(T::in, int::in) = (univ::out) is semidet.
:- pred argument_cc(T::in, int::in, univ::out) is cc_nondet.

	% named_argument(Data, ArgumentName) = ArgumentUniv
	%
	% Same as argument/2, except the chosen argument is specified by giving
	% its name rather than its position. If Data has no argument with that
	% name, named_argument fails.
	%
	% named_argument_cc succeeds even if the first argument is
	% of a non-canonical type.
	%
:- func named_argument(T::in, string::in) = (univ::out) is semidet.
:- pred named_argument_cc(T::in, string::in, univ::out) is cc_nondet.

	% det_arg(Data, ArgumentIndex) = Argument
	% det_argument(Data, ArgumentIndex) = ArgumentUniv
	%
	% Same as arg/2 and argument/2 respectively, except that
	% for cases where arg/2 or argument/2 would fail,
	% det_arg/2 or det_argument/2 will abort.
	%
	% det_arg_cc and det_argument_cc succeed even if the first argument is
	% of a non-canonical type.
	%
:- func det_arg(T::in, int::in) = (ArgT::out) is det.
:- pred det_arg_cc(T::in, int::in, ArgT::out) is cc_multi.
:- func det_argument(T::in, int::in) = (univ::out) is det.
:- pred det_argument_cc(T::in, int::in, univ::out) is cc_multi.

	% det_named_argument(Data, ArgumentName) = ArgumentUniv
	%
	% Same as named_argument/2, except that for cases where
	% named_argument/2 would fail, det_named_argument/2 will abort.
	%
:- func det_named_argument(T::in, string::in) = (univ::out) is det.
:- pred det_named_argument_cc(T::in, string::in, univ::out) is cc_multi.

	% deconstruct(Data, Functor, Arity, Arguments)
	%
	% Given a data item (Data), binds Functor to a string
	% representation of the functor, Arity to the arity of this data
	% item, and Arguments to a list of arguments of the functor.
	% The arguments in the list are each of type univ.
	% (Aborts if the type of Data is a type with a non-canonical
	% representation, i.e. one for which there is a user-defined
	% equality predicate.)
	%
	% The cost of calling deconstruct depends greatly on how many arguments
	% Data has. If Data is an array, then each element of the array is
	% considered one of its arguments. Therefore calling deconstruct
	% on large arrays can take a very large amount of memory and a very
	% long time. If you call deconstruct in a situation in which you may
	% pass it a large array, you should probably use limited_deconstruct
	% instead.
	%
	% deconstruct_cc succeeds even if the first argument is
	% of a non-canonical type.
	%
:- pred deconstruct(T::in, string::out, int::out, list(univ)::out) is det.
:- pred deconstruct_cc(T::in, string::out, int::out, list(univ)::out)
	is cc_multi.

	% limited_deconstruct(Data, MaxArity, Functor, Arity, Arguments)
	%
	% limited_deconstruct works like deconstruct, but if the arity of T is
	% greater than MaxArity, limited_deconstruct fails. This is useful in
	% avoiding bad performance in cases where Data may be a large array.
	%
	% limited_deconstruct_cc succeeds even if the first argument is
	% of a non-canonical type.
	%
:- pred limited_deconstruct(T::in, int::in, string::out,
	int::out, list(univ)::out) is semidet.
:- pred limited_deconstruct_cc(T::in, int::in, string::out,
	int::out, list(univ)::out) is cc_nondet.

%-----------------------------------------------------------------------------%

:- implementation.
:- interface.

% The rest of the interface is for use by implementors only.

	% dynamic_cast(X, Y) succeeds with Y = X iff X has the same
	% ground type as Y (so this may succeed if Y is of type
	% list(int), say, but not if Y is of type list(T)).
	%
:- pred dynamic_cast(T1::in, T2::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, set, int, string, bool.
:- import_module construct, deconstruct.

% XXX This should not be necessary, but the current compiler is broken in that
% it puts foreign_proc clauses into deconstruct.opt without also putting the
% foreign_decl they require into deconstruct.opt as well.

:- pragma foreign_decl("C", "

#include ""mercury_deconstruct.h""
#include ""mercury_deconstruct_macros.h""

").

%-----------------------------------------------------------------------------%

map_maybe(_, no, no).
map_maybe(P, yes(T0), yes(T)) :- P(T0, T).

map_maybe(_, no) = no.
map_maybe(F, yes(T)) = yes(F(T)).

/****
	Is this really useful?
% for use in lambda expressions where the type of functor '-' is ambiguous
:- pred pair(X, Y, pair(X, Y)).
:- mode pair(in, in, out) is det.
:- mode pair(out, out, in) is det.

pair(X, Y, X-Y).
****/
fst(X-_Y) = X.
fst(P,X) :-
	X = fst(P).

snd(_X-Y) = Y.
snd(P,X) :-
	X = snd(P).

maybe_pred(Pred, X, Y) :-
	(
		call(Pred, X, Z)
	->
		Y = yes(Z)
	;
		Y = no
	).

%-----------------------------------------------------------------------------%

/*
** This section defines builtin_aggregate/4 which takes a closure of type
** pred(T) in which the remaining argument is output, and backtracks over
** solutions for this, using the second argument to aggregate them however the
** user wishes.  This is basically a generalization of solutions/2.
*/

:- pred builtin_aggregate(pred(T), pred(T, U, U), U, U).
:- mode builtin_aggregate(pred(out) is multi, pred(in, in, out) is det,
		in, out) is det. /* really cc_multi */
:- mode builtin_aggregate(pred(out) is multi, pred(in, di, uo) is det,
		di, uo) is det. /* really cc_multi */
:- mode builtin_aggregate(pred(muo) is multi, pred(mdi, di, uo) is det,
		di, uo) is det. /* really cc_multi */
:- mode builtin_aggregate(pred(out) is nondet, pred(in, di, uo) is det,
		di, uo) is det. /* really cc_multi */
:- mode builtin_aggregate(pred(out) is nondet, pred(in, in, out) is det,
		in, out) is det. /* really cc_multi */
:- mode builtin_aggregate(pred(muo) is nondet, pred(mdi, di, uo) is det,
		di, uo) is det. /* really cc_multi */

/*
** In order to implement any sort of code that requires terms to survive
** backtracking, we need to (deeply) copy them out of the heap and into some
** other area before backtracking.  The obvious thing to do then is just call
** the generator predicate, let it run to completion, and copy its result into
** another memory area (call it the solutions heap) before forcing
** backtracking.  When we get the next solution, we do the same, this time
** passing the previous collection (which is still on the solutions heap) to
** the collector predicate.  If the result of this operation contains the old
** collection as a part, then the deep copy operation is smart enough
** not to copy again.  So this could be pretty efficient.
**
** But what if the collector predicate does something that copies the previous
** collection?  Then on each solution, we'll copy the previous collection to
** the heap, and then deep copy it back to the solution heap.  This means
** copying solutions order N**2 times, where N is the number of solutions.  So
** this isn't as efficient as we hoped.
**
** So we use a slightly different approach.  When we find a solution, we deep
** copy it to the solution heap.  Then, before calling the collector code, we
** sneakily swap the runtime system's notion of which is the heap and which is
** the solutions heap.  This ensures that any terms are constructed on the
** solutions heap.  When this is complete, we swap them back, and force the
** engine to backtrack to get the next solution.  And so on.  After we've
** gotten the last solution, we do another deep copy to move the solution back
** to the 'real' heap, and reset the solutions heap pointer (which of course
** reclaims all the garbage of the collection process).
**
** Note that this will work with recursive calls to builtin_aggregate as
** well.  If the recursive invocation occurs in the generator pred, there can
** be no problem because by the time the generator succeeds, the inner
** do_ call will have completed, copied its result from the solutions heap,
** and reset the solutions heap pointer.  If the recursive invocation happens
** in the collector pred, then it will happen when the heap and solutions heap
** are 'swapped.'  This will work out fine, because the real heap isn't needed
** while the collector pred is executing, and by the time the nested do_ is
** completed, the 'real' heap pointer will have been reset.
**
** If the collector predicate throws an exception while they are swapped,
** then the code for builtin_throw/1 will unswap the heaps.
** So we don't need to create our own exception handlers to here to
** cover that case.
**
** If we're using conservative GC, then all of the heap-swapping
** and copying operations are no-ops, so we get a "zero-copy" solution.
*/

% Note that the code for builtin_aggregate is very similar to the code
% for do_while (below).

:- pragma promise_pure(builtin_aggregate/4).
builtin_aggregate(GeneratorPred, CollectorPred, Accumulator0, Accumulator) :-
	% Save some of the Mercury virtual machine registers
	impure get_registers(HeapPtr, SolutionsHeapPtr, TrailPtr),

	% Initialize the accumulator
	% /* Mutvar := Accumulator0 */
	impure new_mutvar(Accumulator0, Mutvar),

	(
		% Get a solution
		GeneratorPred(Answer0),

		% Check that the generator didn't leave any
		% delayed goals outstanding
		impure check_for_floundering(TrailPtr),

		% Update the accumulator
		% /* MutVar := CollectorPred(MutVar) */
		impure swap_heap_and_solutions_heap,
		impure partial_deep_copy(HeapPtr, Answer0, Answer),
		impure get_mutvar(Mutvar, Acc0),
		CollectorPred(Answer, Acc0, Acc1),
		impure set_mutvar(Mutvar, Acc1),
		impure swap_heap_and_solutions_heap,

		% Force backtracking, so that we get the next solution.
		% This will automatically reset the heap and trail.
		fail
	;
		% There are no more solutions.
		% So now we just need to copy the final value
		% of the accumulator from the solutions heap
		% back onto the ordinary heap, and then we can
		% reset the solutions heap pointer.
		% We also need to discard the trail ticket
		% created by get_registers/3.
		% /* Accumulator := MutVar */
		impure get_mutvar(Mutvar, Accumulator1),
		impure partial_deep_copy(SolutionsHeapPtr, Accumulator1,
			Accumulator),
		impure reset_solutions_heap(SolutionsHeapPtr),
		impure discard_trail_ticket
	).

% The code for do_while/4 is essentially the same as the code for
% builtin_aggregate (above).  See the detailed comments above.
%
% XXX It would be nice to avoid the code duplication here,
% but it is a bit tricky -- we can't just use a lambda expression,
% because we'd need to specify the mode, but we want it to work
% for multiple modes.  An alternative would be to use a typeclass,
% but typeclasses still don't work in `jump' or `fast' grades.

:- pragma promise_pure(do_while/4).
do_while(GeneratorPred, CollectorPred, Accumulator0, Accumulator) :-
	impure get_registers(HeapPtr, SolutionsHeapPtr, TrailPtr),
	impure new_mutvar(Accumulator0, Mutvar),
	(
		GeneratorPred(Answer0),

		impure check_for_floundering(TrailPtr),

		impure swap_heap_and_solutions_heap,
		impure partial_deep_copy(HeapPtr, Answer0, Answer),
		impure get_mutvar(Mutvar, Acc0),
		CollectorPred(Answer, More, Acc0, Acc1),
		impure set_mutvar(Mutvar, Acc1),
		impure swap_heap_and_solutions_heap,

		% if More = yes, then backtrack for the next solution.
		% if More = no, then we're done.
		More = no
	;
		true
	),
	impure get_mutvar(Mutvar, Accumulator1),
	impure partial_deep_copy(SolutionsHeapPtr, Accumulator1, Accumulator),
	impure reset_solutions_heap(SolutionsHeapPtr),
	impure discard_trail_ticket.

:- type heap_ptr ---> heap_ptr(c_pointer).
:- type trail_ptr ---> trail_ptr(c_pointer).

%
% Save the state of the Mercury heap and trail registers,
% for later use in partial_deep_copy/3 and reset_solutions_heap/1.
% Note that this allocates a trail ticket;
% you need to dispose of it properly when you're finished with it,
% e.g. by calling discard_trail_ticket/0.
%
:- impure pred get_registers(heap_ptr::out, heap_ptr::out, trail_ptr::out)
	is det.

:- pragma foreign_proc("C", 
	get_registers(HeapPtr::out, SolutionsHeapPtr::out, TrailPtr::out),
	[will_not_call_mercury, thread_safe],
"
	/* save heap states */
#ifndef CONSERVATIVE_GC
 	HeapPtr = (MR_Word) MR_hp;
 	SolutionsHeapPtr = (MR_Word) MR_sol_hp;
#else
	HeapPtr = SolutionsHeapPtr = 0;
#endif

	/* save trail state */
#ifdef MR_USE_TRAIL
	MR_store_ticket(TrailPtr);
#else
	TrailPtr = 0;
#endif
").

:- pragma foreign_proc("MC++", 
	get_registers(HeapPtr::out, SolutionsHeapPtr::out, TrailPtr::out),
	[will_not_call_mercury, thread_safe],
"
	/*
	** For MC++, we always use the MS garbage collector,
	** so we don't have to worry here about heap reclamation on failure.
	*/
	HeapPtr = SolutionsHeapPtr = 0;

#ifdef MR_USE_TRAIL
	/* XXX trailing not yet implemented for the MLDS back-end */
	mercury::runtime::Errors::SORRY(""foreign code for get_registers"");
#else
	TrailPtr = 0
#endif

").

:- impure pred check_for_floundering(trail_ptr::in) is det.

:- pragma foreign_proc("C", 
	check_for_floundering(TrailPtr::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	/* check for outstanding delayed goals (``floundering'') */
	MR_reset_ticket(TrailPtr, MR_solve);
#endif
").

:- pragma foreign_proc("MC++", 
	check_for_floundering(_TrailPtr::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for check_for_floundering"");
#endif
").

%
% Discard the topmost trail ticket.
%
:- impure pred discard_trail_ticket is det.

:- pragma foreign_proc("C", 
	discard_trail_ticket,
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	MR_discard_ticket();
#endif
").

:- pragma foreign_proc("MC++", 
	discard_trail_ticket,
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for discard_trail_ticket"");
#endif
").

%
% Swap the heap with the solutions heap
%
:- impure pred swap_heap_and_solutions_heap is det.

:- pragma foreign_proc("C", 
	swap_heap_and_solutions_heap,
	[will_not_call_mercury, thread_safe],
"{
#ifndef CONSERVATIVE_GC
	MR_MemoryZone *temp_zone;
	MR_Word *temp_hp;

	temp_zone = MR_ENGINE(MR_eng_heap_zone);
	MR_ENGINE(MR_eng_heap_zone) = MR_ENGINE(MR_eng_solutions_heap_zone);
	MR_ENGINE(MR_eng_solutions_heap_zone) = temp_zone;
	temp_hp = MR_hp;
	MR_hp = MR_sol_hp;
	MR_sol_hp = temp_hp;
#endif
}").

:- pragma foreign_proc("MC++", 
	swap_heap_and_solutions_heap,
	[will_not_call_mercury, thread_safe],
"
	/*
	** For the .NET back-end, we use the system heap, rather
	** than defining our own heaps.  So we don't need to
	** worry about swapping them.  Hence do nothing here.
	*/
").

%
% partial_deep_copy(SolutionsHeapPtr, OldVal, NewVal):
%	Make a copy of all of the parts of OldVar that occur between
%	SolutionsHeapPtr and the top of the current solutions heap.
%
:- impure pred partial_deep_copy(heap_ptr, T, T) is det.
:-        mode partial_deep_copy(in, di, uo) is det.
:-        mode partial_deep_copy(in, mdi, muo) is det.
:-        mode partial_deep_copy(in, in, out) is det.

:- pragma foreign_decl("C", "

#include ""mercury_deep_copy.h""

#ifdef CONSERVATIVE_GC
  /* for conservative GC, shallow copies suffice */
  #define MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr,			\\
  		OldVar, NewVal, TypeInfo_for_T)				\\
  	do {								\\
		NewVal = OldVal;					\\
	} while (0)
#else
  /*
  ** Note that we need to save/restore the MR_hp register, if it
  ** is transient, before/after calling MR_deep_copy().
  */
  #define MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr,			\\
  		OldVar, NewVal, TypeInfo_for_T)				\\
  	do {								\\
		MR_save_transient_hp();					\\
		NewVal = MR_deep_copy(&OldVal, (MR_TypeInfo) TypeInfo_for_T,\\
			(const MR_Word *) SolutionsHeapPtr,		\\
			MR_ENGINE(MR_eng_solutions_heap_zone)->top);	\\
		MR_restore_transient_hp();				\\
	} while (0)
#endif

").

:- pragma foreign_proc("C",
	partial_deep_copy(SolutionsHeapPtr::in, OldVal::in, NewVal::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").
:- pragma foreign_proc("C", 
	partial_deep_copy(SolutionsHeapPtr::in, OldVal::mdi, NewVal::muo),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").
:- pragma foreign_proc("C",
	partial_deep_copy(SolutionsHeapPtr::in, OldVal::di, NewVal::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").

:- pragma foreign_proc("MC++",
	partial_deep_copy(_SolutionsHeapPtr::in, OldVal::in, NewVal::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	/*
	** For the IL back-end, we don't do heap reclamation on failure,
	** so we don't need to worry about making deep copies here.
	** Shallow copies will suffice.
	*/
	NewVal = OldVal;
").
:- pragma foreign_proc("MC++", 
	partial_deep_copy(_SolutionsHeapPtr::in, OldVal::mdi, NewVal::muo),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	NewVal = OldVal;
").
:- pragma foreign_proc("MC++",
	partial_deep_copy(_SolutionsHeapPtr::in, OldVal::di, NewVal::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	NewVal = OldVal;
").

%
% reset_solutions_heap(SolutionsHeapPtr):
%	Reset the solutions heap pointer to the specified value,
%	thus deallocating everything allocated on the solutions
%	heap since that value was obtained via get_registers/3.
%
:- impure pred reset_solutions_heap(heap_ptr::in) is det.

:- pragma foreign_proc("C", 
	reset_solutions_heap(SolutionsHeapPtr::in),
	[will_not_call_mercury, thread_safe, promise_pure],
"
#ifndef CONSERVATIVE_GC
	MR_sol_hp = (MR_Word *) SolutionsHeapPtr;
#endif
").

:- pragma foreign_proc("MC++", 
	reset_solutions_heap(_SolutionsHeapPtr::in),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	/*
	** For the IL back-end, we don't have a separate `solutions heap'.
	** Hence this operation is a NOP.
	*/
").

%-----------------------------------------------------------------------------%

%%% :- module mutvar.
%%% :- interface.

%  A non-backtrackably destructively modifiable reference type
:- type mutvar(T).

%  Create a new mutvar given a term for it to reference.
:- impure pred new_mutvar(T, mutvar(T)).
:-        mode new_mutvar(in, out) is det.
:-        mode new_mutvar(di, uo) is det.

%  Get the value currently referred to by a reference.
:- impure pred get_mutvar(mutvar(T), T) is det.
:-        mode get_mutvar(in, uo) is det.	% XXX this is a work-around
/*
XXX `ui' modes don't work yet
:-        mode get_mutvar(in, uo) is det.
:-        mode get_mutvar(ui, uo) is det.	% unsafe, but we use it safely
*/

%  destructively modify a reference to refer to a new object.
:- impure pred set_mutvar(mutvar(T), T) is det.
:-        mode set_mutvar(in, in) is det.
/*
XXX `ui' modes don't work yet
:-        pred set_mutvar(ui, di) is det.
*/

%%% :- implementation.

%  This type is implemented in C.
:- type mutvar(T) ---> mutvar(c_pointer).

:- pragma inline(new_mutvar/2).

:- pragma foreign_proc("C",
	new_mutvar(X::in, Ref::out),
	[will_not_call_mercury, thread_safe],
"
	MR_incr_hp_msg(Ref, 1, MR_PROC_LABEL, ""std_util:mutvar/1"");
	*(MR_Word *) Ref = X;
").
:- pragma foreign_proc("C",
	new_mutvar(X::di, Ref::uo),
	[will_not_call_mercury, thread_safe],
"
	MR_incr_hp_msg(Ref, 1, MR_PROC_LABEL, ""std_util:mutvar/1"");
	*(MR_Word *) Ref = X;
").

:- pragma inline(get_mutvar/2).

:- pragma foreign_proc("C",
	get_mutvar(Ref::in, X::uo),
	[will_not_call_mercury, thread_safe],
"
	X = *(MR_Word *) Ref;
").

:- pragma inline(set_mutvar/2).

:- pragma foreign_proc("C",
	set_mutvar(Ref::in, X::in),
	[will_not_call_mercury, thread_safe],
"
	*(MR_Word *) Ref = X;
").

:- pragma foreign_proc("MC++", 
	new_mutvar(X::in, Ref::out),
	[will_not_call_mercury, thread_safe],
"
	MR_untagged_newobj(Ref, 1);
	Ref[0] = X;
").
:- pragma foreign_proc("MC++", 
	new_mutvar(X::di, Ref::uo),
	[will_not_call_mercury, thread_safe],
"
	MR_untagged_newobj(Ref, 1);
	Ref[0] = X;
").

:- pragma inline(get_mutvar/2).

:- pragma foreign_proc("MC++",
	get_mutvar(Ref::in, X::uo),
	[will_not_call_mercury, thread_safe],
"
	X = Ref[0];
").

:- pragma inline(set_mutvar/2).

:- pragma foreign_proc("MC++",
	set_mutvar(Ref::in, X::in),
	[will_not_call_mercury, thread_safe],
"
	Ref[0] = X;
").

%%% end_module mutvar.

%-----------------------------------------------------------------------------%

solutions(Pred, List) :-
	builtin_solutions(Pred, UnsortedList),
	list__sort_and_remove_dups(UnsortedList, List).

solutions_set(Pred, Set) :-
	builtin_solutions(Pred, List),
	set__list_to_set(List, Set).

unsorted_solutions(Pred, List) :-
	builtin_solutions(Pred, UnsortedList),
	cc_multi_equal(UnsortedList, List).

:- pred builtin_solutions(pred(T), list(T)).
:- mode builtin_solutions(pred(out) is multi, out)
	is det. /* really cc_multi */
:- mode builtin_solutions(pred(out) is nondet, out)
	is det. /* really cc_multi */

builtin_solutions(Generator, UnsortedList) :-
	builtin_aggregate(Generator, cons, [], UnsortedList).

:- pred cons(T::in, list(T)::in, list(T)::out) is det.
cons(H, T, [H|T]).

%-----------------------------------------------------------------------------%

aggregate(Generator, Accumulator, Acc0, Acc) :-
	solutions(Generator, Solutions),
	list__foldl(Accumulator, Solutions, Acc0, Acc).

aggregate2(Generator, Accumulator, Acc0, Acc) -->
	{ solutions(Generator, Solutions) },
	list__foldl2(Accumulator, Solutions, Acc0, Acc).

unsorted_aggregate(Generator, Accumulator, Acc0, Acc) :-
	builtin_aggregate(Generator, Accumulator, Acc0, Acc1),
	cc_multi_equal(Acc1, Acc).

%-----------------------------------------------------------------------------%

% semidet_succeed and semidet_fail, implemented using the C interface
% to make sure that the compiler doesn't issue any determinism warnings
% for them.

:- pragma foreign_proc("C",
	semidet_succeed, 
	[will_not_call_mercury, thread_safe, promise_pure],
	"SUCCESS_INDICATOR = TRUE;").
:- pragma foreign_proc("C",
	semidet_fail,
	[will_not_call_mercury, thread_safe, promise_pure],
	"SUCCESS_INDICATOR = FALSE;").
:- pragma foreign_proc("C",
	cc_multi_equal(X::in, Y::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"Y = X;").
:- pragma foreign_proc("C",
	cc_multi_equal(X::di, Y::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"Y = X;").

:- pragma foreign_proc("MC++",
	semidet_succeed, 
	[will_not_call_mercury, thread_safe, promise_pure],
	"SUCCESS_INDICATOR = TRUE;").
:- pragma foreign_proc("MC++",
	semidet_fail, 
	[will_not_call_mercury, thread_safe, promise_pure],
	"SUCCESS_INDICATOR = FALSE;").
:- pragma foreign_proc("MC++",
	cc_multi_equal(X::in, Y::out),
	[will_not_call_mercury, thread_safe, promise_pure],
	"Y = X;").
:- pragma foreign_proc("MC++",
	cc_multi_equal(X::di, Y::uo),
	[will_not_call_mercury, thread_safe, promise_pure],
	"Y = X;").

%-----------------------------------------------------------------------------%

	% We call the constructor for univs `univ_cons' to avoid ambiguity
	% with the univ/1 function which returns a univ.
:- type univ --->
	some [T] univ_cons(T).

univ_to_type(Univ, X) :- type_to_univ(X, Univ).

univ(X) = Univ :- type_to_univ(X, Univ).

det_univ_to_type(Univ, X) :-
	( type_to_univ(X0, Univ) ->
		X = X0
	;
		UnivTypeName = type_desc__type_name(univ_type(Univ)),
		ObjectTypeName = type_desc__type_name(type_desc__type_of(X)),
		string__append_list(["det_univ_to_type: conversion failed\\n",
			"\tUniv Type: ", UnivTypeName,
			"\\n\tObject Type: ", ObjectTypeName], ErrorString),
		error(ErrorString)
	).

univ_value(univ_cons(X)) = X.

:- pragma promise_pure(type_to_univ/2).
type_to_univ(T::di, Univ::uo) :-
	Univ0 = 'new univ_cons'(T),
	unsafe_promise_unique(Univ0, Univ).
type_to_univ(T::in, Univ::out) :-
	Univ0 = 'new univ_cons'(T),
	unsafe_promise_unique(Univ0, Univ).
type_to_univ(T::out, Univ::in) :-
	Univ = univ_cons(T0),
	private_builtin__typed_unify(T0, T).

univ_type(Univ) = type_desc__type_of(univ_value(Univ)).

:- pred construct_univ(T, univ).
:- mode construct_univ(in, out) is det.
:- pragma export(construct_univ(in, out), "ML_construct_univ").

construct_univ(X, Univ) :-
	Univ = univ(X).

:- some [T] pred unravel_univ(univ, T).
:- mode unravel_univ(in, out) is det.
:- pragma export(unravel_univ(in, out), "ML_unravel_univ").

unravel_univ(Univ, X) :-
	univ_value(Univ) = X.

dynamic_cast(X, Y) :-
	univ_to_type(univ(X), Y).

%-----------------------------------------------------------------------------%

% The actual code of these predicates and functions is now in
% the file type_desc.m.

type_of(Value) =
	type_desc__type_of(Value).

has_type(Arg, TypeInfo) :-
	type_desc__has_type(Arg, TypeInfo).

type_name(Type) =
	type_desc__type_name(Type).

type_args(Type) =
	type_desc__type_args(Type).

type_ctor_name(TypeCtor) =
	type_desc__type_ctor_name(TypeCtor).

type_ctor_module_name(TypeCtor) =
	type_desc__type_ctor_module_name(TypeCtor).

type_ctor_arity(TypeCtor) =
	type_desc__type_ctor_arity(TypeCtor).

det_make_type(TypeCtor, ArgTypes) =
	type_desc__det_make_type(TypeCtor, ArgTypes).

type_ctor(TypeInfo) =
	type_desc__type_ctor(TypeInfo).

type_ctor_and_args(TypeDesc, TypeCtorDesc, ArgTypes) :-
	type_desc__type_ctor_and_args(TypeDesc, TypeCtorDesc, ArgTypes).

make_type(TypeCtorDesc, ArgTypes) =
	type_desc__make_type(TypeCtorDesc, ArgTypes).

type_ctor_name_and_arity(TypeCtorDesc, TypeCtorModuleName,
		TypeCtorName, TypeCtorArity) :-
	type_desc__type_ctor_name_and_arity(TypeCtorDesc, TypeCtorModuleName,
		TypeCtorName, TypeCtorArity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The actual code of these predicates and functions is now in
% the file construct.m.

num_functors(TypeInfo) =
	construct__num_functors(TypeInfo).

get_functor(TypeDesc, FunctorNumber, FunctorName, Arity, TypeInfoList) :-
	construct__get_functor(TypeDesc, FunctorNumber, FunctorName,
		Arity, TypeInfoList).

get_functor(TypeDesc, FunctorNumber, FunctorName, Arity, TypeInfoList,
		ArgNameList) :-
	construct__get_functor(TypeDesc, FunctorNumber, FunctorName,
		Arity, TypeInfoList, ArgNameList).

get_functor_ordinal(TypeDesc, FunctorNumber, Ordinal) :-
	construct__get_functor_ordinal(TypeDesc, FunctorNumber, Ordinal).

construct(TypeDesc, FunctorNumber, ArgList) =
	construct__construct(TypeDesc, FunctorNumber, ArgList).

construct_tuple(Args) =
	construct__construct_tuple(Args).

%-----------------------------------------------------------------------------%

% The actual code of these predicates and functions is now in
% the file deconstruct.m.

functor(Term, Functor, Arity) :-
	deconstruct__functor(Term, canonicalize, Functor, Arity).

functor_cc(Term, Functor, Arity) :-
	deconstruct__functor(Term, include_details_cc, Functor, Arity).

arg(Term, Index) = Argument :-
	deconstruct__arg(Term, canonicalize, Index, Argument0),
	private_builtin__typed_unify(Argument0, Argument).

arg_cc(Term, Index, Argument) :-
	deconstruct__arg(Term, include_details_cc, Index, Argument0),
	( private_builtin__typed_unify(Argument0, Argument1) ->
		Argument = Argument1
	;
		error("arg_cc: argument has wrong type")
	).

argument(Term, Index) = ArgumentUniv :-
	deconstruct__arg(Term, canonicalize, Index, Argument),
	type_to_univ(Argument, ArgumentUniv).

argument_cc(Term, Index, ArgumentUniv) :-
	deconstruct__arg(Term, include_details_cc, Index, Argument),
	type_to_univ(Argument, ArgumentUniv).

named_argument(Term, Name) = ArgumentUniv :-
	deconstruct__named_arg(Term, canonicalize, Name, Argument),
	type_to_univ(Argument, ArgumentUniv).

named_argument_cc(Term, Name, ArgumentUniv) :-
	deconstruct__named_arg(Term, include_details_cc,
		Name, Argument),
	type_to_univ(Argument, ArgumentUniv).

deconstruct(Term, Functor, Arity, Arguments) :-
	deconstruct__deconstruct(Term, canonicalize,
		Functor, Arity, Arguments).

deconstruct_cc(Term, Functor, Arity, Arguments) :-
	deconstruct__deconstruct(Term, include_details_cc,
		Functor, Arity, Arguments).

limited_deconstruct(Term, MaxArity, Functor, Arity, Arguments) :-
	deconstruct__limited_deconstruct(Term, canonicalize,
		MaxArity, Functor, Arity, Arguments).

limited_deconstruct_cc(Term, MaxArity, Functor, Arity, Arguments) :-
	deconstruct__limited_deconstruct(Term, include_details_cc,
		MaxArity, Functor, Arity, Arguments).

det_arg(Type, Index) = Argument :-
	deconstruct__det_arg(Type, canonicalize, Index, Argument0),
	( private_builtin__typed_unify(Argument0, Argument1) ->
		Argument = Argument1
	;
		error("det_arg: argument has wrong type")
	).

det_arg_cc(Type, Index, Argument) :-
	deconstruct__det_arg(Type, include_details_cc, Index, Argument0),
	( private_builtin__typed_unify(Argument0, Argument1) ->
		Argument = Argument1
	;
		error("det_arg_cc: argument has wrong type")
	).

det_argument(Type, Index) = ArgumentUniv :-
	deconstruct__det_arg(Type, canonicalize, Index, Argument),
	type_to_univ(Argument, ArgumentUniv).

det_argument_cc(Type, Index, ArgumentUniv) :-
	deconstruct__det_arg(Type, include_details_cc, Index, Argument),
	type_to_univ(Argument, ArgumentUniv).

det_named_argument(Type, Name) = ArgumentUniv :-
	deconstruct__det_named_arg(Type, canonicalize, Name, Argument),
	type_to_univ(Argument, ArgumentUniv).

det_named_argument_cc(Type, Name, ArgumentUniv) :-
	deconstruct__det_named_arg(Type, include_details_cc, Name, Argument),
	type_to_univ(Argument, ArgumentUniv).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%   Function forms added.

pair(X, Y) =
	X-Y.

maybe_func(PF, X) =
	( if Y = PF(X) then yes(Y) else no ).

compose(F, G, X) =
	F(G(X)).

converse(F, X, Y) =
	F(Y, X).

pow(F, N, X) =
	( if N = 0 then X else pow(F, N - 1, F(X)) ).

isnt(P, X) :-
	not P(X).

id(X) = X.

solutions(P) = S :- solutions(P, S).

solutions_set(P) = S :- solutions_set(P, S).

aggregate(P, F, Acc0) = Acc :-
	aggregate(P, (pred(X::in, A0::in, A::out) is det :- A = F(X, A0)),
		Acc0, Acc).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
