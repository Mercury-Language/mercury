%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: std_util.m.
% Main author: fjh.
% Stability: medium to high.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.

:- interface.

:- import_module list, set, bool.

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
:- func univ_type(univ) = type_desc.

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

:- type type_desc.
:- type type_ctor_desc.

	% Type_info and type_ctor_info are the old names for type_desc and
	% type_ctor_desc. They should not be used by new software.

:- type type_info == type_desc.
:- type type_ctor_info == type_ctor_desc.

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
:- func type_of(T) = type_desc.
:- mode type_of(unused) = out is det.

	% The predicate has_type/2 is basically an existentially typed
	% inverse to the function type_of/1.  It constrains the type
	% of the first argument to be the type represented by the
	% second argument.
:- some [T] pred has_type(T::unused, type_desc::in) is det.

	% type_name(Type) returns the name of the specified type
	% (e.g. type_name(type_of([2,3])) = "list:list(int)").
	% Any equivalence types will be fully expanded.
	% Builtin types (those defined in builtin.m) will
	% not have a module qualifier.
	%
:- func type_name(type_desc) = string.

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
:- pred type_ctor_and_args(type_desc, type_ctor_desc, list(type_desc)).
:- mode type_ctor_and_args(in, out, out) is det.

	% type_ctor(Type) = TypeCtor :-
	%	type_ctor_and_args(Type, TypeCtor, _).
	%
:- func type_ctor(type_desc) = type_ctor_desc.

	% type_args(Type) = TypeArgs :-
	%	type_ctor_and_args(Type, _, TypeArgs).
	%
:- func type_args(type_desc) = list(type_desc).

	% type_ctor_name(TypeCtor) returns the name of specified
	% type constructor.
	% (e.g. type_ctor_name(type_ctor(type_of([2,3]))) = "list").
	%
:- func type_ctor_name(type_ctor_desc) = string.

	% type_ctor_module_name(TypeCtor) returns the module name of specified
	% type constructor.
	% (e.g. type_ctor_module_name(type_ctor(type_of(2))) = "builtin").
	%
:- func type_ctor_module_name(type_ctor_desc) = string.

	% type_ctor_arity(TypeCtor) returns the arity of specified
	% type constructor.
	% (e.g. type_ctor_arity(type_ctor(type_of([2,3]))) = 1).
	%
:- func type_ctor_arity(type_ctor_desc) = int.

	% type_ctor_name_and_arity(TypeCtor, ModuleName, TypeName, Arity) :-
	%	Name = type_ctor_name(TypeCtor),
	%	ModuleName = type_ctor_module_name(TypeCtor),
	%	Arity = type_ctor_arity(TypeCtor).
	%
:- pred type_ctor_name_and_arity(type_ctor_desc, string, string, int).
:- mode type_ctor_name_and_arity(in, out, out, out) is det.

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
:- func make_type(type_ctor_desc, list(type_desc)) = type_desc.
:- mode make_type(in, in) = out is semidet.
:- mode make_type(out, out) = in is cc_multi.

	% det_make_type(TypeCtor, TypeArgs):
	%
	% Returns the type formed by applying the specified type
	% constructor to the specified argument types.  Aborts if the
	% length of `TypeArgs' is not the same as the arity of `TypeCtor'.
	%
:- func det_make_type(type_ctor_desc, list(type_desc)) = type_desc.
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
:- func num_functors(type_desc) = int.

	% get_functor(Type, I, Functor, Arity, ArgTypes)
	%
	% Binds Functor and Arity to the name and arity of functor number I
	% for the specified type, and binds ArgTypes to the type_descs for
	% the types of the arguments of that functor.  Fails if the type
	% is not a discriminated union type, or if I is out of range.
	%
:- pred get_functor(type_desc::in, int::in, string::out, int::out,
		list(type_desc)::out) is semidet.

	% get_functor(Type, I, Functor, Arity, ArgTypes, ArgNames)
	%
	% Binds Functor and Arity to the name and arity of functor number I
	% for the specified type, ArgTypes to the type_descs for the types
	% of the arguments of that functor, and ArgNames to the field name
	% of each functor argument, if any.  Fails if the type is not a
	% discriminated union type, or if I is out of range.
	%
:- pred get_functor(type_desc::in, int::in, string::out, int::out,
		list(type_desc)::out, list(maybe(string))::out) is semidet.

	% get_functor_ordinal(Type, I, Ordinal)
	%
	% Returns Ordinal, where Ordinal is the position in declaration order
	% for the specified type of the function symbol that is in position I
	% in lexicographic order. Fails if the type is not a discriminated
	% union type, or if I is out of range.
:- pred get_functor_ordinal(type_desc::in, int::in, int::out) is semidet.

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
:- func construct(type_desc, int, list(univ)) = univ.
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
	%	- for predicates and functions, the string <<predicate>>
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
:- pred functor(T::in, string::out, int::out) is det.

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
:- func arg(T::in, int::in) = (ArgT::out) is semidet.
:- func argument(T::in, int::in) = (univ::out) is semidet.

	% det_arg(Data, ArgumentIndex) = Argument
	% det_argument(Data, ArgumentIndex) = ArgumentUniv
	%
	% Same as arg/2 and argument/2 respectively, except that
	% for cases where arg/2 or argument/2 would fail,
	% det_arg/2 or det_argument/2 will abort.
	%
:- func det_arg(T::in, int::in) = (ArgT::out) is det.
:- func det_argument(T::in, int::in) = (univ::out) is det.

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
:- pred deconstruct(T::in, string::out, int::out, list(univ)::out) is det.

	% limited_deconstruct(Data, MaxArity, Functor, Arity, Arguments)
	%
	% limited_deconstruct works like deconstruct, but if the arity of T is
	% greater than MaxArity, limited_deconstruct fails. This is useful in
	% avoiding bad performance in cases where Data may be a large array.
	%
:- pred limited_deconstruct(T::in, int::in, string::out,
	int::out, list(univ)::out) is semidet.

:- implementation.
:- interface.

% The rest of the interface is for use by implementors only.

:- type functor_tag_info
        --->    functor_integer(int)
        ;       functor_float(float)
        ;       functor_string(string)
        ;       functor_enum(int)
        ;       functor_local(int, int)
        ;       functor_remote(int, int, list(univ))
        ;       functor_unshared(int, list(univ))
        ;       functor_notag(univ)
        ;       functor_equiv(univ).

	% get_functor_info is a variant of deconstruct for use by the compiler,
	% specifically prog_rep.m and static_term.m. It differs from
	% deconstruct in two main ways. First, instead of returning the
	% function symbol, it returns implementation information about
	% its tag. Second, it succeeds for just the kinds of terms needed
	% to represent procedure bodies for ordinary procedures. For the time
	% being, these are procedures that do not involve higher order code
	% or tabling.
:- pred get_functor_info(univ::in, functor_tag_info::out) is semidet.

	% dynamic_cast(X, Y) succeeds with Y = X iff X has the same
	% ground type as Y (so this may succeed if Y is of type
	% list(int), say, but not if Y is of type list(T)).
	%
:- pred dynamic_cast(T1::in, T2::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, set, int, string, bool.

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
		get_registers(HeapPtr::out, SolutionsHeapPtr::out,
		TrailPtr::out), will_not_call_mercury,
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
		get_registers(HeapPtr::out, SolutionsHeapPtr::out,
		TrailPtr::out), will_not_call_mercury,
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
	check_for_floundering(TrailPtr::in), [will_not_call_mercury],
"
#ifdef MR_USE_TRAIL
	/* check for outstanding delayed goals (``floundering'') */
	MR_reset_ticket(TrailPtr, MR_solve);
#endif
").
:- pragma foreign_proc("MC++", 
	check_for_floundering(_TrailPtr::in), [will_not_call_mercury],
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
	discard_trail_ticket, [will_not_call_mercury],
"
#ifdef MR_USE_TRAIL
	MR_discard_ticket();
#endif
").
:- pragma foreign_proc("MC++", 
	discard_trail_ticket, [will_not_call_mercury],
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
	will_not_call_mercury,
"
#ifndef CONSERVATIVE_GC
    {
	MR_MemoryZone *temp_zone;
	MR_Word *temp_hp;

	temp_zone = MR_ENGINE(MR_eng_heap_zone);
	MR_ENGINE(MR_eng_heap_zone) = MR_ENGINE(MR_eng_solutions_heap_zone);
	MR_ENGINE(MR_eng_solutions_heap_zone) = temp_zone;
	temp_hp = MR_hp;
	MR_hp = MR_sol_hp;
	MR_sol_hp = temp_hp;
    }
#endif
").
:- pragma foreign_proc("MC++", 
	swap_heap_and_solutions_heap,
	will_not_call_mercury,
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
				(const MR_Word *) SolutionsHeapPtr,	\\
				MR_ENGINE(MR_eng_solutions_heap_zone)->top);\\
		MR_restore_transient_hp();				\\
	} while (0)
#endif

").

:- pragma foreign_proc("C",
	partial_deep_copy(SolutionsHeapPtr::in,
		OldVal::in, NewVal::out), will_not_call_mercury,
"
	MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").
:- pragma foreign_proc("C", 
	partial_deep_copy(SolutionsHeapPtr::in,
		OldVal::mdi, NewVal::muo), will_not_call_mercury,
"
	MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").
:- pragma foreign_proc("C", partial_deep_copy(SolutionsHeapPtr::in,
		OldVal::di, NewVal::uo), will_not_call_mercury,
"
	MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").

:- pragma foreign_proc("MC++",
	partial_deep_copy(_SolutionsHeapPtr::in,
		OldVal::in, NewVal::out), will_not_call_mercury,
"
	/*
	** For the IL back-end, we don't do heap reclamation on failure,
	** so we don't need to worry about making deep copies here.
	** Shallow copies will suffice.
	*/
	NewVal = OldVal;
").
:- pragma foreign_proc("MC++", 
	partial_deep_copy(_SolutionsHeapPtr::in,
		OldVal::mdi, NewVal::muo), will_not_call_mercury,
"
	NewVal = OldVal;
").
:- pragma foreign_proc("MC++", partial_deep_copy(_SolutionsHeapPtr::in,
		OldVal::di, NewVal::uo), will_not_call_mercury,
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
	will_not_call_mercury,
"
#ifndef CONSERVATIVE_GC
	MR_sol_hp = (MR_Word *) SolutionsHeapPtr;
#endif
").

:- pragma foreign_proc("MC++", 
	reset_solutions_heap(_SolutionsHeapPtr::in),
	will_not_call_mercury,
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
:- pragma foreign_proc("C", new_mutvar(X::in, Ref::out), will_not_call_mercury,
"
	MR_incr_hp_msg(Ref, 1, MR_PROC_LABEL, ""std_util:mutvar/1"");
	*(MR_Word *) Ref = X;
").
:- pragma foreign_proc("C", new_mutvar(X::di, Ref::uo), will_not_call_mercury,
"
	MR_incr_hp_msg(Ref, 1, MR_PROC_LABEL, ""std_util:mutvar/1"");
	*(MR_Word *) Ref = X;
").

:- pragma inline(get_mutvar/2).
:- pragma foreign_proc("C", get_mutvar(Ref::in, X::uo), will_not_call_mercury,
"
	X = *(MR_Word *) Ref;
").

:- pragma inline(set_mutvar/2).
:- pragma foreign_proc("C", set_mutvar(Ref::in, X::in), will_not_call_mercury, "
	*(MR_Word *) Ref = X;
").

:- pragma foreign_proc("MC++", 
	new_mutvar(X::in, Ref::out), will_not_call_mercury,
"
	MR_untagged_newobj(Ref, 1);
	Ref[0] = X;
").
:- pragma foreign_proc("MC++", 
	new_mutvar(X::di, Ref::uo), will_not_call_mercury,
"
	MR_untagged_newobj(Ref, 1);
	Ref[0] = X;
").

:- pragma inline(get_mutvar/2).
:- pragma foreign_proc("MC++",
	get_mutvar(Ref::in, X::uo), will_not_call_mercury,
"
	X = Ref[0];
").

:- pragma inline(set_mutvar/2).
:- pragma foreign_proc("MC++",
	set_mutvar(Ref::in, X::in), will_not_call_mercury,
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

:- pragma foreign_proc("C", semidet_succeed, 
		[will_not_call_mercury, thread_safe],
		"SUCCESS_INDICATOR = TRUE;").
:- pragma foreign_proc("C", semidet_fail, [will_not_call_mercury, thread_safe],
		"SUCCESS_INDICATOR = FALSE;").
:- pragma foreign_proc("C", cc_multi_equal(X::in, Y::out),
               [will_not_call_mercury, thread_safe],
		"Y = X;").
:- pragma foreign_proc("C", cc_multi_equal(X::di, Y::uo),
               [will_not_call_mercury, thread_safe],
		"Y = X;").

:- pragma foreign_proc("MC++", semidet_succeed, 
		[will_not_call_mercury, thread_safe],
		"SUCCESS_INDICATOR = TRUE;").
:- pragma foreign_proc("MC++", semidet_fail, 
		[will_not_call_mercury, thread_safe],
		"SUCCESS_INDICATOR = FALSE;").
:- pragma foreign_proc("MC++", cc_multi_equal(X::in, Y::out),
               [will_not_call_mercury, thread_safe],
		"Y = X;").
:- pragma foreign_proc("MC++", cc_multi_equal(X::di, Y::uo),
               [will_not_call_mercury, thread_safe],
		"Y = X;").


%-----------------------------------------------------------------------------%

	% The type `std_util:type_desc/0' happens to use much the same
	% representation as `private_builtin:type_info/1'.

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
		UnivTypeName = type_name(univ_type(Univ)),
		ObjectTypeName = type_name(type_of(X)),
		string__append_list(["det_univ_to_type: conversion failed\\n",
			"\tUniv Type: ", UnivTypeName,
			"\\n\tObject Type: ", ObjectTypeName], ErrorString),
		error(ErrorString)
	).

univ_value(univ_cons(X)) = X.

:- pragma promise_pure(type_to_univ/2).
type_to_univ(T, Univ) :-
	(
		impure private_builtin__var(T),
		Univ = univ_cons(T0),
		private_builtin__typed_unify(T0, T)
	;
		impure private_builtin__var(Univ),
		Univ0 = 'new univ_cons'(T),
		unsafe_promise_unique(Univ0, Univ)
	).

univ_type(Univ) = type_of(univ_value(Univ)).

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

:- pragma foreign_decl("C", "
#include ""mercury_heap.h""	/* for MR_incr_hp_msg() etc. */
#include ""mercury_misc.h""	/* for MR_fatal_error() */
#include ""mercury_string.h""	/* for MR_make_aligned_string() */
").

:- pragma foreign_code("C", "

#include ""mercury_deep_profiling_hand.h""

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_type_desc_module
*/

/* suppress gcc -Wmissing-decl warnings */
void sys_init_type_desc_module_init(void);
void sys_init_type_desc_module_init_type_tables(void);
#ifdef	MR_DEEP_PROFILING
void sys_init_type_desc_module_write_out_proc_statics(FILE *);
#endif

#ifndef MR_HIGHLEVEL_CODE

#ifdef	MR_DEEP_PROFILING
MR_proc_static_compiler_empty(std_util, __Unify__,   type_desc, 0, 0,
	""std_util.m"", 0, TRUE);
MR_proc_static_compiler_empty(std_util, __Compare__, type_desc, 0, 0,
	""std_util.m"", 0, TRUE);
#endif

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(std_util, type_desc, 0,
	MR_TYPECTOR_REP_TYPEINFO);

MR_define_extern_entry(mercury____Unify___std_util__type_desc_0_0);
MR_define_extern_entry(mercury____Compare___std_util__type_desc_0_0);

MR_BEGIN_MODULE(type_desc_module)
	MR_init_entry(mercury____Unify___std_util__type_desc_0_0);
	MR_init_entry(mercury____Compare___std_util__type_desc_0_0);
#ifdef	MR_DEEP_PROFILING
	MR_init_label(mercury____Unify___std_util__type_desc_0_0_i1);
	MR_init_label(mercury____Unify___std_util__type_desc_0_0_i2);
	MR_init_label(mercury____Unify___std_util__type_desc_0_0_i3);
	MR_init_label(mercury____Unify___std_util__type_desc_0_0_i4);
	MR_init_label(mercury____Compare___std_util__type_desc_0_0_i1);
	MR_init_label(mercury____Compare___std_util__type_desc_0_0_i2);
#endif
MR_BEGIN_CODE

#define	proc_label	mercury____Unify___std_util__type_desc_0_0
#define	proc_static	MR_proc_static_compiler_name(std_util, __Unify__, \
				type_desc, 0, 0)
#define	body_code	do {						\
				int	comp;				\
									\
				MR_save_transient_registers();		\
				comp = MR_compare_type_info(		\
					(MR_TypeInfo) MR_r1,		\
					(MR_TypeInfo) MR_r2);		\
				MR_restore_transient_registers();	\
				MR_r1 = (comp == MR_COMPARE_EQUAL);	\
			} while (0)

#include ""mercury_hand_unify_body.h""

#undef	body_code
#undef	proc_static
#undef	proc_label

#define	proc_label	mercury____Compare___std_util__type_desc_0_0
#define	proc_static	MR_proc_static_compiler_name(std_util, __Compare__, \
				type_desc, 0, 0)
#define	body_code	do {						\
				int	comp;				\
									\
				MR_save_transient_registers();		\
				comp = MR_compare_type_info(		\
					(MR_TypeInfo) MR_r1,		\
					(MR_TypeInfo) MR_r2);		\
				MR_restore_transient_registers();	\
				MR_r1 = comp;				\
			} while (0)

#include ""mercury_hand_compare_body.h""

#undef	body_code
#undef	proc_static
#undef	proc_label

MR_END_MODULE

MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc type_desc_module;

#endif /* ! MR_HIGHLEVEL_CODE */

void
sys_init_type_desc_module_init(void)
{
#ifndef	MR_HIGHLEVEL_CODE
	type_desc_module();

	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_std_util__type_ctor_info_type_desc_0,
		std_util__type_desc_0_0);
#endif
}

void
sys_init_type_desc_module_init_type_tables(void)
{
#ifndef	MR_HIGHLEVEL_CODE
	MR_register_type_ctor_info(
		&mercury_data_std_util__type_ctor_info_type_desc_0);
#endif
}

#ifdef	MR_DEEP_PROFILING
void
sys_init_type_desc_module_write_out_proc_statics(FILE *fp)
{
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_compiler_name(std_util, __Compare__, type_desc,
			0, 0));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_compiler_name(std_util, __Unify__, type_desc,
			0, 0));
}
#endif

").

	% We need to call the rtti_implementation module -- so that we get the
	% dependencies right it's easiest to do it from Mercury.

:- pragma export(call_rtti_compare_type_infos(out, in, in),
	"ML_call_rtti_compare_type_infos").

:- pred call_rtti_compare_type_infos(comparison_result::out, 
	rtti_implementation__type_info::in, rtti_implementation__type_info::in) is det.

:- use_module rtti_implementation.

call_rtti_compare_type_infos(Res, T1, T2) :-
	rtti_implementation__compare_type_infos(Res, T1, T2).

:- pragma foreign_code("MC++", "

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(std_util, type_desc, 0, 
        MR_TYPECTOR_REP_TYPEINFO)

static int MR_compare_type_info(MR_Word t1, MR_Word t2) {
	MR_Word res;

	mercury::std_util::mercury_code::ML_call_rtti_compare_type_infos(
		&res, t1, t2);
	return System::Convert::ToInt32(res[0]);
}

static void
__Compare____type_desc_0_0(
    MR_Word_Ref result, MR_Word x, MR_Word y)
{
	mercury::std_util::mercury_code::ML_call_rtti_compare_type_infos(
		result, x, y);
}

static bool
__Unify____type_desc_0_0(MR_Word x, MR_Word y)
{
	return (MR_compare_type_info(x, y) == MR_COMPARE_EQUAL);
}

static void
special___Compare___type_desc_0_0(
    MR_Word_Ref result, MR_Word x, MR_Word y)
{
	mercury::std_util::mercury_code::ML_call_rtti_compare_type_infos(
		result, x, y);
}

static bool
special___Unify___type_desc_0_0(MR_Word x, MR_Word y)
{
	return (MR_compare_type_info(x, y) == MR_COMPARE_EQUAL);
}

static int
do_unify__type_desc_0_0(MR_Box x, MR_Box y)
{
    return mercury::std_util__cpp_code::mercury_code::__Unify____type_desc_0_0(
	    dynamic_cast<MR_Word>(x),
	    dynamic_cast<MR_Word>(y));
}

static void
do_compare__type_desc_0_0(
    MR_Word_Ref result, MR_Box x, MR_Box y)
{
    mercury::std_util__cpp_code::mercury_code::__Compare____type_desc_0_0(
	    result,
	    dynamic_cast<MR_Word>(x),
	    dynamic_cast<MR_Word>(y));
}

").

%-----------------------------------------------------------------------------%

	% Code for type manipulation.

	% Prototypes and type definitions.

:- pragma foreign_decl("C", "

/* The `#ifndef ... #define ... #endif' guards against multiple inclusion */
#ifndef ML_TYPECTORDESC_GUARD
#define ML_TYPECTORDESC_GUARD

/*
** Values of type `std_util:type_desc' are represented the same way as
** values of type `private_builtin:type_info' (this representation is
** documented in compiler/polymorphism.m). Some parts of the library
** (e.g. the gc initialization code) depend on this.
** The C type corresponding to these Mercury types is `MR_TypeInfo'.
**
** Values of type `std_util:type_ctor_desc' are not guaranteed to be
** represented the same way as values of type `private_builtin:type_ctor_info'.
** The representations *are* in fact identical for first order types, but they
** differ for higher order and tuple types. Instead of a type_ctor_desc
** being a structure containing a pointer to the type_ctor_info for pred/0
** or func/0 and an arity, we have a single small encoded integer. This
** integer is four times the arity, plus zero, one or two; plus zero encodes a
** tuple, plus one encodes a predicate, plus two encodes a function.
** The maximum arity that can be encoded is given by MR_MAX_VARIABLE_ARITY
** (see below).
** The C type corresponding to std_util:type_ctor_desc is `MR_TypeCtorDesc'.
*/

/*
** Declare the MR_TypeCtorDesc ADT.
**
** Note that `struct MR_TypeCtorDesc_Struct' is deliberately left undefined.
** MR_TypeCtorDesc is declared as a pointer to a dummy structure only
** in order to allow the C compiler to catch errors in which things other
** than MR_TypeCtorDescs are given as arguments to macros that depend on their
** arguments being MR_TypeCtorDescs. The actual value is either a small integer
** or a pointer to a MR_TypeCtorInfo_Struct structure, as described above.
*/
typedef struct MR_TypeCtorDesc_Struct *MR_TypeCtorDesc;

/*
** The maximum arity that can be encoded should be set to twice the maximum
** number of general purpose registers, since an predicate or function having
** more arguments that this would run out of registers when passing the input
** arguments, or the output arguments, or both.
**
** XXX When tuples were added this was reduced to be the maximum number
** of general purpose registers, to reduce the probability that the
** `small' integers for higher-order and tuple types are confused with
** type_ctor_info pointers. This still allows higher-order terms with
** 1024 arguments, which is more than ../LIMITATIONS promises.
*/
#define MR_MAX_VARIABLE_ARITY         MR_MAX_VIRTUAL_REG

/*
** Constructors for the MR_TypeCtorDesc ADT
*/

#define MR_TYPECTOR_DESC_MAKE_PRED(Arity)                               \
        ( (MR_TypeCtorDesc) ((Arity) * 4) )
#define MR_TYPECTOR_DESC_MAKE_FUNC(Arity)                               \
        ( (MR_TypeCtorDesc) ((Arity) * 4 + 1) )
#define MR_TYPECTOR_DESC_MAKE_TUPLE(Arity)                              \
        ( (MR_TypeCtorDesc) ((Arity) * 4 + 2) )
#define MR_TYPECTOR_DESC_MAKE_FIXED_ARITY(type_ctor_info)               \
        ( MR_CHECK_EXPR_TYPE(type_ctor_info, MR_TypeCtorInfo),          \
          (MR_TypeCtorDesc) type_ctor_info )

/*
** Access macros for the MR_TypeCtor ADT.
**
** The MR_TYPECTOR_DESC_GET_VA_* macros should only be called if
** MR_TYPECTOR_DESC_IS_VARIABLE_ARITY() returns true.
** The MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO() macro
** should only be called if MR_TYPECTOR_DESC_IS_VARIABLE_ARITY() returns false.
*/
#define MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(T)                           \
        ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                       \
          (MR_Unsigned) (T) <= (4 * MR_MAX_VARIABLE_ARITY + 2) )
#define MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(T)              \
        ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                       \
          (MR_TypeCtorInfo) (T) )
#define MR_TYPECTOR_DESC_GET_VA_ARITY(T)                                \
        ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                       \
          (MR_Unsigned) (T) / 4 )
#define MR_TYPECTOR_DESC_GET_VA_NAME(T)                                 \
        ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                       \
          (MR_ConstString) (((MR_Unsigned) (T) % 4 == 0)                \
                ? ""pred""                                              \
                : (((MR_Unsigned) (T) % 4 == 1)                         \
                    ? ""func""                                          \
                    : ""{}"" )) )
#define MR_TYPECTOR_DESC_GET_VA_MODULE_NAME(T)                          \
        ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                       \
          (MR_ConstString) ""builtin"" )
#define MR_TYPECTOR_DESC_GET_VA_TYPE_CTOR_INFO(T)                       \
        ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                       \
          ((MR_Unsigned) (T) % 4 == 0)                                  \
                ? MR_TYPE_CTOR_INFO_HO_PRED                             \
                : (((MR_Unsigned) (T) % 4 == 1)                         \
                   ? MR_TYPE_CTOR_INFO_HO_FUNC                          \
                   : MR_TYPE_CTOR_INFO_TUPLE ) )

#endif /* ML_TYPECTORDESC_GUARD */

").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

/* The `#ifndef ... #define ... #endif' guards against multiple inclusion */
#ifndef ML_CONSTRUCT_INFO_GUARD
#define ML_CONSTRUCT_INFO_GUARD

typedef struct ML_Construct_Info_Struct {
    MR_ConstString          functor_name;
    MR_Integer              arity;
    const MR_PseudoTypeInfo *arg_pseudo_type_infos;
    const MR_ConstString    *arg_names;
    MR_TypeCtorRep          type_ctor_rep;
    union {
        const MR_EnumFunctorDesc  *enum_functor_desc;
        const MR_NotagFunctorDesc *notag_functor_desc;
        const MR_DuFunctorDesc    *du_functor_desc;
    }                       functor_info;
} ML_Construct_Info;

#endif

extern  void            ML_type_ctor_and_args(MR_TypeInfo type_info,
                            bool collapse_equivalences,
                            MR_TypeCtorDesc *type_ctor_desc_ptr,
                            MR_Word *arg_type_info_list_ptr);
extern  int     	    ML_get_num_functors(MR_TypeInfo type_info);
extern	MR_Word		    ML_type_params_vector_to_list(int arity,
                            MR_TypeInfoParams type_params);
extern	MR_Word		    ML_arg_name_vector_to_list(int arity,
                            const MR_ConstString *arg_names);
extern	MR_Word		    ML_pseudo_type_info_vector_to_type_info_list(int arity,
                            MR_TypeInfoParams type_params,
                            const MR_PseudoTypeInfo *arg_pseudo_type_infos);
extern  bool    	    ML_get_functors_check_range(int functor_number,
                            MR_TypeInfo type_info,
                            ML_Construct_Info *construct_info);
extern  void    	    ML_copy_arguments_from_list_to_vector(int arity,
                            MR_Word arg_list, MR_Word term_vector);
extern  bool    	    ML_typecheck_arguments(MR_TypeInfo type_info,
                            int arity, MR_Word arg_list,
                            const MR_PseudoTypeInfo *arg_pseudo_type_infos);
extern  MR_TypeInfo	    ML_make_type(int arity, MR_TypeCtorDesc type_ctor_desc,
				             MR_Word arg_type_list);
").

	% A type_ctor_desc is not (quite) a subtype of type_desc,
	% so we use a separate type for it.
:- type type_ctor_desc ---> type_ctor_desc(c_pointer).

:- pragma foreign_proc("C", type_of(_Value::unused) = (TypeInfo::out),
	will_not_call_mercury, "
{
	TypeInfo = TypeInfo_for_T;

	/*
	** We used to collapse equivalences for efficiency here,
	** but that's not always desirable, due to the reverse
	** mode of make_type/2, and efficiency of type_infos
	** probably isn't very important anyway.
	*/
#if 0
	MR_save_transient_registers();
	TypeInfo = (MR_Word) MR_collapse_equivalences(
		(MR_TypeInfo) TypeInfo_for_T);
	MR_restore_transient_registers();
#endif

}
").

:- pragma foreign_proc("C#", type_of(_Value::unused) = (TypeInfo::out),
	will_not_call_mercury, "
{
	TypeInfo = TypeInfo_for_T;
}
").


:- pragma foreign_proc("C", 
	has_type(_Arg::unused, TypeInfo::in), will_not_call_mercury, "
	TypeInfo_for_T = TypeInfo;
").

:- pragma foreign_proc("C#", 
	has_type(_Arg::unused, TypeInfo::in), will_not_call_mercury, "
	TypeInfo_for_T = TypeInfo;
").

% Export this function in order to use it in runtime/mercury_trace_external.c
:- pragma export(type_name(in) = out, "ML_type_name").

type_name(Type) = TypeName :-
	type_ctor_and_args(Type, TypeCtor, ArgTypes),
	type_ctor_name_and_arity(TypeCtor, ModuleName, Name, Arity),
	( Arity = 0 ->
		UnqualifiedTypeName = Name
	;
		( ModuleName = "builtin", Name = "func" ->
			IsFunc = yes
		;
		 	IsFunc = no
		),
		(
			ModuleName = "builtin", Name = "{}"
		->
			type_arg_names(ArgTypes, IsFunc, ArgTypeNames),
			list__append(ArgTypeNames, ["}"], TypeStrings0),
			TypeStrings = ["{" | TypeStrings0],
			string__append_list(TypeStrings, UnqualifiedTypeName)
		;
			IsFunc = yes,
			ArgTypes = [FuncRetType]
		->
			FuncRetTypeName = type_name(FuncRetType),
			string__append_list(
				["((func) = ", FuncRetTypeName, ")"],
				UnqualifiedTypeName)
		;
			type_arg_names(ArgTypes, IsFunc, ArgTypeNames),
			( IsFunc = no ->
				list__append(ArgTypeNames, [")"], TypeStrings0)
			;
				TypeStrings0 = ArgTypeNames
			),
			TypeNameStrings = [Name, "(" | TypeStrings0],
			string__append_list(TypeNameStrings,
				UnqualifiedTypeName)
		)
	),
	( ModuleName = "builtin" ->
		TypeName = UnqualifiedTypeName
	;
		string__append_list([ModuleName, ":",
			UnqualifiedTypeName], TypeName)
	).


	% Turn the types into a list of strings representing an argument
	% list, adding commas as separators as required.  For example:
	% 	["TypeName1", ",", "TypeName2"]
	% If formatting a function type, we close the parentheses around
	% the function's input parameters, e.g.
	% 	["TypeName1", ",", "TypeName2", ") = ", "ReturnTypeName"]
	% It is the caller's reponsibility to add matching parentheses.
:- pred type_arg_names(list(type_desc), bool, list(string)).
:- mode type_arg_names(in, in, out) is det.

type_arg_names([], _, []).
type_arg_names([Type|Types], IsFunc, ArgNames) :-
	Name = type_name(Type),
	( Types = [] ->
		ArgNames = [Name]
	; IsFunc = yes, Types = [FuncReturnType] ->
		FuncReturnName = type_name(FuncReturnType),
		ArgNames = [Name, ") = ", FuncReturnName]
	;
		type_arg_names(Types, IsFunc, Names),
		ArgNames = [Name, ", " | Names]
	).

type_args(Type) = ArgTypes :-
	type_ctor_and_args(Type, _TypeCtor, ArgTypes).

type_ctor_name(TypeCtor) = Name :-
	type_ctor_name_and_arity(TypeCtor, _ModuleName, Name, _Arity).

type_ctor_module_name(TypeCtor) = ModuleName :-
	type_ctor_name_and_arity(TypeCtor, ModuleName, _Name, _Arity).

type_ctor_arity(TypeCtor) = Arity :-
	type_ctor_name_and_arity(TypeCtor, _ModuleName, _Name, Arity).

det_make_type(TypeCtor, ArgTypes) = Type :-
	( make_type(TypeCtor, ArgTypes) = NewType ->
		Type = NewType
	;
		error("det_make_type/2: make_type/2 failed (wrong arity)")
	).

:- pragma foreign_proc("C", type_ctor(TypeInfo::in) = (TypeCtor::out),
	will_not_call_mercury, "
{
	MR_TypeCtorInfo type_ctor_info;
	MR_TypeInfo	type_info;

	MR_save_transient_registers();
	type_info = MR_collapse_equivalences((MR_TypeInfo) TypeInfo);
	MR_restore_transient_registers();

	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

	TypeCtor = (MR_Word) ML_make_type_ctor_desc(type_info, type_ctor_info);
}
").

:- pragma foreign_proc("C#", type_ctor(_TypeInfo::in) = (_TypeCtor::out),
	will_not_call_mercury, "
{
	mercury.runtime.Errors.SORRY(""foreign code for type_ctor"");
	_TypeCtor = null;
}
").


:- pragma foreign_decl("C", "

extern	MR_TypeCtorDesc ML_make_type_ctor_desc(MR_TypeInfo type_info,
				MR_TypeCtorInfo type_ctor_info);

").

:- pragma foreign_code("C", "

MR_TypeCtorDesc
ML_make_type_ctor_desc(MR_TypeInfo type_info, MR_TypeCtorInfo type_ctor_info)
{
	MR_TypeCtorDesc type_ctor_desc;

	if (MR_TYPE_CTOR_INFO_IS_HO_PRED(type_ctor_info)) {
		type_ctor_desc = MR_TYPECTOR_DESC_MAKE_PRED(
			MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(type_info));
		if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
			MR_fatal_error(""std_util:ML_make_type_ctor_desc""
				""- arity out of range."");
		}
	} else if (MR_TYPE_CTOR_INFO_IS_HO_FUNC(type_ctor_info)) {
		type_ctor_desc = MR_TYPECTOR_DESC_MAKE_FUNC(
			MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(type_info));
		if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
			MR_fatal_error(""std_util:ML_make_type_ctor_desc""
				""- arity out of range."");
		}
	} else if (MR_TYPE_CTOR_INFO_IS_TUPLE(type_ctor_info)) {
		type_ctor_desc = MR_TYPECTOR_DESC_MAKE_TUPLE(
			MR_TYPEINFO_GET_TUPLE_ARITY(type_info));
		if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
			MR_fatal_error(""std_util:ML_make_type_ctor_desc""
				""- arity out of range."");
		}
	} else {
		type_ctor_desc = MR_TYPECTOR_DESC_MAKE_FIXED_ARITY(
			type_ctor_info);
	}

	return type_ctor_desc;
}

/*
** You need to wrap MR_{save/restore}_transient_registers() around
** calls to this function.
*/

void
ML_type_ctor_and_args(MR_TypeInfo type_info, bool collapse_equivalences,
	MR_TypeCtorDesc *type_ctor_desc_ptr, MR_Word *arg_type_info_list_ptr)
{
	MR_TypeCtorInfo type_ctor_info;
	MR_TypeCtorDesc type_ctor_desc;
	MR_Integer	arity;

	if (collapse_equivalences) {
		type_info = MR_collapse_equivalences(type_info);
	}

	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
	type_ctor_desc = ML_make_type_ctor_desc(type_info, type_ctor_info);
	*type_ctor_desc_ptr = type_ctor_desc;

	if (MR_type_ctor_rep_is_variable_arity(type_ctor_info->type_ctor_rep))
	{
		arity = MR_TYPECTOR_DESC_GET_VA_ARITY(type_ctor_desc);
		*arg_type_info_list_ptr = ML_type_params_vector_to_list(arity,
			MR_TYPEINFO_GET_HIGHER_ORDER_ARG_VECTOR(type_info));
	} else {
		arity = type_ctor_info->arity;
		*arg_type_info_list_ptr = ML_type_params_vector_to_list(arity,
			MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info));
	}
}
").

:- pragma foreign_proc("C", type_ctor_and_args(TypeDesc::in,
		TypeCtorDesc::out, ArgTypes::out), will_not_call_mercury, "
{
	MR_TypeCtorDesc type_ctor_desc;
	MR_TypeInfo	type_info;

	MR_save_transient_registers();

	type_info = (MR_TypeInfo) TypeDesc;
	ML_type_ctor_and_args(type_info, TRUE, &type_ctor_desc, &ArgTypes);
	TypeCtorDesc = (MR_Word) type_ctor_desc;

	MR_restore_transient_registers();
}
").


type_ctor_and_args(TypeDesc::in, TypeCtorDesc::out, ArgTypes::out) :-
	rtti_implementation__type_ctor_and_args(
		rtti_implementation__unsafe_cast(TypeDesc),
		TypeCtorDesc0, ArgTypes0),
	TypeCtorDesc = rtti_implementation__unsafe_cast(TypeCtorDesc0),
	ArgTypes = rtti_implementation__unsafe_cast(ArgTypes0).

	/*
	** This is the forwards mode of make_type/2:
	** given a type constructor and a list of argument
	** types, check that the length of the argument
	** types matches the arity of the type constructor,
	** and if so, use the type constructor to construct
	** a new type with the specified arguments.
	*/

:- pragma foreign_proc("C", 
	make_type(TypeCtorDesc::in, ArgTypes::in) = (TypeDesc::out),
		will_not_call_mercury, "
{
	MR_TypeCtorDesc type_ctor_desc;
	MR_TypeCtorInfo type_ctor_info;
	MR_Word		    arg_type;
	int		        list_length;
	int		        arity;

	type_ctor_desc = (MR_TypeCtorDesc) TypeCtorDesc;

	if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
		arity = MR_TYPECTOR_DESC_GET_VA_ARITY(type_ctor_desc);
	} else {
        type_ctor_info = MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(
            type_ctor_desc);
		arity = type_ctor_info->arity;
	}

	arg_type = ArgTypes;
	for (list_length = 0; ! MR_list_is_empty(arg_type); list_length++) {
		arg_type = MR_list_tail(arg_type);
	}

	if (list_length != arity) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		MR_save_transient_registers();
		TypeDesc = (MR_Word) ML_make_type(arity, type_ctor_desc,
			ArgTypes);
		MR_restore_transient_registers();
		SUCCESS_INDICATOR = TRUE;
	}
}
").

:- pragma foreign_proc("C#", 
	make_type(_TypeCtorDesc::in, _ArgTypes::in) = (_TypeDesc::out),
		will_not_call_mercury, "
{
	mercury.runtime.Errors.SORRY(""make_type"");
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}
").


	/*
	** This is the reverse mode of make_type: given a type,
	** split it up into a type constructor and a list of
	** arguments.
	*/

:- pragma foreign_proc("C", 
	make_type(TypeCtorDesc::out, ArgTypes::out) = (TypeDesc::in),
		will_not_call_mercury, "
{
	MR_TypeCtorDesc type_ctor_desc;
	MR_TypeInfo	type_info;

	MR_save_transient_registers();

	type_info = (MR_TypeInfo) TypeDesc;
	ML_type_ctor_and_args(type_info, FALSE, &type_ctor_desc, &ArgTypes);
	TypeCtorDesc = (MR_Word) type_ctor_desc;

	MR_restore_transient_registers();
}
").

:- pragma foreign_proc("C", type_ctor_name_and_arity(TypeCtorDesc::in,
		TypeCtorModuleName::out, TypeCtorName::out, TypeCtorArity::out),
        will_not_call_mercury, "
{
	MR_TypeCtorDesc type_ctor_desc;

	type_ctor_desc = (MR_TypeCtorDesc) TypeCtorDesc;

	if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
            TypeCtorModuleName = (MR_String) (MR_Word)
                MR_TYPECTOR_DESC_GET_VA_MODULE_NAME(type_ctor_desc);
            TypeCtorName = (MR_String) (MR_Word)
                MR_TYPECTOR_DESC_GET_VA_NAME(type_ctor_desc);
            TypeCtorArity = MR_TYPECTOR_DESC_GET_VA_ARITY(type_ctor_desc);
        } else {
            MR_TypeCtorInfo type_ctor_info;

            type_ctor_info = MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(
                type_ctor_desc);

            /*
            ** We cast away the const-ness of the module and type names,
            ** because MR_String is defined as char *, not const char *.
            */

            TypeCtorModuleName = (MR_String) (MR_Integer)
                type_ctor_info->type_ctor_module_name;
            TypeCtorName = (MR_String) (MR_Integer)
                type_ctor_info->type_ctor_name;
            TypeCtorArity = type_ctor_info->arity;
        }
}
").

:- pragma foreign_proc("C", num_functors(TypeInfo::in) = (Functors::out),
	will_not_call_mercury, "
{
	MR_save_transient_registers();
	Functors = ML_get_num_functors((MR_TypeInfo) TypeInfo);
	MR_restore_transient_registers();
}
").

:- pragma foreign_proc("C", get_functor(TypeDesc::in, FunctorNumber::in,
        FunctorName::out, Arity::out, TypeInfoList::out),
    will_not_call_mercury, "
{
    MR_TypeInfo         type_info;
    int                 arity;
    ML_Construct_Info   construct_info;
    bool                success;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** Get information for this functor number and
        ** store in construct_info. If this is a discriminated union
        ** type and if the functor number is in range, we
        ** succeed.
        */
    MR_save_transient_registers();
    success = ML_get_functors_check_range(FunctorNumber,
                type_info, &construct_info);
    MR_restore_transient_registers();

        /*
        ** Get the functor name and arity, construct the list
        ** of type_infos for arguments.
        */

    if (success) {
        MR_make_aligned_string(FunctorName, (MR_String) (MR_Word)
                construct_info.functor_name);
        arity = construct_info.arity;
        Arity = arity;

        if (MR_TYPE_CTOR_INFO_IS_TUPLE(
                        MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info)))
        {
            MR_save_transient_registers();
            TypeInfoList = ML_type_params_vector_to_list(Arity,
                    MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(type_info));
            MR_restore_transient_registers();
        } else {
            MR_save_transient_registers();
            TypeInfoList = ML_pseudo_type_info_vector_to_type_info_list(
                arity,
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                construct_info.arg_pseudo_type_infos);
            MR_restore_transient_registers();
        }
    }
    SUCCESS_INDICATOR = success;
}
").

get_functor(TypeDesc, I, Functor, Arity, TypeInfoList, ArgNameList) :-
    get_functor_2(TypeDesc, I, Functor, Arity, TypeInfoList, ArgNameList0),
    ArgNameList = map(null_to_no, ArgNameList0).

:- func null_to_no(string) = maybe(string).

null_to_no(S) = ( if null(S) then no else yes(S) ).

:- pred null(string).
:- mode null(in) is semidet.

:- pragma foreign_proc("C", null(S::in), will_not_call_mercury, "
    SUCCESS_INDICATOR = (S == NULL);
").

:- pragma foreign_proc("MC++", null(S::in), will_not_call_mercury, "
    SUCCESS_INDICATOR = (S == NULL);
").

:- pred get_functor_2(type_desc::in, int::in, string::out, int::out,
		list(type_desc)::out, list(string)::out) is semidet.

:- pragma foreign_proc("C", get_functor_2(TypeDesc::in, FunctorNumber::in,
        FunctorName::out, Arity::out, TypeInfoList::out, ArgNameList::out),
    will_not_call_mercury, "
{
    MR_TypeInfo         type_info;
    int                 arity;
    ML_Construct_Info   construct_info;
    bool                success;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** Get information for this functor number and
        ** store in construct_info. If this is a discriminated union
        ** type and if the functor number is in range, we
        ** succeed.
        */
    MR_save_transient_registers();
    success = ML_get_functors_check_range(FunctorNumber,
                type_info, &construct_info);
    MR_restore_transient_registers();

        /*
        ** Get the functor name and arity, construct the list
        ** of type_infos for arguments.
        */

    if (success) {
        MR_make_aligned_string(FunctorName, (MR_String) (MR_Word)
                construct_info.functor_name);
        arity = construct_info.arity;
        Arity = arity;

        if (MR_TYPE_CTOR_INFO_IS_TUPLE(
                        MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info)))
        {
            MR_save_transient_registers();
            TypeInfoList = ML_type_params_vector_to_list(Arity,
                    MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(type_info));
	    ArgNameList = MR_list_empty();
            MR_restore_transient_registers();
        } else {
            MR_save_transient_registers();
            TypeInfoList = ML_pseudo_type_info_vector_to_type_info_list(
                arity,
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                construct_info.arg_pseudo_type_infos);
	    ArgNameList = ML_arg_name_vector_to_list(
		arity,
		construct_info.arg_names);
            MR_restore_transient_registers();
        }
    }
    SUCCESS_INDICATOR = success;
}
").

:- pragma foreign_proc("MC++", get_functor_2(_TypeDesc::in, _FunctorNumber::in,
        _FunctorName::out, _Arity::out, _TypeInfoList::out, _ArgNameList::out),
    will_not_call_mercury, "
	mercury::runtime::Errors::SORRY(""foreign code for get_functor_2"");
	SUCCESS_INDICATOR = FALSE;
").

:- pragma foreign_proc("C", 
	get_functor_ordinal(TypeDesc::in, FunctorNumber::in,
		Ordinal::out), will_not_call_mercury, "
{
    MR_TypeInfo         type_info;
    ML_Construct_Info   construct_info;
    bool                success;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** Get information for this functor number and
        ** store in construct_info. If this is a discriminated union
        ** type and if the functor number is in range, we
        ** succeed.
        */
    MR_save_transient_registers();
    success = ML_get_functors_check_range(FunctorNumber, type_info,
        &construct_info);
    MR_restore_transient_registers();

    if (success) {
        switch (construct_info.type_ctor_rep) {

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
            Ordinal = construct_info.functor_info.
                enum_functor_desc->MR_enum_functor_ordinal;
            break;

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        case MR_TYPECTOR_REP_TUPLE:
            Ordinal = 0;
            break;

        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
        case MR_TYPECTOR_REP_RESERVED_ADDR:
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
            Ordinal = construct_info.functor_info.
                du_functor_desc->MR_du_functor_ordinal;
            break;

        default:
            success = FALSE;

        }
    }
    SUCCESS_INDICATOR = success;
}
").

:- pragma foreign_proc("C", 
	construct(TypeDesc::in, FunctorNumber::in, ArgList::in) = (Term::out),
	will_not_call_mercury, "
{
    MR_TypeInfo         type_info;
    MR_TypeCtorInfo     type_ctor_info;
    MR_Word             new_data;
    ML_Construct_Info   construct_info;
    bool                success;
    MR_DuTypeLayout	du_type_layout;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** Check range of FunctorNum, get info for this
        ** functor.
        */
    MR_save_transient_registers();
    success =
        ML_get_functors_check_range(FunctorNumber, type_info, &construct_info)
        && ML_typecheck_arguments(type_info, construct_info.arity, ArgList,
                construct_info.arg_pseudo_type_infos);
    MR_restore_transient_registers();

        /*
        ** Build the new term in `new_data'.
        */
    if (success) {

        type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

        if (type_ctor_info->type_ctor_rep != construct_info.type_ctor_rep) {
            MR_fatal_error(""std_util:construct: type_ctor_rep mismatch"");
        }

        switch (type_ctor_info->type_ctor_rep) {

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
            new_data = construct_info.functor_info.enum_functor_desc->
                MR_enum_functor_ordinal;
            break;

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            if (MR_list_is_empty(ArgList)) {
                MR_fatal_error(""notag arg list is empty"");
            }

            if (! MR_list_is_empty(MR_list_tail(ArgList))) {
                MR_fatal_error(""notag arg list is too long"");
            }

            new_data = MR_field(MR_UNIV_TAG, MR_list_head(ArgList),
                MR_UNIV_OFFSET_FOR_DATA);
            break;

        case MR_TYPECTOR_REP_RESERVED_ADDR:
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
	    /*
	    ** First check whether the functor we want is one of the
	    ** reserved addresses.
	    */
	    {
		int i;
		MR_ReservedAddrTypeLayout ra_layout;
		int total_reserved_addrs;
		const MR_ReservedAddrFunctorDesc *functor_desc;

		ra_layout = type_ctor_info->type_layout.layout_reserved_addr;
		total_reserved_addrs = ra_layout->MR_ra_num_res_numeric_addrs
			+ ra_layout->MR_ra_num_res_symbolic_addrs;

		for (i = 0; i < total_reserved_addrs; i++) {
		    functor_desc = ra_layout->MR_ra_constants[i];
		    if (functor_desc->MR_ra_functor_ordinal == FunctorNumber)
		    {
		    	new_data = (MR_Word)
			    functor_desc->MR_ra_functor_reserved_addr;

		        /* `break' here would just exit the `for' loop */
			goto end_of_main_switch;
		    }
		}
	    }
		    
	    /*
	    ** Otherwise, it is not one of the reserved addresses,
	    ** so handle it like a normal DU type.
	    */

	    /* fall through */

        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
            {
                const MR_DuFunctorDesc  *functor_desc;
                MR_Word                 arg_list;
                MR_Word                 ptag;
                MR_Word                 arity;
                int                     i;

                functor_desc = construct_info.functor_info.du_functor_desc;
                if (functor_desc->MR_du_functor_exist_info != NULL) {
                    MR_fatal_error(""not yet implemented: construction ""
                        ""of terms containing existentially types"");
                }

                arg_list = ArgList;
                ptag = functor_desc->MR_du_functor_primary;
                switch (functor_desc->MR_du_functor_sectag_locn) {
                case MR_SECTAG_LOCAL:
                    new_data = (MR_Word) MR_mkword(ptag,
                        MR_mkbody((MR_Word)
                            functor_desc->MR_du_functor_secondary));
                    break;

                case MR_SECTAG_REMOTE:
                    arity = functor_desc->MR_du_functor_orig_arity;

                    MR_tag_incr_hp_msg(new_data, ptag, arity + 1,
                        MR_PROC_LABEL, ""<created by std_util:construct/3>"");

                    MR_field(ptag, new_data, 0) =
                        functor_desc->MR_du_functor_secondary;
                    for (i = 0; i < arity; i++) {
                        MR_field(ptag, new_data, i + 1) =
                            MR_field(MR_UNIV_TAG, 
			    	MR_list_head(arg_list),
                                MR_UNIV_OFFSET_FOR_DATA);
                        arg_list = MR_list_tail(arg_list);
                    }

                    break;

                case MR_SECTAG_NONE:
                    arity = functor_desc->MR_du_functor_orig_arity;

                    MR_tag_incr_hp_msg(new_data, ptag, arity,
                        MR_PROC_LABEL, ""<created by std_util:construct/3>"");

                    for (i = 0; i < arity; i++) {
                        MR_field(ptag, new_data, i) =
                            MR_field(MR_UNIV_TAG, 
			    	MR_list_head(arg_list),
                                MR_UNIV_OFFSET_FOR_DATA);
                        arg_list = MR_list_tail(arg_list);
                    }

                    break;
                case MR_SECTAG_VARIABLE:
		    MR_fatal_error(""construct(): cannot construct variable"");
                }

                if (! MR_list_is_empty(arg_list)) {
                    MR_fatal_error(""excess arguments in std_util:construct"");
                }
            }
            break;

        case MR_TYPECTOR_REP_TUPLE:
            {
                int     arity, i;
                MR_Word    arg_list;

                arity = MR_TYPEINFO_GET_TUPLE_ARITY(type_info);
    
                if (arity == 0) {
                    new_data = (MR_Word) NULL;
                } else {
                    MR_incr_hp_msg(new_data, arity, MR_PROC_LABEL,
                            ""<created by std_util:construct/3>"");
            
                    arg_list = ArgList;
                    for (i = 0; i < arity; i++) {
                        MR_field(MR_mktag(0), new_data, i) =
                                MR_field(MR_UNIV_TAG, 
				    MR_list_head(arg_list),
                                    MR_UNIV_OFFSET_FOR_DATA);
                        arg_list = MR_list_tail(arg_list);
                    }

                    if (! MR_list_is_empty(arg_list)) {
                        MR_fatal_error(
                                ""excess arguments in std_util:construct"");
                    }
                }
            }
            break;

        default:
            MR_fatal_error(""bad type_ctor_rep in std_util:construct"");
        }

    end_of_main_switch:

        /*
        ** Create a univ.
        */

        MR_new_univ_on_hp(Term, type_info, new_data);
    }

    SUCCESS_INDICATOR = success;
}
").

:- pragma foreign_proc("C#", 
	make_type(_TypeCtorDesc::out, _ArgTypes::out) = (_TypeDesc::in),
		will_not_call_mercury, "
{
	mercury.runtime.Errors.SORRY(""foreign code for make_type"");
}
").

type_ctor_name_and_arity(TypeCtorDesc0::in, TypeCtorModuleName::out,
		TypeCtorName::out, TypeCtorArity::out) :-
	TypeCtorDesc = rtti_implementation__unsafe_cast(TypeCtorDesc0),
	rtti_implementation__type_ctor_name_and_arity(TypeCtorDesc,
		TypeCtorModuleName, TypeCtorName, TypeCtorArity).


:- pragma foreign_proc("C#", num_functors(_TypeInfo::in) = (Functors::out),
	will_not_call_mercury, "
{
	mercury.runtime.Errors.SORRY(""foreign code for num_functors"");
	// XXX keep the C# compiler quiet
	Functors = 0;
}
").

:- pragma foreign_proc("MC++", get_functor(_TypeDesc::in, _FunctorNumber::in,
        _FunctorName::out, _Arity::out, _TypeInfoList::out),
		will_not_call_mercury, "
{
	mercury::runtime::Errors::SORRY(""foreign code for get_functor"");
}
").

:- pragma foreign_proc("MC++", 
	get_functor_ordinal(_TypeDesc::in, _FunctorNumber::in,
		_Ordinal::out), will_not_call_mercury, "
{
	mercury::runtime::Errors::SORRY(""foreign code for get_functor_ordinal"");
}
").

:- pragma foreign_proc("C#", 
	construct(_TypeDesc::in, _FunctorNumber::in,
		_ArgList::in) = (_Term::out), will_not_call_mercury, "
{
	mercury.runtime.Errors.SORRY(""foreign code for construct"");
	_Term = null;
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}
").

construct_tuple(Args) =
	construct_tuple_2(Args,
		list__map(univ_type, Args),
		list__length(Args)).

:- func construct_tuple_2(list(univ), list(type_desc), int) = univ.

:- pragma foreign_proc("C", 
	construct_tuple_2(Args::in, ArgTypes::in, Arity::in) = (Term::out),
		will_not_call_mercury, "
{
	MR_TypeInfo type_info;
	MR_Word new_data;
	MR_Word arg_value;
	int i;

	/*
	** Construct a type_info for the tuple.
	*/
	MR_save_transient_registers();
	type_info = ML_make_type(Arity, MR_TYPECTOR_DESC_MAKE_TUPLE(Arity),
			ArgTypes);
	MR_restore_transient_registers();

	/*
	** Create the tuple.
	*/
	if (Arity == 0) {
		new_data = (MR_Word) NULL;
	} else {
		MR_incr_hp_msg(new_data, Arity, MR_PROC_LABEL,
			""<created by std_util:construct_tuple/1>"");
		for (i = 0; i < Arity; i++) {
			arg_value = MR_field(MR_UNIV_TAG, 
					MR_list_head(Args),
					MR_UNIV_OFFSET_FOR_DATA);
			MR_field(MR_mktag(0), new_data, i) = arg_value;
			Args = MR_list_tail(Args);
		}
	}

	/*
	** Create a univ.
	*/
	MR_new_univ_on_hp(Term, type_info, new_data);
}
").

:- pragma foreign_proc("C#", 
	construct_tuple_2(_Args::in, _ArgTypes::in, _Arity::in) = (_Term::out),
		will_not_call_mercury, "
{
	mercury.runtime.Errors.SORRY(""construct_tuple_2"");
	_Term = null;
}
").


:- pragma foreign_code("C", "

    /*
    ** Prototypes
    */

static int  ML_get_functor_info(MR_TypeInfo type_info, int functor_number,
                ML_Construct_Info *construct_info);

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

static int
ML_get_functor_info(MR_TypeInfo type_info, int functor_number,
    ML_Construct_Info *construct_info)
{
    MR_TypeCtorInfo     type_ctor_info;

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    construct_info->type_ctor_rep = type_ctor_info->type_ctor_rep;

    switch(type_ctor_info->type_ctor_rep) {

    case MR_TYPECTOR_REP_RESERVED_ADDR:
    case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
    case MR_TYPECTOR_REP_DU:
    case MR_TYPECTOR_REP_DU_USEREQ:
        {
            MR_DuFunctorDesc    *functor_desc;

            if (functor_number < 0 ||
                functor_number >= type_ctor_info->type_ctor_num_functors)
            {
                MR_fatal_error(""ML_get_functor_info: ""
                    ""du functor_number out of range"");
            }

            functor_desc = type_ctor_info->type_functors.
                functors_du[functor_number];
            construct_info->functor_info.du_functor_desc = functor_desc;
            construct_info->functor_name = functor_desc->MR_du_functor_name;
            construct_info->arity = functor_desc->MR_du_functor_orig_arity;
            construct_info->arg_pseudo_type_infos =
                functor_desc->MR_du_functor_arg_types;
            construct_info->arg_names =
                functor_desc->MR_du_functor_arg_names;
        }
        break;

    case MR_TYPECTOR_REP_ENUM:
    case MR_TYPECTOR_REP_ENUM_USEREQ:
        {
            MR_EnumFunctorDesc  *functor_desc;

            if (functor_number < 0 ||
                functor_number >= type_ctor_info->type_ctor_num_functors)
            {
                MR_fatal_error(""ML_get_functor_info: ""
                    ""enum functor_number out of range"");
            }

            functor_desc = type_ctor_info->type_functors.
                functors_enum[functor_number];
            construct_info->functor_info.enum_functor_desc = functor_desc;
            construct_info->functor_name = functor_desc->MR_enum_functor_name;
            construct_info->arity = 0;
            construct_info->arg_pseudo_type_infos = NULL;
            construct_info->arg_names = NULL;
        }
        break;

    case MR_TYPECTOR_REP_NOTAG:
    case MR_TYPECTOR_REP_NOTAG_USEREQ:
    case MR_TYPECTOR_REP_NOTAG_GROUND:
    case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        {
            MR_NotagFunctorDesc *functor_desc;

            if (functor_number != 0) {
                MR_fatal_error(""ML_get_functor_info: ""
                    ""notag functor_number out of range"");
            }

            functor_desc = type_ctor_info->type_functors.functors_notag;
            construct_info->functor_info.notag_functor_desc = functor_desc;
            construct_info->functor_name = functor_desc->MR_notag_functor_name;
            construct_info->arity = 1;
            construct_info->arg_pseudo_type_infos =
                &functor_desc->MR_notag_functor_arg_type;
            construct_info->arg_names =
                &functor_desc->MR_notag_functor_arg_name;
        }
        break;

    case MR_TYPECTOR_REP_EQUIV_GROUND:
    case MR_TYPECTOR_REP_EQUIV:
        return ML_get_functor_info(
            MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                type_ctor_info->type_layout.layout_equiv),
            functor_number, construct_info);

    case MR_TYPECTOR_REP_EQUIV_VAR:
        /*
        ** The current version of the RTTI gives all such equivalence types
        ** the EQUIV type_ctor_rep, not EQUIV_VAR.
        */
        MR_fatal_error(""unexpected EQUIV_VAR type_ctor_rep"");
        break;

    case MR_TYPECTOR_REP_TUPLE:
        construct_info->functor_name = ""{}"";
        construct_info->arity = MR_TYPEINFO_GET_TUPLE_ARITY(type_info);

        /* Tuple types don't have pseudo-type_infos for the functors. */
        construct_info->arg_pseudo_type_infos = NULL;
        construct_info->arg_names = NULL;
        break;

    case MR_TYPECTOR_REP_INT:
    case MR_TYPECTOR_REP_CHAR:
    case MR_TYPECTOR_REP_FLOAT:
    case MR_TYPECTOR_REP_STRING:
    case MR_TYPECTOR_REP_PRED:
    case MR_TYPECTOR_REP_VOID:
    case MR_TYPECTOR_REP_C_POINTER:
    case MR_TYPECTOR_REP_TYPEINFO:
    case MR_TYPECTOR_REP_TYPECLASSINFO:
    case MR_TYPECTOR_REP_ARRAY:
    case MR_TYPECTOR_REP_SUCCIP:
    case MR_TYPECTOR_REP_HP:
    case MR_TYPECTOR_REP_CURFR:
    case MR_TYPECTOR_REP_MAXFR:
    case MR_TYPECTOR_REP_REDOFR:
    case MR_TYPECTOR_REP_REDOIP:
    case MR_TYPECTOR_REP_TRAIL_PTR:
    case MR_TYPECTOR_REP_TICKET:
        return FALSE;

    case MR_TYPECTOR_REP_UNKNOWN:
    default:
        MR_fatal_error(""std_util:construct - unexpected type."");
    }

    return TRUE;
}

    /*
    ** ML_typecheck_arguments:
    **
    ** Given a list of univs (`arg_list'), and a vector of
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
ML_typecheck_arguments(MR_TypeInfo type_info, int arity, MR_Word arg_list,
    const MR_PseudoTypeInfo *arg_pseudo_type_infos)
{
    MR_TypeInfo     arg_type_info;
    MR_TypeInfo     list_arg_type_info;
    int             comp;
    int             i;

        /* Type check list of arguments */

    for (i = 0; i < arity; i++) {
        if (MR_list_is_empty(arg_list)) {
            return FALSE;
        }

        list_arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
            MR_list_head(arg_list), MR_UNIV_OFFSET_FOR_TYPEINFO);

        if (MR_TYPE_CTOR_INFO_IS_TUPLE(
                MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info)))
        {
            arg_type_info = MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(type_info)[i + 1];
        } else {
            arg_type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                arg_pseudo_type_infos[i]);
        }

        comp = MR_compare_type_info(list_arg_type_info, arg_type_info);
        if (comp != MR_COMPARE_EQUAL) {
            return FALSE;
        }
        arg_list = MR_list_tail(arg_list);
    }

        /* List should now be empty */
    return MR_list_is_empty(arg_list);
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
ML_copy_arguments_from_list_to_vector(int arity, MR_Word arg_list,
    MR_Word term_vector)
{
    int i;

    for (i = 0; i < arity; i++) {
        MR_field(MR_mktag(0), term_vector, i) =
            MR_field(MR_UNIV_TAG, MR_list_head(arg_list),
                MR_UNIV_OFFSET_FOR_DATA);
        arg_list = MR_list_tail(arg_list);
    }
}

    /*
    ** ML_make_type(arity, type_ctor_info, arg_types_list):
    **
    ** Construct and return a type_info for a type using the
    ** specified type_ctor for the type constructor,
    ** and using the arguments specified in arg_types_list
    ** for the type arguments (if any).
    **
    ** Assumes that the arity of the type constructor represented
    ** by type_ctor_info and the length of the arg_types_list
    ** are both equal to `arity'.
    **
    ** You need to save and restore transient registers around
    ** calls to this function.
    */

MR_TypeInfo
ML_make_type(int arity, MR_TypeCtorDesc type_ctor_desc, MR_Word arg_types_list)
{
    MR_TypeCtorInfo type_ctor_info;
    MR_Word         *new_type_info_arena;
    MR_TypeInfo     *new_type_info_args;
    int             i;

    /*
    ** We need to treat higher-order and tuple types as a special case here.
    */

    if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
        type_ctor_info = MR_TYPECTOR_DESC_GET_VA_TYPE_CTOR_INFO(
            type_ctor_desc);

        MR_restore_transient_registers();
        MR_incr_hp_atomic_msg(MR_LVALUE_CAST(MR_Word, new_type_info_arena),
            MR_higher_order_type_info_size(arity),
            ""mercury__std_util__ML_make_type"", ""type_info"");
        MR_save_transient_registers();
        MR_fill_in_higher_order_type_info(new_type_info_arena,
            type_ctor_info, arity, new_type_info_args);
    } else {
        type_ctor_info = MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(
            type_ctor_desc);

        if (arity == 0) {
            return (MR_TypeInfo) type_ctor_info;
        }

        MR_restore_transient_registers();
        MR_incr_hp_atomic_msg(MR_LVALUE_CAST(MR_Word, new_type_info_arena),
            MR_first_order_type_info_size(arity),
            ""mercury__std_util__ML_make_type"", ""type_info"");
        MR_save_transient_registers();
        MR_fill_in_first_order_type_info(new_type_info_arena,
            type_ctor_info, new_type_info_args);
    }

    for (i = 1; i <= arity; i++) {
        new_type_info_args[i] = (MR_TypeInfo) MR_list_head(arg_types_list);
        arg_types_list = MR_list_tail(arg_types_list);
    }

    return (MR_TypeInfo) new_type_info_arena;
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
ML_get_functors_check_range(int functor_number, MR_TypeInfo type_info,
    ML_Construct_Info *construct_info)
{
        /*
        ** Check range of functor_number, get functors
        ** vector
        */
    return functor_number < ML_get_num_functors(type_info) &&
        functor_number >= 0 &&
        ML_get_functor_info(type_info, functor_number, construct_info);
}

    /*
    ** ML_type_params_vector_to_list:
    **
    ** Copy `arity' type_infos from the `arg_type_infos' vector, which starts
    ** at index 1, onto the Mercury heap in a list.
    **
    ** You need to save and restore transient registers around
    ** calls to this function.
    */

MR_Word
ML_type_params_vector_to_list(int arity, MR_TypeInfoParams type_params)
{
    MR_TypeInfo arg_type;
    MR_Word     type_info_list;

    MR_restore_transient_registers();
    type_info_list = MR_list_empty();

    while (arity > 0) {
        type_info_list = MR_list_cons((MR_Word) type_params[arity],
		type_info_list);
	--arity;
    }
    MR_save_transient_registers();

    return type_info_list;
}

    /*
    ** ML_arg_name_vector_to_list:
    **
    ** Copy `arity' argument names from the `arg_names' vector, which starts
    ** at index 0, onto the Mercury heap in a list.
    **
    ** You need to save and restore transient registers around
    ** calls to this function.
    */

MR_Word
ML_arg_name_vector_to_list(int arity, const MR_ConstString *arg_names)
{
    MR_TypeInfo arg_type;
    MR_Word     arg_names_list;

    MR_restore_transient_registers();
    arg_names_list = MR_list_empty();

    while (arity > 0) {
	--arity;
        arg_names_list = MR_list_cons((MR_Word) arg_names[arity],
		arg_names_list);
    }
    MR_save_transient_registers();

    return arg_names_list;
}

    /*
    ** ML_pseudo_type_info_vector_to_type_info_list:
    **
    ** Take `arity' pseudo_type_infos from the `arg_pseudo_type_infos' vector,
    ** which starts at index 0, expand them, and copy them onto the heap
    ** in a list.
    **
    ** You need to save and restore transient registers around
    ** calls to this function.
    */

MR_Word
ML_pseudo_type_info_vector_to_type_info_list(int arity,
    MR_TypeInfoParams type_params, const MR_PseudoTypeInfo *arg_pseudo_type_infos)
{
    MR_TypeInfo arg_type;
    MR_Word     type_info_list;

    MR_restore_transient_registers();
    type_info_list = MR_list_empty();

    while (--arity >= 0) {
            /* Get the argument type_info */

            /* Fill in any polymorphic pseudo type_infos */
        MR_save_transient_registers();
        arg_type = MR_create_type_info(type_params,
            arg_pseudo_type_infos[arity]);
        MR_restore_transient_registers();

            /* Look past any equivalences */
        MR_save_transient_registers();
        arg_type = MR_collapse_equivalences(arg_type);
        MR_restore_transient_registers();

            /* Join the argument to the front of the list */
        type_info_list = MR_list_cons((MR_Word) arg_type, type_info_list);
    }
    MR_save_transient_registers();

    return type_info_list;
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
ML_get_num_functors(MR_TypeInfo type_info)
{
    MR_TypeCtorInfo type_ctor_info;
    MR_Integer      functors;

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    switch(type_ctor_info->type_ctor_rep) {
        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
        case MR_TYPECTOR_REP_RESERVED_ADDR:
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
            functors = type_ctor_info->type_ctor_num_functors;
            break;

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        case MR_TYPECTOR_REP_TUPLE:
            functors = 1;
            break;

        case MR_TYPECTOR_REP_EQUIV_VAR:
            /*
            ** The current version of the RTTI gives all such equivalence types
            ** the EQUIV type_ctor_rep, not EQUIV_VAR.
            */
            MR_fatal_error(""unexpected EQUIV_VAR type_ctor_rep"");
            break;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
        case MR_TYPECTOR_REP_EQUIV:
            functors = ML_get_num_functors(
                MR_create_type_info((MR_TypeInfo *) type_info,
                    type_ctor_info->type_layout.layout_equiv));
            break;

        case MR_TYPECTOR_REP_INT:
        case MR_TYPECTOR_REP_CHAR:
        case MR_TYPECTOR_REP_FLOAT:
        case MR_TYPECTOR_REP_STRING:
        case MR_TYPECTOR_REP_PRED:
        case MR_TYPECTOR_REP_VOID:
        case MR_TYPECTOR_REP_C_POINTER:
        case MR_TYPECTOR_REP_TYPEINFO:
        case MR_TYPECTOR_REP_TYPECLASSINFO:
        case MR_TYPECTOR_REP_ARRAY:
        case MR_TYPECTOR_REP_SUCCIP:
        case MR_TYPECTOR_REP_HP:
        case MR_TYPECTOR_REP_CURFR:
        case MR_TYPECTOR_REP_MAXFR:
        case MR_TYPECTOR_REP_REDOFR:
        case MR_TYPECTOR_REP_REDOIP:
        case MR_TYPECTOR_REP_TRAIL_PTR:
        case MR_TYPECTOR_REP_TICKET:
            functors = -1;
            break;

        case MR_TYPECTOR_REP_UNKNOWN:
        default:
            MR_fatal_error(""std_util:ML_get_num_functors :""
                "" unknown type_ctor_rep"");
    }

    return functors;
}

").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

    #include <stdio.h>
	#include ""mercury_library_types.h""		/* for MR_ArrayType */

#ifdef MR_DEEP_PROFILING
    #include ""mercury_deep_profiling.h""
#endif

/* The `#ifndef ... #define ... #endif' guards against multiple inclusion */
#ifndef	ML_EXPAND_INFO_GUARD
#define	ML_EXPAND_INFO_GUARD

typedef struct {
    int                     num_extra_args;
    MR_Word                 *arg_values;
    MR_TypeInfo             *arg_type_infos;
    bool                    can_free_arg_type_infos;
} ML_Expand_Args_Fields;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    MR_ConstString          functor;
    ML_Expand_Args_Fields   args;
} ML_Expand_Functor_Args_Info;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    MR_ConstString          functor;
    ML_Expand_Args_Fields   args;
    bool                    limit_reached;
} ML_Expand_Functor_Args_Limit_Info;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    MR_ConstString          functor_only;
} ML_Expand_Functor_Only_Info;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    ML_Expand_Args_Fields   args_only;
} ML_Expand_Args_Only_Info;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    bool                    chosen_index_exists;
    MR_Word                 *chosen_value_ptr;
    MR_TypeInfo             chosen_type_info;
} ML_Expand_Chosen_Arg_Only_Info;

    /* Prototypes */

extern  void    ML_expand_functor_args(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr,
                    ML_Expand_Functor_Args_Info *expand_info);

extern  void    ML_expand_functor_args_limit(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, int max_arity,
                    ML_Expand_Functor_Args_Limit_Info *expand_info);

extern  void    ML_expand_functor_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr,
                    ML_Expand_Functor_Only_Info *expand_info);

extern  void    ML_expand_args_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr,
                    ML_Expand_Args_Only_Info *expand_info);

extern  void    ML_expand_chosen_arg_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, int chosen,
                    ML_Expand_Chosen_Arg_Only_Info *expand_info);

    /*
    ** NB. ML_arg() is also used by arg_ref and new_arg_ref
    ** in store.m, in trace/mercury_trace_vars.m, and in
    ** extras/trailed_update/tr_store.m.
    */
extern  bool    ML_arg(MR_TypeInfo type_info, MR_Word *term, int arg_index,
                    MR_TypeInfo *arg_type_info_ptr, MR_Word **argument_ptr);

    /*
    ** NB. ML_named_arg_num() is used in mercury_trace_vars.c.
    */
extern  bool    ML_named_arg_num(MR_TypeInfo type_info, MR_Word *term_ptr,
                    const char *arg_name, int *arg_num_ptr);

/*
** The following macros factor out the common parts of the various
** deconstruction predicates.
*/

    /*
    ** Check for attempts to deconstruct a non-canonical type.
    ** Such deconstructions must be cc_multi, which is why we treat
    ** violations of this as runtime errors in det deconstruction
    ** predicates.
    ** (There ought to be cc_multi versions of those predicates.)
    */
#define ML_abort_if_type_is_noncanonical(ei, predname)              \
    do {                                                            \
        if ((ei).non_canonical_type) {                              \
            MR_fatal_error(""called "" predname "" for a type ""    \
                ""with a user-defined equality predicate"");        \
        }                                                           \
    } while (0)

#endif

#define ML_deconstruct_get_functor(ei, functor_field, var)          \
    do {                                                            \
        MR_make_aligned_string(MR_LVALUE_CAST(MR_ConstString, var), \
            (ei).functor_field);                                    \
    } while (0)

#define ML_deconstruct_get_arity(ei, var)                           \
    do {                                                            \
        var = (ei).arity;                                           \
    } while (0)

#define ML_deconstruct_get_arg_list(ei, args_field, var)            \
    do {                                                            \
        int     i;                                                  \
                                                                    \
        var = MR_list_empty_msg(MR_PROC_LABEL);                     \
        i = (ei).arity;                                             \
                                                                    \
        while (--i >= 0) {                                          \
            MR_Word arg;                                            \
                                                                    \
                /* Create an argument on the heap */                \
            MR_new_univ_on_hp(arg,                                  \
                (ei).args_field.arg_type_infos[i],                  \
                (ei).args_field.arg_values[i +                      \
                    (ei).args_field.num_extra_args]);               \
                                                                    \
                /* Join the argument to the front of the list */    \
            var = MR_list_cons_msg(arg, var, MR_PROC_LABEL);        \
        }                                                           \
    } while (0)

    /*
    ** Free any arg_type_infos allocated by the ML_expand variant.
    ** Should be called after we have used them for the last time.
    */
#define ML_deconstruct_free_allocated_arg_type_infos(ei, args_field)\
    do {                                                            \
        if ((ei).args_field.can_free_arg_type_infos) {              \
            MR_GC_free((ei).args_field.arg_type_infos);             \
        }                                                           \
    } while (0)

").

:- pragma foreign_code("C", "

#define EXPAND_FUNCTION_NAME        ML_expand_functor_args
#define EXPAND_TYPE_NAME            ML_Expand_Functor_Args_Info
#define EXPAND_FUNCTOR_FIELD        functor
#define EXPAND_ARGS_FIELD           args
#include ""mercury_ml_expand_body.h""
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_FUNCTOR_FIELD
#undef  EXPAND_ARGS_FIELD

#define EXPAND_FUNCTION_NAME        ML_expand_functor_args_limit
#define EXPAND_TYPE_NAME            ML_Expand_Functor_Args_Limit_Info
#define EXPAND_FUNCTOR_FIELD        functor
#define EXPAND_ARGS_FIELD           args
#define EXPAND_APPLY_LIMIT
#include ""mercury_ml_expand_body.h""
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_FUNCTOR_FIELD
#undef  EXPAND_ARGS_FIELD
#undef  EXPAND_APPLY_LIMIT

#define EXPAND_FUNCTION_NAME        ML_expand_functor_only
#define EXPAND_TYPE_NAME            ML_Expand_Functor_Only_Info
#define EXPAND_FUNCTOR_FIELD        functor_only
#include ""mercury_ml_expand_body.h""
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_FUNCTOR_FIELD

#define EXPAND_FUNCTION_NAME        ML_expand_args_only
#define EXPAND_TYPE_NAME            ML_Expand_Args_Only_Info
#define EXPAND_ARGS_FIELD           args_only
#include ""mercury_ml_expand_body.h""
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_ARGS_FIELD

#define EXPAND_FUNCTION_NAME        ML_expand_chosen_arg_only
#define EXPAND_TYPE_NAME            ML_Expand_Chosen_Arg_Only_Info
#define EXPAND_CHOSEN_ARG
#include ""mercury_ml_expand_body.h""
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_CHOSEN_ARG

/*
** ML_arg() is a subroutine used to implement arg/2, argument/2,
** and also store__arg_ref/5 in store.m.
** It takes the address of a term, its type, and an argument index.
** If the selected argument exists, it succeeds and returns the address
** of the argument, and its type; if it doesn't, it fails (i.e. returns FALSE).
**
** You need to wrap MR_{save/restore}_transient_hp() around
** calls to this function.
*/

bool
ML_arg(MR_TypeInfo type_info, MR_Word *term_ptr, int arg_index,
    MR_TypeInfo *arg_type_info_ptr, MR_Word **arg_ptr)
{
    ML_Expand_Chosen_Arg_Only_Info	expand_info;

    ML_expand_chosen_arg_only(type_info, term_ptr, arg_index, &expand_info);
    ML_abort_if_type_is_noncanonical(expand_info, ""argument/2"");

        /* Check range */
    if (expand_info.chosen_index_exists) {
        *arg_type_info_ptr = expand_info.chosen_type_info;
        *arg_ptr = expand_info.chosen_value_ptr;
		return TRUE;
    }

    return FALSE;
}

/*
** ML_named_arg_num() takes the address of a term, its type, and an argument
** name. If the given term has an argument with the given name, it succeeds and
** returns the argument number (counted starting from 0) of the argument;
** if it doesn't, it fails (i.e. returns FALSE).
**
** You need to wrap MR_{save/restore}_transient_hp() around
** calls to this function.
*/

bool
ML_named_arg_num(MR_TypeInfo type_info, MR_Word *term_ptr,
    const char *arg_name, int *arg_num_ptr)
{
    MR_TypeCtorInfo             type_ctor_info;
    MR_DuTypeLayout       	du_type_layout;
    const MR_DuPtagLayout       *ptag_layout;
    const MR_DuFunctorDesc      *functor_desc;
    const MR_NotagFunctorDesc   *notag_functor_desc;
    MR_Word                     data;
    int                         ptag;
    MR_Word                     sectag;
    MR_TypeInfo                 eqv_type_info;
    int                         i;

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    switch (type_ctor_info->type_ctor_rep) {
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
        case MR_TYPECTOR_REP_RESERVED_ADDR:
	    {
		MR_ReservedAddrTypeLayout ra_layout;
		
	    	ra_layout = type_ctor_info->type_layout.layout_reserved_addr;
		data = *term_ptr;
		
		/*
		** First check if this value is one of
		** the numeric reserved addresses.
		*/
		if ((MR_Unsigned) data <
		    (MR_Unsigned) ra_layout->MR_ra_num_res_numeric_addrs)
		{
		    /*
		    ** If so, it must be a constant, and constants never have
		    ** any arguments.
		    */
		    return FALSE;
		}

		/*
		** Next check if this value is one of the
		** the symbolic reserved addresses.
		*/
		for (i = 0; i < ra_layout->MR_ra_num_res_symbolic_addrs; i++) {
		    if (data ==
		    	(MR_Word) ra_layout->MR_ra_res_symbolic_addrs[i])
		    {
			return FALSE;
		    }
		}
		    
		/*
		** Otherwise, it is not one of the reserved addresses,
		** so handle it like a normal DU type.
		*/
		du_type_layout = ra_layout->MR_ra_other_functors;
		goto du_type;
	    }


        case MR_TYPECTOR_REP_DU_USEREQ:
        case MR_TYPECTOR_REP_DU:
            data = *term_ptr;
	    du_type_layout = type_ctor_info->type_layout.layout_du;
	    /* fall through */

	/*
	** This label handles both the DU case and the second half of the
	** RESERVED_ADDR case.  `du_type_layout' and `data' must both be
	** set before this code is entered.
	*/
	du_type:
            ptag = MR_tag(data);
            ptag_layout = &du_type_layout[ptag];

            switch (ptag_layout->MR_sectag_locn) {
                case MR_SECTAG_NONE:
                    functor_desc = ptag_layout->MR_sectag_alternatives[0];
                    break;
                case MR_SECTAG_LOCAL:
                    sectag = MR_unmkbody(data);
                    functor_desc =
                        ptag_layout->MR_sectag_alternatives[sectag];
                    break;
                case MR_SECTAG_REMOTE:
                    sectag = MR_field(ptag, data, 0);
                    functor_desc =
                        ptag_layout->MR_sectag_alternatives[sectag];
                    break;
                case MR_SECTAG_VARIABLE:
		    MR_fatal_error(""ML_named_arg_num(): unexpected variable"");
            }

            if (functor_desc->MR_du_functor_arg_names == NULL) {
                return FALSE;
            }

            for (i = 0; i < functor_desc->MR_du_functor_orig_arity; i++) {
                if (functor_desc->MR_du_functor_arg_names[i] != NULL
                && streq(arg_name, functor_desc->MR_du_functor_arg_names[i]))
                {
                    *arg_num_ptr = i;
                    return TRUE;
                }
            }

            return FALSE;

        case MR_TYPECTOR_REP_EQUIV:
            eqv_type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                type_ctor_info->type_layout.layout_equiv);
            return ML_named_arg_num(eqv_type_info, term_ptr, arg_name,
                arg_num_ptr);

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            eqv_type_info = MR_pseudo_type_info_is_ground(
                type_ctor_info->type_layout.layout_equiv);
            return ML_named_arg_num(eqv_type_info, term_ptr, arg_name,
                arg_num_ptr);

        case MR_TYPECTOR_REP_EQUIV_VAR:
            /*
            ** The current version of the RTTI gives all such equivalence types
            ** the EQUIV type_ctor_rep, not EQUIV_VAR.
            */
            MR_fatal_error(""unexpected EQUIV_VAR type_ctor_rep"");
            break;

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            notag_functor_desc = type_ctor_info->type_functors.functors_notag;

            if (notag_functor_desc->MR_notag_functor_arg_name != NULL
            && streq(arg_name, notag_functor_desc->MR_notag_functor_arg_name))
            {
                *arg_num_ptr = 0;
                return TRUE;
            }

            return FALSE;

        default:
            return FALSE;
    }
}

").

%-----------------------------------------------------------------------------%

    % Code for functor, arg and deconstruct.

:- pragma foreign_proc("C", functor(Term::in, Functor::out, Arity::out),
    will_not_call_mercury, "
{
    MR_TypeInfo     			type_info;
    ML_Expand_Functor_Only_Info	expand_info;

    type_info = (MR_TypeInfo) TypeInfo_for_T;

    MR_save_transient_registers();
    ML_expand_functor_only(type_info, &Term, &expand_info);
    MR_restore_transient_registers();

    ML_abort_if_type_is_noncanonical(expand_info, ""functor/3"");
    ML_deconstruct_get_functor(expand_info, functor_only, Functor);
    ML_deconstruct_get_arity(expand_info, Arity);
}").

/*
** N.B. any modifications to arg/2 might also require similar
** changes to store__arg_ref in store.m.
*/

:- pragma foreign_proc("C", arg(Term::in, ArgumentIndex::in) = (Argument::out),
        will_not_call_mercury, "
{
    MR_TypeInfo type_info;
    MR_TypeInfo exp_arg_type_info;
    MR_TypeInfo arg_type_info;
    MR_Word     *argument_ptr;
    bool        success;
    int         comparison_result;

    type_info = (MR_TypeInfo) TypeInfo_for_T;
    exp_arg_type_info = (MR_TypeInfo) TypeInfo_for_ArgT;

    MR_save_transient_registers();
    success = ML_arg(type_info, &Term, ArgumentIndex,
        &arg_type_info, &argument_ptr);

    if (success) {
        /* compare the actual type of the argument with its expected type */
        comparison_result = MR_compare_type_info(arg_type_info,
            exp_arg_type_info);
        success = (comparison_result == MR_COMPARE_EQUAL);

        if (success) {
            Argument = *argument_ptr;
        }
    }

    MR_restore_transient_registers();
    SUCCESS_INDICATOR = success;
}").

:- pragma foreign_proc("C",
	argument(Term::in, ArgumentIndex::in) = (ArgumentUniv::out),
        will_not_call_mercury, "
{
    MR_TypeInfo type_info;
    MR_TypeInfo arg_type_info;
    MR_Word     *argument_ptr;
    bool        success;

    type_info = (MR_TypeInfo) TypeInfo_for_T;

    MR_save_transient_registers();
    success = ML_arg(type_info, &Term, ArgumentIndex,
        &arg_type_info, &argument_ptr);
    MR_restore_transient_registers();

    if (success) {
        /* Allocate enough room for a univ */
        MR_new_univ_on_hp(ArgumentUniv, arg_type_info, *argument_ptr);
    }

    SUCCESS_INDICATOR = success;
}").

:- pragma foreign_proc("MC++", functor(_Term::in, _Functor::out, _Arity::out),
    will_not_call_mercury, "
{
	mercury::runtime::Errors::SORRY(""foreign code for functor"");
}").

/*
** N.B. any modifications to arg/2 might also require similar
** changes to store__arg_ref in store.m.
*/

:- pragma foreign_proc("C#", 
	arg(_Term::in, _ArgumentIndex::in) = (_Argument::out),
        will_not_call_mercury, "
{
	mercury.runtime.Errors.SORRY(""foreign code for arg"");
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}").

:- pragma foreign_proc("C#",
	argument(_Term::in, _ArgumentIndex::in) = (_ArgumentUniv::out),
        will_not_call_mercury, "
{
	mercury.runtime.Errors.SORRY(""foreign code for argument"");
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}").

det_arg(Type, ArgumentIndex) = Argument :-
    (
        arg(Type, ArgumentIndex) = Argument0
    ->
        Argument = Argument0
    ;
        ( argument(Type, ArgumentIndex) = _ArgumentUniv ->
            error("det_arg: argument number out of range")
        ;
            error("det_arg: argument had wrong type")
        )
    ).

det_argument(Type, ArgumentIndex) = Argument :-
    (
        argument(Type, ArgumentIndex) = Argument0
    ->
        Argument = Argument0
    ;
        error("det_argument: argument out of range")
    ).

:- pragma foreign_proc("C", 
	deconstruct(Term::in, Functor::out, Arity::out,
        Arguments::out), will_not_call_mercury, "
{
    ML_Expand_Functor_Args_Info	expand_info;
    MR_TypeInfo         		type_info;

    type_info = (MR_TypeInfo) TypeInfo_for_T;

    MR_save_transient_registers();
    ML_expand_functor_args(type_info, &Term, &expand_info);
    MR_restore_transient_registers();

    ML_abort_if_type_is_noncanonical(expand_info, ""deconstruct/4"");
    ML_deconstruct_get_functor(expand_info, functor, Functor);
    ML_deconstruct_get_arity(expand_info, Arity);
    ML_deconstruct_get_arg_list(expand_info, args, Arguments);
    ML_deconstruct_free_allocated_arg_type_infos(expand_info, args);
}").

:- pragma foreign_proc("C", 
	limited_deconstruct(Term::in, MaxArity::in, Functor::out, Arity::out,
        Arguments::out), will_not_call_mercury, "
{
    ML_Expand_Functor_Args_Limit_Info	expand_info;
    MR_TypeInfo         				type_info;

    type_info = (MR_TypeInfo) TypeInfo_for_T;

    MR_save_transient_registers();
    ML_expand_functor_args_limit(type_info, &Term, MaxArity, &expand_info);
    MR_restore_transient_registers();

    ML_abort_if_type_is_noncanonical(expand_info, ""limited_deconstruct/5"");

    if (expand_info.limit_reached) {
        SUCCESS_INDICATOR = FALSE;
    } else {
        SUCCESS_INDICATOR = TRUE;

        ML_deconstruct_get_functor(expand_info, functor, Functor);
        ML_deconstruct_get_arity(expand_info, Arity);
        ML_deconstruct_get_arg_list(expand_info, args, Arguments);
        ML_deconstruct_free_allocated_arg_type_infos(expand_info, args);
    }
}").

deconstruct(Term::in, Functor::out, Arity::out, Arguments::out) :-
	rtti_implementation__deconstruct(Term, Functor, Arity, Arguments).

:- pragma foreign_proc("MC++", 
	limited_deconstruct(_Term::in, _MaxArity::in, _Functor::out,
	_Arity::out, _Arguments::out),
	[will_not_call_mercury], "
{
	mercury::runtime::Errors::SORRY(""foreign code for limited_deconstruct"");
	SUCCESS_INDICATOR = FALSE;
}").

get_functor_info(Univ, FunctorInfo) :-
    ( univ_to_type(Univ, Int) ->
        FunctorInfo = functor_integer(Int)
    ; univ_to_type(Univ, Float) ->
        FunctorInfo = functor_float(Float)
    ; univ_to_type(Univ, String) ->
        FunctorInfo = functor_string(String)
    ; get_enum_functor_info(Univ, Enum) ->
        FunctorInfo = functor_enum(Enum)
    %
    % XXX we should handle reserved_addr types here
    %
    ; get_du_functor_info(Univ, Where, Ptag, Sectag, Args) ->
        ( Where = 0 ->
            FunctorInfo = functor_unshared(Ptag, Args)
        ; Where > 0 ->
            FunctorInfo = functor_remote(Ptag, Sectag, Args)
        ;
            FunctorInfo = functor_local(Ptag, Sectag)
        )
    ; get_notag_functor_info(Univ, ExpUniv) ->
        FunctorInfo = functor_notag(ExpUniv)
    ; get_equiv_functor_info(Univ, ExpUniv) ->
        FunctorInfo = functor_equiv(ExpUniv)
    ;
        fail
    ).

    % Given a value of an arbitrary type, succeed if its type is defined
    % as a notag type, and return a univ which bundles up the value
    % with the type of the single function symbol of the notag type.
:- pred get_notag_functor_info(univ::in, univ::out) is semidet.

:- pragma foreign_proc("C", 
	get_notag_functor_info(Univ::in, ExpUniv::out),
	will_not_call_mercury, "
{
    MR_TypeInfo         type_info;
    MR_TypeInfo         exp_type_info;
    MR_TypeCtorInfo     type_ctor_info;
    MR_NotagFunctorDesc *functor_desc;
    MR_Word             value;

    MR_unravel_univ(Univ, type_info, value);
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    switch (type_ctor_info->type_ctor_rep) {
        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
            functor_desc = type_ctor_info->type_functors.functors_notag;
            exp_type_info = MR_pseudo_type_info_is_ground(
                functor_desc->MR_notag_functor_arg_type);
            MR_new_univ_on_hp(ExpUniv, exp_type_info, value);
            SUCCESS_INDICATOR = TRUE;
            break;

        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            functor_desc = type_ctor_info->type_functors.functors_notag;
            exp_type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                functor_desc->MR_notag_functor_arg_type);
            MR_new_univ_on_hp(ExpUniv, exp_type_info, value);
            SUCCESS_INDICATOR = TRUE;
            break;

        default:
            SUCCESS_INDICATOR = FALSE;
            break;
    }
}").

:- pragma foreign_proc("MC++", 
	get_notag_functor_info(_Univ::in, _ExpUniv::out),
	will_not_call_mercury, "
{
	mercury::runtime::Errors::SORRY(""foreign code for get_notag_functor_info"");
}").

    % Given a value of an arbitrary type, succeed if its type is defined
    % as an equivalence type, and return a univ which bundles up the value
    % with the equivalent type. (I.e. this removes one layer of equivalence
    % from the type stored in the univ.)
:- pred get_equiv_functor_info(univ::in, univ::out) is semidet.

:- pragma foreign_proc("C",
	get_equiv_functor_info(Univ::in, ExpUniv::out),
    will_not_call_mercury, "
{
    MR_TypeInfo     type_info;
    MR_TypeInfo     exp_type_info;
    MR_TypeCtorInfo type_ctor_info;
    MR_Word         value;

    MR_unravel_univ(Univ, type_info, value);
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    switch (type_ctor_info->type_ctor_rep) {
        case MR_TYPECTOR_REP_EQUIV:
            exp_type_info = MR_pseudo_type_info_is_ground(
                type_ctor_info->type_layout.layout_equiv);
            MR_new_univ_on_hp(ExpUniv, exp_type_info, value);
            SUCCESS_INDICATOR = TRUE;
            break;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            exp_type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                type_ctor_info->type_layout.layout_equiv);
            MR_new_univ_on_hp(ExpUniv, exp_type_info, value);
            SUCCESS_INDICATOR = TRUE;
            break;

        default:
            SUCCESS_INDICATOR = FALSE;
            break;
    }
}").

:- pragma foreign_proc("MC++",
	get_equiv_functor_info(_Univ::in, _ExpUniv::out),
    will_not_call_mercury, "
{
	mercury::runtime::Errors::SORRY(""foreign code for get_equiv_functor_info"");
}").

    % Given a value of an arbitrary type, succeed if it is an enum type,
    % and return the integer value corresponding to the value.
:- pred get_enum_functor_info(univ::in, int::out) is semidet.

:- pragma foreign_proc("C",
	get_enum_functor_info(Univ::in, Enum::out),
	will_not_call_mercury, "
{
    MR_TypeInfo     type_info;
    MR_TypeCtorInfo type_ctor_info;
    MR_Word         value;

    MR_unravel_univ(Univ, type_info, value);
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    switch (type_ctor_info->type_ctor_rep) {
        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
            Enum = (MR_Integer) value;
            SUCCESS_INDICATOR = TRUE;
            break;

        default:
            SUCCESS_INDICATOR = FALSE;
            break;
    }
}").

:- pragma foreign_proc("MC++",
	get_enum_functor_info(_Univ::in, _Enum::out),
	will_not_call_mercury, "
{
	mercury::runtime::Errors::SORRY(""foreign code for get_enum_functor_info"");
}").

    % Given a value of an arbitrary type, succeed if it is a general du type
    % (i.e. non-enum, non-notag du type), and return the top function symbol's
    % arguments as well as its tag information: an indication of where the
    % secondary tag is (-1 for local secondary tag, 0 for nonexistent secondary
    % tag, and 1 for remote secondary tag), as well as the primary and
    % secondary tags themselves (the secondary tag argument will be meaningful
    % only if the secondary tag exists, of course).
:- pred get_du_functor_info(univ::in, int::out, int::out, int::out,
    list(univ)::out) is semidet.

:- pragma foreign_proc("C", get_du_functor_info(Univ::in, Where::out,
    Ptag::out, Sectag::out, Args::out), will_not_call_mercury, "
{
    MR_TypeInfo             type_info;
    MR_TypeCtorInfo         type_ctor_info;
    MR_DuPtagLayout         *ptag_layout;
    const MR_DuFunctorDesc  *functor_desc;
    MR_Word                 value;
    MR_Word                 *arg_vector;
    int                     i;

    MR_unravel_univ(Univ, type_info, value);
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    switch (type_ctor_info->type_ctor_rep) {
        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
            SUCCESS_INDICATOR = TRUE;
            Ptag = MR_tag(value);
            ptag_layout = &type_ctor_info->type_layout.layout_du[Ptag];

            switch(ptag_layout->MR_sectag_locn) {
                case MR_SECTAG_LOCAL:
                    Where = -1;
                    Sectag = MR_unmkbody(value);
                    Args = MR_list_empty();
                    break;

                case MR_SECTAG_REMOTE:
                case MR_SECTAG_NONE:
                    if (ptag_layout->MR_sectag_locn == MR_SECTAG_NONE) {
                        Where = 0;
                        arg_vector = (MR_Word *) MR_body(value, Ptag);
                        Sectag = 0;
                    } else {
                        Where = 1;
                        arg_vector = (MR_Word *) MR_body(value, Ptag);
                        Sectag = arg_vector[0];
                        arg_vector++;
                    }

                    functor_desc = ptag_layout->MR_sectag_alternatives[Sectag];
                    if (functor_desc->MR_du_functor_exist_info != NULL) {
                        SUCCESS_INDICATOR = FALSE;
                        break;
                    }

                    Args = MR_list_empty_msg(MR_PROC_LABEL);
                    for (i = functor_desc->MR_du_functor_orig_arity - 1;
                        i >= 0; i--)
                    {
                        MR_Word         arg;
                        MR_TypeInfo     arg_type_info;

                        if (MR_arg_type_may_contain_var(functor_desc, i)) {
                            arg_type_info = MR_create_type_info_maybe_existq(
                                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(
                                    type_info),
                                functor_desc->MR_du_functor_arg_types[i],
                                arg_vector, functor_desc);
                        } else {
                            arg_type_info = MR_pseudo_type_info_is_ground(
                                functor_desc->MR_du_functor_arg_types[i]);
                        }

                        MR_new_univ_on_hp(arg,
                            arg_type_info, arg_vector[i]);
                        Args = MR_list_cons_msg(arg, Args, MR_PROC_LABEL);
                    }
                    break;

                case MR_SECTAG_VARIABLE:
		    MR_fatal_error(
		        ""get_du_functor_info: unexpected variable"");

                default:
                    MR_fatal_error(
                        ""get_du_functor_info: unknown sectag locn"");
            }
            break;

        default:
            SUCCESS_INDICATOR = FALSE;
            break;
    }
}").

:- pragma foreign_proc("MC++", get_du_functor_info(_Univ::in, _Where::out,
    _Ptag::out, _Sectag::out, _Args::out), will_not_call_mercury, "
{
	mercury::runtime::Errors::SORRY(""foreign code for get_du_functor_info"");
}").

%-----------------------------------------------------------------------------%

    % This predicate returns the type_info for the type std_util:type_info.
    % It is intended for use from C code, since Mercury code can access
    % this type_info easily enough even without this predicate.
:- pred get_type_info_for_type_info(type_desc).
:- mode get_type_info_for_type_info(out) is det.

:- pragma export(get_type_info_for_type_info(out),
    "ML_get_type_info_for_type_info").

get_type_info_for_type_info(TypeInfo) :-
    Type = type_of(1),
    TypeInfo = type_of(Type).

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

dynamic_cast(X, Y) :-
	univ_to_type(univ(X), Y).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
