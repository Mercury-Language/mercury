%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: deconstruct.m.
% Main author: zs.
% Stability: low.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module deconstruct.

:- interface.

:- import_module std_util, list.

	% Values of type noncanon_handling are intended to control how
	% predicates that deconstruct terms behave when they find that
	% the term they are about to deconstruct is of a noncanonical type,
	% i.e. of a type in which a single logical value may have more than one
	% concrete representation.
	%
	% The value `do_not_allow' means that in such circumstances the
	% predicate should abort.
	%
	% The value `canonicalize' means that in such circumstances the
	% predicate should return a constant giving the identity of the type,
	% regardless of the actual value of the term.
	%
	% The value `include_details_cc' means that in such circumstances
	% the predicate should proceed as if the term were of a canonical type.
	% Use of this option requires a committed choice context.

:- type noncanon_handling
	--->	do_not_allow
	;	canonicalize
	;	include_details_cc.

:- inst do_not_allow ---> do_not_allow.
:- inst canonicalize ---> canonicalize.
:- inst include_details_cc ---> include_details_cc.

	% functor, argument and deconstruct and their variants take any type
	% (including univ), and return representation information for that type.
	%
	% The string representation of the functor that these predicates
	% return is:
	%
	% 	- for user defined types with standard equality, the functor
	%	  that is given in the type definition. For lists, this means
	%	  the functors [|]/2 and []/0 are used, even if the list uses
	%	  the [....] shorthand.
	%	- for user-defined types with user-defined equality, the
	%	  functor will be of the form <<module:type/arity>>, except
	%	  with include_details_cc, in which case the type will be
	%	  handled as if it had standard equality.
	%	- for integers, the string is a base 10 number;
	%	  positive integers have no sign.
	%	- for floats, the string is a floating point, base 10 number;
	%	  positive floating point numbers have no sign.
	%	- for strings, the string, inside double quotation marks
	%	- for characters, the character inside single quotation marks
	%	- for predicates, the string <<predicate>>, and for functions,
	%	  the string <<function>>, except with include_details_cc,
	%	  in which case it will be the predicate or function name.
	%	  (The predicate or function name will be artificial for
	%	  predicate and function values created by lambda expressions.)
	%	- for tuples, the string {}.
	%	- for arrays, the string <<array>>.
	%
	% The arity that these predicates return is:
	%
	% 	- for user defined types with standard equality, the arity
	%	  of the functor.
	% 	- for user defined types with user-defined equality, zero,
	%	  except with include_details_cc, in which case the type
	%	  will be handled as if it had standard equality.
	%	- for integers, zero.
	%	- for floats, zero.
	%	- for strings, zero.
	%	- for characters, zero.
	%	- for predicates and functions, zero, except with
	%	  include_details_cc, in which case it will be the number of
	%	  arguments hidden in the closure.
	%	- for tuples, the number of elements in the tuple.
	%	- for arrays, the number of elements in the array.
	%
	% Note that in the current University of Melbourne implementation,
	% the implementations of these predicates depart from the above
	% specification in that with --high-level-code, they do not
	% deconstruct predicate- and function-valued terms even with
	% include_details_cc; instead, they return <<predicate>> or
	% <<function>> (in both cases with arity zero) as appropriate.

	% functor(Data, NonCanon, Functor, Arity)
	%
	% Given a data item (Data), binds Functor to a string
	% representation of the functor and Arity to the arity of this
	% data item. 
	%
:- pred functor(T, noncanon_handling, string, int).
:- mode functor(in, in(do_not_allow), out, out) is det.
:- mode functor(in, in(canonicalize), out, out) is det.
:- mode functor(in, in(include_details_cc), out, out) is cc_multi.
:- mode functor(in, in, out, out) is cc_multi.

	% arg(Data, NonCanon, Index, Argument)
	%
	% Given a data item (Data) and an argument index (Index), starting
	% at 0 for the first argument, binds Argument to that argument of
	% the functor of the data item. If the argument index is out of range
	% -- that is, greater than or equal to the arity of the functor or
	% lower than 0 -- then the call fails.
	%
:- some [ArgT] pred arg(T, noncanon_handling, int, ArgT).
:- mode arg(in, in(do_not_allow), in, out) is semidet.
:- mode arg(in, in(canonicalize), in, out) is semidet.
:- mode arg(in, in(include_details_cc), in, out) is cc_nondet.
:- mode arg(in, in, in, out) is cc_nondet.

	% named_arg(Data, NonCanon, Name, Argument)
	%
	% Same as arg/4, except the chosen argument is specified by giving
	% its name rather than its position. If Data has no argument with that
	% name, named_arg fails.
	%
:- some [ArgT] pred named_arg(T, noncanon_handling, string, ArgT).
:- mode named_arg(in, in(do_not_allow), in, out) is semidet.
:- mode named_arg(in, in(canonicalize), in, out) is semidet.
:- mode named_arg(in, in(include_details_cc), in, out) is cc_nondet.
:- mode named_arg(in, in, in, out) is cc_nondet.

	% det_arg(Data, NonCanon, Index, Argument)
	%
	% Same as arg/4, except that for cases where
	% arg/4 would fail, det_arg/4 will abort.
	%
:- some [ArgT] pred det_arg(T, noncanon_handling, int, ArgT).
:- mode det_arg(in, in(do_not_allow), in, out) is det.
:- mode det_arg(in, in(canonicalize), in, out) is det.
:- mode det_arg(in, in(include_details_cc), in, out) is cc_multi.
:- mode det_arg(in, in, in, out) is cc_multi.

	% det_named_arg(Data, NonCanon, Name, Argument)
	%
	% Same as named_arg/4, except that for cases where
	% named_arg/4 would fail, det_named_arg/4 will abort.
	%
:- some [ArgT] pred det_named_arg(T, noncanon_handling, string, ArgT).
:- mode det_named_arg(in, in(do_not_allow), in, out) is det.
:- mode det_named_arg(in, in(canonicalize), in, out) is det.
:- mode det_named_arg(in, in(include_details_cc), in, out) is cc_multi.
:- mode det_named_arg(in, in, in, out) is cc_multi.

	% deconstruct(Data, NonCanon, Functor, Arity, Arguments)
	%
	% Given a data item (Data), binds Functor to a string
	% representation of the functor, Arity to the arity of this data
	% item, and Arguments to a list of arguments of the functor.
	% The arguments in the list are each of type univ.
	%
	% The cost of calling deconstruct depends greatly on how many arguments
	% Data has. If Data is an array, then each element of the array is
	% considered one of its arguments. Therefore calling deconstruct
	% on large arrays can take a very large amount of memory and a very
	% long time. If you call deconstruct in a situation in which you may
	% pass it a large array, you should probably use limited_deconstruct
	% instead.
	%
:- pred deconstruct(T, noncanon_handling, string, int, list(univ)).
:- mode deconstruct(in, in(do_not_allow), out, out, out) is det.
:- mode deconstruct(in, in(canonicalize), out, out, out) is det.
:- mode deconstruct(in, in(include_details_cc), out, out, out) is cc_multi.
:- mode deconstruct(in, in, out, out, out) is cc_multi.

	% limited_deconstruct(Data, NonCanon, MaxArity,
	%	Functor, Arity, Arguments)
	%
	% limited_deconstruct works like deconstruct, but if the arity of T is
	% greater than MaxArity, limited_deconstruct fails. This is useful in
	% avoiding bad performance in cases where Data may be a large array.
	%
:- pred limited_deconstruct(T, noncanon_handling, int, string, int,
	list(univ)).
:- mode limited_deconstruct(in, in(do_not_allow), in, out, out, out)
	is semidet.
:- mode limited_deconstruct(in, in(canonicalize), in, out, out, out)
	is semidet.
:- mode limited_deconstruct(in, in(include_details_cc), in, out, out, out)
	is cc_nondet.
:- mode limited_deconstruct(in, in, in, out, out, out) is cc_nondet.

:- implementation.
:- interface.

% The rest of the interface is for use by implementors only.

:- type functor_tag_info
	--->	functor_integer(int)
	;	functor_float(float)
	;	functor_string(string)
	;	functor_enum(int)
	;	functor_local(int, int)
	;	functor_remote(int, int, list(univ))
	;	functor_unshared(int, list(univ))
	;	functor_notag(univ)
	;	functor_equiv(univ).

	% get_functor_info is a variant of deconstruct for use by the compiler,
	% specifically prog_rep.m and static_term.m. It differs from
	% deconstruct in two main ways. First, instead of returning the
	% function symbol, it returns implementation information about
	% its tag. Second, it succeeds for just the kinds of terms needed
	% to represent procedure bodies for ordinary procedures. For the time
	% being, these are procedures that do not involve higher order code
	% or tabling.
:- pred get_functor_info(univ::in, functor_tag_info::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, require.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

#include ""mercury_deconstruct.h""
#include ""mercury_deconstruct_macros.h""

").

%-----------------------------------------------------------------------------%

% XXX The no-inline pragmas are necessary because when it inlines a predicate
% defined by foreign_procs, the compiler does not preserve the names of the
% typeinfo variables. Thus these foreign_proc's references to TypeInfo_for_T
% will refer to an undefined variable.

:- pragma no_inline(functor/4).
:- pragma no_inline(arg/4).
:- pragma no_inline(named_arg/4).
:- pragma no_inline(deconstruct/5).
:- pragma no_inline(limited_deconstruct/6).

%-----------------------------------------------------------------------------%

functor(Term, NonCanon, Functor, Arity) :-
	(
		NonCanon = do_not_allow,
		functor_dna(Term, Functor, Arity)
	;
		NonCanon = canonicalize,
		functor_can(Term, Functor, Arity)
	;
		NonCanon = include_details_cc,
		functor_idcc(Term, Functor, Arity)
	).

arg(Term, NonCanon, Index, Argument) :-
	(
		NonCanon = do_not_allow,
		univ_arg_dna(Term, Index, Univ)
	;
		NonCanon = canonicalize,
		univ_arg_can(Term, Index, Univ)
	;
		NonCanon = include_details_cc,
		univ_arg_idcc(Term, Index, Univ)
	),
	Argument = univ_value(Univ).

named_arg(Term, NonCanon, Name, Argument) :-
	(
		NonCanon = do_not_allow,
		univ_named_arg_dna(Term, Name, Univ)
	;
		NonCanon = canonicalize,
		univ_named_arg_can(Term, Name, Univ)
	;
		NonCanon = include_details_cc,
		univ_named_arg_idcc(Term, Name, Univ)
	),
	Argument = univ_value(Univ).

det_arg(Term, NonCanon, Index, Argument) :-
	(
		(
			NonCanon = do_not_allow,
			univ_arg_dna(Term, Index, Univ)
		;
			NonCanon = canonicalize,
			univ_arg_can(Term, Index, Univ)
		;
			NonCanon = include_details_cc,
			univ_arg_idcc(Term, Index, Univ)
		)
	->
		Argument = univ_value(Univ)
	;
		error("det_arg: argument number out of range")
	).

det_named_arg(Term, NonCanon, Name, Argument) :-
	(
		(
			NonCanon = do_not_allow,
			univ_named_arg_dna(Term, Name, Univ)
		;
			NonCanon = canonicalize,
			univ_named_arg_can(Term, Name, Univ)
		;
			NonCanon = include_details_cc,
			univ_named_arg_idcc(Term, Name, Univ)
		)
	->
		Argument = univ_value(Univ)
	;
		error("det_named_arg: no argument with that name")
	).

deconstruct(Term, NonCanon, Functor, Arity, Arguments) :-
	(
		NonCanon = do_not_allow,
		deconstruct_dna(Term, Functor, Arity, Arguments)
	;
		NonCanon = canonicalize,
		deconstruct_can(Term, Functor, Arity, Arguments)
	;
		NonCanon = include_details_cc,
		deconstruct_idcc(Term, Functor, Arity, Arguments)
	).

limited_deconstruct(Term, NonCanon, MaxArity, Functor, Arity, Arguments) :-
	(
		NonCanon = do_not_allow,
		limited_deconstruct_dna(Term, MaxArity,
			Functor, Arity, Arguments)
	;
		NonCanon = canonicalize,
		limited_deconstruct_can(Term, MaxArity,
			Functor, Arity, Arguments)
	;
		NonCanon = include_details_cc,
		limited_deconstruct_idcc(Term, MaxArity,
			Functor, Arity, Arguments)
	).

%-----------------------------------------------------------------------------%

:- pred functor_dna(T::in, string::out, int::out) is det.
:- pred functor_can(T::in, string::out, int::out) is det.
:- pred functor_idcc(T::in, string::out, int::out) is cc_multi.

:- pragma foreign_proc("C",
	functor_dna(Term::in, Functor::out, Arity::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	TYPEINFO_ARG			TypeInfo_for_T
#define	TERM_ARG			Term
#define	FUNCTOR_ARG			Functor
#define	ARITY_ARG			Arity
#define	NONCANON			MR_NONCANON_ABORT
#include ""mercury_ml_functor_body.h""
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	NONCANON
}").

:- pragma foreign_proc("C",
	functor_can(Term::in, Functor::out, Arity::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	TYPEINFO_ARG			TypeInfo_for_T
#define	TERM_ARG			Term
#define	FUNCTOR_ARG			Functor
#define	ARITY_ARG			Arity
#define	NONCANON			MR_NONCANON_ALLOW
#include ""mercury_ml_functor_body.h""
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	NONCANON
}").

:- pragma foreign_proc("C",
	functor_idcc(Term::in, Functor::out, Arity::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	TYPEINFO_ARG			TypeInfo_for_T
#define	TERM_ARG			Term
#define	FUNCTOR_ARG			Functor
#define	ARITY_ARG			Arity
#define	NONCANON			MR_NONCANON_CC
#include ""mercury_ml_functor_body.h""
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	NONCANON
}").

functor_dna(_Term::in, _Functor::out, _Arity::out) :-
	private_builtin__sorry("deconstruct__functor_dna/3").
functor_can(Term::in, Functor::out, Arity::out) :-
	rtti_implementation__deconstruct(Term, Functor, Arity, _Arguments).
functor_idcc(_Term::in, _Functor::out, _Arity::out) :-
	private_builtin__sorry("deconstruct__functor_idcc/3").

%-----------------------------------------------------------------------------%

% XXX These predicates return univs instead of existentially typed arguments
% in order to work around the typechecking bug reported on 30 Jan, 2002
% to the mercury-bugs mailing list, and which has sourceforge bug id 512581.

:- pred univ_arg_dna(T::in, int::in, univ::out) is semidet.
:- pred univ_arg_can(T::in, int::in, univ::out) is semidet.
:- pred univ_arg_idcc(T::in, int::in, univ::out) is cc_nondet.

:- pred univ_named_arg_dna(T::in, string::in, univ::out) is semidet.
:- pred univ_named_arg_can(T::in, string::in, univ::out) is semidet.
:- pred univ_named_arg_idcc(T::in, string::in, univ::out) is cc_nondet.

:- pragma foreign_proc("C",
	univ_arg_dna(Term::in, Index::in, Argument::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		Index
#define	SELECTED_ARG		Argument
#define	SELECTED_TYPE_INFO	TypeInfo_for_ArgT
#define	NONCANON		MR_NONCANON_ABORT
#include ""mercury_ml_arg_body.h""
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	SELECTED_TYPE_INFO
#undef	NONCANON
}").

:- pragma foreign_proc("C",
	univ_arg_can(Term::in, Index::in, Argument::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		Index
#define	SELECTED_ARG		Argument
#define	SELECTED_TYPE_INFO	TypeInfo_for_ArgT
#define	NONCANON		MR_NONCANON_ALLOW
#include ""mercury_ml_arg_body.h""
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	SELECTED_TYPE_INFO
#undef	NONCANON
}").

:- pragma foreign_proc("C",
	univ_arg_idcc(Term::in, Index::in, Argument::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		Index
#define	SELECTED_ARG		Argument
#define	SELECTED_TYPE_INFO	TypeInfo_for_ArgT
#define	NONCANON		MR_NONCANON_CC
#include ""mercury_ml_arg_body.h""
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	SELECTED_TYPE_INFO
#undef	NONCANON
}").

:- pragma foreign_proc("C",
	univ_named_arg_dna(Term::in, Name::in, Argument::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		(MR_ConstString) Name
#define	SELECTED_ARG		Argument
#define	SELECTED_TYPE_INFO	TypeInfo_for_ArgT
#define	NONCANON		MR_NONCANON_ABORT
#define	SELECT_BY_NAME
#include ""mercury_ml_arg_body.h""
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	SELECTED_TYPE_INFO
#undef	NONCANON
#undef	SELECT_BY_NAME
}").

:- pragma foreign_proc("C",
	univ_named_arg_can(Term::in, Name::in, Argument::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		(MR_ConstString) Name
#define	SELECTED_ARG		Argument
#define	SELECTED_TYPE_INFO	TypeInfo_for_ArgT
#define	NONCANON		MR_NONCANON_ALLOW
#define	SELECT_BY_NAME
#include ""mercury_ml_arg_body.h""
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	SELECTED_TYPE_INFO
#undef	NONCANON
#undef	SELECT_BY_NAME
}").

:- pragma foreign_proc("C",
	univ_named_arg_idcc(Term::in, Name::in, Argument::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		(MR_ConstString) Name
#define	SELECTED_ARG		Argument
#define	SELECTED_TYPE_INFO	TypeInfo_for_ArgT
#define	NONCANON		MR_NONCANON_CC
#define	SELECT_BY_NAME
#include ""mercury_ml_arg_body.h""
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	SELECTED_TYPE_INFO
#undef	NONCANON
#undef	SELECT_BY_NAME
}").

univ_arg_dna(_Term::in, _Index::in, _Arg::out) :-
	private_builtin__sorry("deconstruct__univ_arg_dna/3").
univ_arg_can(Term::in, Index::in, Arg::out) :-
	rtti_implementation__deconstruct(Term, _Functor, _Arity, Arguments),
	list__index0(Arguments, Index, Arg).
univ_arg_idcc(_Term::in, _Index::in, _Arg::out) :-
	private_builtin__sorry("deconstruct__univ_arg_idcc/3").

univ_named_arg_dna(_Term::in, _Name::in, _Arg::out) :-
	private_builtin__sorry("deconstruct__univ_named_arg_dna/3").
univ_named_arg_can(_Term::in, _Name::in, _Arg::out) :-
	private_builtin__sorry("deconstruct__univ_named_arg_can/3").
univ_named_arg_idcc(_Term::in, _Name::in, _Arg::out) :-
	private_builtin__sorry("deconstruct__univ_named_arg_idcc/3").

%-----------------------------------------------------------------------------%

:- pred deconstruct_dna(T::in, string::out, int::out, list(univ)::out) is det.
:- pred deconstruct_can(T::in, string::out, int::out, list(univ)::out) is det.
:- pred deconstruct_idcc(T::in, string::out, int::out, list(univ)::out)
	is cc_multi.

:- pred limited_deconstruct_dna(T::in, int::in,
	string::out, int::out, list(univ)::out) is semidet.
:- pred limited_deconstruct_can(T::in, int::in,
	string::out, int::out, list(univ)::out) is semidet.
:- pred limited_deconstruct_idcc(T::in, int::in,
	string::out, int::out, list(univ)::out) is cc_nondet.

:- pragma foreign_proc("C", 
	deconstruct_dna(Term::in, Functor::out, Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#define	NONCANON		MR_NONCANON_ABORT
#include ""mercury_ml_deconstruct_body.h""
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
#undef	NONCANON
}").

:- pragma foreign_proc("C", 
	deconstruct_can(Term::in, Functor::out, Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#define	NONCANON		MR_NONCANON_ALLOW
#include ""mercury_ml_deconstruct_body.h""
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
#undef	NONCANON
}").

:- pragma foreign_proc("C", 
	deconstruct_idcc(Term::in, Functor::out, Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#define	NONCANON		MR_NONCANON_CC
#include ""mercury_ml_deconstruct_body.h""
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
#undef	NONCANON
}").

:- pragma foreign_proc("C", 
	limited_deconstruct_dna(Term::in, MaxArity::in,
		Functor::out, Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Limit_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args_limit
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	MAX_ARITY_ARG		MaxArity
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#define	NONCANON		MR_NONCANON_ABORT
#include ""mercury_ml_deconstruct_body.h""
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	MAX_ARITY_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
#undef	NONCANON
}").

:- pragma foreign_proc("C", 
	limited_deconstruct_can(Term::in, MaxArity::in,
		Functor::out, Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Limit_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args_limit
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	MAX_ARITY_ARG		MaxArity
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#define	NONCANON		MR_NONCANON_ALLOW
#include ""mercury_ml_deconstruct_body.h""
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	MAX_ARITY_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
#undef	NONCANON
}").

:- pragma foreign_proc("C", 
	limited_deconstruct_idcc(Term::in, MaxArity::in,
		Functor::out, Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Limit_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args_limit
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	MAX_ARITY_ARG		MaxArity
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#define	NONCANON		MR_NONCANON_CC
#include ""mercury_ml_deconstruct_body.h""
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	MAX_ARITY_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
#undef	NONCANON
}").

deconstruct_dna(_Term::in, _Functor::out, _Arity::out, _Arguments::out) :-
	private_builtin__sorry("deconstuct__deconstruct_dna/4").
deconstruct_can(Term::in, Functor::out, Arity::out, Arguments::out) :-
	rtti_implementation__deconstruct(Term, Functor, Arity, Arguments).
deconstruct_idcc(_Term::in, _Functor::out, _Arity::out, _Arguments::out) :-
	private_builtin__sorry("deconstuct__deconstruct_idcc/4").

limited_deconstruct_dna(_Term::in, _MaxArity::in,
		_Functor::out, _Arity::out, _Arguments::out) :-
	private_builtin__sorry("deconstuct__limited_deconstruct_dna/5").
limited_deconstruct_can(Term::in, MaxArity::in,
		Functor::out, Arity::out, Arguments::out) :-
	rtti_implementation__deconstruct(Term, Functor, Arity, Arguments),
	Arity =< MaxArity.
limited_deconstruct_idcc(_Term::in, _MaxArity::in,
		_Functor::out, _Arity::out, _Arguments::out) :-
	private_builtin__sorry("deconstuct__limited_deconstruct_idcc/5").

%-----------------------------------------------------------------------------%

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
	[will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_TypeInfo         exp_type_info;
    MR_TypeCtorInfo     type_ctor_info;
    MR_NotagFunctorDesc *functor_desc;
    MR_Word             value;

    MR_unravel_univ(Univ, type_info, value);
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
            functor_desc = MR_type_ctor_functors(type_ctor_info).functors_notag;
            exp_type_info = MR_pseudo_type_info_is_ground(
                functor_desc->MR_notag_functor_arg_type);
            MR_new_univ_on_hp(ExpUniv, exp_type_info, value);
            SUCCESS_INDICATOR = MR_TRUE;
            break;

        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            functor_desc = MR_type_ctor_functors(type_ctor_info).functors_notag;
            exp_type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                functor_desc->MR_notag_functor_arg_type);
            MR_new_univ_on_hp(ExpUniv, exp_type_info, value);
            SUCCESS_INDICATOR = MR_TRUE;
            break;

        default:
            SUCCESS_INDICATOR = MR_FALSE;
            break;
    }
}").

:- pragma foreign_proc("MC++", 
	get_notag_functor_info(_Univ::in, _ExpUniv::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	mercury::runtime::Errors::SORRY(""foreign code for get_notag_functor_info"");
").

    % Given a value of an arbitrary type, succeed if its type is defined
    % as an equivalence type, and return a univ which bundles up the value
    % with the equivalent type. (I.e. this removes one layer of equivalence
    % from the type stored in the univ.)
:- pred get_equiv_functor_info(univ::in, univ::out) is semidet.

:- pragma foreign_proc("C",
	get_equiv_functor_info(Univ::in, ExpUniv::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo     type_info;
    MR_TypeInfo     exp_type_info;
    MR_TypeCtorInfo type_ctor_info;
    MR_Word         value;

    MR_unravel_univ(Univ, type_info, value);
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_EQUIV:
            exp_type_info = MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).layout_equiv);
            MR_new_univ_on_hp(ExpUniv, exp_type_info, value);
            SUCCESS_INDICATOR = MR_TRUE;
            break;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            exp_type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).layout_equiv);
            MR_new_univ_on_hp(ExpUniv, exp_type_info, value);
            SUCCESS_INDICATOR = MR_TRUE;
            break;

        default:
            SUCCESS_INDICATOR = MR_FALSE;
            break;
    }
}").

:- pragma foreign_proc("MC++",
	get_equiv_functor_info(_Univ::in, _ExpUniv::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	mercury::runtime::Errors::SORRY(""foreign code for get_equiv_functor_info"");
").

    % Given a value of an arbitrary type, succeed if it is an enum type,
    % and return the integer value corresponding to the value.
:- pred get_enum_functor_info(univ::in, int::out) is semidet.

:- pragma foreign_proc("C",
	get_enum_functor_info(Univ::in, Enum::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo     type_info;
    MR_TypeCtorInfo type_ctor_info;
    MR_Word         value;

    MR_unravel_univ(Univ, type_info, value);
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
            Enum = (MR_Integer) value;
            SUCCESS_INDICATOR = MR_TRUE;
            break;

        default:
            SUCCESS_INDICATOR = MR_FALSE;
            break;
    }
}").

:- pragma foreign_proc("MC++",
	get_enum_functor_info(_Univ::in, _Enum::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
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

:- pragma foreign_proc("C",
	get_du_functor_info(Univ::in, Where::out, Ptag::out, Sectag::out,
		Args::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo             type_info;
    MR_TypeCtorInfo         type_ctor_info;
    const MR_DuPtagLayout   *ptag_layout;
    const MR_DuFunctorDesc  *functor_desc;
    MR_Word                 value;
    MR_Word                 *arg_vector;
    int                     i;

    MR_unravel_univ(Univ, type_info, value);
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
            SUCCESS_INDICATOR = MR_TRUE;
            Ptag = MR_tag(value);
            ptag_layout = &MR_type_ctor_layout(type_ctor_info).layout_du[Ptag];

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
                        SUCCESS_INDICATOR = MR_FALSE;
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
            SUCCESS_INDICATOR = MR_FALSE;
            break;
    }
}").

:- pragma foreign_proc("MC++",
	get_du_functor_info(_Univ::in, _Where::out, _Ptag::out, _Sectag::out,
		_Args::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	mercury::runtime::Errors::SORRY(""foreign code for get_du_functor_info"");
").
