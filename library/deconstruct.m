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

:- pragma foreign_proc("C",
	functor(Term::in, Functor::out, Arity::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME			""functor/3""
#define	TYPEINFO_ARG			TypeInfo_for_T
#define	TERM_ARG			Term
#define	FUNCTOR_ARG			Functor
#define	ARITY_ARG			Arity
#include ""mercury_ml_functor_body.h""
#undef	PREDNAME
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
}").

:- pragma foreign_proc("C",
	functor_cc(Term::in, Functor::out, Arity::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME			""functor_cc/3""
#define	ALLOW_NONCANONICAL
#define	TYPEINFO_ARG			TypeInfo_for_T
#define	TERM_ARG			Term
#define	FUNCTOR_ARG			Functor
#define	ARITY_ARG			Arity
#include ""mercury_ml_functor_body.h""
#undef	PREDNAME
#undef	ALLOW_NONCANONICAL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
}").

functor_cc(_Term::in, _Functor::out, _Arity::out) :-
	error("NYI: std_util__functor_cc/3").

/*
** N.B. any modifications to arg/2 might also require similar
** changes to store__arg_ref in store.m.
*/

:- pragma foreign_proc("C",
	arg(Term::in, ArgumentIndex::in) = (Argument::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME 		""arg/2""
#define	NONCANON_HANDLING	MR_ABORT_ON_NONCANONICAL
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		ArgumentIndex
#define	SELECTED_ARG		Argument
#define	EXPECTED_TYPE_INFO	TypeInfo_for_ArgT
#include ""mercury_ml_arg_body.h""
#undef	PREDNAME
#undef	NONCANON_HANDLING
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	EXPECTED_TYPE_INFO
}").

:- pragma foreign_proc("C",
	arg_cc(Term::in, ArgumentIndex::in, Argument::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME 		""arg/2""
#define	NONCANON_HANDLING	MR_ALLOW_NONCANONICAL
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		ArgumentIndex
#define	SELECTED_ARG		Argument
#define	EXPECTED_TYPE_INFO	TypeInfo_for_ArgT
#include ""mercury_ml_arg_body.h""
#undef	PREDNAME
#undef	NONCANON_HANDLING
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	EXPECTED_TYPE_INFO
}").

:- pragma foreign_proc("C",
	argument(Term::in, ArgumentIndex::in) = (ArgumentUniv::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME 		""argument/2""
#define	NONCANON_HANDLING	MR_FAIL_ON_NONCANONICAL
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		ArgumentIndex
#define	SELECTED_ARG		ArgumentUniv
#include ""mercury_ml_arg_body.h""
#undef	PREDNAME
#undef	NONCANON_HANDLING
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
}").

:- pragma foreign_proc("C",
	argument_cc(Term::in, ArgumentIndex::in, ArgumentUniv::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME 		""argument_cc/3""
#define	NONCANON_HANDLING	MR_ALLOW_NONCANONICAL
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		ArgumentIndex
#define	SELECTED_ARG		ArgumentUniv
#include ""mercury_ml_arg_body.h""
#undef	PREDNAME
#undef	NONCANON_HANDLING
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
}").

:- pragma foreign_proc("C",
	named_argument(Term::in, ArgumentName::in) = (ArgumentUniv::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME 		""named_argument/2""
#define	NONCANON_HANDLING	MR_FAIL_ON_NONCANONICAL
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		(MR_ConstString) ArgumentName
#define	SELECTED_ARG		ArgumentUniv
#define	SELECT_BY_NAME
#include ""mercury_ml_arg_body.h""
#undef	PREDNAME
#undef	NONCANON_HANDLING
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	SELECT_BY_NAME
}").

:- pragma foreign_proc("C",
	named_argument_cc(Term::in, ArgumentName::in, ArgumentUniv::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME 		""named_argument_cc/3""
#define	NONCANON_HANDLING	MR_ALLOW_NONCANONICAL
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	SELECTOR_ARG		(MR_ConstString) ArgumentName
#define	SELECTED_ARG		ArgumentUniv
#define	SELECT_BY_NAME
#include ""mercury_ml_arg_body.h""
#undef	PREDNAME
#undef	NONCANON_HANDLING
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	SELECTOR_ARG
#undef	SELECTED_ARG
#undef	SELECT_BY_NAME
}").

:- pragma foreign_proc("C", 
	deconstruct(Term::in, Functor::out, Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME		""deconstruct/4""
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#include ""mercury_ml_deconstruct_body.h""
#undef	PREDNAME
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
}").

:- pragma foreign_proc("C", 
	deconstruct_cc(Term::in, Functor::out, Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME		""deconstruct_cc/4""
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args
#define	ALLOW_NONCANONICAL
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#include ""mercury_ml_deconstruct_body.h""
#undef	PREDNAME
#undef	NONCANON_HANDLING
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	ALLOW_NONCANONICAL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
}").

deconstruct_cc(_Term::in, _Functor::out, _Arity::out, _Arguments::out) :-
	error("NYI: std_util__deconstruct_cc/3").

:- pragma foreign_proc("C", 
	limited_deconstruct(Term::in, MaxArity::in, Functor::out,
		Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME		""limited_deconstruct/5""
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Limit_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args_limit
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	MAX_ARITY_ARG		MaxArity
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#include ""mercury_ml_deconstruct_body.h""
#undef	PREDNAME
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	MAX_ARITY_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
}").

:- pragma foreign_proc("C", 
	limited_deconstruct_cc(Term::in, MaxArity::in, Functor::out,
		Arity::out, Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
#define	PREDNAME		""limited_deconstruct_cc/5""
#define	EXPAND_INFO_TYPE	MR_Expand_Functor_Args_Limit_Info
#define	EXPAND_INFO_CALL	MR_expand_functor_args_limit
#define	ALLOW_NONCANONICAL
#define	TYPEINFO_ARG		TypeInfo_for_T
#define	TERM_ARG		Term
#define	MAX_ARITY_ARG		MaxArity
#define	FUNCTOR_ARG		Functor
#define	ARITY_ARG		Arity
#define	ARGUMENTS_ARG		Arguments
#include ""mercury_ml_deconstruct_body.h""
#undef	PREDNAME
#undef	EXPAND_INFO_TYPE
#undef	EXPAND_INFO_CALL
#undef	ALLOW_NONCANONICAL
#undef	TYPEINFO_ARG
#undef	TERM_ARG
#undef	MAX_ARITY_ARG
#undef	FUNCTOR_ARG
#undef	ARITY_ARG
#undef	ARGUMENTS_ARG
}").

limited_deconstruct_cc(_Term::in, _MaxArity::in, _Functor::out, _Arity::out,
		_Arguments::out) :-
	error("NYI: std_util__limited_deconstruct_cc/3").

:- pragma foreign_proc("MC++",
	functor(_Term::in, _Functor::out, _Arity::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	mercury::runtime::Errors::SORRY(""foreign code for functor"");
").

:- pragma foreign_proc("C#", 
	arg(_Term::in, _ArgumentIndex::in) = (_Argument::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
	mercury.runtime.Errors.SORRY(""foreign code for arg"");
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}").

:- pragma foreign_proc("C#", 
	arg_cc(_Term::in, _ArgumentIndex::in, _Argument::out),
	[will_not_call_mercury, thread_safe],
"{
	mercury.runtime.Errors.SORRY(""foreign code for arg_cc"");
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}").

:- pragma foreign_proc("C#",
	argument(_Term::in, _ArgumentIndex::in) = (_ArgumentUniv::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
	mercury.runtime.Errors.SORRY(""foreign code for argument"");
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}").

:- pragma foreign_proc("C#",
	argument_cc(_Term::in, _ArgumentIndex::in, _ArgumentUniv::out),
	[will_not_call_mercury, thread_safe],
"{
	mercury.runtime.Errors.SORRY(""foreign code for argument_cc"");
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}").

:- pragma foreign_proc("C#",
	named_argument(_Term::in, _ArgumentName::in) = (_ArgumentUniv::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
	mercury.runtime.Errors.SORRY(""foreign code for named_argument"");
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}").

:- pragma foreign_proc("C#",
	named_argument_cc(_Term::in, _ArgumentName::in, _ArgumentUniv::out),
	[will_not_call_mercury, thread_safe],
"{
	mercury.runtime.Errors.SORRY(""foreign code for named_argument_cc"");
	// XXX this is required to keep the C# compiler quiet
	SUCCESS_INDICATOR = false;
}").

det_arg(Type, ArgumentIndex) = Argument :-
	( deconstruct__arg(Type, ArgumentIndex) = Argument0 ->
		Argument = Argument0
	;
		( deconstruct__argument(Type, ArgumentIndex) = _ArgumentUniv ->
			error("det_arg: argument had wrong type")
		;
			error("det_arg: argument number out of range")
		)
	).

det_arg_cc(Type, ArgumentIndex, Argument) :-
	( deconstruct__arg_cc(Type, ArgumentIndex, Argument0) ->
		Argument = Argument0
	;
		( deconstruct__argument_cc(Type, ArgumentIndex, _ArgumentUniv) ->
			error("det_arg_cc: argument had wrong type")
		;
			error("det_arg_cc: argument number out of range")
		)
	).

det_argument(Type, ArgumentIndex) = Argument :-
	( deconstruct__argument(Type, ArgumentIndex) = Argument0 ->
		Argument = Argument0
	;
		error("det_argument: argument out of range")
	).

det_argument_cc(Type, ArgumentIndex, Argument) :-
	( deconstruct__argument_cc(Type, ArgumentIndex, Argument0) ->
		Argument = Argument0
	;
		error("det_argument_cc: argument out of range")
	).

det_named_argument(Type, ArgumentName) = Argument :-
	( deconstruct__named_argument(Type, ArgumentName) = Argument0 ->
		Argument = Argument0
	;
		error("det_named_argument: no argument with that name")
	).

det_named_argument_cc(Type, ArgumentName, Argument) :-
	( deconstruct__named_argument_cc(Type, ArgumentName, Argument0) ->
		Argument = Argument0
	;
		error("det_named_argument_cc: no argument with that name")
	).

deconstruct(Term::in, Functor::out, Arity::out, Arguments::out) :-
	rtti_implementation__deconstruct(Term, Functor, Arity, Arguments).

:- pragma foreign_proc("MC++", 
	limited_deconstruct(_Term::in, _MaxArity::in, _Functor::out,
		_Arity::out, _Arguments::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
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
            SUCCESS_INDICATOR = TRUE;
            break;

        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            functor_desc = MR_type_ctor_functors(type_ctor_info).functors_notag;
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
            SUCCESS_INDICATOR = TRUE;
            break;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            exp_type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).layout_equiv);
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
            SUCCESS_INDICATOR = TRUE;
            break;

        default:
            SUCCESS_INDICATOR = FALSE;
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
            SUCCESS_INDICATOR = TRUE;
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

:- pragma foreign_proc("MC++",
	get_du_functor_info(_Univ::in, _Where::out, _Ptag::out, _Sectag::out,
		_Args::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	mercury::runtime::Errors::SORRY(""foreign code for get_du_functor_info"");
").
