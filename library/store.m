%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1997, 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: store.m. 
% Main author: fjh.
% Stability: low.
%
% This file provides facilities for manipulating mutable stores.
% A store can be consider a mapping from abstract keys to their values.
% A store holds a set of nodes, each of which may contain a value of any
% type.
%
% Stores may be used to implement cyclic data structures such as
% circular linked lists, etc.
%
% Stores can have two different sorts of keys:
% mutable variables (mutvars) and references (refs).
% The difference between mutvars and refs is that
% mutvars can only be updated atomically,
% whereas it is possible to update individual fields of a reference
% one at a time (presuming the reference refers to a structured term).
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module store.
:- interface.
:- import_module io.

% Stores and keys are indexed by a type S of typeclass store(S) that
% is used to distinguish between different stores.  By using an
% existential type declaration for store__new (see below), we use the
% type system to ensure at compile time that you never attempt to use
% a key from one store to access a different store.

:- typeclass store(S).
:- type store(S).

:- instance store(io__state).
:- instance store(store(S)).

	% initialize a new store
:- some [S] pred store__new(store(S)).
:- mode          store__new(uo) is det.

%-----------------------------------------------------------------------------%
%
% mutvars
%

	% generic_mutvar(T, S):
	% a mutable variable holding a value of type T in store S
:- type generic_mutvar(T, S).
:- type io_mutvar(T) == generic_mutvar(T, io__state).
:- type store_mutvar(T, S) == generic_mutvar(T, store(S)).

	% create a new mutable variable,
	% initialized with the specified value
:- pred store__new_mutvar(T, generic_mutvar(T, S), S, S) <= store(S).
:- mode store__new_mutvar(in, out, di, uo) is det.

	% lookup the value stored in a given mutable variable
:- pred store__get_mutvar(generic_mutvar(T, S), T, S, S) <= store(S).
:- mode store__get_mutvar(in, out, di, uo) is det.

	% replace the value stored in a given mutable variable
:- pred store__set_mutvar(generic_mutvar(T, S), T, S, S) <= store(S) .
:- mode store__set_mutvar(in, in, di, uo) is det.

	% new_cyclic_mutvar(Func, Mutvar):
	% create a new mutable variable, whose value is initialized
	% with the value returned from the specified function `Func'.
	% The argument passed to the function is the mutvar itself,
	% whose value has not yet been initialized (this is safe
	% because the function does not get passed the store, so
	% it can't examine the uninitialized value).
	%
	% This predicate is useful for creating self-referential values
	% such as circular linked lists. 
	% For example:
	%	:- type clist(T, S) ---> node(T, mutvar(clist(T, S))).
	%	:- pred init_cl(T::in, clist(T, S)::out,
	%			store(S)::di, store(S)::uo) is det.
	%	init_cl(X, CList) -->
	%	    store__new_cyclic_mutvar(func(CL) = node(X, CL), CList).
	%
:- pred store__new_cyclic_mutvar(func(generic_mutvar(T, S)) = T,
		generic_mutvar(T, S), S, S) <= store(S).
:- mode store__new_cyclic_mutvar(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%
% references
%

	% generic_ref(T, S):
	% a reference to value of type T in store S
:- type generic_ref(T, S).
:- type io_ref(T, S) == generic_ref(T, io__state).
:- type store_ref(T, S) == generic_ref(T, store(S)).

	% new_ref(Val, Ref):	
	%	/* In C: Ref = malloc(...); *Ref = Val; */
	% Given a value of any type `T', insert a copy of the term
	% into the store and return a new reference to that term.
	% (This does not actually perform a copy, it just returns a view
	% of the representation of that value.
	% It does however allocate one cell to hold the reference;
	% you can use new_arg_ref to avoid that.)
:- pred store__new_ref(T, generic_ref(T, S), S, S) <= store(S).
:- mode store__new_ref(di, out, di, uo) is det.

	% ref_functor(Ref, Functor, Arity):
	% Given a reference to a term, return the functor and arity
	% of that term.
:- pred store__ref_functor(generic_ref(T, S), string, int, S, S)
		<= store(S).
:- mode store__ref_functor(in, out, out, di, uo) is det.

	% arg_ref(Ref, ArgNum, ArgRef):	     
	%	/* Pseudo-C code: ArgRef = &Ref[ArgNum]; */
	% Given a reference to a term, return a reference to
	% the specified argument (field) of that term
	% (argument numbers start from zero).
	% It is an error if the argument number is out of range,
	% or if the argument reference has the wrong type.
:- pred store__arg_ref(generic_ref(T, S), int,
		generic_ref(ArgT, S), S, S) <= store(S).
:- mode store__arg_ref(in, in, out, di, uo) is det.

	% new_arg_ref(Val, ArgNum, ArgRef):
	%	/* Pseudo-C code: ArgRef = &Val[ArgNum]; */
	% Equivalent to `new_ref(Val, Ref), arg_ref(Ref, ArgNum, ArgRef)',
	% except that it is more efficient.
	% It is an error if the argument number is out of range,
	% or if the argument reference has the wrong type.
:- pred store__new_arg_ref(T, int, generic_ref(ArgT, S), S, S)
		<= store(S).
:- mode store__new_arg_ref(di, in, out, di, uo) is det.

	% set_ref(Ref, ValueRef):
	%	/* Pseudo-C code: *Ref = *ValueRef; */
	% Given a reference to a term (Ref), 
	% a reference to another term (ValueRef),
	% update the store so that the term referred to by Ref
	% is replaced with the term referenced by ValueRef.
:- pred store__set_ref(generic_ref(T, S), generic_ref(T, S), S, S)
		<= store(S).
:- mode store__set_ref(in, in, di, uo) is det.

	% set_ref_value(Ref, Value):
	%	/* Pseudo-C code: *Ref = Value; */
	% Given a reference to a term (Ref), and a value (Value),
	% update the store so that the term referred to by Ref
	% is replaced with Value.
:- pred store__set_ref_value(generic_ref(T, S), T, S, S) <= store(S).
:- mode store__set_ref_value(in, di, di, uo) is det.

	% Given a reference to a term, return that term.
	% Note that this requires making a copy, so this pred may
	% be inefficient if used to return large terms; it
	% is most efficient with atomic terms.
	% XXX current implementation buggy (does shallow copy)
:- pred store__copy_ref_value(generic_ref(T, S), T, S, S) <= store(S).
:- mode store__copy_ref_value(in, uo, di, uo) is det.

	% Same as above, but without making a copy.
	% Destroys the store.
:- pred store__extract_ref_value(S, generic_ref(T, S), T) <= store(S).
:- mode store__extract_ref_value(di, in, out) is det.

%-----------------------------------------------------------------------------%
%
% Nasty performance hacks
%
% WARNING: use of these procedures is dangerous!
% Use them only only as a last resort, only if performance
% is critical, and only if profiling shows that using the
% safe versions is a bottleneck.
%
% These procedures may vanish in some future version of Mercury.

	% `unsafe_arg_ref' is the same as `arg_ref',
	% and `unsafe_new_arg_ref' is the same as `new_arg_ref'
	% except that they doesn't check for errors,
	% and they don't work for `no_tag' types (types with
	% exactly one functor which has exactly one argument),
	% and they don't work for types with >4 functors.
	% If the argument number is out of range,
	% or if the argument reference has the wrong type,
	% or if the argument is a `no_tag' type,
	% then the behaviour is undefined, and probably harmful.

:- pred store__unsafe_arg_ref(generic_ref(T, S), int, generic_ref(ArgT, S),
		S, S) <= store(S).
:- mode store__unsafe_arg_ref(in, in, out, di, uo) is det.

:- pred store__unsafe_new_arg_ref(T, int, generic_ref(ArgT, S), S, S)
		<= store(S).
:- mode store__unsafe_new_arg_ref(di, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%
% Interfaces retained only for backwards compatibility.
% Some of these are unsafe.  All of them are deprecated.
%

	% OBSOLETE: use `S' or `some [S] ... S' instead.
:- type some_store_type.

	% initialize a store
	% OBSOLETE: use store__new/1 instead
:- pred store__init(store(some_store_type)).
:- mode store__init(uo) is det.
:- pragma obsolete(store__init/1).

	% OBSOLETE: use store_mutvar/2 or generic_mutvar/2 instead.
:- type mutvar(T, S) == store_mutvar(T, S).

	% OBSOLETE: use store_mutvar/2 or generic_mutvar/2 instead.
:- type ref(T, S) == store_ref(T, S).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util.

:- typeclass store(T) where [].
:- instance store(store(S)) where []. 
:- instance store(io__state) where [].

:- type some_store_type ---> some_store_type. 

:- type store(S) ---> store(c_pointer).

:- type generic_mutvar(T, S) ---> mutvar(c_pointer).

:- type generic_ref(T, S) ---> ref(c_pointer).

store__new(S) :-
	store__do_init(S).

store__init(S) :-
	store__do_init(S).

:- pred store__do_init(store(some_store_type)).
:- mode store__do_init(uo) is det.

:- pragma foreign_proc("C", store__do_init(_S0::uo), will_not_call_mercury, "").

/* 
Note -- the syntax for the operations on stores
might be nicer if we used some new operators, e.g.

	:- op(.., xfx, ('<-')).
	:- op(.., fy, ('!')).
	:- op(.., xfx, (':=')).

Then we could do something like this:

	Ptr <- new(Val)	  -->	new_mutvar(Val, Ptr).
	Val <- !Ptr 	  -->	get_mutvar(Ptr, Val).
	!Ptr := Val	  -->	set_mutvar(Ptr, Val).

I wonder whether it is worth it?  Hmm, probably not.
*/

:- pragma foreign_proc("C", new_mutvar(Val::in, Mutvar::out, S0::di, S::uo),
		will_not_call_mercury,
"
	MR_incr_hp_msg(Mutvar, 1, MR_PROC_LABEL, ""store:mutvar/2"");
	* (MR_Word *) Mutvar = Val;
	S = S0;
").

:- pragma foreign_proc("C", get_mutvar(Mutvar::in, Val::out, S0::di, S::uo),
		will_not_call_mercury,
"
	Val = * (MR_Word *) Mutvar;
	S = S0;
").

:- pragma foreign_proc("C", set_mutvar(Mutvar::in, Val::in, S0::di, S::uo),
		will_not_call_mercury,
"
	* (MR_Word *) Mutvar = Val;
	S = S0;
").

:- pred store__unsafe_new_uninitialized_mutvar(generic_mutvar(T, S),
						S, S) <= store(S).
:- mode store__unsafe_new_uninitialized_mutvar(out, di, uo) is det.

:- pragma foreign_proc("C", unsafe_new_uninitialized_mutvar(Mutvar::out, S0::di, S::uo),
		will_not_call_mercury,
"
	MR_incr_hp_msg(Mutvar, 1, MR_PROC_LABEL, ""store:mutvar/2"");
	S = S0;
").

store__new_cyclic_mutvar(Func, MutVar) -->
	store__unsafe_new_uninitialized_mutvar(MutVar),
	{ Value = apply(Func, MutVar) },
	store__set_mutvar(MutVar, Value).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", new_ref(Val::di, Ref::out, S0::di, S::uo),
		will_not_call_mercury,
"
	MR_incr_hp_msg(Ref, 1, MR_PROC_LABEL, ""store:ref/2"");
	* (MR_Word *) Ref = Val;
	S = S0;
").

copy_ref_value(Ref, Val) -->
	/* XXX need to deep-copy non-atomic types */
	unsafe_ref_value(Ref, Val).

	% unsafe_ref_value extracts the value that a reference
	% refers to, without making a copy; it is unsafe because
	% the store could later be modified, changing the returned
	% value.
:- pred store__unsafe_ref_value(generic_ref(T, S), T, S, S) <= store(S).
:- mode store__unsafe_ref_value(in, uo, di, uo) is det.
:- pragma foreign_proc("C", unsafe_ref_value(Ref::in, Val::uo, S0::di, S::uo),
		will_not_call_mercury,
"
	Val = * (MR_Word *) Ref;
	S = S0;
").

ref_functor(Ref, Functor, Arity) -->
	unsafe_ref_value(Ref, Val),
	{ functor(Val, Functor, Arity) }.

:- pragma c_header_code("
	#include ""mercury_type_info.h""
	#include ""mercury_heap.h""
	#include ""mercury_misc.h""	/* for MR_fatal_error() */

	/* ML_arg() is defined in std_util.m */
	bool ML_arg(MR_TypeInfo term_type_info, MR_Word *term, int arg_index,
			MR_TypeInfo *arg_type_info_ptr, MR_Word **arg_ptr);

").

:- pragma foreign_proc("C", 
	arg_ref(Ref::in, ArgNum::in, ArgRef::out, S0::di, S::uo),
		will_not_call_mercury,
"{
	MR_TypeInfo	type_info;
	MR_TypeInfo	arg_type_info;
	MR_TypeInfo	exp_arg_type_info;
	MR_Word		*arg_ref;

	type_info = (MR_TypeInfo) TypeInfo_for_T;
	exp_arg_type_info = (MR_TypeInfo) TypeInfo_for_ArgT;

	MR_save_transient_registers();

	if (!ML_arg(type_info, (MR_Word *) Ref, ArgNum,
			&arg_type_info, &arg_ref))
	{
		MR_fatal_error(
			""store__arg_ref: argument number out of range"");
	}

	if (MR_compare_type_info(arg_type_info, exp_arg_type_info) !=
		MR_COMPARE_EQUAL)
	{
		MR_fatal_error(""store__arg_ref: argument has wrong type"");
	}

	MR_restore_transient_registers();

	ArgRef = (MR_Word) arg_ref;
	S = S0;
}").

:- pragma foreign_proc("C", 
	new_arg_ref(Val::di, ArgNum::in, ArgRef::out, S0::di, S::uo),
		will_not_call_mercury,
"{
	MR_TypeInfo	type_info;
	MR_TypeInfo	arg_type_info;
	MR_TypeInfo	exp_arg_type_info;
	MR_Word		*arg_ref;

	type_info = (MR_TypeInfo) TypeInfo_for_T;
	exp_arg_type_info = (MR_TypeInfo) TypeInfo_for_ArgT;

	MR_save_transient_registers();

	if (!ML_arg(type_info, (MR_Word *) &Val, ArgNum,
			&arg_type_info, &arg_ref))
	{
		MR_fatal_error(
			""store__new_arg_ref: argument number out of range"");
	}

	if (MR_compare_type_info(arg_type_info, exp_arg_type_info) !=
		MR_COMPARE_EQUAL)
	{
		MR_fatal_error(
			""store__new_arg_ref: argument has wrong type"");
	}

	MR_restore_transient_registers();

	/*
	** For no_tag types, the argument may have the same address as the
	** term.  Since the term (Val) is currently on the C stack, we can't
	** return a pointer to it; so if that is the case, then we need
	** to copy it to the heap before returning.
	*/

	if (arg_ref == &Val) {
		MR_incr_hp_msg(ArgRef, 1, MR_PROC_LABEL, ""store:ref/2"");
		* (MR_Word *) ArgRef = Val;
	} else {
		ArgRef = (MR_Word) arg_ref;
	}
	S = S0;
}").

:- pragma foreign_proc("C", 
	set_ref(Ref::in, ValRef::in, S0::di, S::uo),
		will_not_call_mercury,
"
	* (MR_Word *) Ref = * (MR_Word *) ValRef;
	S = S0;
").

:- pragma foreign_proc("C",	
	set_ref_value(Ref::in, Val::di, S0::di, S::uo),
		will_not_call_mercury,
"
	* (MR_Word *) Ref = Val;
	S = S0;
").

:- pragma foreign_proc("C",
	extract_ref_value(_S::di, Ref::in, Val::out),
		will_not_call_mercury,
"
	Val = * (MR_Word *) Ref;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
	unsafe_arg_ref(Ref::in, Arg::in, ArgRef::out, S0::di, S::uo),
		will_not_call_mercury,
"{
	/* unsafe - does not check type & arity, won't handle no_tag types */
	MR_Word *Ptr = (MR_Word *) MR_strip_tag((MR_Word) Ref);
	ArgRef = (MR_Word) &Ptr[Arg];
	S = S0;
}").

:- pragma foreign_proc("C", unsafe_new_arg_ref(Val::di, Arg::in, ArgRef::out,
				S0::di, S::uo), will_not_call_mercury,
"{
	/* unsafe - does not check type & arity, won't handle no_tag types */
	MR_Word *Ptr = (MR_Word *) MR_strip_tag((MR_Word) Val);
	ArgRef = (MR_Word) &Ptr[Arg];
	S = S0;
}").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("MC++", store__do_init(_S0::uo),
	will_not_call_mercury, "").

:- pragma foreign_proc("MC++", new_mutvar(_Val::in, _Mutvar::out,
		_S0::di, _S::uo), will_not_call_mercury,
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++", get_mutvar(_Mutvar::in, _Val::out,
		_S0::di, _S::uo), will_not_call_mercury,
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++", set_mutvar(_Mutvar::in, _Val::in,
		_S0::di, _S::uo), will_not_call_mercury,
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++", unsafe_new_uninitialized_mutvar(
		_Mutvar::out, _S0::di, _S::uo), will_not_call_mercury,
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++", new_ref(_Val::di, _Ref::out, _S0::di, _S::uo),
		will_not_call_mercury,
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++", unsafe_ref_value(_Ref::in, _Val::uo,
		_S0::di, _S::uo), will_not_call_mercury,
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++", 
	arg_ref(_Ref::in, _ArgNum::in, _ArgRef::out, _S0::di, _S::uo),
		will_not_call_mercury,
"{
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
}").

:- pragma foreign_proc("MC++", 
	new_arg_ref(_Val::di, _ArgNum::in, _ArgRef::out, _S0::di, _S::uo),
		will_not_call_mercury,
"{
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
}").

:- pragma foreign_proc("MC++", 
	set_ref(_Ref::in, _ValRef::in, _S0::di, _S::uo),
		will_not_call_mercury,
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++",	
	set_ref_value(_Ref::in, _Val::di, _S0::di, _S::uo),
		will_not_call_mercury,
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++",
	extract_ref_value(_S::di, _Ref::in, _Val::out),
		will_not_call_mercury,
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++",
	unsafe_arg_ref(_Ref::in, _Arg::in, _ArgRef::out, _S0::di, _S::uo),
		will_not_call_mercury,
"{
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
}").

:- pragma foreign_proc("MC++",
	unsafe_new_arg_ref(_Val::di, _Arg::in, _ArgRef::out,
			_S0::di, _S::uo), will_not_call_mercury,
"{
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
}").



