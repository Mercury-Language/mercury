%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: store.nu.nl. 
% Main author: fjh.
%
% This file provides a Prolog implementation of store.m.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

store__init(store).

%-----------------------------------------------------------------------------%

% mutvars are implemented in terms of SICStus 3 mutable variable primitives

store__new_mutvar(Val, MutVar) -->
	{ create_mutable(Val, MutVar) }. 

store__get_mutvar(MutVar, Val) -->
	{ get_mutable(MutVar, Val) }. 

store__set_mutvar(MutVar, Val) -->
	{ update_mutable(MutVar, Val) }. 

%-----------------------------------------------------------------------------%

% For NU-Prolog and SICStus 2, we implement the SICStus 3 mutable variable
% primitives using setarg/3.  SICStus 3 will ignore these definitions.

create_mutable(Val, mutable(Val)).

get_mutable(mutable(Val), Val).

update_mutable(Ref, NewVal) :-
	setarg(Ref, 1, NewVal).

%-----------------------------------------------------------------------------%

% refs are implemented as (Term - Argnum) pairs, using setarg/3.
% This won't work in SICStus 3.  As far as I can see,
% there is no way to implement this stuff in SICStus 3
% without significant overhead.

store__new_ref(Val, ref(Val) - 1) --> [].

store__unsafe_ref_value(Ref, Val) -->
	{ Ref = Term - Arg },
	{ arg(Term, Arg, Val) }.

store__copy_ref_value(Ref, Val) -->
	store__unsafe_ref_value(Ref, Val0),
	copy_term(Val0, Val).

store__ref_functor(Ref, Functor, Arity) -->
	store__unsafe_ref_value(Ref, Val),
	{ functor(Val, Functor, Arity) }.

store__arg_ref(Ref, ArgNum, ArgRef) -->
	store__unsafe_ref_value(Ref, Val),
	{ ArgRef = Val - ArgNum }.

store__new_arg_ref(Val, ArgNum, ArgRef) -->
	{ ArgRef = Val - ArgNum }.

store__set_ref(Ref, ValRef) -->
	store__unsafe_ref_value(ValRef, Val),
	store__set_ref_value(Ref, Val).

store__set_ref_value(Ref, Val) -->
	{ Ref = Term - ArgNum },
	{ setarg(Term, ArgNum, Val) }.

store__extract_ref_value(Ref, Val) -->
	store__unsafe_ref_value(Ref, Val).

%-----------------------------------------------------------------------------%

store__unsafe_arg_ref(Ref, Arg, ArgRef) -->
	store__arg_ref(Ref, Arg, ArgRef).

store__unsafe_new_arg_ref(Ref, Arg, ArgRef) -->
	store__new_arg_ref(Ref, Arg, ArgRef).

%-----------------------------------------------------------------------------%
