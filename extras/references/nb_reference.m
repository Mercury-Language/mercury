%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002-2003, 2006, 2011 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File      : nb_reference.m
% Authors   : pets (Peter Schachte)
% Stability : low
% Purpose   : A non-backtrackably modifiable storage type
%
%  This module provides impure code for manipulating a non-backtrackable
%  reference type.  The basic idea is that you allocate a reference and pass
%  it through your code, and each time you dereference it you get the value
%  most recently assigned to it, even if the assignment has been backtracked
%  over.  This gives you a way to communicate information between disjunctive
%  alternatives in your code.
%
%  Because assignments to nb_references need to survive backtracking, every
%  term assigned to a nb_reference must be fully copied in some compiler
%  grades.  This means that nb_references may be somewhat expensive to use for
%  large terms.  However, dereferencing an nb_reference does not need to copy
%  the term, so it is very efficient.  Furthermore, if (part of) a new value
%  being assigned to a nb_reference was itself (part of) an nb_reference, that
%  part will not need to be copied.  For example, if you use nb_references to
%  build up a long list of terms one at a time, each time you add a new term
%  to the list it will need to be copied, but the old value will have already
%  been copied so it will not need to be copied again.
%
%  One further issue arises due to the copying of terms.  Because copied terms
%  are not reclaimed on failure, the only way they can be reclaimed is through
%  garbage collection.  If you use nb_references in a grade without garbage
%  collection, they will never be reclaimed.

:- module nb_reference.
:- interface.

%  A non-backtrackably destructively modifiable reference type
:- type nb_reference(T).

%  Create a new nb_reference given a term for it to reference.
:- impure pred new_nb_reference(T::in, nb_reference(T)::out) is det.

%  Get the value currently referred to by a nb_reference.
:- semipure pred value(nb_reference(T)::in, T::out) is det.

%  (non-backtrackably) modify a nb_reference to refer to a new object.
:- impure pred update(nb_reference(T)::in, T::in) is det.


:- implementation.

%  This type is implemented in C.
%  Note that if the C type used to implement nb_references changes (from
%  something equivalent to `MR_Word'), then `c_reference.h' should also be
%  updated.
:- type nb_reference(T) ---> nb_reference(private_builtin.ref(T)).

:- pragma foreign_decl("C", "#include ""mercury_deep_copy.h""").

:- pragma inline(new_nb_reference/2).

new_nb_reference(X, nb_reference(Ref)) :-
	impure new_nb_reference_2(X, Ref).

:- impure pred new_nb_reference_2(T::in, private_builtin.ref(T)::out) is det.
:- pragma inline(new_nb_reference_2/2).
:- pragma foreign_proc("C",
	new_nb_reference_2(X::in, Ref::out),
	[will_not_call_mercury],
"
	MR_incr_hp(Ref, 1);
#ifndef MR_CONSERVATIVE_GC
	MR_save_transient_registers();
#endif
	*(MR_Word *) Ref = MR_make_long_lived(X, (MR_TypeInfo) TypeInfo_for_T,
			(MR_Word *) Ref);
#ifndef MR_CONSERVATIVE_GC
	MR_restore_transient_registers();
#endif
").

:- pragma inline(value/2).

value(nb_reference(Ref), X) :-
	semipure value_2(Ref, X).

:- semipure pred value_2(private_builtin.ref(T)::in, T::out) is det.
:- pragma inline(value_2/2).
:- pragma foreign_proc("C",
	value_2(Ref::in, X::out),
	[promise_semipure, will_not_call_mercury],
"
	X = *(MR_Word *) Ref;
").

:- pragma inline(update/2).

update(nb_reference(Ref), X) :-
	impure update_2(Ref, X).

:- impure pred update_2(private_builtin.ref(T)::in, T::in) is det.
:- pragma inline(update_2/2).
:- pragma foreign_proc("C",
	update_2(Ref::in, X::in),
	[will_not_call_mercury],
"
#ifndef MR_CONSERVATIVE_GC
	MR_save_transient_registers();
#endif
	*(MR_Word *) Ref = MR_make_long_lived(X, (MR_TypeInfo) TypeInfo_for_T,
			(MR_Word *) Ref);
#ifndef MR_CONSERVATIVE_GC
	MR_restore_transient_registers();
#endif
").

:- interface.

% init(Ref, Value)
%	Initialise a reference Ref to have value Value.
%	This is for use with user-declared ME_NbReferences (see
%	c_reference.h), and must be called before using such a reference.
%	Attempting to access the reference before it is initialised is
%	undefined.

:- impure pred init(nb_reference(T)::in, T::in) is det.

% from_c_pointer(CPointer) = Ref
%	Convert a c_pointer to a nb_reference.

:- func nb_reference.from_c_pointer(c_pointer) = nb_reference(T).

% to_c_pointer(Ref) = CPointer
%	Convert a nb_reference to a c_pointer.

:- func nb_reference.to_c_pointer(nb_reference(T)) = c_pointer.

:- implementation.

:- pragma inline(init/2).
init(Ref, X) :-
	impure update(Ref, X).

:- pragma inline(nb_reference.from_c_pointer/1).
nb_reference.from_c_pointer(CPointer) = nb_reference(Ref) :-
	private_builtin.unsafe_type_cast(CPointer, Ref).

:- pragma inline(nb_reference.to_c_pointer/1).
nb_reference.to_c_pointer(nb_reference(Ref)) = CPointer :-
	private_builtin.unsafe_type_cast(Ref, CPointer).

