%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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
:- type nb_reference(T) ---> nb_reference(c_pointer).

:- pragma c_header_code("#include ""mercury_deep_copy.h""").

:- pragma inline(new_nb_reference/2).
:- pragma c_code(new_nb_reference(X::in, Ref::out), will_not_call_mercury, "
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
:- pragma c_code(value(Ref::in, X::out), will_not_call_mercury, "
	X = *(MR_Word *) Ref;
").

:- pragma inline(update/2).
:- pragma c_code(update(Ref::in, X::in), will_not_call_mercury, "
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

:- implementation.

:- pragma inline(init/2).
init(Ref, X) :-
	impure update(Ref, X).

