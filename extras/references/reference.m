%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File      : reference.m
% Authors   : pets (Peter Schachte)
% Stability : low
% Purpose   : A backtrackably modifiable storage type
%
%  This module defines a type which is, in essence, a ``box'' into which you
%  can put any term, and later destructively replace the contents with
%  something else.  The store module in the standard library provides a clean
%  way to do exactly the same thing; the difference is that the reference
%  module allows you to do it without threading a store through your code.  The
%  price for this convenience is that you must mark all predicates that create
%  or modify a reference, and all the predicates that call them, and so on, as
%  impure.  This is probably more inconvenient than just threading the store
%  through your code, so this module should probably only be used in
%  exceptional circumstances.
%
%  This module is implemented using the trailing features described in the
%  "Trailing" section of the "C interface" chapter of the Mercury Language
%  Reference Manual.  This means that in order to use this module, you *must*
%  compile with the --use-trail switch.  The easiest way to do this is to
%  include the line
%
%	GRADEFLAGS=--use-trail
%
%  in your Mmakefile.

:- module reference.
:- interface.

%  A backtrackably destructively modifiable reference type.
:- type reference(T).

%  Create a new reference given a term for it to (initially) refer to.
:- impure pred new_reference(T::in, reference(T)::out) is det.

%  Get the value currently referred to by a reference.
:- semipure pred value(reference(T)::in, T::out) is det.

%  (backtrackably) modify a reference to refer to a new object.
:- impure pred update(reference(T)::in, T::in) is det.


:- implementation.

%  This type is implemented in C.
:- type reference(T) ---> reference(c_pointer).

:- pragma c_header_code("#include ""c_reference.h""").

:- pragma inline(new_reference/2).
:- pragma c_code(new_reference(X::in, Ref::out), will_not_call_mercury, "
	MR_incr_hp(Ref, (sizeof(ME_Reference) + sizeof(MR_Word) - 1) / 
			sizeof(MR_Word));
	((ME_Reference *) Ref)->value = (void *) X;
	((ME_Reference *) Ref)->id = MR_current_choicepoint_id();
").

:- pragma inline(value/2).
:- pragma c_code(value(Ref::in, X::out), will_not_call_mercury, "
	X = (MR_Word) ((ME_Reference *) Ref)->value;
").

:- pragma inline(update/2).
:- pragma c_code(update(Ref::in, X::in), will_not_call_mercury, "
	ME_Reference *ref = (ME_Reference *) Ref;
	if (ref->id != MR_current_choicepoint_id()) {
		MR_trail_current_value((MR_Word *) (&ref->value));
		MR_trail_current_value((MR_Word *) (&ref->id));
		ref->id = MR_current_choicepoint_id();
	}
	ref->value = (void *) X;
").

:- interface.

% init(Ref, Value)
%	Initialise a reference Ref to have value Value.
%	This is for use with user-declared ME_References (see
%	c_reference.h), and must be called before using such a reference.
%	Attempting to access the reference before it is initialised or
%	after the init call is backtracked is undefined.

:- impure pred init(reference(T)::in, T::in) is det.

:- implementation.

:- pragma inline(init/2).
:- pragma c_code(init(Ref::in, X::in), will_not_call_mercury, "
	((ME_Reference *) Ref)->value = (void *) X;
	((ME_Reference *) Ref)->id = MR_current_choicepoint_id();
").

