%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%---------------------------------------------------------------------------%
%
% Main author:	petdr, fjh
% Stability:	low.
%
% This module provides a Mercury version of Haskell mutable variables.
% A mutable variable (mutvar) is a refencence to a mutable location which
% can either contain a value of type T or be empty.
%
% Access to a mutvar is thread-safe and can be used to synchronize
% between different threads.
%
%---------------------------------------------------------------------------%

:- module mutvar.

:- interface.

:- import_module io.

:- type mutvar(T).

	% Create an empty mutvar.
:- pred mutvar__init(mutvar(T)::out, io__state::di, io__state::uo) is det.

	% Take the contents of the mutvar out leaving the mutvar empty.
	% If the mutvar is empty, block until some thread fills the
	% mutvar.
:- pred mutvar__take(mutvar(T)::in, T::out,
		io__state::di, io__state::uo) is det.

	% Place the value of type T into an empty mutvar.  If the
	% mutvar is full block until it becomes empty.
:- pred mutvar__put(mutvar(T)::in, T::in,
		io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module semaphore.

:- type mutvar(T)
	--->	mutvar(
			semaphore,	% full
			semaphore,	% empty
			ref(T)		% data
		).

:- pragma promise_pure(mutvar__init/3).
mutvar__init(mutvar(Full, Empty, Ref)) -->
	semaphore__new(Full),
	semaphore__new(Empty),

	{ impure new_ref(Ref) },
		
		% Initially a mutvar starts empty.
	semaphore__signal(Empty).

:- pragma promise_pure(mutvar__take/4).
mutvar__take(mutvar(Full, Empty, Ref), Data) -->
	semaphore__wait(Full),
	{ impure get_ref(Ref, Data) },
	semaphore__signal(Empty).

:- pragma promise_pure(mutvar__put/4).
mutvar__put(mutvar(Full, Empty, Ref), Data) -->
	semaphore__wait(Empty),
	{ impure set_ref(Ref, Data) },
	semaphore__signal(Full).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

%  A non-backtrackably destructively modifiable reference type

%%% :- interface.

:- type ref(T).

%  Create an empty ref location.
:- impure pred new_ref(ref(T)).
:-        mode new_ref(out) is det.

%  Get the value currently referred to by a reference.
:- impure pred get_ref(ref(T), T) is det.
:-        mode get_ref(in, uo) is det.	% XXX this is a work-around

%  destructively modify a reference to refer to a new object.
:- impure pred set_ref(ref(T), T) is det.
:-        mode set_ref(in, in) is det.

%%% :- implementation.

%  This type is implemented in C.
:- type ref(T) ---> ref(c_pointer).

:- pragma inline(new_ref/1).
:- pragma c_code(new_ref(Ref::out),
		[will_not_call_mercury, thread_safe],
"
	incr_hp_msg(Ref, 1, MR_PROC_LABEL, ""mutvar:ref/1"");
	*(MR_Word *) Ref = NULL;
").

:- pragma inline(get_ref/2).
:- pragma c_code(get_ref(Ref::in, X::uo),
		[will_not_call_mercury, thread_safe],
"
	X = *(MR_Word *) Ref;
	*(MR_Word *) Ref = NULL;
").

:- pragma inline(set_ref/2).
:- pragma c_code(set_ref(Ref::in, X::in),
		[will_not_call_mercury, thread_safe],
"
	*(MR_Word *) Ref = (MR_Word) X;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
