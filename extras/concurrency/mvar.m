%---------------------------------------------------------------------------%
% Copyright (C) 2000-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%---------------------------------------------------------------------------%
%
% Main author:	petdr, fjh
% Stability:	low.
%
% This module provides a Mercury version of Haskell mutable variables.
% A mutable variable (mvar) is a refencence to a mutable location which
% can either contain a value of type T or be empty.
%
% Access to a mvar is thread-safe and can be used to synchronize
% between different threads.
%
%---------------------------------------------------------------------------%

:- module mvar.

:- interface.

:- import_module io.

:- type mvar(T).

	% Create an empty mvar.
:- pred mvar__init(mvar(T)::out, io__state::di, io__state::uo) is det.

	% Take the contents of the mvar out leaving the mvar empty.
	% If the mvar is empty, block until some thread fills the
	% mvar.
:- pred mvar__take(mvar(T)::in, T::out,
		io__state::di, io__state::uo) is det.

	% Place the value of type T into an empty mvar.  If the
	% mvar is full block until it becomes empty.
:- pred mvar__put(mvar(T)::in, T::in,
		io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module semaphore.

:- type mvar(T)
	--->	mvar(
			semaphore,	% full
			semaphore,	% empty
			ref(T)		% data
		).

:- pragma promise_pure(mvar__init/3).
mvar__init(mvar(Full, Empty, Ref)) -->
	semaphore__new(Full),
	semaphore__new(Empty),

	{ impure new_ref(Ref) },
		
		% Initially a mvar starts empty.
	semaphore__signal(Empty).

:- pragma promise_pure(mvar__take/4).
mvar__take(mvar(Full, Empty, Ref), Data) -->
	semaphore__wait(Full),
	{ impure get_ref(Ref, Data) },
	semaphore__signal(Empty).

:- pragma promise_pure(mvar__put/4).
mvar__put(mvar(Full, Empty, Ref), Data) -->
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
:- type ref(T).
:- pragma foreign_type(c,  ref(T), "MR_Word").
:- pragma foreign_type(il, ref(T), "class [mvar__csharp_code]ME_Reference").

:- pragma foreign_decl("C#", "
	public class ME_Reference {
		public object	val;
	}
").
:- pragma inline(new_ref/1).
:- pragma c_code(new_ref(Ref::out),
		[will_not_call_mercury, thread_safe],
"
	MR_incr_hp_msg(Ref, 1, MR_PROC_LABEL, ""mvar:ref/1"");
	*(MR_Word *) Ref = (MR_Word) NULL;
").
:- pragma foreign_proc("C#", new_ref(Ref::out),
		[will_not_call_mercury, thread_safe], "
	Ref = new ME_Reference();
	Ref.val = null;
").


:- pragma inline(get_ref/2).
:- pragma c_code(get_ref(Ref::in, X::uo),
		[will_not_call_mercury, thread_safe],
"
	X = *(MR_Word *) Ref;
	*(MR_Word *) Ref = (MR_Word) NULL;
").
:- pragma foreign_proc("C#", get_ref(Ref::in, X::uo),
		[will_not_call_mercury, thread_safe], "
	X = Ref.val;
	Ref.val = null;
").

:- pragma inline(set_ref/2).
:- pragma c_code(set_ref(Ref::in, X::in),
		[will_not_call_mercury, thread_safe],
"
	*(MR_Word *) Ref = (MR_Word) X;
").
:- pragma foreign_proc("C#", set_ref(Ref::in, X::in),
		[will_not_call_mercury, thread_safe], "
	Ref.val = X;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
