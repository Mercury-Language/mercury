%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Main author: conway
% Stability: medium.
%
% This module provides a simple mechanism for storing values associated
% with keys in the global io__state. It is quite like library/store.m,
% except that it implicitly stores things in the io__state rather than in a
% separate store.
%
%---------------------------------------------------------------------------%
:- module global.

:- interface.

:- import_module io.

:- type global(T).
:- pragma foreign_type(c,  global(T), "MR_Word").
:- pragma foreign_type(il, global(T), "class [global__csharp_code]ME_Global").

	% new(Thing, Key, IO0, IO) binds `Key' to an abstract key refering
	% to the object `Thing'.
:- pred global__new(T, global(T), io__state, io__state).
:- mode global__new(in, out, di, uo) is det.

	% get(Key, Thing, IO0, IO) binds `Thing' to the object currently
	% associated with `Key'.
:- pred global__get(global(T), T, io__state, io__state).
:- mode global__get(in, out, di, uo) is det.

	% set(Key, Thing, IO0, IO) changes the value associated with `Key'
	% to be `Thing'.
:- pred global__set(global(T), T, io__state, io__state).
:- mode global__set(in, in, di, uo) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module std_util.

:- type global(T).

:- pragma foreign_decl("C#", "
	public class ME_Global {
		public object val;
	}
").

:- pragma c_code(global__new(Thing::in, Glob::out, IO0::di, IO::uo),
		will_not_call_mercury, "{
	MR_Word *tmp;
	MR_incr_hp((MR_Word) tmp, 1);
	*tmp = Thing;
	Glob = (MR_Word) tmp;
	IO = IO0;
}").
:- pragma foreign_proc("C#", new(Thing::in, Glob::out, _IO0::di, _IO::uo),
		[will_not_call_mercury, thread_safe, promise_pure], "
	Glob = new ME_Global();
	Glob.val = Thing;
").

:- pragma c_code(global__get(Glob::in, Thing::out, IO0::di, IO::uo),
		will_not_call_mercury, "{
	Thing = * (MR_Word *) Glob;
	IO = IO0;
}").
:- pragma foreign_proc("C#", get(Glob::in, Thing::out, _IO0::di, _IO::uo),
		[will_not_call_mercury, thread_safe, promise_pure], "
	Thing = Glob.val;
").

:- pragma c_code(global__set(Glob::in, Thing::in, IO0::di, IO::uo),
		will_not_call_mercury, "{
	* ((MR_Word *) Glob) = Thing;
	IO = IO0;
}").
:- pragma foreign_proc("C#", set(Glob::in, Thing::in, _IO0::di, _IO::uo),
		[will_not_call_mercury, thread_safe, promise_pure], "
	Glob.val = Thing;
").
