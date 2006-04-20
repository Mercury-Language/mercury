%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: global.m.
% Main author: conway.
% Stability: medium.
%
% This module provides a simple mechanism for storing values associated
% with keys in the global io.state. It is quite like library/store.m,
% except that it implicitly stores things in the io.state rather than in a
% separate store.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module global.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type global(T).

    % new(Thing, Key, !IO) binds `Key' to an abstract key referring
    % to the object `Thing'.
    %
:- pred global.new(T::in, global(T)::out, io::di, io::uo) is det.

    % get(Key, Thing, !IO) binds `Thing' to the object currently
    % associated with `Key'.
    %
:- pred global.get(global(T)::in, T::out, io::di, io::uo) is det.

    % set(Key, Thing, !IO) changes the value associated with `Key'
    % to be `Thing'.
    %
:- pred global.set(global(T)::in, T::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type(c,  global(T), "MR_Word").
:- pragma foreign_type(il, global(T), "class [global__csharp_code]ME_Global").

:- pragma foreign_decl("C#", "
    public class ME_Global {
        public object val;
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    new(Thing::in, Glob::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury],
"
    MR_Word tmp;
    MR_incr_hp(tmp, 1);
    *((MR_Word *)tmp) = Thing;
    Glob = tmp;
    IO = IO0;
").

:- pragma foreign_proc("C#",
    new(Thing::in, Glob::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Glob = new ME_Global();
    Glob.val = Thing;
").

:- pragma foreign_proc("C",
    get(Glob::in, Thing::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Thing = * (MR_Word *) Glob;
    IO = IO0;
").

:- pragma foreign_proc("C#",
    get(Glob::in, Thing::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Thing = Glob.val;
").

:- pragma foreign_proc("C",
    set(Glob::in, Thing::in, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury],
"
    * ((MR_Word *) Glob) = Thing;
    IO = IO0;
").

:- pragma foreign_proc("C#",
    set(Glob::in, Thing::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Glob.val = Thing;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
