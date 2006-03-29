%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mutvar.m.

% This module is used in the implementation of all-solutions predicates
% (solutions.m) and stores (store.m).  This module is a private part of the
% Mercury implementation; user modules should never explicitly import this
% module.  The interface for this module does not get included in the Mercury
% library reference manual.

% XXX When we move to use submodules in the standard library, we should
% make this module a private submodule.

%-----------------------------------------------------------------------------%

:- module mutvar.
:- interface.

    % A non-backtrackably destructively modifiable reference type.
    %
:- type mutvar(T).

    % Create a new mutvar given a term for it to reference.
    %
:- impure pred new_mutvar(T, mutvar(T)).
:-        mode new_mutvar(in, out) is det.
:-        mode new_mutvar(di, uo) is det.

    % Get the value currently referred to by a reference.
    %
:- impure pred get_mutvar(mutvar(T), T) is det.
:-        mode get_mutvar(in, uo) is det.   % XXX this is a work-around
/*
XXX `ui' modes don't work yet
:-        mode get_mutvar(in, uo) is det.
:-        mode get_mutvar(ui, uo) is det.   % unsafe, but we use it safely
*/

    % Destructively modify a reference to refer to a new object.
    % 
:- impure pred set_mutvar(mutvar(T), T) is det.
:-        mode set_mutvar(in, in) is det.
/*
XXX `ui' modes don't work yet
:-        pred set_mutvar(ui, di) is det.
*/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma inline(new_mutvar/2).
:- pragma inline(get_mutvar/2).
:- pragma inline(set_mutvar/2).

%-----------------------------------------------------------------------------%
%
% C implementation
%

    %  This type is a builtin type whose operations are implemented in C.
    %
:- type mutvar(T)
    --->    mutvar(private_builtin.ref(T)).

:- pragma foreign_proc("C",
    new_mutvar(X::in, Ref::out),
    [will_not_call_mercury, thread_safe],
"
    MR_offset_incr_hp_msg(Ref, MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 1,
        MR_PROC_LABEL, ""mutvar.mutvar/1"");
    MR_define_size_slot(0, Ref, 1);
    * (MR_Word *) Ref = X;
").
:- pragma foreign_proc("C",
    new_mutvar(X::di, Ref::uo),
    [will_not_call_mercury, thread_safe],
"
    MR_offset_incr_hp_msg(Ref, MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 1,
        MR_PROC_LABEL, ""mutvar.mutvar/1"");
    MR_define_size_slot(0, Ref, 1);
    * (MR_Word *) Ref = X;
").

:- pragma foreign_proc("C",
    get_mutvar(Ref::in, X::uo),
    [will_not_call_mercury, thread_safe],
"
    X = * (MR_Word *) Ref;
").

:- pragma foreign_proc("C",
    set_mutvar(Ref::in, X::in),
    [will_not_call_mercury, thread_safe],
"
    *(MR_Word *) Ref = X;
").

%-----------------------------------------------------------------------------%
%
% C# implementation
%

:- pragma foreign_proc("C#",
    new_mutvar(X::in, Ref::out),
    [will_not_call_mercury, thread_safe],
"
    Ref = new object[1];
    Ref[0] = X;
").
:- pragma foreign_proc("C#",
    new_mutvar(X::di, Ref::uo),
    [will_not_call_mercury, thread_safe],
"
    Ref = new object[1];
    Ref[0] = X;
").

:- pragma foreign_proc("C#",
    get_mutvar(Ref::in, X::uo),
    [will_not_call_mercury, thread_safe],
"
    X = Ref[0];
").

:- pragma foreign_proc("C#",
    set_mutvar(Ref::in, X::in),
    [will_not_call_mercury, thread_safe],
"
    Ref[0] = X;
").

%-----------------------------------------------------------------------------%
%
% Java implementation
%

:- pragma foreign_code("Java",
"
    public static class Mutvar {
        public Object object;

        public Mutvar(Object init) {
            object = init;
        }
    }
").

:- pragma foreign_type("Java", mutvar(T), "mercury.mutvar.Mutvar").

:- pragma foreign_proc("Java",
    new_mutvar(X::in, Ref::out),
    [will_not_call_mercury, thread_safe],
"
    Ref = new mercury.mutvar.Mutvar(X);
").
:- pragma foreign_proc("Java",
    new_mutvar(X::di, Ref::uo),
    [will_not_call_mercury, thread_safe],
"
    Ref = new mercury.mutvar.Mutvar(X);
").

:- pragma foreign_proc("Java",
    get_mutvar(Ref::in, X::uo),
    [will_not_call_mercury, thread_safe],
"
    X = Ref.object;
").

:- pragma foreign_proc("Java",
    set_mutvar(Ref::in, X::in),
    [will_not_call_mercury, thread_safe],
"
    Ref.object = X;
").

%-----------------------------------------------------------------------------%
:- end_module mutvar.
%-----------------------------------------------------------------------------%
