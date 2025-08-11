%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2007, 2010 The University of Melbourne.
% Copyright (C) 2013-2016, 2018, 2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: mutvar.m.
% Stability: high.
%
% This module is used in the implementation of all-solutions predicates
% (solutions.m) and stores (store.m).  This module is a private part of the
% Mercury implementation; user modules should never explicitly import this
% module.  The interface for this module does not get included in the Mercury
% library reference manual.
%
% XXX When we move to use submodules in the standard library, we should
% make this module a private submodule.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

    % Create a new mutvar with undefined initial value.
    %
:- impure pred new_mutvar0(mutvar(T)).
:-        mode new_mutvar0(uo) is det.

    % Get the value currently referred to by a reference.
    %
:- impure pred get_mutvar(mutvar(T), T).
:-        mode get_mutvar(in, uo) is det.   % XXX this is a work-around
/*
XXX `ui' modes don't work yet
:-        mode get_mutvar(in, uo) is det.
:-        mode get_mutvar(ui, uo) is det.   % unsafe, but we use it safely
*/

    % Destructively modify a reference to refer to a new object.
    %
:- impure pred set_mutvar(mutvar(T), T).
:-        mode set_mutvar(in, in) is det.
/*
XXX `ui' modes don't work yet
:-        pred set_mutvar(ui, di) is det.
*/

    % Destructively clear a reference to avoid retaining the value.
    %
:- impure pred clear_mutvar(mutvar(T)).
:-        mode clear_mutvar(in) is det.
/*
XXX `ui' modes don't work yet
:-        pred clear_mutvar(ui) is det.
*/

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma inline(pred(new_mutvar/2)).
:- pragma inline(pred(new_mutvar0/1)).
:- pragma inline(pred(get_mutvar/2)).
:- pragma inline(pred(set_mutvar/2)).
:- pragma inline(pred(clear_mutvar/1)).

%---------------------------------------------------------------------------%

new_mutvar(X, Ref) :-
    impure new_mutvar0(Ref0),
    impure set_mutvar(Ref0, X),
    Ref = unsafe_promise_unique(Ref0).

%---------------------------------------------------------------------------%

:- pragma foreign_code("Java",
"
    public static class Mutvar implements java.io.Serializable {
        public Object object;

        public Mutvar() {
        }

        public Mutvar(Object o) {
            object = o;
        }
    }
").

    %  This type is a builtin type whose operations are implemented in C.
    %
:- type mutvar(T)
    --->    mutvar(private_builtin.ref(T)).

:- pragma foreign_type("C#", mutvar(T), "object[]").
:- pragma foreign_type("Java", mutvar(T), "mutvar.Mutvar").

%---------------------%

:- pragma foreign_proc("C",
    new_mutvar0(Ref::uo),
    [will_not_call_mercury, thread_safe],
"
    MR_offset_incr_hp_msg(Ref, MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 1,
        MR_ALLOC_ID, ""mutvar.mutvar/1"");
    MR_define_size_slot(0, Ref, 1);
").
:- pragma foreign_proc("C#",
    new_mutvar0(Ref::uo),
    [will_not_call_mercury, thread_safe],
"
    Ref = new object[1];
").
:- pragma foreign_proc("Java",
    new_mutvar0(Ref::uo),
    [will_not_call_mercury, thread_safe],
"
    Ref = new mutvar.Mutvar();
").

%---------------------%

:- pragma foreign_proc("C",
    get_mutvar(Ref::in, X::uo),
    [will_not_call_mercury, thread_safe],
"
    X = * (MR_Word *) Ref;
").
:- pragma foreign_proc("C#",
    get_mutvar(Ref::in, X::uo),
    [will_not_call_mercury, thread_safe],
"
    X = Ref[0];
").
:- pragma foreign_proc("Java",
    get_mutvar(Ref::in, X::uo),
    [will_not_call_mercury, thread_safe],
"
    X = Ref.object;
").

%---------------------%

:- pragma foreign_proc("C",
    set_mutvar(Ref::in, X::in),
    [will_not_call_mercury, thread_safe],
"
    *(MR_Word *) Ref = X;
").
:- pragma foreign_proc("C#",
    set_mutvar(Ref::in, X::in),
    [will_not_call_mercury, thread_safe],
"
    Ref[0] = X;
").
:- pragma foreign_proc("Java",
    set_mutvar(Ref::in, X::in),
    [will_not_call_mercury, thread_safe],
"
    Ref.object = X;
").

%---------------------%

:- pragma foreign_proc("C",
    clear_mutvar(Ref::in),
    [will_not_call_mercury, thread_safe],
"
    *(MR_Word *) Ref = 0;
").
:- pragma foreign_proc("C#",
    clear_mutvar(Ref::in),
    [will_not_call_mercury, thread_safe],
"
    Ref[0] = null;
").
:- pragma foreign_proc("Java",
    clear_mutvar(Ref::in),
    [will_not_call_mercury, thread_safe],
"
    Ref.object = null;
").

%---------------------------------------------------------------------------%
:- end_module mutvar.
%---------------------------------------------------------------------------%
