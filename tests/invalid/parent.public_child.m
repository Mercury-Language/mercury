%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module parent:public_child.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello -->
    io__write_string("parent:public_child:hello\n").
