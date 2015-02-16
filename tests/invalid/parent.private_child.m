%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module parent:private_child.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello -->
    io__write_string("parent:private_child:hello\n").
