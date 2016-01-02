%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module is invalid, because there is no `include_module' declaration
% for it in parent.m.

:- module parent.undeclared_child.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("parent.undeclared_child.hello\n", !IO).
