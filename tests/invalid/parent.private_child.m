%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module parent:private_child.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("parent:private_child:hello\n", !IO).
