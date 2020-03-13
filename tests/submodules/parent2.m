%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% "Hello World" in Mercury, using nested modules.

:- module parent2.
:- interface.

:- include_module child.

:- implementation.

:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

hello(!IO) :-
    print("parent2.hello\n", !IO).
