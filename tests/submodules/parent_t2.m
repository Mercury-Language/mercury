%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% "Hello World" in Mercury, using nested modules.

:- module parent_t2.
:- interface.

:- include_module parent_t2_helper_1.

:- implementation.

:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

hello(!IO) :-
    print("parent_t2.hello\n", !IO).
