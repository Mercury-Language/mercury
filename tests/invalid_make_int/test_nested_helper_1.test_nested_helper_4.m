%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_nested_helper_1.test_nested_helper_4.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("test_nested_helper_1.test_nested_helper_4.hello\n", !IO).
