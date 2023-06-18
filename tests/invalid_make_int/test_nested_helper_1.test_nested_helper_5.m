%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module is invalid, because there is no `include_module' declaration
% for it in test_nested_helper_1.m.

:- module test_nested_helper_1.test_nested_helper_5.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("test_nested_helper_1.test_nested_helper_5.hello\n", !IO).
