%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module user_event_2.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- type foo
    --->    foo(int).

main(!IO) :-
    do_something(foo(43), !IO).

:- pred do_something(foo::in, io::di, io::uo) is det.

do_something(Data, !IO) :-
    event event_with_zero_arity_defined_type(Data).

