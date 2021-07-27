% vim: ft=mercury ts=4 sts=4 sw=4 et

:- module ho_func_call_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type t
    --->    a
    ;       b
    ;       c.

:- inst s
    --->    a
    ;       b.

:- type foo
    --->    foo(func(t) = t).

:- pred callfoo(foo::in, t::in, t::out) is det.

callfoo(foo(F), X, F(X)).

:- func subfoo(t::in) = (t::out(s)) is det.

subfoo(_) = a.

main(!IO) :-
    callfoo(foo(subfoo), c, X),
    io.write_line(X, !IO).
