%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module browse_arg.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(1, X),
    io.write_line(X, !IO).

:- type foo
    --->    bar
    ;       baz(int, foo).

:- pred p(int::in, foo::out) is det.

p(N, baz(N, bar)).
