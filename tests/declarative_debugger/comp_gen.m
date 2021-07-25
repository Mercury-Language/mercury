%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module comp_gen.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    data(A, B),
    ( if p(A, B) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- type foo(X)
    --->    f(X).

:- pred data(foo(int)::out, foo(int)::out) is det.

data(f(1), f(2)).

:- pred p(T::in, T::in) is semidet.

p(X, X).

