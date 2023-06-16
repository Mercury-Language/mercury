%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This program triggers (as of 8/9/2005) a mode error because the type_info
% arguments in the call to g are in the wrong order.

:- module type_info_order.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if p(1) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- some [T] pred g(T::out, U::in) is det.

g(X, X).

:- pred p(T::in) is semidet.

p(A) :-
    g(B, A),
    q(B, B).

:- pred q(T::in, T::in) is semidet.

q(Z, Z).
