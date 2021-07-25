%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test tracking the origin of a sub-term from a failed if-then-else condition.

:- module failed_cond.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(X),
    write(X, !IO),
    nl(!IO).

:- type t
    --->    a
    ;       b
    ;       c.

:- pred p(t::out) is det.

p(Y) :- X = c, q(X, Y).

:- pred q(t::in, t::out) is det.

q(X, Y) :-
    ( if r(X) then
        Y = a
    else
        Y = b
    ).

:- pred r(t::in) is semidet.

r(a).
