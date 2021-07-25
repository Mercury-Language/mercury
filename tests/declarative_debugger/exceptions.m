%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module exceptions.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module univ.

main(!IO) :-
    % Test finding wrong answer children with a try_all.
    q(MaybeExcp, Solutions),
    ( if
        % Test finding missing answer children with a try_all.
        r(_)
    then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ),
    ( if
        % Test finding exception children when there is backtracking.
        v(X)
    then
        io.write_int(X, !IO)
    else
        io.write_string("no\n", !IO)
    ),
    io.write({MaybeExcp, Solutions}, !IO),
    io.nl(!IO).

:- pred p(int::out) is multi.

p(X) :-
    ( X = 1
    ; X = 2
    ; X = 3
    ; throw("Error")
    ).

:- pred q(maybe(univ)::out, list(int)::out) is cc_multi.

q(MaybeExcp, Solutions) :-
    try_all(p, MaybeExcp, Solutions).

:- pred r({maybe(univ), list(int)}::out) is semidet.

r(Sols) :-
    % This is a lie, but is the only way I seem to be able to call try_all
    % in a failing context.
    Sols = promise_only_solution(t),
    semidet_fail.

:- pred s(int::out) is multi.

s(X) :-
    ( X = 4
    ; X = 5
    ; X = 6
    ; throw("Error")
    ).

:- pred t({maybe(univ), list(int)}::out) is cc_multi.

t(S) :-
    try_all(s, MaybeExcp, Solutions),
    S = {MaybeExcp, Solutions}.

:- pred u(int::out) is multi.

u(X) :-
    ( X = 7
    ; X = 8
    ; X = 9
    ; throw("Error")
    ).

:- pred v(int::out) is nondet.

v(X) :-
    y(Z),
    u(Y),
    x(Y),
    add(Y, Z, X).

:- pred x(int::in) is semidet.

x(1).

:- pred add(int::in, int::in, int::out) is det.

add(X, Y, X + Y).

:- pred y(int::out) is det.

y(1).
