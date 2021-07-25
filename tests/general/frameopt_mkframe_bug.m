%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% $ mc bug.m
% Software error: mkframe in frameopt.doit

:- module frameopt_mkframe_bug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module bool.
:- import_module int.

main(!IO) :-
    p1(X1),
    io.write_int(X1, !IO),
    io.nl(!IO),
    p2(X2),
    io.write_int(X2, !IO),
    io.nl(!IO).

:- pred p1(int::out) is multi.

p1(X) :-
    q(FindMe),
    ( if u(41) then
        Z = 1
    else
        ( r(Z)
        ; s(FindMe, Z)
        )
    ),
    t(FindMe, Z, X).

:- pred p2(int::out) is multi.

p2(X) :-
    q(Y2),
    (
%       ( u(41) ->
%           Z = 1
%       ;
            Z = 111
%       )
    ;
        ( r(Z)
        ; s(Y2, Z)
        )
    ),
    t(Y2, Z, X).

:- pred q(int::out) is det.

q(2).

:- pred r(int::out) is det.

r(3).

:- pred s(int::in, int::out) is det.

s(X, Y4) :-
    Y4 = X + 10.

:- pred t(int::in, int::in, int::out) is det.

t(X, Y5, Z) :-
    Z = X * 100 + Y5.

:- pred u(int::in) is semidet.

u(A) :-
    A > 30.

:- pred v(bool::out) is det.

v(yes).
