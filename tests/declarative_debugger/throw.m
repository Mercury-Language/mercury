%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for ???
% The .exp2 file is for when we optimize at -O2 or less.
% The .exp3 file is for when we optimize var saves across stack flushes.
% The .exp4 file is .exp2 but with --delay-constructs (set by -O5).

:- module throw.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module exception.
:- import_module int.

main(!IO) :-
    try(p, X),
    io.write_line(X, !IO),
    try(q, Y),
    io.write_line(Y, !IO).

:- pred p(int::out) is cc_nondet.

p(X) :-
    a(A),
    b(A, X),
    X < 0.

:- pred a(int::out) is multi.

a(2).
a(3).

:- pred b(int::in, int::out) is multi.

b(A, B) :-
    (
        B = A * 3
    ;
        B = A * 4
    ),
    ( if B > 10 then
        throw("Too big")
    else
        true
    ).

:- pred q(int::out) is semidet.

q(1) :-
    not (
        a2(A),
        not (
            b2(A, 0)
        ),
        A < 0
    ).

:- pred a2(int::out) is multi.

a2(2).
a2(3).

:- pred b2(int::in, int::out) is multi.

b2(A, B) :-
    (
        B = A * 3
    ;
        B = A * 4
    ),
    ( if B > 10 then
        throw("Too big")
    else
        true
    ).
