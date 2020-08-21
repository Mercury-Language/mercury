%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% crypt
%
% Cryptomultiplication:
% Find the unique answer to:
%   OEE
%    EE
%   ---
%      EOEE
%      EOE
%      ----
%      OOEE
%
% where E=even, O=odd.
% This program generalizes easily
% to any such problem.
% Originally written by Peter Van Roy

:- module crypt.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module prolog.
:- import_module require.

main(!IO) :-
    ( if crypt(Out) then
        print_list(Out, !IO)
    else
        io.write_string("No solution\n", !IO)
    ).

:- pred crypt(list(int)::out) is nondet.

crypt([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]) :-
    crypt.odd(A),
    crypt.even(B),
    crypt.even(C),
    crypt.even(E),
    mult([C, B, A], E, [I, H, G, F | X]),
    lefteven(F),
    crypt.odd(G),
    crypt.even(H),
    crypt.even(I),
    zero(X),
    lefteven(D),
    mult([C, B, A], D, [L, K, J | Y]),
    lefteven(J),
    crypt.odd(K),
    crypt.even(L),
    zero(Y),
    sum2([I, H, G, F], [0, L, K, J], [P, O, N, M | Z]),
    crypt.odd(M),
    crypt.odd(N),
    crypt.even(O),
    crypt.even(P),
    zero(Z).
    % write(' '), write(A), write(B), write(C), nl,
    % write('  '), write(D), write(E), nl,
    % write(F), write(G), write(H), write(I), nl,
    % write(J), write(K), write(L), nl,
    % write(M), write(N), write(O), write(P), nl.

% In the usual source this predicate is named sum. However, sum is a
% language construct in NU-Prolog, and cannot be defined as a predicate.
% If you try, nc comes up with an obscure error message.

:- pred sum2(list(int)::in, list(int)::in, list(int)::out) is det.

sum2(AL, BL, CL) :-
    sum2(AL, BL, 0, CL).

:- pred sum2(list(int)::in, list(int)::in, int::in, list(int)::out) is det.

sum2([], [], Carry, Cs) :-
    ( if Carry = 0 then
        Cs = []
    else
        Cs = [Carry]
    ).
sum2([], [B | BL], Carry, Cs) :-
    ( if Carry = 0 then
        Cs = [B | BL]
    else
        X = B + Carry,
        NewCarry = X // 10,
        C = X mod 10,
        sum2([], BL, NewCarry, CL),
        Cs = [C | CL]
    ).
sum2([A | AL], [], Carry, Cs) :-
    ( if Carry = 0 then
        Cs = [A | AL]
    else
        X = A + Carry,
        NewCarry = X // 10,
        C = X mod 10,
        sum2([], AL, NewCarry, CL),
        Cs = [C | CL]
    ).
sum2([A | AL], [B | BL], Carry, Cs) :-
    X1 = A + B,
    X = X1 + Carry,
    C = X mod 10,
    NewCarry = X // 10,
    sum2(AL, BL, NewCarry, CL),
    Cs = [C | CL].

:- pred mult(list(int)::in, int::in, list(int)::out) is det.

mult(AL, D, BL) :-
    mult(AL, D, 0, BL).

:- pred mult(list(int)::in, int::in, int::in, list(int)::out) is det.

mult([A | AL], D, Carry, [B | BL] ) :-
    X1 = A * D,
    X = X1 + Carry,
    B = X mod 10,
    NewCarry = X // 10,
    mult(AL, D, NewCarry, BL).
mult([], _, Carry, [C, Cend]) :-
    C = Carry mod 10,
    Cend = Carry // 10.

:- pred zero(list(int)::in) is semidet.

zero([]).
zero([0 | L]) :-
    zero(L).

:- pred odd(int).
:- mode odd(in) is semidet.
:- mode odd(out) is multi.

odd(1).
odd(3).
odd(5).
odd(7).
odd(9).

:- pred even(int).
:- mode even(in) is semidet.
:- mode even(out) is multi.

even(0).
even(2).
even(4).
even(6).
even(8).

:- pred lefteven(int).
:- mode lefteven(in) is semidet.
:- mode lefteven(out) is multi.

lefteven(2).
lefteven(4).
lefteven(6).
lefteven(8).

:- pred print_list(list(int)::in, io::di, io::uo) is det.

print_list(Xs, !IO) :-
    (
        Xs = [],
        io.write_string("[]\n", !IO)
    ;
        Xs = [H | T],
        io.write_string("[", !IO),
        print_list_elements(H, T, !IO),
        io.write_string("]\n", !IO)
    ).

:- pred print_list_elements(int::in, list(int)::in, io::di, io::uo) is det.

print_list_elements(X, Xs, !IO) :-
    io.write_int(X, !IO),
    (
        Xs = []
    ;
        Xs = [H | T],
        io.write_string(", ", !IO),
        print_list_elements(H, T, !IO)
    ).
