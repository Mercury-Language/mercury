%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A simple test of arbitrary precision rationals.
%

:- module rational_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module io.
:- import_module list.
:- import_module rational.
:- import_module string.

main(!IO) :-
    io.write_string(rat2s(cf2rat(root2_cf(80))), !IO), io.nl(!IO),
    io.write_string(rat2s(cf2rat(e_cf(20))), !IO), io.nl(!IO).

:- func rat2s(rational) = string.

rat2s(Rat) = S :-
    Num = numer(Rat),
    Den = denom(Rat),
    NS = integer.to_string(Num),
    ND = integer.to_string(Den),
    string.append_list([NS, " / ", ND], S).

:- func cf2rat(list(int)) = rational.

cf2rat([]) = one.
cf2rat([N | Ns]) = rational(N, 1) + rational__reciprocal(CF) :-
    CF = cf2rat(Ns).

    % Continued fraction expansion of Euler's constant `e'.
    %
:- func e_cf(int) = list(int).

e_cf(N) = CF :-
    list.append([2, 1, 2], Rest, CF),
    Rest = e_aux(N, 4).

:- func e_aux(int, int) = list(int).

e_aux(N, A) = List :-
    ( if N =< 0 then
        List = []
    else
        List = [1, 1, A | e_aux(N-1, A+2)]
    ).

:- func root2_cf(int) = list(int).

root2_cf(N) = [1 | Rest] :-
    Rest = n_of(N, 2).

:- func n_of(int, T) = list(T).

n_of(N, A) =
    ( if N =< 0 then
        []
    else
        [A | n_of(N-1, A)]
    ).
