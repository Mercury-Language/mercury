%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module cc_multi_bug.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(!IO) :-
    plus(N1, N2, s(zero)),
    print(N1, !IO), nl(!IO),
    print(N2, !IO), nl(!IO).

:- type nat
    --->    zero
    ;       s(nat).

:- pred plus(nat, nat, nat).
:- mode plus(out, out, in) is multi.
:- mode plus(out, out, in) is cc_multi.

plus(zero, zero, zero).
plus(zero, s(N), s(N)).
plus(s(N), zero, s(N)).
plus(s(N1), s(N2), s(N3)):-
    plus(N1, s(N2), N3).
