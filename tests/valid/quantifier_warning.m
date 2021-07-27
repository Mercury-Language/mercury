%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The compiler used to issue a spurious warning
% about `M' having an unbound type.

:- module quantifier_warning.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool.
:- import_module int.

main(!IO) :-
    p(1, R),
    io.write_line(R, !IO).

:- pred p(int::in, bool::out) is det.

p(N, R) :-
    ( if
        some [M] (
            M > 5,
            q(N, M)
        )
    then
        R = yes
    else
        R = no
    ).

:- pred q(int::in, int::out) is nondet.

q(0, 0).
q(1, 1).
q(1, 2).
q(1, 3).
q(2, 2).
q(2, 4).
