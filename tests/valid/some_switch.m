%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test of switch detection with explicit existential quantification.

:- module some_switch.
:- interface.

:- type nat
    --->    o
    ;       s(nat).

:- func add(nat, nat) = nat.
:- mode add(in, in)   = in  is semidet.
:- mode add(out, out) = in  is multi.
:- mode add(in, in)   = out is det.

:- implementation.

add(X, Y) = Z :-
    (
        X = o,
        Z = Y
    ;
        some [Xp] (
            X = s(Xp),
            Z = s(add(Xp, Y))
        )
    ).
