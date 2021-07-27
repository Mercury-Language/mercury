%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module quant_constraint_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass c1(T1) where [].

    % Error: T is existentially quantified, but appears in a universal
    % constraint.
    %
:- some [T] (func q = T <= c1(T)).

:- implementation.

:- instance c1(int) where [].

q = 1.

main(!IO) :-
    X = q,
    io.write_line(X, !IO).
