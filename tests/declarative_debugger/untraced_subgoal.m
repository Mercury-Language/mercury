%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module untraced_subgoal.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module untraced_subgoal_sub.

main(!IO) :-
    ( if
        p(1, X),
        X > 10
    then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ),
    ( if
        p(2, Y),
        Y > 10
    then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred p(int::in, int::out) is nondet.

p(1, X) :-
    untraced_subgoal_sub__q(X).

p(2, X) :-
    untraced_subgoal_sub__q(X),
    r(X, Y),
    s(Y).

:- pred r(int::in, int::out) is det.

r(X, X).

:- pred s(int::out) is multi.

s(2).
s(3).

% :- pred q(int::out) is multi.
%
% q(1).
% q(2).
