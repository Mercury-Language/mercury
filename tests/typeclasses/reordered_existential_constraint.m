%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module reordered_existential_constraint.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    foobar,
    io.write_string("Hi!\n", !IO).

:- typeclass c(T) where [].

:- instance c(int) where [].

% :- pred q(T).
:- pred q(T) <= c(T).
:- mode q(in) is det.

q(_).

% :- some [T] pred p(T).
:- some [T] pred p(T) => c(T).
:- mode p(out) is det.

p(1).

:- pred foobar is det.

foobar :-
    q(X),   % XXX polymorphism aborts here, looking for the variable that
            % contains the type class info for c(T).
    p(X).
