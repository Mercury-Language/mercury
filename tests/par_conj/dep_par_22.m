%
% The switch was not detected.
% 
% par22.m:001: Warning: interface for module `par22' does not export anything.
% par22.m:018: Error: parallel conjunct may fail. The current implementation
% par22.m:018:   supports only single-solution non-failing parallel conjunctions.
% par22.m:019:   Unification of `T' and `par22.up' can fail.
% par22.m:020:   Unification of `T' and `par22.down' can fail.
% par22.m:020:   Disjunction has multiple clauses with solutions.
%

:- module dep_par_22.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(up, X),
    io.print(X, !IO),
    io.nl(!IO).

:- type t
    --->    up
    ;	    down.

:- pred p(t::in, t::out) is det.

p(T0, X) :-
    T = T0
    &
    ( T = up, X = up
    ; T = down, X = down
    ).
