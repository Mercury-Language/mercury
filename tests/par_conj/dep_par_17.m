% multiple clauses

:- module dep_par_17.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    p(t1, X),
    io.write_int(X, !IO),
    io.nl(!IO).

:- type t ---> t1 ; t2.

:- pred p(t::in, int::out) is det.
:- pragma no_inline(p/2).

p(t1, X) :-
    q(1, X1) &
    q(X1, X).
p(t2, X) :-
    q(2, X1) &
    q(X1, X).

:- pred q(int::in, int::out) is det.
:- pragma no_inline(q/2).
q(X, X+1).
