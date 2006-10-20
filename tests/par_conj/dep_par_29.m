:- module dep_par_29.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
        p(X),
        % We were missing a get(FutureX, X) call here.
        % It's needed because the signal(FutureX, X) call is
        % pushed into p, so X isn't bound anywhere.
        q(X, Y)
    &
        q(X, Z)
    ),
    io.print({X,Y,Z}, !IO),
    io.nl(!IO).

:- pred p(int::out) is det.
:- pragma no_inline(p/1).
p(1).

:- pred q(int::in, int::out) is det.
:- pragma no_inline(q/2).
q(X,X+10).
