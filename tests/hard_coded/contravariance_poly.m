%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module contravariance_poly.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module bool.

main(!IO) :-
    ( if q(p) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- inst nonempty(I)
    --->    [I | list(I)].

:- type intlist == list(int).
:- inst bit == bound(0 ; 1).

:- pred p(intlist, intlist).
:- mode p((list(I) >> nonempty(I)), (free >> nonempty(I))) is semidet.

p([X], [X]).
p([X, Y | Zs], [Y, X | Zs]).

:- pred q(pred(intlist, intlist)).
:- mode q(pred((nonempty(bit) >> nonempty(bit)), (free >> list(bit)))
    is semidet) is semidet.

q(P) :-
    P([1], L),
    L \= [].
