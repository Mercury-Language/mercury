%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module contravariance_bug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module list.

main(!IO) :-
    ( if q(p) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- type intlist == list(int).
:- inst nonempty
    --->    [ground | list].
:- inst list
    --->    [ground | list]
    ;       [].

:- pred p(intlist, intlist).
:- mode p((list >> nonempty), (free >> nonempty)) is semidet.

p([X], [X]).
p([X, Y | Zs], [Y, X | Zs]).

:- pred q(pred(intlist, intlist)).
:- mode q(pred((nonempty >> nonempty), (free >> list)) is semidet) is semidet.

q(P) :-
    P([1], L),
    L \= [].
