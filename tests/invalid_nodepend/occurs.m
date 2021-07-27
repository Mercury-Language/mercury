%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module occurs.

:- type t
    --->    []
    ;       f(t).

:- pred p is semidet.

p :-
    X = f(X).

:- pred p2 is semidet.
p2 :-
    X = [],
    X = f(X).

:- pred p3 is semidet.
p3 :-
    X = f(_),
    X = f(X).
