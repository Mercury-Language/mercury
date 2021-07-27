%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- export_sym((list/1, append/3, member)).
:- export_pred((append/3, member)).
:- export_type((list/1, bag)).

fact.
rule :-
    fact.

:- incorrect_declaration.

:- pred p(t1).

p(a).
p(X) :- X = b.

:- type t1
    --->    a
    ;       b
    ;       c.

:- type t2
    --->    c
    ;       d
    ;       e.

:- type t3 = t1 + t2.
