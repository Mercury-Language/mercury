%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module simple_code.
:- interface.
:- pred p(int::in, int::out) is erroneous.
:- implementation.

:- import_module require.
p -->
    (
        []
    ;
        { error("foo") }
    ),
    ( { true } ->
        { Z = 2 }
    ;
        { Z = 3 }
    ),
    ( { X = 3, X = 2, Z = 2 } ->
        []
    ;
        []
    ),
    ( { \+ true } ->
        []
    ;
        []
    ),
    ( { \+ det_pred } ->
        []
    ;
        []
    ),
    ( { \+ fail_pred } ->
        []
    ;
        []
    ),
    { \+ fail },
    { obsolete1 },
    { obsolete2 },
    { obsolete3 },
    ( { error("blah") } ->
        []
    ;
        []
    ).

:- pred det_pred is det.

det_pred.

:- pred fail_pred is failure.

fail_pred :- fail.

:- pred obsolete1 is det.
:- pragma obsolete(obsolete1/0).

obsolete1.

:- pred obsolete2 is det.
:- pragma obsolete(obsolete2/0, [pred42/0]).

obsolete2.

:- pred obsolete3 is det.
:- pragma obsolete(obsolete3/0, [pred42/0, wonderful.pred43/0]).

obsolete3.

% This should give a warning about the second disjunct never succeeding.
:- pred r(int, int).
:- mode r(in(bound(1)), out(bound(42))) is det.

r(1, 42).
r(2, 21).

% This should not give a warning, because the second disjunct can
% succeed in the first mode.
:- pred q(int, int).
:- mode q(in, out) is semidet.
:- mode q(in(bound(1)), out(bound(42))) is det.

q(1, 42).
q(2, 21).

:- type node ---> a ; b ; c.

:- pred parent(node, node).
:- mode parent(in, out).
:- mode parent(out, in).
parent(a, b).
parent(b, c).
parent(a, c).

:- pred node(node).
:- mode node(out).
node(a).
node(b).
node(c).

:- pred anc(node, node).
:- mode anc(in, out).
:- mode anc(out, in).
anc(X, X) :-
    node(X).
anc(X, Z) :-
    parent(X, Y),
    anc(Y, Z).

