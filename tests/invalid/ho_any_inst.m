%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho_any_inst.
:- interface.

% Test calling higher order terms that have `any' insts. It is an error
% to call such a term in a negated context.

:- solver type nat.

:- func c(int::in) = (nat::oa) is det.
:- pred eq(nat::ia, nat::ia) is semidet.

:- pred p0(nat::ia, (pred)::out(any_pred is semidet)) is det.

:- pred p1(nat::ia, pred(nat)::out(any_pred(ia) is semidet)) is det.

:- pred cond0(int::in, int::out) is det.
:- pred neg0(int::in) is semidet.

:- pred cond1(int::in, int::out) is det.
:- pred neg1(int::in) is semidet.

:- implementation.

:- solver type nat
    where representation    is int,
          equality          is eq.

c(RA) = A :-
    promise_pure (
        impure A = 'representation to any nat/0'(RA)
    ).

eq(A, B) :-
    promise_pure (
        impure RA = 'representation of any nat/0'(A),
        impure RB = 'representation of any nat/0'(B),
        RA = RB
    ).

p0(A, Pred) :-
    Pred = (any_pred is semidet :- eq(A, c(0))).

p1(A, Pred) :-
    Pred = (any_pred(B::ia) is semidet :- eq(A, B)).

cond0(X, Y) :-
    p0(c(X), P),
    ( if call(P) then           % Illegal!
        Y = 1
    else
        Y = 2
    ).

neg0(X) :-
    p0(c(X), P),
    not call(P).                % Illegal!

cond1(X, Y) :-
    p1(c(X), P),
    ( if call(P, c(0)) then     % Illegal!
        Y = 1
    else
        Y = 2
    ).

neg1(X) :-
    p1(c(X), P),
    not call(P, c(0)).          % Illegal!
