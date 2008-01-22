% vim: ts=4 sw=4 et
:- module ho_any_inst.
:- interface.

% Test creating and calling higher order terms that have `any' insts,
% including terms that have non-local variables with `any' insts.

:- solver type nat.

:- func c(int::in) = (nat::oa) is det.
:- pred eq(nat::ia, nat::ia) is semidet.

:- pred p0(nat::ia, (pred)::out(any_pred is semidet)) is det.
:- pred call_p0((pred)::in(any_pred is semidet)) is semidet.

:- pred p1(nat::ia, pred(nat)::out(any_pred(ia) is semidet)) is det.
:- pred call_p1(pred(nat)::in(any_pred(ia) is semidet), nat::ia) is semidet.

:- pred p2(nat::ia, nat::ia, pred(nat, nat)::out(any_pred(ia, ia) is semidet))
    is det.
:- pred call_p2(pred(nat, nat)::in(any_pred(ia, ia) is semidet),
    nat::ia, nat::ia) is semidet.
:- pred partial_call_p2(pred(nat, nat)::in(any_pred(ia, ia) is semidet),
    nat::ia, pred(nat)::out(any_pred(ia) is semidet)) is det.

:- func f0(nat::ia) = (((func) = nat)::out(any_func = ia is semidet)) is det.
:- func apply_f0(((func) = nat)::in(any_func = ia is semidet)) = (nat::ia)
    is semidet.

:- func f1(nat::ia) = ((func(nat) = nat)::out(any_func(ia) = ia is semidet))
    is det.
:- func apply_f1((func(nat) = nat)::in(any_func(ia) = ia is semidet), nat::ia)
    = (nat::ia) is semidet.

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

call_p0(Pred) :-
    call(Pred).

p1(A, Pred) :-
    Pred = (any_pred(B::ia) is semidet :- eq(A, B)).

call_p1(Pred, B) :-
    call(Pred, B).

p2(A1, A2, Pred) :-
    Pred = (any_pred(B1::ia, B2::ia) is semidet :- eq(A1, A2), eq(B1, B2)).

call_p2(Pred, B1, B2) :-
    call(Pred, B1, B2).

partial_call_p2(Pred2, _B1, Pred1) :-
    Pred1 = (any_pred(B2::ia) is semidet :- Pred2(c(0), B2)).

f0(A) = (any_func = (B::ia) is semidet :- eq(A, B)).

apply_f0(Func) = B :-
    apply(Func) = B.

f1(A) = (any_func(_B::ia) = (C::ia) is semidet :- eq(A, C)).

apply_f1(Func, B) = C :-
    apply(Func, B) = C.

