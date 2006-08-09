% A program to test that calls to par_builtin.wait are being delayed
% as long as possible properly.

:- module dep_par_25.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module int.
:- import_module list.

main(!IO) :-
    ite_cond_dep(Y1),
%     Y1 = 1,
    ite_cond_free(Y2),
%     Y2 = 1,
    switch_on_var(Y3),
%     Y3 = 1,
    switch_not_on_var(Y4),
%     Y4 = 1,
    disj(Y5),
%     Y5 = 1,
    conj(Y6),
%     Y6 = 1,
    par_conj(Y7),
%     Y7 = 7,
    neg(Y8),
%     Y8 = 1,
    scope(Y9),
%     Y9 = 1,
    do_call(Y10),
%     Y10 = 1,
    io.print({Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10}, !IO), 
    io.nl(!IO).

:- func one = int.
:- pragma no_inline(one/0).

one = 1.

:- type t ---> t1 ; t2.

:- func some_t = t.
:- pragma no_inline(some_t/0).

some_t = t1.

:- pred ite_cond_dep(int::out) is det.
:- pragma no_inline(ite_cond_dep/1).

ite_cond_dep(Y) :-
    (
        X = one
    &
        (if int.even(X) then
            Y = X+1
        else
            Y = X+2
        )
    ).

:- pred ite_cond_free(int::out) is det.
:- pragma no_inline(ite_cond_free/1).

ite_cond_free(Y) :-
    (
        X = one
    &
        (if semidet_succeed then
            Y = X+1
        else
            Y = X+2
        )
    ).

:- pred switch_on_var(int::out) is det.
:- pragma no_inline(switch_on_var/1).

switch_on_var(Y) :-
    (
        T = some_t
    &
        (
            T = t1,
            Y = 1
        ;
            T = t2,
            Y = 2
        )
    ).

:- pred switch_not_on_var(int::out) is det.
:- pragma no_inline(switch_not_on_var/1).

switch_not_on_var(Y) :-
    (
        X = one
    &
        T = some_t,
        (
            T = t1,
            Y = X+1
        ;
            T = t2,
            Y = X+2
        )
    ).

:- pred disj(int::out) is cc_multi.
:- pragma no_inline(disj/1).

disj(Y) :-
    (
        X = one
    &
        T = some_t,
        (
            T = t2,
            Y = X+1
        ;
            Y = X+2
        )
    ).

:- pred conj(int::out) is det.
:- pragma no_inline(conj/1).

conj(Y) :-
    U = one,
    V = 2,
    (
        W = 3
    &
        A = U+4,
        Y = U + V + W + A + B
    &
        B = 5
    ).

:- pred par_conj(int::out) is det.
:- pragma no_inline(par_conj/1).

par_conj(Y) :-
    (
        X = one
    &
        U = 2,
        (
            V = X + U
        &
            Y = V + X + U
        )
    ).

:- pred neg(int::out) is cc_multi.
:- pragma no_inline(neg/1).

neg(Y) :-
    (
        X = one
    &
        ( 
            not odd(X),
            Y = 2
        ;
            odd(X),
            Y = 1
        ;
            Y = 0
        )
    ).

:- pred scope(int::out) is det.
:- pragma no_inline(scope/1).

scope(Y) :-
    (
        X = one
    &
        promise_equivalent_solutions [Y]
	( Y = X
        ; Y = 1
        )
    ).

:- pred do_call(int::out) is det.
:- pragma no_inline(do_call/1).

do_call(Y) :-
    (
        X = [1,2,3]
    &
        list.length(X, Y)
    ).
