% A program to test that calls to par_builtin.signal are being called
% as early as possible.

:- module dep_par_25b.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module int.
:- import_module list.

main(!IO) :-
    ite(Y1),
    switch(Y3),
    disj(Y5),
    conj(Y6),
    par_conj(Y7),
    scope(Y9),
    do_call(Y10),
    io.print({Y1, Y3, Y5, Y6, Y7, Y9, Y10}, !IO), 
    io.nl(!IO).

:- func one = int.
:- pragma no_inline(one/0).
one = 1.

:- func two = int.
:- pragma no_inline(two/0).
two = 2.

:- type t ---> t1 ; t2.

:- func some_t = t.
:- pragma no_inline(some_t/0).
some_t = t1.

:- pred ite(int::out) is det.
:- pragma no_inline(ite/1).

ite(Y) :-
    (
        (if int.even(one) then
            X = one
            % signal X
        else
            X = two
            % signal X
        )
    &
        % wait X
        Y = X
    ).

:- pred switch(int::out) is det.
:- pragma no_inline(switch/1).

switch(Y) :-
    (
        T = some_t,
        (
            T = t1,
            X = one
            % signal X
        ;
            T = t2,
            X = two
            % signal X
        )
    &
        Y = X
    ).

:- pred disj(int::out) is cc_multi.
:- pragma no_inline(disj/1).

disj(Y) :-
    (
        T = some_t,
        (
            T = t2,
            X = one
            % signal X
        ;
            X = two
            % signal X
        )
    &
        Y = X
    ).

:- pred conj(int::out) is det.
:- pragma no_inline(conj/1).

conj(Y) :-
    (
        W = 3,
        % signal W
        B = 5
        % signal B
    &
        % wait W
        % wait C
        Y = W + C
    &
        % wait B
        C = B
        % signal C
    ).

:- pred par_conj(int::out) is det.
:- pragma no_inline(par_conj/1).

par_conj(Y) :-
    Y = 1.
    % XXX think of something

:- pred scope(int::out) is det.
:- pragma no_inline(scope/1).

scope(Y) :-
    (
        promise_equivalent_solutions [X]
	( X = one
            % signal X
        ; X = 1
            % signal X
        )
    &
	Y = X
    ).

:- pred do_call(int::out) is det.
:- pragma no_inline(do_call/1).

do_call(Y) :-
    (
        X = [1,2,3]
    &
        mylength(X, Z)
        % should produce parallel version
        %   Parallel__mylength(FutureX, FutureZ)
        %   get(FutureZ, Z)
    &
        % wait Z
        Y = Z
    ).

:- pred mylength(list(_T), int).
:- mode mylength(in, out) is det.
:- pragma no_inline(mylength/2).

mylength(L, N) :-
    mylength_2(L, 0, N).

:- pred mylength_2(list(T), int, int).
:- mode mylength_2(in, in, out) is det.

mylength_2([], N, N).
mylength_2([_ | L1], N0, N) :-
    N1 = N0 + 1,
    mylength_2(L1, N1, N).

