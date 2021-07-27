%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The compiler would abort during liveness detection on an if-then-else goal
% with an unreachable Then branch, due to the Then goal not having its
% liveness-related fields initialised.

:- module bug51.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type fruit
    --->    apple
    ;       banana.

main(!IO) :-
    my_map(make, [apple], Xs),
    do(Xs, !IO).

:- pred my_map(pred(L, M), list(L), list(M)).
:- mode my_map(pred(in, out) is det, in, out) is det.

my_map(_, [], []).
my_map(P, [H0 | T0], [H | T]) :-
    P(H0, H),
    my_map(P, T0, T).

:- pred make(fruit::in, fruit::out) is det.

make(X, X).

:- pred do(list(fruit)::in, io::di, io::uo) is det.

do([], !IO).
do([X | Xs], !IO) :-
    ( if Xs = [_ | _] then
        S = "1"
    else
        S = "2"
    ),
    (
        X = apple
    ;
        X = banana,
        io.write_string(S, !IO)
    ).
