%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_typecheck_eqv.
:- interface.

:- type fruit
    --->    apple
    ;       lemon.

:- type citrus =< fruit
    --->    lemon.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module coerce_typecheck_eqv_helper_1.

:- type good(T)
    --->    good(first(int, T)).    % == second(T, int) == list(int)

:- type bad(T)
    --->    bad(first(T, int)).     % == second(int, T) == list(T)

:- pred test1(good(citrus)::in, good(fruit)::out) is det.

test1(X, Y) :-
    Y = coerce(X).

:- pred test2(bad(citrus)::in, bad(fruit)::out) is det.

test2(X, Y) :-
    % The type parameter T in bad(T), after expanding type equivalences,
    % appears in the discriminated union type list(T), so it must be invariant.
    % This coerce expression cannot be type-correct as T would be bound to
    % 'citrus' in the from-type, but 'fruit' in the to-type.
    Y = coerce(X).

%---------------------------------------------------------------------------%
