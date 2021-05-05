%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_recursive_type.
:- interface.

:- type rec(T)
    --->    nil
    ;       cons(T, rec(T)).

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- inst apple
    --->    apple.

:- inst cons_apple for rec/1
    --->    cons(apple, ground).

:- inst cons_apple2 for rec/1
    --->    cons(ground, cons_apple).

:- inst cons_apple3 for rec/1
    --->    cons(ground, cons_apple2).

%---------------------------------------------------------------------------%

:- implementation.

    % convert subtype to supertype
:- func good(rec(citrus)) = rec(fruit).

good(X) = coerce(X).

    % cannot convert supertype to subtype
:- func bad(rec(fruit)) = rec(citrus).

bad(X) = coerce(X).

    % cannot convert supertype to subtype
:- func bad2(rec(fruit)) = rec(citrus).
:- mode bad2(in(cons_apple3)) = out is det.

bad2(X) = coerce(X).

    % convert subtype to supertype
    % But, the bound inst is wrong for the input type.
:- func bad3(rec(citrus)) = rec(fruit).
:- mode bad3(in(cons_apple3)) = out is det.

bad3(X) = coerce(X).
