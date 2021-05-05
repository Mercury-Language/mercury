%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_recursive_inst.
:- interface.

:- type rec(T)
    --->    nil
    ;       cons(T, rec(T)).

:- inst rec for rec/1
    --->    nil
    ;       cons(ground, rec).

:- inst cons for rec/1
    --->    cons(ground, rec).

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

%---------------------------------------------------------------------------%

:- implementation.

    % convert subtype to supertype
:- func good(rec(citrus)) = rec(fruit).
:- mode good(in(cons)) = out(cons) is det.

good(X) = coerce(X).

    % cannot convert supertype to subtype
:- func bad1(rec(fruit)) = rec(citrus).
:- mode bad1(in(cons)) = out(cons) is det.

bad1(X) = coerce(X).

    % cannot convert supertype to subtype
:- func bad2(rec(fruit)) = rec(citrus).
:- mode bad2(in(cons)) = out is det.

bad2(X) = coerce(X).

%---------------------------------------------------------------------------%
