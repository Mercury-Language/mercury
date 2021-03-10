%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_infer.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- func g(citrus) = fruit.

:- implementation.

g(X) = f(X).

% We cannot infer this yet:
%:- func f(citrus) = fruit.

f(X) = coerce(X).
