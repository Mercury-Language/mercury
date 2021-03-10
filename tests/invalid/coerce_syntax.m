%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_syntax.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

%---------------------------------------------------------------------------%

:- implementation.

:- pred module_qualify_coerce is det.

module_qualify_coerce :-
    _ = coerce_syntax.coerce(1).

:- pred apply_coerce is det.

apply_coerce :-
    _ = apply(coerce, 1).

:- pred empty_apply_coerce is det.

empty_apply_coerce :-
    _ = (coerce)(1).

:- pred coerce_in_head_var(citrus::in, fruit::out) is det.

coerce_in_head_var(X, coerce(X)).

:- pred coerce_in_head_functor(citrus::in, fruit::out) is det.

coerce_in_head_functor(_, coerce(orange : citrus)).

:- func coerce_in_function_arg(citrus) = fruit is semidet.

coerce_in_function_arg(coerce(apple)) = orange.

:- func coerce_in_function_result(citrus) = fruit.

coerce_in_function_result(X) = coerce(X).
