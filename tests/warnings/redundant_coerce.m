%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module redundant_coerce.
:- interface.

:- pred test1 is det.
:- pred test2 is det.
:- pred test3 is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.

:- type maybe_box(T)
    --->    box(T)
    ;       no_box.

:- type box(T) =< maybe_box(T)
    --->    box(T).

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

test1 :-
    coerce(yes(1)) = _ : maybe(T).

test2 :-
    X = box(orange) : box(fruit),
    coerce(X) = _ : box(fruit),
    coerce(X) = _ : box(citrus),
    coerce(X) = _ : maybe_box(fruit),
    coerce(X) = _ : maybe_box(citrus).

test3 :-
    X = box(orange : citrus) : maybe_box(T),
    coerce(X) = _ : box(fruit),
    coerce(X) = _ : box(citrus),
    coerce(X) = _ : maybe_box(fruit),
    coerce(X) = _ : maybe_box(citrus).
