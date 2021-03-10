%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_non_du.
:- interface.

:- type dummy_export
    --->    dummy_export.

:- implementation.

:- func f1(int) = int.

f1(X) = coerce(X).

:- func f2(float) = float.

f2(X) = coerce(X).

:- func f3({}) = {}.

f3(X) = coerce(X).

:- func f4(func(int) = int) = (func(int) = int).

f4(X) = coerce(X).

:- func f5(pred(int, int)) = (pred(int, int)).

f5(X) = coerce(X).
