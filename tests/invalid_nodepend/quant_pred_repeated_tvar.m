% Check that we print an error for repeated variables in quantifiers on
% predicates and functions.

:- module quant_pred_repeated_tvar.
:- interface.

:- type nowarn
    --->    nowarn.

:- all [T, T] pred foo_1(T::in) is semidet.

:- all [T, U, U, V, T] pred foo_2(T::in, U::in, V::in) is semidet.

:- all [T, T, U, U, V, V, W] pred foo_3(T::in, U::in, V::in, W::in)
    is semidet.

:- some [T, T] pred foo_4(T::out) is det.

:- some [T, T, U, U, V] pred foo_5(T::out, U::out, V::out) is det.

:- all [T, T] func bar_1(T) = T.

:- all [T, T, U, U, V] func bar_2(T, U, V) = T.

:- some [T, T] func bar_3(T) = T.

:- some [T, U, U, V, T] func bar_4(T) = T.

:- all [T, T] some [U, U] pred baaz_1(T::in, U::out) is semidet.
