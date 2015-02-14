%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order_syntax2.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int.

:- func curry((func(V_3, V_2) = V_1)) = ((func V_3) = ((func V_2) = V_1)).
curry(F) = (func(X) = (func(Y) = F(X, Y))).

:- func uncurry(((func V_3) = ((func V_2) = T4)), V_3, V_2) = T4.
uncurry(F, X, Y) = F(X)(Y).

:- func id((func(V_3, V_2) = T4)) = (func(V_3, V_2) = T4).
id(F) = uncurry(curry(F)).

:- func pplus(int, int) = int.
pplus(X, Y) = X + Y.

main -->
    { Result = (id)(pplus)(3, 4) },
    print("Result = "), print(Result), nl,
    { Result2 = (func(X, Y) = X + Y)(5, 6) },
    print("Result2 = "), print(Result2), nl.

