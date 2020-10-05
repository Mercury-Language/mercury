%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order_syntax2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

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

main(!IO) :-
    Result = (id)(pplus)(3, 4),
    Result2 = (func(X, Y) = X + Y)(5, 6),
    print("Result = ", !IO),  print(Result, !IO),  nl(!IO),
    print("Result2 = ", !IO), print(Result2, !IO), nl(!IO).

