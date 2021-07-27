%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module semidet_disj.

:- interface.

:- pred p(int::in) is semidet.

:- implementation.

:- pred q(int::in) is semidet.
:- pragma external_pred(q/1).
:- pred r(int::in) is semidet.
:- pragma external_pred(r/1).

p(X) :-
    ( q(X)
    ; r(X)
    ).

:- pragma foreign_code("Java", "
   private static boolean q_1_p_0(int x) { return false; }
   private static boolean r_1_p_0(int x) { return false; }
").
