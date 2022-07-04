%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module stack_alloc.
:- interface.

:- type nat
    --->    zero
    ;       s(nat).

:- pred p(nat::in) is semidet.

:- implementation.

:- pred in(int::in) is semidet.
:- pred out(int::out) is det.

:- pragma external_pred(in/1).
:- pragma external_pred(out/1).

p(s(N)) :-
    (
        out(X),
        out(Y),
        p(N),
        in(X),
        in(Y)
    ;
        out(A),
        out(B),
        p(N),
        in(A),
        in(B)
    ).

:- pragma foreign_code("Java", "
    private static int out_1_p_0() { return 0; }
    private static boolean in_1_p_0(int n) { return false; }
").
