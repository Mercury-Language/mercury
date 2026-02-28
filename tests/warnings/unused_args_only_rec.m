%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
%  Predicates to check that unused_args.m is producing the correct warning.
%
%---------------------------------------------------------------------------%

:- module unused_args_only_rec.
:- interface.

:- pred p(int::in, int::in, int::in, int::in, int::out) is det.

:- pred q(int::in, int::in, int::in, int::in, int::out) is det.

:- implementation.

:- import_module int.

p(A, B, Cur, Least, Out) :-
    ( if Cur < Least then
        Next = Cur * Cur,
        q(B, A, Next, Least, Out)
    else
        Out = Cur
    ).

q(A, B, Cur, Least, Out) :-
    ( if Cur < Least then
        Next = Cur * Cur,
        p(A, B, Next, Least, Out)
    else
        Out = Cur
    ).
