%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Author: Ralph Becket <rafe@cs.mu.oz.au>
%
% This is a regression test for a bug involving the interaction of the mode
% system and quantification, triggered by the state variable transformation.
%
% This version expands out the state variable transformation.
%
%---------------------------------------------------------------------------%

:- module state_var_mode_bug2.

:- interface.

:- import_module bool.
:- import_module int.

:- pred p(bool::in, bool::in, int::in, int::out) is semidet.

:- implementation.

p(A, B, !Y) :-
    some [X0, _X] (
        p0(X0),
        p2(X0, X1),
        p2(X1, X2),
        ( if
            A = yes
        then
            (
                B = yes,
                p1(X2),
                p2(X2, X3),
                p1(X3),
                X4 = X3
            ;
                B = no,
                ( if
                    p1(X2),
                    p2(X2, X3)
                then
                    p1(X3),
                    X4 = X3
                else
                    X4 = X2
                )
            ),
            X5 = X4
        else
            ( if
                p1(X2),
                p2(X2, X3)
            then
                (
                    A = yes,
                    p1(X3),
                    p2(X3, X4),
                    (
                        B = yes,
                        p2(X4, X5)
                    ;
                        B = no,
                        X5 = X4
                    )
                ;
                    A = no,
                    X5 = X3
                )
            else if
                p1(X2)
            then
                p1(X2),
                p2(X2, X3),
                p1(X3),
                X5 = X3
            else
                X5 = X2
            )
        ),
        _X = X5
    ).

:- pred p0(int::out) is det.

p0(0).

:- pred p1(int::in) is semidet.

p1(1).

:- pred p2(int::in, int::out) is semidet.

p2(X0, X0) :-
    p1(X0).
