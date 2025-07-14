%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mode_without_pred.
:- interface.

:- type t
    --->    f(
                f1 :: int,
                f2 :: int
            ).

:- implementation.

% :- func f1(t) = int.
:- mode f1(in) = out is det.

:- func 'f1 :='(t, int) = t.
:- mode 'f1 :='(in, in) = out is det.
