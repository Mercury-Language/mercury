%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module double_vn.

:- interface.

:- pred p(int::out) is det.

:- implementation.

:- import_module int.

p(X) :-
    X = 1 \/ 1 \/ 1.
