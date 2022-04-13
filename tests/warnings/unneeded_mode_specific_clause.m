%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unneeded_mode_specific_clause.
:- interface.

:- pred test_x(int::in, int::out) is det.

:- implementation.

:- import_module int.

test_x(N::in, M::out) :-
    M = N + 1.
