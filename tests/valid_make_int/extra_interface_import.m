%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module extra_interface_import.
:- interface.

:- import_module list.
:- import_module map.       % This import is not needed.

:- pred sum_list(list(int)::in, int::out) is det.

:- implementation.

:- import_module int.

sum_list([], 0).
sum_list([X | Xs], Sum) :-
    sum_list(Xs, XsSum),
    Sum = X + XsSum.
