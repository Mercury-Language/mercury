%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module error.
:- implementation.
:- import_module require.

:- pred t(int::out) is det.

t(_X) :-
    error("").
