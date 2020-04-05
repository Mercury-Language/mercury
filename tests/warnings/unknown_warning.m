%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module unknown_warning.
:- interface.

:- pred unknown_warning(int::in, int::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

unknown_warning(A, B, X) :-
    disable_warning [singleton_varx]
    (
        C = A + B,
        D = A - B,  % D is a singleton
        X = C
    ).
