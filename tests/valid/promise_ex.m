%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module promise_ex.
:- interface.

    % Just to shut up the warning about the module exporting nothing.
    %
:- pred p(int::in, int::out) is det.

:- implementation.

:- import_module int.

p(N, N).

:- all [X, Y] promise_exclusive_exhaustive (
    (
        X < Y
    ;
        X = Y
    ;
        X > Y
    )
).
