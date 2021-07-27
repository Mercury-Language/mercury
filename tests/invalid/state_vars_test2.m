%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module state_vars_test2.

:- interface.

:- pred p(int::in, int::out) is det.

:- implementation.

:- import_module int.

    % Illegally refers to !:Y, which has not been explicitly introduced.
    %
p(!X) :-
    !:X = !.X +
        ( if max(0, !.X, !:Y) then
            1
        else
            2
        ).
