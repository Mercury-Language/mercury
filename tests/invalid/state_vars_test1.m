%---------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module state_vars_test1.

:- interface.

:- pred p(int::in, int::out) is det.

:- implementation.

:- import_module int.

    % Illegally refers to !:X in an if-then-else expr.
    %
p(!X) :-
    !:X = !.X +
        ( if max(0, !.X, !:X) then
            1
        else
            2
        ).
