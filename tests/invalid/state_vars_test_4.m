%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module state_vars_test_4.

:- interface.

:- import_module list.

:- func f(list(int)) = int.

:- implementation.
:- import_module int.

    % Illegally uses !X as a lambda arg.
    %
f(Xs) = foldl(func(!X) = !.X * !:X, Xs, 1).
