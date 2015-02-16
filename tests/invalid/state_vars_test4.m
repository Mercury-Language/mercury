%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module state_vars_test4.

:- interface.

:- import_module int.
:- import_module list.

:- func f(list(int)) = int.

:- implementation.

    % Illegally uses !X as a lambda arg.
    %
f(Xs) = foldl(func(!X) = !.X * !:X, Xs, 1).
