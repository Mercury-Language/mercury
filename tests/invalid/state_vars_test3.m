%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module state_vars_test3.

:- interface.

:- import_module int.

:- func f(int) = int.

:- implementation.

    % Illegally uses !Y as a func result.
    %
f(!X) = !Y.
