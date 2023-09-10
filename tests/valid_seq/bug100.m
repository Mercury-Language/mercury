%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-2010-01-25 and before were incorrectly reporting that the import
% of the module bug100_2 in the interface was unused.
%

:- module bug100.
:- interface.

:- import_module bug100_helper_2.
:- import_module unit.

    % We need this import to get that tc2(unit) superclass constraint
    % is satisfied.
    %
:- import_module bug100_helper_1.

:- typeclass tc(T) <= tc2(T) where [].

:- instance tc(unit).

:- implementation.

:- instance tc(unit) where [].
