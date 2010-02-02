% rotd-2010-01-25 and before were incorrectly reporting that the import
% of the module bug100_2 in the interface was unused.
%
:- module bug100.
:- interface.

:- import_module unit.
:- import_module bug100_3.

    % We need this import to get that tc2(unit)
    % superclasss constraint is satisfied.
    %
:- import_module bug100_2.

:- typeclass tc(T) <= tc2(T) where [].

:- instance tc(unit).

:- implementation.

:- instance tc(unit) where [].
