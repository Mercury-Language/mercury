%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% With --warn-unused-imports, Versions of the compiler before 2019 sep 2
% used to emit a warning about the list module being imported but not used,
% even though it *is* used in the type constructor for the "empty" inst.
% To test whether we emit this warning, this program is compiled with
% --halt-at-warn.
%

:- module bug483.
:- interface.

:- import_module list.

:- inst empty for list/1
    --->    [].
