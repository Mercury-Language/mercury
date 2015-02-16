%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module error_in_list.

:- interface.

:- import_module list.

:- func f = list(int).

:- implementation.

f = [1, 2, 3, 4, "five", 6, 7.0, 8].
