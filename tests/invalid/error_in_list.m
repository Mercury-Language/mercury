:- module error_in_list.

:- interface.

:- import_module list.

:- func f = list(int).

:- implementation.

f = [1, 2, 3, 4, "five", 6, 7.0, 8].
