%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module vars_in_wrong_places.
Oops1 :- blah.
Oops2 = 42 :- blah.
Oops3 = 42.
:- pred Oops4.
:- func Oops5 = int.
:- mode Oops6.
:- mode Oops7 = int.
