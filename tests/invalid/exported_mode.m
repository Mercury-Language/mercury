:- module exported_mode.
:- interface.

:- mode p(in, in) is semidet.

:- implementation.

p(_, "foo").
