%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module exported_mode.
:- interface.

:- mode p(in, in) is semidet.

:- implementation.

p(_, "foo").
