%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_exported_mode_helper_1.
:- interface.

:- mode p(in, in) is semidet.

:- implementation.

p(_, "foo").
