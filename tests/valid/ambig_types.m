%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ambig_types.

:- interface.

:- type a ---> foo ; bar.
:- type b ---> bar ; baz.
:- type c ---> baz ; foo.

:- implementation.

:- end_module ambig_types.
