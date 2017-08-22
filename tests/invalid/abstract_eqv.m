%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Mantis bug 411. abstract_eqv.m should not be able to see that
% abstract_eqv2.m has *privately* defined foo to be equivalent to bar.

:- module abstract_eqv.

:- interface.

:- pred bad is det.

:- implementation.

:- import_module abstract_eqv2.

bad :-
    call_with_foo(bar(1)).
