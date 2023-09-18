%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module should be compiled with `--no-user-guided-type-specialization'
% to check that the declared specializations are still created, to avoid
% link errors if importing modules are compiled with type specialization.

:- module type_spec_helper_1.

:- interface.

:- import_module list.

:- pred no_type_spec(T::in, T::in) is semidet.
:- pragma type_spec(no_type_spec/2, T = list(U)).

:- implementation.

no_type_spec(X, X).
