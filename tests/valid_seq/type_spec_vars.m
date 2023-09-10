%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_spec_vars.

:- interface.

:- import_module list.

:- pred call_p(list(int)::in, list(list(int))::in) is semidet.

:- implementation.

:- import_module type_spec_vars_helper_1.

call_p(A, B) :-
    p(A, B).
