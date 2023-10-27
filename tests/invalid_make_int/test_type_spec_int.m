%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_type_spec_int.

:- interface.

:- import_module list.
:- import_module map.

:- pred type_spec1(list(T)::in) is semidet.
:- pragma external_pred(type_spec1/1).
:- pragma type_spec(pred(bad_module_name.type_spec1/1), U = float).

:- pred type_spec2(map(T, U)::in) is semidet.
:- pragma external_pred(type_spec2/1).
