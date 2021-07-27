%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Uses combined higher-order types and insts with incomplete syntax.
%
%---------------------------------------------------------------------------%

:- module combined_ho_type_inst_2.
:- interface.

:- import_module list.

:- type missing_modes_p
    --->    missing_modes_p(pred(int) is semidet).

:- type missing_modes_p2
    --->    missing_modes_p2(pred(int::in, int) is det).

:- type missing_modes_f
    --->    missing_modes_f(func(int) = (int::out) is det).

:- type missing_modes_f2
    --->    missing_modes_f2(func(int) = int is det).

:- type missing_detism_p
    --->    missing_detism_p(pred(int::in)).

:- type missing_detism_f
    --->    missing_detism_f(func(int::in) = (int::out)).

:- type avoid_spurious_warning == list(int).

