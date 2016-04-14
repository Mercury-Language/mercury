%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Uses combined higher-order types and insts in places where
% they are not currently permitted.
%
%---------------------------------------------------------------------------%

:- module combined_ho_type_inst.
:- interface.

:- import_module list.

:- type bad_eqv_p == (pred(int::in, int::out) is det).
:- type bad_eqv_f == (func(int::in) = (int::out) is semidet).

:- type bad_nested_p
    --->    bad_nested_p(list(pred(int::in, int::out) is det)).
:- type bad_nested_f
    --->    bad_nested_p(list(func(int::in) = (int::out) is semidet)).

:- type bad_arg_p
    --->    bad_arg_p(pred((pred(int::in, int::out) is det))).
:- type bad_arg_p2
    --->    bad_arg_p2(pred((pred(int::in, int::out) is det)::in) is semidet).
:- type bad_arg_f
    --->    bad_arg_f(func((func(int::in) = (int::out) is semidet)) = int).
:- type bad_arg_f2
    --->    bad_arg_f2(
                func((func(int::in) = (int::out) is semidet)::in)
                    = (int::out) is semidet
            ).

:- pred bad_sig_p((pred(int::in, int::out) is semiet)::in) is semidet.
:- func bad_sig_f(func(int::in) = (int::out) is semidet) = int.

:- type avoid_spurious_warning == list(int).

