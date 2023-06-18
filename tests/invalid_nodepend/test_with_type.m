%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_with_type.

:- interface.

:- import_module list.

:- type map_pred(T, U) == pred(T, U).
:- inst map_pred == (pred(in, out) is det).
:- type foldl_pred(T, U) == pred(T, U, U).
:- inst foldl_pred == (pred(in, in, out) is det).

:- pred with_type_1(T, list(_)) `with_type` int.
:- mode with_type_1(in, in) `with_inst` ground.

:- type map_func(T, U) == (func(T) = U).
:- inst map_func == (func(in) = out is det).

:- func with_type_2(T, list(_)) `with_type` map_pred(string, string).
:- mode with_type_2(in, in) `with_inst` map_func.

:- pred with_type_3(T::in, list(_)::in) `with_type` map_pred(string, string)
    `with_inst` foldl_pred.

:- pred with_type_4(T::in, list(_)::in) `with_type` map_pred(string, string)
    `with_inst` foldl_pred is det.

:- pred with_type_5(T::in, list(_)::in) `with_inst` foldl_pred.

:- pred with_type_6(T::in, list(_)::in) `with_type` map_pred(string, string)
    `with_inst` (pred(in, in, out) is foo).

:- pred with_type_7(T, list(_)) `with_type` map_pred(string, string)
    `with_inst` foldl_pred.
