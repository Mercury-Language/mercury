%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% ho_type_mode_bug.m:
%
% This incorrect program once caused a compiler crash with
%
% Software error: propagate_types_into_mode_list: length mismatch
%
% The compiler was not expecting the number of argument types and the number
% of argument modes to differ in declarations involving higher-order preds.

:- module ho_type_mode_bug.

:- interface.

:- import_module list.

:- pred my_foldl2(pred(X, Y, Y, Z, Z), list(X), Y, Y, Z, Z).
:- mode my_foldl2(pred(in, in, out) is det, in, in, out, in, out) is det.
:- mode my_foldl2(pred(in, in, out) is det, in, in, out, di, uo) is det.

:- implementation.

my_foldl2(_, [], FirstAcc, FirstAcc, SecAcc, SecAcc).
my_foldl2(P, [H | T], FirstAcc0, FirstAcc, SecAcc0, SecAcc) :-
    call(P, H, FirstAcc0, FirstAcc1, SecAcc0, SecAcc1),
    my_foldl2(P, T, FirstAcc1, FirstAcc, SecAcc1, SecAcc).
