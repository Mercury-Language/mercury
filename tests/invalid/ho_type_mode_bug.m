%
% ho_type_mode_bug.m:
%
% This incorrect program produces an unexpected runtime error:
%
% Software error: propagate_types_into_mode_list: length mismatch
%
% It seems it's not expecting people to have a different number of types
% to modes in declarations involving higher-order preds.

:- module ho_type_mode_bug.

:- interface.

:- import_module list.

:- pred my_foldl2(pred(X, Y, Y, Z, Z), list(X), Y, Y, Z, Z).
:- mode my_foldl2(pred(in, in, out) is det, in, in, out, in, out) is det.
:- mode my_foldl2(pred(in, in, out) is det, in, in, out, di, uo) is det.

:- implementation.

my_foldl2(_, [], FirstAcc, FirstAcc, SecAcc, SecAcc).
my_foldl2(P, [H|T], FirstAcc0, FirstAcc, SecAcc0, SecAcc) :-
	call(P, H, FirstAcc0, FirstAcc1, SecAcc0, SecAcc1),
	my_foldl2(P, T, FirstAcc1, FirstAcc, SecAcc1, SecAcc).
