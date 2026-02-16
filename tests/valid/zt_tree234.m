%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This module implements maps of zt values using 2-3-4 trees.
% It is based on the tree234.m module from the Mercury standard library.
%
% NOTE: keys are zt/0 values, but they must be par zt/0 values.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module zt_tree234.
:- interface.

%-----------------------------------------------------------------------------%

:- type zt_tree234.

:- type zt ---> zt.
:- type ps ---> ps.

:- type zt_key == zt.
:- type zt_value == zt.

%----------------------%

:- pred zt_tree234.foldl2_values(pred(zt_value, A, A, B, B), zt_tree234,
    A, A, B, B).
:- mode zt_tree234.foldl2_values(pred(in, in, out, in, out) is det, in,
    in, out, in, out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module list.

:- type zt_tree234
    --->    empty
    ;       two(zt_key, zt_value, zt_tree234, zt_tree234)
    ;       three(zt_key, zt_value, zt_key, zt_value,
                zt_tree234, zt_tree234, zt_tree234)
    ;       four(zt_key, zt_value, zt_key, zt_value, zt_key, zt_value,
                zt_tree234, zt_tree234, zt_tree234, zt_tree234).

%------------------------------------------------------------------------------%

% Zinc generated comprehensions will use these type variable bindings so we
% specialise this case.
:- pragma type_spec(pred(foldl2_values/6), (A = zt_tree234, B = ps)).

foldl2_values(_Pred, empty, !A, !B).
foldl2_values(Pred, two(_K, V, T0, T1), !A, !B) :-
    foldl2_values(Pred, T0, !A, !B),
    Pred(V, !A, !B),
    foldl2_values(Pred, T1, !A, !B).
foldl2_values(Pred, three(_K0, V0, _K1, V1, T0, T1, T2), !A, !B) :-
    foldl2_values(Pred, T0, !A, !B),
    Pred(V0, !A, !B),
    foldl2_values(Pred, T1, !A, !B),
    Pred(V1, !A, !B),
    foldl2_values(Pred, T2, !A, !B).
foldl2_values(Pred, four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3), !A, !B) :-
    foldl2_values(Pred, T0, !A, !B),
    Pred(V0, !A, !B),
    foldl2_values(Pred, T1, !A, !B),
    Pred(V1, !A, !B),
    foldl2_values(Pred, T2, !A, !B),
    Pred(V2, !A, !B),
    foldl2_values(Pred, T3, !A, !B).

%-----------------------------------------------------------------------------%
:- end_module zt_tree234.
%-----------------------------------------------------------------------------%
