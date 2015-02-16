%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Compiling the following program with -O3 --no-inlining causes the following
% assertion failure in the compiler:
%
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%   Key Type: term.var(parse_tree.prog_data.prog_var_type)
%   Key Value: var(16)
%   Value Type: ll_backend.var_locn.var_state

:- module ho_and_type_spec_bug2.
:- interface.

:- type sparse_bitset(T)
    --->    []
    ;       [int | sparse_bitset(T)].

:- type robdd(T)
    --->    robdd(int).

:- type unit
    --->    unit.

:- pragma type_spec(alpha/4, T = unit).
:- pred alpha(sparse_bitset(int)::in, robdd(T)::in,
    robdd(T)::in, robdd(T)::out) is det.

:- implementation.

:- typeclass foo(T) where [
    func from_int(int) = T
].

:- instance foo(int) where [
    from_int(X) = X
].

alpha(T, E, R0, R) :-
    Closure = beta(E),
    foldl(Closure, T, R0, R).

:- pred beta(robdd(T)::in, int::in, robdd(T)::in, robdd(T)::out) is det.

beta(_E, _V, _R0, R) :-
    R = robdd(3).

:- pred foldl(pred(T, U, U), sparse_bitset(T), U, U) <= foo(T).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.

foldl(P, _, Acc0, Acc) :-
    fold_bits(P, Acc0, Acc).

:- pred fold_bits(pred(T, U, U), U, U) <= foo(T).
:- mode fold_bits(pred(in, in, out) is det, in, out) is det.

fold_bits(P, Acc0, Acc) :-
    Elem = from_int(30),
    P(Elem, Acc0, Acc).
