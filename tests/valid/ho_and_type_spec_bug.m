%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Compiling the following program at -O3 causes the following
% assertion failure in the compiler:
%
%    Uncaught Mercury exception:
%    Software Error: map__lookup: key not found
%       Key Type: term.var(parse_tree.prog_data.prog_var_type)
%       Key Value: var(20)
%       Value Type: ll_backend.var_locn.var_state
%
:- module ho_and_type_spec_bug.

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

beta(_E, V, _R0, R) :-
    R = gamma(V).

:- func gamma(int) = robdd(T).
:- pragma no_inline(gamma/1).
:- pragma foreign_proc("C",
    gamma(V::in) = (F::out),
    [will_not_call_mercury, promise_pure],
"
    /* V F */
").
:- pragma foreign_proc("C#",
    gamma(V::in) = (F::out),
    [will_not_call_mercury, promise_pure],
"
    /* V */
    F = null;
").
:- pragma foreign_proc("Java",
    gamma(V::in) = (F::out),
    [will_not_call_mercury, promise_pure],
"
    /* V */
    F = null;
").

:- pred foldl(pred(T, U, U), sparse_bitset(T), U, U) <= foo(T).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.

foldl(_, [], Acc, Acc).
foldl(P, [ OffSet | T ], Acc0, Acc) :-
    fold_bits(P, OffSet, Acc0, Acc1),
    foldl(P, T, Acc1, Acc).

:- pred fold_bits(pred(T, U, U), int, U, U) <= foo(T).
:- mode fold_bits(pred(in, in, out) is det, in, in, out) is det.

fold_bits(P, Offset, Acc0, Acc) :-
    Elem = from_int(Offset),
    P(Elem, Acc0, Acc).
