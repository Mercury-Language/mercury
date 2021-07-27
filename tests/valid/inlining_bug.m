%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for an abort in code generation.
% When the actual type for a polymorphic argument is non-polymorphic,
% only the base_type_info is passed, avoiding the construction of
% a type_info. The problem was that the type of the type_info argument
% was being set to `mercury_builtin:base_type_info' rather than
% `type_info'. In the code to compute the type substitution in inlining,
% type_list_subsumes failed on the argument types, and no substitution
% was produced. code_util.cons_id_to_tag then aborted when asked to
% find the tag for a constructor of a variable type.

:- module inlining_bug.

:- interface.

:- pred calling_pred(int::in) is semidet.

:- implementation.

:- import_module int.
:- import_module list.

:- type my_pair(A)
    --->    pair(A, A).

calling_pred(_) :-
    Plus1 =
        ( pred(Int0::in, IntPair::out) is det :-
            IntPair = pair(Int0, Int0)
        ),
    called_pred(Plus1, [1, 2, 3], [X | Ys]),
    X = pair(2, 2),
    Ys = [pair(3, 3), pair(4, 4)].

:- pred called_pred(pred(T, U), list(T), list(U)).
:- mode called_pred(pred(in, out) is det, in, out) is semidet.
:- pragma inline(called_pred/3).

called_pred(P, [A | As], [B | Bs]) :-
    call(P, A, B),
    list.map(P, As, Bs).
