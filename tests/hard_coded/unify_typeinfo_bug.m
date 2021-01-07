%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unify_typeinfo_bug.
:- interface.

:- import_module io.
:- import_module list.

:- pred main(io::di, io::uo) is det.

:- pred unify_bug(list(T)::in, list(T)::in) is semidet.

:- pred exist_unify_bug(list(T)::in, list(T)::in) is semidet.

:- type set_bbbtree(T).

:- pred singleton_set(set_bbbtree(T), T).
:- mode singleton_set(in, in) is semidet.

:- implementation.

main(!IO) :-
    ( if unify_bug([1], [1]) then
        io.write_string("Succeeded\n", !IO)
    else
        io.write_string("Failed\n", !IO)
    ),
    ( if exist_unify_bug([1], [1]) then
        io.write_string("Succeeded\n", !IO)
    else
        io.write_string("Failed\n", !IO)
    ),
    ( if exist_unify_bug([1], [1]) then
        io.write_string("Succeeded\n", !IO)
    else
        io.write_string("Failed\n", !IO)
    ),
    ( if  singleton_set(tree([1], 1, empty, empty), [1]) then
        io.write_string("Succeeded\n", !IO)
    else
        io.write_string("Failed\n", !IO)
    ).

unify_bug(A, B) :-
    A = [H | _],
    B = [H | _].

exist_unify_bug(A, B) :-
    C = D,
    exist_id(A, B, C, D).

:- some [U] pred exist_id(T::in, T::in, U::out, U::out) is det.

exist_id(A, B, A, B).

:- type set_bbbtree(T)
    --->    empty
    ;       tree(T, int, set_bbbtree(T), set_bbbtree(T)).

singleton_set(tree(V, 1, empty, empty), V).
