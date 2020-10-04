%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% rotd-2006-06-30 and before incorrectly hoisted the method call
% new_literal/1 in the function literal_list/1. The problem was that
% loop invariant hoisting was considering calls (and generic_calls) with
% modes that contained an inst `any' component as candidates for hoisting.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module any_call_hoist_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.

%---------------------------------------------------------------------------%

:- typeclass foo(L) where [
    pred new_literal(L::oa) is det
].

:- instance foo(literal) where [
    new_literal(A) :- make_new_literal(A)
].

:- type lit_list(L) == list(pair(int, L)).

main(!IO) :-
    LitList0 = literal_list(5) : lit_list(literal),
    LitList  = cast_to_ground(LitList0),
    io.write(LitList, !IO).

:- func literal_list(int::in) = (lit_list(L)::oa) is det <= foo(L).

literal_list(N) = LitList :-
    ( if N =< 0 then
        LitList = []
    else
        new_literal(A),   % XXX This is incorrectly hoisted.
        LitList0 = literal_list(N - 1),
        LitList  = [ N - A | LitList0 ]
    ).

:- mutable(literal_supply, int, 561, ground, [untrailed]).

:- solver type literal
    where   representation is int.

:- pred make_new_literal(literal::oa) is det.

make_new_literal(NewLiteral) :-
    promise_pure (
        semipure get_literal_supply(NextLiteral),
        impure   set_literal_supply(NextLiteral + 1),
        impure   NewLiteral = 'representation to any literal/0'(NextLiteral)
    ).

:- func cast_to_ground(T::ia) = (T::out) is det.
:- pragma foreign_proc("C",
    cast_to_ground(A::ia) = (B::out),
    [promise_pure, will_not_call_mercury],
"
    B = A;
").
:- pragma foreign_proc("Erlang",
    cast_to_ground(A::ia) = (B::out),
    [promise_pure, will_not_call_mercury],
"
    B = A
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
