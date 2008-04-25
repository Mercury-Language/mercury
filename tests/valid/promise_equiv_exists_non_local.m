% vim: ft=mercury ts=4 sw=4 et
%
% rotd-rotd-2008-04-25 reported the following error for this program:
%
% Error: the `promise_equivalent_solutions'
%   goal binds a variable that is not
%   listed: TypeInfo_for_S.
%
% The problem was that the check that was generating this message
% should not have been considering introduced type-info related
% variables.
%
:- module promise_equiv_exists_non_local.
:- interface.

:- import_module list.

:- some [S] pred test(list(int)::in, S::out) is semidet.

:- implementation.

test(I, S) :-
    promise_equivalent_solutions [S] (
        make_foo(I, Foo),
        Foo = foo(S)
    ).

:- type foo ---> some [S] foo(S).

:- pred make_foo(list(int)::in, foo::out) is nondet.

make_foo(Xs, Foo) :-
    list.member(X, Xs),
    make_foo_2(X, Foo).

:- pred make_foo_2(int::in, foo::out) is det.

make_foo_2(X, 'new foo'(X)).

