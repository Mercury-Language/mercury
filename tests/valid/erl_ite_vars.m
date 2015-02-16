%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a bug in the generation of if-then-else goals
% in the Erlang backend.  The calculation of the variables bound by the then
% branch was wrong -- it included variables bound in the condition.

:- module erl_ite_vars.
:- interface.

:- import_module list.

:- type baz
    --->    and(baz, baz)
    ;       bar(string)
    ;       quux(string).

:- pred foo(list(string)::in, list(string)::out, baz::out) is semidet.

:- implementation.

foo(List, Rest, Expression):-
    ( List = ["(", "(" | R] ->
        foo(["(" | R], Rem, Exp)
    ;
        List = ["(", "bar", Name | _],
        Rem = [],
        Exp = bar(Name)
    ;
        List = ["(", "quux", Name, ")" | Rem],
        Exp = quux(Name)
    ),
    ( Rem = [", " | Next] ->
        foo(Next, Rest, RestExp),
        Expression = and(Exp, RestExp)
    ;
        Rest = [],
        Expression = Exp
    ).
