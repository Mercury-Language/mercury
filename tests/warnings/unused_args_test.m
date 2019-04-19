%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
%  Predicates to check that unused_args.m is producing the correct warning.
%
%---------------------------------------------------------------------------%

:- module unused_args_test.
:- interface.

:- type switch_test.
:- pred recursive(int::in, switch_test::in, int::in) is semidet.

:- pred nonrecursive(int::in) is semidet.

:- implementation.

:- import_module char.
:- import_module int.

:- type switch_test
    --->    f1(int)
    ;       f2(char)
    ;       f3.

recursive(Useless, Used1, Used2) :-
    (
        Used1 = f1(Used2)
    ;
        Used1 = f2(_)
    ),
    nonrecursive(Useless),
    Used3 = Used2 + 1,
    recursive(Useless, Used1, Used3).

nonrecursive(Useless) :-
    (
        recursive(Useless, f2('a'), 2)
    ;
        recursive(Useless, f3, 1)
    ).
