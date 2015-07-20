%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case used to show up an obscure bug in the compiler.
% The compiler reported spurious mode errors in the compiler-generated
% unification/compare/index predicates, due to the name f being held
% by both a data constructor and a function.

:- module nasty_func_test.
:- interface.

:- type foo
    --->    f(int)
    ;       g.

:- func f(int) = foo.

:- implementation.

f(_) = g.
