% This test case shows up an obscure bug in the compiler.
% The compiler reports spurious mode errors in the compiler-generated
% unification/compare/index predicates.
% It's not clear whether or not this code is legal, but at very
% least the compiler ought to issue a better error message.
% If you write obfuscated code like this, you really deserve
% what you get, but I guess we should fix the bug someday anyway...
% for the moment, I'll file the bug report and forget it.

:- module nasty_func_test.
:- type foo ---> f(int) ; g.

:- func f(int) = foo.
f(_) = g.
