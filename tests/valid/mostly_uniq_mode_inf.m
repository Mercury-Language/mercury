% This module tests inference of mostly-unique modes.
% The compiler needs to infer mostly-unique modes for foo/2 and foo2/2.

:- module mostly_uniq_mode_inf.
:- interface.
:- pred p is semidet.

:- implementation.
:- import_module int.

p :- foo(42, Z), bar(Z, A), A > 100.

foo(X, Y) :- foo2(X, Y).

foo2(X, Y) :- Y is X + 10.
foo2(X, Y) :- Y = X.

:- mode bar(mdi, muo) is det.
bar(X, X).

