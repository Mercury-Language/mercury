% This module tests inference of mostly-unique modes.
% The compiler needs to infer muo modes for foo/2 and foo2/2
% and it needs to infer an (mdi,muo) mode for baz/2.

:- module mostly_uniq_mode_inf.
:- interface.
:- pred p is semidet.
:- pred q is semidet.

:- implementation.
:- import_module int.

p :- foo(42, Z), bar(Z, A), A > 100.

q :- foo(42, Z), ( baz(Z, B), bar(B, A), A > 100 ; Z > 100 ).

foo(X, Y) :- foo2(X, Y).

foo2(X, Y) :- Y = X + 10.
foo2(X, Y) :- Y = X.

baz(X, X).

:- mode bar(mdi, muo) is det.
bar(X, X).

