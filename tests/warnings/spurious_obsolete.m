:- module spurious_obsolete.
:- interface.

:- pragma obsolete(foo/1).
:- pred foo(int::in) is semidet.

:- pragma obsolete(bar/1).
:- pred bar(int::in) is semidet.

:- pred baz(int::in) is semidet.

:- implementation.

foo(1).

% We shouldn't get a warning about this call to foo/1 ...
bar(X) :- foo(X).

% but we should get one about this one.
baz(X) :- foo(X).
