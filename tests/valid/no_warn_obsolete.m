% Test case for the --no-warn-obsolete option.
%
:- module no_warn_obsolete.
:- interface.

:- pragma obsolete(foo/1).
:- pred foo(int::in) is semidet.

:- pred bar(int::in) is semidet.

:- implementation.

foo(1).

bar(X) :- foo(X).
