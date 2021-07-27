%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module spurious_obsolete.
:- interface.

:- pred foo(int::in) is semidet.
:- pragma obsolete(foo/1).

:- pred bar(int::in) is semidet.
:- pragma obsolete(bar/1).

:- pred baz(int::in) is semidet.

:- implementation.

foo(1).

bar(X) :-
    % We shouldn't get a warning about this call to foo/1 ...
    foo(X).

baz(X) :-
    % but we should get one about this one.
    foo(X).
