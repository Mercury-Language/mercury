%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% This test case is derived fron an old post on m-users, in which
% the last call in a clause was taken as a entire clause, due to
% the previous line ending with a period instead of a comma.
% The resulting the error about a disjunction with more than one disjunct
% that could succeed confused the user who did not see any disjunction
% in the predicate. The code is made up; the problem is real.
%---------------------------------------------------------------------------%

:- module accidental_clause.
:- interface.

:- type t
    --->    f1
    ;       f2.

:- pred foo(t::in, int::out) is semidet.

:- implementation.

foo(f1, 42).
foo(f2, N) :-
    N = 43.         % This is supposed to be a comma, not a period.
    foo(f1, 44).    % This is therefore supposed to be a call, not a fact.
