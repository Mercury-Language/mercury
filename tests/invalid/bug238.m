% vim: ts=4 sw=4 et ft=mercury
%
% This is a regression test for Mantis bug 238.
%
% The symptom of that bug was a compiler abort, caused by the fact that
% the front end invoked the middle end even if the front end found the
% error in the code below.

:- module bug238.

:- interface.

:- pred foo is semidet.

:- implementation.

:- import_module list.
:- import_module solutions.

foo :-
    promise_equivalent_solutions [] (
        unsorted_solutions(
            pred(3::out) is nondet,
            [])
    ).
