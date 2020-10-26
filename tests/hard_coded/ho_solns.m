%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for solutions
%
% Author: trd
%
% This test case exercises a number of features:
%   - The ability to modecheck equivalence insts correctly (the
%     Mercury compiler of Jan 20 1997 couldn't compile code like
%     this - in fact if you made mypred == ground it still couldn't
%     get it right).
%   - Solutions of higher order predicates
%   - In non-conservative GC grades, deep_copy of closures (since
%     solutions is implemented using deep copy on them).
%   - Higher order syntax -- P(5, 1, X)
%

:- module ho_solns.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
    unsorted_solutions(foo, List0),
    convert_list(List0, List),
    use_list(List, !IO).

:- type mypred == (pred(int, int, int)).
:- inst mypred == (pred(in, in, out) is det).

:- pred convert_list(list(T), list(T)).
:- mode convert_list(in, out(list_skel(mypred))) is det.

:- pragma foreign_proc("C",
    convert_list(L0 :: in, L :: out(list_skel(mypred))),
    [will_not_call_mercury, promise_pure],
"
{
    L = L0;
}
").
:- pragma foreign_proc("C#",
    convert_list(L0 :: in, L :: out(list_skel(mypred))),
    [promise_pure], "
{
    L = L0;
}
").
:- pragma foreign_proc("Java",
    convert_list(L0 :: in, L :: out(list_skel(mypred))),
    [promise_pure], "
{
    L = L0;
}
").

:- pred use_list(list(mypred)::in(list_skel(mypred)), io::di, io::uo) is det.

use_list([], !IO).
use_list([P | Ps], !IO) :-
    P(5, 1, X),
    io.write_int(X, !IO),
    io.write_string("\n", !IO),
    use_list(Ps, !IO).

:- pred foo(mypred).
:- mode foo(out(mypred)) is multi.

foo(X) :- X = (pred(A::in, B::in, C::out) is det :- C = A + B).
foo(X) :- X = (pred(A::in, B::in, C::out) is det :- C = A * B).
foo(X) :- X = (pred(A::in, B::in, C::out) is det :- C = A - B).
