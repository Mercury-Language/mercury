%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test.
% The Mercury compiler of Apr 11 1997 failed for this in non-gc grades,
% because of a bug in deep_copy() of equivalence types.

:- module deep_copy_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module term.
:- import_module varset.

main(!IO) :-
    test1(!IO),
    test2(!IO),
    test3(!IO).

:- pred test1(io::di, io::uo) is det.
test1(!IO) :-
    Lambda = (pred(X::out) is nondet :-
        varset.init(Varset0),
        varset.new_vars(10, Vars, Varset0, _),
        list.member(X, Vars)
    ),
    solutions(Lambda, List),
    io.write_line(List, !IO).

:- pred test2(io::di, io::uo) is det.
test2 -->
    test2b("blahblah").

:- pred test2b(T::in, io::di, io::uo) is det.
test2b(S, !IO) :-
    F = foo(S),
    solutions(F, List),
    io.write_line(List, !IO).

:- pred foo(T, var).
:- mode foo(in, out) is nondet.
foo(Blah, X) :-
    varset.init(Varset0),
    varset.new_vars(10, Vars, Varset0, _),
    list.member(X, Vars).

:- pred test3(io::di, io::uo) is det.
test3(!IO) :-
    solutions((pred(X::out) is nondet :- bar(X)), List),
    io.write_line(List, !IO).

:- pred bar(int).
:- mode bar(out) is det.
bar(42).
