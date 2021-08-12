%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module promise_equivalent_clauses.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module solutions.

main(!IO) :-
    SortedList = [1, 2, 3],
    solutions(rev_sort(SortedList), RawLists),
    list.foldl(test, RawLists, !IO).

:- pred test(list(T)::in, io::di, io::uo) is det.

test(RawList, !IO) :-
    io.write(RawList, !IO),
    io.write_string(" ", !IO),
    rsort(RawList, SortedList),
    io.write_line(SortedList, !IO).

:- pred rev_sort(list(T)::in, list(T)::out) is nondet.

rev_sort(SortedList, RawList) :-
    rsort(RawList, SortedList).

:- pred rsort(list(T), list(T)).
:- mode rsort(in, out) is det.
:- mode rsort(out, in) is nondet.
:- pragma promise_equivalent_clauses(rsort/2).

rsort(Raw::in, Sorted::out) :-
    list.sort(Raw, Sorted0),
    impure impure_copy(Sorted0, Sorted).

rsort(Raw::out, Sorted::in) :-
    is_sorted(Sorted),
    list.perm(Sorted, Raw).

:- pred is_sorted(list(T)::in) is semidet.

is_sorted([]).
is_sorted([_]).
is_sorted([A, B | Rest]) :-
    compare(R, A, B),
    ( R = (<) ; R = (=) ),
    is_sorted([B | Rest]).

:- impure pred impure_copy(T::in, T::out) is det.

:- pragma foreign_proc("C",
    impure_copy(X::in, Y::out),
    [will_not_call_mercury, thread_safe],
"
    Y = X;
").
:- pragma foreign_proc("C#",
    impure_copy(X::in, Y::out),
    [will_not_call_mercury, thread_safe],
"
    Y = X;
").
:- pragma foreign_proc("Java",
    impure_copy(X::in, Y::out),
    [will_not_call_mercury, thread_safe],
"
    Y = X;
").
