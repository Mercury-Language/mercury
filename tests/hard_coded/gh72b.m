%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This is an instance of the same kind of bug that gh72a.m tests for,
% but with a user-written predicate instead of an automatically constructed
% type-specific unification predicate.
%

:- module gh72b.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

:- type maybe_reviewed
    --->    reviewed(package)
    ;       unreviewed(package).

:- type package
    --->    package(string, string).

main(!IO) :-
    R = unreviewed(_),
    fill(R),
    io.write_string(dump_maybe_reviewed(R), !IO),
    io.nl(!IO).

:- pred fill(maybe_reviewed).
:- mode fill(bound(unreviewed(free)) >> ground) is det.
:- pragma no_inline(fill/1).

fill(unreviewed(P)) :-
    P = package("a", "b").

:- func dump_maybe_reviewed(maybe_reviewed) = string.
:- pragma no_inline(dump_maybe_reviewed/1).

dump_maybe_reviewed(reviewed(P)) = "reviewed " ++ dump_package(P).
dump_maybe_reviewed(unreviewed(P)) = "unreviewed " ++ dump_package(P).

:- func dump_package(package) = string.
:- pragma no_inline(dump_package/1).

dump_package(package(A, B)) = A ++ B.
