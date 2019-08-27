%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This is a test for a bug in the interaction of the direct arg optimization
% with partially instantiated data structures.
%
% The bug arises in the call to the type-specific unification predicate
% for the maybe_reviewed type that the compile generates to unify
% the first element of S with R, but ONLY when that call is not inlined.
%
% In the inlined version, when we fill in the argument of R, we update R.
% In the non-inlined version, when we fill in the argument of HeadVar__1,
% for which main passes R, we update HeadVar__1, but do NOT update R in main.
% Normally, this is not a problem: the filled in argument is normally
% on the heap, and the caller's pointer also points to it, so the caller
% also sees the field as being filled in. However, with the direct arg
% optimization, the field being filled in is NOT on the heap; it is
% in the pointer, next to the primary tag. It is the lack of any update
% to this field in the caller's R that leaves the value of R as a
% tagged NULL pointer, whose dereferencing leads to the crash.
%

:- module gh72a.
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
    ( if
        R = unreviewed(_),
        sols(S),
        S = [R]
    then
        io.write_string(dump_maybe_reviewed(R), !IO),
        io.nl(!IO)
    else
        true
    ).

:- pred sols(list(maybe_reviewed)::out) is det.

sols([unreviewed(package("a", "b"))]).

:- func dump_maybe_reviewed(maybe_reviewed) = string.
:- pragma no_inline(dump_maybe_reviewed/1).

dump_maybe_reviewed(reviewed(P)) = "reviewed " ++ dump_package(P).
dump_maybe_reviewed(unreviewed(P)) = "unreviewed " ++ dump_package(P).

:- func dump_package(package) = string.
:- pragma no_inline(dump_package/1).

dump_package(package(A, B)) = A ++ B.
