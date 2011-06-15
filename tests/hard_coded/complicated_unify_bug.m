% vim: ts=4 sw=4 et ft=mercury
%
% We used to get this compiler abort on this file:
%
% Uncaught Mercury exception:
% Software Error: check_hlds.det_analysis:
% predicate `check_hlds.det_analysis.det_infer_unify'/13:
% Unexpected: complicated_unify but no var
%
% The ultimate cause was the three-fold duplication of the X = Y test in p/4
% due to the two-level switch on X and Y. In two of the copies, X and Y are
% known to be non-unifiable (X = a/1 and Y = b/1 or vice versa). Both of those
% copies of X = Y use a mode of the unification procedure of t that cannot
% succeed, and which contain var/functor unifications that cannot succeed.
% The compiler abort was due to determinism analysis not being able to compute
% a failure context from a var/functor unification that cannot succeed.

:- module complicated_unify_bug.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module string.

:- type t
    --->    a(int)
    ;       b(string).

main(!IO) :-
    do_test(a(1), a(1), !IO),
    do_test(a(2), a(3), !IO),
    do_test(a(4), b("four"), !IO),
    do_test(b("five"), a(5), !IO),
    do_test(b("six"), b("six"), !IO),
    do_test(b("seven"), b("eight"), !IO).

:- pred do_test(t::in, t::in, io::di, io::uo) is det.

do_test(X, Y, !IO) :-
    p(X, Y, Z, S),
    io.write(Z, !IO),
    io.write_string(" ", !IO),
    io.write_string(S, !IO),
    io.nl(!IO).

:- pred p(t::in, t::in, t::out, string::out) is det.
:- pragma no_inline(p/4).

p(X, Y, Z, S) :-
    (
        X = a(XN),
        Y = a(YN),
        Z = a(XN + YN),
        S = "aa"
    ;
        (
            X = a(XN),
            Y = b(YS),
            Z = b(string.int_to_string(XN) ++ " " ++ YS)
        ;
            X = b(XS),
            Y = a(YN),
            Z = b(XS ++ " " ++ string.int_to_string(YN))
        ;
            X = b(XS),
            Y = b(YS),
            Z = b(XS ++ " " ++ YS)
        ),
        ( if X = Y then
            S = "same"
        else
            S = "diff"
        )
    ).
