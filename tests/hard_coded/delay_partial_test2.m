%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the --delay-partial-instantiations option with disjunctions.

:- module delay_partial_test2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if foo(2, Y) then
        io.print_line(Y, !IO)
    else
        io.print("foo failed\n", !IO)
    ),
    ( if bar(3, Y2) then
        io.print_line(Y2, !IO)
    else
        io.print("bar failed\n", !IO)
    ),
    quux(Q, yes),
    io.print_line(Q, !IO).

:- type t
    --->    t(
                a :: int,
                b :: int
            ).

:- pred foo(int::in, t::out) is nondet.
:- pragma no_inline(foo/2).

foo(X, Y) :-
    U ^ b = U ^ a - 1,
    Y ^ b = Z,
    (
        X = 1,
        Y ^ a = Z,
        Z = U ^ b
    ;
        int.even(X),
        Z = U ^ a,
        Y ^ a = U ^ a
    ;
        int.odd(X),
        Z = U ^ a,
        Y ^ a = X
    ),
    U ^ a = X.

:- pred bar(int::in, t::out) is nondet.
:- pragma no_inline(bar/2).

bar(X, Y) :-
    Y ^ a = Z,      % constructed outside
    U ^ b = U ^ a - 1,
    (
        X = 1,
        Z = U ^ b,
        Y ^ b = Z   % ground inside
    ;
        int.even(X),
        Z = U ^ a,
        Y ^ b = 2   % ground inside
    ;
        int.odd(X),
        Z = U ^ a,
        Y ^ b = 3   % ground inside
    ),
    U ^ a = X.

:- type q
    --->    qa(int)
    ;       qb(int, int)
    ;       qc(int)
    ;       qd.

:- pred quux(q, bool).
:- mode quux(in, in) is semidet.
:- mode quux(out, in(bound(yes))) is multi.

:- pragma no_inline(quux/2).

quux(qa(_), no).
quux(qb(_, _), no).
quux(qc(42), yes).
quux(qd, yes).
