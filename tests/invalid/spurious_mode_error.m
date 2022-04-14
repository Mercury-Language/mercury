%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module spurious_mode_error.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- type opt(T)
    --->    a(pred(int, T, T))
    ;       b(pred(string, T, T))
    ;       c(pred(float, T, T)).

:- inst opt == bound((
        a(pred(in, in, out) is det)
    ;   b(pred(in, in, out) is det)
    ;   c(pred(in, in, out) is det)
)).

:- type f(T)
    --->    f(
                maybe(pred(int, T, T)),
                maybe(pred(string, T, T)),
                maybe(pred(float, T, T))
            ).

:- inst m(I) == bound((no ; yes(I))).

:- inst f == bound(f(
        m(pred(in, in, out) is det),
        m(pred(in, in, out) is det),
        m(pred(in, in, out) is det)
    )).

main(!IO) :-
    makeit([a(foo), b(bar), c(baz)], f(no, no, no), Thing),
    foldit([i(23), s("42"), f(4.2)], Thing, 0, Stuff),
    write(Stuff, !IO).

:- pred foldit(list(string.poly_type), f(T), T, T).
:- mode foldit(in, in(f), in, out) is det.

foldit([], _F, T, T).
foldit([X | Xs], F, T0, T) :-
    (
        X = i(I),
        (
            F = f(no, _, _),
            T1 = T0
        ;
            F = f(yes(Z), _, _),
            call(Z, I, T0, T1)
        )
    ;
        X = s(S),
        (
            F = f(_, no, _),
            T1 = T0
        ;
            F = f(_, yes(Z), _),
            call(Z, S, T0, T1)
        )
    ;
        X = f(W),
        (
            F = f(_, _, no),
            T1 = T0
        ;
            F = f(_, _, yes(Z)),
            call(Z, W, T0, T1)
        )
    ;
        X = c(J),
        foobie(J, T0, T1)
    ),
    foldit(Xs, F, T1, T).

:- pred makeit(list(opt(T)), f(T), f(T)).
:- mode makeit(in(list_skel(opt)), in(f), out(f)) is det.

makeit([], F, F).
makeit([Opt | Opts], F0, F) :-
    add_opt(Opt, F0, F1),
    makeit(Opts, F1, F).

:- pred add_opt(opt(T), f(T), f(T)).
:- mode add_opt(in(opt), in(f), out(f)) is det.

add_opt(a(Z), F0, F) :-
    F0 = f(_, B, C),
    F = f(yes(Z), B, C).
add_opt(b(Z), F0, F) :-
    F0 = f(A, _, C),
    F = f(A, yes(Z), C).
add_opt(c(Z), F0, F) :-
    F0 = f(A, B, _),
    F = f(A, B, yes(Z)).

:- pred foo(int, T, T).
:- mode foo(in, in, out) is det.

foo(_, T, T).

:- pred bar(string, T, T).
:- mode bar(in, in, out) is det.

bar(_, T, T).

:- pred baz(float, T, T).
:- mode baz(in, in, out) is det.

baz(_, T, T).

/* XXX to make the bug go away, uncomment this predicate!
:- pred foobie(char, T, T).
:- mode foobie(in, in, out) is det.

foobie(_, T, T).
*/
