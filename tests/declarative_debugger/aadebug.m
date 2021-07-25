%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module aadebug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module std_util.

main(!IO) :-
    ( if
        p('a', X),
        test(X)
    then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred test(int).
:- mode test(in) is semidet.

test(_) :-
    semidet_fail.

:- pred p(character::in, int::out) is nondet.

p(A, D) :-
    q(A, B),
    ( if
        r(B, C)
    then
        (
            s(C, D)
        ;
            D = 31
        )
    else
        not(
            q(B, _)
        ),
        D = 32
    ).

:- pred q(character::in, character::out) is nondet.

q('a', 'a').
q('a', 'b').
q('c', 'c').

:- pred r(character::in, int::out) is semidet.

r('a', 10).

:- pred s(int::in, int::out) is det.

s(N, 3 * N).

