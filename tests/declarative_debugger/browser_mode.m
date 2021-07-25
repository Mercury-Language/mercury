%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module browser_mode.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
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

:- pred test(int::in) is semidet.

test(_) :-
    semidet_fail.

:- pred p(char::in, int::out) is nondet.

p(A, D) :-
    q(A, B),
    ( if r(B, C) then
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

:- pred q(char::in, char::out) is nondet.

q('a', 'a').
q('a', 'b').
q('c', 'c').

:- pred r(char::in, int::out) is semidet.

r('a', 10).

:- pred s(int::in, int::out) is det.

s(N, 3 * N).
