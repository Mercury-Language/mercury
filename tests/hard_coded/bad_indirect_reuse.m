%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test.
%---------------------------------------------------------------------------%

:- module bad_indirect_reuse.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
    copy(bar(3, 1), Sub),
    A = foo(2, Sub),
    quux(A, _),                     % bad indirect reuse call
    io.write_line(Sub, !IO),

    copy(foo(1, bar(2, 0)), B),
    quux(B, B1),                    % good indirect reuse call
    io.write_line(B1, !IO).

:- type foo
    --->    foo(int, bar).

:- type bar
    --->    bar(int, int).

:- pred quux(foo::in, foo::out) is det.
:- pragma no_inline(pred(quux/2)).

quux(foo(A, bar(B, C)), foo(C, bar(A, B))).
