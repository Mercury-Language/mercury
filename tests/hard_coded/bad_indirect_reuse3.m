%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test.
%---------------------------------------------------------------------------%

:- module bad_indirect_reuse3.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
    copy(foo(43, 42), A),
    B = bar(-1, A, -2),
    % Although neither A, B are used afterwards they do share memory so reuse
    % is not possible.
    quux(A, B, C, D),
    io.write_line({C, D}, !IO).

:- type foo
    --->    foo(int, int).

:- type bar
    --->    bar(int, foo, int).

:- pred quux(foo::in, bar::in, foo::out, foo::out) is det.
:- pragma no_inline(quux/4).

quux(foo(A, B), bar(_, foo(C, D), _), foo(B, A), foo(D, C)).
