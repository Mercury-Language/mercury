%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module nested_intermod_main.
:- interface.
:- import_module io.

:- pred xyzzy(int).
:- mode xyzzy(in) is semidet.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module nested_intermod.

main(!IO) :-
    test(1, !IO),
    test(2, !IO),
    test(3, !IO),
    test(4, !IO),
    test(5, !IO).

:- pred test(int::in, io::di, io::uo) is det.

test(X, !IO) :-
    io.print("X = ", !IO), io.print(X, !IO), io.print(": ", !IO),
    ( if xyzzy(X) then
        io.print_line("yes", !IO)
    else
        io.print_line("no", !IO)
    ).

xyzzy(X) :-
    foo(X).

:- end_module nested_intermod_main.
