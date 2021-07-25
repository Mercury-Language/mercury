%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A regression test: mercury-0.4 miscompiled this program.

:- module hello_again.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module solutions.

main(!IO) :-
    solutions(hello(1, 1), List),
    io.write_strings(List, !IO).

:- pred hello(int::in, int::in, string::out) is nondet.

hello(1, 1, "Hello, world\n").
hello(1, 1, "Hello again, world\n").

% should output both strings
% ------------------------
% But it does not seem to:
% ------------------------
%
% > mc test4
% > test4
% Hello, world
% >
%

