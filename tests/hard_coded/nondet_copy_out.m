%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test; with `--no-inlining --nondet-copy-out',
% Mercury 0.10.1 generated code for this which went into an infinite loop.
%

:- module nondet_copy_out.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if p(2) then
        io.write_string("success.\n", !IO)
    else
        io.write_string("failure.\n", !IO)
    ).

:- pred p(int::out) is multi.

p(1).
p(2).
