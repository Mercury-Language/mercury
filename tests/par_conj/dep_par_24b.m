%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Uncaught Mercury exception:
% Software Error: liveness.m: Unexpected: branches of switch disagree on liveness
% First: STATE_VARIABLE_IO STATE_VARIABLE_IO_2
% Rest:  STATE_VARIABLE_IO

:- module dep_par_24b.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo)  is det.

:- implementation.

main(!IO) :-
    (
        nop(!IO)
    &
        io.see("no such file", Res, !IO)
        % call to procedure we don't have the code for
    &
        (
            Res = ok
        ;
            Res = error(_),
            (
                true
            &
                nop(!IO)
            )
        )
    ),
    io.write_string("ok\n", !IO).

:- pred nop(io::di, io::uo) is det.
:- pragma no_inline(nop/2).

nop(!IO).

% :- pred f(io.res::out, io::di, io::uo) is det.
% :- pragma no_inline(f/3).

% f(ok, !IO).
