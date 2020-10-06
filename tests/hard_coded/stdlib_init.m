%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that the standard library is initialised before main/2 is called.
%

:- module stdlib_init.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.get_globals(Globals, !IO),
    io.print_line(Globals, !IO),
    io.get_op_table(OpsTable, !IO),
    io.print_line(OpsTable, !IO),
    io.stdin_stream(Stdin, !IO),
    io.print_line(Stdin, !IO),
    io.stdout_stream(Stdout, !IO),
    io.print_line(Stdout, !IO),
    io.stderr_stream(Stderr, !IO),
    io.print_line(Stderr, !IO).
