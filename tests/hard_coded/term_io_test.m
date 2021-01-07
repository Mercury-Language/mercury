%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module term_io_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.
:- import_module term_io.

main(!IO) :-
    read_term(Res0, !IO),
    (
        Res0 = term(VarSet, Term),
        write_term_nl(VarSet, Term, !IO),
        main(!IO)
    ;
        Res0 = eof
    ;
        Res0 = error(Msg, Line),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "%d: %s\n", [i(Line), s(Msg)], !IO),
        main(!IO)
    ).

