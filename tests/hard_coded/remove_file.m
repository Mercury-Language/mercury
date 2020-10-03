%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% If this test fails when run natively under Win32 (no cygwin) then make
% sure that either the TMPDIR environment variable points somewhere
% sensible or that a directory <drive letter>:\tmp exists.
%
% The .exp file is for the C backends.
% The .exp2 file is for the Erlang backend.
% The .exp3 file is for the Java backend.
% The .exp4 file is for the C# backend.
%
%---------------------------------------------------------------------------%

:- module remove_file.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module exception.

main(!IO) :-
    io.make_temp_file(NameResult, !IO),
    (
        NameResult = ok(Name)
    ;
        NameResult = error(Error),
        throw(Error)
    ),
    %%%%%%% io.print("Temp file name = ", !IO), io.print_line(Name, !IO),
    io.tell(Name, TellResult, !IO),
    (
        TellResult = io.ok,
        io.print_line("Just testing", !IO),
        io.told(!IO),
        io.remove_file(Name, RemoveResult, !IO),
        (
            RemoveResult = io.ok,
            io.see(Name, SeeResult, !IO),
            ( if SeeResult = io.ok then
                io.print("Remove didn't remove file\n", !IO),
                io.set_exit_status(1, !IO)
            else
                io.print("Test passed\n", !IO)
            )
        ;
            RemoveResult = io.error(RemoveError),
            io.print("Remove failed: ", !IO),
            io.error_message(RemoveError, RemoveErrorMsg),
            io.print_line(RemoveErrorMsg, !IO),
            io.set_exit_status(1, !IO)
        ),
        io.remove_file(Name, RemoveAgainResult, !IO),
        (
            RemoveAgainResult = io.ok,
            io.print("Second remove didn't report failure\n", !IO),
            io.set_exit_status(1, !IO)
        ;
            RemoveAgainResult = io.error(RemoveAgainError),
            io.print("Second remove failed, as expected: ", !IO),
            io.error_message(RemoveAgainError, RemoveAgainErrorMsg),
            io.print_line(RemoveAgainErrorMsg, !IO)
        )
    ;
        TellResult = io.error(TellError),
        io.print("Tell failed: ", !IO),
        io.error_message(TellError, TellErrorMsg),
        io.print_line(TellErrorMsg, !IO),
        io.set_exit_status(1, !IO)
    ).
