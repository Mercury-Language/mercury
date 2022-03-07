%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% If this test fails when run natively under Win32 (no cygwin) then make
% sure that either the TMPDIR environment variable points somewhere
% sensible or that a directory <drive letter>:\tmp exists.
%
% The .exp file is for the C backends.
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
:- import_module io.file.

main(!IO) :-
    io.file.make_temp_file(NameResult, !IO),
    (
        NameResult = ok(Name)
    ;
        NameResult = error(Error),
        throw(Error)
    ),
    %%%%%%% io.print("Temp file name = ", !IO), io.print_line(Name, !IO),
    io.open_output(Name, OpenOutputResult, !IO),
    (
        OpenOutputResult = io.ok(Stream),
        io.print_line(Stream, "Just testing", !IO),
        io.close_output(Stream, !IO),
        io.file.remove_file(Name, RemoveResult, !IO),
        (
            RemoveResult = io.ok,
            io.open_input(Name, OpenInputResult, !IO),
            (
                OpenInputResult = io.ok(_Stream),
                io.print("Remove didn't remove file\n", !IO),
                io.set_exit_status(1, !IO)
            ;
                OpenInputResult = io.error(_),
                io.print("Test passed\n", !IO)
            )
        ;
            RemoveResult = io.error(RemoveError),
            io.print("Remove failed: ", !IO),
            io.error_message(RemoveError, RemoveErrorMsg),
            io.print_line(RemoveErrorMsg, !IO),
            io.set_exit_status(1, !IO)
        ),
        io.file.remove_file(Name, RemoveAgainResult, !IO),
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
        OpenOutputResult = io.error(OpenOutputError),
        io.print("Open for output failed: ", !IO),
        io.error_message(OpenOutputError, OpenOutputErrorMsg),
        io.print_line(OpenOutputErrorMsg, !IO),
        io.set_exit_status(1, !IO)
    ).
