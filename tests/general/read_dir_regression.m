%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for a problem where I/O errors were being reported as EOF.
%
% The .exp file is for the C grades (on Unix like systems).
% The .exp2 file is for the Java grades.
% The .exp3 file is for the C# grades.
% The .exp4 file is for the C grades (on Windows).
%
%---------------------------------------------------------------------------%

:- module read_dir_regression.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.
:- import_module list.

main(!IO) :-
    io.open_input(".", FileResult, !IO),
    (
        FileResult = ok(File),
        read_line(File, LineResult, !IO),
        (
            LineResult = ok(_),
            io.write_string("ok\n", !IO)
        ;
            LineResult = eof,
            io.write_string("eof\n", !IO)
        ;
            LineResult = error(Error),
            io.format("read failed: %s\n", [s(error_message(Error))], !IO)
        ),
        io.close_input(File, !IO)
    ;
        FileResult = error(Error),
        ErrorMsg0 = io.error_message(Error),
        ( if
            string.split_at_char('\'', ErrorMsg0) =
                ["Access to the path ", _, " is denied."]
        then
            % Replace an error message that reports a possibly
            % workspace-specific path with the standard path
            % in one of the .expN files.
            ErrorMsg = "Access to the path '.' is denied."
        else
            ErrorMsg = ErrorMsg0
        ),
        io.format("open failed: %s\n", [s(ErrorMsg)], !IO)
    ).
