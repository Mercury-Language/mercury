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
    io.open_input(".", FileRes, !IO),
    (
        FileRes = ok(File),
        read_line(File, LineRes, !IO),
        (
            LineRes = ok(_),
            io.write_string("ok\n", !IO)
        ;
            LineRes = eof,
            io.write_string("eof\n", !IO)
        ;
            LineRes = error(Error),
            io.format("read failed: %s\n", [s(error_message(Error))], !IO)
        ),
        io.close_input(File, !IO)
    ;
        FileRes = error(Error),
        io.format("open failed: %s\n", [s(error_message(Error))], !IO)
    ).
