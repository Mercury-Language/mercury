%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Regression test for a problem where io.set_line_number/3 was incorrectly
% setting the line number of the current output stream instead of the
% line number of the current input stream.
%---------------------------------------------------------------------------%

:- module bad_set_line_number.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    % Get the current input stream's line number (should be 1 initially).
    io.get_line_number(LineBefore, !IO),
    io.format("Input line number before set_line_number: %d.\n",
        [i(LineBefore)], !IO),

    % Try to set the input stream's line number to 100.
    io.set_line_number(100, !IO),

    % Read back the input stream's line number.
    io.get_line_number(LineAfter, !IO),
    io.format("Input line number after set_line_number(100): %d.\n",
        [i(LineAfter)], !IO),

    % Also check the output stream's line number to see if it was
    % accidentally modified instead.
    io.get_output_line_number(OutLine, !IO),
    io.format("Output line number: %d.\n", [i(OutLine)], !IO),

    ( if LineAfter = 100 then
        io.write_string(
            "PASS: set_line_number correctly set input line number.\n", !IO)
    else
        io.write_string("FAIL: input line number is not 100.\n", !IO)
    ).
