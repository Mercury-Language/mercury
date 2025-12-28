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
        % We use the approach below because over time, ErrorMsg0 has been both
        % "Access to the path 'xyz' is denied." and
        % "can't open input file: Access to the path 'xyz' is denied.;
        % note the presence of an extra quote in "can't".
        %
        % The code below can cope with both two and three quotes in ErrorMsg0,
        % and therefore with both three and four elements in QuoteChunks0.
        % And if ever needed, with higher numbers as well.
        QuoteChunks0 = string.split_at_char('\'', ErrorMsg0),
        list.reverse(QuoteChunks0, RevQuoteChunks0),
        ( if
            RevQuoteChunks0 =
                [LastChunk1, _PathNameChunk, LastChunk3 | RevEarlyChunks],
            % The text before has to end with "Access to the path ",
            % but what comes before that does not matter.
            string.remove_suffix(LastChunk3, "Access to the path ", _),
            LastChunk1 = " is denied."
        then
            % Replace an error message that reports a path that may be
            % workspace-specific, with the standard path we put into
            % the .expN files, which is the current directory.
            RevQuoteChunks = [LastChunk1, ".", LastChunk3 | RevEarlyChunks],
            list.reverse(RevQuoteChunks, QuoteChunks),
            ErrorMsg = string.join_list("'", QuoteChunks)
        else
            ErrorMsg = ErrorMsg0
        ),
        io.format("open failed: %s\n", [s(ErrorMsg)], !IO)
    ).
