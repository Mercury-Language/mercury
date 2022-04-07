%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% A test of io.write_binary_string_utf8.
%---------------------------------------------------------------------------%

:- module write_binary_utf8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module io.file.
:- import_module list.
:- import_module stream.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.open_binary_output(test_file, OpenOutResult, !IO),
    (
        OpenOutResult = ok(Out),
        output_test_strings(Out, !IO),
        io.close_binary_output(Out, !IO),
        read_and_print_bytes(test_file, !IO),
        io.file.remove_file(test_file, _, !IO)
    ;
        OpenOutResult = error(Error),
        handle_io_error(Error, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred output_test_strings(io.binary_output_stream::in, io::di, io::uo)
    is det.

output_test_strings(Out, !IO) :-

    % Codepoint : Name : UTF-8 encoding

    % U+0061: 'LATIN SMALL LETTER A': 0x61
    % Output bytes: 1
    io.write_binary_string_utf8(Out, "a", !IO),

    % U+03A9: 'GREEK CAPITAL LETTER OMEGA': 0xCE 0xA9
    % Output bytes: 2 - 3
    io.write_binary_string_utf8(Out, "\u03A9", !IO),

    % U+2200: 'FOR ALL': 0xE2 0x88 0x8
    % Output bytes: 4 - 6
    io.write_binary_string_utf8(Out, "\u2200", !IO),

    % U+1D11E: 'MUSICAL SYMBOL G CLEF': 0xF0 0x9D 0x84 0x9E
    % Output bytes: 7 - 10
    io.write_binary_string_utf8(Out, "\U0001D11E", !IO),

    % U+1F600: 'GRINNING FACE': 0xF0 0x9F 0x98 0x80
    % Output bytes: 11 - 14
    io.write_binary_string_utf8(Out, "\U0001F600", !IO).

%---------------------------------------------------------------------------%

% Read write_binary_utf8.bin and print out all the bytes it contains, one
% per line. The point of this is to check that the encoding is correct and
% also to ensure that we are not inadvertently writing out a BOM.

:- pred read_and_print_bytes(string::in, io::di, io::uo) is det.

read_and_print_bytes(FileName, !IO) :-
    io.open_binary_input(FileName, OpenResult, !IO),
    (
        OpenResult = ok(InFile),
        stream.input_stream_fold2_state(InFile, output_code_unit, 1,
             FoldResult, !IO),
        (
            FoldResult = ok(_)
        ;
            FoldResult = error(_, Error),
            handle_io_error(Error, !IO)
        )
    ;
        OpenResult = error(Error),
        handle_io_error(Error, !IO)
    ).

:- pred output_code_unit(uint8::in, int::in, int::out, io::di, io::uo) is det.

output_code_unit(CodeUnit, !N, !IO) :-
    io.format("%d: 0x%X\n", [i(!.N), u8(CodeUnit)], !IO),
    !:N = !.N + 1.

%---------------------------------------------------------------------------%

:- pred handle_io_error(io.error::in, io::di, io::uo) is det.

handle_io_error(Error, !IO) :-
    io.error_message(Error, ErrorMsg),
    io.stderr_stream(Stderr, !IO),
    io.format(Stderr, "error: %s\n", [s(ErrorMsg)], !IO),
    io.set_exit_status(1, !IO).

%---------------------------------------------------------------------------%

:- func test_file = string.

test_file = "write_binary_utf8.bin".

%---------------------------------------------------------------------------%
:- end_module write_binary_utf8.
%---------------------------------------------------------------------------%
