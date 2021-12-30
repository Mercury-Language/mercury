%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .exp file is the C backends with double-precision floats.
% The .exp2 file is for the Java backend.
% The .exp3 file is for the C# backend.
% The .exp4 file is for the C backend with single-precision floats.
%---------------------------------------------------------------------------%

:- module lexer_zero.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_term_lexer.

%---------------------------------------------------------------------------%

main(!IO) :-
    % Read from the current input stream.
    mercury_term_lexer.get_token_list(Tokens, !IO),
    write_token_list(Tokens, !IO),
    io.nl(!IO),

    % Read from a string.
    io.open_input("lexer_zero.inp", OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        io.read_file_as_string(Stream, ReadRes, !IO),
        (
            ReadRes = ok(String),
            Posn0 = posn(1, 0, 0),
            mercury_term_lexer.string_get_token_list(String, StringTokens,
                Posn0, _Posn),
            write_token_list(StringTokens, !IO)
        ;
            ReadRes = error(_, Error),
            io.write_line(Error, !IO)
        ),
        io.close_input(Stream, !IO)
    ;
        OpenRes = error(Error),
        io.write_line(Error, !IO)
    ).

:- pred write_token_list(token_list::in, io::di, io::uo) is det.

write_token_list(token_nil, !IO).
write_token_list(token_cons(Token, Context, List), !IO) :-
    io.write_int(Context, !IO),
    io.write_string(": ", !IO),
    io.write_line(Token, !IO),
    write_token_list(List, !IO).
