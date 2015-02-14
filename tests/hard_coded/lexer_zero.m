%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lexer_zero.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module lexer.

%---------------------------------------------------------------------------%

main(!IO) :-
    % Read from the current input stream.
    lexer.get_token_list(Tokens, !IO),
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
            lexer.string_get_token_list(String, StringTokens, Posn0, _Posn),
            write_token_list(StringTokens, !IO)
        ;
            ReadRes = error(_, Error),
            io.write(Error, !IO),
            io.nl(!IO)
        ),
        io.close_input(Stream, !IO)
    ;
        OpenRes = error(Error),
        io.write(Error, !IO),
        io.nl(!IO)
    ).

:- pred write_token_list(token_list::in, io::di, io::uo) is det.

write_token_list(token_nil, !IO).
write_token_list(token_cons(Token, _Context, List), !IO) :-
    io.write(Token, !IO),
    io.nl(!IO),
    write_token_list(List, !IO).
