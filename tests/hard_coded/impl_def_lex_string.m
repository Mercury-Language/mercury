%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the lexer reads implementation-defined literals from strings correctly.

:- module impl_def_lex_string.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module io.
:- import_module mercury_term_lexer.

%---------------------------------------------------------------------------%

main(!IO) :-
    InitialPos = posn(1, 0, 0),

    string_get_token_list("$f", TokensA, InitialPos, FinalPosA),
    io.write_line({TokensA, FinalPosA}, !IO),

    string_get_token_list("$foo", TokensB, InitialPos, FinalPosB),
    io.write_line({TokensB, FinalPosB}, !IO),

    % followed by non-lowercase character
    string_get_token_list("$FOO", TokensC, InitialPos, FinalPosC),
    io.write_line({TokensC, FinalPosC}, !IO),

    % followed by eof
    string_get_token_list("$", TokensD, InitialPos, FinalPosD),
    io.write_line({TokensD, FinalPosD}, !IO),

    % followed by graphic character
    string_get_token_list("$!$", TokensE, InitialPos, FinalPosE),
    io.write_line({TokensE, FinalPosE}, !IO),

    string_get_token_list("$,", TokensF, InitialPos, FinalPosF),
    io.write_line({TokensF, FinalPosF}, !IO),

    string_get_token_list("$0", TokensG, InitialPos, FinalPosG),
    io.write_line({TokensG, FinalPosG}, !IO).
