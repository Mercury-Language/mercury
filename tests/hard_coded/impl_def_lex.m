%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the lexer reads implementation-defined literals correctly.

:- module impl_def_lex.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_term_lexer.

main(!IO) :-
    get_token_list(Tokens, !IO),
    write_list(Tokens, !IO).

:- pred write_list(token_list::in, io::di, io::uo) is det.

write_list(token_nil, !IO).
write_list(token_cons(Token, Context, List), !IO) :-
    io.write(Token, !IO),
    io.write_char(' ', !IO),
    io.write(Context, !IO),
    io.nl(!IO),
    write_list(List, !IO).
