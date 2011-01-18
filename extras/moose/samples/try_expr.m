%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%

:- module try_expr.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module expr.
:- import_module list.

%-----------------------------------------------------------------------------%

    % We need to wrap this up using a notag type in order to satisfy the
    % requirements on the form that type class instance arguments can take.
    %
:- type token_list
    ---> token_list(list(token)).

:- instance parser_state(token_list) where [

    get_token(eof, token_list([]),       token_list([])),
    get_token(T,   token_list([T | Ts]), token_list(Ts)),

    unget_token(T, token_list(Ts)) = token_list([T | Ts])
].

main(!IO) :-
    read_line(Res0, !IO),
    (
        Res0 = ok(Chars),
        scan(Chars, Toks),
        parse(Res, token_list(Toks), token_list(RemainingToks)),
        io.write(Res, !IO),
        io.nl(!IO),
        io.write(RemainingToks, !IO),
        io.nl(!IO),
        main(!IO)
    ;
        Res0 = eof
    ;
        Res0 = error(Err),
        io.error_message(Err, Msg),
        io.write_string(Msg, !IO),
        io.nl(!IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module try_expr.
%-----------------------------------------------------------------------------%
