%-----------------------------------------------------------------------------%
% Try goal without I/O.
% We should still be able to perform I/O in then, else, catch branches.

:- module try_syntax_5.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

%-----------------------------------------------------------------------------%


main(!IO) :-
    (try []
        ( semidet_true ->
            throw(12345)
        ;
            semidet_fail
        )
    then
        io.write_string("then\n", !IO)
    else
        io.write_string("else\n", !IO)
    catch_any E ->
        io.write_string("caught: ", !IO),
        io.write(E, !IO),
        io.nl(!IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
