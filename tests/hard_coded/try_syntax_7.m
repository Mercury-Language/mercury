%-----------------------------------------------------------------------------%
% Check inst casts get added in try goal transformation.

:- module try_syntax_7.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

main(!IO) :-
    (try [] (
        X = 1,
        Y = "hi"
    )
    then
        % Without inst casts, X and Y would be `ground' here.
        p(X, Y, !IO)
    ).

:- pred p(int::in(bound(1)), string::in(bound("hi")),
    io::di, io::uo) is det.

p(1, "hi", !IO) :-
    io.write_string("It works!\n", !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
