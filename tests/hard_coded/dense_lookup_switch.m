%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module dense_lookup_switch.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type foo ---> a ; b ; c ; d ; e ; f ; g ; h.

main(!IO) :-
    bar(e, !IO).

:- pragma no_inline(bar/3).

:- pred bar(foo::in, io::di, io::uo) is det.

bar(X, !IO) :-
    ( if
        ( X = a ; X = b ; X = c ; X = d )
    then
        io.write_string("a or b or c or d\n", !IO)
    else
        io.write_string("something else\n", !IO)
    ).
