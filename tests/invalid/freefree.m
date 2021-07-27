%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module freefree.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if p then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred p is semidet.

p :-
    some [X, Y] X \= Y.
