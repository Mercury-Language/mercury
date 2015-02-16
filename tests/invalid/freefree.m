%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module freefree.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
    ( { p } ->
        io__write_string("yes\n")
    ;
        io__write_string("no\n")
    ).

:- pred p is semidet.

p :-
    some [X, Y] X \= Y.
