%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module direct_arg_cyclic1.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module direct_arg_cyclic2.
% not :- import_module direct_arg_cyclic3.

main(!IO) :-
    write_string("Hello, world!\n", !IO).

