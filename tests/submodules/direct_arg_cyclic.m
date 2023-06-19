%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module direct_arg_cyclic.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module direct_arg_cyclic_helper_1.
% not :- import_module direct_arg_cyclic_helper_2.

main(!IO) :-
    write_string("Hello, world!\n", !IO).
