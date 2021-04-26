%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_error_use_module.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- use_module type_error_use_module_2.
:- use_module map.

main(!IO) :-
    io.write_line(init, !IO),
    do_main(!IO).
