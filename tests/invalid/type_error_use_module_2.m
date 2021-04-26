%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_error_use_module_2.

:- interface.
:- import_module io.

:- func init = int.

:- pred do_main(io::di, io::uo) is det.

:- implementation.

init = 42.

do_main(!IO) :-
    io.write_string("Hello, world!\n", !IO).
