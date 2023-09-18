%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module submodule_consistency.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.
:- import_module submodule_consistency_helper_1.

main(!IO) :-
    io.write_string("Hello world\n", !IO).
