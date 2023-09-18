%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unqualified_method.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module unqualified_method_helper_1.

main(!IO) :-
    print_modified(1, !IO).

:- pred print_modified_int(int::in, io::di, io::uo) is det.

print_modified_int(_, !IO) :-
    io.write_string("This is the wrong method.\n", !IO).
