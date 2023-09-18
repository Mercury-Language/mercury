%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module submodule_consistency_helper_1.submodule_consistency_helper_3.

:- interface.

:- import_module io.

:- pred test(io::di, io::uo) is det.

:- implementation.

test(!IO) :-
    io.write_string("Sub", !IO).
