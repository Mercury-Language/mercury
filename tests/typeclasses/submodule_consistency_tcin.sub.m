%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module submodule_consistency_tcin.sub.

:- interface.

:- import_module io.

:- pred test(io::di, io::uo) is det.

:- implementation.

test(!IO) :-
    io.write_string("Sub", !IO).
