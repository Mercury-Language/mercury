%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module completion.

:- interface.

:- import_module io.

:- include_module completion.sub1, completion.sub2.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("ok\n", !IO).

:- func z = int.
z = 0.

:- func zz = int.
zz = 0.
