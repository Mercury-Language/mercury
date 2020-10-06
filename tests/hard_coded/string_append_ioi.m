%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_append_ioi.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if
        string.append("", A, "cat"),
        string.append("c", B, "cat"),
        string.append("ca", C, "cat"),
        string.append("cat", D, "cat"),
        not string.append("cat", _, "dogcat")
    then
        io.write([A, B, C, D], !IO),
        io.nl(!IO)
    else
        io.write_string("tested failed\n", !IO)
    ).
