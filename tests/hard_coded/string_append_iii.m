%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_append_iii.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    (
        string.append("", "cat", "cat"),
        string.append("c", "at", "cat"),
        string.append("ca", "t", "cat"),
        string.append("cat", "", "cat")
    ->
        io.write_string("test succeeded\n", !IO)
    ;
        io.write_string("test failed\n", !IO)
    ).
