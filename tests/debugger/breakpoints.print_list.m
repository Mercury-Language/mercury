%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module breakpoints.print_list.

:- interface.

:- import_module list.
:- import_module io.

:- pred print_list(list(int), io, io).
:- mode print_list(in, di, uo) is det.

:- func string / string = string.
:- func string - string = string.

:- pred test_only_in_printlist(io::di, io::uo) is det.

:- pred test_in_both(io::di, io::uo) is det.

:- implementation.

print_list(Xs, !IO) :-
    (
        Xs = [],
        io.write_string("[]\n", !IO)
    ;
        Xs = [_ | _],
        io.write_string("[", !IO),
        print_list_2(Xs, !IO),
        io.write_string("]\n", !IO)
    ).

:- pred print_list_2(list(int)::in, io::di, io::uo) is det.

print_list_2([], !IO).
print_list_2([X | Xs], !IO) :-
    io.write_int(X, !IO),
    (
        Xs = []
    ;
        Xs = [_ | _],
        io.write_string(", ", !IO),
        print_list_2(Xs, !IO)
    ).

Str1 / Str2 = Str1 ++ "/" ++ Str2.
Str1 - Str2 = Str1 ++ "-" ++ Str2.

test_only_in_printlist(!IO) :-
    io.write_string("test_only_in_printlist\n", !IO).

test_in_both(!IO) :-
    io.write_string("test_in_both\n", !IO).
