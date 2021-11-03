%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_not_surrogate.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Strings = [
        "\U0001D800\U0001DFFF",
        "\U0010D800\U0010DFFF"
    ],
    list.foldl(test, Strings, !IO).

:- pred test(string::in, io::di, io::uo) is det.

test(String, !IO) :-
    io.write_string("code points: ", !IO),
    Chars = string.to_char_list(String),
    Ints = list.map(char.to_int, Chars),
    io.write_list(Ints, ", ", write_hex_int, !IO),
    io.nl(!IO).

:- pred write_hex_int(int::in, io::di, io::uo) is det.

write_hex_int(Int, !IO) :-
    io.format("0x%06x", [i(Int)], !IO).

%---------------------------------------------------------------------------%
