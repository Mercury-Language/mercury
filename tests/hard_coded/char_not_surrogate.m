%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module char_not_surrogate.
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
    Chars = [
        '\U0001D800',
        '\U0001DFFF',
        '\U0010D800',
        '\U0010DFFF'
    ],
    list.foldl(test, Chars, !IO).

:- pred test(char::in, io::di, io::uo) is det.

test(Char, !IO) :-
    io.write_string("code point: ", !IO),
    Int = char.to_int(Char),
    io.format("0x%06x", [i(Int)], !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%
