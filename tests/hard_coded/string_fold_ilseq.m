%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_fold_ilseq.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    S0 = "😀",
    S1 = string.between(S0, 0, count_code_units(S0) - 1),
    S = "abc" ++ S0 ++ S1 ++ "xyz",

    io.write_string("string.foldl:\n", !IO),
    string.foldl(write_char_or_hex, S, !IO),
    io.write_string("\n\n", !IO),

    io.write_string("string.foldr:\n", !IO),
    string.foldr(write_char_or_hex, S, !IO),
    io.write_string("\n", !IO).

:- pred write_char_or_hex(char::in, io::di, io::uo) is det.

write_char_or_hex(Char, !IO) :-
    ( if Char = '\ufffd' ; char.is_surrogate(Char) then
        io.format("%#x", [i(char.to_int(Char))], !IO)
    else
        io.write_char(Char, !IO)
    ),
    io.write_char(' ', !IO).
