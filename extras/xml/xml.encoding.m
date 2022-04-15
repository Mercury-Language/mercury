%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% Copyright (C) 2014, 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%

:- module xml.encoding.
:- interface.

:- import_module parsing.

%---------------------------------------------------------------------------%

:- type ascii7 ---> ascii7.

:- instance encoding(ascii7).

:- type latin1 ---> latin1.

:- instance encoding(latin1).

:- type utf8 ---> utf8.

:- instance encoding(utf8).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module unicode.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- instance encoding(ascii7) where [
    pred(decode/4) is decode_ascii7,
    pred(encode/3) is encode_ascii7
].

:- pred decode_ascii7(ascii7::in, unicode::out, entity::in, entity::out)
    is semidet.

decode_ascii7(_, U) -->
    [U].

:- pred encode_ascii7(ascii7::in, list(unicode)::in, string::out) is det.

encode_ascii7(_, Us, Str) :-
    unicodesToAscii7(Us, Cs, []),
    string.from_char_list(Cs, Str).

:- pred unicodesToAscii7(list(unicode)::in, list(char)::out, list(char)::in)
    is det.

unicodesToAscii7([]) --> [].
unicodesToAscii7([U | Us]) -->
    ( if { U > 0x00, U < 0x80, char.to_int(C, U) } then
        [C],
        unicodesToAscii7(Us)
    else
        { format("unicodesToAscii7: couldn't convert U-%x to 7bit ascii",
            [i(U)], Msg) },
        { error(Msg) }
    ).

:- instance encoding(latin1) where [
    pred(decode/4) is decode_latin1,
    pred(encode/3) is encode_latin1
].

:- pred decode_latin1(latin1::in, unicode::out, entity::in, entity::out)
    is semidet.

decode_latin1(_, U) -->
    [U].

:- pred encode_latin1(latin1::in, list(unicode)::in, string::out) is det.

encode_latin1(_, Us, Str) :-
    unicodesToLatin1(Us, Cs, []),
    string.from_char_list(Cs, Str).

:- pred unicodesToLatin1(list(unicode)::in, list(char)::out, list(char)::in)
    is det.

unicodesToLatin1([]) --> [].
unicodesToLatin1([U | Us]) -->
    ( if { char.to_int(C, U) } then
        [C],
        unicodesToLatin1(Us)
    else
        { format("unicodesToLatin1: couldn't convert U-%x to Latin-1",
            [i(U)], Msg) },
        { error(Msg) }
    ).

:- instance encoding(utf8) where [
    pred(decode/4) is decode_utf8,
    pred(encode/3) is encode_utf8
].

:- pred decode_utf8(utf8::in, unicode::out, entity::in, entity::out)
    is semidet.

decode_utf8(_, U) -->
    [U0],
    ( if { U0 /\ 0x80  = 0 } then
        { U = U0 }
    else if { U0 /\ 0x20 = 0 } then
        [U1],
        { U = ((U0 /\ 0x1F) << 6) \/ (U1 /\ 0x3F) }
    else if { U0 /\ 0x10 = 0 } then
        [U1], [U2],
        { U = ((U0 /\ 0x0F) << 12) \/ ((U1 /\ 0x3F) << 6) \/ (U2 /\ 0x3F) }
    else if { U0 /\ 0x08 = 0 } then
        [U1], [U2], [U3],
        { U = ((U0 /\ 0x07) << 18) \/ ((U1 /\ 0x3F) << 12) \/
            ((U2 /\ 0x3F) << 6) \/ (U3 /\ 0x3F) }
    else if { U0 /\ 0x04  = 0 } then
        [U1], [U2], [U3], [U4],
        { U = ((U0 /\ 0x03) << 24) \/ ((U1 /\ 0x3F) << 18) \/
            ((U2 /\ 0x3F) << 12) \/ ((U3 /\ 0x3F) << 6) \/ (U4 /\ 0x3F) }
    else if { U0 /\ 0x02  = 0 } then
        [U1], [U2], [U3], [U4], [U5],
        { U = ((U0 /\ 0x01) << 30) \/ ((U1 /\ 0x3F) << 24) \/
            ((U2 /\ 0x3F) << 18) \/ ((U3 /\ 0x3F) << 12) \/
            ((U4 /\ 0x3F) << 6) \/ (U5 /\ 0x3F) }
    else
        %{ error("decode_utf8: bad value!") }
        { fail }
    ).

:- pred encode_utf8(utf8::in, list(unicode)::in, string::out) is det.

encode_utf8(_, Us, Str) :-
    unicodesToUTF8(Us, Cs, []),
    string.from_char_list(Cs, Str).

:- pred unicodesToUTF8(list(unicode)::in, list(char)::out, list(char)::in)
    is det.

unicodesToUTF8([]) --> [].
unicodesToUTF8([U | Us]) -->
    (if
        { U > 0x00, U =< 0x7F },
        { char.to_int(C, U) }
    then
        [C]
    else if
        { U >= 0x80, U =< 0x07FF },
        { U0 = 0xC0 \/ (0x1F /\ (U >> 6)) },
        { U1 = 0x80 \/ (0x3F /\ U) },
        { char.to_int(C0, U0) },
        { char.to_int(C1, U1) }
    then
        [C0, C1]
    else if
        { U >= 0x0800, U =< 0xFFFF },
        { U0 = 0xE0 \/ (0x0F /\ (U >> 12)) },
        { U1 = 0x80 \/ (0x3F /\ (U >> 6)) },
        { U2 = 0x80 \/ (0x3F /\ U) },
        { char.to_int(C0, U0) },
        { char.to_int(C1, U1) },
        { char.to_int(C2, U2) }
    then
        [C0, C1, C2]
    else if
        { U >= 0x010000, U =< 0x1FFFFF },
        { U0 = 0xF0 \/ (0x07 /\ (U >> 18)) },
        { U1 = 0x80 \/ (0x3F /\ (U >> 12)) },
        { U2 = 0x80 \/ (0x3F /\ (U >> 6)) },
        { U3 = 0x80 \/ (0x3F /\ U) },
        { char.to_int(C0, U0) },
        { char.to_int(C1, U1) },
        { char.to_int(C2, U2) },
        { char.to_int(C3, U3) }
    then
        [C0, C1, C2, C3]
    else if
        { U >= 0x200000, U =< 0x03FFFFFF },
        { U0 = 0xF8 \/ (0x03 /\ (U >> 24)) },
        { U1 = 0x80 \/ (0x3F /\ (U >> 18)) },
        { U2 = 0x80 \/ (0x3F /\ (U >> 12)) },
        { U3 = 0x80 \/ (0x3F /\ (U >> 6)) },
        { U4 = 0x80 \/ (0x3F /\ U) },
        { char.to_int(C0, U0) },
        { char.to_int(C1, U1) },
        { char.to_int(C2, U2) },
        { char.to_int(C3, U3) },
        { char.to_int(C4, U4) }
    then
        [C0, C1, C2, C3, C4]
    else if
        { U >= 0x04000000, U =< 0x7FFFFFFF },
        { U0 = 0xFC \/ (0x01 /\ (U >> 30)) },
        { U1 = 0x80 \/ (0x3F /\ (U >> 24)) },
        { U2 = 0x80 \/ (0x3F /\ (U >> 18)) },
        { U3 = 0x80 \/ (0x3F /\ (U >> 12)) },
        { U4 = 0x80 \/ (0x3F /\ (U >> 6)) },
        { U5 = 0x80 \/ (0x3F /\ U) },
        { char.to_int(C0, U0) },
        { char.to_int(C1, U1) },
        { char.to_int(C2, U2) },
        { char.to_int(C3, U3) },
        { char.to_int(C4, U4) },
        { char.to_int(C5, U5) }
    then
        [C0, C1, C2, C3, C4, C5]
    else
        { format("unicodesToUTF8: couldn't convert U-%x to UTF-8",
            [i(U)], Msg) },
        { error(Msg) }
    ),
    unicodesToUTF8(Us).

:- func [unicode | entity] = entity.
:- mode [out | out] = in is semidet.

[U | E] = E0 :-
    E0 ^ curr < E0 ^ leng,
    string.unsafe_index(E0 ^ text, E0 ^ curr, C),
    char.to_int(C, U),
    E = E0 ^ curr := (E0^curr + 1).

