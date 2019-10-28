%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_from_char_list_ilseq.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("semidet_from_char_list:\n", !IO),
    list.foldl(test_from_char_list, test_cases, !IO),

    io.write_string("\nsemidet_from_rev_char_list:\n", !IO),
    list.foldl(test_from_rev_char_list, test_cases, !IO).

:- func test_cases = list(list(char)).

test_cases = [
    ['a', 'b'],
    ['a', char.det_from_int(0), 'b'],
    ['a', char.det_from_int(0xd83d), 'b'],
    ['a', char.det_from_int(0xde00), 'b'],
    ['a', char.det_from_int(0xd83d), char.det_from_int(0xde00), 'b'],
    ['a', char.det_from_int(0xde00), char.det_from_int(0xd83d), 'b']
].

:- pred test_from_char_list(list(char)::in, io::di, io::uo) is det.

test_from_char_list(Chars, !IO) :-
    ( if semidet_from_char_list(Chars, Str) then
        write_string_debug(Str, !IO),
        io.nl(!IO)
    else
        io.write_string("semidet_from_char_list failed\n", !IO)
    ).

:- pred test_from_rev_char_list(list(char)::in, io::di, io::uo) is det.

test_from_rev_char_list(Chars, !IO) :-
    ( if semidet_from_rev_char_list(Chars, Str) then
        write_string_debug(Str, !IO),
        io.nl(!IO)
    else
        io.write_string("semidet_from_rev_char_list failed\n", !IO)
    ).

:- pred write_string_debug(string::in, io::di, io::uo) is det.

write_string_debug(S, !IO) :-
    write_string_debug_loop(S, 0, !IO).

:- pred write_string_debug_loop(string::in, int::in, io::di, io::uo) is det.

write_string_debug_loop(S, Index, !IO) :-
    ( if string.index_next(S, Index, NextIndex, Char) then
        ( if is_surrogate(Char) then
            write_hex(char.to_int(Char), !IO)
        else
            io.write_char(Char, !IO)
        ),
        io.write_char(' ', !IO),
        write_string_debug_loop(S, NextIndex, !IO)
    else
        true
    ).

:- pred write_hex(int::in, io::di, io::uo) is det.

write_hex(I, !IO) :-
    io.format("%#x", [i(I)], !IO).
