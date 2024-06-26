%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_append_ooi_ilseq.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module solutions.
:- import_module string.
:- import_module uint8.

%---------------------------------------------------------------------------%

main(!IO) :-
    S0 = "😀",
    S1 = string.between(S0, 0, count_code_units(S0) - 1),
    S = S0 ++ S1 ++ "z",
    unsorted_aggregate(test_append_ooi(S), write_result, !IO).

:- pred test_append_ooi(string::in, pair(string, string)::out) is multi.

test_append_ooi(S, L - R) :-
    string.nondet_append(L, R, S).

:- pred write_result(pair(string, string)::in, io::di, io::uo) is det.

write_result(L - R, !IO) :-
    io.write_string("L: ", !IO),
    write_string_debug(L, !IO),
    io.write_string("\n", !IO),
    io.write_string("R: ", !IO),
    write_string_debug(R, !IO),
    io.write_string("\n\n", !IO).

:- pred write_string_debug(string::in, io::di, io::uo) is det.

write_string_debug(S, !IO) :-
    write_string_debug_loop(S, 0, !IO).

:- pred write_string_debug_loop(string::in, int::in, io::di, io::uo) is det.

write_string_debug_loop(S, Index, !IO) :-
    ( if string.index_next_repl(S, Index, NextIndex, Char, MaybeReplaced) then
        (
            MaybeReplaced = replaced_code_unit(CodeUnit),
            write_hex(uint8.to_int(CodeUnit), !IO)
        ;
            MaybeReplaced = not_replaced,
            ( if is_surrogate(Char) then
                write_hex(char.to_int(Char), !IO)
            else
                io.write_char(Char, !IO)
            )
        ),
        io.write_char(' ', !IO),
        write_string_debug_loop(S, NextIndex, !IO)
    else
        true
    ).

:- pred write_hex(int::in, io::di, io::uo) is det.

write_hex(I, !IO) :-
    io.format("%#x", [i(I)], !IO).
