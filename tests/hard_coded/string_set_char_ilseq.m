%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_set_char_ilseq.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module uint8.

%---------------------------------------------------------------------------%

main(!IO) :-
    S0 = "😀",
    S = between(S0, 1, length(S0)) ++
        S0 ++ S0 ++ S0 ++
        between(S0, 0, length(S0) - 1),
    write_string_debug(S, !IO),
    nl(!IO),
    test_loop(S, 0, ['✓', 'a', 'á'], !IO).

:- pred test_loop(string::in, int::in, list(char)::in, io::di, io::uo) is det.

test_loop(S0, I, Chars, !IO) :-
    ( if
        Chars = [C | Cs],
        set_char(C, I, S0, S1)
    then
        write_string_debug(S1, !IO),
        nl(!IO),
        I1 = I + length(char_to_string(C)),
        test_loop(S1, I1, Cs ++ [C], !IO)
    else
        true
    ).

:- pred write_string_debug(string::in, io::di, io::uo) is det.

write_string_debug(S, !IO) :-
    write_string_debug_loop(S, 0, !IO).

:- pred write_string_debug_loop(string::in, int::in, io::di, io::uo) is det.

write_string_debug_loop(S, Index, !IO) :-
    ( if string.index_next_repl(S, Index, NextIndex, Char, MaybeReplaced) then
        (
            MaybeReplaced = replaced_code_unit(Code),
            io.format("[%x]", [i(uint8.to_int(Code))], !IO)
        ;
            MaybeReplaced = not_replaced,
            ( if char.is_surrogate(Char) then
                io.format("[%x]", [i(char.to_int(Char))], !IO)
            else
                io.write_char(Char, !IO)
            )
        ),
        write_string_debug_loop(S, NextIndex, !IO)
    else
        true
    ).
