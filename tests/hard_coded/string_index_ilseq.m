%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_index_ilseq.
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
    S0 = "😀",
    S1 = string.between(S0, 0, 1),
    S = S0 ++ S1 ++ S0,
    list.foldl(test_index(S), 0 .. count_code_units(S), !IO).

:- pred test_index(string::in, int::in, io::di, io::uo) is det.

test_index(S, Index, !IO) :-
    ( if string.index(S, Index, Char) then
        io.format("string.index(S, %d, %#x)\n",
            [i(Index), i(char.to_int(Char))], !IO)
    else
        io.format("string.index(S, %d, _) failed\n",
            [i(Index)], !IO)
    ).
