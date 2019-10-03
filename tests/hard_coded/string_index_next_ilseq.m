%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_index_next_ilseq.
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
    S2 = string.between(S0, 1, 2),
    S = S0 ++ S2 ++ S1 ++ S0,
    test_index_next(S, 0, !IO).

:- pred test_index_next(string::in, int::in, io::di, io::uo) is det.

test_index_next(S, Index, !IO) :-
    ( if string.index_next(S, Index, NextIndex, Char) then
        io.format("string.index_next(S, %d, %d, %#x)\n",
            [i(Index), i(NextIndex), i(char.to_int(Char))], !IO),
        test_index_next(S, NextIndex, !IO)
    else
        io.format("string.index_next(S, %d, _, _) failed\n",
            [i(Index)], !IO)
    ).
