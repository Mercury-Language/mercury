%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the filtering of the contents of a map.
%
%---------------------------------------------------------------------------%

:- module test_one_or_more_chunk.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module one_or_more.
:- import_module string.
:- import_module require.

main(!IO) :-
    fill_list(1, 30, [], List),
    io.write_line(List, !IO),
    det_list_to_one_or_more(List, OoM),
    test_chunk_sizes(1, 6, OoM, !IO).

:- pred test_chunk_sizes(int::in, int::in, one_or_more(int)::in,
    io::di, io::uo) is det.

test_chunk_sizes(CurChunkSize, MaxChunkSize, OoM, !IO) :-
    ( if CurChunkSize > MaxChunkSize then
        true
    else
        chunk(OoM, CurChunkSize, Chunks),
        io.format("Chunk size %d:\n", [i(CurChunkSize)], !IO),
        one_or_more.foldl(io.write_line, Chunks, !IO),
        test_chunk_sizes(CurChunkSize + 1, MaxChunkSize, OoM, !IO)
    ).

:- pred fill_list(int::in, int::in,
    list(int)::in, list(int)::out) is det.

fill_list(Cur, Max, !List) :-
    ( if Cur > Max then
        true
    else
        fill_list(Cur + 1, Max, !List),
        !:List = [Cur | !.List]
    ).
