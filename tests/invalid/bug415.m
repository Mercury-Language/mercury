%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Compiling this module with
%
%    $ mmc -C bug415.m
%
% used to generate a segmentation fault. The cause was a stack overflow
% from an infinite loop, which occurred when trying to convert a recursive
% $merge_inst to a list of format_components.

:- module bug415.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred ip_chunk(list(T)::in(list(I)), int::in, list(list(T))::out(list(I)))
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

ip_chunk(List, ChunkSize, ListOfSmallLists) :-
    ip_chunk_2(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred ip_chunk_2(list(T)::in(list(I)), int::in, list(T)::in(list(I)),
    int::in, list(list(T))::out(list(I))) is det.

ip_chunk_2([], _ChunkSize, List0, _N, Lists) :-
    (
        List0 = [],
        Lists = []
    ;
        List0 = [_ | _],
        ip_reverse(List0, List),
        Lists = [List]
    ).
ip_chunk_2([X | Xs], ChunkSize, List0, N, Lists) :-
    ( if N > 1 then
        ip_chunk_2(Xs, ChunkSize, [X | List0], N - 1, Lists)
    else
        ip_reverse([X | List0], List),
        ip_chunk_2(Xs, ChunkSize, [], ChunkSize, ListsTail),
        Lists = [List | ListsTail]
    ).

:- pred ip_reverse(list(T), list(T)).
:- mode ip_reverse(in(list(I)), out(list(I))) is det.

ip_reverse(List, RevList) :-
    RevList = list.inst_preserving_reverse(List).

%---------------------------------------------------------------------------%
:- end_module bug415.
%---------------------------------------------------------------------------%
