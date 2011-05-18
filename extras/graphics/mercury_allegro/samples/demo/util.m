%-----------------------------------------------------------------------------%

:- module util.
:- interface.

:- import_module list.
:- import_module random.

%-----------------------------------------------------------------------------%

:- func clamp(int, int, int) = int.

:- pred iterate_map_foldl(pred(int, X, A, A), int, int, list(X), A, A).
:- mode iterate_map_foldl(pred(in, out, mdi, muo) is det,
        in, in, out, mdi, muo) is det.

:- pred iterate_map_foldl2(pred(int, X, A, A, B, B),
        int, int, list(X), A, A, B, B).
:- mode iterate_map_foldl2(pred(in, out, in, out, di, uo) is det,
        in, in, out, in, out, di, uo) is det.

:- type rs == random.supply.

:- pred random_list_element(list(X)::in, X::out, rs::mdi, rs::muo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%-----------------------------------------------------------------------------%

clamp(Low, X, High) = max(Low, min(X, High)).

iterate_map_foldl(P, Low, High, Xs, !A) :-
    (if Low >= High then
        Xs = []
    else
        P(Low, X, !A),
        iterate_map_foldl(P, Low+1, High, Xs1, !A),
        Xs = [X | Xs1]
    ).

iterate_map_foldl2(P, Low, High, Xs, !A, !B) :-
    (if Low >= High then
        Xs = []
    else
        P(Low, X, !A, !B),
        iterate_map_foldl2(P, Low+1, High, Xs1, !A, !B),
        Xs = [X | Xs1]
    ).

random_list_element(List, Element, !RS) :-
    random(0, list.length(List), N, !RS),
    list.det_index0(List, N, Element).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
