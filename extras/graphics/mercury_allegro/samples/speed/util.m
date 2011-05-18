%-----------------------------------------------------------------------------%

:- module util.
:- interface.

:- import_module lcg.

:- import_module list.

%-----------------------------------------------------------------------------%

:- func sgn(float) = float.

:- func clamp(int, int, int) = int.

:- func dist(float, float) = float.

:- pred iterate_map_foldl(pred(int, X, A, A), int, int, list(X), A, A).
:- mode iterate_map_foldl(pred(in, out, in, out) is det,
        in, in, out, in, out) is det.

:- pred iterate_map_foldl2(pred(int, X, A, A, B, B),
        int, int, list(X), A, A, B, B).
:- mode iterate_map_foldl2(pred(in, out, in, out, di, uo) is det,
        in, in, out, in, out, di, uo) is det.

:- pred random_list_element(list(X)::in, X::out, rs::in, rs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.

%-----------------------------------------------------------------------------%

sgn(X) = (if X >= 0.0 then 1.0 else -1.0).

clamp(Low, X, High) = max(Low, min(X, High)).

dist(XA, XB) = D :-
    D0 = XA - XB,
    (if D0 < -0.5 then
        D1 = D0 + 1.0
    else if D0 > 0.5 then
        D1 = D0 - 1.0
    else
        D1 = D0
    ),
    D = abs(D1).

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
