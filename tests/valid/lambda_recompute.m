%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% Date: Feb 27: 1998.
% Symptom: Software error: variable XX not found.
% Cause: mode_util:recompute_instmap_delta was not recomputing the instmap
% delta of the inner lambda goal after some duplicate call elimination was
% performed.

:- module lambda_recompute.
:- interface.

:- import_module io.

:- type display_list.
:- pred compile_display_list(display_list::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

compile_display_list([mon(PPos, 1) | DList]) -->
    { PPos = 1 - 1 },
    { map__init(MapIKnow) },
    { for(0, 1, (pred(X::in, in, out) is det -->
        for(0, 1, (pred(Y::in, in, out) is det -->
            ( if { search(MapIKnow, pos(X, Y), Place) } then
                { Place = place(Kind, _Flags, Obj) },
                ( if
                    ( { Kind \= 1 }
                    ; { PPos = pos(X, Y) }
                    )
                then
                    element(place(pos(X, Y), Kind)),
                    ( if { Obj = [_N - T | _] } then
                        element(thing(pos(X, Y), T))
                    else
                        []
                    )
                else
                    []
                )
            else
                []
            )
        ))
    ), [], DList) }.

:- pred element(element::in, display_list::in, display_list::out) is det.

element(E, DL, [E | DL]).

:- type map == map(pos, place).

:- type pos == pair(int).

:- type place
    --->    place(int, int, list(pair(int))).

:- func pos(int, int) = pos.
:- pragma no_inline(pos/2).

pos(X, Y) = X - Y.

:- type map_i_know
    --->    map(
                list(map),
                map,
                list(map)
            ).

:- type element
    --->    place(pos, int)
    ;       thing(pos, int)
    ;       mon(pos, int).

:- type display_list ==  list(element).

%---------------------------------------------------------------------------%

:- pred get_monst(int, int, io, io).
:- mode get_monst(in, out, di, uo) is det.

:- pragma external_pred(get_monst/4).

:- pred for(int, int, pred(int, T, T), T, T).
:- mode for(in, in, pred(in, in, out) is det, in, out) is det.
:- mode for(in, in, pred(in, in, out) is semidet, in, out) is semidet.
:- mode for(in, in, pred(in, di, uo) is det, di, uo) is det.

for(Min, Max, Pred, !Acc) :-
    ( if Min =< Max then
        call(Pred, Min, !Acc),
        for(Min + 1, Max, Pred, !Acc)
    else
        true
    ).
