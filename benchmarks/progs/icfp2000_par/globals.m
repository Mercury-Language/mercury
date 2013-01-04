%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module globals.

:- interface.

:- import_module io.

:- pred init(io::di, io::uo) is det.

:- pred get(T1::in, T2::out, io::di, io::uo) is det.

:- pred set(T1::in, T2::in, io::di, io::uo) is det.

:- implementation.

:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module univ.

init(!IO) :-
    my_map_init(Map),
    type_to_univ(Map, UMap),
    io.set_globals(UMap, !IO).

get(Name, Value, !IO) :-
    io.get_globals(UMap0, !IO),
    ( univ_to_type(UMap0, Map0) ->
        ( map.search(Map0, univ(Name), UValue) ->
            ( univ_to_type(UValue, Value0) ->
                Value = Value0
            ;
                error("globals: value has bad type")
            )
        ;
            error("get: global not found")
        )
    ;
        error("globals: global store stuffed up")
    ).

set(Name, Value, !IO) :-
    io.get_globals(UMap0, !IO),
    ( univ_to_type(UMap0, Map0) ->
        type_to_univ(Value, UValue),
        map.set(univ(Name), UValue, Map0, Map),
        type_to_univ(Map, UMap),
        io.set_globals(UMap, !IO)
    ;
        error("globals: global store stuffed up")
    ).

:- pred my_map_init(map(univ, univ)::out) is det.

my_map_init(Map) :-
    map.init(Map).
