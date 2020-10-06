%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% mercury 0.8 failed this test on some architectures,
% because string literals were not aligned but deep_copy()
% was assuming that they were.
%

:- module string_alignment_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set_ordlist.
:- import_module solutions.
:- import_module string.
:- import_module univ.

main(!IO) :-
    init_globals(!IO),
    gen_tiles(10, 10, Tiles),
    set_global("Tiles", Tiles, !IO),
    init_selection(Selection),
    set_global("Selection", Selection, !IO),
    init_file(MFN),
    set_global("CurrentFile", MFN, !IO).
    % main(bedit.setup, ["robot"], !IO).

:- pred init_file(maybe(string)::out) is det.

init_file(no).

%---------------------------------------------------------------------------%

:- type pos == pair(int).

:- type selection == set_ordlist(pos).

:- pred init_selection(selection::out) is det.

init_selection(Sel) :-
    set_ordlist.init(Sel).

%---------------------------------------------------------------------------%

:- type tile
    --->    tile(kind, list(int)).

:- type kind
    --->    plain.

:- pred gen_tiles(int::in, int::in, map(pos, tile)::out) is det.

gen_tiles(Xmax, Ymax, Tiles) :-
    map.init(Tiles0),
    AllPos =
        ( pred(Pos::out) is nondet :-
            between(0, Xmax-1, X),
            between(0, Ymax-1, Y),
            Pos = X - Y
        ),
    AddTile =
        ( pred(Pos::in, T0::in, T::out) is det :-
            map.set(Pos, tile(plain, []), T0, T)
        ),
    aggregate(AllPos, AddTile, Tiles0, Tiles).

%---------------------------------------------------------------------------%

:- pred between(int::in, int::in, int::out) is nondet.

between(Min, Max, I) :-
    Min =< Max,
    (
        I = Min
    ;
        Min1 = Min + 1,
        between(Min1, Max, I)
    ).

%---------------------------------------------------------------------------%

:- pred init_globals(io::di, io::uo) is det.

init_globals(!IO) :-
    my_map_init(Map),
    type_to_univ(Map, UMap1),
    copy(UMap1, UMap),
    io.set_globals(UMap, !IO).

:- pred get_global(string::in, T::out, io::di, io::uo) is det.

get_global(Name, Value, !IO) :-
    io.get_globals(UMap0, !IO),
    ( if univ_to_type(UMap0, Map0) then
        ( if map.search(Map0, Name, UValue) then
            ( if univ_to_type(UValue, Value0) then
                Value = Value0
            else
                string.format("globals: value for `%s' has bad type",
                    [s(Name)], Str),
                error(Str)
            )
        else
            string.format("globals: %s not found", [s(Name)], Str),
            error(Str)
        )
    else
        error("globals: global store stuffed up")
    ).

:- pred set_global(string::in, T::in, io::di, io::uo) is det.

set_global(Name, Value, !IO) :-
    io.get_globals(UMap0, !IO),
    ( if univ_to_type(UMap0, Map0) then
        type_to_univ(Value, UValue),
        io.write_string("Current global store:\n", !IO),
        io.write(Map0, !IO),
        nl(!IO),
        io.write_string("Adding `", !IO),
        io.write_string(Name, !IO),
        io.write_string("': ", !IO),
        io.write(Value, !IO),
        nl(!IO),
        map.set(Name, UValue, Map0, Map),
        io.write_string("New global store:\n", !IO),
        io.write(Map, !IO),
        nl(!IO),
        type_to_univ(Map, UMap1),
        copy(UMap1, UMap),
        io.set_globals(UMap, !IO)
    else
        error("globals: global store stuffed up")
    ).

:- pred my_map_init(map(string, univ)::out) is det.

my_map_init(Map) :-
    map.init(Map).
