%---------------------------------------------------------------------------%
:- module try_inside_lambda.
%---------------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module list.
:- import_module map.

:- type mmap == map(string, list(string)).
:- type gmap == map(int, g).
:- type g == int.

:- pred p(mmap::in, gmap::in, gmap::out, io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

:- type e
    --->    e(string).

p(Mmap, !GMap, !IO) :-
    map.map_foldl(
        ( pred(_F::in, !.G::in, !:G::out, !.IO1::di, !:IO1::uo) is cc_multi :-
            ( try []
                normal(Mmap, !G)
            then
                true
            catch E@e(_) ->
                fallback(E, !IO1)
            )
        ),
        !GMap, !IO).

:- pred normal(mmap::in, g::in, g::out) is det.

normal(_, !G).

:- pred fallback(e::in, io::di, io::uo) is det.

fallback(_, !IO).

%---------------------------------------------------------------------------%
