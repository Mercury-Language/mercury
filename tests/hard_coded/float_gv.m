% vim: ft=mercury sw=4 ts=4 expandtab
%
% This regression test is an expanded version of the program files by Greg Duck
% in a bug report on Feb 22 2006.

:- module float_gv.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.

:- type c
    --->    c(int, int).

:- type coord.
:- pragma foreign_type("C", coord, "coord *").
:- pragma foreign_type("C#", coord, "Coord").
:- pragma foreign_type("Java", coord, "Coord").

:- pragma foreign_decl(c, "
typedef struct {
    int x, y;
} coord;
").

:- pragma foreign_decl("C#", "
class Coord {
    public int x, y;
}
").

:- pragma foreign_code("Java", "
public static class Coord {
    public int x, y;
}
").

:- func new_coord(int, int) = coord.

:- func x(coord) = int.
:- func y(coord) = int.

:- pragma foreign_proc(c,
    new_coord(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = MR_GC_NEW(coord);
    C->x = X;
    C->y = Y;
").

:- pragma foreign_proc(c,
    x(C::in) = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = C->x;
").

:- pragma foreign_proc(c,
    y(C::in) = (Y::out),
    [will_not_call_mercury, promise_pure],
"
    Y = C->y;
").

:- pragma foreign_proc("C#",
    new_coord(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = new Coord();
    C.x = X;
    C.y = Y;
").

:- pragma foreign_proc("C#",
    x(C::in) = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = C.x;
").

:- pragma foreign_proc("C#",
    y(C::in) = (Y::out),
    [will_not_call_mercury, promise_pure],
"
    Y = C.y;
").

:- pragma foreign_proc("Java",
    new_coord(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = new Coord();
    C.x = X;
    C.y = Y;
").

:- pragma foreign_proc("Java",
    x(C::in) = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = C.x;
").

:- pragma foreign_proc("Java",
    y(C::in) = (Y::out),
    [will_not_call_mercury, promise_pure],
"
    Y = C.y;
").

:- mutable(gv1, float, 0.0, ground, [untrailed]).
:- mutable(gv2, float, 2.3, ground, [untrailed]).
:- mutable(gv3, string, "", ground, [untrailed]).
:- mutable(gv4, string, "def", ground, [untrailed]).
:- mutable(gv5, coord, new_coord(0, 0), ground, [untrailed]).
:- mutable(gv6, coord, new_coord(2, 3), ground, [untrailed]).

:- pragma promise_pure(main/2).
main(!IO) :-
    % Check whether we get back the same value as we set.
    GV1Init = 1.2,
    write(GV1Init, !IO),
    nl(!IO),
    impure set_gv1(GV1Init),
    semipure get_gv1(GV1Final),
    write(GV1Final, !IO),
    nl(!IO),
    ( if GV1Init = GV1Final then
        true
    else
        write_string("GV1 NOT SAME!\n", !IO)
    ),

    % Check whether we get back the same value as the initialization.
    GV2Init = 2.3,
    write(GV2Init, !IO),
    nl(!IO),
    semipure get_gv2(GV2Final),
    write(GV2Final, !IO),
    nl(!IO),
    ( if GV2Init = GV2Final then
        true
    else
        write_string("GV2 NOT SAME!\n", !IO)
    ),

    % Check whether we get back the same value as we set.
    GV3Init = "abc",
    write(GV3Init, !IO),
    nl(!IO),
    impure set_gv3(GV3Init),
    semipure get_gv3(GV3Final),
    write(GV3Final, !IO),
    nl(!IO),
    ( if GV3Init = GV3Final then
        true
    else
        write_string("GV3 NOT SAME!\n", !IO)
    ),

    % Check whether we get back the same value as the initialization.
    GV4Init = "def",
    write(GV4Init, !IO),
    nl(!IO),
    semipure get_gv4(GV4Final),
    write(GV4Final, !IO),
    nl(!IO),
    ( if GV4Init = GV4Final then
        true
    else
        write_string("GV4 NOT SAME!\n", !IO)
    ),

    % Check whether we get back the same value as we set.
    GV5Init = new_coord(1, 2),
    write(c(x(GV5Init), y(GV5Init)), !IO),
    nl(!IO),
    impure set_gv5(GV5Init),
    semipure get_gv5(GV5Final),
    write(c(x(GV5Final), y(GV5Final)), !IO),
    nl(!IO),
    ( if
        x(GV5Init) = x(GV5Final),
        y(GV5Init) = y(GV5Final)
    then
        true
    else
        write_string("GV5 NOT SAME!\n", !IO)
    ),

    % Check whether we get back the same value as the initialization.
    GV6Init = new_coord(2, 3),
    write(c(x(GV6Init), y(GV6Init)), !IO),
    nl(!IO),
    semipure get_gv6(GV6Final),
    write(c(x(GV6Final), y(GV6Final)), !IO),
    nl(!IO),
    ( if
        x(GV6Init) = x(GV6Final),
        y(GV6Init) = y(GV6Final)
    then
        true
    else
        write_string("GV6 NOT SAME!\n", !IO)
    ).
