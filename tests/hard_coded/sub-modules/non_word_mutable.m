% vim: ft=mercury sw=4 ts=4 expandtab
%
% This regression test is an expanded version of the program files by Greg Duck
% in a bug report on Feb 22 2006.

:- module non_word_mutable.

:- interface.

:- import_module io.

:- pred main(io::di,io::uo) is det.

:- implementation.

:- include_module non_word_mutable.child.

:- import_module non_word_mutable.child.
:- import_module float.

:- type c ---> c(int, int).

:- type coord.
:- pragma foreign_type(c, coord, "coord *").

:- pragma foreign_decl(c, "
typedef struct {
    int x, y;
} coord;
").

:- func new_coord(int, int) = coord.

:- pragma foreign_proc(c,
    new_coord(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = MR_GC_NEW(coord);
    C->x = X;
    C->y = Y;
").

:- func x(coord) = int.
:- func y(coord) = int.

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

:- pragma promise_pure(main/2).
main(!IO) :-
    % Check whether we get back the same value as we set.
    GV1Init = 1.2,
    write(GV1Init,!IO),
    nl(!IO),
    impure exported_set_gv1(GV1Init),
    semipure exported_get_gv1(GV1Final),
    write(GV1Final,!IO),
    nl(!IO),
    ( GV1Init = GV1Final ->
        true
    ;
        write_string("GV1 NOT SAME!\n",!IO)
    ),

    % Check whether we get back the same value as the initialization.
    GV2Init = 2.3,
    write(GV2Init,!IO),
    nl(!IO),
    semipure exported_get_gv2(GV2Final),
    write(GV2Final,!IO),
    nl(!IO),
    ( GV2Init = GV2Final ->
        true
    ;
        write_string("GV2 NOT SAME!\n",!IO)
    ),

    % Check whether we get back the same value as we set.
    GV3Init = "abc",
    write(GV3Init,!IO),
    nl(!IO),
    impure exported_set_gv3(GV3Init),
    semipure exported_get_gv3(GV3Final),
    write(GV3Final,!IO),
    nl(!IO),
    ( GV3Init = GV3Final ->
        true
    ;
        write_string("GV3 NOT SAME!\n",!IO)
    ),

    % Check whether we get back the same value as the initialization.
    GV4Init = "def",
    write(GV4Init,!IO),
    nl(!IO),
    semipure exported_get_gv4(GV4Final),
    write(GV4Final,!IO),
    nl(!IO),
    ( GV4Init = GV4Final ->
        true
    ;
        write_string("GV4 NOT SAME!\n",!IO)
    ),

    % Check whether we get back the same value as we set.
    GV5Init = new_coord(1, 2),
    write(c(x(GV5Init), y(GV5Init)),!IO),
    nl(!IO),
    impure exported_set_gv5(GV5Init),
    semipure exported_get_gv5(GV5Final),
    write(c(x(GV5Final), y(GV5Final)),!IO),
    nl(!IO),
    (
        x(GV5Init) = x(GV5Final),
        y(GV5Init) = y(GV5Final)
    ->
        true
    ;
        write_string("GV5 NOT SAME!\n",!IO)
    ),

    % Check whether we get back the same value as the initialization.
    GV6Init = new_coord(2, 3),
    write(c(x(GV6Init), y(GV6Init)),!IO),
    nl(!IO),
    semipure exported_get_gv6(GV6Final),
    write(c(x(GV6Final), y(GV6Final)),!IO),
    nl(!IO),
    (
        x(GV6Init) = x(GV6Final),
        y(GV6Init) = y(GV6Final)
    ->
        true
    ;
        write_string("GV6 NOT SAME!\n",!IO)
    ).
