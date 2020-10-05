%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This tests the use of assertions on C foreign types.
% (Assertions on types in other languages are not yet used.)
%

:- module foreign_type_assertion.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    D0 = north,
    C0 = new_coord(1, 2),
    update_dir(D0, D),
    update_coord(C0, C),
    write_dir(D, !IO),
    write_coord(C, !IO).

:- pragma foreign_decl(c, "
typedef enum {
    north,
    east,
    west,
    south,
} dirs;

typedef struct {
    int x, y;
} coord;

typedef coord *coord_ptr;
").

:- type dir.
:- type coord.

:- pragma foreign_type(c, dir, "dirs", [can_pass_as_mercury_type]).
:- pragma foreign_type(c, coord, "coord_ptr", [can_pass_as_mercury_type]).

:- func north = dir.

:- pragma foreign_proc(c,
    north = (E::out),
    [will_not_call_mercury, promise_pure],
"
    E = north;
").

:- func new_coord(int, int) = coord.

:- pragma foreign_proc(c,
    new_coord(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = MR_NEW(coord);
    C->x = X;
    C->y = Y;
").

:- pred update_dir(dir::in, dir::out) is det.

:- pragma foreign_proc(c,
    update_dir(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    switch (X)
    {
        case north:
            Y = east;
            break;

        case east:
            Y = south;
            break;

        case south:
            Y = west;
            break;

        case west:
            Y = north;
            break;

        default:
            MR_fatal_error(""update_dir: bad dir"");
            break;
    }
").

:- pred update_coord(coord::in, coord::out) is det.

:- pragma foreign_proc(c,
    update_coord(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    Y = MR_NEW(coord);
    Y->x = X->x + 1;
    Y->y = X->y + 1;
").

:- pred write_dir(dir::in, io::di, io::uo) is det.

:- pragma foreign_proc(c,
    write_dir(X::in, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"
    switch (X)
    {
        case north:
            printf(""north\\n"");
            break;

        case east:
            printf(""east\\n"");
            break;

        case south:
            printf(""south\\n"");
            break;

        case west:
            printf(""west\\n"");
            break;

        default:
            MR_fatal_error(""write_dir: bad dir"");
            break;
    }

    S = S0;
").

:- pred write_coord(coord::in, io::di, io::uo) is det.

:- pragma foreign_proc(c,
    write_coord(X::in, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"
    printf(""x: %d, y: %d\\n"", X->x, X->y);
    S = S0;
").
