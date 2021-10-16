%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_type.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- func new(int, int) = coord.

:- func x(coord) = int.
:- func y(coord) = int.

main(!IO) :-
    A = new(4, 5),
    io.write(A, !IO),
    io.nl(!IO),
    copy_coord(A, B),
    io.write_string("X:", !IO),
    io.write_int(x(B), !IO),
    io.nl(!IO),
    io.write_string("Y:", !IO),
    io.write_int(y(B), !IO),
    io.nl(!IO).

:- pred copy_coord(coord::in, coord::out) is det.

copy_coord(A, B) :-
    B = A.

%---------------------------------------------------------------------------%
% C implementation

:- pragma foreign_type(c, coord, "coord *").

:- pragma foreign_decl(c, "
typedef struct {
    int x, y;
} coord;
").

:- pragma foreign_proc(c,
    new(X::in, Y::in) = (C::out),
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

%---------------------------------------------------------------------------%
% Mercury implementation

:- type coord
    --->    coord(x :: int, y :: int).

new(X, Y) = coord(X, Y).

%---------------------------------------------------------------------------%
