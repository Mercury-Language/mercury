%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This tests (not very well) the use of C enum and struct foreign types.

:- module foreign_type3.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    _C = new(1, 2),
    _E = north,
    _Pi = pi,
    io.write_string("Success.\n", !IO).

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
").

:- pragma foreign_decl("C#", "
public enum dirs {
    north,
    east,
    west,
    south
}

public struct coord {
    public int x;
    public int y;
}
").

:- pragma foreign_decl("Java", "
enum dirs {
    north,
    east,
    west,
    south
}

class coord {
    public int x;
    public int y;
}
").

:- type dir.
:- pragma foreign_type(c, dir, "dirs").
:- pragma foreign_type("C#", dir, "dirs").
:- pragma foreign_type(java, dir, "dirs").
:- pragma foreign_type(erlang, dir, "").

:- type coord.
:- pragma foreign_type(c, coord, "coord").
:- pragma foreign_type("C#", coord, "coord").
:- pragma foreign_type(java, coord, "coord").
:- pragma foreign_type(erlang, coord, "").

:- type double.
:- pragma foreign_type(c, double, "double").
:- pragma foreign_type("C#", double, "double").
:- pragma foreign_type(java, double, "Double").
:- pragma foreign_type(erlang, double, "").

:- func north = dir.
:- pragma foreign_proc(c,
    north = (E::out),
    [will_not_call_mercury, promise_pure],
"
    E = north;
").
:- pragma foreign_proc("C#",
    north = (E::out),
    [will_not_call_mercury, promise_pure],
"
    E = dirs.north;
").
:- pragma foreign_proc("Java",
    north = (E::out),
    [will_not_call_mercury, promise_pure],
"
    E = dirs.north;
").
:- pragma foreign_proc("Erlang",
    north = (E::out),
    [will_not_call_mercury, promise_pure],
"
    E = north
").

:- func new(int, int) = coord.
:- pragma foreign_proc(c,
    new(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C.x = X;
    C.y = Y;
").
:- pragma foreign_proc("C#",
    new(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C.x = X;
    C.y = Y;
").
:- pragma foreign_proc("Java",
    new(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
        C = new coord();
    C.x = X;
    C.y = Y;
").
:- pragma foreign_proc("Erlang",
    new(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = {X, Y}
").

:- func pi = double.
:- pragma foreign_proc(c,
    pi = (Pi::out),
    [will_not_call_mercury, promise_pure],
"
    Pi = 3.14;
").
:- pragma foreign_proc("C#",
    pi = (Pi::out),
    [will_not_call_mercury, promise_pure],
"
    Pi = 3.14;
").
:- pragma foreign_proc("Java",
    pi = (Pi::out),
    [will_not_call_mercury, promise_pure],
"
    Pi = 3.14;
").
:- pragma foreign_proc("Erlang",
    pi = (Pi::out),
    [will_not_call_mercury, promise_pure],
"
    Pi = 3.14
").
