%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_type.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type coord.

:- func new(int, int) = coord.
:- pragma foreign_export("C", new(in, in) = out, "exported_new").

:- pred newpred(int::in, int::in, coord::out) is det.
:- pragma foreign_export("C", newpred(in, in, out), "exported_newpred").

:- func export_new(int, int) = coord.
:- pred export_newpred(int::in, int::in, coord::out) is det.

:- func x(coord) = int.
:- func y(coord) = int.

main(!IO) :-
    C = new(4, 5),
    io.write_string("X:", !IO),
    io.write_int(x(C), !IO),
    io.nl(!IO),
    io.write_string("Y:", !IO),
    io.write_int(y(C), !IO),
    io.nl(!IO),

    newpred(42, 52, D),
    io.write_string("X:", !IO),
    io.write_int(x(D), !IO),
    io.nl(!IO),
    io.write_string("Y:", !IO),
    io.write_int(y(D), !IO),
    io.nl(!IO),

    E = export_new(420, 520),
    io.write_string("X:", !IO),
    io.write_int(x(E), !IO),
    io.nl(!IO),
    io.write_string("Y:", !IO),
    io.write_int(y(E), !IO),
    io.nl(!IO),

    export_newpred(4201, 5201, F),
    io.write_string("X:", !IO),
    io.write_int(x(F), !IO),
    io.nl(!IO),
    io.write_string("Y:", !IO),
    io.write_int(y(F), !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pragma foreign_type("C#", coord, "coord").

:- pragma foreign_decl("C#", "
public class coord {
    public int x;
    public int y;
}
").

:- pragma foreign_proc("C#",
    new(X::in, Y::in) = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = new coord();
    C.x = X;
    C.y = Y;
").

:- pragma foreign_proc("C#",
    newpred(X::in, Y::in, C::out),
    [will_not_call_mercury, promise_pure],
"
    C = new coord();
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
    newpred(X::in, Y::in, C::out),
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
newpred(X, Y, coord(X, Y)).

%---------------------------------------------------------------------------%

:- pragma foreign_proc(c,
    export_new(X::in, Y::in) = (C::out),
    [may_call_mercury, promise_pure, thread_safe],
"
    coord *local_c;
    local_c = exported_new(X, Y);
    C = local_c;
").

export_new(X, Y) = new(X, Y).

:- pragma foreign_proc(c,
    export_newpred(X::in, Y::in, C::out),
    [may_call_mercury, promise_pure, thread_safe],
"
    coord *local_c;
    exported_newpred(X, Y, &local_c);
    C = local_c;
").

export_newpred(X, Y, C) :-
    newpred(X, Y, C).

%---------------------------------------------------------------------------%
