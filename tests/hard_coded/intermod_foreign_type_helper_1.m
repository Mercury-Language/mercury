%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_foreign_type_helper_1.

:- interface.

:- type coord.

:- func new(int, int) = coord.

:- func x(coord) = int.
:- func y(coord) = int.

%---------------------------------------------------------------------------%

:- implementation.

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

% C# implementation.

:- pragma foreign_type("C#", coord, "coord").

% Mercury implementation.

:- type coord
    --->    coord(x :: int, y :: int).

new(X, Y) = coord(X, Y).

%---------------------------------------------------------------------------%
