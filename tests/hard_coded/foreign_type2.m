%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests selection of the Mercury definition for
% types with both Mercury and foreign definitions,
% It also tests field access functions for types with
% both Mercury and foreign definitions.
%

:- module foreign_type2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module type_desc.

:- type coord(T).

:- func new(T, int, int) = coord(T).

:- func x(coord(T)) = int.
:- func y(coord(T)) = int.

main(!IO) :-
    C = new(1, 4, 5),
    io.write_string("X:", !IO),
    io.write_int(x(C), !IO),
    io.nl(!IO),
    io.write_string("Y:", !IO),
    io.write_int(y(C), !IO),
    io.nl(!IO),
    io.write_string(type_name(type_of(C)), !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma foreign_type("C#", coord(T), "coord").

:- pragma foreign_decl("C#", "
public class coord {
    public int x;
    public int y;
}
").

:- pragma foreign_proc("C#",
    new(_T::in, X::in, Y::in) = (C::out),
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
%---------------------------------------------------------------------------%

% Mercury implementation

:- type coord(T)
    --->    coord(x :: int, y :: int).

new(_, X, Y) = coord(X, Y).

%---------------------------------------------------------------------------%
