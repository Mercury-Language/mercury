% This modules tests selection of the Mercury definition for
% types with both Mercury and foreign definitions,
% It also tests field access functions for types with
% both Mercury and foreign definitions.
:- module foreign_type2.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util.

:- type coord(T).

:- func new(T, int, int) = coord(T).

:- func x(coord(T)) = int.
:- func y(coord(T)) = int.

main -->
	{ C = new(1, 4, 5) },
	io__write_string("X:"),
	io__write_int(x(C)),
	io__nl,
	io__write_string("Y:"),
	io__write_int(y(C)),
	io__nl,
	io__write_string(type_name(type_of(C))),
	io__nl.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

% IL implementation
:- pragma foreign_type(il, coord(T),
	"class [foreign_type__csharp_code]coord").

:- pragma foreign_decl("C#", "
public class coord {
	public int x;
	public int y;
}
").

:- pragma foreign_proc("C#", new(_T::in, X::in, Y::in) = (C::out),
	[will_not_call_mercury, promise_pure],
"
	C = new coord();
	C.x = X;
	C.y = Y;
").

:- pragma foreign_proc("C#", x(C::in) = (X::out),
	[will_not_call_mercury, promise_pure],
"
	X = C.x;
").

:- pragma foreign_proc("C#", y(C::in) = (Y::out),
	[will_not_call_mercury, promise_pure],
"
	Y = C.y;
").

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

% Mercury implementation
:- type coord(T) ---> coord(x :: int, y :: int).

new(_, X, Y) = coord(X, Y).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
