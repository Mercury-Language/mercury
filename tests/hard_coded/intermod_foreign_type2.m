:- module intermod_foreign_type2.

:- interface.

:- type coord.

:- func new(int, int) = coord.

:- func x(coord) = int.
:- func y(coord) = int.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module std_util.

% IL implementation
:- pragma foreign_type(il, coord,
	"class [foreign_type__csharp_code]coord").

:- pragma foreign_decl("C#", "
public class coord {
	public int x;
	public int y;
}
").

:- pragma foreign_proc("C#", new(X::in, Y::in) = (C::out),
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
:- type coord ---> coord(x :: int, y :: int).

new(X, Y) = coord(X, Y).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
