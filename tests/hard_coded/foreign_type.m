:- module foreign_type.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- type coord.

:- func new(int, int) = coord.
:- pragma export(new(in, in) = out, "exported_new").

:- pred newpred(int::in, int::in, coord::out) is det.
:- pragma export(newpred(in, in, out), "exported_newpred").

:- func export_new(int, int) = coord.
:- pred export_newpred(int::in, int::in, coord::out) is det.

:- func x(coord) = int.
:- func y(coord) = int.

main -->
	{ C = new(4, 5) },
	io__write_string("X:"),
	io__write_int(x(C)),
	io__nl,
	io__write_string("Y:"),
	io__write_int(y(C)),
	io__nl,
	{ newpred(42, 52, D) },
	io__write_string("X:"),
	io__write_int(x(D)),
	io__nl,
	io__write_string("Y:"),
	io__write_int(y(D)),
	io__nl,
	{ E = export_new(420, 520) },
	io__write_string("X:"),
	io__write_int(x(E)),
	io__nl,
	io__write_string("Y:"),
	io__write_int(y(E)),
	io__nl,
	{ export_newpred(4201, 5201, F) },
	io__write_string("X:"),
	io__write_int(x(F)),
	io__nl,
	io__write_string("Y:"),
	io__write_int(y(F)),
	io__nl.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

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

:- pragma foreign_proc("C#", newpred(X::in, Y::in, C::out),
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

% C implementation
:- pragma foreign_type(c, coord, "coord *").

:- pragma foreign_decl(c, "
typedef struct {
	int x, y;
} coord;
").

:- pragma foreign_proc(c, new(X::in, Y::in) = (C::out),
	[will_not_call_mercury, promise_pure],
"
	C = MR_GC_NEW(coord);
	C->x = X;
	C->y = Y;
").

:- pragma foreign_proc(c, newpred(X::in, Y::in, C::out),
	[will_not_call_mercury, promise_pure],
"
	C = MR_GC_NEW(coord);
	C->x = X;
	C->y = Y;
").

:- pragma foreign_proc(c, x(C::in) = (X::out),
	[will_not_call_mercury, promise_pure],
"
	X = C->x;
").

:- pragma foreign_proc(c, y(C::in) = (Y::out),
	[will_not_call_mercury, promise_pure],
"
	Y = C->y;
").

%----------------------------------------------------------------------------%

% Mercury implementation
:- type coord ---> coord(x :: int, y :: int).

new(X, Y) = coord(X, Y).
newpred(X, Y, coord(X, Y)).

%----------------------------------------------------------------------------%

:- pragma foreign_proc(c, export_new(X::in, Y::in) = (C::out),
	[will_not_call_mercury, promise_pure],
"
	coord *local_c;
	local_c = exported_new(X, Y);
	C = local_c;
").

export_new(X, Y) = new(X, Y).

:- pragma foreign_proc(c, export_newpred(X::in, Y::in, C::out),
	[will_not_call_mercury, promise_pure],
"
	coord *local_c;
	exported_newpred(X, Y, &local_c);
	C = local_c;
").

export_newpred(X, Y, C) :-
	newpred(X, Y, C).

%----------------------------------------------------------------------------%
