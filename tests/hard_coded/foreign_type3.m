% This tests (not very well) the use of C enum and struct foreign types.

:- module foreign_type3.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	{ _C = new(1, 2) },
	{ _E = north },
	{ _Pi = pi },
	io__write_string("Success.\n").

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

:- type dir.
:- pragma foreign_type(c, dir, "dirs").
:- pragma foreign_type(il, dir,
		"valuetype [foreign_type3__csharp_code]dirs").

:- type coord.
:- pragma foreign_type(c, coord, "coord").
:- pragma foreign_type(il, coord,
		"valuetype [foreign_type3__csharp_code]coord").

:- type double.
:- pragma foreign_type(c, double, "double").
:- pragma foreign_type(il, double, "valuetype [mscorlib]System.Double").

:- func north = dir.
:- pragma foreign_proc(c, north = (E::out),
		[will_not_call_mercury, promise_pure], "
	E = north;
").
:- pragma foreign_proc("C#", north = (E::out),
		[will_not_call_mercury, promise_pure], "
	E = dirs.north;
").

:- func new(int, int) = coord.
:- pragma foreign_proc(c, new(X::in, Y::in) = (C::out),
		[will_not_call_mercury, promise_pure], "
	C.x = X;
	C.y = Y;
").
:- pragma foreign_proc("C#", new(X::in, Y::in) = (C::out),
		[will_not_call_mercury, promise_pure], "
	C.x = X;
	C.y = Y;
").

:- func pi = double.
:- pragma foreign_proc(c, pi = (Pi::out),
		[will_not_call_mercury, promise_pure], "
	Pi = 3.14;
").
:- pragma foreign_proc("C#", pi = (Pi::out),
		[will_not_call_mercury, promise_pure], "
	Pi = 3.14;
").
