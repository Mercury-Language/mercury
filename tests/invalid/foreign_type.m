:- module foreign_type.

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

:- type dir.
:- pragma foreign_type(c, dir, "dirs").

:- type coord.
:- pragma foreign_type(c, coord, "coord").

:- type double.
:- pragma foreign_type(c, double, "double").

:- func north = dir.
:- pragma foreign_proc(c, north = (E::out),
		[will_not_call_mercury, promise_pure], "
	E = north;
").

:- func new(int, int) = coord.
:- pragma foreign_proc(c, new(X::in, Y::in) = (C::out),
		[will_not_call_mercury, promise_pure], "
	C.x = X;
	C.y = Y;
").

:- func pi = double.
:- pragma foreign_proc(c, pi = (Pi::out),
		[will_not_call_mercury, promise_pure], "
	Pi = 3.14;
").
