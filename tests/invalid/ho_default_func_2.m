% Compiling this module should generate an error message since
% it tries to cast a non-standard func inst to ground.

:- module ho_default_func_2.

:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module ho_default_func_2__sub.
:- import_module ho_default_func_2__id.

:- import_module int, std_util.

main -->
	{ baz(IdF), eq(getval(IdF), F) },
	do_io(F).

:- func foo(int) = int.
foo(X) = X + 1.

:- func bar(int) = int.
:- mode bar(out) = in is det.
bar(X) = X + 1.

:- module sub.
:- interface.
:- type t.
:- pred baz(id(t)::out) is det.
:- pred eq(t::in, t::out) is det.
:- pred do_io(t::in, io__state::di, io__state::uo) is det.
:- implementation.
:- type t == (func(int) = int).
baz(mkid(bar)).
eq(X,X).
do_io(F) --> io__write_int(F(42)), nl.
:- end_module sub.

:- module id.
:- interface.
:- type id(T).
:- func mkid(T) = id(T).
:- func getval(id(T)) = T.
:- implementation.
:- type id(T) ---> id(T).
mkid(X) = id(X).
getval(id(X)) = X.
:- end_module id.

