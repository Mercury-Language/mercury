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

:- include_module ho_default_func_2__sub.
:- include_module ho_default_func_2__id.
