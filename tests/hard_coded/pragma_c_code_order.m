% Ensure that the pragma c_code sections are output in the same order
% they are declared in and that they appear in the generated source file
% before any variables defined in the c_code can be used.
:- module pragma_c_code_order.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	io__write_int(p),
	io__nl.

:- pragma c_header_code("
#define INTEGER	10
").

:- pragma c_code("
#define INTEGER2 20
").

:- pragma c_code("
static int global = INTEGER + INTEGER2;
").

:- func p = int.
:- pragma c_code(p = (X::out), "{
	X = global;
}").
