% A regression test for a bug with the printing
% of type_ctor_descs via io__write.

:- module type_ctor_desc.

:- interface.
:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module int, integer, std_util.

main -->
	{ Type = std_util__type_of(test) },
	{ std_util__type_ctor_and_args(Type, TypeCtor, TypeArgs) },
	io__write(TypeCtor),
	io__print(" "),
	io__write(TypeArgs),
	io__nl.

:- func test(int) = int.
:- mode test(in) = out is det.
test(X) = Y :-
	Y = X + 1.
 
