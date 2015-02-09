% The compiler of 24/8/2000 added the modes for the type_info and
% typeclass_info arguments of p/2 in the wrong order, resulting
% in a mode error.
%
:- module typeclass_exist_method_2.
:- interface.

:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	{ p(1, U) },
	write(U), nl.


:- typeclass t(T) where [
  	some [U] pred p(T::in, U::out) is det
].

:- instance t(int) where [
	pred(p/2) is p_int
].

:- some [T] pred p_int(int::in, T::out) is det.

p_int(_, "OK").

