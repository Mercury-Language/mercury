% The code generated for this test case by the compiler
% of 13/1/2001 aborted with the following output:
%
%	yes(135251024)
%	yes("
%	*** Mercury runtime: caught segmentation violation ***
%
% The type-infos for T and U were being passed in the wrong order.
%
:- module typeclass_order_bug.

:- interface.

:- import_module io, std_util.

:- pred main(io__state::di, io__state::uo) is det.

:- typeclass p(T, U) where [
	pred m(U, T, io__state, io__state),
	mode m(in, in, di, uo) is det
].

:- implementation.

:- instance p(maybe(T), maybe(U)) where [
	pred(m/4) is write_maybe_pair
].

main -->
	m(yes("ok"), yes(1)),
	io__nl.

:- pred write_maybe_pair(maybe(T), maybe(U), io__state, io__state).
:- mode write_maybe_pair(in, in, di, uo) is det.

write_maybe_pair(MaybeT, MaybeU) -->
	io__write(MaybeT),
	io__nl,
	io__write(MaybeU),
	io__nl.

