%-----------------------------------------------------------------------------%
%
% lazy_list_test.m:
%	This is a trivial example of the use of lazy lists.
%
% This source file is hereby placed in the public domain.  -fjh (the author).

:- module lazy_list_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module lazy, lazy_list, int.

:- func double(int) = int.
double(X) = 2 * X.

main -->
	{ L = iterate(double, 1) },	% construct an infinite list...
	{ L10 = take(10, L) },		% extract the first 10 elements
	print(to_list(L10)), nl.	% print them

%-----------------------------------------------------------------------------%
