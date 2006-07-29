% rotd-2006-07-26 was not subtracting one from the arity when constructing the
% name of the table reset and statistics predicates for a memoed function.
%
:- module table_wrong_func_arity.
:- interface.

:- import_module io.

:- pred print_fib(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module exception.

print_fib(!IO) :-
	io.write_int(fib(30), !IO),
	table_statistics_for_fib_1(TableStats, !IO),
	io.write(TableStats, !IO),
	io.nl(!IO),
	table_reset_for_fib_1(!IO).

:- pragma memo(fib/1, [allow_reset, statistics]).
:- func fib(int) = int.

fib(N) = 
	( N = 0 -> 0
	; N = 1 -> 1
	; N < 0 -> throw("fib with negative argument")
	; fib(N - 2) + fib(N - 1)
	).
