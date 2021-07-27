%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
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
    ( if N = 0 then
        0
    else if N = 1 then
        1
    else if N < 0 then
        throw("fib with negative argument")
    else
        fib(N - 2) + fib(N - 1)
    ).
